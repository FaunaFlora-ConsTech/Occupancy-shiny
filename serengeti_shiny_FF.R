

# Serengeti Camera Trap Data Shiny App
# Development phase
# Ugyen Penjor may 2023
# Fauna & Flora

library(tidyverse); library(shiny); library(shinythemes); library(reshape)
library(overlap); library(plotly); library(here); library(scales); library(plyr)
library(shinydashboard); library(leaflet); library(camtrapR); library(dplyr)
library(sp); library(rgdal); library(broom); library(viridis); library(stringr)
library(ggmap); library(magrittr); library(vroom); library(sf); library(lubridate)
library(unmarked); library(parsedate)

# no scientific notation and round to 2 decimals
options(scipen = 999) #, digits = 2)


# Data import -------------------------------------------------------------

# import shapefile
grid <- st_read("SerengetiGridLatLong.shp") 
grid$site <- as.character(grid$site)

dat <- read.csv("cleaned_data_for_shiny_100423.csv")

#dat$Date1 <- as.Date(dat$Date)
dat$Date2 <- parsedate::parse_date(dat$Date)

dat$Month <- month(dat$Date2)
#dat$Month1 <- month(dat$Date1)

camera_metadata <- dat[c("site", "ARCX", "ARCY", "LION.ENCOUNT", "RIV.DIST.M", "KOP.DIST.M", 
                         "ROAD.DIST.M", "PER.TREE.COV")]

names(camera_metadata) <- c("site", "X.Coord", "Y.Coord", "Lion_Density", "River_Distance", 
                            "Kopje_Distance", "Road_Distance", "Percent_Tree Cover")

# Standardise covariates
camera_metadata$River_Distance <- scale(camera_metadata$River_Distance)
camera_metadata$Kopje_Distance <- scale(camera_metadata$Kopje_Distance)
camera_metadata$Road_Distance <- scale(camera_metadata$Road_Distance)
camera_metadata$site <- as.character(camera_metadata$site)

# Remove duplicates
camera_metadata <- camera_metadata[!duplicated(camera_metadata), ]  

# --> at SOME POINT, fix issue with there being different wet and dry season values for lion density
# FOR NOW, average to single value 
camera_metadata <- ddply(camera_metadata, .(site, X.Coord, Y.Coord, River_Distance, 
                                            Kopje_Distance, Road_Distance, `Percent_Tree Cover`), 
                         summarise, Lion_Density = mean(Lion_Density))

x <- camera_metadata[camera_metadata$site == "C07", ]
x <- ddply(x, .(site, X.Coord, Y.Coord), summarise, 
           River_Distance = mean(River_Distance), Kopje_Distance = mean(Kopje_Distance), 
           Lion_Density = mean(Lion_Density), Road_Distance = mean(Road_Distance), 
           `Percent_Tree Cover` = mean(`Percent_Tree Cover`))

camera_metadata <- camera_metadata[!camera_metadata$site == "C07", ]    # all sites except "C07"
camera_metadata <- rbind(camera_metadata, x)  # bind reordered columns of "C07" with main data
camera_metadata$site

# import camera operation spreadsheet
camera_operation <- read.csv("SER_S1-12_siterolleffortexpanded.csv")

camera_operation["date"] <- lapply(camera_operation["date"], as.Date)
camera_operation["date"] <- lapply(camera_operation["date"], parsedate::parse_date)

# Matches site in camera_operation with unique sites in dat and selects only those match
camera_operation <- camera_operation[camera_operation$site %in% unique(dat$site), ]

#########################################################################################

om.calculate <- function(record.table.subset, camop, start.date, end.date, covariate, window, cam_covs){
  
  record.table.subset$window <- paste(day(record.table.subset$Date), record.table.subset$Month, 
                                      year(record.table.subset$Date)) # character: "26 5 2011"
  camop$window <- paste(day(camop$date), month(camop$date), year(camop$date))
  
  # all rows of data are a sighting; summarize presence/absence by week 
  record.table.subset <- record.table.subset[c("site", "window")]
  record.table.subset <- record.table.subset[!duplicated(record.table.subset),]
  record.table.subset$occu <- 1
  
  # search effort (when cameras on/off) 
  start.date <- as.character(range(dat$date)[1])
  end.date <- as.character(range(dat$date)[2])
  daterange <- interval(start.date, end.date)
  
  camop <- camop[which(camop$date %within% daterange), ]
  camop$date <- NULL # remove date column
  camop <- camop[!duplicated(camop), ] # thin down to one row per window
  
  # join search effort and data
  record.table.subset <- join(camop, record.table.subset, type="left")
  
  record.table.subset[is.na(record.table.subset)] <- 0 #NAs when cams on but no sightings; change NAs to 0s
  
  #rows = sites; columns = detection replicates 
  # NAs are when cameras were not on -- do not change to 0s!			
  
  record.table.subset <- record.table.subset %>%
    pivot_wider(names_from=window, values_from=occu)
  
  # Remove columns with particular string
  df <- select(record.table.subset, -contains("2012")) # remove 2012 columns
  df <- select(df, -contains("2011")) # remove 2011 columns
  
  dfns <- df[, -1] # remove sites beacause this is not needed to order
  
  df1 <- dfns # make a copy 
  
  # This function (written by https://stackoverflow.com/users/3522130/rnso) moves
  # NAs to the end but does not alter data in between.
  # Column names (dates in our case) are irrelevant.
  
  rowfn <- function(rr){
    rr2 = rr; j=1
    for(i in 1:length(rr)) if(!is.na(rr[i])){ rr2[j]=rr[i]; j=j+1}
    if(j<(length(rr)+1)) for(k in j:length(rr)) rr2[k]=NA
    rr2
  }
  
  # Now apply the function
  for(i in 1:nrow(dfns)) 
    df1[i, ] <- rowfn(dfns[i, ])
  
  # Bind stations
  df2 <- cbind(df$site, df1)
  
  # Remove rows with all NAs and accrodingly remove rows in covs
  df2 <- df2[rowSums(is.na(df2[,-1])) != ncol(df2[, -1]), ]
  names(df2)[names(df2) == "df$site"] <- "site"
  
  # Removes rows that were all NAs in covariates too
  cov_sub <- camera_metadata[(camera_metadata$site %in% df2$site), ]
  
  # The order of sites doesn't match, we have to fix this
  cov_sub_1 <- cov_sub[match(df2$site, cov_sub$site), ]
  
  sites <- unique(df2$site) #create list of camera trap sites
  ( n <- length(sites) ) # number of sites
  df2$site <- NULL # now data contains just detection histories (need for 'unmarked')
  
  cam_covs <- cov_sub_1
  
  # covariates 
  cam_covs <- cam_covs[c("site", covariate)]  # note here no cov is specified
  cam_covs <- cam_covs[!duplicated(cam_covs), ] # just in case
  names(cam_covs)[2] <- "siteCovs"
  cam_covs_mod <- cam_covs
  cam_covs_mod$site <- NULL # need to remove for 'unmarked' 
  
  # model 
  camtrap <- unmarkedFrameOccu(y = df2, siteCovs = cam_covs_mod)
  
  ( fm1 <- occu(~1 ~siteCovs, camtrap, starts=c(3, 1, 1)) )
  occu_pred <- predict(fm1, newdata = cam_covs, type = "state")
  occu_pred <- cbind(cam_covs, occu_pred)
  
  return(occu_pred)
}


###########################################################################

# Define leaflet legend function ------------------------------------------

# function that adds option to reverse order of legend https://github.com/rstudio/leaflet/issues/256
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}




