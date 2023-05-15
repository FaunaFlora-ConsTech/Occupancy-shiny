# Serengeti Occupancy Data Visualisation App


# User interface ---------------------------------

source("serengeti_shiny.R")

# Sidebar ----------------------------------------

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("By Species", tabName = "species")#,
    # menuItem("Pairwise Comparison", tabName = "pairwise_compare")
  ),
  
  tags$footer(
    p(
      "Developed by ",
      a(href = 'https://ffionline.sharepoint.com/sites/analytics/SitePages/Home.aspx', "Ugyen Penjor")
    ),
    
    align = "left",
    style = "
            position:absolute;
            bottom:0;
            width:100%;
            height:50px; /* Height of the footer */
            color: white;
            padding: 10px;
            background-color: black;
            z-index: 1000;"
  )
)

# Body -------------------------------------------

body <- dashboardBody(
  
  tabItems(
    
    tabItem(
      
      tabName = "overview",
      
      fluidRow(
        box(width = 12,
            h1("Serengeti Camera Traps"),
            "This dashboard facilitates exploration of the subset of camera trap data (year 2010) 
                    from the systematic grid in Serengeti National Park, Tanzania. These data represent 
                    a collaborative effort between Drs. ", a(href = 'www.lionresearch.org', "Craig Packer,"), 
            " Ali Swanson, Margaret Kosmala, and ", a(href = 'www.meredithspalmer.weebly.com', "Meredith Palmer."), 
            "Data presented here were classified by citizen scientists on the website",  
            a(href = 'www.SnapshotSerengeti.org', "Snapshot Serengeti."), "App is based heavily on code
                    originally provided by ", a(href = 'http://www.kaitlyngaynor.com/', "Dr. Kaitlyn Gaynor."), 
            "This dataset is heavily edited by Ugyen Penjor for illustrative purpose only. Please contact the people
                    mentioned above if you want access to the whole dataset.")), 
      
      fluidRow(
        box(width = 12,
            title = "Camera trap study design",
            status = "primary",
            "The camera trap study area covers ~1200 km2 in the center of Serengeti National Park, 
                    Tanzania, covering an area of woodlands and short-grass plains. The figure of the study site
                    /setup below is from Palmer et al. 2020:",
            div(img(src="https://meredithspalmer.files.wordpress.com/2020/06/screen-shot-2020-06-17-at-2.58.02-pm.png", width=600), style = "text-align: center;"),
            "More camera trap information to come. For details on survey methodology, please read ", 
            a(href = 'https://www.nature.com/articles/sdata201526?origin=app', "Swanson et al. 2015;"), 
            "to participate in on-going citizen science classifications of Serengeti data, visit ", 
            a(href = 'https://www.snapshotserengeti.org', "Snapshot Serengeti."), "Raw data can be
                    accessed at the", a(href = 'http://lila.science/datasets/snapshot-serengeti', "LILA image
                                        repository."), 
            "Please contact", a(href = 'mailto:palme516@umn.edu', 'Dr. Meredith Palmer'), "for additional 
                    information, (meta)data, or questions about collaborations.")), 
      
      fluidRow(
        box(title = "Funding and collaboration",
            width = 12,
            status = "primary",
            "Acknowledgements coming soon."))
    ),
    
    # By species -------------------------------------
    
    tabItem(
      
      tabName = "species",
      
      fluidRow(
        box(h2("INDIVIDUAL SPECIES PATTERNS"), width = 12)),
      
      fluidRow(
        box(
          title = "Choose a species",
          selectInput(inputId = "species_select",
                      label = "Select species:",
                      selected = "Baboon",
                      choices = sort(unique(dat$species))),
        ),
      ),
      
      fluidRow(
        box(title = "Basic Occupancy Modeling (OM) across camera grid",
            collapsible = TRUE,
            
            selectInput(inputId = "om_cov",
                        label = "Select occupancy covariate:",
                        selected = "River_Distance",
                        choices = c("River_Distance", "Kopje_Distance", "Road_Distance", 
                                    "Percent_Tree_Cover")), 
            
            leafletOutput(outputId = "om_map"),
            "Occupancy modeling accounts for imperfect detection (i.e., non-detection does not imply absence). In this analysis, 
              there is only ONE covariate on occupancy and NO covariates on detection probability. More 
              in-depth analyses examining how habitat features affect occupancy and detection are strongly 
              recommended." , 
            br(),
            br(),
        ),
        
        box(title = "Environmental covariates of Occupancy",
            collapsible = TRUE,
            selectInput(inputId = "metadata_select",
                        label = "Choose an environmental covariate:",
                        choices = c("River_Distance", "Kopje_Distance", "Road_Distance", 
                                    "Percent_Tree_Cover")),
            
            plotlyOutput(outputId = "om_metadata"),
            "All covariates are standardised to zero mean and unit standard deviation."
        )
      ),
    )))

# Dashboard --------------------------------------

dashboardPage(
  dashboardHeader(title = "SNP Cameras"),
  sidebar,
  body
)
