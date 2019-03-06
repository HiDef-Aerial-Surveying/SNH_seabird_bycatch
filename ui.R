

header <- dashboardHeader(
  
  titleWidth =270,
  title = "Seabird Bycatch ERM",
  tags$li(class = "dropdown", actionLink("appvrsn", label = tags$b(CURRENT.VERSION), style = "font-size: 19px")), 
  tags$li(class = "dropdown", a(icon('github', "fa-2x"), href='https://github.com/HiDef-Aerial-Surveying/SNH_seabird_bycatch', 
                                style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_codeLink")),
  tags$li(class = "dropdown", a(icon('bug', "fa-2x"), href='https://github.com/HiDef-Aerial-Surveying/SNH_seabird_bycatch/issues', #exclamation-circle
                                style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_issuesLink"))
  
  # tags$li(class = "dropdown", actionLink("bookmark_btt", label = NULL, icon("bookmark", "fa-2x", lib = "font-awesome"),
  #                                        style = "padding-top: 10px; padding-bottom: 10px")),
  #tags$li(class = "dropdown", actionLink("saveInputs_btt", label = NULL, icon("save", "fa-2x", lib = "font-awesome"),
  #                                       style = "padding-top: 10px; padding-bottom: 10px")),
  #tags$li(class = "dropdown", actionLink("restoreInputs_btt", label = NULL, icon("window-restore", "fa-2x", lib = "font-awesome"),
  #                                       style = "padding-top: 10px; padding-bottom: 10px")),
  
  
  #tags$li(class = "dropdown", a(img(src = "bioConsultSH_Logo_2.png", height = "40px"), href='https://bioconsult-sh.de/en/',
  #                              style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_bioConsultLogoLink"),
  #        style="float: right"),
  #tags$li(class = "dropdown", a(img(src = "HiDef Logo_2.png", height = "40px"), href='https://hidef.bioconsult-sh.de/',
  #                              style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_hiDefLogoLink"),
  #        style="float: right")
  
)

#################################################################
# Dashboard sidebar
#################################################################

sidebar <- dashboardSidebar(
  width = 270,
  
  # tags$style(".main-sidebar{ position: fixed;}"),
  
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Step 1: Fisheries information", tabName = "tab_fishPars", icon = icon("ship"),
      selectizeInput(inputId = "selectGear",  width = "100%", label="Gear type",  
                     choices = gear, multiple=FALSE,
                     selected = defaultGear
      )
      # selectizeInput(inputId = "selectFish",  width = "100%", label="Target species",  
      #                  choices = fish, multiple=FALSE,
      #                  selected = defaultFish
      # )
      
    ),
    
    menuItem(
      "Step 2: Spatial information", tabName = "tab_spatPar", icon = icon("globe"),
      selectizeInput(inputId = "selectPlace",  width = "100%", label="Scottish marine region",  
                     choices = regions, multiple=FALSE,
                     selected = defaultRegion
      )
      
      #menuItemOutput("menuSubItems_species")
    ),
    
    menuItem(text = "Step 3: Species", icon = icon("dove"),
             selectizeInput(inputId = "selectSpecs",  width = "100%", label="Species",  
                            choices = species, multiple=FALSE,
                            selected = defaultSpecies
             )
    ),
    
    
    menuItem(
      "Step 4: Run simulation", tabName = "tab_simulation", icon = icon("brain"),
      bsButton('run',label='Run',icon=icon('play-circle'),style='info',type='action')
    ),
    
    
    hr(),
    
    #bookmarkButton(id="bookmark_btt"),
    #tags$style(type='text/css', "#bookmark_btt { width:180px;}"),
    
    br(),
    bsAlert(anchorId = "alert")
    
    
    
  )
)


#################################################################
# Dashboard body
#################################################################

body <- dashboardBody(
  fluidRow(
    box(id='fish_info_id',
        title='Fisheries Information',
        width = 4,
        status= 'warning',
        solidHeader = TRUE,
        uiOutput('fisheries_info')
        
    ),
    box(
        title='Species Dive Parameters',
        width = 8,
        status= 'success',
        collapsible= TRUE,
        solidHeader = TRUE,
        column(4,
               fluidRow(
                  column(12,
                         textInput(inputId='speciesName',label='Species'),
                         textInput(inputId='sciName',label='Scientific Name'))
               ),
               fluidRow(
                 column(6,
                        numericInput(inputId = 'diveDepth',label='Mean dive depth (m)',value=0),
                        numericInput(inputId = 'diveDepthMax',label='Max dive depth (m)',value=0),
                        numericInput(inputId = 'diveDepthStd',label='Stdev dive depth',value=0)
                        ),
                 column(6,
                        numericInput(inputId = 'diveDuration',label='Mean dive duration (s)',value=0),       
                        numericInput(inputId = 'diveDurationMax',label='Max dive duration (s)',value=0),
                        numericInput(inputId = 'diveDurationStd',label='Stdev dive duration',value=0)
                        )
               )
               ),
        column(8,
               plotOutput('diveDepth_plot')
               )
        
    ),
    
    
    box(title='Spatio-temporal Parameters',
        width = 12,
        status= 'primary',
        collapsible= TRUE,
        solidHeader = TRUE,
        p(' - Name of the chosen region where fishing will occur (possibly an image of the region on a small map)'),
        p(' - Estimated density of selected species during breeding season; showing which months represent the breeding season'),
        p(' - Estimated density of selected species during non-breeding season; showing which months represent the non-breeding season')
    ),
    
    box(title='Simulation output',
        width = 12,
        status= 'info',
        collapsible= TRUE,
        solidHeader = TRUE,
        h2('The output from the simulations will appear here'),
        
        column(4,
               numericInput(width = "60%",
                            inputId = "numInput_PointEstimate",
                            label = label.help("Estimated bycatch rate", "lbl_PointEstimate"),
                            value = 3.45, min = 1, step = 0.1),
               bsTooltip(id = "lbl_PointEstimate",
                         title = paste0("Estimated bycatch rate.",
                                        ""),
                         options = list(container = "body"), placement = "right", trigger = "hover") 
        ),
        
        column(8,
               box(title='Graphical output',
                   width = 12,
                   status= 'primary',
                   h2('A graphical output here')
               )
        )

        
        
    )
    
    
    
    
  )
  

  
        
) ## End PAR dashboard Body


bootstrapPage(
  
  shinyjs::useShinyjs(),
  #theme = shinytheme('spacelab'),
  includeCSS("style.css"),
  tags$head(
    tags$link(rel="stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Montserrat:100,300,500|Open+Sans:100,300,500|Roboto:100,300,500")
  ),
  
  dashboardPage(skin='black',header,sidebar,body)
  
  
  
)
