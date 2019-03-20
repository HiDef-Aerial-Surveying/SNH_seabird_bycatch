

header <- dashboardHeader(
  
  titleWidth =270,
  title = "Seabird Bycatch ERM",
  tags$li(class = "dropdown", actionLink("howto", label = icon('question', "fa-1x"), style = "font-size: 19px")),
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
    box(
        title='Fisheries Information',
        width = 4,
        status= 'success',
        solidHeader = TRUE,
        collapsible = TRUE,
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
               ),
               fluidRow(
                 column(12,
                        numericInput(inputId='DivesPerDay',label='Number dives per day',value=0)
                        )
               )
               ),
        column(8,
               p('Density plot of simulated dive depth using a truncated normal distribution'),
               plotOutput('diveDepth_plot')
               )
        
    ),
    
    
    box(title='Spatio-temporal Parameters',
        width = 8,
        status= 'primary',
        collapsible= TRUE,
        solidHeader = TRUE,
        column(4,
               fluidRow(
                 column(6,
                        bsButton(inputId = 'display',label='Display Marine Units',icon=icon('eye'),style='primary',type='action')
                 )

               ),
               fluidRow(
                 column(12,
                        uiOutput('mapper'),
                        withSpinner(leafletOutput("mymap",width='100%'))
                        )
               )
               ),
        column(8,
               
               uiOutput('regionName'),
               column(6,
                      h3('Breeding season',style='text-align:center'),
                      
                      uiOutput('breeding_density_Surface'),
                      uiOutput('b_Season')
                      
                      ),
               column(6,
                      h3('Non-breeding season',style='text-align:center'),
                      
                      uiOutput('nonbreeding_density_Surface'),
                      uiOutput('nb_Season')
                      )
               )
      
    ),
    
    box(title='Availability Parameter',
        width = 4,
        status= 'primary',
        collapsible= TRUE,
        solidHeader = TRUE,
        column(12,
               p('Density plot of simulated dive depth using a truncated normal distribution with gear depth plotted'),
               plotOutput('Depth_plot_Output')
               ),
        column(12,
               uiOutput('Prop_avail_Output')
               )
    ),
    
    
    box(title='Simulation output',
        width = 12,
        status= 'info',
        collapsible= TRUE,
        solidHeader = TRUE,
        h2('Encounter rate = Dd x Fe x Ba',style='text-align:center'),
        hr(),
        column(4,
               
               column(6,
                      numericInput(inputId = "numInput_Fish_Effort",
                                   label = label.help(HTML("Fishing effort in m<sup>3</sup>/sec (Fe)"), "lbl_Fish_Effort"),
                                   width = "100%", value = 3.45, min = 1, step = 0.1),
                      bsTooltip(id = "lbl_Fish_Effort",
                                title = "Fishing effort as defined by amount of area fished per second",
                                options = list(container = "body"), placement = "right", trigger = "hover") 
               ),
               
               column(6,
                      numericInput(inputId = "numInput_Bird_Availability",
                                   label = label.help("Availability of birds (Ba)", "lbl_Bird_Availability"),
                                   width = "100%", value = 3.45, min = 1, step = 0.1),
                      bsTooltip(id = "lbl_Bird_Availability",
                                title = paste0("The availability of birds to encounter fishing gear as defined by ",
                                               "the number of dives per day multiplied by the amount of time per dive ",
                                               "then multiplied by the proportion of simulated dives that occur in the ",
                                               "depth range of the gear"),
                                
                                options = list(container = "body"), placement = "right", trigger = "hover") 
               ),
               
               column(6,
                      numericInput(inputId = "numInput_Depth_Density_breed",
                                   label = label.help(HTML("Underwater density per m<sup>3</sup> (Dd): breeding"),
                                                      "lbl_Depth_Density"),
                                   width = "100%", value = 3.45, min = 1, step = 0.1),
                      bsTooltip(id = "lbl_Depth_Density",
                                title = paste0("The underwater density of the species extrapolated ",
                                               "from the surface to the max dive depth of the species"),
                                options = list(container = "body"), placement = "right", trigger = "hover") 
               ),
               column(6,
                      numericInput(inputId = "numInput_Depth_Density_nonbreed",
                                   label = label.help(HTML("Underwater density per m<sup>3</sup> (Dd): non-breeding"),
                                                      "lbl_Depth_Density"),
                                   width = "100%", value = 3.45, min = 1, step = 0.1),
                      bsTooltip(id = "lbl_Depth_Density",
                                title = paste0("The underwater density of the species extrapolated ",
                                               "from the surface to the max dive depth of the species"),
                                options = list(container = "body"), placement = "right", trigger = "hover") 
               ),
               
               
               
               column(12,
                      numericInput(inputId = "numInput_PointEstimate_breed",
                                   label = label.help("Estimated point encounter rate (birds/day): breeding", "lbl_PointEstimate_breed"),
                                   width = "60%", value = 3.45, min = 1, step = 0.1),
                      bsTooltip(id = "lbl_PointEstimate_breed",
                                title = paste0("Estimated encounter rate (Birds per day) as calculated by Dd x Fe x Ba",
                                               ""),
                                options = list(container = "body"), placement = "right", trigger = "hover") 
               ),
               column(12,
                      numericInput(inputId = "numInput_PointEstimate_nonbreed",
                                   label = label.help("Estimated point encounter rate (birds/day): non-breeding", "lbl_PointEstimate_nonbreed"),
                                   width = "60%", value = 3.45, min = 1, step = 0.1),
                      bsTooltip(id = "lbl_PointEstimate_nonbreed",
                                title = paste0("Estimated encounter rate (Birds per day) as calculated by Dd x Fe x Ba",
                                               ""),
                                options = list(container = "body"), placement = "right", trigger = "hover") 
               )
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
