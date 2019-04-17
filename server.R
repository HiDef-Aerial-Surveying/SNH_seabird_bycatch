############################################################################
##### Server functions for SNH Encounter rate model for seabird bycatch ####
############################################################################


function(input, output, session) {
  shinyjs::disable("numInput_PointEstimate")
  shinyjs::disable("speciesName")
  shinyjs::disable("sciName")
  shinyjs::disable('numInput_Fish_Effort')
  shinyjs::disable("numInput_Bird_Availability")
  shinyjs::disable("numInput_Depth_Density_breed")
  shinyjs::disable("numInput_Depth_Density_nonbreed")
  shinyjs::disable("numInput_PointEstimate_breed")
  shinyjs::disable("numInput_PointEstimate_nonbreed")
  
  
  #shinyjs::disable("diveDepth")
  #shinyjs::disable("diveDuration")
  
########################################################################################################################################
  #### Reactive Functions ####
  ############################
  
  ## Loads up that will be used in the final calculations in the simulation. 
  ## These data will be used to bootstrap the densities by simulating density distributions and sampling from them
  Bootstrap.Data <- reactive({
    xx <- data.frame(dive.duration=input$diveDuration,
                     dive.duration.std=input$diveDurationStd,
                     dive.duration.max=input$diveDurationMax,
                     dives.per.day=input$DivesPerDay,
                     F.effort=input$numInput_Fish_Effort,
                     density.mean.b=input$breedDensityUnderwater,
                     density.SD.b=input$breedDensityUnderwaterSD,
                     density.mean.nb=input$nonbreedDensityUnderwater,
                     density.SD.nb=input$nonbreedDensityUnderwaterSD
    )
  })
  
  
  ## Loads up the releveant information to calculate total estimated encounters (Total time per deployment and total number of deployments)
  Fishing.Time <- reactive({
    xx <- data.frame(time.per.deploy=input$numInput_timeFishing, num.deployments=input$numInput_numDeployments)
  })
  
  ## Loads up the value from the bootstrap size input for running simulations
  bootsize <- reactive({input$bootsize})
  
  
  ## Loads up all the specific data to calculate bird availability in the final simulation calculations
  birdAvailability <- reactive({
    xx <- Bird.availability(dive.duration=input$diveDuration ,dives.per.day=input$DivesPerDay , perc.depth=input$proportion_birds_available )
    return(xx)
  })
  
  ## This calculations a point estimate of encounters per day, and changes when base data changes for the breeding season
  bycatch.estimate.breed <- reactive({
    xx <- input$numInput_Depth_Density_breed * input$numInput_Fish_Effort * input$numInput_Bird_Availability * 86400
  })
  
  ## This calculations a point estimate of encounters per day, and changes when base data changes for the NON-breeding season
  bycatch.estimate.nonbreed <- reactive({
    xx <- input$numInput_Depth_Density_nonbreed * input$numInput_Fish_Effort * input$numInput_Bird_Availability * 86400
  })
  
  
  ## Loads the surface and underwater densities for breeding and non-breeding seasons
  breed_sdens <- reactive({input$breedDensitySurface})
  nonbreed_sdens <- reactive({input$nonbreedDensitySurface})
  breed_sdens_sd <- reactive({input$breedDensitySurfaceSD})
  nonbreed_sdens_sd <- reactive({input$nonbreedDensitySurfaceSD})
  
  breed_ddens <- reactive({input$breedDensityUnderwater})
  nonbreed_ddens <- reactive({input$nonbreedDensityUnderwater})
  
  

  ## This makes the bird dive data and the fishing gear information reactive so it can be loaded up
  newdata <- reactive({
    if(input$selectSpecs != ''){
      ## geartop is the depth of the top of the gear in the water (m)
      ## gearbottom is the depth of the bottom of the gear in the water (m)
      ## Initial values of this are set, but can be changed by the user
      geartop <- 0
      gearbottom <- 10
      if(input$selectGear == 'Gill net'){
        geartop <- input$slideInput_gearDepth
        gearbottom <- geartop + input$slideInput_gearHeight
      }else if(input$selectGear == 'Trawl'){
        geartop <- input$slideInput_gearDepth
        gearbottom <- geartop + input$slideInput_gearHeight
      }else if(input$selectGear == 'Purse seine'){
        geartop <- 0
        gearbottom <- input$slideInput_gearDepth
      }else if(input$selectGear == 'Long-line'){
        ## For long-longs we assume that the volume of water that the hook occupies is 1m3 (so we add -.5 and +.5 meters to the depth)
        geartop <- input$slideInput_gearDepth - 0.5
        gearbottom <- input$slideInput_gearDepth + 0.5
      }else{
        geartop <- 0
        gearbottom <- 10
      }
      if(length(input$slideInput_gearDepth)>0){
        out <- data.frame(depth = input$diveDepth,maxdepth = input$diveDepthMax,depthsd = input$diveDepthStd,geartop = geartop,
                          gearbottom = gearbottom)
      }else{
        ## Only if, for some reason there is an issue with the gear Depth parameter input by the user, the dataframe gets defaulted to this
        out <- data.frame(depth = input$diveDepth,maxdepth = input$diveDepthMax,depthsd = input$diveDepthStd,geartop = 1,gearbottom = 5)
      }
      
    }
  })
  
  
  ## Calculates the fishing effort for each fishing gear type and loads it into a reactive object
  fish.effort <- reactive({
    
    ## length = the length of the gear fully deployed
    ## height = the total height of the gear when deployed in the water
    ## deployment.time = the total time (in hours) gear is deployed
    ## gear.type = one of the four gear types
    
    if(input$selectGear == 'Gill net'){
      xx <- Fishing.effort(length = input$slideInput_gearLength,
                           height = input$slideInput_gearHeight,
                           deployment.time = input$numInput_timeFishing,
                           gear.type = 'Gill net')  
    }else if(input$selectGear == 'Trawl'){
      xx <- Fishing.effort(length = input$slideInput_gearLength,
                           height = input$slideInput_gearHeight,
                           deployment.time = input$numInput_timeFishing,
                           gear.type = 'Trawl')
    }else if(input$selectGear == 'Long-line'){
      xx <- Fishing.effort(deployment.time = input$numInput_timeFishing,
                           gear.type = 'Long-line',
                           num.hooks = input$numInput_totalHooks)
    }else if(input$selectGear == 'Purse seine'){
      xx <- Fishing.effort(net.diameter = input$slideInput_gearLength,
                           height = input$slideInput_gearHeight,
                           deployment.time = input$numInput_timeFishing,
                           gear.type = 'Purse seine')
    }
    return(xx)
  })
  
  
  
  #Pulls out the data from the seasons object (found in global.R), matches the species selected by the user
  #And loads the data into a dataframe that can be loaded into the UI
  
  spatTemp_data <- reactive({
    
    #nb... = non-breeding data
    #b... = breeding data
    
    bseason <- seasons$breeding[tolower(seasons$Species) == tolower(input$selectSpecs)]
    nbseason <- seasons$non.breeding[tolower(seasons$Species) == tolower(input$selectSpecs)] 
    
    place <- gsub(pattern=' ','.',input$selectPlace)
    
    nn <- grep(place,names(non_breeding_table))
    nb_density <- non_breeding_table[tolower(non_breeding_table$Species) == tolower(input$selectSpecs),nn]
    nb_density_sd <- non_breeding_table_sd[tolower(non_breeding_table_sd$Species) == tolower(input$selectSpecs),nn]
    
    
    bn <- grep(place,names(breeding_table))
    b_density <- breeding_table[tolower(breeding_table$Species) == tolower(input$selectSpecs),bn]
    b_density_sd <- breeding_table_sd[tolower(breeding_table_sd$Species) == tolower(input$selectSpecs),bn]
    
    #a = breeding season months as a character string
    #b = non-breeding season months as a character string
    #c = non-breeding density at the surface
    #d = breeding season density at the surface
    #c_sd = standard deviation of non-breeding season density
    #d_sd = standard deviation of breeding season density
    
    tabout <- data.frame(a=bseason,b=nbseason,c=nb_density,d=b_density,c_sd=nb_density_sd,d_sd=b_density_sd)
    return(tabout)
  })
  
########################################################################################################################################
  #### Observe Events ####
  ########################
  
  #### Whenever a new species is selected, the text and numeric inputs for the species update based on the data from Dive.data
  #### that can be found in 'global.R'
  
  observeEvent(input$selectSpecs,{
    
    if(input$selectSpecs!=""){
      selfrm <- Dive.data[tolower(Dive.data$Species) == tolower(input$selectSpecs),]
      scinm <- as.character(selfrm$Sci.name)
      divedp <- as.numeric(as.character(selfrm$Dive.depth))
      divedpMX <- as.numeric(as.character(selfrm$Max.dive))
      divedpSD <- as.numeric(as.character(selfrm$Std.dive))
      
      divedu <- as.numeric(as.character(selfrm$Dive.duration))
      diveduMX <- as.numeric(as.character(selfrm$Max.duration))
      diveduSD <- as.numeric(as.character(selfrm$Std.duration))
      
      numdivs <- as.numeric(as.character(selfrm$Dives.per.day))
      
      updateTextInput(session,'speciesName',value=input$selectSpecs)
      updateTextInput(session,'sciName',value=scinm)  
      updateNumericInput(session,'diveDepth',value=divedp)
      updateNumericInput(session,'diveDepthMax',value=divedpMX)
      updateNumericInput(session,'diveDepthStd',value=divedpSD)
      updateNumericInput(session,'diveDuration',value=divedu)
      updateNumericInput(session,'diveDurationMax',value=diveduMX)
      updateNumericInput(session,'diveDurationStd',value=diveduSD)
      updateNumericInput(session,'DivesPerDay',value=numdivs)
      
    }
    
  })
  
  ## When the version button is clicked, pull up the modal that is defined in the variable 'version.notes' in 'global.R'
  observeEvent(input$appvrsn, {
    showModal(version.notes)
  })
  
  ## When the user guide button is clicked, pull up the modal that is defined in the variable 'how.to' in 'global.R'
  observeEvent(input$howto,{
    showModal(how.to)
  }) 
  
  ## When the Dive source button is clicked, bring up a modal that explains where the dive data come from
  observeEvent(input$divesource,{
    showModal(
      modalDialog(
        h2('Dive parameters'),
        hr(),
        HTML(paste0('<p>Dive depth and duration data were extracted from the Penguiness dive database located', 
                 ' at <a href="www.penguiness.net">Penguiness.net</a><sup>1</sup>.</p>')),
        p(paste0('For the baseline dive data, we use the reported mean dive data from the main search page.',
                 ' However, it should be noted that this is just an amalgamation of many different studies',
                 'as such, the user should ensure that the dive parameters used are robust.')),
        HTML(paste0('<p>The number of dives per day were extracted from Chapter 5 of Robbins (2017)<sup>2</sup>.</p>')),
        hr(),
        HTML(paste0('<p style="font-size:9pt"><sup>1</sup>Ropert-Coudert Y, Kato A, Robbins A, and ',
                    'Humphries GRW (2018). The Penguiness book. World Wide Web electronic publication Version 3.0.',
                    'DOI.10.13140/RG.2.2.32289/66406')),
        HTML(paste0('<p style="font-size:9pt"><sup>2</sup>Robbins AMC (2017) Seabird Ecology in high-energy environments:',
                    'approaches to assessing impacts of marine renewables. University of Glasgow. Doctoral thesis</p>'))
        
      )
    )
  })
  
  
  ## When the Density source button is clicked, bring up a modal that explains where the base density data come from
  observeEvent(input$densitysource,{
    showModal(
      modalDialog(
        h2('Baseline density data'),
        hr(),
        HTML(paste0('<p>Baseline surface density data were taken from Bradbury et al. (2017)<sup>1</sup>.', 
                    ' The full report can be downloaded at ',
                    '<a href="http://sciencesearch.defra.gov.uk/Document.aspx?Document=14236_',
                    'MB0126RiskassessmentofseabirdbycatchinUKwaters.pdf">',
                    'THIS LINK</a>.</p>')),
        HTML(paste0('<p>Densities are derived from MRSea density surface models for summer and winter months only. ',
                 'Using the density models, we took the mean and max values from inside each of ',
                 'the 11 <a href="https://data.gov.uk/dataset/f9ef823d-e672-4f35-8f00-41480dad7bf2/',
                 'administrative-units-scottish-marine-regions-smrs">Scottish marine administrative regions.</a></p>')),
        HTML(paste0('<p>For the baseline densities in this report, we assume summer months refer to ',
                    'the breeding season, and winter refer to non-breeding season. However, it is important',
                    ' to note that breeding season timings differ for different species and densities should be ',
                    'calculated for the appropriate time period.</p>')),
        hr(),
        HTML(paste0('<p style="font-size:9pt"><sup>1</sup>Bradbury G, Shackshaft M, Scott-Hayward L, Rexstad E, ',
                    'Miller D, and Edwards D (2017). Risk assessment of seabird bycatch in UK waters. Report: DEFRA'))
        
      )
    )
  })
  
  
  
  
  ## In 'global.R' there are UI objects defined by the variables Gillnet.Gear, Trawl.Gear, etc... 
  ## When the user selects the gear named, the appropriate UI object is loaded
  observeEvent(input$selectGear,{
    
    if(input$selectGear == 'Purse seine'){
      output$fisheries_info <- renderUI({Purseseine.Gear})
    }else if(input$selectGear == 'Gill net'){
      output$fisheries_info <- renderUI({Gillnet.Gear})
    }else if(input$selectGear == 'Trawl'){
      output$fisheries_info <- renderUI({Trawl.Gear})
    }else if(input$selectGear == 'Long-line'){
      output$fisheries_info <- renderUI({Longlines.Gear})
    }
    
    
  })
  
  
  
  ### Using Shiny's JS functionality, a js script was created which is in 'global.R'
  ### If the user hits the button to collapse the data boxes, this code searches for boxes with the class 'boxbox' and closes them
  observeEvent(input$collapse,{js$collapse('boxbox')})
  
  

#################################################################################################################################
#### Observe objects ####
#########################
    
  ## loadfunction comes from 'global.R' and just does a signif (unless the value is nil)
  ## This does the calculation for the density at depth
  observe({
    dd <- newdata()
    breedDENS <-  loadfunction(Depth.density(surf.dens=breed_sdens(),max.dive.depth = dd$maxdepth))
    nonbreedDENS <- loadfunction(Depth.density(surf.dens=nonbreed_sdens(),max.dive.depth = dd$maxdepth))
    
      
    breedDENS_SD <-  loadfunction(Depth.density(surf.dens=breed_sdens_sd(),max.dive.depth = dd$maxdepth))
    nonbreedDENS_SD <- loadfunction(Depth.density(surf.dens=nonbreed_sdens_sd(),max.dive.depth = dd$maxdepth))
    
    
    updateNumericInput(session,'nonbreedDensityUnderwater',value=nonbreedDENS)
    updateNumericInput(session,'breedDensityUnderwater',value=breedDENS)
    
    updateNumericInput(session,'nonbreedDensityUnderwaterSD',value=nonbreedDENS_SD)
    updateNumericInput(session,'breedDensityUnderwaterSD',value=breedDENS_SD)
    
    
    })
  
  
  ## Loads up the values in the Model Output box that are used to get a point estimate
  observe({
    bycatch.b <- loadfunction(bycatch.estimate.breed())
    bycatch.nb <- loadfunction(bycatch.estimate.nonbreed())
    bdens <- loadfunction(breed_ddens())
    nbdens <- loadfunction(nonbreed_ddens())
    fisheffort <- loadfunction(fish.effort())
    birdavail <- loadfunction(birdAvailability())
    
    updateNumericInput(session,'numInput_Depth_Density_breed',value=bdens)
    updateNumericInput(session,'numInput_Depth_Density_nonbreed',value=nbdens)
    updateNumericInput(session,'numInput_Fish_Effort',value=fisheffort)
    updateNumericInput(session,'numInput_Bird_Availability',value=birdavail)
    updateNumericInput(session,'numInput_PointEstimate_breed',value=bycatch.b)
    updateNumericInput(session,'numInput_PointEstimate_nonbreed',value=bycatch.nb)
  })
  
  
  
  ## This loads and creates the dive density plots that are displayed in the species information and bird availability boxes
  ## Dive_dens.plot is a plotting function stored in 'global.R' 
  
  observe({
    if(input$selectSpecs != ''){
      newdat <- newdata()
      XX <- Dive.profile(avg=newdat$depth,mx=newdat$maxdepth,stdev=newdat$depthsd)
      Prop.Avail <- round(Proportion.available(XX,gear.top=newdat$geartop,gear.bottom=newdat$gearbottom),2)
      
      output$diveDepth_plot <- renderPlot(Dive_dens.plot(XX,mx=newdat$maxdepth,avg=newdat$depth))
      output$Depth_plot_Output <- renderPlot(Dive_dens.plot(XX,mx=newdat$maxdepth,avg=newdat$depth,plot.gear=TRUE,
                                                            gear.top=newdat$geartop,gear.bottom=newdat$gearbottom))
      
      ### Once the plots are calculated, the proportion available function (in global.R) calculates the proportion and 
      ### displays the output in the UI
      output$Prop_avail_Output <- renderUI({
        
        
        numericInput(inputId = 'proportion_birds_available',
                     label=HTML('Proportion of birds available at depth range of gear'),
                     value=Prop.Avail,
                     step=0.01)
        
      })
      
    }
  })
  
  
  ### This object takes the breeding density at the surface for breeding and non-breeding seasons and dumps it to the UI
  observe({
    if(input$selectSpecs!='' & input$selectPlace!=''){
      
      newdat <- newdata()
      dive <- newdat$maxdepth
      dat <- spatTemp_data()
      
      b_dens <- signif(Depth.density(surf.dens=dat$d, max.dive.depth=dive),3)
      nb_dens <- signif(Depth.density(surf.dens=dat$c, max.dive.depth=dive),3)
      
      b_dens_sd <- signif(Depth.density(surf.dens=dat$d_sd, max.dive.depth=dive),3)
      nb_dens_sd <- signif(Depth.density(surf.dens=dat$c_sd, max.dive.depth=dive),3)
      
      
      output$breeding_density_Surface <- renderUI({
        tagList(
          numericInput(inputId = 'breedDensitySurface',label=HTML('Density at surface (birds/km<sup>2</sup>)'),value=dat$d),
          numericInput(inputId = 'breedDensitySurfaceSD',label=HTML('Standard deviation'),value=dat$d_sd)
        )
        
      })
      
      output$nonbreeding_density_Surface <- renderUI({
        tagList(
          numericInput(inputId = 'nonbreedDensitySurface',label=HTML('Density at surface (birds/km<sup>2</sup>)'),value=dat$c),  
          numericInput(inputId = 'nonbreedDensitySurfaceSD',label=HTML('Standard deviation'),value=dat$c_sd)
        )
        
      })
      
      output$nb_Season <- renderUI({
        tagList(
          h3(dat$b,style='text-align:center'),
          numericInput(inputId='nonbreedDensityUnderwater',
                       label=HTML('Mean density below surface (birds/m<sup>3</sup>)'),
                       value=nb_dens,
                       step=0.000001),
          numericInput(inputId='nonbreedDensityUnderwaterSD',
                       label=HTML('Standard deviation density below surface'),
                       value=nb_dens_sd,
                       step=0.000001)
          #HTML(paste0("<h3 style=text-align:center>",nb_dens," birds/m<sup>3</sup></h3>"))
        )
        
        
      })
      
      output$b_Season <- renderUI({
        tagList(
          h3(dat$a,style='text-align:center'),
          numericInput(inputId='breedDensityUnderwater',
                       label=HTML('Mean density below surface (birds/m<sup>3</sup>)'),
                       value=b_dens,
                       step=0.000001),
          numericInput(inputId='breedDensityUnderwaterSD',
                       label=HTML('Standard deviation density below surface'),
                       value=b_dens_sd,
                       step=0.000001)
        )

      })
      
      
    }
    
    
  })
  
  
  
  
###################################################################################################################################
  #### MAPPING UI FUNCTIONS ####
  ##############################
  
  
  
  output$mapper <- renderUI({
    p('The map uses a high resolution polygon and depending on your internet connection may take a moments to load',
      style='margin-top:20px')
  })
  
  
  #### This function will load the scottish marine admin units when the display map button is pressed. 
  #### If there isn't a region selected when the user hits the button, it just loads up the polygons by themselves
  #### However, if there is a region selected, then it loads up the polygons as well as the highlighted polygon of the selected region
  #### SMAUs is an object stored in 'global.R' and is a polygon shapefile stored as an RDS
  
  observeEvent(input$display, {
    
    
    proxy <- leafletProxy("mymap")
    
    if(input$selectPlace!=""){
      
      selected_polygon <- subset(SMAUs,SMAUs$objnam==input$selectPlace)
      
      output$mymap <- renderLeaflet({
        leaflet() %>%
          
          addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>%
          setView(lng=-4.5,lat=57.5,zoom=5) %>%
          
          addPolygons(data = SMAUs,color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1, fillOpacity = 0.5,
                      fillColor = regionPalette,group='Scottish marine regions',
                      highlightOptions = highlightOptions(fillColor='red',fillOpacity=1.0),
                      popup = SMAUs$objnam) %>%
          
          addPolygons(stroke=TRUE, weight = 3,color="orange",fillColor='yellow',data=selected_polygon,layerId = "highlighted_polygon")
      })
      
    } else {
      
      output$mymap <- renderLeaflet({
        leaflet() %>%
          
          addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>%
          setView(lng=-4.5,lat=57.5,zoom=5) %>%
          
          addPolygons(data = SMAUs,color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1, fillOpacity = 0.5,
                      fillColor = regionPalette,group='Scottish marine regions',
                      highlightOptions = highlightOptions(fillColor='red',fillOpacity=1.0),
                      popup = SMAUs$objnam)
      })
      
    }
    

    updateButton(session,inputId = 'display',disabled = TRUE)
  })
  
  
  
  
  ##### These are the functions to initiate the original map with nothing loaded and centered on Scotland
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>%
      setView(lng=-4.5,lat=57.5,zoom=5)
  })
    
  
  
  #### When a place is selected, the SMAUs object (stored in global.R) is subset and a highlighted polygon is added to the map
  
  observeEvent(input$selectPlace,{
    
    if(input$selectPlace != ''){
      
      output$regionName <- renderUI({
        h1(input$selectPlace,style='text-align:center')
      })
      
      
      
      proxy <- leafletProxy("mymap")
      if(input$selectPlace!=""){
        #get the selected polygon 
        selected_polygon <- subset(SMAUs,SMAUs$objnam==input$selectPlace)
        
        #remove any previously highlighted polygon
        proxy %>% removeShape(layerId = "highlighted_polygon")
        
        #add a slightly thicker red polygon on top of the selected one
        proxy %>% addPolygons(stroke=TRUE, weight = 3,color="orange",fillColor='yellow',data=selected_polygon,layerId = "highlighted_polygon")
        
      } 
      
      
      
    }else {
      output$regionName <- renderUI({
        h1('Please select a region',style='text-align:center')
      })
    }
    
  })
  
  
#########################################################################################################################
#### SIMULATION FUNCTIONS ####
##############################  
    
  
  
  ### preloads a message into the UI 
  output$Simulation_Output <- renderUI({
    tagList(
      h2('Simulation output will appear here after completing step 4 in the left-hand menu'),
      p('Please ensure that all the base input parameters are correct before running')
    )
  })
  
  
  
  ### This is the main simulation code. We opt to put it in here in order to allow the progress bar to run

  observeEvent(input$run,{
    
    ### When the button is pressed, the reactive data are loaded up into the four following objects:
    newdat <- newdata()
    bData <- Bootstrap.Data()
    bootsize <- bootsize()
    Fishtime <- Fishing.Time()

    ### The code is rendered into the UI as below
    
    output$Simulation_Output <- renderUI({
      input$run

      
      withProgress(message = 'Bootstrapping bird availability', value = 0, {
        ## bootsize is the number of iterations for the bootstrapping
        n <- bootsize

        ## At every loop, a new depth profile is generated using the rtruncnorm function. 
        ## Using that, a new proportion of birds available is generated. 
        ## We create 'bootsize' number of simulated bird availabilities that gets passed into the next part of the formula
        
        bP <- foreach(i=1:n,.combine='c') %do% {
          X <- data.frame(x=rtruncnorm(10000,a=0,b=newdat$maxdepth,mean=newdat$depth,sd=newdat$depthsd))
          #X <- data.frame(x=rtruncnorm(10000,a=0,b=100,mean=60,sd=20))
          incProgress(1/n, detail = paste("Simulation", i,"of",n))
          Sys.sleep(0.02)
          return(Proportion.available(X,gear.top=newdat$geartop,gear.bottom=newdat$gearbottom))
        }
      })

      ## This part of the code will simulate density profiles (i.e bootsize number of random draws)
      ## And then calculates the confidence intervals
      ## Further, it gives point estimates of the total number of birds that could be encountered based on the fishing effort data
      
      withProgress(message='Sampling densities and calculating CVs',value=0,{

        Density.profile.b <- Dive.profile(avg=bData$density.mean.b,mx=1,mn=-1,stdev=bData$density.SD.b)
        Density.profile.nb <- Dive.profile(avg=bData$density.mean.nb,mx=1,mn=-1,stdev=bData$density.SD.nb)
        incProgress(1/6,detail='Density profiles generated')
        Sys.sleep(0.5)

        BootOut.b <<- Do.bootstrap(boot.size=bootsize,prop.avail=bP,dive.duration=bData$dive.duration,
                                  dive.duration.std = bData$dive.duration.std,dive.duration.max = bData$dive.duration.max,
                                  dives.per.day=bData$dives.per.day,F.effort=bData$F.effort,
                                  Density.profile=Density.profile.b)

        incProgress(1/6,detail='Bootstrapped breeding season estimates')
        Sys.sleep(0.5)
        BootOut.nb <<- Do.bootstrap(boot.size=bootsize,prop.avail=bP,dive.duration=bData$dive.duration,
                                   dive.duration.std = bData$dive.duration.std,dive.duration.max = bData$dive.duration.max,
                                   dives.per.day=bData$dives.per.day,F.effort=bData$F.effort,
                                   Density.profile=Density.profile.nb)

        incProgress(1/6,detail='Bootstrapped non-breeding season estimates')
        Sys.sleep(0.5)
        b.CIs <- get.cis(BootOut.b)
        nb.CIs <- get.cis(BootOut.nb)
        b.QT <- quantile(BootOut.b,c(0.05,0.95))
        nb.QT <- quantile(BootOut.nb,c(0.05,0.95))  
        
        incProgress(1/6,detail='CIs calculated')
        Sys.sleep(0.5)
        b.encounter.estimate <- signif(b.CIs$CIestimate/24 * Fishtime$time.per.deploy * Fishtime$num.deployments,3)
        b.encounter.estimate.lower <- signif(b.QT[1]/24 * Fishtime$time.per.deploy * Fishtime$num.deployments,3)
        b.encounter.estimate.upper <- signif(b.QT[2]/24 * Fishtime$time.per.deploy * Fishtime$num.deployments,3)

        b.encounters <- paste0(as.character(b.encounter.estimate),' (',
                               as.character(b.encounter.estimate.lower),'/',
                               as.character(b.encounter.estimate.upper),')')

        incProgress(1/6,detail='Breeding season total calculated')
        Sys.sleep(0.5)

        nb.encounter.estimate <- signif(nb.CIs$CIestimate/24 * Fishtime$time.per.deploy * Fishtime$num.deployments,3)
        nb.encounter.estimate.lower <- signif(nb.QT[1]/24 * Fishtime$time.per.deploy * Fishtime$num.deployments,3)
        nb.encounter.estimate.upper <- signif(nb.QT[2]/24 * Fishtime$time.per.deploy * Fishtime$num.deployments,3)

        nb.encounters <- paste0(as.character(nb.encounter.estimate),' (',
                                as.character(nb.encounter.estimate.lower),'/',
                                as.character(nb.encounter.estimate.upper),')')

        incProgress(1/6,detail='Non-breeding season total calculated')
        Sys.sleep(0.5)
      })

      ### After the code has run, the UI elements below are loaded to the front end
      ### Values come from the above calculations
      
      tagList(

        column(6,
               h3('Breeding season bootstrapped estimate'),
               disabled(numericInput(inputId='breed_CI_estimate',
                            label=HTML('Mean estimate of Encounter rate from bootstrapping'),
                            value=b.CIs$CIestimate,
                            step=0.0000001)),
               disabled(numericInput(inputId='breed_CI_lower',
                            label=HTML('Lower 95% CI of Encounter rate'),
                            value=b.CIs$CIlower,
                            step=0.0000001)),
               disabled(numericInput(inputId='breed_CI_upper',
                            label=HTML('Upper 95% CI of Encounter rate'),
                            value=b.CIs$CIupper,
                            step=0.0000001)),
               disabled(numericInput(inputId='breed_CI_stderr',
                            label=HTML('Standard error of Encounter rate'),
                            value=b.CIs$CIstderr,
                            step=0.0000001)),
               
               disabled(numericInput(inputId='breed_QT_low',
                                     label=HTML('5% quantile'),
                                     value=b.QT[1],
                                     step=0.0000001)),
               
               disabled(numericInput(inputId='breed_QT_high',
                                     label=HTML('95% quantile'),
                                     value=b.QT[2],
                                     step=0.0000001)),
               

               hr(),
               p('Density histogram of simulated bird encounters per day during the breeding season with CIs and quantiles plotted'),
               plotOutput('breeding_histogram'),
               
               hr(),
               
               h3('Encounter estimate based on total fishing effort'),
               p(paste0(b.encounters,' birds'))


        ),
        column(6,
               h3('Non-breeding season bootstrapped estimate'),
               disabled(numericInput(inputId='nonbreed_CI_estimate',
                            label=HTML('Mean estimate of Encounter rate from bootstrapping'),
                            value=nb.CIs$CIestimate,
                            step=0.0000001)),
               disabled(numericInput(inputId='nonbreed_CI_lower',
                            label=HTML('Lower 95% CI of Encounter rate'),
                            value=nb.CIs$CIlower,
                            step=0.0000001)),
               disabled(numericInput(inputId='nonbreed_CI_upper',
                            label=HTML('Upper 95% CI of Encounter rate'),
                            value=nb.CIs$CIupper,
                            step=0.0000001)),
               disabled(numericInput(inputId='nonbreed_CI_stderr',
                            label=HTML('Standard error of Encounter rate'),
                            value=nb.CIs$CIstderr,
                            step=0.0000001)),
               
               disabled(numericInput(inputId='nonbreed_QT_low',
                                     label=HTML('5% quantile'),
                                     value=nb.QT[1],
                                     step=0.0000001)),
               
               disabled(numericInput(inputId='nonbreed_QT_high',
                                     label=HTML('95% quantile'),
                                     value=nb.QT[2],
                                     step=0.0000001)),

               hr(),
               p('Density histogram of simulated bird encounters per day during the non-breeding season with CIs and quantiles plotted'),
               plotOutput('nonbreeding_histogram'),
               
               hr(),
               
               h3('Encounter estimate based on total fishing effort'),
               p(paste0(nb.encounters,' birds'))
        ),
        
        column(12,
               hr(),
               h3('A report of the above output can be downloaded from here.'),
               radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                            inline = TRUE),
               
               #bsButton('downloadReport',label='Download',style='success',type='action',icon=icon('download'))
               
               downloadButton('downloadReport')
               
               )
        

      )
      
      

      
      

    })
    
    output$breeding_histogram <- renderPlot({
    
      bootstrapped.plot(BootOut.b)
    
    })
    
    output$nonbreeding_histogram <- renderPlot({
      
      bootstrapped.plot(BootOut.nb)
      
    })
    
  })
  

##########################################################################  
  
  report.output <- reactive({
    df <- data.frame(nonbreed.estimate=input$nonbreed_CI_estimate,
                     breed.estimate=input$breed_CI_estimate,
                     nonbreed.lower=input$nonbreed_CI_lower,
                     breed.lower=input$breed_CI_lower,
                     nonbreed.upper=input$nonbreed_CI_upper,
                     breed.upper=input$breed_CI_upper,
                     nonbreed.stderr=input$nonbreed_CI_stderr,
                     breed.stderr=input$breed_CI_stderr,
                     
                     breed.lower.qt=input$breed_QT_low,
                     breed.upper.qt=input$breed_QT_high,
                     nonbreed.lower.qt=input$nonbreed_QT_low,
                     nonbreed.upper.qt=input$nonbreed_QT_high
                     
                   )
    
  })
  
  
  
  regFormula <- reactive({
    x <- runif(100)
    y <- runif(200)
    
    df <- data.frame(x,y)
    #as.formula(paste('mpg ~', input$x))
  })
  
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('Report.rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Report.rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('Report.rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  
  
  
  
  
}
