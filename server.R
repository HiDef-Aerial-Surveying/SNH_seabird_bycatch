
function(input, output, session) {
  shinyjs::disable("numInput_PointEstimate")
  shinyjs::disable("speciesName")
  shinyjs::disable("sciName")
  #shinyjs::disable("diveDepth")
  #shinyjs::disable("diveDuration")
  
  
  ## Observe the selection of a species and then change the input parameters in the box
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
  
  
   
  newdata <- reactive({
    if(input$selectSpecs != ''){
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
        geartop <- input$slideInput_gearDepth - 0.5
        gearbottom <- input$slideInput_gearDepth + 0.5
      }else{
        geartop <- 0
        gearbottom <- 10
      }
      if(length(input$slideInput_gearDepth)>0){
        out <- data.frame(depth = input$diveDepth,maxdepth = input$diveDepthMax,depthsd = input$diveDepthStd,geartop = geartop,gearbottom = gearbottom)
      }else{
        out <- data.frame(depth = input$diveDepth,maxdepth = input$diveDepthMax,depthsd = input$diveDepthStd,geartop = 1,gearbottom = 5)
      }
      
    }
  })
  
  
  
  
  
  
  
  
  
  
  fish.effort <- reactive({
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
  
  birdAvailability <- reactive({
    xx <- Bird.availability(dive.duration=input$diveDuration ,dives.per.day=input$DivesPerDay , perc.depth=input$proportion_birds_available )
    return(xx)
  })
  
  
  bycatch.estimate.breed <- reactive({
    xx <- input$numInput_Depth_Density_breed * input$numInput_Fish_Effort * input$numInput_Bird_Availability * 86400
  })
  
  bycatch.estimate.nonbreed <- reactive({
    xx <- input$numInput_Depth_Density_nonbreed * input$numInput_Fish_Effort * input$numInput_Bird_Availability * 86400
  })
  

  breed_sdens <- reactive({input$breedDensitySurface})
  nonbreed_sdens <- reactive({input$nonbreedDensitySurface})
  
  observe({
    dd <- newdata()
    breedDENS <-  loadfunction(Depth.density(surf.dens=breed_sdens(),max.dive.depth = dd$maxdepth))
    nonbreedDENS <- loadfunction(Depth.density(surf.dens=nonbreed_sdens(),max.dive.depth = dd$maxdepth))
    
    updateNumericInput(session,'nonbreedDensityUnderwater',value=nonbreedDENS)
    updateNumericInput(session,'breedDensityUnderwater',value=breedDENS)
    
    })
  
  
  
  breed_ddens <- reactive({input$breedDensityUnderwater})
  nonbreed_ddens <- reactive({input$nonbreedDensityUnderwater})
  
  
  loadfunction <- function(value){
    if(length(value)>0){
      x <- signif(value,4)
    }else{
      x <- 0
    }
    return(x)
  }
  
  
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
  
  
  
  
  
  observe({
    if(input$selectSpecs != ''){
      newdat <- newdata()
      XX <- Dive.profile(avg=newdat$depth,mx=newdat$maxdepth,stdev=newdat$depthsd)
      Prop.Avail <- round(Proportion.available(XX,gear.top=newdat$geartop,gear.bottom=newdat$gearbottom),2)
      
      output$diveDepth_plot <- renderPlot(Dive_dens.plot(XX,mx=newdat$maxdepth,avg=newdat$depth))
      output$Depth_plot_Output <- renderPlot(Dive_dens.plot(XX,mx=newdat$maxdepth,avg=newdat$depth,plot.gear=TRUE,
                                                            gear.top=newdat$geartop,gear.bottom=newdat$gearbottom))
      output$Prop_avail_Output <- renderUI({
        
        
        numericInput(inputId = 'proportion_birds_available',
                     label=HTML('Proportion of birds available at depth range of gear'),
                     value=Prop.Avail,
                     step=0.01)
        
      })
      
    }
  })
  
  
  
  observeEvent(input$appvrsn, {
    showModal(version.notes)
  })
  
  
  
  spatTemp_data <- reactive({
    bseason <- seasons$breeding[tolower(seasons$Species) == tolower(input$selectSpecs)]
    nbseason <- seasons$non.breeding[tolower(seasons$Species) == tolower(input$selectSpecs)] 
      
    place <- gsub(pattern=' ','.',input$selectPlace)
    
    nn <- grep(place,names(non_breeding_table))
    nb_density <- non_breeding_table[tolower(non_breeding_table$Species) == tolower(input$selectSpecs),nn]
    nb_density_sd <- non_breeding_table_sd[tolower(non_breeding_table_sd$Species) == tolower(input$selectSpecs),nn]
    
    
    bn <- grep(place,names(breeding_table))
    b_density <- breeding_table[tolower(breeding_table$Species) == tolower(input$selectSpecs),bn]
    b_density_sd <- breeding_table_sd[tolower(breeding_table_sd$Species) == tolower(input$selectSpecs),bn]
    
    
    tabout <- data.frame(a=bseason,b=nbseason,c=nb_density,d=b_density,c_sd=nb_density_sd,d_sd=b_density_sd)
    return(tabout)
  })
  
  
  get.dive <- reactive({
    dive <- input$diveDepthMax
    return(dive)
  })
  
  
  
  observe({
    if(input$selectSpecs!='' & input$selectPlace!=''){
      dive <- get.dive()
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
  
  
  
  
  
  
  
  
  
  output$mapper <- renderUI({
    p('The map uses a high resolution polygon and depending on your internet connection may take a moments to load',
      style='margin-top:20px')
  })
  
  
  
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
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>%
      setView(lng=-4.5,lat=57.5,zoom=5)
  })
    
  
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
  
  

  
}
