
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
      
      updateTextInput(session,'speciesName',value=input$selectSpecs)
      updateTextInput(session,'sciName',value=scinm)  
      updateNumericInput(session,'diveDepth',value=divedp)
      updateNumericInput(session,'diveDepthMax',value=divedpMX)
      updateNumericInput(session,'diveDepthStd',value=divedpSD)
      updateNumericInput(session,'diveDuration',value=divedu)
      updateNumericInput(session,'diveDurationMax',value=diveduMX)
      updateNumericInput(session,'diveDurationStd',value=diveduSD)
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
    
    bn <- grep(place,names(breeding_table))
    b_density <- breeding_table[tolower(breeding_table$Species) == tolower(input$selectSpecs),bn]
    
    tabout <- data.frame(a=bseason,b=nbseason,c=nb_density,d=b_density)
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
      
      b_dens <- signif(Depth.density(surf.dens=dat$d,net.depth=50, max.dive.depth=dive),3)
      nb_dens <- signif(Depth.density(surf.dens=dat$c,net.depth=50 ,max.dive.depth=dive),3)
      
      output$breeding_density_Surface <- renderUI({
        numericInput(inputId = 'breedDensitySurface',label=HTML('Density at surface (birds/km<sup>2</sup>)'),value=dat$d)
      })
      
      output$nonbreeding_density_Surface <- renderUI({
        numericInput(inputId = 'nonbreedDensitySurface',label=HTML('Density at surface (birds/km<sup>2</sup>)'),value=dat$c)
      })
      
      output$nb_Season <- renderUI({
        tagList(
          h3(dat$b,style='text-align:center'),
          numericInput(inputId='nonbreedDensityUnderwater',
                       label=HTML('Mean density below surface (birds/m<sup>3</sup>)'),
                       value=nb_dens,
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
