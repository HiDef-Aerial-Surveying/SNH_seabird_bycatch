
function(input, output, session) {
  shinyjs::disable("numInput_PointEstimate")
  shinyjs::disable("speciesName")
  shinyjs::disable("sciName")
  #shinyjs::disable("diveDepth")
  #shinyjs::disable("diveDuration")
  
  ## Observe the selection of a species and then change the input parameters in the box
  observeEvent(input$selectSpecs,{
    
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
    
  })
  
  
  newdata <- reactive({
    out <- data.frame(depth = input$diveDepth,maxdepth = input$diveDepthMax,depthsd = input$diveDepthStd)
  })
  
  observe({
    newdat <- newdata()
    
    output$diveDepth_plot <- renderPlot(Dive_dens.plot(avg=newdat$depth,mx=newdat$maxdepth,stdev=newdat$depthsd))  
    
  })
  
  
  
  observeEvent(input$appvrsn, {
    showModal(version.notes)
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
  
  
}
