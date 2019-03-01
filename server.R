
function(input, output, session) {
  shinyjs::disable("numInput_PointEstimate")
  shinyjs::disable("speciesName")
  shinyjs::disable("sciName")
  shinyjs::disable("diveDepth")
  shinyjs::disable("diveDuration")
  
  ## Observe the selection of a species and then change the input parameters in the box
  observeEvent(input$selectSpecs,{
    
    selfrm <- Dive.data[tolower(Dive.data$Species) == tolower(input$selectSpecs),]
    scinm <- as.character(selfrm$Sci.name)
    divedp <- as.numeric(as.character(selfrm$Dive.depth))
    divedu <- as.numeric(as.character(selfrm$Dive.duration))
    
    
    updateTextInput(session,'speciesName',value=input$selectSpecs)
    updateTextInput(session,'sciName',value=scinm)  
    updateNumericInput(session,'diveDepth',value=divedp)
    updateNumericInput(session,'diveDuration',value=divedu)
    
  })
  
  
}
