##############################################################################
### This script contains version notes for each release of the application ###
##############################################################################


version.notes <- modalDialog(
  h2('v0.0.1'),
  tags$ul(
    tags$li('Initial upload to development page'),
    tags$li('Basic dive data added to global.R'),
    tags$li('Added function to draw in data on species select')
  ),
  
  title = 'Version notes'
  
)