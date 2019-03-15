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


version.notes <- modalDialog(
  h2('v0.0.2'),
  tags$ul(
    tags$li('Added plotting function to plot distribution of dive data based on rtrunc normal distribution'),
    tags$li('Added the version_notes.R script which houses previous version notes'),
    tags$li('Added output that displays dive durations and standard deviations as well')
  ),
  
  title = 'Version notes'
  
)


version.notes <- modalDialog(
  h2('v0.0.3'),
  tags$ul(
    tags$li('Added a leaflet map of Scotland that loads when the app comes up'),
    tags$li('Added some fake density data for each species in each marine admin unit around Scotland'),
    tags$li('Added a button which allows for users to choose to load the large marine unit shapefile'),
    tags$li('Density information as well as breeding season timing appears in the Spatio-Temporal information screen')
  ),
  
  title = 'Version notes'
  
)

