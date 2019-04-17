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


version.notes <- modalDialog(
  h2('v0.0.4'),
  tags$ul(
    tags$li('Added plot showing simulated dive profile versus depth of various gears'),
    tags$li('Added controls for gear type'),
    tags$li('Added functions for performing the ERM but not integrated into front end yet'),
    tags$li('Proportion of simulated dives that fall within the range of the gear added to plot'),
    tags$li('Improved speed and functionality of the polygons on map by simplifying polygons')
  ),
  
  title = 'Version notes'
  
)


version.notes <- modalDialog(
  h2('v0.0.5'),
  tags$ul(
    tags$li('Added data for number of dives per day for several species'),
    tags$li('Standard deviation input parameters have been added to the front end'),
    tags$li('Calculations for Dd, Fe and Ba for breeding and non-breeding seasons integrated'),
    tags$li('Values are now all reactive and point estimates in the output box now change as values change')
  ),
  
  title = 'Version notes'
  
)


version.notes <- modalDialog(
  h2('v0.0.6'),
  tags$ul(
    tags$li('Added code for doing bootstrapping on data'),
    tags$li('Option to select the number of bootstraps added to menu tab'),
    tags$li('Initial simulation output created for display'),
    tags$li('Script added for testing bootstrapping on back-end')
  ),
  
  title = 'Version notes'
  
)




version.notes <- modalDialog(
  h2('v0.0.7'),
  tags$ul(
    tags$li('Added progress bar for bootstrap function'),
    tags$li('Moved bootstrap function to server.r for use in creating progress bar'),
    tags$li('Added collapse button for data boxes'),
    tags$li('Created a user guide integrated into a modal')
  ),
  
  title = 'Version notes'
  
)


