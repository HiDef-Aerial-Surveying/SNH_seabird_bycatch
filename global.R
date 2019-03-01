localUse <- TRUE

if(localUse){
  # function in package "pacman" to load packages, automatically installing those missing
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(shiny,
                 shinydashboard,
                 rhandsontable,
                 plyr,
                 tidyverse,
                 magrittr,
                 shinyBS,
                 msm,
                 shinyjs,
                 V8,
                 shinyWidgets,
                 data.table,
                 DT,
                 zip,
                 RColorBrewer,
                 pracma,
                 d3heatmap, 
                 devtools,
                 jsonlite,
                 R6,
                 httpuv,
                 rlang)
  
  pacman::p_load_gh("trestletech/shinyStore")
  #install_github("nik01010/dashboardthemes")
  
}else{
  library(shiny)
  library(shinydashboard)
  library(rhandsontable)
  library(plyr)
  library(tidyverse)
  library(magrittr)
  library(shinyBS)
  library(msm)
  library(shinyjs)
  library(V8)
  library(shinyWidgets)
  library(data.table)
  library(DT)
  library(zip)
  library(RColorBrewer)
  library(pracma)
  library(d3heatmap)
  library(devtools)
  library(shinyStore)
}



options(shiny.sanitize.errors = FALSE)



species <- sort(c("Common Guillemot", "Northern Gannet","Atlantic Puffin", 
                  "Razorbill", "Common Scoter","European Shag", "Long-tailed Duck",
                  "Red-throated Diver"))


defaultSpecies<- "Common Guillemot"


fish <- sort(c("Herring","Sprat","Cod"))

defaultFish <- "Herring"



gear <- sort(c("Purse seine","Gill net","Trawl","Long-line"))


defaultGear<- "Trawl"


regions <- sort(c("Forth and tay","North East", "Moray Firth","Orkney Islands", "Shetland Isles","North Coast",
                  "West Highlands","Outer Hebrides","Argyll","Clyde","Solway"))

defaultRegion <- "Moray Firth"



###############################################
### Helper functions ###

label.help <- function(label, id){
  HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}




###############################################################################################
#### Underlying data ####
#########################

Header <- c('Species','Sci.name','Dive.depth','Dive.duration','Max.dive','Max.duration')
COGU <- c('Common guillemot','Uria aalge',42.0,76.0,138.0,202.0)
RAZO <- c('Razorbill','Alca torda',15.0,46.0,140.0,NA)
ATPU <- c('Atlantic Puffin','Fratercula arctica',35.0,27.0,68.0,115.0)
RTDI <- c('Red-throated diver','Gavia stellata',NA,NA,NA,NA)
COSC <- c('Common scoter','Melanitta nigra',NA,36.0,NA,NA)
LTDU <- c('Long-tailed duck','Clangula hyemalis',NA,NA,NA,NA)
NOGA <- c('Northern gannet','Morus bassanus',6.0,9.0,34.0,40.0)
EUSH <- c('European shag','Phalocrocorax aristotelis',27.0,56.0,61.0,163.0)


Dive.data <- data.frame(do.call('rbind',list(COGU,RAZO,ATPU,RTDI,COSC,LTDU,NOGA,EUSH)))
names(Dive.data) <- Header






