

#########################################

### Whenever the script is published to the web, change this to the latest version and update the version notes
CURRENT.VERSION <- 'v0.0.2'

#########################################

version.notes <- modalDialog(
  h2(CURRENT.VERSION),
  tags$ul(
    tags$li('Added plotting function to plot distribution of dive data based on rtrunc normal distribution'),
    tags$li('Added the version_notes.R script which houses previous version notes'),
    tags$li('Added output that displays dive durations and standard deviations as well')
  ),
  
  title = 'Version notes'
  
)

############################################################################





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
                 rlang,
                 truncnorm,
                 cowplot)
  
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
  library(truncnorm)
  library(cowplot)
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


Dive_dens.plot <- function(avg,mx,mn=0,stdev){
  X <- data.frame(x=rtruncnorm(1000,a=mn,b=mx,mean=avg,sd=stdev))
  
  Ylim <- max(density(X$x)$y) + max(density(X$x)$y)*.25
  
  ggplot(X,aes(x)) + 
    geom_density(bw='nrd0',adjust=2,fill='lightgoldenrod',alpha=0.4,color='black',size=0.5)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_cartesian(xlim=c(0,mx),ylim=c(0,Ylim))+
    geom_vline(xintercept=avg,color='red',linetype='dashed',size=0.5)+
    xlab('Depth (m)')+ylab('Density')
}










###############################################################################################
#### Underlying data ####
#########################

Header <- c('Species','Sci.name','Dive.depth','Dive.duration','Max.dive','Max.duration','Std.dive','Std.duration')
COGU <- c('Common guillemot','Uria aalge',42.0,76.0,138.0,202.0,50.2,38.0)
RAZO <- c('Razorbill','Alca torda',15.0,46.0,140.0,NA,7.10,16.0)
ATPU <- c('Atlantic Puffin','Fratercula arctica',35.0,27.0,68.0,115.0,15.3,15.0)
RTDI <- c('Red-throated diver','Gavia stellata',NA,NA,NA,NA,NA,NA)
COSC <- c('Common scoter','Melanitta nigra',NA,36.0,NA,NA,NA,NA)
LTDU <- c('Long-tailed duck','Clangula hyemalis',NA,NA,NA,NA,NA,NA)
NOGA <- c('Northern gannet','Morus bassanus',6.0,9.0,34.0,40.0,7.5,6.9)
EUSH <- c('European shag','Phalocrocorax aristotelis',27.0,56.0,61.0,163.0,13.8,15.8)


Dive.data <- data.frame(do.call('rbind',list(COGU,RAZO,ATPU,RTDI,COSC,LTDU,NOGA,EUSH)))
names(Dive.data) <- Header














