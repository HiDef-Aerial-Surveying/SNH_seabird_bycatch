

#########################################

### Whenever the script is published to the web, change this to the latest version and update the version notes
CURRENT.VERSION <- 'v0.0.4'

#########################################

version.notes <- modalDialog(
  h2(CURRENT.VERSION),
  tags$ul(
    tags$li('Added plot showing simulated dive profile versus depth of various gears'),
    tags$li('Added controls for gear type'),
    tags$li('Added functions for performing the ERM but not integrated into front end yet'),
    tags$li('Proportion of simulated dives that fall within the range of the gear added to plot'),
    tags$li('Improved speed and functionality of the polygons on map by simplifying polygons')
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
                 cowplot,
                 leaflet,
                 leaflet.esri,
                 shinycssloaders)
  
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
  library(leaflet)
  library(leaflet.esri)
  library(shinycssloaders)
}



options(shiny.sanitize.errors = FALSE)



SMAUs <- readRDS('data/SMAU.rds')


species <- sort(c("Common Guillemot", "Northern Gannet","Atlantic Puffin", 
                  "Razorbill", "Common Scoter","European Shag", "Long-tailed Duck",
                  "Red-throated Diver"))
defaultSpecies<- "Common Guillemot"


fish <- sort(c("Herring","Sprat","Cod"))
defaultFish <- "Herring"


gear <- sort(c("Purse seine","Gill net","Trawl","Long-line"))
defaultGear<- "Gill net"


regions <- sort(c("","Forth and Tay","North East", "Moray Firth","Orkney Islands", "Shetland Isles","North Coast",
                  "West Highlands","Outer Hebrides","Argyll","Clyde","Solway"))
defaultRegion <- ""

regionPalette <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
  
  
  #c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3',
                   #'#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')


###############################################
### Helper functions ###

label.help <- function(label, id){
  HTML(paste0(label, actionLink(id,label=NULL,icon=icon('info-circle'))))
}

Dive.profile <- function(avg,mx,mn=0,stdev){
  set.seed(99)
  X <- data.frame(x=rtruncnorm(10000,a=mn,b=mx,mean=avg,sd=stdev))
}


Proportion.available <- function(Density.prof,gear.top,gear.bottom){
  
  prop.in.range <- (length(which(Density.prof$x > gear.top & Density.prof$x < gear.bottom)))/10000
  
}



Dive_dens.plot <- function(Density.prof,mx=10,avg=10,plot.gear=FALSE,gear.top=0,gear.bottom=10){
  X <- Density.prof
  if(gear.bottom > mx){
    Xmax <- gear.bottom
  }else{
    Xmax <- mx
  }
  
  Ylim <- max(density(X$x)$y) + max(density(X$x)$y)*.25
  
  
  ggplot(X,aes(x)) +
    geom_density(bw='nrd0',adjust=2,fill='lightgoldenrod',alpha=0.4,color='black',size=0.5)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_cartesian(xlim=c(0,Xmax),ylim=c(Ylim,0))+
    geom_vline(xintercept=avg,color='red',linetype='dashed',size=0.25)+
    xlab('Depth (m)')+ylab('')+
    {if(plot.gear==TRUE)geom_vline(xintercept=gear.top,color='dodgerblue',linetype='solid',size=0.75)}+
    {if(plot.gear==TRUE)geom_vline(xintercept=gear.bottom,color='dodgerblue',linetype='solid',size=0.75)}+
    {if(plot.gear==TRUE)geom_hline(aes(yintercept=100,color='gear'),linetype='solid',size=0.75)}+
    {if(plot.gear==TRUE)scale_color_manual(name='',values=c(gear='dodgerblue'),labels=c('Gear depth'))}+
    {if(plot.gear==TRUE)coord_flip(ylim=c(Ylim,0))}+
    {if(plot.gear==TRUE)scale_x_reverse()}+
    {if(plot.gear==TRUE)theme(legend.position=c(0.5,0.1))}
}


### We calculate the density, generally, as per Wilson et al 2007. 
### Wilson et al. (2007) defines the density in the range of impact as DaQ2R / 2R
### Da = Density of animal at any depth
### Q2R is dependent on depth / diving profile.
### 11 Mar 2019:  For now we use the basic model of assuming equal density through the water column
###               Will change this when functions are more solidified

Depth.density <- function(surf.dens,net.depth,gear.type='Gill net',max.dive.depth){
  #D = DAQ2R/2R = Wilson et al (2007)
  #surf.dens in km2  (will be converted to m2)
  d <- net.depth
  Da <- surf.dens/1000000
  Qd <-  d/max.dive.depth
  
  D <- Da * Qd / d  
  
  return(D)
}




######## Gill net forumlae

Gillnet.effort <- function(length,depth,deployment.time,number.deployments){
  # Length of net fully deployed in M
  # Width of net (i.e. DEPTH of net from top to bottom)
  # Total time per deployment
  # Total number of deployments
  
  ### For this, we assume that the net is covering a block of 1m (width)
  ### This means that Net.area actually becomes m^3.  So... a net of 90m x 30m depth = 2700 m^2 at 1m width = 2700 m^3
  ### We assume this to stay in line with the fact that we are working in number of birds/m^3
  
  Net.area <- length * depth
  
  Effort <- (Net.area/deployment.time) * number.deployments
  return(Effort)
}


Bird.availability <- function(dive.duration,dives.per.day,perc.depth){
  ## In here, there will be factors that affect the likilhood that a bird will get caught up in the nets / hooks
  ## Availability bias can be used here - gives idea of how much time birds spend underwater
  
  dives.per.sec <- dives.per.day / 86400
  
  Ba <- dive.duration * dives.per.sec * perc.depth
  return(Ba)
  
}
#   if(gear.type=='Gill net'){
#     
#     
#     
#   }











#Trawl.effort <- function(){}

#Longline.effort <- function(){}

#Purseseine.effort <- function(){}


#Bycatch.estimate <- function(d.density,gear.type='Gill net',bootstrap.estimate=FALSE){
#}



  # if(gear.type=='Gill net'){
  #   #D = DaQd / d
  #   #Da = density at the surface and extrapolated to 1m depth to get a density per m3 
  #   #d = depth of net from surface to bottom
  #   #Qd = d / max dive depth  (simple model of equal density through the water column) - NOTE: need to account for bathymetry
  #   
  #   Qd <-  d/max.dive.depth
  #   
  #   D <- Da * Qd / d  
  #   
  #   return(D)
  # }
  #}










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


################

breeding_table <- read.csv('data/breeding_table.csv')
non_breeding_table <- read.csv('data/nonbreeding_table.csv')
seasons <- read.csv('data/seasons.csv')



##########################################################################################################
#### These are the varying UI elements that get created depending on what fishing gear is selected
###################################################################################################

### Parameters for trawl fisheries

Trawl.Gear <- tagList(
  
  column(6,
         numericInput(width = "85%",
                     inputId = "slideInput_gearLength",
                     label = label.help("Total length of trawl mouth (m)", "lbl_gearLength"),
                     value = 10, min = 1,max=300, step = 1),
         bsTooltip(id = "lbl_gearLength",
                   title = "The length of the trawl mouth opening from left to right (as sitting in water) in Meters",
                   options = list(container = "body"), placement = "right", trigger = "hover")
  ),
  column(6,
         numericInput(width = "85%",
                      inputId = "slideInput_gearHeight",
                      label = label.help("Total height of trawl mouth (m)", "lbl_gearHeight"),
                      value = 10, min = 1,max=300, step = 1),
         bsTooltip(id = "lbl_gearHeight",
                   title = "Height of trawl mouth in Meters (from top to bottom of net as sitting in the water)",
                   options = list(container = "body"), placement = "right", trigger = "hover")
  ),
  
  
  sliderInput(width = "85%",
              inputId = "slideInput_gearDepth",
              label = label.help("Depth to top of net (m)", "lbl_gearDepth"),
              value = 10, min = 1,max=300, step = 1),
  bsTooltip(id = "lbl_gearDepth",
            title = "Depth to the top of the net from the surface (in Meters)",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  
  numericInput(width = "85%",
               inputId = "numInput_timeFishing",
               label = label.help("Approximate total fishing effort (hrs)", "lbl_timeFishing"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_timeFishing",
            title = paste0("Approximate time fishing.  ",
                           "Estimate the total amount of time gear will be in the water for entire fishing season"),
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  
  numericInput(width = "85%",
               inputId = "numInput_numDeployments",
               label = label.help("Number of net deployments", "lbl_numDeployments"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_numDeployments",
            title = "Total number of net deployments to be made over a season fishing",
            options = list(container = "body"), placement = "right", trigger = "hover")
)

### Parameters for long-line fisheries

Longlines.Gear <- tagList(
  sliderInput(width = "85%",
              inputId = "slideInput_gearLength",
              label = label.help("Total length of gear (m)", "lbl_gearLength"),
              value = 10, min = 1,max=300, step = 1),
  bsTooltip(id = "lbl_gearLength",
            title = "Total length of line in Meters",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  numericInput(width = "85%",
               inputId = "numInput_totalHooks",
               label = label.help("Number of hooks", "lbl_totalHooks"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_totalHooks",
            title = "Total number of hooks used per deployed line",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  
  sliderInput(width = "85%",
              inputId = "slideInput_gearDepth",
              label = label.help("Depth to hooks (m)", "lbl_gearDepth"),
              value = 10, min = 1,max=300, step = 1),
  bsTooltip(id = "lbl_gearDepth",
            title = "Depth to hooks on line (in Meters)",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  numericInput(width = "85%",
               inputId = "numInput_timeFishing",
               label = label.help("Approximate total fishing effort (hrs)", "lbl_timeFishing"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_timeFishing",
            title = "Total time in hours that a single line is deployed",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  numericInput(width = "85%",
               inputId = "numInput_numDeployments",
               label = label.help("Number of line deployments", "lbl_numDeployments"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_numDeployments",
            title = "Total number of line deployments to be made over a season fishing",
            options = list(container = "body"), placement = "right", trigger = "hover")
  
)

### Parameters for gillnets


Gillnet.Gear <- tagList(
  
  column(6,
         numericInput(width = "85%",
                     inputId = "slideInput_gearLength",
                     label = label.help("Total length of gear (m)", "lbl_gearLength"),
                     value = 10, min = 1,max=300, step = 1),
         bsTooltip(id = "lbl_gearLength",
                   title = "Total length of net in Meters",
                   options = list(container = "body"), placement = "right", trigger = "hover")
         ),
  column(6,
         numericInput(width = "85%",
                     inputId = "slideInput_gearHeight",
                     label = label.help("Total height of gear (m)", "lbl_gearHeight"),
                     value = 10, min = 1,max=300, step = 1),
         bsTooltip(id = "lbl_gearHeight",
                   title = "Height of gear in Meters (from top to bottom of net as sitting in the water)",
                   options = list(container = "body"), placement = "right", trigger = "hover")
         ),
  
  
  sliderInput(width = "85%",
              inputId = "slideInput_gearDepth",
              label = label.help("Depth to top of net (m)", "lbl_gearDepth"),
              value = 10, min = 1,max=300, step = 1),
  bsTooltip(id = "lbl_gearDepth",
            title = "Depth to the top of the net from the surface (in Meters)",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  
  numericInput(width = "85%",
               inputId = "numInput_timeFishing",
               label = label.help("Approximate total fishing effort (hrs)", "lbl_timeFishing"),
               value = 10, min = 1, step = 0.5),
  bsTooltip(id = "lbl_timeFishing",
            title =  "Estimate the total amount of time gear will be in the water per deployment",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  
  numericInput(width = "85%",
               inputId = "numInput_numDeployments",
               label = label.help("Number of net deployments", "lbl_numDeployments"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_numDeployments",
            title = "Total number of net deployments to be made over a season fishing",
            options = list(container = "body"), placement = "right", trigger = "hover")
  
)


### Parameters for purse seine fisheries

Purseseine.Gear <- tagList(
  
  sliderInput(width = "85%",
              inputId = "slideInput_gearLength",
              label = label.help("Diameter of nets when deployed (m)", "lbl_gearDiameter"),
              value = 10, min = 1,max=300, step = 1),
  bsTooltip(id = "lbl_gearDiameter",
            title = "Diameter of the purse seine when fully deployed",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  sliderInput(width = "85%",
              inputId = "slideInput_gearDepth",
              label = label.help("Depth to bottom of net (m)", "lbl_gearDepth"),
              value = 10, min = 1,max=300, step = 1),
  bsTooltip(id = "lbl_gearDepth",
            title = "Depth to the bottom of the seine from the surface (in Meters)",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  
  numericInput(width = "85%",
               inputId = "numInput_netDeploy",
               label = label.help("Total time per deployment (hrs)", "lbl_netDeploy"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_netDeploy",
            title = "Total time for a single net deployment",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  numericInput(width = "85%",
               inputId = "numInput_numDeploy",
               label = label.help("Total number of deployments", "lbl_numDeploy"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_numDeploy",
            title = "Total number of deployments per trip",
            options = list(container = "body"), placement = "right", trigger = "hover")
)








