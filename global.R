

#########################################

### Whenever the script is published to the web, change this to the latest version and update the version notes
CURRENT.VERSION <- 'v0.1.1'

#########################################


version.notes <- modalDialog(
  h2(CURRENT.VERSION),
  tags$ul(
    tags$li('Distribution sampling added for dive duration in simulation'),
    tags$li('5% and 95% quantiles calculated for simulated estimates'),
    tags$li('density histograms created and added to output for simulations'),
    tags$li('Improved some of the formatting of the PDF reports')
  ),

  title = 'Version notes'

)

############################################################################


how.to <- modalDialog(
  h1('Using the seabird bycatch application'),
  hr(),
  p('There is a substantial amount of baseline data that is loaded automatically as options are selected'),
  p('These data can be altered as needed to create outputs that can be downloaded as a PDF report'),
  h3('Step 1: Fisheries information'),
  tags$ul(
    tags$li('In the left menu bar, click on the Fisheries information tab to access a drop-down list of fishing gear types'),
    tags$li('Each gear type will give a different set of parameters to select in the green fisheries information box in the main panel'),
    tags$li('Information buttons exist over each parameter that can be accessed by hovering over them'),
    tags$li('The user is required to ensure that these parameters are accurate for the analysis')
  ),
  hr(),
  h3('Step 2: Species information'),
  tags$ul(
    tags$li('In the left menu bar, click on the Species information tab to access a drop-down list of species'),
    tags$li('By default, common guillemot is selected. Selecting a new species will load data into the Species Dive Parameters box. It will also allow for the calculation of the availability parameter which takes into account depth and size of fishing gear selected'),
    tags$li('Changing values of dive depth and standard deviation will re-calculate the density histogram and also the bird availability parameter'),
    tags$li('The selected species impacts the estimated density within each region as pre-loaded density estimations will be loaded when a species is selected')
  ),
  hr(),
  h3('Step 3: Spatial information'),
  tags$ul(
    tags$li('In the left menu bar, click on the Spatial information tab to access a drop-down list of marine regions'),
    tags$li('Select the region where the proposed fishing will occur, which will load base data for the selected species into the spatio-temporal information box on the main panel. The region will be highlighted on the map in orange'),
    tags$li('Changing values in the "density at surface" box will change the estimates below the surface'),
    tags$li('When running the analysis, the density estimates below the surface are used in the formula, so user is responsible for accuracy of these values'),
    tags$li('Clicking on the "display marine units" button above the map will load an interactive polygon of Scottish marine administrative units.')
  ),
  hr(),
  h3('Step 4: Run simulation'),
  tags$ul(
    tags$li('All the parameters that will be used in the calculation of encounter rate can be found in the Model output box at the bottom of the main panel'),
    tags$li('These parameters are calculated from the information in the four boxes above'),
    tags$li('By default, these parameters cannot be altered in the Model output box - the base parameters must be changed to change these values'),
    tags$li('The user must select the number of bootstrap simulations to run, and then click the button. The higher the bootstrap number the longer it will take to run'),
    tags$li('Output from the simulation will appear in the simulation output box')
  ),
  hr(),
  
  h3('Step 5: Download report'),
  tags$ul(
    tags$li('Once the simulation has been run, the option to download the report will appear at the bottom of the results screen'),
    tags$li('Select the format for the report output. PDF report is recommended'),
    tags$li('Click download, and the file will be downloaded to your Downloads folder'),
    tags$li('Locate the file and rename if required')
  ),
  hr(),
  
  
  h3('Data toggle button'),
  p('Click on the toggle data boxes button to collapse the data boxes. This is recommended before running the simulations for display purposes')
)



############################################################################

jscode <- paste0("shinyjs.collapse = function(boxid) {",
                 "$('.' + boxid).closest('.box').find('[data-widget=collapse]').click();}")


############################################################################

is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}




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
                 gmodels,
                 foreach,
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
  library(gmodels)
  library(devtools)
  library(shinyStore)
  library(truncnorm)
  library(cowplot)
  library(leaflet)
  library(leaflet.esri)
  library(foreach)
  library(shinycssloaders)
}



options(shiny.sanitize.errors = FALSE)



SMAUs <- readRDS('data/SMAU.rds')


species <- sort(c("Common Guillemot", "Northern Gannet","Atlantic Puffin", 
                  "Razorbill", 
                  "Red-throated Diver"))#"Common Scoter","European Shag", "Long-tailed Duck",
defaultSpecies<- "Common Guillemot"


fish <- sort(c("Herring","Sprat","Cod"))
defaultFish <- "Herring"


gear <- sort(c("Purse seine","Gill net","Trawl","Long-line"))
defaultGear<- "Gill net"


regions <- sort(c("","Forth and Tay","North East", "Moray Firth","Orkney Islands", "Shetland Isles","North Coast",
                  "West Highlands","Outer Hebrides","Argyll","Clyde","Solway"))
defaultRegion <- "Clyde"

regionPalette <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
  
  
  #c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3',
                   #'#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')


###############################################
### Helper functions ###

loadfunction <- function(value){
  if(length(value)>0){
    x <- signif(value,4)
  }else{
    x <- 0
  }
  return(x)
}


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



bootstrapped.plot <- function(BOOTS) {
  BOOTS <- data.frame(BOOTS)
  CIS <- ci(BOOTS$BOOTS)
  QNT <- quantile(BOOTS$BOOTS,c(0.05,0.95))
  
  Ylim <- max(density(BOOTS$BOOTS)$y) + max(density(BOOTS$BOOTS)$y)*.25
  
  ggplot(data=BOOTS,aes(BOOTS)) + 
    geom_histogram(aes(y=..density..),
                   col='grey',
                   fill='dodgerblue',
                   alpha=0.4) + 
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_cartesian(xlim=c(0,max(BOOTS)),ylim=c(Ylim,0))+
    xlab('Bird encounters per day')+
    geom_vline(xintercept=CIS[3],color='red',linetype='solid',size=0.25)+
    geom_vline(xintercept=CIS[2],color='red',linetype='solid',size=0.25)+
    geom_vline(xintercept=QNT[1],color='black',linetype='dashed',size=0.25)+
    geom_vline(xintercept=QNT[2],color='black',linetype='dashed',size=0.25)+
    
    geom_hline(aes(yintercept=10000,color='CI',linetype='CI'),size=0.2)+
    geom_hline(aes(yintercept=10000,color='QT',linetype='QT'),size=0.2)+
    
    scale_color_manual(name='',values=c(CI='red',QT='black'),labels=c('95% CI', '5% and 95% quantiles'))+
    scale_linetype_manual(name='',values=c(CI='solid',QT='dashed'),labels=c('95% CI', '5% and 95% quantiles'))+
    theme(legend.position='bottom')
  
}



### We calculate the density, generally, as per Wilson et al 2007. 
### Wilson et al. (2007) defines the density in the range of impact as DaQ2R / 2R
### Da = Density of animal at any depth
### Q2R is dependent on depth / diving profile.
### 11 Mar 2019:  For now we use the basic model of assuming equal density through the water column
###               Will change this when functions are more solidified

Depth.density <- function(surf.dens,max.dive.depth){
  #D = DAQ2R/2R = Wilson et al (2007)
  #surf.dens in km2  (will be converted to m2)
  #d <- net.depth
  Da <- surf.dens/1000000
  d <-  max.dive.depth
   
  D <- Da / d  
  
  return(D)
}




######## Fishing effort functions based on chosen gear type

Fishing.effort <- function(length=NULL,height=NULL,deployment.time=NULL,number.deployments=NULL,gear.type=NULL,
                           net.diameter=NULL,num.hooks=NULL){
  # Length of net fully deployed in Meters
  # height of net (i.e. from top to bottom)
  # Total time per deployment in hours (will be converted to seconds)
  # Total number of deployments
  
  if(gear.type=='Gill net'){
    ### For this, we assume that the net is covering a block of 1m (width)
    ### This means that Net.area actually becomes m^3.  So... a net of 90m x 30m depth = 2700 m^2 at 1m width = 2700 m^3
    ### We assume this to stay in line with the fact that we are working in number of birds/m^3
    Net.area <- length * height
    Effort <- Net.area#/(deployment.time*3600)
  }else if(gear.type == 'Trawl'){
    
    ### Assume ship travels at 4.2 m/s -> meaning the trawl mouth area * 4.2 m/s gives us the volume fished per second
    
    Net.area <- length * height
    Net.area.covered <- Net.area * 4.2
    Effort <- Net.area.covered
    
  }else if(gear.type == 'Purse seine'){
    ### Here we assume that the purse seine acts like a gill net. We calculate the length of that net
    ### using the diameter of the seine.
    net.length <- 2*pi*(net.diameter/2)
    Net.area <- net.length*height
    Effort <- Net.area#/(deployment.time * 3600)
    
  }else if(gear.type == 'Long-line'){
    ### We assign a volume of 1m3 to each hook
    gear.area <- num.hooks
    Effort <- gear.area#/(deployment.time * 3600)
    
  }
  
  
  return(Effort)
}

###################################################################################################


Bird.availability <- function(dive.duration,dives.per.day,perc.depth){
  ## In here, there will be factors that affect the likilhood that a bird will get caught up in the nets / hooks
 
  dives.per.sec <- dives.per.day / 86400
  
  Ba <- dive.duration * dives.per.sec * perc.depth
  return(Ba)
  
}





###################################################################################################
bootstrap.proportions <- function(mn,mx,avg,stdev,boot.size=1000,gear.top=10,gear.bottom=50){
  
  output <- foreach(i=1:boot.size,.combine='c') %do% {
    X <- data.frame(x=rtruncnorm(10000,a=mn,b=mx,mean=avg,sd=stdev))
    #X <- data.frame(x=rtruncnorm(10000,a=0,b=100,mean=60,sd=20))
    return(Proportion.available(X,gear.top,gear.bottom))
  }
  return(output)
  
}



Do.bootstrap <- function(boot.size=1000,prop.avail,dive.duration,dive.duration.std,dive.duration.max,dives.per.day,Density.profile,F.effort){
  
  Density <- sample(Density.profile$x,boot.size,replace=FALSE)
  FishEffort <- rep(F.effort,times=boot.size)
  
  Dive.durations <- data.frame(x=rtruncnorm(10000,a=0,b=dive.duration.max,mean=dive.duration,sd=dive.duration.std))
  div.dur <- sample(Dive.durations$x,boot.size,replace=FALSE)
  
  DF <- data.frame(div.dur, rep(dives.per.day,times=boot.size),prop.avail)
  
  BirdAvail <- sapply(1:nrow(DF),function(x) Bird.availability(DF[x,1],DF[x,2],DF[x,3]))
  
  ER <- Density * FishEffort * BirdAvail * 86400
  
  return(ER)
}


get.cis <- function(vals){
  CIs <- ci(vals)
  CIestimate <- signif(CIs[1],3)
  
  CIlower <- signif(CIs[2],3)
  if(CIlower < 0){CIlower <- 0}
  
  CIupper <- signif(CIs[3],3)
  CIstderr <- signif(CIs[4],3)
  
  return(data.frame(CIestimate,CIlower,CIupper,CIstderr))
  
}
###################################################################################################


















###############################################################################################
#### Underlying data ####
#########################

Header <- c('Species','Sci.name','Dive.depth','Dive.duration','Max.dive','Max.duration','Std.dive','Std.duration','Dives.per.day')
COGU <- c('Common guillemot','Uria aalge',42.0,76.0,138.0,202.0,50.2,38.0,52)
RAZO <- c('Razorbill','Alca torda',15.0,46.0,140.0,NA,7.10,16.0,397)
ATPU <- c('Atlantic Puffin','Fratercula arctica',35.0,27.0,68.0,115.0,15.3,15.0,333)
RTDI <- c('Red-throated diver','Gavia stellata',NA,NA,NA,NA,NA,NA,NA)
COSC <- c('Common scoter','Melanitta nigra',NA,36.0,NA,NA,NA,NA,NA)
LTDU <- c('Long-tailed duck','Clangula hyemalis',NA,NA,NA,NA,NA,NA,NA)
NOGA <- c('Northern gannet','Morus bassanus',6.0,9.0,34.0,40.0,7.5,6.9,NA)
EUSH <- c('European shag','Phalocrocorax aristotelis',27.0,56.0,61.0,163.0,13.8,15.8,73.7)


Dive.data <- data.frame(do.call('rbind',list(COGU,RAZO,ATPU,RTDI,COSC,LTDU,NOGA,EUSH)))
names(Dive.data) <- Header


################

breeding_table_mean <- read.csv('data/breeding_table.csv')
non_breeding_table_mean <- read.csv('data/nonbreeding_table.csv')
breeding_table_sd <- read.csv('data/breeding_table_sd.csv')
non_breeding_table_sd <- read.csv('data/nonbreeding_table_sd.csv')
breeding_table <- read.csv('data/breeding_table_max.csv')
non_breeding_table <- read.csv('data/nonbreeding_table_max.csv')


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
               inputId = "numInput_timeFishing",
               label = label.help("Total time per deployment (hrs)", "lbl_netDeploy"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_netDeploy",
            title = "Total time for a single net deployment",
            options = list(container = "body"), placement = "right", trigger = "hover"),
  
  numericInput(width = "85%",
               inputId = "numInput_numDeployments",
               label = label.help("Total number of deployments", "lbl_numDeploy"),
               value = 10, min = 1, step = 1),
  bsTooltip(id = "lbl_numDeploy",
            title = "Total number of deployments per trip",
            options = list(container = "body"), placement = "right", trigger = "hover")
)









