
# letter validation on BINGO headers
# 12 / (prod(75:71) / factorial(5))
# add number of items in list next to theme selector

sidebar_width <- "450px"

#initialize colors
v_light_gray <- '#a3a3a3' #'#CDCDCD'
v_light_gray2 <- '#bebebe'
med_gray <- '#696969' #"#808080"
v_dark_gray <- '#323232' #'#252525'
v_dark_gray2 <- '#595959'
plum <- '#24292e' # '#8c001a'
chocolate <- '#7d430e'
sidebar_gray <- '#222d32'

#test color pallette
electric_lime <- '#88D317'
electric_blue <- '#4be0f6'
sunshine <- '#fda302' # orange
shadow <- '#535353'
cyan <- '#43c0f5'
charcoal <- '#3d3d3d'

library(shiny)
library(shinyBS) # for tooltips
library(shinydashboard)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)
library(png) # for displaying heart image
library(grid)
library(shinyWidgets)
library(gridExtra)
library(colourpicker)
library(showtext) # use other fonts in ggplot2
library(curl) # required for showtext?

# load pre-includes themes
wedding_theme <- read.csv("wedding.csv", stringsAsFactors = FALSE)
animal_theme <-  read.csv("animals.csv", stringsAsFactors = FALSE)
baby_theme <-  read.csv("baby.csv", stringsAsFactors = FALSE)
starwars_theme <- starwars %>% 
  # keep most popular characters determined by film count
  mutate(film_count = unlist(lapply(films,length))) %>% 
  arrange(desc(film_count)) %>% 
  slice(1:50) %>% distinct(name) %>% rename(tiles = name)
number_theme <- data.frame(tiles = 1:75)

# create grids
grid_df <- data.frame(
  x = rep(1:5, 5), # horizontal tiles
  y = rep(1:5, each = 5), # vertical tiles
  w = 1 # tile width
)

# set special image(s) for free space
heart <- readPNG("www/heart.png")
g <- rasterGrob(heart, interpolate=TRUE)

tiles <- 25

simulations <- 1000 # how many simulations / simulated games

showtext_opts(dpi = 225)
showtext_auto() # automatically spin up showtext for all graphics devices
font_add_google("Lobster") # add Lobster from Google!
font_add_google("Bilbo")

font_paths("www/") # tell showtext there are font files in www directory
# make fonts available from www directory
font_add("Bonbon","Bonbon-Regular.ttf")
font_add("Butcherman","Butcherman-Regular.ttf")
font_add("Butterfly Kids","ButterflyKids-Regular.ttf")
font_add("Beth Ellen","BethEllen-Regular.ttf")
font_add("Saira Stencil One","SairaStencilOne-Regular.ttf")
font_add("Anton","Anton-Regular.ttf")

# install fonts on shinyapps.io Linux servers
  # only needed for .css file to pickup and use for font preview in selectinput
if(Sys.info()[['sysname']] == "Linux"){
  dir.create('~/.fonts')
  file.copy("www/Bonbon-Regular.ttf", "~/.fonts")
  file.copy("www/Butcherman-Regular.ttf", "~/.fonts")
  file.copy("www/ButterflyKids-Regular.ttf", "~/.fonts")
  file.copy("www/Anton-Regular.ttf", "~/.fonts")
  file.copy("www/BethEllen-Regular.ttf", "~/.fonts")
  file.copy("www/SairaStencilOne-Regular.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
}

# allow box collapse on title click
title_collapse <- function(x){
  HTML(
    paste0('<strong class="box-title" data-widget="collapse" style="cursor: pointer;">'
           ,x
           ,'</strong>'))
}


ggplot_theme <- theme(panel.background = element_blank(),
                  axis.text = element_text(size = '15'),
                  axis.title = element_text(size = '15'),
                  plot.title = element_text(size = '18',face = 'bold',hjust = 0.5),
                  axis.line = element_line(color = 'black'),
                  strip.background = element_rect(fill = 'black'),
                  strip.text = element_text(color = 'white',size = '18'),
                  legend.position = "top",
                  legend.text = element_text(size = '18'),
                  panel.grid.major.y = element_line(color="gray"),
                  panel.grid.major.x = element_blank()
                  )

# use custom color pallette across app
custom_colors <- HTML(paste0('
                                         /* logo */
                             .skin-blue .main-header .logo {
                             background-color: #141a1d;
                              color: white;
                             }

                             /* logo when hovered */
                             .skin-blue .main-header .logo:hover {
                             background-color: #141a1d;
                              color:','white',';
                             }

                             /* toggle button when hovered  */
                             .skin-blue .main-header .navbar .sidebar-toggle:hover{
                             background-color:',sidebar_gray,';
                              color:black;
                             }
.skin-blue .main-header .navbar .sidebar-toggle {color:white;}

                             /* navbar (rest of the header) */
                             .skin-blue .main-header .navbar {
                             background-color:',sidebar_gray,';
                             }

                             /* main sidebar */
/* disabled
                             .skin-blue .main-sidebar {
                             background-color:',v_dark_gray,';
                             }

                             .skin-blue .sidebar-menu > li:hover > a,
                             .skin-blue .sidebar-menu > li.active > a {
                             color: white;
                             background:',cyan,';
                             border-left-color:',cyan,';
                             }
                             .skin-blue .sidebar-menu > li > .treeview-menu {
                             margin: 0 1px;
                             background:',med_gray,';
                             }
                             .skin-blue .treeview-menu > li.active > a,
                             .skin-blue .treeview-menu > li > a:hover {
                             color: white;
                             background:',cyan,';
                             }

                             .skin-blue .sidebar a {
                             color: white;
                             }
                             .skin-blue .treeview-menu > li > a {
                             color: white;
                             }

                             .small-box h3 {
                             font-size: 38px;
                             font-weight: 700;
                             margin: 0 0 10px;
                             white-space: nowrap;
                             padding: 0;
                             }
                             .bg-primary {
                             color: #fff;
                             background-color: #337ab7;
                             }

*/
                             '))
