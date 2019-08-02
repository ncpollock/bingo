
# letter validation on BINGO headers
# customize fonts: https://stackoverflow.com/questions/55100069/ggplot-with-customized-font-not-showing-properly-on-shinyapps-io/55158772#55158772
# 12 / (prod(75:71) / factorial(5))
# color picker

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
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(stringr)
library(png) # for displaying heart image
library(grid)
library(shinyWidgets)
library(gridExtra)
library(colourpicker)

# create grids
grid_df <- data.frame(
  x = rep(1:5, 5),
  y = rep(1:5, each = 5),
  w = 1
)

# set special image(s) for free space
heart <- readPNG("www/heart.png")
g <- rasterGrob(heart, interpolate=TRUE)

tiles <- 25

plot_font <- "Courier"

if(Sys.info()[['sysname']] == "Linux"){
  dir.create('~/.fonts')
  file.copy("www/Bonbon-Regular.ttf", "~/.fonts")
  file.copy("www/Butcherman-Regular.ttf", "~/.fonts")
  file.copy("www/ButterflyKids-Regular.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
  
  plot_font <- "Bonbon-Regular"
}

# allow box collapse on title click
title_collapse <- function(x){
  HTML(
    paste0('<strong class="box-title" data-widget="collapse" style="cursor: pointer;">'
           ,x
           ,'</strong>'))
}


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
