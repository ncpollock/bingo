


ui <- tagList(tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
  dashboardPage(
    dashboardHeader(
    title = p(HTML("<i class='fa fa-ticket'></i>"),"Bingo Generator")
                    , titleWidth = sidebar_width
                    ),
    
    sidebar <- dashboardSidebar(
      width = sidebar_width
      , sidebarMenu(
         br()
          , downloadButton("download_bingo","Download Bingo Sheets"
                         ,style="text-align:center;background-color: blue; vertical-align: middle")
          , menuItem("Select Your Content", icon = icon("search")
                     , startExpanded = TRUE
                     , selectInput('theme',"Choose a theme: ",
                                   c("Wedding","Star Wars","Numbers","Upload Custom List"),
                                   selected = "Wedding")
                     # show only on "Upload Custom List" selection
                     # use insertui and removeui?
                     , fileInput("data_file", "Upload your own CSV file:",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv"))
                     # create modal window and update to validate 5 letters?
                     , textInput('head_letters',"Choose five letters to be columns:"
                                 , value = "BINGO")
                     , selectInput("free_space","What do you want in the free space?:"
                                   , choices = list(
                                     "Text" = list("FREE","Free","*")
                                     , "Shape" = list("Heart")
                                     , "Other" = list("blank")))
                     
                     )
          , menuItem("Iron Out the Details",icon = icon("gear")
                     , sliderInput("boards","How many game boards do you need?",1,150,10,width="100%")
                     , sliderInput("boards_per_page","How many game boards do you want per page?",1,2,1
                                   ,step = 1,width = "100%")
                     , radioGroupButtons(
                       inputId = "page_layout", label = p(icon("file-text"),"Page Layout: "), 
                       choices = c("Portrait", "Landscape"),
                       justified = TRUE, status = "primary",
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                     )
                     # ,       prettyRadioButtons(inputId = "checkgroup5",
                     #                             label = "Click me!", icon = icon("check"),
                     #                             choices = c("Portrait", "Landscape")
                     #                            # ,icon = icon("document")
                     #                             ,animation = "pulse", status = "default")
                     )
          , menuItem("Colors",icon = icon("paint-brush")
                     , textInput('head_text_color',"Text for column letters:"
                                 , value = "black")
                     , textInput('tile_text_color',"Text for tiles:"
                                 , value = "black")
                     , textInput('panel_color',"Board Background:"
                                 , value = "white")
                     , textInput('tile_color',"Tile Background:"
                                 , value = "white") 
                     , textInput('tile_lines',"Tile Outlines:"
                                 , value = "black")
                     )
         , menuItem("Font",icon = icon("font")
                    , sliderInput("head_text_size","Column letter Size",1,44,14
                                  ,step = .5,width = "100%")
                    , sliderInput("tile_text_size","Column letter Size",1,12,5
                                  ,step = .5,width = "100%")
                    , selectInput("font","Choose a Font:",choices = "Coming Soon!")
         )
          , menuItem("About",icon = icon("question")
                     , fluidPage(width = sidebar_width, style="white-space: normal;"
                    , p("In May 2019, my sister-in-law mentioned she was going to play Bingo at her wedding shower.
                     As she thought about creating them herself or looking for a tool online I immediately jumped
                     at the challenge and thought I would build it in R.")
          ))
            ,br()
            ,br()
         # , img(src="headshot.jpg",id="face-img",align="center")
         # icon("user fa-3x")
         # , div(
         #   a(href="https://ncpollock.github.io/"
         #     ,target="_blank"
         #     , img(src="headshot.jpg",id="face-img",align="left"))
         #   # icon("user fa-pull-right fa-3x") # generic user icon instead of my face
         #   , strong("Developed by: "),
         #   br(),
         #   a(href="https://ncpollock.github.io/"
         #     ,target="_blank"
         #     ,"Noah C. Pollock"),br()
         #  , a(href = "https://github.com/ncpollock/bingo"
         #        ,target="_blank"
         #        ,"Code on GitHub")
         #   ,style = "float:center;padding-left:15px;white-space:nowrap;")
            , p(img(src="headshot.jpg",id="face-img",align="center"),br(),
               strong("Developed by: "),
               br(),
               a(href="https://ncpollock.github.io/"
                 ,target="_blank"
                 ,"Noah C. Pollock"),
               br(),
               a(href = "https://github.com/ncpollock/bingo"
                 ,target="_blank"
                 ,"Code on GitHub"),
               align="center")
        )
    ),
    
    body <- dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto:300italic,400,700")
        )
        , style="font-family: 'Roboto';",
        tags$head(tags$style(custom_colors))
        , fluidPage(style="height: 1000px;"
        , h3("Preview Your Bingo Board!")
        , p("Note that this preview version is likely distorted compared to the downloaded version.")
        , plotOutput("preview")
        )
          #           infoBoxOutput('file_rows'),
          #           box(solidHeader = TRUE,width=12,title = "Preview Data",status = "success",
          #               dataTableOutput("full_dataset")))),
          


        )
)
)
