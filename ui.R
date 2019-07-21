


ui <- tagList(tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
  dashboardPage(
    dashboardHeader(
    # title = "Bingo Generator"
    #                 , titleWidth = sidebar_width
                    # disable = TRUE
                    ),
    
    sidebar <- dashboardSidebar(
      width = sidebar_width
      , sidebarMenu(
         br()
          , fluidPage(
          HTML("<i class='fa fa-ticket fa-5x fa-pull-left' style='color: green;'></i>")
          # 'display:block; font-size: 50px; text-align: center;'
          # , br()
          , downloadButton("download_bingo","Download Bingo Sheets",style="align:center;background-color: blue; vertical-align: middle")
          )
          # , br()
          # , br()
          # , br()
          , menuItem("Select Your Content", icon = icon("search")
                     , startExpanded = TRUE
                     , selectInput('theme',"Choose a theme: ",
                                   c("Wedding","Star Wars","Numbers","Upload Custom List"),
                                   selected = "Wedding")
                     # show only on "Upload Custom List" selection
                     , fileInput("data_file", "Upload your own CSV file:",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")))
          , menuItem("Iron Out the Details",icon = icon("gear")
                     , sliderInput("boards","How many game boards do you need?",1,150,10)
                     )
          , menuItem("Formats and Styles",icon = icon("paint-brush")
                     # create modal window and update to validate 5 letters
                     , textInput('head_letters',"Choose five letters to be columns:"
                                   , value = "BINGO")
                     , textInput('head_text_color',"Text color for column letters:"
                                 , value = "black")
                     , textInput('tile_text_color',"Text color for tiles:"
                                 , value = "black")
                     , textInput('panel_color',"Background color:"
                                 , value = "white")

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
