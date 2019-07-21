


ui <- dashboardPage(
    dashboardHeader(title = "Bingo Generator",
                    titleWidth = sidebar_width),
    
    sidebar <- dashboardSidebar(
      width = sidebar_width
      , sidebarMenu(
          br()
          , HTML("<i class='fa fa-ticket' style='display:block; font-size: 50px; text-align: center;'></i>")
          , br()
          , downloadButton("download_bingo","Download Bingo Sheets",style="text-align: center;background-color: blue;")
          , br()
          , menuItem("Select Your Content"
                     , startExpanded = TRUE
                     ,icon = icon("search")
                     , h3("Select Your Content.")
                     , selectInput('theme',"Choose a theme: ",
                                   c("Wedding","Star Wars","Numbers","Upload Custom List"),
                                   selected = "Wedding")
                     # show only on "Upload Custom List" selection
                     , fileInput("data_file", "Upload your own CSV file:",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")))
          , menuItem("Iron Out the Details",icon = icon("gear"))
          , menuItem("Formats and Styles",icon = icon("paint-brush")
                     # create modal window and update to validate 5 letters
                     , textInput('head_letters',"Choose five letters to be columns:",
                                   value = "BINGO"))
          , menuItem("About"
                     , fluidPage(width = sidebar_width, style="white-space: normal;"
                    , p("In May 2019, my sister-in-law mentioned she was going to play Bingo at her wedding shower.
                     As she thought about creating them herself or looking for a tool online I immediately jumped
                     at the challenge and thought I would build it in R.")
          ))
            ,br()
            ,br()
            ,p(strong("Developed by: "),
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
        ),
        style="font-family: 'Roboto';",
        tags$head(tags$style(custom_colors))
        , p("this is a test if I can push content without tabs?")
        , plotOutput("preview")

          #           infoBoxOutput('file_rows'),
          #           box(solidHeader = TRUE,width=12,title = "Preview Data",status = "success",
          #               dataTableOutput("full_dataset")))),
          


        )
)
