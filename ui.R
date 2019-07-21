


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
                     , textInput('head_letters',"Choose five letters to be columns:",
                                   value = "BINGO"))
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
    #    , tags$script('window.onload = function() {
    #   function fixBodyHeight() {
    #     var el = $(document.getElementsByClassName("content-wrapper")[0]);
    #     var h = el.height();
    #     el.css("min-height", h + 50 + "px");
    #   };
    #   window.addEventListener("resize", fixBodyHeight);
    #   fixBodyHeight();
    # };')
        , style="font-family: 'Roboto';",
        tags$head(tags$style(custom_colors))
        , p("this is a test if I can push content without tabs?")
        , plotOutput("preview")

          #           infoBoxOutput('file_rows'),
          #           box(solidHeader = TRUE,width=12,title = "Preview Data",status = "success",
          #               dataTableOutput("full_dataset")))),
          


        )
)
)
