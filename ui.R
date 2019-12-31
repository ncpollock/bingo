


ui <- tagList(tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
  dashboardPage(
    dashboardHeader(
    title = "Bingo Generator"
                    , titleWidth = sidebar_width
                    ),
    
    sidebar <- dashboardSidebar(
      width = sidebar_width
      , sidebarMenu(
         br()
          , div(style="text-align:center;vertical-align: middle"
            ,downloadButton("download_bingo","Download Bingo Sheets",width = sidebar_width, height = "100px"
                         # , style="background-color: #4be0f6;"
                         , class = "my_button"
                         ))
         , br()
          , menuItem("Select Your Content", icon = icon("search")
                     , startExpanded = TRUE
                     , selectInput('theme',"Choose a theme: ",
                                   c("Wedding","Animal","Star Wars","Numbers","Upload Custom List"),
                                   selected = "Wedding")
                     , a(id="file_upload_placeholder")
                     , br()
                     , div(downloadButton("download_items","See Full List"
                                          , class = "my_button"), style = "padding-left:17px;")
                     , bsTooltip(id = "download_items", placement = "right", trigger = "hover"
                                 , title = "Download the full list of items used in the selected theme. You could use this list as a starting point to craft and upload your own!" 
                                 )
                     # create modal window and update to validate 5 letters?
                     , textInput('head_letters',"Choose five letters to be columns:"
                                 , value = "BINGO")
                     , selectInput("free_space","What do you want in the free space?:"
                                   , choices = list(
                                     "Text" = list("FREE","Free","X","*")
                                     , "Shape" = list("Heart","☻","☺","♦")
                                     , "Other" = list("blank")))
                     )
          , menuItem("Iron Out the Details",icon = icon("gear")
                     , sliderInput("boards",p(icon("th")," How many game boards do you need?"),2,150,10,width="100%")
                     , radioGroupButtons(
                       inputId = "boards_per_page", label = p(icon("columns")," How many game boards do you want per page?"), 
                       choiceNames = c("One","Two")
                       , choiceValues =  1:2,
                       justified = TRUE, status = "primary",
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                     )
                     , radioGroupButtons(
                       inputId = "page_layout", label = p(icon("file-text")," Page Layout: "), 
                       choiceNames = c(HTML("Portrait: <i class='fa fa-file-pdf-o'></i>"),HTML("Landscape: <i class='fa fa-file-pdf-o fa-rotate-90'></i>"))
                       , choiceValues =  c(TRUE, FALSE)
                       , justified = TRUE, status = "primary",
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                     )
                     , fluidPage(width = sidebar_width, style="white-space: normal;",p(icon("arrows-alt")," Margins (inches):"))
                     , fluidRow(
                       column(3,numericInput("top_margin","Top",.25,min = 0,max=5))
                     , column(3,numericInput("right_margin","Right",.25,min = 0,max=5))
                     , column(3,numericInput("bottom_margin","Bottom",.25,min = 0,max=5))
                     , column(3,numericInput("left_margin","Left",.25,min = 0,max=5))
                     ))
          , menuItem("Colors",icon = icon("paint-brush")
                     , colourInput("head_text_color", "Text for column letters:", "black")
                     , colourInput("tile_text_color", "Text for tiles:", "black")
                     , colourInput("panel_color", "Board Background:", "white")
                     , colourInput("panel_outline", "Board Outline:", "black")
                     , colourInput("tile_color", "Tile Background:", "white")
                     , colourInput("tile_lines", "Tile Outlines:", "black")
                     , br()
                     )
         , menuItem("Font",icon = icon("font")
                    , sliderInput("head_text_size","Column letter Size",1,125,40
                                  ,step = 1,width = "100%")
                    , sliderInput("tile_text_size","Tile letter Size",1,35,14
                                  ,step = 1,width = "100%")
                    , selectInput("font","Choose a Font:"
                                  , choices = list(
                                    "Standard" = list("Arial" = "sans","Times New Roman" = "serif","Courier New" = "mono","Anton")
                                    , "Fancy" = list("Lobster","Beth Ellen","Bilbo","Bonbon","Butterfly Kids")
                                    , "Other" = list("Saira Stencil One","Butcherman")
                                                ))
                    
         )
         # , menuItem("Summary",icon = icon("map-signs")
         #            , textOutput("item_list_length")
         #            , textOutput("number_of_boards")
         # )
         , menuItem("Simulations",icon = icon("clock-o")
                    , fluidPage(width = sidebar_width, style="white-space: normal;"
                                , p("This is an advanced feature. One Thousand Bingo game simulations will run based
                                    on all of your inputs (e.g., # of boards, # of items in your theme). 
                                    Simulations may help you understand how many item calls
                                    your game might take before a prize is award or all prizes are awarded. This is important 
                                    because some games may be very short (e.g., when there are many boards, few items in your theme, and few prizes) 
                                    while some games may be very long (e.g., few boards, many items, and many prizes)."))
                    , sliderInput("prizes",p(icon("trophy")," How many prizes are you giving away?"),1,15,5,width="100%")
                    , actionButton("run_simulation","Run Simulations"
                                   , class = "my_button"
                                   , icon = icon("list")),br()
                    , bsTooltip(id = "run_simulation", placement = "right", trigger = "hover"
                                , title = "Running simulations may take a minute or so.")
         )
          , menuItem("About",icon = icon("question")
                     , fluidPage(width = sidebar_width, style="white-space: normal;"
                    , p("In May 2019, my sister-in-law was going to play Bingo at her wedding shower.
                     As she thought about creating the boards herself or looking for a tool online I jumped
                     at the idea to build it myself in R. Impractical, perhaps, but it was a lot of fun!
                        It took me less time than expected to create a halfway decent board, so I decided to
                        build this shiny app with my spare time. Though functional, it is a work in progress.
                        For example, I am still working on an algorithm that would mitigate the potential 
                        for multiple wins at the same time.")
          ))
            ,br()
            ,br()
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
            tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto")
        )
        , style="font-family: 'Roboto';"
        , tags$head(tags$style(custom_colors))
        , fluidRow(
          box(title = title_collapse("Quick Preview"), solidHeader = TRUE, width = 12, status = 'primary', collapsible = TRUE
              , p("Note that this preview version is fast, but it's distorted. Generate actual PDF
                  previews below when you have the formatting you want.")
              , plotOutput("preview", height = 500)
        ))
        , actionButton('generate_pdf_preview',"Generate Actual PDF Preview"
                       ,icon = icon("file-pdf-o"), class = "my_button", width = "100%")
        , p("Note that this only generates two bingo boards to preview how the actual PDF download will look.")
        , uiOutput("pdf_preview")
        )
)
)
