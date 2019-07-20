


ui <- dashboardPage(
    dashboardHeader(title = "Bingo Generator",
                    titleWidth = 450),
    
    sidebar <- dashboardSidebar(
      width = 450
      , sidebarMenu(
          br()
            ,HTML("<i class='fa fa-ticket' style='display:block; font-size: 50px; text-align: center;'></i>")
            ,br()
          , menuItem("Select Your Content",icon = icon("search")
                     , h3("Select Your Content.")
                     , p("Only a true CSV file with headers will work with this tool.")
                     , selectInput('select_test',"Choose a Dataset Instead: ",
                                   c("Wedding","Star Wars","Numbers","Upload Custom List"),
                                   selected = "Wedding")
                     # show only on "Upload Custom List" selection
                     , fileInput("data_file", "Upload your own CSV file:",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")))
          , menuItem("Iron Out the Details",icon = icon("gear"))
          , menuItem("Formats and Styles",icon = icon("paint-brush"))
          
          # could put in fluidPage() to format nicely
            # toggle button to select or upload?
          , h3("Select Your Content.")
            ,p("Only a true CSV file with headers will work with this tool.")
            ,fileInput("data_file", "Choose a CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            ,p("Don't have any data to upload?")
            ,selectInput('select_dataset',"Choose a Dataset Instead: ",
                        c("Upload","Post-Grad Outcomes"
                          ,"Department of Education API"
                          ,"iris","diamonds","PlantGrowth"),
                        selected = "Upload")
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
        tags$head(tags$style(custom_colors)),
        p("this is a test if I can push content without tabs?")
        # tabItems(
        #   tabItem(tabName = "about",
        #           box(title = "Welcome!",solidHeader = TRUE,status = "primary",width=12,
        #           h3("This dashboard is a Business Intelligence (BI) tool built off of R using Shiny and related packages.
        #               Please consider it a proof of concept rather than a fully featured BI tool. There are many freely available
        #               BI tools with much richer feature sets (e.g., Microsoft Power BI, Tableau, Excel). In fact,
        #             this tool was heavily inspired by RapidMiner and JMP."),
        #           h3("Follow the tabs to the left in numeric order to see what this dashboard can do!")
        #   )),
          # tabItem(tabName = "upload",
          #         fluidRow(
          #           box(title=p(icon("file-copy",class = "fas"),"Upload Data"),
          #               width=12,
          #               p("Only a true CSV file with headers will work with this tool."),
          #               fileInput("data_file", "Choose a CSV File",
          #                         multiple = FALSE,
          #                         accept = c("text/csv",
          #                                    "text/comma-separated-values,text/plain",
          #                                    ".csv")),
          #               h4("Don't have any data to upload?"),
          #               selectInput('select_dataset',"Choose a Dataset Instead: ",
          #                           c("Upload","Post-Grad Outcomes"
          #                             ,"Department of Education API"
          #                             ,"iris","diamonds","PlantGrowth"),
          #                           selected = "Upload")
          #               ),
          #           infoBoxOutput('file_columns'),
          #           infoBoxOutput('file_rows'),
          #           infoBoxOutput('file_size'),
          #           box(solidHeader = TRUE,width=12,title = "Preview Data",status = "success",
          #               dataTableOutput("full_dataset")))),
          


        )
)
