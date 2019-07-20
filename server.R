


# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
    file_df <- reactive({
      
      if(input$select_dataset!="Upload Custom List"){
        
        dataset_list <- list(PlantGrowth = PlantGrowth
                             ,iris = iris
                             ,diamonds = diamonds)
        
        file_df <- data.frame(
          dataset_list[[input$select_dataset]])
        
      } else {
        
      inFile <- input$data_file
      
      if (is.null(inFile))
        return(NULL)
      
      file_df <- read.csv(inFile$datapath,
                          stringsAsFactors = FALSE,
                          na.strings = c("","NA","N/A"," ")
                          #,header = input$header, sep = input$sep, quote = input$quote
      )
      }
      
      file_df
    })
    
    output$full_dataset <- renderDataTable({
      
      datatable(file_df(),filter='top',options=list(scrollX=TRUE))

    })
  
    output$file_rows <- renderInfoBox({
      
      tdata <- ifelse(is.null(file_df()),0,nrow(file_df()))
      
      infoBox(tdata,
               title = "Number of Rows",
               icon=icon("align-justify"),
               color="blue")
    })
    
    output$file_size <- renderInfoBox({
      
      tdata <- ifelse(is.null(file_df()),0,
                      format(object.size(file_df()),units="b"))

      infoBox(value = p(tdata,br(),
              format(object.size(file_df()),units="Mb")),
               title="Dataset Size",
               icon=icon("expand-arrows-alt"),
               color="blue")
    })
    

      
    
    
})