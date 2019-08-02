
# eval(parse(text=paste(1:5,collapse="*")))

# 12 / (eval(parse(text=paste(75:(75-14),collapse="*"))) / factorial(14))


# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {

  # identify theme to use
  bingo_df <- reactive({
      
    wedding_theme <- read.csv("wedding.csv", stringsAsFactors = FALSE)
    starwars_theme <- starwars %>% 
      # keep most popular characters determined by film count
      mutate(film_count = unlist(lapply(films,length))) %>% 
      arrange(desc(film_count)) %>% 
      slice(1:50) %>% distinct(name) %>% rename(tiles = name)
    number_theme <- data.frame(tiles = 1:75)
    
      if(input$theme!="Upload Custom List"){
        
        dataset_list <- list(
          "Wedding" = wedding_theme
          , "Star Wars" = starwars_theme
          , "Numbers" = number_theme)
        
        bingo_df <- data.frame(
          dataset_list[[input$theme]])
        
      } else {
        
      inFile <- input$custom_list
      
      if (is.null(inFile))
        return(NULL)
      
      bingo_df <- read.csv(inFile$datapath,
                          stringsAsFactors = FALSE,
                          na.strings = c("","NA","N/A"," ")
                          #,header = input$header, sep = input$sep, quote = input$quote
      )
      
      }
    
    validate(
      need(nrow(bingo_df) > 24, "You must have at least 25 items (rows) in your file.")
    )
    
    validate(
      need(is.data.frame(bingo_df), "You must upload a CSV file if you want to use a Custom List.")
    )
    
    # rename first column to 'tiles'
    names(bingo_df)[1] <- "tiles"
    
    # avoid duplicates and cap text
    bingo_df <- bingo_df %>%
      mutate(Tiles = str_to_title(tiles)) %>%
      distinct(Tiles)
    
    bingo_df
    })
    
  output$download_bingo <- downloadHandler(
    filename = "bingo.pdf",
    content = function(file) {
      withProgress(message = 'Creating Bingo Boards', value = 0, {
        
        if(input$page_layout==TRUE){
        ggsave("www/bingo.pdf"
               , gridExtra::marrangeGrob(grobs = plot_list(), nrow=as.numeric(input$boards_per_page), ncol=1,top = NULL))
        } else {
        ggsave("www/bingo.pdf"
               , gridExtra::marrangeGrob(grobs = plot_list(), nrow=1, ncol=as.numeric(input$boards_per_page),top = NULL)
               ,width=11, height=8.5)
        } # else
      }) # withProgress

      file.copy("www/bingo.pdf", file)
    }
  )
  
  # if custom list is selected, show a file upload option.
  observeEvent(input$theme, {
    
    if(input$theme == "Upload Custom List"){
    insertUI(
      selector = "#file_upload_placeholder",
      where = "beforeBegin",
      ui = div(id = "custom_list_container"
               , fileInput("custom_list", "Upload your own CSV file:",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"))
      )
    )
    } else {
      removeUI(selector = "#custom_list_container")
    }
    
  })
  
    # output$file_rows <- renderInfoBox({
    #   
    #   tdata <- ifelse(is.null(bingo_df()),0,nrow(bingo_df()))
    #   
    #   infoBox(tdata,
    #            title = "Number of Rows",
    #            icon=icon("align-justify"),
    #            color="blue")
    # })
  
    output$preview <- renderPlot(height = 500, {
      
      validate(
        need(!is.null(bingo_df()), "You must upload a CSV file if you want to use a Custom List.")
      )
      
      plot_df <- grid_df %>%
        mutate(Tiles = str_wrap(sample(bingo_df()$Tiles,25),width = 8)) %>%
        # force center to be a free space, separate variable to give it unique aesthetics
        mutate(Tiles = ifelse(x==3 & y==3,input$free_space,Tiles))
      
      # clear free space when user selects a shape/image
      if(input$free_space %in% c("Heart","blank")) {
        plot_df <- plot_df %>%
          mutate(Tiles = ifelse(x==3 & y==3,NA,Tiles))
      }
      
      temp_plot <- ggplot(plot_df, aes(x, y, width = w)) +
        geom_tile(color = input$tile_lines,fill=input$tile_color) +
        geom_text(aes(label=Tiles), color = input$tile_text_color, size = input$tile_text_size/ggplot2:::.pt) +
        geom_text(data = data.frame(
          head = unlist(strsplit(input$head_letters,"")),x=1:5,y=6,w=1)
          , aes(x=x,y=y,label=head)
          , color = input$head_text_color
          , size = input$head_text_size/ggplot2:::.pt) +
        scale_y_continuous(limits = c(0,6.2)) +
        theme(panel.background = element_rect(fill=input$panel_color,color = input$panel_color)
              , text = element_text(family = input$font)
              , panel.grid = element_blank()
              , axis.text = element_blank()
              , axis.title = element_blank()
              , axis.line = element_blank()
              , axis.ticks = element_blank()
              , panel.border = element_rect(colour = input$panel_outline, fill=NA, size=5)
              , plot.margin=grid::unit(
                c(input$top_margin,input$right_margin,input$bottom_margin,input$right_margin)
                , "in")
        )
      
      # add free space shape/image if selected
      if(input$free_space=="Heart") {
        temp_plot <- temp_plot + 
          annotation_custom(g, xmin=2.5, xmax=3.5, ymin=2.5, ymax=3.5) # adds custom image to free space
      }
      
      temp_plot
    })
    
    observeEvent(input$generate_pdf_preview, {
    output$pdf_preview <- renderUI({
      isolate({
      # save preview pdf
      if(input$page_layout==TRUE){
        ggsave("www/preview_bingo.pdf"
               , gridExtra::marrangeGrob(grobs = plot_list()[1:2], nrow=as.numeric(input$boards_per_page), ncol=1,top = NULL))
      } else {
        ggsave("www/preview_bingo.pdf"
               , gridExtra::marrangeGrob(grobs = plot_list()[1:2], nrow=1, ncol=as.numeric(input$boards_per_page),top = NULL)
               ,width=11, height=8.5)
      } # else
      }) # isolate
      tags$iframe(style="height:600px; width:100%", src="preview_bingo.pdf")
    })
    })

    plot_list <- reactive({
      
      # initialize list to store plots
      plot_list = list()
      
      for(i in 1:input$boards){
        plot_df <- grid_df %>%
          mutate(Tiles = str_wrap(sample(bingo_df()$Tiles,25),width = 8)) %>%
          # force center to be a free space, separate variable to give it unique aesthetics
          mutate(Tiles = ifelse(x==3 & y==3,input$free_space,Tiles))
        
        # clear free space when user selects a shape/image
        if(input$free_space %in% c("Heart","blank")) {
          plot_df <- plot_df %>%
            mutate(Tiles = ifelse(x==3 & y==3,NA,Tiles))
        }
        
        temp_plot <- ggplot(plot_df, aes(x, y, width = w)) +
          geom_tile(color = input$tile_lines,fill=input$tile_color) +
          geom_text(aes(label=Tiles), color = input$tile_text_color, size = input$tile_text_size/ggplot2:::.pt) +
          geom_text(data = data.frame(
            head = unlist(strsplit(input$head_letters,"")),x=1:5,y=6,w=1)
            , aes(x=x,y=y,label=head)
            , color = input$head_text_color
            , size = input$head_text_size/ggplot2:::.pt) +
          scale_y_continuous(limits = c(0,6.2)) +
          theme(panel.background = element_rect(fill=input$panel_color, color = input$panel_color)
                , text = element_text(family = input$font)
                , panel.grid = element_blank()
                , axis.text = element_blank()
                , axis.title = element_blank()
                , axis.line = element_blank()
                , axis.ticks = element_blank()
                , panel.border = element_rect(colour = input$panel_outline, fill=NA, size=5)
                , plot.margin=grid::unit(
                  c(input$top_margin,input$right_margin,input$bottom_margin,input$right_margin)
                  , "in")
          )
        
        if(input$free_space=="Heart") {
          temp_plot <- temp_plot + 
            annotation_custom(g, xmin=2.5, xmax=3.5, ymin=2.5, ymax=3.5) # adds custom image to free space
        }
        
        plot_list[[i]] <- temp_plot
        
      }
      plot_list
    })
    
    
})