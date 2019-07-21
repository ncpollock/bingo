
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
        
      inFile <- input$data_file
      
      if (is.null(inFile))
        return(NULL)
      
      bingo_df <- read.csv(inFile$datapath,
                          stringsAsFactors = FALSE,
                          na.strings = c("","NA","N/A"," ")
                          #,header = input$header, sep = input$sep, quote = input$quote
      )
      }
      
    
    # avoid duplicates and cap text
    bingo_df <- bingo_df %>%
      mutate(Tiles = str_to_title(tiles)) %>%
      distinct(Tiles)
    
    bingo_df
    })
    
  output$download_bingo <- downloadHandler(
    filename = "bingo.pdf",
    content = function(file) {
      file.copy("www/bingo.pdf", file)
    }
  )
  
  
    # output$full_dataset <- renderDataTable({
    #   
    #   datatable(bingo_df(),filter='top',options=list(scrollX=TRUE))
    # 
    # })
  
    # output$file_rows <- renderInfoBox({
    #   
    #   tdata <- ifelse(is.null(bingo_df()),0,nrow(bingo_df()))
    #   
    #   infoBox(tdata,
    #            title = "Number of Rows",
    #            icon=icon("align-justify"),
    #            color="blue")
    # })
    
    output$preview <- renderPlot(height = 550, {
      
      plot_df <- grid_df %>%
        mutate(Tiles = str_wrap(sample(bingo_df()$Tiles,25),width = 8)) %>%
        # force center to be a free space, separate variable to give it unique aesthetics
        mutate(Tiles = ifelse(x==3 & y==3,"♥",Tiles))
      
      ggplot(plot_df, aes(x, y, width = w)) +
        geom_tile(color = "black",fill=NA) +
        geom_text(aes(label=Tiles)) +
        # annotation_custom(g, xmin=2.5, xmax=3.5, ymin=2.5, ymax=3.5) + # adds custom image to free space
        geom_text(data = data.frame(
          head = unlist(strsplit(input$head_letters,"")),x=1:5,y=6,w=1)
          , aes(x=x,y=y,label=head)
          , size = 14) +
        scale_y_continuous(
          limits = c(0,6.2)
        ) +
        theme(panel.background = element_blank()
              , axis.text = element_blank()
              , axis.title = element_blank()
              , axis.line = element_blank()
              , plot.title = element_text(hjust = 0.5,size = 14)
              , axis.ticks = element_blank()
              , panel.border = element_rect(colour = "black", fill=NA, size=5)
              , plot.margin=grid::unit(rep(.25,4), "in")
        )
      
    })
    

    plot_list <- reactive({
      
      for(i in 1:input$boards){
        plot_df <- grid_df %>%
          mutate(gift = str_wrap(sample(bingo_df$Gift_Title,25),width = 8)) %>%
          # force center to be a free space, separate variable to give it unique aesthetics
          mutate(gift = ifelse(x==3 & y==3,NA,gift))
        
        temp_plot <- ggplot(plot_df, aes(x, y, width = w)) +
          geom_tile(color = "black",fill=NA) +
          geom_text(aes(label=gift)) +
          annotation_custom(g, xmin=2.5, xmax=3.5, ymin=2.5, ymax=3.5) +
          geom_text(data = data.frame(
            head = c("B","I","N","G","O"),x=1:5,y=6,w=1)
            , aes(x=x,y=y,label=head)
            , size = 14) +
          scale_y_continuous(
            limits = c(0,6.2)
          ) +
          theme(panel.background = element_blank()
                , axis.text = element_blank()
                , axis.title = element_blank()
                , axis.line = element_blank()
                , plot.title = element_text(hjust = 0.5,size = 14)
                , axis.ticks = element_blank()
                , panel.border = element_rect(colour = "black", fill=NA, size=5)
                , plot.margin=grid::unit(rep(.25,4), "in")
          )
        
        plot_list[[i]] <- temp_plot
        
      }
    })
    
    
})