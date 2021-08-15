
# eval(parse(text=paste(1:5,collapse="*")))

# 12 / (eval(parse(text=paste(75:(75-14),collapse="*"))) / factorial(14))


# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {

  # identify theme to use
  bingo_df <- reactive({
      
    wedding_theme <- read.csv("wedding.csv", stringsAsFactors = FALSE)
    animal_theme <-  read.csv("animals.csv", stringsAsFactors = FALSE)
    baby_theme <-  read.csv("baby.csv", stringsAsFactors = FALSE)
    starwars_theme <- starwars %>% 
      # keep most popular characters determined by film count
      mutate(film_count = unlist(lapply(films,length))) %>% 
      arrange(desc(film_count)) %>% 
      slice(1:50) %>% distinct(name) %>% rename(tiles = name)
    number_theme <- data.frame(tiles = 1:75)
    
      if(input$theme!="Upload Custom List"){
        
        dataset_list <- list(
          "Animal" = animal_theme
          , "Baby" = baby_theme
          , "Star Wars" = starwars_theme
          , "Wedding" = wedding_theme
          , "Numbers" = number_theme
          )
        
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
      ) %>%
        # only keep up to 100 as the game would be incredibly long 
          # and the simulation would likely crash
        slice(1:100) 
      
      }
    
    validate(
      need(nrow(bingo_df) > 24, "You must have at least 25 items (rows) in your file.")
    )
    
    validate(
      need(is.data.frame(bingo_df)
           , "You must upload a CSV file if you want to use a Custom List.")
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
  
  output$download_items <- downloadHandler(
    filename = "bingo_item_list.csv",
    content = function(file) {
      write.csv(bingo_df(),"www/bingo_item_list.csv",row.names = FALSE,na = "")
      file.copy("www/bingo_item_list.csv", file)
    }
  )
  
  # if custom list is selected, show a file upload option.
  observeEvent(input$theme, {
    
    if(input$theme == "Upload Custom List"){
    insertUI(
      selector = "#file_upload_placeholder",
      where = "beforeBegin",
      ui = div(id = "custom_list_container",style = "background-color: green;"
               , fileInput("custom_list", "Upload your own CSV file:",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"))
               , fluidPage(width = sidebar_width, style="white-space: normal;"
                  , p("The file should contain one column where the first row is a header / column title. 
                   Microsoft Excel can 'Save As' a CSV."
                      , "Note: only up to the first 100 items/rows will be used. 
                      In the USA, Bingo typically has 75 items."))
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
  
  # output$item_list_length <- renderText({
  #   tdata <- ifelse(is.null(bingo_df()),0,nrow(bingo_df()))
  #   paste("The total number of items sampled from:",tdata)
  #   })
  # output$number_of_boards <- renderText({
  #   paste("Bingo Boards:",input$boards)
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
        geom_text(aes(label=Tiles)
                  , color = input$tile_text_color
                  , size = input$tile_text_size/ggplot2:::.pt
                  , family = input$font) +
        geom_text(data = data.frame(
          head = unlist(strsplit(input$head_letters,"")),x=1:5,y=6,w=1)
          , aes(x=x,y=y,label=head)
          , color = input$head_text_color
          , size = input$head_text_size/ggplot2:::.pt
          , family = input$font) +
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
          geom_text(aes(label=Tiles)
                    , color = input$tile_text_color
                    , size = input$tile_text_size/ggplot2:::.pt
                    , family = input$font) +
          geom_text(data = data.frame(
            head = unlist(strsplit(input$head_letters,"")),x=1:5,y=6,w=1)
            , aes(x=x,y=y,label=head)
            , color = input$head_text_color
            , size = input$head_text_size/ggplot2:::.pt
            , family = input$font) +
          scale_y_continuous(limits = c(0,6.2)) +
          theme(panel.background = element_rect(fill=input$panel_color, color = input$panel_color)
                , text = element_text(family = input$font)
                # , text = element_text(family = 'Bonbon')
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
    
    
    win_df <- reactive({
      # identify diagonal groups
      grid_df$diag <- NA
      grid_df$diag[c(1,7,19,25)] <- "TLBR"
      grid_df$diag[c(5,9,17,21)] <- "BLTR"
      
      boards <- input$boards # how many boards per simulation, you could give each player two boards, or allow players to win more boards
      n_prizes <- input$prizes # how many prizes available per game?
      n_tiles <- nrow(bingo_df()) # how many items to be selected from? # n_tile_options might be a more fitting name
      tiles <- 1:n_tiles
      board_size <- nrow(grid_df)
      board_df <- grid_df
      
      
      # identify the actual order that bingo tiles will be called for this simulation
      call_df <- do.call(rbind,lapply(1:simulations,function(x){
        data.frame(simulation = x
                   , Tile = sample(tiles,n_tiles)
                   , order = 1:n_tiles
                   , stringsAsFactors = FALSE)
      }))
      
      #clear board
      # board_df <- grid_df
      # init a board for every player within simulation
      board_df <- data.frame(grid_df
                             , simulation = rep(1:simulations,each = boards*board_size)
                             , player = rep(1:boards, each = board_size)
                             # complex because I need to avoid duplicate numbers within a board
                             , Tile = unlist(lapply(1:boards
                                                    , function(x) sample(tiles,board_size)
                             ))
      )
      
      # faster than logic based eg when x = 3 and y = 3 then clear
      board_df$Tile[seq(13,nrow(board_df),25)] <- NA # clear free spaces
      # join player boards with Tile call order for each simulation
      board_df <- board_df %>%
        inner_join(call_df,by=c("Tile","simulation"))
      
      # identify the call that results in a win for each player for each simulation
      win_df <- bind_rows(
        board_df %>% # find last call for each horizontal win
          group_by(simulation,player,x) %>%
          summarise(win_call = max(order)
                    , called = n()) 
        , board_df %>% # find last call for each vertical win
          group_by(simulation,player,y) %>%
          summarise(win_call = max(order)
                    , called = n())
        , board_df %>% # find last call for each diagonal win
          filter(!is.na(diag)) %>%
          group_by(simulation,player,diag) %>%
          summarise(win_call = max(order)
                    , called = n() + 1) # plus one because 3x3 is both TLBR and BRTL
      ) %>%
        filter(called == 5) %>%
        group_by(simulation,player) %>% # find first win for each player for each simulation
        summarise(winning_call = min(win_call)) %>%
        ungroup() %>%
        group_by(simulation) %>%
        arrange(winning_call) %>%
        slice(1:n_prizes) %>% # select last call that wins each n prize per simulation
        mutate(prize_ind = 1:n())
      
      win_df
    })
    
    output$avg_calls_to_win <- renderPlot({
      
      boards <- input$boards # how many boards per simulation, you could give each player two boards, or allow players to win more boards
      n_prizes <- input$prizes # how many prizes available per game?
      n_tiles <- nrow(bingo_df()) # how many items to be selected from? # n_tile_options might be a more fitting name
      tiles <- 1:n_tiles
      board_size <- nrow(grid_df)
      
      pdata <- win_df() %>%
        group_by(prize_ind) %>%
        summarize(avg_call = round(mean(winning_call),1)
                  , sd_call = round(sd(winning_call),1)
                  , low = avg_call-sd_call
                  , hi = avg_call+sd_call)
      
      if(max(pdata$hi) > 50){
        call_breaks <- seq(0,max(pdata$hi),10)
      } else {
        call_breaks <- seq(0,max(pdata$hi),5)
      }
      
      ggplot(pdata
        , aes(y=avg_call, x=prize_ind)) +
        geom_col() +
        geom_errorbar(aes(ymin=low,ymax=hi),width = 0.5) +
        scale_x_continuous(breaks=1:n_prizes) +
        scale_y_continuous(breaks=call_breaks) +
        geom_text(aes(label = avg_call),y = -5) +
        geom_text(aes(label = sd_call),y = -10) +
        annotate(geom="text", x=.5, y=-5, label="Avg:", colour="black",fontface="bold") +
        annotate(geom="text", x=.5, y=-10, label="SD:", colour="black",fontface="bold") +
        labs(title = "Figure 1. Average Items Called Before Winning x Prizes",
             x="Prizes",
             y="Items Called",
             fill = "Prizes") + 
        ggplot_theme
      
    })
    
    output$med_calls_to_win <- renderPlot({
      
      boards <- input$boards # how many boards per simulation, you could give each player two boards, or allow players to win more boards
      n_prizes <- input$prizes # how many prizes available per game?
      n_tiles <- nrow(bingo_df()) # how many items to be selected from? # n_tile_options might be a more fitting name
      tiles <- 1:n_tiles
      board_size <- nrow(grid_df)
      
      if(max(win_df()$winning_call) > 50){
        call_breaks <- seq(0,max(win_df()$winning_call),10)
      } else {
        call_breaks <- seq(0,max(win_df()$winning_call),5)
      }
      
      ggplot(
        win_df()
        , aes(y=winning_call, x=prize_ind
              ,fill = cut(prize_ind, breaks=0:n_prizes, labels=1:n_prizes)
              ,group = prize_ind)
      ) +
        geom_boxplot() +
        scale_fill_manual(drop=FALSE, values=colorRampPalette(c("yellow","red"))(n_prizes)
                           , na.value="#EEEEEE") +
        scale_x_continuous(breaks=1:n_prizes) +
        scale_y_continuous(breaks=call_breaks) +
        labs(title = "Figure 2. Distribution of Items Called Before Winning x Prizes",
             x="Prizes",
             y="Items Called",
             fill = "Prizes") + 
        ggplot_theme +
        theme(legend.position = "none")
    })
    
    output$pct_sims_winning_call <- renderPlot({
      
      boards <- input$boards # how many boards per simulation, you could give each player two boards, or allow players to win more boards
      n_prizes <- input$prizes # how many prizes available per game?
      n_tiles <- nrow(bingo_df()) # how many items to be selected from? # n_tile_options might be a more fitting name
      tiles <- 1:n_tiles
      board_size <- nrow(grid_df)
      
      if(n_tiles>50){
        call_breaks <- seq(0,n_tiles,10)
      } else {
        call_breaks <- seq(0,n_tiles,5)
      }
      
      ggplot(
        win_df() %>%
          group_by(prize_ind) %>%
          count(winning_call) %>%
          mutate(cum_prob = cumsum(n/simulations))
        , aes(x=winning_call,y=cum_prob
              ,color = cut(prize_ind, breaks=0:n_prizes, labels=1:n_prizes)
              ,group = prize_ind)
      ) +
        geom_line(size=2) +
        geom_point(size=3) +
        scale_color_manual(drop=FALSE, values=colorRampPalette(c("yellow","red"))(n_prizes)
                           , na.value="#EEEEEE") +
        labs(title = "Figure 3. Cumulative Probability of n Prizes Won by Items Called",
             x="Items Called",
             y="% of Simulations",
             color = "Prizes") + 
        scale_y_continuous(breaks=seq(0,1,0.1),limits = 0:1,labels=scales::percent) +
        scale_x_continuous(breaks=call_breaks) +
        ggplot_theme +
        theme(panel.grid.major.x = element_line(color="gray"))
    })
    
    # show simulations on button click
    observeEvent(input$run_simulation, {
      showModal(modalDialog(
        title = "Simulate Bingo Games"
        , fluidPage(fluidRow(infoBox("Simulations",1000,icon = icon("list"),width = 6,fill = TRUE,color="navy")
          , infoBox("Boards Per Simulation",input$boards,icon = icon("th"),width = 6,fill = TRUE,color="navy")
          , infoBox("Prizes",input$prizes,icon = icon("trophy"),width = 6,fill = TRUE,color="navy")
          , infoBox("Possible Tiles",nrow(bingo_df()),icon = icon("sitemap"),width = 6,fill = TRUE,color="navy"))
        )
        , size = "l"
        , easyClose = TRUE
        , hr(class = "sepline")
        , plotOutput("avg_calls_to_win")
        , p("This chart demonstrates the average number of items called 
            until a player wins the first prize, the second prize, or the nth prize.")
        , hr(class = "sepline")
        , plotOutput("med_calls_to_win")
        , p("This chart demonstrates a more detailed view of the data than 
            Figure 1 as averages often oversimplify a group of data. 
            Figure 2 shows the distribution of items called until a player
            wins the first prize, second prize, or nth prize.")
        , hr(class = "sepline")
        , plotOutput("pct_sims_winning_call")
        , p("Figure 3 demonstrates the percent of simulations that yield a first win, second win, or nth win by the number of items called. 
            For example, it allows you to make statements such as 50% of all simulations yielded a win by x item called 
            or that 90% of simulations had all prizes awarded by x item called.")
        , footer = modalButton("Close Simulation")
      ))
    })
    
    
})