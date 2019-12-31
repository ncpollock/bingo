
# TO-DO:
  # init rules to avoid many bingos at once
    # identify sets eg all B, I, row 1, 2, and diagonals
    # force unique combinations

# I should use sample and pull out 25 without replacement each time
# do this twice as many times as needed?
# then remove the boards with more than one winner at a time?

library(dplyr)
library(stringr)
library(ggplot2)
library(png) # for displaying heart image
library(grid)

bingo_df <- read.csv("wedding.csv"
  , stringsAsFactors = FALSE
)

heart <- readPNG("www/heart.png")
g <- rasterGrob(heart, interpolate=TRUE)

tiles <- 25
guests <- 55

# avoid duplicates and cap text
bingo_df <- bingo_df %>%
  mutate(Gift_Title = str_to_title(Gift)) %>%
  distinct(Gift_Title)

# create grids
grid_df <- data.frame(
  x = rep(1:5, 5),
  y = rep(1:5, each = 5),
  w = 1
)

# Make plots.
plot_list = list()

for(i in 1:guests){
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

# two per page
# ggsave("D:/bingo_two_per_page.pdf", gridExtra::marrangeGrob(grobs = plot_list, nrow=2, ncol=1,top = NULL)
#        , device=cairo_pdf)

ggsave("bingo_one_per_page.pdf", gridExtra::marrangeGrob(grobs = plot_list, nrow=1, ncol=1,top = NULL))

ggsave("bingo_landscape_two.pdf"
       , gridExtra::marrangeGrob(grobs = plot_list, nrow=1, ncol=2,top = NULL)
       ,width=11, height=8.5)

# SIMULATION_TEST_3 #########################################

# create grids
grid_df <- data.frame(
  x = rep(1:5, 5), # horizontal
  y = rep(1:5, each = 5), # vertical
  w = 0 # tile width, unnecessary here
)

# identify diagonal groups
grid_df$diag <- NA
grid_df$diag[c(1,7,19,25)] <- "TLBR"
grid_df$diag[c(5,9,17,21)] <- "BLTR"

boards <- input$boa # how many boards per simulation, you could give each player two boards, or allow players to win more boards
n_prizes <- 6 # how many prizes available per game?
n_tiles <- 90 # how many items to be selected from? # n_tile_options might be a more fitting name
tiles <- 1:n_tiles
board_size <- nrow(grid_df)
board_df <- grid_df


start <- Sys.time()
#for each simulated game

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
# NEED TO ONLY DO THIS WHEN THEY HAVE FIVE CALLED! (or 4 diag) 
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
  ungroup()


start - Sys.time()

# calculate odds of awarding all prizes
compare_n_prizes_df <- win_df %>%
  group_by(simulation) %>%
  arrange(winning_call) %>%
  slice(1:n_prizes) %>% # select last call ie call that wins last prize
  mutate(prize_ind = 1:n()) %>%
  group_by(prize_ind) %>%
  count(winning_call) %>%
  mutate(prob = n/simulations
         , sims = simulations
         , count = sum(n)
         , cum_prob = cumsum(prob))

ggplot(
  compare_n_prizes_df
  , aes(x=winning_call,y=cum_prob
        ,color = cut(prize_ind, breaks=0:n_prizes, labels=1:n_prizes)
        ,group = prize_ind)
) +
  geom_line(size=3) +
  scale_color_manual(drop=FALSE, values=colorRampPalette(c("yellow","red"))(n_prizes)
                     , na.value="#EEEEEE", name="Prizes")


win_df %>%
  group_by(simulation) %>%
  arrange(winning_call) %>%
  slice(1:n_prizes) %>% # select last call ie call that wins last prize
  mutate(prize_ind = 1:n()) %>%
  group_by(prize_ind) %>%
  summarize(avg_call = mean(winning_call)
            , sd_call = sd(winning_call)
            , min_call = min(winning_call)
            , med_call = median(winning_call)
            , max_call = max(winning_call)
  )

ggplot(
  win_df %>%
    group_by(simulation) %>%
    arrange(winning_call) %>%
    slice(1:n_prizes) %>% # select last call ie call that wins last prize
    mutate(prize_ind = 1:n())
  , aes(y=winning_call, x=prize_ind
        ,color = cut(prize_ind, breaks=0:n_prizes, labels=1:n_prizes)
        ,group = prize_ind)
) +
  geom_boxplot() +
  scale_color_manual(drop=FALSE, values=colorRampPalette(c("yellow","red"))(n_prizes)
                     , na.value="#EEEEEE", name="Prizes")

##########################################





# I want to report on:
  # how many rounds until game is over (all prizes are awarded)
  # 



# SANDBOX #############
# 
# all_board_df <- all_board_df %>%
#   bind_rows(
#     board_df %>% 
#       gather("x","y","diag",key="tile_type",value="tile_index")
#   )
# 
# 
# win_df <- bind_rows(
#   all_board_df %>% # find last call for each horizontal win
#     group_by(game,player,x) %>%
#     summarise(win_call = max(order))
#   , all_board_df %>% # find last call for each vertical win
#     group_by(game,player,y) %>%
#     summarise(win_call = max(order))
#   , all_board_df %>% # find last call for each diagonal win
#     filter(!is.na(diag)) %>% 
#     group_by(game,player,diag) %>%
#     summarise(win_call = max(order))
# ) %>%
#   group_by(game,player) %>% # find first win for each player
#   summarise(simulation = game
#             , winning_call = min(win_call))