
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



# SANDBOX #########################################################################

bingo_df_temp <- bingo_df
ext_bingo_df <- do.call(rbind,lapply(1:8,function(x) bingo_df <- bind_rows(bingo_df_temp,bingo_df)))

test_df <- ext_bingo_df %>%
  mutate(
    board_id = rep(1:nrow(ext_bingo_df),length.out = nrow(ext_bingo_df),each=25)
    , column = rep(c("B","I","N","G","O"),length.out = nrow(ext_bingo_df))
    , row = rep(1:5,length.out = nrow(ext_bingo_df), each = 5)) %>%
  mutate(diagonal = case_when(
    column == "B" & row == 1 ~ 'TB'
    , column == "I" & row == 2 ~ 'TB'
    , column == "N" & row == 3 ~ 'TB'
    , column == "G" & row == 4 ~ 'TB'
    , column == "O" & row == 5 ~ 'TB'
    , column == "B" & row == 5 ~ 'BT'
    , column == "I" & row == 4 ~ 'BT'
    , column == "N" & row == 3 ~ 'BT'
    , column == "G" & row == 2 ~ 'BT'
    , column == "O" & row == 1 ~ 'BT'
  ))

# I just need to know what the winning indices will be!
test_df[1:5,]
test_df$column[3]

# assign 1 to a hit, then if(sum(hit))==5, then win!
  # take the max(index) of all the hit rows?


# # all the winning indexes
# wins <- data.frame(
#   row1 = 1:5
#   , row2 = 6:10
#   , row3 = 11:15
#   , row4 = 16:20
#   , row5 = 21:25
#   , colb = c(1,6,11,16,21)
#   , coli = c(2,7,12,17,22)
#   , coln = c(3,8,13,18,23)
#   , colg = c(4,9,14,19,24)
#   , colo = c(5,10,15,20,25)
#   , diag_tb = c(1,7,13,19,25)
#   , diag_bt = c(5,9,13,17,21)
# )


# simulate bingo winnings ######################
# about 7.5 seconds

# create grids
grid_df <- data.frame(
  x = rep(1:5, 5),
  y = rep(1:5, each = 5),
  w = 0
)

# identify diagonal groups
grid_df$diag <- NA
grid_df$diag[c(1,7,19,25)] <- "TLBR"
grid_df$diag[c(5,9,17,21)] <- "BLTR"

simulations <- 40 # how many simulations / games
boards <- 50 # how many boards per simulation / players
n_prizes <- 6 # how many prizes available per game?
bingo_df <- data.frame(Tile = 1:75)
board_df <- grid_df

# init df for storing winning calls across players and simulations
win_df <- data.frame(
  player = NA
  , simulation = NA
  , winning_call = NA 
  , stringsAsFactors = FALSE
)
start <- Sys.time()
#for each simulated game
for(game in 1:simulations){
  
  # identify the actual order that bingo tiles will be called
  call_df <- data.frame(Tile = sample(bingo_df$Tile,nrow(bingo_df))
                      , order = 1:nrow(bingo_df)
                      , stringsAsFactors = FALSE)
  
  # loop through each player / bingo board
    # eventually deal with the fact that people tend to no-show ie players < boards
  for(player in 1:boards){
    
    # should I lapply for all boards and assign a player number
      # then inner join once?
    
    #clear board
    board_df <- grid_df
    
    board_df$Tile <- sample(bingo_df$Tile,25) # set individual players bingo board
    board_df$Tile[13] <- NA # clear free space
    
    # join player board with Tile call order
    board_df <- board_df %>%
      inner_join(call_df,by="Tile")
    
    # identify the call that results in a win
      # benchmark selecting just win_call before binding rows
      # this is probably the call that takes the longest
    win_calls_df <- bind_rows(
      board_df %>%
        group_by(x) %>%
        summarise(win_call = max(order))
      , board_df %>%
          group_by(y) %>%
          summarise(win_call = max(order))
      , board_df %>%
          group_by(diag) %>%
          summarise(win_call = max(order))
    )

    # identify first winning call
    first_win <- min(win_calls_df$win_call)
    
    # saved .5 second...
    # first_win <- min(c(
    #   (board_df %>%
    #     group_by(x) %>%
    #     summarise(win_call = max(order)))$win_call
    #   , (board_df %>% 
    #     group_by(y) %>%
    #     summarise(win_call = max(order)))$win_call
    #   , (board_df %>%
    #     group_by(diag) %>%
    #     summarise(win_call = max(order)))$win_call
    # ))
    
    # log first winning call and sim number
    win_df <- win_df %>%
      bind_rows(
        data.frame(
          player = player
          , simulation = game
          , winning_call = first_win)
      )
    
    
  } # player
  
  # log the nth winning call in where n = number of prizes
  
  
} # simulations

start - Sys.time()


# TEST 2 #########################################
# about 1.5 seconds

# create grids
grid_df <- data.frame(
  x = rep(1:5, 5),
  y = rep(1:5, each = 5),
  w = 0
)

# identify diagonal groups
grid_df$diag <- NA
grid_df$diag[c(1,7,19,25)] <- "TLBR"
grid_df$diag[c(5,9,17,21)] <- "BLTR"

simulations <- 100 # how many simulations / games
boards <- 50 # how many boards per simulation / players
n_prizes <- 6 # how many prizes available per game?
bingo_df <- data.frame(Tile = 1:75)
board_df <- grid_df

# init df for storing winning calls across players and simulations
win_df <- data.frame(
  player = NA
  , simulation = NA
  , winning_call = NA 
  , stringsAsFactors = FALSE
)
start <- Sys.time()
#for each simulated game
for(game in 1:simulations){
  
  # identify the actual order that bingo tiles will be called
  call_df <- data.frame(Tile = sample(bingo_df$Tile,nrow(bingo_df))
                        , order = 1:nrow(bingo_df)
                        , stringsAsFactors = FALSE)
  
#clear board
board_df <- grid_df
board_df <- do.call(rbind,lapply(1:boards,function(x){data.frame(grid_df,player=x,Tile = sample(bingo_df$Tile,25))}))
board_df$Tile[seq(13,nrow(board_df),25)] <- NA # clear free spaces
# join player board with Tile call order
board_df <- board_df %>%
  inner_join(call_df,by="Tile")

# identify the call that results in a win
# benchmark selecting just win_call before binding rows
# this is probably the call that takes the longest
win_df <- win_df %>%
  bind_rows(
  bind_rows(
  board_df %>%
    group_by(player,x) %>%
    summarise(win_call = max(order))
  , board_df %>%
    group_by(player,y) %>%
    summarise(win_call = max(order))
  , board_df %>%
    filter(!is.na(diag)) %>% 
    group_by(player,diag) %>%
    summarise(win_call = max(order))
) %>%
  group_by(player) %>%
  summarise(simulation = game
            , winning_call = min(win_call))
)

} # simulations

start - Sys.time()

##########################################

win_df <- win_df %>%
  slice(-1) %>% # get rid of init row
  arrange(winning_call)

# plot odds of winning at certain call
win_df %>% 
  # group_by(simulation) %>%
  count(winning_call) %>%
  mutate(prob = n/sum(n)
         , cum_prob = cumsum(prob))

# plot the winning distribution
  # show median
  # show quantiles
ggplot(win_df %>% count(winning_call),aes(x=winning_call,y=n)) + 
  geom_col()

ggplot(
  win_df %>% 
    count(winning_call) %>%
    mutate(prob = n/sum(n)
           , cum_prob = cumsum(prob))
  , aes(x=winning_call,y=cum_prob)
) +
  geom_line()

# calculate odds of awarding all prizes
win_df %>%
  group_by(simulation) %>%
  arrange(winning_call) %>%
  slice(n_prizes) %>% # select last call ie call that wins last prize
  ungroup() %>%
  count(winning_call) %>%
  mutate(prob = n/sum(n)
         , cum_prob = cumsum(prob))

# I want to report on:
  # how many rounds until game is over (all prizes are awarded)
  # 