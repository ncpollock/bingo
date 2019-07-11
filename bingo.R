
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

bingo_df <- read.csv("bingo.csv"
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

