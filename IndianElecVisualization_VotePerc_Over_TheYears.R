#This code displays animation chart for vote % over the years for parties Congress and BJP
#Data source given below
##devtools::install_github('thomasp85/gganimate') load latest gganimate packages
#load the packages
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(gganimate)
library(gifski) #part of gganimate



#get the data

Results <- read.csv('D:/Harish/R practice projects/india/election/Data/Voting_Per_Over_The_Years_BJP_INC.csv', stringsAsFactors = F, na.strings=c("","NA"))

#EDA
typeof(Results)
head(Results)
sum(is.na(Results))
str(Results)
nrow(Results)

#1 Scatter Plot
ind_1 <- ggplot(data = Results, aes(x = Year, y = Votes_PERCEN, colour = Party)) +
  geom_line() +
             scale_color_manual(values=c('BJP'='#FF8C00',
                              'INC'='#0000FF')) +
  labs(subtitle="Indian Elections Voting % Share Over the Years", 
       y="Voting %", 
       x="Party", 
       title="Scatterplot", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in, indiavotes.com & wiki") 

ind_1 


#animation chart with geom points
theme_set(theme_bw())
#Result_Sel_BJP <- Results %>% filter(Results$Party == 'BJP' )

ind_2 <- ggplot(Results, aes(Year, Votes_PERCEN, size = Votes_PERCEN, frame = Year, colour = Party)) +
  geom_point() + transition_time(Year) +
  labs(title = "Year : {frame_time}") + shadow_wake(wake_length = 0.1, alpha = FALSE) +
  labs(subtitle="BJP & COngress  % Share Over the Years", 
           caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in, indiavotes.com & wiki")
  
ind_2 
#saving the animation chart

anim_save("animation_chart_with_geom_points.gif")


#1 animation chart with geom  lines
ind_3 <- ggplot(data = Results, aes(x = Year, y = Votes_PERCEN, colour = Party)) +
  geom_line(size = 1) +
  scale_color_manual(values=c('BJP'='#FF8C00',
                              'INC'='#0000FF')) +
  labs(subtitle="BJP & COngress  % Share Over the Years", 
       y="Voting %", 
       x="Party", 
       title="Scatterplot", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in, indiavotes.com & wiki") +
  transition_reveal(Year)

ind_3 

anim_save("animation_chart_with_geom_lines.gif")
# end of the code