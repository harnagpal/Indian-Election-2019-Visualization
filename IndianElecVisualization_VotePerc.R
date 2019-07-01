#This code displays Indian Elections Data 2019 visualiztion
# For latest data, please go to Election Commission of India site.
#load the packages
library(ggplot2)
library(readr)
library(dplyr)
library(ggalt) # for encircling data


#get the data
Results <- read.csv('D:/Harish/R practice projects/india/election/Data/voteper_2019_fnl_csv.csv', stringsAsFactors = T, na.strings=c("","NA"))

#EDA
typeof(Results)
head(Results)
sum(is.na(Results))
str(Results)
nrow(Results)

#1 Scatter Plot
ind_1 <- ggplot(data = Results, aes(x = Party, y = Vote_Percentage, colour = Party)) +
  geom_point(aes(size=Vote_Percentage)) +
             scale_color_manual(values=c('BJP'='#FF8C00',
                              'Congress'='#0000FF',
                              'DMK'='#00FA9A',
                              'TMC'='#20B2AA',
                              'YSRC'='#B8860B',
                              'Shiv Sena'='#B22222',
                              'BSP'='#ADFF2F',
                              'TDP'='#F08080',
                              'Samajwadi Party'='#E9967A',
                              'Others'='#FA8072')) +
  labs(subtitle="Indian Elections 2019 Voting % Share", 
       y="Voting %", 
       x="Party", 
       title="Scatterplot", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki")
                           
print(ind_1)


#Scatter plot with Encircle
Highest_vote_per <- Results[Results$Vote_Percentage >= 30.00, ]

ind_2 <- ggplot(data = Results, aes(x = Party, y = Vote_Percentage, colour = Party)) +
  geom_point(aes(size=Vote_Percentage)) +
  scale_color_manual(values=c('BJP'='#FF8C00',
                              'Congress'='#0000FF',
                              'DMK'='#00FA9A',
                              'TMC'='#20B2AA',
                              'YSRC'='#B8860B',
                              'Shiv Sena'='#B22222',
                              'BSP'='#ADFF2F',
                              'TDP'='#F08080',
                              'Samajwadi Party'='#E9967A',
                              'Others'='#FA8072')) +
  labs(subtitle="Indian Elections 2019 Voting % Share", 
       y="Voting %", 
       x="Party", 
       title="Scatterplot", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki") +
  geom_encircle(aes(x=Party, y=Vote_Percentage), 
                data=Highest_vote_per, 
                color="blue")

print(ind_2)
                                                                                  
                 
#Lollipop chart

ind_3 <- ggplot(data = Results, aes(x = Party, y = Vote_Percentage, colour = Party)) +
  geom_point(aes(size=Vote_Percentage)) +
  scale_color_manual(values=c('BJP'='#FF8C00',
                              'Congress'='#0000FF',
                              'DMK'='#00FA9A',
                              'TMC'='#20B2AA',
                              'YSRC'='#B8860B',
                              'Shiv Sena'='#B22222',
                              'BSP'='#ADFF2F',
                              'TDP'='#F08080',
                              'Samajwadi Party'='#E9967A',
                              'Others'='#FA8072')) +
  labs(subtitle="Indian Elections 2019 Voting % Share", 
       y="Voting %", 
       x="Party", 
       title="Lolliport chart", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki") +
  geom_segment(aes(y = 0, 
                   x = `Party`, 
                   yend = Vote_Percentage, 
                   xend = `Party`), 
               color = "black") 
print(ind_3)



# pie chart

Results <- Results %>%
  arrange(desc(Party)) %>%
  mutate(lab.ypos = cumsum(Vote_Percentage) - 0.5*Vote_Percentage)
Results

ggplot(Results, aes(x = "", y = Vote_Percentage, fill = Party)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Vote_Percentage), color = "white")+
  scale_color_manual(values=c('BJP'='#FF8C00',
                              'Congress'='#0000FF',
                              'DMK'='#00FA9A',
                              'TMC'='#20B2AA',
                              'YSRC'='#B8860B',
                              'Shiv Sena'='#B22222',
                              'BSP'='#ADFF2F',
                              'TDP'='#F08080',
                              'Samajwadi Party'='#E9967A',
                              'Others'='#FA8072')) +
  labs(subtitle="Indian Elections 2019 Voting % Share", 
       y="Voting %", 
       x="Party", 
       title="Pie chart", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki") +
  theme_void()

# Bulls eye

Results <- Results %>%
  arrange(desc(Party)) %>%
  mutate(lab.ypos = cumsum(Vote_Percentage) - 0.5*Vote_Percentage)
Results

ggplot(Results, aes(x = "", y = Vote_Percentage, fill = Party)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar()+
  geom_text(aes(y = lab.ypos, label = Party), color = "black")+
  scale_color_manual(values=c('BJP'='#FF8C00',
                              'Congress'='#0000FF',
                              'DMK'='#00FA9A',
                              'TMC'='#20B2AA',
                              'YSRC'='#B8860B',
                              'Shiv Sena'='#B22222',
                              'BSP'='#ADFF2F',
                              'TDP'='#F08080',
                              'Samajwadi Party'='#E9967A',
                              'Others'='#FA8072')) +
    labs(subtitle="Indian Elections 2019 Voting % Share", 
       y="Voting %", 
       x="Party", 
       title="Bulls eye chart", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki") +
  theme_void()

#Donuts chart

ggplot(Results, aes(x = 2, y = Vote_Percentage, fill = Party)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Vote_Percentage), color = "black")+
    scale_color_manual(values=c('BJP'='#FF8C00',
                              'Congress'='#0000FF',
                              'DMK'='#00FA9A',
                              'TMC'='#20B2AA',
                              'YSRC'='#B8860B',
                              'Shiv Sena'='#B22222',
                              'BSP'='#ADFF2F',
                              'TDP'='#F08080',
                              'Samajwadi Party'='#E9967A',
                              'Others'='#FA8072'))  +
  labs(subtitle="Indian Elections 2019 Voting % Share", 
       y="Voting %", 
       x="Party", 
       title="Donuts chart", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki") +
  theme_void() +
  xlim(0.5, 2.5)



################# end of code