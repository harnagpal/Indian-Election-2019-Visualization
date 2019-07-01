#This code displays Indian Elections Data 2019 in an Indian Parliament chart view.
# For correct data, please go to Election Commission of India site.
#load the packages
library(ggplot2)
library(ggparliament)
library(readr)
library(dplyr)


#get the data
Results <- read.csv('D:/Harish/R practice projects/india/election/Data/NoOfSeats 2019.csv', stringsAsFactors = T, na.strings=c("","NA"))

#EDA
typeof(Results)
head(Results)
sum(is.na(Results))
str(Results)
nrow(Results)
typeof(Results)

#Decide type
Parlia_semicircle <- parliament_data(election_data = Results,
                                       type = "semicircle",
                                       parl_rows = 12,
                                       party_seats = Results$seats)
#EDA
typeof(Parlia_semicircle)
is.na(Parlia_semicircle)
sum(is.na(Parlia_semicircle))
nrow(Parlia_semicircle)
summary(Parlia_semicircle)


ind <- ggplot(Parlia_semicircle, aes(x =x, y = y, colour = party_short)) +
  geom_parliament_seats(stat = "identity",position = "identity",size = 5) + 
  theme_ggparliament() +
  labs(colour = "Parties", 
  title = "Indian 2019 Parliament", legend.position = "center") +
  scale_color_manual(values=c('BJP'='#FF8C00',
                              'Congress'='#0000FF',
                              'DMK'='#00FA9A',
                              'TMC'='#20B2AA',
                              'YSRC'='#B8860B',
                              'Shiv Sena'='#B22222',
                              'JDU'='#FF00FF',
                              'BJD'='#6495ED',
                              'BSP'='#ADFF2F',
                              'TRS'='#FF7F50',
                              'LJP'='#CD5C5C',
                              'NCP'='#F08080',
                              'Samajwadi Party'='#E9967A',
                              'Independent'='#FA8072',
                              'CPM'='#FF0000',
                              'IUML'='#FF4500',
                              'National Conference'='#800000',
                              'TDP'='#9ACD32',
                              'Apna Dal S'='#FFD700',
                              'CPI'='#FF0000',
                              'Shiromani Akali Dal'='#000080',
                              'AAP'='#EEE8AA',
                              'AIADMK'='#E81B23',
                              'AIMIM'='#000000',
                              'AIUDF'='#000000',
                              'AJSU Party'='#FF0000',
                              'JDS'='#00FF00',
                              'JMM'='#20B2AA',
                              'Kerala Congress (M)'='#FFFF00',
                              'MIM'='#00FFFF',
                              'Mizo National Front'='#FF00FF',
                              'NDPP'='#C0C0C0',
                              'NPF'='#808080',
                              'NPP'='#800000',
                              'RLP'='#808000',
                              'RSP'='#008000',
                              'SKM'='#800080',
                              'VCK'='#008080'  )) +
  theme(legend.position = "bottom") +
  geom_highlight_government(government == 1, shape = 25) +
  draw_totalseats(n = 542, type = 'semicircle') +
  draw_majoritythreshold(
    n = 272,
    label = TRUE,
    type = 'semicircle'
  ) +
  theme(plot.title = element_text(hjust = 0.5)) 
 

#Display the plot
ind


