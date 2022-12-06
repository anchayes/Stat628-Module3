library(shiny)
library(maps)
library(mapdata)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(leaflet)
library(plotly)
library(dplyr)
library(scales)
library(maptools)
library(rgdal)
library(mapproj)
library(classInt)
library(RColorBrewer)
library(sf)
library(shinyWidgets)
library(data.table)
library(reshape)

dflong <- read.csv("total.csv")
filter_sample <- unique(dflong$city)[1]
dflong %>%
  filter( city %in% filter_sample) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  labs(x = "topics", y = expression(gamma))+ 
  ggtitle("Topics distribution in selected city")+
  scale_x_discrete(labels = c("food quality","customer loyalty","staff attitude","serving speed"))+
  theme(axis.text.x=element_text(size=13))
