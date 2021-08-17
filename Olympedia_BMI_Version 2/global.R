library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(viridis)
library(lubridate)
library(ggiraph)
library(glue)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
options(shiny.usecairo=T)


olympic_athlete <- read.csv("athlete_events.csv") #main data
olympic_country <- read.csv("country_definitions.csv") #supporting data
athlete_def <- read.csv("athlete_events_data_dictionary.csv") #definition each coloumn from athlete_events.csv
country_def <-  read.csv("country_definitions_data_dictionary.csv") #definition each coloumn from country_definiitons.csv

olympic <- merge(olympic_athlete, olympic_country, by.x = "NOC", by.y = "NOC", all.x = T)
olympic_raw <- cbind(olympic)

olympic[,c("Sex","Team","NOC","Games","Season","City","Sport","Medal","Event","region")] <-
  lapply(olympic[,c("Sex","Team","NOC","Games","Season","City","Sport","Medal","Event","region")], as.factor)


olympic <- olympic[!is.na(olympic$Age),]
olympic <- olympic[!is.na(olympic$Height),]
olympic <- olympic[!is.na(olympic$Weight),]
olympic <- olympic[!is.na(olympic$region),]

no_medal <- olympic$Medal
no_medal <- as.character(no_medal)
no_medal[is.na(no_medal)] <- "No_Medal"
no_medal <- as.factor(no_medal)
olympic$Medal <- no_medal

olympic$notes <- NULL

olympic <- olympic %>%
  mutate(BMI = Weight/(Height/100)^2) %>%
  mutate_at(vars(BMI), funs(round(., 2)))

olympic <- olympic %>%
  mutate(BMI_category = case_when(BMI < 18.5 ~ "Underweight",
                                  BMI <= 24.9 ~ "Normal",
                                  BMI <= 29.9 ~ "Overweight",
                                  BMI <= 39.9 ~ "Obese",
                                  BMI >39.9 ~ "Morbidly Obese"))

olympic <- olympic %>%
  pivot_wider(
    names_from = Medal,
    values_from = Medal,
    values_fn = length,
    values_fill = 0) %>%
  mutate(Total_Medal = Bronze + Silver + Gold)

olympic$No_Medal <- NULL

olympic_raw <- olympic_raw %>%
  pivot_wider(
    names_from = Medal,
    values_from = Medal,
    values_fn = length,
    values_fill = 0) %>%
  mutate(Total_Medal = Bronze + Silver + Gold)

olympic_raw$No_Medal <- NULL
olympic_raw$"NA" <- NULL
olympic_raw$notes <- NULL


source("ui.R")
source("server.R")




shinyApp(ui, server)
