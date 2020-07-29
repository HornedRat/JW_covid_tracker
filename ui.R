#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


countries <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
    select(`Country/Region`)

countries <- unique(countries$`Country/Region`)

# Define UI for application that draws a histogram
fluidPage(
    
    verticalLayout(
        titlePanel("COVID Data"),
    
        wellPanel(
            selectInput("country",
                "Select country:",
                choices = countries,
                selected = "Poland")),
    fluidRow(
        column(1),
        column(4, 
            plotOutput("active_cases_graph")
        ),
        column(4,
            plotOutput("weekly_increase_graph")
        )
    ),
    fluidRow(
        column(1),
        column(4,
            plotOutput("stringency_graph")
        ),
        column(4,
            plotOutput("RKI_graph")
        )
    )
    )
)

