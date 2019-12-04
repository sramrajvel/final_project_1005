#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(plotly)

data_1 <- read_rds("clean_data/data_1.rds")
data_2 <- read_rds("clean_data/data_2.rds")

ui <- navbarPage("Homelessness in the U.S.",
                 tabPanel("Overview",
                          h1("Background"),
                          textOutput("text_1"),
                          h3("Contact"),
                          textOutput("text_2"),
                          h3("Data"),
                          textOutput("text_3")
                 ),
                 tabPanel("Overall",
                          h2("General trends"),
                          sidebarPanel(
                              selectInput("state", "Select a state", list("Total", 
                                                                          `Northeast` = list("ME", "VT", "NH", "MA", "RI", "PA", "NY", "NJ", "CT"),
                                                                          `West` = list("WA", "OR", "CA", "NV", "ID", "AZ", "UT", "MT", "WY", "CO", "NM"),
                                                                          `Midwest` = list("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH"),
                                                                          `South` = list("TX", "OK", "AR", "LA", "MS", "KY", "TN", "AL", "GA", "FL", "MD", "DE", "SC", "NC", "VA", "WV"),
                                                                          `Other` = list("AK", "HI")
                                                                          ), selected = "Total"), 
                              width = 2),
                          mainPanel(
                              plotlyOutput("plot_1"),
                              br(),
                              br(),
                              plotlyOutput("plot_2")
                          )
                 ),
                 tabPanel("Model",
                          h2("How effectively is our country meeting the housing needs of its homeless population?"),
                          br(),
                          sidebarPanel(
                              selectInput("year", "Select a year", c("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007"), selected = "2018"), 
                              width = 2),
                          mainPanel(
                              plotlyOutput("plot_3"),
                              br(),
                              br(),
                              plotlyOutput("plot_4")
                          )
                 )
)

server <- function(input, output) {
    
    output$plot_1 <- renderPlotly({
        plot_1 <- data_1 %>%
            filter(State == input$state) %>%
            ggplot(aes(x = year, y = `Overall Homeless`)) +
            geom_point() +
            geom_path(aes(group = 1, color = State)) +
            labs(title = "Trends in number of people experiencing homelessness from 2007 - 2018")
    })
    
    output$plot_2 <- renderPlotly({
        plot_2 <- data_2 %>%
            filter(State == input$state) %>%
            ggplot(aes(x = year, y = `Total Year-Round Beds (ES, TH, SH)`)) +
            geom_point() +
            geom_path(aes(group = 1, color = State)) +
            labs(title = "Trends in number of beds available for people who experience homelessness from 2007 - 2018")
    })
    
    output$plot_3 <- renderPlotly({
        plot_3 <- merge(data_1, data_2, by = c("year", "State")) %>%
            filter(`Overall Homeless` < 150000) %>%
            filter(year == input$year) %>%
            ggplot(aes(x = `Overall Homeless`, y = `Total Year-Round Beds (ES, TH, SH)`)) +
            geom_jitter() +
            geom_smooth(method = lm, se = FALSE) +
            labs(title = "Relationship between number of beds available and homeless individuals")
    })
    
    output$plot_4 <- renderPlotly({
        plot_4 <- merge(data_1, data_2, by = c("year", "State")) %>%
            filter(year == input$year) %>%
            filter(State != "Total") %>%
            mutate(percent = 100*`Unsheltered Homeless` / `Overall Homeless`) %>%
            ggplot(aes(x = State, y = percent, color = State)) +
            geom_col() +
            ylim(0,100) +
            labs(title = "Annual percentage of unsheltered homeless individuals")
    })
    
    output$text_1 <- renderText({ 
        "My name is Sanjana Ramrajvel, and I'm a sophomore at Harvard studying applied math and psychology. 
        Feel free to contact me at sramrajvel@college.harvard.edu." 
    })
    
    output$text_2 <- renderText({ 
        "My name is Sanjana Ramrajvel, and I'm a sophomore at Harvard studying applied math and psychology. 
        Feel free to contact me at sramrajvel@college.harvard.edu." 
    })
    
    output$text_3 <- renderText({ 
        "These graphics were created from two large datasets containing data on homeslessness and homeless shelters in the United States by state from the years 2007 - 2018.
        Both datasets were taken from the US department of Housing and Urban Development. 

The source code can be found at my GitHub here."
    })
}


shinyApp(ui = ui, server = server)