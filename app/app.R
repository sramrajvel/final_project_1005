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
                          br(),
                          textOutput("text_2"),
                          h3("Data & Contact"),
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
                              plotlyOutput("plot_4"),
                              br(),
                              br(),
                              plotlyOutput("plot_3"),
                              br(),
                              br(),
                              plotlyOutput("plot_5"),
                              br(),
                              textOutput("text_4"),
                              br()
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
                geom_smooth(method = "lm", se = FALSE) +
                labs(title = "Relationship between number of beds available and homeless individuals", x = "Average annual number of homeless individuals (by state)")
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
    
    output$plot_5 <- renderPlotly({
        plot_5 <- merge(data_1, data_2, by = c("year", "State")) %>%
            filter(`Overall Homeless` < 150000) %>%
            group_by(year) %>%
            nest() %>%
            mutate(model = map(data, ~ lm(`Total Year-Round Beds (ES, TH, SH)` ~ `Overall Homeless`, data = .x))) %>% 
            mutate(coefficients = map(model, ~ coef(.x))) %>%
            mutate(slope = map_dbl(coefficients, ~ pluck(.x, "`Overall Homeless`"))) %>%
            ggplot(aes(x = year, y = slope)) +
                geom_point() +
                geom_path(aes(group = 1)) +
                labs(title = "How does the regression slope value change over the years?")
    })
    
    output$text_1 <- renderText({ 
        "I was particularly interested in analyzing data related to homelessness because I work at a student-run youth homeless shelter in Harvard Square. 
        Cambrdige has a pretty sizeable youth homeless population, so the 27 overnight beds that we provide usually get filled up on an average night. However,
        the demand varies from night to night. We run three different lotteries for beds a day with the goal of maximizing the number of guests we serve, but 
        on some nights we have empty beds that don't get filled, and on others we have more guests enter the lotteries than there are beds available. In those cases,
        the guests who don't win a bed with us will either try to find another shelter they can go to in the area, or they will find a public location to spend the night." 
    })
    
    output$text_2 <- renderText({
        "With this project, I wanted to examine how well our country meets the shelter needs of its homeless population, specifically by comparing availability of beds 
        with number of people experiencing homelessness. Overall, the data suggest that the U.S. on the whole is improving in matching the need for overnight shelter beds 
        for homeless individuals. However, this data compares the number of people and beds within each state. The issue with this analysis is that the evaluation of 
        effectiveness in serving the homeless population is not entirely accurate. For example, a bed isn’t useful to an individual if it is located on the other side 
        of the state, since the person could not reasonably travel that far to make use of it. We can’t know whether or not individuals or beds are clustered in 
        specific locations. For this reason, I would love to get access to data on homeless populations and shelters by cities or counties and perform a similar analysis. "
    })
    
    output$text_3 <- renderText({ 
        "My name is Sanjana Ramrajvel, and I'm a sophomore at Harvard studying applied math and psychology. These graphics were created from two large datasets 
        containing data on homeslessness and homeless shelters in the United States by state from the years 2007 - 2018. Both datasets were taken from the 
        US department of Housing and Urban Development. The source code for this project can be found at my GitHub here : https://github.com/sramrajvel/final_project_1005. 
        Also, feel free to contact me at sramrajvel@college.harvard.edu." 
    })
    
    output$text_4 <- renderText({
        "Ideally, we would want the number of available shelter beds on any given day to match the number of homeless individuals, 
        or at least for the two numbers to be somewhat close to each other. This would mean that the slope of the regression line in the second graph is near 1.
        Obviously, we know this isn't true. However, we can analyze whether or not this relationship has improved over the years. 
        By extracting the slope value from the regression line for each year, we can plot this value against year. 
        Looking at the last graph, we can observe an overall positive trend, despite the dip in the last few years."
    })
}


shinyApp(ui = ui, server = server)