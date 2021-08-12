library(shiny)
library(shinydashboard)
library(corrr)
library(shinydashboardPlus)
library(tidyverse)
library(RColorBrewer)
#game_total <- read_csv("game_total.csv")

pal <- brewer.pal(8, "Set1")


colnames(game_total) <- str_to_title(colnames(game_total))
game_total <- game_total  %>% 
    rename("Tournament Stage" = Tournament_stage,
           "Mintues Played" = Mp,
           "Field Goals" = Fg,
           "Field Goals Attempted" = Fga,
           "3P" = X3p,
           "3PA" = X3pa,
           "Free Throws" = Ft,
           "Free Throws Attempted" = Fta,
           "Offensive Rebounds" = Orb,
           "Total Rebounds" = Trb,
           "Assists" = Ast,
           "Steals" = Stl,
           "Blocks" = Blk,
           "Turnovers" = Tov,
           "Personal Fouls" = Pf,
           "Points" = Pts,
           "Result" = Results,
           "Margin of Victory" = Mov,
           "Field Goal Percent" = Fg_p,
           "3 Point Percent" = X3p_p,
           "Free Throw Percent" = Ft_p,
           "True Shooting Attempts" = Tsa,
           "True Shooting Percent" = Ts_p,
           "Effective Field Goal Percent" = Efg_p,
           "Turnover Percent" = Tov_p, 
           "Defensive Rebounds" = Drb,
           "Possessions" = Poss,
           "Mean PER" = Mean_per,
           "Top 4 Teams" = Top4)
scatter_vars <- game_total %>% 
    select(!c(Id, Date, Game_number, Year, `Tournament Stage`, Competition, Result, Country, `Top 4 Teams`)) %>% 
    colnames(.)

boxplot_vars_x <- game_total %>% 
    select(Year, `Tournament Stage`, Competition, Result, `Top 4 Teams`) %>% 
    colnames(.)

boxplot_vars_y <- game_total %>% 
    select(!c(Id, Date, Game_number, Year, `Tournament Stage`, 
              Competition, Result, Country, `Top 4 Teams`)) %>% 
    colnames(.)

group_var <- game_total %>% 
    select(Year, `Tournament Stage`, Competition, Result, `Top 4 Teams`) %>% 
    colnames(.)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Plot Type"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Scatter Plot", tabName = "sp", icon = icon("chart-line")),
            menuItem("Box Plot", tabName = "bp", icon = icon("chart-bar")),
            br(),br(),
            tags$a(href="https://www.basketball-reference.com/about/glossary.html", "Basketball Reference!"),
            tags$h6("Information on how statistics are"),
            tags$h6("calculated can be found by following"),
            tags$h6("this link")
            ),
        # Custom CSS to hide the default logout panel
        tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        
        # The dynamically-generated user panel
        uiOutput("userpanel")
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "sp",
                    fluidRow(
                        tags$h1("International Basketball Exploratory Data Analysis", 
                                align = "center",
                                style = "font-family: sans-serif;")
                    ),
                    fluidRow(
                        column(4,
                               selectInput("Xaxis_sp", "X axis",
                                           choices = scatter_vars,
                                           selected = "Points")),
                        column(4,
                               selectInput("Yaxis_sp", "Y axis",
                                           choices = scatter_vars,
                                           selected = "Field Goals Attempted")),
                        column(4,
                               selectInput("Group_sp", "Grouping",
                                           choices = group_var,
                                           selected = "Result"))),
                    fluidRow(
                        column(12,
                               plotOutput("scatterplot"),
                               h4(textOutput("corr")),
                        )),
                    ),
            tabItem(tabName = "bp",
                    fluidRow(
                        tags$h1("International Basketball Exploratory Data Analysis", 
                                align = "center",
                                style = "font-family: sans-serif;")
                    ),
                    fluidRow(
                        column(4,
                               selectInput("Xaxis_bp", "X axis",
                                           choices = boxplot_vars_x,
                                           selected = "Year")),
                        column(4,
                               selectInput("Yaxis_bp", "Y axis",
                                           choices = boxplot_vars_y,
                                           selected = "Points")),
                        column(4,
                               selectInput("Group_bp", "Grouping",
                                           choices = group_var,
                                           selected = "Result"))),
                    fluidRow(
                        column(12,
                               plotOutput("boxplot")))
            ))))
            
server <- function(input, output) {
    
    output$scatterplot <- renderPlot({
        ggplot(game_total, aes(.data[[input$Xaxis_sp]], .data[[input$Yaxis_sp]], 
                               colour = factor(.data[[input$Group_sp]]))) +
            geom_point(size = 3, alpha = 0.5) +
            theme_minimal() +
            labs(title = paste(input$Xaxis_sp, "and", input$Yaxis_sp),
                 x = input$Xaxis_sp,
                 y = input$Yaxis_sp,
                 colour = input$Group_sp) +
            theme(plot.title = element_text(hjust = .5, size = 16),
                  axis.title = element_text(size = 12),
                  text = element_text(size = 12)) +
            scale_color_manual(values = pal)

    })

    
    output$boxplot <- renderPlot({
        ggplot(game_total, aes(factor(.data[[input$Xaxis_bp]]), .data[[input$Yaxis_bp]], 
                               fill = factor(.data[[input$Group_bp]]))) +
            geom_boxplot() +
            theme_minimal() +
            labs(title = paste(input$Xaxis_bp, "and", input$Yaxis_bp),
                 x = input$Xaxis_bp,
                 y = input$Yaxis_bp,
                 fill = input$Group_bp) +
            theme(plot.title = element_text(hjust = .5, size = 16),
                  axis.title = element_text(size = 12),
                  text = element_text(size = 12)) +
            scale_fill_manual(values = pal)
        
    })
    
}

shinyApp(ui = ui, server = server)
