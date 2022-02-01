#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


# Redefine ggplot

ggplot <- function(...) ggplot2::ggplot(...) + theme_minimal()

compute_streak <- function(day) {
    if (length(day) == 1) {
        return(1)
    }
    streak <- 1
    for (i in 2:length(day)) {
        if (day[i-1] - 1 == day[i]) {
            streak = streak + 1
        } else {
            break
        }
    }
    return(streak)
}


data <- read_csv("results.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Resultats del WordleCAT"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("range",
                        "Rang de dies:",
                        min = data %>% select(day) %>% min(),
                        max = data %>% select(day) %>% max(),
                        value = c(data %>% select(day) %>% min(), data %>% select(day) %>% max())
            ),
            selectInput("authors",
                        "Usuaris",
                        choices = data %>% select(author),
                        multiple = T,
            ),
        ),

        # Show a plot of the results
        mainPanel(
            tabsetPanel(
                tabPanel("Històric", plotOutput("resultsPlot")),
                tabPanel("Classificació", tableOutput('classification')),
                tabPanel("Distribucions", plotOutput("distributionPlot")),
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    filter_data_ <- reactive(data %>% filter(day >= input$range[1] & day <= input$range[2]) %>% filter(author %in% input$authors))
    levels_ <- reactive(unlist(filter_data_() %>% group_by(author) %>% summarize(score=sum(score) / n()) %>% arrange(score) %>% select(author)))
    filter_data <- reactive(filter_data_() %>% mutate(author = fct_relevel(author, levels_())))
    
    output$resultsPlot <- renderPlot(
        {
            if (!is.null(input$authors)) {
                
                plot <- ggplot(filter_data(), aes(x=day, y=score, group=author, color=author)) +
                    geom_line() +
                    geom_point() +
                    scale_y_reverse("Intents", limits=c(7.5, 0.5), breaks = c(7:1), minor_breaks = NULL) +
                    scale_x_continuous("Dies", breaks = seq(input$range[1], input$range[2], by = 1), minor_breaks = F) +
                    labs(
                        colour = "Usuari",
                    ) + 
                    theme(
                        legend.position="bottom",
                    )
                plot
            }
        }
    )
    
    output$distributionPlot <- renderPlot(
        {
            if (!is.null(input$authors)) {
                plot <- ggplot(filter_data(), aes(x=author, y=score, color=author, fill=author)) +
                    geom_violin(trim = FALSE, alpha=0.3) + 
                    geom_jitter(height = 0.05, width = 0.05) +
                    scale_y_reverse("Intents", limits=c(7.5, 0.5), breaks = 7:1, minor_breaks = NULL) +
                    scale_x_discrete("Usuari") +
                    labs(
                        fill = "Usuari",
                        color = "Usuari",
                    ) + 
                    theme(
                        legend.position="bottom",
                    )
                plot
            }        
        }
    )
    
    output$classification <- renderTable(
        {   
            if (!is.null(input$authors)) {
                filter_data() %>% group_by(author) %>%
                    summarize(score=sum(score) / n(), streak = as.integer(compute_streak(day))) %>%
                    mutate(rank = row_number(), .before = author) %>%
                    rename_all(~c("Rànking", "Usuari", "Puntuació mitjana", "Ratxa de dies"))
            }
        },
        width="100%"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
