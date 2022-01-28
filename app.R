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
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    filter_data <- reactive(data %>% filter(day >= input$range[1] & day <= input$range[2]) %>% filter(author %in% input$authors))
    
    output$resultsPlot <- renderPlot({
        plot <- ggplot(filter_data(), aes(x=day, y=score, group=author, color=author)) +
            geom_line() +
            geom_point() +
            scale_y_reverse(limits=c(6, 1), breaks = c(6:1), minor_breaks = NULL) +
            scale_x_continuous(breaks = seq(input$range[1], input$range[2], by = 1), minor_breaks = F) +
            xlab("Dies") +
            ylab("Intents") +
            labs(
                colour = "Usuari",
            ) + 
            theme_light() + 
            theme(
                legend.position="bottom",
            )
        
        plot
    })
    
    output$classification <- renderTable(
        {
            filter_data() %>% group_by(author) %>% summarize(score=sum(score) / n()) %>% arrange(score) %>% mutate(rank = row_number(), .before = author) %>% rename_all(~c("Rànking", "Usuari", "Puntuació mitjana"))
        },
        width="100%"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
