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
library(ggbump)

# Redefine ggplot

ggplot <- function(...) ggplot2::ggplot(...) +
    theme_void() +
    theme(
        plot.margin = margin(.5, .5, .5, .5, "cm"),
        plot.background = element_rect(fill="white", colour = "white"),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 24, color = "black", family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(color = "black", family = "Helvetica"),
        plot.caption = element_text(color = "black", family = "Helvetica"),
    ) +
    labs(
        caption = "@aleixalbo"
    )


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
                        value = c(data %>% select(day) %>% min(), data %>% select(day) %>% max()),
                        ticks = F,
            ),
            selectInput("authors",
                        "Usuaris",
                        choices = data %>% select(author),
                        multiple = T,
                        selected = unlist(data %>% select(author)),
            ),
        ),

        # Show a plot of the results
        mainPanel(
            tabsetPanel(
                tabPanel("Classificació", plotOutput("rankingPlot")),
                tabPanel("Històric", plotOutput("resultsPlot")),
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
                p_data <- filter_data()
                
                plot <- ggplot(p_data, aes(x=day, y=score, group=author, color=author)) +
                    geom_bump(size=2) +
                    geom_point(size=4) +
                    facet_wrap(~ author, ncol = 1) +
                    scale_y_reverse("Intents", limits=c(7.5, 0.5), breaks = c(7:1), minor_breaks = NULL) +
                    scale_x_continuous("Dies", breaks = seq(input$range[1], input$range[2], by = 1), minor_breaks = F) +
                    labs(
                        colour = "Usuari",
                    )

                plot
            }
        },
        height = 500,
    )
    
    output$distributionPlot <- renderPlot(
        {
            if (!is.null(input$authors)) {
                plot <- ggplot(filter_data(), aes(x=author, y=score, color=author, fill=author)) +
                    geom_violin(trim = FALSE, alpha=0.2) + 
                    geom_jitter(height = 0.05, width = 0.05) +
                    scale_y_reverse("Intents", limits=c(7.5, 0.5), breaks = 7:1, minor_breaks = NULL) +
                    scale_x_discrete("Usuari") +
                    labs(
                        fill = "Usuari",
                        color = "Usuari",
                    ) +
                    theme(
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                        )
                
                plot
            }        
        }
    )
    
    
    output$rankingPlot <- renderPlot(
        {
            if(!is.null(input$authors)) {
                p_data <- filter_data() %>%
                    group_by(author) %>%
                    arrange(day) %>%
                    mutate(score=cumsum(score) / row_number()) %>%
                    ungroup() %>%
                    complete(author, day) %>%
                    group_by(author) %>%
                    fill(score, .direction = "down") %>%
                    drop_na() %>%
                    ungroup() %>%
                    arrange(desc(day)) %>%
                    group_by(day) %>%
                    arrange(score) %>%
                    mutate(rnk=row_number()) %>%
                    ungroup()
            
                end_rank <- input$range[2]
                end_nameline <- end_rank + 2

                plot <- ggplot(p_data, aes(x=day, y=rnk, group=author, color=author)) +
                    geom_bump(size=2) +
                    geom_point(data = . %>% group_by(author) %>% slice_min(day), size=4) +
                    geom_segment(data = . %>% group_by(author) %>% slice_max(day),
                                 aes(x=end_rank, xend=end_nameline, y = rnk, yend=rnk), size=2) +
                    geom_segment(data = . %>% group_by(author) %>% slice_max(day),
                                 aes(x=end_nameline, xend=end_nameline + score * 4 / 7, y = rnk, yend=rnk), size=2) +
                    geom_vline(xintercept = end_nameline, size=2, color = "white") +
                    geom_text(data = . %>% filter(day == max(day)),
                              aes(x = end_nameline, label = author), hjust = 1, vjust = -1.1) +
                    geom_text(data = . %>% filter(day == max(day)),
                              aes(x = end_nameline + score * 4 / 7 + 0.1, label = round(score, 2)), hjust =0, nudge_y = 0) +
                    scale_y_reverse("",
                                    breaks = length(unique(unlist(p_data %>% select(author)))):1,
                                    limits = c(length(unique(unlist(p_data %>% select(author)))) + 0.5, 0.5)
                                    ) +
                    scale_x_continuous("",
                                       breaks = seq(input$range[1], input$range[2], by = 1),
                                       limits = c(input$range[1], input$range[2] + 5.5),
                                       ) +
                    labs(
                        title = str_wrap("Classificació al #WordleCAT"),
                        subtitle = str_wrap("Evolució al llarg dels dies de la classifciació dels usuaris de Twitter que han compartit el resultats del #WordleCAT mencionant a @aleixalbo.
                                            La puntuació utilitzada és la mitjana dels intents realitzats cada dia.", 100),
                        ) +
                    theme(
                        axis.text.y = element_blank(),
                    )
                    
                plot
                }
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
