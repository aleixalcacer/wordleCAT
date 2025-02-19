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
        caption = "Per a participar, comparteix els teus resultats mencionant a @aleixalbo!"
    )

data <- read_csv("results.csv") %>% complete(author, day)


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

compute_streak <- function(score) {
    c_streak = 0
    streak = c()
    for (i in 1:length(score)) {
        s_new <- score[i]
        if (is.na(s_new)) {
            c_streak <- c_streak + 1
        }
        streak <- c(streak, c_streak)
        if (is.na(s_new)) {
            c_streak <- c_streak + 1
        }
    }
    return(streak)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    filter_data <- reactive(data %>% filter(day >= input$range[1] & day <= input$range[2]) %>% filter(author %in% input$authors))
    
    output$resultsPlot <- renderPlot(
        {
            if (!is.null(input$authors)) {
                p_data <- filter_data()
                
                p_data <- p_data %>% group_by(author) %>% arrange(day) %>% mutate(streak = compute_streak(score))
                levels <-p_data %>% group_by(author) %>% filter(streak == max(streak)) %>% tally() %>% arrange(desc(n)) %>% pull(author)
                p_data <- p_data %>% mutate(author = factor(author, levels, ordered = T))  %>% group_by(author) %>% mutate(rnk=cur_group_id())
                
                end_rank <- input$range[2]
                end_nameline <- end_rank + 2
                
                plot <- ggplot(p_data, aes(x=day, y=rnk, color=author)) +
                    geom_path(aes(group = interaction(author, streak)), size=2, color="lightgrey") +
                    geom_point(aes(size=as.factor(score)), color="lightgrey") +
                    geom_path(data = . %>% filter(streak == max(streak)), aes(group = interaction(author, streak)), size=2) +
                    geom_point(data = . %>% filter(streak == max(streak)), aes(size=as.factor(score))) +
                    geom_segment(data = . %>% group_by(author) %>% slice_max(day),
                                 aes(x=end_rank, xend=end_nameline, y = rnk, yend=rnk), size=2) +
                    geom_segment(data = . %>% group_by(author) %>% filter(streak == max(streak)) %>% drop_na() %>% add_tally(),
                                 aes(x=end_nameline, xend=end_nameline + n * 5 / max(n), y = rnk, yend=rnk), size=2) +
                    geom_vline(xintercept = end_nameline, size=2, color = "white") +
                    geom_text(data = . %>% filter(day == max(day)),
                              aes(x = end_nameline, label = author), hjust = 1, vjust = -1.1) +
                    geom_text(data = . %>% filter(streak == max(streak)) %>% drop_na() %>% add_tally(),
                              aes(x = end_nameline + n * 5 / max(n) + 0.1, label = n), hjust = 0, nudge_y = 0) +
                    scale_y_reverse("",
                                    breaks = length(unique(unlist(p_data %>% select(author)))):1,
                                    limits = c(length(unique(unlist(p_data %>% select(author)))) + 0.5, 0.5)
                    ) +
                    scale_x_continuous("",
                                       breaks = seq(input$range[1], input$range[2], by = 1),
                                       limits = c(input$range[1], input$range[2] + 8),
                    ) +
                    labs(
                        title = str_wrap("Històric al #WordleCAT"),
                        subtitle = str_wrap("Resultats obtinguts per cada jugador al #WordleCAT.
                                             En color es pot vore els dies consecutius que porta jugant cada jugador.
                                             El tamany dels punts indica el número d'intents realitzats.", 100),
                    ) +
                    theme(
                        axis.text.y = element_blank(),
                    )

                plot
            }
        },
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
                levels <- filter_data() %>% drop_na() %>% group_by(author) %>% summarize(score=sum(score) / n()) %>% arrange(score) %>% pull(author)
                p_data <- filter_data() %>%
                    drop_na() %>%
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
            
                p_data <- p_data %>% mutate(author = factor(author, levels, ordered = T))
                print(levels)
                
                end_rank <- input$range[2]
                end_nameline <- end_rank + 2

                plot <- ggplot(p_data, aes(x=day, y=rnk, group=author, color=author)) +
                    geom_bump(size=2) +
                    geom_point(data = . %>% group_by(author) %>% slice_min(day), size=4) +
                    geom_segment(data = . %>% group_by(author) %>% slice_max(day),
                                 aes(x=end_rank, xend=end_nameline, y = rnk, yend=rnk), size=2) +
                    geom_segment(data = . %>% group_by(author) %>% slice_max(day),
                                 aes(x=end_nameline, xend=end_nameline + score * 5 / max(score), y = rnk, yend=rnk), size=2) +
                    geom_vline(xintercept = end_nameline, size=2, color = "white") +
                    geom_text(data = . %>% filter(day == max(day)),
                              aes(x = end_nameline, label = author), hjust = 1, vjust = -1.1) +
                    geom_text(data = . %>% filter(day == max(day)),
                              aes(x = end_nameline + score * 5 / max(score) + 0.1, label = round(score, 2)), hjust =0, nudge_y = 0) +
                    scale_y_reverse("",
                                    breaks = length(unique(unlist(p_data %>% select(author)))):1,
                                    limits = c(length(unique(unlist(p_data %>% select(author)))) + 0.5, 0.5)
                                    ) +
                    scale_x_continuous("",
                                       breaks = seq(input$range[1], input$range[2], by = 1),
                                       limits = c(input$range[1], input$range[2] + 8),
                                       ) +
                    labs(
                        title = str_wrap("Classificació al #WordleCAT"),
                        subtitle = str_wrap("Evolució al llarg dels dies de la classifciació al #WordleCAT.
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
