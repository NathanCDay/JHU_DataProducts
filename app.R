

library(shiny)
library(shinythemes)
library(plotly)
library(magrittr)
library(tidyverse)
library(ggbeeswarm)
theme_set(theme_classic(base_size = 18) +
              theme(panel.border = element_rect(colour = "#9d9d9e", fill=NA, size=1),
                    plot.background = element_rect(fill = "#1c1e22"),
                    strip.background = element_rect(linetype = "blank", fill = "#3e444c"),
                    strip.text = element_text(color = "white"),
                    axis.text = element_text(color = "#9d9d9e"),
                    axis.text.x = element_text(vjust = 1),
                    panel.grid.minor = element_line(colour="grey90", size=0.5),
                    panel.grid.major = element_line(colour="grey90", size=0.5),
                    legend.position = "none")
          )
poke_df <- read.csv("Pokemon.csv")
poke_df %<>% rename(Index = X., Sp_Atk = Sp..Atk, Sp_Def = Sp..Def)
axis_type <- c("Bu", "Dk", "Dg", "El", "Fa", "Ft", "Fr", "Fl", "Gh", "Gs", "Gr",
               "Ic", "No", "Po", "Ps", "Ro", "St", "Wa") %>%
    set_names(levels(poke_df$Type.1))
poke_df %<>% mutate(Mega = grepl(" ", Name), 
                    Legendary = grepl("True", Legendary),
                    axis_type = axis_type[Type.1])

poke_melt <- gather(poke_df, Stat, Value, 6:11)
poke_melt$Stat %<>% factor(levels = c("HP", "Speed", "Attack", "Sp_Atk",
                                      "Defense", "Sp_Def"))


poke_type <- poke_df %>% group_by(Type.1, axis_type) %>%
    summarise_at(vars(HP, Attack, Defense, Sp_Atk, Sp_Def, Speed), mean) %>%
    gather(Stat, Value, -Type.1, -axis_type)

poke_pallet <- c("#d5f246", "#282030", "#7a51cc", "#f9dd3e", "#e094d2",
                 "#c43417", "#f46804", "#d7aaed", "#6c03a0", "#078c23",
                 "#f9da8b", "#aaf6f7", "#efe8ce", "#a86bb2", "#ce02a5",
                 "#826a02", "#919396", "#1166d6")
names(poke_pallet) <- levels(poke_type$Type.1)

ui <- fluidPage(theme = shinytheme("slate"),
   fluidRow(
       column(6, offset = 1,
              tags$h1("Shiny Viz for Pokemon"),
              tags$h3("Gotta See 'em All!!!"),
              tags$p("Use the input selectors to choose which Pokemon to inclue
                     in the plots and use the tabs to see either 'All Types' or
                     'Filtered Types' to compare only a few."),
              tags$br(),
              tags$p("Bees plots are generated with library(ggsbeeswarm) and show the Pokemon's Primary Type, Total Pts, and Stat value when hovered.
                     Boxes plots show the Pokemon Type group's five number summary (Q1, Q2, Median, Q3, Q4) when hovered.
                     Table tabs show a interactive DataTables view of the wide-form data being used after the relavant filter controls are apply.
                     Pokemon are grouped based on Primary Type only in this app.")
       ),
       
       column(3, 
              tags$img(src = "http://assets.pokemon.com/assets/cms2/img/pokedex/full/464.png",
                       width = "350px", height = "350px"))
   ),
   hr(),
   fluidRow(
       column(width = 2, offset = 1,
              checkboxGroupInput("gen_select", "Generation:",
                                 choices = c("Gen 1" = 1, "Gen 2" = 2, "Gen 3" = 3, "Gen 4" = 4, "Gen 5" = 5),
                                 selected = c(1,2,3,4,5))),
       column(width = 2,
              checkboxGroupInput("legendary_select", "Legendary:", choices = c("Legendary" = T, "Non-Legendary" = F),
                                 selected = "Non-Legendary"),
              checkboxGroupInput("mega_select", "Mega:",choices = c("Mega" = T, "Non-Mega" = F), selected = "Non-Mega")),
       
       column(width = 4, offset = 1,
              tags$h4("Filter Types:"),
              selectizeInput("type_select", label = "Select up to 5 Types:", multiple = T,
                             selected = c("Water", "Fire", "Grass"),
                             choices = levels(poke_df$Type.1),
                             options = list(maxItems = 5, placeholder = "Choose 1 - 5")),
              sliderInput("total_select", label = "Total Points", min = 150, max = 800, value = c(180, 780))
       )
   ),
   hr(),
   fluidRow(
       column(10, offset = 1,
              tabsetPanel(
                  tabPanel(title = "All Types",
                              tabsetPanel(
                                  tabPanel("Bees", icon = icon("bug"),
                                        plotlyOutput("all_bee_plot", height = "800px")),
                                  tabPanel(title = "Boxes", icon = icon("inbox"),
                                        plotlyOutput("all_box_plot", height = "800px")),
                                  tabPanel(title = "Table", icon = icon("list-ol"),
                                           dataTableOutput("all_table"))
                                  )
                  ),
                  tabPanel(title = "Filtered Types",
                           tabsetPanel(
                               tabPanel(title = "Bees", icon = icon("bug"),
                                        plotlyOutput("bee_plot", height = "800px")),
                               tabPanel(title = "Boxes", icon = icon("inbox"),
                                        plotlyOutput("box_plot", height = "800px")),
                               tabPanel(title = "Table", icon = icon("list-ol"),
                                        dataTableOutput("table")))
                           )
              )

          )
   ),
   fluidRow(
       column(10, offset = 1,
              tags$h4("Licenses"),
              tags$p("Image is the offical art of Ken Sugimori '#464 Rhyperior', source from:"),
              tags$a(href = "http://www.pokemon.com/us/pokedex/rhyperior", "Sourced from Pokemon.co"),
              tags$h4("Data Source"),
              tags$p("Pokemon Data and Inspiration sourced from:"),
              tags$a(href= "https://www.kaggle.com/abcsds/pokemon", "Kaggle - Pokemon Stats"),
              tags$h4("Code Content"),
              tags$p("Created by Nathan Day, February 12, 2017, released under:"),tags$a(href = "https://www.gnu.org/licenses/gpl-3.0.en.html" ,"GNU General Public License")
       )
   )
)


server <- function(input, output) {
    
    all <- reactive({
        poke_melt %>% filter(Generation %in% input$gen_select) %>%
            filter(Legendary %in% input$legendary_select) %>%
            filter(Mega %in% input$mega_select)
    })
    
    output$all_bee_plot <- renderPlotly({
        p <- ggplot(data = all(), aes(x = axis_type, y = Value, color = Type.1, fill = Type.1)) +
            geom_quasirandom(aes(text = Name), size = 2, alpha = .5) +
            scale_color_manual(values = poke_pallet) +
            scale_fill_manual(values = poke_pallet) +
            facet_grid(Stat~.,scales = "free_y") +
            labs(x = NULL, y = "")
        ggplotly(p, tooltip = c("text", "y", "colour"))
    })
    
    output$all_box_plot <- renderPlotly({
        p <- ggplot(data = all(), aes(x = axis_type, y = Value, color = Type.1, fill = Type.1)) +
            geom_boxplot(color = "#1c1e22", alpha = .75) +
            scale_color_manual(values = poke_pallet) +
            scale_fill_manual(values = poke_pallet) +
            facet_grid(Stat~.,scales = "free_y") +
            labs(x = NULL, y = "")
        ggplotly(p, originalData = F)
    })
    
    all_spread <- reactive({
        all() %>% 
            spread(Stat, Value) %>%
            unite(Type, Type.1, Type.2, sep = "/") %>%
            mutate(Type = gsub("\\/$", "", Type)) %>%
            select(-axis_type) %>%
            select(1:4, 8:13, 5:7)
    })
    
    output$all_table <- renderDataTable({ all_spread() })
    
    filtered <- reactive({
        poke_melt %>% filter(Type.1 %in% input$type_select) %>%
            filter(Generation %in% input$gen_select) %>%
            filter(Legendary %in% input$legendary_select) %>%
            filter(Mega %in% input$mega_select) %>%
            filter(Total > input$total_select[1]) %>%
            filter(Total < input$total_select[2])
    })
    
    output$bee_plot <- renderPlotly({
         p <- ggplot(data = filtered(), aes(x = axis_type, y = Value, color = Type.1, fill = Type.1)) +
             geom_quasirandom(aes(text = Name, size = Total), alpha = .5) +
             scale_color_manual(values = poke_pallet) +
             scale_fill_manual(values = poke_pallet) +
             scale_size(range = c(1,4)) +
             facet_wrap(~Stat, nrow = 3) +
             labs(x = NULL, y = "")
            
         ggplotly(p, tooltip = c("text", "y", "size", "colour"))
    })
    
    output$box_plot <- renderPlotly({
        p <- ggplot(data = filtered(), aes(x = axis_type, y = Value, fill = Type.1)) +
            geom_boxplot(color = "#1c1e22", alpha = .75) +
            scale_color_manual(values = poke_pallet) +
            scale_fill_manual(values = poke_pallet) +
            facet_wrap(~Stat, nrow = 3) +
            labs(x = NULL, y = NULL)
        
        ggplotly(p, originalData = F)
        
    })
    
    filtered_spread <- reactive({
        filtered() %>% 
            spread(Stat, Value) %>%
            unite(Type, Type.1, Type.2, sep = "/") %>%
            mutate(Type = gsub("\\/$", "", Type)) %>%
            select(-axis_type) %>%
            select(1:4, 8:13, 5:7)
    })
    
    output$table <- renderDataTable({ filtered_spread() })
}

shinyApp(ui = ui, server = server)

