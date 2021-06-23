library(shiny)
library(shinydashboard)

components <- readRDS("S:/Kozos/Inkluziv_novekedesi_index/Adatok/components.rds")
source('C:/rprojects/novekedesi_index/R/functions.R', encoding = 'UTF-8')

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Inkluzív növekedés"),
                    dashboardSidebar(
                      h3("Dashboard bemutatása")
                    ),
                    
                    dashboardBody(
                      shiny::fluidRow(
                        shiny::column(width = 6,
                                      box(title = "Felhasznált adatok", height = "600px", width = NULL,
                                          shiny::tableOutput("descriptive")
                                      ),
                                      box(
                                        title = "Adatminőség", height = "700px", width = NULL,
                                        shiny::inputPanel(
                                          selectInput("choosen_var",
                                                      "Változó:", choices = NiceName(names(components[-c(1:3)]))),
                                          selectInput("development",
                                                      "Fejletség:", choices = c("Fejlett", "Fejlődő", "Fejlődő (alsóbb)"))
                                        ),
                                        infoBoxOutput("box1"),
                                        shiny::plotOutput("missing_heatmap")
                                      )
                                      
                        ), 
                        box(
                          # Title can include an icon
                          title = "WEF jellegű ábrák", width = 6, height = "1300px",
                          shiny::inputPanel(
                            selectInput("development_wef",
                                        "Fejletség:", choices = c("Fejlett", "Fejlődő", "Fejlődő (alsóbb)"))
                          ),
                          shiny::plotOutput("wef_plot", height = "1100px")
                          
                        )
                        
                      ),
                      shiny::fluidRow(
                        box(title = "Dinamika", width = 12, height = "900px",
                            inputPanel(
                              selectInput("x",
                                          "X-tengely:", choices = NiceName(names(components[-c(1:3)])), selected = "Foglalkoztatási ráta"),
                              selectInput("y",
                                          "Y-tengely:", choices = NiceName(names(components[-c(1:3)])), selected = "Függőségi ráta"),
                              selectInput("size",
                                          "Méret:", choices = NiceName(names(components[-c(1:3)])), selected = "GDP (PPP)")
                            ), 
                            plotly::plotlyOutput("dynamic_plot")
                            )
                      )
                    )
)


server <- function(input, output) { 
  
  output$descriptive <- renderTable({
    data.frame(
      stringsAsFactors = FALSE,
      Adat = c("GDP/fő", "Várható élettartam", "Foglalkoztatási ráta", "..."),
      Forrás = c("Világ Bank", "Világ Bank", "Világ Bank", "..."),
      Leírás = c("GDP per capita, PPP (constant 2011 international $)","Születéskori várható élettartam",
                 "Employment to population ratio, 15+, total (%) (modeled ILO estimate)","...")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "800px")
  
  
  output$box1 <- shinydashboard::renderInfoBox({
    
    development_filter <- case_when(
      input$development == "Fejlett" ~ "advanced_economies",
      input$development == "Fejlődő" ~ "emerging_economies",
      input$development == "Fejlődő (alsóbb)" ~ "emerging_economies_cont_d",
      TRUE ~ c("advanced_economies", "emerging_economies", "emerging_economies_cont_d")
    )
    
    out <- components %>% 
      pivot_longer(-c(1:3)) %>% 
      mutate(observed = !is.na(value)) %>% 
      filter(NiceName(name) == input$choosen_var) %>% 
      {
        if (input$development != "összes") {
          filter(., development == development_filter)
        } else {
          .
        }
      } %>% 
      {mean(.$observed)} %>% 
      percent_hu(accuracy = 1)
    
    infoBox(
      "elérhető", out, icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$missing_heatmap <-  renderPlot({
    
    development_filter <- case_when(
      input$development == "Fejlett" ~ "advanced_economies",
      input$development == "Fejlődő" ~ "emerging_economies",
      input$development == "Fejlődő (alsóbb)" ~ "emerging_economies_cont_d",
      TRUE ~ c("advanced_economies", "emerging_economies", "emerging_economies_cont_d")
    )
    
    components %>% 
      pivot_longer(-c(1:3)) %>% 
      mutate(
        name = NiceName(name),
        observed = ifelse(!is.na(value), "elérhető", "nem elérhető")
      ) %>% 
      filter(name == input$choosen_var) %>% 
      {
        if (input$development != "összes") {
          filter(., development == development_filter)
        } else {
          .
        }
      } %>% 
      ggplot(aes(country, time, fill = observed)) + 
      geom_tile(color = "black") +
      theme_minimal() + 
      scale_y_continuous(labels = as.character, breaks = 2010:2020) +
      labs(x = "Ország", y = "Év", fill = NULL) +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 40, hjust = .4, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 20)
      ) + 
      scale_fill_manual(values = c("elérhető" = "steelblue","nem elérhető" = "red2"))
  })
  
  output$wef_plot <- shiny::renderPlot({
    development_filter <- case_when(
      input$development_wef == "Fejlett" ~ "advanced_economies",
      input$development_wef == "Fejlődő" ~ "emerging_economies",
      input$development_wef == "Fejlődő (alsóbb)" ~ "emerging_economies_cont_d",
      TRUE ~ c("advanced_economies", "emerging_economies", "emerging_economies_cont_d")
    )
    
    components %>%
      arrange(time) %>% 
      group_by(country) %>% 
      fill(everything()) %>% 
      ungroup() %>% 
      filter(time == 2020) %>% 
      select(-gdp_ppp, -population) %>% 
      pivot_longer(-c(1:3), names_to = "variable") %>% 
      group_by(variable, development) %>%
      group_modify(~ mutate(.x, value_rank = GetRank(value))) %>%
      ungroup() %>%
      mutate(
        value_rank = ifelse(is.na(value), NA, value_rank),
        value_rank = ifelse(add_direction(variable) == "good", value_rank, 5 + 1 - value_rank),
        value_rank = factor(value_rank, levels = 1:5, ordered = T),
        dimension = add_dimension(variable, hun = TRUE)
      ) %>% 
      filter(development == development_filter) %>% 
      ggplot() +
      aes(country, variable, fill = value_rank, label = value) + 
      geom_tile(color = "black") + 
      scale_y_discrete(labels = NiceName) + 
      scale_fill_brewer(palette = "Spectral", guide = guide_legend(nrow = 1, label.position = "bottom"),
                        label = c("Legrosszabb 20%", "", "", "", "Legjobb 20%", "Nincs adat"),
                        na.value = "grey") +
      labs(y = NULL, x = NULL, fill = NULL) +
      facet_wrap(~ dimension, scales = "free_y", ncol = 1) +
      theme(
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = .4),
        axis.text = element_text(size = 14),
        legend.key.width = unit(2, "cm"),
      )
  })
  
  output$dynamic_plot <- plotly::renderPlotly({
    x_axis <- recode_name(input$x, FALSE)
    y_axis <- recode_name(input$y, FALSE)
    input_size <- recode_name(input$size, FALSE)
    
    p <- components %>% 
      select(development, time, country, x_axis, y_axis, input_size) %>% 
      set_names("development", "Év", "country", "x", "y", "size") %>% 
      mutate(
        development = NiceName(development),
        text = str_c("Ország: ", country, "\n",
                     NiceName(x_axis), ": ", format(x, digits = 2), "\n",
                     NiceName(y_axis), ": ", format(y, digits = 2), "\n",
                     NiceName(input_size), ": ", format(size, digits = 2)
        )
      ) %>% 
      ggplot() +
      aes(x = x, y = y, fill = country, size = size, frame = Év, text = text) + 
      geom_point(show.legend = FALSE) +
      scale_size(range = c(5, 20)) +
      facet_wrap(~ development) +
      labs(x = input$x, y = input$y) + 
      theme(legend.position = "none", 
            strip.text = element_text(size = 20),
            axis.text = element_text(size = 14), 
            axis.title = element_text(size = 16)
            )
    
    myplotly(p, height = 700, width = 1900)
  })
  
}

shinyApp(ui, server)