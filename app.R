library(shiny)
library(shinymanager)
library(shinydashboard)

components <- read.csv("components.csv")
data_descript <- readRDS("data_descript.RDS")
source('functions.R', encoding = 'UTF-8')
# data_descript <- readxl::read_excel("adatok_forrasa_leirasa.xlsx")


dbHeader <- dashboardHeader(title = "Inkluzív növekedés",
                            tags$li(a(href = 'https://makronomintezet.hu/',
                                      img(src = 'company_logo.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

credentials <- data.frame(
  user = c("maki", "Vékás Péter"),
  password = c("Patriot2020", "vendeg"),
  stringsAsFactors = FALSE
)

ui <- secure_app(head_auth = tags$script(inactivity),
                 dashboardPage(skin = "red",
                               dbHeader,
                               dashboardSidebar(
                                 sidebarMenu(
                                   menuItem("Felhasznált adatok", tabName = "data", icon = icon("info")),
                                   
                                   menuItem("Adatminőség", tabName = "missing", icon = icon("database")),
                                   menuItem("WEF ábrák", tabName = "wef", icon = icon("th")),
                                   menuItem("Dinamika", tabName = "dynamics", icon = icon("chart-line"))
                                 )
                               ),
                               
                               dashboardBody(
                                 tabItems(
                                   # First tab content
                                   tabItem(tabName = "data",
                                           box(title = "Felhasznált adatok", height = "1700px", width = 12,
                                               shiny::tableOutput("descriptive")
                                           )
                                           
                                   ),
                                   tabItem(tabName = "missing",
                                           box(
                                             title = "Adatminőség", height = "700px", width = 12,
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
                                   tabItem(tabName = "wef",
                                           box(
                                             # Title can include an icon
                                             title = "WEF jellegű ábrák", width = 12, height = "780px",
                                             shiny::inputPanel(
                                               selectInput("development_wef",
                                                           "Fejletség:", choices = c("Fejlett", "Fejlődő", "Fejlődő (alsóbb)")),
                                               selectInput("wef_dimension", "Dimenzió", choices = c("Fenntarthatóság", "Munkaalapú társadalom", "Gazdasági fejlettség és növekedés", "Társadalmi egyenlőtlenség"))
                                             ),
                                             shiny::plotOutput("wef_plot", height = "600px")
                                             
                                           )
                                   ),
                                   tabItem(tabName = "dynamics",
                                           box(title = "Dinamika", width = 12, height = "900px",
                                               inputPanel(
                                                 selectInput("x",
                                                             "X-tengely:", choices = NiceName(names(components[-c(1:3)])), selected = "Foglalkoztatási ráta"),
                                                 selectInput("y",
                                                             "Y-tengely:", choices = NiceName(names(components[-c(1:3)])), selected = "Függőségi ráta"),
                                                 selectInput("size",
                                                             "Méret:", choices = NiceName(names(components[-c(1:3)])), selected = "GDP (PPP)"),
                                                 selectInput("dynamic_development",
                                                             "Fejlettség:", choices = c("Összes", "Fejlett", "Fejlődő", "Fejlődő (alsóbb)")),
                                                 sliderInput("dynamic_time", "Év:", 2010, 2019, step = 1, sep = "", value = 2019, animate = TRUE)
                                               ), 
                                               shiny::plotOutput("dynamic_plot")
                                           )
                                   )
                                 )
                               )
                 )
)

server <- function(input, output) { 
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  
  output$descriptive <- shiny::renderTable({
    
    data_descript 
    # %>%
      # arrange(Adat) %>%
      # replace_na(list(Leírás = "")) %>%
      # mutate(Leírás = str_wrap(Leírás, 30)) %>%
      # DT::datatable(
      #   extensions = c('Buttons', "Scroller"),
      #   options = list(
      #     deferRender = TRUE,
      #     scrollY = 300,
      #     scroller = TRUE,
      #     style = "bootstrap4",
      #     searching = TRUE,
      #     autoWidth = TRUE,
      #     ordering = TRUE,
      #     dom = 'Bfrtip',
      #     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      #   )
      # )
  })


  output$box1 <- shinydashboard::renderInfoBox({
    # TODO WORKS
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
      scale_y_continuous(labels = as.character, breaks = 2010:2019) +
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
    
    dimension_filter <- case_when(
      input$wef_dimension == "Fenntarthatóság" ~ "sustainability",
      input$wef_dimension == "Munkaalapú társadalom" ~ "work",
      input$wef_dimension == "Gazdasági fejlettség és növekedés" ~ "economic",
      input$wef_dimension == "Társadalmi egyenlőtlenség" ~ "equality"
    )
    
    components %>%
      arrange(time) %>%
      group_by(country) %>%
      fill(everything()) %>%
      ungroup() %>%
      filter(time == 2019) %>%
      select(-gdp_ppp, -population) %>%
      pivot_longer(-c(1:3), names_to = "variable") %>%
      group_by(variable, development) %>%
      group_modify(~ mutate(.x, value_rank = GetRank(value))) %>%
      ungroup() %>%
      mutate(
        value_rank = ifelse(is.na(value), NA, value_rank),
        value_rank = ifelse(add_direction(variable) == "good", value_rank, 5 + 1 - value_rank),
        value_rank = factor(value_rank, levels = 1:5, ordered = T),
        dimension = add_dimension(variable, hun = FALSE)
      ) %>%
      filter(dimension == dimension_filter) %>%
      filter(development == development_filter) %>%
      ggplot() +
      aes(country, variable, fill = value_rank, label = value) +
      geom_tile(color = "black", size = .8) +
      scale_y_discrete(labels = NiceName) +
      scale_fill_brewer(palette = "Spectral", guide = guide_legend(nrow = 1, label.position = "bottom"),
                        label = c("Legrosszabb 20%", "", "", "", "Legjobb 20%", "Nincs adat"),
                        na.value = "grey") +
      labs(y = NULL, x = NULL, fill = NULL) +
      theme(
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = .4),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.width = unit(3, "cm")
      )
  })
  
  output$dynamic_plot <- shiny::renderPlot({
    x_axis <- recode_name(input$x, FALSE)
    y_axis <- recode_name(input$y, FALSE)
    input_size <- recode_name(input$size, FALSE)

    development_filter <- case_when(
      input$development_wef == "Fejlett" ~ "advanced_economies",
      input$development_wef == "Fejlődő" ~ "emerging_economies",
      input$development_wef == "Fejlődő (alsóbb)" ~ "emerging_economies_cont_d",
      TRUE ~ "all"
    )

    df <- components %>%
      select(development, time, country, x_axis, y_axis, input_size) %>%
      set_names("development", "time", "country", "x", "y", "size")
    
    plot_limits <- list(
      xmin = min(df$x),
      xmax = max(df$x),
      ymin = min(df$y),
      ymax = max(df$y)
    )

    if (development_filter != "all") {
      df <- df %>%
        filter(development == development_filter)
    }
    
    df <- df  %>%
      filter(time == input$dynamic_time)
    
    ggplot(df) +
      aes(x = x, y = y, fill = country, size = size) + # TODO modify the frame title
      geom_point(show.legend = FALSE) +
      scale_size(range = c(5, 20)) +
      labs(x = input$x, y = input$y) +
      theme(legend.position = "none",
            strip.text = element_text(size = 20),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16)
      ) + 
      scale_x_continuous(limits = c(plot_limits$xmin, plot_limits$xmax)) +
      scale_y_continuous(limits = c(plot_limits$ymin, plot_limits$ymax))
  })
  
}

shinyApp(ui, server)