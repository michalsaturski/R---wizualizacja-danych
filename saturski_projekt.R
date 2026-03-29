library(jsonlite)
library(dplyr)
library(shiny)
library(plotly)
library(leaflet)
library(DT)
library(tidyr)
library(stringr)

# --- PRZYGOTOWANIE DANYCH ---
ev_data_sample <- fromJSON("https://data.wa.gov/resource/f6w7-q2d2.json?$limit=5000&$order=:id")

# Główny zbiór danych
df <- ev_data_sample %>%
  mutate(
    model_year = as.numeric(as.character(model_year)),
    electric_range = as.numeric(as.character(electric_range))
  ) %>%
  filter(!is.na(model_year))

# Tabela unikalnych modeli
df_models <- df %>%
  filter(electric_range > 0) %>%
  group_by(make, model, model_year, ev_type) %>%
  summarise(electric_range = max(electric_range), .groups = "drop") %>%
  arrange(model_year, make, model)

# Dane geograficzne
df_map <- df %>%
  mutate(
    lng = as.numeric(sapply(geocoded_column$coordinates, function(x) x[1])),
    lat = as.numeric(sapply(geocoded_column$coordinates, function(x) x[2]))
  ) %>%
  filter(!is.na(lng) & !is.na(lat))

# --- INTERFEJS UŻYTKOWNIKA (UI) ---
ui <- fluidPage(
  titlePanel("Analiza Adopcji Pojazdów Elektrycznych w stanie Waszyngton"),
  hr(),
  
  navlistPanel(
    widths = c(3, 9),
    
    tabPanel("Opis projektu",
             h3("Wstęp"),
             p("Projekt analizuje czynniki wpływające na popularność oraz trendy technologiczne aut EV w stanie Waszyngton."),
             h4("Pytania badawcze:"),
             tags$ul(
               tags$li("Jak zmieniał się zasięg pojazdów?"),
               tags$li("Którzy producenci dominują na rynku?"),
               tags$li("Czy dotacje wpływają na wybory konsumentów?"),
               tags$li("Gdzie znajduje się najwięcej aut elektrycznych w stanie Waszyngton?")
             )),
    
    tabPanel("Dane źródłowe",
             h3("Opis datasetu"),
             p("Dataset przedstawia pojazdy elektryczne zarejestrowane w stanie Waszyngton z podziałem na Battery Electric Vehicles (BEVs) oraz Plug-in Hybrid Electric Vehicles (PHEVs)."),
             p("Źródło: State of Washington Open Data"),
             p("Pełen dataset składa się z 271 tys. wierszy. Na potrzeby projektu wybrana została próbka 5000 rekordów, aby usprawnić działanie aplikacji."),
             h3("Podgląd datasetu"),
             DTOutput("raw_data_table"),
             h4("Unikalne modele"),
             p("Z datasetu utworzona została tabela unikalnych modeli aut w celu analizy zasięgu i popularności konkretnych modeli."),
             DTOutput("raw_data_table_models")),
    
    tabPanel("Wizualizacje",
             h3("Analiza i Trendy"),
             
             wellPanel(
               h4("1. Ewolucja zasięgu aut"),
               sliderInput("years_plotly", "Zakres lat:", 2011, 2024, c(2011, 2024), sep = ""),
               checkboxGroupInput("ev_type_filter", "Typ napędu:", choices = unique(df$ev_type), selected = unique(df$ev_type)[2]),
               plotlyOutput("range_plotly_plot")
             ),
             
             wellPanel(
               h4("2. Ranking popularności marek"),
               sliderInput("years_plotly2", "Zakres lat:", 2011, 2024, c(2011, 2024), sep = ""),
               checkboxGroupInput("ev_type_filter2", "Typ napędu:", choices = unique(df$ev_type), selected = unique(df$ev_type)),
               plotlyOutput("popularity_bar_plot")
             ),
             
             wellPanel(
               h4("3. Wpływ dotacji (CAFV)"),
               sliderInput("years_plotly3", "Zakres lat:", 2011, 2024, c(2011, 2024), sep = ""),
               plotlyOutput("cafv_bar_plot")
             ),
             
             wellPanel(
               h4("4. Mapa rozmieszczenia pojazdów"),
               leafletOutput("ev_map", height = "500px")
             )),
    
    tabPanel("Podsumowanie",
             h3("Główne wnioski"),
             tags$ol(
               tags$li("Zasięg aut elektrycznych (BEV) jest wyższy w nowszych modelach. W przypadku aut hybrydowych (PHEV) nie ma widocznego trendu zasięgu na zasilaniu bateryjnym."),
               tags$li("Wśród aut całkowicie elektrycznych wyraźnie dominuje marka Tesla. W przypadku aut hybrydowyh typu plug-in liderem jest Toyota, ale konkurencja jest bardziej wyrównana."),
               tags$li("W stanie Waszyngton większość rejestrowanych aut jest objęta programem dotacji CAFV, co może świadczyć o tym że konsumenci chętniej wybierają te pojazdy."),
               tags$li("Największa koncentracja aut występuje w obszarach metropolitalnych. Może być to spowodowane większą dostępnością infrastruktury (np. ładowarek)")
             ))
  )
)

# --- LOGIKA SERWERA (SERVER) ---
server <- function(input, output) {
  
  # Wykres 1: Trend zasięgu
  output$range_plotly_plot <- renderPlotly({
    req(input$ev_type_filter, input$years_plotly)
    plot_data <- df %>%
      filter(electric_range > 0, ev_type %in% input$ev_type_filter,
             model_year >= input$years_plotly[1], model_year <= input$years_plotly[2]) %>%
      group_by(model_year) %>%
      summarise(avg_range = round(mean(electric_range), 1), count = n())
    
    fit <- loess(avg_range ~ model_year, data = plot_data)
    plot_data$trend <- predict(fit)
    
    plot_ly(plot_data, x = ~model_year) %>%
      add_markers(y = ~avg_range, name = "Śr. zasięg", text = ~paste("Aut:", count)) %>%
      add_lines(y = ~trend, name = "Trend (LOESS)") %>%
      layout(xaxis = list(title = "Rok"), yaxis = list(title = "Zasięg (mile)"))
  })
  
  # Wykres 2: Popularność marek
  output$popularity_bar_plot <- renderPlotly({
    req(input$ev_type_filter2, input$years_plotly2)
    plot_data <- df %>%
      filter(ev_type %in% input$ev_type_filter2,
             model_year >= input$years_plotly2[1], model_year <= input$years_plotly2[2]) %>%
      count(make) %>% arrange(desc(n)) %>% head(15)
    
    plot_ly(plot_data, x = ~reorder(make, -n), y = ~n, type = "bar", marker = list(color = '#3498db')) %>%
      layout(xaxis = list(title = "Producent"), yaxis = list(title = "Liczba rejestracji"))
  })
  
  # Wykres 3: Analiza CAFV
  output$cafv_bar_plot <- renderPlotly({
    req(input$years_plotly3)
    cafv_data <- df %>%
      filter(model_year >= input$years_plotly3[1], model_year <= input$years_plotly3[2],
             cafv_type != "Eligibility unknown as battery range has not been researched") %>%
      mutate(status = case_when(
        cafv_type == "Clean Alternative Fuel Vehicle Eligible" ~ "Kwalifikuje się",
        cafv_type == "Not eligible due to low battery range" ~ "Brak kwalifikacji",
        TRUE ~ "Inne"
      )) %>%
      group_by(model_year, status) %>%
      summarise(count = n(), .groups = "drop")
    
    plot_ly(cafv_data, x = ~model_year, y = ~count, color = ~status, type = "bar", colors = c("#e74c3c", "#2ecc71")) %>%
      layout(barmode = "stack", xaxis = list(title = "Rok"), yaxis = list(title = "Liczba aut"))
  })
  
  # Wykres 4: Mapa Leaflet
  output$ev_map <- renderLeaflet({
    req(df_map)
    leaflet(df_map) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        lng = ~lng, lat = ~lat,
        popup = ~paste0("<b>", make, " ", model, "</b><br>Rok: ", model_year),
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lng = -120.74, lat = 47.75, zoom = 6)
  })
  
  # Tabele danych
  output$raw_data_table <- renderDataTable({ df }, options = list(pageLength = 5, scrollX = TRUE))
  output$raw_data_table_models <- renderDataTable({ df_models }, options = list(pageLength = 5, scrollX = TRUE))
}

shinyApp(ui, server)