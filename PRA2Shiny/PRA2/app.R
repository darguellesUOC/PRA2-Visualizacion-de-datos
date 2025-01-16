# Cargar librerías
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(maps)

# Cargar datos
data_clean <- read.csv("global-data-on-sustainable-energy.csv")

# Renombrar columnas para facilitar su manejo
colnames(data_clean) <- gsub("\\.+", "_", colnames(data_clean))

# Limpieza inicial y filtrado de datos
data_clean <- data_clean %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  filter(
    !is.na(Year),
    Year > 1999 & Year <= 2019
  )

# Calcular variables adicionales
data_clean <- data_clean %>%
  mutate(
    Proporcion_Electricidad_Renovable = Electricity_from_renewables_TWh_ /
      (Electricity_from_fossil_fuels_TWh_ + Electricity_from_nuclear_TWh_ + Electricity_from_renewables_TWh_),
    Densidad_Energetica = Electricity_from_renewables_TWh_ / Land_Area_Km2_,
    Intensidad_de_Emisiones = Value_co2_emissions_kt_by_country / Primary_energy_consumption_per_capita_kWh_person_
  ) %>%
  # Filtrar valores extremos
  filter(
    !is.na(Proporcion_Electricidad_Renovable),
    !is.na(Densidad_Energetica),
    !is.na(Intensidad_de_Emisiones)
  ) %>%
  mutate(Region = case_when(
    Entity %in% c("Germany", "France", "Spain", "Italy", "UK", "Ireland", "Austria", "Belgium", "Switzerland", 
                  "Netherlands", "Portugal", "Sweden", "Norway", "Finland", "Denmark", "Iceland", "Greece", 
                  "Poland", "Czech Republic", "Slovakia", "Hungary", "Slovenia", "Croatia", "Bosnia and Herzegovina", 
                  "Serbia", "Montenegro", "North Macedonia", "Albania", "Bulgaria", "Romania", "Lithuania", 
                  "Latvia", "Estonia", "Belarus", "Ukraine") ~ "Europe",
    
    Entity %in% c("China", "Japan", "India", "South Korea", "North Korea", "Indonesia", "Thailand", "Malaysia", 
                  "Philippines", "Vietnam", "Myanmar", "Laos", "Cambodia", "Bangladesh", "Sri Lanka", "Nepal", 
                  "Maldives", "Bhutan", "Pakistan", "Afghanistan", "Kazakhstan", "Uzbekistan", "Turkmenistan", 
                  "Kyrgyzstan", "Tajikistan", "Mongolia", "Iran", "Iraq", "Israel", "Jordan", "Lebanon", 
                  "Syria", "Yemen", "Saudi Arabia", "United Arab Emirates", "Qatar", "Kuwait", "Bahrain", "Oman") ~ "Asia",
    
    Entity %in% c("USA", "Canada", "Mexico", "Guatemala", "Honduras", "El Salvador", "Nicaragua", "Costa Rica", 
                  "Panama", "Cuba", "Jamaica", "Dominican Republic", "Haiti", "Puerto Rico", "Bahamas", "Barbados", 
                  "Trinidad", "Saint Kitts", "Saint Lucia", "Saint Vincent", "Grenada", "Belize") ~ "North America",
    
    Entity %in% c("Brazil", "Argentina", "Chile", "Colombia", "Venezuela", "Peru", "Paraguay", "Uruguay", 
                  "Bolivia", "Ecuador", "Guyana", "Suriname") ~ "South America",
    
    Entity %in% c("South Africa", "Nigeria", "Egypt", "Kenya", "Ethiopia", "Sudan", "South Sudan", "Morocco", 
                  "Algeria", "Tunisia", "Libya", "Ghana", "Ivory Coast", "Senegal", "Mali", "Burkina Faso", 
                  "Niger", "Chad", "Cameroon", "Central African Republic", "Republic of Congo", "Democratic Republic of the Congo", 
                  "Rwanda", "Burundi", "Uganda", "Tanzania", "Zambia", "Malawi", "Zimbabwe", "Angola", "Namibia", 
                  "Botswana", "Lesotho", "Eswatini", "Mozambique", "Madagascar", "Mauritius", "Seychelles", 
                  "Comoros", "Sao Tome and Principe", "Cape Verde", "Somalia", "Djibouti", "Eritrea", "Gambia", 
                  "Guinea", "Guinea-Bissau", "Liberia", "Sierra Leone") ~ "Africa",
    
    Entity %in% c("Australia", "New Zealand", "Fiji", "Papua New Guinea", "Samoa", "Tonga", "Vanuatu", "Kiribati", 
                  "Nauru", "Tuvalu", "Solomon Islands") ~ "Oceania",
    
    TRUE ~ "Other"
  )
  )

# Crear categorías de emisiones
data_clean <- data_clean %>%
  mutate(emisiones_categoria = case_when(
    Value_co2_emissions_kt_by_country <= 50000 ~ "Emisiones CO2: Bajas",
    Value_co2_emissions_kt_by_country <= 500000 ~ "Emisiones CO2: Medias",
    TRUE ~ "Emisiones CO2: Altas"
  ))

# Crear el objeto world_map
world_map <- map_data("world")

# Corregir los nombres de los países en el dataset
data_clean <- data_clean %>%
  mutate(Entity = case_when(
    Entity == "Antigua and Barbuda" ~ "Antigua",
    Entity == "Congo" ~ "Republic of Congo",
    Entity == "Czechia" ~ "Czech Republic",
    Entity == "Eswatini" ~ "Swaziland",
    Entity == "Saint Kitts and Nevis" ~ "Saint Kitts",
    Entity == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
    Entity == "Trinidad and Tobago" ~ "Trinidad",
    Entity == "United Kingdom" ~ "UK",
    Entity == "United States" ~ "USA",
    Entity == "Russian Federation" ~ "Russia",
    TRUE ~ Entity
  ))

map_data <- left_join(world_map, data_clean, by = c("region" = "Entity"))

# Interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Análisis de Energía Sostenible"),
  navbarPage(
    title = "Exploración de Datos",
    
    # Pestaña 1: Tabla de Datos
    tabPanel("Tabla de Datos",
             fluidRow(
               column(6, dataTableOutput("summaryTable"))
             ),
             fluidRow(
               column(12, uiOutput("dataDescription"))
             )
    ),
    
    # Pestaña 2: Consumo de Energía Renovable
    tabPanel("Consumo de Energía Renovable",
             fluidRow(
               column(8, plotOutput("summaryPlot")),
               column(4, textOutput("renewableExplanation"))
             )
    ),
    
    # Pestaña 3: Proporción de Electricidad Renovable
    tabPanel("Proporción de Electricidad Renovable",
             fluidRow(
               column(8, plotOutput("renewablePlot")),
               column(4, textOutput("renewableSummary"))
             )
    ),
    
    # Pestaña 4: Intensidad de Emisiones
    tabPanel("Intensidad de Emisiones",
             fluidRow(
               column(8, plotOutput("emissionIntensityPlotLog")),
               column(4, textOutput("emissionIntensityLogExplanation"))
             )
    ),
    
    # Pestaña 5: Densidad Energética Renovable
    tabPanel("Densidad Energética Renovable",
             fluidRow(
               column(8, plotOutput("densityPlot")),
               column(4, textOutput("densitySummary"))
             )
    ),
    
    # Pestaña 6: Acceso a Electricidad
    tabPanel("Acceso a Electricidad",
             fluidRow(
               column(8, plotOutput("electricityAccessPlot")),
               column(4, textOutput("electricityAccessSummary"))
             )
    ),
    
    # Pestaña 7: Relación Entre Emisiones de CO2 y Energía Renovable
    tabPanel("Relación entre Emisiones de CO2 y Energía Renovable",
             fluidRow(
               column(8, plotOutput("co2RenewablePlot")),
               column(4, textOutput("co2RenewableConclusion"))
             )
    ),
    
    # Pestaña 8: Mapas de energía renovable
    tabPanel("Evolución Mundial de la Energía Renovable",
             fluidRow(
               column(10, 
                      selectInput("selected_year", "Seleccione un Año:", 
                                  choices = c(2000, 2005, 2010, 2015, 2019)),
                      plotOutput("renewableMap"))
             )
    ),
    
    # Pestaña 9: Mapa de Energía Renovable en Países Tropicales
    tabPanel("Energía Renovable en los Trópicos",
             fluidRow(
               column(10, plotOutput("tropicsRenewableMap")),
               column(2, textOutput("tropicsExplanation"))
             )
    ),
    
    # Pestaña 10: Acceso a Combustibles Limpios y la Perspectiva de Género
    tabPanel("Combustibles Limpios y Género",
             fluidRow(
               column(8, plotOutput("cleanFuelsPlot")),
               column(4, textOutput("cleanFuelsExplanation"))
             )
    ),
    
    # Pestaña 11: Electricidad Renovable vs Consumo Total por Región
    tabPanel("Electricidad Renovable frente a Consumo Total Energético por Región",
             fluidRow(
               column(8, plotOutput("renewableEnergyByRegion")),
               column(4, textOutput("renewableEnergyByRegionExplanation"))  
             )
    )
  )
)

# Lógica del servidor (Server)
server <- function(input, output) {
  # Tabla de Datos
  output$summaryTable <- renderDataTable({
    data_clean %>%
      select(Year, Access_to_electricity_of_population_, Access_to_clean_fuels_for_cooking,
             Renewable_electricity_generating_capacity_per_capita, Financial_flows_to_developing_countries_US_,
             Renewable_energy_share_in_the_total_final_energy_consumption_) %>%
      arrange(Year) # Ordenar por el año, opcional
  })
  
  output$dataDescription <- renderUI({
    HTML("Los datos utilizados en esta aplicación provienen del conjunto de datos 
        <a href='https://www.kaggle.com/datasets/anshtanwar/global-data-on-sustainable-energy' 
        target='_blank'>Global Data on Sustainable Energy (2000-2020)</a>, disponible en Kaggle. 
        Este conjunto de datos abarca información sobre el acceso a la electricidad, el uso de combustibles limpios para cocinar, 
        la capacidad de generación de electricidad renovable per cápita, los flujos financieros hacia países en desarrollo 
        para energía sostenible y la proporción de energía renovable en el consumo final de energía. 
        La recopilación de estos datos permite analizar tendencias y patrones en el desarrollo de la energía sostenible 
        a nivel mundial durante el período 2000-2020.")
  })
  
  # Gráfico Resumen General
  output$summaryPlot <- renderPlot({
    ggplot(data_clean, aes(x = Year, y = Renewable_energy_share_in_the_total_final_energy_consumption_, color = Region)) +
      geom_smooth(se = FALSE) +
      labs(
        title = "Consumo de Energía Renovable por Año",
        x = "Año",
        y = "Proporción (%)"
      ) +
      theme_minimal()
  })
  
  # Proporción de Electricidad Renovable
  output$renewablePlot <- renderPlot({
    ggplot(data_clean, aes(x = Year, y = Proporcion_Electricidad_Renovable, color = Region)) +
      geom_smooth(se = FALSE, method = "loess") +
      labs(
        title = "Proporción de Electricidad Renovable por Región",
        x = "Año",
        y = "Proporción (%)"
      ) +
      theme_minimal()
  })
  
  output$renewableExplanation <- renderText({
    "Este gráfico muestra la proporción de energía renovable en el consumo final total de energía por región. África lidera esta métrica debido a su dependencia histórica de fuentes renovables tradicionales, como la biomasa utilizada para cocinar y calefacción. Sin embargo, esta dependencia también resalta una brecha tecnológica en el desarrollo de infraestructuras modernas. Europa y América del Sur presentan proporciones significativas, impulsadas por avances en energía hidroeléctrica y políticas renovables. En contraste, regiones como Asia y América del Norte tienen proporciones más bajas debido a su mayor dependencia de combustibles fósiles para transporte, industria y generación de energía"
  })
  
  output$renewableSummary <- renderText({
    "Este gráfico muestra la proporción de electricidad generada a partir de fuentes renovables por región. Sudamérica lidera gracias a su alta dependencia de la energía hidroeléctrica, mientras que Europa registra un crecimiento constante impulsado por inversiones en energía eólica y solar. Regiones como Asia y América del Norte presentan menores proporciones, reflejando su dependencia de combustibles fósiles para generación eléctrica. Este análisis se enfoca exclusivamente en la electricidad, destacando diferencias regionales en la transición energétic"
  })
  
  # Intensidad de Emisiones
  output$emissionIntensityPlot <- renderPlot({
    ggplot(data_clean, aes(x = Year, y = Intensidad_de_Emisiones, color = Region)) +
      geom_smooth(se = FALSE) +
      labs(
        title = "Intensidad de Emisiones por Región",
        x = "Año",
        y = "Intensidad de Emisiones (CO₂ por unidad de energía)"
      ) +
      theme_minimal()
  })
  
  output$emissionSummary <- renderText({
    "La intensidad de emisiones indica qué tan eficientes son las regiones en términos de consumo energético y emisiones de CO₂. Regiones con menores valores son más eficientes."
  })
  
  # Densidad Energética
  output$densityPlot <- renderPlot({
    ggplot(data_clean, aes(x = Year, y = Densidad_Energetica, color = Region)) +
      geom_smooth(se = FALSE, method = "loess") +
      scale_y_continuous(trans = "log10", labels = scales::comma) +
      labs(
        title = "Densidad Energética por Región",
        x = "Año",
        y = "Densidad Energética (TWh por Km²)"
      ) +
      theme_minimal()
  })
  
  output$densitySummary <- renderText({
    "La densidad energética renovable, medida en TWh por kilómetro cuadrado, indica cuánta electricidad renovable se genera en relación con el área de una región. Europa lidera este indicador gracias a su alta eficiencia en generación de energía renovable, particularmente eólica y solar, combinada con su superficie relativamente pequeña. Por otro lado, regiones como África y Asia presentan densidades más bajas debido a su infraestructura renovable menos desarrollada y sus grandes extensiones territoriales. Este análisis destaca la densidad energética como una medida clave para evaluar la eficiencia espacial de la transición energética."
  })


# Acceso a Electricidad
  output$electricityAccessPlot <- renderPlot({
    paises_seleccionados <- c("Afghanistan", "Bangladesh", "Angola", "Guatemala", "Nepal", "Vietnam", 
                              "Kenya", "Germany", "China", "Brazil", "United States")
    
    data_clean %>%
      filter(Entity %in% paises_seleccionados) %>%
      ggplot(aes(x = Year, y = Access_to_electricity_of_population_, color = Entity)) +
      geom_line(size = 1) +
      labs(
        title = "Acceso a Electricidad (% de Población)",
        x = "Año",
        y = "Porcentaje de Acceso",
        color = "País"
      ) +
      theme_minimal()
  })
  
  output$electricityAccessSummary <- renderText({
    "La gráfica evidencia una brecha inicial significativa en el acceso a electricidad entre países desarrollados y en desarrollo, pero también muestra que muchos países en desarrollo han logrado avances notables en la electrificación de su población. Esto resalta el impacto de iniciativas globales y locales dirigidas a cerrar esta brecha."
  })
  
  # Gráfica de relación entre emisiones de CO2 y energía renovable
  output$co2RenewablePlot <- renderPlot({
    resumen <- data_clean %>%
      group_by(emisiones_categoria) %>%
      summarise(energia_renovable_promedio = mean(Renewable_energy_share_in_the_total_final_energy_consumption_, na.rm = TRUE))
    
    ggplot(resumen, aes(x = emisiones_categoria, y = energia_renovable_promedio, fill = emisiones_categoria)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Promedio de Energía Renovable por Categoría de Emisiones",
        x = "Categoría de Emisiones de CO2",
        y = "Porcentaje Promedio de Energía Renovable",
        fill = "Categoría de Emisiones"
      ) +
      theme_minimal() +
      scale_fill_manual(
        values = c("Emisiones CO2: Bajas" = "green", 
                   "Emisiones CO2: Medias" = "blue", 
                   "Emisiones CO2: Altas" = "red")
      )
  })
  
  # Conclusión sobre la relación entre emisiones de CO2 y energía renovable
  output$co2RenewableConclusion <- renderText({
    "La relación entre emisiones de CO₂ y el uso de energía renovable muestra que:
    - Los países con bajas emisiones de CO₂ tienen un mayor porcentaje de energía renovable.
    - Los países con emisiones medias presentan un balance moderado entre renovables y combustibles fósiles.
    - Los países con altas emisiones tienen el menor porcentaje de renovables, reflejando su alta dependencia de combustibles fósiles."
  })
  
  # Función para generar el mapa por cada año
  generate_map <- function(year) {
    # Filtrar los datos por el año inciado
    year_data <- data_clean %>% filter(Year == year)
    
    # Juntar los datos del mapa con los datos del año específico
    map_data <- left_join(world_map, year_data, by = c("region" = "Entity"))
    
    # Crear el mapa
    ggplot(map_data, aes(long, lat, group = group, 
                         fill = Renewable_energy_share_in_the_total_final_energy_consumption_)) +
      geom_polygon(color = "white") +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                           midpoint = 50, name = "Energía Renovable (%)") +
      labs(title = paste("Proporción de Energía Renovable por País -", year)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold")
      )
  }
  
  # Generar mapa dinámico en función del año seleccionado
  output$renewableMap <- renderPlot({
    generate_map(as.numeric(input$selected_year))
  })
  
  # Generar el mapa para los países entre los trópicos
  output$tropicsRenewableMap <- renderPlot({
    # Filtrar datos para los países dentro de los trópicos
    map_data_tropics <- map_data %>%
      filter(lat <= 23.5 & lat >= -23.5)
    
    # Crear el mapa
    ggplot() +
      # Fondo: Mapa completo en gris claro
      geom_polygon(data = world_map, aes(long, lat, group = group), fill = "grey90", color = "white") +
      # Colorear los países entre los trópicos con los valores de energía renovable
      geom_polygon(data = map_data_tropics, aes(long, lat, group = group, fill = Renewable_energy_share_in_the_total_final_energy_consumption_), color = "white") +
      # Escala de colores
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Energía Renovable (%)") +
      # Agregar las líneas de los trópicos
      geom_hline(yintercept = 23.5, color = "red", linetype = "dashed", size = 1) +  # Trópico de Cáncer
      geom_hline(yintercept = -23.5, color = "blue", linetype = "dashed", size = 1) +  # Trópico de Capricornio
      # Etiquetas y tema
      labs(title = "Proporción de Energía Renovable por País (Entre los Trópicos)") +
      theme_minimal()
  })
  
  # Texto explicativo sobre los trópicos
  output$tropicsExplanation <- renderText({
    "La región tropical es particularmente relevante desde el punto de vista del desarrollo, ya que esta región combina climas favorables para energía renovable, como solar e hidroeléctrica, con desafíos como acceso limitado a tecnología e infraestructura. Analizar su distribución energética puede impulsar estrategias sostenibles que reduzcan la dependencia de combustibles fósiles y fortalezcan la resiliencia climática."
  })
  
  # Gráfico de Acceso a Combustibles Limpios por Región
  output$cleanFuelsPlot <- renderPlot({
    data_clean %>%
      filter(Region != "Other") %>%
      group_by(Region) %>%
      summarise(CleanFuelsAccess = mean(Access_to_clean_fuels_for_cooking, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Region, -CleanFuelsAccess), y = CleanFuelsAccess, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Acceso a Combustibles Limpios por Región",
        x = "Región",
        y = "Porcentaje de Acceso a Combustibles Limpios"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Texto explicativo sobre el acceso a combustibles limpios
  output$cleanFuelsExplanation <- renderText({
    "El acceso a combustibles limpios para cocinar es una métrica clave en la perspectiva de género, ya que en muchas regiones, las mujeres son las principales responsables de la cocina y la recolección de combustibles. 
   Una mayor disponibilidad de combustibles limpios no solo reduce el tiempo invertido en esta tarea, sino que también disminuye los riesgos para la salud asociados con el uso de combustibles sólidos y la contaminación del aire en interiores. 
   Esto tiene un impacto positivo en el empoderamiento de las mujeres y el desarrollo sostenible."
  })
  
  output$renewableEnergyByRegion <- renderPlot({
    data_clean %>%
      filter(Region != "Other") %>%
      group_by(Region, Year) %>%
      summarise(RenewableEnergy = mean(Renewable_energy_share_in_the_total_final_energy_consumption_, na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = RenewableEnergy, color = Region)) +
      geom_line(size = 1.2) +
      labs(
        title = "Proporción de Electricidad Producida Mediante Energía Renovable por Región frente a Consumo Total de Energía",
        x = "Año",
        y = "Porcentaje de Energía Renovable",
        color = "Región"
      ) +
      theme_minimal()
  })
  
  output$renewableEnergyByRegionExplanation <- renderText({
    "Este gráfico representa la proporción de electricidad generada a partir de fuentes renovables en comparación con el consumo total de energía (electricidad y otras formas de energía) por región. Europa muestra un crecimiento continuo gracias a inversiones significativas en tecnologías renovables modernas, como energía eólica y solar. Sudamérica también mantiene una alta proporción, impulsada principalmente por la energía hidroeléctrica. En África, la proporción de electricidad renovable es relativamente baja debido a limitaciones en la infraestructura para generación moderna, aunque destaca por su dependencia de fuentes renovables no eléctricas. Este gráfico resalta cómo las regiones avanzan de manera desigual en la transición hacia la electricidad limpia"
  })
  
  # Intensidad de Emisiones con Escala Logarítmica
  output$emissionIntensityPlotLog <- renderPlot({
    data_clean %>%
      filter(
        !is.na(Value_co2_emissions_kt_by_country) &
          Value_co2_emissions_kt_by_country > 0 &
          !is.na(Primary_energy_consumption_per_capita_kWh_person_) &
          Primary_energy_consumption_per_capita_kWh_person_ > 0
      ) %>%
      mutate(
        Intensidad_de_Emisiones = Value_co2_emissions_kt_by_country / Primary_energy_consumption_per_capita_kWh_person_
      ) %>%
      filter(
        !is.na(Intensidad_de_Emisiones) &
          Intensidad_de_Emisiones > 0
      ) %>%
      ggplot(aes(x = Year, y = Intensidad_de_Emisiones, color = Region)) +
      geom_smooth(se = FALSE, size = 1.2, method = "loess") +
      scale_y_continuous(trans = "log10", labels = scales::comma) +
      labs(
        title = "Intensidad de Emisiones de CO2 por Región",
        subtitle = "Escala logarítmica aplicada para resaltar variaciones extremas",
        x = "Año",
        y = "Intensidad de Emisiones (CO2 por unidad de energía, escala logarítmica)",
        color = "Región"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12)
      )
  })
  
  output$emissionIntensityLogExplanation <- renderText({
    "Este gráfico utiliza una escala logarítmica para representar la intensidad de emisiones de CO₂ (definida como las emisiones totales divididas por el consumo de energía primaria per cápita) por región. 
  La escala logarítmica resalta variaciones extremas y permite observar tendencias tanto en regiones con emisiones muy bajas como en aquellas con valores mucho más altos. 
  Asia muestra consistentemente los valores más altos, reflejando una fuerte dependencia de combustibles fósiles. Por el contrario, Oceanía y Europa tienen valores más bajos, destacando su transición hacia fuentes de energía más limpias."
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

