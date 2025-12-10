library(shiny)
library(leaflet)
library(sf)
library(DT)
library(RColorBrewer)
library(raster)

# --- ПОЛЬЗОВАТЕЛЬСКИЙ ИНТЕРФЕЙС (UI) ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Стили заголовков */
      .mytitle-style {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
        font-size: 16px !important;      
        font-weight: 700 !important;     
        color: #333333 !important;       
        line-height: 1.2 !important;
        margin: 0 !important;
        padding: 0 !important;
      }
      /* Стили лейблов */
      label[for='city'] {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
        font-size: 16px !important;
        font-weight: 700 !important;
        color: #333333 !important;
        line-height: 1.2 !important;
      }
      /* Глобальные настройки контейнеров ввода (Flexbox) */
      .shiny-input-container {
        display: flex !important;       
        align-items: center !important; 
      }
      .shiny-input-container label {
        margin-right: 15px !important;  
        white-space: nowrap !important; 
        width: auto !important;
      }
      .shiny-input-container .selectize-control {
        flex-grow: 0 !important;        
        width: 20ch !important;         
        margin-bottom: 0 !important;
      }
      /* Стилизация таблицы DT */
      table.dataTable tbody tr td {
        padding: 4px 5px !important;
        line-height: 1.1 !important;
        font-size: 11px;
      }
      table.dataTable thead tr th {
        padding: 5px 5px !important;
        font-size: 11px;
      }
      table.dataTable tbody tr.selected td {
        background-color: #b0f2b6 !important; 
        color: black !important;               
        box-shadow: none !important;           
      }
      /* Отступы для заголовков в сайдбаре */
      .modeling-wrapper {
        margin-top: 25px; 
        margin-bottom: 10px;
      }
      /* Стилизация вкладок (Tabs) */
      .tab-content {
        border: none !important;
        background-color: transparent !important;
        padding-top: 15px !important;
        box-shadow: none !important;
      }
      .nav-tabs {
        border-bottom: 1px solid #ddd;
      }
      /* Блок деталей региона */
      .region-details {
        background-color: #f9f9f9;
        border: 1px solid #e3e3e3;
        padding: 10px;
        border-radius: 4px;
        margin-bottom: 0px; 
        font-size: 12px;
        height: 100%;       
      }
      
      /* Компактный стиль для правой колонки во вкладке моделирования */
      .compact-input-col .shiny-input-container {
        display: flex !important;             
        align-items: center !important;       
        margin-bottom: 10px !important;
        width: 100% !important;
      }
      .compact-input-col label {
        width: 65% !important;                
        white-space: normal !important;       
        margin-right: 5px !important;         
        font-size: 11px !important;           
        line-height: 1.1 !important;
        font-weight: normal !important;
      }
      .compact-input-col input {
        width: 100% !important;               
        padding: 4px !important;              
        height: 30px !important;              
      }
      .compact-input-col .form-group {
        width: 35% !important;                
        margin-bottom: 0 !important;
      }
      
      /* Стиль для текста результата расчета */
      .calc-result {
        margin-top: 5px;
        font-size: 12px;
        color: #2c3e50;
        font-weight: bold;
        text-align: right; 
        padding-right: 5px;
      }
    "))
  ),
  
  titlePanel("Heat Risk Index (HRI) for Colorado Cities"),
  
  sidebarLayout(
    sidebarPanel(
      width = 5, 
      
      # Выбор города
      selectInput(
        "city",
        "Select a city:", 
        choices = c("Fort Collins" = "HRI_FoCo",
                    "Loveland" = "HRI_Loveland", 
                    "Aspen" = "HRI_Aspen",
                    "Grand Junction" = "HRI_Grand_Junction"),
        selected = "HRI_FoCo"
      ),
      
      # Таблица данных
      DTOutput("table"),
      
      div(class = "modeling-wrapper",
          div("Modeling", class = "mytitle-style")
      ),
      
      # Вкладки управления
      tabsetPanel(type = "tabs",
                  id = "main_tabs",
                  
                  # Вкладка 1: Описание и выбор метода расчета
                  tabPanel("Description",
                           div(class = "tab-content",
                               tags$style(HTML("
                                 #desc_radio_fix .shiny-input-container {
                                   display: block !important; 
                                 }
                                 #desc_radio_fix label.control-label {
                                   white-space: normal !important;
                                   width: 100% !important;
                                   margin-bottom: 10px !important;
                                 }
                                 #desc_radio_fix .radio label {
                                   white-space: normal !important;
                                   padding-left: 20px !important;
                                   width: 100% !important;
                                 }
                                 #desc_radio_fix .radio {
                                   margin-top: 5px !important;
                                   margin-bottom: 10px !important;
                                 }
                               ")),
                               
                               fluidRow(
                                 column(6, 
                                        div(id = "desc_radio_fix",
                                            radioButtons("desc_radio", "Method of calculating the index:",
                                                         choices = c("Pop_Densit + PCT_Lackin + MAX" = "index1",
                                                                     "Pop_Densit + PCT_Lackin + MEAN" = "index2",
                                                                     "Pop_Densit + PCT_Lackin + MIN" = "index3"))
                                        )
                                 ),
                                 column(6,
                                        p("1. The modeling results are evaluated five years after the changes have been implemented."),
                                        p("2. We assume that after planting a seedling, its crown area will reach 4 square meters within five years."),
                                        p("3. The 'utility of the changes' is defined as the anticipated reduction in peak temperature within a specific area, multiplied by the population of that area.")
                                 )
                               )
                           )
                  ),
                  
                  # Вкладка 2: Изменение параметров региона
                  tabPanel("Change current region",
                           div(class = "tab-content",
                               fluidRow(
                                 column(4, 
                                        uiOutput("current_region_info")
                                 ),
                                 column(8, class = "compact-input-col",
                                        numericInput("input_trees", "Plant trees (pcs):", value = 0, min = 0, step = 1),
                                        numericInput("input_area", "Increase planting area (%):", value = 0, min = 0, step = 1),
                                        div(class = "calc-result",
                                            textOutput("utility_result")
                                        )
                                 )
                               )
                           )
                  ),
                  
                  # Вкладка 3: Оптимальные изменения (заглушка)
                  tabPanel("Optimal change",
                           div(class = "tab-content",
                               p("...optimum_output..."),
                               verbatimTextOutput("optimum_output")
                           )
                  )
      )
    ),
    
    # Основная панель с картой
    mainPanel(
      width = 7, 
      leafletOutput("map", height = "700px")
    )
  )
)

# --- ЛОГИКА СЕРВЕРА ---
server <- function(input, output, session) {
  
  # ------------------------------
  # 0. РЕАКТИВНЫЕ ХРАНИЛИЩА
  # ------------------------------
  
  rv <- reactiveValues(
    df = NULL,        
    last_row = NULL,
    dt_trigger = 0 
  )
  
  utility_msg <- reactiveVal("") 
  
  
  # ------------------------------
  # 1. ЗАГРУЗКА ДАННЫХ ГОРОДА
  # ------------------------------
  
  city_data <- reactive({
    req(input$city)
    
    base_path <- "data/"
    shp_path <- file.path(base_path, input$city, paste0(input$city, ".shp"))
    
    # --- Основные реальные данные ---
    if (file.exists(shp_path)) {
      shp <- st_read(shp_path, quiet = TRUE)
      shp <- st_zm(shp)
      shp <- st_transform(shp, 4326)
      return(shp)
    }
    
    # --- Тестовые данные ---
    poly <- st_polygon(list(matrix(
      c(-105,40,-104,40,-104,41,-105,41,-105,40),
      ncol = 2, byrow = TRUE
    )))
    
    d <- st_sf(geometry = st_sfc(poly), crs = 4326)
    
    d$Area_km      <- c(3.77, 4.01, 2.50)
    d$Total_pop_   <- c(2192, 702, 1500)
    d$Pop_Dens_1   <- c(5, 7, 6)
    d$PCT_Lack_1   <- c(5, 10, 8)
    
    d$MIN          <- c(15, 14, 13)
    d$MEAN         <- c(25, 24, 23)
    d$MAX          <- c(35, 32, 30)
    
    d$MAX_MIN_MA   <- d$MAX
    d$MEAN_MIN_M   <- d$MEAN
    d$MIN_MIN_MA   <- d$MIN
    
    # Дублируем для проверки пагинации
    d <- rbind(d, d, d, d, d) 
    return(d)
  })
  
  
  # ------------------------------
  # 2. ИНИЦИАЛИЗАЦИЯ rv$df
  # ------------------------------
  
  observeEvent(city_data(), {
    df <- st_drop_geometry(city_data())
    names(df) <- make.names(names(df), unique = TRUE)
    
    rv$df <- df
    rv$last_row <- NULL
    
    rv$dt_trigger <- rv$dt_trigger + 1
    
    utility_msg("")
    updateNumericInput(session, "input_trees", value = 0)
    updateNumericInput(session, "input_area", value = 0)
  })
  
  
  # ------------------------------
  # 3. ЧТЕНИЕ КОЭФФИЦИЕНТА LM
  # ------------------------------
  
  lm_coef <- reactive({
    path <- file.path("data", input$city, "LM.txt")
    if (!file.exists(path)) return(-0.5) 
    df <- tryCatch(read.table(path, header = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) < 2) return(-0.5)
    as.numeric(df[2,1])
  })
  
  
  # ------------------------------
  # 4. ТАБЛИЦА
  # ------------------------------
  
  output$table <- renderDT({
    req(rv$dt_trigger > 0)
    data <- isolate(rv$df)
    
    datatable(
      data,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tp",
        autoWidth = FALSE,
        lengthChange = FALSE
      )
    )
  })
  
  
  # ------------------------------
  # 5. ИНФО О РЕГИОНЕ
  # ------------------------------
  
  output$current_region_info <- renderUI({
    idx <- input$table_rows_selected
    if (is.null(idx)) 
      return(div(class="region-details","Select a region..."))
    
    row <- rv$df[idx, ]
    
    cols <- intersect(
      c("Area_km","Total_pop_","PCT_Lack_1","MAX","MEAN","MIN","MAX_MIN_MA"),
      names(row)
    )
    
    li <- lapply(cols, function(col){
      val <- round(row[[col]], 2)
      tags$li(tags$strong(col),": ",val)
    })
    
    div(class="region-details",
        h5("Region details:"),
        tags$ul(li)
    )
  })
  
  
  # ------------------------------
  # 6. КАРТА (ДОБАВЛЕНА РАБОТА СО СЛОЯМИ)
  # ------------------------------
  
  output$map <- renderLeaflet({
    req(city_data())
    data <- city_data()
    
    # --- 1. Подготовка данных (City Data) ---
    col_temp <- switch(input$desc_radio,
                       "index1"="MAX_MIN_MA",
                       "index2"="MEAN_MIN_M",
                       "index3"="MIN_MIN_MA")
    
    df <- st_drop_geometry(data)
    
    if (all(c("Pop_Dens_1","PCT_Lack_1",col_temp) %in% names(df))) {
      raw_values <- df$Pop_Dens_1 + df$PCT_Lack_1 + df[[col_temp]]
    } else {
      raw_values <- runif(nrow(df), 3, 15)
    }
    
    # Считаем точные границы для City Data
    c_min <- min(raw_values, na.rm = TRUE)
    c_max <- max(raw_values, na.rm = TRUE)
    
    pal_city <- colorNumeric("YlOrRd", domain = raw_values)
    
    # --- 2. Поиск TIF (Landsat) ---
    city_path <- paste0("data/", input$city, "/")
    tif_files <- list.files(city_path, pattern = "\\.tif$", full.names = TRUE)
    
    # --- 3. Инициализация карты ---
    map <- leaflet(data) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")
    
    my_overlays <- c("City Data")
    
    # --- 4. Добавляем растр (Landsat) ---
    if (length(tif_files) > 0) {
      r <- raster(tif_files[1]) 
      r_values <- values(r)
      
      # Считаем точные границы для Landsat
      r_min <- min(r_values, na.rm = TRUE)
      r_max <- max(r_values, na.rm = TRUE)
      
      pal_raster <- colorNumeric("inferno", domain = r_values, na.color = "transparent")
      
      map <- map %>%
        addRasterImage(
          r, 
          colors = pal_raster, 
          opacity = 0.8, 
          group = "Landsat TIF", 
          maxBytes = 10 * 1024 * 1024
        ) %>%
        addLegend(
          "bottomright",
          pal = pal_raster,
          values = r_values,
          title = "Landsat",
          group = "Landsat TIF",
          opacity = 1,
          bins = 5, # Делает шкалу высокой (много делений)
          labFormat = function(type, cuts, p) {
            n <- length(cuts)
            # Создаем пустые подписи
            labels <- rep("", n)
            # Насильно ставим точный МИН и МАКС в начало и конец
            labels[1] <- round(r_min, 2)
            labels[n] <- round(r_max, 2)
            return(labels)
          }
        )
      
      my_overlays <- c(my_overlays, "Landsat TIF")
    }
    
    # --- 5. Финальная сборка (City Data) ---
    map %>%
      addPolygons(
        layerId = ~1:nrow(data),
        fillColor = ~pal_city(raw_values),
        fillOpacity = 0.7,
        color="white",
        weight=1,
        popup = ~paste("Index:",round(raw_values,2)),
        group = "City Data" 
      ) %>%
      addLegend(
        "bottomright", 
        pal=pal_city, 
        values=raw_values, 
        title = "City Index", 
        group = "City Data",
        opacity = 1,
        bins = 5, # Делает шкалу высокой
        labFormat = function(type, cuts, p) {
          n <- length(cuts)
          labels <- rep("", n)
          # Насильно ставим точный МИН и МАКС
          labels[1] <- round(c_min, 2)
          labels[n] <- round(c_max, 2)
          return(labels)
        }
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        overlayGroups = my_overlays,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft"
      ) %>%
      hideGroup("Landsat TIF")
  })
  
  # ------------------------------
  # 7. КЛИК ПО КАРТЕ
  # ------------------------------
  
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    
    row_id <- as.numeric(input$map_shape_click$id)
    page <- ceiling(row_id / 10)
    
    proxy <- dataTableProxy("table")
    selectPage(proxy, page)
    selectRows(proxy, row_id)
  })
  
  
  # ------------------------------
  # 8. ВЫБОР СТРОКИ
  # ------------------------------
  
  observeEvent(input$table_rows_selected, {
    row <- input$table_rows_selected
    if (is.null(row)) return()
    
    if (!is.null(rv$last_row) && row == rv$last_row) return()
    rv$last_row <- row
    
    data <- city_data()
    poly <- data[row, ]
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = poly,
        group = "highlight",
        color = "#00FFFF",
        weight = 5, fill = FALSE
      )
    
    utility_msg("")
    updateNumericInput(session,"input_trees",value=0)
    updateNumericInput(session,"input_area", value=0)
  })
  
  
  # ------------------------------
  # 9. ОБНОВЛЕНИЕ ДАННЫХ
  # ------------------------------
  
  update_simulation <- function(area_pct, row_id) {
    # --- 1. Подготовка данных ---
    orig_sf <- city_data()
    orig_df <- st_drop_geometry(orig_sf)
    names(orig_df) <- names(rv$df)
    
    coef_val <- lm_coef()
    pop_val  <- orig_df$Total_pop_[row_id]
    delta_temp <- area_pct * coef_val
    
    new_df <- rv$df
    temp_cols <- c("MAX", "MEAN")
    
    # --- 2. Обновляем температуру ---
    for (col in temp_cols) {
      if (col %in% names(new_df)) {
        new_df[row_id, col] <- orig_df[row_id, col] - delta_temp
      }
    }
    
    # --- 3. Пересчет индекса ---
    if ("MAX" %in% names(new_df)) {
      vals <- new_df$MAX
      min_v <- min(vals, na.rm = TRUE)
      max_v <- max(vals, na.rm = TRUE)
      if (max_v != min_v) {
        new_df$MAX_MIN_MA <- 1 + (vals - min_v) / (max_v - min_v) * 4
      } else {
        new_df$MAX_MIN_MA <- 1
      }
    }
    
    if ("MEAN" %in% names(new_df)) {
      vals <- new_df$MEAN
      min_v <- min(vals, na.rm = TRUE)
      max_v <- max(vals, na.rm = TRUE)
      if (max_v != min_v) {
        new_df$MEAN_MIN_M <- 1 + (vals - min_v) / (max_v - min_v) * 4
      } else {
        new_df$MEAN_MIN_M <- 1
      }
    }
    
    # --- 4. Сохраняем данные ---
    rv$df <- new_df
    
    # --- 5. Обновляем таблицу (Тихо) ---
    proxy_dt <- dataTableProxy("table")
    replaceData(proxy_dt, new_df, resetPaging = FALSE, clearSelection = "none", rownames = FALSE)
    selectRows(proxy_dt, row_id)
    
    # --- 6. ОБНОВЛЕНИЕ КАРТЫ (С учетом слоев) ---
    
    col_idx <- switch(input$desc_radio,
                      "index1" = "MAX_MIN_MA",
                      "index2" = "MEAN_MIN_M",
                      "index3" = "MIN_MIN_MA")
    
    if (all(c("Pop_Dens_1", "PCT_Lack_1", col_idx) %in% names(new_df))) {
      map_vals <- new_df$Pop_Dens_1 + new_df$PCT_Lack_1 + new_df[[col_idx]]
    } else {
      map_vals <- rep(0, nrow(new_df))
    }
    
    pal <- colorNumeric("YlOrRd", c(3, 15))
    geom_data <- city_data()
    
    # [ВАЖНО] Используем группу "City Data", чтобы она соответствовала Layers Control
    leafletProxy("map") %>%
      clearGroup("City Data") %>% 
      addPolygons(
        data = geom_data,
        layerId = ~1:nrow(geom_data),
        fillColor = ~pal(map_vals),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste("Index:", round(map_vals, 2)),
        group = "City Data" # <-- Имя группы должно совпадать с тем, что в renderLeaflet
      )
    
    # Восстанавливаем подсветку
    if (!is.null(row_id)) {
      poly_selected <- geom_data[row_id, ]
      leafletProxy("map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = poly_selected,
          group = "highlight",
          color = "#00FFFF",
          weight = 5, 
          fill = FALSE
        )
    }
    
    # --- 7. Текст результата ---
    utility_val <- delta_temp * pop_val
    paste(
      "Reducing peak temperature:", round(delta_temp, 1),
      "Utility of changes:", round(utility_val, 0)
    )
  }
  
  
  # ------------------------------
  # 10. МОДЕЛИРОВАНИЕ: ДЕРЕВЬЯ
  # ------------------------------
  
  observeEvent(input$input_trees, {
    req(rv$df)
    row_id <- input$table_rows_selected
    if (is.null(row_id)) return()
    
    trees <- input$input_trees
    if (is.null(trees) || is.na(trees)) trees <- 0
    
    orig <- st_drop_geometry(city_data())
    area <- orig$Area_km[row_id]
    
    #new_pct <- (trees * 4) / (area * 1e6) * 100
    new_pct <- (trees * 4) / (area * 1e6) * 100 * 10 #Увеличиваем процент площади в 10 раз
    
    updateNumericInput(session,"input_area", value = round(new_pct,4))
    
    msg <- update_simulation(new_pct, row_id)
    utility_msg(msg)
  })
  
  
  # ------------------------------
  # 11. МОДЕЛИРОВАНИЕ: % ПЛОЩАДИ
  # ------------------------------
  
  observeEvent(input$input_area, {
    req(rv$df)
    row_id <- input$table_rows_selected
    if (is.null(row_id)) return()
    
    pct <- input$input_area
    if (is.null(pct) || is.na(pct)) pct <- 0
    
    orig <- st_drop_geometry(city_data())
    area <- orig$Area_km[row_id]
    
    #trees <- round((pct/100) * area * 1e6 / 4)
    trees <- round((pct/100) * area * 1e6 / 4 / 10) #Уменьшаем кол-во деревьев в 10 раз
    
    if (input$input_trees != trees) {
      updateNumericInput(session, "input_trees", value = trees)
    }
    
    msg <- update_simulation(pct, row_id)
    utility_msg(msg)
  })
  
  
  # ------------------------------
  # 12. ТЕКСТ РЕЗУЛЬТАТА
  # ------------------------------
  
  output$utility_result <- renderText(utility_msg())
  output$optimum_output <- renderText("...optimum output....")
}

shinyApp(ui, server)