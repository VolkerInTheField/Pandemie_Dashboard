# # Shiny-Dashboard zu Corona - Covid19

packages <- c("dplyr", "stringr", "data.table", "DT", "fs",
              "lubridate", "ggplot2", "plotly",
              "rmarkdown", "shiny", "shinydashboard")
          
lapply(packages, require, character.only = TRUE)

source(paste0(getwd(),"/start.R"))

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(
    title = "Corona - Covid-19"
    
  ), # # ---Ende dashboardheader---
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Überblick",
               tabName = "id_ueberblick"),
      menuItem("Tabellarische Darstellung",
               tabName = "id_tabellarisch"),
      menuItem("Pivotbericht",
               tabName = "id_pivot"),
      menuItem("Grafische Darstellung",
               tabName = "id_grafisch")
    ) # # ---Ende sidebarMenu---
    
  ),
  dashboardBody(
    h5(paste0("Aktueller Stand: ", Sys.Date())),
    tabItems(
      tabItem(tabName = "id_ueberblick",
              h2("Überblick weltweit"), br(),
              fluidRow(
                infoBox(title = "Weitere Fallzahlen weltweit",
                        textOutput("id_infobox_01"),
                        width = 3,
                        color = "blue"),
                infoBox(title = "Weitere Todesfälle weltweit",
                        textOutput("id_infobox_02"),
                        width = 3,
                        color = "blue"),
                infoBox(title = "Kumulierte Fallzahlen weltweit",
                        textOutput("id_infobox_03"),
                        width = 3,
                        color = "red"),
                infoBox(title = "Kumulierte Todesfälle weltweit",
                        textOutput("id_infobox_04"),
                        width = 3,
                        color = "red")
              ), # # ---Ende 1. fluidRow in tabItem Ueberblick---
              
              fluidRow(
                infoBox(title = "Veränderungsrate Fallzahlen",
                        textOutput("id_infobox_05"),
                        width = 3,
                        color = "green"),
                infoBox(title = "Veränderungsrate Todesfälle",
                        textOutput("id_infobox_06"),
                        width = 3,
                        color = "green")
                
              ), # # ---Ende 2. fluidRow in tabItem Ueberblick---
              
              h2("Überblick deutschlandweit"), br(),
              fluidRow(
                infoBox(title = "Weitere Fallzahlen in Deutschland",
                        textOutput("id_infobox_07"),
                        width = 3,
                        color = "blue"),
                infoBox(title = "Weitere Todesfälle in Deutschland",
                        textOutput("id_infobox_08"),
                        width = 3,
                        color = "blue"),
                infoBox(title = "Kumulierte Fallzahlen in Deutschland",
                        textOutput("id_infobox_09"),
                        width = 3,
                        color = "red"),
                infoBox(title = "Kumulierte Todesfälle in Deutschland",
                        textOutput("id_infobox_10"),
                        width = 3,
                        color = "red")
              ), # # ---Ende 3. fluidRow in tabItem Ueberblick---
              
              fluidRow(
                infoBox(title = "Veränderungsrate Fallzahlen",
                        textOutput("id_infobox_11"),
                        width = 3,
                        color = "green"),
                infoBox(title = "Veränderungsrate Todesfälle",
                        textOutput("id_infobox_12"),
                        width = 3,
                        color = "green")
              ) # # ---Ende 4. fluidRow in tabItem Ueberblick---
              
            ), # # ---Ende tabItem Ueberblick---
      
      tabItem(tabName = "id_tabellarisch",
              h2("Tabellarische Darstellung"),
              box(DTOutput("id_datatable_01"), width = 12)
              
              ), # # ---Ende tabItem tabellarisch
      
      tabItem(tabName = "id_pivot",
              h2("Pivotbericht"),
              box(width = 12)
              
              ), # # ---Ende tabItem pivotbericht
      
      tabItem(tabName = "id_grafisch",
              h2("Grafische Darstellungen"),
              fluidRow(
                box(plotOutput("id_plot_uebersicht_weltweit")),
                
                box(plotOutput("id_plot_aenderungen_weltweit"))
                
                ), # # ---Ende 1. fluidRow in tabItem grafisch
              
              fluidRow(
                h2("Länderauswahl"),
                box(selectInput(inputId = "id_country_select",
                                label = "Länderauswahl",
                                selected = "Germany",
                                choices = df_list_country$Country,
                                multiple = TRUE),
                    sliderInput(inputId = "id_date_select",
                                label = "Zeitraum",
                                min = as.Date("2019-12-30"),
                                max = Sys.Date() - 1,
                                value = Sys.Date() - 1),
                    width = 3),
                
                box(plotOutput("id_plot_country_select"),
                    width = 9)
                
                
              ) # # ---Ende 2. fluidRow in tabItem grafisch
              
              ) # # ---Ende tabItem grafisch---
      
    
    ) # # ---Ende tabItems---
  )

) # # ---Ende ui---


server <- function(input, output){
  
  output$id_text_01 <- renderText({
    wert = as.character(df_time[1,1])
    value = wert
    #value = as.Date(df_time[1,1])
  })
  
  # # Neue Fälle weltweit
  output$id_infobox_01 <- renderText({
   value = paste0("Aktuell: ", format(as.integer(df_time[1,2]), big.mark = ".", decimal.mark = ",")
                  , " -- Vortag: ", format(as.integer(df_time[2,2]), big.mark = ".", decimal.mark = ","))
                 
  })
  
  # # Neue Todesfälle weltweit
  output$id_infobox_02 <- renderText({
    value = paste0("Aktuell: ", format(as.integer(df_time[1,3]), big.mark = ".", decimal.mark = ",")
                   , " -- Vortag: ", format(as.integer(df_time[2,3]), big.mark = ".", decimal.mark = ","))
  })
  
  # # kumulierte Fälle welzweit
  output$id_infobox_03 <- renderText({
    value = format(as.integer(df_time[1,4]), big.mark = ".", decimal.mark = ",")
  })
  
  # # kumulierte Todesfälle welzweit
  output$id_infobox_04 <- renderText({
    value = format(as.integer(df_time[1,5]), big.mark = ".", decimal.mark = ",")
  })
  
  # # Veränderungsrate Fälle weltweit
  output$id_infobox_05 <- renderText({
    value = paste0(format(as.integer(df_time[1,6]), big.mark = ".", decimal.mark = ","), "%")
  })
  
  # # Veränderungsrate Todesfälle weltweit
  output$id_infobox_06 <- renderText({
    value = paste0(format(as.integer(df_time[1,7]), big.mark = ".", decimal.mark = ","), "%")
  })
  
  # # Neue Fälle Deutschland
  output$id_infobox_07 <- renderText({
    value = paste0("Aktuell: ", format(as.integer(df_germany[1,3]), big.mark = ".", decimal.mark = ",")
                   , " -- Vortag: ", format(as.integer(df_germany[2,3]), big.mark = ".", decimal.mark = ","))
    
  })
  
  # # Neue Todesfälle Deutschland
  output$id_infobox_08 <- renderText({
    value = paste0("Aktuell: ", format(as.integer(df_germany[1,4]), big.mark = ".", decimal.mark = ",")
                   , " -- Vortag: ", format(as.integer(df_germany[2,4]), big.mark = ".", decimal.mark = ","))
  })
  
  # # kumulierte Fälle Deutschland
  output$id_infobox_09 <- renderText({
    value = format(as.integer(df_germany[1,5]), big.mark = ".", decimal.mark = ",")
  })
  
  # # kumulierte Todesfälle Deutschland
  output$id_infobox_10 <- renderText({
    value = format(as.integer(df_germany[1,6]), big.mark = ".", decimal.mark = ",")
  })
  
  # # Veränderungsrate Fälle Deutschland
  output$id_infobox_11 <- renderText({
    value = paste0(format(as.integer(df_germany[1,7]), big.mark = ".", decimal.mark = ","), "%")
  })
  
  # # Veränderungsrate Todesfälle Deutschland
  output$id_infobox_12 <- renderText({
    value = paste0(format(as.integer(df_germany[1,8]), big.mark = ".", decimal.mark = ","), "%")
  })
  
  # # Tabellarische Darstellung datatable 01
  output$id_datatable_01 <- renderDT({
    datatable(data_all_cum, rownames = FALSE,
              colnames = c("Land", "Datum", "weitere Fälle", "weitere Todesfälle", 
                           "kumulierte Fälle", "kumulierte Todesfälle", "Änderungsrate Fälle",
                           "Änderungsrate Todesfälle", "Fälle weltweit", "Todesfälle weltweit",
                           "kumulierte Fälle weltweit", "kumulierte Todesfälle weltweit",
                           "Änderungsrate Fälle weltweit", "Änderungsrate Todesfälle weltweit"))
    
  })
  
  # # # Pivotbericht
  # output$id_pivotbericht <- renderRpivotTable({
  #   rpivotTable(data_all_cum, rows = c("cases_country", "cum_cases"))
  # })
  
  # # Grafische Darstellungen
  output$id_plot_uebersicht_weltweit <- renderPlot({
    gp_ue_ww <- ggplot(df_time, aes(x = datum, y = day_cases_ww)) +
      geom_point() +
      geom_point(aes(x = datum, y = cum_day_cases_ww)) +
      scale_y_log10() +
      labs(title = "Weltweite Entwicklung der Corona Fallzahlen",
           x = "Zeit", y = "Neue und kumulierte Fallzahlen, logarithmisch") +
      theme(
        panel.background = element_rect(fill = "#ffcc99"),
        axis.title = element_text(color = "blue"),
        title = element_text(color = "blue")
      )
    gp_ue_ww
    
  })
  
  output$id_plot_aenderungen_weltweit <- renderPlot({
    gp_ae_ww <- ggplot(df_time, aes(x = datum, y = day_cases_ww)) +
      geom_point() +
      labs(title = "Tägliche Neuinfektionen weltweit",
           x = "Zeit", y = "Neuinfektionen") +
      theme(
        panel.background = element_rect(fill = "#ffcc99"),
        axis.title = element_text(color = "blue"),
        title = element_text(color = "blue")
      )
    gp_ae_ww
    
  })
  
  
  output$id_plot_country_select <- renderPlot({
    gp_country_select <- ggplot(df_all_cum_countries %>% filter(country %in% input$id_country_select,
                                                                datum <= input$id_date_select),
                                aes(x = datum, y = cases_country, color = country)) +
      geom_line() +
      labs(title = "Tägliche Neuinfektionen",
           x = "Zeit", y = "Neuinfektionen") +
      theme(
        panel.background = element_rect(fill = "#ffcc99"),
        axis.title = element_text(color = "blue"),
        title = element_text(color = "blue")
      )
    
    gp_country_select
    
  })
  
  
} # # ---Ende server---


shinyApp(ui, server)
