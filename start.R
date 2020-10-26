# # Packages
packages <- c("dplyr", "stringr", "data.table", "DT", "fs",
              "lubridate", "ggplot2", "plotly",
              "rmarkdown", "rpivotTable")

lapply(packages, require, character.only = TRUE)


# # Datenimport
dataset <- fread("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
#write.table(dataset, paste0(getwd(),"/data/dataset.csv"), sep = ";", row.names = FALSE)


# # Tidy Dataset
data <- dataset %>% 
  transmute(datum = dmy(dateRep),
            day, month, year,
            cases, deaths,
            country = countriesAndTerritories,
            population = popData2019,
            continent = continentExp)


# # Kontinent- und Länderlisten erstellen
df_list_continent <- as.data.frame(unique(data$continent))
names(df_list_continent) = c("Continet")
#write.table(df_list_continent, paste0(getwd(),"/data/continent.csv"), sep = ";", row.names = FALSE)

df_list_country <- as.data.frame(unique(data$country))
names(df_list_country) = c("Country")
#write.table(df_list_country, paste0(getwd(),"/data/country.csv"), sep = ";", row.names = FALSE)


# # Kumulierte Werte für das Tagesdatum ermitteln
df_time <- data %>% 
  arrange(datum) %>% 
  group_by(datum) %>% 
  summarise(day_cases_ww = sum(cases),
            day_deaths_ww = sum(deaths)) %>% 
  mutate(cum_day_cases_ww = cumsum(day_cases_ww),
         cum_day_deaths_ww = cumsum(day_deaths_ww),
         growth_rate_cases_ww = round((1 - (lag(day_cases_ww) / day_cases_ww)) * 100, 2),
         growth_rate_deaths_ww = round((1 - (lag(day_deaths_ww) / day_deaths_ww)) * 100, 2)) %>% 
  arrange(desc(datum))


fun_cum_country <- function(x, y){
  # # Die Funktion ermittelt zu jedem Land und für
  # # jedes Tagesdatum die kumulierten Werte 
  # # der Neuinfektionen und Todeszahlen.
  # # x ist das bereinigte Dataset data und
  # # y ist das einzelne Land.
  
  df = x %>% 
    filter(country == y) %>% 
    arrange(datum) %>% 
    group_by(country, datum) %>% 
    summarise(cases_country = sum(cases),
              deaths_country = sum(deaths)) %>% 
  ungroup() %>% 
    mutate(cum_cases = cumsum(cases_country),
           cum_deaths = cumsum(deaths_country))
  
  return(df)
  
}


# # Mit der Funktion fun_cum_country werden zu allen
# # Ländern und zu allen Tagen die kumulierten Werte ermittelt
# # und zu einem Dataframe zusammen gesetzt
df_all_cum_countries <- fun_cum_country(data, "Afghanistan")
for(i in 1 : nrow(df_list_country)){
  y = df_list_country[i,]
  #print(y)
  df_current <- fun_cum_country(data,y)
  df_all_cum_countries <- bind_rows(df_all_cum_countries, df_current)
  
}

df_all_cum_countries <- df_all_cum_countries %>% 
  mutate(growth_rate_cases_country = round((1 - (lag(cases_country) / cases_country)) * 100, 2),
         growth_rate_deaths_country = round((1 - (lag(deaths_country) / deaths_country)) * 100, 2)) %>% 
  arrange(desc(datum))

# # Zusammenfügen der kumulierten Länderwerte mit den kumulierten Tageswerten
data_all_cum <- left_join(df_all_cum_countries, df_time, by = "datum") %>% 
  arrange(desc(datum))
  
df_germany <- df_all_cum_countries %>% 
  filter(country == "Germany")

