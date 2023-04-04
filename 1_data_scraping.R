library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(httr)
library(lubridate)

source("common.R")

manufacturer = "93"
model_vec <- c("707","702","1703","708","8155","1286","705",
               "6445","6125","8744","704","703","8290","7665",
               "7595","699","700","706")


output <- tibble()
for (model in model_vec){
  temp_output <- tibble()
  query <- makeQuery(manufacturer,
            model)
  resp <- GET("https://www.sauto.cz/api/v1/items/search?", query = query)
  data <- fromJSON(resp$request$url)
  max_items <- data$pagination$total
  remainder <- max_items%%query$limit
  offsets <- seq(0, to=max_items-remainder, by=20)
  
  # scraping each model by 20 ads
  for (o in offsets){
    temp <- tibble()
    query <- makeQuery(manufacturer,
              model = model,
              offset = o)
    resp <- GET("https://www.sauto.cz/api/v1/items/search?", query = query)
    data <- fromJSON(resp$request$url)
    # serialize data into tibble object
    temp <-  serializeData(data)
    temp_output <- bind_rows(
      temp_output, temp
    )
  }
  
  # temp_output <- object
  print(max_items)
  output <- output %>% 
    bind_rows(temp_output)
  print(paste("temporary output:", "length", nrow(temp_output), "max_items:", max_items))
  Sys.sleep(0.25)
}


data <- 
  output %>%
  mutate(additional_model_name = tolower(additional_model_name)) %>% 
  mutate(kombi = ifelse(str_detect(.$additional_model_name, "kombi|combi"),1,0)) %>%
  mutate(additional_model_name = str_replace(additional_model_name, "(?<=\\d)\\,+(?=\\d)", "\\.")) %>% 
  mutate(objem = str_extract(additional_model_name, "[1-3]\\.\\d{1,1}")) %>% 
  mutate(in_operation_date = as.Date(in_operation_date)) %>% 
  mutate(now = Sys.Date()) %>% 
  mutate(age = time_length(
    difftime(as.Date(now), in_operation_date),"years") ) %>%
  mutate(across(.cols = everything(), ~ifelse(.=="" & !is.Date(.), NA, .))) %>%
  mutate(objem = as.numeric(objem)) %>% 
  na.omit() %>% 
  mutate(index = row_number())




write.csv(data, "data_skoda.csv", row.names = FALSE)
 