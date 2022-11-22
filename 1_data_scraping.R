library(tidyverse)
library(jsonlite)
library(httr)
library(lubridate)

model_vec <- c("707","702","1703","708","8155","1286","705",
               "6445","6125","8744","704","703","8290","7665",
               "7595","699","700","706")

output <- tibble()

for (model in model_vec){
  
  temp_output <- tibble()
  
  query <- list()
  
  model_input <- paste0("93:", model)
  
  query <- list(manufacturer_model_cb = model_input)

  resp <- GET("https://www.sauto.cz/api/v1/items/search?", query = query)
  

  data <- fromJSON(resp$request$url)
  
  max_items <- data$pagination$total
  
  query$limit <- max_items
  
  resp <- GET("https://www.sauto.cz/api/v1/items/search?", query = query)
  data <- fromJSON(resp$request$url)
  
  print(max_items)
  
  
  temp_output <- data$results %>%
    tibble()
  
  
  print(paste("temporary output:", "length", nrow(temp_output), "max_items:", max_items))
  
  
  Sys.sleep(0.25)
  
  output <- output %>% 
    bind_rows(temp_output)
  
}

data <- output %>% 
  select(name, model_cb, fuel_cb, gearbox_cb, additional_model_name,
         tachometer, price, premise, locality, in_operation_date) %>% 
  mutate(additional_model_name = tolower(additional_model_name)) %>% 
  mutate(kombi = ifelse(str_detect(.$additional_model_name, "kombi|combi"),1,0)) %>%
  mutate(additional_model_name = str_replace(additional_model_name, "(?<=\\d)\\,+(?=\\d)", "\\.")) %>% 
  mutate(objem = str_extract(additional_model_name, "[1-3]\\.\\d{1,1}")) %>%
  mutate(in_operation_date = as.Date(in_operation_date)) %>% 
  mutate(now = Sys.Date()) %>% 
  mutate(age = time_length(
    difftime(as.Date(now), in_operation_date),"years") ) %>% 
  select(price, objem, age, tachometer, fuel_cb, gearbox_cb, kombi) %>% 
  unnest() %>% 
  select(-c(value, name1, seo_name1, value1, name)) %>% 
  mutate(objem = as.numeric(objem)) %>% 
  na.omit()

# write.csv(data, "data_skoda.csv", row.names = FALSE)