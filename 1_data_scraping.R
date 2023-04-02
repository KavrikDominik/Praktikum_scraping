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
  
  query$limit <- 20
  
  remainder <- max_items%%query$limit
  
  offsets <- seq(0, to=max_items-remainder, by=20)
  # object <- tibble()
  
  # scraping each model by 20 ads
  for (o in offsets){
    temp <- tibble()
    query$offset <- o 

    resp <- GET("https://www.sauto.cz/api/v1/items/search?", query = query)
    data <- fromJSON(resp$request$url)
    # serialize data into tibble object
    temp <-  tibble(
      manufacturer = data$results$manufacturer_cb$seo_name,
      manufacturer_id = data$results$manufacturer_cb$value,
      model = data$results$model_cb$seo_name,
      model_id = data$results$model_cb$value,
      name = data$results$category$seo_name,
      price = data$results$price,
      miliage = data$results$tachomete,
      price_by_agreement = data$results$price_by_agreement,
      topped = data$results$topped,
      premise = data$results$premise$name,
      id = data$results$id,
      additional = data$results$additional_model_name,
      fuel = data$results$fuel_cb$s,
      fuel_id = data$results$fuel_cb$value,
      gearbox = data$results$gearbox_cb$seo_name,
      gearbox_id = data$results$gearbox_cb$value,
      create_date = data$results$create_date,
      custom_id = data$results$custom_id,
      in_operation_date = data$results$in_operation_date,
      manufacturing_date = data$results$manufacturing_date,
      deal_type = data$results$deal_type
    )
    
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
# data <- 
  output %>% 
  select(name, model_cb, fuel_cb, gearbox_cb, additional_model_name,
         tachometer, price, premise, locality, in_operation_date, images) %>% 
  mutate(img_url=map(images, .f = ~lapply(.x, '[[', 1))) %>% 
  mutate(img_url=map(img_url, .f = ~unlist(.x))) %>% unnest(img_url) %>%
  mutate(img_url = paste0("http:",img_url, suffix)) %>% 
  mutate(additional_model_name = tolower(additional_model_name)) %>% 
  mutate(kombi = ifelse(str_detect(.$additional_model_name, "kombi|combi"),1,0)) %>%
  mutate(additional_model_name = str_replace(additional_model_name, "(?<=\\d)\\,+(?=\\d)", "\\.")) %>% 
  mutate(objem = str_extract(additional_model_name, "[1-3]\\.\\d{1,1}")) %>%
  mutate(in_operation_date = as.Date(in_operation_date)) %>% 
  mutate(now = Sys.Date()) %>% 
  mutate(age = time_length(
    difftime(as.Date(now), in_operation_date),"years") ) %>% 
  select(price,model_cb, objem, age, tachometer, fuel_cb, gearbox_cb, kombi, locality,img_url) %>% 
  unnest() %>% 
  mutate(across(.cols = everything(), ~ifelse(.=="", NA, .))) %>%
  select(-c(value, name1, seo_name1, value1, name, housenumber, municipality,  
            municipality_id, entity_type,source, street, street_id, streetnumber,
            quarter, quarter_id, municipality_seo_name, ward_id, ward, citypart,
            country_id,address ,address_id, zip, region, region_id, region_seo_name,name2, seo_name2)) %>% 
  mutate(objem = as.numeric(objem)) %>% 
  na.omit() %>% 
  mutate(index = row_number())

model_names <- output %>% 
  select(name, model_cb, fuel_cb, gearbox_cb, additional_model_name,
         tachometer, price, premise, locality, in_operation_date, images) %>% 
  mutate(img_url=map(images, .f = ~lapply(.x, '[[', 1))) %>% 
  mutate(img_url=map(img_url, .f = ~unlist(.x))) %>% unnest(img_url) %>%
  mutate(img_url = paste0("http:",img_url, suffix)) %>% 
  mutate(additional_model_name = tolower(additional_model_name)) %>% 
  mutate(kombi = ifelse(str_detect(.$additional_model_name, "kombi|combi"),1,0)) %>%
  mutate(additional_model_name = str_replace(additional_model_name, "(?<=\\d)\\,+(?=\\d)", "\\.")) %>% 
  mutate(objem = str_extract(additional_model_name, "[1-3]\\.\\d{1,1}")) %>%
  mutate(in_operation_date = as.Date(in_operation_date)) %>% 
  mutate(now = Sys.Date()) %>% 
  mutate(age = time_length(
    difftime(as.Date(now), in_operation_date),"years") ) %>% 
  select(model_cb, img_url) %>% unnest() %>% rename(model = seo_name) %>%  select(model, img_url) %>%  na.omit()


data <- data %>% 
  left_join(model_names, by = "img_url") %>% 
  select(model, 1:12)



write.csv(data, "data_skoda.csv", row.names = FALSE)
 