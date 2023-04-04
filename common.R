makeQuery <- function(manufacturer = character(),
                      model = character(),
                      limit = 20,
                      offset=0){
  query <- list()
  model_input <- paste0(manufacturer,":", model)
  query <- list(manufacturer_model_cb = model_input,
                limit = limit,
                offset = offset)
  return(query)
}

serializeData <- function(data){
  serialized_df <- tibble(
    manufacturer = data$results$manufacturer_cb$seo_name,
    manufacturer_id = data$results$manufacturer_cb$value,
    model = data$results$model_cb$seo_name,
    model_id = data$results$model_cb$value,
    name = data$results$category$seo_name,
    additional_model_name = data$results$additional_model_name,
    price = data$results$price,
    miliage = data$results$tachomete,
    price_by_agreement = data$results$price_by_agreement,
    topped = data$results$topped,
    premise = data$results$premise$name,
    id = data$results$id,
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
  return(serialized_df)
}
