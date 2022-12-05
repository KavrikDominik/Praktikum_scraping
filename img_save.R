data <- read.csv("data_skoda.csv")

data[205:207,]

suffix <- "?fl=exf|crr,1.33333,0|res,1024,768,1|wrm,/watermark/sauto.png,10,10|jpg,80,,1"


40

output$images %>% 
sapply("[[", 2) 


img_urls <- data %>% 
  select(img_url, index)

download_jpeg <- function(url, dest_file, folder,index, name=as.character()){
  
  if (!dir.exists(folder)){
    
    dir.create(folder)
  }
  if (is.null(index)){
  order = length(list.files(folder))+1
  }
  order <- index
  filename = paste0(order,"_",name)
  destination_file <- paste0(file.path(folder),"/",filename,".jpeg")
  
  download.file(paste(url),
                destfile = destination_file, mode = 'wb', quiet = TRUE)
  
}
library(progress)
for (i in 1:nrow(img_urls)){
  
  img_url <- img_urls[i,1]
  index <- img_urls[i,2]
  download_jpeg(url = img_url,
                index = index,
                folder = "skoda_photos",
                name = "photo_skoda")
  Sys.sleep(.1)
  
}

download_jpeg(url = "http://d19-a.sdn.cz/d_19/c_img_QQ_Eh/toeTWQ.jpeg?fl=exf|crr,1.33333,0|res,1024,768,1|wrm,/watermark/sauto.png,10,10|jpg,80,,1",
              folder = "folder",
              name = "name")

download.file("http://d19-a.sdn.cz/d_19/c_img_QO_Eo/RIeTci.jpeg?fl=exf|crr,1.33333,0|res,1024,768,1|wrm,/watermark/sauto.png,10,10|jpg,80,,1",
              destfile = "photo.jpeg", mode = 'wb')
