n_iter = nrow(img_urls)

for (i in 13592:n_iter){
    
  img_url <- img_urls[i,1]
  index <- img_urls[i,2]
  download_jpeg(url = img_url,
                index = index,
                folder = "skoda_photos",
                name = "photo_skoda")
  print(i)
  Sys.sleep(0.1)

}

data[13591,] %>% View()
