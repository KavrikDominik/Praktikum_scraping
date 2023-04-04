library(tidyverse)
library(gam)
options(scipen=999)
set.seed(2222)


# Data --------------------------------------------------------------------



# data <- read_csv("data_skoda.csv")

# Test/train split --------------------------------------------------------

smp_size <- floor(0.75 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

new_x_df <- tibble(test[, -1])


# GAM estimation ----------------------------------------------------------


gam1 <- gam(
  price ~ ns(miliage, df = 5) + ns(age, df = 5) +
    model + ns(objem, df = 5),
  data = train
)

summary(gam1)



# GAM results Visualization -----------------------------------------------

DF <- preplot(gam1, se=T)

miliage <- tibble(
  x = DF$`ns(miliage, df = 5)`$x,
  y = DF$`ns(miliage, df = 5)`$y,
  se.y = DF$`ns(miliage, df = 5)`$se.y
) %>% 
  # pivot_longer(names_to = "variable",
  #              values_to = "value", 1:3) %>% 
  ggplot(aes(x, y))+
  geom_ribbon(aes( ymin = y - 1.96*se.y,
                   ymax = y + 1.96*se.y),
              alpha = 0.3, fill = "#1e81b0")+
  geom_line()+
  theme_light()+
  labs(title = "mezni efekt promenne tachometr na cenu",
       x = "tachometr",
       y = "cena")


age <- tibble(
  x = DF$`ns(age, df = 5)`$x,
  y = DF$`ns(age, df = 5)`$y,
  se.y = DF$`ns(age, df = 5)`$se.y
) %>% 
  # pivot_longer(names_to = "variable",
  #              values_to = "value", 1:3) %>% 
  ggplot(aes(x, y))+
  geom_ribbon(aes( ymin = y - 1.96*se.y,
                   ymax = y + 1.96*se.y),
              alpha = 0.3, fill = "#1e81b0")+
  geom_line()+
  theme_light()+
  labs(title = "mezni efekt promenne age na cenu",
       x = "age",
       y = "cena")


objem <- tibble(
  x = DF$`ns(objem, df = 5)`$x,
  y = DF$`ns(objem, df = 5)`$y,
  se.y = DF$`ns(objem, df = 5)`$se.y
) %>% 
  # pivot_longer(names_to = "variable",
  #              values_to = "value", 1:3) %>% 
  ggplot(aes(x, y))+
  geom_ribbon(aes( ymin = y - 1.96*se.y,
                   ymax = y + 1.96*se.y),
              alpha = 0.3, fill = "#1e81b0")+
  geom_line()+
  theme_light()+
  labs(title = "mezni efekt promenne objem na cenu",
       x = "objem",
       y = "cena")

