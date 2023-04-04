
histogramy <- data %>%
  select(c(model, price)) %>%
  pivot_longer(
    names_to = "variable",
    values_to = "values", 2:ncol(.)
  ) %>%
  ggplot(aes(x = values)) +
  geom_histogram(aes(y = ..density..), 
                 fill = "#1e81b0",
                 color = "white") +
  geom_density() +
  facet_wrap(~variable, scales = "free") +
  theme_light()

histogramy

sloupce <- data %>%
  select(model, fuel) %>%
  mutate(fuel = factor(fuel, levels = c(
    "benzin", "nafta", "cng-benzin",
    "hybridni", "lpg-benzin", "elektro"
  ))) %>%
  ggplot(aes(fuel, fill = fuel)) +
  geom_bar(fill = "#1e81b0") +
  theme_light()+
  facet_wrap(~model, scales = "free")

sloupce
