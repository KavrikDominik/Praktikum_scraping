
histogramy <- data %>%
  select(-c(seo_name, kombi)) %>%
  pivot_longer(
    names_to = "variable",
    values_to = "values", 1:ncol(.)
  ) %>%
  ggplot(aes(x = values)) +
  geom_histogram(aes(y = ..density..), 
                 fill = "#1e81b0",
                 color = "white") +
  geom_density() +
  facet_wrap(~variable, scales = "free") +
  theme_light()


sloupce <- data %>%
  select(seo_name) %>%
  mutate(seo_name = factor(seo_name, levels = c(
    "benzin", "nafta", "cng-benzin",
    "hybridni", "lpg-benzin", "elektro"
  ))) %>%
  ggplot(aes(seo_name)) +
  geom_bar(fill = "#1e81b0") +
  theme_light()