# Import knihoven
library(tidyverse)

# Vytvoření data-framu

sample_df <- data.frame(
  produkt = c("Malý šroubek", "Velký šroub", "Malá matička", "Velká matka"),
  jednotka = c("100 g", "ks", "100 g", "ks"),
  cena = c(10, 5, 8.5, 6),
  skladem = c(TRUE, TRUE, FALSE, TRUE)
)

# Import integrovaného datasetu do "iris_df"

iris_df <- data.frame(iris)

# Zobrazení dat

view(iris_df)
view(starwars)

# Zobrazení sloupců v datasetu 
glimpse(starwars)

# Odebrání sloupců
starwards_edit <- select(starwars, name: species)
glimpse (starwards_edit)

# Statistiky jednotlivých sloupců
summary (starwards_edit)

#Filtr na hodnoty ve sloupcích
view (filter(starwards_edit, hair_color == "black", is.na(mass)))

# Kombinace sloupců (výpočet nového sloupce + Tlouštík || Name)
view(mutate(starwards_edit, bmi = mass / (height / 100) ** 2, name = paste0(if_else(bmi > 30, "Tlouštík ",""), name)))

#Jiný přepis kombinace sloupců + funkce 'case_when' do nového sloupce
view(
starwards_edit<-
  starwards_edit|> 
  select(name:mass) |> 
  mutate(
    bmi = round(mass / (height / 100) ** 2),
    postava = case_when(
      bmi < 25 ~ "Hubeňour ",
      bmi >= 25 & bmi <= 30 ~ "",
      bmi > 30 ~ "Tlouštík "
    )
  )
)
view(starwards_edit)

# Grafy 

# import knihoven
library(ggplot2)

#histogram
starwards_edit |> 
  filter(postava == "Tlouštík ") |>
  ggplot(aes(x = height)) +
  geom_histogram()

#Bodový graf
starwars |> 
  ggplot(aes(x = height, y = mass)) +
  geom_point()

#Vyfiltrované vychylující hodnoty
starwars |> 
  filter(mass != max(mass, na.rm = TRUE)) |> 
  ggplot(aes(x = height, y = mass)) +
  geom_point()

#Bodový graf + Trend
starwars |> 
  filter(mass != max(mass, na.rm = TRUE)) |> 
  ggplot(aes(x = height, y = mass)) +
  geom_point() +
  geom_smooth()

#Sloupcový graf
starwars |> 
  group_by(species) |> 
  summarise(height = mean(height, na.rm = TRUE)) |> 
  slice_max(order_by = height, n = 10) |> 
  ggplot(aes(x = height, y = fct_reorder(species, height))) +
  geom_col() +
  labs(x = "výška v cm", y = "druh", title = "Průměrná výška 10 nejvyšších druhů")3



