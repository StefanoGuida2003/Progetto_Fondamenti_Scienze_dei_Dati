library(dplyr)
library(magrittr)
library(ggplot2)
library(ggbeeswarm)
library(reshape2)
dataset <- read.csv2("MyGames.csv")

freq_console <- sort(table(dataset$Console), decreasing = TRUE)

graph1 <- barplot(freq_console, col = c("green", "purple", "blue", "orange", "lightblue", "grey"), 
                  main = "Videogiochi per console", 
                  xlab = "Console", ylab = "Frequenza", 
                  names.arg = names(freq_console))

videogiochi_2009_2017 <- dataset %>% filter(Anno_Di_Gioco >= 2009 & Anno_Di_Gioco < 2018)
videogiochi_2018_2024 <- dataset %>% filter(Anno_Di_Gioco >= 2018 & Anno_Di_Gioco <= 2024)

videogiochi_2009_2017$Periodo <- "2009-2017"
videogiochi_2018_2024$Periodo <- "2018-2024"

videogiochi_uniti <- rbind(videogiochi_2009_2017, videogiochi_2018_2024)

levels <- unique(videogiochi_uniti$Periodo)
colors <- c("2009-2017" = "lightblue", "2018-2024" = "orange")

graph2 <- ggplot(videogiochi_uniti, aes(x = Periodo, y = Voto_Personale, fill = Periodo)) +
          geom_boxplot() +
          labs(title = "Confronto dei Voti Personali tra i Periodi 2009-2017 e 2018-2024",
                x = "Periodo",
                y = "Voto Personale") +
          theme_minimal() +
          scale_fill_manual(values = colors, breaks = levels) +
          guides(fill=FALSE)

graph3 <- ggplot(dataset, aes(x = as.factor(1), y = Voto_Personale)) +
          geom_beeswarm(aes(color = Voto_Critica), alpha = 1) +
          labs(x = "", y = "Voto Personale", title = "Bee Swarm Plot dei Voti Personali e Voti Critica") +
          scale_color_gradient2(low = "red", mid = "blue", high = "green", midpoint = 6.5) +
          theme_minimal()

videogames <- dataset %>%
  mutate(Vendite_Globali_Milioni = Vendite_Globali * 10^-3)

dataset_filtrato <- subset(videogames, Vendite_Globali_Milioni >= 0 & Vendite_Globali_Milioni <= 30)

dataset_filtrato$Categoria <- cut(dataset_filtrato$Vendite_Globali_Milioni, 
                                  breaks = c(0, 2, 13, 30), 
                                  labels = c("Basso", "Medio", "Alto"))

# Crea il grafico a dispersione con ggplot2
graph4 <- ggplot(data = dataset_filtrato, aes(x = Voto_Personale, y = Vendite_Globali_Milioni, color = Categoria)) +
            geom_point(size = 3) +
            labs(title = "Grafico a dispersione di Vendite Globali e Voto Personale",
              x = "Voto Personale",
              y = "Vendite Globali (Milioni)") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_color_brewer(palette = "Set1")

dataset_filtrato2 <- subset(videogames, Vendite_Globali_Milioni >= 0 & Vendite_Globali_Milioni <= 400)

dataset_filtrato2$Categoria <- cut(dataset_filtrato2$Vendite_Globali_Milioni, 
                                  breaks = c(0, 2, 13, 30, 400), 
                                  labels = c("Basso", "Medio", "Alto", "Top"))

ggplot(data = dataset_filtrato2, aes(x = Voto_Personale, y = Vendite_Globali_Milioni, color = Categoria)) +
  geom_point(size = 3) +
  labs(title = "Grafico a dispersione di Vendite Globali e Voto Personale",
       x = "Voto Personale",
       y = "Vendite Globali (Milioni)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Set1")

