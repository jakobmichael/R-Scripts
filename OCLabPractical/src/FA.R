# Benötigte Pakete laden
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# CSV-Datei aus dem Unterordner "data" importieren
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)

# Die erste Tabelle "fa" extrahieren
fa_start <- which(df[,1] == "FA") + 2  # Suchen, wo die Tabelle beginnt
fa_columns <- df[fa_start, 2:15] |> as.character()  # Spaltennamen extrahieren
fa_data <- df[(fa_start+1):(fa_start+3), 2:15]  # Werte für OF 3, OF 17, JAM 1

# Spaltennamen setzen und Umwandlung in numerische Werte
colnames(fa_data) <- fa_columns
fa_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), fa_data)
fa_data <- fa_data |> mutate(across(-Sample, as.numeric))

# Daten ins long format bringen
fa_long <- pivot_longer(fa_data, cols = -Sample, names_to = "Parameter", values_to = "Value")

# Plot erstellen mit zwei Achsenunterbrechungen
ggplot(fa_long, aes(x = Parameter, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_break(c(600, 800), scales = "free") +  # Erste Unterbrechung für kleine Werte
  scale_y_break(c(4700, 6000), scales = "free") +  # Zweite Unterbrechung für große Werte
  theme_minimal() +
  labs(y = "concentration [ng/g]", x = "parameter", title = "FA analysis with double-axis break") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
