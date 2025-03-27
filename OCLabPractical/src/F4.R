# Benötigte Pakete laden
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# CSV-Datei aus dem Unterordner "data" importieren
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)

# Die erste Tabelle "f4" extrahieren
f4_start <- which(df[,1] == "F4") + 2  # Suchen, wo die Tabelle beginnt
f4_columns <- df[f4_start, 2:15] |> as.character()  # Spaltennamen extrahieren
f4_data <- df[(f4_start+1):(f4_start+3), 2:15]  # Werte für OF 3, OF 17, JAM 1

# Spaltennamen setzen und Umwandlung in numerische Werte
colnames(f4_data) <- f4_columns
f4_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), f4_data)
f4_data <- f4_data |> mutate(across(-Sample, as.numeric))

# Daten ins long format bringen
f4_long <- pivot_longer(f4_data, cols = -Sample, names_to = "Parameter", values_to = "Value")

# Plot erstellen mit zwei Achsenunterbrechungen
ggplot(f4_long, aes(x = Parameter, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_break(c(300, 400), scales = "free") +  # Erste Unterbrechung für kleine Werte
  scale_y_break(c(1500, 1600), scales = "free") +  # Zweite Unterbrechung für große Werte
  scale_y_break(c(3000, 3500), scales = "free") +  # Dritte Unterbrechung für große Werte
  theme_minimal() +
  labs(y = "concentration [ng/g]", x = "parameter", title = "F4 analysis with trippel-axis break") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
