# Benötigte Pakete laden
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# CSV-Datei aus dem Unterordner "data" importieren
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)

# Die zweite Tabelle "F1" extrahieren
f1_start <- which(df[,1] == "F1") + 2  # Suchen, wo die Tabelle beginnt
f1_columns <- df[f1_start, 2:8] |> as.character()  # Spaltennamen extrahieren
f1_data <- df[(f1_start+1):(f1_start+3), 2:8]  # Werte für OF 3, OF 17, JAM 1

# Spaltennamen setzen und Umwandlung in numerische Werte
colnames(f1_data) <- f1_columns
f1_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), f1_data)
f1_data <- f1_data |> mutate(across(-Sample, as.numeric))

# Daten ins long format bringen
f1_long <- pivot_longer(f1_data, cols = -Sample, names_to = "Parameter", values_to = "Value")

# Plot erstellen mit zwei Achsenunterbrechungen
ggplot(f1_long, aes(x = Parameter, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_break(c(20, 25), scales = "free") +  # Erste Unterbrechung für kleine Werte
  scale_y_break(c(100, 150), scales = "free") +  # Zweite Unterbrechung für kleine Werte
  scale_y_break(c(300, 1500), scales = "free") +  # Dritte Unterbrechung für große Werte
  theme_minimal() +
  labs(y = "concentration [ng/g]", x = "parameter", title = "F1 analysis with trippel-axis break") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
