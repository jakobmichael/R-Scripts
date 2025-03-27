# Benötigte Pakete laden
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# CSV-Datei aus dem Unterordner "data" importieren
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)

# Die erste Tabelle "f2" extrahieren
f2_start <- which(df[,1] == "F2") + 2  # Suchen, wo die Tabelle beginnt
f2_columns <- df[f2_start, 2:7] |> as.character()  # Spaltennamen extrahieren
f2_data <- df[(f2_start+1):(f2_start+3), 2:7]  # Werte für OF 3, OF 17, JAM 1

# Spaltennamen setzen und Umwandlung in numerische Werte
colnames(f2_data) <- f2_columns
f2_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), f2_data)
f2_data <- f2_data |> mutate(across(-Sample, as.numeric))

# Daten ins long format bringen
f2_long <- pivot_longer(f2_data, cols = -Sample, names_to = "Parameter", values_to = "Value")

# Plot erstellen mit zwei Achsenunterbrechungen
ggplot(f2_long, aes(x = Parameter, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_break(c(10, 50), scales = "free") +  # Erste Unterbrechung für kleine Werte
  scale_y_break(c(150, 500), scales = "free") +  # Zweite Unterbrechung für große Werte
  scale_y_break(c(1200, 5000), scales = "free") +  # Dritte Unterbrechung für große Werte
  theme_minimal() +
  labs(y = "concentration [ng/g]", x = "parameter", title = "F2 analysis with trippel-axis break") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
