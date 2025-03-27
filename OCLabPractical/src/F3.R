# Benötigte Pakete laden
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# CSV-Datei aus dem Unterordner "data" importieren
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)

# Die erste Tabelle "f3" extrahieren
f3_start <- which(df[,1] == "F3") + 2  # Suchen, wo die Tabelle beginnt
f3_columns <- df[f3_start, 2:13] |> as.character()  # Spaltennamen extrahieren
f3_data <- df[(f3_start+1):(f3_start+3), 2:13]  # Werte für OF 3, OF 17, JAM 1

# Spaltennamen setzen und Umwandlung in numerische Werte
colnames(f3_data) <- f3_columns
f3_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), f3_data)
f3_data <- f3_data |> mutate(across(-Sample, as.numeric))

# Daten ins long format bringen
f3_long <- pivot_longer(f3_data, cols = -Sample, names_to = "Parameter", values_to = "Value")

# Plot erstellen mit zwei Achsenunterbrechungen
ggplot(f3_long, aes(x = Parameter, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_break(c(200, 250), scales = "free") +  # Erste Unterbrechung für kleine Werte
  scale_y_break(c(1200, 2000), scales = "free") +  # Zweite Unterbrechung für große Werte
  theme_minimal() +
  labs(y = "concentration [ng/g]", x = "parameter", title = "F3 analysis with double-axis break") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
