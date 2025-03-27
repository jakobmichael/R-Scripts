# Benötigte Pakete laden
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# CSV-Datei aus dem Unterordner "data" importieren
df <- read_csv("data/BSc_Lab_Course_2025-02-26.csv", col_names = FALSE)

# Die erste Tabelle "LCMS" extrahieren
lcms_start <- which(df[,1] == "LCMS") + 1  # Suchen, wo die Tabelle beginnt
lcms_columns <- df[lcms_start, 2:7] |> as.character()  # Spaltennamen extrahieren
lcms_data <- df[(lcms_start+1):(lcms_start+3), 2:7]  # Werte für OF 3, OF 17, JAM 1

# Spaltennamen setzen und Umwandlung in numerische Werte
colnames(lcms_data) <- lcms_columns
lcms_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), lcms_data)
lcms_data <- lcms_data |> mutate(across(-Sample, as.numeric))

# Daten ins long format bringen
lcms_long <- pivot_longer(lcms_data, cols = -Sample, names_to = "Parameter", values_to = "Value")

# Plot erstellen mit zwei Achsenunterbrechungen
ggplot(lcms_long, aes(x = Parameter, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_break(c(1, 10), scales = "free") +  # Erste Unterbrechung für kleine Werte
  scale_y_break(c(100, 850), scales = "free") +  # Zweite Unterbrechung für große Werte
  theme_minimal() +
  labs(y = "concentration [ng/g]", x = "parameter", title = "LCMS analysis with double-axis break") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

