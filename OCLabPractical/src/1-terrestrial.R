# Benötigte Pakete laden
library(ggplot2)
library(ggbreak)
library(readr)
library(dplyr)
library(tidyr)

# CSV-Datei aus dem Unterordner "data" importieren
df <- read_csv("data/PlotsInGroups.csv", col_names = FALSE)

# Die erste Tabelle "1 Terrestrial" extrahieren
terrestrial_start <- which(df[,1] == "1 Terrestrial") + 1  # Suchen, wo die Tabelle beginnt
terrestrial_columns <- df[terrestrial_start, 2:27] |> as.character()  # Spaltennamen extrahieren
terrestrial_data <- df[(terrestrial_start+1):(terrestrial_start+3), 2:27]  # Werte für OF 3, OF 17, JAM 1

# Spaltennamen setzen und Umwandlung in numerische Werte
colnames(terrestrial_data) <- terrestrial_columns
terrestrial_data <- cbind(Sample = c("OF 3", "OF 17", "JAM 1"), terrestrial_data)
terrestrial_data <- terrestrial_data |> mutate(across(-Sample, as.numeric))

# Daten ins long format bringen
terrestrial_long <- pivot_longer(terrestrial_data, cols = -Sample, names_to = "Parameter", values_to = "Value")

# Plot erstellen mit zwei Achsenunterbrechungen
ggplot(terrestrial_long, aes(x = Parameter, y = Value, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  #scale_y_break(c(20, 25), scales = "free") +  # Erste Unterbrechung für kleine Werte
  #scale_y_break(c(100, 150), scales = "free") +  # Zweite Unterbrechung für kleine Werte
  #scale_y_break(c(300, 1500), scales = "free") +  # Dritte Unterbrechung für große Werte
  theme_minimal() +
  labs(y = "concentration [ng/g]", x = "parameter", title = "terrestrial analysis with trippel-axis break") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
