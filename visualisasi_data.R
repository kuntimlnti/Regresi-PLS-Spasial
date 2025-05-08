library(dplyr)
library(tidyr)
library(ggplot2)

## Visualisasi Data ----------------------------------------------------------

## Pulau Jawa
treeloss_jawa <- read.csv("D:/2024/TA/Data/treeloss_jawa.csv")
# Plot tren treeloss di setiap provinsi di pulau jawa
ggplot(treeloss_jawa, aes(x = tahun, y = y, color = provinsi, group = provinsi)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = expression("Tren "*italic("Tree Cover Loss")*" di Pulau Jawa"), 
    x = "Tahun", 
    y = expression(italic("Tree Cover Loss"))
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Hitung persentase penurunan tree cover loss
persentase_kehilangan <- treeloss_jawa %>%
  filter(tahun %in% c(2016, 2022)) %>%
  select(provinsi, tahun, y) %>%
  pivot_wider(names_from = tahun, values_from = y, names_prefix = "y_") %>%
  mutate(persen_hilang = ((y_2016 - y_2022) / y_2016) * 100)
# Visualisasi dengan bar chart
ggplot(persentase_kehilangan, aes(x = reorder(provinsi, -persen_hilang), y = persen_hilang)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = paste0(round(persen_hilang, 1), "%")), vjust = -0.3, size = 3.5) + # Menambahkan label persentase
  labs(
    title = expression("Persentase Penurunan" * italic("Tree Cover Loss") * " di Pulau Jawa (2016–2022)"),
    x = "Provinsi",
    y = expression(italic("Tree Cover Loss (%)"))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5) )

## Pulau Kalimantan
treeloss_kalimantan <- read.csv("D:/2024/TA/Data/treeloss_kalimantan.csv")
# Plot tren treeloss di setiap provinsi di pulau Kalimantan
ggplot(treeloss_kalimantan, aes(x = tahun, y = y, color = provinsi, group = provinsi)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = expression("Tren "*italic("Tree Cover Loss")*" di Pulau Kalimantan"), 
    x = "Tahun", 
    y = expression(italic("Tree Cover Loss"))
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Hitung persentase penurunan tree cover loss
persentase_kehilangan <- treeloss_kalimantan %>%
  filter(tahun %in% c(2016, 2022)) %>%
  select(provinsi, tahun, y) %>%
  pivot_wider(names_from = tahun, values_from = y, names_prefix = "y_") %>%
  mutate(persen_hilang = ((y_2016 - y_2022) / y_2016) * 100)
# Visualisasi dengan bar chart
ggplot(persentase_kehilangan, aes(x = reorder(provinsi, -persen_hilang), y = persen_hilang)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = paste0(round(persen_hilang, 1), "%")), vjust = -0.3, size = 3.5) + # Menambahkan label persentase
  labs(
    title = expression("Persentase Penurunan" * italic("Tree Cover Loss") * " di Pulau Kalimantan (2016–2022)"),
    x = "Provinsi",
    y = expression(italic("Tree Cover Loss (%)"))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5) )

## Pulau Papua dan Maluku
treeloss_papua.maluku <- read.csv("D:/2024/TA/Data/treeloss_papuamaluku.csv")
# Plot tren treeloss di setiap provinsi di pulau papua dan maluku
ggplot(treeloss_papua.maluku, aes(x = tahun, y = y, color = provinsi, group = provinsi)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = expression("Tren "*italic("Tree Cover Loss")*" di Pulau Papua dan Maluku"), 
    x = "Tahun", 
    y = expression(italic("Tree Cover Loss"))
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Hitung persentase penurunan tree cover loss
persentase_kehilangan <- treeloss_papua.maluku %>%
  filter(tahun %in% c(2016, 2022)) %>%
  select(provinsi, tahun, y) %>%
  pivot_wider(names_from = tahun, values_from = y, names_prefix = "y_") %>%
  mutate(persen_hilang = ((y_2016 - y_2022) / y_2016) * 100)
# Visualisasi dengan bar chart
ggplot(persentase_kehilangan, aes(x = reorder(provinsi, -persen_hilang), y = persen_hilang)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = paste0(round(persen_hilang, 1), "%")), vjust = -0.3, size = 3.5) + # Menambahkan label persentase
  labs(
    title = expression("Persentase Penurunan" * italic("Tree Cover Loss") * " di Pulau Papua dan Maluku (2016–2022)"),
    x = "Provinsi",
    y = expression(italic("Tree Cover Loss (%)"))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5) )

## Pulau Sulawesi
treeloss_sulawesi <- read.csv("D:/2024/TA/Data/treeloss_sulawesi.csv")
# Plot tren treeloss di setiap provinsi di pulau sulawesi
ggplot(treeloss_sulawesi, aes(x = tahun, y = y, color = provinsi, group = provinsi)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = expression("Tren "*italic("Tree Cover Loss")*" di Pulau Sulawesi"), 
    x = "Tahun", 
    y = expression(italic("Tree Cover Loss"))
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Hitung persentase penurunan tree cover loss
persentase_kehilangan <- treeloss_sulawesi %>%
  filter(tahun %in% c(2016, 2022)) %>%
  select(provinsi, tahun, y) %>%
  pivot_wider(names_from = tahun, values_from = y, names_prefix = "y_") %>%
  mutate(persen_hilang = ((y_2016 - y_2022) / y_2016) * 100)
# Visualisasi dengan bar chart
ggplot(persentase_kehilangan, aes(x = reorder(provinsi, -persen_hilang), y = persen_hilang)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = paste0(round(persen_hilang, 1), "%")), vjust = -0.3, size = 3.5) + # Menambahkan label persentase
  labs(
    title = expression("Persentase Penurunan" * italic("Tree Cover Loss") * " di Pulau Sulawesi (2016–2022)"),
    x = "Provinsi",
    y = expression(italic("Tree Cover Loss (%)"))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5) )

## Pulau Sumatra
treeloss_sumatra <- read.csv("D:/2024/TA/Data/treeloss_sumatra.csv")
# Plot tren treeloss di setiap provinsi di pulau sumatra
ggplot(treeloss_sumatra, aes(x = tahun, y = y, color = provinsi, group = provinsi)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = expression("Tren "*italic("Tree Cover Loss")*" di Pulau Sumatra"), 
    x = "Tahun", 
    y = expression(italic("Tree Cover Loss"))
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Hitung persentase penurunan tree cover loss
persentase_kehilangan <- treeloss_sumatra %>%
  filter(tahun %in% c(2016, 2022)) %>%
  select(provinsi, tahun, y) %>%
  pivot_wider(names_from = tahun, values_from = y, names_prefix = "y_") %>%
  mutate(persen_hilang = ((y_2016 - y_2022) / y_2016) * 100)
# Visualisasi dengan bar chart
ggplot(persentase_kehilangan, aes(x = reorder(provinsi, -persen_hilang), y = persen_hilang)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = paste0(round(persen_hilang, 1), "%")), vjust = -0.3, size = 3.5) + # Menambahkan label persentase
  labs(
    title = expression("Persentase Penurunan" * italic("Tree Cover Loss") * " di Pulau Sumatra (2016–2022)"),
    x = "Provinsi",
    y = expression(italic("Tree Cover Loss (%)"))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5) )
