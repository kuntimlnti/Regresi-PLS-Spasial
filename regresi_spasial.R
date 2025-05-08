library(lmtest)
library(car)
library(MASS)
library(spdep)
library(dplyr)
library(spatialreg)

# Regresi Spasial (SAR dan SEM)

## Pulau Jawa -----------------------------------------------------------------
treeloss_jawa <- read.csv("D:/2024/TA/Data/treeloss_jawa.csv")
# Tambahkan centroid (long dan lat) untuk masing-masing provinsi
centroid <- data.frame(
  provinsi = c("Banten", "DI. Yogyakarta", "Jawa Barat", "Jawa Tengah", "Jawa Timur"),
  long = c(106.06401790, 110.42620880, 107.66888700, 110.14025940, 112.23840170),  # Longitude tiap provinsi
  lat = c(-6.40581720, -7.87538490, -7.09091100, -7.15097500, -7.53606390)         # Latitude tiap provinsi
)
treeloss_jawa1 <- merge(treeloss_jawa, centroid, by = "provinsi")

# Buat matriks keterhubungan spasial (berdasarkan koordinat)
coords1 <- cbind(treeloss_jawa1$long, treeloss_jawa1$lat)
nb1 <- knearneigh(coords1, k = 2)  # Tetangga terdekat (k = 2 tetangga)
lw1 <- nb2listw(knn2nb(nb1), style = "W")  # Matriks bobot spasial
# Hitung Moran's I untuk variabel y
moran_result1 <- moran.test(treeloss_jawa1$y, lw1)
print(moran_result1)

# Multiple linear regression
mod_jawa <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data = treeloss_jawa1)
summary(mod_jawa) #x3, x9
mod_jawa1 <- lm(y~x3+x9+tahun, data = treeloss_jawa1)
summary(mod_jawa1)
AIC(mod_jawa1)

# SAR
sar_jawa <- lagsarlm(y~x3+x9+tahun, data = treeloss_jawa1, listw = lw1)
summary(sar_jawa)
Wy <- lag.listw(lw1, treeloss_jawa1$y)

# SEM
sem_jawa <- errorsarlm(y~x3+x9+tahun, data = treeloss_jawa1, listw = lw1)
summary(sem_jawa)
residual_sem_jawa <- residuals(sem_jawa)
Wu <- lag.listw(lw1, residual_sem_jawa)

# R-squared dan Adjusted R-Squared
# SAR
n_sar <- length(treeloss_jawa1$y) # Jumlah observasi
p_sar <- length(sar_jawa$coefficients) # Jumlah parameter
TSS_sar <- sum((treeloss_jawa1$y - mean(treeloss_jawa1$y))^2) # Total Sum of Squares
r2_sar <- 1 - (sar_jawa$SSE / TSS_sar) # Pseudo R-squared
adj_r2_sar <- 1 - ((1 - r2_sar) * (n_sar - 1) / (n_sar - p_sar - 1)) # Adjusted pseudo R-squared
r2_sar
adj_r2_sar
# SEM
n_sem <- length(treeloss_jawa1$y)
p_sem <- length(sem_jawa$coefficients) # Jumlah parameter
TSS_sem <- sum((treeloss_jawa1$y - mean(treeloss_jawa1$y))^2)
r2_sem <- 1 - (sem_jawa$SSE / TSS_sem)
adj_r2_sem <- 1 - ((1 - r2_sem) * (n_sem - 1) / (n_sem - p_sem - 1))
r2_sem
adj_r2_sem

## Uji asumsi klasik
# uji heteroskedastisitas
bptest(residuals(sar_jawa) ~ fitted(sar_jawa))
bptest(residuals(sem_jawa) ~ fitted(sem_jawa))
# uji autokorelasi spasial
residuals_sar <- residuals(sar_jawa)
residuals_sem <- residuals(sem_jawa)
moran.test(residuals_sar, lw1)  # Uji Moran untuk SAR
moran.test(residuals_sem, lw1)  # Uji Moran untuk SEM
# uji normalitas
ks.test(residuals_sar, "pnorm", mean = mean(residuals_sar), sd = sd(residuals_sar)) # Uji normalitas residual SAR
ks.test(residuals_sem, "pnorm", mean = mean(residuals_sem), sd = sd(residuals_sem)) # Uji normalitas residual SEM

## Pulau Kalimantan ---------------------------------------------------------------
treeloss_kalimantan <- read.csv("D:/2024/TA/Data/treeloss_kalimantan.csv")
# Tambahkan centroid (long dan lat) untuk masing-masing provinsi
centroid <- data.frame(
  provinsi = c("Kalimantan Barat", "Kalimantan Selatan", "Kalimantan Tengah", "Kalimantan Timur"),
  long = c(106.61314050, 115.28375850, 113.38235450, 116.41938900),  # Longitude tiap provinsi
  lat = c(0.47734750, -3.09264150, -1.68148780, 0.53865860)          # Latitude tiap provinsi
)
treeloss_kalimantan1 <- merge(treeloss_kalimantan, centroid, by = "provinsi")

# Buat matriks keterhubungan spasial (berdasarkan koordinat)
coords2 <- cbind(treeloss_kalimantan1$long, treeloss_kalimantan1$lat)
nb2 <- knearneigh(coords2, k = 3)  # Tetangga terdekat (k = 3 tetangga)
lw2 <- nb2listw(knn2nb(nb2), style = "W")  # Matriks bobot spasial
# Hitung Moran's I untuk variabel y
moran_result <- moran.test(treeloss_kalimantan1$y, lw2)
print(moran_result)

# Multiple linear regression
mod_kalimantan <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data = treeloss_kalimantan1)
summary(mod_kalimantan) #x2, x8
AIC(mod_kalimantan)
mod_kalimantan1 <- lm(y~x2+x8+tahun, data = treeloss_kalimantan1)
summary(mod_kalimantan1)
AIC(mod_kalimantan1)

# SAR
sar_kalimantan <- lagsarlm(y~x2+x8+tahun, data = treeloss_kalimantan1, listw = lw2)
summary(sar_kalimantan)
# SEM
sem_kalimantan <- errorsarlm(y~x2+x8+tahun, data = treeloss_kalimantan1, listw = lw2)
summary(sem_kalimantan)

# R-squared dan Adjusted R-Squared
# SAR
n_sar <- length(treeloss_kalimantan1$y) # Jumlah observasi
p_sar <- length(sar_kalimantan$coefficients) # Jumlah parameter
TSS_sar <- sum((treeloss_kalimantan1$y - mean(treeloss_kalimantan1$y))^2) # Total Sum of Squares
r2_sar <- 1 - (sar_kalimantan$SSE / TSS_sar) # Pseudo R-squared
adj_r2_sar <- 1 - ((1 - r2_sar) * (n_sar - 1) / (n_sar - p_sar - 1)) # Adjusted pseudo R-squared
r2_sar
adj_r2_sar
# SEM
n_sem <- length(treeloss_kalimantan1$y)
p_sem <- length(sem_kalimantan$coefficients) # Jumlah parameter
TSS_sem <- sum((treeloss_kalimantan1$y - mean(treeloss_kalimantan1$y))^2)
r2_sem <- 1 - (sem_kalimantan$SSE / TSS_sem)
adj_r2_sem <- 1 - ((1 - r2_sem) * (n_sem - 1) / (n_sem - p_sem - 1))
r2_sem
adj_r2_sem


## Uji asumsi klasik
# uji heteroskedastisitas
bptest(residuals(sar_kalimantan) ~ fitted(sar_kalimantan)) #p-value > 0.05 homoskedastisitas
bptest(residuals(sem_kalimantan) ~ fitted(sem_kalimantan)) #p-value < 0.05 ada heteroskedastisitas
# uji autokorelasi spasial
residuals_sar_kalimantan <- residuals(sar_kalimantan)
residuals_sem_kalimantan <- residuals(sem_kalimantan)
moran.test(residuals_sar_kalimantan, lw2)  # Uji Moran untuk SAR
moran.test(residuals_sem_kalimantan, lw2)  # Uji Moran untuk SEM
# uji normalitas
ks.test(residuals_sar_kalimantan, "pnorm", mean = mean(residuals_sar_kalimantan), sd = sd(residuals_sar_kalimantan)) # Uji normalitas residual SAR
ks.test(residuals_sem_kalimantan, "pnorm", mean = mean(residuals_sem_kalimantan), sd = sd(residuals_sem_kalimantan)) # Uji normalitas residual SEM

## Pulau Papua dan Maluku -----------------------------------------------------
treeloss_papua.maluku <- read.csv("D:/2024/TA/Data/treeloss_papuamaluku.csv")
# Tambahkan centroid (long dan lat) untuk masing-masing provinsi
centroid <- data.frame(
  provinsi = c("Maluku", "Maluku Utara", "Papua", "Papua Barat"),
  long = c(130.14527340, 127.80876930, 141.34701590, 133.17471620),  # Longitude tiap provinsi
  lat = c(-3.23846160, 1.57099930, -5.01222020, -1.33611540)         # Latitude tiap provinsi
)
treeloss_papua.maluku1 <- merge(treeloss_papua.maluku, centroid, by = "provinsi")

# Buat matriks keterhubungan spasial (berdasarkan koordinat)
coords3 <- cbind(treeloss_papua.maluku1$long, treeloss_papua.maluku1$lat)
nb3 <- knearneigh(coords3, k = 1)  # Tetangga terdekat (k = 1 tetangga)
lw3 <- nb2listw(knn2nb(nb3), style = "W")  # Matriks bobot spasial
# Hitung Moran's I untuk variabel y
moran_result <- moran.test(treeloss_papua.maluku1$y, lw3)
print(moran_result)

# Multiple linear regression
mod_papua.maluku <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data = treeloss_papua.maluku1)
summary(mod_papua.maluku) #x2, x8
AIC(mod_papua.maluku)
mod_papua.maluku1 <- lm(y~x2+x8+tahun, data = treeloss_papua.maluku1)
summary(mod_papua.maluku1) 
AIC(mod_papua.maluku1)

# SAR
sar_papua.maluku <- lagsarlm(y~x2+x8+tahun, data = treeloss_papua.maluku1, listw = lw3)
summary(sar_papua.maluku)
# SEM
sem_papua.maluku <- errorsarlm(y~x2+x8+tahun, data = treeloss_papua.maluku1, listw = lw3)
summary(sem_papua.maluku)

# R-squared dan Adjusted R-Squared
# SAR
n_sar <- length(treeloss_papua.maluku1$y) # Jumlah observasi
p_sar <- length(sar_papua.maluku$coefficients) # Jumlah parameter
TSS_sar <- sum((treeloss_papua.maluku1$y - mean(treeloss_papua.maluku1$y))^2) # Total Sum of Squares
r2_sar <- 1 - (sar_papua.maluku$SSE / TSS_sar) # Pseudo R-squared
adj_r2_sar <- 1 - ((1 - r2_sar) * (n_sar - 1) / (n_sar - p_sar - 1)) # Adjusted pseudo R-squared
r2_sar
adj_r2_sar
# SEM
n_sem <- length(treeloss_papua.maluku1$y)
p_sem <- length(sem_papua.maluku$coefficients) # Jumlah parameter
TSS_sem <- sum((treeloss_papua.maluku1$y - mean(treeloss_papua.maluku1$y))^2)
r2_sem <- 1 - (sem_papua.maluku$SSE / TSS_sem)
adj_r2_sem <- 1 - ((1 - r2_sem) * (n_sem - 1) / (n_sem - p_sem - 1))
r2_sem
adj_r2_sem

## Uji asumsi klasik
# uji heteroskedastisitas
bptest(residuals(sar_papua.maluku) ~ fitted(sar_papua.maluku)) #p-value < 0.05 ada homoskedastisitas
bptest(residuals(sem_papua.maluku) ~ fitted(sem_papua.maluku)) #p-value > 0.05 tdk ada heteroskedastisitas
# uji autokorelasi spasial
residuals_sar_papua.maluku <- residuals(sar_papua.maluku)
residuals_sem_papua.maluku <- residuals(sem_papua.maluku)
moran.test(residuals_sar_papua.maluku, lw3)  # Uji Moran untuk SAR
moran.test(residuals_sem_papua.maluku, lw3)  # Uji Moran untuk SEM
# uji normalitas
ks.test(residuals_sar_papua.maluku, "pnorm", mean = mean(residuals_sar_papua.maluku), sd = sd(residuals_sar_papua.maluku)) # Uji normalitas residual SAR
ks.test(residuals_sem_papua.maluku, "pnorm", mean = mean(residuals_sem_papua.maluku), sd = sd(residuals_sem_papua.maluku)) # Uji normalitas residual SEM

## Pulau Sulawesi --------------------------------------------------------------
treeloss_sulawesi <- read.csv("D:/2024/TA/Data/treeloss_sulawesi.csv")
# Tambahkan centroid (long dan lat) untuk masing-masing provinsi
centroid <- data.frame(
  provinsi = c("Gorontalo", "Sulawesi Barat", "Sulawesi Selatan", "Sulawesi Tengah", "Sulawesi Tenggara", "Sulawesi Utara"),
  long = c(123.05676930, 119.23207840, 119.9740534, 121.44561790, 122.17460500, 123.97500180),  # Longitude tiap provinsi
  lat = c(0.54354420, -2.84413710, -3.66879940, -1.43002540, -4.14491000, 0.62469320)           # Latitude tiap provinsi
)
treeloss_sulawesi1 <- merge(treeloss_sulawesi, centroid, by = "provinsi")

# Buat matriks keterhubungan spasial (berdasarkan koordinat)
coords4 <- cbind(treeloss_sulawesi1$long, treeloss_sulawesi1$lat)
nb4 <- knearneigh(coords4, k = 2)  # Tetangga terdekat (k = 2 tetangga)
lw4 <- nb2listw(knn2nb(nb4), style = "W")  # Matriks bobot spasial
# Hitung Moran's I untuk variabel y
moran_result <- moran.test(treeloss_sulawesi1$y, lw4)
print(moran_result)

# Multiple linear regression
mod_sulawesi <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data = treeloss_sulawesi1)
summary(mod_sulawesi) #x1,x2,x3,x5,x7
AIC(mod_sulawesi)
mod_sulawesi1 <- lm(y~x1+x2+x3+x5+x7+tahun, data = treeloss_sulawesi1)
summary(mod_sulawesi1)
AIC(mod_sulawesi1)

# SAR
sar_sulawesi <- lagsarlm(y~x1+x2+x3+x5+x7+tahun, data = treeloss_sulawesi1, listw = lw4)
summary(sar_sulawesi)
# SEM
sem_sulawesi <- errorsarlm(y~x1+x2+x3+x5+x7+tahun, data = treeloss_sulawesi1, listw = lw4)
summary(sem_sulawesi)

# R-squared dan Adjusted R-Squared
# SAR
n_sar <- length(treeloss_sulawesi1$y) # Jumlah observasi
p_sar <- length(sar_sulawesi$coefficients) # Jumlah parameter
TSS_sar <- sum((treeloss_sulawesi1$y - mean(treeloss_sulawesi1$y))^2) # Total Sum of Squares
r2_sar <- 1 - (sar_sulawesi$SSE / TSS_sar) # Pseudo R-squared
adj_r2_sar <- 1 - ((1 - r2_sar) * (n_sar - 1) / (n_sar - p_sar - 1)) # Adjusted pseudo R-squared
r2_sar
adj_r2_sar
# SEM
n_sem <- length(treeloss_sulawesi1$y)
p_sem <- length(sem_sulawesi$coefficients) # Jumlah parameter
TSS_sem <- sum((treeloss_sulawesi1$y - mean(treeloss_sulawesi1$y))^2)
r2_sem <- 1 - (sem_sulawesi$SSE / TSS_sem)
adj_r2_sem <- 1 - ((1 - r2_sem) * (n_sem - 1) / (n_sem - p_sem - 1))
r2_sem
adj_r2_sem

## Uji asumsi klasik
# uji heteroskedastisitas
bptest(residuals(sar_sulawesi) ~ fitted(sar_sulawesi)) #p-value < 0.05 ada heteroskedastisitas
bptest(residuals(sem_sulawesi) ~ fitted(sem_sulawesi)) #p-value < 0.05 ada heteroskedastisitas
# uji autokorelasi spasial
residuals_sar_sulawesi <- residuals(sar_sulawesi)
residuals_sem_sulawesi <- residuals(sem_sulawesi)
moran.test(residuals_sar_sulawesi, lw4)  # Uji Moran untuk SAR
moran.test(residuals_sem_sulawesi, lw4)  # Uji Moran untuk SEM
# uji normalitas
ks.test(residuals_sar_sulawesi, "pnorm", mean = mean(residuals_sar_sulawesi), sd = sd(residuals_sar_sulawesi)) # Uji normalitas residual SAR
ks.test(residuals_sem_sulawesi, "pnorm", mean = mean(residuals_sem_sulawesi), sd = sd(residuals_sem_sulawesi)) # Uji normalitas residual SEM

## Pulau Sumatra --------------------------------------------------------------
treeloss_sumatra <- read.csv("D:/2024/TA/Data/treeloss_sumatra.csv")
# Tambahkan centroid (long dan lat) untuk masing-masing provinsi
centroid <- data.frame(
  provinsi = c("Aceh", "Bengkulu", "Jambi", "Bangka Belitung", "Kepulauan Riau",
               "Lampung", "Riau", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"),
  long = c(96.74939930, 102.26076410, 103.61312030, 106.44058720, 108.14286690,
           105.40680790, 101.70682940, 100.80000510, 103.91439900, 99.54509740),  # Longitude tiap provinsi
  lat = c(4.69513500, -3.79284510, -1.61012290, -2.74105130, 3.94565140, -4.55858490,
          0.29334690, -0.73993970, -3.31943740, 2.11535470)          # Latitude tiap provinsi
)
treeloss_sumatra1 <- merge(treeloss_sumatra, centroid, by = "provinsi")

# Buat matriks keterhubungan spasial (berdasarkan koordinat)
coords5 <- cbind(treeloss_sumatra1$long, treeloss_sumatra1$lat)
nb5 <- knearneigh(coords5, k = 4)  # Tetangga terdekat (k = 4 tetangga)
lw5 <- nb2listw(knn2nb(nb5), style = "W")  # Matriks bobot spasial
# Hitung Moran's I untuk variabel y
moran_result <- moran.test(treeloss_sumatra1$y, lw5)
print(moran_result)

# Multiple linear regression
mod_sumatra <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data = treeloss_sumatra1)
summary(mod_sumatra) #x1,x3,x10
AIC(mod_sumatra)
mod_sumatra1 <- lm(y~x1+x3+x10+tahun, data = treeloss_sumatra1)
summary(mod_sumatra1) 
AIC(mod_sumatra1)

# SAR
sar_sumatra <- lagsarlm(y~x1+x3+x10+tahun, data = treeloss_sumatra1, listw = lw5)
summary(sar_sumatra)
# SEM
sem_sumatra <- errorsarlm(y~x1+x3+x10+tahun, data = treeloss_sumatra1, listw = lw5)
summary(sem_sumatra)

# R-squared dan Adjusted R-Squared
# SAR
n_sar <- length(treeloss_sumatra1$y) # Jumlah observasi
p_sar <- length(sar_sumatra$coefficients) # Jumlah parameter
TSS_sar <- sum((treeloss_sumatra1$y - mean(treeloss_sumatra1$y))^2) # Total Sum of Squares
r2_sar <- 1 - (sar_sumatra$SSE / TSS_sar) # Pseudo R-squared
adj_r2_sar <- 1 - ((1 - r2_sar) * (n_sar - 1) / (n_sar - p_sar - 1)) # Adjusted pseudo R-squared
r2_sar
adj_r2_sar
# SEM
n_sem <- length(treeloss_sumatra1$y)
p_sem <- length(sem_sumatra$coefficients) # Jumlah parameter
TSS_sem <- sum((treeloss_sumatra1$y - mean(treeloss_sumatra1$y))^2)
r2_sem <- 1 - (sem_sumatra$SSE / TSS_sem)
adj_r2_sem <- 1 - ((1 - r2_sem) * (n_sem - 1) / (n_sem - p_sem - 1))
r2_sem
adj_r2_sem

## Uji asumsi klasik
# uji heteroskedastisitas
bptest(residuals(sar_sumatra) ~ fitted(sar_sumatra)) #p-value > 0.05 tdk ada heteroskedastisitas
bptest(residuals(sem_sumatra) ~ fitted(sem_sumatra)) #p-value > 0.05 tdk ada heteroskedastisitas
# uji autokorelasi spasial
residuals_sar_sumatra <- residuals(sar_sumatra)
residuals_sem_sumatra <- residuals(sem_sumatra)
moran.test(residuals_sar_sumatra, lw5)  # Uji Moran untuk SAR
moran.test(residuals_sem_sumatra, lw5)  # Uji Moran untuk SEM
# uji normalitas
ks.test(residuals_sar_sumatra, "pnorm", mean = mean(residuals_sar_sumatra), sd = sd(residuals_sar_sumatra)) # Uji normalitas residual SAR
ks.test(residuals_sem_sumatra, "pnorm", mean = mean(residuals_sem_sumatra), sd = sd(residuals_sem_sumatra)) # Uji normalitas residual SEM
# Data tidak normal

## Transformasi logaritmik pada variabel y
treeloss_sumatra1$log_y <- log(treeloss_sumatra1$y)
# Hitung Moran's I untuk variabel log_y
moran_result1 <- moran.test(treeloss_sumatra1$log_y, lw5)
print(moran_result1)

# SAR setelah transformasi
sar_tsumatra <- lagsarlm(log_y ~ x1+x3+x10+tahun, data = treeloss_sumatra1, listw = lw5)
summary(sar_tsumatra)
# SEM setelah transformasi
sem_tsumatra <- errorsarlm(log_y ~ x1+x3+x10+tahun, data = treeloss_sumatra1, listw = lw5)
summary(sem_tsumatra)

# R-squared dan Adjusted R-Squared untuk SAR (log_y)
n_sar_log <- length(treeloss_sumatra1$log_y)  # Jumlah observasi
p_sar_log <- length(sar_tsumatra$coefficients)  # Jumlah parameter
TSS_sar_log <- sum((treeloss_sumatra1$log_y - mean(treeloss_sumatra1$log_y))^2)  # Total Sum of Squares
r2_sar_log <- 1 - (sar_tsumatra$SSE / TSS_sar_log)  # Pseudo R-squared
adj_r2_sar_log <- 1 - ((1 - r2_sar_log) * (n_sar_log - 1) / (n_sar_log - p_sar_log - 1))  # Adjusted pseudo R-squared
r2_sar_log
adj_r2_sar_log

# R-squared dan Adjusted R-Squared untuk SEM (log_y)
n_sem_log <- length(treeloss_sumatra1$log_y)
p_sem_log <- length(sem_tsumatra$coefficients)
TSS_sem_log <- sum((treeloss_sumatra1$log_y - mean(treeloss_sumatra1$log_y))^2)
r2_sem_log <- 1 - (sem_tsumatra$SSE / TSS_sem_log)
adj_r2_sem_log <- 1 - ((1 - r2_sem_log) * (n_sem_log - 1) / (n_sem_log - p_sem_log - 1))
r2_sem_log
adj_r2_sem_log

## Uji asumsi klasik
#uji heteroskedastisitas
bptest(residuals(sar_tsumatra) ~ fitted(sar_tsumatra)) #p-value > 0.05 tdk ada heteroskedastisitas
bptest(residuals(sem_tsumatra) ~ fitted(sem_tsumatra)) #p-value > 0.05 tdk ada heteroskedastisitas
# uji autokorelasi spasial
library(spdep)
residuals_sar_tsumatra <- residuals(sar_tsumatra)
residuals_sem_tsumatra <- residuals(sem_tsumatra)
moran.test(residuals_sar_tsumatra, lw5)  # Uji Moran untuk SAR
moran.test(residuals_sem_tsumatra, lw5)  # Uji Moran untuk SEM
# uji normalitas
shapiro.test(residuals_sar_tsumatra)  # Uji normalitas residual SAR #normal
shapiro.test(residuals_sem_tsumatra)  # Uji normalitas residual SEM #normal