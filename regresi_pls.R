library(lmtest)
library(car)
library(pls)
library(randomForest)
library(MLmetrics)

# Regresi Linier Berganda dan Regresi PLS 

## Pulau Jawa ---------------------------------------------------------------
## Regresi Linier Berganda
treeloss_jawa <- read.csv("D:/2024/TA/Data/treeloss_jawa.csv")
model1 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data=treeloss_jawa)
summary(model1)
AIC(model1)
residual1 <- model1$residuals

## Uji Asumsi Klasik
# Uji normalitas residual
ks_test_result <- ks.test(residual1, "pnorm", mean = mean(residual1), sd = sd(residual1))
print(ks_test_result)
# Uji autokorelasi
dwtest(model1)
# Uji heteroskedastisitas
bptest(model1, studentize = FALSE)
# Uji multikolinearitas
vif(model1)

## Regresi PLS
# Buat model PLS
model_pls <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                  data = treeloss_jawa,
                  scale = TRUE, #buat standarisasi data
                  validation = "LOO")  # Leave-One-Out cross-validation
summary(model_pls)
coef(model_pls)
# Loading matriks
loading_matrix <- loadings(model_pls)
print(loading_matrix)
# Scores = nilai komponen PLS
scores_matrix <- scores(model_pls)
print(scores_matrix)
# Menyimpan nilai komponen saja ke dalam variabel baru
komponen_pls <- data.frame(unclass(scores(model_pls)))
# Buat data frame gabungan dengan y dan scores
data_plsjawa <- data.frame(y = treeloss_jawa$y, komponen_pls)
# Regresi y terhadap semua komponen PLS
reg_plsjawa <- lm(y ~ ., data = data_plsjawa)
summary(reg_plsjawa) #yang signifikan komponen 1 sampai 4
# Regresi y terhadap 4 komponen PLS
reg_plsjawa1 <- lm(y ~ Comp.1+Comp.2+Comp.3+Comp.4, data = data_plsjawa)
summary(reg_plsjawa1)
vif(reg_plsjawa1)
AIC(reg_plsjawa1)
# Buat ulang model PLS dengan 4 komponen
model_pls_final <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                        data = treeloss_jawa,
                        scale = TRUE,
                        ncomp = 4)

## Uji asumsi klasik regresi pls
residuals_pls <- residuals(model_pls_final)
# Uji normalitas
ks.test(residuals_pls, "pnorm", mean = mean(residuals_pls), sd = sd(residuals_pls))
# Uji autokorelasi
dwtest(model_pls_final)
# Uji heteroskedastisitas
bptest(model_pls_final, studentize = FALSE)

## Pulau Kalimantan -----------------------------------------------------------
## Regresi Linier Berganda
treeloss_kalimantan <- read.csv("D:/2024/TA/Data/treeloss_kalimantan.csv")
model2 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data=treeloss_kalimantan)
summary(model2)
AIC(model2)
residual2 <- model2$residuals

## Uji Asumsi Klasik 
# Uji normalitas residual
ks_test_result <- ks.test(residual2, "pnorm", mean = mean(residual2), sd = sd(residual2))
print(ks_test_result)
# Uji autokorelasi
dwtest(model2)
# Uji heteroskedastisitas
bptest(model2, studentize = FALSE)
# Uji multikolinearitas
vif(model2)

## Regresi PLS
# Buat model PLS
model_pls <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                  data = treeloss_kalimantan,
                  scale = TRUE, #buat standarisasi data
                  validation = "LOO")  # Leave-One-Out cross-validation
summary(model_pls)
coef(model_pls)
# Loading matriks
loading_matrix <- loadings(model_pls)
print(loading_matrix)
# Scores = nilai komponen PLS
scores_matrix <- scores(model_pls)
print(scores_matrix)
# Menyimpan nilai komponen saja ke dalam variabel baru
komponen_pls <- data.frame(unclass(scores(model_pls)))
# Buat data frame gabungan dengan y dan scores
data_plskalimantan <- data.frame(y = treeloss_kalimantan$y, komponen_pls)
# Regresi y terhadap semua komponen PLS
reg_plskalimantan <- lm(y ~ ., data = data_plskalimantan)
summary(reg_plskalimantan) #yang signifikan komponen 1 sampai 4
# Regresi y terhadap 4 komponen PLS
reg_plskalimantan1 <- lm(y ~ Comp.1+Comp.2+Comp.3+Comp.4, data = data_plskalimantan)
summary(reg_plskalimantan1)
vif(reg_plskalimantan1)
AIC(reg_plskalimantan1)
# Buat ulang model PLS dengan 4 komponen
model_pls_final <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                        data = treeloss_kalimantan,
                        scale = TRUE,
                        ncomp = 4)

## Uji asumsi klasik regresi pls
residuals_pls <- residuals(model_pls_final)
# Uji normalitas
ks.test(residuals_pls, "pnorm", mean = mean(residuals_pls), sd = sd(residuals_pls))
# Uji autokorelasi
dwtest(model_pls_final)
# Uji heteroskedastisitas
bptest(model_pls_final, studentize = FALSE)

## Pulau Papua dan Maluku -----------------------------------------------------
## Regresi Linier Berganda
treeloss_papua.maluku <- read.csv("D:/2024/TA/Data/treeloss_papuamaluku.csv")
model3 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data=treeloss_papua.maluku)
summary(model3)
AIC(model3)
residual3 <- model3$residuals

## Uji Asumsi Klasik
# Uji normalitas residual
ks_test_result <- ks.test(residual3, "pnorm", mean = mean(residual3), sd = sd(residual3))
print(ks_test_result)
# Uji autokorelasi
dwtest(model3)
# Uji heteroskedastisitas
bptest(model3, studentize = FALSE)
# Uji multikolinearitas
vif(model3)

## Regresi PLS
# Buat model PLS
model_pls <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                  data = treeloss_papua.maluku,
                  scale = TRUE, #buat standarisasi data
                  validation = "LOO")  # Leave-One-Out cross-validation
summary(model_pls)
coef(model_pls)
# Loading matriks
loading_matrix <- loadings(model_pls)
print(loading_matrix)
# Scores = nilai komponen PLS
scores_matrix <- scores(model_pls)
print(scores_matrix)
# Menyimpan nilai komponen saja ke dalam variabel baru
komponen_pls <- data.frame(unclass(scores(model_pls)))
# Buat data frame gabungan dengan y dan scores
data_plspapuamaluku <- data.frame(y = treeloss_papua.maluku$y, komponen_pls)
# Regresi y terhadap semua komponen PLS
reg_plspapuamaluku <- lm(y ~ ., data = data_plspapuamaluku)
summary(reg_plspapuamaluku) #yang signifikan komponen 1 sampai 3
# Regresi y terhadap 3 komponen PLS
reg_plspapuamaluku1 <- lm(y ~ Comp.1+Comp.2+Comp.3, data = data_plspapuamaluku)
summary(reg_plspapuamaluku1)
vif(reg_plspapuamaluku1)
AIC(reg_plspapuamaluku1)
# Buat ulang model PLS dengan 3 komponen
model_pls_final <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                        data = treeloss_papua.maluku,
                        scale = TRUE,
                        ncomp = 3)

## Uji asumsi klasik regresi pls
residuals_pls <- residuals(model_pls_final)
# Uji normalitas
ks.test(residuals_pls, "pnorm", mean = mean(residuals_pls), sd = sd(residuals_pls))
# Uji autokorelasi
dwtest(model_pls_final)
# Uji heteroskedastisitas
bptest(model_pls_final, studentize = FALSE)

## Pulau Sulawesi -------------------------------------------------------------
## Regresi Linier Berganda
treeloss_sulawesi <- read.csv("D:/2024/TA/Data/treeloss_sulawesi.csv")
model4 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data=treeloss_sulawesi)
summary(model4)
AIC(model4)
residual4 <- model4$residuals

## Uji Asumsi Klasik 
# Uji normalitas residual
ks_test_result <- ks.test(residual4, "pnorm", mean = mean(residual4), sd = sd(residual4))
print(ks_test_result)
# Uji autokorelasi
dwtest(model4)
# Uji heteroskedastisitas
bptest(model4, studentize = FALSE)
# Uji multikolinearitas
vif(model4)

## Regresi PLS
model_pls <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                  data = treeloss_sulawesi,
                  scale = TRUE, #buat standarisasi data
                  validation = "LOO")  # Leave-One-Out cross-validation
summary(model_pls)
coef(model_pls)
# Loading matriks
loading_matrix <- loadings(model_pls)
print(loading_matrix)
# Scores = nilai komponen PLS
scores_matrix <- scores(model_pls)
print(scores_matrix)
# Menyimpan nilai komponen saja ke dalam variabel baru
komponen_pls <- data.frame(unclass(scores(model_pls)))
# Buat data frame gabungan dengan y dan scores
data_plssulawesi <- data.frame(y = treeloss_sulawesi$y, komponen_pls)
# Regresi y terhadap semua komponen PLS
reg_plssulawesi <- lm(y ~ ., data = data_plssulawesi)
summary(reg_plssulawesi) #yang signifikan komponen 1 sampai 5
# Regresi y terhadap 5 komponen PLS
reg_plssulawesi1 <- lm(y ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5, data = data_plssulawesi)
summary(reg_plssulawesi1)
vif(reg_plssulawesi1)
AIC(reg_plssulawesi1)
# Buat ulang model PLS dengan 5 komponen
model_pls_final <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                        data = treeloss_sulawesi,
                        scale = TRUE,
                        ncomp = 5)

## Uji asumsi klasik pls
residuals_pls <- residuals(model_pls_final)
# Uji normalitas
ks.test(residuals_pls, "pnorm", mean = mean(residuals_pls), sd = sd(residuals_pls))
# Uji autokorelasi
dwtest(model_pls_final)
# Uji heteroskedastisitas
bptest(model_pls_final, studentize = FALSE)

## Pulau Sumatra -------------------------------------------------------------
## Regresi Linier Berganda
treeloss_sumatra <- read.csv("D:/2024/TA/Data/treeloss_sumatra.csv")
model5 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+tahun, data=treeloss_sumatra)
summary(model5)
AIC(model5)
residual5 <- model5$residuals

## Uji Asumsi Klasik 
# Uji normalitas residual
ks_test_result <- ks.test(residual5, "pnorm", mean = mean(residual5), sd = sd(residual5))
print(ks_test_result)
# Uji autokorelasi
dwtest(model5)
# Uji heteroskedastisitas
bptest(model5, studentize = FALSE)
# Uji multikolinearitas
vif(model5)

## Regresi PLS
# Buat model PLS
model_pls <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                  data = treeloss_sumatra,
                  scale = TRUE, #buat standarisasi data
                  validation = "LOO")  # Leave-One-Out cross-validation
summary(model_pls)
coef(model_pls)
# Loading matriks
loading_matrix <- loadings(model_pls)
print(loading_matrix)
# Scores = nilai komponen PLS
scores_matrix <- scores(model_pls)
print(scores_matrix)
# Menyimpan nilai komponen saja ke dalam variabel baru
komponen_pls <- data.frame(unclass(scores(model_pls)))
# Buat data frame gabungan dengan y dan scores
data_plssumatra <- data.frame(y = treeloss_sumatra$y, komponen_pls)
# Regresi y terhadap semua komponen PLS
reg_plssumatra <- lm(y ~ ., data = data_plssumatra)
summary(reg_plssumatra) #yang signifikan komponen 1 sampai 5
# Regresi y terhadap 5 komponen PLS
reg_plssumatra1 <- lm(y ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5, data = data_plssumatra)
summary(reg_plssumatra1)
vif(reg_plssumatra1)
AIC(reg_plssumatra1)
# Buat ulang model PLS dengan 5 komponen
model_pls_final <- plsr(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + tahun,
                        data = treeloss_sumatra,
                        scale = TRUE,
                        ncomp = 5)

## Uji asumsi klasik pls
residuals_pls <- residuals(model_pls_final)
# Uji normalitas
ks.test(residuals_pls, "pnorm", mean = mean(residuals_pls), sd = sd(residuals_pls))
# Uji autokorelasi
dwtest(model_pls_final)
# Uji heteroskedastisitas
bptest(model_pls_final, studentize = FALSE)