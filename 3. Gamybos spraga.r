######################################
# Estijos gamybos spraga
######################################

### Rekursinis gamybos spragos suradimas hp filtro pagalba
# Paverčiame Estijos BVP duomenis laiko eilutėmis
L_GDP_EE <- with(data_prod, ts(log(GDP_EE), start = c(1995, 1), frequency = 4))

# 
n <- length(L_GDP_EE)
k <- 12
### HPFilter su trendu
# Apskaičiuojame potencialų Estijos BVP, naudodami Hodrick-Prescott filtrą
POT_L_GDP_EE <- c(hpfilter(L_GDP_EE[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
# Rekursyviniu būdu surandame potencialų BVP
for (i in (k + 1):n) {
  POT_L_GDP_EE[i] <- tail(hpfilter(L_GDP_EE[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP_EE <- ts(POT_L_GDP_EE, start = c(1995, 1), frequency = 4)
ts.plot(L_GDP_EE)
lines(POT_L_GDP_EE, col = "blue")

# Apskaičiuojame gamybos spragą
L_GDP_GAP_EE <- (L_GDP_EE - POT_L_GDP_EE)*100
ts.plot(L_GDP_GAP_EE)

### Redukuotos formos VAR'o įvertinimas
# Apjungiame duomenis analizei
data_ee <- data.frame(diff(with(data_prod, cbind(L_GDP_GAP_EE, L_HW_EE, L_GFCF_EE))))
names(data_ee) <- c("DL_GDP_GAP_EE", "DL_HW_EE", "DL_GFCF_EE")
ts.plot(data_ee)
plot.ts(data_ee)

# Skirtingos dimensijos. Reikia ištrinti eilutes, kuriose yra NA
# Išmetame pirmas 20 eilučių, kurioms neturime darbo kintamojo
#data_ee <- data_ee[-1:-20,]
data_ee <- data_ee[!is.na(data_ee$DL_HW_EE),];beep()
plot.ts(data_ee)
ts.plot(data_ee)
# data_ee$log.HW_EE. <- diff(data_ee$log.HW_EE.)


VARselect(data_ee, lag.max = 8)
var_ee <- VAR(data_ee, p = 4)
summary(var_ee)
acf(residuals(var_ee))
# Paklaidų normališkumo testas
normality.test(var_ee, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_ee, cause = "DL_GDP_GAP_EE")
causality(var_ee, cause = "DL_HW_EE")
causality(var_ee, cause = "DL_GFCF_EE")
# Pirmas porinis Grangerio priežastingumo testas
data_ee1 <- data.frame(with(data_prod, cbind(L_GDP_GAP_EE, DL_HW_EE)))
# ištriname eilutes, kuriose trūksta duomenų
data_ee1 <- data_ee1[!is.na(data_ee1$DL_HW_EE),];beep()
#
var_ee1 <- VAR(data_ee1, p = 4)
summary(var_ee1)
acf(residuals(var_ee1))
Box.test(residuals(var_ee1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ee1, cause = "L_GDP_GAP_EE")
causality(var_ee1, cause = "DL_HW_EE")
# Antras porinis Grangerio priežastingumo testas
data_ee2 <- data.frame(with(data_prod, cbind(L_GDP_GAP_EE, DL_GFCF_EE)))
# Ištriname eilutes, kuriose trūksta duomenų
data_ee2 <- data_ee2[!is.na(data_ee2$DL_GFCF_EE),];beep()

var_ee2 <- VAR(data_ee2, p = 4)
summary(var_ee2)
acf(residuals(var_ee2))
Box.test(residuals(var_ee2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ee2, cause = "L_GDP_GAP_EE")
causality(var_ee2, cause = "DL_GFCF_EE")
# Trečias porinis Grangerio priežastingumo testas
data_ee3 <- data.frame(with(data_prod, cbind(DL_HW_EE, DL_GFCF_EE)))
# Ištriname eilutes, kuriose trūksta duomenų
data_ee3 <- data_ee3[!is.na(data_ee3$DL_HW_EE),];beep()

var_ee3 <- VAR(data_ee3, p = 4)
summary(var_ee3)
acf(residuals(var_ee3))
Box.test(residuals(var_ee3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ee3, cause = "DL_HW_EE")
causality(var_ee3, cause = "DL_GFCF_EE")
# Struktūrinio A tipo SVAR'o įvertinimas
AM <- diag(3)
AM[1, 2] <- NA
AM[1, 3] <- NA
AM[2, 3] <- NA
AM
svar_ee <- SVAR(var_ee, estmethod = "direct", Amat = AM, 
                 method = "BFGS", hessian = TRUE)
svar_ee
svar_ee$A/svar_ee$Ase
plot(irf(svar_ee))
irf(svar_ee, boot = FALSE)
fevd(svar_ee)

# Gamybos spraga. Cycle
y_gap_ee <- with(data_prod, hpfilter(L_GDP_EE, freq = 1600)$cycle*100)
ts.plot(y_gap_ee)
ts.plot(L_GDP_EE)
# Potencialus BVP iš mechaninio ex post 
y_pot_ee <- ts(hpfilter(L_GDP_EE, freq = 1600)$trend, start = c(1995, 3), frequency = 4)
lines(y_pot_ee, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_ee2 <- with(phillips_lt2, cbind(DL_GFKF, DL_EMPL_RP, y_gap))
VARselect(data_ee2, lag.max = 8)
var_ee2 <- VAR(data_ee2, p = 5)
summary(var_ee2)
acf(residuals(var_ee2))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar_ee <- BQ(var_ee2)
bq_svar_ee
plot(irf(bq_svar_ee))
irf(bq_svar_ee, boot = FALSE)
fevd(bq_svar_ee)

########################################
# Latvijos gamybos spraga
########################################


# attach(phillips_lt2)
# with()
# Rekursinis gamybos spragos suradimas hp filtro pagalba
L_GDP_LV <- with(data_prod, ts(log(GDP_LV), start = c(1995, 1), frequency = 4))
n <- length(L_GDP_LV)
k <- 12
# HPFilter su trendu
POT_L_GDP_LV <- c(hpfilter(L_GDP_LV[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
for (i in (k + 1):n) {
  POT_L_GDP_LV[i] <- tail(hpfilter(L_GDP_LV[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP_LV <- ts(POT_L_GDP_LV, start = c(1995, 1), frequency = 4)
ts.plot(L_GDP_LV)
lines(POT_L_GDP_LV, col = "blue")
L_GDP_GAP_LV <- (L_GDP_LV - POT_L_GDP_LV)*100
ts.plot(L_GDP_GAP_LV)
# Redukuotos formos VAR'o įvertinimas
data_lv <- data.frame(with(data_prod, cbind(L_GDP_GAP_LV, log(HW_LV), log(GFCF_LV)))) # reikia diff L ir diff K
data_lv <- data_lv[!is.na(data_lv$log.HW_LV),];beep()

VARselect(data_lv, lag.max = 8)
var_lv <- VAR(data_lv, p = 4)
summary(var_lv)
acf(residuals(var_lv))
# Paklaidų normališkumo testas
normality.test(var_lv, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_lv, cause = "L_GDP_GAP")
causality(var_lv, cause = "L_GDP_GAP_LV")

causality(var_lv, cause = "DL_EMPL_RP")
causality(var_lv, cause = "log.HW_LV.")

causality(var_lv, cause = "DL_GFKF")
causality(var_lv, cause = "log.GFCF_LV.")

# Pirmas porinis Grangerio priežastingumo testas
data_lv1 <- data.frame(with(data_prod, cbind(L_GDP_GAP_LV, log(HW_LV))))
data_lv1 <- data_lv1[!is.na(data_lv1$log.HW_LV.),];beep()

var_lv1 <- VAR(data_lv1, p = 4)
summary(var_lv1)
acf(residuals(var_lv1))
Box.test(residuals(var_lv1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lv1, cause = "L_GDP_GAP_LV")
causality(var_lv1, cause = "DL_EMPL_RP")#  teisingas
causality(var_lv1, cause = "log.HW_LV.")
# Antras porinis Grangerio priežastingumo testas
data_lv2 <- data.frame(with(data_prod, cbind(L_GDP_GAP_LV, log(GFCF_LV)))) # reikia diff K
data_lv2 <- data_lv2[!is.na(data_lv2$log.GFCF_LV.),];beep()
plot.ts(data_lv2)


var_lv2 <- VAR(data_lv2, p = 4)
summary(var_lv2)
acf(residuals(var_lv2))
Box.test(residuals(var_lv2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lv2, cause = "L_GDP_GAP_LV")
causality(var_lv2, cause = "log.GFCF_LV.")
# Trečias porinis Grangerio priežastingumo testas
data_lv3 <- data.frame(with(data_prod, cbind(log(HW_LV), log(GFCF_LV))))
names(data_lv3) <- c("log.HW_LV.", "log.GFCF_LV.")
data_lv3 <- data_lv3[!is.na(data_lv3$log.HW_LV.),];beep()



var_lv3 <- VAR(data_lv3, p = 4)
summary(var_lv3)
acf(residuals(var_lv3))
Box.test(residuals(var_lv3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lv3, cause = "log.HW_LV.")
causality(var_lv3, cause = "log.GFCF_LV.")
# Struktūrinio A tipo SVAR'o įvertinimas
AM <- diag(3)
AM[1, 2] <- NA
AM[1, 3] <- NA
AM[2, 3] <- NA
AM
svar_lv <- SVAR(var_lv, estmethod = "direct", Amat = AM, 
                 method = "BFGS", hessian = TRUE)
svar_lv
svar_lv$A/svar_lv$Ase
plot(irf(svar_lv))
irf(svar_lv, boot = FALSE)
fevd(svar_lv)

# Gamybos spraga. Cycle
y_gap_lv <- hpfilter(L_GDP_LV, freq = 1600)$cycle*100
ts.plot(y_gap_lv)
ts.plot(L_GDP_LV)
# Potencialus BVP iš mechaninio ex post 
y_pot_lv <- ts(hpfilter(L_GDP_LV, freq = 1600)$trend, start = c(1995, 1), frequency = 4)
lines(y_pot_lv, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lv4 <- data.frame(with(data_prod, cbind(GFCF_LV, HW_LV, y_gap_lv))) # 
data_lv4 <- data_lv4[!is.na(data_lv4$log.HW_LV.),];beep()


VARselect(data_lv4, lag.max = 8)
var_lt2 <- VAR(data_lt2, p = 5)
summary(var_lt2)
acf(residuals(var_lt2))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar <- BQ(var_lt2)
bq_svar
plot(irf(bq_svar))
irf(bq_svar, boot = FALSE)
fevd(bq_svar)

#################################################
# Lietuvos gamybos spraga
#################################################

library(mFilter)
library(vars)

phillips_lt2 <- read.delim("~/R/TS/phillips_lt2.txt")
# attach(phillips_lt2)
# with()
# Rekursinis gamybos spragos suradimas hp filtro pagalba
L_GDP_LT <- with(data_prod, ts(log(GDP_LT), start = c(1998, 3), frequency = 4))
n <- length(L_GDP_LT)
k <- 12
# HPFilter su trendu
POT_L_GDP_LT <- c(hpfilter(L_GDP_LT[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
for (i in (k + 1):n) {
  POT_L_GDP_LT[i] <- tail(hpfilter(L_GDP_LT[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP_LT <- ts(POT_L_GDP_LT, start = c(1998, 3), frequency = 4)
ts.plot(L_GDP_LT)
lines(POT_L_GDP_LT, col = "blue")
L_GDP_GAP_LT <- (L_GDP_LT - POT_L_GDP_LT)*100
ts.plot(L_GDP_GAP_LT)
# Redukuotos formos VAR'o įvertinimas
data_ltu <- with(data_prod, cbind(L_GDP_GAP_LT, diff(log(HW_LT)), diff(log(GFCF_LT))))
VARselect(data_ltu, lag.max = 8)
var_ltu <- VAR(data_ltu, p = 4)
summary(var_ltu)
acf(residuals(var_ltu))
# Paklaidų normališkumo testas
normality.test(var_ltu, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_ltu, cause = "L_GDP_GAP")
causality(var_ltu, cause = "DL_EMPL_RP")
causality(var_ltu, cause = "DL_GFKF")
# Pirmas porinis Grangerio priežastingumo testas
data_ltu1 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP))
var_ltu1 <- VAR(data_ltu1, p = 4)
summary(var_ltu1)
acf(residuals(var_ltu1))
Box.test(residuals(var_ltu1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu1, cause = "L_GDP_GAP")
causality(var_ltu1, cause = "DL_EMPL_RP")
# Antras porinis Grangerio priežastingumo testas
data_ltu2 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_GFKF))
var_ltu2 <- VAR(data_ltu2, p = 4)
summary(var_ltu2)
acf(residuals(var_ltu2))
Box.test(residuals(var_ltu2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu2, cause = "L_GDP_GAP")
causality(var_ltu2, cause = "DL_GFKF")
# Trečias porinis Grangerio priežastingumo testas
data_ltu3 <- with(phillips_lt2, cbind(DL_EMPL_RP, DL_GFKF))
var_ltu3 <- VAR(data_ltu3, p = 4)
summary(var_ltu3)
acf(residuals(var_ltu3))
Box.test(residuals(var_ltu3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu3, cause = "DL_EMPL_RP")
causality(var_ltu3, cause = "DL_GFKF")
# Struktūrinio A tipo SVAR'o įvertinimas
AM <- diag(3)
AM[1, 2] <- NA
AM[1, 3] <- NA
AM[2, 3] <- NA
AM
svar_ltu <- SVAR(var_ltu, estmethod = "direct", Amat = AM, 
                 method = "BFGS", hessian = TRUE)
svar_ltu
svar_ltu$A/svar_ltu$Ase
plot(irf(svar_ltu))
irf(svar_ltu, boot = FALSE)
fevd(svar_ltu)

# Gamybos spraga. Cycle
y_gap <- hpfilter(L_GDP, freq = 1600)$cycle*100
ts.plot(y_gap)
ts.plot(L_GDP)
# Potencialus BVP iš mechaninio ex post 
y_pot <- ts(hpfilter(L_GDP, freq = 1600)$trend, start = c(1998, 3), frequency = 4)
lines(y_pot, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lt2 <- with(phillips_lt2, cbind(DL_GFKF, DL_EMPL_RP, y_gap))
VARselect(data_lt2, lag.max = 8)
var_lt2 <- VAR(data_lt2, p = 5)
summary(var_lt2)
acf(residuals(var_lt2))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar <- BQ(var_lt2)
bq_svar
plot(irf(bq_svar))
irf(bq_svar, boot = FALSE)
fevd(bq_svar)

###########################################
# Lenkijos gamybos spraga
###########################################

library(mFilter)
library(vars)

phillips_lt2 <- read.delim("~/R/TS/phillips_lt2.txt")
# attach(phillips_lt2)
# with()
# Rekursinis gamybos spragos suradimas hp filtro pagalba
L_GDP <- with(phillips_lt2, ts(L_GDP, start = c(1998, 3), frequency = 4))
n <- length(L_GDP)
k <- 12
# HPFilter su trendu
POT_L_GDP <- c(hpfilter(L_GDP[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
for (i in (k + 1):n) {
  POT_L_GDP[i] <- tail(hpfilter(L_GDP[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP <- ts(POT_L_GDP, start = c(1998, 3), frequency = 4)
ts.plot(L_GDP)
lines(POT_L_GDP, col = "blue")
L_GDP_GAP <- (L_GDP - POT_L_GDP)*100
ts.plot(L_GDP_GAP)
# Redukuotos formos VAR'o įvertinimas
data_ltu <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP, DL_GFKF))
VARselect(data_ltu, lag.max = 8)
var_ltu <- VAR(data_ltu, p = 4)
summary(var_ltu)
acf(residuals(var_ltu))
# Paklaidų normališkumo testas
normality.test(var_ltu, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_ltu, cause = "L_GDP_GAP")
causality(var_ltu, cause = "DL_EMPL_RP")
causality(var_ltu, cause = "DL_GFKF")
# Pirmas porinis Grangerio priežastingumo testas
data_ltu1 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP))
var_ltu1 <- VAR(data_ltu1, p = 4)
summary(var_ltu1)
acf(residuals(var_ltu1))
Box.test(residuals(var_ltu1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu1, cause = "L_GDP_GAP")
causality(var_ltu1, cause = "DL_EMPL_RP")
# Antras porinis Grangerio priežastingumo testas
data_ltu2 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_GFKF))
var_ltu2 <- VAR(data_ltu2, p = 4)
summary(var_ltu2)
acf(residuals(var_ltu2))
Box.test(residuals(var_ltu2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu2, cause = "L_GDP_GAP")
causality(var_ltu2, cause = "DL_GFKF")
# Trečias porinis Grangerio priežastingumo testas
data_ltu3 <- with(phillips_lt2, cbind(DL_EMPL_RP, DL_GFKF))
var_ltu3 <- VAR(data_ltu3, p = 4)
summary(var_ltu3)
acf(residuals(var_ltu3))
Box.test(residuals(var_ltu3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu3, cause = "DL_EMPL_RP")
causality(var_ltu3, cause = "DL_GFKF")
# Struktūrinio A tipo SVAR'o įvertinimas
AM <- diag(3)
AM[1, 2] <- NA
AM[1, 3] <- NA
AM[2, 3] <- NA
AM
svar_ltu <- SVAR(var_ltu, estmethod = "direct", Amat = AM, 
                 method = "BFGS", hessian = TRUE)
svar_ltu
svar_ltu$A/svar_ltu$Ase
plot(irf(svar_ltu))
irf(svar_ltu, boot = FALSE)
fevd(svar_ltu)

# Gamybos spraga. Cycle
y_gap <- hpfilter(L_GDP, freq = 1600)$cycle*100
ts.plot(y_gap)
ts.plot(L_GDP)
# Potencialus BVP iš mechaninio ex post 
y_pot <- ts(hpfilter(L_GDP, freq = 1600)$trend, start = c(1998, 3), frequency = 4)
lines(y_pot, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lt2 <- with(phillips_lt2, cbind(DL_GFKF, DL_EMPL_RP, y_gap))
VARselect(data_lt2, lag.max = 8)
var_lt2 <- VAR(data_lt2, p = 5)
summary(var_lt2)
acf(residuals(var_lt2))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar <- BQ(var_lt2)
bq_svar
plot(irf(bq_svar))
irf(bq_svar, boot = FALSE)
fevd(bq_svar)

#########################################
# Čekijos gamybos spraga
#########################################

library(mFilter)
library(vars)

phillips_lt2 <- read.delim("~/R/TS/phillips_lt2.txt")
# attach(phillips_lt2)
# with()
# Rekursinis gamybos spragos suradimas hp filtro pagalba
L_GDP <- with(phillips_lt2, ts(L_GDP, start = c(1998, 3), frequency = 4))
n <- length(L_GDP)
k <- 12
# HPFilter su trendu
POT_L_GDP <- c(hpfilter(L_GDP[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
for (i in (k + 1):n) {
  POT_L_GDP[i] <- tail(hpfilter(L_GDP[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP <- ts(POT_L_GDP, start = c(1998, 3), frequency = 4)
ts.plot(L_GDP)
lines(POT_L_GDP, col = "blue")
L_GDP_GAP <- (L_GDP - POT_L_GDP)*100
ts.plot(L_GDP_GAP)
# Redukuotos formos VAR'o įvertinimas
data_ltu <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP, DL_GFKF))
VARselect(data_ltu, lag.max = 8)
var_ltu <- VAR(data_ltu, p = 4)
summary(var_ltu)
acf(residuals(var_ltu))
# Paklaidų normališkumo testas
normality.test(var_ltu, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_ltu, cause = "L_GDP_GAP")
causality(var_ltu, cause = "DL_EMPL_RP")
causality(var_ltu, cause = "DL_GFKF")
# Pirmas porinis Grangerio priežastingumo testas
data_ltu1 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP))
var_ltu1 <- VAR(data_ltu1, p = 4)
summary(var_ltu1)
acf(residuals(var_ltu1))
Box.test(residuals(var_ltu1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu1, cause = "L_GDP_GAP")
causality(var_ltu1, cause = "DL_EMPL_RP")
# Antras porinis Grangerio priežastingumo testas
data_ltu2 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_GFKF))
var_ltu2 <- VAR(data_ltu2, p = 4)
summary(var_ltu2)
acf(residuals(var_ltu2))
Box.test(residuals(var_ltu2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu2, cause = "L_GDP_GAP")
causality(var_ltu2, cause = "DL_GFKF")
# Trečias porinis Grangerio priežastingumo testas
data_ltu3 <- with(phillips_lt2, cbind(DL_EMPL_RP, DL_GFKF))
var_ltu3 <- VAR(data_ltu3, p = 4)
summary(var_ltu3)
acf(residuals(var_ltu3))
Box.test(residuals(var_ltu3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu3, cause = "DL_EMPL_RP")
causality(var_ltu3, cause = "DL_GFKF")
# Struktūrinio A tipo SVAR'o įvertinimas
AM <- diag(3)
AM[1, 2] <- NA
AM[1, 3] <- NA
AM[2, 3] <- NA
AM
svar_ltu <- SVAR(var_ltu, estmethod = "direct", Amat = AM, 
                 method = "BFGS", hessian = TRUE)
svar_ltu
svar_ltu$A/svar_ltu$Ase
plot(irf(svar_ltu))
irf(svar_ltu, boot = FALSE)
fevd(svar_ltu)

# Gamybos spraga. Cycle
y_gap <- hpfilter(L_GDP, freq = 1600)$cycle*100
ts.plot(y_gap)
ts.plot(L_GDP)
# Potencialus BVP iš mechaninio ex post 
y_pot <- ts(hpfilter(L_GDP, freq = 1600)$trend, start = c(1998, 3), frequency = 4)
lines(y_pot, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lt2 <- with(phillips_lt2, cbind(DL_GFKF, DL_EMPL_RP, y_gap))
VARselect(data_lt2, lag.max = 8)
var_lt2 <- VAR(data_lt2, p = 5)
summary(var_lt2)
acf(residuals(var_lt2))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar <- BQ(var_lt2)
bq_svar
plot(irf(bq_svar))
irf(bq_svar, boot = FALSE)
fevd(bq_svar)

#######################################
# Vengrjos gamybos spraga
#######################################

library(mFilter)
library(vars)

phillips_lt2 <- read.delim("~/R/TS/phillips_lt2.txt")
# attach(phillips_lt2)
# with()
# Rekursinis gamybos spragos suradimas hp filtro pagalba
L_GDP <- with(phillips_lt2, ts(L_GDP, start = c(1998, 3), frequency = 4))
n <- length(L_GDP)
k <- 12
# HPFilter su trendu
POT_L_GDP <- c(hpfilter(L_GDP[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
for (i in (k + 1):n) {
  POT_L_GDP[i] <- tail(hpfilter(L_GDP[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP <- ts(POT_L_GDP, start = c(1998, 3), frequency = 4)
ts.plot(L_GDP)
lines(POT_L_GDP, col = "blue")
L_GDP_GAP <- (L_GDP - POT_L_GDP)*100
ts.plot(L_GDP_GAP)
# Redukuotos formos VAR'o įvertinimas
data_ltu <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP, DL_GFKF))
VARselect(data_ltu, lag.max = 8)
var_ltu <- VAR(data_ltu, p = 4)
summary(var_ltu)
acf(residuals(var_ltu))
# Paklaidų normališkumo testas
normality.test(var_ltu, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_ltu, cause = "L_GDP_GAP")
causality(var_ltu, cause = "DL_EMPL_RP")
causality(var_ltu, cause = "DL_GFKF")
# Pirmas porinis Grangerio priežastingumo testas
data_ltu1 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP))
var_ltu1 <- VAR(data_ltu1, p = 4)
summary(var_ltu1)
acf(residuals(var_ltu1))
Box.test(residuals(var_ltu1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu1, cause = "L_GDP_GAP")
causality(var_ltu1, cause = "DL_EMPL_RP")
# Antras porinis Grangerio priežastingumo testas
data_ltu2 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_GFKF))
var_ltu2 <- VAR(data_ltu2, p = 4)
summary(var_ltu2)
acf(residuals(var_ltu2))
Box.test(residuals(var_ltu2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu2, cause = "L_GDP_GAP")
causality(var_ltu2, cause = "DL_GFKF")
# Trečias porinis Grangerio priežastingumo testas
data_ltu3 <- with(phillips_lt2, cbind(DL_EMPL_RP, DL_GFKF))
var_ltu3 <- VAR(data_ltu3, p = 4)
summary(var_ltu3)
acf(residuals(var_ltu3))
Box.test(residuals(var_ltu3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu3, cause = "DL_EMPL_RP")
causality(var_ltu3, cause = "DL_GFKF")
# Struktūrinio A tipo SVAR'o įvertinimas
AM <- diag(3)
AM[1, 2] <- NA
AM[1, 3] <- NA
AM[2, 3] <- NA
AM
svar_ltu <- SVAR(var_ltu, estmethod = "direct", Amat = AM, 
                 method = "BFGS", hessian = TRUE)
svar_ltu
svar_ltu$A/svar_ltu$Ase
plot(irf(svar_ltu))
irf(svar_ltu, boot = FALSE)
fevd(svar_ltu)

# Gamybos spraga. Cycle
y_gap <- hpfilter(L_GDP, freq = 1600)$cycle*100
ts.plot(y_gap)
ts.plot(L_GDP)
# Potencialus BVP iš mechaninio ex post 
y_pot <- ts(hpfilter(L_GDP, freq = 1600)$trend, start = c(1998, 3), frequency = 4)
lines(y_pot, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lt2 <- with(phillips_lt2, cbind(DL_GFKF, DL_EMPL_RP, y_gap))
VARselect(data_lt2, lag.max = 8)
var_lt2 <- VAR(data_lt2, p = 5)
summary(var_lt2)
acf(residuals(var_lt2))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar <- BQ(var_lt2)
bq_svar
plot(irf(bq_svar))
irf(bq_svar, boot = FALSE)
fevd(bq_svar)

###########################################
# Rumunijos gamybos spraga
###########################################

library(mFilter)
library(vars)

phillips_lt2 <- read.delim("~/R/TS/phillips_lt2.txt")
attach(phillips_lt2)
# with()
# Rekursinis gamybos spragos suradimas hp filtro pagalba
L_GDP <- with(phillips_lt2, ts(L_GDP, start = c(1998, 3), frequency = 4))
n <- length(L_GDP)
k <- 12
# HPFilter su trendu
POT_L_GDP <- c(hpfilter(L_GDP[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
for (i in (k + 1):n) {
  POT_L_GDP[i] <- tail(hpfilter(L_GDP[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP <- ts(POT_L_GDP, start = c(1998, 3), frequency = 4)
ts.plot(L_GDP)
lines(POT_L_GDP, col = "blue")
L_GDP_GAP <- (L_GDP - POT_L_GDP)*100
ts.plot(L_GDP_GAP)
# Redukuotos formos VAR'o įvertinimas
data_ltu <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP, DL_GFKF))
VARselect(data_ltu, lag.max = 8)
var_ltu <- VAR(data_ltu, p = 4)
summary(var_ltu)
acf(residuals(var_ltu))
# Paklaidų normališkumo testas
normality.test(var_ltu, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_ltu, cause = "L_GDP_GAP")
causality(var_ltu, cause = "DL_EMPL_RP")
causality(var_ltu, cause = "DL_GFKF")
# Pirmas porinis Grangerio priežastingumo testas
data_ltu1 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_EMPL_RP))
var_ltu1 <- VAR(data_ltu1, p = 4)
summary(var_ltu1)
acf(residuals(var_ltu1))
Box.test(residuals(var_ltu1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu1, cause = "L_GDP_GAP")
causality(var_ltu1, cause = "DL_EMPL_RP")
# Antras porinis Grangerio priežastingumo testas
data_ltu2 <- with(phillips_lt2, cbind(L_GDP_GAP, DL_GFKF))
var_ltu2 <- VAR(data_ltu2, p = 4)
summary(var_ltu2)
acf(residuals(var_ltu2))
Box.test(residuals(var_ltu2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu2, cause = "L_GDP_GAP")
causality(var_ltu2, cause = "DL_GFKF")
# Trečias porinis Grangerio priežastingumo testas
data_ltu3 <- with(phillips_lt2, cbind(DL_EMPL_RP, DL_GFKF))
var_ltu3 <- VAR(data_ltu3, p = 4)
summary(var_ltu3)
acf(residuals(var_ltu3))
Box.test(residuals(var_ltu3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_ltu3, cause = "DL_EMPL_RP")
causality(var_ltu3, cause = "DL_GFKF")
# Struktūrinio A tipo SVAR'o įvertinimas
AM <- diag(3)
AM[1, 2] <- NA
AM[1, 3] <- NA
AM[2, 3] <- NA
AM
svar_ltu <- SVAR(var_ltu, estmethod = "direct", Amat = AM, 
                 method = "BFGS", hessian = TRUE)
svar_ltu
svar_ltu$A/svar_ltu$Ase
plot(irf(svar_ltu))
irf(svar_ltu, boot = FALSE)
fevd(svar_ltu)

# Gamybos spraga. Cycle
y_gap <- hpfilter(L_GDP, freq = 1600)$cycle*100
ts.plot(y_gap)
ts.plot(L_GDP)
# Potencialus BVP iš mechaninio ex post 
y_pot <- ts(hpfilter(L_GDP, freq = 1600)$trend, start = c(1998, 3), frequency = 4)
lines(y_pot, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lt2 <- with(phillips_lt2, cbind(DL_GFKF, DL_EMPL_RP, y_gap))
VARselect(data_lt2, lag.max = 8)
var_lt2 <- VAR(data_lt2, p = 5)
summary(var_lt2)
acf(residuals(var_lt2))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar <- BQ(var_lt2)
bq_svar
plot(irf(bq_svar))
irf(bq_svar, boot = FALSE)
fevd(bq_svar)



