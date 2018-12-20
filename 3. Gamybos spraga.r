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
y_pot_ee <- ts(hpfilter(L_GDP_EE, freq = 1600)$trend, start = c(1995, 1), frequency = 4)
lines(y_pot_ee, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_ee22 <- data.frame(with(data_prod, cbind(DL_GFCF_EE, DL_HW_EE, y_gap_ee)))
plot.ts(data_ee22)
# Ištriname eilutes, kuriose trūksta duomenų
data_ee22 <- data_ee22[!is.na(data_ee22$DL_GFCF_EE),];beep()
data_ee22 <- data_ee22[!is.na(data_ee22$DL_HW_EE),];beep()
plot.ts(data_ee22)
VARselect(data_ee22, lag.max = 8)
var_ee22 <- VAR(data_ee22, p = 5)
summary(var_ee22)
acf(residuals(var_ee22))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar_ee <- BQ(var_ee22)
bq_svar_ee
plot(irf(bq_svar_ee))
irf(bq_svar_ee, boot = FALSE)
fevd(bq_svar_ee)

######################################
# Latvijos gamybos spraga
######################################

### Rekursinis gamybos spragos suradimas hp filtro pagalba
# Paverčiame Latvijos BVP duomenis laiko eilutėmis
L_GDP_LV <- with(data_prod, ts(log(GDP_LV), start = c(1995, 1), frequency = 4))

# 
n <- length(L_GDP_LV)
k <- 12
### HPFilter su trendu
# Apskaičiuojame potencialų Latvijos BVP, naudodami Hodrick-Prescott filtrą
POT_L_GDP_LV <- c(hpfilter(L_GDP_LV[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
# Rekursyviniu būdu surandame potencialų BVP
for (i in (k + 1):n) {
  POT_L_GDP_LV[i] <- tail(hpfilter(L_GDP_LV[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP_LV <- ts(POT_L_GDP_LV, start = c(1995, 1), frequency = 4)
ts.plot(L_GDP_LV)
lines(POT_L_GDP_LV, col = "blue")

# Apskaičiuojame gamybos spragą
L_GDP_GAP_LV <- (L_GDP_LV - POT_L_GDP_LV)*100
ts.plot(L_GDP_GAP_LV)

### Redukuotos formos VAR'o įvertinimas
# Apjungiame duomenis analizei
data_lv <- data.frame(diff(with(data_prod, cbind(L_GDP_GAP_LV, L_HW_LV, L_GFCF_LV))))
names(data_lv) <- c("DL_GDP_GAP_LV", "DL_HW_LV", "DL_GFCF_LV")
ts.plot(data_lv)
plot.ts(data_lv)

# Skirtingos dimensijos. Reikia ištrinti eilutes, kuriose yra NA
# Išmetame pirmas 20 eilučių, kurioms neturime darbo kintamojo
#data_ee <- data_ee[-1:-20,]
# data_ee <- data_ee[!is.na(data_ee$DL_HW_EE),];beep()
plot.ts(data_lv)
ts.plot(data_lv)
# data_ee$log.HW_EE. <- diff(data_ee$log.HW_EE.)


VARselect(data_lv, lag.max = 8)
var_lv <- VAR(data_lv, p = 4)
summary(var_lv)
acf(residuals(var_lv))
# Paklaidų normališkumo testas
normality.test(var_lv, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_lv, cause = "DL_GDP_GAP_LV")
causality(var_lv, cause = "DL_HW_LV")
causality(var_lv, cause = "DL_GFCF_LV")
# Pirmas porinis Grangerio priežastingumo testas
data_lv1 <- data.frame(with(data_prod, cbind(L_GDP_GAP_LV, DL_HW_LV)))
# ištriname eilutes, kuriose trūksta duomenų
data_lv1 <- data_lv1[!is.na(data_lv1$DL_HW_LV),];beep()
#
var_lv1 <- VAR(data_lv1, p = 4)
summary(var_lv1)
acf(residuals(var_lv1))
Box.test(residuals(var_lv1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lv1, cause = "L_GDP_GAP_LV")
causality(var_lv1, cause = "DL_HW_LV")
# Antras porinis Grangerio priežastingumo testas
data_lv2 <- data.frame(with(data_prod, cbind(L_GDP_GAP_LV, DL_GFCF_LV)))
# Ištriname eilutes, kuriose trūksta duomenų
data_lv2 <- data_lv2[!is.na(data_lv2$DL_GFCF_LV),];beep()

var_lv2 <- VAR(data_lv2, p = 4)
summary(var_lv2)
acf(residuals(var_lv2))
Box.test(residuals(var_lv2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lv2, cause = "L_GDP_GAP_LV")
causality(var_lv2, cause = "DL_GFCF_LV")
# Trečias porinis Grangerio priežastingumo testas
data_lv3 <- data.frame(with(data_prod, cbind(DL_HW_LV, DL_GFCF_LV)))
# Ištriname eilutes, kuriose trūksta duomenų
data_lv3 <- data_lv3[!is.na(data_lv3$DL_HW_LV),];beep()

var_lv3 <- VAR(data_lv3, p = 4)
summary(var_lv3)
acf(residuals(var_lv3))
Box.test(residuals(var_lv3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lv3, cause = "DL_HW_LV")
causality(var_lv3, cause = "DL_GFCF_LV")
# Struktūrinio A tipo SVAR'o įvertinimas
AM
svar_lv <- SVAR(var_lv, estmethod = "direct", Amat = AM, 
                method = "BFGS", hessian = TRUE)
svar_lv
svar_lv$A/svar_lv$Ase
plot(irf(svar_lv))
irf(svar_lv, boot = FALSE)
fevd(svar_lv)

# Gamybos spraga. Cycle
y_gap_lv <- with(data_prod, hpfilter(L_GDP_LV, freq = 1600)$cycle*100)
ts.plot(y_gap_lv)
ts.plot(L_GDP_LV)
# Potencialus BVP iš mechaninio ex post 
y_pot_lv <- ts(hpfilter(L_GDP_LV, freq = 1600)$trend, start = c(1995, 1), frequency = 4)
lines(y_pot_lv, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lv22 <- data.frame(with(data_prod, cbind(DL_GFCF_LV, DL_HW_LV, y_gap_lv)))
plot.ts(data_lv22)
# Ištriname eilutes, kuriose trūksta duomenų
data_lv22 <- data_lv22[!is.na(data_lv22$DL_GFCF_LV),];beep()
# data_lv22 <- data_lv22[!is.na(data_lv22$DL_HW_LV),];beep()
plot.ts(data_lv22)
VARselect(data_lv22, lag.max = 8)
var_lv22 <- VAR(data_lv22, p = 5)
summary(var_lv22)
acf(residuals(var_lv22))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar_lv <- BQ(var_lv22)
bq_svar_lv
plot(irf(bq_svar_lv))
irf(bq_svar_lv, boot = FALSE)
fevd(bq_svar_lv)

######################################
# Lietuvos gamybos spraga
######################################

### Rekursinis gamybos spragos suradimas hp filtro pagalba
# Paverčiame Lietuvos BVP duomenis laiko eilutėmis
L_GDP_LT <- with(data_prod, ts(log(GDP_LT), start = c(1995, 1), frequency = 4))

# 
n <- length(L_GDP_LT)
k <- 12
### HPFilter su trendu
# Apskaičiuojame potencialų Lietuvos BVP, naudodami Hodrick-Prescott filtrą
POT_L_GDP_LT <- c(hpfilter(L_GDP_LT[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
# Rekursyviniu būdu surandame potencialų BVP
for (i in (k + 1):n) {
  POT_L_GDP_LT[i] <- tail(hpfilter(L_GDP_LT[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP_LT <- ts(POT_L_GDP_LT, start = c(1995, 1), frequency = 4)
ts.plot(L_GDP_LT)
lines(POT_L_GDP_LT, col = "blue")

# Apskaičiuojame gamybos spragą
L_GDP_GAP_LT <- (L_GDP_LT - POT_L_GDP_LT)*100
ts.plot(L_GDP_GAP_LT)

### Redukuotos formos VAR'o įvertinimas
# Apjungiame duomenis analizei
data_lt <- data.frame(diff(with(data_prod, cbind(L_GDP_GAP_LT, L_HW_LT, L_GFCF_LT))))
names(data_lt) <- c("DL_GDP_GAP_LT", "DL_HW_LT", "DL_GFCF_LT")
ts.plot(data_lt)
plot.ts(data_lt)

# Skirtingos dimensijos. Reikia ištrinti eilutes, kuriose yra NA
# Išmetame pirmas 20 eilučių, kurioms neturime darbo kintamojo
#data_ee <- data_ee[-1:-20,]
# data_ee <- data_ee[!is.na(data_ee$DL_HW_EE),];beep()
plot.ts(data_lt)
ts.plot(data_lt)
# data_ee$log.HW_EE. <- diff(data_ee$log.HW_EE.)


VARselect(data_lt, lag.max = 8)
var_lt <- VAR(data_lt, p = 4)
summary(var_lt)
acf(residuals(var_lt))
# Paklaidų normališkumo testas
normality.test(var_lt, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_lt, cause = "DL_GDP_GAP_LT")
causality(var_lt, cause = "DL_HW_LT")
causality(var_lt, cause = "DL_GFCF_LT")
# Pirmas porinis Grangerio priežastingumo testas
data_lt1 <- data.frame(with(data_prod, cbind(L_GDP_GAP_LT, DL_HW_LT)))
# ištriname eilutes, kuriose trūksta duomenų
data_lt1 <- data_lt1[!is.na(data_lt1$DL_HW_LT),];beep()
#
var_lt1 <- VAR(data_lt1, p = 4)
summary(var_lt1)
acf(residuals(var_lv1))
Box.test(residuals(var_lt1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lt1, cause = "L_GDP_GAP_LT")
causality(var_lt1, cause = "DL_HW_LT")
# Antras porinis Grangerio priežastingumo testas
data_lt2 <- data.frame(with(data_prod, cbind(L_GDP_GAP_LT, DL_GFCF_LT)))
# Ištriname eilutes, kuriose trūksta duomenų
data_lt2 <- data_lt2[!is.na(data_lt2$DL_GFCF_LT),];beep()

var_lt2 <- VAR(data_lt2, p = 4)
summary(var_lt2)
acf(residuals(var_lt2))
Box.test(residuals(var_lt2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lt2, cause = "L_GDP_GAP_LT")
causality(var_lt2, cause = "DL_GFCF_LT")
# Trečias porinis Grangerio priežastingumo testas
data_lt3 <- data.frame(with(data_prod, cbind(DL_HW_LT, DL_GFCF_LT)))
# Ištriname eilutes, kuriose trūksta duomenų
data_lt3 <- data_lt3[!is.na(data_lt3$DL_HW_LT),];beep()

var_lt3 <- VAR(data_lt3, p = 4)
summary(var_lt3)
acf(residuals(var_lt3))
Box.test(residuals(var_lt3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_lt3, cause = "DL_HW_LT")
causality(var_lt3, cause = "DL_GFCF_LT")
# Struktūrinio A tipo SVAR'o įvertinimas
AM
svar_lt <- SVAR(var_lt, estmethod = "direct", Amat = AM, 
                method = "BFGS", hessian = TRUE)
svar_lt
svar_lt$A/svar_lt$Ase
plot(irf(svar_lt))
irf(svar_lt, boot = FALSE)
fevd(svar_lt)

# Gamybos spraga. Cycle
y_gap_lt <- with(data_prod, hpfilter(L_GDP_LT, freq = 1600)$cycle*100)
ts.plot(y_gap_lt)
ts.plot(L_GDP_LT)
# Potencialus BVP iš mechaninio ex post 
y_pot_lt <- ts(hpfilter(L_GDP_LT, freq = 1600)$trend, start = c(1995, 1), frequency = 4)
lines(y_pot_lt, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_lt22 <- data.frame(with(data_prod, cbind(DL_GFCF_LT, DL_HW_LT, y_gap_lt)))
plot.ts(data_lt22)
# Ištriname eilutes, kuriose trūksta duomenų
data_lt22 <- data_lt22[!is.na(data_lt22$DL_GFCF_LT),];beep()
# data_lv22 <- data_lv22[!is.na(data_lv22$DL_HW_LV),];beep()
plot.ts(data_lt22)
VARselect(data_lt22, lag.max = 8)
var_lt22 <- VAR(data_lt22, p = 5)
summary(var_lt22)
acf(residuals(var_lt22))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar_lt <- BQ(var_lt22)
bq_svar_lt
plot(irf(bq_svar_lt))
irf(bq_svar_lt, boot = FALSE)
fevd(bq_svar_lt)

######################################
# Lenkijos gamybos spraga
######################################

### Rekursinis gamybos spragos suradimas hp filtro pagalba
# Paverčiame Lenkijos BVP duomenis laiko eilutėmis
L_GDP_PL <- with(data_prod, ts(log(GDP_PL), start = c(1995, 1), frequency = 4))

# 
n <- length(L_GDP_PL)
k <- 12
### HPFilter su trendu
# Apskaičiuojame potencialų Lenkijos BVP, naudodami Hodrick-Prescott filtrą
POT_L_GDP_PL <- c(hpfilter(L_GDP_PL[1:k], freq = 1600, drift = FALSE)$trend, rep(NA, n - k))
# Rekursyviniu būdu surandame potencialų BVP
for (i in (k + 1):n) {
  POT_L_GDP_PL[i] <- tail(hpfilter(L_GDP_PL[1:i], freq = 1600)$trend, 1)
}
POT_L_GDP_PL <- ts(POT_L_GDP_PL, start = c(1995, 1), frequency = 4)
ts.plot(L_GDP_PL)
lines(POT_L_GDP_PL, col = "blue")

# Apskaičiuojame gamybos spragą
L_GDP_GAP_PL <- (L_GDP_PL - POT_L_GDP_PL)*100
ts.plot(L_GDP_GAP_PL)

### Redukuotos formos VAR'o įvertinimas
# Apjungiame duomenis analizei
data_pl <- data.frame(diff(with(data_prod, cbind(L_GDP_GAP_PL, L_HW_PL, L_GFCF_PL))))
names(data_pl) <- c("DL_GDP_GAP_PL", "DL_HW_PL", "DL_GFCF_PL")
ts.plot(data_pl)
plot.ts(data_pl)

# Skirtingos dimensijos. Reikia ištrinti eilutes, kuriose yra NA
# Išmetame pirmas 20 eilučių, kurioms neturime darbo kintamojo
#data_ee <- data_ee[-1:-20,]
data_pl <- data_pl[!is.na(data_pl$DL_HW_PL),];beep()
data_pl <- data_pl[!is.na(data_pl$DL_GFCF_PL),];beep()
plot.ts(data_pl)
ts.plot(data_pl)
# data_ee$log.HW_EE. <- diff(data_ee$log.HW_EE.)


VARselect(data_pl, lag.max = 8)
var_pl <- VAR(data_pl, p = 4)
summary(var_pl)
acf(residuals(var_pl))
# Paklaidų normališkumo testas
normality.test(var_pl, multivariate.only = FALSE)
# Daugiamatis Grangerio priežastingumo testas
causality(var_pl, cause = "DL_GDP_GAP_PL")
causality(var_pl, cause = "DL_HW_PL")
causality(var_pl, cause = "DL_GFCF_PL")
# Pirmas porinis Grangerio priežastingumo testas
data_pl1 <- data.frame(with(data_prod, cbind(L_GDP_GAP_PL, DL_HW_PL)))
# ištriname eilutes, kuriose trūksta duomenų
data_pl1 <- data_pl1[!is.na(data_pl1$DL_HW_PL),];beep()
#
var_pl1 <- VAR(data_pl1, p = 4)
summary(var_pl1)
acf(residuals(var_lv1))
Box.test(residuals(var_pl1)[, 2], lag = 8, type = "Ljung-Box")
causality(var_pl1, cause = "L_GDP_GAP_PL")
causality(var_pl1, cause = "DL_HW_PL")
# Antras porinis Grangerio priežastingumo testas
data_pl2 <- data.frame(with(data_prod, cbind(L_GDP_GAP_PL, DL_GFCF_PL)))
# Ištriname eilutes, kuriose trūksta duomenų
data_pl2 <- data_pl2[!is.na(data_pl2$DL_GFCF_PL),];beep()

var_pl2 <- VAR(data_pl2, p = 4)
summary(var_pl2)
acf(residuals(var_pl2))
Box.test(residuals(var_pl2)[, 2], lag = 8, type = "Ljung-Box")
causality(var_pl2, cause = "L_GDP_GAP_PL")
causality(var_pl2, cause = "DL_GFCF_PL")
# Trečias porinis Grangerio priežastingumo testas
data_pl3 <- data.frame(with(data_prod, cbind(DL_HW_PL, DL_GFCF_PL)))
# Ištriname eilutes, kuriose trūksta duomenų
data_pl3 <- data_pl3[!is.na(data_pl3$DL_HW_PL),];beep()
data_pl3 <- data_pl3[!is.na(data_pl3$DL_GFCF_PL),];beep()
var_pl3 <- VAR(data_pl3, p = 4)
summary(var_pl3)
acf(residuals(var_pl3))
Box.test(residuals(var_pl3)[, 2], lag = 8, type = "Ljung-Box")
causality(var_pl3, cause = "DL_HW_PL")
causality(var_pl3, cause = "DL_GFCF_PL")
# Struktūrinio A tipo SVAR'o įvertinimas
AM
svar_pl <- SVAR(var_pl, estmethod = "direct", Amat = AM, 
                method = "BFGS", hessian = TRUE)
svar_pl
svar_pl$A/svar_pl$Ase
plot(irf(svar_pl))
irf(svar_pl, boot = FALSE)
fevd(svar_pl)

# Gamybos spraga. Cycle
y_gap_pl <- with(data_prod, hpfilter(L_GDP_PL, freq = 1600)$cycle*100)
ts.plot(y_gap_pl)
ts.plot(L_GDP_PL)
# Potencialus BVP iš mechaninio ex post 
y_pot_pl <- ts(hpfilter(L_GDP_PL, freq = 1600)$trend, start = c(1995, 1), frequency = 4)
lines(y_pot_pl, col = "blue")
# Redukuotos formos VAR'o įvertinimas
data_pl22 <- data.frame(with(data_prod, cbind(DL_GFCF_PL, DL_HW_PL, y_gap_pl)))
plot.ts(data_pl22)
# Ištriname eilutes, kuriose trūksta duomenų
data_pl22 <- data_pl22[!is.na(data_pl22$DL_GFCF_PL),];beep()
data_pl22 <- data_pl22[!is.na(data_pl22$DL_HW_PL),];beep()
# data_lv22 <- data_lv22[!is.na(data_lv22$DL_HW_LV),];beep()
plot.ts(data_pl22)
VARselect(data_pl22, lag.max = 8)
var_pl22 <- VAR(data_pl22, p = 5)
summary(var_pl22)
acf(residuals(var_pl22))
# Blanchardo-Quah SVAR'o įvertinimas
bq_svar_pl <- BQ(var_pl22)
bq_svar_pl
plot(irf(bq_svar_pl))
irf(bq_svar_pl, boot = FALSE)
fevd(bq_svar_pl)

# Čekija 

# Vengrija

# Rumunija



###########################################################
###########################################################
### REDUNDANT
###########################################################
###########################################################


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



