####################################
# 4.0 Cobb-Douglas gamybos funkcija
####################################


##############################
#### Kalman filter use example
##############################

## an ARIMA fit
fit3 <- arima(presidents, c(3, 0, 0))
predict(fit3, 12)
## reconstruct this
pr <- KalmanForecast(12, fit3$model)
pr$pred + fit3$coef[4]
sqrt(pr$var * fit3$sigma2)
## and now do it year by year
mod <- fit3$model
for(y in 1:3) {
  pr <- KalmanForecast(4, mod, TRUE)
  print(list(pred = pr$pred + fit3$coef["intercept"], 
             se = sqrt(pr$var * fit3$sigma2)))
  mod <- attr(pr, "mod")
}

###################################################
# Estijos Cobb-Douglas visuminė gamybos funkcija
###################################################

###########################################
###########################################
## 1. Neapribota Cobbo-Douglaso funkcija ##
###########################################
###########################################
# BVP logaritmas
L_GDP_EE <- with(data_prod, ts(L_GDP_EE, start = c(1995, 1), frequency = 4))
# Užimtumo logaritmas
L_HW_EE <- with(data_prod, ts(L_HW_EE, start = c(1995, 1), frequency = 4))
# Kapitalo logaritmas
L_GFCF_EE <- with(data_prod, ts(L_GFCF_EE, start = c(1995, 1), frequency = 4))
# Nestacionarių, bet kointegruotų kintamųjų matrica
data_ee0 <- as.data.frame(cbind(L_GDP_EE, L_HW_EE, L_GFCF_EE))
data_ee0 <- data_ee0[!is.na(data_ee0$L_HW_EE),];beep()
plot.ts(data_ee0)
# Fiktyus krizės kintamasis, 2009 m. prilygintas 1
crisis <- c(rep(0, 59), rep(1, 4), rep(0, 32))
crisis <- as.matrix(crisis)
colnames(crisis) <- c("crisis")
# Johanseno testai
joh_ee_trace0 <- ca.jo(data_ee0, type = "trace", ecdet = "const", 
                       K = 5, spec = "transitory", dumvar = crisis)
summary(joh_lt_trace0)
joh_lt_eigen0 <- ca.jo(data_lt0, type = "eigen", ecdet = "const", 
                       K = 5, spec = "transitory", dumvar = crisis)
summary(joh_lt_eigen0)
############################################
# Paklaidos korekcijos modelio išmatavimas #
# su dviem kointegruojančiais vektoriais,  #
# pagal pėdsako (trace) testo parodymus    #
############################################
vecm_lt0 <- cajorls(joh_lt_trace0, r = 2)
vecm_lt_eq0 <- vecm_lt0$rlm
# Neišsprendžiama autokoreliacija pirmos lygties paklaidose
acf(residuals(vecm_lt_eq0))
summary(vecm_lt_eq0)
round(vecm_lt0$beta, 4)
############################################
# Paklaidos korekcijos modelio išmatavimas #
# su vienu kointegruojančiu vektoriumi,    #
# pagal maksimalios tikrinės reikšmės      #
# (eigen) testo parodymus                  #
############################################
vecm_lt0 <- cajorls(joh_lt_trace0, r = 1)
vecm_lt_eq0 <- vecm_lt0$rlm
# Neišsprendžiama autokoreliacija pirmos lygties paklaidose
acf(residuals(vecm_lt_eq0))
summary(vecm_lt_eq0)
# Neįmanomas paaiškinti ilgalaikis neigiamas užimtumo poveikis gamybai
# nebent tik tuo, kad Lietuvoje po finansinės krizės gamyba augo, 
# esant stagnuojančiam užimtumui
round(vecm_lt0$beta, 4)
# Reziumuojant galima pasakyti, kad Cobbo-Douglaso funkcija
# be pastovios masto grąžos apribojimo nesuderinama su faktine
# šių kintamųjų dinamika tiriamu laikotarpiu.
##########################################
##########################################
## 2. Cobbo-Douglaso gamybos funkcija   ##
## su pastovios masto grąžos apribojimu ##
##########################################
##########################################
# BVP ir užimtumo santykio logaritmas
L_GDP_EE_PW <- ts(L_GDP_EE - L_HW_EE, start = c(1998, 3), frequency = 4)
# Kapitalo ir užimtumo santykio logaritmas
L_GFCF_EE_PW <- ts(L_GFCF_EE - L_HW_EE, start = c(1998, 3), frequency = 4)
# Nestacionarių, tačiau kointegruotų kintamųjų matrica
data_lt <- cbind(L_GDP_EE_PW, L_GFCF_EE_PW)
plot.ts(data_lt)
# Johanseno testai, į kointegruojantį vektorių įtraukiant trendą
joh_lt_trace_trend <- ca.jo(data_lt, type = "trace", ecdet = "trend", 
                            K = 4, spec = "transitory", dumvar = crisis)
summary(joh_lt_trace_trend)
joh_lt_eigen_trend <- ca.jo(data_lt, type = "trace", ecdet = "trend", 
                            K = 4, spec = "transitory", dumvar = crisis)
summary(joh_lt_eigen_trend)
# Paklaidos korekcijos modelio išmatavimas
vecm_lt_trend <- cajorls(joh_lt_trace_trend, r = 1)
vecm_lt_trend_eq <- vecm_lt_trend$rlm
# Paklaidos neautokoreliuotos
acf(residuals(vecm_lt_trend_eq))
# Korekcijos greičio koeficientas reikšmingas tik investicijų lygtyje
summary(vecm_lt_trend_eq)
# Kointegruojančiame vektoriuje išmatuotas BVP elastingumas kapitalui,
# kuris yra ~0.47. Atitinkamai BVP elastingumas užimtumui yra ~0.53
round(vecm_lt_trend$beta, 4)
# Pusiausvyros paklaidos (formulėse trumpintos kaip e) išmatavimas
trend <- 1:length(L_GDP_EE_PW)
eq_error <- cbind(data_lt, trend)%*%vecm_lt_trend$beta
ts.plot(eq_error)
mean(eq_error)
############################################
# Harrodo neutralios technologijos versija #
############################################
# Kintamojo a išmatavimas
a_techn_harrod <- (eq_error - vecm_lt_trend$beta[3]*trend)/
  (1 + vecm_lt_trend$beta[2])
ts.plot(a_techn_harrod)
mean(a_techn_harrod)
exp(a_techn_harrod)
# Technologijos funkcija
harrod_eq <- lm(a_techn_harrod ~ trend)
summary(harrod_eq)
# Technologijos trikdžių AR modelis
res_harrod <- residuals(harrod_eq)
acf(res_harrod)
pacf(res_harrod)
ts.plot(res_harrod)
ar_harrod <- arima(res_harrod, order = c(8, 0, 0), 
                   include.mean = FALSE,
                   fixed = c(NA, rep(0, 6), NA))
coeftest(ar_harrod)
acf(residuals(ar_harrod))
# Technologijos trikdžių stacionarumo tikrinimas
df_harrod <- ur.df(res_harrod, type = "none", lags = 0)
plot(df_harrod)
summary(df_harrod)
summary(ur.pp(res_harrod, type = "Z-tau"))
##########################################
# Solow neutralios technologijos versija #
##########################################
# Kintamojo a išmatavimas
a_techn_solow <- -(eq_error - vecm_lt_trend$beta[3]*trend)/
  vecm_lt_trend$beta[2]
ts.plot(a_techn_solow)
mean(a_techn_solow)
exp(a_techn_solow)
# Technologijos funkcija
solow_eq <- lm(a_techn_solow ~ trend)
summary(solow_eq)
# Technologijos trikdžių AR modelis
res_solow <- residuals(solow_eq)
acf(res_solow)
pacf(res_solow)
ts.plot(res_solow)
ar_solow <- arima(res_solow, order = c(8, 0, 0), 
                  include.mean = FALSE,
                  fixed = c(NA, rep(0, 6), NA))
coeftest(ar_solow)
acf(residuals(ar_solow))
# Technologijos trikdžių stacionarumo tikrinimas
df_solow <- ur.df(res_solow, type = "none", lags = 0)
plot(df_solow)
summary(df_solow)
summary(ur.pp(res_solow, type = "Z-tau"))
###########################################
# Hickso neutralios technologijos versija #
###########################################
# Kintamojo a išmatavimas
a_techn_hicks <- eq_error - vecm_lt_trend$beta[3]*trend
ts.plot(a_techn_hicks)
mean(a_techn_hicks)
exp(a_techn_hicks)
# Technologijos funkcija
hicks_eq <- lm(a_techn_hicks ~ trend)
summary(hicks_eq)
# Technologijos trikdžių AR modelis
res_hicks <- residuals(hicks_eq)
acf(res_hicks)
pacf(res_hicks)
ts.plot(res_hicks)
ar_hicks <- arima(res_hicks, order = c(8, 0, 0), 
                  include.mean = FALSE,
                  fixed = c(NA, rep(0, 6), NA))
coeftest(ar_hicks)
acf(residuals(ar_hicks))
# Technologijos trikdžių stacionarumo tikrinimas
df_hicks <- ur.df(res_hicks, type = "none", lags = 0)
plot(df_hicks)
summary(df_hicks)
summary(ur.pp(res_hicks, type = "Z-tau"))
####################
## 3. ADL modelis ##
####################
DL_GDP_EE_PW <- (DL_GDP_EE - DL_HW_EE)/100
DL_GFCF_EE_PW <- (DL_GFCF_EE - DL_HW_EE)/100
adl_lt <- lm(DL_GFCF_EE_PW ~ L_GDP_EE_PW + L_GFCF_EE_PW + trend + DL_GDP_EE_PW 
             + Lag(DL_GDP_EE_PW, 1) + Lag(DL_GFCF_EE_PW, 1) + Lag(DL_GDP_EE_PW, 2) 
             + Lag(DL_GFCF_EE_PW, 8))
summary(adl_lt)
res_adl_lt <- residuals(adl_lt)
acf(res_adl_lt)

###################################################
# Latvijos Cobb-Douglas visuminė gamybos funkcija
###################################################


###################################################
# Lietuvos Cobb-Douglas visuminė gamybos funkcija
###################################################


###################################################
# Lenkijos Cobb-Douglas visuminė gamybos funkcija
###################################################


###################################################
# Čekijos Cobb-Douglas visuminė gamybos funkcija
###################################################


###################################################
# Vengrijos Cobb-Douglas visuminė gamybos funkcija
###################################################


###################################################
# Rumunijos Cobb-Douglas visuminė gamybos funkcija
###################################################

