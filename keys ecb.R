# ecb keys
library(ecb)
library(ggplot2)
library(lubridate)
# sample key
key <- "ICP.M.LT+PL+U2.N.000000+XEF000.4.ANR" # hicp

# 1 dimensija. Frequency
# [Q] - quarterly

# 2 dimensija. Adjustment indicator
# [Y] - callendar and seasonally adjusted data

# 3 dimensija. Reference area
# [LT+PL+U2] - Lithuania, Poland, Ero area

# 4 dimensija. Counterpart area
# [W2] - Domestic (home or reference area)
# [W1] - Rest of the World

# 5 dimensija. Reference sector
# [S1] - Total economy
# [S13] - General government
# [S1M] - Households and non profit institutions serving households

# 6 dimensija. Counterpart sector
# [S1] - Total economy

# 7 dimensija. Account entries
# [B] - Balance (Credits minus Debits)
# [C] - Credit (resources)
# [D] - Debit (Uses)

# 8 dimensija. Stocks, Transactions, Other Flows
# [B1GQ] - Gross domestic product at market prices
# [P3] - Final consumption expenditure
# [P31] - Indvidual consumption expenditure
# [P51G] - Gross fixed capital formation
# [P6] - Exports of goods and services
# [P7] - Imports of goods and services

# 9 dimensija. Instrument and assets classification
# [N111G] - Dwellings (gross)
# [N112G] - Other buildings and structures (gross)
# [N1131G] - Transport equipment (gross)
# [N1132G] - ICT equipment (gross)
# [N115G] - Cultivated biological resources (gross)
# [N117G] - Intellectual property products (gross)
# [N11G] - Fixed assets by type of asset (gross)
# [N11KG] - Total construction (Buildings and structures) (gross)
# [N11MG] - Machinery and equipment and weapons systems (gross)
# [N11OG] - Other machinery and equipment and weapons systems (gross)
# [_Z] - Non applicable

# 10 dimensija. Activity classification
# [_T] - Total - Allactivities
# [_Z] - Non applicable

# 11 dimensija. Expenditure (COFOG, COICOP, COPP or COPNI)
# [_T] - Total
# [_Z] - Non applicable

# 12 dimensija. Unit of measure
# [EUR] - Euro
# [XDC] - Domestic currency

# 13 dimensija. Prices
# [LR] - Chain linked VOLUME (rebased)
# [V] - Current prices

# 14 dimensija. Transformation
# [N] - Non transformed data
# [G1] - Growth rate, period on period
# [GY] - Growth rate, over 1 year


# <Lietuva>

# <Lietuvos raktai>
# Lietuvos pajamos ir gamyba (Priklausomi kintamieji)
keyLT_GDP <- "MNA.Q.Y.LT.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.N"
keyLT_GVA <- "MNA.Q.Y.LT.W2.S1.S1.B.B1G._Z.F._Z.EUR_R_B1GQ.V.N" # Gross value added

# Lietuvos darbas (nepriklausomi kintamieji)
# keyLT_LF <- " " # Lietuvos darbo jėga
keyLT_TEMPLh <- "MNA.Q.Y.LT.W2.S1.S1._Z.EMP._Z._T._Z.HW._Z.N" # Total employemnt, hours worked
keyLT_TEMPLp <- "MNA.Q.Y.LT.W2.S1.S1._Z.EMP._Z._T._Z.PS._Z.N" # Total employment, persons
keyLT_EMPLh <- "ENA.Q.Y.LT.W2.S1.S1._Z.SAL._Z._T._Z.HW._Z.N" # Employees, hours worked
keyLT_EMPLp <- "ENA.Q.Y.LT.W2.S1.S1._Z.SAL._Z._T._Z.PS._Z.N" # Employees, persons
keyLT_LPh <- "MNA.Q.Y.LT.W0.S1.S1._Z.LPR_HW._Z._T._Z.XDC.LR.N" # Labour productivity (per hours worked)
keyLT_LPp <- "MNA.Q.Y.LT.W0.S1.S1._Z.LPR_PS._Z._T._Z.XDC.LR.N" # Labour productivity (per persons)
keyLT_LHC <- "MNA.Q.Y.LT.W2.S1.S1._Z.COM_HW._Z._T._Z.XDC.V.N" # Hourly compensation
keyLT_LCe <- "MNA.Q.Y.LT.W2.S1.S1._Z.COM_PS._Z._T._Z.XDC.V.N" # Compensation per employee
keyLT_CEa <- "MNA.A.N.LT.W2.S1.S1.D.D1._Z._T._Z.EUR.V.N" # Compensation of employees, annual

# Lietuvos kapitalas (nepriklausomi kintamieji)
# visas
keyLT_K <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11G._T._Z.XDC.V.N" # GFCF Fixed assets by type

# Lietuvos išskaidytas kapitalas (nepriklausomi kintamiji)
keyLT_K01 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N111G._T._Z.XDC.V.N" # GFCF Dwellings
keyLT_K02 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N112G._T._Z.XDC.V.N" # GFCF Other buildings and structures
keyLT_K03 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N1131G._T._Z.XDC.V.N" # GFCF Transport equipment
keyLT_K04 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N1132G._T._Z.XDC.V.N" # GFCF ICT equipment
keyLT_K05 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N115G._T._Z.XDC.V.N" # GFCF Cultivated biological resources
keyLT_K06 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N117G._T._Z.XDC.V.N" # GFCF Intellectual property
keyLT_K07 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11KG._T._Z.XDC.V.N" # GFCF Total construction (Buildings and structures)
keyLT_K08 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11MG._T._Z.XDC.V.N" # GFCF Machinery and equipment and weapons systems
keyLT_K09 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11OG._T._Z.XDC.V.N" # GFCF Other machinery and equipment and weapons systems
# keyLT_ <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11MG._T._Z.XDC.V.N" # Gand equipment and weapons systems

# Papildomi (egzogeniniai) Lietuvos kintamieji
# Lietuvos eksportas

# Lietuvos importas

# </Lietuvos raktai>

# <ecb paketui reikalingi parametrai>
# Lietuvos laikotarpio filtras
filterLT <- list(lastNObservations = 93, detail = "full") # Lietuvos duomenys ilgesniam laikotarpiui. Nuo 1995Q1 iki 2018Q1 imtinai.
# </ecb paketui reikalingi paraetrai>

# <Lietuvos duomenų išgavimas>
# Lietuvos gamyba ir pajamos
GDP_LT <- get_data(keyLT_GDP, filterLT)[c(24,25)] # GDP
GVA_LT <- get_data(keyLT_GVA, filterLT)[c(21,22)] # GVA
# gdp <- ts(log(GDP_LT), start = c(1998, 2), frequency = 4)

# Lietuvos darbas
# LF_LT <- get_data(keyLT_LF, filterLT)

#neveikia LTEMPLh_LT <- get_data(keyLT_TEMPLh, filterLT) # Total employemnt, hours worked
#neveikia LTEMPLp_LT <- get_data(keyLT_TEMPLp, filterLT)  # Total employment, persons
LEMPLh_LT <- get_data(keyLT_EMPLh, filterLT)[c(22,23)] # Employees, hours worked
LEMPLp_LT <- get_data(keyLT_EMPLp, filterLT)[c(22,23)] # Employees, persons
LHC_LT <- get_data(keyLT_LHC, filterLT)[c(23,24)] # Hourly compensation
LCe_LT <- get_data(keyLT_LCe, filterLT)[c(23,24)] # Compensation per employee
LPh_LT <- get_data(keyLT_LPh, filterLT)[c(24,25)]  # Labour productivity (per hours worked)
LPp_LT <- get_data(keyLT_LPp, filterLT)[c(24,25)] # Labour productivity (per persons)
LCEMPLa_LT <- get_data(keyLT_CEa, filterLT)[c(22,23)] # Compensation of employees, annual


# Lietuvos kapitalas
K_LT <- get_data(keyLT_K, filterLT)[c(23,24)]
KDW_LT <- get_data(keyLT_K01, filterLT)[c(23,24)] # GFCF Dwellings
KBS_LT <- get_data(keyLT_K02, filterLT)[c(23,24)] # GFCF Other buildings and structures
KTR_LT <- get_data(keyLT_K03, filterLT)[c(23,24)] # GFCF Transport equipment
KIT_LT <- get_data(keyLT_K04, filterLT)[c(23,24)] # GFCF ICT equipment
KBR_LT <- get_data(keyLT_K05, filterLT)[c(23,24)] # GFCF Cultivated biological resources
KIP_LT <- get_data(keyLT_K06, filterLT)[c(23,24)] # GFCF Intellectual property
KTC_LT <- get_data(keyLT_K07, filterLT)[c(22,23)] # GFCF Total construction (Buildings and structures)
KWM_LT <- get_data(keyLT_K08, filterLT)[c(23,24)] # GFCF Machinery and equipment and weapons systems
KWO_LT <- get_data(keyLT_K09, filterLT)[c(23,24)] # GFCF Other machinery and equipment and weapons systems


# </Lietuvos duomenų išgavimas>
# <Lietuvos duomenų susistemizavimas>
# Pervadiname stulpelius
colnames(GDP_LT) <- c("date", "GDP_LT")
colnames(GVA_LT) <- c("date", "GVA_LT")
colnames(K_LT) <- c("date", "K_LT")
colnames(KBR_LT) <- c("date", "KBR_LT")
colnames(KBS_LT) <- c("date", "KBS_LT")
colnames(KDW_LT) <- c("date", "KDW_LT")
colnames(KIP_LT) <- c("date", "KIP_LT")
colnames(KIT_LT) <- c("date", "KIT_LT")
colnames(KTC_LT) <- c("date", "KTC_LT")
colnames(KWM_LT) <- c("date", "KTR_LT")
colnames(KWO_LT) <- c("date", "KWO_LT")
colnames(LCe_LT) <- c("date", "LCe_LT")
colnames(LCEMPLa_LT) <- c("date", "LCEMPLa")
colnames(LEMPLh_LT) <- c("date", "LEMPLh_LT")
colnames(LEMPLp_LT) <- c("date", "LEMPLp_LT")
colnames(LHC_LT) <- c("date", "LHC_LT")
colnames(LPh_LT) <- c("date", "LPh_LT")
colnames(LPp_LT) <- c("date", "LPp_LT")

# Apjungiam kintamuosius į vieną lentelę
# išskyrus LCEMPLa_LT , nes jame metiniai duomenys
dataLT<- merge(GDP_LT[,1:2],GVA_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:3],K_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:4],KBR_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:5],KBS_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:6],KDW_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:7],KIP_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:8],KTC_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:9],KTR_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:10],KWM_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:11],KWO_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:12],LCe_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:13],LEMPLh_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:14],LEMPLp_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:15],LHC_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:16],LPh_LT[,1:2],by=1,all=TRUE)
dataLT<- merge(dataLT[,1:17],LPp_LT[,1:2],by=1,all=TRUE)

# Datos transformavimas iš YYYY-QQ į YYYY-MM-DD
dataLT$date<-parse_date_time(dataLT$date, "y q")
dataLT
plot(dataLT$K_LT)
plot(dataLT$date, dataLT$GVA_LT)
ts.plot(dataLT)

# Lietuvos duomenų įrašymas į lentelę
write.table(dataLT, "dataLT.csv")
write.table(dataLT, "dataLT.xls")
write.table(dataLT, "dataLT")
write.csv(dataLT, "dataLT.csv")

# </Lietuvos duomenų susistemizavimas>
# </Lietuva>



# <Lenkija>
# Lenkijos pajamos ir gamyba (Priklausomi kintamieji)
keyPL_GDP <- "MNA.Q.Y.PL.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.N"
keyPL_VA <- "MNA.Q.Y.PL.W2.S1.S1.B.B1G._Z.F._Z.EUR_R_B1GQ.V.N" # Gross value added

# Lenkijos darbas (nepriklausomi kintamieji)
keyPL_LF <- " " # Lenkijos darbo jėga
keyPL_LPh <- "MNA.Q.Y.PL.W0.S1.S1._Z.LPR_HW._Z._T._Z.XDC.LR.N" # Labour productivity (per hours worked)
keyPL_HC <- "MNA.Q.Y.PL.W2.S1.S1._Z.COM_HW._Z._T._Z.XDC.V.N" # Hourly compensation
keyPL_CEq <- "MNA.Q.N.PL.W2.S1.S1.D.D1._Z._T._Z.XDC.V.N" # Compensation of employees, quarterly
keyPL_TEMPLh <- "MNA.Q.Y.PL.W2.S1.S1._Z.EMP._Z._T._Z.HW._Z.N" # Total employment, hours worked
keyPL_EMPLh <- "ENA.Q.Y.PL.W2.S1.S1._Z.SAL._Z._T._Z.HW._Z.N" # Employees, hourw worked
# Lenkijos kapitalas (nepriklausomi kintamieji)
# visas
keyPL_K <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N11G._T._Z.XDC.V.N" # GFCF Fixed assets by type

# Lenkijos išskaidytas kapitalas (nepriklausomi kintamiji)
keyPL_K01 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N111G._T._Z.XDC.V.N" # GFCF Dwellings
keyPL_K02 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N112G._T._Z.XDC.V.N" # GFCF Other buildings and structures
keyPL_K03 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N1131G._T._Z.XDC.V.N" # GFCF Transport equipment
keyPL_K04 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N1132G._T._Z.XDC.V.N" # GFCF ICT equipment
keyPL_K05 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N115G._T._Z.XDC.V.N" # GFCF Cultivated biological resources
keyPL_K06 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N117G._T._Z.XDC.V.N" # GFCF Intellectual property
keyPL_K07 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N11KG._T._Z.XDC.V.N" # GFCF Total construction (Buildings and structures)
keyPL_K08 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N11MG._T._Z.XDC.V.N" # GFCF Machinery and equipment and weapons systems
keyPL_K09 <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N11OG._T._Z.XDC.V.N" # GFCF Other machinery and equipment and weapons systems
keyPL_ <- "MNA.Q.Y.PL.W0.S1.S1.D.P51G.N11MG._T._Z.XDC.V.N" # GFCF Machinery and equipment and weapons systems

# Papildomi (egzogeniniai) Lietuvos kintamieji
# Lenkijos eksportas

# Lenkijos importas

# ecb paketo parametrai
# Lenkijos laikotarpio filtras
filterPL <- list(lastNObservations = 64, detail = "full") # Lenkijos duomenys trumpesni. Nuo 2002Q1 iki 2017Q4 imtinai.

# Lenkijos duomenų išgavimas
# Lenkijos gamyba ir pajamos
GDP_PL <- get_data(keyPL_1, filterPL)
GVA_PL <- get_data(key, filterPL)

# Lenkijos darbas


# <Lenkijos kapitalas>
finconsLT <- get_data(keyLT_2, filterLT)


gdpPl <- get_data(keyPL_1, filterPL)
# </Lenkijos duomenų išgavimas>
# </Lenkija>



# Duomenų atvaizdavimas su ggplot2

GVA <- convert_dates(GVA_LT$obstime)
GDP_LT <- convert_dates(GDP_LT$obstime)

ggplot(GDP_LT, aes(x = obstime, y = obsvalue, color = title)) +
  geom_line() +
  facet_wrap(~ref_area, ncol = 3) +
  theme_bw(8) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Percent per annum\n", color = NULL,
       title = "Lietuvos BVP")


library(ecb)
library(ggplot2)

key <- "ICP.M.DE+FR+ES+IT+NL+U2.N.000000+XEF000.4.ANR"
filter <- list(lastNObservations = 12, detail = "full")

hicp <- get_data(key, filter)

hicp$obstime <- convert_dates(hicp$obstime)

ggplot(hicp, aes(x = obstime, y = obsvalue, color = title)) +
  geom_line() +
  facet_wrap(~ref_area, ncol = 3) +
  theme_bw(8) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Percent per annum\n", color = NULL,
       title = "HICP - headline and core\n")

