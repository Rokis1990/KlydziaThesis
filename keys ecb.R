# ecb keys
library(ecb)
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
keyLT_LF <- " " # Lietuvos darbo jėga
keyLT_LPh <- "MNA.Q.Y.LT.W0.S1.S1._Z.LPR_HW._Z._T._Z.XDC.LR.N" # Labour productivity (per hours worked)
keyLT_LPp <- "MNA.Q.Y.LT.W0.S1.S1._Z.LPR_PS._Z._T._Z.XDC.LR.N" # Labour productivity (per persons)
keyLT_LHC <- "MNA.Q.Y.LT.W2.S1.S1._Z.COM_HW._Z._T._Z.XDC.V.N" # Hourly compensation
keyLT_LHCe <- "MNA.Q.Y.LT.W2.S1.S1._Z.COM_PS._Z._T._Z.XDC.V.N" # Compensation per employee


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
keyLT_ <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11MG._T._Z.XDC.V.N" # GFCF Machinery and equipment and weapons systems

# Papildomi (egzogeniniai) Lietuvos kintamieji
# Lietuvos eksportas

# Lietuvos importas

# </Lietuvos raktai>

# <ecb paketo parametrai>
# Lietuvos laikotarpio filtras
filterLT <- list(lastNObservations = 93, detail = "full") # Lietuvos duomenys ilgesniam laikotarpiui. Nuo 1995Q1 iki 2018Q1 imtinai.
# </ecb paketo paraetrai>

# <Lietuvos duomenų išgavimas>
# Lietuvos gamyba ir pajamos
GDP_LT <- get_data(keyLT_GDP, filterLT) # GDP
GVA_LT <- get_data(keyLT_GVA, filterLT) # GVA

# Lietuvos darbas
LF_LT <- get_data()
LHC_LT <- get_data()
LHCe_LT <- get_data()
LPh_LT <- get_data()
LPp_LT <- get_data()


# Lietuvos kapitalas
K_LT <- get_data(keyLT_K, filterLT)
KDW_LT <- get_data(keyLT_K01, filterLT) # GFCF Dwellings
KBS_LT <- get_data(keyLT_K02, filterLT) # GFCF Other buildings and structures
KTR_LT <- get_data(keyLT_K03, filterLT) # GFCF Transport equipment
KIT_LT <- get_data(keyLT_K04, filterLT) # GFCF ICT equipment
KBR_LT <- get_data(keyLT_K05, filterLT) # GFCF Cultivated biological resources
KIP_LT <- get_data(keyLT_K06, filterLT) # GFCF Intellectual property
KTC_LT <- get_data(keyLT_K07, filterLT) # GFCF Total construction (Buildings and structures)
KWM_LT <- get_data(keyLT_K08, filterLT) # GFCF Machinery and equipment and weapons systems
KWO_LT <- get_data(keyLT_K09, filterLT) # GFCF Other machinery and equipment and weapons systems

finconsLT <- get_data(keyLT_2, filterLT)

# </Lietuvos duomenų išgavimas>
# </Lietuva>



# <Lenkija>
# Lenkijos pajamos ir gamyba (Priklausomi kintamieji)
keyPL_GDP <- "MNA.Q.Y.PL.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.N"
keyPL_VA <- "MNA.Q.Y.PL.W2.S1.S1.B.B1G._Z.F._Z.EUR_R_B1GQ.V.N" # Gross value added

# Lenkijos darbas (nepriklausomi kintamieji)
keyPL_LF <- " " # Lenkijos darbo jėga
keyPL_LPh <- "MNA.Q.Y.PL.W0.S1.S1._Z.LPR_HW._Z._T._Z.XDC.LR.N" # Labour productivity (per hours worked)
keyPL_HC <- "MNA.Q.Y.PL.W2.S1.S1._Z.COM_HW._Z._T._Z.XDC.V.N" # Hourly compensation

# Lenkijos kapitalas (nepriklausomi kintamieji)
# visas
keyPL_K <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11G._T._Z.XDC.V.N" # GFCF Fixed assets by type

# Lenkijos išskaidytas kapitalas (nepriklausomi kintamiji)
keyPL_K01 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N111G._T._Z.XDC.V.N" # GFCF Dwellings
keyPL_K02 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N112G._T._Z.XDC.V.N" # GFCF Other buildings and structures
keyPL_K03 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N1131G._T._Z.XDC.V.N" # GFCF Transport equipment
keyPL_K04 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N1132G._T._Z.XDC.V.N" # GFCF ICT equipment
keyPL_K05 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N115G._T._Z.XDC.V.N" # GFCF Cultivated biological resources
keyPL_K06 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N117G._T._Z.XDC.V.N" # GFCF Intellectual property
keyPL_K07 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11KG._T._Z.XDC.V.N" # GFCF Total construction (Buildings and structures)
keyPL_K08 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11MG._T._Z.XDC.V.N" # GFCF Machinery and equipment and weapons systems
keyPL_K09 <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11OG._T._Z.XDC.V.N" # GFCF Other machinery and equipment and weapons systems
keyPL_ <- "MNA.Q.Y.LT.W0.S1.S1.D.P51G.N11MG._T._Z.XDC.V.N" # GFCF Machinery and equipment and weapons systems

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

