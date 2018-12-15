# Užkrauname paketus, reikalingus duomenų parsisiuntimui iš Eurostat ir jų transformavimui
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, beepr, openxlsx, Hmisc, tabplot, data.table, tidyverse, corrplot, bit64, eurostat, xts, tidyr, mFilter, vars, lubridate)

##########
# Duomenys
##########

# Atsisiunčiame ketvirtinius BVP bei pagrindinių jo komponentų duomenis
# GDP and main components (output, expenditure and income) (namq_10_gdp) 	
NAMQ_10_GDP <- get_eurostat("namq_10_gdp")

# Atsisiunčiame ketvirtinius pridėtinės vertės ir pajamų duomenis EVRK (NACE) klasifikacijos pjūviu
# Gross value added and income A*10 industry breakdowns (namq_10_a10) 
NAMQ_10_A10 <- get_eurostat("namq_10_a10")

# Atsisiunčiame ketvirtinius darbo jėgos apklausos (tyrimo) duomenis pagal išsilavinimo lygį 
# (Employment by educational attainment level - quarterly data)
LFSI_EDUC_Q <- get_eurostat("lfsi_educ_q")

# Atsisiunčiame ketvirtinius darbo jėgos duomenis EVRK (NACE) klasifikacijos pjūviu
# Employment A*10 industry breakdowns (namq_10_a10_e)
NAMQ_10_A10_E <- get_eurostat("namq_10_a10_e")

# Atsisiunčiame ketvirtinius kapitalo duomenis ilgalaikio turto rūšių pjūviu
# Gross fixed capital formation with AN_F6 asset breakdowns (namq_10_an6)
NAMQ_10_AN6 <- get_eurostat("namq_10_an6")

# Atsisiunčiame ketvirtinius tarptautinės prekybos duomenis
# Exports and imports by Member States of the EU/third countries (namq_10_exi)
NAMQ_10_EXI <- get_eurostat("namq_10_exi")

############
# Dimensijos
############

# Pagal šias dimesijas kursime filtrus

# BVP duomenų pagr. dimensijos
unique(NAMQ_10_GDP$s_adj)
unique(NAMQ_10_GDP$unit)
unique(NAMQ_10_GDP$na_item)

# Pridėtinės vertės ir pajamų duomenų pagr. dimensijos
unique(NAMQ_10_A10$na_item)
unique(NAMQ_10_A10$s_adj)
unique(NAMQ_10_A10$nace_r2)
unique(NAMQ_10_A10$unit)

# Darbo jėgos duomenų (apklausos duomenų) pagr. dimensijos
unique(LFSI_EDUC_Q$unit)
unique(LFSI_EDUC_Q$isced11)
unique(LFSI_EDUC_Q$s_adj)

# Darbo jėgos duomenų (pagal EVRK - NACE) pagr. dimensijos
unique(NAMQ_10_A10_E$unit)
unique(NAMQ_10_A10_E$nace_r2)
unique(NAMQ_10_A10_E$s_adj)
unique(NAMQ_10_A10_E$na_item)

# Kapitalo duomenų pagr. dimensijos
unique(NAMQ_10_AN6$unit)
unique(NAMQ_10_AN6$asset10)
unique(NAMQ_10_AN6$s_adj)

# Tarptautinės prekybos duomenų pagr. dimensijos
unique(NAMQ_10_EXI$unit)
unique(NAMQ_10_EXI$s_adj)
unique(NAMQ_10_EXI$na_item)


#########
# Filtrai
#########

# Bendrasis vidaus produktas (BVP)

# Duomenų filtro parametrai:
# Chain linked volumes 2010=100
# Seasonally and callendar adjusted
# NA items:
# B1GQ - BVP
# B1G - Pridėtinė vertė
# P5G - Grynojo kapitalo formavimasis
# P51G - Grynojo ilgalaikio kapitalo formavimasis
# P6 - Prekių ir paslaugų eksportas 
# P61 - Prekių eksportas
# P62 - Paslaugų eksportas
# P7 - Prekių ir paslaugų importas
# P71 - Prekių importas
# P72 - Paslaugų importas
# D1 - Darbuotojų atlygis
# D11 - Darbo užmokestis
# D12 - Darbdavio mokamos socialinio draudimo įmokos
# B2A3G - Grynosios mišrios pajamos ir 


NAMQ_10_GDP <- NAMQ_10_GDP %>% filter(
  unit=="CLV10_MNAC",
  s_adj=="SCA",
  na_item %in% c("B1GQ","B1G","P5G","p51G","P6","P61","P62","P7","P71","P72","D1","D11","D12","B2A3G"),
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO")
)

# Pridėtinė vertė

# Žemiau esantis duomenų filtras atrenka duomenis šiais pjūviais: 
# vienetai - Chain linked volumes 2010=100
# Sezoniškumo korekcija - Seasonally corrected
# EVRK klasifikacija - visos šakos (sektoriai)
# Nacionalinės sąskaitos vienetas - B1G (pridėtinė vertė), D1 (darbuotojų atlygis)
NAMQ_10_A10 <- NAMQ_10_A10 %>% filter(
  unit=="CLV10_MNAC",
  s_adj=="SCA",
  nace_r2=="TOTAL",
  na_item=="B1G",
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO")
)


# Darbo jėga iš Darbo jėgos apklausos (tyrimo)

# Duomenų filtras atrenka:
# Amžius - nuo 15 iki 64 metų
# Vienetai - tūkst. asmenų
# Lytys - suma
# Nusezoninti duomenys
LFSI_EDUC_Q <- LFSI_EDUC_Q %>% filter(
  age=="Y15-64",
  unit=="THS_PER",
  s_adj=="SA",
  sex=="T",
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO")
)


# Darbo jėga pagal EVRK (NACE) klasifikatorių

# Duomenų filtras atrenka:
# vienetai - THS_HW (tūkst. nudirbtų valandų), THS_PER (tūkst. darbuotojų), THS_JOB (tūkst. darbo vietų)
# nusezoninti ir pagal kalendorių pakoreguoti duomenys
# Nacionalinių sąskaitų vienetai - EMP_DC (Visas užimtumas), SAL_DC (Darbuotojai)
NAMQ_10_A10_E <- NAMQ_10_A10_E %>% filter(
  unit == "THS_HW",
  s_adj=="SCA",
  na_item == "EMP_DC",
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO"),
  nace_r2=="TOTAL"
)


# Kapitalas pagal ilgalaikio turto rūšis

# Duomenų filtras atrenka:
# vienetai - Chain linked volumes 2010
# Ilgalaikio turto rūšis - visas ilgalaikis turtas
# Nusezoninimas - nusezoninti ir pagal kalendorių pakoreguoti duomenys

NAMQ_10_AN6 <- NAMQ_10_AN6 %>% filter(
  unit=="CLV10_MNAC",
  s_adj=="SCA",
  asset10=="N11G",
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO")
)


# Tarptautinė prekyba

# Duomenų filtras atrenka:
# vienetai - Chain linked volumes 2010 nacionaline valiuta
# nusezoninti ir pagal kalendorių pakoreguoti duomenys
# nacionalinė sąskaita - P6 (Prekiių ir paslaugų eksportas), P7 (Prekių ir paslaugų importas)

NAMQ_10_EXI <- NAMQ_10_EXI %>% filter(
  unit=="CLV10_MNAC",
  s_adj=="SCA",
  na_item%in%c("P6","P7"),
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO")
)


#########################
# Duomenų transformavimas
#########################

esLFSI_EDU_Q <- spread(LFSI_EDUC_Q, geo, values)

esNAMQ_10_A10 <- spread(NAMQ_10_A10, geo, values)

esNAMQ_10_A10_E <- spread(NAMQ_10_A10_E, geo, values)

esNAMQ_10_AN6 <- spread(NAMQ_10_AN6, geo, values)

esNAMQ_10_EXI <- spread(NAMQ_10_EXI, geo, values)

esNAMQ_10_GDP <- spread(NAMQ_10_GDP, geo, values)




#esLFSI_EDU_Qtemp <- spread(esLFSI_EDU_Q,)
#esLFSI_EDU_Q <- LFSI_EDUC_Q %>% group_by(geo) %>% dplyr::summarise(, LT=LT, PL=PL, CZ=CZ, HU=HU, RO=RO)

#esNAMQ_10_A10 <- NAMQ_10_A10 %>% dplyr::summarise()

#esNAMQ_10_A10_E <- NAMQ_10_A10_E %>% dplyr::summarise()

#esNAMQ_10_AN6 <- NAMQ_10_AN6 %>% dplyr::summarise()

#esNAMQ_10_EXI <- NAMQ_10_EXI %>% dplyr::summarise()

#esNAMQ_10_GDP <- NAMQ_10_GDP %>% dplyr::summarise()


##################
# Duomenų lentelės
##################

###########
# Kapitalas
###########

#data_GFCF <- data.frame()
data_GFCF <- as.data.frame(esNAMQ_10_A10$LT)
data_GFCF$GFCF_LV <- esNAMQ_10_AN6$LV
data_GFCF$GFCF_EE <- esNAMQ_10_AN6$EE
data_GFCF$GFCF_PL <- esNAMQ_10_AN6$PL
data_GFCF$GFCF_CZ <- esNAMQ_10_AN6$CZ
data_GFCF$GFCF_HU <- esNAMQ_10_AN6$HU
data_GFCF$GFCF_RO <- esNAMQ_10_AN6$RO 
#data_GFCF$time <- esNAMQ_10_AN6$time # pridedame datos stulpelį

# Logaritmuojame GFCF duomenis
data_L_GFCF <- as.data.frame(lapply(data_GFCF, log))

# Apjungiame GFCF su logaritmuotais GFCF
data_K <- cbind(data_GFCF, data_L_GFCF)

# Diferencijuojame logaritmuotus GFCF duomenis
data_DL_GFCF <- as.data.frame(lapply(data_L_GFCF, diff))

# Pervadiname kapitalo duomenų stulpelius
names(data_DL_GFCF) <- c("DL_GFCF_LT","DL_GFCF_LV","DL_GFCF_EE","DL_GFCF_PL","DL_GFCF_CZ","DL_GFCF_HU","DL_GFCF_RO")
names(data_K) <- c("GFCF_LT","GFCF_LV","GFCF_EE","GFCF_PL","GFCF_CZ","GFCF_HU","GFCF_RO","L_GFCF_LT","L_GFCF_LV","L_GFCF_EE","L_GFCF_PL","L_GFCF_CZ","L_GFCF_HU","L_GFCF_RO")

# Sukuriame trumpesnį viena eilute laiko (datos) kintamąjį diferencijuotiems duomenims  
time93 <- esNAMQ_10_AN6[-nrow(esNAMQ_10_AN6),] 
data_K$time <- esNAMQ_10_AN6$time # pridedame datos stulpelį
data_DL_GFCF$time <- time93$time # pridedame trumpesnį datos stulpelį

# Apjungiame GFCF ir L_GFCF su DL_GFCF
data_K <- left_join(data_K, data_DL_GFCF, by="time")


#require(reshape2)
#data_a$id <- rownames(data_a) 
#melt(data_a)

#####
# BVP
#####

# Sukuriame BVP duomenų objektą
data_GDP <- data.frame()

# sukuriame BVP laiko (datos) duomenų objektą
time_GDP <- as.data.frame(unique(esNAMQ_10_GDP$time))

# Prijungiame 7 šalių BVP duomenis
data_GDP <- as.data.frame((esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$LT)
data_GDP$GDP_LV <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$LV
data_GDP$GDP_EE <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$EE
data_GDP$GDP_PL <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$PL
data_GDP$GDP_CZ <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$CZ
data_GDP$GDP_HU <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$HU
data_GDP$GDP_RO <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$RO

# Logaritmuojame BVP
data_L_GDP <- as.data.frame(lapply(data_GDP, log))

# Diferencijuojame logaritmuotus BVP duomenis
data_DL_GDP <- as.data.frame(lapply(data_L_GDP, diff))

# Prijungiame laiko (datos) stulpelį prie BVP duomenų
data_GDP$time <- time_GDP$`unique(esNAMQ_10_GDP$time)`

# Prijungiame laiko (datos) stulpelį prie log(BVP) duomenų
data_L_GDP$time <- time_GDP$`unique(esNAMQ_10_GDP$time)`

# Sukuriame laiko duomenų objektą, trumpesnį viena eilute, nei yra BVP matavimų 
time94 <- esNAMQ_10_AN6[-nrow(esNAMQ_10_AN6),] 

# Prijungiame lako (datos) stulpelį prie diferencijuotų ir logaritmuotų BVP duomenų
data_DL_GDP$time <- time94$time

# Pervadiname BVP duomenų stulpelius
names(data_GDP) <- c("GDP_LT","GDP_LV","GDP_EE","GDP_PL","GDP_CZ","GDP_HU","GDP_RO","time")
names(data_L_GDP) <- c("L_GDP_LT","L_GDP_LV","L_GDP_EE","L_GDP_PL","L_GDP_CZ","L_GDP_HU","L_GDP_RO","time")
names(data_DL_GDP) <- c("DL_GDP_LT","DL_GDP_LV","DL_GDP_EE","DL_GDP_PL","DL_GDP_CZ","DL_GDP_HU","DL_GDP_RO", "time")

# Apjungiame BVP ir log(BVP) duomenis
data_Y <- left_join(data_GDP, data_L_GDP, by="time")

# Apjungiame BVP, log(BVP) ir diff(log(BVP))
data_Y <- left_join(data_Y, data_DL_GDP, by="time")

#####################
# Darbo jėga
#####################

#####
# Nudirbtos valandos, visi dirbantieji (ne tik samdomi darbuotojai)
#####

data_HW <- as.data.frame(esNAMQ_10_A10_E$LT)
data_HW$HW_LV <- esNAMQ_10_A10_E$LV
data_HW$HW_EE <- esNAMQ_10_A10_E$EE
data_HW$HW_PL <- esNAMQ_10_A10_E$PL
data_HW$HW_CZ <- esNAMQ_10_A10_E$CZ
data_HW$HW_HU <- esNAMQ_10_A10_E$HU
data_HW$HW_RO <- esNAMQ_10_A10_E$RO

data_L_HW <- as.data.frame(lapply(data_HW, log))

data_DL_HW <- as.data.frame(lapply(data_L_HW, diff))

data_HW$time <- esNAMQ_10_A10_E$time

data_L_HW$time <- esNAMQ_10_A10_E$time

data_DL_HW$time <- time93$time

data_L <- left_join(data_HW, data_L_HW, by="time")

data_L <- left_join(data_L, data_DL_HW, by="time")

names(data_L) <- c("HW_LT","HW_LV","HW_EE","HW_PL","HW_CZ","HW_HU","HW_RO", "time","L_HW_LT","L_HW_LV","L_HW_EE","L_HW_PL","L_HW_CZ","L_HW_HU","L_HW_RO","DL_HW_LT","DL_HW_LV","DL_HW_EE","DL_HW_PL","DL_HW_CZ","DL_HW_HU","DL_HW_RO")

#####################################
# Duomenų lentelių apjungimas
#####################################


data_prod <- dplyr::left_join(data_Y, data_K, by = "time")
data_prod <- dplyr::left_join(data_prod, data_L, by="time")

boxplot(data_prod)
mosaicplot(data_prod$GDP_CZ ~ data_prod$GFCF_CZ)
mosaicplot(data_prod$GDP_LT ~ data_prod$HW_LT)

par(mfrow=c(1,2))
plot(data_prod$GDP_LT, data_prod$GFCF_LT)
title("LT BVP")
plot(data_prod$GDP_LT, data_prod$GFCF_LT, log="xy")
title("LT BVP log-log skalė")

####
# Skiriasi eiluciu skaicius - LFS 84 eilutes
####

# LFS darbo jėgos duomenys - skiriasi dimensijos
data_LF <- 
  data_LF$LF02_LT <- (esLFSI_EDU_Q %>% dplyr::filter(
    isced11=="ED0-2"
  ))$LT
data_LF$LF02_LV <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$LV
data_LF$LF02_EE <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$EE
data_LF$LF02_PL <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$PL
data_LF$LF02_CZ <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$CZ
data_LF$LF02_HU <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$HU
data_LF$LF02_RO <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$RO
######
# Darbo jegos kintamasis is NAMQ
######
data_HW$HW_LT <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$LT
data_HW$HW_LV <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$LV
data_HW$HW_EE <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$EE
data_HW$HW_PL <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$PL
data_HW$HW_CZ <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$CZ
data_HW$HW_HU <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$HU
data_HW$HW_RO <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$RO
