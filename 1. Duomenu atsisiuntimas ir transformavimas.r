# Užkrauname paketus, reikalingus duomenų parsisiuntimui iš Eurostat ir jų transformavimui
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, beepr, openxlsx, Hmisc, tabplot, data.table, tidyverse, corrplot, bit64, eurostat, xts, tidyr)


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
  unit %in% c("THS_HW","THS_PER","THS_JOB"),
  s_adj=="SCA",
  nace_r2=="TOTAL",
  na_item %in% c("EMP_DC","SAL_DC"),
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO")
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

# tidyr
tidyr::spread()
tidyr::gather()
tidyr::expand()
tidyr::separate()
tidyr::unite()



esLFSI_EDU_Q <- spread(LFSI_EDUC_Q, geo, values)

esNAMQ_10_A10 <- spread(NAMQ_10_A10, geo, values)

esNAMQ_10_A10_E <- spread(NAMQ_10_A10_E, geo, values)

esNAMQ_10_AN6 <- spread(NAMQ_10_AN6, geo, values)

esNAMQ_10_EXI <- spread(NAMQ_10_EXI, geo, values)

esNAMQ_10_GDP <- spread(NAMQ_10_GDP, geo, values)




#esLFSI_EDU_Qtemp <- spread(esLFSI_EDU_Q,)
#esLFSI_EDU_Q <- LFSI_EDUC_Q %>% group_by(geo) %>% dplyr::summarise(, LT=LT, PL=PL, CZ=CZ, HU=HU, RO=RO)

esNAMQ_10_A10 <- NAMQ_10_A10 %>% dplyr::summarise()

esNAMQ_10_A10_E <- NAMQ_10_A10_E %>% dplyr::summarise()

esNAMQ_10_AN6 <- NAMQ_10_AN6 %>% dplyr::summarise()

esNAMQ_10_EXI <- NAMQ_10_EXI %>% dplyr::summarise()

esNAMQ_10_GDP <- NAMQ_10_GDP %>% dplyr::summarise()


##################
# Duomenų lentelės
##################

data <- as.data.frame(esNAMQ_10_A10$time)
data$GFCF_LT <- esNAMQ_10_AN6$LT
data$GFCF_LV <- esNAMQ_10_AN6$LV
data$GFCF_EE <- esNAMQ_10_AN6$EE
data$GFCF_PL <- esNAMQ_10_AN6$PL
data$GFCF_CZ <- esNAMQ_10_AN6$CZ
data$GFCF_HU <- esNAMQ_10_AN6$HU
data$GFCF_RO <- esNAMQ_10_AN6$RO
data$GDP_LT <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$LT
data$GDP_LV <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$LV
data$GDP_EE <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$EE
data$GDP_PL <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$PL
data$GDP_CZ <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$CZ
data$GDP_HU <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$HU
data$GDP_RO <- (esNAMQ_10_GDP %>% dplyr::filter(
  na_item=="B1GQ"
))$RO

####
# Skiriasi eiluciu skaicius - LFS 84 eilutes
####

data$LF02_LT <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$LT
data$LF02_LV <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$LV
data$LF02_EE <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$EE
data$LF02_PL <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$PL
data$LF02_CZ <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$CZ
data$LF02_HU <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$HU
data$LF02_RO <- (esLFSI_EDU_Q %>% dplyr::filter(
  isced11=="ED0-2"
))$RO
######
# Darbo jegos kintamasis is NAMQ
######
data$HW_LT <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$LT
data$HW_LV <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$LV
data$HW_EE <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$EE
data$HW_PL <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$PL
data$HW_CZ <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$CZ
data$HW_HU <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$HU
data$HW_RO <- (esNAMQ_10_A10_E %>% dplyr::filter(
  unit=="THS_HW",
  na_item=="EMP_DC"
))$RO





###
K <- NAMQ_10_GDP %>% filter(
  na_item=="B1GQ"
)
K$unit <- NULL
K$s_adj <- NULL
K$na_item <- NULL

plot.ts(K$values)

unique(NAMQ_10_GDP$na_item)
L <- NAMQ_10_GDP %>% filter(
  na_item=="D11"
)


L <- NAMQ_10_A10_E %>% filter(
  na_item=="EMP_DC",
  unit=="THS_HW"
)

plot.ts(L$values)
