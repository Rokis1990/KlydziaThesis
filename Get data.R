# Užkrauname duomenų parsisiuntimui iš Eurostat ir transformavimui reikalingus paketus

library(eurostat)
library(dplyr)
library(openxlsx)
library(rvest)
library(tidyverse)
library(knitr)


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
# Chain linked volumes 2010
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
# Vienetai - tūkst. asmenų
# Lytys - suma
# Nusezoninti duomenys
LFSI_EDUC_Q <- LFSI_EDUC_Q %>% filter(
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
NAMQ_10_AN6 <- NAMQ_10_AN6 %>% filter(
  unit %in% c("THS_HW","THS_PER","THS_JOB"),
  s_adj=="SCA",
  nace_r2=="TOTAL",
  na_item %in% c("EMP_DC","SAL_DC"),
  geo %in% c("EE", "LV","LT","PL","CZ","HU","RO")
)

#
#
NAMQ_10_EXI <- NAMQ_10_EXI



##################
# Duomenų lentelės
##################

dat <- get_eurostat(id="teina011", time_format = "num") # laikas kaip skaičius, bet vietoje pavadinimo kodas
head(dat)
labels<-label_eurostat(dat)

datLabels <- get_eurostat(id="teina011", type = "label") # data kaip data, bet pilnas pavadinimas

GDP_LTPL <- get_eurostat("teina011", filters=list(geo=c("LT", "PL")))

GDP_LTPL_numTime <- get_eurostat("teina011", time_format="num", filters=list(geo=c("LT","PL")))


ggplot(GDP_LTPL, aes(x=time,y=values,color=geo,group=geo,shape=geo))+
  geom_point(size=2)+
  geom_line()+theme_bw()+
  labs(title="Lietuvos ir Lenkijos BVP. In volumes", x="Metai", Y="tūkst. Vietinės valiutos")

ggplot(GDP_LTPL_numTime, aes(x=time, y=values,color=geo,group=geo,shape=geo))+
  geom_point(size=2)+
  geom_line()+theme_bw()+
  labs(title="Lietuvos ir Lenkijos BVP. In volumes. Laikas 'num'", x="Metai",Y="pinigai. tūkst. ar mln. vietine valiuta")

LF_LTPL_ano <- get_eurostat("tipslm16", filters=list(geo=c("LT","PL")))

NAMQ_10 <- get_eurostat("namq_10_a10", filters=list(geo=c("LT","PL")))

kable(head(TOC))
latex(kable(head(TOC)))
