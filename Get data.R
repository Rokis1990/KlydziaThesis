# Ask pacman to load these packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(eurostat, dplyr, openxlsx, rvest, tidyverse, knitr)

# Using eurostat package function "get_eurostat", download the quarterly data for ... from the national accounts (the data table name in the eurostat database is "namq_1_a10")
NAMQ_10_A10 <- get_eurostat("namq_10_a10")


######################

filtered <- NAMQ_10_A10 %>% filter(
  unit=="CLV05_MEUR",
  s_adj=="SCA",
  nace_r2=="TOTAL",
  na_item=="B1G",
  geo %in% c("EU28", "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
  )


#######################

# Filter out the data that is not related to Lithuania and Poland
NAMQ_10_A10 <- NAMQ_10_A10 %>%
  filter(geo %in% c("LT", "PL"))
 
NAMQ_10_A10 <- NAMQ_10_A10 %>% 
  filter(s_adj %in% "CA")


TEINA040 <- get_eurostat("teina040") # Gross fixed capital formation, current prices
TEINA040 <- TEINA040 %>% filter(geo %in% c("LT", "PL"))

TEINA041 <- get_eurostat("teina041") # Gross fixed capital formation, volumes
TEINA041 <- TEINA041 %>% filter(geo %in% c("LT", "PL"))

TEC00011 <- get_eurostat("tec00011") # Gross fixed capital formation (investments)
TEC00011 <- TEC00011 %>% filter(geo %in% c("LT", "PL"))


TIPSNA20 <- get_eurostat("tipsna20") # Gross fixed capital formation at current prices
TIPSNA20 <- TIPSNA20 %>% filter(geo %in% c("LT", "PL"))


TIPSAU20 <- get_eurostat("tipsau20") # Gross domestic product (GDP) at market prices - quarterly data
TIPSAU20 <- TIPSAU20 %>% filter(geo %in% c("LT", "PL"))


TIPSNA71 <- get_eurostat("tipsna71") # Real labour productivity per person employed - quarterly data
TIPSNA71 <- TIPSNA71 %>% filter(geo %in% c("LT", "PL"))

TIN00150 <- get_eurostat("tin00150") # Value added by NACE Rev. 2
TIN00150 <- TIN00150 %>% filter(geo %in% c("LT", "PL"))

TIN00147 <- get_eurostat("tin00147") # Value added of the non-financial business economy by size class of employment
TIN00147 <- TIN00147 %>% filter(geo %in% c("LT", "PL"))


# National accounts basic breakdown
NAMQ_10_A10_E <- get_eurostat("namq_10_a10_e") # Employment A*10 industry breakdowns
NAMQ_10_A10_E <- NAMQ_10_A10_E %>% filter(geo %in% c("LT", "PL"))

NAMQ_10_AN6 <- get_eurostat("namq_10_an6") # Gross fixed capital formation with AN_F6 asset breakdowns
NAMQ_10_AN6 <- NAMQ_10_AN6 %>% filter(geo %in% c("LT", "PL"))

NAMQ_10_A10 <- get_eurostat("namq_10_a10") # Gross value added and income A*10 industry breakdowns
NAMQ_10_A10 <- NAMQ_10_A10 %>% filter(geo %in% c("LT", "PL"))

# National Accounts main aggregates
NAMQ_10_GDP <- get_eurostat("namq_10_gdp") # GDP and main components (output, expenditure and income)
NAMQ_10_GDP <- NAMQ_10_GDP %>% filter(geo %in% c("LT", "PL"))

NAMQ_10_FCS <- get_eurostat("namq_10_fcs") # Final consumption aggregates
NAMQ_10_FCS <- NAMQ_10_FCS %>% filter(geo %in% c("LT", "PL"))

NAMQ_10_EXI <- get_eurostat("namq_10_exi") # Exports and imports by Member States of the EU/third countries
NAMQ_10_EXI <- NAMQ_10_EXI %>% filter(geo %in% c("LT", "PL"))


# Auxhiliary measures
NAMQ_10_PE <- get_eurostat("namq_10_pe") # Population and employment 
NAMQ_10_PE <- NAMQ_10_PE %>% filter(geo %in% c("LT", "PL"))

NAMQ_10_PC <- get_eurostat("namq_10_pc") # Main GDP aggregates per capita
NAMQ_10_PC <- NAMQ_10_PC %>% filter(geo %in% c("LT", "PL"))

NAMQ_10_LP_ULC <- get_eurostat("namq_10_lp_ulc") # Labour productivity and unit labour costs
NAMQ_10_LP_ULC <- NAMQ_10_LP_ULC %>% filter(geo %in% c("LT", "PL"))


# Regional National accounts
NAMA_10r_2gdp <- get_eurostat("nama_10r_2gdp") # Gross domestic product (GDP) at current market prices by NUTS 2 regions

NAMA_10R_3GDP <- get_eurostat("nama_10r_3gdp") # Gross domestic product (GDP) at current market prices by NUTS 3 regions 	 

NAMA_10R_3POPGDP <- get_eurostat("nama_10r_3popgdp") # Average annual population to calculate regional GDP data (thousand persons) by NUTS 3 regions 

# Branch accounts REGIONAL
NAMA_10R_3GVA <- get_eurostat("nama_10r_3gva")	 # Gross value added at basic prices by NUTS 3 regions  
NAMA_10R_2GFCF <- get_eurostat("nama_10r_2gfcf") # Gross fixed capital formation by NUTS 2 regions 	 
NAMA_10R_2COE <- get_eurostat("nama_10r_2coe") # Compensation of employees by NUTS 2 regions 	 
NAMA_10R_EMPERS <- get_eurostat("nama_10r_3empers") # Employment (thousand persons) by NUTS 3 regions 	 
NAMA_10R_2EMHRW <- get_eurostat("nama_10r_2emhrw") # Employment (thousand hours worked) by NUTS 2 regions 	 


# Supply, use and input-output table
NAIO_10_CP15 <- get_eurostat("naio_10_cp15") # Supply table at basic prices incl. transformation into purchasers' prices (naio_10_cp15)	 
NAIO_10_CP15 <- NAIO_10_CP15 %>% filter(geo %in% c("LT", "PL"))

NAIO_10_CP16 <- get_eurostat("naio_10_cp16") # Use table at purchasers' prices 	 
NAIO_10_CP16 <- NAIO_10_CP16 %>% filter(geo %in% c("LT", "PL"))

NAIO_10_CP1610 <- get_eurostat("naio_10_cp1610") # Use table at basic prices 	 
NAIO_10_CP1610 <- NAIO_10_CP1610 %>% filter(geo %in% c("LT", "PL"))
  
NAIO_10_CP1620 <- get_eurostat("naio_10_cp1620")	 # Table of trade and transport margins 
NAIO_10_CP1620 <- NAIO_10_CP1620 %>% filter(geo %in% c("LT", "PL"))

NAIO_10_CP1630 <- get_eurostat("naio_10_cp1630")	 # Table of taxes less subsidies on product  
NAIO_10_CP1630 <- NAIO_10_CP1630 %>% filter(geo %in% c("LT", "PL"))

NAIO_10_CO1700 <- get_eurostat("naio_10_cp1700")	 # Symmetric input-output table at basic prices (product by product) 
NAIO_10_CO1700 <- NAIO_10_CO1700 %>% filter(geo %in% c("LT", "PL"))

NAIO_10_CP1750 <- get_eurostat("naio_10_cp1750")	# Symmetric input-output table at basic prices (industry by industry)  
 # NAIO_10_CP1750 <- NAIO_10_CP1750 %>% filter(geo %in% c("LT", "PL")) #not working filter



GDPTables <- search_eurostat("product", type = "table")

LabourTables <- search_eurostat("labour", type = "table")

CapitalTables <- search_eurostat("capital", type="table")

WageTables <- search_eurostat("wage", type="table")

GVATables <- search_eurostat("Value added", type="table")

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