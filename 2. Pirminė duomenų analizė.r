# Pirminė duomenų analizė

# Laiko eilučių atvaizdavimas

ts.plot(data_prod$GFCF_LT)
ts.plot(data_prod$GFCF_LV)
ts.plot(data_prod$GFCF_EE)
ts.plot(data_prod$GFCF_PL)
ts.plot(data_prod$GFCF_CZ)
ts.plot(data_prod$GFCF_HU)
ts.plot(data_prod$GFCF_RO)
ts.plot(data_prod$GDP_LT)
ts.plot(data_prod$GDP_LV)
ts.plot(data_prod$GDP_EE)
ts.plot(data_prod$GDP_PL)
ts.plot(data_prod$GDP_CZ)
ts.plot(data_prod$GDP_HU)
ts.plot(data_prod$GDP_RO)
ts.plot(data_prod$HW_LT)
ts.plot(data_prod$HW_LV)
ts.plot(data_prod$HW_EE)
ts.plot(data_prod$HW_PL)
ts.plot(data_prod$HW_CZ)
ts.plot(data_prod$HW_HU)
ts.plot(data_prod$HW_RO)

ts.plot(data_prod$GDP_LT)
ts.plot(data_prod$GDP_LV)
ts.plot(data_prod$GDP_EE)
ts.plot(data_prod$GDP_PL)
ts.plot(data_prod$GDP_CZ)
ts.plot(data_prod$GDP_HU)
ts.plot(data_prod$GDP_RO)
ts.plot(data_prod$L_GDP_LT)
ts.plot(data_prod$L_GDP_LV)
ts.plot(data_prod$L_GDP_EE)
ts.plot(data_prod$L_GDP_PL)
ts.plot(data_prod$L_GDP_CZ)
ts.plot(data_prod$L_GDP_HU)
ts.plot(data_prod$L_GDP_RO)
ts.plot(data_prod$DL_GDP_LT)
ts.plot(data_prod$DL_GDP_LV)
ts.plot(data_prod$DL_GDP_EE)
ts.plot(data_prod$DL_GDP_PL)
ts.plot(data_prod$DL_GDP_CZ)
ts.plot(data_prod$DL_GDP_HU)
ts.plot(data_prod$DL_GDP_RO)

ts.plot(data_prod$GFCF_LT)
ts.plot(data_prod$GFCF_LV)
ts.plot(data_prod$GFCF_EE)
ts.plot(data_prod$GFCF_PL)
ts.plot(data_prod$GFCF_CZ)
ts.plot(data_prod$GFCF_HU)
ts.plot(data_prod$GFCF_RO)
ts.plot(data_prod$L_GFCF_LT)
ts.plot(data_prod$L_GFCF_LV)
ts.plot(data_prod$L_GFCF_EE)
ts.plot(data_prod$L_GFCF_PL)
ts.plot(data_prod$L_GFCF_CZ)
ts.plot(data_prod$L_GFCF_HU)
ts.plot(data_prod$L_GFCF_RO)
ts.plot(data_prod$DL_GFCF_LT)
ts.plot(data_prod$DL_GFCF_LV)
ts.plot(data_prod$DL_GFCF_EE)
ts.plot(data_prod$DL_GFCF_PL)
ts.plot(data_prod$DL_GFCF_CZ)
ts.plot(data_prod$DL_GFCF_HU)
ts.plot(data_prod$DL_GFCF_RO)

ts.plot(data_prod$HW_LT)
ts.plot(data_prod$HW_LV)
ts.plot(data_prod$HW_EE)
ts.plot(data_prod$HW_PL)
ts.plot(data_prod$HW_CZ)
ts.plot(data_prod$HW_HU)
ts.plot(data_prod$HW_RO)
ts.plot(data_prod$L_HW_LT)
ts.plot(data_prod$L_HW_LV)
ts.plot(data_prod$L_HW_EE)
ts.plot(data_prod$L_HW_PL)
ts.plot(data_prod$L_HW_CZ)
ts.plot(data_prod$L_HW_HU)
ts.plot(data_prod$L_HW_RO)
ts.plot(data_prod$DL_HW_LT)
ts.plot(data_prod$DL_HW_LV)
ts.plot(data_prod$DL_HW_EE)
ts.plot(data_prod$DL_HW_PL)
ts.plot(data_prod$DL_HW_CZ)
ts.plot(data_prod$DL_HW_HU)
ts.plot(data_prod$DL_HW_RO)

# Koreliacijų matricos

data_prod_num <- as.data.frame(lapply(data_prod, as.numeric))

M_data_prod <- cor(data_prod_num, method = "spearman")

corrplot(M_data_prod, method = "color", na.label=".")

# Tarpusavio koreliacijos tarp šalių kintamųjų

# Lietuva
# Nominaliių verčių koreliacijos
cor(data_prod_num$GDP_LT, data_prod_num$GFCF_LT)
cor(data_prod_num$GDP_LT, data_prod_num$HW_LT)
cor(data_prod_num$GFCF_LT, data_prod_num$HW_LT)

# Logaritmuotų verčių koreliacijos
cor(data_prod_num$L_GDP_LT, data_prod_num$L_GFCF_LT)
cor(data_prod_num$L_GDP_LT, data_prod_num$L_HW_LT)
cor(data_prod_num$L_GFCF_LT, data_prod_num$L_HW_LT)

# Diferencijuotų ir logaritmuotų verčių koreliacijos
cor(data_prod_num$DL_GDP_LT, data_prod_num$DL_GFCF_LT, na.rm = TRUE)
cor(data_prod_num$DL_GDP_LT, data_prod_num$DL_HW_LT)
cor(data_prod_num$DL_GFCF_LT, data_prod_num$DL_HW_LT)

# Latvija
# Nominaliių verčių koreliacijos
cor(data_prod_num$GDP_LV, data_prod_num$GFCF_LV)
cor(data_prod_num$GDP_LV, data_prod_num$HW_LV)
cor(data_prod_num$GFCF_LV, data_prod_num$HW_LV)

# Logaritmuotų verčių koreliacijos
cor(data_prod_num$L_GDP_LV, data_prod_num$L_GFCF_LV)
cor(data_prod_num$L_GDP_LV, data_prod_num$L_HW_LV)
cor(data_prod_num$L_GFCF_LV, data_prod_num$L_HW_LV)

# Diferencijuotų ir logaritmuotų verčių koreliacijos
cor(data_prod_num$DL_GDP_LV, data_prod_num$DL_GFCF_LV, na.rm = TRUE)
cor(data_prod_num$DL_GDP_LV, data_prod_num$DL_HW_LV)
cor(data_prod_num$DL_GFCF_LV, data_prod_num$DL_HW_LV)

# Estija
# Nominaliių verčių koreliacijos
cor(data_prod_num$GDP_EE, data_prod_num$GFCF_EE)
cor(data_prod_num$GDP_EE, data_prod_num$HW_EE)
cor(data_prod_num$GFCF_EE, data_prod_num$HW_EE)

# Logaritmuotų verčių koreliacijos
cor(data_prod_num$L_GDP_EE, data_prod_num$L_GFCF_EE)
cor(data_prod_num$L_GDP_EE, data_prod_num$L_HW_EE)
cor(data_prod_num$L_GFCF_EE, data_prod_num$L_HW_EE)

# Diferencijuotų ir logaritmuotų verčių koreliacijos
cor(data_prod_num$DL_GDP_EE, data_prod_num$DL_GFCF_EE, na.rm = TRUE)
cor(data_prod_num$DL_GDP_EE, data_prod_num$DL_HW_EE)
cor(data_prod_num$DL_GFCF_EE, data_prod_num$DL_HW_EE)

# Lenkija
# Nominaliių verčių koreliacijos
cor(data_prod_num$GDP_PL, data_prod_num$GFCF_PL)
cor(data_prod_num$GDP_PL, data_prod_num$HW_PL)
cor(data_prod_num$GFCF_PL, data_prod_num$HW_PL)

# Logaritmuotų verčių koreliacijos
cor(data_prod_num$L_GDP_PL, data_prod_num$L_GFCF_PL)
cor(data_prod_num$L_GDP_PL, data_prod_num$L_HW_PL)
cor(data_prod_num$L_GFCF_PL, data_prod_num$L_HW_PL)

# Diferencijuotų ir logaritmuotų verčių koreliacijos
cor(data_prod_num$DL_GDP_PL, data_prod_num$DL_GFCF_PL, na.rm = TRUE)
cor(data_prod_num$DL_GDP_PL, data_prod_num$DL_HW_PL)
cor(data_prod_num$DL_GFCF_PL, data_prod_num$DL_HW_PL)

# Čekija
# Nominaliių verčių koreliacijos
cor(data_prod_num$GDP_CZ, data_prod_num$GFCF_CZ)
cor(data_prod_num$GDP_CZ, data_prod_num$HW_CZ)
cor(data_prod_num$GFCF_CZ, data_prod_num$HW_CZ)

# Logaritmuotų verčių koreliacijos
cor(data_prod_num$L_GDP_CZ, data_prod_num$L_GFCF_CZ)
cor(data_prod_num$L_GDP_CZ, data_prod_num$L_HW_CZ)
cor(data_prod_num$L_GFCF_CZ, data_prod_num$L_HW_CZ)

# Diferencijuotų ir logaritmuotų verčių koreliacijos
cor(data_prod_num$DL_GDP_CZ, data_prod_num$DL_GFCF_CZ, na.rm = TRUE)
cor(data_prod_num$DL_GDP_CZ, data_prod_num$DL_HW_CZ)
cor(data_prod_num$DL_GFCF_CZ, data_prod_num$DL_HW_CZ)

# Vengrija
# Nominaliių verčių koreliacijos
cor(data_prod_num$GDP_HU, data_prod_num$GFCF_HU)
cor(data_prod_num$GDP_HU, data_prod_num$HW_HU)
cor(data_prod_num$GFCF_HU, data_prod_num$HW_HU)

# Logaritmuotų verčių koreliacijos
cor(data_prod_num$L_GDP_HU, data_prod_num$L_GFCF_HU)
cor(data_prod_num$L_GDP_HU, data_prod_num$L_HW_HU)
cor(data_prod_num$L_GFCF_HU, data_prod_num$L_HW_HU)

# Diferencijuotų ir logaritmuotų verčių koreliacijos
cor(data_prod_num$DL_GDP_HU, data_prod_num$DL_GFCF_HU, na.rm = TRUE)
cor(data_prod_num$DL_GDP_HU, data_prod_num$DL_HW_HU)
cor(data_prod_num$DL_GFCF_HU, data_prod_num$DL_HW_HU)

# Rumunija
# Nominaliių verčių koreliacijos
cor(data_prod_num$GDP_RO, data_prod_num$GFCF_RO)
cor(data_prod_num$GDP_RO, data_prod_num$HW_RO)
cor(data_prod_num$GFCF_RO, data_prod_num$HW_RO)

# Logaritmuotų verčių koreliacijos
cor(data_prod_num$L_GDP_RO, data_prod_num$L_GFCF_RO)
cor(data_prod_num$L_GDP_RO, data_prod_num$L_HW_RO)
cor(data_prod_num$L_GFCF_RO, data_prod_num$L_HW_RO)

# Diferencijuotų ir logaritmuotų verčių koreliacijos
cor(data_prod_num$DL_GDP_RO, data_prod_num$DL_GFCF_RO, na.rm = TRUE)
cor(data_prod_num$DL_GDP_RO, data_prod_num$DL_HW_RO)
cor(data_prod_num$DL_GFCF_RO, data_prod_num$DL_HW_RO)

