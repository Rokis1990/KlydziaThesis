# duomenų įkėlimas

install.packages("readxl")
library(readxl)

# ketvirtinių duomenų įkėlimas

Qdatai <- read_excel("C:/Users/Home/Dropbox/Masters/The Impact of Labour and Capital Inputs on Production and Income in modern Economies/Eurostat data/Quarterly/namq_10_gdp(1).xls", col_names=TRUE)
