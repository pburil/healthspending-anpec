library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(write.csv)

setwd("E:\\Pedro Buril\\solicitacao gabriel")


pib_2021 <- read_xlsx("E:\\Pedro Buril\\solicitacao gabriel\\PIB dos Municípios - base de dados 2010-2021.xlsx")

pib_2021 <- pib_2021 %>%
  mutate(cod_ibge = substr(cod_ibge, 1, nchar(cod_ibge) - 1))

pib_2021 <- pib_2021 %>%
  mutate(cod_ibge = as.character(cod_ibge))

munic <- read_xlsx("E:\\Pedro Buril\\solicitacao gabriel\\MUNICS.xlsx")

munic <- munic %>%
  mutate(cod_ibge = as.character(cod_ibge))

munic <- left_join(munic, pib_2021, by = "cod_ibge")

despesas_asps <- read_xlsx("E:\\Pedro Buril\\solicitacao gabriel\\SIOPS - Despesas ASPS.xlsx")

despesas_asps <- despesas_asps %>%
  mutate(cod_ibge = as.character(cod_ibge))

base_munic_final <- left_join(munic, despesas_asps, by = "cod_ibge")

write_xlsx(base_munic_final, "MUNIC_FINAL.xlsx")




