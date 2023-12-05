library(tidyverse)
library(pointblank)

source("./tk2023/02_scripts/load_data.R")

informant_partijen <- partijen %>% 
  create_informant(
    label = "Beschrijving tabel Partijen.",
    tbl_name = "partijen"
  ) %>% 
  info_tabular(
    description = "Deze tabel bevat de namen van alle politieke partijen die deelnamen
    aan de Tweede Kamerverkiezingen van 22 november 2023. De politieke partijen
    zijn ingedeeld naar links, rechts en radicaal-rechts. Voor alle partijen is een korte naam
    opgenomen."
  ) %>% 
  info_columns(
    columns = "partij",
    info = "Naam van partij volgens OSV4.",
    info = "Komt overeen met de kolom partij in de tabel *tk2023*."
  ) %>% 
  info_columns(
    columns = "groep",
    info = "Indeling van partij naar links, rechts, radicaal-rechts."
  ) %>% 
  info_columns(
    columns = "afkorting",
    info = "Korte naam voor partij"
  ) %>% 
  incorporate()

informant_partijen

scan_data(partijen, sections = "OVMS")

informant_tk2023 <- tk2023 %>% 
  create_informant(
    label = "Beschrijving van de tabel tk2023",
    tbl_name = "tk2023"
  ) %>% 
  info_tabular(
    description = "Deze tabel bevat de aantallen stemmen per partij per gemeente
    zoals gepubliceerd door de Kieskringen."
  ) %>% 
  incorporate()

informant_tk2023

scan_data(tk2023, sections = "OVMS")
