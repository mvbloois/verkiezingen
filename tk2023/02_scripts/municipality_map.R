library(dplyr)
library(stringr)
library(sf)

# url <- "https://download.cbs.nl/regionale-kaarten/wijkbuurtkaart_2023_v1.zip"
# 
# dest_file <- "./tk2023/01_data/kaart/wijkbuurtkaart_2023_v1.zip"
# 
# download.file(url = url,
#               destfile = dest_file)
# 
# # Just select the municipality files
# files <- unzip(dest_file, list = TRUE) %>%
#   filter(str_detect(Name, "gemeenten_")) %>%
#   pull(Name)
# 
# unzip(dest_file,
#       files = files,
#       exdir = "./tk2023/01_data/kaart")

gem_map <- st_read("./tk2023/01_data/kaart/gemeenten_2023_v1.shp",
                   quiet = TRUE) %>% 
  filter(H2O == "NEE") %>% 
  mutate(gemcode = str_remove(GM_CODE, "GM"))

#fs::file_delete(dest_file)
