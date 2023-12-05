
source("./tk2023/02_scripts/functions.R")
source("./tk2023/02_scripts/municipality_map.R")

partijen <- readxl::read_xlsx("./tk2023/01_data/overig/partijen.xlsx") %>% 
  mutate(partij = ifelse(partij == "Christenunie", "ChristenUnie", partij))

tk2023 <- fs::dir_ls("./tk2023/01_data/kieskring/") %>% 
  purrr::map(\(x) read_osv_file(x)) %>% 
  purrr::reduce(dplyr::bind_rows) %>% 
  dplyr::bind_rows(
    read_total_votes("./tk2023/01_data/gemeente/osv4-3_telling_tk2023_gemeente_rotterdam.csv")
  )

rm(read_osv_file)
rm(read_total_votes)
