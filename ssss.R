library(tidyverse)
library(sf)
library(readxl)
library(fs)
library(glue)
library(patchwork)

pc4_map <- st_read("s:/administratie/datakluis/cartographica/2023/Nlp4_r2301.shp")

gem_map <- pc4_map %>% 
  group_by(gemcode = GEMCODE, gemnr = GEMNR, gemnaam = GEMNAAM, provc = PROVC, provc_nm = PROVC_NM) %>% 
  summarise(population = sum(INW_T))

read_total_votes <- function(filename) {
  gemeente <- str_extract(str_remove(filename, ".csv"), "[:alpha:]+$")
  
  gn <- str_pad(parse_number(readLines(filename, n = 4)[4]), 4, "left", pad = "0")
  
  read_csv2(filename,
            skip = 5) %>%  
    filter(! is.na(Aanduiding)) %>% 
    filter(! is.na(Lijstnummer)) %>% 
    mutate(gemnaam = str_to_title(gemeente),
           gemcode = gn) %>% 
    select(lijst = Lijstnummer, partij = Aanduiding, gemnaam, stemmen = Totaal, gemcode) 
}

read_osv_file <- function(filename) {
  df <- read_csv2(filename,
                  skip = 5)
  
  df %>% 
    filter(!is.na(Aanduiding)) %>% 
    filter(!is.na(Lijstnummer)) %>% 
    select(-Volgnummer, -`Naam kandidaat`, -Totaal) %>%  
    pivot_longer(cols = 3:last_col(),
                 values_transform = as.integer) %>% 
    select(lijst = Lijstnummer, partij = Aanduiding, gemnaam = name, stemmen = value) %>% 
    inner_join(
      df %>% 
        slice_head(n = 1) %>% 
        select(6:last_col()) %>% 
        t() %>% 
        as.data.frame() %>% 
        rownames_to_column("gemnaam") %>% 
        rename(gemcode = V1) %>% 
        mutate(gemcode = str_pad(gemcode, 4, "left", pad = "0")),
      by = join_by(gemnaam)
    )
  
}

partijen <- read_xlsx("partijen.xlsx")

files <- fs::dir_ls("./data")

tk2023 <- map(files, \(x) read_osv_file(x)) %>% 
  reduce(bind_rows) %>% 
  bind_rows(
    read_total_votes("./asdrt/osv4-3_telling_tk2023_gemeente_rotterdam.csv")
  )

tk2023 %>% 
  group_by(partij) %>% 
  filter(stemmen == max(stemmen)) %>% 
  arrange(desc(stemmen)) %>% head(20)

tk2023 %>% 
  filter(partij == "SP (Socialistische Partij)") %>% 
  arrange(desc(stemmen)) %>% head(20)

data <- tk2023 %>% 
  inner_join(partijen) %>% 
  group_by(gemcode, groep) %>% 
  summarise(stemmen = sum(stemmen)) %>% 
  group_by(gemcode) %>% 
  mutate(stemmen_p = stemmen / sum(stemmen)) %>% 
  pivot_wider(id_cols = gemcode, names_from = groep, values_from = stemmen_p)

data_partij_2 <- tk2023 %>% 
  inner_join(partijen) %>% 
  group_by(gemcode, partij = afkorting) %>% 
  summarise(stemmen = sum(stemmen)) 

data_partij %>% filter(gemcode == "0840")

nsc <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = NSC)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen", limits = c(0, 0.6)) +
  theme_void()

bbb <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = BBB)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen", , limits = c(0, 0.6)) +
  theme_void()

pvv <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = PVV)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkblue", , limits = c(0, 0.6)) +
  theme_void()

vvd <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = VVD)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkblue", , limits = c(0, 0.6)) +
  theme_void()

glp <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = GLPvdA)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkred", , limits = c(0, 0.6)) +
  theme_void()

d66 <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = D66)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkred", , limits = c(0, 0.6)) +
  theme_void()

(nsc + bbb + pvv) / (vvd + glp + d66)


coal <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  mutate(coal = PVV + BBB + NSC + VVD) %>% 
  ggplot(aes(fill = coal)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_void()



gem_map %>% 
  st_drop_geometry() %>% 
  inner_join(
    data_partij_2, by = join_by(gemcode)
  ) %>% 
  group_by(provc_nm, partij) %>% 
  summarise(stemmen = sum(stemmen)) %>% 
  filter(partij %in% c("NSC", "BBB", "PVV", "VVD", "GLPvdA", "D66")) %>% 
  ggplot(aes(x = partij, y = stemmen, fill = provc_nm)) +
  geom_col(position = position_fill()) +
  coord_flip()

kd <- 10096040/150

gem_map %>% 
  st_drop_geometry() %>% 
  inner_join(
    data_partij_2, by = join_by(gemcode)
  ) %>% 
  filter(partij == "BBB") %>% 
  group_by(provc_nm) %>% 
  summarise(stemmen = sum(stemmen)) %>% 
  arrange(-stemmen) %>% 
  mutate(per = stemmen / sum(stemmen)) %>% 
  mutate(zetels = stemmen / kd)

tk2023 %>% 
  group_by(gemnaam) %>% 
  summarise(stemmen = sum(stemmen)) %>% 
  arrange(desc(stemmen))

l <- gem_map %>% 
  inner_join(
    data
  ) %>% 
  ggplot(aes(fill = links)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkred", limits = c(0, .8)) +
  theme_void()

r <- gem_map %>% 
  inner_join(
    data
  ) %>% 
  ggplot(aes(fill = rechts)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen", limits = c(0, .8)) +
  theme_void()

rr <- gem_map %>% 
  inner_join(
    data
  ) %>% 
  ggplot(aes(fill = rrechts)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkblue", limits = c(0, .8)) +
  theme_void()

l + r + rr

gem_map %>% 
  st_drop_geometry() %>% 
  inner_join(
    data
  ) %>% 
  arrange(desc(links)) %>% 
  head(20)


data %>% 
  ggtern::ggtern(aes(x = links, y = rechts, z = rrechts)) +
  geom_point() +
  ggtern::theme_rgbw() 


data %>% arrange(links)
