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

scan_data(partijen)

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


tk2023 %>% 
  group_by(partij) %>% 
  filter(stemmen == max(stemmen)) %>% 
  arrange(desc(stemmen)) %>% head(20)

tk2023 %>% 
  filter(partij == "SP (Socialistische Partij)") %>% 
  arrange(desc(stemmen)) %>% head(20)

data <- tk2023 %>% 
  inner_join(partijen, by = join_by(partij)) %>% 
  group_by(gemcode, groep) %>% 
  summarise(stemmen = sum(stemmen),
            .groups = "drop") %>% 
  group_by(gemcode) %>% 
  mutate(stemmen_p = stemmen / sum(stemmen)) %>% 
  pivot_wider(id_cols = gemcode, names_from = groep, values_from = stemmen_p)

data_partij <- tk2023 %>% 
  inner_join(partijen, by = join_by(partij)) %>% 
  group_by(gemcode, partij = afkorting) %>% 
  summarise(stemmen = sum(stemmen),
            .groups = "drop")  %>% 
  group_by(gemcode) %>% 
  mutate(stemmen_p = stemmen / sum(stemmen)) %>% 
  pivot_wider(id_cols = gemcode, names_from = partij, values_from = stemmen_p)

data_partij_2 <- tk2023 %>% 
  inner_join(partijen, by = join_by(partij)) %>% 
  group_by(gemcode, partij = afkorting) %>% 
  summarise(stemmen = sum(stemmen),
            .groups = "drop") 

data_partij %>% filter(gemcode == "0840")

nsc <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = NSC)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen", limits = c(0, 1)) +
  theme_void()

bbb <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = BBB)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen",  limits = c(0, 1)) +
  theme_void()

pvv <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = PVV)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen", limits = c(0, 1)) +
  theme_void()

vvd <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = VVD)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen",  limits = c(0, 1)) +
  theme_void()

glp <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = GLPvdA)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen",  limits = c(0, 1)) +
  theme_void()

d66 <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = D66)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen",  limits = c(0, 1)) +
  theme_void()

(nsc + bbb + pvv) / (vvd + glp + d66)

data_partij %>% 
  select(gemcode, BBB, D66, GLPvdA, PVV, NSC, VVD) %>% 
  pivot_longer(cols = -gemcode,
               names_to = "partij",
               values_to = "percentage") %>% 
  ggplot(aes(x = percentage, fill = partij)) +
  geom_density(alpha = 0.5)


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
