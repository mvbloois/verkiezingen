library(tidyverse)
library(sf)
library(patchwork)

tk2023 %>% 
  group_by(partij) %>% 
  filter(stemmen == max(stemmen)) %>% 
  arrange(desc(stemmen)) %>% head(20)

tk2023 %>% 
  group_by(gemnaam) %>% 
  summarise(stemmen = sum(stemmen)) %>% 
  arrange(desc(stemmen)) %>% head(20)

tk2023 %>% 
  group_by(gemnaam) %>% 
  summarise(stemmen = sum(stemmen)) %>% 
  arrange(desc(stemmen)) %>% tail(20)

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

data_partij_abs <- tk2023 %>% 
  inner_join(partijen, by = join_by(partij)) %>% 
  group_by(gemcode, partij = afkorting) %>% 
  summarise(stemmen = sum(stemmen),
            .groups = "drop")  %>% 
  group_by(gemcode) %>% 
  pivot_wider(id_cols = gemcode, names_from = partij, values_from = stemmen)


data_partij_2 <- tk2023 %>% 
  inner_join(partijen, by = join_by(partij)) %>% 
  group_by(gemcode, partij = afkorting) %>% 
  summarise(stemmen = sum(stemmen),
            .groups = "drop") 

data_partij %>% filter(gemcode == "0840")

plot_map <- function(selectie = "PVV") {
  selectie <- enquo(selectie)
  gem_map %>%
    inner_join(data_partij, by = join_by(gemcode)) %>%
    ggplot(aes(fill = !!selectie)) +
    geom_sf(linewidth = 0.1) +
    scale_fill_gradient(
      low = "white",
      high = "darkgreen",
      limits = c(0, NA),
      labels = scales::percent_format()
    ) +
    theme_void() + 
    theme(legend.title = element_text(size = 5), 
          legend.text = element_text(size = 5))
  
}

(plot_map(PVV) + plot_map(BBB)) / (plot_map(NSC) + plot_map(VVD)) /
  (plot_map(GLPvdA) + plot_map(D66)) +
  plot_annotation(title = "Tweede Kamerverkiezingen 2023",
                  subtitle = "Percentage stemmen per gemeente")

ggsave("./tk2023/09_output/top-6.png",
       width = 5,
       height = 7)
tk2023$partij %>% unique

gem_map %>%
  inner_join(data_partij, by = join_by(gemcode)) %>%
  mutate(selectie = PVV + BBB + NSC) %>% 
  ggplot(aes(fill = selectie)) +
  geom_sf(linewidth = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0, NA),
    labels = scales::percent_format()
  ) +
  theme_void() + 
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))

gem_map %>%
  inner_join(data_partij, by = join_by(gemcode)) %>%
  mutate(selectie = GLPvdA + D66 + PvdD) %>% 
  ggplot(aes(fill = selectie)) +
  geom_sf(linewidth = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0, NA),
    labels = scales::percent_format()
  ) +
  theme_void() + 
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))

gem_map %>%
  inner_join(data_partij, by = join_by(gemcode)) %>%
  mutate(selectie = SGP + CU) %>% 
  ggplot(aes(fill = selectie)) +
  geom_sf(linewidth = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0, NA),
    labels = scales::percent_format()
  ) +
  theme_void() + 
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))

gem_map %>%
  inner_join(data_partij, by = join_by(gemcode)) %>%
  mutate(selectie = DENK + SP) %>% 
  ggplot(aes(fill = selectie)) +
  geom_sf(linewidth = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0, NA),
    labels = scales::percent_format()
  ) +
  theme_void() + 
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))

full <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = NSC)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen",  limits = c(0, 1)) +
  theme_void()

scale <- gem_map %>% 
  inner_join(
    data_partij, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(fill = NSC)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen",  limits = c(0, NA)) +
  theme_void()

size <- gem_map %>% 
  mutate(geometry = sf::st_centroid(geometry)) %>% 
  inner_join(
    data_partij_abs, by = join_by(gemcode)
  ) %>% 
  ggplot(aes(geometry = geometry,
             size = NSC)) +
  geom_sf(gem_map,
          mapping = aes(geometry = geometry),
          fill = "white",
          inherit.aes = FALSE) +
  geom_sf(colour = "darkgreen") +
  scale_size(range = c(0.1, 5),
             #breaks = c(100, 1000, 10000, 50000, 100000),
             labels = scales::number_format()) +
  theme_void()

size

full / scale / size 

ggsave("./tk2023/09_output/types.png", width = 5, height = 7)

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
