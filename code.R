# load package ----
pacman::p_load(pacman, readr, here, janitor, tidyverse, ggExtra, paletteer, dplyr)

# load data ----
data0 <- readxl::read_xlsx("data/Phone number.xlsx")

# data wrangling

tt_vul <- data0 %>%
  select(transport_type, vulnerable_group) %>% 
  mutate(
    tt_main = case_when(
      transport_type %in% c("mobil", "motor", "online") ~ "pribadi",
      transport_type %in% c("mrt", "krl", "bus") ~ "publik",
      is.na(transport_type) == TRUE ~ "tidak ada data",
      TRUE ~ "campuran"),
    jenis_kendaraan = case_when(
      transport_type %in% c("mobil", "motor", "online", "mrt", "krl", "bus") ~ transport_type,
      is.na(transport_type) == TRUE ~ "tidak ada data",
      TRUE ~ "campuran")
  )

# plotting 1 ----
# base
tt_vul %>% ggplot(aes(x = tt_main, fill = vulnerable_group)) +
# geom  
  geom_bar(width = 0.5) +
  
# labels  
  labs(
    title = "Jenis Transportasi dan Kerentanan Penghuni",
    subtitle = "248 responden dalam wilayah studi Jabodetabek",
    x = "Jenis Transportasi",
    y = "Jumlah"
  ) +

# themes and scales    
  scale_fill_paletteer_d("ghibli::KikiMedium", direction = -1, name = "Tinggal bersama kelompok rentan") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "grey50", angle = 40),
    axis.text.y = element_text(color = "grey50", face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10, colour = "grey50"),
    legend.title = element_text(size = 10, colour = "grey50"))

ggsave("visualisasi/tt_vul.png", width = 361.421, height = 203.2, units = "mm", dpi =300)

# plotting 2 ----
p_load(treemap)

tt_vul2 <- as.data.frame(tt_vul %>% 
    group_by(tt_main, jenis_kendaraan) %>% 
    count(name = "Jumlah"))

plot2 <- treemap(tt_vul2,
                 
                 # data
                 index = c("tt_main", "jenis_kendaraan"),
                 vSize = "Jumlah",
                 type = "index",
                 
                 # main
                 title = "",
                 palette = "Set2",
                 
                 # borders
                 border.col = c("grey50","grey30"),
                 border.lwds = c(0.5, 0.3),
                 
                 # labels
                 fontsize.labels = c(0.5, 0.4),
                 fontcolor.labels = c("black", "white"),
                 fontface.labels = 1,
                 bg.labels = c("transparent"),
                 align.labels = list(c("left", "top"), c("right", "bottom")),
                 overlap.labels=0.7,
                 inflate.labels=T
                 )

ggsave("visualisasi/treemap.png", width = 361.421, height = 203.2, units = "mm", dpi =320)