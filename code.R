# load package ----
pacman::p_load(pacman, readr, here, janitor, tidyverse, ggExtra, paletteer, dplyr, hrbrthemes)

# load data ----
data0 <- readxl::read_xlsx("data/2020_08_10_Hasil_survey.xlsx") %>% clean_names()
data0 <- data0[-1,]

# data wrangling 1 ----

tt_vul <- data0 %>%
  select(transport_type, vulnerable_group) %>% 
  mutate(
    tt_main = case_when(
      transport_type %in% c("mobil", "motor", "online") ~ "Kendaraan Pribadi",
      transport_type %in% c("mrt", "krl", "bus") ~ "Kendaraan Publik",
      is.na(transport_type) == TRUE ~ "tidak ada data",
      TRUE ~ "mixed"),
    jenis_kendaraan = case_when(
      transport_type == "mobil" ~ "Mobil Pribadi",
      transport_type == "motor" ~ "Motor Pribadi",
      transport_type == "online" ~ "Angkutan Online",
      transport_type == "mrt" ~ "MRT",
      transport_type == "krl" ~ "KRL",
      transport_type == "bus" ~ "Bus",
      is.na(transport_type) == TRUE ~ "N/A",
      TRUE ~ "mixed")
  )



# plotting 1: moda - rentan ----
# base
tt_vul %>% ggplot(aes(x = tt_main, fill = vulnerable_group)) +
# geom  
  geom_bar(width = 0.5) +
  
# labels  
  labs(
    title = "Jenis Moda dan Kerentanan Penghuni",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Jenis Moda",
    y = "Jumlah"
  ) +

# themes and scales    
  scale_fill_paletteer_d("basetheme::deepblue", direction = -1, name = "Tinggal bersama kelompok rentan") +
  theme_ipsum_rc() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "grey50"),
    axis.text.y = element_text(color = "grey50", face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 14, colour = "grey50"),
    legend.title = element_text(size = 14, colour = "grey50"))+
  coord_flip()

ggsave("visualisasi/tt_vul.png", width = 9, height = 6, dpi ="retina")

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

# plotting 3: bubble ----
packing <- circleProgressiveLayout(tt_vul2$Jumlah, sizetype='area')
packing$radius <- 0.95*packing$radius
tt_vul2 <- cbind(tt_vul2, packing)
gg <- circleLayoutVertices(packing, npoints=50)

ggplot() +
  geom_polygon(data =gg, aes(x, y, group = id, fill=as.factor(id)), alpha = 0.6) +
  geom_text(data = tt_vul2, aes(x, y, size = (2*Jumlah), label = jenis_kendaraan)) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() +
  coord_equal() +
  scale_fill_paletteer_d("awtools::a_palette") +
  labs(title = "Sebaran Penggunaan Moda Transportasi",
       subtitle = "216 responden dalam wilayah studi Jabodetabek") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 8, face = "italic"))

ggsave("visualisasi/bubblemap.png", width = 6, height = 4, dpi = "retina")

# plotting 4: Usia ----
data0 %>% ggplot() +
  geom_histogram(aes(x=age), binwidth = 2, fill="#0E84B4FF", color="#278B9AFF", alpha=0.9) +
  labs(
    title = "Distribusi Usia Responden",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Usia",
    y = "Jumlah"
  ) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, color = "grey50"),
    axis.text.y = element_text(size = 14, color = "grey50", face = "bold")
  )
ggsave("visualisasi/usia.png", width = 9, height = 6, dpi = "retina")


# plotting 5: pekerjaan ----
data0 %>% count(occupation, name = "jumlah") %>% 
  as.data.frame() %>%
  arrange(jumlah) %>% 
  mutate(occupation = factor(occupation, levels = occupation)) -> pekerjaan


data0 %>% count(occupation, name = "jumlah") %>% 
  as.data.frame() %>%
  arrange(jumlah) %>% 
  mutate(occupation = factor(occupation, levels = occupation)) %>% 
  ggplot() +
  geom_segment(aes(x = occupation, xend = occupation, y = 0, yend = jumlah), color = "#7EBAC2FF") +
  geom_point(aes(x = occupation, y = jumlah), color = "#0E84B4FF", fill = "#278B9AFF", alpha = 0.8, size = 4) +
  coord_flip() +
  labs(
    title = "Jenis Pekerjaan Responden",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Pekerjaan",
    y = "Jumlah"
  ) +
  theme_ipsum() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 14, color = "grey50"),
    axis.text.y = element_text(size = 14, color = "grey50", face = "bold")
  )
ggsave("visualisasi/pekerjaan.png", width = 9, height = 6, dpi = "retina")

# plotting 6: tempat kerja ----
tempat_kerja <- data0 %>% count(workplace, name = "jumlah") %>% 
  as.data.frame() %>%
  arrange(jumlah) %>% 
  mutate(workplace = case_when(
    is.na(workplace) == T ~ "others",
    TRUE ~ workplace 
  )) %>% 
  mutate(workplace = factor(workplace, levels = workplace))


tempat_kerja %>% 
  ggplot() +
  geom_segment(aes(x = workplace, xend = workplace, y = 0, yend = jumlah), color = "#7EBAC2FF") +
  geom_point(aes(x = workplace, y = jumlah), color = "#0E84B4FF", fill = "#278B9AFF", alpha = 0.8, size = 4) +
  coord_flip() +
  labs(
    title = "Jenis Tempat Pekerjaan Responden",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Tempat Kerja",
    y = "Jumlah"
  ) +
  theme_ipsum() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 14, color = "grey50"),
    axis.text.y = element_text(size = 14, color = "grey50", face = "bold")
  )
ggsave("visualisasi/tempat_kerja.png", width = 9, height = 6, dpi = "retina")

# plotting 7: kondisi tempat tinggal sekitar ----
sekitar_tinggal <- data0 %>% count(surrounding_environment, name = "jumlah") %>% 
  as.data.frame() %>%
  arrange(jumlah) %>% 
  mutate(surrounding_environment = case_when(
    is.na(surrounding_environment) == T ~ "others",
    TRUE ~ surrounding_environment 
  )) %>% 
  mutate(surrounding_environment = factor(surrounding_environment, levels = surrounding_environment))


sekitar_tinggal %>% 
  ggplot() +
  geom_segment(aes(x = surrounding_environment, xend = surrounding_environment, y = 0, yend = jumlah), color = "#7EBAC2FF") +
  geom_point(aes(x = surrounding_environment, y = jumlah), color = "#0E84B4FF", fill = "#278B9AFF", alpha = 0.8, size = 4) +
  coord_flip() +
  labs(
    title = "Jenis Lingkungan Sekitar Responden",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Lingkungan Sekitar",
    y = "Jumlah"
  ) +
  theme_ipsum() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 14, color = "grey50"),
    axis.text.y = element_text(size = 14, color = "grey50", face = "bold")
  )
ggsave("visualisasi/ling_sekitar.png", width = 9, height = 6, dpi = "retina")

# plotting 8: jenis tempat tinggal ----
tempat_tinggal <- data0 %>% count(house_type, name = "jumlah") %>% 
  as.data.frame() %>%
  arrange(jumlah) %>% 
  mutate(house_type = case_when(
    is.na(house_type) == T ~ "others",
    TRUE ~ house_type 
  )) %>% 
  mutate(house_type = factor(house_type, levels = house_type))


tempat_tinggal %>% 
  ggplot() +
  geom_segment(aes(x = house_type, xend = house_type, y = 0, yend = jumlah), color = "#7EBAC2FF") +
  geom_point(aes(x = house_type, y = jumlah), color = "#0E84B4FF", fill = "#278B9AFF", alpha = 0.8, size = 4) +
  coord_flip() +
  labs(
    title = "Jenis Tempat Tinggal Responden",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Tempat Tinggal",
    y = "Jumlah"
  ) +
  theme_ipsum() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 14, color = "grey50"),
    axis.text.y = element_text(size = 14, color = "grey50", face = "bold")
  )
ggsave("visualisasi/tempat_tinggal.png", width = 9, height = 6, dpi = "retina")

# plotting 9: jenis kendaraan ----
tt_vul %>% count(jenis_kendaraan, name = "jumlah") %>% 
  mutate(jenis_kendaraan = fct_reorder(jenis_kendaraan, jumlah)) -> moda_trans 
  
  moda_trans %>% 
  ggplot() +
  geom_bar(aes(x = jenis_kendaraan, y = jumlah), stat = "identity", 
           color = "#019875FF", fill = "#00B7EBFF", 
           alpha = 0.8, width = 0.4) +
  coord_flip() +
  labs(
    title = "Jenis Moda Transportasi Responden",
    x = "Moda",
    y = "Jumlah"
  ) +
  theme_ipsum_rc() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, color = "grey50"),
    axis.text.y = element_text(size = 14, color = "grey50", face = "bold")
  )
ggsave("visualisasi/moda_trans.png", width = 9, height = 6, dpi = "retina")

# plotting 10: tempat tinggal dan jumlah orang ----
tempat_jml <- data0 %>% 
  mutate(jml_org = as.numeric(total_people),
         tmp_tgl = as.character(house_type)) %>%
  select(tmp_tgl,jml_org)

tempat_jml[is.na(tempat_jml)] <- 0

tempat_jml <- tempat_jml %>% 
  mutate(tmp_tgl = fct_reorder(tmp_tgl, jml_org))

mean_tmp_jml <- tempat_jml %>% 
  mutate(grand_mean = mean(jml_org, na.rm = T)) %>% 
  group_by(tmp_tgl, grand_mean) %>% 
  summarise(mean_wt = mean(jml_org, na.rm = T))

arrows <- tibble(
  x1 = c(2, 0.8, 1.7, 2.6),
  x2 = c(1.4,1, 2, 3),
  y1 = c(16, 1.2, 3, 9),
  y2 = c(5, 2.5, 4.3, 7.8)
)

tempat_jml %>% 
  ggplot(aes(x = tmp_tgl, y = jml_org, color = tmp_tgl)) +
  geom_jitter(position = position_jitter(seed = 2020, width = .2), alpha = .4, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 5, alpha = .9) +
  geom_hline(aes(yintercept = 5), color = "gray70", size = .9) +
  geom_segment(data = mean_tmp_jml, aes(x = tmp_tgl, xend = tmp_tgl, y = mean_wt, yend = grand_mean), size = .9) +
  coord_flip() +
  scale_color_paletteer_d("fishualize::Epinephelus_fasciatus", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Jumlah Penghuni berdasarkan Tipe Hunian",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "",
    y = "jumlah penghuni (jiwa)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 13, color = "grey50"),
    axis.text.y = element_text(size = 13, color = "grey50", face = "bold")
  ) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(2.5, "mm")), size = 0.7,
    color = "gray20", curvature = -0.3
  ) +
  annotate("text", x = 2.1, y = 17, size = 4.5, lineheight = .8, 
           label = glue("Rata-rata sebanyak\n 5 orang\n tinggal di satu hunian")) +
  annotate("text", x = 0.7, y = 0.8, size = 4, lineheight = .8, 
           label = glue("Rerata penghuni:\n 2.5 orang\n per unit apartemen")) +
annotate("text", x = 1.6, y = 3, size = 4, lineheight = .8, 
         label = glue("Rerata penghuni:\n 4.3 orang\n per unit rumah")) +
  annotate("text", x = 2.5, y = 10, size = 4, lineheight = .8, 
           label = glue("Rerata penghuni:\n 7.8 orang\n per unit kost"))

ggsave("visualisasi/jml_penghuni.png", width = 9, height = 6, dpi = "retina")

# plotting 11: tempat tinggal dan WFO WFH ----
data0 <- data0 %>% 
  mutate(oh = case_when(
    start_work == FALSE ~ "WFH",
    TRUE ~ "WFO"))

data0 %>% ggplot(aes(x = house_type, fill = oh)) +
  # geom  
  geom_bar(width = 0.5) +
  
  # labels  
  labs(
    title = "Jenis Hunian dan Tempat Bekerja",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Jenis Hunian",
    y = "Jumlah"
  ) +
  
  # themes and scales    
  scale_fill_paletteer_d("basetheme::deepblue", direction = -1, name = "Asal Kerja") +
  theme_ipsum_rc() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "grey50"),
    axis.text.y = element_text(color = "grey50", face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 14, colour = "grey50"),
    legend.title = element_text(size = 14, colour = "grey50"))+
  coord_flip()

ggsave("visualisasi/tt_oh.png", width = 9, height = 6, dpi ="retina")  

# plotting 12: lingkungan tempat tinggal dan WFO WFH ----
data0 %>% ggplot(aes(x = surrounding_environment, fill = oh)) +
  # geom  
  geom_bar(width = 0.5) +
  # labels  
  labs(
    title = "Jenis Lingkungan Hunian dan Asal Bekerja",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Jenis Hunian",
    y = "Jumlah"
  ) +
    # themes and scales    
  scale_fill_paletteer_d("basetheme::deepblue", direction = -1, name = "Asal Bekerja") +
  theme_ipsum_rc() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "grey50"),
    axis.text.y = element_text(color = "grey50", face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 14, colour = "grey50"),
    legend.title = element_text(size = 14, colour = "grey50"))+
  coord_flip()

ggsave("visualisasi/li_oh.png", width = 9, height = 6, dpi ="retina")

# plotting 13: lingkungan tempat tinggal dan WFO WFH ----
data0 %>% ggplot(aes(x = oh, fill = vulnerable_group)) +
  # geom  
  geom_bar(width = 0.5) +
  # labels  
  labs(
    title = "Asal Bekerja dan Tingkat Kerentanan",
    subtitle = "216 responden dalam wilayah studi Jabodetabek",
    x = "Asal Bekerja",
    y = "Jumlah"
  ) +
  # themes and scales    
  scale_fill_paletteer_d("basetheme::deepblue", direction = -1, name = "Tinggal bersama kelompok rentan") +
  theme_ipsum_rc() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "grey50"),
    axis.text.y = element_text(color = "grey50", face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 14, colour = "grey50"),
    legend.title = element_text(size = 14, colour = "grey50"))+
  coord_flip()

ggsave("visualisasi/rentan_oh.png", width = 9, height = 6, dpi ="retina")
