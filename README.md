# Analysis_Covid19_Indonesia
library(httr)
resp <- GET ("https://data.covid19.go.id/public/api/update.json")
status_code(resp)

headers(resp)
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE)

length(cov_id_raw)
names(cov_id_raw)
cov_id_update <-cov_id_raw$update

lapply(cov_id_update,names)
cov_id_update$penambahan$tanggal
cov_id_update$penambahan$jumlah_sembuh
cov_id_update$penambahan$jumlah_meninggal
cov_id_update$total$jumlah_positif
cov_id_update$total$jumlah_meninggal


resp_jabar <-GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)

names(cov_jabar_raw)
cov_jabar_raw$kasus_total
cov_jabar_raw$meninggal_persen
cov_jabar_raw$sembuh_persen

cov_jabar <- cov_jabar_raw$list_perkembangan
str(cov_jabar)
head(cov_jabar)

library(dplyr)
new_cov_jabar<-
  cov_jabar %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal/ 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)  
  )
str(new_cov_jabar)

library(ggplot2)
install.packages(hrbrthemes)
library(hrbrthemes)
ggplot(new_cov_jabar, aes(x = tanggal, y = kasus_baru)) +
  geom_col()

ggplot(new_cov_jabar, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x=NULL,
    y="jumlah kasus",
    title = " Kasus Harian Positif COVID-19 di jawa Barat",
    subtitle = "Laporan per 1 Juni 2021",
    caption = "Sumber data: covid.19.go.id"
  )+
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  )+
  theme(plot.title.position = "plot")

ggplot(new_cov_jabar, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")


ggplot(new_cov_jabar, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
