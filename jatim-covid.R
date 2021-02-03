# Access jatim data API
# install.packages("httr")
library(httr)
readAPI <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
jatimCovid <- content(readAPI, as = "parsed", simplifyVector = TRUE)

# View element
names(jatimCovid)

# list_perkembangan as dataset
covidJatim <- jatimCovid$list_perkembangan
head(covidJatim)

# install.packages("dplyr")
library(dplyr)
# merapikan data
newdata <- covidJatim %>%
  select(-contains('DIRAWAT_OR_ISOLASI')) %>%
  select(-starts_with('AKUMULASI')) %>%
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>%
  mutate(
    tanggal = as.POSIXct(tanggal/1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(newdata)

# visualising daily cases
library(ggplot2) #already installed
# install.packages("hrbrthemes")
library(hrbrthemes)
ggplot(newdata, aes(tanggal, kasus_baru)) +
  geom_col(fill = "#450101") +
  labs(
    x = "Bulan Terkahir",
    y = "Jumlah Kasus",
    title = "Kasus Positif Harian COVID19 di Jawa Timur",
    caption = "Sumber data: covid.19.co.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

# print(jatimCovid$last_date)

#visualising recovered cases
ggplot(newdata, aes(tanggal, sembuh)) +
  geom_col(fill = "#617830") +
  labs(
    x = "Bulan Terkahir",
    y = "Jumlah Kasus",
    title = "Kasus Sembuh Harian COVID19 di Jawa Timur",
    caption = "Sumber data: covid.19.co.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#visualising death cases
ggplot(newdata, aes(tanggal, meninggal)) +
  geom_col(fill = "black") +
  labs(
    x = "Bulan Terkahir",
    y = "Jumlah Kasus",
    title = "Kasus Meninggal Harian COVID19 di Jawa Timur",
    caption = "Sumber data: covid.19.co.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

# check out if this week is better


