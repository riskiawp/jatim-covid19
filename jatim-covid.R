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
# install.packages("lubridate")
library(lubridate)
jatimWeek <- newdata %>%
  count(
    tahun = year(tanggal),
    pekan = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )
glimpse(jatimWeek)

jatimWeek <- jatimWeek %>%
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(jatimWeek)

#compare with bar chart
ggplot(jatimWeek, aes(pekan, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = waiver(), expand = c(0,0)) + # breaks can use comparation like 9:29
  scale_fill_manual(values = c("TRUE" = "#617830", "FALSE"= "#450101")) +
  labs(
    x = "Bulan Terakhir",
    y = "Jumlah Kasus",
    title = "Kasus Pekanan Positif COVID19 di Jawa Timur",
    subtitle = "Kolom hijau menunjukkan penurunan covid dibandaingkan pekan sebelumnya",
    caption = "Sumber data: covid19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#view active cases use cumsum() funtcion
jatimAcc <- newdata %>%
  transmute(
    tanggal,
    activeAcc = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    recoverAcc = cumsum(sembuh),
    deathAcc = cumsum(meninggal)
  )
tail(jatimAcc)

#data transformation
# install.packages('tidyr')
library(tidyr)
dim(jatimAcc)
jatimPivot <- jatimAcc %>%
  gather(
    key = 'kategori',
    value = 'jumlah',
    -tanggal
  ) %>%
  mutate(
    kategori = sub(
      pattern = "akumulasi_",
      replacement = "",
      kategori
    )
  )
dim(jatimPivot)
glimpse(jatimAcc)

#test as pivot_longer() function
jatimPivots <- jatimAcc %>%
  pivot_longer(
    cols = -tanggal,
    names_to = 'kategori',
    names_prefix = 'akumulasi_',
    values_to = 'jumlah'
  )
dim(jatimPivots)

  