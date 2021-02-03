# Access jatim data API
#install.packages("httr")
library(httr)
readAPI <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
jatimCovid <- content(readAPI, as = "parsed", simplifyVector = TRUE)

# View element
names(jatimCovid)