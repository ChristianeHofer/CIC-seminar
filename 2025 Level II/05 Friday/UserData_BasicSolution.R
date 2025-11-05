library(readxl)
library(dsa)
library(xts)


# Chile, retail -----------------------------------------------------------

UserData <- read_excel("UserData.xlsx", sheet = "Chile")

chile_retail <- xts::xts(UserData[[2]], as.Date(UserData[[1]]))

chile_retail_dsa <- dsa(
  chile_retail,
  s.window1 = 13,
  s.window3 = 13,
  reg_create = c("Easter"),
  reg_dummy = c(-3, 0),
  cval = 20 # Otherwise single days, such as May 1st and Dec 25 are all modelled as outliers
)
output(chile_retail_dsa)



# India Electricity -------------------------------------------------------

UserData <- read_excel("UserData.xlsx", sheet = "India")

india_ele <- xts::xts(UserData[[2]], as.Date(UserData[[1]]))

india_ele_dsa <- dsa(
  india_ele,
  s.window1 = 21,
  s.window3 = 9
)
output(india_ele_dsa)


# Sri Lanka ASPI -------------------------------------------------------

# Only day-of-the week

UserData <- read_excel("UserData.xlsx", sheet = "Sri Lanka")

aspi <- xts::xts(as.numeric(UserData[[2]]), as.Date(UserData[[1]]))
colnames(aspi) <- "aspi"

aspi[aspi==0] <- NA
aspi <- zoo::na.locf(aspi)


aspi_dsa <- dsa(
  aspi,
  s.window1 = 11,
  s.window3 = 13
)
output(aspi_dsa)

# Korea RailPassenger -------------------------------------------------------


UserData <- read_excel("UserData.xlsx", sheet = "Korea")

rail_passenger <- xts::xts(UserData[[2]], as.Date(UserData[[1]]))


### holidays (Chinese New Year)

cny <- as.Date(c("2017-01-28", "2018-02-16",
                 "2019-02-05", "2020-01-25",
                 "2021-02-12", "2022-02-01",
                 "2023-01-22", "2024-02-10",
                 "2025-01-29", "2026-02-17"))

hol <- holidays$Base
hol[cny] <- 1

restrict <- seq.Date(from=stats::start(rail_passenger),
                     to=stats::end(rail_passenger), by="days")
restrict_forecast <- seq.Date(from=stats::end(rail_passenger)+1,
                              length.out=365, by="days")

AllHol <- merge(lag(hol, -3), lag(hol, -2), lag(hol, -1),
                hol,
                lag(hol, 1), lag(hol, 2))
colnames(AllHol) <- c("CNY_lead3", "CNY_lead2", "CNY_lead1", "CNY", "CNY_lag1", "CNY_lag2")

AllHolUse <- multi_xts2ts(AllHol[restrict])
AllHolForecast <- multi_xts2ts(AllHol[restrict_forecast], short=TRUE)
AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]


rail_passenger_dsa <- dsa(
  rail_passenger,
  s.window1 = 13,
  s.window3 = 9,
  regressor=AllHolUse,
  forecast_regressor = AllHolForecast
)
output(rail_passenger_dsa)


# Colombia, TRM-----------------------------------------------------------

# Only day-of-the-week

UserData <- read_excel("UserData.xlsx", sheet = "Colombia")

trm <- xts::xts(UserData[[2]], as.Date(UserData[[1]]))

trm_dsa <- dsa(
  trm,
  s.window1 = 13,
  s.window3 = 51,
  reg_create = c("Easter"),
  reg_dummy = c(-3, 0),
  cval = 20 # Otherwise single days, such as May 1st and Dec 25 are all modelled as outliers
)
output(trm_dsa)

