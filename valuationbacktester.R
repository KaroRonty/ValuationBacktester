library(httr) # downloading the xls(x) files
library(readxl) # reading xls(x) files
library(dplyr) # data formatting

# -------------------------------- Read Shiller data
GET("http://www.econ.yale.edu/~shiller/data/ie_data.xls", write_disk(temp <- tempfile(fileext = ".xls")))
shillerdata <- read_xls(temp, sheet = 3, skip = 7)

# Format the years and months correctly
corrected_dates <- expand.grid(1:12, 1871:2018)
last_month <- length(grep("2018", shillerdata$Date))
months_to_be_cut_off <- 12 - last_month
corrected_dates <- head(corrected_dates, nrow(corrected_dates) - months_to_be_cut_off)

# Add leading zeros
corrected_dates$Var1 <- sprintf("%02d", as.numeric(corrected_dates$Var1))
dates <- as.data.frame(paste(corrected_dates$Var2, corrected_dates$Var1, sep = "-"))
names(dates) <- "dates"

# Remove possible excess rows & add corrected dates back
shillerdata <- head(shillerdata, nrow(dates))
shillerdata <- cbind(dates, shillerdata)
shillerdata$Date <- NULL

# -------------------------------- Read Goyal data
GET("http://www.hec.unil.ch/agoyal/docs/PredictorData2017.xlsx", write_disk(temp <- tempfile(fileext = ".xls")))
goyaldata <- read_xlsx(temp, sheet = 1)
goyaldata <- select(goyaldata, c("yyyymm", "b/m"))

# Make dates into same format as above and prepare names for joining
goyaldata$yyyymm <- paste(substr(goyaldata$yyyymm, 1, 4), substr(goyaldata$yyyymm, 5, 6), sep = "-")
names(goyaldata) <- c("dates", "bm")

full_data <- full_join(shillerdata, goyaldata, by = "dates")

# --------------------------------  Replace written NAs with real NAs
full_data$bm[full_data$bm == "NaN"] <- NA
full_data$CAPE[full_data$CAPE == "NA"] <- NA
full_data$CAPE <- as.numeric(full_data$CAPE)

# --------------------------------
# Calculate returns for the next 10 years
# First calculate the daily returns
full_data$diff <- (lag(lead(full_data$P) / full_data$P))
# Then calculate an index including dividends
full_data$index <- NA
full_data$index[2] <- (full_data$P[1] + full_data$D[1] / 12) * full_data$diff[2]

for (i in 1:I(nrow(full_data) - 2)) {
  full_data$index[i + 2] <- (full_data$index[i + 1] + full_data$D[i + 1] / 12) * full_data$diff[i + 2]
}
# Calculate ten year returns
for (i in 1:I(nrow(full_data) - 1)) {
  full_data$tenyear[i + 1] <- (full_data$index[i + 121] / full_data$index[i + 1])^0.1
}
# Backtest for CAPE
cape_quantiles <- as.data.frame(quantile(full_data$CAPE, seq(0.1, 0.9, by = 0.1), na.rm = T))
cape_quantiles$returns <- cbind(rep(NA, 9))
colnames(cape_quantiles) <- c("cape", "returns")
cape_quantiles <- rbind(c(-Inf, NA), cape_quantiles, c(Inf, NA))

for (i in 1:10) {
  full_data %>%
    filter(
      CAPE > cape_quantiles[i, 1],
      CAPE < cape_quantiles[i + 1, 1]
    ) %>%
    summarise(mean(tenyear, na.rm = T)) -> cape_quantiles[i + 1, 2]
}

cape_quantiles <- cape_quantiles[2:11, ]
plot(cape_quantiles)
abline(lm(cape_quantiles$returns[1:9] ~ cape_quantiles$capes[1:9]))

# Backtest for P/B
full_data$pb <- 1 / as.numeric(full_data$bm)
pb_quantiles <- as.data.frame(quantile(full_data$pb, seq(0.1, 0.9, by = 0.1), na.rm = T))
pb_quantiles$returns <- cbind(rep(NA, 9))
colnames(pb_quantiles) <- c("pb", "returns")
pb_quantiles <- rbind(c(-Inf, NA), pb_quantiles, c(Inf, NA))

for (i in 1:10) {
  full_data %>%
    filter(
      pb > pb_quantiles[i, 1],
      pb < pb_quantiles[i + 1, 1]
    ) %>%
    summarise(mean(tenyear, na.rm = T)) -> pb_quantiles[i + 1, 2]
}

pb_quantiles <- pb_quantiles[2:11, ]
plot(pb_quantiles)
abline(lm(pb_quantiles$returns[1:9] ~ pb_quantiles$pb[1:9]))
