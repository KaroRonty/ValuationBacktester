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

for (i in 1:I(nrow(full_data) - 2)){
  full_data$index[i+2] <- (full_data$index[i+1] + full_data$D[i+1] / 12) * full_data$diff[i+2]
}
# Calculate ten year returns
for (i in 1:I(nrow(full_data) - 1)){
  full_data$tenyear[i+1] <- (full_data$index[i+121] / full_data$index[i+1]) ^ 0.1
}
# Change the criteria and settings below, rownames act as criteria values
# Backtest for CAPE
temp <- as.data.frame(matrix(nrow = 45-4, ncol = 1))
for (i in 4:45){
  x <- filter(full_data, CAPE < i + 1 & CAPE > i)
  temp[i,1] <- mean(x$tenyear, na.rm = T)
}
plot(rownames(temp), temp$V1, xlab = "CAPE", ylab = "Returns")
abline(lm(temp$V1 ~ as.numeric(rownames(temp))))

# Another backtest for P/E
full_data$PE <- full_data$P / full_data$E
temp <- as.data.frame(matrix(nrow = 55-5, ncol = 1))
for (i in 5:55){
  x <- filter(full_data, PE < i + 1 & PE > i)
  temp[i,1] <- mean(x$tenyear, na.rm = T)
}
plot(rownames(temp), temp$V1, xlab = "P/E", ylab = "Returns")
abline(lm(temp$V1 ~ as.numeric(rownames(temp))))

