library(dplyr) # data formatting

# Retrieve data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

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
abline(lm(cape_quantiles$returns[1:9] ~ cape_quantiles$cape[1:9]))

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
