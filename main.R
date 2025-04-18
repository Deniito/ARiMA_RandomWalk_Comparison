library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

# ---- Data Preparation ----
stock_data <- read_csv("CIK0000320193.csv")
close_prices <- stock_data$close
log_returns <- diff(log(close_prices))

split_index <- floor(0.8 * length(log_returns))
train <- window(log_returns, end = c(time(log_returns)[split_index]))
test <- window(close_prices, start = c(time(close_prices)[split_index + 1]))
start_price <- close_prices[length(train)]

# ---- Random walk ----
mu_hat <- mean(train, na.rm = TRUE)
sigma_hat <- sd(train, na.rm = TRUE)

simulate_walk <- function(start_price, n, mu, sigma) {
  steps <- rnorm(n, mean = mu, sd = sigma)
  prices <- start_price * exp(cumsum(steps))
  return(prices)
}

# ---- 90% range of random walk ----
n_simulations <- 1000
n_days <- length(test)
sim_matrix <- replicate(n_simulations, simulate_walk(start_price, n_days, mu_hat, sigma_hat))

# ---- ARIMA ----
#adf.test(train)  #Stationarity test

arima_model <- auto.arima(train)
arima_forecast <- forecast(arima_model, h = n_days)
simulated_arima <- start_price * exp(cumsum(arima_forecast$mean))

summary(arima_model)

# ---- Plot ----
percentiles_df <- tibble(
  Day = 1:n_days,
  P5 = apply(sim_matrix, 1, quantile, probs = 0.05),
  P95 = apply(sim_matrix, 1, quantile, probs = 0.95),
  P50 = apply(sim_matrix, 1, quantile, probs = 0.50),
  Real = test,
  ARIMA = simulated_arima,
  RW1 = sim_matrix[, 1] #to demonstrate the potential random walk
)

ggplot(percentiles_df, aes(x = Day)) +
  geom_ribbon(aes(ymin = P5, ymax = P95, fill = "90% Confidence Band"), alpha = 0.1) +
  geom_line(aes(y = P50, color = "Random Walk Mean"), linetype = "dashed", size = 1.0) +
  geom_line(aes(y = Real, color = "Real Stock Price"), size = 0.8) +
  geom_line(aes(y = ARIMA, color = "ARIMA Forecast"), linetype = "dashed", size = 0.8) +
  geom_line(aes(y = RW1, color = "Potential Random Walk"), size = 0.8, alpha = 0.5) +
  labs(title = "Stock Price Forecast: Real vs Random Walks (90%) & ARIMA",
       y = "Price", x = "Day") +
  scale_fill_manual(name = "Bands:", values = c("90% Confidence Band" = "red")) +
  scale_color_manual(name = "Lines:",
                     values = c("Real Stock Price" = "blue",
                                "Random Walk Mean" = "red",
                                "ARIMA Forecast" = "green",
                                "Potential Random Walk" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")


