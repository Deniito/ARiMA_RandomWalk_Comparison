library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

# Data prep
stock_data <- read_csv("CIK0000320193.csv")

close_prices <- stock_data$close
log_returns <- diff(log(close_prices))

split_index <- floor(0.8 * length(log_returns))

train <- window(log_returns, end = c(time(log_returns)[split_index]))
test <- window(close_prices, start = c(time(close_prices)[split_index + 1]))

start_price <- close_prices[length(train)]

# Random Walk
mu_hat <- mean(train, na.rm = TRUE)
sigma_hat <- sd(train, na.rm = TRUE)

simulate_walk <- function(start_price, n, mu, sigma) {
  steps <- rnorm(n, mean = mu, sd = sigma)
  prices <- start_price * exp(cumsum(steps))
  return(prices)
}

simulated_walk <- simulate_walk(start_price = start_price,
                                n = length(test),
                                mu = mu_hat,
                                sigma = sigma_hat)

# ARiMA
adf.test(train)

arima_model <- auto.arima(train)
arima_forecast <- forecast(arima_model, h = length(test))
simulated_arima <- start_price * exp(cumsum(arima_forecast$mean))

# Plotting
plot_df <- tibble(
  Day = 1:length(test),
  Real = test,
  Gaussian_RW = simulated_walk,
  ARIMA = simulated_arima
)

ggplot(plot_df, aes(x = Day)) +
  geom_line(aes(y = Real, color = "Real Data")) +
  geom_line(aes(y = Gaussian_RW, color = "Gaussian Random Walk")) +
  geom_line(aes(y = ARIMA, color = "ARIMA Forecast")) +
  labs(title = "Real vs Simulated Stock Price (Random Walk & ARIMA)",
       y = "Price", color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Real Data" = "blue",
                                "Gaussian Random Walk" = "red",
                                "ARIMA Forecast" = "green"))
