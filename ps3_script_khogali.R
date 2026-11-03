
# 2.2a
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)


# 2.2b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# 2.3a
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)


# 2.4a
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

# 2.4b
head(retail_ts)
has_gaps(retail_ts)

# 2.5a
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

# 2.5b
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)
head(can_gas)

# 2.6a
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# 2.6b
can_gas_recent <- can_gas_recent |>
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# 2.6c
can_gas_recent |>
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# 2.6d
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)
can_gas_q |> autoplot(Volume)

# 2.7a
retail_ts |> autoplot(sales)

# 2.7b
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# 2.7c
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Monthly Natural Gas Production in Canada",
    subtitle = "January 1960 - February 2005",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()

# 2.7d
can_gas |>
  autoplot(Volume) +
  geom_smooth(se = FALSE, color = "red") +
  labs(
    title = "Canadian Gas Production with a Loess Trend",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()

# 3.1b
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(title = "Log of Canadian Gas Production", x = NULL, y = "log(Volume)") +
  theme_minimal()

# 3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales", x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "Log of NSA Retail Sales", x = NULL, y = "log(sales)") +
  theme_minimal()

# 3.2a
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(title = "Monthly Growth Rate of Canadian Gas Production", x = NULL, y = "Growth rate (%)") +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(title = "Gap Between Direct and Log Difference Growth Rates", x = NULL, y = "Percentage points") +
  theme_minimal()

summary(can_gas$growth_gap)

# 3.3
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# 4.1a
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(title = "Seasonal Pattern in NSA Retail Sales", y = "Millions of dollars") +
  theme_minimal()

# 4.1b
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(title = "Seasonal Pattern in SA Retail Sales", y = "Millions of dollars") +
  theme_minimal()

# 4.1c
can_gas |>
  gg_season(log_volume) +
  labs(title = "Seasonal Pattern in Canadian Gas Production", y = "log(Volume)") +
  theme_minimal()

# 4.2
can_gas |>
  gg_subseries(log_volume) +
  labs(title = "Subseries Plot: Canadian Gas Production", y = "log(Volume)")

# 4.3a
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# average seasonal factor for each month
retail_nsa_x13 |>
  as_tibble() |>
  mutate(month = month(Month, label = TRUE)) |>
  group_by(month) |>
  summarize(avg_seasonal = mean(seasonal))

# 4.3b
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

summary(sa_compare$pct_diff)

# 4.4a
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# 4.4b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# 4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
