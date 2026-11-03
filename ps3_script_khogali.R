
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
