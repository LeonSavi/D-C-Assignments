library(ggplot2)
library(tidyr)
library(dplyr)

set.seed(1)

N <- 10000


# AI Investment: change in cost of investment for short term funds, i.e. interest rates (the lower the better)
inv_cost <- rnorm(N, 0, 3)

# raw materials supply: production/extraction of commodities: fixed production,
# shock/error can be reffered as when a new mine is found or somthing or some disruption in production
commodities <- rnorm(N,100,15)

# Supply/Market Capacity of production: the capacity/supply can increase or decrease depending on the investment cost, 
capacity <- 2*commodities + (-20)*inv_cost + rnorm(N,0,20)

# Logistics/Distribution costs
logistics <- 0.7*capacity + (-10)*inv_cost + rnorm(N,0,25)

# Aggregate demand AI, since is a DAG cannot depend on RAM prices
demand <- 1.8*logistics + (-30)*inv_cost + rnorm(N,0,50)

# current market price of RAM: 
RAMp <- 1.8*demand + (-0.3)*capacity + rnorm(N,0,20)

df <- data.frame(
  Investment_cost = inv_cost,
  commodities = commodities,
  capacity = capacity,
  logistics = logistics,
  demand = demand,
  RAM_prices = RAMp
)




# check distribution, done fast
df_long <- df %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Value")

ggplot(df_long, aes(x = Value)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  theme_minimal(base_size = 14) +
  labs(title = "Distributions of Variables",
       x = "Value",
       y = "Density")


# check output
library("scatterplot3d")
scatterplot3d(cbind(capacity,demand,capacity), angle = 10, color=22,
              xlab="capacity", ylab="demand",
              zlab="RAMp")
