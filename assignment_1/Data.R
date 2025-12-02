library(ggplot2)
library(tidyr)
library(dplyr)

set.seed(1)

N <- 10000

# I
# AI Investment: change in cost of investment for short term funds, 
# i.e. change in interest rates (the lower the better) -> if they go up it is more expensive to get funds
inv_cost <- rnorm(N, 0, 2)
# C
# change in raw materials supply: how the production/extraction of commodities change,
# shock/error can be reffered as when a new mine is found or somthing or some disruption in production
commodities <- rnorm(N,0,75)
# S
# changes in supply/Market Capacity of production:
# we can think that for a change in raw material the supply is halved/1.5
# Also the capacity/supply can increase or decrease depending on the interest rate fluctuations, 
supply <- 1.5*commodities + (-20)*inv_cost + rnorm(N,0,20)
# L
# Logistics/Distribution/Inventory changes/prices
# we can think it as the number of trucks needed to meet the shock in supply
# we can also think that interest rates do not have a big impact
# basically logistics companies change prices/costs according to supply
logistics <- 0.7*supply + (-10)*inv_cost + rnorm(N,0,25) # maybe we can delete this?
# D
# Aggregate demand AI change, 
# for a rule we can say that the demand is ~twice how much is readily available
# according to the availability(logisitcs) demand is increase
# it is deeply interconnected with the investment cost and volatile (depending on market fluctuations)
demand <- 0.9*logistics + (-35)*inv_cost + rnorm(N,0,60)
# P
# change in market price of RAM: 
# we can assume simply that RAM prices are mainly demand driven, but also the supply have an impact
# demand shock increase the price
# supply shock decrease
prices <- 0.7*demand + (-0.3)*supply + rnorm(N,0,15)

# df
df <- data.frame(
  Investment_cost = inv_cost,
  commodities = commodities,
  supply = supply,
  logistics = logistics,
  demand = demand,
  prices = prices
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


# dag
library(ggplot2)
library(dagitty)
library(ggdag)

dag <- dagitty('

dag {
  I -> S
  I -> L
  I -> D
  C -> S
  S -> L
  L -> D
  S -> P
  D -> P

}
',)

ggdag(dag) +
  theme_dag()
