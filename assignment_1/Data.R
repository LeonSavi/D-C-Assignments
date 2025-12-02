library(ggplot2)
library(tidyr)
library(dplyr)

set.seed(1)

N <- 1000


# E
# changes in Economics Outlook/Market Sentiment
# Our unobservable variable (they ask us for a Open Door Path)
# my idea is that it can affect Demand And Supply, positive outlook increase in overall Supply and demand

eco_outlook <- rnorm(N,0,100)


# I
# AI Investment: change in cost of investment for short term funds, 
# i.e. change in interest rates (the lower the better) -> if they go up it is more expensive to get funds
# can assume that the central bank wants to control short term inflation and does not care about long term 
# -> therefore inv_cost is referred only to short term funding
inv_cost <- rnorm(N, 0, 2)
# C
# change in raw materials supply: how the production/extraction of commodities change,
# shock/error can be reffered as when a new mine is found or somthing or some disruption in production
# not effected by economic outlook,
commodities <- rnorm(N,0,75)
# S
# changes in supply/Market Capacity of production:
# we can think that for a change in raw material the supply is halved/1.5
# Also the capacity/supply can increase or decrease depending on the interest rate fluctuations,
# positive changes in economic outlook make them increase the supply
supply <- 1.5*commodities + (-20)*inv_cost + 0.5*eco_outlook + rnorm(N,0,20)
# L
# Logistics/Distribution/Inventory changes/prices
# we can think it as the number of trucks needed to meet the shock in supply
# we can also think that interest rates do not have a big impact
# basically logistics companies change prices/costs according to supply
# rigid to E, deeply depends on supply
logistics <- 0.7*supply + (-10)*inv_cost + 0.1*eco_outlook  + rnorm(N,0,25) # maybe we can delete this?
# D
# Aggregate demand AI change, 
# for a rule we can say that the demand is ~twice how much is readily available
# according to the availability(logisitcs) demand is increase
# it is deeply interconnected with the investment cost and volatile (depending on market fluctuations)
# Demand is strongly driven by future perspective (E)
demand <- 0.9*logistics + (-35)*inv_cost  + 0.6*eco_outlook + rnorm(N,0,30)
# P
# change in current market price of RAM: 
# we can assume simply that RAM prices are mainly demand driven, but also the supply have an impact
# demand shock increase the price
# supply shock decrease
# I dont know if we should add E?
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
scatterplot3d(cbind(supply,demand,prices), angle = 10, color=22,
              xlab="capacity", ylab="demand",
              zlab="prices")


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
  E -> D
  E -> S
  E -> L

}
',)

ggdag(dag) +
  theme_dag()
