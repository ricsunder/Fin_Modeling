library(FinCal)
library(tidyquant)
library(jrvFinance)
library(ggthemes)
library(dygraphs)

# calculating Payment with equal payment
r <- 0.1
cf <- tribble(~Year,~cf,
              1,100,
              2,100,
              3,100,
              4,100,
              5,100)

#Calculating the PV of the cash flows
cf %>%
  mutate(pv = cf/(1+r)^Year) %>%
  .[[3]] %>%
  sum()

# Calculating the pv of uneven payments

cf <- tribble(~Year,~cf,
              1,100,
              2,200,
              3,300,
              4,400,
              5,500)

#Calculating the PV of the uneven cash flows
cf %>%
  mutate(pv = cf/(1+r)^Year) %>%
  .[[3]] %>%
  sum()

#Calculating PV with negative cash

cf <- tribble(~Year,~cf,
              0,-250,
              1,100,
              2,100,
              3,100,
              4,100,
              5,100)
              
cf %>%
  mutate(pv = cf/(1+r)^Year) %>%
  .[[3]] %>%
  sum()


#Value of finite Annuity

pmt <- 1000
n <- 5
r <- 0.12

cf <- tibble(period = 1:5,
             pmt = pmt)

cf %>%
  mutate(pv = pmt/(1+r)^period) %>%
  .[[3]] %>%
  sum()

#Value of infinite annuity

pmt <- 1000
r <- 0.12

annuity <- pmt/r
annuity

#calculating the PV of gr


cf <- tribble(~Year,~cf,
              0,-250,
              1,100,
              2,100,
              3,100,
              4,100,
              5,100)




