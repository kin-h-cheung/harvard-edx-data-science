# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500

head(brexit_polls)

p*N #722
sqrt(p*(1-p)*N) #19.4
X_hat <- (p*N)+N*(1-p)
se_hat <- sqrt(p*(1-p))/sqrt(N) #0.0129
d #-0.028
N*d #-57
#sqrt(d*(1-d)*N)
sqrt(d*(1-d))/sqrt(N)
p - (1-p) #-0.109
2*sqrt(0.481*(0.519)/N) #0.0258 se of spread (d)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
head(brexit_polls)
mean(brexit_polls$spread) #0.0201
sd(brexit_polls$spread) #0.0588
mean(brexit_polls$x_hat) #0.51
sd(brexit_polls$x_hat) #0.0294

brexit_polls[1,]
N <- 4772
p <- 0.48
#X_hat <- (p*N)+N*(1-p)
x_hat <- 0.52
se_hat <- sqrt(p*(1-p))/sqrt(N) #0.0129
X_hat #4772
qnorm(0.975)*se_hat #0.0142
4772 - 0.0142
x_hat - qnorm(0.975)*se_hat #0.506
x_hat + qnorm(0.975)*se_hat #0.534

library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

p <- 0.481
p_hat <- (d+1)/2
p_hat
#d <- −0.038
june_polls <- brexit_polls %>% filter(enddate >= '2016-06-01') %>%
  mutate(
    se_x_hat = 2*sqrt(x_hat*(1-x_hat)/samplesize),
    se_spread = 2*sqrt(se_x_hat*(1-se_x_hat)/samplesize),
    lower = se_spread-qnorm(0.975)*se_x_hat,
    upper = se_spread+qnorm(0.975)*se_x_hat,
    hit = (-0.038 >= lower & -0.038 <= upper)
  )
head(june_polls)
nrow(june_polls)
mean(june_polls$hit)
(between(0, june_polls$lower, june_polls$upper))
min(june_polls$lower)
max(june_polls$upper)
june_polls

june_polls2 <- brexit_polls %>% filter(enddate >= '2016-06-01') %>%
  mutate(
    se_x_hat = 2*sqrt(x_hat*(1-x_hat)/samplesize),
    se_spread = 2*sqrt(se_x_hat*(1-se_x_hat)/samplesize),
    lower = se_spread-qnorm(0.975)*se_x_hat,
    upper = se_spread+qnorm(0.975)*se_x_hat,
    hit = (-0.038 >= lower & -0.038 <= upper)
  ) %>% group_by(pollster) %>% summarize(m = mean(hit)) %>% arrange(desc(m))
june_polls2

head(june_polls)
june_polls %>% ggplot(aes(x=poll_type, y=spread)) + geom_point() + geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type %>% mutate(
  se_x_hat = 2*sqrt(p_hat*(1-p_hat)/N),
  lower = spread-qnorm(0.975)*se_x_hat,
  upper = spread+qnorm(0.975)*se_x_hat,
  interval = upper - lower
) 
#%>% select(poll_type, interval)

data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_hit2 <- brexit_hit %>% group_by(poll_type, hit) %>% 
  summarize(num = n()) %>% spread(poll_type, num)
chisq_test <- brexit_hit2 %>% chisq.test()
chisq_test$p.value

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038,
         p_hit = spread_lower >= 0.00133 & spread_upper <= 0.00133) 

brexit_hit2
brexit_hit_2 <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))