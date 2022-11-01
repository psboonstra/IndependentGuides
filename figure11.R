library(tidyverse) 

micro_world <- 
  read_csv("micro_world.csv")

wb_income <- 
  read_csv("wb_income.csv")

account_by_income <-
  left_join(micro_world, 
            wb_income) %>%
  group_by(economy, income_category) %>% 
  summarize(account = 
              100 * weighted.mean(account, w = wgt))

ggplot(data = account_by_income) + 
  geom_point(aes(x = account, y = income_category, color = income_category))


micro_world %>%
  filter(inc_q == 1 | inc_q == 2) %>%
  group_by(economy) %>%
  summarize(x = account, 100 * weighted.mean(x, w = wgt)) 
