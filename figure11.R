library(tidyverse) 

micro_world <- 
  read_csv("micro_world.csv")

micro_world_account <-
  micro_world %>%
  group_by(economycode) %>%
  summarize(account = 
              100 * weighted.mean(account, w = wgt))

wb_income <- 
  read_csv("wb_income.csv")

account_by_income <-
  left_join(micro_world_account, 
            wb_income)

figure1.1 <- 
  ggplot(data = account_by_income, 
         mapping = aes(x = account,
                       y = income_category,
                       color = income_category)) + 
  geom_point()

ggplot(account_by_income, 
       aes(account,
           income_category,
           color = income_category)) + 
  #geom_point() +
  scale_x_continuous(#breaks = c(0, 20, 40, 60, 80, 100), 
                     limits = c(0, 100)) +
  geom_boxplot()



##################

ggplot(data = account_by_income) + 
  geom_histogram(mapping = aes(x = account, 
                               fill = income_category), 
                 binwidth = 5)

ggplot(data = account_by_income) + 
  geom_bar(aes(x = income_category))

ggplot(data = account_by_income) + 
  geom_boxplot(mapping = aes(x = account, y = income_category))

