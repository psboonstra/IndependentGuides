library(tidyverse) 
micro_world <- 
  read_csv("micro_world.csv")

micro_world %>%
  group_by(economy) %>% 
  summarize(account = 
              weighted.mean(account, w = wgt))

# calculate the design effects and margin of error as defined in 
# the footenote of table A.1
moe <- 
  micro_world %>% 
  group_by(economy, pop_adult) %>% 
  summarize(design_effect = n() * sum(wgt^2) / (sum(wgt)^2),
            moe = 100 * sqrt(0.25 / n()) * 1.96 * sqrt(design_effect),
            .groups = "drop")

# overall proportions of those holding an account
overall_account <- 
  micro_world %>%
  group_by(economy) %>% 
  summarize(account = 
              100 * weighted.mean(account, w = wgt),
            .groups = "drop")

# gender gap in holding accounts
account_gap_female <- 
  micro_world %>%
  group_by(economy, female) %>% 
  summarize(account = 
              100 * weighted.mean(account, w = wgt),
            .groups = "drop") %>%
  pivot_wider(id_cols = economy, 
              names_from = female, 
              names_prefix = "female",
              values_from = account) %>%
  mutate(gap_female = female1 - female2) %>%
  select(economy, gap_female)

# income gaps in holding accounts
account_gap_inc <- 
  micro_world %>%
  mutate(lower_inc = inc_q <= 2) %>%
  group_by(economy, lower_inc) %>% 
  summarize(account = 
              100 * weighted.mean(account, w = wgt),
            .groups = "drop") %>%
  pivot_wider(id_cols = economy, 
              names_from = lower_inc, 
              names_prefix = "lower_inc",
              values_from = account) %>%
  mutate(gap_inc = lower_incFALSE - lower_incTRUE) %>%
  select(economy, gap_inc)

# join the results above to make the table, 
# rounding the results
indicator_table <- 
  full_join(overall_account, 
            account_gap_female, 
            by = "economy") %>%
  full_join(account_gap_inc, 
            by = "economy") %>%
  mutate(account = round(account),
         gap_female = round(gap_female),
         gap_inc = round(gap_inc))

# auxiliary table,
# dropping the results that
# fall below the margin of error
aux_indicator_table <- 
  full_join(overall_account, 
            account_gap_female, 
            by = "economy") %>%
  full_join(account_gap_inc, 
            by = "economy")  %>%
  full_join(moe, 
            by = "economy") %>%
  mutate(account = round(account), 
         gap_female = 
           case_when(
             abs(gap_female) > moe ~ as.character(round(gap_female)), 
             TRUE ~ "-"
           ),
         gap_inc = 
           case_when(
             abs(gap_inc) > moe ~ as.character(round(gap_inc)), 
             TRUE ~ "-"
           )) %>% 
  select(economy, account, gap_female, gap_inc)
