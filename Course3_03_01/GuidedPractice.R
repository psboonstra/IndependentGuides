library(tidyverse)
library(healthds)
htn_model <- glm(hypertension ~ smoking + age + bmi + activity + salt_group, family = "binomial", data = BPUrban300)

# Video 1: Calibration plot
BPUrban300 %>% 
  mutate(pred_htn_prob = predict(htn_model, newdata = ., type = "response")) %>%
  mutate(pred_htn_prob_cat = cut_number(pred_htn_prob, 10)) %>%
  group_by(pred_htn_prob_cat) %>%
  summarize(pred_htn_prob = mean(pred_htn_prob), 
            obs_htn = mean(hypertension)) %>%
  ggplot() + 
  geom_point(aes(x = pred_htn_prob, y = obs_htn)) + 
  geom_abline(intercept = 0, slope = 1)

BPUrban1000 %>% 
  mutate(pred_htn_prob = predict(htn_model, newdata = ., type = "response")) %>%
  mutate(pred_htn_prob_cat = cut_number(pred_htn_prob, 10)) %>%
  group_by(pred_htn_prob_cat) %>%
  summarize(pred_htn_prob = mean(pred_htn_prob), 
            obs_htn = mean(hypertension)) %>%
  ggplot() + 
  geom_point(aes(x = pred_htn_prob, y = obs_htn)) + 
  geom_abline(intercept = 0, slope = 1)


# Video 2: AUC / ROC
library(pROC)
BPUrban300 %>% 
  mutate(pred_htn_prob = predict(htn_model, newdata = ., type = "response")) %>%
  roc(hypertension, pred_htn_prob, plot = T)

BPUrban1000 %>% 
  mutate(pred_htn_prob = predict(htn_model,  newdata = ., type = "response")) %>%
  roc(hypertension, pred_htn_prob, plot = T) 

# Video 3: Brier
BPUrban300 %>% 
  mutate(pred_htn_prob = predict(htn_model, newdata = ., type = "response")) %>%
  summarize(brier = mean((hypertension - pred_htn_prob)^2), 
            brier_max = mean(pred_htn_prob) * (1 - mean(pred_htn_prob))) %>%
  mutate(scaled_brier = 1 - brier / brier_max)

BPUrban1000 %>% 
  mutate(pred_htn_prob = predict(htn_model,  newdata = ., type = "response")) %>%
  summarize(brier = mean((hypertension - pred_htn_prob)^2), 
            brier_max = mean(pred_htn_prob) * (1 - mean(pred_htn_prob))) %>%
  mutate(scaled_brier = 1 - brier / brier_max)

