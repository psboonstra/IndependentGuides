library(tidyverse)
library(healthds)

# Will start the video with this code already there. I will explain what 
# this code does and show how we are using
# some of what we learned in Course 1 plus some additional things
smoking_by_hypertension <- 
  BPUrban1000 %>%
  group_by(smoking) %>%
  summarize(num_htn = sum(hypertension),
            num_no_htn = sum(1 - hypertension)) %>%
  column_to_rownames("smoking") %>%
  as.matrix()

#ztest
prop.test(x = smoking_by_hypertension)
# equivalent way to use prop.test
prop.test(x = c(186, 26), n = c(186 + 733, 26 + 55))

#fishers
fisher.test(x = smoking_by_hypertension)
            
# Then emphasize importance of proper interpretation:
smoking_by_hypertension[2:1,]
fisher.test(x = smoking_by_hypertension[2:1,])
            
            
            