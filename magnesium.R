library(readr)
library(dplyr)
library(tidyr)
iMg = read_csv("iMg_EXTRACTION_Resultsfinal_Ansu.csv")
iMg = iMg[,c(1, 4, 7, 9, 10, 11, 12)] # 261 rows
iMg = iMg[!is.na(iMg$grp1_base_mean),] # remove 18 rows without means
iMg = iMg[!is.na(iMg$grp1_base_n),] # remove 10 rows without sample sizes
iMg = unite(iMg, group, 2:3, remove = T)

# replace missing sd with sd calculated from se and n
missing.sd = is.na(iMg$grp1_base_sd)
iMg$grp1_base_sd[missing.sd] = with(iMg, grp1_base_se[missing.sd] * 
                                      sqrt(grp1_base_n[missing.sd]))
iMg = iMg[!is.na(iMg$grp1_base_sd),] # remove 22 rows without sd
  
groups = iMg %>%
  group_by(group) %>%
  mutate(n = sum(grp1_base_n)) %>%
  mutate(weighted.mean = grp1_base_mean * grp1_base_n / (n-1)) %>%
  mutate(mean = sum(weighted.mean)) %>%
  mutate(q = (grp1_base_n - 1) * grp1_base_sd^2 + grp1_base_n * grp1_base_mean^2) %>%
  mutate(qc = sum(q)) %>%
  mutate(var = (qc - n * mean^2) / (n-1)) %>%
  mutate(sd = sqrt((qc - n * mean^2) / (n-1)))

report1 = groups[,c(2, 9, 13)]
unique(report1)


######### alternate code 
########## this code leave in the lines that only have mean until overall mean has been calculated
############ and then it removes those lines and recalculates n and calculates overall sd

iMg = read_csv("iMg_EXTRACTION_Resultsfinal_Ansu.csv")
iMg = iMg[,c(1, 4, 7, 9, 10, 11, 12)] # 261 rows
iMg = iMg[!is.na(iMg$grp1_base_mean),] # remove 18 rows without means
iMg = iMg[!is.na(iMg$grp1_base_n),] # remove 10 rows without sample sizes
iMg = unite(iMg, group, 2:3, remove = T)

# replace missing sd with sd calculated from se and n
missing.sd = is.na(iMg$grp1_base_sd)
iMg$grp1_base_sd[missing.sd] = with(iMg, grp1_base_se[missing.sd] * 
                                      sqrt(grp1_base_n[missing.sd]))


groups = iMg %>%
  group_by(group) %>%
  mutate(n = sum(grp1_base_n)) %>%
  mutate(weighted.mean = grp1_base_mean * grp1_base_n / (n-1)) %>%
  mutate(mean = sum(weighted.mean))

groups = groups[!is.na(groups$grp1_base_sd),] # remove 22 rows without sd

groups = groups %>%
  mutate(new_n = sum(grp1_base_n)) %>%
  mutate(q = (grp1_base_n - 1) * grp1_base_sd^2 + grp1_base_n * grp1_base_mean^2) %>%
  mutate(qc = sum(q)) %>%
  mutate(var = (qc - new_n * mean^2) / (n-1)) %>%
  mutate(sd = sqrt((qc - new_n * mean^2) / (n-1)))

report2 = groups[,c(2, 9, 14)]

# compare difference in means, depending on when we remove the lines with missing sd
m1 = unique(report1)
m2 = unique(report2)

# rows with NaN overall sd have only one observation, so overall sd is same as original sd
m1[10,3] = 0.080
m2[10,3] = 0.080
m1[11,3] = 0.055
m2[11,3] = 0.055

m1 = m1[order(m1$group),] 
m2 = m2[order(m2$group),] 

m1 = m1 %>%
  mutate(lower = mean - 1.96 * sd) %>%
  mutate(upper = mean + 1.96 * sd)
m2 = m2 %>%
  mutate(lower = mean - 1.96 * sd) %>%
  mutate(upper = mean + 1.96 * sd)

m1 = separate(m1, group, c("health condition", "metric"), sep = "_")
m2 = separate(m2, group, c("health condition", "metric"), sep = "_")

m1[6,2] = "Serum Mg"
m2[6,2] = "Serum Mg"

