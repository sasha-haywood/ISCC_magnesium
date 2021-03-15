library(readr)
library(dplyr)
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
  mutate(overall_mean = sum(weighted.mean)) %>%
  mutate(q = (grp1_base_n - 1) * grp1_base_sd^2 + grp1_base_n * grp1_base_mean^2) %>%
  mutate(qc = sum(q)) %>%
  mutate(overall_var = (qc - n * overall_mean^2) / (n-1)) %>%
  mutate(overall_sd = sqrt((qc - n * overall_mean^2) / (n-1)))

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
  mutate(overall_mean = sum(weighted.mean))

groups = groups[!is.na(groups$grp1_base_sd),] # remove 22 rows without sd

groups = groups %>%
  mutate(new_n = sum(grp1_base_n)) %>%
  mutate(q = (grp1_base_n - 1) * grp1_base_sd^2 + grp1_base_n * grp1_base_mean^2) %>%
  mutate(qc = sum(q)) %>%
  mutate(overall_var = (qc - new_n * overall_mean^2) / (n-1)) %>%
  mutate(overall_sd = sqrt((qc - new_n * overall_mean^2) / (n-1)))

report2 = groups[,c(2, 9, 14)]

# compare difference in means, depending on when we remove the lines with missing sd
unique(report1)
unique(report2)

# rows with NaN overall sd have only one observation, so overall sd is same as original sd
