library(readr)
library(dplyr)
library(tidyr)
iMg = read_csv("iMg_Results03Final.csv")
iMg = iMg[,c(2, 5, 8, 10, 11, 13)] # 219 rows
iMg = iMg[!is.na(iMg$grp1_base_mean),] # remove 1 rows without means
iMg = iMg[!is.na(iMg$grp1_base_n),] # remove 4 rows without sample sizes
iMg = unite(iMg, group, 2:3, remove = T)

iMg = iMg[!is.na(iMg$sd_2),] # remove 9 rows without sd
  
# sort by groups of condition/measurement and calculate total n,
# mean, and sd.  (q, qc, and var are steps to find sd)

groups = iMg %>%
  group_by(group) %>%
  mutate(n = sum(grp1_base_n)) %>%
  mutate(weighted.mean = grp1_base_mean * grp1_base_n / (n-1)) %>%
  mutate(mean = sum(weighted.mean)) %>%
  mutate(q = (grp1_base_n - 1) * sd_2^2 + grp1_base_n * grp1_base_mean^2) %>%
  mutate(qc = sum(q)) %>%
  mutate(var = (qc - n * mean^2) / (n-1)) %>%
  mutate(sd = sqrt((qc - n * mean^2) / (n-1)))

# report only those columns (condition/measurement, mean, and sd)
report1 = groups[,c(2, 8, 12)]
# We only need one entry for each condition/measurement
m1 = unique(report1)

######### alternate code 
########## this code leaves in the lines that only have mean until overall mean has been calculated
############ and then it removes those lines and recalculates n and calculates overall sd

iMg = read_csv("iMg_Results03Final.csv")
iMg = iMg[,c(2, 5, 8, 10, 11, 13)] # 219 rows
iMg = iMg[!is.na(iMg$grp1_base_mean),] # remove 1 rows without means
iMg = iMg[!is.na(iMg$grp1_base_n),] # remove 4 rows without sample sizes
iMg = unite(iMg, group, 2:3, remove = T)

groups = iMg %>%
  group_by(group) %>%
  mutate(n = sum(grp1_base_n)) %>%
  mutate(weighted.mean = grp1_base_mean * grp1_base_n / (n-1)) %>%
  mutate(mean = sum(weighted.mean))

groups = groups[!is.na(groups$sd_2),] # remove 9 rows without sd

groups = groups %>%
  mutate(new_n = sum(grp1_base_n)) %>%
  mutate(q = (grp1_base_n - 1) * sd_2^2 + grp1_base_n * grp1_base_mean^2) %>%
  mutate(qc = sum(q)) %>%
  mutate(var = (qc - new_n * mean^2) / (n-1)) %>%
  mutate(sd = sqrt((qc - new_n * mean^2) / (n-1)))

report2 = groups[,c(2, 8, 13)]

m2 = unique(report2)

# rows with NaN overall sd have only one observation, so overall sd is same as 
# original sd.  Add those in manually.  If data is changed, this code 
# may have to change

m1[10,3] = 0.077
m2[10,3] = 0.077

m1 = m1[order(m1$group),] 
m2 = m2[order(m2$group),] 

# calculate lower and upper bounds
m1 = m1 %>%
  mutate(lower = mean - 1.96 * sd) %>%
  mutate(upper = mean + 1.96 * sd)
m2 = m2 %>%
  mutate(lower = mean - 1.96 * sd) %>%
  mutate(upper = mean + 1.96 * sd)

# make it look pretty
m1 = separate(m1, group, c("health condition", "metric"), sep = "_")
m2 = separate(m2, group, c("health condition", "metric"), sep = "_")

m1
m2


