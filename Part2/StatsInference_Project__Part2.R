# Loading the dataset
library(datasets)
data("ToothGrowth")

# sample size n
length(ToothGrowth$len) #or nrows(ToothGrowth)

# Viewing the object types in the data
str(ToothGrowth)

# Dimensions of the dataset
dim(ToothGrowth)

# Summary of the data
summary(ToothGrowth)

# table view of the data
table(ToothGrowth$supp,ToothGrowth$dose)

# Box plot
library(ggplot2)
ggplot(ToothGrowth, aes(x = factor(dose), y = len, fill = factor(dose)))+
  geom_boxplot()+
  facet_grid(.~supp)+
  labs(title = "Tooth Length vs. Dose  by for OJ & VC",
       x = "Doses", y = "Tooth Length")

library(magrittr)
library(dplyr)

# Hypothesis test for 0.5mg Dose
ojdose0.5 <- ToothGrowth %>% filter(supp=="OJ" & dose=="0.5")
vcdose0.5 <- ToothGrowth %>% filter(supp=="VC" & dose=="0.5")
t.test(ojdose0.5$len,vcdose0.5$len)

# Hypothesis test for 1mg Dose
ojdose1 <- ToothGrowth %>% filter(supp=="OJ" & dose=="1")
vcdose1 <- ToothGrowth %>% filter(supp=="VC" & dose=="1")
t.test(ojdose1$len,vcdose1$len)

# Hypothesis test for 2mg Dose
ojdose2 <- ToothGrowth %>% filter(supp=="OJ" & dose=="2")
vcdose2 <- ToothGrowth %>% filter(supp=="VC" & dose=="2")
t.test(ojdose2$len,vcdose2$len)