
library(readr)

Naturalgas <- read_csv("Naturalgas.csv")# Load Natural gas Dataset
View(Naturalgas)
WindData <- read_csv("WindData.csv") #Load Wind Dataset.
View(WindData)
remove_words<-c("a","b","c") # removed unwanted 
Naturalgas$Year<- gsub(paste0(remove_words, collapse = "|"),"",Naturalgas$Year)
Naturalgas$Year
WindData$Year<- gsub(paste0(remove_words, collapse = "|"),"",WindData$Year)
WindData$Year
library(lattice)
hist(Naturalgas$Value) # curve is not normaliy distributed
str(Naturalgas)
str(WindData)
library("ggpubr")
# checking normality line for wind gas values
ggqqplot(wind_gas_value$Value, ylab = "Co2 emmison Wind", ggtheme = theme_minimal())

# checking normality line for natural gas
ggqqplot(Naturalgas_value$Value, ylab = "Co2 emmison Natural gas", ggtheme = theme_minimal())

hist(WindData$Value)
# creating subset of Naturalgas for making the hyposthesis based on value
Naturalgas_value<- subset(Naturalgas,select = c('Value','Data Classification'))
str(Naturalgas_value)
mean(Naturalgas_value$Value)

wind_gas_value<- subset(WindData,select = c('Value','Data Classification'))
mean(wind_gas_value$Value)
str(wind_gas_value)
normality_test<- shapiro.test(Naturalgas_value$Value)
normality_test$p.value # p value < .05 hence it is not normaly distributed.
normality_test

# shaprio test for normality checking
normality_test<- shapiro.test(wind_gas_value$Value)
normality_test$p.value # pvalue is less then .05, data is not normaly distributed
# H0 = co2 emmison have a significant relationship in green energy

# Ha = co2 emission don't have any significant role in green energy
normality_test
# wilcox test for checking the hypothesis, hence it's not normaly distributed
# Going for Wilcox test

boxplot(Naturalgas_value$Value,wind_gas_value$Value)
test<- wilcox.test(Naturalgas_value$Value,wind_gas_value$Value, mu = 0,
alternative = "less",paired = TRUE, conf.int =.99, exact = F, correct=F )
test
# p value less than .05, so have to reject the H0 and go with Ha


#binding new data set


df_new<- rbind(Naturalgas_value,wind_gas_value)# Bind the dataset to find Sd value
df_new
mean(df_new$Value)
mean(Naturalgas_value$Value)
mean(wind_gas_value$Value)
# finding the Standard Deviation For Delta Value
sd(df_new$Value)
# sd = 202.412
# d value = .93 # calculated by substracting the mean value and divide by s.d

install.packages("pwr")
library(pwr)
# find the sample size
power_information<- power.t.test(delta = .93, n = NULL, sig.level = .05, 
                                 power = .90, type = "two.sample", 
                                 alternative = "two.sided")
power_information
# find the power value
power_information<- power.t.test(delta = .93, n = 25.92, sig.level = .05, 
                                 type = "two.sample", 
                                 alternative = "two.sided")
#sample size n = 26

power_information
# power value obtained is = .9

plot(power_information)
# calculate effect size
cohen.ES(test = c("t"), size = c("small")) # to find effect size


# bind the sample data size to 26
sample_data1 <- Naturalgas_value[1:13,]
sample_data1
View(sample_data1)
sample_data2<- wind_gas_value[1:13,]
sample_data2
View(sample_data2)
sample_data_value<- rbind(sample_data1,sample_data2)
sample_data_value

View(sample_data_value)


# effect size = .2
summary(power_information)
# checking for additional unforseen relationship
corr_test<- cor.test(sample_data_value$Value, sample_data_value$Value,method = "spearman")
corr_test


