install.packages("Ecdat")
library(Ecdat)
head(Cigarette)
View(Cigarette)

library("ggplot2")
library("dplyr")

# Create boxplot of average # of packs per capita by state. Highest? Lowest? 
ggplot(Cigarette, aes(x = state, y = packpc)) + geom_boxplot()

Cigarette %>% group_by(state) %>% 
  summarise(Mean = mean(packpc)) %>% arrange(Mean)
#Lowest = UT @ 56.8 packpc

Cigarette %>% group_by(state) %>% 
  summarise(Mean = mean(packpc)) %>% arrange(desc(Mean))
#Highest = KY @ 174 packpc

#Find the Median #of packs per capita for each year
MedianPPC <- Cigarette %>% group_by(year) %>%
  summarise(Median = median(packpc))

#Plot median value for 1985-1995
unique(Cigarette$year)
ggplot(MedianPPC, aes(x = year, y = Median)) + geom_point()

#scatter plot of price per pack vs number of packs per capita
ggplot(Cigarette, aes(x = cpi, y = packpc)) +
  geom_point() + geom_smooth(method = lm)

#Correlated? 
cor.test(Cigarette$cpi, Cigarette$packpc, method = "pearson", use = "complete.obs")
#pvalue = 2.2e-16
#cor value = -0.4035584 = moderately correlated
#expected b/c as the price increases the fewer packs people will buy, decreaseing
#packs per capita 

#Change scatter plot to show the points for each year in a diff. color. 
ggplot(Cigarette, aes(x = cpi, y = packpc, color = year)) +
  geom_point() + geom_smooth(method = lm)

#Linear regression for two variables
regression <- lm(packpc~cpi, Cigarette)
summary(regression)
#adjusted r-squared = 0.1613 = 16% of the variability

#adjust for inflation
Inflation_adjust <- Cigarette %>% mutate(inflationRate = avgprs / cpi)

#Adjusted inflation scatter plot & linear regression
ggplot(Inflation_adjust, aes(x = inflationRate, y = packpc)) + geom_point() +
  geom_smooth(method = lm)
regression2 <- lm(packpc~inflationRate, Inflation_adjust)
summary(regression2)
#adjusted r-squared = 0.3757 = 37% of the variability

#data frames w/just the rows from 1985 & 1995
packpc1985 <- Cigarette %>% filter(year == 1985)
packpc1995 <- Cigarette %>% filter(year == 1995)

#paired t-test number of packspc in '95 sig diff than '85
t.test(packpc1985$packpc, packpc1995$packpc, paired = TRUE)
#pvalue = 2.2e-16 = less than .05, yes significantly different


