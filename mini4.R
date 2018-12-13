library(ggplot2)
library(gridExtra)
library(MASS)
library(anytime)
library(tidyr)
library(leaps)
#
# Preparing data as in lab 2 to fit model
# Using the elastic net to estimate.
#
#
houseDat = read.csv("http://www.math.chalmers.se/Stat/Grundutb/GU/MSG500/A18/kc_house_data.csv")


houseDat$yr_renovated[(houseDat$yr_renovated > 0 & houseDat$yr_renovated < 1980)] = 1
houseDat$yr_renovated[(houseDat$yr_renovated >= 1980 & houseDat$yr_renovated < 1990)] = 2
houseDat$yr_renovated[(houseDat$yr_renovated >= 1990 & houseDat$yr_renovated < 2000)] = 3
houseDat$yr_renovated[(houseDat$yr_renovated >= 2000 & houseDat$yr_renovated < 2010)] = 4
houseDat$yr_renovated[(houseDat$yr_renovated >= 2010)] = 5

houseDat$grade[(houseDat$grade >= 0 & houseDat$grade <= 3)] = 0
houseDat$grade[(houseDat$grade > 3 & houseDat$grade <= 6)] = 1
houseDat$grade[houseDat$grade == 7] = 2
houseDat$grade[(houseDat$grade > 7 & houseDat$grade <= 9)] = 3
houseDat$grade[houseDat$grade > 9] = 4


houseDat$date = anytime(houseDat$date)
houseDat$price = log(houseDat$price)
houseDat$sqft_lot = log(houseDat$sqft_lot)
houseDat$sqft_lot15 = log(houseDat$sqft_lot15)
houseDat$sqft_above = log(houseDat$sqft_above)
houseDat$sqft_living = log(houseDat$sqft_living)
houseDat$sqft_living15 = log(houseDat$sqft_living15)
houseDat = subset(houseDat, select = -14)

houseDat$bedrooms[houseDat$bedrooms >= 7] = 7

ggplot(data = houseDat, aes(fill = factor(grade))) +
  geom_bar(alpha = 0.6) + aes(factor(grade)) +
  scale_x_discrete(labels = c("poor", "lower", "average", "better", "higher"), name = "") +
  scale_fill_discrete(labels = c("poor", "lower", "average", "better", "higher")) +
  stat_count(aes(label=..count..), vjust=-0.5, geom="text")
#
#
#
# Check how bedrooms data match to a poisson distribution
#
#
#
#
# Find suitable parameters for a COM-poisson model 
# TODO: Read up on COM-poisson, what does it do exactly?
#
library(compoisson)

dat = cbind(0:7, as.numeric(table(houseDat$bedrooms)))
COMpoisson.args = com.fit(dat)

counts = sum(as.numeric(table(houseDat$bedrooms)))
nCounts = as.numeric(table(houseDat$bedrooms))/counts
poisson.dist = dpois(0:7, lambda = mean(houseDat$bedrooms))
#dispersion = (var(houseDat$bedrooms) - mean(houseDat$bedrooms))/mean(houseDat$bedrooms)^2
COMpoisson.dist = COMpoisson.args$fitted.values/sum(COMpoisson.args$fitted.values)
x = 0:7

dat.tmp = data.frame(nCounts, poisson.dist, x, COMpoisson.dist)
  
ggplot(data = dat.tmp, aes(x, nCounts)) +
  geom_histogram(alpha = 0.6, stat = "identity") +
  geom_smooth(aes(y = poisson.dist, color = "Poisson"), se = FALSE, lwd = 1.5) +
  geom_smooth(aes(y = COMpoisson.dist, color = "COM-Poisson"), se = FALSE, lwd = 1.5) +
  ylab("Proportion") +
  xlab("Bedrooms")
#
#
# Question is if bedrooms above certain number should be summed together?
#
#

ggplot(data = houseDat, aes(fill = factor(bedrooms))) +
  geom_bar(alpha = 0.6) + aes(factor(bedrooms)) +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6", ">7"), name = "") +
  scale_fill_discrete(labels = c("0", "1", "2", "3", "4", "5", "6", ">7")) +
  stat_count(aes(label=..count..), vjust=-0.5, geom="text")


#
# Check bedrooms against the numerical variables
#
#
houseDat2 = gather(houseDat, dat, value, -c(bedrooms, date, id, condition, grade, waterfront, view, yr_renovated, floors))
ggplot(data = houseDat2, aes(factor(bedrooms), value, fill = factor(bedrooms))) + geom_boxplot() +
  theme(legend.position = "none") +
  facet_wrap(~dat, scales = "free") +
  theme(axis.text.x= element_text(size = 6, angle= 45)) +
  ggtitle("Bedrooms vs numerical predictors")
#
# Check bedrooms vs the categorical variables
#
library(ggmosaic)

g1 = ggplot(data = houseDat) + geom_mosaic(aes(x = product(bedrooms), fill = factor(view)), offset = 0.03) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(name = "view") +
  xlab("Bedrooms")

g2 = ggplot(data = houseDat) + geom_mosaic(aes(x = product(bedrooms), fill = factor(grade)), offset = 0.03) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(name = "grade") +
  xlab("Bedrooms")

g3 = ggplot(data = houseDat) + geom_mosaic(aes(x = product(bedrooms), fill = factor(condition)), offset = 0.03) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(name = "cond.") +
  xlab("Bedrooms")

g4 = ggplot(data = houseDat) + geom_mosaic(aes(x = product(bedrooms), fill = factor(waterfront)), offset = 0.03) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(name = "waterfront") +
  xlab("Bedrooms")

dat.tmp = data.frame(houseDat$yr_renovated[houseDat$yr_renovated != 0], houseDat$bedrooms[houseDat$yr_renovated != 0])
colnames(dat.tmp) = c("yr_renovated", "bedrooms")

g5 = ggplot(data = dat.tmp) + geom_mosaic(aes(x = product(bedrooms), fill = factor(yr_renovated)), offset = 0.03) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(name = "yr_ren.", 
                      labels = c("<1979", 
                                 "1980-1989", 
                                 "1990-1999", 
                                 "2000-2009", 
                                 ">2010")) +
  xlab("Bedrooms")

g6 = ggplot(data = houseDat) + geom_mosaic(aes(x = product(bedrooms), fill = factor(floors)), offset = 0.03) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(name = "floors") +
  xlab("Bedrooms")

library(grid)
grid.arrange(g1,g2,g3,g4,g5,g6, ncol = 2, bottom = textGrob("Bedrooms"))
#
#
#
# Fitting of data to poisson GLM model and checking for over/underdispersion
#
#
poisson.model = glm(bedrooms~., family = "poisson", data = houseDat)
summary(poisson.model)

poisson.int = glm(bedrooms~1, family = "poisson", data = houseDat)

library(AER)
disp.test = dispersiontest(poisson.int, alternative = "two.sided")
#
# Significant p-value for underdispersion
# Let's check QQ plot to see if we can determine underdispersion from it
#
#
library(DHARMa)
sim.res <- simulateResiduals(poisson.model, n = 250)
n = length(sim.res$scaledResiduals)
expected = (1:n)/(n+1)
tmp.dat = data.frame(sort(sim.res$scaledResiduals), expected)
colnames(tmp.dat) = c("Observed", "Expected")

ggplot(data = tmp.dat, aes(Expected, Observed, color = Observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1, linetype = 2) +
  xlab("Expected residuals")+
  ylab("Observed scaled residuals")+
  theme(legend.position = "none")+
  geom_text(x = 0.1, y = 0.8, label = "Dispersion = 0.24\np-value significant", color = "black", lwd = 5)

dat.tmp = data.frame(poisson.model$residuals, poisson.model$linear.predictors, houseDat$bedrooms)
colnames(dat.tmp) = c("residuals", "fitted", "bedrooms")

ggplot(data = dat.tmp, aes(fitted, residuals, color = factor(bedrooms))) +
  geom_point()
#
#
# Remove some predictors that "don't make sense" based on intuition
#
houseDat3 = houseDat[,-c(1,2,15)] #removed: id, yr_renovated, date

#
# glm version of regsubsets. Uses CV to do varaible selection. 
# Don't work with COM-poisson models, but idea could be to fit
# ordinary poisson model and choose predictors from there and then
# fit COM-poisson model on these predictors
#
# OBS: bestglm takes data with target in last column!!!
#
library(bestglm)
#
# Fix data so works correctly with bestglm
#
tmp = houseDat3$bedrooms
houseDat3 = houseDat3[,-2]
houseDat3 = as.data.frame(cbind(houseDat3, tmp))
names(houseDat3)[colnames(houseDat3) == "tmp"] = "Bedrooms"
rm(tmp)

var.select = bestglm()