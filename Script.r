#######################
### AMSA assignment ###
### Lucas De Vrieze ###
###    r0665032     ###
###   27/01/2023    ###
#######################

library(ggplot2)
library(reshape2)
library(randomForest)
library(pls)
library(mt)
source("pca.outlier.R") # -> overwriting pca.outlier function from mt package
environment(pca.outlier) = asNamespace('mt') # restoring access to hidden auxiliary functions in mt package
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

## Loading data
data(RiceFarms, package="plm")
data = RiceFarms

### (0) Data wrangling
data = subset(data, select = -c(id,status,goutput))

# Correcting wage and labour data
data$corrWage = data$hiredlabor/data$totlabor*data$wage
data$hiredlabor = NULL
data$famlabor = NULL
data$wage = NULL

# Mixed varieties could be too heterogeneous
data = subset(data, varieties != "mixed")
data = subset(data, bimas != "mixed")
data = droplevels(data)

# Nominal variables
table(data$bimas, data$region) # few intensified fields in comparison with non-intensified
table(data$varieties, data$region, dnn = c("Varieties", "Region")) # Regions have a preference for varieties, except sukaambit
table(data$varieties, data$bimas, dnn = c("Varieties", "Bimas")) # few intensified fields in comparison with non-intensified
data = subset(data, bimas == "no", select = -c(bimas)) # removing intensified field data

data.only.numeric = subset(data, select = -c(varieties, region))

### (1) Checking correlations
## Correlation matrix
cormat = melt(cor(data.only.numeric))
names(cormat) = c('Variable1', 'Variable2', 'Correlation')
pdf(file = "CorrelationHeatmap.pdf", height = 3, width = 5)
ggplot(data = cormat, aes(x = Variable1, y = Variable2 , fill = Correlation)) +
  geom_tile() + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_gradient(low="blue", high="red")
dev.off()
# Many correlations here

## PCA
data.pca = princomp(data.only.numeric, cor=T)
screeplot(data.pca, type = "lines") # 3 PCs will do -> big dimension reduction
summary(data.pca) # 5 principal components cover almost 90% of the variance
data.pca$loadings
# First PC captures all variables, but the weights of the inputs are larger than those of the price
# Second mainly corrects for cost variables
# Third mainly corrects for pesticides

ggbiplot(data.pca, var.axes = T, circle = T) +
  theme_classic()
# Two groups? Quite some outliers here

## PCA outlier detection
outliers.12 = pca.outlier(data.only.numeric, center = T, scale = T, conf.level = 0.99, pcs = c(1,2))
plot(outliers.12$plot)
outliers.13 = pca.outlier(data.only.numeric, center = T, scale = T, conf.level = 0.99, pcs = c(1,3))
plot(outliers.13$plot)
outliers.23 = pca.outlier(data.only.numeric, center = T, scale = T, conf.level = 0.99, pcs = c(2,3))
plot(outliers.23$plot)
outliers = unique(c(outliers.12$outlier, outliers.13$outlier, outliers.23$outlier))
data = data[-outliers,]
data.only.numeric = subset(data, select = -c(varieties, region))
# Redo PCA
data.pca = princomp(data.only.numeric, cor=T)
screeplot(data.pca, type = "lines") # 3 PCs will do -> big dimension reduction
summary(data.pca) # 6 principal components cover about 90% of the variance
data.pca$loadings
# Same observations still with regard to the loadings

## Biplots
# Two groups appear, but these are not governed by the regions nor the seed varieties
pdf(file = "Biplot_region.pdf", width = 6, height = 3.25)
ggbiplot(data.pca, groups = data$region, var.axes = T, circle = T) +
  theme_classic()
dev.off()
pdf(file = "Biplot_variety.pdf", width = 6, height = 3.25)
ggbiplot(data.pca, groups = data$varieties, var.axes = T, circle = T) +
  theme_classic()
dev.off()

# Combinations of the first three PCs
ggbiplot(data.pca, groups = data$region, var.axes = T, circle = T, choices = c(2,3)) +
  theme_classic()
ggbiplot(data.pca, groups = data$varieties, var.axes = T, circle = T, choices = c(2,3)) +
  theme_classic()
ggbiplot(data.pca, groups = data$region, var.axes = T, circle = T, choices = c(1,3)) +
  theme_classic()
ggbiplot(data.pca, groups = data$varieties, var.axes = T, circle = T, choices = c(1,3)) +
  theme_classic()

# Might the two groups be reflecting cheap and premium rice?
# Market price - and its correlated prices - is one of the main components of the two dominant PCs
# Defining a low and high price category
pdf("PriceHistogram.pdf", width = 5, height = 3)
hist(data$price, xlab = "Market price", main = "") # some bimodal tendencies
abline(v=mean(data$price), col = "red")
dev.off()
premium = ifelse(data$price > mean(data$price), "Premium", "Cheap")
data$premium = factor(premium)
pdf("Biplot_premium.pdf", width = 6, height = 3.25)
ggbiplot(data.pca, groups = premium, var.axes = T, circle = T) +
  theme_classic()
dev.off()
# Pretty clear groups!

### (2) Assessing influence of regions
# An explorative boxplot
boxplot(noutput ~ region, data = data)
# Some differences between the regions, but is this because of the region or because of the inputs?

# Drilling down to the same variety in some regions to minimise other influences
highVarietyRegions = c('wargabinangun','langan', 'sukaambit')
tradVarietyRegions = c('gunungwangi','malausma','sukaambit', 'ciwangi')
data.highVarietyRegions = data[data$region %in% highVarietyRegions & data$varieties == "high",]
data.highVarietyRegions.region = data.highVarietyRegions
data.highVarietyRegions.region$varieties = NULL
data.highVarietyRegions.region$premium = NULL
data.highVarietyRegions.region = droplevels(data.highVarietyRegions.region)
data.highVarietyRegions = subset(data.highVarietyRegions, select = -c(region, varieties, premium))
data.tradVarietyRegions = data[data$region %in% tradVarietyRegions & data$varieties == "trad",]
data.tradVarietyRegions.region = data.tradVarietyRegions
data.tradVarietyRegions.region$varieties = NULL
data.tradVarietyRegions.region$premium = NULL
data.tradVarietyRegions.region = droplevels(data.tradVarietyRegions.region)
data.tradVarietyRegions = subset(data.tradVarietyRegions, select = -c(region, varieties, premium))

## So, let's assume there is a grouping structure and check whether the data can be classified well.
## if able to distinguish regions, check variable importances

# Setting up training sets
# high variety
set.seed(100)
train.indices = sample(nrow(data.highVarietyRegions.region), size = nrow(data.highVarietyRegions.region)*0.75)
region.high.train = data.highVarietyRegions.region[train.indices,]
region.high.test = data.highVarietyRegions.region[-train.indices,]
# trad variety
set.seed(101)
train.indices = sample(nrow(data.tradVarietyRegions.region), size = nrow(data.tradVarietyRegions.region)*0.75)
region.trad.train = data.tradVarietyRegions.region[train.indices,]
region.trad.test = data.tradVarietyRegions.region[-train.indices,]

# randomForests
# High variety
set.seed(7)
region.high.rf = randomForest(region~., data = region.high.train, importance = T)
region.high.rf # Ok OOB accuracy (77.78%)
region.high.rf.predict = predict(region.high.rf, newdata = region.high.test)
mean(region.high.rf.predict == region.high.test$region) # Okish test accuracy (71.4%)
table(region.high.rf.predict, region.high.test$region)
varImpPlot(region.high.rf)
importance((region.high.rf))
# noutput is not the most important variable -> no strong region-specific factor

# trad variety
set.seed(7)
region.trad.rf = randomForest(region~., data = region.trad.train, importance = T)
region.trad.rf # Ok OOB accuracy of 76.9%
region.trad.rf.predict = predict(region.trad.rf, newdata = region.trad.test)
mean(region.trad.rf.predict == region.trad.test$region) # Good test accuracy of 78.9%
table(region.trad.rf.predict, region.trad.test$region, deparse.level = 2)
varImpPlot(region.trad.rf)
importance(region.trad.rf)
# noutput is not the most important variable -> no strong region-specific factor
# differences are mostly explained by the other variables

# CONCLUSION: There are no strong specific region effects -> neglect region
data$region = NULL
data.only.numeric = subset(data, select = -c(varieties, premium))

### (3) Assessing influence of rice variety
## Checking group structure via randomForests
# Setting up training set
set.seed(2)
train.indices = sample(nrow(data), size = nrow(data)*0.75)
variety.train = subset(data, select = -c(premium))[train.indices,]
variety.test = subset(data, select = -c(premium))[-train.indices,]
set.seed(8)
variety.rf = randomForest(varieties~., data = variety.train, importance = T)
variety.rf # Good accuracy: 84.63 %
variety.rf.predict = predict(variety.rf, newdata = variety.test)
mean(variety.rf.predict == variety.test$varieties) # Good test accuracy: 82.78 %
table(variety.rf.predict, variety.test$varieties, deparse.level = 2)
varImpPlot(variety.rf)
importance(variety.rf)
# There is a substantial difference in accuracy because of phosphate and the price of the seed
# Different nutrient needs?

## CONCLUSION: It would be useful to make separate models for the different varieties.

### (4) Verifying influence of price
# A clear group structure has already been seen at the biplot!
## Checking group structure via randomForests
set.seed(5)
train.indices = sample(nrow(data), size = nrow(data)*0.75)
premium.train = subset(data, select = -c(varieties, price))[train.indices,] # removing price as premium was constructed from it
premium.test = subset(data, select = -c(varieties, price))[-train.indices,]
set.seed(6)
premium.rf = randomForest(premium~., data = premium.train, importance = T)
premium.rf # Excellent accuracy: 97.41 %
premium.rf.predict = predict(premium.rf, newdata = premium.test)
mean(premium.rf.predict == premium.test$premium) # Excellent test accuracy: 97.77 %
table(premium.rf.predict, premium.test$premium, deparse.level = 2)
varImpPlot(premium.rf)
importance(premium.rf)
# Group structure defined by the prices

### (5) Making predictive models
table(data$premium, data$varieties)
# groups are big enough to make four models

# Defining the groups
data$groups = factor(ifelse(data$premium == "Premium", ifelse(data$varieties == "high", "PH", "PT"), ifelse(data$varieties == "high", "CH", "CT")))

# Biplot with variety and premium grouping
ggbiplot(data.pca, groups = data$groups, var.axes = T, circle = T, choices = c(1,2)) +
  theme_classic()
ggbiplot(data.pca, groups = data$groups, var.axes = T, circle = T, choices = c(1,3)) +
  theme_classic()
ggbiplot(data.pca, groups = data$groups, var.axes = T, circle = T, choices = c(2,3)) +
  theme_classic()
# Premium distinction is more clear than the variety distinction

## group 1: high yield variant & cheap
# selecting number of components
set.seed(3)
data.1 = subset(data, groups == "CH", select = -c(varieties, premium, groups))
model.1 = plsr(noutput~., data = data.1, center = T, scale = T, validation = "CV")
summary(model.1)
validationplot(model.1) # 6 components

# Final model
train.indices = sample(nrow(data.1), size = nrow(data.1)*0.75)
data.1.train = data.1[train.indices,]
data.1.test = subset(data.1, select = -c(noutput))[-train.indices,]
y.1.test = subset(data.1, select = c(noutput))[-train.indices,]
model.1.final = plsr(noutput~., data = data.1.train, center = T, scale = T, validation="CV")
data.1.predict = predict(model.1.final, data.1.test, ncomp = 6)
rs.1 = (data.1.predict-y.1.test)^2 # residual squares
rss.1 = sum(rs.1) # residual sum of squares
sqrt(mean(rs.1)) # RMSE
tss.1 = sum((y.1.test-mean(y.1.test))^2) # total sum of squares
rsq.1 = 1-rss.1/tss.1 # R²
rsq.1 # 0.699
pdf(file = "PLSR_group1.pdf", width = 5, height = 4)
plot(y.1.test, data.1.predict, xlab = "Observations", ylab = "Predictions", pch = 20)
lines(y.1.test, y.1.test)
dev.off()

## group 2: high yield variant & premium
# selecting number of components
set.seed(4)
data.2 = subset(data, groups == "PH", select = -c(varieties, premium, groups))
model.2 = plsr(noutput~., data = data.2, center = T, scale = T, validation = "CV")
summary(model.2)
validationplot(model.2) # 2 components

# Final model
train.indices = sample(nrow(data.2), size = nrow(data.2)*0.75)
data.2.train = data.2[train.indices,]
data.2.test = subset(data.2, select = -c(noutput))[-train.indices,]
y.2.test = subset(data.2, select = c(noutput))[-train.indices,]
model.2.final = plsr(noutput~., data = data.2.train, center = T, scale = T, validation="CV")
data.2.predict = predict(model.2.final, data.2.test, ncomp = 2)
rs.2 = (data.2.predict-y.2.test)^2 # residual squares
rss.2 = sum(rs.2) # residual sum of squares
sqrt(mean(rs.2)) # RMSE
tss.2 = sum((y.2.test-mean(y.2.test))^2) # total sum of squares
rsq.2 = 1-rss.2/tss.2 # R²
rsq.2 # 0.804
pdf(file = "PLSR_group2.pdf", width = 5, height = 4)
plot(y.2.test, data.2.predict, xlab = "Observations", ylab = "Predictions", pch = 20)
lines(y.2.test, y.2.test)
dev.off()

## group 3: trad yield variant & cheap
# selecting number of components
set.seed(5)
data.3 = subset(data, groups == "CT", select = -c(varieties, premium, groups))
model.3 = plsr(noutput~., data = data.3, center = T, scale = T, validation = "CV")
summary(model.3)
validationplot(model.3) # 6 components

# Final model
train.indices = sample(nrow(data.3), size = nrow(data.3)*0.75)
data.3.train = data.3[train.indices,]
data.3.test = subset(data.3, select = -c(noutput))[-train.indices,]
y.3.test = subset(data.3, select = c(noutput))[-train.indices,]
model.3.final = plsr(noutput~., data = data.3.train, center = T, scale = T, validation="CV")
data.3.predict = predict(model.3.final, data.3.test, ncomp = 6)
rs.3 = (data.3.predict-y.3.test)^2 # residual squares
rss.3 = sum(rs.3) # residual sum of squares
sqrt(mean(rs.3)) # RMSE
tss.3 = sum((y.3.test-mean(y.3.test))^2) # total sum of squares
rsq.3 = 1-rss.3/tss.3 # R²
rsq.3 # 0.753
pdf(file = "PLSR_group3.pdf", width = 5, height = 4)
plot(y.3.test, data.3.predict, xlab = "Observations", ylab = "Predictions", pch = 20)
lines(y.3.test, y.3.test)
dev.off()

## group 4: trad yield variant & premium
# selecting number of components
set.seed(6)
data.4 = subset(data, groups == "PT", select = -c(varieties, premium, groups))
model.4 = plsr(noutput~., data = data.4, center = T, scale = T, validation = "CV")
summary(model.4)
validationplot(model.4) # 1 components

# Final model
train.indices = sample(nrow(data.4), size = nrow(data.4)*0.75)
data.4.train = data.4[train.indices,]
data.4.test = subset(data.4, select = -c(noutput))[-train.indices,]
y.4.test = subset(data.4, select = c(noutput))[-train.indices,]
model.4.final = plsr(noutput~., data = data.4.train, center = T, scale = T, validation="CV")
data.4.predict = predict(model.4.final, data.4.test, ncomp = 1)
rs.4 = (data.4.predict-y.4.test)^2 # residual squares
rss.4 = sum(rs.4) # residual sum of squares
sqrt(mean(rs.4)) # RMSE
tss.4 = sum((y.4.test-mean(y.4.test))^2) # total sum of squares
rsq.4 = 1-rss.4/tss.4 # R²
rsq.4 # 0.879
pdf(file = "PLSR_group4.pdf", width = 5, height = 4)
plot(y.4.test, data.4.predict, xlab = "Observations", ylab = "Predictions", pch = 20)
lines(y.4.test, y.4.test)
dev.off()

## comparing the groups
# Getting all predictions
data.all = subset(data.only.numeric, select = -c(noutput))
data.all.predict.1 = predict(model.1.final,  newdata = data.all, ncomp = 6)
data.all.predict.2 = predict(model.2.final,  newdata = data.all, ncomp = 2)
data.all.predict.3 = predict(model.3.final,  newdata = data.all, ncomp = 6)
data.all.predict.4 = predict(model.4.final,  newdata = data.all, ncomp = 1)
data.all.predict = as.data.frame(cbind(data.all.predict.1, data.all.predict.2, data.all.predict.3, data.all.predict.4))
colnames(data.all.predict) = c("CH", "PH", "CT", "PT")

# Boxplot per group
pdf("Boxplot_modelgroups.pdf", width = 6, height = 4)
boxplot(data.all.predict, ylab = "Net output", xlab = "Model")
dev.off()
# High-yield varieties appear better than the traditional ones regarding output
# Premium rice does not appear better regarding output
