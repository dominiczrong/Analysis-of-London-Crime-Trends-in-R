##Installing Relevant R Packages
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("ggplot2")
install.packages('gbm')
install.packages('psych')

##Reading the Dataset into R
library(readxl)
Crime_Data <- read_excel("/Users/dominicongzhirui/Desktop/London\ Crime\ Data.xlsx", sheet = "Main")
View(Crime_Data)
summary(Crime_Data)

##Running Multi-Linear Regression

#Running 2018 Multi-Linear-Regression
Crime_Data_2018 <- read_excel("/Users/dominicongzhirui/Desktop/London\ Crime\ Data.xlsx", sheet = "2018 Linear Reg")

#Level 1 Crime
lm_low = lm(y1 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2018)
summary(lm_low)

#Level 2 Crime
lm_med = lm(y2 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2018)
summary(lm_med)

#Level 3 Crime
lm_high = lm(y3 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2018)
summary(lm_high)

#Running Stepwise Regression
#Level 1 Crime
lm_low_max = lm(y1 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2018)
lm_low_min = lm(y1 ~ 1, data=Crime_Data_2018)
step.low   = step(lm_low_min, scope=list(upper=lm_low_max))
summary(step.low)

#Level 2 Crime
lm_med_max = lm(y2 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2018)
lm_med_min = lm(y2 ~ 1, data=Crime_Data_2018)
step.med   = step(lm_med_min, scope=list(upper=lm_med_max))
summary(step.med)

#Level 3 Crime
lm_high_max = lm(y3 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2018)
lm_high_min = lm(y3 ~ 1, data=Crime_Data_2018)
step.high   = step(lm_high_min, scope=list(upper=lm_high_max))
summary(step.high)

#Running 2013 Multi-Linear-Regression
Crime_Data_2013 <- read_excel("/Users/dominicongzhirui/Desktop/London\ Crime\ Data.xlsx", sheet = "2013 Linear Reg")
#Level 1 Crime
lm_low_2013 = lm(y1 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2013)
summary(lm_low_2013)
#Level 2 Crime
lm_med_2013 = lm(y2 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2013)
summary(lm_med_2013)
#Level 3 Crime
lm_high_2013 = lm(y3 ~ minwage + density + dwell_poor + dwell_mid + dwell_best + diversity + police + emp_white + emp_ethnic, data=Crime_Data_2013)
summary(lm_high_2013)


##Matrix Plot
library(psych)
pairs.panels(Crime_Data[,6:15], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             cex.cor = 1.2,
             cex.labels = 0.7
)

##Running Regression Trees
library(rpart)
library(rpart.plot)
library(tree)

#Regression Tree for Total Crime
regtree.data = subset(Crime_Data, select = -c(y1, y2, y3, Borough)) #Removing Crime Level 1-3 and Borough
set.seed(7)
training_index = sample(1:nrow(regtree.data), nrow(regtree.data)*0.80)
training_set = regtree.data[training_index,] #dataset for training
testing_set = regtree.data[-training_index,] #dataset for testing

tree.TotalCrime = tree(regtree.data$ytotal~., data=regtree.data, subset=training_index) #trained decision tree model
plot(tree.TotalCrime); text(tree.TotalCrime, cex = .7) #initial plot
summary(tree.TotalCrime)

##Using rpart to plot tree
reg_tree = rpart(
  formula = ytotal~.,
  data = regtree.data,
  subset = training_index,
  method = "anova"
)
par(mfrow=c(1,2)) 
rpart.plot(reg_tree,type=5, extra = 1)

#Interpreting this tree
summary(reg_tree)
par(mfrow=c(1,2)) 
plotcp(reg_tree) #visualize cross-validation results
printcp(reg_tree) #display the results

#Pruning Regression Tree
pruned_reg_tree = prune(reg_tree, cp=reg_tree$cptable[which.min(reg_tree$cptable[, "xerror"]), "CP"])
rpart.plot(pruned_reg_tree,type=5, extra=1)
par(mfrow=c(1,2))
summary(pruned_reg_tree)

#Finding the MSE of the Regression Tree
yhat = predict(reg_tree, newdata=regtree.data[-training_index,])
reg_tree.test = regtree.data[-training_index,"ytotal"]
Diff = ((yhat-reg_tree.test)^2)
MSE = mean(as.numeric(Diff$ytotal))
MSE #To view MSE of the initial Regression Tree

##Regression Tree for Level 1 Crime
regtree.data1 = subset(Crime_Data, select = -c(ytotal, y2, y3, Borough))
set.seed(7)
training_index_Lv1 = sample(1:nrow(regtree.data1), nrow(regtree.data1)*0.80)
training_set_Lv1 = regtree.data1[training_index_Lv1,] #dataset for training
testing_set_Lv1 = regtree.data1[-training_index_Lv1,] #dataset for testing
reg_tree1 = rpart(
  formula = y1~.,
  data = regtree.data1,
  subset = training_index_Lv1,
  method = "anova"
)
rpart.plot(reg_tree1,type=5, extra = 1)
summary(reg_tree1)

#Interpreting this tree
summary(reg_tree1)
par(mfrow=c(1,2)) 
plotcp(reg_tree1) #visualize cross-validation results
printcp(reg_tree1) #display the results

#Pruning Regression Tree for Level 1 Crime
pruned_reg_tree1 = prune(reg_tree1, cp=reg_tree1$cptable[which.min(reg_tree1$cptable[, "xerror"]), "CP"])
rpart.plot(pruned_reg_tree1,type=5, extra=1)
par(mfrow=c(1,2))
summary(pruned_reg_tree1)

#Regression Tree for Level 2 Crime
regtree.data2 = subset(Crime_Data, select = -c(ytotal, y1, y3, Borough))
set.seed(7)
training_index_Lv2 = sample(1:nrow(regtree.data2), nrow(regtree.data2)*0.80)
training_set_Lv2 = regtree.data2[training_index_Lv2,] #dataset for training
testing_set_Lv2 = regtree.data2[-training_index_Lv2,] #dataset for testing
reg_tree2 = rpart(
  formula = y2~.,
  data = regtree.data2,
  subset = training_index_Lv2,
  method = "anova"
)
rpart.plot(reg_tree2,type=5, extra = 1)
summary(reg_tree2)

#Interpreting this tree
summary(reg_tree2)
par(mfrow=c(1,2)) 
plotcp(reg_tree2) #visualize cross-validation results
printcp(reg_tree2) #display the results

#Pruning Regression Tree for Level 2 Crime
pruned_reg_tree2 = prune(reg_tree2, cp=reg_tree2$cptable[which.min(reg_tree2$cptable[, "xerror"]), "CP"])
rpart.plot(pruned_reg_tree2,type=5, extra=1)
par(mfrow=c(1,2))
summary(pruned_reg_tree2)


#Regression Tree for Level 3 Crime
regtree.data3 = subset(Crime_Data, select = -c(ytotal, y1, y2, Borough))
set.seed(7)
training_index_Lv3 = sample(1:nrow(regtree.data3), nrow(regtree.data3)*0.80)
training_set_Lv3 = regtree.data3[training_index_Lv3,] #dataset for training
testing_set_Lv3 = regtree.data[-training_index_Lv3,] #dataset for testing
reg_tree3 = rpart(
  formula = y3~.,
  data = regtree.data3,
  subset = training_index_Lv3,
  method = "anova"
)
rpart.plot(reg_tree3,type=5, extra = 1)
summary(reg_tree3)

#Interpreting this tree
summary(reg_tree3)
par(mfrow=c(1,2)) 
plotcp(reg_tree3) #visualize cross-validation results
printcp(reg_tree3) #display the results

#Pruning Regression Tree for Level 3 Crime
pruned_reg_tree3 = prune(reg_tree3, cp=reg_tree3$cptable[which.min(reg_tree3$cptable[, "xerror"]), "CP"])
rpart.plot(pruned_reg_tree3,type=5, extra=1)
par(mfrow=c(1,2))
summary(pruned_reg_tree3)


#Bagging
library(randomForest)
set.seed(7)
bagging.TotalCrime = randomForest(regtree.data$ytotal~., data=regtree.data, subset=training_index, mtry=10, ntree=500, importance=T)
bagging.TotalCrime

#Random Forest
set.seed(7)
rf.TotalCrime = randomForest(regtree.data$ytotal~., data=regtree.data, subset=training_index, mtry=5, ntree= 500, importance=T)
rf.TotalCrime #To view MSE of the Rf Model
importance(rf.TotalCrime)
varImpPlot(rf.TotalCrime, col=c("blue", "darkblue"))

#Boosting
library(gbm)
set.seed(7)
boost.TotalCrime = gbm(ytotal~.,data=regtree.data[training_index,], distribution = "gaussian", n.trees=500, interaction.depth = 3)
summary(boost.TotalCrime)

#Partial Dependence Plots
plot(boost.TotalCrime, i="police")
plot(boost.TotalCrime, i="minwage")
plot(boost.TotalCrime, i="diversity")

#Finding the MSE of the Boosted Model
yhat.boost = predict(boost.TotalCrime, newdata=regtree.data[-training_index,],n.trees=500)
reg_tree.test = regtree.data[-training_index,"ytotal"]
Diff.boost = ((yhat.boost-reg_tree.test)^2)
MSE.boost = mean(as.numeric(Diff.boost$ytotal))
MSE.boost #MSE of Boosted Model
