#bankrupt.train <- read.csv('/Users/victorialu/Desktop/Datasets/bankruptcy_training.csv')
#bankrupt.test <- read.csv('/Users/victorialu/Desktop/Datasets/bankruptcy_testing.csv')

bankrupt.train <- read.csv('/capstone_project/datasets/bankruptcy_training.csv')
bankrupt.test <- read.csv('/capstone_project/datasets/bankruptcy_testing.csv')

class_distribution <- table(bankrupt.train$status_label) #7076 and 7076

bankrupt.train$cycle_type <- as.factor(bankrupt.train$cycle_type)
bankrupt.train$status_label <- as.factor(bankrupt.train$status_label)
bankrupt.test$cycle_type <- as.factor(bankrupt.test$cycle_type)
bankrupt.test$status_label <- as.factor(bankrupt.test$status_label)

################ section 1: tree-based methods ################
#! The deviance plot varies even if the seed is set, so the best tree may vary
#1. CART
par(mfrow = c(1,1)) 
library(tree)
#full tree
tree.bankrupt <- tree(as.factor(status_label)~., data=bankrupt.train)
plot(tree.bankrupt)
text(tree.bankrupt, pretty=0, cex=0.8)

result <- cv.tree(tree.bankrupt, K=10, FUN=prune.tree)

#pruning
plot(result) #smallest deviance at 5 (some randomness)

besttree.bankrupt <- prune.tree(tree.bankrupt, best=5)

plot(besttree.bankrupt)
text(besttree.bankrupt, pretty=0, cex=0.8)

#evaluate performance  
p.post.tree <- predict(besttree.bankrupt,newdata=bankrupt.test)
Yhat.tree <-ifelse(p.post.tree[,2]>0.5,1,0)
confusion.tree = table(Yhat.tree, bankrupt.test$status_label)
#sensitivity
sensitivity.tree = confusion.tree[2,2]/(confusion.tree[1,2]+confusion.tree[2,2])
#specificity
specificity.tree <-confusion.tree[1,1]/(confusion.tree[1,1]+confusion.tree[2,1])
#misclas
misclass.tree = (confusion.tree[1,2]+confusion.tree[2,1])/length(Yhat.tree) 
tree.metrics <- c(sensitivity.tree, specificity.tree, misclass.tree)

#2. bagging
#install.packages("randomForest")
library(randomForest)
bag.bankrupt <- randomForest(as.factor(status_label)~., data=bankrupt.train, ntree=100) #mtry-96; invalid

#evaluate performance
Yhat.bag <- predict(bag.bankrupt, newdata=bankrupt.test,type="class")
confusion.bag = table(Yhat.bag, bankrupt.test$status_label)
#sensitivity
sensitivity.bag = confusion.bag[2,2]/(confusion.bag[1,2]+confusion.bag[2,2])
#specificity
specificity.bag <-confusion.bag[1,1]/(confusion.bag[1,1]+confusion.bag[2,1])
#misclas
misclass.bag = (confusion.bag[1,2]+confusion.bag[2,1])/length(Yhat.bag) 
bag.metrics <- c(sensitivity.bag, specificity.bag, misclass.bag)

#3. random forest
RF.bankrupt <- randomForest(as.factor(status_label)~.,data=bankrupt.train, ntree=100, mtry=10)
varImpPlot(RF.bankrupt)
importance(RF.bankrupt)

#evaluate performance
Yhat.RF <- predict(RF.bankrupt, newdata=bankrupt.test,type="class")
confusion.RF = table(Yhat.RF, bankrupt.test$status_label)
#sensitivity
sensitivity.RF = confusion.RF[2,2]/(confusion.RF[1,2]+confusion.RF[2,2])
#specificity
specificity.RF <-confusion.RF[1,1]/(confusion.RF[1,1]+confusion.RF[2,1])
#misclas
misclass.RF = (confusion.RF[1,2]+confusion.RF[2,1])/length(Yhat.RF) 
RF.metrics <- c(sensitivity.RF, specificity.RF, misclass.RF)

#4. comparing the three tree-based methods 
tree.metrics #0.7646310 0.4500000 0.2576832
bag.metrics #0.9287532 0.2333333 0.1205674
RF.metrics #0.9173028 0.2833333 0.1276596

################ section 2: SVC & SVM ################
library("e1071")

# The code below takes a very long time to run, so I commented it out
# #1. tuning 
# ### tuning cost and gamma for radial kernel ###
# #make this finer later
# #cost.list <-  c(40,50,100,500) >> 100
# #gamma.list <- c(5,10,30,50) >> 10
# 
# #cost.list <-  c(80,90,100,200,300) >> 300
# #gamma.list <-  c(10,15,20,25) >> 25
# 
# #cost.list <-  c(300,350,400,450,500) >> 450 
# #gamma.list <-  c(20,25,30,35,40) >> 40
# 
# #initialize values
# sensitivity.linear <- rep(NA,length(cost.list))
# sensitivity.poly <- rep(NA,length(cost.list))
# sensitivity.rad.full <- matrix(0, nrow=length(cost.list), ncol=length(gamma.list))
# 
# #tuning cost and gamma for radial kernel
# for (i in 1:length(cost.list)) {
#   for (j in 1:length(gamma.list)) {
#     #support vector machine with radial kernel 
#     svm.fit.rad <- svm(formula=status_label~., data=bankrupt.train, type="C-classification", kernel="radial", gamma=gamma.list[j], cost=cost.list[i])
#     Y.hat.rad <- predict(svm.fit.rad, bankrupt.test)
#     table.svm.rad <- table(Y.hat.rad, bankrupt.test$status_label)
#     sensitivity.rad.full[i,j] <- table.svm.rad[2,2]/(table.svm.rad[1,2]+table.svm.rad[2,2])
#   }
# }
# 
# #best cost and gamma combination for radial kernel
# sensitivity.rad.full
# max.value <- max(sensitivity.rad.full)
# indices <- which(sensitivity.rad.full == max.value, arr.ind = TRUE)
# row.index <- indices[, 1]
# col.index <- indices[, 2]
# best.cost <- cost.list[row.index]
# best.cost
# #450 
# best.gamma <- gamma.list[col.index]
# best.gamma
# #40
# #Together, sensitivity is 0.9274809

# ### tuning cost for svc and svm with poly kernal ###
# for(i in 1:length(cost.list)) {
#   #support vector classifier
#   svm.fit.linear <- svm(formula=status_label~., data=bankrupt.train, type="C-classification", kernel="linear", cost=cost.list[i])
#   Y.hat.linear <- predict(svm.fit.linear, bankrupt.test)
#   table.svc <- table(Y.hat.linear, bankrupt.test$status_label)
#   sensitivity.linear[i] = table.svc[2,2]/(table.svc[1,2]+table.svc[2,2])
#   
#   #support vector machine with poly kernel 
#   svm.fit.poly <- svm(formula=status_label~., data=bankrupt.train, type="C-classification", kernel="polynomial", cost=cost.list[i])
#   Y.hat.poly <- predict(svm.fit.poly, bankrupt.test)
#   table.svm.poly <- table(Y.hat.poly, bankrupt.test$status_label)
#   sensitivity.poly[i] = table.svm.poly[2,2]/(table.svm.poly[1,2]+table.svm.poly[2,2])
# }
# 
# #best cost for linear
# sensitivity.linear 
# cost.list[which(sensitivity.linear==max(sensitivity.linear))]
# #any of the follow 4  5  6  7  8  9 10 11 12 13 14 15
# 
# #best cost for poly 
# sensitivity.poly
# cost.list[which(sensitivity.poly==max(sensitivity.poly))]
# #0.1

#2. model fitting
bankrupt.svm.linear <- svm(formula=status_label~., data=bankrupt.train, type="C-classification", kernel="linear", cost=1)
bankrupt.svm.poly <- svm(formula=status_label~., data=bankrupt.train, type="C-classification", kernel="poly", cost=0.1)
bankrupt.svm.rad <- svm(formula=status_label~., data=bankrupt.train, type="C-classification", kernel="radial", gamma=40, cost=450)

#3. evaluate performance
#linear kernel 
Y.hat.svc <- predict(bankrupt.svm.linear, bankrupt.test)
confusion.svc <- table(Y.hat.svc, bankrupt.test$status_label)
#sensitivity
sensitivity.svc = confusion.svc[2,2]/(confusion.svc[1,2]+confusion.svc[2,2])
#specificity
specificity.svc <-confusion.svc[1,1]/(confusion.svc[1,1]+confusion.svc[2,1])
#misclas
misclass.svc = (confusion.svc[1,2]+confusion.svc[2,1])/length(Y.hat.svc) 

#poly kernel 
Y.hat.svm.poly <- predict(bankrupt.svm.poly, bankrupt.test)
confusion.svm.poly <- table(Y.hat.svm.poly, bankrupt.test$status_label)
#sensitivity
sensitivity.svm.poly = confusion.svm.poly[2,2]/(confusion.svm.poly[1,2]+confusion.svm.poly[2,2])
#specificity
specificity.svm.poly <-confusion.svm.poly[1,1]/(confusion.svm.poly[1,1]+confusion.svm.poly[2,1])
#misclas
misclass.svm.poly = (confusion.svm.poly[1,2]+confusion.svm.poly[2,1])/length(Y.hat.svm.poly) 

#radial kernel 
Y.hat.svm.rad <- predict(bankrupt.svm.rad, bankrupt.test)
confusion.svm.rad <- table(Y.hat.svm.rad, bankrupt.test$status_label)
#sensitivity
sensitivity.svm.rad = confusion.svm.rad[2,2]/(confusion.svm.rad[1,2]+confusion.svm.rad[2,2])
#specificity
specificity.svm.rad <-confusion.svm.rad[1,1]/(confusion.svm.rad[1,1]+confusion.svm.rad[2,1])
#misclas
misclass.svm.rad = (confusion.svm.rad[1,2]+confusion.svm.rad[2,1])/length(Y.hat.svm.rad) 

#comparing the kernels 
svc.metrics <- c(sensitivity.svc, specificity.svc, misclass.svc)
svm.poly.metrics <- c(sensitivity.svm.poly, specificity.svm.poly, misclass.svm.poly)
svm.rad.metrics <- c(sensitivity.svm.rad, specificity.svm.rad, misclass.svm.rad)
svc.metrics #0.4389313 0.6500000 0.5460993
svm.poly.metrics #0.01399491 0.98333333 0.91725768
svm.rad.metrics #0.8104326 0.3500000 0.2222222

################ section 3: logistic regression ################
###### step 1: addressing multicollinearity
nrow(bankrupt.train)
outliers.ind <- c(7390, 8900, 8253, 2042, 749, 4932)
bankrupt.train <- bankrupt.train[-outliers.ind, ]

library("usdm")
x.quant <- bankrupt.train[, -which(names(bankrupt.train) %in% c("status_label", "cycle_type"))]
vifstep(x.quant, th=10)
#5 variables from the 20 input variables have collinearity problem: 
#Interest_Coverage_Ratio Operating_Profit_Margin EBIT_Margin EBITDA_Margin Quick_Ratio-didn't 

######approach 1: pca
#feature scaling
x.scaled <- scale(x.quant)
status_label <- bankrupt.train$status_label
pca.data <- as.data.frame(cbind(status_label, x.scaled))
head(pca.data)

#pca
library(pls)
pca.model <- pcr(status_label~., data=pca.data, scale=TRUE, validation='CV')
summary(pca.model) #11 pcs explain over 90% of the variation in x
pca.model$loadings 
Z <- x.scaled %*%(pca.model$loadings)
cycle_type <- bankrupt.train$cycle_type
bankrupt.train.pca <- as.data.frame(cbind(status_label, Z, cycle_type))

######approach 2: remove multicollinear variables
variables_to_remove <- c(
  "Interest_Coverage_Ratio",
  "Operating_Profit_Margin",
  "EBIT_Margin",
  "EBITDA_Margin",
  "Quick_Ratio"
)

bankrupt.train.vif <- bankrupt.train[, !names(bankrupt.train) %in% variables_to_remove]

######step 2: feature selection
######! I have commented out the stepwise selection commands as they take a very long time to run
######part 1: vif dataset 
lm.full = glm(as.factor(status_label)~., data=bankrupt.train.vif, family=binomial("logit")) 
#??? glm.fit: fitted probabilities numerically 0 or 1 occurred 

#step-wise selection-AIC 
#step(lm.full, direction="both", k=2, trace=0)
#before removing outliers
logreg.aic <- glm(as.factor(status_label) ~ Current_Ratio + Net_Income_Margin + 
                    Return_on_Assets + Debt_to_EBITDA_Ratio + Debt_Service_Coverage_Ratio + 
                    Asset_Turnover_Ratio + Inventory_Turnover_Ratio + Fixed_Asset_Turnover_Ratio + 
                    Accounts_Payable_Turnover_Ratio + Price_to_Book_Ratio + cycle_type, 
                  family = binomial("logit"), data = bankrupt.train.vif)


#after removing outliers-no change

#step-wise selection-BIC
n = nrow(bankrupt.train.vif)
#step(lm.full,direction="both", k=log(n), trace=0)
#before removing outliers 
logreg.bic <- glm(as.factor(status_label) ~ Current_Ratio + Return_on_Assets + 
                    Inventory_Turnover_Ratio + Accounts_Payable_Turnover_Ratio + 
                    cycle_type, family = binomial("logit"), data = bankrupt.train.vif)

summary(logreg.bic)
summary(logreg.aic)

#after removing outliers: no change

######part 2: pca dataset 
lm.full.pca = glm(as.factor(status_label)~., data=bankrupt.train.pca, family=binomial("logit")) 
#??? glm.fit: fitted probabilities numerically 0 or 1 occurred

#step-wise selection-AIC 
#step(lm.full.pca, direction="both", k=2, trace=0)
#before removing outliers: 
logreg.aic.pca <- glm(as.factor(status_label) ~ `Comp 3` + `Comp 4` + 
                        `Comp 6` + `Comp 7` + `Comp 8` + `Comp 9` + `Comp 10` + `Comp 11` + 
                        `Comp 12` + `Comp 13` + `Comp 15` + `Comp 17` + `Comp 19` + 
                        `Comp 20` + cycle_type, family = binomial("logit"), data = bankrupt.train.pca)

#after removing outliers-changed
logreg.aic.pca <- glm(as.factor(status_label) ~ `Comp 1` + `Comp 3` + 
                        `Comp 4` + `Comp 5` + `Comp 6` + `Comp 7` + `Comp 8` + `Comp 11` + 
                        `Comp 12` + `Comp 14` + `Comp 15` + `Comp 16` + `Comp 17` + 
                        `Comp 19` + `Comp 20` + cycle_type, family = binomial("logit"), data = bankrupt.train.pca)


#step-wise selection-BIC
#step(lm.full.pca,direction="both", k=log(n), trace=0)

#before removing outliers
logreg.bic.pca <- glm(as.factor(status_label) ~ `Comp 3` + `Comp 4` + 
                        `Comp 6` + `Comp 7` + `Comp 8` + `Comp 9` + `Comp 10` + `Comp 11` + 
                        `Comp 12` + `Comp 13` + `Comp 15` + `Comp 17` + `Comp 20` + 
                        cycle_type, family = binomial("logit"), data = bankrupt.train.pca)


#after removing outliers-changed 
logreg.bic.pca <- glm(as.factor(status_label) ~ `Comp 1` + `Comp 3` + 
                        `Comp 4` + `Comp 5` + `Comp 6` + `Comp 7` + `Comp 8` + `Comp 11` + 
                        `Comp 12` + `Comp 15` + cycle_type, family = binomial("logit"), 
                      data = bankrupt.train.pca)

###### step 3: performance evaluation with threshold tuning 
#four models 
#1) logreg.aic
#2) logreg.bic
#3) logreg.aic.pca 
#4) logreg.bic.pca 

#1) and 2)
models <- list(logreg.aic, logreg.bic)
model_metrics <- list()
threshold <- seq(from = 0.3, to = 0.95, by = 0.05)

for (model_idx in 1:length(models)) {
  #make predictions
  predict <- predict(models[[model_idx]], newdata = bankrupt.test, type = "response")
  
  #initialize vectors for each model
  sensitivity <- rep(NA, length(threshold))
  specificity <- rep(NA, length(threshold))
  misclassification <- rep(NA, length(threshold))
  
  for (i in 1:length(threshold)) {
    Y.hat <- ifelse(predict > threshold[i], 1, 0)
    results <- table(Y.hat, bankrupt.test$status_label)
    
    #compute sensitivity and specificity
    sensitivity[i] <- results[2,2]/(results[1,2] + results[2,2])
    specificity[i] <- results[1,1]/(results[1,1] + results[2,1])
    
    #compute misclassification rate 
    misclassification[i] <- (results[1,2] + results[2,1]) / length(Y.hat)
  }
  
  #find the best threshold
  composite <- (0.25*sensitivity)+(0.5*specificity)+(0.25*(1-misclassification))
  optimal_threshold_index <- which.max(composite)
  optimal_threshold <- threshold[optimal_threshold_index]
  print(optimal_threshold)
  
  #store metrics 
  model_metrics[[model_idx]] <- c(
    sensitivity[optimal_threshold_index],
    specificity[optimal_threshold_index],
    misclassification[optimal_threshold_index]
  )
}

par(mfrow=c(1,1))
plot(threshold, sensitivity, type = "l", col = "blue", xlab = "Threshold", ylab = "Sensitivity", main = "Model Sensitivity vs. Threshold")
lines(threshold, specificity, type = "l", col = "red")
lines(threshold, misclassification, type = "l", col = "green")
legend("topright", legend = c("Sensitivity", "Specificity", "Misclassification"), col = c("blue", "red", "green"), lty = 1)

log.reg.aic.metrics <- model_metrics[1]
log.reg.aic.metrics
#sensitivity: 0.6921120 spec: 0.4833333 misclass: 0.3226950 (before and after removing outliers)
log.reg.bic.metrics <- model_metrics[2]
log.reg.bic.metrics
#sensitivity: 0.6959288 spec: 0.4666667 misclass: 0.3203310 (before and after  removing outliers)

#3) and 4)
#transform the testing data 
x.scaled.test <- scale(bankrupt.test[, -which(names(bankrupt.test) %in% c("status_label", "cycle_type"))])
Z.test <- x.scaled.test %*% (pca.model$loadings)
cycle_type.test <- bankrupt.test$cycle_type
bankrupt.test.pca <- as.data.frame(cbind(status_label = bankrupt.test$status_label, Z.test, cycle_type=cycle_type.test))

# col names need to match
#predict <- predict(logreg.aic.pca, newdata=bankrupt.test.pca, type = "response") #??? 
models2 <- list(logreg.aic.pca, logreg.aic.pca)
model_metrics2 <- list()
threshold2 <- seq(from = 0.3, to = 0.95, by = 0.05)

for (model_idx in 1:length(models2)) {
  #make predictions
  predict <- predict(models2[[model_idx]], newdata = bankrupt.test.pca, type = "response")
  
  #initialize vectors for each model
  sensitivity2 <- rep(NA, length(threshold2))
  specificity2 <- rep(NA, length(threshold2))
  misclassification2 <- rep(NA, length(threshold2))
  
  for (i in 1:length(threshold2)) {
    Y.hat <- ifelse(predict > threshold2[i], 1, 0)
    results <- table(Y.hat, bankrupt.test.pca$status_label)
    
    #compute sensitivity and specificity
    sensitivity2[i] <- results[2, 2] / (results[1, 2] + results[2, 2])
    specificity2[i] <- results[1, 1] / (results[1, 1] + results[2, 1])
    
    #compute misclassification rate 
    misclassification2[i] <- (results[1, 2] + results[2, 1]) / length(Y.hat)
  }
  
  #find threshold that maximizes specificity
  composite <- (0.25 * sensitivity2) + (0.5 * specificity2) + (0.25 * (1 - misclassification2))
  optimal_threshold_index <- which.max(composite)
  optimal_threshold <- threshold2[optimal_threshold_index]
  print(optimal_threshold)
  
  #store metrics 
  model_metrics2[[model_idx]] <- c(
    sensitivity2[optimal_threshold_index],
    specificity2[optimal_threshold_index],
    misclassification2[optimal_threshold_index]
  )
}

log.reg.aic.pca.metrics <- model_metrics2[1]
log.reg.aic.pca.metrics
#sensitivity: 0.1641221 spec: 0.9333333 misclass: 0.7813239 (before removing outliers)
#sensitivity: 0.6857506 spec: 0.4833333 misclass: 0.3286052 (after removing outliers)
log.reg.bic.pca.metrics <- model_metrics2[2]
log.reg.bic.pca.metrics
#sensitivity: 0.1641221 spec: 0.9333333 misclass: 0.7813239 (before removing outliers)
#sensitivity: 0.6857506 spec: 0.4833333 misclass: 0.3286052 (after removing outliers)

###### step 4: model diagnostics 
par(mfrow=c(2,2))
plot(logreg.bic)
#outliers <- c(7390, 8900, 8253, 2042, 749, 4932) this is moved to the top 

###### step 5: interaction model
#step(logreg.bic,.~.^2, direction="both", k=2, trace=0)
logreg.bic.interaction <- glm(as.factor(status_label) ~ Current_Ratio + Return_on_Assets + 
                                Inventory_Turnover_Ratio + Accounts_Payable_Turnover_Ratio + 
                                cycle_type + Current_Ratio:cycle_type + Current_Ratio:Return_on_Assets + 
                                Return_on_Assets:Accounts_Payable_Turnover_Ratio + Return_on_Assets:cycle_type + 
                                Return_on_Assets:Inventory_Turnover_Ratio + Inventory_Turnover_Ratio:Accounts_Payable_Turnover_Ratio + 
                                Current_Ratio:Inventory_Turnover_Ratio, family = binomial("logit"), data = bankrupt.train.vif)
summary(logreg.bic.interaction)
threshold <- seq(from = 0.3, to = 0.95, by = 0.05)
# Initialize vectors
sensitivity <- rep(NA, length(threshold))
specificity <- rep(NA, length(threshold))
misclassification <- rep(NA, length(threshold))

for (i in 1:length(threshold)) {
  predict <- predict(logreg.bic.interaction, newdata = bankrupt.test, type = "response")
  Y.hat <- ifelse(predict > threshold[i], 1, 0)
  results <- table(Y.hat, bankrupt.test$status_label)
  
  # Compute sensitivity and specificity
  sensitivity[i] <- results[2, 2] / (results[1, 2] + results[2, 2])
  specificity[i] <- results[1, 1] / (results[1, 1] + results[2, 1])
  
  # Compute misclassification rate
  misclassification[i] <- (results[1, 2] + results[2, 1]) / length(Y.hat)
}

# Find the best threshold
composite <- (0.25 * sensitivity) + (0.5 * specificity) + (0.25 * (1 - misclassification))
optimal_threshold_index <- which.max(composite)
optimal_threshold <- threshold[optimal_threshold_index]
print(optimal_threshold)

# Store metrics
model_metrics <- c(
  sensitivity[optimal_threshold_index],
  specificity[optimal_threshold_index],
  misclassification[optimal_threshold_index]
)
model_metrics
#sensitivity: 0.5610687 specificity: 0.5833333 misclassfication: 0.4373522

summary(logreg.bic.interaction)
################ section 4: LDA/QDA ################
#normality assumption
par(mfrow = c(2,2)) # Set the layout for multiple plots

for (feature in colnames(x.quant)) {
  qqnorm(bankrupt.train[[feature]], main=paste("Q-Q Plot for", feature))
  qqline(bankrupt.train[[feature]])
}
##fails normality assumption

################ section 5: impact of cycle (random forest model) ################
bankrupt_train_cycle0 <- subset(bankrupt.train, cycle_type == 0)
bankrupt_train_cycle1 <- subset(bankrupt.train, cycle_type == 1)
bankrupt_test_cycle0 <- subset(bankrupt.test, cycle_type == 0)
bankrupt_test_cycle1 <- subset(bankrupt.test, cycle_type == 1)

bankrupt_train_cycle0 <- bankrupt_train_cycle0[, -which(names(bankrupt_train_cycle0) == "cycle_type")]
bankrupt_test_cycle0 <- bankrupt_test_cycle0[, -which(names(bankrupt_test_cycle0) == "cycle_type")]
bankrupt_train_cycle1 <- bankrupt_train_cycle1[, -which(names(bankrupt_train_cycle1) == "cycle_type")]
bankrupt_test_cycle1 <- bankrupt_test_cycle1[, -which(names(bankrupt_test_cycle1) == "cycle_type")]

######cycle_type = 0 (easing)
par(mfrow = c(1,1)) # Set the layout for multiple plots

RF_bankrupt_cycle0 <- randomForest(as.factor(status_label) ~ ., data = bankrupt_train_cycle0, ntree = 100, mtry = 10)
#importance plot
varImpPlot(RF_bankrupt_cycle0)
importance(RF_bankrupt_cycle0)

#evaluate performance
Yhat_RF_cycle0 <- predict(RF_bankrupt_cycle0, newdata = bankrupt_test_cycle0, type = "class")
confusion_RF_cycle0 <- table(Yhat_RF_cycle0, bankrupt_test_cycle0$status_label)
sensitivity_RF_cycle0 <- confusion_RF_cycle0[2, 2] / (confusion_RF_cycle0[1, 2] + confusion_RF_cycle0[2, 2])
specificity_RF_cycle0 <- confusion_RF_cycle0[1, 1] / (confusion_RF_cycle0[1, 1] + confusion_RF_cycle0[2, 1])
misclass_RF_cycle0 <- (confusion_RF_cycle0[1, 2] + confusion_RF_cycle0[2, 1]) / length(Yhat_RF_cycle0)

RF_metrics_cycle0 <- c(sensitivity_RF_cycle0, specificity_RF_cycle0, misclass_RF_cycle0)
#0.8555305 0.4102564 0.1804979

######cycle_type = 1 (hiking)
RF_bankrupt_cycle1 <- randomForest(as.factor(status_label) ~ ., data = bankrupt_train_cycle1, ntree = 100, mtry = 10)
#importance plot
varImpPlot(RF_bankrupt_cycle1)
importance(RF_bankrupt_cycle1)

#evaluate performance
Yhat_RF_cycle1 <- predict(RF_bankrupt_cycle1, newdata = bankrupt_test_cycle1, type = "class")
confusion_RF_cycle1 <- table(Yhat_RF_cycle1, bankrupt_test_cycle1$status_label)
sensitivity_RF_cycle1 <- confusion_RF_cycle1[2, 2] / (confusion_RF_cycle1[1, 2] + confusion_RF_cycle1[2, 2])
specificity_RF_cycle1 <- confusion_RF_cycle1[1, 1] / (confusion_RF_cycle1[1, 1] + confusion_RF_cycle1[2, 1])
misclass_RF_cycle1 <- (confusion_RF_cycle1[1, 2] + confusion_RF_cycle1[2, 1]) / length(Yhat_RF_cycle1)

RF_metrics_cycle1 <- c(sensitivity_RF_cycle1, specificity_RF_cycle1, misclass_RF_cycle1)
#0.99416910 0.00000000 0.06318681

