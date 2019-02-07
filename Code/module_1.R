library(car)
library(leaps)
library(faraway)
library(glmnet)
library(dplyr)

#=====Read data=================================================
bf <- read.csv('BodyFat.csv', header = TRUE)
bf <- bf[ , -1]
names(bf) <- tolower(names(bf))

#=====Summary data============================================
summary(bf)

for(i in 1:ncol(bf)){
  hist(bf[, i], breaks = 30, main = names(bf)[i])
}

# ankle 31, 86 large than expected but still within threshold
# 39 largest in biceps, knee, hip, thigh, abdomen, chest, neck
# 182 smallest in abdomen, hip, thigh, chest, adiposity, weight


#=====Verify data============================================  

bdfat <- 495/bf$density - 450
plot(bf$bodyfat, bdfat)

hist(bdfat - bf$bodyfat, breaks = 20)
sp_point <- which(abs(bdfat - bf$bodyfat) > 2)
sp_data <- bf[sp_point, ] 
sp_data <- cbind(bdfat[sp_point], sp_data)
View(sp_data)


bmi <-  (bf$weight*0.454)/(bf$height*2.54/100)^2
hist(bmi - bf$adiposity, breaks = 20)
sp_point1 <- which(abs(bmi - bf$adiposity) > 5)
sp_data1 <- cbind(bf[sp_point1 , 1:6], bmi = bmi[sp_point1], bf[sp_point1, 7:16])
# 42 wrong height
bf[42, ]$height <- round(sqrt((bf[42, ]$weight*0.454)/bf[42, ]$adiposity)*100/2.54, 2)

bf <- bf[ , -2]
#=====Diagnose data==============================================

fit1 <- lm(bodyfat ~ ., data = bf[-182, ])
summary(fit1)
plot(fit1, which = 4)
abline(h = 4/(nrow(bf) - ncol(bf)), lty = 2)

outlierTest(fit1) # 221

bf[c(39, 86, 221), ]
# 39 too fat, 86 strange ankle, 221 large abdomen

# confirmed to exclude 39, 182 in the model

#=====Stepwise selection==========================================================
set.seed(0202)

regModel <- function(bf_data){
  result <- list()
  fit2 <- lm(bodyfat ~ ., data = bf_data) # without 2 outliers

  fit2_AIC <- step(fit2, k = 2, direction = 'backward', trace = F)
  fit2_BIC <- step(fit2, k = log(nrow(bf_data)), direction = 'backward', trace = F)

  fit_base <- lm(bodyfat ~ 1, data =  bf_data)
  base_AIC <- step(fit_base, scope = list(lower = ~ 1, upper = fit2), 
                   direction = 'forward', trace = F)
  base_BIC <- step(fit_base, scope = list(lower = ~ 1, upper = fit2), 
                   k = log(nrow(bf_data)), direction = 'forward', trace = F)
  
  # fit_both <- lm(bodyfat ~ weight + thigh + height + biceps + hip + wrist, data = bf_data)
  # fit_both_AIC <- step(fit_both, k = 2, scope = list(lower = ~ 1, upper = fit2), trace = F)
  # fit_both_BIC <- step(fit_both, k = log(nrow(bf_data)), scope = list(lower = ~ 1, upper = fit2), trace = F)

  result <- list()
  result[['fit']] = fit2
  result[['back_AIC']] = fit2_AIC
  result[['back_BIC']] = fit2_BIC
  result[['for_AIC']] = base_AIC
  result[['for_BIC']] = base_BIC
  # result[['both_AIC']] = fit_both_AIC
  # result[['both_BIC']] = fit_both_BIC

  return(result)
}

# step_AIC <- step(fit2_AIC, direction = 'both', k = 2)
# step_BIC <- step(fit2_BIC, direction = 'both', k = log(nrow(bf_data)))
# 

sumReg <- function(fun, reg){
  for(i in 1:7){
    print(names(reg)[i])
    cat("\t")
    print(fun(reg[[i]]))
  }
}

sumReg0 <- function(fun, reg, n = length(reg)){
    out <- data.frame()
  for(i in 1:n){
    tmp <- data.frame(t(round(fun(reg[[i]]), 4)))
    out <- bind_rows(out, tmp)
  }
    row.names(out) <- names(reg)
    return(out)
}
a <- sumReg0(coef, reg_with_bmi)


Rsqr <- function(reg){
  tmp <- data.frame()
  for(i in 1:length(reg)){
    out <- data.frame("R square" =  round(summary(reg[[i]])$r.squared, 4), "adj R square" = round(summary(reg[[i]])$adj.r.squared, 4), 
                      "number of var" = length(reg[[i]]$coefficients))
    tmp <- rbind(tmp, out)}
  row.names(tmp) <- names(reg)
  return(tmp)}



bf_data <- bf[c(-39, -182), ]
bf_new <- bf[c(-39, -182), ]
bf_new$w.sqr <- bf_new$weight ^ 2
bf_new$h.inv <- 1/bf_new$height

bf_new1 <- bf[c(-39, -182), ]
bf_new1$adj.abd <- bf_new1$abdomen /bf_new1$height

reg_with_bmi <- regModel(bf_data)
reg_without_bmi <- regModel(bf_new)
reg_without_bmi1 <- regModel(bf_new1)

sumReg0(coef, reg_with_bmi)
sumReg0(coef, reg_without_bmi)
sumReg0(coef, reg_without_bmi1)

sumReg0(vif, reg_without_bmi)
sumReg0(vif, reg_without_bmi1)

summary(reg_with_bmi[["for_BIC"]])
summary(reg_without_bmi[["for_BIC"]])
summary(reg_without_bmi1[["for_BIC"]])


for(i in 1:5){
  print(names(reg_with_bmi)[i])
  cat('\t')
  print(mean(reg_with_bmi[[i]]$residuals^2))
}


#======Mallow's cp==================================================================================================
X <- model.matrix(lm(bodyfat ~ ., data = bf_data))[, -1]
Y <- bf_data[, 1]
g <- leaps(X, Y, nbest = 1)
Cpplot(g)

X <- model.matrix(lm(bodyfat ~ ., data = bf_new))[, -1]
Y <- bf_new[, 1]
g <- leaps(X, Y, nbest = 1)
Cpplot(g)

#=====10 fold Train test split============================================================

cvReg <- function(data, model){
  set.seed(0202)
  index <- sample(1:10, nrow(bf_data), replace = T)
  hist(index, breaks = 0:10, include.lowest = F)
  mse <- c()
  for(i in 1:10){
    train.ind <- index[index == i]
    train_data <- data[train.ind, ]
    test_data <- data[-train.ind, ]
    fit <- lm(model, data = data)
    out <- mean((predict(fit, newdata = test_data) - test_data$bodyfat)^2)
    mse <- c(mse, out)
  }
  return(mse)
}

cvReg(bf_data, bodyfat ~ abdomen + weight + wrist)
cvReg(bf_data, reg_with_bmi[["for_AIC"]][["call"]][["formula"]])
cvReg(bf_new, reg_without_bmi[["for_BIC"]][["call"]][["formula"]])
cvReg(bf_new1, reg_without_bmi1[["for_BIC"]][["call"]][["formula"]])

#======ridge================================================================
fit_BIC <- lm(bodyfat ~ abdomen + weight + wrist, data = bf_data)


lambda <- seq(0, 10, length.out = 50)
bf_x <- model.matrix(fit_BIC)[ , -1]
bf_y <- as.matrix(bf_data[ , 1])
fit_r <- glmnet(bf_x, bf_y, alpha = 1, lambda = lambda)
fit_r_cv <- cv.glmnet(bf_x, bf_y, alpha = 1)
best_lam <- fit_r_cv$lambda.min

# newx_r <- as.matrix(bf_test[ , c(8, 3, 15)])
# newx <- bf_test[ , c(3, 8, 15)]
# newy <- bf_test[ , 1]
pred_r <- predict(fit_r, s = best_lam, newx = bf_x, type = 'link')

print(mean((pred_r - bf_y)^2))
print(mean((fit_BIC$residuals)^2))
# not enough proof for improvement

predict(fit_r, s = best_lam, newx = newx_r, type = 'coefficients')
coef(fit_BIC)

#=====Lasso=====================================================================
X <- model.matrix(lm(bodyfat ~ ., bf_new))[, -1]
Y <- bf_new[, 1]
set.seed(0202)
l_cv <- cv.glmnet(X[1:200, ], Y[1:200], alpha = 1)
lambda <- l_cv$lambda.1se
fit_l <- glmnet(X, Y, lambda = lambda, alpha = 1)
print(fit_l)
plot(l_cv)
coef(fit_l)
