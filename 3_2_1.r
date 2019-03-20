
library("ElemStatLearn")
train <- subset( prostate, train==TRUE )[,1:9] # w.o the last variable
test  <- subset( prostate, train==FALSE)[,1:9] # w.o the last variable


# fit linear model on training dataset using LS method
trainst <- train
for(i in 1:8) {
  trainst[,i] <- trainst[,i] - mean(prostate[,i]);
  trainst[,i] <- trainst[,i]/sd(prostate[,i]);
}

fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )

fitlsr <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst )


## check testing errors
testst <- test
for(i in 1:8) {
  testst[,i] <- testst[,i] - mean(prostate[,i]);
  testst[,i] <- testst[,i]/sd(prostate[,i]);
}

# mean prediction error on testing data using mean training value
mean(trainst[,9])                             # 2.452345
# mean (absolute) prediction error
mean(abs(testst[,9]-mean(trainst[,9])))       # 0.7409334
# mean (squared) prediction error
mean((testst[,9]-mean(trainst[,9]))^2)        # 1.056733
# standard error of mean (squared) prediction error
sd((testst[,9]-mean(trainst[,9]))^2)/sqrt(30) # 0.396115

# mean prediction error based on full model, reproducing some numbers in Table 3.3 on page 63
test.fitls=predict(fitls, newdata=testst)  
# mean (absolute) prediction error
mean(abs(test[,9]-test.fitls))                # 0.5233719
# mean (squared) prediction error
mean((test[,9]-test.fitls)^2)                 # 0.521274
# standard error of mean (squared) prediction error
sd((test[,9]-test.fitls)^2)/sqrt(30)          #  0.178724


# mean prediction error based on reduced model
test.fitlsr=predict(fitlsr, newdata=testst)  
# mean (absolute) prediction error
mean(abs(test[,9]-test.fitlsr))                # 0.5139785
# mean (squared) prediction error
mean((test[,9]-test.fitlsr)^2)                 # 0.4563321
# standard error of mean (squared) prediction error
sd((test[,9]-test.fitlsr)^2)/sqrt(30)          # 0.1242902




############################ Solution:::::::::::::

MaPE1<-c() # mean (absolute) prediction error
MsPE1<-c() # mean (squared) prediction error


# mean prediction error based on full model
MaPE2<-c() # mean (absolute) prediction error
MsPE2<-c() # mean (squared) prediction error


# mean prediction error based on reduced model  
MaPE3<-c() # mean (absolute) prediction error
MsPE3<-c() # mean (squared) prediction error



for ( i in 1:100)
{

labling<- sample(c(rep(1,67),rep(0,30)),replace = FALSE) # Creates vector w/ 67 T & 30 F
train <- subset( prostate, labling==TRUE )[,1:9] # w.o the last variable
test  <- subset( prostate, labling==FALSE)[,1:9] # w.o the last variable

# fit linear model on training dataset using LS method

trainst <- train
for(i in 1:8) {
  trainst[,i] <- trainst[,i] - mean(prostate[,i]);
  trainst[,i] <- trainst[,i]/sd(prostate[,i]);
}
fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )

## [2] Fit the reduced model:
fitlsr <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst )

## check testing errors

testst <- test
for(i in 1:8) {
  testst[,i] <- testst[,i] - mean(prostate[,i]);
  testst[,i] <- testst[,i]/sd(prostate[,i]);
}



MaPE1 <- append(MaPE1,mean(abs(testst[,9]-mean(trainst[,9])))) # mean (absolute) prediction error
MsPE1 <- append(MsPE1,mean((testst[,9]-mean(trainst[,9]))^2)) # mean (squared) prediction error       


test.fitls=predict(fitls, newdata=testst)  # mean prediction error based on full model
MaPE2 <- append(MaPE2,mean(abs(test[,9]-test.fitls)))# mean (absolute) prediction error
MsPE2 <- append(MsPE2,mean((test[,9]-test.fitls)^2))# mean (squared) prediction error


test.fitlsr=predict(fitlsr, newdata=testst) # mean prediction error based on reduced model  
MaPE3 <- append(MaPE3,mean(abs(test[,9]-test.fitlsr)))# mean (absolute) prediction error
MsPE3 <- append(MsPE3,mean((test[,9]-test.fitlsr)^2))# mean (squared) prediction error

}
# Comparing Naive vs Full and Reduced

# Naive vs Full model mean absolute prediction error
t.test(MaPE1,MaPE2,alternative = "greater") # p-value < 2.2e-16
# Naive vs Full model mean squared prediction error
t.test(MsPE1,MsPE2,alternative = "greater") # p-value < 2.2e-16

# Naive vs Reduced model mean absolute prediction error
t.test(MaPE1,MaPE3,alternative = "greater") # p-value < 2.2e-16
# Naive vs Full model mean squared prediction error
t.test(MsPE1,MsPE3,alternative = "greater") # p-value < 2.2e-16

#======================================================================
# Full and reduced model are significantly better than the naive model.
#======================================================================

# Comparing Full against Reduced 
# Absolute prediction error
t.test(MaPE2,MaPE3,alternative = "two.sided") # p-value = 0.8373
# Squared prediction error
t.test(MsPE2,MsPE3,alternative = "two.sided") # p-value = 0.1406
#======================================================================
# No significant evidence found to conclude that one superior model 
# between reduced and full.
#======================================================================



