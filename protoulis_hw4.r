library(freqparcoord)
library(tidyverse)
library(regtools)
data(mlb)



#1, p62-------------------------------
# In Section 1.12.1.2, the reader was reminded that the results of a cross-
#   validation are random, due to the random partitioning into training and
# test sets. Try doing several runs of the linear and k-NN code in that section,
# comparing results.

# arguments :
#
# data : f u l l data
# ycol : column number of resp . var .
# predvars : column numbers of predictors
# p: prop . for training set
# meanabs : see ' value ' below
# value : if meanabs is TRUE, the mean absolute
# prediction error ; otherwise , an R l i s t
# containing pred . , real Y

xvalpart <- function(data ,p) {
  n <-nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain , replace=FALSE)
  list ( train=data [ trainidxs ,] ,
         valid=data[-trainidxs , ] )
}

xvallm <- function(data , ycol , predvars ,p, meanabs=TRUE){
  tmp <- xvalpart (data ,p)
  train <- tmp$train
  valid <- tmp$valid
  # f i t model to training data
  # 14 There are sophisticated packages on CRAN for this, such as cvTools [4]. But to
  # keep things simple, and to better understand the concepts, we will write our own code.
  # Similarly, as mentioned, we will not use R's predict() function for the time being.
  # 1.12. CROSS-VALIDATION 29
  trainy <- train [ , ycol ]
  trainpreds <- train [ , predvars ]
  # using matrix form in lm() call
  trainpreds <- as.matrix( trainpreds )
  lmout <- lm( trainy ~ trainpreds )
  # apply fitted model to validation data ; note
  # that %???% works only on matrices , not data frames
  validpreds <- as.matrix( valid [ , predvars ])
  predy <- cbind(1 , validpreds )%???% coef(lmout)
  realy <- valid [ , ycol ]
  if (meanabs) return(mean(abs(predy - realy )))
  list (predy = predy , realy = realy )
}
 
ourRes = vector()

for (i in 1:10) {
  ourRes[i] = xvallm(mlb ,5 ,c(4 ,6) ,2/3)
}
ourRes #how many pounds are we off the real result, on average

#k-NN


function(data , ycol , predvars ,k ,p, meanabs=TRUE){
  # cull out just Y and the Xs
  data <- data [ , c( predvars , ycol )]
  ycol <- length( predvars ) + 1
  tmp <- xvalpart (data ,p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix( valid )
  xd <- preprocessx ( train [,- ycol ] ,k)
  kout <- knnest ( train [ , ycol ] ,xd ,k)
  predy <- predict(kout , valid [,- ycol ] ,TRUE)
  realy <- valid [ , ycol ]
  if (meanabs) return(mean(abs(predy - realy )))
  list (predy = predy , realy = realy )
}


set.seed(9999)
ourResKnn = vector()
for (i2 in 1:10) {
  ourResKnn[i2] = xvalknn(mlb ,5 ,c(4 ,6) ,25 ,2/3)
}
ourResKnn
mean(abs(ourRes - ourResKnn)) #mean absolute difference between the two 

#2, p62-------------------------------

# 2. Extend (1.28) to include interaction terms for age and gender, and age^2
# and gender. Run the new model, and find the estimated effect of being
# female, for a 32-year-old person with a Master's degree.

library ( freqparcoord )
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng [ edu >= 13 ,]
pe <- tmp[ , c(1 ,12 ,9 ,13 ,14 ,15 ,8)]
pe <- as.matrix(pe)

wageModel = lm(wageinc ~
    age+age2+wkswrkd+ms+phd+fem + age:fem + age2:fem ,data=prgeng)
wageModel
coeffs = coefficients(wageModel); coeffs

ourPrediction = predict(object = wageModel,newdata = c(data.frame(age = 32, age2 = 32^2, wkswrkd = 52,ms = 1, phd = 0.3, fem = 1)))
#phd = 0.3 is an assumption: I assumed that 30% of 32year old female MSc holders also have a PhD
ourPrediction

#3, p62-------------------------------

# Consider the bodyfat data mentioned in Section 1.2. Use lm() to form
# a prediction equation for density from the other variables (skipping the
#                                                             first three), and comment on whether use of indirect methods in this way
# seems feasible.
library(mfp)
data(bodyfat)

bFatModel = lm(ourBodyFat ~ 
                 age + height + weight + wrist + abdomen ,data = bodyfat)
# ourTest = predict(object = bFatModel,newdata = c(data.frame(age = bodyfat$age[1], age2 = bodyfat$age2[1],height= bodyfat$height[1],
#                                                             weight = bodyfat$weight[1], wrist = bodyfat$wrist[1],abdomen = bodyfat$abdomen[1])))

bodyfatTest = bodyfat %>%
  map(.x = bodyfatTest, .f = predict(bFatModel))



# predVec = vector()
# predList = list()
# for (i in 1:nrow(bodyfat) ) {
#   predVec[i] = bodyfat[i,]
# }
#   
# 
# predListaaaa = lapply(predList, function(x){predict(bFatModel, newdata = x )}) #how to do this??


#4, p62--------------------

# a)
# 
# The overall population mean height can be calculated as following: Find the mean height for males and the mean height for females.
# Then, sum them together by using the corresponding share of population that is male or female as weights 
# (e.g 0.48*meanMaleHeight + 0.52*meanFemaleHeight)


# b) similarly, find proportion of males > 70in and proportion of females > 70 in
# proportion of overall population would be: malePerc*0.48 + femalePerc*0.52



#1, p.120------------------------

# (a) Form an approximate 95% confidence interval for ?? 6 in the model
# (1.28).
# (b) Form an approximate 95% confidence interval for the gender effect
# for Master's degree holders, ?? 6 + ?? 7 , in the model (1.28).


confint(wageModel, level = 0.95)


#we need to make msc "static" = 1 then run confint again. How?

ourPrediction2 = predict(object = wageModel,newdata = c(data.frame(ms = 1, phd = 0.3)))

# 2 p.120------------- 
#The full bikeshare dataset spans 3 years' time. Our analyses here have
# only used the first year. Extend the analysis in Section 2.8.5 to the full
# data set, adding dummy variables indicating the second and third year.
# Form an approximate 95% confidence interval for the difference between
# the coefficients of these two dummies.


# 3.p.120-----------------------
# Suppose we are studying growth patterns in children, at k particular
# ages. Denote the height of the i th child in our sample data at age j by
# H ij , with H i = (H i1 ,...,H ik ) ' denoting the data for child i. Suppose the
# population distribution of each H i is k-variate normal with mean vector µ
# and covariance matrix ??. Say we are interested in successive differences in
# heights, D ij = H i,j+1 ???H ij , j = 1,2,...,k ???1. Define D i = (D i1 ,...,D ik ) ' .
# Explain why each D i is (k???1)-variate normal, and derive matrix expressions
# for the mean vector and covariance matrices.


# 4.p.120----------------------- 
#In the simulation in Section 2.9.3, it is claimed that ?? 2 = 0.50. Confirm
# this through derivation.
