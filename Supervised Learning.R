######################## DAMI Programming Assignment 3: Classification #########################

# Submitted by Melkote Shivashankar sandesh 

# Create a dataset
# The "almost" same example from the lecture

buys <- c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no")
credit <- c("fair", "excellent", "fair", "fair", "fair", "excellent", "excellent", "fair", "fair", "fair", "excellent", "excellent", "fair", "excellent")
student <- c("no", "no", "no","no", "yes", "yes", "yes", "no", "yes", "yes", "yes", "no", "yes", "no")
income <- c("high", "high", "high", "medium", "low", "low", "low", "medium", "low", "medium", "medium", "medium", "high", "medium")
age <- c(25, 27, 35, 41, 48, 42, 36, 29, 26, 45, 23, 33, 37, 44) # we change the age from categorical to numeric

data <- data.frame(age, income, student, credit, buys) # create a data frame using the attributes defined above, where buys is the class label
data

attach(data) # attach the dataset so that you can use the column names directly in the later coding (for example, using age instead of data$age)


############### Part 1: Calculate Information Gain ###########

# Entropy before split
# The function requires the frequency list of each class label
info <- function(CLASS.FREQ){
  freq.class <- CLASS.FREQ
  info <- 0
  for(i in 1:length(freq.class)){
    if(freq.class[[i]] != 0){ # if the number of examples in class i is not 0
      ### calculate the entropy for class i and name it as "entropy". Hint: use "sum(freq.class)" to calculate the total number of examples
        pi <- freq.class[[i]] / sum(freq.class)
        entropy <-  -( pi * log2(pi)) # 2 points Calculate pi,
        }else{ 
      entropy <- 0 # if we face log(0), the entropy is given 0
    }
    info <- info + entropy # sum up entropy from all classes
  }
  return(info)
}

buys.freq <- table(buys)
buys.freq
info.buys <- info(buys.freq)
info.buys

# Entropy after split
# This function requires the attribute name and class name
info.target <- function(ATTRIBUTE, CLASS){
  input <- data.frame(ATTRIBUTE, CLASS)
  freq <- as.matrix(table(input))
  print(freq) # here you can check the contingency table for ATTRIBUTE and CLASS
  info.target <- 0
  for(j in 1:nrow(freq)){
    ## calculate the entropy of splitting on this ATTRIBUTE

    spl_entropy1 <- sum(freq[j,]) / sum(freq)
    spl_entropy <- spl_entropy1 * info(freq[j,])
    
    info.target <- info.target +  spl_entropy  # 2 points 
  }
  return(info.target)
}

# for example
info.income <- info.target(income, buys)
info.income
info.student <- info.target(student, buys)
info.student
info.credit <- info.target(credit, buys)
info.credit

# Information gain
gain <- function(ATTRIBUTE, CLASS){
  CLASS.FREQ <- table(CLASS)
  if(class(ATTRIBUTE) == "numeric"){ # Numeric attributes
    input <- data.frame(ATTRIBUTE, CLASS)
    input <- input[order(ATTRIBUTE),] # sort the examples based on the numeric attribute
    rownames(input) <- seq(length = nrow(input)) # reorder the row names (indexes)
    gain.num <- c()
    # in this for loop, we calculate the information gain for each split point
    # that is generated between two consecutive examples
    for(i in 1:(nrow(input) - 1)){ 
      split <- (input[i, 1] + input[i+1, 1])/2
      small <- ifelse(input[, 1] <= split, "Yes", "No")
      y <- table(CLASS)
      ## the code to calculate the information gain for this ATTRIBUTE, using the info and info.target function
            gain.num[i] <-  info(y) - info.target(small,CLASS)
      # 2 points
    }
    return(max(gain.num))
  }else{ # Categorical attributes
    ## the code to calculate the information gain for this ATTRIBUTE, using the info and info.target function 
    gain.cat <- info(CLASS.FREQ) - info.target(ATTRIBUTE, CLASS) # 2 points
    return(gain.cat)
  }  
}
# for example
gain(age, buys)
gain(student, buys)
gain(income, buys)
gain(credit, buys)

detach(data) # corresponding to the attach(data) earlier 

############# Part 2: Perceptron Learning Algorithm ###############

# Create 100 uniformly distributed points in [-1,1]x[-1,1]
set.seed(5)
x1 <- runif(100, -1, 1)
x2 <- runif(100, -1, 1)
X <- data.frame(x1, x2)
# Randomly select two points to create a line going through them, points on one side
# of the line get class label -1, the ones on the other side get class label 1
p1x <- runif(1, -1, 1)
p1y <- runif(1, -1, 1)
p2x <- runif(1, -1, 1)
p2y <- runif(1, -1, 1)
slope <- (p1y-p2y)/(p1x-p2x)
intercept <- p1y - slope * p1x
y <- ifelse((slope*X[,1]+intercept) >= X[,2], -1, 1) # assign class label 
# plot the data
data <- cbind(X, y)
plot(data[data$y == -1, 1:2], xlim=c(-1,1), ylim=c(-1,1), col="red")
points(data[data$y == 1, 1:2], col="green")
abline(intercept, slope)

# Perceptron learning algorithm  
perceptron <- function(DATA, CLASS){
  X.new <- cbind(1, DATA) # Add X0, which is 1 for all examples 
 print(X.new)
   w <- matrix(0,1,3) # Initial weight vector with only 0, note that the first element is w0
   print(w)
   while(TRUE){
    # use matrix product to calculate the hypothesis. Hint: make sure both parts are matrix 

     hypothesis <- w %*% t(X.new) # 5 points
   # print(hypothesis)
    label.new <- ifelse(hypothesis >= 0, 1, -1) # use the sign of hypothesis to decide the class label
    if(all(label.new==CLASS)){ # if the new class label from hypothesis is the same with the true class label, then stop the iteration
      return(w)
      break
    }else{ # if the new class label from hypothesis is not the same with the true label, update the weight vector and continue the iteration
      where <- label.new == CLASS
      misclass <- sample(grep("FALSE", where), 1) # randomly select a misclassified point
      # update the weight vector using this randomly selected misclassified point
     w <- w + y[misclass]*as.matrix(X.new[misclass,]) # 5 points
    }   
  }  
} 

perceptron(X, y)

############# Part 3: Different Classifiers #############

# In this part, we use the Adult dataset from assignment 1
# first set the working directory to where you have the adult dataset saved
adult_db <- read.table(file="~/Desktop/adult.data.txt", header=FALSE, sep=",", 
                       na.strings=c("?","NA"), strip.white=TRUE, 
                       stringsAsFactors=FALSE)
names(adult_db) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "class")
adult_db_nomiss <- na.omit(adult_db) # remove all the rows with missing values
row.names(adult_db_nomiss) <- 1:nrow(adult_db_nomiss)
adult_db_nomiss$class <- as.factor(adult_db_nomiss$class)
set.seed(5)
data <- adult_db_nomiss[sample(1:nrow(adult_db_nomiss), 100),] # randomly select 100 rows to be used later

attach(data)
summary(data)
set.seed(5)
data.train <- c(sample(1:100, 50)) # randomly select 50 examples as training data
print(data[data.train,]$workclass)

## Decision Tree
library(tree)
#build a decision tree called tree.data by using the tree() function on the training data
#tree.data <- tree (class ~age+workclass+fnlwgt+education+education_num+marital_status+occupation+relationship+race+sex+capital_gain+capital_loss+hours_per_week+native_country+class, data[data.train,] ) # 3 points
tree.data <- tree(class ~. , data [data.train, ]) # 3 points
plot(tree.data)
text(tree.data)

## Naive Bayes
library(e1071)
#build a naive Bayes model called nb.data by using the naiveBayes() function on the training data

nb.data <- naiveBayes(class ~ . ,data = data[data.train,]) # 3 points 
prediction <- predict(nb.data, data[-data.train, ], type="class") # predict the class labels for the test data
print(prediction)
res.nb <- table(prediction, data[-data.train, "class"]) # generate a confusion matrix using the predictions and the real labels of the test data
print(res.nb)
# calculate the accuracy, precision and recall by using the confusion matrix res.nb 5 points
accuracy <- sum(diag(res.nb))/(sum(res.nb))  # 1 point 
print(accuracy)

recall <-   res.nb[2,2] / sum(res.nb [2,] ) # Hit Recall
print(recall)
precision <- res.nb[2,2] / sum(res.nb [,2] ) # Miss precision  # 1 point 
print(precision)

# we can also only work with certain features, for example, here we select only the numeric features
data.select <- subset(data, select=c(age, fnlwgt, education_num, capital_gain, capital_loss, hours_per_week, class))


## Support Vector Machine 
#build a support vector machine  called svm.data on data.select using the svm() function, where choose kernel radial and set cost to 1
svm.data <- svm( class ~ . ,data = data.select[data.train,],  kernel = "radial" , cost = 1 ) # 3 points
#generate predictions using the predict() function
prediction <- predict(svm.data , data.select[-data.train,], type="class") # 1 point 
print(prediction)
#generate the confusion matrix
res.svm <- table(prediction, data.select[-data.train,"class"]) # 1 point 
print(res.svm)
#calculate accuracy

accuracy.svm <- sum(diag(res.svm)) / sum(res.svm) # 1 point 
print(accuracy.svm)


## Random Forest
library(randomForest)
library(ROCR)
#build a random forest model called rf.data on data.select using the randomForest() function, where number of trees is 100 and produce the variable importance
rf.data <- randomForest(class~.,data = data.select[data.train,],  ntree= 100, importance = TRUE)  # 3 points 
#generate predictions using the predict() function
prediction <- predict(rf.data , data.select[data.train,], type ="class") # 1 point 
#generate the confusion matrix
res.rf <- table( prediction, data.select[-data.train,"class"] ) # 1 point 
#calculate accuracy
accuracy.rf <- sum(diag(res.rf)) / sum(res.rf) # 1 point 
print(accuracy.rf)
#generate the probabilities of each prediction, using the predict() function where the type should be "prob"
probability <- predict(rf.data,data.select[-data.train,], type="prob") # 1 point 
# if we are interested in the class >50K, we can calculate its AUC score like this:
auc.rf <- as.numeric(performance(prediction(probability[,2], data.select[-data.train, "class"]), measure='auc')@y.values) 
auc.rf
detach(data)
################################### END #####################################