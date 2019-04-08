setwd("/Users/aytacozkan/works/text.mining/exam/Final-Exam-18-19/")

# data <- read.csv("spamDataNum.csv", stringsAsFactors = FALSE)

fname <- "spamDataNum.csv"
headset <- read.csv(fname, header = TRUE)

df = data.frame(headset)

# (1 for spams and 0 for non spams).
#spamData <- subset(df, CSpam == 1)

df$CSpam <- factor(df$CSpam)

df$CSpam=NULL
# 2. Apply the algorithm kmeans with k = 2 to this data.
km <- kmeans(df, 2)

# 3. Compare the clusters and the classes. What 'accuracy' do we obtain?
df1 <- data.frame(headset)

# Compare the clusters and the classes
conf1 <- table(df1$CSpam, km$cluster)
conf1

# Accuracy
acc1 <- sum(diag(conf1)) / sum(conf1)
acc1

#4 Normalize the data and apply again kMeans. How do you explain the
# difference between the two accuracies ?
normalize <- function(x)
{
  return ((x-min(x))/(max(x)-min(x)))
}
data_norm <- as.data.frame(lapply(df1, normalize))

data_norm$CSpam <- factor(df1$CSpam)

data_norm$CSpam=NULL

km <- kmeans(data_norm, 2)

conf2 <- table(df1$CSpam, km$cluster)

conf2

acc2 <- sum(diag(conf2)) / sum(conf2)
acc2

# Compute and plot the importance values.
# install.packages('randomForest')

library(randomForest)
set.seed(4543)

data.rf <- randomForest(X1 ~ ., data=df1, ntree=1000,
                         keep.forest=FALSE, importance=TRUE)
# 6. Compute and plot the importance values.
# Dotchart of variable importance as measured by a Random Forest
varImpPlot(data.rf)
# 7. What are the most three important variables ?
df.importance <- importance(data.rf)

df.importance1 = data.frame(df.importance)

df.importance1

# %IncMSE is the most robust and informative measure. 
# the higher number, the more important

# IncNodePurity relates to the loss function 
# which by best splits are chosen. The loss function 
# is mse for regression and gini-impurity for classification. 
# More useful variables achieve higher increases in node purities, that is to find a split which has a high inter node 'variance' and a small intra node 'variance'. 
# IncNodePurity is biased and should only be used if the extra computation time of calculating %IncMSE is unacceptable.
