# Stats 202 Project

library(class)

data = read.csv('~/Documents/OneDrive/Year 2/summer/Stanford/Stats 202/project/training.csv',header = TRUE)
head(data)
attach(data)

sum(is.na(data))

par(mfrow=c(2,4))

hist(sig1)
hist(sig2)
hist(sig3)
hist(sig4)
hist(sig5)
hist(sig6)
hist(sig7)
hist(sig8)


# scale and clean the data 
data = data[,3:13]
SD = apply(data,2,sd)
attach(data)
sweep(data, 1, SD, `/`)
new[] <- lapply(names(data), function(x) x/sd(x) )


search = data[,1]/SD[1]
for (i in 2:ncol(data)-1){
  search = cbind(search,data[,i] / SD[i])
}

search = cbind(search,data[ncol(data)])

# we use 10 fold here
train.size = nrow(search) * 9/10
train = sample(1:nrow(search), train.size, replace = FALSE)
test = -train
data.train = search[train, ]
data.test = search[test, ]
relevance.test = relevance[test]


train.relevance = relevance[train]

pred_knn = knn(data.train,data.test,train.relevance, k = 1)

table(pred_knn,relevance.test)

mean(relevance[-train] == pred_knn)
