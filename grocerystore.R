#read data
setwd("E:/Semester 6/Pengantar Data Mining/Tugas Proyek")

data <- read.csv("grocerystore.csv", header=F)

#melihat data
View(data)
str(data)

#one-hot encoding
library(qdapTools)
data <- mtabulate(strsplit(data$V1 , ","))
for (i in names(data)){
  data[,i]<-as.factor(data[,i])
}

summary(data)
dim(data)

View(data)

#membuat association rules
library(arules)
rules.all <- apriori(data)
rules.all

#association rules dengan parameter
rules <- apriori(data, control = list(verbose=F),
                 parameter = list(minlen=2,maxlen=3,supp=0.3, conf=0.8))
rules
quality(rules) <- round(quality(rules), digits=3)

#pruned redundancy
redundant <- is.redundant(rules)
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned

#mengurutkan berdasarkan support
rules.sorted <- sort(rules.pruned, by="support")

#menampilkan rules
inspect(rules.sorted)

#visualisasi rules
library(arulesViz)
plot(rules.sorted)