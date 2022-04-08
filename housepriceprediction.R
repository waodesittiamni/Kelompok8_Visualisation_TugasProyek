#read data
setwd("E:/Semester 6/Pengantar Data Mining/Tugas Proyek")

data <- read.csv("housepriceprediction.csv")
dim(data)

#melihat data
View(data)
str(data)

#deskrisi data
summary(data)

#melihat nama kolom
names(data)

#melihat values dari country
library(plyr)
count(data,vars = 'country')

##melihat values dari floors
count(data,vars = 'floors')

##melihat values dari bathrooms
count(data,vars = 'bathrooms')

#kolom date di drop
#karena hanya terdapat 1 value, maka country di drop
#kolom street dan statezip di drop karena telah terwakilkan di city
#kolom floors dan bathrooms di drop karena "jumlah lantai" dan "jumlah kamar mandi" mestinya bilangan bulat, tetapi pada data bukan bilangan bulat
#karena terdapat price yang bernilai 0, maka baris dengan price==0 di drop

data <- data[,!(names(data) %in% c('date','country','street','statezip','floors','bathrooms'))]
data <- data[!(data$price==0),]

##melihat values dari sqft_basement
head(count(data,vars = 'sqft_basement'))

#mayoritas data tidak memiliki basement, karena tidak memungkinkan membandingkan luas basement padahal jauh lebih banyak yang tidak memiliki basement, maka dilakukan recode
#menambahkan variabel basement dengan values 1=ada dan 0=tidak ada dengan basis sqft_basement (jika luas basement adalah 0 maka tidak memiliki basement)
data$basement <- ifelse(data$sqft_basement > 0, 1, 0)
barplot(count(data$basement)$freq, names.arg=count(data$basement)$x)

##melihat values dari yr_renovated
head(count(data,vars = 'yr_renovated'))

#mayoritas data tidak direnovasi, karena tidak memungkinkan membandingkan tahun renovasi padahal jauh lebih banyak yang tidak direnovasi, maka dilakukan recode
#menambahkan variabel renovated dengan values 1=pernah dan 0=tidak pernah dengan basis yr_renovated
data$renovated <- ifelse(data$yr_renovated == 0, 0, 1)
barplot(count(data$renovated)$freq, names.arg=count(data$renovated)$x)

#menambahkan variabel after1960 dengan values 1=setelah tahun 1960 dan 0=sebelum tahun 2000 dengan basis yr_built
data$after1960 <- ifelse(data$yr_built <1960, 0, 1)
barplot(count(data$after1960)$freq, names.arg=count(data$after1960)$x)

#korelasi terhadap variabel 'sqft_living','sqft_lot' dan 'sqft_above'
library(corrplot)
corrplot(cor(data[,c('sqft_living','sqft_lot','sqft_above')]),
         method = "color", type = "upper", tl.col = "black",tl.srt = 45, 
         addCoef.col = "black", diag = T, number.cex = 1.2)

#korelasi antara 'sqft_living' dan 'sqft_above' tinggi, hal ini berarti sqft_above telah terdeskripsi ke dalam sqft_living (berdasarkan penjelasan dataset pula telah dijelaskan)
#drop data yang telah direcode
data <- data[,!(names(data) %in% c('sqft_basement','yr_renovated','yr_built','sqft_above'))]

#karena terdapat data yang merupakan skala ordinal/nominal sebagai numeric dan character, maka diconvert menjadi factor
for (x in c('bedrooms','waterfront','view','condition','city','basement','renovated','after1960')){
  data[,x]<-as.factor(data[,x]) 
}

#melihat outlier
boxplot(data[,c('price','sqft_living','sqft_lot')])

#mengganti outlier dengan NA, kemudian nilai missing dihapus
for (x in c('price','sqft_living','sqft_lot')){
  value = data[,x][data[,x] %in% boxplot.stats(data[,x])$out]
  data[,x][data[,x] %in% value] = NA
}

#melihat banyaknya outlier yang digantikan NA
colSums(is.na(data))

#Removing the null values
library(tidyr)
data = drop_na(data)

colSums(is.na(data))

dim(data)

#melihat price
mean(data$price)
hist(data$price,xlab = "Price")
abline(v=mean(data$price),col='red')

#mengubah price menjadi factor dengan 1=mahal dan 0=murah dengan batas adalah nilai mean(price)
data$price <- ifelse(data$price > mean(data$price), 1, 0)
data$price<-as.factor(data$price)
barplot(count(data$price)$freq, names.arg=count(data$price)$x)

View(data)
names(data)
summary(data)

#membagi menjadi data latih dan data test dengan perbandingan 80:20
set.seed(123)

partisi <- sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
data_train <- data[partisi==1,]
data_testing <- data[partisi==2,]

#model C50
names(data_train)
library(C50)
modelc50 = C5.0(price~city+sqft_living, data_train)

#menampilkan summary model
summary(modelc50)

#menampilkan importance  model
C5imp(modelc50)

#visualisasi decision tree
plot(modelc50)

#prediksi terhadap data testing
pred_modelc50<- predict(modelc50,data_testing)

#melihat confusion matrix
library(caret)
confusionMatrix(pred_modelc50,data_testing$price)

#grafik ROC
library(ROCR)
roc.prediction = prediction(as.numeric(pred_modelc50), as.numeric(data_testing$price))
roc.tpr.fpr = performance(roc.prediction,"tpr","fpr")
roc.auc = performance(roc.prediction,"auc")
plot(roc.tpr.fpr, col="red",lty=3, main='ROC Graph')
abline(a=0, b= 1)
text(0.8, 0.1, labels="AUC : ")
text(0.8, 0.03, labels=roc.auc@y.values)