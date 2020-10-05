#Import:
data <- read.delim(file.choose(),quote = "", stringsAsFactors = FALSE)
  #READ AS STRING not factors

View(data)

#Packages:
install.packages('tm')
install.packages('SnowballC')
library('tm')
library('SnowballC')

#Bag of words:
#*Next 10-12 lines are the same for any dataset.
#*Corpus = collection of words
corpus =VCorpus(VectorSource(data$Review))
    #Choose a column to create corpus
corpus = tm_map(corpus, content_transformer(tolower)) #Makes it lowercase
  #tm_map(in a corpus, creates a "map" of words)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())#Stopwords (the,a,who,they...)
  #stopwords(x) <- includes "X as well as the pre-determined stopwords)
corpus = tm_map(corpus,stemDocument)
  #Stem = root of word. Loved = Love (stem). We want to make everything the root.
corpus = tm_map(corpus, stripWhitespace)

#Creating model for corpus:
  #* Creates a matrix that assigns a 1 to rows with 'love' in them.
dtm = DocumentTermMatrix(corpus) #converting into matrix
dtm = removeSparseTerms(dtm, 0.999) #all columns with only 0s and one 1 will go away.
dataset = as.data.frame(as.matrix(dtm))
View(dataset)
#^ Every column is a word found in the corpus

#Adding the "liked" column into the dataset:
dataset$Liked <- data$Liked
# *You might need to encode the liked column!
dataset$Liked<-factor(dataset$Liked, levels = 0:1)

#Split and Test the data:
library(caTools)
split = sample.split(dataset$Liked, SplitRatio = 0.8)

training_set = subset(dataset, split==TRUE) #80%
test_set = subset(dataset, split == FALSE) #20%

#everything above this serves as a template

#logistic!
classifier = glm(formula = Liked ~., #.~ for all other variables
                 family = binomial, data = training_set) #glm: Generalized linear models, binomial: 0/1

#probability of buying
prob = predict(classifier, type = 'response', test_set[,-692])
#I want to take the 4th column away so it can predict without knowing the answer
y_predict = ifelse(prob>0.5,1,0)

#confusion matrix
con_matrix = table(y_predict, test_set[,692])

con_matrix


#SVM
library(e1071)
classifier = svm(formula = Liked ~., data = training_set,
                 type = 'C-classification', kernel = 'linear')
#C-classification is when the response variable is a factor

y_predict = predict(classifier, test_set[,-692])

#confusion matrix
con_matrix2 = table(y_predict, test_set[,692])
con_matrix2
#accuracy of the model = (82+75)/(82+75+25+18) = 78.5%

####NETFLIX DATA#####
data<- read.csv(file.choose(), header = T)
View(data)

library(dplyr)
recommend <- select(data,listed_in.1, netflix.recommended, duration)

#checking for missing values
library(questionr)
freq.na(recommend)

#removing seasons
recommend <- filter(recommend, duration !=1)

str(recommend)

#changing the structure of netflix.recommend and listed_in.1
recommend$netflix.recommended <- as.factor(recommend$netflix.recommended)
recommend$listed_in.1 <- factor(recommend$listed_in.1, labels = 1:33)

freq.na(recommend)
str(recommend)
library(caTools)

set.seed(32) 

split = sample.split(recommend$netflix.recommended, SplitRatio = 0.8)

training_set = subset(recommend, split==TRUE) #80%
test_set = subset(recommend, split == FALSE)

library(e1071)
classifier = svm(formula = netflix.recommended ~., 
                 data = training_set,
                 type = "C-classification", kernel= 'linear')

y_predict = predict(classifier,newdata = test_set[,-2])
#C-Classification is when the response varible is a factor!  

#Confusion Matrix
con_matrix3 <- table(y_predict, test_set[,2])
con_matrix3
