#Natural Language Processing

#Importing the Dataset
dataset_original=read.delim('Restaurant_Reviews.tsv', quote='',stringsAsFactors = FALSE)

#cleaning the texts
#install.packages('tm)
library(tm)
Corpus= VCorpus(VectorSource(dataset$Review))
Corpus= tm_map(Corpus, content_transformer(tolower))
Corpus= tm_map(Corpus, removeNumbers)
Corpus= tm_map(Corpus, removePunctuation)
Corpus= tm_map(Corpus, removeWords, stopwords())
Corpus= tm_map(Corpus, stemDocument)
Corpus= tm_map(Corpus, stripWhitespace)

#creating bag full of words
dtm=DocumentTermMatrix(Corpus)
dtm= removeSparseTerms(dtm,0.98)
dataset= as.data.frame(as.matrix(dtm))


#factoring the target feature
dataset$Purchased=factor(dataset$Purchased, levels = c(0,1))


#diving the dataset
library(caTools)
set.seed(1234)
split=sample.split(dataset$liked,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#feature scaling
training_set[-3]=scale(training_set[-3])
test_set[-3]=scale(test_set[-3])

#classifier
library(randomForest)
classifier=randomForest(x=training_set[-3],
                        y=training_set$liked)

#predicting the results
y_pred=predict(classifier, newdata= test_set[-34] )

#table
cm=table(test_set[ ,34],y_pred)
