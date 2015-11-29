

#data jam project
#assignment from digital rand
#task:tweets classification
#Authors, Abinah and Derrick

#set working directory
setwd("E:/datajam")

#load library
library(tm)
library(ggplot2)
library(wordcloud)
library(topicmodels)

#read the data
data=read.csv("E:/datajam/data-sample.csv",header=TRUE)
#subset the data we only need the text column which is the 4th column
data=data[,c(1,2,3,4)]
data=data[,4]
#convert it into a dataframe
data=as.data.frame(data)
dim(data)

#load corpus

corpus <-Corpus(VectorSource(data))
summary(corpus)
inspect(corpus)
# clean corpus to remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeNumPunct))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c("aaa"," aaaa"," aaaand"," aaahhh"," aaas","aac","aag","aalborg","aap","aapl","aar","aarch","aardvark")
)
corpus <- tm_map(corpus, PlainTextDocument) 
# create term document matrix 
dtm<-DocumentTermMatrix(corpus,control=list(removePunctuation=removePunctuation, stopwords=stopwords ('SMART'), tolower=tolower))
dtm <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum

#DATA EXPLORATION
# inspect frequent words

#or u can also view the top n terms by..findFreqTerms(dtm, lowfreq=50)  
# this is a better way of getting the frequent terms

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 


# plot a gplot of the top 250 terms(those that appera more than 250 times)
p <- ggplot(subset(wf, freq>200), aes(word, freq,fill=word))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  

#TERM CORELATIONS
#what words are highly associated with one of the frequent term ie women
findAssocs(dtm, "women", 0.55)
#WORD CLOUD

#wordcloud(ploting a word cloud)
#plot a word cloud of the 100 most frequent words
set.seed(142)
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")  
par(mar = rep(4, 4))
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)    



#clustering.
#cluster by term similarity
# cluster terms(Hierarchal Clustering)
library(cluster) 
x=as.data.frame(dtms)
d <- dist((dtms))   
fit <- hclust(d)   
fit   


#KMEANS

m3 <- as.data.frame(as.matrix(t(dtms))) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 30 # number of clusters
kmeansResult <- kmeans(m3, k)
s=as.data.frame(split(data, kmeansResult$cluster))
cluster1=View(s$'1')
cluster2=View(s$'2')
cluster3=View(s$'3')
cluster4=View(s$'4')
cluster5=View(s$'5')
cluster6=View(s$'6')
cluster7=View(s$'7')
cluster8=View(s$'8')
plotcluster(data, kmeansResult$cluster)
#print the tweets for each cluster
# print(tweets[which(kmeansResult$cluster==i)])

final = data.frame()
for (i in 1:k) {
    cat(paste("cluster ", i, "$ ", sep=""))
    s <- sort(kmeansResult$centers[i], decreasing=T)
    cat(names(s)[1:6], "/n")
    
    t = as.data.frame(kmeansResult$cluster)
    colnames(t) = c('cluster')
    # print the tweets of every cluster + # 
    
    sub = subset(t,t$cluster == i)
    final = rbind(sub,final)
    #final = data[which(t$cluster==i),]
}
t1=(round(kmeansResult$centers, digits=3 ))
#coming up with the topics
#library(topicmodels)
dtm <- as.DocumentTermMatrix(dtm)
lda <- LDA(dtm, k = 30) # find 8 topics
x=as.data.frame(term <- terms(lda, 20)) # first 12 terms of every topiplot(x)
ggplotLDAPrep <- function(x){
    if (!is.null(Terms <- x$terms)) {
        data <- model.frame(x)
        X <- model.matrix(delete.response(Terms), data)
        g <- model.response(data)
        xint <- match("(Intercept)", colnames(X), nomatch = 0L)
        if (xint > 0L) 
            X <- X[, -xint, drop = FALSE]
    }
    means <- colMeans(x$means)
    X <- scale(X, center = means, scale = FALSE) %*% x$scaling
    rtrn <- as.data.frame(cbind(X,labels=as.character(g)))
    rtrn <- data.frame(X,labels=as.character(g))
    return(rtrn)
}

fitGraph <- ggplotLDAPrep(x)
x
