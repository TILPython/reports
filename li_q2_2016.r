# fully operational==To create monthly sentiment analsysis and management report
#to connect to sqlite database 06072016
#added proces to dtm and inspect terms on 11072016; completeed two pages on 04082016
#automated the gsub process to corpus tm_map
library(tm)
library(RWeka)
library(igraph)
library(ggplot2)
library(RColorBrewer)
library("RSQLite")
con <- dbConnect(drv=RSQLite::SQLite(), dbname="f:/python27/dbase/li_q2_2016.db")
ibdict<-read.csv("f:/r/data/li_dictionary.txt", stringsAsFactors = FALSE)
colnames(ibdict)<-c("pattern","replacement")
nibdict<-nrow(ibdict)
#keep the tables required
intable <- tables[tables = "banks"]

query="select story_id,fulltext from banks"
rs=dbSendQuery(con,query)
mydf=fetch(rs,n=-1)
dbDisconnect(con)
myReader <- readTabular(mapping=list( id="story_id",content="fulltext"))
mycorpus<- VCorpus(DataframeSource(mydf), readerControl=list(reader=myReader))

summary(mycorpus) 
lilist<-c("iprulife","sbilife","bajaj","sbilife","hdfclife","reliance")
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) 

nli<-length(lilist)
ncorpus=length(mycorpus)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus<-tm_map(mycorpus, content_transformer(removeWords), stopwords("SMART"))
mycorpus<-tm_map(mycorpus, content_transformer(tolower))
mycorpus<- tm_map(mycorpus, stripWhitespace)
for (i in 1:nibdict) {
    mycorpus<-tm_map(mycorpus,content_transformer(function(x, pattern) {return (gsub(pattern, ibdict[i,2], x))}),ibdict[i,1])
  }

dtm <- DocumentTermMatrix(mycorpus, control = list(tokenize = TrigramTokenizer))
inspect(dtm[1:ncorpus, intersect(colnames(dtm), lilist)])
x.df<-as.matrix(dtm[1:ncorpus, intersect(colnames(dtm), lilist)])
#Page 1 of graphics

page1.out<-matrix(,nrow=ncol(x.df), ncol=4)
rownames(page1.out)<-colnames(x.df)
colnames(page1.out)<-c("Total mentions","Number of stories","Exclusive stories","Authored articles")
for (i in 1: ncol(x.df)){
item<-colnames(x.df)[i]
page1.out[i,1]<-colSums(x.df)[i] #total number of mentions
page1.out[i,2]<-sum(x.df[,i]!=0)# number of stories with item
not_item<-x.df[,colnames(x.df)!=item, drop=FALSE]
final<-x.df[(( x.df[,item]>0) & rowSums(not_item)==0),]
#exclusive story count
#if (nrow(final)!=0){page1.out[i,3]<-sum(final[item]!=0)} else{page1.out[i,3]<-0}
if (nrow(final)!=0){page1.out[i,3]<-sum(final[,item]!=0)} else{page1.out[i,3]<-0}
}
#from http://stackoverflow.com/questions/20673584/visualizing-crosstab-tables-with-a-plot-in-r
#to chart
# Set up the vectors                           
x.axis <- rownames(page1.out)
y.axis <- colnames(page1.out)
df <- expand.grid(x.axis,y.axis)
df$value <- as.vector(page1.out)
df[df==0]<-NA
#this one works
#jpeg("f:/r/data/ipru1.jpg", width = 11, height = 6, units = 'in', res = 300)
g <- ggplot(df, aes(Var1, Var2)) + geom_point(aes(size = 0.90*value), colour = "green") + theme_bw() + xlab("") + ylab("")
g + scale_size_continuous(range=c(10,30)) + geom_text(aes(label = value)) + theme(legend.position="none") +scale_fill_brewer(palette="Set1")
dev.off()

#readline(prompt="Press [enter] to continue")
Sys.sleep(5)
dev.off()
#page 2 procesing
tdm <- TermDocumentMatrix(mycorpus, control = list(tokenize = TrigramTokenizer))
a3<-findAssocs(tdm,"avendus", corlimit=0.7)
a4<-names(a3$avendus)
tdm.mat<-as.matrix(tdm[intersect(rownames(tdm), a4),1:ncorpus])
tdm.mat[tdm.mat>=1] <- 1 # create boolean matrix
tdm.adj<-tdm.mat%*%t(tdm.mat) # create adjacency matrix
#++++++++++++++++++++++
#To create network graph with tdm, create matrix
# build adjacency graph with igraph 
oldpar<-par()
par(mar=c(0,0,0,0)+.1)
set.seed(1234)
g <- graph.adjacency(tdm.adj, weighted=TRUE, mode="undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)$color <- "green"
# change label size of vertices (nodes) V()
# change the edge color (connections) E()
V(g)$label.cex <- 0.8 * V(g)$degree / max(V(g)$degree) + .2
V(g)$label.color <- "black" 
V(g)$frame.color <- NA
egam <- (log(E(g)$weight) + .4) / max(log(E(g)$weight) + .4)
E(g)$weight <- edge.betweenness(g) 
plot(g,layout=layout_with_fr)	 
par<-oldpar

Sys.sleep(5)
dev.off()
#page 3 procesing
#3. Authored articles




oldpar<-par()
par(mai=c(0.50,2,1,0.50))
#4. Top ten newspapers with max stories, fixed 28102016
con <- dbConnect(drv=RSQLite::SQLite(), dbname="f:/python27/dbase/li_q2_2016.db")
#keep the tables required
intable <- tables[tables = "banks"]
query="select Publication, count(*) as frequency from banks group by Publication order by count(*) desc"
rs=dbSendQuery(con,query)
mydf=fetch(rs,n=-1)
mydf<-mydf[order(mydf$frequency),]
mydf<-tail(mydf,10)
freq<- as.numeric(as.character(mydf$frequency))
bp<-barplot(mydf$frequency,main="Top 10 newspapers with max. stories", col=brewer.pal(length(mydf$frequency),"Paired"),horiz=TRUE,names.arg=mydf$Publication,las=2, axes='F', xlim=c(0,max(freq)*1.1))
text(x=freq+0.03, y=bp, label=as.character(freq), pos=4,font=2)
# colour options are Paired, Accent, Pastel1, Pastel2, Pastel3, Set1, Set2, Set3 and Dark2
#5. Journalists with max stories==fixed 28102016
query="select Journalist, count(*) as frequency from banks where ((Journalist is not NULL) and (Journalist!=' ')) group by Journalist order by count(*) desc"
rs=dbSendQuery(con,query)
mydf=fetch(rs,n=-1)
mydf<-mydf[order(mydf$frequency),]
mydf<-tail(mydf,10)
freq<- as.numeric(as.character(mydf$frequency))
bp<-barplot(mydf$frequency,main="Top 10 journalists with max. stories", col=brewer.pal(length(mydf$frequency),"Paired"),horiz=TRUE,names.arg=mydf$Journalist,las=2, axes='F', xlim=c(0,max(freq)*1.1))
text(x=freq+0.05, y=bp, label=as.character(freq), pos=4,font=2)
#








#6. Top 20 key phrases/terms in media
#7. Table of context of top 20 key phrases=keytem_extraction_final.py
dbDisconnect(con)