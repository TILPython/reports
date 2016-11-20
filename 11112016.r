# fully operational==To create monthly sentiment analsysis and management report
#to connect to sqlite database 06072016
#added proces to dtm and inspect terms on 11072016; completeed two pages on 04082016
#automated the gsub process to corpus tm_map
library(tm)
library(RWeka)
library("RSQLite")
library(RcolorBrewer)
mystopwords<-scan("f:/r/data/stopwords.txt", what="character", sep=",")

full_dict<-read.csv("f:/r/data/full_li_list.txt", stringsAsFactors = FALSE)
colnames(full_dict)<-c("pattern","replacement")
nfulldict<-nrow(full_dict)
#keep the tables required
con <- dbConnect(drv=RSQLite::SQLite(), dbname="f:/python27/dbase/li_q2_2016.db")
intable <- tables[tables = "banks"]
query="select story_id,fulltext from banks"
rs=dbSendQuery(con,query)
mydf=fetch(rs,n=-1)
dbDisconnect(con)
myReader <- readTabular(mapping=list( id="story_id",content="fulltext"))
mycorpus<- VCorpus(DataframeSource(mydf), readerControl=list(reader=myReader))

#summary(mycorpus) 
comp_list<-c("iprulife","sbilife","bajajli","hdfclife","reliancelife")
full_list<-c("iprulife","sbilife","bajajli","hdfclife","reliancelife","aegonli","avivali","birlali","canarahsbcli",
	"dhflli","edelweissli","exideli","futuregenerali","idbifedli","indiafirstli","kotakli","maxlife","pnbmetlife","saharali",
	"shriramli","starunionli","tatalife","lic")
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) 

ncorpus=length(mycorpus)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus<-tm_map(mycorpus, content_transformer(removeWords), c(stopwords("SMART"),mystopwords))
mycorpus<-tm_map(mycorpus, content_transformer(tolower))
mycorpus<- tm_map(mycorpus, stripWhitespace)
for (i in 1:nfulldict) {
    mycorpus<-tm_map(mycorpus,content_transformer(function(x, pattern) {return (gsub(pattern, full_dict[i,2], x))}),full_dict[i,1])
  }

dtm <- DocumentTermMatrix(mycorpus, control = list(tokenize = TrigramTokenizer))
#inspect(dtm[1:ncorpus, intersect(colnames(dtm), comp_list)])
x.df<-as.matrix(dtm[1:ncorpus, intersect(colnames(dtm), comp_list)])
#===========================Page 1 of graphics

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
library(ggplot2)
#jpeg("f:/r/data/ipru1.jpg", width = 11, height = 6, units = 'in', res = 300)
#g <- ggplot(df, aes(Var1, Var2)) + geom_point(aes(size = 0.90*value), colour = "green") + theme_bw() + xlab("") + ylab("")
g <- ggplot(df, aes(Var1, Var2)) + geom_point(aes(size = 0.90*value),colour="green") + theme_bw() + xlab("") + ylab("") 
g + scale_size_continuous(range=c(10,30)) + geom_text(aes(label = value)) + theme(legend.position="none") + scale_fill_brewer(palette="Set1")
dev.off() # working perfect

#readline(prompt="Press [enter] to continue")
Sys.sleep(5)
dev.off()
#============================page 2 procesing
library(igraph)
library(RColorBrewer)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
tdm <- TermDocumentMatrix(mycorpus, control = list(tokenize = TrigramTokenizer))
a3<-findAssocs(tdm,"iprulife", corlimit=0.7)
a4<-names(a3$iprulife)
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
#=======================================page 3 procesing Authored articles

#=============================Page 4 Top ten newspapers with max stories, fixed 28102016
con <- dbConnect(drv=RSQLite::SQLite(), dbname="f:/python27/dbase/li_q2_2016.db")
library(RColorBrewer)
#keep the tables required
intable <- tables[tables = "banks"]
query="select Publication, count(*) as frequency from banks group by Publication order by count(*) desc"
rs=dbSendQuery(con,query)
mydf=fetch(rs,n=-1)
dbDisconnect(con)
mydf<-mydf[order(mydf$frequency),]
mydf<-tail(mydf,10)
freq<- as.numeric(as.character(mydf$frequency))
bp<-barplot(mydf$frequency,main="Top 10 newspapers with max. stories", col=brewer.pal(length(mydf$frequency),"Paired"),horiz=TRUE,names.arg=mydf$Publication,las=2, axes='F', xlim=c(0,max(freq)*1.1))
text(x=freq+0.03, y=bp, label=as.character(freq), pos=4,font=2)
# colour options are Paired, Accent, Pastel1, Pastel2, Pastel3, Set1, Set2, Set3 and Dark2
#5. Journalists with max stories==fixed 28102016
con <- dbConnect(drv=RSQLite::SQLite(), dbname="f:/python27/dbase/li_q2_2016.db")
query="select Journalist, count(*) as frequency from banks where ((Journalist is not NULL) and (Journalist!=' ')) group by Journalist order by count(*) desc"
rs=dbSendQuery(con,query)
mydf=fetch(rs,n=-1)
dbDisconnect(con)
mydf<-mydf[order(mydf$frequency),]
mydf<-tail(mydf,10)
freq<- as.numeric(as.character(mydf$frequency))
oldpar<-par()
par(mai=c(0.50,2,1,0.50))

bp<-barplot(mydf$frequency,main="Top 10 journalists with max. stories", col=brewer.pal(length(mydf$frequency),"Paired"),horiz=TRUE,names.arg=mydf$Journalist,las=2, axes='F', xlim=c(0,max(freq)*1.1))
text(x=freq+0.05, y=bp, label=as.character(freq), pos=4,font=2)
#revert to old parameters
par()<-oldpar

  
#================================Page 6 Top 10 key topics in media
library(topicmodels)
item<-"iprulife" # loop for each item
nx<- as.vector(dtm[,item]>0) #identify rows for whicj item>0, exists
dtm_item<-dtm[nx,] #subset the dtm on identified ids, now you have item-specific dtm
#Set parameters for Gibbs sampling
burnin <- 2000
iter <- 1000
thin <- 200
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 10
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
#write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
 #ALL THE TOPIC FOR THE ENTIRE INDUSTRY
#top 10 terms in each topic, either output directly to slide or make document map
ldaOut.terms <- as.data.frame(terms(ldaOut,10))


#======================================Page 7 Ego map, industry
# subset dtm for excl stories of company and each of its competitors
#run lda for 10 topics each
#output to arcdiagram

#ego map project
#starting with dtm, to remove sparse terms? to be done
#select documents with at least one of the competition listed

dtm.a<-dtm[,intersect(colnames(dtm),comp_list)]
dtm.mat<-as.matrix(dtm.a)
dtm.mat.b<-dtm.mat[rowSums(dtm.mat)!=0,]
ids.a<-as.numeric(rownames(dtm.mat.b)) #all the tow (doc ids) which have the competition listed
#with these doc ids, subset the main dtm, using rows=ids, and cols + lilist + unique terms in the  key terms
keyterms<-as.vector(unlist(unname(ldaOut.terms[,,drop=TRUE])))
col.list<-c(keyterms, comp_list) # now we have selected columns and selected rows; lets create a dtm
dtm.b<-dtm[intersect(rownames(dtm),ids.a),intersect(colnames(dtm),col.list)]


# ==================================================================
# Network between top characters
# ==================================================================
diag_mat<-as.matrix(t(dtm.b))
which_mfw<-comp_list
# Option 1
library(arcdiagram)
# matrix with most frequent terms
mft_mat = diag_mat[,which_mfw]
# first adjacency matrix
adj_mat1 = diag_mat %*% t(diag_mat)
# set zeros in diagonal
diag(adj_mat1) = 0
# create graph from adjacency matrix
graph_chars1 = graph.adjacency(adj_mat1, mode="undirected", weighted=TRUE, diag=FALSE)
# get edgelist 1
edges1 = get.edgelist(graph_chars1)

# Option 2
# transform mft_mat to a binary (ie boolean) matrix
bin_mat = mft_mat
bin_mat[mft_mat != 0] = 1
# second adjacency matrix using binary distances
adj_mat2 = dist(bin_mat, method="binary", diag=TRUE, upper=TRUE)
adj_mat2 = 1 - as.matrix(adj_mat2)
# set zeros in the diagonal
diag(adj_mat2) = 0
# create graph from adjacency matrix
graph_chars2 = graph.adjacency(adj_mat2, mode="undirected", weighted=TRUE, diag=FALSE)
# get edgelist 2
edges2 = get.edgelist(graph_chars2)


w1 = E(graph_chars1)$weight
#lwds = sqrt(w1/10)
lwds<-log(w1+1)
# arc colors based on graph-chars2
w2 = E(graph_chars2)$weight
#cols = ifelse(w2>median(w2),hsv(h=0, s=w2/max(w2), v=0, alpha=0.5*w2/max(w2)),0)
cols = hsv(h=w2/max(w2), s=1, v=1, alpha=0.6*w2/max(w2))

# The width of the arcs connecting two characters reflect the number 
# of words in common. In turn, the color of the arcs reflect the 
# association between two characters depending on the similarity 
# of the words in their dialogues.
# arc-diagram
arcDiagram(edges1, lwd=lwds, col=cols, cex=0.8, mar=c(7,1,4,1))
title("Preliminary arc-diagram", cex.main=0.9)


.d<-function{# ==================================================================
# Arc-diagram with pie charts
# ==================================================================

# now let's add some pie-charts in each node to reflect the
# dialogues participation proportion of each character by episode

# arc-diagram with pie charts on nodes
# color bands (of episodes VI, V, IV)
#col.pies = c("#C7F464", "#4ECDC4", "#556270")
col.pies = c("#C7F464", "#4ECDC4", "#6d849c")
pies<-as.data.frame(page1.out[,2])
# arc-diagram with pie charts
arcPies(edges1, pies, cex=1, col.pies=col.pies, lwd=lwds, col=cols, mar=c(7,1,4,1))
title("Arc-diagram with pie charts", cex.main=0.9)
legend(x=0.9, y=0.5, title="Episodes", text.col="gray65", cex=0.8,
       legend=c("IV","V","VI"), pch=19, col=col.pies, bty="n")
}
# ==================================================================
# Arc-diagram with bands: this is the one
# ==================================================================
arcBands <- function(
  edgelist, bands, col.bands=NULL,
  sorted=TRUE, decreasing=FALSE, lwd=NULL,
  col=NULL, cex=NULL, col.nodes="black", lend=1, ljoin=2, lmitre=1,
  las=2, bg=NULL, mar=c(4,1,3,1))
{
  # ARGUMENTS
  # edgelist:   two-column matrix with edges
  # bands:      numeric matrix with rows=nodes and columns=numbers
  # sorted:     logical to indicate if nodes should be sorted
  # decreasing: logical to indicate type of sorting (used only when sorted=TRUE)
  # lwd:        widths for the arcs (default 1)
  # col:        color for the arcs (default "gray50")
  # cex:        magnification of the nodes labels (default 1)
  # col.nodes:  color of the nodes labels (default "gray50")
  # lend:       the line end style for the arcs (see par)
  # ljoin:      the line join style for the arcs (see par)
  # lmitre:     the line mitre limit fort the arcs (see par)
  # las:        numeric in {0,1,2,3}; the style of axis labels (see par)
  # bg:         background color (default "white")
  # mar:        numeric vector for margins (see par)
  
  # make sure edgelist is a two-col matrix
  if (!is.matrix(edgelist) || ncol(edgelist)!=2)
    stop("argument 'edgelist' must be a two column matrix")
  edges = edgelist
  # how many edges
  ne = nrow(edges)
  # get nodes
  nodes = unique(as.vector(edges))
  nums = seq_along(nodes)
  # how many nodes
  nn = length(nodes)  
  # ennumerate
  if (sorted) {
    nodes = sort(nodes, decreasing=decreasing)
    nums = order(nodes, decreasing=decreasing)
  }
  # make sure bands is correct
  if (!is.matrix(bands) && !is.data.frame(bands))
    stop("argument 'bands' must be a numeric matrix or data frame")
  if (is.data.frame(bands))
    bands = as.matrix(bands)
  if (nrow(bands) != nn)
    stop("number of rows in 'bands' is different from number of nodes")
  
  # check default argument values
  if (is.null(lwd)) lwd = rep(1, ne)
  if (length(lwd) != ne) lwd = rep(lwd, length=ne)
  if (is.null(col)) col = rep("gray50", ne)
  if (length(col) != ne) col = rep(col, length=ne)
  if (is.null(col.nodes)) col.nodes = rep("gray50", nn)
  if (length(col.nodes) != nn) col.nodes = rep(col.nodes, length=nn)
  if (!is.null(cex) && length(cex) != nn) cex = rep(cex, length=nn)
  if (is.null(bg)) bg = "white"
  
  # nodes frequency from bands
  nf = rowSums(bands) / sum(bands)
  # words center coordinates
  fin = cumsum(nf)
  ini = c(0, cumsum(nf)[-nn])
  centers = (ini + fin) / 2
  names(centers) = nodes
  # node radiums
  nrads = nf / 2
  
  # arcs coordinates
  # matrix with numeric indices
  e_num = matrix(0, nrow(edges), ncol(edges))
  for (i in 1:nrow(edges))
  {
    e_num[i,1] = centers[which(nodes == edges[i,1])]
    e_num[i,2] = centers[which(nodes == edges[i,2])]
  }
  # max arc radius
  radios = abs(e_num[,1] - e_num[,2]) / 2
  max_radios = which(radios == max(radios))
  max_rad = unique(radios[max_radios] / 2)
  # arc locations
  locs = rowSums(e_num) / 2

  # function to get pie segments
  t2xy <- function(x1, y1, u, rad)
  {
    t2p <- pi * u + 0 * pi/180
    list(x2 = x1 + rad * cos(t2p), y2 = y1 + rad * sin(t2p))
  }
  
  # plot
  par(mar = mar, bg = bg)
  plot.new()
  plot.window(xlim=c(-0.025, 1.025), ylim=c(0, 1*max_rad*2))
  # plot connecting arcs
  z = seq(0, pi, l=100)
  for (i in 1:ne)
  {
    radio = radios[i]
    x = locs[i] + radio * cos(z)
    y = radio * sin(z)
    lines(x, y, col=col[i], lwd=lwd[i], 
          lend=lend, ljoin=ljoin, lmitre=lmitre)
  }
  # plot node bands
  for (i in 1:nn)
  {
    radius = nrads[i]
    p = c(0, cumsum(bands[i,] / sum(bands[i,])))
    dp = diff(p)
    np = length(dp)
    angle <- rep(45, length.out = np)
    for (k in 1:np)
    {
      n <- max(2, floor(200 * dp[k]))
      P <- t2xy(centers[i], 0, seq.int(p[k], p[k+1], length.out=n), rad=radius)
      polygon(c(P$x2, centers[i]), c(P$y2, 0), angle=angle[i], 
              border=NA, col=col.bands[k], lty=0)
    }
    # draw white circles
    theta = seq(0, pi, length=100)
    x3 = centers[i] + 0.7*nrads[i] * cos(theta)
    y3 = 0 + 0.7*nrads[i] * sin(theta)
    polygon(x3, y3, col=bg, border=bg, lty=1, lwd=2)    
  }
  # add node names
  if (is.null(cex)) {
    cex = nf
    cex[nf < 0.01] = 0.01
    cex = cex * 5
  }
  # add node names
  text(centers, 0, nodes, cex=cex, adj=c(0.5,0), col=col.nodes)
}
# arc-diagram with bands on nodes
col.bands = "#C7F464"
cols = hsv(h=w2/(max(w2)-min(w2)), s=1, v=1, alpha=0.5*w2/max(w2))

pies<-as.data.frame(log(page1.out[,2]))
# arc-diagram with bands
arcBands(edges1, pies, col.bands=col.bands, lwd=lwds, col=cols, mar=c(4,1,4,1))
title("Ego map, industry", cex.main=0.9)



#===============================Page 8 Top 20 terms associated with each entity
#for each entity loop
entity<-"bajajli"
col.list<-c(keyterms, entity)
dtm.80<-dtm[,intersect(colnames(dtm),col.list)]
keep.rows<-which(as.matrix(dtm.80[,entity])!=0)
dtm.81<-dtm.80[keep.rows,]# now has documenst which contain entity and full set of key terms
term.freq <- sort(colSums(as.matrix(dtm.81)),decreasing=TRUE)
term.freq<-head(term.freq,20)
#now again subset the dtm.b
dtm.82<- dtm.81[,intersect(colnames(dtm.81),names(term.freq))] 

#SNA graph 20112016
tdm.c<-as.matrix(t(dtm.82))
tdm.c[tdm.c>=1] <- 1
termMatrix <- tdm.c %*% t(tdm.c)
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g,mode="in")

#Plot the Graph
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
V(g)$label.cex <- 1.0 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- ifelse(V(g)$name==entity,"blue3","black")
V(g)$frame.color <- NA
V(g)$size=degree(g)*0.5 
V(g)$color<-ifelse(V(g)$name==entity,"skyblue","whitesmoke")
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$width <- egam*0.8
cut.off <- mean(E(g)$weight)
g.sp <- delete_edges(g, E(g)[weight<cut.off])
E(g)$color <- "gray80"
# plot the graph in layout1
plot(g.sp, layout=layout1)














