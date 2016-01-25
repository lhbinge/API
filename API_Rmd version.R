
##=====================##
## READING IN THE DATA ##
##=====================##
suppressMessages(library(zoo))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(stargazer))
suppressMessages(library(micEcon))
suppressMessages(library(quantreg))
suppressMessages(library(McSpatial))
suppressMessages(library(quantmod))
suppressMessages(library(xtable))

setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\PhD Proposal Readings\\Art Price Index\\R Code")
artdata <- read.csv("Auction database.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE, 
                    colClasses=c("character","numeric","numeric","numeric","numeric","factor","factor","factor","character",
                                 "factor","factor","factor","character","factor","factor","factor","numeric","character",
                                 "numeric","numeric","numeric","numeric","numeric","numeric"))
##===================##
## CLEANING THE DATA ##
##===================##

artdata$date <- as.Date(artdata$date)
artdata$med_code <- factor(artdata$med_code, labels=c("Drawing", "Watercolour", "Oil", "Acrylic", "Print/Woodcut",
                                                      "Mixed Media","Sculpture","Photography", "Other"))
artdata$ah_code <- factor(artdata$ah_code, labels=c("5th Avenue","Ashbeys","Bernardi","Bonhams","Russell Kaplan",
                                                    "Stephan Welz","Strauss","Christies"))
artdata$timedummy <- factor(as.yearqtr(artdata$date, "%Y-%m-%d"))
artdata$lnprice <- log(artdata$price)
artdata$lnarea <- log(artdata$area)
artdata$lnarea2 <- artdata$lnarea*artdata$lnarea
artdata$lnsculpt_area <- ifelse(artdata$med_code=="Sculpture", artdata$lnarea, 0)
artdata$counter <- as.numeric(artdata$timedummy)

##----------------------
##Rank Artists by Volume
##----------------------
#Rank by Total Volume (all)
rankings <- count(artdata, artist)
rankings$rank_all <- dense_rank(desc(rankings$n))    #rank by density, with no gaps (ties = equal)
rankings$rank_total <- row_number(desc(rankings$n))  #equivalent to rank(ties.method = "first")
rankings$n <- NULL

artdata <- merge(artdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)

artplot <- aggregate(artdata$hammer_price, by=list(artdata$year,artdata$ah_code), FUN = sum, na.rm=TRUE)
g <- ggplot(artplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Auction House")
g <- g + ylab("Turnover (Sum of Hammer Price)")
g <- g + xlab("Date")
g

artplot1 <- aggregate(artdata$hammer_price, by=list(artdata$year), length)
artplot2 <- aggregate(artdata$hammer_price, by=list(artdata$year), FUN = median, na.rm=TRUE)
artplot <- merge(artplot1, artplot2, by="Group.1",all.x=TRUE)
names(artplot) <- c("Date","Total Sales","Median Price")
artplot <- melt(artplot, id="Date") 
g <- ggplot(artplot, aes(x=Date,value,colour=variable,fill=variable))
g <- g + geom_bar(subset=.(variable=="Total Sales"),stat="identity")
g <- g + geom_line(subset=.(variable=="Median Price"),size=1)
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g

summaryfunction <- function(x) {
    if( is.numeric(x)!=TRUE) {stop("Supplied X is not numeric")}
    mysummary = data.frame("Min." =as.numeric(min(x,na.rm=TRUE)),"1st Qu." = quantile(x,na.rm=TRUE)[2],
                           "Median" = median(x,na.rm=TRUE),"Mean" = mean(x,na.rm=TRUE),"3rd Qu." = quantile(x,na.rm=TRUE)[4],
                           "Max." = max(x,na.rm=TRUE),row.names="")
    names(mysummary) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
    return(mysummary)
}
xt <- xtable(summaryfunction(artdata$hammer_price), caption="Descriptive statistics of auction hammer prices")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


##------------------------------------------##
##---ARTIST REPUTATION VARIABLE (Kraussl)---##
##------------------------------------------##
modeldata <- subset(artdata, artdata$rank_all<101)
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                    "nr_works","artist","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
model_100 <- lm(expl_vars, data=modeldata)

#Second step: betaj coefficients are plugged into equation for every artist pair (base & another) 
rep <- list()
rep[[1]] <- 1
for(i in 2:(max(artdata$rank_total))) {
    list_vars <- c(list_expl_vars,"price")
    
    #geometric mean of paintings by artist y
    y <- subset(artdata[,list_vars], artdata$rank_total==1)
    y <- y[!rowSums(is.na(y)), ]
    py <-  exp(mean(log(y$price)))  
    ym1 <- subset(artdata[,list_vars], artdata$rank_total==i)
    ym1 <- ym1[!rowSums(is.na(ym1)), ]
    pym1 <-  exp(mean(log(ym1$price)))
    sbx <- 0
    
    #average of characteristics time implicit attribute price
    xy <- mean(y$lnarea)
    xym1 <- mean(ym1$lnarea)   
    b <- summary(model_100)$coefficients[grepl("lnarea", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xym1-xy)   
    sbx <- sbx + bx
    
    xy <- mean(y$lnsculpt_area)
    xym1 <- mean(ym1$lnsculpt_area)   
    b <- summary(model_100)$coefficients[grepl("lnsculpt_area", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xym1-xy)   
    sbx <- sbx + bx
    
    xy <- mean(y$nr_works)
    xym1 <- mean(ym1$nr_works)   
    b <- summary(model_100)$coefficients[grepl("nr_works", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xym1-xy)   
    sbx <- sbx + bx
    
    xy <- mean(as.numeric(y$dum_signed)-1)
    xym1 <- mean(as.numeric(ym1$dum_signed)-1)   
    b <- summary(model_100)$coefficients[grepl("dum_signed", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xym1-xy)   
    sbx <- sbx + bx
    
    xy <- mean(as.numeric(y$dum_dated)-1)
    xym1 <- mean(as.numeric(ym1$dum_dated)-1)   
    b <- summary(model_100)$coefficients[grepl("dum_dated", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xym1-xy)   
    sbx <- sbx + bx
    
    auc_house <- c("Ashbeys","Bernardi","Bonhams","Russell Kaplan","Stephan Welz","Strauss","Christies")  
    for(j in auc_house) {
        xy <- mean(as.numeric(y$ah_code==j))
        xym1 <- mean(as.numeric(ym1$ah_code==j))  
        b <- summary(model_100)$coefficients[grepl(j, rownames(summary(model_100)$coefficients)),1]
        bx <- b*(xym1-xy)   
        sbx <- sbx + bx
    }
    
    medium <- c("Watercolour","Oil","Acrylic","Print/Woodcut","Mixed Media","Sculpture","Photography","Other")  
    for(k in medium) {
        xy <- mean(as.numeric(y$med_code==k))
        xym1 <- mean(as.numeric(ym1$med_code==k))   
        b <- summary(model_100)$coefficients[grepl(k, rownames(summary(model_100)$coefficients)),1]
        bx <- b*(xym1-xy)   
        sbx <- sbx + bx
    }
    rep[i] <- (pym1/py)/exp(sbx)
}

for(i in 1:(max(artdata$rank_total))) { 
    artdata$reputation[(artdata$rank_total==i)] <- rep[i]
}

artdata$reputation <- as.numeric(unlist(artdata$reputation))
artdata$lnrep <- log(artdata$reputation)

#------------------------------------------------------------------------------------------------------------------------------
#Load pre-calculated dataset (for speed) from API_3.R
artdata <- read.csv("artdata_lnrep.csv", header=TRUE)

#The result: index of average price per artist adjusted for quality, relative to the base artist 
#It can replace the artist dummies as a continuous variable in a second regression of equation 1 
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","lnrep","timedummy")

source("full_model.R")
rep_results <- full_model(artdata,list_expl_vars)

source("overlap1y_model.R")
suppressMessages(rep_overlap1 <- overlap1y_model(artdata,list_expl_vars))

source("overlap2y_model.R")
suppressMessages(rep_overlap2 <- overlap2y_model(artdata,list_expl_vars))

source("rolling_model.R")
suppressMessages(rep_rolling <- rolling_model(artdata,list_expl_vars))

list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","lnrep","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+"))) 
modeldata <- artdata 
model_all <- lm(expl_vars, data=modeldata) 
stargazer(model_all, title = "Hedonic Regression results", omit=c("timedummy"), omit.labels = "Quarterly dummies", header=FALSE, single.row = TRUE, type = "latex")


index_plot <- melt(rep_overlap2[,c(-2,-12)], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

hedonic_indices <- rep_rolling[,c(1,2)]
colnames(hedonic_indices) <- c("Date","Full (Rep)")
hedonic_indices <- cbind(hedonic_indices,Adjacent_1y=rep_overlap1[,19])
hedonic_indices <- cbind(hedonic_indices,Adjacent_2y=rep_overlap2[,11])
hedonic_indices <- cbind(hedonic_indices,Rolling=rep_rolling[,15])

index_plot <- melt(hedonic_indices, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g



allDup <- function(value) {
duplicated(value) | duplicated(value, fromLast = TRUE)
}
rsartdata <- artdata[allDup(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated")]),]
rsartdata <- transform(rsartdata, id = as.numeric(interaction(artist,factor(title),med_code,factor(area),factor(dum_signed),
factor(dum_dated), drop=TRUE)))

repdata <- repsaledata(rsartdata$lnprice,rsartdata$counter,rsartdata$id)
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=2,
graph=FALSE)
repeatsales_index <- exp(as.data.frame(repeatsales$pindex))*100

##------------------------------------------------------------------
##--------------- Pseudo Repeat Sales ------------------------------
##------------------------------------------------------------------
#The distance metric between any two sales (across the intervening time period) is the 
#absolute value of the difference between the two predicted hedonic log values. 
#The threshold for this distance metric can be customised. 
#At one extreme, one can choose to select only one pair with the smallest value of the distance metric. 

#match per artist by hedonic function
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
"nr_works","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))

ps.RS <- function(threshold) {
teller <-0
repdata <- data.frame()
rartdata <- subset(artdata,artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
keep <- c("lnprice","artist","title","lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
"nr_works","timedummy","counter")
rartdata <- rartdata[,names(rartdata) %in% keep]
rartdata <- rartdata[complete.cases(rartdata),]  

## Rank total again for new sample
rankings <- count(rartdata, artist)
rankings$rank_total <- row_number(desc(rankings$n))
rartdata <- merge(rartdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)

for(k in 1:max(rartdata$rank_total)) { #max(rartdata$rank_total)
modeldata <- subset(rartdata, rartdata$rank_total==k)
modeldata$med_code <- factor(modeldata$med_code)
modeldata$ah_code <- factor(modeldata$ah_code)
modeldata$dum_signed <- factor(modeldata$dum_signed)
modeldata$dum_dated <- factor(modeldata$dum_dated)
modeldata$timedummy <- factor(modeldata$timedummy)

#modeldata <- cbind(modeldata,model.matrix(~modeldata$med_code))
if(length(levels(modeldata$med_code))==1)   { modeldata$med_code <- as.numeric(0) }
if(length(levels(modeldata$ah_code))==1)    { modeldata$ah_code <- as.numeric(0) }
if(length(levels(modeldata$dum_signed))==1) { modeldata$dum_signed <- as.numeric(0) }
if(length(levels(modeldata$dum_dated))==1)  { modeldata$dum_dated <- as.numeric(0)   }

if(length(levels(modeldata$timedummy))!=1) { #modeldata$timedummy <- as.numeric(0)   
model <- lm(expl_vars, data=modeldata, na.action = na.exclude)
newdata <- modeldata
newdata$timedummy <- newdata$timedummy[1]
modeldata <- cbind(modeldata,fitted=predict.lm(model,newdata=newdata))

modeldata$id <- 0
for(i in 1:nrow(modeldata)) {
teller <- teller + 1
if(modeldata$id[i]==0) {
modeldata$id[i] <- teller
medium <- modeldata$med_code[i]
modeldata$distance <- abs(modeldata$fitted[i]-modeldata$fitted)/modeldata$fitted[i]
if(threshold=="nearest"){
modeldata$id[(modeldata$distance==min(modeldata$distance,na.rm=TRUE) & modeldata[,"id"]==0 & modeldata[,"med_code"]==medium)] <- teller
} else {
modeldata$id[(modeldata$distance<threshold & modeldata[,"id"]==0 & modeldata[,"med_code"]==medium)] <- teller
} 
}
}
repdata <- rbind(repdata,modeldata)
}
}

fullrep <- cbind(repsaledata(repdata$lnprice,repdata$counter,repdata$id),
repsaledata(repdata$lnarea,repdata$counter,repdata$id)[,4:5],
repsaledata(repdata$med_code,repdata$counter,repdata$id)[,4:5],
repsaledata(repdata$ah_code,repdata$counter,repdata$id)[,4:5],
repsaledata(repdata$lnsculpt_area,repdata$counter,repdata$id)[,4:5],
repsaledata(repdata$dum_signed,repdata$counter,repdata$id)[,4:5],
repsaledata(repdata$dum_dated,repdata$counter,repdata$id)[,4:5],
repsaledata(repdata$nr_works,repdata$counter,repdata$id)[,4:5])

colnames(fullrep) <- c("id","time0","time1","price0","price1","area0","area1","med_code0","med_code1",
"ah_code0","ah_code1","sculpt0","sculpt1","sign0","sign1","date0","date1",
"nr0","nr1")

repeatsales <- repsale(fullrep$price0,fullrep$time0,fullrep$price1,fullrep$time1,mergefirst=2,
graph=FALSE)
sps.RS_index <- exp(as.data.frame(repeatsales$pindex))*100

dy <- fullrep$price1 - fullrep$price0
timevar <- levels(factor(c(fullrep$time0, fullrep$time1)))
nt = length(timevar)
n = length(dy)
xmat <- array(0, dim = c(n, nt - 1))
for (j in seq(1 + 1, nt)) {
xmat[, j - 1] <- ifelse(fullrep$time1 == timevar[j], 1, xmat[, j - 1])
xmat[, j - 1] <- ifelse(fullrep$time0 == timevar[j],-1, xmat[, j - 1])
}
colnames(xmat) <- paste("Time", seq(1 + 1, nt))
fit <- lm(dy ~ xmat + 0)

fullrep$med_code0[is.na(fullrep$med_code0)] <- "Oil"
fullrep$med_code1[is.na(fullrep$med_code1)] <- "Oil"
fullrep$ah_code0[is.na(fullrep$ah_code0)] <- "Strauss"
fullrep$ah_code1[is.na(fullrep$ah_code1)] <- "Strauss"

darea <- fullrep$area1 - fullrep$area0

med0 <- model.matrix(~fullrep$med_code0)
med1 <- model.matrix(~fullrep$med_code1)
dmed <- med1 - med0

ah0 <- model.matrix(~fullrep$ah_code0)
ah1 <- model.matrix(~fullrep$ah_code1)
dah <- ah1 - ah0

dsculpt <- fullrep$sculpt1 - fullrep$sculpt0
dnr <- fullrep$nr1 - fullrep$nr0

sign0 <- model.matrix(~fullrep$sign0)
sign1 <- model.matrix(~fullrep$sign1)
dsign <- sign1 - sign0

date0 <- model.matrix(~fullrep$date0)
date1 <- model.matrix(~fullrep$date1)
ddate <- date1 - date0

#ps.RS <- lm(dy ~ darea + dmed + dah + dsculpt + dnr + dsign + ddate + xmat + 0)
ps.RS <- lm(dy ~ darea + dah + dsculpt + dnr + dsign + ddate + xmat + 0)
ps.RS_results <- summary(ps.RS)$coefficients[grepl("Time", rownames(summary(ps.RS)$coefficients)),1]
ps.RS_results <- as.data.frame(ps.RS_results)
ps.RS_results$index_all <- exp(ps.RS_results$ps.RS_results)*100
if(threshold=="nearest"){
ps.RS_results <- cbind(ps.RS_results,sps.RS_index[c(3:62),])
} else {
ps.RS_results <- cbind(ps.RS_results,sps.RS_index[2:62,])
} 
ps.RS_results$pairs <- nrow(fullrep)

return(ps.RS_results)
}

ps.RS_1 <- ps.RS(0.0001)
ps.RS_2 <- ps.RS(0.00005)
ps.RS_n <- ps.RS("nearest")

rep_indices <- ps.RS_1[,c(2,3)] #time_results
rep_indices$Date <- c("2000Q2","2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
"2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
"2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
"2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
"2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
"2015Q1","2015Q2")

repeatsales_index$Date <- c("2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q4",
"2003Q1","2003Q2","2003Q4","2004Q1","2004Q2","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
"2006Q1","2006Q2","2006Q3","2006Q4","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
"2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
"2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
"2015Q1","2015Q2")
rep_indices <- merge(rep_indices, repeatsales_index, by="Date", all=TRUE)

rep_indices <- cbind(rep_indices,Full_ps.RS2=ps.RS_2[,2])

ps.RS_n$Date <- c("2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
"2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
"2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
"2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
"2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
"2015Q1","2015Q2")
rep_indices <- merge(rep_indices, ps.RS_n, by="Date", all=TRUE)

rep_indices <- rep_indices[,c(1,4,7,2,5)]
colnames(rep_indices) <- c("Date","Repeat Sales","ps.RS(nearest)","ps.RS(0.01%)","ps.RS(0.005%)")
index_plot <- melt(rep_indices, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g


#Comparison and Evaluation
##Compare the different indices

all_indices <- cbind(hedonic_indices[-1],rep_indices[,c(-1,-3)])
all_indices <- rbind(c(seq(100,100, length.out = ncol(rep_indices))),all_indices)
all_indices[2,5] <- 100

Dates <- as.factor(c("2000Q1","2000Q2","2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
                     "2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                     "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                     "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                     "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                     "2015Q1","2015Q2"))
all_indices$Date <- Dates

index_plot <- cbind(all_indices[,c(1,6,8)],naive_index$index)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g



for(i in 1:ncol(all_indices)) {all_indices[,i] <- as.numeric(all_indices[,i]) }
ts.all_indices <- as.ts(all_indices,start =c(2000,1),end=c(2015,2),frequency=4) 

#Check correlations (in levels and growth rates)
#cor(ts.all_indices, use ="complete.obs") 
dl.indices <- as.data.frame(diff(log(ts.all_indices)))
#cor(dl.indices, use ="complete.obs")
source("corstarsl.R")
xt <- xtable(corstarsl(ts.all_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))
xt <- xtable(corstarsl(dl.indices[c(-1:-3),]), caption="Correlations in DLogs")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


##Comparing index smoothness


##Compared to international art price indices (Artprice)

assets <- read.csv("Assets.csv", header=TRUE, na.strings = "", skipNul = TRUE)

index_plot <- cbind(rw_indices[,c(1,7)],Date)
index_plot <- cbind(index_plot,assets[,2:6])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

#Compared to other art pices

index_plot <- cbind(rw_indices[,c(1,7)],Date)
index_plot <- cbind(index_plot,assets[,7:9])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

#==============================#
# Bubbles: Explosive Behaviour
#==============================#

#test for whether we should include a drift or trend term
y <- all_indices[,5]
toets <- ur.df(y, type= "none", lags = 4, selectlags = c("AIC"))
toets <- ur.df(y, type= "drift", lags = 4, selectlags = c("AIC"))
toets <- ur.df(y, type= "trend", lags = 4, selectlags = c("AIC"))
summary(toets)
#indicate dat daar nie 'n drift of trend component hoef te wees nie.

#---------------------------------------------------------------------------------
#Get test statistics
y_indices <- all_indices[,-6]
bubble.nc <- list()
for(i in 1:ncol(y_indices)) {
    #bubble1 <- numeric()
    bubble <- numeric()
    for(j in 12:62) {
        y <- y_indices[1:j,i]
        #toets1 <- adf.test(y, alternative = "explosive", k=4)
        #bubble1 <- rbind(bubble1,toets1$statistic)
        toets <- ur.df(y, type= "none", lags = 4, selectlags = c("AIC"))
        #toets <- ur.df(y, type= "drift", lags = 4, selectlags = c("AIC"))
        #toets <- ur.df(y, type= "trend", lags = 4, selectlags = c("AIC"))
        bubble <- rbind(bubble,toets@teststat)
    }
    bubble.nc[[i]] <- bubble
}


##--------------------------------------------------------------------------
#Get critical values
K1 <- numeric()
K2 <- numeric()
K3 <- numeric()

for(j in 12:62) {
    
    set.seed(123)                           #for replicability
    reps <- 2000                            #Monte Carlo replications
    burn <- 50                              #burn in periods: first generate a T+B sample
    #To make "sure" that influence of initial values has faded
    #obs <- 62                              #ultimate sample size
    obs <- j
    
    tstat.nc <- numeric()
    tstat.c <- numeric()
    tstat.ct <- numeric()
    
    for(i in 1:reps) {     
        e <- rnorm(obs+burn)
        e[1] <- 0
        Y1 <- cumsum(e)
        DY1 <- diff(Y1)
        
        y1 <- Y1[(burn+1):(obs+burn)]               #trim off burn period
        dy1 <- DY1[(burn+1):(obs+burn)]             
        ly1 <- Y1[burn:(obs+burn-1)] 
        trend <- 1:obs
        
        EQ1 <- lm(dy1 ~ 0 + ly1)       
        tstat.nc <- rbind(tstat.nc,summary(EQ1)$coefficients[1,3]) 
        EQ2 <- lm(dy1 ~ ly1)            
        tstat.c <- rbind(tstat.c,summary(EQ2)$coefficients[2,3])  
        EQ3 <- lm(dy1 ~ lag(y1) + trend)    
        tstat.ct <- rbind(tstat.ct,summary(EQ3)$coefficients[2,3]) 
    }                                       
    
    #hist(tstat.nc)
    K1 <- rbind(K1,quantile(tstat.nc, probs=c(0.9,0.95,0.99)))
    K2 <- rbind(K2,quantile(tstat.c, probs=c(0.9,0.95,0.99)))
    K3 <- rbind(K3,quantile(tstat.ct, probs= c(0.9,0.95,0.99)))
}

#Dit gee 'n vector van critical values


##---------------------------------------------------------------------------
#plot die test stats en critical values 

bubble.test <- numeric()
for(k in 1:7) { bubble.test <- cbind(bubble.test,bubble.nc[[k]])}
bubble.test <- as.data.frame(bubble.test)
#colnames(bubble.test) <- colnames(y_indices)
bubble.test <- cbind(bubble.test,K1)
bubble.test$Date <- c("2002Q4","2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                      "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                      "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                      "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                      "2015Q1","2015Q2")

index_plot <- bubble.test[,c(1,8,9,10,11)]
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g

index_plot <- bubble.test[,c(6,8,9,10,11)]
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


#report die bubble period datums
datum <- data.frame()
datums <- data.frame()
for(i in 1:7) {
    for(l in 1:51) {
        if(bubble.test[l,i]>bubble.test$"95%") { 
            datum[l,i] <- bubble.test[l,"Date"]
        }
    }
    NonNAindex <- which(!is.na(datum[,i]))
    firstNonNA <- min(NonNAindex)
    datums[1,i] <- datum[firstNonNA,i]
    lastNonNA <- max(NonNAindex)
    datums[2,i] <- datum[lastNonNA,i]
}

colnames(datums) <- colnames(bubble.test[1:7])
rownames(datums) <- c("start","end")
datums <- t(datums)
xt <- xtable(datums, caption="Dates of explovive behaviour")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))
#table(datums)


