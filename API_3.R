##===============================================================================================##
## -------------------------------- ART PRICE INDEX ---------------------------------------------##
##===============================================================================================##

##=====================##
## READING IN THE DATA ##
##=====================##
library(zoo)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

#setwd("C:/Users/Laurie/OneDrive/Documents/BING/METRICS/PhD Proposal Readings/Art Price Index")
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\PhD Proposal Readings\\Art Price Index")

#library(rJava)
#library(xlsxjars)
#library(xlsx)
#artdata <- read.xlsx("Auction database.xlsx",sheetIndex=1,header=TRUE)

artdata <- read.csv("Auction database.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE, 
                    colClasses=c("character","numeric","numeric","numeric","numeric","factor","factor","factor","character",
                                 "factor","factor","factor","character","factor","factor","factor","numeric","character",
                                 "numeric","numeric","numeric","numeric","numeric","numeric"))


##===================##
## CLEANING THE DATA ##
##===================##


artdata$date <- as.Date(artdata$date)
artdata$med_code <- factor(artdata$med_code, labels=c("Drawing", "Watercolour", "Oil", "Acrylic", "Print/Woodcut",
                                                      "Mixed Media","Sculpture","Photograhy", "Other"))
artdata$ah_code <- factor(artdata$ah_code, labels=c("5th Avenue","Ashbeys","Bernardi","Bonhams","Russell Kaplan",
                                                    "Stephan Welz","Strauss","Christies"))


artdata$timedummy <- factor(as.yearqtr(artdata$date, "%Y-%m-%d"))

#artdata$timedummy <- factor(paste(artdata$year, artdata$quarter, sep="_")  
#dummies = model.matrix(~artdata$timedummy)
#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data

artdata$lnprice <- log(artdata$price)
artdata$lnarea <- log(artdata$area)
artdata$lnarea2 <- artdata$lnarea*artdata$lnarea

#artdata$lnsculpt_area <- 0
#artdata$lnsculpt_area[na.omit(artdata$med_code==7)] <- artdata$lnarea   #inteaction term: sculptures often only reported with 1 dimension (height)
artdata$lnsculpt_area <- ifelse(artdata$med_code=="Sculpture", artdata$lnarea, 0)

artdata$counter <- as.numeric(artdata$timedummy)

#source(themes)
#source(materials)

#change reference category with relevel()
#artdata$ah_code <- relevel(artdata$ah_code, ref = "Stephan Welz & Co")
#artdata$artist <- relevel(artdata$artist, ref = "Battiss, Walter Whall")
#artdata$med_code <- relevel(artdata$med_code, ref = "Oil")

#head(artdata)
#str(artdata)


##----------------------
##Rank Artists by Volume
##----------------------


#Rank by Total Volume (all)

rankings <- count(artdata, artist)
rankings$rank_all <- dense_rank(desc(rankings$n))    #rank by density, with no gaps between ranks
#rankings$rank_all <- row_number(desc(rankings$n))   #equivalent to rank(ties.method = "first")
#rankings$rank_all <- min_rank(desc(rankings$n))     #equivalent to rank(ties.method = "min")

rankings$n <- NULL
#rankings$rank_all <- factor(rankings$rank_all, labels=c)
#artdata <- merge(artdata, tel, by.x="artist", by.y="artist")

##Rank by Rolling 5-year window
for(i in 1:11) {
    teller <- 1998+i
    som <- count(artdata[(artdata$year>teller & artdata$year<(teller+6)),], artist)
    som$rank_new <- dense_rank(desc(som$n))  
    # the alternative is to rank by row_number or min_rank
    som$n <- NULL
    colnames(som) <- c("artist", paste0("rank_", i))
    rankings <- merge(rankings, som, by.x="artist", by.y="artist",all.x=TRUE)
}
#Rank Update
som <- count(artdata[(artdata$counter>42 & artdata$counter<63),], artist)
som$rank_new <- dense_rank(desc(som$n))  # rank by equivalent to rank(ties.method = "first")
# the alternative is to rank by row_number or min_rank
som$n <- NULL
colnames(som) <- c("artist", "rank_update")
rankings <- merge(rankings, som, by.x="artist", by.y="artist",all.x=TRUE)


##Rank by Annual Volume
for(i in 1:16) {
    teller <- 1999+i
    som <- count(artdata[(artdata$year==teller),], artist)
    som$rank_new <- dense_rank(desc(som$n))
    # the alternative is to rank by row_number or min_rank
    som$n <- NULL
    colnames(som) <- c("artist", paste0("rank_y", teller))
    rankings <- merge(rankings, som, by.x="artist", by.y="artist",all.x=TRUE)
}

##Rank by 2-year Volume
for(i in 1:8) {
    teller <- 1998+(i*2-1)
    som <- count(artdata[(artdata$year>teller & artdata$year<(teller+3)),], artist)
    som$rank_new <- dense_rank(desc(som$n))  
    # the alternative is to rank by row_number or min_rank
    som$n <- NULL
    colnames(som) <- c("artist", paste0("rank_a", i))
    rankings <- merge(rankings, som, by.x="artist", by.y="artist",all.x=TRUE)
}    

artdata <- merge(artdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)


##======================##
## EXPLORATORY ANALYSIS ##
##======================##


#RECREATE THOSE GRAPHS!!!!!

# Example plot
#p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
#    geom_histogram(colour="black", binwidth=50) +
#    facet_grid(Diet ~ .) +
#    ggtitle("Final weight, by diet") +
#    theme(legend.position="none")        # No legend (redundant in this graph)    
#    multiplot(p1, p2, p3, p4, cols=2)


#Simple plots
g <- ggplot(artdata, aes(x=year, y=lnprice))
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + stat_summary(fun.y="median", geom="line")
g <- g + ylab("log of Price")
g <- g + xlab("Year")
g

artplot <- aggregate(artdata$hammer_price, by=list(artdata$timedummy), FUN = sum, na.rm=TRUE)
g <- ggplot(artplot, aes(x=Group.1, y=x))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + ylab("Sum of Hammer Price")
g <- g + xlab("Date")
g

artplot <- aggregate(artdata$hammer_price, by=list(artdata$timedummy), FUN = median, na.rm=TRUE)
g <- ggplot(artplot, aes(x=Group.1, y=x, group=1))
g <- g + geom_line()
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + ylab("Median of Hammer Price")
g <- g + xlab("Date")
g

artplot <- aggregate(artdata$hammer_price, by=list(artdata$year), FUN = median, na.rm=TRUE)
g <- ggplot(artdata, aes(x=year))
g <- g + geom_bar(stat="bin", alpha=0.75)
g <- g + geom_line(data=artplot, aes(x=Group.1, y=x, group=1), colour="blue")
g <- g + ylab("Median of Hammer Price & Number of Sales")
g <- g + xlab("Year")
g

g <- ggplot(artdata, aes(x=ah_code, fill = ah_code))
g <- g + geom_bar(stat="bin")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + ylab("Number od Sales")
g <- g + xlab("Auction House")
g

g <- ggplot(artdata, aes(x=lnprice)) 
g <- g + geom_histogram(aes(y=..density..),binwidth=0.25,colour="black", fill="white") 
g <- g + geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
g

#Doen grpahs van die relationships tussen variables. Bv. price en size

artplot <- subset(artdata, med_code!="Sculpture")
g <- ggplot(artplot, aes(x=lnarea, y=lnprice))
g <- g + geom_point(size = 2, alpha = 0.5, aes(colour = med_code))
g <- g + geom_smooth()
g <- g + ylab("log of Price")
g <- g + xlab("log of Area")
g

g <- ggplot(artdata, aes(x=ah_code, y=lnprice, colour = ah_code))
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + ylab("log of Price")
g <- g + xlab("Auction House")
g

g <- ggplot(artdata, aes(x=med_code, y=lnprice, colour = med_code))
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + ylab("log of Price")
g <- g + xlab("Medium")
g

g <- ggplot(artdata, aes(x=dum_signed, y=lnprice, group=1, colour = dum_signed))
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + geom_smooth(method = "lm", color = "blue")
g <- g + ylab("log of Price")
g <- g + xlab("Signed")
g

g <- ggplot(artdata, aes(x=dum_dated, y=lnprice, group=1, colour = dum_dated))
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + geom_smooth(method = "lm", color = "blue")
g <- g + ylab("log of Price")
g <- g + xlab("Dated")
g

g <- ggplot(artdata, aes(x=dum_titled, y=lnprice, group=1, colour = dum_titled))
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + geom_smooth(method = "lm", color = "blue")
g <- g + ylab("log of Price")
g <- g + xlab("Titled")
g

g <- ggplot(artdata, aes(x=nr_works, y=lnprice, group=1, colour = nr_works))
g <- g + geom_point(size = 2, alpha = 0.5)
g <- g + geom_smooth(method = "lm", color = "blue")
g <- g + ylab("log of Price")
g <- g + xlab("Number of Works")
g



#Look at correlations etc.


##===================##
## REGREESION MODELS ##
##===================##

list_expl_vars=c("lnarea","lnarea2","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                 "nr_works","artist","timedummy")

#-------------------
# FULL SAMPLE MODEL
#-------------------


source("full_model.R")
time_results <- full_model(artdata)


#----------------------
#ROLLING 5-YEAR WINDOWS
#----------------------
source("rolling_model.R")
rolling <- rolling_model(artdata)


index_plot <- melt(rolling[,c(-2,-16)], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g

index_plot <- melt(rolling[,c(1,2,15)], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g

#It does not corresponds to the CAPI anymore - because of the ranking method


#-----------------------------
# OVERLAPPING PERIODS (1-year)
#-----------------------------

source("overlap1y_model.R")
overlap <- overlap1y_model(artdata)

index_plot <- melt(overlap[,c(-2,-20)], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g

index_plot <- rolling[,c(1,2,15)]
index_plot <- cbind(index_plot,Index_Adjacent=overlap[,19])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


#-----------------------------
# OVERLAPPING PERIODS (2-year)
#-----------------------------

source("overlap2y_model.R")
overlap2 <- overlap2y_model(artdata)


index_plot <- melt(overlap2[,c(-2,-12)], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g

index_plot <- rolling[,c(1,2,15)]
index_plot <- cbind(index_plot,Index_Adjacent=overlap[,19])
index_plot <- cbind(index_plot,Index_Adj2=overlap2[,11])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


#----------------------------------
# STRATIFIED REGRESSION RESULTS
#----------------------------------

expl_vars=c("lnarea","ah_code","dum_signed", "dum_dated","nr_works","artist","timedummy")
oil <- full_model(subset(artdata, artdata$med_code="Oil"),expl_vars)



#Look at stratification by price category - Soos Federke


##===============================##
## NAIVE/CENTRAL TENDENCY METHOD ##
##===============================##

#Kyk na 'n naive central tendency index as 'n baseline comparison

#naive_index <- aggregate(artdata$lnprice, by=list(artdata$timedummy), FUN=mean, na.rm=TRUE)
#naive_index$index <- exp(naive_index$x)
#naive_index$index <- naive_index$index/naive_index[1,3]*100

naive_index <- aggregate(artdata$price, by=list(artdata$timedummy), FUN=median, na.rm=TRUE)
naive_index$index <- naive_index$x
naive_index$index <- naive_index$index/naive_index2[1,3]*100

index_plot <- rolling[,c(1,2,15)]
index_plot <- cbind(index_plot,Index_Adjacent=overlap[,19])
#index_plot <- cbind(index_plot,Index_Naive=naive_index[2:62,3])
index_plot <- cbind(index_plot,Index_Naive=naive_index[2:62,3])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


#---------------
#Chained indices
#---------------
strat_p <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist), FUN=median)
strat_q <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist), FUN=sum)
strat_q$x <- strat_q$x/strat_p$x

source(chain)

lys <- c("2000 Q1","2000 Q2","2000 Q3","2000 Q4","2001 Q1","2001 Q2","2001 Q3","2001 Q4","2002 Q1","2002 Q2","2002 Q3","2002 Q4",
         "2003 Q1","2003 Q2","2003 Q3","2003 Q4","2004 Q1","2004 Q2","2004 Q3","2004 Q4","2005 Q1","2005 Q2","2005 Q3","2005 Q4",
         "2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q4",
         "2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1","2010 Q2","2010 Q3","2010 Q4","2011 Q1","2011 Q2","2011 Q3","2011 Q4",
         "2012 Q1","2012 Q2","2012 Q3","2012 Q4","2013 Q1","2013 Q2","2013 Q3","2013 Q4","2014 Q1","2014 Q2","2014 Q3","2014 Q4",
         "2015 Q1","2015 Q2")

ketting <- chain(strat_p,strat_q,lys[1],lys[2])
ketting <- rbind(ketting,chain(strat_p,strat_q,lys[2],lys[3]))
for(i in 3:61) {
    ketting <- rbind(ketting,chain(strat_p,strat_q,lys[i],lys[(i+1)]))
}

ketting <- as.data.frame(ketting)
ketting$V3 <- sqrt(ketting[,1]*ketting[,2])
ketting$V4[1] <- ketting$V3[1]
for(i in 2:61) {
    ketting$V4[i] <- ketting$V4[(i-1)]*ketting$V3[i]
}
ketting$V4 <- ketting$V4*100

index_plot <- rolling[,c(1,2)]
index_plot <- cbind(index_plot,Index_Naive=naive_index[2:62,3])
index_plot <- cbind(index_plot,Index_Fish=ketting$V4)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g

#Stratify by artist and medium

strat_p <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist, artdata$med_code), FUN=median)
strat_q <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist, artdata$med_code), FUN=sum)
strat_q$x <- strat_q$x/strat_p$x

source(chain2)

ketting2 <- chain2(strat_p,strat_q,lys[1],lys[2])
ketting2 <- rbind(ketting2,chain2(strat_p,strat_q,lys[2],lys[3]))
for(i in 3:61) {
    ketting2 <- rbind(ketting2,chain2(strat_p,strat_q,lys[i],lys[(i+1)]))
}
ketting2 <- as.data.frame(ketting2)
ketting2$V3 <- sqrt(ketting2[,1]*ketting2[,2])
ketting2$V4[1] <- ketting2$V3[1]
for(i in 2:61) {
    ketting2$V4[i] <- ketting2$V4[(i-1)]*ketting2$V3[i]
}
ketting2$V4 <- ketting2$V4*100

index_plot <- rolling[,c(1,2)]
index_plot <- cbind(index_plot,Index_Naive=naive_index[2:62,3])
index_plot <- cbind(index_plot,Index_Fish=ketting$V3)
index_plot <- cbind(index_plot,Index_Fish2=ketting2$V3)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


#Fixed base
strat_p <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist), FUN=median)
strat_q <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist), FUN=sum)
strat_q$x <- strat_q$x/strat_p$x

source(chain)
ketting.f1 <- chain(strat_p,strat_q,lys[1],lys[2])
for(i in 3:62) {
    ketting.f1 <- rbind(ketting.f1,chain(strat_p,strat_q,lys[1],lys[i]))
}
ketting.f2 <- chain(strat_p,strat_q,lys[1],lys[62])
for(i in 2:61) {
    ketting.f2 <- rbind(ketting.f2,chain(strat_p,strat_q,lys[i],lys[62]))
}

ketting.f1 <- as.data.frame(ketting.f1)
ketting.f2 <- as.data.frame(ketting.f2)
ketting3 <- as.data.frame(cbind(ketting.f1$V1,ketting.f2$V2))
ketting3$V3 <- sqrt(ketting3[,1]*ketting3[,2])
ketting3$V3 <- ketting3$V3*100

index_plot <- rolling[,c(1,2)]
index_plot <- cbind(index_plot,Index_Naive=naive_index[2:62,3])
index_plot <- cbind(index_plot,Index_Fish=ketting$V4)
index_plot <- cbind(index_plot,Index_Fish2=ketting2$V4)
index_plot <- cbind(index_plot,Index_FishF=ketting3$V3)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


##----------------------------------------------------------------------------------------------------
## Anual Comparisons
##----------------------------------------------------------------------------------------------------
#compare naive and Fisher met full model met annual time dummies
#matches much more closely

naive_annual <- aggregate(artdata$price, by=list(artdata$year), FUN=median, na.rm=TRUE)
naive_annual$index <- naive_annual$x
naive_annual$index <- naive_annual$index/naive_annual[1,3]*100


#source(full_model)
list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                 "nr_works","artist","f.year")
modeldata <- subset(artdata, artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
modeldata$f.year <- factor(modeldata$year)
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
model_all <- lm(expl_vars, data=modeldata)
annual_results <- summary(model_all)$coefficients[grepl("year", rownames(summary(model_all)$coefficients)),1]
annual_results <- as.data.frame(annual_results)
annual_results$index_all <- exp(annual_results$annual_results)*100
annual_results$naive <- naive_annual[2:16,3] 
annual_results$annual_results <- NULL
annual_results$Date <- factor(2001:2015)


strat_p <- aggregate(artdata$price, by=list(artdata$year, artdata$artist), FUN=median)
strat_q <- aggregate(artdata$price, by=list(artdata$year, artdata$artist), FUN=sum)
strat_q$x <- strat_q$x/strat_p$x

chain_a <- function(strat_p, strat_q, jaar1,jaar2) {
    strat_p1 <- subset(strat_p, strat_p$Group.1==jaar1)
    strat_q1 <- subset(strat_q, strat_q$Group.1==jaar1)
    strat_p2 <- subset(strat_p, strat_p$Group.1==jaar2)
    strat_q2 <- subset(strat_q, strat_q$Group.1==jaar2)
    strat_pc <- merge(strat_p1, strat_p2, by="Group.2")
    strat_qc <- merge(strat_q1, strat_q2, by="Group.2")
    Lasp <- sum(strat_pc$x.y*strat_qc$x.x,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.x,na.rm=TRUE)
    Paas <- sum(strat_pc$x.y*strat_qc$x.y,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.y,na.rm=TRUE)
    return(c(Lasp,Paas))
}

lys <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")

source(chain)
kettinga <- chain(strat_p,strat_q,lys[1],lys[2])
kettinga <- rbind(kettinga,chain(strat_p,strat_q,lys[2],lys[3]))
for(i in 3:15) {
    kettinga <- rbind(kettinga,chain(strat_p,strat_q,lys[i],lys[(i+1)]))
}
kettinga <- as.data.frame(kettinga)
kettinga$V3 <- sqrt(kettinga[,1]*ketting[,2])
kettinga$V4[1] <- kettinga$V3[1]
for(i in 2:15) {
    kettinga$V4[i] <- kettinga$V4[(i-1)]*kettinga$V3[i]
}
kettinga$V4 <- kettinga$V4*100

index_plot <- cbind(annual_results,Index_Fisha=kettinga$V4)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g


##------------------------------------------##
##---ARTIST REPUTATION VARIABLE (Kraussl)---##
##------------------------------------------##

##Create reputation variable

#The first step is to estimate equation 1 on a sub-sample of artists in order to obtain 
#the betaj coefficients
modeldata <- subset(artdata, artdata$rank_all<101)
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                    "nr_works","artist","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
model_100 <- lm(expl_vars, data=modeldata)


#In the second step, the betaj coefficients are plugged into equation 5 
#This equation is calculated for every artist pair that consists of the base artist and another. 
rep <- list()
rep[[1]] <- 1

rankings <- count(artdata, artist)
rankings$rank_total <- row_number(desc(rankings$n))
artdata <- merge(artdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)

for(i in 2:(max(artdata$rank_total))) {
    ##maak dit 'n function waaroor jy sapply?????
    sbx <- 0
    list_vars <- c(list_expl_vars,"price")
    
    #geometric average of paintings by artist y
    y <- subset(artdata[,list_vars], artdata$rank_total==1)
    y <- y[!rowSums(is.na(y)), ]
    py <-  exp(mean(log(y$price)))
    #py <- max(cumprod(y$price^(1/nrow(y))))
    
    ym1 <- subset(artdata[,list_vars], artdata$rank_total==i)
    ym1 <- ym1[!rowSums(is.na(ym1)), ]
    pym1 <-  exp(mean(log(ym1$price)))
    #pym1 <- max(cumprod(ym1$price^(1/nrow(ym1))))
    
    #average of characteristics time implicit attribute price
    xy <- mean(y$lnarea)
    xym1 <- mean(ym1$lnarea)   
    b <- summary(model_100)$coefficients[grepl("lnarea", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xy-xym1)   
    sbx <- sbx + bx
    
    xy <- mean(y$lnsculpt_area)
    xym1 <- mean(ym1$lnsculpt_area)   
    b <- summary(model_100)$coefficients[grepl("lnsculpt_area", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xy-xym1)   
    sbx <- sbx + bx
    
    xy <- mean(y$nr_works)
    xym1 <- mean(ym1$nr_works)   
    b <- summary(model_100)$coefficients[grepl("nr_works", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xy-xym1)   
    sbx <- sbx + bx
    
    xy <- mean(as.numeric(y$dum_signed)-1)
    xym1 <- mean(as.numeric(ym1$dum_signed)-1)   
    b <- summary(model_100)$coefficients[grepl("dum_signed", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xy-xym1)   
    sbx <- sbx + bx
    
    xy <- mean(as.numeric(y$dum_dated)-1)
    xym1 <- mean(as.numeric(ym1$dum_dated)-1)   
    b <- summary(model_100)$coefficients[grepl("dum_dated", rownames(summary(model_100)$coefficients)),1]
    bx <- b*(xy-xym1)   
    sbx <- sbx + bx
    
    auc_house <- c("Ashbeys","Bernardi","Bonhams","Russell Kaplan","Stephan Welz & Co","Strauss & Co","Christies")  
    for(j in auc_house) {
        xy <- mean(as.numeric(y$ah_code==j))
        xym1 <- mean(as.numeric(ym1$ah_code==j))  
        b <- summary(model_100)$coefficients[grepl(j, rownames(summary(model_100)$coefficients)),1]
        bx <- b*(xy-xym1)   
        sbx <- sbx + bx
    }
    
    medium <- c("Watercolour","Oil","Acrylic","Print/Woodcut","Mixed Media","Sculpture","Photograhy","Other")  
    for(k in medium) {
        xy <- mean(as.numeric(y$med_code==k))
        xym1 <- mean(as.numeric(ym1$med_code==k))   
        b <- summary(model_100)$coefficients[grepl(k, rownames(summary(model_100)$coefficients)),1]
        bx <- b*(xy-xym1)   
        sbx <- sbx + bx
    }
    
    rep[i] <- (py/pym1)/exp(sbx)
}

#reputation <- as.matrix(rep)

for(i in 1:(max(artdata$rank_total))) { 
    artdata$reputation[(artdata$rank_total==i)] <- rep[i]
}

artdata$reputation <- as.numeric(unlist(artdata$reputation))
artdata$lnrep <- log(artdata$reputation)

g <- ggplot(artdata, aes(x=lnrep, y=lnprice))
g <- g + geom_point(size = 2, alpha = 0.5, aes(colour = med_code))
g <- g + geom_smooth()
g <- g + ylab("log of Price")
g <- g + xlab("log of Reputation")
g

#The result is an index representing average price per artist adjusted for quality, 
#relative to the base artist. 
#The values of this index can proxy for artistic value. 
#It can replace the artist dummies as a continuous variable in a second regression of equation 1. 
#In this regression nearly the full sample is used, leading to a better representation of 
#the total art market.

list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                    "nr_works","lnrep","timedummy")
modeldata <- subset(artdata, rank_all<max(rank_all))

source("full_model.R")
rep_results <- full_model(modeldata,list_expl_vars)

source("overlap1y_model.R")
rep_overlap1 <- overlap1y_model(modeldata,list_expl_vars)

source("overlap2y_model.R")
rep_overlap2 <- overlap2y_model(modeldata,list_expl_vars)

## Maar regression wys 'n neagtive coefficient ?????
## Los dalk die artists uit wat net een paiting verkoop het !!!!

index_plot <- rolling[,c(1,2)]
index_plot <- cbind(index_plot,Index_Adjacent=overlap[,19])
index_plot <- cbind(index_plot,Index_Adj2=overlap2[,11])
index_plot <- cbind(index_plot,RepIndex_Full=rep_results[,2])
index_plot <- cbind(index_plot,RepOverlap_1y=rep_overlap1[,19])
index_plot <- cbind(index_plot,RepOverlap_2y=rep_overlap2[,11])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g





##=====================##
## QUANTILE REGRESSION ##
##=====================##

#qreg


##======================##
## MATCHING METHODOLOGY ##
##======================##

#check for duplicates (how many)

#match per artist by hedonic function

#see MatchIt function
#see McMillen repeat sales function




##==============##
## DIAGNIOSTICS ##
##==============##


#Check outilers, leverage, influence
#Check regression diagnostics, heteroskedasticity, etc.
# Robustness Tests
# Try themes and materials with grepl












