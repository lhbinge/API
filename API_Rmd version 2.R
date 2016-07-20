
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
suppressMessages(library(scales))
suppressMessages(library(tseries))
suppressMessages(library(urca))
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\Art Price Index\\R Code")
artdata <- read.csv("Auction database.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE, 
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
#inteaction term: sculptures often only reported with 1 dimension (height)
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

#-------------------------------
#Plot total sales by auction house
artplot <- aggregate(artdata$hammer_price, by=list(artdata$year,artdata$ah_code), FUN = length)
g <- ggplot(artplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Auction House")
g <- g + scale_y_continuous(labels=comma)
g <- g + ylab("Total Number of Sales (Auction Lots)")
g <- g + xlab("Date")
g


#Plot total turnover and annual median price
artplot1 <- aggregate(artdata$hammer_price, by=list(artdata$year), sum, na.rm=TRUE)
artplot2 <- aggregate(artdata$hammer_price, by=list(artdata$year), FUN = median, na.rm=TRUE)
artplot <- merge(artplot1, artplot2, by="Group.1",all.x=TRUE)
names(artplot) <- c("Date","Turnover","Median_Price")
artplot$Turnover <- artplot$Turnover/1000000
par(mar = c(5,5,2,5))
plot(artplot$Date, artplot$Median_Price, type="l", lwd=2, col="#F8766D", ylim=c(0,10000), ylab="Median Price", xlab="Date")
par(new = T)
barplot(artplot$Turnover, col="#00BFC4",  ylim=c(0,600), axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, "Turnover (Rm)")
legend("topleft", legend=c("Median Price", "Turnover (Rm)"), lty=c(1,0), pch=c(NA, 15), col=c("#F8766D","#00BFC4"))


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

##----------------------##
##---CENTRAL TENDENCY---##
##----------------------##
naive_index <- aggregate(artdata$price, by=list(artdata$timedummy), FUN=median, na.rm=TRUE)
naive_index$index <- naive_index$x
naive_index$index <- naive_index$index/naive_index[1,2]*100
naive_index$index <- as.numeric(naive_index$index)
colnames(naive_index) <- c("Date","Median","Median_Index")

#------------------------------
#Stratify by artist and medium
#get quantity and median price per group per quarter    
strat_p <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist, artdata$med_code), FUN=median)
strat_q <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist, artdata$med_code), FUN=sum)
strat_q$x <- strat_q$x/strat_p$x        #the count q

chain2 <- function(strat_p, strat_q, kwartaal1,kwartaal2) {
    strat_p1 <- subset(strat_p, strat_p$Group.1==kwartaal1)
    strat_q1 <- subset(strat_q, strat_q$Group.1==kwartaal1)
    strat_p2 <- subset(strat_p, strat_p$Group.1==kwartaal2)
    strat_q2 <- subset(strat_q, strat_q$Group.1==kwartaal2)
    #get sample of median prices and quantities for specific artist for the two quarters
    strat_pc <- merge(strat_p1, strat_p2, by=c("Group.2","Group.3"))
    strat_qc <- merge(strat_q1, strat_q2, by=c("Group.2","Group.3"))
    #Laspeyres (keeps quantity weights fixed at base)
    Lasp <- sum(strat_pc$x.y*strat_qc$x.x,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.x,na.rm=TRUE)
    #Paasche (keeps quantity weights fixed at end)
    Paas <- sum(strat_pc$x.y*strat_qc$x.y,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.y,na.rm=TRUE)
    return(c(Lasp,Paas))
}

datum <- c("2000 Q1","2000 Q2","2000 Q3","2000 Q4","2001 Q1","2001 Q2","2001 Q3","2001 Q4","2002 Q1","2002 Q2","2002 Q3","2002 Q4",
           "2003 Q1","2003 Q2","2003 Q3","2003 Q4","2004 Q1","2004 Q2","2004 Q3","2004 Q4","2005 Q1","2005 Q2","2005 Q3","2005 Q4",
           "2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q4",
           "2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1","2010 Q2","2010 Q3","2010 Q4","2011 Q1","2011 Q2","2011 Q3","2011 Q4",
           "2012 Q1","2012 Q2","2012 Q3","2012 Q4","2013 Q1","2013 Q2","2013 Q3","2013 Q4","2014 Q1","2014 Q2","2014 Q3","2014 Q4",
           "2015 Q1","2015 Q2","2015 Q3","2015 Q4")

ketting2 <- chain2(strat_p,strat_q,datum[1],datum[2])
ketting2 <- rbind(ketting2,chain2(strat_p,strat_q,datum[2],datum[3]))
for(i in 3:63) {
    ketting2 <- rbind(ketting2,chain2(strat_p,strat_q,datum[i],datum[(i+1)]))
}
ketting2 <- as.data.frame(ketting2)
ketting2$V3 <- sqrt(ketting2[,1]*ketting2[,2])  #Fisher index is the geometric mean
ketting2$V4[1] <- ketting2$V3[1]*100
for(i in 2:63) {                                #use the growth rates to generate the index
    ketting2$V4[i] <- ketting2$V4[(i-1)]*ketting2$V3[i]
}
ketting2$Date <- as.factor(datum[-1])
colnames(ketting2) <- c("Las","Paas","Fisher","Fisher_Index","Date")

naive_index <- merge(naive_index, ketting2, by.x="Date", by.y="Date",all=TRUE)
naive_index[1,4:7] <- 100

index_plot <- naive_index[,c(1,3,7)]
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g








