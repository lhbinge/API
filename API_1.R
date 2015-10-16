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
library(stargazer)
library(micEcon)
library(quantreg)
library(McSpatial)

#setwd("C:/Users/Laurie/OneDrive/Documents/BING/METRICS/PhD Proposal Readings/Art Price Index")
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\PhD Proposal Readings\\Art Price Index\\R Code")

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
                                                      "Mixed Media","Sculpture","Photography", "Other"))
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

## Rank total
rankings <- count(artdata, artist)
rankings$rank_total <- row_number(desc(rankings$n))
artdata <- merge(artdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)

##======================##
## EXPLORATORY ANALYSIS ##
##======================##

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

artplot <- aggregate(artdata$hammer_price, by=list(artdata$year,artdata$ah_code), FUN = sum, na.rm=TRUE)
g <- ggplot(artplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Auction House")
g <- g + ylab("Turnover (Sum of Hammer Price)")
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


artplot1 <- aggregate(artdata$hammer_price, by=list(artdata$year), length)
artplot2 <- aggregate(artdata$hammer_price, by=list(artdata$year), FUN = median, na.rm=TRUE)
artplot <- merge(artplot1, artplot2, by="Group.1",all.x=TRUE)
names(artplot) <- c("Date","Total Sales","Median Price")
artplot <- melt(artplot, id="Date") 
g <- ggplot(artplot, aes(x=Date,value,colour=variable,fill=variable))
g <- g + geom_bar(subset=.(variable=="Total Sales"),stat="identity")
g <- g + geom_line(subset=.(variable=="Median Price"),size=1.5)
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
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

list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                 "nr_works","artist","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))

#-------------------
# FULL SAMPLE MODEL
#-------------------

#source("full_model.R")
#time_results <- full_model(artdata)

modeldata <- subset(artdata, artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
model_all <- lm(expl_vars, data=modeldata)
time_results <- summary(model_all)$coefficients[grepl("time", rownames(summary(model_all)$coefficients)),1]
time_results <- as.data.frame(time_results)
time_results$index_all <- exp(time_results$time_results)*100

#stargazer(model_all, omit=c("artist","timedummy"))

#----------------------
#ROLLING 5-YEAR WINDOWS
#----------------------
#source("rolling_model.R")
#rolling <- rolling_model(artdata)

res_list <- list()
for(i in 1:11) {
    modeldata <- subset(artdata, artdata[,(31+i)]<max(artdata[,(31+i)],na.rm=TRUE))
    modeldata <- subset(modeldata, modeldata$counter>(i*4-4)&modeldata$counter<(i*4+17))
    model <- lm(expl_vars, data=modeldata)  
    summary(model)
    res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
}
    
#Update
modeldata <- subset(artdata, artdata[,43]<max(artdata[,43],na.rm=TRUE))
modeldata <- subset(modeldata, modeldata$counter>42& modeldata$counter<63)
model <- lm(expl_vars, data=modeldata)  
summary(model)
res_list[[12]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
    
#Merge all results
rolling <- time_results
rolling$time_results <- NULL
rolling <- merge(rolling, res_list[[1]], by="row.names", all=TRUE)
rolling[,3] <- exp(rolling[,3])*100
for(i in 2:12) {
    rolling <- merge(rolling, res_list[[i]], by.x = "Row.names", by.y = "row.names", all=TRUE)
    rolling[,(i+2)] <- exp(rolling[i+2])*100
}    

#Calculate index
rolling$ind <- rolling[,3]
rolling[2,15] <- rolling[3,15]*rolling[2,2]/rolling[3,2]  #interpolate
rolling$teller <- c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,
                    10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14)
for(i in 19:60) {  #chaining
    j <- rolling[(i+1),16]
    rolling[(i+1),15] <- rolling[i,15]*rolling[(i+1),j]/rolling[i,j]
}
    
colnames(rolling) <- c("Date","Index_Full","Index_m1","Index_m2","Index_m3","Index_m4","Index_m5","Index_m6",
                       "Index_m7","Index_m8","Index_m9","Index_m10","Index_m11","Index_m12","Index_Rolling","teller")

rolling$Date <- c("2000Q2","2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
                  "2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                  "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                  "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                  "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                  "2015Q1","2015Q2")
    
rolling$Date <- factor(rolling$Date)


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

#source("overlap1y_model.R")
#overlap <- overlap1y_model(artdata)

res_list <- list()
for(i in 1:16) {
    modeldata <- subset(artdata, artdata[,(43+i)]<max(artdata[,(43+i)],na.rm=TRUE))
    modeldata <- subset(modeldata, modeldata$counter>(i*4-5)& modeldata$counter<(i*4+1))
    model <- lm(expl_vars, data=modeldata)  
    res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
}
    
#Merge all results
overlap <- time_results
overlap$time_results <- NULL
overlap <- merge(overlap, res_list[[1]], by="row.names", all=TRUE)
overlap[,3] <- exp(overlap[,3])*100
for(i in 2:16) {
    overlap <- merge(overlap, res_list[[i]], by.x = "Row.names", by.y = "row.names", all=TRUE)
    overlap[,(i+2)] <- exp(overlap[i+2])*100
} 
    
#Calculate index
overlap$ind <- overlap[,3]
overlap[2,19] <- overlap[3,19]*overlap[2,2]/overlap[3,2]   #Interpolate
overlap$teller <- c(3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12,
                    13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18)
    
for(i in 3:60) {
    j <- overlap[(i+1),20]
    if(is.na(overlap[i,j])) {
        overlap[(i+1),19] <- overlap[i,19]*overlap[(i+1),j]/100
    } else { 
        overlap[(i+1),19] <- overlap[i,19]*overlap[(i+1),j]/overlap[i,j] 
    }   
}
    
colnames(overlap) <- c("Date","Index_Full","Index_m1","Index_m2","Index_m3","Index_m4","Index_m5","Index_m6",
                       "Index_m7","Index_m8","Index_m9","Index_m10","Index_m11","Index_m12","Index_m13",
                       "Index_m14","Index_m15","Index_m16","Index_Adjacent","teller")
    
overlap$Date <- c("2000Q2","2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
                  "2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                  "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                  "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                  "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                  "2015Q1","2015Q2")
    
overlap$Date <- factor(overlap$Date)

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

#source("overlap2y_model.R")
#overlap2 <- overlap2y_model(artdata)

res_list <- list()
for(i in 1:8) {
    modeldata <- subset(artdata, artdata[,(59+i)]<max(artdata[,(59+i)],na.rm=TRUE))
    modeldata <- subset(modeldata, modeldata$counter>(i*8-9)& modeldata$counter<(i*8+1))
    model <- lm(expl_vars, data=modeldata)  
    summary(model)
    res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
}

#Merge all results
overlap2 <- time_results
overlap2$time_results <- NULL
overlap2 <- merge(overlap2, res_list[[1]], by="row.names", all=TRUE)
overlap2[,3] <- exp(overlap2[,3])*100
for(i in 2:8) {
    overlap2 <- merge(overlap2, res_list[[i]], by.x = "Row.names", by.y = "row.names", all=TRUE)
    overlap2[,(i+2)] <- exp(overlap2[i+2])*100
} 

#Calculate index
overlap2$ind <- overlap2[,3]
overlap2[2,11] <- overlap2[3,11]*overlap2[2,2]/overlap2[3,2]   #Interpolate
overlap2$teller <- c(3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,
                     7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,10,10,10,10,10,10)

for(i in 7:60) {
    j <- overlap2[(i+1),12]
    if(is.na(overlap2[i,j])) {
        overlap2[(i+1),11] <- overlap2[i,11]*overlap2[(i+1),j]/100
    } else { 
        overlap2[(i+1),11] <- overlap2[i,11]*overlap2[(i+1),j]/overlap2[i,j] 
    }   
}

colnames(overlap2) <- c("Date","Index_Full","Index_m1","Index_m2","Index_m3","Index_m4","Index_m5",
                        "Index_m6","Index_m7","Index_m8","Index_Adj2","teller")

overlap2$Date <- c("2000Q2","2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
                   "2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                   "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                   "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                   "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                   "2015Q1","2015Q2")

overlap2$Date <- factor(overlap2$Date)


index_plot <- melt(overlap2[,c(-2,-12)], id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g

png(file = "Adjacent Period.png", width=600,height=360)
index_plot <- rolling[,c(1,2,15)]
index_plot <- cbind(index_plot,Adjacent_1year=overlap[,19])
index_plot <- cbind(index_plot,Adjacent_2year=overlap2[,11])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g
dev.off()

#----------------------------------
# STRATIFIED REGRESSION RESULTS
#----------------------------------

source("full_model.R")
expl_vars=c("lnarea","ah_code","dum_signed","dum_dated","nr_works","lnrep","timedummy")
drawing     <- full_model(subset(artdata, artdata$med_code=="Drawing"),expl_vars)
watercolour <- full_model(subset(artdata, artdata$med_code=="Watercolour"),expl_vars)
oil         <- full_model(subset(artdata, artdata$med_code=="Oil"),expl_vars)
acrylic     <- full_model(subset(artdata, artdata$med_code=="Acrylic"),expl_vars)
print       <- full_model(subset(artdata, artdata$med_code=="Print/Woodcut"),expl_vars)
mixed       <- full_model(subset(artdata, artdata$med_code=="Mixed Media"),expl_vars)
sculpture   <- full_model(subset(artdata, artdata$med_code=="Sculpture"),expl_vars)
photo       <- full_model(subset(artdata, artdata$med_code=="Photography"),expl_vars)
other       <- full_model(subset(artdata, artdata$med_code=="Other"),expl_vars)

l <- list(drawing,watercolour,oil,acrylic,print,mixed,sculpture,photo,other)
l <- lapply(l, function(x) data.frame(x, rn = row.names(x)))
mediums <- merge(l[1], l[2], by="rn", all=TRUE) # merge by row names (by=0 or by="row.names")
for(i in 3:9) { mediums <- merge(mediums, l[i], by="rn", all=TRUE)}
mediums <- mediums[,c(-1,-2,-4,-6,-8,-10,-12,-14,-16,-18)]
names(mediums) <- c("drawing","watercolour","oil","acrylic","print","mixed","sculpture","photo","other")
mediums <- rbind(c(100,100,100,100,100,100,100,100,100),mediums)

#weight by sales value
sales <- aggregate(artdata$hammer_price,by=list(artdata$med_code,artdata$timedummy),FUN=sum,na.rm=TRUE)
sales <- dcast(sales, Group.2 ~ Group.1)
sales <- sales[-1]
sales <- sales/rowSums(sales,na.rm =TRUE)

w.ave <- as.data.frame(rowSums(mediums*sales,na.rm=TRUE))

hed_strat <- cbind(mediums,sales)
hed_strat[is.na(hed_strat)] <- 0
Las <- priceIndex(c("drawing","watercolour","oil","acrylic","print","mixed","sculpture","photo","other"),
           c("Drawing", "Watercolour", "Oil", "Acrylic", "Print/Woodcut","Mixed Media","Sculpture","Photography", 
             "Other"), 1, hed_strat, "Laspeyres" ,na.rm=TRUE)
Paas <- priceIndex(c("drawing","watercolour","oil","acrylic","print","mixed","sculpture","photo","other"),
                  c("Drawing", "Watercolour", "Oil", "Acrylic", "Print/Woodcut","Mixed Media","Sculpture","Photography", 
                    "Other"), 1, hed_strat, "Paasche" ,na.rm=TRUE)
Fish <- priceIndex(c("drawing","watercolour","oil","acrylic","print","mixed","sculpture","photo","other"),
                  c("Drawing", "Watercolour", "Oil", "Acrylic", "Print/Woodcut","Mixed Media","Sculpture","Photography", 
                    "Other"), 1, hed_strat, "Fisher" ,na.rm=TRUE)

Las <- as.data.frame(Las*100)
Paas <- as.data.frame(Paas*100)
Fish <- as.data.frame(Fish*100)

index_plot <- rolling[,c(1,2)]
index_plot <- cbind(index_plot,Stat_Hedonic=w.ave[-1,])
index_plot <- cbind(index_plot,Laspeyres=Las[-1,])
index_plot <- cbind(index_plot,Paasche=Paas[-1,])
index_plot <- cbind(index_plot,Fisher=Fish[-1,])
index_plot <- melt(index_plot, id="Date")  # convert to long format
#png(file = "Hedonic Stratified.png", width=600,height=360)
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g
#dev.off()


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

#source("chain.R")

chain <- function(strat_p, strat_q, kwartaal1,kwartaal2) {
    strat_p1 <- subset(strat_p, strat_p$Group.1==kwartaal1)
    strat_q1 <- subset(strat_q, strat_q$Group.1==kwartaal1)
    strat_p2 <- subset(strat_p, strat_p$Group.1==kwartaal2)
    strat_q2 <- subset(strat_q, strat_q$Group.1==kwartaal2)
    strat_pc <- merge(strat_p1, strat_p2, by="Group.2")
    strat_qc <- merge(strat_q1, strat_q2, by="Group.2")
    #strat_p2 <- subset(strat_p2, strat_p2$Group.2 %in% strat_p1$Group.2)
    
    #Laspeyres
    Lasp <- sum(strat_pc$x.y*strat_qc$x.x,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.x,na.rm=TRUE)
    #Paasche
    Paas <- sum(strat_pc$x.y*strat_qc$x.y,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.y,na.rm=TRUE)
    return(c(Lasp,Paas))
}

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

#------------------------------
#Stratify by artist and medium

strat_p <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist, artdata$med_code), FUN=median)
strat_q <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist, artdata$med_code), FUN=sum)
strat_q$x <- strat_q$x/strat_p$x

#source("chain2.R")

chain2 <- function(strat_p, strat_q, kwartaal1,kwartaal2) {
    strat_p1 <- subset(strat_p, strat_p$Group.1==kwartaal1)
    strat_q1 <- subset(strat_q, strat_q$Group.1==kwartaal1)
    strat_p2 <- subset(strat_p, strat_p$Group.1==kwartaal2)
    strat_q2 <- subset(strat_q, strat_q$Group.1==kwartaal2)
    strat_pc <- merge(strat_p1, strat_p2, by=c("Group.2","Group.3"))
    strat_qc <- merge(strat_q1, strat_q2, by=c("Group.2","Group.3"))
    #strat_p2 <- subset(strat_p2, strat_p2$Group.2 %in% strat_p1$Group.2)
    
    #Laspeyres
    Lasp <- sum(strat_pc$x.y*strat_qc$x.x,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.x,na.rm=TRUE)
    #Paasche
    Paas <- sum(strat_pc$x.y*strat_qc$x.y,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.y,na.rm=TRUE)
    return(c(Lasp,Paas))
}

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

#------------------------------
#Fixed base

strat_p <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist), FUN=median)
strat_q <- aggregate(artdata$price, by=list(artdata$timedummy, artdata$artist), FUN=sum)
strat_q$x <- strat_q$x/strat_p$x

source("chain.R")
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
modeldata <- subset(artdata, artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
modeldata$f.year <- factor(modeldata$year)
list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                 "nr_works","artist","f.year")
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

#source("chain_a.R")
kettinga <- chain_a(strat_p,strat_q,lys[1],lys[2])
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

for(i in 2:(max(artdata$rank_total))) {
    ##maak dit 'n function waaroor jy sapply?????
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

list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","lnrep","timedummy")
#modeldata <- subset(artdata, rank_all<max(rank_all))

source("full_model.R")
rep_results <- full_model(artdata,list_expl_vars)

source("overlap1y_model.R")
rep_overlap1 <- overlap1y_model(artdata,list_expl_vars)

source("overlap2y_model.R")
rep_overlap2 <- overlap2y_model(artdata,list_expl_vars)

source("rolling_model.R")
rep_rolling <- rolling_model(artdata,list_expl_vars)

## Maar regression wys 'n neagtive coefficient ?????
## Los dalk die artists uit wat net een paiting verkoop het !!!!

index_plot <- rolling[,c(1,2)]
#index_plot <- rolling[,c(1,2,15)]
#index_plot <- cbind(index_plot,Index_Adjacent=overlap[,19])
#index_plot <- cbind(index_plot,Index_Adj2=overlap2[,11])
index_plot <- cbind(index_plot,RepIndex_Full=rep_results[,2])
index_plot <- cbind(index_plot,RepOverlap_1y=rep_overlap1[,19])
index_plot <- cbind(index_plot,RepOverlap_2y=rep_overlap2[,11])
index_plot <- cbind(index_plot,RepRolling=rep_rolling[,15])
index_plot <- melt(index_plot, id="Date")  # convert to long format

png(file = "Reputation.png", width=600,height=360)
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g
dev.off()




##=====================##
## QUANTILE REGRESSION ##
##=====================##

list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                    "nr_works","lnrep","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
quant <- rq(expl_vars, tau=c(0.10,0.25,0.5,0.75,0.90), data=artdata)
quant_results <- coef(quant)[grepl("time", rownames(coef(quant))),1:5]
quant_results <- as.data.frame(quant_results)
quant_results <- exp(quant_results)*100

index_plot <- rolling[,c(1,2)]
index_plot <- cbind(index_plot,quant_results)
index_plot <- index_plot[,-2]
index_plot <- melt(index_plot, id="Date")  # convert to long format

#png(file = "Quantile.png", width=600,height=360)
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g
#dev.off()

#quant.all <- rq(expl_vars,data=artdata,tau=seq(0.05,0.95,0.05))
#par(mar=c(0.1,0.1,0.1,0.1))
#quant.plot <- summary(quant.all)
#plot(quant.plot)

##=====================##
## REPEAT SALES METHOD ##
##=====================##

#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated")]))

allDup <- function (value)
{
    duplicated(value) | duplicated(value, fromLast = TRUE)
}
rsartdata <- artdata[allDup(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated")]),]
rsartdata <- transform(rsartdata,id=as.numeric(factor(title)))

repdata <- repsaledata(rsartdata$lnprice,rsartdata$counter,rsartdata$id)
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=3,
                       graph=TRUE,graph.conf=TRUE,conf=.95)
repeatsales_index <- exp(as.data.frame(repeatsales$pindex))*100

#The distance metric between any two sales (across the intervening time period) is the 
#absolute value of the difference between the two predicted hedonic log values. 
#The threshold for this distance metric can be customised. 
#At one extreme, one can choose to select only one pair with the smallest value of the distance metric. 

#match per artist by hedonic function
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))


teller <-0
fullrep <- data.frame()
fullrep1<- data.frame()
fullrep2 <- data.frame()
fullrep3<- data.frame()
fullrep4 <- data.frame()
fullrep5<- data.frame()
fullrep6 <- data.frame()
fullrep7<- data.frame()
rartdata <- subset(artdata,artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
keep <- c("lnprice","artist","lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
          "nr_works","timedummy","counter")
rartdata <- rartdata[,names(rartdata) %in% keep]
rartdata <- rartdata[complete.cases(rartdata),]  

## Rank total again for new sample
rankings <- count(rartdata, artist)
rankings$rank_total <- row_number(desc(rankings$n))
rartdata <- merge(rartdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)
    
for(k in 1:max(rartdata$rank_total)) {
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
        #calculate predicted value for each painting (excluding time dummies)
        #newdata <- modeldata
        #newdata$timedummy <- modeldata$timedummy[1]
        modeldata$timedummy <- modeldata$timedummy[1]
        #pr <- which(!(newdata$med_code %in% levels(modeldata$med_code)))
        #newdata$med_code[pr] <- NA
        #modeldata <- cbind(modeldata,fitted=predict.lm(model,newdata))
        #fitted<-as.data.frame(predict.lm(model,newdata,type="terms",na.action = na.exclude,
        #        terms = c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated","nr_works")))
        modeldata <- cbind(modeldata,fitted=predict.lm(model))
        #modeldata$fitted <- rowSums(fitted)            
        #modeldata <- modeldata[!is.na(modeldata$fitted),]

#try id manually:
        modeldata$id <- 0
        for(i in 1:nrow(modeldata)) {
            teller <- teller + 1
            if(modeldata$id[i]==0) {
                modeldata$id[i] <- teller
                medium <- modeldata$med_code[i]
                modeldata$distance <- abs(modeldata$fitted[i]-modeldata$fitted)
                #modeldata$id[(modeldata$distance==min(modeldata[modeldata[,"id"]==0,"distance"],na.rm=TRUE))] <- teller
                modeldata$id[(modeldata$distance==min(modeldata[modeldata[,"id"]==0 & modeldata[,"med_code"]==medium ,"distance"],na.rm=TRUE))] <- teller
            } 
        }

        repdata <- repsaledata(modeldata$lnprice,modeldata$counter,modeldata$id)
        fullrep <- rbind(fullrep,repdata)
        repdata1 <- repsaledata(modeldata$lnarea,modeldata$counter,modeldata$id)
        fullrep1 <- rbind(fullrep1,repdata1)
        repdata2 <- repsaledata(modeldata$med_code,modeldata$counter,modeldata$id)
        fullrep2 <- rbind(fullrep2,repdata2)
        repdata3 <- repsaledata(modeldata$ah_code,modeldata$counter,modeldata$id)
        fullrep3 <- rbind(fullrep3,repdata3)
        repdata4 <- repsaledata(modeldata$lnsculpt_area,modeldata$counter,modeldata$id)
        fullrep4 <- rbind(fullrep4,repdata4)
        repdata5 <- repsaledata(modeldata$dum_signed,modeldata$counter,modeldata$id)
        fullrep5 <- rbind(fullrep5,repdata5)
        repdata6 <- repsaledata(modeldata$dum_dated,modeldata$counter,modeldata$id)
        fullrep6 <- rbind(fullrep6,repdata6)
        repdata7 <- repsaledata(modeldata$nr_works,modeldata$counter,modeldata$id)
        fullrep7 <- rbind(fullrep7,repdata7)
    }
}

fullrep <- cbind(fullrep,fullrep1[,4:5])
fullrep <- cbind(fullrep,fullrep2[,4:5])
fullrep <- cbind(fullrep,fullrep3[,4:5])
fullrep <- cbind(fullrep,fullrep4[,4:5])
fullrep <- cbind(fullrep,fullrep5[,4:5])
fullrep <- cbind(fullrep,fullrep6[,4:5])
fullrep <- cbind(fullrep,fullrep7[,4:5])

colnames(fullrep) <- c("id","time0","time1","price0","price1","area0","area1","med_code0","med_code1",
                    "ah_code0","ah_code1","sculpt0","sculpt1","sign0","sign1","date0","date1",
                    "nr0","nr1")

repeatsales <- repsale(fullrep$price0,fullrep$time0,fullrep$price1,fullrep$time1,mergefirst=3,
         graph=TRUE,graph.conf=TRUE,conf=.95)
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



index_plot <- time_results

index_plot$Date <- c("2000Q2","2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
                      "2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                      "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                      "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                      "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                      "2015Q1","2015Q2")

repeatsales_index$Date <- c("2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
                       "2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                       "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                       "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                       "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                       "2015Q1","2015Q2")
index_plot <- merge(index_plot, repeatsales_index, by="Date", all=TRUE)
index_plot <- cbind(index_plot,Simple_ps.RS=sps.RS_index[2:62,1])
index_plot <- cbind(index_plot,Full_ps.RS=ps.RS_results[2:62,2])
index_plot <- index_plot[,-2]
#png(file = "Repeat Sales.png", width=600,height=360)

index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g
#dev.off()

#Alternatively, one can set a specified threshold, with all the pairs ranking from the smallest to the largest 
#distance values, and then select the lowest x% of the pairs with their distance metric smaller than a certain value. 
#Pseudo pairs are generated by matching each transaction with its most adjacent subsequent transaction. 

#if predicted value < distance metric & timedummy = following quarter 
#=> gee dieselfde id number

list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))


teller <-0
fullrep <- data.frame()
fullrep1<- data.frame()
fullrep2 <- data.frame()
fullrep3<- data.frame()
fullrep4 <- data.frame()
fullrep5<- data.frame()
fullrep6 <- data.frame()
fullrep7<- data.frame()
rartdata <- subset(artdata,artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
keep <- c("lnprice","artist","lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
          "nr_works","timedummy","counter")
rartdata <- rartdata[,names(rartdata) %in% keep]
rartdata <- rartdata[complete.cases(rartdata),]  

## Rank total again for new sample
rankings <- count(rartdata, artist)
rankings$rank_total <- row_number(desc(rankings$n))
rartdata <- merge(rartdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)

for(k in 1:max(rartdata$rank_total)) {
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
        #calculate predicted value for each painting (excluding time dummies)
        #newdata <- modeldata
        #newdata$timedummy <- modeldata$timedummy[1]
        modeldata$timedummy <- modeldata$timedummy[1]
        #pr <- which(!(newdata$med_code %in% levels(modeldata$med_code)))
        #newdata$med_code[pr] <- NA
        #modeldata <- cbind(modeldata,fitted=predict.lm(model,newdata))
        #fitted<-as.data.frame(predict.lm(model,newdata,type="terms",na.action = na.exclude,
        #        terms = c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated","nr_works")))
        modeldata <- cbind(modeldata,fitted=predict.lm(model))
        #modeldata$fitted <- rowSums(fitted)            
        #modeldata <- modeldata[!is.na(modeldata$fitted),]
        
        #try id manually:
        modeldata$id <- 0
        for(i in 1:nrow(modeldata)) {
            teller <- teller + 1
            if(modeldata$id[i]==0) {
                modeldata$id[i] <- teller
                medium <- modeldata$med_code[i]
                modeldata$distance <- abs(modeldata$fitted[i]-modeldata$fitted)
                modeldata$id[(modeldata$distance<0.1 & modeldata[,"id"]==0 & modeldata[,"med_code"]==medium)] <- teller
            } 
        }
        
        repdata <- repsaledata(modeldata$lnprice,modeldata$counter,modeldata$id)
        fullrep <- rbind(fullrep,repdata)
        repdata1 <- repsaledata(modeldata$lnarea,modeldata$counter,modeldata$id)
        fullrep1 <- rbind(fullrep1,repdata1)
        repdata2 <- repsaledata(modeldata$med_code,modeldata$counter,modeldata$id)
        fullrep2 <- rbind(fullrep2,repdata2)
        repdata3 <- repsaledata(modeldata$ah_code,modeldata$counter,modeldata$id)
        fullrep3 <- rbind(fullrep3,repdata3)
        repdata4 <- repsaledata(modeldata$lnsculpt_area,modeldata$counter,modeldata$id)
        fullrep4 <- rbind(fullrep4,repdata4)
        repdata5 <- repsaledata(modeldata$dum_signed,modeldata$counter,modeldata$id)
        fullrep5 <- rbind(fullrep5,repdata5)
        repdata6 <- repsaledata(modeldata$dum_dated,modeldata$counter,modeldata$id)
        fullrep6 <- rbind(fullrep6,repdata6)
        repdata7 <- repsaledata(modeldata$nr_works,modeldata$counter,modeldata$id)
        fullrep7 <- rbind(fullrep7,repdata7)
    }
}

fullrep <- cbind(fullrep,fullrep1[,4:5])
fullrep <- cbind(fullrep,fullrep2[,4:5])
fullrep <- cbind(fullrep,fullrep3[,4:5])
fullrep <- cbind(fullrep,fullrep4[,4:5])
fullrep <- cbind(fullrep,fullrep5[,4:5])
fullrep <- cbind(fullrep,fullrep6[,4:5])
fullrep <- cbind(fullrep,fullrep7[,4:5])

colnames(fullrep) <- c("id","time0","time1","price0","price1","area0","area1","med_code0","med_code1",
                       "ah_code0","ah_code1","sculpt0","sculpt1","sign0","sign1","date0","date1",
                       "nr0","nr1")

repeatsales <- repsale(fullrep$price0,fullrep$time0,fullrep$price1,fullrep$time1,mergefirst=3,
                       graph=TRUE,graph.conf=TRUE,conf=.95)
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



#try with matchit:

rescale <- function (x, nx1, nx2, minx, maxx) { 
    nx = nx1 + (nx2 - nx1) * (x - minx)/(maxx - minx)
    return(nx)
}
modeldata$ny <- rescale(modeldata$fitted,0,1,min(modeldata$fitted),max(modeldata$fitted)) 

list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","timedummy")
form <- as.formula(paste("ny~",paste(list_expl_vars,collapse="+")))
modeldata <- modeldata[,c(-13,-21,-22)]
modeldata <- modeldata[complete.cases(modeldata),]

m.out <- matchit(expl_vars, method="nearest",
                 data=modeldata)  #propensity score matching


#----------------------------------
#MatchIt function (see source code)
#----------------------------------

library(MatchIt)
m.out <- matchit(treatment ~ x1+x2+x3+x4+x5+x6, method="nearest",
                 data=data1,ratio=1)  #propensity score matching

m.out <- matchit(treatment ~ x1+x2+x3+x4+x5+x6, method="exact",
                 data=data1,ratio=4)

#distance: the method used to estimate the distance measure (default = "logit",
#logistic regression) or a numerical vector of user's own distance measure.

#replace: logical value indicating whether each control unit can be matched to more
#than one treated unit (default = replace = FALSE, each control unit is used at most
#once - i.e., sampling without replacement)

#exact: variables on which to perform exact matching within the nearest neighbor
#matching (default = NULL, no exact matching).
#exact should be entered as a vector of variable names (e.g., exact =c("X1", "X2"))

m.out
final.data <- match.data(m.out)  #save matched data
names(final.data)
plot(m.out)
plot(m.out,type="jitter")
plot(m.out,type="hist")
write.csv(final_data,file="matchNN.csv")

m.out <- zz$match.matrix  # This gives us the matched matrix
m.out
options("scipen" =100, "digits" = 4) # override R's tendency to use scientific notation

z.out <- zelig(Y ~ treat + x1 + x2, model = mymodel, data = final.data)

t.test(m.data1$re78[m.data1$treat==1],  # perform paired t-tests on matched data
       m.data1$re78[m.data1$treat==0],paired=TRUE)

#---------------------------------------
#McMillen repeat sales (see source code)
#---------------------------------------


#Identifies repeat sales from a data set with observations on sale price, time of sale, and a property
#id. Returns a data frame in which each observation is a repeat sales pair.
repsaledata(price,timevar,id)

#If some of the original hedonic data set variables need to be included in the 
#repeat sales data set, the original hedonic data set and the
#repsaledata data frame can be merged by the id variable.

#Example:
id <- c(1,1,1, 2,2,2, 3,3,3, 4,4,4, 5,5,5)
timevar <- c(1,2,3, 1,2,2, 3,1,1, 1,1,2, 2,2,3)
price <- seq(1:15)
basedata <- data.frame(id,timevar,price)
basedata
rdata <- repsaledata(price,timevar,id)
rdata

#Standard and Weighted Least Squares Repeat Sales Estimation
repsale(price0,time0,price1,time1,mergefirst=1,
        graph=TRUE,graph.conf=TRUE,conf=.95,
        stage3=FALSE,stage3_xlist=~timesale,print=TRUE)

#Example:
set.seed(189)
n = 2000
# sale dates range from 0-10
# drawn uniformly from all possible time0, time1 combinations with time0<time1
tmat <- expand.grid(seq(0,10), seq(0,10))
tmat <- tmat[tmat[,1]<tmat[,2], ]
tobs <- sample(seq(1:nrow(tmat)),n,replace=TRUE)
time0 <- tmat[tobs,1]
time1 <- tmat[tobs,2]
timesale <- time1-time0
table(timesale)
# constant variance; index ranges from 0 at time 0 to 1 at time 10
y0 <- time0/10 + rnorm(n,0,.2)
y1 <- time1/10 + rnorm(n,0,.2)
fit <- repsale(price0=y0, price1=y1, time0=time0, time1=time1)
repsaledata 
# variance rises with timesale
# var(u0) = .2^2; var(u1) = (.2 + timesale/10)^2
# var(u1-u0) = var(u0) + var(u1) = 2*(.2^2) + .4*timesale/10 + (timesale^2)/100
y0 <- time0/10 + rnorm(n,0,.2)
y1 <- time1/10 + rnorm(n,0,.2+timesale/10)
par(ask=TRUE)
fit <- repsale(price0=y0, price1=y1, time0=time0, time1=time1)
summary(fit$pindex)
fit <- repsale(price0=y0, price1=y1, time0=time0, time1=time1, stage3="abs")
summary(fit$pindex)
timesale2 <- timesale^2
fit <- repsale(price0=y0, price1=y1, time0=time0, time1=time1, stage3="square",
               stage3_xlist=~timesale+timesale2)


##==============##
## DIAGNIOSTICS ##
##==============##

#Check outilers, leverage, influence
#Check regression diagnostics, heteroskedasticity, etc.
# Robustness Tests
# Try themes and materials with grepl


list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                    "nr_works","lnrep","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
modeldata <- subset(artdata, artdata$rank_all<101)
model_100 <- lm(expl_vars, data=modeldata)
summary(model_100)$coefficients
