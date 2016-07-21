##===============================================================================================##
## -------------------------------- ART PRICE INDEX ---------------------------------------------##
##===============================================================================================##

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
suppressMessages(library(mFilter))

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
plot(artplot$Date, artplot$Median_Price, type="l", lwd=2, col="#F8766D", ylim=c(0,10000), ylab="Median Price", xlab=NA, cex.axis=0.8, cex.lab=0.8)
par(new = T)
barplot(artplot$Turnover, col="#00BFC4",  ylim=c(0,600), axes=F, xlab=NA, ylab=NA)
axis(side = 4, cex.axis=0.8)
mtext(side = 4, line = 3, "Turnover (Rm)",cex = 0.8)
par(xpd=TRUE)
legend(5,-90,legend=c("Median Price", "Turnover (Rm)"), lty=c(1,0), pch=c(NA, 15), col=c("#F8766D","#00BFC4"),horiz = TRUE, bty="n", cex=0.8)


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

datum <- levels(artdata$timedummy)

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
naive_indices <- naive_index[,c(1,3,7)]

index_plot <- naive_indices
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


#Plot surface area and prices by medium
artplot <- subset(artdata)
g <- ggplot(artplot, aes(x=lnarea, y=lnprice))
g <- g + geom_point(size = 2, alpha = 0.5, aes(colour = med_code))
g <- g + ylab("log of Price")
g <- g + xlab("log of Area")
g <- g + labs(colour = "Medium")
g <- g + guides(colour = guide_legend(override.aes = list(size=5)))
g

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

#Plot artist reputation index and prices
g <- ggplot(artdata, aes(x=lnrep, y=lnprice))
g <- g + geom_point(size = 2, alpha = 0.5, aes(colour = med_code))
g <- g + ylab("log of Price")
g <- g + xlab("log of Reputation")
g <- g + labs(colour = "Medium")
g <- g + guides(colour = guide_legend(override.aes = list(size=5)))
g

#-------------------
# FULL SAMPLE MODEL
#-------------------
full_model <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                 "nr_works","artist","timedummy")) {
    modeldata <- artdata
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    model_all <- lm(expl_vars, data=modeldata)
    time_results <- summary(model_all)$coefficients[grepl("time", rownames(summary(model_all)$coefficients)),1]
    time_results <- as.data.frame(time_results)
    time_results$index_all <- exp(time_results$time_results)*100
    return(time_results)
}

#-----------------------------
# OVERLAPPING PERIODS (1-year)
#-----------------------------
overlap1y_model <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                      "nr_works","artist","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:16) {
        modeldata <- artdata
        modeldata <- subset(modeldata, modeldata$counter>(i*4-5)& modeldata$counter<(i*4+1))
        model <- lm(expl_vars, data=modeldata)  
        res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
    }
    #Merge all results
    overlap <- rep_results
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
                        13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18)
    for(i in 3:62) {
        j <- overlap[(i+1),20]
        if(is.na(overlap[i,j])) {
            overlap[(i+1),19] <- overlap[i,19]*overlap[(i+1),j]/100
        } else { 
            overlap[(i+1),19] <- overlap[i,19]*overlap[(i+1),j]/overlap[i,j] 
        }   
    }
    colnames(overlap) <- c("Date","Index_Full","Index_m1","Index_m2","Index_m3","Index_m4","Index_m5","Index_m6",
                           "Index_m7","Index_m8","Index_m9","Index_m10","Index_m11","Index_m12","Index_m13",
                           "Index_m14","Index_m15","Index_m16","Index_Adjacent1y","teller")
    overlap$Date <- factor(levels(artdata$timedummy)[-1])
    return(overlap)
}

#-----------------------------
# OVERLAPPING PERIODS (2-year)
#-----------------------------
overlap2y_model <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                      "nr_works","artist","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:8) {
        modeldata <- artdata
        modeldata <- subset(modeldata, modeldata$counter>(i*8-9)& modeldata$counter<(i*8+1))
        model <- lm(expl_vars, data=modeldata)  
        res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
    }
    #Merge all results
    overlap2 <- rep_results
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
                         7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10)
    for(i in 7:62) {
        j <- overlap2[(i+1),12]
        if(is.na(overlap2[i,j])) {
            overlap2[(i+1),11] <- overlap2[i,11]*overlap2[(i+1),j]/100
        } else { 
            overlap2[(i+1),11] <- overlap2[i,11]*overlap2[(i+1),j]/overlap2[i,j] 
        }   
    }
    colnames(overlap2) <- c("Date","Index_Full","Index_m1","Index_m2","Index_m3","Index_m4","Index_m5",
                            "Index_m6","Index_m7","Index_m8","Index_Adj2y","teller")
    overlap2$Date <- factor(levels(artdata$timedummy)[-1])
    return(overlap2)
}

#----------------------
#ROLLING 5-YEAR WINDOWS
#----------------------
rolling_model <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                    "nr_works","artist","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:12) {
        modeldata <- artdata
        modeldata <- subset(modeldata, modeldata$counter>(i*4-4)&modeldata$counter<(i*4+17))
        model <- lm(expl_vars, data=modeldata)  
        summary(model)
        res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
    }
    
    #Merge all results
    rolling <- rep_results
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
                        10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14)
    for(i in 19:62) {  #chaining
        j <- rolling[(i+1),16]
        rolling[(i+1),15] <- rolling[i,15]*rolling[(i+1),j]/rolling[i,j]
    }
    
    colnames(rolling) <- c("Date","Index_Full","Index_m1","Index_m2","Index_m3","Index_m4","Index_m5","Index_m6",
                           "Index_m7","Index_m8","Index_m9","Index_m10","Index_m11","Index_m12","Index_Rolling","teller")
    rolling$Date <- factor(levels(artdata$timedummy)[-1])
    return(rolling)
}

#========================================================================================
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","lnrep","timedummy")

rep_results <- full_model(artdata,list_expl_vars)
suppressMessages(rep_overlap1 <- overlap1y_model(artdata,list_expl_vars))
suppressMessages(rep_overlap2 <- overlap2y_model(artdata,list_expl_vars))
suppressMessages(rep_rolling <- rolling_model(artdata,list_expl_vars))

#--------------------------------------------------------------------------
#Full model for regression results
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","lnrep","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+"))) 
modeldata <- artdata 
model <- lm(expl_vars, data=modeldata) 
stargazer(model, title = "Hedonic Regression results", omit=c("timedummy"), omit.labels = "Quarterly dummies", header=FALSE, single.row = TRUE, type = "latex", cex=0.9)

#---------------------------------------------------------------------------
index_plot <- melt(rep_overlap2[,c(-2,-12)], id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

#----------------------------------------------
hedonic_indices <- rep_overlap1[,c(1,2)]
colnames(hedonic_indices) <- c("Date","Hedonic_full")
hedonic_indices <- cbind(hedonic_indices,Adjacent_1y=rep_overlap1[,19])
hedonic_indices <- cbind(hedonic_indices,Adjacent_2y=rep_overlap2[,11])
hedonic_indices <- cbind(hedonic_indices,Rolling=rep_rolling[,15])
hedonic_indices <- cbind(Date=factor(levels(artdata$timedummy)),
                         rbind(c(seq(100,100, length.out=4)),hedonic_indices[,-1]))

index_plot <- melt(hedonic_indices, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##=====================##
## REPEAT SALES METHOD ##
##=====================##
#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated")]))
allDup <- function(value) {  #identify duplicated values
    duplicated(value) | duplicated(value, fromLast = TRUE)
}
rsartdata <- artdata[allDup(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated")]),]
rsartdata <- transform(rsartdata, id = as.numeric(interaction(artist,factor(title),med_code,factor(area),factor(dum_signed),
                                                              factor(dum_dated), drop=TRUE)))

repdata <- repsaledata(rsartdata$lnprice,rsartdata$counter,rsartdata$id)  #transform the data to sales pairs
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index
RS_index <- exp(as.data.frame(repeatsales$pindex))*100   
RS_index$Date <- levels(rsartdata$timedummy)[c(-1:-3,-11,-15,-19,-29)] #missing values

##=====================##
#do the same but expand it to not match by title - i.e. all other attributes are the same
#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","med_code","area","dum_signed","dum_dated")]))

allDup <- function (value)  { #identify duplicated values
    duplicated(value) | duplicated(value, fromLast = TRUE)
}
rsartdata1 <- artdata[allDup(artdata[,c("artist","med_code","area","dum_signed","dum_dated")]),]
#rsartdata <- transform(rsartdata,id=as.numeric(factor(title)))
rsartdata1 <- transform(rsartdata1, id = as.numeric(interaction(artist,med_code,factor(area),factor(dum_signed),
                                                                factor(dum_dated), drop=TRUE)))

repdata1 <- repsaledata(rsartdata1$lnprice,rsartdata1$counter,rsartdata1$id)    #transform the data to sales pairs
repeatsales <- repsale(repdata1$price0,repdata1$time0,repdata1$price1,repdata1$time1,mergefirst=1,
                       graph=FALSE)                 #generate the repeat sales index
RS_index1 <- exp(as.data.frame(repeatsales$pindex))*100
RS_index1$Date <- levels(rsartdata$timedummy)

##=====================##
#do the same but expand it to not match by title - i.e. all other attributes are the same
#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","med_code","area")]))

allDup <- function (value)  { #identify duplicated values
    duplicated(value) | duplicated(value, fromLast = TRUE)
}
rsartdata2 <- artdata[allDup(artdata[,c("artist","med_code","area")]),]
#rsartdata <- transform(rsartdata,id=as.numeric(factor(title)))
rsartdata2 <- transform(rsartdata2, id = as.numeric(interaction(artist,med_code,factor(area), drop=TRUE)))

repdata2 <- cbind(repsaledata(rsartdata2$lnprice,rsartdata2$counter,rsartdata2$id),
                  repsaledata(rsartdata2$ah_code,rsartdata2$counter,rsartdata2$id)[,4:5],
                  repsaledata(rsartdata2$dum_signed,rsartdata2$counter,rsartdata2$id)[,4:5],
                  repsaledata(rsartdata2$dum_dated,rsartdata2$counter,rsartdata2$id)[,4:5])
colnames(repdata2) <- c("id","time0","time1","price0","price1","ah_code0","ah_code1","sign0","sign1","date0","date1")

repdata2$ah_code0[is.na(repdata2$ah_code0)] <- "Strauss"
repdata2$ah_code1[is.na(repdata2$ah_code1)] <- "Strauss"

dy <- repdata2$price1 - repdata2$price0
dsign <- repdata2$sign1 - repdata2$sign0
ddate <- repdata2$date1 - repdata2$date0
ah0 <- model.matrix(~repdata2$ah_code0)
ah1 <- model.matrix(~repdata2$ah_code1)
dah <- ah1 - ah0

timevar <- levels(factor(c(repdata2$time0, repdata2$time1)))
nt = length(timevar)
n = length(dy)
xmat <- array(0, dim = c(n, nt - 1))
for (j in seq(1 + 1, nt)) {
    xmat[,j-1] <- ifelse(repdata2$time1 == timevar[j], 1, xmat[,j-1])
    xmat[,j-1] <- ifelse(repdata2$time0 == timevar[j],-1, xmat[,j-1])
}
colnames(xmat) <- paste("Time", seq(1 + 1, nt))

ps.RS <- lm(dy ~ dah + dsign + ddate + xmat + 0)
RS_index2 <- summary(ps.RS)$coefficients[grepl("Time", rownames(summary(ps.RS)$coefficients)),1]
RS_index2 <- as.data.frame(RS_index2)
RS_index2$index <- exp(RS_index2$RS_index2)*100
RS_index2$Date <- levels(rsartdata2$timedummy)[-1]
    
#------------------------------------------------------------------------

RS_indices <- merge(RS_index, RS_index1, by="Date", all=TRUE)
RS_indices <- merge(RS_indices, RS_index2, by="Date", all=TRUE)
RS_indices[1,5] <- 100
RS_indices <- RS_indices[,-4]
colnames(RS_indices) <- c("Date","Repeat Sales","pseudo-RS1","pseudo-RS2")       

index_plot <- melt(RS_indices, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##============##
## EVALUATION ##
##============##
all_indices <- cbind(naive_indices,hedonic_indices[-1],RS_indices[-1])
all_indices[is.na(all_indices)]<- 100
    
##Re-weighted to 2000=100
rw_all <- all_indices
for(i in 2:10) {
    rw_all[,i] <- all_indices[,i]/mean(all_indices[1:4,i])*100 
}

index_plot <- melt(all_indices[,c(1,2,5,10)], id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

# Check correlations (in levels and growth rates)
temp_indices <- all_indices
colnames(temp_indices) <- c("Date","Median","Fisher","Hedonic","Adj1y","Adj2y","Roll","RepSale","ps.RS1","ps.RS2")
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices[,-1],start =c(2000,1),end=c(2015,4),frequency=4) 

source("corstarsl.R")
xt <- xtable(corstarsl(ts.all_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))

dl.indices <- as.data.frame(diff(log(ts.all_indices)))
xt <- xtable(corstarsl(dl.indices), caption="Correlations in DLogs")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


##Comparing index smoothness
# Check std dev or volatility en AC(1)
ac.1 <- numeric()
eval <- data.frame()
returns <- dl.indices

vol <- apply(returns,MARGIN=2, FUN=sd, na.rm=TRUE)
for(i in 1:ncol(returns)) {
    ac.1[i] <- acf(returns,na.action = na.pass, plot = FALSE, lag.max = 1)$acf[,,i][2,i]
}

hp <- all_indices
for(i in 2:10) {
    hp[,i] <- hpfilter(all_indices[,i],freq = 1600)[2]
}
ts.hp <- as.ts(hp[,-1],start =c(2000,1),end=c(2015,4),frequency=4)
hpreturns <- as.data.frame(diff(log(ts.hp)))
for(i in 1:ncol(returns)) {
    HPdev[i] <- sum((hpreturns[,i] - returns[,i])^2)
}
eval <- cbind(vol=vol,ac.1=ac.1[1:9],HPdev)
xt <- xtable(eval, caption="Smoothness Indicators")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


##Compared to other art markets
assets <- read.csv("Assets.csv", header=TRUE, na.strings = "", skipNul = TRUE)
##Re-weighted to 2000=100
rw_assets <- assets
for(i in 2:8) {
    rw_assets[,i] <- assets[,i]/mean(assets[1:4,i])*100 
}

index_plot <- cbind(rw_all[,c(1,6,9)],rw_assets[,c(6,7,8)])
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##Compared to other assets
index_plot <- cbind(rw_all[,c(1,6,9)],rw_assets[,c(2,3,4)])
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g 

#==============================#
# Bubbles: Explosive Behaviour
#==============================#
#Make them real
real_indices <- all_indices
for(i in 2:ncol(all_indices)) { 
    for(j in 1:64) {
        real_indices[j,i] <- all_indices[j,i]/assets$CPI[j]*100 
    }
}

#Calculate test statistics
y_indices <- log(real_indices[,-1])
bubble.nc <- list()
bubble.c <- list()
for(i in 1:ncol(y_indices)) {
    bubble1 <- numeric()
    bubble2 <- numeric()
    for(j in 12:64) {
        y <- y_indices[1:j,i]
        toets1 <- ur.df(y, type= "none", lags = 4, selectlags = c("AIC"))
        toets2 <- ur.df(y, type= "drift", lags = 4, selectlags = c("AIC"))
        bubble1 <- rbind(bubble1,toets1@teststat)
        bubble2 <- rbind(bubble2,toets2@teststat)
    }
    bubble.nc[[i]] <- bubble1
    bubble.c[[i]] <- bubble2
}

##--------------------------------------------------------------------------
#Calculate critical values
K1 <- numeric()
K2 <- numeric()
K3 <- numeric()
K4 <- numeric()

for(j in 12:64) {
    set.seed(123)                           #for replicability
    reps <- 2000                            #Monte Carlo replications
    burn <- 100                             #burn in periods: first generate a T+B sample
    #obs <- 62                              #To make "sure" that influence of initial values has faded
    obs <- j                                #ultimate sample size
    tstat.nc <- numeric()
    tstat.c <- numeric()
    tstat.ct <- numeric()
    tstat.lc <- numeric()
    
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
    }                                       
    #hist(tstat.nc)
    K1 <- rbind(K1,quantile(tstat.nc, probs=c(0.9,0.95,0.99)))
    K2 <- rbind(K2,quantile(tstat.c, probs=c(0.9,0.95,0.99)))
}   #Provides a vector of critical values

##---------------------------------------------------------------------------
#plot die test stats en critical values 
bubble.test1 <- numeric()
bubble.test2 <- numeric()

for(k in 1:9) { 
    bubble.test1 <- cbind(bubble.test1,bubble.nc[[k]])
    bubble.test2 <- cbind(bubble.test2,bubble.c[[k]][1:53])
}
bubble.test1 <- as.data.frame(bubble.test1)
bubble.test2 <- as.data.frame(bubble.test2)
bubble.test1 <- cbind(bubble.test1,K1)
bubble.test2 <- cbind(bubble.test2,K2)

Dates <- levels(artdata$timedummy)[-1:-11]
bubble.test1$Date <- Dates
bubble.test2$Date <- Dates

colnames(bubble.test1)[1:9] <- c("Median","Fisher","Hedonic","Adj1y","Adj2y","Roll","RepSale","ps.RS1","ps.RS2")
colnames(bubble.test2)[1:9] <- c("Median","Fisher","Hedonic","Adj1y","Adj2y","Roll","RepSale","ps.RS1","ps.RS2")

index_plot <- bubble.test1[,c(1,4,9,11,12,13)]
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line(aes(linetype=variable))
g <- g + scale_linetype_manual(values = c(1,1,1,4,4))
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- bubble.test2[,c(1,4,9,11,12,13)]
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line(aes(linetype=variable))
g <- g + scale_linetype_manual(values = c(1,1,1,4,4))
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

#----------------------------------------------------------------------------------------------------------------------------
#report  bubble period dates
datum1 <- data.frame()
datum2 <- data.frame()
datums1 <- data.frame()
datums2 <- data.frame()

bubble.test1 <- bubble.test1[,c(-1,-7)]
bubble.test2 <- bubble.test2[,c(-1,-7)]

for(i in 1:7) {
    for(l in 1:53) {
        if(bubble.test1[l,i]>bubble.test1$"95%"[l]) { 
            datum1[l,i] <- bubble.test1[l,"Date"]
        }
        if(bubble.test2[l,i]>bubble.test2$"95%"[l]) { 
            datum2[l,i] <- bubble.test2[l,"Date"]
        }
    }
    NonNAindex <- which(!is.na(datum1[,i]))
    firstNonNA <- min(NonNAindex)
    datums1[1,i] <- datum1[firstNonNA,i]
    if (NonNAindex[NROW(NonNAindex)-1]==(max(NonNAindex)-1)) { 
        lastNonNA <- max(NonNAindex)
    } else lastNonNA <- NonNAindex[NROW(NonNAindex)-1]
    datums1[2,i] <- datum1[lastNonNA,i]
    
    NonNAindex <- which(!is.na(datum2[,i]))
    firstNonNA <- min(NonNAindex)
    datums2[1,i] <- datum2[firstNonNA,i]
    lastNonNA <- max(NonNAindex)
    datums2[2,i] <- datum2[lastNonNA,i]
}    

datums <- rbind(datums1,datums2)
colnames(datums) <- colnames(bubble.test1)[1:7]
rownames(datums) <- c("None-Start","None-End","Drift-Start","Drift-End")
datums <- t(datums)

xt <- xtable(datums, caption="Dates of explosive behaviour")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))



