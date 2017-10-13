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

datums <- read.csv("datums.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

assets <- read.csv("Assets.csv", header=TRUE, na.strings = "", skipNul = TRUE)

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
#artdata$timedummy2 <- factor(as.yearmon(artdata$date, "%Y/%m/%d"))
#artdata$counter2 <- as.numeric(artdata$timedummy2)


##----------------------
##Rank Artists by Volume
##----------------------
#Rank by Total Volume (all)
rankings <- count(artdata, artist)
#rankings <- rankings[order(rankings$n, decreasing = TRUE),]
rankings$rank_all <- dense_rank(desc(rankings$n))    #rank by density, with no gaps (ties = equal)
rankings$rank_total <- row_number(desc(rankings$n))  #equivalent to rank(ties.method = "first")
rankings$n <- NULL

#artdata <- merge(artdata, rankings, by.x="artist", by.y="artist",all.x=TRUE)



#-------------------------------
#Plot total sales by auction house
artplot <- aggregate(artdata$hammer_price, by=list(artdata$timedummy,artdata$ah_code), FUN = length)
artplot$Group.1 <- as.Date(as.yearqtr(artplot$Group.1, format = "%Y Q%q"))
g <- ggplot(artplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Auction House")
g <- g + scale_y_continuous(labels=comma)
g <- g + ylab("Total Number of Sales (Auction Lots)")
g <- g + xlab(" ")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


#boxplot hammer prices (in three periods)
artplot <- artdata[,c("year","lnprice")]
artplot$period <- factor(artplot$year)
g <- ggplot(artplot, aes(x=period, y=lnprice, fill="#00BFC4"))
g <- g + stat_boxplot(geom = "errorbar", width = 0.5) 
g <- g + geom_boxplot() + guides(fill=FALSE) 
g <- g + ylab("log of Hammer Prices")
g <- g + xlab("")
g 


#dev.copy(png,"area.png")
#png(filename = "area2.png", width = 600, height = 360)
#Plot surface area and prices by medium
artplot <- subset(artdata, med_code!="NA")
g <- ggplot(artplot, aes(x=lnarea, y=lnprice))
g <- g + geom_point(size = 2, alpha = 0.5, aes(colour = med_code))
g <- g + ylab("log of Price")
g <- g + xlab("log of Surface Area")
g <- g + labs(colour = "Medium")
g <- g + guides(colour = guide_legend(override.aes = list(size=5)))
g
#dev.off()

#library(png)
#library(grid)
#grid.raster(readPNG("area2.png"))



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

write.csv(artdata,"artdata_lnrep2.csv")

#------------------------------------------------------------------------------------------------------------------------------
#Load pre-calculated dataset (for speed) from API_3.R
artdata <- read.csv("artdata_lnrep.csv", header=TRUE)
artdata$med_code <- factor(artdata$med_code, labels=c("NA","Drawing", "Watercolour", "Oil", "Acrylic", "Print/Woodcut",
                                                      "Mixed Media","Sculpture","Photography", "Other"))

#Or:
artdata <- read.csv("artdata_lnrep2.csv", header=TRUE)
artdata$med_code <- factor(artdata$med_code, labels=c("Acrylic","Drawing","Mixed Media","Oil","Other",        
                                                      "Photography","Print/Woodcut","Sculpture","Watercolour"))

#The result: index of average price per artist adjusted for quality, relative to the base artist 
#It can replace the artist dummies as a continuous variable in a second regression of equation 1 

##----------------------
##Rank Artists by Value
value <- aggregate(artdata$hammer_price, by=list(artdata$artist), FUN = sum)
volume <- aggregate(artdata$hammer_price, by=list(artdata$artist), FUN = length)
value <- cbind(value,volume[,2],value$x/volume$x)    
colnames(value) <- c("artist","value","volume","ave_price")

rankings <- count(artdata, artist)
#rankings <- rankings[order(rankings$n, decreasing = TRUE),]
rankings$rank_all <- dense_rank(desc(rankings$n))    #rank by density, with no gaps (ties = equal)
rankings$rank_total <- row_number(desc(rankings$n))  #equivalent to rank(ties.method = "first")
rankings$n <- NULL

rankings$value <- row_number(desc(value$value)) 
rankings$volume <- row_number(desc(value$volume))
rankings$ave_price <- row_number(desc(value$ave_price))

artdata <- merge(artdata, rankings[,c(1,4,6)], by.x="artist", by.y="artist",all.x=TRUE)

##----------------------

#dev.copy(png,"area.png")
png(filename = "reputation.png", width = 600, height = 360)
#Plot artist reputation index and prices
artplot <- subset(artdata, med_code!="NA")
g <- ggplot(artplot, aes(x=lnrep, y=lnprice))
g <- g + geom_point(size = 2, alpha = 0.5, aes(colour = med_code))
g <- g + ylab("log of Price")
g <- g + xlab("log of Artist Reputation")
g <- g + labs(colour = "Medium")
g <- g + guides(colour = guide_legend(override.aes = list(size=5)))
g
dev.off()

library(png)
library(grid)
grid.raster(readPNG("reputation.png"))


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
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

rm(ketting2,naive_index,strat_p,strat_q)

##-------------##
##---HEDONIC---##
##-------------##
#-------------------
# FULL SAMPLE MODEL
#-------------------
full_model <- function(data, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                              "nr_works","artist","lnarea:med_code","timedummy")) {
    modeldata <- data
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    model_all <- lm(expl_vars, data=modeldata)
    time_results <- summary(model_all)$coefficients[grepl("timedummy", rownames(summary(model_all)$coefficients)),1]
    time_results <- as.data.frame(time_results)
    time_results$Index <- exp(time_results$time_results)*100
    time_results$Date <- sub("timedummy","",row.names(time_results))
    time_results <- merge(datums,time_results,by="Date",all=TRUE)[,c(1,4)]
    return(time_results)
}

#data <- artdata
#-----------------------------
# OVERLAPPING PERIODS (1-year)
#-----------------------------
overlap1y_model <- function(data, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                   "nr_works","artist","lnarea:med_code","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:16) {
        modeldata <- data
        modeldata <- subset(modeldata, modeldata$counter>(i*4-5)& modeldata$counter<(i*4+1))
        model <- lm(expl_vars, data=modeldata)  
        time_results <- as.data.frame(summary(model)$coefficients[grepl("timedummy", rownames(summary(model)$coefficients)),1])
        time_results$Date <- sub("timedummy","",row.names(time_results))
        colnames(time_results) <- c("Coef","Date")
        res_list[[i]] <- time_results
    }
    #Merge all results
    overlap <- rep_results
    overlap <- merge(overlap, res_list[[1]], by="Date", all=TRUE)
    overlap[,3] <- exp(overlap[,3])*100
    for(i in 2:16) {
        overlap <- merge(overlap, res_list[[i]], by = "Date",all=TRUE)
        overlap[,(i+2)] <- exp(overlap[i+2])*100
    } 
    #Calculate index
    overlap$ind <- overlap[,3]
    overlap[2,19] <- overlap[3,19]*overlap[2,2]/overlap[3,2]   #Interpolate
    overlap$teller <- as.numeric(substring(overlap$Date,1,4))-1997
    
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
                                                      "nr_works","artist","lnarea:med_code","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:8) {
        modeldata <- artdata
        modeldata <- subset(modeldata, modeldata$counter>(i*8-9)& modeldata$counter<(i*8+1))
        model <- lm(expl_vars, data=modeldata)  
        time_results <- as.data.frame(summary(model)$coefficients[grepl("timedummy", rownames(summary(model)$coefficients)),1])
        time_results$Date <- sub("timedummy","",row.names(time_results))
        colnames(time_results) <- c("Coef","Date")
        res_list[[i]] <- time_results
    }
    #Merge all results
    overlap2 <- rep_results
    overlap2 <- merge(overlap2, res_list[[1]], by="Date", all=TRUE)
    overlap2[,3] <- exp(overlap2[,3])*100
    for(i in 2:8) {
        overlap2 <- merge(overlap2, res_list[[i]], by = "Date", all=TRUE)
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
                                                    "nr_works","artist","lnarea:med_code","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:12) {
        modeldata <- artdata
        modeldata <- subset(modeldata, modeldata$counter>(i*4-4)&modeldata$counter<(i*4+17))
        model <- lm(expl_vars, data=modeldata)  
        time_results <- as.data.frame(summary(model)$coefficients[grepl("timedummy", rownames(summary(model)$coefficients)),1])
        time_results$Date <- sub("timedummy","",row.names(time_results))
        colnames(time_results) <- c("Coef","Date")
        res_list[[i]] <- time_results    
    }
    
    #Merge all results
    rolling <- rep_results
    rolling <- merge(rolling, res_list[[1]], by="Date", all=TRUE)
    rolling[,3] <- exp(rolling[,3])*100
    for(i in 2:12) {
        rolling <- merge(rolling, res_list[[i]], by = "Date", all=TRUE)
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
list_expl_vars <- c("lnarea","ah_code","med_code","dum_signed","dum_dated",  
                    "nr_works","lnrep","lnarea:med_code","timedummy")
rep_results <- full_model(artdata,list_expl_vars)
suppressMessages(rep_overlap1 <- overlap1y_model(artdata,list_expl_vars))
suppressMessages(rep_overlap2 <- overlap2y_model(artdata,list_expl_vars))
suppressMessages(rep_rolling <- rolling_model(artdata,list_expl_vars))


#list_expl_vars2 <- c("lnarea","lnarea2","ah_code","med_code","dum_signed","dum_dated",  
#                    "nr_works","lnrep","lnarea:med_code","lnarea*lnrep","lnrep:med_code","timedummy")
#rep_results2 <- full_model(artdata,list_expl_vars2)
#----------------------------------------------
hedonic_indices <- rep_overlap1[,c(1,2)]
colnames(hedonic_indices) <- c("Date","Hedonic_full")
hedonic_indices <- cbind(hedonic_indices,Adjacent_1y=rep_overlap1[,19])
hedonic_indices <- cbind(hedonic_indices,Adjacent_2y=rep_overlap2[,11])
hedonic_indices <- cbind(hedonic_indices,Rolling_5y=rep_rolling[,15])
hedonic_indices <- cbind(Date=factor(levels(artdata$timedummy)),
                         rbind(c(seq(100,100, length.out=4)),hedonic_indices[,-1]))


#--------------------------------------------------------------------------
#Full model for regression results
list_expl_vars <- c("lnarea","ah_code","med_code","dum_signed","dum_dated",  
                    "nr_works","lnrep","lnarea:med_code","timedummy")

expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+"))) 
modeldata <- artdata 
model2 <- lm(expl_vars, data=modeldata) 
stargazer(model2, title = "Hedonic Regression results", omit=c("lnarea:med_code","timedummy"),
          omit.labels = c("Medium Size Interactions","Quarterly dummies"), header=FALSE, single.row = TRUE, type = "latex", 
          table.placement = "!h")

#---------------------------------------------------------------------------
index_plot <- melt(rep_overlap2[,c(-2,-12)], id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- melt(hedonic_indices, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.position="bottom") + theme(legend.title=element_blank())
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

rm(rep_overlap1,rep_overlap2,rep_rolling)

##=====================##
## REPEAT SALES METHOD ##
##=====================##
#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated","nr_works")]))
allDup <- function(value) {  #identify duplicated values
    duplicated(value) | duplicated(value, fromLast = TRUE)
}
rsartdata <- artdata[allDup(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated","nr_works")]),]
rsartdata <- transform(rsartdata, id = as.numeric(interaction(artist,factor(title),med_code,factor(area),factor(dum_signed),
                                                              factor(dum_dated), factor(nr_works),drop=TRUE)))

repdata <- repsaledata(rsartdata$lnprice,rsartdata$counter,rsartdata$id)  #transform the data to sales pairs
repdata <- repdata[complete.cases(repdata),]
repeatsales <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                       graph=FALSE)   #generate the repeat sales index
RS_index <- exp(as.data.frame(repeatsales$pindex))*100   

n <- as.numeric(sub("Time ","",row.names(RS_index)))
n[1] <- 1
RS_index$Date <- levels(rsartdata$timedummy)[unique(c(repdata$time0,repdata$time1))[order(unique(c(repdata$time0,repdata$time1)))]][n] #missing values

RS_index <- merge(datums,RS_index, by="Date", all=TRUE)[,-2]
colnames(RS_index) <- c("Date","Repeat Sales_Index")       

index_plot <- melt(RS_index, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

##------------------------------------------------------------------
##--------------- Pseudo Repeat Sales ------------------------------
##------------------------------------------------------------------
#do the same but expand it to not match by title - i.e. all other attributes are the same
#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","med_code","area","dum_signed","dum_dated","nr_works")]))
rsartdata1 <- artdata[allDup(artdata[,c("artist","med_code","area","dum_signed","dum_dated","nr_works")]),]
rsartdata1 <- transform(rsartdata1, id = as.numeric(interaction(artist,med_code,factor(area),factor(dum_signed),
                                                                factor(dum_dated),factor(nr_works), drop=TRUE)))

repdata1 <- cbind(repsaledata(rsartdata1$lnprice,rsartdata1$counter,rsartdata1$id),
                  repsaledata(rsartdata1$ah_code,rsartdata1$counter,rsartdata1$id)[,4:5]) #transform the data to sales pairs
repdata1 <- repdata1[complete.cases(repdata1),]
colnames(repdata1) <- c("id","time0","time1","price0","price1","ah_code0","ah_code1")

dy <- repdata1$price1 - repdata1$price0
ah0 <- model.matrix(~repdata1$ah_code0)
ah1 <- model.matrix(~repdata1$ah_code1)
dah <- ah1 - ah0

timevar <- levels(factor(c(repdata1$time0, repdata1$time1)))
nt = length(timevar)
n = length(dy)
xmat <- array(0, dim = c(n, nt - 1))
for (j in seq(1 + 1, nt)) {
    xmat[,j-1] <- ifelse(repdata1$time1 == timevar[j], 1, xmat[,j-1])
    xmat[,j-1] <- ifelse(repdata1$time0 == timevar[j],-1, xmat[,j-1])
}
colnames(xmat) <- paste("Time", seq(1 + 1, nt))

ps.RS <- lm(dy ~ dah + xmat + 0)
RS_index1 <- summary(ps.RS)$coefficients[grepl("Time", rownames(summary(ps.RS)$coefficients)),1]
RS_index1 <- as.data.frame(RS_index1)
RS_index1$index <- exp(RS_index1$RS_index1)*100
RS_index1$Date <- levels(rsartdata1$timedummy)[-1]
RS_index1 <- RS_index1[,c(2,3)]

##=====================##
#do the same but expand it to not match by title or authenticity dummies
#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","med_code","area","nr_works")]))
rsartdata2 <- artdata[allDup(artdata[,c("artist","med_code","area","nr_works")]),]
rsartdata2 <- transform(rsartdata2, id = as.numeric(interaction(artist,med_code,factor(area),factor(nr_works), drop=TRUE)))

repdata2 <- cbind(repsaledata(rsartdata2$lnprice,rsartdata2$counter,rsartdata2$id),
                  repsaledata(rsartdata2$ah_code,rsartdata2$counter,rsartdata2$id)[,4:5],
                  repsaledata(rsartdata2$dum_signed,rsartdata2$counter,rsartdata2$id)[,4:5],
                  repsaledata(rsartdata2$dum_dated,rsartdata2$counter,rsartdata2$id)[,4:5])
repdata2 <- repdata2[complete.cases(repdata2),]
colnames(repdata2) <- c("id","time0","time1","price0","price1","ah_code0","ah_code1","sign0","sign1","date0","date1")

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
RS_index2 <- RS_index2[,c(2,3)]

#------------------------------------------------------------------------
RS_indices <- merge(RS_index, RS_index1, by="Date", all=TRUE)
RS_indices <- merge(RS_indices, RS_index2, by="Date", all=TRUE)
RS_indices <- cbind(Date=factor(levels(artdata$timedummy)),
                    rbind(c(seq(100,100, length.out=3)),RS_indices[,-1]))
colnames(RS_indices) <- c("Date","Repeat Sales","pseudo-RS1","pseudo-RS2")    

#maak_indeks <- function(indeks) {
#    indeks[,2] <- indeks[,2]/mean(indeks[1:4,2],na.rm=TRUE)*100
#    indeks[,3] <- indeks[,3]/mean(indeks[1:4,3],na.rm=TRUE)*100
#    indeks[,4] <- indeks[,4]/mean(indeks[1:4,4],na.rm=TRUE)*100
#    return(indeks)
#}
#RS_indices1 <- maak_indeks(RS_indices)

#------------------------------------------------------------------------
index_plot <- melt(RS_indices[,-2], id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

rm(ah0,ah1,dah,xmat,repdata,repdata1,repdata2,RS_index,RS_index1,RS_index2)
rm(rsartdata,rsartdata1,rsartdata2)

##============##
## EVALUATION ##
##============##
all_indices <- cbind(naive_indices,hedonic_indices[-1],RS_indices[-1])
all_indices[is.na(all_indices)]<- 100

#Load pre-calculated indices
#write.csv(all_indices, "all_indices.csv")
#all_indices <- read.csv("all_indices.csv", header=TRUE, na.strings = "", skipNul = TRUE)[,-1]

index_plot <- melt(all_indices[,c(1,2,5,10)], id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line(aes(linetype=variable))
g <- g + scale_linetype_manual(values = c(4,1,2))
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

# Check correlations (in growth rates)
source("corstarsl.R")
temp_indices <- all_indices[-1:-3,]
colnames(temp_indices) <- c("Date","Median","Fisher","Hedonic","Adj1y","Adj2y","Roll","RepSale","ps.RS1","ps.RS2")
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices[,-1],start =c(2000,4),end=c(2015,4),frequency=4) 
#xt <- xtable(corstarsl(ts.all_indices), caption="Correlations in Levels")
#print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))
returns <- as.data.frame(diff(log(ts.all_indices)))
xt <- xtable(corstarsl(returns), caption="Correlations in DLogs")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))

#check <- corstarsl(returns)
##Comparing index smoothness
# Check std dev or volatility en AC(1)
ac.1 <- numeric()
eval <- data.frame()
HPdev <- numeric()
smoothness <- numeric()

vol <- apply(returns,MARGIN=2, FUN=sd, na.rm=TRUE)
for(i in 1:ncol(returns)) {
    ac.1[i] <- acf(returns,na.action = na.pass, plot = FALSE, lag.max = 1)$acf[,,i][2,i]
}

hp <- temp_indices
for(i in 2:10) {
    hp[,i] <- hpfilter(temp_indices[,i],freq = 1600)[2]
}
ts.hp <- as.ts(hp[,-1],start =c(2000,1),end=c(2015,4),frequency=4)
hpreturns <- as.data.frame(diff(log(ts.hp)))
for(i in 1:ncol(returns)) {
    HPdev[i] <- sum((hpreturns[,i] - returns[,i])^2)
}

#spectral density
smooth <- function (datavec,k,l) {     # calculates smoothness coefficient for 'datavec' with 
    # 'k' specifies the width of the Daniell window which smooths the raw periodogram 
    
    ## Step 1: Calculate and record power spectral density using 'speccalcs'
    speccalcs <- spec.pgram(datavec,spans=c(k,l),demean=TRUE,plot=FALSE)
    spectra <- speccalcs$spec
    
    ## Step 2 Take natural logs of power spectral frequencies
    logspec <- log(spectra)
    n <- length(logspec)
    m <- n/2
    
    p1 <- mean(logspec[1:m])
    p2 <- mean(logspec[(m+1):n])
    
    smcoef <- p1-p2
    smcoefvar <- (pi^2)/6*((1/m)+(1/(n-m)))
    smcoefse <- sqrt(smcoefvar)
    #list(smcoef,smcoefse)
    return(smcoef)
}
for(i in 1:10) { smoothness[i] <- smooth(all_indices[,i],3,3) }

eval <- cbind(vol=vol,ac.1=ac.1[1:9],HPdev,smoothness=smoothness[-1])
xt <- xtable(eval, caption="Smoothness Indicators")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


##Compared to other art markets
index_plot <- cbind(all_indices[,c(1,5,10)],assets[,c(6,7,8)])
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


##Compared to other assets
index_plot <- cbind(all_indices[,c(1,5,10)],assets[,c(2,3,4)])
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g 


source("corstarsl.R")

all_assets <- cbind(all_indices[,c(5,10)],assets[,c(2,3,4,6,7,8)])
colnames(all_assets) <- c("SA.Art_Adj1y","SA.Art_ps.RS2","SA.Bonds","SA.Equity","SA.Property","US.Art","UK.Art","French.Art")
ts.all_assets <- as.ts(all_assets,start =c(2000,4),end=c(2015,4),frequency=4) 
#xt <- xtable(corstarsl(ts.all_assets), caption="Correlations in Levels")
#print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))
dl.all_assets <- as.data.frame(diff(log(ts.all_assets)))

xt1 <- corstarsl(dl.all_assets[,1:5])[,1:2]
xt2 <- corstarsl(dl.all_assets[,c(1:2,6:8)])[,1:2]
xt1 <- rbind(xt1,xt2)
xt1 <- xt1[c(-1:-2,-6:-7),]

#xt1 <- cbind(xt1,c("SA.Art_Adj1y","SA.Art_ps.RS2","US.Art","UK.Art","French.Art"),xt2)
#colnames(xt1)[5] <- " "

xt2 <- xt2[-1,]
xt <- xtable(xt1, caption="Correlations of returns (growth rates)")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.75)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, scalebox = 0.75)


xt1 <- cbind(corstarsl(manufac[,-1]),corstarsl(construct[,-1]))
xt2 <- cbind(corstarsl(trade[,-1]),corstarsl(services[,-1]))

xt2[3,4:5] <- ""
xt2[4,6] <- ""
xt1[1,] <- c("Current","Expected","BER_BCI","Current","Expected","BER_BCI")
xt2[1,] <- c("Current","Expected","BER_BCI","Current","Expected","BER_BCI")
colnames(xt1) <- c(" ","Manufacturing"," "," ","Construction"," ")
colnames(xt2) <- c(" ","Trade"," "," ","Services"," ")
row.names(xt1) <- c(" ","Expected","BER_BCI","RGDP_Growth")
row.names(xt2) <- c(" ","Expected","BER_BCI","RGDP_Growth")

#========================================
#Segments
#========================================
summary(artdata$price)

#Segment by prices
source("full_model.R")

list_expl_vars <- c("lnarea","ah_code","med_code","dum_signed","dum_dated",  
                    "nr_works","artist","med_code:lnarea","timedummy")
model4.1 <- full_model_a(artdata[artdata$price<=2400,],list_expl_vars) 
model4.2 <- full_model_a(artdata[artdata$price>2400 & artdata$price<22000,],list_expl_vars) 
model4.3 <- full_model_a(artdata[artdata$price>=22000,],list_expl_vars) 

pr_seg <- cbind(model4.1[,c(1,2)],model4.2[,2],model4.3[,2])
colnames(pr_seg) <- c("Date", "Lower", "Middle", "Upper")
pr_seg <- cbind(Date=factor(levels(artdata$timedummy)),
                rbind(c(seq(100,100, length.out=2)),pr_seg[,-1]))

#---------------------------------------------------
summary(artdata$ave_price)
source("full_model.R")
list_expl_vars <- c("lnarea","ah_code","med_code","dum_signed","dum_dated",  
                    "nr_works","artist","med_code:lnarea","timedummy")
model4.1 <- full_model_a(artdata[artdata$ave_price>=1693,],list_expl_vars) 
model4.2 <- full_model_a(artdata[artdata$ave_price>262 & artdata$ave_price<1693,],list_expl_vars) 
model4.3 <- full_model_a(artdata[artdata$ave_price<=262,],list_expl_vars) 

ave_seg <- cbind(model4.1,model4.2[,2],model4.3[,2])
colnames(ave_seg) <- c("Date", "Lower", "Middle", "Upper")

ave_seg <- cbind(Date=factor(levels(artdata$timedummy)),
                 rbind(c(seq(100,100, length.out=2)),ave_seg[,-1]))

#---------------------------------------------------

list_expl_vars <- c("lnarea","ah_code","dum_signed","dum_dated","nr_works",
                    "artist","timedummy")
drawing     <- full_model_a(subset(artdata, artdata$med_code=="Drawing"),list_expl_vars)
watercolour <- full_model_a(subset(artdata, artdata$med_code=="Watercolour"),list_expl_vars)
oil         <- full_model_a(subset(artdata, artdata$med_code=="Oil"),list_expl_vars)
acrylic     <- full_model_a(subset(artdata, artdata$med_code=="Acrylic"),list_expl_vars)
print       <- full_model_a(subset(artdata, artdata$med_code=="Print/Woodcut"),list_expl_vars)
mixed       <- full_model_a(subset(artdata, artdata$med_code=="Mixed Media"),list_expl_vars)
sculpture   <- full_model_a(subset(artdata, artdata$med_code=="Sculpture"),list_expl_vars)
photo       <- full_model_a(subset(artdata, artdata$med_code=="Photography"),list_expl_vars)
other       <- full_model_a(subset(artdata, artdata$med_code=="Other"),list_expl_vars)

l <- list(drawing,watercolour,oil,acrylic,print,mixed,sculpture,photo,other)
l <- lapply(l, function(x) data.frame(x, rn = row.names(x)))
mediums <- merge(l[1], l[2], by="rn", all=TRUE) # merge by row names (by=0 or by="row.names")
for(i in 3:9) { mediums <- merge(mediums, l[i], by="rn", all=TRUE)}
mediums <- mediums[,c(-1,-2,-4,-6,-8,-10,-12,-14,-16,-18)]
names(mediums) <- c("drawing","watercolour","oil","acrylic","print","mixed","sculpture","photo","other")
mediums <- rbind(c(100,100,100,100,100,100,100,100,100),mediums)
mediums <- cbind(mediums,hedonic_indices[,1])

write.csv(mediums,"mediums.csv")
#---------------------------------------------------

list_expl_vars <- c("lnarea","ah_code","med_code","dum_signed","dum_dated",  
                    "nr_works","lnrep","med_code:lnarea","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
quant <- rq(expl_vars, tau=c(0.25,0.5,0.75), data=artdata)
quant_results <- coef(quant)[grepl("time", rownames(coef(quant))),1:3]
quant_results <- as.data.frame(quant_results)
quant_results <- exp(quant_results)*100
quant_results[,4] <- hedonic_indices[-1,1]
write.csv(quant_results,"quant_results.csv")


#---------------------------------------------------
mediums <- read.csv("mediums.csv", header=TRUE, sep=",",na.strings = "NA", skipNul = TRUE)

quant_results <- read.csv("quant_results.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)[,c(5,2,3,4)]
colnames(quant_results) <- c("Date","tau=0.25","tau=0.50","tau=0.75")
quant_results <- cbind(Date=factor(levels(artdata$timedummy)),
                       rbind(c(seq(100,100, length.out=2)),quant_results[,-1]))

#---------------------------------------------------
index_plot <- pr_seg
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Segmented by Price") 
g1 <- g1 + ylab("Index") + xlab("")
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.margin=unit(-0.2, "cm"))

index_plot <- ave_seg
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Segmented by Artist Value") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.margin=unit(-0.2, "cm"))

index_plot <- mediums[,c(1:4,6,7)]
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Segmented by Medium") 
g3 <- g3 + ylab("Index") + xlab("")
g3 <- g3 + theme(legend.text=element_text(size=8),legend.position="bottom")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.margin=unit(-0.2, "cm"))

index_plot <- quant_results
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
index_plot <- melt(index_plot, id="Date")  # convert to long format
g4 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Quantile Regressions") 
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="bottom")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g4 <- g4 + theme(legend.margin=unit(-0.2, "cm"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2) 


#=====================================
#Individual Artists
#=====================================
list_expl_vars <- c("lnarea","ah_code","med_code","lnarea:med_code","dum_signed","dum_dated",  
                    "nr_works","timedummy")
data <- artdata[artdata$counter>0,]
battiss <- full_model(data[data$artist=="Battiss, Walter Whall",],list_expl_vars)
boon <- full_model(data[data$artist=="Boonzaier, Gregoire Johannes",],list_expl_vars)
pier <- full_model(data[data$artist=="Pierneef, Jacob Hendrik",],list_expl_vars)
kent <- full_model(data[data$artist=="Kentridge, William Joseph",],list_expl_vars)
stern <- full_model(data[data$artist=="Stern, Irma",],list_expl_vars)
coet <- full_model(data[data$artist=="Coetzee, Christo",],list_expl_vars)
maggie <- full_model(data[data$artist=="Laubser, Maggie (Maria Magdalena)",],list_expl_vars)

ps.repsales2 <- function(data) {
    #do the same but expand it to not match by title or authenticity dummies
    sum(duplicated(data[,c("artist","med_code","area","nr_works")]))
    rsdata2 <- data[allDup(data[,c("artist","med_code","area","nr_works")]),]
    rsdata2 <- transform(rsdata2, id = as.numeric(interaction(artist,med_code,factor(area),factor(nr_works), drop=TRUE)))
    
    repdata2 <- cbind(repsaledata(rsdata2$lnprice,rsdata2$counter,rsdata2$id),
                      repsaledata(rsdata2$ah_code,rsdata2$counter,rsdata2$id)[,4:5],
                      repsaledata(rsdata2$dum_signed,rsdata2$counter,rsdata2$id)[,4:5],
                      repsaledata(rsdata2$dum_dated,rsdata2$counter,rsdata2$id)[,4:5])
    repdata2 <- repdata2[complete.cases(repdata2),]
    colnames(repdata2) <- c("id","time0","time1","price0","price1","ah_code0","ah_code1","sign0","sign1","date0","date1")
    
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
    n <- as.numeric(sub("xmatTime ","",row.names(RS_index2)))
    
    RS_index2$Date <- levels(rsdata2$timedummy)[unique(c(repdata2$time0,repdata2$time1))[order(unique(c(repdata2$time0,repdata2$time1)))]][n]
    RS_index2 <- RS_index2[,c(2,3)]
    RS_indices <- merge(datums, RS_index2, by="Date", all=TRUE)[,-2]
    return(RS_indices)
    
}

battiss <- cbind(battiss, ps.repsales2(data[data$artist=="Battiss, Walter Whall",])[,2])
boon <- cbind(boon, ps.repsales2(data[data$artist=="Boonzaier, Gregoire Johannes",])[,2])
pier <- cbind(pier, ps.repsales2(data[data$artist=="Pierneef, Jacob Hendrik",])[,2])
kent <- cbind(kent, ps.repsales2(data[data$artist=="Kentridge, William Joseph",])[,2])
stern <- cbind(stern, ps.repsales2(data[data$artist=="Stern, Irma",])[,2])
coet <- cbind(coet, ps.repsales2(data[data$artist=="Coetzee, Christo",])[,2])
maggie <- cbind(maggie, ps.repsales2(data[data$artist=="Laubser, Maggie (Maria Magdalena)",])[,2])


maak_indeks <- function(indeks) {
    indeks <- cbind(Date=factor(levels(artdata$timedummy)),
                    rbind(c(seq(100,100, length.out=2)),indeks[,-1]))
    indeks[,2] <- indeks[,2]/mean(indeks[1:4,2],na.rm=TRUE)*100
    indeks[,3] <- indeks[,3]/mean(indeks[1:4,3],na.rm=TRUE)*100
    colnames(indeks) <- c("Date","Hedonic Index","ps-RS Index")
    return(indeks)
}

battiss <- maak_indeks(battiss)
boon <- maak_indeks(boon)
pier <- maak_indeks(pier)
kent <- maak_indeks(kent)
stern <- maak_indeks(stern)
coet <- maak_indeks(coet)
maggie <- maak_indeks(maggie)

#------------------------------------------------------------------------
index_plot <- battiss[,-3]
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"), frac = 1)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
#g1 <- g1 + geom_point(size = 0.5) 
g1 <- g1 + geom_line(colour="#F8766D")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Battiss Index") 
g1 <- g1 + ylab("Index") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- boon[,-3]
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"), frac = 1)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line(colour="#7CAE00")
#g2 <- g2 + geom_point(size = 0.5) 
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Boonzaier Index") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- pier[,-3]
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"), frac = 1)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line(colour="#00BFC4")
#g3 <- g3 + geom_point(size = 0.5) 
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Pierneef Index") 
g3 <- g3 + ylab("Index") + xlab("")
g3 <- g3 + theme(legend.position="bottom")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- stern[,-3]
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"), frac = 1)
index_plot <- melt(index_plot, id="Date")  # convert to long format
g4 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g4 <- g4 + geom_line(colour="#C77CFF")
#g4 <- g4 + geom_point(size = 0.5) 
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Stern Index") 
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="bottom")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#==============================#
# Bubbles: Explosive Behaviour
#==============================#
#Make them real
make_real <- function(indeks) {
    for(i in 2:ncol(indeks)) { 
        for(j in 1:64) {
            indeks[j,i] <- indeks[j,i]/assets$CPI[j]*100 
        }
    }
    return(indeks)
}
real_indices <- make_real(all_indices)

index_plot <- melt(real_indices[,c(-3,-8)], id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"), frac = 1)
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Index") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g 

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

colnames(bubble.test1)[1:9] <- colnames(all_indices[-1])
colnames(bubble.test2)[1:9] <- colnames(all_indices[-1])

index_plot <- bubble.test1[,c(1,4,9,11,12,13)]
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 0.8) 
g <- g + geom_line(aes(linetype=variable))
g <- g + scale_linetype_manual(values = c(6,1,5,3,3))
g <- g + ylab("") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

index_plot <- bubble.test2[,c(1,4,9,11,12,13)]
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"))
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line(aes(linetype=variable))
g <- g + scale_linetype_manual(values = c(1,1,1,1,1,4,4))
g <- g + ylab("") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

sup <- data.frame()
for(i in 1:9) {
    sup[i,1] <- max(bubble.test1[,i])
    sup[i,2] <- max(bubble.test2[,i])
}
sup[10,1] <- bubble.test1[53,11]
sup[11,1] <- bubble.test1[53,12]
sup[10,2] <- bubble.test2[53,11]
sup[11,2] <- bubble.test2[53,12]

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

datum <- rbind(datums1,datums2)
datum <- t(datum)
datum <- rbind(c("Start","End","Start","End"), datum)
rownames(datum) <- c(' ', colnames(bubble.test1)[1:7])
xt <- xtable(datum, caption="Dates of explosive behaviour")
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{2}{c}{  No Drift  } & \\multicolumn{2}{c}{ Drift }', 
                                  collapse=''), '\\\\')

print(xt, "latex",comment=FALSE, add.to.row=addtorow, include.colnames=F,
      caption.placement = getOption("xtable.caption.placement", "top"))


#---------------------------------------
#Segmented versions
make_real <- function(indeks) {
    for(i in 2:ncol(indeks)) { 
        for(j in 1:64) {
            indeks[j,i] <- indeks[j,i]/assets$CPI[j]*100 
        }
    }
    return(indeks)
}
alt <- cbind(pr_seg[,],ave_seg[,-1],mediums[,c(-1,-5,-9,-10)],quant_results[,-1])
alt <- make_real(alt)

y_indices <- log(na.approx(alt[,-1]))
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

bubble.test1 <- numeric()
bubble.test2 <- numeric()

for(k in 1:ncol(y_indices)) { 
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

colnames(bubble.test1)[1:ncol(y_indices)] <- colnames(alt[-1])
colnames(bubble.test2)[1:ncol(y_indices)] <- colnames(alt[-1])

index_plot <- bubble.test1[,c(3,6,9,17,19)]
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"), frac = 1)
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 0.5) 
g <- g + geom_line(aes(linetype=variable))
g <- g + scale_linetype_manual(values = c(1,1,1,1,1,4,4))
g <- g + ylab("") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g


#report  bubble period dates
datum1 <- bubble.test1
datum1[!is.na(datum1)] <- NA
datum2 <- bubble.test1
datum2[!is.na(datum2)] <- NA
datums1 <- data.frame()
datums2 <- data.frame()

#bubble.test1 <- bubble.test1[,c(-19)]
#bubble.test2 <- bubble.test2[,c(-19)]

for(i in 1:15) {
    for(l in 1:53) {
        if(bubble.test1[l,i]>bubble.test1$"95%"[l]) { 
            datum1[l,i] <- bubble.test1[l,"Date"]
        }
        if(bubble.test2[l,i]>bubble.test2$"95%"[l]) { 
            datum2[l,i] <- bubble.test2[l,"Date"]
        }
    }
    if(sum(!is.na(datum1[,i]))>1) {
        NonNAindex <- which(!is.na(datum1[,i]))
        firstNonNA <- min(NonNAindex)
        datums1[1,i] <- datum1[firstNonNA,i]
        if (NonNAindex[NROW(NonNAindex)-1]==(max(NonNAindex)-1)) { 
            lastNonNA <- max(NonNAindex)
        } else lastNonNA <- NonNAindex[NROW(NonNAindex)-1]
    }
    
    datums1[2,i] <- datum1[lastNonNA,i]
    
    
    if(sum(!is.na(datum2[,i]))>1) {
        NonNAindex <- which(!is.na(datum2[,i]))
        firstNonNA <- min(NonNAindex)
        datums2[1,i] <- datum2[firstNonNA,i]
        lastNonNA <- max(NonNAindex)
    }    
    datums2[2,i] <- datum2[lastNonNA,i]
}    

datum <- rbind(datums1,datums2)
datum <- t(datum)
datum <- rbind(c("Start","End","Start","End"), datum)
rownames(datum) <- c(' ', colnames(bubble.test1)[1:15])
rownames(datum)[2:7] <- c("price_lower","price_middle","price_upper",
                          "value_lower","value_middle","value_upper")
datum[is.na(datum)] <- ""    
xt <- xtable(datum, caption="Dates of explosive behaviour")
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& \\multicolumn{2}{c}{  No Drift  } & \\multicolumn{2}{c}{ Drift }', 
                                  collapse=''), '\\\\')

print(xt, "latex",comment=FALSE, add.to.row=addtorow, include.colnames=F,
      caption.placement = getOption("xtable.caption.placement", "top"))


#---------------------------------------
png(file = "Art_plot.png", width=720,height=480)
index_plot <- all_indices[,c(1,2,5,10)]
colnames(index_plot) <- c("Date","Median","Hedonic","ps-Repeat Sales")
index_plot <- melt(index_plot, id="Date")  # convert to long format
index_plot$Date <- as.Date(as.yearqtr(index_plot$Date, format = "%Y Q%q"), frac = 1)
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 1) 
g <- g + geom_line()
g <- g + ylab("Index")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + theme(legend.position="bottom")
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g
dev.off()




