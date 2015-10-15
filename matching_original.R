##======================##
## MATCHING METHODOLOGY ##
##======================##

#check for duplicates (how many)
sum(duplicated(artdata[,c("artist","title","med_code","area","dum_signed","dum_dated")]))


#The distance metric between any two sales (across the intervening time period) is the 
#absolute value of the difference between the two predicted hedonic log values. 
#The threshold for this distance metric can be customised. 
#At one extreme, one can choose to select only one pair with the smallest value of the distance metric. 


#match per artist by hedonic function
list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))




    modeldata <- subset(artdata, artdata$rank_total==1)
    model <- lm(expl_vars, data=modeldata, na.action = na.exclude)
    
    #calculate predicted value for each painting (excluding time dummies)
    newdata <- modeldata
    newdata$timedummy <- "2000 Q1"
    modeldata <- cbind(modeldata,fitted=predict.lm(model,newdata))
    modeldata <- modeldata[!is.na(modeldata$fitted),]
    
    #try id manually:
    modeldata$id <- 0
    for(i in 1:nrow(modeldata)) {
        if(modeldata$id[i]==0) {
            modeldata$id[i] <- i
            modeldata$distance <- abs(modeldata$fitted[i]-modeldata$fitted)
            modeldata$id[(modeldata$distance==min(modeldata[modeldata[,"id"]==0,"distance"]
                                                  ,na.rm=TRUE))] <- i
        } 
    }
    
    repdata <- repsaledata(modeldata$lnprice,modeldata$counter,modeldata$id)
    
    






ps.RS <- repsale(repdata$price0,repdata$time0,repdata$price1,repdata$time1,mergefirst=1,
                 graph=TRUE,graph.conf=TRUE,conf=.95, stage3=FALSE)
ps.RS_index <- exp(as.data.frame(ps.RS$pindex))*100