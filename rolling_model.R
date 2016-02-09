#----------------------
#ROLLING 5-YEAR WINDOWS
#----------------------

rolling_model <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                    "nr_works","artist","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:12) {
        if("artist" %in% list_expl_vars) {
            modeldata <- subset(artdata, artdata[,(32+i)]<max(artdata[,(32+i)],na.rm=TRUE))
        } else modeldata <- artdata
        modeldata <- subset(modeldata, modeldata$counter>(i*4-4)&modeldata$counter<(i*4+17))
        model <- lm(expl_vars, data=modeldata)  
        summary(model)
        res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
    }
    
#Update
    #if("artist" %in% list_expl_vars) {
    #    modeldata <- subset(artdata, artdata[,44]<max(artdata[,44],na.rm=TRUE))
    #} else modeldata <- artdata
    #modeldata <- subset(modeldata, modeldata$counter>42& modeldata$counter<63)
    #model <- lm(expl_vars, data=modeldata)  
    #summary(model)
    #res_list[[12]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]

    #Merge all results
    if("artist" %in% list_expl_vars) {
        rolling <- time_results
    } else rolling <- rep_results
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
    rolling$Date <- c("2000Q2","2000Q3","2000Q4","2001Q1","2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
                    "2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4",
                    "2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4",
                    "2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4",
                    "2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4",
                    "2015Q1","2015Q2","2015Q3","2015Q4")
    rolling$Date <- factor(rolling$Date)
    return(rolling)
}
