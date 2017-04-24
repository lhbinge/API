#-------------------
# FULL SAMPLE MODEL
#-------------------

full_model_a <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                 "nr_works","artist","timedummy")) {
    if("artist" %in% list_expl_vars) { 
        modeldata <- subset(artdata, artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
    } else modeldata <- artdata
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    model_all <- lm(expl_vars, data=modeldata)
    time_results <- summary(model_all)$coefficients[grepl("time", rownames(summary(model_all)$coefficients)),1]
    time_results <- as.data.frame(time_results)
    time_results$index_all <- exp(time_results$time_results)*100
    time_results$Date <- sub("timedummy","",row.names(time_results))
    time_results <- merge(datums,time_results,by="Date",all=TRUE)[,c(1,4)]
    return(time_results)
}


#-----------------------------
# OVERLAPPING PERIODS (1-year)
#-----------------------------
overlap1y_model_a <- function(data, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                   "nr_works","artist","lnarea:med_code","timedummy")) {
    if("artist" %in% list_expl_vars) { 
        modeldata <- subset(artdata, artdata$rank_all<max(artdata$rank_all,na.rm=TRUE))
    } else modeldata <- artdata
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

