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
    return(time_results)
}


