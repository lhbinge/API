#-----------------------------
# OVERLAPPING PERIODS (2-year)
#-----------------------------
overlap2y_model <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                "nr_works","artist","timedummy")) {
    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
    res_list <- list()
    for(i in 1:8) {
        if("artist" %in% list_expl_vars) {
            modeldata <- subset(artdata, artdata[,(60+i)]<max(artdata[,(60+i)],na.rm=TRUE))
        } else modeldata <- artdata
        modeldata <- subset(modeldata, modeldata$counter>(i*8-9)& modeldata$counter<(i*8+1))
        model <- lm(expl_vars, data=modeldata)  
        res_list[[i]] <- summary(model)$coefficients[grepl("time", rownames(summary(model)$coefficients)),1]
    }
    #Merge all results
    if("artist" %in% list_expl_vars) {
        overlap2 <- time_results
    } else overlap2 <- rep_results
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
    return(overlap2)
}




