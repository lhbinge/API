#-----------------------------
# OVERLAPPING PERIODS (1-year)
#-----------------------------
overlap1y_model <- function(artdata, list_expl_vars=c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed", "dum_dated",  
                                                    "nr_works","artist","timedummy")) {

    expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))

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
    return(overlap)

}


