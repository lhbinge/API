list_expl_vars <- c("lnarea","ah_code","med_code","lnsculpt_area","dum_signed","dum_dated",  
                    "nr_works","lnrep","timedummy")
expl_vars <- as.formula(paste("lnprice~",paste(list_expl_vars,collapse="+")))
res_list <- list()
for(i in 1:16) {
    modeldata <- artdata
    modeldata <- subset(modeldata, modeldata$counter>(i*4-5)& modeldata$counter<(i*4+1))
    model <- lm(expl_vars, data=modeldata)  
    res_list[[i]] <- model
}

model_i <- merge(summary(model_all)$coefficients, summary(res_list[[1]])$coefficients, by="row.names", all=TRUE)
model_i <- model_i[,c(1,2,6)]
for(i in 2:16) {
    model_i <- merge(model_i, summary(res_list[[i]])$coefficients, by.x = "Row.names", by.y = "row.names", all=TRUE)
    model_i <- model_i[,1:(i+2)]
} 
model_i$Ave <- rowMeans(model_i[,3:18], na.rm=TRUE)
model_i <- model_i[,c(1,2,19)]
#modelAve <- model_all 
#modelAve$coefficients <- model_i$Ave

#print.xtable(model_i)



