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