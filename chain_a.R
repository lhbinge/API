chain_a <- function(strat_p, strat_q, jaar1,jaar2) {
    strat_p1 <- subset(strat_p, strat_p$Group.1==jaar1)
    strat_q1 <- subset(strat_q, strat_q$Group.1==jaar1)
    strat_p2 <- subset(strat_p, strat_p$Group.1==jaar2)
    strat_q2 <- subset(strat_q, strat_q$Group.1==jaar2)
    strat_pc <- merge(strat_p1, strat_p2, by="Group.2")
    strat_qc <- merge(strat_q1, strat_q2, by="Group.2")
    Lasp <- sum(strat_pc$x.y*strat_qc$x.x,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.x,na.rm=TRUE)
    Paas <- sum(strat_pc$x.y*strat_qc$x.y,na.rm=TRUE)/sum(strat_pc$x.x*strat_qc$x.y,na.rm=TRUE)
    return(c(Lasp,Paas))
}