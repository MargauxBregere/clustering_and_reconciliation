####################################################################
#                      Accuracy test
####################################################################

BU <- prev_BU %>% filter(area=="ALL" & reconciliation=="no_clust")
MinT_st <- prev_MinT %>% filter(area=="ALL" & reconciliation=="st")
MinT_temp <- prev_MinT %>% filter(area=="ALL" & reconciliation=="temp")
MinT_noclust <- prev_MinT %>% filter(area=="ALL" & reconciliation=="no_clust")

# Reconciliation versus Base

e_unrec <- MinT_noclust$load-MinT_noclust$estimated_load

e_bu <- BU$load-BU$estimated_load_rec
e_st <- MinT_st$load-MinT_st$estimated_load_rec
e_temp <- MinT_temp$load-MinT_temp$estimated_load_rec
e_noclust <- MinT_noclust$load-MinT_noclust$estimated_load_rec

DMstat <- matrix(NA, nrow = 4, ncol=2)
DMpval <- matrix(NA, nrow = 4, ncol=2)

# Does reconciliation improve base?

DMstat[1,1] <- forecast::dm.test(e_unrec,e_bu, alternative = "greater")$stat
DMpval[1,1] <- forecast::dm.test(e_unrec,e_bu, alternative = "greater")$p.val
DMstat[2,1] <- forecast::dm.test(e_unrec,e_noclust, alternative = "greater")$stat
DMpval[2,1] <- forecast::dm.test(e_unrec,e_noclust, alternative = "greater")$p.val
DMstat[3,1] <- forecast::dm.test(e_unrec,e_temp, alternative = "greater")$stat
DMpval[3,1] <- forecast::dm.test(e_unrec,e_temp, alternative = "greater")$p.val
DMstat[4,1] <- forecast::dm.test(e_unrec,e_st, alternative = "greater")$stat
DMpval[4,1] <- forecast::dm.test(e_unrec,e_st, alternative = "greater")$p.val

# Is MinT_st the best approach?

DMstat[1,2] <- forecast::dm.test(e_st,e_bu, alternative = "greater")$stat
DMpval[1,2] <- forecast::dm.test(e_st,e_bu, alternative = "greater")$p.val
DMstat[2,2] <- forecast::dm.test(e_st,e_noclust, alternative = "greater")$stat
DMpval[2,2] <- forecast::dm.test(e_st,e_noclust, alternative = "greater")$p.val
DMstat[3,2] <- forecast::dm.test(e_st,e_temp, alternative = "greater")$stat
DMpval[3,2] <- forecast::dm.test(e_st,e_temp, alternative = "greater")$p.val
DMstat[4,2] <- forecast::dm.test(e_st,e_st, alternative = "greater")$stat
DMpval[4,2] <- forecast::dm.test(e_st,e_st, alternative = "greater")$p.val

xtable::xtable(DMstat)
xtable::xtable(DMpval)
