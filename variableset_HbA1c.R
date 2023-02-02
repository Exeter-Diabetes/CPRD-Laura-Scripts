#
# Variable set for osmotic symptoms models -------------------------------------
#


X                   <- "treatment"
Y                   <- "outcome_hba1c"
W_general           <- c("dstartdate_age", "gender_chr", "tx_startyear", "dstartdate_dm_dur_all", 
                      "prehba1c", "preegfr", "prebmi", "prealt", "drugline_all", "ncurrtx")

all_W               <- c(W_general)
all_variables       <- c(X, Y,  W_general)

