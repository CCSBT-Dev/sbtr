all.base <- c("MP1_2035_5000_inc\\MP1_2d_lag1_base", 
				 "HK7a5_2035_5000_inc\\v2\\HK7a5_5000_2d_lag1_base",

				 "MP1_2035_5000_noinc\\MP1_2d_lag1_base_5000_noinc", 
				 "HK7a6_2035_5000_noinc\\v2\\HK7a6_5000_2d_lag1_base",

				 "MP1_2030_3000_inc\\MP1_7d_lag1_base_3000_inc",
				 "HK7a5_2030_3000_inc\\v7\\HK7a5_3000_7d_lag1_base",

				 "MP1_2030_3000_noinc\\MP1_7d_lag1_base_3000_noinc",
				 "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_base",

				 "MP1_2035_3000_inc\\MP1_2d_lag1_base",

				 "MP1_2035_3000_noinc\\MP1_2d_lag1_base_3000_noinc"
)

base.s3 <- paste(all.base,".s3",sep="")
base.s4 <- paste(all.base,".s4",sep="")

title.base <- c("MP1 2035 5000 inc", "MP2_a5 2035 5000 inc", 
               "MP1 2035 5000 noinc", "MP2_a6 2035 5000 noinc",
               "MP1 2030 3000 inc", "MP2_a6 2030 3000 inc",
               "MP1 2030 3000 noinc", "MP2_a6 2030 3000 noinc",
               "MP1 2035 3000 inc", 
               "MP1 2035 3000 noinc")

robust_2035_5000_inc <- list(
             c("MP1_2035_5000_inc\\MP1_2d_lag1_base",         "HK7a5_2035_5000_inc\\v2\\HK7a5_5000_2d_lag1_base"),
             c("MP1_2035_5000_inc\\MP1_2d_lag1_base_lowR",    "HK7a5_2035_5000_inc\\v2\\HK7a5_5000_2d_lag1_lowr"),               
             c("MP1_2035_5000_inc\\MP1_2d_lag1_base_Omega75", "HK7a5_2035_5000_inc\\v2\\HK7a5_5000_2d_lag1_omega75flexLL3"),               
             c("MP1_2035_5000_inc\\MP1_2d_lag1_base_upq2008", "HK7a5_2035_5000_inc\\v2\\HK7a5_5000_2d_lag1_baseflexLL3_upq2008"),
             c("MP1_2035_5000_inc\\MP1_2d_lag1_base_STWin", "HK7a5_2035_5000_inc\\v2\\HK7a5_5000_2d_lag1_STwindowsflexLL3"),
             c("MP1_2035_5000_inc\\MP1_2d_lag1_base_updownq", "HK7a5_2035_5000_inc\\v2\\HK7a5_5000_2d_lag1_baseflexLL3_updownq") )               


robust_2030_3000_inc <- list(
             c("MP1_2030_3000_inc\\MP1_7d_lag1_base_3000_inc",  "HK7a5_2030_3000_inc\\v7\\HK7a5_3000_7d_lag1_base"),
             c("MP1_2030_3000_inc\\MP1_7d_lag1_3000_inc_lowR",    "HK7a5_2030_3000_inc\\v7\\HK7a5_3000_7d_lag1_lowr"),               
             c("MP1_2030_3000_inc\\MP1_7d_lag1_3000_inc_Omega75", "HK7a5_2030_3000_inc\\v7\\HK7a5_3000_7d_lag1_omega75flexLL3"),               
             c("MP1_2030_3000_inc\\MP1_7d_lag1_3000_inc_upq2008", "HK7a5_2030_3000_inc\\v7\\HK7a5_3000_7d_lag1_baseflexLL3_upq2008"),   
             c("MP1_2030_3000_inc\\MP1_7d_lag1_3000_inc_STWin",   "HK7a5_2030_3000_inc\\v7\\HK7a5_3000_7d_lag1_STwindowsflexLL3"),
             c("MP1_2030_3000_inc\\MP1_7d_lag1_3000_inc_updownq", "HK7a5_2030_3000_inc\\v7\\HK7a5_3000_7d_lag1_baseflexLL3_updownq")  )              

list_robust_2030_3000_noinc <- c(
             "MP1_2030_3000_noinc\\MP1_7d_lag1_base_3000_noinc",    "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_base",
             "MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_lowR",    "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_lowr",               
             "MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_Omega75", "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_omega75flexLL3",               
             "MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_upq2008", "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_baseflexLL3_upq2008",   
             "MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_STWin",   "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_STwindowsflexLL3",
             "MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_updownq", "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_baseflexLL3_updownq")              

title_robust_2030_3000_noinc <- c("MP1 base", "MP2 base", "MP1 lowR", "MP2 lowR", "MP1 omega 75", "MP2 omega 75",
                                  "MP1 upq2008", "MP2 upq2008", "MP1 STwin", "MP2 STwin", "MP1 updownq", "MP2 updownq")


robust_2030_3000_noinc <- list(
             c("MP1_2030_3000_noinc\\MP1_7d_lag1_base_3000_noinc",    "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_base"),
             c("MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_lowR",    "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_lowr"),               
             c("MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_Omega75", "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_omega75flexLL3"),               
             c("MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_upq2008", "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_baseflexLL3_upq2008"),   
             c("MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_STWin",   "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_STwindowsflexLL3"),
             c("MP1_2030_3000_noinc\\MP1_7d_lag1_3000_noinc_updownq", "HK7a6_2030_3000_noinc\\v7\\HK7a6_3000_7d_lag1_baseflexLL3_updownq")  )              


robust_2035_5000_noinc <- list(
             c("MP1_2035_5000_noinc\\MP1_2d_lag1_base_5000_noinc",    "HK7a6_2035_5000_noinc\\v2\\HK7a6_5000_2d_lag1_base"),
             c("MP1_2035_5000_noinc\\MP1_2d_lag1_5000_noinc_lowR",    "HK7a6_2035_5000_noinc\\v2\\HK7a6_5000_2d_lag1_lowr"),               
             c("MP1_2035_5000_noinc\\MP1_2d_lag1_5000_noinc_Omega75", "HK7a6_2035_5000_noinc\\v2\\HK7a6_5000_2d_lag1_omega75flexLL3"),               
             c("MP1_2035_5000_noinc\\MP1_2d_lag1_5000_noinc_upq2008", "HK7a6_2035_5000_noinc\\v2\\HK7a6_5000_2d_lag1_baseflexLL3_upq2008"),   
             c("MP1_2035_5000_noinc\\MP1_2d_lag1_5000_noinc_STWin",   "HK7a6_2035_5000_noinc\\v2\\HK7a6_5000_2d_lag1_STwindowsflexLL3"),
             c("MP1_2035_5000_noinc\\MP1_2d_lag1_5000_noinc_updownq", "HK7a6_2035_5000_noinc\\v2\\HK7a6_5000_2d_lag1_baseflexLL3_updownq")  )              


###available robustness comparisons
#2030 3000 inc
#2030 3000 noinc
#2035 5000 noinc
#
#waiting Aus 2025_5000_inc
#
#
MP1.robust.35.5000.inc <- c("MP1_2035_5000_inc\\MP1_2d_lag1_base")
robustness.names <- c("base")