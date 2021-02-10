

# Mother data from the Intergenerational study of parents and children
# (See SAFC file for description of why I use SAF)

#local load
safm80 <- read_dta("~/Dropbox/data/saf/saf_mom80.dta")
safm85 <- read_dta("~/Dropbox/data/saf/saf_mom85.dta")
safm93 <- read_dta("~/Dropbox/data/saf/saf_mom93.dta")

#combine data
safm <- left_join(safm80, safm85, by = "FAMID62") %>%
  left_join(safm93, by = "FAMID62")

#source functions
source("~/ambivalence_everywhere/functions/model_function.R")


df <- safm %>%
  select(V2089, V3078, V11082) %>%
  mutate(y1 = V2089,
         y2 = V3078, 
         y3 = V11082) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mnolive_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="mnolive",
                      qtype="5")

df <- safm %>%
  select(V2087, V3076, V11080) %>%
  mutate(y1 = V2087,
         y2 = V3076, 
         y3 = V11080) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mnosex_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="mnosex",
                     qtype="5")

df <- safm %>%
  select(V2058, V3058, V11066) %>%
  mutate(y1 = V2058,
         y2 = V3058, 
         y3 = V11066) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mnodiv_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="mnodiv",
                    qtype="5")


df <- safm %>%
  select(V2057, V3057, V11065) %>%
  mutate(y1 = V2057,
         y2 = V3057, 
         y3 = V11065) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mmandec_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="mmandec",
                    qtype="5")


df <- safm %>%
  select(V2060, V3060, V11068) %>%
  mutate(y1 = V2060,
         y2 = V3060, 
         y3 = V11068) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mmenswrk_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="mmenswrk",
                     qtype="5")


df <- safm %>%
  select(V2061, V3061, V11069) %>%
  mutate(y1 = V2061,
         y2 = V3061, 
         y3 = V11069) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mnohelp_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="mnohelp",
                      qtype="5")


df <- safm %>%
  select(V2065, V3065, V11073) %>%
  mutate(y1 = V2065,
         y2 = V3065, 
         y3 = V11073) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mhelphsbnd_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="mhelphsbnd",
                     qtype="5")


df <- safm %>%
  select(V2062, V3062, V11070) %>%
  mutate(y1 = V2062,
         y2 = V3062, 
         y3 = V11070) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mwrkwarm_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="mwrkwarm",
                        qtype="5")


df <- safm %>%
  select(V2084, V3073, V11077) %>%
  mutate(y1 = V2084,
         y2 = V3073, 
         y3 = V11077) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mmarhap_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="mmarhap",
                      qtype="5")


df <- safm %>%
  select(V2086, V3075, V11079) %>%
  mutate(y1 = V2086,
         y2 = V3075, 
         y3 = V11079) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mfewgd_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="mfewgd",
                     qtype="5")


df <- safm %>%
  select(V2092, V3081, V11085) %>%
  mutate(y1 = V2092,
         y2 = V3081, 
         y3 = V11085) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
madvsngle_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="madvsngle",
                    qtype="5")


df <- safm %>%
  select(V2085, V3074, V11078) %>%
  mutate(y1 = V2085,
         y2 = V3074, 
         y3 = V11078) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mcohabok_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="mcohabok",
                       qtype="5")


df <- safm %>%
  select(V2091, V3080, V11084) %>%
  mutate(y1 = V2091,
         y2 = V3080, 
         y3 = V11084) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mpremarok_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="mpremarok",
                     qtype="5")


df <- safm %>%
  select(V2088, V3077, V11081) %>%
  mutate(y1 = V2088,
         y2 = V3077, 
         y3 = V11081) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mdivbest_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="mdivbest",
                      qtype="5")


df <- safm %>%
  select(V2059, V3059, V11067) %>%
  mutate(y1 = V2059,
         y2 = V3059, 
         y3 = V11067) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mokclub_model <- fmm(waves= c("y1", "y2", "y3"),
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="mokclub",
                   qtype="5")


df <- safm %>%
  select(V2063, V3063, V11071) %>%
  mutate(y1 = V2063,
         y2 = V3063, 
         y3 = V11071) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mmanearn_model <- fmm(waves= c("y1", "y2", "y3"),
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="mmanearn",
                   qtype="5")



df <- safm %>%
  select(V2090, V3079, V11083) %>%
  mutate(y1 = V2090,
         y2 = V3079, 
         y3 = V11083) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
mmarbet_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="mmarbet",
                     qtype="5")





safm_results <- list(mnolive_model, mnosex_model, mnodiv_model, mmandec_model, mmenswrk_model, #5
                     mnohelp_model, mhelphsbnd_model, mwrkwarm_model, mmarhap_model, mfewgd_model, #10 
                     madvsngle_model, mcohabok_model, mpremarok_model, mdivbest_model, mokclub_model, #15
                     mmanearn_model, mmarbet_model) #17
#local save (for KK)
# save(safm_results, file = "~/Dropbox/hill_kreisi/results/safmesults.Rdata")

#cleanup
rm(mnolive_model, mnosex_model, mnodiv_model, mmandec_model, mmenswrk_model, #5
   mnohelp_model, mhelphsbnd_model, mwrkwarm_model, mmarhap_model, mfewgd_model, #10 
   madvsngle_model, mcohabok_model, mpremarok_model, mdivbest_model, mokclub_model, #15
   mmanearn_model, mmarbet_model)
