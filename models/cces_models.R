#cces

cces <- read_dta("~/Dropbox/data/cces/CCES_Panel_Full3waves_VV_V4.dta")



# pid	Would you call yourself a strong Democrat or a not very strong Democrat?
#   Would you call yourself a strong Republican or a not very strong Republican?
#   Do you think of yourself as closer to the Democratic or the Republican Party?		
#   pid7_10	pid7 12	pid7 14
df <- cces %>% select(pid7_10,pid7_12,pid7_14, caseid) %>%
  mutate(y1 = as.numeric(pid7_10), y2 = as.numeric(pid7_12),
         y3 = as.numeric(pid7_14)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=6, "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=6, "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=6, "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_))
pidcc_model <- fmm(waves= c("y1", "y2", "y3"), id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="pidcc",
                       qtype="7")


# ideo5	Ideology		ideo5_10	ideo5 12	ideo5 14
df <- cces %>% select(ideo5_10,ideo5_12,ideo5_14, caseid) %>%
  mutate(y1 = as.numeric(ideo5_10), y2 = as.numeric(ideo5_12),
         y3 = as.numeric(ideo5_14)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_))
ideo5cc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="ideo5cc",
                   qtype="5")

# bornagain	Would you describe yourself as a born-again or evangelical Christian, or not?		
#   pew_bornagain_10	pew bornagain 12	pew bornagain 14
df <- cces %>% select(pew_bornagain_10,pew_bornagain_12,pew_bornagain_14, caseid) %>%
  mutate(y1 = as.numeric(pew_bornagain_10), y2 = as.numeric(pew_bornagain_12),
         y3 = as.numeric(pew_bornagain_14)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
brnagaincc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="brnagaincc",
                   qtype="3")

# relimpt	How important is religion in your life?		
#   pew_religimp_10	pew religimp 12	pew religimp 14
df <- cces %>% select(pew_religimp_10,pew_religimp_12,pew_religimp_14,caseid) %>%
  mutate(y1 = as.numeric(pew_religimp_10), y2 = as.numeric(pew_religimp_12),
         y3 = as.numeric(pew_religimp_14)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=4, "4"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=4, "4"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=4, "4"=4, "8"=NA_real_, "9"=NA_real_))
relimptcc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="relimptcc",
                      qtype="3")

# iraqmist	All things considered, do you think it was a mistake to invade Iraq?		
#   CC10 304	CC12 304	CC14 304
df <- cces %>% select(CC10_304,CC12_304,CC14_304,caseid) %>%
  mutate(y1 = as.numeric(CC10_304), y2 = as.numeric(CC12_304),
         y3 = as.numeric(CC14_304)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=3, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=3, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=3, "8"=NA_real_, "9"=NA_real_))
iraqmist_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="iraqmist",
                       qtype="3")

# afghmist			CC10 305	CC12 305	CC14 305
df <- cces %>% select(CC10_305,CC12_305,CC14_305,caseid) %>%
  mutate(y1 = as.numeric(CC10_305), y2 = as.numeric(CC12_305),
         y3 = as.numeric(CC14_305)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=3, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=3, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=3, "8"=NA_real_, "9"=NA_real_))
afghmist_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="afghmist",
                      qtype="3")

# obamaapprove	Do you approve of the way each is doing their job...President Obama		
# CC10 308a	CC12 308a	CC14 308a
# df <- cces %>% select(CC10_308a,CC12_308a,CC14_308a,caseid) %>%
#   mutate(y1 = as.numeric(CC10_308a), y2 = as.numeric(CC12_308a),
#          y3 = as.numeric(CC14_308a)) %>%
#   mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_))
# bhoapprovecc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
#                       data=df, cov_estimator="binom", iterations=2500,
#                       n_chains=5, burn=500, var_name="bhoapprovecc",
#                       qtype="5")

# 
# congapprove	Do you approve of the way each is doing their job...the U.S. Congress		
# CC10 308b	CC12 308b	CC14 308b
# df <- cces %>% select(CC10_308b,CC12_308b,CC14_308b,caseid) %>%
#   mutate(y1 = as.numeric(CC10_308b), y2 = as.numeric(CC12_308b),
#          y3 = as.numeric(CC14_308b)) %>%
#   mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_))
# congapprovecc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
#                           data=df, cov_estimator="binom", iterations=2500,
#                           n_chains=5, burn=500, var_name="congapprovecc",
#                           qtype="5")
# 
# scapprove	Do you approve of the way each is doing their job...the U.S. Supreme Court		
# CC10 308c	CC12 308c	CC14 308c
# df <- cces %>% select(CC10_308c,CC12_308c,CC14_308c,caseid) %>%
#   mutate(y1 = as.numeric(CC10_308c), y2 = as.numeric(CC12_308c),
#          y3 = as.numeric(CC14_308c)) %>%
#   mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_))
# scapprovecc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
#                            data=df, cov_estimator="binom", iterations=2500,
#                            n_chains=5, burn=500, var_name="scapprovecc",
#                            qtype="5")
# 
# govapprove	Do you approve of the way each is doing their job...the Governor		
# CC10 308d	CC12 308d	CC14 308d
# df <- cces %>% select(CC10_308d,CC12_308d,CC14_308d,caseid) %>%
#   mutate(y1 = as.numeric(CC10_308d), y2 = as.numeric(CC12_308d),
#          y3 = as.numeric(CC14_308d)) %>%
#   mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_))
# govapprovecc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
#                            data=df, cov_estimator="binom", iterations=2500,
#                            n_chains=5, burn=500, var_name="govapprovecc",
#                            qtype="5")
# 
# approvestleg	Do you approve of the way each is doing their job...the State Legislature		
# CC10 308e	CC12 308e	CC14 308e
# df <- cces %>% select(CC10_308e,CC12_308e,CC14_308e,caseid) %>%
#   mutate(y1 = as.numeric(CC10_308e), y2 = as.numeric(CC12_308e),
#          y3 = as.numeric(CC14_308e)) %>%
#   mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_),
#          y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "5"=3, "8"=NA_real_, "9"=NA_real_))
# stlegapprovecc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
#                           data=df, cov_estimator="binom", iterations=2500,
#                           n_chains=5, burn=500, var_name="stlegapprovecc",
#                           qtype="5")
# 
# gunlaw	Should firearm laws be more strict, less strict or kept same		
# CC10 320	CC12 320	CC14 320
df <- cces %>% select(CC10_320,CC12_320,CC14_320,caseid) %>%
  mutate(y1 = as.numeric(CC10_320), y2 = as.numeric(CC12_320),
         y3 = as.numeric(CC14_320)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=NA_real_, "9"=NA_real_))
gunstrict_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                            data=df, cov_estimator="binom", iterations=2500,
                            n_chains=5, burn=500, var_name="gunstrict",
                            qtype="3")

# 
# climatechg	Climate Change Opinion		  
# CC10 321	CC12 321	CC14 321
df <- cces %>% select(CC10_321,CC12_321,CC14_321,caseid) %>%
  mutate(y1 = as.numeric(CC10_321), y2 = as.numeric(CC12_321),
         y3 = as.numeric(CC14_321)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=4, "4"=4, "5"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=4, "4"=4, "5"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=4, "4"=4, "5"=4, "8"=NA_real_, "9"=NA_real_))
climchg_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="climchg",
                       qtype="3")

# immlegal	Grant legal status		
# CC10 322 1	CC12 322 1	CC14 322 1
df <- cces %>% select(CC10_322_1,CC12_322_1,CC14_322_1,caseid) %>%
  mutate(y1 = as.numeric(CC10_322_1), y2 = as.numeric(CC12_322_1),
         y3 = as.numeric(CC14_322_1)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
immlegal_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="immlegal",
                     qtype="3")

# immbrdr	Increase border patros		
# CC10 322 2	CC12 322 2	CC14 322 2
df <- cces %>% select(CC10_322_2,CC12_322_2,CC14_322_2,caseid) %>%
  mutate(y1 = as.numeric(CC10_322_2), y2 = as.numeric(CC12_322_2),
         y3 = as.numeric(CC14_322_2)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
immbrdr_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="immbrdr",
                      qtype="3")

# immquest	Question suspected illegal immigrants		
# CC10 322 3	CC12 322 3	CC14 322 3
df <- cces %>% select(CC10_322_3,CC12_322_3,CC14_322_3,caseid) %>%
  mutate(y1 = as.numeric(CC10_322_3), y2 = as.numeric(CC12_322_3),
         y3 = as.numeric(CC14_322_3)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
immquest_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="immquest",
                      qtype="3")

# immfine	Fine businesses that hire immigrants		
# CC10 322 4	CC12 322 4	CC14 322 4
# df <- cces %>% select(CC10_322_4,CC12_322_4,CC14_322_4) %>%
#   mutate(y1 = as.numeric(CC10_322_4), y2 = as.numeric(CC12_322_4),
#          y3 = as.numeric(CC14_322_4)) %>%
#   mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
#          y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
#          y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
# immfine_model <- fmm(waves= c("y1", "y2", "y3"),
#                       data=df, cov_estimator="binom", iterations=2500,
#                       n_chains=5, burn=500, var_name="immfine",
#                       qtype="3")


# abort	Abortion		  
# CC10 324	CC12 324	CC14 324
df <- cces %>% select(CC10_324,CC12_324,CC14_324,caseid) %>%
  mutate(y1 = as.numeric(CC10_324), y2 = as.numeric(CC12_324),
         y3 = as.numeric(CC14_324)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=2, "4"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=2, "4"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=2, "4"=4, "8"=NA_real_, "9"=NA_real_))
abort_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="abortcc",
                     qtype="3")

# envecon	Environment-Economy		
# CC10 325	CC12 325	CC14 325
df <- cces %>% select(CC10_325,CC12_325,CC14_325,caseid) %>%
  mutate(y1 = as.numeric(CC10_325), y2 = as.numeric(CC12_325),
         y3 = as.numeric(CC14_325)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=3, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=3, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=3, "8"=NA_real_, "9"=NA_real_))
envecon_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="envecon",
                   qtype="5")


# maramend	Marriage Amendment	
# CC10 326	CC12 326	CC14 326
df <- cces %>% select(CC10_326,CC12_326,CC14_326,caseid) %>%
  mutate(y1 = as.numeric(CC10_326), y2 = as.numeric(CC12_326),
         y3 = as.numeric(CC14_326)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
maramend_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="maramend",
                     qtype="3")


# affact	Affirmative Action		
# CC10 327	CC12 327	CC14 327
df <- cces %>% select(CC10_327,CC12_327,CC14_327,caseid) %>%
  mutate(y1 = as.numeric(CC10_327), y2 = as.numeric(CC12_327),
         y3 = as.numeric(CC14_327)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "8"=NA_real_, "9"=NA_real_))
affactcc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="affactcc",
                      qtype="5")

#Most: Cut domestic; cut foreign; raise taxes		 
# CC10 328 	CC12 328	CC14 328
df <- cces %>% select(CC10_328,CC12_328,CC14_328,caseid) %>%
  mutate(y1 = as.numeric(CC10_328), y2 = as.numeric(CC12_328),
         y3 = as.numeric(CC14_328)) %>%
  mutate(y1 = recode(y1, "1"=4, "2"=2, "3"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=4, "2"=2, "3"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=4, "2"=2, "3"=4, "8"=NA_real_, "9"=NA_real_))
cutspend_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="cutspend",
                      qtype="3")

# Least: Cut domestic; cut foreign; raise taxes		
# CC10 329	CC12 329	CC14 329
df <- cces %>% select(CC10_329,CC12_329,CC14_329,caseid) %>%
  mutate(y1 = as.numeric(CC10_329), y2 = as.numeric(CC12_329),
         y3 = as.numeric(CC14_329)) %>%
  mutate(y1 = recode(y1, "1"=4, "2"=4, "3"=2, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=4, "2"=4, "3"=2, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=4, "2"=4, "3"=2, "8"=NA_real_, "9"=NA_real_))
donttax_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="donttax",
                      qtype="3")

# ideology	Ideology		
# CC10 334A	CC12 341A	CC14 341A
df <- cces %>% select(CC10_341A,CC12_341A,CC14_341A,caseid) %>%
  mutate(y1 = as.numeric(CC10_341A), y2 = as.numeric(CC12_341A),
         y3 = as.numeric(CC14_341A)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_))
ideo7cc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="ideo7cc",
                     qtype="7")

# bhoideo	Obama Ideology		
# CC10 334C	CC12 341C	CC14 341C
df <- cces %>% select(CC10_341C,CC12_341C,CC14_341C,caseid) %>%
  mutate(y1 = as.numeric(CC10_341C), y2 = as.numeric(CC12_341C),
         y3 = as.numeric(CC14_341C)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_))
bhoideo_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="bhoideo",
                     qtype="7")

# demideo	Dems Ideology	
# CC10 334D	CC12 341E	CC14 341E
df <- cces %>% select(CC10_334E,CC12_341E,CC14_341E,caseid) %>%
  mutate(y1 = as.numeric(CC10_334E), y2 = as.numeric(CC12_341E),
         y3 = as.numeric(CC14_341E)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_))
demideo_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="demideo",
                     qtype="7")

# repideo	Rep Ideology		
# CC10 334E	CC12 341F	CC14 341F
df <- cces %>% select(CC10_341F,CC12_341F,CC14_341F,caseid) %>%
  mutate(y1 = as.numeric(CC10_341F), y2 = as.numeric(CC12_341F),
         y3 = as.numeric(CC14_341F)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_))
repideo_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="repideo",
                     qtype="7")

# teaideo	Tea Party Ideolgoy		
# CC10 334M	CC12 341R	CC14 341R
df <- cces %>% select(CC10_341R,CC12_341R,CC14_341R,caseid) %>%
  mutate(y1 = as.numeric(CC10_341R), y2 = as.numeric(CC12_341R),
         y3 = as.numeric(CC14_341R)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                     "7"=7, "8"=4, "98"=NA_real_, "99"=NA_real_))
teaideo_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="teaideo",
                     qtype="7")


# troopsoil	Troops for Oil		
# CC10 414 1	CC12 414 1	CC14 414 1
df <- cces %>% select(CC10_414_1,CC12_414_1,CC14_414_1,caseid) %>%
  mutate(y1 = as.numeric(CC10_414_1), y2 = as.numeric(CC12_414_1),
         y3 = as.numeric(CC14_414_1)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
troopsoil_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="troopsoil",
                     qtype="3")


# troopsterror	Troops to fight terror		
# CC10 414 2	CC12 414 2	CC14 414 2
df <- cces %>% select(CC10_414_2,CC12_414_2,CC14_414_2,caseid) %>%
  mutate(y1 = as.numeric(CC10_414_2), y2 = as.numeric(CC12_414_2),
         y3 = as.numeric(CC14_414_2)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
troopsterror_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="troopsterror",
                       qtype="3")


# troopsgeno	Troops to prevent genocide		
# CC10 414 3	CC12 414 3	CC14 414 3
df <- cces %>% select(CC10_414_3,CC12_414_3,CC14_414_3,caseid) %>%
  mutate(y1 = as.numeric(CC10_414_3), y2 = as.numeric(CC12_414_3),
         y3 = as.numeric(CC14_414_3)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
troopsgeno_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="troopsgeno",
                       qtype="3")


# troopsdmcy	Troops for democract		
# CC10 414 4	CC12 414 4	CC14 414 4
df <- cces %>% select(CC10_414_4,CC12_414_4,CC14_414_4,caseid) %>%
  mutate(y1 = as.numeric(CC10_414_4), y2 = as.numeric(CC12_414_4),
         y3 = as.numeric(CC14_414_4)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
troopsdmcy_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="troopsdmcy",
                       qtype="3")

# troopsally	Troops to protect allies		
# CC10 414 5	CC12 414 5	CC14 414 5
df <- cces %>% select(CC10_414_5,CC12_414_5,CC14_414_5,caseid) %>%
  mutate(y1 = as.numeric(CC10_414_5), y2 = as.numeric(CC12_414_5),
         y3 = as.numeric(CC14_414_5)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
troopsally_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="troopsally",
                       qtype="3")


# troopsun	Troops to help UN	
# CC10 414 6	CC12 414 6	CC14 414 6
df <- cces %>% select(CC10_414_6,CC12_414_6,CC14_414_6,caseid) %>%
  mutate(y1 = as.numeric(CC10_414_6), y2 = as.numeric(CC12_414_6),
         y3 = as.numeric(CC14_414_6)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
troopsun_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="troopsun",
                       qtype="3")


# troopsnone	Troops for None		
# CC10 414 7	CC12 414 7	CC14 414 7
df <- cces %>% select(CC10_414_7,CC12_414_7,CC14_414_7,caseid) %>%
  mutate(y1 = as.numeric(CC10_414_7), y2 = as.numeric(CC12_414_7),
         y3 = as.numeric(CC14_414_7)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
troopsnone_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="troopsnone",
                       qtype="3")


# statetaxspend	State budget: taxes/spending		
# CC10 415r	CC12 415r	CC14 415r
df <- cces %>% select(CC10_415r,CC12_415r,CC14_415r,caseid) %>%
  mutate(y1 = as.numeric(CC10_415r), y2 = as.numeric(CC12_415r),
         y3 = as.numeric(CC14_415r)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
statetaxspend_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="statetaxspend",
                        qtype="5")

# statesalesincome	State budget: sales/income		
# CC10 416r	CC12 416r	CC14 416r
df <- cces %>% select(CC10_416r,CC12_416r,CC14_416r,caseid) %>%
  mutate(y1 = as.numeric(CC10_416r), y2 = as.numeric(CC12_416r),
         y3 = as.numeric(CC14_416r)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
statesalesincome_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                           data=df, cov_estimator="binom", iterations=2500,
                           n_chains=5, burn=500, var_name="statesalesincome",
                           qtype="5")


# wrkwayup	Blacks wrkwayup		
# CC10 422a	CC12 422a	CC14 422a
df <- cces %>% select(CC10_422a,CC12_422a,CC14_422a,caseid) %>%
  mutate(y1 = as.numeric(CC10_422a), y2 = as.numeric(CC12_422a),
         y3 = as.numeric(CC14_422a)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_))
wrkwayupcc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="wrkwayupcc",
                   qtype="5")

# barriers	Slavery makes rising difficult		
# CC10 422b	CC12 422b	CC14 422b
df <- cces %>% select(CC10_422b,CC12_422b,CC14_422b,caseid) %>%
  mutate(y1 = as.numeric(CC10_422b), y2 = as.numeric(CC12_422b),
         y3 = as.numeric(CC14_422b)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "6"=3, "98"=NA_real_, "99"=NA_real_))
slvdiffcc_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="slvdiffcc",
                        qtype="5")

# arra	Recovery and Reinvestment		
# CC10 332A	CC12 330A	CC14 330A
df <- cces %>% select(CC10_330A,CC12_330A,CC14_330A,caseid) %>%
  mutate(y1 = as.numeric(CC10_330A), y2 = as.numeric(CC12_330A),
         y3 = as.numeric(CC14_330A)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
arra_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="arra",
                      qtype="3")


# cleanenergy	Clean Energy		
# CC10 332C	CC12 330C	CC14 330C
df <- cces %>% select(CC10_330C,CC12_330C,CC14_330C,caseid) %>%
  mutate(y1 = as.numeric(CC10_330C), y2 = as.numeric(CC12_330C),
         y3 = as.numeric(CC14_330C)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
clnenrgy_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="clnenrgy",
                  qtype="3")


# aca	Health Reform	
# CC10 332D	CC12 330D	CC14 330D
df <- cces %>% select(CC10_330D,CC12_330D,CC14_330D,caseid) %>%
  mutate(y1 = as.numeric(CC10_330D), y2 = as.numeric(CC12_330D),
         y3 = as.numeric(CC14_330D)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
aca_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="aca",
                  qtype="3")


# kagan	Elena Kagan		
# CC10 332E	CC12 330E	CC14 330E
df <- cces %>% select(CC10_330E,CC12_330E,CC14_330E,caseid) %>%
  mutate(y1 = as.numeric(CC10_330E), y2 = as.numeric(CC12_330E),
         y3 = as.numeric(CC14_330E)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
kagan_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                 data=df, cov_estimator="binom", iterations=2500,
                 n_chains=5, burn=500, var_name="kagan",
                 qtype="3")


# finreform	Financial Reform Bill		
# CC10 332F	CC12 330F	CC14 330F
df <- cces %>% select(CC10_330F,CC12_330F,CC14_330F,caseid) %>%
  mutate(y1 = as.numeric(CC10_330F), y2 = as.numeric(CC12_330F),
         y3 = as.numeric(CC14_330F)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
finrefrm_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                 data=df, cov_estimator="binom", iterations=2500,
                 n_chains=5, burn=500, var_name="finrefrm",
                 qtype="3")


# dadt	Don't Ask Don't Tell		
# CC10 332G	CC12 330G	CC14 330G
df <- cces %>% select(CC10_330G,CC12_330G,CC14_330G, caseid) %>%
  mutate(y1 = as.numeric(CC10_330G), y2 = as.numeric(CC12_330G),
         y3 = as.numeric(CC14_330G)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
dadt_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                 data=df, cov_estimator="binom", iterations=2500,
                 n_chains=5, burn=500, var_name="dadt",
                 qtype="3")


# stemcell	Stem Cell Research		
# CC10 332I	CC12 330H	CC14 330H
df <- cces %>% select(CC10_330H,CC12_330H,CC14_330H, caseid) %>%
  mutate(y1 = as.numeric(CC10_330H), y2 = as.numeric(CC12_330H),
         y3 = as.numeric(CC14_330H)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=NA_real_, "9"=NA_real_))
stemcell_model <- fmm(waves= c("y1", "y2", "y3"),id="caseid",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="stemcell",
                  qtype="3")


cces_results <- list(pidcc_model, ideo5cc_model, brnagaincc_model, relimptcc_model, iraqmist_model, #5
                     afghmist_model, gunstrict_model,  climchg_model, immlegal_model, immbrdr_model, #10
                     immquest_model, abort_model, envecon_model, maramend_model, affactcc_model, #15
                     cutspend_model, donttax_model, ideo7cc_model, troopsoil_model, troopsterror_model, #20
                     troopsgeno_model, troopsdmcy_model, troopsally_model, troopsun_model, troopsnone_model, #25
                     statesalesincome_model, statetaxspend_model, wrkwayupcc_model, slvdiffcc_model, arra_model, #30
                     clnenrgy_model, aca_model, kagan_model, finrefrm_model, dadt_model, #35
                     stemcell_model) #36
save(cces_results, file = "~/Dropbox/hill_kreisi/results/ccesresults.Rdata")

cces_results[[12]] <- abort_model

#teaideo
#stlegapprove
#demideo

rm(pidcc_model, ideo5cc_model, brnagaincc_model,
   relimptcc_model, iraqmist_model, afghmist_model, 
   bhoapprovecc_model, congapprovecc_model, scapprovecc_model,
   govapprovecc_model, stlegapprovecc_model, gunstrict_model, 
   climchg_model, immlegal_model, immbrdr_model, immquest_model,
   abort_model, envecon_model, maramend_model, 
   affactcc_model, cutspend_model, donttax_model, ideo7cc_model, 
   bhoideo_model, demideo_model, repideo_model, teaideo_model, 
   troopsoil_model, troopsterror_model, troopsgeno_model, 
   troopsdmcy_model, troopsally_model, troopsun_model, 
   troopsnone_model, statesalesincome_model, statetaxspend_model,
   wrkwayupcc_model, slvdiffcc_model, arra_model, 
   clnenrgy_model, aca_model, kagan_model, finrefrm_model,
   dadt_model, stemcell_model)
