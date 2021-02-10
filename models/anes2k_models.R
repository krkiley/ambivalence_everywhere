
############################# 2K ###############################################
################################################################################
################################################################################

#Panel is avaialble at: https://electionstudies.org/data-center/2000-2004-merged-file/

# local load: anes2k <- read_dta("~/Dropbox/data/anes/anes0004/anes_mergedfile_2000to2004.dta")

#better of stay home M000513a	M023033	M045143
df <- anes2k %>% select(M000514,M023033,M045143) %>%
  mutate(y1 = as.numeric(M000514), y2 = as.numeric(M023033),
         y3 = as.numeric(M045143)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
ushome_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="ushome",
                        qtype="3")



#R Well-Qualified For Political Partic	M001517	M025169	M045144
df <- anes2k %>% select(M001517,M025169,M045144) %>%
  mutate(y1 = as.numeric(M001517), y2 = as.numeric(M025169),
         y3 = as.numeric(M045144)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_))
poliqual_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=1000,
                    n_chains=5, burn=500, var_name="poliqual",
                    qtype="5")

#R Better Informed Than Most People	M001519	M025170	M045145
df <- anes2k %>% select(M001519,M025170,M045145) %>%
  mutate(y1 = as.numeric(M001519), y2 = as.numeric(M025170),
         y3 = as.numeric(M045145)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_))
poliinfo_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="poliinfo",
                      qtype="5")

#Doesn't Matter Whether Vote Or Not	M001520	M025171	M045146
df <- anes2k %>% select(M001520,M025171,M045146) %>%
  mutate(y1 = as.numeric(M001520), y2 = as.numeric(M025171),
         y3 = as.numeric(M045146)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_))
votematter_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="votematter",
                      qtype="5")

#Public Officials Don't Care	M001527	M025172	M045147
df <- anes2k %>% select(M001527,M025172,M045147) %>%
  mutate(y1 = as.numeric(M001527), y2 = as.numeric(M025172),
         y3 = as.numeric(M045147)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=3, "4"=4, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=3, "4"=4, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=3, "4"=4, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
dontcare_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="dontcare",
                        qtype="3") 

#People Like Me Have No Say	M001528	M025173	M045148
df <- anes2k %>% select(M001528,M025173,M045148) %>%
  mutate(y1 = as.numeric(M001528), y2 = as.numeric(M025173),
         y3 = as.numeric(M045148)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_))
nosay_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="nosay",
                      qtype="5") 

#Govt Run By Big Interests/Benefit Of All	M001536	M025176	M045151
df <- anes2k %>% select(M001536,M025176,M045151) %>%
  mutate(y1 = as.numeric(M001536), y2 = as.numeric(M025176),
         y3 = as.numeric(M045151)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
govbiz_model <- fmm(waves= c("y1", "y2", "y3"),
                   data=df, cov_estimator="binom", iterations=1000,
                   n_chains=5, burn=500, var_name="govbiz",
                   qtype="3") 

#People can be trusted M001475	M025101	M045158
df <- anes2k %>% select(M001475,M025101,M045158) %>%
  mutate(y1 = as.numeric(M001475), y2 = as.numeric(M025101),
         y3 = as.numeric(M045158)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
trustanes_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=1000,
                    n_chains=5, burn=500, var_name="trustanes",
                    qtype="3")  

#People Take Advantage/Be Fair	M001476	M025102	M045159
df <- anes2k %>% select(M001476,M025102,M045159) %>%
  mutate(y1 = as.numeric(M001476), y2 = as.numeric(M025102),
         y3 = as.numeric(M045159)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
fairanes_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="fairanes",
                       qtype="3")  

#People Helpful/Looking Out For Themselves	M001477	M025103	M045160
df <- anes2k %>% select(M001477,M025103,M045160) %>%
  mutate(y1 = as.numeric(M001477), y2 = as.numeric(M025103),
         y3 = as.numeric(M045160)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
helpfulanes_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="helpfulanes",
                      qtype="3")  

# 2000 Presidential Elect Fair/Unfair	M001291	M023113	M045004
df <- anes2k %>% select(M001291,M023113,M045004) %>%
  mutate(y1 = as.numeric(M001291), y2 = as.numeric(M023113),
         y3 = as.numeric(M045004)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_, "0"=NA_real_))
fair2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="fair2k",
                         qtype="5")  


#thermometer GWB
df <- anes2k %>% select(M001294,M025043,M045007) %>%
  mutate(y1 = as.numeric(M001294), y2 = as.numeric(M025043),
         y3 = as.numeric(M045007)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermgwb2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="thermgwb2k",
                        qtype="5")


#thermometer Cheney
df <- anes2k %>% select(M000367,M023011,M045009) %>%
  mutate(y1 = as.numeric(M000367), y2 = as.numeric(M023011),
         y3 = as.numeric(M045009)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermcheney2k_model <- fmm(waves= c("y1", "y2", "y3"),
                           data=df, cov_estimator="binom", iterations=1000,
                           n_chains=5, burn=500, var_name="thermcheney2k",
                           qtype="5")

#thermometer Gore
df <- anes2k %>% select(M001293,M023012,M045010) %>%
  mutate(y1 = as.numeric(M001293), y2 = as.numeric(M023012),
         y3 = as.numeric(M045010)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermgore2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermgore2k",
                         qtype="5")
#thermometer Nader
df <- anes2k %>% select(M001295,M023014,M045012) %>%
  mutate(y1 = as.numeric(M001295), y2 = as.numeric(M023014),
         y3 = as.numeric(M045012)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermnader2k_model <- fmm(waves= c("y1", "y2", "y3"),
                          data=df, cov_estimator="binom", iterations=1000,
                          n_chains=5, burn=500, var_name="thermnader2k",
                          qtype="5")

#thermometer Jackson
df <- anes2k %>% select(M001296,M023018,M045016) %>%
  mutate(y1 = as.numeric(M001296), y2 = as.numeric(M023018),
         y3 = as.numeric(M045016)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermjksn2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermjksn2k",
                         qtype="5")

#thermometer Hillary
df <- anes2k %>% select(M000368,M023020,M045018) %>%
  mutate(y1 = as.numeric(M000368), y2 = as.numeric(M023020),
         y3 = as.numeric(M045018)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermhill2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermhill2k",
                         qtype="5")
#thermometer Supreme Court
df <- anes2k %>% select(M001304,M025051,M045019) %>%
  mutate(y1 = as.numeric(M001304), y2 = as.numeric(M025051),
         y3 = as.numeric(M045019)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermsc2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="thermsc2k",
                       qtype="5")

#thermometer Congress
df <- anes2k %>% select(M001305,M025052,M045020) %>%
  mutate(y1 = as.numeric(M001305), y2 = as.numeric(M025052),
         y3 = as.numeric(M045020)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermcong2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermcong2k",
                         qtype="5")

#thermometer Military
df <- anes2k %>% select(M001306,M025053,M045021) %>%
  mutate(y1 = as.numeric(M001306), y2 = as.numeric(M025053),
         y3 = as.numeric(M045021)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermmil2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="thermmil2k",
                        qtype="5")

#thermometer Fed
df <- anes2k %>% select(M001307,M025054,M045022) %>%
  mutate(y1 = as.numeric(M001307), y2 = as.numeric(M025054),
         y3 = as.numeric(M045022)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermfed2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="thermfed2k",
                        qtype="5")
#thermometer blacks
df <- anes2k %>% select(M001308,M025055,M045023) %>%
  mutate(y1 = as.numeric(M001308), y2 = as.numeric(M025055),
         y3 = as.numeric(M045023)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermblks2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermblks2k",
                         qtype="5")
#thermometer whites
df <- anes2k %>% select(M001309,M025056,M045024) %>%
  mutate(y1 = as.numeric(M001309), y2 = as.numeric(M025056),
         y3 = as.numeric(M045024)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermwhts2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermwhts2k",
                         qtype="5")

#thermometer conservatives
df <- anes2k %>% select(M001310,M025057,M045025) %>%
  mutate(y1 = as.numeric(M001310), y2 = as.numeric(M025057),
         y3 = as.numeric(M045025)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermcons2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermcons2k",
                         qtype="5")
#thermometer liberals
df <- anes2k %>% select(M001311,M025058,M045026) %>%
  mutate(y1 = as.numeric(M001311), y2 = as.numeric(M025058),
         y3 = as.numeric(M045026)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermlibs2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermlibs2k",
                         qtype="5")
#thermometer unions
df <- anes2k %>% select(M001312,M025059,M045027) %>%
  mutate(y1 = as.numeric(M001312), y2 = as.numeric(M025059),
         y3 = as.numeric(M045027)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermunion2k_model <- fmm(waves= c("y1", "y2", "y3"),
                          data=df, cov_estimator="binom", iterations=1000,
                          n_chains=5, burn=500, var_name="thermunion2k",
                          qtype="5")

#thermometer big biz
df <- anes2k %>% select(M001313,M025060,M045028) %>%
  mutate(y1 = as.numeric(M001313), y2 = as.numeric(M025060),
         y3 = as.numeric(M045028)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermbiz2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="thermbiz2k",
                        qtype="5")
#thermometer poor
df <- anes2k %>% select(M001314,M025061,M045029) %>%
  mutate(y1 = as.numeric(M001314), y2 = as.numeric(M025061),
         y3 = as.numeric(M045029)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermpoor2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermpoor2k",
                         qtype="5")

#thermometer welfare people
df <- anes2k %>% select(M001315,M025062,M045030) %>%
  mutate(y1 = as.numeric(M001315), y2 = as.numeric(M025062),
         y3 = as.numeric(M045030)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermwelf2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermwelf2k",
                         qtype="5")
#thermometer hispanics
df <- anes2k %>% select(M001316,M025063,M045031) %>%
  mutate(y1 = as.numeric(M001316), y2 = as.numeric(M025063),
         y3 = as.numeric(M045031)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermhisp2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermhisp2k",
                         qtype="5")
#thermometer fundamentalists
df <- anes2k %>% select(M001317,M025064,M045032) %>%
  mutate(y1 = as.numeric(M001317), y2 = as.numeric(M025064),
         y3 = as.numeric(M045032)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermfund2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermfund2k",
                         qtype="5")

#thermometer elderly
df <- anes2k %>% select(M001319,M025065,M045033) %>%
  mutate(y1 = as.numeric(M001319), y2 = as.numeric(M025065),
         y3 = as.numeric(M045033)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermelder2k_model <- fmm(waves= c("y1", "y2", "y3"),
                          data=df, cov_estimator="binom", iterations=1000,
                          n_chains=5, burn=500, var_name="thermelder2k",
                          qtype="5")

#thermometer environmentalists
df <- anes2k %>% select(M001320,M025066,M045034) %>%
  mutate(y1 = as.numeric(M001320), y2 = as.numeric(M025066),
         y3 = as.numeric(M045034)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermenviro2k_model <- fmm(waves= c("y1", "y2", "y3"),
                           data=df, cov_estimator="binom", iterations=1000,
                           n_chains=5, burn=500, var_name="thermenviro2k",
                           qtype="5")
#thermometer LGBT
df <- anes2k %>% select(M001321,M025067,M045035) %>%
  mutate(y1 = as.numeric(M001321), y2 = as.numeric(M025067),
         y3 = as.numeric(M045035)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermlgbt2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermlgbt2k",
                         qtype="5")
#thermometer catholics
df <- anes2k %>% select(M001323,M025068,M045036) %>%
  mutate(y1 = as.numeric(M001323), y2 = as.numeric(M025068),
         y3 = as.numeric(M045036)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermcath2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermcath2k",
                         qtype="5")
#thermometer jews
df <- anes2k %>% select(M001324,M025069,M045037) %>%
  mutate(y1 = as.numeric(M001324), y2 = as.numeric(M025069),
         y3 = as.numeric(M045037)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermjews2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermjews2k",
                         qtype="5")
#thermometer protestants
df <- anes2k %>% select(M001325,M025070,M045038) %>%
  mutate(y1 = as.numeric(M001325), y2 = as.numeric(M025070),
         y3 = as.numeric(M045038)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermprot2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermprot2k",
                         qtype="5")
#thermometer feminists
df <- anes2k %>% select(M001326,M025071,M045039) %>%
  mutate(y1 = as.numeric(M001326), y2 = as.numeric(M025071),
         y3 = as.numeric(M045039)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermfems2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermfems2k",
                         qtype="5")
#thermometer asian americans
df <- anes2k %>% select(M001327,M025072,M045040) %>%
  mutate(y1 = as.numeric(M001327), y2 = as.numeric(M025072),
         y3 = as.numeric(M045040)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(887, 888), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(777, 888), 3, NA)))))))
thermasia2k_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="thermasia2k",
                         qtype="5")

#partyid
df <- anes2k %>% select(M000523,M023038X,M045058x) %>%
  mutate(y1 = as.numeric(M000523), y2 = as.numeric(M023038X),
         y3 = as.numeric(M045058x)) %>%
  mutate(y1 = recode(y1, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5,
                     "5"=6, "6"=7, "7"=4, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5,
                     "5"=6, "6"=7, "7"=4, "8"=4, "9"=NA_real_),
         y3 = recode(y3, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5,
                     "5"=6, "6"=7, "7"=4, "8"=4, "9"=NA_real_))
partyid2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="partyid2k",
                       qtype="7")

##############################################################################
############################ START HERE ######################################
##############################################################################

# fsenviro	Environmental Protection-Federal Spending	M000682	M025113	M045068
df <- anes2k %>% select(M000682,M025113X,M045068) %>%
  mutate(y1 = as.numeric(M000682), y2 = as.numeric(M025113X),
         y3 = as.numeric(M045068)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fsenviro2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="fsenviro2k",
                       qtype="3")


# fsaids	AIDS Research-Federal Spending	M000677	M025106	M045069
df <- anes2k %>% select(M000677,M025106X,M045069) %>%
  mutate(y1 = as.numeric(M000677), y2 = as.numeric(M025106X),
         y3 = as.numeric(M045069)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fsaids2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="fsaids2k",
                        qtype="3")

# fswelfare	Welfare-Federal Spending	M000676	M025107	M045070
#Also modified for stability. "Keep same == increase"
df <- anes2k %>% select(M000676,M025107X,M045070) %>%
  mutate(y1 = as.numeric(M000676), y2 = as.numeric(M025107X),
         y3 = as.numeric(M045070)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=2, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=2, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=2, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fswelf2k_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="fswelf2k",
                      qtype="3")

# fsschools	Public Schools Federal Spending	M000683	M025108A	M045071a
df <- anes2k %>% select(M000683,M025108X,M045071a) %>%
  mutate(y1 = as.numeric(M000683), y2 = as.numeric(M025108X),
         y3 = as.numeric(M045071a)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fsschools2k_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="fsschools2k",
                      qtype="3")

# fscrime	Crime-Federal Spending	M000684	M025109	M045072
df <- anes2k %>% select(M000684,M025109X,M045072) %>%
  mutate(y1 = as.numeric(M000684), y2 = as.numeric(M025109X),
         y3 = as.numeric(M045072)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fscrime2k_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="fscrime2k",
                      qtype="3")

# fschild	Child Care-Federal Spending	M000685	M025110	M045073
df <- anes2k %>% select(M000685,M025110X,M045073) %>%
  mutate(y1 = as.numeric(M000685), y2 = as.numeric(M025110X),
         y3 = as.numeric(M045073)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fschild2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="fschild2k",
                       qtype="3")

# fspoor	Aid To Poor People-Federal Spending	M000680	M025115X	M045075a
df <- anes2k %>% select(M000680,M025115X,M045075a) %>%
  mutate(y1 = as.numeric(M000680), y2 = as.numeric(M025115X),
         y3 = as.numeric(M045075a)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fspoor2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="fspoor2k",
                       qtype="3")

# fsforeign	Foreign Aid-Federal Spending	M000678	M025116	M045076
#Different. Keep the same on the same side as increase
df <- anes2k %>% select(M000678,M025116X,M045076) %>%
  mutate(y1 = as.numeric(M000678), y2 = as.numeric(M025116X),
         y3 = as.numeric(M045076)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=2, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=2, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=2, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fsaid2k_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="fsaid2k",
                      qtype="3")

# fssocsec	Social Security-Federal Spending	M000681	M025117	M045077
df <- anes2k %>% select(M000681,M025117X,M045077) %>%
  mutate(y1 = as.numeric(M000681), y2 = as.numeric(M025117X),
         y3 = as.numeric(M045077)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fssocsec2k_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="fssocsec2k",
                     qtype="3")

# fsblacks	Aid To Blacks-Federal Spending	M000687	M025119	M045079
df <- anes2k %>% select(M000687,M025119X,M045079) %>%
  mutate(y1 = as.numeric(M000687), y2 = as.numeric(M025119X),
         y3 = as.numeric(M045079)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
fsblacks2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="fsblacks2k",
                        qtype="3")

# campfin	Extent Of Campaign Finance Reform	M001490	M025146	M045092
df <- anes2k %>% select(M001490,M025146,M045092) %>%
  mutate(y1 = as.numeric(M001490), y2 = as.numeric(M025146),
         y3 = as.numeric(M045092)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=2, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=2, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=2, "5"=4, "7"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
campfin2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="campfin2k",
                        qtype="3")

# trustgov	Trust Govt To Do What Is Right	M001534	M025174	M045149
df <- anes2k %>% select(M001534,M025174,M045149) %>%
  mutate(y1 = as.numeric(M001534), y2 = as.numeric(M025174),
         y3 = as.numeric(M045149)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=2, "3"=4, "4"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=4, "4"=4, "8"=3,
                     "9"=NA_real_, "0"=NA_real_))
trustgov2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="trustgov2k",
                       qtype="3")

# wastetax	Govt Wastes Tax Money	M001535	M025175	M045150
df <- anes2k %>% select(M001535,M025175,M045150) %>%
  mutate(y1 = as.numeric(M001535), y2 = as.numeric(M025175),
         y3 = as.numeric(M045150)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_))
wastetax2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=3, burn=500, var_name="wastetax2k",
                        qtype="3")

# govcrook	How Many In Govt Crooked	M001537	M025177	M045152
df <- anes2k %>% select(M001537,M025177,M045152) %>%
  mutate(y1 = as.numeric(M001537), y2 = as.numeric(M025177),
         y3 = as.numeric(M045152)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_))
crooked2k_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="crooked2k",
                        qtype="3")

# govatt	How Much Elections Make Govt Pay Attn	M001538	M025178	M045153
df <- anes2k %>% select(M001538,M025178,M045153) %>%
  mutate(y1 = as.numeric(M001538), y2 = as.numeric(M025178),
         y3 = as.numeric(M045153)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_))
govatt2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="govatt2k",
                       qtype="3")

# satdemo	Satisfaction With How US Democracy Works	M001651	M025179	M045154
df <- anes2k %>% select(M001651,M025179,M045154) %>%
  mutate(y1 = as.numeric(M001651), y2 = as.numeric(M025179),
         y3 = as.numeric(M045154)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=4, "4"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=4, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_))
satdemo2k_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="satdemo2k",
                      qtype="3")

# imptrelig	Religion Important Part of Rs Life	M000872	M023082	M045173
df <- anes2k %>% select(M000872,M023082,M045173) %>%
  mutate(y1 = as.numeric(M000872), y2 = as.numeric(M023082),
         y3 = as.numeric(M045173)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3,
                     "0"=NA_real_, "9"=NA_real_))
relimpt2k_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="relimpt2k",
                       qtype="3")

anes2kresults <- list(ushome_model, poliqual_model, poliinfo_model, votematter_model, dontcare_model, #5
                      nosay_model, govbiz_model, trustanes_model, fairanes_model, helpfulanes_model, #10
                      fair2k_model, thermgwb2k_model, thermcheney2k_model, thermgore2k_model, thermnader2k_model, #15
                      thermjksn2k_model, thermhill2k_model, thermsc2k_model, thermcong2k_model, thermmil2k_model, #20
                      thermfed2k_model, thermblks2k_model, thermwhts2k_model, thermcons2k_model, thermlibs2k_model, #25
                      thermunion2k_model, thermbiz2k_model, thermpoor2k_model, thermwelf2k_model,  thermhisp2k_model, #30
                      thermfund2k_model, thermelder2k_model, thermenviro2k_model, thermlgbt2k_model, thermcath2k_model, #35
                      thermjews2k_model, thermprot2k_model, thermfems2k_model, thermasia2k_model, partyid2k_model, #40
                      fsenviro2k_model, fsaids2k_model, fswelf2k_model, fsschools2k_model, fscrime2k_model, #45
                      fschild2k_model, fspoor2k_model, fsaid2k_model, fssocsec2k_model, fsblacks2k_model, #50
                      campfin2k_model, trustgov2k_model, wastetax2k_model, crooked2k_model, govatt2k_model, #55
                      satdemo2k_model, relimpt2k_model) #60

#store
# save(anes2kresults, file = "~/Dropbox/hill_kreisi/results/anes2kresults.Rdata")
