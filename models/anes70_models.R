

#Anes 1972-1976 panel
# Available at: https://electionstudies.org/data-center/1972-1976-merged-file/

#local load (for Kevin)
#anes7 <- read_dta("~/Dropbox/data/anes/anes7276/anes_mergedfile_1972to1976.dta")

# Help minority scale	
# 720629	742296	763264
df <- anes7 %>% select(V720002, V720629,V742296,V763264) %>%
  mutate(y1 = as.numeric(V720629), y2 = as.numeric(V742296),
         y3 = as.numeric(V763264)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_)) %>%
  filter(!is.na(y1) | !is.na(y2) | !is.na(y3))
helpblk7_model <- fmm(waves= c("y1", "y2", "y3"), 
                          data=df, cov_estimator="binom", iterations=2500,
                          n_chains=5, burn=500, var_name="helpblk7",
                          qtype="7")


# busing	
# 720202	742288	763257
df <- anes7 %>% select(V720002, V720202,V742288,V763257) %>%
  mutate(y1 = as.numeric(V720202), y2 = as.numeric(V742288),
         y3 = as.numeric(V763257)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_)) %>%
  filter(!is.na(y1) | !is.na(y2) | !is.na(y3))
busing7_model <- fmm(waves= c("y1", "y2", "y3"), 
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="busing7",
                      qtype="7")

# job guar	
# 721067	742265	763758
df <- anes7 %>% select(V721067,V742265,V763758) %>%
  mutate(y1 = as.numeric(V721067), y2 = as.numeric(V742265),
         y3 = as.numeric(V763758)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
jobguar7_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="jobguar7",
                     qtype="7")

# urban unrest	
# 720670	742273	763767
df <- anes7 %>% select(V720670,V742273,V763767) %>%
  mutate(y1 = as.numeric(V720670), y2 = as.numeric(V742273),
         y3 = as.numeric(V763767)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
urbunrest7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="urbunrest7",
                      qtype="7")

# rights of accused	
# 720621	742281	763248
df <- anes7 %>% select(V720621,V742281,V763248) %>%
  mutate(y1 = as.numeric(V720621), y2 = as.numeric(V742281),
         y3 = as.numeric(V763248)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
accused7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="accused7",
                        qtype="7")

# equal role	
# 720232	742302	763787
df <- anes7 %>% select(V720232,V742302,V763787) %>%
  mutate(y1 = as.numeric(V720232), y2 = as.numeric(V742302),
         y3 = as.numeric(V763787)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
eqrole7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="eqrole7",
                        qtype="7")

# trust	
# 720581	742400	763745
df <- anes7 %>% select(V720581,V742400,V763745) %>%
  mutate(y1 = as.numeric(V720581), y2 = as.numeric(V742400),
         y3 = as.numeric(V763745)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_))
trust7_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="trust7",
                     qtype="3")

# helpful	
# 720582	742401	763746
df <- anes7 %>% select(V720582,V742401,V763746) %>%
  mutate(y1 = as.numeric(V720582), y2 = as.numeric(V742401),
         y3 = as.numeric(V763746)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_))
helpful7_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="helpful7",
                    qtype="3")


# take advantage	
# 720583	742402	763747
df <- anes7 %>% select(V720583,V742402,V763747) %>%
  mutate(y1 = as.numeric(V720583), y2 = as.numeric(V742402),
         y3 = as.numeric(V763747)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "8"=3, "0"=NA_real_, "9"=NA_real_))
takeadv7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="takeadv7",
                      qtype="3")

# run for few	
# 720572	742231	763164
df <- anes7 %>% select(V720572,V742231,V763164) %>%
  mutate(y1 = as.numeric(V720572), y2 = as.numeric(V742231),
         y3 = as.numeric(V763164)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
runfew7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="runfew7",
                      qtype="3")

# trust gov	
# 720090	742230	763163

# no say	
# 720269	742222	763815
df <- anes7 %>% select(V720269,V742222,V763815) %>%
  mutate(y1 = as.numeric(V720269), y2 = as.numeric(V742222),
         y3 = as.numeric(V763815)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
nosay7_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="nosay7",
                     qtype="3")

# vote say	
# 720560	742223	763816
df <- anes7 %>% select(V720560,V742222,V763815) %>%
  mutate(y1 = as.numeric(V720560), y2 = as.numeric(V742222),
         y3 = as.numeric(V763815)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
votesay7_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="votesay7",
                    qtype="3")
# so complicated	
# 720561	742224	763817
df <- anes7 %>% select(V720561,V742224,V763817) %>%
  mutate(y1 = as.numeric(V720561), y2 = as.numeric(V742224),
         y3 = as.numeric(V763817)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
complicated7_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="complicated7",
                    qtype="3")

# wallace libcon	
# 720655	742307	763292
df <- anes7 %>% select(V720655,V742307,V763292) %>%
  mutate(y1 = as.numeric(V720655), y2 = as.numeric(V742307),
         y3 = as.numeric(V763292)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
wallacelibcon7_model <- fmm(waves= c("y1", "y2", "y3"),
                          data=df, cov_estimator="binom", iterations=2500,
                          n_chains=5, burn=500, var_name="wallacelibcon7",
                          qtype="7")

# demlibcon	
# 720656	742309	763289
df <- anes7 %>% select(V720656,V742309,V763289) %>%
  mutate(y1 = as.numeric(V720656), y2 = as.numeric(V742309),
         y3 = as.numeric(V763289)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
demslibcon7_model <- fmm(waves= c("y1", "y2", "y3"),
                            data=df, cov_estimator="binom", iterations=2500,
                            n_chains=5, burn=500, var_name="demslibcon7",
                            qtype="7")

# replibcon	
# 720657	742310	763290
df <- anes7 %>% select(V720657,V742310,V763290) %>%
  mutate(y1 = as.numeric(V720657), y2 = as.numeric(V742310),
         y3 = as.numeric(V763290)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
repslibcon7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="repslibcon7",
                         qtype="7")

# libcon	
# 720652	742305	763286
df <- anes7 %>% select(V720652,V742305,V763286) %>%
  mutate(y1 = as.numeric(V720652), y2 = as.numeric(V742305),
         y3 = as.numeric(V763286)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
libcon7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="libcon7",
                         qtype="7")

# polsmrt	
# 720092	742232	763165
df <- anes7 %>% select(V720092,V742232,V763165) %>%
  mutate(y1 = as.numeric(V720092), y2 = as.numeric(V742232),
         y3 = as.numeric(V763165)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
polsmrt7_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="polsmrt7",
                     qtype="3")


# poldontcare	
# 720272	742225	763818
df <- anes7 %>% select(V720272,V742225,V763818) %>%
  mutate(y1 = as.numeric(V720272), y2 = as.numeric(V742225),
         y3 = as.numeric(V763818)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
poldontcare7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="poldontcare7",
                      qtype="3")

# losetouch	
# 720273	742226	763819
df <- anes7 %>% select(V720273,V742226,V763819) %>%
  mutate(y1 = as.numeric(V720273), y2 = as.numeric(V742226),
         y3 = as.numeric(V763819)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
losetouch7_model <- fmm(waves= c("y1", "y2", "y3"),
                          data=df, cov_estimator="binom", iterations=2500,
                          n_chains=5, burn=500, var_name="losetouch7",
                          qtype="3")
# runforall	
# 720572	742231	763164
df <- anes7 %>% select(V720572,V742231,V763164) %>%
  mutate(y1 = as.numeric(V720572), y2 = as.numeric(V742231),
         y3 = as.numeric(V763164)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
runforall7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="runforall7",
                        qtype="3")

# interestvote	
# 720564	742227	763820
df <- anes7 %>% select(V720564, V742227,V763820) %>%
  mutate(y1 = as.numeric(V720564), y2 = as.numeric(V742227),
         y3 = as.numeric(V763820)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
interestvote7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="interestvote7",
                        qtype="3")

# jobguardem	
# 720176	742269	763244
df <- anes7 %>% select(V720176,V742269,V763244) %>%
  mutate(y1 = as.numeric(V720176), y2 = as.numeric(V742269),
         y3 = as.numeric(V763244)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
jobguardem7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="jobguardem7",
                      qtype="7")

# jobguarrep	
# 720177	742270	763245
df <- anes7 %>% select(V720177,V742270,V763245) %>%
  mutate(y1 = as.numeric(V720177), y2 = as.numeric(V742270),
         y3 = as.numeric(V763245)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
jobguarrep7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="jobguarrep7",
                         qtype="7")

# unrestdem	
# 720674	742277	763770
df <- anes7 %>% select(V720674,V742277,V763770) %>%
  mutate(y1 = as.numeric(V720674), y2 = as.numeric(V742277),
         y3 = as.numeric(V763770)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
unrestdem7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="unrestdem7",
                         qtype="7")

# unrestrep	
# 720675	742278	763771
df <- anes7 %>% select(V720675,V742278,V763771) %>%
  mutate(y1 = as.numeric(V720675), y2 = as.numeric(V742278),
         y3 = as.numeric(V763771)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
unrestrep7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="unrestrep7",
                         qtype="7")

# blksptdem	
# 720633	742300	763267
df <- anes7 %>% select(V720633,V742300,V763267) %>%
  mutate(y1 = as.numeric(V720633), y2 = as.numeric(V742300),
         y3 = as.numeric(V763267)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
blksprtdem7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="blksprtdem7",
                        qtype="7")

# blksptrep	
# 720634	742301	763268
df <- anes7 %>% select(V720634,V742301,V763268) %>%
  mutate(y1 = as.numeric(V720634), y2 = as.numeric(V742301),
         y3 = as.numeric(V763268)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
blksprtrep7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="blksprtrep7",
                         qtype="7")

# busingdem	
# 720206	742292	763260
df <- anes7 %>% select(V720206,V742292,V763260) %>%
  mutate(y1 = as.numeric(V720206), y2 = as.numeric(V742292),
         y3 = as.numeric(V763260)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
busingdem7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="busingdem7",
                         qtype="7")
# busingrep	
# 720207	742293	763262
df <- anes7 %>% select(V720207,V742293,V763261) %>%
  mutate(y1 = as.numeric(V720207), y2 = as.numeric(V742293),
         y3 = as.numeric(V763261)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
busingrep7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="busingrep7",
                        qtype="7")

# accuseddem	
# 720625	742284	763251
df <- anes7 %>% select(V720625,V742284,V763251) %>%
  mutate(y1 = as.numeric(V720625), y2 = as.numeric(V742284),
         y3 = as.numeric(V763251)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
accuseddem7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="accuseddem7",
                        qtype="7")
# accusedrep	
# 720626	742285	763252
df <- anes7 %>% select(V720626,V742285,V763252) %>%
  mutate(y1 = as.numeric(V720626), y2 = as.numeric(V742285),
         y3 = as.numeric(V763252)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "0"=NA_real_, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "0"=NA_real_, "9"=NA_real_))
accusedrep7_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="accusedrep7",
                         qtype="7")

# thermblkmil	
# 720727	742372	763841
df <- anes7 %>% select(V720727,V742372,V763841) %>%
  mutate(y1 = as.numeric(V720727), y2 = as.numeric(V742372),
         y3 = as.numeric(V763841)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermblkmil7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermblkmil7",
                        qtype="5")

# thermcivrts	
# 720729	742373	763843
df <- anes7 %>% select(V720729,V742373,V763843) %>%
  mutate(y1 = as.numeric(V720729), y2 = as.numeric(V742373),
         y3 = as.numeric(V763843)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermcivrts7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermcivrts7",
                        qtype="5")

# thermbiz	
# 720707	742356	763821
df <- anes7 %>% select(V720707,V742356,V763821) %>%
  mutate(y1 = as.numeric(V720707), y2 = as.numeric(V742356),
         y3 = as.numeric(V763821)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermbiz7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermbiz7",
                        qtype="5")


# thermpoor	
# 720708	742357	763822
df <- anes7 %>% select(V720708,V742357,V763822) %>%
  mutate(y1 = as.numeric(V720708), y2 = as.numeric(V742357),
         y3 = as.numeric(V763822)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermpoor7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermpoor7",
                        qtype="5")


# thermlibs	
# 720709	742358	763823
df <- anes7 %>% select(V720709,V742358,V763823) %>%
  mutate(y1 = as.numeric(V720709), y2 = as.numeric(V742358),
         y3 = as.numeric(V763823)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermlibs7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermlibs7",
                        qtype="5")

# thermradst	
# 720713	742359	763827
df <- anes7 %>% select(V720713,V742359,V763827) %>%
  mutate(y1 = as.numeric(V720713), y2 = as.numeric(V742359),
         y3 = as.numeric(V763827)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermradst7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermradst7",
                        qtype="5")

# thermcops	
# 720714	742360	763828
df <- anes7 %>% select(V720714,V742360,V763828) %>%
  mutate(y1 = as.numeric(V720714), y2 = as.numeric(V742360),
         y3 = as.numeric(V763828)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermcops7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermcops7",
                        qtype="5")

# thermmil	
# 720717	742362	763831
df <- anes7 %>% select(V720717,V742362,V763831) %>%
  mutate(y1 = as.numeric(V720717), y2 = as.numeric(V742362),
         y3 = as.numeric(V763831)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermmil7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermmil7",
                        qtype="5")

# thermwhts	
# 720718	742363	763846
df <- anes7 %>% select(V720718,V742363,V763846) %>%
  mutate(y1 = as.numeric(V720718), y2 = as.numeric(V742363),
         y3 = as.numeric(V763846)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermwhts7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermwhts7",
                        qtype="5")

# thermdems	
# 720719	742364	763833
df <- anes7 %>% select(V720719,V742364,V763833) %>%
  mutate(y1 = as.numeric(V720719), y2 = as.numeric(V742364),
         y3 = as.numeric(V763833)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermdems7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermdems7",
                        qtype="5")

# thermblks	
# 720720	742365	763832
df <- anes7 %>% select(V720720,V742365,V763832) %>%
  mutate(y1 = as.numeric(V720720), y2 = as.numeric(V742365),
         y3 = as.numeric(V763832)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermblks7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermblks7",
                        qtype="5")

# thermreps	
# 720721	742366	763835
df <- anes7 %>% select(V720721,V742366,V763835) %>%
  mutate(y1 = as.numeric(V720721), y2 = as.numeric(V742366),
         y3 = as.numeric(V763835)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermreps7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermreps7",
                        qtype="5")

# thermunion	
# 720722	742367	763836
df <- anes7 %>% select(V720722,V742367,V763836) %>%
  mutate(y1 = as.numeric(V720722), y2 = as.numeric(V742367),
         y3 = as.numeric(V763836)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermunion7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermunion7",
                        qtype="5")

# thermyoung	
# 720723	742368	763837
df <- anes7 %>% select(V720723,V742368,V763837) %>%
  mutate(y1 = as.numeric(V720723), y2 = as.numeric(V742368),
         y3 = as.numeric(V763837)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermyoung7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermyoung7",
                        qtype="5")

# thermcons	
# 720724	742369	763838
df <- anes7 %>% select(V720724,V742369,V763838) %>%
  mutate(y1 = as.numeric(V720724), y2 = as.numeric(V742369),
         y3 = as.numeric(V763838)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermcons7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermcons7",
                        qtype="5")

# thermfems	
# 720725	742370	763839
df <- anes7 %>% select(V720725,V742370,V763839) %>%
  mutate(y1 = as.numeric(V720725), y2 = as.numeric(V742370),
         y3 = as.numeric(V763839)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermfems7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermfems7",
                        qtype="5")

# thermgrass	
# 720726	742371	763840
df <- anes7 %>% select(V720726,V742371,V763840) %>%
  mutate(y1 = as.numeric(V720726), y2 = as.numeric(V742371),
         y3 = as.numeric(V763840)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermgrass7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermgrass7",
                        qtype="5")


# protest	
# 720277	742240	763909
df <- anes7 %>% select(V720277,V742240,V763909) %>%
  mutate(y1 = as.numeric(V720277), y2 = as.numeric(V742240),
         y3 = as.numeric(V763909)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
protest7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="protest7",
                        qtype="3")

# thermhump	
# 720264	742352	763300
df <- anes7 %>% select(V720264,V742352,V763300) %>%
  mutate(y1 = as.numeric(V720264), y2 = as.numeric(V742352),
         y3 = as.numeric(V763300)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermhump7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermhump7",
                        qtype="5")


# thermwallace	
# 720253	742338	763297
df <- anes7 %>% select(V720253,V742338,V763297) %>%
  mutate(y1 = as.numeric(V720253), y2 = as.numeric(V742338),
         y3 = as.numeric(V763297)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermwallace7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermwallace7",
                        qtype="5")

# thermscoop	
# 720259	742348	763303
df <- anes7 %>% select(V720259,V742348,V763303) %>%
  mutate(y1 = as.numeric(V720259), y2 = as.numeric(V742348),
         y3 = as.numeric(V763303)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermscoop7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermscoop7",
                        qtype="5")

# thermted	
# 720258	742347	763302
df <- anes7 %>% select(V720258,V742347,V763302) %>%
  mutate(y1 = as.numeric(V720258), y2 = as.numeric(V742347),
         y3 = as.numeric(V763302)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermted7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermted7",
                        qtype="5")

# thermnixon	
# 720255	742354	763307
df <- anes7 %>% select(V720255,V742354,V763307) %>%
  mutate(y1 = as.numeric(V720255), y2 = as.numeric(V742354),
         y3 = as.numeric(V763307)) %>%
  mutate(y1 = ifelse(y1 == 99, NA_real_, ifelse(y1 == 98, 3,
              ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2,
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80, 5, NA))))))),
         y2 = ifelse(y2 == 99, NA_real_, ifelse(y2 == 98, 3,
              ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2,
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80, 5, NA))))))),
         y3 = ifelse(y3 == 99, NA_real_, ifelse(y3 == 98, 3,
              ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2,
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80, 5, NA))))))))
thermnixon7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermnixon7",
                        qtype="5")


# btrplan	
# 720565	742396	763736
df <- anes7 %>% select(V720565,V742396,V763736) %>%
  mutate(y1 = as.numeric(V720565), y2 = as.numeric(V742396),
         y3 = as.numeric(V763736)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
btrplan7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="btrplan7",
                        qtype="3")

# carryout	
# 720566	742397	763737
df <- anes7 %>% select(V720566,V742397,V763737) %>%
  mutate(y1 = as.numeric(V720566), y2 = as.numeric(V742397),
         y3 = as.numeric(V763737)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "3"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
carryout7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="carryout7",
                      qtype="3")
# workout	
# 720280	742398	763738
df <- anes7 %>% select(V720280,V742398,V763738) %>%
  mutate(y1 = as.numeric(V720280), y2 = as.numeric(V742398),
         y3 = as.numeric(V763738)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "3"=3, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "3"=3, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "3"=3, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
workout7_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="workout7",
                       qtype="3")
# runlife	
# 720281	742399	763739
df <- anes7 %>% select(V720281,V742399,V763739) %>%
  mutate(y1 = as.numeric(V720281), y2 = as.numeric(V742399),
         y3 = as.numeric(V763739)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "3"=3, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "3"=3, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "5"=4, "3"=3, "7"=3, "8"=3, "0"=NA_real_, "9"=NA_real_))
runlife7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="runlife7",
                      qtype="3")

# partyid	
# 720140	742204	763174
df <- anes7 %>% select(V720140,V742204,V763174) %>%
  mutate(y1 = as.numeric(V720140), y2 = as.numeric(V742204),
         y3 = as.numeric(V763174)) %>%
  mutate(y1 = recode(y1, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, "6"=7,
                     "7"=4, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, "6"=7,
                     "7"=4, "8"=4, "9"=NA_real_),
         y3 = recode(y3, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, "6"=7,
                     "7"=4, "8"=4, "9"=NA_real_))
partyid7_model <- fmm(waves= c("y1", "y2", "y3"),
                            data=df, cov_estimator="binom", iterations=2500,
                            n_chains=5, burn=500, var_name="partyid7",
                            qtype="7")

# influnion	V720737	V742379	V763566
df <- anes7 %>% select(V720737,V742379,V763566) %>%
  mutate(y1 = as.numeric(V720737), y2 = as.numeric(V742379),
         y3 = as.numeric(V763566)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
influnion7_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="influnion7",
                      qtype="3")

# inflpoor	V720738	V742380	V763567
df <- anes7 %>% select(V720738,V742380,V763567) %>%
  mutate(y1 = as.numeric(V720738), y2 = as.numeric(V742380),
         y3 = as.numeric(V763567)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflpoor7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflpoor7",
                        qtype="3")

# inflbiz	V720V744	V742383	V763573
df <- anes7 %>% select(V720V744,V742383,V763573) %>%
  mutate(y1 = as.numeric(V720V744), y2 = as.numeric(V742383),
         y3 = as.numeric(V763573)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflbiz7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflbiz7",
                        qtype="3")

# inflblks	V720V745	V742384	V763570
df <- anes7 %>% select(V720V745,V742384,V763570) %>%
  mutate(y1 = as.numeric(V720V745), y2 = as.numeric(V742384),
         y3 = as.numeric(V763570)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflblks7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflblks7",
                        qtype="3")

# infllibs	V720V746	V742385	V763575
df <- anes7 %>% select(V720V746,V742385,V763575) %>%
  mutate(y1 = as.numeric(V720V746), y2 = as.numeric(V742385),
         y3 = as.numeric(V763575)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
infllibs7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="infllibs7",
                        qtype="3")

# inflyoung	V720V749	V742388	V763578
df <- anes7 %>% select(V720V749,V742388,V763578) %>%
  mutate(y1 = as.numeric(V720V749), y2 = as.numeric(V742388),
         y3 = as.numeric(V763578)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflyoung7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflyoung7",
                        qtype="3")

# inflwomen	V720750	V742389	V763579
df <- anes7 %>% select(V720750,V742389,V763579) %>%
  mutate(y1 = as.numeric(V720750), y2 = as.numeric(V742389),
         y3 = as.numeric(V763579)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflwomen7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflwomen7",
                        qtype="3")

# inflreps	V720752	V742390	V763581
df <- anes7 %>% select(V720752,V742390,V763581) %>%
  mutate(y1 = as.numeric(V720752), y2 = as.numeric(V742390),
         y3 = as.numeric(V763581)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflreps7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflreps7",
                        qtype="3")

# inflwelf	V720753	V742391	V763582
df <- anes7 %>% select(V720753,V742391,V763582) %>%
  mutate(y1 = as.numeric(V720753), y2 = as.numeric(V742391),
         y3 = as.numeric(V763582)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflwelf7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflwelf7",
                        qtype="3")

# inflold	V720755	V742393	V763584
df <- anes7 %>% select(V720755,V742393,V763584) %>%
  mutate(y1 = as.numeric(V720755), y2 = as.numeric(V742393),
         y3 = as.numeric(V763584)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
inflold7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="inflold7",
                        qtype="3")

# infldems	V720756	V742394	V763585
df <- anes7 %>% select(V720756,V742394,V763585) %>%
  mutate(y1 = as.numeric(V720756), y2 = as.numeric(V742394),
         y3 = as.numeric(V763585)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2,"1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "8"=3, "9"=NA_real_, "0"=NA_real_))
infldems7_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="infldems7",
                        qtype="3")

anes70_results <- list(helpblk7_model, busing7_model, jobguar7_model, urbunrest7_model, accused7_model, #5
                       eqrole7_model, trust7_model, helpful7_model, takeadv7_model, runfew7_model, #10
                       nosay7_model, votesay7_model, complicated7_model, wallacelibcon7_model, demslibcon7_model, #15
                       repslibcon7_model, libcon7_model, polsmrt7_model, poldontcare7_model, losetouch7_model, #20
                       runforall7_model, interestvote7_model, jobguardem7_model, jobguarrep7_model, unrestdem7_model, #25 
                       unrestrep7_model, blksprtdem7_model, blksprtrep7_model, busingdem7_model, busingrep7_model, #30
                       accuseddem7_model, accusedrep7_model, thermblkmil7_model, thermcivrts7_model, thermbiz7_model, #35
                       thermpoor7_model, thermlibs7_model, thermradst7_model, thermcops7_model, thermmil7_model, #40
                       thermwhts7_model, thermdems7_model, thermblks7_model, thermreps7_model, thermunion7_model, #45
                       thermyoung7_model, thermcons7_model, thermfems7_model, thermgrass7_model, protest7_model, #50
                       thermhump7_model, thermwallace7_model, thermscoop7_model, thermted7_model, thermnixon7_model, #55
                       btrplan7_model, carryout7_model, workout7_model, runlife7_model, partyid7_model, #60
                       influnion7_model, inflpoor7_model, inflbiz7_model, inflblks7_model, infllibs7_model, #65 
                       inflyoung7_model, inflwomen7_model, inflreps7_model, inflwelf7_model, inflold7_model, #70
                       infldems7_model) #71
#local save:
#save(anes70_results,  file = "~/Dropbox/hill_kreisi/results/anes70_results.Rdata")

#removing models
rm(helpblk7_model, busing7_model, jobguar7_model, urbunrest7_model, accused7_model,
   eqrole7_model, trust7_model, helpful7_model, takeadv7_model, runfew7_model,
   nosay7_model, votesay7_model, complicated7_model, wallacelibcon7_model,
   demslibcon7_model, repslibcon7_model, libcon7_model, polsmrt7_model,
   poldontcare7_model, losetouch7_model, runforall7_model,
   interestvote7_model, jobguardem7_model, jobguarrep7_model,
   unrestdem7_model, unrestrep7_model, blksprtdem7_model, 
   blksprtrep7_model, busingdem7_model, busingrep7_model,
   accuseddem7_model, accusedrep7_model, thermblkmil7_model, 
   thermcivrts7_model, thermbiz7_model, thermpoor7_model,
   thermlibs7_model, thermradst7_model, thermcops7_model, thermmil7_model,
   thermwhts7_model, thermdems7_model, thermblks7_model, thermreps7_model, 
   thermunion7_model, thermyoung7_model, thermcons7_model, thermfems7_model,
   thermgrass7_model, protest7_model, thermhump7_model, thermwallace7_model,
   thermscoop7_model, thermted7_model, thermnixon7_model,
   btrplan7_model, carryout7_model, workout7_model, runlife7_model, 
   partyid7_model,
   influnion7_model, inflpoor7_model, inflbiz7_model, inflblks7_model,
   infllibs7_model, inflyoung7_model, inflwomen7_model, inflreps7_model,
   inflwelf7_model, inflold7_model, infldems7_model, anes7)
