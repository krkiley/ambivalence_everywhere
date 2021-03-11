
# 1992-1996 ANES panel
# Available here: https://electionstudies.org/data-center/1992-1997-merged-file/

#local load
# anes <- read_dta("~/Dropbox/data/anes/anes9297/anes_mergedfile_1992to1997.dta") 

#Make my own ID variable
anes <- anes %>%
  mutate(id = 1:nrow(anes))


#Anes 92-96

# 7-point Government/Private Insurance Plan V923716	V940950	V960479 
df <- anes %>%
  select(V923716,V940950,V960479, id) %>%
  mutate(y1 = as.numeric(V923716), y2 = as.numeric(V940950),
         y3 = as.numeric(V960479)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4)) 
govins_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="govins",
                    qtype="7")

#Government Job Guarantee		V923718	V940930	V960483
df <- anes %>% select(V923718,V940930,V960483, id) %>%
  mutate(y1 = as.numeric(V923718), y2 = as.numeric(V940930),
         y3 = as.numeric(V960483)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4)) 
jobguar_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="jobguar",
                    qtype="7")

#Spending/Services Scale		V923701	V940940	V960450
df <- anes %>% select(V923701,V940940,V960450, id) %>%
  mutate(y1 = as.numeric(V923701), y2 = as.numeric(V940940),
         y3 = as.numeric(V960450)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4)) 
servspend_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="servspend",
                     qtype="7")

#7pt - Government Help Blacks/Help Themselves		V923724	V940936	V960487
df <- anes %>% select(V923724,V940936,V960487, id) %>%
  mutate(y1 = as.numeric(V923724), y2 = as.numeric(V940936),
         y3 = as.numeric(V960487)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4)) 
govblks_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="govblks",
                       qtype="7")

#Affirmative action - 4 point V925936	V941002	V961209
df <- anes %>% select(V925936,V941002,V961209, id) %>%
  mutate(y1 = as.numeric(V925936), y2 = as.numeric(V941002),
         y3 = as.numeric(V961209)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
affactanes9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="affactanes9",
                     qtype="5")

#Society should ensure equal opportunity		V926024	V940914	V961229
df <- anes %>% select(V926024,V940914,V961229, id) %>%
  mutate(y1 = as.numeric(V926024), y2 = as.numeric(V940914),
         y3 = as.numeric(V961229)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
eqop_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="eqop",
                        qtype="5")


#Too far in pushing equality		V926025	V940915	V961230
df <- anes %>% select(V926025,V940915,V961230, id) %>%
  mutate(y1 = as.numeric(V926025), y2 = as.numeric(V940915),
         y3 = as.numeric(V961230)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
toofar_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="toofar",
                  qtype="5")


#Big problem that we don't give equal chance		V926029	V940916	V961231
df <- anes %>% select(V926029,V940916,V961231, id) %>%
  mutate(y1 = as.numeric(V926029), y2 = as.numeric(V940916),
         y3 = as.numeric(V961231)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
nochance_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="nochance",
                    qtype="5")

#Problem that some people have more of a chance V926027 V940918 V961233
df <- anes %>% select(V926027,V940918,V961233, id) %>%
  mutate(y1 = as.numeric(V926027), y2 = as.numeric(V940918),
         y3 = as.numeric(V961233)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
morechance_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="morechance",
                      qtype="5")

#Should worry less about how equal people are		V926026	V940917	V961232
df <- anes %>% select(V926026,V940917,V961232, id) %>%
  mutate(y1 = as.numeric(V926026), y2 = as.numeric(V940917),
         y3 = as.numeric(V961232)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
worryless_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="worryless",
                        qtype="5")

#Would have fewer problems is people treated equally		V926028	V940919	V961234
df <- anes %>% select(V926028,V940919,V961234, id) %>%
  mutate(y1 = as.numeric(V926028), y2 = as.numeric(V940919),
         y3 = as.numeric(V961234)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
treateq_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="treateq",
                       qtype="5")

#Newer lifestyles are contributing to breakdown of society		V926118	V941029	V961247
df <- anes %>% select(V926118,V941029,V961247, id) %>%
  mutate(y1 = as.numeric(V926118), y2 = as.numeric(V941029),
         y3 = as.numeric(V961247)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
newstyles_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="newstyles",
                     qtype="5")


#We should be more tolerant		V926116	V941032	V961250
df <- anes %>% select(V926116,V941032,V961250, id) %>%
  mutate(y1 = as.numeric(V926116), y2 = as.numeric(V941032),
         y3 = as.numeric(V961250)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
moretol_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="moretol",
                       qtype="5")

#World changing and we should adjust morals		V926115	V941030	V961248
df <- anes %>% select(V926115,V941030,V961248, id) %>%
  mutate(y1 = as.numeric(V926115), y2 = as.numeric(V941030),
         y3 = as.numeric(V961248)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
adjmoral_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="adjmoral",
                     qtype="5")

#Fewer problems if more emphaiss on traditional family ties		V926117	V941031	V961249
df <- anes %>% select(V926117,V941031,V961249, id) %>%
  mutate(y1 = as.numeric(V926117), y2 = as.numeric(V941031),
         y3 = as.numeric(V961249)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_))
tradties_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="tradties",
                      qtype="5")

#Equal Role/Woman's Place in Home		V923801	V940928	V960543
df <- anes %>% select(V923801,V940928,V960543, id) %>%
  mutate(y1 = as.numeric(V923801), y2 = as.numeric(V940928),
         y3 = as.numeric(V960543)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                     "8"=4, "9"=NA_real_, "0"=4)) 
eqroles_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="eqroles",
                      qtype="7")

#liberal-conservative V923509	V940839	V961269
df <- anes %>% select(V923509,V940839,V961269, id) %>%
  mutate(y1 = as.numeric(V923509), y2 = as.numeric(V940839),
         y3 = as.numeric(V961269)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, 
                    "8"=4, "0"=4, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, 
                     "8"=4, "0"=4, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, 
                     "8"=4, "0"=4, "9"=NA_real_)) 
libcon90_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="libcon90",
                     qtype="7")

#Summary ID 7 point		V923634	V940655	V960420
df <- anes %>% select(V923634,V940655,V960420, id) %>%
  mutate(y1 = as.numeric(V923634), y2 = as.numeric(V940655),
         y3 = as.numeric(V960420)) %>%
  mutate(y1 = recode(y1, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, "6"=7,
                     "7"=4, "8"=4, "9"=NA_real_),
         y2 = recode(y2, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, "6"=7,
                     "7"=4, "8"=4, "9"=NA_real_),
         y3 = recode(y3, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, "6"=7,
                     "7"=4, "8"=4, "9"=NA_real_)) 
partyid90_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="partyid90",
                      qtype="7")

#Consider religion important		V923820	V941043	V960571
df <- anes %>% select(V923820,V941043,V960571, id) %>%
  mutate(y1 = as.numeric(V923820), y2 = as.numeric(V941043),
         y3 = as.numeric(V960571)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "8"=3, "9"=NA_real_, "0"=NA_real_)) 
relimptanes_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="relimptanes",
                     qtype="3")

#Officials do not care		V926103	V941037	V961244
df <- anes %>% select(V926103,V941037,V961244, id) %>%
  mutate(y1 = as.numeric(V926103), y2 = as.numeric(V941037),
         y3 = as.numeric(V961244)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
dontcare90_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="dontcare90",
                     qtype="5")


#Politics/government too complex to understand		V926104	V941039	V961246
df <- anes %>% select(V926104,V941039,V961246, id) %>%
  mutate(y1 = as.numeric(V926104), y2 = as.numeric(V941039),
         y3 = as.numeric(V961246)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "8"=3, "9"=NA_real_, "0"=NA_real_)) 
complex90_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="complex90",
                        qtype="5")

#Government runs by big interestGovernment runs by big interest V926122	V941035	V961253
df <- anes %>% select(V926122,V941035,V961253, id) %>%
  mutate(y1 = as.numeric(V926122), y2 = as.numeric(V941035),
         y3 = as.numeric(V961253)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "5"=2, "1"=4, "8"=3, "9"=NA_real_, "0"=NA_real_)) 
govbiz90_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="govbiz90",
                       qtype="3")

#Term limits V923747	V940651	V960412

df <- anes %>% select(V923747,V940651,V960412, id) %>%
  mutate(y1 = as.numeric(V923747), y2 = as.numeric(V940651),
         y3 = as.numeric(V960412)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "7"=3, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "7"=3, "8"=3, "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "7"=3, "8"=3, "9"=NA_real_, "0"=NA_real_)) 
termlimits_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="termlimits",
                      qtype="3")


#Approve of job congress is doing?
df <- anes %>% select(V925950,V940321,V960271, id) %>%
  mutate(y1 = as_factor(V925950), y2 = as_factor(V940321),
         y3 = as_factor(V960271)) %>%
  mutate(y1 = recode(y1, "1. APPROVE STRONGLY"=1, "2. APPROVE NOT STRONGLY"=2,
                     "4. DISAPPROVE NOT STRONGLY"=4, "5. DISAPPROVE STRONGLY"=5,
                     "8. DK"=3),
         y2 = recode(y2, "1. STRONGLY APPROVE"=1, "2. NOT STRONGLY APPROVE"=2,
                     "4. NOT STRONGLY DISAPPROVE"=4, "5. STRONGLY DISAPPROVE"=5,
                     "8. DK"=3),
         y3 = recode(y3, "1. Approve strongly"=1, "2. Approve not strongly"=2, 
                     "4. Disapprove not strongly"=4, "5. Disapprove strongly"=5,
                     "8. DK"=3)) 
congapprove_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="congapprove",
                     qtype="5")


#better off stay home?
df <- anes %>% select(V923604,V941019,V960410, id) %>%
  mutate(y1 = as_factor(V923604), y2 = as_factor(V941019),
         y3 = as_factor(V960410)) %>%
  mutate(y1 = recode(y1, "1. AGREE"=2, "5. DISAGREE"=4, "8. DK"=3),
         y2 = recode(y2, "1. AGREE"=2, "5. DISAGREE"=4, "8. DONT KNOW"=3),
         y3 = recode(y3, "1. Agree"=2, "5. Disagree"=4, "8. DK"=3)) 
stayhome_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="stayhome",
                         qtype="3")


#thermometer clinton
df <- anes %>% select(V925302,V940223,V961019, id) %>%
  mutate(y1 = as.numeric(V925302), y2 = as.numeric(V940223),
         y3 = as.numeric(V961019)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
              ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
              ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
              ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
              ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
              ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
              ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermclint_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="thermclint",
                      qtype="5")

#thermometer perot
df <- anes %>% select(V925303,V940224,V961021, id) %>%
  mutate(y1 = as.numeric(V925303), y2 = as.numeric(V940224),
         y3 = as.numeric(V961021)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermperot_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermperot",
                        qtype="5")

#thermometer gore
df <- anes %>% select(V923309,V940227,V960275, id) %>%
  mutate(y1 = as.numeric(V923309), y2 = as.numeric(V940227),
         y3 = as.numeric(V960275)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermgore_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermgore",
                        qtype="5")

#thermometer hillary
df <- anes %>% select(V923313,V940229,V960281, id) %>%
  mutate(y1 = as.numeric(V923313), y2 = as.numeric(V940229),
         y3 = as.numeric(V960281)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermhill_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermhill",
                       qtype="5")

#thermometer jackson
df <- anes %>% select(V923316,V940228,V960283, id) %>%
  mutate(y1 = as.numeric(V923316), y2 = as.numeric(V940228),
         y3 = as.numeric(V960283)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermjksn_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermjksn",
                       qtype="5")
#thermometer blacks
df <- anes %>% select(V925323,V940305,V961029, id) %>%
  mutate(y1 = as.numeric(V925323), y2 = as.numeric(V940305),
         y3 = as.numeric(V961029)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermblks_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermblks",
                       qtype="5")

#thermometer whites
df <- anes %>% select(V925333,V940313,V961030, id) %>%
  mutate(y1 = as.numeric(V925333), y2 = as.numeric(V940313),
         y3 = as.numeric(V961030)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermwhts_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermwhts",
                       qtype="5")

#thermometer liberals
df <- anes %>% select(V925326,V940311,V961032, id) %>%
  mutate(y1 = as.numeric(V925326), y2 = as.numeric(V940311),
         y3 = as.numeric(V961032)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermlibs_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermlibs",
                       qtype="5")

#thermometer conservatives
df <- anes %>% select(V925319,V940306,V961031, id) %>%
  mutate(y1 = as.numeric(V925319), y2 = as.numeric(V940306),
         y3 = as.numeric(V961031)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermcons_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermcons",
                       qtype="5")

#thermometer labor unions
df <- anes %>% select(V925316,V940307,V961033, id) %>%
  mutate(y1 = as.numeric(V925316), y2 = as.numeric(V940307),
         y3 = as.numeric(V961033)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermunion_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermunion",
                       qtype="5")



#thermometer big business
df <- anes %>% select(V925322,V940314,V961034, id) %>%
  mutate(y1 = as.numeric(V925322), y2 = as.numeric(V940314),
         y3 = as.numeric(V961034)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermbiz_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="thermbiz",
                        qtype="5")

#thermometer poor people
df <- anes %>% select(V961035,V940312,V961035, id) %>%
  mutate(y1 = as.numeric(V925320), y2 = as.numeric(V940312),
         y3 = as.numeric(V961035)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermpoor_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="thermpoor",
                      qtype="5")

#thermometer people on welfare
df <- anes %>% select(V925318,V940309,V961036, id) %>%
  mutate(y1 = as.numeric(V925318), y2 = as.numeric(V940309),
         y3 = as.numeric(V961036)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermwelf_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermwelf",
                       qtype="5")
#thermometer hispanics
df <- anes %>% select(V925327,V940304,V961037, id) %>%
  mutate(y1 = as.numeric(V925327), y2 = as.numeric(V940304),
         y3 = as.numeric(V961037)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermhisp_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermhisp",
                       qtype="5")

#thermometer environmentalists
df <- anes %>% select(V925329,V940310,V961041, id) %>%
  mutate(y1 = as.numeric(V925329), y2 = as.numeric(V940310),
         y3 = as.numeric(V961041)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermenviro_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="thermenviro",
                       qtype="5")


#themometer women's movement
df <- anes %>% select(V925324,V940308,V961039, id) %>%
  mutate(y1 = as.numeric(V925324), y2 = as.numeric(V940308),
         y3 = as.numeric(V961039)) %>%
  mutate(y1 = ifelse(y1 <= 20, 1, ifelse(y1 > 20 & y1 < 50, 2, 
                                         ifelse(y1 == 50, 3, ifelse(y1 > 50 & y1 < 80, 4,
                                                                    ifelse(y1 >= 80 & y1 <=100, 5, ifelse(y1 %in% c(997, 998), 3, NA)))))),
         y2 = ifelse(y2 <= 20, 1, ifelse(y2 > 20 & y2 < 50, 2, 
                                         ifelse(y2 == 50, 3, ifelse(y2 > 50 & y2 < 80, 4,
                                                                    ifelse(y2 >= 80 & y2 <=100, 5, ifelse(y2 %in% c(997, 998), 3, NA)))))),
         y3 = ifelse(y3 <= 20, 1, ifelse(y3 > 20 & y3 < 50, 2, 
                                         ifelse(y3 == 50, 3, ifelse(y3 > 50 & y3 < 80, 4,
                                                                    ifelse(y3 >= 80 & y3 <=100, 5, ifelse(y3 %in% c(997, 998), 3, NA)))))))
thermwomlib_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="thermwomlib",
                         qtype="5")

#better econ
df <- anes %>% select(V923545,V940829,V960397, id) %>%
  mutate(y1 = as.numeric(V923545), y2 = as.numeric(V940829),
         y3 = as.numeric(V960397)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=3, "4"=3, "7"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=3, "5"=4, "6"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=3, "2"=4, "8"=3, "0"=NA_real_, "9"=NA_real_))
betterecon_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                         data=df, cov_estimator="binom", iterations=2500,
                         n_chains=5, burn=500, var_name="betterecon",
                         qtype="3")
#better foreign
df <- anes %>% select(V923546,V940832,V960398, id) %>%
  mutate(y1 = as.numeric(V923546), y2 = as.numeric(V940832),
         y3 = as.numeric(V960398)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=3, "4"=3, "7"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=3, "5"=4, "6"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=3, "2"=4, "8"=3, "0"=NA_real_, "9"=NA_real_))
betterfrgn_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="betterfrgn",
                        qtype="3")

#better health care
df <- anes %>% select(V923548,V940833,V960399, id) %>%
  mutate(y1 = as.numeric(V923548), y2 = as.numeric(V940833),
         y3 = as.numeric(V960399)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=3, "4"=3, "7"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=3, "5"=4, "6"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=3, "2"=4, "8"=3, "0"=NA_real_, "9"=NA_real_))
betterhlth_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="betterhlth",
                        qtype="3")
#more likely raise taxes
df <- anes %>% select(V923550,V940828,V960407, id) %>%
  mutate(y1 = as.numeric(V923550), y2 = as.numeric(V940828),
         y3 = as.numeric(V960407)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "3"=3, "8"=3, "9"=NA_real_), #democrats 1, reps 5
         y2 = recode(y2, "1"=2, "5"=4, "3"=3, "8"=3, "9"=NA_real_), 
         y3 = recode(y3, "1"=2, "3"=3, "2"=4, "8"=3, "0"=NA_real_, "9"=NA_real_))
raisetax_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="raisetax",
                        qtype="3")
#clinton spend/serv
df <- anes %>% select(V923703,V940941,V960453, id) %>%
  mutate(y1 = as.numeric(V923703), y2 = as.numeric(V940941),
         y3 = as.numeric(V960453)) %>%
  mutate(y1 = recode(y1, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "8"=4, "0"=NA_real_, "9"=NA_real_))
clintspend_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="clintspend",
                      qtype="7")
######### Start here
#republicans spend/serv
df <- anes %>% select(V923704,V940945,V960462, id) %>%
  mutate(y1 = as.numeric(V923704), y2 = as.numeric(V940945),
         y3 = as.numeric(V960462)) %>%
  mutate(y1 = recode(y1, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "8"=4, "0"=NA_real_, "9"=NA_real_))
repspend_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="repspend",
                        qtype="7")
#democrats spend/serv
df <- anes %>% select(V923705,V940944,V960461, id) %>%
  mutate(y1 = as.numeric(V923705), y2 = as.numeric(V940944),
         y3 = as.numeric(V960461)) %>%
  mutate(y1 = recode(y1, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "8"=4, "0"=NA_real_, "9"=NA_real_))
demspend_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="demspend",
                      qtype="7")
#rep lib/con
df <- anes %>% select(V923517,V940848,V960380, id) %>%
  mutate(y1 = as.numeric(V923517), y2 = as.numeric(V940848),
         y3 = as.numeric(V960380)) %>%
  mutate(y1 = recode(y1, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "8"=4, "0"=NA_real_, "9"=NA_real_))
replibcon_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="replibcon",
                      qtype="7")
#dem lib/con
df <- anes %>% select(V923518,V940847,V960379, id) %>%
  mutate(y1 = as.numeric(V923518), y2 = as.numeric(V940847),
         y3 = as.numeric(V960379)) %>%
  mutate(y1 = recode(y1, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y2 = recode(y2, "8"=4, "0"=NA_real_, "9"=NA_real_),
         y3 = recode(y3, "8"=4, "0"=NA_real_, "9"=NA_real_))
demlibcon_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="demlibcon",
                       qtype="7")

### More

# fs.child	FS - Child Care		V923813	V940824	V960564
df <- anes %>% select(V923813,V940824,V960564, id) %>%
  mutate(y1 = as.numeric(V923813), y2 = as.numeric(V940824),
         y3 = as.numeric(V960564)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fschild9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="fschild9",
                       qtype="3")

# fs.crime	FS - Crime		V923814	V940825	V960563
df <- anes %>% select(V923814,V940825,V960563, id) %>%
  mutate(y1 = as.numeric(V923814), y2 = as.numeric(V940825),
         y3 = as.numeric(V960563)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fscrime9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="fscrime9",
                      qtype="3")

# fs.aids	FS - AIDS		V923727	V940821	V960498
df <- anes %>% select(V923727,V940821,V960498, id) %>%
  mutate(y1 = as.numeric(V923727), y2 = as.numeric(V940821),
         y3 = as.numeric(V960498)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fsaids9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="fsaids9",
                      qtype="3")

# fs.schools	FS - Public Schools		V923818	V940823	V960562
df <- anes %>% select(V923818,V940823,V960562, id) %>%
  mutate(y1 = as.numeric(V923818), y2 = as.numeric(V940823),
         y3 = as.numeric(V960562)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fsschool9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="fsschool9",
                     qtype="3")

# fs.welfare	FS - Welfare		V923726	V940820	V960497
df <- anes %>% select(V923726,V940820,V960497, id) %>%
  mutate(y1 = as.numeric(V923726), y2 = as.numeric(V940820),
         y3 = as.numeric(V960497)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fswelf9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="fswelf9",
                      qtype="3")

# fs.foodstamps	FS - Food Stamps		V923725	V940822	V960496
df <- anes %>% select(V923725,V940822,V960496, id) %>%
  mutate(y1 = as.numeric(V923725), y2 = as.numeric(V940822),
         y3 = as.numeric(V960496)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fsfood9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="fsfood9",
                      qtype="3")

# fs.enviro	FS - Environment		V923815	V940817	V960561
df <- anes %>% select(V923815,V940817,V960561, id) %>%
  mutate(y1 = as.numeric(V923815), y2 = as.numeric(V940817),
         y3 = as.numeric(V960561)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fsenviro9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="fsenviro9",
                     qtype="3")

# fs.socsec	FS - Social Security		V923811	V940819	V960560
df <- anes %>% select(V923811,V940819,V960560, id) %>%
  mutate(y1 = as.numeric(V923811), y2 = as.numeric(V940819),
         y3 = as.numeric(V960560)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_))
fssocsec9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="fssocsec9",
                       qtype="3")

# inc.imm	Increase/decrease immigration		V926235	V941016	V961325
df <- anes %>% select(V926235,V941016,V961325, id) %>%
  mutate(y1 = as.numeric(V926235), y2 = as.numeric(V941016),
         y3 = as.numeric(V961325)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=2, "4"=4, "5"=4, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=2, "4"=4, "5"=4, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=2, "4"=4, "5"=4, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_))
incimm9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="incimm9",
                       qtype="3")

# homo.mil5	Homosexuals in military (4 point)		V925926	V937331	V961196
df <- anes %>% select(V925926,V937331,V961196, id) %>%
  mutate(y1 = as.numeric(V925926), y2 = as.numeric(V937331),
         y3 = as.numeric(V961196)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_))
homomil9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="homomil9",
                     qtype="5")

# homo.job5	Favor/Oppose Law Protecting Homosexuals from Discrimination (4 point)		V925924	V937327	V961194
df <- anes %>% select(V925924,V937327,V961194, id) %>%
  mutate(y1 = as.numeric(V925924), y2 = as.numeric(V937327),
         y3 = as.numeric(V961194)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "0"=NA_real_, 
                     "8"=3, "9"=NA_real_))
homojob9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="homojob9",
                      qtype="5")

# abortion	Abortion view		V923732	V941014	V960503
df <- anes %>% select(V923732,V941014,V960503, id) %>%
  mutate(y1 = as.numeric(V923732), y2 = as.numeric(V941014),
         y3 = as.numeric(V960503)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=2, "4"=4, "0"=NA_real_, 
                     "6"=4, "7"=NA_real_, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=2, "4"=4, "0"=NA_real_, 
                     "6"=4, "7"=NA_real_, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=2, "4"=4, "0"=NA_real_, 
                     "6"=4, "7"=NA_real_, "8"=3, "9"=NA_real_))
abortion9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="abortion9",
                      qtype="3")

# sch.pray4	Strength of opinion (4-point)		V925946	V941021	V961215
df <- anes %>% select(V925945,V941020,V961214, id) %>%
  mutate(y1 = as.numeric(V925945), y2 = as.numeric(V941020),
         y3 = as.numeric(V961214)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "4"=4, "0"=NA_real_, 
                      "7"=NA_real_, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "0"=NA_real_, 
                     "7"=NA_real_, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "0"=NA_real_, 
                     "7"=NA_real_, "8"=3, "9"=NA_real_))
schpray9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="schpray9",
                       qtype="3")

# relig.impt	Consider religion important		V923820	V941043	V960571
df <- anes %>% select(V923820,V941043,V960571, id) %>%
  mutate(y1 = as.numeric(V923820), y2 = as.numeric(V941043),
         y3 = as.numeric(V960571)) %>%
  mutate(y1 = recode(y1, "1"=2, "5"=4, "0"=NA_real_, 
                     "7"=NA_real_, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=2, "5"=4, "0"=NA_real_, 
                     "7"=NA_real_, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "0"=NA_real_, 
                     "7"=NA_real_, "8"=3, "9"=NA_real_))
relimpt9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="relimpt9",
                      qtype="3")

# view.bible	View on bible		V923824	V941047	V960575
df <- anes %>% select(V923824,V941047,V960575, id) %>%
  mutate(y1 = as.numeric(V923824), y2 = as.numeric(V941047),
         y3 = as.numeric(V960575)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_,
                     "0"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_,
                     "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "7"=4, "8"=3, "9"=NA_real_,
                     "0"=NA_real_))
viewbible9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="viewbible9",
                      qtype="3")

# crooked	How many crooked		V926123	V941036	V961254
df <- anes %>% select(V926123,V941036,V961254, id) %>%
  mutate(y1 = as.numeric(V926123), y2 = as.numeric(V941036),
         y3 = as.numeric(V961254)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=2, "5"=4, "8"=3, "9"=NA_real_,
                     "0"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=2, "5"=4, "8"=3, "9"=NA_real_,
                     "0"=NA_real_),
         y3 = recode(y3, "1"=4, "3"=2, "5"=2, "8"=3, "9"=NA_real_,
                     "0"=NA_real_))
crooked9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="crooked9",
                        qtype="3")

# trust.gov	Can trust government		V926120	V941033	V961251
df <- anes %>% select(V926120,V941033,V961251, id) %>%
  mutate(y1 = as.numeric(V926120), y2 = as.numeric(V941033),
         y3 = as.numeric(V961251)) %>%
  mutate(y1 = recode(y1, "1"=2, "3"=2, "5"=4, "7"=4, "8"=3, 
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "1"=2, "3"=2, "5"=4, "7"=4, "8"=3, 
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=4, "4"=4, "8"=3, 
                     "9"=NA_real_, "0"=NA_real_))
trustgov9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="trustgov9",
                      qtype="3")

# waste.tax	Government wastes taxes		V926121	V941034	V961252
df <- anes %>% select(V926121,V941034,V961252, id) %>%
  mutate(y1 = as.numeric(V926121), y2 = as.numeric(V941034),
         y3 = as.numeric(V961252)) %>%
  mutate(y1 = recode(y1, "5"=2, "1"=4, "3"=4, "8"=3, 
                     "9"=NA_real_, "0"=NA_real_),
         y2 = recode(y2, "5"=2, "1"=4, "3"=4, "8"=3, 
                     "9"=NA_real_, "0"=NA_real_),
         y3 = recode(y3, "1"=2, "3"=4, "5"=4, "8"=3, 
                     "9"=NA_real_, "0"=NA_real_))
wastetax9_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="wastetax9",
                       qtype="3")


anes90results <- list(govins_model, jobguar_model, servspend_model, govblks_model, affactanes9_model, #5
                      eqop_model, toofar_model, nochance_model, morechance_model, worryless_model, #10
                      treateq_model, newstyles_model, moretol_model, adjmoral_model, tradties_model, #15
                      eqroles_model, libcon90_model, partyid90_model, dontcare90_model, complex90_model, #20
                      govbiz90_model, termlimits_model, congapprove_model, stayhome_model, thermclint_model, #25
                      thermgore_model, thermhill_model, thermjksn_model, thermblks_model, thermwhts_model, #30
                      thermcons_model, thermlibs_model, thermunion_model, thermbiz_model, thermpoor_model, #35
                      thermwelf_model, thermhisp_model, thermenviro_model, thermwomlib_model, betterecon_model, #40 
                      betterfrgn_model, betterhlth_model, raisetax_model, clintspend_model, repspend_model, #45
                      demspend_model, replibcon_model, demlibcon_model, fschild9_model, fscrime9_model, #50
                      fsaids9_model, fsschool9_model, fswelf9_model, fsfood9_model, fsenviro9_model, #55
                      fssocsec9_model, incimm9_model, homomil9_model, homojob9_model, abortion9_model, #60
                      schpray9_model, relimpt9_model, viewbible9_model, crooked9_model, trustgov9_model, #65
                      wastetax9_model) #66
#Save locally (for Kevin)
# save(anes90results, file = "~/Dropbox/hill_kreisi/results/anes90results.Rdata")

#Clean up workspace
rm(govins_model, jobguar_model, servspend_model, govblks_model, 
   affactanes9_model, eqop_model, toofar_model, 
   nochance_model, morechance_model, worryless_model, treateq_model,
   newstyles_model, moretol_model, adjmoral_model, tradties_model,
   eqroles_model, libcon90_model, partyid90_model, 
   dontcare90_model, complex90_model, govbiz90_model, termlimits_model,
   congapprove_model, stayhome_model,
   thermclint_model, thermgore_model, thermhill_model, thermjksn_model,
   thermblks_model, thermwhts_model, thermcons_model, thermlibs_model,
   thermunion_model, thermbiz_model, thermpoor_model, thermwelf_model,
   thermhisp_model, thermenviro_model, thermwomlib_model,
   betterecon_model, betterfrgn_model, betterhlth_model, 
   raisetax_model, clintspend_model, repspend_model, demspend_model,
   replibcon_model, demlibcon_model,
   fschild9_model, fscrime9_model, fsaids9_model, fsschool9_model, 
   fswelf9_model, fsfood9_model, fsenviro9_model, fssocsec9_model,
   incimm9_model, homomil9_model, homojob9_model, abortion9_model,
   schpray9_model, relimpt9_model, viewbible9_model,
   crooked9_model, trustgov9_model, wastetax9_model, anes)

  


