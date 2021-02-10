

#1956-60 ANES Panel
# Panel available: https://electionstudies.org/data-center/1956-1960-panel-study/

#local load (for Kevin)
# anes5 <- read_dta("~/Dropbox/data/anes/anes5660/anes_mergedfile_1956to1960.dta") 

#negroes get fair treatment 		560044	580329	600628
df <- anes5 %>% select(V560044,V580329,V600628, id) %>%
  mutate(y1 = as.numeric(V560044), y2 = as.numeric(V580329),
         y3 = as.numeric(V600628)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
helpblk5_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="helpblk5",
                      qtype="5")

#government stay out of question about whether black/white children go to school		560074	580333	600636
df <- anes5 %>% select(V560074,V580333,V600636, id) %>%
  mutate(y1 = as.numeric(V560074), y2 = as.numeric(V580333),
         y3 = as.numeric(V600636)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
integrate5_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="integrate5",
                      qtype="5")

#government should see to it that everybody gets a job		560032	580321	600620
df <- anes5 %>% select(V560032,V580321,V600620, id) %>%
  mutate(y1 = as.numeric(V560032), y2 = as.numeric(V580321),
         y3 = as.numeric(V600620)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
govjob5_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="govjob5",
                        qtype="5")

#economic help to poorer countries		560041	580327	600626
df <- anes5 %>% select(V560041,V580327,V600626, id) %>%
  mutate(y1 = as.numeric(V560041), y2 = as.numeric(V580327),
         y3 = as.numeric(V600626)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
poorcntry5_model <- fmm(waves= c("y1", "y2", "y3"), id="id",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="poorcntry5",
                     qtype="5")

#government Help building schools		560053	580325	600624
df <- anes5 %>% select(V560053,V580325,V600624, id) %>%
  mutate(y1 = as.numeric(V560053), y2 = as.numeric(V580325),
         y3 = as.numeric(V600624)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
bldschls5_model <- fmm(waves= c("y1", "y2", "y3"),id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="bldschls5",
                        qtype="5")

#keep soldiers overseas to fight communism		560056	580331	600630
df <- anes5 %>% select(V560056,V580331,V600630, id) %>%
  mutate(y1 = as.numeric(V560056), y2 = as.numeric(V580331),
         y3 = as.numeric(V600630)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
fightcomm5_model <- fmm(waves= c("y1", "y2", "y3"),id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="fightcomm5",
                       qtype="5")

#leave electricity/power to private market		560059	580319	600618
df <- anes5 %>% select(V560059,V580319,V600618, id) %>%
  mutate(y1 = as.numeric(V560059), y2 = as.numeric(V580319),
         y3 = as.numeric(V600618)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
prvtpwr5_model <- fmm(waves= c("y1", "y2", "y3"),id="id",
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="prvtpwr5",
                        qtype="5")

#better off stayed home		560035	580323	600622
df <- anes5 %>% select(V560035,V580323,V600622, id) %>%
  mutate(y1 = as.numeric(V560035), y2 = as.numeric(V580323),
         y3 = as.numeric(V600622)) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "7"=3, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                     "0"=3, "8"=3, "9"=NA_real_))
stayhome5_model <- fmm(waves= c("y1", "y2", "y3"),id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="stayhome5",
                      qtype="5")

#fair share of bad luck		560289	580460	600805
df <- anes5 %>% select(V560289,V580460,V600805, id) %>%
  mutate(y1 = as.numeric(V560289), y2 = as.numeric(V580460),
         y3 = as.numeric(V600805)) %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=3, "4"=4, "5"=4,
                     "0"=3, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=4, "3"=3, "5"=2, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=4, "3"=3, "5"=2, "8"=3, "9"=NA_real_))
badluck5_model <- fmm(waves= c("y1", "y2", "y3"),id="id",
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="badluck5",
                       qtype="3")

#partyid		560088	580360	600835
df <- anes5 %>% select(V560088,V580360,V600835, id) %>%
  mutate(y1 = as.numeric(V560088), y2 = as.numeric(V580360),
         y3 = as.numeric(V600835)) %>%
  mutate(y1 = recode(y1, "0"=1, "1"=2, "2"=3, "3"=4,
                     "4"=5, "5"=6, "6"=7, "7"=3,
                     "8"=3, "9"=NA_real_),
         y2 = recode(y2, "0"=1, "1"=2, "2"=3, "3"=4,
                     "4"=5, "5"=6, "6"=7, "7"=3,
                     "8"=3, "9"=NA_real_),
         y3 = recode(y3, "0"=1, "1"=2, "2"=3, "3"=4,
                     "4"=5, "5"=6, "6"=7, "7"=3,
                     "8"=3, "9"=NA_real_))
partyid5_model <- fmm(waves= c("y1", "y2", "y3"),id="id",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="partyid5",
                      qtype="7")



anes50_results <- list(helpblk5_model, integrate5_model, govjob5_model, poorcntry5_model,
                   bldschls5_model, fightcomm5_model, prvtpwr5_model, stayhome5_model, 
                   badluck5_model, partyid5_model)
save(anes50_results,  file = "~/Dropbox/hill_kreisi/results/anes50_results.Rdata")

