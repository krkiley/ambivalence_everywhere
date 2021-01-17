


#Load in data file
anes5 <- read_dta("~/Dropbox/data/anes/anes5660/anes_mergedfile_1956to1960.dta") 

anes5 <- anes5 %>% 
  mutate(id = 1:nrow(anes5))


#Knowledge
know <- anes5 %>%
  select(V560142, V560145, V580317, V580318, V580394, V580395, V600608, V600609,
         V600611, V600612, V600613, V600615, V600798) %>%
  zap_labels() %>%
  mutate(V560142 = recode(V560142, "1"=0, "2"=0, "3"=0, "4"=1, "5"=1, "8"=0, "9"=NA_real_),
         V560145 = recode(V560142, "1"=1, "2"=1, "3"=0, "4"=0, "5"=0, "8"=0, "9"=NA_real_),
         V580317 = recode(V560142, "1"=1, "2"=0, "8"=0, "9"=NA_real_),
         V580318 = recode(V560142, "1"=1, "2"=0, "8"=0, "9"=NA_real_),
         V580394 = recode(V580394, "1"=0, "2"=1, "3"=2, "4"=2, "5"=2, "9"=NA_real_),
         V580395 = recode(V580395, "1"=1, "2"=0, "3"=0, "4"=1, "5"=0, "8"=0, "9"=NA_real_),
         V600608 = ifelse(V600608 == 71, 1, 0),
         V600609 = ifelse(V600609 == 99, NA_real_,
                          ifelse(V600609 >= 45 & V600609 <= 50, 1,
                                 ifelse(V600609 == 14, 1, 0))),
         V600611 = ifelse(V600611 %in% c()))


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


#democrats closer on getting job		560034	580322	600621
#democrats closer on help poorer countries		560043	580328	600627
#democrats more likely help negroes get fair treatment		560046	580330	600629
#democrats closer on help schools		560055	580326	600625
#democrats closer on keep soldiers overseas		560058	580331	600631
#democrats closer on leave electricity to market		560061	580320	600619
#democrats stay out of schools		560076	580336	600639

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


#close to labor union		560245	580506	600719
#interest in labor union		560246	580507	600720
#close to negroes		560250	580514	600725
#close to catholics		560254	580510	
#interest in how catholics are doing		560255	580511	

anes50_results <- list(helpblk5_model, integrate5_model, govjob5_model, poorcntry5_model,
                   bldschls5_model, fightcomm5_model, prvtpwr5_model, stayhome5_model, 
                   badluck5_model, partyid5_model)
save(anes50_results,  file = "~/Dropbox/hill_kreisi/results/anes50_results.Rdata")


a5res <- vector(mode = "list", length = length(anes50_results))
for (i in 1:length(anes50_results)) {
  var <- anes50_results[[i]]$model_info$var
  qtype <- anes50_results[[i]]$model_info$qtype
  a5res[[i]] <- anes50_results[[i]]$pattern_param_summary %>%
    mutate(var = var, qtype = qtype, ds = "anes5660")
  
}

