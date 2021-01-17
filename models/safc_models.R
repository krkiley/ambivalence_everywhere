



safc80 <- read_dta("~/Dropbox/data/saf/saf_child80.dta")
safc85 <- read_dta("~/Dropbox/data/saf/saf_child85.dta")
safc93 <- read_dta("~/Dropbox/data/saf/saf_child93.dta")


safc <- left_join(safc80, safc85, by = "FAMID62") %>%
  left_join(safc93, by = "FAMID62")

df <- safc %>%
  select(V2633, V4323, V12030) %>%
  mutate(y1 = V2633,
         y2 = V4323, 
         y3 = V12030) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_))
cnolive_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="cnolive",
                      qtype="5")



df <- safc %>%
  select(V2631, V4321, V12028) %>%
  mutate(y1 = V2631,
         y2 = V4321, 
         y3 = V12028) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3, "9"=NA_real_)) 
cnosex_model <- fmm(waves= c("y1", "y2", "y3"),
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="cnosex",
                   qtype="5")


df <- safc %>%
  select(V2235, V4011, V12014) %>%
  mutate(y1 = V2235,
         y2 = V4011, 
         y3 = V12014) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
cnodiv_model <- fmm(waves= c("y1", "y2", "y3"),
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="cnodiv",
                   qtype="5")


df <- safc %>%
  select(V2234, V4010, V12013) %>%
  mutate(y1 = V2234,
         y2 = V4010, 
         y3 = V12013) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_))
cmandec_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="cmandec",
                     qtype="5")


df <- safc %>%
  select(V2237, V4013, V12016) %>%
  mutate(y1 = V2237,
         y2 = V4013, 
         y3 = V12016) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cmenswrk_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="cmenswrk",
                       qtype="5")


df <- safc %>%
  select(V2238, V4014, V12017) %>%
  mutate(y1 = V2238,
         y2 = V4014, 
         y3 = V12017) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cnohelp_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="cnohelp",
                       qtype="5")


df <- safc %>%
  select(V2242, V4018, V12021) %>%
  mutate(y1 = V2242,
         y2 = V4018, 
         y3 = V12021) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
chelphsbnd_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="chelphsbnd",
                     qtype="5")


df <- safc %>%
  select(V2239, V4015, V12018) %>%
  mutate(y1 = V2239,
         y2 = V4015, 
         y3 = V12018) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cwrkwarm_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="cwrkwarm",
                        qtype="5")


df <- safc %>%
  select(V2241, V4017, V12020) %>%
  mutate(y1 = V2241,
         y2 = V4017, 
         y3 = V12020) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_))
cstayhome_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=2500,
                        n_chains=5, burn=500, var_name="cstayhome",
                        qtype="5")


df <- safc %>%
  select(V2628, V4318, V12025) %>%
  mutate(y1 = V2628,
         y2 = V4318, 
         y3 = V12025) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cmarhap_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="cmarhap",
                       qtype="5")


df <- safc %>%
  select(V2630, V4320, V12027) %>%
  mutate(y1 = V2630,
         y2 = V4320, 
         y3 = V12027) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cfewgd_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="cfewgd",
                     qtype="5")

df <- safc %>%
  select(V2636, V4326, V12033) %>%
  mutate(y1 = V2636,
         y2 = V4326, 
         y3 = V12033) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cadvsngle_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="cadvsngle",
                    qtype="5")

df <- safc %>%
  select(V2629, V4319, V12026) %>%
  mutate(y1 = V2629,
         y2 = V4319, 
         y3 = V12026) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
ccohabok_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="ccohabok",
                       qtype="5")


df <- safc %>%
  select(V2635, V4325, V12032) %>%
  mutate(y1 = V2635,
         y2 = V4325, 
         y3 = V12032) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cpremarok_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="cpremarok",
                      qtype="5")


df <- safc %>%
  select(V2632, V4322, V12029) %>%
  mutate(y1 = V2632,
         y2 = V4322, 
         y3 = V12029) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cdivbest_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=2500,
                       n_chains=5, burn=500, var_name="cdivbest",
                       qtype="5")


df <- safc %>%
  select(V2236, V4012, V12015) %>%
  mutate(y1 = V2236,
         y2 = V4012, 
         y3 = V12015) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
cokclub_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="cokclub",
                      qtype="5")

df <- safc %>%
  select(V2240, V4016, V12019) %>%
  mutate(y1 = V2240,
         y2 = V4016, 
         y3 = V12019) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
cmanearn_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="cmanearn",
                     qtype="5")


df <- safc %>%
  select(V2240, V4016, V12019) %>%
  mutate(y1 = V2240,
         y2 = V4016, 
         y3 = V12019) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
cmanearn_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="cmanearn",
                      qtype="5")


df <- safc %>%
  select(V2634, V4324, V12031) %>%
  mutate(y1 = V2634,
         y2 = V4324, 
         y3 = V12031) %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "8"=3, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "9"=NA_real_)) 
cmarbet_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="cmarbet",
                      qtype="5")



safc_results <- list(cnolive_model, cnosex_model, cnodiv_model, cmandec_model, cmenswrk_model,#5
                     cnohelp_model, chelphsbnd_model, cstayhome_model, cmarhap_model, cfewgd_model, #10 
                     cadvsngle_model, ccohabok_model, cpremarok_model, cdivbest_model, cokclub_model, #15
                     cmanearn_model, cmarbet_model, cwrkwarm_model) #18
save(safc_results, file = "~/Dropbox/hill_kreisi/results/safcesults.Rdata")

safc_results[[17]] <- cmarbet_model

safcres <- vector(mode = "list", length = length(safc_results))
for (i in 1:length(safc_results)) {
  var <- safc_results[[i]]$model_info$var
  qtype <- safc_results[[i]]$model_info$qtype
  safcres[[i]] <- safc_results[[i]]$pattern_param_summary %>%
    mutate(var = var, qtype = qtype)
  
}

bind_rows(safcres) %>%
  filter(param %in% c("pi2", "alpha1")) %>%
  select(param, mean, var, qtype) %>%
  spread(param, mean) %>%
  ggplot(aes(x = pi2, y = alpha1, fill = qtype, label = var)) +
  geom_point(shape = 21) + 
  geom_text_repel() + 
  geom_vline(xintercept = .6, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2) +
  geom_hline(yintercept = .25, linetype = 2) +
  theme_bw() + 
  labs(x = "Proportion of people with vascillating views",
       y = "Agreement among stable view holders") 
