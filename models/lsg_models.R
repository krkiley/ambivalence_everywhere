
load("~/Dropbox/data/lsg/lsgfull.Rdata")

#This country would be better off if religion had a greater influence on daily life
df <- lsg %>% select(OP001R6, OP001R7, OP001R8) %>%
  mutate(y1 = OP001R6, y2 = OP001R7,
         y3 = OP001R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) 
betterreg_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="betterreg",
                      qtype="5")

#chldidel
df <- lsg %>% select(OP007R6, OP007R7, OP007R8) %>%
  mutate(y1 = OP007R6, y2 = OP007R7,
         y3 = OP007R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

childrel_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="childrel",
                         qtype="5")

df <- lsg %>% select(OP166R6, OP166R7, OP166R8) %>%
  mutate(y1 = OP166R6, y2 = OP166R7,
         y3 = OP166R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)
relimptlsg_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="relimptlsg",
                      qtype="5")


df <- lsg %>% select(OP016U6, OP016R7, OP016R8) %>%
  mutate(y1 = OP016U6, y2 = OP016R7,
         y3 = OP016R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

godreal_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="godreal",
                     qtype="5")


df <- lsg %>% select(OP161R6, OP161R7, OP161R8) %>%
  mutate(y1 = OP161R6, y2 = OP161R7,
         y3 = OP161R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

adameve_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="adameve",
                     qtype="5")


df <- lsg %>% select(OP013U6, OP013R7, OP013R8) %>%
  mutate(y1 = OP013U6, y2 = OP013R7,
         y3 = OP013R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

dutywork_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="dutywork",
                     qtype="5")


df <- lsg %>% select(OP019R6, OP019R7, OP019R8) %>%
  mutate(y1 = OP019R6, y2 = OP019R7,
         y3 = OP019R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

welflazy_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="welflazy",
                      qtype="5")


df <- lsg %>% select(OP025R6, OP025R7, OP025R8) %>%
  mutate(y1 = OP025R6, y2 = OP025R7,
         y3 = OP025R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

anschal_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="anschal",
                      qtype="5")



df <- lsg %>% select(OP194R6, OP194R7, OP194R8) %>%
  mutate(y1 = OP194R6, y2 = OP194R7,
         y3 = OP194R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

morepatrt_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="morepatrt",
                     qtype="5")

#####################################################################################
#####################################################################################

df <- lsg %>% select(OP182R6, OP182R7, OP182R8) %>%
  mutate(y1 = OP182R6, y2 = OP182R7,
         y3 = OP182R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

fmlyfrst_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="fmlyfrst",
                       qtype="5")

df <- lsg %>% select(OP197R6, OP197R7, OP197R8) %>%
  mutate(y1 = OP197R6, y2 = OP197R7,
         y3 = OP197R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

frpreschlsg_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="frpreschlsg",
                      qtype="5")


df <- lsg %>% select(OP190U4, OP190R5, OP190R6) %>%
  mutate(y1 = OP190U4, y2 = OP190R5,
         y3 = OP190R6) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

flexrules_model <- fmm(waves= c("y1", "y2", "y3"),
                         data=df, cov_estimator="binom", iterations=1000,
                         n_chains=5, burn=500, var_name="flexrules",
                         qtype="5")

df <- lsg %>% select(OP004R6, OP004R7, OP004R8) %>%
  mutate(y1 = OP004R6, y2 = OP004R7,
         y3 = OP004R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

mareql_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="mareql",
                       qtype="5")


df <- lsg %>% select(OP176R6, OP176R7, OP176R8) %>%
  mutate(y1 = OP176R6, y2 = OP176R7,
         y3 = OP176R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

marfmly_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=1000,
                    n_chains=5, burn=500, var_name="marfmly",
                    qtype="5")


df <- lsg %>% select(OP010R6, OP010R7, OP010R8) %>%
  mutate(y1 = OP010R6, y2 = OP010R7,
         y3 = OP010R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

feauth_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="feauth",
                     qtype="5")


df <- lsg %>% select(OP159R6, OP159R7, OP159R8) %>%
  mutate(y1 = OP159R6, y2 = OP159R7,
         y3 = OP159R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

abort_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=1000,
                    n_chains=5, burn=500, var_name="abort",
                    qtype="5")


df <- lsg %>% select(OP165R6, OP165R7, OP165R8) %>%
  mutate(y1 = OP165R6, y2 = OP165R7,
         y3 = OP165R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

fewrkfrce_model <- fmm(waves= c("y1", "y2", "y3"),
                   data=df, cov_estimator="binom", iterations=1000,
                   n_chains=5, burn=500, var_name="fewrkfrce",
                   qtype="5")

df <- lsg %>% select(OP170R6, OP170R7, OP170R8) %>%
  mutate(y1 = OP170R6, y2 = OP170R7,
         y3 = OP170R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

feobey_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="feobey",
                       qtype="5")


df <- lsg %>% select(OP175R6, OP175R7, OP175R8) %>%
  mutate(y1 = OP175R6, y2 = OP175R7,
         y3 = OP175R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

felibrt_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=1000,
                    n_chains=5, burn=500, var_name="felibrt",
                    qtype="5")


df <- lsg %>% select(OP177R6, OP177R7, OP177R8) %>%
  mutate(y1 = OP177R6, y2 = OP177R7,
         y3 = OP177R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

fecareer_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="fecareer",
                     qtype="5")



df <- lsg %>% select(OP196R6, OP196R7, OP196R8) %>%
  mutate(y1 = OP196R6, y2 = OP196R7,
         y3 = OP196R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

sharhswrk_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="sharhswrk",
                      qtype="5")



df <- lsg %>% select(OP028R6, OP028R7, OP028R8) %>%
  mutate(y1 = OP028R6, y2 = OP028R7,
         y3 = OP028R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

discfam_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="discfam",
                       qtype="5")



df <- lsg %>% select(OP031R6, OP031R7, OP031R8) %>%
  mutate(y1 = OP031R6, y2 = OP031R7,
         y3 = OP031R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

algnfam_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="algnfam",
                     qtype="5")


df <- lsg %>% select(OP178R6, OP178R7, OP178R8) %>%
  mutate(y1 = OP178R6, y2 = OP178R7,
         y3 = OP178R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

famweight_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="famweight",
                     qtype="5")


df <- lsg %>% select(OP202N6, OP202R7, OP202R8) %>%
  mutate(y1 = OP202N6, y2 = OP202R7,
         y3 = OP202R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

nostep_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="nostep",
                       qtype="5")



df <- lsg %>% select(OP034R6, OP034R7, OP034R8) %>%
  mutate(y1 = OP034R6, y2 = OP034R7,
         y3 = OP034R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

leavemoney_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=1000,
                    n_chains=5, burn=500, var_name="leavemoney",
                    qtype="5")

df <- lsg %>% select(OP171R6, OP171R7, OP171R8) %>%
  mutate(y1 = OP171R6, y2 = OP171R7,
         y3 = OP171R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

sharact_model <- fmm(waves= c("y1", "y2", "y3"),
                        data=df, cov_estimator="binom", iterations=1000,
                        n_chains=5, burn=500, var_name="sharact",
                        qtype="5")


df <- lsg %>% select(OP180R6, OP180R7, OP180R8) %>%
  mutate(y1 = OP180R6, y2 = OP180R7,
         y3 = OP180R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_))
liveclose_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="liveclose",
                     qtype="5")

df <- lsg %>% select(OP193R6, OP193R7, OP193R8) %>%
  mutate(y1 = OP193R6, y2 = OP193R7,
         y3 = OP193R8) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

careeldr_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="careeldr",
                       qtype="5")


df <- lsg %>% select(OP184U4, OP184R5, OP184R6) %>%
  mutate(y1 = OP184U4, y2 = OP184R5,
         y3 = OP184R6) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

talkback_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="talkback",
                      qtype="5")


df <- lsg %>% select(OP187U4, OP187R5, OP187R6) %>%
  mutate(y1 = OP187U4, y2 = OP187R5,
         y3 = OP187R6) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

chldwrng_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="chldwrng",
                      qtype="5")


df <- lsg %>% select(OP189N4, OP189R5, OP189R6) %>%
  mutate(y1 = OP189N4, y2 = OP189R5,
         y3 = OP189R6) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

chldpref_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="chldpref",
                      qtype="5")



df <- lsg %>% select(OP160N3, OP160R4, OP160R5) %>%
  mutate(y1 = OP160N3, y2 = OP160R4,
         y3 = OP160R5) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_)) %>%
  select(y1, y2, y3)

nukepeace_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="nukepeace",
                      qtype="5")


df <- lsg %>% select(OPNSA17, OP022U2, OP022R3) %>%
  mutate(y1 = OPNSA17, y2 = OP022U2,
         y3 = OP022R3) %>% 
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=5, "2"=4, "3"=2, "4"=1, "9"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=4, "4"=5, "9"=NA_real_))
imptlno_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="imptlno",
                       qtype="5")


###############################################################################

###############################################################################

###############################################################################

# VALUE1	An exciting life (novelty, adventure)			AV010U6	AV010R7	AV010R8	
df <- lsg %>% select(AV010U6, AV010R7, AV010R8) %>%
  mutate(y1=AV010U6, y2=AV010R7, y3=AV010R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) 
valadv_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="valadv",
                     qtype="3")

# VALUE2	Equality (working for social justice for all)		AV011U6	AV011R7	AV011R8	
df <- lsg %>% select(AV011U6, AV011R7, AV011R8) %>%
  mutate(y1=AV011U6, y2=AV011R7, y3=AV011R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valequal_model <- fmm(waves= c("y1", "y2", "y3"),
                    data=df, cov_estimator="binom", iterations=1000,
                    n_chains=5, burn=500, var_name="valequal",
                    qtype="3")

# VALUE3	A sense of accomplishment (achievement)	AV012U6	AV012R7	AV012R8	
df <- lsg %>% select(AV012U6, AV012R7, AV012R8) %>%
  mutate(y1=AV012U6, y2=AV012R7, y3=AV012R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valachieve_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valachieve",
                      qtype="3")

# VALUE4	Financial comfort (enough to have the things you really want in life)		AV013U6	AV013R7	AV013R8	
df <- lsg %>% select(AV013U6, AV013R7, AV013R8) %>%
  mutate(y1=AV013U6, y2=AV013R7, y3=AV013R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valfin_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valfin",
                      qtype="3")

# VALUE5	Respect or recognition from other people		AV014U6	AV014R7	AV014R8	
df <- lsg %>% select(AV014U6, AV014R7, AV014R8) %>%
  mutate(y1=AV014U6, y2=AV014R7, y3=AV014R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valresp_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valresp",
                      qtype="3")

# VALUE6	Religious participation (working with others in your own church or organization)	AV015U6	AV015R7	AV015R8	
df <- lsg %>% select(AV015U6, AV015R7, AV015R8) %>%
  mutate(y1=AV015U6, y2=AV015R7, y3=AV015R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valrelpat_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valrelpat",
                      qtype="3")

# VALUE7	Service (devotion to bettering mankind)		AV016U6	AV016R7	AV016R8	
df <- lsg %>% select(AV016U6, AV016R7, AV016R8) %>%
  mutate(y1=AV016U6, y2=AV016R7, y3=AV016R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valserv_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valserv",
                      qtype="3")

# VALUE8	Friendship (meaningful relations with others who really care)		AV017U6	AV017R7	AV017R8	
df <- lsg %>% select(AV017U6, AV017R7, AV017R8) %>%
  mutate(y1=AV017U6, y2=AV017R7, y3=AV017R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valfriend_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valfriend",
                      qtype="3")

# VALUE9	Family life (working for the well-being of family members)	AV018U6	AV018R7	AV018R8	
df <- lsg %>% select(AV018U6, AV018R7, AV018R8) %>%
  mutate(y1=AV018U6, y2=AV018R7, y3=AV018R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valfam_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valfam",
                      qtype="3")


# VALUE10	An attractive appearance (knowing others admire the way you look)		AV019U6	AV019R7	AV019R8	
#Also slightly different
df <- lsg %>% select(AV019U6, AV019R7, AV019R8) %>%
  mutate(y1=AV019U6, y2=AV019R7, y3=AV019R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(7,8,9), 2, ifelse(y1 %in% c(1,2,3,4,5,6), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(7,8,9), 2, ifelse(y2 %in% c(1,2,3,4,5,6), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(7,8,9), 2, ifelse(y3 %in% c(1,2,3,4,5,6), 4, NA_real_))) 
valappear_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valappear",
                      qtype="3")

# VALUE11	A world at peace (working for peace on earth)		AV020U6	AV020R7	AV020R8	
df <- lsg %>% select(AV020U6, AV020R7, AV020R8) %>%
  mutate(y1=AV020U6, y2=AV020R7, y3=AV020R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valpeace_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valpeace",
                      qtype="3")

# VALUE12	Loyalty to your own (family and loved ones, church or group)	AV021U6	AV021R7	AV021R8	
df <- lsg %>% select(AV021U6, AV021R7, AV021R8) %>%
  mutate(y1=AV021U6, y2=AV021R7, y3=AV021R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valloyal_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valloyal",
                      qtype="3")

# VALUE13	An ethical life (responsible living toward all)	AV022U6	AV022R7	AV022R8	
df <- lsg %>% select(AV022U6, AV022R7, AV022R8) %>%
  mutate(y1=AV022U6, y2=AV022R7, y3=AV022R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valeth_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valeth",
                      qtype="3")

# VALUE14	Possessions (enough things so you can do what you really enjoy doing)	AV023U6	AV023R7	AV023R8	
df <- lsg %>% select(AV023U6, AV023R7, AV023R8) %>%
  mutate(y1=AV023U6, y2=AV023R7, y3=AV023R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_)))
valstuff_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valstuff",
                      qtype="3")

# VALUE15	Patriotism (working for our country)	AV024U6	AV024R7	AV024R8	
df <- lsg %>% select(AV024U6, AV024R7, AV024R8) %>%
  mutate(y1=AV024U6, y2=AV024R7, y3=AV024R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valptrt_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valptrt",
                      qtype="3")

# VALUE16	Personal freedom (independence, free choice, autonomy)	AV025U6	AV025R7	AV025R8	
df <- lsg %>% select(AV025U6, AV025R7, AV025R8) %>%
  mutate(y1=AV025U6, y2=AV025R7, y3=AV025R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valfree_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valfree",
                      qtype="3")

# VALUE17	Skill (being good at something you enjoy doing)	AV026U6	AV026R7	AV026R8	
df <- lsg %>% select(AV026U6, AV026R7, AV026R8) %>%
  mutate(y1=AV026U6, y2=AV026R7, y3=AV026R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valskill_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valskill",
                      qtype="3")

# VALUE18	Career advancement (achieving success in your job or profession)	AV027U6	AV027R7	AV027R8	
df <- lsg %>% select(AV027U6, AV027R7, AV027R8) %>%
  mutate(y1=AV027U6, y2=AV027R7, y3=AV027R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(1,2,3), 2, ifelse(y1 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(1,2,3), 2, ifelse(y2 %in% c(4,5,6,7,8,9), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(1,2,3), 2, ifelse(y3 %in% c(4,5,6,7,8,9), 4, NA_real_))) %>%
  select(y1, y2, y3)
valcareer_model <- fmm(waves= c("y1", "y2", "y3"),
                      data=df, cov_estimator="binom", iterations=1000,
                      n_chains=5, burn=500, var_name="valcareer",
                      qtype="3")


# RESPCOMP	Adult children to provide COMPANIONSHIP or spend time-with elderly parents?	GE001R6	GE001R7	GE001R8
df <- lsg %>% select(GE001R6, GE001R7, GE001R8) %>%
  mutate(y1=GE001R6, y2=GE001R7, y3=GE001R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(4, 5), 2, ifelse(y1 %in% c(1,2,3), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(4, 5), 2, ifelse(y2 %in% c(1,2,3), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(4, 5), 2, ifelse(y3 %in% c(1,2,3), 4, NA_real_))) %>%
  select(y1, y2, y3)
rescomp_model <- fmm(waves= c("y1", "y2", "y3"),
                       data=df, cov_estimator="binom", iterations=1000,
                       n_chains=5, burn=500, var_name="rescomp",
                       qtype="3")

# RESPCHOR	 HOUSEHOLD CHORES and REPAIRS and/or to provide TRANSPORTATION for elderly parents ?	GE002R6	GE002R7	GE002R8
df <- lsg %>% select(GE002R6, GE002R7, GE002R8) %>%
  mutate(y1=GE002R6, y2=GE002R7, y3=GE002R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(4, 5), 2, ifelse(y1 %in% c(1,2,3), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(4, 5), 2, ifelse(y2 %in% c(1,2,3), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(4, 5), 2, ifelse(y3 %in% c(1,2,3), 4, NA_real_))) %>%
  select(y1, y2, y3)
respchor_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="respchor",
                     qtype="3")

# RESPLSTN	To LISTEN to the problems and concerns of elderly parents and to provide ADVICE AND GUIDANCE?	GE003R6	GE003R7	GE003R8
df <- lsg %>% select(GE003R6, GE003R7, GE003R8) %>%
  mutate(y1=GE003R6, y2=GE003R7, y3=GE003R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(4, 5), 2, ifelse(y1 %in% c(1,2,3), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(4, 5), 2, ifelse(y2 %in% c(1,2,3), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(4, 5), 2, ifelse(y3 %in% c(1,2,3), 4, NA_real_))) %>%
  select(y1, y2, y3)
resplstn_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="resplstn",
                     qtype="3")

# RESPCARE	To provide for the PERSONAL and HEALTH CARE needs of elderly parents ?	GE004R6	GE004R7	GE004R8
df <- lsg %>% select(GE004R6, GE004R7, GE004R8) %>%
  mutate(y1=GE004R6, y2=GE004R7, y3=GE004R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(4, 5), 2, ifelse(y1 %in% c(1,2,3), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(4, 5), 2, ifelse(y2 %in% c(1,2,3), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(4, 5), 2, ifelse(y3 %in% c(1,2,3), 4, NA_real_))) %>%
  select(y1, y2, y3)
respcare_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="respcare",
                     qtype="3")

# RESPFNSP	To provide FINANCIAL SUPPORT of elderly parents who are in need?	GE005R6	GE005R7	GE005R8
df <- lsg %>% select(GE005R6, GE005R7, GE005R8) %>%
  mutate(y1=GE005R6, y2=GE005R7, y3=GE005R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(4, 5), 2, ifelse(y1 %in% c(1,2,3), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(4, 5), 2, ifelse(y2 %in% c(1,2,3), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(4, 5), 2, ifelse(y3 %in% c(1,2,3), 4, NA_real_))) %>%
  select(y1, y2, y3)
respfnsp_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="respfnsp",
                     qtype="3")

# RESPHSNG	To provide HOUSING for elderly parents who are in need?	GE006R6	GE006R7	GE006R8
df <- lsg %>% select(GE006R6, GE006R7, GE006R8) %>%
  mutate(y1=GE006R6, y2=GE006R7, y3=GE006R8) %>% 
  zap_labels() %>%
  mutate(y1 = ifelse(y1 %in% c(4, 5), 2, ifelse(y1 %in% c(1,2,3), 4, NA_real_)),
         y2 =  ifelse(y2 %in% c(4, 5), 2, ifelse(y2 %in% c(1,2,3), 4, NA_real_)),
         y3 =  ifelse(y3 %in% c(4, 5), 2, ifelse(y3 %in% c(1,2,3), 4, NA_real_))) %>%
  select(y1, y2, y3)
resphsng_model <- fmm(waves= c("y1", "y2", "y3"),
                     data=df, cov_estimator="binom", iterations=1000,
                     n_chains=5, burn=500, var_name="resphsng",
                     qtype="3")

lsg_results <- list(betterreg_model, childrel_model, relimptlsg_model, godreal_model,
                    adameve_model, dutywork_model, welflazy_model, anschal_model,
                    morepatrt_model, fmlyfrst_model, frpreschlsg_model, flexrules_model,
                    mareql_model, marfmly_model, feauth_model, abort_model, fewrkfrce_model,
                    feobey_model, felibrt_model, fecareer_model, sharhswrk_model,
                    discfam_model, algnfam_model, famweight_model, nostep_model,
                    leavemoney_model, sharact_model, liveclose_model, careeldr_model,
                    talkback_model, chldwrng_model, chldpref_model, nukepeace_model,
                    imptlno_model, #34
                    valadv_model, valequal_model, valachieve_model, valfin_model,
                    valresp_model, valrelpat_model, valserv_model, valfriend_model,
                    valfam_model, valappear_model, valpeace_model, valloyal_model,
                    valeth_model, valstuff_model, valptrt_model, valfree_model, 
                    valskill_model, valcareer_model, rescomp_model, respchor_model, #53, 54
                    resplstn_model, respcare_model, respfnsp_model, resphsng_model)

save(lsg_results, file = "~/Dropbox/hill_kreisi/results/lsgresults.Rdata")

rm(betterreg_model, childrel_model, relimptlsg_model, godreal_model,
   adameve_model, dutywork_model, welflazy_model, anschal_model,
   morepatrt_model, fmlyfrst_model, frpreschlsg_model, flexrules_model,
   mareql_model, marfmly_model, feauth_model, abort_model, fewrkfrce_model,
   feobey_model, felibrt_model, fecareer_model, sharhswrk_model,
   discfam_model, algnfam_model, famweight_model, nostep_model,
   leavemoney_model, sharact_model, liveclose_model, careeldr_model,
   talkback_model, chldwrng_model, chldpref_model, nukepeace_model,
   imptlno_model,
   valadv_model, valequal_model, valachieve_model, valfin_model,
   valresp_model, valrelpat_model, valserv_model, valfriend_model,
   valfam_model, valappear_model, valpeace_model, valloyal_model,
   valeth_model, valstuff_model, valptrt_model, valfree_model, 
   valskill_model, valcareer_model, rescomp_model, respchor_model,
   resplstn_model, respcare_model, respfnsp_model, resphsng_model, 
   lsg)

lsgres <- vector(mode = "list", length = length(lsg_results))
for (i in 1:length(lsg_results)) {
  var <- lsg_results[[i]]$model_info$var
  qtype <- lsg_results[[i]]$model_info$qtype
  lsgres[[i]] <- lsg_results[[i]]$pattern_param_summary %>%
    mutate(var = var, qtype = qtype)
}

bind_rows(lsgres) %>%
  filter(param %in% c("pi1", "alpha1")) %>%
  select(param, mean, var, qtype) %>%
  spread(param, mean) %>%
  ggplot(aes(x = pi1, y = alpha1, fill = qtype, label = var)) +
  geom_point(shape = 21) + 
  geom_text_repel() + 
  geom_vline(xintercept = .6, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2) +
  geom_hline(yintercept = .25, linetype = 2) +
  theme_bw() + 
  labs(x = "Proportion of people with stable views",
       y = "Agreement among stable view holders") 

