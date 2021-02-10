
#All waves available at: https://www.thearda.com/Archive/NSYR.asp

#local load:
n1 <- read_dta("~/Dropbox/data/nsyr/nsyr1.DTA")
n2 <- read_dta("~/Dropbox/data/nsyr/nsyr2.DTA")
n3 <- read_dta("~/Dropbox/data/nsyr/nsyr3.DTA")
n4 <- read_dta("~/Dropbox/data/nsyr/nsyr4.dta")

#source functions
source("~/ambivalence_everywhere/functions/model_function.R")

# control
df <- full_join(n2 %>% mutate(y1 = control) %>% select(ids, y1),
                n3 %>% mutate(y2 = control) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = control_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
control_model <- fmm(waves= c("y1", "y2", "y3"), id="ids", 
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="control",
                   qtype="5")

# lifeidl
df <- full_join(n2 %>% mutate(y1 = lifeidl) %>% select(ids, y1),
                n3 %>% mutate(y2 = lifeidl) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = lifeideal_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
lifeidl_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="lifeidl",
                     qtype="5")

# lifeexcl
df <- full_join(n2 %>% mutate(y1 = lifeexcl) %>% select(ids, y1),
                n3 %>% mutate(y2 = lifeexcl) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = lifeexclent_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
lifeexcl_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="lifeexcl",
                     qtype="5")

# solveprb
df <- full_join(n2 %>% mutate(y1 = solveprb) %>% select(ids, y1),
                n3 %>% mutate(y2 = solveprb) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = solveprob_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
solveprb_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="solveprb",
                      qtype="5")

# getwants
df <- full_join(n2 %>% mutate(y1 = getwants) %>% select(ids, y1),
                n3 %>% mutate(y2 = getwants) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = getwants_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
getwants_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="getwants",
                      qtype="5")

# change
df <- full_join(n2 %>% mutate(y1 = change) %>% select(ids, y1),
                n3 %>% mutate(y2 = change) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = change_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
change_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="change",
                      qtype="5")

# lifesat
df <- full_join(n2 %>% mutate(y1 = lifesat) %>% select(ids, y1),
                n3 %>% mutate(y2 = lifesat) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = lifesatis_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
lifesat_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="lifesat",
                    qtype="5")

# menwrk
df <- full_join(n2 %>% mutate(y1 = menwrk) %>% select(ids, y1),
                n3 %>% mutate(y2 = menwrk) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = menwrkwmenhme_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
menwrk_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="menwrk",
                     qtype="5")

# wommar
df <- full_join(n2 %>% mutate(y1 = wommar) %>% select(ids, y1),
                n3 %>% mutate(y2 = wommar) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = womenmar_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
wommar_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="wommar",
                    qtype="5")

# manmar
df <- full_join(n2 %>% mutate(y1 = manmar) %>% select(ids, y1),
                n3 %>% mutate(y2 = manmar) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = manmar_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
manmar_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="manmar",
                    qtype="5")

# mandecid
df <- full_join(n2 %>% mutate(y1 = mandecid) %>% select(ids, y1),
                n3 %>% mutate(y2 = mandecid) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = mandecide_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
mandecid_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="mandecid",
                    qtype="5")

# wrkngmom
df <- full_join(n2 %>% mutate(y1 = wrkngmom) %>% select(ids, y1),
                n3 %>% mutate(y2 = wrkngmom) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = wrkngmom_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
wrkngmom_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="wrkngmom",
                      qtype="5")

# unmarsex
df <- full_join(n2 %>% mutate(y1 = unmarsex) %>% select(ids, y1),
                n3 %>% mutate(y2 = unmarsex) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = unmarsex_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
unmarsex_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="unmarsex",
                      qtype="5")

# moralrel
df <- full_join(n2 %>% mutate(y1 = moralrel) %>% select(ids, y1),
                n3 %>% mutate(y2 = moralrel) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = moralrel_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
moralrel_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="moralrel",
                      qtype="5")

# moralchg
df <- full_join(n2 %>% mutate(y1 = moralchg) %>% select(ids, y1),
                n3 %>% mutate(y2 = moralchg) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = moralitychnge_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
moralchg_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="moralchg",
                      qtype="5")

# brkmoral
df <- full_join(n2 %>% mutate(y1 = brkmoral) %>% select(ids, y1),
                n3 %>% mutate(y2 = brkmoral) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = brkmorality_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
brkmoral_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="brkmoral",
                      qtype="5")

# relprvte
df <- full_join(n2 %>% mutate(y1 = relprvte) %>% select(ids, y1),
                n3 %>% mutate(y2 = relprvte) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = relprvte_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
relprvte_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="relprvte",
                      qtype="5")

# helpless
df <- full_join(n2 %>% mutate(y1 = helpless) %>% select(ids, y1),
                n3 %>% mutate(y2 = helpless) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = helpless_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
helpless_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="helpless",
                      qtype="5")

# risks
df <- full_join(n2 %>% mutate(y1 = risks) %>% select(ids, y1),
                n3 %>% mutate(y2 = risks) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = risks_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "888"=NA_real_, "999"=NA_real_, 
                     "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "5"=3, "3"=4, "4"=5,
                     "888"=NA_real_, "999"=NA_real_, "666"=NA_real_))
risks_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="risks",
                      qtype="5")

# prayansr
df <- full_join(n1 %>% mutate(y1 = prayansr) %>% select(ids, y1),
                n2 %>% mutate(y2 = prayansr) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = prayansr) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "0"=4, "1"=2, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "0"=4, "1"=2, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "0"=4, "1"=2, "777"=3, "888"=NA_real_, "666"=NA_real_))
prayansr_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="prayansr",
                      qtype="3")

# divrceok
df <- full_join(n1 %>% mutate(y1 = divceok) %>% select(ids, y1),
                n2 %>% mutate(y2 = divrceok) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = divrceok) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
divrceok_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="divrceok",
                      qtype="3")

# aftrlife
df <- full_join(n1 %>% mutate(y1 = aftrlife) %>% select(ids, y1),
                n2 %>% mutate(y2 = aftrlife) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = aftrlife) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
aftrlife_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="aftrlife",
                      qtype="3")

# angels
df <- full_join(n1 %>% mutate(y1 = angels) %>% select(ids, y1),
                n2 %>% mutate(y2 = angels) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = angels) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
angels_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="angels",
                      qtype="3")

# demons
df <- full_join(n1 %>% mutate(y1 = demons) %>% select(ids, y1),
                n2 %>% mutate(y2 = demons) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = demons) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
demons_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="demons",
                    qtype="3")

# astrolgy
df <- full_join(n1 %>% mutate(y1 = astrolgy) %>% select(ids, y1),
                n2 %>% mutate(y2 = astrolgy) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = astrolgy) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
astrolgy_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="astrolgy",
                    qtype="3")

# miracles
df <- full_join(n1 %>% mutate(y1 = miracles) %>% select(ids, y1),
                n2 %>% mutate(y2 = miracles) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = miracles) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
miracles_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="miracles",
                      qtype="3")

# reincar
df <- full_join(n1 %>% mutate(y1 = reincar) %>% select(ids, y1),
                n2 %>% mutate(y2 = reincar) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = reincar) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=3, "3"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
reincar_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="reincar",
                      qtype="3")

# god
df <- full_join(n1 %>% mutate(y1 = god) %>% select(ids, y1),
                n2 %>% mutate(y2 = god) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = god) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=3, "777"=3, "888"=NA_real_, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=3, "777"=3, "888"=NA_real_, "999"=NA_real_))
god_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="god",
                     qtype="3")

# judgeday
df <- full_join(n1 %>% mutate(y1 = judgeday) %>% select(ids, y1),
                n2 %>% mutate(y2 = judgeday) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = judgeday) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "0"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "0"=4, "777"=3, "888"=NA_real_, "666"=NA_real_,
                     "999"=NA_real_),
         y3 = recode(y3, "1"=2, "0"=4, "3"=3, "777"=3, "888"=NA_real_, "999"=NA_real_))
judgeday_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                 data=df, cov_estimator="binom", iterations=2500,
                 n_chains=5, burn=500, var_name="judgeday",
                 qtype="3")

# abstain1
df <- full_join(n1 %>% mutate(y1 = ABSTAIN1) %>% select(ids, y1),
                n2 %>% mutate(y2 = ABSTAIN1) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = ABSTAIN1) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "777"=3, "888"=NA_real_, "666"=NA_real_,
                     "999"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=3, "777"=3, "888"=NA_real_, "999"=NA_real_))
abstain1_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="abstain1",
                      qtype="3")
# howdecid
df <- full_join(n1 %>% mutate(y1 = howdecid) %>% select(howdecid, ids, y1),
                n2 %>% mutate(y2 = howdecid) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = howdecid_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=4, "4"=4, "5"=3, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=4, "4"=4, "5"=3, "777"=3, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=4, "4"=4, "5"=3, "777"=3, "888"=NA_real_)) %>%
  mutate(ahead = ifelse(howdecid == 2, 1, 0),
         parent = ifelse(howdecid == 3, 1, 0),
         god = ifelse(howdecid == 4, 1, 0))
howdecid_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="howdecid",
                      qtype="3")

# "spiritua"
df <- full_join(n1 %>% mutate(y1 = spiritua) %>% select(ids, y1),
                n2 %>% mutate(y2 = spiritua) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = spirtual) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=4, "777"=3, "666"=NA_real_, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=4, "777"=3, "666"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=4, "777"=3, "666"=NA_real_, "888"=NA_real_))
spiritua_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="spiritua",
                      qtype="3")
#"body"
df <- full_join(n1 %>% mutate(y1 = body) %>% select(ids, y1),
                n2 %>% mutate(y2 = body) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = body) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "777"=3, "999"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "777"=3, "666"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "777"=3, "999"=NA_real_))
body_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="body",
                      qtype="5")
# "accepted",
df <- full_join(n1 %>% mutate(y1 = accepted) %>% select(ids, y1),
                n2 %>% mutate(y2 = accepted) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = accepted) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "4"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "4"=4, "777"=3, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "4"=4, "777"=3, "999"=NA_real_))
accepted_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="accepted",
                      qtype="3")
# "alienate"
df <- full_join(n1 %>% mutate(y1 = alienate) %>% select(ids, y1),
                n2 %>% mutate(y2 = alienate) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = alienate) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=2, "4"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=2, "4"=4, "777"=3, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=2, "4"=4, "777"=3, "999"=NA_real_))
alienate_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="alienate",
                      qtype="3")
# "sad"
df <- full_join(n1 %>% mutate(y1 = sad) %>% select(ids, y1),
                n2 %>% mutate(y2 = sad) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = sad) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=2, "4"=4, "5"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=2, "4"=4, "5"=4, "777"=3, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=2, "4"=4, "5"=4, "777"=3, "999"=NA_real_))
sad_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="sad",
                      qtype="3")
# "invisibl", 
df <- full_join(n1 %>% mutate(y1 = invisibl) %>% select(ids, y1),
                n2 %>% mutate(y2 = invisibl) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = invisibl) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=2, "4"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=2, "4"=4, "777"=3, "666"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=2, "4"=4, "777"=3, "999"=NA_real_))
invisibl_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                 data=df, cov_estimator="binom", iterations=2500,
                 n_chains=5, burn=500, var_name="invisibl",
                 qtype="3")
#congmust
df <- full_join(n1 %>% mutate(y1 = congmust) %>% select(ids, y1),
                n2 %>% mutate(y2 = congmust) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = congmust) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "777"=3, "666"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "777"=3, "999"=NA_real_, "888"=NA_real_))
congmust_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="congmust",
                      qtype="3")
#"godclose"
df <- full_join(n1 %>% mutate(y1 = godclose) %>% select(ids, y1),
                n2 %>% mutate(y2 = godclose) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = godclose) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=1, "2"=2, "3"=3, "4"=5, "5"=6, "6"=7, "777"=4, 
                     "888"=NA_real_, "999"=NA_real_),
         y2 = recode(y2, "1"=1, "2"=2, "3"=3, "4"=5, "5"=6, "6"=7, "777"=4, 
                     "666"=NA_real_, "999"=NA_real_),
         y3 = recode(y3, "1"=1, "2"=2, "3"=3, "4"=5, "5"=6, "6"=7, "777"=4, 
                     "888"=NA_real_, "999"=NA_real_))
godclose_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="godclose",
                      qtype="7")

#okaypick
df <- full_join(n1 %>% mutate(y1 = okaypick) %>% select(ids, y1),
                n2 %>% mutate(y2 = okaypick) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = okaypick) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "777"=3, "888"=NA_real_, "999"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "777"=3, "666"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
okaypick_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="okaypick",
                      qtype="3")

#viewrel
df <- full_join(n1 %>% mutate(y1 = viewrel) %>% select(ids, y1),
                n2 %>% mutate(y2 = viewrel) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = viewrel) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=4, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=4, "777"=3, "666"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=4, "777"=3, "888"=NA_real_, "999"=NA_real_))
viewrel_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="viewrel",
                      qtype="3")

#worldorig
df <- full_join(n2 %>% mutate(y1 = wrldorig) %>% select(ids, y1),
                n3 %>% mutate(y2 = wrldorig) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = wrldorigin_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=4, "3"=3, "4"=3, "666"=NA_real_, 
                     "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=4, "3"=3, "4"=3, "777"=3, "999"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=4, "3"=3, "4"=3, "777"=3, "888"=NA_real_, "999"=NA_real_))
wrldorig_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="wrldorig",
                     qtype="3")

#heaven
df <- full_join(n2 %>% mutate(y1 = heaven) %>% select(ids, y1),
                n3 %>% mutate(y2 = heaven) %>% select(ids, y2)) %>%
  full_join(n4 %>% mutate(y3 = heaven_w4) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "0"=4, "666"=NA_real_, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "0"=4, "777"=3, "999"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "0"=4, "2"=3, "777"=3, "888"=NA_real_, "999"=NA_real_))
heaven_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="heaven",
                      qtype="3")

#faith1
df <- full_join(n1 %>% mutate(y1 = FAITH1) %>% select(ids, y1),
                n2 %>% mutate(y2 = FAITH1) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = FAITH1) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=4, "4"=4, 
                     "666"=NA_real_, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=4, "4"=4, 
                     "777"=3, "666"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=4, "4"=4,
                     "777"=3, "888"=NA_real_, "999"=NA_real_))
faith1_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="faith1",
                    qtype="3")

#doubts
df <- full_join(n1 %>% mutate(y1 = DOUBTS1) %>% select(ids, y1),
                n2 %>% mutate(y2 = DOUBTS1) %>% select(ids, y2)) %>%
  full_join(n3 %>% mutate(y3 = DOUBTS1) %>% select(ids, y3)) %>%
  zap_labels() %>%
  mutate(y1 = recode(y1, "1"=2, "2"=2, "3"=2, "4"=4, 
                     "666"=NA_real_, "777"=3, "888"=NA_real_),
         y2 = recode(y2, "1"=2, "2"=2, "3"=2, "4"=4, "5"=NA_real_,
                     "777"=3, "666"=NA_real_, "888"=NA_real_),
         y3 = recode(y3, "1"=2, "2"=2, "3"=2, "4"=4, 
                     "777"=3, "888"=NA_real_, "999"=NA_real_))
doubts1_model <- fmm(waves= c("y1", "y2", "y3"), id="ids",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="doubts1",
                    qtype="3")

nsyr_results <- list(control_model, lifeidl_model, lifeexcl_model, solveprb_model, getwants_model, #5
                     change_model, lifesat_model, menwrk_model, wommar_model, manmar_model, #10
                     mandecid_model, wrkngmom_model, unmarsex_model, moralrel_model, moralchg_model, #15 
                     brkmoral_model, relprvte_model, helpless_model, risks_model, prayansr_model, #20
                     divrceok_model, aftrlife_model, angels_model, demons_model, astrolgy_model, #25
                     miracles_model, reincar_model, god_model, judgeday_model, abstain1_model, #30
                     howdecid_model, spiritua_model, body_model, accepted_model, alienate_model, #35
                     invisibl_model, sad_model, congmust_model, godclose_model, okaypick_model, #40
                     viewrel_model, wrldorig_model, heaven_model, faith1_model, doubts1_model) #45

#local save (for KK)
# save(nsyr_results, file = "~/Dropbox/hill_kreisi/results/nsyrresult.Rdata")

#clean up
rm(control_model, lifeidl_model, lifeexcl_model, solveprb_model,
   getwants_model, change_model, lifesat_model, menwrk_model, 
   wommar_model, manmar_model, mandecid_model, wrkngmom_model, 
   unmarsex_model, moralrel_model, moralchg_model, brkmoral_model,
   relprvte_model, helpless_model, risks_model, prayansr_model,
   divrceok_model, aftrlife_model, angels_model, demons_model,
   astrolgy_model, miracles_model, reincar_model, god_model,
   judgeday_model, abstain1_model, howdecid_model, spiritua_model, 
   body_model, accepted_model, alienate_model, invisibl_model, 
   sad_model, congmust_model, godclose_model, okaypick_model, 
   viewrel_model, wrldorig_model, heaven_model, faith1_model,
   doubts1_model, n1, n2, n3, n4)

