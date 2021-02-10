
#GSS panels available at: https://gss.norc.org/get-the-data/stata

#Local load (for KK)
g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")
g8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta") 
g10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")

#source function
source("~/ambivalence_everywhere/functions/model_function.R")

#TWO Choices
# - Postlife (1:yes, 2:no)
df <- bind_rows(g6 %>% select(id_1, postlife_1, postlife_2, postlife_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
          g8 %>% select(id_1, postlife_1, postlife_2, postlife_3) %>% 
            mutate(id_1 = paste(id_1, "08", sep = "-")), 
          g10 %>% select(id_1, postlife_1, postlife_2, postlife_3) %>% 
            mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(postlife_1), w2 = as_factor(postlife_2),
         w3 = as_factor(postlife_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
postlife_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1", 
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="postlife",
                     qtype="3")

# - relexper 
df <- bind_rows(g6 %>% select(id_1, relexper_1, relexper_2, relexper_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, relexper_1, relexper_2, relexper_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, relexper_1, relexper_2, relexper_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(relexper_1), w2 = as_factor(relexper_2),
         w3 = as_factor(relexper_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "1"=2, "2"=4, "DK"=3),
         y3 = recode(w3, "1"=2, "2"=4, "DK"=3))
relexper_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="relexper",
                      qtype="3")

# - relexp  (1:yes; 2:no) #same problem with recoding
df <- bind_rows(g6 %>% select(id_1, relexp_1, relexp_2, relexp_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, relexp_1, relexp_2, relexp_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, relexp_1, relexp_2, relexp_3)  %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(relexp_1), w2 = as_factor(relexp_2),
         w3 = as_factor(relexp_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "1"=2, "2"=4, "DK"=3),
         y3 = recode(w3, "1"=2, "2"=4, "DK"=3))
relexp_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="relexp",
                      qtype="3")

# - reborn 
df <- bind_rows(g6 %>% select(id_1, reborn_1, reborn_2, reborn_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, reborn_1, reborn_2, reborn_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, reborn_1, reborn_2, reborn_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(reborn_1), w2 = as_factor(reborn_2),
         w3 = as_factor(reborn_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
reborn_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="reborn",
                    qtype="3")

# - xmovie
# - suicide1
df <- bind_rows(g6 %>% select(id_1, suicide1_1, suicide1_2, suicide1_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, suicide1_1, suicide1_2, suicide1_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, suicide1_1, suicide1_2, suicide1_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(suicide1_1), w2 = as_factor(suicide1_2),
         w3 = as_factor(suicide1_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
suicide1_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1", id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="suicide1",
                      qtype="3")
# - suicide2
df <- bind_rows(g6 %>% select(id_1, suicide2_1, suicide2_2, suicide2_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, suicide2_1, suicide2_2, suicide2_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, suicide2_1, suicide2_2, suicide2_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(suicide2_1), w2 = as_factor(suicide2_2),
         w3 = as_factor(suicide2_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
suicide2_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="suicide2",
                      qtype="3")
# - suicide3
df <- bind_rows(g6 %>% select(id_1, suicide3_1, suicide3_2, suicide3_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, suicide3_1, suicide3_2, suicide3_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, suicide3_1, suicide3_2, suicide3_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(suicide3_1), w2 = as_factor(suicide3_2),
         w3 = as_factor(suicide3_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
suicide3_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="suicide3",
                      qtype="3")
# - suicide4
df <- bind_rows(g6 %>% select(id_1, suicide4_1, suicide4_2, suicide4_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, suicide4_1, suicide4_2, suicide4_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, suicide4_1, suicide4_2, suicide4_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(suicide4_1), w2 = as_factor(suicide4_2),
         w3 = as_factor(suicide4_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
suicide4_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="suicide4",
                      qtype="3")
# - letdie1
df <- bind_rows(g6 %>% select(id_1, letdie1_1, letdie1_2, letdie1_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, letdie1_1, letdie1_2, letdie1_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, letdie1_1, letdie1_2, letdie1_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(letdie1_1), w2 = as_factor(letdie1_2),
         w3 = as_factor(letdie1_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
letdie1_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="letdie1",
                      qtype="3")
# - polescap
df <- bind_rows(g6 %>% select(id_1, polescap_1, polescap_2, polescap_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, polescap_1, polescap_2, polescap_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, polescap_1, polescap_2, polescap_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(polescap_1), w2 = as_factor(polescap_2),
         w3 = as_factor(polescap_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
polescap_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="polescap",
                      qtype="3")
# - polhitok
df <- bind_rows(g6 %>% select(id_1, polhitok_1, polhitok_2, polhitok_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, polhitok_1, polhitok_2, polhitok_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, polhitok_1, polhitok_2, polhitok_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(polhitok_1), w2 = as_factor(polhitok_2),
         w3 = as_factor(polhitok_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
polhitok_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="polhitok",
                      qtype="3")
# - polabuse
df <- bind_rows(g6 %>% select(id_1, polabuse_1, polabuse_2, polabuse_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, polabuse_1, polabuse_2, polabuse_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, polabuse_1, polabuse_2, polabuse_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(polabuse_1), w2 = as_factor(polabuse_2),
         w3 = as_factor(polabuse_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
polabuse_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="polabuse",
                      qtype="3")
# - polmurdr
df <- bind_rows(g6 %>% select(id_1, polmurdr_1, polmurdr_2, polmurdr_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, polmurdr_1, polmurdr_2, polmurdr_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, polmurdr_1, polmurdr_2, polmurdr_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(polmurdr_1), w2 = as_factor(polmurdr_2),
         w3 = as_factor(polmurdr_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
polmurdr_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="polmurdr",
                      qtype="3")
# - polattak
df <- bind_rows(g6 %>% select(id_1, polattak_1, polattak_2, polattak_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, polattak_1, polattak_2, polattak_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, polattak_1, polattak_2, polattak_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(w1 = as_factor(polattak_1), w2 = as_factor(polattak_2),
         w3 = as_factor(polattak_3)) %>%
  mutate(y1 = recode(w1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(w2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(w3, "yes"=2, "no"=4, "DK"=3))
polattak_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="polattak",
                      qtype="3")
# - gunlaw
df <- bind_rows(g6 %>% select(id_1, gunlaw_1, gunlaw_2, gunlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, gunlaw_1, gunlaw_2, gunlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, gunlaw_1, gunlaw_2, gunlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(gunlaw_1), y2 = as_factor(gunlaw_2),
         y3 = as_factor(gunlaw_3)) %>%
  mutate(y1 = recode(y1, "favor"=2, "oppose"=4, "DK"=3),
         y2 = recode(y2, "favor"=2, "oppose"=4, "DK"=3),
         y3 = recode(y3, "favor"=2, "oppose"=4, "DK"=3))
gunlaw_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="gunlaw",
                      qtype="3")
# - fear
df <- bind_rows(g6 %>% select(id_1, fear_1, fear_2, fear_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fear_1, fear_2, fear_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fear_1, fear_2, fear_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fear_1), y2 = as_factor(fear_2),
         y3 = as_factor(fear_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
fear_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="fear",
                    qtype="3")
# - cappun
df <- bind_rows(g6 %>% select(id_1, cappun_1, cappun_2, cappun_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, cappun_1, cappun_2, cappun_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, cappun_1, cappun_2, cappun_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(cappun_1), y2 = as_factor(cappun_2),
         y3 = as_factor(cappun_3)) %>%
  mutate(y1 = recode(y1, "favor"=2, "oppose"=4, "DK"=3),
         y2 = recode(y2, "favor"=2, "oppose"=4, "DK"=3),
         y3 = recode(y3, "favor"=2, "oppose"=4, "DK"=3))
cappun_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="cappun",
                  qtype="3")
# - grass
df <- bind_rows(g6 %>% select(id_1, grass_1, grass_2, grass_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, grass_1, grass_2, grass_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, grass_1, grass_2, grass_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(grass_1), y2 = as_factor(grass_2),
         y3 = as_factor(grass_3)) %>%
  mutate(y1 = recode(y1, "legal"=2, "NOT LEGAL"=4, "DK"=3),
         y2 = recode(y2, "legal"=2, "NOT LEGAL"=4, "DK"=3),
         y3 = recode(y3, "legal"=2, "NOT LEGAL"=4, "DK"=3))
grass_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="grass",
                    qtype="3")
# - prayer
df <- bind_rows(g6 %>% select(id_1, prayer_1, prayer_2, prayer_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, prayer_1, prayer_2, prayer_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, prayer_1, prayer_2, prayer_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(prayer_1), y2 = as_factor(prayer_2),
         y3 = as_factor(prayer_3)) %>%
  mutate(y1 = recode(y1, "approve"=2, "disapprove"=4, "DK"=3),
         y2 = recode(y2, "approve"=2, "disapprove"=4, "DK"=3),
         y3 = recode(y3, "approve"=2, "disapprove"=4, "DK"=3))
prayer_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="prayer",
                   qtype="3")
# - spkath
df <- bind_rows(g6 %>% select(id_1, spkath_1, spkath_2, spkath_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, spkath_1, spkath_2, spkath_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, spkath_1, spkath_2, spkath_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(spkath_1), y2 = as_factor(spkath_2),
         y3 = as_factor(spkath_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
spkath_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="spkath",
                    qtype="3")
# - spkrac
df <- bind_rows(g6 %>% select(id_1, spkrac_1, spkrac_2, spkrac_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, spkrac_1, spkrac_2, spkrac_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, spkrac_1, spkrac_2, spkrac_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(spkrac_1), y2 = as_factor(spkrac_2),
         y3 = as_factor(spkrac_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
spkrac_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="spkrac",
                    qtype="3")
# - spkcom
df <- bind_rows(g6 %>% select(id_1, spkcom_1, spkcom_2, spkcom_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, spkcom_1, spkcom_2, spkcom_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, spkcom_1, spkcom_2, spkcom_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(spkcom_1), y2 = as_factor(spkcom_2),
         y3 = as_factor(spkcom_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
spkcom_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="spkcom",
                    qtype="3")
# - spkmil
df <- bind_rows(g6 %>% select(id_1, spkmil_1, spkmil_2, spkmil_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, spkmil_1, spkmil_2, spkmil_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, spkmil_1, spkmil_2, spkmil_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(spkmil_1), y2 = as_factor(spkmil_2),
         y3 = as_factor(spkmil_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
spkmil_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="spkmil",
                    qtype="3")
# - spkhomo
df <- bind_rows(g6 %>% select(id_1, spkhomo_1, spkhomo_2, spkhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, spkhomo_1, spkhomo_2, spkhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, spkhomo_1, spkhomo_2, spkhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(spkhomo_1), y2 = as_factor(spkhomo_2),
         y3 = as_factor(spkhomo_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
spkhomo_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="spkhomo",
                    qtype="3")
# - spkmslm
df <- bind_rows(g8 %>% select(id_1, SPKMSLM_1, SPKMSLM_2, SPKMSLM_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, SPKMSLM_1, SPKMSLM_2, SPKMSLM_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(SPKMSLM_1), y2 = as_factor(SPKMSLM_2),
         y3 = as_factor(SPKMSLM_3)) %>%
  mutate(y1 = recode(y1, "Yes, allowed"=2, "Not allowed"=4, "DONT KNOW"=3),
         y2 = recode(y2, "Yes, allowed"=2, "Not allowed"=4, "DONT KNOW"=3),
         y3 = recode(y3, "Yes, allowed"=2, "Not allowed"=4, "DONT KNOW"=3))
spkmslm_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="spkmslm",
                    qtype="3")
# - colath (4: allowed; 5: not allowed)
df <- bind_rows(g6 %>% select(id_1, colath_1, colath_2, colath_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, colath_1, colath_2, colath_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, colath_1, colath_2, colath_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(colath_1), y2 = as_factor(colath_2),
         y3 = as_factor(colath_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
colath_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="colath",
                     qtype="3")

# - colrac
df <- bind_rows(g6 %>% select(id_1, colrac_1, colrac_2, colrac_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, colrac_1, colrac_2, colrac_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, colrac_1, colrac_2, colrac_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(colrac_1), y2 = as_factor(colrac_2),
         y3 = as_factor(colrac_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
colrac_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="colrac",
                    qtype="3")
# - colcom (be fired?) (4:fired; 5:not fired)
df <- bind_rows(g6 %>% select(id_1, colcom_1, colcom_2, colcom_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, colcom_1, colcom_2, colcom_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, colcom_1, colcom_2, colcom_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(colcom_1), y2 = as_factor(colcom_2),
         y3 = as_factor(colcom_3)) %>%
  mutate(y1 = recode(y1, "fired"=2, "NOT FIRED"=4, "DK"=3),
         y2 = recode(y2, "fired"=2, "NOT FIRED"=4, "DK"=3),
         y3 = recode(y3, "fired"=2, "NOT FIRED"=4, "DK"=3))
colcom_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="colcom",
                    qtype="3")
# - colmil
df <- bind_rows(g6 %>% select(id_1, colmil_1, colmil_2, colmil_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, colmil_1, colmil_2, colmil_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, colmil_1, colmil_2, colmil_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(colmil_1), y2 = as_factor(colmil_2),
         y3 = as_factor(colmil_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
colmil_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="colmil",
                    qtype="3")

# - colhomo
df <- bind_rows(g6 %>% select(id_1, colhomo_1, colhomo_2, colhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, colhomo_1, colhomo_2, colhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, colhomo_1, colhomo_2, colhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(colhomo_1), y2 = as_factor(colhomo_2),
         y3 = as_factor(colhomo_3)) %>%
  mutate(y1 = recode(y1, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y2 = recode(y2, "allowed"=2, "NOT ALLOWED"=4, "DK"=3),
         y3 = recode(y3, "allowed"=2, "NOT ALLOWED"=4, "DK"=3))
colhomo_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="colhomo",
                    qtype="3")

# - colmslm
df <- bind_rows(g8 %>% select(id_1, COLMSLM_1, COLMSLM_2, COLMSLM_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, COLMSLM_1, COLMSLM_2, COLMSLM_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(COLMSLM_1), y2 = as_factor(COLMSLM_2),
         y3 = as_factor(COLMSLM_3)) %>%
  mutate(y1 = recode(y1, "Yes, allowed"=2, "Not allowed"=4, "DONT KNOW"=3),
         y2 = recode(y2, "Yes, allowed"=2, "Not allowed"=4, "DONT KNOW"=3),
         y3 = recode(y3, "Yes, allowed"=2, "Not allowed"=4, "DONT KNOW"=3))
colmslm_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="colmslm",
                    qtype="3")

# - libath (1: remove; 2:not remove)
df <- bind_rows(g6 %>% select(id_1, libath_1, libath_2, libath_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, libath_1, libath_2, libath_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, libath_1, libath_2, libath_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(libath_1), y2 = as_factor(libath_2),
         y3 = as_factor(libath_3)) %>%
  mutate(y1 = recode(y1, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y2 = recode(y2, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y3 = recode(y3, "remove"=2, "NOT REMOVE"=4, "DK"=3))
libath_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="libath",
                     qtype="3")

# - librac
df <- bind_rows(g6 %>% select(id_1, librac_1, librac_2, librac_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, librac_1, librac_2, librac_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, librac_1, librac_2, librac_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(librac_1), y2 = as_factor(librac_2),
         y3 = as_factor(librac_3)) %>%
  mutate(y1 = recode(y1, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y2 = recode(y2, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y3 = recode(y3, "remove"=2, "NOT REMOVE"=4, "DK"=3))
librac_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="librac",
                    qtype="3")

# - libcom
df <- bind_rows(g6 %>% select(id_1, libcom_1, libcom_2, libcom_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, libcom_1, libcom_2, libcom_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, libcom_1, libcom_2, libcom_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(libcom_1), y2 = as_factor(libcom_2),
         y3 = as_factor(libcom_3)) %>%
  mutate(y1 = recode(y1, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y2 = recode(y2, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y3 = recode(y3, "remove"=2, "NOT REMOVE"=4, "DK"=3))
libcom_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="libcom",
                    qtype="3")

# - libmil
df <- bind_rows(g6 %>% select(id_1, libmil_1, libmil_2, libmil_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, libmil_1, libmil_2, libmil_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, libmil_1, libmil_2, libmil_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(libmil_1), y2 = as_factor(libmil_2),
         y3 = as_factor(libmil_3)) %>%
  mutate(y1 = recode(y1, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y2 = recode(y2, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y3 = recode(y3, "remove"=2, "NOT REMOVE"=4, "DK"=3))
libmil_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="libmil",
                    qtype="3")

# - libhomo
df <- bind_rows(g6 %>% select(id_1, libhomo_1, libhomo_2, libhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, libhomo_1, libhomo_2, libhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, libhomo_1, libhomo_2, libhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(libhomo_1), y2 = as_factor(libhomo_2),
         y3 = as_factor(libhomo_3)) %>%
  mutate(y1 = recode(y1, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y2 = recode(y2, "remove"=2, "NOT REMOVE"=4, "DK"=3),
         y3 = recode(y3, "remove"=2, "NOT REMOVE"=4, "DK"=3))
libhomo_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="libhomo",
                    qtype="3")

# - libmslm
df <- bind_rows(g8 %>% select(id_1, LIBMSLM_1, LIBMSLM_2, LIBMSLM_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, LIBMSLM_1, LIBMSLM_2, LIBMSLM_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(LIBMSLM_1), y2 = as_factor(LIBMSLM_2),
         y3 = as_factor(LIBMSLM_3)) %>%
  mutate(y1 = recode(y1, "Remove"=2, "Not remove"=4, "DK"=3),
         y2 = recode(y2, "Remove"=2, "Not remove"=4, "DK"=3),
         y3 = recode(y3, "Remove"=2, "Not remove"=4, "DK"=3))
libmslm_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="libmslm",
                    qtype="3")

# - richwork (1: continue working; 2: stop working)
df <- bind_rows(g6 %>% select(id_1, richwork_1, richwork_2, richwork_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, richwork_1, richwork_2, richwork_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, richwork_1, richwork_2, richwork_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(richwork_1), y2 = as_factor(richwork_2),
         y3 = as_factor(richwork_3)) %>%
  mutate(y1 = recode(y1, "CONTINUE WORKING"=2, "STOP WORKING"=4, "DK"=3),
         y2 = recode(y2, "CONTINUE WORKING"=2, "STOP WORKING"=4, "DK"=3),
         y3 = recode(y3, "CONTINUE WORKING"=2, "STOP WORKING"=4, "DK"=3))
richwork_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="richwork",
                    qtype="3")

# - fepol-men better (1:agree; 2:disagree; 8:not sure)
df <- bind_rows(g6 %>% select(id_1, fepol_1, fepol_2, fepol_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fepol_1, fepol_2, fepol_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fepol_1, fepol_2, fepol_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fepol_1), y2 = as_factor(fepol_2),
         y3 = as_factor(fepol_3)) %>%
  mutate(y1 = recode(y1, "agree"=2, "disagree"=4, "DK"=3, "NOT SURE"=3),
         y2 = recode(y2, "agree"=2, "disagree"=4, "DK"=3, "NOT SURE"=3),
         y3 = recode(y3, "agree"=2, "disagree"=4, "DK"=3, "NOT SURE"=3))
fepol_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="fepol",
                      qtype="3")

# - abany
df <- bind_rows(g6 %>% select(id_1, abany_1, abany_2, abany_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, abany_1, abany_2, abany_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, abany_1, abany_2, abany_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(abany_1), y2 = as_factor(abany_2),
         y3 = as_factor(abany_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
abany_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="abany",
                   qtype="3")
# - abdefect
df <- bind_rows(g6 %>% select(id_1, abdefect_1, abdefect_2, abdefect_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, abdefect_1, abdefect_2, abdefect_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, abdefect_1, abdefect_2, abdefect_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(abdefect_1), y2 = as_factor(abdefect_2),
         y3 = as_factor(abdefect_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
abdefect_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="abdefect",
                   qtype="3")
# - abhlth
df <- bind_rows(g6 %>% select(id_1, abhlth_1, abhlth_2, abhlth_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, abhlth_1, abhlth_2, abhlth_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, abhlth_1, abhlth_2, abhlth_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(abhlth_1), y2 = as_factor(abhlth_2),
         y3 = as_factor(abhlth_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
abhlth_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="abhlth",
                   qtype="3")
# - abnomore
df <- bind_rows(g6 %>% select(id_1, abnomore_1, abnomore_2, abnomore_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, abnomore_1, abnomore_2, abnomore_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, abnomore_1, abnomore_2, abnomore_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(abnomore_1), y2 = as_factor(abnomore_2),
         y3 = as_factor(abnomore_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
abnomore_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="abnomore",
                   qtype="3")
# - abpoor
df <- bind_rows(g6 %>% select(id_1, abpoor_1, abpoor_2, abpoor_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, abpoor_1, abpoor_2, abpoor_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, abpoor_1, abpoor_2, abpoor_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(abpoor_1), y2 = as_factor(abpoor_2),
         y3 = as_factor(abpoor_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
abpoor_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="abpoor",
                   qtype="3")
# - abrape
df <- bind_rows(g6 %>% select(id_1, abrape_1, abrape_2, abrape_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, abrape_1, abrape_2, abrape_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, abrape_1, abrape_2, abrape_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(abrape_1), y2 = as_factor(abrape_2),
         y3 = as_factor(abrape_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
abrape_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="abrape",
                   qtype="3")
# - absingle
df <- bind_rows(g6 %>% select(id_1, absingle_1, absingle_2, absingle_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, absingle_1, absingle_2, absingle_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, absingle_1, absingle_2, absingle_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(absingle_1), y2 = as_factor(absingle_2),
         y3 = as_factor(absingle_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
absingle_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="absingle",
                   qtype="3")
# - racdif1
df <- bind_rows(g6 %>% select(id_1, racdif1_1, racdif1_2, racdif1_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, racdif1_1, racdif1_2, racdif1_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, racdif1_1, racdif1_2, racdif1_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(racdif1_1), y2 = as_factor(racdif1_2),
         y3 = as_factor(racdif1_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
racdif1_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="racdif1",
                   qtype="3")
# - racdif2
df <- bind_rows(g6 %>% select(id_1, racdif2_1, racdif2_2, racdif2_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, racdif2_1, racdif2_2, racdif2_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, racdif2_1, racdif2_2, racdif2_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(racdif2_1), y2 = as_factor(racdif2_2),
         y3 = as_factor(racdif2_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
racdif2_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="racdif2",
                   qtype="3")
# - racdif3
df <- bind_rows(g6 %>% select(id_1, racdif3_1, racdif3_2, racdif3_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, racdif3_1, racdif3_2, racdif3_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, racdif3_1, racdif3_2, racdif3_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(racdif3_1), y2 = as_factor(racdif3_2),
         y3 = as_factor(racdif3_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
racdif3_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="racdif3",
                   qtype="3")
# - racdif4
df <- bind_rows(g6 %>% select(id_1, racdif4_1, racdif4_2, racdif4_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, racdif4_1, racdif4_2, racdif4_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, racdif4_1, racdif4_2, racdif4_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(racdif4_1), y2 = as_factor(racdif4_2),
         y3 = as_factor(racdif4_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"= 2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
racdif4_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="racdif4",
                   qtype="3")
# - raclive (any opposite race)


# Three Options
# - fair (1: take advantage; 2: fair; 3:depends)
df <- bind_rows(g6 %>% select(id_1, fair_1, fair_2, fair_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fair_1, fair_2, fair_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fair_1, fair_2, fair_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fair_1), y2 = as_factor(fair_2),
         y3 = as_factor(fair_3)) %>%
  mutate(y1 = recode(y1, "fair"=2, "depends"=3, "TAKE ADVANTAGE"=4, "DK"=3),
         y2 = recode(y2, "fair"=2, "depends"=3, "TAKE ADVANTAGE"=4, "DK"=3),
         y3 = recode(y3, "fair"=2, "depends"=3, "TAKE ADVANTAGE"=4, "DK"=3))
fair_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="fair",
                     qtype="3")

# - trust (1:can trust; 2: cant be too careful; 3:depends)
df <- bind_rows(g6 %>% select(id_1, trust_1, trust_2, trust_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, trust_1, trust_2, trust_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, trust_1, trust_2, trust_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(trust_1), y2 = as_factor(trust_2),
         y3 = as_factor(trust_3)) %>%
  mutate(y1 = recode(y1, "CAN TRUST"=2, "depends"=3, "CANNOT TRUST"=4, "DK"=3),
         y2 = recode(y2, "CAN TRUST"=2, "depends"=3, "CANNOT TRUST"=4, "DK"=3),
         y3 = recode(y3, "CAN TRUST"=2, "depends"=3, "CANNOT TRUST"=4, "DK"=3))
trust_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="trust",
                  qtype="3")
# - helpful (1:helpful; 2:look out self; 3:depends)
df <- bind_rows(g6 %>% select(id_1, helpful_1, helpful_2, helpful_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, helpful_1, helpful_2, helpful_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, helpful_1, helpful_2, helpful_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(helpful_1), y2 = as_factor(helpful_2),
         y3 = as_factor(helpful_3)) %>%
  mutate(y1 = recode(y1, "helpful"=2, "depends"=3, "LOOKOUT FOR SELF"=4, "DK"=3),
         y2 = recode(y2, "helpful"=2, "depends"=3, "LOOKOUT FOR SELF"=4, "DK"=3),
         y3 = recode(y3, "helpful"=2, "depends"=3, "LOOKOUT FOR SELF"=4, "DK"=3))
helpful_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="helpful",
                   qtype="3")
# - aged (1:good idea; 2:bad idea; 3:depends)
df <- bind_rows(g6 %>% select(id_1, aged_1, aged_2, aged_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, aged_1, aged_2, aged_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, aged_1, aged_2, aged_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(aged_1), y2 = as_factor(aged_2),
         y3 = as_factor(aged_3)) %>%
  mutate(y1 = recode(y1, "A GOOD IDEA"=2, "depends"=3, "A BAD IDEA"=4, "DK"=3),
         y2 = recode(y2, "A GOOD IDEA"=2, "depends"=3, "A BAD IDEA"=4, "DK"=3),
         y3 = recode(y3, "A GOOD IDEA"=2, "depends"=3, "A BAD IDEA"=4, "DK"=3))
aged_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="aged",
                     qtype="3")
# - racopen (1:owner decides; 2:cant discrimingate; 3:neither)
df <- bind_rows(g6 %>% select(id_1, racopen_1, racopen_2, racopen_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, racopen_1, racopen_2, racopen_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, racopen_1, racopen_2, racopen_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(racopen_1), y2 = as_factor(racopen_2),
         y3 = as_factor(racopen_3)) %>%
  mutate(y1 = recode(y1, "CANT DISCRIMINATE"=2, "neither"=3, "OWNER DECIDES"=4, "DK"=3),
         y2 = recode(y2, "CANT DISCRIMINATE"=2, "neither"=3, "OWNER DECIDES"=4, "DK"=3),
         y3 = recode(y3, "CANT DISCRIMINATE"=2, "neither"=3, "OWNER DECIDES"=4, "DK"=3))
racopen_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="racopen",
                  qtype="3")

# Four Options:
#   - fepresch-prek suffer (1:strong agree; 2 agree; 3:disagree; 4:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, fepresch_1, fepresch_2, fepresch_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fepresch_1, fepresch_2, fepresch_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fepresch_1, fepresch_2, fepresch_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fepresch_1), y2 = as_factor(fepresch_2),
         y3 = as_factor(fepresch_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3))
fepresch_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1", id = "id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="fepresch",
                     qtype="5")
# - fechld-working mom warm (1:strong agree; 2 agree; 3:disagree; 4:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, fechld_1, fechld_2, fechld_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fechld_1, fechld_2, fechld_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fechld_1, fechld_2, fechld_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fechld_1), y2 = as_factor(fechld_2),
         y3 = as_factor(fechld_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3))
fechld_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1", id = "id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="fechld",
                      qtype="5")
# - spanking-favor spanking (1:strong agree; 2 agree; 3:disagree; 4:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, spanking_1, spanking_2, spanking_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, spanking_1, spanking_2, spanking_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, spanking_1, spanking_2, spanking_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(spanking_1), y2 = as_factor(spanking_2),
         y3 = as_factor(spanking_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3))
spanking_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="spanking",
                    qtype="5")
# - fejobaff-preferences hiring women (1:strong for; 2:for; 3:against; 4:strong agains)
df <- bind_rows(g6 %>% select(id_1, fejobaff_1, fejobaff_2, fejobaff_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fejobaff_1, fejobaff_2, fejobaff_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fejobaff_1, fejobaff_2, fejobaff_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fejobaff_1), y2 = as_factor(fejobaff_2),
         y3 = as_factor(fejobaff_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY FOR"=1, "for"=2, "against"=4, "STRONGLY AGAINST"=5, "DK"=3),
         y2 = recode(y2, "STRONGLY FOR"=1, "for"=2, "against"=4, "STRONGLY AGAINST"=5, "DK"=3),
         y3 = recode(y3, "STRONGLY FOR"=1, "for"=2, "against"=4, "STRONGLY AGAINST"=5, "DK"=3))
fejobaff_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="fejobaff",
                    qtype="5")

# - fefam-better man work (1:strong agree; 2 agree; 3:disagree; 4:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, fefam_1, fefam_2, fefam_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fefam_1, fefam_2, fefam_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fefam_1, fefam_2, fefam_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fefam_1), y2 = as_factor(fefam_2),
         y3 = as_factor(fefam_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3))
fefam_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="fefam",
                    qtype="5")
# - discaffw - woman lose job to less qualified man (1:very likely; 2 somwhat likely; 3 somewhat unlikely; 4: very unlikely)
df <- bind_rows(g6 %>% select(id_1, discaffw_1, discaffw_2, discaffw_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, discaffw_1, discaffw_2, discaffw_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, discaffw_1, discaffw_2, discaffw_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(discaffw_1), y2 = as_factor(discaffw_2),
         y3 = as_factor(discaffw_3)) %>%
  mutate(y1 = recode(y1, "VERY LIKELY"=1, "SOMEWHAT LIKELY"=2, "SOMEWHAT UNLIKELY"=4, "VERY UNLIKELY"=5, "DONT KNOW"=3),
         y2 = recode(y2, "VERY LIKELY"=1, "SOMEWHAT LIKELY"=2, "SOMEWHAT UNLIKELY"=4, "VERY UNLIKELY"=5, "DONT KNOW"=3),
         y3 = recode(y3, "VERY LIKELY"=1, "SOMEWHAT LIKELY"=2, "SOMEWHAT UNLIKELY"=4, "VERY UNLIKELY"=5, "DONT KNOW"=3))
discaffw_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="discaffw",
                      qtype="5")

# - discaffm - man lose job to less qualified woman (1:very likely; 2 somwhat likely; 3 somewhat unlikely; 4: very unlikely)
df <- bind_rows(g6 %>% select(id_1, discaffm_1, discaffm_2, discaffm_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, discaffm_1, discaffm_2, discaffm_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, discaffm_1, discaffm_2, discaffm_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(discaffm_1), y2 = as_factor(discaffm_2),
         y3 = as_factor(discaffm_3)) %>%
  mutate(y1 = recode(y1, "VERY LIKELY"=1, "SOMEWHAT LIKELY"=2, "SOMEWHAT UNLIKELY"=4, "VERY UNLIKELY"=5, "DONT KNOW"=3),
         y2 = recode(y2, "VERY LIKELY"=1, "SOMEWHAT LIKELY"=2, "SOMEWHAT UNLIKELY"=4, "VERY UNLIKELY"=5, "DONT KNOW"=3),
         y3 = recode(y3, "VERY LIKELY"=1, "SOMEWHAT LIKELY"=2, "SOMEWHAT UNLIKELY"=4, "VERY UNLIKELY"=5, "DONT KNOW"=3))
discaffm_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="discaffm",
                      qtype="5")
# - discaff - white lose job to less qualified black (1:very likely; 2 somwhat likely; 3 somewhat unlikely; 4: very unlikely)
# ^ Not coded the same ^
# - pillok - (1:strong agree; 2 agree; 3:disagree; 4:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, pillok_1, pillok_2, pillok_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, pillok_1, pillok_2, pillok_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, pillok_1, pillok_2, pillok_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(pillok_1), y2 = as_factor(pillok_2),
         y3 = as_factor(pillok_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "disagree"=4, "STRONGLY DISAGREE"=5, "DK"=3))
pillok_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="pillok",
                   qtype="5")
# - affrmact - (1:strong support; 2 support; 3:oppose pref; 4:strongly oppose)
df <- bind_rows(g6 %>% select(id_1, affrmact_1, affrmact_2, affrmact_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, affrmact_1, affrmact_2, affrmact_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, affrmact_1, affrmact_2, affrmact_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(affrmact_1), y2 = as_factor(affrmact_2),
         y3 = as_factor(affrmact_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY SUPPORT PREF"=1, "SUPPORT PREF"=2, "OPPOSE PREF"=4, "STRONGLY OPPOSE PREF"=5, "DK"=3),
         y2 = recode(y2, "STRONGLY SUPPORT PREF"=1, "SUPPORT PREF"=2, "OPPOSE PREF"=4, "STRONGLY OPPOSE PREF"=5, "DK"=3),
         y3 = recode(y3, "STRONGLY SUPPORT PREF"=1, "SUPPORT PREF"=2, "OPPOSE PREF"=4, "STRONGLY OPPOSE PREF"=5, "DK"=3))
affrmact_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="affrmact",
                    qtype="5")

# Five options ("unsure" midpoint):
#   - popespks
df <- bind_rows(g6 %>% select(id_1, popespks_1, popespks_2, popespks_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, popespks_1, popespks_2, popespks_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, popespks_1, popespks_2, popespks_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(popespks_1), y2 = as_factor(popespks_2),
         y3 = as_factor(popespks_3)) %>%
  mutate(y1 = recode(y1, "CERTAINLY TRUE"=1, "PROBABLY TRUE"=2, "UNCERTAIN TRUE OR FALSE"=3,
                     "PROBABLY FALSE"=4, "CERTAINLY FALSE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "CERTAINLY TRUE"=1, "PROBABLY TRUE"=2, "UNCERTAIN TRUE OR FALSE"=3,
                     "PROBABLY FALSE"=4, "CERTAINLY FALSE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "CERTAINLY TRUE"=1, "PROBABLY TRUE"=2, "UNCERTAIN TRUE OR FALSE"=3,
                     "PROBABLY FALSE"=4, "CERTAINLY FALSE"=5, "DONT KNOW"=3))
popespks_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="popespks",
                      qtype="5")

# - meovrwrk-family suffers men work too hard (1:strong agree; 2 agree; 3:neither; 4:disagree; 5:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, meovrwrk_1, meovrwrk_2, meovrwrk_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, meovrwrk_1, meovrwrk_2, meovrwrk_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, meovrwrk_1, meovrwrk_2, meovrwrk_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(meovrwrk_1), y2 = as_factor(meovrwrk_2),
         y3 = as_factor(meovrwrk_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CAN'T CHOOSE"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CAN'T CHOOSE"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CAN'T CHOOSE"=3))
meovrwrk_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="meovrwrk",
                    qtype="5")

# - fehire-hire and promote women (1:strong agree; 2 agree; 3:neither; 4:disagree; 5:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, fehire_1, fehire_2, fehire_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, fehire_1, fehire_2, fehire_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, fehire_1, fehire_2, fehire_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(fehire_1), y2 = as_factor(fehire_2),
         y3 = as_factor(fehire_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "DONT KNOW"=3))
fehire_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="fehire",
                      qtype="5")

# - marhomo (1:strong agree; 2 agree; 3:neither; 4:disagree; 5:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, marhomo_1, marhomo_2, marhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, marhomo_1, marhomo_2, marhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, marhomo_1, marhomo_2, marhomo_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(marhomo_1), y2 = as_factor(marhomo_2),
         y3 = as_factor(marhomo_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CANT CHOOSE"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CANT CHOOSE"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CANT CHOOSE"=3))
marhomo_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="marhomo",
                    qtype="5")


# - wrkwayup (1:strong agree; 2 agree; 3:neither; 4:disagree; 5:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, wrkwayup_1, wrkwayup_2, wrkwayup_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, wrkwayup_1, wrkwayup_2, wrkwayup_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, wrkwayup_1, wrkwayup_2, wrkwayup_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(wrkwayup_1), y2 = as_factor(wrkwayup_2),
         y3 = as_factor(wrkwayup_3)) %>%
  mutate(y1 = recode(y1, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y2 = recode(y2, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y3 = recode(y3, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, "NEITHER AGREE NOR DISAGREE"=3,
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3))
wrkwayup_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="wrkwayup",
                     qtype="5")
# - marwht (1:strong favor; 2 favor; 3:neither; 4:oppose; 5:strongly oppose)
df <- bind_rows(g6 %>% select(id_1, marwht_1, marwht_2, marwht_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, marwht_1, marwht_2, marwht_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, marwht_1, marwht_2, marwht_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(marwht_1), y2 = as_factor(marwht_2),
         y3 = as_factor(marwht_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3))
marwht_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="marwht",
                      qtype="5")
# - marblk (1:strong favor; 2 favor; 3:neither; 4:oppose; 5:strongly oppose)
df <- bind_rows(g6 %>% select(id_1, marblk_1, marblk_2, marblk_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, marblk_1, marblk_2, marblk_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, marblk_1, marblk_2, marblk_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(marblk_1), y2 = as_factor(marblk_2),
         y3 = as_factor(marblk_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3))
marblk_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="marblk",
                    qtype="5")

# - livewhts (1:strong favor; 2 favor; 3:neither; 4:oppose; 5:strongly oppose)
df <- bind_rows(g6 %>% select(id_1, livewhts_1, livewhts_2, livewhts_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, livewhts_1, livewhts_2, livewhts_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, livewhts_1, livewhts_2, livewhts_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(livewhts_1), y2 = as_factor(livewhts_2),
         y3 = as_factor(livewhts_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3))
livewhts_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="livewhts",
                    qtype="5")
# - liveblks (1:strong favor; 2 favor; 3:neither; 4:oppose; 5:strongly oppose)
df <- bind_rows(g6 %>% select(id_1, liveblks_1, liveblks_2, liveblks_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, liveblks_1, liveblks_2, liveblks_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, liveblks_1, liveblks_2, liveblks_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(liveblks_1), y2 = as_factor(liveblks_2),
         y3 = as_factor(liveblks_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3))
liveblks_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="liveblks",
                    qtype="5")
# - goodlife (1:strong agree; 2 agree; 3:neither; 4:disagree; 5:strongly disagree)
df <- bind_rows(g6 %>% select(id_1, goodlife_1, goodlife_2, goodlife_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, goodlife_1, goodlife_2, goodlife_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, goodlife_1, goodlife_2, goodlife_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(goodlife_1), y2 = as_factor(goodlife_2),
         y3 = as_factor(goodlife_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "neither"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CANT CHOOSE"=3),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "neither"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CANT CHOOSE"=3),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "neither"=3,
                     "disagree"=4, "STRONGLY DISAGREE"=5, "CANT CHOOSE"=3))
goodlife_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="goodlife",
                     qtype="5")

# - helppoor (1:govt action; 2: ; 3:agree both; 4: ; 5: people help selves)
df <- bind_rows(g6 %>% select(id_1, helppoor_1, helppoor_2, helppoor_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, helppoor_1, helppoor_2, helppoor_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, helppoor_1, helppoor_2, helppoor_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(helppoor_1), y2 = as_factor(helppoor_2),
         y3 = as_factor(helppoor_3)) %>%
  mutate(y1 = recode(y1, "GOVT ACTION"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "PEOPLE HELP SELVES"=5, "DK"=3),
         y2 = recode(y2, "GOVT ACTION"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "PEOPLE HELP SELVES"=5, "DK"=3),
         y3 = recode(y3, "GOVT ACTION"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "PEOPLE HELP SELVES"=5, "DK"=3))
helppoor_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="helppoor",
                      qtype="5")
# - helpnot (1:gov do more; 2: ; 3:agree both; 4: ; 5: gov do less)
df <- bind_rows(g6 %>% select(id_1, helpnot_1, helpnot_2, helpnot_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, helpnot_1, helpnot_2, helpnot_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, helpnot_1, helpnot_2, helpnot_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(helpnot_1), y2 = as_factor(helpnot_2),
         y3 = as_factor(helpnot_3)) %>%
  mutate(y1 = recode(y1, "GOVT DO MORE"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "GOVT DOES TOO MUCH"=5, "DK"=3),
         y2 = recode(y2, "GOVT DO MORE"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "GOVT DOES TOO MUCH"=5, "DK"=3),
         y3 = recode(y3, "GOVT DO MORE"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "GOVT DOES TOO MUCH"=5, "DK"=3))
helpnot_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="helpnot",
                      qtype="5")
# - helpsick (1:gov should help; 2: ; 3:agree both; 4: ; 5: people help selves)
df <- bind_rows(g6 %>% select(id_1, helpsick_1, helpsick_2, helpsick_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, helpsick_1, helpsick_2, helpsick_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, helpsick_1, helpsick_2, helpsick_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(helpsick_1), y2 = as_factor(helpsick_2),
         y3 = as_factor(helpsick_3)) %>%
  mutate(y1 = recode(y1, "GOVT SHOULD HELP"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "PEOPLE HELP SELVES"=5, "DK"=3),
         y2 = recode(y2, "GOVT SHOULD HELP"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "PEOPLE HELP SELVES"=5, "DK"=3),
         y3 = recode(y3, "GOVT SHOULD HELP"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "PEOPLE HELP SELVES"=5, "DK"=3))
helpsick_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="helpsick",
                     qtype="5")
# - helpblks (1:gov help blacks; 2: ; 3:agree both; 4: ; 5: no special treatment)
df <- bind_rows(g6 %>% select(id_1, helpblk_1, helpblk_2, helpblk_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, helpblk_1, helpblk_2, helpblk_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, helpblk_1, helpblk_2, helpblk_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(helpblk_1), y2 = as_factor(helpblk_2),
         y3 = as_factor(helpblk_3)) %>%
  mutate(y1 = recode(y1, "GOVT HELP BLKS"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "NO SPECIAL TREATMENT"=5, "DK"=3),
         y2 = recode(y2, "GOVT HELP BLKS"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "NO SPECIAL TREATMENT"=5, "DK"=3),
         y3 = recode(y3, "GOVT HELP BLKS"=1, "2"=2, "AGREE WITH BOTH"=3,
                     "4"=4, "NO SPECIAL TREATMENT"=5, "DK"=3))
helpblk_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="helpblk",
                      qtype="5")

# Seven Options
# - partyid (0:strong democrat; 2:not strong democrat; 3:ind, near dem; 4:independent; 5:ind, near rep; 6:not strong rep; 6:strong rep; 7:other)
df <- bind_rows(g6 %>% select(id_1, partyid_1, partyid_2, partyid_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, partyid_1, partyid_2, partyid_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, partyid_1, partyid_2, partyid_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(partyid_1), y2 = as_factor(partyid_2),
         y3 = as_factor(partyid_3)) %>%
  mutate(y1 = recode(y1, "STRONG DEMOCRAT"=1, "NOT STR DEMOCRAT"=2, "IND,NEAR DEM"=3, "independent"=4,
                     "IND,NEAR REP"=5, "NOT STR REPUBLICAN"=6, "STRONG REPUBLICAN"=7, "OTHER PARTY"=4, "DK"=4),
         y2 = recode(y2, "STRONG DEMOCRAT"=1, "NOT STR DEMOCRAT"=2, "IND,NEAR DEM"=3, "independent"=4,
                     "IND,NEAR REP"=5, "NOT STR REPUBLICAN"=6, "STRONG REPUBLICAN"=7, "OTHER PARTY"=4, "DK"=4),
         y3 = recode(y3, "STRONG DEMOCRAT"=1, "NOT STR DEMOCRAT"=2, "IND,NEAR DEM"=3, "independent"=4,
                     "IND,NEAR REP"=5, "NOT STR REPUBLICAN"=6, "STRONG REPUBLICAN"=7, "OTHER PARTY"=4, "DK"=4))
partyid_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="partyid",
                     qtype="7")

# - polviews (1:extremely liberal; 2:liberal; 3:slightly liberal; 4:moderate; 5:slightly conservative; 6: conservative; 7:extremely conservative; 8:DK
df <- bind_rows(g6 %>% select(id_1, polviews_1, polviews_2, polviews_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, polviews_1, polviews_2, polviews_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, polviews_1, polviews_2, polviews_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(polviews_1), y2 = as_factor(polviews_2),
         y3 = as_factor(polviews_3)) %>%
  mutate(y1 = recode(y1, "EXTREMELY LIBERAL"=1, "liberal"=2, "SLIGHTLY LIBERAL"=3, "moderate"=4,
                     "SLGHTLY CONSERVATIVE"=5, "conservative"=6, "EXTRMLY CONSERVATIVE"=7, "DK"=4),
         y2 = recode(y2, "EXTREMELY LIBERAL"=1, "liberal"=2, "SLIGHTLY LIBERAL"=3, "moderate"=4,
                     "SLGHTLY CONSERVATIVE"=5, "conservative"=6, "EXTRMLY CONSERVATIVE"=7, "DK"=4),
         y3 = recode(y3, "EXTREMELY LIBERAL"=1, "liberal"=2, "SLIGHTLY LIBERAL"=3, "moderate"=4,
                     "SLGHTLY CONSERVATIVE"=5, "conservative"=6, "EXTRMLY CONSERVATIVE"=7, "DK"=4))
polviews_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="polviews",
                     qtype="7")
# - eqwlth (1:gov reduce differences; ... 7: no gov't action)
df <- bind_rows(g6 %>% select(id_1, eqwlth_1, eqwlth_2, eqwlth_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, eqwlth_1, eqwlth_2, eqwlth_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, eqwlth_1, eqwlth_2, eqwlth_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(eqwlth_1), y2 = as_factor(eqwlth_2),
         y3 = as_factor(eqwlth_3)) %>%
  mutate(y1 = recode(y1, "GOVT REDUCE DIFF"=1, "2"=2, "3"=3, "4"=4,
                     "5"=5, "6"=6, "NO GOVT ACTION"=7, "DK"=4),
         y2 = recode(y2, "GOVT REDUCE DIFF"=1, "2"=2, "3"=3, "4"=4,
                     "5"=5, "6"=6, "NO GOVT ACTION"=7, "DK"=4),
         y3 = recode(y3, "GOVT REDUCE DIFF"=1, "2"=2, "3"=3, "4"=4,
                     "5"=5, "6"=6, "NO GOVT ACTION"=7, "DK"=4))
eqwlth_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="eqwlth",
                      qtype="7")


#### Questions needing adjustment
# homosex
df <- bind_rows(g6 %>% select(id_1, homosex_1, homosex_2, homosex_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, homosex_1, homosex_2, homosex_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, homosex_1, homosex_2, homosex_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(homosex_1), y2 = as_factor(homosex_2),
         y3 = as_factor(homosex_3)) %>%
  mutate(y1 = recode(y1, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y2 = recode(y2, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y3 = recode(y3, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3))
homosex_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="homosex",
                    qtype="3")
# premarsx
df <- bind_rows(g6 %>% select(id_1, premarsx_1, premarsx_2, premarsx_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, premarsx_1, premarsx_2, premarsx_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, premarsx_1, premarsx_2, premarsx_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(premarsx_1), y2 = as_factor(premarsx_2),
         y3 = as_factor(premarsx_3)) %>%
  mutate(y1 = recode(y1, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y2 = recode(y2, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y3 = recode(y3, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3))
premarsx_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="premarsx",
                     qtype="3")

# xmarsex_model$pattern_param_summary %>% mutate(var = "xmarsex"),
df <- bind_rows(g6 %>% select(id_1, xmarsex_1, xmarsex_2, xmarsex_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, xmarsex_1, xmarsex_2, xmarsex_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, xmarsex_1, xmarsex_2, xmarsex_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(xmarsex_1), y2 = as_factor(xmarsex_2),
         y3 = as_factor(xmarsex_3)) %>%
  mutate(y1 = recode(y1, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y2 = recode(y2, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y3 = recode(y3, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3))
xmarsex_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="xmarsex",
                      qtype="3")

# teensex_model$pattern_param_summary %>% mutate(var = "teensex"),
df <- bind_rows(g6 %>% select(id_1, teensex_1, teensex_2, teensex_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, teensex_1, teensex_2, teensex_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, teensex_1, teensex_2, teensex_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(teensex_1), y2 = as_factor(teensex_2),
         y3 = as_factor(teensex_3)) %>%
  mutate(y1 = recode(y1, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y2 = recode(y2, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3),
         y3 = recode(y3, "ALWAYS WRONG"=2, "ALMST ALWAYS WRG"=2, "SOMETIMES WRONG"=4,
                     "NOT WRONG AT ALL"=4, "other"=3, "DK"=3))
teensex_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="teensex",
                     qtype="3")
# conbus_model$pattern_param_summary %>% mutate(var = "conbus"),
df <- bind_rows(g6 %>% select(id_1, conbus_1, conbus_2, conbus_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conbus_1, conbus_2, conbus_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conbus_1, conbus_2, conbus_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conbus_1), y2 = as_factor(conbus_2),
         y3 = as_factor(conbus_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conbus_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="conbus",
                     qtype="3")
# confinan_model$pattern_param_summary %>% mutate(var = "confinan"),
df <- bind_rows(g6 %>% select(id_1, confinan_1, confinan_2, confinan_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, confinan_1, confinan_2, confinan_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, confinan_1, confinan_2, confinan_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(confinan_1), y2 = as_factor(confinan_2),
         y3 = as_factor(confinan_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
confinan_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="confinan",
                    qtype="3")
# conclerg_model$pattern_param_summary %>% mutate(var = "conclerg"),
df <- bind_rows(g6 %>% select(id_1, conclerg_1, conclerg_2, conclerg_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conclerg_1, conclerg_2, conclerg_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conclerg_1, conclerg_2, conclerg_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conclerg_1), y2 = as_factor(conclerg_2),
         y3 = as_factor(conclerg_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conclerg_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="conclerg",
                    qtype="3")
# coneduc_model$pattern_param_summary %>% mutate(var = "coneduc"),
df <- bind_rows(g6 %>% select(id_1, coneduc_1, coneduc_2, coneduc_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, coneduc_1, coneduc_2, coneduc_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, coneduc_1, coneduc_2, coneduc_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(coneduc_1), y2 = as_factor(coneduc_2),
         y3 = as_factor(coneduc_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
coneduc_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="coneduc",
                    qtype="3")
# confed_model$pattern_param_summary %>% mutate(var = "confed"),
df <- bind_rows(g6 %>% select(id_1, confed_1, confed_2, confed_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, confed_1, confed_2, confed_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, confed_1, confed_2, confed_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(confed_1), y2 = as_factor(confed_2),
         y3 = as_factor(confed_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
confed_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="confed",
                    qtype="3")
# conlabor_model$pattern_param_summary %>% mutate(var = "conlabor"),
df <- bind_rows(g6 %>% select(id_1, conlabor_1, conlabor_2, conlabor_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conlabor_1, conlabor_2, conlabor_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conlabor_1, conlabor_2, conlabor_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conlabor_1), y2 = as_factor(conlabor_2),
         y3 = as_factor(conlabor_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conlabor_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="conlabor",
                    qtype="3")
#conpress_model$pattern_param_summary %>% mutate(var = "conpress"),
df <- bind_rows(g6 %>% select(id_1, conpress_1, conpress_2, conpress_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conpress_1, conpress_2, conpress_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conpress_1, conpress_2, conpress_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conpress_1), y2 = as_factor(conpress_2),
         y3 = as_factor(conpress_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conpress_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="conpress",
                    qtype="3")
# conmedic_model$pattern_param_summary %>% mutate(var = "conmedic"),
df <- bind_rows(g6 %>% select(id_1, conmedic_1, conmedic_2, conmedic_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conmedic_1, conmedic_2, conmedic_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conmedic_1, conmedic_2, conmedic_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conmedic_1), y2 = as_factor(conmedic_2),
         y3 = as_factor(conmedic_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conmedic_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="conmedic",
                    qtype="3")
# contv_model$pattern_param_summary %>% mutate(var = "contv"),
df <- bind_rows(g6 %>% select(id_1, contv_1, contv_2, contv_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, contv_1, contv_2, contv_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, contv_1, contv_2, contv_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(contv_1), y2 = as_factor(contv_2),
         y3 = as_factor(contv_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
contv_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="contv",
                    qtype="3")
# conjudge_model$pattern_param_summary %>% mutate(var = "conjudge"),
df <- bind_rows(g6 %>% select(id_1, conjudge_1, conjudge_2, conjudge_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conjudge_1, conjudge_2, conjudge_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conjudge_1, conjudge_2, conjudge_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conjudge_1), y2 = as_factor(conjudge_2),
         y3 = as_factor(conjudge_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conjudge_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="conjudge",
                    qtype="3")
# consci_model$pattern_param_summary %>% mutate(var = "consci"),
df <- bind_rows(g6 %>% select(id_1, consci_1, consci_2, consci_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, consci_1, consci_2, consci_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, consci_1, consci_2, consci_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(consci_1), y2 = as_factor(consci_2),
         y3 = as_factor(consci_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
consci_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="consci",
                    qtype="3")
# conlegis_model$pattern_param_summary %>% mutate(var = "conlegis"),
df <- bind_rows(g6 %>% select(id_1, conlegis_1, conlegis_2, conlegis_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conlegis_1, conlegis_2, conlegis_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conlegis_1, conlegis_2, conlegis_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conlegis_1), y2 = as_factor(conlegis_2),
         y3 = as_factor(conlegis_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conlegis_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="conlegis",
                    qtype="3")
# conarmy_model$pattern_param_summary %>% mutate(var = "conarmy")) %>%
df <- bind_rows(g6 %>% select(id_1, conarmy_1, conarmy_2, conarmy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, conarmy_1, conarmy_2, conarmy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, conarmy_1, conarmy_2, conarmy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(conarmy_1), y2 = as_factor(conarmy_2),
         y3 = as_factor(conarmy_3)) %>%
  mutate(y1 = recode(y1, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y2 = recode(y2, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3),
         y3 = recode(y3, "A GREAT DEAL"=2, "ONLY SOME"=4, "HARDLY ANY"=4, "DK"=3))
conarmy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="conarmy",
                    qtype="3")

#natspac
df <- bind_rows(g6 %>% select(id_1, natspac_1, natspac_2, natspac_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natspac_1, natspac_2, natspac_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natspac_1, natspac_2, natspac_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natspac_1), y2 = as_factor(natspac_2),
         y3 = as_factor(natspac_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natspac_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natspac",
                     qtype="3")

#natenvir
df <- bind_rows(g6 %>% select(id_1, natenvir_1, natenvir_2, natenvir_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natenvir_1, natenvir_2, natenvir_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natenvir_1, natenvir_2, natenvir_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natenvir_1), y2 = as_factor(natenvir_2),
         y3 = as_factor(natenvir_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natenvir_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natenvir",
                     qtype="3")

#natheal
df <- bind_rows(g6 %>% select(id_1, natheal_1, natheal_2, natheal_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natheal_1, natheal_2, natheal_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natheal_1, natheal_2, natheal_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natheal_1), y2 = as_factor(natheal_2),
         y3 = as_factor(natheal_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natheal_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natheal",
                     qtype="3")

#natcity
df <- bind_rows(g6 %>% select(id_1, natcity_1, natcity_2, natcity_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natcity_1, natcity_2, natcity_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natcity_1, natcity_2, natcity_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natcity_1), y2 = as_factor(natcity_2),
         y3 = as_factor(natcity_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natcity_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natcity",
                     qtype="3")

#natcrime
df <- bind_rows(g6 %>% select(id_1, natcrime_1, natcrime_2, natcrime_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natcrime_1, natcrime_2, natcrime_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natcrime_1, natcrime_2, natcrime_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natcrime_1), y2 = as_factor(natcrime_2),
         y3 = as_factor(natcrime_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natcrime_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natcrime",
                     qtype="3")

#natdrug
df <- bind_rows(g6 %>% select(id_1, natdrug_1, natdrug_2, natdrug_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natdrug_1, natdrug_2, natdrug_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natdrug_1, natdrug_2, natdrug_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natdrug_1), y2 = as_factor(natdrug_2),
         y3 = as_factor(natdrug_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natdrug_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natdrug",
                     qtype="3")

#nateduc
df <- bind_rows(g6 %>% select(id_1, nateduc_1, nateduc_2, nateduc_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, nateduc_1, nateduc_2, nateduc_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, nateduc_1, nateduc_2, nateduc_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(nateduc_1), y2 = as_factor(nateduc_2),
         y3 = as_factor(nateduc_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
nateduc_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="nateduc",
                     qtype="3")

#natrace
df <- bind_rows(g6 %>% select(id_1, natrace_1, natrace_2, natrace_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natrace_1, natrace_2, natrace_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natrace_1, natrace_2, natrace_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natrace_1), y2 = as_factor(natrace_2),
         y3 = as_factor(natrace_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natrace_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natrace",
                     qtype="3")

#natarms
df <- bind_rows(g6 %>% select(id_1, natarms_1, natarms_2, natarms_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natarms_1, natarms_2, natarms_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natarms_1, natarms_2, natarms_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natarms_1), y2 = as_factor(natarms_2),
         y3 = as_factor(natarms_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natarms_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natarms",
                     qtype="3")

#nataid
df <- bind_rows(g6 %>% select(id_1, nataid_1, nataid_2, nataid_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, nataid_1, nataid_2, nataid_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, nataid_1, nataid_2, nataid_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(nataid_1), y2 = as_factor(nataid_2),
         y3 = as_factor(nataid_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
nataid_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="nataid",
                     qtype="3")

#natfare
df <- bind_rows(g6 %>% select(id_1, natfare_1, natfare_2, natfare_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natfare_1, natfare_2, natfare_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natfare_1, natfare_2, natfare_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natfare_1), y2 = as_factor(natfare_2),
         y3 = as_factor(natfare_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natfare_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natfare",
                     qtype="3")

#natroad
df <- bind_rows(g6 %>% select(id_1, natroad_1, natroad_2, natroad_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natroad_1, natroad_2, natroad_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natroad_1, natroad_2, natroad_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natroad_1), y2 = as_factor(natroad_2),
         y3 = as_factor(natroad_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natroad_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natroad",
                     qtype="3")

#natsoc
df <- bind_rows(g6 %>% select(id_1, natsoc_1, natsoc_2, natsoc_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natsoc_1, natsoc_2, natsoc_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natsoc_1, natsoc_2, natsoc_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natsoc_1), y2 = as_factor(natsoc_2),
         y3 = as_factor(natsoc_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natsoc_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natsoc",
                     qtype="3")

#natmass
df <- bind_rows(g6 %>% select(id_1, natmass_1, natmass_2, natmass_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natmass_1, natmass_2, natmass_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natmass_1, natmass_2, natmass_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natmass_1), y2 = as_factor(natmass_2),
         y3 = as_factor(natmass_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natmass_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natmass",
                     qtype="3")

#natpark
df <- bind_rows(g6 %>% select(id_1, natpark_1, natpark_2, natpark_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natpark_1, natpark_2, natpark_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natpark_1, natpark_2, natpark_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natpark_1), y2 = as_factor(natpark_2),
         y3 = as_factor(natpark_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natpark_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natpark",
                     qtype="3")

#natchld
df <- bind_rows(g6 %>% select(id_1, natchld_1, natchld_2, natchld_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natchld_1, natchld_2, natchld_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natchld_1, natchld_2, natchld_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natchld_1), y2 = as_factor(natchld_2),
         y3 = as_factor(natchld_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natchld_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natchld",
                     qtype="3")

#natsci
df <- bind_rows(g6 %>% select(id_1, natsci_1, natsci_2, natsci_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natsci_1, natsci_2, natsci_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natsci_1, natsci_2, natsci_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natsci_1), y2 = as_factor(natsci_2),
         y3 = as_factor(natsci_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natsci_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="natsci",
                     qtype="3")


#natspacy
df <- bind_rows(g6 %>% select(id_1, natspacy_1, natspacy_2, natspacy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natspacy_1, natspacy_2, natspacy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natspacy_1, natspacy_2, natspacy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natspacy_1), y2 = as_factor(natspacy_2),
         y3 = as_factor(natspacy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natspacy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natspacy",
                    qtype="3")

#natenviy
df <- bind_rows(g6 %>% select(id_1, natenviy_1, natenviy_2, natenviy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natenviy_1, natenviy_2, natenviy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natenviy_1, natenviy_2, natenviy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natenviy_1), y2 = as_factor(natenviy_2),
         y3 = as_factor(natenviy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natenviy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natenviy",
                    qtype="3")

#nathealy
df <- bind_rows(g6 %>% select(id_1, nathealy_1, nathealy_2, nathealy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, nathealy_1, nathealy_2, nathealy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, nathealy_1, nathealy_2, nathealy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(nathealy_1), y2 = as_factor(nathealy_2),
         y3 = as_factor(nathealy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
nathealy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="nathealy",
                    qtype="3")

#natcityy
df <- bind_rows(g6 %>% select(id_1, natcityy_1, natcityy_2, natcityy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natcityy_1, natcityy_2, natcityy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natcityy_1, natcityy_2, natcityy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natcityy_1), y2 = as_factor(natcityy_2),
         y3 = as_factor(natcityy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natcityy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natcityy",
                    qtype="3")

#natcrimy
df <- bind_rows(g6 %>% select(id_1, natcrimy_1, natcrimy_2, natcrimy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natcrimy_1, natcrimy_2, natcrimy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natcrimy_1, natcrimy_2, natcrimy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natcrimy_1), y2 = as_factor(natcrimy_2),
         y3 = as_factor(natcrimy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natcrimy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natcrimy",
                    qtype="3")

#natdrugy
df <- bind_rows(g6 %>% select(id_1, natdrugy_1, natdrugy_2, natdrugy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natdrugy_1, natdrugy_2, natdrugy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natdrugy_1, natdrugy_2, natdrugy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natdrugy_1), y2 = as_factor(natdrugy_2),
         y3 = as_factor(natdrugy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natdrugy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natdrugy",
                    qtype="3")

#nateducy
df <- bind_rows(g6 %>% select(id_1, nateducy_1, nateducy_2, nateducy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, nateducy_1, nateducy_2, nateducy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, nateducy_1, nateducy_2, nateducy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(nateducy_1), y2 = as_factor(nateducy_2),
         y3 = as_factor(nateducy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
nateducy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="nateducy",
                    qtype="3")

#natracey
df <- bind_rows(g6 %>% select(id_1, natracey_1, natracey_2, natracey_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natracey_1, natracey_2, natracey_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natracey_1, natracey_2, natracey_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natracey_1), y2 = as_factor(natracey_2),
         y3 = as_factor(natracey_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natracey_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natracey",
                    qtype="3")

#natarmsy
df <- bind_rows(g6 %>% select(id_1, natarmsy_1, natarmsy_2, natarmsy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natarmsy_1, natarmsy_2, natarmsy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natarmsy_1, natarmsy_2, natarmsy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natarmsy_1), y2 = as_factor(natarmsy_2),
         y3 = as_factor(natarmsy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natarmsy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natarmsy",
                    qtype="3")

#nataidy
df <- bind_rows(g6 %>% select(id_1, nataidy_1, nataidy_2, nataidy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, nataidy_1, nataidy_2, nataidy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, nataidy_1, nataidy_2, nataidy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(nataidy_1), y2 = as_factor(nataidy_2),
         y3 = as_factor(nataidy_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
nataidy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="nataidy",
                    qtype="3")

#natfarey
df <- bind_rows(g6 %>% select(id_1, natfarey_1, natfarey_2, natfarey_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, natfarey_1, natfarey_2, natfarey_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, natfarey_1, natfarey_2, natfarey_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(natfarey_1), y2 = as_factor(natfarey_2),
         y3 = as_factor(natfarey_3)) %>%
  mutate(y1 = recode(y1, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y2 = recode(y2, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3),
         y3 = recode(y3, "TOO MUCH"=2, "ABOUT RIGHT"=4, "TOO LITTLE"=4, "DK"=3))
natfarey_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="natfarey",
                    qtype="3")


#reliten
df <- bind_rows(g6 %>% select(id_1, reliten_1, reliten_2, reliten_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, reliten_1, reliten_2, reliten_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, reliten_1, reliten_2, reliten_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(reliten_1), y2 = as_factor(reliten_2),
         y3 = as_factor(reliten_3)) %>%
  mutate(y1 = recode(y1, "strong"=2, "SOMEWHAT STRONG"=3, "NOT VERY STRONG"=4, 
                     "NO RELIGION"=4, "DK"=3),
         y2 = recode(y2, "strong"=2, "SOMEWHAT STRONG"=3, "NOT VERY STRONG"=4, 
                     "NO RELIGION"=4, "DK"=3),
         y3 = recode(y3, "strong"=2, "SOMEWHAT STRONG"=3, "NOT VERY STRONG"=4, 
                     "NO RELIGION"=4, "DK"=3))
reliten_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="reliten",
                    qtype="3")

#bible
df <- bind_rows(g6 %>% select(id_1, bible_1, bible_2, bible_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, bible_1, bible_2, bible_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, bible_1, bible_2, bible_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(bible_1), y2 = as_factor(bible_2),
         y3 = as_factor(bible_3)) %>%
  mutate(y1 = recode(y1, "WORD OF GOD"=2, "INSPIRED WORD"=4, "BOOK OF FABLES"=4, 
                     "other"=4, "DK"=3),
         y2 = recode(y2, "WORD OF GOD"=2, "INSPIRED WORD"=4, "BOOK OF FABLES"=4, 
                     "other"=4, "DK"=3),
         y3 = recode(y3, "WORD OF GOD"=2, "INSPIRED WORD"=4, "BOOK OF FABLES"=4, 
                     "other"=4, "DK"=3))
bible_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="bible",
                     qtype="3")
#tax
df <- bind_rows(g6 %>% select(id_1, tax_1, tax_2, tax_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, tax_1, tax_2, tax_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, tax_1, tax_2, tax_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(tax_1), y2 = as_factor(tax_2),
         y3 = as_factor(tax_3)) %>%
  mutate(y1 = recode(y1, "TOO HIGH"=2, "ABOUT RIGHT"=4, "TOO LOW"=4, 
                     "R PAYS NONE<VOL.>"=4, "DK"=3),
         y2 = recode(y2, "TOO HIGH"=2, "ABOUT RIGHT"=4, "TOO LOW"=4, 
                     "R PAYS NONE<VOL.>"=4, "DK"=3),
         y3 = recode(y3, "TOO HIGH"=2, "ABOUT RIGHT"=4, "TOO LOW"=4, 
                     "R PAYS NONE<VOL.>"=4, "DK"=3))
tax_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="tax",
                   qtype="3")
#closeblk
df <- bind_rows(g6 %>% select(id_1, closeblk_1, closeblk_2, closeblk_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, closeblk_1, closeblk_2, closeblk_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, closeblk_1, closeblk_2, closeblk_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(closeblk_1), y2 = as_factor(closeblk_2),
         y3 = as_factor(closeblk_3)) %>%
  mutate(y1 = recode(y1, "NOT AT ALL CLOSE"=1, "2"=1, "3"=2, "4"=2, 
                     "NEITHER ONE OR THE OTHER"=3, "6"=4, "7"=4, "8"=5,
                     "VERY CLOSE"=5, "DK"=3),
         y2 = recode(y2, "NOT AT ALL CLOSE"=1, "2"=1, "3"=2, "4"=2, 
                     "NEITHER ONE OR THE OTHER"=3, "6"=4, "7"=4, "8"=5,
                     "VERY CLOSE"=5, "DK"=3),
         y3 = recode(y3, "NOT AT ALL CLOSE"=1, "2"=1, "3"=2, "4"=2, 
                     "NEITHER ONE OR THE OTHER"=3, "6"=4, "7"=4, "8"=5,
                     "VERY CLOSE"=5, "DK"=3))
closeblk_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                 data=df, cov_estimator="binom", iterations=2500,
                 n_chains=5, burn=500, var_name="closeblk",
                 qtype="5")

#closewht
df <- bind_rows(g6 %>% select(id_1, closewht_1, closewht_2, closewht_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, closewht_1, closewht_2, closewht_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, closewht_1, closewht_2, closewht_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(closewht_1), y2 = as_factor(closewht_2),
         y3 = as_factor(closewht_3)) %>%
  mutate(y1 = recode(y1, "NOT AT ALL CLOSE"=1, "2"=1, "3"=2, "4"=2, 
                     "NEITHER ONE OR THE OTHER"=3, "6"=4, "7"=4, "8"=5,
                     "VERY CLOSE"=5, "DK"=3),
         y2 = recode(y2, "NOT AT ALL CLOSE"=1, "2"=1, "3"=2, "4"=2, 
                     "NEITHER ONE OR THE OTHER"=3, "6"=4, "7"=4, "8"=5,
                     "VERY CLOSE"=5, "DK"=3),
         y3 = recode(y3, "NOT AT ALL CLOSE"=1, "2"=1, "3"=2, "4"=2, 
                     "NEITHER ONE OR THE OTHER"=3, "6"=4, "7"=4, "8"=5,
                     "VERY CLOSE"=5, "DK"=3))
closewht_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="closewht",
                      qtype="5")

#happy
df <- bind_rows(g6 %>% select(id_1, happy_1, happy_2, happy_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, happy_1, happy_2, happy_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, happy_1, happy_2, happy_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(happy_1), y2 = as_factor(happy_2),
         y3 = as_factor(happy_3)) %>%
  mutate(y1 = recode(y1, "VERY HAPPY"=2, "PRETTY HAPPY"=2, "NOT TOO HAPPY"=4, "DK"=3),
         y2 = recode(y2, "VERY HAPPY"=2, "PRETTY HAPPY"=2, "NOT TOO HAPPY"=4, "DK"=3),
         y3 = recode(y3, "VERY HAPPY"=2, "PRETTY HAPPY"=2, "NOT TOO HAPPY"=4, "DK"=3))
happy_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="happy",
                      qtype="3")

#hapmar
df <- bind_rows(g6 %>% select(id_1, hapmar_1, hapmar_2, hapmar_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, hapmar_1, hapmar_2, hapmar_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, hapmar_1, hapmar_2, hapmar_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(hapmar_1), y2 = as_factor(hapmar_2),
         y3 = as_factor(hapmar_3)) %>%
  mutate(y1 = recode(y1, "VERY HAPPY"=2, "PRETTY HAPPY"=2, "NOT TOO HAPPY"=4, "DK"=3),
         y2 = recode(y2, "VERY HAPPY"=2, "PRETTY HAPPY"=2, "NOT TOO HAPPY"=4, "DK"=3),
         y3 = recode(y3, "VERY HAPPY"=2, "PRETTY HAPPY"=2, "NOT TOO HAPPY"=4, "DK"=3))
hapmar_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                   data=df, cov_estimator="binom", iterations=2500,
                   n_chains=5, burn=500, var_name="hapmar",
                   qtype="3")

#health

#obey
df <- bind_rows(g6 %>% select(id_1, obey_1, obey_2, obey_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, obey_1, obey_2, obey_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, obey_1, obey_2, obey_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(obey_1), y2 = as_factor(obey_2),
         y3 = as_factor(obey_3)) %>%
  mutate(y1 = recode(y1, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y2 = recode(y2, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y3 = recode(y3, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3))
obey_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="obey",
                    qtype="3")
#popular (slightly different than others)
#Is it the least important?
#Wont converge without revision...
df <- bind_rows(g6 %>% select(id_1, popular_1, popular_2, popular_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, popular_1, popular_2, popular_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, popular_1, popular_2, popular_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(popular_1), y2 = as_factor(popular_2),
         y3 = as_factor(popular_3)) %>%
  mutate(y1 = recode(y1, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=2, 
                     "4TH IMPORTANT"=2, "LEAST IMPORTANT"=4, "DK"=3),
         y2 = recode(y2, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=2, 
                     "4TH IMPORTANT"=2, "LEAST IMPORTANT"=4, "DK"=3),
         y3 = recode(y3, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=2, 
                     "4TH IMPORTANT"=2, "LEAST IMPORTANT"=4, "DK"=3))
popular_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="popular",
                  qtype="3")

#thnkself
df <- bind_rows(g6 %>% select(id_1, thnkself_1, thnkself_2, thnkself_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, thnkself_1, thnkself_2, thnkself_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, thnkself_1, thnkself_2, thnkself_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(thnkself_1), y2 = as_factor(thnkself_2),
         y3 = as_factor(thnkself_3)) %>%
  mutate(y1 = recode(y1, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y2 = recode(y2, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y3 = recode(y3, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3))
thnkself_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                  data=df, cov_estimator="binom", iterations=2500,
                  n_chains=5, burn=500, var_name="thnkself",
                  qtype="3")

#workhard
df <- bind_rows(g6 %>% select(id_1, workhard_1, workhard_2, workhard_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, workhard_1, workhard_2, workhard_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, workhard_1, workhard_2, workhard_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(workhard_1), y2 = as_factor(workhard_2),
         y3 = as_factor(workhard_3)) %>%
  mutate(y1 = recode(y1, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y2 = recode(y2, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y3 = recode(y3, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3))
workhard_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="workhard",
                      qtype="3")
#helpoth
df <- bind_rows(g6 %>% select(id_1, helpoth_1, helpoth_2, helpoth_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, helpoth_1, helpoth_2, helpoth_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, helpoth_1, helpoth_2, helpoth_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(helpoth_1), y2 = as_factor(helpoth_2),
         y3 = as_factor(helpoth_3)) %>%
  mutate(y1 = recode(y1, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y2 = recode(y2, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3),
         y3 = recode(y3, "MOST IMPORTANT"=2, "2ND IMPORTANT"=2, "3RD IMPORTANT"=4, 
                     "4TH IMPORTANT"=4, "LEAST IMPORTANT"=4, "DK"=3))
helpoth_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="helpoth",
                      qtype="3")

#joblose
df <- bind_rows(g6 %>% select(id_1, joblose_1, joblose_2, joblose_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, joblose_1, joblose_2, joblose_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, joblose_1, joblose_2, joblose_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(joblose_1), y2 = as_factor(joblose_2),
         y3 = as_factor(joblose_3)) %>%
  mutate(y1 = recode(y1, "VERY LIKELY"=1, "FAIRLY LIKELY"=2, "NOT TOO LIKELY"=4, 
                     "NOT LIKELY"=5, "LEAVING LABOR FORCE"=NA_real_, "DK"=3),
         y2 = recode(y2, "VERY LIKELY"=1, "FAIRLY LIKELY"=2, "NOT TOO LIKELY"=4, 
                     "NOT LIKELY"=5, "LEAVING LABOR FORCE"=NA_real_, "DK"=3),
         y3 = recode(y3, "VERY LIKELY"=1, "FAIRLY LIKELY"=2, "NOT TOO LIKELY"=4, 
                     "NOT LIKELY"=5, "LEAVING LABOR FORCE"=NA_real_, "DK"=3))
joblose_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="joblose",
                     qtype="5")

#jobfind
df <- bind_rows(g6 %>% select(id_1, jobfind_1, jobfind_2, jobfind_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, jobfind_1, jobfind_2, jobfind_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, jobfind_1, jobfind_2, jobfind_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(jobfind_1), y2 = as_factor(jobfind_2),
         y3 = as_factor(jobfind_3)) %>%
  mutate(y1 = recode(y1, "VERY EASY"=2, "SOMEWHAT EASY"=2, "NOT EASY"=4, "DK"=3),
         y2 = recode(y2, "VERY EASY"=2, "SOMEWHAT EASY"=2, "NOT EASY"=4, "DK"=3),
         y3 = recode(y3, "VERY EASY"=2, "SOMEWHAT EASY"=2, "NOT EASY"=4, "DK"=3))
jobfind_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="jobfind",
                     qtype="3")
#satjob
df <- bind_rows(g6 %>% select(id_1, satjob_1, satjob_2, satjob_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, satjob_1, satjob_2, satjob_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, satjob_1, satjob_2, satjob_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(satjob_1), y2 = as_factor(satjob_2),
         y3 = as_factor(satjob_3)) %>%
  mutate(y1 = recode(y1, "VERY SATISFIED"=1, "MOD. SATISFIED"=2, "A LITTLE DISSAT"=4, 
                     "VERY DISSATISFIED"=5, "DK"=3),
         y2 = recode(y2, "VERY SATISFIED"=1, "MOD. SATISFIED"=2, "A LITTLE DISSAT"=4, 
                     "VERY DISSATISFIED"=5, "DK"=3),
         y3 = recode(y3, "VERY SATISFIED"=1, "MOD. SATISFIED"=2, "A LITTLE DISSAT"=4, 
                     "VERY DISSATISFIED"=5, "DK"=3))
satjob_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="satjob",
                     qtype="5")
#satfin
df <- bind_rows(g6 %>% select(id_1, satfin_1, satfin_2, satfin_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, satfin_1, satfin_2, satfin_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, satfin_1, satfin_2, satfin_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(satfin_1), y2 = as_factor(satfin_2),
         y3 = as_factor(satfin_3)) %>%
  mutate(y1 = recode(y1, "satisfied"=2, "MORE OR LESS"=3, "NOT AT ALL SAT"=4, 
                     "DK"=3),
         y2 = recode(y2, "satisfied"=2, "MORE OR LESS"=3, "NOT AT ALL SAT"=4, 
                     "DK"=3),
         y3 = recode(y3, "satisfied"=2, "MORE OR LESS"=3, "NOT AT ALL SAT"=4, 
                     "DK"=3))
satfin_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="satfin",
                    qtype="3")
#finalter

#getahead
df <- bind_rows(g6 %>% select(id_1, getahead_1, getahead_2, getahead_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, getahead_1, getahead_2, getahead_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, getahead_1, getahead_2, getahead_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(getahead_1), y2 = as_factor(getahead_2),
         y3 = as_factor(getahead_3)) %>%
  mutate(y1 = recode(y1, "HARD WORK"=2, "BOTH EQUALLY"=3, "LUCK OR HELP"=4, 
                     "DK"=3, "other"=3),
         y2 = recode(y2, "HARD WORK"=2, "BOTH EQUALLY"=3, "LUCK OR HELP"=4, 
                     "DK"=3, "other"=3),
         y3 = recode(y3, "HARD WORK"=2, "BOTH EQUALLY"=3, "LUCK OR HELP"=4, 
                     "DK"=3, "other"=3))
getahead_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="getahead",
                    qtype="3")

#sexeduc
df <- bind_rows(g6 %>% select(id_1, sexeduc_1, sexeduc_2, sexeduc_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, sexeduc_1, sexeduc_2, sexeduc_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, sexeduc_1, sexeduc_2, sexeduc_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(sexeduc_1), y2 = as_factor(sexeduc_2),
         y3 = as_factor(sexeduc_3)) %>%
  mutate(y1 = recode(y1, "favor"=2, "depends"=3, "oppose"=4, "DK"=3),
         y2 = recode(y2, "favor"=2, "depends"=3, "oppose"=4, "DK"=3),
         y3 = recode(y3, "favor"=2, "depends"=3, "oppose"=4, "DK"=3))
sexeduc_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="sexeduc",
                      qtype="3")
#divlaw
df <- bind_rows(g6 %>% select(id_1, divlaw_1, divlaw_2, divlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, divlaw_1, divlaw_2, divlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, divlaw_1, divlaw_2, divlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(divlaw_1), y2 = as_factor(divlaw_2),
         y3 = as_factor(divlaw_3)) %>%
  mutate(y1 = recode(y1, "easier"=2, "MORE DIFFICULT"=4, "STAY SAME"=4, "DK"=3),
         y2 = recode(y2, "easier"=2, "MORE DIFFICULT"=4, "STAY SAME"=4, "DK"=3),
         y3 = recode(y3, "easier"=2, "MORE DIFFICULT"=4, "STAY SAME"=4, "DK"=3))
divlaw_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="divlaw",
                     qtype="3")
#pornlaw
df <- bind_rows(g6 %>% select(id_1, pornlaw_1, pornlaw_2, pornlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, pornlaw_1, pornlaw_2, pornlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, pornlaw_1, pornlaw_2, pornlaw_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(pornlaw_1), y2 = as_factor(pornlaw_2),
         y3 = as_factor(pornlaw_3)) %>%
  mutate(y1 = recode(y1, "ILLEGAL TO ALL"=2, "ILLEGAL UNDER 18"=4, "legal"=4, "DK"=3),
         y2 = recode(y2, "ILLEGAL TO ALL"=2, "ILLEGAL UNDER 18"=4, "legal"=4, "DK"=3),
         y3 = recode(y3, "ILLEGAL TO ALL"=2, "ILLEGAL UNDER 18"=4, "legal"=4, "DK"=3))
pornlaw_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="pornlaw",
                    qtype="3")


#god
df <- bind_rows(g6 %>% select(id_1, god_1, god_2, god_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, god_1, god_2, god_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, god_1, god_2, god_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(god_1), y2 = as_factor(god_2),
         y3 = as_factor(god_3)) %>%
  mutate(y1 = recode(y1, "KNOW GOD EXISTS"=2, "BELIEVE BUT DOUBTS"=4, 
                     "BELIEVE SOMETIMES"=4, "SOME HIGHER POWER"=4,
                     "NO WAY TO FIND OUT"=4, "DONT BELIEVE"=4, "DK"=3),
         y2 = recode(y2, "KNOW GOD EXISTS"=2, "BELIEVE BUT DOUBTS"=4, 
                     "BELIEVE SOMETIMES"=4, "SOME HIGHER POWER"=4,
                     "NO WAY TO FIND OUT"=4, "DONT BELIEVE"=4, "DK"=3),
         y3 = recode(y3, "KNOW GOD EXISTS"=2, "BELIEVE BUT DOUBTS"=4, 
                     "BELIEVE SOMETIMES"=4, "SOME HIGHER POWER"=4,
                     "NO WAY TO FIND OUT"=4, "DONT BELIEVE"=4, "DK"=3))
godgss_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="god",
                     qtype="3")

#rellife
df <- bind_rows(g6 %>% select(id_1, rellife_1, rellife_2, rellife_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, rellife_1, rellife_2) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, rellife_1) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(rellife_1), y2 = as_factor(rellife_2),
         y3 = as_factor(rellife_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY AGREE"=1, "agree"=2, "DONT KNOW"=3,
                     "disagree"=4, "STRONG DISAGREE"=5),
         y2 = recode(y2, "STRONGLY AGREE"=1, "agree"=2, "DONT KNOW"=3,
                     "disagree"=4, "STRONG DISAGREE"=5),
         y3 = recode(y3, "STRONGLY AGREE"=1, "agree"=2, "DONT KNOW"=3,
                     "disagree"=4, "STRONG DISAGREE"=5))
rellife_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                 data=df, cov_estimator="binom", iterations=2500,
                 n_chains=5, burn=500, var_name="rellife",
                 qtype="5")

#relpersn
df <- bind_rows(g6 %>% select(id_1, relpersn_1, relpersn_2, relpersn_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, relpersn_1, relpersn_2, relpersn_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, relpersn_1, relpersn_2, relpersn_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(relpersn_1), y2 = as_factor(relpersn_2),
         y3 = as_factor(relpersn_3)) %>%
  mutate(y1 = recode(y1, "VERY RELIGIOUS"=2, "MODRTE RELIGIOUS"=2, 
                     "SLIGHT RELIGIOUS"=2, "NOT RELIGIOUS"=4, "DONT KNOW"=3),
         y2 = recode(y2, "VERY RELIGIOUS"=2, "MODRTE RELIGIOUS"=2, 
                     "SLIGHT RELIGIOUS"=2, "NOT RELIGIOUS"=4, "DONT KNOW"=3),
         y3 = recode(y3, "VERY RELIGIOUS"=2, "MODRTE RELIGIOUS"=2, 
                     "SLIGHT RELIGIOUS"=2, "NOT RELIGIOUS"=4, "DONT KNOW"=3))
relpersn_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="relpersn",
                     qtype="3")
#sprtprsn
df <- bind_rows(g6 %>% select(id_1, sprtprsn_1, sprtprsn_2, sprtprsn_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, sprtprsn_1, sprtprsn_2, sprtprsn_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, sprtprsn_1, sprtprsn_2, sprtprsn_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(sprtprsn_1), y2 = as_factor(sprtprsn_2),
         y3 = as_factor(sprtprsn_3)) %>%
  mutate(y1 = recode(y1, "VERY SPIRITUAL"=2, "MODEATE SPIRTUAL"=2, 
                     "SLIGHT SPIRITUAL"=2, "NOT SPIRITUAL"=4, "DONT KNOW"=3),
         y2 = recode(y2, "VERY SPIRITUAL"=2, "MODEATE SPIRTUAL"=2, 
                     "SLIGHT SPIRITUAL"=2, "NOT SPIRITUAL"=4, "DONT KNOW"=3),
         y3 = recode(y3, "VERY SPIRITUAL"=2, "MODEATE SPIRTUAL"=2, 
                     "SLIGHT SPIRITUAL"=2, "NOT SPIRITUAL"=4, "DONT KNOW"=3))
sprtprsn_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="sprtprsn",
                      qtype="3")

#punsin
df <- bind_rows(g6 %>% select(id_1, punsin_1, punsin_2, punsin_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, punsin_1, punsin_2) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, punsin_1) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(punsin_1), y2 = as_factor(punsin_2),
         y3 = as_factor(punsin_3)) %>%
  mutate(y1 = recode(y1, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y2 = recode(y2, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y3 = recode(y3, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3))
punsin_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="punsin",
                      qtype="5")
#blkwht
df <- bind_rows(g6 %>% select(id_1, blkwhite_1, blkwhite_2, blkwhite_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, blkwhite_1, blkwhite_2) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, blkwhite_1) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(blkwhite_1), y2 = as_factor(blkwhite_2),
         y3 = as_factor(blkwhite_3)) %>%
  mutate(y1 = recode(y1, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y2 = recode(y2, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y3 = recode(y3, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3))
blkwhite_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="blkwhite",
                    qtype="5")
#rotapple
df <- bind_rows(g6 %>% select(id_1, rotapple_1, rotapple_2, rotapple_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, rotapple_1, rotapple_2) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, rotapple_1) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(rotapple_1), y2 = as_factor(rotapple_2),
         y3 = as_factor(rotapple_3)) %>%
  mutate(y1 = recode(y1, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y2 = recode(y2, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y3 = recode(y3, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3))
rotapple_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="rotapple",
                      qtype="5")
#permoral
df <- bind_rows(g6 %>% select(id_1, permoral_1, permoral_2, permoral_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, permoral_1, permoral_2) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, permoral_1) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(permoral_1), y2 = as_factor(permoral_2),
         y3 = as_factor(permoral_3)) %>%
  mutate(y1 = recode(y1, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y2 = recode(y2, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3),
         y3 = recode(y3, "AGREE STRONGLY"=1, "AGREE SOMEWHAT"=2, 
                     "DISAGREE SOMEWHAT"=4, "DISAGREE STRONGLY"=5, "DK"=3))
permoral_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="permoral",
                      qtype="5")

#marasian
df <- bind_rows(g6 %>% select(id_1, marasian_1, marasian_2, marasian_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, marasian_1, marasian_2, marasian_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, marasian_1, marasian_2, marasian_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(marasian_1), y2 = as_factor(marasian_2),
         y3 = as_factor(marasian_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3))
marasian_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="marasian",
                      qtype="5")

#marhisp
df <- bind_rows(g6 %>% select(id_1, marhisp_1, marhisp_2, marhisp_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, marhisp_1, marhisp_2, marhisp_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, marhisp_1, marhisp_2, marhisp_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(marhisp_1), y2 = as_factor(marhisp_2),
         y3 = as_factor(marhisp_3)) %>%
  mutate(y1 = recode(y1, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y2 = recode(y2, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3),
         y3 = recode(y3, "STRONGLY FAVOR"=1, "favor"=2, "NEITHER FAVOR NOR OPPOSE"=3,
                     "oppose"=4, "STRONGLY OPPOSE"=5, "DONT KNOW"=3))
marhisp_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="marhisp",
                      qtype="5")

#nextgen
df <- bind_rows(g6 %>% select(id_1, nextgen_1, nextgen_2, nextgen_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, nextgen_1, nextgen_2, nextgen_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, nextgen_1, nextgen_2, nextgen_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(nextgen_1), y2 = as_factor(nextgen_2),
         y3 = as_factor(nextgen_3)) %>%
  mutate(y1 = recode(y1, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5),
         y2 = recode(y2, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5),
         y3 = recode(y3, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5))
nextgen_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="nextgen",
                     qtype="5")
#toofast
df <- bind_rows(g6 %>% select(id_1, toofast_1, toofast_2, toofast_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, toofast_1, toofast_2, toofast_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, toofast_1, toofast_2, toofast_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(toofast_1), y2 = as_factor(toofast_2),
         y3 = as_factor(toofast_3)) %>%
  mutate(y1 = recode(y1, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5),
         y2 = recode(y2, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5),
         y3 = recode(y3, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5))
toofast_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="toofast",
                     qtype="5")
#advfront
df <- bind_rows(g6 %>% select(id_1, advfront_1, advfront_2, advfront_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, advfront_1, advfront_2, advfront_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, advfront_1, advfront_2, advfront_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(advfront_1), y2 = as_factor(advfront_2),
         y3 = as_factor(advfront_3)) %>%
  mutate(y1 = recode(y1, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5),
         y2 = recode(y2, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5),
         y3 = recode(y3, "Strongly agree"=1, "Agree"=2, "DONT KNOW"=3,
                     "Disagree"=4, "Strongly disagree"=5))
advfront_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="advfront",
                     qtype="5")

#uswar
#uswary
df <- bind_rows(g6 %>% select(id_1, uswary_1, uswary_2, uswary_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, uswary_1, uswary_2, uswary_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, uswary_1, uswary_2, uswary_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(uswary_1), y2 = as_factor(uswary_2),
         y3 = as_factor(uswary_3)) %>%
  mutate(y1 = recode(y1, "yes"=2, "no"=4, "DK"=3),
         y2 = recode(y2, "yes"=2, "no"=4, "DK"=3),
         y3 = recode(y3, "yes"=2, "no"=4, "DK"=3))
uswary_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="uswary",
                      qtype="3")

#astrosci
df <- bind_rows(g6 %>% select(id_1, astrosci_1, astrosci_2, astrosci_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, astrosci_1, astrosci_2, astrosci_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, astrosci_1, astrosci_2, astrosci_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(astrosci_1), y2 = as_factor(astrosci_2),
         y3 = as_factor(astrosci_3)) %>%
  mutate(y1 = recode(y1, "Very scientific"=2, "Sort of scientific"=2, 
                     "Not at all scientific"=4, "DONT KNOW"=3),
         y2 = recode(y2, "Very scientific"=2, "Sort of scientific"=2, 
                     "Not at all scientific"=4, "DONT KNOW"=3),
         y3 = recode(y3, "Very scientific"=2, "Sort of scientific"=2, 
                     "Not at all scientific"=4, "DONT KNOW"=3))
astrosci_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="astrosci",
                    qtype="3")

#scibnfts
df <- bind_rows(g6 %>% select(id_1, scibnfts_1, scibnfts_2, scibnfts_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, scibnfts_1, scibnfts_2, scibnfts_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, scibnfts_1, scibnfts_2, scibnfts_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(scibnfts_1), y2 = as_factor(scibnfts_2),
         y3 = as_factor(scibnfts_3)) %>%
  mutate(y1 = recode(y1, "Benefits greater"=2, "ABOUT EQUAL IF VOLUNTEERED"=3,
                     "Harmful results greater"=4, "DONT KNOW"=3),
         y2 = recode(y2, "Benefits greater"=2, "ABOUT EQUAL IF VOLUNTEERED"=3,
                     "Harmful results greater"=4, "DONT KNOW"=3),
         y3 = recode(y3, "Benefits greater"=2, "ABOUT EQUAL IF VOLUNTEERED"=3,
                     "Harmful results greater"=4, "DONT KNOW"=3))
scibnfts_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="scibnfts",
                      qtype="3")

#parsol
df <- bind_rows(g6 %>% select(id_1, parsol_1, parsol_2, parsol_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, parsol_1, parsol_2, parsol_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, parsol_1, parsol_2, parsol_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(parsol_1), y2 = as_factor(parsol_2),
         y3 = as_factor(parsol_3)) %>%
  mutate(y1 = recode(y1, "MUCH BETTER"=2, "SOMEWHAT BETTER"=2, "ABOUT THE SAME"=4,
                     "SOMEWHAT WORSE"=4, "MUCH WORSE"=4, "DK"=3),
         y2 = recode(y2, "MUCH BETTER"=2, "SOMEWHAT BETTER"=2, "ABOUT THE SAME"=4,
                     "SOMEWHAT WORSE"=4, "MUCH WORSE"=4, "DK"=3),
         y3 = recode(y3, "MUCH BETTER"=2, "SOMEWHAT BETTER"=2, "ABOUT THE SAME"=4,
                     "SOMEWHAT WORSE"=4, "MUCH WORSE"=4, "DK"=3))
parsol_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="parsol",
                    qtype="3")
#kidssol
df <- bind_rows(g6 %>% select(id_1, kidssol_1, kidssol_2, kidssol_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, kidssol_1, kidssol_2, kidssol_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, kidssol_1, kidssol_2, kidssol_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(kidssol_1), y2 = as_factor(kidssol_2),
         y3 = as_factor(kidssol_3)) %>%
  mutate(y1 = recode(y1, "MUCH BETTER"=2, "SOMEWHAT BETTER"=2, "ABOUT THE SAME"=4,
                     "SOMEWHAT WORSE"=4, "MUCH WORSE"=4, "DK"=3),
         y2 = recode(y2, "MUCH BETTER"=2, "SOMEWHAT BETTER"=2, "ABOUT THE SAME"=4,
                     "SOMEWHAT WORSE"=4, "MUCH WORSE"=4, "DK"=3),
         y3 = recode(y3, "MUCH BETTER"=2, "SOMEWHAT BETTER"=2, "ABOUT THE SAME"=4,
                     "SOMEWHAT WORSE"=4, "MUCH WORSE"=4, "DK"=3))
kidssol_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                    data=df, cov_estimator="binom", iterations=2500,
                    n_chains=5, burn=500, var_name="kidssol",
                    qtype="3")

#courts
df <- bind_rows(g6 %>% select(id_1, courts_1, courts_2, courts_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, courts_1, courts_2, courts_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, courts_1, courts_2, courts_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(courts_1), y2 = as_factor(courts_2),
         y3 = as_factor(courts_3)) %>%
  mutate(y1 = recode(y1, "TOO HARSH"=2, "NOT HARSH ENOUGH"=4, "ABOUT RIGHT"=4, "DK"=3),
         y2 = recode(y2, "TOO HARSH"=2, "NOT HARSH ENOUGH"=4, "ABOUT RIGHT"=4, "DK"=3),
         y3 = recode(y3, "TOO HARSH"=2, "NOT HARSH ENOUGH"=4, "ABOUT RIGHT"=4, "DK"=3))
courts_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                     data=df, cov_estimator="binom", iterations=2500,
                     n_chains=5, burn=500, var_name="courts",
                     qtype="3")

#letin/letin1a
df <- bind_rows(g6 %>% select(id_1, letin1a_1, letin1a_2, letin1a_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, letin1a_1, letin1a_2, letin1a_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, letin1a_1, letin1a_2, letin1a_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(letin1a_1), y2 = as_factor(letin1a_2),
         y3 = as_factor(letin1a_3)) %>%
  mutate(y1 = recode(y1, "Increased a lot"=2, "Increased a little"=2,
                     "Remain the same as it is"=4, "Reduced a little"=4,
                     "Reduced a lot"=4, "Don't know"=3),
         y2 = recode(y2, "Increased a lot"=2, "Increased a little"=2,
                     "Remain the same as it is"=4, "Reduced a little"=4,
                     "Reduced a lot"=4, "Don't know"=3),
         y3 = recode(y3, "Increased a lot"=2, "Increased a little"=2,
                     "Remain the same as it is"=4, "Reduced a little"=4,
                     "Reduced a lot"=4, "Don't know"=3))
letin1_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="letin1",
                      qtype="3")

#life
df <- bind_rows(g6 %>% select(id_1, life_1, life_2, life_3) %>% 
                  mutate(id_1 = paste(id_1, "06", sep = "-")), 
                g8 %>% select(id_1, life_1, life_2, life_3) %>% 
                  mutate(id_1 = paste(id_1, "08", sep = "-")), 
                g10 %>% select(id_1, life_1, life_2, life_3) %>% 
                  mutate(id_1 = paste(id_1, "10", sep = "-"))) %>%
  mutate(y1 = as_factor(life_1), y2 = as_factor(life_2),
         y3 = as_factor(life_3)) %>%
  mutate(y1 = recode(y1, "exciting"=2, "routine"=4, "dull"=4, "DK"=3),
         y2 = recode(y2, "exciting"=2, "routine"=4, "dull"=4, "DK"=3),
         y3 = recode(y3, "exciting"=2, "routine"=4, "dull"=4, "DK"=3))
life_model <- fmm(waves= c("y1", "y2", "y3"), id="id_1",
                      data=df, cov_estimator="binom", iterations=2500,
                      n_chains=5, burn=500, var_name="life",
                      qtype="3")

#Save results
gss_results <- list(letdie1_model, suicide1_model, suicide2_model, suicide3_model, suicide4_model, #5
                    polescap_model, polhitok_model, polabuse_model, polmurdr_model, polattak_model, #10
                    gunlaw_model, fear_model, cappun_model, grass_model, prayer_model, #15
                    spkath_model, spkrac_model, spkcom_model, spkmil_model, spkhomo_model, #20
                    libath_model, librac_model, libcom_model, libmil_model, libhomo_model, #25
                    colath_model, colrac_model, colcom_model, colmil_model, colhomo_model, #30
                    spkmslm_model, colmslm_model, libmslm_model, postlife_model, relexper_model, #35
                    relexp_model, reborn_model, abany_model, abdefect_model, abhlth_model, #40
                   abnomore_model, abpoor_model, abrape_model, absingle_model, racdif1_model, #45
                   racdif2_model, racdif3_model, racdif4_model, fepol_model, richwork_model, #50
                   homosex_model, premarsx_model, xmarsex_model, teensex_model, conbus_model, #55
                   confinan_model, conclerg_model, coneduc_model, confed_model, conlabor_model, #60
                   conpress_model, conmedic_model, contv_model, conjudge_model, consci_model, #65
                   conlegis_model, conarmy_model, fair_model, trust_model, helpful_model, #70
                   aged_model, racopen_model, polviews_model, eqwlth_model, partyid_model, #75
                   fepresch_model, fechld_model, spanking_model, fefam_model, pillok_model, #80
                   discaffw_model, discaffm_model, fejobaff_model, affrmact_model, popespks_model, #85
                   meovrwrk_model, fehire_model, marhomo_model, wrkwayup_model, goodlife_model, #90
                   marwht_model, marblk_model, liveblks_model, livewhts_model, helppoor_model, #95
                   helpnot_model, helpsick_model, helpblk_model, natspac_model, natenvir_model, #100
                   natheal_model, natcity_model, natcrime_model, natdrug_model, nateduc_model, #105
                   natrace_model, natarms_model, nataid_model, natfare_model, natroad_model, #110
                   natsoc_model, natmass_model, natpark_model, natchld_model, natsci_model, #115
                   natspacy_model, natenviy_model, nathealy_model, natcityy_model, natcrimy_model, #120
                   natdrugy_model, nateducy_model, natracey_model, natarmsy_model, nataidy_model, #125
                   natfarey_model,  reliten_model, bible_model, tax_model, closeblk_model, #130
                   closewht_model, happy_model, obey_model, popular_model, thnkself_model, #135
                   workhard_model, helpoth_model, joblose_model, jobfind_model, satjob_model, #140
                   satfin_model, getahead_model, sexeduc_model, divlaw_model, pornlaw_model, #145
                   godgss_model, rellife_model, relpersn_model, sprtprsn_model, punsin_model, #150
                   rotapple_model, blkwhite_model, permoral_model, marasian_model, marhisp_model, #155
                   nextgen_model, toofast_model,  advfront_model, uswary_model, astrosci_model, #160
                   scibnfts_model, parsol_model, kidssol_model, courts_model, letin1_model, #165
                   life_model) #166

#local save (for KK)
#save(gss_results, file = "~/Dropbox/hill_kreisi/results/gssresults.Rdata")

#clean up space
rm(letdie1_model, suicide1_model, suicide2_model, suicide3_model, 
   suicide4_model, polescap_model, polhitok_model, polabuse_model,
   polmurdr_model, polattak_model, gunlaw_model, fear_model,
   cappun_model, grass_model, prayer_model, spkath_model, spkrac_model,
   spkcom_model, spkmil_model, spkhomo_model, libath_model, librac_model,
   libcom_model, libmil_model, libhomo_model, colath_model, colrac_model,
   colcom_model, colmil_model, colhomo_model, 
   spkmslm_model, colmslm_model, libmslm_model,
   postlife_model, relexper_model,
   relexp_model, reborn_model, abany_model, abdefect_model, abhlth_model,
   abnomore_model, abpoor_model, abrape_model, absingle_model,
   racdif1_model, racdif2_model, racdif3_model, racdif4_model,
   fepol_model, richwork_model, homosex_model, premarsx_model,
   xmarsex_model, teensex_model, conbus_model, confinan_model, conclerg_model,
   coneduc_model, confed_model, conlabor_model, conpress_model, conmedic_model,
   contv_model, conjudge_model, consci_model, conlegis_model, conarmy_model,
   fair_model, trust_model, helpful_model,
   aged_model, racopen_model, polviews_model, eqwlth_model, partyid_model,
   fepresch_model, fechld_model, spanking_model, fefam_model, pillok_model,
   discaffw_model, discaffm_model, fejobaff_model, affrmact_model, popespks_model,
   meovrwrk_model, fehire_model, marhomo_model, wrkwayup_model, goodlife_model,
   marwht_model, marblk_model, liveblks_model, livewhts_model, helppoor_model,
   helpnot_model, helpsick_model, helpblk_model,
   natspac_model, natenvir_model, natheal_model, natcity_model, 
   natcrime_model, natdrug_model, nateduc_model, natrace_model, 
   natarms_model, nataid_model, natfare_model, natroad_model, 
   natsoc_model, natmass_model, natpark_model, natchld_model,
   natsci_model, natspacy_model, natenviy_model, nathealy_model,
   natcityy_model, natcrimy_model, natdrugy_model, nateducy_model, 
   natracey_model, natarmsy_model, nataidy_model, natfarey_model, 
   reliten_model, bible_model, tax_model, closeblk_model,
   closewht_model, happy_model, obey_model, 
   popular_model, thnkself_model, workhard_model, helpoth_model,
   joblose_model, jobfind_model, satjob_model, satfin_model,
   getahead_model, sexeduc_model, divlaw_model, pornlaw_model, 
   godgss_model, rellife_model, relpersn_model, sprtprsn_model,
   punsin_model, rotapple_model, blkwhite_model, permoral_model,
   marasian_model, marhisp_model, nextgen_model, toofast_model, 
   advfront_model, uswary_model,
   astrosci_model, scibnfts_model, parsol_model, kidssol_model,
   courts_model, letin1_model, life_model, g6, g8, g10)

