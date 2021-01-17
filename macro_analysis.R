qm <- read_csv("~/Dropbox/hill_kreisi/questionmacro.csv") %>%
  mutate(gss = ifelse(ds == "gss", 1, 0),
         anes2k = ifelse(ds == "anes2k", 1, 0),
         anes5 = ifelse(ds == "anes5", 1, 0),
         anes7 = ifelse(ds == "anes7", 1, 0),
         anes9 = ifelse(ds == "anes9", 1, 0),
         nsyr = ifelse(ds == "nsyr", 1, 0),
         lsg = ifelse(ds == "lsg", 1, 0),
         safc = ifelse(ds == "saf-c", 1, 0),
         safm = ifelse(ds == "saf-m", 1, 0),
         cces = ifelse(ds == "cces", 1, 0),
         choice2 = ifelse(toptions == 2, 1, 0),
         choice3 = ifelse(toptions == 3, 1, 0),
         choice4 = ifelse(toptions == 4, 1, 0),
         choice5 = ifelse(toptions == 5, 1, 0),
         choice7 = ifelse(toptions %in% c(6,7), 1, 0)) %>%
  select(-c(text, qtype, midpoint, recode_q))

test <- results_df %>%
  left_join(qm, by = c("var"="var_name")) %>%
  filter(param == "pi2") %>%
  filter(var != "runforall7") 

m1 <- lm(mean ~ ft + infl + other + bipolar +
           civlibs + medsci + race + famgndr + 
           sex + ses + moral + 
           work + foreign + self + religion + crime + trust + 
           other_assess + not_politics + 
           group + person + 
           choice2 + choice3 + choice4 + choice7 + 
           anes5 + anes7 + anes9 + anes2k + 
           lsg + nsyr + safm + safc + cces, data = test)


new.data <- data.frame(ft = 0, infl = 0, other = 0, bipolar = 0, civlibs = 0, medsci = 0,
                       race = 0, famgndr = 0, sex = 0, ses = 0, moral = 0, work = 0, 
                       foreign = 0, self = 0, religion = 0, crime = 0, 
                       trust = 0, other_assess = 0, not_politics = 0, 
                       group = 0, person = 0, toptions = 5,
                       ds.x = as.factor("gss"))
predict(m1, test[3,], se.fit = TRUE)




qm %>% filter(religion == 1) %>% View()



test$resid <- m1$residuals
test$yhat <- m1$fitted.values

test %>% arrange(desc(abs(resid))) %>% View()

ggplot(test, aes(x = yhat, y = resid, fill = ds.x)) + 
  geom_point(shape = 21) 
  

test %>%
  arrange(desc(abs(resid))) %>% View()

reg_results <- tidy(m1)

reg_results %>%
  filter(term != "(Intercept)") %>%
  mutate(group = ifelse(term %in% c("as.factor(toptions)2", "as.factor(toptions)3", 
                                    "as.factor(toptions)4", "as.factor(toptions)7",
                                    "ft", "infl", "other", "bipolar", "ds.xanes5660",
                                    "ds.xanes7276", "ds.xanes9297", "ds.xgss",
                                    "ds.xlsg", "ds.xnsyr", "ds.xsaf-c", 
                                    "ds.xsaf-m"), "structure",
                        "content")) %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate,
             fill = group)) + 
  geom_hline(yintercept = 0) + 
  geom_linerange(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) + 
  geom_point(shape = 21) + 
  facet_wrap(~group, scales = "free_y") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")





coefs <- vector(mode = "list", length = 100)
for (j in 1:100) {
  pi2_params <- vector(mode = "list", length = 509)
  count <- 0
  for (i in 1:length(anes70_results)) {
    count <- count + 1
    var <- anes70_results[[i]]$model_info$var
    t <- anes70_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(anes50_results)) {
    count <- count + 1
    var <- anes50_results[[i]]$model_info$var
    t <- anes50_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(anes90results)) {
    count <- count + 1
    var <- anes90results[[i]]$model_info$var
    t <- anes90results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(anes2kresults)) {
    count <- count + 1
    var <- anes2kresults[[i]]$model_info$var
    t <- anes2kresults[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(gss_results)) {
    count <- count + 1
    var <- gss_results[[i]]$model_info$var
    t <- gss_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(lsg_results)) {
    count <- count + 1
    var <- lsg_results[[i]]$model_info$var
    t <- lsg_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(nsyr_results)) {
    count <- count + 1
    var <- nsyr_results[[i]]$model_info$var
    t <- nsyr_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(safc_results)) {
    count <- count + 1
    var <- safc_results[[i]]$model_info$var
    t <- safc_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(safm_results)) {
    count <- count + 1
    var <- safm_results[[i]]$model_info$var
    t <- safm_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  for (i in 1:length(cces_results)) {
    count <- count + 1
    var <- cces_results[[i]]$model_info$var
    t <- cces_results[[i]]$pattern_params %>% filter(it > 500)
    pi2 <- sample(t$pi2, 1)
    pi2_params[[count]] <- data.frame(var_name = var, pi2 = pi2)
  }
  
  df <- bind_rows(pi2_params) %>% left_join(qm)
  coefs[[j]] <- tidy(lm(pi2 ~ ft + infl + other + bipolar +
                          civlibs + medsci + race + famgndr + 
                          sex + ses + moral + 
                          work + foreign + self + religion + crime + trust + 
                          other_assess + not_politics + 
                          group + person + 
                          choice2 + choice3 + choice4 + choice7 + 
                          anes5 + anes7 + anes9 + anes2k + 
                          lsg + nsyr + safm + safc, data = df)) %>%
    select(term, estimate) %>%
    spread(term, estimate)
  
} 

bind_rows(coefs) %>% gather(key = "param", value = "value") %>%
  group_by(param) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            q25=quantile(value, .025, na.rm = TRUE), 
            q975=quantile(value, .975, na.rm = TRUE))   %>%
  mutate(group = ifelse(param %in% c("choice2", "choice3", "choice4", "choice7",
                                    "ft", "infl", "other", "bipolar", "anes5",
                                    "anes7", "anes9", "anes2k", "lsg",
                                    "nsyr", "safc", "safm"), "structure",
                        "content")) %>%
  filter(param != "(Intercept)") %>%
  ggplot(aes(x = reorder(param, mean), y = mean,
             fill = group)) + 
  geom_hline(yintercept = 0) + 
  geom_linerange(aes(ymin = q25, ymax = q975)) + 
  geom_point(shape = 21) + 
  facet_wrap(~group, scales = "free_y") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")



pi3_coefs <- vector(mode = "list", length = 100)
for (j in 1:100) {
  pi3_params <- vector(mode = "list", length = 509)
  count <- 0
  for (i in 1:length(anes70_results)) {
    count <- count + 1
    var <- anes70_results[[i]]$model_info$var
    t <- anes70_results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(anes50_results)) {
    count <- count + 1
    var <- anes50_results[[i]]$model_info$var
    t <- anes50_results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(anes90results)) {
    count <- count + 1
    var <- anes90results[[i]]$model_info$var
    t <- anes90results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(anes2kresults)) {
    count <- count + 1
    var <- anes2kresults[[i]]$model_info$var
    t <- anes2kresults[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(gss_results)) {
    count <- count + 1
    var <- gss_results[[i]]$model_info$var
    t <- gss_results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(lsg_results)) {
    count <- count + 1
    var <- lsg_results[[i]]$model_info$var
    t <- lsg_results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(nsyr_results)) {
    count <- count + 1
    var <- nsyr_results[[i]]$model_info$var
    t <- nsyr_results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(safc_results)) {
    count <- count + 1
    var <- safc_results[[i]]$model_info$var
    t <- safc_results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  for (i in 1:length(safm_results)) {
    count <- count + 1
    var <- safm_results[[i]]$model_info$var
    t <- safm_results[[i]]$pattern_params %>% filter(it > 500)
    pi3 <- sample(t$pi3, 1)
    pi3_params[[count]] <- data.frame(var_name = var, pi3 = pi3)
  }
  
  df <- bind_rows(pi3_params) %>% left_join(qm)
  pi3_coefs[[j]] <- tidy(lm(pi3 ~ ft + infl + other + bipolar +
                          civlibs + medsci + race + famgndr + 
                          sex + ses + moral + 
                          work + foreign + self + religion + crime + trust + 
                          other_assess + not_politics + 
                          group + person + 
                          choice2 + choice3 + choice4 + choice7 + 
                          anes5 + anes7 + anes9 + anes2k + 
                          lsg + nsyr + safm + safc, data = df)) %>%
    select(term, estimate) %>%
    spread(term, estimate)
  
} 

bind_rows(pi3_coefs) %>% gather(key = "param", value = "value") %>%
  group_by(param) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            q25=quantile(value, .025, na.rm = TRUE), 
            q975=quantile(value, .975, na.rm = TRUE))   %>%
  mutate(group = ifelse(param %in% c("choice2", "choice3", "choice4", "choice7",
                                     "ft", "infl", "other", "bipolar", "anes5",
                                     "anes7", "anes9", "anes2k", "lsg",
                                     "nsyr", "safc", "safm"), "structure",
                        "content")) %>%
  filter(param != "(Intercept)") %>%
  ggplot(aes(x = reorder(param, mean), y = mean,
             fill = group)) + 
  geom_hline(yintercept = 0) + 
  geom_linerange(aes(ymin = q25, ymax = q975)) + 
  geom_point(shape = 21) + 
  facet_wrap(~group, scales = "free_y") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")

pi2s <- results_df %>% filter(param == "pi2")
pi2s <- left_join(pi2s, qm, by = c("var"="var_name"))

boot_coefs <- vector(mode = "list", length = 10000)
for (i in 1:10000) {
  temp <- pi2s[sample(1:nrow(pi2s), replace = TRUE),]
  boot_coefs[[i]] <- tidy(lm(mean ~ ft + infl + other + bipolar +
                              civlibs + medsci + race + famgndr + 
                              sex + ses + moral + 
                              work + foreign + self + religion + crime + trust + 
                              other_assess + not_politics + oth_content + 
                              group + person + 
                              choice2 + choice3 + choice4 + choice7 + 
                              anes5 + anes7 + anes9 + anes2k + 
                              lsg + nsyr + safm + safc + cces, data = temp)) %>%
    select(term, estimate) %>%
    spread(term, estimate)
  
}

pi2_boot <- bind_rows(boot_coefs) 

save(pi2_boot, file = "~/Dropbox/hill_kreisi/results/pi2_boot.Rdata")


pi2_boot %>%
  gather(key = "param", value = "value") %>%
  group_by(param) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            q25=quantile(value, .025, na.rm = TRUE), 
            q975=quantile(value, .975, na.rm = TRUE)) %>%
  mutate(group = ifelse(param %in% c("choice2", "choice3", "choice4", "choice7",
                                     "ft", "infl", "other", "bipolar", "anes5",
                                     "anes7", "anes9", "anes2k", "lsg",
                                     "nsyr", "safc", "safm", "cces"), "Question Structure",
                        "Question Content")) %>%
  filter(param != "(Intercept)") %>%
  mutate(param = recode(param, "other_assess"="Asses another", "civlibs"="Civil liberties",
                        "moral"="Morality", "foreign"="Foreign policy/military",
                        "not_politics"="Not politics", "group"="Subject: Group",
                        "medsci"="Medicine/science", "ses"="SES", "famgndr"="Family/gender",
                        "work"="Work", "race"="Race", "person"="Subject: Person",
                        "crime"="Crime", "trust"="Social Trust", "self"="Self-eval.",
                        "religion"="Religion", "sex"="Sex/sexuality",
                        "oth_content"="Other content",
                        "other"="Structure: Other", "bipolar"="Structure: Bipolar",
                        "ft"="Structure: Feeling therm.", "infl"="Structure: Influence",
                        "choice7"="Choices: 7", "choice4"="Choices: 4", "choice3"="Choices: 3",
                        "choice2"="Choices: 2",
                        "gss"="GSS (2006-2014)", "safm"="ISPC-Mom (1980-1993)",
                        "safc"="ISPC-Child (1980-1993)", "lsg"="LSG (varies)", 
                        "nsyr"="NSYR (varies)", "anes7"="ANES (1972-1976)",
                        "anes5"="ANES (1956-1960)", "anes9"="ANES (1992-1996)",
                        "anes2k"="ANES (2000-2004)", "cces"="CCES (2010-2014)")) %>%
  ggplot(aes(x = reorder(param, mean), y = mean,
             fill = group)) + 
  geom_hline(yintercept = 0) + 
  geom_linerange(aes(ymin = q25, ymax = q975)) + 
  geom_point(shape = 21) + 
  facet_wrap(~group, scales = "free_y") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none") + 
  labs(x = "", y = "Coefficient estimate")

save(pi2_boot, file = "~/Dropbox/hill_kreisi/results/pi2_boot.Rdata")


pi3s <- results_df %>% filter(param == "pi3")
pi3s <- left_join(pi3s, qm, by = c("var"="var_name"))

boot_coefs <- vector(mode = "list", length = 10000)
for (i in 1:10000) {
  temp <- pi3s[sample(1:nrow(pi3s), replace = TRUE),]
  boot_coefs[[i]] <- tidy(lm(mean ~ ft + infl + other + bipolar +
                               civlibs + medsci + race + famgndr + 
                               sex + ses + moral + 
                               work + foreign + self + religion + crime + trust + 
                               other_assess + not_politics + oth_content + 
                               group + person + 
                               choice2 + choice3 + choice4 + choice7 + 
                               anes5 + anes7 + anes9 + anes2k + 
                               lsg + nsyr + safm + safc + cces, data = temp)) %>%
    select(term, estimate) %>%
    spread(term, estimate)
  
}
pi3_boot <- bind_rows(boot_coefs) 

save(pi3_boot, file = "~/Dropbox/hill_kreisi/results/pi3_boot.Rdata")
pi3_boot %>%
  gather(key = "param", value = "value") %>%
  group_by(param) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            q25=quantile(value, .025, na.rm = TRUE), 
            q975=quantile(value, .975, na.rm = TRUE)) %>%
  mutate(group = ifelse(param %in% c("choice2", "choice3", "choice4", "choice7",
                                     "ft", "infl", "other", "bipolar", "anes5",
                                     "anes7", "anes9", "anes2k", "lsg",
                                     "nsyr", "safc", "safm", "cces"), "Question Structure",
                        "Question Content")) %>%
  filter(param != "(Intercept)") %>%
  mutate(param = recode(param, "other_assess"="Asses another", "civlibs"="Civil liberties",
                        "moral"="Morality", "foreign"="Foreign policy/military",
                        "not_politics"="Not politics", "group"="Subject: Group",
                        "medsci"="Medicine/science", "ses"="SES", "famgndr"="Family/gender",
                        "work"="Work", "race"="Race", "person"="Subject: Person",
                        "crime"="Crime", "trust"="Social Trust", "self"="Self-eval.",
                        "religion"="Religion", "sex"="Sex/sexuality",
                        "oth_content"="Other content",
                        "other"="Structure: Other", "bipolar"="Structure: Bipolar",
                        "ft"="Structure: Feeling therm.", "infl"="Structure: Influence",
                        "choice7"="Choices: 7", "choice4"="Choices: 4", "choice3"="Choices: 3",
                        "choice2"="Choices: 2",
                        "gss"="GSS (2006-2014)", "safm"="ISPC-Mom (1980-1993)",
                        "safc"="ISPC-Child (1980-1993)", "lsg"="LSG (varies)", 
                        "nsyr"="NSYR (varies)", "anes7"="ANES (1972-1976)",
                        "anes5"="ANES (1956-1960)", "anes9"="ANES (1992-1996)",
                        "anes2k"="ANES (2000-2004)", "cces"="CCES (2010-2014)")) %>%
  ggplot(aes(x = reorder(param, mean), y = mean,
             fill = group)) + 
  geom_hline(yintercept = 0) + 
  geom_linerange(aes(ymin = q25, ymax = q975)) + 
  geom_point(shape = 21) + 
  facet_wrap(~group, scales = "free_y") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none") + 
  labs(x = "", y = "Coefficient estimate")




pi1s <- results_df %>% filter(param == "pi1")
pi1s <- left_join(pi1s, qm, by = c("var"="var_name"))

boot_coefs <- vector(mode = "list", length = 1000)
for (i in 1:1000) {
  temp <- pi1s[sample(1:nrow(pi1s), replace = TRUE),]
  boot_coefs[[i]] <- tidy(lm(mean ~ ft + infl + other + bipolar +
                               civlibs + medsci + race + famgndr + 
                               sex + ses + moral + 
                               work + foreign + self + religion + crime + trust + 
                               other_assess + not_politics + oth_content + 
                               group + person + 
                               choice2 + choice3 + choice4 + choice7 + 
                               anes5 + anes7 + anes9 + anes2k + 
                               lsg + nsyr + safm + safc + cces, data = temp)) %>%
    select(term, estimate) %>%
    spread(term, estimate)
  
}
pi1_boot <- bind_rows(boot_coefs) 

save(pi1_boot, file = "~/Dropbox/hill_kreisi/results/pi1_boot.Rdata")
pi1_boot %>%
  gather(key = "param", value = "value") %>%
  group_by(param) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            q25=quantile(value, .025, na.rm = TRUE), 
            q975=quantile(value, .975, na.rm = TRUE)) %>%
  mutate(group = ifelse(param %in% c("choice2", "choice3", "choice4", "choice7",
                                     "ft", "infl", "other", "bipolar", "anes5",
                                     "anes7", "anes9", "anes2k", "lsg",
                                     "nsyr", "safc", "safm", "cces"), "Question Structure",
                        "Question Content")) %>%
  filter(param != "(Intercept)") %>%
  mutate(param = recode(param, "other_assess"="Asses another", "civlibs"="Civil liberties",
                        "moral"="Morality", "foreign"="Foreign policy/military",
                        "not_politics"="Not politics", "group"="Subject: Group",
                        "medsci"="Medicine/science", "ses"="SES", "famgndr"="Family/gender",
                        "work"="Work", "race"="Race", "person"="Subject: Person",
                        "crime"="Crime", "trust"="Social Trust", "self"="Self-eval.",
                        "religion"="Religion", "sex"="Sex/sexuality",
                        "oth_content"="Other content",
                        "other"="Structure: Other", "bipolar"="Structure: Bipolar",
                        "ft"="Structure: Feeling therm.", "infl"="Structure: Influence",
                        "choice7"="Choices: 7", "choice4"="Choices: 4", "choice3"="Choices: 3",
                        "choice2"="Choices: 2",
                        "gss"="GSS (2006-2014)", "safm"="ISPC-Mom (1980-1993)",
                        "safc"="ISPC-Child (1980-1993)", "lsg"="LSG (varies)", 
                        "nsyr"="NSYR (varies)", "anes7"="ANES (1972-1976)",
                        "anes5"="ANES (1956-1960)", "anes9"="ANES (1992-1996)",
                        "anes2k"="ANES (2000-2004)", "cces"="CCES (2010-2014)")) %>%
  ggplot(aes(x = reorder(param, mean), y = mean,
             fill = group)) + 
  geom_hline(yintercept = 0) + 
  geom_linerange(aes(ymin = q25, ymax = q975)) + 
  geom_point(shape = 21) + 
  facet_wrap(~group, scales = "free_y") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none") + 
  labs(x = "", y = "Coefficient estimate")

