bind_rows(a5res, a7res, a9res, a2kres, gssres, nsyrres, lsgres, 
          #bes2res, bes9res,
          safcres, safmres) %>%
  filter(var %!in% c("wallacelibcon7",
                     "demslibcon7", "repslibcon7", "jobguardem7", "jobguarrep7",
                     "unrestdem7", "unrestrep7", "blksprtdem7", 
                     "blksprtrep7", "busingdem7", "busingrep7",
                     "accuseddem7", "accusedrep7",
                     "partyid7", "libcon7", "clintspend", "repspend", "demspend", 
                     "replibcon", "demlibcon", "uklabspendserv", 
                     "labtxs")) %>%
  
  filter(param %in% c("delta1", "delta2")) %>%
  select(param, mean, var, qtype, ds) %>%
  spread(param, mean) %>%
  mutate(name = ifelse(delta2 > delta1, var, NA),
         diff = delta1 - delta2) %>%
  arrange(diff)


#Make stability correlation

ids <- data.frame(id = c(paste(g6$id_1, "06", sep = "-"),
                         paste(g8$id_1, "08", sep = "-"),
                         paste(g10$id_1, "10", sep = "-")))


for (i in 1:length(gss_results)) {
  
  var <- gss_results[[i]]$model_info$var
  df <- gss_results[[i]]$grp_predict_summary %>%
    select(id, prob1)
  names(df) <- c("id", var)
  
  
  if (i == 1) {
    gss_stable_results <- left_join(ids, df)
  } else {
    gss_stable_results <- left_join(gss_stable_results, df, by = c("id"="id"))
  }
  
}

gss_stable_results <- gss_stable_results %>%
  mutate(natspac = ifelse(is.na(natspac), natspacy, natspac),
         natenvir = ifelse(is.na(natenvir), natenviy, natenvir),
         natheal = ifelse(is.na(natheal), nathealy, natheal),
         natcity = ifelse(is.na(natcity), natcityy, natcity),
         natcrime = ifelse(is.na(natcrime), natcrimy, natcrime),
         natdrug = ifelse(is.na(natdrug), natdrugy, natdrug),
         nateduc = ifelse(is.na(nateduc), nateducy, nateduc),
         natrace = ifelse(is.na(natrace), natracey, natrace),
         natarms = ifelse(is.na(natarms), natarmsy, natarms),
         nataid = ifelse(is.na(nataid), nataidy, nataid),
         natfare = ifelse(is.na(natfare), natfarey, natfare)) %>%
  select(-c(natspacy, natenviy, nathealy, natcityy, natdrugy, 
         nateducy, natracey, natarmsy, nataidy, natfarey,
         natcrimy))




stab.cors <- cor(gss_stable_results[,2:ncol(gss_stable_results)], use = "pairwise.complete.obs")


stab.cors[abs(stab.cors) < .2] <- 0
stab.cors[abs(stab.cors) > .2] <- 1

sc2 <- stab.cors[rowSums(stab.cors, na.rm = TRUE) > 1, colSums(stab.cors, na.rm = TRUE) > 1]

stab.g <- graph_from_adjacency_matrix(sc2, mode = "undirected", 
                                      diag = FALSE)
plot(stab.g, 
     vertex.size = 5,
     vertex.color = "gray",
     vertex.label.cex = .7,
     vertex.label.color = "black")


gss_stable_results[gss_stable_results > 1]





anes90results[[1]] 



