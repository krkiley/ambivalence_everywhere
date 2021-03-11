fmmNoStrong <- function(waves, covariates=NA, id=NA, data, n_chains = 5, iterations = 2500, 
                burn = 500, strong = FALSE, cov_estimator="multinom", var_name = NA) {
  
  if (is.na(id)) {
    data$id <- 1:nrow(data)
    id <- "id"
  }
  
  
  #Extract Y and covariates from the model
  if (is.na(covariates)) {
    Y=data %>% select(id, waves)
    names(Y) <- c("id", "y1", "y2", "y3")
    covariate_formula=as.formula("G ~ 1")
    Y=Y%>%
      filter(!is.na(y1) | !is.na(y2) | !is.na(y3)) 
    
  } else {
    Y=data %>% select(id, waves, covariates) 
    names(Y) <- c("id", "y1", "y2", "y3", covariates)
    covariate_formula=as.formula(paste("G ~ ", paste(covariates, collapse = " + ")))
    Y=Y%>%
      filter(!is.na(y1) | !is.na(y2) | !is.na(y3)) %>%
      filter_at(vars(-y1, -y2, -y3), all_vars(!is.na(.)))
    
    W=as.data.frame(Y[,5:ncol(Y)])
    W_add=as.data.frame(bind_rows(W, colMeans(W), colMeans(W), colMeans(W), colMeans(W),
                                  colMeans(W), colMeans(W)))
    
  }
  
  ID=Y[,1]
  Y=Y[,2:4]
  Ymiss <- rowSums(is.na(Y))>0
  
  count <- 0
  model_params <- vector(mode = "list", length = iterations*n_chains)
  pattern_params <- vector(mode = "list", length = iterations*n_chains)
  grp_predict <- vector(mode = "list", length = iterations*n_chains)
  for (chain in 1:n_chains) {
    #Randomly generate the parameters
    #from their respective prior distributions
    #print("generating starting parameters")
    alpha1=rbeta(1,1,1)
    delta1=rbeta(1,1,1)
    delta2=rbeta(1,1,1)
    tau3=rbeta(1,1,1)
    alpha3post=rbeta(1,1,1)
    delta3=rbeta(1,1,1)
    
    #Draw a vector psi for covariates
    #From a multivariate normal distribution
    if (!is.na(covariates)) {
      psi=mvrnorm(1, rep(0, 2*(1 + length(covariates))), 
                  1*diag(2*(1 + length(covariates))))
    } else {
      pis=rdirichlet(1, c(1,1,1))
      pi1i=pis[1]
      pi2i=pis[2]
      pi3i=pis[3]
    }
    
    two_params=rdirichlet(1, c(1,1,1))
    phi2=two_params[1]
    alpha2=two_params[2]
    
    three_params_1=rdirichlet(1, c(1,1,1))
    phi3pre1=three_params_1[1]
    alpha3pre1=three_params_1[2]
    
    three_params_2=rdirichlet(1,c(1,1,1))
    phi3pre2=three_params_2[1]
    alpha3pre2=three_params_2[2]
    
    #Randomly fill in missing data
    Y_star <- as.matrix(Y)

    Y_star[is.na(Y_star[,1]),1] <- sample(c(2,3,4), sum(is.na(Y_star[,1])), replace = TRUE)
    Y_star[is.na(Y_star[,2]),2] <- sample(c(2,3,4), sum(is.na(Y_star[,2])), replace = TRUE)
    Y_star[is.na(Y_star[,3]),3] <- sample(c(2,3,4), sum(is.na(Y_star[,3])), replace = TRUE)

    
    for (j in 1:iterations) {
      count <- count + 1
      
      #profvis({
      
      #Create parameter vectors
      Ai = ifelse(Y_star[,1] %in% c(4,5), 1, 0)
      Ci = ifelse(Y_star[,1] == 3, 1, 0) + ifelse(Y_star[,2] == 3, 1, 0) +
        ifelse(Y_star[,3] == 3, 1, 0)
      Di = ifelse(Y_star[,1] < 3 & Y_star[,2] >= 3, 1, 0) +
        ifelse(Y_star[,1] > 3 & Y_star[,2] <= 3, 1, 0) +
        ifelse(Y_star[,2] < 3 & Y_star[,3] >= 3, 1, 0) +
        ifelse(Y_star[,2] > 3 & Y_star[,3] <= 3, 1, 0) +
        ifelse(Y_star[,1] == 3 & Y_star[,2] != 3, 1, 0) +
        ifelse(Y_star[,2] == 3 & Y_star[,3] != 3, 1, 0)
      Ei = ifelse(Di != 1, 0, 
                  ifelse((Y_star[,1] < 3 & Y_star[,2] >= 3) | 
                           (Y_star[,1] == 3 & Y_star[,2] != 3) |
                           (Y_star[,1] > 3 & Y_star[,2] <= 3), 1, 2))
      Fi = ifelse(Di != 1 | (Ei == 1 & Y_star[,1] %in% c(1,2,3)) |
                    (Ei == 2 & Y_star[,2] %in% c(1,2,3)), 0, 1)
      Hi = ifelse(Di == 1 & Y_star[,1] == 3, 1, 0)
      Mi = ifelse(Di == 1 & Y_star[,3] %in% c(4,5), 1, 0)
      Qi = ifelse(Di == 1 & Y_star[,3] == 3, 1, 0)
      Ri = ifelse(Y_star[,1] %in% c(1,2), 1, 0) + ifelse(Y_star[,2] %in% c(1,2), 1, 0) +
        ifelse(Y_star[,3] %in% c(1,2), 1, 0) 
      
      #Do Groups Assignment based on randomly generated parameters
      #Estimate the probabilities of falling in group,
      #based on randomly generated parameters
      p1=(alpha1^(1-Ai)*(1-alpha1)^Ai)*ifelse(Ci==0,1,0)*ifelse(Di==0,1,0)
      p2=phi2^Ci*alpha2^Ri*(1-phi2-alpha2)^(3-Ci-Ri)
      p3=(tau3^ifelse(Ei==1,1,0)*
            (1-tau3)^ifelse(Ei==2,1,0)*
            (phi3pre1^Hi*
               alpha3pre1^((1-Fi)*(1-Hi))*
               (1-alpha3pre1-phi3pre1)^Fi)^ifelse(Ei==1,1,0)*
            (phi3pre2^Hi*
               alpha3pre2^((1-Fi)*(1-Hi))*
               (1-alpha3pre2-phi3pre2)^Fi)^ifelse(Ei==2,1,0)*
            ((1-alpha3post)^Mi*
               (alpha3post)^(1-Mi))^Hi)*
        ifelse(Di==1,1,0)*ifelse(Qi==0,1,0)
      
      
      #Estimate pi'is
      if (!is.na(covariates)) {
        s_21 <- as.matrix(cbind(1, W))%*%as.matrix(psi[1:(1+length(covariates))])
        s_23 <- as.matrix(cbind(1, W))%*%as.matrix(psi[((1+length(covariates))+1):length(psi)])
        
        pi1i <- exp(s_21)/(exp(0) + exp(s_21) + exp(s_23))
        pi2i <- exp(0)/(exp(0) + exp(s_21) + exp(s_23))
        pi3i <- exp(s_23)/(exp(0) + exp(s_21) + exp(s_23))
      } 
      
      pi1 <- mean(pi1i)
      pi2 <- mean(pi2i)
      pi3 <- mean(pi3i)
      
      #Group allocation, based on randomly generated 
      #parameters
      #Determine the probabilities of draws
      #print("estimating group membership")
      g1 = (pi1i*p1)/((pi1i*p1) + (pi2i*p2) + (pi3i*p3))
      g2 = (pi2i*p2)/((pi1i*p1) + (pi2i*p2) + (pi3i*p3))
      g3 = (pi3i*p3)/((pi1i*p1) + (pi2i*p2) + (pi3i*p3))
      
      #Sample from this distribution
      g_x <- cbind(g1,g2,g3)
      x <- runif(nrow(g_x))
      cumul.w <- g_x %*% upper.tri(diag(ncol(g_x)), diag = TRUE) / rowSums(g_x)
      G <- rowSums(x > cumul.w) + 1L
      
      g1=ifelse(G==1,1,0)
      g2=ifelse(G==2,1,0)
      g3=ifelse(G==3,1,0)
      
      #Sample parameters from their posterior distribution
      #print("drawing parameters")
      alpha1=rbeta(1, sum(g1)-sum(Ai*g1)+1, sum(Ai*g1)+1)
      two_params=rdirichlet(1, c((sum(Ci[g2==1])/3)+(2/3), (sum(Ri[g2==1])/3)+(2/3),
                                 sum(g2==1)-(sum(Ci[g2==1])/3)-(sum(Ri[g2==1])/3)+(2/3)))
      phi2=two_params[1]
      alpha2=two_params[2]
      three_params_1=rdirichlet(1,c(sum(Hi[g3==1&Ei==1])+(2/6),
                                    sum(g3[Ei==1])-sum(Fi[g3==1&Ei==1])-
                                      sum(Hi[g3==1&Ei==1])+(2/6),
                                    sum(Fi[g3==1&Ei==1])+(2/6)))
      phi3pre1=three_params_1[1]
      alpha3pre1=three_params_1[2]
      three_params_2=rdirichlet(1,c(sum(Hi[g3==1&Ei==2])+(2/6),
                                    sum(g3[Ei==2])-sum(Fi[g3==1&Ei==2])-
                                      sum(Hi[g3==1&Ei==2])+(2/6),
                                    sum(Fi[g3==1&Ei==2])+(2/6)))
      phi3pre2=three_params_2[1]
      alpha3pre2=three_params_2[2]
      
      alpha3post=rbeta(1,sum(g3[Hi==1])-sum(Mi[g3==1&Hi==1]) + (2/6),
                       sum(Mi[g3==1&Hi==1]) + (2/6))
      tau3=rbeta(1,sum(g3[Ei==1]) + 1,sum(g3[Ei==2]) + 1)
      
      delta1=delta2=delta3=0
      
      #Sample Gamma's from the multinomial regression output
      if (is.na(covariates)) {
        pis=rdirichlet(1,c(sum(g1)+2, sum(g2)+2, sum(g3)+2))
        pi1i=pis[1]
        pi2i=pis[2]
        pi3i=pis[3]
      } else {
        G_add <- data.frame(G = as.factor(c(G,1,1,2,2,3,3)))
        G_add$G <- relevel(G_add$G, ref = "2")
        df <- bind_cols(G_add, W_add) 
        
        if (cov_estimator=="multinom") {
          m1 <- multinom(covariate_formula, data = df, trace = FALSE,
                         Hess = TRUE)
          psi <- mvrnorm(1, mu = as.vector(t(coef(m1))), Sigma = unname(vcov(m1)))
        } else if (cov_estimator=="binom") {
          
          m1 <- glm(covariate_formula, family = binomial(link = "logit"), data = df %>%
                      filter(G%in%c(2,1)) %>% mutate(G=ifelse(G==1,1,0)))
          m2 <- glm(covariate_formula, family = binomial(link = "logit"), data = df %>%
                      filter(G%in%c(2,3)) %>% mutate(G=ifelse(G==3,1,0)))
          psi <- c(mvrnorm(1, mu = as.vector(t(coef(m1))), Sigma = unname(vcov(m1))), 
                   mvrnorm(1, mu = as.vector(t(coef(m2))), Sigma = unname(vcov(m2))))
        }
        
        rm(df, m1)
        
      }
      
      #Re-estimate missing values (crazy logic)
      #print("drawing missing values")
      GY <- cbind(G, Y)
      Y_star[Ymiss,] <- t(apply(GY[Ymiss,], 1, logicThree, alpha1, 
                                phi2, alpha2, 
                                phi3pre1, phi3pre2, alpha3pre1, alpha3pre2, 
                                alpha3post, tau3))
      
      #}, interval = .005)
      
      if (!is.na(covariates)) {
        model_params[[count]]=data.frame(names = c(paste(c("Intercept", covariates), 
                                                         "_21", sep = ""), 
                                                   paste(c("Intercept", covariates), "_23", sep = "")),
                                         val = psi) %>% spread(names, val) %>% mutate(it=j, chain=chain)
        
      }
      
      #Calculate average within person variance:
      sd <- sqrt(rowSums((Y_star - rowMeans(Y_star))^2)/2)
      sd_g1 = mean(sd[G == 1])
      sd_g2 = mean(sd[G == 2])
      sd_g3 = mean(sd[G == 3])
      
      #Store parameter estimates in a vector/data frame
      pattern_params[[count]]=data.frame(it=j, chain=chain, pi1=pi1, pi2=pi2, pi3=pi3, 
                                         alpha1=alpha1, 
                                         phi2=phi2, alpha2=alpha2, 
                                         phi3pre1=phi3pre1, phi3pre2=phi3pre2,
                                         alpha3pre1=alpha3pre1, alpha3pre2=alpha3pre2, 
                                         alpha3post=alpha3post, 
                                         tau3=tau3, 
                                         llike=sum(log10(pi1i^g1) + log10(p1^g1) + 
                                                     log10(pi2i^g2) + log10(p2^g2) + 
                                                     log10(pi3i^g3) + log10(p3^g3)),
                                         sd_g1=sd_g1,sd_g2=sd_g2,sd_g3=sd_g3)
      if (j > burn) {
        grp_predict[[count]] <- G
      }
      
      print(paste(chain, j, sep = " "))
    }
    
    
  }
  pattern_params=bind_rows(pattern_params)
  pattern_param_summary=pattern_params %>%
    filter(it > burn) %>% select(-c(it, chain)) %>%
    gather(key = "param", value = "value") %>%
    group_by(param) %>%
    summarise(mean = mean(value, na.rm = TRUE), 
              q25=quantile(value, .025, na.rm = TRUE), 
              q975=quantile(value, .975, na.rm = TRUE))
  
  suppressMessages({
    grp_predict=bind_cols(grp_predict)
    prob_1 = rowMeans(grp_predict==1)
    prob_2 = rowMeans(grp_predict==2)
    prob_3 = rowMeans(grp_predict==3)
    if (is.na(covariates)) {
      grp_predict_summary=bind_cols(ID, Y, prob1=prob_1, prob2=prob_2, prob3=prob_3)
    } else {
      grp_predict_summary=bind_cols(ID, Y, W, prob1=prob_1, prob2=prob_2, prob3=prob_3)
    }
  })
  
  model_info = list(var = var_name, qtype="3", n_chains = n_chains, iterations = iterations, 
                    covariates = covariates, burn = burn, cov_estimator=cov_estimator)
  
  if (!is.na(covariates)) {
    model_params=bind_rows(model_params)
    model_param_summary=model_params %>%
      filter(it > burn) %>% select(-c(it, chain)) %>%
      gather(key = "param", value = "value") %>%
      group_by(param) %>%
      summarise(mean = mean(value), q25=quantile(value, .025), q975=quantile(value, .975))
    return(list(model_info=model_info, model_param_summary=model_param_summary, 
                pattern_param_summary=pattern_param_summary,
                model_params=model_params,
                pattern_params=pattern_params, grp_predict_summary=grp_predict_summary))
    
  } else {
    return(list(model_info=model_info, model_param_summary=NA,
                pattern_param_summary=pattern_param_summary,
                model_params=NA,
                pattern_params=pattern_params, grp_predict_summary=grp_predict_summary))
    
  }
}



logicThree <- function(vec, alpha1, phi2, alpha2, 
                           phi3pre1, phi3pre2, alpha3pre1, alpha3pre2, 
                           alpha3post, tau3) {
  g <- vec[1]
  y <- vec[2:4]
  if (g==1) {
    if (y[!is.na(y)][1] < 3) {
      y[is.na(y)] <- 2
    } else {
      y[is.na(y)] <- 4
    }
  } else if (g==2) {
    y[is.na(y)] <- 
      sample(2:4, sum(is.na(y)), replace=T, 
             prob=c(alpha2,phi2,(1-alpha2)))
  } else if (g==3) {
    if (sum(is.na(y)==c(0,1,1))==3) {
      if (rbinom(1,1,tau3)) {
        if (y[1]==3) {
          if (rbinom(1,1,alpha3post)) {
            y[c(2,3)] <- 2
          } else {
            y[c(2,3)] <- 4
          }
        } else if (y[1] %in% c(1,2)) {
          y[c(2,3)] <- 4
        } else {
          y[c(2,3)] <- 2
        }
      } else {
        if (y[1]==3) {
          y[c(2,3)] <- c(3,sample(c(2,4),1,
                                  prob=c(alpha3post,(1-alpha3post))))
        } else if (y[1] %in% c(1,2)) {
          y[c(2,3)] <- c(2,4)
        } else {
          y[c(2,3)] <- c(4,2)
        }
      }
    } else if (sum(is.na(y)==c(0,0,1))==3) {
      if (y[1]==3&y[2]==3) {
        y[3] <- sample(c(2,4),1,
                       prob=c(alpha3post,(1-alpha3post)))
      } else if ((y[1]%in%c(1,2,3)&y[2]%in%c(4,5)) | 
                 (y[1]%in%c(1,2)&y[2]%in%c(1,2))) {
        y[3] <- 4
      } else {
        y[3] <- 2
      } 
    } else if (sum(is.na(y)==c(0,1,0))==3) {
      if (rbinom(1,1,tau3)) {
        if (y[3] %in% c(4,5)) {
          y[2] <- 4
        } else {
          y[2] <- 2
        }
      } else {
        if (y[1]==3) {
          y[2] <- 3
        } else if (y[3] %in% c(4,5)) {
          y[2] <- 2
        } else {
          y[2] <- 4
        }
      }
    } else if (sum(is.na(y)==c(1,0,1))==3) {
      if (y[2]==3) {
        y[1] <- 3
        y[3] <- sample(c(2,4),1,
                       prob = c(alpha3post,(1-alpha3post)))
      } else {
        if (rbinom(1,1,tau3)) {
          if (y[2]%in%c(1,2)) {
            y[c(1,3)] <- c(sample(c(3,4),1,prob=c(phi3pre1,(1-phi3pre1))),
                           2)
          } else {
            y[c(1,3)] <- c(sample(c(3,2),1,prob=c(phi3pre1,(1-phi3pre1))),
                           4)
          }
        } else {
          if (y[2]%in%c(1,2)) {
            y[c(1,3)] <- c(2,4)
          } else {
            y[c(1,3)] <- c(4,2)
          }
        }
      }
    } else if (sum(is.na(y)==c(1,0,0))==3) {
      if (y[2]==3) {
        y[1] <- 3
      } else if (y[2] %in% c(1,2) & y[3] %in% c(1,2)) {
        y[1] <- sample(c(3,4),1,
                       prob=c(phi3pre1,(1-phi3pre1)))
      } else if (y[2] %in% c(1,2) & y[3] %in% c(4,5)) {
        y[1] <- 2
      } else if (y[2] %in% c(4,5) & y[3] %in% c(1,2)) {
        y[1] <- 4
      } else if (y[2] %in% c(4,5) & y[3] %in% c(4,5)) {
        y[1] <- 
          sample(c(3,2),1,prob=c(phi3pre1,(1-phi3pre1)))
      }
    } else if (sum(is.na(y)==c(1,1,0))==3) {
      if (rbinom(1,1,tau3)) {
        if (y[3]%in%c(1,2)) {
          y[1] <- sample(c(3,4),1,prob=c(phi3pre1,(1-phi3pre1)))
          y[2] <- 2
        } else {
          y[1] <- sample(c(3,2),1,prob=c(phi3pre1,(1-phi3pre1)))
          y[2] <- 4
        }
      } else {
        if (rbinom(1,1,phi3pre2)) {
          y[1] <- 3
          y[2] <- 3
        } else {
          if (y[3]%in%c(1,2)) {
            y[1] <- 4
            y[2] <- 4
          } else {
            y[1] <- 2
            y[2] <- 2
          }
        }
      }
    }
  }
  return(as.numeric(c(y[1], y[2], y[3])))
}









