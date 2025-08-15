
combine_topics <- 
  function(optimal_K, labelled_lda_models, constrained_topics_gamma, constrained_topics_beta){
    
    # gamma from the non-Lactobacillus counts
    gamma <- labelled_lda_models[[optimal_K]]$gamma # gamma from the optimal K
    gamma <- gamma[match(rownames(constrained_topics_gamma), rownames(gamma)),] # re-ordering samples so that they match the Lacto gammas
    gamma[is.na(gamma)] <- 0 # replacing NAs with 0s (can happen if a sample is 100% Lactobacillus)
    gamma <- gamma * (1-rowSums(constrained_topics_gamma)) # scaling the non-Lacto gamma so that it sums to 1 with the Lacto gamma
    
    # beta from the non-Lactobacillus counts
    beta <- labelled_lda_models[[optimal_K]]$beta
    beta <- beta[, match(rownames(constrained_topics_beta), colnames(beta))] # re-ordering taxa so that they match the Lacto betas
    colnames(beta) <- rownames(constrained_topics_beta) # re-naming the non-Lacto beta columns so that they have the "Lacto" taxa
    beta[is.na(beta)] <- -Inf # replacing NAs with -Inf (for the Lacto taxa, which are not present in the non-Lacto counts)
    
    
    # Re-ordering the topics by prevalence
    j <- colSums(gamma) |> order(decreasing = TRUE) 
    gamma <- gamma[, j]
    beta <- beta[j, ]
    
    # combining with gammas and betas
    gamma <- cbind(constrained_topics_gamma, gamma)
    beta <- rbind(constrained_topics_beta %>% t() %>% log(), beta)

    list(gamma = gamma, beta = beta)
  }
