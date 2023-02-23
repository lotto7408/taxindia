



#------------------------------
# To Compare how Rate of growth dictates the Trajectory of Corpus
#-------------------------------

modules <- c("tidyverse", "ggsci")
lapply(modules, library, character.only = TRUE)



source("./Functions/sip_calculator.R")

sip_amount = 3.7e3
tenure_in_years = 25
expected_return = seq(0, 16, 4)
final_corpus <- sip_calculator(sip_amount, tenure_in_years, expected_return)

data <- data.frame(sip_amount, 
                   tenure_in_years, 
                   expected_return) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(final_corpus = sip_calculator(sip_amount = sip_amount, 
                                                tenure_in_years = tenure_in_years, 
                                                expected_return = expected_return)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(final_corpus = paste0(round(final_corpus/1e5, 4), " lacs"),
                  total_invested = final_corpus[expected_return==0])

























