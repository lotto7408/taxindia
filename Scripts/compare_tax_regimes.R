
#------------------------------
# How to Select Your Tax regime
#-------------------------------
# If you have a homeloan, trivially you should opt for the old 
# Generally the decison becomes difficult when you don't have 
# anything other than 80c to exempt tax. 
# In that case the decision solely depends on how much rent you pay
#
# This code creates a plot with in hand salary varying with monthly rent
# -----------------------------------------------------------------------


modules <- c("tidyverse", "ggsci")
lapply(modules, library, character.only = TRUE)



source("./Functions/monthly_inhand_new_2020.R")
source("./Functions/monthly_inhand_new_2023.R")
source("./Functions/monthly_inhand_old.R")


# Assumptions:
basic <- 6.78e5
base <- 17e5
hra <- 2.71e5

# basic <- 11.9e5
# base <- 27.67e5
# hra <- 1.19e5

# basic <- 15.48e5
# base <- 38.72e5
# hra <- 7.74e5

tidyr::expand_grid(base = base, basic = basic, hra = hra, 
                   rent_monthly = seq(0, 40e3, 2e3), NPS = c(TRUE, FALSE)) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(rent = rent_monthly *12,
                  old_with_NPS = monthly_inhand_old(base = base, 
                                                    basic = basic, 
                                                    hra = hra, 
                                                    rent_yearly = rent, 
                                                    NPS = TRUE)$pay,
                  old_without_NPS = monthly_inhand_old(base = base, 
                                                       basic = basic, 
                                                       hra = hra, 
                                                       rent_yearly = rent, 
                                                       NPS = FALSE)$pay,
                  new_2020 = monthly_inhand_new_2020(base = base, 
                                                     basic = basic)$pay,
                  new_2023 = monthly_inhand_new_2023(base = base, 
                                                     basic = basic)$pay) %>% 
    tidyr::pivot_longer(cols = starts_with(c("new", "old")), 
                        names_to = "Regime", values_to = "Salary") %>%
    ggplot(aes(x = rent_monthly, y = Salary, group = Regime, col = Regime)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1.4) +
    scale_x_continuous(breaks=seq(0, 40000, 4e3)) +
    scale_y_continuous(breaks=seq(90e3, 300e3, 1e3)) +
    labs(title = "Monthly Rent to Determine Old vs New Regime",
         subtitle = paste("Basic: ", basic, ",  Base: ", base, ",  HRA: ", hra),
         x = "Monthly Rent") +
    theme_bw() +
    scale_color_lancet()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle =  element_text(hjust = 0.5),
          text = element_text(size = 15))
