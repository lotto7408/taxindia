


hrabasic <- 10e5
base <- 23e5
hra <- 1e5


data <- expand_grid(rent = seq(0, 480000, 12000),
                    component = c("hra", "metro", "rent_on_top")) %>% 
    dplyr::mutate(exempt = ifelse(component == "hra",
                                  hra,
                                  ifelse(component == "metro",
                                         basic*0.5,
                                         rent - basic/10)))


ggplot(aes(x = rent, y = exempt, group = component, col = component),
       data = data) +
    geom_point(size = 2) +
    geom_line(linewidth = 1.4) +
    scale_x_continuous(breaks=seq(0, 480000, 48e3)) +
    scale_y_continuous(breaks=seq(-1e5, 6e5, 50e3)) +
    labs(title = "HRA Components",
         subtitle = paste("Basic: ", basic, ",  Base: ", base, ",  HRA: ", hra),
         x = "Yearly Rent") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle =  element_text(hjust = 0.5))