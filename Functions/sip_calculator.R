


sip_calculator <- function(sip_amount, tenure_in_years, expected_return){
    tenure_in_months <- tenure_in_years*12
    time_seq <- 1:tenure_in_months
    r_monthly <- expected_return/12
    
    amount <- sapply(time_seq, function(x){sip_amount*(1 + (r_monthly/100))^x})
    sum(amount)
}

# sip_calculator(sip_amount = 50e3, tenure_in_years = 10, expected_return = 12)
# 
sip_amount <- c(5e3, 10e3)
tenure_in_months <- 20*12
time_seq <- 1:tenure_in_months
r_monthly <- c(16/12, 0/12)

amount <- lapply(time_seq, function(x){sip_amount*(1 + (r_monthly/100)^x)})

