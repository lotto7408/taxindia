


sip_calculator <- function(sip_amount, tenure_in_months, expected_return){
    time_seq <- 0:tenure_in_months
    r_monthly <- expected_return/12
    
    amount <- sapply(time_seq, function(x){sip_amount*(1 + (r_monthly/100)^x)})
    sum(amount)
}

sip_calculator(sip_amount = 50e3, tenure_in_months = 60, expected_return = 12)


