


sip_calculator <- function(sip_amount, tenure_in_years, expected_return){
    tenure_in_months <- tenure_in_years*12
    time_seq <- 1:tenure_in_months
    r_monthly <- expected_return/12
    
    amount <- sapply(time_seq, function(x){sip_amount*(1 + (r_monthly/100))^x})
    sum(amount)
}