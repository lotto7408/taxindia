

monthly_inhand_old <- function(basic, base, hra, rent_yearly, NPS){
    require("dplyr")
    
    income_before_deduction <- base
    pf <- basic * 0.12
    prof_tax <- 2400
    std_deduction <- 50000
    
    taxable_income_before_exemptions <- income_before_deduction - std_deduction - prof_tax
    
    
    
    # Exemptions:
    # 1 HRA
    hra_components <- rep(0, 3)
    hra_components[1] <- hra #Actual HRA received
    hra_components[2] <- basic*0.5 #40% (non-metro city) or 50% (metro city ) of the salary
    hra_components[3] <- rent_yearly - basic/10 #Actual rent paid more than 10% of the salary.
    hra_exempt <- max(min(hra_components), 0)
    
    
    # Others
    other_exempt <- ifelse(NPS == FALSE, 150e3, 200e3)  # EPF and NPS
    
    taxable_income <- taxable_income_before_exemptions - hra_exempt - other_exempt
    
    
    # calculate tax based on old regime
    income_tax_before_cess <- case_when(between(taxable_income, 0, 250e3)        ~ 0,
                                        between(taxable_income, 250e3+1, 500e3)  ~ (taxable_income - 250e3)*0.05,
                                        between(taxable_income, 500e3+1, 1000e3) ~ 12500 + (taxable_income - 500e3)*0.2,
                                        taxable_income >  1000e3                 ~ 112500 + (taxable_income - 1000e3)*0.3)
    
    tax_after_cess <- income_tax_before_cess * 1.04
    
    pay_after_tax <- taxable_income_before_exemptions + std_deduction - tax_after_cess - pf
    pay_after_tax_monthly <- pay_after_tax / 12
    
    list(pay = pay_after_tax_monthly,
         tax = tax_after_cess)
    
}

