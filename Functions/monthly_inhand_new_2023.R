

monthly_inhand_new_2023 <- function(basic, base){
    
    require("dplyr")
    
    income_before_deduction <- base
    pf <- basic * 0.12
    prof_tax <- 2400
    std_deduction <- 52.5e3
    
    taxable_income_before_exemptions <- income_before_deduction - prof_tax - std_deduction
    
    # Others
    other_exempt <- 150e3 + 50e3 # EPF and NPS
    
    taxable_income <- taxable_income_before_exemptions - 0 # sadly no exepmtions
    
    # calculate tax based on old regime
    income_tax_before_cess <- case_when(between(taxable_income, 0, 3e5)         ~ 0,
                                        between(taxable_income, 3e5+1, 6e5)   ~ (taxable_income - 250e3)*0.05,
                                        between(taxable_income, 6e5+1, 9e5)   ~ 15e3 + (taxable_income - 6e5)*0.1,
                                        between(taxable_income, 9e5+1, 12e5)  ~ 45e3 + (taxable_income - 9e5)*0.15,
                                        between(taxable_income, 12e5+1, 15e5) ~ 90e3 + (taxable_income - 12e5)*0.20,
                                        taxable_income >  15e5                  ~ 150e3 + (taxable_income - 15e5)*0.3)
    
    
    tax_after_cess <- income_tax_before_cess * 1.04
    
    
    
    pay_after_tax <- taxable_income_before_exemptions + std_deduction - tax_after_cess - pf
    pay_after_tax_monthly <- pay_after_tax / 12
    
    
    list(pay = pay_after_tax_monthly,
         tax = tax_after_cess)
    
}


