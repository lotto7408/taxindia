
monthly_inhand_new_2020 <- function(basic, base){
    require("dplyr")
    
    income_before_deduction <- base
    pf <- basic * 0.12
    prof_tax <- 2400
    
    taxable_income_before_exemptions <- income_before_deduction 
    
    
    taxable_income <- taxable_income_before_exemptions - 0 # sadly no exepmtions
    
    # calculate tax based on old regime
    income_tax_before_cess <- case_when(between(taxable_income, 0, 250e3)         ~ 0,
                                        between(taxable_income, 250e3+1, 500e3)   ~ (taxable_income - 250e3)*0.05,
                                        between(taxable_income, 500e3+1, 750e3)   ~ 12500 + (taxable_income - 500e3)*0.1,
                                        between(taxable_income, 750e3+1, 1000e3)  ~ 37500 + (taxable_income - 750e3)*0.15,
                                        between(taxable_income, 1000e3+1, 1250e3) ~ 75e3 + (taxable_income - 1000e3)*0.20,
                                        between(taxable_income, 1250e3+1, 1500e3) ~ 125e3 + (taxable_income - 1250e3)*0.25,
                                        taxable_income >  1500e3                  ~ 187500 + (taxable_income - 1500e3)*0.3)
    
    
    tax_after_cess <- income_tax_before_cess * 1.04
    
    
    
    pay_after_tax <- taxable_income_before_exemptions - tax_after_cess - pf - prof_tax
    pay_after_tax_monthly <- pay_after_tax / 12
    
    
    list(pay = pay_after_tax_monthly,
         tax = tax_after_cess)
    
}
