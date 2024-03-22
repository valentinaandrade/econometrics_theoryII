* ------------------------------------------------------------------------------
* AUTHOR: Valentina Andrade
* CREATION: March-2022
* ACTION: Problem set 1 - Problem 1
* ------------------------------------------------------------------------------

set more off 
clear all
set cformat %9.2f

* ------------------------------------------------------------------------------
* SET PATH AND LOCALS
* ------------------------------------------------------------------------------

if c(username)=="valentinaandrade" global github"/Users/valentinaandrade/Documents/GitHub/me/econometrics-theoryII/tarea1"

global src 	    "$github/01input/src"
global tmp  	"$github/01input/tmp"
global output   "$github/03output"

* ------------------------------------------------------------------------------
* use data
* ------------------------------------------------------------------------------

use "$src/AEJMicro-2009-0153_data", clear

* ------------------------------------------------------------------------------
**# (b) tabla 5 + APE o PAE
* ------------------------------------------------------------------------------
* 1. Model 5 - No robust
qui eststo m5: probit  change_or_exit complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq


* 2. Marginals (in probability) atmeans PEA
qui eststo m51: margins, dydx (complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq) atmeans post

* 3. Marginals (in probability) atmeans APE
estimate restore m5
qui eststo m52: margins, dydx (complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq) post

* 4. Save all
esttab m5 m51 m52 using "$output/models/model5.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace

*addnotes("A Probit Regression Where the Dependent Variable is Equal to 1 if a Firm Changed Its Name or Exited Completely between 2008 and 2009, and 0 Otherwise") mtitle("Model 1" "Marginal effect") replace


* ------------------------------------------------------------------------------
**#  (c) tabla 6
* ------------------------------------------------------------------------------

* 1.Model multinomial probit

* col 1
qui eststo m61: mprobit decision complaints firm_age emp_size ad_spend_k chicago comp_sq age_sq emp_sq ad_sq, base(0)

* col 2
qui eststo m62: mprobit decision complaints firm_age  emp_size ad_spend_k chicago comp_sq age_sq emp_sq ad_sq comp_chicago age_chicago comp_ad age_ad comp_emp age_emp, base(0)

* col 3
qui eststo m63: mprobit decision complaints firm_age emp_size ad_spend_k chicago  comp_sq age_sq emp_sq ad_sq comp_chicago  age_chicago comp_ad age_ad comp_emp age_emp  ad_chicago comp_ad_chicago age_ad_chicago, base(0)

* 2. Save all
esttab m61 m62 m63 using "$output/models/model6.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label lines star(* 0.10 ** 0.05 *** 0.01) compress  cells(b(star fmt(%9.3f)))  replace 

*A Multinomial Probit Regression Where the Choices or a Firm are not Changing Its Name and Not Exiting (Base Outcome), Exiting, and Changing Its Name between 2008 and 2009


* ------------------------------------------------------------------------------
**# (d) tabla 5 y 6 - logit
* ------------------------------------------------------------------------------

* 1. Model 5 - No robust
qui eststo m7: logit  change_or_exit complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq


* 2. Marginals (in probability) atmeans PEA
qui eststo m71: margins, dydx (complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq) atmeans post

* 3. Marginals (in probability) atmeans APE
estimate restore m7
qui eststo m72: margins, dydx (complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq) post

* 4. Save all
esttab m7 m71 m72 using "$output/models/model7.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace
*addnotes("A Logit Regression Where the Dependent Variable is Equal to 1 if a Firm Changed Its Name or Exited Completely between 2008 and 2009, and 0 Otherwise") mtitle("Model 1" "Model 2" "Model 3") replace


* 1.Model multinomial probit

* col 1
qui eststo m81: mlogit decision complaints firm_age emp_size ad_spend_k chicago comp_sq age_sq emp_sq ad_sq, base(0)

* col 2
qui eststo m82: mlogit decision complaints firm_age  emp_size ad_spend_k chicago comp_sq age_sq emp_sq ad_sq comp_chicago age_chicago comp_ad age_ad comp_emp age_emp, base(0)

* col 3
qui eststo m83: mlogit decision complaints firm_age emp_size ad_spend_k chicago  comp_sq age_sq emp_sq ad_sq comp_chicago  age_chicago comp_ad age_ad comp_emp age_emp  ad_chicago comp_ad_chicago age_ad_chicago, base(0)

* 2. Save all
esttab m81 m82 m83 using "$output/models/model8.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace
*addnotes("A Multinomial Logit Regression Where the Choices for a Firm are not Changing Its Name and Not Exiting (Base Outcome), Exiting, and Changing Its Name between 2008 and 2009") mtitle("Model 1" "Model 2" "Model 3") replace


* ------------------------------------------------------------------------------
**# (e) tabla 7 y 8
* ------------------------------------------------------------------------------

*Tabla 7

*1. Probit
qui eststo m9: probit drop_name complaints firm_age emp_size ad_spend_k chicago comp_sq age_sq emp_sq ad_sq

* 2. Marginals (in probability) atmeans PEA
qui eststo m91: margins, dydx (complaints firm_age emp_size ad_spend_k chicago comp_sq age_sq emp_sq ad_sq) atmeans post

* 3. Save all
esttab m9 m91 using "$output/models/model9.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace
*addnotes("A Probit Regression Where the Dependent Variable is Equal to 1 if a Firm Drops a Name in 2009, and 0 Otherwise") mtitle("Model " "Marginal Effect") replace
*Tabla 8

*1. Probit
qui eststo m10: probit multiple_names complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq

* 2. Marginals (in probability) atmeans PEA

qui eststo m110: margins, dydx (complaints firm_age emp_size ad_spend_k  chicago comp_sq age_sq emp_sq ad_sq) atmeans post

* 3. Save all
esttab m10 m110 using "$output/models/model10.tex",  se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace
*addnotes("A Probit Regression Where the Dependent Variable is Equal to 1 if a Firm Uses More Than One Name in 2008, and 0 Otherwise") mtitle("Model" "Marginal Effect") replace

