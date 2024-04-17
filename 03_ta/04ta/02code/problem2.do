 ------------------------------------------------------------------------------
* AUTHOR: Valentina Andrade
* CREATION: March - 2024
* ACTION: TA 4 - Problem 2
* ------------------------------------------------------------------------------

set more off 
clear all
eststo clear
* ------------------------------------------------------------------------------
* SET PATH AND LOCALS
* ------------------------------------------------------------------------------

if c(username)=="valentinaandrade" global github"/Users/valentinaandrade/Documents/GitHub/teaching/econometrics_theoryII/03_ta/04ta"

global src 	    "$github/01input/src"
global tmp  	"$github/01input/tmp"
global output   "$github/03output"

* ------------------------------------------------------------------------------
**# use data
* ------------------------------------------------------------------------------

import delimited "$src/bell_female.dat",  delimiter(" ") clear

rename v1 id
rename v2 weight
rename v3 wages
rename v4 weeks
rename v5 race
rename v6 educ
rename v7 age
rename v8 ses
rename v9 faminc
rename v10 everwed
rename v11 immig
rename v12 parent14
rename v13 both14
rename v14 sex
rename v15 afqt
rename v16 married
rename v17 poverty
rename v18 outoflf
* ------------------------------------------------------------------------------
**# (a) 
* ------------------------------------------------------------------------------

gen agexage = age*age 
gen agexeduc = age*educ
gen afqtxeduc = afqt* educ 

probit outoflf educ age ses afqt race agexage agexeduc afqtxeduc faminc married

test married

estimates store m1

probit outoflf educ age ses afqt race agexage agexeduc afqtxeduc faminc 

estimates store m2
lrtest m1 m2


* ------------------------------------------------------------------------------
**# (b) 
* ------------------------------------------------------------------------------

logit outoflf educ age ses afqt race agexage agexeduc afqtxeduc faminc married 

test married

estimates store m3

logit outoflf educ age ses afqt race agexage agexeduc afqtxeduc faminc 

estimates store m4

lrtest m3 m4

* ------------------------------------------------------------------------------
**# (c) 
* ------------------------------------------------------------------------------
gen b=0  
replace b = 1 if (weeks  == 0 ) 
replace b = 2 if (weeks  >0 ) & (weeks <20 ) 
replace b = 3 if (weeks >=20 ) 

mlogit b educ age ses afqt race agexage agexeduc afqtxeduc faminc married 


* ------------------------------------------------------------------------------
**# (d) 
* ------------------------------------------------------------------------------

replace b = 2 if (weeks ==0 ) 
replace b = 1 if (weeks >0) & (weeks <20 )
mfx compute, at ( mean b = 2) 
mfx compute, at(median b = 2)


* ------------------------------------------------------------------------------
**# (e) 
* ------------------------------------------------------------------------------

gen lwage = log(wage)

heckman lwage educ age ses afqt agexage afqtxeduc, select(outoflf = educ age ses afqt agexage agexeduc afqtxeduc faminc married) twostep mills(lambda)
