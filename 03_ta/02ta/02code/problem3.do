* ------------------------------------------------------------------------------
* AUTHOR: Valentina Andrade
* CREATION: March-2022
* ACTION: Problem set 1 - Problem 3
* ------------------------------------------------------------------------------

* ------------------------------------------------------------------------------
* SET 
* ------------------------------------------------------------------------------
set more off 
clear all
* I have STATA 17 MP but some people don't
version 16 
est drop _all
* ------------------------------------------------------------------------------
* PATH AND LOCALS
* ------------------------------------------------------------------------------

if c(username)=="valentinaandrade" global github"/Users/valentinaandrade/Documents/GitHub/me/econometrics-theoryII/tarea1"

global src 	    "$github/01input/src"
global tmp  	"$github/01input/tmp"
global output   "$github/03output"

* ------------------------------------------------------------------------------
* use data
* ------------------------------------------------------------------------------

use "$src/panel_muni_week", clear


*The dependent variable is the number of crimes divided by the weekly average reported in the pre-period in the same crime category

* ------------------------------------------------------------------------------
**# use data
* ------------------------------------------------------------------------------
* Count number of crimes (sum) by type of crime and week
collapse (sum) crime, by(cash bus week)

* ------------------------------------------------------------------------------
**# Gen times vars
* ------------------------------------------------------------------------------

* 2005 to 2010 (313 weeks)

*Pre: From Saturday, 1 January 2005 to Saturday, 1 October 2005, there are 39 weeks
* But for replicate is 22 October 2005 (I saw on internet)
gen pre =0
replace pre=1 if week<42

* Tran: From Saturday, 1 October 2005 to Wednesday, 10 February 2010, there are 227 weeks and 4 days
*p.12 inconsistency
* Real: from 22 october 2005 y 10 febraury 2007
gen tra =0
replace tra=1 if week>=42 & week<110

* Post: From Saturday, 10 February 2007 to Wednesday, 10 February 2010, there are 156 weeks and 4 days
* Real check 
gen post =0
replace post=1 if week>=110

* gen period ------------------------------------------------------------------
* Ordinal period 
gen period =0 if pre
replace period =1 if tra
replace period=2 if post

* ------------------------------------------------------------------------------
**#  0. Tab 1
* ------------------------------------------------------------------------------
* I want to know if I'm doing the things right (sanity check)

table period cash if bus==1, c(mean crime semean crime) format(%9.2fc)
table period cash if bus==0, c(mean crime semean crime) format(%9.2fc)


* ------------------------------------------------------------------------------
**#  b. Model Table 2
* ------------------------------------------------------------------------------

* ------------------------------------------------------------------------------
**#  b.1 create vars
* ------------------------------------------------------------------------------

** Normalize crime
* each coefficient represents the "percentage change in crime for each period" (week),
* reltive to level during the pre-period in the same category p.16

gen crime_n = .
forvalues i=0/1 {
    forvalues k=0/1 {
        quietly summarize crime if !tra & !post & bus == `i' & cash == `k', meanonly
        replace crime_n = crime / r(mean) if bus == `i' & cash == `k'
    }
}


* Robbps represent robberies in streets an ps in the same crime category (cash)
* In avarage (mean) and with crime normalized
gen crime_n0 = crime_n if bus==0 
bys cash week: egen robb_ps=mean(crime_n0)


** Month variable FE
* Month variable FE
gen year = 2005 + floor((week)/52)
replace year = 2010 if week == 312

gen month =. 
foreach year of numlist 2005/2010 {
replace month = int((week - 52 * (`year' - 2005))/52*12)+1 if year == `year'
}
replace month = int((week-52+1)/52*12)+1 if year ==2006
replace month = 12 if month == 13 & (year == 2010 | year == 2006)

* ------------------------------------------------------------------------------
**# Model linear
* ------------------------------------------------------------------------------

** Times 
global times tra post

* Estimation  robust----------------------------------------------------------

* No cash models cash==0 & bus==1
qui eststo m_1: reg crime_n $times if cash==0 & bus==1, r
qui eststo m_2: reg crime_n $times i.month if cash==0 & bus==1, r
qui eststo m_3: reg crime_n $times robb_ps i.month if cash==0 & bus==1, r

* Cash models cash==1 & bus==1
qui eststo m_4: reg crime_n $times if cash==1 & bus==1, r
qui eststo m_5: reg crime_n $times i.month if cash==1 & bus==1, r
qui eststo m_6: reg crime_n $times robb_ps i.month if cash==1 & bus==1, r

*Table 2 ----------------------------------------------------------------------
esttab m_* using "$output/models/model11.tex", keep (tra post) se r2 brackets nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace
*su crime if cash==0 & bus==1 & pre
*su crime if cash==1 & bus==1 & pre


* ------------------------------------------------------------------------------
**# c.1 Model Table 2 - Poisson
* ------------------------------------------------------------------------------

* No cash models cash==0 & bus==1
qui eststo p_1: poisson crime $times if cash==0 & bus==1, r nolog
qui eststo p_2: poisson crime $times i.month if cash==0 & bus==1, r nolog
qui eststo p_3: poisson crime $times robb_ps i.month if cash==0 & bus==1, r nolog

* Cash models cash==1 & bus==1
qui eststo p_4: poisson crime $times if cash==1 & bus==1, r nolog
qui eststo p_5: poisson crime $times i.month if cash==1 & bus==1, r nolog
qui eststo p_6: poisson crime $times robb_ps i.month if cash==1 & bus==1, r nolog

*Table 2 ----------------------------------------------------------------------
esttab p_* using "$output/models/model12.tex", keep (tra post) se pr2 brackets nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace

* ------------------------------------------------------------------------------
**# c.2 Model Table 2 - Binomial negative
* ------------------------------------------------------------------------------

* No cash models cash==0 & bus==1
qui eststo b_1: nbreg crime $times if cash==0 & bus==1, r nolog
qui eststo b_2: nbreg crime $times i.month if cash==0 & bus==1, r nolog
qui eststo b_3: nbreg crime $times robb_ps i.month if cash==0 & bus==1, r nolog

* Cash models cash==1 & bus==1
qui eststo b_4: nbreg crime $times if cash==1 & bus==1, r nolog
qui eststo b_5: nbreg crime $times i.month if cash==1 & bus==1, r nolog
qui eststo b_6: nbreg crime $times robb_ps i.month if cash==1 & bus==1, r nolog

*Table 2 ----------------------------------------------------------------------
esttab b_* using "$output/models/model13.tex", keep(tra post) se pr2 brackets nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace

esttab *_6 using "$output/models/model14.tex", keep (tra post) se pr2 r2 brackets nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace


* ------------------------------------------------------------------------------
**# d. Additional  test
* ------------------------------------------------------------------------------
* Equidispersion
est restore p_6
estat gof
