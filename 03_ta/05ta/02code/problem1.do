* -------------------------------------------------
* AUTHOR: Valentina Andrade
* CREATION: March - 2024
* ACTION: TA 5 - Problem 1
* ----------------------------------------------

set more off 
clear all
eststo clear
* ---------------------------------------------------
* SET PATH AND LOCALS
* ---------------------------------------------------

if c(username)=="valentinaandrade" global github"/Users/valentinaandrade/Documents/GitHub/teaching/econometrics_theoryII/03_ta/05ta"

global src 	    "$github/01input/src"
global tmp  	"$github/01input/tmp"
global output   "$github/03output"

* ------------------------------------------------------
**# use data
* -------------------------------------------------------

use "/Users/valentinaandrade/Downloads/uis.dta", clear

 *Specify time of censoring or failure, tvar, and specify that event = 2 represents a failure
stset time, failure(censor)

* ------------------------------------------------------
**# 1. Univariate analysis
* -------------------------------------------------------
list ID time censor age ndrugtx treat site herco in 1/10, nodisplay


sts test treat, logrank
sts graph, by(treat)

sts test site, logrank
sts graph, by(site)

sts test herco
sts graph, by(herco) 

*Cox

stcox ndrugtx, nohr
stcox age, nohr

* ------------------------------------------------------
**# 2. Model building
* -------------------------------------------------------

stcox age ndrugtx i.treat i.site i.herco, nohr
test 2.herco 3.herco
stcox age ndrugtx i.treat i.site, nohr


stcox age ndrugtx i.treat i.site c.age#c.ndrug, nohr
stcox age ndrugtx i.treat i.site c.age#i.treat, nohr
stcox age ndrugtx i.treat i.site c.age#i.site, nohr
stcox age ndrugtx i.treat i.site c.ndrug#i.treat, nohr
stcox age ndrugtx i.treat i.site c.ndrug#i.site, nohr
stcox age ndrugtx i.treat i.site i.treat#i.site, nohr
stcox age ndrugtx i.treat i.site c.age#i.site, nohr
estimates store m1

stcox age ndrugtx i.treat i.site, nohr
lrtest . m1
stcox age ndrugtx i.treat i.site c.age#i.site
stcox, nohr


* ------------------------------------------------------
**# 3. Proportional assumption
* -------------------------------------------------------
*Option A
stcox age ndrugtx i.treat i.site c.age#i.site, nohr tvc(age ndrugtx treat site) texp(ln(_t))


*Option B
quietly stcox age ndrugtx treat site c.age#i.site, schoenfeld(sch*) scaledsch(sca*)
stphtest, detail
stphtest, plot(age) msym(oh)
stphtest, plot(ndrugtx) msym(oh)
stphtest, plot(treat) msym(oh)
stphtest, plot(site) msym(oh)
stphtest, plot(c.age#1.site) msym(oh)

stphplot, by(treat) plot1(msym(oh)) plot2(msym(th))
stphplot, by(site) plot1(msym(oh)) plot2(msym(th))
drop sch1-sch5 sca1-sca5

*Solution?
stcox age ndrugtx site c.age#i.site, nohr strata(treat)


* ------------------------------------------------------
**# 4. Survival functions
* -----------------------------------------------------

stcox age ndrugtx treat site c.age#i.site, nohr basesurv(surv0)
generate surv1 = surv0^exp( (-0.0336943*30+0.0364537*5 - 0.2674113))
line surv1 _t, sort ylab(0 .1 to 1) xlab(0 200 to 1200)

generate surv2 = surv0^exp( (-0.0336943*30+0.0364537*5))
label variable surv1 "long treatment"
label variable surv2 "short treatment"
line surv1 surv2 _t, sort ylab(0 .1 to 1) xlab(0 200 to 1200) 
drop surv0-surv2



