* ------------------------------------------------------------------------------
* AUTHOR: Valentina Andrade
* CREATION: March - 2024
* ACTION: TA 2 - Problem 2
* ------------------------------------------------------------------------------

set more off 
clear all
eststo clear
* ------------------------------------------------------------------------------
* SET PATH AND LOCALS
* ------------------------------------------------------------------------------

if c(username)=="valentinaandrade" global github"/Users/valentinaandrade/Documents/GitHub/teaching/econometrics_theoryII/03_ta/02ta"

global src 	    "$github/01input/src"
global tmp  	"$github/01input/tmp"
global output   "$github/03output"

* ------------------------------------------------------------------------------
**# use data
* ------------------------------------------------------------------------------

use "$src/ayudantia2", clear

* ------------------------------------------------------------------------------
**# (a) program likelihood
* ------------------------------------------------------------------------------
* in R

* ------------------------------------------------------------------------------
**# (b) logit and marginal effects
* ------------------------------------------------------------------------------

* 1. Estimate logit in Stata
qui eststo m1: logit municipal cod_nivel i.es_mujer i.prioritario i.alto_rendimiento, r


* 2. Marginals (in probability) atmeans PEA
qui eststo m11: margins, dydx (cod_nivel i.es_mujer i.prioritario i.alto_rendimiento) atmeans post

* 3. Marginals (in probability) atmeans APE
estimate restore m1
qui eststo m12: margins, dydx (cod_nivel i.es_mujer i.prioritario i.alto_rendimiento) post


* 4. Save logit and margins
esttab m1 m11 m12 using "$output/models/model1.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label lines star(* 0.10 ** 0.05 *** 0.01) compress  cells(b(star fmt(%9.3f)))  replace 

* ------------------------------------------------------------------------------
**# (c) type_school
* ------------------------------------------------------------------------------

* Label tipo
label define type_school 1 municipalnoPIE 2 municipalPIE 3 subvencionadonoPIE 4 subvencionadoPIE, replace
label value tipo type_school
* Tab
estpost tab tipo
esttab . using "$output/models/tab00.tex", cell("b pct(fmt(a))")  collab("Freq." "Percent") noobs nonumb nomtitle replace

* ------------------------------------------------------------------------------
**# (d) multinomial logit and marginal effects
* ------------------------------------------------------------------------------
* 1. Estimate multinomial logit in Stata
qui eststo m2: mlogit tipo cod_nivel i.es_mujer i.prioritario i.alto_rendimiento, r base(1)

* 2. Margins
qui eststo m21: margins, dydx (cod_nivel i.es_mujer i.prioritario i.alto_rendimiento) atmeans post

* 3. Save model2 
esttab m2 m21 using "$output/models/model2.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace

* ------------------------------------------------------------------------------
**# (e) conditional logit
* ------------------------------------------------------------------------------
* 1. Long data across alternatives
reshape long distancia p_prioritario simce_lect simce_mate, i(mrun) j(type_school) 
* 222 with 4 possible alternatives, combination = 4*222 = 888
* map from alternatives (type_school) to choice (tipo)

* 2. Dummy choice
* From manual: the outcome or chosen alternative is identified by a value of 1 in depvar, whereas zeros indicate the alternatives that were not chosen. In this case we use 1 as base category
gen choice=0
replace choice=1 if type_school==tipo

* 3. Estimation with McFadden for general conditional logit
* Similar as clogit
qui eststo m3: asclogit choice distancia p_prioritario simce_lect simce_mate, case(mrun) alternatives(type_school) vce(robust) nocons

* 4. Marginals
qui eststo m31: margins, dydx (distancia p_prioritario simce_lect simce_mate) atmeans post
* 5. Save table
esttab m3 m31 using "$output/models/model3.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace

* ------------------------------------------------------------------------------
**# (f) mixed logit or alternative conditional logit
* ------------------------------------------------------------------------------
* Xs determined by individual and alternative (mix)

*1. Estimate mixed logit with casevars
* noncons not allowed 
qui eststo m4: asclogit choice distancia p_prioritario simce_lect simce_mate, case(mrun) alternatives(type_school) casevars(cod_nivel i.es_mujer i.prioritario i.alto_rendimiento) vce(robust)

* 2. Marginals
qui eststo m41: margins, dydx (distancia p_prioritario simce_lect simce_mate cod_nivel i.es_mujer i.prioritario i.alto_rendimiento) atmeans post

* 3. Save tab models
esttab m4 m41 using "$output/models/model4.tex", se brackets pr2 nonumbers plain type nobase unstack noomitted label booktabs lines star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) replace


