* -------------------------------------------------
* AUTHOR: Valentina Andrade
* CREATION: May - 2024
* ACTION: TA 8 - Problem 2
* ----------------------------------------------

set more off 
clear all
eststo clear
* ---------------------------------------------------
* 0. set path and locals
* ---------------------------------------------------

if c(username)=="valentinaandrade" global github"/Users/valentinaandrade/Documents/GitHub/teaching/econometrics_theoryII/03_ta/08ta"

global src 	    "$github/01input/src"
global tmp  	"$github/01input/tmp"
global output   "$github/03output"

* ------------------------------------------------------
**# 1. use data
* -------------------------------------------------------

use "$src/DS2004.dta", clear
	
// Miremos la data
	describe
	sum
	unique block
	tab month
	sort block month
	br
	
// New variables

	// Generamos variable para el treatment mas alejado
	gen twoblock = (distance==2)

	// Dummies foreach month
	forvalues x=4(1)12{
		gen m`x' = (month==`x')
	}
	
	* tabulate month, generate(m)
	
	// Treatment x Post dummy
	foreach var of varlist sameblock oneblock twoblock{
		gen `var'_t = `var'*(month>=8)
	}

	// Pre and Post Dummies
	gen post = (month>=8)
	gen pre = (month<8)


	// Leads and lags for treatment
	foreach var of varlist sameblock {
		forvalues j=4(1)12{
		gen m`j'_`var' = m`j'*`var'
		}	
		}
	


// -----------------------------------------------------------------------------
// 2.- DiD
// -----------------------------------------------------------------------------

	eststo: reghdfe thefts sameblock_t , abs(block month) vce(cluster block)

	eststo: reghdfe thefts sameblock_t oneblock_t , abs(block month) vce(cluster ///
	block)

	eststo: reghdfe thefts sameblock_t oneblock_t twoblock_t , abs(block month) ///
		vce(cluster block)
		
	esttab, se
	eststo clear
	
// -----------------------------------------------------------------------------
// 3.1 Prueba Tendencias Paralelas sobre muestra restringida
// -----------------------------------------------------------------------------

	// Rezagamos el tratamiento y corremos la regresion de antes sobre las 
	// observaciones previas al tratamiento.

	eststo: reghdfe thefts m5_sameblock m6_sameblock m7_sameblock if month<8, ///
	abs(block month) vce(cluster block)
	
	esttab, p
	eststo clear
					
	// Verificamos significancia individual y conjunta					
	test m5_sameblock m6_sameblock m7_sameblock	

// -----------------------------------------------------------------------------
// 3.2 Prueba Tendencias Paralelas usando linear-trend model
// -----------------------------------------------------------------------------
	
	// Time trends
	gen pre_trend = sameblock*month*pre
	gen post_trend = sameblock*month*post


	eststo: reghdfe	thefts sameblock_t pre_trend post_trend , abs(block month) ///
			vce(cluster block)
			
	esttab, p
	eststo clear
			
	// H0: No hay diferencias en tendencias previas al tratamiento (Parallel trend)
	test pre_trend			
					
// -----------------------------------------------------------------------------
// 3.3 Prueba Visual Tendencias Paralelas 
// -----------------------------------------------------------------------------
// Disclaimer: Sin la opcion "residuals", la prediccion falla, porque no tiene 
// en cuenta los efectos fijos absorbidos.

	reghdfe thefts sameblock_t pre_trend post_trend , abs(block month) ///
			vce(cluster block) residuals(resid)
			
	gen thefts_hat = (resid - thefts)*(-1)

	//	Colapsamos para ver los promedios de tratados y controles
	*preserve
	collapse (mean) thefts thefts_hat , by (sameblock month)

	capture graph drop _all

	// Promedios observados
	twoway line thefts month if sameblock==1 , || /*
		*/	line thefts month if sameblock==0 , /*
		*/ xtitle("Mes") ytitle("Total Autos robados") xlabel(4(1)12)/*
		*/ title("Promedios observados") xline(7, lcolor(red)) /*
		*/ legend(order(1 "Tratados" 2 "Controles") position(6) rows(1)) /*
		*/ name(obs_mean)

	// Predicciones del linear trend model
	twoway line thefts_hat month if sameblock==1 , || /*
		*/	line thefts_hat month if sameblock==0 , /*
		*/ xtitle("Mes") ytitle("Total Autos robados") xlabel(4(1)12) /*
		*/ title("Linear-trends model") xline(7, lcolor(red)) /*
		*/ legend(order(1 "Tratados" 2 "Controles") position(6) rows(1)) /*
		*/ name(trend_mean)	
		
		
	graph combine obs_mean trend_mean
	*restore	

	
	

