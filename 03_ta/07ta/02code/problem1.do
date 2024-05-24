* -------------------------------------------------
* AUTHOR: Valentina Andrade
* CREATION: May - 2024
* ACTION: TA 7 - Problem 1
* ----------------------------------------------

set more off 
clear all
eststo clear
* ---------------------------------------------------
* 0. set path and locals
* ---------------------------------------------------

if c(username)=="valentinaandrade" global github"/Users/valentinaandrade/Documents/GitHub/teaching/econometrics_theoryII/03_ta/07ta"

global src 	    "$github/01input/src"
global tmp  	"$github/01input/tmp"
global output   "$github/03output"

* ------------------------------------------------------
**# 1. use data
* -------------------------------------------------------

use "$src/ta07"

* ------------------------------------------------------
**# 2. set data structure
* -------------------------------------------------------

* 2.1 es importante especificar que nuestros datos conforman un panel

* 2.2 los datos deben estar en formato LONG. Usar el comando reshape si estuvieran en formato WIDE.

sum // estudiamos si nuestros datos tienen missings o no, i.e., si el panel esta balanceado
xtset id t // le declaramos a Stata que nuestros datos son un panel, reconociendo el nivel del panel y la variable temporal 

* ------------------------------------------------------
**# 3. descriptive analysis
* -------------------------------------------------------

xtsum 

// observando concluimos que hay mas variacion between que within, ¿que significa esto en este caso?

* ------------------------------------------------------
**# 4. estimation and test
* -------------------------------------------------------

* 4.1 Asumiendo One Way Error --------------------------

* OLS simple sin corregir por heterocedasticidad
reg lwage ed exp exp2 wks occ ind south smsa ms fem union blk 

* OLS correccion por correlacion:
* a) Eicker Huber White (corregir por heterocedasticidad)
reg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, vce(robust)
estimates store ols

* b) Correlacion within (cluster)
reg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, vce(cluster id)

// notemos que no cambian los coeficientes, solo los errores estandar 

* 4.2 Fixed Effects (FE) vs Random Effects (RE) --------

* a) FE: Least-Squares Dummy Variables (LSDV): modelo explicita heterogeneidad individual
set matsize 700
* hacemos efecto fijo por individuo
quietly reg lwage ed exp exp2 wks occ ind south smsa ms fem union blk i.id, vce(robust) 
* casi 600 dummies
estimates store lsdv

* b) FE: within estimator or mean-difference: modelo no estima efectos fijos explicitamente (FWL)
xtreg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, fe vce(robust)
* al agregar opcion fe al comando de stata, como datos son panel asume que es a nivel de individuo
estimates store within
* se utilizan estos errores estandar

* c) FE: first difference estimator (en diferencias temporales)
* xtreg no tiene la opcion, debe hacerse manualmente
sort id t
* generamos diferencia con D. y modelo no tiene constante porque desaparece al restar
reg D.(lwage ed exp exp2 wks occ ind south smsa ms fem union blk), vce(robust) nocons
* sin constante porque de lo contrario estariamos incluyendo una tendencia.
estimates store fd

* d) comparamos
esttab ols lsdv within fd, keep(ed D.ed exp D.exp exp2 D.exp wks D.wks occ D.occ ind D.ind south D.south smsa D.smsa ms D.ms fem D.fem union D.union blk D.blk) order(ed D.ed exp D.exp exp2 D.exp wks D.wks occ D.occ ind D.ind south D.south smsa D.smsa ms D.ms fem D.fem union D.union blk D.blk) nogap compress nodep se

* e) "RE": between estimator (promedio ponderado del FE intragrupos y entregrupos)
quietly xtreg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, be vce(bootstrap)
* solo permite correccion de errores por bootstrap
estimates store between

* f) RE: Feasible Generalized Least Squares (FGLS) y MLE
quietly xtreg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, re vce(robust)
estimates store fgls
quietly xtreg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, mle vce(bootstrap)
*solo permite correccion de errores por bootstrap
estimates store mle

* g) comparamos
esttab ols lsdv within fd between fgls mle, keep(ed D.ed exp D.exp exp2 D.exp wks D.wks occ D.occ ind D.ind south D.south smsa D.smsa ms D.ms fem D.fem union D.union blk D.blk) order(ed D.ed exp D.exp exp2 D.exp wks D.wks occ D.occ ind D.ind south D.south smsa D.smsa ms D.ms fem D.fem union D.union blk D.blk) nogap compress nodep se


* 4.3 Test de Hausman ----------------------------------

* Test de Hausman (no puede ser betas con correccion de errores pues en ese caso
* el estadistico de Hausman no tiene distribucion Chi-Cuadrado y el comando no corre 

* estimamos y guardamos entonces FE y RE sin correccion de errores estandar

quietly xtreg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, fe
estimates store within1
quietly xtreg lwage ed exp exp2 wks occ ind south smsa ms fem union blk, re 
estimates store fgls1

hausman within1 fgls1

* Se rechaza, como nuestra hipotesis nula es que preferimos RE pues son mas eficientes,
* al rechazar la nula nos quedamos con la hipotesis alternativa. Entonces Efectos Fijos 

* 4.4 Two Way Error Component Model ---------------------

* Tenemos entonces que nos gusta usar un modelo de FE en este caso, estimamos con efectos temporales ademas de los individuales

* Efectos temporales fijos
xtreg lwage ed exp exp2 wks occ ind south smsa ms fem union blk i.t, fe vce(robust)
testparm i.t // testeamos la significancia de los efectos temporales una vez corrida la regresion

* Se rechaza H0. Entonces si importan los efectos temporales.

* ------------------------------------------------------
**# 5.Additional
* -------------------------------------------------------

* En este caso estamos trabajando con datos de panel pero no hemos estimado un

* tratamiento usando un modelo DiD como el visto en clases
* Inventemos una asignacion al tratamiento 

* supongamos el tratamiento ocurre en el periodo 4 y que los tratados son los con id impar

gen dpost = (t>=4) // generamos dummy para periodo posterior
gen dtreat =mod(id,2) // generamos dummy tratamiento
gen dpostXdtreat = dpost * dtreat // generamos interacción de dummies

* 5.1 DiD estimation ---------------------------------
* corramos entonces un modelo de DiD en nuestro tratamiento inventado
** No incluimos efectos fijos temporales ni individuales pues son colineales a dpost dtreat
** Corregimos los errores a heterocedasticidad y autocorrelación intra grupos

reg lwage dpostXdtreat dpost dtreat ed exp exp2 wks occ ind south smsa ms fem union blk, cl(id)

* 5.2 Parallel Trends ---------------------------------
* ploteamos parallel trends (recordemos son datos de mentira los que estamos usando)

* imaginemos queremos hacerlo para experiencia

collapse ed exp lwage, by(dtreat t)
twoway line exp t if dtreat == 1, lcolor(blue) || line exp t if dtreat == 0, lcolor(red) ///
xline(4) /// hacemos linea que marca antes y despues
xtitle("Time") ytitle("Work Experience") title("Parallel Trends") legend(order(1 "Treated" 2 "Control"))

* ¿Cómo haríamos un DiDiD? ¿Qué variables tendríamos que incluir?

* ------------------------------------------------------
**# 6. Tips
* -------------------------------------------------------

* 6.1 Existen distintos comandos que permiten trabajar datos de panel y agregar efectos fijos
* cada uno tiene sus propias ventajas. Les recomiendo nvestigar acerca de ellos. 
 
* - xi.reg (el comando mas viejito para agregar efectos fijos haciendo i.nombrevariable)
* - reghdfe (permite hacer matchind DID y DID con IV que veran mas adelante en el curso)
* - prefijo xt y muchos comandos, ver https://www.stata.com/manuals13/xt.pdf


