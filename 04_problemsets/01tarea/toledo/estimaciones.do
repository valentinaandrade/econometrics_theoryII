clear all
cd "C:\Users\joaqu\Documents\Maestría Chile\Econometrica II\Tarea 1"
use "base.dta"

*** Parte 1: Modelo Probit
probit permanence_ESUP female age applied_18 applied_19 received_18 received_19 dur_total_carr_m18 metropolitana universidades_cruch face_to_face regular accredited_prog accredited_inst i.area_conocimiento_m18 
eststo margin1: margins, dydx(female age applied_18 applied_19 received_18 received_19 dur_total_carr_m18 metropolitana universidades_cruch face_to_face regular accredited_prog accredited_inst i.area_conocimiento_m18) post

*** Parte 2: Multinomial Probit
mprobit choice female age applied_18 applied_19 received_18 received_19 dur_total_carr_m18 metropolitana universidades_cruch face_to_face regular accredited_prog accredited_inst i.area_conocimiento_m18  
eststo margin2: margins, dydx(female age applied_18 applied_19 received_18 received_19 dur_total_carr_m18 metropolitana universidades_cruch face_to_face regular accredited_prog accredited_inst i.area_conocimiento_m18) atmeans post

*** Parte 3: Conditional Logit

* 1. Long data across alternatives
reshape long tax  time, i(mrun) j(choice) 
* 222 with 4 possible alternatives, combination = 4*222 = 888
* map from alternatives (type_school) to choice (tipo)


*map from alternatives (options) to choice (choice)
gen index = _n
expand 3
sort mrun 
gen obs_id = _n
by mrun: gen rep = _n
gen options = rep
drop index obs_id rep

*We need a dummie as dependent variable. For this case we use permanence as dependente and choice in alternatives option

gen permanence = 0
replace permanence = 1 if choice== 2
asclogit permanence tax time, case(mrun) alternatives(options)
eststo margin3:  margins, dydx (tax time) atmeans post

*** Parte 4: mixed Logit
