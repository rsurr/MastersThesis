
use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace
keep choice inst medimae distk occ_var URR CAPACNUM ID_CAIMAE

*Institución: dummy
* Interpretación inst: cuánto aumenta la probabilidad por pasar de 0 a 1?
clogit choice inst medimae distk occ_var URR , group(CAPACNUM)
margins, dydx(inst)
margins, dydx(inst) at(inst=0)
* Interpretación dydx: manteniendo todos los demás efectos constantes?
* Interpretación dydx: saca el beta de la función de utilidad?
margins, dydx(*)
* Debería hacerlo con expression?
margins, expression( ///
logistic(_b[inst]*inst + _b[medimae]*medimae + _b[distk]*distk + _b[occ_var]*occ_var + _b[URR]*URR) ///
- logistic(_b[medimae]*medimae)/ ///
					logistic(_b[medimae]*medimae + _b[distk]*distk + _b[occ_var]*occ_var + _b[URR]*URR)) ///
					dydx(inst)

* Distancia: continua. A lo Cameron y Trivedi
* Es "back of the envelope"?
clogit choice inst medimae distk occ_var URR , group(CAPACNUM)
margins, dydx(distk)
predict pinitial					
					
gen med_todos=.
gen med_todos_choice=.

foreach i in 2 40 57 60 61 62 63 64 65 66 67 68 69 70 72 86 87 88 95 {
replace distk = distk + 1 if ID_CAIMAE=="`i'"
predict pnewd_`i' 
gen med_`i' = pnewd_`i' - pinitial
replace med_todos =  med_`i' if ID_CAIMAE=="`i'"
replace med_todos_choice =  med_`i' if ID_CAIMAE=="`i'" & choice==1
replace distk = distk - 1 if ID_CAIMAE=="`i'"
}

* Tengo que promediar solo para los que eligen la opción o para todos?
clogit choice inst medimae distk occ_var URR , group(CAPACNUM)
margins, dydx(distk)					
sum med_todos med_todos_choice

* Lo mismo pero para inst
preserve
clogit choice inst medimae distk occ_var URR , group(CAPACNUM)
margins, dydx(inst)

gen med_inst_todos=.
gen med_inst_todos_choice=.
foreach i in 2 40 57 60 61 62 63 64 65 66 67 68 69 70 72 86 87 88 95 {
replace inst = 0 if ID_CAIMAE=="`i'"
predict pinital_inst_`i' 
replace inst = 1 if ID_CAIMAE=="`i'"
predict pnewi_`i' 
gen med_inst_`i' = pnewi_`i' - pinital_inst_`i'
replace med_inst_todos =  med_inst_`i' if ID_CAIMAE=="`i'"
replace med_inst_todos_choice =  med_inst_`i' if ID_CAIMAE=="`i'" & choice==1
}
	
sum med_inst_todos med_inst_todos_choice
restore
