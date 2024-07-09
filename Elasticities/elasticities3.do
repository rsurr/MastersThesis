clear 

use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace 


gen turnos_op=.
gen hab_op=.

/// TURNOS
replace turnos_op=6 if ZCAIMAE=="ASOCIACION ESPAÑOLA"
replace turnos_op=6 if ZCAIMAE=="CANMU"
replace turnos_op=6 if ZCAIMAE=="CASA DE GALICIA"
replace turnos_op=6 if ZCAIMAE=="CASMU"
replace turnos_op=4 if ZCAIMAE=="CE.DI.SA."
replace turnos_op=6 if ZCAIMAE=="HOSPITAL BRITANICO"
replace turnos_op=4 if ZCAIMAE=="HOSPITAL DE CLINICAS"
replace turnos_op=6 if ZCAIMAE=="HOSPITAL EVANGELICO"
replace turnos_op=6.333 if ZCAIMAE=="HOSPITAL MACIEL"
replace turnos_op=6 if ZCAIMAE=="INTIR"
replace turnos_op=5 if ZCAIMAE=="INU"
replace turnos_op=5 if ZCAIMAE=="NEPHROS"
replace turnos_op=6 if ZCAIMAE=="RENIS"
replace turnos_op=. if ZCAIMAE=="SARI"
replace turnos_op=6 if ZCAIMAE=="SEDIC"
replace turnos_op=5 if ZCAIMAE=="SMI - SERVICIO MEDICO INTEGRAL"
replace turnos_op=5 if ZCAIMAE=="UNIVERSAL"
replace turnos_op=6 if ZCAIMAE=="URUGUAYANA"

/// ESTACIONES
replace hab_op=12 if ZCAIMAE=="ASOCIACION ESPAÑOLA"
replace hab_op=18 if ZCAIMAE=="CANMU"
replace hab_op=15 if ZCAIMAE=="CASA DE GALICIA"

replace hab_op=15 if ZCAIMAE=="CASMU"
replace hab_op=18 if ZCAIMAE=="CASMU" & (anio_solicitud==2013 | anio_solicitud==2014)
replace hab_op=36 if ZCAIMAE=="CASMU" & (anio_solicitud==2015 | anio_solicitud==2016)

replace hab_op=10 if ZCAIMAE=="CE.DI.SA."

replace hab_op=8 if ZCAIMAE=="HOSPITAL BRITANICO"
replace hab_op=9 if ZCAIMAE=="HOSPITAL BRITANICO" & /// 
(anio_solicitud==2014 | anio_solicitud==2015 | anio_solicitud==2016)

replace hab_op=8 if ZCAIMAE=="HOSPITAL DE CLINICAS"
replace hab_op=11 if ZCAIMAE=="HOSPITAL EVANGELICO"
replace hab_op=12 if ZCAIMAE=="HOSPITAL MACIEL"
replace hab_op=12 if ZCAIMAE=="INTIR"
replace hab_op=12 if ZCAIMAE=="INU"
replace hab_op=12 if ZCAIMAE=="NEPHROS"
replace hab_op=12 if ZCAIMAE=="RENIS"
replace hab_op=. if ZCAIMAE=="SARI"
replace hab_op=12 if ZCAIMAE=="SEDIC"
replace hab_op=12 if ZCAIMAE=="SMI - SERVICIO MEDICO INTEGRAL"
replace hab_op=12 if ZCAIMAE=="UNIVERSAL"
replace hab_op=15 if ZCAIMAE=="URUGUAYANA"

drop if ZCAIMAE=="CANMU" & (anio_solicitud==2014 | ///
anio_solicitud==2015 | anio_solicitud==2016)
 
drop if ZCAIMAE=="CASMU" & anio_solicitud==2005

/// CAPACIDAD
gen cap_op=turnos_op*hab_op
gen cong_op=n_capacnum/cap_op

qui bysort anio_solicitud: egen total=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n=sum(choice)

sort ZCAIMAE anio_solicitud 
by ZCAIMAE: gen n_lag = n[_n-1]	

matrix drop _all 

encode ZCAIMAE, generate(iZCAIMAE)

gen publico=0
replace publico=1 if chain=="PUBLICO"
gen privado=0
replace privado=1 if chain=="PRIVADO"
gen indep=0
replace indep=1 if tipo_imae=="INDEPENDIENTE"
gen forprofit=0
replace forprofit=1 if chain=="INDEPENDIENTE"
replace forprofit=1 if ZCAIMAE=="HOSPITAL BRITANICO"
gen l_distk=log(distk)

label variable inst "Insurance"
label variable medimae "Nephrologist"
label variable l_distk "Log Dist (km)"
label variable distk "Dist (km)"
label variable surv "Survival"
label variable URR "Quality"
label variable transp "Transport"
label variable turnos "Shifts"
label variable pac_var "Occupancy"
label variable indep "Independent"
label variable privado "Priv Ins"
label variable publico "Public"
label variable n_lag "Lagged new patients"
label variable habilitados_def "Stations"
label variable hab_op "Stations"
label variable cong_op "Congestion"
label variable turnos_op "Shifts"

eststo reg1_tiene: clogit choice inst medimae distk URR turnos_op cong_op if tiene_imae==1, group(CAPACNUM)
eststo reg2_tiene: clogit choice inst medimae distk URR turnos_op hab_op if tiene_imae==1, group(CAPACNUM)
eststo reg3_tiene: clogit choice inst medimae distk URR cong_op i.iZCAIMAE if tiene_imae==1, group(CAPACNUM)

esttab reg1_tiene reg2_tiene reg3_tiene ///
using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Elasticities\tiene.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) pr2 ///
    label nomtitle drop(*.iZCAIMAE) ///
    style(tex) title("Patients whose insurance has a facility") replace booktab


eststo reg1_notiene: clogit choice inst medimae distk URR turnos_op cong_op if tiene_imae==0, group(CAPACNUM)
eststo reg2_notiene: clogit choice inst medimae distk URR turnos_op hab_op if tiene_imae==0, group(CAPACNUM)
eststo reg3_notiene: clogit choice inst medimae distk URR cong_op i.iZCAIMAE if tiene_imae==0, group(CAPACNUM)

esttab reg1_notiene reg2_notiene reg3_notiene ///
using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Elasticities\notiene.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) pr2 ///
    label nomtitle drop(*.iZCAIMAE) ///
    style(tex) title("Patients whose insurance doesn't have a facility") replace  booktab

	
global continuous distk URR turnos_op hab_op
global discrete inst medimae 


/// TIENE IMAE

clogit choice $discrete $continuous if tiene_imae==1, group(CAPACNUM)

predict prob if e(sample), pc1

qui foreach x in $continuous {
cap drop b_`x'
gen b_`x'=_b[`x']
gen e_`x' = b_`x'*(1-prob)*`x'
}

qui foreach x in $discrete {
cap drop b_`x'
gen actual_`x'=`x'
gen se_`x'=.
gen or_`x'=.
gen rr_`x'=.
gen rd_`x'=.

	foreach i in 2 40 57 60 61 62 63 64 65 66 67 68 69 70 72 86 87 88 95 {
	replace `x' = 0 if ID_CAIMAE=="`i'"
	predict p_`x'0_`i' if e(sample), pc1
	replace `x' = 1 if ID_CAIMAE=="`i'"
	predict p_`x'1_`i' if e(sample), pc1
	replace `x' = actual_`x' 
	replace se_`x' = (p_`x'1_`i' - p_`x'0_`i')/p_`x'0_`i'  if ID_CAIMAE=="`i'"
	replace or_`x' = (p_`x'1_`i'/(1 - p_`x'1_`i'))/(p_`x'0_`i'/(1 - p_`x'0_`i'))  if ID_CAIMAE=="`i'"
	replace rr_`x' = p_`x'1_`i'/p_`x'0_`i'  if ID_CAIMAE=="`i'"
	replace rd_`x' = p_`x'1_`i' - p_`x'0_`i'  if ID_CAIMAE=="`i'"
	}
}

tabstat e_* se_* rd_*, statistics(mean sd) format(%9.3g) save
matrix stats_tiene = r(StatTotal)'
matrix list stats_tiene

/// NO TIENE IMAE

drop e_* se_* or_* rr_* rd_* prob p_* b_* actual_*

clogit choice $discrete $continuous if tiene_imae==0, group(CAPACNUM)
predict prob if e(sample), pc1

qui foreach x in $continuous {
gen b_`x'=_b[`x']
gen e_`x' = b_`x'*(1-prob)*`x'
}

qui foreach x in $discrete {
gen actual_`x'=`x'
gen se_`x'=.
gen or_`x'=.
gen rr_`x'=.
gen rd_`x'=.

	foreach i in 2 40 57 60 61 62 63 64 65 66 67 68 69 70 72 86 87 88 95 {
	replace `x' = 0 if ID_CAIMAE=="`i'"
	predict p_`x'0_`i' if e(sample), pc1
	replace `x' = 1 if ID_CAIMAE=="`i'"
	predict p_`x'1_`i' if e(sample), pc1
	replace `x' = actual_`x' 
	replace se_`x' = (p_`x'1_`i' - p_`x'0_`i')/p_`x'0_`i'  if ID_CAIMAE=="`i'"
	replace or_`x' = (p_`x'1_`i'/(1 - p_`x'1_`i'))/(p_`x'0_`i'/(1 - p_`x'0_`i'))  if ID_CAIMAE=="`i'"
	replace rr_`x' = p_`x'1_`i'/p_`x'0_`i'  if ID_CAIMAE=="`i'"
	replace rd_`x' = p_`x'1_`i' - p_`x'0_`i'  if ID_CAIMAE=="`i'"
	}
}

tabstat e_* se_* rd_*, statistics(mean sd) format(%9.3g) save
matrix stats_notiene = r(StatTotal)'
matrix list stats_notiene

matrix stats= stats_tiene , stats_notiene
matrix rownames stats = Distance Quality Shifts Stations Insurance Nephrologist Insurance Nephrologist
matrix colnames stats = Mean SD Mean SD
matrix list stats

*ssc install matmap
matmap stats stats2, map(round(@, 0.01))

*ssc install estout

// Create a table using estout
esttab matrix(stats2) using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Elasticities\elasticities3.tex", ///
    cells(fmt(%3.3g)) ///
    label  ///
    style(tex) title("Elasticites and semielasticites") replace
