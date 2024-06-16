clear 

use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace 

drop if ZCAIMAE=="CANMU" & (anio_solicitud==2014 | ///
anio_solicitud==2015 | anio_solicitud==2016)
 
drop if ZCAIMAE=="CASMU" & anio_solicitud==2005

qui bysort anio_solicitud: egen total_obs=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n_obs=sum(choice)
gen s_obs2=n_obs/total_obs

sort ZCAIMAE anio_solicitud 

encode ZCAIMAE, generate(iZCAIMAE)

gen l_distk=log(distk)

label variable inst "Insurance"
label variable medimae "Nephrologist"
label variable l_distk "Log Dist (km)"
label variable distk "Dist (km)"
label variable surv "Survival"
label variable URR "Quality"
label variable transp "Transport"
label variable turnos "Shifts"
label variable indep "Independent"
label variable privado "Priv Ins"
label variable publico "Public"
label variable n_lag "Lagged new patients"
label variable hab_op "Stations"
label variable cong_op "Congestion"
label variable turnos_op "Shifts"
label variable n_centro "Caseload"

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
