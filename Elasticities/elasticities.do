clear 

use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace

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
label variable surv "Survival"
label variable URR "Quality"
label variable transp "Transport"
label variable turnos "Shifts"
label variable pac_var "Occupancy"
label variable indep "Independent"
label variable privado "Priv Ins"


global continuous l_distk URR pac_var 
global discrete inst medimae privado indep

eststo reg1_tiene: clogit choice inst medimae l_distk URR pac_var if tiene_imae==1, group(CAPACNUM)
eststo reg2_tiene: clogit choice inst medimae l_distk URR pac_var indep privado if tiene_imae==1, group(CAPACNUM)
eststo reg3_tiene: clogit choice inst medimae l_distk URR pac_var i.iZCAIMAE if tiene_imae==1, group(CAPACNUM)
eststo reg4_tiene: clogit choice inst medimae l_distk i.iZCAIMAE i.anio_solicitud i.iZCAIMAE#i.anio_solicitud if tiene_imae==1, group(CAPACNUM) dif

esttab reg1_tiene reg2_tiene reg3_tiene reg4_tiene using "hola.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) pr2 ///
    label nomtitle keep(inst medimae l_distk URR pac_var privado indep) ///
    style(tex) title("Regressions") replace


eststo reg1_notiene: clogit choice inst medimae l_distk URR pac_var if tiene_imae==0, group(CAPACNUM)
eststo reg2_notiene: clogit choice inst medimae l_distk URR pac_var indep privado if tiene_imae==0, group(CAPACNUM)
eststo reg3_notiene: clogit choice inst medimae l_distk URR pac_var i.iZCAIMAE if tiene_imae==0, group(CAPACNUM)
eststo reg4_notiene: clogit choice inst medimae l_distk i.iZCAIMAE i.anio_solicitud i.iZCAIMAE#i.anio_solicitud if tiene_imae==0, group(CAPACNUM) dif

esttab reg1_notiene reg2_notiene reg3_notiene reg4_notiene using "hola2.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) pr2 ///
    label nomtitle keep(inst medimae l_distk URR pac_var privado indep) ///
    style(tex) title("Regressions") replace


/// TIENE IMAE

clogit choice $discrete $continuous if tiene_imae==1, group(CAPACNUM)

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
matrix rownames stats = LogDistance Quality Occupancy Insurance Nephrologist Private Independent Insurance Nephrologist Private Independent
matrix colnames stats = Mean SD Mean SD
matrix list stats

*ssc install matmap
matmap stats stats2, map(round(@, 0.01))

*ssc install estout

// Create a table using estout
esttab matrix(stats2) using "elasticities2.tex", ///
    cells(fmt(%3.3g)) ///
    label  ///
    style(tex) title("Elasticites and semielasticites") replace
