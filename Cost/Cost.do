
use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace 



gen l_distk=log(distk)

bys ZCAIMAE anio_solicitud: egen p_count = sum(p_iv)

encode ZCAIMAE, generate(iZCAIMAE)

bysort ZCAIMAE anio_solicitud: egen n_choice=sum(choice)

** Type: tiene imae
eststo uij: clogit choice inst medimae l_distk if tiene_imae==1, group(CAPACNUM)

qui foreach x in inst medimae l_distk {
cap drop b_`x'_1
gen b_`x'_1=_b[`x']
}

** Type: no tiene imae
eststo uij_notiene: clogit choice medimae l_distk if tiene_imae==0, group(CAPACNUM)

qui foreach x in medimae l_distk {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}


collapse (mean) p_d* p_e* b_*  ///
p_count inst medimae distk p_iv ktv turnos_op n delta total URR hemo ///
n_centro n_trans ses_centro ses_trans s_obs privado indep publico s iZCAIMAE Zins Zmed ///
hab_op cong_op cap_op n_choice, by(ZCAIMAE anio_solicitud)
 
/// INSTRUMENTOS BLP
* Congestion
egen total_cong = total(cong_op), by(anio_solicitud)
egen count_cong = count(cong_op), by(anio_solicitud)
gen mean_cong_op = (total_cong - cong_op)/(count_cong - 1)

* Patients
egen total_n_centro = total(n_centro), by(anio_solicitud)
egen count_n_centro = count(n_centro), by(anio_solicitud)
gen mean_n_centro = (total_n_centro - n_centro)/(count_n_centro - 1)

* Stations
egen total_hab_op = total(hab_op), by(anio_solicitud)
egen count_hab_op = count(hab_op), by(anio_solicitud)
gen mean_hab_op = (total_hab_op - hab_op)/(count_hab_op - 1)

* Shifts
egen total_turnos_op = total(turnos_op), by(anio_solicitud)
egen count_turnos_op = count(turnos_op), by(anio_solicitud)
gen mean_turnos_op = (total_turnos_op - turnos_op)/(count_turnos_op - 1)


/// Eliminar observaciones
*drop if ZCAIMAE=="CANMU" & (anio_solicitud==2014 | ///
*anio_solicitud==2015 | anio_solicitud==2016)
* 
*drop if ZCAIMAE=="CASMU" & anio_solicitud==2005

/// IV
eststo OLS: reg delta URR n_centro hab_op turnos_op publico, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op publico (n_centro = mean_hab_op mean_turnos_op), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV

qui foreach x in URR turnos_op hab_op publico {
cap drop b_`x'
gen b_`x'=_b[`x']
}

esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z_BLP.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
	
----


use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace 

gen l_distk=log(distk)

bys ZCAIMAE anio_solicitud: egen p_count1 = sum(p_iv) if tiene_imae==1
bys ZCAIMAE anio_solicitud: egen p_count0 = sum(p_iv) if tiene_imae==0
bys ZCAIMAE anio_solicitud: egen p_count = sum(p_iv)

encode ZCAIMAE, generate(iZCAIMAE)

bysort ZCAIMAE anio_solicitud: egen n_choice=sum(choice)

drop b_*

** Uij: tiene imae
eststo uij: clogit choice inst medimae l_distk if tiene_imae==1, group(CAPACNUM)

qui foreach x in inst medimae l_distk {
gen b_`x'=_b[`x']
}

** Uij: no tiene imae
eststo uij_notiene: clogit choice medimae l_distk if tiene_imae==0, group(CAPACNUM)

qui foreach x in medimae l_distk {
replace b_`x'=_b[`x'] if tiene_imae==0
}
replace b_inst=0 if tiene_imae==0

** Mean decomposition
gen b_URR = 2.940758
gen b_n_centro = -.1205158
gen b_hab_op = .5332263
gen b_turnos_op = 1.702969
gen b_publico = -.2192158
	
/// Compute shares
	cap drop eV
	gen eV= ///
	exp(URR*b_URR + n_centro*b_n_centro + hab_op*b_hab_op + ///
	turnos_op*b_turnos_op + publico*b_publico + ///
	inst*b_inst + medimae*b_medimae + l_distk*b_l_distk)
	
	cap drop sum_eV
	qui bysort CAPACNUM: egen sum_eV=sum(eV)
		
	cap drop share
	qui gen share=eV/(1+sum_eV)	
	

collapse (mean) p_* b_* share arancel ///
 inst medimae distk ktv turnos_op n delta total URR hemo ///
n_centro n_trans ses_centro ses_trans s_obs privado indep publico s iZCAIMAE Zins Zmed ///
hab_op cong_op cap_op n_choice (first) chain, by(ZCAIMAE anio_solicitud)


/// Eliminar observaciones
*drop if ZCAIMAE=="CANMU" & (anio_solicitud==2014 | ///
*anio_solicitud==2015 | anio_solicitud==2016)
* 
*drop if ZCAIMAE=="CASMU" & anio_solicitud==2005
*
gen URR2=URR^2
gen P=arancel

gen diaverum=0
replace diaverum=1 if chain=="DIAVERUM"

gen ceneu=0
replace ceneu=1 if chain=="CENEU"



/// INSTRUMENTOS BLP
* Congestion
egen total_cong = total(cong_op), by(anio_solicitud)
egen count_cong = count(cong_op), by(anio_solicitud)
gen mean_cong_op = (total_cong - cong_op)/(count_cong - 1)

* Patients
egen total_n_centro = total(n_centro), by(anio_solicitud)
egen count_n_centro = count(n_centro), by(anio_solicitud)
gen mean_n_centro = (total_n_centro - n_centro)/(count_n_centro - 1)

* Stations
egen total_hab_op = total(hab_op), by(anio_solicitud)
egen count_hab_op = count(hab_op), by(anio_solicitud)
gen mean_hab_op = (total_hab_op - hab_op)/(count_hab_op - 1)

* Shifts
egen total_turnos_op = total(turnos_op), by(anio_solicitud)
egen count_turnos_op = count(turnos_op), by(anio_solicitud)
gen mean_turnos_op = (total_turnos_op - turnos_op)/(count_turnos_op - 1)

/// Compute partial S partial Q
gen dSdQ = b_URR*share*(1-share)


/// GMM
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*publico ),	///
instruments(mean_hab_op mean_turnos_op mean_n_centro publico)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*publico

by ZCAIMAE: sum cost
 

 
 
/// GMM 2
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*publico - {a5}*indep ),	///
instruments(mean_hab_op mean_turnos_op mean_n_centro publico indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: a5: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
} 

gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*privado + b_a5*indep

by ZCAIMAE: sum cost


/// GMM 3
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR*cong_op - {a3}*URR2*cong_op - {a4}*publico - {a5}*indep ),	///
instruments(mean_hab_op mean_turnos_op mean_n_centro publico indep)

drop b_* cost

qui foreach x in a1: a2: a3: a4: a5: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
} 

gen cost= b_a1*n_centro + b_a2*URR*cong_op + b_a3*URR2*cong_op + b_a4*privado + b_a5*indep

by ZCAIMAE: sum cost


/// GMM 4
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR*cong_op - {a4}*publico ),	///
instruments(mean_hab_op mean_turnos_op mean_n_centro publico)

drop b_* 
drop cost

qui foreach x in a1: a2: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR*cong_op + b_a4*publico

by ZCAIMAE: sum cost


/// GMM 5
gmm ( (P*dSdQ - ({a2}*cong_op +2*{a3}*cong_op)*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR*cong_op - {a3}*URR2*cong_op - {a4}*publico ),	///
instruments(mean_hab_op mean_turnos_op mean_n_centro publico indep)

drop b_* cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
} 

gen cost= b_a1*cong_op + b_a2*URR*cong_op + b_a3*URR2*cong_op + b_a4*privado

by ZCAIMAE: sum cost


/// GMM 6
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*publico ),	///
instruments(mean_hab_op mean_turnos_op publico)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*publico

by ZCAIMAE: sum cost

/// GMM 7
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*indep ),	///
instruments(mean_hab_op mean_turnos_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 8
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*privado ),	///
instruments(mean_hab_op mean_turnos_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*privado

by ZCAIMAE: sum cost

/// GMM 9
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2),	///
instruments(mean_hab_op mean_turnos_op)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2

by ZCAIMAE: sum cost


/// GMM 10
gmm ( (P*dSdQ - ({a2}*cong_op +2*{a3}*cong_op)*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR*cong_op - {a3}*URR2*cong_op),	///
instruments(mean_hab_op mean_turnos_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR*cong_op + b_a3*URR2*cong_op

by ZCAIMAE: sum cost


/// GMM 10
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR),	///
instruments(mean_hab_op mean_turnos_op)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR*cong_op + b_a3*URR2*cong_op

by ZCAIMAE: sum cost


/// GMM 11
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2),	///
instruments(mean_hab_op mean_turnos_op)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2

by ZCAIMAE: sum cost


/// GMM 12
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*hab_op - {a2}*URR - {a3}*URR2),	///
instruments(hab_op mean_turnos_op mean_hab_op)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*hab_op + b_a2*URR + b_a3*URR2

by ZCAIMAE: sum cost


/// GMM 12
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*hab_op - {a2}*URR - {a3}*URR2 - {a4}*indep),	///
instruments(hab_op mean_turnos_op mean_hab_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*hab_op + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 12
gmm ( (P*dSdQ - ({a2}*cong_op +2*{a3}*cong_op)*share)/dSdQ - ///
{a1}*hab_op - {a2}*URR*cong_op - {a3}*URR2*cong_op - {a4}*indep),	///
instruments(hab_op mean_turnos_op mean_hab_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*hab_op + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 12
gen hab_op2=hab_op^2

gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*hab_op - {a5}*hab_op2 - {a2}*URR - {a3}*URR2 - {a4}*indep),	///
instruments(URR URR2 mean_turnos_op mean_hab_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: a5: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*hab_op + b_a5*hab_op2 + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 12
gen hab_op2=hab_op^2

gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*hab_op - {a5}*hab_op2 - {a2}*URR - {a3}*URR2 - {a4}*indep),	///
instruments(hab_op mean_turnos_op mean_hab_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: a5: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*hab_op + b_a5*hab_op2 + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 13
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*indep),	///
instruments(mean_turnos_op mean_hab_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: a5: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*hab_op + b_a5*hab_op2 + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 14
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*n_centro - {a2}*URR - {a3}*URR2 - {a4}*indep),	///
instruments(mean_turnos_op p_count indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*n_centro + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 15
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*publico ),	///
instruments(mean_turnos_op p_count publico)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*publico

by ZCAIMAE: sum cost


/// GMM 16
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*indep ),	///
instruments(p_dcisq p_diab indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 17
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*n_centro - {a2}*URR - {a3}*URR2 - {a4}*indep ),	///
instruments(p_dcisq p_diab indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*n_centro + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 18
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*indep ),	///
instruments(p_devp p_diab indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 18
gmm ( (P*dSdQ - ({a2} +2*{a3})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a3}*URR2 - {a4}*indep ),	///
instruments(p_dcisq p_devp p_diab indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a3*URR2 + b_a4*indep

by ZCAIMAE: sum cost


/// GMM 18
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a4}*indep ),	///
instruments(p_dcisq p_devp p_diab indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a4*indep

gen markup=P/cost

by ZCAIMAE: sum cost markup



/// GMM 18
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a4}*indep ),	///
instruments(mean_hab_op mean_turnos_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a4*indep

gen markup=P/cost

by ZCAIMAE: sum cost markup


/// GMM 19
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a4}*indep ),	///
instruments(p_count mean_turnos_op indep)

drop b_* 
drop cost

qui foreach x in a1: a2: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a4*indep

gen markup=P/cost

by ZCAIMAE: sum cost markup


/// GMM 20
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a1}*cong_op - {a2}*URR - {a4}*indep ),	///
instruments(mean_n_centro mean_turnos_op indep)

drop b_* 
drop cost
drop markup

qui foreach x in a1: a2: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a1*cong_op + b_a2*URR + b_a4*indep

gen markup=P/cost

by ZCAIMAE: sum cost markup


/// GMM 21
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a1}*cong_op - {a2}*URR - {a4}*indep ),	///
instruments(mean_n_centro mean_turnos_op indep)

drop b_* 
drop cost
drop markup

qui foreach x in a0: a1: a2: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a1*cong_op + b_a2*URR + b_a4*indep

gen markup=P/cost

by ZCAIMAE: sum cost markup


/// GMM 22
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR - {a4}*indep ),	///
instruments(mean_n_centro mean_turnos_op indep)

drop b_* 
drop cost
drop markup

qui foreach x in a0: a2: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR + b_a4*indep

gen markup=P/cost

by ZCAIMAE: sum cost markup


/// GMM 23
gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR),	///
instruments(mean_hab_op mean_turnos_op)

drop b_* 
drop cost
drop markup

qui foreach x in a0: a2:{
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR

gen markup=P/cost

by ZCAIMAE: sum cost markup



