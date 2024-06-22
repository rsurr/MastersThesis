

use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\2loops_2.dta", replace 

** Mean decomposition
gen b_URR = 2.520
gen b_n_centro = -0.131
gen b_hab_op = 0.595
gen b_turnos_op = 1.981
gen b_publico = 0
	
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
	
/// Compute partial S partial Q
gen dSdQ = b_URR*share*(1-share)

collapse (mean) p_* b_* share arancel dSdQ ///
 inst medimae distk ktv turnos_op n delta total URR hemo ///
n_centro n_trans ses_centro ses_trans s_obs privado indep publico s iZCAIMAE Zins Zmed ///
hab_op cong_op cap_op (first) chain, by(ZCAIMAE anio_solicitud)


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


/// GMM BASE
eststo GMM_BASE: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR),	///
instruments(p_diab p_devp p_dcisq)

cap drop b_* 
cap drop cost
cap drop markup

qui foreach x in a0: a2:{
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR

sum cost

estadd scalar mean_cost = `r(mean)': GMM_BASE

gen markup=P/cost

sum markup

estadd scalar mean_markup = `r(mean)': GMM_BASE


/// GMM BASE + INDEP
eststo GMM_INDEP: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR - {a3}*indep),	///
instruments(p_diab p_devp p_dcisq indep)

cap drop b_* 
cap drop cost
cap drop markup

qui foreach x in a0: a2: a3: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR + b_a3*indep 

sum cost

estadd scalar mean_cost = `r(mean)': GMM_INDEP

gen markup=P/cost

sum markup

estadd scalar mean_markup = `r(mean)': GMM_INDEP

ssc install tabout
tabout ZCAIMAE using ///
"C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Cost\tabout.tex", ///
c(mean cost mean markup) sum bt replace style(tex) clab(Marginal_cost Markup)

/// GMM BASE + INDEP + PUBLICO
eststo GMM_INDEP_PUBLICO: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR - {a3}*indep - {a4}*publico),	///
instruments(p_diab p_devp p_dcisq indep publico)

cap drop b_* 
cap drop cost
cap drop markup

qui foreach x in a0: a2: a3: a4: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR + b_a3*indep + b_a4*publico 

sum cost

estadd scalar mean_cost = `r(mean)': GMM_INDEP_PUBLICO

gen markup=P/cost

sum markup

estadd scalar mean_markup = `r(mean)': GMM_INDEP_PUBLICO


/// GMM BASE + INDEP + PUBLICO + CENEU + DIAVERUM
eststo GMM_CHAINS: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR - {a3}*indep - {a4}*publico - {a5}*ceneu - {a6}*diaverum),	///
instruments(p_diab p_devp p_dcisq indep publico ceneu diaverum)

cap drop b_* 
cap drop cost
cap drop markup

qui foreach x in a0: a2: a3: a4: a5: a6: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR + b_a3*indep + b_a4*publico + b_a5*ceneu + b_a6*diaverum 

sum cost

estadd scalar mean_cost = `r(mean)': GMM_CHAINS

gen markup=P/cost

sum markup

estadd scalar mean_markup = `r(mean)': GMM_CHAINS

/// GMM CON QUALITY A LA ALPHA
eststo GMM_QUALITY: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR - n_centro^{a8}),	///
instruments(p_diab p_devp p_dcisq)

cap drop b_* 
cap drop cost
cap drop markup

qui foreach x in a0: a2: a8: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR + n_centro^b_a8

sum cost

estadd scalar mean_cost = `r(mean)': GMM_QUALITY

gen markup=P/cost

sum markup

estadd scalar mean_markup = `r(mean)': GMM_QUALITY


/// GMM CON TIME FIXED EFFECTS
tabulate anio_solicitud, generate(anio)

eststo GMM_TIME: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR - ///
{a3}*anio1- {a4}*anio2 - {a5}*anio3 - {a6}*anio4 - {a7}*anio5 - /// 
{a8}*anio6 - {a9}*anio7 - {a10}*anio8 - {a11}*anio9 - {a12}*anio10 - ///
{a13}*anio11 - {a14}*anio12 - {a15}*anio13 - {a16}*anio14),	///
instruments(p_diab p_devp p_dcisq anio1 anio2 anio3 anio4 anio5 anio6 ///
 anio7 anio8 anio9 anio10 anio11 anio12 anio13 anio14)

cap drop b_* 
cap drop cost
cap drop markup

qui foreach x in a0: a2: a3: {
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR + b_a3*anio_solicitud

sum cost

estadd scalar mean_cost = `r(mean)': GMM_TIME

gen markup=P/cost

sum markup

estadd scalar mean_markup = `r(mean)': GMM_TIME

////
esttab GMM_BASE GMM_QUALITY GMM_INDEP GMM_INDEP_PUBLICO GMM_CHAINS  using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Cost\Cost.tex", ///
order(a0: a2: a3: a4: a5: a6:) style(tex) ///
varlabels(a0: Constant a2: Quality a3: Independent a4: Public a5: Ceneu a6: Diaverum) ///
stats(N mean_cost mean_markup, labels("Observations" "Mean cost" "Mean markup") fmt(%9.0fc %9.1fc %9.1fc)) ///
eqlabels("" "" "" "" "" "") booktab ///
title("GMM cost estimate") replace




