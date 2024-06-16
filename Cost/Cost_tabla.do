

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


/// GMM BASE
eststo GMM_BASE: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR),	///
instruments(mean_turnos_op mean_hab_op)

drop b_* 
drop cost
drop markup

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
instruments( mean_turnos_op mean_hab_op indep)

drop b_* 
drop cost
drop markup

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

bys ZCAIMAE: sum cost markup

table ZCAIMAE, c(mean cost mean markup)
estimates store table2
esttab table2

ssc install tabout
tabout ZCAIMAE using ///
"C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Cost\tabout.tex", ///
c(mean cost mean markup) sum bt replace style(tex) clab(Marginal_cost Markup)

/// GMM BASE + INDEP + PUBLICO
eststo GMM_INDEP_PUBLICO: gmm ( (P*dSdQ - ({a2})*share)/dSdQ - ///
{a0} - {a2}*URR - {a3}*indep - {a4}*publico),	///
instruments( mean_turnos_op mean_hab_op indep publico)

drop b_* 
drop cost
drop markup

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
instruments( mean_turnos_op mean_hab_op indep publico ceneu diaverum)

drop b_* 
drop cost
drop markup

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



////
esttab GMM_BASE GMM_INDEP GMM_INDEP_PUBLICO GMM_CHAINS  using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Cost\Cost.tex", ///
order(a0: a2: a3: a4: a5: a6:) style(tex) ///
varlabels(a0: Constant a2: Quality a3: Independent a4: Public a5: Ceneu a6: Diaverum) ///
stats(N mean_cost mean_markup, labels("Observations" "Mean cost" "Mean markup") fmt(%9.0fc %9.1fc %9.1fc)) ///
eqlabels("" "" "" "" "" "") booktab ///
title("GMM cost estimate") replace






