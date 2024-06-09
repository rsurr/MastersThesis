use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace 

/// SARI TURNOS Y HABILITADOS
*replace turnos_op = 4 if ZCAIMAE=="SARI" & anio_solicitud<2014
*replace hab_op = 10  if ZCAIMAE=="SARI" & anio_solicitud<2014


gen l_distk=log(distk)

bys ZCAIMAE anio_solicitud: egen p_count1 = sum(p_iv) if tiene_imae==1
bys ZCAIMAE anio_solicitud: egen p_count0 = sum(p_iv) if tiene_imae==0
bys ZCAIMAE anio_solicitud: egen p_count = sum(p_iv)

encode ZCAIMAE, generate(iZCAIMAE)

qui bysort anio_solicitud: egen total_obs=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n_obs=sum(choice)
gen s_obs2=n_obs/total_obs

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
gen b_URR = 2.109
gen b_n_centro = -0.108
gen b_hab_op = 0.504
gen b_turnos_op = 1.731
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
	
	
gen b_constant = ///
	n_centro*b_n_centro + hab_op*b_hab_op + ///
	turnos_op*b_turnos_op + publico*b_publico + ///
	inst*b_inst + medimae*b_medimae + l_distk*b_l_distk

collapse (mean) p_* b_* arancel share s_obs2 ///
 inst medimae distk ktv turnos_op n delta total URR hemo ///
n_centro n_trans ses_centro ses_trans s_obs privado indep publico s iZCAIMAE Zins Zmed ///
hab_op cong_op cap_op (first) chain, by(ZCAIMAE anio_solicitud)


/// Eliminar observaciones
*drop if ZCAIMAE=="CANMU" & (anio_solicitud==2014 | ///
*anio_solicitud==2015 | anio_solicitud==2016)
* 
*drop if ZCAIMAE=="CASMU" & anio_solicitud==2005

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

qui foreach x in a0: a2:{
cap drop b_`x'_0
gen b_`x'_0=_b[`x']
}
 
gen cost= b_a0 + b_a2*URR

/// a_constant 
gen a_constant = (P - cost)*dSdQ

*gen equation = a_constant - b_a1*(exp(b_constant+URR*b_URR)/

by ZCAIMAE: sum cost

br ZCAIMAE anio_solicitud cost P dSdQ URR b_URR share


collapse (mean) a_constant P dSdQ b_constant b_URR b_a0 b_a2 URR share s_obs2, by(ZCAIMAE anio_solicitud)

export delimited using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Cost\constantes.csv", replace

