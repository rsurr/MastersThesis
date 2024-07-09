

use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\2loops_2.dta", replace 
drop if anio_solicitud==2003

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
	

bysort ZCAIMAE anio_solicitud: egen n_choice=sum(choice)
qui bysort anio_solicitud: egen total_obs=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n_obs=sum(choice)
gen s_obs2=n_obs/total_obs
	

/// Compute partial S partial Q
gen dSdQ = b_URR*share*(1-share)

/// Compute partial S partial Q cruzado
destring ID_CAIMAE, replace
egen ID_CAIMAE_t=concat(ID_CAIMAE anio_solicitud)
destring ID_CAIMAE_t, replace
levelsof ID_CAIMAE_t, local(imaes)
foreach l of local imaes {

*Creating s_`l'
bysort CAPACNUM: gen aux_`l'=share if ID_CAIMAE_t==`l'
bysort CAPACNUM: egen s_`l'=max(aux_`l')
drop aux_`l'

*Creating dSdQ_`l'
gen dSdQ_`l' = -b_URR*share*s_`l' if ID_CAIMAE_t!=`l'
replace dSdQ_`l' = dSdQ if ID_CAIMAE_t==`l'

drop s_`l'
}

gen URR2=URR^2
gen P=arancel

gen diaverum=0
replace diaverum=1 if chain=="DIAVERUM"

gen ceneu=0
replace ceneu=1 if chain=="CENEU"

collapse (mean) P share s_obs2 dSdQ_* (first) chain num_choice ID_CAIMAE p_* URR n_centro indep publico, by(ZCAIMAE anio_solicitud)

export delimited using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Cost\matrices_t.csv", replace

/// GMM BASE
eststo GMM_BASE: gmm ( P*dSdQ - ({a2})*share)/dSdQ - ///
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


