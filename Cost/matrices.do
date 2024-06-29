

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
	

bysort ZCAIMAE anio_solicitud: egen n_choice=sum(choice)
qui bysort anio_solicitud: egen total_obs=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n_obs=sum(choice)
gen s_obs2=n_obs/total_obs
	

/// Compute partial S partial Q
gen dSdQ = b_URR*share*(1-share)

/// Compute partial S partial Q cruzado
destring ID_CAIMAE, replace
levelsof ID_CAIMAE, local(imaes)
foreach l of local imaes {

*Creating s_`l'
bysort CAPACNUM: gen aux_`l'=share if ID_CAIMAE==`l'
bysort CAPACNUM: egen s_`l'=max(aux_`l')
drop aux_`l'

*Creating dSdQ_`l'
gen dSdQ_`l' = -b_URR*share*s_`l' if ID_CAIMAE!=`l'
replace dSdQ_`l' = dSdQ if ID_CAIMAE==`l'

drop s_`l'
}

gen URR2=URR^2
gen P=arancel

gen diaverum=0
replace diaverum=1 if chain=="DIAVERUM"

gen ceneu=0
replace ceneu=1 if chain=="CENEU"

collapse (mean) P share s_obs2 dSdQ_* URR (first) chain ID_CAIMAE, by(ZCAIMAE anio_solicitud)

export delimited using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Cost\matrices.csv", replace

