use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\2loops.dta", replace

*encode ZCAIMAE, generate(iZCAIMAE)
*gen l_distk=log(distk)
*bysort ZCAIMAE anio_solicitud: egen delta0=mean(delta)

global max 1000
global tol 10^(-8)
global b_tol 10^(-8)


forvalues k = 0(1)$max {

cap drop b_medimae0
qui gen b_medimae0=b_medimae

*Tiene imae
constraint 1 delta0 = 1
eststo uij_tiene: clogit choice inst medimae l_distk delta0 if tiene_imae==1, group(CAPACNUM) constraint(1)
qui foreach x in inst medimae l_distk{
replace b_`x'=_b[`x'] if tiene_imae==1
}

*No tiene imae
constraint 1 delta0 = 1
eststo uij_notiene: clogit choice medimae l_distk delta0 if tiene_imae==0, group(CAPACNUM) constraint(1)
qui foreach x in medimae l_distk{
replace b_`x'=_b[`x'] if tiene_imae==0
}
replace b_inst=0 if tiene_imae==0


qui forvalues i = 0(1)$max {
		  cap drop eV
		  qui gen eV=exp(delta + inst*b_inst + medimae*b_medimae + l_distk*b_l_distk)
		  
		  cap drop sum_eV
		  qui bysort CAPACNUM: egen sum_eV=sum(eV)
          
		  cap drop s
		  qui gen s=eV/(1+sum_eV)
		  
		  cap drop new_delta
          qui gen new_delta= delta + log(s_obs) - log(s)

		  cap drop epsilon
		  qui gen epsilon=abs(new_delta - delta)
		  
		  cap drop max_epsilon
		  qui egen max_epsilon=max(epsilon)
		  
		  qui replace delta=new_delta
		  cap drop delta0
		  bysort ZCAIMAE anio_solicitud: egen delta0=mean(delta)
		   
		  if max_epsilon<$tol {
		  qui gen inner_fixed_point=1
		  display "Inner fixed point reached"
		  continue, break
		  }
		  }

cap drop b_epsilon
qui gen  b_epsilon=abs(b_medimae0 - b_medimae)

cap drop max_b_epsilon
qui egen max_b_epsilon=max(b_epsilon)
		  
if max_b_epsilon<$b_tol {
		  qui gen outer_fixed_point=1
		  display "Outer fixed point reached"
		  continue, break
		  }
		  }

---		  
* Con 1000 iteraciones: max_epsilon=4.77e-06, max_b_epsilon=4.03e-06


save "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\2loops_2.dta", replace
		  
---

use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\2loops_2.dta", replace

bysort ZCAIMAE anio_solicitud: egen delta0=mean(delta)

*Tiene imae
constraint 1 delta0 = 1
eststo uij_tiene: clogit choice inst medimae l_distk delta0 if tiene_imae==1, group(CAPACNUM) constraint(1)

*No tiene imae
constraint 1 delta0 = 1
eststo uij_notiene: clogit choice medimae l_distk delta0 if tiene_imae==0, group(CAPACNUM) constraint(1)


bysort ZCAIMAE anio_solicitud: egen n_choice=sum(choice)
qui bysort anio_solicitud: egen total_obs=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n_obs=sum(choice)
gen s_obs2=n_obs/total_obs

collapse (mean) delta URR turnos_op hab_op cong_op n_centro n_choice total_obs n_obs s_obs2, by(ZCAIMAE anio_solicitud)
 
egen total_cong = total(cong_op), by(anio_solicitud)
egen count_cong = count(cong_op), by(anio_solicitud)
gen mean_cong_op = (total_cong - cong_op)/(count_cong - 1)

egen total_n_centro = total(n_centro), by(anio_solicitud)
egen count_n_centro = count(n_centro), by(anio_solicitud)
gen mean_n_centro = (total_n_centro - n_centro)/(count_n_centro - 1)

egen total_hab_op = total(hab_op), by(anio_solicitud)
egen count_hab_op = count(hab_op), by(anio_solicitud)
gen mean_hab_op = (total_hab_op - hab_op)/(count_hab_op - 1)

egen total_turnos_op = total(turnos_op), by(anio_solicitud)
egen count_turnos_op = count(turnos_op), by(anio_solicitud)
gen mean_turnos_op = (total_turnos_op - turnos_op)/(count_turnos_op - 1)


drop if ZCAIMAE=="CANMU" & (anio_solicitud==2014 | ///
anio_solicitud==2015 | anio_solicitud==2016)
 
drop if ZCAIMAE=="CASMU" & anio_solicitud==2005


*label variable inst "Insurance"
*label variable medimae "Nephrologist"
*label variable l_distk "Log Dist (km)"
*label variable URR "Quality"
*label variable turnos_op "Shifts"
*label variable publico "Public"
*label variable hab_op "Stations"
*label variable cong_op "Congestion"
*label variable turnos_op "Shifts"
*label variable n_centro "Total patients"
*label variable n_obs "New patients"
*label variable s_obs2 "New patients share"
*label variable mean_hab_op "Mean stations -j"

	
////

eststo IV1: ivreg2 delta URR turnos_op hab_op (n_centro = mean_hab_op), nocons ///
first savefirst r savefprefix(st1)
estadd scalar cdf1 =  `e(cdf)': IV1
estadd scalar rkf1 = `e(rkf)': IV1

eststo IV2: ivreg2 delta URR turnos_op hab_op (n_obs = mean_hab_op), nocons  ///
first savefirst r savefprefix(st1)
estadd scalar cdf1 =  `e(cdf)': IV2
estadd scalar rkf1 = `e(rkf)': IV2

eststo IV3: ivreg2 delta URR turnos_op hab_op (s_obs2 = mean_hab_op), nocons  ///
first savefirst r savefprefix(st1)
estadd scalar cdf1 =  `e(cdf)': IV3
estadd scalar rkf1 = `e(rkf)': IV3

/// ESTTAB
	
esttab uij_tiene uij_notiene IV1 IV2 IV3 using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\tabla_demanda.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) order(inst medimae l_distk URR turnos_op hab_op n_centro n_obs s_obs2) ///
    label mtitles(CL CL 2SLS 2SLS 2SLS) alignment(c|cccc) ///
    style(tex) title("Demand estimates") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
/// ESTTAB
	
esttab uij_tiene uij_notiene IV1 using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\tabla_demanda_simple.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) order(inst medimae l_distk URR turnos_op hab_op n_centro) drop(delta0) ///
    label mtitles(CL CL 2SLS 2SLS 2SLS) alignment(c|cccc) ///
    style(tex) title("Demand estimates") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	