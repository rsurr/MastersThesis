
use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace 



encode ZCAIMAE, generate(iZCAIMAE)

bysort ZCAIMAE anio_solicitud: egen n_choice=sum(choice)

bys ZCAIMAE anio_solicitud: egen p_count = sum(p_iv)

qui bysort anio_solicitud: egen total_obs=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n_obs=sum(choice)
gen s_obs2=n_obs/total_obs

gen l_distk=log(distk)




** Uij: tiene imae
eststo uij_tiene: clogit choice inst medimae l_distk if tiene_imae==1, group(CAPACNUM)

** Uij: no tiene imae
eststo uij_notiene: clogit choice medimae l_distk if tiene_imae==0, group(CAPACNUM)

** Full regression
eststo reg_tiene: clogit choice inst medimae l_distk URR turnos_op hab_op n_centro if tiene_imae==1, group(CAPACNUM)



collapse (mean) p_d* p_e* s_obs2 n_obs total_obs l_distk ///
p_count inst medimae distk p_iv ktv turnos_op n delta total URR hemo ///
n_centro n_trans ses_centro ses_trans s_obs privado indep publico s iZCAIMAE Zins Zmed ///
hab_op cong_op cap_op n_choice, by(ZCAIMAE anio_solicitud)

sum n_centro n_obs 
 
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


label variable inst "Insurance"
label variable medimae "Nephrologist"
label variable l_distk "Log Dist (km)"
label variable URR "Quality"
label variable turnos_op "Shifts"
label variable publico "Public"
label variable hab_op "Stations"
label variable cong_op "Congestion"
label variable turnos_op "Shifts"
label variable n_centro "Total patients"
label variable n_obs "New patients"
label variable s_obs2 "New patients share"
label variable mean_hab_op "Mean stations -j"

	
////
estimates drop st1*

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
	
esttab reg_tiene uij_tiene IV1 IV2 IV3 using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\tabla_demanda.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) order(inst medimae l_distk URR turnos_op hab_op n_centro n_obs s_obs2) ///
    label mtitles(CL CL 2SLS 2SLS 2SLS) alignment(c|cccc) ///
    style(tex) title("Demand estimates") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
esttab st1* using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\first.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) order(URR turnos_op hab_op mean_hab_op) ///
    label mtitles("Total patients" "New patients" "New patients share") ///
    style(tex) title("First stages") replace ///
	eqlabels(" " " ") booktab
	
	