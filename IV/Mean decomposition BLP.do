
use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace 

encode ZCAIMAE, generate(iZCAIMAE)

bysort ZCAIMAE anio_solicitud: egen n_choice=sum(choice)

bys ZCAIMAE anio_solicitud: egen p_count = sum(p_iv)

qui bysort anio_solicitud: egen total_obs=sum(choice)
qui bysort ZCAIMAE anio_solicitud: egen n_obs=sum(choice)
gen s_obs2=n_obs/total_obs

collapse (mean) p_d* p_e* s_obs2 n_obs total_obs ///
p_count inst medimae distk p_iv ktv turnos_op n delta total URR hemo ///
n_centro n_trans ses_centro ses_trans s_obs privado indep publico s iZCAIMAE Zins Zmed ///
hab_op cong_op cap_op n_choice, by(ZCAIMAE anio_solicitud)
 
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

sort ZCAIMAE anio_solicitud
by ZCAIMAE : gen n_choice_lag = n_choice[_n-1]	
by ZCAIMAE : gen cong_op_lag = cong_op[_n-1]	

drop if ZCAIMAE=="CANMU" & (anio_solicitud==2014 | ///
anio_solicitud==2015 | anio_solicitud==2016)
 
drop if ZCAIMAE=="CASMU" & anio_solicitud==2005


eststo OLS: reg delta URR cong_op turnos_op i.iZCAIMAE, nocons

eststo IV: ivreg2 delta turnos_op cong_op i.iZCAIMAE (URR = p_count ktv), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) drop(*iZCAIMAE) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
	
////

eststo OLS: reg delta URR turnos_op hab_op n_centro publico, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op publico (n_centro = ses_trans), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
	
////

eststo OLS: reg delta URR n_centro hab_op turnos_op, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op (n_centro = mean_n_centro), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
	
////

eststo OLS: reg delta URR n_centro hab_op turnos_op, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op (n_centro = mean_hab_op), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	

////

eststo OLS: reg delta URR n_centro hab_op turnos_op, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op (n_centro =  mean_turnos_op), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	

////

eststo OLS: reg delta URR n_centro hab_op turnos_op publico, nocons

eststo IV: ivreg2 delta turnos_op hab_op publico (URR n_centro = mean_hab_op mean_turnos_op p_count), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
	
////

eststo OLS: reg delta URR n_centro hab_op turnos_op, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op (n_centro =  p_count), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	

////


eststo OLS: reg delta URR n_centro hab_op turnos_op, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op (n_centro = mean_hab_op), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
	
////


eststo OLS: reg delta URR n_centro hab_op turnos_op, nocons

eststo IV: ivreg2 delta URR turnos_op hab_op (s_obs2 = mean_hab_op), nocons  ///
first savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z_s_obs.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
	
	
////


eststo IV1: ivreg2 delta URR turnos_op hab_op (n_centro = mean_hab_op), nocons  ///
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


esttab uij_tiene IV1 IV2 IV3 using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\Z_n_obs_n_centro.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) order(inst medimae l_distk URR turnos_op hab_op n_centro n_obs s_obs2) ///
    label nonumber mtitles(CL 2SLS 2SLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	