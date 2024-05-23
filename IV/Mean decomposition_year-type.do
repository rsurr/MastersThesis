
use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\delta200_000.dta", replace 

** Type: tiene imae
eststo uij_tiene: clogit choice inst medimae l_distk if tiene_imae==1, group(CAPACNUM)

** Type: no tiene imae
eststo uij_notiene: clogit choice medimae l_distk if tiene_imae==0, group(CAPACNUM)
	 
collapse (mean) transp inst medimae l_distk p_iv ktv turnos n delta total URR hemo n_capacnum s_obs privado indep publico ///
pac_var s share_iv_0 share_iv_1 count_iv_0 count_iv_1 iZCAIMAE habilitados_def, ///
 by(ZCAIMAE anio_solicitud tiene_imae)
 
gen p_count=p_iv*total

by ZCAIMAE anio_solicitud: egen ntotal=sum(n)
by ZCAIMAE anio_solicitud: egen ptotal=sum(p_count)

sort ZCAIMAE tiene_imae anio_solicitud
by ZCAIMAE tiene_imae: gen p_iv_lag = p_iv[_n-1]	
by ZCAIMAE tiene_imae: gen p_count_lag = p_count[_n-1]	
by ZCAIMAE tiene_imae: gen n_capacnum_lag = n_capacnum[_n-1]
by ZCAIMAE tiene_imae: gen n_lag = n[_n-1]	
by ZCAIMAE tiene_imae: gen s_lag = s[_n-1]	
by ZCAIMAE tiene_imae: gen ntotal_lag = ntotal[_n-1]
by ZCAIMAE tiene_imae: gen ptotal_lag = ptotal[_n-1]

label variable delta "Mean utility"
label variable URR "Quality"
label variable n "Caseload"
label variable n_lag "Lagged caseload"
label variable turnos "Shifts"
label variable privado "Priv Ins"
label variable indep "Independent"
label variable publico "Public"
label variable inst "Insurance"
label variable medimae "Nephrologist"
label variable l_distk "Log Dist"
label variable habilitados_def "Stations"
label variable n_capacnum_lag "Lagged total patients"
label variable n_lag "Lagged new patients"


/// CAPACNUM - sin publico

eststo OLS: reg delta URR n_capacnum_lag habilitados_def turnos, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos (n_capacnum_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\year_type n_capacnum_lag.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
/// CAPACNUM - con publico

eststo OLS: reg delta URR n_capacnum_lag habilitados_def turnos publico, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos publico (n_capacnum_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\year_type n_capacnum_lag pub.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
/// N - sin publico

eststo OLS: reg delta URR n_lag habilitados_def turnos, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos (n_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\year_type n_lag.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
/// N - con publico

eststo OLS: reg delta URR n_lag habilitados_def turnos publico, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos publico (n_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar rkf1 = `e(rkf)': IV


esttab uij_tiene uij_notiene OLS IV using "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\IV\year_type n_lag pub.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 rkf1, labels("Observations" "CD Wald F" "KP Wald rk F") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
