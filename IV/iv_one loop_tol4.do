use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace
encode ZCAIMAE, generate(iZCAIMAE)
gen l_distk=log(distk)
gen publico=0
replace publico=1 if chain=="PUBLICO"
gen privado=0
replace privado=1 if chain=="PRIVADO"
gen indep=0
replace indep=1 if tipo_imae=="INDEPENDIENTE"

label variable inst "Insurance"
label variable medimae "Nephrologist"
label variable l_distk "Log Dist"

gen delta=.
gen b_inst=.
gen b_medimae=.
gen b_l_distk=.

* Initial guess for delta
clogit choice inst medimae l_distk i.iZCAIMAE, group(CAPACNUM) 
levelsof iZCAIMAE if e(sample), local(Zlevels)
 foreach d of local Zlevels {
    replace delta = _b[`d'.iZCAIMAE] if iZCAIMAE == `d'
 } 

* Betas Uij
** Type: tiene imae
eststo uij_tiene: clogit choice inst medimae l_distk if tiene_imae==1, group(CAPACNUM)
qui foreach x in inst medimae l_distk{
replace b_`x'=_b[`x'] if tiene_imae==1
}
** Type: no tiene imae
eststo uij_notiene: clogit choice medimae l_distk if tiene_imae==0, group(CAPACNUM)
qui foreach x in medimae l_distk{
replace b_`x'=_b[`x'] if tiene_imae==0
}
replace b_inst=0 if tiene_imae==0

*foreach t in 0 1 {
*clogit choice inst medimae l_distk if tiene_imae==`t', group(CAPACNUM)
*predict p_iv_`t' if tiene_imae==`t'
*bys CAPACNUM: egen max_p_iv_`t' = max(p_iv_`t')
*gen choice_iv_`t' = 1 if p_iv_`t'==max_p_iv_`t' & p_iv_`t'!=.
*replace choice_iv_`t' = 0 if p_iv_`t'!=max_p_iv_`t' & p_iv_`t'!=.
*bys ZCAIMAE: egen share_iv_`t'=sum(choice_iv_`t')
*}

save "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\delta_Logit_INGRESOS.dta", replace

clear all

global max 200000
global tol 10^(-14)

tempfile building
save `building', emptyok

local anios 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
local tipos 0 1

foreach a of local anios {

foreach t of local tipos {

qui use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\delta_Logit_INGRESOS.dta", replace
qui keep if anio_solicitud==`a' & tiene_imae==`t'

qui egen total=sum(choice)
qui bysort ZCAIMAE: egen n=sum(choice)
qui gen s_obs=n/total
*replace s_obs=. if s_obs==0

forvalues i = 0(1)$max {
		  
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
		  
		  if max_epsilon<$tol {
		  qui gen fixed_point=1
		  display "fixed point reached `a'"
		  continue, break
		  }
		  		  
		  }
		  
		  
// INSTRUMENTO VARIANDO POR AÑO
		  if `t'==1 {
		  qui clogit choice inst medimae l_distk, group(CAPACNUM)
		  }
		  else {
		  qui clogit choice medimae l_distk, group(CAPACNUM)
		  }
		  
		   	qui predict p_iv
			qui bys CAPACNUM: egen max_p_iv = max(p_iv)
			qui gen choice_iv = 1 if p_iv==max_p_iv & p_iv!=.
			qui replace choice_iv = 0 if p_iv!=max_p_iv & p_iv!=.
			qui bys ZCAIMAE: egen count_iv_`t'=sum(choice_iv)		  
			qui egen total_iv=sum(count_iv_`t')
			qui gen share_iv_`t'=count_iv_`t'/total_iv	  
		  

   append using `building'
   save `"`building'"', replace
   

		  }
		  }
		  

  
---

use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\delta200_000.dta", replace 

label variable inst "Insurance"
label variable medimae "Nephrologist"
label variable l_distk "Log Dist"

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

/// CAPACNUM - sin publico

eststo OLS: reg delta URR n_capacnum_lag habilitados_def turnos, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos (n_capacnum_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar sstat1 = `e(sstat)': IV


esttab uij_tiene uij_notiene OLS IV using "iv_reg n_capacnum_lag.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 sstat1, labels("Observations" "CD Wald F" "SW S stat.") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
/// CAPACNUM - con publico

eststo OLS: reg delta URR n_capacnum_lag habilitados_def turnos publico, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos publico (n_capacnum_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar sstat1 = `e(sstat)': IV


esttab uij_tiene uij_notiene OLS IV using "iv_reg n_capacnum_lag pub.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 sstat1, labels("Observations" "CD Wald F" "SW S stat.") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
/// N - sin publico

eststo OLS: reg delta URR n_lag habilitados_def turnos, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos (n_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar sstat1 = `e(sstat)': IV


esttab uij_tiene uij_notiene OLS IV using "iv_reg n_lag", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 sstat1, labels("Observations" "CD Wald F" "SW S stat.") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
	
/// N - con publico

eststo OLS: reg delta URR n_lag habilitados_def turnos publico, nocons

eststo IV: ivreg2 delta URR habilitados_def turnos publico (n_lag = ptotal_lag), nocons  ///
ffirst savefirst r savefprefix(st1)
qui estadd scalar cdf1 =  `e(cdf)': IV
qui estadd scalar sstat1 = `e(sstat)': IV


esttab uij_tiene uij_notiene OLS IV using "iv_reg n_lag pub.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber mtitles(CL CL OLS 2SLS) ///
    style(tex) title("Patient-specific coefficients and mean decomposition") replace ///
	stats(N cdf1 sstat1, labels("Observations" "CD Wald F" "SW S stat.") fmt(%9.0fc %9.3fc %9.3fc)) ///
	eqlabels(" " " ") booktab
