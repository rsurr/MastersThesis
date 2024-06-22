use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace
encode ZCAIMAE, generate(iZCAIMAE)
gen l_distk=log(distk)

gen choice_iv=.
gen share_iv=.

keep if anio_solicitud==2012

*egen total=sum(choice)
*bysort ZCAIMAE: gen n=sum(choice)
*gen s_obs=n/total
*replace s_obs=. if s_obs==0
*
*gen delta=.
*gen b_inst=.
*gen b_medimae=.
*gen b_l_distk=.

clogit choice inst medimae l_distk i.iZCAIMAE, group(CAPACNUM) 
levelsof iZCAIMAE if e(sample), local(Zlevels)
 foreach d of local Zlevels {
    replace delta = _b[`d'.iZCAIMAE] if iZCAIMAE == `d'
 }

gen delta0=delta

forvalues k = 0(1)200 {
constraint 1 delta0 = 1
clogit choice inst medimae l_distk delta0, group(CAPACNUM) constraint(1)

qui foreach x in inst medimae l_distk{
replace b_`x'=_b[`x']
}

*qui foreach d of local Zlevels {
*    replace delta0 = _b[delta]
* }
*
qui forvalues i = 0(1)200 {
		  capture drop eV_`i'
		  gen eV_`i'=exp(delta`i' + inst*b_inst + medimae*b_medimae + l_distk*b_l_distk)
		  capture drop sum_eV_`i'
		  bysort CAPACNUM: egen sum_eV_`i'=sum(eV_`i')
		  capture drop s_`i'
		  gen s_`i'=eV_`i'/(1+sum_eV_`i')
          local j = `i'+1
		  capture drop delta`j'
          gen delta`j'=delta`i'+ log(s_obs) - log(s_`i')
		  capture drop epsilon_`i'
		  gen epsilon_`i'=abs(delta`j'-delta`i')
		  capture drop max_epsilon_`i'
		  egen max_epsilon_`i'=max(epsilon_`i')
		  
		  if max_epsilon_`i'<50*10^(-7) {
		  replace delta0 = delta`j'
		  display "fixed point reached"
		  continue, break
		  }
		  }
		  }
		 
clogit choice inst medimae l_distk if anio_solicitud==2008, group(CAPACNUM) 
predict p_iv
bys CAPACNUM: egen max_p_iv = max(p_iv)
replace choice_iv = 1 if p_iv==max_p_iv & p_iv!=.
replace choice_iv = 0 if p_iv!=max_p_iv & p_iv!=.
bys ZCAIMAE: egen share_iv_2008=sum(choice_iv) if anio_solicitud==2008
replace share_iv=share_iv_2008  if anio_solicitud==2008
