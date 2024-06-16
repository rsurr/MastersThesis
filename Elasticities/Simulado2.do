
use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Logit_INGRESOS.dta", replace
cd "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\Simulacion"

encode ZCAIMAE, generate(iZCAIMAE)
gen l_distk=log(distk)
gen l_distk_sim=log(distk_sim)

global continuous URR hab_op turnos_op l_distk
global discrete inst medimae i.iZCAIMAE

clogit choice $discrete $continuous, group(CAPACNUM) iterate(30)  

gen actual_l_distk=l_distk
gen choice_hat=.
gen choice_sim=.
gen choice_sample=.
gen coincide_hat=.

foreach j in 1 0 {

clogit choice $discrete $continuous if tiene_imae==`j', group(CAPACNUM)

predict p_hat`j'
replace l_distk=l_distk_sim
predict p_sim`j'
replace l_distk=actual_l_distk

replace p_hat`j'=. if ZCAIMAE=="SARI"
replace p_sim`j'=. if ZCAIMAE=="SARI"

bys CAPACNUM: egen max_p_hat`j' = max(p_hat`j')
bys CAPACNUM: egen max_p_sim`j' = max(p_sim`j')

gen choice_hat`j'=.
replace choice_hat`j' = 1 if p_hat`j'==max_p_hat`j' & p_hat`j'!=.
replace choice_hat`j' = 0 if p_hat`j'!=max_p_hat`j' & p_hat`j'!=.

gen choice_sim`j'=.
replace choice_sim`j' = 1 if p_sim`j'==max_p_sim`j' & p_sim`j'!=.
replace choice_sim`j' = 0 if p_sim`j'!=max_p_sim`j' & p_sim`j'!=.

gen choice_sample`j'=.
replace choice_sample`j' = 1 if choice==1 & p_hat`j'!=.
replace choice_sample`j' = 0 if choice==0 & p_hat`j'!=.

replace choice_hat = choice_hat`j' if tiene_imae==`j'
replace choice_sim = choice_sim`j' if tiene_imae==`j'
replace choice_sample = choice_sample`j' if tiene_imae==`j'

replace coincide_hat=1 if choice_hat==choice & choice==1 & choice_hat!=.
replace coincide_hat=0 if choice_hat!=choice & choice==1 & choice_hat!=.

}

bys ZCAIMAE: sum choice_hat choice_sim choice_sample choice


summarize coincide_hat


export delimited using "simulacion2.csv", nolab replace
