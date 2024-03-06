

*foreach var of varlist CASEXO ZCASINST ZB1SRAZA ZB1SOCUP0 B1SNIVEL {
*encode `var', generate(i`var')
*
*}
*gen pub=1 if inlist(choice,2,16)
*gen ind=1 if inlist(choice,7,13,17,5,10,18,9,12)
*gen pri=1 if inlist(choice,1,14,19,6,15,4,11,8,20)
*ssc install estout

cd "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis"
use "C:\Users\julie\OneDrive\Documentos\Proyecto Tesis\MastersThesis\mlogitdta.dta", replace
gen distk=dist/1000

label variable inst "Affiliated"
label variable medimae "Nephrologist"
label variable distk "Distance (km)"
label variable quality "Quality"

// MERCADOS: Tiene o no tiene
qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins, eydx(*)  post
estimates store notiene

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins, eydx(*)  post
estimates store tiene

esttab tiene notiene using "mercados.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber booktab ///
    collabels("hola" "holi" "holo") ///
    style(tex) title("Elasticities") replace
	
// Por tipo de paciente
qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_pac=="Activo", eydx(*)  post
estimates store activo_n

qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_pac=="Inactivo", eydx(*)  post
estimates store inactivo_n

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_pac=="Activo", eydx(*)  post
estimates store activo

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_pac=="Inactivo", eydx(*)  post
estimates store inactivo

esttab activo inactivo activo_n inactivo_n using "tipo_pac.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber booktab ///
    collabels("hola" "holi" "holo") ///
    style(tex) title("Elasticities") replace
	
qui clogit choice inst medimae distk if tiene_imae==0 & tipo_pac=="Activo", group(CAPACNUM)
margins, eydx(*)  post
estimates store activo_n

qui clogit choice inst medimae distk  if tiene_imae==0 & tipo_pac=="Inactivo", group(CAPACNUM)
margins, eydx(*)  post
estimates store inactivo_n

qui clogit choice inst medimae distk if tiene_imae==1 & tipo_pac=="Activo", group(CAPACNUM)
margins, eydx(*)  post
estimates store activo

qui clogit choice inst medimae distk  if tiene_imae==1 & tipo_pac=="Inactivo", group(CAPACNUM)
margins, eydx(*)  post
estimates store inactivo

esttab activo inactivo activo_n inactivo_n using "tipo_pac2.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber booktab ///
    collabels("hola" "holi" "holo") ///
    style(tex) title("Elasticities") replace

	
// Por tipo IMAE
qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_imae=="PUBLICO", eydx(*)  post
estimates store publico_n

qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_imae=="PRIVADO", eydx(*)  post
estimates store privado_n

qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_imae=="INDEPENDIENTE", eydx(*)  post
estimates store independiente_n

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_imae=="PUBLICO", eydx(*)  post
estimates store publico

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_imae=="PRIVADO", eydx(*)  post
estimates store privado

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_imae=="INDEPENDIENTE", eydx(*)  post
estimates store independiente

esttab publico privado independiente publico_n privado_n independiente_n using "tipo_imae.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber booktab ///
    collabels("hola" "holi" "holo") ///
    style(tex) title("Elasticities") replace
	
// Por tipo IMAE choice
qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_choice=="PUBLICO", eydx(*)  post
estimates store publico_n

qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_choice=="PRIVADO", eydx(*)  post
estimates store privado_n

qui clogit choice inst medimae distk if tiene_imae==0, group(CAPACNUM)
margins if tipo_choice=="INDEPENDIENTE", eydx(*)  post
estimates store independiente_n

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_choice=="PUBLICO", eydx(*)  post
estimates store publico

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_choice=="PRIVADO", eydx(*)  post
estimates store privado

qui clogit choice inst medimae distk if tiene_imae==1, group(CAPACNUM)
margins if tipo_choice=="INDEPENDIENTE", eydx(*)  post
estimates store independiente

esttab publico privado independiente publico_n privado_n independiente_n using "imae_choice.tex", ///
    cells(b(star fmt(3)) se(par fmt(3))) ///
    label nonumber booktab ///
    collabels("hola" "holi" "holo") ///
    style(tex) title("Elasticities") replace
	

------------


// Manual-----------------------------------

preserve
replace inst=0 if choice==1
predict phat0, pu0
replace inst=1 if choice==1
predict phat1
gen dd=phat1-phat0
gen ed=(phat1-phat0)/phat0*100
summarize dd if choice==1
summarize ed if choice==1
restore

preserve
replace medimae=0
predict phat0
replace medimae=1 if choice==1
predict phat1
gen dd=phat1-phat0
gen ed=(phat1-phat0)/phat0*100
summarize dd if choice==1
summarize ed if choice==1
restore

preserve
predict phat0
replace distk=distk*1.01 if choice==1
predict phat1
gen de=phat1-phat0
gen ee=(phat1-phat0)/phat0
summarize de if choice==1
summarize ee if choice==1
restore 





eststo tiene: margins, eydx(inst medimae distk) atmeans

reg choice inst medimae distk


clogit choice medimae distk if tiene_imae==0, group(CAPACNUM)
eststo notiene: margins, eydx(medimae distk) atmeans

estout tiene notiene using "uso.tex", ///
  style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) ///
   stats(N, labels("\hline Observations") fmt(%9.0fc)) ///
  replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01)

clogit choice inst medimae distk  if IMAE==1, group(CAPACNUM)
eststo pub: margins, eydx(inst medimae distk) atmeans

clogit choice inst medimae distk if pri==1, group(CAPACNUM)
eststo pri: margins, eydx(inst medimae distk) atmeans

clogit choice inst medimae distk if ind==1, group(CAPACNUM)
eststo ind: margins, eydx(inst medimae distk) atmeans




*pub 2 16
*ind 7 13 17 5 10 18 9 12
*pri 1 14 19 6 15 4 11 8 20


clogit choice inst medimae dist IMAE##iCASEXO IMAE##iB1SNIVEL, group(CAPACNUM)



margins, dydx(dist) atmeans


estout reg1 reg2 reg3

clogit choice inst medimae dist, group(CAPACNUM)
margins, eydx(inst medimae dist) atmeans
