gen choice1=.
replace choice1=1 if choice=="TRUE"
replace choice1=1 if choice=="FALSE"


foreach var of varlist CASEXO ZCASINST ZB1SRAZA ZB1SOCUP0 B1SNIVEL {
encode `var', generate(i`var')

}

clogit choice dist medimae inst CASEDADA iCASEXO iZCASINST iZB1SRAZA iZB1SOCUP0 iB1SNIVEL, group(CAPACNUM)

cmset CAPACNUM id2
cmclogit choice dist medimae inst casevars(CASEDADA iCASEXO iZCASINST iZB1SRAZA iZB1SOCUP0 iB1SNIVEL)
