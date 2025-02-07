

*************************
*** clean HILDA waves ***
*************************
{

	// Wave 1 variables
	use xwaveid ahhrhid aanatsi ahhssa2 ahhssa3 ahhssa4 ahhwtrp ahgint ahgsex ahgage ahhiage aedhigh1 amrcurr atifditn atifditp ahglth aghgh aghbp aghmh aghpf aghre aghrht ///
		aghpf aghrp aghbp aghgh aghvt aghsf aghre aghmh aghsf6d aghpf aghmh agh1 aanatsi aesbrd ajbmo61 alssmoke ahifdip ahifdin ahhpers ahhadult aghmh aghsf /// aherate
		ahhstate ahhsos aanbcob aes alsdrink alssmoke arg* ahhpno *cob* alosat* amhrea* *hhhqivw *hglth ///
		using "$data\Combined_a220u.dta", clear		// was 160, then 180, now 190		
	rename a* *		// Strip off wave prefix
	gen wave = 1		// Create wave indicator (1, 2 ...) 
	
	rename lsdrink lsdrkf
	label define lsdrkf 1 "has never drunk alcohol" 2 "no longer drinks" 3 "drinks rarely" 4 "drinks less than once a week" 5 "drinks 1-2 days/week" 6 "drinks 3-4 days/week" 7 "drinks 4-5 days/week" 8 "drinks daily"
	label values lsdrkf lsdrkf
	
	rename lssmoke lssmkf
	label define lssmkf 1 "has never smoked" 2 "no longer smoke" 3 "smokes"
	label values lssmkf lssmkf
	
	* household disposable income
	gen hhincome = hifdip - hifdin
	replace hhincome = hhincome/1000
	label var hhincome "household disposable income (thousands)"

	gen hhchild = hhpers - hhadult
	label var hhchild "number of children in household"
	gen ehi = hhincome/(1+0.5*(hhadult-1)+0.3*hhchild)
	label var ehi "equivalised household income (thousands)"

	** income quintile based on EHI by wave
	sort ehi
	gen rank = _n/_N
	gen ehiq2 = [rank >= 0.2 & rank < 0.4]
	gen ehiq3 = [rank >= 0.4 & rank < 0.6]
	gen ehiq4 = [rank >= 0.6 & rank < 0.8]
	gen ehiq5 = [rank >= 0.8]
	
	sort xwaveid
	gen ehiqall   = 1  +  ehiq2        +  2*ehiq3        +  3*ehiq4  +  4*ehiq5
	
	** income quartile based on EHI by wave
	sort ehi
	
	sort xwaveid
	gen     ehi_quart = 1 if [rank < 0.25 ]
	replace ehi_quart = 2 if [rank >= 0.25 & rank < 0.5]
	replace ehi_quart = 3 if [rank >= 0.5 & rank < 0.75]
	replace ehi_quart = 4 if [rank >= 0.75]
	
drop rank
save "$data\wave_1.dta", replace



local i=2
	foreach w in b c d e f g h i j k l m n o p q r s t u v {		

		use xwaveid `w'anatsi* `w'hglth `w'hhrhid `w'hhssa2 `w'hhssa3 `w'hhssa4 `w'hhwtrp `w'hgint `w'hgsex `w'hgage `w'hhiage `w'edhigh1 `w'mrcurr `w'tifditn `w'tifditp  `w'ghgh `w'ghbp `w'ghmh `w'ghpf `w'ghre `w'ghrht ///
			`w'ghpf `w'ghrp `w'ghbp `w'ghgh `w'ghvt `w'ghsf `w'ghre `w'ghmh `w'ghsf6d `w'gh3a `w'ghpf `w'gh1  `w'anatsi `w'esbrd  `w'jbmo61 `w'hifdip `w'hifdin `w'hhpers `w'hhadult `w'ghmh ///
			`w'hhstate `w'hhsos `w'anbcob `w'es `w'lsdrkf `w'lssmkf  `w'lstbcn `w'rg* `w'hhpno *cob* `w'losat* `w'mhrea* `w'hhhqivw  `w'hglth ///
			using "$data\Combined_`w'220u.dta", clear	
		
			rename `w'* *		// Strip off wave prefix
			gen wave = `i'		// Create wave indicator (a=1, b=2, ..., s=2019)
			recode lsdrkf 3=8 4=7 5=6 6=5 7=4 8=3
			label define lsdrkf 1 "has never drunk alcohol" 2 "no longer drinks" 3 "drinks rarely" 4 "drinks less than once a week" 5 "drinks 1-2 days/week" 6 "drinks 3-4 days/week" 7 "drinks 4-5 days/week" 8 "drinks daily"
			label values lsdrkf lsdrkf
			
			/*
			recode lssmkf 4=3 5=3 // change "yes i smoke less often than weekly=5" to "smokes=3" OR "no longer smoke==2" //4=2 5=2 is for appendix
			label define lssmkf 1 "has never smoked" 2 "no longer smoke" 3 "smokes"
			label values lssmkf lssmkf
			*/
			
			* household disposable income
			gen hhincome = hifdip - hifdin
			replace hhincome = hhincome/1000
			label var hhincome "household disposable income (thousands)"

			gen hhchild = hhpers - hhadult
			label var hhchild "number of children in household"
			gen ehi = hhincome/(1+0.5*(hhadult-1)+0.3*hhchild)
			label var ehi "equivalised household income (thousands)"

			** income quintile based on EHI by wave
			sort ehi
			gen rank = _n/_N
			gen ehiq2 = [rank >= 0.2 & rank < 0.4]
			gen ehiq3 = [rank >= 0.4 & rank < 0.6]
			gen ehiq4 = [rank >= 0.6 & rank < 0.8]
			gen ehiq5 = [rank >= 0.8]
			
			sort xwaveid
			gen ehiqall   = 1  +  ehiq2        +  2*ehiq3        +  3*ehiq4  +  4*ehiq5
			
			** income quartile based on EHI by wave
			sort ehi
			
			sort xwaveid
			gen     ehi_quart = 1 if [rank < 0.25 ]
			replace ehi_quart = 2 if [rank >= 0.25 & rank < 0.5]
			replace ehi_quart = 3 if [rank >= 0.5 & rank < 0.75]
			replace ehi_quart = 4 if [rank >= 0.75]
			
		drop rank
			
	save "$data\wave_`i'.dta", replace
	local i=`i'+1
	}
	
use "$data\wave_1.dta", clear
	forvalues i=2/22 {
		append using "$data\wave_`i'.dta"
	}
		
	sort xwaveid wave
	

	*general data cleaning:
	
	* weight
	rename hhwtrp weight

	* sex
	rename hgsex male
	recode male 2=0
	label define male 0 "female" 1 "male"
	label values male male
	label var male "=1 if male"

	* in labour force
	gen lf = 1 if esbrd == 1 | esbrd == 2
	replace lf = 0 if lf == .
	label var lf "=1 if in labour force"
	label define lf 1 "in labour force" 0 "not in labour force"
	label values lf lf

	* age
	rename hgage age0

	* married
	gen married = 1 if mrcurr == 1 | mrcurr == 2
	replace married = 0 if married == . & mrcurr > 0
	label var married "=1 if married or de facto"
	label define married 1 "married or de facto" 0 "not married"
	label values married married

	* indigenous
	gen indig = 1 if anatsi == 2 | anatsi == 3 | anatsi == 4
	replace indig = 0 if indig == . & anbcob > 0
	label var indig "=1 if indigenous or Torres Strait Islander"
	label define indig 1 "indigenous or TSI" 0 "not indigenous"
	label values indig indig

	* education
	gen degree = 1 if edhigh1 <= 3
	replace degree = 0 if degree == . & edhigh1 ~= 10
	label var degree "=1 if has a bachelor or higher degree"

	gen oth_psq = 1 if edhigh1 == 4 | edhigh1 == 5
	replace oth_psq = 0 if oth_psq == . & edhigh1 ~= 10
	label var oth_psq "=1 if has other non-degree post-school qualifications"

	gen yr12 = 1 if edhigh1 == 8
	replace yr12 = 0 if yr12 == . & edhigh1 ~= 10
	label var yr12 "=1 if completed year 12"

	// or something like the following
	gen educ = 1 if edhigh1 == 9
	replace educ = 2 if edhigh1 == 8 | edhigh1 == 5
	replace educ = 3 if edhigh1 == 4 | edhigh1 == 3 | edhigh1 == 2 | edhigh1 == 1
	tab edhigh1 educ, m
	label var educ "highest education"
	label define educ 1 "less than Yr 12" 2 "Yr 12 or equivalent" 3 "Bachelor or above"
	label values educ educ

	* spouse in labour force
	*vlookup hhpxid, gen(sp_lf) key(xwaveid) value(lf)
	*replace sp_lf = 0 if married == 0
	*label var sp_lf "=1 if married and the spouse in lf"

	* state of residence
	rename hhstate state

	* rural/urban
	gen rural = 0 if hhsos == 0 | hhsos == 1
	replace rural = 1 if hhsos == 2 | hhsos == 3 | hhsos == 4
	label var rural "=1 if rural"
	label define rural 1 "rural" 0 "urban"
	label values rural rural

	* cob
	gen cob_os = 1 if anbcob == 2 | anbcob == 3
	replace cob_os = 0 if anbcob == 1
	label var cob_os "=1 if born overseas"

	gen cob_n_en = 1 if anbcob == 3
	replace cob_n_en = 0 if anbcob == 1 | anbcob == 2
	label var cob_n_en "=1 if born in non-English speaking foeign country"

	// or something like the following
	rename anbcob cob
	replace cob = . if cob < 0

	* occupation 
	gen trade = 1 if jbmo61 == 3
	replace trade = 0 if trade == . & jbmo61 ~= -7
	label var trade "=1 if tradesperson or related worker"

	gen cle_s = 1 if jbmo61 == 4 | jbmo61 == 5 | jbmo61 == 6
	replace cle_s = 0 if cle_s == . & jbmo61 ~= -7
	label var cle_s "=1 if clerical, sales or service worker"

	gen prd_tr = 1 if jbmo61 == 7
	replace prd_tr = 0 if prd_tr == . & jbmo61 ~= 7
	label var prd_tr "=1 if production or transport worker"

	gen labour = 1 if jbmo61 == 8
	replace labour = 0 if labour == . & jbmo61 ~= -7
	label var labour "=1 if labourer or related worker"

	// or something like the following
	rename jbmo61 occuptn
	replace occuptn = . if occuptn < 0

	gen whtcollar = 1 if occuptn == 4 | occuptn == 5 | occuptn == 6
	replace whtcollar = 0 if whtcollar == .
	label var whtcollar "=1 if white collar (not professional/manager)"

	gen bluecollar = 1 if occuptn == 3 | occuptn == 7 | occuptn == 9
	replace bluecollar = 0 if bluecollar == .
	label var bluecollar "=1 if blue collar"
	
	* personal disposable income
	gen income = tifditp - tifditn
	replace income = income/1000
	label var income "personal disposable income (thousands)"

	xtile hh_inc_q=hhincome, nq(4) // quartiles hh income
	
	* long term health condition
	
	g lth=0 if hglth==2
	replace lth=1 if hglth==1
 
	
save "$data\waves1_22.dta", replace
}

************************************
*** prepare sample for analysis: ***
************************************
{
	use "waves1_22.dta", clear

	*merge m:1 xwaveid using "G:\My Drive\Data\Saxby\hilda22\data\Combined_v220u.dta", keepusing(vscsxgn)  
	merge m:1 xwaveid using "Combined_v220u.dta", keepusing(vscsxgn)  
	ta vscsxgn,m

	*people who did not complete wave 22:
	g comp22=0
	replace comp22=1 if wave==22
	bys xwaveid: egen max_comp22=max(comp22)
	ta max_comp22
drop if max_comp22==0 // 140,686	
	
	unique(xwaveid) // 21,732
	
*	Age groups
keep if age0>=15	//69,357 
	di 69357/454861

	unique(xwaveid)  
	
cap drop age_g 

g age_g=1 if age0>=15 & age0<=24
replace age_g=2 if age0>=25 & age0<=34
replace age_g=3 if age0>=35 & age0<=44
replace age_g=4 if age0>=45 & age0<=54
replace age_g=5 if age0>=55 & age0<=64
replace age_g=6 if age0>=65 & age0<=74
replace age_g=7 if age0>=75
	

	*Missing population weight:
drop if sc_weight==-10	//8,564
	di 8564/454861
	
	*Restricting to those who responded to TGD
	ta vscsxgn ,m
drop if vscsxgn==-10 // non-responding	
	di 5276/454861
drop if vscsxgn==-8 // no-SCQ	
	di 14593/454861	
drop if vscsxgn==-4 // refused / not stated
	di 3272/454861		
drop if vscsxgn==3 // inadequately described
	di 978/454861		
	
	


	* Missing data MHI

	gen miss_ghmh=1 if ghmh<0
	replace miss_ghmh=0 if ghmh>=0	
	tab miss_ghmh	
	
	codebook xwaveid
	
	gen tgd22=0 if vscsxgn==1
	replace tgd2=1 if  vscsxgn==2
	tab tgd22,m

save "sample_premissMHIdrop.dta", replace

drop if miss_ghmh==1		 
	di 12048/454861
	
	ta vscsxgn ,m
	
*	Count number of observations per individual
	gen x=1
	bys xwaveid: egen obs = total(x) 
	
	svyset [pweight=sc_weight]

	bys tgd22: sum obs

save "sample_reg_new.dta"	, replace
}

****************
*** analysis ***
****************

{
use "sample_reg_new.dta", clear

	unique(xwaveid)
	cap bys xwaveid: gen no_wave=_N
	sum no_wave
	
	egen double ID=group(xwaveid)
	egen double HHID=group(hhrhid)
	cap bys xwaveid: egen min_wave=min(wave)	
	
	cap g weight_nw=hhwtsc if wave<=10
	replace weight_nw=hhwtscm if wave>=11
	
	cap drop year
	g year=wave+2000
	cap drop tgd_year
	g tgd_year=tgd*year
	
	replace year=wave+2000
	cap egen double HHID=group(hhrhid)
	cap g weight_nw=hhwtsc if wave<=10
	replace weight_nw=hhwtscm if wave>=11
	
	cap drop year
	g year=wave+2000
	cap drop tgd_year
	g tgd_year=tgd*year
	
	
	
	
	ta wave tgd if ghmh!=.
	ta wave tgd
	************************
	***age distribution: ***
	************************	
{	
	tabstat age0 if tgd==0, stat(mean median min max sd) by(wave)
	tabstat age0 if tgd==1, stat(mean median min max sd) by(wave)
	
cap drop tag
cap drop _merge 
	ssc install joyplot

	joyplot age0 if tgd == 0 , by(wave) title("Cisgender") xtitle("Age (years)") ytitle("Wave", size(medsmall))
	graph export "$output\age_dist_cis.png", as(png) name("Graph") replace

	joyplot age0 if tgd == 1, by(wave) title("TGD") xtitle("Age (years)") ytitle("Wave", size(medsmall))
	graph export "$output\age_dist_tgd.png", as(png) name("Graph") replace

	bys wave tgd: egen mn_age=mean(age0)
	bys wave tgd: egen sd_age=sd(age0)
	g age_ll=mn_age-sd_age
	g age_ul=mn_age+sd_age
	
	bys wave tgd: gen k =_n 
	
	set scheme white_tableau
	tw  rarea age_ll age_ul year if tgd==1 & k==1 || ///
		line mn_age year if tgd==1 & k==1  , ///
		legend(pos(6) col(2) order(1 "Standard deviation" 2 "Mean") size(medium)) ytitle("{bf:Age (years)}", size(medium)) ///
		title("TGD") xtitle("{bf:Wave}", size(medium))
	graph export "$output\age_sd_tgd.png", as(png) name("Graph") replace
	
	 tw rarea age_ll age_ul year if tgd==0 & k==1 || ///
		line mn_age year if tgd==0 & k==1 , ///
		legend(pos(6) col(2) order(1 "Standard deviation" 2 "Mean") size(medium))  ytitle("{bf:Age (years)}", size(medium)) ///
		title("Cisgender")  xtitle("{bf:Wave}", size(medium)) 
	graph export "$output\age_sd_cis.png", as(png) name("Graph") replace	
}	
	********************
	***descriptives: ***
	********************
	svyset [pweight=sc_weight]
	la def tgd_la 0 "Cisgender" 1 "TGD", modify
	la val tgd tgd_la
	
	*larger age group to have none <10:
	g age_g2=1 if age0>=15 & age0<=24
	replace age_g2=2 if age0>=25 & age0<=34
	replace age_g2=3 if age0>=35 & age0<=44
	replace age_g2=4 if age0>=45 & age0<=54
	replace age_g2=5 if age0>=55 & age0<=64
	replace age_g2=6 if age0>=65 & age0<=.
	
	desctable age0 i.age_g2 i.male i.educ i.male i.married i.cob_os  i.ehi_quart i.state rural, ///
	filename($output\desc_wave22) stats(svymean svysemean) group(tgd)

	ta age_g2, gen(age_g2)
	ta educ, gen(educ)
	ta ehi_quart, gen(ehi_quart)
	ta state, gen(state)
	mean age0 age_g21 age_g22 age_g23 age_g24 age_g25 age_g26 ///
		 male educ1 educ2 educ3 married cob_os ///
		 ehi_quart1 ehi_quart2 ehi_quart3 ehi_quart4 ///
		 rural state1 state2 state3 state4 state5 state6 state7 state8 [pw=sc_weight] if tgd==0
	
	mean age0 age_g21 age_g22 age_g23 age_g24 age_g25 age_g26 ///
		 male educ1 educ2 educ3 married cob_os ///
		 ehi_quart1 ehi_quart2 ehi_quart3 ehi_quart4 ///
		 rural state1 state2 state3 state4 state5 state6 state7 state8 [pw=sc_weight] if tgd==1
	
	, over(tgd)
	
	
	******************************************************************
	*** unadjusted results & differences (table s4 supp material): ***
	******************************************************************
{	
	*do unad diff first:
	reg ghmh i.year i.tgd_year [pw=sc_weight]
	coefplot ., keep(*tgd_year*) vert gen(ud)
	format udb udll1 udul1  %12.1f
	br udat udb udll1 udul1  if udat!=. // copy to excel
	
	reg ghmh i.wave   if tgd==0 [pw=sc_weight]
	margins, at(wave=(1(1)22)) saving(unadj_res_cis, replace) 
	
	reg ghmh i.wave   if tgd==1 [pw=sc_weight]
	margins, at(wave=(1(1)22)) saving(unadj_res_tgd, replace) 

	preserve 
	
use unadj_res_cis, clear
	ren _margin cis_beta 
	ren _ci_lb cis_ll
	ren _ci_ub cis_ul

	merge 1:1 _at using unadj_res_tgd
	ren _margin tgd_beta 
	ren _ci_lb  tgd_ll
	ren _ci_ub  tgd_ul
	g year=_at+2000
	
	
	order year cis_* tgd*
keep year cis_* tgd*
	format cis_beta cis_ll cis_ul tgd_beta tgd_ll tgd_ul %12.1f // copy to excel
}	
	
	********************************************************************************
	*** adjusted results & differences (main figure 1 & table s5 supp material): ***
	********************************************************************************
{	
	global xi i.age_g ib2.educ ib1.male ib1.married ib1.cob_os  ib3.ehi_quart i.state rural

	reghdfe ghmh i.year i.tgd_year [pw=sc_weight] , a($xi) cl(HHID) 
cap drop ad 
cap drop ad*	
cap drop year1
	coefplot ., keep(*tgd_year*) vert gen(ad)
	
	format adb adll1 adul1  %12.1f
	br adat adb adll1 adul1  if adat!=. // copy to excel
	
	*graph Figure 1 inequalities:
	cap g year1=adat+2000
	
	twoway rarea  adll1 adul1  year1  , vertical fcolor(gs12) fi(inten20) lcolor(gs12) ||  ///
	line adb year1 , lpattern(solid) lcolor(black)  ///
	lcolor(gray) fcolor(gray) fi(inten20) yline(0) xlab(2001(5) 2022) ///
	ytitle("Mean Difference in MHI Score", size(medsmall)) ///
	xtitle("") ///
	legend(off) ///
	graphregion(fcolor(white))
	graph save "Graph" "$output\fig1_inequalities.gph", replace
	graph export "$output\fig1_inequalities.png", as(png) name("Graph") replace

	margins ,  at(tgd=(0) year=(2001(1)2022)) saving($output\margins_res_cis, replace) 
	margins ,  at(tgd_year=(2001(1)2022) ) saving($output\margins_res_tgd, replace) // nb dafault is asobserved

preserve 

use $output\margins_res_cis, clear
	ren _margin cis_beta 
	ren _ci_lb cis_ll
	ren _ci_ub cis_ul

	merge 1:1 _at using $output\margins_res_tgd
	ren _margin tgd_beta 
	ren _ci_lb  tgd_ll
	ren _ci_ub  tgd_ul
	
	g year=_at+2000
	
	format cis_beta cis_ll cis_ul tgd_beta tgd_ll tgd_ul %12.1f
	order year cis_* tgd* // copy to excel 
	
	twoway 	rarea cis_ll cis_ul year , vertical fcolor(gs12%40)  lcolor(gs12) || ///
			line  cis_beta year , lpattern(solid) lcolor(black) ||  ///
			rarea tgd_ll tgd_ul year , vertical fcolor(gs12%20) lcolor(gs12) || ///
			line  tgd_beta year , lpattern(dash) lcolor(black)  ///
			yline(0) xlab(2001(5) 2022) ///
			ytitle("MHI Score", size(medsmall)) ///
			xtitle("") ///
			legend(order(2 "Cisgender" 4 "TGD") position(6) cols(3)) ///
			graphregion(fcolor(white)) 
	graph save "Graph" "$output\fig1_margins.gph", replace
	graph export "$output\fig1_margins.png", as(png) name("Graph") replace
}
			
	*******************************************		
	*** robustness checks in table format : ***
	*******************************************
	global xi i.age_g ib2.educ ib1.male ib1.married ib1.cob_os ib3.ehi_quart i.state rural
	
	eststo a:  reghdfe ghmh i.year i.tgd_year [pw=sc_weight] , a($xi) cl(HHID) 
	unique(xwaveid) if e(sample)
	eststo b:  reghdfe ghmh i.year i.tgd_year [pw=sc_weight] if min_wave<11, a($xi) cl(HHID) 	
	unique(xwaveid) if e(sample)
	eststo c:  reghdfe ghmh i.year i.tgd_year [pw=weight_nw] , a($xi) cl(HHID) 
	unique(xwaveid) if e(sample)
	eststo d:  reghdfe ghmh i.year i.tgd_year [pw=sc_weight] if age0<=30 , a($xi) cl(HHID) 
	*coefplot ., keep(*tgd*) vert
	unique(xwaveid) if e(sample)  
	
	esttab a b c d using "$output\robust.rtf", keep(*tgd*) replace  ///
		cells("b(fmt(2)) ci(par(( - )) fmt(2))") label	stat(N r2) 
	
	**************************************************
	*** graph for everyone in sample prior to 2011 ***
	**************************************************
{	
	unique(xwaveid) if min_wave<11 & tgd==1
	
	reghdfe ghmh tgd#i.wave  [pw=sc_weight] if min_wave<11, a($xi) cl(HHID) 
cap drop tgd_coef 
cap drop tgd_coef*	
cap drop year2
	coefplot ., keep(1.tgd*) vert gen(tgd_coef)
	cap g year2=tgd_coefat+2000
	
	twoway rarea tgd_coefll1 tgd_coeful1 year2  , vertical fcolor(gs12) fi(inten20) lcolor(gs12) ||  ///
	line tgd_coefb year2, lpattern(solid) lcolor(black)  ///
	lcolor(gray) fcolor(gray) fi(inten20) yline(0) xlab(2001(5) 2022) ///
	ytitle("Mean Difference in MHI Score", size(medsmall)) ///
	xtitle("") ///
	legend(off) ///
	graphregion(fcolor(white))			
			
	graph export "$output\before2011_ineq.png", as(png) name("Graph") replace
	
	
margins ,  at(tgd=(0) wave=(1(1)22)) saving(margins_res_pre2011_cis, replace) 
margins ,  at(tgd=(1) wave=(1(1)22)) saving(margins_res_pre2011_tgd, replace)

preserve 

use margins_res_pre2011_cis, clear
	ren _margin cis_beta 
	ren _ci_lb cis_ll
	ren _ci_ub cis_ul

	merge 1:1 _at using margins_res_pre2011_tgd
	ren _margin tgd_beta 
	ren _ci_lb  tgd_ll
	ren _ci_ub  tgd_ul
	
	g year=_at+2000
	
	twoway 	rarea cis_ll cis_ul year , vertical fcolor(gs12%40)  lcolor(gs12) || ///
			line  cis_beta year , lpattern(solid) lcolor(black) ||  ///
			rarea tgd_ll tgd_ul year , vertical fcolor(gs12%20) lcolor(gs12) || ///
			line  tgd_beta year , lpattern(dash) lcolor(black)  ///
			yline(0) xlab(2001(5) 2022) ///
			ytitle("MHI Score", size(medsmall)) ///
			xtitle("") ///
			legend(order(2 "Cisgender" 4 "TGD") position(6) cols(3)) ///
			graphregion(fcolor(white)) 
	graph export "$output\before2011_means.png", as(png) name("Graph") replace
}
	
	
	******************************************************************
	*** graph for different weights as per HILDA team suggestion: ****
	*** ("Use hhwtsc for W1-10 and then hhwtscm for W11+") ***********
	******************************************************************
{	
	cap g weight_nw=hhwtsc if wave<=10
	replace weight_nw=hhwtscm if wave>=11
	cap egen double HHID=group(hhrhid)

	global xi i.age_g ib2.educ ib1.male ib1.married ib1.cob_os  ib3.ehi_quart i.state rural

	reghdfe ghmh tgd#i.wave  [pw=weight_nw] , a($xi) cl(HHID) 
cap drop tgd_coef 
cap drop tgd_coef*	
cap drop year3
	coefplot ., keep(1.tgd*) vert gen(tgd_coef)
	g year3=tgd_coefat+2000
	
	twoway rarea tgd_coefll1 tgd_coeful1 year3  , vertical fcolor(gs12) fi(inten20) lcolor(gs12) ||  ///
	line tgd_coefb year3, lpattern(solid) lcolor(black)  ///
	lcolor(gray) fcolor(gray) fi(inten20) yline(0) xlab(2001(5) 2022) ///
	ytitle("Mean Difference in MHI Score", size(medsmall)) ///
	xtitle("") ///
	legend(off) ///
	graphregion(fcolor(white))			
			
	graph export "$output\diffweight_ineq.png", as(png) name("Graph") replace
	
	
margins ,  at(tgd=(0) wave=(1(1)22)) saving(margins_res_diffweight_cis, replace) 
margins ,  at(tgd=(1) wave=(1(1)22)) saving(margins_res_diffweight_tgd, replace)

preserve 

use margins_res_diffweight_cis, clear
	ren _margin cis_beta 
	ren _ci_lb cis_ll
	ren _ci_ub cis_ul

	merge 1:1 _at using margins_res_diffweight_tgd
	ren _margin tgd_beta 
	ren _ci_lb  tgd_ll
	ren _ci_ub  tgd_ul
	
	g year=_at+2000
	
	twoway 	rarea cis_ll cis_ul year , vertical fcolor(gs12%40)  lcolor(gs12) || ///
			line  cis_beta year , lpattern(solid) lcolor(black) ||  ///
			rarea tgd_ll tgd_ul year , vertical fcolor(gs12%20) lcolor(gs12) || ///
			line  tgd_beta year , lpattern(dash) lcolor(black)  ///
			yline(0) xlab(2001(5) 2022) ///
			ytitle("MHI Score", size(medsmall)) ///
			xtitle("") ///
			legend(order(2 "Cisgender" 4 "TGD") position(6) cols(3)) ///
			graphregion(fcolor(white)) 
	graph export "$output\diffweight_means.png", as(png) name("Graph") replace
}	
	
	
	
	******************************* 
	*** graph for young people : **
	******************************* 
{	
	 

	global xi i.age_g ib2.educ ib1.male ib1.married ib1.cob_os  ib3.ehi_quart i.state rural

	reghdfe ghmh tgd#i.wave  [pw=sc_weight] if age0<=30, a($xi) cl(HHID) 
cap drop tgd_coef 
cap drop tgd_coef*	
cap drop year3
	coefplot ., keep(1.tgd*) vert gen(tgd_coef)
	g year3=tgd_coefat+2000
	
	twoway rarea tgd_coefll1 tgd_coeful1 year3  , vertical fcolor(gs12) fi(inten20) lcolor(gs12) ||  ///
	line tgd_coefb year3, lpattern(solid) lcolor(black)  ///
	lcolor(gray) fcolor(gray) fi(inten20) yline(0) xlab(2001(5) 2022) ///
	ytitle("Mean Difference in MHI Score", size(medsmall)) ///
	xtitle("") ///
	legend(off) ///
	graphregion(fcolor(white))			
			
	graph export "$output\under30_ineq.png", as(png) name("Graph") replace
	
	
margins ,  at(tgd=(0) wave=(1(1)22)) saving(margins_res_diffweight_cis, replace) 
margins ,  at(tgd=(1) wave=(1(1)22)) saving(margins_res_diffweight_tgd, replace)

preserve 

use margins_res_diffweight_cis, clear
	ren _margin cis_beta 
	ren _ci_lb cis_ll
	ren _ci_ub cis_ul

	merge 1:1 _at using margins_res_diffweight_tgd
	ren _margin tgd_beta 
	ren _ci_lb  tgd_ll
	ren _ci_ub  tgd_ul
	
	g year=_at+2000
	
	twoway 	rarea cis_ll cis_ul year , vertical fcolor(gs12%40)  lcolor(gs12) || ///
			line  cis_beta year , lpattern(solid) lcolor(black) ||  ///
			rarea tgd_ll tgd_ul year , vertical fcolor(gs12%20) lcolor(gs12) || ///
			line  tgd_beta year , lpattern(dash) lcolor(black)  ///
			yline(0) xlab(2001(5) 2022) ///
			ytitle("MHI Score", size(medsmall)) ///
			xtitle("") ///
			legend(order(2 "Cisgender" 4 "TGD") position(6) cols(3)) ///
			graphregion(fcolor(white)) 
	graph export "$output\diffweight_means.png", as(png) name("Graph") replace
	
	restore
}	
	
		
	

}
 
***************************
*** multiple imputation ***
***************************
{
 
use "sample_premissMHIdrop.dta", clear

// *Missing MHI per wave

tab wave miss_ghmh

*Individuals with missing MHI
codebook xwaveid if miss_ghmh==1 & tgd==0	//4,689 CIS
codebook xwaveid if miss_ghmh==1 & tgd==1	//57 tgd

*	How many missing bc no SCQ and refused/not states	
tab ghmh if miss_ghmh==1

*	Missing data table
tab miss_ghmh
tab tgd miss_ghmh
tab male miss_ghmh
tab married miss_ghmh	// 8 missing observations in married
tab age_g miss_ghmh
tab cob_os miss_ghmh	//50 missing observations in cob_os
tab educ miss_ghmh		//96 missing observations (3 individuals) in educ
tab ehi_quart miss_ghmh	
tab state miss_ghmh
tab rural miss_ghmh

*Keep only necessary vars
keep xwaveid wave sc_weight tgd ghmh miss_ghmh age0 age_g ageg_d* male married cob_os educ ehi_quart state rural 
sum tgd age0 ageg_d* male married cob_os educ ehi_quart state rural

* I have missings in educ and cob_os so will create dummies
replace educ=99 if educ==.
replace cob_os=99 if cob_os==.
replace married=99 if married==.
replace rural=99 if rural==.
sum

*add household ID
merge 1:1 xwaveid wave using "G:\My Drive\Data\Saxby\hilda22\data\waves1_22.dta", keepusing(hhrhid)
keep if _merge==3
drop _merge
egen double HHID=group(hhrhid)
save "$imputed\impute.dta", replace
// logit miss_ghmh i.male i.cob_os i.educ i.ehi_quart i.state


**# Multiple imputation
clear all
use "$imputed\impute.dta"

gen miss_mhi = ghmh
replace miss_mhi=. if ghmh<0

* Setup for multiple imputation
mi set flong

* Register variables to be imputed
mi register imputed miss_mhi

* Register variables to be held constant across imputations (no missing values)
mi register regular tgd age_g male married ehi_quart state educ cob_os rural

* Imputation - PMM, knn(5)
mi impute pmm miss_mhi tgd age_g male married ehi_quart state educ cob_os rural, knn(5) add(50) noisily rseed(64321) replace

* Save the imputated dataset
save "$imputed\impute_fix.dta", replace

* Imputed dataset

clear all
use "$imputed\impute_fix.dta"

// mi svyset [pweight=sc_weight]

* REG
g year=wave+2000 
g tgd_year=tgd*year


	******************************************************************
	*** unadjusted results & differences (table s5 supp material): ***
	******************************************************************
	
	*do unad diff first:
	mi estimate, post: reg miss_mhi i.year i.tgd_year [pw=sc_weight] 
	
	coefplot ., keep(*tgd_year*) vert gen(ud)
	format udb udll1 udul1  %12.1f
	br udat udb udll1 udul1  if udat!=. // copy to excel
	
	*	Difference plot -Unadjusted
	twoway rarea udll1 udul1 udat , vertical fcolor(gs12) fi(inten20) lcolor(gs12) ||  ///
	line udb udat , lpattern(solid) lcolor(black)  ///
	lcolor(gray) fcolor(gray) fi(inten20) yline(0) xlab(1(5) 22) ///
	ytitle("Mean Difference in MHI Score (Imputed)", size(medsmall)) ///
	xtitle("") ///
	legend(order(1 "Mean" 2 "95% CI") position(6) cols(2)) ///
	graphregion(fcolor(white))

	*adjusted
	
	mi estimate, post: reg miss_mhi i.year i.tgd_year i.age_g ib2.educ ib1.male ib1.married ib1.cob_os  ib3.ehi_quart i.state rural [pw=sc_weight], cl(HHID)
	
// 	cap drop ad 
// 	cap drop ad*	
// 	cap drop year1
	coefplot ., keep(*tgd_year*) vert gen(ad)
	
	
	format adb adll1 adul1  %12.2f
	br adat adb adll1 adul1  if adat!=. // copy to excel

		*	Difference plot -Adjusted
	twoway rarea adll1 adul1 year , vertical fcolor(gs12) fi(inten20) lcolor(gs12) ||  ///
	line adb year , lpattern(solid) lcolor(black)  ///
	lcolor(gray) fcolor(gray) fi(inten20) yline(0) xlab(1(5) 22) ///
	ytitle("Mean Difference in MHI Score (Imputed)", size(medsmall)) ///
	xtitle("") legend(off) ///
	graphregion(fcolor(white))

}