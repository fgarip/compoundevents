********************************************************************************
************************ Culture & Mexico-US Migration ***********************
************************* Kunyuan Qiao & Filiz Garip ***************************
********************************************************************************


* This code compiles the working data set based on LIFE and SPOUSE (household
* heads and spouses) on the one hand, and on PERS (all individuals) on the other.
* Last update: 26 May 2022 11am

*    I - Household heads
*   II - Spouses
*  III - All individuals
*   IV - Append and merge LIFE and SPOUSE
*	 V - Working Data Sets

********************************************************************************

clear matrix
set matsize 800
set more off

/* * Run code that fills in missing national-level indicators and generates hh-level wealth measures

do hhwealth170
do macro170

*/


cd "/Users/fgarip/Dropbox/Current_work/Projects by Filiz and Kunyuan/mmp/data" 


* Part I ***********************************************************************
* Household heads **************************************************************
********************************************************************************

use life170, clear


* Individual variables
**********************

	gen persnum=1  // 1 = household heads
	
* Age (time-varying)

	gen ageg=age
	recode ageg 15/24=1 25/34=2 35/44=3 45/max=4
	lab define ageg 1 "15-24" 2 "25-34" 3 "35-44" 4 "45+"
	lab value ageg ageg
	lab var ageg "age groups"
						
* Sex - 1=male

	recode sex 2=0
	lab def sex 1 "male" 0 "female"
	lab value sex sex

* Married (time-varing)

	gen marry=(married==1 | cunion==1)
	lab var marry "married or consensual union" 
	lab define dummy 1 "yes" 0 "no"
	lab value marry dummy
              
* Number of children in the household (time-varing)
	
	recode cebsofar  9999=.

* Education (time-varing)
	
	* Continuous
	gen edyr=educ 
	recode edyr 9999=. 
	lab var edyr "years of education"
	
	* Categorical
	recode educ 0/6=1 7/11=2 12/15=3 16/50=4 9999=.
	lab define ed 1 "primary or less" 2 "some secondary" 3 "secondary" 4 "advanced"
	lab value educ ed

* Occupation (time-varying)

	* 14 categories
	for var occ occup: gen X_17=X
	for var occ_17 occup_17: recode X 10=1 20 21=2 42 43=3 30 40 41 50/99=4 110/119=5 120/129=6 ///
		130/139=7 140/149=8  210/219=9 410/419=10 510/539=11 540/549=12 610/629=13 710/719=14 810/819=15 ///
		820=16 550/559 720/729 830/839=17 0=0 *=.
	lab define occlab0 1 "seekwork" 2 "homemkr" 3 "stud" 4 "nowork" 5 "profsnl" 6 "techncl" ///
		7 "eductr" 8 "artssprts" 9 "admin" 10 "agri" 11 "skldmanuf" 12 "unskldmanuf" 13 "office" 14 "sales" 15  "prsnlserv" ///
		16 "domestc" 17 "othserv" 		
	for var occ_17 occup_17: lab value X occlab0 \ recode X 0/4=4
	ren occup_17 occup_14
	lab var occup_14 "14-category occupation"
			
	* 5 categories
	for var occ occup: gen X_5=X
	for var occ_5 occup_5: recode X min/99=1 410/419=2 510/549=3 550/839=4 110/219=5 *=. 
	lab define occlab1 1 "unemployed"  2 "agriculture" 3 "manufacturing" 4 "service" 5 "others"
	for var occ_5 occup_5: lab value X occlab1 /*others=prof., managerial, etc.*/
	lab var occup_5 "5-category occupation"
	ta occup_5, gen(occup)
		
	drop  occ_17 occ_5

* Individual ever an internal migrant
	
	for var  doyr1: recode X  9999=.
	gen mxmig = year>doyr1
	lab value mxmig dummy
	lab var mxmig "an internal migrant?"
	
* Individual cumulative number of US trips
	
	sort year
	replace trip = 0 if age<15
	replace trip = 0 if trip==1 & year<usyr1
	replace trip = 1 if trip==0 & year==usyr1
	bys commun hhnum: gen nbustrips = sum(trip)

	gen itrip=nbustrips
	replace itrip = itrip-1 if trip==1
	lab var itrip "lagged cumulative number of trips"
	gen itripcat = itrip
	recode itripcat 3/10=3 11/max=4
	lab var itrip "lagged cumulative number of trips (cat)"
	
	gen migrant = itrip>0
	lab var migrant "already a US migrant"
	
* First trip

	* Illegal?
	
	gen illegal = usdoc1==8
	replace illegal = . if usdoc1==8888 | usdoc1==9999
	lab var illegal "first trip to the US an illegal trip"
	
	* Time spent in the US
	replace usdur1 = . if usdur1==8888 | usdur1==9999

* Total job duration - cumulative experience
	
	recode jobdur 9999=.   /*in month*/
	
	bys country commun: gen id=_n
	gen jobdurto = . 
	forvalues id=1(1)2 {
		bys  commun hhnum: replace jobdurto=sum(jobdur[_n]) 
	}   
	lab var jobdurto "total cumulative job duration (months)"
	
	gen jobdurtot = jobdurto/12
	lab var jobdurtot "total cumulative job duration (years)"

* Parent a prior migrant

	gen parusmig = (fausmig ==1|mousmig==1)
	lab var parusmig "parent a migrant?"
	lab value parusmig dummy	

* Number of siblings who were prior U.S. migrants
	
	gen nosibusmig=brousmig+sisusmig
	lab var nosibusmig "number of siblings as migrants"

* Value of land - merge with the house_wealth data, vland

	sort  commun hhnum year
	merge commun hhnum year using house_wealth.dta
	drop _merge
	sum  vland  /*several outliers*/
	
	* Categories of land owned - vland
	
	//ssc install egenmore
	
	gen vland_tem=vland_nr if vland_nr>0
	egen vlandcat=xtile(vland_tem), by(year) nq(3)
	lab var vlandcat "3 quantiles of vland"
	replace vlandcat=0 if vland==0
	drop vland_tem
	
	lab define lc 0 "no land" 1 "low" 2 "medium" 3 "high"
	lab value vlandcat lc
	
* Number of rooms -troom

	* Categories of number of properties - troom
	
	gen troom_tem=troom if troom>0
	egen troomcat=xtile(troom_tem), by(year) nq(3)
	lab var troomcat "3 quantiles of troom"
	replace troomcat=0 if troom==0
	drop troom_tem
	
	lab define pc 0 "no property" 1 "low" 2 "medium" 3 "high"
	lab value troomcat pc
	
* Number of businesses owned - tbus

	gen tbuscat=tbus
	lab var tbuscat "at least 1 business?"
	lab value tbuscat dummy
	
* Years

	gen qyear = year
	recode qyear 1965/1969=1 1970/1974=2 1975/1979=3 1980/1984=4 1985/1989=5 1990/1994=6 ///
		1995/1999=7 2000/2004=8 2005/2009=9 2010/2016=10
	gen svyrdist = surveyyr - year
	gen qsdist = svyrdist
	recode qsdist 1/5=1 6/10=2 11/15=3 16/20=4 21/25=5 26/30=6 31/35=7 36/40=8 41/45=9 46/51=10
	
	
* Community variables
*********************

sort  commun
merge commun using commun170_tem.dta
drop _merge

	
* Community population
	
	for any 50 60 70 80 90 00 10: ren COMPOPX compopX 

	gen compop = .
	replace compop = compop50 if year>=1950 & year<1960
	replace compop = compop60 if year>=1960 & year<1970
	replace compop = compop70 if year>=1970 & year<1980
	replace compop = compop80 if year>=1980 & year<1990
	replace compop = compop90 if year>=1990 & year<2000
	replace compop = compop00 if year>=2000 & year<2010
	replace compop = compop10 if year>=2010 
	lab var compop "population of community 50-10"

	gen lnpop = ln(compop)
	lab var lnpop "log of population of community 50-10"

* Political category
	
	ren POLCAT polcat
	recode polcat 1=1 2/3=0 // 1=state capital
	lab define pol 1 "state capital" 0 "cabecera/rancheria"
	lab value polcat pol

* Metropolitan category
	
	ren  METROCAT metrocat
	lab def metro1 1 "metropolitan" 2 "small urban" 3 "town" 4 "rancho"
	lab val metrocat metro1
	ta metrocat, gen(metrocat)
	
	gen metro = (metrocat<=2)
	lab var metro "community in metropolitan or urban area?"
	lab value metro dummy

* % earnings twice the minimum wage
	
	ren  MINX270 minx270
	ren  MINX280 minx280
	ren  MINX290 minx290
	ren  MINX200 minx200
	ren  MINX210 minx210

	gen minx2 = .
	replace minx2 = minx270 if year<1980 & minx270~=8888 & minx270~=9999
	replace minx2 = minx280 if year>=1980 & year<1990 & minx280~=8888 & minx280~=9999
	replace minx2 = minx290 if year>=1990 & year<2000 & minx290~=8888 & minx290~=9999
	replace minx2 = minx200 if year>=2000 & year<2010 & minx200~=8888 & minx200~=9999
	replace minx2 = minx210 if year>=2010 & minx210~=8888 & minx210~=9999

	lab var minx2 "prop. earning twice the min wage"

	* For community=74, minx2 is missing for decades 70 and 80, 
	* equate to 90 values. For community=102, minx2 is missing for 80, take the
	* average of 70 and 90 values.

	replace minx2 = minx290 if minx2==. & commun==74 & year>=1970 & year<1990
	replace minx2 = (minx270 + minx290)/2 if minx2==. & commun==102

* % of male labor force employed in agriculture

	for any 50 60 70 80 90 00 10:  ren  AGRIMX agrimX 

	gen agrim = .
	replace agrim = agrim50 if year>=1950 & year<1960 & agrim50~=8888 & agrim50~=9999
	replace agrim = agrim60 if year>=1960 & year<1970 & agrim60~=8888 & agrim60~=9999
	replace agrim = agrim70 if year>=1970 & year<1980 & agrim70~=8888 & agrim70~=9999
	replace agrim = agrim80 if year>=1980 & year<1990 & agrim80~=8888 & agrim80~=9999
	replace agrim = agrim90 if year>=1990 & year<2000 & agrim90~=8888 & agrim90~=9999
	replace agrim = agrim00 if year>=2000 & year<2010 & agrim00~=8888 & agrim00~=9999
	replace agrim = agrim10 if year>=2010 & agrim10~=8888 & agrim10~=9999
			
	lab var agrim "prop. lf in agriculture 50-00 males"
	
	* For community=74, agri is missing for decades 70 and 80, equate to 90 values
	replace agrim = agrim90  if agrim==. & commun==74 & year>=1970 & year<1990
	
* Is there a primary school in community (municipio)

	ren  YRPRIM yrprim
	gen primary=(year>=yrprim)
	lab var primary "a primary school in the community?"
	lab value primary dummy

* Is there a secondary school in community?

	ren  YRSECON yrsecon
	gen secondary=(year>=yrsecon)
	lab var secondary "a secondary school in the community?"
	lab value secondary dummy

* Is there a bank in community?

	ren  YRBANK1 yrbank1
	gen bank=(year>=yrbank1)
	lab var bank "a bank in the community?"
	lab value bank dummy

* Sampling weights

	ren MXWEIGHT mxweight
	ren USWEIGHT usweight
	recode usweight 8888=.
	
drop compop50-compop00 compop10 agrim50-minx210


* Macroeconomic variables
**************************

sort year 
	 	 
* Merge with macroeconomic variables

merge year using natlyear170.dta
drop _merge
sort year
merge year using natlhist170.dta
drop _merge
		
* Community data and distance variables

sort commun
		
	* Merge with distance data

	merge commun using commun_id.dta
	drop if _merge==2
	drop _merge
	
	gen logdist = ln(distance+1)

	* Rainfall variables
 /*
sort commun year
merge commun year using rainfall_tem
drop _merge
*/


* Prevalence ratio
******************

sort  commun year
merge commun year using pratio170.dta
drop if _merge==2
rename pratio prev
drop if _merge==2 // the only prevalence ratios missing are for years before 1945
drop _merge
	
* Sample correction

	recode statebrn 33/max=.
	for var doyr1 doyrl doplace1 doplacel dostate1 dostatel dodur1 dodurl: recode X  8888/9999=.
	
	gen nonlocal = 0
	replace nonlocal = 1 if statenum!=statebrn & statebrn!=. & doyr1!=.
	gen placeid = statenum*1000+placenum
	
	replace doplace1 = . if doplace1==0
	replace doplacel = . if doplacel==0
	gen place = placeid if nonlocal==0
	replace place = placeid if nonlocal==1 & year>=doyr1 & dostate1*1000+doplace1==placeid & doplace1!=.
	replace place = placeid if nonlocal==1 & year>=doyrl & doplacel!=.
	gen state = statenum if nonlocal==0
	replace state = statenum if nonlocal==1 & year>=doyr1 & dostate1==statenum
	replace state = statenum if nonlocal==1 & year>=doyrl & dostatel==statenum
	
	gen noncount=1 if nonlocal==1 & place!=placeid
	replace noncount=1 if nonlocal==1 & state==. & place==.
	replace noncount=0 if nonlocal==1 & state==statenum & place==.
	recode noncount .=0
	replace noncount=0 if noncount==1 & dostate1==statebrn
	lab var noncount "cases not considered when calculatng ctrip"
		
* Community trip with/without sample bias correction

	sort commun year hhnum
	
	* Without correction
	bys commun year: egen ctrip1=total(itrip) 
	lab var ctrip1 "cumulative us trips by community-year: without correction"
	
	* With correction
	sort  commun  year hhnum
	bys commun year: egen ctrip2=total(itrip) if noncount==0 
	lab var ctrip2 "cumulative us trips by community-year: with correction"
 
* Community trips by different groups
 	 	   
	* Sample attrition
	
keep if age>=15
keep if year>=1965
keep if surveypl==1 | surveypl==2
	 
	* Age group
	for any 1 2 3 4: bys commun year: egen ctrip2_ageX=total(itrip) if noncount==0 & ageg==X
	
	* Education group
	for any 1 2 3 4: bys commun year: egen ctrip2_educX=total(itrip) if noncount==0 & educ==X
	
	* Occupation group
	for any 1 2 3 4 5: bys commun year: egen ctrip2_occuX=total(itrip) if noncount==0 & occup_5==X
	
	* Land group
	for any 0 1 2 3: bys commun year: egen ctrip2_landX=total(itrip) if noncount==0 & vlandcat==X
	
	* Property group
	for any 0 1 2 3 : bys commun year: egen ctrip2_propX=total(itrip) if noncount==0 &  troomcat==X
	
	* Property group
	for any 0 1: bys commun year: egen ctrip2_busX=total(itrip) if noncount==0 &   tbuscat==X

* The size of the groups
 
	* Age group
	for any 1 2 3 4: bys commun year: egen no_ageX=count(ageg) if noncount==0 & ageg==X 
		
	* Education group
	for any 1 2 3 4: bys commun year: egen no_educX=count(educ) if noncount==0 & educ==X
	
	* Occupation group
	for any 1 2 3 4 5: bys commun year: egen no_occuX=count(occup_5) if noncount==0 & occup_5==X
	
	* Land group
	for any 0 1 2 3: bys commun year: egen no_landX=count(vlandcat) if noncount==0 & vlandcat==X
	
	* Property group
	for any 0 1 2 3 : bys commun year: egen no_propX=count(troomcat) if noncount==0 &  troomcat==X
	
	* Property group
	for any 0 1: bys commun year: egen no_busX=count(tbuscat) if noncount==0 &   tbuscat==X
	
	* Fill in the missing values	
	for var   ctrip2_age1-  no_bus1: bys commun year: replace X=X[_n-1] if X==. & noncount==0
	for var   ctrip2_age1-  no_bus1: bys commun year: egen X_mode=mode(X) if noncount==0, max
	for var   ctrip2_age1-  no_bus1:  bys commun year: replace X=X_mode if X==. 

sum ctrip2_age1- no_bus1
drop *_mode
									
* The proportion of the groups
		
		bys commun year: gen tocom=_N if noncount==0
		lab var tocom "the size of community in commun-year, sample correction"
		
		for any 1 2 3 4:  bys commun year:  gen pr_ageX=no_ageX/tocom
		for any 1 2 3 4:  bys commun year:  gen pr_educX=no_educX/tocom
		for any 0 1 2 3:  bys commun year:  gen pr_landX=no_landX/tocom
		for any 0 1 2 3:  bys commun year:  gen pr_propX=no_propX/tocom
		for any 0 1 :  bys commun year:  gen pr_busX=no_busX/tocom
		
save life_hh_tem, replace
		
* Prevalence rates
		
	* Prevalence ratio by community
			
use life_hh_tem, clear
drop if yrborn==.
keep if usyr1>=yrborn+15

keep commun usyr1  noncount
drop if usyr1==.
	
	bys commun usyr1: gen mig = _N 
	bys commun usyr1: gen mig_sc = _N if noncount==0

	ren usyr1 year
	bys commun year: keep if _n==1

	sort commun year
	by commun: gen totmig = sum(mig)
	by commun: gen totmig_sc = sum(mig_sc)

	lab var totmig "total ever migrants (15+ yr old, from pers) in community-year"
	lab var totmig_sc "total ever migrants (15+ yr old, from pers) in community-year, sample correction"

keep commun year totmig totmig_sc
sort commun year
save "life170_totmig.dta", replace

use life_hh_tem, clear
drop if yrborn==.
gen yr15 = yrborn +15
drop if yr15 > 2016

	keep commun yr15 noncount
	bys commun yr15: gen ind = _N 
	bys commun yr15: gen ind_sc = _N if noncount==0

	ren yr15 year
	bys commun year: keep if _n==1

	sort commun year
	by commun: gen totind = sum(ind)
	by commun: gen totind_sc = sum(ind_sc)
	lab var totind "total # of individuals (15+ yr old, from pers) in community-year"
	lab var totind_sc "total # of individuals (15+ yr old, from pers) in community-year, sample correction"

	keep commun year totind totind_sc

	sort commun year
	merge commun year using "life170_totmig.dta"
	drop if _merge==2
	drop _merge

	sort commun year
	by commun: replace totmig = totmig[_n-1] if totmig==. & totmig[_n-1]~=.
	by commun: replace totmig_sc = totmig_sc[_n-1] if totmig_sc==. & totmig_sc[_n-1]~=.

	replace totmig = 0 if totmig==.
	replace totmig_sc = 0 if totmig_sc==.

	gen prevhh1 = totmig/totind
	gen prevhh1_sc = totmig_sc/totind_sc
	lab var prevhh1 "prevalence in community year for heads (from life170)"
	lab var prevhh1_sc "prevalence in community year for heads (from life170), sample correction"

sort commun year
drop  totind totmig  totind_sc totmig_sc
save "life170_prev1.dta", replace

	* Pervalence ratio by placenum

use life_hh_tem.dta, clear
drop if yrborn==.
keep if usyr1>=yrborn+15

	keep placenum usyr1  noncount
	drop if usyr1==.
	bys placenum usyr1: gen mig = _N 
	bys placenum usyr1: gen mig_sc = _N if noncount==0
	
	ren usyr1 year
	bys placenum year: keep if _n==1

	sort placenum year
	by placenum: gen totmig = sum(mig)
	by placenum: gen totmig_sc = sum(mig_sc)
	lab var totmig "total ever migrants (15+ yr old, from pers) in placenum-year"
	lab var totmig_sc "total ever migrants (15+ yr old, from pers) in placenum-year, sample correction"	

keep placenum year totmig totmig_sc
sort placenum year
save "life170_totmig2.dta", replace

use life_hh_tem, clear
drop if yrborn==.

	gen yr15 = yrborn +15
	drop if yr15 > 2016

	keep placenum yr15 noncount
	bys placenum yr15: gen ind = _N 
	bys placenum yr15: gen ind_sc = _N if noncount==0

	ren yr15 year
	bys placenum year: keep if _n==1

	sort placenum year
	by placenum: gen totind = sum(ind)
	by placenum: gen totind_sc = sum(ind_sc)
	lab var totind "total # of individuals (15+ yr old, from pers) in placenum-year"
	lab var totind_sc "total # of individuals (15+ yr old, from pers) in placenum-year, sample correction"

	keep placenum year totind totind_sc 

	sort placenum yea
	merge placenum year using "life170_totmig2.dta"
	drop if _merge==2
	drop _merge

	sort placenum year
	by placenum: replace totmig = totmig[_n-1] if totmig==. & totmig[_n-1]~=.
	by placenum: replace totmig_sc = totmig_sc[_n-1] if totmig_sc==. & totmig_sc[_n-1]~=.

	replace totmig = 0 if totmig==.
	replace totmig_sc = 0 if totmig_sc==.

	gen prevhh2 = totmig/totind
	gen prevhh2_sc = totmig_sc/totind_sc
	lab var prevhh2 "prevalence in placenum year for heads (from life 170)"
	lab var prevhh2_sc "prevalence in placenum year for heads (from life 170), sample correction"

sort placenum year
drop  totind totmig totind_sc totmig_sc
save "life170_prev2.dta", replace
			
use life_hh_tem, clear
sort commun year
merge commun year using life170_prev1.dta
drop if _merge==2
drop _merge
	
	sort placenum year
	merge placenum year using life170_prev2.dta
	drop if _merge==2
	drop _merge

	sort commun year
	for var prevhh1 prevhh1_sc: by commun: replace X = X[_n-1] if X==.

	sort placenum year
	for var  prevhh2 prevhh2_sc:by placenum: replace  X = X[_n-1] if X==.
		
sort commun hhnum year
save life170_hh.dta, replace



* Part II **********************************************************************
* Spouses **********************************************************************
********************************************************************************

use spouse170.dta, clear
drop if commun<33


* Individual variables
**********************

	gen persnum=2  // 2=spouse
	
* Age (time-varying)
	
	gen ageg=age
	recode ageg 15/24=1 25/34=2 35/44=3 45/max=4
	lab define ageg 1 "15-24" 2 "25-34" 3 "35-44" 4 "45+"
	lab value ageg ageg
	lab var ageg "age groups"
	
* Sex - 1=male

	recode sex 2=0
	lab def sex 1 "male" 0 "female"
	lab value sex sex
              
* Education (time-varing)

	* Continuous
	gen edyr=educ 
	recode edyr 9999=. 
	lab var edyr "years of education"

	* Categorical
	recode educ 0/6=1 7/11=2 12/15=3 16/50=4 9999=.
	lab define ed 1 "primary or less" 2 "some secondary" 3 "secondary" 4 "advanced"
	lab value educ ed

* Occupation (time-varying)
	
	* 14 categories
	
	for var occ occup: gen X_17=X
	
	for var occ_17 occup_17: recode X 10=1 20 21=2 42 43=3 30 40 41 50/99=4 110/119=5 120/129=6 ///
		130/139=7 140/149=8  210/219=9 410/419=10 510/539=11 540/549=12 610/629=13 710/719=14 810/819=15 ///
		820=16 550/559 720/729 830/839=17 0=0 *=.

	lab define occlab0 1 "seekwork" 2 "homemkr" 3 "stud" 4 "nowork" 5 "profsnl" 6 "techncl" ///
		7 "eductr" 8 "artssprts" 9 "admin" 10 "agri" 11 "skldmanuf" 12 "unskldmanuf" 13 "office" 14 "sales" 15  "prsnlserv" ///
		16 "domestc" 17 "othserv" 		
	for var occ_17 occup_17: lab value X occlab0 \ recode X 0/4=4
	ren occup_17 occup_14
	lab var occup_14 "14-category occupation"
			
	* 5 categories
	
	for var occ occup: gen X_5=X
	
	for var occ_5 occup_5: recode X min/99=1 410/419=2 510/549=3 550/839=4 110/219=5 *=. 
	lab define occlab1 1 "unemployed"  2 "agriculture" 3 "manufacturing" 4 "service" 5 "others"
	for var occ_5 occup_5: lab value X occlab1 /*others=prof., managerial, etc.*/
	lab var occup_5 "5-category occupation"
	ta occup_5, gen(occup)
		
	drop  occ_17 occ_5

* Individual ever an internal migrant

	for var  doyr1: recode X  9999=.
	gen mxmig=(year> doyr1) 
	lab value mxmig dummy
	lab var mxmig "an internal migrant?"
	
* Individual cumulative number of US trips
	
	sort year
	replace trip = 0 if age<15
	replace trip = 0 if trip==1 & year<usyr1
	replace trip = 1 if trip==0 & year==usyr1
	bys commun hhnum: gen nbustrips = sum(trip)

	gen itrip=nbustrips
	replace itrip = itrip-1 if trip==1
	lab var itrip "lagged cumulative number of trips"
	gen itripcat = itrip
	recode itripcat 3/10=3 11/max=4
	lab var itrip "lagged cumulative number of trips (cat)"
	
	gen migrant = itrip>0
	lab var migrant "already a US migrant"
	
* First trip

	* Illegal?
	
	gen illegal = usdoc1==8
	replace illegal = . if usdoc1==8888 | usdoc1==9999
	lab var illegal "first trip to the US an illegal trip"
	
	* Time spent in the US
	replace usdur1 = . if usdur1==8888 | usdur1==9999

* Total job duration - cumulative experience
	
	recode jobdur 9999=.   /*in month*/
	
	bys country commun: gen id=_n
	gen jobdurto = . 
	forvalues id=1(1)2 {
		bys  commun hhnum: replace jobdurto=sum(jobdur[_n]) 
	}   
	lab var jobdurto "total cumulative job duration (months)"
	
	gen jobdurtot = jobdurto/12
	lab var jobdurtot "total cumulative job duration (years)"

* Value of land - merge with the house_wealth data, vland
	
	sort  commun hhnum year
	merge commun hhnum year using house_wealth.dta
	drop if _merge==2
	drop _merge
	sum  vland  /*several outliers*/
	
	* Categories of land owned - vland
	
	gen vland_tem=vland_nr if vland_nr>0
	egen vlandcat=xtile(vland_tem), by(year) nq(3)
	lab var vlandcat "3 quantiles of vland"
	replace vlandcat=0 if vland==0
	drop vland_tem
	
	lab define lc 0 "no land" 1 "low" 2 "medium" 3 "high"
	lab value vlandcat lc
	
* Number of rooms -troom

	* Categories of number of properties - troom
	
	gen troom_tem=troom if troom>0
	egen troomcat=xtile(troom_tem), by(year) nq(3)
	lab var troomcat "3 quantiles of troom"
	replace troomcat=0 if troom==0
	drop troom_tem
	
	lab define pc 0 "no property" 1 "low" 2 "medium" 3 "high"
	lab value troomcat pc
	
* Number of businesses owned - tbus

	gen tbuscat=tbus
	lab var tbuscat "at least 1 business?"
	lab value tbuscat dummy
	
* Years

	gen qyear = year
	recode qyear 1965/1969=1 1970/1974=2 1975/1979=3 1980/1984=4 1985/1989=5 1990/1994=6 ///
		1995/1999=7 2000/2004=8 2005/2009=9 2010/2016=10
	gen svyrdist = surveyyr - year
	gen qsdist = svyrdist
	recode qsdist 1/5=1 6/10=2 11/15=3 16/20=4 21/25=5 26/30=6 31/35=7 36/40=8 41/45=9 46/51=10
	
	
* Community variables
*********************

sort  commun
merge commun using commun170_tem.dta
drop _merge
	
* Community population

	for any 50 60 70 80 90 00 10: ren COMPOPX compopX 

	gen compop = .
	replace compop = compop50 if year>=1950 & year<1960
	replace compop = compop60 if year>=1960 & year<1970
	replace compop = compop70 if year>=1970 & year<1980
	replace compop = compop80 if year>=1980 & year<1990
	replace compop = compop90 if year>=1990 & year<2000
	replace compop = compop00 if year>=2000 & year<2010
	replace compop = compop10 if year>=2010 
	lab var compop "population of community 50-10"

	gen lnpop = ln(compop)
	lab var lnpop "log of population of community 50-10"

* Political category

	ren POLCAT polcat
	recode polcat 1=1 2/3=0 // 1=state capital
	lab define pol 1 "state capital" 0 "cabecera/rancheria"
	lab value polcat pol

* Metropolitan category

	ren  METROCAT metrocat
	lab def metro1 1 "metropolitan" 2 "small urban" 3 "town" 4 "rancho"
	lab val metrocat metro1
	ta metrocat, gen(metrocat)
	
	gen metro = (metrocat<=2)
	lab var metro "community in metropolitan or urban area?"
	lab value metro dummy

* % earnings twice the minimum wage

	ren  MINX270 minx270
	ren  MINX280 minx280
	ren  MINX290 minx290
	ren  MINX200 minx200
	ren  MINX210 minx210

	gen minx2 = .
	replace minx2 = minx270 if year<1980 & minx270~=8888 & minx270~=9999
	replace minx2 = minx280 if year>=1980 & year<1990 & minx280~=8888 & minx280~=9999
	replace minx2 = minx290 if year>=1990 & year<2000 & minx290~=8888 & minx290~=9999
	replace minx2 = minx200 if year>=2000 & year<2010 & minx200~=8888 & minx200~=9999
	replace minx2 = minx210 if year>=2010 & minx210~=8888 & minx210~=9999

	lab var minx2 "prop. earning twice the min wage"

	* For community=74, minx2 is missing for decades 70 and 80, 
	* equate to 90 values. For community=102, minx2 is missing for 80, take the
	* average of 70 and 90 values.

	replace minx2 = minx290 if minx2==. & commun==74 & year>=1970 & year<1990
	replace minx2 = (minx270 + minx290)/2 if minx2==. & commun==102

* % of male labor force employed in agriculture

	for any 50 60 70 80 90 00 10:  ren  AGRIMX agrimX 

	gen agrim = .
	replace agrim = agrim50 if year>=1950 & year<1960 & agrim50~=8888 & agrim50~=9999
	replace agrim = agrim60 if year>=1960 & year<1970 & agrim60~=8888 & agrim60~=9999
	replace agrim = agrim70 if year>=1970 & year<1980 & agrim70~=8888 & agrim70~=9999
	replace agrim = agrim80 if year>=1980 & year<1990 & agrim80~=8888 & agrim80~=9999
	replace agrim = agrim90 if year>=1990 & year<2000 & agrim90~=8888 & agrim90~=9999
	replace agrim = agrim00 if year>=2000 & year<2010 & agrim00~=8888 & agrim00~=9999
	replace agrim = agrim10 if year>=2010 & agrim10~=8888 & agrim10~=9999
			
	lab var agrim "prop. lf in agriculture 50-00 males"
	
	* For community=74, agri is missing for decades 70 and 80, equate to 90 values
	replace agrim = agrim90  if agrim==. & commun==74 & year>=1970 & year<1990
	
* Is there a primary school in community (municipio)

	ren  YRPRIM yrprim
	gen primary=(year>=yrprim)
	lab var primary "a primary school in the community?"
	lab value primary dummy

* Is there a secondary school in community?

	ren  YRSECON yrsecon
	gen secondary=(year>=yrsecon)
	lab var secondary "a secondary school in the community?"
	lab value secondary dummy

* Is there a bank in community?

	ren  YRBANK1 yrbank1
	gen bank=(year>=yrbank1)
	lab var bank "a bank in the community?"
	lab value bank dummy

* Sampling weights

	ren MXWEIGHT mxweight
	ren USWEIGHT usweight
	recode usweight 8888=.
	
drop compop50-compop00 compop10 agrim50-minx210

	
* Macroeconomic variables
*************************

	sort year 
	 
* Merge with macro-economic variables
	
	merge year using  natlyear170.dta
	
	drop _merge
	sort year
	merge year using  natlhist170.dta
	drop if _merge==2
	drop _merge
	
* Community data and Distance variables
	
	sort commun
	
	* Merge with distance data
	merge commun using commun_id.dta
	drop if _merge==2
	drop _merge
	
	gen logdist = ln(distance+1)

* Rainfall variables
/*
	sort commun year
	merge commun year using rainfall_tem
	drop if _merge==2
	drop _merge
*/


* Prevalence ratio
******************

sort  commun year
merge commun year using pratio170.dta
drop if _merge==2
rename pratio prev
drop if _merge==2 // the only prevalence ratios missing are for years before 1945
drop _merge
	
* Sample correctiom

	recode statebrn 33/max=.
	for var doyr1 doyrl doplace1 doplacel dostate1 dostatel dodur1 dodurl: recode X  8888/9999=.
	
	gen nonlocal = 0
	replace nonlocal = 1 if statenum!=statebrn & statebrn!=. & doyr1!=.
	gen placeid = statenum*1000+placenum
	
	replace doplace1 = . if doplace1==0
	replace doplacel = . if doplacel==0
	gen place = placeid if nonlocal==0
	replace place = placeid if nonlocal==1 & year>=doyr1 & dostate1*1000+doplace1==placeid & doplace1!=.
	replace place = placeid if nonlocal==1 & year>=doyrl & doplacel!=.
	gen state = statenum if nonlocal==0
	replace state = statenum if nonlocal==1 & year>=doyr1 & dostate1==statenum
	replace state = statenum if nonlocal==1 & year>=doyrl & dostatel==statenum
	
	gen noncount=1 if nonlocal==1 & place!=placeid
	replace noncount=1 if nonlocal==1 & state==. & place==.
	replace noncount=0 if nonlocal==1 & state==statenum & place==.
	recode noncount .=0
	replace noncount=0 if noncount==1 & dostate1==statebrn
	lab var noncount "cases not considered when calculatng ctrip"
			
* Community trip with/without sample bias correction

	sort  commun  year hhnum
	
	* Without correction
	bys commun year: egen ctrip1=total(itrip) 
	lab var ctrip1 "cumultive us trips by community-year: without correction"
	
	* With correction
	sort  commun  year hhnum
	bys commun year: egen ctrip2=total(itrip) if noncount==0 
	lab var ctrip2 "cumultive us trips by community-year: with correction"
 
* Community trips by different groups
 	 	   
	* Sample attrition
	
	keep if age>=15
	keep if year>=1965
	keep if surveypl==1 | surveypl==2
 
	* Age group
	for any 1 2 3 4: bys commun year: egen ctrip2_ageX=total(itrip) if noncount==0 & ageg==X
	
	* Education group
	for any 1 2 3 4: bys commun year: egen ctrip2_educX=total(itrip) if noncount==0 & educ==X
	
	* Occupation group
	for any 1 2 3 4 5: bys commun year: egen ctrip2_occuX=total(itrip) if noncount==0 & occup_5==X
	
	* Land group
	for any 0 1 2 3: bys commun year: egen ctrip2_landX=total(itrip) if noncount==0 & vlandcat==X
	
	* Property group
	for any 0 1 2 3 : bys commun year: egen ctrip2_propX=total(itrip) if noncount==0 &  troomcat==X
	
	* Property group
	for any 0 1: bys commun year: egen ctrip2_busX=total(itrip) if noncount==0 &   tbuscat==X
  
* The size of the groups
 
	* Age group
	for any 1 2 3 4: bys commun year: egen no_ageX=count(ageg) if noncount==0 & ageg==X 
		
	* Education group
	for any 1 2 3 4: bys commun year: egen no_educX=count(educ) if noncount==0 & educ==X
	
	* Occupation group
	for any 1 2 3 4 5: bys commun year: egen no_occuX=count(occup_5) if noncount==0 & occup_5==X
	
	* Land group
	for any 0 1 2 3: bys commun year: egen no_landX=count(vlandcat) if noncount==0 & vlandcat==X
	
	* Property group
	for any 0 1 2 3 : bys commun year: egen no_propX=count(troomcat) if noncount==0 &  troomcat==X
	
	* Property group
	for any 0 1: bys commun year: egen no_busX=count(tbuscat) if noncount==0 &   tbuscat==X
	
	* Fill in the missing values	
	for var   ctrip2_age1-  no_bus1: bys commun year: replace X=X[_n-1] if X==. & noncount==0
	for var   ctrip2_age1-  no_bus1: bys commun year: egen X_mode=mode(X) if noncount==0, max
	for var   ctrip2_age1-  no_bus1:  bys commun year: replace X=X_mode if X==. 

	sum ctrip2_age1-no_bus1
	drop *_mode
						
* The proportion of the groups

	bys commun year: gen tocom=_N 
	bys commun year: gen tocom_sc=_N if noncount==0
	lab var tocom "the size of community in commun-year"
	lab var tocom_sc "the size of community in commun-year, sample correction"
	
	for any 1 2 3 4:  bys commun year:  gen pr_ageX=no_ageX/tocom_sc
	for any 1 2 3 4:  bys commun year:  gen pr_educX=no_educX/tocom_sc
	for any 0 1 2 3:  bys commun year:  gen pr_landX=no_landX/tocom_sc
	for any 0 1 2 3:  bys commun year:  gen pr_propX=no_propX/tocom_sc
	for any 0 1 :  bys commun year:  gen pr_busX=no_busX/tocom_sc
		
save spouse_sp_tem, replace
		
* Prevalence rates

	* Pervalence ratio by community
			
use spouse_sp_tem, clear
drop if yrborn==.
keep if usyr1>=yrborn+15

	keep commun usyr1 noncount
	drop if usyr1==.
	bys commun usyr1: gen mig = _N 
	bys commun usyr1: gen mig_sc = _N if noncount==0

	ren usyr1 year
	bys commun year: keep if _n==1

	sort commun year
	by commun: gen totmig = sum(mig)
	by commun: gen totmig_sc = sum(mig_sc)

	lab var totmig "total ever migrants (15+ yr old, from pers) in community-year"
	lab var totmig_sc "total ever migrants (15+ yr old, from pers) in community-year, sample correction"

keep commun year totmig totmig_sc
sort commun year
save "spouse170_totmig.dta", replace

use spouse_sp_tem, clear
drop if yrborn==.
gen yr15 = yrborn +15
drop if yr15 > 2016

	keep commun yr15 noncount
	bys commun yr15: gen ind = _N 
	bys commun yr15: gen ind_sc = _N if noncount==0

	ren yr15 year
	bys commun year: keep if _n==1

	sort commun year
	by commun: gen totind = sum(ind)
	by commun: gen totind_sc = sum(ind_sc)
	lab var totind "total # of individuals (15+ yr old, from pers) in community-year"
	lab var totind_sc "total # of individuals (15+ yr old, from pers) in community-year, sample correction"

	keep commun year totind totind_sc

	sort commun year
	merge commun year using "spouse170_totmig.dta"
	drop if _merge==2
	drop _merge

	sort commun year
	by commun: replace totmig = totmig[_n-1] if totmig==. & totmig[_n-1]~=.
	by commun: replace totmig_sc = totmig_sc[_n-1] if totmig_sc==. & totmig_sc[_n-1]~=.

	replace totmig = 0 if totmig==.
	replace totmig_sc = 0 if totmig_sc==.

	gen prevsp1 = totmig/totind
	gen prevsp1_sc = totmig_sc/totind_sc
	lab var prevsp1 "prevalence in community year for spouses (from spouse170)"
	lab var prevsp1_sc "prevalence in community year for spouses (from spouse170), sample correction"

sort commun year
drop  totind totmig  totind_sc totmig_sc
save "spouse170_prev1.dta", replace
	
	* Pervalence ratio by placenum

use spouse_sp_tem, clear
	
	drop if yrborn==.
	keep if usyr1>=yrborn+15

	keep placenum usyr1  noncount
	drop if usyr1==.
	bys placenum usyr1: gen mig = _N 
	bys placenum usyr1: gen mig_sc = _N if noncount==0
	
	ren usyr1 year
	bys placenum year: keep if _n==1

	sort placenum year
	by placenum: gen totmig = sum(mig)
	by placenum: gen totmig_sc = sum(mig_sc)
	lab var totmig "total ever migrants (15+ yr old, from pers) in placenum-year"
	lab var totmig_sc "total ever migrants (15+ yr old, from pers) in placenum-year, sample correction"	

keep placenum year totmig totmig_sc
sort placenum year
save "spouse170_totmig2.dta", replace

use spouse_sp_tem, clear
drop if yrborn==.

	gen yr15 = yrborn +15
	drop if yr15 > 2016

	keep placenum yr15 noncount
	bys placenum yr15: gen ind = _N 
	bys placenum yr15: gen ind_sc = _N if noncount==0

	ren yr15 year
	bys placenum year: keep if _n==1

	sort placenum year
	by placenum: gen totind = sum(ind)
	by placenum: gen totind_sc = sum(ind_sc)
	lab var totind "total # of individuals (15+ yr old, from pers) in placenum-year"
	lab var totind_sc "total # of individuals (15+ yr old, from pers) in placenum-year, sample correction"

	keep placenum year totind totind_sc 

	sort placenum yea
	merge placenum year using "spouse170_totmig2.dta"
	drop if _merge==2
	drop _merge

	sort placenum year
	by placenum: replace totmig = totmig[_n-1] if totmig==. & totmig[_n-1]~=.
	by placenum: replace totmig_sc = totmig_sc[_n-1] if totmig_sc==. & totmig_sc[_n-1]~=.

	replace totmig = 0 if totmig==.
	replace totmig_sc = 0 if totmig_sc==.

	gen prevsp2 = totmig/totind
	gen prevsp2_sc = totmig_sc/totind_sc
	lab var prevsp2 "prevalence in placenum year for spouses (from spouse134)"
	lab var prevsp2_sc "prevalence in placenum year for spouses (from spouse134), sample correction"

sort placenum year
drop  totind totmig totind_sc totmig_sc
save "spouse170_prev2.dta", replace

use spouse_sp_tem, clear
sort commun year
merge commun year using spouse170_prev1.dta
drop if _merge==2
drop _merge
	
		sort placenum year
		merge placenum year using spouse170_prev2.dta
		drop if _merge==2
		drop _merge
	
		sort commun year
		for var prevsp1 prevsp1_sc: by commun: replace X = X[_n-1] if X==. 

		sort placenum year
		for var prevsp2 prevsp2_sc: by placenum: replace X = X[_n-1] if X==. 
		
sort commun hhnum year
save spouse170_sp.dta, replace



* Part III *********************************************************************
* All individuals **************************************************************
********************************************************************************

* Household variables

use life170_hh, clear
keep year commun hhnum cebsofar vlandcat troomcat
sort commun hhnum year
save lifetopers, replace

use pers170, clear

* Expand the data set

	gen exp1 = surveyyr-1965+1
	expand exp1
	sort commun hhnum persnum
	bys commun hhnum persnum: gen year = surveyyr if _n==1
	bys commun hhnum persnum: replace year = surveyyr-_n+1 if _n>1
	
* Merge with household data

	sort commun hhnum year
	merge m:1 commun hhnum year using lifetopers
	keep if _merge==3
	drop _merge
	

* Individual variables
**********************

* Position in the household

	replace relhead = relhead==1 | relhead==2
	lab var relhead "head of the household or his/her spouse" 
	lab define dummy 1 "yes" 0 "no"
	lab value relhead dummy

* Age (time-varying)

	drop age
	gen age = year-yrborn
	drop if age<15
	gen ageg=age
	recode ageg 15/24=1 25/34=2 35/44=3 45/max=4
	lab define ageg 1 "15-24" 2 "25-34" 3 "35-44" 4 "45+"
	lab value ageg ageg
	lab var ageg "age groups"
	
* Sex - 1=male

	recode sex 2=0
	lab def sex 1 "male" 0 "female"
	lab value sex sex
	
* Number of children in the household (time-varing) - cebsofar

* Education (time-varing)

	* Reconstitution of age (panel)
	replace edyrs = . if edyrs==9999 | edyrs==8888
	gen endedage=6+edyrs-1
	
	* Continuous
	gen edyr=edyrs
	replace edyr=edyrs-(endedage-age) if age<endedage
	replace edyr=0 if edyrs==0
	lab var edyr "years of education"
	
	* Categorical
	gen educ=edyr
	recode educ 0/6=1 7/11=2 12/15=3 16/50=4 9999=.
	lab define ed 1 "primary or less" 2 "some secondary" 3 "secondary" 4 "advanced"
	lab value educ ed
	
* Occupation

	* 14 categories
	gen occ_17=occ
	recode occ_17 10=1 20 21=2 42 43=3 30 40 41 50/99=4 110/119=5 120/129=6 ///
		130/139=7 140/149=8  210/219=9 410/419=10 510/539=11 540/549=12 610/629=13 710/719=14 810/819=15 ///
		820=16 550/559 720/729 830/839=17 0=0 *=.
	lab define occlab0 1 "seekwork" 2 "homemkr" 3 "stud" 4 "nowork" 5 "profsnl" 6 "techncl" ///
		7 "eductr" 8 "artssprts" 9 "admin" 10 "agri" 11 "skldmanuf" 12 "unskldmanuf" 13 "office" 14 "sales" 15  "prsnlserv" ///
		16 "domestc" 17 "othserv" 		
	lab value occ_17 occlab0
	recode occ_17 0/4=4
	ren occ_17 occup_14
	lab var occup_14 "14-category occupation"
			
	* 5 categories
	gen occup_5=occ
	recode occup_5 min/99=1 410/419=2 510/549=3 550/839=4 110/219=5 *=. 
	lab define occlab1 1 "unemployed"  2 "agriculture" 3 "manufacturing" 4 "service" 5 "others"
	lab value occup_5 occlab1 /*others=prof., managerial, etc.*/
	lab var occup_5 "5-category occupation"
	ta occup_5, gen(occup)

* Individual ever an internal migrant
	
	recode doyr1 9999=.
	gen mxmig = year>doyr1
	lab value mxmig dummy
	lab var mxmig "an internal migrant?"
	
* Flag the year of first trip and years after first trip
	
	recode usyr1 9999=.
	
	gen firsttrip = year==usyr1
	lab value firsttrip dummy
	lab var firsttrip "the first trip occured that year"
	
	gen usmig = year>usyr1
	lab value usmig dummy
	lab var usmig "year after first trip"
	
	bys commun hhnum year: egen sum_hhusmig = total(usmig)
	gen hh_usmig = sum_hhusmig>1
	replace hh_usmig = 1 if sum_hhusmig==1 & usmig==0
	lab var hh_usmig "any former US migrant in the hh?"
	
* First trip

	* Illegal?
	
	gen illegal = usdoc1==8
	replace illegal = . if usdoc1==8888 | usdoc1==9999
	lab var illegal "first trip to the US an illegal trip"
	
	* Time spent in the US
	replace usdur1 = . if usdur1==8888 | usdur1==9999

* Value of land - merge with the house_wealth data, vland

	sort  commun hhnum year
	merge m:1 commun hhnum year using house_wealth.dta
	drop if _merge==2
	drop _merge
	sum  vland  /*several outliers*/
	
* Number of rooms -troom
	
* Number of businesses owned - tbus
	
	gen tbuscat=tbus
	lab var tbuscat "at least 1 business?"
	lab value tbuscat dummy
	
* Years

	gen qyear = year
	recode qyear 1965/1969=1 1970/1974=2 1975/1979=3 1980/1984=4 1985/1989=5 1990/1994=6 ///
		1995/1999=7 2000/2004=8 2005/2009=9 2010/2016=10
	gen svyrdist = surveyyr - year
	gen qsdist = svyrdist
	recode qsdist 1/5=1 6/10=2 11/15=3 16/20=4 21/25=5 26/30=6 31/35=7 36/40=8 41/45=9 46/51=10

	
* Community variables
*********************

sort  commun
merge commun using commun170_tem.dta
drop _merge
	
* Community population
	
	for any 50 60 70 80 90 00 10: ren COMPOPX compopX 

	gen compop = .
	replace compop = compop50 if year>=1950 & year<1960
	replace compop = compop60 if year>=1960 & year<1970
	replace compop = compop70 if year>=1970 & year<1980
	replace compop = compop80 if year>=1980 & year<1990
	replace compop = compop90 if year>=1990 & year<2000
	replace compop = compop00 if year>=2000 & year<2010
	replace compop = compop10 if year>=2010 
	lab var compop "population of community 50-10"

	gen lnpop = ln(compop)
	lab var lnpop "log of population of community 50-10"

* Political category
	
	ren POLCAT polcat
	recode polcat 1=1 2/3=0 // 1=state capital
	lab define pol 1 "state capital" 0 "cabecera/rancheria"
	lab value polcat pol

* Metropolitan category
	
	ren  METROCAT metrocat
	lab def metro1 1 "metropolitan" 2 "small urban" 3 "town" 4 "rancho"
	lab val metrocat metro1
	ta metrocat, gen(metrocat)
	
	gen metro = (metrocat<=2)
	lab var metro "community in metropolitan or urban area?"
	lab value metro dummy

* % earnings twice the minimum wage
	
	ren  MINX270 minx270
	ren  MINX280 minx280
	ren  MINX290 minx290
	ren  MINX200 minx200
	ren  MINX210 minx210

	gen minx2 = .
	replace minx2 = minx270 if year<1980 & minx270~=8888 & minx270~=9999
	replace minx2 = minx280 if year>=1980 & year<1990 & minx280~=8888 & minx280~=9999
	replace minx2 = minx290 if year>=1990 & year<2000 & minx290~=8888 & minx290~=9999
	replace minx2 = minx200 if year>=2000 & year<2010 & minx200~=8888 & minx200~=9999
	replace minx2 = minx210 if year>=2010 & minx210~=8888 & minx210~=9999

	lab var minx2 "prop. earning twice the min wage"

	* For community=74, minx2 is missing for decades 70 and 80, 
	* equate to 90 values. For community=102, minx2 is missing for 80, take the
	* average of 70 and 90 values.

	replace minx2 = minx290 if minx2==. & commun==74 & year>=1970 & year<1990
	replace minx2 = (minx270 + minx290)/2 if minx2==. & commun==102

* % of male labor force employed in agriculture

	for any 50 60 70 80 90 00 10:  ren  AGRIMX agrimX 

	gen agrim = .
	replace agrim = agrim50 if year>=1950 & year<1960 & agrim50~=8888 & agrim50~=9999
	replace agrim = agrim60 if year>=1960 & year<1970 & agrim60~=8888 & agrim60~=9999
	replace agrim = agrim70 if year>=1970 & year<1980 & agrim70~=8888 & agrim70~=9999
	replace agrim = agrim80 if year>=1980 & year<1990 & agrim80~=8888 & agrim80~=9999
	replace agrim = agrim90 if year>=1990 & year<2000 & agrim90~=8888 & agrim90~=9999
	replace agrim = agrim00 if year>=2000 & year<2010 & agrim00~=8888 & agrim00~=9999
	replace agrim = agrim10 if year>=2010 & agrim10~=8888 & agrim10~=9999
			
	lab var agrim "prop. lf in agriculture 50-00 males"
	
	* For community=74, agri is missing for decades 70 and 80, equate to 90 values
	replace agrim = agrim90  if agrim==. & commun==74 & year>=1970 & year<1990
	
* Is there a primary school in community (municipio)

	ren  YRPRIM yrprim
	gen primary=(year>=yrprim)
	lab var primary "a primary school in the community?"
	lab value primary dummy

* Is there a secondary school in community?

	ren  YRSECON yrsecon
	gen secondary=(year>=yrsecon)
	lab var secondary "a secondary school in the community?"
	lab value secondary dummy

* Is there a bank in community?

	ren  YRBANK1 yrbank1
	gen bank=(year>=yrbank1)
	lab var bank "a bank in the community?"
	lab value bank dummy

* Sampling weights

	ren MXWEIGHT mxweight
	ren USWEIGHT usweight
	recode usweight 8888=.
	
drop compop50-compop00 compop10 agrim50-minx210


* Macroeconomic variables
**************************

sort year 
	 	 
* Merge with macroeconomic variables

merge year using natlyear170.dta
drop _merge
sort year
merge year using natlhist170.dta
drop _merge
		
* Community data and distance variables

sort commun
		
	* Merge with distance data

	merge commun using commun_id.dta
	drop if _merge==2
	drop _merge
	
	gen logdist = ln(distance+1)

	* Rainfall variables
 /*
sort commun year
merge commun year using rainfall_tem
drop _merge
*/


* Prevalence ratio
******************

sort  commun year
merge commun year using pratio170.dta
drop if _merge==2
rename pratio prev
drop if _merge==2 // the only prevalence ratios missing are for years before 1945
drop _merge

* Sample correction

	recode statebrn 33/max=.
	for var doyr1 doyrl doplace1 doplacel dostate1 dostatel dodur1 dodurl: recode X  8888/9999=.
	
	gen nonlocal = 0
	replace nonlocal = 1 if statenum!=statebrn & statebrn!=. & doyr1!=.
	gen placeid = statenum*1000+placenum
	
	replace doplace1 = . if doplace1==0
	replace doplacel = . if doplacel==0
	gen place = placeid if nonlocal==0
	replace place = placeid if nonlocal==1 & year>=doyr1 & dostate1*1000+doplace1==placeid & doplace1!=.
	replace place = placeid if nonlocal==1 & year>=doyrl & doplacel!=.
	gen state = statenum if nonlocal==0
	replace state = statenum if nonlocal==1 & year>=doyr1 & dostate1==statenum
	replace state = statenum if nonlocal==1 & year>=doyrl & dostatel==statenum
	
	gen noncount=1 if nonlocal==1 & place!=placeid
	replace noncount=1 if nonlocal==1 & state==. & place==.
	replace noncount=0 if nonlocal==1 & state==statenum & place==.
	recode noncount .=0
	replace noncount=0 if noncount==1 & dostate1==statebrn
	lab var noncount "cases not considered when calculatng ctrip"
	
* Prevalence ratio by household

	gen evermigrant = year>usyr1
	bys commun hhnum year: gen tothh = _N
	bys commun hhnum year: egen totmighh = total(evermigrant)
	gen revermigranthh = totmighh/tothh
	gen evermigranthh = totmighh>0
	gen logevermghh = ln(revermigranthh*100+1)
	
	preserve
	
		bys commun hhnum year: gen hhindic = _n==1
		keep if hhindic==1
		keep commun hhnum year revermigranthh evermigranthh logevermghh
		
		sort commun hhnum year
		save migranthh, replace
		
	restore
	
save pers170_temp, replace

* Prevalence ratio by community

	* Compute the total number of migrants in each year

	use pers170_temp.dta, clear
	drop if yrborn==.
	keep if usyr1>=yrborn+15

	* Compute the total number of migrants in each year of first migration
	
	keep commun usyr1  noncount
	drop if usyr1==.
	bys commun usyr1: gen mig = _N 
	bys commun usyr1: gen mig_sc = _N if noncount==0
		
		* Keep one observation per community-year - then compute cumulative
		* number of migrants using sum() command

	ren usyr1 year

	bys commun year: keep if _n==1

	sort commun year
	by commun: gen totmig = sum(mig)
	by commun: gen totmig_sc = sum(mig_sc)
	
	lab var totmig "total ever migrants (15+ yr old, from pers) in community-year"
	lab var totmig_sc "total ever migrants (15+ yr old, from pers) in community-year, sample correction"

	keep commun year totmig totmig_sc
	sort commun year
	save pers170_totmig, replace

	* Second, compute the total number of 15+ year olds in each year

	use pers170_temp, clear
	drop if yrborn==.

	* Compute the total number of individuals who turn 15 in each year

	gen yr15 = yrborn +15
	drop if yr15 > 2016

	keep commun yr15 noncount
	bys commun yr15: gen ind = _N 
	bys commun yr15: gen ind_sc = _N if noncount==0

		* Keep one observation per community-year - then compute cumulative
		* number of individuals using sum() command

	ren yr15 year
	bys commun year: keep if _n==1

	sort commun year
	by commun: gen totind = sum(ind)
	by commun: gen totind_sc = sum(ind_sc)
	lab var totind "total # of individuals (15+ yr old, from pers) in community-year"
	lab var totind_sc "total # of individuals (15+ yr old, from pers) in community-year, sample correction"

	keep commun year totind totind_sc

	* Merge to the total migrant data

	sort commun year
	merge commun year using pers170_totmig
	drop if _merge==2
	drop _merge

		* _merge==1 are observations where there are missing years in the totmig data
		* recall these data were created based on the years migrants were observed. 
		* Equate the values in missing years to those in the preceding year.

	sort commun year
	by commun: replace totmig = totmig[_n-1] if totmig==. & totmig[_n-1]~=.
	by commun: replace totmig_sc = totmig_sc[_n-1] if totmig_sc==. & totmig_sc[_n-1]~=.

		* The remaining missing values for totmig are due to the absence of any  migrants
		* - all in initial years before there is a first migrant from community - set to 0.

	replace totmig = 0 if totmig==.
	replace totmig_sc = 0 if totmig_sc==.

	gen prev1 = totmig/totind
	gen prev1_sc = totmig_sc/totind_sc
	lab var prev1 "prevalence in community  year (from pers)"
	lab var prev1_sc "prevalence in community in year ( from pers), sample correction"
	
	sort commun year
	bys commun: gen dprev = prev1_sc[_n-1]
	gen dprevcat = dprev
	recode dprevcat min/.1=1 .10001/.2=2 .20001/.3=3 .30001/max=4

	sort commun year
	drop  totind totmig  totind_sc totmig_sc
	save pers170_prev1, replace
	
* Prevalence ratio by placenum

	use pers170_temp, clear
	drop if yrborn==.
	keep if usyr1>=yrborn+15

	* Compute the total number of migrants in each year of first migration

	keep placenum usyr1  noncount
	drop if usyr1==.
	bys placenum usyr1: gen mig = _N 
	bys placenum usyr1: gen mig_sc = _N if noncount==0
		
	ren usyr1 year
	bys placenum year: keep if _n==1
	
	sort placenum year
	by placenum: gen totmig = sum(mig)
	by placenum: gen totmig_sc = sum(mig_sc)
	lab var totmig "total ever migrants (15+ yr old, from pers) in placenum-year"
	lab var totmig_sc "total ever migrants (15+ yr old, from pers) in placenum-year, sample correction"	

	keep placenum year totmig totmig_sc
	sort placenum year
	save pers170_totmig2, replace

	* Second, compute the total number of 15+ year olds in each year

	use pers170_temp, clear
	drop if yrborn==.

	* Compute the total number of individuals who turn 15 in each year

	gen yr15 = yrborn +15
	drop if yr15 > 2016
	
	keep placenum yr15 noncount
	bys placenum yr15: gen ind = _N 
	bys placenum yr15: gen ind_sc = _N if noncount==0

	* Keep one observation per community-year - then compute cumulative
	* number of individuals using sum() command

	ren yr15 year
	bys placenum year: keep if _n==1

	sort placenum year
	by placenum: gen totind = sum(ind)
	by placenum: gen totind_sc = sum(ind_sc)
	lab var totind "total # of individuals (15+ yr old, from pers) in placenum-year"
	lab var totind_sc "total # of individuals (15+ yr old, from pers) in placenum-year, sample correction"
	
	keep placenum year totind totind_sc 

	* Merge to the total migrant data

	sort placenum year
	merge placenum year using pers170_totmig2
	drop if _merge==2
	drop _merge

		* _merge==1 are observations where there are missing years in the totmig data
		* recall these data were created based on the years migrants were observed. 
		* Equate the values in missing years to those in the preceding year.

	sort placenum year
	by placenum: replace totmig = totmig[_n-1] if totmig==. & totmig[_n-1]~=.
	by placenum: replace totmig_sc = totmig_sc[_n-1] if totmig_sc==. & totmig_sc[_n-1]~=.

		* The remaining missing values for totmig are due to the absence of any  migrants
		* - all in initial years before there is a first migrant from community - set to 0.

	replace totmig = 0 if totmig==.
	replace totmig_sc = 0 if totmig_sc==.

	gen prev2 = totmig/totind
	gen prev2_sc = totmig_sc/totind_sc
	lab var prev2 "prevalence in placenum year (from pers170)"
	lab var prev2_sc "prevalence in placenum year (from pers170), sample correction"
	
	sort placenum year
	bys placenum: gen dprev2 = prev2_sc[_n-1]
	gen dprevcat2 = dprev2
	recode dprevcat2 min/.1=1 .10001/.2=2 .20001/.3=3 .30001/max=4

	sort placenum year
	drop  totind totmig totind_sc totmig_sc
	save pers170_prev2, replace
	
* Merge prevalence ratio

use pers170_temp, clear

	sort commun year
	merge commun year using pers170_prev1
	drop if _merge==2
	drop _merge

	sort placenum year
	merge placenum year using pers170_prev2
	drop if _merge==2
	drop _merge

		* prev1/prev2is missing in certain years - i.e., when there were no individuals
		* who turned 15. Equate those years to the prior year. First keep one observation
		* for community-year

	sort commun year
	for var prev1 prev1_sc: by commun: replace X = X[_n-1] if X==. 

	sort placenum year
	for var prev2 prev2_sc: by placenum: replace X = X[_n-1] if X==. 
	
save ind170, replace


	
* Part IV **********************************************************************
* Append and merge LIFE and SPOUSE *********************************************
********************************************************************************

* Prevalence ratio computed on all individuals
**********************************************

* LIFE

use life170_hh, clear

	sort commun hhnum year
	merge commun hhnum year using migranthh
	drop if _merge==2
	drop _merge

	sort commun year
	merge commun year using pers170_prev1
	drop if _merge==2
	drop _merge

	sort placenum year
	merge placenum year using pers170_prev2
	drop if _merge==2
	drop _merge

		* prev1/prev2is missing in certain years - i.e., when there were no individuals
		* who turned 15. Equate those years to the prior year. First keep one observation
		* for community-year

	sort commun year
	for var prev1 prev1_sc: by commun: replace X = X[_n-1] if X==. 

	sort placenum year
	for var prev2 prev2_sc: by placenum: replace X = X[_n-1] if X==. 
	
save life170_hh, replace

* SPOUSE

use  spouse170_sp, clear

	sort commun hhnum year
	merge commun hhnum year using migranthh
	drop if _merge==2
	drop _merge

	sort commun year
	merge commun year using pers170_prev1
	drop if _merge==2
	drop _merge

	sort placenum year
	merge placenum year using pers170_prev2
	drop if _merge==2
	drop _merge

		* prev1/prev2/prev3 is missing in certain years - i.e., when there were no individuals
		* who turned 15. Equate those years to the prior year. First keep one observation
		* for community-year

	sort commun year
	for var prev1 prev1_sc: by commun: replace X = X[_n-1] if X==. 

	sort placenum year
	for var prev2 prev2_sc: by placenum: replace X = X[_n-1] if X==. 

save spouse170_sp, replace


* Append LIFE and SPOUSE
************************

use life170_hh, clear

	gen relhead = 1
	append using spouse170_sp
	replace relhead = 0 if relhead==.

	* Spouse is a migrant
	bys commun hhnum year: egen sum_spusmig = total(migrant)
	gen sp_migrant = sum_spusmig>1
	replace sp_migrant = 1 if sum_spusmig==1 & migrant==0
	
	* Number of children in the household
	bys commun hhnum year: egen sum_cebsofar = total(cebsofar)
	replace cebsofar = sum_cebsofar if cebsofar==.
	
	* Married
	bys commun hhnum year: egen sum_marry = total(marry)
	replace marry = sum_marry>0 if marry==.
	
save couples170, replace



* Part V ***********************************************************************
* Working Data Set *************************************************************
********************************************************************************

* Couples170
************

use couples170, clear

	
* Event-history variables

	* Dependent variables
	
	gen mig = trip==1					// any migration trip
	gen migf = trip==1 & itrip==0		// first migration trip
	gen migrep = trip==1 & itrip>0		// repeated migration trips
	
	* Samples
				
	gen ss1 = trip==1 | inus==0					// all person-years spent in Mexico
	gen ss2 = itrip==0 & (trip==1 | inus==0)	// before first trip person-years
	gen ss3 = itrip>0 & (trip==1 | inus==0)		// after first trip person-years
	
	* Event-history
	
	drop persnum
	egen persnum = group(commun hhnum relhead)
	
	gen firstrisk = yrborn+15
	gen firstobs = 1965
	replace firstobs = firstrisk if firstrisk>1965
	gen timezero = year-1
	
	
save couples170_w2, replace


* Ind170
********

use ind170, clear

	
* Event-history variables

	* Dependent variable
	
	gen migf = firsttrip==1		// first migration trip
	
	* Sample
				
	gen ss = usmig==0		// before first trip person-years
	
	* Event-history
	
	rename persnum persnum_or
	egen persnum = group(commun hhnum persnum_or)
	
	gen firstrisk = yrborn+15
	gen firstobs = 1965
	replace firstobs = firstrisk if firstrisk>1965
	gen timezero = year-1
	

* Community means

	* Education
	
	recode educ 27=4
	ta educ, gen(educ)
	for var educ1-educ4: bys commun year: egen X_cy = mean(X)
	
	* Age

	gen age1 = age<=24
	gen age2 = age>24 & age<=35
	gen age3 = age>35 & age<=60
	gen age4 = age>60
	for var age1-age4: bys commun year: egen X_cy = mean(X)
	
	* Sex

	bys commun year: egen sex_cy = mean(sex)
	
	* Occupation

	for var occup1-occup4: bys commun year: egen X_cy = mean(X)
	
	* Land tenure

	ta vlandcat, gen(land)
	for var land1-land4: bys commun year: egen X_cy = mean(X)
	
	* Housing tenure

	ta troomcat, gen(room)
	for var room1-room4: bys commun year: egen X_cy = mean(X)
	
	* Business

	bys commun year: egen bus_cy = mean(tbuscat)
	

* Select 50 percent of individuals

	gen odd = mod(persnum,2)
	
* Flag community-years with less than 100 individuals in the sample

	bys commun year: gen nb_cy = _N
	gen smallcy = nb_cy<100
	
save ind170, replace

