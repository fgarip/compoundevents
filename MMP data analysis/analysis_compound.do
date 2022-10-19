******************************************
******* 2. Models & tables & figures *****
******************************************

* edited by Filiz Garip 
* last update 1 jul 22, 8am

clear 
capture drop _all
global data "/Users/fgarip/Dropbox/Analysis/filiz_mig_env_analysis_jan2019/data" 
global results "/Users/fgarip/Dropbox/Analysis/filiz_mig_env_analysis_jan2019/compound-events-analysis/results"

set more off
set scheme lean2

* 1. Bring in HOUSE data to measure household work arrangements *
*****************************************************************

use $data/house170.dta, clear

lab def life 1 "no child" 2 "all child<13" 3 "some teenagers" 4 "all teenagers" 5 "all adults"
lab val lifecyc life

gen hocc_agri = 0
replace hocc_agri = 1 if hocc>=410 & hocc<=419
lab var hocc_agri "head works in agri?"

replace famw = . if famw==9999 // no of family members working on farming
replace famw = 0 if famw==8888 // set it to 0 for non-farming families

replace jornw = . if jornw==9999  // no of day laborers working on farming
replace jornw = 0 if jornw==8888 // set it 0 for non-farming families

replace machine =  0 if machine==2 | machine==8888
replace machine =  . if machine==9999

replace fertiliz =  0 if fertiliz==2 | fertiliz==8888
replace fertiliz =  . if fertiliz==9999

replace insectic =  0 if insectic==2 | insectic==8888
replace insectic =  . if insectic==9999

replace farmdol =  0 if farmdol==2 | farmdol==8888
replace farmdol =  . if farmdol==9999


sort commun hhnum
keep commun hhnum members workers nonwork lifecyc hocc_agri famw jornw machine fertiliz insectic farmdol
save $data/house170_red.dta, replace




	// merge to full data generated in 1_cleaning_compound.do
	
use $data/ind170_analysis_u.dta, clear

sort commun hhnum
merge m:1 commun hhnum using $data/house170_red.dta 
drop if _merge==2
drop _merge  // about 5K observations only in the master data

save $data/ind170_analysis_u_temp.dta, replace


* 2. Bring household level remittance data in *
***********************************************

	// merge to MIG & MIGOTHER data to get remittance information

use $data/mig170.dta, clear
replace remit = . if remit==9999
replace savretrn = . if savretrn==9999
replace usyrl = . if usyrl==9999
gen husyrl = usyrl
sort commun hhnum 
save $data/mig170a.dta, replace

use $data/migother170.dta, clear
replace remit = . if remit==9999
replace savretrn = . if savretrn==9999
replace usyrl = . if usyrl==9999
gen husyrlo = usyrl
sort commun hhnum
save $data/migother170a.dta, replace

	// we are merging remittance variable at the household level
	// b/c we are interested in if hh ever received remittances up to a given year
	
use $data/ind170_analysis_u_temp.dta, clear
sort commun hhnum  
merge m:1 commun hhnum using $data/mig170a.dta, keepusing(remit savretrn husyrl)
drop if _merge==2
drop _merge

codebook remit savretrn husyrl

	// get values from migother data for remittances only if the remit/savretrn are missing
	// for a household - using the "update" option 
	
sort commun hhnum  
merge m:1 commun hhnum using $data/migother170a.dta, keepusing(husyrlo)
drop if _merge==2
drop _merge
	
sort commun hhnum  
merge m:1 commun hhnum using $data/migother170a.dta, keepusing(remit savretrn) update
drop if _merge==2
drop _merge

	// generate indicator for if hh ever received remittances through that year

gen husyrle = .								// earliest year of migration of a household member
replace husyrle = husyrl  if husyrl~=.		// pick the mig from MIG or MIGOTHER - whomever migrated earlier
replace husyrle = husyrlo if husyrle==. & husyrlo~=.
replace husyrle = husyrlo if husyrlo<husyrl & husyrl~=. & husyrlo~=. 

gen hremit = 0
replace hremit = 1 if (remit~=. | savretrn~=.) & year>=husyrle
lab var hremit "hh ever received remittances?"

bys commun year: egen cremit = mean(hremit)
lab var cremit "share of ind w prior access to remit in community"

* 3. Create the key indicators * 
*********************************

	// internal migration - currently . if person never migrated internally
	// set to zero
	
replace fmxmig = 0 if fmxmig==.

	// create a new variable - person 'on' first mexican migration trip
	
gen onfmxmig = fmxmig

forvalues  i=1(1)80 {
replace onfmxmig = 1 if onfmxmig==0 & year==doyr1+`i' & dodur1>12*`i'
}

lab var onfmxmig "on first internal migration trip in year"

	// prevalence of internal migration

	
gen evermxmig =  0
replace evermxmig = 1 if year>doyr1 & doyr1~=.

bys commun hhnum year: egen hh_mxmig = total(evermxmig)
replace hh_mxmig = hh_mxmig - evermxmig
replace hh_mxmig = (hh_mxmig>0)
lab var hh_mxmig "any mx migrants in hh"

bys commun year: egen mxprev = mean(evermxmig)
lab var mxprev "community prevalence of internal migration (lagged)"

xtile mxprevcat = mxprev, nq(3)
gen lowmxprev = mxprevcat==1
gen medmxprev = mxprevcat==2
gen himxprev  = mxprevcat==3


	// weather categories 

/*
gen dry = L1_precp_apraug_cat==2 | L1_precp_apraug_cat==3
gen hot = L1_cdays30p_apraug_cat==4 | L1_cdays30p_apraug_cat==5
gen wet = L1_precp_apraug_cat==4 | L1_precp_apraug_cat==5
gen cool = L1_cdays30p_apraug_cat==2 | L1_cdays30p_apraug_cat==3

gen dry2 = L2_precp_apraug_cat==2 | L2_precp_apraug_cat==3
gen hot2 = L2_cdays30p_apraug_cat==4 | L2_cdays30p_apraug_cat==5
gen wet2 = L2_precp_apraug_cat==4 | L2_precp_apraug_cat==5
gen cool2 = L2_cdays30p_apraug_cat==2 | L2_cdays30p_apraug_cat==3

gen vdry = L1_precp_apraug_cat==2 
gen vhot = L1_cdays30p_apraug_cat==5
gen vwet = L1_precp_apraug_cat==5
gen vcool = L1_cdays30p_apraug_cat==2 

gen dry2 = L2_precp_apraug_cat==2 | L2_precp_apraug_cat==3
gen hot2 = L2_cdays30p_apraug_cat==4 | L2_cdays30p_apraug_cat==5
gen wet2 = L2_precp_apraug_cat==4 | L2_precp_apraug_cat==5
gen cool2 = L2_cdays30p_apraug_cat==2 | L2_cdays30p_apraug_cat==3
*/


gen dry = L1_precp_seasona_cat==2 | L1_precp_seasona_cat==3
gen hot = L1_cdays30p_seasona_cat==4 | L1_cdays30p_seasona_cat==5
gen wet = L1_precp_seasona_cat==4 | L1_precp_seasona_cat==5
gen cool = L1_cdays30p_seasona_cat==2 | L1_cdays30p_seasona_cat==3

gen normr = L1_precp_seasona_cat==1
gen normt = L1_cdays30p_seasona_cat==1

gen vdry = L1_precp_seasona_cat==2 
gen vhot = L1_cdays30p_seasona_cat==5
gen vwet = L1_precp_seasona_cat==5
gen vcool = L1_cdays30p_seasona_cat==2

gen dry2 = L2_precp_seasona_cat==2 | L2_precp_seasona_cat==3
gen hot2 = L2_cdays30p_seasona_cat==4 | L2_cdays30p_seasona_cat==5
gen wet2 = L2_precp_seasona_cat==4 | L2_precp_seasona_cat==5
gen cool2 = L2_cdays30p_seasona_cat==2 | L2_cdays30p_seasona_cat==3


gen dryy = L1_precp_year_cat==2 | L1_precp_year_cat==3
gen hoty = L1_cdays30p_year_cat==4 | L1_cdays30p_year_cat==5
gen wety = L1_precp_year_cat==4 | L1_precp_year_cat==5
gen cooly = L1_cdays30p_year_cat==2 | L1_cdays30p_year_cat==3

gen normry = L1_precp_year_cat==1
gen normty = L1_cdays30p_year_cat==1

gen dryy2 = L2_precp_year_cat==2 | L2_precp_year_cat==3
gen hoty2 = L2_cdays30p_year_cat==4 | L2_cdays30p_year_cat==5
gen wety2 = L2_precp_year_cat==4 | L2_precp_year_cat==5
gen cooly2 = L2_cdays30p_year_cat==2 | L2_cdays30p_year_cat==3

	// no of unique households and individuals

egen personid = group(commun hhnum persnum)
egen hhid = group(commun hhnum)
gen head = relhead==1

	// assets
	
gen assets = troomcat+vlandcat+tbuscat
gen assets0 = assets>0

	// alternative cuts for troomcat

xtile troomc = troom if troom>0, nq(2)
replace troomc = 0 if troomc==.

lab def tro 0 "no prop" 1 "low" 2 "high"
lab val troomc tro

	// irrigation

gen missirg = 0
replace missirg = 1 if shareirg==.
lab var missirg "missing irrigation info"

gen shareirg_nm = shareirg
replace shareirg_nm = 0 if shareirg==.
lab var shareirg_nm "share irrigated (missing=0)"

gen shareirg0 = shareirg==0
lab var shareirg0 "no irrigation"

gen shareirg25 = shareirg<=0.25
lab var shareirg25 "less than 25% irrigated"


	// household diversity of occupations

bys commun hhnum year: egen hagri  = total(occup2)
bys commun hhnum year: egen hmanuf = total(occup3)
bys commun hhnum year: egen hserv  = total(occup4)
bys commun hhnum year: egen hoth   = total(occup5)

gen hnagri = hmanuf + hserv + hoth

gen shagri = hagri/(hagri+hnagri)
replace shagri = 0 if shagri==.

lab var hagri  "# of hh in agri work"
lab var hnagri "# of hh in non-agri work"
lab var shagri "share hh agri/total work"

gen shagri0 = shagri==0
gen shagrilh = shagri>0 & shagri<0.5
gen shagrimh = shagri>=0.5 & shagri<1
gen shagria  = shagri==1
gen shagris = shagri>0 & shagri<1

lab var shagri0 "hh in agri"
lab var shagrilh "< half hh in agri"
lab var shagrimh "> half in agri"
lab var shagria "all hh in agri"
lab var shagris "some hh in agri"

	// community irrigation
	
gen shareirg30 = shareirg>=0.30 & shareirg~=.

save $data/ind170_analysis_u_temp.dta, replace


* 4. Merge state-level counts of extreme weather events * 
*********************************************************

use $data/ind170_analysis_u_temp.dta, clear

// spatially compounding events

bys commun year: keep if _n==1

local var "dry hot wet cool"

foreach v of local var {
bys statenum year: egen sh`v's  = total(`v')  // compute total number of communities experiencing dry weather in state
bys statenum year: egen sh`v'ys = total(`v'y)
replace sh`v's  = sh`v's-`v'                    // exclude index community
replace sh`v'ys = sh`v'ys-`v'y          
gen `v's1  = sh`v's>=1    
gen `v'ys1 = sh`v'ys>=1    
lab var `v's1 "any other commun in state w weather (seasonal)"
lab var `v'ys1 "any other commun in state w weather (annual)"
bys statenum year: replace sh`v's  = sh`v's/(_N-1)	// compute share
bys statenum year: replace sh`v'ys = sh`v'ys/(_N-1)						
lab var sh`v's  "share in state excl comm (seasonal)"
lab var sh`v'ys "share in state excl comm (annual)"

gen `v's50  = sh`v's>=0.5   
gen `v'ys50 = sh`v'ys>=0.5   
sum sh`v's sh`v'ys	
}

keep commun year shdrys shhots shwets shcools shdryys shhotys shwetys shcoolys *s1 *s50

sort commun year
save $data/state-weather.dta, replace


use $data/ind170_analysis_u_temp.dta, clear
sort commun year
merge m:1 commun year using $data/state-weather
drop _merge

save $data/ind170_analysis_compound.dta, replace


* 5. Descriptives *
*******************
	
use $data/ind170_analysis_compound.dta, clear


egen communyear = group(commun year)


		// descriptive: means

set logtype text

log using $results/means.txt, replace

qui reghdfe fmxmig dryy hoty wety cooly ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)

gen s = 1 if e(sample)
	
	
	* Sample size and composition

codebook statenum if s==1
codebook year if s==1

codebook commun if s==1
codebook commun if agrim>=0.45 
codebook commun if agrim<0.45 
codebook communyear if s==1

codebook hhid if s==1

codebook personid if s==1
codebook personid if s==1 & fmxmig==1

sum fmxmig dryy normry wety hoty normty cooly dry normr wet hot normt cool age sex head edyr shagri0 shagris shagria hh_mxmig hh_usmig agrim mxprev lowmxprev medmxprev himxprev prev shareirg shareirg0 if s==1, sep(50)

tab troomc, gen(t)
sum t1 t2 t3

log cl

	// descriptive: share of migrants over time

lgraph fmxmig year if s==1, stat(mean) title(Share of individuals on first internal migration trip in Mexico)
graph export $results/fmxmig-year.png, replace


* 6. Statistical models *
*************************

use $data/ind170_analysis_compound.dta, clear

// baseline //
//////////////

	* outcome: whether a person takes a first internal migration trip
	* drop individuals after the year of first migration

	* annual weather measures
	
eststo b1: reghdfe fmxmig dryy hoty wety cooly ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/baseline.xls, cti("weather") dec(4) noparen  replace

eststo b2: reghdfe fmxmig dryy hoty wety cooly ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/baseline.xls, cti("weather") dec(4) noparen append

	* seasonal weather measures
	
eststo b3: reghdfe fmxmig dry hot wet cool ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/baseline.xls, cti("weather") dec(4) noparen append

eststo b4: reghdfe fmxmig dry hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/baseline.xls, cti("weather") dec(4) noparen append


/*
	* outcome: whether a person is on a first internal migration trip
	
eststo b5: reghdfe onfmxmig dry hot wet cool ///
if agrim>=0.45, absorb(year##statenum) vce(cluster commun)
*outreg2 using $results/baseline.xls, cti("weather") dec(4) noparen append


eststo b6: reghdfe onfmxmig dry hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev shareirg0 if agrim>=0.45, absorb(year##statenum) vce(cluster commun)
*outreg2 using $results/baseline.xls, cti("weather") dec(4) noparen  append
*/


esttab b1 b2 b3 b4, label r2	///
		title(Linear prob model of taking first internal trip) ///
		nonumbers mtitles("annual" "+control" "seasonal" "+control") star(* 0.1 ** 0.05) 

		
		/// REVISIONS: Analysis by time

eststo b4x: reghdfe fmxmig i.year dry hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(statenum) vce(cluster commun)

coefplot, keep(*.year) xline(0) ylabel(,labsize(vsmall)) levels(95 90) legend(order(1 "95%" 2 "90%") rows(1) pos(6))
graph export $results/year-coef.png, replace

eststo b4x: reghdfe fmxmig i.year#i.dry hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(statenum) vce(cluster commun)

coefplot, keep(*.year#*) nolabel xline(0) ylabel(,labsize(vsmall)) levels(95 90) legend(order(1 "95%" 2 "90%") rows(1) pos(6))
graph export $results/dry-by-year-coef.png, replace


// interactions //
//////////////////

	* hh livelihood strategies & weather impact
	* annual weather measures
	
eststo i1: reghdfe fmxmig i.dryy##i.troomc hoty wety cooly ///
age sex head edyr shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/interactions.xls, cti("weather") dec(4) noparen replace

	* seasonal weather measures
	
eststo i2: reghdfe fmxmig i.dry##i.troomc hot wet cool ///
age sex head edyr shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/interactions.xls, cti("weather") dec(4) noparen append

	* hh involvement in agri & weather impact
	* annual weather measures
	
eststo i3: reghdfe fmxmig i.dryy##i.shagri0 i.dryy##i.shagris hoty wety cooly ///
age sex head edyr i.troomc hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/interactions.xls, cti("weather") dec(4) noparen append

	* seasonal weather measures
	
eststo i4: reghdfe fmxmig i.dry##i.shagri0 i.dry##i.shagris  hot wet cool ///
age sex head edyr i.troomc hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/interactions.xls, cti("weather") dec(4) noparen append


	* community vulnerabilities & weather impact
	* annual weather measures
	
eststo i5: reghdfe fmxmig i.dryy##i.shareirg0 hoty wety cooly ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev  ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/interactions.xls, cti("weather") dec(4) noparen  append

	* seasonal weather measures
	
eststo i6: reghdfe fmxmig i.dry##i.shareirg0 hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev  ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/interactions.xls, cti("weather") dec(4) noparen append


esttab i1 i2 i3 i4 i5 i6, label r2	///
		title(Linear prob model of taking first internal trip) ///
		nonumbers mtitles("a-hwealth int" "s-hwealth int" "a-hagri int" "s-hagri int" "a-irrig int" "s-irrig int") star(* 0.1 ** 0.05) 



// temporally compounding events ///
////////////////////////////////////

	* annual weather measures

eststo t1: reghdfe fmxmig i.dryy##i.dryy2 hoty wety cooly ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/temp-compound.xls, cti("weather") dec(4) noparen replace


eststo t2: reghdfe fmxmig i.dryy##i.dryy2 hoty wety cooly ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg<=0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/temp-compound.xls, cti("weather") dec(4) noparen append


eststo t3: reghdfe fmxmig i.dryy##i.dryy2 hoty wety cooly ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg>0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/temp-compound.xls, cti("weather") dec(4) noparen append


	* seasonal weather measures
	

eststo t4: reghdfe fmxmig i.dry##i.dry2 hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/temp-compound.xls, cti("weather") dec(4) noparen append


eststo t5: reghdfe fmxmig i.dry##i.dry2 hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg<=0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/temp-compound.xls, cti("weather") dec(4) noparen append


eststo t6: reghdfe fmxmig i.dry##i.dry2 hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg>0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/temp-compound.xls, cti("weather") dec(4) noparen append


esttab t1 t2 t3 t4 t5 t6, label r2	///
		title(Linear prob model of taking first tinternal rip) ///
		nonumbers mtitles("a-dry-dry" "irrig<25%" "irrig>25%" "s-dry-dry" "irrig<25%" "irrig>25%") star(* 0.1 ** 0.05) 

		
// spatially compounding events ///
////////////////////////////////////

	* annual weather measures

eststo s1: reghdfe fmxmig dry shdrys hoty wety cooly  ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/spatial-compound.xls, cti("weather") dec(4) noparen replace

	* seasonal weather measures

eststo s2: reghdfe fmxmig dry shdrys hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/spatial-compound.xls, cti("weather") dec(4) noparen append


eststo s3: reghdfe fmxmig dry shdrys hot wet cool shdrys ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg<=0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/spatial-compound.xls, cti("weather") dec(4) noparen append


eststo s4: reghdfe fmxmig dry shdrys hot wet cool shdrys ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg>0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/spatial-compound.xls, cti("weather") dec(4) noparen append

	* seasonal weather measures with binary state-level indicator

eststo s5: reghdfe fmxmig dry drys1 hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/spatial-compound.xls, cti("weather") dec(4) noparen append


eststo s6: reghdfe fmxmig dry drys1 hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg<=0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/spatial-compound.xls, cti("weather") dec(4) noparen append


eststo s7: reghdfe fmxmig dry drys1 hot wet cool ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1) & shareirg>0.25, absorb(year##statenum) vce(cluster commun)
outreg2 using $results/spatial-compound.xls, cti("weather") dec(4) noparen append


esttab s1 s2 s3 s4, label r2	///
		title(Linear prob model of taking first internal trip) ///
		nonumbers mtitles("spatial-annual" "seasonal" "irrig<=0.25" "irrig>0.25") star(* 0.1 ** 0.05) 


// multivariate compounding events ///
////////////////////////////////////

	* annual weather measures

eststo m1: reghdfe fmxmig i.dryy##i.hoty wety cooly  ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/multi-compound.xls, cti("weather") dec(4) noparen replace


	* seasonal weather measures
	
eststo m2: reghdfe fmxmig i.dry##i.hot wet cool  ///
age sex head edyr i.troomc shagri0 shagris hh_mxmig hh_usmig ///
agrim medmxprev himxprev prev shareirg0 ///
if agrim>=0.45 & (doyr1==. | year<=doyr1), absorb(year##statenum) vce(cluster commun)
outreg2 using $results/multi-compound.xls, cti("weather") dec(4) noparen append


esttab m1 m2, label r2	///
		title(Linear prob model of taking first internal trip) ///
		nonumbers mtitles("dry x hot - annual" "seasonal") star(* 0.1 ** 0.05) 

