********************
* 1. Data cleaning *
********************


* only the essentials kept
* edited by Filiz Garip 
* last update 7 jan 2022, 2pm


clear 
global data "/Users/fgarip/Dropbox/Analysis/filiz_mig_env_analysis_jan2019/data" 
set more off

* A. obtain geocodes of MMP communities *
*****************************************

import excel $data\original\MMP170_updated.xlsx, firstrow case(lower)
rename no commun
rename clavegeoestadística geocode5
drop if _n>170
destring commun, replace
format geocode %12.0g
keep commun geocode*
save $data/mmp170_communities.dta, replace


* B. clean MMP170 individual-level data *
*****************************************

use $data/original/ind170_w, clear

* enter missing distance values

replace distance = 783010.23 if commun==161
replace distance = 764196.998 if commun==162
replace distance = 805106.523 if commun==163
replace distance = 872859.905 if commun==164
replace distance = 854642.131 if commun==165
replace distance = 836311.703 if commun==166
replace distance = 1784762 if commun==167
replace distance = 1773496.59 if commun==168
replace distance = 561661 if commun==169
replace distance = 562803.69 if commun==170
replace logdist = ln(distance) if logdist==.

* enter missing statenum

replace statenum = 21 if commun==161
replace statenum = 17 if commun>=162 & commun<=166
replace statenum = 31 if commun==167 | commun==168
replace statenum = 22 if commun==169 | commun==170

* create first domestic migration var

gen fmxmig=0
replace fmxmig=1 if doyr1==year
replace fmxmig=. if doyr1==.

save $data/ind170_w, replace


* merge individual-level data with geocodes

use $data/mmp170_communities.dta, clear

merge 1:m commun using $data/ind170_w.dta

drop _merge
save $data/ind170_analysis.dta, replace


* C. clean and merge environment data *
***************************************

* clean temperature data from DayMet

* (I) days over 30c

clear
import delimited using $data/environment/days30p170_monthly.csv, varnames(1)
save $data/environment/days30p170_monthly.dta, replace

use $data/environment/days30p170_monthly.dta, clear

reshape long tmaxjan tmaxfeb tmaxmar tmaxapr tmaxmay tmaxjun tmaxjul tmaxago tmaxsep tmaxoct tmaxnov tmaxdec, i(geocode) j(year)

* need last year's nov-dec values for seasonal variation

gen tmaxnovl = tmaxnov
gen tmaxdecl = tmaxdec

sort geocode year
by geocode: replace tmaxnovl =  tmaxnov[_n-1] if _n>1
by geocode: replace tmaxdecl =  tmaxdec[_n-1] if _n>1

* create annual and seasonal vars [Seasons based on sowing of corn: Apr-Aug main season (rainfed), Nov-Feb secondary season (irrigated)]

gen days30p_year=tmaxmar+tmaxapr+tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb
gen days30p_apraug=tmaxapr+tmaxmay+tmaxjun+tmaxjul+tmaxago
gen days30p_novfeb=tmaxnovl+tmaxdecl+tmaxjan+tmaxfeb
gen days30p_rest=tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb+tmaxmar

* alternative seasons for corn based on the University of Wisconsin crop calendar by region (NW, Yucatan, other)
* the first set of months capture median planting date to median harvesting date (or month in which the median day falls)
* the second set of months include the entire planting and harvesting period
* the third set of months include the planting to the beginning of the harvesting period.

gen days30p_octfeb = tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb
gen days30p_juljan = tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan
gen days30p_maynov = tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov

gen days30p_sepmar = tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxjun+tmaxmar
gen days30p_junfeb = tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb
gen days30p_maydec = tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec

gen days30p_sepdec = tmaxsep+tmaxoct+tmaxnov+tmaxdec
gen days30p_junoct = tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct
gen days30p_mayoct = tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct


* create days over 30c deviation from 80-90 norm

local season "year apraug novfeb rest octfeb juljan maynov sepmar junfeb maydec sepdec junoct mayoct"

foreach s of local season {
	
	*create weather treatment measures: deviation from 10-year temperature norm

	bys geocode: egen days30p_norm_`s'=mean(days30p_`s') if year>=1980 & year<=1990
	bys geocode: egen days30p_sd_`s'=sd(days30p_`s') if year>=1980 & year<=1990
	bys geocode: egen days30p80to90_mean_`s'=max(days30p_norm_`s')
	bys geocode: egen days30p80to90_sd_`s'=max(days30p_sd_`s')
	// for few observations the standard deviation is zero -- add a small amount to avoid
	// divide-by-zero error
	replace days30p80to90_sd_`s' = days30p80to90_sd_`s' + 0.1 if days30p80to90_sd_`s'==0 
	bys geocode: gen days30p_dev_`s'=(days30p_`s'-days30p80to90_mean_`s')/days30p80to90_sd_`s'
	
	drop days30p_norm_`s' days30p_sd_`s' days30p80to90_sd_`s'

	*create categories of temperature: deviation from 80-90 norm
	
	gen days30p_`s'_cat=1 // within 1 sd below and above norm
	replace days30p_`s'_cat=2 if days30p_dev_`s'<-2  //severely cold
	replace days30p_`s'_cat=3 if days30p_dev_`s'>=-2 & days30p_dev_`s'<-1  //cold
	replace days30p_`s'_cat=4 if days30p_dev_`s'>1 & days30p_dev_`s'<=2 //hot
	replace days30p_`s'_cat=5 if days30p_dev_`s'>2  //severely hot
	replace days30p_`s'_cat=. if days30p_dev_`s'==.
	}

save $data/environment/days30p170_monthly_clean.dta, replace


* (II) consecutive days over 30c

clear
import delimited using $data/environment/cdays30p170_monthly.csv, varnames(1)
save $data/environment/cdays30p170_monthly.dta, replace

use $data/environment/cdays30p170_monthly.dta, clear

reshape long tmaxjan tmaxfeb tmaxmar tmaxapr tmaxmay tmaxjun tmaxjul tmaxago tmaxsep tmaxoct tmaxnov tmaxdec, i(geocode) j(year)

* need last year's nov-dec values for seasonal variation

gen tmaxnovl = tmaxnov
gen tmaxdecl = tmaxdec

sort geocode year
by geocode: replace tmaxnovl =  tmaxnov[_n-1] if _n>1
by geocode: replace tmaxdecl =  tmaxdec[_n-1] if _n>1

* create seasonal vars
gen cdays30p_year=tmaxmar+tmaxapr+tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb
gen cdays30p_apraug=tmaxapr+tmaxmay+tmaxjun+tmaxjul+tmaxago
gen cdays30p_novfeb=tmaxnovl+tmaxdecl+tmaxjan+tmaxfeb
gen cdays30p_rest=tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb+tmaxmar

* alternative seasons for corn based on the University of Wisconsin crop calendar by region (NW, Yucatan, other)
* the first set of months capture median planting date to median harvesting date (or month in which the median day falls)
* the second set of months include the entire planting and harvesting period

gen cdays30p_octfeb = tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb
gen cdays30p_juljan = tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan
gen cdays30p_maynov = tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov

gen cdays30p_sepmar = tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxjun+tmaxmar
gen cdays30p_junfeb = tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec+tmaxjan+tmaxfeb
gen cdays30p_maydec = tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct+tmaxnov+tmaxdec

gen cdays30p_sepdec = tmaxsep+tmaxoct+tmaxnov+tmaxdec
gen cdays30p_junoct = tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct
gen cdays30p_mayoct = tmaxmay+tmaxjun+tmaxjul+tmaxago+tmaxsep+tmaxoct


* create days over 30c deviation from 80-90 norm
local season "year apraug novfeb rest octfeb juljan maynov sepmar junfeb maydec sepdec junoct mayoct"

foreach s of local season {
	*create weather treatment measures: deviation from 10-year temperature norm
	bys geocode: egen cdays30p_norm_`s'=mean(cdays30p_`s') if year>=1980 & year<=1990
	bys geocode: egen cdays30p_sd_`s'=sd(cdays30p_`s') if year>=1980 & year<=1990
	bys geocode: egen cdays30p80to90_mean_`s'=max(cdays30p_norm_`s')
	bys geocode: egen cdays30p80to90_sd_`s'=max(cdays30p_sd_`s')
	// for many observations the standard deviation is zero -- add a small amount to avoid
	// divide-by-zero error
	replace cdays30p80to90_sd_`s' = cdays30p80to90_sd_`s' + 0.1 if cdays30p80to90_sd_`s'==0 
	bys geocode: gen cdays30p_dev_`s'=(cdays30p_`s'-cdays30p80to90_mean_`s')/cdays30p80to90_sd_`s'
	
	drop cdays30p_norm_`s' cdays30p_sd_`s' cdays30p80to90_mean_`s' cdays30p80to90_sd_`s'

	*create categories of temperature: deviation from 80-90 norm
	gen cdays30p_`s'_cat=1 // within 1 sd below and above norm
	replace cdays30p_`s'_cat=2 if cdays30p_dev_`s'<-2  //severely cold
	replace cdays30p_`s'_cat=3 if cdays30p_dev_`s'>=-2 & cdays30p_dev_`s'<-1  //cold
	replace cdays30p_`s'_cat=4 if cdays30p_dev_`s'>1 & cdays30p_dev_`s'<=2 //hot
	replace cdays30p_`s'_cat=5 if cdays30p_dev_`s'>2  //severely hot
	replace cdays30p_`s'_cat=. if cdays30p_dev_`s'==.
	}

save $data/environment/cdays30p170_monthly_clean.dta, replace

* (III) rainfall

clear
import delimited using $data/environment/precp170_monthly.csv, varnames(1)
save $data/environment/precp170_monthly.dta, replace

use $data/environment/precp170_monthly.dta, clear

reshape long prcpjan prcpfeb prcpmar prcpapr prcpmay prcpjun prcpjul prcpago prcpsep prcpoct prcpnov prcpdec, i(geocode) j(year)

* need last year's nov-dec values for seasonal variation

gen prcpnovl = prcpnov
gen prcpdecl = prcpdec

sort geocode year
by geocode: replace prcpnovl =  prcpnov[_n-1] if _n>1
by geocode: replace prcpdecl =  prcpdec[_n-1] if _n>1

* create seasonal rainfall vars
gen precp_year=prcpmar+prcpapr+prcpmay+prcpjun+prcpjul+prcpago+prcpsep+prcpoct+prcpnov+prcpdec+prcpjan+prcpfeb
gen precp_apraug=prcpapr+prcpmay+prcpjun+prcpjul+prcpago
gen precp_novfeb=prcpnovl+prcpdecl+prcpjan+prcpfeb
gen precp_rest=prcpsep+prcpoct+prcpnov+prcpdec+prcpjan+prcpfeb+prcpmar


* alternative seasons for corn based on the University of Wisconsin crop calendar by region (NW, Yucatan, other)
* the first set of months capture median planting date to median harvesting date (or month in which the median day falls)
* the second set of months include the entire planting and harvesting period

gen precp_octfeb = prcpoct+prcpnov+prcpdec+prcpjan+prcpfeb
gen precp_juljan = prcpjul+prcpago+prcpsep+prcpoct+prcpnov+prcpdec+prcpjan
gen precp_maynov = prcpmay+prcpjun+prcpjul+prcpago+prcpsep+prcpoct+prcpnov

gen precp_sepmar = prcpsep+prcpoct+prcpnov+prcpdec+prcpjan+prcpjun+prcpmar
gen precp_junfeb = prcpjun+prcpjul+prcpago+prcpsep+prcpoct+prcpnov+prcpdec+prcpjan+prcpfeb
gen precp_maydec = prcpmay+prcpjun+prcpjul+prcpago+prcpsep+prcpoct+prcpnov+prcpdec

gen precp_sepdec = prcpsep+prcpoct+prcpnov+prcpdec
gen precp_junoct = prcpjun+prcpjul+prcpago+prcpsep+prcpoct
gen precp_mayoct = prcpmay+prcpjun+prcpjul+prcpago+prcpsep+prcpoct



* create precipitation deviation from 80-90 norm
local season "year apraug novfeb rest octfeb juljan maynov sepmar junfeb maydec sepdec junoct mayoct"
foreach s of local season {
	bys geocode: egen p10_norm_`s'=mean(precp_`s') if year>=1980 & year<=1990
	bys geocode: egen p10_sd_`s'=sd(precp_`s') if year>=1980 & year<=1990
	bys geocode: egen p10_mean_`s'=max(p10_norm_`s')
	bys geocode: egen p80to90_sd_`s'=max(p10_sd_`s')
	bys geocode: gen p80to90_dev_`s'=(precp_`s'-p10_mean_`s')/p80to90_sd_`s'

	// there are no obs w standard deviation zero -- still keep the code to 
	// add a small amount to avoid divide-by-zero error
	replace p80to90_sd_`s' = p80to90_sd_`s' + 0.1 if p80to90_sd_`s'==0 
	drop p10_norm_`s' p10_sd_`s' p10_mean_`s' p80to90_sd_`s'
	
	gen precp_`s'_cat=1 // within 1 sd below and above norm
	replace precp_`s'_cat=2 if p80to90_dev_`s'<-2  //severely drought
	replace precp_`s'_cat=3 if p80to90_dev_`s'>=-2 & p80to90_dev_`s'<-1  //drought
	replace precp_`s'_cat=4 if p80to90_dev_`s'>1 & p80to90_dev_`s'<=2 //wet
	replace precp_`s'_cat=5 if p80to90_dev_`s'>2  //severely wet
	replace precp_`s'_cat=. if p80to90_dev_`s'==.

	}

save $data/environment/precp170_monthly_clean.dta, replace


* D. Merge with individual-level data *
***************************************

use $data/ind170_analysis, clear

* merge temperature data
merge m:1 geocode year using $data/environment/days30p170_monthly_clean.dta
drop if _merge==2
drop _merge

merge m:1 geocode year using $data/environment/cdays30p170_monthly_clean.dta
drop if _merge==2
drop _merge

* merge crainfall data
merge m:1 geocode year using $data/environment/precp170_monthly_clean.dta
drop if _merge==2
drop _merge

save $data/ind170_analysis, replace

* add var ejido to analysis data
use $data/original/commun170.dta, clear
rename COMMUN commun
rename EJIDO ejido

*recode ejido to be 0/1, originally yes=1 no=2 unknown=9999
replace ejido=0 if ejido==2
keep commun ejido 

merge 1:m commun using $data/ind170_analysis.dta
drop _merge
save $data/ind170_analysis, replace



* E. Weather categories for intensification analysis *
******************************************************

use $data/ind170_analysis.dta, clear

* weathercat: normal=0, bad=1, good=2
local season "year apraug novfeb rest octfeb juljan maynov sepmar junfeb maydec sepdec junoct mayoct"
foreach s of local season {
	gen weathercat_`s'=0
	replace weathercat_`s'=1 if (precp_`s'_cat==2 & cdays30p_`s'_cat==5)|(precp_`s'_cat==2 & cdays30p_`s'_cat==4)|(precp_`s'_cat==3 & cdays30p_`s'_cat==5)
	replace weathercat_`s'=2 if (precp_`s'_cat==5 & cdays30p_`s'_cat==2)|(precp_`s'_cat==5 & cdays30p_`s'_cat==3)|(precp_`s'_cat==4 & cdays30p_`s'_cat==2)
}

* weathercat_a: normal=0, very bad=1, bad=2, good=3, very good=4, mixed=5

local season "year apraug novfeb rest octfeb juljan maynov sepmar junfeb maydec sepdec junoct mayoct"
foreach s of local season {
	gen weathercata_`s'=5
	replace weathercata_`s'=1 if (precp_`s'_cat==2 & cdays30p_`s'_cat==5)|(precp_`s'_cat==2 & cdays30p_`s'_cat==4)|(precp_`s'_cat==3 & cdays30p_`s'_cat==5)
	replace weathercata_`s'=2 if (precp_`s'_cat==1 & cdays30p_`s'_cat==2)|(precp_`s'_cat==1 & cdays30p_`s'_cat==3)|(precp_`s'_cat==4 & cdays30p_`s'_cat==1)|(precp_`s'_cat==5 & cdays30p_`s'_cat==1)|(precp_`s'_cat==4 & cdays30p_`s'_cat==3)
	replace weathercata_`s'=3 if (precp_`s'_cat==2 & cdays30p_`s'_cat==1)|(precp_`s'_cat==3 & cdays30p_`s'_cat==1)|(precp_`s'_cat==1 & cdays30p_`s'_cat==4)|(precp_`s'_cat==1 & cdays30p_`s'_cat==5)|(precp_`s'_cat==3 & cdays30p_`s'_cat==4)
	replace weathercata_`s'=4 if (precp_`s'_cat==5 & cdays30p_`s'_cat==2)|(precp_`s'_cat==5 & cdays30p_`s'_cat==3)|(precp_`s'_cat==4 & cdays30p_`s'_cat==2)
	replace weathercata_`s'=0 if (precp_`s'_cat==1 & cdays30p_`s'_cat==1)
}

* weathercat_b: normal or mixed=0, very bad=1, bad=2, good=3, very good=4

local season "year apraug novfeb rest octfeb juljan maynov sepmar junfeb maydec sepdec junoct mayoct"
foreach s of local season {
	gen weathercatb_`s'=0
	replace weathercatb_`s'=1 if (precp_`s'_cat==2 & cdays30p_`s'_cat==5)|(precp_`s'_cat==2 & cdays30p_`s'_cat==4)|(precp_`s'_cat==3 & cdays30p_`s'_cat==5)
	replace weathercatb_`s'=2 if (precp_`s'_cat==1 & cdays30p_`s'_cat==2)|(precp_`s'_cat==1 & cdays30p_`s'_cat==3)|(precp_`s'_cat==4 & cdays30p_`s'_cat==1)|(precp_`s'_cat==5 & cdays30p_`s'_cat==1)|(precp_`s'_cat==4 & cdays30p_`s'_cat==3)
	replace weathercatb_`s'=3 if (precp_`s'_cat==2 & cdays30p_`s'_cat==1)|(precp_`s'_cat==3 & cdays30p_`s'_cat==1)|(precp_`s'_cat==1 & cdays30p_`s'_cat==4)|(precp_`s'_cat==1 & cdays30p_`s'_cat==5)|(precp_`s'_cat==3 & cdays30p_`s'_cat==4)
	replace weathercatb_`s'=4 if (precp_`s'_cat==5 & cdays30p_`s'_cat==2)|(precp_`s'_cat==5 & cdays30p_`s'_cat==3)|(precp_`s'_cat==4 & cdays30p_`s'_cat==2)
}

* create vars for lagged and lead weathers
xtset persnum year

local cat "precp_year_cat days30p_year_cat cdays30p_year_cat weathercat_year weathercata_year weathercatb_year"
foreach c of local cat {
	gen L1_`c'=L.`c'
	gen L2_`c'=L2.`c'
	gen F1_`c'=F.`c'
	gen F2_`c'=F2.`c'
	gen F3_`c'=F3.`c'
	gen F4_`c'=F4.`c'
	}

local cat "precp_apraug_cat precp_novfeb_cat precp_rest_cat days30p_apraug_cat days30p_novfeb_cat days30p_rest_cat cdays30p_apraug_cat cdays30p_novfeb_cat cdays30p_rest_cat"
foreach c of local cat {
	gen L1_`c'=L.`c'
	gen L2_`c'=L2.`c'
	gen F1_`c'=F.`c'
	gen F2_`c'=F2.`c'
	gen F3_`c'=F3.`c'
	gen F4_`c'=F4.`c'
	}

	
local cat "precp_octfeb_cat precp_juljan_cat precp_maynov_cat precp_sepmar_cat precp_junfeb_cat precp_maydec_cat precp_sepdec_cat precp_junoct_cat precp_mayoct_cat" 
foreach c of local cat {
	gen L1_`c'=L.`c'
	gen L2_`c'=L2.`c'
	gen F1_`c'=F.`c'
	gen F2_`c'=F2.`c'
	gen F3_`c'=F3.`c'
	gen F4_`c'=F4.`c'
	}


local cat "days30p_octfeb_cat days30p_juljan_cat days30p_maynov_cat days30p_sepmar_cat days30p_junfeb_cat days30p_maydec_cat days30p_sepdec_cat days30p_junoct_cat days30p_mayoct_cat" 
foreach c of local cat {
	gen L1_`c'=L.`c'
	gen L2_`c'=L2.`c'
	gen F1_`c'=F.`c'
	gen F2_`c'=F2.`c'
	gen F3_`c'=F3.`c'
	gen F4_`c'=F4.`c'
	}
local cat "cdays30p_octfeb_cat cdays30p_juljan_cat cdays30p_maynov_cat cdays30p_sepmar_cat cdays30p_junfeb_cat cdays30p_maydec_cat cdays30p_sepdec_cat cdays30p_junoct_cat cdays30p_mayoct_cat" 
foreach c of local cat {
	gen L1_`c'=L.`c'
	gen L2_`c'=L2.`c'
	gen F1_`c'=F.`c'
	gen F2_`c'=F2.`c'
	gen F3_`c'=F3.`c'
	gen F4_`c'=F4.`c'
	}

local cat "weathercat_apraug weathercat_novfeb weathercat_rest weathercata_apraug weathercata_novfeb weathercata_rest weathercatb_apraug weathercatb_novfeb weathercatb_rest"
foreach c of local cat {
	gen L1_`c'=L.`c'
	gen L2_`c'=L2.`c'
	gen F1_`c'=F.`c'
	gen F2_`c'=F2.`c'
	gen F3_`c'=F3.`c'
	gen F4_`c'=F4.`c'
	}


	
* generate region-specific seasonal precipitation and temperature categories
* based on information from the University of Wisconsin crop calendar


local cat "precp days30p cdays30p"
foreach c of local cat {
	gen `c'_seasonm_cat     = `c'_maynov_cat
	replace `c'_seasonm_cat = `c'_juljan_cat if statenum==31  // yucatan
	replace `c'_seasonm_cat = `c'_octfeb_cat if statenum==2 | statenum==3 | statenum==8 | ///
	statenum==18 | statenum==25 | statenum==26 // baja california *, chihuahua, nayarit, sinaloa, sonora
	
	lab var `c'_seasonm_cat "region-specific corn season based on median plant-harvest"
	
	gen `c'_seasona_cat     = `c'_maydec_cat
	replace `c'_seasona_cat = `c'_junfeb_cat if statenum==31  // yucatan
	replace `c'_seasona_cat = `c'_sepmar_cat if statenum==2 | statenum==3 | statenum==8 | ///
	statenum==18 | statenum==25 | statenum==26 // baja california *, chihuahua, nayarit, sinaloa, sonora
	
	lab var `c'_seasona_cat "region-specific corn season based on all plant-harvest"
	
	gen `c'_seasonp_cat     = `c'_mayoct_cat
	replace `c'_seasonp_cat = `c'_junoct_cat if statenum==31  // yucatan
	replace `c'_seasonp_cat = `c'_sepdec_cat if statenum==2 | statenum==3 | statenum==8 | ///
	statenum==18 | statenum==25 | statenum==26 // baja california *, chihuahua, nayarit, sinaloa, sonora
	
	lab var `c'_seasonp_cat "region-specific corn season based on all planting (to beg of harvest)"

	}
	
local cat "precp days30p cdays30p"
local tim "L1 L2 F1 F2 F3 F4"
foreach t of local tim{
foreach c of local cat {
	gen `t'_`c'_seasonm_cat     = `t'_`c'_maynov_cat
	replace `t'_`c'_seasonm_cat = `t'_`c'_juljan_cat if statenum==31  // yucatan
	replace `t'_`c'_seasonm_cat = `t'_`c'_octfeb_cat if statenum==2 | statenum==3 | statenum==8 | ///
	statenum==18 | statenum==25 | statenum==26 // baja california *, chihuahua, nayarit, sinaloa, sonora
	
	gen `t'_`c'_seasona_cat     = `t'_`c'_maydec_cat
	replace `t'_`c'_seasona_cat = `t'_`c'_junfeb_cat if statenum==31  // yucatan
	replace `t'_`c'_seasona_cat = `t'_`c'_sepmar_cat if statenum==2 | statenum==3 | statenum==8 | ///
	statenum==18 | statenum==25 | statenum==26 // baja california *, chihuahua, nayarit, sinaloa, sonora
	
	gen `t'_`c'_seasonp_cat     = `t'_`c'_mayoct_cat
	replace `t'_`c'_seasonp_cat = `t'_`c'_junoct_cat if statenum==31  // yucatan
	replace `t'_`c'_seasonp_cat = `t'_`c'_sepdec_cat if statenum==2 | statenum==3 | statenum==8 | ///
	statenum==18 | statenum==25 | statenum==26 // baja california *, chihuahua, nayarit, sinaloa, sonora

	}
	}
* create indicator var for community migration prevalence above and below median
* use prevelence in year 1990 to categorize communities before the start of sample period

//sum prev if year==1990, de

gen highbaseprev=0
replace highbaseprev=1 if prev>13.38 & year==1991 
bys commun: egen highprev=max(highbaseprev)

* for migrants, drop obs after first US trip
sort persnum year
gen migyear=year if migf==1
by persnum: egen effectiveyear=max(migyear)
order persnum statenum commun hhnum persnum_or year migf fmxmig migyear effectiveyear

replace effectiveyear=9999 if effectiveyear==.
drop if year>effectiveyear 

* sample restrictions
drop if year<=1990

* generate indicators for undocumented and documented migrants
* only useful as an outcome when combined with migf
gen undoc = (usdoc1==8) & migf==1
gen doc = (usdoc1!=8 & usdoc1<8888) & migf==1

* variable descriptives - check for missing

codebook age sex edyr hh_usmig prev minx2 agrim primary secondary bank L2*
drop if edyr==. | minx2==. | agrim==. | L2_precp_year_cat==.
sum L1* L2*

* re-coding some variables
replace agrim = agrim/100
replace minx2 = minx2/100
replace prev = prev/100

gen school = (primary==1) | (secondary==1)

save $data/ind170_analysis_u.dta, replace

* F. Data on agricultural disposition in communities *
******************************************************

use "/Users/fgarip/Dropbox/Analysis/filiz_mig_env_analysis_jan2019/data/original/commun170.dta", clear

local var "CORN WHEAT BEAN SORG POTAT FORGE SUGAR OTHER"

foreach v of local var {
replace DIS`v' = . if DIS`v'==8888 | DIS`v'==9999
replace HCT`v' = . if HCT`v'==9999
*tab DIS`v'
}

local var2 "60 70 80 90 00 10"

foreach v of local var2 {
replace MANUF`v' = . if MANUF`v'==8888 | MANUF`v'==9999
rename MANUF`v' manuf`v'
}


rename COMMUN commun
rename DISCORN discorn

rename NAGRINST nagrinst
replace nagrinst = . if nagrinst==9999

rename HCTIRLND hctirlnd
rename HCTRNLND hctrnlnd
replace hctirlnd = . if hctirlnd==9999
replace hctrnlnd = . if hctrnlnd==9999

rename HCTCORN	hctcorn
rename HCTWHEAT hctwheat
rename HCTBEANS hctbeans
rename HCTSORG  hctsorg
rename HCTPOTAT hctpotat
rename HCTFORGE hctforge
rename HCTSUGAR hctsugar
rename HCTOTHER hctother

codebook hct*

gen hcttot = hctcorn + hctwheat + hctbeans + hctsorg + hctpotat + hctforge + hctsugar + hctother


gen sharecorn  = hctcorn/hcttot
gen sharebeans = hctbeans/hcttot
gen sharesorg  = hctsorg/hcttot
gen sharewheat = hctwheat/hcttot
gen sharepotat = hctpotat/hcttot
gen shareforge = hctforge/hcttot
gen sharesugar = hctsugar/hcttot
gen shareother = hctother/hcttot

gen logcorn  = 0
gen logbeans = 0
gen logsorg  = 0
gen logwheat = 0
gen logpotat = 0
gen logforge = 0
gen logsugar = 0
gen logother = 0

replace logcorn  = log(sharecorn) if sharecorn>0
replace logbeans = log(sharebeans) if sharebeans>0
replace logsorg  = log(sharesorg) if sharesorg>0
replace logwheat = log(sharewheat) if sharewheat>0
replace logpotat = log(sharepotat) if sharepotat>0
replace logforge = log(shareforge) if shareforge>0
replace logsugar = log(sharesugar) if sharesugar>0
replace logother = log(shareother) if shareother>0

gen nocrops = (hctcorn>0) + (hctbeans>0) + (hctsorg>0) + (hctwheat>0) + (hctpotat>0) + ///
              (hctforge>0) + (hctsugar>0) + (hctother>0)


gen cropdiv = -sharecorn*logcorn -sharebeans*logbeans -sharesorg*logsorg -sharewheat*logwheat ///
			  -sharepotat*logpotat -shareforge*logforge -sharesugar*logsugar -shareother*logother
			  
replace cropdiv = cropdiv/log(10)
lab var cropdiv "crop diversity (entropy wrt cultivated land)"

gen cropdiva = cropdiv*log(10)/log(nocrops)


gen shareirg = hctirlnd/(hctirlnd + hctrnlnd)

sort commun
keep commun discorn nagrinst share* hctcorn hctwheat hctbeans ///
			hctsorg hctpotat hctforge hctsugar hctother hcttot nocrops cropdiv* manuf*
save $data/commun-corn, replace


* Merge *
*********

use $data/ind170_analysis_u.dta, clear

	// merge with commun data with crop information
	
sort commun
merge m:1 commun using $data/commun-corn
keep if _merge==3
drop _merge

gen manuf = manuf90 if year>=1991 & year<=1999
replace manuf = manuf00 if year>=2000 & year<=2009
replace manuf = manuf10 if year>=2010
lab var manuf "share of female labor force in manufacturing"

save $data/ind170_analysis_u.dta, replace




