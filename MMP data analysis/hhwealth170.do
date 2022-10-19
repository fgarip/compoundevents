********************************************************************************
************************ Culture & Mexico-US Migration ***********************
************************* Kunyuan Qiao & Filiz Garip ***************************
********************************************************************************

* This code compiles household-level wealth data using the HOUSE file.
* Last update: 26 May 2022 (10am)

*    I - Exchange rate
*   II - Household domestic income and wages
*  III - Household amenities
*   IV - Municipality and state identifiers
*    V - Household land
*   VI - Missing observations

********************************************************************************

clear matrix
set matsize 800
set more off

cd "/Users/fgarip/Dropbox/Current_work/Projects by Filiz and Kunyuan/mmp/data" 


* Part I ***********************************************************************
* Exchange rate (Peso-Dollar, Dollar-Dollar2010) *******************************
********************************************************************************

* Sort data to be merged

use natlyear170.dta, clear
sort year
save natlyear170_temp.dta, replace

* Merge with HOUSE

use house170.dta, clear
	
	gen year = surveyyr
	sort year
	merge year using natlyear170.dta, keep(exchrate const10)
	drop if _merge==2
	drop _merge


* Part II **********************************************************************
* Household domestic income and wages ******************************************
********************************************************************************

	* In communities 1-52, hh income or wages is measured by 'hhhincom'. This variable
	* is replaced by 'hldowage' - last domestic wage of the hh head for communities 53-170.
	* I create a combined variable 'hhinc' for all communities that combines both variables.
	* Alternatively, I create 'hhdowage' for communities 53-170 which contains the more reliable
	* domestic wage information. Both variables will be considered in analyses.
	
	replace hhhincom = . if hhhincom==8888 | hhhincom==9999	
	replace hldowage = . if hldowage==8888 | hldowage==9999
	replace hby = . if hby==8888 | hby==9999


* Compute last domestic wage (in Mexico) recorded for communities 52-170 (year>=1997)

	replace hldowage = hldowage*12 if hby==5
	replace hldowage = hldowage*26 if hby==4
	replace hldowage = hldowage*52 if hby==3
	replace hldowage = hldowage*365 if hby==2
	replace hldowage = hldowage*40*52 if hby==1

	* Convert to constant dollars

	replace hldowage = hldowage/exchrate
	replace hldowage = hldowage*const10
	tabstat hldowage, by(year) stat(p25 p50 p75)

* Compute household income (from any source, wages or other, could include migrant remittances as well) using 'hhhincom'
* (collected in communities 1-52). This is not a very reliable indicator. When crossed with HBY (frequency indicator), 
* we see that some individuals make about $2500/hour! Also, we see that wages were recorded either hourly (HBY==1) 
* or monthly (HBY==5).	

	replace hhhincom = hhhincom*12 if hby==5
	replace hhhincom = hhhincom*40*52 if hby==1
	replace hhhincom = hhhincom/exchrate
	replace hhhincom = hhhincom*const10

	* VERY IMPORTANT NOTE - In 1993, three zeros were taken out of the Mexican peso.
	* HHINCOME was recorded in years 1982-1997. We need to correct for the values before 1993.

	replace hhhincom = hhhincom/1000 if year<1993
	tabstat hhhincom, by(year) stat(p25 p50 p75 max)

* Create the new variables

	gen hhdowage = hldowage
	lab var hhdowage "last domestic wage (annual) of hh head in 2010 constant US$ (commun 53-170)"
	gen hhinc = hldowage if commun>=53
	replace hhinc = hhhincom if commun<53
	lab var hhinc "hh income (combined wages and any other income) in survey year in 2010 constant US$"
	tabstat hhinc, by(year) stat(p25 p50 p75 max)

	* Important Note - hhinc and hhdowage are static variables (measuring hh income in the survey year or
	* during a migrant's last employment year in Mexico) - In order to use them in our regression analysis,
	* we need to assume that they remain static throughout the life history of hh head. To relax that assumption,
	* we can assume, instead, that the  income category of the household will remain the same and use dummies for 
	* income or wage quintiles in analysis. (Note - we first need to expand this data over time by merging to
	* life history data, then compute the categories. We do that in the code below.)

	keep 	commun hhnum year hhinc hhdowage cl* pl* prop* bus* ///
			water electric sewer stove refrig washing sewing radio tv stereo phone vh* ///
			hushealt hmxhealt famw jornw machine fertiliz farmdol insectic cows pigs ///
			horses burros oxen goats other
	sort commun hhnum year

save house_wealth_temp, replace


* Part III *********************************************************************
* Household amenities **********************************************************
********************************************************************************

use house_wealth_temp.dta, clear

* Recode the household amenity dummies, set the few missing observations (<0.1%) to zero

	replace water = 0 if water==2 | water==9999
	replace electric = 0 if electric==2 | electric==9999
	replace sewer = 0 if sewer==2 | sewer==9999
	replace stove = 0 if stove==2 | stove==9999
	replace refrig = 0 if refrig==2 | refrig==9999
	replace washing = 0 if washing==2 | washing==9999
	replace sewing = 0 if sewing==2 | sewing==9999
	replace tv = 0 if tv==2 | tv==9999
	replace stereo = 0 if stereo==2 | stereo==9999
	replace phone = 0 if phone==2 | phone==9999

	* Note - Amenities are measured on survey year only

	pca water electric sewer stove refrig washing sewing tv stereo phone, comp(5)
	predict p1, score
	egen amenities = std(p1)
	lab var amenities "pca score of 11 amenities (standardized) in survey yr"

	keep commun hhnum year hhinc hhdowage  cl* pl* prop* bus* amenities hushealt hmxhealt ///
			famw jornw machine fertiliz farmdol insectic cows pigs ///
			horses burros oxen goats other

save house_wealth_temp.dta, replace


* Part IV **********************************************************************
* Municipality and state identifiers *******************************************
********************************************************************************

* Set up the community 124 id data

use commun_distance.dta, clear
sort commun
save, replace

* Add 134-170 communities
		
use commun_statenum_placenum, clear
	
rename no commun
sort commun

	set obs 170
	replace commun=_n
	replace year=2012 if commun>134
	replace year=2013 if commun>139
	replace year=2014 if commun>143
	replace year=2015 if commun>150 // community 150 was surveyed in 2014 and 2015, but mostly in 2014
	replace year=2016 if commun>154
	
	replace name="San Joaquín" if commun==135
	replace municipio="San Joaquín" if commun==135
	replace state="Querétaro" if commun==135
	replace statenum=22 if commun==135
	replace placenum=15 if commun==135
	replace name="El Palmar" if commun==136
	replace municipio="Cadereyta de Montes" if commun==136
	replace state="Querétaro" if commun==136
	replace statenum=22 if commun==136
	replace placenum=4 if commun==136
	replace name="Santillán" if commun==137
	replace municipio="Tequisquiapan" if commun==137
	replace state="Querétaro" if commun==137
	replace statenum=22 if commun==137
	replace placenum=17 if commun==137
	replace name="Villa Progreso" if commun==138
	replace municipio="Ezequiel Montes" if commun==138
	replace state="Querétaro" if commun==138
	replace statenum=22 if commun==138
	replace placenum=7 if commun==138
	replace name="La Trinidad" if commun==139
	replace municipio="Tequisquiapan" if commun==139
	replace state="Querétaro" if commun==139
	replace statenum=22 if commun==139
	replace placenum=17 if commun==139
	replace name="Jalpa de Méndez" if commun==140
	replace municipio="Jalpa de Méndez" if commun==140
	replace state="Tabasco" if commun==140
	replace statenum=27 if commun==140
	replace placenum=10 if commun==140
	replace name="Huimanguillo" if commun==141
	replace municipio="Huimanguillo" if commun==141
	replace state="Tabasco" if commun==141
	replace statenum=27 if commun==141
	replace placenum=8 if commun==141
	replace name="Soyataco" if commun==142
	replace municipio="Jalpa de Méndez" if commun==142
	replace statenum=27 if commun==142
	replace placenum=10 if commun==142
	replace state="Tabasco" if commun==142
	replace name="Chiltepec" if commun==143
	replace municipio="Paraíso" if commun==143
	replace state="Tabasco" if commun==143
	replace statenum=27 if commun==143
	replace placenum=14 if commun==143
	replace name="Pegueros" if commun==144
	replace municipio="Tepatitlán de Morelos" if commun==144
	replace state="Jalisco" if commun==144
	replace statenum=14 if commun==144
	replace placenum=93 if commun==144
	replace name="Valle de Guadalupe" if commun==145
	replace municipio="Valle de Guadalupe" if commun==145
	replace state="Jalisco" if commun==145
	replace statenum=14 if commun==145
	replace placenum=14 if commun==145
	replace name="Cañadas de Obregón" if commun==146
	replace municipio="Cañadas de Obregón" if commun==146
	replace state="Jalisco" if commun==146
	replace statenum=14 if commun==146
	replace placenum=117 if commun==146
	replace name="Mexticacán" if commun==147
	replace municipio="Mexticacán" if commun==147
	replace state="Jalisco" if commun==147
	replace statenum=14 if commun==147
	replace placenum=60 if commun==147
	replace name="Tulcingo del Valle" if commun==148
	replace municipio="Tulcingo" if commun==148
	replace state="Puebla" if commun==148
	replace statenum=21 if commun==148
	replace placenum=191 if commun==148
	replace name="Chinantla" if commun==149
	replace municipio="Chinantla" if commun==149
	replace state="Puebla" if commun==149
	replace statenum=21 if commun==149
	replace placenum=59 if commun==149
	replace name="Piaxtla" if commun==150
	replace municipio="Piaxtla" if commun==150
	replace state="Puebla" if commun==150
	replace statenum=21 if commun==150
	replace placenum=113 if commun==150
	replace name="Acatic" if commun==151
	replace municipio="Acatic" if commun==151
	replace state="Jalisco" if commun==151
	replace statenum=14 if commun==151
	replace placenum=1 if commun==151
	replace name="Capilla de Milpillas" if commun==152
	replace municipio="Tepatitlán de Morelos" if commun==152
	replace state="Jalisco" if commun==152
	replace statenum=14 if commun==152
	replace placenum=93 if commun==152
	replace name="Nochistlán de Mejía" if commun==153
	replace municipio="Nochistlán de Mejía" if commun==153
	replace state="Zacatecas" if commun==153
	replace statenum=32 if commun==153
	replace placenum=34 if commun==153
	replace name="San Miguel el Alto" if commun==154
	replace municipio="San Miguel el Alto" if commun==154
	replace state="Jalisco" if commun==154
	replace statenum=14 if commun==154
	replace placenum=78 if commun==154
	replace name="Malinalco" if commun==155
	replace municipio="Malinalco" if commun==155
	replace state="Mexico" if commun==155
	replace statenum=15 if commun==155
	replace placenum=52 if commun==155
	replace name="Ocuilan" if commun==156
	replace municipio="Ocuilan" if commun==156
	replace state="Mexico" if commun==156
	replace statenum=15 if commun==156
	replace placenum=63 if commun==156
	replace name="Ixtapan de la Sal" if commun==157
	replace municipio="Ixtapan de la Sal" if commun==157
	replace state="Mexico" if commun==157
	replace statenum=15 if commun==157
	replace placenum=40 if commun==157
	replace name="Coatzingo" if commun==158
	replace municipio="Coatzingo" if commun==158
	replace state="Puebla" if commun==158
	replace statenum=21 if commun==158
	replace placenum=31 if commun==158
	replace name="Tepexi de Rodriguez" if commun==159
	replace municipio="Tepexi de Rodriguez" if commun==159
	replace state="Puebla" if commun==159
	replace statenum=21 if commun==159
	replace placenum=31 if commun==159
	replace name="San Pedro Escanela" if commun==160
	replace municipio="Pinal de Amoles" if commun==160
	replace state="Queretaro" if commun==160
	replace statenum=22 if commun==160
	replace placenum=2 if commun==160
	replace name="Tepeaca" if commun==170
	replace municipio="Tepeaca" if commun==170
	replace state="Puebla" if commun==170
	replace statenum=21 if commun==170
	replace placenum=164 if commun==170
	
	drop placenum
	egen placenum = group(municipio)
	egen munno = group(municipio)

drop municipio state name
sort commun
merge commun using commun_distance.dta
drop _merge

	replace distance=580180 if commun==135
	replace distance=616210 if commun==136
	replace distance=632430 if commun==137
	replace distance=628180	if commun==138
	replace distance=642760 if commun==139
	replace distance=1364330 if commun==140
	replace distance=1345310 if commun==141
	replace distance=1382750 if commun==142
	replace distance=1383910 if commun==143
	replace distance=710120	if commun==144
	replace distance=719570	if commun==145
	replace distance=690300 if commun==146
	replace distance=683440	if commun==147
	replace distance=869080 if commun==148
	replace distance=856900	if commun==149
	replace distance=857720	if commun==150
	replace distance=741410 if commun==151
	replace distance=748560 if commun==152
	replace distance=682430 if commun==153
	replace distance=690260 if commun==154
	replace distance=800650 if commun==155
	replace distance=790460 if commun==156
	replace distance=815610 if commun==157
	replace distance=808620 if commun==158
	replace distance=811710 if commun==159
	replace distance=565850 if commun==160
	replace distance=769090 if commun==170

* Fill in the distance values for communities 125-134

	for any 125 126 :  replace  distance=1246 in X  /*miles to Puente Internacional Reynosa-Pharr*/
	for any 127 :  replace  distance=1312 in X  
	for any 128 :  replace  distance=1241 in X  
	for any 129 :  replace  distance=342 in X  
	for any 130 :  replace  distance=325 in X  
	for any 131 :  replace  distance=512 in X  
	for any 132 :  replace  distance=622 in X  
	for any 133 :  replace  distance=438 in X  
	for any 134 :  replace  distance=534 in X
	for any 125 126 127 128 129 130 131 132 133 134:  replace distance=distance*1600 in X /*miles to m*/

sort commun
save commun_id.dta, replace


* Part V ***********************************************************************
* Household land ***************************************************************
********************************************************************************


* Price and land information from COMMUN
****************************************

use commun170.dta, clear

sort commun
keep commun PRC*
ren PRCIRLND  prcir
ren PRCRNLND prcrn
ren PRCPASTR prcpas
ren PRCORCH prcor

	* Set missing values

	replace prcir = . if prcir==8888 | prcir==9999
	replace prcrn = . if prcrn==8888 | prcrn==8999
	replace prcpas = . if prcpas==8888 | prcpas==9999
	replace prcor = . if prcor==8888 | prcor==9999
	codebook prc*

	* For mising values, first obtain the municipality and state id's. Then assume that the prices
	* are similar across municipality and states and use average values to fill-in missing community
	* price values.

	sort commun
	merge commun using commun_id.dta
	tab _merge
	drop _merge

	* First, compute the prices in constant 2010 U.S. dollars

	sort year
	merge year using natlyear170_temp.dta, keep(exchrate const10)
	tab _merge
	drop if _merge==2
	drop _merge

	replace prcir = prcir*const10/exchrate
	replace prcrn = prcrn*const10/exchrate
	replace prcpas = prcpas*const10/exchrate
	replace prcor = prcor*const10/exchrate

	* To fill-in missing values, first compute municipio averages

	bys munno: egen m_av_ir = mean(prcir)
	bys munno: egen m_av_rn = mean(prcrn)
	bys munno: egen m_av_pas = mean(prcpas)
	bys munno: egen m_av_or = mean(prcor)
	codebook m_av_ir m_av_rn m_av_pas m_av_or

	* Compute state-level averages

	bys statenum: egen s_av_ir = mean(prcir)
	bys statenum: egen s_av_rn = mean(prcrn)
	bys statenum: egen s_av_pas = mean(prcpas)
	bys statenum: egen s_av_or = mean(prcor)
	codebook s_av_ir s_av_rn s_av_pas s_av_or

	* Compute overall averages

	egen all_av_ir = mean(prcir)
	egen all_av_rn = mean(prcrn)
	egen all_av_pas = mean(prcpas)
	egen all_av_or = mean(prcor)
	codebook all_av_ir all_av_rn all_av_pas all_av_or
	
	* Fill-in the missing community prices by municipio groups
	* Changes made in 4 land price groups in 8, 4, 5, and 3 communities respectively.

	replace prcir = m_av_ir if prcir==. & m_av_ir~=.
	replace prcrn = m_av_rn if prcrn==. & m_av_rn~=.
	replace prcpas = m_av_pas if prcpas==. & m_av_pas~=.
	replace prcor = m_av_or if prcor==. & m_av_or~=.

	* Fill-in the remaining missing data using state-level information
	* Changes made in 4 land price groups in 38, 14, 40, and 53 communities respectively.

	replace prcir = s_av_ir if prcir==. & s_av_ir~=.
	replace prcrn = s_av_rn if prcrn==. & s_av_rn~=.
	replace prcpas = s_av_pas if prcpas==. & s_av_pas~=.
	replace prcor = s_av_or if prcor==. & s_av_or~=.

	* Fill-in the remaining missing data using overall average price information
	* Changes made in 4 land price groups in 15, 4, 32, and 52 communities respectively.

	replace prcir = all_av_ir if prcir==. & all_av_ir~=.
	replace prcrn = all_av_rn if prcrn==. & all_av_rn~=.
	replace prcpas = all_av_pas if prcpas==. & all_av_pas~=.
	replace prcor = all_av_or if prcor==. & all_av_or~=.

keep commun prc*
sort commun
save commun_price_temp.dta, replace


* Household land information from HOUSE
***************************************

	* Expand the data across years to complete the computations (year acquired
	* is different for each piece of land, if we are going to compute aggregates,
	* we need to work with annual data.)
	* Merge the data to life history data (with only hhnum, commun and year kept)

use life170.dta, clear
keep commun hhnum year 
sort commun hhnum year

	* Note - year in the household data is survey year - all other years should have missing land information.

merge commun hhnum year using house_wealth_temp.dta
tab _merge
drop if _merge==2
drop _merge
sort commun hhnum

	* Merge to community data and obtain land price information

merge commun using commun_price_temp.dta
tab _merge
drop _merge

	* Identify the missing land observations (8888 means no land)
	* Start with current land 

	replace clhect1 = 0 if clhect1==8888
	replace clhect1 = . if clhect1==9999
	replace clhect2 = 0 if clhect2==8888
	replace clhect2 = . if clhect2==9999
	replace clhect3 = 0 if clhect3==8888
	replace clhect3 = . if clhect3==9999
	replace clhect4 = 0 if clhect4==8888
	replace clhect4 = . if clhect4==9999

	replace cltype1 = . if cltype1==8888 | cltype1==9999
	replace cltype2 = . if cltype2==8888 | cltype2==9999
	replace cltype3 = . if cltype3==8888 | cltype3==9999
	replace cltype4 = . if cltype4==8888 | cltype4==9999

	* Set the year of acquisition to zero for mising years, pick a small number such that for 
	* households with land but missing year we assume they have owned it during the whole period.
	* This is the case for less than 1.8% of the observations (tab clhect1 if clacq1>=8888). 	

	replace clacq1 = 0 if clacq1==8888 | clacq1==9999
	replace clacq2 = 0 if clacq2==8888 | clacq2==9999
	replace clacq3 = 0 if clacq3==8888 | clacq3==9999
	replace clacq4 = 0 if clacq4==8888 | clacq4==9999

	* Identify the missing land observations for PAST land(8888 means no land)

	replace plhect1 = 0 if plhect1==8888
	replace plhect1 = . if plhect1==9999
	replace plhect2 = 0 if plhect2==8888
	replace plhect2 = . if plhect2==9999
	replace plhect3 = 0 if plhect3==8888
	replace plhect3 = . if plhect3==9999
	replace plhect4 = 0 if plhect4==8888
	replace plhect4 = . if plhect4==9999

	replace pltype1 = . if pltype1==8888 | pltype1==9999
	replace pltype2 = . if pltype2==8888 | pltype2==9999
	replace pltype3 = . if pltype3==8888 | pltype3==9999
	replace pltype4 = . if pltype4==8888 | pltype4==9999

	* Set the year of acquisition to zero for missing years, pick a small number such that for 
	* households with land but missing year we assume they have owned it during the whole period.
	* This is the case for less than 0.3% of the observations (tab plhect1 if placq1>=8888). 	

	replace placq1 = 0 if placq1==8888 | placq1==9999
	replace placq2 = 0 if placq2==8888 | placq2==9999
	replace placq3 = 0 if placq3==8888 | placq3==9999
	replace placq4 = 0 if placq4==8888 | placq4==9999
	replace plhect1 = . if plhect1==9999
	replace plhect2 = 0 if plhect2==8888
	replace plhect2 = . if plhect2==9999
	replace plhect3 = 0 if plhect3==8888
	replace plhect3 = . if plhect3==9999
	replace plhect4 = 0 if plhect4==8888
	replace plhect4 = . if plhect4==9999

	replace pltype1 = . if pltype1==8888 | pltype1==9999
	replace pltype2 = . if pltype2==8888 | pltype2==9999
	replace pltype3 = . if pltype3==8888 | pltype3==9999
	replace pltype4 = . if pltype4==8888 | pltype4==9999

	* Set the year of acquisition to zero for missing years, pick a small number such that for 
	* households with land but missing year we assume they have owned it during the whole period.
	* This is the case for less than 0.3% of the observations (tab plhect1 if placq1>=8888). 	

	replace placq1 = 0 if placq1==8888 | placq1==9999
	replace placq2 = 0 if placq2==8888 | placq2==9999
	replace placq3 = 0 if placq3==8888 | placq3==9999
	replace placq4 = 0 if placq4==8888 | placq4==9999

		* Note - Do not set plsold# to missing. If a person has acquired but not
		* sold the land, we should show him/her as owning the land. If we set plsold to
		* . the if conditions below will not hold. Leave missing plsold# at large values (8888 or 9999)
		* so that a person still in posession of land is recorded as such. (Note - this does not
		* make a difference here since placq# and plsold# have the same number of observations missing.
		* In the case of property, it could make a difference.
		
	* Identify the missing business observations (8888 means no business)

	replace bustype1 = 0 if bustype1==8888
	replace bustype1 = . if bustype1==9999
	replace bustype2 = 0 if bustype2==8888
	replace bustype2 = . if bustype2==9999
	replace bustype3 = 0 if bustype3==8888
	replace bustype3 = . if bustype3==9999
	replace bustype4 = 0 if bustype4==8888
	replace bustype4 = . if bustype4==9999

	* Set the year of acquisition to zero for missing years, pick a small number such that for 
	* households with business but missing year we assume they have owned it during the whole period.	

	replace busbeg1 = 0 if busbeg1==8888 | busbeg1==9999
	replace busbeg2 = 0 if busbeg2==8888 | busbeg2==9999
	replace busbeg3 = 0 if busbeg3==8888 | busbeg3==9999
	replace busbeg4 = 0 if busbeg4==8888 | busbeg4==9999
	
	* Identify the missing nb of rooms for the house of residence

	replace proprms1 = . if proprms1==9999
	replace proprms1 = 0 if (propdol1==2 | propctr1==204 | propten1!=3) & proprms1!=.

	* Generate a new variable that excludes land purchased with remittance money
	* This is to prevent endogeneity

	gen clhect1_nr = clhect1
	gen clhect2_nr = clhect2
	gen clhect3_nr = clhect3
	gen clhect4_nr = clhect4

	gen plhect1_nr = plhect1
	gen plhect2_nr = plhect2
	gen plhect3_nr = plhect3
	gen plhect4_nr = plhect4

	replace clhect1_nr = 0 if cldol1==1
	replace clhect2_nr = 0 if cldol2==1
	replace clhect3_nr = 0 if cldol3==1
	replace clhect4_nr = 0 if cldol4==1

	replace plhect1_nr = 0 if pldol1==1
	replace plhect2_nr = 0 if pldol2==1
	replace plhect3_nr = 0 if pldol3==1
	replace plhect4_nr = 0 if pldol4==1
	
	* Generate a new variable that excludes land purchased with remittance money
	* This is to prevent endogeneity

	gen bustype1_nr = bustype1
	gen bustype2_nr = bustype2
	gen bustype3_nr = bustype3
	gen bustype4_nr = bustype4

	replace bustype1_nr = 0 if busdol1==1
	replace bustype2_nr = 0 if busdol2==1
	replace bustype3_nr = 0 if busdol3==1
	replace bustype4_nr = 0 if busdol4==1

	* Ignore the land in the U.S. - they are likely purchased with remittances
	* This is measured starting with the 53rd community 

	replace clhect1 = 0 if clctr1==204
	replace clhect2 = 0 if clctr2==204
	replace clhect3 = 0 if clctr3==204
	replace clhect4 = 0 if clctr4==204

	replace clhect1_nr = 0 if clctr1==204
	replace clhect2_nr = 0 if clctr2==204
	replace clhect3_nr = 0 if clctr3==204
	replace clhect4_nr = 0 if clctr4==204

	replace plhect1 = 0 if plctr1==204
	replace plhect2 = 0 if plctr2==204
	replace plhect3 = 0 if plctr3==204
	replace plhect4 = 0 if plctr4==204

	replace plhect1_nr = 0 if plctr1==204
	replace plhect2_nr = 0 if plctr2==204
	replace plhect3_nr = 0 if plctr3==204
	replace plhect4_nr = 0 if plctr4==204
	
	* Ignore the business in the U.S. - they are likely purchased with remittances
	* This is measured starting with the 53rd community 

	replace bustype1 = 0 if busctr1==204
	replace bustype2 = 0 if busctr2==204
	replace bustype3 = 0 if busctr3==204
	replace bustype4 = 0 if busctr4==204

	replace bustype1_nr = 0 if busctr1==204
	replace bustype2_nr = 0 if busctr2==204
	replace bustype3_nr = 0 if busctr3==204
	replace bustype4_nr = 0 if busctr4==204

	* Now, fill-in the years according to the following rules. (1) For cltype#, all the years after clacq#
	* equal to the current land amount. (2) For pltype#, all the years after placq# and before plsol#, equal
	* the same amount.

		* Each hhnum has one observation, first compute the mean (which should equal the one observation), then equate
		* the years after the year of acquisition should obtain this value. (Same steps are repeated for land type as well.)

	bys commun hhnum: egen mncl1 = mean(clhect1)
	bys commun hhnum: egen mncl2 = mean(clhect2)
	bys commun hhnum: egen mncl3 = mean(clhect3)
	bys commun hhnum: egen mncl4 = mean(clhect4)

	bys commun hhnum: egen mncl1_nr = mean(clhect1_nr)
	bys commun hhnum: egen mncl2_nr = mean(clhect2_nr)
	bys commun hhnum: egen mncl3_nr = mean(clhect3_nr)
	bys commun hhnum: egen mncl4_nr = mean(clhect4_nr)

	bys commun hhnum: egen mnclt1 = mean(cltype1)
	bys commun hhnum: egen mnclt2 = mean(cltype2)
	bys commun hhnum: egen mnclt3 = mean(cltype3)
	bys commun hhnum: egen mnclt4 = mean(cltype4)

	bys commun hhnum: egen mnclacq1 = mean(clacq1)
	bys commun hhnum: egen mnclacq2 = mean(clacq2)
	bys commun hhnum: egen mnclacq3 = mean(clacq3)
	bys commun hhnum: egen mnclacq4 = mean(clacq4)

	replace clhect1 = mncl1 if year>=mnclacq1
	replace clhect2 = mncl2 if year>=mnclacq2
	replace clhect3 = mncl3 if year>=mnclacq3
	replace clhect4 = mncl4 if year>=mnclacq4

	replace clhect1_nr = mncl1_nr if year>=mnclacq1
	replace clhect2_nr = mncl2_nr if year>=mnclacq2
	replace clhect3_nr = mncl3_nr if year>=mnclacq3
	replace clhect4_nr = mncl4_nr if year>=mnclacq4

	replace cltype1 = mnclt1 if year>=mnclacq1
	replace cltype2 = mnclt2 if year>=mnclacq2
	replace cltype3 = mnclt3 if year>=mnclacq3
	replace cltype4 = mnclt4 if year>=mnclacq4

	drop mncl* clacq* 

	* Repeat the same steps for past land holdings (plhect#)

	bys commun hhnum: egen mnpl1 = mean(plhect1)
	bys commun hhnum: egen mnpl2 = mean(plhect2)
	bys commun hhnum: egen mnpl3 = mean(plhect3)
	bys commun hhnum: egen mnpl4 = mean(plhect4)

	bys commun hhnum: egen mnpl1_nr = mean(plhect1_nr)
	bys commun hhnum: egen mnpl2_nr = mean(plhect2_nr)
	bys commun hhnum: egen mnpl3_nr = mean(plhect3_nr)
	bys commun hhnum: egen mnpl4_nr = mean(plhect4_nr)

	bys commun hhnum: egen mnplt1 = mean(pltype1)
	bys commun hhnum: egen mnplt2 = mean(pltype2)
	bys commun hhnum: egen mnplt3 = mean(pltype3)
	bys commun hhnum: egen mnplt4 = mean(pltype4)

	bys commun hhnum: egen mnplacq1 = mean(placq1)
	bys commun hhnum: egen mnplacq2 = mean(placq2)
	bys commun hhnum: egen mnplacq3 = mean(placq3)
	bys commun hhnum: egen mnplacq4 = mean(placq4)

	bys commun hhnum: egen mnplsold1 = mean(plsold1)
	bys commun hhnum: egen mnplsold2 = mean(plsold2)
	bys commun hhnum: egen mnplsold3 = mean(plsold3)
	bys commun hhnum: egen mnplsold4 = mean(plsold4)

	replace plhect1 = mnpl1 if year>=mnplacq1 & year<mnplsold1
	replace plhect2 = mnpl2 if year>=mnplacq2 & year<mnplsold2
	replace plhect3 = mnpl3 if year>=mnplacq3 & year<mnplsold3
	replace plhect4 = mnpl4 if year>=mnplacq4 & year<mnplsold4

	replace plhect1_nr = mnpl1_nr if year>=mnplacq1 & year<mnplsold1
	replace plhect2_nr = mnpl2_nr if year>=mnplacq2 & year<mnplsold2
	replace plhect3_nr = mnpl3_nr if year>=mnplacq3 & year<mnplsold3
	replace plhect4_nr = mnpl4_nr if year>=mnplacq4 & year<mnplsold4

	replace pltype1 = mnplt1 if year>=mnplacq1 & year<mnplsold1
	replace pltype2 = mnplt2 if year>=mnplacq2 & year<mnplsold2
	replace pltype3 = mnplt3 if year>=mnplacq3 & year<mnplsold3
	replace pltype4 = mnplt4 if year>=mnplacq4 & year<mnplsold4

	drop mnpl* placq* plsold*
	
	* Repeat the same for business (bustype#)
	
	bys commun hhnum: egen mnbt1 = mean(bustype1)
	bys commun hhnum: egen mnbt2 = mean(bustype2)
	bys commun hhnum: egen mnbt3 = mean(bustype3)
	bys commun hhnum: egen mnbt4 = mean(bustype4)

	bys commun hhnum: egen mnbt1_nr = mean(bustype1_nr)
	bys commun hhnum: egen mnbt2_nr = mean(bustype2_nr)
	bys commun hhnum: egen mnbt3_nr = mean(bustype3_nr)
	bys commun hhnum: egen mnbt4_nr = mean(bustype4_nr)

	bys commun hhnum: egen mnbbeg1 = mean(busbeg1)
	bys commun hhnum: egen mnbbeg2 = mean(busbeg2)
	bys commun hhnum: egen mnbbeg3 = mean(busbeg3)
	bys commun hhnum: egen mnbbeg4 = mean(busbeg4)

	bys commun hhnum: egen mnbend1 = mean(busend1)
	bys commun hhnum: egen mnbend2 = mean(busend2)
	bys commun hhnum: egen mnbend3 = mean(busend3)
	bys commun hhnum: egen mnbend4 = mean(busend4)

	replace bustype1 = mnbt1 if year>=mnbbeg1 & year<mnbend1
	replace bustype2 = mnbt2 if year>=mnbbeg2 & year<mnbend2
	replace bustype3 = mnbt3 if year>=mnbbeg3 & year<mnbend3
	replace bustype4 = mnbt4 if year>=mnbbeg4 & year<mnbend4

	replace bustype1_nr = mnbt1_nr if year>=mnbbeg1 & year<mnbend1
	replace bustype2_nr = mnbt2_nr if year>=mnbbeg2 & year<mnbend2
	replace bustype3_nr = mnbt3_nr if year>=mnbbeg3 & year<mnbend3
	replace bustype4_nr = mnbt4_nr if year>=mnbbeg4 & year<mnbend4

	drop mnbt* mnbbeg* mnbend*
	
	* Repeat the same for the number of rooms of the house of residence (proprms1)
	
	bys commun hhnum: egen troom = mean(proprms1)
	
		* If the variable is missing (6082 household-years), use the median with the community
		
		bys commun: egen m_troom = median(troom)
		replace troom = m_troom if troom==.

	* Here set the missing land values to zero (this is necessary to add them - 
	* sum operator does not treat missing as zero). Set the missing land types to 7
	* (so that it appears last in the tabulations - and we do not create dummies for that category)

	replace clhect1 = 0 if clhect1==.
	replace clhect2 = 0 if clhect2==.
	replace clhect3 = 0 if clhect3==.
	replace clhect4 = 0 if clhect4==.

	replace clhect1_nr = 0 if clhect1_nr==.
	replace clhect2_nr = 0 if clhect2_nr==.
	replace clhect3_nr = 0 if clhect3_nr==.
	replace clhect4_nr = 0 if clhect4_nr==.

	replace cltype1 = 7 if cltype1==.
	replace cltype2 = 7 if cltype2==.
	replace cltype3 = 7 if cltype3==.
	replace cltype4 = 7 if cltype4==.

	replace plhect1 = 0 if plhect1==.
	replace plhect2 = 0 if plhect2==.
	replace plhect3 = 0 if plhect3==.
	replace plhect4 = 0 if plhect4==.

	replace plhect1_nr = 0 if plhect1_nr ==.
	replace plhect2_nr = 0 if plhect2_nr ==.
	replace plhect3_nr = 0 if plhect3_nr ==.
	replace plhect4_nr = 0 if plhect4_nr ==.

	replace pltype1 = 7 if pltype1==.
	replace pltype2 = 7 if pltype2==.
	replace pltype3 = 7 if pltype3==.
	replace pltype4 = 7 if pltype4==.


	* Create dummies for land type

	tab cltype1, gen(c1_)
	tab cltype2, gen(c2_)

		* For cltype3 and 4, not all the land categories are represented. Create dummies manually.

		gen c3_1 = cltype3==1
		gen c3_2 = cltype3==2
		gen c3_3 = cltype3==3
		gen c3_4 = cltype3==4
		gen c3_5 = cltype3==5
		gen c3_6 = cltype3==6

		gen c4_1 = cltype4==1
		gen c4_2 = cltype4==2
		gen c4_3 = cltype4==3
		gen c4_4 = cltype4==4
		gen c4_5 = cltype4==5
		gen c4_6 = cltype4==6

	* Create dummies for land type

	tab pltype1, gen(p1_)

		* For pltype2, 3 and 4, not all the land categories are represented. Create dummies manually.

		gen p2_1 = pltype2==1
		gen p2_2 = pltype2==2
		gen p2_3 = pltype2==3
		gen p2_4 = pltype2==4
		gen p2_5 = pltype2==5
		gen p2_6 = pltype2==6

		gen p3_1 = pltype3==1
		gen p3_2 = pltype3==2
		gen p3_3 = pltype3==3
		gen p3_4 = pltype3==4
		gen p3_5 = pltype3==5
		gen p3_6 = pltype3==6

		gen p4_1 = pltype4==1
		gen p4_2 = pltype4==2
		gen p4_3 = pltype4==3
		gen p4_4 = pltype4==4
		gen p4_5 = pltype4==5
		gen p4_6 = pltype4==6

	* Categorize land holding by type using cltype# variable (1: Irrigated, 2: Wetland, 3: Dryland,
	* 4: Pasture, 5: Orchard, 6: other)

	gen clandir = clhect1*c1_1 + clhect2*c2_1 + clhect3*c3_1 + clhect4*c4_1
	gen clandwl = clhect1*c1_2 + clhect2*c2_2 + clhect3*c3_2 + clhect4*c4_2
	gen clanddl = clhect1*c1_3 + clhect2*c2_3 + clhect3*c3_3 + clhect4*c4_3
	gen clandpas = clhect1*c1_4 + clhect2*c2_4 + clhect3*c3_4 + clhect4*c4_4
	gen clandor = clhect1*c1_5 + clhect2*c2_5 + clhect3*c3_5 + clhect4*c4_5
	gen clandoth = clhect1*c1_6 + clhect2*c2_6 + clhect3*c3_6 + clhect4*c4_6

	gen clandir_nr  = clhect1_nr*c1_1 + clhect2_nr*c2_1 + clhect3_nr*c3_1 + clhect4_nr*c4_1
	gen clandwl_nr  = clhect1_nr*c1_2 + clhect2_nr*c2_2 + clhect3_nr*c3_2 + clhect4_nr*c4_2
	gen clanddl_nr  = clhect1_nr*c1_3 + clhect2_nr*c2_3 + clhect3_nr*c3_3 + clhect4_nr*c4_3
	gen clandpas_nr = clhect1_nr*c1_4 + clhect2_nr*c2_4 + clhect3_nr*c3_4 + clhect4_nr*c4_4
	gen clandor_nr  = clhect1_nr*c1_5 + clhect2_nr*c2_5 + clhect3_nr*c3_5 + clhect4_nr*c4_5
	gen clandoth_nr = clhect1_nr*c1_6 + clhect2_nr*c2_6 + clhect3_nr*c3_6 + clhect4_nr*c4_6

	* Repeat the same step for past land holdings

	gen plandir  = plhect1*p1_1 + plhect2*p2_1 + plhect3*p3_1 + plhect4*p4_1
	gen plandwl  = plhect1*p1_2 + plhect2*p2_2 + plhect3*p3_2 + plhect4*p4_2
	gen planddl  = plhect1*p1_3 + plhect2*p2_3 + plhect3*p3_3 + plhect4*p4_3
	gen plandpas = plhect1*p1_4 + plhect2*p2_4 + plhect3*p3_4 + plhect4*p4_4
	gen plandor  = plhect1*p1_5 + plhect2*p2_5 + plhect3*p3_5 + plhect4*p4_5
	gen plandoth = plhect1*p1_6 + plhect2*p2_6 + plhect3*p3_6 + plhect4*p4_6

	gen plandir_nr  = plhect1_nr*p1_1 + plhect2_nr*p2_1 + plhect3_nr*p3_1 + plhect4_nr*p4_1
	gen plandwl_nr  = plhect1_nr*p1_2 + plhect2_nr*p2_2 + plhect3_nr*p3_2 + plhect4_nr*p4_2
	gen planddl_nr  = plhect1_nr*p1_3 + plhect2_nr*p2_3 + plhect3_nr*p3_3 + plhect4_nr*p4_3
	gen plandpas_nr = plhect1_nr*p1_4 + plhect2_nr*p2_4 + plhect3_nr*p3_4 + plhect4_nr*p4_4
	gen plandor_nr  = plhect1_nr*p1_5 + plhect2_nr*p2_5 + plhect3_nr*p3_5 + plhect4_nr*p4_5
	gen plandoth_nr = plhect1_nr*p1_6 + plhect2_nr*p2_6 + plhect3_nr*p3_6 + plhect4_nr*p4_6

	* Add all land holdings by type

	gen landir = clandir + plandir
	gen landwl = clandwl + plandwl
	gen landdl = clanddl + planddl
	gen landpas = clandpas + plandpas
	gen landor = clandor + plandor
	gen landoth = clandoth + plandoth

	gen landir_nr = clandir_nr + plandir_nr
	gen landwl_nr = clandwl_nr + plandwl_nr
	gen landdl_nr = clanddl_nr + planddl_nr
	gen landpas_nr = clandpas_nr + plandpas_nr
	gen landor_nr = clandor_nr + plandor_nr
	gen landoth_nr = clandoth_nr + plandoth_nr

	gen tland = landir + landwl + landdl + landpas + landor + landoth
	lab var tland "total hectars of land owned"

	gen tland_nr = landir_nr + landwl_nr + landdl_nr + landpas_nr + landor_nr + landoth_nr
	lab var tland_nr "total hectars of land owned (excl. land financed with remit.)"

		* NOTE - About 9.7 percent of observations have non-zero land. This number is very close to the 
		* 10% observed-rate in the LIFE124.dta. It is slightly smaller since we are excluding land
		* purchased using U.S. dollars (remittances).


	* Value of land by type

	gen vlandir = prcir*landir
	gen vlandwl = prcrn*landwl
	gen vlanddl = prcrn*landdl
	gen vlandpas = prcpas*landpas
	gen vlandor = prcor*landor

	gen vlandir_nr = prcir*landir_nr
	gen vlandwl_nr = prcrn*landwl_nr
	gen vlanddl_nr = prcrn*landdl_nr
	gen vlandpas_nr = prcpas*landpas_nr
	gen vlandor_nr = prcor*landor_nr

		* Assume other land is similarly priced to rainfed land - most common kind in the data - 
		* Note that only 0.04% of the observations have other land listed.

	gen vlandoth = prcrn*landoth
	gen vlandoth_nr = prcrn*landoth_nr

	gen vland = vlandir + vlandwl + vlanddl + vlandpas + vlandor + vlandoth
	lab var vland "value (in 2010 U.S.$) of total land in year"

	gen vland_nr = vlandir_nr + vlandwl_nr + vlanddl_nr + vlandpas_nr + vlandor_nr + vlandoth_nr
	lab var vland_nr "value (in 2010 U.S.$) of total land in year (excl. land financed with remit.)"

	gen lnvland = ln(vland+1)
	lab var lnvland "log of value (in 2010 U.S.$) of total land in year"

	gen lnvland_nr = ln(vland_nr+1)
	lab var lnvland_nr "log of value (in 2010 U.S.$) of total land in year (excl. land financed with remit.)"

	* hist lnvland if lnvland>0
	* hist lnvland_nr if lnvland_nr>0
	* kdensity lnvland if lnvland>0, gen(land_x land_d)
	* kdensity lnvland_nr if lnvland_nr>0, gen(landnr_x landnr_d)
	* lab var land_d "all land"
	* lab var landnr_d "land - not funded by remit"

	* sc land_d land_x, c(l) || sc landnr_d landnr_x, c(l)
	
	* Dummy: business
	
	gen tbus_nr = (bustype1_nr>0 & bustype1_nr<13) | (bustype2_nr>0 & bustype2_nr<13) ///
		| (bustype3_nr>0 & bustype3_nr<13) | (bustype4_nr>0 & bustype4_nr<13)

	gen tbus = (bustype1>0 & bustype1<13) | (bustype2>0 & bustype2<13) ///
		| (bustype3>0 & bustype3<13) | (bustype4>0 & bustype4<13)

		* NOTE - The distribution of positive values of lnvland looks gaussian.


* Part VI **********************************************************************
* Missing observations *********************************************************
********************************************************************************

* Household Amenities

	* NOTE - We are assuming that the amenities index remains constant over time. We also
	* do not know what percentage of the amenities were purchased using remittances.

	gsort commun hhnum -amenities
	by commun hhnum: replace amenities = amenities[_n-1] if _n>1

* Household Income - assuming the survey year values are static over time

	gsort commun hhnum -hhdowage
	by commun hhnum: replace hhdowage = hhdowage[_n-1] if _n>1

	gsort commun hhnum -hhinc
	by commun hhnum: replace hhinc = hhinc[_n-1] if _n>1

	* Compute categorical and logged versions

	gen lnhhdowage = ln(hhdowage+1)
	lab var lnhhdowage "log of last domestic wage (annual) of hh head in 2010 constant US$ (commun 53-124)"
	
	gen lnhhinc = ln(hhinc+1)
	lab var lnhhinc "log of hh income (combined wages and any other income) in 2010 constant US$"

keep commun hhnum year hhinc hhdowage lnhhinc lnhhdowage  tland* vland* lnvland* tbus troom ///
		amenities hushealt hmxhealt famw jornw machine fertiliz farmdol insectic cows pigs ///
			horses burros oxen goats other
			
			
* Household farming and livestock

replace famw = 0 if famw==8888
replace famw = . if famw==9999

replace jornw = 0 if jornw==8888
replace jornw = . if jornw==9999

foreach v of varlist machine fertiliz insectic farmdol {
recode `v' (2=0) (8888=0) (9999=.)
}

foreach v of varlist cows pigs horses burros oxen goats other {
recode `v' (9999=.)
}

foreach v of varlist famw jornw machine fertiliz insectic farmdol cows pigs horses burros oxen goats other {
gsort commun hhnum -`v'
by commun hhnum: replace `v' = `v'[_n-1] if _n>1
}


sort commun hhnum year
save house_wealth, replace
