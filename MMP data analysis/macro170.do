********************************************************************************
************************ Culture & Mexico-US Migration ***********************
************************* Kunyuan Qiao & Filiz Garip ***************************
********************************************************************************


* This code compiles community and macro data using various sources.
* Last update: 26 May 2022 (11am)

*    I - Community variables
*   II - Macroeconomic variables

********************************************************************************

clear matrix
set matsize 800
set more off

cd "/Users/fgarip/Dropbox/Current_work/Projects by Filiz and Kunyuan/mmp/data" 


* Part I ***********************************************************************
* Community variables **********************************************************
********************************************************************************

* Number of inhabitants

use commun170, clear
sort commun
keep  commun COMPOP50-COMPOP10 POLCAT METROCAT AGRIM50-AGRIM10  ///
		MINX270-MINX210 YRPRIM YRSECON YRBANK1 MXWEIGHT USWEIGHT
save commun170_tem.dta, replace
	
* Prevalence ratio

use pratio170.dta, clear
sort commun year
keep commun year pratio
save pratio170.dta, replace


* Part II **********************************************************************
* Macroeconomic variables ******************************************************
********************************************************************************

use natlyear, clear
set obs 55

* Fill in the 2014-2016 missing values

	* Year
	replace year = 2014 in 50
	replace year = 2015 in 51
	replace year = 2016 in 52
	replace year = 2017 in 53
	replace year = 2018 in 54
	replace year = 2019 in 55
	
	
	* US average wage (source: Social Security Administration)
	* National average wage indexing (NAWI)
	replace usavwage = 22.33*46481.52/44888.16 in 50
	replace usavwage = 22.33*48098.63/44888.16 in 51
	replace usavwage = 22.33*48098.63/44888.16 in 52 // 2016 NAWI is not available yet, we use 2015 value
	replace usavwage = 22.33*50321.89/44888.16 in 53 
	replace usavwage = 22.33*50321.89/44888.16 in 54 // 2018 and 2019 NAWI is not available yet, we use 2017 value
	replace usavwage = 22.33*50321.89/44888.16 in 55 

	
	* Exchange rate (source: http://www.irs.gov/Individuals/International-Taxpayers/Yearly-Average-Currency-Exchange-Rates)
	replace exchrate = 13.840 if year==2014
	replace exchrate = 16.505 if year==2015
	replace exchrate = 19.435 if year==2016
	replace exchrate = 19.679 if year==2017
	replace exchrate = 19.227 if year==2018
	replace exchrate = 19.246 if year==2019

	* Peso devaluation
	replace deval = .0828 if year==2014
	replace deval = .1926 if year==2015
	replace deval = .1775 if year==2016
	replace deval = .1775 if year==2017 // assume constant after 2016
	replace deval = .1775 if year==2018 
	replace deval = .1775 if year==2019 

	* Lag-exchange rate
	replace lagexch = 12.762 if year==2014
	replace lagexch = 13.840 if year==2015
	replace lagexch = 16.505 if year==2016
	replace lagexch = 16.505 if year==2017
	replace lagexch = 16.505 if year==2018
	replace lagexch = 16.505 if year==2019

	
	* 2010 Constant USD (source: http://www.inflation.eu/inflation-rates/united-states/historic-inflation/cpi-inflation-united-states.aspx)
	replace const10 = 93.63/101.62 if year==2014
	replace const10 = 92.14/100.12 if year==2015
	replace const10 = 92.03/101.26 if year==2016
	replace const10 = 92.03/101.26 if year==2017 
	replace const10 = 92.03/101.26 if year==2018 
	replace const10 = 92.03/101.26 if year==2019

	* US unemployment (source: https://www.statista.com/statistics/193290/unemployment-rate-in-the-usa-since-1990/)
	replace usunemp = .062 if year==2014
	replace usunemp = .053 if year==2015
	replace usunemp = .049 if year==2016
	replace usunemp = .044 if year==2017
	replace usunemp = .039 if year==2018
	replace usunemp = .037 if year==2019

	* MX unemployment (source: ILO)
	replace mxunemp = .048 if year==2014
	replace mxunemp = .044 if year==2015
	replace mxunemp = .041 if year==2016
	replace mxunemp = .034 if year==2017 //assume constant after 2017
	replace mxunemp = .034 if year==2018 
	replace mxunemp = .034 if year==2019 

	
	* MX inflation rate (source: http://www.inflation.eu/inflation-rates/mexico/historic-inflation/cpi-inflation-mexico.aspx)
	replace infrate = .0402 if year==2014
	replace infrate = .0272 if year==2015
	replace infrate = .0282 if year==2016
	replace infrate = .0604 if year==2017
	replace infrate = .0490 if year==2018
	replace infrate = .0364 if year==2019

* Mexican minimum wage

	* Source: Servicio de Administración Tributaria (http://www.sat.gob.mx/informacion_fiscal/tablas_indicadores/Paginas/salarios_minimos.aspx)
	* Averaging the values for the two regions for 2014 and 2015 (from 2016 onwards, both regions are merged),
	* weighting for the period in force
	
	replace mxminwag=1/2*(67.29+63.77) if year==2014
	replace mxminwag=1/4*(1/2*(70.10+66.45))+1/2*(1/2*(70.10+68.28))+1/4*17.10 if year==2015
	replace mxminwag=73.04 if year==2016
	replace mxminwag=73.04 if year>=2017 //assume constant

	
	* The 2000 value seems off (all other years are consistent with official data), but 2000
	* min wage, which is around 35 pesos in the official data, is recorded as 25.12. Correct.
	
	replace mxminwag = (37.90+35.10+32.70)/3 if year==2000
	
	* In year 1993, three zeros were taken out of the Mexican peso. Correct for that, then convert
	* to 2000 constant pesos, and to US dollars (using the 2000 exchange rate)
	
	replace mxminwag = mxminwag/1000 if year<1993
	replace mxminwag = mxminwag*const10
	replace mxminwag = mxminwag/exchrate
	
	* Convert to hourly wages (currently daily)
	replace mxminwag = mxminwag/8
	lab var mxminwag "Mexican hourly wages in constant 2000 pesos converted to U.S.$"
	
* US/Mexico wage ratio

	replace usavwage = usavwage*const10
	gen wratio = usavwage/mxminwag
	lab var wratio "Hourly wage ratio (US/Mexico)"
	
* Visa accessibility

	* Apprehension probability
	* Owing to the very small number of illegal migrations after 2010, that is why we use the 2010 value for 2010 onwards
	
	replace probapp=.283 if year>2010
	replace visaaccs=143446/(143446+(489530*((1-probapp)/probapp))) if year==2011
	replace visaaccs=146406/(146406+(448697*((1-probapp)/probapp))) if year==2012
	replace visaaccs=135028/(135028+(424978*((1-probapp)/probapp))) if year==2013
	replace visaaccs=134052/(134052+(350177*((1-probapp)/probapp))) if year==2014
	replace visaaccs=158619/(158619+(267885*((1-probapp)/probapp))) if year==2015
	replace visaaccs=158619/(158619+(267885*((1-probapp)/probapp))) if year==2016 // 2016 visa accessibility is not available yet, we use 2015 value
	replace visaaccs=158619/(158619+(267885*((1-probapp)/probapp))) if year>=2017 // assume constant

keep year visaaccs mxminwag usavwage wratio exchrate lagexch deval infrate const10 mxunemp usunemp
sort year
save natlyear170, replace
	
* US-Mexican trade (source: US Census Bureau)

use natlhist2019.dta, clear
set obs 120

	replace year = 2018 in 119
	replace year = 2019 in 120

	rename tradeval tradebal
	replace tradebal = exports - imports if year >= 2018
	replace totrade = exports + imports if year >= 2018
	
keep year totrade
gen lntrade=ln(totrade)
sort year
save natlhist170, replace
