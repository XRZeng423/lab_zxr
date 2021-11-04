
* Wage gap series
use clghsgwg-march-regseries-exp.dta,clear
desc
keep year clghsg_all* clphsg_all*

* Adding supplies series
sort year
merge year using effunits-exp-byexp-6305
assert _merge==3
drop _merge
desc eu_* euexp*
drop euexp*
tab expcat
keep if expcat==1
drop expcat
desc

* Save working series
label data "March 1963-2005 wage gaps and efficiency units: Experienced-based measures"
save march-price-quantity-exp-all,replace
use march-price-quantity-exp-all,clear

desc

*Different College/HS Wage Diff Series, M&F Combined and Rel Supply
list year clphsg_all eu_lnclg
*Trend Terms
gen t = year-1962
gen t2 = t*t
*Post-1992 Trend Shift -- Change in CPS Education Coding
gen t92 = max(year-1992, 0)

*Trend Regressions for Wage Diff and Relative Supply Terms
reg clphsg_all t
predict gapdt,resid
reg eu_lnclg t
predict supdt,resid
label variable gapdt "Detrended Wage Differential"
label variable supdt "Detrended Relative Supply"
label variable year " "
label variable clphsg_all "Observed CLG/HS Gap"

#delimit ;
scatter gapdt supdt year, c(l l) msymbol (oh dh) xlabel(1963(6)2005) yline(0) 
legend(region(lstyle(none))) 
ti("A. Detrended College-High School Wage Differential and Relative Supply, 1963-2005", size(medium))  ytitle("Log Points") ylabel(-.15(.05).15)  legend(size(small))
saving("km-detrended-wagegap.gph", replace);
pause;

* College/high-school relative supply series;
scatter eu_lnclg year, c(l) msymbol (O) xlabel(1963(6)2005) xline(1982) 
legend(region(lstyle(none))) ylab(-.9(.3).3) ytitle("Log Relative Supply Index") ti(" ")
saving("relative-supply-index.gph",replace);


*1963-87 Regressions and Predictions ;
reg clphsg_all t eu_lnclg if year<1988;
predict gap6387;
label variable gap6387 "Katz-Murphy Predicted Wage Gap: 1963-1987 Trend" ;

* graph clphsg_all gap6387 year, c(ll) xlabel(1963(6)2005) xline(1987, 1992)
ti("KM 1963-87 Prediction Model, Gap") saving("km6387", replace);

scatter clphsg_all gap6387 year, c(l l) msymbol (oh dh) xlabel(1963(6)2005) xline(1987 1992) 
legend(region(lstyle(none))) ylab(.35(.1).75) ytitle("Log Wage Gap") ti("B. Katz-Murphy Prediction Model for the College-High School Wage Gap", size(medium)) legend(size(small))
saving("km-predicted-series-6387.gph", replace);
pause

*Full Sample Regressions with Different Trend Models, 1963-2005:
*Wage Gap Measure;
reg clphsg_all t eu_lnclg;
predict gapt;
reg clphsg_all t t2 eu_lnclg;
label variable gapt "Predicted Gap, Linear Trend";
reg clphsg_all t t92 eu_lnclg;
predict gapt92;
label variable gapt92 "Predicted Gap, Trend + Break in 1992";
label variable clphsg_all "Observed CLG/HS Gap";
list year clphsg_all gap6387 gapt gapt92;

* graph clphsg_all gapt92 year, c(ll) xlabel(1963(6)2005) xline(1987,1992)
ti("KM 1963-2005 Prediction, Gap Trend92")
saving("km6303_t92.gph", replace);

scatter clphsg_all gapt92 year, c(l l ) msymbol (oh dh) xlabel(1963(6)2005) xline(1987 1992) 
legend(region(lstyle(none))) ylab(.35(.1).65) ti("Katz Murphy 1963-2005 Predicted Gap: Trend and Break 1992+") ytitle("Log Wage Gap")
saving("km-6305-gap92.gph", replace);
pause;

* graph clphsg_all gap6387 gapt gapt92 year, c(llll) xlabel(1963(6)2005)
xline(1987,1992)  ti("KM 1963-2005 Prediction, Gap All")
saving("km6305_all_g.gph", replace);

scatter clphsg_all gap6387 gapt gapt92 year, c(l l l l) msymbol (oh dh t x) xlabel(1963(6)2005) xline(1987 1992) 
legend(region(lstyle(none))) ylab(.35(.1).75) ti("Wage Gap 1963-2003: Actual and Predictions") ytitle("Log Wage Gap")
saving("km-6305-gap-all.gph", replace);


*Predicted Demand Growth with Different Sigmas;
*Gap Measure;
*1963-87 Sigma, Trend Model;
gen sig6387 = 1/.731 ;
gen sig92t = 1/.6308806;
sum sig6387 sig92t;
gen dem6387 = sig6387*clphsg_all + eu_lnclg;
gen dem92t = sig92t*clphsg_all + eu_lnclg;
gen dem2 = 2*clphsg_all + eu_lnclg;
gen dem1 = clphsg_all + eu_lnclg;
gen dem3 = 3*clphsg_all + eu_lnclg;

sort year;
*Normalize Demand to 0 in 1963;
gen ndem92t = dem92t - dem92t[1];
gen ndem1 = dem1 - dem1[1];
gen ndem2 = dem2 - dem2[1];
gen ndem3 = dem3 - dem3[1];
gen ndem6387 = dem6387 - dem6387[1];
label variable ndem1 "sigma = 1";
label variable ndem92t "sigma = 1.58";
label variable ndem2 "sigma = 2";
label variable ndem3 "sigma = 3";
list ndem92t ndem1 ndem2 ndem3 ndem6387;

* graph ndem92t ndem1 ndem2 ndem3 year, c(llll) ti("Implied Demand Growth, 1963-2003")
xlabel(1963(6)2005)
saving("dem6305.gph", replace);

scatter ndem92t ndem1 ndem2 ndem3 year, c(l l l l) msymbol (oh dh t x) xlabel(1963(6)2005) xline(1987 1992) 
legend(region(lstyle(none))) ti("Implied Log Demand Growth, 1963-2003") ytitle("Change in Demand Index Since 1963")
saving("km-implieddemand-6305.gph", replace);

*Relative Demand and Relative Supply Growth Rates for Different Periods;
*1963-71;
*8 Year Growth Rates;
gen dem8 = ndem92t - ndem92t[_n-8];
gen ndem8 = dem8/sig92t;
gen sup8 = eu_lnclg - eu_lnclg[_n-8];
gen nsup8 = sup8/sig92t;
gen dgapt92 = gapt92 - gapt92[_n-8];
gen dgap6387 = gap6387 - gap6387[_n-8];
gen dgapt = gapt - gapt[_n-8];
gen dgap = clphsg_all - clphsg_all[_n-8];
list year dgap ndem8 nsup8 dem8 sup8;
list year dgap dgapt92 dgap6387 dgapt;

*Decadal (10 Year Growth Rates);
gen dem10 = ndem92t - ndem92t[_n-10];
gen ndem10 = dem10/sig92t;
gen sup10 = eu_lnclg - eu_lnclg[_n-10];
gen nsup10 = sup10/sig92t;
gen dgapt92_10 = gapt92 - gapt92[_n-10];
gen dgap6387_10 = gap6387 - gap6387[_n-10];
gen dgapt_10 = gapt - gapt[_n-10];
gen dgap10 = clphsg_all - clphsg_all[_n-10];
list year dgap10 ndem10 nsup10 dem10 sup10;
list year dgap10 dgapt92_10 dgap6387_10 dgapt_10;


*Basic Katz-Murphy Regressions Other Sample Periods:
*1967-2005 -- Drop Supsect 1963-66 Observations;
reg clphsg_all t eu_lnclg if year>1966;
reg clphsg_all t t92 eu_lnclg if year>1966;
*1973-2005 MORG Sample Period;
reg clphsg_all t eu_lnclg if year>1972;
reg clphsg_all t t92 eu_lnclg if year>1972;
*1963-1991  Drop Period After Education Data Changes;
reg clphsg_all t eu_lnclg if year<1992  ;
*1967-1991;
reg clphsg_all t eu_lnclg if year>1966 & year<1992;


log close ;
