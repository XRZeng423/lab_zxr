ssc install reghdfe, replace

insheet using E:\data\SIEPR\panel.csv
gen log_price=log(p_last)

reghdfe log_price defaulter#post, absorb(country) vce(cluster country)
reghdfe log_price defaulter#post, absorb(country year) vce(cluster country)
reghdfe log_price defaulter#post, absorb(country year id) vce(cluster country)

encode defclass, generate(defclass2)
reghdfe log_price defaulter##post##defclass2, absorb(country) vce(cluster country)
reghdfe log_price defaulter##post##defclass2, absorb(country year) vce(cluster country)
reghdfe log_price defaulter##post##defclass2, absorb(country year id) vce(cluster country)


