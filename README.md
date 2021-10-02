lab_zxr
===

retrieve_data
-------
* joinquant, ricequant, tushare(python)
* yahoo finance(R-quantmod)
* Wind(R-WindR)

strategies_and_report
-------
* Adaptive Asset Allocation(AAA)
* Active Combined Asset(ACA)
* Accelerating Dual Momentum(ADM)
* Generalized Protective Momentum(GPM)
* Global Tactical Asset Allocation(GTAA3/6)
* Hierarchical Risk Parity(HRP)
* Protective Asset Allocation(PAA)
* Quint Switching Filtered(QSF)
* Robust Asset Allocation(RAA Balanced/Aggressive)
* Tactical Bond Strategy(TBS)
* Traditional Dual Momentum(TDM)
* Vigilant Asset Allocation(VAA)


performance_report
-------
source
* model2.r: all strategies code(R)
* cal_ret.r: code for performance report generation
* hrp.py: all strategies code(python)

run
* 业绩报告.rmd: performance report for all strategies

performance_report
-------
Replication of the paper: Deep learning for finance: deep portfolio  
[Heaton J B, Polson N G, Witte J H. Deep learning for finance: deep portfolios[J]. Applied Stochastic Models in Business and Industry, 2017, 33(1): 3-12.]  
(https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2838013)  
* reproduce IBB.py: reproduce IBB index in the article (5.2 Smart Indexing the IBB Index)
* beat IBB.py: beat IBB index in the article(5.3 Outperforming the IBB Index)
* reproduce hs300.py: conduct empirical study in China A share market and reproduce hs300 index
* beat IBB.py: conduct empirical study in China A share market and beat hs300 index
* deep portfolio.pdf: reference paper written by J.Heaton, N.Polson and J.Witte
* data IBB: IBB index and component stock's data used to replicate the article
* data hs300: hs300 index and component stock's data gathered from Wind

Factor-based Asset Allocation
-------
BA thesis: The Empirical Study of Factor-based Asset Allocation on A-Share Market

