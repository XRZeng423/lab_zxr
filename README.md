lab_zxr
===

retrieve_data
-------
* 从聚宽、米筐、tushare提取数据的python代码
* 用quantmod包提取yahoo finance数据的r代码
* 用WindR接口提取数据的r代码

strategies_and_report
-------
每个策略单独的代码与业绩报告
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
调用
* model2.r: 所有策略r代码
* cal_ret.r: 业绩报告所用函数
* hrp.py: 用python写的策略的代码

运行
* 业绩报告.rmd: 整合所有策略的业绩报告

