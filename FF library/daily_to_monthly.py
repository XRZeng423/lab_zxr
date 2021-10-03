import pandas as pd
import numpy as np
from datetime import datetime, timedelta

#导入原始数据：
#df=pd.read_csv("ChinaFF_results.csv",index_col=0)#原有数据第一列当做index 就是日期
#df=df.sort_index(ascending=True)

#定义大函数
#此函数input为因子日频数据，将返回月频数据（dataframe)，以及一个关于此csv的简介(dataframe)
#简介是关于当月，近三个月，近十二个月为节点的因子值
def to_monthly_summary(df):
    df_value=df.dropna()
    df_value=df_value.fillna(0)
    # 将日期读取
    df_value.index=pd.to_datetime(df_value.index)
    # 累计乘法得到净值
    df_value = (df_value+1).cumprod()

    # 生成月频倒序--------------------------
    output1 = df_value.resample("M").last().pct_change()
    output1.index = output1.index.strftime("%Y-%m")  
    #第一个月收益率有可能不准确，且去掉na
    output1 = output1[2:] 
    output1 = output1.sort_index(ascending=False)

    #生成summary--------------------------
    output2=pd.DataFrame()
    #current_month与上方计算最后一个月的收益率的结果相同，拷贝改名即可
    #all_months=pd.date_range(df.index[0],df.index[-1],freq="M")
    #all_months_middle=all_months.drop(all_months[0])
    current_month=output1.iloc[0].copy()
    current_month=current_month.rename("Current Month")
    output2=output2.append(current_month)
    #用最后一天的值除以三个月前那个月的最后交易日,再-1
    last_3_month=df_value.iloc[-1]/df_value[output1.index[3]].iloc[-1]-1
    last_3_month=last_3_month.rename("Last 3 Month")
    output2=output2.append(last_3_month)

    #用最后一天的值除以十二个月前那个月的最后交易日,再-1
    last_12_month=df_value.iloc[-1]/df_value[output1.index[12]].iloc[-1]-1
    last_12_month=last_12_month.rename("Last 12 Month")
    output2=output2.append(last_12_month)
    output2=output2.transpose()
    monthly_results=output1
    summary_results=output2
    return monthly_results, summary_results

#to_monthly_summary(df)
    