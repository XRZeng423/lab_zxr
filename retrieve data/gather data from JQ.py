# JQ数据提取

#开始
import numpy as np
import pandas as pd
import jqdatasdk as jq
jq.auth("15102022205","steven990131")

#获取收盘行情（'open','close','low','high','volume','money','avg','pre_close'）
indexs=jq.get_index_stocks('000903.XSHG')
A=jq.get_price(indexs, start_date='2019-05-08', end_date='2019-05-08', frequency='daily', fields=['open','close','low','high','volume','money','avg','pre_close'], skip_paused=False, fq='pre')
B=A.to_frame()
B.to_csv('C:/Users/Administrator/Desktop/收盘行情20190508.csv')

#提取估值（'pe_ratio','turnover_ratio','pb_ratio','ps_ratio',market_cap','circulating_market_cap','pe_ratio_lyr'）
def maintask():
    a=pd.DataFrame()
    j=1
    for i in indexs:
        print('正在获取第%d家，股票代码%s.' % (j, i))
        j+=1
        q = jq.query(jq.valuation).filter(jq.valuation.code == i)
        df = jq.get_fundamentals(q, '2019-05-08')
        a=a.append(df)
        a.to_csv("C:/Users/Administrator/Desktop/估值20190508.csv")
        print(df)
        #print(a)
        
if __name__ == '__main__':
  maintask()

#区间成交量、成交额
range_jq=jq.get_price(indexs, start_date='2018-05-08', end_date='2019-05-08', frequency='365d', fields=['volume','money'], skip_paused=False, fq='pre', count=None)
range_jq.to_frame()
B.to_csv('C:/Users/Administrator/Desktop/1y_20180508.csv')

#区间涨跌幅
A=jq.get_price(indexs, start_date='2018-05-07', end_date='2019-05-08', frequency='1d', fields=['close'], skip_paused=False, fq='post', count=None)
B=A.pct_change()
B.to_csv('C:/Users/Administrator/Desktop/1y_pct_20190508.csv')

#区间换手率
def maintask():
    a=pd.DataFrame()
    j=1
    for i in indexs:
        print('正在获取第%d家，股票代码%s.' % (j, i))
        j+=1
        q = jq.query(jq.valuation.turnover_ratio).filter(jq.valuation.code == i)
        df = jq.get_fundamentals_continuously(q,end_date='2019-05-8', count=244)
        dff=df.to_frame()
        a=a.append(dff)
        a.to_csv("C:/Users/Administrator/Desktop/1y.csv")
        print(dff)
        #print(a)
        
if __name__ == '__main__':
  maintask()
