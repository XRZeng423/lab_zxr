# -*- coding: utf-8 -*-
"""
Created on Tue Oct 22 15:55:21 2019

@author:  86572
"""

#!/usr/bin/env python
# coding: utf-8




import numpy as np
from pandas import DataFrame
import pandas as pd
import datetime #引入datetime module not just datetime class
import time
import os

import sys
sys.path.append(r'E:\data') #daily_to_monthly.py 文件所在的文件夹
import daily_to_monthly


# 导入数据
t0=time.time()

# 地址
host = '172.19.3.249'
# 端口
port = 3306
# 用户名
username = "ktruc001"
# 连接密码
password = "ktruc001"
# 数据库名
database = "cn_stock_quote"
# 表名
collection = 'daily_quote'

# 连接数据库
conn = pymysql.connect(host=host, port=port, user=username, password=password, db=database)
# 使用cursor()方法获取操作游标
cur = conn.cursor()
# 查询数据
#fields = get_field(cur, collection, database)


#if 'trade_date' in fields:
date = 'trade_date'

sql = "SELECT `code`,  `shortname`,  `trade_date`,  `type`,  `exchange`,  `pre_close`,  `issue_share` FROM %s WHERE  %s> '%s' AND %s< '%s' " % (collection, date,'2005/06/27 0:00:00', date, '2019/12/25 0:00:00') 
#开区间，所以抽出来的数据是从2005/06/28-2019/12/24
cur.execute(sql)
rows = cur.fetchall()

dataframe_all_daily_quote = pd.DataFrame(list(rows),columns=['code',  'shortname',  'trade_date',  'type_name',  'exchange',  'pre_close',  'issue_share'])#创建DataFrame
# 关闭数据库
cur.close()
conn.close()


# 数据库名
database = "cn_stock_basic"
# 表名
collection = 'financial_index_general'
# 查询的股票代码，为None将返回所有数据
# 连接数据库
conn = pymysql.connect(host=host, port=port, user=username, password=password, db=database)
# 使用cursor()方法获取操作游标
cur = conn.cursor()
# 查询数据

date = 'end_date'
sql = "SELECT  `company_id`,  `code`,  `pub_date`,  `start_date`,  `end_date`,  `pb`,  `total_profit`,  `total_asset`,  `total_owner_equity` FROM %s WHERE  %s> '%s' AND %s< '%s' " % (collection, date,'2003/12/30 0:00:00', date, '2019/01/01 0:00:00')
cur.execute(sql)
rows = cur.fetchall()

dataframe_all_financial = pd.DataFrame(list(rows),columns=['company_id',  'code',  'pub_date',  'start_date',  'end_date',  'pb',  'total_profit',  'total_asset',  'total_owner_equity'])#创建DataFrame

# 关闭数据库
cur.close()
conn.close()
t1=time.time()

print('time spent to introduce all the data', t1-t0)
#%%
# 数据清洗
#把type不是a股的包括type值缺失（指数数据）daily quote数据全部删掉（daily quote里面有指数数据,国债等）
#输入导入的daily_quote 的df数据

t2=time.time()
def del_nona(df):     
    index_del=df[df.type_name!='A股'].index.tolist()
    #找出type不是A股的所在行索引
    df=df.drop(index_del)
    #删除行索引对应的行数据
    return df

dataframe_all_daily_quote= del_nona(dataframe_all_daily_quote)

t_mid=time.time()
print('time spent to drop not type A stocks in daily quote', t_mid-t2)

#trade_date 和 end date的数据格式都是datetime
#print(dataframe_all_daily_quote['trade_date']) #trade_date：2016-10-01 datetime64
#print(dataframe_all_financial['end_date']) #end_date:2019-06-30 datetime64

dataframe_all_daily_quote.rename(columns={'trade_date':'short_date'}, inplace=True)
dataframe_all_financial.rename(columns={'end_date':'short_date'},inplace=True)


#把code的形式全部统一 6位数，前面的用0补足 (注意现在code的格式是int64, 经过这一步之后code的格式变为str）


'''
t_transformcode0=time.time()
def transform_code(df):
    df['code']=df['code'].map(lambda x:str(x).zfill(6))
    return df

dataframe_all_daily_quote=transform_code(dataframe_all_daily_quote)
dataframe_all_financial=transform_code(dataframe_all_financial)

t_transformcode=time.time()
print('time spent to transform the code', t_transformcode-t_transformcode0)
'''


#在daily quote中添加收盘价，收益率，总市值数据
#把所有表里面的数据格式都改成np。float64 和python里面的 float是一样的 （不同于decimal

t_add0=time.time()
def add_close_return_cap(df):
    df.sort_values(by=['code','short_date'],ascending=True,inplace=True)
    #先按照公司code顺序排序
    #注意这个时候最后一天的收盘价都是不准的，因为其实是下一个公司的pre-close
    #所以trade_date_cover里面没有最后一天
    df['close']=df['pre_close'].shift(-1)
    #df['returns']=df['close'].diff()/df['pre_close']
    df['returns_decimal']=df['close']/df['pre_close']-1
    #df['returns']=financial_12['returns_decimal'].astype(np.float64)
    df['returns']=df['returns_decimal'].map(lambda x:float(x))
    df['close']=df['close'].map(lambda x:float(x))
    df['issue_share']=df['issue_share'].map(lambda x:float(x))
    df['total_market_cap'] = df.apply(lambda x: x['close']* x['issue_share'], axis=1)
    return df

dataframe_all_daily_quote=add_close_return_cap(dataframe_all_daily_quote)

t_add=time.time()
print('time spent to clean the data in daily quote', t_add-t_add0)


#%%
# 整理出所有需要用到的日期：trade_date_cover,trade_date_juneend,fin_yearend_date_list
#确定daily_quote所覆盖的时间区间
t4=time.time()
trade_date_daily=sorted(set(dataframe_all_daily_quote['short_date']))[1:]#属性series->set 集合元素不可重复 但是sorted 又把它变成list了
trade_date_daily=trade_date_daily[:-1]#最后一天没有收盘价，把他去了
#print(trade_date_daily) #确定trade date daily是datetime


#找出所需要的每年6月30日的日期
#years=sorted(set([x[:4] for x in trade_date_daily])) 前提 x为str
#juneend_date= [x+'-06-30'for x in years]
years=sorted(set([x.year for x in trade_date_daily])) #里面的元素为int
juneend_date_str= [str(x)+'-06-30'for x in years]
juneend_date=[datetime.datetime.strptime(i,'%Y-%m-%d') for i in juneend_date_str]#里面的元素为datetime

#确保trade date daily的日期涵盖这些调仓节点
if juneend_date[0]<trade_date_daily[0]:
    del juneend_date[0]
if juneend_date[-1]>trade_date_daily[-1]:
    del juneend_date[-1]


#取每一年6月底最近的交易日 
#用trade date daily 而不用ts数据的前提条件是调仓日期必须在trade date daily必须涵盖这几个时间节点，上面的操作已经实现
"""
trade_date_juneend = []
for i in juneend_date: 
    if i in trade_date_daily:
        trade_date_juneend.append(i)#如果这一天开市，就把这一天的日期加在表中
    else:
        trade_date_1 = datetime.datetime.strptime(i,'%Y-%m-%d') - datetime.timedelta(days=1)
        #如果这一天没有开市，求其前一天（这个时候的格式是datatime
        if trade_date_1.strftime('%Y-%m-%d')in trade_date_daily:
            trade_date_juneend.append(trade_date_1.strftime('%Y-%m-%d'))
            #如果前一天开市了，加入列表
        else:
            trade_date_2 = datetime.datetime.strptime(i,'%Y-%m-%d') - datetime.timedelta(days=2) 
            trade_date_juneend.append(trade_date_2.strftime('%Y-%m-%d'))
            #如果前一天也没开市，把前两天加入列表
"""

trade_date_juneend = []
for i in juneend_date: 
    if i in trade_date_daily:
        trade_date_juneend.append(i)#如果这一天开市，就把这一天的日期加在表中
    else:
        trade_date_1 = i - datetime.timedelta(days=1)
        #如果这一天没有开市，求其前一天（这个时候的格式是datatime
        if trade_date_1 in trade_date_daily:
            trade_date_juneend.append(trade_date_1)
            #如果前一天开市了，加入列表
        else:
            trade_date_2 = i - datetime.timedelta(days=2) 
            trade_date_juneend.append(trade_date_2)
            #如果前一天也没开市，把前两天加入列表
                

#FF5，FF3所有的因子值如果只能用所给数据计算，可以cover的日期
trade_date_cover=[x for x in trade_date_daily if x >trade_date_juneend[0]]
#注意去掉每次第一次调仓的当天 

def fin_yearend_date(df):#输入trade_date_juneend
    years=[x.year for x in df] #元素是int
    start_year=df[0].year
    pre_year=start_year-1#第一次调仓那年的前一年
    pre_pre_year=pre_year-1#在计算Inv的时候需要用到前两年的数据
    years.append(pre_year)
    years.append(pre_pre_year)
    years=sorted(years)
    years=years[:-1] #最后一次调仓用的是前一年的
    fin_yearend_date_list_=[str(x)+'-12-31'for x in years]
    fin_yearend_date_list_dt=[datetime.datetime.strptime(i,'%Y-%m-%d') for i in fin_yearend_date_list_]
    
    
    return fin_yearend_date_list_dt

fin_yearend_date_list=fin_yearend_date(trade_date_juneend)


t5=time.time()
print('time spent to sort the dates', t5-t4)


#%%
# 对财务数据做处理：保留年报数据，添加use year，以及之后用来确定投资组合的各个指标
t6=time.time()
dataframe_all_financial.dropna(axis=1, how='all', inplace=True)
#因为是一年调一次仓的时候需要用到financial，所以只保留年报数据，即short_date 是12-31结尾

financial_12 = dataframe_all_financial[dataframe_all_financial['short_date'].isin(fin_yearend_date_list)]
#提取有用的financial数据

#添加我们什么时候会用到这个年报数据的日期，因为涉及到具体trade day 比较麻烦 所以直接写年
financial_12['use_year']=financial_12['short_date'].map(lambda x: x.year+1) #注意这边use year的格式是int

#把表格先按照code再按照use year 排序，方便后面shift得到pre total asset
financial_12.sort_values(by=['code','use_year'],ascending=True,inplace=True)

#在financial表中添加帐面市值比BM=1/pb,营业利润OP
financial_12['pb']=financial_12['pb'].astype(np.float64)
financial_12['BM']=1/financial_12['pb']

financial_12['total_profit']=financial_12['total_profit'].astype(np.float64)
financial_12['total_owner_equity']=financial_12['total_owner_equity'].astype(np.float64)

financial_12['OP']=financial_12['total_profit']/financial_12['total_owner_equity']

financial_12['total_asset']=financial_12['total_asset'].astype(np.float64)

financial_12['pre_total_asset']=financial_12['total_asset'].shift(1)
financial_12['Inv']=financial_12['total_asset'].diff()/financial_12['pre_total_asset']

#注意此时第一年的Inv是错的 不能用，但是不影响，用trade date juneend调用的时候不会用到
t7=time.time()
print('time spent to clean the data in financial df', t7-t6)


#%%
t_indroduce_rf0=time.time()
path_rf = r'E:\data\rf_trading date.csv'#编码是utf-8

df_risk_free_rate=pd.read_csv(path_rf,encoding='utf-8',low_memory = False)#有两列：date，rate。 date的数据格式是str，rate的数据格式是float,注意此时单位为1%

df_risk_free_rate['date_dt']=df_risk_free_rate['date'].astype('datetime64[ns]')#增加一列为datetime格式的日期
#df_risk_free_rate['date_dt']=df_risk_free_rate['date'].map(lambda x: datetime.datetime.strptime(x,'%Y-%m-%d'))# 会报错 因为 1992/2/1 不符合%Y-%m-%的格式
t_indroduce_rf=time.time()
print('time spent to introduce rf.csv', t_indroduce_rf-t_indroduce_rf0)

#%%
#FF5计算Rm-Rf因子
t8=time.time()
def cal_Rm_minus_Rf(trade_date_cover_):

    Rm_daily=pd.DataFrame(columns=['Rm-Rf'],index=trade_date_cover_)

    for i in trade_date_cover_: 

        df_daily_quote_1=dataframe_all_daily_quote.loc[dataframe_all_daily_quote['short_date']==i]

        #提取出这一天的所有a股信息

        all_market_cap=float(df_daily_quote_1['total_market_cap'].sum())
        
        if all_market_cap==0: ##这一块的代码是因为数据库的错，等到他们那边修复完了 就可以删掉这个if了
            print(i)
            #print(df_daily_quote_1)
            i_index=trade_date_cover_.index(i)
            pre=trade_date_cover_[i_index-1]
            df_daily_quote_1=dataframe_all_daily_quote.loc[dataframe_all_daily_quote['short_date']==pre]
            all_market_cap=float(df_daily_quote_1['total_market_cap'].sum())

        df_daily_quote_1['weight']= df_daily_quote_1['total_market_cap'].map(lambda x:float(x/all_market_cap))

        rm=(df_daily_quote_1['returns'] * df_daily_quote_1['weight']).sum() 

        risk_free_rate=df_risk_free_rate['rate'][df_risk_free_rate['date_dt']==i].values[0]/100

        Rm_daily.loc[i,'Rm-Rf']=float(rm)-risk_free_rate

    return Rm_daily

Rm_minus_Rf=cal_Rm_minus_Rf(trade_date_cover)
#FF5计算Rm-Rf因子


t9=time.time()
print('time spent to calculate Rm-rf', t9-t8)


#%%FF5

#筛选出一张新的daily quote，只包含这些公司的daily quote数据,inter可以是set/list
def filter_(df,codelist):
    df_new=df[df['code'].isin(codelist)]
    return df_new



#删除某一列有缺失的行
def del_novalue(df,namelist):
    for name in namelist:
        df[name]=df[name].fillna('9999999999')
        to_del=df[df[name]=='9999999999'].index.tolist()
        df=df.drop(to_del)
    return df

#%%
#在调仓日那天对股票池的股票进行划分，组建投资组合
#tp是调仓日的日期，tp is in trade_date_juneend
def get_18groups(tp,year):
    
    #调出当天两张表的数据
    #financial 12需要用use year 即日期的前4位进行调取
    financial_12_tp=financial_12[financial_12['use_year']==int(year)]
    
    
    #删除当天没有这个指标值的股票（如果之后输入换成已经筛选过的，调仓日当天有这些数据的股票那么这行可以不用）
    financial_12_tp=del_novalue(financial_12_tp,['BM','OP','Inv'])
    intersection=financial_12_tp['code'].values
    
    daily_quote_tp=dataframe_all_daily_quote[dataframe_all_daily_quote['short_date']==tp]
    daily_quote_tp=del_novalue(daily_quote_tp,['total_market_cap'])
    
    
    #找各个分位点
    size_med=daily_quote_tp['total_market_cap'].median()
    BMborder_down, BMborder_up = financial_12_tp['BM'].quantile([0.3, 0.7]) 
    OPborder_down, OPborder_up = financial_12_tp['OP'].quantile([0.3, 0.7]) 
    INVborder_down, INVborder_up = financial_12_tp['OP'].quantile([0.3, 0.7]) 
    
    #求各个分组中code的集合
    S=daily_quote_tp['code'][daily_quote_tp['total_market_cap']<=size_med].tolist()
    #这边把等于中位数的划分为S了
    #tolist（）把series转成list
    B=daily_quote_tp['code'][daily_quote_tp['total_market_cap']>size_med].tolist()
    
    L=financial_12_tp['code'][financial_12_tp['BM']<=BMborder_down].tolist()#low-BM
    H=financial_12_tp['code'][financial_12_tp['BM']>BMborder_up].tolist()#high-BM
    N_BM=list(set(intersection).difference(set(L+H)))
    
    W=financial_12_tp['code'][financial_12_tp['OP']<=OPborder_down].tolist()
    R=financial_12_tp['code'][financial_12_tp['OP']>OPborder_up].tolist()
    N_OP=list(set(intersection).difference(set(W+R)))
    
    A=financial_12_tp['code'][financial_12_tp['Inv']>INVborder_up].tolist()
    C=financial_12_tp['code'][financial_12_tp['Inv']<=INVborder_down].tolist()
    N_Inv=list(set(intersection).difference(set(A+C)))
    
    #get 18 groups
    SH_BM=list(set(S)&set(H))
    SN_BM=list(set(S)&set(N_BM))
    SL_BM=list(set(S)&set(L))
    BH_BM=list(set(B)&set(H))
    BN_BM=list(set(B)&set(N_BM))
    BL_BM=list(set(B)&set(L))
    
    
    SR_OP=list(set(S)&set(R))
    SN_OP=list(set(S)&set(N_OP))
    SW_OP=list(set(S)&set(W))
    BR_OP=list(set(B)&set(R))
    BN_OP=list(set(B)&set(N_OP))
    BW_OP=list(set(B)&set(W))
    
    
    SC_Inv=list(set(S)&set(C))
    SN_Inv=list(set(S)&set(N_Inv))
    SA_Inv=list(set(S)&set(A))
    BC_Inv=list(set(B)&set(C))
    BN_Inv=list(set(B)&set(N_Inv))
    BA_Inv=list(set(B)&set(A))
    
    
    name=['SH_BM','SN_BM', 'SL_BM','BH_BM','BN_BM', 'BL_BM','SR_OP','SN_OP', 'SW_OP', 'BR_OP', 'BN_OP','BW_OP','SC_Inv','SN_Inv','SA_Inv','BC_Inv','BN_Inv','BA_Inv']
    port_list=[SH_BM,SN_BM,SL_BM,BH_BM,BN_BM,BL_BM,SR_OP,SN_OP,SW_OP,BR_OP,BN_OP,BW_OP,SC_Inv,SN_Inv,SA_Inv,BC_Inv,BN_Inv,BA_Inv]
    for i in port_list:
        v=port_list.index(i)
        dic={name[v]:i}
        portfolios_df=pd.DataFrame(dic)
        portfolios_df.to_csv('E:\data\FF_18_Portfolios got at june end of' + str(year)+'\portfolio'+ str(name[v])+ 'got at june end of' +str(year) + '.csv', index=False,header=True)
    
    return SH_BM,SN_BM,SL_BM,BH_BM,BN_BM,BL_BM,SR_OP,SN_OP,SW_OP,BR_OP,BN_OP,BW_OP,SC_Inv,SN_Inv,SA_Inv,BC_Inv,BN_Inv,BA_Inv

#%%

#得到投资组合x在一年的期间内日收益率序列
#注意每个投资组合x的有效时间是一年
def get_dailyreturn(x,year,tp):
    year=str(year)
    start_date_str=year+'-06-30'
    start_date=datetime.datetime.strptime(start_date_str,'%Y-%m-%d')
    year_after=str(int(year)+1)
    end_date_str=year_after+'-06-30'
    end_date=datetime.datetime.strptime(end_date_str,'%Y-%m-%d')
    
    
    
    trade_date_cover_=[x for x in trade_date_cover if x >start_date and x<=end_date]
    #这边左边不取等于的原因是那一天仍然会按照上一年的去调仓

    R=pd.DataFrame(columns=['R'],index=trade_date_cover_)
    
    
    #提取出这些公司的daily quote
    company_daily_quote=filter_(dataframe_all_daily_quote,x)#这是之前自定义的函数
    
    #提取出调仓日tp当天的daily quote 如果下面用的不是调仓日当天的固定市值权重的话 这行代码就不用了
    #company_daily_quote_tp=company_daily_quote.loc[company_daily_quote['short_date']==tp].set_index('code')#这边去不去掉loc是一样的 
    #print(company_daily_quote_tp['total_market_cap'])

    for i in trade_date_cover_:
        #提取出这一天所有公司的daily quote 数据
        daily_quote_1=company_daily_quote.loc[company_daily_quote['short_date']==i].set_index('code')
        #print((daily_quote_1['returns']*company_daily_quote_tp['total_market_cap']).sum())
        #a=trade_date_cover_.index(i)
        #这边一定要注意！！！用的权重是每天的市值比！！！
        all_market_cap = daily_quote_1['total_market_cap'].sum()
        
        if all_market_cap==0: ##这一块的代码是因为数据库的错，等到他们那边修复完了 就可以删掉这个if了
            #print(i)
            #print(daily_quote_1)
            i_index=trade_date_cover_.index(i)
            pre=trade_date_cover_[i_index-1]
            daily_quote_1=dataframe_all_daily_quote.loc[dataframe_all_daily_quote['short_date']==pre]
            all_market_cap=float(daily_quote_1['total_market_cap'].sum())
        
        R.loc[i,'R'] = (daily_quote_1['returns'] * daily_quote_1['total_market_cap']).sum()/all_market_cap 
        
        #下面的代码计算的是以调仓日那天的value 作为固定权重而计算的回报率
        #R.loc[i,'R']=(daily_quote_1['returns']*company_daily_quote_tp['total_market_cap']).sum()/company_daily_quote_tp['total_market_cap'].sum()
        
        """
        all_cap=float(company_daily_quote_tp['total_market_cap'].sum())
        company_daily_quote_tp['weight@daily']= company_daily_quote_tp['total_market_cap'].map(lambda x: float(x/all_cap))
        r_portfolio=(daily_quote_1['returns'] * company_daily_quote_tp['weight@tp']).sum() #returns 为双精度，所以weight必须也转化成双精度，而不能用单精度的float
        R.loc[i,'R']=float(r_portfolio)
        """
    return R


#%%
#计算4个因子值

def findtp(year):
    for i in trade_date_juneend:
        if i.year==year:
            return i

R_COMPLETE=pd.DataFrame()

yearlist=[x.year for x in trade_date_juneend]



for year in yearlist:#year is str
    #通过输入的year找每一年的调仓日
    tp=findtp(year)
    #得到这一年调仓日调仓的所有投资组合
    t10=time.time()
    SH_BM,SN_BM,SL_BM,BH_BM,BN_BM,BL_BM,SR_OP,SN_OP,SW_OP,BR_OP,BN_OP,BW_OP,SC_Inv,SN_Inv,SA_Inv,BC_Inv,BN_Inv,BA_Inv=get_18groups(tp,year)
    t11=time.time()
    print('time spent to get 18 portfolios at'+str(tp), t11-t10) 
    
    
    t12=time.time()
    R_SH_BM=get_dailyreturn(SH_BM,year,tp) #得到18个投资组合的收益率的df
    R_SN_BM=get_dailyreturn(SN_BM,year,tp)
    R_SL_BM=get_dailyreturn(SL_BM,year,tp)
    R_BH_BM=get_dailyreturn(BH_BM,year,tp)
    R_BN_BM=get_dailyreturn(BN_BM,year,tp)
    R_BL_BM=get_dailyreturn(BL_BM,year,tp)
    
    R_SR_OP=get_dailyreturn(SR_OP,year,tp)
    R_SN_OP=get_dailyreturn(SN_OP,year,tp)
    R_SW_OP=get_dailyreturn(SW_OP,year,tp)
    R_BR_OP=get_dailyreturn(BR_OP,year,tp)
    R_BN_OP=get_dailyreturn(BN_OP,year,tp)
    R_BW_OP=get_dailyreturn(BW_OP,year,tp)
    
    R_SC_Inv=get_dailyreturn(SC_Inv,year,tp)
    R_SN_Inv=get_dailyreturn(SN_Inv,year,tp)
    R_SA_Inv=get_dailyreturn(SA_Inv,year,tp)
    R_BC_Inv=get_dailyreturn(BC_Inv,year,tp)
    R_BN_Inv=get_dailyreturn(BN_Inv,year,tp)
    R_BA_Inv=get_dailyreturn(BA_Inv,year,tp)
    
    
    R_all=pd.concat([R_SH_BM,R_SN_BM,R_SL_BM,R_BH_BM,R_BN_BM,R_BL_BM,R_SR_OP,R_SN_OP,R_SW_OP,R_BR_OP,R_BN_OP,R_BW_OP,R_SC_Inv,R_SN_Inv,R_SA_Inv,R_BC_Inv,R_BN_Inv,R_BA_Inv],axis=1)
    SH_BM=R_SH_BM['R'].values.T
    SN_BM=R_SN_BM['R'].values.T
    SL_BM=R_SL_BM['R'].values.T
    BH_BM=R_BH_BM['R'].values.T
    BN_BM=R_BN_BM['R'].values.T
    BL_BM=R_BL_BM['R'].values.T
    SR_OP=R_SR_OP['R'].values.T
    SN_OP=R_SN_OP['R'].values.T
    SW_OP=R_SW_OP['R'].values.T
    BR_OP=R_BR_OP['R'].values.T
    BN_OP=R_BN_OP['R'].values.T
    BW_OP=R_BW_OP['R'].values.T
    SC_Inv=R_SC_Inv['R'].values.T
    SN_Inv=R_SN_Inv['R'].values.T
    SA_Inv=R_SA_Inv['R'].values.T
    BC_Inv=R_BC_Inv['R'].values.T
    BN_Inv=R_BN_Inv['R'].values.T
    BA_Inv=R_BA_Inv['R'].values.T
    
    
    R_all['SMB']=1/3*((SH_BM+SN_BM+SL_BM)/3-(BH_BM+BN_BM+BL_BM)/3+(SR_OP+SN_OP+SW_OP)/3-(BR_OP+BN_OP+BW_OP)/3+(SC_Inv+SN_Inv+SA_Inv)/3-(BC_Inv+BN_Inv+BA_Inv)/3)
    R_all['HML']=1/2*(BH_BM+SH_BM-BL_BM-SL_BM)
    R_all['RMW']=1/2*(BR_OP+SR_OP-BW_OP-SW_OP)
    R_all['CMA']=1/2*(BC_Inv+SC_Inv-BA_Inv-SA_Inv)
    #FF3的前期与FF5重合，在此基础上直接计算即可
    R_all['SMB3']=1/3*((SH_BM+SN_BM+SL_BM)-(BH_BM+BN_BM+BL_BM)) 
    R_all['HML3']=1/2*(BH_BM+SH_BM-BL_BM-SL_BM)
    R_COMPLETE=pd.concat([R_COMPLETE,R_all], axis=0)


    t13=time.time()
    print('time spent to get the returns of 18 portfolios at'+str(tp), t13-t12) 
    
#print(R_COMPLETE)



 
#%%
#整合
cur_dir="E:/data/results"
period_calculated="/2005-07-01 to 2019-12-23"

#三因子
folder_name3="three_factors"
path_3=cur_dir+"/"+folder_name3
if os.path.isdir(cur_dir) and not os.path.isdir(path_3):
    os.mkdir(os.path.join(cur_dir, folder_name3))
FF3_2factors=R_COMPLETE[['SMB3','HML3']]
FF3=pd.concat([Rm_minus_Rf,FF3_2factors],axis=1)
FF3.to_csv(path_3 + period_calculated + "daily FF3.csv",index=True,header=True)
monthly_ff3,summary_ff3=daily_to_monthly.to_monthly_summary(FF3)
monthly_ff3.to_csv(path_3 + period_calculated + "monthly FF3.csv",index=True,header=True)
summary_ff3.to_csv(path_3 + period_calculated + "summary FF3.csv",index=True,header=True)

#五因子
folder_name5="five_factors"
path_5=cur_dir+"/"+folder_name5
if os.path.isdir(cur_dir) and not os.path.isdir(path_5):
    os.mkdir(os.path.join(cur_dir, folder_name5))
FF5_4factors=R_COMPLETE[['SMB','HML','RMW','CMA']]
FF5=pd.concat([Rm_minus_Rf,FF5_4factors],axis=1)
FF5.to_csv(path_5 + period_calculated + "daily FF5.csv",index=True,header=True)
monthly_ff5,summary_ff5=daily_to_monthly.to_monthly_summary(FF5)
monthly_ff5.to_csv(path_5 + period_calculated + "monthly FF5.csv",index=True,header=True)
summary_ff5.to_csv(path_5 + period_calculated + "summary FF5.csv",index=True,header=True)


#具体的组合收益不计算月频
folder_name_port="6portfolio"
path_port=cur_dir+"/"+folder_name_port
if os.path.isdir(cur_dir) and not os.path.isdir(path_port):
    os.mkdir(os.path.join(cur_dir, folder_name_port))

FF_6port_SizeBM=R_COMPLETE.iloc[:,[0,1,2,3,4,5]]
FF_6port_SizeBM.columns=['SH','SN','SL','BH','BN','BL']
FF_6port_SizeOP=R_COMPLETE.iloc[:,[6,7,8,9,10,11]]
FF_6port_SizeOP.columns=['SR','SN','SW','BR','BN','BW']
FF_6port_SizeInv=R_COMPLETE.iloc[:,[12,13,14,15,16,17]]
FF_6port_SizeInv.columns=['SC','SN','SA','BC','BN','BA']

FF_6port_SizeBM.to_csv(path_port + "/Size_and_Book-to-Market(2by3).csv",index=True,header=True)
FF_6port_SizeOP.to_csv(path_port + '/Size_and_Operating_Profitability(2by3).csv',index=True,header=True)
FF_6port_SizeInv.to_csv(path_port + '/Size_and_Investment(2by3).csv',index=True,header=True)


#%%
