
# -*- coding: utf-8 -*-
"""
Created on Sun May  2 19:56:41 2021

@author: XuranZENG
"""

# Run some setup code.
%matplotlib inline 

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import copy

from keras.layers import Input, Dense
from keras.models import Model
from keras import regularizers
from keras.models import load_model
from sklearn.preprocessing import StandardScaler  
from collections import defaultdict


# Load data

# stock componenet data
stock = defaultdict(defaultdict)

# 将last price, net change, percentage change分为训练集和测试集，0-104为calibrate，后面是validate
stock_lp = pd.read_csv('E:/spyder/data2/last_price_hs300.csv',index_col=0).dropna(axis=1, how='any').astype('float32')
stock['calibrate']['lp'] = stock_lp.iloc[1:244, :]
stock['validate']['lp'] = stock_lp.iloc[244:, :]

stock_net = stock_lp.diff()
stock['calibrate']['net'] = stock_net.iloc[1:244, :]
stock['validate']['net'] = stock_net.iloc[244:, :]


stock_percentage = stock_lp.pct_change()
stock['calibrate']['percentage'] = stock_net.iloc[1:244, :]
stock['validate']['percentage'] = stock_net.iloc[244:, :]


# ibb data
# ibb是一个指数，包含若干成分股，目标是复现ibb指数
ibb = defaultdict(defaultdict)
ibb_full = pd.read_csv('E:/spyder/data2/hs300.csv', index_col=0).astype('float32')

ibb_lp = ibb_full.iloc[:,0] # Series
ibb['calibrate']['lp'] = ibb_lp[1:244]
ibb['validate']['lp'] = ibb_lp[244:]

ibb_net = ibb_full.iloc[:,1] # Series
ibb['calibrate']['net'] = ibb_net[1:244]
ibb['validate']['net'] = ibb_net[244:]

ibb_percentage = ibb_full.iloc[:,2] # Series
ibb['calibrate']['percentage'] = ibb_percentage[1:244]
ibb['validate']['percentage'] = ibb_percentage[244:]


# Phase 2: Auto-encoding


encoding_dim = 5 # 5 neurons
num_stock = len(stock_lp.columns) # Use 83 stocks as features

# connect all layers
input_img = Input(shape=(num_stock, ))
encoded = Dense(encoding_dim, activation='relu', kernel_regularizer=regularizers.l2(0.01))(input_img)
decoded = Dense(num_stock, activation= 'linear', kernel_regularizer=regularizers.l2(0.01))(encoded) # see 'Stacked Auto-Encoders' in paper

# construct and compile AE model
autoencoder = Model(input_img, decoded)
autoencoder.compile(optimizer='sgd', loss='mean_squared_error')

# train autoencoder
data = stock['calibrate']['net']
autoencoder.fit(data, data, shuffle=False, epochs=500, batch_size = 10)
autoencoder.save('model/retrack_autoencoder.h5')

# test/reconstruct market information matrix
reconstruct = autoencoder.predict(data)

communal_information = []

for i in range(0,(len(stock_lp.columns)-1)):
    diff = np.linalg.norm((data.iloc[:,i] - reconstruct[:,i])) # 2 norm difference
    communal_information.append(float(diff))
 
print("stock #, 2-norm, stock name")
ranking = np.array(communal_information).argsort()


ranking_table = pd.DataFrame(columns=['stock', '2-norm', 'stock name'])

for stock_index in ranking:
    
    ranking_table.loc[stock_index,'stock']   = stock_index
    ranking_table.loc[stock_index,'2-norm']   = communal_information[stock_index]
    ranking_table.loc[stock_index,'stock name']   = stock['calibrate']['net'].iloc[:,stock_index].name

    print(stock_index, communal_information[stock_index], stock['calibrate']['net'].iloc[:,stock_index].name) # print stock name from lowest different to highest


# visualize communals ranking
def plot_commonals(ranking_table):

    ranking_df = ranking_table.iloc[:,1:2]
    ranking_df.index = ranking_table.iloc[:,2]
    
    indexes = ranking_df.index
    cutted_indexes = [i for i in indexes]
    for i in range(len(cutted_indexes)):
        if i%15 != 0 and i != 0 and i!= len(cutted_indexes)-1:
            cutted_indexes[i] = ''
    for i in range(2,15):
        cutted_indexes[-i] = ''
    ranking_df = pd.DataFrame(ranking_df.values, index = cutted_indexes, columns=['2_norms'])
    
    ranking_df_test = ranking_df[:]
    ranking_df_test.plot.bar()
    plt.title('Commonals')
    plt.savefig('List of all commonals')
    plt.show()
    
plot_commonals(ranking_table)


which_stock = 211 # nice
#which_stock = 244 # not bad
which_stock = 244

# now decoded last price plot
stock_autoencoder = copy.deepcopy(reconstruct[:, which_stock])
stock_autoencoder[0] = 0
stock_autoencoder = stock_autoencoder.cumsum()
stock_autoencoder += (stock['calibrate']['lp'].iloc[0, which_stock])

## plot for comparison
pd.Series(stock['calibrate']['lp'].iloc[:, which_stock].to_numpy(), index=pd.date_range(start='01/04/2017', periods=243, freq='D')).plot(label='stock original', legend=True)
pd.Series(stock_autoencoder, index=pd.date_range(start='01/04/2017', periods = 243,freq='D')).plot(label='stock autoencoded', legend=True)

# Phase 2: Calibrating

ibb_predict = defaultdict(defaultdict)
total_2_norm_diff = defaultdict(defaultdict)
dl_scaler = defaultdict(StandardScaler)
relative_percentage_test = defaultdict(defaultdict)

for non_communal in [45, 105, 145]:  
    
    # some numerical values
    encoding_dim = 5
    communal = 30
    s = communal + non_communal
    
    stock_index = np.concatenate((ranking[0:communal], ranking[-non_communal:])) # portfolio index
    
    
    # connect all layers
    input_img = Input(shape=(s,))
    encoded = Dense(encoding_dim, activation='relu', kernel_regularizer=regularizers.l2(0.01))(input_img)
    decoded = Dense(1, activation= 'linear', kernel_regularizer=regularizers.l2(0.01))(encoded)
    
    
    # construct and compile deep learning routine
    deep_learner = Model(input_img, decoded)
    deep_learner.compile(optimizer='sgd', loss='mean_squared_error')
    
    x = stock['calibrate']['percentage'].iloc[:, stock_index]
    y = ibb['calibrate']['percentage']
    
    dl_scaler[s] = StandardScaler()       # Multi-layer Perceptron is sensitive to feature scaling, so it is highly recommended to scale your data
    dl_scaler[s].fit(x)
    x = dl_scaler[s].transform(x)  
    
    deep_learner.fit(x, y, shuffle=False, epochs=500, batch_size = 10)    # fit the model
    deep_learner.save('model/retrack_s' + str(s) + '.h5') # for validation phase use
    

    
    # is it good?
    relative_percentage = copy.deepcopy(deep_learner.predict(x))
    relative_percentage[0] = 0
    relative_percentage = (relative_percentage /100) + 1
    
    relative_percentage_test['calibrate'][s] = relative_percentage
    
    
    ibb_predict['calibrate'][s] = ibb['calibrate']['lp'][0] * (relative_percentage.cumprod())          
    total_2_norm_diff['calibrate'][s] = np.linalg.norm((ibb_predict['calibrate'][s] - ibb['calibrate']['lp']))

# construct a percentage change table    
relative_percentage_25 = pd.DataFrame(relative_percentage_test['calibrate'][75])
relative_percentage_45 = pd.DataFrame(relative_percentage_test['calibrate'][135])
relative_percentage_65 = pd.DataFrame(relative_percentage_test['calibrate'][175])
relative_percentage_ibb = pd.DataFrame((ibb['calibrate']['percentage']/100)+1)
relative_percentage_table = pd.DataFrame(np.concatenate((relative_percentage_25,relative_percentage_45, relative_percentage_65, relative_percentage_ibb),axis=1))
relative_percentage_table.columns = [75,135,175, 'original']
relative_percentage_table.index = data.index

# plot results and 2-norm differences 
pd.Series(ibb['calibrate']['lp'].to_numpy(), index=pd.date_range(start='01/04/2017', periods = 243,freq='D')).plot(label='HS300 original', legend=True)
  
for s in [ 75,135,175]:
    pd.Series(ibb_predict['calibrate'][s], index=pd.date_range(start='01/04/2017', periods = 243,freq='D')).plot(label='HS300 S'+str(s), legend=True)
    print("S" +str(s) + " 2-norm difference: ", total_2_norm_diff['calibrate'][s])


# Phase 3: validating
relative_percentage_test = defaultdict(defaultdict)

for non_communal in [45, 105, 145]:  
    
    # some numerical values
    encoding_dim = 5
    communal = 30
    s = communal + non_communal
    
    stock_index = np.concatenate((ranking[0:communal], ranking[-non_communal:])) # portfolio index
        
    # load our trained models
    deep_learner = load_model('model/retrack_s' + str(s) + '.h5')
    
    
    x = stock['validate']['percentage'].iloc[:, stock_index]
    x = dl_scaler[s].transform(x)  
    
    # is it good?
    relative_percentage = copy.deepcopy(deep_learner.predict(x))
    relative_percentage[0] = 0
    relative_percentage = (relative_percentage /100) + 1
    relative_percentage_test['validate'][s] = relative_percentage
    
    
    ibb_predict['validate'][s] = ibb['validate']['lp'][0] * (relative_percentage.cumprod())          
    total_2_norm_diff['validate'][s] = np.linalg.norm((ibb_predict['validate'][s][0:-1] - ibb['validate']['lp']))
    
 # construct a percentage change table    
relative_percentage_25 = pd.DataFrame(relative_percentage_test['validate'][75])[0:-1]
relative_percentage_45 = pd.DataFrame(relative_percentage_test['validate'][135])[0:-1]
relative_percentage_65 = pd.DataFrame(relative_percentage_test['validate'][175])[0:-1]
relative_percentage_ibb = pd.DataFrame((ibb['validate']['percentage']/100)+1)
relative_percentage_table = pd.DataFrame(np.concatenate((relative_percentage_25,relative_percentage_45, relative_percentage_65, relative_percentage_ibb),axis=1))
relative_percentage_table.columns = [75,135,175, 'original']
relative_percentage_table.index = ibb['validate']['percentage'].index
    
 
# plot results and 2-norm differences
pd.Series(ibb['validate']['lp'][0:370].to_numpy(), index=pd.date_range(start='01/02/2018', periods=370, freq='D')).plot(label='HS300 original', legend=True) 
  
for s in [75,135,175]:
    pd.Series(ibb_predict['validate'][s][0:370], index=pd.date_range(start='01/02/2018', periods=370, freq='D')).plot(label='HS300 S'+str(s), legend=True)
    #pd.Series(ibb_predict['validate'][s][0:-1], index=pd.date_range(start='01/02/2019', periods=565, freq='D')).plot(label='HS300 S'+str(s), legend=True)
    print("S" +str(s) + " 2-norm difference: ", total_2_norm_diff['validate'][s])
    
# Phase 4 : Verifying    
error = []
for non_communal in range(5, 79, 1):  
    
    # some numerical values
    encoding_dim = 5
    s = 30 + non_communal
    stock_index = np.concatenate((ranking[0:10], ranking[-non_communal:])) # portfolio index


    # training
    input_img = Input(shape=(s,))
    encoded = Dense(encoding_dim, activation='relu', kernel_regularizer=regularizers.l2(0.01))(input_img)
    decoded = Dense(1, activation= 'linear', kernel_regularizer=regularizers.l2(0.01))(encoded)
    
    deep_learner = Model(input_img, decoded)
    deep_learner.compile(optimizer='sgd', loss='mean_squared_error')
    
    x_train = stock['calibrate']['percentage'].iloc[:, stock_index]
    y_train = ibb['calibrate']['percentage']
    
    dl_scaler[s] = StandardScaler()       # Multi-layer Perceptron is sensitive to feature scaling, so it is highly recommended to scale your data
    dl_scaler[s].fit(x_train)
    x_train = dl_scaler[s].transform(x_train)  
    
    deep_learner.fit(x_train, y_train, shuffle=False, epochs=500, batch_size = 10)    # fit the model


    # testing
    x_test = stock['validate']['percentage'].iloc[:, stock_index]
    y_test = ibb['validate']['percentage']

    x_test = dl_scaler[s].transform(x_test)  
    
    relative_percentage = copy.deepcopy(deep_learner.predict(x_test))
    relative_percentage[0] = 0
    relative_percentage = (relative_percentage /100) + 1
    
    predict_curve = ibb['validate']['lp'][0] * (relative_percentage.cumprod())          
    error.append(np.linalg.norm((predict_curve - ibb['validate']['lp'])))
    
    
# Plot Efficient Deep Frontier

mse = [e/len(ibb['validate']['lp']) for e in error] # mse = sum of 2 norm difference/ # of test dates
plt.gca().invert_yaxis()
plt.plot(mse, list(range(5,79,1)))
plt.xlabel('Mean Squared Error')
plt.ylabel('number of stocks in the portfolio')


