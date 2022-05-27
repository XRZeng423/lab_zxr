### Data cleaning

# Perform data cleaning based on your EDA result
import numpy as np
import pandas as pd

def Data_cleaning(X):
    '''
       Perform Data cleaning procedure based on EDA result.
       (This code should match your EDA result.)

       Input:
       X       : Raw data

       Output:
       X       : Cleaned Data
    '''
    ##############################################################################
    ### TODO: Perform Data Cleaning procedure                                  ###
    ##############################################################################
    dummy_col = ['verification_status', 'purpose', 'initial_list_status',
                 'application_type', 'home_ownership', 'addr_state','term']

    emp_length_values = {'< 1 year': 0,
                     '1 year': 1,
                     '2 years' :2,
                     '3 years' : 3,
                     '4 years' : 4,
                     '5 years' : 5,
                     '6 years' : 6,
                     '7 years' : 7,
                     '8 years' : 8,
                     '9 years' : 9,
                     '10+ years' : 10}

    # term_values = {' 36 months': 36, ' 60 months': 60}

    # X.int_rate = X.int_rate.apply(lambda x: float(x[:-1]))
    # X.revol_util = X.revol_util.apply(lambda x: str(x)[:-1])
    # X.revol_util = X.revol_util.replace('na','0')
    # X.revol_util = X.revol_util.astype(np.float64)
    X.drop(columns = ['emp_title','title'],axis = 1, inplace=True)
    X.mort_acc.fillna(X.mort_acc.median(), inplace = True)
    # X['term'] = X.term.map(term_values)
    X.drop(columns = ['zip_code'], inplace = True)
    X= pd.get_dummies(X, columns=dummy_col, drop_first=True)
    X.drop('issue_d', axis=1, inplace=True)
    X['earliest_cr_line'] = X.earliest_cr_line.apply(lambda x: int(x[-4:]))
    X['emp_length'] = X.emp_length.map(emp_length_values)
    X.fillna(X.median(), inplace = True)


    ##############################################################################
    #                               END OF YOUR CODE                             #
    ##############################################################################
    
    return X

### Data cleaning

# Perform data cleaning based on your EDA result
import numpy as np
import pandas as pd

def Data_cleaning_grade(X):
    '''
       Perform Data cleaning procedure based on EDA result.
       (This code should match your EDA result.)

       Input:
       X       : Raw data

       Output:
       X       : Cleaned Data
    '''
    ##############################################################################
    ### TODO: Perform Data Cleaning procedure                                  ###
    ##############################################################################
    dummy_col = ['verification_status', 'purpose', 'initial_list_status',
                 'application_type', 'home_ownership', 'addr_state']

    emp_length_values = {'< 1 year': 0,
                     '1 year': 1,
                     '2 years' :2,
                     '3 years' : 3,
                     '4 years' : 4,
                     '5 years' : 5,
                     '6 years' : 6,
                     '7 years' : 7,
                     '8 years' : 8,
                     '9 years' : 9,
                     '10+ years' : 10}

    term_values = {' 36 months': 36, ' 60 months': 60}

    X.int_rate = X.int_rate.apply(lambda x: float(x[:-1]))
    X.revol_util = X.revol_util.apply(lambda x: str(x)[:-1])
    X.revol_util = X.revol_util.replace('na','0')
    X.revol_util = X.revol_util.astype(np.float64)
    X.drop(columns = ['emp_title','title'],axis = 1, inplace=True)
    X.mort_acc.fillna(X.mort_acc.median(), inplace = True)
    X['term'] = X.term.map(term_values)
    X.drop(columns = ['sub_grade','zip_code'], inplace = True)
    X= pd.get_dummies(X, columns=dummy_col, drop_first=True)
    X.drop('issue_d', axis=1, inplace=True)
    X['earliest_cr_line'] = X.earliest_cr_line.apply(lambda x: int(x[-4:]))
    X['emp_length'] = X.emp_length.map(emp_length_values)
    X.fillna(X.median(), inplace = True)


    ##############################################################################
    #                               END OF YOUR CODE                             #
    ##############################################################################
    
    return X

### Data cleaning

# Perform data cleaning based on your EDA result
import numpy as np
import pandas as pd

def Data_cleaning_multi(X):
    '''
       Perform Data cleaning procedure based on EDA result.
       (This code should match your EDA result.)

       Input:
       X       : Raw data

       Output:
       X       : Cleaned Data
    '''
    ##############################################################################
    ### TODO: Perform Data Cleaning procedure                                  ###
    ##############################################################################
    dummy_col = ['sub_grade', 'verification_status', 'purpose', 'initial_list_status',
                 'application_type', 'home_ownership', 'addr_state','term']

    emp_length_values = {'< 1 year': 0,
                     '1 year': 1,
                     '2 years' :2,
                     '3 years' : 3,
                     '4 years' : 4,
                     '5 years' : 5,
                     '6 years' : 6,
                     '7 years' : 7,
                     '8 years' : 8,
                     '9 years' : 9,
                     '10+ years' : 10}

    term_values = {' 36 months': 36, ' 60 months': 60}

    # X.int_rate = X.int_rate.apply(lambda x: float(x[:-1]))
    X.revol_util = X.revol_util.apply(lambda x: str(x)[:-1])
    X.revol_util = X.revol_util.replace('na','0')
    X.revol_util = X.revol_util.astype(np.float64)
    # X.drop(columns = ['emp_title','title'],axis = 1, inplace=True)
    # X.mort_acc.fillna(X.mort_acc.median(), inplace = True)
    # X['term'] = X.term.map(term_values)
    X.drop(columns = ['grade','zip_code'], inplace = True)
    X= pd.get_dummies(X, columns=dummy_col, drop_first=True)
    X.drop('issue_d', axis=1, inplace=True)
    X['earliest_cr_line'] = X.earliest_cr_line.apply(lambda x: int(x[-4:]))
    X['emp_length'] = X.emp_length.map(emp_length_values)
    X.fillna(X.median(), inplace = True)


    ##############################################################################
    #                               END OF YOUR CODE                             #
    ##############################################################################
    
    return X
