#Only PASAT or only PVT Processing
import csv
import pandas as pd
import numpy as np
import glob, os
import re

data = pd.read_csv("C:\\Users\\span\\Desktop\\Heather Misc\\PASAT\\PASAT Only Behavioral Data Summary.csv")
data = data.drop(['ID'], axis=1)
data = data.apply(pd.to_numeric, errors='coerce', axis=1)

def justify(a, invalid_val=0, axis=1, side='left'):    
    """
    Justifies a 2D array

    Parameters
    ----------
    A : ndarray
        Input array to be justified
    axis : int
        Axis along which justification is to be made
    side : str
        Direction of justification. It could be 'left', 'right', 'up', 'down'
        It should be 'left' or 'right' for axis=1 and 'up' or 'down' for axis=0.

    """

    if invalid_val is np.nan:
        mask = ~np.isnan(a)
    else:
        mask = a!=invalid_val
    justified_mask = np.sort(mask,axis=axis)
    if (side=='up') | (side=='left'):
        justified_mask = np.flip(justified_mask,axis=axis)
    out = np.full(a.shape, invalid_val) 
    if axis==1:
        out[justified_mask] = a[mask]
    else:
        out.T[justified_mask.T] = a.T[mask.T]
    return out

data = data.set_index('Ids')\
  .groupby(level=0)\
  .transform(
      justify, invalid_val=np.nan, axis=0, side='up'
  )\
  .dropna(how='all')

data.to_csv("C:\\Users\\span\\Desktop\\Heather Misc\\PASAT\\PASAT Only Behavioral Data Summary PROCESSED.csv")