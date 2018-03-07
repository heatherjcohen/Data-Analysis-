#PASAT
#21 = right
#20 = stimulus presented
#2= button pressed/response recorded
#22= missed
#23 = incor
#

#PVT
# 1= Start task block*
# 2 = Cue/Zeros onset
# 3 = Hit
# 4 = Miss (from previous trial, when nothing pressed, so often occurs just before code 2)
# 5 = Wrong (occurs as many times as they press)
#PVTcolumns=['Trial_Count', 'Response_Count','Missed', 'Correct', 'Wrong']
#2, 3 + 5, 4, 3, 5, 

import csv
import pandas as pd
import numpy as np
import glob, os
import re
#

columns=['Stim_Count', 'Response_Count','Missed', 'Correct', 'Wrong']
PASATdfs=[]


def PASATscore(pdframe):
	Taskname ='PASAT_'
	ID=(re.findall('\\d+', file))
	dfName = 'Scoringdf_'+str(ID)
	dfName = pd.DataFrame([[0,0,0,0,0]],columns=columns, index=ID)
	for index, row in pdframe.iterrows():
#		dfName.ID = ID
#		dfName.set_index('ID')
		if row[1] == '21':
			dfName.Correct[0]+=1
		if row[1] == '20':
			dfName.Stim_Count[0]+=1
		if row[1] == '2':
			dfName.Response_Count[0]+=1
		if row[1] == '22':
			dfName.Missed[0]+=1
		if row[1] == '23':
			dfName.Wrong[0]+=1
	if 'post' in file:
		print('Looks like a Post')
		PrePost = 'Post_'
		dfName.columns = [Taskname+ PrePost +x for x in columns]
	elif'pre' in file: 
		print('Looks like a PRE')
		PrePost = 'Pre_'
		dfName.columns = [Taskname+ PrePost +x for x in columns]
	PASATdfs.append(dfName)
	#print(dfName)

PVTdfs=[]

def PVTscore(pdframe):
	Taskname ='PVT_'
	ID=(re.findall('\\d+', file))
	dfName = 'Scoringdf_'+str(ID)
	dfName = pd.DataFrame([[0,0,0,0,0]],columns=columns, index=ID)
	for index, row in pdframe.iterrows():
		if row[1] == '3':
			dfName.Correct[0]+=1
		if row[1] == '2':
			dfName.Stim_Count[0]+=1
		if row[1] == '3' or '5':
			dfName.Response_Count[0]+=1
		if row[1] == '4':
			dfName.Missed[0]+=1
		if row[1] == '5':
			dfName.Wrong[0]+=1
	if 'post' in file:
		print('Looks like a Post')
		PrePost = 'Post_'
		dfName.columns = [Taskname+ PrePost +x for x in columns]
	elif'pre' in file: 
		print('Looks like a PRE')
		PrePost = 'Pre_'
		dfName.columns = [Taskname+ PrePost +x for x in columns]
	PVTdfs.append(dfName)
	#print(dfName)

def Score(csvfile):
	csvfile=csvfile.lower()
	print('Now reading', csvfile)
	df= pd.read_csv(csvfile, header=None)
	if 'pasat' in csvfile:
		print('Looks like a PASAT')
		PASATscore(df)
	elif'pvt' in csvfile: 
		print('Looks like a PVT')
		PVTscore(df) 
	else:
		print('Something weird happened with ', csvfile )
	return;

myDir = 'C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo\\'
os.chdir(myDir)
myFiles = glob.glob('*processed*')
for file in myFiles:
	file=file.lower()
	Score(file)


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

if PASATdfs:
	results1 = pd.concat(PASATdfs)
	results1.index.name = 'Ids'
	results1['ID'] = results1.index
if PVTdfs:
	results2 = pd.concat(PVTdfs)
	results2.index.name = 'Ids'
	results2['ID'] = results2.index
#If they both have data 
if PASATdfs and PVTdfs:
	listofresults =[results1,results2]
	results3 = pd.merge(results1, results2, on="ID", how="outer", left_index=True, right_index=True)
	results3 = results3[['ID'] + results3.columns[:-1].tolist()]
	#print(results3)
	results3.to_csv('Behavioral Data Summary.csv')
#if there are just PASATs
elif PASATdfs and not PVTdfs:
	#print(results1)
	results1.to_csv('PASAT Only Behavioral Data Summary.csv')
#if there are just PVTs
elif  PVTdfs and not PASATdfs:
	#print(results2)
	results2.to_csv('PVT Only Behavioral Data Summary.csv')
else:
	print("Something super weird happened, check BatchingProc.py")
