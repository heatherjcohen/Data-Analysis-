

####### Next up: Make Scoringdf create a new line for every new ID -- right now ID is always the same
#Since Index=ID is defined out of the loop

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


import csv
import pandas as pd
import numpy as np
import glob, os

#


#Functions
def Score(csvfile):
	print('Now reading %s\n', csvfile)
	csvfile.lower()
	df= pd.read_csv(csvfile, header=None)
	if glob.glob('*pasat*'):
		print('Looks like a PASAT')
		PASATscore(df)
	elif glob.glob('*pvt*'): 
		print('Looks like a PVT')
		PVTscore(df) 
	else:
		print('Something weird happened with %s\n', csvfile )
	return;

columns=['Trial Count', 'Response Count','Missed', 'Correct', 'Wrong']
Scoringdf = pd.DataFrame([[0,0,0,0,0]],columns=columns, index=ID)

def PASATscore(pdframe):
	Taskname ='PASAT_'
	for index, row in pdframe.iterrows():
		if row[1] == 21:
			Scoringdf.loc[ID,'Correct']+=1
		if row[1] == 20:
			Scoringdf.loc[ID,'Trial Count']+=1
		if row[1] == 2:
			Scoringdf.loc[ID,'Response Count']+=1
		if row[1] == 22:
			Scoringdf.loc[ID,'Missed']+=1
		if row[1] == 23:
			Scoringdf.loc[ID,'Wrong']+=1
	if glob.glob('*pre*'):
		PrePost = 'Pre_'
		Scoringdf.columns = [Taskname+ PrePost +x for x in columns]
	if glob.glob('*post*'):
		PrePost = 'Post_'
		Scoringdf.columns = [Taskname+ PrePost +x for x in columns]
	print(Scoringdf)


#Batching code
myDir = 'C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\'
os.chdir(myDir)
myFiles = glob.glob('*processed*')
for x in myFiles:
	ID = (re.findall('\\d+', x))
	Score(x)



