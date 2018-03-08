import csv
import pandas as pd
import numpy as np
import glob, os
import re
import csv

myDir = 'C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo\\'
os.chdir(myDir)
myFiles = glob.glob('*processedoutput*')
for file in myFiles:
	file=file.lower()
	if 'pasat' in file:
		print('Looks like a PASAT, ignoring', file)
	if 'Behavioral' in file:
		print('Ignoring ', file)
	if 'pvt' in file: 
		print('Looks like a PVT', file)
		df= pd.read_csv(file)
		df['ReactionTime'] = abs(df['Relative Time'] - df['Relative Time'].shift(-1))
		df["ReactionTime"]=df['ReactionTime'].shift(2)
		outFile = 'TimeMarkerProcessed' +file
		df.to_csv(outFile)
	else:
		print('Something weird happened with ', file )