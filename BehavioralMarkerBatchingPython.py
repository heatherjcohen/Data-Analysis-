import csv
import pandas as pd
import numpy as np
import glob, os
#Batching code
myDir = 'C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo\\'
os.chdir(myDir)
myFiles = glob.glob('output*')
for file in myFiles:
    print('Now reading ', file)
    df= pd.read_csv(file, header=None).transpose()
    df = df[df[1] != 0]
    df.columns =['Delete','Markers']
    df.index.name ='Relative Time'
    del df['Delete']
    outFile = 'processed' +file
    df.to_csv(outFile)