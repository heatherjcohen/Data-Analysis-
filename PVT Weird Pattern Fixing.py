import csv
import pandas as pd
import numpy as np
import glob, os
import re

###### Purpose of script: to find the incidence and count of 2-2, 3-3, and 4-4 as they indicate something wonky is happening 
#

columns=['TwoTwos', 'TwoThrees','TwoFours']

PVTdfs=[]

def PVTscore(pdframe):
    Taskname ='PVT_'
    ID=(re.findall('\\d+', file))
    dfName = 'Scoringdf_'+str(ID)
    dfName = pd.DataFrame([[0,0,0]],columns=columns, index=ID)
    pdframe['ShiftedMarkers'] = pdframe.Markers.shift()
    for index, row in pdframe.iterrows():
        if row[1] == row[2]:
            if row[1] ==2:
                print("looks like two twos")
                dfName.TwoTwos[0]+=1
            elif row[1]==3:
                print("looks like two threes")
                dfName.TwoThrees[0]+=1
            elif row[1]==4:
                print("looks like two fours")
                dfName.TwoFours[0]+=1
            else: 
                print("Something weird happened.")
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
    df= pd.read_csv(csvfile, header=0)
    if 'pasat' in csvfile:
        print('Looks like a PASAT')
        PASATscore(df)
    elif'pvt' in csvfile: 
        print('Looks like a PVT')
        PVTscore(df) 
    else:
        print('Something weird happened with ', csvfile )
    return;

myDir = 'C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo'####UPDATE

os.chdir(myDir)
myFiles = glob.glob('processed*')
for file in myFiles:
    file=file.lower()
    Score(file)






results2 = pd.concat(PVTdfs)
results2.index.name = 'Ids'
results2['ID'] = results2.index
results2.to_csv('PVT Weird Pattern Spotting.csv')

