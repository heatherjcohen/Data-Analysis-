#PVT Batching 
#            Stim1_Outcome Stim1_RT Stim2_Outcome Stim2_RT Stim3_Outcome...
#Subject1
#Subject2
#Subject3
##
##
# Outcome = Marker code
#RT = time between '2' and first response marker after it


import csv
import pandas as pd
import numpy as np
import glob, os
import re

###### REQUIRES making sure that there are only the BLOCK ONE ONLY files - manually deleted the redundant full files so only the block ones remain
######


ListofLists=[]
names = ["Stim_Output", "Stim_RT"]

myDir = 'C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo\\'
os.chdir(myDir)
myFiles = glob.glob('*TimeMarkerProcessedprocessedoutput*')
for file in myFiles:
    listylist=[]
    file=file.lower()
    print('Working on', file)
    ID=(re.findall('\\d+', file)) ###stuck on prepost id and concat
    df= pd.read_csv(file)
    #Create new df for scoring
    #Scoring file
    df["ReactionTime"]=df['ReactionTime'].shift(2)
    twos_list=df.index[df['Markers']==2].tolist()
    after_twos_list = [x+1 for x in twos_list]
    after_twos_list= [s for s in after_twos_list if s <len(df)]
    df=df.iloc[after_twos_list,:]
    for index, row in df.iterrows():
        listylist.append(row['Markers'])
        listylist.append(row['ReactionTime'])
    df = pd.DataFrame([listylist], index=ID)
    number = len(df.columns)/2
    if 'post' in file:
        print('Looks like a Post')
        PrePost = 'Post_'
    elif'pre' in file: 
        print('Looks like a PRE')
        PrePost = 'Pre_'
    columns=[(PrePost+"Stim" +str(x) +"_Outcome" ,PrePost+"Stim" +str(x)+"_RT") for x in range(int(number))]
    flat_list = [item for sublist in columns for item in sublist]
    df.columns = flat_list
    ListofLists.append(df)



results1 = pd.concat(ListofLists)
results1['ID'] = results1.index
results1.to_csv('PVT Reaction Time Data.csv')