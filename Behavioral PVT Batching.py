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




ListofLists=[]
names = ["Stim_Output", "Stim_RT"]

myDir = 'C:\\Users\\span\\Desktop\\Heather Misc\\PVT Behavoiral Files' #change this if the files move
os.chdir(myDir)
myFiles = glob.glob('*block1_pvt.txt*') #only use block one 
for file in myFiles:
    listylist=[]
    file=file.lower()
    print('Working on', file)
    ID=(re.findall('\\d+', file)) ###stuck on prepost id and concat
    ID=ID[0] #added because these file names have multiple numbers in them so just grab the first ones
    df= pd.read_csv(file, sep=",")
    #Create new df for scoring
    #Scoring file
    for index, row in df.iterrows():
        listylist.append(row['Acc'])
        listylist.append(row['RT'])
    df = pd.DataFrame([listylist], index=[ID])
    number = len(df.columns)/2
    if 'day1' in file:
        print('Looks like a Pre')
        PrePost = 'Pre_'
    elif'day2' in file: 
        print('Looks like a Post')
        PrePost = 'Post_'
    columns=[(PrePost+"Stim" +str(x) +"_Outcome" ,PrePost+"Stim" +str(x)+"_RT") for x in range(int(number))]
    flat_list = [item for sublist in columns for item in sublist]
    df.columns = flat_list
    ListofLists.append(df)



results1 = pd.concat(ListofLists)
results1['ID'] = results1.index
results1.to_csv('PVT Reaction Time Data FROM BEHAVIORAL.csv')