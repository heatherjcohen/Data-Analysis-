data = pd.read_csv("C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo\\PVT Reaction Time Data PROCESSED.csv")
data.dropna(subset=['Post_Stim0_Outcome'], inplace=True)
data.to_csv("C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo\\PVT Reaction Time Data PROCESSED.csv")