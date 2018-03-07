myDir = uigetdir; %gets directory
myFiles = dir(fullfile(myDir,'*.edf'));
for k = 1:length(myFiles)
	baseFileName = myFiles(k).name;
 	fullFileName = fullfile(myDir, baseFileName);
 	fprintf(1, 'Now reading %s\n', fullFileName);
 	[hdr, record] = edfread(fullFileName);
 	lrows =[1,36];
 	m = [record(lrows,:)];
 	s = "output";
 	newStr = erase(baseFileName,'.edf')
	outputfile = strcat('C:\Users\span\Desktop\Heather Misc\ForDevelopingScriptsToExtractBehavioralData\PVTtrytwo', '\', s,newStr,'.csv');
 	csvwrite(outputfile,m);
end





