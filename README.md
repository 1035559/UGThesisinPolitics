# UGThesisinPolitics

##Speeches Analysis
### Recreating the Speeches analysis will also require you to download the .pdf files for the Hansard transcripts of New Zealand's 44th and 45th Parliaments.
### Links are found here: https://www.parliament.nz/en/pb/hansard-debates/historical-hansard
### Stored in Vols 539 - 580.
### Any time the code requires an input (e.g. the .pdf files above, the .csv files output by the code, or the .xlsx conscience vote data), path names will need to be edited in your copy of the code. 
### \UGThesisinPolitics\Speeches Code\1035559 Thesis 44th Parliament Scraper.ipynb
### and
### \UGThesisinPolitics\Speeches Code\1035559 Thesis 45th Parliament Scraper.ipynb
### each have the code to convert those .pdf files into .xmls, and to handle the .pdf and .xml formatting elements to convert the 6 years of transcripts into two dataframes: one for each parliament. 
### The raw data is prepared and categorised in the 'Parliament Scraper.ipynb' files, and the NLP is carried out in the 'Speech Analysis.ipynb' file. 
### The 'Parliament Scraper.ipynb' stage can be optionally skipped by downloading
### \UGThesisinPolitics\OutputData\Output Data Files1.zip
### and
### \UGThesisinPolitics\OutputData\Output Data Files2.zip,
### and unzipping them into a directory. 
### Edit the path names in the 'Speech Analysis.ipynb' code, and run it from that starting point.
### The final stage of the analysis - including visualisations and regressions - is carried out in R. 
### The pathnames in the R code can be edited with either the '\OutputData\Output Data Files' files provided, or the output of 'Speech Analysis.ipynb'.
### It's worth noting that the machine learning process in 'Speech Analysis.ipynb' is
### The visualisations (including word clouds and box-and-whisker plots included in the thesis) are output by this code, and 

##Conscience Votes Analysis
### Recreating the Conscience Vote analysis involves the 
### \UGThesisinPolitics\InputData\ConscienceVotesDataLegacy.xlsx
### file, which is a reformatted version of
### \UGThesisinPolitics\InputData\Personal Votes since 1990.xlsx
### such that vote results can be interpreted, and the SCRicekj calculations were applied in the excel sheet.
### This is because of the multiple sources involved in compiling the data, and the formatting of the original data. 
### The filepath referenced in the R code will need to be edited, but the code will output the visualisations and regressions of the conscience votes analysis.

##Types of MP Analysis
### The 'types of MP' analysis is contained in the R code for each of the above two analyses as the final portion. 
