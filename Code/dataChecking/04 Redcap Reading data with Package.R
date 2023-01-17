library(REDCapDataSummary)

dataDict <- readDataDictionary('Data/meta/04 CONCORD REDCap for data management2021-11-26.csv')
longerDataDict <- expandDataDictionary(dataDict)
concordData <- readREDCapData('Data/raw/01 ConcordanceStudy_DATA_2022-12-30_1413.csv',longerDataDict)
