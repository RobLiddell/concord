library(tidyverse)
library(lubridate)
library(stringr)
library(openxlsx2)
library(flextable)



####DATA CHECKING STEPS####
#1.get variable
#2.Identify type of information it is supposed to contain
#3.Check if it has this type of information
#4.Record if there is an error an what type of error

####Data Loading####
#load the ConcordanceStudy Data

##Source the second file to load data
dataLoc <- 'Data/raw'

source("Data/raw/02 ConcordanceStudy_R_2022-12-30_1413.r") 
data <- data %>% 
  tibble() %>% 
  mutate(ord=row_number(),.before = vis_id)


#ConcordanceStudy Data Dictionary
dataDictLoc <- 'Data/meta/02 CONCORD REDCap for data management2021-11-26.xlsx'

dataDict <- read_xlsx(dataDictLoc,
          sheet=2,
          skipEmptyCols = T) %>% 
  tibble() %>% 
  slice(-1)

currentDate=ymd('2021-11-26')

#ConcordanceStudy Summary Graphic Type
dataGraphicTypeLoc <- 'Data/meta/03 CONCORD REDCap Summary Type.xlsx'

dataDict <- read_xlsx(dataGraphicTypeLoc,
                         sheet=1,
                         skipEmptyRows = TRUE,
                         skipEmptyCols = TRUE) %>% 
  tibble() %>% 
  {left_join(dataDict,.)}

####Helper Functions####

#convert a string Number into a date
stringNumToDate <- function(string){
  dateString <- string %>% 
    as.integer() %>% 
    as.Date(origin="1970-01-01") %>% 
    as.character()
  return(dateString)
}

formatDataLabels <- function(values){
  values %>% 
    str_remove_all('\"') %>% 
    str_split_1(',') %>% 
    str_wrap(11) %>% 
    return()
}

plotSummarizer <- function(data,variableLabel,graphLabels){


  dataBreaks <- data$value %>% 
    levels()
  
  p <- data %>% 
    ggplot(aes(y=value,fill=value)) +
    scale_fill_grey(guide='none')+
    scale_y_discrete(drop=FALSE)+
    geom_bar()+
    facet_grid(rows=vars(name))+
    labs(x='Count', y=variableLabel)

  
  graphLabels <- graphLabels %>% 
    group_by(name,value) %>% 
    summarise(vis_id=list(vis_id),.groups='drop') %>% 
    rowwise() %>% 
    mutate(vis_id=paste0(vis_id,collapse=', ')) %>% 
    ungroup()
  
  
  p <- p+
    geom_text(graphLabels,mapping=aes(x=5,y=value,label=vis_id),
              hjust = 0)
  
  
  return(p)
}

tableSummarizer <- function(idValues,variableLabel){
  idValues %>% 
    select(vis_id,value) %>% 
    mutate(vis_id=as.character(vis_id),
           value=str_replace_all(value,'\n','->')) %>%
    group_by(value) %>% 
    summarise(vis_id=vis_id %>% str_flatten(collapse=', ') ) %>% 
    select(vis_id,value) %>% 
    arrange(value) %>% 
    flextable() %>% 
    width(2,4.5) %>% 
    width(1,2) %>% 
    add_header_row(values=variableLabel,colwidths = 2)
}


####getData Function####

#getData uses variableName, instrument label, dataType, and values to find and 
#return the appropriate data from 'data'
#Return data for each variable along with row number and vis id
getData <- function(data,variableName,instrumentLabel,dataType,values){
  
  if(dataType=='01'){
    
    checkBoxVariables <- paste0(variableName,'___',values)

    returnDat <- data %>%
      filter(redcap_repeat_instrument==instrumentLabel) %>%
      select(ord,vis_id,all_of(checkBoxVariables)) %>% 
      mutate(across(all_of(checkBoxVariables),~as.numeric(.x))) %>% 
      pivot_longer(cols=all_of(checkBoxVariables))
    
  }else{
    returnDat <- filter(data,redcap_repeat_instrument==instrumentLabel) %>% 
      select(ord,vis_id,all_of(variableName)) %>% 
      pivot_longer(cols=all_of(variableName))
  }
  
  return(returnDat)
}

####Summary Data Formatting####

formatSummaryData <- function(data,dataType,variableName,variableValues,dataLabels){
  
  if(dataType == '01'){
    
    checkBoxVariables <- paste0(variableName,'___',variableValues)
    
    data <- data %>% 
      mutate(name = factor(name,levels=checkBoxVariables,labels = dataLabels %>% str_wrap(11)))
  }
  
  
  data$value <- switch (dataType,
    `$` = data$value %>% as.character() %>% str_wrap(80) %>% factor(),
    `01` = data$value %>% factor(levels=c(1,0)),
    `##` = data$value %>% as.numeric(),
    `#` = data$value %>%  as.integer(),
    `dt` = data$value %>% ymd() %>% as.integer(),
    `b` = data$value %>%  factor(levels=variableValues,labels = dataLabels) %>% str_wrap(11) %>% fct_rev(),
    `b0` = data$value %>% factor(levels=variableValues,labels = variableValues) %>% fct_rev()
    )
  
  return(data)
}

####Cutting up Continuous Data####


  



dataCutting <- function(data,dataType){
  
  if(dataType%in%c('$','01','b','b0')){
    return(data)
  }
  
  nGroups <- data %>% 
    distinct(value) %>% 
    pull(value) %>% 
    length()

  if(nGroups<=11){
    
    data <- data %>% 
      mutate(value=factor(value) %>% fct_rev())
    
    if(dataType=='dt'){
      levels(data$value) <- levels(data$value) %>% 
        str_replace('(?<=\\().*?(?=,)',stringNumToDate) %>% 
        str_replace('(?<=,).*?(?=\\])',stringNumToDate) %>% 
        str_replace('[0-9]{5}',stringNumToDate)
    }
    
    return(data)
  }
  
  data <- data %>% 
    mutate(value=cut(value,11),
           value=fct_rev(value))
  
  if(dataType=='dt'){
    levels(data$value) <- levels(data$value) %>% 
      str_replace('(?<=\\().*?(?=,)',stringNumToDate) %>% 
      str_replace('(?<=,).*?(?=\\])',stringNumToDate) %>% 
      str_replace('[0-9]{5}',stringNumToDate)
  }
  levels(data$value) <- levels(data$value) %>% 
    str_remove_all('[\\(\\]]') %>% 
    str_replace(',','\n') 
  
  return(data)
  
}


#variableSummarizer takes data and returns an appropriate visual summary 
#based on the data type
variableSummarizer <- function(data, variableLabel){

  lowCounts <- data %>% 
    group_by(name) %>% 
    count(value) %>% 
    filter(n<=5) %>% 
    select(name,value)
  
  idsToAdd <- data %>% 
    filter(interaction(name,value)%in%interaction(lowCounts$name,lowCounts$value)) %>% 
    select(name,value,ord,vis_id)
  
  plotType='default'
  tableType='default'
  
  figurePlot <- switch(plotType,
         default=plotSummarizer(data,variableLabel,idsToAdd))
  
  
  figureTable <- switch(tableType,
                        default=tableSummarizer(idsToAdd,variableLabel))


  tibble(summaryPlot=list(figurePlot),summaryTable=list(figureTable),lowCountIds=list(idsToAdd)) %>% 
  return()
}


####Vars To Check####
variableNumbers <- c(3:10,24,25:26,30,59,60:70,87:90,91:101,113:143,169:202)

####Testing and Output####


dataSummary <- dataDict %>% 
  slice(variableNumbers) %>%
  mutate(rep=if_else(is.na(rep),'',source),
         values=str_remove_all(values,'\"') %>% str_split(','),
         shortcats=str_remove_all(shortcats,'\"') %>% str_split(',')) %>% 
  mutate(dat=pmap(list(varnm,rep,tp,values),
                  ~getData(data=data,variableName=..1,instrumentLabel=..2,dataType=..3,values=..4)))%>% 
  mutate(dat=pmap(list(dat,tp,varnm,values,shortcats),
                  ~formatSummaryData(data=..1,dataType = ..2,variableName = ..3, variableValues = ..4,dataLabels = ..5))) %>% 
  mutate(dat=pmap(list(dat,tp),
                  ~dataCutting(..1,..2))) %>% 
  mutate(summaryOutput=pmap(list(dat,myLabel),
                            ~variableSummarizer(..1,..2)))
  
#summaryOutput
#summaryPlot
#summaryTable


dataDict %>% 
  filter(source == 'tumor_char_detail') %>% 
  slice(1) %>% 
  pull(rep) %>% 
  {!is.na(.)}