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

##Sourcing this file provides the data, dataDictionary
##and expanded variable info for the 

source("Code/dataChecking/03 Redcap Data Reader.r") 
data 
dataDict
allVariableInfo
currentDate=ymd('2021-11-26')



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

tableSummarizer <- function(data,variableLabel){
  
  summaryData <- data %>% 
    filter(value!='') %>% 
    group_by(name,value) %>%
    mutate(vis_id=as.numeric(vis_id),
           value=str_replace_all(value,'(?<=[0-9])\n(?=[0-9])','->'),
           value=str_replace_all(value,'\n',' ')) %>%
    arrange(vis_id) %>% 
    mutate(vis_id=as.character(vis_id)) %>% 
    summarise(firstID=as.numeric(first(vis_id)),
              vis_id=str_flatten(vis_id, collapse=', '),.groups = 'drop') %>% 
    arrange(firstID) %>% 
    select(vis_id,value)
    

  # summaryData <- idValues %>% 
  #   select(vis_id,value) %>% 
  #   mutate(vis_id=as.character(vis_id),
  #          value=str_replace_all(value,'\n','->')) %>%
  #   group_by(value) %>% 
  #   summarise(vis_id= str_flatten(vis_id, collapse=', ') ) %>% 
  #   select(vis_id,value) %>% 
  #   arrange(value)
  
  summaryTable <- summaryData %>% 
    flextable() %>% 
    width(2,6) %>% 
    width(1,1) %>% 
    add_header_row(values=variableLabel,colwidths = 2) %>% 
    bg(i=seq(1,nrow(summaryData),2),bg='grey95')
  return(summaryTable)
}


####getData Function####

#getData uses variableName, instrument label, dataType, and values to find and 
#return the appropriate data from 'data'
#Return data for each variable along with row number and vis id
getData <- function(data,variableName,instrumentLabel){
  
  data <- data %>% 
    mutate(redcap_repeat_instrument=if_else(is.na(redcap_repeat_instrument),'',
                                            redcap_repeat_instrument))
  
  returnDat <- data %>%
    filter(redcap_repeat_instrument==instrumentLabel) %>%
    select(rowID,vis_id,all_of(variableName)) %>% 
    pivot_longer(cols=all_of(variableName))
    
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
    `$` = data$value %>% as.character() %>% str_wrap(20) %>% factor(),
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

  if(nGroups<=10){
    
    data <- data %>% 
      mutate(value=factor(value) %>% fct_rev())
    
    # if(dataType=='dt'){
    #   levels(data$value) <- levels(data$value) %>% 
    #     str_replace('(?<=\\().*?(?=,)',stringNumToDate) %>% 
    #     str_replace('(?<=,).*?(?=\\])',stringNumToDate) %>% 
    #     str_replace('[0-9]{5}',stringNumToDate)
    # }
    
    return(data)
  }
  
  if(dataType=='dt'){
    data <- data %>%
      mutate(value=as.numeric(value))
  }
  
  data <- data %>% 
    mutate(value=cut(value,10),
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
                        default=tableSummarizer(data,variableLabel))


  tibble(summaryPlot=list(figurePlot),summaryTable=list(figureTable),lowCountIds=list(idsToAdd)) %>% 
  return()
}




####Testing and Output####

if(FALSE){
  
  #ConcordanceStudy Summary Graphic Type
  dataGraphicTypeLoc <- 'Code/reportRender/dataSummaryReportOutline.csv'
  reportRows <- c(3:10,24,25:26,30,59,60:70,87:90,91:101,113:143,169:202,215:220)
  dataSummaryOutline <- read_csv(dataGraphicTypeLoc) %>% 
    slice(reportRows)
    
  
  
  
  dataSummaryOutline %<>%
    left_join(dataDict %>% select(varnm,rep,source,expandedVarnm)) %>% 
    mutate(dataRows=if_else(is.na(rep),'',source),
           varnm=expandedVarnm) %>% 
    select(-rep,-source,-expandedVarnm) %>% 
    unnest_longer(varnm) %>% 
    group_by(section,subsection) %>% 
    summarise(ord=first(ord),
              varnm=list(varnm),
              dataRows=first(dataRows),
              graphicStyle=first(graphicStyle),
              graphic=first(graphic),
              .groups = 'drop') %>% 
    arrange(ord) %>% 
    relocate(ord,.before = 1)
                       
  dataSummaryOutline %>% 
    slice(1) %>% 
    mutate(graphicData=map2(varnm,dataRows,~getData(data,.x,.y)))
  
  
  
dataSummary <- dataDict %>% 
  mutate(rep=if_else(is.na(rep),'',source)) %>% 
  slice(variableNumbers) %>%
  slice(12) %>%
  mutate(dat=pmap(list(varnm,rep,tp,values),
                  ~getData(data=data,variableName=..1,instrumentLabel=..2,dataType=..3,values=..4))) %>% 
  mutate(dat=pmap(list(dat,tp),
                  ~dataCutting(..1,..2)))


dataSummary %>% 
  # slice(12) %>% 
  pull(dat)

  mutate(dat=pmap(list(dat,tp,varnm,values,shortcats),
                  ~formatSummaryData(data=..1,dataType = ..2,variableName = ..3, variableValues = ..4,dataLabels = ..5))) %>% 
  mutate(dat=pmap(list(dat,tp),
                  ~dataCutting(..1,..2))) %>% 
  mutate(summaryOutput=pmap(list(dat,myLabel),
                            ~variableSummarizer(..1,..2)))
  



}





