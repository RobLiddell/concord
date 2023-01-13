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
    select(rowID,vis_id,all_of(variableName))
    
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


cutData <- function(graphicData,dataTypeList,graphicStyle,graphic){
  
  if(graphicStyle!='default'){
    return(graphicData)
  }else if(graphic == 'table'){
    return(graphicData)
  }
  
  cutColumn <- function(graphicData,variableName,dataType){
    
    if(dataType%in%c('$','01','b','b0')){
      return()
    }
    
    nGroups <- graphicData %>% 
      distinct(across(all_of(variableName))) %>% 
      pull(variableName) %>% 
      length()
    
    if(nGroups<=10){
      
      graphicData <- graphicData %>% 
        mutate({{variableName}}:=factor(.data[[variableName]]) %>% fct_rev())
      graphicData[[variableName]] <<- graphicData[[variableName]]
      return()
    }
    
    if(dataType=='dt'){
      graphicData <- graphicData %>%
        mutate({{variableName}}:=as.numeric(.data[[variableName]]))
    }
    
    graphicData <- graphicData %>% 
      mutate({{variableName}}:=cut(.data[[variableName]],10),
             {{variableName}}:=fct_rev(.data[[variableName]]))
    
    if(dataType=='dt'){
      levels(graphicData[[variableName]]) <- levels(graphicData[[variableName]]) %>% 
        str_replace('(?<=\\().*?(?=,)',stringNumToDate) %>% 
        str_replace('(?<=,).*?(?=\\])',stringNumToDate) %>% 
        str_replace('[0-9]{5}',stringNumToDate)
    }
    levels(graphicData[[variableName]]) <- levels(graphicData[[variableName]]) %>% 
      str_remove_all('[\\(\\]]') %>% 
      str_replace(',','\n') 
    
    graphicData[[variableName]] <<- graphicData[[variableName]]
    #assign('test',test,envir=.GlobalEnv) may want to implement this to be more specific about assignment environment
    
  }
  
  tibble(dataType=dataTypeList,dataColumns=colnames(graphicData)[3:ncol(graphicData)])%$%
    walk2(dataColumns,dataType,~cutColumn(graphicData,.x,.y))
  
  return(graphicData)
  
  
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
    left_join(dataDict %>% select(varnm,rep,source,expandedVarnm,myLabel,shortcats,tp)) %>% 
    mutate(subsection=if_else(subsection==varnm,myLabel,subsection)) %>% 
    select(-myLabel) %>% 
    mutate(dataRows=if_else(is.na(rep),'',source),
           varnm=expandedVarnm) %>% 
    select(-rep,-source,-expandedVarnm) %>% 
    unnest_longer(varnm) %>% 
    group_by(section,subsection) %>% 
    summarise(ord=first(ord),
              varnm=list(varnm),
              dataRows=first(dataRows),
              tp=list(tp),
              shortcats=list(shortcats),
              graphicStyle=first(graphicStyle),
              graphic=first(graphic),
              .groups = 'drop') %>% 
    arrange(ord) %>% 
    relocate(ord,.before = 1) %>% 
    mutate(graphicData=map2(varnm,dataRows,~getData(data,.x,.y))) %>% 
    mutate(graphicData=pmap(list(graphicData,tp,graphicStyle,graphic),~cutData(..1,..2,..3,..4))) 
  
  
  
  plotGen <- function(graphicData,graphicStyle,tp){
    
    p <- switch(graphicStyle,
                default=plotGenDefault(graphicData,tp))
    
    p <- p +
      scale_fill_grey(guide='none')+
      coord_flip()
    
    return(p)
  }
  
  plotGenDefault <- function(graphicData,tp){
    
    pivotData <- graphicData %>% 
      pivot_longer(cols=-c(1,2))
    
    if(tp%in%c('01','b','$')){
      levels(pivotData$value) <- levels(pivotData$value) %>% 
        str_wrap(11)
    }
    
    plotData <- pivotData %>% 
      group_by(name) %>% 
      count(value) %>% 
      ungroup() 
    
    graphLabels <- plotData %>% 
      filter(n<=5) %>%
      mutate(grouped=interaction(name,value)) %>% 
      pull(grouped) %>% 
      {filter(pivotData,interaction(name,value)%in%.)} %>% 
      group_by(name,value) %>% 
      summarise(vis_id=str_flatten(vis_id, ', '),.groups='drop')
    
    
    p <- plotData %>% 
      ggplot(aes(x=value,fill=value,y=n)) +
      geom_col()+
      geom_text(graphLabels,mapping=aes(y=5,x=value,label=vis_id),
                hjust = 0)+
      facet_grid(rows=vars(name),scales = 'free_y')+
      labs(y='Count',x=NULL)
    
    if(tp=='01'){
      p <- p + scale_x_discrete(drop=TRUE)
    }else{
      p <- p + scale_x_discrete(drop=FALSE)
    }
    
    return(p)
  }
  
  tableGen <- function(graphicData,graphicStyle,tp){
    flxTbl <- switch(graphicStyle,
                     default=tableGenDefault(graphicData,tp))
    
    flxTbl %>% 
      bg(i=seq(1,nrow(summaryData),2),bg='grey95')
  }
  
  tableGenDefault <- function(graphicData,tp){
    
    columnNames <- colnames(graphicData)[-c(1,2)]
    
    tableData <- graphicData %>% 
      mutate(vis_id=as.numeric(vis_id),
             across(all_of(columnNames),as.character)) %>% 
      mutate(across(all_of(columnNames),~str_replace_all(.x,'(?<=[0-9])\n(?=[0-9])','->'))) %>% 
      mutate(across(all_of(columnNames),~str_replace_all(.x,'\n',' '))) %>% 
      group_by(across(-c(1,2))) %>% 
      summarise(firstID=first(vis_id),
                vis_id=str_flatten(vis_id,', ')) %>% 
      relocate(vis_id,.before=1) %>% 
      arrange(firstID) %>% 
      select(-firstID)
    
    flxTbl <- tableData %>% 
      flextable() %>% 
      width(c(1,2),c(2,6)) 
    
    return(flxTbl)
    
  }
  figureGen <- function(graphicData,graphicStyle,graphic,dataType){
    figure <- switch(graphic,
           plot=plotGen(graphicData,graphicStyle,dataType),
           table=tableGen(graphicData,graphicStyle,dataType))
    return(figure)
  }
  
  dataSummaryOutline %>% 
    filter(graphic=='plot') %>%
    filter(ord%in%c(87)) %$% 
    # filter(ord%in%c(3:10,87)) %$% 
    pmap(list(graphicData,graphicStyle,graphic,dataType),~figureGen(..1,..2,..3,..4))

    
  
  
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
  



}





