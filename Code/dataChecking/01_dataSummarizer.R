library(tidyverse)
library(lubridate)
library(stringr)
library(openxlsx2)


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

####getData Function####

#getData uses variableName, instrument label, dataType, and values to find and 
#return the appropriate data from 'data'
#Return data for each variable along with row number and vis id
getData <- function(data,variableName,instrumentLabel,dataType,values){
  
  if(dataType=='01'){
    
    checkBoxVariables <- str_split_1(values,',') %>%
      {paste0(variableName,'___',.)}

    returnDat <- data %>%
      filter(redcap_repeat_instrument==instrumentLabel) %>%
      select(ord,vis_id,all_of(checkBoxVariables))
    
    return(returnDat)
    
  }
  
  returnDat <- filter(data,redcap_repeat_instrument==instrumentLabel) %>% 
    select(ord,vis_id,all_of(variableName))
  
  return(returnDat)
}

####Summary Data Formatting####

formatSummaryData <- function(data,dataType,columns,variableValues,dataLabels){
  #Change data type to match data
  if(dataType %in% c('$')){
    data <- data %>% 
      mutate(across(all_of(columns),~as.character(.x))) %>% 
      pivot_longer(cols=all_of(columns))
    
  }else if(dataType %in% c('01')){
    data <- data %>% 
      mutate(across(all_of(columns),~as.numeric(.x))) %>% 
      pivot_longer(cols=all_of(columns)) %>% 
      mutate(name = factor(name,levels=columns,labels = dataLabels))
    
    
  }else if(dataType %in% c('##')){
    data <- data %>% 
      mutate(across(all_of(columns),~as.numeric(.x))) %>% 
      pivot_longer(cols=all_of(columns))
    
  }else if(dataType %in% c('#')){
    data <- data %>% 
      mutate(across(all_of(columns),~as.integer(.x))) %>% 
      pivot_longer(cols=all_of(columns))
    
  }else if(dataType %in% c('b0')){
    data <- data %>% 
      mutate(across(all_of(columns),~factor(.x,levels=variableValues,labels = dataLabels))) %>% 
      pivot_longer(cols=all_of(columns))
    
  }else if(dataType %in% c('b')){
    data <- data %>% 
      mutate(across(all_of(columns),
                    ~factor(.x,levels=variableValues,labels = dataLabels))) %>% 
      pivot_longer(cols=all_of(columns))

  }
  else if(dataType %in% c('dt')){
    data <- data %>% 
      mutate(across(all_of(columns),~ymd(.x))) %>% 
      pivot_longer(cols=all_of(columns)) %>% 
      mutate(value=as.numeric(value))
  }
  return(data)
}




#variableSummariesPlots takes data and returns an appropriate visual summary 
#based on the data type
variableSummaryPlot <- function(data, dataType, variableLabel,variableValues,shortcats,longcats){
  
  #data should always have ord and vis_id
  columns <- data %>% #columns will include the names for the other variables
    colnames() %>% 
    .[-(1:2)]
  
  
  #Format data Labels
  if(dataType%in%c('b','b0','01')){
    variableValues <- variableValues %>% 
      formatDataLabels()
    
    dataLabels <- switch(dataType,
           b=formatDataLabels(shortcats),
           `01`=formatDataLabels(shortcats),
           b0=variableValues)
  }
  
  data <- data %>% 
    formatSummaryData(dataType,columns,variableValues, dataLabels)

  #Arrange data in correct order for plotting
  numberOfBins <- data %>% 
    distinct(value) %>% 
    nrow() 
  
  if(numberOfBins>15 & !dataType%in%c('$')){
    numberOfBins=15
    data <- data %>% 
      mutate(value=cut(value,numberOfBins),
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
      # {if_else(str_detect(.,'\n'),.,str_wrap(.,20))}
    
    
  }else{
    data <- data %>% 
      mutate(value=factor(value),
             value=fct_rev(value))
    if(dataType=='dt'){
      levels(data$value) <- levels(data$value) %>% 
        str_replace('[0-9]{5}',stringNumToDate)
    }
    # levels(data$value) <- levels(data$value) %>% 
    #   {if_else(str_detect(.,'\n'),.,str_wrap(.,20))}
      
    
  }

  
  lowCounts <- data %>% 
    group_by(name) %>% 
    count(value) %>% 
    filter(n<=5) %>% 
    select(name,value)
  
  idsToAdd <- data %>% 
    filter(interaction(name,value)%in%interaction(lowCounts$name,lowCounts$value)) %>% 
    select(name,value,ord,vis_id)


  p <- data %>% 
    ggplot(aes(y=value,fill=value)) +
    scale_fill_grey(guide='none')+
    geom_bar()+
    facet_grid(rows=vars(name))+
    labs(x='Count', y=variableLabel)
  
  tibble(plot=list(p),lowCountIds=list(idsToAdd)) %>% 
  return()
}




dataTable <- function(variable,dataDict){
  dataDict %>% 
    filter(varnm %in% variable) %>% 
    mutate(rep=if_else(is.na(rep),'',source)) %>% 
    pull(rep) %>% 
    return()
}


dateCheck <- function(value,after=NULL,before=NULL){
  dates <- value %>% 
    ymd() %>%
    tibble() %>% 
    mutate(check=TRUE)
  
  if(!is.null(after)){
    dates <- dates %>%
      mutate(check=check&.>{{after}}) 
  }
  
  if(!is.null(before)){
    dates <- dates %>%
      mutate(check=check&.<{{before}})
  }
  
  dates %>% 
    pull(check) %>%
    return()
}

defaultChecker <- function(value){
  return(FALSE)
}

checkerChooser <-  function(variableType){
  switch(variableType,
         dt=dateCheck) %>% 
    return()
}


####Vars To Check####
variableNumbers <- c(3:10,24,25:26,30,59,60:70,88:90,91:101,113:143,169:202)

####Testing and Output####

variableInfo <- dataDict %>% 
  slice(87) %>% 
  # slice(variableNumbers) %>%
  mutate(rep=if_else(is.na(rep),'',source)) %>% 
  mutate(dat=pmap(list(varnm,rep,tp,values),~getData(data=data,variableName=..1,instrumentLabel=..2,dataType=..3,values=..4)))


dataSummary <- variableInfo %>% 
  mutate(summaryOutput=pmap(list(dat,tp,`myLabel `,values,shortcats,longcats),~variableSummaryPlot(..1,..2,..3,..4,..5,..6)))


dataSummary %>% 
  unnest(summaryOutput) %>% 
  select(plot) %>% 
  .[[1]]
