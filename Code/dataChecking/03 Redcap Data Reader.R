#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Read Data
dataDict=read_csv('Data/meta/04 CONCORD REDCap for data management2021-11-26.csv') %>% 
  mutate(values=str_remove_all(values,'\"') %>% str_split(','),
         shortcats=str_remove_all(shortcats,'\"') %>% str_split(',')) %>%
  mutate(tp=if_else(tp=='1','01',tp),
         varnm=if_else(tp=='01',map2(varnm,values,function(.x,.y) (paste0(.x,'___',.y))),map(varnm,function(.x) (.x))))


dataWType <- dataDict %>% 
  select(varnm,tp,values,shortcats) %>% 
  unnest_longer(varnm)

colNames <- dataWType %>% 
  pull(varnm) %>% 
  append(c('redcap_repeat_instrument','redcap_repeat_instance','redcap_data_access_group'),after=0)

colTypeVec <- dataWType %>% 
  mutate(colTypes=factor(tp,
                         levels=c('$','dt','#','##','b0','b','01'),
                         labels=c('c','D','i','n','f','f','f')),
         colTypes=as.character(colTypes)) %>%
  pull(colTypes) %>% 
  append(c('c','i','c'),0) %>% 
  str_flatten()


replaceDataTypeWithCol <- function(dataType,dataValues,dataLabels){
  switch (currentDataType,
          `$` = col_character(),
          `01` =col_factor(levels=c(1,0),labels=c(currentDataLabels,'0')),
          `##` = col_ %>% as.numeric(),
          `#` = currentData %>%  as.integer(),
          `dt` = currentData %>% ymd() %>% as.integer(),
          `b` = currentData %>%  factor(levels=currentDataValues,labels = currentDataLabels) %>% str_wrap(11) %>% fct_rev(),
          `b0` = currentData %>% factor(levels=currentDataValues,labels = currentDataValues) %>% fct_rev())
}

data=read_csv('Data/raw/01 ConcordanceStudy_DATA_2022-12-30_1413.csv',col_select=all_of(colNames))


#col_types = cols(redcap_repeat_instance = col_factor(levels = c("1","2", "3","4")))

shortcatsCheckBoxFormatting <- function(tp,varnm,values,shortcats){
  if(tp=='01'){
    variableValue=str_extract(varnm,'(?<=[_]{3})[0-9]+')
    variableLabelIndex=which(values==variableValue)
    return(shortcats[variableLabelIndex])
  }
  return(shortcats)
}

valueCheckBoxFormatting <- function(tp,varnm,values){
  if(tp=='01'){
    return(str_extract(varnm,'(?<=[_]{3})[0-9]+'))
  }
  return(values)
}


dataTypeChanging <- dataDict %>% 
  select(varnm,tp,values,shortcats) %>% 
  mutate(values=str_remove_all(values,'\"') %>% str_split(','),
         shortcats=str_remove_all(shortcats,'\"') %>% str_split(',')) %>%
  mutate(varnm=if_else(tp=='01',map2(varnm,values,function(.x,.y) (paste0(.x,'___',.y))),map(varnm,function(.x) (.x)))) %>% 
  unnest_longer(varnm) %>% 
  left_join(tibble(varnm=colnames(data)) %>% 
              mutate(row=row_number())) %>%
  # filter(str_detect(varnm,'cl_mets')) %>% 
  mutate(shortcats=pmap(list(tp,varnm,values,shortcats),~shortcatsCheckBoxFormatting(..1,..2,..3,..4)),
         values=pmap(list(tp,varnm,values),~valueCheckBoxFormatting(..1,..2,..3))) 




loadedData <- data %>% 
  select(ord,starts_with('redcap_'))

for(i in 1:nrow(dataTypeChanging)){
  currentVariable=dataTypeChanging$varnm[[i]]
  currentDataType=dataTypeChanging$tp[[i]]
  currentDataValues=dataTypeChanging$values[[i]]
  currentDataLabels=dataTypeChanging$shortcats[[i]]
  
  currentData <- data[[currentVariable]]
  
  
  currentData <- switch (currentDataType,
                         `$` = currentData %>% as.character() %>% str_wrap(20) %>% factor(exclude=c('',NA)),
                         `01` =currentData %>% factor(levels=c(1,0),labels=c(currentDataLabels,'0')),
                         `##` = currentData %>% as.numeric(),
                         `#` = currentData %>%  as.integer(),
                         `dt` = currentData %>% ymd() %>% as.integer(),
                         `b` = currentData %>%  factor(levels=currentDataValues,labels = currentDataLabels) %>% str_wrap(11) %>% fct_rev(),
                         `b0` = currentData %>% factor(levels=currentDataValues,labels = currentDataValues) %>% fct_rev())
  loadedData[[currentVariable]] <- currentData
  
}

loadedData