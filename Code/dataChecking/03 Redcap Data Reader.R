#Clear existing data and graphics
rm(list=ls())
graphics.off()

library(tidyverse)
library(magrittr)

#Read Data Dictionary####
dataDict=read_csv('Data/meta/04 CONCORD REDCap for data management2021-11-26.csv') %>% 
  mutate(values=str_remove_all(values,'\"') %>% str_split(','),
         shortcats=str_remove_all(shortcats,'\"') %>% str_split(',')) %>%
  mutate(tp=if_else(tp=='1','01',tp),
         expandedVarnm=if_else(tp=='01',map2(varnm,values,function(.x,.y) (paste0(.x,'___',.y))),map(varnm,function(.x) (.x)))) 

#Subset dataDict creating variables for '01' data types
allVariableInfo <- dataDict %>% 
  mutate(varnm=expandedVarnm) %>% 
  select(varnm,tp,values,shortcats,source,myLabel) %>% 
  unnest_longer(varnm)

#Function for creating variable type list for redcap data
createColList <- function(dataType,variableValues){
    switch (dataType,
          `$` = col_character(),
          `01` = 'i',
          `##` = 'n',
          `#` = 'i',
          `dt` = col_character(),
          `b` = col_factor(levels = variableValues),
          `b0` = col_factor(levels = variableValues))
}

#Creating column types list
columnTypes <- allVariableInfo %>% 
  select(values,tp) %$%
  map2(tp,values,~createColList(.x,.y)) %>% 
  setNames(allVariableInfo$varnm) %>% 
  append(list(redcap_repeat_instrument=col_character(),redcap_repeat_instance='i'),after=0)

#Extracting names to read in
colNames <- columnTypes %>% 
  names()

#Read Data####
data=vroom('Data/raw/01 ConcordanceStudy_DATA_2022-12-30_1413.csv',delim=',',col_types=columnTypes,col_select=all_of(colNames)) %>% 
  mutate(rowID=row_number(),.before=1) %>% 
  mutate(vis_id=str_remove(vis_id,'^[0]*'))

#Format Months Properly
data <- allVariableInfo %>% 
  filter(tp=='dt') %$% 
  mutate(data,across(.col=all_of(varnm),ymd))


#Formatting CheckBoxVariables
checkBoxFactorer <- function(data,variableName,variableValues,variableLabels){
  
  variableValues <- variableValues %>% 
    append(0)
  variableLabels <- variableLabels %>% 
    append(0) 
  
  #assign('test',test,envir=.GlobalEnv) may want to impliment this to be more specific
  
  data<<-data %>%mutate({{variableName}}:=.data[[variableName]]*as.integer(str_extract(variableName,'(?<=_)[0-9]+$')),
                        {{variableName}}:=factor(.data[[variableName]],
                                                 levels=variableValues,
                                                 labels=variableLabels))
}

allVariableInfo %>% 
  filter(tp=='01') %$% 
  pwalk(list(varnm,values,shortcats),~checkBoxFactorer(data,..1,..2,..3))

#Formatting b variables
radioShortCatsFactorer <- function(data,variableName,variableValues,variableLabels){
  
  data<<-data %>%mutate({{variableName}}:=factor(.data[[variableName]],
                                                 levels=variableValues,
                                                 labels=variableLabels) %>% fct_rev())
}

allVariableInfo %>% 
  filter(tp=='b') %$% 
  pwalk(list(varnm,values,shortcats),~radioShortCatsFactorer(data,..1,..2,..3))

#Formatting b0 Variables
radioBasicFactorer <- function(data,variableName,variableValues,variableLabels){
  
  data<<-data %>%mutate({{variableName}}:=factor(.data[[variableName]],
                                                 levels=variableValues,
                                                 labels=variableValues) %>% fct_rev())
}

allVariableInfo %>% 
  filter(tp=='b0') %$% 
  pwalk(list(varnm,values,shortcats),~radioBasicFactorer(data,..1,..2,..3))



