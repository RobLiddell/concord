#Clear existing data and graphics
rm(list=ls())
graphics.off()

library(tidyverse)
library(magrittr)

#Read Data Dictionary
dataDict=read_csv('Data/meta/04 CONCORD REDCap for data management2021-11-26.csv') %>% 
  mutate(values=str_remove_all(values,'\"') %>% str_split(','),
         shortcats=str_remove_all(shortcats,'\"') %>% str_split(',')) %>%
  mutate(tp=if_else(tp=='1','01',tp),
         varnm=if_else(tp=='01',map2(varnm,values,function(.x,.y) (paste0(.x,'___',.y))),map(varnm,function(.x) (.x))))


allVariableInfo <- dataDict %>% 
  select(varnm,tp,values,shortcats,source,myLabel) %>% 
  unnest_longer(varnm)

createColList <- function(dataType){
    switch (dataType,
          `$` = 'c',
          `01` = 'f',
          `##` = 'n',
          `#` = 'i',
          `dt` = 'D',
          `b` = 'f',
          `b0` = 'f')
}


columnTypes <- allVariableInfo %>% 
  select(varnm,tp) %$%
  map(tp,~createColList(.x)) %>% 
  unlist() %>% 
  setNames(dataWType$varnm) %>% 
  append(list(redcap_repeat_instrument='c',redcap_repeat_instance='i'),after=0) %>% 
  cols(!!!.)


colNames <- dataWType %>% 
  pull(varnm) %>% 
  append(c('redcap_repeat_instrument','redcap_repeat_instance'),after=0) 

#Read Data
data=read_csv('Data/raw/01 ConcordanceStudy_DATA_2022-12-30_1413.csv',col_types=columnTypes,col_select=all_of(colNames))



