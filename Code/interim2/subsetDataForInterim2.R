library(tidyverse)
library(magrittr)
library(stringr)

data <- read_tsv('Data/raw/04 ConcordanceStudy_DATA_2023-01-26_1433_LABELS_tabdelimited_withvnms.txt')
patientsToSelect <- read_tsv('Data/raw/05 interim2studypats tabdelimited.txt')
columnsToSelect <- c("Study ID number",
                     "Biopsy or Surgery Date",
                     "Pathology Report Date",
                     "Enter conclusion from Pathology Report",
                     "Tissue site",
                     "Please specify other tissue site",
                     "Method of biopsy collection:",
                     "Please specify other method of biopsy collection",
                     "What was the pathologist's decision?",
                     "If other, please explain pathologist decision",
                     "Was receptor status determined from biopsy?",
                     "ER status",
                     "Percentage of ER positive",
                     "Intensity of ER",
            	       "PgR status",
										 "Percentage of PgR positive",
										 "Intensity of PgR",
										 "HER2- IHC",
										 "IHC Score",
										 "Was HER2 FISH Performed?",
										 "FISH",
										 "FISH ratio",
										 "HER2 FISH: CEP17 Copy Number", #Not in this data Set
										 "FISH Copy Number",
										 "HER2 FISH Comment",
										 "What was the Treating Oncologists decision/interpretation?",
										 "If other, please explain treating oncologists decision/interpretation.",
										 "Is the treating oncologist planning to treat as metastatic breast cancer?",
										 "Is the treating oncologist planning treatment as Hormone Receptor positive breast cancer?",
										 "Is the treating oncologist planning treatment as HER2+ breast cancer?",
										 "Is the treating oncologist planning treatment as triple negative breast cancer?")


columnReference <- data |>
  slice(1) |>
  pivot_longer(cols=everything(),
               names_to = 'variable',
               values_to = 'variableLabel')

columnsToSelect <- columnReference |> 
  filter(variableLabel %in% columnsToSelect | variable == 'cor_commnt') |>
  filter(str_detect(variable,'(^vis)|(^cor)'))%$%
  set_names(variableLabel,variable)

extractedData <- data %>% 
  filter(is.na(redcap_repeat_instrument)) |>
  filter(vis_id %in% patientsToSelect$vis.id) |> 
  select(all_of(names(columnsToSelect))) |>
  left_join(patientsToSelect,join_by(vis_id==vis.id)) |>
  relocate(pat.id,.before=1) %>% 
  bind_rows(as_tibble_row(append(c(pat.id='Patient ID'),columnsToSelect)),.)

extractedData %>% 
  openxlsx2::write_xlsx("output/interim2/interim2Data.xlsx")
  


  
