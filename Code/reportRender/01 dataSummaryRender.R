rmarkdown::render(input='Code/dataChecking/02_dataSummaryReport.Rmd',
                  output_file = '05_Concord_dataSummary_Labeled.docx',
                  output_dir = 'Output/reports',knit_root_dir = getwd())

