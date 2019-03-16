##' Robin Elahi
##' 16 March 2019
library(rmarkdown)
rmarkdown::render("index.Rmd", output_format = "pdf_document", 
                  output_dir = "syllabus", 
                  output_file = paste("Biohopk-174", "syllabus.pdf", sep = "_"))
