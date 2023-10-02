#Rachel Fellman
#ST558 Project 2
#code to render rmd file and create .md file.

rmarkdown::render("C:/Users/rfell/Documents/Grad Certificate/ST 558/ST558Project2/vignette.rmd", 
                  output_format = "github_document", 
                  output_file = "README.md"
                  output_options = list(
                    toc = TRUE,
                    number_sections = TRUE,
                    toc_depth = 2,
                    df_print = 'tibble'
                  )
)