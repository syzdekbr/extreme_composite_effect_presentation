#### SHINY CODE FUNCTIONS ####

### Rmarkdown styling functions
{
  ## Adds color to any text, either in html or other output format
  colorize <- function(text_to_color, color) {
    if (knitr::is_latex_output()) {
      sprintf("\\textcolor{%s}{%s}", color, text_to_color)
    } else if (knitr::is_html_output()) {
      sprintf("<span style='color: %s;'>%s</span>", color, 
              text_to_color)
    } else text_to_color
  }
  
  ## Use to print tables in any format
  
  # Preferred Kable styling table options
  kable_options <- function(., ...) {kable_styling(.,bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                                              ...)} #latex_options = c("striped", "scale_down", "repeat_header"))}

  ## Use this table_print call in code because it will run in console- good for development- can also pair with
    ## table_counter_func in general scripts as caption to count figures programatically
  table_print <- function(df, colnames = "", caption = "", ...){
    colnames <- case_when(
      colnames == "" ~ pretty_columns_func(colnames(df)),
      TRUE ~ colnames
    )
      df %>%
        kable(caption = caption, col.names = colnames) %>%
        kable_options(., ...)
     }

  # Prettify column names
  pretty_columns_func <- function(colnames){
    tools::toTitleCase(gsub("[_|.]", " ", colnames))
  }
}