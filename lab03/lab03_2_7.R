
# Zestaw zadan 2



#2.7 merge

x <- c("uhgf", "khwq", "GLIGLG")

merge_string_list <- function(x, sep = "")
{
  wector_x <- unlist(x)
  paste(wector_x, collapse = sep)
}

merge_string_list(x, sep = " x ")
