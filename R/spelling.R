
#' Checks spelling in folder
#'
#' @param dir_to_check input directory to check for files
#' @param language language of spelling
#'
#' @noRd
check_spelling <- function(dir_to_check, language) {

  book_files <- fs::dir_ls(dir_to_check,
                           glob = "*.R |*.Rmd")

  dict_en <- qdapDictionaries::GradyAugmented

  df_words <- purrr::map_df(
    book_files,
    spelling::spell_check_files,
    #ignore = readr::read_lines('spelling/accepted_words.txt'),
    lang = language
  ) |>
    dplyr::filter(!word %in% dict_en)

  return(df_words)
}
