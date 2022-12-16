check_folder <- function(dir_to_check, language) {

  book_files <- fs::dir_ls(dir_to_check,
                           glob = "*.R |*.Rmd")

  if (length(book_files) == 0) stop("Found no book files!")

  df_links <- dplyr::bind_rows(
    purrr::map(book_files, get_links)
  )

  # check todos
  purrr::walk(book_files, find_todos)

  unique_links <- unique(df_links$urls)

  df_out <- dplyr::bind_rows(
    purrr::map(unique_links, check_link)
  )

  return(df_out)

}
