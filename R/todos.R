#' Find "TODO:"  in files
#'
#' @param f_in file to be tested
#'
#' @return a flag (TRUE/FALSE)
#' @export
#'
#' @examples
#'   example_file <- fs::path(
#' system.file('extdata/examples/example_RMD.Rmd', package = 'textcheck')
#' )
#' flag <- find_todos(example_file)
find_todos <- function(f_in) {
  txt <- paste0(readr::read_lines(f_in), collapse = '\n')
  txt <- readr::read_lines(f_in)

  my_regex <- stringr::fixed("TODO:")

  flag <- stringr::str_detect(txt, my_regex)

  if (any(flag)) {
    cli::cli_alert_danger("Found TODO in {f_in}:")

    for (i_flag in which(flag)) {
      cli::cli_alert_info("line {i_flag} -> {txt[i_flag]}")

    }
  }

  return(invisible(any(flag)))

}
