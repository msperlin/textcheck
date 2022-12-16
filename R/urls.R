#' check if link work
#'
#' @param link_in link to be tested (character)
#'
#' @return dataframe with result
#' @export
#'
#' @examples
#' check_link("www.google.com")
check_link <- function(link_in) {

  Sys.sleep(1)
  cli::cli_alert_info("\tChecking {link_in}")

  out_get <- list()
  try({
    out_get <- httr::GET(link_in)
  })

  if (length(out_get) == 0) {
    status_code <- NA
    status <- NA
    status_message <- NA
    status_explanation <- NA
  } else {
    status_code <- out_get$status_code
    status <- httpcode::http_code(status_code)
    status_message <- status$message
    status_explanation <- status$explanation

    if (status_code == 200) {

      cli::cli_alert_success('\t\turl {status_message}')

    } else {
      cli::cli_alert_danger('\t\turl {status_message}')
    }
  }

  tib_out <- dplyr::tibble(
    url = link_in,
    status_code,
    status_message,
    status_explanation,
    time = Sys.time()
  )

  return(tib_out)

}


get_links <- function(f_in) {
  txt <- paste0(readr::read_lines(f_in), collapse = '\n')

  my_regex <- r"((http|ftp|https):\/\/([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:\/~+#-]*[\w@?^=%&\/~+#-]))"

  all_urls <- stringr::str_extract_all(txt, my_regex)[[1]]

  tib_out <- dplyr::tibble(
    file = basename(f_in),
    urls = all_urls
  )

  return(tib_out)
}
