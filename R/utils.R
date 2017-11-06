#' New journal
#'
#' @param title Title of the new journal
#' @param date Date given as "YYYYMMDD" e.g. "20170901"
#' @param open Optional. Whether to open the file after creation.
#' @param file Optional. Filename is generated from date and title if not given.
#'
#' @export
#'
new_journal <- function(
  title, date = gsub("-", "", Sys.Date()), open = interactive(),
  file = NULL
) {
  if (is.null(file)) file = journal_filename(title, date)

  writeLines(c(
    paste0("# ", date, ' ', title),
    '',
    "Briefly descripe the goal of this experiment. It can also be a good idea to reference previous experiments by their date, e.g. [link](path_to_journal.html).",
    '',
    "## Protocol",
    '',
    '## Journal',
    '',
    '## Results and discussion',
    '',
    '## Conclusion and outlook'), file)

  if (open) open_file(file)
  file
}

# Return a filename based on title, date and folder
journal_filename <- function(title, date) {
  title = gsub(" ", "_", title)
  paste0(date, "_", title, ".Rmd")
}

# use RStudio to open the file if possible
open_file = function(x) {
  tryCatch(rstudioapi::navigateToFile(x), error = function(e) file.edit(x))
}
