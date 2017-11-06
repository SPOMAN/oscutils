#' Group meeting planner
#'
#' Generates random group meeting plans until the following conditions are met:
#' No people having presentations in consequitive weeks or on the same day.
#' All project presentations unique.
#' No more than two journal clubs per person
#'
#' @param ndates Number of days in the plan
#' @param namelist Tibble containing columns name and journalclub
#' @param nrep Max number of journal clubs per person
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' nms <- tibble::tribble(
#' ~name, ~journalclub,
#' "Asger", "Electrochemistry",
#' "Emil", "Electrochemistry",
#' "Magnus", "Electrochemistry",
#' "Matteo", "Electrochemistry",
#' "Andreas", "Polymer chemistry",
#' "Paolo", "Polymer chemistry",
#' "Stefan", "Polymer chemistry",
#' "Monica", "Electrochemistry",
#' "Siad", "Electrochemistry",
#' "Christina", "Polymer chemistry",
#' "Annika", "Polymer chemistry",
#' "Cecilie", "Polymer chemistry",
#' "Mahdi", "Polymer chemistry",
#' "Surnimal", "Polymer chemistry",
#' "Xinming", "Electrochemistry",
#' "Lizhi", "Polymer chemistry",
#' "Mie", "Polymer chemistry"
#' #"Kim", "Polymer chemistry",
   #"Steen", "Electrochemistry"
#' )
#'
#' plan <- group_plan(13, nms, nrep = 2)
#'
#' readr::write_excel_csv(plan, "2017_fall_group_meetings.csv")
#'
#' nms %>% dplyr::filter(journalclub=="Electrochemistry") %>% dplyr::pull(name) %>% paste(collapse = ", ")
#' nms %>% dplyr::filter(journalclub=="Polymer chemistry") %>% dplyr::pull(name) %>% paste(collapse = ", ")

group_plan <- function(ndates, namelist, nrep = 2) {
  n <- 0
  repeat {
    n <- n + 1
    plan <- tibble::tibble(
      project = sample(dplyr::pull(namelist, name), size = ndates),
      electrochem = sample(namelist %>% dplyr::filter(journalclub == "Electrochemistry") %>% dplyr::pull(name), replace = TRUE, size = ndates),
      polymer = sample(namelist %>% dplyr::filter(journalclub == "Polymer chemistry") %>% dplyr::pull(name), replace = TRUE, size = ndates)
    )

    check <- check_plan(plan, ndates, nrep)

    if (n %% 100 == 0) cat(paste("No solution in", n, "iterations..\n"))

    if (check == 0) {
      cat(paste("Solution found in", n, "iterations\n"))
      break
    }
  }
  print(plan)
  plan
}


# Aggregate function used to check number of journal clubs per person
f1 <- function(x)
{
  aggregate(x, by=list(x), FUN=length)
}

# Check function
# Contains all checks. Returns 0 if the plan passes.
check_plan <- function(plan, ndates, nrep) {
  check <- 0

  plan <- plan %>%
    dplyr::mutate(electrochemnext = dplyr::lag(electrochem), polymernext = dplyr::lag(polymer))
  # Every project presentation should be done by a new person
  if (length(unique(plan$project)) != ndates) check = check + 1

  # No person should have project presentation and journal club on the same day
  # No person should present two weeks in a row
  res <- logical(ndates)
  for (i in seq(2, ndates)) {
    res[i] <- length(unique(as.character(plan[i,]))) != ncol(plan)
  }
  check = check + sum(res)

  # Remove extra columns for adjacent weeks
  plan <- plan %>% dplyr::select(project, electrochem, polymer)

  # Check that the total number of duplicate entries in each column is not too large
  # dup <- unlist(lapply(lapply(plan, duplicated), sum))
  # if (max(dup) > ndup) check = check + 1

  # Check that no person has journal club more than twice.
  repeats <- lapply(plan, f1)
  if(max(repeats$electrochem$x) > nrep | max(repeats$polymer$x) > nrep) check = check + 1

  check
}
