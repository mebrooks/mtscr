#' Creativity assessment through semantic distance dataset
#'
#' A dataset from Forthmann, Karwowski & Beaty ([2023](https://doi.org/10.1037/aca0000571)) paper.
#' It contains a set of responses in Alternative Uses Task for different items with their
#' semantic distance assessment.
#'
#' @return a [tibble][tibble::tibble-package]
#' @format ## `mtscr_creativity`
#' A `tibble` with 4585 rows and 10 columns:
#' \describe{
#'   \item{id}{patricipants' unique identification number}
#'   \item{response}{response in AUT}
#'   \item{item}{item for which alternative uses were searched for}
#'   \item{SemDis_MEAN}{mean semantic distance}
#' }
#'
#' @source <https://osf.io/7rgsp/>
"mtscr_creativity"
