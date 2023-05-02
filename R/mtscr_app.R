#' mtscr GUI
#'
#' Shiny app used as graphical interface for mtscr. Simply invoke `mtscr_app()` to run.
#'
#' @return Runs the app. No explicit return value.
#' @export
#'
#' @details
#' First thing you see after running the app is `datamods` window for importing your data.
#' You can use the data already loaded in your environment or any other option.
#' Then you'll see four dropdown lists used to choose arguments for `mtscr_model()`
#' and `mtscr_score()` functions. Consult these functions' documentation for
#' more details (execute `?mtscr_score` in the console). When the parameters are chosen,
#' click "Generate model" button. After a while (up to a dozen or so seconds) models'
#' parameters and are shown along with a scored dataframe.
#'
#' You can download your data as a .csv or an .xlsx file using buttons in the sidebar.
#' You can either download the scores only (i.e. the dataframe you see displayed) or
#' your whole data with `.all_max` and `.all_top2` columns added.
#'
#' For testing purposes, you may use `mtscr_creativity` dataframe. In the importing
#' window change "Global Environment" to "mtscr" and our dataframe should appear
#' in the upper dropdown list. Use `id` for the ID column, `item` for the item
#' column and `SemDis_MEAN` for the score column.
#'
#' @seealso
#'   [mtscr::mtscr_score()] for more information on the arguments.
#'   [mtscr_creativity] for more information about the example dataset.
#'   <div class="csl-bib-body" style="line-height: 2; margin-left: 2em; text-indent:-2em;">
#'   <div class="csl-entry">Forthmann, B., Karwowski, M., &amp; Beaty, R. E. (2023). Don’t throw the “bad” ideas away! Multidimensional top scoring increases reliability of divergent thinking tasks. <i>Psychology of Aesthetics, Creativity, and the Arts</i>. <a href="https://doi.org/10.1037/aca0000571">https://doi.org/10.1037/aca0000571</a></div>
#'   <span class="Z3988" title="url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_id=info%3Adoi%2F10.1037%2Faca0000571&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Ajournal&amp;rft.genre=article&amp;rft.atitle=Don't%20throw%20the%20%E2%80%9Cbad%E2%80%9D%20ideas%20away!%20Multidimensional%20top%20scoring%20increases%20reliability%20of%20divergent%20thinking%20tasks.&amp;rft.jtitle=Psychology%20of%20Aesthetics%2C%20Creativity%2C%20and%20the%20Arts&amp;rft.stitle=Psychology%20of%20Aesthetics%2C%20Creativity%2C%20and%20the%20Arts&amp;rft.aufirst=Boris&amp;rft.aulast=Forthmann&amp;rft.au=Boris%20Forthmann&amp;rft.au=Maciej%20Karwowski&amp;rft.au=Roger%20E.%20Beaty&amp;rft.date=2023-04-20&amp;rft.issn=1931-390X%2C%201931-3896&amp;rft.language=en"></span>
#'   </div>
#' @examples
#' \dontrun{
#' mtscr_app()
#' }
mtscr_app <- function() {
  app_dir <- system.file("GUI", package = "mtscr")
  if (app_dir == "") {
    cli::cli_abort(
      c(
        "The app not found.",
        "i" = "Try reinstalling the {.pkg mtscr} package with {.run devtools::intall_github(\"jakub-jedrusiak/mtscr\")"
      )
    )
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
