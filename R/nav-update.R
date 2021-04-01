#' Programmatic panel updating
#'
#' @inheritParams shiny::updateTabsetPanel
#' @export
nav_update <- function(input_id, selected = NULL,
                       session = shiny::getDefaultReactiveDomain()) {
  shiny::updateTabsetPanel(session, input_id, selected)
}

#' @inheritParams shiny::insertTab
#' @export
#' @rdname nav_update
nav_insert <- function(input_id, nav_item, target, position = c("before", "after"),
                       select = FALSE, session = shiny::getDefaultReactiveDomain()) {
  shiny::insertTab(input_id, panel, target, position, select, session)
}

#' @inheritParams shiny::removeTab
#' @export
#' @rdname nav_update
nav_remove <- function(input_id, target, session = shiny::getDefaultReactiveDomain()) {
  shiny::removeTab(input_id, target, session)
}
