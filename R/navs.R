#' Create navigation items
#'
#' @inheritParams shiny::tabPanel
#' @export
#' @seealso [navs_tab]
nav <- function(title, ..., value = title, icon = NULL) {
  if (is.character(icon)) icon <- shiny::icon(icon) # TODO: should tabPanel() do this?
  shiny::tabPanel(title, ..., value = title, icon = icon)
}

#' @inheritParams shiny::navbarMenu
#' @export
nav_menu <- function(title, ..., value = title, icon = NULL) {
  if (is.character(icon)) icon <- shiny::icon(icon) # TODO: should navbarMenu() do this?
  shiny::navbarMenu(title, ..., menuName = value, icon = icon)
}

#' @inheritParams shiny::navItem
#' @export
nav_item <- function(...) {
  if (!is_available("shiny", "1.6.0.9001")) {
    stop("shiny v1.6.0.9001 or higher is required for this functionality")
  }
  shiny::navItem(...)
}

#' @export
nav_spacer <- function() {
  if (!is_available("shiny", "1.6.0.9001")) {
    stop("shiny v1.6.0.9001 or higher is required for this functionality")
  }
  shiny::navSpacer()
}


#' Create a navigation panel
#'
#' Render a collection of [nav()] items into a panel of contents.
#'
#' @inheritParams shiny::tabsetPanel
#' @export
#' @rdname navs
navs_tab <- function(..., id = NULL, selected = NULL, header = NULL, footer = NULL) {
  tabset <- shiny::tabsetPanel(
    ..., type = "tabs", id = id, selected = selected, header = header, footer = footer
  )
  fragment(tabset)
}

#' @export
#' @rdname nav
navs_tab_card <- function(..., id = NULL, selected = NULL, header = NULL, footer = NULL) {
  navs_card(navs_tab(..., id = id, selected = selected, header = header, footer = footer))
}

#' @export
#' @rdname navs
navs_pill <- function(..., id = NULL, selected = NULL, header = NULL, footer = NULL) {
  tabset <- shiny::tabsetPanel(
    ..., type = "pills", id = id, selected = selected, header = header, footer = footer
  )
  fragment(tabset)
}

#' @export
#' @rdname navs
navs_pill_card <- function(..., id = NULL, selected = NULL, header = NULL, footer = NULL) {
  navs_card(navs_pill(..., id = id, selected = selected, header = header, footer = footer))
}

#' @export
#' @rdname navs
navs_pill_list <- function(..., id = NULL, selected = NULL, header = NULL, footer = NULL) {
  tabset <- shiny::navtreePanel(..., id = id, selected = selected, header = header, footer = footer)
  fragment(tabset)
}

#' @export
#' @rdname navs
navs_hidden <- function(..., id = NULL, selected = NULL) {
  fragment(shiny::tabsetPanel(..., type = "hidden", id = NULL, selected = NULL))
}

#' @inheritParams shiny::navbarPage
#' @export
#' @rdname nav
navs_bar <- function(..., title = NULL, id = NULL, selected = NULL,
                     # TODO: add sticky-top as well?
                     position = c("static-top", "fixed-top", "fixed-bottom"),
                     header = NULL, footer = NULL, collapsible = TRUE, bg = NULL) {

  inverse <- TRUE
  if (!is.null(bg)) {
    bg <- htmltools::parseCssColors(bg)
    bg_contrast <- bs_get_contrast(bs_theme("navbar-bg" = bg), "navbar-bg")
    inverse <- col2rgb(bg_contrast)[1,] > 127.5
  }

  # TODO: throw if we detect theme, lang, etc?
  navbar <- shiny::navbarPage(
    title = title, ..., id = id, selected = selected,
    position = match.arg(position), header = header, footer = footer,
    collapsible = collapsible, inverse = inverse
  )

  if (!is.null(bg)) {
    navbar <- tag_append_attrs(
      navbar, "navbar",
      style = css(background_color = paste(bg, "!important"))
    )
  }

  fragment(navbar, page = bs_page)
}


# TODO: A special version of navs_bar() that renders top-level nav_menu()s inline via flex
# similar to this https://www.tracktherecovery.org/ ?
# navs_bar_banner <- function() {}

#' @export
navs_bars <- function(...) {
  # TODO: assert these are navs_bar() objects
  bars <- rlang::list2(...)
  navbars <- lapply(bars, find_first_tag, "navbar")
  idx <- seq_len(length(bars) - 1)
  navbars[idx] <- tag_append_attrs(
    navbars[idx], "navbar",
    style = css(margin_bottom = 0)
  )
  contents <- lapply(bars, find_first_tag, "tab-content")
  fragment(tagList(navbars, contents), page = bs_page)
}


navs_card <- function(x) {
  content <- find_first_tag(x, "tab-content")
  nav <- find_first_tag(x, "nav")
  if (has_class(nav, "nav-tabs")) {
    nav <- tagAppendAttributes(nav, class = "card-header-tabs")
  }
  if (has_class(nav, "nav-pills")) {
    nav <- tagAppendAttributes(nav, class = "card-header-pills")
  }
  card(content, header = nav)
}
