#' Write one table or multiple tables to a 'GraphPad Prism' '.pzfx' file
#'
#' Write one table or multiple tables to a 'GraphPad Prism' '.pzfx' file. A table can be a 'matrix',
#'   a 'data.frame', or a 'tibble'. All elements of the table should be numeric.
#'
#' @param x Input table or named list of tables that will be 'Data Tables' in the '.pzfx' file
#' @param path Path to the output '.pzfx' file.
#' @param row_names Logical. If row names of the input table should be preserved and become row
#'   titles in the output '.pzfx' file. If the length is greater than 1, it must match the length of
#'   list of input tables. Default: TRUE.
#' @param x_col 1-based column index or name of the column to be used as the 'X' column. If the
#'   length is greater than 1, it must match the length of list of input tables. All other columns
#'   in the input tables will be treated as "Y" columns in the output '.pzfx' file. Default: NA
#'
#' @return write_pzfx returns the input x invisibly.
#'
#' @export
#'
#' @examples
#' pzfx_file <- system.file("extdata/exponential_decay.pzfx", package = "pzfx", mustWork = TRUE)
#' df <- read_pzfx(pzfx_file, table = 1, strike_action = "exclude")
#' write_pzfx(df, path = tempfile(fileext = ".pzfx"), row_names = TRUE)
write_pzfx <- function(x, path, x_var = "Minutes", sub_var = "Animal", grp_var = "Group", val_var = "Value") {
  lst <- base_lst()
  lst$GraphPadPrismFile$TableSequence <- table_seq_lst()
  lst$GraphPadPrismFile <- append(lst$GraphPadPrismFile, table_lst(x, x_var, sub_var, grp_var, val_var))
  attr(lst$GraphPadPrismFile, "PrismXMLVersion") <- "5.00"
  xml <- xml2::as_xml_document(lst)
  xml2::write_xml(xml, path)
  invisible(x)
}

# The basic list for a pzfx xml
base_lst <- function() {
  lst <- list(
    "GraphPadPrismFile" = list(
      "Created" = list(
        "OriginalVersion" = structure(
          list(),
          CreatedByProgram = "GraphPad Prism",
          CreatedByVersion = "6.0f.254",
          Login = "",
          DateTime = strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
        )
      ),
      "InfoSequence" = list(
        "Ref" = structure(
          list(),
          "ID" = "Info0",
          "Selected" = "1"
        )
      ),
      "Info" = structure(
        list(
          "Title" = list("Project info 1"),
          "Notes" = list(""),
          "Constant" = list("Name" = list("Experiment Date"), "Value" = list("")),
          "Constant" = list("Name" = list("Experiment ID"), "Value" = list("")),
          "Constant" = list("Name" = list("Notebook ID"), "Value" = list("")),
          "Constant" = list("Name" = list("Project"), "Value" = list("")),
          "Constant" = list("Name" = list("Experimenter"), "Value" = list("")),
          "Constant" = list("Name" = list("Protocol"), "Value" = list(""))
        ),
        "ID" = "Info0"
      )
      # Then, the "TableSequence" list goes here
      # Then, all "Table" lists go here
    )
  )
  return(lst)
}

# "TableSequence" element of the list for a pzfx xml
# Number of Refs corresponds to number of tables
table_seq_lst <- function() {
  # ret <- lapply(seq_len(length(x_lst)), function(i) {

  ref <- structure(
    list(),
    "ID" = "Table0"
  )
  attr(ref, "Selected") <- "1"
  # return(ref)
  # })
  list(
    Ref = ref
  )
}

# "Table" elements of the list for a pzfx xml
# As many tables as you have
# Currently only supports pzfx's "Column" type of tables
table_lst <- function(.data, x_var, sub_var, grp_var, val_var) {
  subcol_helper <- function(v) {
    v <- as.vector(v)
    lapply(v, function(e) list("d" = list(as.character(e))))
  }


  # ret <- lapply(seq_len(length(x_lst)), function(i) {

  X_vals <- .data[[x_var]] %>%
    unique() %>%
    sort()


  XColumn <- list(
    "XColumn" = structure(
      list(
        "Title" = list(x_var),
        "Subcolumn" = subcol_helper(X_vals)
      ),
      Width = "89",
      Decimals = "0",
      Subcolumns = "1"
    )
  )


  GRP_vals <- .data[[grp_var]] %>% unique()

  YColumns <- purrr::map(GRP_vals, function(grp) {
    grp_dat <- .data %>%
      dplyr::filter(!!sym(grp_var) == grp)

    sub_cols <- grp_dat[[sub_var]] %>% unique()
    Subcolumns <- purrr::map(sub_cols, function(subcol) {
      grp_dat %>%
        dplyr::filter(!!sym(sub_var) == subcol) %>%
        pull(!!sym(val_var)) %>%
        subcol_helper()
    })

    names(Subcolumns) <- rep("Subcolumn", length(Subcolumns))

    structure(
      c(
        list("Title" = grp),
        Subcolumns
      ),
      Width = "89",
      Decimals = "0",
      Subcolumns = as.character(length(sub_cols))
    )
  })

  names(YColumns) <- rep("YColumn", length(YColumns))

  list(
    "Table" = structure(
      c(
        XColumn,
        YColumns
      ),
      ID = "Table0",
      XFormat = "numbers",
      YFormat = "replicates",
      Replicates = "3",
      TableType = "XY",
      EVFormat = "AsteriskAfterNumber"
    )
  )
}
