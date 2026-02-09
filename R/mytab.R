#' Create a "Table 1" with Flexible Selection and Formatting
#'
#' A wrapper around the \code{tableone} package that allows for tidyverse-style
#' variable selection (e.g., \code{starts_with()}, \code{contains()}) and
#' integrates custom printing options and CSV export.
#'
#' @param data A data frame containing the variables to analyze.
#' @param ... Variable selection (unnamed arguments) and print options (named arguments).
#'   You can use tidyselect helpers like \code{starts_with("age")}.
#'   Named arguments (e.g., \code{nonnormal = "age"}, \code{exact = "stage"}) are passed
#'   directly to the \code{print.TableOne} method.
#' @param by Character. The name of the stratification variable (optional).
#' @param cont_patterns Character vector. A regex pattern to identify continuous variables.
#'   Variables NOT matching this pattern will be treated as categorical.
#'   If NULL, standard \code{tableone} logic applies.
#' @param export_csv Logical. If TRUE, saves the formatted table to a timestamped CSV file
#'   in the working directory. Defaults to FALSE.
#'
#' @return If \code{export_csv = TRUE}, returns the formatted data frame invisibly.
#'   Otherwise, prints the table and returns the \code{tableone} summary object invisibly.
#'
#' @importFrom rlang enquos eval_tidy quo_is_null as_name
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom tableone CreateTableOne
#' @importFrom utils write.csv modifyList
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' mytab(mtcars, mpg, cyl, by = am)
#'
#' # With tidyselect and print options
#' mytab(mtcars, starts_with("d"), nonnormal = "disp", contDigits = 2)
#' }
#' @export
mytab <- function(data, ..., by = NULL, cont_patterns = NULL, export_csv = FALSE) {

  # 1. Capture EVERYTHING as quosures first (prevents early evaluation errors)
  all_quos <- rlang::enquos(...)

  # 2. Separate Selection (unnamed) from Print Options (named)
  arg_names <- names(all_quos)

  # Handle case where no arguments are named (names() returns NULL)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(all_quos))
  }

  # Split logic
  is_selector <- arg_names == ""

  # Selection quosures (keep them unevaluated for select())
  selection_quos <- all_quos[is_selector]

  # Print options (evaluate these now so we get the values like "age", TRUE, 2)
  # We use eval_tidy to turn the quosure back into the actual value
  print_args <- lapply(all_quos[!is_selector], rlang::eval_tidy)

  # 3. Capture 'by' variable safely
  by_quo <- rlang::enquo(by)
  has_by <- !rlang::quo_is_null(by_quo)
  by_name <- if (has_by) rlang::as_name(by_quo) else NULL

  # 4. Smart Selection
  # Now we pass the unevaluated selection_quos into select() using !!!
  if (length(selection_quos) == 0) {
    # If no columns specified, take all
    df_subset <- data
    vars_list <- names(data)
    if (has_by) vars_list <- setdiff(vars_list, by_name)
  } else {
    # Combine user selection + 'by' variable
    if (has_by) {
      df_subset <- data %>% dplyr::select(!!!selection_quos, !!by_quo)
      vars_list <- setdiff(names(df_subset), by_name)
    } else {
      df_subset <- data %>% dplyr::select(!!!selection_quos)
      vars_list <- names(df_subset)
    }
  }

  # 5. OVERRIDE: Continuous vs Categorical Logic
  if (!is.null(cont_patterns)) {
    regex_str <- paste0("^(", paste(cont_patterns, collapse = "|"), ")")
    cat_vars <- vars_list[!grepl(regex_str, vars_list)]
  } else {
    cat_vars <- vars_list
  }

  # 6. Create TableOne object
  t1_args <- list(
    vars = vars_list,
    data = df_subset,
    factorVars = cat_vars,
    includeNA = TRUE,
    test = FALSE,
    smd = FALSE
  )
  if (has_by) t1_args$strata <- by_name

  t1_obj <- do.call(tableone::CreateTableOne, t1_args)

  # 7. Generate Matrix with Custom Print Arguments
  final_print_args <- list(
    x = t1_obj,
    quote = FALSE,
    noSpaces = TRUE,
    printToggle = FALSE
  )

  # Merge user-defined print arguments (e.g., nonnormal, contDigits)
  final_print_args <- utils::modifyList(final_print_args, print_args)

  # Generate the table matrix
  tab_mat <- do.call(print, final_print_args)

  # 8. Output and Export
  if (export_csv) {
    file_name <- paste0("Table1_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    utils::write.csv(tab_mat, file = file_name)
    message(paste("Table exported to:", file_name))
    return(invisible(as.data.frame(tab_mat)))
  } else {
    # Print the standard tableone formatted matrix
    print(tab_mat, quote = FALSE)
    cat("\n--- Detailed Summary for inspection ---\n")
    return(invisible(summary(t1_obj)))
  }
}
