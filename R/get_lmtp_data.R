#' Process Covariates for LMTP/TMLE Analysis
#'
#' Automatically detects variable types among a specified set of covariates and prepares
#' them for causal inference analysis (e.g., LMTP or TMLE). It converts binary variables
#' to 0/1 numeric format and creates dummy variables (one-hot encoding) for categorical
#' variables with 3 to 6 levels.
#'
#' @param data A data frame containing the dataset to be processed.
#' @param covs A character vector of column names representing the covariates to process.
#'   Variables not listed here are ignored during the type-detection phase but are
#'   retained in the output.
#'
#' @return A data frame with:
#' \itemize{
#'   \item Binary variables (2 levels) converted to numeric 0/1.
#'   \item Categorical variables (3-6 levels) one-hot encoded with the first level removed (to avoid multicollinearity).
#'   \item Column names cleaned to snake_case.
#' }
#'
#' @importFrom dplyr select summarise across everything mutate filter pull all_of n_distinct
#' @importFrom tidyr pivot_longer
#' @importFrom fastDummies dummy_cols
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = 1:5,
#'   sex = c("M", "F", "M", "M", "F"),
#'   education = c("HS", "BA", "MA", "PhD", "HS"), # 4 levels
#'   status = c(1, 2, 1, 2, 1),                    # Binary numeric
#'   age = c(20, 30, 40, 50, 60)                   # Continuous
#' )
#'
#' # 'age' will be ignored by encoding because it has >6 levels (all unique)
#' # 'sex' and 'status' will become 0/1
#' # 'education' will become dummy columns
#' clean_df <- process_covariates_lmtp(df, c("sex", "education", "status", "age"))
#' }
#'
get_lmtp_data <- function(data, covs) {

  # 1. Input Validation
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.character(covs)) stop("`covs` must be a character vector.")
  missing_cols <- setdiff(covs, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("The following covariates are missing from data: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  # 2. Identify Variable Types (Safely ignoring NAs)
  # We use .data$ notation to satisfy R CMD Check
  var_stats <- data %>%
    dplyr::select(dplyr::all_of(covs)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~dplyr::n_distinct(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "variable",
                        values_to = "n_levels")

  # 3. Define Thresholds
  # Binary: Exactly 2 distinct levels
  vars_binary <- var_stats %>%
    dplyr::filter(.data$n_levels == 2) %>%
    dplyr::pull(.data$variable)

  # Categorical: Between 3 and 6 distinct levels
  vars_categorical <- var_stats %>%
    dplyr::filter(.data$n_levels > 2 & .data$n_levels < 7) %>%
    dplyr::pull(.data$variable)

  # 4. Process Data
  data_processed <- data %>%
    # A. Handle Binary Variables (Universal 0/1 conversion)
    # Converts (1,2) -> (0,1), ("M","F") -> (0,1), (0,1) -> (0,1)
    dplyr::mutate(dplyr::across(dplyr::all_of(vars_binary),
                                ~as.numeric(as.factor(.)) - 1)) %>%

    # B. Handle Categorical Variables (One-Hot Encoding)
    fastDummies::dummy_cols(
      select_columns = vars_categorical,
      remove_first_dummy = TRUE,      # Prevent dummy variable trap
      remove_selected_columns = TRUE, # Replace original column
      ignore_na = TRUE                # Preserves NAs
    ) %>%

    # C. Cleanup column names
    janitor::clean_names()

  return(data_processed)
}
