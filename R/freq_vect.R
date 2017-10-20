#' freq_vect
#'
#' freq_vect returns a frequency table with counts and percentages of values
#'   from an atomic vector of type character, integer, double, or logical
#'
#' @param data_vector   an atomic vector of type character, integer, double, or logical
#' @param sort_by_count   boolean value that determines if output will be sorted by count or name
#' @param round   an integer value that determines the number of decimal places displayed
#' @param miss_row   a boolean value that determines if the NA row is displayed
#' @param total_row   a boolean value that determines if the output will have a summary row appended
#'
#' @return a data_frame containing the counts and percentages of each value from the provided data
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' # Sample vector and data frame to demo the freq_vect function.
#' numbers <- sample(1:10, 200, replace = TRUE)
#' tbl     <- data.frame(numbers = sample(1:10, 200, replace = TRUE),
#'   letters = sample(letters, 200, replace = TRUE),
#'   dates = sample(seq(as.Date('1999/10/01'), as.Date('2000/01/01'), by="day"),
#'                  200, replace = TRUE),
#'   logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
#'   stringsAsFactors = FALSE)
#'
#' freq_vect(numbers)
#' freq_vect(tbl$numbers)
#' freq_vect(tbl$letters, sort_by_count = TRUE, total_row = FALSE)
#' freq_vect(mtcars$cyl)

freq_vect2 <- function(data_vector, sort_by_count = FALSE, round = 1, miss_row = TRUE, total_row = TRUE) {

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  data = n = total = Percentage = Cum. = NULL

  # Check validity of data_vector argument. The argument needs to be a string and the data frame needs to exist.
  if (!is.atomic(data_vector)) return(stop('freq_vect requires an atomic vector.'))

  if (!(typeof(data_vector) %in% c('logical', 'integer', 'double','character'))) {
    return(stop('Vector not of acceptable data type. Needs to be of type logical, integer, double, or character.'))
  }

  if (length(data_vector) < 2) return(stop('Vector needs a length greater than 1.'))


  # Check if sort_by_count is set.
  if (!is.logical(sort_by_count)) sort_by_count <- TRUE
  sort_by <- ifelse(sort_by_count, 'desc(n)', 'data')

  df <- data.frame(data = data_vector, stringsAsFactors = FALSE)

  result <- dplyr::count(df, data)

  if (!is.logical(miss_row)) miss_row <- TRUE
  if (miss_row == FALSE) {
    result <- dplyr::filter(result, data != "<NA>")
  }

  result <- result %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::group_by(data) %>%
    dplyr::mutate(Percentage = formatC(n * 100 / total, digits = round, format = "f")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(sort_by) %>%
    dplyr::mutate(Cum. = cumsum(Percentage)) %>%
    dplyr::mutate(Cum. = ifelse(Cum. > 100, round(Cum., 0), Cum.)) %>%
    dplyr::mutate(Cum. = formatC(Cum., digits = round, format = "f")) %>%
    dplyr::select(data,
                  Count = n,
                  Percentage,
                  Cum.)

  if (!is.logical(total_row)) total_row <- TRUE
  if (total_row) {
    x <- formatC(100, digits = round, format = "f")
    result[,1] <- lapply(result[,1], as.character)
    result <- rbind.data.frame(result,c('Total', sum(result$Count), x, ""))
  }

  return(result)
}
