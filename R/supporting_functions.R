


#' rsq, range01 and str_convert - functions used mid process inside other functions within the fplBuddy package.
#'
#' @export



# Supporting functions


# Find the r-squared value between variables x and y
rsq <- function (x, y) cor(x, y) ^ 2


# range01()
#
# Function for standardising range between 0 & 1
# See the following:
# https://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r
range01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


# Function for computing the inverse of a normalised range
# https://stackoverflow.com/questions/39355942/denormalize-data
denormalize <- function(x, minval, maxval) {
  x * (maxval - minval) + minval
}


# Sweet function for converting special characters
#https://stackoverflow.com/questions/68618922/iconv-returns-na-when-given-a-string-with-a-specific-special-character
str_convert <- function(x) {
  utf8 = iconv(x,'utf8','ascii//translit')
  latin1 = iconv(x,'latin1','ascii//translit')
  win1250 = iconv(x,'Windows-1250','ascii//translit')
  result = ifelse(
    is.na(utf8),
    ifelse(
      is.na(latin1),
      win1250,
      latin1
    ),
    utf8
  )
  return(result)
}



# "Not In" operator - the opposite of "%in%"
# https://stackoverflow.com/questions/5831794/opposite-of-in-exclude-rows-with-values-specified-in-a-vector

`%!in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))


