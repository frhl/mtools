#' easy perl-like regex
#' 
#' returns a perl like regex match
#' 
#' @param pattern the regex pattern
#' @param text the text to be matched
#' @param invert logical indicating whether non-matched string 
#' should be returned insteadof matched string.
#' @param perl logical indicating whether perl like regex should
#' be used
#' @export

regex_find <- function(pattern, text, invert = F, perl = F){
  return(regmatches(text, regexpr(pattern, text, perl), invert))
}





