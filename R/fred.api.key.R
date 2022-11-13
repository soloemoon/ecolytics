#' Fredr API Key Wrapper
#' 
#' @description Wrapper to set Fred API Key
#' @param key Fred API Key
#' @examples
#' \dontrun{fred.api.key('AAAAAA')}
#' @export

fred.api.key <-function(key){fredr::fredr_set_key(key)}