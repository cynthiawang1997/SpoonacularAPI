#' Get the nutritional information of menu items.
#'
#' This functions gets the nutritional information of one menu item using menu item ID.
#' @param key API key
#' @param item_id The menu item ID.
#' @return The nutritional information of menu items.
#' @examples
#' get_menu_info(key = Sys.getenv("SPOON_KEY"), item_id = 278131)
#' @export
#' @importFrom magrittr "%>%"
get_menu_info <- function(key = NULL, item_id = NULL) {
  if (is.null(key) | is.null(item_id)) {
    return("API key or item_id is missing.")
  }
  querypar_minfo <- list("apiKey" = key)
  minfo_data <- httr::GET(paste("https://api.spoonacular.com/food/menuItems/",item_id,sep = ""), query = querypar_minfo)
  if (httr::http_status(minfo_data)$category != "Success") {
    return(httr::http_status(minfo_data))
  } else {
    minfo_list <- jsonlite::fromJSON(httr::content(minfo_data,as = "text"), flatten = TRUE)[[6]]
    if (is.null(minfo_list)) {
      return("No results found. Please enter new parameters.")
    } else {
      general_nutrition_info <- as.data.frame(minfo_list[1:4])
      minfo_df <- as.data.frame(minfo_list)
      return(minfo_df)
    }
  }
}
