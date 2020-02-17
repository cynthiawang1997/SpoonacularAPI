#' Get the menu items.
#'
#' This functions gets the menu items from different restaurant chains.
#' @param key API key
#' @param query keywords in the menu items.
#' @param minCalories The minimum amount of calories the menu item must have.
#' @param maxCalories The maximum amount of calories the menu item can have.
#' @param minCarbs 	The minimum amount of carbohydrates in grams the menu item must have.
#' @param maxCarbs The maximum amount of carbohydrates in grams the menu item can have.
#' @param minProtein 	The minimum amount of protein in grams the menu item must have.
#' @param maxProtein The maximum amount of protein in grams the menu item can have.
#' @param minFat The minimum amount of fat in grams the menu item must have.
#' @param maxFat The maximum amount of fat in grams the menu item can have.
#' @param number The number of expected results (between 1 and 10).
#' @return The menu items requested by users.
#' @examples
#' get_menu(key = Sys.getenv("SPOON_KEY"),query = "burger" ,number = 10)
#' @export
get_menu <- function(key = NULL,query = NULL, minCalories = NULL,maxCalories = NULL,minCarbs = NULL,maxCarbs = NULL,minProtein = NULL, maxProtein = NULL, minFat = NULL, maxFat = NULL, number = 10) {
  if (is.null(key)) {
    return("please enter your API key.")
  }
  querypar_menu <- list(apiKey = key, query = query, minCalories = minCalories,maxCalories = maxCalories,minCarbs = minCarbs,maxCarbs = maxCarbs,minProtein = minProtein, maxProtein = maxProtein, minFat = minFat, maxFat = maxFat, number = number)
  menu_url <- "https://api.spoonacular.com/food/menuItems/search"
  menu_data <- httr::GET(paste(menu_url), query = querypar_menu)
  if (httr::http_status(menu_data)$category != "Success") {
    return(httr::http_status(menu_data))
  } else {
    menu_df <- jsonlite::fromJSON(httr::content(menu_data,as = "text"), flatten = TRUE)[2][[1]]
    if (is.data.frame(menu_df) == FALSE) {
      return("No results found. Please enter new parameters.")
    } else {
      menu_df_2 <- menu_df %>% dplyr::select(item_id = id, food_name = title, restaurantChain)
      return(menu_df_2)
    }
  }
}
