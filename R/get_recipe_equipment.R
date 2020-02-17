#' Get the equipments required in recipes.
#'
#' This functions gets the equipments needed for one recipe.
#' @param key API key
#' @param recipe_id The recipe ID.
#' @return The equipments for the recipe entered.
#' @examples
#' get_recipe_equipment(key = Sys.getenv("SPOON_KEY"), recipe_id = "753644")
#' @export
get_recipe_equipment <- function(key = NULL,recipe_id = NULL) {
  if (is.null(key) | is.null(recipe_id)) {
    return("API key or recipe_id is missing.")
  }
  querypar_equi <- list("apiKey" = key)
  equi_data <- httr::GET(paste("https://api.spoonacular.com/recipes/",recipe_id,"/equipmentWidget.json",sep = ""), query = querypar_equi)
  if (httr::http_status(equi_data)$category != "Success") {
    return(httr::http_status(equi_data))
  } else {
    equi_df <- jsonlite::fromJSON(httr::content(equi_data,as = "text"), flatten = TRUE)[[1]]
    if (is.data.frame(equi_df) == FALSE) {
      return("No results found. Please enter new parameters.")
    } else {
      equi_df_2 <- equi_df %>% dplyr::select(equipment = name)
      return(equi_df_2)
    }
  }
}

