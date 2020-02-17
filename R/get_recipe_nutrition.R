#' Get the nutritional information of the recipes.
#'
#' This functions gets nutritional information of one recipe.
#' @param key API key
#' @param recipe_id The recipe ID.
#' @return The nutritional information of the recipe entered.
#' @examples
#' get_recipe_nutrition(key = Sys.getenv("SPOON_KEY"), recipe_id = "753644")
#' @export
get_recipe_nutrition <- function(key = NULL,recipe_id = NULL) {
  if (is.null(key) | is.null(recipe_id)) {
    return("API key or recipe_id is missing.")
  }
  querypar_nutr <- list("apiKey" = key)
  nutr_data <- httr::GET(paste("https://api.spoonacular.com/recipes/",recipe_id,"/nutritionWidget.json",sep = ""), query = querypar_nutr)
  if (httr::http_status(nutr_data)$category != "Success") {
    return(httr::http_status(nutr_data))
  } else {
    nutr_list <- jsonlite::fromJSON(httr::content(nutr_data,as = "text"), flatten = TRUE)
    if (is.null(nutr_list)) {
      return("No results found. Please enter new parameters.")
    } else {
      general_nutrition_info <- as.data.frame(nutr_list[1:4])
      bad_nutrients <- nutr_list[[5]] %>% dplyr::select(nutrients = title, amount, percentOfDailyNeeds)
      good_nutrients <- nutr_list[[6]] %>% dplyr::select(nutrients = title, amount, percentOfDailyNeeds)
      return(list(general_nutrition_info,good_nutrients,bad_nutrients))
    }
  }
}
