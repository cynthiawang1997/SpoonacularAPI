#' Get the recipes.
#'
#' This functions gets the recipes with specified requirements.
#' @param key API key
#' @param query 	The (natural language) recipe search query.
#' @param cuisine The cuisine(s) of the recipes. One or more, comma separated (will be interpreted as 'OR')
#' @param diet The diet for which the recipes must be suitable.
#' @param exclude A comma-separated list of ingredients or ingredient types that the recipes must not contain.
#' @param intolerance A comma-separated list of intolerances. All recipes returned must not contain ingredients that are not suitable for people with the intolerances entered.
#' @param instruction Whether the recipes must have instructions.
#' @param maxReadyTime The maximum time in minutes it should take to prepare and cook the recipe.
#' @param minCalories The minimum amount of calories the recipes must have.
#' @param maxCalories The maximum amount of calories the recipes can have.
#' @param minCarbs 	The minimum amount of carbohydrates in grams the recipes must have.
#' @param maxCarbs The maximum amount of carbohydrates in grams the recipes can have.
#' @param minProtein 	The minimum amount of protein in grams the recipes must have.
#' @param maxProtein The maximum amount of protein in grams the recipes can have.
#' @param minFat The minimum amount of fat in grams the mrecipes must have.
#' @param maxFat The maximum amount of fat in grams the recipes can have.
#' @param minAlcohol The minimum amount of alcohol in grams the recipe must have.
#' @param maxAlcohol The maximum amount of alcohol in grams the recipe can have.
#' @param minCaffeine The minimum amount of caffeine in milligrams the recipe must have.
#' @param maxCaffeine The maximum amount of caffeine in milligrams the recipe can have.
#' @param minSaturatedFat The minimum amount of saturated fat in grams the recipe must have.
#' @param maxSaturatedFat The maximum amount of saturated fat in grams the recipe can have.
#' @param number The number of expected results (between 1 and 10).
#' @return The recipes requested by users.
#' @examples
#' get_recipe(key = Sys.getenv("SPOON_KEY"), query = "pork" ,number = 10)
#' @export
get_recipe <- function(key = NULL,query = NULL,cuisine = NULL,diet = NULL,exclude = NULL,intolerance = NULL,number = 10, instruction = NULL, maxReadyTime = NULL, minCalories = NULL,maxCalories = NULL,minCarbs = NULL,maxCarbs = NULL,minProtein = NULL, maxProtein = NULL, minFat = NULL, maxFat = NULL,minAlcohol = NULL, maxAlcohol = NULL, minCaffeine = NULL, maxCaffeine = NULL, minSaturatedFat = NULL, maxSaturatedFat = NULL) {
  if (is.null(key)) {
    return("please enter your API key.")
  }
  querypar <- list(apiKey = key, query = query, cuisine = cuisine, diet = diet, excludeIngredients = exclude, number = number, instructionsRequired = instruction, maxReadyTime = maxReadyTime, minCalories = minCalories,maxCalories = maxCalories,minCarbs = minCarbs,maxCarbs = maxCarbs,minProtein = minProtein, maxProtein = maxProtein, minFat = minFat, maxFat = maxFat,minAlcohol = minAlcohol,maxAlcohol = maxAlcohol,minCaffeine = minCaffeine, maxCaffeine = maxCaffeine, minSaturatedFat = minSaturatedFat, maxSaturatedFat = maxSaturatedFat)
  base_url <- "https://api.spoonacular.com/recipes/complexSearch"
  data <- httr::GET(paste(base_url), query = querypar)
  if (httr::http_status(data)$category != "Success") {
    return(httr::http_status(data))
  } else {
    df <- jsonlite::fromJSON(httr::content(data, as = "text"), flatten = TRUE)[[1]]
    if (is.data.frame(df) == FALSE) {
      return("No results found. Please enter new parameters.")
    } else {
      df <- df %>% dplyr::select(recipe_id = id, recipe_name = title)
      return(df)
    }
  }
}
