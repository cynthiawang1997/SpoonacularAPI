## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE----------------------------------------------------
devtools::load_all(".")

## ------------------------------------------------------------------------
SpoonacularAPI::get_recipe(key = Sys.getenv("SPOON_KEY"), query = "beef" ,number = 10)

## ------------------------------------------------------------------------
SpoonacularAPI::get_recipe_information(key = Sys.getenv("SPOON_KEY"), recipe_ids = "509163,825127,600427")

## ------------------------------------------------------------------------
SpoonacularAPI::get_recipe_equipment(key = Sys.getenv("SPOON_KEY"), recipe_id = "753644")

## ------------------------------------------------------------------------
SpoonacularAPI::get_recipe_ingredient(key = Sys.getenv("SPOON_KEY"), recipe_id = "753644")

## ------------------------------------------------------------------------
SpoonacularAPI::get_recipe_nutrition(key = Sys.getenv("SPOON_KEY"), recipe_id = "753644")

## ------------------------------------------------------------------------
SpoonacularAPI::get_menu(key = Sys.getenv("SPOON_KEY"),query = "burger" ,number = 10)

## ------------------------------------------------------------------------
SpoonacularAPI::get_menu_info(key = Sys.getenv("SPOON_KEY"), item_id = 419357)

