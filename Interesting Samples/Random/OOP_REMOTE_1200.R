#generic method - .x adds overload for class x
# Create a default method for get_n_elements
get_n_elements.default<-function(x,...){
  length(unlist(x))
}
#define generic
get_n_elements<-function(x,...){
  UseMethod("get_n_elements")
}

# Assign classes
class(kitty)<-c("cat", "mammal",  "character")

# Does kitty inherit from cat/mammal/character vector?
inherits(kitty, "cat")
inherits(kitty, "mammal")
inherits(kitty,  "character")

# Is kitty a character vector?
is.character(kitty)

# Does kitty inherit from dog?
inherits(kitty,  "dog")


what_am_i.mammal <- function(x, ...)
{
  message("I'm a mammal")
  NextMethod("what_am_i")
}

# character method
what_am_i.character <- function(x, ...)
{
  message("I'm a character vector")
}
#R6####
# install.packages("R6")
library(R6)
# Add a cook method to the factory definition
# Add an initialize method
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    ..power_rating_watts = 800,
    ..door_is_open = FALSE
  ),
  public = list(
    cook = function(time_seconds) {
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    },
    open_door = function() {
      private$..door_is_open = TRUE
    },
    close_door = function() {
      private$..door_is_open = FALSE
    },
    # Add initialize() method here
    initialize = function(power_rating_watts, door_is_open) {
      if(!missing(power_rating_watts)) {
        private$..power_rating_watts <- power_rating_watts
      }
      if(!missing(door_is_open)) {
        private$..door_is_open <- door_is_open
      }
    }
    
  ),
  active = list(
    # add the binding here
    power_rating_watts = function(){
      private$..power_rating_watts
    },
    power_level_watts = function(value) {
      if(missing(value)) {
        private$..power_level_watts
      } else {
        assert_is_a_number(value)
        assert_all_are_in_closed_range(
          value, lower = 0, upper = private$..power_rating_watts
        )
        private$..power_level_watts <- value
      }
    }
  )
)

# Make a microwave
a_microwave_oven <- microwave_oven_factory$new(650,T)


# Create microwave oven object
a_microwave_oven <- microwave_oven_factory$new()

# Call cook method for 1 second
a_microwave_oven$cook(1)
