<<<<<<< HEAD
# Explore microwave oven class
microwave_oven_factory

# Update the class definition
fancy_microwave_oven_factory <- R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory,
  # Add a public list with a cook method
  public = list(
    cook = function(time_seconds) {
      super$cook(time_seconds)
      message("Enjoy your dinner!")
    }
  )
)

# Instantiate a fancy microwave
a_fancy_microwave <- fancy_microwave_oven_factory$new()

# Call the cook() method
a_fancy_microwave$cook(1)



#Access grand parent suepr

# Expose the parent functionality
fancy_microwave_oven_factory <- R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory,
  public = list(
    cook_baked_potato = function() {
      self$cook(3)
    },
    cook = function(time_seconds) {
      super$cook(time_seconds)
      message("Enjoy your dinner!")
    }
  ),
  # Add an active element with a super_ binding
  
  active = list(
    super_ = function() super
  )
)

# Instantiate a fancy microwave
a_fancy_microwave <- fancy_microwave_oven_factory$new()

# Call the super_ binding
a_fancy_microwave$super_




# Explore other microwaves
microwave_oven_factory
fancy_microwave_oven_factory

# Define a high-end microwave oven class
high_end_microwave_oven_factory <- R6Class(
  "HighEndMicrowaveOven",
  inherit = fancy_microwave_oven_factory,
  public = list(
    cook = function(time_seconds) {
      super$super_$cook(time_seconds)
      message(ascii_pizza_slice)
    }
  )
)

# Instantiate a high-end microwave oven
a_high_end_microwave <- high_end_microwave_oven_factory$new()

# Use it to cook for one second
a_high_end_microwave$cook(1)


#static implementation with Env####
# Complete the class definition
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    shared = {
      # Create a new environment named e
      e <- new.env()
      # Assign safety_warning into e
      e$safety_warning <- "Warning. Do not try to cook metal objects."
      # Return e
      e
    }
  ),
  active = list(
    safety_warning = function(value) {
      if(missing(value)) {
        private$shared$safety_warning
      } else {
        private$shared$safety_warning <- value
=======
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
>>>>>>> no message
      }
    }
  )
)

<<<<<<< HEAD
# Create two microwave ovens
a_microwave_oven <- microwave_oven_factory$new()
another_microwave_oven <- microwave_oven_factory$new()

# Change the safety warning for a_microwave_oven
a_microwave_oven$safety_warning <- "Warning. If the food is too hot you may scald yourself."

# Verify that the warning has change for another_microwave
another_microwave_oven$safety_warning



# Microwave_factory is pre-defined
microwave_oven_factory

# Complete the class definition
smart_microwave_oven_factory <- R6Class(
  "SmartMicrowaveOven",
  inherit = microwave_oven_factory, # Specify inheritance
  private = list(
    # Add a field to store connection
    conn = NULL 
  ),
  public = list(
    initialize = function() {
      # Connect to the database
      private$conn <- dbConnect(SQLite(), "cooking-times.sqlite")
    },
    get_cooking_time = function(food) {
      dbGetQuery(
        private$conn,
        sprintf("SELECT time_seconds FROM cooking_times WHERE food = '%s'", food)
      )
    },
    finalize = function() {
      # Print a message
      print("Disconnecting from the cooking times database.")
      # Disconnect from the database
      dbDisconnect(private$conn)
    }
  )
)

# Create a smart microwave object
a_smart_microwave <- smart_microwave_oven_factory$new()

# Call the get_cooking_time() method
a_smart_microwave$get_cooking_time("soup")

# Remove the smart microwave
rm(a_smart_microwave)

# Force garbage collection
gc()
=======
# Make a microwave
a_microwave_oven <- microwave_oven_factory$new(650,T)


# Create microwave oven object
a_microwave_oven <- microwave_oven_factory$new()

# Call cook method for 1 second
a_microwave_oven$cook(1)
>>>>>>> no message
