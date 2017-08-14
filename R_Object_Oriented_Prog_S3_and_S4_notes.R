# Course Description

# Object-oriented programming (OOP) lets you specify relationships between
# functions and the objects that they can act on, helping you manage complexity
# in your code. This is an intermediate level course, providing an introduction
# to OOP, using the S3 and R6 systems. S3 is a great day-to-day R programming
# tool that simplifies some of the functions that you write. R6 is especially
# useful for industry-specific analyses, working with web APIs, and building
# GUIs. The course concludes with an interview with Winston Chang, creator of
# the R6 package.

#Packages

install.packages("pryr")
install.packages("R6")
install.packages("assertive.types")
install.packages("assertive.numbers")

#Library
library(pryr)
library(R6)
library(assertive.types)
library(assertive.numbers)

#-------------------------------------------------------------> Introduction to Object-Oriented Programming

# Learn what object-oriented programming (OOP) consists of, when to use it, and
# what OOP systems are available in R. You'll also learn how R identifies
# different types of variable, using classes, types, and modes.

#--------------------------------------> You've Already Been Working With Objects

# Create these variables
a_numeric_vector <- rlnorm(50)
a_factor <- factor(
  sample(c(LETTERS[1:5], NA), 50, replace = TRUE)
)
a_data_frame <- data.frame(
  n = a_numeric_vector,
  f = a_factor
)
a_linear_model <- lm(dist ~ speed, cars)

# Call summary() on the numeric vector
summary(a_numeric_vector)

# Do the same for the other three objects
summary(a_factor)

summary(a_data_frame)

summary(a_linear_model)


#--------------------------------------> What's my type?

type_info <- function(x)
{
  c(
    class = class(x),
    typeof = typeof(x),
    mode = mode(x),
    storage.mode = storage.mode(x)
  )
}



# Look at the definition of type_info()
type_info

# Create list of example variables
some_vars <- list(
  an_integer_vector = rpois(24, lambda = 5),
  a_numeric_vector = rbeta(24, shape1 = 1, shape2 = 1),
  an_integer_array = array(rbinom(24, size = 8, prob = 0.5), dim = c(2, 3, 4)),
  a_numeric_array = array(rweibull(24, shape = 1, scale = 1), dim = c(2, 3, 4)),
  a_data_frame = data.frame(int = rgeom(24, prob = 0.5), num = runif(24)),
  a_factor = factor(month.abb),
  a_formula = y ~ x,
  a_closure_function = mean,
  a_builtin_function = length,
  a_special_function = `if`
)

# Loop over some_vars calling type_info() on each element to explore them
lapply(some_vars, type_info)


#-------------------------------------->Make it Classy (1)

chess <- list(white = list(    king = "g1",    queen = "h4",    bishops = c("c2", "g5"),
                               knights = character(),    rooks = c("f1", "f6"),    pawns = c("a2", "b2", "d4", "e3", "g2", "h2")),
              black = list(    king = "g8",    queen = "d7",    bishops = c("b7", "e7"),
                               knights = character(),    rooks = c("a6", "f8"),    pawns = c("a5", "c3", "c4", "d5", "f7", "g6")  
                               )
)
# -------------------

# Explore the structure of chess
str(chess)

# Override the class of chess
class(chess) <- "chess_game"

# Is chess still a list?
is.list(chess)

# How many pieces are left on the board?
length(unlist(chess))

#-------------------------------------------------------------> Using S3

# S3is a very simple object-oriented system that lets you define different
# behavior for functions, depending upon their input argument. This chapter
# explains how to use S3, and how generics and methods work.

#--------------------------------------> What's in a Name?
library(pryr)
is_s3_generic("t")           # generic transpose function
is_s3_method("t.data.frame") # transpose method for data.frames
is_s3_method("t.test")       # a function for Student's t-tests 

#--------------------------------------> Creating a Generic Function

# Create get_n_elements
get_n_elements <- function(x, ...){
  UseMethod("get_n_elements")
}

#--------------------------------------> Creating an S3 Method (1)

# View get_n_elements
get_n_elements

# Create a data.frame method for get_n_elements
get_n_elements.data.frame <- function(x, ...){
  nrow(x) * ncol(x)
}

# Call the method on the sleep dataset
n_elements_sleep <- get_n_elements.data.frame(sleep) 

# View the result
n_elements_sleep

#--------------------------------------> Creating an S3 method (2)

# View pre-defined objects
#ls.str()

# Create a default method for get_n_elements
get_n_elements.default <- function(x,...){
  length(unlist(x))
}

# Call the method on the ability.cov dataset
n_elements_ability.cov <- get_n_elements.default(ability.cov)

#--------------------------------------> Finding Available Methods (1)

# Find methods for print
methods(print)

#--------------------------------------> Method Lookup for Primitive Generics

hair <- list(colors = c("black", "brown","blonde", "ginger", "grey"),
          styles = c("afro", "beehive", "crew cut", "mohawk", "mullet", "pony tail", "quiff"))

class(hair) <- "hairstylist"

# View the structure of hair
str(hair)

# What primitive generics are available?
.S3PrimitiveGenerics

# Does length.hairstylist exist?
exists("length.hairstylist")

# What is the length of hair?
length(hair)

#--------------------------------------> Very Classy

kitty <- "Miaow!"

# View the kitty
kitty

# Assign classes
class(kitty) <- c("cat", "mammal", "character")

# Does kitty inherit from cat/mammal/character vector?
inherits(kitty, "cat" )
inherits(kitty, "mammal" )
inherits(kitty, "character" )

# Is kitty a character vector?
is.character(kitty)

# Does kitty inherit from dog?
inherits(kitty, "dog")

#--------------------------------------> Writing the Next Method

what_am_i <- function(x, ...){
  UseMethod("what_am_i")
  }

# cat method
what_am_i.cat <- function(x, ...)
{
  # Write a message
  message("I'm a cat")
  # Call NextMethod
  NextMethod("what_am_i")
}

# mammal method
what_am_i.mammal <- function(x,...)
{
  # Write a message
  message("I'm a mammal")
  # Call NextMethod
  NextMethod("what_am_i")
}

# character method
what_am_i.character <- function(x,...)
{
  # Write a message
  message("I'm a character vector")
}

# Call what_am_i()
what_am_i(kitty)

#-------------------------------------------------------------> Using R6 

# Learn how to define R6 classes, and to create R6 objects. You'll also
# learn about the structure of R6 classes, and how to separate the user
# interface from the implementation details. 


#--------------------------------------> Specifying the Microwave Oven Class

# Define microwave_oven_factory
microwave_oven_factory <- R6Class(
  "MicrowaveOven", private = list(
    power_ratings_watts = 800))

#--------------------------------------> Making Microwave Ovens

# View the microwave_oven_factory
microwave_oven_factory

# Make a new microwave oven
microwave_oven <- microwave_oven_factory$new() 

#--------------------------------------> Learning to Cook

# Add a cook method to the factory definition
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800
  ),
  public = list(
    cook = function(time_seconds){
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    }
  )
)

# Create microwave oven object
a_microwave_oven <- microwave_oven_factory$new()

# Call cook method for 1 second
a_microwave_oven$cook(1)

#--------------------------------------> Close the Door

# Add a close_door() method
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800,
    door_is_open = FALSE
  ),
  public = list(
    cook = function(time_seconds) {
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    },
    open_door = function() {
      private$door_is_open = TRUE
    },
    close_door = function() {
      private$door_is_open = FALSE
    }
    
    
  )
)

#-------------------------------------->First Thing's First

# Add an initialize method
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800,
    door_is_open = FALSE
  ),
  public = list(
    cook = function(time_seconds) {
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    },
    open_door = function() {
      private$door_is_open = TRUE
    },
    close_door = function() {
      private$door_is_open = FALSE
    },
    # Add initialize() method here
    initialize = function(power_rating_watts, door_is_open) {
      if (!missing(power_rating_watts)) {
        private$power_rating_watts <- power_rating_watts
      }
      if (!missing(door_is_open)) {
        private$door_is_open <- door_is_open
      }
    }
    
    
  )
)

# Make a microwave
a_microwave_oven <- microwave_oven_factory$new(
  power_rating_watts = 650,
  door_is_open = TRUE
)


#--------------------------------------> Read the Rating

# Add a binding for power rating
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    ..power_rating_watts = 800
  ),
  active = list(
    # add the binding here
    power_rating_watts = function() {
      private$..power_rating_watts
    }
    
  )
)

# Make a microwave 
a_microwave_oven <- microwave_oven_factory$new() 

# Get the power rating

a_microwave_oven$power_rating_watts


#--------------------------------------> Control the Power

# Add a binding for power rating
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    ..power_rating_watts = 800,
    ..power_level_watts = 800
  ),
  # Add active list containing an active binding
  active = list(
    power_level_watts = function(value) {
      if (missing(value)) {
        private$..power_level_watts
      }
      else {
        assert_is_a_number(value)
        assert_all_are_in_closed_range(value, 0, private$..power_rating_watts)
        private$..power_level_watts <- value
      }
    }
  )
)
# Make a microwave 
a_microwave_oven <- microwave_oven_factory$new()

# Get the power level
a_microwave_oven$power_level_watts 

# Try to set the power level to "400"
a_microwave_oven$power_level_watts <- "400"

# Try to set the power level to 1600 watts
a_microwave_oven$power_level_watts <- 1600

# Set the power level to 400 watts
a_microwave_oven$power_level_watts <- 400



#-------------------------------------------------------------> Using R6Learn how to define R6


# classes, and to create R6 objects. You'll also learn about the structure of R6
# classes, and how to separate the user interface from the implementation
# details. 

#--------------------------------------> Specifying a Fancy Microwave Oven

# Explore the microwave oven class
microwave_oven_factory

# Define a fancy microwave class inheriting from microwave oven
fancy_microwave_oven_factory <- R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory
) 


#--------------------------------------> Making a Fancy Microwave Oven

#PRECODE
library(R6)
library(assertive.types)
library(assertive.numbers)

microwave_oven_factory <- R6Class("MicrowaveOven",
                                  private = list(
                                    ..power_rating_watts = 800,
                                    ..power_level_watts = 800,
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
                                    }
                                  ),
                                  active = list(
                                    power_rating_watts = function() {
                                      private$..power_rating_watts
                                    },
                                    power_level_watts = function(value) {
                                      if (missing(value)) {
                                        private$..power_level_watts
                                      } else {
                                        assert_is_a_number(value)
                                        assert_all_are_in_closed_range(
                                          value, 0, private$..power_rating_watts
                                        )
                                        private$..power_level_watts <- value
                                      }
                                    }
                                  )
)                                  

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
  active = list(
    super_ = function() super
  )
)

# -----------------------------------------
                        
                                                
# Explore microwave oven classes
microwave_oven_factory
fancy_microwave_oven_factory

# Instantiate both types of microwave
a_microwave_oven <- microwave_oven_factory$new()
a_fancy_microwave <- fancy_microwave_oven_factory$new()

# Get power rating for each microwave
microwave_power_rating <- a_microwave_oven$power_rating_watts 
fancy_microwave_power_rating <- a_fancy_microwave$power_rating_watts

# Verify that these are the same
identical(microwave_power_rating, a_fancy_microwave$power_rating_watts)

# Cook with each microwave
a_microwave_oven$cook(1)
a_fancy_microwave$cook(1)

#--------------------------------------> Extending the Cooking Capabilities

# Explore microwave oven class
microwave_oven_factory

# Extend the class definition
fancy_microwave_oven_factory <- R6Class(  
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory,
  # Add a public list with a cook baked potato method
  public = list(
    cook_baked_potato = function() {
      self$cook(3)
    }
  )
)
# Instantiate a fancy microwave
a_fancy_microwave <- fancy_microwave_oven_factory$new()
# Call the cook_baked_potato() method
a_fancy_microwave$cook_baked_potato()

#--------------------------------------> Overriding the Cooking Capabilities

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


#--------------------------------------> Exposing your Parent

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

#--------------------------------------> Over-Overriding the Cooking Capabilities
library(R6)
library(assertive.types)
library(assertive.numbers)
microwave_oven_factory <- R6Class("MicrowaveOven",
                                  private = list(
                                    ..power_rating_watts = 800,
                                    ..power_level_watts = 800,
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
                                      }
                                    ),
                                  active = list(
                                    power_rating_watts = function() {
                                      private$..power_rating_watts
                                      },
                                    power_level_watts = function(value) {
                                      if (missing(value)) {
                                        private$..power_level_watts
                                        } else {
                                          assert_is_a_number(value)
                                          assert_all_are_in_closed_range(
                                            value, 0, private$..power_rating_watts
                                            )
                                          private$..power_level_watts <- value
                                          }
                                      }
                                    )
)                                  

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
                                    active = list(
                                       super_ = function() super
                                     )
                              )


ascii_pizza_slice <- "   __
\n // \"\"--.._
\n||  (_)  _ \"-._
\n||    _ (_)    '-.
\n||   (_)   __..-'
\n \\\\__..--\"\""



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


#-------------------------------------------------------------> Advanced R6

# UsageComplete your mastery of R6 by learning about advanced topics such as
# copying by reference, shared fields, cloning objects, and finalizing objects.
# The chapter concludes with an interview with Winston Chang, creator of the R6
# package.

#-------------------------------------->
#-------------------------------------->
#-------------------------------------->
#-------------------------------------->
#-------------------------------------->
#-------------------------------------->