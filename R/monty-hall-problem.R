#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Select a random door
#'
#' @description
#'   `select_door()` selects a random door of the three available
#'
#' @details
#' Contestant selects one of three available doors. The item behind
#' the selected door would be their prize.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a integer of the selected door
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Host opens unselected door with goat behind it
#'
#' @description
#'  `open_goat_door()` host opens door and reveals a goat.
#'
#' @details
#'    Host opens one of the two unselected doors revealing a goat.
#'
#' @param game A array of strings representing the current game.
#'
#' @param a.pick A number representing the picked door.
#'
#' @return A number representing a door with a goat behind it that is not the contestant picked door
#'
#' @examples
#'   open_goat_door( game, a.pick )
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Contestant can decide to change doors
#' @description
#'   `change_door()` contestant decides if they want to change
#'   their selected door to open
#'
#' @details
#'   The contestant can choose to change doors to the other unopened door,
#'   or keep the existing door they originally chose
#'
#' @param stay A boolean.
#' @param opened.door A number.
#' @param a.pick A number.
#'
#' @return A number representing the final door number to open in the game
#'
#' @examples
#'   change_door( stay=T, opened.door, a.pick )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title Determine if te contestant won
#' @description
#'   `determine_winner()` determines the winner of the game
#'
#' @details
#'   This function takes the final state of teh game and determines if the
#'   contestant won the car or not
#'
#' @param final.pick A number
#' @param game A array of strings representing the current game.
#' @return A string of either WIN or LOSE
#' @examples
#'   determine_winner()
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Play the game
#' @description
#'   `play_game()` starts the monty hall game
#' @details This function plays the game and calls all the necessary functions.
#'   At the end it will report if the player won or lost
#' @param ... no arguments are used by the function.
#' @return A string with the game result
#' @examples
#'   play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Play the game multiple times
#' @description
#'   `play_n_games()` Plays the game multiple times
#' @details This plays the game n number of specified times and stores the
#' results in a dataframe for later analysis
#' @param n A number
#' @return A Dataframe with the results of the n number of games
#' @examples
#'   play_n_games(n=50)
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
