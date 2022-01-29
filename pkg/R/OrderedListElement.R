# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class OrderedListElement ####

#' @export
#'
#' @title
#'   An element of an ordered list
#'
#' @description
#'   A base class for tracking an element of an ordered list
#'
OrderedListElement <- R6Class(
  classname = "OrderedListElement",
  public = list(

    name = NULL,

    obsDelete = NULL,
    obsMoveup = NULL,
    obsMovedown = NULL,

    idButtonDelete = NULL,
    idButtonMoveup = NULL,
    idButtonMovedown = NULL,

    initialize = function(name) {

      self$name <- name;

      self$idButtonDelete <- sprintf("%s_buttonDelete", name);
      self$idButtonMoveup <- sprintf("%s_buttonMoveup", name);
      self$idButtonMovedown <- sprintf("%s_buttonMovedown", name);

    }

  )
)
