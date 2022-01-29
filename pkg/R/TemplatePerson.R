# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class TemplatePerson ####

#' @export
#'
#' @title
#'   A template for a person element
#'
#' @description
#'   A template for a person element
#'
TemplatePerson <- R6Class(
  classname = "TemplatePerson",
  public = list(

    people = NULL,

    initialize = function(people) {

      self$people <- people;

    },

    getPeopleIDs = function() {

      return( people$internalUniqueID );

    },

    getPersonInfo = function(personID) {

      return( people[personID, ] );

    }

  )
)
