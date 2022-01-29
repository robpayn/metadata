# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class MetadataPersonFactory ####

#' @export
#'
#' @title
#'   Factory for creating a person element
#'
#' @description
#'   Factory for creating a person element
#'
MetadataPersonFactory <- R6Class(
  classname = "MetadataPersonFactory",
  public = list(

    templatePerson = NULL,
    role = NULL,

    initialize = function(templatePerson, role = NULL) {

      self$templatePerson <- templatePerson;
      self$role <- role;

    },

    createElement = function(name) {

      return(
        MetadataPerson$new(
          name = name,
          templatePerson = self$templatePerson,
          role = self$role
        )
      )

    }

  )
)
