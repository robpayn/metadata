# Dependencies for ROxygen ####

#' @importFrom R6 R6Class
#'
#' @importFrom shiny wellPanel
#'
#' @importFrom htmltools h3

# Class MetadataPeople ####

#' @export
#'
#' @title
#'   Shiny interface for managing personnel on a data product
#'
#' @description
#'   A modular shiny interface for managing personnel on a data product
#'
MetadataPeople <- R6Class(
  classname = "MetadataPeople",
  public = list(

    creators = NULL,

    initialize = function(name = "people", templatePerson = NULL) {

      self$creators <- OrderedList$new(
        elementFactory = MetadataPersonFactory$new(
          templatePerson = templatePerson,
          role = "creator"
        ),
        name = sprintf("%s_creators", name),
        buttonAddLabel = "Add creator"
      );

    },

    createShinyUI = function(input, output, session) {

      return(
        list(
          h3("Creators"),
          do.call(
            wellPanel,
            self$creators$createShinyUI()
          )
        )
      )

    },

    createShinyServer = function(input, output, session) {

      self$creators$createShinyServer(input, output, session);

      invisible(self);

    }

  )
)
