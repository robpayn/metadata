# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class OrderedListPeople ####

#' @export
#'
#' @title
#'   An ordered list of people
#'
#' @description
#'   An ordered list of people
#'
OrderedListPeople <- R6Class(
  classname = "OrderedListPeople",
  inherit = OrderedList,
  public = list(

    templatePerson = NULL,
    role = NULL,
    funding = NULL,

    initialize = function(
      name,
      buttonAddLabel = "Add person",
      templatePerson = NULL,
      role = NULL,
      funding = FALSE,
      copy = NULL
    )
    {

      super$initialize(
        name = name,
        buttonAddLabel = buttonAddLabel
      );

      self$templatePerson <- templatePerson;
      self$role <- role;
      self$funding <- funding;

      if (!is.null(copy)) {
        self$copyData(copy);
      }

    },

    createElement = function(name, copy = NULL) {

      return(

        OrderedListElement$new(
          name = name,
          content = MetadataPerson$new(
            name = name,
            templatePerson = self$templatePerson,
            role = self$role,
            funding = self$funding,
            copy = copy$content
          ),
          orderedList = self
        )

      );

    }

  )
)
