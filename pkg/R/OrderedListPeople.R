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

    filePath = NULL,
    templatePerson = NULL,
    role = NULL,
    funding = NULL,

    initialize = function(
      name,
      buttonAddLabel = "Add person",
      filePath = NULL,
      templatePerson = NULL,
      role = NULL,
      funding = FALSE
    )
    {

      super$initialize(
        name = name,
        buttonAddLabel = buttonAddLabel
      )

      self$filePath <- filePath
      self$templatePerson <- templatePerson
      self$role <- role
      self$funding <- funding

      if (file.exists(self$filePath)) {
        self$read()
      }

    },

    read = function(filePath = self$filePath)
    {

      lapply(
        X = self$elements,
        FUN = function(element) {
          self$removeElement(element);
          element$removeShinyUI();
        }
      );

      df <- read.table(file = filePath, header = TRUE, sep = ",")

      if ( nrow(df) > 0 ) {
        for (index in 1:nrow(df)) {
          self$addElement(elementInfo = df[index,])
        }
      }

      invisible(self);

    },

    write = function(filePath = self$filePath)
    {
      rowCount <- length(self$elements)
      df <- data.frame(
        givenName = character(length = rowCount),
        middleName = character(length = rowCount),
        surname = character(length = rowCount),
        org = character(length = rowCount),
        email = character(length = rowCount),
        orcid = character(length = rowCount),
        role = character(length = rowCount),
        project = character(length = rowCount),
        agency = character(length = rowCount),
        awardNumber = character(length = rowCount),
        roleDefined = integer(length = rowCount),
        funding = integer(length = rowCount)
      )
      if (rowCount > 0) {
        for (index in 1:length(self$elements)) {
          df[index,] <- self$elements[[index]]$content$getPersonInfo()
        }
      }
      write.table(df, file = filePath, sep = ",", row.names = FALSE)
    },

    createElement = function(name, elementInfo = NULL)
    {

      return(

        OrderedListElement$new(
          name = name,
          content = MetadataPerson$new(
            name = name,
            personInfo = elementInfo,
            templatePerson = self$templatePerson,
            role = self$role,
            funding = self$funding
          ),
          orderedList = self
        )

      )

    }

  )
)
