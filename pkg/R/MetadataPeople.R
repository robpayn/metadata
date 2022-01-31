# Dependencies for ROxygen ####

#' @importFrom R6 R6Class
#'
#' @importFrom shiny wellPanel
#' @importFrom shiny tags

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

    name = NULL,

    creators = NULL,
    contact = NULL,
    others = NULL,
    funding = NULL,

    initialize = function(
      name = "people",
      templatePerson = NULL,
      templateFunding = NULL,
      copy = NULL
    )
    {

      self$name <- name;

      self$creators <- OrderedListPeople$new(
        name = sprintf("%s_creators", name),
        buttonAddLabel = "Add creator",
        templatePerson = templatePerson,
        role = "creator",
        copy = copy$creators
      );

      self$contact <- MetadataPerson$new(
        templatePerson = templatePerson,
        name = sprintf("%s_contact", name),
        role = "contact",
        copy = copy$contact
      );

      self$others <- OrderedListPeople$new(
        name = sprintf("%s_others", name),
        templatePerson = templatePerson,
        copy = copy$others
      );

      self$funding <- OrderedListPeople$new(
        name = sprintf("%s_funding", name),
        buttonAddLabel = "Add funding PI",
        templatePerson = templateFunding,
        role = "PI",
        funding = TRUE,
        copy = copy$funding
      );

    },

    copyData = function(metadataPeople) {

      self$creators$copyData(metadataPeople$creators);
      self$contact$copyData(metadataPeople$contact);
      self$others$copyData(metadataPeople$others);
      self$funding$copyData(metadataPeople$funding);

      invisible(self);

    },

    updateShinyUI = function() {

      self$creators$updateShinyUI();
      self$contact$updateShinyUI();
      self$others$updateShinyUI();
      self$funding$updateShinyUI();

      invisible(self);

    },

    createShinyUI = function() {

      return(

        list(

          tags$h3("Creators"),
          do.call(
            what = wellPanel,
            args = self$creators$createShinyUI()
          ),

          tags$h3("Primary contact"),
          do.call(
            what = wellPanel,
            args = self$contact$createShinyUI()
          ),

          tags$h3("Other personnel"),
          do.call(
            what = wellPanel,
            args = self$others$createShinyUI()
          ),

          tags$h3("Lead PIs of funding sources"),
          do.call(
            what = wellPanel,
            args = self$funding$createShinyUI()
          )

        )

      )

    },

    createShinyServer = function(input, output, session) {

      self$creators$createShinyServer(input, output, session);

      self$contact$createShinyServer(input, output, session);

      self$others$createShinyServer(input, output, session);

      self$funding$createShinyServer(input, output, session);

      invisible(self);

    }

  )
)
