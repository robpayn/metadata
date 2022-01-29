# Dependencies for ROxygen ####

#' @importFrom R6 R6Class
#'
#' @importFrom shiny selectInput
#' @importFrom shiny actionButton
#' @importFrom shiny textInput
#' @importFrom shiny updateTextInput
#' @importFrom shiny observeEvent

# Class MetadataPerson ####

#' @export
#'
#' @title
#'   Shiny interface for managing metadata for a person
#'
#' @description
#'   A modular shiny interface for managing metdata for a person
#'
MetadataPerson <- R6Class(
  classname = "MetadataPerson",
  inherit = OrderedListElement,
  public = list(

    input = NULL,
    output = NULL,
    session = NULL,

    templatePerson = NULL,

    idSelectUniqueID = NULL,
    idButtonApplyTemplate = NULL,

    idTextGivenName = NULL,
    idTextMiddleName = NULL,
    idTextSurname = NULL,
    idTextOrg = NULL,
    idTextEmail = NULL,
    idTextORCID = NULL,
    idTextRole = NULL,

    givenName = NULL,
    middleName = NULL,
    surname = NULL,
    org = NULL,
    email = NULL,
    orcid = NULL,
    role = NULL,

    initialize = function(name, templatePerson = NULL, role = NULL) {

      super$initialize(
        name = name
      );

      self$templatePerson <- templatePerson;

      self$idSelectUniqueID <- sprintf("%s_selectUniqueID", name);
      self$idButtonApplyTemplate <- sprintf("%s_buttonApplyTemplate", name);

      self$idTextGivenName <- sprintf("%s_textGivenName", name);
      self$idTextMiddleName <- sprintf("%s_textMiddleName", name);
      self$idTextSurname <- sprintf("%s_textSurname", name);
      self$idTextOrg <- sprintf("%s_textOrg", name);
      self$idTextEmail <- sprintf("%s_textEmail", name);
      self$idTextORCID <- sprintf("%s_textORCID", name)
      self$idTextRole <- sprintf("%s_textRole", name);

      if (!is.null(role)) {
        self$role <- role;
      }

    },

    createShinyUI = function() {

      fields <- list(
        textInput(
          inputId = self$idTextGivenName,
          label = "Given name",
          value = self$givenName,
          width = "100%"
        ),
        textInput(
          inputId = self$idTextMiddleName,
          label = "Middle name",
          value = self$middleName,
          width = "100%"
        ),
        textInput(
          inputId = self$idTextSurname,
          label = "Surname",
          value = self$surname,
          width = "100%"
        ),
        textInput(
          inputId = self$idTextOrg,
          label = "Organization",
          value = self$org,
          width = "100%"
        ),
        textInput(
          inputId = self$idTextEmail,
          label = "Email",
          value = self$email,
          width = "100%"
        ),
        textInput(
          inputId = self$idTextORCID,
          label = "ORCID",
          value = self$orcid,
          width = "100%"
        )
      );

      if (is.null(self$role)) {
        fields <- c(
          fields,
          list(
            textInput(
              inputId = self$idTextRole,
              label = "Role",
              value = self$role,
              width = "100%"
            )
          )
        )
      }

      if (!is.null(self$templatePerson)) {
        fields <- c(
          fields,
          list(
            selectInput(
              inputId = self$idSelectUniqueID,
              label = "Directory unique ID",
              choices = self$templatePerson$getPeopleIDs()
            ),
            actionButton(
              inputId = self$idButtonApplyTemplate,
              label = "Apply directory"
            )
          )
        );
      }

      return(fields);

    },

    updateShinyUI = function() {

      updateTextInput(
        session = self$session,
        inputId = self$idTextGivenName,
        value = self$givenName
      );

      updateTextInput(
        session = self$session,
        inputId = self$idTextMiddleName,
        value = self$middleName
      );

      updateTextInput(
        session = self$session,
        inputId = self$idTextSurname,
        value = self$surname
      );

      updateTextInput(
        session = self$session,
        inputId = self$idTextOrg,
        value = self$org
      );

      updateTextInput(
        session = self$session,
        inputId = self$idTextEmail,
        value = self$email
      );

      updateTextInput(
        session = self$session,
        inputId = self$idTextORCID,
        value = self$orcid
      );

      if (is.null(self$role)) {
        updateTextInput(
          session = self$session,
          inputId = self$idTextRole,
          value = self$role
        );
      }

      invisible(self);

    },

    createShinyServer = function(input, output, session) {

      self$input <- input;
      self$output <- output;
      self$session <- session;

      observeEvent(
        eventExpr = input[[self$idTextGivenName]],
        handlerExpr = {
          self$givenName <- input[[self$idTextGivenName]];
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextMiddleName]],
        handlerExpr = {
          self$middleName <- input[[self$idTextMiddleName]];
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextSurname]],
        handlerExpr = {
          self$surname <- input[[self$idTextSurname]];
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextOrg]],
        handlerExpr = {
          self$org <- input[[self$idTextOrg]];
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextEmail]],
        handlerExpr = {
          self$email <- input[[self$idTextEmail]];
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextORCID]],
        handlerExpr = {
          self$orcid <- input[[self$idTextORCID]];
        }
      );

      if (is.null(self$role)) {
        observeEvent(
          eventExpr = input[[self$idTextRole]],
          handlerExpr = {
            self$role<- input[[self$idTextRole]];
          }
        );
      }

      if (!is.null(self$templatePerson)) {
        observeEvent(
          eventExpr = input[[self$idButtonApplyTemplate]],
          handlerExpr = {
            person <- self$templatePerson$getPersonInfo(
              input[[self$idSelectUniqueID]]
            );

            self$givenName <- person$givenName;
            self$middleName <- person$middleInitial;
            self$surname <- person$surName;
            self$org <- person$organizationName;
            self$email <- person$electronicMailAddress;
            self$orcid <- person$ORCID;

            self$updateShinyUI();
          },
          ignoreInit = TRUE
        );
      }

      invisible(self);

    }

  )
);
