# Dependencies for ROxygen ####

#' @importFrom R6 R6Class
#'
#' @importFrom shiny selectInput
#' @importFrom shiny actionButton
#' @importFrom shiny textInput
#' @importFrom shiny updateTextInput
#' @importFrom shiny observeEvent
#' @importFrom shiny tags

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
    idTextProject = NULL,
    idTextAgency = NULL,
    idTextAwardNumber = NULL,

    givenName = as.character(NA),
    middleName = as.character(NA),
    surname = as.character(NA),
    org = as.character(NA),
    email = as.character(NA),
    orcid = as.character(NA),
    role = as.character(NA),
    project = as.character(NA),
    agency = as.character(NA),
    awardNumber = as.character(NA),

    roleDefined = as.logical(NA),
    funding = as.logical(NA),

    initialize = function(
      name,
      personInfo = NULL,
      templatePerson = NULL,
      role = NULL,
      funding = FALSE
    )
    {

      self$templatePerson <- templatePerson

      self$idSelectUniqueID <- sprintf("%s_selectUniqueID", name)
      self$idButtonApplyTemplate <- sprintf("%s_buttonApplyTemplate", name)

      self$idTextGivenName <- sprintf("%s_textGivenName", name)
      self$idTextMiddleName <- sprintf("%s_textMiddleName", name)
      self$idTextSurname <- sprintf("%s_textSurname", name)
      self$idTextOrg <- sprintf("%s_textOrg", name)
      self$idTextEmail <- sprintf("%s_textEmail", name)
      self$idTextORCID <- sprintf("%s_textORCID", name)
      self$idTextRole <- sprintf("%s_textRole", name)
      self$idTextProject = sprintf("%s_textProject", name)
      self$idTextAgency = sprintf("%s_textAgency", name)
      self$idTextAwardNumber = sprintf("%s_textAwardNumber", name)

      self$roleDefined <- !is.null(role)
      if (self$roleDefined) {
        self$role <- role
      }

      self$funding <- funding

      if (!is.null(personInfo)) {
        self$copyPersonInfo(personInfo = personInfo)
      }

    },

    getEmptyDataFrame = function() {

      return(
        data.frame(
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
      )

    },

    getPersonInfo = function() {

      return(
        list(
          givenName = self$givenName,
          middleName = self$middleName,
          surname = self$surname,
          org = self$org,
          email = self$email,
          orcid = self$orcid,
          role = self$role,
          project = self$project,
          agency = self$agency,
          awardNumber = self$awardNumber,
          roleDefined = as.integer(self$roleDefined),
          funding = as.integer(self$funding)
        )
      )

    },

    copyPersonInfo = function(personInfo) {

      self$givenName <- personInfo$givenName
      self$middleName <- personInfo$middleName
      self$surname <- personInfo$surname
      self$org <- personInfo$org
      self$email <- personInfo$email
      self$orcid <- personInfo$orcid
      self$role <- personInfo$role
      self$project <- personInfo$project
      self$agency <- personInfo$agency
      self$awardNumber <- personInfo$awardNumber

      self$roleDefined <- as.logical(personInfo$roleDefined)
      self$funding <- as.logical(personInfo$funding)

      invisible(self)

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

      if (!self$roleDefined) {
        fields[[length(fields) + 1]] <- textInput(
          inputId = self$idTextRole,
          label = "Role",
          value = self$role,
          width = "100%"
        );
      }

      if (self$funding) {
        fields <- c(
          fields,
          list(
            textInput(
              inputId = self$idTextProject,
              label = "Project",
              value = self$project,
              width = "100%"
            ),
            textInput(
              inputId = self$idTextAgency,
              label = "Agency",
              value = self$agency,
              width = "100%"
            ),
            textInput(
              inputId = self$idTextAwardNumber,
              label = "Award number",
              value = self$awardNumber,
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
              choices = c(
                "(Clear)",
                self$templatePerson$getPeopleIDs()
              )
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

      if (!self$roleDefined) {
        updateTextInput(
          session = self$session,
          inputId = self$idTextRole,
          value = self$role
        );
      }

      if (self$funding) {

        updateTextInput(
          session = self$session,
          inputId = self$idTextProject,
          value = self$project
        );

        updateTextInput(
          session = self$session,
          inputId = self$idTextAgency,
          value = self$agency
        );

        updateTextInput(
          session = self$session,
          inputId = self$idTextAwardNumber,
          value = self$awardNumber
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
        },
        ignoreInit = TRUE
      );

      observeEvent(
        eventExpr = input[[self$idTextMiddleName]],
        handlerExpr = {
          self$middleName <- input[[self$idTextMiddleName]];
        },
        ignoreInit = TRUE
      );

      observeEvent(
        eventExpr = input[[self$idTextSurname]],
        handlerExpr = {
          self$surname <- input[[self$idTextSurname]];
        },
        ignoreInit = TRUE
      );

      observeEvent(
        eventExpr = input[[self$idTextOrg]],
        handlerExpr = {
          self$org <- input[[self$idTextOrg]];
        },
        ignoreInit = TRUE
      );

      observeEvent(
        eventExpr = input[[self$idTextEmail]],
        handlerExpr = {
          self$email <- input[[self$idTextEmail]];
        },
        ignoreInit = TRUE
      );

      observeEvent(
        eventExpr = input[[self$idTextORCID]],
        handlerExpr = {
          self$orcid <- input[[self$idTextORCID]];
        },
        ignoreInit = TRUE
      );

      if (!self$roleDefined) {
        observeEvent(
          eventExpr = input[[self$idTextRole]],
          handlerExpr = {
            self$role <- input[[self$idTextRole]];
          },
          ignoreInit = TRUE
        );
      }

      if (self$funding) {

        observeEvent(
          eventExpr = input[[self$idTextProject]],
          handlerExpr = {
            self$project <- input[[self$idTextProject]];
          },
          ignoreInit = TRUE
        );

        observeEvent(
          eventExpr = input[[self$idTextAgency]],
          handlerExpr = {
            self$agency <- input[[self$idTextAgency]];
          },
          ignoreInit = TRUE
        );

        observeEvent(
          eventExpr = input[[self$idTextAwardNumber]],
          handlerExpr = {
            self$awardNumber <- input[[self$idTextAwardNumber]];
          },
          ignoreInit = TRUE
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

            if (self$funding) {
              self$project <- person$projectTitle;
              self$agency <- person$fundingAgency;
              self$awardNumber <- person$fundingNumber;
            }

            self$updateShinyUI();
          },
          ignoreInit = TRUE
        );
      }

      invisible(self);

    }

  )
);
