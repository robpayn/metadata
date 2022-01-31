# Dependencies for ROxygen ####

#' @importFrom R6 R6Class
#'
#' @importFrom shiny textInput
#' @importFrom shiny updateTextInput
#' @importFrom shiny textAreaInput
#' @importFrom shiny updateTextAreaInput
#' @importFrom shiny fileInput
#' @importFrom shiny observeEvent
#' @importFrom shiny tags

# Class MetadataNarrative ####

#' @export
#'
#' @title
#'   Shiny interface for managing narrative metadata for a data product
#'
#' @description
#'   A modular shiny interface for managing narrative metadata for a
#'   data product
#'
MetadataNarrative <- R6Class(
  classname = "MetadataNarrative",
  public = list(

    input = NULL,
    output = NULL,
    session = NULL,

    name = NULL,

    idTextTitle = NULL,
    title = "Replace this with the title of the data product",

    idTextAbstract = NULL,
    abstract = "Replace this with the abstract for the data product",
    idButtonImportAbstract = NULL,

    idTextPurpose = NULL,
    purpose = "Replace this with the purpose of generating the data product",
    idButtonImportPurpose = NULL,

    idTextIntellectualRights = NULL,
    intellectualRights =
      "Replace this with the intellectual rights to the data product",
    idButtonImportIntellectualRights = NULL,

    idTextMethods = NULL,
    methods = "Replace this with the methods used to generate the data product",
    idButtonImportMethods = NULL,

    initialize = function(
      name = "narrative",
      copy = NULL
    )
    {

      self$name <- name;

      self$idTextTitle <- sprintf("%s_textTitle", self$name);

      self$idTextAbstract <- sprintf("%s_textAbstract", self$name);
      self$idButtonImportAbstract <-
        sprintf("%s_buttonImportAbstract", self$name);

      self$idTextPurpose <- sprintf("%s_textPurpose", self$name);
      self$idButtonImportPurpose <-
        sprintf("%s_buttonImportPurpose", self$name);

      self$idTextIntellectualRights <-
        sprintf("%s_textIntellectualRights", self$name);
      self$idButtonImportIntellectualRights <-
        sprintf("%s_buttonImportIntellectualRights", self$name);

      self$idTextMethods <- sprintf("%s_textMethods", self$name);
      self$idButtonImportMethods <-
        sprintf("%s_buttonImportMethods", self$name);

      if (!is.null(copy)) {
        self$copyData(copy);
      }

    },

    copyData = function(metadataNarrative) {

      self$title <- metadataNarrative$title;
      self$abstract <- metadataNarrative$abstract;
      self$purpose <- metadataNarrative$purpose;
      self$intellectualRights <- metadataNarrative$intellectualRights;
      self$methods <- metadataNarrative$methods;

      invisible(self);

    },

    createShinyUI = function() {

      return(

        list(

          textAreaInput(
            inputId = self$idTextTitle,
            label = "Title",
            value = self$title,
            width = "100%",
            rows = 2,
            resize = "vertical"
          ),

          textAreaInput(
            inputId = self$idTextAbstract,
            label = "Abstract",
            value = self$abstract,
            width = "100%",
            rows = 5,
            resize = "vertical"
          ),
          fileInput(
            inputId = self$idButtonImportAbstract,
            multiple = FALSE,
            label = NULL,
            buttonLabel = "Import abstract text file",
            width = "100%"
          ),

          textAreaInput(
            inputId = self$idTextPurpose,
            label = "Purpose",
            value = self$purpose,
            width = "100%",
            rows = 5,
            resize = "vertical"
          ),
          fileInput(
            inputId = self$idButtonImportPurpose,
            multiple = FALSE,
            label = NULL,
            buttonLabel = "Import purpose text file",
            width = "100%"
          ),

          textAreaInput(
            inputId = self$idTextIntellectualRights,
            label = "Intellectual Rights",
            value = self$intellectualRights,
            width = "100%",
            rows = 5,
            resize = "vertical"
          ),
          fileInput(
            inputId = self$idButtonImportIntellectualRights,
            multiple = FALSE,
            label = NULL,
            buttonLabel = "Import rights text file",
            width = "100%"
          ),

          textAreaInput(
            inputId = self$idTextMethods,
            label = "Methods",
            value = self$methods,
            width = "100%",
            rows = 10,
            resize = "vertical"
          ),
          fileInput(
            inputId = self$idButtonImportMethods,
            multiple = FALSE,
            label = NULL,
            buttonLabel = "Import methods text file",
            width = "100%"
          )

        )

      )

    },

    createShinyServer = function(input, output, session) {

      self$input <- input;
      self$output <- output;
      self$session <- session;

      observeEvent(
        eventExpr = input[[self$idTextTitle]],
        {
          self$title <- input[[self$idTextTitle]];
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextAbstract]],
        {
          self$abstract <- input[[self$idTextAbstract]];
        }
      );
      observeEvent(
        eventExpr = input[[self$idButtonImportAbstract]],
        {
          filePath <- input[[self$idButtonImportAbstract]]$datapath[1];
          self$abstract <- readChar(
            con = filePath,
            nchars = file.info(filePath)$size
          );
          updateTextAreaInput(
            session = session,
            inputId = self$idTextAbstract,
            value = self$abstract
          );
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextPurpose]],
        {
          self$purpose <- input[[self$idTextPurpose]];
        }
      );
      observeEvent(
        eventExpr = input[[self$idButtonImportPurpose]],
        {
          filePath <- input[[self$idButtonImportPurpose]]$datapath[1];
          self$purpose <- readChar(
            con = filePath,
            nchars = file.info(filePath)$size
          );
          updateTextAreaInput(
            session = session,
            inputId = self$idTextPurpose,
            value = self$purpose
          );
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextIntellectualRights]],
        {
          self$intellectualRights <- input[[self$idTextIntellectualRights]];
        }
      );
      observeEvent(
        eventExpr = input[[self$idButtonImportIntellectualRights]],
        {
          filePath <-
            input[[self$idButtonImportIntellectualRights]]$datapath[1];
          self$intellectualRights <- readChar(
            con = filePath,
            nchars = file.info(filePath)$size
          );
          updateTextAreaInput(
            session = session,
            inputId = self$idTextIntellectualRights,
            value = self$intellectualRights
          );
        }
      );

      observeEvent(
        eventExpr = input[[self$idTextMethods]],
        {
          self$methods <- input[[self$idTextMethods]];
        }
      );
      observeEvent(
        eventExpr = input[[self$idButtonImportMethods]],
        handlerExpr = {
          filePath <- input[[self$idButtonImportMethods]]$datapath[1];
          self$methods <- readChar(
            con = filePath,
            nchars = file.info(filePath)$size
          );
          updateTextAreaInput(
            session = session,
            inputId = self$idTextMethods,
            value = self$methods
          );
        }
      );

      invisible(self);

    },

    updateShinyUI = function() {

      updateTextAreaInput(
        session = self$session,
        inputId = self$idTextTitle,
        value = ifelse(
          is.null(self$title),
          "",
          self$title
        )
      );

      updateTextAreaInput(
        session = self$session,
        inputId = self$idTextAbstract,
        value = ifelse(
          is.null(self$abstract),
          "",
          self$abstract
        )
      );

      updateTextAreaInput(
        session = self$session,
        inputId = self$idTextPurpose,
        value = ifelse(
          is.null(self$purpose),
          "",
          self$purpose
        )
      );

      updateTextAreaInput(
        session = self$session,
        inputId = self$idTextIntellectualRights,
        value = ifelse(
          is.null(self$intellectualRights),
          "",
          self$intellectualRights
        )
      );

      updateTextAreaInput(
        session = self$session,
        inputId = self$idTextMethods,
        value = ifelse(
          is.null(self$methods),
          "",
          self$methods
        )
      );

      updateTextInput(
        session = self$session,
        inputId = self$idTextOutputFile,
        value = ifelse(
          is.null(self$outputRData),
          "",
          self$outputRData
        )
      )

      invisible(self);

    }

  )
)
