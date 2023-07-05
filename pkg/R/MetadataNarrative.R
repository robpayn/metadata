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

    path = NULL,
    name = NULL,
    fileNames = NULL,

    idTextTitle = NULL,
    title = NULL,

    idTextAbstract = NULL,
    abstract = NULL,
    idButtonImportAbstract = NULL,

    idTextPurpose = NULL,
    purpose = NULL,
    idButtonImportPurpose = NULL,

    idTextIntellectualRights = NULL,
    intellectualRights = NULL,
    idButtonImportIntellectualRights = NULL,

    idTextMethods = NULL,
    methods = NULL,
    idButtonImportMethods = NULL,

    initialize = function(
      name = "narrative",
      path = "./01_input/meta",
      fileNames = c(
        title = "narr_title.txt",
        abstract = "narr_abstract.md",
        purpose = "narr_purpose.md",
        rights = "narr_rights.md",
        methods = "narr_methods.md"
      )
    )
    {

      self$name <- name
      self$path <- path
      self$fileNames <- fileNames

      if (file.exists(self$path)) {
        self$read()
      } else {
        dir.create(path = self$path, recursive = TRUE)
      }

      self$idTextTitle <- sprintf("%s_textTitle", self$name)

      self$idTextAbstract <- sprintf("%s_textAbstract", self$name)
      self$idButtonImportAbstract <-
        sprintf("%s_buttonImportAbstract", self$name)

      self$idTextPurpose <- sprintf("%s_textPurpose", self$name)
      self$idButtonImportPurpose <-
        sprintf("%s_buttonImportPurpose", self$name)

      self$idTextIntellectualRights <-
        sprintf("%s_textIntellectualRights", self$name)
      self$idButtonImportIntellectualRights <-
        sprintf("%s_buttonImportIntellectualRights", self$name)

      self$idTextMethods <- sprintf("%s_textMethods", self$name)
      self$idButtonImportMethods <-
        sprintf("%s_buttonImportMethods", self$name)

    },

    read = function(path = self$path, fileNames = self$fileNames) {

      filePath <- sprintf("%s/%s", path, fileNames["title"])
      if (file.exists(filePath)) {
        self$title <-
          readChar(con = filePath, nchars = file.size(filePath))
      } else {
        self$title <-
          "Replace this with the title of the data product"
      }

      filePath <- sprintf("%s/%s", path, fileNames["abstract"])
      if (file.exists(filePath)) {
        self$abstract <-
          readChar(con = filePath, nchars = file.size(filePath))
      } else {
        self$abstract <-
          "Replace this with the abstract for the data product"
      }

      filePath <- sprintf("%s/%s", path, fileNames["purpose"])
      if (file.exists(filePath)) {
        self$purpose <-
          readChar(con = filePath, nchars = file.size(filePath))
      } else {
        self$purpose <-
          "Replace this with the purpose of generating the data product"
      }

      filePath <- sprintf("%s/%s", path, fileNames["rights"])
      if (file.exists(filePath)) {
        self$intellectualRights <-
          readChar(con = filePath, nchars = file.size(filePath))
      } else {
        self$intellectualRights <-
          "Replace this with the intellectual rights to the data product"
      }

      filePath <- sprintf("%s/%s", path, fileNames["methods"])
      if (file.exists(filePath)) {
        self$methods <-
          readChar(con = filePath, nchars = file.size(filePath))
      } else {
        self$methods <-
          "Replace this with the methods used to generate the data product"
      }

      invisible(self)

    },

    write = function(path = self$path, fileNames = self$fileNames) {

      filePath <- sprintf("%s/%s", path, fileNames["title"])
      write(x = self$title, file = filePath)

      filePath <- sprintf("%s/%s", path, fileNames["abstract"])
      write(x = self$abstract, file = filePath)

      filePath <- sprintf("%s/%s", path, fileNames["purpose"])
      write(x = self$purpose, file = filePath)

      filePath <- sprintf("%s/%s", path, fileNames["rights"])
      write(x = self$intellectualRights, file = filePath)

      filePath <- sprintf("%s/%s", path, fileNames["methods"])
      write(x = self$methods, file = filePath)

      invisible(self)

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
