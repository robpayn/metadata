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

    path = NULL,
    name = NULL,
    fileNames = NULL,

    creators = NULL,
    creatorPath = NULL,
    contact = NULL,
    contactPath = NULL,
    others = NULL,
    otherPath = NULL,
    funding = NULL,
    fundingPath = NULL,

    initialize = function(
      name = "people",
      templatePerson = NULL,
      templateFunding = NULL,
      path = "./01_input/meta",
      fileNames = c(
        creators = "ppl_creators.csv",
        contact = "ppl_contact.csv",
        others = "ppl_others.csv",
        funding = "ppl_funding.csv"
      )
    )
    {

      self$name <- name;
      self$path <- path
      self$fileNames <- fileNames

      if (!file.exists(self$path)) {
        dir.create(path = self$path, recursive = TRUE)
      }

      self$creatorPath <-
        sprintf("%s/%s", self$path, self$fileNames["creators"])
      self$creators <- OrderedListPeople$new(
        name = sprintf("%s_creators", name),
        buttonAddLabel = "Add creator",
        filePath = self$creatorPath,
        templatePerson = templatePerson,
        role = "creator"
      )

      self$contactPath <-
        sprintf("%s/%s", self$path, self$fileNames["contact"])
      if (file.exists(self$contactPath)) {
        personInfo <-
          read.table(file = self$contactPath, header = TRUE, sep = ",")[1,]
      } else {
        personInfo <- NULL
      }
      self$contact <- MetadataPerson$new(
        name = sprintf("%s_contact", name),
        personInfo = personInfo,
        templatePerson = templatePerson,
        role = "contact"
      )

      self$otherPath <-
        sprintf("%s/%s", self$path, self$fileNames["others"])
      self$others <- OrderedListPeople$new(
        name = sprintf("%s_others", name),
        filePath = self$otherPath,
        templatePerson = templatePerson
      )

      self$fundingPath <-
        sprintf("%s/%s", self$path, self$fileNames["funding"])
      self$funding <- OrderedListPeople$new(
        name = sprintf("%s_funding", name),
        buttonAddLabel = "Add funding PI",
        filePath = self$fundingPath,
        templatePerson = templateFunding,
        role = "PI",
        funding = TRUE
      )

    },

    read = function(path = self$path, fileNames = self$fileNames)
    {

      self$creators$read(
        filePath = sprintf("%s/%s", path, fileNames["creators"])
      )

      contactPath <- sprintf("%s/%s", path, fileNames["contact"])
      if (file.exists(contactPath)) {
        personInfo <- read.table(file = filePath, header = TRUE, sep = ",")[1,]
      } else {
        personInfo <- NULL
      }
      self$contact$copyPersonInfo(personInfo)

      self$others$read(
        filePath = sprintf("%s/%s", path, fileNames["others"])
      )

      self$funding$read(
        filePath = sprintf("%s/%s", path, fileNames["funding"])
      )

      invisible(self)

    },

    write = function(path = self$path, fileNames = self$fileNames)
    {

      self$creators$write(
        filePath = sprintf("%s/%s", path, fileNames["creators"])
      )

      df <- as.data.frame(self$contact$getPersonInfo())
      write.table(
        df,
        file = sprintf("%s/%s", path, fileNames["contact"]),
        sep = ",",
        row.names = FALSE
      )

      self$others$write(
        filePath = sprintf("%s/%s", path, fileNames["others"])
      )

      self$funding$write(
        filePath = sprintf("%s/%s", path, fileNames["funding"])
      )

      invisible(self)

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
