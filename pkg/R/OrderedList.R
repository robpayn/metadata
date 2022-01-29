# Dependencies for ROxygen ####

#' @importFrom R6 R6Class
#'
#' @importFrom shiny observeEvent
#' @importFrom shiny actionButton
#' @importFrom shiny insertUI
#' @importFrom shiny wellPanel
#' @importFrom shiny tags

# Class OrderedList ####

#' @export
#'
#' @title
#'   Shiny interface for an ordered list of elements
#'
#' @description
#'   A modular shiny interface for managing an ordered list of elements
#'
OrderedList <- R6Class(
  classname = "OrderedList",
  public = list(

    elementFactory = NULL,

    elements = NULL,

    input = NULL,
    output = NULL,
    session = NULL,

    name = NULL,

    idCounter = NULL,

    idDivList = NULL,
    idButtonAddElement = NULL,
    buttonAddLabel = NULL,

    initialize = function(
      elementFactory,
      name,
      buttonAddLabel = "Add"
    )
    {

      self$elementFactory <- elementFactory;

      self$name <- name;

      self$elements <- vector(mode = "list", length = 0);

      self$idCounter <- 0;

      self$idDivList <- sprintf("%s_divList", self$name);
      self$idButtonAddElement <- sprintf("%s_buttonAddElement", self$name);
      self$buttonAddLabel <- buttonAddLabel;

    },

    createShinyUI = function() {

      return(

        list(

          tags$div(id = self$idDivList),

          actionButton(
            inputId = self$idButtonAddElement,
            label = self$buttonAddLabel
          )

        )

      )

    },

    createShinyServer = function(input, output, session) {

      self$input <- input;
      self$output <- output;
      self$session <- session;

      observeEvent(
        eventExpr = input[[self$idButtonAddElement]],
        handlerExpr = {

          self$addElementUI(self$addElement());

        }
      );

      invisible(self);

    },

    addElement = function() {

      self$idCounter <- self$idCounter + 1;
      id <- sprintf("%s_%d", self$name, self$idCounter);
      self$elements[[id]] <- self$elementFactory$createElement(
        name = id
      );

      return( self$elements[[id]] );

    },

    addElementUI = function(element) {

      insertUI(
        session = self$session,
        selector = sprintf("#%s", self$idDivList),
        ui = tags$div(

          do.call(
            what = wellPanel,
            args = c(
              element$createShinyUI(),
              list(
                actionButton(
                  inputId = element$idButtonDelete,
                  label = "Delete"
                ),
                actionButton(
                  inputId = element$idButtonMoveup,
                  label = "Move up"
                ),
                actionButton(
                  inputId = element$idButtonMovedown,
                  label = "Move down"
                )
              )
            )
          ),

          id = element$name

        )
      );

      element$createShinyServer(self$input, self$output, self$session);

      element$obsDelete <- observeEvent(
        eventExpr = self$input[[element$idButtonDelete]],
        handlerExpr = {

          self$removeElement(element);
          self$removeElementUI(element);

        },
        ignoreInit = TRUE
      );

      element$obsMoveup <- observeEvent(
        eventExpr = self$input[[element$idButtonMoveup]],
        handlerExpr = {

          self$moveElementUp(element);

        },
        ignoreInit = TRUE
      );

      element$obsMovedown <- observeEvent(
        eventExpr = self$input[[element$idButtonMovedown]],
        handlerExpr = {

          self$moveElementDown(element);

        },
        ignoreInit = TRUE
      );

      invisible(self);

    },

    removeElement = function(element) {

      self$elements <- self$elements[names(self$elements) != element$name];

      invisible(self);

    },

    removeElementUI = function(element) {

      removeUI(
        session = self$session,
        selector = sprintf("#%s", element$name)
      );
      element$obsDelete$destroy();
      element$obsMoveup$destroy();
      element$obsMovedown$destroy();

      invisible(self);

    },

    moveElementUp = function(element) {

      index <- which(names(self$elements) == element$name);
      if (index > 1) {

        lapply(
          X = self$elements,
          FUN = function(x) {
            self$removeElementUI(x)
          }
        )

        to <- self$elements[[index - 1]];

        self$elements[[index - 1]] <- self$elements[[index]];
        names(self$elements)[index - 1] <- self$elements[[index - 1]]$name;

        self$elements[[index]] <- to;
        names(self$elements)[index] <- self$elements[[index]]$name;

        lapply(
          X = self$elements,
          FUN = function(x) {
            self$addElementUI(x);
          }
        )

      }

      invisible(self);

    },

    moveElementDown = function(element) {

      index <- which(names(self$elements) == element$name);
      if (index < length(self$elements)) {

        lapply(
          X = self$elements,
          FUN = function(x) {
            self$removeElementUI(x);
          }
        )

        to <- self$elements[[index + 1]];

        self$elements[[index + 1]] <- self$elements[[index]];
        names(self$elements)[index + 1] <- self$elements[[index + 1]]$name;

        self$elements[[index]] <- to;
        names(self$elements)[index] <- self$elements[[index]]$name;

        lapply(
          X = self$elements,
          FUN = function(x) {
            self$addElementUI(x);
          }
        )

      }

      invisible(self);

    }

  )
);
