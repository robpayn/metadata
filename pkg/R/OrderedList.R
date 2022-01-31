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
      name,
      buttonAddLabel = "Add"
    )
    {

      self$name <- name;

      self$elements <- vector(mode = "list", length = 0);

      self$idCounter <- 0;

      self$idDivList <- sprintf("%s_divList", self$name);
      self$idButtonAddElement <- sprintf("%s_buttonAddElement", self$name);
      self$buttonAddLabel <- buttonAddLabel;

    },

    copyData = function(copy) {

      lapply(
        X = self$elements,
        FUN = function(element) {
          self$removeElement(element);
          self$removeElementUI(element);
        }
      );

      lapply(
        X = copy$elements,
        FUN = function(element) {
          self$addElement(copy = element);
        }
      );

      invisible(self);

    },

    updateShinyUI = function() {

      lapply(
        X = self$elements,
        FUN = function(element) {
          insertUI(
            session = self$session,
            selector = sprintf("#%s", self$idDivList),
            ui = self$createElementUI(element)
          );
          self$createElementServer(element);
        }
      );

      invisible(self);

    },

    createShinyUI = function() {

      return(

        list(

          do.call(
            what = tags$div,
            c(
              unname(
                lapply(
                  X = self$elements,
                  FUN = function(element) {
                    self$createElementUI(element);
                  }
                )
              ),
              list(id = self$idDivList)
            )
          ),

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

      lapply(
        X = self$elements,
        FUN = function(element) {
          self$createElementServer(element)
        }
      );

      observeEvent(
        eventExpr = input[[self$idButtonAddElement]],
        handlerExpr = {

          element <- self$addElement();
          insertUI(
            session = self$session,
            selector = sprintf("#%s", self$idDivList),
            ui = self$createElementUI(element)
          );
          self$createElementServer(element);

        }
      );

      invisible(self);

    },

    addElement = function(copy = NULL) {

      self$idCounter <- self$idCounter + 1;
      id <- sprintf("%s_%d", self$name, self$idCounter);
      self$elements[[id]] <- self$createElement(
        name = id,
        copy = copy
      );

      return( self$elements[[id]] );

    },

    createElementUI = function(element) {

      return(

        tags$div(

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

    },

    createElementServer = function(element) {

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
          FUN = function(element) {
            self$removeElementUI(element)
          }
        )

        to <- self$elements[[index - 1]];

        self$elements[[index - 1]] <- self$elements[[index]];
        names(self$elements)[index - 1] <- self$elements[[index - 1]]$name;

        self$elements[[index]] <- to;
        names(self$elements)[index] <- self$elements[[index]]$name;

        self$updateShinyUI();

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

        self$updateShinyUI();

      }

      invisible(self);

    }

  )
);
