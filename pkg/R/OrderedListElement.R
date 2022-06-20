# Dependencies for ROxygen ####

#' @importFrom R6 R6Class
#'
#' @importFrom shiny wellPanel
#' @importFrom shiny actionButton
#' @importFrom shiny removeUI
#' @importFrom shiny observeEvent

# Class OrderedListElement ####

#' @export
#'
#' @title
#'   An element of an ordered list
#'
#' @description
#'   A base class for tracking an element of an ordered list
#'
OrderedListElement <- R6Class(
  classname = "OrderedListElement",
  public = list(

    name = NULL,
    content = NULL,
    orderedList = NULL,

    obsDelete = NULL,
    obsMoveup = NULL,
    obsMovedown = NULL,

    idButtonDelete = NULL,
    idButtonMoveup = NULL,
    idButtonMovedown = NULL,

    initialize = function(name, content, orderedList) {

      self$name <- name;
      self$content <- content;
      self$orderedList <- orderedList;

      self$idButtonDelete <- sprintf("%s_buttonDelete", name);
      self$idButtonMoveup <- sprintf("%s_buttonMoveup", name);
      self$idButtonMovedown <- sprintf("%s_buttonMovedown", name);

    },

    createShinyUI = function() {

      return(

        tags$div(

          do.call(
            what = wellPanel,
            args = c(

              self$content$createShinyUI(),

              list(
                actionButton(
                  inputId = self$idButtonDelete,
                  label = "Delete"
                ),
                actionButton(
                  inputId = self$idButtonMoveup,
                  label = "Move up"
                ),
                actionButton(
                  inputId = self$idButtonMovedown,
                  label = "Move down"
                )
              )
            )
          ),

          id = self$name

        )

      );

    },

    removeShinyUI = function() {

      removeUI(
        session = self$orderedList$session,
        selector = sprintf("#%s", self$name)
      );
      self$obsDelete$destroy();
      self$obsMoveup$destroy();
      self$obsMovedown$destroy();

      invisible(self);

    },

    createShinyServer = function() {

      self$content$createShinyServer(
        self$orderedList$input,
        self$orderedList$output,
        self$orderedList$session
      );

      self$obsDelete <- observeEvent(
        eventExpr = self$orderedList$input[[self$idButtonDelete]],
        handlerExpr = {

          self$orderedList$removeElement(self);
          self$removeShinyUI();

        },
        ignoreInit = TRUE
      );

      self$obsMoveup <- observeEvent(
        eventExpr = self$orderedList$input[[self$idButtonMoveup]],
        handlerExpr = {

          self$orderedList$moveElementUp(self);

        },
        ignoreInit = TRUE
      );

      self$obsMovedown <- observeEvent(
        eventExpr = self$orderedList$input[[self$idButtonMovedown]],
        handlerExpr = {

          self$orderedList$moveElementDown(self);

        },
        ignoreInit = TRUE
      );

      invisible(self);

    }

  )
)
