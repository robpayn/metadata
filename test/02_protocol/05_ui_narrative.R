rm(list = ls());

library(shiny);
library(metadata);

if (file.exists("./03_incremental/meta/metadata_narrative.RData")) {
  import = readRDS(file = "./03_incremental/meta/metadata_narrative.RData")
} else {
  import = NULL
}

md <- MetadataNarrative$new(copy = import);

runApp(
  shinyApp(
    ui = fluidPage(
      fileInput(
        inputId = "buttonImport",
        multiple = FALSE,
        label = "Import RData",
        buttonLabel = "Import",
        width = "100%"
      ),
      
      do.call(
        what = tags$div,
        args = md$createShinyUI()
      ),
      
      textInput(
        inputId = "textSaveRData",
        label = "Save to RData file",
        value = "./03_incremental/meta/metadata_narrative.RData",
        width = "100%"
      ),
      actionButton(
        inputId = "buttonSaveRData",
        label = "Save"
      )
    ),
    server = function(input, output, session) {
      
      observeEvent(
        eventExpr = input$buttonImport,
        handlerExpr = {
          import <- readRDS(
            file = input$buttonImport$datapath[1]
          );
          md$copyData(import);
          md$updateShinyUI();
        }
      );
      
      md$createShinyServer(input, output, session);
  
      observeEvent(
        eventExpr = input$buttonSaveRData,
        handlerExpr = {
          saveRDS(
            md,
            file = input$textSaveRData
          );
        }
      );
      
    }
  )
);

