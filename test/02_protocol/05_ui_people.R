rm(list = ls());

library(shiny);
library(metadata);
library(R6);

people <- as.data.frame(
  readxl::read_excel(
    path = "./01_input/meta/people.xlsx",
    sheet = "Table 1",
    range = readxl::cell_limits(ul = c(4, 2)),
    col_names = TRUE,
    col_types = rep("text", times = 7)
  )
);
row.names(people) <- people$internalUniqueID;
templatePerson <- TemplatePerson$new(
  people = people
);

funding <- as.data.frame(
  readxl::read_excel(
    path = "./01_input/meta/people.xlsx",
    sheet = "Funding",
    range = readxl::cell_limits(ul = c(4, 2)),
    col_names = TRUE,
    col_types = rep("text", times = 10)
  )
);
row.names(funding) <- funding$internalUniqueID;
templateFunding <- TemplatePerson$new(
  people = funding
);

if (file.exists("./03_incremental/meta/metadata_people.RData")) {
  import = readRDS(file = "./03_incremental/meta/metadata_people.RData")
} else {
  import = NULL
}


#### Script ####

md <- MetadataPeople$new(
  templatePerson = templatePerson,
  templateFunding = templateFunding,
  copy = import
);

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
        value = "./03_incremental/meta/metadata_people.RData",
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
