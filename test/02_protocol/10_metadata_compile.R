rmarkdown::render(
   input = "./02_protocol/10_metadata.Rmd",
   output_format = c("html_document"),
   output_file = c("metadata_summary.html"),
   output_dir = "./03_incremental",
   knit_root_dir = getwd()
)

# incrementalPath <- "./03_incremental";
# capture.output(
#    system(
#       command = paste(
#          "zip",
#          ifelse (file.exists("./04_product/03_incremental.zip"), "-ru", "-r"),
#          "./04_product/03_incremental.zip",
#          paste(
#             sprintf(
#                fmt = c(
#                   "%s/metadata_summary.html"
#                ),
#                incrementalPath
#             ),
#             collapse = " "
#          )
#       ),
#       intern = TRUE
#    ),
#    type = "message"
# );

makeEML();
EMLassemblyline::issues();
