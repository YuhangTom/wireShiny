## code to prepare `x3p_subsamples` dataset
library(purrr)

x3p_names <- c(
  "T1AW-LI-R1",
  "T1AW-LI-R2",
  "T1AW-LM-R1"
)

dir <- x3p_names %>%
  paste0("../Wirecuts/scans/", .) %>%
  paste0(".x3p")

x3p_subsamples <- map(dir, x3ptools::x3p_read) %>%
  map(x3ptools::x3p_average, b = 10) %>%
  set_names(x3p_names)

dir.create("./inst/wireShinyApp/testDat", showWarnings = FALSE)
file.remove(list.files("./inst/wireShinyApp/testDat", full.names = TRUE))

x3p_subsamples %>%
  iwalk(
    function(x, i){
      x3ptools::x3p_write(x, file = paste0("./inst/wireShinyApp/testDat/", i, ".x3p"))
    })


x3p_subsamples_1 <- x3p_subsamples[1]
x3p_subsamples_2 <- x3p_subsamples[1:2]
x3p_subsamples_3 <- x3p_subsamples[1:3]

save(x3p_subsamples_1, file = "./inst/wireShinyApp/testDat/x3p_subsamples_1.rda")
save(x3p_subsamples_2, file = "./inst/wireShinyApp/testDat/x3p_subsamples_2.rda")
save(x3p_subsamples_3, file = "./inst/wireShinyApp/testDat/x3p_subsamples_3.rda")


x3p_subsamples <- x3p_subsamples[1:2]
usethis::use_data(x3p_subsamples, overwrite = TRUE)
