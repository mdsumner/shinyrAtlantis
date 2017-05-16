context("map-object")

## TODO: Rename context
## TODO: Add more tests
library(dplyr)
test_that("rbgm read is the same", {
  skip_if_not(Sys.info()[["nodename"]] == "ace-ecostats")
  bgm.file <- "~/Git/shinyrAtlantis/egfiles/gbr_box_03012012.bgm"
  cum.depth <- c(0,5,10,20,50,100,200,3000)  # cumulative water layer depths
  
  bgm <- make.map.object.frc(bgm.file, cum.depth)
  bgm_new <- bgm_map_object(bgm.file, cum.depth)
  expect_true(identical(bgm$map.vertices, as.data.frame(bgm_new$map.vertices)))
  expect_true(all.equal(bgm$box.data, as.data.frame(bgm_new$box.data)))
  
})
