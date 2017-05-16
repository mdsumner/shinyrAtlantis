context("map-object")

## TODO: Rename context
## TODO: Add more tests
library(dplyr)
test_that("rbgm read is the same", {
  skip_if_not(Sys.info()[["nodename"]] == "ace-ecostats")
  bgm.file <- "~/Git/shinyrAtlantis/egfiles/gbr_box_03012012.bgm"
  cum.depth <- c(0,5,10,20,50,100,200,3000)  # cumulative water layer depths
  
  bgm <- make.map.object.frc(bgm.file, cum.depth)
  bgm_new <- rbgm::bgmfile(bgm.file)
  box.data <- bgm_new$boxes %>% 
    transmute(boxid = factor(.bx0), z = botz, is.island = z >= 0, area, 
              volume = abs(area * botz), x.in = insideX, y.in = insideY, 
              numlayers = unlist(lapply(z, function(x) sum(pmin(pmax(0, -x), max(cum.depth)) > cum.depth))))
  map.vertices <- bgm_new$boxes %>% select(.bx0) %>% inner_join(bgm_new$boxesXverts) %>% inner_join(bgm_new$vertices) %>% 
    transmute(boxid = .bx0, x, y)
  
  expect_true(identical(bgm$map.vertices, as.data.frame(map.vertices)))
  expect_true(all.equal(bgm$box.data, as.data.frame(box.data)))
  
})
