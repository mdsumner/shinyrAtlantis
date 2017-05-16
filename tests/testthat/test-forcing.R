context("forcing")

test_that("we can skip any test", {
  skip("no way")
  library(shinyrAtlantis)
  fp <- "egfiles"
  
  salinity.file    <- file.path(fp, "SOsaltdummy.nc")
  temperature.file <- file.path(fp, "SOtempdummy.nc"       )
  exchange.file <- file.path(fp, "SOhydrodummy.nc")
  bgm.file         <- file.path(fp, "Antarctica_28.bgm" )
  cum.depth <- c(0,5,10,20,50,100,200,300, 500, 3000)  # cumulative water layer depths
  
  
  # 
  salinity.file    <- file.path(fp, "GBR108_salt.nc")
  temperature.file <- file.path(fp, "GBR108_temp.nc"       )
  exchange.file <- "/home/shared/data/Atlantis/GBR/GBR108_hydro.nc"
  bgm.file         <- file.path(fp, "gbr_box_03012012.bgm" )
  cum.depth <- c(0,5,10,20,50,100,200,3000)  # cumulative water layer depths
  # 
  
  
  input.object <- make.sh.forcings.object(
    bgm.file         = bgm.file,
    exchange.file    = exchange.file,
    cum.depth        = cum.depth,
    temperature.file = temperature.file,
    salinity.file    = salinity.file
  )

  sh.forcings(input.object)
  
  
  
})

