#' Read BGM with rbgm
#'
#' @param bgm_file path to BGM file
#' @param cum_depth vector of cumulative depths
#' @keywords internal
#' @return list of map.vertices and box.data 
#' @importFrom rbgm bgmfile
#' @importFrom dplyr %>% inner_join select transmute
bgm_map_object <- function(bgm_file, cum_depth) {
  bgm_mesh <- rbgm::bgmfile(bgm_file)
  list(box.data = bgm_mesh$boxes %>% 
    dplyr::transmute(boxid = factor(.bx0), z = botz, is.island = z >= 0, area, 
              volume = abs(area * botz), x.in = insideX, y.in = insideY, 
              numlayers = unlist(lapply(z, function(x) sum(pmin(pmax(0, -x), max(cum_depth)) > cum_depth)))), 
  map.vertices = bgm_mesh$boxes %>% dplyr::select(.bx0) %>% 
    dplyr::inner_join(bgm_mesh$boxesXverts) %>% dplyr::inner_join(bgm_mesh$vertices) %>% 
    dplyr::transmute(boxid = .bx0, x, y))
  
}

# +==========================================================+
# |  make.map.object.frc : collect data for displaying maps  |
# +==========================================================+
make.map.object.frc <- function(bgm.file, cum.depth){
  bgm <- readLines(bgm.file) # read in the geometry file
  
  numboxes <- 0
  j <- grep(pattern = "nbox", x = bgm, value = FALSE) # file row(s)
  if (length(j) > 0) { # found rows with nbox
    jnew <- NULL
    for (jj in 1:length(j)) {
      # Valid row is when tmplt is the first entry and second is a number
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j[jj]], replacement = " "), " "))
      if (text.split[1] == "nbox") {
        jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
      }
    }
    j <- jnew # use this list of rows as they are valid
    if (length(j) == 1) { # a single row is found
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j], replacement = " "), " "))
      numboxes <- as.numeric(text.split[2])
    }
  }  
  
  # Extract the box vertices
  map.vertices <- data.frame()
  for(i in 1:numboxes){
    txt.find <- paste("box", i - 1, ".vert", sep = "")
    j <- grep(txt.find, bgm)
    for (jj in 1:length(j)) {
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j[jj]], replacement = " "), " "))
      if (text.split[1] == txt.find) {
        map.vertices <- rbind(map.vertices, cbind(i - 1, as.numeric(text.split[2]),
                                                  as.numeric(text.split[3])))
      } 
    }
  }  
  names(map.vertices) <- c("boxid", "x", "y")  
  
  # find the depths and areas, and identify island boxes
  box.indices <- rep(0, numboxes)  
  for(i in 1:numboxes){ # box depth
    box.indices[i] <- grep(paste("box", i - 1, ".botz", sep = ""), bgm)
  }
  z.tmp <- strsplit(bgm[box.indices], "\t")
  z <- as.numeric(sapply(z.tmp,`[`,2))
  box.data <- data.frame(boxid = 0:(numboxes-1), z = z)
  box.data <- mutate(box.data, is.island = (z >= 0.0))
  for(i in 1:numboxes){ # box area
    box.indices[i] <- grep(paste("box", i - 1, ".area", sep = ""), bgm)
  }
  a.tmp <- strsplit(bgm[box.indices], "\t")
  a <- as.numeric(sapply(a.tmp,`[`,2))
  box.data$area <- a
  box.data <- mutate(box.data, volume = -z*area)
  
  # read in the internal coordinates from bgm file
  box.indices <- rep(0, numboxes)  
  x.in <- rep(0, numboxes)
  y.in <- rep(0, numboxes)
  for(i in 1:numboxes){
    j <- grep(paste("box", i - 1, ".inside", sep = ""), bgm)
    text.split <- unlist(str_split(
      gsub(pattern = "[\t ]+", x = bgm[j], replacement = " "), " "))
    x.in[i] <- as.numeric(text.split[2])
    y.in[i] <- as.numeric(text.split[3])
  }
  box.data$x.in <- x.in # add internal y-location
  box.data$y.in <- y.in # add internal y-location
  box.data$boxid <- factor(box.data$boxid) # make boxid a factor
  
  # calculate the number of water layers per box base don cumulative depths
  # CHECK THIS IS CORRECT: boxid = 21, index = 21 (6 but should be 5)
  z <- -box.data$z # convert depths so depth below surface is positive
  z <- pmax(0,z) # remove depths above the surface 
  z <- pmin(z, max(cum.depth)) # don't alow depth to be greater than max depth
  box.numlayers <- rep(0, length(z)) # vector containing number of water layers
  for (i in 1: length(z)) {
    box.numlayers[i] <- sum(z[i] > cum.depth)
  }
  box.data$numlayers <- box.numlayers # add the vector to box.data
  
  return(list(
    map.vertices = map.vertices, 
    box.data     = box.data)
  )
}
