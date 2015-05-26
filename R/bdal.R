get_path <- function (fp) {
  if(!dir.exists(fp)){
    ok <- dir.create(fp, recursive = TRUE)
    if(ok) return(fp)
    return("")
  }
  fp
}

get_hidap_path <- function(){
  fp = file.path(Sys.getenv("HOME"),"HIDAPDB")
  ok = FALSE
  get_path(fp)
}

is_crop_name <- function(crop){
  nchar(crop) == nchar(stringr::str_match(crop, "[a-z]+")[[1]])
}

get_crop_path <- function(crop){
  base_path <- get_hidap_path()
  if(!is_crop_name(crop)) return("")
  if(base_path != ""){
    fp <- file.path(base_path, crop)
    return(get_path(fp))
  }
  ""
}

read_resource <- function(file, sheets){
  n = length(sheets)
  if(!file.exists(file)) return("")
  out <- list(n)
  for(i in 1:n){
    try({
      out[[i]] <- readxl::read_excel(file, sheets[i])
      names(out)[i] <- sheets[i]
    }
    )
  }
  out
}


post_dictionary <- function(tbl, crop){
  fp <- file.path(get_crop_path(crop), "data_dictionary.rda")
  if(!file.exists(fp)){
    data_dictionary <- tbl
    save(data_dictionary, file = fp)
  } else {
    # merge old and new table
    load(fp)
    data_dictionary <- rbind(data_dictionary, tbl)
    data_dictionary <- dplyr::distinct(data_dictionary)
    save(data_dictionary, file = fp)
  }
}

get_fieldbook <- function(fbid, crop){
  if(!is_crop_name(crop)) stop("Invalid crop name")
  fn <- paste0(fbid, ".rda")
  fp <- file.path(get_crop_path(crop), "fieldbooks")
  fp <- get_path(fp)
  fb_all <- list.files(fp, recursive=TRUE)
  if(length(fb_all) == 0) return("")
  
}

extract_cropid <- function(fbid){
  basename(fbid) %>% stringr::str_sub(1,2)
}

extract_crop <- function(fbid, crop){
  if(crop == "unknown"){
    cid <- extract_cropid(fbid)
    crop = switch(cid,
                  PT = "potato",
                  SP = "sweetpotato"
    )
  }
  crop
}

extract_type <- function(fbid, type){
  if(type == "unknown"){
    type <- basename(fbid) %>% stringr::str_sub(3,4)
  }
  type
}

extract_year <- function(fbid, year){
  if(year == 0){
    year <- basename(fbid) %>% stringr::str_sub(5,8) %>% as.integer()
  }
  year
}


extract_season <- function(fbid){
  basename(fbid) %>% stringr::str_sub(9,10) %>% as.integer()
}


extract_location <- function(fbid){
  (
    (basename(fbid) %>% stringr::str_split("_"))[[1]][2] %>%
      stringr::str_split("\\.")
  )[[1]][1]
}

extract_id <- function(fbid){
  (basename(fbid) %>% stringr::str_split("\\."))[[1]][1]
}

import_fieldbook_dc <- function(fb_path, crop="unknown", year=0, program="unknown", phase="unknown",
                                type="unknown" ){
  if(!file.exists(fb_path)) stop("Invalid file name!")
  sheets <- c("Minimal", "Installation", "Material List", "Soil_analysis", "Weather_data",
              "Crop_management", "Var List", "Fieldbook")
  out <- read_resource(fb_path, sheets)
  # add tentative attributes
  attr(out, "id") <- extract_id(fbid)
  attr(out, "cropid") <- extract_cropid(fbid)
  attr(out, "crop") <- extract_crop(fbid, crop)
  attr(out, "program") <- program
  attr(out, "phase") <- phase
  attr(out, "type") <- extract_type(fbid, type)
  attr(out, "year") <- extract_year(fbid, year)
  attr(out, "season") <- extract_season(fbid)
  attr(out, "location") <- extract_location(fbid)
  out
}

# Assume meta-data like crop, program phase, type, year, location, ... are attached.
post_fieldbook <- function(fb){
  fbid <- attr(fb, "id")
  crop <- attr(fb, "crop")
  year <- attr(fb, "year")
  fp <- file.path(get_crop_path(crop), "fieldbooks", year)
  fp <- get_path(fp)
  fn <- paste0(fbid, ".rda")
  fp <- file.path(fp, fn)
  save(fb, file = fp )
}


