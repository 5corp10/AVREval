#' Initialize package (Run first)
#'
#' Import dependent packages, import patient data and point mapping, and set constants
#' @export
pkgInit = function()
{
  #import libraries
  #library("devtools")
  #library("roxygen2")
  library("ggplot2")
  #library("cowplot")
  #library("UpSetR")
  #library("venneuler")
  library("XLConnect")
  library("reshape2")
  
  # specify excel file
  excel.file = file.path("GitHub/AVREval/AVR_data.xlsx")
  
  # import all data
  assign("df.all_data", readWorksheetFromFile(excel.file, sheet=1), envir = .GlobalEnv)
  
  # read patient number
  assign("NUM_PAT", nrow(df.all_data), envir = .GlobalEnv)
}

sort = function()
{
  df.unique = df.linked1 = df.linked2 = data.frame(df.all_data[1,])
  
  i = j = k = 1
  for(pat in 1:(NUM_PAT-1)){
    linked = FALSE
    id = df.all_data[pat,"Patient_ID"]
    
    for(pat2 in (pat+1):NUM_PAT){
      if(id == df.all_data[pat2,"Patient_ID"]){
        if(("First Op" == df.all_data[pat,"redoAny"]) && ("Reop #1" == df.all_data[pat2,"redoAny"])){
          df.linked1[j,] = df.all_data[pat,]
          df.linked2[j,] = df.all_data[pat2,]
          j = j + 1
          linked = TRUE
          break
        }
        else if(("First Op" == df.all_data[pat2,"redoAny"]) && ("Reop #1" == df.all_data[pat,"redoAny"])){
          df.linked1[j,] = df.all_data[pat2,]
          df.linked2[j,] = df.all_data[pat,]
          j = j + 1
          linked = TRUE
          break
        }
      }
    }
    
    if(("First Op" == df.all_data[pat,"redoAny"]) && (linked == FALSE) && (!is.na(df.all_data[pat,"avType"]))){
      df.unique[i,] = df.all_data[pat,]
      i = i + 1
    }
  }
  if("First Op" == df.all_data[NUM_PAT,"redoAny"]){
    df.unique[i,] = df.all_data[NUM_PAT,]
  }
  
  assign("df.unique", df.unique, envir = .GlobalEnv)
  assign("df.linked1", df.linked1, envir = .GlobalEnv) 
  assign("df.linked2", df.linked2, envir = .GlobalEnv)
  
  assign("NUM_unique", nrow(df.unique), envir = .GlobalEnv)
  assign("NUM_linked", nrow(df.linked1), envir = .GlobalEnv)
}

byType = function()
{
  df.by_type = data.frame("type" = c("B", "B", "M", "M"), "group" = c("implants", "explants", "implants", "explants"), 
                          "2006" = c(0,0,0,0), "2007" = c(0,0,0,0), "2008" = c(0,0,0,0), "2009" = c(0,0,0,0), 
                          "2010" = c(0,0,0,0), "2011" = c(0,0,0,0), "2012" = c(0,0,0,0), "2013" = c(0,0,0,0),
                          "2014" = c(0,0,0,0), "2015" = c(0,0,0,0), "2016" = c(0,0,0,0), check.names = F)
  
  for(pat in 1:NUM_unique){
    for(year in 2006:2016){
      if(as.integer(format(df.unique[pat,"ordate"],"%Y")) == year){
        if(df.unique[pat,"avType"] == "B"){
          df.by_type[1,as.character(year)] = df.by_type[1,as.character(year)] + 1
        }
        else if(df.unique[pat,"avType"] == "M"){
          df.by_type[3,as.character(year)] = df.by_type[3,as.character(year)] + 1
        }
      }
    }
  }
  for(pat in 1:NUM_linked){
    for(year in 2006:2016){
      if(as.integer(format(df.linked1[pat,"ordate"],"%Y")) == year){
        if(df.linked1[pat,"avType"] == "B"){
          df.by_type[1,as.character(year)] = df.by_type[1,as.character(year)] + 1
          df.by_type[2,as.character(year)] = df.by_type[2,as.character(year)] + 1
        }
        else if(df.linked1[pat,"avType"] == "M"){
          df.by_type[3,as.character(year)] = df.by_type[3,as.character(year)] + 1
          df.by_type[4,as.character(year)] = df.by_type[4,as.character(year)] + 1
        }
      }
    }
  }
  assign("df.by_type", df.by_type, envir = .GlobalEnv)
}

byBrand = function()
{
  df.by_brand = data.frame("brand" = c("CE", "CE", "SJM.Epic", "SJM.Epic", "SJM.Trifecta", "SJM.Trifecta", "Mechanical", "Mechanical"), 
                           "group" = c("implants", "explants", "implants", "explants", "implants", "explants", "implants", "explants"), 
                           "2006" = c(0,0,0,0,0,0,0,0), "2007" = c(0,0,0,0,0,0,0,0), "2008" = c(0,0,0,0,0,0,0,0), "2009" = c(0,0,0,0,0,0,0,0), 
                           "2010" = c(0,0,0,0,0,0,0,0), "2011" = c(0,0,0,0,0,0,0,0), "2012" = c(0,0,0,0,0,0,0,0), "2013" = c(0,0,0,0,0,0,0,0),
                           "2014" = c(0,0,0,0,0,0,0,0), "2015" = c(0,0,0,0,0,0,0,0), "2016" = c(0,0,0,0,0,0,0,0), check.names = F)
  
  for(pat in 1:NUM_unique){
    for(year in 2006:2016){
      if(as.integer(format(df.unique[pat,"ordate"],"%Y")) == year){
        if(length(grep("CE ", df.unique[pat,"avImp"])) == 1){
          df.by_brand[1,as.character(year)] = df.by_brand[1,as.character(year)] + 1
        }
        else if(length(grep("Epic", df.unique[pat,"avImp"])) == 1){
          df.by_brand[3,as.character(year)] = df.by_brand[3,as.character(year)] + 1
        }
        else if(length(grep("Tri", df.unique[pat,"avImp"])) == 1){
          df.by_brand[5,as.character(year)] = df.by_brand[5,as.character(year)] + 1
        }
        else if(df.unique[pat,"avType"] == "M"){
          df.by_brand[7,as.character(year)] = df.by_brand[7,as.character(year)] + 1
        }
      }
    }
  }
  
  for(pat in 1:NUM_linked){
    for(year in 2006:2016){
      if(as.integer(format(df.linked1[pat,"ordate"],"%Y")) == year){
        if(length(grep("CE ", df.linked1[pat,"avImp"])) == 1){
          df.by_brand[1,as.character(year)] = df.by_brand[1,as.character(year)] + 1
          df.by_brand[2,as.character(year)] = df.by_brand[2,as.character(year)] + 1
        }
        else if(length(grep("Epic", df.linked1[pat,"avImp"])) == 1){
          df.by_brand[3,as.character(year)] = df.by_brand[3,as.character(year)] + 1
          df.by_brand[4,as.character(year)] = df.by_brand[4,as.character(year)] + 1
        }
        else if(length(grep("Tri", df.linked1[pat,"avImp"])) == 1){
          df.by_brand[5,as.character(year)] = df.by_brand[5,as.character(year)] + 1
          df.by_brand[6,as.character(year)] = df.by_brand[6,as.character(year)] + 1
        }
        else if(df.linked1[pat,"avType"] == "M"){
          df.by_brand[7,as.character(year)] = df.by_brand[7,as.character(year)] + 1
          df.by_brand[8,as.character(year)] = df.by_brand[8,as.character(year)] + 1
        }
      }
    }
  }
  assign("df.by_brand", df.by_brand, envir = .GlobalEnv)
}
