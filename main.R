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
  library("ggfortify")
  #library("cowplot")
  #library("UpSetR")
  #library("venneuler")
  library("readxl")
  library("reshape2")
  library("dplyr")
  
  # specify excel file
  excel.file = file.path("AVR_data.xlsx")
  
  # import all data
  assign("df.all_data", data.frame(read_xlsx(excel.file, sheet=1)), envir = .GlobalEnv)
  
  # read patient number
  assign("NUM_PAT", nrow(df.all_data), envir = .GlobalEnv)
}

sort = function()
{
  assign("df.all_data", arrange(df.all_data, Patient_ID), envir = .GlobalEnv)
  df.unique = df.linked1 = df.linked2 = data.frame(df.all_data[1,])
  
  i = j = k = 1
  for(pat in 1:(NUM_PAT-1)){
    print(pat)
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

compByBrand = function()
{
  df.comp_by_brand = data.frame("brand" = c("CE", "CE", "SJM.Epic", "SJM.Epic", "SJM.Trifecta", "SJM.Trifecta", "Mechanical", "Mechanical"), 
                           "group" = c("remaining", "explants", "remaining", "explants", "remaining", "explants", "remaining", "explants"),
                           "OpComp" = c(0,0,0,0), "CardiacComp" = c(0,0,0,0), "NeuroComp" = c(0,0,0,0), "PulmComp" = c(0,0,0,0),
                           "longVent" = c(0,0,0,0), "Pneumonia" = c(0,0,0,0), "Reintubation" = c(0,0,0,0), "AnyComp" = c(0,0,0,0), check.names = F)
  
  for(pat in 1:NUM_unique){
    for(comp in c("OpComp", "CardiacComp", "NeuroComp", "PulmComp", "longVent", "Pneumonia", "Reintubation", "AnyComp")){
      if((!is.na(df.unique[pat,comp])) && (df.unique[pat,comp] == "Yes")){
        if(length(grep("CE ", df.unique[pat,"avImp"])) == 1){
          df.comp_by_brand[1,comp] = df.comp_by_brand[1,comp] + 1
        }
        else if(length(grep("Epic", df.unique[pat,"avImp"])) == 1){
          df.comp_by_brand[3,comp] = df.comp_by_brand[3,comp] + 1
        }
        else if(length(grep("Tri", df.unique[pat,"avImp"])) == 1){
          df.comp_by_brand[5,comp] = df.comp_by_brand[5,comp] + 1
        }
        else if(df.unique[pat,"avType"] == "M"){
          df.comp_by_brand[7,comp] = df.comp_by_brand[7,comp] + 1
        }
      }
    }
  }
  
  for(pat in 1:NUM_linked){
    for(comp in c("OpComp", "CardiacComp", "NeuroComp", "PulmComp", "longVent", "Pneumonia", "Reintubation", "AnyComp")){
      if((!is.na(df.linked1[pat,comp])) && (df.linked1[pat,comp] == "Yes")){
        if(length(grep("CE ", df.linked1[pat,"avImp"])) == 1){
          df.comp_by_brand[2,comp] = df.comp_by_brand[2,comp] + 1
        }
        else if(length(grep("Epic", df.linked1[pat,"avImp"])) == 1){
          df.comp_by_brand[4,comp] = df.comp_by_brand[4,comp] + 1
        }
        else if(length(grep("Tri", df.linked1[pat,"avImp"])) == 1){
          df.comp_by_brand[6,comp] = df.comp_by_brand[6,comp] + 1
        }
        else if(df.linked1[pat,"avType"] == "M"){
          df.comp_by_brand[8,comp] = df.comp_by_brand[8,comp] + 1
        }
      }
    }
  }
  
  assign("df.comp_by_brand", df.comp_by_brand, envir = .GlobalEnv)  
}

preOpByBrand = function()
{
  df.preop_by_brand = data.frame("brand" = c("CE", "CE", "SJM.Epic", "SJM.Epic", "SJM.Trifecta", "SJM.Trifecta", "Mechanical", "Mechanical"), 
                                "group" = c("remaining", "explants", "remaining", "explants", "remaining", "explants", "remaining", "explants"),
                                "Smokhx"=0,	"Smokcurr"=0,	"Diabetes"=0,	"PreopRI"=0,	"PreopRF"=0,	"Preopdial"=0,	
                                "Pulmtens"=0,	"CVA"=0,	"Endocard"=0,	"EndocardAct"=0,	"EndocardTreat"=0, "cvd"=0,
                                check.names = F)
  
  for(pat in 1:NUM_unique){
    for(comp in c("Smokhx",	"Smokcurr",	"Diabetes",	"PreopRI",	"PreopRF",	"Preopdial", "Pulmtens",	"CVA", "Endocard",	"EndocardAct",	"EndocardTreat",	"cvd")){
      if((!is.na(df.unique[pat,comp])) && (df.unique[pat,comp] == "Yes")){
        if(length(grep("CE ", df.unique[pat,"avImp"])) == 1){
          df.preop_by_brand[1,comp] = df.preop_by_brand[1,comp] + 1
        }
        else if(length(grep("Epic", df.unique[pat,"avImp"])) == 1){
          df.preop_by_brand[3,comp] = df.preop_by_brand[3,comp] + 1
        }
        else if(length(grep("Tri", df.unique[pat,"avImp"])) == 1){
          df.preop_by_brand[5,comp] = df.preop_by_brand[5,comp] + 1
        }
        else if(df.unique[pat,"avType"] == "M"){
          df.preop_by_brand[7,comp] = df.preop_by_brand[7,comp] + 1
        }
      }
    }
  }
  
  for(pat in 1:NUM_linked){
    for(comp in c("Smokhx",	"Smokcurr",	"Diabetes",	"PreopRI",	"PreopRF",	"Preopdial", "Pulmtens",	"CVA", "Endocard",	"EndocardAct",	"EndocardTreat",	"cvd")){
      if((!is.na(df.linked1[pat,comp])) && (df.linked1[pat,comp] == "Yes")){
        if(length(grep("CE ", df.linked1[pat,"avImp"])) == 1){
          df.preop_by_brand[2,comp] = df.preop_by_brand[2,comp] + 1
        }
        else if(length(grep("Epic", df.linked1[pat,"avImp"])) == 1){
          df.preop_by_brand[4,comp] = df.preop_by_brand[4,comp] + 1
        }
        else if(length(grep("Tri", df.linked1[pat,"avImp"])) == 1){
          df.preop_by_brand[6,comp] = df.preop_by_brand[6,comp] + 1
        }
        else if(df.linked1[pat,"avType"] == "M"){
          df.preop_by_brand[8,comp] = df.preop_by_brand[8,comp] + 1
        }
      }
    }
  }
  
  assign("df.preop_by_brand", df.preop_by_brand, envir = .GlobalEnv)  
}


failureRate = function()
{
  df.failure_rate = data.frame("implants"=c(0,0,0,0,0), "explants"=c(0,0,0,0,0), "valve.yrs"=c(0,0,0,0,0), "failure.rate"=c(0,0,0,0,0), "upper.CI"=c(0,0,0,0,0), "lower.CI"=c(0,0,0,0,0))
  row.names(df.failure_rate) = c("CE", "SJM.Epic", "SJM.Trifecta", "Bioprosthetic", "Mechanical")
  
  df.failure_rate["CE","implants"] = sum(df.by_brand[1,3:13])
  df.failure_rate["SJM.Epic","implants"] = sum(df.by_brand[3,3:13])
  df.failure_rate["SJM.Trifecta","implants"] = sum(df.by_brand[5,3:13])
  df.failure_rate["Bioprosthetic","implants"] = sum(df.failure_rate[1:3,"implants"])
  df.failure_rate["Mechanical","implants"] = sum(df.by_brand[7,3:13])
  
  df.failure_rate["CE","explants"] = sum(df.by_brand[2,3:13])
  df.failure_rate["SJM.Epic","explants"] = sum(df.by_brand[4,3:13])
  df.failure_rate["SJM.Trifecta","explants"] = sum(df.by_brand[6,3:13])
  df.failure_rate["Bioprosthetic","explants"] = sum(df.failure_rate[1:3,"explants"])
  df.failure_rate["Mechanical","explants"] = sum(df.by_brand[8,3:13])
  
  for(pat in 1:NUM_unique){
    valve_yrs = abs((as.POSIXct("2016-12-31 UTC", tz="UCT") - df.unique[pat,"ordate"]) / 365.2422)
    
    if(length(grep("CE ", df.unique[pat,"avImp"])) == 1){
      df.failure_rate["CE","valve.yrs"] = df.failure_rate["CE","valve.yrs"] + valve_yrs
    }
    else if(length(grep("Epic", df.unique[pat,"avImp"])) == 1){
      df.failure_rate["SJM.Epic","valve.yrs"] = df.failure_rate["SJM.Epic","valve.yrs"] + valve_yrs
    }
    else if(length(grep("Tri", df.unique[pat,"avImp"])) == 1){
      df.failure_rate["SJM.Trifecta","valve.yrs"] = df.failure_rate["SJM.Trifecta","valve.yrs"] + valve_yrs
    }
    else if(df.unique[pat,"avType"] == "M"){
      df.failure_rate["Mechanical","valve.yrs"] = df.failure_rate["Mechanical","valve.yrs"] + valve_yrs
    }
  }
  
  for(pat in 1:NUM_linked){
    valve_yrs = abs((df.linked2[pat,"ordate"] - df.linked1[pat,"ordate"]) / 365.2422)
    
    if(length(grep("CE ", df.linked1[pat,"avImp"])) == 1){
      df.failure_rate["CE","valve.yrs"] = df.failure_rate["CE","valve.yrs"] + valve_yrs
    }
    else if(length(grep("Epic", df.linked1[pat,"avImp"])) == 1){
      df.failure_rate["SJM.Epic","valve.yrs"] = df.failure_rate["SJM.Epic","valve.yrs"] + valve_yrs
    }
    else if(length(grep("Tri", df.linked1[pat,"avImp"])) == 1){
      df.failure_rate["SJM.Trifecta","valve.yrs"] = df.failure_rate["SJM.Trifecta","valve.yrs"] + valve_yrs
    }
    else if(df.linked1[pat,"avType"] == "M"){
      df.failure_rate["Mechanical","valve.yrs"] = df.failure_rate["Mechanical","valve.yrs"] + valve_yrs
    }
  }
  
  df.failure_rate["Bioprosthetic","valve.yrs"] = sum(df.failure_rate[1:3,"valve.yrs"])
  
  df.failure_rate["CE","failure.rate"] = df.failure_rate["CE","explants"] / df.failure_rate["CE","valve.yrs"]
  df.failure_rate["SJM.Epic","failure.rate"] = df.failure_rate["SJM.Epic","explants"] / df.failure_rate["SJM.Epic","valve.yrs"]
  df.failure_rate["SJM.Trifecta","failure.rate"] = df.failure_rate["SJM.Trifecta","explants"] / df.failure_rate["SJM.Trifecta","valve.yrs"]
  df.failure_rate["Bioprosthetic","failure.rate"] = df.failure_rate["Bioprosthetic","explants"] / df.failure_rate["Bioprosthetic","valve.yrs"]
  df.failure_rate["Mechanical","failure.rate"] = df.failure_rate["Mechanical","explants"] / df.failure_rate["Mechanical","valve.yrs"]
  
  # confidence intervals
  
  assign("df.failure_rate", df.failure_rate, envir = .GlobalEnv)
  
  # generate data frame to be graphed
  df.results_graph = cbind(valve=row.names(df.failure_rate), failure.rate=df.failure_rate[,"failure.rate"])
  #df.results_graph = within(df.results_graph,  failure.rate <- factor(failure.rate, levels=failure.rate))
  print(df.results_graph)
  
  # melt
  #df.melted = melt(df.results_graph, variable.name = "valve", value.name = "failure.rate")
  # round hit rates to 2 sig figs
  df.results_graph[,"failure.rate"] = round(as.double(df.results_graph[,"failure.rate"]), digits=4)
  # add CI columns
  
  print(df.results_graph)
  
  ggplot(df.results_graph, aes(x=valve, y=failure.rate, fill=valve)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    geom_text(aes(label = failure.rate, group = valve), size=6, hjust=0.5, vjust=5, position=position_dodge(0.9)) +
    theme_bw(base_size = 22) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}

kmCurve = function()
{
  library("survival")
  library("survminer")
  df.km_dataunique = data.frame(df.unique[,c("casenum", "Patient_ID", "age", "sex", "avImp", "avType")], 
                                 "valve.yrs" = abs((as.POSIXct("2016-12-31 UTC", tz="UCT") - df.unique[,"ordate"]) / 365.2422), "status"=1)
  df.km_datalinked = data.frame(df.linked1[,c("casenum", "Patient_ID", "age", "sex", "avImp", "avType")], 
                                 "valve.yrs" = abs((df.linked2[,"ordate"] - df.linked1[,"ordate"]) / 365.2422), "status"=2)
  df.km_data = rbind(df.km_dataunique, df.km_datalinked)
  
  i=0
  for(pat in 1:nrow(df.km_data)){
    if(length(grep("CE ", df.km_data[pat,"avImp"])) == 1){
      df.km_data[pat,"avImp"] = "CE"
    }
    else if(length(grep("Epic", df.km_data[pat,"avImp"])) == 1){
      df.km_data[pat,"avImp"] = "SJM.Epic"
    }
    else if(length(grep("Tri", df.km_data[pat,"avImp"])) == 1){
      df.km_data[pat,"avImp"] = "SJM.Trifecta"
    }
    else if(df.km_data[pat,"avType"] == "M"){
      df.km_data[pat,"avImp"] = "Mechanical"
    }
  }
  df.km_data = rbind(df.km_data[which(df.km_data[,"avImp"] == "CE"), ],
                     df.km_data[which(df.km_data[,"avImp"] == "SJM.Epic"), ],
                     df.km_data[which(df.km_data[,"avImp"] == "SJM.Trifecta"), ],
                     df.km_data[which(df.km_data[,"avType"] == "M"), ])
  
  df.km_data = df.km_data %>% mutate(ifelse(sex=="Male", 1, 2))
  df.km_data = df.km_data %>% mutate(ifelse(avType=="B", 1, 2))
  df.km_data = df.km_data %>% mutate(ifelse(avImp=="CE", 1, ifelse(avImp=="SJM.Epic", 2, ifelse(avImp=="SJM.Trifecta", 3, 4))))
  df.km_data = df.km_data[,-c(4,5, 6)]
  colnames(df.km_data)[6:8] = c("sex", "avType", "avImp")
  
  #df.km_data$SurvObj <- with(df.km_data, Surv(valve.yrs, status == 2))
  
  km.by.sex <- survfit(SurvObj ~ sex, data = df.km_data, conf.type = "log-log")
  km.by.type <- survfit(SurvObj ~ avType, data = df.km_data, conf.type = "log-log")
  km.by.brand <- survfit(SurvObj ~ avImp, data = df.km_data, conf.type = "log-log")
  
  
  ggsurvplot(km.by.sex, xlab = "valve yrs", ylab = "probability of valve durability", legend.labs=c("Male", "Female"),
             risk.table = T, conf.int = T, pval = T, pval.coord = c(0, 0.9), ylim = c(0.85, 1))
  ggsurvplot(km.by.type, xlab = "valve yrs", ylab = "probability of valve durability", legend.labs=c("B", "M"),
             risk.table = T, conf.int = T, pval = T, pval.coord = c(0, 0.9), ylim = c(0.85, 1))
  ggsurvplot(km.by.brand, xlab = "valve yrs", ylab = "probability of valve durability", legend.labs=c("CE", "SJM Epic", "SJM Trifecta", "Mechanical"),
             risk.table = T, conf.int = T, pval = T, pval.coord = c(0, 0.9), ylim = c(0.85, 1))
  
  #plot hazard curves
  ggsurvplot(km.by.sex, xlab = "valve yrs", ylab = "probability of valve failure", legend.labs=c("Male", "Female"),
             risk.table = T, conf.int = T, pval = T, pval.coord = c(0, 0.1), ylim = c(0, 0.15), fun="cumhaz")
  ggsurvplot(km.by.type, xlab = "valve yrs", ylab = "probability of valve failure", legend.labs=c("B", "M"),
             risk.table = T, conf.int = T, pval = T, pval.coord = c(0, 0.1), ylim = c(0, 0.15), fun="cumhaz")
  ggsurvplot(km.by.brand, xlab = "valve yrs", ylab = "probability of valve failure", legend.labs=c("CE", "SJM Epic", "SJM Trifecta", "Mechanical"),
             risk.table = T, conf.int = T, pval = T, pval.coord = c(0, 0.1), ylim = c(0, 0.15), fun="cumhaz")
  
  #univariate cox regression analysis
  covariates <- c("age", "sex", "avType", "avImp")
  
  univ_formulas <- sapply(covariates,
                          function(x) as.formula(paste('Surv(valve.yrs, status)~', x)))
  
  univ_models <- lapply(univ_formulas, function(x){coxph(x, data = df.km_data)})
  
  univ_results <- lapply(univ_models,
                         function(x){ 
                           x <- summary(x)
                           p.value<-signif(x$wald["pvalue"], digits=2)
                           wald.test<-signif(x$wald["test"], digits=2)
                           beta<-signif(x$coef[1], digits=2);#coeficient beta
                           HR <-signif(x$coef[2], digits=2);#exp(beta)
                           HR.confint.lower <- signif(x$conf.int[,"lower .95"],2)
                           HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                           HR <- paste0(HR, " (", 
                                        HR.confint.lower, "-", HR.confint.upper, ")")
                           res<-c(beta, HR, wald.test, p.value)
                           names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                         "p.value")
                           return(res)
                           #return(exp(cbind(coef(x),confint(x))))
                         })
  res <- t(as.data.frame(univ_results, check.names = FALSE))
  as.data.frame(res)
  
  #res <- rbind(univ_results[[2]], univ_results[[3]], univ_results[[4]])
  #rownames(res) = c("sex", "avType", "avImp")
  
  #multivariate cox regression analysis
  res.cox <- coxph(Surv(valve.yrs, status) ~ age + sex + avImp, data = df.km_data)
  res.cox <- coxph(Surv(valve.yrs, status==2) ~ age + sex, data = df.km_data)
  
  summary(res.cox)
  
  ggsurvplot(survfit(res.cox, data = df.km_data, conf.type = "log-log"), xlab = "valve yrs", ylab = "probability of valve durability", conf.int = T, ylim = c(0.85, 1))
  
  cox.by.sex = with(df.km_data, data.frame(age = rep(mean(age, na.rm=T), 2), sex = c(1,2), avImp = c(1,1)))
  cox.by.sex = with(df.km_data, data.frame(age = rep(mean(age, na.rm=T), 2), sex = c(1,2)))
  ggsurvplot(survfit(formula=res.cox, newdata=cox.by.sex, data = df.km_data, conf.type = "log-log"),
             xlab = "valve yrs", ylab = "probability of valve durability", legend.labs=c("Male", "Female"), conf.int = T, ylim = c(0.85, 1))
  
  cox.by.type = with(df.km_data, data.frame(age = rep(mean(age, na.rm=T), 2), sex = c(1,1), avType = c(1,2)))
  cox.by.type = with(df.km_data, data.frame(age = rep(mean(age, na.rm=T), 2), sex = c(1,1), avType = c(1,2), avImp = c(4,4)))
  ggsurvplot(survfit(res.cox, newdata=cox.by.type, data = df.km_data, conf.type = "log-log"), 
             xlab = "valve yrs", ylab = "probability of valve durability", legend.labs=c("B", "M"), conf.int = T, ylim = c(0.85, 1))
  
  cox.by.brand = with(df.km_data, data.frame(age = rep(mean(age, na.rm=T), 4), sex = c(1,1,1,1), avImp = c(1,2,3,4)))# avType = c(1,1,1,1), avImp = c(1,2,3,4)))
  cox.by.brand = with(df.km_data, data.frame(age = rep(mean(age, na.rm=T), 4), sex = c(1,1,1,1), avImp = c(1,2,3,4)))# avType = c(2,2,2,2), avImp = c(1,2,3,4)))
  ggsurvplot(survfit(res.cox, newdata=cox.by.brand, data = df.km_data, conf.type = "log-log"), 
             xlab = "valve yrs", ylab = "probability of valve durability", legend.labs=c("CE", "SJM.Epic", "SJM.Trifecta", "Mechanical"), conf.int = T, ylim = c(0.85, 1))
  
  }
