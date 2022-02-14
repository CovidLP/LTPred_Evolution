library(dplyr)
library(PandemicLP)
library(stringr)
library(httr)
library(ggplot2)

source("plot_graph.R")

## ----- LOAD PREDICTION RESULTS -----

req <- GET("https://api.github.com/repos/CovidLP/app_COVID19/git/trees/master?recursive=1")
stop_for_status(req)

# extract list of files on github
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
files <- grep("STpredictions/", filelist, value = TRUE, fixed = TRUE)
files <- unlist(lapply(strsplit(files,"/"),"[",2))
files <- grep(".rds",files, value=TRUE)
aux <- sub(pattern = '(\\_n.rds$)|(\\_d.rds$)', replacement = '', x = files)

## List of countries
countries <- sort(unique(
  unlist(lapply(strsplit(aux, "_"), function(x) x[1]))))
countries_orig <- gsub("-", " ", countries)

## List of Brazil's states
statesBR <- unique(unlist(lapply(strsplit(aux, "_"), function(x) if(x[1] == "Brazil") return(x[2]))))
statesBR[is.na(statesBR)] <- "<all>"
statesBR <- sort(statesBR)[-1]

## ----- UPDATING HISTORY ----- 

for(location in files){ 
  
  rds = readRDS(paste0("History/", location))
  date = names(rds)
  
  rds.aux = readRDS(url(paste0("https://github.com/CovidLP/app_COVID19/raw/master/STpredictions/", location)))
  
  date.aux = as.character(rds.aux$df_predict[1]$date[1])
  if(!is.element(date.aux, date)){
    date = c(date, date.aux) # primeiro tempo de previsao
    rds = c(rds, list(rds.aux))
  }
  
  order = order(date)
  date = date[order]
  rds = rds[order]
  names(rds) = date
  
  saveRDS(rds, paste0("History/", location))
  
  closeAllConnections()
  
}

## ----- UPDATING GRAPHS ----- 

# init = Sys.time()

last.date = as.Date("2022-12-31")

for(country in countries_orig){
  
  local_rds = str_replace_all(country, " ", "-")
 
  for(confirmed in c(TRUE, FALSE)){
    
    if(confirmed){
      metric = "n"
      title.name = "- Casos confirmados/Confirmed cases"
    } else{
      metric = "d"
      title.name = "- Mortes/Deaths"
    }
    
    if(!dir.exists(paste0("GraphsPng/", local_rds, "_", metric, "/"))) dir.create(paste0("GraphsPng/", local_rds, "_", metric, "/"))

    # Reading rds e downloading data
    data = load_covid(country)$data
    
    if(confirmed){ data = data %>% select(date, y = new_cases) } else{ data = data %>% select(date, y = new_deaths) }
    rds_completo = readRDS(paste0("History/", local_rds, "_", metric, ".rds"))
    
    # Quantidades importantes
    n_ajustes = length(rds_completo)
    dates_1  = seq.Date(data$date[1], last.date, by = 1)
    dates_14 = seq.Date(data$date[1], last.date, by = 14)
    dates = data.frame(date = dates_1)
    dates_month = seq.Date(as.Date("2020-01-01"), tail(dates$date, 1), by = "month")
    max_data = max(data[, "y"])
    max_mu = unlist(lapply(rds_completo, function(x) max(x$mu_plot[, "mu"])))
    y_max = max(max_data, quantile(na.omit(max_mu), 0.80))
    
    if(ceiling(y_max/10) < 10){ # 1 a 99
      seq_y = seq(0, ceiling(y_max/10)*10, l = 11)
      if(sum(seq_y[-1]/1000 < 1) > 0){ lab_y = seq_y } else{ lab_y = ifelse(seq_y/1000 < 1, seq_y, paste(seq_y/1000, "k", sep = "")) }
    } else if(ceiling(y_max/100) < 10){ # 100 a 999
      seq_y = seq(0, ceiling(y_max/100)*100, l = 11)
      if(sum(seq_y[-1]/1000 < 1) > 0){ lab_y = seq_y } else{ lab_y = ifelse(seq_y/1000 < 1, seq_y, paste(seq_y/1000, "k", sep = "")) }
    } else{ # 1000 a 9999
      seq_y = seq(0, ceiling(y_max/1000)*1000, l = 11)
      if(sum(seq_y[-1]/1000 < 1) > 0){ lab_y = seq_y } else{ lab_y = ifelse(seq_y/1000 < 1, seq_y, paste(seq_y/1000, "k", sep = "")) }
    }
    
    y_max = max(seq_y)
    
    for(i in 1:n_ajustes){
      
      ajuste = rds_completo[[i]]
      plot_graph(ajuste, dates, dates_month, seq_y, lab_y, is.state = FALSE, date2save = names(rds_completo)[i])
      
    }
    
    date.vector = names(rds_completo)
    write(date.vector, paste0("GraphsPng/", local_rds, "_", metric, "/date.vector.txt"))
    
  }
  
  closeAllConnections()
  # print(country)
  
}

for(state in statesBR){
  
  local_rds = paste0("Brazil_", state)
 
  for(confirmed in c(TRUE, FALSE)){
    
    if(confirmed){
      metric = "ne"
      title.name = "- Casos confirmados/Confirmed cases"
    } else{
      metric = "de"
      title.name = "- Mortes/Deaths"
    }
    
    if(!dir.exists(paste0("GraphsPng/", local_rds, "_", metric, "/"))) dir.create(paste0("GraphsPng/", local_rds, "_", metric, "/"))
    
    # Reading rds e downloading data
    data = load_covid("Brazil", state_name = state)$data
    
    if(confirmed){ data = data %>% select(date, y = new_cases) } else{ data = data %>% select(date, y = new_deaths) }
    rds_completo = readRDS(paste0("History/", local_rds, "_", metric, ".rds"))
    
    # Important Quantities
    n_ajustes = length(rds_completo)
    dates_1  = seq.Date(data$date[1], last.date, by = 1)
    dates_14 = seq.Date(data$date[1], last.date, by = 14)
    dates = data.frame(date = dates_1)
    dates_month = seq.Date(as.Date("2020-01-01"), tail(dates$date, 1), by = "month")
    max_data = max(data[, "y"])
    max_mu = unlist(lapply(rds_completo, function(x) max(x$mu_plot[, "mu"])))
    y_max = max(max_data, quantile(na.omit(max_mu), 0.80))
    
    if(ceiling(y_max/10) < 10){ # 1 a 99
      seq_y = seq(0, ceiling(y_max/10)*10, l = 11)
      if(sum(seq_y[-1]/1000 < 1) > 0){ lab_y = seq_y } else{ lab_y = ifelse(seq_y/1000 < 1, seq_y, paste0(seq_y/1000, "k")) }
    } else if(ceiling(y_max/100) < 10){ # 100 a 999
      seq_y = seq(0, ceiling(y_max/100)*100, l = 11)
      if(sum(seq_y[-1]/1000 < 1) > 0){ lab_y = seq_y } else{ lab_y = ifelse(seq_y/1000 < 1, seq_y, paste0(seq_y/1000, "k")) }
    } else{ # 1000 a 9999
      seq_y = seq(0, ceiling(y_max/1000)*1000, l = 11)
      if(sum(seq_y[-1]/1000 < 1) > 0){ lab_y = seq_y } else{ lab_y = ifelse(seq_y/1000 < 1, seq_y, paste0(seq_y/1000, "k")) }
    }
    
    y_max = max(seq_y)
    
    for(i in 1:n_ajustes){
      
      ajuste = rds_completo[[i]]
      plot_graph(ajuste, dates, dates_month, seq_y, lab_y, is.state = TRUE, date2save = names(rds_completo)[i])
      
    }
    
    date.vector = names(rds_completo)
    write(date.vector, paste0("GraphsPng/", local_rds, "_", metric, "/date.vector.txt"))
    
  }
  
  closeAllConnections()
  # print(state)
  
}

# Sys.time() - init