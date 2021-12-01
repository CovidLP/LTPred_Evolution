library(dplyr)
library(PandemicLP)

setwd("C:/Users/55219/Desktop/PastPredictions")

# definindo o pais
countries = c("Ethiopia", "Peru", "Venezuela")

for(country in countries){
  for(casos in c(TRUE, FALSE)){
    
    if(casos){
      rds.name = "n"
      title.name = "- Casos confirmados/Confirmed cases"
    } else{
      rds.name = "d"
      title.name = "- Mortes/Deaths"
    }
    
    country_rds = ifelse(country == "South Korea", "Korea,-South", country)
    last.date = as.Date("2023-12-31")
    
    # Lendo o rds e baixando os dados
    data = load_covid(country)$data
    if(casos){ data = data %>% select(date, y = new_cases) } else{ data = data %>% select(date, y = new_deaths) }
    rds_completo = readRDS(paste(country_rds, "_", rds.name, ".rds", sep = ""))
    
    # Quantidades importantes
    n_ajustes = length(rds_completo)
    dates_1  = seq.Date(data$date[1], last.date, by = 1)
    dates_14 = seq.Date(data$date[1], last.date, by = 14)
    dates_30 = seq.Date(data$date[1], last.date, by = 30)
    dates = data.frame(date = dates_1)
    max_data = max(data[, "y"])
    max_mu = unlist(lapply(rds_completo, function(x) max(x$mu_plot[, "mu"])))
    y_max = max(max_data, quantile(max_mu, 0.80))
    
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
    graphs = list()
    
    for(i in 1:n_ajustes){
      
      ajuste = rds_completo[[i]]
      
      ajuste_df = suppressMessages(dates %>%
                                     full_join(data) %>%
                                     full_join(ajuste$mu_plot) %>%
                                     full_join(ajuste$lt_predict) %>%
                                     mutate(prev = is.na(m), 
                                            y = ifelse(prev, y, NA)) %>%
                                     filter(date <= last.date))
      
      plot(ajuste_df[, c("date", "y")], col = NA, ylim = c(0, max(seq_y)), bty = "n", axes = FALSE, xlab = "", ylab = "",
           main = paste(country, title.name), cex.main = 0.9)
      
      abline(v = dates_14, col = "gray90")
      abline(h = seq_y, col = "gray90")
      abline(h = 0)
      axis(1, at = dates_14, labels = dates_14, las = 2, tick = FALSE, cex.axis = 0.7)
      axis(2, at = seq_y, labels = lab_y, las = 2, tick = FALSE, cex.axis = 0.8)
      
      interval = data.frame(x = c(ajuste_df[, "date"], ajuste_df[nrow(ajuste_df):1, "date"]),
                            y = c(ajuste_df[, "q25"], ajuste_df[nrow(ajuste_df):1, "q975"])) %>%
        tidyr::drop_na()
      
      polygon(x = interval$x,
              y = interval$y,
              col = "#b1b1b1", border = NA)
      
      points(ajuste_df[, c("date", "y")], type = "l", col = "#648cf0")
      points(ajuste_df[, c("date", "y")], col = "#648cf0", pch = 16)
      points(ajuste_df[, c("date", "y")], col = "#10167b")
      
      seq.pico = seq.Date(ajuste$lt_summary$high.dat.low, ajuste$lt_summary$high.dat.upper, by = 1)
      seq.pico = seq.pico[seq.pico < last.date]
      n.pico = length(seq.pico)
      mu.pico = (ajuste_df %>% filter(is.element(date, seq.pico)))$mu
      mu.pico = ifelse(mu.pico > y_max, y_max, mu.pico)
      
      if(sum(is.na(mu.pico)) > 0){
        which.na = is.na(mu.pico)
        seq.pico.na = seq.pico[!which.na]
        mu.pico.na = mu.pico[!which.na]
        polygon(x = c(seq.pico.na, seq.pico.na[length(seq.pico.na):1]),
                y = c(rep(0, length(seq.pico.na)), mu.pico.na[length(seq.pico.na):1]),
                col = adjustcolor("#b6000d", 0.5),
                border = NA)
      } else{
        polygon(x = c(seq.pico, seq.pico[n.pico:1]),
                y = c(rep(0, n.pico), mu.pico[n.pico:1]),
                col = adjustcolor("#b6000d", 0.5),
                border = adjustcolor("#b6000d", 0.5),
                lwd = 2)
      }
      
      low.end = as.Date(ajuste$lt_summary$end.dat.low)
      upp.end = as.Date(ajuste$lt_summary$end.dat.upper)
      if(low.end > upp.end){
        low.end = ajuste$lt_summary$end.dat.upper
        upp.end = ajuste$lt_summary$end.dat.low 
      }
      seq.fim = seq.Date(low.end, upp.end, by = 1)
      n.fim = length(seq.fim)
      mu.fim = rep(y_max, n.fim)
      polygon(x = c(seq.fim, seq.fim[n.fim:1]),
              y = c(rep(0, n.fim), mu.fim[n.fim:1]),
              col = adjustcolor("#7aad79", 0.5),
              border = adjustcolor("#7aad79", 0.5),
              lwd = 2)
      
      legend("topright", legend = "", title = format(round(ajuste$lt_summary$NTC500), big.mark = ".", decimal.mark = ","), bty = "n", cex = 2, text.col = "white")
      legend("topright", legend = "", title = format(round(ajuste$lt_summary$NTC500), big.mark = ".", decimal.mark = ","), bty = "n", cex = 2, text.col = "white")
      legend("topright", legend = "", title = format(round(ajuste$lt_summary$NTC500), big.mark = ".", decimal.mark = ","), bty = "n", cex = 2, text.col = "white")
      legend("topright", legend = "", title = format(round(ajuste$lt_summary$NTC500), big.mark = ".", decimal.mark = ","), bty = "n", cex = 2, text.col = "white")
      legend("topright", legend = "", title = format(round(ajuste$lt_summary$NTC500), big.mark = ".", decimal.mark = ","), bty = "n", cex = 2, text.col = "white")
      legend("topright", legend = "", title = format(round(ajuste$lt_summary$NTC500), big.mark = ".", decimal.mark = ","), bty = "n", cex = 2, text.col = "gray50")
      
      lines(ajuste_df[, c("date", "mu")], lwd = 2, col = "#e67300")
      polygon(x = c(dates_1, dates_1[length(dates_1):1]),
              y = c(rep(y_max, length(dates_1)), rep(y_max + 100000, length(dates_1))),
              col = "white",
              border = NA)
      
      abline(v = interval$x[1])
      
      graphs[[i]] = recordPlot()
      
      Sys.sleep(0.2)
      
    }
    
    names(graphs) = names(rds_completo)
    
    # graphs[[which(names(rds_completo) == "2021-04-12")]]
    # i = which(names(rds_completo) == "2021-04-12")
    
    saveRDS(graphs, paste(country, "_", rds.name, "_graph.rds", sep = ""))

  }
  
}
