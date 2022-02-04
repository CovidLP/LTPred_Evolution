plot_graph2 = function(ajuste, dates, dates_14, seq_y, lab_y){
  
  ajuste_df = suppressMessages(dates %>%
                                 full_join(data) %>%
                                 full_join(ajuste$mu_plot) %>%
                                 full_join(ajuste$lt_predict) %>%
                                 mutate(prev = is.na(m), 
                                        y = ifelse(prev, y, NA)) %>%
                                 filter(date <= last.date))
  
  plot(ajuste_df[, c("date", "y")], col = NA, ylim = c(0, max(seq_y)),
       bty = "n", axes = FALSE, xlab = "", ylab = "", cex.main = 0.9,
       main = paste("Brazil /", state, title.name))
  
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
  
  if(confirmed){
    points(ajuste_df[, c("date", "y")], type = "l", col = "#648cf0")
    points(ajuste_df[, c("date", "y")], col = "#648cf0", pch = 16)
    points(ajuste_df[, c("date", "y")], col = "#10167b")
  } else{
    points(ajuste_df[, c("date", "y")], type = "l", col = "#641e1e")
    points(ajuste_df[, c("date", "y")], col = "#c81e1e", pch = 16)
    points(ajuste_df[, c("date", "y")], col = "#641e1e")
  }
  
  if(length(ajuste$lt_summary$high.dat.low) > 0 & length(ajuste$lt_summary$high.dat.upper) > 0 ){
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
  }
  
  if(length(ajuste$lt_summary$end.dat.low) > 0 & length(ajuste$lt_summary$end.dat.upper) > 0 ){
    if(!is.na(ajuste$lt_summary$end.dat.low) & !is.na(ajuste$lt_summary$end.dat.upper)){
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
    }
  }
  
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
  
}
