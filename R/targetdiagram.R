target_diagram_data <- function(df, aggrlist) { 
  
  # Function to compute variables for a target diagram:
  # script will expect a dataframe with 5 columns, in fixed order:
  #   1. parameter: name of the parameter
  #   2. location: name of the location
  #   3. date/time/year: label
  #   4. D: Observed value
  #   5. M: Modelled value
  # a list of aggregation fields, for instance parameter, location, time
  library(plyr)
  
  # 1. Compute statistcs: avg and sd per location, parameter
  data_stat <- ddply(df, agglist, summarize, M_avg = mean(M), D_avg = mean(D), M_sd = sd(M), D_sd = sd(D))
  
  # 2. Compute sign based on difference of sd:
  data_stat$sgn <- sign(data_stat$M_sd -data_stat$D_sd) 
  
  # 3. Compute Bias based on avg_modeled - avg_measured:
  data_stat$bias <- data_stat$M_avg - data_stat$D_avg
  
  # 4. Compute B*:
  data_stat$bias_s <- data_stat$bias / data_stat$D_sd
  
  # 5. Merge df and statistics:
  df <- merge(df,data_stat, by=agglist)
  
  # 6. Compute Res.
  df$D_res <- df$D - df$D_avg
  df$M_res <- df$M - df$M_avg
  
  # 7. Compute Residue Differenced Squared:
  df$RMSD <- (df$M_res - df$D_res)^2
  
  # 8. Aggregate to get MRDS (Mean Residue Squared)
  data_RMSD <- ddply(df, agglist, summarize, MRDS = mean(RMSD))
  
  # 9. Merge datastat's:
  target <- merge(data_stat,data_RMSD, by=agglist)
  
  # 10. Compute RMSD:
  target$RMSD <- sqrt(target$MRDS)
  
  # 11. Compute RMSD*:
  target$RMSD_s <- target$RMSD * target$sgn / target$D_sd
  
  return(target)
  
}

# function for creating a circle:
crt_circle <- function(center = c(0,0),radius = 1, npoints = 100){
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}
