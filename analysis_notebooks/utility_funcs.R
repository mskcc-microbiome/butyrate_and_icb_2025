makeSpline <- function(df, survtime, survevent, var,
                       knots=knots, centerpoint = NULL,
                       title=paste("Spline with", knots, "knots"),
                       xlab = xlab, ylab=ylab,
                       xticks = NULL) {
  
  require(survival)
  require(tidyverse)
  
  model_spline <- coxph(Surv(survtime, survevent)~rms::rcs(var, knots), df) #
  ptemp <- termplot(model_spline, se=T, plot=F)
  
  splinem <- ptemp$var
  
  if(is.null(centerpoint)){centerpoint = median(var, na.rm=T)}
  
  if(centerpoint %in% splinem$x){
    c_row = which(splinem$x == centerpoint)
  } else {
    c_row = which(abs(splinem$x - centerpoint) == min(abs(splinem$x - centerpoint)))
  }
  
  center <- splinem$y[c_row]
  
  ytemp <- splinem$y + outer(splinem$se, c(0, -1.96, 1.96), '*')
  
  exp_ytemp <- exp(ytemp - center)
  
  # Create a spline_data frame for plotting
  spline_data <- data.frame(xvar = splinem$x, Estimate = exp_ytemp[,1],
                            Lower = exp_ytemp[,2], Upper = exp_ytemp[,3])
  
  ggplot(spline_data, aes(x = xvar)) +
    geom_hline(yintercept=1, lty=2)+
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.5) +
    geom_line(aes(y = Estimate), color = "blue") +
    geom_rug(sides = "b") +  # Add rug plot at the bottom ('b') of the plot
    scale_y_log10() +  # Log scale for y-axis
    scale_x_log10() +
    labs(x = xlab, y = ylab, title=title) +
    theme_classic()
}


save_figures <- function(name_base, save_date = F, w = 8, h = 8){
  if (save_date) { # allows for snapshots to be saved based on date:
    ggsave(paste0("figures/svg/all_versions/", Sys.Date(), "_", name_base, ".svg"),width=w, height=h)
    ggsave(paste0("figures/png/all_versions/", Sys.Date(), "_", name_base, ".png"),width=w, height=h)
  }
  ggsave(paste0("figures/svg/", name_base, ".svg"),width=w, height=h)
  ggsave(paste0("figures/png/", name_base, ".png"),width=w, height=h)
  return(paste0("figures/png/", name_base, ".png"))
}

ggsave_survival_workaround <- function(g){survminer:::.build_ggsurvplot(x = g,
                                                                        surv.plot.height = NULL,
                                                                        risk.table.height = NULL,
                                                                        ncensor.plot.height = NULL)}


ggsurv_save <- function(plt, name_base, save_date = F, w = 8, h = 8){
  fixed_p <- ggsave_survival_workaround(plt)
  if (save_date) { # allows for snapshots to be saved based on date:
    ggsave(paste0("svg/dated_versions/", Sys.Date(), "_", name_base, ".svg"),width=w, height=h)
    ggsave(paste0("png/dated_versions/", Sys.Date(), "_", name_base, ".png"),width=w, height=h)
  }
  ggsave(file=paste0("svg/", name_base, ".svg"), plot=fixed_p, width=w, height=h)
  ggsave(file=paste0("png/", name_base, ".pngg"), plot=fixed_p, width=w, height=h)
}