

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