##############################################################################

############# Summarize the results of BLRM in tables and plot ###############

##############################################################################

# convert the results of BLRM (4 lists) to data.frame can be used by ggplot ------------------------------------

trans.plot <- function(trans.results, formula, dose.levels, ewoc, int.cut, multiSce.var)
{
 # input data to be used
 pDLT.summary <- copy(trans.results$pDLT.Sce.summary)
 Interval.prop <- copy(trans.results$Interval.Sce.summary)
 
 # check is there any covariates ---------------------------------------------------------
 Check.name <- formula.check(paste(formula))
 DLT.name <- Check.name$DLT.name
 npat.name <- Check.name$npat.name
 drug.name <- Check.name$drug.name
 covariates <- Check.name$covariates
 
 # set id key for single/multiple scenario ----------------------------------------------------------------
 id.key <- c(multiSce.var, "Interval")[c(multiSce.var, "Interval")%in%colnames(Interval.prop)]
 
 # convert pDLT.summary --------------------------------------------------------------------------
 pDLT.summary[, (drug.name) := lapply(.SD, as.factor), .SDcols = drug.name]
 
 # convert Interval.prop -------------------------------------------------------------------------
 Interval.category <- names(table(cut(c(0, sort(int.cut, decreasing = FALSE), 1), breaks = c(0, sort(int.cut, decreasing = FALSE), 1), include.lowest = TRUE, right = FALSE)))
 
 # melt Interval.prop to long format
 Interval.prop.data <- Interval.prop %>% data.table::melt(id.vars = c(multiSce.var, drug.name, covariates, "EWOC"),
                                                          measure.vars = Interval.category,
                                                          variable.name = "Interval",
                                                          value.name = "Probability",
                                                          variable.factor = TRUE,
                                                          value.factor = FALSE)
 
 # split one column into two dose columns
 Interval.prop.data[, (drug.name) := lapply(.SD, function(x) factor(x, levels = unique(x))), .SDcols = drug.name]
 
 return(list(Interval.prop.data = Interval.prop.data,
             pDLT.summary.data = pDLT.summary))
}


# visualize BLRM output using ggplot ------------------------------------------------------------------------------------------

plot.summary <- function(trans.plot.data, formula, predict, ewoc, int.cut, dose.unit, multiSce.var)
{
 ########################################
 ########## plot by scenario ############
 ########################################

 pDLT.summary.data <- copy(trans.plot.data$pDLT.summary.data)
 Interval.prop.data <- copy(trans.plot.data$Interval.prop.data)
 Interval.category <- names(table(cut(c(0, sort(int.cut, decreasing = FALSE), 1), breaks = c(0, sort(int.cut, decreasing = FALSE), 1), include.lowest = TRUE, right = FALSE)))

 # check if mono or combo
 # Note: if 3 or more drugs are used, this code shoulded be edited
 # check is there any covariates ---------------------------------------------------------
 Check.name <- formula.check(paste(formula))
 DLT.name <- Check.name$DLT.name
 npat.name <- Check.name$npat.name
 drug.name <- Check.name$drug.name
 covariates <- Check.name$covariates
 n.drug <- length(drug.name)
 num.sce <- length(unique(Interval.prop.data[[multiSce.var]]))
 
 pDLT.ewoc.info <- Interval.prop.data[, .(EWOC.idx = any(EWOC)), by = c(multiSce.var, drug.name, covariates)]
 Interval.prop.data[, EWOC.idx := (Interval == tail(Interval.category, 1) & EWOC == TRUE), by = c(multiSce.var, drug.name, covariates)]
 pDLT.summary.data <- merge.data.table(pDLT.summary.data, pDLT.ewoc.info, by = c(multiSce.var, drug.name, covariates), sort = FALSE)

 # initiate the number of combinations of predictors
 num.predict <- 1
 if(!is.null(covariates)) 
 {
  # update the number of combinations of predictors
  num.predict <- nrow(predict)
  Interval.prop.data[, paste(covariates, collapse = ":") := apply(.SD, 1, paste, collapse = ":"), .SDcols = c(covariates)]
  pDLT.summary.data[, paste(covariates, collapse = ":") := apply(.SD, 1, paste, collapse = ":"), .SDcols = c(covariates)]
 }
 
 # Define theme used for plots -------------------------------------------------------------------------
 mono.theme <- theme_bw() + 
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               panel.spacing.x = unit(0.2, "lines"),
               panel.spacing.y = unit(0.2, "lines"),
               #axis.line = element_line(colour = "black"),
               #axis.text.x = element_text(angle = 45, hjust = 1),
               strip.background.x = element_rect(fill="wheat1"),
               strip.background.y = element_rect(fill="wheat1"),
               legend.position = "bottom", #c(0.9, 0.8),
               # legend.text = element_text(size=8),
               # legend.title = element_text(size = 8),
               legend.background = element_rect(fill = NA, size = 1),
               legend.key.width = unit(0.5, "lines"), 
               legend.key.height = unit(0.5, "lines"),
               legend.spacing.y = unit(0.1, "lines"),
               legend.box = "horizontal", # "vertical"
               legend.box.spacing = unit(0.1, "lines"),
               plot.title = element_text(face = "bold", vjust = -1),
               plot.subtitle=element_text(face="italic", color="black"))
 
 combo.theme <- theme_bw() + 
   theme(panel.background = element_blank(),
         panel.spacing.x = unit(0.2, "lines"),
         panel.spacing.y = unit(0.2, "lines"),
         #axis.line = element_line(colour = "black"),
         #axis.text.x = element_text(angle = 45, hjust = 1),
         strip.background.x = element_rect(fill="wheat1"),
         strip.background.y = element_rect(fill="wheat1"),
         legend.position = "bottom", #c(0.9, 0.8),
         # legend.text = element_text(size=8),
         # legend.title = element_text(size = 8),
         legend.background = element_rect(fill = NA, size = 1),
         legend.key.width = unit(0.5, "lines"), 
         legend.key.height = unit(0.5, "lines"),
         #legend.spacing.y = unit(0.1, "lines"),
         legend.box = "horizontal", # "vertical"
         legend.box.spacing = unit(0.1, "lines"),
         plot.title = element_text(face = "bold", vjust = -1),
         plot.subtitle=element_text(face="italic", color="black"))
 
 if(n.drug == 1)
 {
  # Bar/line plot for Interval Category --------------------------------------------------------------------
  if(is.null(covariates))
  {
    p.Interval <- ggplot(data = Interval.prop.data, aes(x = get(drug.name), y = Probability, fill = EWOC.idx)) +   
      geom_bar(stat = "identity", colour = "black", width = 0.5, size = 0) +
      geom_hline(data = Interval.prop.data[Interval==tail(Interval.category, 1)], aes(yintercept = ewoc), colour="red", linetype = 2)
  } else
  {
    # p.Interval <- ggplot(data = Interval.prop.data, aes(x = get(drug.name), y = Probability, group = get(paste(covariates, collapse = ":")), shape = get(paste(covariates, collapse = ":")), color = EWOC.idx)) + 
    #   geom_point(position = position_dodge2(0.8), 
    #              size = 4) +
    #   geom_linerange(aes(ymin = 0, ymax = Probability),
    #                  position = position_dodge2(width = 0.8)) +
    #   geom_hline(data = Interval.prop.data[Interval==tail(Interval.category, 1)], aes(yintercept = ewoc), colour="red", linetype = 2)
    p.Interval <- ggplot(Interval.prop.data, aes(x = get(drug.name), y = get(paste(covariates, collapse = ":")), size = Probability)) +
      geom_point(aes(fill = EWOC.idx), color = "black", shape = 21)
  }
         
  if(num.sce >1)
  {
   if(num.sce <= 8)
   {
    p.Interval <- p.Interval + 
       facet_grid(Scenario ~ Interval)
   } else
   {
    ncols <- ifelse(num.sce < 10, 3, ifelse(num.sce > 20, 9, 6))
    p.Interval <- p.Interval + 
            facet_wrap( ~ Scenario + Interval, ncol = ncols)
   }
  } else
  {
   p.Interval <- p.Interval + 
     facet_wrap( ~ Interval, nrow = length(Interval.category), ncol = 1)
  }
  
  if(is.null(covariates))
  {
   p.Interval <- p.Interval + 
     mono.theme +
     xlab(paste0(drug.name, " (", dose.unit, ")")) +
     ylab("Probability") +
     labs(title = "Interval probabilities by dose levels",
          subtitle = paste("EWOC = ", ewoc, sep = "")) +
     scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
     scale_fill_manual(name = "EWOC",
                       label = c("below", "above"),
                       values = c("green", "red"))
  } else
  {
   # p.Interval <- p.Interval +
   #   scale_color_manual(name = "EWOC",
   #                      label = c("below", "above"),
   #                      values = c('FALSE' = "#11E333", 'TRUE' = "tomato1"))
   # if(num.predict <= 4)
   # {
   #  p.Interval <- p.Interval +
   #    scale_shape_manual(name = paste(covariates, collapse = ":"),
   #                       values = (15:18)[1:num.predict])
   # } else
   # {
   #  p.Interval <- p.Interval +
   #   scale_shape_manual(name = paste(covariates, collapse = ":"))
   # }
   p.Interval <- p.Interval +
     mono.theme +
     xlab(paste0(drug.name, " (", dose.unit, ")")) +
     ylab(paste(covariates, collapse = ":")) +
     labs(title = "Interval probabilities by dose levels",
          subtitle = paste("EWOC = ", ewoc, sep = "")) +
     scale_size_continuous(range = c(0, 6)) +
     scale_fill_manual(name = "EWOC",
                       label = c("below", "above"),
                       values = c("green", "red"))
  }
  
  # line plot for posterior summary of probability of DLT by dose levels -------------------------------------------
  p.pDLT <- ggplot(pDLT.summary.data, aes(x = get(drug.name), fill = EWOC.idx))
    
  if(!is.null(covariates))
  {
   p.pDLT <- p.pDLT +
     geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`, color = get(paste(covariates, collapse = ":"))),
                    position = position_dodge2(width = 0.9),
                    size = 1) +
     geom_point(aes(y = Mean, shape = "Mean"),
                position = position_dodge2(width = 0.9),
                #shape = 23, 
                size = 3,
                colour = "black") +
     geom_point(aes(y = `50%`, shape = "Median"), 
                position = position_dodge2(width = 0.9),
                #shape = 21, 
                size = 3,
                colour = "black")
  } else
  {
   p.pDLT <- p.pDLT +
     geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`), 
                      size = 1) +
     geom_point(aes(y = Mean, shape = "Mean"), 
                # fill = "white",
                #shape = 23, 
                size = 3,
                colour = "black") +
     geom_point(aes(y = `50%`, shape = "Median"), 
                # fill= "white", 
                #shape = 21, 
                size = 3,
                colour = "black")
  }

   p.pDLT <- p.pDLT +
     geom_hline(yintercept = int.cut[1], linetype = 1, show.legend = TRUE) +
     geom_hline(yintercept = int.cut[2], linetype = 2, show.legend = TRUE)
   
  if(num.sce > 1)
  {
   p.pDLT <- p.pDLT +
     facet_wrap( ~ Scenario)
  }
  
  p.pDLT <- p.pDLT +
    mono.theme +
    xlab(paste0(drug.name, " (", dose.unit, ")")) +
    ylab("Probability") +
    labs(title = paste("Posterior distribution of DLT probability"),
         subtitle = "Percentile: 2.5% - 97.5%") +
    scale_y_continuous(breaks = c(0, int.cut, min(round(max(pDLT.summary.data$`97.5%`), 2) + 0.02, 1)),
                       limits = c(0, min(round(max(pDLT.summary.data$`97.5%`), 2) + 0.02, 1)),
                       expand = expansion(mult = c(0.02, 0.02))) +
    scale_shape_manual(name = "Statistic",
                       breaks = c("Mean", "Median"),
                       labels = c("Mean", "Median"),
                       values = c(23, 21)) +
    scale_fill_manual(name = "EWOC",
                      labels = c("below", "above"),
                      values = c('FALSE' = "#11E333", 'TRUE' = "tomato1"))
  if(!is.null(covariates))
  {
   p.pDLT <- p.pDLT +
     scale_colour_viridis_d(name = paste(covariates, collapse = ":")) +
     guides(fill = guide_legend(override.aes=list(shape=22)))
  } else
  {
   p.pDLT <- p.pDLT +
     guides(fill = guide_legend(override.aes=list(shape=22)))
  }
 } 
 
 # Combo case
 if(n.drug == 2)
 {
  p.Interval <- ggplot(Interval.prop.data, aes(x = get(drug.name[1]), y = get(drug.name[2]), size = Probability)) +
    geom_point(aes(fill = EWOC.idx), color = "black", shape = 21)

  if(num.sce >1)
  {
    if((num.sce * num.predict) <= 12)
    {
     if(is.null(covariates))
     {
      p.Interval <- p.Interval + 
        facet_grid(Scenario ~ Interval)
     } else
     {
      p.Interval <- p.Interval + 
        facet_grid(Scenario + get(paste(covariates, collapse = ":")) ~ Interval)
     }
    } else
    {
     ncols <- ifelse(num.sce < 15, 3, ifelse(num.sce > 20, 9, 6))
     if(is.null(covariates))
     {
      p.Interval <- p.Interval + 
        facet_wrap( ~ Scenario + Interval, ncol = ncols)
     } else
     {
      p.Interval <- p.Interval + 
        facet_grid(Scenario ~ Interval + get(paste(covariates, collapse = ":")))
     }
    }
  } else
  {
   if(is.null(covariates))
   {
    p.Interval <- p.Interval + 
      facet_wrap( ~ Interval, nrow = length(int.cut) + 1, ncol = 1)
   } else
   {
    p.Interval <- p.Interval + 
      facet_grid(get(paste(covariates, collapse = ":")) ~ Interval)
   }
  }
  
  p.Interval <- p.Interval +
    combo.theme +
    xlab(paste0(drug.name[1], " (", dose.unit[1], ")")) +
    ylab(paste0(drug.name[2], " (", dose.unit[2], ")")) +
    labs(title = "Interval probabilities by dose levels",
         subtitle = paste("EWOC = ", ewoc, if(is.null(covariates)) NULL else paste0(",", " Covariates = ", paste(covariates, collapse = ":")), sep = "")) +
    scale_size_continuous(range = c(0, 6)) +
    scale_fill_manual(name = "EWOC",
                      label = c("below", "above"),
                      values = c("green", "red"))
  
  # line plot for posterior summary of probability of DLT by dose levels -------------------------------------------
  num.dose <- do.call(data.frame, lapply(pDLT.summary.data[, .SD, .SDcols = c(drug.name)], function(x) length(unique(x)))) # %>% sort(decreasing = TRUE)
  # num.dose.order <- do.call(c, lapply(1:n.drug, function(i) which(grepl(i, colnames(num.dose)))))
  p.pDLT <- ggplot(pDLT.summary.data, aes(x = get(drug.name[1]))) +
    geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`, color = get(drug.name[2])),
                   position = position_dodge2(width = 0.8),
                   size = 1) +
    geom_hline(yintercept = int.cut[1], linetype = 1, show.legend = TRUE) +
    geom_hline(yintercept = int.cut[2], linetype = 2, show.legend = TRUE) +
    geom_point(aes(y = Mean, fill = EWOC.idx, shape = "Mean"), 
               position = position_dodge2(width = 0.8),
               #fill = "white",
               #shape = 23, 
               size = 3,
               colour = "black") +
    geom_point(aes(y = `50%`, fill = EWOC.idx, shape = "Median"), 
               position = position_dodge2(width = 0.8),
               #fill= "white", 
               #shape = 21, 
               size = 3,
               colour = "black")
  if(num.sce > 1)
  {
   if(is.null(covariates))
   {
    p.pDLT <- p.pDLT +
      facet_wrap( ~ Scenario)
   } else
   {
    p.pDLT <- p.pDLT +
      facet_grid(Scenario ~ get(paste(covariates, collapse = ":")))
   }
  } else
  {
   if(!is.null(covariates))
   {
    p.pDLT <- p.pDLT +
      facet_wrap( ~ get(paste(covariates, collapse = ":")))
   }
  }
  
  p.pDLT <- p.pDLT +
    combo.theme +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlab(paste0(drug.name[1], " (", dose.unit[1], ")")) +
    ylab("Probability") +
    labs(title = paste("Posterior distribution of DLT probability"),
         subtitle = paste0("Percentile: 2.5% - 97.5%", if(is.null(covariates)) NULL else paste0(",", " Covariates = ", paste(covariates, collapse = ":")))) +
    scale_y_continuous(breaks = c(0, int.cut, min(round(max(pDLT.summary.data$`97.5%`), 2)+ 0.02, 1)),
                       limits = c(0, min(round(max(pDLT.summary.data$`97.5%`), 2) + 0.02, 1)),
                       expand = expansion(mult = c(0.02, 0.02))) +
    scale_shape_manual(name = "Statistic",
                       breaks = c("Mean", "Median"),
                       labels = c("Mean", "Median"),
                       values = c(23, 21)) +
    scale_fill_manual(name = "EWOC",
                      breaks = c(FALSE, TRUE),
                      labels = c("below", "above"),
                      values = c("green", "tomato1")) +
    guides(fill = guide_legend(override.aes=list(shape=21))) +
    
    # solution 1: manual viridis
    scale_color_manual(name = paste0(drug.name[2], " (", dose.unit[2], ")"),
                       values = viridis::viridis_pal(direction = -1)(num.dose[[drug.name[2]]] + 1)[2:(num.dose[[drug.name[2]]]+1)])
  
    # solution 2: built-in YlGn palette
    # scale_color_brewer(name = colnames(num.dose)[2],
    #                    type = "sequential",
    #                    palette = "YlGn")
  
    # solution 3: built-in viridis palette
    # scale_color_viridis_d(name = colnames(num.dose)[2],
    #                       #discrete = TRUE,
    #                       direction = -1,
    #                       begin = 0.15,
    #                       end = 1) 
  }

 return(list(p.Interval = p.Interval,
             p.pDLT = p.pDLT))
}


# create table summary for BLRM -------------------------------------------------------------------------------------------------

table.summary <- function(trans.results, formula, data, trialInfo, bayesInfo, ewoc, int.cut, multiSce.var)
{
 ########################################################
 ####### summarize the results of BLRM by tables ########
 ########################################################

 # extract summarized info to be used -------------------------------------------------------------
 para.summary <- copy(data.table(trans.results$para.Sce.summary))
 pDLT.summary <- copy(data.table(trans.results$pDLT.Sce.summary))
 Interval.prop <- copy(data.table(trans.results$Interval.Sce.summary))
 NDR.summary <- copy(data.table(trans.results$NDR.Sce.summary))
 Scenario.cumu.data <- copy(data.table(data))

  # check is there any covariates ---------------------------------------------------------
 Check.name <- formula.check(paste(formula))
 DLT.name <- Check.name$DLT.name
 npat.name <- Check.name$npat.name
 drug.name <- Check.name$drug.name
 covariates <- Check.name$covariates
 n.drug <- length(drug.name)
 is.patwise <- is.null(npat.name)
 
 # set key if only one Scenario ----------------------------------------------------------------------------------------------
 Sce.key <- multiSce.var
 
 # further process Interval summary data ----------------------------------------------------------------
 Interval.category <- names(table(cut(c(0, sort(int.cut, decreasing = FALSE), 1), breaks = c(0, sort(int.cut, decreasing = FALSE), 1), include.lowest = TRUE, right = FALSE)))

 # create summary table 1: general info ----------------------------------------------------------------------------------------------
 General.info <- list(Date = format(Sys.Date(), "%Y-%m-%d"), 
                      Trial = trialInfo$trial.name,
                      Drug.name = trialInfo$drug.name,
                      Drug.unit = trialInfo$drug.unit)
 
 dose.info <- list('Dose levels' = trialInfo$dose.levels, 
                   'Reference dose' = trialInfo$ref.dose,
                   "EWOC" = trialInfo$ewoc,
                   "Category bounds" = trialInfo$int.cut)
 
 MCMC.info <- list('Prior mean' = bayesInfo$prior$mean, 
                   'Prior std' = bayesInfo$prior$std, 
                   'Prior corr' = bayesInfo$prior$corr, 
                   'MCMC samples' = bayesInfo$n.sample, 
                   'Burned MCMC samples' = bayesInfo$n.burnin, 
                   'Adaptive MCMC samples' = bayesInfo$n.adapt, 
                   'MCMC chains' = bayesInfo$n.chain, 
                   'MCMC thins' = bayesInfo$n.thin)
 
 General.info.summary <- data.frame(as.list(do.call(c, General.info)), stringsAsFactors = FALSE)
 colnames(General.info.summary) <- c("Date", "Trial", paste0("Drug", 1:n.drug, ".name"), paste0("Drug", 1:n.drug, ".unit"))
 General.info.summary$`R Version` <- paste(version$major, version$minor, sep = ".")
 
 # create summary table 2: dose info (include ewoc, intreval cutoff points)--------------------------------------------------------------------------------------------------
 dose.info.summary <- dose.info[which(!(names(dose.info)%in%"Dose levels"))]
 dose.list <- dose.info$`Dose levels`
 dose.info.summary <- c(dose.list, dose.info.summary)  
 dose.info.summary <- plyr::ldply(dose.info.summary, rbind)
 
 # create summary table 3: MCMC info -----------------------------------------------------------------------------------------------------
 MCMC.info.summary <- MCMC.info[which(!(names(MCMC.info)%in%c("Prior mean", "Prior std", "Prior corr")))]
 MCMC.prior.mean <- MCMC.info[["Prior mean"]]
 MCMC.prior.std <- MCMC.info[["Prior std"]]
 MCMC.prior.corr <- MCMC.info[["Prior corr"]]
 names(MCMC.prior.mean) <- c(paste0(drug.name, " Prior Mean"), if(length(dose.list) > 1) "Inter Prior Mean" else NULL)
 names(MCMC.prior.std) <- c(paste0(drug.name, " Prior Std"), if(length(dose.list) > 1) "Inter Prior Std" else NULL)
 names(MCMC.prior.corr) <- paste0(drug.name, " Prior corr")
 MCMC.info.summary <- c(MCMC.prior.mean, MCMC.prior.std, MCMC.prior.corr, MCMC.info.summary)
 MCMC.info.summary <- plyr::ldply(MCMC.info.summary, rbind)
 MCMC.info.summary <- rbind(MCMC.info.summary[1:(nrow(MCMC.info.summary)-5), ], " " = c(NA, NA), MCMC.info.summary[(nrow(MCMC.info.summary)-4):nrow(MCMC.info.summary),])
 
 # round summary statistics in pDLT.summary ---------------------------------------------------------------------------------------------------------
 Summary.stat <- c("Mean", "Std", "2.5%", "50%", "97.5%")
 pDLT.summary[, (Summary.stat) := round(.SD, 4), .SDcols = Summary.stat]
 
 # create summary table 4: (faster than overall for loop); (sum(Scenario.cumu.data$npat)==0) accounts for Prior only when npat = 0 --------------------------------------------------------------------------------------
 if(is.patwise | (sum(Scenario.cumu.data$npat)==0)) Scenario.cumu.data <- Scenario.cumu.data[, .(npat = if("npat"%in%colnames(Scenario.cumu.data)) 0 else .N, nDLT = sum(get(DLT.name))), by = c(Sce.key, drug.name)]
 Scenario.output.summary <- Scenario.cumu.data[, lapply(.SD, paste, collapse = ", "), by = Sce.key, .SDcols = c(drug.name, "npat", "nDLT")]
 Scenario.output.summary <- data.table::merge.data.table(Scenario.output.summary, NDR.summary, by = Sce.key)
 
 # round summary statistics in para.summary ------------------------------------------------------------------------------------------------------------
 para.summary[, colnames(para.summary)[!(colnames(para.summary)%in%Sce.key)] := round(.SD, 4), .SDcols = colnames(para.summary)[!(colnames(para.summary)%in%Sce.key)]]
 
 # Interval.prop drop EWOC column ---------------------------------------------------------------------------------------------------------------------------------
 Interval.prop[, EWOC := NULL,]
 
 # reorder cols
 setcolorder(pDLT.summary, c(Sce.key, drug.name, covariates, Summary.stat))
 setcolorder(Interval.prop, c(Sce.key, drug.name, covariates, Interval.category))
 setcolorder(Scenario.output.summary, c(Sce.key, drug.name, "npat", "nDLT", covariates, paste0(drug.name, ".NDR"), Summary.stat, Interval.category))
 
 return(list(para.Sce.summary = para.summary,
             pDLT.Sce.summary = pDLT.summary,
             Interval.Sce.summary = Interval.prop,
             Scenario.output.summary = Scenario.output.summary,
             General.info = General.info.summary,
             dose.info = dose.info.summary,
             MCMC.info = MCMC.info.summary))
}


# create excel workbook -------------------------
table.summary.output<-function(res.summary, formula = formula, data, multiSce.var, figs = NULL, file.prefix, code.file.name, ...)
{
 # extract results -------------------------------------------------------------------
 General.info.summary <- res.summary$General.info
 dose.ewoc.info.summary <- res.summary$dose.info
 MCMC.info.summary <- res.summary$MCMC.info
 para.Sce.summary <- copy(res.summary$para.Sce.summary)
 pDLT.Sce.summary <- copy(res.summary$pDLT.Sce.summary)
 Interval.Sce.summary <- copy(res.summary$Interval.Sce.summary)
 Scenario.output.summary <- copy(res.summary$Scenario.output.summary)
 
 # Check is there any covariates --------------------------------------------------------------
 Check.name <- formula.check(paste(formula))
 DLT.name <- Check.name$DLT.name
 npat.name <- Check.name$npat.name
 drug.name <- Check.name$drug.name
 covariates <- Check.name$covariates
 n.drug <- length(drug.name)

 pDLT.Interval.Sce.summary <- data.table::merge.data.table(pDLT.Sce.summary, Interval.Sce.summary, by = c(multiSce.var, drug.name, covariates), sort = FALSE)
 
 data.BLRM <- copy(data)
 num.Sce <- length(para.Sce.summary$Scenario)

 code.info <- data.frame(name = c("Core", "Summary"), file.name = code.file.name)
 
 # create excel file -----------------------------------------------------------------
 output.excel <- createWorkbook()

 title.style <- createStyle(fontSize = 14, 
                            fontColour = "#ffffff",
                            # border = c("top", "bottom", "left", "right"), 
                            # borderStyle = rep("medium", 4), 
                            fgFill = "#A50026",
                            halign = "center", 
                            valign = "center", 
                            textDecoration = "bold")

 head.style <- createStyle(fontSize = 12, 
                           fontColour = "#ffffff",
                           #border = c("top", "bottom", "left", "right"), 
                           #borderStyle = rep("medium", 4), 
                           fgFill = "#313695",
                           halign = "center", 
                           valign = "center", 
                           textDecoration = "bold")

 body.style <- createStyle(#fontSize = 11,
                           halign = "center",
                           valign = "center")

 # create sheet 1: general info and data sets ----------------------------------------------------------------
 row.idx <- 1

 addWorksheet(wb = output.excel, sheetName = "Info Review", gridLines = FALSE)

 # write title for sheet 1
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = "Review of Model Information", 
           startCol = 1,
           startRow = row.idx)
 mergeCells(wb = output.excel, sheet = "Info Review", cols = 1:max(ncol(General.info.summary), ncol(dose.ewoc.info.summary), ncol(MCMC.info.summary)), rows = row.idx)
 addStyle(wb = output.excel, sheet = "Info Review", style = title.style, cols = 1, rows = row.idx)

 row.idx <- row.idx + 2

 # write Date and Drug info for sheet 1 -------------------------------------------------------------------
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = General.info.summary, 
           startCol = 1, 
           startRow = row.idx,
           rowNames = FALSE, 
           colNames = TRUE, 
           headerStyle = head.style, 
           borders = "surrounding",
           borderStyle = "medium")
 addStyle(wb = output.excel, sheet = "Info Review", style = body.style, cols = 1:ncol(General.info.summary), rows = row.idx + 1, gridExpand = TRUE, stack = TRUE)
 
 row.idx <- row.idx + nrow(General.info.summary) + 1 + 1

 # write General information: dose, ewoc, interval cut used in the BLRM -------------------------------------------------------------
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = "General Info: dose, ewoc", 
           startCol = 1, 
           startRow = row.idx, 
           headerStyle = head.style, 
           borders = "surrounding",
           borderStyle = "medium")
 mergeCells(wb = output.excel, sheet = "Info Review", cols = 1:ncol(dose.ewoc.info.summary), rows = row.idx)
 addStyle(wb = output.excel, sheet = "Info Review", style = head.style, cols = 1, rows = row.idx)

 row.idx <- row.idx + 1

 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = dose.ewoc.info.summary,
           startCol = 1, 
           startRow = row.idx, 
           borders = "surrounding",
           borderStyle = "medium",
           colNames = FALSE)
 
 addStyle(wb = output.excel, sheet = "Info Review", style = body.style, cols = 1:ncol(dose.ewoc.info.summary), rows = row.idx:(row.idx + nrow(dose.ewoc.info.summary) - 1), gridExpand = TRUE, stack = TRUE)
 
 row.idx <- row.idx + nrow(dose.ewoc.info.summary) + 1

 # write MCMC info into excel -------------------------------------------------------------------
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = "MCMC Info", 
           startCol = 1, 
           startRow = row.idx, 
           borders = "surrounding",
           borderStyle = "medium")
 mergeCells(wb = output.excel, sheet = "Info Review", cols = 1:ncol(MCMC.info.summary), rows = row.idx)
 addStyle(wb = output.excel, sheet = "Info Review", style = head.style, cols = 1, rows = row.idx)

 row.idx <- row.idx + 1
 Prior.head <- if(is.null(covariates)) data.frame(Prior.Info = "Prior Info", log.a = "log(alpha)", log.b = "log(beta)", stringsAsFactors = FALSE) else data.frame(Prior.Info = "Prior Info", log.a = "log(alpha)", log.b = "log(beta)", as.list(covariates), stringsAsFactors = FALSE)
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = Prior.head, 
           startCol = 1, 
           startRow = row.idx,
           colNames = FALSE)
 addStyle(wb = output.excel, sheet = "Info Review", style = head.style, cols = 1:ncol(Prior.head), rows = row.idx, gridExpand = TRUE)

 row.idx <- row.idx + 1
 
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = MCMC.info.summary, 
           startCol = 1, 
           startRow = row.idx, 
           colNames = FALSE,
           borders = "surrounding",
           borderStyle = "medium")
 addStyle(wb = output.excel, sheet = "Info Review", style = body.style, cols = 1:ncol(MCMC.info.summary), rows = row.idx:(row.idx + nrow(MCMC.info.summary) - 1), gridExpand = TRUE, stack = TRUE)

 row.idx <- row.idx + nrow(MCMC.info.summary) + 1
 
 # write code info into excel ----------------------------------------------------------------------------------------
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = "Code Info", 
           startCol = 1, 
           startRow = row.idx, 
           colNames = FALSE,
           borders = "surrounding",
           borderStyle = "medium")
 mergeCells(wb = output.excel, sheet = "Info Review", cols = 1:ncol(code.info), rows = row.idx)
 addStyle(wb = output.excel, sheet = "Info Review", style = head.style, cols = 1, rows = row.idx)
 
 row.idx <- row.idx + 1
 writeData(wb = output.excel, 
           sheet = "Info Review", 
           x = code.info, 
           startCol = 1, 
           startRow = row.idx, 
           colNames = FALSE,
           borders = "surrounding",
           borderStyle = "medium")
 addStyle(wb = output.excel, sheet = "Info Review", style = body.style, cols = 1:ncol(code.info), rows = row.idx:(row.idx + nrow(code.info) - 1), gridExpand = TRUE, stack = TRUE)
 
 # set column widths sheet 1
 s1.col1.width <- max(c(nchar(dose.ewoc.info.summary$.id), nchar(General.info.summary$Date), nchar(MCMC.info.summary$.id)), na.rm = TRUE) + 2
 s1.col2.width <- max(c(nchar(General.info.summary$Trial), nchar(Prior.head))) + 2
 setColWidths(wb = output.excel, sheet = "Info Review", cols = 1, widths = s1.col1.width, ignoreMergedCells = TRUE)
 setColWidths(wb = output.excel, sheet = "Info Review", cols = 2:max(ncol(dose.ewoc.info.summary), ncol(MCMC.info.summary)), widths = s1.col2.width, ignoreMergedCells = TRUE)
 
 # write sheet 2: data and next dose results ------------------------------------------------------
 addWorksheet(wb = output.excel, sheetName = "Next dose", gridLines = FALSE)
 row.idx <- 1

 writeData(wb = output.excel, 
           sheet = "Next dose", 
           x = "Review of Next Dose Recommendation", 
           startCol = 1,
           startRow = row.idx)
 mergeCells(wb = output.excel, sheet = "Next dose", cols = 1:6, rows = row.idx)
 addStyle(wb = output.excel, sheet = "Next dose", style = title.style, cols = 1, rows = row.idx)

 row.idx <- row.idx + 2

 # write next dose summary
 writeData(wb = output.excel,
           sheet = "Next dose",
           x = "Next Dose Summary", 
           startCol = 1, 
           startRow = row.idx)
 mergeCells(wb = output.excel, sheet = "Next dose", cols = 1:ncol(Scenario.output.summary), rows = row.idx)
 addStyle(wb = output.excel, sheet = "Next dose", style = head.style, cols = 1, rows = row.idx)
 
 row.idx <- row.idx + 1
 
 writeData(wb = output.excel, 
           sheet = "Next dose", 
           x = Scenario.output.summary,
           startCol = 1, 
           startRow = row.idx, 
           headerStyle = head.style,
           colNames = TRUE,
           rowNames = FALSE,
           borders = "surrounding",
           borderStyle = "medium")
 addStyle(wb = output.excel, sheet = "Next dose", style = body.style, cols = 1:ncol(Scenario.output.summary), rows = (row.idx + 1):(row.idx + nrow(Scenario.output.summary)), gridExpand = TRUE, stack = TRUE)
 
 row.idx <- row.idx + nrow(Scenario.output.summary) + 1 + 1
 
 # write data
 writeData(wb = output.excel,
           sheet = "Next dose",
           x = "Data by Scenario", 
           startCol = 1, 
           startRow = row.idx)
 mergeCells(wb = output.excel, sheet = "Next dose", cols = 1:ncol(data.BLRM), rows = row.idx)
 addStyle(wb = output.excel, sheet = "Next dose", style = head.style, cols = 1, rows = row.idx)

 row.idx <- row.idx + 1

 writeData(wb = output.excel, 
           sheet = "Next dose", 
           x = data.BLRM, 
           startCol = 1, 
           startRow = row.idx, 
           colNames = TRUE, 
           rowNames = FALSE, 
           headerStyle = head.style, 
           borders = "surrounding", 
           borderStyle = "medium")
 addFilter(wb = output.excel, sheet = "Next dose", rows = row.idx, cols = 1)
 addStyle(wb = output.excel, sheet = "Next dose", style = body.style, cols = 1:ncol(data.BLRM), rows = (row.idx + 1):(row.idx + nrow(data.BLRM)), gridExpand = TRUE, stack = TRUE)
 
 row.idx <- row.idx + nrow(data.BLRM) + 1 + 1
 
 # set column width sheet 2
 s2.col.width <- max(nchar(colnames(Scenario.output.summary))) + 2
 setColWidths(wb = output.excel, sheet = "Next dose", cols = 1:max(ncol(data.BLRM), ncol(Scenario.output.summary)), widths = s2.col.width, ignoreMergedCells = TRUE)
 
 # write sheet 3: BLRM summary -------------------------------------------------------------------------------
 # create sheet 3: Interval Probability if add filter ---------------------------------------------------------------------------------------------------------
  addWorksheet(wb = output.excel, sheetName = "Summary", gridLines = FALSE)
  row.idx <- 1
  
  writeData(wb = output.excel, 
            sheet = "Summary", 
            x = "Estimates of parameters",
            startCol = 1, 
            startRow = row.idx)
  mergeCells(wb = output.excel, sheet = "Summary", cols = 1:ncol(para.Sce.summary), rows = row.idx)
  addStyle(wb = output.excel, sheet = "Summary", style = title.style, cols = 1, rows = row.idx)
  
  row.idx <- row.idx + 1
  
  writeData(wb = output.excel, 
            sheet = "Summary", 
            x = para.Sce.summary, 
            startCol = 1, 
            startRow = row.idx, 
            colNames = TRUE, 
            rowNames = FALSE, 
            headerStyle = head.style, 
            borders = "surrounding", 
            borderStyle = "medium")
  addStyle(wb = output.excel, sheet = "Summary", style = body.style, cols = 1:ncol(para.Sce.summary), rows = (row.idx + 1):(row.idx + nrow(para.Sce.summary)), gridExpand = TRUE, stack = TRUE)
  row.idx <- row.idx + nrow(para.Sce.summary) + 1 + 1
  
  writeData(wb = output.excel, 
           sheet = "Summary", 
           x = "Summary of DLT Probability",
           startCol = 1, 
           startRow = row.idx)
  mergeCells(wb = output.excel, sheet = "Summary", cols = 1:ncol(pDLT.Interval.Sce.summary), rows = row.idx)
  addStyle(wb = output.excel, sheet = "Summary", style = title.style, cols = 1, rows = row.idx)
  
  row.idx <- row.idx + 1
  
  writeData(wb = output.excel,
           sheet = "Summary",
           x = pDLT.Interval.Sce.summary,
           startCol = 1,
           startRow = row.idx, 
           colNames = TRUE, 
           rowNames = FALSE, 
           headerStyle = head.style, 
           borders = "surrounding", 
           borderStyle = "medium")
  addFilter(wb = output.excel, sheet = "Summary", cols = 1:(n.drug + 1 + length(covariates)), rows = row.idx)
  addStyle(wb = output.excel, sheet = "Summary", style = body.style, cols = 1:ncol(pDLT.Interval.Sce.summary), rows = (row.idx + 1):(row.idx + nrow(pDLT.Interval.Sce.summary)), gridExpand = TRUE, stack = TRUE)
  
  row.idx <- row.idx + nrow(pDLT.Interval.Sce.summary) + 1 + 1
  
  # set column width for sheet 3 -----------------------------------------------------------------------------------------------------------------------------------------
  s3.col.width <- max(c(nchar(colnames(pDLT.Interval.Sce.summary)), nchar(colnames(para.Sce.summary)))) + 2
  setColWidths(wb = output.excel, sheet = "Summary", cols = 1:max(ncol(pDLT.Interval.Sce.summary), ncol(para.Sce.summary)), widths = s3.col.width, ignoreMergedCells = TRUE)

 if(!is.null(figs))
 {
  # save figs first ------------------------------------------------------------
  ggsave(filename = paste0(file.prefix, 1, ".png"), figs[[1]], device = "png", ...) 
  ggsave(filename = paste0(file.prefix, 2, ".png"), figs[[2]], device = "png", ...)
  
  # insert figures -------------------------------------------------------------
  addWorksheet(wb = output.excel, sheetName = "Interval_Plot", gridLines = FALSE)
  addWorksheet(wb = output.excel, sheetName = "Summary_Plot", gridLines = FALSE)
  insertImage(wb = output.excel, sheet = "Interval_Plot", file = paste0(file.prefix, 1, ".png"), ...)
  insertImage(wb = output.excel, sheet = "Summary_Plot", file = paste0(file.prefix, 2, ".png"), ...)
 }
 
 output.excel
}
