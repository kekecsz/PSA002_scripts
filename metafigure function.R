metafigure=function(model, studynames, total_N, main = "") {
  op = par(cex = 0.8, font = 1)
  studynames = deparse(substitute(studynames))
  total_N = deparse(substitute(total_N))
  
  call <- model$call
  data <- get(as.character(call$data))
  
  #name of the first moderator called in the model (if any)
  first_mod_called = as.character(call$mods)[2]
  
  if(!is.na(first_mod_called)){
    call_without_mods = call[-which(names(call) == "mods")]
    
    new_call = call_without_mods
    new_call$data = as.name("data")
    new_model_full_without_mods <- eval(new_call)
    
    # for fresher info see http://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups
    
    # calculate the structure of the forest plot with subgroups
    blank_row_n = 4 # number of blank rows to insert at group borders
    n_of_studies_per_subgroup <- rev(as.numeric(table(data[,first_mod_called])[table(data[,first_mod_called]) > 0])) # number of studies per subgroup (reverted because it will be displayed from bottom to top order)
    subgroup_names <- rev(names(table(data[,first_mod_called])[table(data[,first_mod_called]) > 0]))
    where_to_insert_blank_with_top <- cumsum(n_of_studies_per_subgroup[1:length(n_of_studies_per_subgroup)]) #positions of group bounderies
    where_to_insert_blank <- where_to_insert_blank_with_top[1:(length(where_to_insert_blank_with_top)-1)]
    # vector containing the row numbers of the forst plot where the study results should be displayed
    rows_to_print_results <- 1:length(data[,first_mod_called])
    rows_to_print_results = add.blank.at(vector = rows_to_print_results, pos = where_to_insert_blank, how_much_blank_to_add = blank_row_n)
    rows_to_print_results = rows_to_print_results+(blank_row_n-2)
    last_row <- rows_to_print_results[length(rows_to_print_results)] # last row
    
    identify_gaps <- NA
    for(i in 1:(length(rows_to_print_results)-1)){
      identify_gaps[i] <- rows_to_print_results[i+1] - rows_to_print_results[i]
    }
    to_insert <- as.data.frame(matrix(NA, ncol = 5, nrow = length(subgroup_names)))
    names(to_insert) <- c("subgroup_names", "where_to_insert_groupnames", "where_to_insert_model_summary", "total_N")
    to_insert[,"subgroup_names"] = subgroup_names
    to_insert[,"where_to_insert_groupnames"] <- c(rows_to_print_results[which(identify_gaps > 1)]+1, last_row+1)
    for(i in 1:length(to_insert[,"subgroup_names"])){
      to_insert[i,"where_to_insert_model_summary"] <- as.numeric(to_insert[i,"where_to_insert_groupnames"] - table(data[,first_mod_called])[table(data[,first_mod_called]) > 0][names(table(data[,first_mod_called])[table(data[,first_mod_called]) > 0]) == to_insert[i,"subgroup_names"]]) - 1
      to_insert[i,"total_N"] <- sum(data[data[,first_mod_called] == to_insert[i,"total_N"],total_N])
    }
    
    forest.rma(new_model_full_without_mods,
               slab = paste("    ", data[ ,studynames], sep = ""),
               alim=c(-3,3),
               xlim=c(-10,6),
               at = c(-2.00, -1.00, 0.00, 1.00, 2.00),
               ilab = data[ ,total_N],
               ilab.xpos = -5,
               ylim=c(-1, last_row+4),
               order=rev(order(data[,first_mod_called])),
               rows=rows_to_print_results,
               mlab="RE Model for All Studies",
               cex = 0.8,
               main = main)    
    
    
    
    ### set font expansion factor (as in forest() above) and use bold italic
    ### font and save original settings in object 'op'
    op <- par(cex=.7, font=4)
    
    ### add text for the subgroups
    text(-10, c(to_insert[,"where_to_insert_groupnames"]), pos=4,   to_insert[,"subgroup_names"])
    
    
    
    # fit random-effects model in the subgroups if k > 1
    for(i in 1:length(subgroup_names)){
      if(sum(data[,first_mod_called] == subgroup_names[i]) > 1){
        res_subgroup <- update(new_model_full_without_mods, subset=data[,first_mod_called] == subgroup_names[i])
        Q_subgroup <- round(res_subgroup$QE, 2)
        I_squared_subgroup <- round(calc_I_squared(res_subgroup), 2)
        op <- par(cex=.75, font=4)
        addpoly(res_subgroup, row=to_insert[i,"where_to_insert_model_summary"], cex=.8, mlab="RE Model for Subgroup", col = "white")
        op <- par(cex=.69, font=4)
        text(-5, to_insert[i,"where_to_insert_model_summary"], to_insert[i,"total_N"])
        op <- par(cex=.7, font=1)
        text(-10, to_insert[i,"where_to_insert_model_summary"]-1, pos=4, paste("Heterogeneity for subgroup: Q = ", Q_subgroup, " , ", expression(I^2), " = ", I_squared_subgroup, "%", sep = ""))
      } else {
        op <- par(cex=.7, font=1)
        text(-10, to_insert[i,"where_to_insert_model_summary"], pos=4, "Subgroup summary not produced because k = 1")
      }
      op <- par(cex=.75, font=1)
    }
    
    header_row_pos <- last_row + 3
    heter_total_Q <- round(model$QE, 2)
    I_squared_total <- round(calc_I_squared(model), 2)
    
  } else {
    forest.rma(model,
               slab = paste("    ", data[,studynames], sep = ""),
               alim=c(-3,3),
               xlim=c(-10,6),
               at = c(-2.00, -1.00, 0.00, 1.00, 2.00),
               ilab = data[ ,total_N],
               ilab.xpos = -5,
               cex = 0.8,
               main = main)
    
    header_row_pos <- model$k + 2
    heter_total_Q <- round(model$QE, 2)
    I_squared_total <- round(calc_I_squared(model), 2)
    
  }
  
  N_all_combined = sum(data[,total_N])
  N_control_total = sum(data[,total_N])
  
  text(-5, -1, N_all_combined, cex = 0.8)
  
  op = par(cex = 0.8, font = 2)
  
  
  text(-5, header_row_pos, "Sample size")
  text(-10, header_row_pos, "Author(s) and Year", pos = 4)
  text(6, header_row_pos, "Fisher's Z [95% CI]", pos = 2)
  text(-0.5, header_row_pos+1, "Negative", pos = 2)
  text(0.5, header_row_pos+1, "Positive", pos = 4)
  text(-0.5, header_row_pos, "relationship", pos = 2)
  text(0.5, header_row_pos, "relationship", pos = 4)
  op = par(cex = 0.7, font = 1)
  
  text(-10, -2, pos=4, paste("Heterogeneity for All Studies: Q = ", heter_total_Q, " , ", expression(I^2), " = ", I_squared_total, "%", sep = ""))
  op = par(cex = 0.8, font = 1)
  
  if(!is.na(first_mod_called)){
    if(is.numeric(data[,first_mod_called])){
      ### calculate predicted relative risks for 0 to 60 degrees absolute latitude
      preds <- predict(model, newmods=c(0:max(data[,first_mod_called]+2)))
      
      ### calculate point sizes by rescaling the standard errors
      SE    <- 1/sqrt(model$vi)
      size  <- 0.5 + 3.0 * (SE - min(SE))/(max(SE) - min(SE))
      
      ### plot the relative risks against absolute latitude
      plot(data[,first_mod_called], model$yi, pch=19, cex=size,
           xlab=first_mod_called, ylab="Effect Size (g)",
           las=1, bty="l")
      
      ### add predicted values (and corresponding CI bounds)
      lines(0:max(data[,first_mod_called]+2), preds$pred)
      lines(0:max(data[,first_mod_called]+2), preds$ci.lb, lty="dashed")
      lines(0:max(data[,first_mod_called]+2), preds$ci.ub, lty="dashed")
    }
  }
}  


# function was adapted from here: http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
# runs with models with moderators as well (although not sure how accurate)
calc_I_squared <- function(mod){
  W <- diag(1/mod$vi)
  X <- model.matrix(mod)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  I_squared <- 100 * sum(mod$sigma2) / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P)))
  return(I_squared)
}


# add blanks into vector at specific positions
add.blank.at <- function(vector, pos, how_much_blank_to_add){
  result <- vector("list",length(pos)+1)
  result <- split(vector, cumsum(seq_along(vector) %in% (pos+1)))
  for(i in 1:length(pos)){
    result[[i+1]] = result[[i+1]] + how_much_blank_to_add*i
  }
  result = as.vector(unlist(result))
  return(result)
}