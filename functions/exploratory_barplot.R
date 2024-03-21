exp_plot<- function(data, outcome_var, exp_var) {
    ggplot(data = data, mapping = aes(x = !! sym(outcome_var), fill = !! sym(exp_var))) +
      geom_bar(position = "fill") +
      labs(y = "Percentage")
  
}

#plots<- lapply(colnames(data), exp_plot, data = DATA_FRAME_NAME, outcome_var = OUTCOME_VAR_NAME)
