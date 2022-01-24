function(input, output) {
  
  estim_year_react <- eventReactive(input$generate, {
    input$estim_year
  }, ignoreNULL=FALSE)
  
  estim_type_react <- eventReactive(input$generate, {
    input$estim_type
  }, ignoreNULL=FALSE)
  
  state_react <- eventReactive(input$generate, {
    input$state
  }, ignoreNULL=FALSE)
  
  n_react <- eventReactive(input$generate, {
    input$target_n
  }, ignoreNULL=FALSE)
  
  male_react <- eventReactive(input$generate, {
    input$target_male_prop
  }, ignoreNULL=FALSE)
  
  # Generate planned enrollment distribution in nicely formatted table
  df_dist_format <- reactive({
    # Process inputs
    estim_year <- estim_year_react()
    estim_type_str <- estim_type_react() # string input
    estim_type <- ifelse(estim_type_str == "ACS 5-Year Estimates", "acs/acs5", "acs/acs1")
    state_list <- state_react()
    if (is.null(state_list)) { state_list <- "All" }
    n <- n_react()
    if (is.na(n) | n == 0) { n <- 1 }
    male_prop <- male_react()
    
    # Generate df from inputs
    code_list <- convert_states_to_codes(get_state_df(), state_list)
    df_sums <- get_sums_for_state_list(estim_type, estim_year, code_list)
    df_adj <- calculate_adjusted_proportions(df_sums)
    df_dist <- generate_distribution(df_adj, n, male_prop)
    df_dist_format <- format_distribution(df_dist)
    
    df_dist_format
  })
  
  # Generate (adjusted) population distribution in nicely formatted table
  df_adj_format <- reactive({
    # Process inputs
    estim_year <- estim_year_react()
    estim_type_str <- estim_type_react() # string input
    estim_type <- ifelse(estim_type_str == "ACS 5-Year Estimates", "acs/acs5", "acs/acs1")
    state_list <- state_react()
    state_list <- state_react()
    if (is.null(state_list)) { state_list <- "All" }
    
    # Generate df from inputs
    code_list <- convert_states_to_codes(get_state_df(), state_list)
    df_sums <- get_sums_for_state_list(estim_type, estim_year, code_list)
    df_adj <- calculate_adjusted_proportions(df_sums)
    df_adj_format <- format_proportion(df_adj)
    
    df_adj_format
  })

  output$dist <- renderTable({
    df_dist_format()
  })
  
  output$pop <- renderTable({
    df_adj_format()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("enrollment_n-", n_react(), "_", male_react(), "-male_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df_dist_format(), file, row.names=FALSE)
    }
  )
}