
# Define the server logic
server <- function(input, output, session) {
  
  # Function to convert "HH:MM" to minutes since midnight
  convert_to_minutes <- function(time_str) {
    parts <- as.numeric(unlist(strsplit(time_str, ":")))
    return(parts[1] * 60 + parts[2])
  }
  
  # Function to calculate duration in minutes, considering day cross
  min_cal <- function(time_range) {
    times <- unlist(strsplit(time_range, " - "))
    start_minutes <- convert_to_minutes(times[1])
    end_minutes <- convert_to_minutes(times[2])
    
    if (end_minutes < start_minutes) {
      end_minutes <- end_minutes + 1440  # Add 24 hours
    }
    
    duration <- end_minutes - start_minutes
    return(duration)
  }
  
  # Reactive value to hold the data frame
  data <- reactiveVal(data.frame(
    Time = c("24:00 - 07:00", "07:00 - 07:15", "07:30 - 08:00", "08:00 - 08:15", "08:15 - 08:30", 
             "08:30 - 08:45", "08:45 - 15:30", "15:30 - 16:00", "16:00 - 17:30", "17:30 - 19:00", 
             "19:00 - 20:00", "21:00 - 24:00"),
    Activity = c("bedtime", "wake up", "breakfast", "getting ready", "going to school", 
                 "rest", "school", "coming home", "activity", "dinner", 
                 "ready for bed", "bedtime")
  ))
  
  # Calculate length of each interval in minutes
  calculate_lengths <- function(df) {
    df$Length <- sapply(df$Time, min_cal)
    return(df)
  }
  
  # Render the schedule table with editing functionality
  output$scheduleTable <- renderDT({
    datatable(data(), editable = TRUE, selection = 'single')
  }, server = FALSE)
  
  # Observe table edits
  observeEvent(input$scheduleTable_cell_edit, {
    info <- input$scheduleTable_cell_edit
    df <- data()
    df[info$row, info$col] <- info$value
    df <- calculate_lengths(df)  # Ensure lengths are recalculated
    data(df)  # Update reactive data
  })
  
  # Render the pie chart
  output$pieChart <- renderPlot({
    df <- data()
    # Prepare data for ggplot
    df <- df %>%
      mutate(
        Fraction = Length / sum(Length),
        StartAngle = cumsum(Fraction) * 360 - Fraction * 360,
        EndAngle = cumsum(Fraction) * 360,
        # MidAngle = (StartAngle + EndAngle) / 2,
        MidAngle = EndAngle ,
        EndTime = sub(".* - ", "", Time)  # Extract the end time from the interval
      )
    
    ggplot(df, aes(x = 1, ymin = StartAngle, ymax = EndAngle, fill = Activity)) +
      geom_rect(aes(xmin = 0.5, xmax = 1.5)) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      geom_text(aes(x = 1.7, y = MidAngle, label = EndTime), size = 4) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Activity Throughout the Day")+theme(
        plot.title = element_text(size = 22),  # Increased title size
        legend.text = element_text(size = 18),  # Increased legend text size
        legend.title = element_text(size = 18)  # Increased legend title size
      )
  })
  
  # Observe event for the "Add" button
  observeEvent(input$addButton, {
    new_time <- input$newTime
    new_activity <- input$newActivity
    
    # Validate new entry
    if (new_time != "" && new_activity != "") {
      df <- data()
      new_entry <- data.frame(Time = new_time, Activity = new_activity, stringsAsFactors = FALSE)
      df <- rbind(df, new_entry)
      df <- calculate_lengths(df)  # Recalculate lengths
      data(df)  # Update reactive data
    }
  })
  
  # Observe event for the "Delete" button
  observeEvent(input$deleteButton, {
    selected_row <- input$scheduleTable_rows_selected
    if (length(selected_row) > 0) {
      df <- data()
      df <- df[-selected_row, , drop = FALSE]
      df <- calculate_lengths(df)  # Recalculate lengths
      data(df)  # Update reactive data
    }
  })
}