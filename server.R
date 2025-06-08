# server.R

# This file contains the server logic of the Shiny application.
# It uses functions and variables defined in global.R.
server <- function(input, output, session) {
  
  # Reactive value to store the user's email for the current session
  current_user_email <- reactiveVal(NULL)
  email_feedback_msg <- reactiveVal(list(message = "", type = ""))
  
  # Reactive value to track authentication status for regular users
  is_authenticated <- reactiveVal(FALSE)
  
  # Reactive values for tab-specific authentication
  is_voting_tab_authenticated <- reactiveVal(FALSE)
  is_leaderboard_tab_authenticated <- reactiveVal(FALSE)
  
  # Reactive value to track authentication status for admin
  is_admin_authenticated <- reactiveVal(FALSE)
  
  # The secret passwords
  SECRET_USER_PASSWORD <- "MQSS"
  SECRET_VOTING_PASSWORD <- "MQSS1" # New password for View & Vote tab
  SECRET_LEADERBOARD_PASSWORD <- "MQSS2" # New password for Leaderboard tab
  SECRET_ADMIN_PASSWORD <- "admin admin"
  
  # Display current user email
  output$current_user_email_display <- renderText({
    if (is.null(current_user_email())) {
      "Not set"
    } else {
      current_user_email()
    }
  })
  
  # Observer for setting user email and checking password
  observeEvent(input$set_email_button, {
    email_input <- trimws(tolower(input$user_email_input)) # Trim whitespace and convert to lowercase
    password_input <- input$user_password_input # Get the entered password
    
    # Basic email validation regex
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email_input)) {
      email_feedback_msg(list(message = "Please enter a valid email address.", type = "error"))
      current_user_email(NULL)
      is_authenticated(FALSE)
      return()
    }
    
    # Password check
    if (password_input != SECRET_USER_PASSWORD) {
      email_feedback_msg(list(message = "Incorrect password.", type = "error"))
      current_user_email(NULL)
      is_authenticated(FALSE)
      return()
    }
    
    # If both email and password are valid
    current_user_email(email_input)
    is_authenticated(TRUE)
    email_feedback_msg(list(message = paste("Logged in as '", email_input, "'!", sep=""), type = "success"))
    updateTextInput(session, "user_email_input", value = "") # Clear email input
    updateTextInput(session, "user_password_input", value = "") # Clear password input
    
    # Clear feedback after 3 seconds
    invalidateLater(3000, session)
    email_feedback_msg(list(message = "", type = ""))
  })
  
  # Observer for admin login
  observeEvent(input$admin_login_button, {
    admin_password_input <- input$admin_password_input # Get the entered admin password
    
    if (admin_password_input == SECRET_ADMIN_PASSWORD) {
      is_admin_authenticated(TRUE)
      showNotification("Admin login successful!", type = "message", duration = 3)
      updateTextInput(session, "admin_password_input", value = "") # Clear password input
    } else {
      is_admin_authenticated(FALSE)
      showNotification("Incorrect admin password.", type = "error", duration = 3)
    }
  })
  
  # Observer for Voting Tab Login
  observeEvent(input$vote_tab_login_button, {
    if (input$vote_tab_password == SECRET_VOTING_PASSWORD) {
      is_voting_tab_authenticated(TRUE)
      showNotification("Access to View & Vote tab granted!", type = "message", duration = 3)
    } else {
      is_voting_tab_authenticated(FALSE)
      showNotification("Incorrect password for View & Vote tab.", type = "error", duration = 3)
    }
    updateTextInput(session, "vote_tab_password", value = "") # Clear password input
  })
  
  # Observer for Leaderboard Tab Login
  observeEvent(input$leaderboard_tab_login_button, {
    if (input$leaderboard_tab_password == SECRET_LEADERBOARD_PASSWORD) {
      is_leaderboard_tab_authenticated(TRUE)
      showNotification("Access to Leaderboard tab granted!", type = "message", duration = 3)
    } else {
      is_leaderboard_tab_authenticated(FALSE)
      showNotification("Incorrect password for Leaderboard tab.", type = "error", duration = 3)
    }
    updateTextInput(session, "leaderboard_tab_password", value = "") # Clear password input
  })
  
  # Render email feedback message (for main app login)
  output$email_feedback <- renderUI({
    fb <- email_feedback_msg()
    if (nchar(fb$message) > 0) {
      div(class = paste0("text-feedback ", ifelse(fb$type == "success", "text-success", "text-error")), fb$message)
    } else {
      NULL
    }
  })
  
  # Conditional rendering of the main app content based on authentication status
  output$main_app_content <- renderUI({
    if (!is_authenticated()) {
      div(class = "container-fluid",
          div(class = "well text-center",
              h2("Welcome! Please Log In"),
              p("Enter your email and the password to access the app."),
              fluidRow(
                column(8, textInput("user_email_input", "Your Email Address:", value = "", placeholder = "e.g., example@domain.com")),
                column(8, passwordInput("user_password_input", "Password:", value = "", placeholder = "Enter password")),
                column(4, actionButton("set_email_button", "Login", class = "btn-primary w-full mt-6"))
              ),
              uiOutput("email_feedback") # Feedback for login attempts
          )
      )
    } else {
      # Render the main navbarPage content after successful login
      navbarPage(
        title = "Fill in the Blank Game", # Changed app name
        id = "main_navbar", # ID for navigation control
        tabPanel(
          "Submit Answers",
          div(class = "container-fluid", # Centralized content container
              h2("Submit Your Answers"),
              p("Current User: ", span(textOutput("current_user_email_display"), class = "font-mono bg-gray-100 p-1 rounded text-xs mt-2")),
              uiOutput("submit_feedback"), # Feedback message area for answer submission
              
              # Dynamically generate submission sections for each question
              lapply(names(QUESTIONS), function(q_id) {
                div(class = "well",
                    h3(paste0("Question ", which(names(QUESTIONS) == q_id), ": ", QUESTIONS[q_id])), # Display question text
                    textAreaInput(paste0("answer", q_id), NULL, value = "", rows = 3, width = "100%", placeholder = "Type your answer here (max 100 characters)"),
                    div(class = "text-right text-sm text-gray-500 mt-2",
                        span(textOutput(paste0("char_count", q_id))), " / 100 characters"),
                    actionButton(paste0("submit_answer", q_id), paste0("Submit Answer for Question ", which(names(QUESTIONS) == q_id)), class = "btn-primary w-full mt-4")
                )
              })
          )
        ),
        tabPanel(
          "View & Vote",
          div(class = "container-fluid",
              h2("All Answers & Voting"),
              uiOutput("vote_feedback"), # Feedback message area for voting
              
              # Conditional UI for View & Vote tab content
              uiOutput("voting_tab_content")
          )
        ),
        tabPanel(
          "Leaderboard",
          div(class = "container-fluid",
              h2("Leaderboard"),
              # Conditional UI for Leaderboard tab content
              uiOutput("leaderboard_tab_content")
          )
        ),
        tabPanel(
          "Admin", # New Admin Tab
          div(class = "container-fluid",
              h2("Admin Tools"),
              uiOutput("admin_login_ui"), # Admin login form
              uiOutput("admin_tools_ui") # Conditional display of admin tools
          )
        )
      )
    }
  })
  
  # UI for View & Vote tab (conditional on password)
  output$voting_tab_content <- renderUI({
    if (!is_voting_tab_authenticated()) {
      div(class = "well text-center",
          h3("Access View & Vote Tab"),
          passwordInput("vote_tab_password", "Password for View & Vote:", value = "", placeholder = "Enter password"),
          actionButton("vote_tab_login_button", "Access View & Vote", class = "btn-primary w-full mt-4")
      )
    } else {
      tagList(
        div(class = "well",
            selectInput("selected_question_for_vote",
                        "Select a Question to View and Vote:",
                        choices = setNames(names(QUESTIONS), paste0("Question ", 1:length(QUESTIONS), ": ", QUESTIONS))) # Dynamic choices
        ),
        uiOutput("conditional_answers_and_votes")
      )
    }
  })
  
  # UI for Leaderboard tab (conditional on password)
  output$leaderboard_tab_content <- renderUI({
    if (!is_leaderboard_tab_authenticated()) {
      div(class = "well text-center",
          h3("Access Leaderboard Tab"),
          passwordInput("leaderboard_tab_password", "Password for Leaderboard:", value = "", placeholder = "Enter password"),
          actionButton("leaderboard_tab_login_button", "Access Leaderboard", class = "btn-primary w-full mt-4")
      )
    } else {
      tagList(
        # Dynamically generate leaderboard sections for each question
        lapply(names(QUESTIONS), function(q_id) {
          div(class = "well",
              h3(paste0("Question ", which(names(QUESTIONS) == q_id), ": ", QUESTIONS[q_id])),
              uiOutput(paste0(q_id, "_leaderboard")) # Rendered leaderboard for this question
          )
        })
      )
    }
  })
  
  
  # Admin Login UI
  output$admin_login_ui <- renderUI({
    if (!is_admin_authenticated()) {
      div(class = "well text-center",
          h3("Admin Login"),
          passwordInput("admin_password_input", "Admin Password:", value = "", placeholder = "Enter admin password"),
          actionButton("admin_login_button", "Login as Admin", class = "btn-primary w-full mt-4")
      )
    } else {
      NULL
    }
  })
  
  # Admin Tools UI (Conditionally rendered)
  output$admin_tools_ui <- renderUI({
    req(is_admin_authenticated()) # Only show if admin is authenticated
    div(class = "well",
        h3("Data Management"),
        fluidRow(
          column(6,
                 downloadButton("download_answers_csv", "Download Answers CSV", class = "btn-primary w-full mb-3"),
                 actionButton("clear_answers_button", "Clear All Answers", class = "btn-danger w-full")
          ),
          column(6,
                 downloadButton("download_votes_csv", "Download Votes CSV", class = "btn-primary w-full mb-3"),
                 actionButton("clear_votes_button", "Clear All Votes", class = "btn-danger w-full")
          )
        )
    )
  })
  
  # Download Handler for Answers CSV
  output$download_answers_csv <- downloadHandler(
    filename = function() {
      paste("answers-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      answers <- answers_data()
      write.csv(answers, file, row.names = FALSE)
    }
  )
  
  # Download Handler for Votes CSV
  output$download_votes_csv <- downloadHandler(
    filename = function() {
      paste("votes-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      votes <- votes_data()
      write.csv(votes, file, row.names = FALSE)
    }
  )
  
  # Observer for Clear Answers button
  observeEvent(input$clear_answers_button, {
    req(is_admin_authenticated())
    # In a real app, you'd use a modal for confirmation, not browser's confirm()
    # For now, using a simple confirm via showNotification for demo purposes
    showNotification("Are you sure you want to clear ALL answers? This cannot be undone.", type = "warning", duration = 3, id = "confirmClearAnswers") # Reduced duration
    
    # We will trigger the clear logic directly after a short notification to simulate confirmation
    # In a production app, replace this with a proper modal confirmation from the user.
    # For this demo, this means a very quick "Are you sure" and then it proceeds.
    
    invalidateLater(0, session) # Ensure reactivity immediately
    saveRDS(canonical_empty_answers_df, answers_file)
    showNotification("All answers cleared!", type = "message", duration = 3)
    removeNotification(id = "confirmClearAnswers") # Attempt to remove earlier notification
  })
  
  # Observer for Clear Votes button
  observeEvent(input$clear_votes_button, {
    req(is_admin_authenticated())
    # Similar to clear answers, real app would use a modal for confirmation
    showNotification("Are you sure you want to clear ALL votes? This cannot be undone.", type = "warning", duration = 3, id = "confirmClearVotes") # Reduced duration
    
    invalidateLater(0, session) # Ensure reactivity immediately
    saveRDS(canonical_empty_votes_df, votes_file)
    showNotification("All votes cleared!", type = "message", duration = 3)
    removeNotification(id = "confirmClearVotes") # Attempt to remove earlier notification
  })
  
  
  # Reactive values for feedback messages (used to display temporary messages)
  feedback_submit <- reactiveVal(list(message = "", type = ""))
  feedback_vote <- reactiveVal(list(message = "", type = ""))
  
  # Dynamically generate character count outputs for each question
  lapply(names(QUESTIONS), function(q_id) {
    output[[paste0("char_count", q_id)]] <- renderText({
      nchar(input[[paste0("answer", q_id)]])
    })
  })
  
  # Reactive expression to read answers data from RDS
  answers_data <- reactivePoll(
    intervalMillis = 2000, # Corrected argument name
    session = session, # Added session argument
    checkFunc = function() {
      if (file.exists(answers_file)) {
        file.info(answers_file)$mtime[1] # Return file modification time
      } else {
        0 # File doesn't exist, treat as never modified
      }
    },
    valueFunc = function() {
      # Pass the canonical template for answers to the robust RDS reader
      read_rds_robust(answers_file, canonical_empty_answers_df)
    }
  )
  
  # Reactive expression to read votes data from RDS (similar to answers_data)
  votes_data <- reactivePoll(
    intervalMillis = 2000, # Corrected argument name
    session = session, # Added session argument
    checkFunc = function() {
      if (file.exists(votes_file)) {
        file.info(votes_file)$mtime[1]
      } else {
        0
      }
    },
    valueFunc = function() {
      # Pass the canonical template for votes to the robust RDS reader
      read_rds_robust(votes_file, canonical_empty_votes_df)
    }
  )
  
  # Reactive expression to get the current user's vote for the selected question
  current_user_vote_for_selected_q <- reactive({
    req(current_user_email(), input$selected_question_for_vote)
    # Ensure tab is authenticated before accessing input$selected_question_for_vote
    req(is_voting_tab_authenticated()) 
    
    all_votes <- votes_data()
    selected_q <- input$selected_question_for_vote
    
    # Filter for the current user's vote on the selected question
    voted_id <- all_votes %>%
      filter(user_email == current_user_email(), question_id == selected_q) %>%
      pull(voted_answer_id)
    
    # Return NA_character_ if no vote found, otherwise the first (and only) vote ID
    if (length(voted_id) == 0) NA_character_ else as.character(voted_id[1])
  })
  
  # --- Conditional Answer and Vote Display Logic ---
  output$conditional_answers_and_votes <- renderUI({
    # Require current user email, overall authentication, AND voting tab authentication
    req(current_user_email(), is_authenticated(), is_voting_tab_authenticated())
    # Require selected_question_for_vote to have a value (since this is part of the voting tab content)
    req(input$selected_question_for_vote) 
    
    # Get the currently selected question from the dropdown
    selected_q <- input$selected_question_for_vote
    
    all_answers <- answers_data()
    # Use the new reactive for current user's vote
    current_user_voted_id <- current_user_vote_for_selected_q()
    
    # Filter answers for the selected question
    filtered_answers <- all_answers %>%
      filter(question_id == selected_q) %>%
      distinct(user_email, answer_text, .keep_all = TRUE) %>%
      rowwise() %>%
      mutate(actual_answer_identifier = paste0(user_email, "_", substr(digest(answer_text, algo="md5"), 1, 8))) %>%
      ungroup()
    
    if (nrow(filtered_answers) == 0) {
      return(p(class = "text-gray-500 text-center py-4", "No answers submitted for this question yet."))
    }
    
    # Generate UI for each answer for the selected question
    lapply(1:nrow(filtered_answers), function(i) {
      ans <- filtered_answers[i, ]
      
      # Determine if this answer is the one currently voted by the user
      is_voted_by_current_user <- !is.na(current_user_voted_id) && current_user_voted_id == ans$actual_answer_identifier
      
      # Determine button label and class based on current user's vote
      button_label <- if (is_voted_by_current_user) "Voted!" else "Vote"
      button_class <- paste("vote-button", if (is_voted_by_current_user) "active-vote" else "")
      
      div(class = "answer-item",
          p(class = "answer-text",
            # Display "You: " only if it's the current user's answer
            if (ans$user_email == current_user_email()) {
              tagList(span(class = "answer-author", "You"), ": ", ans$answer_text)
            } else {
              ans$answer_text # Just the answer for other users, no email or colon
            }
          ),
          actionButton(
            inputId = paste0("vote_", selected_q, "_", ans$actual_answer_identifier), # Dynamic ID
            label = button_label,
            class = button_class
          )
      )
    })
  })
  
  # --- Submit Answers Logic (Dynamic Observers) ---
  lapply(names(QUESTIONS), function(q_id) {
    observeEvent(input[[paste0("submit_answer", q_id)]], {
      # Require email and authentication to be set before submission
      req(current_user_email(), is_authenticated())
      
      answer_text_input_id <- paste0("answer", q_id)
      req(input[[answer_text_input_id]]) # Ensure input is not empty
      
      answer_text <- trimws(input[[answer_text_input_id]]) # Remove leading/trailing whitespace
      if (nchar(answer_text) > 100 || nchar(answer_text) == 0) {
        feedback_submit(list(message = "Answer must be between 1 and 100 characters.", type = "error"))
        return()
      }
      
      # Create new answer data frame - explicitly defined to match canonical structure
      new_answer <- data.frame(
        user_email = current_user_email(), # Use user's email
        question_id = q_id, # Use dynamic question ID
        answer_text = answer_text,
        timestamp = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )
      
      # Append new answer to existing data and write back to RDS
      tryCatch({
        existing_answers <- answers_data() # This now guaranteed to have compatible structure
        
        # Use dplyr::bind_rows for more robust combination
        updated_answers <- dplyr::bind_rows(existing_answers, new_answer)
        
        saveRDS(updated_answers, answers_file) # Save to RDS file
        feedback_submit(list(message = paste0("Your answer for Question ", which(names(QUESTIONS) == q_id), " has been saved!"), type = "success"))
        updateTextAreaInput(session, answer_text_input_id, value = "") # Clear input field
      }, error = function(e) {
        feedback_submit(list(message = paste("Error saving answer:", e$message), type = "error"))
      })
      # Clear feedback message after 3 seconds
      invalidateLater(3000, session)
      feedback_submit(list(message = "", type = ""))
    })
  })
  
  
  # Render the feedback message UI
  output$submit_feedback <- renderUI({
    fb <- feedback_submit()
    if (nchar(fb$message) > 0) {
      div(class = paste0("text-feedback ", ifelse(fb$type == "success", "text-success", "text-error")), fb$message)
    } else {
      # If not authenticated, display a general warning
      if (!is_authenticated()) {
        div(class = "text-feedback text-error", "Please log in to submit answers.")
      } else {
        NULL
      }
    }
  })
  
  # --- Vote Logic (Centralized Observer) ---
  observeEvent(input$last_vote_clicked_data, {
    # This observer now handles ALL vote button clicks from the JS
    req(current_user_email(), is_authenticated())
    req(is_voting_tab_authenticated()) # Require voting tab authentication
    
    clicked_q_id <- input$last_vote_clicked_data$qId
    clicked_ans_id <- input$last_vote_clicked_data$ansId
    
    all_votes <- votes_data() # Re-read latest votes data
    
    # Remove previous vote by the current user for THIS specific question
    votes_to_keep <- all_votes %>%
      filter(!(user_email == current_user_email() & question_id == clicked_q_id))
    
    # Create new vote entry
    new_vote <- data.frame(
      user_email = current_user_email(),
      question_id = clicked_q_id,
      voted_answer_id = clicked_ans_id,
      timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      updated_votes <- dplyr::bind_rows(votes_to_keep, new_vote)
      saveRDS(updated_votes, votes_file)
      feedback_vote(list(message = "Your vote has been cast!", type = "success"))
    }, error = function(e) {
      feedback_vote(list(message = paste("Error casting vote:", e$message), type = "error"))
    })
    
    # Invalidate reactive dependencies to force UI updates (e.g., button labels)
    invalidateLater(2000, session) # Keep short delay for visual feedback, then clear
    feedback_vote(list(message = "", type = ""))
  })
  
  # Render the vote feedback message UI
  output$vote_feedback <- renderUI({
    fb <- feedback_vote()
    if (nchar(fb$message) > 0) {
      div(class = paste0("text-feedback ", ifelse(fb$type == "success", "text-success", "text-error")), fb$message)
    } else {
      # If not authenticated for voting tab, display a warning
      if (!is_voting_tab_authenticated()) {
        div(class = "text-feedback text-error", "Please unlock the 'View & Vote' tab to cast votes.")
      } else {
        NULL
      }
    }
  })
  
  # --- Leaderboard Logic ---
  leaderboard_data <- reactive({
    # Require leaderboard tab authentication
    req(is_leaderboard_tab_authenticated())
    
    answers <- answers_data()
    votes <- votes_data()
    
    if (is.null(answers) || nrow(answers) == 0) { 
      return(NULL)
    }
    
    # Generate the actual answer identifiers for answers so they can be joined with votes
    answers_with_ids <- answers %>%
      distinct(user_email, answer_text, .keep_all = TRUE) %>% 
      rowwise() %>%
      mutate(
        actual_answer_identifier = paste0(user_email, "_", substr(digest(answer_text, algo="md5"), 1, 8)) 
      ) %>%
      ungroup()
    
    if (nrow(answers_with_ids) == 0) {
      return(NULL)
    }
    
    # Calculate vote counts for each actual_answer_identifier
    valid_votes <- votes %>%
      inner_join(
        answers_with_ids, # Join with answers_with_ids directly
        by = c("voted_answer_id" = "actual_answer_identifier", "question_id")
      )
    
    vote_counts <- valid_votes %>%
      group_by(question_id, voted_answer_id) %>%
      summarise(votes = as.numeric(n()), .groups = 'drop') # Explicitly cast to numeric
    
    
    # Combine answers with their calculated vote counts
    leaderboard_df <- answers_with_ids %>%
      left_join(vote_counts, by = c("question_id", "actual_answer_identifier" = "voted_answer_id")) %>%
      mutate(votes = ifelse(is.na(votes), 0, votes)) %>% # Replace NA votes with 0
      filter(votes > 0) %>% # Filter out answers with 0 votes for leaderboard display
      dplyr::select(question_id, user_email, answer_text, votes) # Ensure dplyr::select for clarity and specify columns
    
    leaderboard_df
  })
  
  # Dynamically render leaderboard sections for each question
  lapply(names(QUESTIONS), function(q_id) {
    output[[paste0(q_id, "_leaderboard")]] <- renderUI({
      # Require leaderboard tab authentication
      req(is_leaderboard_tab_authenticated())
      
      lb_data <- leaderboard_data()
      if (is.null(lb_data) || nrow(lb_data %>% filter(question_id == q_id)) == 0) {
        return(p(class = "text-gray-500 text-center py-4", "No answers or votes yet for this question."))
      }
      
      # Filter for current question, arrange by votes in descending order
      current_q_lb <- lb_data %>%
        filter(question_id == q_id) %>%
        arrange(desc(votes))
      
      # Generate UI for each leaderboard entry
      lapply(1:nrow(current_q_lb), function(i) {
        entry <- current_q_lb[i, ]
        div(class = "leaderboard-item",
            # Conditionally add class for top item
            class = if(i == 1) "leaderboard-item-first",
            p(class = "answer-text",
              span(class = "answer-author", entry$user_email) , # Display full email
              ": ", entry$answer_text
            ),
            span(class = "leaderboard-votes",
                 span(entry$votes), " votes"
            )
        )
      })
    })
  })
}
