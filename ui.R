# ui.R

# This file defines the user interface (UI) of the Shiny application.

ui <- fluidPage(
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f3f4f6; /* Light gray background */
        color: #374151; /* Dark gray text */
      }
      .container-fluid {
        max-width: 960px;
        margin-left: auto;
        margin-right: auto;
        padding: 20px;
      }
      .well {
        background-color: #ffffff;
        border-radius: 12px; /* Rounded corners */
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06); /* Shadow */
        padding: 24px;
        margin-bottom: 20px;
        border: 1px solid #e5e7eb; /* Light border */
      }
      h1, h2, h3 {
        color: #ca4e0a; /* Custom Orange-Brown */
        font-weight: 700; /* Bold font */
      }
      h1 { font-size: 2.25rem; margin-bottom: 0.5rem; } /* text-4xl */
      h2 { font-size: 1.875rem; margin-bottom: 1rem; } /* text-3xl */
      h3 { font-size: 1.5rem; margin-bottom: 0.75rem; } /* text-2xl */

      .btn-primary {
        background-color: #ca4e0a; /* Custom Orange-Brown */
        border-color: #ca4e0a;
        color: #ffffff;
        font-weight: 600;
        padding: 0.75rem 1.5rem;
        border-radius: 0.5rem;
        transition: all 0.3s ease-in-out;
        transform: scale(1.0);
      }
      .btn-primary:hover {
        background-color: #b34409; /* Darker Orange-Brown for hover */
        border-color: #b34409;
        transform: scale(1.02); /* Slight scale on hover */
      }
      .form-control {
        border-radius: 0.5rem;
        padding: 0.75rem;
        border: 1px solid #d1d5db; /* Gray-300 border */
        transition: all 0.2s ease-in-out;
      }
      .form-control:focus {
        border-color: #d85811; /* More vibrant Orange-Brown on focus */
        box-shadow: 0 0 0 3px rgba(216, 88, 17, 0.5); /* Focus ring */
        outline: none;
      }
      .tab-content {
        padding: 20px 0;
      }
      .nav-tabs .nav-link {
        padding: 0.75rem 1.5rem;
        font-size: 1.125rem; /* text-lg */
        font-weight: 500;
        border-radius: 0.5rem 0.5rem 0 0;
        margin-right: 4px;
        color: #6b7280; /* gray-500 */
        transition: all 0.3s ease-in-out;
        border: none;
        border-bottom: 4px solid transparent;
      }
      .nav-tabs .nav-link.active {
        color: #ca4e0a; /* Custom Orange-Brown */
        border-color: #ca4e0a;
        background-color: #ffffff;
      }
      .nav-tabs .nav-link:hover:not(.active) {
        color: #4b5563; /* gray-700 */
        border-color: #d1d5db; /* gray-300 */
      }
      .text-feedback {
        padding: 1rem;
        border-radius: 0.5rem;
        text-align: center;
        font-weight: 500;
        margin-bottom: 1rem;
      }
      .text-success {
        background-color: #dcfce7; /* Green-100 */
        color: #15803d; /* Green-700 */
      }
      .text-error {
        background-color: #fee2e2; /* Red-100 */
        color: #b91c1c; /* Red-700 */
      }
      .answer-item {
        display: flex;
        align-items: center;
        justify-content: space-between;
        background-color: #f9fafb; /* Lightest gray */
        padding: 1rem;
        border-radius: 0.5rem;
        border: 1px solid #f3f4f6; /* Very light border */
        margin-bottom: 0.5rem;
      }
      .answer-text {
        flex: 1;
        color: #1f2937; /* Dark text */
        word-break: break-word; /* Allow long words to break */
        padding-right: 1rem;
      }
      .answer-author {
        font-weight: 500;
        color: #ca4e0a; /* Custom Orange-Brown */
      }
      .vote-button {
        padding: 0.5rem 1.25rem;
        border-radius: 0.5rem;
        font-weight: 600;
        transition: all 0.3s ease-in-out;
        white-space: nowrap; /* Prevent button text from wrapping */
      }
      .vote-button.active-vote {
        background-color: #ca4e0a; /* Custom Orange-Brown for active vote */
        color: #ffffff;
        box-shadow: inset 0 2px 4px 0 rgba(0, 0, 0, 0.06); /* Inner shadow for active */
      }
      .vote-button:not(.active-vote) {
        background-color: #f2e8e0; /* Very light Orange-Brown for inactive */
        color: #b34409; /* Darker Orange-Brown */
        box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05); /* Light shadow for inactive */
      }
      .vote-button:not(.active-vote):hover {
        background-color: #e6d3c7; /* Lighter Orange-Brown on hover */
        color: #8e3607; /* Even darker Orange-Brown on hover */
      }
      .leaderboard-item {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 1rem;
        border-radius: 0.5rem;
        box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
        margin-bottom: 0.75rem;
      }
      .leaderboard-item:first-child {
        background-color: #fffbeb; /* Yellow-50 for top item */
        border: 1px solid #fcd34d; /* Yellow-300 border */
      }
      .leaderboard-item:not(:first-child) {
        background-color: #f9fafb; /* Gray-50 */
        border: 1px solid #f3f4f6; /* Gray-100 */
      }
      .leaderboard-votes {
        font-weight: 700;
        font-size: 1.125rem; /* text-lg */
        color: #ca4e0a; /* Custom Orange-Brown */
        white-space: nowrap;
      }
      .leaderboard-votes span {
        margin-right: 0.25rem;
      }
    "))
  ),
  
  # Main application UI wrapped in an output that's only shown after login
  uiOutput("main_app_content"),
  
  # JavaScript to capture clicks on dynamically generated vote buttons
  tags$script(HTML("
    $(document).on('click', '.vote-button', function() {
      var buttonId = this.id; // e.g., 'vote_q1_user@example.com_hash'
      var parts = buttonId.split('_');

      if (parts.length >= 3 && parts[0] === 'vote') {
        var questionId = parts[1];
        // The remaining parts form the answer identifier (could contain underscores)
        var answerIdentifier = parts.slice(2).join('_');

        // Send a custom input value to Shiny
        Shiny.setInputValue('last_vote_clicked_data', {
          qId: questionId,
          ansId: answerIdentifier,
          nonce: Math.random() // Use a nonce to ensure the event always triggers, even if data is the same
        }, {priority: 'event'}); // priority: 'event' ensures it's handled immediately
      }
    });
  "))
)
