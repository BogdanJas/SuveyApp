# Load required packages
library(shiny)
library(shinymanager)
library(shinyjs)
library(readxl)
library(writexl)
library(tidyverse)
library(R6)

# Define the User class
User <- R6Class("User",
                public = list(
                  username = NULL,
                  password = NULL,
                  admin = NULL,
                  initialize = function(username, password, admin) {
                    self$username <- username
                    self$password <- password
                    self$admin <- admin
                  },
                  check_auth = function(credentials, role) {
                    cred <- credentials %>% filter(user == self$username, password == self$password, admin == role)
                    if (nrow(cred) == 1) {
                      return(cred$admin)
                    } else {
                      return(NA)
                    }
                  }
                )
)

# Define the SurveyManager class
SurveyManager <- R6Class("SurveyManager",
                         public = list(
                           survey_questions = NULL,
                           correct_answers = NULL,
                           allowed_users = NULL,
                           initialize = function() {
                             self$survey_questions <- reactiveVal()
                             self$correct_answers <- reactiveVal()
                             self$allowed_users <- reactiveVal(character())
                           },
                           load_survey = function(file_path) {
                             survey_data <- read_excel(file_path)
                             questions <- names(survey_data)
                             correct_answers <- survey_data[1, ]
                             self$survey_questions(questions)
                             self$correct_answers(correct_answers)
                             write_xlsx(data.frame(questions), "survey_questions.xlsx")
                             write_xlsx(survey_data, "answers.xlsx")
                           },
                           generate_survey_ui = function() {
                             questions <- self$survey_questions()
                             fluidPage(
                               h2("Survey"),
                               lapply(seq_along(questions), function(i) {
                                 textInput(paste0("question_", i), questions[[i]])
                               }),
                               actionButton("submit_survey", "Submit")
                             )
                           },
                           count_correct_answers = function(user_answers) {
                             correct_answers <- self$correct_answers()
                             correct_count <- sum(user_answers == correct_answers, na.rm = TRUE)
                             return(correct_count)
                           }
                         )
)

# Define the AppUI class
AppUI <- R6Class("AppUI",
                 public = list(
                   create_ui = function() {
                     fluidPage(
                       useShinyjs(),
                       navbarPage("Survey App", id = "navbar",
                                  tabPanel("Login", uiOutput("login_ui")),
                                  tabPanel("Register", uiOutput("register_ui")),
                                  tabPanel("Upload Survey", uiOutput("upload_survey_ui")),
                                  tabPanel("Survey", uiOutput("survey_ui")),
                                  tabPanel("Analytics", uiOutput("analytics_ui")),
                                  tabPanel("Download Answers", uiOutput("download_answers_ui")),
                                  tabPanel("Logout", uiOutput("logout_ui"))
                       )
                     )
                   }
                 )
)

# Define the server logic
AppServer <- R6Class("AppServer",
                     public = list(
                       credentials = NULL,
                       current_user = NULL,
                       survey_manager = NULL,

                       initialize = function() {
                         self$survey_manager <- SurveyManager$new()
                         self$credentials <- reactiveVal(data.frame(
                           user = character(),
                           password = character(),
                           admin = logical(),
                           stringsAsFactors = FALSE
                         ))
                         self$current_user <- reactiveVal(NULL)
                       },

                       setup_reactive_values = function(session) {
                         observe({
                           if (file.exists("users.xlsx")) {
                             self$credentials(read_xlsx("users.xlsx"))
                           } else {
                             write_xlsx(self$credentials(), "users.xlsx")
                           }
                         })
                       },

                       observe_login = function(input, output, session) {
                         observeEvent(input$login, {
                           user <- User$new(input$username, input$password, input$login_is_admin)
                           is_admin <- user$check_auth(self$credentials(), input$login_is_admin)
                           if (!is.na(is_admin)) {
                             self$current_user(user$username)
                             showModal(modalDialog(
                               title = "Login Successful",
                               paste("Welcome", user$username, "!"),
                               easyClose = TRUE,
                               footer = NULL
                             ))
                             if (is_admin) {
                               updateNavbarPage(session, "navbar", selected = "Upload Survey")
                               shinyjs::show(selector = 'a[data-value="Upload Survey"]')
                               shinyjs::show(selector = 'a[data-value="Survey"]')
                               shinyjs::show(selector = 'a[data-value="Analytics"]')
                               shinyjs::show(selector = 'a[data-value="Download Answers"]')
                               shinyjs::show(selector = 'a[data-value="Logout"]')
                             } else {
                               updateNavbarPage(session, "navbar", selected = "Survey")
                               shinyjs::hide(selector = 'a[data-value="Upload Survey"]')
                               shinyjs::hide(selector = 'a[data-value="Analytics"]')
                               shinyjs::hide(selector = 'a[data-value="Download Answers"]')
                               shinyjs::show(selector = 'a[data-value="Survey"]')
                               shinyjs::show(selector = 'a[data-value="Logout"]')
                             }
                             shinyjs::hide(selector = 'a[data-value="Login"]')
                             shinyjs::hide(selector = 'a[data-value="Register"]')
                           } else {
                             showModal(modalDialog(
                               title = "Error",
                               "Invalid username, password, or role."
                             ))
                           }
                         })
                       },

                       observe_register = function(input, output, session) {
                         observeEvent(input$register, {
                           new_user <- input$new_username
                           new_pass <- input$new_password
                           is_admin <- input$is_admin

                           if (new_user %in% self$credentials()$user) {
                             showModal(modalDialog(
                               title = "Error",
                               "Username already exists."
                             ))
                           } else {
                             new_cred <- data.frame(user = new_user, password = new_pass, admin = is_admin, stringsAsFactors = FALSE)
                             updated_credentials <- bind_rows(self$credentials(), new_cred)
                             write_xlsx(updated_credentials, "users.xlsx")
                             self$credentials(updated_credentials)
                             showModal(modalDialog(
                               title = "Success",
                               "Registration successful. You can now log in."
                             ))
                           }
                         })
                       },

                       observe_upload_survey = function(input, output, session) {
                         output$upload_survey_ui <- renderUI({
                           fluidPage(
                             h2("Upload Survey"),
                             fileInput("survey_file", "Upload Excel File", accept = c(".xlsx")),
                             checkboxGroupInput("user_selection", "Select Users for Survey", choices = self$credentials()$user),
                             actionButton("generate_survey", "Generate Survey")
                           )
                         })

                         observeEvent(input$generate_survey, {
                           req(input$survey_file)
                           self$survey_manager$load_survey(input$survey_file$datapath)
                           self$survey_manager$allowed_users(input$user_selection)
                           updateTabsetPanel(session, "navbar", selected = "Survey")
                         })
                       },

                       observe_survey_submission = function(input, output, session) {
                         observeEvent(input$submit_survey, {
                           req(self$survey_manager$survey_questions())
                           responses <- sapply(seq_along(self$survey_manager$survey_questions()), function(i) {
                             as.character(input[[paste0("question_", i)]])
                           })
                           responses <- c(username = as.character(self$current_user()), responses)
                           correct_count <- self$survey_manager$count_correct_answers(responses[-1]) # Exclude the username

                           responses <- c(responses, Correctness = as.character(correct_count))

                           if (file.exists("userAnswers.xlsx")) {
                             user_answers <- read_xlsx("userAnswers.xlsx")
                             # Ensure all columns are character type
                             user_answers <- mutate_all(user_answers, as.character)
                           } else {
                             # Initialize with character columns
                             user_answers <- data.frame(matrix(ncol = length(responses), nrow = 0,
                                                               dimnames = list(NULL, c("username", self$survey_manager$survey_questions(), "Correctness"))),
                                                        stringsAsFactors = FALSE)
                             user_answers[] <- lapply(user_answers, as.character)
                           }

                           # Ensure the new row has the correct column names
                           new_row <- as.data.frame(t(responses), stringsAsFactors = FALSE)
                           colnames(new_row) <- colnames(user_answers)

                           # Ensure column types match before binding
                           new_row[] <- lapply(new_row, as.character)

                           user_answers <- bind_rows(user_answers, new_row)

                           write_xlsx(user_answers, "userAnswers.xlsx")

                           showModal(modalDialog(
                             title = "Success",
                             "Your answers have been submitted successfully.",
                             easyClose = TRUE,
                             footer = NULL
                           ))
                         })
                       },

                       observe_logout = function(input, output, session) {
                         output$logout_ui <- renderUI({
                           fluidPage(
                             h2("Logout"),
                             actionButton("logout", "Logout")
                           )
                         })

                         observeEvent(input$logout, {
                           session$reload()
                         })
                       },

                       hide_initial_pages = function() {
                         shinyjs::hide(selector = 'a[data-value="Upload Survey"]')
                         shinyjs::hide(selector = 'a[data-value="Survey"]')
                         shinyjs::hide(selector = 'a[data-value="Analytics"]')
                         shinyjs::hide(selector = 'a[data-value="Download Answers"]')
                         shinyjs::hide(selector = 'a[data-value="Logout"]')

                         observe({
                           if (!is.null(self$current_user()) && self$current_user() %in% self$survey_manager$allowed_users()) {
                             shinyjs::show(selector = 'a[data-value="Survey"]')
                           } else {
                             shinyjs::hide(selector = 'a[data-value="Survey"]')
                           }
                         })
                       },

                       observe_analytics = function(input, output, session) {
                         output$analytics_ui <- renderUI({
                           req(self$current_user())
                           if (self$current_user() %in% self$credentials()$user[self$credentials()$admin]) {
                             fluidPage(
                               h2("Analytics"),
                               tableOutput("user_rankings"),
                               selectInput("selected_user", "Select User", choices = self$credentials()$user),
                               actionButton("show_stats", "Show Statistics"),
                               tableOutput("user_stats"),
                               selectInput("selected_question", "Select Question", choices = self$survey_manager$survey_questions(), selected = NULL),
                               plotOutput("question_histogram")
                             )
                           } else {
                             fluidPage(
                               h2("Analytics"),
                               p("Access denied. Admins only.")
                             )
                           }
                         })

                         observeEvent(input$show_stats, {
                           req(input$selected_user)
                           if (file.exists("userAnswers.xlsx")) {
                             user_answers <- read_xlsx("userAnswers.xlsx")
                             user_data <- user_answers %>% filter(username == input$selected_user)
                             if (nrow(user_data) > 0) {
                               output$user_stats <- renderTable({
                                 user_data
                               })
                             } else {
                               output$user_stats <- renderTable({
                                 data.frame(Message = "No data available for this user.")
                               })
                             }
                           }
                         })
                         observe({
                           req(input$selected_question)
                           if (file.exists("userAnswers.xlsx")) {
                             user_answers <- read_xlsx("userAnswers.xlsx")
                             question_column <- input$selected_question
                             correct_answer <- as.character(app_server$survey_manager$correct_answers()[[question_column]])

                             output$question_histogram <- renderPlot({
                               ggplot(user_answers, aes_string(x = question_column)) +
                                 geom_bar(stat = "count", fill = "blue", alpha = 0.5) +
                                 geom_bar(data = subset(user_answers, user_answers[[question_column]] == correct_answer),
                                          aes_string(x = question_column), fill = "red", alpha = 0.8) +
                                 geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
                                 geom_text(data = subset(user_answers, user_answers[[question_column]] == correct_answer),
                                           aes_string(x = question_column), label = "Correct Answer",
                                           color = "black", fontface = "bold", vjust = -1.5) +
                                 theme_minimal() +
                                 labs(title = paste("Distribution of Answers for", question_column),
                                      x = "Answers", y = "Count") +
                                 theme(axis.text.x = element_text(angle = 45, hjust = 1))
                             })
                           }
                         })


                         output$user_rankings <- renderTable({
                           if (file.exists("userAnswers.xlsx")) {
                             user_answers <- read_xlsx("userAnswers.xlsx")
                             rankings <- user_answers %>%
                               group_by(username) %>%
                               summarise(TotalCorrect = sum(as.numeric(Correctness), na.rm = TRUE)) %>%
                               arrange(desc(TotalCorrect))
                             rankings
                           } else {
                             data.frame(Message = "No data available.")
                           }
                         })
                       }
                     )
)

# Initialize UI and server
app_ui <- AppUI$new()
app_server <- AppServer$new()

ui <- app_ui$create_ui()

server <- function(input, output, session) {
  app_server$setup_reactive_values(session)
  app_server$observe_login(input, output, session)
  app_server$observe_register(input, output, session)
  app_server$observe_upload_survey(input, output, session)
  app_server$observe_survey_submission(input, output, session)
  app_server$observe_logout(input, output, session)
  app_server$hide_initial_pages()
  app_server$observe_analytics(input, output, session)

  output$login_ui <- renderUI({
    fluidPage(
      h2("Login"),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      checkboxInput("login_is_admin", "Login as Admin"),
      actionButton("login", "Login")
    )
  })

  output$register_ui <- renderUI({
    fluidPage(
      h2("Register"),
      textInput("new_username", "New Username"),
      passwordInput("new_password", "New Password"),
      checkboxInput("is_admin", "Register as Admin"),
      actionButton("register", "Register")
    )
  })

  output$survey_ui <- renderUI({
    if (file.exists("survey_questions.xlsx")) {
      questions <- read_xlsx("survey_questions.xlsx")
      correct_answers <- app_server$survey_manager$correct_answers()

      fluidPage(
        h2("Survey"),
        lapply(seq_along(questions$questions), function(i) {
          fluidRow(
            column(6, textInput(paste0("question_", i), questions$questions[[i]])),
            column(6, verbatimTextOutput(paste0("correct_answer_", i)))
          )
        }),
        actionButton("submit_survey", "Submit")
      )
    } else {
      fluidPage(
        h2("Survey"),
        p("No survey available.")
      )
    }
  })

  output$analytics_ui <- renderUI({
    req(app_server$current_user())
    if (app_server$current_user() %in% app_server$credentials()$user[app_server$credentials()$admin]) {
      fluidPage(
        h2("Analytics"),
        tableOutput("user_rankings"),
        selectInput("selected_user", "Select User", choices = app_server$credentials()$user),
        actionButton("show_stats", "Show Statistics"),
        tableOutput("user_stats"),
        selectInput("selected_question", "Select Question", choices = app_server$survey_manager$survey_questions(), selected = NULL),
        plotOutput("question_histogram")
      )
    } else {
      fluidPage(
        h2("Analytics"),
        p("Access denied. Admins only.")
      )
    }
  })

  observeEvent(input$show_stats, {
    req(input$selected_user)
    if (file.exists("userAnswers.xlsx")) {
      user_answers <- read_xlsx("userAnswers.xlsx")
      user_data <- user_answers %>% filter(username == input$selected_user)
      if (nrow(user_data) > 0) {
        output$user_stats <- renderTable({
          user_data
        })
      } else {
        output$user_stats <- renderTable({
          data.frame(Message = "No data available for this user.")
        })
      }
    }
  })

  observe({
    req(input$selected_question)
    if (file.exists("userAnswers.xlsx")) {
      user_answers <- read_xlsx("userAnswers.xlsx")
      question_column <- input$selected_question
      correct_answer <- as.character(app_server$survey_manager$correct_answers()[[question_column]])

      output$question_histogram <- renderPlot({
        ggplot(user_answers, aes_string(x = question_column)) +
          geom_bar(stat = "count", fill = "blue", alpha = 0.5) +
          geom_bar(data = subset(user_answers, user_answers[[question_column]] == correct_answer),
                   aes_string(x = question_column), fill = "red", alpha = 0.8) +
          geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.5) +
          geom_text(data = subset(user_answers, user_answers[[question_column]] == correct_answer),
                    aes_string(x = question_column, y = paste0("sum(user_answers[['", question_column, "']] == '", correct_answer, "') + 0.5")),
                    label = "Correct Answer",
                    color = "black", fontface = "bold", vjust = -1.5) +
          theme_minimal() +
          labs(title = paste("Distribution of Answers for", question_column),
               x = "Answers", y = "Count") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
  })

  output$user_rankings <- renderTable({
    if (file.exists("userAnswers.xlsx")) {
      user_answers <- read_xlsx("userAnswers.xlsx")
      rankings <- user_answers %>%
        group_by(username) %>%
        summarise(TotalCorrect = sum(as.numeric(Correctness), na.rm = TRUE)) %>%
        arrange(desc(TotalCorrect))
      rankings
    } else {
      data.frame(Message = "No data available.")
    }
  })

  output$download_answers_ui <- renderUI({
    fluidPage(
      h2("Download Answers"),
      downloadButton("download_answers", "Download Answers")
    )
  })

  output$download_answers <- downloadHandler(
    filename = function() {
      "userAnswers.xlsx"
    },
    content = function(file) {
      file.copy("userAnswers.xlsx", file)
    }
  )
}

shinyApp(ui = ui, server = server)
