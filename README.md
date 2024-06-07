Following application is Shiny-based web application for managing and conducting surveys.

The most valuables features of application are:
- User registration and login
- User and Admin roles with different accesses
- Generating surveys based on provided excel file
- Basic analytic of survey responses
- Survey answers downloading

App Structure

Classes:
- User: authentication and use information
- SurveyManager: all activities connected with survey(loading it generating UI, collecting responses, counting correct answers)
- AppUI: UI for the app
- AppServer: server-side logic and reactive values

MainUI(navbar page):
- Login: Allows users to log in.
- Register: Allows new users to register.
- Upload Survey: Allows admins to upload surveys.
- Survey: Displays the survey for users to complete.
- Analytics: Displays analytics for admins.
- Download Answers: Allows admins to download the survey answers.
- Logout: Allows users to log out.

User roles:
- Admin: All activities within application
- Regular User: registration + survey taking

File Management
- users.xlsx: Stores user credentials.
- survey_questions.xlsx: Stores the survey questions.
- answers.xlsx: Stores the correct answers for the survey.
- userAnswers.xlsx: Stores the user responses to the survey.

Analytics
- User rankings based on the number of correct answers.
- Individual user statistics.
- Distribution of answers for each survey question.
- Admins can also download all users responses

This app uses the following R packages:
- shiny
- shinymanager
- shinyjs
- readxl
- writexl
- tidyverse
- R6
