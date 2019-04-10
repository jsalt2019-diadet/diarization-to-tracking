library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Speaker tracking"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("BabyTrain", tabName = "BabyTrain", icon = icon("database")),
      menuItem("AMI", tabName = "AMI", icon = icon("database")),
      menuItem("CHiME5", tabName = "CHiME5", icon = icon("database"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "BabyTrain",
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "enr_dur", label = "Enrollment duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_babytrain",height=400)),
                box(plotOutput("enrollment2_babytrain",height=400))
              ),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("trial1_babytrain",height=400)),
                box(plotOutput("trial2_babytrain",height=400))
              ),
              fluidRow(
                column(width = 12, align = "right", plotOutput("trial3_babytrain",height=400))
              )
      ),
      tabItem(tabName = "AMI",
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "enr_dur", label = "Enrollment duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_ami",height=400)),
                box(plotOutput("enrollment2_ami",height=400))
              ),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("trial1_ami",height=400)),
                box(plotOutput("trial2_ami",height=400))
              ),
              fluidRow(
                column(width = 12, align = "right", plotOutput("trial3_ami",height=400))
              )
      ),
      tabItem(tabName = "CHiME5",
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "enr_dur", label = "Enrollment duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_chime5",height=400)),
                box(plotOutput("enrollment2_chime5",height=400))
              ),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("trial1_chime5",height=400)),
                box(plotOutput("trial2_chime5",height=400))
              ),
              fluidRow(
                column(width = 12, align = "right", plotOutput("trial3_chime5",height=400))
              )
      )
    )
  )
)

get_enrollment <- function(folder_path){
  enrollment = read.table(file.path(folder_path, "enrollment.txt"), header = TRUE, sep = "\t", dec = ".")
  enrollment["duration"] =  as.numeric(enrollment$offset) - as.numeric(enrollment$onset)
  return(enrollment)
}

get_trials <- function(folder_path){
  trials = read.table(file.path(folder_path, "trials.txt"), header = TRUE, sep = "\t", dec = ".")
  return(trials)
}

plot_nb_enrollment_per_speaker <- function(enrollment,bbt=FALSE){
  if(bbt == TRUE){
    fontsize=1
  } else {
    fontsize=12
  }
  # First, let's find the number of enrollment per speaker
  enrollment_per_speaker = aggregate(model_number ~ speaker, data=enrollment, max)
  enrollment_per_speaker["model_number"] = enrollment_per_speaker["model_number"] + 1
  p1 <- ggplot(data=enrollment_per_speaker,  mapping=aes(x=reorder(speaker,-model_number, identity),model_number)) + 
    geom_col(fill=I("cadetblue"),col=I("brown")) + 
    xlab("speaker id") + ylab("number of enrollments") + 
    ggtitle("Number of enrollments per speaker") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size=fontsize))
  return(p1)
}

plot_last_decile_enrollment_duration_per_speaker <- function(enrollment,bbt=FALSE){
  if(bbt == TRUE){
    fontsize=1
  } else {
    fontsize=12
  }
  dur_per_enrollment = aggregate(duration~speaker+model_number, enrollment, sum)
  dur_per_enrollment = aggregate(duration~speaker, dur_per_enrollment,  FUN = quantile, probs = 0.9)
  
  p1 <- ggplot(data=dur_per_enrollment,  mapping=aes(x=reorder(speaker,-duration, identity),duration)) +
    geom_col(fill=I("cadetblue"),col=I("brown")) +
    xlab("speaker id") + ylab("last decile") +
    ggtitle("Last decile of enrollment's duration per speaker") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size=fontsize))
  return(p1)
}

plot_non_target_trials_per_speaker <- function(trials,bbt=FALSE){
  if(bbt == TRUE){
    fontsize=1
  } else {
    fontsize=12
  }
  non_target_trials = trials[trials["duration_total_speech"] == 0,]
  non_target_trials_per_speaker = count(non_target_trials, target_speaker)
  p1 <- ggplot(data=non_target_trials_per_speaker,  mapping=aes(x=reorder(target_speaker,-n, identity),n)) +
    geom_col(fill=I("cadetblue"),col=I("brown")) +
    xlab("speaker id") + ylab("number of non-target trials") +
    ggtitle("Number of non-target trials per speaker") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size=fontsize))
  return(p1)
}

plot_target_trials_per_speaker <- function(trials,bbt=FALSE){
  if(bbt == TRUE){
    fontsize=1
  } else {
    fontsize=12
  }
  target_trials = trials[trials["duration_total_speech"] != 0,]
  target_trials_per_speaker = count(target_trials, target_speaker)
  p1 <- ggplot(data=target_trials_per_speaker,  mapping=aes(x=reorder(target_speaker,-n, identity),n)) +
    geom_col(fill=I("cadetblue"),col=I("brown")) +
    xlab("speaker id") + ylab("number of target trials") +
    ggtitle("Number of target trials per speaker") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size=fontsize))
  return(p1)
}

plot_most_overlapping_trials <- function(trials,bbt=FALSE){
  if(bbt == TRUE){
    fontsize=1
  } else {
    fontsize=12
  }
  target_trials = trials[trials["duration_total_speech"] != 0,]
  most_overlapping = aggregate(duration_overlapping_speech~target_speaker, target_trials,  FUN = quantile, probs = 0.9)
  p1 <- ggplot(data=most_overlapping,  mapping=aes(x=reorder(target_speaker,-duration_overlapping_speech, identity),duration_overlapping_speech)) +
    geom_col(fill=I("cadetblue"),col=I("brown")) +
    xlab("speaker id") + ylab("last decile") +
    ggtitle("Last decile of the overlapping speech duration per speaker") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size=fontsize))
  return(p1)
}

server <- function(input, output) {
  # AMI
  ami_path = "../data/ami"
  ami_trials = reactive({
    get_trials(ami_path)
  })
  ami_enrollment = reactive({
    get_enrollment(ami_path)
  })
  
  output$enrollment1_ami <- renderPlot({plot_nb_enrollment_per_speaker(ami_enrollment())})
  output$enrollment2_ami <- renderPlot({plot_last_decile_enrollment_duration_per_speaker(ami_enrollment())})
  
  output$trial1_ami <- renderPlot({plot_non_target_trials_per_speaker(ami_trials())})
  output$trial2_ami <- renderPlot({plot_target_trials_per_speaker(ami_trials())})
  output$trial3_ami <- renderPlot({plot_most_overlapping_trials(ami_trials())})
  
  # CHiME5
  chime5_path = "../data/chime5"
  chime5_trials = reactive({
    get_trials(chime5_path)
  })
  chime5_enrollment = reactive({
    get_enrollment(chime5_path)
  })
  
  output$enrollment1_chime5 <- renderPlot({plot_nb_enrollment_per_speaker(chime5_enrollment())})
  output$enrollment2_chime5 <- renderPlot({plot_last_decile_enrollment_duration_per_speaker(chime5_enrollment())})
  
  output$trial1_chime5 <- renderPlot({plot_non_target_trials_per_speaker(chime5_trials())})
  output$trial2_chime5 <- renderPlot({plot_target_trials_per_speaker(chime5_trials())})
  output$trial3_chime5 <- renderPlot({plot_most_overlapping_trials(chime5_trials())})
  
  # babytrain
  babytrain_path = "../data/babytrain"
  babytrain_trials = reactive({
    get_trials(babytrain_path)
  })
  babytrain_enrollment = reactive({
    get_enrollment(babytrain_path)
  })
  
  output$enrollment1_babytrain <- renderPlot({plot_nb_enrollment_per_speaker(babytrain_enrollment(), bbt=TRUE)})
  output$enrollment2_babytrain <- renderPlot({plot_last_decile_enrollment_duration_per_speaker(babytrain_enrollment(), bbt=TRUE)})
  
  output$trial1_babytrain <- renderPlot({plot_non_target_trials_per_speaker(babytrain_trials(), bbt=TRUE)})
  output$trial2_babytrain <- renderPlot({plot_target_trials_per_speaker(babytrain_trials(), bbt=TRUE)})
  output$trial3_babytrain <- renderPlot({plot_most_overlapping_trials(babytrain_trials(), bbt=TRUE)})
  
  
  # output$plot1 <- renderPlot({data <- histdata[seq_len(input$enr_dur)]
  #                             hist(data)
  #                             })
  # output$enr_dur <- renderText({ 
  #   paste("You have selected", input$enr_dur)
  # })
  
  # Might be useful for debugging
  # output$data <- renderTable({
  #   head(ami_trials(), n=5)
  # })
}

shinyApp(ui, server)

