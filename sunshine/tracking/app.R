library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(lattice)
library(dplyr)

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
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "BABYTRAIN_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_babytrain",height=400)),
                box(plotOutput("enrollment2_babytrain",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("Trials"),
              fluidRow(column(width = 6, textOutput("nb_non_target_trials_babytrain")), column(width = 6, textOutput("nb_target_trials_babytrain"))),
              fluidRow(
                box(plotOutput("trial1_babytrain",height=400)),
                box(plotOutput("trial2_babytrain",height=400))
              ),
              fluidRow(
                box(plotOutput("trial3_babytrain",height=400)),
                box(plotOutput("trial4_babytrain", height=400))
              ),
              fluidRow(
                column(width = 12, offset= 3, plotOutput("scatter_babytrain", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Trials\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Trials\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Trials\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Trials\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of enrollments\n", align="center")
              )
      ),
      tabItem(tabName = "AMI",
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "AMI_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_ami",height=400)),
                box(plotOutput("enrollment2_ami",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("Trials"),
              fluidRow(column(width = 6, textOutput("nb_non_target_trials_ami")), column(width = 6, textOutput("nb_target_trials_ami"))),
              fluidRow(
                box(plotOutput("trial1_ami",height=400)),
                box(plotOutput("trial2_ami",height=400))
              ),
              fluidRow(
                box(plotOutput("trial3_ami",height=400)),
                box(plotOutput("trial4_ami",height=400))
              ),
              fluidRow(
                column(width = 12, offset= 3, plotOutput("scatter_ami", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Trials\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Trials\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Trials\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Trials\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of enrollments\n", align="center")
              )
      ),
      tabItem(tabName = "CHiME5",
              titlePanel("Enrollment"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "CHIME5_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_chime5",height=400)),
                box(plotOutput("enrollment2_chime5",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("Trials"),
              fluidRow(column(width = 6, textOutput("nb_non_target_trials_chime5")), column(width = 6, textOutput("nb_target_trials_chime5"))),
              fluidRow(
                box(plotOutput("trial1_chime5",height=400)),
                box(plotOutput("trial2_chime5",height=400))
              ),
              fluidRow(
                box(plotOutput("trial3_chime5",height=400)),
                box(plotOutput("trial4_chime5",height=400))
              ),
              fluidRow(
                column(width = 12, offset= 3, plotOutput("scatter_chime5", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Trials\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Trials\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Trials\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Trials\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of Enrollments\n", align="center")
              )
      )
    )
  )
)

get_enrollment <- function(folder_path, enrollment_duration){
  enrollment = read.table(file.path(folder_path, paste("enrollment_", enrollment_duration, ".txt", sep="")), header = TRUE, sep = "\t", dec = ".")
  enrollment["duration"] =  as.numeric(enrollment$offset) - as.numeric(enrollment$onset)
  return(enrollment)
}

get_trials <- function(folder_path, trial_duration=60){
  trials = read.table(file.path(folder_path, paste("trials_", trial_duration, ".txt", sep="")), header = TRUE, sep = "\t", dec = ".")
  return(trials)
}

# summary_trials <- function(trials){
#   nb_target_trials = length(ami_trials[ami_trials["duration_total_speech"] != 0,"type"])
#   nb_non_target_trials = length(ami_trials[ami_trials["duration_total_speech"] == 0,"type"])
# }
# 
# summary_enrollment <- function(enrollment){
#   nb_enrollment = length(enrollment)
# }
plot_nb_enrollment_per_speaker <- function(enrollment){
  # First, let's find the number of enrollment per speaker
  enrollment_per_speaker = aggregate(model_number ~ speaker, data=enrollment, max)
  enrollment_per_speaker["model_number"] = enrollment_per_speaker["model_number"] + 1
  p1 <- ggplot(data=enrollment_per_speaker, mapping=aes(model_number)) + geom_histogram(color='brown', fill='cadetblue') +
    xlab("number of enrollments") + ylab("number of speakers") + ggtitle("Distribution of the number of enrollments")
  return(p1)
}

plot_enrollment_duration_per_speaker <- function(enrollment){
  dur_per_enrollment = aggregate(duration~speaker+model_number, enrollment, sum)

  p1 <- ggplot(data=dur_per_enrollment,  mapping=aes(duration)) +
    geom_histogram(color='brown', fill='cadetblue') +
    ylab("number of enrollments") + xlab("duration (s)") +
    ggtitle("Distribution of the duration of speech pronounced by the target speaker")
  return(p1)
}

plot_non_target_trials_per_speaker <- function(trials){
  non_target_trials = trials[trials["duration_total_speech"] == 0,]
  non_target_trials = count(non_target_trials, target_speaker)
  p1 <- ggplot(data=non_target_trials,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue') +
    xlab("number of non-target trials") + ylab("number of speakers") +
    ggtitle("Distribution of the number of non-target trials")
  return(p1)
}

plot_target_trials_per_speaker <- function(trials){
  target_trials = trials[trials["duration_total_speech"] != 0,]
  target_trials = count(target_trials, target_speaker)
  p1 <- ggplot(data=target_trials,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue') +
    xlab("number of target trials") + ylab("number of speakers") +
    ggtitle("Distribution of the number of target trials")
  return(p1)
}

plot_overlapping_duration <- function(trials){
  target_trials = trials[trials["duration_total_speech"] != 0,]
  target_trials["duration_clean_speech"] = target_trials["duration_total_speech"] - target_trials["duration_overlapping_speech"]

  p1 <- ggplot(data=target_trials,  mapping=aes(c(duration_overlapping_speech,duration_clean_speech))) +
    geom_histogram(color='brown', fill='cadetblue', mapping = aes(duration_overlapping_speech)) +
    #geom_histogram(color='brown', fill='red', mapping = aes(duration_clean_speech)) +
    xlab("Duration (s)") + ylab("number of target trials") +
    ggtitle("Distribution of the amount of overlapping speech in trials")
  
  return(p1)
}

plot_overlapping_percent <- function(trials){
  target_trials = trials[trials["duration_total_speech"] != 0,]
  target_trials["duration_overlapping_percent"] = target_trials["duration_overlapping_speech"] * 100 / target_trials["duration_total_speech"]

  p1 <- ggplot(data=target_trials,  mapping=aes(duration_overlapping_percent)) +
    geom_histogram(color='brown', fill='cadetblue', mapping = aes(duration_overlapping_percent)) +
    #geom_histogram(color='brown', fill='red', mapping = aes(duration_clean_speech)) +
    xlab("Duration (%)") + ylab("number of target trials") +
    ggtitle("Distribution of the part of the overlapping speech in target trials")
  return(p1)
}

plot_scatter <- function(trials, enrollment){
  # At the file scale
  # Let's compute number of target trials vs number of non-target trials
  trials[c("target","non_target")] = 0
  trials[trials["duration_total_speech"] != 0,"target"] = 1
  trials[trials["duration_total_speech"] == 0,"non_target"] = 1
  nb_trials = aggregate(.~file_basename, trials, sum)[c("file_basename", "target", "non_target")]
  colnames(nb_trials) = c("filename", "number_target_trials", "number_non_target_trials")
  
  # The average duration of total speech and overlapping speech
  average_duration_tri = aggregate(.~file_basename, trials, mean)[c("file_basename", "duration_total_speech", "duration_overlapping_speech")]
  colnames(average_duration_tri) = c("filename", "average_duration_total_speech_trials", "average_duration_clean_speech_trials")
  
  # The average duration of speech in enrollment for the target speaker
  aggregated_enrollment = aggregate(duration~filename+model_number+speaker, enrollment, sum)
  avg_duration_enr = aggregate(duration~filename, aggregated_enrollment, mean)
  colnames(avg_duration_enr) = c("filename", "average_duration_speech_target_speaker_enrollment")
  
  # The number of enrollment
  nb_enr = aggregate(model_number~filename,aggregated_enrollment, length)
  colnames(nb_enr) = c("filename", "number_enrollments")
  
  scatter = merge(nb_trials, average_duration_tri, by="filename")
  scatter = merge(scatter, avg_duration_enr, by="filename")
  scatter = merge(scatter, nb_enr, by="filename")
  colnames(scatter) = c("filename", "#NTT", "#NNTT", "TSTT", "OSTT", "TSE", "#E")
  
  p1 <- splom(scatter[2:7], alpha=0.6, pscales=0)
  return(p1)
}

server <- function(input, output) {
  enrollment_duration_bbt = reactive({input$BABYTRAIN_enr_dur})
  enrollment_duration_ami = reactive({input$AMI_enr_dur})
  enrollment_duration_chime5 = reactive({input$CHIME5_enr_dur})
  
  # AMI
  ami_path = "data/ami"
  ami_trials = reactive({
    get_trials(ami_path)
  })
  ami_enrollment = reactive({
    get_enrollment(ami_path, enrollment_duration_ami())
  })
  
  output$nb_non_target_trials_ami <- renderText({paste("Number of non-target trials : " , nrow(ami_trials()[ami_trials()["duration_total_speech"] == 0,]))})
  output$nb_target_trials_ami <- renderText({paste("Number of target trials : " , nrow(ami_trials()[ami_trials()["duration_total_speech"] != 0,]))})
  output$enrollment1_ami <- renderPlot({plot_nb_enrollment_per_speaker(ami_enrollment())})
  output$enrollment2_ami <- renderPlot({plot_enrollment_duration_per_speaker(ami_enrollment())})

  output$trial1_ami <- renderPlot({plot_non_target_trials_per_speaker(ami_trials())})
  output$trial2_ami <- renderPlot({plot_target_trials_per_speaker(ami_trials())})
  output$trial3_ami <- renderPlot({plot_overlapping_duration(ami_trials())})
  output$trial4_ami <- renderPlot({plot_overlapping_percent(ami_trials())})
  
  output$scatter_ami <- renderPlot({plot_scatter(ami_trials(), ami_enrollment())})
  
  # CHiME5
  chime5_path = "data/chime5"
  chime5_trials = reactive({
    get_trials(chime5_path)
  })
  chime5_enrollment = reactive({
    get_enrollment(chime5_path, enrollment_duration_chime5())
  })

  output$nb_non_target_trials_chime5 <- renderText({paste("Number of non-target trials : " , nrow(chime5_trials()[chime5_trials()["duration_total_speech"] == 0,]))})
  output$nb_target_trials_chime5 <- renderText({paste("Number of target trials : " , nrow(chime5_trials()[chime5_trials()["duration_total_speech"] != 0,]))})
  
  output$enrollment1_chime5 <- renderPlot({plot_nb_enrollment_per_speaker(chime5_enrollment())})
  output$enrollment2_chime5 <- renderPlot({plot_enrollment_duration_per_speaker(chime5_enrollment())})

  output$trial1_chime5 <- renderPlot({plot_non_target_trials_per_speaker(chime5_trials())})
  output$trial2_chime5 <- renderPlot({plot_target_trials_per_speaker(chime5_trials())})
  output$trial3_chime5 <- renderPlot({plot_overlapping_duration(chime5_trials())})
  output$trial4_chime5 <- renderPlot({plot_overlapping_percent(chime5_trials())})
  
  output$scatter_chime5 <- renderPlot({plot_scatter(chime5_trials(), chime5_enrollment())})
  # babytrain
  babytrain_path = "data/babytrain"
  babytrain_trials = reactive({
    get_trials(babytrain_path)
  })
  babytrain_enrollment = reactive({
    get_enrollment(babytrain_path, enrollment_duration_bbt())
  })

  output$nb_non_target_trials_babytrain <- renderText({paste("Number of non-target trials : " , nrow(babytrain_trials()[babytrain_trials()["duration_total_speech"] == 0,]))})
  output$nb_target_trials_babytrain <- renderText({paste("Number of target trials : " , nrow(babytrain_trials()[babytrain_trials()["duration_total_speech"] != 0,]))})
  output$enrollment1_babytrain <- renderPlot({plot_nb_enrollment_per_speaker(babytrain_enrollment())})
  output$enrollment2_babytrain <- renderPlot({plot_enrollment_duration_per_speaker(babytrain_enrollment())})

  output$trial1_babytrain <- renderPlot({plot_non_target_trials_per_speaker(babytrain_trials())})
  output$trial2_babytrain <- renderPlot({plot_target_trials_per_speaker(babytrain_trials())})
  output$trial3_babytrain <- renderPlot({plot_overlapping_duration(babytrain_trials())})
  output$trial4_babytrain <- renderPlot({plot_overlapping_percent(babytrain_trials())})
  
  output$scatter_babytrain <- renderPlot({plot_scatter(babytrain_trials(), babytrain_enrollment())})
}

shinyApp(ui, server)

