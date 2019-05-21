library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(lattice)
library(dplyr)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "Speaker tracking"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("BabyTrain", tabName = "bbt", icon = icon("database")),
      menuItem("AMI", tabName = "ami", icon = icon("database")),
      menuItem("CHiME5", tabName = "CHiME5", icon = icon("database"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "bbt",
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "bbt_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(15,30)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_bbt",height=400)),
                box(plotOutput("enrollment2_bbt",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "test_segments_dur", label = "test_segments duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("Test segments"),
              fluidRow(column(width = 6, textOutput("nb_non_target_test_segments_bbt")), column(width = 6, textOutput("nb_target_test_segments_bbt"))),
              fluidRow(
                box(plotOutput("test_segments1_bbt",height=400)),
                box(plotOutput("test_segments2_bbt",height=400))
              ),
                div(plotOutput("test_segments3_bbt",height=400),align="center")
              ,
              fluidRow(
                box(plotOutput("test_segments4_bbt",height=400)),
                box(plotOutput("test_segments5_bbt",height=400))
              ),
              fluidRow(
                box(plotOutput("test_segments6_bbt",height=400)),
                box(plotOutput("test_segments7_bbt",height=400))
              ),
              titlePanel("Trials (natural subsampling)"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "bbt_n_tri_nat", label = "N (total number of trials):", choices = c(100,200,300,400,500,600,700,800,900,1000)*10))
              ),
              fluidRow(column(width = 6, textOutput("nb_target_trials_bbt_nat")), column(width = 6, textOutput("nb_non_target_trials_bbt_nat"))),
              fluidRow(column(width = 6, textOutput("nb_speakers_bbt_nat"))),
              fluidRow(column(12, align="center", tableOutput("gender_bbt_nat"))),
              fluidRow(column(12, align="center", tableOutput("inter_ses_mic_bbt_nat"))),
              fluidRow(
                box(plotOutput("bbt_trial_plots_nat1",height=400)),
                box(plotOutput("bbt_trial_plots_nat2",height=400))
              ),
              titlePanel("Trials (square subsampling)"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "bbt_n_tri_square", label = "N (number of trials per speaker):", choices = c(100,200,300,400,500,600,700,800,900,1000)))
              ),
              fluidRow(column(width = 6, textOutput("nb_target_trials_bbt_square")), column(width = 6, textOutput("nb_non_target_trials_bbt_square"))),
              fluidRow(column(width = 6, textOutput("nb_speakers_bbt_square"))),
              fluidRow(column(12, align="center", tableOutput("gender_bbt_square"))),
              fluidRow(column(12, align="center", tableOutput("inter_ses_mic_bbt_square"))),
              fluidRow(
                box(plotOutput("bbt_trial_plots_square1",height=400)),
                box(plotOutput("bbt_trial_plots_square2",height=400))
              ),
              titlePanel("Correlations"),
              fluidRow(
                box(plotOutput("scatter_bbt", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Tests\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Tests\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Tests\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Tests\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of Enrollments\n", align="center")
              )
      ),
      tabItem(tabName = "ami",
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "ami_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,15,30)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_ami",height=400)),
                box(plotOutput("enrollment2_ami",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "test_segments_dur", label = "test_segments duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("Test segments"),
              fluidRow(column(width = 6, textOutput("nb_non_target_test_segments_ami")), column(width = 6, textOutput("nb_target_test_segments_ami"))),
              fluidRow(
                box(plotOutput("test_segments1_ami",height=400)),
                box(plotOutput("test_segments2_ami",height=400))
              ),
              div(plotOutput("test_segments3_ami",height=400),align="center")
              ,
              fluidRow(
                box(plotOutput("test_segments4_ami",height=400)),
                box(plotOutput("test_segments5_ami",height=400))
              ),
              fluidRow(
                box(plotOutput("test_segments6_ami",height=400)),
                box(plotOutput("test_segments7_ami",height=400))
              ),
              titlePanel("Trials (natural subsampling)"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "ami_n_tri_nat", label = "N (total number of trials):", choices = c(100,200,300,400,500,600,700,800,900,1000)*10))
              ),
              fluidRow(column(width = 6, textOutput("nb_target_trials_ami_nat")), column(width = 6, textOutput("nb_non_target_trials_ami_nat"))),
              fluidRow(column(width = 6, textOutput("nb_speakers_ami_nat"))),
              fluidRow(column(12, align="center", tableOutput("gender_ami_nat"))),
              fluidRow(column(12, align="center", tableOutput("inter_ses_mic_ami_nat"))),
              fluidRow(
                box(plotOutput("ami_trial_plots_nat1",height=400)),
                box(plotOutput("ami_trial_plots_nat2",height=400))
              ),
              titlePanel("Trials (square subsampling)"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "ami_n_tri_square", label = "N (number of trials per speaker):", choices = c(100,200,300,400,500,600,700,800,900,1000)))
              ),
              fluidRow(column(width = 6, textOutput("nb_target_trials_ami_square")), column(width = 6, textOutput("nb_non_target_trials_ami_square"))),
              fluidRow(column(width = 6, textOutput("nb_speakers_ami_square"))),
              fluidRow(column(12, align="center", tableOutput("gender_ami_square"))),
              fluidRow(column(12, align="center", tableOutput("inter_ses_mic_ami_square"))),
              fluidRow(
                box(plotOutput("ami_trial_plots_square1",height=400)),
                box(plotOutput("ami_trial_plots_square2",height=400))
              ),
              titlePanel("Correlations"),
              fluidRow(
                column(width = 12, offset= 3, plotOutput("scatter_ami", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Tests\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Tests\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Tests\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Tests\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of Enrollments\n", align="center")
              )
      ),
      tabItem(tabName = "CHiME5",
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "chime5_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,15,30)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_chime5",height=400)),
                box(plotOutput("enrollment2_chime5",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "test_segments_dur", label = "test_segments duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("Test segments"),
              fluidRow(column(width = 6, textOutput("nb_non_target_test_segments_chime5")), column(width = 6, textOutput("nb_target_test_segments_chime5"))),
              fluidRow(
                box(plotOutput("test_segments1_chime5",height=400)),
                box(plotOutput("test_segments2_chime5",height=400))
              ),
              div(plotOutput("test_segments3_chime5",height=400),align="center")
              ,
              fluidRow(
                box(plotOutput("test_segments4_chime5",height=400)),
                box(plotOutput("test_segments5_chime5",height=400))
              ),
              fluidRow(
                box(plotOutput("test_segments6_chime5",height=400)),
                box(plotOutput("test_segments7_chime5",height=400))
              ),
              titlePanel("Trials (natural subsampling)"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "chime5_n_tri_nat", label = "N (total number of trials):", choices = c(100,200,300,400,500,600,700,800,900,1000)*10))
              ),
              fluidRow(column(width = 6, textOutput("nb_target_trials_chime5_nat")), column(width = 6, textOutput("nb_non_target_trials_chime5_nat"))),
              fluidRow(column(width = 6, textOutput("nb_speakers_chime5_nat"))),
              fluidRow(column(12, align="center", tableOutput("inter_ses_mic_chime5_nat"))),
              fluidRow(
                box(plotOutput("chime5_trial_plots_nat1",height=400)),
                box(plotOutput("chime5_trial_plots_nat2",height=400))
              ),
              titlePanel("Trials (square subsampling)"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "chime5_n_tri_square", label = "N (number of trials per speaker):", choices = c(100,200,300,400,500,600,700,800,900,1000)))
              ),
              
              fluidRow(column(width = 6, textOutput("nb_target_trials_chime5_square")), column(width = 6, textOutput("nb_non_target_trials_chime5_square"))),
              fluidRow(column(width = 6, textOutput("nb_speakers_chime5_square"))),
              fluidRow(column(12, align="center", tableOutput("inter_ses_mic_chime5_square"))),
              fluidRow(
                box(plotOutput("chime5_trial_plots_square1",height=400)),
                box(plotOutput("chime5_trial_plots_square2",height=400))
              ),
              titlePanel("Correlations"),
              fluidRow(
                column(width = 12, offset= 3, plotOutput("scatter_chime5", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Tests\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Tests\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Tests\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Tests\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of Enrollments\n", align="center")
              )
      )
    )
  )
)

get_enrollment <- function(folder_path, enrollment_duration){
  enrollment = read.table(file.path(folder_path, paste("enrollments_", enrollment_duration, ".txt", sep="")), header = TRUE, sep = "\t", dec = ".")
  enrollment["duration"] =  as.numeric(enrollment$offset) - as.numeric(enrollment$onset)
  return(enrollment)
}

get_test_segments <- function(folder_path, test_segments_duration=60){
  test_segments = read.table(file.path(folder_path, paste("test_segments_", test_segments_duration, ".txt", sep="")), header = TRUE, sep = "\t", dec = ".")
  return(test_segments)
}

plot_enrollment <- function(enrollment){
  # Plot distribution of number of enrollments (across speakers)
  enrollment_per_speaker = aggregate(model_number ~ speaker, data=enrollment, max)
  enrollment_per_speaker["model_number"] = enrollment_per_speaker["model_number"]+1
  p1 <- ggplot(data=enrollment_per_speaker, mapping=aes(model_number)) + 
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) + 
    xlab("number of enrollments") + ylab("number of speakers") + ggtitle("Distribution of the number of enrollments")
  
  # Plot distribution duration of enrollment
  dur_per_enrollment = aggregate(duration~speaker+model_number, enrollment, sum)
  p2 <- ggplot(data=dur_per_enrollment,  mapping=aes(duration)) +
    geom_histogram(color='brown', fill='cadetblue', bins=50, closed = "left", boundary=0) +
    ylab("number of enrollments") + xlab("duration (s)") +
    ggtitle("Distribution of the duration of speech pronounced by the target speaker")
  
  return(list("p1" = p1, "p2" = p2))
}

plot_test_segments <- function(test_segments){
  # Distribution of the number of non-target test_segments
  non_target_test_segments = test_segments[test_segments["duration_total_speech"] == 0,]
  non_target_test_segments = count(non_target_test_segments, target_speaker)
  
  p1 <- ggplot(data=non_target_test_segments,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("number of non-target test segments") + ylab("number of speakers") +
    ggtitle("Distribution of the number of non-target test segments")
  
  # Distribution of the number of target test_segments
  target_test_segments = test_segments[test_segments["duration_total_speech"] != 0,]
  nb_target_test_segments = count(target_test_segments, target_speaker)
  
  p2 <- ggplot(data=nb_target_test_segments,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("number of target test segments") + ylab("number of speakers") +
    ggtitle("Distribution of the number of target test segments")
  
  # Distribution of speech (overlapping+clean) (s)
  p3 <- ggplot(data=target_test_segments,  mapping=aes(duration_total_speech)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("Duration (s)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of speech (overlapping+clean) in target test segments")
  
  # Distribution of overlapping speech duration (s)
  p4 <- ggplot(data=target_test_segments,  mapping=aes(duration_overlapping_speech)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    #geom_histogram(color='brown', fill='red', mapping = aes(duration_clean_speech)) +
    xlab("Duration (s)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of overlapping speech (in s) in target test segments")
  
  # Distribution of overlapping speech duration (%)
  target_test_segments["duration_overlapping_percent"] = target_test_segments["duration_overlapping_speech"] * 100 / target_test_segments["duration_total_speech"]
  p5 <- ggplot(data=target_test_segments,  mapping=aes(duration_overlapping_percent)) +
    geom_histogram(color='brown', fill='cadetblue', bins=50, closed = "left", boundary=0) +
    #geom_histogram(color='brown', fill='red', mapping = aes(duration_clean_speech)) +
    xlab("Duration (%)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of overlapping speech (in %) in target test segments")
  
  # Distribution of clean speech duration (s)
  target_test_segments["duration_clean_speech"] = target_test_segments["duration_total_speech"] - target_test_segments["duration_overlapping_speech"]
  p6 <- ggplot(data=target_test_segments,  mapping=aes(duration_clean_speech)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("Duration (s)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of clean speech (in s) in target test segments")
  
  # Distribution of clean speech duration (%)
  target_test_segments["duration_clean_percent"] = target_test_segments["duration_clean_speech"] * 100 / target_test_segments["duration_total_speech"]
  p7 <- ggplot(data=target_test_segments,  mapping=aes(duration_clean_percent)) +
    geom_histogram(color='brown', fill='cadetblue', bins=50, closed = "left", boundary=0) +
    xlab("Duration (%)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of clean speech (in %) in target test segments")
  
  return(list("p1" = p1, "p2" = p2, "p3" = p3,
              "p4" = p4, "p5" = p5, "p6" = p6,
              "p7" = p7))
}

plot_trials <- function(trials){
  # Distribution of the number of non-target trials
  non_target_trials = trials[trials["duration_total_speech"] == 0,]
  non_target_trials = count(non_target_trials, target_speaker)
  
  p1 <- ggplot(data=non_target_trials,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue', closed = "left", boundary=0) +
    xlab("number of non-target trials") + ylab("number of speakers") +
    ggtitle("Distribution of the number of non-target trials")
  
  # Distribution of the number of target trials
  target_trials = trials[trials["duration_total_speech"] != 0,]
  nb_target_trials = count(target_trials, target_speaker)
  
  p2 <- ggplot(data=nb_target_trials,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue', closed = "left", boundary=0) +
    xlab("number of trials") + ylab("number of speakers") +
    ggtitle("Distribution of the number of target trials")
  
  return(list("p1" = p1, "p2" = p2))
}

get_gender_percentage <- function(trials, bbt=FALSE){
  if(bbt) {
    if(dim(trials)[1] != 0) {
      nb_mal = sum(apply(trials["target_speaker"], 2, startsWith, "!FAT"))
      nb_fem = sum(apply(trials["target_speaker"], 2, startsWith, "!MOT"))
      nb_inv = sum(apply(trials["target_speaker"], 2, startsWith, "!INV"))
      nb_chi = sum(apply(trials["target_speaker"], 2, startsWith, "!CHI"))
      tot = nb_mal+nb_fem+nb_inv+nb_chi
      nb_mal = nb_mal/tot
      nb_fem = nb_fem/tot
      nb_inv = nb_inv/tot
      nb_chi = nb_chi/tot
    } else {
      nb_mal = 0
      nb_fem = 0
      nb_inv = 0
      nb_chi = 0
    }
    percentage = data.frame("MAL" = nb_mal, "FEM" = nb_fem, "INV" = nb_inv, "CHI" = nb_chi)
  } else {
    # Handle AMI case
    if(dim(trials)[1] != 0) {
      nb_mal = sum(apply(trials["target_speaker"], 2, startsWith, "M"))
      nb_fem = sum(apply(trials["target_speaker"], 2, startsWith, "F"))
      tot = nb_mal+nb_fem
      nb_mal = nb_mal/tot
      nb_fem = nb_fem/tot
    } else {
      nb_mal = 0
      nb_fem = 0
    }
    percentage = data.frame("MAL" = nb_mal, "FEM" = nb_fem)
  }
  return(percentage)
}

plot_scatter <- function(test_segments, enrollment, bbt=FALSE){
  # At the file scale
  # Let's compute number of target test_segments vs number of non-target test_segments
  test_segments[c("target","non_target")] = 0
  test_segments[test_segments["duration_total_speech"] != 0,"target"] = 1
  test_segments[test_segments["duration_total_speech"] == 0,"non_target"] = 1
  nb_test_segments = aggregate(.~filename, test_segments, sum)[c("filename", "target", "non_target")]
  colnames(nb_test_segments) = c("filename", "number_target_test_segments", "number_non_target_test_segments")
  
  # The average duration of total speech and overlapping speech
  average_duration_tri = aggregate(.~filename, test_segments, mean)[c("filename", "duration_total_speech", "duration_overlapping_speech")]
  colnames(average_duration_tri) = c("filename", "average_duration_total_speech_test_segments", "average_duration_clean_speech_test_segments")
  
  # The average duration of speech in enrollment for the target speaker
  aggregated_enrollment = aggregate(duration~filename+model_number+speaker, enrollment, sum)
  avg_duration_enr = aggregate(duration~filename, aggregated_enrollment, mean)
  colnames(avg_duration_enr) = c("filename", "average_duration_speech_target_speaker_enrollment")
  
  # The number of enrollment
  nb_enr = aggregate(model_number~filename,aggregated_enrollment, length)
  colnames(nb_enr) = c("filename", "number_enrollments")
  
  scatter = merge(nb_test_segments, average_duration_tri, by="filename")
  scatter = merge(scatter, avg_duration_enr, by="filename")
  scatter = merge(scatter, nb_enr, by="filename")
  colnames(scatter) = c("filename", "#NTT", "#NNTT", "TSTT", "OSTT", "TSE", "#E")
  
  if(bbt){
    alpha=.1
  } else {
    alpha=.3
  }
  p1 <- splom(scatter[2:7], pch=20, col=alpha("black",alpha), pscales=0)
  return(p1)
}

get_trials <- function(folder_path, enr_dur, n_tri, sampling="natural"){
  trials = read.table(file.path(folder_path, paste0("trials_", sampling, "_enr_", enr_dur, "_test_60_N_", n_tri, ".txt")), header = TRUE, sep = "\t", dec = ".")
  return(trials)
}

get_nb_inter_sessions_mic <- function(trials, enrollments, corpora){
  if (corpora == "bbt") {
    sep = ""
  } else if (corpora == "ami") {
    sep = "\\."
  } else if (corpora == "chime5") {
    sep = "\\_"
  } else {
    print("Unknown corpora.")
    exit()
  }
  enrollments_agg <- enrollments[!duplicated(enrollments[c("speaker", "model_number")]),]
  merged = merge(trials, enrollments_agg, by.x=c("target_speaker", "enrollment_number"), by.y=c("speaker", "model_number"), all.x = TRUE, all.y=FALSE)
  merged["session.y"] = sapply(strsplit(as.character(merged$filename.y), sep), `[`, 1)
  merged["microphone.y"] =  sapply(strsplit(as.character(merged$filename.y), sep), `[`, 2)
  
  nb_inter_sessions = sum(merged["session"] != merged["session.y"])
  nb_inter_mic = sum(merged["microphone"] != merged["microphone.y"])
  res = data.frame("nb_inter-sessions" = nb_inter_sessions, "nb_inter-microphones" = nb_inter_mic)
  return (res)
  }

server <- function(input, output) {
  enrollment_duration_bbt = reactive({input$bbt_enr_dur})
  enrollment_duration_ami = reactive({input$ami_enr_dur})
  enrollment_duration_chime5 = reactive({input$chime5_enr_dur})
  
  n_tri_bbt_nat = reactive({input$bbt_n_tri_nat})
  n_tri_ami_nat = reactive({input$ami_n_tri_nat})
  n_tri_chime5_nat = reactive({input$chime5_n_tri_nat})
  
  n_tri_bbt_square = reactive({input$bbt_n_tri_square})
  n_tri_ami_square = reactive({input$ami_n_tri_square})
  n_tri_chime5_square = reactive({input$chime5_n_tri_square})

  # ami
  ami_path = "data/ami"
  ami_test_segments = reactive({
    get_test_segments(ami_path)
  })
  ami_enrollment = reactive({
    get_enrollment(ami_path, enrollment_duration_ami())
  })
  
  ami_trials_nat = reactive({
    get_trials(ami_path, enrollment_duration_ami(), n_tri_ami_nat(), sampling="natural")
  })
  ami_trials_square = reactive({
    get_trials(ami_path, enrollment_duration_ami(), n_tri_ami_square(), sampling="square")
  })
  
  enrollment_plots = reactive({
    plot_enrollment(ami_enrollment())
  })
  test_segments_plots = reactive({
    plot_test_segments(ami_test_segments())
  })
  
  gender_ami_nat <- reactive({
    get_gender_percentage(ami_trials_nat())
  })
  gender_ami_square <- reactive({
    get_gender_percentage(ami_trials_square())
  })
  inter_ses_mic_ami_nat <- reactive({
    get_nb_inter_sessions_mic(ami_trials_nat(), ami_enrollment(), corpora="ami")
  })
  inter_ses_mic_ami_square <- reactive({
    get_nb_inter_sessions_mic(ami_trials_square(), ami_enrollment(), corpora="ami")
  })
  
  output$enrollment1_ami = renderPlot({enrollment_plots()$p1})
  output$enrollment2_ami = renderPlot({enrollment_plots()$p2})
  
  output$nb_non_target_test_segments_ami <- renderText({paste("Number of non-target test segments : " , nrow(ami_test_segments()[ami_test_segments()["duration_total_speech"] == 0,]))})
  output$nb_target_test_segments_ami <- renderText({paste("Number of target test segments : " , nrow(ami_test_segments()[ami_test_segments()["duration_total_speech"] != 0,]))})
  
  output$test_segments1_ami <- renderPlot({test_segments_plots()$p1})
  output$test_segments2_ami <- renderPlot({test_segments_plots()$p2})
  output$test_segments3_ami <- renderPlot({test_segments_plots()$p3})
  output$test_segments4_ami <- renderPlot({test_segments_plots()$p4})
  output$test_segments5_ami <- renderPlot({test_segments_plots()$p5})
  output$test_segments6_ami <- renderPlot({test_segments_plots()$p6})
  output$test_segments7_ami <- renderPlot({test_segments_plots()$p7})

  output$nb_non_target_trials_ami_nat <- renderText({paste("Number of target trials : " , if (dim(ami_trials_nat())[1] != 0) nrow(ami_trials_nat()[ami_trials_nat()["duration_total_speech"] != 0,]) else 0)})
  output$nb_target_trials_ami_nat <- renderText({paste("Number of non-target trials : " , if (dim(ami_trials_nat())[1] != 0) nrow(ami_trials_nat()[ami_trials_nat()["duration_total_speech"] == 0,]) else 0)})
  output$nb_speakers_ami_nat <- renderText({paste("Number of speakers : " , dim(unique(ami_trials_nat()["target_speaker"]))[1])})
  output$gender_ami_nat <- renderTable(gender_ami_nat())
  output$inter_ses_mic_ami_nat <- renderTable(inter_ses_mic_ami_nat())
  
  output$nb_non_target_trials_ami_square <- renderText({paste("Number of target trials : " , nrow(ami_trials_square()[ami_trials_square()["duration_total_speech"] != 0,]))})
  output$nb_target_trials_ami_square <- renderText({paste("Number of non-target trials : " , nrow(ami_trials_square()[ami_trials_square()["duration_total_speech"] == 0,]))})
  output$nb_speakers_ami_square <- renderText({paste("Number of speakers : " , dim(unique(ami_trials_square()["target_speaker"]))[1])})
  output$gender_ami_square <- renderTable(gender_ami_square())
  output$inter_ses_mic_ami_square <- renderTable(inter_ses_mic_ami_square())
  
  ami_trials_plots_nat = reactive({
    plot_trials(ami_trials_nat())
  })
  output$ami_trial_plots_nat1 <- renderPlot({ami_trials_plots_nat()$p1})
  output$ami_trial_plots_nat2 <- renderPlot({ami_trials_plots_nat()$p2})
  
  ami_trials_plots_square = reactive({
    plot_trials(ami_trials_square())
  })
  output$ami_trial_plots_square1 <- renderPlot({ami_trials_plots_square()$p1})
  output$ami_trial_plots_square2 <- renderPlot({ami_trials_plots_square()$p2})
  
  output$scatter_ami <- renderPlot({plot_scatter(ami_test_segments(), ami_enrollment())})
  
  # CHiME5
  chime5_path = "data/chime5"
  chime5_test_segments = reactive({
    get_test_segments(chime5_path)
  })
  chime5_enrollment = reactive({
    get_enrollment(chime5_path, enrollment_duration_chime5())
  })
  chime5_trials_nat = reactive({
    get_trials(chime5_path, enrollment_duration_chime5(), n_tri_chime5_nat(), sampling="natural")
  })
  chime5_trials_square = reactive({
    get_trials(chime5_path, enrollment_duration_chime5(), n_tri_chime5_square(), sampling="square")
  })
  
  chime5_enrollment_plots = reactive({
    plot_enrollment(chime5_enrollment())
  })
  chime5_test_segments_plots = reactive({
    plot_test_segments(chime5_test_segments())
  })
  
  inter_ses_mic_chime5_nat <- reactive({
    get_nb_inter_sessions_mic(chime5_trials_nat(), chime5_enrollment(), corpora="chime5")
  })
  inter_ses_mic_chime5_square <- reactive({
    get_nb_inter_sessions_mic(chime5_trials_square(), chime5_enrollment(), corpora="chime5")
  })
  
  
  output$enrollment1_chime5 = renderPlot({chime5_enrollment_plots()$p1})
  output$enrollment2_chime5 = renderPlot({chime5_enrollment_plots()$p2})
  
  output$nb_non_target_test_segments_chime5 <- renderText({paste("Number of non-target test segments: " , nrow(chime5_test_segments()[chime5_test_segments()["duration_total_speech"] == 0,]))})
  output$nb_target_test_segments_chime5 <- renderText({paste("Number of target test segments : " , nrow(chime5_test_segments()[chime5_test_segments()["duration_total_speech"] != 0,]))})
  
  output$test_segments1_chime5 <- renderPlot({chime5_test_segments_plots()$p1})
  output$test_segments2_chime5 <- renderPlot({chime5_test_segments_plots()$p2})
  output$test_segments3_chime5 <- renderPlot({chime5_test_segments_plots()$p3})
  output$test_segments4_chime5 <- renderPlot({chime5_test_segments_plots()$p4})
  output$test_segments5_chime5 <- renderPlot({chime5_test_segments_plots()$p5})
  output$test_segments6_chime5 <- renderPlot({chime5_test_segments_plots()$p6})
  output$test_segments7_chime5 <- renderPlot({chime5_test_segments_plots()$p7})
  
  output$nb_non_target_trials_chime5_nat <- renderText({paste("Number of target trials : " , nrow(chime5_trials_nat()[chime5_trials_nat()["duration_total_speech"] != 0,]))})
  output$nb_target_trials_chime5_nat <- renderText({paste("Number of non-target trials : " , nrow(chime5_trials_nat()[chime5_trials_nat()["duration_total_speech"] == 0,]))})
  output$nb_speakers_chime5_nat <- renderText({paste("Number of speakers : " , dim(unique(chime5_trials_nat()["target_speaker"]))[1])})
  output$inter_ses_mic_chime5_nat <- renderTable(inter_ses_mic_chime5_nat())
  
  output$nb_non_target_trials_chime5_square <- renderText({paste("Number of target trials : " , nrow(chime5_trials_square()[chime5_trials_square()["duration_total_speech"] != 0,]))})
  output$nb_target_trials_chime5_square <- renderText({paste("Number of non-target trials : " , nrow(chime5_trials_square()[chime5_trials_square()["duration_total_speech"] == 0,]))})
  output$nb_speakers_chime5_square <- renderText({paste("Number of speakers : " , dim(unique(chime5_trials_square()["target_speaker"]))[1])})
  output$inter_ses_mic_chime5_square <- renderTable(inter_ses_mic_chime5_square())
  
  chime5_trials_plots_nat = reactive({
    plot_trials(chime5_trials_nat())
  })
  output$chime5_trial_plots_nat1 <- renderPlot({chime5_trials_plots_nat()$p1})
  output$chime5_trial_plots_nat2 <- renderPlot({chime5_trials_plots_nat()$p2})
  
  chime5_trials_plots_square = reactive({
    plot_trials(chime5_trials_square())
  })
  output$chime5_trial_plots_square1 <- renderPlot({chime5_trials_plots_square()$p1})
  output$chime5_trial_plots_square2 <- renderPlot({chime5_trials_plots_square()$p2})
  
  output$scatter_chime5 <- renderPlot({plot_scatter(chime5_test_segments(), chime5_enrollment())})
  
  # bbt
  bbt_path = "data/babytrain"
  bbt_test_segments = reactive({
    get_test_segments(bbt_path)
  })
  bbt_enrollment = reactive({
    get_enrollment(bbt_path, enrollment_duration_bbt())
  })
  bbt_trials_nat = reactive({
    get_trials(bbt_path, enrollment_duration_bbt(), n_tri_bbt_nat(), sampling="natural")
  })
  bbt_trials_square = reactive({
    get_trials(bbt_path, enrollment_duration_bbt(), n_tri_bbt_square(), sampling="square")
  })
  
  bbt_enrollment_plots = reactive({
    plot_enrollment(bbt_enrollment())
  })
  bbt_test_segments_plots = reactive({
    plot_test_segments(bbt_test_segments())
  })

  gender_bbt_nat <- reactive({
    get_gender_percentage(bbt_trials_nat(), bbt=TRUE)
  })
  gender_bbt_square <- reactive({
    get_gender_percentage(bbt_trials_square(), bbt=TRUE)
  })
  
  inter_ses_mic_bbt_nat <- reactive({
    get_nb_inter_sessions_mic(bbt_trials_nat(), bbt_enrollment(), corpora="bbt")
  })
  inter_ses_mic_bbt_square <- reactive({
    get_nb_inter_sessions_mic(bbt_trials_square(), bbt_enrollment(), corpora="bbt")
  })
  
  output$enrollment1_bbt = renderPlot({bbt_enrollment_plots()$p1})
  output$enrollment2_bbt = renderPlot({bbt_enrollment_plots()$p2})
  
  output$nb_non_target_test_segments_bbt <- renderText({paste("Number of non-target test segments : " , nrow(bbt_test_segments()[bbt_test_segments()["duration_total_speech"] == 0,]))})
  output$nb_target_test_segments_bbt <- renderText({paste("Number of target test segments : " , nrow(bbt_test_segments()[bbt_test_segments()["duration_total_speech"] != 0,]))})
  
  output$test_segments1_bbt <- renderPlot({bbt_test_segments_plots()$p1})
  output$test_segments2_bbt <- renderPlot({bbt_test_segments_plots()$p2})
  output$test_segments3_bbt <- renderPlot({bbt_test_segments_plots()$p3})
  output$test_segments4_bbt <- renderPlot({bbt_test_segments_plots()$p4})
  output$test_segments5_bbt <- renderPlot({bbt_test_segments_plots()$p5})
  output$test_segments6_bbt <- renderPlot({bbt_test_segments_plots()$p6})
  output$test_segments7_bbt <- renderPlot({bbt_test_segments_plots()$p7})
  
  output$nb_non_target_trials_bbt_nat <- renderText({paste("Number of target trials : " , nrow(bbt_trials_nat()[bbt_trials_nat()["duration_total_speech"] != 0,]))})
  output$nb_target_trials_bbt_nat <- renderText({paste("Number of non-target trials : " , nrow(bbt_trials_nat()[bbt_trials_nat()["duration_total_speech"] == 0,]))})
  output$nb_speakers_bbt_nat <- renderText({paste("Number of speakers : " , dim(unique(bbt_trials_nat()["target_speaker"]))[1])})
  output$gender_bbt_nat <- renderTable(gender_bbt_nat())
  output$inter_ses_mic_bbt_nat <- renderTable(inter_ses_mic_bbt_nat())
  
  output$nb_non_target_trials_bbt_square <- renderText({paste("Number of target trials : " , nrow(bbt_trials_square()[bbt_trials_square()["duration_total_speech"] != 0,]))})
  output$nb_target_trials_bbt_square <- renderText({paste("Number of non-target trials : " , nrow(bbt_trials_square()[bbt_trials_square()["duration_total_speech"] == 0,]))})
  output$nb_speakers_bbt_square <- renderText({paste("Number of speakers : " , dim(unique(bbt_trials_square()["target_speaker"]))[1])})
  output$gender_bbt_square <- renderTable(gender_bbt_square())
  output$inter_ses_mic_bbt_square <- renderTable(inter_ses_mic_bbt_square())
  
  bbt_trials_plots_nat = reactive({
    plot_trials(bbt_trials_nat())
  })
  output$bbt_trial_plots_nat1 <- renderPlot({bbt_trials_plots_nat()$p1})
  output$bbt_trial_plots_nat2 <- renderPlot({bbt_trials_plots_nat()$p2})
  
  bbt_trials_plots_square = reactive({
    plot_trials(bbt_trials_square())
  })
  output$bbt_trial_plots_square1 <- renderPlot({bbt_trials_plots_square()$p1})
  output$bbt_trial_plots_square2 <- renderPlot({bbt_trials_plots_square()$p2})
  
  output$scatter_bbt <- renderPlot({plot_scatter(bbt_test_segments(), bbt_enrollment(), bbt=TRUE)})
}

shinyApp(ui, server)

