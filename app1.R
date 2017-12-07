check_packages = function(names)
{
    for(name in names)
    {
        if (!(name %in% installed.packages()))
            install.packages(name, repos="http://cran.us.r-project.org")
        
        library(name, character.only=TRUE)
    }
}
# Checks to see if required packages are already installed.
check_packages(c("shiny","XML","RCurl","reshape","plyr","dplyr","broom","ggplot2"))  

######################################################################################
######################################################################################
######################################################################################

# First function to calculate BMI and Diagnosis (Used for second function to graph BMI).
bmi.diagnosis<-function(bmi){
  if (bmi < 18.5) {
    Diagnosis <- "Underweight"
  } else if (bmi >= 18.5 && bmi < 25) {
    Diagnosis <- "Normal Weight"
  } else if (bmi >= 25 && bmi < 30) {
    Diagnosis <- "Overweight"
  } else if (bmi >= 30) {
    Diagnosis <- "Obese"
  }
  return(Diagnosis)
}
health.analysis <- function(height,weight, target.weight, unit=input$metric_sys) {    # Input height (in) and weight (lb) into function
    
    if (unit=="Standard"){
    BMI <- weight / height^2 * 703              # Converts height (in) and weight (lb) into BMI
    target.BMI <- target.weight / height^2 * 703       # Converts height (in) and target weight (lb) into BMI
    } else if (unit=="Metric"){
    BMI <- (weight*2.205) / (height*0.3937)^2 * 703              # Converts height (in) and weight (lb) into BMI
    target.BMI <- target.weight*2.205 / (height*0.3937)^2 * 703       # Converts height (in) and target weight (lb) into BMI
    }
    
    # Classifies Diagnosis based on BMI 
    Diagnosis=bmi.diagnosis(BMI)
    target.diagnosis=bmi.diagnosis(target.BMI)
    
    output<-list(BMI=BMI,Diagnosis=Diagnosis, Target.BMI=target.BMI, Target.Diagnosis=target.diagnosis)       # Creates a list so function can return two variables
    return(output)      # Function outputs variable BMI and Diagnosis
}

######################################################################################
####################### Webscraping and Tidying Data #################################
######################################################################################

#Webscraping
#Save URL into a variable
url.1<-"http://www.nutristrategy.com/caloriesburned.htm"
#Use getURL to download the URL
urldata.1<-getURL(url.1)
#read the table from the website 
data_table.1<-readHTMLTable(urldata.1, which=1)
#print out data table
table.paste<-function(){
    old=data.frame(data_table.1[[1]][13:261])
    for (i in 2:5){
        old=cbind(old,data.frame(data_table.1[[i]][13:261]))
    }
    return(old)
}
data_table.1=table.paste()

lame_exercises<-c(3:4,7:9,11,13,22,24,26:28,33,35,39,41,43,45,48:56,60:64,66,68:74,81:82,84:85,89,93:100,104:105,108,116,118,120:121,124,126,128:129,134:136,138:139,141:169,171:178,184:194,197:204,208:210,214,216:249)

colnames(data_table.1)<-c("Activity","p130","p155","p180","p205")

data_table.1<-data_table.1[-lame_exercises, ]

data_table.2<-melt(data_table.1, id="Activity")
data_table.2<-data_table.2 %>% 
    mutate_at("variable", funs(as.numeric(substr(., 2, 4)))) %>%
    mutate(metric=variable*.454)%>%
    arrange(Activity)

mods.standard <- dlply(data_table.2, .(Activity), lm, formula=as.numeric(as.character(value))~as.numeric(variable)-1)
mods.metric <- dlply(data_table.2, .(Activity), lm, formula=as.numeric(as.character(value))~as.numeric(metric)-1)
coefs.standard = ldply(mods.standard, coef)
coefs.metric= ldply(mods.metric, coef)

reg_table= inner_join(data_table.1, coefs.standard) %>% inner_join(.,coefs.metric, by="Activity")
names(reg_table)[6:7]<-c("standard", "metric")


######################################################################################
######################### Building Shiny App #########################################
######################################################################################

### Creating Input section of Shiny App ###
ui <- fluidPage(
    titlePanel("Healthier U - Weight Loss Program"),    # Adds Title
    sidebarLayout(
        sidebarPanel(
            # Adding input slider bars reference name, label, min/max values, and starting value respectively
            radioButtons("gender", "Gender", c("Male","Female")),
            radioButtons("metric_sys", "Units",choices = c("Metric","Standard"), selected = "Standard"),
            wellPanel(uiOutput("ui_height")),
            wellPanel(uiOutput("ui_weight")),
            sliderInput("target.date", "Maximum Number of Weeks To Achieve Desired Weight", min = 1, max = 100,value = 50),
            sliderInput("intensity", "Number of Hours Devoted to Exercising Every Week", min = 1, max = 20,value = 5)
        ),
        
        mainPanel(
            wellPanel(plotOutput("weight_distribution"), inline=T),
            br(), br(),  "Your BMI fits the criteria to participate in our weight loss program!",  # Adds two spaces for separation
            wellPanel(tableOutput("exercises"),inline=T)
            #tableOutput("exercises")             # Name of table to be referenced below
        )
    )
)

### Creating Output section of Shiny App ###
server <- function(input, output) {
    
    output$ui_height <- renderUI({
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$metric_sys,
               "Metric" = sliderInput("height", "Current Height (cm)", min = 140, max = 215, value = 178),
               "Standard" = sliderInput("height", "Current Height (in)", min = 55, max = 85, value = 70)
        )
    })
    
    output$ui_weight <- renderUI({
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$metric_sys,
               "Metric" = sliderInput("weights", "Desired Weight & Current Weight (kg)", min = 40, max = 160,value = c(80,90)),
               "Standard" = sliderInput("weights", "Desired Weight & Current Weight (lbs)", min = 85, max = 350,value = c(180,200))
        )
    })
    
    
    # Adds graph for bmi distribution
    output$weight_distribution <- renderPlot({
        gender <- input$gender
        height <- input$height      # Converts Shiny App user input for height slidebar into one variable
        weight <- input$weights[2]  # Converts Shiny App user input for current weight slidebar into one variable
        target.weight<-input$weights[1] # Converts Shiny App user input for target weight slidebar into one variable
        units<-input$metric_sys
        bmi <- health.analysis(height,weight, target.weight,units)$BMI               # Gets BMI from previously specified function
        diagnosis <- health.analysis(height,weight, target.weight,units)$Diagnosis   # Gets BMI diagnosis from previously specified function
        target.bmi <- health.analysis(height,weight, target.weight,units)$Target.BMI  # Converts height (in) and weight (lb) into target BMI
        target.diagnosis <- health.analysis(height,weight, target.weight,units)$Target.Diagnosis   # Gets BMI diagnosis from previously specified function
        
        if (gender == "Male") {
            mean <- 28.7
            sd <- sqrt(5223)*0.13
            fill1 <- "cadetblue1"
            fill2 <- "cadetblue3"
            color1 <- "cadetblue4"
            y <- 0
        } else if (gender =="Female"){
            mean <- 29.2
            sd <- sqrt(5413)*0.17
            fill1 <- "lightpink1"
            fill2 <- "lightpink3"
            color1 <- "lightpink4"
            y <- 0.01
        }
        
        percent <- round(pnorm(bmi,mean,sd) * 100, 2)            # Rounds bmi to 2 decimal places just for the graph
        
        # Creates cumulative normal distribution graph shading in given BMI
        ggplot(data.frame(x=c(10,48)), aes(x)) +    # Creates plot from x = 10.6 to 40.6
            # Plots a normal curve with mean = 25.6, sd = 4. Colors area below light blue.
            stat_function(fun=dnorm, args=list(mean,sd), color = color1, size = 2, geom="area", fill=fill1, alpha = 0.4) +     
            scale_x_continuous(name="BMI") +            # Labels x axis "BMI"
            ggtitle(paste0("BMI Distribution In The United States For ",gender, " Adult")) +   # Adds a graph title
            theme(plot.title = element_text(size = 30, face = "bold", hjust=.25)) +
            theme_classic() +                           # Makes the background white theme
            # Shades the normal curve to the left of given BMI dark blue
            stat_function(fun=dnorm, args=list(mean,sd), xlim=c(target.bmi,bmi), geom="area", fill=fill2, alpha = 0.7) +
             
            
            # Creates lines and text on graph
            geom_vline(xintercept=c(18.5,25,30)) +              # Adds black vertical lines in desired x locations
            geom_label(aes(43,0.045-y,label=paste0("Your BMI:  ", round(bmi,digits=1), "\n", "Diagnosis:  ", diagnosis)), size=5)  +    # prints rounded BMI to 1 decimal and Diagnosis on top right 
            geom_label(aes(43,0.035-y,label=paste0("Your Target BMI:  ", round(target.bmi,digits=1), "\n", "Diagnosis:  ", target.diagnosis)), size=5)  +
            geom_label(aes(43,0.025-y, label=paste0("Note: BMI may be a misinformative measure", "\n", " of health as it doesn't take into account", "\n", " for muscle mass or body shape.")), color="red", size=4) +
            geom_text(aes(mean,0.02,label=paste0(percent,"%")), size=10)  +     # Adds % label in middle of graph
            geom_text(aes(mean,0.0001, label=paste0("|", "\n", "U.S. Average")), color=color1) +  # Adds average tick mark
            geom_text(aes(15,0.05-y, label="Underweight")) +       # Adds Underweight text in top corresponding region
            geom_text(aes(22,0.05-y, label="Normal Weight")) +     # Adds Normal Weight text in top corresponding region
            geom_text(aes(27.5,0.05-y, label="Overweight")) +      # Adds Overweight text in top corresponding region
            geom_text(aes(32,0.05-y, label="Obese"))              # Adds Obese text in top corresponding region
            #theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())    # Removes y axis information
        
    })
    
#######################################################################################
    
### Creating Table of Exercises ###
    output$exercises <- renderTable({
        units<-input$metric_sys
        height <- input$height            # Converts Shiny App user input for height slidebar into one variable
        weight<-input$weights[2]          # Converts Shiny App user input for current weight slidebar into one variable
        target.weight<-input$weights[1]   # Converts Shiny App user input for desired weight slidebar into one variable
        bmi <- health.analysis(height,weight, target.weight,units)$BMI               # Gets BMI from previously specified function
       
        target.bmi <- health.analysis(height,weight, target.weight,units)$Target.BMI            # Converts height (in) and weight (lb) into target BMI
        target.date<-input$target.date    # Converts Shiny App user input for desired date slidebar into one variable
        intensity<-input$intensity        # Converts Shiny App user input for intensity slidebar into one variable
        
        if (units == "Standard"){
        cal.per.week <- -((target.weight - weight) / target.date * 3500)   # How many calories should be lost per week on average
        reg_table<- reg_table %>% group_by(Activity)%>%
          mutate(burn.rate=mean(c(target.weight, weight))*standard,
                 burn.calories=burn.rate*intensity) 
        } else if (units =="Metric"){
        cal.per.week <- -((target.weight - weight) / target.date * 3500/ 0.453592)   # How many calories should be lost per week on average
        reg_table<- reg_table %>% group_by(Activity)%>%
          mutate(burn.rate=mean(c(target.weight, weight))*metric,
                 burn.calories=burn.rate*intensity) 
        }
        
        
            summary_table=reg_table %>% filter((.9*cal.per.week)<=burn.calories) 
            #& (1.1*cal.per.week)>=burn.calories) #find activities that are within 10% of cal.per.week
        
    
        if (target.bmi < 17) {                 # Checks to see if target weight is too low
            if (bmi > 17){          
            print("Desired weight is very underweight and may be unhealthy. Please consider a different weight.")
            } else{                            # Checks to see if current weight needs a weight loss program
            print("You are already very underweight and may not need a weight loss program.")
            }
        } else if (cal.per.week > 7000){       # Checks to see if weight loss plan is too extreme
            print("Losing more than 2 lbs/week may be considered unrealistic and unsafe.")
        } else if (nrow(summary_table)==0){    # Checks to see if any exercises are available
            print("No exercises match your criteria. Please change intensity and/or target date.")
        } else{
            summary_table %>% select(Activity,burn.rate) %>% arrange(burn.rate) %>% rename("Calories Per Hour"=burn.rate)  # Prints all exercises that can burn that many calories per hour
        }
    })
}
shinyApp(ui = ui, server = server)
