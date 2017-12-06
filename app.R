check_packages = function(names)
{
    for(name in names)
    {
        if (!(name %in% installed.packages()))
            install.packages(name, repos="http://cran.us.r-project.org")
        
        library(name, character.only=TRUE)
    }
}

check_packages(c("shiny","XML","RCurl","reshape","plyr","dplyr","broom","ggplot2"))   # Checks to see if required packages are already installed.

######################################################################################
######################################################################################
######################################################################################
# First function to calculate BMI and Diagnose
health.analysis <- function(height,weight) {    # Input height (in) and weight (lb) into function

    BMI <- weight / height^2 * 703      # Converts height (in) and weight (lb) into BMI
    
    # Classifies Diagnosis based on BMI 
    if (BMI < 18.5) {
        Diagnosis <- "Underweight"
    } else if (BMI >= 18.5 && BMI < 25) {
        Diagnosis <- "Normal Weight"
    } else if (BMI >= 25 && BMI < 30) {
        Diagnosis <- "Overweight"
    } else if (BMI >= 30) {
        Diagnosis <- "Obese"
    }
    
    output<-list(BMI=BMI,Diagnosis=Diagnosis)       # Creates a list so function can return two variables
    return(output)      # Function outputs variable BMI and Diagnosis
}

######################################################################################
######################################################################################
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
    arrange(Activity)

mods <- dlply(data_table.2, .(Activity), lm, formula=as.numeric(as.character(value))~as.numeric(variable)-1)
coefs = ldply(mods, coef)

reg_table= inner_join(data_table.1, coefs, by="Activity")
names(reg_table)[6]<-"coef"

######################################################################################
######################################################################################
######################################################################################
### Building Shiny App ###
# Adding input slider bars
ui <- fluidPage(
    titlePanel("Healthier U - Weight Loss Program"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("height", "Current Height (in)", min = 50, max = 80,value = 65),
            sliderInput("weights", "Desired Weight & Current Weight (lbs)", min = 50, max = 300,value = c(100,154)),
            sliderInput("target.date", "Number of Weeks To Achieve Desired Weight", min = 1, max = 100,value = 50),
            sliderInput("intensity", "Number of Hours Devoted to Exercising Every Week", min = 1, max = 20,value = 5)
        ),
        mainPanel(
            plotOutput("weight_distribution"),
            br(), br(),
            tableOutput("exercises")
        )
    )
)
# Adding graphs and output table
server <- function(input, output) {
    output$weight_distribution <- renderPlot({
        height<-input$height
        weight<-input$weights[2]
        
        bmi <- health.analysis(height,weight)$BMI
        diagnosis <- health.analysis(height,weight)$Diagnosis
        percent <- round(pnorm(bmi,25.6,4) * 100, 2)
        
        ggplot(data.frame(x=c(10.6,40.6)), aes(x)) +    # Creates plot from x = 10.6 to 40.6
            # Plots a normal curve with mean = 25.6, sd = 4. Colors area blue.
            stat_function(fun=dnorm, args=list(25.6,4), color = "dodgerblue", size = 2, geom="area", fill="cadetblue1", alpha = 0.4) +     
            scale_x_continuous(name="BMI") +        # Labels x axis "BMI"
            ggtitle("Percent of United States Population Less Than Given BMI") +   # Adds a graph title
            theme_bw() +        # Makes the background white theme
            # Shades only below given BMI dark blue
            stat_function(fun=dnorm, args=list(25.6,4), xlim=c(10.6,bmi), geom="area", fill="cadetblue3", alpha = 0.7) +   
            
            geom_vline(xintercept=c(18.5,25,30)) +      # Adds black vertical lines in desired locations
            geom_label(aes(35.5,0.085,label=paste0("Your BMI:  ", round(bmi,digits=1), "\n", "Diagnosis:  ", diagnosis)), size=8)  +    # prints rounded BMI to 1 decimal and Diagnosis on top right 
            geom_label(aes(35.5,0.067, label=paste0("Note: BMI may be a misinformative measure", "\n", " of health as it doesn't take into account", "\n", " for muscle mass or body shape.")), color="red", size=4.5) +
            geom_text(aes(25.6,0.02,label=paste0(percent,"%")), size=10)  +     # Adds % label in middle of graph
            geom_text(aes(25.6,0.0001, label=paste0("|", "\n", "US Average")), color="green") +  # Adds average tick mark
            geom_text(aes(15,0.1, label="Underweight")) +
            geom_text(aes(22,0.1, label="Normal Weight")) +
            geom_text(aes(27.5,0.1, label="Overweight")) +
            geom_text(aes(32,0.1, label="Obese")) +
            theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())    # Removes y axis information
        
    })
####################################################################################
### Creating Table of Exercises ###
    output$exercises <- renderTable({
        weight<-input$weights[2]
        target.weight<-input$weights[1]
        target.date<-input$target.date
        intensity<-input$intensity

        if (target.weight > weight) {       # Checks to see if target weight is less than current weight
            print("This is a weight loss program")
        }
        
        cal.per.week <- -((target.weight - weight) / target.date * 3500)   # How many calories should be lost per week on average
        # Carol: flipped sign to positive. Changed 12 to target.date
        
        reg_table<- reg_table %>% group_by(Activity)%>%
            mutate(burn.rate=mean(c(target.weight, weight))*coef,
                   burn.calories=burn.rate*intensity) 
        
        if (cal.per.week > 7000){       # Checks to see if weight loss plan is too extreme
            print("Losing more than 2 lbs/week may be considered unrealistic and unsafe")
        }else{ 
            summary_table=reg_table %>% filter((.9*cal.per.week)<=burn.calories & (1.1*cal.per.week)>=burn.calories) #find activities that are within 10% of cal.per.week
        }
        
        if (nrow(summary_table)==0){      # Checks to see if any exercises are available
            print("No exercises match your criteria. Please consider increasing intensity or target date to achieve desired weight")
        } else{
            summary_table %>% select(Activity,burn.rate)
        }
    })
}
shinyApp(ui = ui, server = server)