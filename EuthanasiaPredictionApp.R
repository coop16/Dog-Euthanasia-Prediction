#App for predicting dog shelter probability of euthanasia

library(shiny)

#load prediction model 
library(glmnet)
loadfit<-load(file="FinalGlmnetModel.rda",.GlobalEnv)
mod<-finalglmnetmodel$finalmod

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Probability of Euthanasia"),
   
   # Sidebar with inputs for dog characteristics
   sidebarLayout(
      sidebarPanel(
        
         sliderInput(inputId = "age",
                     label = "Age (years):",
                     min = 0,
                     max = 20,
                     value = 3,
                     step = .5),
         
         selectInput(inputId = "breed",
                     label = "Primary Breed",
                     choices = c("Other",
                                 "Australian Cattle Dog",
                                 "Beagle",
                                 "Boxer",
                                 "Bull",
                                 "Catahoula",
                                 "Chihuahua",
                                 "Collie",
                                 "Dachshund",
                                 "Great Pyrenees",
                                 "Hound",
                                 "Husky",
                                 "Miniature Schnauzer",
                                 "Pointer",
                                 "Poodle",
                                 "Retriever",
                                 "Rottweiler",
                                 "Shepherd",
                                 "Shih Tzu",
                                 "Terrier" ),
                     selected = "Other"),
         
         checkboxInput(inputId = "mix",
                       label = "Mix",
                       value = FALSE),
         
         selectInput(inputId = "color",
                     label = "Color",
                     choices = c("Black",
                                 "Blue",
                                 "Brown",
                                 "Gray",
                                 "Tan",
                                 "Tricolor",
                                 "White"),
                     selected="Black"),
         
         selectInput(inputId = "sex",
                     label = "Sex",
                     choices = c("Intact Female",
                                 "Intact Male",
                                 "Neutered Male",
                                 "Spayed Female",
                                 "Unknown"),
                     selected="Intact Female"),
         
         dateInput(inputId = "date",label = "Date"),
         
         sliderInput(inputId = "temp",
                     label = "Temperature (degrees F):",
                     min = 0,
                     max = 100,
                     value = 65),
         
         sliderInput(inputId = "precip",
                     label = "Precipitation (inches):",
                     min = 0,
                     max = 3,
                     step = .1,
                     value = 0)
         
         
      ),
      
      # Show a plot of the predicted probability
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to plot the probability
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     #get month and day of week from date input
     month<-vapply(strsplit(as.character(as.Date(input$date,"%m/%d/%Y"))[1],"-"), `[`, 2, FUN.VALUE=character(1))
     dayofweek<-weekdays(as.Date(input$date,"%m/%d/%Y"))
     
     
     # take inputs and build a vector of predictors to input into the model
     newdat<-cbind(
       
         "(Intercept)"=1,
         
         "a_SexUponOutcomeIntact Male"=ifelse(input$sex=="Intact Male",1,0),
         "a_SexUponOutcomeNeutered Male"=ifelse(input$sex=="Neutered Male",1,0),
         "a_SexUponOutcomeSpayed Female"=ifelse(input$sex=="Spayed Female",1,0),
         "a_SexUponOutcomeUnknown"=ifelse(input$sex=="Unknown",1,0),
         
         ColorBlue=ifelse(input$color=="Blue",1,0),
         ColorBrown=ifelse(input$color=="Brown",1,0),
         ColorGray=ifelse(input$color=="Gray",1,0),
         ColorTan =ifelse(input$color=="Tan",1,0),
         ColorTricolor =ifelse(input$color=="Tricolor",1,0),
         ColorWhite =ifelse(input$color=="White",1,0),
         
         "mixNot Mix"=ifelse(input$mix=="Not Mix",1,0),
         
         primarybreedBeagle=ifelse(input$breed=="Beagle",1,0),
         primarybreedBoxer =ifelse(input$breed=="Boxer",1,0),
         primarybreedBull =ifelse(input$breed=="Bull",1,0),
         primarybreedCatahoula=ifelse(input$breed=="Catahoula",1,0),
         primarybreedChihuahua =ifelse(input$breed=="Chihuahua",1,0),
         primarybreedCollie =ifelse(input$breed=="Collie",1,0),
         primarybreedDachshund =ifelse(input$breed=="Dachshund",1,0),
         "primarybreedGreat Pyrenees" =ifelse(input$breed=="Great Pyrenees",1,0),
         primarybreedHound =ifelse(input$breed=="Hound",1,0),
         primarybreedHusky=ifelse(input$breed=="Husky",1,0),
         "primarybreedMiniature Schnauzer" =ifelse(input$breed=="Miniature Schnauzer",1,0),
         primarybreedOther =ifelse(input$breed=="Other",1,0),
         primarybreedPointer =ifelse(input$breed=="Pointer",1,0),
         primarybreedPoodle =ifelse(input$breed=="Poodle",1,0),
         primarybreedRetriever =ifelse(input$breed=="Retriever",1,0),
         primarybreedRottweiler=ifelse(input$breed=="Rottweiler",1,0),
         primarybreedShepherd =ifelse(input$breed=="Shepherd",1,0),
         "primarybreedShih Tzu" =ifelse(input$breed=="Shih Tzu",1,0),
         primarybreedTerrier =ifelse(input$breed=="Terrier",1,0),
         
         month02 =ifelse(month=="02",1,0),
         month03 =ifelse(month=="03",1,0),
         month04 =ifelse(month=="04",1,0),
         month05 =ifelse(month=="05",1,0),
         month06 =ifelse(month=="06",1,0),
         month07 =ifelse(month=="07",1,0),
         month08 =ifelse(month=="08",1,0),
         month09 =ifelse(month=="09",1,0),
         month10 =ifelse(month=="10",1,0),
         month11 =ifelse(month=="11",1,0),
         month12 =ifelse(month=="12",1,0),
         
         dayofweekMonday =ifelse(dayofweek=="Monday",1,0),
         dayofweekSaturday =ifelse(dayofweek=="Saturday",1,0),
         dayofweekSunday =ifelse(dayofweek=="Sunday",1,0),
         dayofweekThursday =ifelse(dayofweek=="Thursday",1,0),
         dayofweekTuesday =ifelse(dayofweek=="Tuesday",1,0),
         dayofweekWednesday =ifelse(dayofweek=="Wednesday",1,0),      
         
         age=input$age,
         
         temperature.scaled=(input$temp-23.7 )/90.2,
         
         precip.scaled=input$precip/5.46
       )
     
  
       
     #probability of euthanasia
     prob<-predict(mod, s=mod$lambdaOpt, newdat, type="response")
   
   
      #make color green if 0-.333, orange if .334-.667, red if .667-1
      col<-ifelse(prob>.333,"orange","green")
      col<-ifelse(prob>.667,"red",col)
      
      # Barplot of probability
      barplot(prob,ylim=c(0,1),col=col,main="Probability of Euthanasia",xaxt='n',xlab=paste("Probability=",round(prob,digits=2)))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

