#   App for elicitation of effect size

#### Packages

rm(list=ls())

library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
library(magick)
library(ggimage)
library(shinyalert)
library(png)
library(egg)

#####  Customizable Settings ####



nfiches.max<-12 # Number of chips that can be used (2 + the actual number of chips)


# Comparisons to be elicited (expressed in years between 6 and 19)
# The first value of 0 is for the trial exercise

age.sample<-data.frame(a=c(0,16,12,8,14),
                       b=c(0,14,8,12,16))



# Decide between Probability of Superiority ("CL") or Choen's U3 di Choen ("U3")

comparison.sample<-c("CL","U3")

 
# Only group 1 is used to identify if you want to compare Boys vs Girls or Girls vs Boys 

group1.sample<-c("Boys","Girls")
group2<- "Girls"           


date<-round(as.numeric(Sys.time()),0)  # The date is used as an identifier along with the participant nick-name

load("data.comparison.rda")  # I load my dataset with the indices U3 and CL (boys vs girls comparison

show.results<-"YES"   # Decide to show the results at the end "YES" or "NO"


path<-"⁩"    #Specify where to save the experiment results


#### Immagini: 

# The size of the images must be with this ratio:
# left and right edges of about 0.055 of the total length
# The size of each single person must be about 0.089 of the total length.

# In the present case the image is about 2020 pixels wide, 110 pixels at the edges and 180 for each person


######  ShinyApp  ####


##### UI  #####

ui <- navbarPage('Heights Elicitation',id = "App",
                 
                 
                 #####  Initial Questionnaire  ####
                 
                 tabPanel(title = "Questionnaire", value = "info",
                          
                          ### Title
                          
                          fluidRow(column(width = 8,offset = 2, 
                                          h1("Heights Elicitation", align = "center"))),
                          
                          br(),
                          
                          fluidRow(column(width = 8,offset = 2,align="center",
                                          h3("Questionnaire", align="center"),
                                          br(),
                                          br(),
                                          p("Before starting, you need to complete a short and easy 
questionnaire.", align="center"),
                                          p("Complete all the fields and click 'Next' to proceed.", align="center"),
                                          br())),
                          
                          fluidRow(column(width = 4, offset = 4,
                                          textInput("name", label=h4("Enter a nick-name:"), value=""),
                                          radioButtons("sex", label=h4("Gender:"),
                                                       choices = list("M" = 1, "F" = 2, "Other"=3, "I'd rather not answer"=4), 
                                                       selected=4),
                                          sliderInput("age", label = h4("How old are you?"), min = 17, max = 45, value = 17, step = 1),
                                          radioButtons("education", label=h4("Educational level:"),
                                                       choices = list("Enrolled in a bachelor's degree"=1,"Bachelor's degree"=2,
                                                                      "Master's degree"=3,"Post-graduate degree"=4,
                                                                      "I'd rather not answer"=5), 
                                                       selected=5),
                                          sliderInput("height", label = h4("How tall are you (cm)?"), min = 120, max = 220, value = 120, step = 1),
                                          radioButtons("q1", label=h4("Do you have brothers or sisters?"),
                                                       choices = list("No" = 1, "Yes - I have one brother / sister" = 2,
                                                                      "Yes - I have two brothers / sisters" = 3,"Yes - I have three (or more) brothers / sisters" = 4,
                                                                      "I'd rather not answer" = 5), 
                                                       selected=5),
                                          radioButtons("q2", label=h4("Have you ever worked as a volunteer, educator or in supporting activities with groups of young people (6-18 years)?"),
                                                       choices = list("Never" = 1, "For less than a month in a single year" = 2, "For more than a month in a single year" = 3,
                                                                      "For less than a month for several years" = 4,"For more than a month for several years" = 5,
                                                                      "I'd rather not answer"= 6), 
                                                       selected=6),
                                          radioButtons("privacy", label=h6("We guarantee to all our participants that their privacy will be respected
according to the EU General Data Protection Regulation (GDPR 679/2016). The collected data will be used exclusively for research purposes. I declare
to agree to participation in the present study:"),
                                                       choices = list("Accept" = 1, "Decline" = 2),
                                                       selected = 2)
                          )),
                          
                          
                          fluidRow(column(width = 8,offset = 2, align="right",
                                          actionButton('nextintro', 'Next')
                          )),
                          br(),
                          hr(),
                          br()
                          
                 ),
                 
                 #####  Introduction   ####
                 
                 tabPanel(title = "Introduction", value = "intro",
                          
                          ### Title
                          
                          fluidRow(column(width = 8,offset = 2, 
                                          h1("Heights Elicitation", align = "center"))),
                          
                          br(),
                          
                          fluidRow(column(width = 8,offset = 2, align="center",
                                          h4(tags$i(uiOutput("subtitle")))
                          )),
                          br(),
                          
                          ### Text
                          
                          fluidRow(column(width = 8,offset = 2,
                                          h3("1. Introduction"),
                                          br(),
                                          p("It looks like an easy question, but would you be able to evaluate
how the average heights ", textOutput("intro1", inline=TRUE), " change according to age?"),
                                          p("In the present exercise you will evaluate the
difference between ", textOutput("intro2", inline=TRUE), " different ages. However, to answer 
this question you will not refer to heights in meters or centimeters, but you will consider the probability of being taller in a
comparison."),
                                          p("It may seem odd and difficult, but now we will try to clarify the
                                            meaning: ",(htmlOutput("intro3"))),
                                          br(),
                                          
                                          h3("2. Instruction"),
                                          br(),
                                          p(textOutput("intro8", inline=TRUE), br(),"In particular, first you have to indicate
a minimum and a maximum number that will delimit the possible values; then, you have to bet 10 chips on these values 
according to which you think are the most-likely values."),
                                          p("Summarizing, you have to:"),
                                          br())),
                          
                          fluidRow(column(width = 6,offset = 3,align="center",
                                          htmlOutput("instruction")
                          )),
                          
                          fluidRow(column(width = 8, offset = 2,
                                          p("Remember that betting chips is a way to express how confident you are with
your answer. If you are really sure about your answer you can bet all the chips on a single value, whereas if you are 
really uncertain you can distribute them among the different values."),
                                          br()
                          )),
                          
                          fluidRow(column(width = 8,offset = 2,
                                          h3("3. Graphical interface"),
                                          br(),
                                          p("The following graphical interface will be used to collect your answers. ",
textOutput("intro4", inline = TRUE)),
                                          
                                          #### People image
                                          imageOutput("plot.img.intro", height = "auto"),
                                          
                                          #### Values text
                                          htmlOutput("index.intro", align="center",style = "font-size:24px"),
                                          
                                          #### Chips Plot
                                          
                                          plotOutput("plot1.intro", height = "120px",
                                                     hover = hoverOpts(id="plot_hover.intro", delay = 150, 
                                                                       delayType = "throttle", 
                                                                       nullOutside = FALSE)),
                                          
                                          imageOutput("intro.img", height = "auto"),
                                          br(),
                                          p("By moving the mouse", strong("within the grid,"), " it is possible to select different values. 
                                            It is important to emphasize that: "),
                                          htmlOutput("intro5"),
                                          br(),
                                          p("Great! Now you are ready to start. First you will do a trial to get familiar with the graphical 
interface and then you will start the real exercise. If you have any questions, please ask the assistant present in the room."),
                                          p("Click on the 'Next' button."),
                                          br()
                          )),                          
                          
                          fluidRow(column(2,offset=9,
                                          actionButton('nextpage', 'Next'))
                          ),
                          br(),
                          hr(),
                          br()
                 ),
                 
                 
                 
                 
                 
                 #####  Exercise page  ####
                 
                 tabPanel(title = "Exercise", value = "exs",
                          
                          
                          ### Allert message
                          
                          useShinyalert(),
                          
                          
                          ### Title
                          
                          fluidRow(column(width = 8,offset = 2,align = "center",
                                          h1("Heights Elicitation", align = "center"),
                                          br(),
                                          h3(textOutput("n_exp",inline = TRUE)),
                                          br()
                          )),
                          
                          ### Number of chips already played
                          
                          fluidRow(
                            column(width = 2, offset = 1,
                                   h4("Commands"),
                                   p(strong("Click") ,"- assigns one chip"),
                                   p(strong("Double Click"),"- remove one chip")),
                            
                            column(width= 6, align="center",
                                   htmlOutput("exercise1"),
                                   br(),
                                   htmlOutput("exercise2"),
                                   h2(textOutput("age")),
                                   uiOutput("next.exp")
                            ),
                            
                            column(width = 2,align = "right",
                                   br(),
                                   br(),
                                   p("Played chips:",strong(textOutput("fiches_index",inline = TRUE))),
                                   p("Remaining chips:",strong(textOutput("fiches_left",inline = TRUE)))
                            )
                          ),
                          
                          
                          
                          ### Graphic interface
                          
                          fluidRow(column(width = 10,offset = 1,align = "center",
                                          br(),
                                          
                                          #### People image
                                          imageOutput("plot.img", height = "auto"),
                                          
                                          #### Values text
                                          htmlOutput("index",style = "font-size:24px"),
                                          
                                          #### Chips Plot
                                          
                                          plotOutput("plot1", height = "200px",
                                                     click = clickOpts(id="plot_click"),
                                                     hover = hoverOpts(id="plot_hover", delay = 150, 
                                                                       delayType = "throttle", 
                                                                       nullOutside = FALSE),
                                                     dblclick = dblclickOpts(id="plot_dblclick"))
                          )),
                          br(),
                          hr(),
                          br(),
                          br()
                 ),
                 
                 
                 #####  Final panel ####
                 
                 tabPanel(title = "Finish", value = "end",
                          
                          ### Title
                          
                          fluidRow(column(width = 8,offset = 2,align = "center", 
                                          h1("Heights Elicitation"),
                                          br(),
                                          h3("You have finished!!!!"),
                                          br(),
                                          p("Thank you for your participation."),
                                          br()
                          )),
                          
                          fluidRow(column(width = 8,offset = 2,
                                          uiOutput("results"))),
                          br(),
                          hr(),
                          br(),
                          br()
                 )
                 
                 
)



#####   Server    #####


server <- function(input, output, session) {
  
  
  #####  General settings  ####
  
  ####  Tab settings ####
  
  hideTab(inputId = "App", target = "intro")
  hideTab(inputId = "App", target = "exs")
  hideTab(inputId = "App", target = "end")
  
  ####  Randomization  #####
  
  age<-age.sample[,sample(1:2,1)]
  
  comparison<-sample(comparison.sample,1)
  
  group1<- sample(group1.sample,1)
  
  ####  Load the images  ####
  
  coin <- readPNG(source="Coin Icon.png")
  triangle <- readPNG(source="Triangolo.png")
  
  ##
  
  if(comparison=="CL"){
    
    if(group1=="Boys"){
      img.back <- image_read("donne.jpg")
      img.front <- image_read("uomini.jpg")
    } else if (group1=="Girls"){
      img.back <- image_read("uomini.jpg")
      img.front <- image_read("donne.jpg")
    }
    
  } else if(comparison=="U3"){
    
    if(group1=="Boys"){
      img.back <- image_read("U3.uomini.jpg")
      img.front <- image_read("uomini.jpg")
    } else if (group1=="Girls"){
      img.back <- image_read("U3.donne.jpg")
      img.front <- image_read("donne.jpg")
    }
    
  }
  
  
  ####  Reactive Values ####
  
  vals<-reactiveValues(df=data.frame(x=vector(mode ="numeric",length = 0),
                                     y=vector(mode ="numeric", length = 0)))
  
  nfiches<-reactiveValues(n=0)
  
  intervalli<-reactiveValues(intervalli=list(df=data.frame(
    x=vector(mode = "numeric"),y=vector(mode = "numeric"), yend=vector(mode = "numeric")),
    min=numeric(), max=numeric(), n=numeric()))
  
  
  hover.reactive<-reactiveValues(hover.reactive=0)
  hover.reactive.intro<-reactiveValues(hover.reactive=0)
  
  click.reactive<-reactiveValues(click.reactive=list(x=numeric(),
                                                     y=vector(mode = "numeric")))
  
  last.p<-reactiveValues(x=-3)
  
  result<-reactiveValues(df=data.frame(name = factor(),
                                       fiches = vector(mode = "numeric"),
                                       fiches.type=factor(),
                                       exp = vector(mode = "numeric"),
                                       age = vector(mode = "numeric"),
                                       comparison = factor(),
                                       group1 = factor()
  ),
  win=0)
  
  exp<-reactiveValues(n=1)
  
  name<-reactiveValues(name="")
  
  ####  Observe event plot_click  #####
  
  observeEvent(input$plot_click, {
    
    # Condition without interals (first two chips)
    
    if(nfiches$n<2){
      
      if (input$plot_click$x<(-.25)){
        shinyalert("Oops...", "You can only select values between 0 and 10.", type = "warning", closeOnClickOutside = TRUE)
      }
      else if (input$plot_click$x>10.24){
        shinyalert("Oops...", "You can only select values between 0 and 10.", type = "warning", closeOnClickOutside = TRUE)
      }
      else {
        click.reactive$click.reactive$x=((input$plot_click$x+.25)%/%.5)*.5
        last.p$x<-click.reactive$click.reactive$x
        
        # Evito la sovrapposizione degli estremi
        if(nfiches$n==1 & click.reactive$click.reactive$x==vals$df$x[1]){
          shinyalert("Oops...", "The minimum and the maximum must be different.", type = "warning", closeOnClickOutside = TRUE)
        }
        else {
          
          vals$df<-rbind(vals$df,data.frame(x=click.reactive$click.reactive$x, 
                                            y=0))
          
          #Adding 1 to the chip indicator
          nfiches$n<-nfiches$n+1
        }
        
        #Create the intervals whene you have played 2 chips
        
        if (nfiches$n==2){
          
          intervalli$intervalli$min<-min(vals$df$x[1:2])
          intervalli$intervalli$max<-max(vals$df$x[1:2])
          
          intervalli$intervalli$n<-(intervalli$intervalli$max-
                                      intervalli$intervalli$min)%/%.5
          
          intervalli$intervalli$df<-data.frame(
            x=seq(from=intervalli$intervalli$min-.25, to=intervalli$intervalli$max+.25, 
                  by=.5),
            y=rep(0,intervalli$intervalli$n+2), yend=rep(-4,intervalli$intervalli$n+2))
          
          click.reactive$click.reactive$y=vector(mode="numeric", length=intervalli$intervalli$n+1)
        }
      }
    }
    
    
    
    # Condition with interals (more than two chips)
    
    else  {
      
      ### clickreactive$x value
      
      if (input$plot_click$x<intervalli$intervalli$min-.25){
        shinyalert("Oops...", "It is only possible to bet on the values between the minimum and the maximum. If you want to change remove the minimum or maximum value.",
                   type = "warning", closeOnClickOutside = TRUE)
      }
      
      else if (input$plot_click$x>intervalli$intervalli$max+.25){
        shinyalert("Oops...", "It is only possible to bet on the values between the minimum and the maximum. If you want to change remove the minimum or maximum value.", 
                   type = "warning", closeOnClickOutside = TRUE)
      }
      
      else {
        
        
        #### Check if I have played 13 chips
        
        if(nfiches$n<nfiches.max){
          
          click.reactive$click.reactive$x =(input$plot_click$x+.25)%/%.5*.5
          last.p$x<-click.reactive$click.reactive$x
          
          
          # click.reactive$y value
          
          index<-(click.reactive$click.reactive$x-intervalli$intervalli$min+.75)%/%.5
          
          click.reactive$click.reactive$y[index]<-click.reactive$click.reactive$y[index]+1
          
          vals$df<-rbind(vals$df,
                         data.frame(x=click.reactive$click.reactive$x, 
                                    y=-1-click.reactive$click.reactive$y[index]))
          
          #Adding 1 to the chip indicator
          nfiches$n<-nfiches$n+1
          
        }else{
          
          shinyalert("Oops...", "You have already played all your chips!", 
                     type = "warning", closeOnClickOutside = TRUE)
          
        }
      }
    }
    
  })
  
  
  ####  Observe event plot_dblclick  ####
  
  observeEvent(input$plot_dblclick, {
    
    #Condition without interals
    
    if(nfiches$n==0){
      
      shinyalert("Oops...", "There are no chips to remove here.", type = "warning", closeOnClickOutside = TRUE)
      
    }
    
    else if(nfiches$n==1) {
      if(abs(input$plot_dblclick$x-vals$df$x[1])<.3){
        
        vals$df<-vals$df[-1,]
        
        # Remove 1 from the chips indicator
        nfiches$n<-nfiches$n-1
        
        last.p$x<-(input$plot_dblclick$x+.25)%/%.5*.5
      }
      else{
        shinyalert("Oops...", "There are no chips to remove here.", type = "warning", closeOnClickOutside = TRUE)
      }
    }
    
    else if(nfiches$n==2){
      
      if(abs(input$plot_dblclick$x-vals$df$x[1])<.3) {
        vals$df<-vals$df[-1,]
        
        #Remove 1 from the chips indicator
        nfiches$n<-nfiches$n-1
        
        last.p$x<-(input$plot_dblclick$x+.25)%/%.5*.5
      }
      else if (abs(input$plot_dblclick$x-vals$df$x[2])<.3){
        vals$df<-vals$df[-2,]
        
        #Remove 1 from the chips indicator
        nfiches$n<-nfiches$n-1
        
        last.p$x<-(input$plot_dblclick$x+.25)%/%.5*.5
      }
      else{
        shinyalert("Oops...", "There are no chips to remove here.", type = "warning", closeOnClickOutside = TRUE)
      }
      
      
    }
    
    #Condition with interals (more than two chips)
    
    else {
      
      ### I understand in which interval we are
      
      if (input$plot_dblclick$x<intervalli$intervalli$min-.25){
        shinyalert("Oops...", "There are no chips to remove here.", type = "warning", closeOnClickOutside = TRUE)
      }
      else if (input$plot_dblclick$x>intervalli$intervalli$max+.25){
        shinyalert("Oops...", "There are no chips to remove here.", type = "warning", closeOnClickOutside = TRUE)
      }
      else {
        
        index<-(.75+input$plot_dblclick$x-intervalli$intervalli$min)%/%.5
        
        #I look if there is a chip to remove
        
        if(click.reactive$click.reactive$y[index]>0){
          
          x.pos<-(index)*.5+intervalli$intervalli$min-.5
          
          vals$df<-vals$df[vals$df$x!=x.pos | 
                             vals$df$y!=(-1-click.reactive$click.reactive$y[index]),]
          
          click.reactive$click.reactive$y[index]<-click.reactive$click.reactive$y[index]-1
          
          #Remove 1 from the chips indicator
          nfiches$n<-nfiches$n-1
          
          last.p$x<-x.pos 
        } 
        else if (index==intervalli$intervalli$n+1 | index==1) {
          shinyalert("Caution!!", "If you remove the minimum or maximum value all the chips played will be removed. Click 'Remove' to proceed otherwise 'Back'.", 
                     type = "warning",showConfirmButton = TRUE, showCancelButton = TRUE, confirmButtonText = "Remove",
                     confirmButtonCol = "#E6296B",cancelButtonText = "Back",
                     callbackR = function(x){
                       
                       if (x==TRUE){
                         
                         ### Values reset
                         
                         vals$df=data.frame(x=vector(mode ="numeric",length = 0),
                                            y=vector(mode ="numeric", length = 0))
                         
                         nfiches$n=0
                         
                         intervalli$intervalli=list(df=data.frame(
                           x=vector(mode = "numeric"),y=vector(mode = "numeric"), yend=vector(mode = "numeric")),
                           min=numeric(), max=numeric(), n=numeric())
                         
                         
                       }
                     })
        }else{
          shinyalert("Oops...", "There are no chips to remove here.", type = "warning", closeOnClickOutside = TRUE)
        }
        
      }
      
    }  
    
  })
  
  
  ####  Observe plot_hover  ####
  
  observe(
    
    # Condition without interals
    
    if(nfiches$n<2){
      
      if(is.null(input$plot_hover$x) & last.p$x<0){
        hover.reactive$hover.reactive=5} 
      else if(is.null(input$plot_hover$x) & last.p$x>=0 & last.p$x<=10){
        hover.reactive$hover.reactive=last.p$x}
      else {if (input$plot_hover$x<.25){hover.reactive$hover.reactive=0}
        else if (input$plot_hover$x>9.75){hover.reactive$hover.reactive=10}
        else {hover.reactive$hover.reactive=(input$plot_hover$x+.25)%/%.5*.5}}
    }
    
    # Condition with interals
    
    else{
      
      if(is.null(input$plot_hover$x)){
        hover.reactive$hover.reactive=last.p$x
      }
      else {if (input$plot_hover$x<intervalli$intervalli$min-.25){
        hover.reactive$hover.reactive=intervalli$intervalli$min}
        else if (input$plot_hover$x>intervalli$intervalli$max+.25){
          hover.reactive$hover.reactive=intervalli$intervalli$max}
        else {
          hover.reactive$hover.reactive =(input$plot_hover$x+.25)%/%.5*.5
        }}
    }
  )
  
  
  ####  Observe plot_hover.intro ####
  
  observe(
    if(is.null(input$plot_hover.intro$x)){
      hover.reactive.intro$hover.reactive=5} 
    else {if (input$plot_hover.intro$x<.25){hover.reactive.intro$hover.reactive=0}
      else if (input$plot_hover.intro$x>9.75){hover.reactive.intro$hover.reactive=10}
      else {hover.reactive.intro$hover.reactive=(input$plot_hover.intro$x+.25)%/%.5*.5}}
  )
  
  
  ####  Text Outputs  ####
  
  
  #### Intro
  
  output$subtitle<-renderUI({
    
    if(group1=="Boys"){
      x<-paste("On average who is taller between boys and girls?")
    } else if (group1=="Girls") {
      x<-paste("On average who is taller between girls and boys?")
    }
    
    HTML(x)
  })
  
  output$intro1<-renderText(
    if(group1=="Boys"){" of boys and girls"
    }else if (group1=="Girls"){" of girls and boys"}
  )
  
  output$intro2<-renderText({
    if(group1=="Boys"){paste(" average boys' height and girls' height at ",length(age)-1)
    }else if (group1=="Girls"){paste(" average girls' height and boys' height at ",length(age)-1)}
    
  })
  
  output$intro3<-renderUI({ 
    
    
    if (comparison=="CL" & group1=="Boys"){
      text<-"Imagine that 10 couples are formed randomly selecting one boy and one girl of the same age.
In how many of these 10 couples is the boy taller than the girl?"
    } else if (comparison=="CL" & group1=="Girls"){
      text<-"Imagine that 10 couples are formed randomly selecting one girl and one boy of the same age.
In how many of these 10 couples is the girl taller than the boy?"
    } else if(comparison=="U3" & group1=="Boys"){
      text<-"Imagine that 10 boys of a specific age are randomly selected. How many of these boys 
are taller than the average girl of the same age?"
    } else if(comparison=="U3" & group1=="Girls"){
      text<-"Imagine that 10 girls of a specific age are randomly selected. How many of these girls 
are taller than the average boy of the same age?"
    }
    
    x<-paste("<ul><li>", text, "</li><ul>", collapse = "" )
    
    HTML(x)
  })
  
  output$intro4<-renderText (if (comparison=="CL" & group1=="Boys"){
    "The graphical interface is composed of 10 people, one for each couple, where each figure represents who is the 
tallest among the boy and the girl in that couple." 
  } else if (comparison=="CL" & group1=="Girls"){
    "The graphical interface is compsed of 10 people, one for each couple, where each figure represents who is the 
tallest among the girl and the boy in that couple."
  } else if(comparison=="U3" & group1=="Boys"){
    "The graphical interface is composed of 10 people, where the colored figures represent the boys taller than the 
average girl of the same age, whereas the non-colored figures represent the boys shorter than the average girl of the same age."
  } else if (comparison=="U3" & group1=="Girls"){
    "The graphical interface is compsed of 10 people, where the colored figures represent the girls taller than the 
average boy of the same age, whereas the non-colored figures represent the girls shorter than the average boy of the same age."
 })
  
  output$intro5<-renderUI({
    
    if (comparison=="CL" & group1=="Boys"){
      x<-paste("<ul>", 
               "<li> There are no couples in which the boy and girl have the same height. </li>", 
               "<li> Claiming that the boy is taller than the girl in 5 couples out of 10 is the same 
as saying that there is no difference between the average boys' height and girls' height. </li>",
               "<li> A number of couples greater than 5 indicates that on average boys are taller. </li>",
               "<li> A number of couples lower than 5 indicates that on average girls are taller. </li>",
               "</ul>")
    } else if (comparison=="CL" & group1=="Girls"){
      x<-paste("<ul>", 
               "<li> There are no couples in which the girl and boy have the same height. </li>", 
               "<li> Claiming that the girl is taller than the boy in 5 couples out of 10 is the same 
as saying that there is no difference between the average girls' height and boys' height. </li>",
               "<li> A number of couples greater than 5 indicates that on average girls are taller. </li>",
               "<li> A number of couples lower than 5 indicates that on average boys are taller. </li>",
               "</ul>")
    } else if(comparison=="U3" & group1=="Boys"){
      x<-paste("<ul>",
               "<li> Claiming that 5 boys out of 10 are taller than the average girl of the same age is the same as saying 
that there is no difference between the average boys' height and girls' height. </li>",
               "<li> Values greater than 5 indicate that on average boys are taller. </li>",
               "<li> Values lower than 5 indicate that on average girls are taller. </li>",
               "</ul>")
    } else if(comparison=="U3" & group1=="Girls"){
      x<-paste("<ul>",
               "<li> Claiming that 5 girls out of 10 are taller than the average boy of the same age is the same as saying 
that there is no difference between the average girls' height and boys' height. </li>",
               "<li> Values greater than 5 indicate that on average girls are taller. </li>",
               "<li> Values lower than 5 indicate that on average boys are taller. </li>",
               "</ul>")
    }
    
    HTML(x)
  })
  
  output$intro8<-renderText(
    if(comparison=="CL" & group1=="Boys"){
      "At each different age you will have to indicate in how many couples out of 10 the boy is taller than 
the girl. Try to consider the difference between the average boys' height and girls' height at that specific age."
    } else if (comparison=="CL" & group1=="Girls") {
      "At each different age you will have to indicate in how many couples out of 10 the girl is taller than 
the boy. Try to consider the difference between the average girls' height and boys' height at that specific age."
    } else if(comparison=="U3" & group1=="Boys"){
      "At each different age you will have to indicate how many boys out of 10 are taller than the average 
girl of the same age. Try to consider the difference between the average boys' height and girls' height at 
that specifc age."
    } else if (comparison=="U3" & group1=="Girls") {
      "At each different age you will have to indicate how many girls out of 10 are taller than the average 
boy of the same age. Try to consider the difference between the average girls' height and boys' height at
that specifc age."
    }
  )
  
  
  output$instruction<-renderUI({
    
    if(comparison=="CL" & group1=="Boys"){
      
      x<-paste(
        "<ol>", 
        "<li>Indicate a <b> minimum number</b> of couples in which the boy is taller than the girl</li></br>",
        "<li>Indicate a <b> maximum number</b> of couples in which the boy is taller than the girl</li></br>",
        "<li><b>Bet",nfiches.max-2,"chips</b> on the <b> most-likely values</b> between the minimum and the maximum</li></br>",
        "</ol>")
      
    }else if(comparison=="CL" & group1=="Girls"){
      
      x<-paste(
        "<ol>", 
        "<li>Indicate a <b> minimum number</b> of couples in which the girl is taller than the boy</li></br>",
        "<li>Indicate a <b> maximum number</b> of couples in which the girl is taller than the boy</li></br>",
        "<li><b>Bet",nfiches.max-2,"chips</b> on the <b> most-likely values</b> between the minimum and the maximum</li></br>",
        "</ol>")
      
    }else if(comparison=="U3" & group1=="Boys"){
      
      x<-paste(
        "<ol>", 
        "<li>Indicate a <b> minimum number</b> of boys that are taller than the average girl</li></br>",
        "<li>Indicate a <b> maximum number</b> of boys that are taller than the average girl</li></br>",
        "<li><b>Bet",nfiches.max-2,"chips</b> on the <b> most-likely values</b> between the minimum and the maximum</li></br>",
        "</ol>") 
      
    }else if(comparison=="U3" & group1=="Girls"){
      
      x<-paste(
        "<ol>", 
        "<li>Indicate a <b> minimum number</b> of girls that are taller than the average boy</li></br>",
        "<li>Indicate a <b> maximum number</b> of girls that are taller than the average boy</li></br>",
        "<li><b>Bet",nfiches.max-2,"chips</b> on the <b> most-likely values</b> between the minimum and the maximum</li></br>",
        "</ol>")       
    }
    
    HTML(x)
    
  })
  
  
  #### Experiment title
  
  output$n_exp<-renderText(
    if(exp$n==1){
      "Trial Exercise"
    }else{
      paste( "Exercise:", exp$n-1, "of", length(age)-1)})
  
  #### Age index
  
  output$age_index1<-renderText(paste( age[exp$n]))
  output$age_index2<-renderText(paste( age[exp$n]))
  
  
  #### Chips index
  
  output$fiches_max<-renderText(paste( nfiches.max-2))
  
  output$fiches_index<-renderText(paste( 
    if(nfiches$n<=2){0
    }else{nfiches$n-2}))
  
  output$fiches_left<-renderText(paste(
    if(nfiches$n<=2){nfiches.max-2
    }else{nfiches.max - nfiches$n}))
  
  ### Age exercise index
  
  output$age<-renderText(
    if(exp$n==1){
    } else {paste(age[exp$n],"years old" )}
  )
  
  ####  People image Intro   ####
  
  output$plot.img.intro<-renderImage({
    
    img.crop<-image_crop(img.front, geometry_area(
      width = 110+hover.reactive.intro$hover.reactive*180,height = 450))
    img<-c(img.back, img.crop)
    
    comb1<-image_mosaic(img)
    
    tmpfile.intro <- comb1%>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile.intro, contentType = "image/jpeg",style="width: 100%")
  })
  
  
  ####  Values Text Intro  #####
  
  output$index.intro<-renderUI({
    
    if(comparison=="CL" & group1=="Boys"){
      x<-paste0( paste("<b>",round(hover.reactive.intro$hover.reactive,1),"</b>", collapse = ""), "Couples out of <b>10</b>")
    } else if(comparison=="CL" & group1=="Girls"){
      x<-paste0( paste("<b>",round(hover.reactive.intro$hover.reactive,1),"</b>", collapse = ""), "Couples out of <b>10</b>")
    } else if (comparison=="U3" & group1=="Boys"){
      x<-paste0( paste("<b>",round(hover.reactive.intro$hover.reactive,1),"</b>", collapse = ""), "Boys out of <b>10</b>")
    } else  if (comparison=="U3" & group1=="Girls"){
      x<-paste0( paste("<b>",round(hover.reactive.intro$hover.reactive,1),"</b>", collapse = ""), "Girls out of <b>10</b>")
    }
    
    HTML(x)
  })
  
  
  ####  Chips Plot Intro  ####
  
  output$plot1.intro<-renderPlot({
    
    ggplot()+
      theme_bw()+
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.line.x.top = element_line(colour = "black"),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_line(size=.20)
      )+
      scale_x_continuous(position = "top", limits = c(0,10), 
                         breaks = seq(0,10,.5), labels = c("0","","1","","2","",
                                                           "3","","4","","5","",
                                                           "6","","7","","8","",
                                                           "9","","10"))+
      scale_y_continuous(limits = c(-3,1), breaks = c(0,-2))
    
  })
  
  
  
  ####  Exercise instruction ####
  
  output$exercise1<-renderUI({
    if(exp$n==1){
      x<-paste0("Get used to the graphical interface by assigning and removing chips <b> inside the grid</b>. 
Without referring to a specific age, try to:")
      
    }else{
      x<-paste0("Consider the difference between the average ",
                if(group1=="Boys"){"boys' height and girls' height at"}
                else if(group1=="Girls"){"girls' height and boys' height at"}, 
                paste("<b>",age[exp$n],"years</b>.", collapse=""),"<br>",
                if(comparison=="CL" & group1=="Boys"){
                  "In how many couples out of 10 is the boy taller than the girl?"
                }else if(comparison=="CL" & group1=="Girls"){
                  "In how many couples out of 10 is the girl taller than the boy?"
                }else if(comparison=="U3" & group1=="Boys"){
                  "How many boys out of 10 are taller than the average girl of the same age?"
                }else if(comparison=="U3" & group1=="Girls"){
                  "How many girls out of 10 are taller than the average boy of the same age?"
                }
      )
    }
    
    HTML(x)
  })
  
  
  output$exercise2<-renderUI({
    if(exp$n==1){
      if(nfiches$n==0){
        x<-paste0("1. Select  <b> any minimum value </b> within the grid.</br> A first indicator will appear.")
      }else if (nfiches$n==1){
        x<-paste0("2. Select  <b> any maximum value </b> within the grid.</br> A second indicator will appear.")
      }else if (nfiches$n==nfiches.max){
        x<-paste0("Great! You have played all your chips. Click on '<b> Next </b>' button to proceed.")
      }else{
        x<-paste0("3. Now intervals have been created between the minimum and maximum value on which it is possible to bet. ",
                  "<b>Bet all your chips</b> between these intervals.")
      }
    } else{
      if(nfiches$n==0){
        x<-paste0("1. Select a <b>minimum number</b>.")
      }else if (nfiches$n==1){
        x<-paste0("2. Select a <b>maximum number</b>.")
      }else if (nfiches$n==nfiches.max){
        x<-paste0("Great! You have played all your chips. Click on '<b> Next </b>' button to proceed.")
      }else{
        x<-paste0("3. Bet your chips on the <b>most likely values</b>.")
      }
    }
    
    HTML(x)
  })
  
  ####  People Image ####
  
  output$plot.img <- renderImage({
    
    img.crop<-image_crop(img.front, geometry_area(
      width = 110+hover.reactive$hover.reactive*180,height = 450))
    img<-c(img.back, img.crop)
    
    comb1<-image_mosaic(img)
    
    tmpfile <- comb1%>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg",style="width: 100%")
  })
  
  ####  Values Text ####
  
  output$index<-renderUI({
    if(comparison=="CL"){
      x<-paste0("<b>",round(hover.reactive$hover.reactive,1),"</b> Couples out of <b>10</b>", collapse = "")
      
    } else if (comparison=="U3"){
      x<-paste0( paste("<b>",round(hover.reactive$hover.reactive,1),"</b>", collapse = ""), 
                 if(group1=="Boys"){
                   "Boys out of <b>10</b>"
                 }else if (group1=="Girls"){
                   "Girls out of <b>10</b>"
                 })
    }
    HTML(x)
  })
  
  
  ####  Chips Plot ####
  
  output$plot1<-renderPlot({
    
    if (nfiches$n<=2){
      data.image<-rep(list(triangle),length(vals$df$x))
    }else{
      data.image<-c(rep(list(triangle),2),rep(list(coin),length(vals$df$x)-2))
    }
    
    ggplot(vals$df, aes(x,y))+
      geom_custom(aes(x,y,data=data.image),grob_fun=rasterGrob,
                  fun_params = list(height=unit(1.1,"cm")))+
      theme_bw()+
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.line.x.top = element_line(colour = "black"),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_line(size=.20)
      )+
      scale_x_continuous(position = "top", limits = c(0,10), 
                         breaks = seq(0,10,.5), labels = c("0","","1","","2","",
                                                           "3","","4","","5","",
                                                           "6","","7","","8","",
                                                           "9","","10"))+
      scale_y_continuous(limits = c(-12,1))+
      if(nfiches$n>=2){
        geom_segment(data= intervalli$intervalli$df,
                     aes(x = x, y = y, xend = x, yend = yend))
      }
  })
  
  
  
  ####  Buttons next pages  ####
  
  
  #### Button botton questionnaire
  
  
  observeEvent(input$nextintro, {
    
    if(input$name=="" & input$privacy==2){
      shinyalert("Oops...", "To proceed you have to enter your nick-name and accept privacy terms.", type = "warning", closeOnClickOutside = TRUE)
    }else if(input$name!="" & input$privacy==2){
      shinyalert("Oops...", "To proceed you have to accept privacy terms.", type = "warning", closeOnClickOutside = TRUE)
    }else if(input$name=="" & input$privacy==1){
      shinyalert("Oops...", "To proceed you have to enter your nick-name.", type = "warning", closeOnClickOutside = TRUE)
    }else{
      
      showTab(inputId = "App", target = "intro")
      updateTabsetPanel(session, "App",selected = "intro")
      
      hideTab(inputId = "App", target = "info")
      
      name$name<-paste0(input$name,date)
    }
  })
  
  
  #### Button botton Instruction
  
  observeEvent(input$nextpage, {
    
    showTab(inputId = "App", target = "exs")
    
    updateTabsetPanel(session, "App",
                      selected = "exs")
    
  })
  
  
  #### Button next experiment
  
  output$next.exp<-renderUI({
    if(nfiches$n==nfiches.max){
      
      ##### Tasto Avanti
      
      actionButton('nextexp', 'Next')
      
    }
  })
  
  
  ##
  
  observeEvent(input$nextexp, {
    
    if(nfiches$n<nfiches.max){
      shinyalert("Oops...", "You have to bet all your chips before proceeding.",
                 type = "warning", closeOnClickOutside = TRUE)
    } else{
      
      ### save elicited values
      
      result$df<-rbind(result$df,
                       data.frame(name = rep(name$name, nfiches.max),
                                  fiches = vals$df$x[1:nfiches.max],
                                  fiches.type=c(rep("limits",2), rep("fiches",nfiches.max-2)),
                                  exp = rep(exp$n, nfiches.max),
                                  age = rep (age[exp$n], nfiches.max),
                                  comparison = factor(rep(comparison, nfiches.max)),
                                  group1 = factor(rep(group1, nfiches.max))))
      
      ### Values reset
      
      vals$df=data.frame(x=vector(mode ="numeric",length = 0),
                         y=vector(mode ="numeric", length = 0))
      
      nfiches$n=0
      
      intervalli$intervalli=list(df=data.frame(
        x=vector(mode = "numeric"),y=vector(mode = "numeric"), yend=vector(mode = "numeric")),
        min=numeric(), max=numeric(), n=numeric())
      
      
      ### Go to the next exercise
      
      exp$n = exp$n+1
      
      
      if(exp$n>length(age)){
        
        ### Compute the number of winning chips
        
        if(comparison=="CL" & group1=="Boys"){
          for(i in 2:length(age)){
            result$win<-result$win+length(result$df$fiches[result$df$age==age[i] & result$df$fiches.type=="fiches" &
                                                             result$df$fiches==(data.comparison$CL[data.comparison$year==age[i]]+2)%/%5*.5])
          }
        }else if (comparison=="CL" & group1=="Girls"){
          for(i in 2:length(age)){
            result$win<-result$win+length(result$df$fiches[result$df$age==age[i] & result$df$fiches.type=="fiches" &
                                                             result$df$fiches==10-(data.comparison$CL[data.comparison$year==age[i]]+2)%/%5*.5])
          } 
        }else if (comparison=="U3" & group1=="Boys"){
          for(i in 2:length(age)){
            result$win<-result$win+length(result$df$fiches[result$df$age==age[i] & result$df$fiches.type=="fiches" &
                                                             result$df$fiches==(data.comparison$U3[data.comparison$year==age[i]]+2)%/%5*.5])
          }
        }else if (comparison=="U3" & group1=="Girls"){
          for(i in 2:length(age)){
            result$win<-result$win+length(result$df$fiches[result$df$age==age[i] & result$df$fiches.type=="fiches" &
                                                             result$df$fiches==10-(data.comparison$U3[data.comparison$year==age[i]]+2)%/%5*.5])
          }
        }
        
        
        ### Save 
        
        data1<-cbind(result$df,
                     win=rep(result$win,nrow(result$df)),
                     nick.name=rep(input$name,nrow(result$df)),
                     sex=rep(input$sex,nrow(result$df)),
                     age.subj=rep(input$age,nrow(result$df)),
                     education=rep(input$education,nrow(result$df)),
                     height=rep(input$height,nrow(result$df)),
                     q1=rep(input$q1,nrow(result$df)),
                     q2=rep(input$q2,nrow(result$df)),
                     privacy=rep(input$privacy,nrow(result$df))
        )
        
        save(data1, file = paste0(path,name$name, ".rda", collapse = ""))
        
        #### Go to the final page
        
        showTab(inputId = "App", target = "end")
        updateTabsetPanel(session, "App",selected = "end")
        
        hideTab(inputId = "App", target = "intro")
        hideTab(inputId = "App", target = "exs")
        
        
        
      }
    }
  })
  
  
  
  
  ####  Return Results  ####
  
  output$results<- renderUI({
    
    
    if(show.results=="YES"){
      
      age.ordered<-sort(age[-1])
    
    tex<-"<div align='center'>I can guess that now  you are curious to know which is the actual difference 
between the average"
    
    if(group1=="Boys"){
      tex<-paste(tex,"boys' height and girls' height at the different ages. Here you find the official data 
by the World Health Organization (WHO) from 2007: </div> <br/> <ul>", collapse ="")
    } else if(group1=="Girls"){
      tex<-paste(tex,"girls' height and boys' height at the different ages. Here you find the official data 
by the World Health Organization (WHO) from 2007: </div> <br/> <ul>", collapse ="")
    }
    
    for(i in 1:length(age.ordered)){
      tex<-paste(tex,"<br/> ","<li> At",paste("<b>",age.ordered[i],"years </b>", collapse=""), 
                 if(group1=="Boys"){
                   paste("the average boys' height is ",data.comparison$mean.m[data.comparison$year==age.ordered[i]],
                         "cm and girls' height is",data.comparison$mean.f[data.comparison$year==age.ordered[i]], collapse = "")
                 }else if (group1=="Girls"){
                   paste("the average girls' height is",data.comparison$mean.f[data.comparison$year==age.ordered[i]],
                         "cm and boys' height is",data.comparison$mean.m[data.comparison$year==age.ordered[i]], collapse = "")
                 },"cm </li>",
                 
                 "<img src='",
                 if(comparison=="CL" & group1=="Boys"){
                   paste0(paste(data.comparison$img.CL.m[data.comparison$year==age.ordered[i]], collapse = ''), "' width= '100%' />",
                          " <br/>",
                          "<div align='center'>",
                          paste("<b>",(data.comparison$CL[data.comparison$year==age.ordered[i]]+2)%/%5*.5,"</b>", collapse = ""), 
                          " Couples out of <b>10</b>", collapse = "")
                 }else if (comparison=="CL" & group1=="Girls"){
                   paste0(paste(data.comparison$img.CL.f[data.comparison$year==age.ordered[i]], collapse = ''), "' width= '100%' />",
                          " <br/>",
                          "<div align='center'>",
                          paste("<b>",(10-(data.comparison$CL[data.comparison$year==age.ordered[i]]+2)%/%5*.5),"</b>", collapse = ""),
                          " Couples out of <b>10</b>", collapse = "")
                 }else if (comparison=="U3" & group1=="Boys"){
                   paste0(paste(data.comparison$img.U3.m[data.comparison$year==age.ordered[i]], collapse = ''), "' width= '100%' />",
                          " <br/>",
                          "<div align='center'>",
                          paste("<b>",(data.comparison$U3[data.comparison$year==age.ordered[i]]+2)%/%5*.5,"</b> Boys out of <b>10</b>", collapse = ""))
                 }else if (comparison=="U3" & group1=="Girls"){
                   paste0(paste(data.comparison$img.U3.f[data.comparison$year==age.ordered[i]], collapse = ''), "' width= '100%' />",
                          " <br/>",
                          "<div align='center'>",
                          paste("<b>",(10-(data.comparison$U3[data.comparison$year==age.ordered[i]]+2)%/%5*.5),"</b> Girls out of <b>10</b>", collapse = ""))
                 },
                 "</div>",
                 collapse = "")
      
    }
    
    x<-paste0(tex, "</ul>","<br/> <div align='center'> <h4> You have won ",result$win," chips!!!</h4></div>")
    
    
    
    HTML(x)
    }
    
    
  })
  
  
  
  
}


shinyApp(ui, server)
