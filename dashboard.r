header <- dashboardHeader(title = "Play Store App Analysiser") 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("HOME", tabName = "dashboard", icon = icon("home",class="fas fa-home")),
    menuItem("DATATABLE", tabName = "data", icon = icon("search",class="fas fa-search")),
    menuItem("BAR REPERSENTATION", tabName = "dar", icon = icon("chart-bar",class="fas fa-chart-bar")),
    menuItem("POINT REPRESENTATION", tabName = "his", icon = icon("chart-bar",class="fas fa-chart-bar")),
    menuItem("REVIEWS", tabName = "Areview", icon = icon("question-circle",class="fas fa-question-circle")),
    menuItem("APP RATING PREDICTOR",tabName = "Rating", icon = icon("award",class="fas fa-award")),
    menuItem("ABOUT US ", tabName = "Aboutus", icon = icon("user",class="fas fa-user"))
      )
  )

frow1 <- fluidRow(
  valueBoxOutput("value1"),
  valueBoxOutput("value2"),
  valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Pie Chart Of Apps By There Rating",
    status = "primary",
    solidHeader = TRUE ,
    collapsible = TRUE ,
    plotOutput("q1", height = "500px")
  ),
  box(
    title = "App Rating  V/S  Apps Reviews ",
    status = "primary",
    solidHeader = TRUE ,
    collapsible = TRUE ,
    plotOutput("q2", height = "500px")
  ),
  box(
    title = "Number Of Apps By Top 15 Categories",
    status = "primary",
    solidHeader = TRUE ,
    collapsible = TRUE ,
    plotOutput("p1", height = "500px")
  ),
  box(
    title = "Top 15 Categories Of Apps With maximum Number Of Installer",
    status = "primary",
    solidHeader = TRUE ,
    collapsible = TRUE ,
    plotOutput("p2", height = "500px")
  ),
  box(
    title = "Top 15 Categories of Paid Apps",
    status = "primary",
    solidHeader = TRUE ,
    collapsible = TRUE ,
    plotOutput("p3", height = "500px")
  ),
  box(
    title = "Top 15 Categories Of Free Apps",
    status = "primary",
    solidHeader = TRUE ,
    collapsible = TRUE ,
    plotOutput("p4", height = "500px")
  )
)

body <- dashboardBody(tabItems(
                        tabItem(tabName = "dashboard",
                                frow1, frow2     
                        ),
                        tabItem(tabName = "dar",
                                titlePanel("Creating The Plots"),
                                sidebarLayout(sidebarPanel(
                                  selectInput(
                                    inputId = "characterstic",
                                    label="Select The Category for which you want to histogram For Top 10 Genres",
                                    choices =sort(unique(dataset$Category)),
                                    selected = "ART_AND_DESIGN"
                                  )
                                ),
                                mainPanel(plotOutput("ploty1"))),
                                sidebarLayout(sidebarPanel(
                                  selectInput(
                                    inputId = "TI1",
                                    label="select the characterstic for which you want to summary",
                                    choices = c("Rating","Reviews","Size"),
                                    selected = "Rating"
                                  )
                                ),
                                mainPanel(tabsetPanel(
                                  tabPanel("summary",verbatimTextOutput("summary1")),
                                  tabPanel("Histplot",plotOutput("ploty11")),
                                  tabPanel("BarPlot",plotOutput("ploty12"))
                                ))
                                )
                                ),
                        
                        tabItem(tabName = "data",
                                titlePanel("Data Table Of Play Store Data Set"),
                                sidebarPanel(selectInput(inputId="SI1",label="Category",choices=c("All",sort(as.character(factor(unique(playstore$Category))))),
                                                         selected = "All")),
                                sidebarPanel(selectInput(inputId="SI2",label="Genres",choices=c("All",sort(as.character(factor(unique(playstore$Genres))))),
                                                         selected = "All")),
                                sidebarPanel(selectInput(inputId="SI3",label="Rating",choices=c("All",sort(as.character(factor(unique(playstore$Rating))))),
                                                         selected = "All")),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Data Table",DT::dataTableOutput("m1"))
                                  ))),
                        tabItem(tabName = "his",
                                fluidPage(pageWithSidebar(
                                  
                                  headerPanel("App Graph  Explorer"),
                                  
                                  sidebarPanel(
                                    
                                    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(setdat),
                                                value=min(1000, nrow(setdat)), step=500, round=0),
                                    
                                    selectInput('x', 'X', names(setdat)),
                                    selectInput('y', 'Y', names(setdat), names(setdat)[[2]]),
                                    selectInput('color', 'Color', c('None', names(setdat))),
                                    
                                    checkboxInput('jitter', 'Jitter'),
                                    checkboxInput('smooth', 'Smooth'),
                                    
                                    selectInput('facet_row', 'Facet Row', c(None='.', names(setdat))),
                                    selectInput('facet_col', 'Facet Column', c(None='.', names(setdat)))
                                  ),
                                  
                                  mainPanel(
                                    plotOutput('plot')
                                  )
                                ))),
                        
                        tabItem(tabName = "Areview",titlePanel("Top Word Of Reviews Of Your App"),
                                  tabsetPanel(
                                    tabPanel("Positive Reviews",DT::dataTableOutput("plus1")),
                                    tabPanel("Netural Reviews",DT::dataTableOutput("netural1")),
                                    tabPanel("Negative Reviews",DT::dataTableOutput("minus1"))
                                )
                                ),
                        
                        tabItem(tabName = "Aboutus",
                                titlePanel("About Devloper :"),
                                infoBoxOutput("id1"),
                                infoBoxOutput("id2")),
                        tabItem(tabName ="Rating",
                                titlePanel("Rating Of Your App"),
                                sidebarPanel(selectInput(inputId="II1",label="Category",choices=seq(1,34),
                                                         selected=2)),
                                sidebarPanel(selectInput(inputId="II2",label="Genres",choices=seq(1,120),
                                                         selected=2)),
                                infoBoxOutput("r1"),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Category Table ",DT::dataTableOutput("n1")),
                                    tabPanel("Genres Table",DT::dataTableOutput("n2"))
                                    )
                                ))
                        ))

ui<-dashboardPage(title = 'Play Store App Analysiser', 
                    header, sidebar, body, skin='red')



server <- function(input, output) { 
  output$value1 <- renderValueBox({
    valueBox(formatC(nrow(app), big.mark=',')
             ,paste('Total Apps On DataSet       ',"Average Rating:",round(sum(app$Rating)/nrow(app),3)),
        icon = icon("android",class="fab fa-android"),
        color = "purple")  
  })

  output$n1<-DT::renderDataTable({cf})
  output$n2<-DT::renderDataTable({gf})
  output$r1<-renderInfoBox({
    infoBox("Rating Of Your App",
            round(predict(lnmod,data.frame(Category=as.integer(input$II1),
                                           Genres=as.integer(input$II2)))-(as.integer(input$II1))/100,4),
            icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow", fill = TRUE
    )
  })
  output$id1<-renderInfoBox({
    infoBox("Akshay Kumar Jain",
            "Click To Know More",
            href = "https://www.linkedin.com/in/akshay-jain-5ba359155",
            icon = icon("user-graduate",class="fas fa-user-graduate"),
            color = "purple", fill = TRUE
    )
  })
  output$id2<-renderInfoBox({
    infoBox("Dhruv Marothi",
            "Click To Know More",
            href = "https://www.linkedin.com/in/dhruv-marothi-a81804179",
            icon = icon("user-graduate",class="fas fa-user-graduate"),
            color = "black", fill = TRUE
    )
  })
  app1<-app %>% filter(Installs=="500000000")
  app2<-app1 %>% arrange(desc(Rating)) %>% head(1)
  output$value2 <- renderValueBox({
    valueBox(paste(app2$Rating),
      paste('Top App:',app2$App,'Total User:',app2$Installs),
      icon = icon("download",lib='glyphicon'),
      color = "purple")  
  })
  x<-app %>% group_by(Category) %>% summarise(maximum=n()) %>% arrange(desc(maximum))
  top.category <- app %>% group_by(Category) %>% summarise(value = sum(as.numeric(Installs))) %>% filter(value == max(value))
  output$value3 <- renderValueBox({
    valueBox(
      paste(nrow(x)),
      paste("Total Categories Apps      ","   Top Category:",top.category$Category),
      icon = icon("gamepad",class="fas fa-gamepad"),
      color = "yellow")   
  })
  output$q1 <- renderPlot({
    rating1<-dataset$Rating
    l1<-0
    l2<-0
    l3<-0
    l4<-0
    l5<-0
    for (i in rating1){
      if(i<=1){
        l1<-l1+1
      }else if(i<=2){
        l2<-l2+1
      }else if(i<=3){
        l3<-l3+1
      }else if(i<=4){
        l4<-l4+1
      }else{
        l5<-l5+1
      }
    }
    x<-data.frame(Apps=c(l1,l2,l3,l4,l5),Rating=c(1,2,3,4,5))
    ggplot(x,aes(x="",y=Rating,fill=Apps))+geom_bar(width=1,stat="identity",color="white")+
      coord_polar("y",start=0)+theme_void()
  })
  output$q2 <- renderPlot({
    ggplot(dataset, aes(x=Reviews, y=Rating)) +
      scale_x_continuous(trans='log10', labels=comma) +
      geom_point(aes(col=Type)) +
      labs(title="Android App Ratings vs Number of Reviews", subtitle="Playstore Dataset", y="Rating from 1 to 5 stars", x="Number of Reviews") +
      theme_linedraw()
    })
  output$p1 <- renderPlot({
    c <- app %>%group_by(Category) %>%summarize(Count = n()) %>%arrange(desc(Count))
    c <- head(c, 15)
    ggplot(c, aes(x = Category, y = Count,fill = Category)) +
      geom_bar(stat="identity", width=0.7) +ylab("Total Apps") + 
      xlab("Categories Of App") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+ggtitle("Top15 Categories ")
    
  })
  output$p2 <- renderPlot({
      dataset %>% group_by(Category) %>% summarize(totalInstalls = sum(Installs)) %>%
      arrange(desc(totalInstalls)) %>% head(15) %>%
      ggplot(aes(x = Category, y = totalInstalls, fill = Category)) +
      geom_bar(stat="identity") +ylab("Total Apps") + 
      xlab("Categories Of App") +
      labs(title= "Top15 Installed Categories" ) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  output$p3 <- renderPlot({dataset %>%filter(Type == "Paid") %>%
      group_by(Category) %>%summarize(totalInstalls = sum(Installs)) %>%
      arrange(desc(totalInstalls)) %>%
      head(15) %>%
      ggplot(aes(x = Category, y = totalInstalls,fill=Category)) +
      geom_bar(stat="identity", width=.7) +
      labs(title= "Top15 Paid Categories" ) +ylab("Total Apps") + 
      xlab("Categories Of App") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    
  })
  output$p4 <- renderPlot({
    dataset %>%
      filter(Type == "Free") %>%
      group_by(Category) %>%
      summarize(totalInstalls = sum(Installs)) %>%
      arrange(desc(totalInstalls)) %>%
      head(15) %>%
      ggplot(aes(x = Category, y = totalInstalls,fill=Category)) +
      geom_bar(stat="identity", width=.7) +
      labs(title= "Top15 Free Categories" ) +ylab("Total Apps") + 
      xlab("Categpry Of App") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+ggtitle("Top 15 Free Categories ")
    
  })
  output$netural1<-DT::renderDataTable({
    nuet
  })
  output$minus1<-DT::renderDataTable({
    minus
  })
  output$plus1<-DT::renderDataTable({
    plus
  })
  
  output$plot <- renderPlot({
    setdat <- reactive({
      app[sample(nrow(app), input$sampleSize),]
    })
    
    p <- ggplot(setdat(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    print(p)
    
  }, height=700)
  
  output$ploty1=renderPlot({
    dataset %>%
      filter(Category ==input$characterstic) %>%
      group_by(Genres) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(10) %>%
      ggplot(aes(x = Genres, y = Count)) +
      geom_bar(stat="identity", width=.5,  fill="gold1")+
      labs(title= paste("TOP10 GENRES OF",input$characterstic,"CATEGORY" ))
  })
  output$summary1=renderPrint(
    {
      summary(dataset[,input$TI1])
    }
  )
  output$ploty11=renderPlot({
    hist(dataset[,input$TI1],main="Hist Plot",xlab=input$TI1)
  })
  output$ploty12=renderPlot({
    barplot(dataset[,input$TI1],main="bar plot",xlab=input$TI1)
  })
  output$m1<-DT::renderDataTable({
    if(input$SI1=="All" & input$SI2=="All" & input$SI3=="All"){
      playstore
    }else if(input$SI1!="All" & input$SI2=="All" & input$SI3=="All"){
      filter(playstore,playstore$Category==input$SI1)
    }else if(input$SI1=="All" & input$SI2!="All" & input$SI3=="All"){
      filter(playstore,playstore$Genres==input$SI2)
    }else if(input$SI1=="All" & input$SI2=="All" & input$SI3!="All"){
      filter(playstore,playstore$Rating==input$SI3)
    }else if(input$SI1!="All" & input$SI2!="All" & input$SI3=="All"){
      data1<-filter(playstore,playstore$Category==input$SI1)
      filter(data1,data1$Genres==input$SI2)
    }else if(input$SI1=="All" & input$SI2!="All" & input$SI3!="All"){
      data1<-filter(playstore,playstore$Genres==input$SI2)
      filter(data1,data1$Rating==input$SI3)
    }else if(input$SI1!="All" & input$SI2=="All" & input$SI3!="All"){
      data1<-filter(playstore,playstore$Category==input$SI1)
      filter(data1,data1$Rating==input$SI3)
    }else{
      data1<-filter(playstore,playstore$Category==input$SI1)
      data2<-filter(data1,data1$Rating==input$SI3)
      filter(data2,data2$Genres==input$SI2)
    }
  })
}
shinyApp(ui, server)
