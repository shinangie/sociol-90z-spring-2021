library(shiny)
library(ggforce)
library(tidyverse)
library(shinydashboard)
library(stargazer)
library(haven)
library(Hmisc)
library(tidymodels)
library(broom)

nlsy97 <- read_dta("nlsy97/nlsy97.dta") %>% 
    rename_all(str_to_lower) %>% 
    select(ts = asvab_math_verbal_score_pct_1999) %>% 
    mutate(ts = ifelse(ts %in% c(-1, -2, -3, -4, -5), NA, ts),
           ts = ts/100000)

caaspp <- read.delim("cadash/caaspp.txt", 
                     header = TRUE, sep = ",", dec = ".") %>% 
    clean_names()

ui <- navbarPage(
    "SOCIOL 90Z FINAL PROJECT",
    tabPanel("Introduction", 
             titlePanel("Introduction"),
             
             # intro
             p("Can a regression model based on data from the ", 
               a(href = "https://www.nlsinfo.org/content/cohorts/nlsy97", 
                 "1997 National Longitudinal Survey of Youth (NLSY97)"), 
               "predict college matriculation rates for students in the state of California? I have inspected several socioeconomic and educational variables shared between NLSY97 and the ", 
               a(href = "https://dq.cde.ca.gov/dataquest/", 
                 "California Department of Education’s DataQuest dashboard"), 
               "for California's K-12 public educational system, to determine what variables most strongly impact postsecondary academic activity for students from the state of California within the structural continuities between the two datasets."
               )
             
             # end of tab panel
             ),
    
    tabPanel("Data",
             titlePanel("Data"),
             
             # nlsy97
             p("The 1997 National Longitudinal Survey of Youth (NLSY97) is a longitudinal project that follows the lives of a sample of American youth born between 1980-84 (ages 12-17 when first interviewed in 1997). This ongoing cohort of 8,984 respondents has been surveyed 18 times to date and was interviewed biennially."),
             
             # cadash
             p("The California Department of Education has a searchable database called DataQuest that centralizes the accessibility of data on public schools in the state of California. From DataQuest, I used the ")
             
             # end of tab panel
             ),
    
    tabPanel("Analysis",
             titlePanel("Analysis"),
             
             fluidPage(plotOutput("plot2"))
             
             # begin fluid page
             # fluidPage(selectInput("x", "X variable", choices = names(df)),
             #           selectInput("y", "Y variable", choices = names(df)),
             #           selectInput("geom", "geom", c("point", "column", "jitter")),
             # 
             #           # plot outputs
             #           plotOutput("plot"),
             #           p("something about the first plot"),
             # 
             #           plotOutput("plot2"),
             #           p("something about the second plot"),
             #           
             #           plot regression table
             #           uiOutput("lm1"),#A regression table
             #           p("something about the regression table"),
             #           )
             
             # end of tab panel
             ),
    
    tabPanel("Conclusion", 
             titlePanel("Conclusion"),
             
             p("Text")
             
             # end of tab panel
             ),
    
    tabPanel("Details", 
             titlePanel("Details"),
             
             p("Angie Shin is a sophomore studying Data Science in the Government department at Harvard College. This Shiny App is a project for the course Sociology 90z, a research-centered course on inequality in the United States.")
             
             # end of tab panel
             ),
    
    tabPanel("References",
             titlePanel("References"),
             
             h3("Readings"),
             p("Autor, D.H., 2014. Skills, education, and the rise of earnings inequality among the other 99 percent. Science, 344(6186), pp.843-851."),
             p("Bailey, M.J. and Dynarski, S.M., 2011. Gains and gaps: Changing inequality in US college entry and completion (No.w17633). National Bureau of Economic Research."),
             p("Chetty, R., Hendren, N., Jones, M.R. and Porter, S.R., 2020. Race and economic opportunity in the United States: An intergenerational perspective. The Quarterly Journal of Economics, 135(2), pp.711-783."),
             p("Chetty, R., Hendren, N., Kline, P., Saez, E. and Turner, N., 2014. Is the United States still a land of opportunity? Recent trends in intergenerational mobility. American Economic Review, 104(5), pp.141-47."),
             p("Duncan, O.D., 1968. Inheritance of race or inheritance of poverty? On understanding poverty, pp.85-110."),
             p("Morgan and Winship, 2015. Counterfactuals and Causal Inference: Methods and Principles for Social Research. Chapters 1.1-1.5,3.1-3.3"),
             p("Reardon, S.F., 2011. The widening academic achievement gap between the rich and the poor: New evidence and possible explanations. Whither opportunity, pp.91-116."),
             
             h3("Datasets"),
             p("http://naasurvey.com/reports/")
             
             # end of tab panel
             )
)

server <- function(input, output, session) {
    
    #Your regression
    # m.lm <- lm(y~x, data=df)
    # m.lm <- glm(y~x, data = df, family = binomial()) #logit
    
    #You can use stargazer to make your tables look nice. 
    #Here you can use stargazer's functions like you normally would
    # output$lm1 <- renderUI(HTML(stargazer(m.lm, type="html")))
    
    # plot_geom <- reactive({
    #     switch(input$geom,
    #            point = geom_point(),
    #            smooth = geom_smooth(se = TRUE, na.rm = TRUE),
    #            jitter = geom_jitter()
    #     )
    # })
    # note that .data[[input$x]] is just indexing the data which 
    # match the input from the dropdown (select input) above
    # output$plot <- renderPlot({
    #     ggplot(df, aes(.data[[input$x]], .data[[input$y]])) +  
    #         plot_geom()
    # }, res = 96)
    # 
    output$plot2 <- renderPlot({
        ggplot(nlsy97, aes(x = ts)) +
            geom_histogram() +
            labs(title = "Distribution of NLSY97 test scores",
                 x = "Percentile Rank Performance",
                 y = "Frequency") +
            theme_classic()
    }, res = 96)
    # 
    # Note plot 3 is just a basic histogram with .data[[input$x]] instead
    # You can totally make a regular ggplot without the drop option by
    # changing .data[[input$x]] to x = your variable
    # output$plot3 <- renderPlot({
    #     ggplot(df, aes(.data[[input$x]])) +
    #         geom_histogram()
    # }, res = 96)
}

shinyApp(ui = ui, server = server)
