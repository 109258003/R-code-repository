library(shiny)
library(ca)
data(iris)

# PCA
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
eigs <- ir.pca$sdev^2
pcaVar <- rbind(Variance = round(eigs,2), Proportion = round(eigs/sum(eigs),2), 
                Cumulative = round(cumsum(eigs)/sum(eigs),2))
pcaVar <- t(pcaVar)
pcaVar <- as.data.frame(pcaVar)
pcaVar <- cbind(PC=c('PC1','PC2','PC3','PC4'), pcaVar)
ir.pca.df <- data.frame(PC1=ir.pca$x[,1], PC2=ir.pca$x[,2], 
                        PC3=ir.pca$x[,3], PC4=ir.pca$x[,4])
ir.pca.df$category <- iris$Species
colors <- c("red","blue","orange")

# CA
iris2 <- iris[, 1:4]
ca1 <- ca(iris2)

# UI
ui <- fluidPage(
        titlePanel("PCA and CA analysis"),
        sidebarLayout(
        sidebarPanel(
        selectInput('xcol', 'Variable X', choices = names(ir.pca.df[,1:4])),
        selectInput('ycol', 'Variable Y', choices = names(ir.pca.df[,1:4]))),
        mainPanel(
          tabsetPanel(
            tabPanel("PCA plot",       plotOutput("pcaplot")),
            tabPanel("CA plot",        plotOutput("caplot")),
            tabPanel("input data",     tableOutput("iris_input")),
            tabPanel("PC",             tableOutput("iris_table")),
            tabPanel("Variance & PVE", plotOutput("iris_VAR"), 
                     plotOutput("iris_PVE"), plotOutput("iris_cumPVE"), 
                     tableOutput("iris_vartable")))
                 )
                     )
                )

# server
server <- function(input, output, session) {
    selectedData <- reactive({ir.pca.df[c(input$xcol, input$ycol, 'category')]})
    output$pcaplot <- renderPlot({plot(selectedData()[,c(1:2)], main = "PCA analysis",
                                  col = colors[factor(selectedData()$category)], pch = 16)
                                  legend("bottomleft", legend = c('setosa','versicolor','virginica'), 
                                         col = colors, pch = 16)})
    output$caplot <- renderPlot({plot(ca1, main = "CA analysis")})
    output$iris_input <- renderTable(iris)
    output$iris_table <- renderTable(selectedData())
    output$iris_VAR <- renderPlot({plot(pcaVar$Variance, type = 'o', xlab = 'Principal Component', 
                                        ylab = 'Variance', 
                                        pch = 19)})
    output$iris_PVE <- renderPlot({plot(pcaVar$Proportion, type = 'o', xlab = 'Principal Component', 
                                        ylab = 'Proportion of Variance Explained', 
                                        ylim = c(0,1),
                                        pch = 19)})
    output$iris_cumPVE <- renderPlot({plot(pcaVar$Cumulative, type = 'o', xlab = 'Principal Component', 
                                        ylab = 'Cumulative Proportion of Variance Explained', 
                                        ylim = c(0,1),
                                        pch = 19)})
    output$iris_vartable <- renderTable(pcaVar)
}


# Run the application 
shinyApp(ui = ui, server = server)
