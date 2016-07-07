require("shiny")
require("shinysky")
require("shinythemes")
require("devtools")
require("MASS")
require("lattice")
require("datasets")
require("plotly")
require("ggplot2")
require("ggplot2movies")





norm_mean <- 0
norm_sd <- 1

##Intervalo de 

ci_media_amostral <- function(n, p=0.95, pop_mean=0, pop_sd=1) {
  amostra <- rnorm(n, norm_mean, norm_sd)
  amostra_sd <- sd(amostra)
  amostra_mean <- mean(amostra)
  tailprob <- (1 - p) / 2
  q <- -qt(tailprob, df=(n - 1), lower.tail=TRUE)
  se <- amostra_sd / sqrt(n)
  ci <- data.frame(ci_lb = amostra_mean - q * se,
                   ci_ub = amostra_mean + q * se)
  ci
}


shinyServer(function(input, output, session) {
    
    
    
    output$pmf <- renderPlot({
      # draw.plotmath.cell(expression (bgroup("(", atop(x,y), ")")))
      if (!(input$n <= 0 | input$n %% 1 !=0)) n=input$n else n=10
      
      if (input$prob >= 0 & input$prob <=1 & is.numeric(input$prob)) prob = input$prob else prob=0.5
      
      p=seq(0,1,by=0.001)
      par(mfrow=c(1,2))
      plot(0:n, dgeom(0:n,prob=prob), col="blue",pch=16,
           xlab="X (números de tentativas até o primeiro sucesso)", ylab="", main=paste("Função de probabilidade, p=", prob, "n=",n))
      segments(x0=0:n, y0=rep(0, (n+1)), x1=0:n, y1=dgeom(0:n, prob=prob),lwd=2,col="blue")
      abline(h=0)
      X <- rgeom(n=n,prob=prob)
      loglikgeo<-function(p){
        n*log(p) + (sum(X))*log(1-p)}
      plot(p,loglikgeo(p=p), type="l",xlim=c(0,1), ylab="",xlab="p", main=
             paste("Log verossimilhança, \n log(L(p|X=x", ", ", "n=", n, "))", sep="") )
      abline(v=n/(n+sum(X)), col="blue", lty=2)
      abline(v=prob, col="red", lty=2)
      
    })
    
    
    
    
    
    output$norm <- renderPlot({
      
      if (!(input$n1 <= 0 | input$n %% 1 !=0)) n1=input$n else n1=10
      
      if (is.numeric(input$mean)) mean = input$mean else mean=0
      
      if (input$var >= 0 & is.numeric(input$var)) var= input$var else var=1
      
      if (is.numeric(input$x)) x = input$x else x=50
      
      if (is.numeric(input$y)) y = input$y else y=25
      
      if (is.numeric(input$z)) z = input$z else z=80
      
      X<-rnorm(n1,mean,sqrt(var))
      
      MLEnorm <- function(mean,var){
        SumX<-0
        for(i in 1:n1){
          SumX<-SumX + (X[i]-mean)^2
        }
        T <- -(n1/2)*log(2*pi) -(n1/2)*log(var) - (1/(2*var))*(SumX)
        return(T)
      } 
      #estimadores de máxima verossimilhança
      head(X)
      mean(X)
      var(X)
      #Gráfico da função de máxima verossimilhança
      g <- expand.grid(mu = seq(mean-3*var,mean+3*var,length=50) , sigma2 = seq(0.1,2*var,length=50), gr = 1:3)
      g$z <- MLEnorm(mean=g$mu,var=g$sigma2)
      
      wireframe(z ~ mu * sigma2, data = g, groups = gr,
                scales = list(arrows = FALSE),
                drape = TRUE, colorkey = TRUE,
                screen = list( x = x,y= y,z= z))
      
    })
    
    
    Amostrabeta <- reactive({
      
      if (!(input$n2 <= 0 | input$n2 %% 1 !=0)) n2=input$n2 else n2=10
      
      if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
      
      if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
      
      Amostra <- rbeta(n2,Alpha,Beta)
      return(Amostra)
    })
    
    
    
    Estima.rbeta <- reactive({
      
      if (!(input$n2 <= 0 | input$n2 %% 1 !=0)) n2=input$n2 else n2=10
      
      if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
      
      if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
      
      X <- Amostrabeta()
      
      MLE<-function(theta){
        T <- (theta[1]-1)*sum(log(X)) + (theta[2]-1)*sum(log(1-X)) - n2*log(beta(theta[1],theta[2]))
        return(T)
      }
      Xbar<-mean(X)
      Xvar <- var(X)
      chute[1] <- (Xbar*(1-Xbar) - Xvar)*Xbar/Xvar
      chute[2] <- chute[1]*(1-Xbar)/Xbar
      Hat<-optim(par=chute,fn=MLE,method="BFGS",control=list(fnscale=-1))$par
      Hessiana <- optim(par=chute,fn=MLE,method="BFGS",control=list(fnscale=-1),hessian = T)$hessian
      EP<-(-diag(Hessiana))^(-.5)
      
      tabelabeta <- matrix(c(Hat[1],EP[1],Hat[2],EP[2]),nrow=1)
      colnames(tabelabeta) <- c("\u03B1","ep(\u03B1)","\u03B2","ep(\u03B2)")
      row.names(tabelabeta) <- c("\u2023")
      return(tabelabeta)
    })
    
    
    output$estimabeta <- renderTable({
      Estima.rbeta()
      
    })
    
    
    
    
    output$beta <- renderPlot({
      
      
      if (!(input$n2 <= 0 | input$n2 %% 1 !=0)) n2=input$n2 else n2=10
      
      if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
      
      if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
      
      if (is.numeric(input$x1)) x1 = input$x1 else x1=50
      
      if (is.numeric(input$y1)) y1 = input$y1 else y1=25
      
      if (is.numeric(input$z1)) z1 = input$z1 else z1=80
      
      
      library("lattice")
      
      X<-Amostrabeta()
      
      
      MLEwire<-function(Alpha,Beta){
        T <- (Alpha-1)*sum(log(X)) + (Beta-1)*sum(log(1-X)) - n2*log(beta(Alpha,Beta))
        return(T)
      }
      
      g <- expand.grid(alpha = seq(Alpha-3*sd(X),Alpha+3*sd(X),length=50)  , beta = seq(1,2*Beta,length=50), gr = 1:3)
      g$z <- MLEwire(Alpha=g$alpha,Beta=g$beta)
      wireframe(z ~ alpha * beta, data = g, groups = gr,
                scales = list(arrows = FALSE),
                drape = TRUE, colorkey = TRUE,
                screen = list(z = z1, y= y1, x = x1))
      
    })
    
    
    #Rafael 
    
    
    output$plot1 <- renderPlotly({
      
      
      graus <- input$graus
      alpha <-  input$alpha
      x <- seq(-4,4,len=1000)
      y <- dnorm(x)
      
      Linf <- qnorm(alpha/2)
      
      data = data.frame(x=x,y=y)
      dat = rbind(subset(data,x < Linf ),subset(data, x > -Linf  ))
      
      
      #Intervalo para a mÃ©dia  
      
      if(input$Intervpara=="media"){
        if(input$Varia == "Conhecida"){
          gg <- ggplot(data, aes(x=x,y=y)) + 
            theme_bw() +
            geom_line() +
            geom_segment(data = dat, aes(x=x, y = 0, xend =x, yend =y), colour="blue") +
            ggtitle(paste("Quantis te\u00F3ricos sim\u00E9tricos para 1-\u03B1 = ", paste0((1-alpha)*100,"%"), ".\n")) +
            ylab("f(z)") + 
            xlab("z") + 
            annotate('text', x = 0, y = 0.2, label = "1-\u03B1 ",parse = TRUE,size=5)
          
          p <- ggplotly(gg)
          p
          
        }
        else{
          # graus <- input$graus
          y1 <- dt(x,graus-1 ) 
          Linf1 <- qt(alpha/2,graus-1)
          
          data1 = data.frame(x=x,y=y1)
          dat1 = rbind(subset(data1,x <= Linf1 ),subset(data1, x >= -Linf1  ))
          
          gg <- ggplot(data1, aes(x=x,y=y)) +
            theme_bw() + 
            geom_line() +
            geom_segment(data = dat1, aes(x=x, y = 0, xend =x, yend =y), colour="green4") +
            ggtitle(paste("Quantis te\u00F3ricos sim\u00E9tricos para 1-\u03B1 =", paste0((1-alpha)*100,"%"), ".\n")) +
            ylab("t(x)") + 
            xlab("x") + 
            annotate('text', x = 0, y = 0.2, label = "1-\u03B1 ",parse = TRUE,size=5) 
          
          p <- ggplotly(gg)
          p
        }
        
      }
      
      # Intervalo para a variÃ¢ncia  
      
      else{
        
        x2 <- seq( 0 ,50 + graus,len=1000)
        y2 <- dchisq(x2,graus-1)
        Linf2 <- qchisq(alpha/2,graus-1)
        Lsup2 <- qchisq(1-alpha/2,graus-1)
        
        data2 = data.frame(x=x2,y=y2)
        dat2 = rbind(subset(data2,x <= Linf2 ),subset(data2, x >= Lsup2  ))
        
        gg<- ggplot(data2, aes(x=x,y=y)) + 
          theme_bw() +
          geom_line() +
          geom_segment(data = dat2, aes(x=x, y = 0, xend =x, yend =y), colour="red") +
          ggtitle(paste("Quantis te\u00F3ricos sim\u00E9tricos para 1-\u03B1 =", paste0((1-alpha)*100,"%"), ".\n")) +
          ylab("f(x)") 
        
        p <- ggplotly(gg)
        p
        
      }
      
      
      
      
    })
    
    ##################################################################   
    
    AmostraL <- reactive({
      n <- input$graus
      mu <- input$mu
      sigma <- input$sigma
      amostra <- rnorm(n,mu,sigma)
      return(amostra)
    })
    
    
    
    output$ex4 <- renderUI({
      n <- input$graus
      alpha <-  input$alpha
      mu <- input$mu
      sigma <- input$sigma
      amostra <- AmostraL()
      xbar <- round(mean(amostra),2)
      sqrtn <- round(sqrt(n),2)
      sx <- round(sd(amostra),2)
      NIC <- (1-alpha)*100
      #Normal
      
      quantil1 <- round(qnorm(alpha/2,lower.tail = F),2)
      sqsigma <- round(sqrt(sigma),2)
      inf1 <- round(xbar - quantil1*sqsigma/sqrtn,3)
      sup1 <- round(xbar + quantil1*sqsigma/sqrtn,3)
      
      #T-student
      
      quantil2 <- round(qt(alpha/2,n-1,lower.tail = F),2)
      inf2 <- round(xbar - quantil2*sx/sqrtn,3)
      sup2 <- round(xbar + quantil2*sx/sqrtn,3)
      
      #Chi
      
      quantil13 <- round(qchisq(alpha/2,n-1,lower.tail = T),2)
      quantil23 <- round(qchisq(1-alpha/2,n-1,lower.tail = T),2)
      inf3 <- round((n-1)*sx^2/quantil23,3)
      sup3 <- round((n-1)*sx^2/quantil13,3)
      
      
      
      if(input$Intervpara=="media"){
        if(input$Varia == "Conhecida"){
          
          withMathJax(sprintf("$$IC_{%d}(\\mu) = 
                              (%.03f \\pm %.03f \\times \\frac{%.03f}{%.03f} ) = [ %.03f ; %.03f]$$",NIC ,xbar,quantil1,sqsigma,sqrtn,inf1,sup1))
          
        }
        else{
          
          withMathJax(sprintf("$$IC_{%d}(\\mu) = 
                              (%.03f \\pm %.03f \\times \\frac{%.03f}{%.03f} ) = [ %.03f ; %.03f]$$",NIC ,xbar,quantil2,sx,sqrtn,inf2,sup2))
          
        }
        
      }
      
      # Intervalo para a variancia  
      
      else{
        
        
        withMathJax(sprintf("$$IC_{%d}(\\sigma^2) = 
                            (\\frac{%.03f \\times %.03f}{%.03f} ; \\frac{%.03f \\times %.03f}{%.03f}) = [ %.03f ; %.03f]$$",
                            NIC ,n-1,round(sx^2,2),quantil23,n-1,round(sx^2,2),quantil13,inf3,sup3))
        
        
        
      }
        })
    
    
    
    Nomestable <- reactive({
      tabela <- matrix(round(c(summary(AmostraL()),sd(AmostraL())),2),nrow = 1)
      colnames(tabela) <- c("Min","1\u00BA quartil","Mediana","M\u00E9dia","3\u00BA quartil","Max","sd")
      row.names(tabela) <- c("\u2023")
      return(tabela)
    })
    
    
    output$resumo <- renderTable({
      Nomestable()
      
    })
    
    
    
    output$plot2 <- renderPlotly({
      #set.seed(1234)
      n <- input$graus
      alpha <-  input$alpha
      mu <- input$mu
      sigma <- input$sigma
      amostra <- AmostraL()
      data = data.frame(x=amostra)
      bins <- seq(min(amostra), max(amostra), length.out = round(sqrt(n)) + round(sqrt(n)/2))
      
      
      gg <- ggplot(data = data, aes(x=x)) + 
        theme_bw(11)+
        ylab("Densidade") +
        xlab("amostra") +
        ggtitle(paste("Histograma de X \u007E N( \u03BC =",mu,", \u03C3 \u00B2 =",sigma,") \n")) + 
        geom_histogram(aes(y = ..density..),breaks = bins, colour = "white", fill = "purple",size=1)
      
      p <- ggplotly(gg)
      p
      
    })
    
    
    ##################################################################   
    
    
    
    
    sample_ci <- reactive({
      input$draw
      mutate(rdply(input$nsamples, ci_media_amostral(input$amostrasize,
                                                     input$confidence / 100)),
             n = seq_len(input$nsamples),
             contains_mean = ((0 > ci_lb) & (0 < ci_ub)))
    })
    
    output$plot <- renderPlot({
      print(ggplot(sample_ci(), aes(x = n, ymin = ci_lb, ymax = ci_ub,
                                    colour = contains_mean))
            + theme_bw()
            + geom_hline(yintercept = 0, colour="blue")
            + geom_linerange()
            + coord_flip()
            + scale_x_continuous("Amostras")
            + scale_y_continuous(sprintf("CI de %d%%", input$confidence))
            + scale_colour_manual(values = c("red", "black"))
            + theme(legend.position = "none",
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank())
            
      )
    })
    
    output$npct <- renderText({
      c("Porcentagem de intervalos que possuem a m\u00E9dia populacional:", round(mean(sample_ci()$contains_mean) * 100, 2), "%")
    })
    
    
    datasetInput <- reactive({
      switch(input$dataset,
             "crabs" = crabs[,-c(1:3)],
             "anorexia" = anorexia[,-1],
             "genotype" = data.frame(genotype = genotype[,3]),
             "nhtemp"= data.frame(nhtemp=nhtemp),
             "ToothGrowth"=data.frame(ToothGrowth=ToothGrowth[,1])
      )
    })
    
    output$dv = renderUI({
      selectInput('dv', h5('Vari\u00E1vel'), choices = names(datasetInput()))
    })
    
    
    output$summarystats = renderTable({
      if(input$displaystats){
        xb    <- datasetInput()[,input$dv]  
        xb  <- xb[is.na(xb)==FALSE]
        vec = as.numeric(as.character(xb))
        table = t(matrix(c((as.matrix(summary(vec)[1:6])),
                           round(sd(vec,na.rm=TRUE)))))
        
        rownames(table) = input$dv
        
        colnames(table) =  c("Min","1\u00BA quartil","Mediana","M\u00E9dia","3\u00BA quartil","Max","sd")
        return(table)
      }
    })
    
    
    output$sw = renderTable({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      dat=unlist(xb)
      dat1=data.frame(x=as.numeric(as.character(dat)))
      norm = shapiro.test(dat1$x)
      tab = matrix(c(norm$statistic,norm$p.value),nrow=1)
      colnames(tab) = c("Estat\u00EDstica W ","Valor p")
      rownames(tab) = input$dv
      tab
      
    })
    
    
    
    
    
    
    
    # histograms   
    output$distPlot_dv <- renderPlotly({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      n<-length(xb)
      bins <- seq(min(xb), max(xb),length.out = round(sqrt(n)) + round(sqrt(n)/2))
      
      gg <-  ggplot(data = data.frame(x=xb), aes(x) )+
        theme_bw() +
        ylab("")+
        xlab(input$dv)+
        ggtitle(paste("Histograma de",input$dv))+
        geom_histogram(aes(y = ..density..),breaks = bins,colour = "white", fill = "purple")
      
      p <- ggplotly(gg)
      p
      
    })
    
    
    output$intervmed1<- renderUI({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      n <- as.numeric(length(xb))
      alpha1 <- as.numeric(input$alpha1)
      xbar <- round(mean(xb),2)
      quantil <- round(qt(alpha1/2,n-1,lower.tail = F),2)
      sx <- round(sd(xb),2)
      sqrtn <- round(sqrt(n),2)
      inf2 <- round(xbar - quantil*sx/sqrtn,3)
      sup2 <- round(xbar + quantil*sx/sqrtn,3)
      NIC <- (1-alpha1)*100
      
      withMathJax(sprintf("$$IC_{%d}(\\mu) = 
                          (%.03f \\pm %.03f \\times \\frac{%.03f}{%.03f} ) = [ %.03f ; %.03f]$$",NIC ,xbar,quantil,sx,sqrtn,inf2,sup2))
      
      
    })
    
    output$intervarip<- renderUI({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      xbar <- round(mean(xb),2)
      n <- as.numeric(length(xb))
      alpha <- as.numeric(input$alpha1)
      sx2 <- round(var(xb),2)
      NIC <- (1-alpha)*100
      
      quantil13 <- qchisq(alpha/2,n-1,lower.tail = T)
      quantil23 <- qchisq(1-alpha/2,n-1,lower.tail = T)
      
      inter.v1 <- (n-1)*sx2/quantil23
      inter.v2 <- (n-1)*sx2/quantil13
      
      withMathJax(sprintf("$$IC_{%0.f}(\\sigma^2) = 
                          (\\frac{%0.f \\times %.03f  }{%.03f} ; \\frac{%0.f \\times %.03f  }{%.03f} ) = [ %.03f ; %.03f]$$",NIC,n-1, sx2,quantil23,n-1,sx2,quantil13,inter.v1,inter.v2))
      
      
    })
    
    
    })