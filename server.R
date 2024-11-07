server <- function(input, output) {

###########################################################################################################################################################################
### mean response curves for two levels with measurements in clients
###########################################################################################################################################################################
  output$responsecurves <- renderPlot({
    beta0.c=input$beta0.c
    beta0.i=input$beta0.i
    beta1.c=input$beta1.c
    beta1.i=input$beta1.i

    var.e=input$var.e
    var.u0=input$var.u0
    var.u1=input$var.u1
    covar.u01=input$covar.u01

    ### time points
    time <- as.numeric(unlist(strsplit(input$timepoints,",")))
    log10time=log10(time+1)
    nr.points=length(time)
    
    ### response
    resp.c=beta0.c+beta1.c*log10time
    resp.i=beta0.i+beta1.i*log10time
    
    ### 95% lower and upper boundaries for control
    var.resp=var.u0+var.u1*log10time^2+2*log10time*covar.u01+var.e
    lower.c=resp.c-1.96*sqrt(var.resp)
    upper.c=resp.c+1.96*sqrt(var.resp)
    
    ### 95% lower and upper boundaries for treatment
    lower.i=resp.i-1.96*sqrt(var.resp)
    upper.i=resp.i+1.96*sqrt(var.resp)
    
    ### limits on vertical axis
    ymin=min(lower.c,lower.i,upper.c,upper.i)
    ymax=max(lower.c,lower.i,upper.c,upper.i)
  
    ### plot means responses for both groups
    control=data.frame(time=time,resp=resp.c,group=rep("control",nr.points))
    treat=data.frame(time=time,resp=resp.i,group=rep("intervention",nr.points))
    dat=rbind(control,treat)
    ggplot(dat, aes(x = time, y = resp, group = group))+geom_line(size=0.75,aes(color=group))+geom_point(aes(shape=group,color=group),size=3)+ylim(ymin,ymax)+xlab("Time")+ylab("Response")
 
  })

###########################################################################################################################################################################
### mean response curves with intervals for two levels with measurements in clients
###########################################################################################################################################################################
output$boundarycurves <- renderPlot({
  beta0.c=input$beta0.c
  beta0.i=input$beta0.i
  beta1.c=input$beta1.c
  beta1.i=input$beta1.i
  
  var.e=input$var.e
  var.u0=input$var.u0
  var.u1=input$var.u1
  covar.u01=input$covar.u01
  
  ### time points
  time <- as.numeric(unlist(strsplit(input$timepoints,",")))
  log10time=log10(time+1)
  nr.points=length(time)
  
  ### response
  resp.c=beta0.c+beta1.c*log10time
  resp.i=beta0.i+beta1.i*log10time
  
  ### 95% lower and upper boundaries for control
  var.resp=var.u0+var.u1*log10time^2+2*log10time*covar.u01+var.e
  lower.c=resp.c-1.96*sqrt(var.resp)
  upper.c=resp.c+1.96*sqrt(var.resp)
  
  ### 95% lower and upper boundaries for treatment
  lower.i=resp.i-1.96*sqrt(var.resp)
  upper.i=resp.i+1.96*sqrt(var.resp)
  
  ### limits on vertical axis
  ymin=min(lower.c,lower.i,upper.c,upper.i)
  ymax=max(lower.c,lower.i,upper.c,upper.i)

  
  ### plot with interval both treatments combined
  control=data.frame(time=time,resp=resp.c,x=c(time,rev(time)),y=c(lower.c,rev(upper.c)),group=rep("control",nr.points))
  treat=data.frame(time=time,resp=resp.i,x=c(time,rev(time)),y=c(lower.i,rev(upper.i)),group=rep("intervention",nr.points))
  dat=rbind(control,treat)
  ggplot(dat, aes(x = x, y = y, fill = group)) + geom_polygon(alpha = 0.5)+geom_line(data=dat,aes(x=time,y=resp),size=0.75)+geom_point(aes(x=time,y=resp,shape=group),size=3)+xlab("Time")+ylab("Response")

  })
  
  
###########################################################################################################################################################################
### Power graph for two levels with measurements in clients
###########################################################################################################################################################################
  
  output$powerplot <- renderPlot({

    alpha=input$alpha
    testtype=as.numeric(input$testtype)
    
    N.min=input$N[1]
    N.max=input$N[2]
    
    N.vector=seq(N.min,N.max)
 
    beta0.c=input$beta0.c
    beta0.i=input$beta0.i
    beta1.c=input$beta1.c
    beta1.i=input$beta1.i
    
    var.e=input$var.e
    var.u0=input$var.u0
    var.u1=input$var.u1
    covar.u01=input$covar.u01

    time <- as.numeric(unlist(strsplit(input$timepoints,",")))
    log10time=log10(time+1)
    nr.points=length(time)
    var.points=var(time)*(nr.points-1)/nr.points # biased variance
    
    var.interaction.N=4*(var.e+nr.points*var.u1*var.points)/(nr.points*N.vector*var.points)
    power.interaction.N=pnorm(abs(beta1.c-beta1.i)/sqrt(var.interaction.N)-qnorm(1-alpha/testtype))
    
    dat=as.data.frame(cbind(N.vector,power.interaction.N))
    ggplot(data=dat, aes(x = N.vector, y = power.interaction.N))+geom_line(aes(x = N.vector, y = power.interaction.N),size=1)+xlab("Number of subjects")+ylab("Power for time*treatment interaction")+scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))
    
  })

###########################################################################################################################################################################
### Power table for two levels with measurements in clients
###########################################################################################################################################################################
  
  output$ResultsTable <- DT::renderDataTable({
    
    alpha=input$alpha
    testtype=as.numeric(input$testtype)
    
    N.min=input$N[1]
    N.max=input$N[2]
    
    N.vector=seq(N.min,N.max,by=2)
    
    beta0.c=input$beta0.c
    beta0.i=input$beta0.i
    beta1.c=input$beta1.c
    beta1.i=input$beta1.i
    
    var.e=input$var.e
    var.u0=input$var.u0
    var.u1=input$var.u1
    covar.u01=input$covar.u01
    
    time <- as.numeric(unlist(strsplit(input$timepoints,",")))
    log10time=log10(time+1)
    nr.points=length(time)
    var.points=var(time)*(nr.points-1)/nr.points # biased variance
    
    var.interaction.N=4*(var.e+nr.points*var.u1*var.points)/(nr.points*N.vector*var.points)
    power.interaction.N=pnorm(abs(beta1.c-beta1.i)/sqrt(var.interaction.N)-qnorm(1-alpha/testtype))
    
    dat=as.data.frame(round(cbind(N.vector,power.interaction.N),2))
    
    colnames(dat) <- c("nr.subjects","power")
    
    datatable(dat)
  
  })
  
  
###########################################################################################################################################################################
### mean response curves for three levels with measurements in clients in therapists
###########################################################################################################################################################################
  output$responsecurves3L <- renderPlot({
    beta0.c=input$beta0.c3L
    beta0.i=input$beta0.i3L
    beta1.c=input$beta1.c3L
    beta1.i=input$beta1.i3L
    
    var.e=input$var.e3L
    var.u0=input$var.u03L
    var.u1=input$var.u13L
    covar.u01=input$covar.u013L
    var.v0=input$var.v03L
    var.v1=input$var.v13L
    covar.v01=input$covar.v013L
    
    ### time points
    time <- as.numeric(unlist(strsplit(input$timepoints3L,",")))
    log10time=log10(time+1)
    nr.points=length(time)
    
    ### response
    resp.c=beta0.c+beta1.c*log10time
    resp.i=beta0.i+beta1.i*log10time
    
    ### 95% lower and upper boundaries for control
    var.resp=var.v0+var.v1*log10time^2+2*log10time*covar.v01+var.u0+var.u1*log10time^2+2*log10time*covar.u01+var.e
    lower.c=resp.c-1.96*sqrt(var.resp)
    upper.c=resp.c+1.96*sqrt(var.resp)
    
    ### 95% lower and upper boundaries for treatment
    lower.i=resp.i-1.96*sqrt(var.resp)
    upper.i=resp.i+1.96*sqrt(var.resp)
    
    ### limits on vertical axis
    ymin=min(lower.c,lower.i,upper.c,upper.i)
    ymax=max(lower.c,lower.i,upper.c,upper.i)
    
    ### plot means responses for both groups
    control=data.frame(time=time,resp=resp.c,group=rep("control",nr.points))
    treat=data.frame(time=time,resp=resp.i,group=rep("intervention",nr.points))
    dat=rbind(control,treat)
    ggplot(dat, aes(x = time, y = resp, group = group))+geom_line(size=0.75,aes(color=group))+geom_point(aes(shape=group,color=group),size=3)+ylim(ymin,ymax)+xlab("Time")+ylab("Response")
    
  })
  
  
###########################################################################################################################################################################
### mean response curves with intervals for three levels with measurements in clients in therapists
###########################################################################################################################################################################
  output$boundarycurves3L <- renderPlot({
    beta0.c=input$beta0.c3L
    beta0.i=input$beta0.i3L
    beta1.c=input$beta1.c3L
    beta1.i=input$beta1.i3L

    var.e=input$var.e3L
    var.u0=input$var.u03L
    var.u1=input$var.u13L
    covar.u01=input$covar.u013L
    var.v0=input$var.v03L
    var.v1=input$var.v13L
    covar.v01=input$covar.v013L
    
    ### time points
    time <- as.numeric(unlist(strsplit(input$timepoints3L,",")))
    log10time=log10(time+1)
    nr.points=length(time)
    
    ### response
    resp.c=beta0.c+beta1.c*log10time
    resp.i=beta0.i+beta1.i*log10time
    
    ### 95% lower and upper boundaries for control
    var.resp=var.v0+var.v1*log10time^2+2*log10time*covar.v01+var.u0+var.u1*log10time^2+2*log10time*covar.u01+var.e
    lower.c=resp.c-1.96*sqrt(var.resp)
    upper.c=resp.c+1.96*sqrt(var.resp)
    
    ### 95% lower and upper boundaries for treatment
    lower.i=resp.i-1.96*sqrt(var.resp)
    upper.i=resp.i+1.96*sqrt(var.resp)
    
    ### limits on vertical axis
    ymin=min(lower.c,lower.i,upper.c,upper.i)
    ymax=max(lower.c,lower.i,upper.c,upper.i)

    ### plot with interval both treatments combined
    control=data.frame(time=time,resp=resp.c,x=c(time,rev(time)),y=c(lower.c,rev(upper.c)),group=rep("control",nr.points))
    treat=data.frame(time=time,resp=resp.i,x=c(time,rev(time)),y=c(lower.i,rev(upper.i)),group=rep("intervention",nr.points))
    dat=rbind(control,treat)
    ggplot(dat, aes(x = x, y = y, fill = group)) + geom_polygon(alpha = 0.5)+geom_line(data=dat,aes(x=time,y=resp),size=0.75)+geom_point(aes(x=time,y=resp,shape=group),size=3)+xlab("Time")+ylab("Response")
    
  })

  
###########################################################################################################################################################################
### Power graph for three levels with measurements in clients in therapists
###########################################################################################################################################################################
  
  output$powerplot3L <- renderPlot({
    
    alpha=input$alpha
    testtype=as.numeric(input$testtype)
    
    N.min=input$N3L[1]
    N.max=input$N3L[2]
    
    N.vector=seq(N.min,N.max) # number of patients per therapist
    K=input$K3L # number of therapists (fixed value)
    
    beta0.c=input$beta0.c3L
    beta0.i=input$beta0.i3L
    beta1.c=input$beta1.c3L
    beta1.i=input$beta1.i3L
    
    var.e=input$var.e3L
    var.u0=input$var.u03L
    var.u1=input$var.u13L
    covar.u01=input$covar.u013L
    var.v0=input$var.v03L
    var.v1=input$var.v13L
    covar.v01=input$covar.v013L
    
    time <- as.numeric(unlist(strsplit(input$timepoints,",")))
    log10time=log10(time+1)
    nr.points=length(time)
    var.points=var(time)*(nr.points-1)/nr.points # biased variance
    
    var.interaction.N=4*(var.e+nr.points*var.u1*var.points+nr.points*N.vector*var.v1*var.points)/(nr.points*N.vector*K*var.points)
    power.interaction.N=pnorm(abs(beta1.c-beta1.i)/sqrt(var.interaction.N)-qnorm(1-alpha/testtype))
    
    dat=as.data.frame(cbind(N.vector,power.interaction.N))
    ggplot(data=dat, aes(x = N.vector, y = power.interaction.N))+geom_line(aes(x = N.vector, y = power.interaction.N),size=1)+xlab("Number of subjects per therapist")+ylab("Power for time*treatment interaction")+scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))
    
    
  })

###########################################################################################################################################################################
### Power table for three levels with measurements in clients in therapists
###########################################################################################################################################################################
  
  output$ResultsTable3L <- DT::renderDataTable({
    
    alpha=input$alpha
    testtype=as.numeric(input$testtype)
    
    N.min=input$N3L[1]
    N.max=input$N3L[2]
    
    N.vector=seq(N.min,N.max) # number of patients per therapist
    K=input$K3L # number of therapists (fixed value)
    
    beta0.c=input$beta0.c3L
    beta0.i=input$beta0.i3L
    beta1.c=input$beta1.c3L
    beta1.i=input$beta1.i3L
    
    var.e=input$var.e3L
    var.u0=input$var.u03L
    var.u1=input$var.u13L
    covar.u01=input$covar.u013L
    var.v0=input$var.v03L
    var.v1=input$var.v13L
    covar.v01=input$covar.v013L
    
    time <- as.numeric(unlist(strsplit(input$timepoints,",")))
    log10time=log10(time+1)
    nr.points=length(time)
    var.points=var(time)*(nr.points-1)/nr.points # biased variance
    
    var.interaction.N=4*(var.e+nr.points*var.u1*var.points+nr.points*N.vector*var.v1*var.points)/(nr.points*N.vector*K*var.points)
    power.interaction.N=pnorm(abs(beta1.c-beta1.i)/sqrt(var.interaction.N)-qnorm(1-alpha/testtype))
    
    dat=as.data.frame(round(cbind(N.vector,power.interaction.N),2))
    
    colnames(dat) <- c("nr.subjects.per.therapist","power")
    
    datatable(dat)
    
  })
  
  
  
}