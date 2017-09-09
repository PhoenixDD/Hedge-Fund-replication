library(GA)

monthly_dataset=read.csv("C:/Users/monthly_dataset_bfill_ffill_returns_plus_10.csv",header=T,stringsAsFactors = F)
monthly_dataset=monthly_dataset[-1,]

training_data=monthly_dataset[1:150,3:dim(monthly_dataset)[2]]
training_labels=monthly_dataset[1:150,2]
test_data=monthly_dataset[151:dim(monthly_dataset)[1],3:dim(monthly_dataset)[2]]
test_labels=monthly_dataset[151:dim(monthly_dataset)[1],2]


df=cbind(training_labels,training_data)
linear_model=lm(df)
x=model.matrix(linear_model)
y <- model.response(model.frame(linear_model))

source('C:/Users/factor_method_error.R')

fitness = function(string){
     
     a=25
     b=75
     include=which(string==1)
     X_training=x[,include]
     X_test=x_test[,include]
     #mod=lm.fit(X,y)
     w=c(0.4,0.6)
     r_squared=factor_method_error(X_training,y_training,X_test,y_test)
     fit = w[1]*(1-r_squared) #+ w[2](a *abs(mean(y) - matching_moments_mean(x)) + b* abs(hf_sd - )
     
     return(fit)
     
}


GA <- ga("binary", fitness = fitness, nBits = ncol(x), names = colnames(x), monitor = plot, maxiter=200,popSize=10)

summary(GA)
sum(GA@solution)

predicted_returns=read.csv('C:/Users/test_plus_training_predictions.csv')

plot(monthly_dataset[,2],type='l',col='blue',ylab=c('Returns (In $US)'),xlab=c('Days Since Beginning Of Sampling Period'), main=c('HF Absolute Returns'))
lines(predicted_returns$Predictions,cex=1.2,col='red',pch=3)

legend("topleft",legend=c("HF", "Clone"), col=c("blue","red"),lty=c(1,1))

plot(monthly_dataset[,2],predicted_returns$Predictions,ylab=c('Predicted Returns ($US)'),xlab=c('HF Returns ($US)'), cex=0.75)
text(50,80,paste("cor=",cor(monthly_dataset[,2],predicted_returns$Predictions),sep=""))

error_plot = read.csv('C:/Users/error_train.csv')
plot(error_plot, col='blue',cex=0.1,xlab=c('Iteration'),ylab=('Cost'),main=c('Training Data From Optimally Chosen Portfolio'))
error_plot
