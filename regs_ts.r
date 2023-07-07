rm(list=ls())

require(data.table)
require(sandwich)
require(lmtest)
require(ivreg)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(reshape2)
require(dplyr)
require(stringr)
require(readxl)

setwd('~/OneDrive/Documents/research/BHPS/MP_Inequality/')


q2=c(20,40,60,80,90,99)-5
q10=c(10,20,30,40,50,60,70,80,90,100)-5

ressuf='_annagg'
# data

load('input/dt_clean.rda')

dtagg=data.table(read_excel('input/svar_data.xlsx',sheet='monthly',range='A2:AT386'))

# create lags
dtagg$year=year(dtagg$date)
dtagg$month=month(dtagg$date)

Y=dtagg[,.(year,month,`1 year rate`,`Exchange Rate`,`Corporate Spread`,FTSE,GDP,FSScm2,`CPI core`,opec,oil,kanzig,svarshock)]

colnames(Y)=c('year','month','r1yr','FX','corp_spread','FTSE','GDP','FSScm2','CPIc','opec','oil','kanzig','svarshock')

Y$dr=Y$r1yr-lag(Y$r1yr)
Y$doil=Y$oil-lag(Y$oil)
Y$dFTSE=Y$FTSE-lag(Y$FTSE)
Y$dcorp_spread=Y$corp_spread-lag(Y$corp_spread)
Y$dFX=Y$FX-lag(Y$FX)


#Y$svarshock=Y$kanzig

# first stage stuff (relationship between shocks and variable of agg variable)

m.dr=summary(lm(data=Y,dr~svarshock))

m.doil=summary(lm(data=Y,doil~kanzig))
m.doil2=summary(lm(data=Y,doil~opec))

# annual dataset
 
Ya=Y[,.(opec=sum(opec),kanzig=sum(kanzig),
        svarshock=sum(svarshock)/100,
        FSScm2=sum(FSScm2)/100,
        oil=log(mean(exp(oil/100))),
       doil=sum(doil)/100,
       dFTSE=sum(dFTSE)/100,
       dFX=sum(dFX)/100,
       dcorp_spread=sum(dcorp_spread)/100,
       dr=sum(dr)/100,
     gdp=log(mean(exp(GDP/100)))),by=year]

# some lags
for(x in colnames(Ya)[2:ncol(Ya)]){
  
  for(l in c(1:3)){
    Ya=Ya[,paste(x,'_L',l,sep=''):=lag(Ya[[x]],l)]
  }
}    

Ya$dgdp=Ya$gdp-lag(Ya$gdp)
Ya$dgdpF1=lead(Ya$gdp)-Ya$gdp
Ya$dgdpF2=lead(Ya$gdp,2)-Ya$gdp

Ya$gdpF1=lead(Ya$gdp,1)
Ya$gdpF2=lead(Ya$gdp,2)

# macro regs
macrovars=c('dFX','dcorp_spread','dFTSE','gdp','dr')

macrocontrols=paste(rep(macrovars,2),'_L',kronecker(c(1:2),rep(1,length(macrovars))),sep='')

Xagg=as.matrix(Ya[,c(macrovars,macrocontrols),with=FALSE])

m.gdpF1=summary(lm(data=Ya,gdpF1~svarshock+Xagg))
m.gdpF2=summary(lm(data=Ya,gdpF2~svarshock+Xagg))

mininc=5000 # based on real unemployment benefit 

dtm=dt[ytot>mininc &age_dv>=20&age_dv<=65&year<=2019]


# additional cleaning steps

dtmh2_ind=dtm[year>=1991&ytot_H2>0&is.na(g2_indbar3)==FALSE&is.na(indin_lw)==FALSE
              &is.na(ytot_H1)==FALSE&is.na(ytot_H2)==FALSE&is.na(E_H2)==FALSE&is.na(E_H1)==FALSE&is.na(E)==FALSE&
                !(year %in% c(2007,2008,2009))]

# aggregate

dtmh2_agg=dtmh2_ind[,.(y0=sum(ytot*indin_lw)/sum(indin_lw),
                       y1=sum(ytot_H1*indin_lw)/sum(indin_lw),
                 y2=sum(ytot_H2*indin_lw)/sum(indin_lw),
                 y0lab=sum(ylab*indin_lw)/sum(indin_lw),
                 y1lab=sum(ylab_H1*indin_lw)/sum(indin_lw),
                 y2lab=sum(ylab_H2*indin_lw)/sum(indin_lw),
                 E=sum(E*indin_lw)/sum(indin_lw),
                 E_H1=sum(E_H1*indin_lw)/sum(indin_lw),
                 E_H2=sum(E_H2*indin_lw)/sum(indin_lw),
                 U=sum(U*indin_lw)/sum(indin_lw),
                 U_H1=sum(U_H1*indin_lw)/sum(indin_lw),
                 U_H2=sum(U_H2*indin_lw)/sum(indin_lw),
                 .N),by=.(year,g2_indbar3)]


dtmh2_agg=merge(dtmh2_agg,Ya,by.x=c('year'),by.y=c('year'),all.x=TRUE)

dtmh2_agg$dytot_H1=log(dtmh2_agg$y1/dtmh2_agg$y0)
dtmh2_agg$dytot_H2=log(dtmh2_agg$y2/dtmh2_agg$y0)

X=as.matrix(dtmh2_agg[,c(macrocontrols),with=FALSE])

dtmh2_aggHH=dtmh2_ind[hoh==1,.(y0=sum(ytot*indin_lw)/sum(indin_lw),
                       y1=sum(ytot_H1*indin_lw)/sum(indin_lw),
                       y2=sum(ytot_H2*indin_lw)/sum(indin_lw),
                       y0hh=sum(ytothh*indin_lw)/sum(indin_lw),
                       y1hh=sum(ytothh_H1*indin_lw)/sum(indin_lw),
                       y2hh=sum(ytothh_H2*indin_lw)/sum(indin_lw),
                       E=sum(E*indin_lw)/sum(indin_lw),
                       E_H1=sum(E_H1*indin_lw)/sum(indin_lw),
                       E_H2=sum(E_H2*indin_lw)/sum(indin_lw),.N),by=.(year,g2_indbar3)]


dtmh2_aggHH=merge(dtmh2_aggHH,Ya,by.x=c('year'),by.y=c('year'),all.x=TRUE)

Xhh=as.matrix(dtmh2_aggHH[,c(macrovars,macrocontrols),with=FALSE])


# data checks

m.dgdp1=summary(lm(data=dtmh2_agg,dytot_H1~dgdpF1))
m.dgdp2=summary(lm(data=dtmh2_agg,dytot_H2~dgdpF2))

# 
fg.ts=ggplot(data=dtmh2_agg)+geom_point(aes(x=year,y=y0,color=factor(g2_indbar3)))

# Figures

fg.hor<-function(coef,V,x,q,tit=''){
  
  xloc=c(grep(x,row.names(coef))[1],grep(paste(x,':',sep=''),row.names(coef)))
  
  pe=coef[xloc,1]+coef[xloc[1],1]
  pe[1]=coef[xloc[1],1]
  
  varx=diag(V)[xloc] + V[xloc[1],xloc[1]] + 2*V[xloc[1],xloc]
  varx[1]=V[xloc[1],xloc[1]] 
  
  l68=pe-sqrt(varx)*qnorm(0.84)
  l90=pe-sqrt(varx)*qnorm(0.95)
  u68=pe+sqrt(varx)*qnorm(0.84)
  u90=pe+sqrt(varx)*qnorm(0.95)
  
  dtfg=data.frame(q,pe,l68,l90,u68,u90)  
  
  fg=ggplot(data=dtfg)+geom_line(aes(x=q,y=pe),color='darkblue',linewidth=1.2)+
    geom_ribbon(aes(x=q,ymin=l68,ymax=u68),fill='blue',alpha=0.2)+
    geom_ribbon(aes(x=q,ymin=l90,ymax=u90),fill='blue',alpha=0.1)+
    theme_minimal(base_size=12)+labs(x='Income Percentile',y='',title=tit)
  
  return(fg) 
}


# individual income

  m.h1.ytot=lm(data=dtmh2_agg,log(y1)~log(y0)+svarshock*factor(g2_indbar3)+X)
  V.h1.ytot=vcovHAC(m.h1.ytot)
  coef.h1.ytot=coeftest(m.h1.ytot,V.h1.ytot)
  fg.h1.ytot=fg.hor(coef.h1.ytot,V.h1.ytot,'svarshock',q2,'Individual Income: 12 months')
  ggsave(paste('output/h1_ytot',ressuf,'.pdf',sep=''),fg.h1.ytot,units = 'cm',width=15,height=12)

  m.h2.ytot=lm(data=dtmh2_agg,log(y2)~log(y0)+svarshock*factor(g2_indbar3)+X)
  V.h2.ytot=vcovHAC(m.h2.ytot)
  coef.h2.ytot=coeftest(m.h2.ytot,V.h2.ytot)
  fg.h2.ytot=fg.hor(coef.h2.ytot,V.h2.ytot,'svarshock',q2,'Individual Income: 24 months')
  ggsave(paste('output/h2_ytot',ressuf,'.pdf',sep=''),fg.h2.ytot,units = 'cm',width=15,height=12)
  
  # labour income
  
  m.h1.ylab=lm(data=dtmh2_agg,log(y1)~log(y0)+svarshock*factor(g2_indbar3)+X)
  V.h1.ylab=vcovHAC(m.h1.ylab)
  coef.h1.ylab=coeftest(m.h1.ylab,V.h1.ylab)
  fg.h1.ylab=fg.hor(coef.h1.ylab,V.h1.ylab,'svarshock',q2,'Labour Income: 12 months')
  ggsave(paste('output/h1_ylab',ressuf,'.pdf',sep=''),fg.h1.ylab,units = 'cm',width=15,height=12)
  
  m.h2.ylab=lm(data=dtmh2_agg,log(y2)~log(y0)+svarshock*factor(g2_indbar3)+X)
  V.h2.ylab=vcovHAC(m.h2.ylab)
  coef.h2.ylab=coeftest(m.h2.ylab,V.h2.ylab)
  fg.h2.ylab=fg.hor(coef.h2.ylab,V.h2.ylab,'svarshock',q2,'Labour Income: 24 months')
  ggsave(paste('output/h2_ylab',ressuf,'.pdf',sep=''),fg.h2.ylab,units = 'cm',width=15,height=12)  
  
  # emploment
  
  m.h1.E=lm(data=dtmh2_agg,E_H1~svarshock*factor(g2_indbar3)+E+X)
  V.h1.E=vcovHAC(m.h1.E)
  coef.h1.E=coeftest(m.h1.E,V.h1.E)
  fg.h1.E=fg.hor(coef.h1.E,V.h1.E,'svarshock',q2,'Employment probability: 12 months')
  ggsave(paste('output/h1_E',ressuf,'.pdf',sep=''),fg.h1.E,units = 'cm',width=15,height=12)
  
  m.h2.E=lm(data=dtmh2_agg,E_H2~svarshock*factor(g2_indbar3)+E+X)
  V.h2.E=vcovHAC(m.h2.E)
  coef.h2.E=coeftest(m.h2.E,V.h2.E)
  fg.h2.E=fg.hor(coef.h2.E,V.h2.E,'svarshock',q2,'Employment probability: 24 months')
  ggsave(paste('output/h2_E',ressuf,'.pdf',sep=''),fg.h2.E,units = 'cm',width=15,height=12)
  
  # unemployment
  
  m.h1.U=lm(data=dtmh2_agg,U_H1~svarshock*factor(g2_indbar3)+E+X)
  V.h1.U=vcovHAC(m.h1.U)
  coef.h1.U=coeftest(m.h1.U,V.h1.U)
  fg.h1.U=fg.hor(coef.h1.U,V.h1.U,'svarshock',q2,'Unemployment probability: 12 months')
  ggsave(paste('output/h1_U',ressuf,'.pdf',sep=''),fg.h1.U,units = 'cm',width=15,height=12)
  
  m.h2.U=lm(data=dtmh2_agg,U_H2~svarshock*factor(g2_indbar3)+E+X)
  V.h2.U=vcovHAC(m.h2.U)
  coef.h2.U=coeftest(m.h2.U,V.h2.U)
  fg.h2.U=fg.hor(coef.h2.U,V.h2.U,'svarshock',q2,'Unemployment probability: 24 months')
  ggsave(paste('output/h2_U',ressuf,'.pdf',sep=''),fg.h2.U,units = 'cm',width=15,height=12)
  
  # household income
  
  m.h1.ytothh=lm(data=dtmh2_aggHH,log(y1hh)~log(y0hh)+svarshock*factor(g2_indbar3)+X)
  V.h1.ytothh=vcovHAC(m.h1.ytothh)
  coef.h1.ytothh=coeftest(m.h1.ytothh,V.h1.ytothh)
  fg.h1.ytothh=fg.hor(coef.h1.ytothh,V.h1.ytothh,'svarshock',q2,'Household Income: 12 months')
  ggsave(paste('output/h1_ytothh',ressuf,'.pdf',sep=''),fg.h1.ytothh,units = 'cm',width=15,height=12)
  
  m.h2.ytothh=lm(data=dtmh2_aggHH,log(y2hh)~log(y0hh)+svarshock*factor(g2_indbar3)+X)
  V.h2.ytothh=vcovHAC(m.h2.ytothh)
  coef.h2.ytothh=coeftest(m.h2.ytothh,V.h2.ytothh)
  fg.h2.ytothh=fg.hor(coef.h2.ytothh,V.h2.ytothh,'svarshock',q2,'Houeshold Income: 24 months')
  ggsave(paste('output/h2_ytothh',ressuf,'.pdf',sep=''),fg.h2.ytothh,units = 'cm',width=15,height=12)