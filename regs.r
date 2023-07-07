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

ressuf='_rewgt_g10'

# data

  load('input/dt_clean.rda')

  dtagg=data.table(read_excel('input/svar_data.xlsx',sheet='monthly',range='A2:AT386'))
  
    # create lags
  dtagg$year=year(dtagg$date)
  dtagg$month=month(dtagg$date)
  
    Y=dtagg[,.(year,month,`1 year rate`,`Exchange Rate`,`Corporate Spread`,FTSE,GDP,FSScm2,`CPI core`,opec,oil,kanzig,svarshock)]
    
    colnames(Y)=c('year','month','r1yr','FX','corp_spread','FTSE','GDP','FSScm2','CPIc','opec','oil','kanzig','svarshock')
  
    Y$dr=(Y$r1yr-lag(Y$r1yr))/100
    Y$doil=Y$oil-lag(Y$oil)
    
    
    for(x in colnames(Y)[3:ncol(Y)]){
      
      for(l in c(1:12)){
      Y=Y[,paste(x,'_L',l,sep=''):=lag(Y[[x]],l)]
      }
    }    
  
    
    macrovars=c('dr','FX','corp_spread','FTSE','GDP','CPIc')
    macrocontrols=paste(rep(macrovars,12),'_L',kronecker(c(1:12),rep(1,length(macrovars))),sep='')
    
    Xagg=as.matrix(Y[,macrocontrols,with=FALSE])
    
    Y$dgdp=(lead(Y$GDP,12)-Y$GDP)/100
    



  # merge on macro data  
    
    mininc=75*52 # based on real unemployment benefit 
    
    dtm=merge(dt,Y,by.x=c('incyear','incmonth'),by.y=c('year','month'),all.x=TRUE)
    
    dtm=dtm[ytot>mininc &age_dv>=20&age_dv<=65&year<=2019]
    
    dtm$dytothh_H1=ifelse(dtm$dytothh_H1<10,dtm$dytothh_H1,10)
    dtm$dytothh_H2=ifelse(dtm$dytothh_H2<10,dtm$dytothh_H2,10)
    dtm$dytot_H1=ifelse(dtm$dytot_H1<10,dtm$dytot_H1,10)
    dtm$dytot_H2=ifelse(dtm$dytot_H2<10,dtm$dytot_H2,10)
    dtm$dylab_H2=ifelse(dtm$dylab_H2<10,dtm$dylab_H2,10)
    
    # additional cleaning steps
    
    dtmh1=dtm[dint_1 %in% c(10,11,12,13,14)&year>=1991&!(year %in% c(2007,2008,2009))&ytothh>0&ytothh_H1>0&hoh==1&is.na(g10_hhbar)==FALSE]
    dtmh2=dtm[dint_2 %in% c(21,22,23,24,25,26,27)&year>=1991&!(year %in% c(2007,2008,2009))&ytothh>0&ytothh_H2>0&hoh==1&is.na(g10_hhbar)==FALSE]

    dtmh1_ind=dtm[dint_1 %in% c(10,11,12,13,14)&!(year %in% c(2007,2008,2009))&ytot_H1>0&ytot>0&is.na(g10_indbar3)==FALSE]
    dtmh2_ind=dtm[dint_2 %in% c(21,22,23,24,25,26,27)&!(year %in% c(2007,2008,2009))&ytot_H2>0&is.na(g10_indbar3)==FALSE]
    
    dtmh2_indlab=dtm[dint_2 %in% c(21,22,23,24,25,26,27)&!(year>=1991&year %in% c(2007,2008,2009))&ylab>mininc&is.na(g10_indbar3)==FALSE]
  

      wgtrebal<-function(dt){ # rebalance to above to give each year same weight and have 10% weight in each income bucket by year
        
        dtw=dt[,.(wyear=sum(indin_lw,na.rm=TRUE),
                  wyearx=sum(indin_xw,na.rm=TRUE)),by=.(year,g10_ind)]
        
        dtw$wyear=1000/dtw$wyear
        dtw$wyearx=1000/dtw$wyearx
        
          
        dt=merge(dt,dtw,by=c('year','g10_ind'),all.x=TRUE)
        
        dt$indin_lwnorm=dt$indin_lw*dt$wyear
        dt$indin_xwnorm=dt$indin_xw*dt$wyearx
        
        minxw=quantile(dt$indin_xwnorm[dt$indin_xwnorm>0],0.0,na.rm=TRUE)
        minlw=quantile(dt$indin_lwnorm[dt$indin_lwnorm>0],0.0,na.rm=TRUE)
        
        dt$indin_xwnorm[dt$indin_xwnorm<minxw]=minxw
        dt$indin_lwnorm[dt$indin_lwnorm<minlw]=minlw
        
      return(dt)        
      }
      
      dtmh1=wgtrebal(dtmh1)
      dtmh2=wgtrebal(dtmh2)
      dtmh1_ind=wgtrebal(dtmh1_ind)
      dtmh2_ind=wgtrebal(dtmh2_ind)
      dtmh2_indlab=wgtrebal(dtmh2_indlab)
      
      dtmh1$wgt_sel=dtmh1$indin_lwnorm
      dtmh2$wgt_sel=dtmh2$indin_lwnorm
      dtmh1_ind$wgt_sel=dtmh1_ind$indin_lwnorm
      dtmh2_ind$wgt_sel=dtmh2_ind$indin_lwnorm
      dtmh2_indlab$wgt_sel=dtmh2_indlab$indin_lwnorm
      
      
      
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
      
      
    # macro controls
    
    Xmh1=as.matrix(dtmh1[,macrocontrols,with=FALSE])
    Xmh2=as.matrix(dtmh2[,macrocontrols,with=FALSE])
    Xmh1_ind=as.matrix(dtmh1_ind[,macrocontrols,with=FALSE])
    Xmh2_ind=as.matrix(dtmh2_ind[,macrocontrols,with=FALSE])
    Xmh2_indlab=as.matrix(dtmh2_indlab[,macrocontrols,with=FALSE])
    
    # macro specifciation
    
    magg24=ivreg(data=Y,lead(GDP,24)~dr+Xagg,~FSScm2+Xagg)
    V.magg24=vcovHAC(magg24)
    magg.coef24=coeftest(magg24,V.magg24)
    
    magg241s=lm(data=Y,lead(GDP,24)~FSScm2+dr+Xagg+FSScm2_L1+FSScm2_L2+FSScm2_L3)
    V.magg241s=vcovHAC(magg241s)
    magg.coef241s=coeftest(magg241s,V.magg241s)
    
    magg12oil=ivreg(data=Y,lead(GDP,12)~doil+Xagg,~kanzig+Xagg)
    V.magg12oil=vcovHAC(magg12oil)
    magg.coef12oil=coeftest(magg12oil,V.magg12oil)
    
    
    q2=c(20,40,60,80,90,99)-5
    q10=c(10,20,30,40,50,60,70,80,90,100)-5
    
    q=q2
    
    
  # initial check on correlation with gdp
    
    m.dgdp=summary(lm(data=dtmh1,dytothh_H1~dgdp,weights = wgt_sel))

  # 1 year horizon regression on total household income
    
    m.h1.ytothh=ivreg(data=dtmh1,dytothh_H1~dr*factor(g10_hhbar)+Xmh1,~FSScm2*factor(g10_hhbar)+Xmh1,weights = wgt_sel)
    V.h1.ytothh=vcovCL(m.h1.ytothh,~wave)
    coef.h1.ytothh=coeftest(m.h1.ytothh,V.h1.ytothh)
    fg.h1.ytothh=fg.hor(coef.h1.ytothh,V.h1.ytothh,'dr',q10,'Household Income: 12 months')
    
  # 2 year horizon regression on total household income
    
    #m.h2.ytothh=ivreg(data=dtmh2,log(ytothh_H2)~dr*factor(g2_indbar3)+log(ytothh)+Xmh2,~FSScm2*factor(g2_indbar3)+log(ytothh)+Xmh2,weights = wgt_sel)
    m.h2.ytothh=lm(data=dtmh2,log(ytothh_H2)~svarshock*factor(g2_indbar3)+log(ytothh)+Xmh2)
    V.h2.ytothh=vcovCL(m.h2.ytothh,~wave)
    coef.h2.ytothh=coeftest(m.h2.ytothh,V.h2.ytothh)
    fg.h2.ytothh=fg.hor(coef.h2.ytothh,V.h2.ytothh,'svarshock',q2,'Household Income: 24 months')
    
  # 2 year horizon regression on individual income
    
    m.h2.ytot=ivreg(data=dtmh2_ind,log(ytot_H2)~dr*factor(g10_indbar3)+Xmh2_ind+log(ytot),~FSScm2*factor(g10_indbar3)+log(ytot)+Xmh2_ind)
    V.h2.ytot=vcovCL(m.h2.ytot,~wave)
    coef.h2.ytot=coeftest(m.h2.ytot,V.h2.ytot)
    fg.h2.ytot=fg.hor(coef.h2.ytot,V.h2.ytot,'dr',q10,'Individual Income: 24 months')

    # 1 year horizon regression on individual income [oil]
    
    m.h1.ytot.oil=ivreg(data=dtmh1_ind,dytot_H1~doil*factor(g10_indbar3)+Xmh1_ind,~kanzig*factor(g10_indbar3)+Xmh1_ind,weights = wgt_sel)
    V.h1.ytot.oil=vcovCL(m.h1.ytot.oil,~wave)
    coef.h1.ytot.oil=coeftest(m.h1.ytot.oil,V.h1.ytot.oil)
    fg.h1.ytot.oil=fg.hor(coef.h1.ytot.oil,V.h1.ytot.oil,'doil',q10,'Individual Income [oil]: 12 months')
        
  # 2 year horizon regression on individual income [oil]
    
    m.h2.ytot.oil=ivreg(data=dtmh2_ind,dytot_H2~doil*factor(g10_indbar3)+Xmh2_ind,~kanzig*factor(g10_indbar3)+Xmh2_ind,weights = wgt_sel)
    V.h2.ytot.oil=vcovCL(m.h2.ytot.oil,~wave)
    coef.h2.ytot.oil=coeftest(m.h2.ytot.oil,V.h2.ytot.oil)
    fg.h2.ytot.oil=fg.hor(coef.h2.ytot.oil,V.h2.ytot.oil,'doil',q10,'Individual Income [oil]: 24 months')
    
  # 2 year horizon regression on individual labour income
    
    m.h2.ylab=ivreg(data=dtmh2_indlab,dylab_H2~dr*factor(g10_indbar3)+Xmh2_indlab,~FSScm2*factor(g10_indbar3)+Xmh2_indlab,weights = wgt_sel)
    m.h2.ylab=lm(data=dtmh2_indlab,dylab_H2~svarshock*factor(g2_indbar3)+Xmh2_indlab)
    V.h2.ylab=vcovCL(m.h2.ylab,~wave)
    coef.h2.ylab=coeftest(m.h2.ylab,V.h2.ylab)
    fg.h2.ylab=fg.hor(coef.h2.ylab,V.h2.ylab,'dr',q2,'Individual Income [labour]: 24 months')
    
    # 1 year horizon employment probability
    
    m.h1.E=ivreg(data=dtmh1,E_H1~dr*factor(g10_indbar3)+Xmh1,~FSScm2*factor(g10_indbar3)+Xmh1,weights = wgt_sel)
    V.h1.E=vcovCL(m.h1.E,~wave)
    coef.h1.E=coeftest(m.h1.E,V.h1.E)
    fg.h1.E=fg.hor(coef.h1.E,V.h1.E,'dr',q10,'Employment probability: 12 months')
    
    # 2 year horizon employment probability
    
    m.h2.E=ivreg(data=dtmh2,E_H2~dr*factor(g10_indbar3)+Xmh2,~FSScm2*factor(g10_indbar3)+Xmh2,weights = wgt_sel)
    V.h2.E=vcovCL(m.h2.E,~wave)
    coef.h2.E=coeftest(m.h2.E,V.h2.E)
    fg.h2.E=fg.hor(coef.h2.E,V.h2.E,'dr',q10,'Employment probability: 24 months')
    
    
    # 1 year horizon unemployment probability
    
    m.h1.E=ivreg(data=dtmh1,U_H1~dr*factor(g10_indbar3)+Xmh1,~FSScm2*factor(g10_indbar3)+Xmh1,weights = wgt_sel)
    V.h1.E=vcovCL(m.h1.E,~wave)
    coef.h1.E=coeftest(m.h1.E,V.h1.E)
    fg.h1.E=fg.hor(coef.h1.E,V.h1.E,'dr',q10,'Employment probability: 12 months')
    
    # 2 year horizon unemployment probability
    
    m.h2.E=ivreg(data=dtmh2,E_H2~dr*factor(g10_indbar3)+Xmh2,~FSScm2*factor(g10_indbar3)+Xmh2,weights = wgt_sel)
    V.h2.E=vcovCL(m.h2.E,~wave)
    coef.h2.E=coeftest(m.h2.E,V.h2.E)
    fg.h2.E=fg.hor(coef.h2.E,V.h2.E,'dr',q10,'Employment probability: 24 months')
    
    # save figures
    ggsave(paste('output/h2_ytothh',ressuf,'.pdf',sep=''),fg.h2.ytothh,units = 'cm',width=15,height=12)
    ggsave(paste('output/h2_ytot',ressuf,'.pdf',sep=''),fg.h2.ytot,units = 'cm',width=15,height=12)
    ggsave(paste('output/h2_ylab',ressuf,'.pdf',sep=''),fg.h2.ylab,units = 'cm',width=15,height=12)
    ggsave(paste('output/h1_E',ressuf,'.pdf',sep=''),fg.h1.E,units = 'cm',width=15,height=12)
    ggsave(paste('output/h2_E',ressuf,'.pdf',sep=''),fg.h2.E,units = 'cm',width=15,height=12)
    ggsave(paste('output/h1_ytot_oil',ressuf,'.pdf',sep=''),fg.h1.ytot.oil,units = 'cm',width=15,height=12)
    ggsave(paste('output/h2_ytot_oil',ressuf,'.pdf',sep=''),fg.h2.ytot.oil,units = 'cm',width=15,height=12)
    
    # transition matrix
    
    PIg2=matrix(rep(0,6*6),nrow=6,ncol=6)
    
    i=0
    for(j in c(20,40,60,80,90,99)){
      i=i+1
      PIg2[i,]=as.numeric(table(dtmh1$g2_hh_H1[dtmh1$g2_hhbar==j])/sum(table(dtmh1$g2_hh_H1[dtmh1$g2_hh==j])))
    }
    
    
    PIg10=matrix(rep(0,10*10),nrow=10,ncol=10)
    
    i=0
    for(j in c(10,20,30,40,50,60,70,80,90,99)){
      i=i+1
      PIg10[i,]=as.numeric(table(dtmh1$g10_hh_H1[dtmh1$g10_hhbar==j])/sum(table(dtmh1$g10_hh_H1[dtmh1$g10_hh==j])))
    }
    
    
    dtmagg=dtm[,.(y=sum(ytot*indin_xw,na.rm = FALSE)/sum(indin_xw,na.rm = FALSE)),by=.(incyear,incqtr)]
    
    
    