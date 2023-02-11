#loading packages
require(ggplot2)
require(geiger)
require(phylotools)
require(viridis)
require(lme4)
require(r2glmm)
require(car)
require(gridExtra)
require(ape)
require(nlme)
require(qpcR)
require(betapart)
require(RColorBrewer)
# color palette for graident plot
heatcolor1<-brewer.pal(n=11,name="RdBu")
heatcolor2<-colorRampPalette(heatcolor1)
ncols<-1000
heatcolor3<-heatcolor2(ncols)
## reading data
comm_all<-read.csv("comm_all_filtered.csv")

# beta diveristy
#1. NMDS for all communities at XTBG
data_xtbg<-comm_all[comm_all$Country=="XTBG",]
comm_xtbg<-data_xtbg[,12:17]
tbd_xtbg<-beta.pair.abund(comm_xtbg,index.family = "bray")$beta.bray
#write.csv(as.matrix(tbd_a1),file="tbd_all.csv")
nmds_xtbg<-isoMDS(tbd_xtbg)
nmds_xtbg<-data.frame(nmds1=nmds_xtbg$points[,1],nmds2=nmds_xtbg$points[,2],max_tem=data_xtbg$Monthly_max_tem,Year=data_xtbg$Year)
#write.csv(nmds_a1,file="nmds_all.csv")
#plotting
pdf(file="nmds_xtbg_all_new.pdf",height = 5,width = 5)
ggplot(nmds_xtbg,aes(x=nmds1,y=nmds2,colour=max_tem))+geom_point(size=3)+scale_colour_viridis()+xlim(-.65,.65)+ylim(-.65,.65)+theme(legend.position = c(0.88,0.88))
ggplot(nmds_xtbg,aes(x=nmds1,y=nmds2,colour=max_tem))+geom_point(size=3)+scale_colour_viridis()+xlim(-.65,.65)+ylim(-.65,.65)+theme(legend.position = c(0.88,0.88))
dev.off()


#2. NMDS for all communities
comm_all1<-comm_all[,12:17]
tbd_all<-beta.pair.abund(comm_all1,index.family = "bray")$beta.bray
#write.csv(as.matrix(tbd_a1),file="tbd_all.csv")
nmds_all<-isoMDS(tbd_all)
nmds_all<-data.frame(nmds1=nmds_all$points[,1],nmds2=nmds_all$points[,2],max_tem=comm_all$Monthly_max_tem,locality=comm_all$Country)
#plotting
pdf(file="nmds_all_new.pdf",height = 5,width = 5)
ggplot(nmds_all,aes(x=nmds1,y=nmds2,colour=nmds_all$max_tem,shape=nmds_all$locality))+geom_point(size=3)+scale_colour_viridis()+xlim(-.65,.65)+ylim(-.65,.65)+theme(legend.position = c(0.12,0.82))
dev.off()

#3. NMDS for all communities in liuku
data_liuku<-comm_all[comm_all$Country=="Liuku",]
comm_liuku<-data_liuku[,12:17]
tbd_liuku<-beta.pair.abund(comm_liuku,index.family = "bray")$beta.bray
#write.csv(as.matrix(tbd_a1),file="tbd_all.csv")
nmds_liuku<-isoMDS(tbd_liuku)
nmds_liuku<-data.frame(nmds1=nmds_liuku$points[,1],nmds2=nmds_liuku$points[,2],max_tem=data_liuku$Monthly_max_tem,Year=data_liuku$Year)
#write.csv(nmds_a1,file="nmds_all.csv")
#plotting
pdf(file="nmds_liuku_new.pdf",height = 5,width = 5)
ggplot(nmds_liuku,aes(x=nmds1,y=nmds2,colour=max_tem))+geom_point(size=3)+scale_colour_viridis()+xlim(-.5,.5)+ylim(-.5,.5)+theme(legend.position = c(0.88,0.88))
dev.off()

#3. NMDS for all communities in 2018 (laos, myanmai, xtbg, and thailand)
data_2018<-comm_all[comm_all$Year==2018,]
comm_2018<-data_2018[,12:17]
tbd_2018<-beta.pair.abund(comm_2018,index.family = "bray")$beta.bray
#write.csv(as.matrix(tbd_a1),file="tbd_all.csv")
nmds_2018<-isoMDS(tbd_2018)
nmds_2018<-data.frame(nmds1=nmds_2018$points[,1],nmds2=nmds_2018$points[,2],max_tem=data_2018$Monthly_max_tem,Locality=data_2018$Country)
#write.csv(nmds_a1,file="nmds_all.csv")
#plotting
pdf(file="nmds_2018.pdf",height = 5,width = 5)
ggplot(nmds_2018,aes(x=nmds1,y=nmds2,colour=max_tem,shape=Locality))+geom_point(size=3)+scale_colour_viridis()+xlim(-.5,.5)+ylim(-.5,.5)+theme(legend.position = c(0.88,0.88))
dev.off()

#3. NMDS for all communities in 2013-2014 (Liuku and xtbg)
data_1314<-comm_all[comm_all$Year==2013|comm_all$Year==2014,]
comm_1314<-data_1314[,12:17]
tbd_1314<-beta.pair.abund(comm_1314,index.family = "bray")$beta.bray
#write.csv(as.matrix(tbd_a1),file="tbd_all.csv")
nmds_1314<-isoMDS(tbd_1314)
nmds_1314<-data.frame(nmds1=nmds_1314$points[,1],nmds2=nmds_1314$points[,2],max_tem=data_1314$Monthly_max_tem,Locality=data_1314$Country)
#write.csv(nmds_a1,file="nmds_all.csv")
#plotting
pdf(file="nmds_1314.pdf",height = 5,width = 5)
ggplot(nmds_1314,aes(x=nmds1,y=nmds2,colour=max_tem,shape=Locality))+geom_point(size=3)+scale_colour_viridis()+xlim(-.6,.6)+ylim(-.6,.6)+theme(legend.position = c(0.88,0.88))
dev.off()


###which factors affect the abuandance of fig wasps
## GLMM
# the correlation between each predictor pair
cor(comm_all[,3:6])
#high correlation among temperature varaibles. and prep~ humidity
# select t- and p-related varibles
cor.test(comm_all$Ceratosolen_sp,comm_all$Monthly_ave_Tem)
cor.test(comm_all$Ceratosolen_sp,comm_all$Monthly_max_tem)
cor.test(comm_all$Ceratosolen_sp,comm_all$Monthly_mini_temp)

cor.test(comm_all$Sycophaga_testacea,comm_all$Monthly_ave_Tem)
cor.test(comm_all$Sycophaga_testacea,comm_all$Monthly_max_tem)
cor.test(comm_all$Sycophaga_testacea,comm_all$Monthly_mini_temp)

cor.test(comm_all$Apocrypta_sp,comm_all$Monthly_ave_Tem)
cor.test(comm_all$Apocrypta_sp,comm_all$Monthly_max_tem)
cor.test(comm_all$Apocrypta_sp,comm_all$Monthly_mini_temp)

cor.test(comm_all$Sycophaga_mayri,comm_all$Monthly_ave_Tem)
cor.test(comm_all$Sycophaga_mayri,comm_all$Monthly_max_tem)
cor.test(comm_all$Sycophaga_mayri,comm_all$Monthly_mini_temp)

cor.test(comm_all$Sycophaga_agrensis,comm_all$Monthly_ave_Tem)
cor.test(comm_all$Sycophaga_agrensis,comm_all$Monthly_max_tem)
cor.test(comm_all$Sycophaga_agrensis,comm_all$Monthly_mini_temp)

cor.test(comm_all$Apocrypta_westwoodi,comm_all$Monthly_ave_Tem)
cor.test(comm_all$Apocrypta_westwoodi,comm_all$Monthly_max_tem)
cor.test(comm_all$Apocrypta_westwoodi,comm_all$Monthly_mini_temp)

clim_wc<-read.csv("climate data_all_JAE.csv")
cor.test(clim_wc$Monthly_mini_temp,clim_wc$Monthly_mini_temp_wc)

pdf(file="cor.tmax.pdf",width = 5,height = 5)
ggplot(data=clim_wc,aes(x=clim_wc$Monthly_max_tem,y=clim_wc$Monthly_max_temp_wc))+geom_point(size=3)
dev.off()

pdf(file="cor.tmin.pdf",width = 5,height = 5)
ggplot(data=clim_wc,aes(x=clim_wc$Monthly_mini_temp,y=clim_wc$Monthly_mini_temp_wc))+geom_point(size=3)
dev.off()

pdf(file="cor.p.pdf",width = 5,height = 5)
ggplot(data=clim_wc,aes(x=clim_wc$Monthly_total_Prep,y=clim_wc$Monthly_total_Prep_wc))+geom_point(size=3)
dev.off()

#model building
## pollinator
model_c1<-lmer(Ceratosolen_sp~Monthly_max_tem+I(Monthly_max_tem^2)+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c2<-lmer(Ceratosolen_sp~Monthly_max_tem+I(Monthly_max_tem^2)+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c3<-lmer(Ceratosolen_sp~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_c1,model_c2,model_c3)
#quadratic regression
model_c1<-lmer(Ceratosolen_sp~I(Monthly_max_tem^2)+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c2<-lmer(Ceratosolen_sp~I(Monthly_max_tem^2)+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c3<-lmer(Ceratosolen_sp~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_c1,model_c2,model_c3)
r2beta(model_c1)

## Sycophaga_testacea
model_st1<-lmer(Sycophaga_testacea~I(Monthly_max_tem^2)+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_st2<-lmer(Sycophaga_testacea~I(Monthly_max_tem^2)+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_st3<-lmer(Sycophaga_testacea~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_st1,model_st2,model_st3)
#quadratic regression
model_st1<-lmer(Sycophaga_testacea~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_st2<-lmer(Sycophaga_testacea~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_st3<-lmer(Sycophaga_testacea~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_st1,model_st2,model_st3)
r2beta(model_st1)
summary(model_st1)


#Sycophaga_mayri
model_sm1<-lmer(Sycophaga_mayri~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sm2<-lmer(Sycophaga_mayri~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sm3<-lmer(Sycophaga_mayri~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_sm1,model_sm2,model_sm3)
#quadratic regression
model_sm1<-lmer(Sycophaga_mayri~I(Monthly_max_tem^2)+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sm2<-lmer(Sycophaga_mayri~I(Monthly_max_tem^2)+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sm3<-lmer(Sycophaga_mayri~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_sm1,model_sm2,model_sm3)
r2beta(model_sm1)
summary(model_sm1)

#Sycophaga_agrensis
model_sa1<-lmer(Sycophaga_agrensis~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sa2<-lmer(Sycophaga_agrensis~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sa3<-lmer(Sycophaga_agrensis~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_sa1,model_sa2,model_sa3)
#quadratic regression
model_sa1<-lmer(Sycophaga_agrensis~I(Monthly_max_tem^2)+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sa2<-lmer(Sycophaga_agrensis~I(Monthly_max_tem^2)+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_sa3<-lmer(Sycophaga_agrensis~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_sa1,model_sa2,model_sa3)
r2beta(model_sa1)
summary(model_sa1)

#Apocrypta_westwoodi
model_w1<-lmer(Apocrypta_westwoodi~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_w2<-lmer(Apocrypta_westwoodi~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_w3<-lmer(Apocrypta_westwoodi~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_ap1,model_ap2,model_ap3)
#quadratic regression
model_w1<-lmer(Apocrypta_westwoodi~I(Monthly_max_tem^2)+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_w2<-lmer(Apocrypta_westwoodi~I(Monthly_max_tem^2)+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_w3<-lmer(Apocrypta_westwoodi~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_w1,model_w2,model_w3)
r2beta(model_w1)
summary(model_w1)

#Apocrypta_sp
model_ap1<-lmer(Apocrypta_sp~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ap2<-lmer(Apocrypta_sp~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ap3<-lmer(Apocrypta_sp~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_ap1,model_ap2,model_ap3)
#quadratic regression
model_ap1<-lmer(Apocrypta_sp~I(Monthly_max_tem^2)+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ap2<-lmer(Apocrypta_sp~I(Monthly_max_tem^2)+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ap3<-lmer(Apocrypta_sp~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
AIC(model_ap1,model_ap2,model_ap3)
r2beta(model_ap1)
summary(model_ap1)


#1 for ceratosolen
model_c1<-lmer(Ceratosolen_sp~Monthly_ave_Tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c2<-lmer(Ceratosolen_sp~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c3<-lmer(Ceratosolen_sp~Monthly_mini_temp+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c4<-lmer(Ceratosolen_sp~Monthly_ave_Tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c5<-lmer(Ceratosolen_sp~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c6<-lmer(Ceratosolen_sp~Monthly_mini_temp+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c7<-lmer(Ceratosolen_sp~Monthly_ave_Tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c8<-lmer(Ceratosolen_sp~Monthly_max_tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c9<-lmer(Ceratosolen_sp~Monthly_mini_temp+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c10<-lmer(Ceratosolen_sp~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c11<-lmer(Ceratosolen_sp~Monthly_ave_Tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c12<-lmer(Ceratosolen_sp~Monthly_max_tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c13<-lmer(Ceratosolen_sp~Monthly_mini_temp+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c14<-lmer(Ceratosolen_sp~Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_c15<-lmer(Ceratosolen_sp~GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)

# model selection
AIC(model_c1,model_c2,model_c3,model_c4,model_c5,model_c6,model_c7,model_c8,model_c9,model_c10,model_c11,model_c12,model_c13,model_c14,model_c15)
# best model is model_C1 with AIC= 11271.08
summary(model_c1)
Anova(model_c1)
r2beta(model_c1,partial = T)

#2 for ST
model_ST1<-lmer(Sycophaga_testacea~Monthly_ave_Tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST2<-lmer(Sycophaga_testacea~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST3<-lmer(Sycophaga_testacea~Monthly_mini_temp+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST4<-lmer(Sycophaga_testacea~Monthly_ave_Tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST5<-lmer(Sycophaga_testacea~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST6<-lmer(Sycophaga_testacea~Monthly_mini_temp+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST7<-lmer(Sycophaga_testacea~Monthly_ave_Tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST8<-lmer(Sycophaga_testacea~Monthly_max_tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST9<-lmer(Sycophaga_testacea~Monthly_mini_temp+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST10<-lmer(Sycophaga_testacea~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST11<-lmer(Sycophaga_testacea~Monthly_ave_Tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST12<-lmer(Sycophaga_testacea~Monthly_max_tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST13<-lmer(Sycophaga_testacea~Monthly_mini_temp+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST14<-lmer(Sycophaga_testacea~Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ST15<-lmer(Sycophaga_testacea~GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
# model selection
AIC(model_ST1,model_ST2,model_ST3,model_ST4,model_ST5,model_ST6,model_ST7,model_ST8,model_ST9,model_ST10,model_ST11,model_ST12,model_ST13,model_ST14,model_ST15)
# best model is model_ST5 with AIC= 9226.284
summary(model_ST5)
Anova(model_ST5)
r2beta(model_ST5,partial = T)

#3 for SM
model_SM1<-lmer(Sycophaga_mayri~Monthly_ave_Tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM2<-lmer(Sycophaga_mayri~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM3<-lmer(Sycophaga_mayri~Monthly_mini_temp+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM4<-lmer(Sycophaga_mayri~Monthly_ave_Tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM5<-lmer(Sycophaga_mayri~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM6<-lmer(Sycophaga_mayri~Monthly_mini_temp+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM7<-lmer(Sycophaga_mayri~Monthly_ave_Tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM8<-lmer(Sycophaga_mayri~Monthly_max_tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM9<-lmer(Sycophaga_mayri~Monthly_mini_temp+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM10<-lmer(Sycophaga_mayri~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM11<-lmer(Sycophaga_mayri~Monthly_ave_Tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM12<-lmer(Sycophaga_mayri~Monthly_max_tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM13<-lmer(Sycophaga_mayri~Monthly_mini_temp+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM14<-lmer(Sycophaga_mayri~Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SM15<-lmer(Sycophaga_mayri~GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
# model selection
AIC(model_SM1,model_SM2,model_SM3,model_SM4,model_SM5,model_SM6,model_SM7,model_SM8,model_SM9,model_SM10,model_SM11,model_SM12,model_SM13,model_SM14,model_SM15)
# best model is model_SM7 with AIC= 9628.774
summary(model_SM7)
Anova(model_SM7)
r2beta(model_SM7,partial = T)

#4 for SA
model_SA1<-lmer(Sycophaga_agrensis~Monthly_ave_Tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA2<-lmer(Sycophaga_agrensis~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA3<-lmer(Sycophaga_agrensis~Monthly_mini_temp+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA4<-lmer(Sycophaga_agrensis~Monthly_ave_Tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA5<-lmer(Sycophaga_agrensis~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA6<-lmer(Sycophaga_agrensis~Monthly_mini_temp+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA7<-lmer(Sycophaga_agrensis~Monthly_ave_Tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA8<-lmer(Sycophaga_agrensis~Monthly_max_tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA9<-lmer(Sycophaga_agrensis~Monthly_mini_temp+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA10<-lmer(Sycophaga_agrensis~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA11<-lmer(Sycophaga_agrensis~Monthly_ave_Tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA12<-lmer(Sycophaga_agrensis~Monthly_max_tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA13<-lmer(Sycophaga_agrensis~Monthly_mini_temp+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA14<-lmer(Sycophaga_agrensis~Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_SA15<-lmer(Sycophaga_agrensis~GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
# model selection
AIC(model_SA1,model_SA2,model_SA3,model_SA4,model_SA5,model_SA6,model_SA7,model_SA8,model_SA9,model_SA10,model_SA11,model_SA12,model_SA13,model_SA14,model_SA15)
# best model is model_SA9 with AIC= 7532.577
summary(model_SA9)
Anova(model_SA9)
r2beta(model_SA9,partial = T)

#5 for AW
model_AW1<-lmer(Apocrypta_westwoodi~Monthly_ave_Tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW2<-lmer(Apocrypta_westwoodi~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW3<-lmer(Apocrypta_westwoodi~Monthly_mini_temp+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW4<-lmer(Apocrypta_westwoodi~Monthly_ave_Tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW5<-lmer(Apocrypta_westwoodi~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW6<-lmer(Apocrypta_westwoodi~Monthly_mini_temp+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW7<-lmer(Apocrypta_westwoodi~Monthly_ave_Tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW8<-lmer(Apocrypta_westwoodi~Monthly_max_tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW9<-lmer(Apocrypta_westwoodi~Monthly_mini_temp+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW10<-lmer(Apocrypta_westwoodi~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW11<-lmer(Apocrypta_westwoodi~Monthly_ave_Tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW12<-lmer(Apocrypta_westwoodi~Monthly_max_tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW13<-lmer(Apocrypta_westwoodi~Monthly_mini_temp+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW14<-lmer(Apocrypta_westwoodi~Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_AW15<-lmer(Apocrypta_westwoodi~GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
# model selection
AIC(model_AW1,model_AW2,model_AW3,model_AW4,model_AW5,model_AW6,model_AW7,model_AW8,model_AW9,model_AW10,model_AW11,model_AW12,model_AW13,model_AW14,model_AW15)
# best model is model_AW1 with AIC= 8022.354
summary(model_AW1)
Anova(model_AW1)
r2beta(model_AW1,partial = T)

#6 for ASP
model_ASP1<-lmer(Apocrypta_sp~Monthly_ave_Tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP2<-lmer(Apocrypta_sp~Monthly_max_tem+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP3<-lmer(Apocrypta_sp~Monthly_mini_temp+Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP4<-lmer(Apocrypta_sp~Monthly_ave_Tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP5<-lmer(Apocrypta_sp~Monthly_max_tem+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP6<-lmer(Apocrypta_sp~Monthly_mini_temp+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP7<-lmer(Apocrypta_sp~Monthly_ave_Tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP8<-lmer(Apocrypta_sp~Monthly_max_tem+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP9<-lmer(Apocrypta_sp~Monthly_mini_temp+Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP10<-lmer(Apocrypta_sp~Monthly_total_Prep+GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP11<-lmer(Apocrypta_sp~Monthly_ave_Tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP12<-lmer(Apocrypta_sp~Monthly_max_tem+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP13<-lmer(Apocrypta_sp~Monthly_mini_temp+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP14<-lmer(Apocrypta_sp~Monthly_total_Prep+(1|Year)+(1|Month),data=comm_all,REML=F)
model_ASP15<-lmer(Apocrypta_sp~GPS1+(1|Year)+(1|Month),data=comm_all,REML=F)
# model selection
AIC(model_ASP1,model_ASP2,model_ASP3,model_ASP4,model_ASP5,model_ASP6,model_ASP7,model_ASP8,model_ASP9,model_ASP10,model_ASP11,model_ASP12,model_ASP13,model_ASP14,model_ASP15)
# best model is model_ASP2 with AIC= 8354.918
summary(model_ASP2)
Anova(model_ASP2)
r2beta(model_ASP2,partial = T)






##plotting
#1. fig wasp abundance ~ temperature 
pdf(file="pollinator_abudance_tem.pdf",height = 8,width = 10)
ggplot(aes(x=as.factor(comm_all$Monthly_max_tem), y=comm_all$Ceratosolen_sp, fill=comm_all$Country), data=comm_all)+scale_fill_viridis_d()+geom_boxplot()+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))+ylim(0,3000)
dev.off()

pdf(file="ST_abudance_tem.pdf",height = 8,width = 10)
ggplot(aes(x=as.factor(comm_all$Monthly_max_tem), y=comm_all$Sycophaga_testacea, fill=comm_all$Country), data=comm_all)+scale_fill_viridis_d()+geom_boxplot()+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()

pdf(file="SA_abudance_tem.pdf",height = 8,width = 10)
ggplot(aes(x=as.factor(comm_all$Monthly_max_tem), y=comm_all$Sycophaga_agrensis, fill=comm_all$Country), data=comm_all)+scale_fill_viridis_d()+geom_boxplot()+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()

pdf(file="SM_abudance_tem.pdf",height = 8,width = 10)
ggplot(aes(x=as.factor(comm_all$Monthly_max_tem), y=comm_all$Sycophaga_mayri, fill=comm_all$Country), data=comm_all)+scale_fill_viridis_d()+geom_boxplot()+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()

pdf(file="ASP_abudance_tem.pdf",height = 8,width = 10)
ggplot(aes(x=as.factor(comm_all$Monthly_max_tem), y=comm_all$Apocrypta_sp, fill=comm_all$Country), data=comm_all)+scale_fill_viridis_d()+geom_boxplot()+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()

pdf(file="AW_abudance_tem.pdf",height = 8,width = 10)
ggplot(aes(x=as.factor(comm_all$Monthly_max_tem), y=comm_all$Apocrypta_westwoodi, fill=comm_all$Country), data=comm_all)+scale_fill_viridis_d()+geom_boxplot()+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()

#2 relative abundance of each wasps
ra_wasp<-read.csv("wasp_abundance_average2.csv")
ra_wasp$species <- factor(ra_wasp$species, levels = c("Ceratosolen_sp", "Sycophaga_mayri","Sycophaga_testacea","Sycophaga_agrensis","Apocrypta_sp","Apocrypta_westwoodi"))
## 2.1 for all the galler species. examining the competitive relationship
ra_wasp_gall<-ra_wasp[ra_wasp$species=="Ceratosolen_sp"|ra_wasp$species=="Sycophaga_testacea"|ra_wasp$species=="Sycophaga_mayri",]
pdf(file="re_abudance_galler.pdf",height = 5,width = 5)
ggplot(aes(x=Monthly_max_tem, y=(abundance/sum_wasp),color=species), data=ra_wasp_gall)+scale_color_viridis_d()+geom_line()+geom_point(size=3)+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()
#2.2 for each parasite pairs

ra_wasp_p1<-ra_wasp[ra_wasp$species=="Ceratosolen_sp"|ra_wasp$species=="Sycophaga_agrensis",]
pdf(file="re_abudance_p1.pdf",height = 5,width = 5)
ggplot(aes(x=Monthly_max_tem, y=(abundance/sum_wasp),color=species), data=ra_wasp_p1)+scale_color_viridis_d()+geom_line()+geom_point(size=3)+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()

ra_wasp_p2<-ra_wasp[ra_wasp$species=="Sycophaga_testacea"|ra_wasp$species=="Apocrypta_sp",]
pdf(file="re_abudance_p2.pdf",height = 5,width = 5)
ggplot(aes(x=Monthly_max_tem, y=(abundance/sum_wasp),color=species), data=ra_wasp_p2)+scale_color_viridis_d()+geom_line()+geom_point(size=3)+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()

ra_wasp_p3<-ra_wasp[ra_wasp$species=="Sycophaga_mayri"|ra_wasp$species=="Apocrypta_westwoodi",]
pdf(file="re_abudance_p3.pdf",height = 5,width = 5)
ggplot(aes(x=Monthly_max_tem, y=(abundance/sum_wasp),color=species), data=ra_wasp_p3)+scale_color_viridis_d()+geom_line()+geom_point(size=3)+xlab("Monthly max T")+ylab("Abundance")+theme(legend.position = c(0.15,0.85))
dev.off()



###extrating climate data from worldclim

library(raster)

temp1 <- raster("./wc2.1_2.5m_tmin_2010-2018/wc2.1_2.5m_tmin_2013-10.tif")
temp1

site<-c("xtbg","liuku","laos","Thailand","Myanmar")
lat<-c(21.92707,25.84269, 21.05000, 17.35000,21.98333)
lon<-c(101.26484,98.85453,101.63333,102.71700,96.20000)
samples <- data.frame(site, lon, lat, row.names="site")

temp1 <- raster("./wc2.1_2.5m_prec_2010-2018/wc2.1_2.5m_prec_2018-12.tif")
temp.data <- samples
temp.data_2010$v<- extract(temp1, samples)
temp.data_2010$v

###########2022
ab<-read.csv("0_pollinator_st_abundance.csv")
pdf(file="pollinator_st.pdf",width = 5,height = 5)
ggplot(data=ab,aes(x=ab$pollinator,y=ab$ST))+geom_point(size=2)
dev.off()

pdf(file="pollinator_sm.pdf",width = 5,height = 5)
ggplot(data=ab,aes(x=ab$pollinator,y=ab$sm))+geom_point(size=2)
dev.off()

cor.test(ab$pollinator,ab$ST)
cor.test(ab$pollinator,ab$sm)
