my_theme <- function(base_size =8, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size =8),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size =8),
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f7fdff"),
      strip.background = element_rect(fill = "#001d60", color = "#00113a", size =0.5),
      strip.text = element_text(face = "bold", size = 8, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey5", fill = NA, size = 0.5)
    )
}
theme_set(my_theme())

myfillcolors=c("#ff003f","#0094ff", "#ae00ff" , "#94ff00", "#ffc700","#fc1814",
               "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#000099","#CC0000","#db0229","#026bdb")

mycolors=c("#db0229","#026bdb","#48039e","#0d7502","#c97c02","#c40c09",
           "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
           "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
           "#000099","#CC0000","#ff003f","#0094ff")
#Distribution of the outcome
(all_weather
 %>%gather(Hr_of_day,key="Factors",value="Class")
 %>%ggplot(aes(x=Class,y=Demand,fill=Factors,color=Factors))
           +geom_boxplot(alpha=0.6,width=0.8)
           +coord_flip()+facet_wrap(~Factors,scales="free",ncol=1)
           +scale_fill_manual(values=myfillcolors)
           +scale_color_manual(values=mycolors))
(all_weather
  %>%gather(Hr_of_day,key="Factors",value="Class")
  %>%ggplot(aes(x=Class,y=Demand,fill=Factors,color=Factors))
             +geom_boxplot(alpha=0.6,width=0.8)
             +coord_flip()
             +facet_wrap(~Factors,scales="free",ncol=1)
             +scale_fill_manual(values=myfillcolors)
             +scale_color_manual(values=mycolors))



(all_weather
  %>%gather(Hr_of_day,key="Factors",value="Class")
  %>%ggplot(aes(x=Temperature,y=Demand,fill=Class,color=Class))
  +geom_jitter(shape=21,alpha=0.5,size=3)
  +geom_smooth(method="loess",alpha=0.3)
  +facet_wrap(~Factors,,scales="free",ncol=2)
  +scale_fill_manual(values=myfillcolors)
  +scale_color_manual(values=mycolors))


a1<-ggplot(all_weather,aes(x=Temperature,y=Demand))

print(a1 +geom_jitter(shape=21,alpha=0.5,size=3)
  +geom_smooth(method="loess",alpha=0.3)
  +facet_wrap(~Hr_of_day))

task= makeRegrTask(id = "demand", data=all_weather,target = "Demand")
lrn=makeLearner("regr.glm")
pmi=generateFilterValuesData(task,method="permutation.importance",imp.learner=lrn)%>%.$data%>%ggplot(aes(x=reorder(name,permutation.importance),y=permutation.importance,fill=reorder(name,permutation.importance)))+geom_bar(stat="identity",color="black",show.legend=F)+scale_fill_manual(values=myfillcolors,name="permut.Importance")+scale_x_discrete("Features")

uni=generateFilterValuesData(task,method="univariate.model.score",learner=lrn)%>%.$data%>%ggplot(aes(x=reorder(name,univariate.model.score),y=univariate.model.score,fill=reorder(name,univariate.model.score)))+geom_bar(stat="identity",color="black",show.legend=F)+scale_fill_manual(values=myfillcolors,name="Univar Score")+scale_x_discrete("Features")

gt=generateFilterValuesData(task,method="gain.ratio")%>%.$data%>%ggplot(aes(x=reorder(name,gain.ratio),y=gain.ratio,fill=reorder(name,gain.ratio)))+geom_bar(stat="identity",color="black",show.legend=F)+scale_fill_manual(values=myfillcolors,name="Gain ratio")+scale_x_discrete("Features")

ig=generateFilterValuesData(task,method="information.gain")%>%.$data%>%ggplot(aes(x=reorder(name,information.gain),y=information.gain,fill=reorder(name,information.gain)))+geom_bar(stat="identity",color="black",show.legend=F)+scale_fill_manual(values=myfillcolors,name="information gain")+scale_x_discrete("Features")

csq=generateFilterValuesData(task,method="chi.squared")%>%.$data%>%ggplot(aes(x=reorder(name,chi.squared),y=chi.squared,fill=reorder(name,chi.squared)))+geom_bar(stat="identity",color="black")+scale_fill_manual(values=myfillcolors,name="features")+scale_x_discrete("Features")

grid.arrange(pmi,uni,gt,ig,csq,ncol=3)


rdesc=makeResampleDesc("Bootstrap",iters=100L)
ctrlExh= makeFeatSelControlExhaustive(maxit=20L,max.features=4)

wrap=makeFeatSelWrapper(lrn,rdesc,control=ctrlExh)

modwrap=train(wrap,task)

modwrap$learner.model$opt.result

summary(modwrap$learner.model$next.model$learner.model)