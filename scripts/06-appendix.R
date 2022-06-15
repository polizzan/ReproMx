###########
#APPENDIX#
#########

#Appendix 1:
#projection accuracy
#projected vs. registered births
A1a <-
  ggplot() +
  geom_line(aes(x=2010:2019, y=Births.US.2010to2019$Births/1000, 
                color="Registered", linetype="Registered"), size=0.6) +
  geom_line(aes(x=2010:2019, y=colSums(Births.US.12to55.2010to2019.observed/1000), 
                color="Projected", linetype="Projected"), size=0.6) +
  ggtitle("Number of Births (in Thousands)") +
  xlab(" ") +
  ylab(" ") +
  labs(color=" ",
       linetype=" ") +
  scale_x_continuous(limits=c(2010, 2020),
                     breaks=(seq(2010, 2020, 2)),
                     minor_breaks=(seq(2011, 2019, 2)),
                     guide="prism_minor") +
  scale_y_continuous(limits=c(3700, 4250),
                     breaks=(seq(3700, 4250, 50)),
                     labels=c("3,700", "3,750",
                              "3,800", "3,850",
                              "3,900", "3,950",
                              "4,000", "4,050",
                              "4,100", "4,150",
                              "4,200", "4,250")) +
  scale_color_manual(values=c("Registered"="grey60",
                              "Projected"="black")) +
  scale_linetype_manual(values=c("Registered"=1,
                                 "Projected"=3)) +  
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,
                                 hjust=0.95,
                                 size=12),
        axis.text.y=element_text(size=12), 
        plot.title=element_text(face="bold",
                                hjust=0.5,
                                size=16,
                                margin=margin(t=0, r=0, b=10, l=0)),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        legend.position="bottom",
        legend.key.width=unit(1, 'cm'),
        legend.text=element_text(size=10), 
        text=element_text(family="serif"))

#projected minus registered births
A1b <-
  ggplot() +
  geom_line(aes(x=2010:2019, 
                y=(colSums(Births.US.12to55.2010to2019.observed)-Births.US.2010to2019$Births)/1000, 
                color="Projected \226 Registered", 
                linetype="Projected \226 Registered"), size=0.6) +
  ggtitle("Projection Error (in Thousands)") +
  xlab(" ") +
  ylab(" ") + 
  labs(color=" ",
       linetype=" ") +
  scale_x_continuous(limits=c(2010, 2020),
                     breaks=(seq(2010, 2020, 2)),
                     minor_breaks=(seq(2011, 2019, 2)),
                     guide="prism_minor") +
  scale_y_continuous(limits=c(0, 20),
                     breaks=(seq(0, 20, 5)),
                     minor_breaks=(seq(0, 20, 1)),
                     guide="prism_minor") +
  scale_color_manual(values=c("Projected \226 Registered"="black")) +
  scale_linetype_manual(values=c("Projected \226 Registered"=4)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,
                                 hjust=0.95,
                                 size=12),
        axis.text.y=element_text(size=12),
        plot.title=element_text(face="bold",
                                hjust=0.5,
                                size=16,
                                margin=margin(t=0, r=0, b=10, l=0)),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        legend.position="bottom",
        legend.key.width=unit(1, 'cm'),
        legend.text=element_text(size=10),
        text=element_text(family="serif")) 

Appendix1 <- plot_grid(A1a, A1b, ncol=2, align = "hv")

#Appendix 2:
#development of fertility rates and population shares
#by age over time (ages 12-55, years 2010-2019)
A.aux1 <-
  as.data.frame(
    cbind(C=rep(nNx.US.12to55.2000to2019.standardized %>% 
                  filter(Year==2010) %>% 
                  pull(C), 9),
          Year=rep(2011:2019, each=44),
          Age=rep(12:55), times=9)) ##2010 population shares

A.aux2 <-
  as.data.frame(
    cbind(ASFR=rep(ASFR.US.12to55.1990to2019 %>% 
                     filter(Year==2010) %>% 
                     pull(ASFR), 9),
          Year=rep(2011:2019, each=44),
          Age=rep(12:55), times=9)) ##2010 ASFRs

Appendix2 <-
  ggplot() +
    geom_line(data=nNx.US.12to55.2000to2019.standardized %>% filter(Year>=2011), 
              aes(x=Age, y=C), color="black") +
    geom_line(data=ASFR.US.12to55.1990to2019 %>% filter(Year>=2011), 
              aes(x=Age, y=ASFR/10), color="black") + ##divide by 10 for secondary y-axis
    geom_hline(yintercept=0.015, color="black", size=0.4) + ##horizontal line to divide panels into segments
    scale_y_continuous(name="Population Share", 
                       limits=c(0, 0.03),
                       breaks=seq(0, 0.03, 0.005),
                       labels=c(" ", " ", " ",  
                                "0.015", "0.020", "0.025", "0.030"),
                       sec.axis = sec_axis(trans= ~ . * 10, 
                                           name="Age-specific Fertility Rate",
                                           breaks=seq(0, 0.3, 0.05),
                                           labels=c("0", "0.05", "0.10", "0.15",
                                                    " ", " ", " ")),
                       guide="axis_minor") +
    scale_x_continuous(limits=c(10, 60),
                       breaks=seq(10, 60, 5),
                       guide="axis_minor") +
    guides(y.sec="axis_minor") +
    facet_wrap(~ Year) +
    geom_line(data=A.aux1, aes(x=Age, y=C), color="grey60") + ##add 2010 values to each panel
    geom_line(data=A.aux2, aes(x=Age, y=ASFR/10), color="grey60") + ##add 2010 values to each panel
    geom_text(data=A.aux1 %>% filter(Year==2016), aes(label="2010", x=50, y=0.029),             
              size=2.5, 
              family="serif",
              hjust=0.5) +
    geom_segment(data=A.aux1 %>% filter(Year==2016), aes(x=50, y=0.0275, xend=50, yend=0.0255), size=0.01) +
    geom_text(data=A.aux1 %>% filter(Year==2016), aes(label="2016", x=25, y=0.029), 
              size=2.5, 
              family="serif",
              hjust=0.5) +
    geom_segment(data=A.aux1 %>% filter(Year==2016), aes(x=25, y=0.0275, xend=25, yend=0.0255), size=0.01) +  
    geom_text(data=A.aux2 %>% filter(Year==2019), aes(label="2010", x=13.5, y=0.005), 
              size=2.5, 
              family="serif",
              hjust=1) +
    geom_segment(data=A.aux2 %>% filter(Year==2019), aes(x=14, y=0.005, xend=17.5, yend=0.005), size=0.01) +  
    geom_text(data=A.aux2 %>% filter(Year==2019), aes(label="2019", x=24.5, y=0.005), 
              size=2.5, 
              family="serif",
              hjust=0) +
    geom_segment(data=A.aux2 %>% filter(Year==2019), aes(x=20.5, y=0.005, xend=24, yend=0.005), size=0.01) +
    theme_bw() +
    theme(axis.title=element_text(face="bold",
                                  size=16),
          axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0)),
          axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0)),
          axis.title.y.right=element_text(margin=margin(t=0, r=0, b=0, l=10)),
          axis.text=element_text(size=12),
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(),
          strip.background=element_rect(colour="black",
                                        fill="white"),
          strip.text=element_text(size=12),
          text=element_text(family="serif"))

#Appendix 3
#additional/diminished births by age (Scenarios 1-3)
Appendix3 <-
  ggplot() +
  geom_line(aes(x=12:55, 
                y=rowSums(Births.US.12to55.2010to2019.observed/1000)-rowSums(Births.US.12to55.2010to2019.nMx.all.cause.fixed/1000), 
                color="Scenario 1", 
                linetype="Scenario 1"), 
            size=0.6) +
  geom_line(aes(x=12:55,
                y=rowSums(Births.US.12to55.2010to2019.observed/1000)-rowSums(Births.US.12to55.2010to2019.nMx.EU/1000),
                color="Scenario 2",
                linetype="Scenario 2"),
            size=0.6) +
  geom_line(aes(x=12:55,
                y=(rowSums(Births.US.12to55.2010to2019.observed/1000)-rowSums(Births.US.12to55.2010to2019.ASFR.fixed/1000))*8/400,
                color="Scenario 3",
                linetype="Scenario 3"),
            size=0.6) +
  geom_hline(yintercept=0, color="grey60", alpha=0.4, size=0.6) + 
  xlab(" ") +
  ylab("Scenarios 1 and 2") + 
  labs(color=" ",
       linetype=" ") +
  scale_x_continuous(limits=c(10, 60),
                     breaks=(seq(10, 60, 5)),
                     minor_breaks=(seq(10, 60, 1)),
                     guide="axis_minor") +
  scale_y_continuous(limits=c(-8, 4),
                     breaks=(seq(-8, 4, 2)),
                     sec.axis=sec_axis(trans= ~ . * 400/8, 
                                       name="Scenario 3",
                                       breaks=seq(-500, 200, 50)),
                     guide="axis_minor") +
  guides(y.sec="axis_minor") +
  scale_color_manual(values=c("Scenario 1"="black",
                              "Scenario 2"="black",
                              "Scenario 3"="black")) +
  scale_linetype_manual(values=c("Scenario 1"=3,
                                 "Scenario 2"=2,
                                 "Scenario 3"=4)) +
  theme_bw() +
  theme(axis.title=element_text(face="bold",
                                size=16),
        axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0)),
        axis.title.y.right=element_text(margin=margin(t=0, r=0, b=0, l=10)),
        axis.text.x=element_text(angle=45,
                                 hjust=0.95,
                                 size=12),
        axis.text.y=element_text(size=12),        
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        legend.position="bottom",
        legend.key.width=unit(1, 'cm'),
        legend.text=element_text(size=10),        
        text=element_text(family="serif"))  

#save plots
save_plot(here("plots", "Appendix 1.svg"),
          plot=Appendix1,
          base_height=6,
          base_width=8)

save_plot(here("plots", "Appendix 2.svg"),
          plot=Appendix2,
          base_height=6,
          base_width=8)

save_plot(here("plots", "Appendix 3.svg"),
          plot=Appendix3,
          base_height=6,
          base_width=10)