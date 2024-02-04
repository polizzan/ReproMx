########
#PLOTS#
######

#Figure 1a
#trends in TFR
F1a <-
 ggplot(TFR.US.1990to2019, aes(x=Year, y=`Total Fertility Rate`)) +
 geom_vline(xintercept=2010, color="grey60", alpha=0.4, size=0.3) +
 geom_line(size=0.6) +
 ggtitle("Total Fertility Rate") +  
 xlab(" ") +
 ylab(" ") +
 scale_x_continuous(limits=c(1990, 2020),
                    breaks=(seq(1990, 2020, 5)),
                    minor_breaks=(seq(1991, 2019, 1)),
                    guide="prism_minor") +
 scale_y_continuous(limits=c(1.7, 2.2),
                    breaks=(seq(1.7, 2.2, 0.1))) +
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
       text=element_text(family="serif"))

#Figure 1b
#trends in temporary life expectancy
F1b <-
  ggplot(e1255.USandEU.1990to2019, aes(x=Year, y=`Temporary Life Expectancy`)) +
  geom_vline(xintercept=2010, color="grey60", alpha=0.4, size=0.3) +
  geom_line(size=0.6) +
  ggtitle("Temporary Life Expectancy") +  
  xlab(" ") +
  ylab(" ") +
  scale_x_continuous(limits=c(1990, 2020),
                     breaks=(seq(1990, 2020, 5)),
                     minor_breaks=(seq(1991, 2019, 1)),
                     guide="prism_minor") +
  scale_y_continuous(limits=c(43.2, 43.8),
                     breaks=(seq(43.2, 43.8, 0.1))) +
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
        text=element_text(family="serif"),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        strip.text=element_text(size=12)) +
  facet_wrap(~Country)

Figure1 <- plot_grid(F1a, F1b, ncol=2)

#Figure 2
#age-standardized mortality rates
Figure2 <- 
  ggplot(data=nMx.US.12to55.2010to2019.cause.specific.standardized,
         aes(x=Year, y=nmx_i.index)) +
  geom_line(size=0.6) +
  geom_hline(yintercept=100, color="grey60", alpha=0.4, size=0.3) +
  geom_text(data=nMx.US.12to55.2010to2019.cause.specific.standardized %>% filter(Year==2010), 
            aes(label=round(nmx_i.std, 2), x=2010.3, y=117), 
            size=2.5, 
            family="serif") +
  geom_segment(aes(x=2010, y=101, xend=2010, yend=112), size=0.01) +
  xlab(" ") +
  ylab("Percent Change in Deaths per 100,000") +
  scale_x_continuous(limits=c(2010, 2020),
                     breaks=seq(2010, 2020, 2),
                     minor_breaks=(seq(2011, 2019, 1)),
                     guide="prism_minor") +
  scale_y_continuous(trans='log',
                     limits=c(80, 170),
                     breaks=seq(80, 170, 10),
                     labels=c(-20, -10, 0,
                              10, 20, 30, 40,
                              50, 60, 70),
                     minor_breaks=seq(85, 165, 10),
                     guide="prism_minor") +
  theme_bw() +
  facet_wrap(~ Cause, nrow=3) +
  theme(axis.title=element_text(face="bold",
                                size=16),
        axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0)),
        axis.text.x=element_text(angle=45,
                                 hjust=0.95,
                                 size=8),
        axis.text.y=element_text(size=8),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        strip.background=element_rect(colour="black",
                                      fill="white"),
        strip.text=element_text(size=8),
        text=element_text(family="serif"))

#Figure 3a
#projected births (Scenarios 1-3)
F3a <-
  ggplot() +
  geom_line(aes(x=2010:2019, y=colSums(Births.US.12to55.2010to2019.observed/1000), 
                color="Observed", linetype="Observed"), size=0.6) +
  geom_line(aes(x=2010:2019, y=colSums(Births.US.12to55.2010to2019.nMx.all.cause.fixed/1000), 
                color="Scenario 1", linetype="Scenario 1"), size=0.6) +
  geom_line(aes(x=2010:2019, y=colSums(Births.US.12to55.2010to2019.nMx.EU/1000), 
                color="Scenario 2", linetype="Scenario 2"), size=0.6) + 
  geom_line(aes(x=2010:2019, y=colSums(Births.US.12to55.2010to2019.ASFR.fixed/1000), 
                color="Scenario 3", linetype="Scenario 3"), size=0.6) + 
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
                     minor_breaks=(seq(3725, 4225, 50)),
                     labels=c("3,700", "3,750",
                              "3,800", "3,850",
                              "3,900", "3,950",
                              "4,000", "4,050",
                              "4,100", "4,150",
                              "4,200", "4,250"),
                     guide="prism_minor") +
  scale_color_manual(values=c("Observed"="grey60",
                              "Scenario 1"="black",
                              "Scenario 2"="black",
                              "Scenario 3"="black")) +
  scale_linetype_manual(values=c("Observed"=1,
                                 "Scenario 1"=3,
                                 "Scenario 2"=2,
                                 "Scenario 3"=4)) +  
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

#Figure 3b
#additional/diminished births (Scenarios 1-3)
F3b <-
  ggplot() +
  geom_line(aes(x=2010:2019, 
                y=colSums(Births.US.12to55.2010to2019.observed/1000)-colSums(Births.US.12to55.2010to2019.nMx.all.cause.fixed/1000), 
                color="Scenario 1", 
                linetype="Scenario 1"), 
            size=0.6) +
  geom_line(aes(x=2010:2019,
                y=colSums(Births.US.12to55.2010to2019.observed/1000)-colSums(Births.US.12to55.2010to2019.nMx.EU/1000),
                color="Scenario 2",
                linetype="Scenario 2"),
            size=0.6) +
  geom_line(aes(x=2010:2019,
                y=(colSums(Births.US.12to55.2010to2019.observed/1000)-colSums(Births.US.12to55.2010to2019.ASFR.fixed/1000))*12/500,
                color="Scenario 3",
                linetype="Scenario 3"),
            size=0.6) +
  xlab(" ") +
  ylab("Scenarios 1 and 2") + 
  ggtitle("Diminished Births (in Thousands)") +
  labs(color=" ",
       linetype=" ") +
  scale_x_continuous(limits=c(2010, 2020),
                     breaks=(seq(2010, 2020, 2)),
                     minor_breaks=(seq(2011, 2019, 2)),
                     guide="axis_minor") +
  scale_y_continuous(limits=c(-12, 0),
                     breaks=(seq(-12, 0, 2)),
                     sec.axis=sec_axis(trans= ~ . * 500/12, 
                                       name="Scenario 3",
                                       breaks=seq(-500, 0, 50)),
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
        axis.title.y.right=element_text(margin=margin(t=0, r=0, b=0, l=20)),
        axis.text.x=element_text(angle=45,
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

Figure3 <- plot_grid(F3a, F3b, ncol=2, align="hv")

#Figure 4
#gained/lost births by cause of death
Figure4 <-
  Births.lost.US.12to55.2010to2019.cause.specific %>% 
  ggplot(aes(x=Cause, y=`Births Lost`)) +
  geom_hline(yintercept=0, color="grey60", alpha=0.4, size=0.3) +  
  geom_point(color="black") +
  xlab("Cause of Death") +
  ylab("Births Gained or Lost") +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(limits=c(-4750, 250),
                     breaks=(seq(-4750, 250, 250)),
                     labels=c("-4,750", "-4,500", "-4,250", 
                              "-4,000", "-3,750", "-3,500", 
                              "-3,250", "-3,000", "-2,750", 
                              "-2,500", "-2,250", "-2,000", 
                              "-1,750", "-1,500", "-1,250", 
                              "-1,000", "-750", "-500", 
                              "-250", "0", "250")) +
  geom_brace(aes(x=c(10, 12), y=c(-3900, -4050), label="Accidents"), family="serif", inherit.data=FALSE, 
             labelsize=4, labeldistance=300, rotate=180, npoints=1000) +
  coord_flip(clip="off", expand=FALSE, x=c(0, 14), y=c(300, -3800)) +
  theme_bw() +
  theme(plot.margin=unit(c(0.05, 0.125, 0.05, 0.05), units="npc"),
        axis.title=element_text(face="bold",
                                size=16),
        axis.text.x=element_text(angle=45,
                                 hjust=0.95,
                                 size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0)),
        axis.title.y=element_text(margin=margin(t=0, r=30, b=0, l=0)),
        panel.grid.major.x=element_blank(), 
        panel.grid.minor=element_blank(),
        text=element_text(family="serif"))

#save plots
save_plot(here("plots", "Figure 1.svg"),
          plot=Figure1,
          base_height=6,
          base_width=12)

save_plot(here("plots", "Figure 2.svg"),
          plot=Figure2,
          base_height=6,
          base_width=10)

save_plot(here("plots", "Figure 3.svg"),
          plot=Figure3,
          base_height=6,
          base_width=10)

save_plot(here("plots", "Figure 4.svg"),
          plot=Figure4,
          base_height=6,
          base_width=10)
