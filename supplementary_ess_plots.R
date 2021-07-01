


cat("\014") # clean screen
rm(list = ls()) # remove every existing object
 
####################################################################################################################
########################################### SUPPLEMENTARY PLOTS FROM ESS ###########################################
####################################################################################################################


file.name.ess <- paste("",sep="") # PATH TO ESS FRENCH DATA

file.name.ess.2 <- paste("",sep="") # PATH TO ESS 5 COUNTRY DATA 


 ess <- read.dta13(file.name.ess, convert.factors = FALSE) %>% as_tibble()
 ess_5_countries <- read.dta13(file.name.ess.2, convert.factors = FALSE) %>% as_tibble()

## Simple bar graph by groups

binary_set <- c( "red","orange")
ess_years <- c(		`1` = "2002",
					`2` = "2004",
                    `3` = "2006",
                    `4` = "2008",
                    `5` = "2010",
                    `6` = "2012",
                    `7` = "2014",
                    `8` = "2016",
                    `9` = "2018"
                    )

ess_years2 <- c(	`1` = "'02",
					`2` = "'04",
                    `3` = "'06",
                    `4` = "'08",
                    `5` = "'10",
                    `6` = "'12",
                    `7` = "'14",
                    `8` = "'16",
                    `9` = "'18"
                    )


ess_cntry <- c(		`BE` = "BE",
					`DE` = "GE",
                    `FR` = "FR",
                    `NL` = "NL",
                    `SE` = "SW"
                    )

ess_cntry_long <- c(		`BE` = "Belgium",
							`DE` = "Germany",
                 		    `FR` = "France",
               		        `NL` = "Netherlands",
             		        `SE` = "Sweden"
                    )


### PLOT WITH FRANCE ONLY

# SUBJECTIVE RELIGIOSITY ## CHANGE SE WITH 1.96*SE of the Mean

plot.rel.native.muslim.ess.france <- ess %>% filter((rel_imm==2 | rel_imm==3) & essround>2) %>% group_by(rel_imm, essround) %>% 
summarise(mean_relig=mean(religiosity), sd_relig=sd(religiosity), sem=sd(religiosity, na.rm=TRUE)/sqrt(sum(!is.na(religiosity))), ll_relig=mean_relig-1.96*sem, ul_relig=mean_relig+1.96*sem) %>%
ggplot(aes(x=factor(rel_imm), y=mean_relig, group=factor(rel_imm) , fill=factor(rel_imm))) +
geom_bar(stat="identity") +  geom_errorbar(aes(x=factor(rel_imm), ymin=ll_relig, ymax=ul_relig, width=0.4)) +
facet_grid( ~ essround, labeller = as_labeller(ess_years)) +    
ggtitle("")  + theme_grey(base_size = 15) + theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) +
labs(x= "", y = "Self-rated religiosity (0-10 scale)") +
scale_fill_manual(name="Immigrant-religious group",
                    breaks=c("2", "3"),
                    values=c("deepskyblue2", "#E69F00", "#56B4E9"),
                    labels=c( "1: Muslim 1G & 2G", "2: Native (3G+)")) + 
scale_x_discrete("", breaks = c("2","3"), labels=c("1","2"))+
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) + theme(text = element_text(size=20)) 

print(plot.rel.native.muslim.ess.france)

dev.print(pdf,'plot.rel.native.muslim.ess.france.pdf')

# PROPORTION PRAYING DAILY

plot.pray.native.muslim.ess.france <- ess %>% filter((rel_imm==2 | rel_imm==3) & essround>2) %>% group_by(rel_imm, essround) %>% 
summarise(mean_relig=mean(pray_daily, na.rm=TRUE), sd_relig=sd(pray_daily, na.rm=TRUE), sem=sd(pray_daily, na.rm=TRUE)/sqrt(sum(!is.na(pray_daily))), ll_relig=mean_relig-1.96*sem, ul_relig=mean_relig+1.96*sem) %>%
ggplot(aes(x=factor(rel_imm), y=mean_relig, group=factor(rel_imm) , fill=factor(rel_imm))) +
geom_bar(stat="identity") +  geom_errorbar(aes(x=factor(rel_imm), ymin=ll_relig, ymax=ul_relig, width=0.4)) +
facet_grid( ~ essround, labeller = as_labeller(ess_years)) +    
ggtitle("")  + theme_grey(base_size = 15) + theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) +
labs(x= "", y = "Proportion stating they pray daily") +
scale_fill_manual(name="Immigrant-religious group",
                    breaks=c("2", "3"),
                    values=c("deepskyblue2", "#E69F00", "#56B4E9"),
                    labels=c( "1: Muslim 1G & 2G", "2: Native (3G+)")) + 
scale_x_discrete("", breaks = c("2","3"), labels=c("1","2"))+
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) + theme(text = element_text(size=20)) 

print(plot.pray.native.muslim.ess.france)

dev.print(pdf,'plot.pray.native.muslim.ess.france.pdf')


### PLOT WITH BE, DE, FR, NL, SE

# SUBJECTIVE RELIGIOSITY

plot.rel.native.muslim.ess.europe <- ess_5_countries %>% filter((rel_imm==2 | rel_imm==3) & essround>2 & cntry!="FR") %>% group_by(rel_imm, essround, cntry) %>% 
summarise(mean_relig=mean(religiosity), sd_relig=sd(religiosity), sem=sd(religiosity, na.rm=TRUE)/sqrt(sum(!is.na(religiosity))), ll_relig=mean_relig-1.96*sem, ul_relig=mean_relig+1.96*sem) %>%
ggplot(aes(x=factor(rel_imm), y=mean_relig, group=factor(rel_imm) , fill=factor(rel_imm))) +
geom_bar(stat="identity", width = 1.7) +  geom_errorbar(aes(x=factor(rel_imm), ymin=ll_relig, ymax=ul_relig, width=0.4)) +
facet_grid( ~ cntry + essround, labeller = labeller(cntry = ess_cntry, essround = ess_years2)) +    
ggtitle("")  + theme_grey(base_size = 15) + theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) +
labs(x= "", y = "Self-rated religiosity (0-10 scale)") +
scale_fill_manual(name="Immigrant-religious group",
                    breaks=c("2", "3"),
                    values=c("deepskyblue2", "#E69F00", "#56B4E9"),
                    labels=c( "1: Muslim 1G & 2G", "2: Native (3G+)")) + 
scale_x_discrete("", breaks = c("2","3"), labels=c("1","2"))+
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) + theme(text = element_text(size=20)) 

print(plot.rel.native.muslim.ess.europe)

dev.print(pdf,'plot.rel.native.muslim.ess.europe.pdf')


# PROPORTION stating they PRAY DAILY

plot.praying.native.muslim.ess.europe <- ess_5_countries %>% filter((rel_imm==2 | rel_imm==3) & essround>2 & cntry!="FR") %>% group_by(rel_imm, essround, cntry) %>% 
summarise(mean_relig=mean(pray_daily, na.rm=TRUE), sd_relig=sd(pray_daily,na.rm=TRUE), sem=sd(pray_daily, na.rm=TRUE)/sqrt(sum(!is.na(pray_daily))), ll_relig=mean_relig-1.96*sem, ul_relig=mean_relig+1.96*sem) %>%
ggplot(aes(x=factor(rel_imm), y=mean_relig, group=factor(rel_imm) , fill=factor(rel_imm))) +
geom_bar(stat="identity", width = 1.7) +  geom_errorbar(aes(x=factor(rel_imm), ymin=ll_relig, ymax=ul_relig, width=0.4)) +
facet_grid( ~ cntry + essround, labeller = labeller(cntry = ess_cntry, essround = ess_years2)) +    
ggtitle("")  + theme_grey(base_size = 15) + theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) +
labs(x= "", y = "Proportion stating they pray daily") +
scale_fill_manual(name="Immigrant-religious group",
                    breaks=c("2", "3"),
                    values=c("deepskyblue2", "#E69F00", "#56B4E9"),
                    labels=c( "1: Muslim 1G & 2G", "2: Native (3G+)")) + 
scale_x_discrete("", breaks = c("2","3"), labels=c("1","2"))+
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) + theme(text = element_text(size=20)) 

print(plot.praying.native.muslim.ess.europe)

dev.print(pdf,'plot.praying.native.muslim.ess.europe.pdf')

# Graphs integrating the catholic, and splitting things up by generation, not years

plot.rel.native.muslim.ess.europe2 <- ess_5_countries %>% filter((rel_gen!=3 & rel_gen!=4) & essround>2 & cntry!="SE" & cntry!="NL") %>% group_by(rel_gen, cntry) %>% 
summarise(mean_relig=mean(religiosity), sd_relig=sd(religiosity), sem=sd(religiosity, na.rm=TRUE)/sqrt(sum(!is.na(religiosity))), ll_relig=mean_relig-1.96*sem, ul_relig=mean_relig+1.96*sem) %>%
ggplot(aes(x=factor(rel_gen), y=mean_relig, group=factor(rel_gen) , fill=factor(rel_gen))) +
geom_bar(stat="identity", width=1.2, alpha=0.90) +  geom_errorbar(aes(x=factor(rel_gen), ymin=ll_relig, ymax=ul_relig, width=0.4)) +
facet_grid( ~ cntry, labeller = labeller(cntry = ess_cntry_long)) +    
ggtitle("")  + theme_grey(base_size = 15) + theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) +
labs(x= "", y = "Self-rated religiosity (0-10 scale)") +
scale_fill_manual(name="Immigrant-religious group",
                    breaks=c("1", "2", "5", "6","7"),
                    values=c("#196619","#53c653","#003380","#80b3ff","#2F4F4F"),
                    labels=c("1: Catholic 1G", "2: Catholic 2G", "3: Muslim 1G", "4: Muslim 2G", "5: Native (3G+)")) + 
scale_x_discrete("", breaks = c("1","2","5","6","7"), labels=c("1","2","3","4","5"))+
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) + theme(text = element_text(size=20)) 

print(plot.rel.native.muslim.ess.europe2)
dev.print(pdf,'plot.rel.native.muslim.ess.europe2.pdf')


plot.praying.native.muslim.ess.europe2 <- ess_5_countries %>% filter((rel_gen!=3 & rel_gen!=4) & essround>2 & cntry!="SE" & cntry!="NL") %>% group_by(rel_gen, cntry) %>% 
summarise(mean_relig=mean(pray_daily, na.rm=TRUE), sd_relig=sd(pray_daily,na.rm=TRUE), sem=sd(pray_daily, na.rm=TRUE)/sqrt(sum(!is.na(pray_daily))), ll_relig=mean_relig-sem, ul_relig=mean_relig+1.96*sem) %>%
ggplot(aes(x=factor(rel_gen), y=mean_relig, group=factor(rel_gen) , fill=factor(rel_gen))) +
geom_bar(stat="identity", width=1.2, alpha=0.90) +  geom_errorbar(aes(x=factor(rel_gen), ymin=ll_relig, ymax=ul_relig, width=0.4)) +
facet_grid( ~ cntry, labeller = labeller(cntry = ess_cntry_long)) +    
ggtitle("")  + theme_grey(base_size = 15) + theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) +
labs(x= "", y = "Proportion stating they pray daily") +
scale_fill_manual(name="Immigrant-religious group",
                    breaks=c("1", "2", "5", "6","7"),
                    values=c("#196619","#53c653","#003380","#80b3ff","#2F4F4F"),
                    labels=c("1: Catholic 1G", "2: Catholic 2G", "3: Muslim 1G", "4: Muslim 2G", "5: Native (3G+)")) + 
scale_x_discrete("", breaks = c("1","2","5","6","7"), labels=c("1","2","3","4","5"))+
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) + theme(text = element_text(size=20)) 

print(plot.praying.native.muslim.ess.europe2)
dev.print(pdf,'plot.praying.native.muslim.ess.europe2.pdf')





 





