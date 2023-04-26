#N Kraft  June 30 2022

read.csv("Germination_prepped.csv")->g
read.csv("fecundity_prepped.csv")->f

## noted that NA had to be changed to NAAT to not read as an 'NA'

## create germination rate in g:

g$germination<-g$count/g$sown_seeds

## quick checks on rates:


boxplot(g$germination~g$species)
boxplot(g$germination~g$plot_type)
boxplot(g$germination~g$plot_type+g$species)

splist<-unique(g$species)
ave_g<-NULL
sd_g<-NULL

for(i in 1:length(splist)){
	
	subset(g, g$species==splist[i])->temp
	ave_g[i]<-mean(temp$germination)
	sd_g[i]<-sd(temp$germination)
	
}

germ_aves<-data.frame(splist, ave_g, sd_g)


####

boxplot(log10(f$seeds)~f$species)

boxplot(log10(f$seeds)~f$plot_type)

boxplot(log10(f$seeds)~f$plot_type+f$species, las=2, col="red")