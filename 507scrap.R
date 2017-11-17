#507 scrap!

#create a matrix with ecosections as row names!
mat2<-as.matrix(mat1)
row.names(mat2) <- mat2[,1]
gar.matrix <- mat2[,-1]
class(gar.matrix)<-"numeric"

View(gar.matrix)
#check that it all worked out okay

#region, proportion based dissimilarity - bray curtis
reg.nmds.bc<- metaMDS(gar.matrix,distance="bray",labels=eco, trymax = 100, autotransform = FALSE)
reg.nmds.bc  
plot(reg.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.1187554

#region, proportion based dissimilarity - manhattan
reg.nmds.mh<- metaMDS(gar.matrix,distance="manhattan",labels=eco, trymax = 100, autotransform = FALSE)
reg.nmds.mh  
plot(reg.nmds.mh)
#found convergence easily (<20), stress is rel. low = 0.1187554 (exact same!)

#region, proportion based dissimilarity - kulczynski
reg.nmds.ku<- metaMDS(gar.matrix,distance="kulczynski",labels=eco, trymax = 100, autotransform = FALSE)
reg.nmds.ku  
plot(reg.nmds.ku)
#found convergence easily (<20), stress is rel. low = 0.1187554 (exact same!)

rankindex(eco, gar.matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#try to compare different distance calculations methods
#results 0.06075846 euc, 0.08129483 man, 0.05805424 gow, 0.08129483 bra, 0.08129484 kul
#so kul is ever so slightly better than bra and man, any of the 3 are good!

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_reg.bc<-adonis(gar.matrix ~ eco, permutations = 999, method="bray")
permanova_reg.bc #significant! p = 0.001 so plot it

permanova_reg.mh<-adonis(gar.matrix ~ eco, permutations = 999, method="manhattan")
permanova_reg.mh #significant! p = 0.001 so plot it (has higher mean sqs and sum of sqs tho)

permanova_reg.ku<-adonis(gar.matrix ~ eco, permutations = 999, method="kulczynski")
permanova_reg.ku #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=reg.nmds.bc$points[,1],NMDS2.bc=reg.nmds.bc$points[,2],group=eco)
#plot NMDS, only once (picking Bray because all similar), no presence absence

View(NMDS.bc)

ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
	geom_point(stat = "identity", aes(fill=group), size=3, shape=21, color = "black")+
	scale_fill_manual(values=c("lightslateblue", "lavender", "plum1", "goldenrod1", "tomato", "lightblue1", "rosybrown4", "green", "lemonchiffon", "magenta2", "aquamarine3", "darkorchid4", "antiquewhite3"),
										labels=c("Aristazabal Banks Upwelling", "Mainland Fjords", "East Queen Charlotte Sound", "Cape Scott Tidal Mixing", "Johnstone Strait", "Northern Strait of Georgia", "Central Strait of Georgia", "Southern Strait of Georgia", "Interior Gulf Island", "Haro Strait and Rosario Passage", "Juan de Fuca Strait", "Low Flow Nearshore", "Dogfish Bank Frontal Region"),
										name="Ecosection") +
	scale_y_continuous(limits=c(-0.5,0.7),breaks=seq(-0.5,0.7,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
	scale_x_continuous(limits=c(-0.75,0.4),breaks=seq(-0.75,0.4,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
	theme_bw()+
	theme(axis.text.x=element_text(size=12),
				axis.title.x=element_text(size=12),
				axis.title.y=element_text(angle=90,size=12), 
				axis.text.y=element_text(size=12), 
				panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
	annotate("text",x=-0.65,y=0.7,label="stress = 0.12",size=4)

#geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
#	scale_colour_manual(values=c("lightslateblue", "lavender", "plum1", "goldenrod1", "tomato", "lightblue1", "rosybrown4", "green", "lightgoldenrodyellow", "magenta2", "aquamarine3", "darkorchid4", "antiquewhite3"),
#											breaks=c("Aristazabal Banks Upwelling", "Mainland Fjords", "East Queen Charlotte Sound", "Cape Scott Tidal Mixing", "Johnstone Strait", "Northern Strait of Georgia", "Central Strait of Georgia", "Southern Strait of Georgia", "Interior Gulf Island", "Haro Strait and Rosario Passage", "Juan de Fuca Strait", "Low Flow Nearshore", "Dogfish Bank Frontal Region")
#											labels=c("Aristazabal Banks Upwelling", "Mainland Fjords", "East Queen Charlotte Sound", "Cape Scott Tidal Mixing", "Johnstone Strait", "Northern Strait of Georgia", "Central Strait of Georgia", "Southern Strait of Georgia", "Interior Gulf Island", "Haro Strait and Rosario Passage", "Juan de Fuca Strait", "Low Flow Nearshore", "Dogfish Bank Frontal Region"),
#											name="Ecosection", guide=FALSE) +

#redo-ing analysis here without ArBU, EQCS, CSTM and SSoG (>3 points)