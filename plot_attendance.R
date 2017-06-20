library(RColorBrewer)
library(ggplot2)
source('~/GitHub/MPs_attendance/attendance.R')
A <- get_attendance_stat()
A <- filter(A, !is.na(faction))
A$faction <- as.character(A$faction)
A$faction[grepl("Блок", A$faction)] <- "БПП"
A$faction <- as.factor(A$faction)
A$faction <- with(A, reorder(faction, faction, function(x) -length(x)))
pl <- ggplot(A, aes(faction, present, color = faction, fill = faction)) + xlab(NULL) + ylab("Не прогуляно засідань")
jit <- pl + geom_jitter(width = 0.15, alpha = 0.5, size = 2) + 
  theme_tufte() + theme(panel.grid = element_line(color = "grey"), legend.position = "none", axis.text.x = element_text(face = "bold"), panel.grid.minor = element_blank()) + 
  scale_y_continuous(breaks = c(0,3,6,9,12)) + 
  ggtitle("Відвідування у лютому за даними письмової реєстрації") +
  scale_color_manual( name = "Фракції", values = brewer.pal(9, "Set1")) 

jit <- pl + geom_jitter(aes(x=''), width = 0.15, alpha = 0.5, size = 2) + 
  theme_tufte() + theme(panel.grid = element_line(color = "grey"), legend.position = "none", axis.text.x = element_text(face = "bold"), panel.grid.minor = element_blank()) + 
  scale_y_continuous(breaks = c(0,3,6,9,12)) + 
  scale_x_discrete(expand = c(0,0)) + 
  ggtitle("Відвідування у лютому за даними письмової реєстрації") +
  scale_color_manual( name = "Фракції", values = brewer.pal(9, "Set1")) +
  theme(text = element_text(size = 14), strip.text.x = element_text(face = "bold")) +
  facet_wrap(~faction, scales = "free_x")
  #coord_fixed(2/3)
print(jit)
viol <- pl + geom_violin(color = "grey") + 
  theme_tufte() + theme(panel.grid = element_line(color = "grey"), legend.position = "none", axis.text.x = element_text(face = "bold")) + 
  scale_y_continuous(breaks = c(0,3,6,9,12)) +
  scale_fill_manual( name = "Фракції", values = brewer.pal(9, "Set1"))
print(viol)