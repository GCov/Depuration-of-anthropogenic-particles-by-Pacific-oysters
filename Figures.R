library(ggplot2)
library(cowplot)

## Figure 2

Plot1 <- 
  ggplot(totalblanksums, aes(y=sum)) + 
  geom_boxplot(position=position_dodge(), size=0.5, fill = 'grey90') + 
  ylab(expression(paste('# AFs'~sample^-1))) +
  guides(fill=FALSE) +
  theme_bw() +
  coord_cartesian(ylim = c(0,7)) +
  theme(
    text = element_text(size = 7),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )

Plot2 <- 
  ggplot(blanksprop3, aes(x=sampleday, y=100*mean, fill = size.cat)) + 
  geom_col(size = 0.25, colour = 'black') + 
  xlab('') + 
  ylab('Average percent') +
  guides(fill=guide_legend(title="Particle size \ncategory (μm)")) +
  theme_bw() +
  scale_fill_manual(values = c("#009E73", "#56B4E9", "#0072B2", "black")) +
  theme(legend.text = element_text(size=7), 
        text = element_text(size=7), 
        legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()+
  scale_x_discrete(expand = expand_scale(0, 0.5))
  )

Plot3 <- 
  ggplot(blankstypeprop3, aes(x=sampleday, y=100*mean, fill = variable)) + 
  geom_col(size = 0.25, colour = 'black') + 
  xlab('Sampling Day') + 
  ylab('Average percent') +
  guides(fill=guide_legend(title="Particle size \ncategory (μm)")) +
  theme_bw() +
  scale_fill_manual(values = c('blue', 'grey90', 'pink')) +
  theme(legend.text = element_text(size=7), text = element_text(size=7),
        legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = expand_scale(0,0.5))

Plot4 <- plot_grid(Plot2, Plot3, align = 'v', nrow = 2, ncol = 1, 
                   rel_widths = c(1,1), 
               rel_heights = c(1,1), labels = c('B','C'), label_size = 8)

png(
  filename = "Figure 2.png",
  width = 9,
  height = 7,
  units = "cm",
  pointsize = 7,
  res = 600
)

plot_grid(Plot1, Plot4, nrow = 1, ncol = 2, rel_widths = c(1, 3),
          rel_heights = c(1, 1), labels = c('A', ''), label_size = 8)

dev.off()


## Figure 2

A <- 
  ggplot(totalwatersums, aes(x=day, y=sum)) + 
    geom_boxplot(position=position_dodge(), size=0.5, fill = 'grey90') + 
    xlab('Day') +
    ylab(expression(paste('# AFs'~L^-1))) +
    guides(fill=FALSE) +
    theme_bw() +
    coord_cartesian(ylim = c(0,7)) +
    theme(
      text = element_text(size = 7),
      axis.text.x = element_text(
        size = 7),
      axis.text.y = element_text(size = 7),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )

B <- 
  ggplot(waterprop3, aes(x=day, y=100*mean, fill = size.cat)) + 
    geom_col(size = 0.25, colour = 'black') + 
    xlab('Sampling Day') + 
    ylab('Average percent') +
    guides(fill=guide_legend(title="Particle size \ncategory (μm)")) +
    theme_bw() +
    scale_fill_manual(values = c("#009E73", "#56B4E9", "#0072B2", "black")) +
    theme(legend.text = element_text(size=7), text = element_text(size=7), 
          legend.key.size = unit(0.4, 'cm'),
          panel.spacing = unit(1, "lines"),  
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7, 
                                     margin = margin(t=0, r=0, b=0, l=2)),
          strip.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.placement = "outside") +
    scale_x_discrete(expand = expand_scale(0,0.5))

C <- 
  ggplot(watertypeprop3, aes(x=day, y=100*mean, fill = variable)) + 
    geom_col(size = 0.25, colour = 'black') + 
    xlab('Sampling Day') + 
    ylab('Average percent') +
    guides(fill=guide_legend(title="Particle size \ncategory (μm)")) +
    theme_bw() +
    scale_fill_manual(values = c('blue', 'grey90', 'red', 'turquoise')) +
    theme(legend.text = element_text(size=7), text = element_text(size=7),
          legend.key.size = unit(0.4, 'cm'),
          panel.spacing = unit(1, "lines"),  
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          strip.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    scale_x_discrete(expand = expand_scale(0,0.5))

D <- plot_grid(B, C, align = 'v', nrow = 2, ncol = 1, rel_widths = c(1,1), 
               rel_heights = c(1,1), labels = c('B','C'), label_size = 8)

png(
  filename = "Figure 3.png",
  width = 9,
  height = 7,
  units = "cm",
  pointsize = 7,
  res = 600
)

plot_grid(A, D, nrow = 1, ncol = 2, rel_widths = c(1, 2),
          rel_heights = c(1, 1), labels = c('A', ''), label_size = 8)

dev.off()

### Figure 3

A.1 <- 
  ggplot(totalsums2, aes(x=sampleday, y=sum)) + 
    geom_boxplot(position=position_dodge(), size=0.5, fill = 'grey90',
                 outlier.size = 0.5) + 
    xlab('Day') + 
    coord_cartesian(ylim = c(0, 35)) +
    ylab(expression(paste('# AFs '~ind^-1))) +
    guides(fill=FALSE) +
    theme_bw() +
    theme(text = element_text(size=7), 
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          strip.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    annotate("text", x=1, y=27, label = "*", size = 4) + 
    annotate("text", x=2, y=30, label = "*", size = 4) +
    annotate("text", x=3, y=25, label = "*", size = 4) +
    annotate("text", x=4, y=7, label = "**", size = 4) +
    annotate("text", x=5, y=12, label = "**", size = 4)

B.1 <- 
  ggplot(oysterprop6, aes(x=sampleday, y=100*mean, fill = size.cat)) + 
    geom_col(size = 0.25, colour = 'black') + 
    xlab('Sampling Day') + 
    ylab('Average percent') +
    guides(fill=guide_legend(title="Particle size \ncategory (μm)")) +
    theme_bw() +
    scale_fill_manual(values = c("#F0E442", "#009E73", 
                               "#56B4E9", "#0072B2", "black")) +
    theme(legend.text = element_text(size=7), text = element_text(size=7), 
          legend.key.size = unit(0.4, 'cm'),
          legend.position = 'bottom',
          panel.spacing = unit(1, "lines"),  
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7, 
                                     margin = margin(t=0, r=0, b=0, l=2)),
          strip.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.placement = "outside") +
    scale_x_discrete(expand = expand_scale(0,0.5)) + 
    scale_y_continuous(expand = expand_scale(0,0))

C.1 <- 
  ggplot(oystertypeprop6, aes(x=sampleday, y=100*mean, fill = variable)) + 
    geom_col(size = 0.25, colour = 'black') + 
    xlab('Sampling Day') + 
    ylab('Average percent') +
    guides(fill=guide_legend(title="Particle colour and shape")) +
    theme_bw() +
    scale_fill_manual(values = c('black', 'blue', 'saddlebrown', 'grey90', 
                                 'green4', 'orange',  'pink', 'red', 'turquoise', 
                                 'yellow')) +
    theme(legend.text = element_text(size=7), text = element_text(size=7),
          legend.key.size = unit(0.4, 'cm'),
          panel.spacing = unit(1, "lines"),  
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          strip.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    scale_x_discrete(expand = expand_scale(0,0.5)) + 
    scale_y_continuous(expand = expand_scale(0,0))

D.1 <- plot_grid(A.1, B.1, align = 'v', nrow = 2, ncol = 1, rel_widths = 1, 
               rel_heights = 1, labels = c('A','B'), label_size = 8)

png(
  filename = "Figure 4.png",
  width = 19,
  height = 14,
  units = "cm",
  pointsize = 7,
  res = 600
)

plot_grid(D.1, C.1, nrow = 1, ncol = 2, rel_widths = c(1, 1),
          rel_heights = c(1, 1), labels = c('', 'C'), label_size = 8)

dev.off()

