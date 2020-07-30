library(ggplot2)
library(cowplot)

## Figure 2

Plot1 <- 
  ggplot(totalblanksums) + 
  geom_jitter(aes(x = 1, 
                  y = sum),
              size=0.5) + 
  ylab(expression(paste('# APs'~sample^-1))) +
  xlab("") +
  guides(fill=FALSE) +
  theme_bw() +
  coord_cartesian(ylim = c(0,3)) +
  theme1 +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

Plot2 <-
  ggplot(blanksprop, aes(x = sampleday, y = 100 * prop, fill = size.cat)) +
  geom_col(size = 0.25, colour = 'black') +
  xlab('Sampling day') +
  ylab('Percent') +
  guides(fill = guide_legend(title = expression(paste(
    "Particle size category (" * mu * "m)"
  )))) +
  theme_bw() +
  scale_fill_manual(values = c('grey25', 'grey50', 'grey75')) +
  theme1

Plot3 <- 
  ggplot(blankstypeprop, aes(x=sampleday, y=100*prop, fill = variable)) + 
  geom_col(size = 0.25, colour = 'black') + 
  xlab('Sampling day') + 
  ylab('Percent') +
  guides(fill=guide_legend(title="Particle type")) +
  theme_bw() +
  scale_fill_manual(values = c('Grey16', 'Grey90')) +
  theme1

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


## Figure 3

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

### Figure 4

A.1 <- 
  ggplot(totalsums3) +
  geom_ribbon(aes(x = dry.weight,
                  ymin = lower,
                  ymax = upper),
              fill = 'steel blue',
              alpha = 0.5) +
  geom_line(aes(x = dry.weight,
                y = mean),
            size = 0.5) +
  geom_point(aes(x = dry.weight,
                 y = sum),
             size = 0.5) +
  facet_grid(. ~ sampleday,
             labeller = as_labeller(c('0' = 'Day 0',
                                      '1' = 'Day 1',
                                      '3' = 'Day 3',
                                      '5' = 'Day 5',
                                      '10' = 'Day 10'))) +
  labs(x = 'Dry tissue weight (g)',
       y = 'Number of APs') +
  theme1

B.1 <- 
  ggplot(oysterprop2, aes(x=sampleday, y=100*prop, fill = size.cat)) + 
  geom_col(size = 0.5, colour = 'black') + 
  xlab('Sampling Day') + 
  ylab('Percent') +
  guides(fill=guide_legend(title="Particle size category \n (microns)")) +
  theme_bw() +
  scale_fill_manual(values = c('grey25', 'grey50', 'grey75')) +
  theme1

C.1 <- 
  ggplot(oystertypeprop2, aes(x = sampleday, y = 100 * prop, fill = variable)) +
  geom_col(size = 0.5, colour = 'black') +
  xlab('Sampling Day') +
  ylab('Percent') +
  guides(fill = guide_legend(title = "Particle type")) +
  theme_bw() +
  scale_fill_manual(values = c('Grey16', 'Steel Blue', 'Grey90', 'Orange', 
                               'Pink', 'Red', 'Turquoise', 'White', 
                               'Yellow')) +
  theme1

D.1 <- plot_grid(A.1, B.1, align = 'v', nrow = 2, ncol = 1, rel_widths = 1, 
               rel_heights = 1, labels = c('A','B'), label_size = 8)

png(
  filename = "Figure 4.png",
  width = 19,
  height = 6,
  units = "cm",
  pointsize = 7,
  res = 500
)

plot_grid(D.1, C.1, nrow = 1, ncol = 2, rel_widths = c(1, 1),
          rel_heights = c(1, 1), labels = c('', 'C'), label_size = 8)

dev.off()

