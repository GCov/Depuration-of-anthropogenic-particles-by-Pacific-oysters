library(cowplot)

theme1 <-
  theme_bw() +
  theme(
    panel.spacing = unit(1, "lines"), 
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.grid = element_blank()
  )

#### Size and Colours plot ####

daylabs <- c("Day 0", 
             "Day 1", 
             "Day 3", 
             "Day 5", 
             "Day 10")
names(daylabs) <- c(0, 1, 3, 5, 10)

depwater5$size.cat <- factor(depwater5$size.cat,
                             levels = c("100-500",
                                        "500-1000",
                                        "1000-5000"))

waterplot <- 
  ggplot(depwater5) +
  geom_col(aes(x = size.cat,
               y = finalcount,
               fill = variable),
           colour = "black") +
  scale_fill_manual(values = c("steel blue", "grey90", "red", "turquoise")) +
  labs(x = expression(paste("Particle size category ("*mu*"m)")),
       y = "Particle count") +
  facet_wrap(~day,
             labeller = labeller(day = daylabs),
             nrow = 1) +
  theme1 +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

algae5$size.cat <- factor(algae5$size.cat,
                          levels = c("100-500",
                                     "500-1000",
                                     "1000-5000"))

algaeplot <-
  ggplot(algae5) +
  geom_col(aes(x = size.cat,
               y = finalcount,
               fill = variable),
           colour = "black") +
  scale_fill_manual(values = c("steel blue", "grey90", "red", "turquoise")) +
  labs(x = "",
       y = "Particle count") +
  facet_wrap(~species,
             ncol = 1) +
  theme1 +
  theme(legend.position = "none",
        strip.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 20, hjust = 1))

oysterdep5$size.cat <- factor(oysterdep5$size.cat,
                              levels = c("100-500",
                                         "500-1000",
                                         "1000-5000"))

oysterplot <- 
  ggplot(oysterdep5) +
  geom_col(aes(x = size.cat,
               y = finalcount,
               fill = variable),
           colour = "black") +
  scale_fill_manual(
    values = c(
      "Grey16",
      "Steel Blue",
      "Saddle Brown",
      "Grey90",
      "Grey70",
      "Forest Green",
      "Orange",
      "Pink",
      "Red",
      "Turquoise",
      "White",
      "Yellow"
    )) +
  labs(x = "",
       y = "Particle count") +
  facet_wrap(~sampleday,
             labeller = labeller(sampleday = daylabs),
             nrow = 1) +
  guides(fill = guide_legend(title = "Particle type")) +
  theme1 +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 20, hjust = 1))

grid1 <- plot_grid(oysterplot, waterplot, 
                   labels = c("Oysters", "Water"),
                   rel_heights = c(1, 0.32),
                   label_size = 8,
                   ncol = 1)

tiff("Particle Categories Plot.tiff",
     width = 19,
     height = 16,
     units = "cm",
     compression = "none",
     res = 500)

plot_grid(grid1, algaeplot,
          nrow = 1,
          rel_widths = c(1, 0.24),
          labels = c("", "Algae"),
          label_size = 8)

dev.off()


#### Model fit ####

tiff("Model fit.tiff",
     width = 9,
     height = 7,
     units = "cm",
     compression = "none",
     res = 500)

ggplot() +
  geom_ribbon(data = predictions,
              aes(x = sampleday,
                  ymin = lower,
                  ymax = upper,
                  fill = length),
              alpha = 0.5) +
  geom_line(data = predictions,
            aes(x = sampleday,
                y = mean,
                colour = length),
            size = 1) +
  geom_point(data = totalsums,
             aes(x = sampleday,
                 y = sum),
             size = 0.75) +
  scale_colour_brewer(type = "qual",
                      palette = "Set2",
                      name = "Length (mm)") +
  
  scale_fill_brewer(type = "qual",
                    palette = "Set2",
                    name = "Oyster Shell \nWidth (mm)") +
  scale_x_continuous(breaks = c(0, 1, 3, 5, 10)) +
  labs(x = "Day",
       y = expression(paste("APs"~ind^-1))) +
  theme1

dev.off()


