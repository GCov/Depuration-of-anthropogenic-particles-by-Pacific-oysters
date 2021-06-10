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

#### Size Plots ####

oysters_size_plot <-
  ggplot(oysterprop, aes(x=sampleday, y=100*prop, fill = size.cat)) + 
  geom_col(size = 0.5, colour = "black") + 
  xlab("Sampling Day") + 
  ylab("Percent") +
  guides(fill=guide_legend(title="Particle size category \n (microns)")) +
  theme_bw() +
  scale_fill_manual(values = c("grey25", "grey50", "grey75")) +
  theme1 +
  theme(plot.margin = margin(0.5,0,0,0, unit = "cm"),
        legend.position = "none") +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0.03,0))

water_size_plot <-
  ggplot(waterprop3, aes(x=day, y=100*mean, fill = size.cat)) + 
  geom_col(size = 0.5, colour = "black") + 
  xlab("Sampling day") + 
  ylab("Percent") +
  guides(fill=guide_legend(title="Particle size category \n (microns)")) +
  theme_bw() +
  scale_fill_manual(values = c("grey25", "grey50", "grey75")) +
  theme1 +
  theme(plot.margin = margin(0.5,0,0,0, unit = "cm"),
        legend.position = "none") +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0.03,0))

algae_size_plot <- 
  ggplot(algaeprop, aes(x=species, y=100*prop, fill = size.cat)) + 
  geom_col(size = 0.5, colour = "black") + 
  xlab("") + 
  ylab("Percent") +
  guides(fill=guide_legend(title="Particle size category \n (microns)")) +
  theme_bw() +
  scale_fill_manual(values = c("grey25", "grey50", "grey75")) +
  theme1 +
  theme(plot.margin = margin(0.5,0,0,0, unit = "cm"),
        axis.text.x = element_text(face = "italic")) +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0.03,0))

grid1 <- plot_grid(water_size_plot, 
                   algae_size_plot,
                   oysters_size_plot,
                   nrow = 3,
                   labels = c("Water", "Algae", "Oysters"),
                   label_size = 8,
                   align = "v",
                   axis = "l")

tiff("Depuration Size Plots.tiff",
     width = 14,
     height = 10,
     units = "cm",
     compression = "none",
     res = 500)

grid1

dev.off()


#### Colour Plots ####

water_colour_plot <-
  ggplot(watertypeprop3, aes(x=day, y=100*mean, fill = variable)) + 
  geom_col(size = 0.5, colour = "black") + 
  xlab("Sampling Day") + 
  ylab("Percent") +
  guides(fill=guide_legend(title="Particle size category \n (microns)")) +
  theme_bw() +
  scale_fill_manual(values = c("steel blue", "grey90", "red", "turquoise")) +
  theme1 + 
  theme(plot.margin = margin(0.5,0,0,0, unit = "cm"),
        legend.position = "none") +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0.03,0))

algae_colour_plot <-
  ggplot(algaetypeprop, aes(x=species, y=100*prop, fill = variable)) + 
  geom_col(size = 0.5, colour = "black") + 
  xlab("") + 
  ylab("") +
  guides(fill=guide_legend(title="Particle size category \n (microns)")) +
  theme_bw() +
  scale_fill_manual(values = c("steel blue", "grey90", "red", "turquoise")) +
  theme1 + 
  theme(plot.margin = margin(0.5,0,0,0, unit = "cm"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0.03,0))

oysters_colour_plot <-
  ggplot(oystertypeprop, aes(x = sampleday, y = 100 * prop, fill = variable)) +
  geom_col(size = 0.5, colour = "black") +
  xlab("Sampling Day") +
  ylab("Percent") +
  guides(fill = guide_legend(title = "Particle size category \n (microns)")) +
  theme_bw() +
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
    )
  ) +
  theme1 + 
  theme(plot.margin = margin(0.5,0,0,0, unit = "cm")) +
  scale_x_discrete(expand = c(0,0.5)) + 
  scale_y_continuous(expand = c(0.03,0))

grid2 <- 
  plot_grid(water_colour_plot, 
            algae_colour_plot,
            nrow = 1,
            labels = c("Water", "Algae"),
            label_size = 8,
            align = "h",
            rel_widths = c(1, 0.6))

grid3 <- plot_grid(oysters_colour_plot,
                   grid2, 
                   nrow = 2,
                   labels = c("Oysters", ""),
                   label_size = 8)

tiff("Depuration Colour Plots.tiff",
     width = 14,
     height = 17,
     units = "cm",
     compression = "none",
     res = 500)

grid3

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
                    name = "Length (mm)") +
  scale_x_continuous(breaks = c(0, 1, 3, 5, 10)) +
  labs(x = "Day",
       y = expression(paste("APs"~ind^-1))) +
  theme1

dev.off()

