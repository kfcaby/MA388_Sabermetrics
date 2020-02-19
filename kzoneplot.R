
plate_width <- 17 + 2 * (9/pi)

k_zone_plot <- ggplot(NULL, aes(x = px, y = pz)) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = "blue", alpha = 0) +
  coord_equal() +
  scale_x_continuous("Horizontal location (ft.)",
                     limits = c(-2,2)) +
  scale_y_continuous("Vertical location (ft.)",
                     limits = c(0,5))
