library(ggplot2)
library(ggrepel)

theme_1950s <- function() {
  theme_minimal(base_family = "Arial", base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#FAF5E9", color = NA),
      panel.background = element_rect(fill = "#FAF5E9", color = NA),
      panel.grid.major = element_line(color = "#E6DECA", linewidth = 0.6),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", color = "#3A3A3A"),
      axis.text = element_text(color = "#4A4A4A"),
      plot.title = element_text(face = "bold", size = 20, color = "#3A3A3A"),
      plot.subtitle = element_text(size = 13, color = "#5A5A5A"),
      plot.margin = margin(15, 15, 15, 15),
      legend.position = "none"
    )
}

# ── Verified driving distances and approximate compass bearings from Stamford ──
# Bearings estimated from actual lat/lon positions
# Knox City:  ~39 mi driving, bearing ~355° (nearly due north, slightly west)
# Haskell:    ~16 mi driving, bearing ~10°  (north-northeast)
# Stonewall:  ~30 mi driving, bearing ~315° (northwest)
# Anson:      ~15 mi driving, bearing ~205° (south-southwest)
# Hendrick:   ~40 mi driving, bearing ~185° (nearly due south, slightly east)

# Convert bearing + distance to x,y
# bearing: 0=north, 90=east, 180=south, 270=west
bearing_to_xy <- function(dist, bearing_deg) {
  rad <- bearing_deg * pi / 180
  x <- dist * sin(rad)
  y <- dist * cos(rad)
  c(x, y)
}

knox     <- bearing_to_xy(39, 355)
haskell  <- bearing_to_xy(16, 10)
stonewall<- bearing_to_xy(30, 315)
anson    <- bearing_to_xy(15, 205)
hendrick <- bearing_to_xy(40, 185)

facilities <- data.frame(
  name = c("Stamford\nHealth Clinic",
           "Knox County\nHospital",
           "Haskell Memorial\nHospital",
           "Stonewall Memorial\nHospital",
           "Anson General\nHospital",
           "Hendrick\nMedical Center"),
  x = c(0, knox[1], haskell[1], stonewall[1], anson[1], hendrick[1]),
  y = c(0, knox[2], haskell[2], stonewall[2], anson[2], hendrick[2]),
  type = c("Closed Hospital", "CAH", "CAH", "CAH", "CAH", "Major Hospital"),
  drive_mi = c(0, 39, 16, 30, 15, 40),
  stringsAsFactors = FALSE
)

cat("Facility positions (x, y in miles from Stamford):\n")
for (i in 1:nrow(facilities)) {
  cat(sprintf("  %-30s x=%6.1f  y=%6.1f  drive=%d mi\n",
              gsub("\n", " ", facilities$name[i]),
              facilities$x[i], facilities$y[i], facilities$drive_mi[i]))
}

# ── Colors / shapes ──
col_closed <- "#C0392B"; col_major <- "#2471A3"; col_cah <- "#7D8C8E"

facilities$color <- ifelse(facilities$type == "Closed Hospital", col_closed,
                    ifelse(facilities$type == "Major Hospital", col_major, col_cah))
facilities$pt_size <- ifelse(facilities$type == "Closed Hospital", 7,
                     ifelse(facilities$type == "Major Hospital", 5.5, 4))
facilities$pt_shape <- ifelse(facilities$type == "Closed Hospital", 4,
                      ifelse(facilities$type == "Major Hospital", 16, 17))

# ── Segments from Stamford to each facility ──
others <- facilities[facilities$drive_mi > 0, ]
segs <- data.frame(
  x = 0, y = 0,
  xend = others$x, yend = others$y,
  lbl = paste0("~", others$drive_mi, " mi")
)
segs$mx <- segs$xend * 0.5
segs$my <- segs$yend * 0.5

# Nudge labels perpendicular to the line
segs$nx <- c(3,  3,  -4, -3,  4)  # Knox, Haskell, Stonewall, Anson, Hendrick
segs$ny <- c(0,  -2,  2,  2, -1)

# ── Plot ──
p <- ggplot() +
  # Distance lines
  geom_segment(data = segs, aes(x=x, y=y, xend=xend, yend=yend),
               color = "#BFAD8E", linewidth = 0.5, linetype = "dashed") +
  # Distance labels
  geom_text(data = segs, aes(x = mx + nx, y = my + ny, label = lbl),
            size = 3.5, color = "#8A7D65", fontface = "italic", family = "Arial") +
  # Points
  geom_point(data = facilities, aes(x = x, y = y),
             color = facilities$color, size = facilities$pt_size,
             shape = facilities$pt_shape,
             stroke = ifelse(facilities$type == "Closed Hospital", 2.5, 1)) +
  # Labels
  geom_label_repel(
    data = facilities, aes(x = x, y = y, label = name),
    size = 3.5, family = "Arial", color = facilities$color, fontface = "bold",
    fill = alpha("#FAF5E9", 0.9), label.size = 0.3,
    box.padding = 1.0, point.padding = 0.6,
    segment.color = "#BFAD8E", segment.size = 0.3,
    max.overlaps = 20, seed = 42,
    nudge_x = c(10, 8, 8, -10, -10, -10),
    nudge_y = c(3, 3, -3, 3, -3, 3)
  ) +
  # Legend
  annotate("point", x = -28, y = 42, shape = 4, color = col_closed, size = 4, stroke = 2) +
  annotate("text",  x = -24, y = 42, label = "Closed hospital (clinic remains)",
           hjust = 0, size = 3.2, color = "#5A5A5A", family = "Arial") +
  annotate("point", x = -28, y = 38, shape = 16, color = col_major, size = 4) +
  annotate("text",  x = -24, y = 38, label = "Full-service medical center",
           hjust = 0, size = 3.2, color = "#5A5A5A", family = "Arial") +
  annotate("point", x = -28, y = 34, shape = 17, color = col_cah, size = 3.5) +
  annotate("text",  x = -24, y = 34, label = "Critical Access Hospital",
           hjust = 0, size = 3.2, color = "#5A5A5A", family = "Arial") +
  # Use coord_cartesian so data fills the wide output frame
  # Line lengths are schematic; distance labels provide the actual mileage
  coord_cartesian(xlim = c(-30, 20), ylim = c(-43, 44)) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title = "Healthcare Around Stamford, Texas",
    subtitle = "Stamford Memorial Hospital closed July 2018. Jones County has no hospital.",
    x = NULL, y = NULL,
    caption = "Copyright (c) 2026 Andrew R. Crocker. All rights reserved.   |   Data: CMS Hospital General Information; distance-cities.com; withinhours.com\nDriving distances shown. Facilities placed at approximate compass bearing from Stamford."
  ) +
  theme_1950s() +
  theme(
    panel.grid.major = element_blank(),
    plot.caption = element_text(size = 8, color = "#8A8A8A", hjust = 0, margin = margin(t = 10)),
    axis.text = element_blank(),
    plot.subtitle = element_text(size = 12, color = "#5A5A5A", margin = margin(b = 10))
  )

ggsave("/home/claude/stamford_map.png", p, width = 1456/100, height = 1000/100, dpi = 100, bg = "#FAF5E9")
cat("Done.\n")
