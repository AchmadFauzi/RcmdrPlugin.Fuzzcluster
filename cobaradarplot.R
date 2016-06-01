mtcarsscaled <- as.data.frame(lapply(mtcars[1:5,], scale))
mtcarsscaled$model <- rownames(mtcars[1:5,])
mtcarsmelted <- reshape2::melt(mtcarsscaled)
library("ggplot2")
ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model), size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2))

#multiple
ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model), size = 2) +
  facet_wrap(~ model) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = "none")

#Polar
ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model),  size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_polar()
#multiple_
ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model), size = 2) +
  facet_wrap(~ model) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = "none") +
  coord_polar()

#----
coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = model, color = model), fill = NA, size = 2, show.legend = FALSE) +
  geom_line(aes(group = model, color = model), size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_radar()

ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = model, color = model), fill = NA, size = 2) +
  facet_wrap(~ model) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = "none") +
  coord_radar()
