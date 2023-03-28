library(tidyverse)
library(patchwork)

p1 <- mpg %>%
  ggplot(aes(x=displ, y=hwy)) +
    geom_point() +
    geom_smooth(color="blue", se=FALSE) +
    labs(title = "1")

p2 <- mpg %>%
  ggplot(aes(x=displ, y=hwy)) +
  geom_point() +
  geom_smooth(color="blue", se=FALSE, aes(group = drv)) +
  labs(title = "2")

p3 <- mpg %>%
  ggplot(aes(x=displ, y=hwy, color=drv)) +
  geom_point() +
  geom_smooth(se=FALSE)+
  labs(title = "3")

p4 <- mpg %>%
  ggplot(aes(x=displ, y=hwy)) +
  geom_point(aes(color=drv)) +
  geom_smooth(se=FALSE, color="blue")+
  labs(title = "4")

p5 <- mpg %>%
  ggplot(aes(x=displ, y=hwy)) +
  geom_point(aes(color=drv)) +
  geom_smooth(se=FALSE, color="blue",
              aes(linetype = drv))+
  labs(title = "5")

p6 <- mpg %>%
  ggplot(aes(x=displ, y=hwy)) +
  geom_point(shape = 21,
             aes(fill=drv), color="white", stroke=2)+
  labs(title = "6")

p1 + p2 / p3 + p4 / p5 + p6 +
  plot_annotation(title = "Hint: just use default colors")

