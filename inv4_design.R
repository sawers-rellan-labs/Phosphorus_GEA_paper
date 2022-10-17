
design <- list(
trt = c("P-","P+"),
plot = c(1:4),
gt = c("_B73","MICH"),
plant = c(1:4),
leaf =  as.integer(c(1:4)),
dup = c("L","R")
)





invP <- expand.grid(design) %>%
  mutate(trt = factor(trt, levels = c("P-","P+"))) %>%
  arrange_at(names(design))


library(dplyr)

label <- invP %>%
  filter(plant == 1) %>%
  group_by(trt,dup) %>% 
  mutate(n = row_number(),
         top_label = paste0(dup, sprintf("%02d", n)),
         side_label = paste0("'",trt, "P",plot, gt, "_L",leaf,dup)
         ) %>% ungroup() %>%
  select(trt:gt)  %>% unique()


write.csv(label,"~/Desktop/penn_label_FIELD.csv", quote = FALSE, row.names = FALSE)

