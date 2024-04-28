library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(paletteer)

hm_matrix_known <- known_genes_exp |> 
  select(!Name) |> 
  data.frame(row.names = 1) |> 
  as.matrix() |> 
  t()

hm_matrix_c4q24 <- C4q24_genes_exp |> 
  select(!Name) |> 
  data.frame(row.names = 1) |> 
  as.matrix() |> 
  t()

hm_matrix_filtered <- filtered_data |> 
  select(!Name) |> 
  data.frame(row.names = 1) |> 
  as.matrix() |> 
  t()

hm_matrix_clustered <- all_data |> 
  select(!Name) |> 
  data.frame(row.names = 1) |> 
  as.matrix() |> 
  t()

# Column annotations
filtered <- filtered_data |> 
  select(Description) |> 
  mutate(Group = 'C4q24')

known <- known_genes_exp |> 
  select(Description) |> 
  mutate(Group = 'Known')

col_ann <- bind_rows(filtered, known) |> 
  data.frame(row.names = 1)
col_ann$Group <- factor(col_ann$Group)
ann_colors <- list(Group = c("C4q24" = "chocolate", "Known" = "darkgreen"))

# Generating a color palette from blue to white to red
color_palette <- colorRampPalette(c("blue", "white", "red"))(n = 100)

# Define breaks. We expect most data to lie between -3 and 3 after scaling
breaks <- seq(-4, 4, length.out = length(color_palette) + 1)

pheatmap(hm_matrix_known,
         scale = "column",
         color = color_palette,
         breaks = breaks,
         cluster_cols = FALSE,
         treeheight_row = 25,
         cellwidth = 10,
         fontsize = 4,
         show_rownames = TRUE,
         filename = "known genes.png",
         width = 5
)

pheatmap(hm_matrix_c4q24,
         scale = "column",
         color = color_palette,
         breaks = breaks,
         cluster_cols = FALSE,
         treeheight_row = 25,
         cellwidth = 10,
         fontsize = 4,
         show_rownames = TRUE,
         filename = "c4q24 genes.png"
)

pheatmap(hm_matrix_filtered,
         scale = "column",
         color = color_palette,
         breaks = breaks,
         cluster_cols = FALSE,
         treeheight_row = 25,
         cellwidth = 10,
         fontsize = 4,
         show_rownames = TRUE,
         filename = "filtered genes.png"
)

pheatmap(hm_matrix_clustered,
         scale = "column",
         color = color_palette,
         annotation_col=col_ann,
         annotation_colors = ann_colors,
         breaks = breaks,
         treeheight_row = 25,
         cutree_cols = 9,
         cellwidth = 10,
         fontsize = 4,
         show_rownames = TRUE,
         filename = "all data.png",
)

