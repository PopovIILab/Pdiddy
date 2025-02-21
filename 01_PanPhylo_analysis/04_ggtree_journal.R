# Set the dir

main_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(main_dir)

# Load/install required packages

#install.packages('BiocManager')

if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(
  ggtree,
  ggimage,
  ggtext,
  phangorn,
  ggplot2,
  treeio,
  ggnewscale,
  viridis,
  phytools,
  patchwork
)

####################
# METADATA PARSING #
####################

# Load `metadata`

metadata <- read.table('metadata/metadata.tsv', sep = '\t', header = T)

# `Year` column

meta.year <- as.data.frame(metadata[, 'Year'])
colnames(meta.year) <- 'Year'
rownames(meta.year) <- metadata$Name
meta.year$Year[meta.year$Year == "ND"] <- NA

# `Host` column

meta.host <- as.data.frame(metadata[, 'Host'])
colnames(meta.host) <- 'Host'
rownames(meta.host) <- metadata$Name
meta.host$Host[meta.host$Host == "ND"] <- NA

# `Country` column

meta.loc <- as.data.frame(metadata[, 'Country'])
colnames(meta.loc) <- 'Country'
rownames(meta.loc) <- metadata$Name
meta.loc$Country[meta.loc$Country == "ND"] <- NA
meta.loc$Country[meta.loc$Country == "USA: New York, Williams Hotel"] <- "USA"
meta.loc$Country[meta.loc$Country == "Switzerland: Boedmerenwald"] <- "Switzerland"
colnames(meta.loc) <- c("Location")


###############################
# ATP synthase subunit a TREE #
###############################

# Read the tree file

ATP_ssa_tree <- read.tree("tree/ATP_synthase_subunit_a_ufb.treefile")

# Midpoint root the tree

midpoint.root(ATP_ssa_tree)

# Draft tree

ATP_ssa_tree_fig <- ggtree(ATP_ssa_tree) %<+% metadata +
  xlim(0, 0.6) +
  geom_hilight(
    mapping = aes(subset = node %in% c(24), fill = S),
    fill = "steelblue",
    alpha = .6,
    extend = 1
  ) +
  geom_tiplab(
    aes(
      label = AN_OrganismName,
      fontface = ifelse(grepl("^NC_033907", AN_OrganismName), "bold", "plain"),
    ),
    align = TRUE,
    geom = "label",
    fill = "white",
    label.size = 0
  ) +
  scale_color_identity() +
  geom_treescale(x = 0, y = -0.25, width = 0.05)

# Onehot encode bootstrap values (<70 = 0; >70 = 1)

ATP_ssa_tree_boot <- ATP_ssa_tree_fig$data
ATP_ssa_tree_boot <- ATP_ssa_tree_boot[!ATP_ssa_tree_boot$isTip, ]
ATP_ssa_tree_boot$label <- as.numeric(ATP_ssa_tree_boot$label)
ATP_ssa_tree_boot$bootstrap <- '0'
ATP_ssa_tree_boot$bootstrap[ATP_ssa_tree_boot$label >= 70] <- '1'
ATP_ssa_tree_boot$bootstrap[is.na(ATP_ssa_tree_boot$label)] <- '1'

# Add bootstrap values to the tree (black branches = bootstrap >70; grey branches = bootstrap <70)

ATP_ssa_tree_fig <- ATP_ssa_tree_fig + new_scale_color() +
  geom_tree(data = ATP_ssa_tree_boot, aes(color = bootstrap == '1')) +
  scale_color_manual(name = 'Bootstrap',
                     values = setNames(c("black", "grey"), c(T, F)),
                     guide = "none")

ggsave(
  'imgs/ATP_synthase_subunit_a.png',
  ATP_ssa_tree_fig,
  width = 14,
  height = 8,
  dpi = 600
)

########################
# Cytochrome b/c1 TREE #
########################

# Read the tree file

Cbc1_tree <- read.tree("tree/Cytochrome_b_c1_ufb.treefile")

# Midpoint root the tree

midpoint.root(Cbc1_tree)

# Draft tree

Cbc1_tree_fig <- ggtree(Cbc1_tree) %<+% metadata +
  xlim(0, 0.3) +
  geom_hilight(
    mapping = aes(subset = node %in% c(25), fill = S),
    fill = "steelblue",
    alpha = .6,
    extend = 1
  ) +
  geom_tiplab(
    aes(
      label = AN_OrganismName,
      fontface = ifelse(grepl("^NC_033907", AN_OrganismName), "bold", "plain"),
    ),
    align = TRUE,
    geom = "label",
    fill = "white",
    label.size = 0
  ) +
  scale_color_identity() +
  geom_treescale(x = 0, y = -0.25, width = 0.05)

# Onehot encode bootstrap values (<70 = 0; >70 = 1)

Cbc1_tree_boot <- Cbc1_tree_fig$data
Cbc1_tree_boot <- Cbc1_tree_boot[!Cbc1_tree_boot$isTip, ]
Cbc1_tree_boot$label <- as.numeric(Cbc1_tree_boot$label)
Cbc1_tree_boot$bootstrap <- '0'
Cbc1_tree_boot$bootstrap[Cbc1_tree_boot$label >= 70] <- '1'
Cbc1_tree_boot$bootstrap[is.na(Cbc1_tree_boot$label)] <- '1'

# Add bootstrap values to the tree (black branches = bootstrap >70; grey branches = bootstrap <70)

Cbc1_tree_fig <- Cbc1_tree_fig + new_scale_color() +
  geom_tree(data = Cbc1_tree_boot, aes(color = bootstrap == '1')) +
  scale_color_manual(name = 'Bootstrap',
                     values = setNames(c("black", "grey"), c(T, F)),
                     guide = "none")

ggsave(
  'imgs/Cytochrome_b_c1.png',
  Cbc1_tree_fig,
  width = 14,
  height = 8,
  dpi = 600
)

##############################################
# NADH-quinone oxidoreductase subunit K TREE #
##############################################

# Read the tree file

NADHqosK_tree <- read.tree("tree/NADH-quinone_oxidoreductase_subunit_K_ufb.treefile")

# Midpoint root the tree

midpoint.root(NADHqosK_tree)

# Draft tree

NADHqosK_tree_fig <- ggtree(NADHqosK_tree) %<+% metadata +
  xlim(0, 0.5) +
  geom_hilight(
    mapping = aes(subset = node %in% c(29), fill = S),
    fill = "steelblue",
    alpha = .6,
    extend = 1
  ) +
  geom_tiplab(
    aes(
      label = AN_OrganismName,
      fontface = ifelse(grepl("^NC_033907", AN_OrganismName), "bold", "plain"),
    ),
    align = TRUE,
    geom = "label",
    fill = "white",
    label.size = 0
  ) +
  scale_color_identity() +
  geom_treescale(x = 0, y = -0.25, width = 0.05)

# Onehot encode bootstrap values (<70 = 0; >70 = 1)

NADHqosK_tree_boot <- NADHqosK_tree_fig$data
NADHqosK_tree_boot <- NADHqosK_tree_boot[!NADHqosK_tree_boot$isTip, ]
NADHqosK_tree_boot$label <- as.numeric(NADHqosK_tree_boot$label)
NADHqosK_tree_boot$bootstrap <- '0'
NADHqosK_tree_boot$bootstrap[NADHqosK_tree_boot$label >= 70] <- '1'
NADHqosK_tree_boot$bootstrap[is.na(NADHqosK_tree_boot$label)] <- '1'

# Add bootstrap values to the tree (black branches = bootstrap >70; grey branches = bootstrap <70)

NADHqosK_tree_fig <- NADHqosK_tree_fig + new_scale_color() +
  geom_tree(data = NADHqosK_tree_boot, aes(color = bootstrap == '1')) +
  scale_color_manual(name = 'Bootstrap',
                     values = setNames(c("black", "grey"), c(T, F)),
                     guide = "none")

ggsave(
  'imgs/NADH-quinone_oxidoreductase_subunit_K.png',
  NADHqosK_tree_fig,
  width = 14,
  height = 8,
  dpi = 600
)

##############################################
# NADH-quinone oxidoreductase subunit K TREE #
##############################################

# Read the tree file

NADHqosM_tree <- read.tree("tree/NADH-quinone_oxidoreductase_subunit_M_ufb.treefile")

# Midpoint root the tree

midpoint.root(NADHqosM_tree)

# Draft tree

NADHqosM_tree_fig <- ggtree(NADHqosM_tree) %<+% metadata +
  xlim(0, 0.35) +
  geom_hilight(
    mapping = aes(subset = node %in% c(24), fill = S),
    fill = "steelblue",
    alpha = .6,
    extend = 1
  ) +
  geom_tiplab(
    aes(
      label = AN_OrganismName,
      fontface = ifelse(grepl("^NC_033907", AN_OrganismName), "bold", "plain"),
    ),
    align = TRUE,
    geom = "label",
    fill = "white",
    label.size = 0
  ) +
  scale_color_identity() +
  geom_treescale(x = 0, y = -0.25, width = 0.05)

# Onehot encode bootstrap values (<70 = 0; >70 = 1)

NADHqosM_tree_boot <- NADHqosM_tree_fig$data
NADHqosM_tree_boot <- NADHqosM_tree_boot[!NADHqosM_tree_boot$isTip, ]
NADHqosM_tree_boot$label <- as.numeric(NADHqosM_tree_boot$label)
NADHqosM_tree_boot$bootstrap <- '0'
NADHqosM_tree_boot$bootstrap[NADHqosM_tree_boot$label >= 70] <- '1'
NADHqosM_tree_boot$bootstrap[is.na(NADHqosM_tree_boot$label)] <- '1'

# Add bootstrap values to the tree (black branches = bootstrap >70; grey branches = bootstrap <70)

NADHqosM_tree_fig <- NADHqosM_tree_fig + new_scale_color() +
  geom_tree(data = NADHqosM_tree_boot, aes(color = bootstrap == '1')) +
  scale_color_manual(name = 'Bootstrap',
                     values = setNames(c("black", "grey"), c(T, F)),
                     guide = "none")

ggsave(
  'imgs/NADH-quinone_oxidoreductase_subunit_M.png',
  NADHqosM_tree_fig,
  width = 14,
  height = 8,
  dpi = 600
)

######################
###### COMBINED ######
######################

everything <- (ATP_ssa_tree_fig + Cbc1_tree_fig) / (NADHqosK_tree_fig + NADHqosM_tree_fig) + plot_annotation(tag_levels = list(c("A", "B", "C", "D")))
ggsave(
  "imgs/combined_tree.png",
  plot = everything,
  width = 28,
  height = 16,
  dpi = 600
)
