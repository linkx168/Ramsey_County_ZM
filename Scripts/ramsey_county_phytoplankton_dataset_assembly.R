'---
#' title: "Phyto_data_cleaning"
#' author: "Katie Polik"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script takes an aggregated Ramsey county phytoplankton clean(ish) csv with all the cell count concentration data and rearranges and visualizes it. There are still a few data issues that we may need to check with John Manske about, but I'm pretty sure I've caught and corrected the vast majority of the typos at this point
#' 


#' # Document Preamble
#+ warning = FALSE

# load libraries ------------------------------------------------------------------
# # Load Libraries


library(tidyverse)
library(lubridate)
library(ggplot2)


# load in data -------------------------------------------------
# # 
CellCounts <- read.csv("data/Ramsey_Phyto_CellCounts.csv")
PhytoID <- read.csv("PhytoID.csv")

## Data Wrangling  -------------------------
# Transform data into long format
sp_vec <- select(CellCounts, starts_with("NAME")) %>% as.matrix() %>% t() %>% as.vector()
co_vec <- select(CellCounts, starts_with("CONC")) %>% as.matrix() %>% t() %>% as.vector()
col_num <- length(names(select(CellCounts, starts_with("NAME"))))

CellCounts.long <- tibble(DNRID = rep(unlist(CellCounts[,"DNRID"]), each = col_num),
                          DATE = rep(unlist(CellCounts[,"DATE"]), each = col_num),
                          SITE = rep(unlist(CellCounts[,"SITE"]), each = col_num),
                          NAME = sp_vec,
                          CONC.NO.ML = co_vec) %>% 
                    subset(NAME != "#N/A") %>%
                    mutate(CONC.NO.ML = as.numeric(CONC.NO.ML))

# Transform data into wide format
CellCounts.wide <- CellCounts.long %>%
  pivot_wider(id_cols = c(DNRID, DATE, SITE), 
              names_from = NAME, 
              values_from = CONC.NO.ML, 
              values_fn = sum)

#NOTE Sometimes a single taxa will have multiple count entries for one site/date.
#You can find the duplicates by going into CellCounts.wide and searching for ","
#for now we solve this by summing, making the assumption that these counts are split for some reason?

## Group Counts --------------------------
# I have checked that all of the taxa (92) in CellCounts.wide are also in PhytoID. 
# If more taxa get added to CellCounts.wide, you should check again.
# (e.g. when we add the 2023 data)

# subset the phytoIDs to only be the taxa present in the cell counts
TAXA.counts <- names(CellCounts.wide)
TAXA.ids <- as.vector(PhytoID$NAME)
unused.taxa <- setdiff(TAXA.ids, TAXA.counts)
PhytoID.sub <- PhytoID[! PhytoID$NAME %in% unused.taxa,2:3]
PhytoID.sub <- PhytoID.sub[!duplicated(PhytoID.sub),] #some taxa have two IDs??
                
# make vectors of taxa names within each group
cyanophyta <- as.vector(subset(PhytoID.sub, GROUP == "CYANOPHYTA (BLUE-GREEN)")$NAME)
chlorophyta <- as.vector(subset(PhytoID.sub, GROUP == "CHLOROPHYTA (GREEN)")$NAME)
diatom <- as.vector(subset(PhytoID.sub, GROUP == "BACILLARIOPHYCEAE (DIATOM)")$NAME)
euglenophyta <- as.vector(subset(PhytoID.sub, GROUP == "EUGLENOPHYTA (EUGLENOIDS)")$NAME)
dinoflag <- as.vector(subset(PhytoID.sub, GROUP == "PYRRHOPHYTA (DINOFLAGELLATES)")$NAME)
cryptophyta <- as.vector(subset(PhytoID.sub, GROUP == "CRYPTOPHYTA (CRYPTOMONADS)")$NAME)
chrysophyta <- as.vector(subset(PhytoID.sub, GROUP == "CHRYSOPHYTA (YELLOW-GREEN)")$NAME)
taxa.groups <- c("CYANOS", "GREEN", "DIATOM" , "EUGLENA", "DINOS", "CRYPTOS", "CHRYSOS")

# Calculate Group Counts
CellCounts.groups <- CellCounts.wide %>% 
  mutate(CYANOS = rowSums(.[cyanophyta], na.rm = T),
         GREEN = rowSums(.[chlorophyta], na.rm = T),
         DIATOM = rowSums(.[diatom], na.rm = T),
         EUGLENA = rowSums(.[euglenophyta], na.rm = T),
         DINOS = rowSums(.[dinoflag], na.rm = T),
         CRYPTOS = rowSums(.[cryptophyta], na.rm = T),
         CHRYSOS = rowSums(.[chrysophyta], na.rm = T)) %>%
  mutate(ALL.CELLS = rowSums(.[taxa.groups], na.rm = T)) %>%
  mutate(CYANOS.rel = CYANOS/ALL.CELLS, 
         GREEN.rel = GREEN/ALL.CELLS, 
         DIATOM.rel = DIATOM/ALL.CELLS, 
         EUGLENA.rel = EUGLENA/ALL.CELLS, 
         DINOS.rel = DINOS/ALL.CELLS, 
         CRYPTOS.rel = CRYPTOS/ALL.CELLS, 
         CHRYSOS.rel = CHRYSOS/ALL.CELLS )

# here is where we could also add calculations of diversity  
# simplify data into smaller dataframe if we only care about groups
GroupCounts <- CellCounts.groups %>%
  select(DNRID, DATE, SITE, ALL.CELLS, all_of(taxa.groups), 
         CYANOS.rel, GREEN.rel, DIATOM.rel, EUGLENA.rel, DINOS.rel, CRYPTOS.rel, CHRYSOS.rel)
  
## Data playtime! ----------------------------------

#Note that cell counts aren't always a great way to think about data
#Most people prefer biovolume (I think)
#We should decided if we absolutely need biovolume or if we can get away with just cell counts.

ISLAND <- GroupCounts %>%
  subset(DNRID == "62-0075" & DATE <= "20120000" & DATE >= "20100000") %>%
  select(DNRID, DATE, SITE, all_of(taxa.groups))

ISLAND.long <- ISLAND %>%
  pivot_longer(cols = all_of(taxa.groups), names_to = "group") %>%
  mutate(DATE = ymd(DATE)) %>%
  mutate(YEAR = year(DATE),
         month_day = as_date(paste(2023,month(DATE),day(DATE), sep = "-")))

ggplot(ISLAND.long, aes(fill=group, y=value, x=DATE)) + 
  geom_bar(position="fill", stat="identity") #+
  #facet_wrap(~YEAR)

#my takeaway from playing with data briefly is that stacked bar-chart timeseries are slightly annoying to plot because of all the winter time gaps
#I sort of tried to do this with facets but didn't try very hard (and it didn't work)
           
  



