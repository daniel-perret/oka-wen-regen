####### Initial FIA filtering for regeneration plots on the Oka-Wen #######
###########################################################################
# D. Perret, 13 Feb 2026


## load FIA data for WA state ####

fia <- rFIA::readFIA(dir = "data/WA_FIA_021326/",states = "WA", common = T)

# fia <- rFIA::readFIA(dir = "/Users/DanielPerret/Box/01. daniel.perret Workspace/FIA_DATA/fia_data_092525/",states = c("WA","OR","ID","MT"), common = T)

####### creating some fields for convenience ####

fia$PLOT <- fia$PLOT %>% 
  mutate(pltID = paste(UNITCD,STATECD,COUNTYCD,PLOT,sep="_"),
         PLT_CN = CN) %>% 
  group_by(pltID) %>% 
  mutate(most.recent = ifelse(MEASYEAR==max(MEASYEAR),
                              1,0)) %>% 
  ungroup()

fia$COND <- fia$COND %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,PREV_PLT_CN,most.recent),
            by="PLT_CN")

fia$SEEDLING <- fia$SEEDLING %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,PREV_PLT_CN,most.recent),
            by="PLT_CN")

fia$TREE <- fia$TREE %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,most.recent, MEASYEAR),
            by="PLT_CN") %>% 
  mutate(TRE_CN = CN,
         agent_key = case_when(STATUSCD==2 & AGENTCD %in% c(00,70) ~ "unknown1",
                               STATUSCD==2 & AGENTCD == 10 ~ "insect",
                               STATUSCD==2 & AGENTCD == 20 ~ "disease",
                               STATUSCD==2 & AGENTCD == 30 ~ "fire",
                               STATUSCD==2 & AGENTCD == 40 ~ "animal",
                               STATUSCD==2 & AGENTCD == 50 ~ "weather",
                               STATUSCD==2 & AGENTCD == 60 ~ "competition",
                               STATUSCD==2 & AGENTCD == 80 ~ "land use",
                               STATUSCD==2 & is.na(AGENTCD) & 
                                 (PREV_STATUS_CD==1 | 
                                    is.na(PREV_STATUS_CD)) ~ "unknown2"),
         insect.damage = case_when(DAMAGE_AGENT_CD1 >= 10000 &
                                     DAMAGE_AGENT_CD1 < 19000 ~ 1,
                                   DAMAGE_AGENT_CD2 >= 10000 &
                                     DAMAGE_AGENT_CD2 < 19000 ~ 1,
                                   DAMAGE_AGENT_CD3 >= 10000 &
                                     DAMAGE_AGENT_CD3 < 19000 ~ 1,
                                   TRUE ~ 0),
         disease.damage = case_when(DAMAGE_AGENT_CD1 >= 20000 &
                                      DAMAGE_AGENT_CD1 < 30000 ~ 1,
                                    DAMAGE_AGENT_CD2 >= 20000 &
                                      DAMAGE_AGENT_CD2 < 30000 ~ 1,
                                    DAMAGE_AGENT_CD3 >= 20000 &
                                      DAMAGE_AGENT_CD3 < 30000 ~ 1,
                                    TRUE ~ 0),
         other.damage = case_when(DAMAGE_AGENT_CD1 > 30000 ~ 1,
                                  DAMAGE_AGENT_CD2 > 30000 ~ 1,
                                  DAMAGE_AGENT_CD3 > 30000 ~ 1,
                                  TRUE ~ 0)) %>% 
  # this bit of code corrects for instances where a tree is assigned one species at T1 and another at T2 --> we assume the first was incorrect (easy for saplings)
  left_join(.,
            fia$TREE %>% 
              select(PREV_TRE_CN, SPCD) %>% 
              rename(LATER_SPCD=SPCD),
            by=c("TRE_CN"="PREV_TRE_CN")) %>% 
  mutate(SPCD = case_when(SPCD!=LATER_SPCD & !is.na(LATER_SPCD) ~ LATER_SPCD,
                          is.na(LATER_SPCD) ~ SPCD,
                          TRUE ~ SPCD))


## reproducing Emmerson's filtering logic ####

## filtering for disturbed plots ####

########### my thought here is to make a couple different levels of what we consider disturbed -- based on condition class (most restrictive) down to AGENTCD + DAMTYP + COND (least restrictive)

# based on COND$DSTRBCD
dist.plt <- fia$COND %>% 
  filter(most.recent == T, # we can think about this
         DSTRBCD1 %in% c(30:32) |
           DSTRBCD2 %in% c(30:32) |
           DSTRBCD3 %in% c(30:32),
         TRTCD1 != 30 &
           TRTCD2 != 30 &
           TRTCD3 != 30) %>% 
  select(PLT_CN) %>% 
  distinct() %>% 
  mutate(seed.pres = ifelse(PLT_CN %in% fia$SEEDLING$PLT_CN, 1, 0))
# expand out to make larger summary of plot, fire disturbance id type, and seedling presence

single.cond <- fia$COND %>% 
  filter(CONDPROP_UNADJ == 1) %>% 
  pull(PLT_CN)

cond.plt <- fia$COND %>% 
  filter(DSTRBCD1 %in% c(30:32) |
           DSTRBCD2 %in% c(30:32) |
           DSTRBCD3 %in% c(30:32)) %>% 
  pull(PLT_CN) %>% 
  unique()

trt.plt <- fia$COND %>% 
  filter(TRTCD1 == 30 |
           TRTCD2 == 30 |
           TRTCD3 == 30) %>% 
  pull(PLT_CN) %>% 
  unique()

mort.plt <- fia$TREE %>% 
  filter(AGENTCD == 30) %>% 
  pull(PLT_CN) %>% 
  unique()

dam.plt <- fia$TREE %>% 
  filter(DAMAGE_AGENT_CD1 == 30000 |
           DAMAGE_AGENT_CD2 == 30000 |
           DAMAGE_AGENT_CD3 == 30000) %>% 
  pull(PLT_CN) %>% 
  unique()

plt.cd <- fia$PLOT %>% 
  filter(most.recent == 1, #only include most recent measurement
         !PLT_CN %in% trt.plt) %>% 
  select(PLT_CN) %>% 
  mutate(single.cond = ifelse(PLT_CN %in% single.cond, 1, 0),
         cond.fire = ifelse(PLT_CN %in% cond.plt, 1, 0),
         mort.fire = ifelse(PLT_CN %in% mort.plt, 1, 0),
         dam.fire = ifelse(PLT_CN %in% dam.plt, 1, 0),
         all.fire = cond.fire+mort.fire+dam.fire,
         seed.pres = ifelse(PLT_CN %in% fia$SEEDLING$PLT_CN, 1, 0))
  



























