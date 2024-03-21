# LENS medication analysis------
# Guilherme Amorim
# 12/04/22

# 0. Libraries and tools--------

# database connection
library(odbc) # connection to LENS database
library(DBI) # connection to LENS database
library(RODBC) # connection to LENS database
library(Rcpp) # connection to LENS database

# importing and manipulating data
library(readxl) # import excel data
library(tidyverse) # data manipulation
library(lubridate) # handling dates
library(magrittr) # %<>% operator
library(skimr) # rapid summaries
library(scales) # to ease some calculations

# plotting
library(patchwork) # combining ggplots
library(ggridges) # ridgeplots for ggplot
library(viridis) # color scheme
library(ggrepel) # repel labels in plots
library(ggdark) # for oxpop theme
library(ggthemes) # for oxpop theme
library(ggside) # produce plots on the side of the main plot
library(ggalluvial) # alluvial plots (changes along time)
library(grDevices) # color handling
library(RColorBrewer) # color handling
library(palettes) # color handling

# exporting tables
library(tableone) # for generating nicely formatted tables
library(gtsummary) # summary word tables 
library(flextable) # handling word tables
source("Tools/Table templates/customtab.R") # some tools to customize flextables, from https://www.r-bloggers.com/2021/11/publication-ready-tables-with-flextable-and-own-theme-in-r/
customtab_defaults() 
library(officer) # exporting word tables


# stats
library(fmsb) # Kappa calculations
library(irr) #  intraclass correlation coefficient

# misc
library(beepr) # produce sound after running code chunk

# complete session info (list of packages loaded and respective versions) 
sessionInfo()


##### 0.1 saving and loading workspace------
# save.image("ens_workspace.Rdata")
# load("lens_workspace.Rdata")

##### 0.2 Default plotting functions -------

update_geom_defaults("text", list(colour="black",
                                  family="Mulish"))

theme_set(theme_gray(base_size=25))
theme_update(text=element_text(family="Mulish"))

# ggplot <- function(...) ggplot2::ggplot(...) 
  # scale_fill_viridis(discrete = T) +  
  # scale_color_viridis(discrete = T)




# Oxpop theme for powerpoint
oxpop_blue_panel<- (
  # dark_mode(
    #theme_fivethirtyeight(base_size = 20))+
    theme(plot.background = element_rect(fill = "transparent", color=NA), # the color argument removes white margin
          panel.background = element_rect(fill = "transparent"), # for ppt BG color use #0d1d41
          
          panel.grid.major = element_line(color = "grey90", size=0.1, linetype = "dashed"),
          panel.grid.minor = element_line(color = "grey90", size=0.1, linetype = "dashed"),
          
          strip.background = element_blank(),
          legend.background = element_blank(),
          
          axis.title.y = element_text(family="Mulish",
                                      color = "white"),
          axis.title.x = element_text(family="Mulish",
                                      color = "white"),
          axis.text.y = element_text(family="Mulish",
                                      color = "white"),
          axis.text.x = element_text(family="Mulish",
                                      color = "white"),
          plot.subtitle = element_text(hjust=0),
          plot.caption = element_text(hjust=0),
          
          strip.text = element_text(size=20, color="white"),
          
          axis.ticks = element_line(color="white"),
          
          text = element_text(family="Mulish",color = "White", size=25),
          panel.border = element_blank()
          )
)






# 1. Connection -----------
con <- dbConnect(odbc::odbc(), "lens_mirror_64", 
                 timeout = 10,
                 uid="guilhermep",
                 pwd="DuducasBebe!2023")

# 2. Data extraction ------------

# list of tables and variables needed

# PARTICIPANTS
# participants: participant_id, dob, is_female, centre_id
# randomization_record: pt_id, rand_date
# centre: centre_id, centre_name, address_id
# rand_medical_event_status (comorbidities): participa                                                   nt_id, laser_treat_retinopathy, vitrectomy, eye_injection, myocardial_infarction, stroketia, coronary_bypass, coronary_angioplasty, other_arterial_stent, amputation
# rand_physical_measurement (measurements): participant_id, sbp_first, sbp_second, dbp_first, dbp_second, pulse_rate, weight, height

# DRUG CATEGORIES
# drugs (DrugsMan dug codes): term_id, read_code, name_u64, category_flag, category_flag2
# drug_cats (DrugsMan category names): cat_id, cat_name
# drug_cat_drugs (DrugsMan category codelists): cat_id, term_id

# DRUG RECORDS
# screening_med_exclusion_criteria (excluded drugs at screening)
# pt_med_form (non-study drugs taken by a participant at randomisation; creates ids for each drug and each visit but does not contain the drug name)
# pt_med_drug (drugs recorded at randomisation; contains an ID and drug code):
# fup_other_meds_status (excluded drugs at follow-up)

# flag_scot_drugs (PIS data)

## 2.1 Drugs table ------

drugs<-dbGetQuery(
  con,
  "select 
  ds.term_id, 
  ds.read_code, 
  nvarchar(deunicode64(name_u64)) as drug_term, 
  dcd.cat_id, 
  dc.cat_name 
  from drugs ds

left join drug_cat_drugs dcd on dcd.term_id = ds.term_id

left join drug_cats dc on dcd.cat_id=dc.cat_id

order by drug_term")

drugs%<>%
  select(cat_name,
         cat_id,
         read_code,
         term_id,
         drug_term)%>%
  arrange(cat_name, drug_term)%>%
  rename(med_drug_code=term_id)


drug_labels_order<-c("Insulin",
                     "Metformin",
                     "Sulphonylureas",
                     "Thiazolideniones",
                     "DPP-4 inhibitors",
                     "GLP-1 agonists",
                     "SGLT2 inhibitors",
                     "Acarbose or similar",
                     "Statins",
                     "Ezetimibe",
                     "PCSK9 inhibitors",
                     "ACE inhibitors",
                     "Angiotensin-receptor blockers",
                     "Mineralocorticoid receptor antagonists",
                     "Beta-adrenergic blockers",
                     "Calcium-channel blockers",
                     "Loop diuretics",
                     "Thiazide diuretics"
)


lens_drugs <- tibble(
  cat_name = c("acarbose or similar",
               "ace inhibitor",
               "aldosterone anta",
               "arb",
               "beta-blocker",
               "biguanide",
               "ca channel block",
               "dpp4 inhibitor",
               "ezetimibe",
               "GLP-1 agonist",
               "Insulin",
               "loop diuretic",
               "PCSK9 inhibitors",
               "SGLT2 Inhibitors",
               "statin",
               "Sulphonylurea",
               "thiazide(like)",
               "thiazolidinedion"),
  labels = c("Acarbose or similar",
             "ACE inhibitors",
             "Mineralocorticoid receptor antagonists",
             "Angiotensin-receptor blockers",
             "Beta-adrenergic blockers",
             "Metformin",
             "Calcium-channel blockers",
             "DPP-4 inhibitors",
             "Ezetimibe",
             "GLP-1 agonists",
             "Insulin",
             "Loop diuretics",
             "PCSK9 inhibitors",
             "SGLT2 inhibitors",
             "Statins",
             "Sulphonylureas",
             "Thiazide diuretics",
             "Thiazolideniones"))%>%
  mutate(labels=factor(labels, levels=drug_labels_order))


contraindications_drugsman <-tibble(
  cat_name=c("cyclosporine",
             "fibrate",
             "LENS_contra",
             "warfarin-type",
             "rosuvastatin"),
  labels=c("Cyclosporine",
           "Fibrates",
           "Contraindicated drugs",
           "Vitamin K antagonists",
           "Rosuvastatin")
)

write_csv(drugs, "Tools/drugsman_categories.csv")

## 2.2 Baseline participant data ----------

participants<-dbGetQuery(
  con,
  "select 
  p.participant_id,
  sf.changed_when as screening_date,
  rand_date,
  dob,
  is_female,
  p.centre_id,
  centre_name,
  address_id,
  sc.diabetes_type,
  s.vitrectomy as vitrectomy_screening,
  s.myocardial_infarction as myocardial_infarction_screening,
  retinal_laser_treat as retinal_laser_treat_screening,
  stroke as stroke_screening,
  tia as tia_screening,
  coronary_artery_bypass as coronary_artery_bypass_screening,
  coronary_stent as coronary_stent_screening,
  other_arterial_revas as other_arterial_revasc_screening,
  lower_limb_ulceration,
  lower_limb_amputation as amputation_screening,
  m.laser_treat_retinopathy retinal_laser_treat_rand,
  m.vitrectomy as vitrectomy_rand,
  m.eye_injection,
  m.myocardial_infarction as myocardial_infarction_rand,
  m.stroketia as stroke_tia_rand,
  m.coronary_bypass as coronary_artery_bypass_rand,
  m.coronary_angioplasty as coronary_stent_rand,
  m.other_arterial_stent as other_arterial_revasc_rand,
  m.amputation as amputation_rand,
  sbp_first,
  sbp_second,
  dbp_first,
  dbp_second,
  pulse_rate,
  weight,
  height
  
  from participants p
  
  left join randomization_record r on p.participant_id = r.pt_id
  
  left join screening_medical_history s on s.participant_id = p.participant_id
  
  left join centre c on c.centre_id = p.centre_id
  
  left join rand_medical_event_status m on m.participant_id = p.participant_id
  
  left join rand_physical_measurement ph on ph.participant_id = p.participant_id
  
  left join screening_form sf on sf.participant_id = p.participant_id
  
  left join screening_inclusion_criteria sc on sc.participant_id=p.participant_id
  
  where rand_date is not null"
)
  
## 2.3 Screening data ------

screening_drugs <-dbGetQuery(
  con,
  "select
  participant_id,
  warfarin,
  rosuvastatin,
  vitamin_k_antagonist,
  cyclosporine,
  colchicine,
  ketoprofen,
  daptomycin,
  fibrate_therapy
  
  from screening_med_exclusion_criteria"
)

## 2.4 Randomisation data ------

rand_drugs <-dbGetQuery(
  con,
  "select
  pt_id as participant_id,
  f.med_id,
  med_drug_code,
  nvarchar(deunicode64(med_drug_text_u64)) as med_drug_text,
  drug_dosage
  
  from pt_med_form f
  
  left join pt_med_drug d on f.med_id = d.med_id
  
  "
)%>%
  mutate(participant_id=as.character(participant_id))

## 2.5 Follow-up data ----

fu_drugs <- dbGetQuery(
  con,
  "select
  participant_id,
  assessment_id,
  changed_when,
  warfarin,
  rosuvastatin,
  vitamin_k_antagonist,
  cyclosporine,
  colchicine,
  ketoprofen,
  daptomycin,
  fibrate_therapy
  
  from fup_other_meds_status
  "
)

fu_drugs%<>%
  mutate(visit_date = str_sub(changed_when, 1, 10))%>%
  group_by(participant_id, visit_date)%>%
  filter(changed_when==max(changed_when))%>%
  ungroup()%>%
  select(-changed_when)


## 2.6 PIS data ------

pis_data <- dbGetQuery(
  con,
  "select 
  pt_id as participant_id,
  approved_name,
  bnf_code,
  disp_ym,
  number_items,
  dispenser_location
  
  from flagging_scot_drugs
  
  "
)%>%
  mutate(participant_id=as.character(participant_id))




## 2.7 BNF dictionary ------

## BNF chapter codelists


file.list <- grep(list.files(path="Tools/Codelists/BNF chapters/", 
                        full.names=T),
                  pattern = "dmd.csv",
                  invert=T,
                  value=T
                        )

df = lapply(file.list, function(i){
  x=read_csv(i, col_types = "cccc")
  x$codelist=i
  x
})

bnf_chapters<-bind_rows(df)

rm(df, file.list)

bnf_chapters%<>%
  select(-codelist)%>%
  mutate(chapter=as.character(str_sub(code, 1, 2)))


## BNF chapter labels

bnf_chapter_labels<-data.frame(chapter=c(1,
                                         2,
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8,
                                         9,
                                         10,
                                         11,
                                         12,
                                         13,
                                         14,
                                         15,
                                         18,
                                         19,
                                         20,
                                         21,
                                         22,
                                         23), 
                               label=c("Gastro-Intestinal System",
                                       "Cardiovascular System",
                                       "Respiratory System",
                                       "Central Nervous System",
                                       "Infections",
                                       "Endocrine System",
                                       "Obstetrics, Gynaecology and Urinary-Tract Disorders",
                                       "Malignant Disease and Immunosuppression",
                                       "Nutrition and Blood",
                                       "Musculoskeletal and Joint Diseases",
                                       "Eye",
                                       "Ear, Nose and Oropharynx",
                                       "Skin",
                                       "Immunological Products and Vaccines",
                                       "Anaesthesia",
                                       "Preparations used in Diagnosis",
                                       "Other Drugs and Preparations",
                                       "Dressings",
                                       "Appliances",
                                       "Incontinence Appliances",
                                       "Stoma Appliances"))






















## 2.8 BNF codelists ------

file.list <- list.files(path="Tools/Codelists/BNF codelists/", full.names=T, pattern = ".xlsx")

df = lapply(file.list, function(i){
  x=read_excel(i, sheet="Codelist")
  x$codelist=i
  x
})

codelists_bnf<-bind_rows(df)

codelists_bnf$codelist<-str_sub(codelists_bnf$codelist,31, -10)

rm(df, file.list)

# remove unnecessary fields and rename code field

codelists_bnf%<>%
  select(-dmd_id, -dmd_name)%>%
  rename(bnf_code = code)

# trim to codelists of interest
codelists_bnf %<>%
  filter(codelist %in% c(
    "acarbose_or_similar",
    "acei",
    "mra",
    #"alpha_blockers",
    #"amiodarone",
    #"antihypertensives",
    # "antidyslipidemics",
    "arb",
    # "aspirin",
    #"atorvastatin",
    "beta_blockers",
    "metformin",
    "ccb",
    #"central_antiHT",
    # "oral_diabetes_drugs",
    "dpp4i",
    "ezetimibe",
    "fibrates",
    #"fluvastatin",
    # "glinides",
    "glp1_agonists",
    "insulin",
    #"other_diuretics",
    # "antidyslipidemics",
    "loop_diuretics",
    #"lovastatin",
    # "nitrates",
    # "noacs",
    # "other_antiplatelets",
    "pcsk9",
    #"other_antianginals",
    #"pitavastatin",
    #"pravastatin",
    # "resins",
    # "rosuvastatin",
    "sglt2",
    #"simvastatin",
    "statins",
    "sulphonylureas",
    "thiazide_diuretics",
    "thiazolideniones",
    #"vasodilator_antiHT",
    #"verapamil,
    "warfarin",
    "ciclosporin",
    "daptomycin",
    "ketoprofen",
    "lens_contraindicated",
    "colchicine",
    "other_anti_vitk",
    "vitk_ant",
    "rosuvastatin"))

contraindications_pis <-tibble(
  codelist=c("fibrates",
             "warfarin",
             "ciclosporin",
             "daptomycin",
             "ketoprofen",
             "lens_contraindicated",
             "colchicine",
             "other_anti_vitk",
             "vitk_ant",
             "rosuvastatin"),
  labels = c(
    "Fibrates",
    "Warfarin",
    "Cyclosporine",
    "Daptomycin",
    "Ketoprofen",
    "Contraindicated drugs",
    "Colchicine",
    "Other anti-vitamin K antagonists",
    "Vitamin K antagonists",
    "Rosuvastatin"
  )
)





### ensure no duplicate codes in each list to avoid overcounting

codelists_bnf%<>%
  group_by(codelist)%>%
  distinct(bnf_code)


## BNF codelist labels

codelist_labels<-tibble(
codelists_bnf%>%distinct(codelist)%>%ungroup()%>%filter(!codelist%in%contraindications_pis$codelist),
labels = c("Acarbose or similar",
           "ACE inhibitors",
           # "Antidyslipidemics",
           "Angiotensin-receptor blockers",
          # "Aspirin",
          "Beta-adrenergic blockers",
          "Calcium-channel blockers",
          "DPP-4 inhibitors",
          "Ezetimibe",
          # "Glinides",
          "GLP-1 agonists",
          "Insulin",
          "Loop diuretics",
          "Metformin",
          "Mineralocorticoid receptor antagonists",
          # "NOACs",
          # "Diabetes drugs (oral)",
          # "Other antiplatelets",
          "PCSK9 inhibitors",
          # "Resins",
          "SGLT2 inhibitors",
          "Statins",
          "Sulphonylureas",
          "Thiazide diuretics",
          "Thiazolideniones"
          # "Vitamin K antagonists"
          )
)%>%mutate(labels=factor(labels, levels = drug_labels_order))


## 2.9 Free-text drug coding -----

free_text_drugs<-
  rand_drugs%>%
  mutate(med_drug_)
  filter(med_drug_code<0)%>%
  select(med_drug_code, med_drug_text)
  
write_csv(free_text_drugs, "Tools/Free text drugs/free_text_drugs_list.csv")

drugs_coded<-drugs%>%
  select(med_drug_code, drug_term)


write_csv(drugs_coded, "Tools/Free text drugs/drugs_coded_list.csv")

free_text_drugs_list_coded <- read_excel("Tools/Free text drugs/free_text_drugs_list_coded_GPA.xlsx",
                                         trim_ws = F)


rand_drugs%<>%
  left_join(free_text_drugs_list_coded)%>%
  mutate(med_drug_code=if_else(med_drug_code<0 & !is.na(coded_term), coded_term, med_drug_code))%>%
  select(-coded_term, -coded_text)


## 2.10 Data audit corrections -----

data_corrections<-dbGetQuery(con,
                             "select * from dc_envelope")%>%
  filter(table_name %in% c("pt_med_drug                     ",
                           "pt_med_form                     ",
                           "screening_medical_history       "))
  

  
  
  
  
  
  

# 3. Data wrangling --------

## 3.1 Age at randomisation ------
participants%<>%
  mutate(age_at_rand = as.numeric(str_sub(as.character(time_length(difftime(as.Date(rand_date), as.Date(dob)),"years")), 0, 2)))


## 3.2 Gender ------

participants%<>%
  mutate(gender=factor(if_else(is_female=="2", "Female", "Male")))%>%
  select(-is_female)

## 3.3 Population size and ID list-------
lens_participants_number<-participants%>%
  distinct(participant_id)%>%
  nrow()

lens_participants_list<-participants%>%
  distinct(participant_id, rand_date)

## 3.4 Right-censoring (August 2021) -------

fu_drugs%<>%
  filter(visit_date<="2021-08-31")


## 3.5 Restrict drugs at randomisation to people with randomisation records ---------


rand_drugs%<>%
  filter(participant_id %in% lens_participants_list$participant_id)



## 3.6 Manually apply data corrections ------

# baseline characteristics

participants%<>%
  mutate(retinal_laser_treat_screening = case_when(participant_id=="2004902" ~ "0",
                                                   participant_id=="2011360" ~ "0",
                                                   participant_id=="2018358" ~ "0",
                                                   .default=as.character(retinal_laser_treat_screening)),
         coronary_artery_bypass_screening = case_when(participant_id == "2012663" ~ "1",
                                                      .default=as.character(coronary_artery_bypass_screening)))

# medications

rand_drugs%<>%
  rbind(
    data.frame(
      participant_id = c("2002523",
                         "2002523",
                         "2011336",
                         "2014651",
                         "2014651",
                         "2014651"),
      med_id = c(NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA),
      med_drug_code = c(1141201730,
                        79377,
                        1140883066,
                        1141201730,
                        1140874744,
                        1140861958),
      med_drug_text = c("metformin",
                        "dapagliflozin",
                        "insulin product",
                        "metformin",
                        "gliclazide",
                        "simvastatin"),
      drug_dosage= c(NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     80)
    )
    
  )
  



# 4. General features of the LENS population-------

# participants
participants%>%
  distinct(participant_id)%>%
  nrow()
  # 1151 participants

# centres
participants%>%
  distinct(centre_id)%>%
  nrow()
  # 11 centres

participants%>%
  count(centre_name)%>%
  arrange(desc(n))


## 4.1 age and gender ------
# 
# participants%>%
#   select(age_at_rand, gender)%>%
#   
#   ggplot()+
#   geom_histogram(aes(age_at_rand, fill=gender), color="black")+
#   labs(title="Age of LENS participants",
#        subtitle="Split by gender",
#        x="Age at randomisation",
#        y="Count",
#        fill="Gender",
#        color="Gender",
#        caption="Dashed lines depict mean per gender")+
#   scale_x_continuous(limits=c(0, 100), breaks=seq(0,100, by=20))+
#   geom_vline(data=participants%>%
#                group_by(gender),
#              aes(xintercept = mean(age_at_rand), color=gender), linetype="dashed", size=1, alpha=0.5)+
#   annotate(geom="text",
#            x=20,
#            y=150, label = paste0("Mean (Female): ", round(mean(participants$age_at_rand[participants$gender=="Female"]), 1), 
#                                 "\nMean (Male): ", round(mean(participants$age_at_rand[participants$gender=="Male"]), 1)),
#            size=6)+
#   theme_gray(base_size=20)+
#   theme(legend.position="bottom")
# 
# ggsave("Outputs/Figures/age_vs_gender_histogram.png",
#        width = 20,
#        height=10)

## 4.2 run-in time between screening and randomisation --------

participants%>%
  select(screening_date, rand_date)%>%
  mutate(run_in_duration = difftime(rand_date, screening_date, units="days"))->run_in

quantile(run_in$run_in_duration)
  # median: 62; IQR: 52-70


  
run_in%>%
  ggplot(aes(run_in_duration))+
  geom_density(fill="orange", color=NA)+
  geom_xsideboxplot(orientation="y", color="white", fill="orange")+
  ggside(x.pos="bottom", collapse="x")+
  scale_xsidey_discrete()+
  oxpop_blue_panel+
  theme(panel.grid.minor = element_blank())+
  labs(title="Run-in period duration distribution",
       x="Days",
       y="Density")+
  geom_vline(aes(xintercept=median(run_in_duration, na.rm=T)), linetype="dashed")+
  annotate(geom="text", x=median(run_in$run_in_duration), y = 0.03, label=paste0("Median: ", round(median(run_in$run_in_duration, na.rm=T), 1)), size=6)+
  scale_x_continuous(limits = c(0, NA))
  

ggsave("Outputs/Figures/run_in_density.png",
       width = 20,
       height= 10)

rm(run_in)

## 4.3 summary baseline characteristics -----

# compute baseline characteristics by merging events from screening and rand forms

participants%>%
  transmute(participant_id = participant_id,
            rand_date = rand_date,
            Age=age_at_rand,
            Gender=gender,
            centre_id = centre_id,
            centre_name=centre_name ,
            address_id = address_id,
            `Diabetes type`=factor(case_when(diabetes_type==0 ~ "Type 1",
                                      diabetes_type==1 ~ "Type 2",
                                      diabetes_type==2 ~ "Other"),c("Type 1", "Type 2", "Other")),
            Vitrectomy = if_else(vitrectomy_screening == "1" | vitrectomy_rand=="1", "1", "0"),
            `Myocardial infarction` = if_else(myocardial_infarction_screening == "1" | myocardial_infarction_rand=="1", "1", "0"),
            `Retinal laser treatment` = if_else(retinal_laser_treat_screening == "1" | retinal_laser_treat_rand=="1", "1", "0"),
            `Stroke/TIA` = if_else(stroke_screening == "1" | stroke_screening=="1" | stroke_tia_rand=="1", "1", "0"),
            `Coronary artery bypass graft` = if_else(coronary_artery_bypass_screening == "1" | coronary_artery_bypass_rand=="1", "1", "0"),
            `Coronary stent` = if_else(coronary_stent_screening == "1" | coronary_stent_rand=="1", "1", "0"),
            `Other arterial revascularisation` = if_else(other_arterial_revasc_screening == "1" | other_arterial_revasc_rand=="1", "1", "0"),
            `Lower limb ulceration` = lower_limb_ulceration,
            `Lower limb amputation` = if_else(amputation_screening == "1" | amputation_rand=="1", "1", "0"),
            `Systolic blood pressure` = sbp_first+sbp_second/2,
            `Diastolic blood pressure` = dbp_first+dbp_second/2,
            `Heart rate` = pulse_rate,
            weight=weight,
            height=height/100,
            `Body mass index` = round(weight/(height^2), 1))%>%

# baseline table
  mutate(linked = if_else(participant_id %in% pis_data$participant_id, "Linked", "Non-linked"))%>%
  select(-c(participant_id, centre_id, rand_date, centre_name, address_id, weight, height))%>%
  select(Age,
         linked,
         Female = Gender,
         `Diabetes type`,
         `Myocardial infarction`,
         `Stroke/TIA`,
         `Coronary artery bypass graft`,
         `Coronary stent`,
         `Other arterial revascularisation`,
         `Lower limb ulceration`,
         `Lower limb amputation`,
         `Vitrectomy`,
         `Retinal laser treatment`)%>%

  tbl_summary( statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
               value=list(Female ~"Female",
                          `Vitrectomy` ~"1",
                          `Myocardial infarction`  ~ "1",
                          `Retinal laser treatment` ~"1",
                          `Stroke/TIA`~"1",
                          `Coronary artery bypass graft`~"1",
                          `Coronary stent`~"1",
                          `Other arterial revascularisation` ~"1",
                          `Lower limb ulceration` ~"1",
                          `Lower limb amputation` ~ "1"),
               by="linked")%>%
  add_overall()%>%

  custom_tab_gtsummary(header="Baseline characteristics of the LENS population",
             footer=NULL)%>%
  width(j=1, width = 3)%>%
  width(j=2, width=2)->baseline_characteristics_table


baseline_characteristics_table

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(baseline_characteristics_table, 
             path="Outputs/Tables/baseline_characteristics_table.docx",
             pr_section=sect_properties)
  
  

## THESIS TABLE ------

participants%>%
  transmute(participant_id = participant_id,
            rand_date = rand_date,
            Age=age_at_rand,
            Gender=gender,
            centre_id = centre_id,
            centre_name=centre_name ,
            address_id = address_id,
            `Diabetes type`=factor(case_when(diabetes_type==0 ~ "Type 1",
                                             diabetes_type==1 ~ "Type 2",
                                             diabetes_type==2 ~ "Other"),c("Type 1", "Type 2", "Other")),
            Vitrectomy = if_else(vitrectomy_screening == "1" | vitrectomy_rand=="1", "1", "0"),
            `Myocardial infarction` = if_else(myocardial_infarction_screening == "1" | myocardial_infarction_rand=="1", "1", "0"),
            `Retinal laser treatment` = if_else(retinal_laser_treat_screening == "1" | retinal_laser_treat_rand=="1", "1", "0"),
            `Stroke/TIA` = if_else(stroke_screening == "1" | stroke_screening=="1" | stroke_tia_rand=="1", "1", "0"),
            `Coronary artery bypass graft` = if_else(coronary_artery_bypass_screening == "1" | coronary_artery_bypass_rand=="1", "1", "0"),
            `Coronary stent` = if_else(coronary_stent_screening == "1" | coronary_stent_rand=="1", "1", "0"),
            `Other arterial revascularisation` = if_else(other_arterial_revasc_screening == "1" | other_arterial_revasc_rand=="1", "1", "0"),
            `Lower limb ulceration` = lower_limb_ulceration,
            `Lower limb amputation` = if_else(amputation_screening == "1" | amputation_rand=="1", "1", "0"),
            `Systolic blood pressure` = sbp_first+sbp_second/2,
            `Diastolic blood pressure` = dbp_first+dbp_second/2,
            `Heart rate` = pulse_rate,
            weight=weight,
            height=height/100,
            `Body mass index` = round(weight/(height^2), 1))%>%
  
  # baseline table
  mutate(linked = if_else(participant_id %in% pis_data$participant_id, "Linked", "Non-linked"))%>%
  select(-c(participant_id, centre_id, rand_date, centre_name, address_id, weight, height))%>%
  select(Age,
         linked,
         Female = Gender,
         `Diabetes type`,
         `Myocardial infarction`,
         `Stroke/TIA`,
         `Coronary artery bypass graft`,
         `Coronary stent`,
         `Other arterial revascularisation`,
         `Lower limb ulceration`,
         `Lower limb amputation`,
         `Vitrectomy`,
         `Retinal laser treatment`)%>%
  
  tbl_summary( statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
               value=list(Female ~"Female",
                          `Vitrectomy` ~"1",
                          `Myocardial infarction`  ~ "1",
                          `Retinal laser treatment` ~"1",
                          `Stroke/TIA`~"1",
                          `Coronary artery bypass graft`~"1",
                          `Coronary stent`~"1",
                          `Other arterial revascularisation` ~"1",
                          `Lower limb ulceration` ~"1",
                          `Lower limb amputation` ~ "1"),
               by="linked")%>%
  add_overall()%>%
  
  # custom_tab_gtsummary(header="Baseline characteristics of the LENS population",
  #                      footer=NULL)%>%
  as_flex_table()%>%
  width(j=1, width = 3)%>%
  width(j=2, width=2)->baseline_characteristics_table


baseline_characteristics_table

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

save_as_docx(baseline_characteristics_table, 
             path="Outputs/Thesis/Tables/baseline_characteristics_table.docx",
             pr_section=sect_properties)


rm(baseline_characteristics_table)



  
# how many people have drugs recorded at randomisation?
 
rand_drugs%>%
  distinct(participant_id)%>%
  nrow() # 1133

# do people with no PIS data have drugs recorded at randomisation?

setdiff(
  # people in lens
  lens_participants_list%>%
  select(participant_id)%>%
  .[[1]],
  
  # people in PIS
  pis_data%>%
    distinct(participant_id)%>%
    .[[1]]
) -> non_linked_participants
  

rand_drugs%>%
  filter(participant_id %in% non_linked_participants)%>%
  View()
  
## 4.4 Follow-up period duration --------

fu_drugs%>%
  select(participant_id, visit_date)%>%
  left_join(participants)%>%
  mutate(fu_duration = difftime(visit_date, rand_date, "days"))%>%
  group_by(participant_id)%>%
  arrange(desc(fu_duration))%>%
  slice_head(n=1)%>%
  select(participant_id, fu_duration)->fu_duration

quantile(fu_duration$fu_duration) 
  # IQR for follow-up duration: 512.5-776
  # median: 646.00 (restricted to records before August 2021)

fu_duration%>%
  ggplot(aes(fu_duration))+
  geom_density(fill="orange", color=NA)+
  oxpop_blue_panel+
  labs(title="Follow-up period distribution",
       x="Days",
       y="Density",
       caption="Follow-up period duration calculated based on the most recent follow-up visit for each participant")+
  scale_x_continuous(limits=c(0,NA))+
  geom_vline(aes(xintercept=median(fu_duration, na.rm=T)), linetype="dashed")+
  annotate(geom="text", x=median(fu_duration$fu_duration), y = 0.005, label=paste0("Median: ", round(median(fu_duration$fu_duration, na.rm=T), 1)), size=6)

ggsave("Outputs/Figures/follow_up_density.png",
       height=30,
       width= 60,
       dpi="retina",
       units = "cm")

rm(fu_duration)

## 4.5 randomisation timeseries --------

lens_participants_list%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"))%>%
  group_by(rand_month)%>%
  summarise(Randomisations=n_distinct(participant_id))%>%
  mutate(`Cumulative randomisations` = cumsum(Randomisations),
         rand_month=as.Date(rand_month))%>%
  
  ggplot(aes(rand_month, `Cumulative randomisations`, group=1))+
  geom_point(color="orange")+
  geom_line(color="orange")+
  labs(x="Month",
       title="Cumulative randomisations",
       y="Participants")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%Y %B")+
  oxpop_blue_panel+
  theme(axis.text.x = element_text(angle=30, hjust=1),
        panel.grid.minor = element_blank())

ggsave("Outputs/Figures/cumulative_randomisations.png",
       height=30,
       width= 60,
       dpi="retina",
       units = "cm")




# 5. Explore PIS data ---------

colnames(pis_data)

nrow(pis_data) # 94775

pis_data%>%
  distinct(participant_id)%>%
  nrow()
  # 1147 participants

## 5.1 Dispenser location -------

pis_data%>%
  group_by(dispenser_location)%>%
  summarise(records = n())%>%
  ungroup()%>%
  mutate(proportion=round(records/sum(records)*100, 1))%>%
  mutate(dispenser_location=if_else(dispenser_location=="CP", "Community pharmacy", "Dispensing doctor"))%>%
  
  ggplot(aes(dispenser_location, records, fill=dispenser_location))+
  geom_bar(stat="identity", width=0.2)+
  geom_text(aes(label=paste0(records, " (", proportion, "%)")), size=6, vjust=-1)+
  oxpop_blue_panel+
  theme(legend.position = "none")+
  labs(title="Distribution of dispensing locations",
       x="Location",
       y="Number of records")

ggsave("Outputs/Figures/dispensing_location.png",
       width=25,
       height=10,
       dpi="retina")


## 5.2 Dispensing date (TIMELINESS) -----

### Versus screening -------

participants%>%
  mutate(screening_date=as.Date(paste0(str_sub(screening_date, 1, 8), "01")))%>%
  count(screening_date)%>%
  ggplot(aes(screening_date, n))+
  geom_point(color="orange")+
  geom_line(color="orange")+
  labs(title= "Timeseries of monthly dispensing events and screening visits (for randomised LENS participants)",
       subtitle="Screening visits",
       x="Screening month",
       y="Number of participants")+
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2022-05-01")),
               date_minor_breaks = "1 month")+
  geom_vline(aes(xintercept=as.Date("2022-04-05")), size=1.5, color="orange")+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  annotate(geom="text", x=as.Date("2022-04-05"), y = 75, label=paste0("Date PIS data received\n(04/04/22)"), size=6)+
scale_y_continuous(limits=c(0, NA))+
  oxpop_blue_panel->p2
  

pis_data%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  count(disp_ym)%>%
  
  ggplot(aes(disp_ym, n), color="orange")+
  geom_point(color="orange")+
  geom_line(color="orange")+
  geom_vline(aes(xintercept=as.Date("2022-04-05")), size=1.5, color="orange")+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  annotate(geom="text", x=as.Date("2022-04-05"), y = 3000, label=paste0("Date PIS data received\n(04/04/22)"), size=6)+
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2022-05-01")),
               date_minor_breaks = "1 month" )+
  labs(
    subtitle="Dispensing records",
    x="Dispensing month",
    y="Number of records",
    # caption="The vertical line depicts the date in which the PIS data were received (2022-04-05)"
    )+
  oxpop_blue_panel->p1



p2/p1&theme(plot.background = element_rect(fill="transparent", color="transparent"))

ggsave("Outputs/Figures/timeseries_dispensing_screening.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")

rm(p1,p2)

### versus randomisation --------

#### slides ------

pis_data%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  count(disp_ym)%>%
  
  ggplot(aes(disp_ym, n), color="orange")+
  geom_point(color="orange")+
  geom_line(color="orange")+
  geom_vline(aes(xintercept=as.Date("2022-04-05")), size=1.5, color="orange")+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  annotate(geom="text", x=as.Date("2022-04-05"), y = 3000, label=paste0("PIS data received\n(04/04/22)"), size=6, color="white")+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%Y %B",
               # expand=expansion(c(0.1,0.1))
               )+
  labs(subtitle="Dispensing records",
       x="Month",
       y="Number of records",
       # caption="The vertical line depicts the date in which the PIS data were received (2022-04-05)"
  )+
  oxpop_blue_panel+
  theme(axis.text.x = element_text(angle=30, hjust=1),
        plot.margin = unit(c(0,2,0,2), "cm"))->p2

lens_participants_list%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"))%>%
  group_by(rand_month)%>%
  summarise(Randomisations=n_distinct(participant_id))%>%
  mutate(`Cumulative randomisations` = cumsum(Randomisations),
         rand_month=as.Date(rand_month))%>%
  
  ggplot(aes(rand_month, `Cumulative randomisations`, group=1))+
  geom_point(color="orange")+
  geom_line(color="orange")+
  labs(title="Monthly counts of PIS dispensing records and cumulative randomisations",
       x="Month",
       subtitle="Cumulative randomisations",
       y="Participants")+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%Y %B",
               # expand=expansion(c(0.1,0.1))
               )+
  # scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2022-05-01")),
  #              date_minor_breaks = "1 month")+
  geom_vline(aes(xintercept=as.Date("2022-04-05")), size=1.5, color="orange")+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  annotate(geom="text", x=as.Date("2022-04-05"), y = 900, label=paste0("PIS data received\n(04/04/22)"), size=6, color="white")+
  scale_y_continuous(limits=c(0, NA))+
  oxpop_blue_panel+
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,2,0,2), "cm"))->p1






p1/p2&theme(plot.background = element_rect(fill="transparent", color="transparent"))

ggsave("Outputs/Figures/timeseries_dispensing_randomisation.png",
       last_plot(),
       width=60,
       height=30,
       units = "cm",
       dpi="retina")

#### thesis (SUPPLEMENTAL FIGURE 1) ------


lens_participants_list%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"))%>%
  group_by(rand_month)%>%
  summarise(Randomisations=n_distinct(participant_id))%>%
  mutate(`Cumulative randomisations` = cumsum(Randomisations),
         rand_month=as.Date(rand_month))%>%
  
  ggplot(aes(rand_month, `Cumulative randomisations`, group=1))+
  geom_point()+
  geom_line()+
  labs(x="Month",
       subtitle="Randomisations (cumulative)",
       y="Participants")+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%Y %B",
               limits = c(as.Date("2018-06-01", format="%Y-%m-%d"), as.Date("2022-06-01", format="%Y-%m-%d")))+
  # scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2022-05-01")),
  #              date_minor_breaks = "1 month")+
  # geom_vline(aes(xintercept=as.Date("2022-04-05")), size=1.5)+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  scale_y_continuous(limits=c(0, NA))+
  theme(
    axis.text.x = element_blank(),
    axis.title.x=element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=30))->p1



pis_data%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  count(disp_ym)%>%
  
  ggplot(aes(disp_ym, n))+
  geom_point()+
  geom_line()+
  # geom_vline(aes(xintercept=as.Date("2022-04-05")), size=1.5)+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  annotate(geom="text", x=as.Date("2022-01-05"), y = 3000, label=paste0("PIS data reception\n(04/04/22)"), size=10)+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%Y %B",
               limits = c(as.Date("2018-06-01", format="%Y-%m-%d"), as.Date("2022-06-01", format="%Y-%m-%d")))+
  labs(
    # title="Monthly counts of PIS dispensing records and cumulative randomisations",
       subtitle="Dispensing records (each month)",
       x="Month",
       y="Number of records",
       # caption="The vertical line depicts the date in which the PIS data were received (2022-04-05)"
  )+
  theme(
        axis.text.x = element_text(angle=30, hjust=1),
        text=element_text(size=30))->p2



p1/p2&theme(plot.background = element_rect(fill="transparent", color="transparent"))

ggsave("Outputs/Thesis/Figures/timeseries_dispensing_randomisation.png",
       last_plot(),
       width=30,
       height=20,
       dpi="retina")


ggsave("Outputs/Thesis/Figures/HR/timeseries_dispensing_randomisation.tiff",
       last_plot(),
       width=30,
       height=20,
       dpi="retina")








rm(p1,p2)














### Versus follow-up -------

fu_drugs%>%
  mutate(visit_date=as.Date(paste0(str_sub(visit_date, 1, 8), "01")))%>%
  count(visit_date)%>%
  ggplot(aes(visit_date, n))+
  geom_point(color="orange")+
  geom_line(color="orange")+
  labs(title= "Timeseries of monthly dispensing events and follow-up visits",
       subtitle="Follow-up visits",
       x="Visit month",
       y="Number of participants")+
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2022-05-01")),
               date_minor_breaks = "1 month")+
  geom_vline(aes(xintercept=as.Date("2022-04-05")))+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  annotate(geom="text", x=as.Date("2022-04-05"), y = 220, label=paste0("Date PIS data received\n(04/04/22)"), size=6)+
  scale_y_continuous(limits=c(0, NA), expand=expansion(c(0,0.3)))+
  oxpop_blue_panel->p1

pis_data%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  count(disp_ym)%>%
  
  ggplot(aes(disp_ym, n), color="orange")+
  geom_point(color="orange")+
  geom_line(color="orange")+
  geom_vline(aes(xintercept=as.Date("2022-04-05")))+
  geom_vline(aes(xintercept=as.Date("2021-09-01")))+
  geom_vline(aes(xintercept=as.Date("2018-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2019-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2021-01-01")), linetype="dashed")+
  geom_vline(aes(xintercept=as.Date("2022-01-01")), linetype="dashed")+
  annotate(geom="text", x=as.Date("2022-04-05"), y = 4500, label=paste0("Date PIS data received\n(04/04/22)"), size=6)+
  annotate(geom="text", x=as.Date("2021-09-01"), y = 4500, label=paste0("Most recent PIS records\n(September 2021)"), size=6)+
  
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2022-05-01")),
               date_minor_breaks = "1 month" )+
  labs(
    subtitle="Dispensing records",
    x="Dispensing month",
    y="Number of records",
    # caption="The vertical line depicts the date in which the PIS data were received (2022-04-05)"
  )+
  oxpop_blue_panel+
  scale_y_continuous(limits=c(0, NA), expand=expansion(c(0,0.3)))->p2


p1/p2&theme(plot.background = element_rect(fill="transparent", color="transparent"))

ggsave("Outputs/Figures/timeseries_dispensing_follow_up.png",
       last_plot(),
       width=25,
       height=10,
       dpi="retina")

rm(p1,p2)


















## 5.3 Approved name ------

pis_data%>%
  distinct(approved_name)%>%
  nrow()
# 108 distinct drugs

pis_data%>%
  group_by(approved_name)%>%
  summarise(records = n(),
            participants = n_distinct(participant_id))%>%
  arrange(desc(records))%>%
  select(`Drug name` = approved_name,
         `Number of records` = records,
         `Number of individual participants` = participants)%>%
  View()

## 5.4 BNF code --------

### BNF chapters -----

pis_data%>%
  select(bnf_code, participant_id)%>%
  mutate(bnf_chapter= as.integer(str_sub(bnf_code, 1, 2)))%>%
  group_by(bnf_chapter)%>%
  summarise(Records=n(),
            Participants=n_distinct(participant_id))%>%
  left_join(bnf_chapter_labels, by=c("bnf_chapter"="chapter"))%>%
  pivot_longer(c(Participants, Records), names_to = "key", values_to="value")%>%
  ggplot(aes(label, value, fill=label))+
  geom_col(width=0.5)+
  facet_wrap(~key, nrow=2, scales="free_y")+
  labs(x="BNF chapter",
       y="Count",
       title="Distribution of participant and record counts in the PIS data (per BNF chapter)")+
  geom_text(aes(label=value), vjust=-0.5, size=6)+
  #theme_gray(base_size=25)+
  scale_y_continuous(expand = expansion(0.1, 0.1))+
  oxpop_blue_panel+
  theme(legend.position = "none",
        strip.text.x = element_text(hjust = 0))

ggsave("Outputs/Figures/pis_bnf_chapters.png",
       width=25,
       height=10,
       dpi="retina")


### Specific drugs ------


pis_data%>%
  left_join(bnf_chapters, by=c("bnf_code"="code"))%>%
  select(bnf_code, participant_id, approved_name, term)%>%
  group_by(bnf_code, term, approved_name)%>%
  summarise(Records=n(),
            Participants=n_distinct(participant_id))%>%View()

# 409 distinct formulations




## 5.5 Number_items --------

# counts of records with 1 vs 2 items for each drug
pis_data%>%
  select(approved_name, number_items)%>%
  group_by(approved_name, number_items)%>%
  summarise(records=n())%>%
  arrange(desc(number_items), desc(records))

# counts of records with 1 vs 2 items per drug group
pis_data%>%
  select(number_items, bnf_code)%>%
  inner_join(codelists_bnf, by="bnf_code")%>%
  left_join(codelist_labels, by="codelist")%>%
  group_by(number_items, labels)%>%
  summarise(records=n())%>%
  arrange(desc(number_items), desc(records))

# plot 
pis_data%>%
  select(number_items, bnf_code)%>%
  inner_join(codelists_bnf, by="bnf_code")%>%
  left_join(codelist_labels, by="codelist")%>%
  filter(!codelist%in%contraindications_pis$codelist)%>%
  group_by(number_items, labels)%>%
  summarise(Records=n())%>%
  arrange(desc(number_items), desc(Records))%>%
  mutate(number_items=recode_factor(number_items, `1` = "1 item", `2` = "2 items"))%>%
  
  ggplot(aes(labels, Records, fill=labels))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~number_items, nrow=2, 
             # scales="free_y"
             )+
  oxpop_blue_panel+
  theme(axis.text.x = element_text(angle=30, hjust=1),
        legend.position = "none",
        strip.text=element_text(hjust=0),
        panel.spacing = unit(2, "cm"),
        panel.grid.major = element_blank())+
  geom_text(aes(label=Records), vjust=-0.5, size=6, color="white")+
  labs(x="",
       title="Recording of number of items in each dispensing record (per drug group)")+
  scale_y_continuous(expand = expansion(mul=c(0.0,0.2)))

ggsave("Outputs/Figures/number_items_barchart.png",
       height=32,
       width=62,
       dpi="retina",
       units="cm")
  


# manual inspection of temporal arrays
pis_data%>%
  left_join(bnf_chapters, by=c("bnf_code"="code"))%>%
  group_by(participant_id)%>%
  filter(any(number_items=="2"))%>%
  arrange(participant_id, approved_name, disp_ym)%>%View()


## 5.6 Distinct codes per participant -------


pis_data%>%
  inner_join(codelists_bnf)%>%
  group_by(participant_id, codelist)%>%
  summarise(distinct_codes = n_distinct(bnf_code))%>%
  left_join(codelist_labels)%>%
  ungroup()->distinct_codes

distinct_codes%>%
  mutate(labels=fct_rev(labels))%>%
  filter(codelist!="lens_contraindicated" & !is.na(labels))%>%
  ggplot(aes(distinct_codes, y=labels, fill=labels))+
  geom_density_ridges(alpha=0.5)+
  oxpop_blue_panel+
  theme(legend.position = "none")+
  labs(title="Number of distinct BNF codes per participant",
       y="",
       x="Number of codes",
       fill="",
       caption = "Density plots right-capped at 5")+
  scale_x_continuous(limits=c(0,5))

ggsave("Outputs/Figures/distinct_codes_density.png",
       width=25,
       height=10,
       dpi="retina")

rm(distinct_codes)

## 5.7 Time between events -------

pis_data%>%
  select(participant_id, disp_ym, approved_name, bnf_code, number_items)%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%  
  distinct(participant_id, bnf_code, disp_ym)%>%
  group_by(participant_id, bnf_code)%>%
  filter(n()>1)%>%
  arrange(as.Date(disp_ym))%>%
  group_by(participant_id, bnf_code)%>%
  summarise(average=round(as.numeric(median(diff(disp_ym))),0))%>%
  inner_join(codelists_bnf)%>%
  left_join(codelist_labels, by=("codelist"))%>%
  filter(codelist!="lens_contraindicated" & !is.na(labels))%>%
  ungroup()->timeintervals


#### FIGURE (SLIDES) ------

timeintervals%>%
  mutate(labels=fct_rev(labels))%>%
  ggplot(aes(x=average, fill=labels))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  oxpop_blue_panel+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        # axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1),
        legend.position = "none"
  )+
  labs(x = "Average day difference",
       title="Time interval between consecutive entries",
       caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual BNF code and participant level as the median day difference between consecutive records. Only one record per dispensing date was retained. Vertical lines depict medians per drug group and dataset.", 150),
       y="Drug group")

  
ggsave("Outputs/Figures/time_between_events_density.png",
       height=10,
       width=25,
       dpi="retina")





#### FIGURE (THESIS) ------

timeintervals%>%
  mutate(labels=fct_rev(labels))%>%
  ggplot(aes(x=average, fill=labels))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  scale_x_continuous(breaks=c(0,30,60,90,120,150, 180, 210, 240, 270, 300, 330, 360), limits=c(0, 360))+
  # oxpop_blue_panel+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        # axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1),
        legend.position = "none"
  )+
  labs(x = "Average day difference",
       # title="Time interval between consecutive entries",
       # caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual BNF code and participant level as the median day difference between consecutive records. Only one record per dispensing date was retained. Vertical lines depict medians per drug group and dataset.", 150),
       y="Drug group")


ggsave("Outputs/Thesis/Figures/time_between_events_density.png",
       height=10,
       width=25,
       dpi="retina")

ggsave("Outputs/Thesis/Figures/HR/time_between_events_density.tiff",
       height=10,
       width=25,
       dpi="retina")



## 5.8 Proportions in each time interval --------

##### FIGURE (SLIDES) ------

timeintervals%>%
  filter(codelist!="lens_contraindicated" & !is.na(labels))%>%
  group_by(labels)%>%
  summarise(total = n_distinct(participant_id),
            interval_30 = round(n_distinct(participant_id[average<=30])/total*100,0),
            interval_60 = round(n_distinct(participant_id[average<=60])/total*100,0),
            interval_90 = round(n_distinct(participant_id[average<=90])/total*100,0),
            interval_180 = round(n_distinct(participant_id[average<=180])/total*100,0),
            interval_360 = round(n_distinct(participant_id[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels), names_to="key", values_to="value")%>%
  # mutate(key=fct_relevel(key, levels=c("30", "60", "90", "180", "360")))%>%
  mutate(key=as.numeric(key))%>%

  ggplot(aes(x=as.numeric(key), y=value, color=labels))+
  geom_line(size=2)+
  geom_point(size=4)+
  geom_text(aes(label=paste0(value, "%")),
                  direction="both",
                  nudge_y = 6,
                  size=5,
            color="white"
            )+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             ncol=4)+
  
  oxpop_blue_panel+
  theme(legend.position="none",
        panel.grid.minor = element_blank())+
  
  labs(y="Participants (%)",
       title="Proportions of participants with distinct average time intervals between consecutive entries",
       x="Time between consecutive records (in days)",
       caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group",
  )+
  scale_x_continuous(breaks=c(0, 30,60,90, 180, 360), limits=c(0, NA))+
  scale_y_continuous(breaks=seq(0,100, by=20), expand=expansion(c(0,0.4)))

ggsave("Outputs/Figures/time_interval_proportions.png",
       height=38,
       width=67,
       dpi="retina",
       units="cm")


##### FIGURE (THESIS) ------

timeintervals%>%
  filter(codelist!="lens_contraindicated" & !is.na(labels))%>%
  group_by(labels)%>%
  summarise(total = n_distinct(participant_id),
            interval_30 = round(n_distinct(participant_id[average<=30])/total*100,0),
            interval_60 = round(n_distinct(participant_id[average<=60])/total*100,0),
            interval_90 = round(n_distinct(participant_id[average<=90])/total*100,0),
            interval_180 = round(n_distinct(participant_id[average<=180])/total*100,0),
            interval_360 = round(n_distinct(participant_id[average<=360])/total*100,0))%>%
  rename("30" = interval_30,
         "60" = interval_60,
         "90" = interval_90,
         "180" = interval_180,
         "360" = interval_360)%>%
  select(-total)%>%
  pivot_longer(-c(labels), names_to="key", values_to="value")%>%
  # mutate(key=fct_relevel(key, levels=c("30", "60", "90", "180", "360")))%>%
  mutate(key=as.numeric(key))%>%
  
  ggplot(aes(x=as.numeric(key), y=value, color=labels))+
  geom_line(size=2)+
  geom_point(size=4)+
  geom_text(aes(label=paste0(value, "%")),
            direction="both",
            nudge_y = 6,
            size=8,
            color="black"
  )+
  facet_wrap(~labels,
             labeller = label_wrap_gen(width = 30),
             ncol=4)+
  
  # oxpop_blue_panel+
  theme(legend.position="none",
        panel.grid.minor = element_blank(),
        text=element_text(size=25,
                          family="Mulish"))+
  
  labs(y="Participants (%)",
       # title="Proportions of participants with distinct average time intervals between consecutive entries",
       x="Time between consecutive records (in days)",
       # caption="Time intervals calculated as the median day difference between consecutive entries with the same drug code for the same participant\nDuplicate entries (same drug code, same participant, same date) were excluded to avoid counting duplicate entries as distinct events\nProportions refer to total number of participants with an entry for each drug group",
  )+
  scale_x_continuous(breaks=c(0, 30,60,90, 180, 360), limits=c(0, NA),expand=expansion(c(0,0.1)))+
  scale_y_continuous(breaks=seq(0,100, by=20), expand=expansion(c(0,0.4)))

ggsave("Outputs/Thesis/Figures/time_interval_proportions.png",
       height=38,
       width=67,
       dpi="retina",
       units="cm")

ggsave("Outputs/Thesis/Figures/HR/time_interval_proportions.tiff",
       height=38,
       width=67,
       dpi="retina",
       units="cm")

# rm(timeintervals)  


## 5.9 Number of records per monthly period --------

# per BNF code

  # wrangle
pis_data%>%
  select(participant_id, disp_ym, approved_name, bnf_code, number_items)%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%  
  group_by(participant_id, bnf_code, disp_ym)%>%
  summarise(records=n())%>%
  ungroup()%>%
  group_by(records)%>%
  summarise(n=n())%>%
  
  # plot
  ggplot(aes(x=records, y=n))+
  geom_col(fill="orange")+
  scale_x_continuous(limits=c(NA, 5))+
  oxpop_blue_panel+
  labs(x = "Records per month",
       title="Number of records for each BNF code per monthly dispensing period and participant",
       caption=str_wrap("Counts presented are BNF code-participant-month triplets", 150),
       y="Count")

  # save
ggsave("Outputs/Figures/records_per_month_per_code.png",
       height=38,
       width=67,
       dpi="retina",
       units="cm")
  
# per drug group

  # wrangle

pis_data%>%
  select(participant_id, disp_ym, approved_name, bnf_code, number_items)%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  group_by(participant_id, labels, disp_ym)%>%
  summarise(records=n())%>%
  filter(!is.na(labels))%>%
  # mutate(labels=as.character(labels))%>%
  # mutate(labels=fct_reorder(labels, labels))%>%
  # mutate(labels=as.character(fct_inorder(labels)))%>%
  # mutate(labels=as.character(fct_reorder(labels, desc(labels))))%>%
  
  
  # plot
  ggplot(aes(x=records, fill=labels))+
  stat_density_ridges(aes(y=labels), quantile_lines = TRUE, quantiles = 2, alpha=0.5)+
  oxpop_blue_panel+
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank()        
        legend.position = "none"
  )+
  labs(x = "Average number of records per month",
       title="Number of records for each drug group per monthly period and participant",
       # caption=str_wrap("Horizontal axis capped at 360. Day differences calculated at individual BNF code and participant level as the median day difference between consecutive records. Only one record per dispensing date was retained. Vertical lines depict medians per drug group and dataset.", 150),
       y="Drug group")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(0, 6), breaks=seq(0,6,1))
  
  # save
ggsave("Outputs/Figures/records_per_month_per_group.png",
       height=38,
       width=67,
       dpi="retina",
       units="cm")















# 6. Drug recording frequency (PIS) -------

## 6.1 Absolute counts (barcharts) -----

pis_data%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  filter(!is.na(labels))%>%
  group_by(labels)%>%
  summarise(Records = n(),
            Participants = n_distinct(participant_id))%>%
  pivot_longer(c(Records, Participants), names_to="key", values_to="value")%>%

  
  ggplot(aes(labels, value, fill=labels))+
  geom_bar(stat="identity")+
  facet_wrap(~key, scales="free_y", nrow=2)+
  labs(title="Recording of different drug groups in the PIS data (participant and record counts)",
       x="Drug group",
       y="Count")+
  geom_text(aes(label=value),vjust=-1, size=6, 
            color="white"
            )+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust=1),
        # strip.text.y.left = element_text(),
        strip.text.x = element_text(hjust = 0),
        text=element_text(color="white"))+
  oxpop_blue_panel+
  scale_y_continuous(expand=expansion(c(0,0.3)))
  
ggsave("Outputs/Figures/barcharts_pis.png",
       height=38,
       width=67,
       dpi="retina",
       units = "cm")

## 6.2 Timeseries --------
pis_data%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  filter(!is.na(labels))%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         dispensing_month = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  group_by(labels, dispensing_month)%>%
  summarise(Participants = n_distinct(participant_id))%>%

  ggplot(aes(dispensing_month, Participants, color=labels))+
  geom_point()+
  geom_line()+
  facet_wrap(~labels, scales="free_y")+
  labs(title="Recording along time for different drug groups in the PIS data",
       subtitle="At monthly level",
       x="Dispensing month",
       y="Participants")+
  # geom_text(aes(label=value),vjust=-1, size=6)+
  oxpop_blue_panel+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust=1),
        # strip.text.y.left = element_text(),
        strip.text.x = element_text(hjust = 0))+
  scale_y_continuous(limits=c(0, NA))

ggsave("Outputs/Figures/timeseries_participants_pis.png",
       height=38,
       width=67,
       dpi="retina",
       units = "cm")




# 7. Drug recording in LENS ------
## 7.1 Screening ---------

screening_drugs%>%
  rename(Warfarin = warfarin,
         Colchicine = colchicine,
         Cyclosporine = cyclosporine,
         Daptomycin = daptomycin,
         Ketoprofen = ketoprofen,
         Fibrates = fibrate_therapy,
         `Rosuvastatin (40mg)` = rosuvastatin,
         `Other vitamin K antagonists` = vitamin_k_antagonist)%>%
  pivot_longer(cols=-participant_id, names_to="Drug", values_to="value")%>%
  group_by(Drug)%>%
  summarise(Participants = sum(value))%>%View()

screening_drugs%>%
  rename(Warfarin = warfarin,
         Colchicine = colchicine,
         Cyclosporine = cyclosporine,
         Daptomycin = daptomycin,
         Ketoprofen = ketoprofen,
         Fibrates = fibrate_therapy,
         `Rosuvastatin (40mg)` = rosuvastatin,
         `Other vitamin K antagonists` = vitamin_k_antagonist)%>%
  pivot_longer(cols=-participant_id, names_to="Drug", values_to="value")%>%
  filter(value>0)%>%
  distinct(Drug, participant_id)%>%
  select(participant_id)%>%.[[1]]->participants_on_contraindicated_drugs_screening

rand_dugs%>%
  filter(participant_id %in% participants_on_contraindicated_drugs_screening)%>%
  View() # none went on to be randomised




## 7.2 Randomisation ------


# barchart
rand_drugs%>%
  left_join(drugs)%>%
  filter(cat_name %in% lens_drugs$cat_name)%>%
  left_join(lens_drugs)%>%
  group_by(labels)%>%
  summarise(Participants = n_distinct(participant_id),
            Proportion = round(Participants/lens_participants_number*100, 1))%>%
  
  ggplot(aes(labels, Participants, fill=labels))+
  geom_bar(stat="identity", width=0.8)+
  geom_text(aes(label=paste0(Participants, "\n(", Proportion, "%)")), size=6, vjust=-1, color="white")+
  oxpop_blue_panel+
  theme(axis.text.x = element_text(angle=30, hjust=1),
        legend.position = "none")+
  labs(title="Drug exposure at randomisation (self-reported)",
       x="Drug groups",
       y="Participants")+
  scale_y_continuous(expand = expansion(c(0,0.5)))

ggsave("Outputs/Figures/randomisation_barchart.png",
       height=32,
       width=62,
       dpi="retina",
       units="cm")

## 7.3 Follow-up -----


View(fu_drugs)

# barchart
fu_drugs%>%
  select(-assessment_id, -visit_date)%>%
  rename(Warfarin = warfarin,
         Colchicine = colchicine,
         Cyclosporine = cyclosporine,
         Daptomycin = daptomycin,
         Ketoprofen = ketoprofen,
         Fibrates = fibrate_therapy,
         `Rosuvastatin (40mg)` = rosuvastatin,
         `Other vitamin K antagonists` = vitamin_k_antagonist)%>%
  pivot_longer(cols=-participant_id, names_to="Drug", values_to="value")%>%
  filter(value==1)%>%
  distinct(participant_id, Drug)%>%
  group_by(Drug)%>%
  summarise(Participants = length(participant_id),
            Proportion = round(Participants/lens_participants_number*100, 1))%>%
  
  ggplot(aes(Drug, Participants, fill=Drug))+
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(Participants, "\n(", Proportion, "%)")), size=6, vjust=-0.5, color="white")+
  labs(title="Participants initiating contra-indicated drugs after randomisation (self-reported)")+
  oxpop_blue_panel+ 
  theme(legend.position = "none",
        axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  scale_y_continuous(expand = expansion(c(0,0.5)))


ggsave("Outputs/Figures/fu_barchart.png",
       width=25,
       height=10,
       dpi="retina")

fu_drugs%>%
  select(-assessment_id, -visit_date)%>%
  rename(Warfarin = warfarin,
         Colchicine = colchicine,
         Cyclosporine = cyclosporine,
         Daptomycin = daptomycin,
         Ketoprofen = ketoprofen,
         Fibrates = fibrate_therapy,
         `Rosuvastatin (40mg)` = rosuvastatin,
         `Other vitamin K antagonists` = vitamin_k_antagonist)%>%
  pivot_longer(cols=-participant_id, names_to="Drug", values_to="value")%>%
  filter(value==1)%>%View()
  distinct(participant_id)%>%
  nrow() ## 11 people


# timeseries
fu_drugs%>%
  select(-assessment_id)%>%
  rename(Warfarin = warfarin,
         Colchicine = colchicine,
         Cyclosporine = cyclosporine,
         Daptomycin = daptomycin,
         Ketoprofen = ketoprofen,
         Fibrates = fibrate_therapy,
         `Rosuvastatin (40mg)` = rosuvastatin,
         `Other vitamin K antagonists` = vitamin_k_antagonist)%>%
  left_join(participants%>%select(participant_id, rand_date))%>%
  mutate(days_after_rand = difftime(as.Date(visit_date, format = "%Y-%m-%d"), as.Date(rand_date, format = "%Y-%m-%d"), units= "days"))%>%
  select(-visit_date, - rand_date)%>%
  pivot_longer(cols=-c(participant_id, days_after_rand), names_to="Drug", values_to="value")%>%
  mutate(days_after_rand=as.integer(str_sub(days_after_rand, 1, -1)))%>%
  filter(value>0)%>%
  distinct(participant_id, Drug, days_after_rand)%>%
  filter(participant_id %in% c("2002851", "2010368"))%>% # removing participants felt to have contraindicated drugs recorded in error (after manual review of contact and participant status logs)
  group_by(participant_id, Drug)%>%
  slice_min(order_by=days_after_rand)%>%
  group_by(Drug)%>%
  arrange(days_after_rand)%>%
  mutate(value=1)%>%
  mutate(cumulative = cumsum(value))->contraindicated_drugs_after_rand_self_report
  # group_by(Drug)%>%
  # summarise(Participants = sum(value), Proportion = round(Participants/lens_participants_number*100, 1))%>%
  
contraindicated_drugs_after_rand_self_report%>%
  ggplot(aes(days_after_rand, cumulative, color=Drug))+
  geom_point(size=4)+
  geom_line()+
  geom_text_repel(aes(label=cumulative), size=6, vjust=-1)+
  facet_wrap(~Drug)+
  labs(title="Timeseries of participants initiating contra-indicated drugs after randomisation (self-reported)",
       x="Days after randomisation",
       y="Cumulative sum")+
  oxpop_blue_panel+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(0, NA), expand = expansion(c(0,0.5)))+
  scale_x_continuous(limits=c(0, NA))







ggsave("Outputs/Figures/fu_timeseries.png",
       height=32,
       width=62,
       dpi="retina",
       units="cm")




# export participant ids

# write_csv(contraindicated_drugs_after_rand_self_report%>% ungroup()%>%distinct(participant_id), 
#   "Outputs/contraindicated_drugs_follow_up.csv")
# 
# 
# 
# 





















# 8. Data derivation development ------

## 8.1. Look back period -----

### Flags for self-reported data ------

rand_drugs%>%
  left_join(drugs)%>%
  left_join(lens_drugs)%>%
  select(participant_id, med_drug_code, labels)%>%
  distinct(participant_id, labels)%>%
  mutate(self_report="1")%>%
  right_join(expand.grid(labels=lens_drugs%>%select(labels)%>%.[[1]],
                         participant_id=lens_participants_list$participant_id))%>%  
  left_join(participants%>%select(participant_id, rand_date)%>%mutate(participant_id=as.character(participant_id)))%>%
  mutate(self_report=replace_na(self_report, "0"))->flagging_rand_drugs
  
### Wrangle PIS data for rules---------

pis_data%>%
  select(participant_id, bnf_code, disp_ym)%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  select(-disp_y, -disp_m)%>%
  left_join(participants%>%select(participant_id, rand_date)%>%mutate(participant_id=as.character(participant_id)))%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"))%>%
  mutate(time_before_rand = interval(disp_ym, rand_month)%/%months(1))%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  filter(!is.na(labels))%>%
  distinct(participant_id, labels, disp_ym, rand_date, time_before_rand)->pis_data_with_months_before_rand

### Rule 1 (month before rand) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand=="1")%>% # filter records in month before rand
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>% # rule flag
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>% # bring back the full combinatory list of people and drugs
  mutate(pis=replace_na(pis, "0"))%>% # create a negative flag for those not meeting criteria
  mutate(rule="R1")->rule1 # apply rule flag and store

### Rule 2 (2 months before rand) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand%in% c(1,2))%>% # filter records within 1 or 2 months before randomisation month
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R2")->rule2

### Rule 3 (3 months before rand) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand%in% c(1,2, 3))%>% # filter records within 1, 2 or 3 months before randomisation month
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R3")->rule3

### Rule 4 (any record before randomisation) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand>0)%>% # filter records happening any time before randomisation month
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R4")->rule4

### Rule 5 (month of randomisation or month before) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand %in% c(0,1))%>% # records in randomisation month or month before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R5")->rule5

### Rule 6 (month of randomisation or 2 months before) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand %in% c(0,1, 2))%>% # records in randomisation month or 2 months before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R6")->rule6

### Rule 7 (month of randomisation or 3 months before) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand %in% c(0,1, 2, 3))%>% # records in randomisation month or 3 months before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R7")->rule7

### Rule 8 (month of randomisation or any time in run-in) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand >(-1))%>% # records in randomisation month or any time before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R8")->rule8

### Rule 9 (one month before up to one month after) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand %in% c(-1,0,1))%>% # records in randomisation month or month before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R9")->rule9

### Rule 10 (two months before up to one month after) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand %in% c(-1,0,1, 2))%>% # records in randomisation month or month before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R10")->rule10

### Rule 11 (three months before up to one month after) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand %in% c(-1,0,1,2,3))%>% # records in randomisation month or month before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R11")->rule11

### Rule 12 (anytime before up to one month after) ---------

pis_data_with_months_before_rand%>%
  filter(time_before_rand >(-2))%>% # records in randomisation month or any time before
  select(-time_before_rand)%>%
  distinct(participant_id, labels)%>%
  mutate(pis="1")%>%
  full_join(flagging_rand_drugs%>%select(-rand_date), by=c("participant_id", "labels"))%>%
  mutate(pis=replace_na(pis, "0"))%>%
  mutate(rule="R12")->rule12




### Aggregate -----------

pis_data_rules_flagged <- rbind(rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10, rule11, rule12)

View(pis_data_rules_flagged)

rm(rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10, rule11, rule12)

### Calculate counts and agreement (aggregate)-------

# counts
pis_data_rules_flagged%>%
  group_by(rule)%>%
  summarise(pairs = n_distinct(participant_id, labels),
                      positives = length(self_report[self_report=="1"]),
                      negatives = length(self_report[self_report=="0"]),
                      true_positives = length(self_report[self_report=="1" & pis=="1"]),
                      true_negatives = length(self_report[self_report=="0" & pis=="0"]),
                      false_positives = length(self_report[self_report=="0" & pis =="1"]),
                      false_negatives = length(self_report[self_report =="1" & pis == "0"]),
                      Sensitivity = round(true_positives/positives*100, 1),
                      Specificity = round(true_negatives/negatives*100, 1),
                      PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                      NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                      Both = sum(pis=="1" & self_report=="1"),
                      Self_report_only = sum(pis=="0" & self_report=="1"),
                      PIS_only = sum(pis=="1" & self_report=="0"),
                      Neither = sum(pis=="0" & self_report=="0"))%>%
  select(-c(pairs, positives, negatives, true_positives, true_negatives, false_positives, false_negatives))->rules_counts_aggregated

View(rules_counts_aggregated)

### Calculate agreement ------

kappa_rules_aggregate<-list()

for(i in seq(1,12,1)){
  pis_data_rules_flagged%>%
  filter(rule==paste0("R", i))->table
  
  try(k<-Kappa.test(table$pis, table$self_report))

  if(exists("k")){
    kappa_rules_aggregate[[i]]<-k
  }
  else{
    kappa_rules_aggregate[[i]]<-list()
  }

rm(i, table, k)

}

# transform results into table
kappa_rules_aggregate_results<-data.frame()

for(i in seq(1,12,1)) {
  
  name<-i
  
  try(
    table<-(data.frame(
      rule = paste0("R", name),
      kappa = unlist(kappa_rules_aggregate[[i]]$Result$estimate),
      CI_lower = unlist(kappa_rules_aggregate[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_rules_aggregate[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_rules_aggregate[[i]]$Result$p.value),
      judgement = unlist(kappa_rules_aggregate[[i]]$Judgement)
    )))
  
  try(kappa_rules_aggregate_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}

### Format table ------
kappa_rules_aggregate_results%>%
  mutate(CI = paste0(
                round(CI_lower,2),
                " : ",
                round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate(p_value=as.character(if_else(p_value==0, "<0.001", p_value)))%>%
  select(rule, Kappa=kappa, CI, p_value, judgement)->kappa_table

rules_counts_aggregated%>%
  left_join(kappa_table)%>%
  mutate(Rule = case_when(rule=="R1" ~ "1-A",
                          rule=="R2" ~ "2-A",
                          rule=="R3" ~ "3-A",
                          rule=="R4" ~ "4-A",
                          rule=="R5" ~ "1-B",
                          rule=="R6" ~ "2-B",
                          rule=="R7" ~ "3-B",
                          rule=="R8" ~ "4-B",
                          rule=="R9" ~ "1-C",
                          rule=="R10" ~ "2-C",
                          rule=="R11" ~ "3-C",
                          rule=="R12" ~ "4-C",
  ))%>%
  relocate(Rule, .before=Both)%>%
  mutate(`Kappa (95% CI)`= paste0(Kappa, " (", CI, ")"))%>%
  relocate(`Kappa (95% CI)`, .before=judgement)%>%
  select(-rule, - CI, -p_value, -Kappa)%>%
  rename(`Both sources*` = Both,
         `Self-report only*` = Self_report_only,
         `PIS data only*` = PIS_only,
         `Neither source*` = Neither,
         # `p value` = p_value,
         Judgement = judgement)%>%tibble()%>%
  relocate(c(Sensitivity, Specificity, PPV, NPV), .after=`Neither source*`)%>%
  mutate(Rule=factor(Rule, levels = c("1-A",
                                      "2-A",
                                      "3-A",
                                      "4-A",
                                      "1-B",
                                      "2-B",
                                      "3-B",
                                      "4-B",
                                      "1-C",
                                      "2-C",
                                      "3-C",
                                      "4-C")))%>%
  arrange(Rule)->rules_table



#### TABLE (SLIDES) ------

### Export table -----



rules_table%>%
  mutate(`Lookback period` = case_when(str_detect(Rule, "1") ~ "1 month",
                                       str_detect(Rule, "2") ~ "2 months",
                                       str_detect(Rule, "3") ~ "3 months",
                                       str_detect(Rule, "4") ~ "Anytime"),
         `Handling data in month of randomisation and month after` = case_when(str_detect(Rule, "A") ~ "Exclude month of randomisation",
                                                        str_detect(Rule, "B") ~ "Include month of randomisation",
                                                        str_detect(Rule, "C") ~ "Include month of randomisation and month after randomisation"))%>%
  relocate(`Lookback period`, .after=Rule)%>%
  relocate(`Handling data in month of randomisation and month after`, .after=`Lookback period`)%>%
  custom_tab_data_frame(header = NULL,
             footer = "*Counts presented are participant-drug pairs. Self-report used as reference for sensitivity, specificity, PPV, and NPV calculations. CI - confidence interval; PPV - positive predictive value; NPV - negative predictive value")%>%
  width(j=c(1:11), 2.2, unit = "cm")%>%
  width(j=3, 6, unit = "cm")%>%
  width(j=c(12), 4, unit = "cm")%>%
  width(j=c(13), 5, unit = "cm")%>%
  align(i=NULL, j=NULL, "center", "all")%>%
  # align(i=NULL, j=10, "left", "body")%>%
  align(i=1, j=1, "left", "header")%>%
  align(i=1, j=1, "left", "footer")%>%
  add_header_row(values=c("Rules", "Participant-drug pair counts", "Agreement parameters"), colwidths = c(3, 4, 6))%>%
  add_header_row(values="Performance of different rules to derive baseline drug exposure from PIS data when compared with self-reported exposure", colwidths = 13)%>%
  border(i=2, j=c(1,4), border.right = fp_border(width=2), part="header")%>%
  border(i=3, j=c(3,7), border.right = fp_border(width=2), part="header")%>%
  border(j=c(3,7), border.right = fp_border(width=2), part="body")%>%
  border(i=c(4, 8), border.bottom = fp_border(width=2), part="body")%>%
  save_as_docx(path="Outputs/Tables/baseline_derivation_rules_table.docx", pr_section = sect_properties)




#### TABLE (THESIS) ------


sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

rules_table%>%
  mutate(`Lookback period` = case_when(str_detect(Rule, "1") ~ "1 month",
                                       str_detect(Rule, "2") ~ "2 months",
                                       str_detect(Rule, "3") ~ "3 months",
                                       str_detect(Rule, "4") ~ "Anytime"),
         `Handling data in month of randomisation and month after` = case_when(str_detect(Rule, "A") ~ "Exclude month of randomisation",
                                                                               str_detect(Rule, "B") ~ "Include month of randomisation",
                                                                               str_detect(Rule, "C") ~ "Include month of randomisation and month after randomisation"))%>%
  relocate(`Lookback period`, .after=Rule)%>%
  relocate(`Handling data in month of randomisation and month after`, .after=`Lookback period`)%>%
  flextable()%>%
  add_footer_lines(values =c("*Counts presented are participant-drug pairs. Self-report used as reference for sensitivity, specificity, PPV, and NPV calculations. CI - confidence interval; PPV - positive predictive value; NPV - negative predictive value"))%>%
  width(j=c(1:11), 2.2, unit = "cm")%>%
  width(j=3, 6, unit = "cm")%>%
  width(j=c(12), 4, unit = "cm")%>%
  width(j=c(13), 5, unit = "cm")%>%
  align(i=NULL, j=NULL, "center", "all")%>%
  # align(i=NULL, j=10, "left", "body")%>%
  align(i=1, j=1, "left", "header")%>%
  align(i=1, j=1, "left", "footer")%>%
  add_header_row(values=c("Rules", "Participant-drug pair counts", "Agreement parameters"), colwidths = c(3, 4, 6))%>%
  add_header_row(values="Performance of different rules to derive baseline drug exposure from PIS data when compared with self-reported exposure", colwidths = 13)%>%
  border(i=2, j=c(1,4), border.right = fp_border(width=2), part="header")%>%
  border(i=3, j=c(3,7), border.right = fp_border(width=2), part="header")%>%
  border(j=c(3,7), border.right = fp_border(width=2), part="body")%>%
  border(i=c(4, 8), border.bottom = fp_border(width=2), part="body")%>%
  save_as_docx(path="Outputs/Thesis/Tables/baseline_derivation_rules_table.docx", pr_section = sect_properties)


### Proportions of baseline versus initiation under different rules -----------

pis_data_with_months_before_rand%>%
  mutate(`1A` = if_else(time_before_rand =="1", "Baseline", "Initiation"),
         `2A` = if_else(time_before_rand %in%c(1,2), "Baseline", "Initiation"),
         `3A` = if_else(time_before_rand %in%c(1,2, 3), "Baseline", "Initiation"),
         `4A` = if_else(time_before_rand >0, "Baseline", "Initiation"),
         `1B` = if_else(time_before_rand %in%c(0,1), "Baseline", "Initiation"),
         `2B` = if_else(time_before_rand %in%c(0,1, 2), "Baseline", "Initiation"),
         `3B` = if_else(time_before_rand %in%c(0,1,2, 3), "Baseline", "Initiation"),
         `4B` = if_else(time_before_rand >(-1), "Baseline", "Initiation"),
         `1C` = if_else(time_before_rand %in%c(-1,0,1), "Baseline", "Initiation"),
         `2C` = if_else(time_before_rand %in%c(-1,0,1, 2), "Baseline", "Initiation"),
         `3C` = if_else(time_before_rand %in%c(-1,0,1,2, 3), "Baseline", "Initiation"),
         `4C` = if_else(time_before_rand >(-2), "Baseline", "Initiation"))%>%
  group_by(labels)%>%
  distinct(participant_id, disp_ym, .keep_all = T)%>% # keep one record per participant, month, and drug group
  pivot_longer(cols=6:17, names_to = "Rule", values_to = "Event")%>%
  distinct(participant_id, labels, Rule, Event)%>% # keep one record per participant, drug, Rule, and Event (i.e. people cannot be in baseline and initiation at the same time - which could be the case if they had repeat records)
  group_by(participant_id, labels, Rule)%>%
  arrange(participant_id, labels, Rule, Event)%>%
  group_by(participant_id, labels, Rule)%>%
  slice_head(n=1)%>%
  # mutate(flag="1")%>%
  group_by(labels, Rule, Event)%>%
  summarise(Participants = n_distinct(participant_id),
            Proportion = round(Participants/lens_participants_number*100, 2))->rules_counts


#### FIGURE (SLIDES) ------

rules_counts%>%  

  rbind(rand_drugs%>%
          left_join(drugs)%>%
          filter(cat_name %in% lens_drugs$cat_name)%>%
          left_join(lens_drugs)%>%
          group_by(labels)%>%
          summarise(Participants = n_distinct(participant_id),
                    Proportion = round(Participants/lens_participants_number*100, 1))%>%
          filter(labels%in%drug_labels_order)%>%
          mutate(Rule="CRF",
                 Event="Baseline"))%>%
  
  mutate(Rule=ordered(Rule, levels=c("CRF", "1A", "2A", "3A", "4A", "1B", "2B", "3B", "4B", "1C", "2C", "3C", "4C")))%>%
  # mutate(Rule=reorder(Rule, as.factor(Rule)))%>%
  # mutate(Rule=fct_reorder(Rule, Rule))%>%
  # arrange(Rule)%>%
  # View()
ggplot()+
  geom_col(aes(x=Rule, y=Proportion, fill=factor(Event, levels=c("Initiation", "Baseline"))), 
           position="stack", 
           alpha=1,
           # width=0.8
           )+
    facet_wrap(~labels, 
             scales = "free_y", 
             strip.position = "top",
             labeller = label_wrap_gen(width = 25))+
  # scale_fill_manual(values=c("#F8766D", "#00BFC4"), labels=c("Dispensing only", "Dispensing plus GP"))+
  labs(title=str_wrap("Impact of different derivation rules on drug exposure adjudication as baseline versus post-randomisation initiation",120),
       y="Proportion of participants (%)",
       x="Derivation rule",
       fill="Event",
       # caption="The dotted line represents the proportion identified at randomisation via self-report\nBaseline exposure rule logic:
       # 1-4 (lookback period): 1- 1 month; 2- 2 months; 3- 3 months; 4- anytime
       # A-C (handling of data in month of randomisation and month after): A- exclude; B- include month of randomisation; C- include month of randomisation and month after"
       )+
  oxpop_blue_panel+
  theme(strip.text.x = element_text(size=15),
        legend.position="bottom")+
  scale_y_continuous(expand=expansion(c(0, 0.2)))+
  geom_hline(data=rand_drugs%>%
               left_join(drugs)%>%
               filter(cat_name %in% lens_drugs$cat_name)%>%
               left_join(lens_drugs)%>%
               group_by(labels)%>%
               summarise(Participants = n_distinct(participant_id),
                         Proportion = round(Participants/lens_participants_number*100, 1))%>%
               filter(labels%in%drug_labels_order)%>%
               mutate(Rule="CRF",
                      Event="Baseline"),
             aes(yintercept=Proportion),
             linetype="dashed",
             color="black"
             # size=2
             )+
  geom_text(aes(x=Rule, y=Proportion, label=paste0(round(Proportion, 1))), 
            position=position_stack(vjust=0.5), 
            color="black",
            size=5)

ggsave("Outputs/Figures/baseline_rules_impact.png",
       height=32,
       width= 65,
       dpi="retina",
       units = "cm")




#### FIGURE (THESIS) ------

rules_counts%>%  
  
  rbind(rand_drugs%>%
          left_join(drugs)%>%
          filter(cat_name %in% lens_drugs$cat_name)%>%
          left_join(lens_drugs)%>%
          group_by(labels)%>%
          summarise(Participants = n_distinct(participant_id),
                    Proportion = round(Participants/lens_participants_number*100, 1))%>%
          filter(labels%in%drug_labels_order)%>%
          mutate(Rule="CRF",
                 Event="Baseline"))%>%
  
  mutate(Rule=ordered(Rule, levels=c("CRF", "1A", "2A", "3A", "4A", "1B", "2B", "3B", "4B", "1C", "2C", "3C", "4C")))%>%
  # mutate(Rule=reorder(Rule, as.factor(Rule)))%>%
  # mutate(Rule=fct_reorder(Rule, Rule))%>%
  # arrange(Rule)%>%
  # View()
  ggplot()+
  geom_col(aes(x=Rule, y=Proportion, fill=factor(Event, levels=c("Initiation", "Baseline"))), 
           position="stack", 
           color="black",
           alpha=1,
           # width=0.8
  )+
  facet_wrap(~labels, 
             ncol=3,
             scales = "free", 
             strip.position = "top",
             labeller = label_wrap_gen(width = 25))+
  # scale_fill_manual(values=c("#F8766D", "#00BFC4"), labels=c("Dispensing only", "Dispensing plus GP"))+
  labs(
    # title=str_wrap("Impact of different derivation rules on drug exposure adjudication as baseline versus post-randomisation initiation",120),
       y="Proportion of participants (%)",
       x="Derivation rule",
       fill="Event",
       # caption="The dotted line represents the proportion identified at randomisation via self-report\nBaseline exposure rule logic:
       # 1-4 (lookback period): 1- 1 month; 2- 2 months; 3- 3 months; 4- anytime
       # A-C (handling of data in month of randomisation and month after): A- exclude; B- include month of randomisation; C- include month of randomisation and month after"
  )+
  # oxpop_blue_panel+
  theme(
    strip.text.x = element_text(size=30),
        legend.position="bottom",
        axis.text.x=element_text(size=20)
        )+
  scale_y_continuous(expand=expansion(c(0, 0.2)))+
  geom_hline(data=rand_drugs%>%
               left_join(drugs)%>%
               filter(cat_name %in% lens_drugs$cat_name)%>%
               left_join(lens_drugs)%>%
               group_by(labels)%>%
               summarise(Participants = n_distinct(participant_id),
                         Proportion = round(Participants/lens_participants_number*100, 1))%>%
               filter(labels%in%drug_labels_order)%>%
               mutate(Rule="CRF",
                      Event="Baseline"),
             aes(yintercept=Proportion),
             linetype="dashed",
             color="black"
             # size=2
  )+
  scale_fill_discrete(breaks=c("Baseline","Initiation"))+
  geom_text(aes(x=Rule, y=Proportion, label=paste0(round(Proportion, 1))), 
            position=position_stack(vjust=0.5), 
            color="black",
            size=7)

ggsave("Outputs/Thesis/Figures/baseline_rules_impact.png",
       height=80,
       width=70,
       dpi="retina",
       units = "cm")


ggsave("Outputs/Thesis/Figures/HR/baseline_rules_impact.tiff",
       height=50,
       width= 65,
       dpi="retina",
       units = "cm")


 # rm(flagging_rand_drugs, kappa_rules_aggregate, kappa_rules_aggregate_results, pis_data_rules_flagged, pis_data_with_months_before_rand, kappa_table, derivation_rules_flextable, rules_counts, rules_counts_aggregated, rules_table)



### investigate for insulin only ----

 
 # counts
 pis_data_rules_flagged%>%
   filter(labels=="Insulin")%>%
   group_by(rule)%>%
   summarise(pairs = n_distinct(participant_id, labels),
             positives = length(self_report[self_report=="1"]),
             negatives = length(self_report[self_report=="0"]),
             true_positives = length(self_report[self_report=="1" & pis=="1"]),
             true_negatives = length(self_report[self_report=="0" & pis=="0"]),
             false_positives = length(self_report[self_report=="0" & pis =="1"]),
             false_negatives = length(self_report[self_report =="1" & pis == "0"]),
             Sensitivity = round(true_positives/positives*100, 1),
             Specificity = round(true_negatives/negatives*100, 1),
             PPV = round(true_positives/(true_positives+false_positives)*100, 1),
             NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
             Both = sum(pis=="1" & self_report=="1"),
             Self_report_only = sum(pis=="0" & self_report=="1"),
             PIS_only = sum(pis=="1" & self_report=="0"),
             Neither = sum(pis=="0" & self_report=="0"))%>%
   select(-c(pairs, positives, negatives, true_positives, true_negatives, false_positives, false_negatives))->rules_counts_aggregated_insulin
 

 kappa_rules_aggregate_insulin<-list()
 
 for(i in seq(1,12,1)){
   pis_data_rules_flagged%>%
     filter(rule==paste0("R", i),
            labels=="Insulin")->table
   
   try(k<-Kappa.test(table$pis, table$self_report))
   
   if(exists("k")){
     kappa_rules_aggregate_insulin[[i]]<-k
   }
   else{
     kappa_rules_aggregate_insulin[[i]]<-list()
   }
   
   rm(i, table, k)
   
 }
 
 # transform results into table
 kappa_rules_aggregate_insulin_results<-data.frame()
 
 for(i in seq(1,12,1)) {
   
   name<-i
   
   try(
     table<-(data.frame(
       rule = paste0("R", name),
       kappa = unlist(kappa_rules_aggregate_insulin[[i]]$Result$estimate),
       CI_lower = unlist(kappa_rules_aggregate_insulin[[i]]$Result$conf.int[1]),
       CI_upper = unlist(kappa_rules_aggregate_insulin[[i]]$Result$conf.int[2]),
       p_value = unlist(kappa_rules_aggregate_insulin[[i]]$Result$p.value),
       judgement = unlist(kappa_rules_aggregate_insulin[[i]]$Judgement)
     )))
   
   try(kappa_rules_aggregate_insulin_results%<>%bind_rows(table))
   
   
   rm(table, name, i)
   
 }
 
 kappa_rules_aggregate_insulin_results%>%
   mutate(CI = paste0(
     round(CI_lower,2),
     " : ",
     round(CI_upper, 2)))%>%
   
   mutate(kappa=round(kappa,2),
          p_value=as.character(round(p_value, 2)))%>%
   
   mutate(p_value=as.character(if_else(p_value==0, "<0.001", p_value)))%>%
   select(rule, Kappa=kappa, CI, p_value, judgement)->kappa_table_insulin
 
 rules_counts_aggregated_insulin%>%
   left_join(kappa_table_insulin)%>%
   mutate(Rule = case_when(rule=="R1" ~ "1-A",
                           rule=="R2" ~ "2-A",
                           rule=="R3" ~ "3-A",
                           rule=="R4" ~ "4-A",
                           rule=="R5" ~ "1-B",
                           rule=="R6" ~ "2-B",
                           rule=="R7" ~ "3-B",
                           rule=="R8" ~ "4-B",
                           rule=="R9" ~ "1-C",
                           rule=="R10" ~ "2-C",
                           rule=="R11" ~ "3-C",
                           rule=="R12" ~ "4-C",
   ))%>%
   relocate(Rule, .before=Both)%>%
   mutate(`Kappa (95% CI)`= paste0(Kappa, " (", CI, ")"))%>%
   relocate(`Kappa (95% CI)`, .before=judgement)%>%
   select(-rule, - CI, -p_value, -Kappa)%>%
   rename(`Both sources*` = Both,
          `Self-report only*` = Self_report_only,
          `PIS data only*` = PIS_only,
          `Neither source*` = Neither,
          # `p value` = p_value,
          Judgement = judgement)%>%tibble()%>%
   relocate(c(Sensitivity, Specificity, PPV, NPV), .after=`Neither source*`)%>%
   mutate(Rule=factor(Rule, levels = c("1-A",
                                       "2-A",
                                       "3-A",
                                       "4-A",
                                       "1-B",
                                       "2-B",
                                       "3-B",
                                       "4-B",
                                       "1-C",
                                       "2-C",
                                       "3-C",
                                       "4-C")))%>%
   arrange(Rule)->rules_table_insulin
 
 View(rules_table_insulin)
 
 
 rules_table_insulin%>%
   mutate(`Lookback period` = case_when(str_detect(Rule, "1") ~ "1 month",
                                        str_detect(Rule, "2") ~ "2 months",
                                        str_detect(Rule, "3") ~ "3 months",
                                        str_detect(Rule, "4") ~ "Anytime"),
          `Handling data in month of randomisation and month after` = case_when(str_detect(Rule, "A") ~ "Exclude month of randomisation",
                                                                                str_detect(Rule, "B") ~ "Include month of randomisation",
                                                                                str_detect(Rule, "C") ~ "Include month of randomisation and month after randomisation"))%>%
   relocate(`Lookback period`, .after=Rule)%>%
   relocate(`Handling data in month of randomisation and month after`, .after=`Lookback period`)%>%
   custom_tab_data_frame(header = NULL,
                         footer = "*Counts presented are participant-drug pairs. Self-report used as reference for sensitivity, specificity, PPV, and NPV calculations. CI - confidence interval; PPV - positive predictive value; NPV - negative predictive value")%>%
   width(j=c(1:11), 2.2, unit = "cm")%>%
   width(j=3, 6, unit = "cm")%>%
   width(j=c(12), 4, unit = "cm")%>%
   width(j=c(13), 5, unit = "cm")%>%
   align(i=NULL, j=NULL, "center", "all")%>%
   # align(i=NULL, j=10, "left", "body")%>%
   align(i=1, j=1, "left", "header")%>%
   align(i=1, j=1, "left", "footer")%>%
   add_header_row(values=c("Rules", "Participant-drug pair counts", "Agreement parameters"), colwidths = c(3, 4, 6))%>%
   add_header_row(values="Performance of different rules to derive baseline drug exposure from PIS data when compared with self-reported exposure (insulin only)", colwidths = 13)%>%
   border(i=2, j=c(1,4), border.right = fp_border(width=2), part="header")%>%
   border(i=3, j=c(3,7), border.right = fp_border(width=2), part="header")%>%
   border(j=c(3,7), border.right = fp_border(width=2), part="body")%>%
   border(i=c(4, 8), border.bottom = fp_border(width=2), part="body")
 
 

## 8.2 Date handling -------


### 8.2.1 Baseline status -------

# self-reported data
rand_drugs%>%
  left_join(drugs)%>%
  left_join(lens_drugs)%>%
  select(participant_id, med_drug_code, labels)%>%
  distinct(participant_id, labels)%>%
  mutate(self_report="1")%>%
  right_join(expand.grid(labels=lens_drugs%>%select(labels)%>%.[[1]],
                         participant_id=lens_participants_list$participant_id))%>%    
  left_join(participants%>%select(participant_id, rand_date))%>%
  mutate(self_report=replace_na(self_report, "0"))->flagging_rand_drugs

# pis_data
pis_data%>%
  select(participant_id, bnf_code, disp_ym)%>%
  mutate(disp_y = substr(disp_ym,1,4), # apply rules
         disp_m = substr(disp_ym,5,6),
         date_first = as.Date(paste0(disp_y, "-", disp_m, "-01")),
         date_15 = as.Date(paste0(disp_y, "-", disp_m, "-15")),
         date_last =  ceiling_date(date_first, "month") - 1)%>%
  select(-disp_y, -disp_m)%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  select(-codelist, -bnf_code)%>%
  left_join(participants%>%select(participant_id, rand_date))%>%
  pivot_longer(c(date_first, date_15, date_last), names_to = "Rule", values_to = "Date")%>%
  mutate(time_before_rand = difftime(rand_date, Date, units = "days"), # calculate time before rand
         time_before_rand = as.numeric(str_sub(time_before_rand, 1, -1)))%>%
  filter(time_before_rand%in%seq(0,90,1))%>% # filter for differences within 90 days
  group_by(participant_id, labels, Rule)%>%
  slice(which.min(time_before_rand))%>% # retain one record per rule (the one closest to randomisation)
  ungroup()%>%
  select(-rand_date, -Date, -disp_ym)%>%
  pivot_wider(names_from = Rule, values_from = time_before_rand)%>%
  mutate(across(starts_with("date"), ~if_else(!is.na(.), "1", "0")))%>% # apply a flag of 1 or 0 for presence of absence of record in each rule
  filter(!is.na(labels))->pis_data_date_rules_flagged

flagging_rand_drugs%>%
  # filter(self_report>0)%>%
  left_join(pis_data_date_rules_flagged, by=c("participant_id", "labels"))%>%
  select(-rand_date)%>%
  mutate(across(-c(participant_id, labels), ~replace_na(., "0")))%>% # apply flag 0 for absent records
  pivot_longer(starts_with("date"), names_to = "Rule", values_to = "flag")->date_rules_with_rand_data

    
# Kappa loops


#### Calculate agreement ------

kappa_rules_aggregate<-list()

for(i in c("date_first", "date_15", "date_last")){
  
  date_rules_with_rand_data%>%
    filter(Rule==paste0(i))->table
  
  try(k<-Kappa.test(table$self_report, table$flag))
  
  if(exists("k")){
    kappa_rules_aggregate[[i]]<-k
  }
  else{
    kappa_rules_aggregate[[i]]<-list()
  }
  
  rm(i, table, k)
  
}

# transform results into table
kappa_rules_aggregate_results<-data.frame()

for(i in c("date_first", "date_15", "date_last")){
  
  name<-i
  
  try(
    table<-(data.frame(
      Rule = paste0(name),
      kappa = unlist(kappa_rules_aggregate[[i]]$Result$estimate),
      CI_lower = unlist(kappa_rules_aggregate[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_rules_aggregate[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_rules_aggregate[[i]]$Result$p.value),
      judgement = unlist(kappa_rules_aggregate[[i]]$Judgement)
    )))
  
  try(kappa_rules_aggregate_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}



kappa_rules_aggregate_results

date_rules_with_rand_data%>%
  group_by(Rule)%>%
  summarise(pairs = n_distinct(participant_id, labels),
            positives = length(self_report[self_report=="1"]),
            negatives = length(self_report[self_report=="0"]),
            true_positives = length(self_report[self_report=="1" & flag=="1"]),
            true_negatives = length(self_report[self_report=="0" & flag=="0"]),
            false_positives = length(self_report[self_report=="0" & flag =="1"]),
            false_negatives = length(self_report[self_report =="1" & flag == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(flag=="1" & self_report=="1"),
            Self_report_only = sum(flag=="0" & self_report=="1"),
            PIS_only = sum(flag=="1" & self_report=="0"),
            Neither = sum(flag=="0" & self_report=="0"),)%>%
  select(-c(pairs, positives, negatives, true_positives, true_negatives, false_positives, false_negatives))->two_by_two


#### Format table ------
kappa_rules_aggregate_results%>%
  left_join(two_by_two)%>%
  mutate(CI = paste0(
    round(CI_lower,2),
    " : ",
    round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate(p_value=as.character(if_else(p_value==0, "<0.001", p_value)))%>%
  select(Rule, Both, Self_report_only, PIS_only, Neither, Sensitivity, Specificity, PPV, NPV, Kappa=kappa, CI, p_value, judgement)->kappa_table

kappa_table%>%
  mutate(Rule = case_when(Rule=="date_first" ~ "Use first day of month",
                          Rule=="date_15" ~ "Use day 15",
                          Rule=="date_last" ~ "Use last day of month"
                          ))%>%
  relocate(Rule, .before=Both)%>%
  mutate(`Kappa (95% CI)`= paste0(Kappa, " (", CI, ")"))%>%
  relocate(`Kappa (95% CI)`, .before=judgement)%>%
  select(- CI, -p_value, -Kappa)%>%
  rename(`Both sources*` = Both,
         `Self-report only*` = Self_report_only,
         `PIS data only*` = PIS_only,
         `Neither source*` = Neither,
         # `p value` = p_value,
         Judgement = judgement)%>%tibble()->rules_table




#### Export table -----
sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

##### TABLE (SLIDES) ------

rules_table%>%
  custom_tab_data_frame(header = "Performance of different rules for handling dates from PIS data, compared with baseline self-reported drug exposure",
                        footer = "*Counts presented are participant-drug pairs. Self-report used as reference for sensitivity, specificity, PPV, and NPV calculations. CI - confidence interval; PPV - positive predictive value; NPV - negative predictive value")%>%
  width(j=1, 3, unit = "cm")%>%
  width(j=c(2:7), 2.1, unit = "cm")%>%
  width(j=c(1, 10,11), 3.5, unit = "cm")%>%
  align(i=NULL, j=NULL, "center", "all")%>%
  # align(i=NULL, j=8, "left", "body")%>%
  align(i=1, j=1, "left", "header")%>%
  align(i=1, j=1, "left", "footer")%>%
  save_as_docx(path="Outputs/Tables/date_handling_rules_table.docx", pr_section = sect_properties)


##### TABLE (THESIS) ------


rules_table%>%
  flextable()%>%
  add_footer_lines("*Counts presented are participant-drug pairs. Self-report used as reference for sensitivity, specificity, PPV, and NPV calculations. CI - confidence interval; PPV - positive predictive value; NPV - negative predictive value")%>%
  width(j=1, 3, unit = "cm")%>%
  width(j=c(2:7), 2.1, unit = "cm")%>%
  width(j=c(1, 10,11), 3.5, unit = "cm")%>%
  align(i=NULL, j=NULL, "center", "all")%>%
  # align(i=NULL, j=8, "left", "body")%>%
  align(i=1, j=1, "left", "header")%>%
  align(i=1, j=1, "left", "footer")%>%
  border(j=c(1,5,9), border.right = fp_border(width=2), part="body")%>%
  border(i=c(1, 2), border.bottom = fp_border(width=2), part="body")%>%
  save_as_docx(path="Outputs/Thesis/Tables/date_handling_rules_table.docx", pr_section = sect_properties)








rm(kappa_rules_aggregate, kappa_rules_aggregate_results, kappa_table, pis_data_date_rules_flagged, pis_data_rules_flagged, rules_table, two_by_two, flagging_rand_drugs, date_rules_with_rand_data)




### 8.2.2 (ABANDONED) Drug initiation events ---------

# initiation events in LENS data
fu_drugs%>%
  select(-assessment_id)%>%
  rename(Warfarin = warfarin,
         Colchicine = colchicine,
         Cyclosporine = cyclosporine,
         Daptomycin = daptomycin,
         Ketoprofen = ketoprofen,
         Fibrates = fibrate_therapy,
         `Rosuvastatin (40mg)` = rosuvastatin,
         `Other vitamin K antagonists` = vitamin_k_antagonist)%>%
  left_join(participants%>%select(participant_id, rand_date))%>%
  mutate(days_after_rand = difftime(as.Date(visit_date, format = "%Y-%m-%d"), as.Date(rand_date, format = "%Y-%m-%d"), units= "days"))%>%
  select(-visit_date, - rand_date)%>%
  pivot_longer(cols=-c(participant_id, days_after_rand), names_to="Drug", values_to="value")%>%
  mutate(days_after_rand=as.integer(str_sub(days_after_rand, 1, -1)))%>%
  filter(value>0)%>%
  select(-value)%>%
  group_by(Drug, participant_id)%>%
  arrange(days_after_rand)%>%
  slice_head(n=1)%>%View()
  
# initiation events in PIS data

pis_data%>%
  left_join(codelists_bnf)%>%
  #filter(codelist %in% contraindications_pis$codelist)%>%
  left_join(contraindications_pis)%>%
  filter(!is.na(labels))%>%
  filter(!codelist%in%c("rosuvastatin", "lens_contraindicated"))%>%
  left_join(participants)%>%
  select(participant_id, rand_date, disp_ym, bnf_code, labels)%>%
  group_by(participant_id, labels)%>%
  arrange(disp_ym)%>%
  slice_head(n=1)%>%View() # only 4 drugs, for 3 participants







# 9. Drug dosing --------

# lens data

## dose distribution

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

rand_drugs%>%
  filter(!is.na(drug_dosage))%>%
  left_join(drugs%>%
              filter(str_detect(cat_name,"statin")))%>%
  distinct(participant_id, drug_term, drug_dosage)%>%
  rename(lens_term= drug_term, lens_dose = drug_dosage)%>%
  mutate(lens_term = if_else(lens_term=="lipitor", "atorvastatin", lens_term),
         lens_term = if_else(lens_term=="simvador", "simvastatin", lens_term),
         lens_term = if_else(lens_term=="simvastatin 40mg / ezetimibe 10mg tablet", "simvastatin", lens_term))%>%
  group_by(lens_term)%>%
  count(lens_dose)%>%
  mutate(Statin = str_to_title(lens_term))%>%
  rename(Dose=lens_dose)%>%
  ggplot(aes(Dose, n))+
  geom_col(width=3)+
  facet_wrap(~Statin, 
             scales="free")+
  labs(y="Participants",
       x="Dose (mg)")+
  scale_x_continuous(limits=c(0,NA), breaks=seq(0,80,10))+
  scale_y_continuous(breaks = integer_breaks())

ggsave("Outputs/Thesis/Figures/statin_dose_distributions.png",
       height=30,
       width= 60,
       dpi="retina",
       units = "cm")

rand_drugs%>%
  filter(!is.na(drug_dosage))%>%
  left_join(participants%>%select(participant_id, rand_date))%>%
  left_join(drugs)%>%
  select(participant_id, rand_date, drug_term, drug_dosage)%>%
  distinct(participant_id, drug_term, .keep_all = T)%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"))%>%
  rename(lens_term= drug_term, lens_dose = drug_dosage)%>%
  select(-rand_date, -rand_month)%>%
  mutate(lens_term = if_else(lens_term=="lipitor", "atorvastatin", lens_term),
         lens_term = if_else(lens_term=="simvador", "simvastatin", lens_term),
         lens_term = if_else(lens_term=="simvastatin 40mg / ezetimibe 10mg tablet", "simvastatin", lens_term))%>%
  
  left_join(
 
# pis data 
  pis_data%>%
  left_join(codelists_bnf)%>%
  filter(codelist=="statins")%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  left_join(bnf_chapters, by=c("bnf_code"="code"))%>%
  separate(term, into=c("drug", "dose", "form"))%>%
  mutate(dose=str_sub(dose, 1, regexpr("mg", dose)-1))%>%
  select(-form, -chapter,-disp_y, -disp_m, -dispenser_location, -approved_name, -codelist)%>%
  rename(pis_dose = dose, pis_term = drug)%>%
  left_join(participants%>%select(participant_id, rand_date))%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"))%>%
  mutate(time_before_rand = interval(disp_ym, rand_month)%/%months(1))%>%
  filter(time_before_rand>(-1))%>%
  group_by(pis_term, participant_id)%>%
  arrange(time_before_rand)%>%
  slice_head(n=1)%>%
  distinct(participant_id, pis_term, .keep_all=T)%>%
  select(-rand_date, -rand_month, -disp_ym, -bnf_code)%>%
  mutate(pis_term = tolower(pis_term)),
by=c("participant_id", "lens_term"="pis_term")
)->dose_comparison_data

# calculate distinct vs similar

#### FIGURE (SLIDES) -----

dose_comparison_data%>%
  group_by(lens_term, number_items)%>%
  summarise(`Different dose`=n_distinct(participant_id[lens_dose!=pis_dose]),
            `Equal dose` = n_distinct(participant_id[lens_dose==pis_dose]))%>%
  pivot_longer(c(`Different dose`, `Equal dose`), names_to = "Group", values_to = "Count")%>%
  group_by(lens_term, number_items)%>%
  mutate(Prop = round(Count/sum(Count)*100,0))%>%
  filter(!is.na(number_items))%>%
  mutate(lens_term = str_to_title(lens_term))%>%
  mutate(number_items = if_else(number_items=="1", "1 item", "2 items"))%>%
  filter(number_items=="1 item")%>%
  
  ggplot(aes(Group, Count, fill=Group))+
  geom_bar(stat="identity")+
  # facet_grid(cols=vars(lens_term), rows=vars(number_items),switch = "y")+
  facet_wrap(~lens_term, ncol=4)+
  geom_text(aes(label=paste0(Count, " (", Prop, "%)")), vjust=-1, size=6, color="white")+
  oxpop_blue_panel+
  labs(title="Equivalence of PIS data drug coding to self-reported drug dosage for statins",
       subtitle = "Split by number of items dispensed (in PIS record)",
       # caption="Equivalent records defined as the PIS record in the month of randomisation or the most recent one before randomisation. Proportions calculated per drug and number of items",
       x="Dose equivalence",
       fill=NULL)+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  theme(legend.position = "bottom")

ggsave("Outputs/Figures/drug_coding_equivalence.png",
       height=30,
       width= 60,
       dpi="retina",
       units = "cm")

#### FIGURE (THESIS) -----

dose_comparison_data%>%
  group_by(lens_term, number_items)%>%
  summarise(`Different dose`=n_distinct(participant_id[lens_dose!=pis_dose]),
            `Equal dose` = n_distinct(participant_id[lens_dose==pis_dose]))%>%
  pivot_longer(c(`Different dose`, `Equal dose`), names_to = "Group", values_to = "Count")%>%
  group_by(lens_term, number_items)%>%
  mutate(Prop = round(Count/sum(Count)*100,0))%>%
  filter(!is.na(number_items))%>%
  mutate(lens_term = str_to_title(lens_term))%>%
  mutate(number_items = if_else(number_items=="1", "1 item", "2 items"))%>%
  filter(number_items=="1 item")%>%
  
  
  ggplot(aes(Group, Count, fill=Group))+
  geom_bar(stat="identity")+
  # facet_grid(cols=vars(lens_term), rows=vars(number_items),switch = "y")+
  facet_wrap(~lens_term, ncol=4)+
  geom_text(aes(label=paste0(Count, " (", Prop, "%)")), vjust=-1, size=6, 
            # color="white"
            )+
  # oxpop_blue_panel+
  labs(
    # title="Equivalence of PIS data drug coding to self-reported drug dosage for statins",
    #    subtitle = "Split by number of items dispensed (in PIS record)",
       # caption="Equivalent records defined as the PIS record in the month of randomisation or the most recent one before randomisation. Proportions calculated per drug and number of items",
       x="Dose equivalence",
       fill=NULL)+
  scale_y_continuous(expand=expansion(c(0,0.3)))+
  theme(legend.position = "bottom",
        text=element_text(size=30, color="black"))

ggsave("Outputs/Thesis/Figures/drug_coding_equivalence.png",
       height=30,
       width= 60,
       dpi="retina",
       units = "cm")

ggsave("Outputs/Thesis/Figures/HR/drug_coding_equivalence.tiff",
       height=30,
       width= 60,
       dpi="retina",
       units = "cm")



# rm(dose_comparison_data)
















# 10. Agreement between PIS and LENS (baseline) -------

## 10.1 LENS ----------

rand_drugs%>%
  left_join(drugs, by="med_drug_code")%>%
  filter(cat_name %in% lens_drugs$cat_name)%>%
  left_join(lens_drugs)%>%
  distinct(participant_id, labels)%>%
  mutate(lens_flag="1")->lens_participants_on_each_drug_list

lens_participants_on_each_drug_list%>%
  group_by(labels)%>%
  summarise(Participants = n_distinct(participant_id),
            Proportion = round(Participants/lens_participants_number*100, 1))->lens_drugs_counts


## 10.2 PIS ----------

pis_data%>%
  select(participant_id, bnf_code, disp_ym)%>%
  mutate(disp_y = substr(disp_ym,1,4),
         disp_m = substr(disp_ym,5,6),
         disp_ym = as.Date(paste0(disp_y, "-", disp_m, "-01")))%>%
  left_join(participants%>%select(participant_id, rand_date))%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"))%>%
  mutate(time_before_rand = interval(disp_ym, rand_month)%/%months(1))%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  filter(!is.na(labels))%>%
  filter(time_before_rand %in% c(1,2,3,0))%>% # restrict to records in 90 days before rand
  distinct(participant_id, labels)%>%
  mutate(pis_flag="1")->pis_participants_on_each_drug_list
  
pis_participants_on_each_drug_list%>%
  group_by(labels)%>%
  summarise(Participants = n_distinct(participant_id),
            Proportion = round(Participants/lens_participants_number*100, 1))->pis_drugs_counts



## 10.3 Join participants lists ---------

lens_participants_on_each_drug_list%>%
  full_join(pis_participants_on_each_drug_list, by=c("participant_id", "labels"))%>%
  mutate(across(everything(), ~as.character(.)))%>%
  mutate(across(-participant_id, ~replace_na(., "0")))%>%
  right_join(expand.grid(labels=lens_drugs%>%select(labels)%>%.[[1]],
                         participant_id=as.character(lens_participants_list$participant_id)))%>% 
  mutate(across(-participant_id, ~replace_na(., "0")))->joint_lens_pis_participants_list_per_drug_group

rm(lens_participants_on_each_drug_list, pis_participants_on_each_drug_list)

joint_lens_pis_participants_list_per_drug_group%>%distinct(labels)%>%.[[1]]->drug_list



## 10.4 Calculate agreement ----------


kappa_drugs<-list()

for(i in drug_list){
  
  joint_lens_pis_participants_list_per_drug_group%>%
    filter(labels==paste0(i))->table
  
  try(k<-Kappa.test(table$lens_flag, table$pis_flag))
  
  if(exists("k")){
    kappa_drugs[[i]]<-k
  }
  else{
    kappa_drugs[[i]]<-list()
  }
  
  rm(i, table, k)
  
}

# transform results into table
kappa_drugs_results<-data.frame()

for(i in drug_list){
  
  name<-i
  
  try(
    table<-(data.frame(
      drug_group = paste0(name),
      kappa = unlist(kappa_drugs[[i]]$Result$estimate),
      CI_lower = unlist(kappa_drugs[[i]]$Result$conf.int[1]),
      CI_upper = unlist(kappa_drugs[[i]]$Result$conf.int[2]),
      p_value = unlist(kappa_drugs[[i]]$Result$p.value),
      judgement = unlist(kappa_drugs[[i]]$Judgement)
    )))
  
  try(kappa_drugs_results%<>%bind_rows(table))
  
  
  rm(table, name, i)
  
}



joint_lens_pis_participants_list_per_drug_group%>%
  group_by(labels)%>%
  summarise(pairs = n_distinct(participant_id, labels),
            positives = length(lens_flag[lens_flag=="1"]),
            negatives = length(lens_flag[lens_flag=="0"]),
            true_positives = length(lens_flag[lens_flag=="1" & pis_flag=="1"]),
            true_negatives = length(lens_flag[lens_flag=="0" & pis_flag=="0"]),
            false_positives = length(lens_flag[lens_flag=="0" & pis_flag =="1"]),
            false_negatives = length(lens_flag[lens_flag =="1" & pis_flag == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(pis_flag=="1" & lens_flag=="1"),
            LENS_only = sum(pis_flag=="0" & lens_flag=="1"),
            PIS_only = sum(pis_flag=="1" & lens_flag=="0"),
            Neither = sum(pis_flag=="0" & lens_flag=="0"))%>%
  mutate(across(c(Sensitivity, PPV), ~ifelse(str_detect(., "NaN"), "N/A", .)))%>%
  select(-c(pairs, positives, negatives, true_positives, true_negatives, false_positives, false_negatives))->two_by_two


#### Format table ------
kappa_drugs_results%>%
  full_join(two_by_two, by=c("drug_group" = "labels"))%>%
  mutate(CI = paste0(
    round(CI_lower,2),
    " : ",
    round(CI_upper, 2)))%>%
  
  mutate(kappa=round(kappa,2),
         p_value=as.character(round(p_value, 2)))%>%
  
  mutate(p_value=as.character(if_else(p_value==0, "<0.001", p_value)))%>%
  select(`Drug group` = drug_group, Both, LENS_only, PIS_only, Neither, Sensitivity, Specificity, PPV, NPV, Kappa=kappa, CI, p_value, judgement)->kappa_table

kappa_table%>%
  mutate(across(everything(), as.character))%>%
  mutate(across(c(Kappa, p_value, judgement), ~ifelse(str_detect(., "NA"), "N/A", .)),
         across(c(Kappa, p_value, judgement), ~replace_na(., "N/A")))%>%
  mutate(`Kappa (95% CI)`= paste0(Kappa, " (", CI, ")"))%>%
  relocate(`Kappa (95% CI)`, .before=judgement)%>%
  select(- CI, -p_value, -Kappa)%>%
  rename(`Both sources*` = Both,
         `Self-report only*` = LENS_only,
         `PIS data only*` = PIS_only,
         `Neither source*` = Neither,
         # `p value` = p_value,
         Judgement = judgement)%>%
  tibble()%>%
  arrange(`Drug group`)->drug_agreement_table

drug_agreement_table


#### Export table -----
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

#### TABLE (SLIDES) ------

drug_agreement_table%>%
  mutate(`Drug group` = factor(`Drug group`, levels=drug_labels_order))%>%
  arrange(`Drug group`)%>%
  custom_tab_data_frame(header = NULL,
                        footer = "Baseline exposure calculated using a 3 month lookback period and including data in the month of randomisation.\n*Counts presented are individual participants. Self-report used as reference for sensitivity, specificity, PPV, and NPV calculations. CI - confidence interval; PPV - positive predictive value; NPV - negative predictive value")%>%
  width(j=1, 7, unit = "cm")%>%
  width(j=c(2:7), 2.1, unit = "cm")%>%
  width(j=10, 3.5, unit = "cm")%>%
  width(j=11, 5, unit = "cm")%>%
  align(i=NULL, j=NULL, "center", "all")%>%
  # align(i=NULL, j=8, "left", "body")%>%
  align(i=1, j=1, "left", "header")%>%
  align(i=NULL, j=1, "left", "body")%>%
  align(i=1, j=1, "left", "footer")%>%
  add_header_row(values=c("", "Participant counts", "Agreement metrics"),
                 colwidths = c(1, 4, 6))%>%
  add_header_row(values="Agreement for drug exposure at randomisation between self-report and PIS data",
                 colwidths = 11)%>%
  border(j=c(1, 5), border.right=fp_border(width=2), part="body")%>%
  border(i=2, j=c(1, 2), border.right=fp_border(width=2), part="header")%>%
  border(i=3, j=c(1, 5), border.right=fp_border(width=2), part="header")%>%
  border(i=c(8, 11), border.bottom =fp_border(width=2), part="body")%>%
  save_as_docx(path="Outputs/Tables/agreement_drugs_table.docx", pr_section = sect_properties)

#### TABLE (THESIS) ------


drug_agreement_table%>%
  mutate(`Drug group` = factor(`Drug group`, levels=drug_labels_order))%>%
  arrange(`Drug group`)%>%
  flextable()%>%
  add_footer_lines("Baseline exposure calculated using a 3 month lookback period and including data in the month of randomisation.\n*Counts presented are individual participants. Self-report used as reference for sensitivity, specificity, PPV, and NPV calculations. CI - confidence interval; PPV - positive predictive value; NPV - negative predictive value")%>%
  width(j=1, 7, unit = "cm")%>%
  width(j=c(2:7), 2.1, unit = "cm")%>%
  width(j=10, 3.5, unit = "cm")%>%
  width(j=11, 5, unit = "cm")%>%
  align(i=NULL, j=NULL, "center", "all")%>%
  # align(i=NULL, j=8, "left", "body")%>%
  align(i=1, j=1, "left", "header")%>%
  align(i=NULL, j=1, "left", "body")%>%
  align(i=1, j=1, "left", "footer")%>%
  add_header_row(values=c("", "Participant counts", "Agreement metrics"),
                 colwidths = c(1, 4, 6))%>%
  # add_header_row(values="Agreement for drug exposure at randomisation between self-report and PIS data",
  #                colwidths = 11)%>%
  border(j=c(1, 5), border.right=fp_border(width=2), part="body")%>%
  border(i=2, j=c(1, 5), border.right=fp_border(width=2), part="header")%>%
  # border(i=3, j=c(1, 5), border.right=fp_border(width=2), part="header")%>%
  border(i=c(8, 11), border.bottom =fp_border(width=2), part="body")%>%
  save_as_docx(path="Outputs/Thesis/Tables/agreement_drugs_table.docx", pr_section = sect_properties)

### investigate discrepant cases -----


joint_lens_pis_participants_list_per_drug_group%>%
  filter(lens_flag!=pis_flag)%>%
  # filter(labels=="Insulin")%>%
  left_join(participants%>%select(participant_id, rand_date)%>%mutate(participant_id=as.character(participant_id)))%>%
  left_join(pis_data%>%mutate(participant_id=as.character(participant_id)))%>%
  select(-dispenser_location, -number_items)%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels%>%rename(bnf_label=labels))%>%
  arrange(participant_id, disp_ym)%>%
  mutate(rand_month=as.integer(paste0(str_sub(rand_date, 1,4),str_sub(rand_date, 6,7))),
         disp_date = as.Date(paste0(str_sub(disp_ym,1,4), "-", str_sub(disp_ym,5,6), "-01"), format="%Y-%m-%d"),
         disp_ym=as.integer(disp_ym),
         rand_date=paste0(str_sub(rand_date,1,8), "01"))%>%
  group_by(participant_id)%>%
  mutate(minimum_time_between_rand_first_dispensing = as.integer(interval(min(disp_date),rand_date)%/%months(1)))%>%
    # calculate availability of PIS data before randomisation
  # filter(codelist=="insulin")%>%
  filter(labels==bnf_label)%>%
  group_by(participant_id, labels)%>%
  
  mutate(dispensing_interval=if_else(as.integer(interval(lag(disp_date, n=1L), disp_date)%/%months(1))>0, as.integer(interval(lag(disp_date, n=1L), disp_date)%/%months(1)), NA_integer_))%>%
  # calculate interval between each dispensing record or same drug group (excludes repeat records in one month)
  mutate(average_dispensing_interval=median(dispensing_interval, na.rm=T))%>%
  # calculate average dispensing interval for each participant and drug group
  
  mutate(interval_rand_dispensing = interval(rand_date,disp_date)%/%months(1))%>%
  # calculate interval between randomisation and each dispensing record
  
  mutate(records_before_after_rand = if_else(min(interval_rand_dispensing)<0 & max (interval_rand_dispensing)>0, "Y", "N"))%>%
  
  
  mutate(discrepancy_type=case_when(
    lens_flag==1 & 
      pis_flag==0 & 
        (average_dispensing_interval>=minimum_time_between_rand_first_dispensing  | (records_before_after_rand=="Y"))~ "Missed by PIS (short availability pre-randomisation)",
    
    lens_flag==1 & 
      pis_flag==0 & 
      (minimum_time_between_rand_first_dispensing>max(dispensing_interval) | is.na(average_dispensing_interval))&
      interval_rand_dispensing>=0 ~ "Incorrect recording in CRF (post-randomisation initiation)",
    
    lens_flag==1 & 
      pis_flag==0 & 
        records_before_after_rand=="N" &
          interval_rand_dispensing<0 ~ "Incorrect recording in CRF (medication stopped before randomisation)",
    
    lens_flag==0 & 
      pis_flag==1 & 
        min(disp_ym)==rand_month&
          length(labels)>1~ "Unclear (possible initiation in month of randomisation)",
    
    lens_flag==1 & 
      pis_flag==0 & 
      min(interval_rand_dispensing)>=1 &
      average_dispensing_interval < minimum_time_between_rand_first_dispensing&
      length(labels)>1~ "Unclear (missed by PIS or initiation in month after randomisation)",
    
    
    
    lens_flag==0 & 
      pis_flag==1 & 
        min(disp_ym)<rand_month &
          records_before_after_rand=="Y" &
            length(labels[interval_rand_dispensing>0]>1)~ "Missed by CRF (established pre-randomisation, continued after)",
    
    lens_flag==1 & 
      pis_flag==0 & 
      is.na(approved_name) ~ "Missed by PIS (no PIS data available)",
    
    lens_flag==0 & 
      pis_flag==1 & 
      max(disp_ym)<=rand_month ~ "Incorrect recording in PIS (medication stopped)"))->discrepant_records_table  

# View(discrepant_records_table)
# 
# View(pis_data)

discrepant_records_table%>%
  mutate(labels=factor(labels, drug_labels_order))%>%
  group_by(labels, discrepancy_type)%>%
  summarise(Participants=n_distinct(participant_id))%>%
  mutate(discrepancy_group = case_when(discrepancy_type == "Missed by PIS (short availability pre-randomisation)" ~ "1",
                                       discrepancy_type == "Missed by CRF (established pre-randomisation, continued after)" ~ "2",
                                       discrepancy_type == "Incorrect recording in CRF (post-randomisation initiation)" ~ "3",
                                       discrepancy_type == "Incorrect recording in CRF (medication stopped before randomisation)" ~ "4",
                                       discrepancy_type == "Incorrect recording in PIS (medication stopped)" ~ "5",
                                       discrepancy_type == "Unclear (possible initiation in month of randomisation)" ~ "6",
                                       discrepancy_type == "Unclear (missed by PIS or initiation in month after randomisation)" ~ "7"))%>%
  
  ggplot(aes(discrepancy_group, Participants, fill=discrepancy_group))+
  geom_col(color="black")+
  facet_wrap(~labels, 
             # scales="free_x"
             )+
  theme(
    # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        legend.position = "none",
        text=element_text(size=30,
                          family="Mulish"))+
  geom_text(aes(label=Participants), vjust=-1,
            size=6)+
  guides(fill=guide_legend(
    nrow=7,
    #                        ncol=7
                           # byrow=TRUE
                           ))+
  labs(fill="Discrepancy type",
       x = "Discrepancy type"
       # title="Classification of discrepancies between CRF and PIS data for baseline drug exposure"
       )+
  scale_y_continuous(limits=c(0,50))+
  scale_fill_manual(values=brewer.pal(7, "BrBG"),
                    labels = function(x) str_wrap(x, width = 250))

  

# figure label:
# Missed by PIS (short availability pre-randomisation) - CRF yes, PIS no; no relevant records during run-in, relevant records within 3 months after randomisation, and average dispensing interval is larger or same as PIS availability pre-randomisation or relevant records before and after randomisation
# Incorrect recording in CRF (post-randomisation initiation) - CRF yes, PIS no; first relevant PIS record after month of randomisation, and PIS availability pre-randomisation is longer than the larger dispensing interval for that drug
# Incorrect recording in CRF (medication stopped before randomisation) - CRF yes, PIS no; relevant PIS records before randomisation, but none in the month of randomisation or after
# Unclear (possible initiation in month of randomisation) - CRF no, PIS yes; first record in month of randomisation, several records after
# Unclear (missed by PIS or initiation in month after randomisation) - CRF yes, PIS no; first relevant PIS record in the month after randomisation, several relevant PIS records after randomisation, average dispensing interval shorter than PIS availability before randomisation
# Missed by CRF (established pre-randomisation, continued after) - CRF no, PIS yes; distinct relevant PIS records before and after randomisation, several distinct relevant records after randomisation
# Missed by PIS (no PIS data available)
# Incorrect recording in PIS (medication stopped) - CRF no, PIS yes; relevant records only before or in the month of randomisation"


##### save figure (thesis) -----
ggsave("Outputs/Thesis/Figures/discrepancy_counts.png",
       dpi="retina",
       width=62,
       height=35,
       units="cm")


ggsave("Outputs/Thesis/Figures/HR/discrepancy_counts.tiff",
       dpi="retina",
       width=62,
       height=35,
       units="cm")


##### save figure (slides) -----

discrepant_records_table%>%
  group_by(labels, discrepancy_type)%>%
  summarise(Participants=n_distinct(participant_id))%>%
  
  ggplot(aes(discrepancy_type, Participants, fill=discrepancy_type))+
  geom_col()+
  facet_wrap(~labels, 
             # scales="free_x"
  )+
  oxpop_blue_panel+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "bottom")+
  geom_text(aes(label=Participants), vjust=-1, color="white")+
  guides(fill=guide_legend(
    nrow=7,
    #                        ncol=7
    # byrow=TRUE
  ))+
  labs(fill="Discrepancy type",
       title="Classification of discrepancies between CRF and PIS data for baseline drug exposure")+
  scale_y_continuous(limits=c(0,50))+
  scale_fill_discrete(
    #values=brewer.pal(7, "BrBG"),
                    labels = function(x) str_wrap(x, width = 150))

ggsave("Outputs/Figures/discrepancy_counts.png",
       dpi="retina",
       width=62,
       height=35,
       units="cm")

## 10.5 Barcharts --------

joint_lens_pis_participants_list_per_drug_group%>%
  group_by(labels)%>%
  summarise(Both=n_distinct(participant_id[pis_flag=="1" & lens_flag=="1"]),
            Either = n_distinct(participant_id[pis_flag=="1" | lens_flag=="1"]),
            `Self-report only` = n_distinct(participant_id[pis_flag=="0" & lens_flag=="1"]),
            `PIS only` = n_distinct(participant_id[pis_flag=="1" & lens_flag=="0"]))%>%
  pivot_longer(-labels, names_to = "group", values_to="count")%>%
  group_by(labels)%>%
  mutate(prop=round(count/sum(count[group!="Either"])*100, 1))%>%
  mutate(group=fct_relevel(group, "Either", "Both", "Self-report only", "PIS only"),
         prop=if_else(is.na(prop), 0, prop))%>%
  mutate(labels=factor(labels, levels=drug_labels_order))%>%
  arrange(labels)%>%
  
  
  ggplot(aes(group, count, fill=group))+
  geom_bar(stat="identity")+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  geom_text(aes(label=paste0(count, "\n(", prop, "%)")), vjust=-0.2, size=6, color="white")+
  oxpop_blue_panel+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  labs(x="Data source",
       fill="Data source",
       y="Participant count",
       title="Representation of different drug groups in participant self-report versus PIS data",
       # caption="Proportions calculated with the total number of participants identified in either source (Either) as reference\nBaseline exposure defined as a record within 3 months before randomisation or within the month of randomisation"
       )+
  scale_y_continuous(limits=c(0, NA),
                   expand=expansion(c(0,0.6)))


ggsave("Outputs/Figures/agreement_barcharts.png",
       dpi="retina",
       width=62,
       height=40,
       units="cm")

### 10.5.2 Total counts each source -----

#### slides -----

joint_lens_pis_participants_list_per_drug_group%>%
  group_by(labels)%>%
  summarise(Both=n_distinct(participant_id[pis_flag=="1" & lens_flag=="1"]),
            Either = n_distinct(participant_id[pis_flag=="1" | lens_flag=="1"]),
            `CRF` = n_distinct(participant_id[lens_flag=="1"]),
            `PIS` = n_distinct(participant_id[pis_flag=="1"]))%>%
  pivot_longer(-labels, names_to = "group", values_to="count")%>%
  group_by(labels)%>%
  mutate(prop=round(count/lens_participants_number*100, 1))%>%
  mutate(group=fct_relevel(group, "Either", "Both", "CRF", "PIS"),
         prop=if_else(is.na(prop), 0, prop))%>%
  mutate(labels=factor(labels, levels=drug_labels_order))%>%
  arrange(labels)%>%
  filter(group%in%c("PIS", "CRF"))%>%
  
  
  ggplot(aes(group, count, fill=group))+
  geom_bar(stat="identity")+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  geom_text(aes(label=paste0(count, "\n(", prop, "%)")), vjust=-0.2, size=6, color="white")+
  oxpop_blue_panel+
  theme(legend.position="bottom",
        axis.text.x = element_blank())+
  labs(x="Data source",
       fill="Data source",
       y="Participant count",
       title="Representation of different drug groups in participant self-report versus PIS data",
       # caption="Proportions calculated with the total number of participants identified in either source (Either) as reference\nBaseline exposure defined as a record within 3 months before randomisation or within the month of randomisation"
  )+
  scale_y_continuous(limits=c(0, NA),
                     expand=expansion(c(0,0.6)))

ggsave("Outputs/Figures/agreement_barcharts_baseline_pis_vs_crf.png",
       dpi="retina",
       width=62,
       height=35,
       units="cm")

#### thesis  -----


joint_lens_pis_participants_list_per_drug_group%>%
  group_by(labels)%>%
  summarise(Both=n_distinct(participant_id[pis_flag=="1" & lens_flag=="1"]),
            Either = n_distinct(participant_id[pis_flag=="1" | lens_flag=="1"]),
            `CRF` = n_distinct(participant_id[lens_flag=="1"]),
            `PIS` = n_distinct(participant_id[pis_flag=="1"]))%>%
  pivot_longer(-labels, names_to = "group", values_to="count")%>%
  group_by(labels)%>%
  mutate(prop=round(count/lens_participants_number*100, 1))%>%
  mutate(group=fct_relevel(group, "Either", "Both", "CRF", "PIS"),
         prop=if_else(is.na(prop), 0, prop))%>%
  mutate(labels=factor(labels, levels=drug_labels_order))%>%
  arrange(labels)%>%
  filter(group%in%c("PIS", "CRF"))%>%
  
  
  ggplot(aes(group, count, fill=group))+
  geom_bar(stat="identity")+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 15))+
  geom_text(aes(label=paste0(count, "\n(", prop, "%)")), vjust=-0.2, size=6)+
  theme(legend.position="bottom",
        axis.text.x = element_blank(),
        text=element_text(size=30),
        axis.title.x=element_blank())+
  labs(
    # x="Data source",
       fill="Data source",
       y="Participant count",
       # title="Representation of different drug groups in participant self-report versus PIS data",
       # caption="Proportions calculated with the total number of participants identified in either source (Either) as reference\nBaseline exposure defined as a record within 3 months before randomisation or within the month of randomisation"
  )+
  scale_y_continuous(limits=c(0, NA),
                     expand=expansion(c(0,0.6)))

ggsave("Outputs/Thesis/Figures/agreement_barcharts_baseline_pis_vs_crf.png",
       dpi="retina",
       width=62,
       height=45,
       units="cm")

ggsave("Outputs/Thesis/Figures/HR/agreement_barcharts_baseline_pis_vs_crf.tiff",
       dpi="retina",
       width=62,
       height=35,
       units="cm")




















# 
# 
# rm(lens_drugs_counts, kappa_rules_aggregate_results, kappa_rules_aggregate, kappa_drugs, kappa_drugs_results, pis_drugs_counts, two_by_two,joint_lens_pis_participants_list_per_drug_group, drug_agreement_table, kappa_table)




# 11. Agreement (follow-up) ----------

# pis vs self-report

pis_data%>%
  left_join(codelists_bnf)%>%
  left_join(bnf_chapters, by=c("bnf_code"="code"))%>%
  left_join(contraindications_pis)%>%
  filter(!is.na(labels))%>%
  filter(labels!="Contraindicated drugs")%>%
  separate(term, into=c("drug", "dose", "form"), remove=F)%>%
  mutate(dose=str_sub(dose, 1, regexpr("mg", dose)-1))%>%
  filter(labels!="Rosuvastatin" | (labels=="Rosuvastatin" & dose=="40"))%>%
  select(participant_id, labels, disp_ym)%>%
  left_join(lens_participants_list%>%mutate(participant_id=as.character(participant_id)))%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"),
         disp_ym = as.Date(paste0(substr(disp_ym,1,4), "-", substr(disp_ym,5,6), "-01")))%>%
  mutate(time_after_rand=difftime(disp_ym, rand_month, units = "days"))%>%
  distinct(participant_id, labels, time_after_rand)%>%
  group_by(participant_id, labels)%>%
  slice_min(order_by = time_after_rand)%>%
  mutate(time_after_rand = as.integer(time_after_rand))%>%
  mutate(Source="PIS")%>%
  group_by(labels)%>%
  mutate(value=1)%>%
  arrange(time_after_rand)%>%
  group_by(labels)%>%
  mutate(cumulative=cumsum(value))%>%
  select(-value)%>%
  filter(labels!="Vitamin K antagonists")->contraindicated_drugs_after_rand_pis

## timeseries plot -----
contraindicated_drugs_after_rand_self_report%>%
  mutate(Source="Self-report")%>%
  mutate(participant_id=as.character(participant_id))%>%
  rename(time_after_rand=days_after_rand,
         labels=Drug)%>%
  select(-value)%>%
  
  rbind(contraindicated_drugs_after_rand_pis)%>%
  
  ggplot(aes(time_after_rand, cumulative, color=Source))+
  geom_point(size=4)+
  geom_line()+
  geom_text_repel(aes(label=cumulative), size=6, vjust=-1)+
  facet_wrap(~labels)+
  labs(title="Timeseries of participants initiating contra-indicated drugs after randomisation (self-reported vs PIS data)",
       x="Days after randomisation",
       y="Cumulative sum")+
  oxpop_blue_panel+
  theme(legend.position = "bottom")+
  scale_y_continuous(limits=c(0, NA), expand = expansion(c(0,0.5)))+
  scale_x_continuous(limits=c(0, NA),
                     breaks=seq(0,800, 50))


ggsave("Outputs/Figures/fu_timeseries_self_report_vs_pis.png",
       height=32,
       width=62,
       dpi="retina",
       units="cm")

## barchart ----

### slides ------

#### counts in either/both/one source only ----

contraindicated_drugs_after_rand_self_report%>%
  mutate(Source="Self-report")%>%
  rename(time_after_rand=days_after_rand,
         labels=Drug)%>%
  select(-value)%>%
  mutate(participant_id=as.character(participant_id))%>%
  
  rbind(contraindicated_drugs_after_rand_pis)%>%
  select(-cumulative)%>%
  mutate(flag=1)%>%
  pivot_wider(c(labels, participant_id), names_from="Source", values_from="flag")%>%
  mutate(across(everything(), ~as.character(.)))%>%
  mutate(across(everything(), ~replace_na(., "0")))%>%
  group_by(labels)%>%
  summarise(`Self-report only` = n_distinct(participant_id[`Self-report`==1 & PIS==0]),
            `PIS only` = n_distinct(participant_id[`Self-report`==0 & PIS==1]),
            `Self-report and PIS` = n_distinct(participant_id[`Self-report`==1 & PIS==1]))%>%
  pivot_longer(-labels, names_to="Group", values_to="Participants")%>%
  mutate(Group=factor(Group, levels=c("Self-report and PIS", "Self-report only", "PIS only")))%>%
  
  ggplot(aes(Group, Participants, fill=Group))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(cols=vars(labels),
             labeller = label_wrap_gen(width = 15),
)+
  geom_text(aes(label=Participants), vjust=-1, color="white", size=6)+
  oxpop_blue_panel+
  theme(legend.position="bottom",
        # axis.text.x=element_text(angle=30, 
        #                          hjust=1, 
        #                          # vjust=0
        #                          ),
        axis.text.x = element_blank()
                                 )+
  labs(title="Representation of participants initiating contra-indicated drugs after randomisation in each source",
       fill="Source")
  
ggsave("Outputs/Figures/fu_barcharts_self_report_vs_pis.png",
       height=32,
       width=62,
       dpi="retina",
       units="cm")

#### counts in each source -----

contraindicated_drugs_after_rand_self_report%>%
  mutate(Source="Self-report")%>%
  rename(time_after_rand=days_after_rand,
         labels=Drug)%>%
  select(-value)%>%
  mutate(participant_id=as.character(participant_id))%>%
  
  
  rbind(contraindicated_drugs_after_rand_pis)%>%
  select(-cumulative)%>%
  mutate(flag=1)%>%
  pivot_wider(c(labels, participant_id), names_from="Source", values_from="flag")%>%
  mutate(across(everything(), ~as.character(.)))%>%
  mutate(across(everything(), ~replace_na(., "0")))%>%
  group_by(labels)%>%
  summarise(`CRF` = n_distinct(participant_id[`Self-report`==1]),
            `PIS` = n_distinct(participant_id[PIS==1]))%>%
  pivot_longer(-labels, names_to="Group", values_to="Participants")%>%
  mutate(Group=factor(Group, levels=c("CRF", "PIS")))%>%
  
  ggplot(aes(Group, Participants, fill=Group))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(cols=vars(labels),
             labeller = label_wrap_gen(width = 15),
  )+
  geom_text(aes(label=Participants), vjust=-1, color="white", size=6)+
  oxpop_blue_panel+
  theme(legend.position="bottom",
        # axis.text.x=element_text(angle=30, 
        #                          hjust=1, 
        #                          # vjust=0
        #                          ),
        axis.text.x = element_blank()
  )+
  labs(title="Representation of participants initiating contra-indicated drugs after randomisation in each source",
       fill="Source")

ggsave("Outputs/Figures/fu_barcharts_self_report_vs_pis_counts_each_source.png",
       height=32,
       width=62,
       dpi="retina",
       units="cm")


### thesis -----


contraindicated_drugs_after_rand_self_report%>%
  mutate(Source="Self-report")%>%
  rename(time_after_rand=days_after_rand,
         labels=Drug)%>%
  select(-value)%>%
  mutate(participant_id=as.character(participant_id))%>%
  
  
  rbind(contraindicated_drugs_after_rand_pis%>%mutate(participant_id=as.character(participant_id)))%>%
  select(-cumulative)%>%
  mutate(flag=1)%>%
  pivot_wider(c(labels, participant_id), names_from="Source", values_from="flag")%>%
  mutate(across(everything(), ~as.character(.)))%>%
  mutate(across(everything(), ~replace_na(., "0")))%>%
  group_by(labels)%>%
  summarise(`CRF` = n_distinct(participant_id[`Self-report`==1]),
            `PIS` = n_distinct(participant_id[PIS==1]))%>%
  pivot_longer(-labels, names_to="Group", values_to="Participants")%>%
  mutate(Group=factor(Group, levels=c("CRF", "PIS")))%>%
  filter(labels%in%c("Fibrates", "Warfarin"))%>%
  
  ggplot(aes(Group, Participants, fill=Group))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(cols=vars(labels),
             labeller = label_wrap_gen(width = 15),
  )+
  geom_text(aes(label=Participants), vjust=-1,size=10)+
  theme(legend.position="bottom",
        text=element_text(size=30),
        # axis.text.x=element_text(angle=30, 
        #                          hjust=1, 
        #                          # vjust=0
        #                          ),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
        
  )+
  labs(
    # title="Representation of participants initiating contra-indicated drugs after randomisation in each source",
       fill="Source")+
  scale_y_continuous(breaks=seq(0,2,1),
                     expand=expansion(c(0,0.2)))

ggsave("Outputs/Thesis/Figures/fu_barcharts_self_report_vs_pis_counts_each_source.png",
       height=32,
       width=62,
       dpi="retina",
       units="cm")

ggsave("Outputs/Thesis/Figures/HR/fu_barcharts_self_report_vs_pis_counts_each_source.tiff",
       height=32,
       width=62,
       dpi="retina",
       units="cm")







# 12. Changes after randomisation -------

## binary (yes or now) -------

### all participants -------

# CRF data at randomisation

crf_flags_at_randomisation<-
  
  as.data.frame(expand.grid(
    participant_id = participants%>%select(participant_id)%>%.[[1]],
    labels=lens_drugs%>%select(labels)%>%.[[1]]))%>%
  mutate(participant_id=as.character(participant_id))%>%
  left_join(
  rand_drugs%>%
  left_join(drugs)%>%
  left_join(lens_drugs)%>%
  distinct(participant_id, labels)%>%
  filter(!is.na(labels))%>%
  mutate(at_randomisation="1"))%>%
  
  # add 0 flags for drugs not used
  mutate(at_randomisation = if_else(is.na(at_randomisation), "0", at_randomisation))





# pis records after randomisation
pis_records_after_randomisation<-
  pis_data%>%
  select(participant_id, bnf_code, disp_ym)%>%
  left_join(lens_participants_list %>%mutate(participant_id=as.character(participant_id)))%>% # join randomisation dates
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"),
         disp_ym = as.Date(paste0(substr(disp_ym,1,4), "-", substr(disp_ym,5,6), "-01")))%>%
  select(-rand_date)%>%
  filter(disp_ym>rand_month)%>% # restricting to records after the month of randomisation
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  distinct(participant_id, labels)%>%
  mutate(after_randomisation="1")

# merge
changes_after_rand<-
  crf_flags_at_randomisation%>%
  full_join(pis_records_after_randomisation%>%mutate(participant_id=as.character(participant_id)), by=c("participant_id", "labels"))%>% # using a full join ensures that people with no records at randomisation but with records in PIS afterwards can be represented; likewise people with records at randomisation but no records in PIS are also represented
  filter(!is.na(labels))%>%
  mutate(across(c(at_randomisation, after_randomisation), ~replace_na(., "0")))

# plot
changes_after_rand%>%
  rename("At\nrandomisation"=at_randomisation,
         "After\nrandomisation" = after_randomisation)%>%
  pivot_longer(c("At\nrandomisation", "After\nrandomisation"), names_to = "Period", values_to = "Event")%>%
  mutate(Event=case_when(Event=="1" ~ "Yes",
                         TRUE~"No"))%>%
  mutate(Period=fct_relevel(Period, "At\nrandomisation", "After\nrandomisation"))->alluvial_plot_data

#### FIGURE (SLIDES) -----

alluvial_plot_data%>%
  ggplot(aes(x=Period, 
             stratum=Event,
             alluvium=participant_id,
             fill=Event,
             label=Event
             ))+
  geom_flow(stat="alluvium", 
            # color="darkgray"
            )+
  geom_stratum(na.rm=T)+
  geom_text(
    data=.%>%filter(Period=="At\nrandomisation"),
      stat="stratum",
    aes(label=
    paste0(after_stat(count),
           "\n(",
      round(after_stat(prop)*100,1), "%)")
    ),
    nudge_x = -0.4,
    color="white"
    )+
  geom_text(data=.%>%filter(Period!="At\nrandomisation"),
    stat="stratum",
            aes(label=
                  paste0(after_stat(count), 
                         "\n(",
                         round(after_stat(prop)*100,1), "%)")
            ),
             nudge_x = 0.4,
    color="white"
    )+
  facet_wrap(~labels, 
             scales="free_y",
             # ncol=6,
             labeller = label_wrap_gen(width = 30))+
  geom_text(
    stat="flow", 
    aes(color = Period == "At\nrandomisation",
        label=after_stat(count)),
    nudge_x=0.3,
    show.legend = F
  )+
  scale_color_manual(values=c("00000000", "White"))+
  oxpop_blue_panel+
  scale_y_continuous(expand=expansion(c(0,0.2)))->plot_binary

plot_binary

ggsave("Outputs/Figures/changes_after_rand_binary_all_participants.png",
       width = 60,
       height=30,
       dpi="retina",
       units="cm")


#### FIGURE (THESIS) -------


alluvial_plot_data%>%
  mutate(Event=if_else(Event=="Yes", "Exposed", "Not exposed"))%>%
  mutate(Event=factor(Event, c("Exposed","Not exposed")))%>%
  ggplot(aes(x=Period, 
             stratum=Event,
             alluvium=participant_id,
             fill=Event,
             label=Event
  ))+
  geom_flow(stat="alluvium", 
            reverse=F
            # color="darkgray"
  )+
  geom_stratum(na.rm=T,
               reverse=F)+
  facet_wrap(~labels, 
             scales="free_y",
             ncol=6,
             labeller = label_wrap_gen(width = 20))+
  geom_text(
    data=.%>%filter(Period=="At\nrandomisation"),
    stat="stratum",
    aes(label=
          paste0(after_stat(count),
                 "\n(",
                 round(after_stat(prop)*100,1), "%)")
    ),
    nudge_x = -0.4,
    # color="white"
  )+
  geom_text(data=.%>%filter(Period!="At\nrandomisation"),
            stat="stratum",
            aes(label=
                  paste0(after_stat(count), 
                         "\n(",
                         round(after_stat(prop)*100,1), "%)")
            ),
            nudge_x = 0.4,
            # color="white"
  )+
  
  geom_text_repel(
    stat="flow", 
    reverse=F,
    aes(color = Period == "At\nrandomisation",
        label=after_stat(count)),
    nudge_x=0.3,
    show.legend = F,
    segment.size=0.2,
    segment.color = 'transparent'
    
  )+
  theme(text=element_text(size=15,
                          color="black"),
        legend.position="bottom")+
  scale_color_manual(values=c("00000000", "Black"),
                     # breaks = c("Exposed","Not exposed")
                     )+
  # oxpop_blue_panel+
  scale_y_continuous(expand=expansion(c(0.2,0.2)))->plot_binary

plot_binary

ggsave("Outputs/Thesis/Figures/changes_after_rand_binary_all_participants.png",
       width = 60,
       height=30,
       dpi="retina",
       units="cm")

ggsave("Outputs/Thesis/Figures/HR/changes_after_rand_binary_all_participants.tiff",
       width = 60,
       height=40,
       dpi="retina",
       units="cm")

### only participants with relevant drug at randomisaiton ------


# CRF data at randomisation

crf_flags_at_randomisation<-
  rand_drugs%>%
  left_join(drugs)%>%
  left_join(lens_drugs)%>%
  distinct(participant_id, labels)%>%
  filter(!is.na(labels))%>%
  mutate(at_randomisation="1")



# pis records after randomisation
pis_records_after_randomisation<-
  pis_data%>%
  select(participant_id, bnf_code, disp_ym)%>%
  left_join(lens_participants_list %>%mutate(participant_id=as.character(participant_id)))%>% # join randomisation dates
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"),
         disp_ym = as.Date(paste0(substr(disp_ym,1,4), "-", substr(disp_ym,5,6), "-01")))%>%
  select(-rand_date)%>%
  filter(disp_ym>rand_month)%>% # restricting to records after the month of randomisation
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  distinct(participant_id, labels)%>%
  mutate(after_randomisation="1")

# merge
changes_after_rand<-
  crf_flags_at_randomisation%>%
  full_join(pis_records_after_randomisation%>%mutate(participant_id=as.character(participant_id)), by=c("participant_id", "labels"))%>% # using a full join ensures that people with no records at randomisation but with records in PIS afterwards can be represented; likewise people with records at randomisation but no records in PIS are also represented
  filter(!is.na(labels))%>%
  mutate(across(c(at_randomisation, after_randomisation), ~replace_na(., "0")))

# plot
changes_after_rand%>%
  rename("At\nrandomisation"=at_randomisation,
         "After\nrandomisation" = after_randomisation)%>%
  pivot_longer(c("At\nrandomisation", "After\nrandomisation"), names_to = "Period", values_to = "Event")%>%
  mutate(Event=case_when(Event=="1" ~ "Yes",
                         TRUE~"No"))%>%
  mutate(Period=fct_relevel(Period, "At\nrandomisation", "After\nrandomisation"))->alluvial_plot_data

#### FIGURE (THESIS) ------

alluvial_plot_data%>%
  ggplot(aes(x=Period, 
             stratum=Event,
             alluvium=participant_id,
             fill=Event,
             label=Event
  ))+
  geom_flow(stat="alluvium", 
            # color="darkgray"
  )+
  geom_stratum(na.rm=T)+
  geom_text(
    data=.%>%filter(Period=="At\nrandomisation"),
    stat="stratum",
    aes(label=
          paste0(after_stat(count),
                 "\n(",
                 round(after_stat(prop)*100,1), "%)")
    ),
    nudge_x = -0.4,
  )+
  geom_text(data=.%>%filter(Period!="At\nrandomisation"),
            stat="stratum",
            aes(label=
                  paste0(after_stat(count), 
                         "\n(",
                         round(after_stat(prop)*100,1), "%)")
            ),
            nudge_x = 0.4,
  )+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 30))+
  geom_text(
    stat="flow", 
    aes(color = Period == "At\nrandomisation",
        label=after_stat(count)),
    nudge_x=0.3,
    show.legend = F
  )+
  scale_color_manual(values=c("00000000", "White"))+
  scale_y_continuous(expand=expansion(c(0,0.2)))->plot_binary

plot_binary

ggsave("Outputs/Figures/changes_after_rand_binary_participants_on_at_least_one_drug.png",
       width = 60,
       height=30,
       dpi="retina",
       units="cm")

#### FIGURE (SLIDES) ------

alluvial_plot_data_slides<-
  alluvial_plot_data%>%
  mutate(Period=if_else(Period=="At\nrandomisation", "Baseline\n(CRF only)", "Follow-up\n(PIS only)"))%>%
  mutate(Period=factor(Period, c("Baseline\n(CRF only)","Follow-up\n(PIS only)")))

alluvial_plot_data_slides%>%
  ggplot(aes(x=Period, 
             stratum=Event,
             alluvium=participant_id,
             fill=Event,
             label=Event
  ))+
  geom_flow(stat="alluvium", 
            # color="darkgray"
  )+
  geom_stratum(na.rm=T)+
  geom_text(
    data=.%>%filter(Period=="Baseline\n(CRF only)",
                    Event=="Yes"),
    stat="stratum",
    aes(label=
          paste0(after_stat(count),
                 "\n(",
                 round(after_stat(count)/lens_participants_number*100,1), "%)")
    ),
    nudge_x = -0.4,
    nudge_y=250,
    color="white"
  )+
  geom_text(data=.%>%filter(Period!="Baseline\n(CRF only)",
                            Event=="Yes"),
            stat="stratum",
            aes(label=
                  paste0(after_stat(count), 
                         "\n(",
                         round(after_stat(count)/lens_participants_number*100,1), "%)")
            ),
            nudge_x = 0.4,
            nudge_y=250,
            color="white"
  )+
  facet_wrap(~labels, 
             scales="free_y",
             labeller = label_wrap_gen(width = 25))+
  geom_text_repel(
    stat="flow", 
    aes(color = Period == "Baseline\n(CRF only)",
        label=after_stat(count)),
    nudge_x=0.5,
    show.legend = F,
    segment.size=0.3,
    segment.color = 'transparent'
    
  )+
  scale_color_manual(values=c("00000000", "White"))+
  oxpop_blue_panel+
  scale_y_continuous(expand=expansion(c(0,0.2)))->plot_binary

plot_binary

ggsave("Outputs/Figures/changes_after_rand_binary_participants_on_at_least_one_drug.png",
       width = 60,
       height=30,
       dpi="retina",
       units="cm")




## over time (ABANDONED) ------

# compute monthly records in pis
pis_records_after_randomisation_with_time_frames<-
  pis_data%>%
  select(participant_id, bnf_code, disp_ym)%>%
  mutate(participant_id=as.character(participant_id))%>%
  left_join(lens_participants_list%>%  mutate(participant_id=as.character(participant_id)))%>%
  mutate(rand_month = paste0(substr(rand_date,1,8),"01"),
         disp_ym = as.Date(paste0(substr(disp_ym,1,4), "-", substr(disp_ym,5,6), "-01")))%>%
  select(-rand_date)%>%
  filter(disp_ym>rand_month)%>%
  left_join(codelists_bnf)%>%
  left_join(codelist_labels)%>%
  mutate(months_after_rand=interval(rand_month, disp_ym)%/%months(1))%>%
  filter(!is.na(labels))%>%
  distinct(participant_id, labels, months_after_rand)



# merge
changes_after_rand_with_time_frames<-
  crf_flags_at_randomisation%>%
  right_join(expand.grid(participant_id=crf_flags_at_randomisation$participant_id,
                        labels=crf_flags_at_randomisation$labels),
             by=c("participant_id", "labels"))%>%
  mutate(at_randomisation=replace_na(at_randomisation, "0"))%>%
  distinct(participant_id, labels, at_randomisation)

changes_after_rand_with_time_frames%>%
  filter(at_randomisation=="1")%>%
  mutate(months_after_rand=0)%>%
  select(-at_randomisation)%>%
  rbind(pis_records_after_randomisation_with_time_frames)->drugs_timeframes

changes_after_rand_with_time_frames%>%
  mutate(participant_id=as.character(participant_id))%>%
  left_join(drugs_timeframes)%>%
  arrange(participant_id, labels, at_randomisation, months_after_rand)->flags_and_timeframes


# plot
# flags_and_timeframes%>%
#   rename(`Months after randomisation` = months_after_rand)%>%
#   ggplot(aes(x=`Months after randomisation`, 
#              stratum=Event,
#              alluvium=participant_id,
#              fill=Event,
#              label=Event))+
#   geom_flow(stat="alluvium", color="darkgray")+
#   geom_stratum(na.rm=T)+
#   facet_wrap(~labels, scales="free_y")->plot_timeframes
# 
# plot_timeframes


flags_and_timeframes%>%
  group_by(participant_id, labels)%>%
  mutate(after_randomisation=if_else(!is.na(months_after_rand), 1,0))%>%
  mutate(group=case_when(at_randomisation==1 & after_randomisation==1 ~ "before_after",
                         at_randomisation==0 & after_randomisation==1 ~ "after_only",
                         at_randomisation==1 & after_randomisation==0 ~ "before_only",
                         at_randomisation==0 & after_randomisation==0 ~ "none",
  ))%>%
  group_by(labels, months_after_rand, group)%>%
  summarise(n=n_distinct(participant_id))%>%
  mutate(months_after_rand=as.numeric(months_after_rand))%>%View()
  ggplot(aes(months_after_rand, n, group=group, color=group))+
  geom_point()+
  geom_line()+
  facet_wrap(~labels, scales = "free_y")->plot_timeframes

plot_timeframes
