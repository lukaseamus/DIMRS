# Small no-take areas benefit hard corals more than regulation through fishing permits
This repository contains data and R code accompanying article 10.1002/aqc.3814 in *Aquatic Conservation: Marine and Freshwater Ecosystems*.

The repository contains a single dataframe (`DIMRS.csv`) and a corresponding R script (`DIMRS.R`). The former is arranged in tidy format, meaning columns are variables and rows are replicates, and has the variables
- *lat* = categorical latitude with levels "North" and "South"
- *lon* = categorical longitude with levels "East" and "West"
- *site* = site name ("Manta", "Tabon", "Moray" and "Bamboo")
- *depth* = categorical depth with levels "Shallow" and "Deep"
- *HC* = hard (scleractinian) coral (%) 
- *NIA* = nutrient indicator (turf) algae (%)
- *RC* = rock (%)
- *RB* = rubble (%)
- *SD* = sand (%)
- *SI* = silt (%)
- *SC* = soft coral (%)
- *SP* = sponge (%)
- *OT* = other, e.g. anemone, giant clam etc. (%)
- *TR* = trash, e.g. plastic litter (%)
- *LC* = live coral (% of hard coral)
- *DC* = dead coral (% of hard coral)
- *RKC* = recenly killed coral (% of hard coral)
- *DIS* = diseased coral (% of hard coral)
- *FBL* = fully bleached coral (% of hard coral)
- *PBL* = partially bleached coral (% of hard coral)
- *M* = massive growth morphology (% of hard coral)
- *SM* = submassive growth morphology (% of hard coral)
- *C* = corymbose growth morphology (% of hard coral)
- *B* = branching growth morphology (% of hard coral)
- *D* = digitate growth morphology (% of hard coral)
- *L* = laminar growth morphology (% of hard coral)
- *F* = foliose growth morphology (% of hard coral)
- *S* = solitary growth morphology (% of hard coral)
- *E* = encrusting growth morphology (% of hard coral)
- *T* = tabular growth morphology (% of hard coral)

Luka Seamus Wright, 10 March 2022
