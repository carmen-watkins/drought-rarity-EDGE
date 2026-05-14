This repository contains the code for all models and figures associated with the paper 'Drought conditions reorder plant communities through expansion of spatially sparse and temporally intermittent species', Ecology, 2026.

Authors: Carmen R. E. Watkins, Beatriz A. Aguirre, Y. Anny Chung, Lukas P. Bell-Dereske, Lauren M. Hallett, David L. Hoover, Laureano A. Gherardi, Joan C. Dudney, Megan E. Wilcots, Jennifer A.  Rudgers, Scott L. Collins, Melinda D. Smith, Forest Isbell, Tadashi Fukami, Hanan Farah, Cristina Portales-Reyes

## data
all data needed to run the code is stored in the repository. Data files for the Sevilleta EDGE sites are publicly available at the EDI data portal (Baur et al., 2024) and data for Northern EDGE sites will
be archived at the EDI data portal upon publication. 

## manuscript figures
main figures can be generated from the following scripts in the analyses folder: 
    ## Fig1_site_characteristics.R
    ## Fig2_RR_analyses.R
    ## Fig3_rarity_categories.R
    ## Fig4_slope_predictor.R
    ## each script sources data cleaning scripts located in the data-prep folder

## models
models can be run in the following scripts in the analyses folder: 
    ## Q1_mixed_models.R                ## mixed effects models, equation 4
    ## Q2_pt1_linear_models_by_site.R   ## individual site level models of rarity effects on drought responses
    ## Q2_pt2_slope_predictor_models.R  ## site level predictors (MAP, MAT, etc) of the slope of rarity-drought relationships

## supplementary analyses
files associated with supplementary analyses can be found in the analyses > supplementary_analyses folder. 
