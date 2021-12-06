Mammals-v2.0
================

\#Modelling workflow for Mammal SDMs

## Files required in input folder

``` r
list.files("data input")
```

    ##  [1] "adhoc_records_rm.csv"  "IUCN range - bats"     "IUCN shapefiles"      
    ##  [4] "IUCN shapefiles - all" "QDS files"             "Queries"              
    ##  [7] "record filter files"   "RSA_fixed.cpg"         "RSA_fixed.dbf"        
    ## [10] "RSA_fixed.prj"         "RSA_fixed.sbn"         "RSA_fixed.sbx"        
    ## [13] "RSA_fixed.shp"         "RSA_fixed.shp.xml"     "RSA_fixed.shx"        
    ## [16] "zagrid_aea_sf.rds"

``` r
list.files("data input/Queries")
```

    ##  [1] "SDM_query_Amblysomus corriae.xlsx"                    
    ##  [2] "SDM_query_Amblysomus septentrionalis.xlsx"            
    ##  [3] "SDM_query_Aonyx capensis.xlsx"                        
    ##  [4] "SDM_query_Atelerix frontalis.xlsx"                    
    ##  [5] "SDM_query_Calcochloris obtusirostris.xlsx"            
    ##  [6] "SDM_query_Cephalophus natalensis.xlsx"                
    ##  [7] "SDM_query_Cercopithecus albogularis erythrarchus.xlsx"
    ##  [8] "SDM_query_Crocuta crocuta.xlsx"                       
    ##  [9] "SDM_query_Dasymys incomtus.xlsx"                      
    ## [10] "SDM_query_Graphiurus ocularis.xlsx"                   
    ## [11] "SDM_query_Leptailurus serval.xlsx"                    
    ## [12] "SDM_query_Otomys auratus.xlsx"                        
    ## [13] "SDM_query_Parahyaena brunnea.xlsx"                    
    ## [14] "SDM_query_Paraxerus palliatus.xlsx"                   
    ## [15] "SDM_query_Pelea capreolus.xlsx"                       
    ## [16] "SDM_query_Petrodromus tetradactylus.xlsx"             
    ## [17] "SDM_query_Poecilogale albinucha.xlsx"                 
    ## [18] "SDM_query_TEMPLATE.xlsx"                              
    ## [19] "SDM_Species_Environmental_Variables.xlsx"

## Sub-folders required in the data output folder

``` r
list.files("data output")
```

    ## [1] "sdm BART results"    "sdm data processing" "temp_bck_dir"
