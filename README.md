# nnl

nnl function allows to extrapolate SpatialLinesDataFrame A to a SpatialLinesDataFrame B by find nearest neighbors lines. It create points along to SpatialLinesDataFrames and it find nearest points. Then, statistics are use to select nearest lines.

# Install

Install package with **devtools** :

    devtools::install_github("naub1n/nnl")

# How to use

## Required arguments
Just use `nnl::nnl()`function.
**l_A** is the original SpatialLinesDataFrame
**l_B** is a SpatialLinesDataFrame where you want to extrapolate **l_A**
**id_l_A** an **id_l_B** are columns names contain unique values (IDs)

## Optionals arguments
in progress

# Examples
in progress

# Performance
With **shp_me**, [DCE river](http://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/metadata/3c126d49-0f17-457b-ae74-918808371268) on [Seine-Normandie](https://fr.wikipedia.org/wiki/Bassin_Seine-Normandie).

    Large SpatialLinesDataFrame (1651 elements, 10.3 Mb)

With **shp_topo**, [BD topo V2](http://professionnels.ign.fr/bdtopo-hydrographie) on Seine-Normandie

    Large SpatialLinesDataFrame (295908 elements, 626.1 Mb)

Start **nnl()** function:

    system.time(test <- nnl::nnl(l_A = shp_me, l_B = shp_topo, id_l_A = "CdMasseDEa", id_l_B = "ID",verbose = T))
Function responses :

    CRS Comparaison
    Projection test
    Filter l_B
    Create points to l_A
    5163216 points created
    Create points to l_B
    8210254 points created
    Find nearest points
    8210254 points attached
    Select nearest lines : Step 1
    55643 lines found
    Select nearest lines : Step 2
    2345 lines found
    Aggregate result
    Select nearest l_B
    Add IDs
    Compare length result with l_A length
    End
Time result :

    utilisateur     système      écoulé 
         122.21        7.05      256.55 
**4 min 16 sec***

> \* time with [i7-7500U](https://ark.intel.com/content/www/fr/fr/ark/products/95451/intel-core-i7-7500u-processor-4m-cache-up-to-3-50-ghz.html) CPU (2C4T) and 16Go RAM


