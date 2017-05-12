# PED: Read me and metadata
Data and code for data analysis presented in Block and Meave (2017) Plant Ecology &amp; Diversity.
For a full description of our methods and interpretation of our results, please consult our paper at http://dx.doi.org/10.1080/17550874.2017.1330367.

## Code 

The R markdown file `Data analysis.Rmd` is a commented script of the main analysis presented in the paper. 

The file `Analysis without CH.Rmd` is exactly the same analysis but excluding data from the Chichinautzin lava field, 
the youngest geomorphological unit in our study and the one with (by far) the highest values of exposed rock. We analysed 
our data excluding this unit to evaluate to what extent our conclusions depended on the inclusion of the extreme data points 
from this unit.

The file `Indicator species analysis.R` is an R script of the Indicator Species analysis we performed. 

## Data

The following files contain the data used in the paper. 

* `env.csv` contains the environmental variables measured at each of our 60 plots (10 in each of 6 geomorphological units). 
**altitude**: elevation above sea level in meters. **slope**: slope of the plot in degrees. **rock**: percentage of exposed rock in the plot.
**disturbance**: Human chronic disturbance index (see paper for details on its calculation). **unit**: Geomorphological unit to which the 
plot belongs (CH = Chichinautzin; OC = Oclayuca; SU = Suchiooc; LO = Lower Otates; UO = Upper Otates; TE = Tepozteco Mountains).

* `aspect.csv` is a vector containing values of the aspect (in degrees) of each plot. Plots are in the same order as in `env.csv`.

* `str.csv` contains vegetation structure variables measured in each plot (each row corresponds to one plot). **ba**: basal area (squared centimeters).
**cov**: Crown cover (squared meters). **den**: Stem density. **hei**: Mean height of the three tallest trees in the plot (meters).

* `can.csv`: Abundance of canopy species (columns) in each plot (rows). 

* `und.csv`: Abundance estimates of understorey species (columns) in each plot (rows).

* `PA_total`: Presence/absence data of all species in each plot. 
