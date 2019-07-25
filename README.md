# Temporal paper

Repo of R scripts elaborated for the paper: "The impact of temporal resolution on public transport accessibility measurement" (2019).

___

## Intro 

The repo of scirpts used for the paper *The impact of temporal resolution on public transport accessibility measurement: review and case study in Poland*, accepted for publication in the *Journal of Transport Geography* on 11th January 2019 (submitted: 18th July 2018).

Stepniak, M., Pritchard, J.P., Geurs K.T., Goliszek S., 2019, *The impact of temporal resolution on public transport accessibility measurement: review and case study in Poland*, Journal of Transport Geography, doi: https://doi.org/10.1016/j.jtrangeo.2019.01.007.

All the **data** used for the study can be downloaded from Open Data Repository [RepOD](https://repod.pon.edu.pl/). Direct link and reference of the dataset:

Stepniak, M., Goliszek, S., Pritchard, J., Geurs, K., 2019. The Impact of Temporal Resolution on Public Transport Accessibility Measurement. [Dataset] RepOD. https://doi.org/10.18150/repod.7727991.

**Authors:** 

+ Marcin Stępniak (tGIS, Department of Geography, Complutense University of Madrid, Spain)
+ Sławomir Goliszek (Institute of Geography and Spatial Organization, Polish Academy of Sciences)
+ John P. Pritchard (Centre for Transport Studies, University of Twente)
+ Karst T. Geurs (Centre for Transport Studies, University of Twente)

___

## List of Rscripts 
[R01. Compare precision of accessibility measurement](#r01-compare-precision-of-accessibility-measurement)<br>
[R02 Travel time calculations](#r02-travel-time-calculations)<br>
[R03 Compare travel times](#r03-compare-travel-times)<br>
[R04 Compare accessibility measures](#r04-compare-accessibility-measures)<br>
[R05 Combine Gini coefficients](#r05-combine-gini-coefficients)<br>
[R06 Frequency graph](#r06-frequency-graph)<br>

[**Licence**](#licence)<br>

___

## Input

The input for the repo are origin-destination (OD) travel time matrices which uses census track centroids as **origins**. All ODs are stored in two subfolders in `Data.zip`.  
**Destinations** of ODs are:

+ Subfolder `f03_Aggregates`

    For proximity measure:
    
    1) `Adm`: Local administration office (1 point)
    2) `Zlob`: Nurseries (30 points)
    
    For cumulative opportunities measure:
    
    3) `Teatr`: Theatres (21 points)
    4) `SpecHC`: Specialized health centres (169 points)
    
    + Subfolder `f03_Aggregates_Ai`
    
    For potential accessibility measure:
    
    5) `HOS`: Hospitals with attached number of beds (9 points)
    6) `Edu_Lo`: Secondary schools with attached number of classes (68 points)
    7) `OBWOD`: census track centroids with number of inhabitants (1745 points)

For the details please consult the file `Data_description.pdf`.

## Sampling procedures

a) **Systematic Sampling** departure time selected using a regular interval
b) **Simple Random Sampling**  a specified number of sample times are selected at random (without replacement)
c) **Hybrid Sampling** departure times are randomly selected from given time intervals (resulted from applied temporal resolution)
d) **Constrained Random Walk Sampling** 1st departure time is randomly selected within the first time interval, and next ones from subsequent time intervals defined by a temporal resolution (+1 resolution +/- 0.5 temporal resolution).

For details please consult [Owen & Murphy (2018)](https://trid.trb.org/view/1497217). 

The detailed description of the script which enables to generate departure time can be found in this [repo](https://github.com/stmarcin/Sampling_Departure_Time).


## Tested temporal resolutions

The table below shows applied temporal resolutions and number of iterations required for 1-hour long time window:

resolution | interations
------------ | -------------
2 | 30
3 | 20
4 | 15
5 | 12
6 | 10
10 | 6
12 | 5
15 | 4
20 | 3
30 | 2
60 | 1


## Workflow

The repo consists of serveral of subsequent scripts:

#### **R01 Compare precision of accessibility measurement**

The `R01_Ai_Calculations.R` script applies different functions, depending on which of accessibility measures is in use. The code select departure time according to a given sampling method for all considered temporal resolutions. Then it calculates accessibility measures and calculate (aggregated) errors:

+ MAPE (*Mean Absolute Percentage Error*), expressed in %, calculated according to the formula: `mean(abs((y - x)/y))*100`;
+ MAE (*Mean Absolute Error*), expressed in absolute values (e.g. minutes) calculated according to the formula: `mean(abs(y-x))`;
+ maxdif (*maximum difference*), expressed in absolute values (e.g. minutes) calculated according to the formula: `max(abs(y-x))`;

    where x is an evaluated value, while y - a benchmark one.

Additionally each of the scripts calculate Gini coefficient for evaluated as well as benchmark values.

Particular functions (seperate for each of the applied accessibility measures) are stored in separate Rscripts:

1) **proximity** (or travel-time-to-nearest-provider) for public administration `Adm` and nurseries `Zlob`. Function stored in `R011_Ai_proximity.R` 
Function syntax:

    ```R011_Ai_proximity(file_all, mc_max)```

    Application of temporal resolution: This script aggregates selected travel times using an arithmetic mean.
    </br>
    </br>

2) **cumulative opportunities** (or isochrones) for accessibility to specilized health care `SpecHC` and theatres `Teatr`. Function syntax:

    ```Ai_cumulative(file_all, threshold, mc_max)```

    Application of temporal resolution: This script aggregates calculated accessibility measures using an arithmetic mean.
    </br>
    </br>


3) **potential accessibility** for accessibility to education (secondary schools, `Edu`), hospitals `HOS` and population `OBWOD`. The funciton uses negative exponential function: `(mass*(exp(-beta*TravelTime)`. 

    Application of temporal resolution: This script aggregates selected travel times using harmonic-based means (for details please consult [Stępniak & Jacobs-Crisioni (2017)](http://www.sciencedirect.com/science/article/pii/S0966692316305385)). 
    
    Function syntax:
    
    ```Ai_potential(file_mass, file_all, beta, mc_max)```

    where:

    * `mc_max` number of iterations in case of simple *random*, *hybrid*, and *constrained random walk* sampling methods; 
    * `file_all` defines a list of a given types of OD matrixes (for different types of destinations; e.g. *HOS*)
    * `file_mass` (relative or absolute) path to the file where the quantitative value of attractiveness of destinations is stored. The file should be two-column, in the first there should be ID (to be used while merging with destinations ID) and the second - value of attractiveness of a given destination, e.g.:
        + number of beds in a hospital (*"Results/t00_data/HOS.csv"*)
        + number of classes in a school (*"Results/t00_data/EduLO.csv"*)
        + number of population in a census track (*"Results/t00_data/POP.csv"*)
    * `beta` the value of *beta* parameter (applied value: *0.023105*)  
    </br>

    Each of sampling procedures is repeated an user-defined number of times (in case of the paper = 100).

##### Input

Set of origin-destination matrixes stored in two folders: 

+ `f03_Aggregates` for measures without distance decay (proximity and cumulative opportunities measures)
+ `f03_Aggregates_Ai` for measures with distance decay (potential accessibility measure)

##### Output

Set of `csv` files stored in two folders (names of files depends on the type of destination):

+ `t06_Results` files with aggregated values of:
    + MAPE, MAE, maximum difference 
    + values of Gini (stored in subfolder `Gini`)

+ `t04_Temporary` files with disaggrated values (calculated separately for each of randomly selected scenarios):
    + MAPE (data used for **Table 4**)
    + MAE
    + maxdiff
    + Gini

Additionally, in the `t04_Temporary` the script saves values of accessibility measures calculated for a systematic approach (one file for each of destination and time-window period; for the details please consult `Data_description.pdf` file).

#### **R02 Travel time calculations**

`R02_TravelTime.R` compares precision of travel times' estimation using MAPE, MAE and maximum difference (maxdiff). The script uses all 4 sampling methods.


##### Input

Set of OD matrixes stored in `f03_Aggregates_Ai` folder (census track centroids as origin & departure points)

##### Output 

`TravTime.csv` file stored in `Results/t05_TTResults` folder which contains MAPE, MAE and maxdiff (maximum difference) indicators (MAPE values used for **Table 3**)

#### **R03 Compare travel times**

`R03_Comparison_TravelTime.R` prepares graphs which present the loose of precision of travel time estimation due to reduced temporal resolution.

##### Input

`TravTime.csv` file (stored in `t05_TTResults` folder) which contains MAPE, MAE & maxdiff of travel times aggregated for different temporal resolutions and obtained using different sampling methods.

##### Output

+ `TravelTime_sampling.png` figure which compares the quality of different sampling methods. (**Figure 3**)
+ `TravelTime_estimation.png` figure which presents the loose of precision (MAPE & MAE) of hybrid model in different 1-hour-long scenarios and their average. (**Figure 5**)

#### **R04 Compare accessibility measures**

`R03_Comparison_TravelTime.R` prepares graphs which present the loose of precision of travel time estimation due to reduced temporal resolution.

##### Input

Set of files (one per each destination) with MAPEs vales stored in `t06_Results` folder.

##### Output

+ `Ai_Sampling.png` compares the quality of different sampling methods (**Figure 4**).
+ `Ai_Hybrid.png` compares all measures calculated for particular destinations, using MAPE of hybrid model in different 1-hour-long scenarios (not used in the paper).
+ `Ai_Hybrid_complex.png` the same as above but added a zoom-in with excluded curves for cumulative opportunities (**Figure 6**).
+ `Ai_H_scenarios.png` presents MAPE of hybrid model in different 1-hour-long scenarios, for different types of measures and destinations (**Figure 7** in the annex)


#### **R05 Combine Gini coefficients**

`R05_Gini_Ai.R` combines Gini coefficients stored in separate files (one for each destination and time-window) and save an output to xlsx file.

##### Input

Set of files (one per each destination and time window) with Gini coefficients stored in `t06_Results` folder.

##### Output

+ `Gini_table.xlsx` stored in `t07_Graphs` folder (data used for **Table 5**)


#### **R06 Frequency graph**    

Simple script used to draw a graph which presents the total number of vehicles in 1-hour-long periods of time (Figure 2 in the paper).

##### Input

Selected GTFS files: `stop_times.txt`, `trips.txt`, `calendar.txt` and `routes.txt` stored in `Results/p00_data/GTFS_Szczecin` folder.

##### Output 

Graph `Plot_Freq.png` stored in `Results/p07_Graphs/` folder (**Figure 2**)

___

### Licence

License for scripts: CC-BY-4.0

