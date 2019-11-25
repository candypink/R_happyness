Challenge the “Happiness” Score
================
Chiara
13/10/2017

In this document I will look at the happiness score and try to
understand how much it really reflects the level of “happiness” in a
country.

# Quick Look at Happiness Data

In this section we will take a quick look at the happiness data. In
particular, we will see how to read the data, what the data tells us
about how happiness is spread around the World and how it is changing
over time.

The code below shows the first steps of the data analysis, that do not
produce any graphical output but are necessary to setup the analysis.
The starting point is the Kaggle dataset on [World Happiness
Report](https://www.kaggle.com/unsdsn/world-happiness).

``` r
# setup
knitr::opts_chunk$set(message=FALSE,results='hide', fig.height=3, fig.width=5)
library(ggplot2) # to make pretty plots
library(dplyr) # for inner_join and left_join
library(car) # to recode
library(maps) # for the maps
library(gridExtra) # to arrange the plots in grids
library(plotly) # for interactive plots
library(cowplot) # to have get_legent
#read the datasets and take a look
happy_2015 <- read.csv("../input/world-happiness/2015.csv")
happy_2016 <- read.csv("../input/world-happiness/2016.csv")
happy_2017 <- read.csv("../input/world-happiness/2017.csv")
str(happy_2015)
str(happy_2016)
str(happy_2017)
# Based on the structure, rename columns to prepare for merging
colnames(happy_2015) <- c("Country","Region","Rank_2015","Score_2015","Err_2015","GDP_2015","Family_2015","Health_2015","Freedom_2015","Gov_2015","Generosity_2015","Dist_res_2015")
colnames(happy_2016) <- c("Country","Region_2016","Rank_2016","Score_2016","LCI_2016","UCI_2016","GDP_2016","Family_2016","Health_2016","Freedom_2016","Gov_2016","Generosity_2016","Dist_res_2016")
colnames(happy_2017) <- c("Country","Rank_2017","Score_2017","Whisker.high_2016","Whisker.low_2017","GDP_2017","Family_2017","Health_2017","Freedom_2017","Generosity_2017","Gov_2017","Dist_res_2017")
# And finally merge the datasets into one
happy <- merge(happy_2015, happy_2016, by.x = "Country", by.y = "Country")
happy <- merge(happy, happy_2017, by.x = "Country", by.y = "Country")
happy$Region_2016 <- NULL
str(happy) # when running for the first time, check the structure of the merged dataset
# rename some countries 
happy$Country <- recode(happy$Country, 
                           "'Congo (Brazzaville)' = 'Democratic Republic of the Congo';
                            'Congo (Kinshasa)'='Republic of Congo';
                            'United States'='USA';
                             'United Kingdom'= 'UK'
                           ")
```

## Happiness Score Around the World

Plot of the happiness score in the World in 2017

``` r
w <- map_data("world")
colnames(w)[5]<- "Country"
myw <- inner_join(w, happy, by = "Country")
worldplot <- ggplot(data = w, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = "gray") +
    geom_polygon(data=myw, aes(x = long, y = lat, group = group, fill = Score_2017),color = "white") +
    geom_polygon(color = "black", fill = NA) +
    theme_bw() +
    ggtitle("Happiness Score in 2017 in the World") +
    scale_fill_distiller(palette = "Spectral")
worldplot
```

![](are-happy-people-really-happy_files/figure-gfm/world-1.png)<!-- -->

## Variations in Happiness Score

Compute the variation in percentage between 2017 and
2015.

``` r
happy$var_2016_2015 = 100*(happy$Score_2016 - happy$Score_2015)/happy$Score_2015
happy$var_2017_2016 = 100*(happy$Score_2017 - happy$Score_2016)/happy$Score_2016
happy$var_2017_2015 = 100*(happy$Score_2017 - happy$Score_2015)/happy$Score_2015
```

Is there a correlation between the fact that the happiness score changes
and the value of the happiness score
itself?

<!--html_preserve-->

<div id="htmlwidget-93335428dbe13056aca6" class="plotly html-widget" style="width:672px;height:288px;">

</div>

<script type="application/json" data-for="htmlwidget-93335428dbe13056aca6">{"x":{"data":[{"x":[6.12587161831076,-6.3520860373914,4.76360780441997,-5.90131226645302,0.380285626891088,0.597699483235644,-1.09978912303327e-06,-2.69444386164348,0.422106791896584,2.13087069108188,-1.83213034436002,-4.19749239113245,-0.663113824328239,9.49101990568414,-4.60688832281383,-1.1375219995988,4.70802505995736,-13.0655578849277,-4.9835281557806,11.7591328844746,12.4059120393044,-9.84861608980291e-07,9.13852373009792,10.4186305658838,-1.49454712883788,7.33569971963104,-0.269865810960419,2.58754403674651,-1.85271153134183,7.57081698104237,-5.24684060641246,-2.03432056206338,-8.09167874192674,-1.19529244015064,1.59877334348424,-0.0664297068151972,7.06243641910932,0.552299531433818,12.8993832502241,-2.07178168740195,3.35236804264431,-1.15248142404764,0.850659771413726,-2.02281502263179,14.6047267091348,-0.255997790717693,2.97778094256371,-11.0727415161001,7.61787598334835,5.40584674481953,-4.07549317608261,-20.2523256836343,8.20802491011966,10.9166642030079,-0.753866063745938,-5.47645000612356,-2.53750539130691,0.128039100767398,-3.84861626175454,0.533144618317423,-0.89310496414907,0.269001766428029,14.3638803923015,-6.97145086179207,-1.11909009029648,2.77349702749846,-0.614863069112384,3.03235965822857,-5.54661289228055,-3.01826816404305,-5.33484322890333,14.7508808284145,7.97685275124137,-22.7083790774314,-3.97983845381595,1.18292388837545,-1.19493354338237,3.35530638575714,-1.00516019000924,-7.50232924952866,5.44194299178891,4.88110280902277,3.57029435748874,-3.24617184638119,2.77524533357987,-8.47363199297969,-0.866024440325521,1.66187779454351,0.866717807317215,4.42848859992819,5.52588986055141,9.92467641091006,-0.0135557733599859,0.384300435076998,4.16952812598026,4.75942465356801,-3.68261424175436,2.01931418401299,0.199417432740886,1.44397484416172,1.2725364871141,-4.92189594541998,-6.5498463675131,-1.87156331408156,7.0372526776783,3.14280866156207,1.82281794710661,-3.56980789593102,13.6807144665328,4.32120050226085,0.173158907098997,-1.04508091294805,16.1629059275643,5.30938865755436,4.48191947251431,-3.32450681519741,1.71809924255313,-1.53898944933019,4.0284359367794,-2.43984256836179,1.16921911723322,3.95692009413393,-9.03296837439911,-1.0863671932189,-1.22578149415671,15.1696571253653,5.32803780555329,-11.4255494019619,-0.480251179288156,23.1067236899993,3.58710180740401e-06,1.3926952593083,3.15078769692423,4.93871713595674,3.81581916120097,-12.497325474267,-3.66613876990624,-2.22804390465794,-1.76991107574687,-0.478026273164229,-0.533063559061144,-22.9074889867841,-5.33582310178396,-11.8714757137564,-11.9906426405763,-15.943600867679],"y":[8,2,5,9,4,2,1,10,2,5,8,2,10,9,8,4,2,9,4,2,9,9,7,9,6,9,4,3,4,9,9,4,2,10,2,10,4,4,5,4,2,9,10,10,9,2,10,9,10,4,9,4,4,2,10,8,7,5,5,10,5,10,9,4,3,5,2,9,2,5,2,2,5,9,5,2,10,2,9,9,7,9,10,9,9,4,2,3,2,5,7,8,10,1,4,9,9,10,10,8,5,4,4,4,7,2,10,5,2,2,9,5,9,2,9,7,2,2,9,3,10,8,9,10,10,5,2,9,7,9,4,5,5,2,9,2,5,10,6,4,2,4,7,5,9,9],"text":["Score_2017: 3.794<br />var_2017_2015:  6.125872e+00<br />Region: Southern Asia<br />country: Afghanistan","Score_2017: 4.644<br />var_2017_2015: -6.352086e+00<br />Region: Central and Eastern Europe<br />country: Albania","Score_2017: 5.872<br />var_2017_2015:  4.763608e+00<br />Region: Middle East and Northern Africa<br />country: Algeria","Score_2017: 3.795<br />var_2017_2015: -5.901312e+00<br />Region: Sub-Saharan Africa<br />country: Angola","Score_2017: 6.599<br />var_2017_2015:  3.802856e-01<br />Region: Latin America and Caribbean<br />country: Argentina","Score_2017: 4.376<br />var_2017_2015:  5.976995e-01<br />Region: Central and Eastern Europe<br />country: Armenia","Score_2017: 7.284<br />var_2017_2015: -1.099789e-06<br />Region: Australia and New Zealand<br />country: Australia","Score_2017: 7.006<br />var_2017_2015: -2.694444e+00<br />Region: Western Europe<br />country: Austria","Score_2017: 5.234<br />var_2017_2015:  4.221068e-01<br />Region: Central and Eastern Europe<br />country: Azerbaijan","Score_2017: 6.087<br />var_2017_2015:  2.130871e+00<br />Region: Middle East and Northern Africa<br />country: Bahrain","Score_2017: 4.608<br />var_2017_2015: -1.832130e+00<br />Region: Southern Asia<br />country: Bangladesh","Score_2017: 5.569<br />var_2017_2015: -4.197492e+00<br />Region: Central and Eastern Europe<br />country: Belarus","Score_2017: 6.891<br />var_2017_2015: -6.631138e-01<br />Region: Western Europe<br />country: Belgium","Score_2017: 3.657<br />var_2017_2015:  9.491020e+00<br />Region: Sub-Saharan Africa<br />country: Benin","Score_2017: 5.011<br />var_2017_2015: -4.606888e+00<br />Region: Southern Asia<br />country: Bhutan","Score_2017: 5.823<br />var_2017_2015: -1.137522e+00<br />Region: Latin America and Caribbean<br />country: Bolivia","Score_2017: 5.182<br />var_2017_2015:  4.708025e+00<br />Region: Central and Eastern Europe<br />country: Bosnia and Herzegovina","Score_2017: 3.766<br />var_2017_2015: -1.306556e+01<br />Region: Sub-Saharan Africa<br />country: Botswana","Score_2017: 6.635<br />var_2017_2015: -4.983528e+00<br />Region: Latin America and Caribbean<br />country: Brazil","Score_2017: 4.714<br />var_2017_2015:  1.175913e+01<br />Region: Central and Eastern Europe<br />country: Bulgaria","Score_2017: 4.032<br />var_2017_2015:  1.240591e+01<br />Region: Sub-Saharan Africa<br />country: Burkina Faso","Score_2017: 2.905<br />var_2017_2015: -9.848616e-07<br />Region: Sub-Saharan Africa<br />country: Burundi","Score_2017: 4.168<br />var_2017_2015:  9.138524e+00<br />Region: Southeastern Asia<br />country: Cambodia","Score_2017: 4.695<br />var_2017_2015:  1.041863e+01<br />Region: Sub-Saharan Africa<br />country: Cameroon","Score_2017: 7.316<br />var_2017_2015: -1.494547e+00<br />Region: North America<br />country: Canada","Score_2017: 3.936<br />var_2017_2015:  7.335700e+00<br />Region: Sub-Saharan Africa<br />country: Chad","Score_2017: 6.652<br />var_2017_2015: -2.698658e-01<br />Region: Latin America and Caribbean<br />country: Chile","Score_2017: 5.273<br />var_2017_2015:  2.587544e+00<br />Region: Eastern Asia<br />country: China","Score_2017: 6.357<br />var_2017_2015: -1.852712e+00<br />Region: Latin America and Caribbean<br />country: Colombia","Score_2017: 4.291<br />var_2017_2015:  7.570817e+00<br />Region: Sub-Saharan Africa<br />country: Democratic Republic of the Congo","Score_2017: 4.280<br />var_2017_2015: -5.246841e+00<br />Region: Sub-Saharan Africa<br />country: Republic of Congo","Score_2017: 7.079<br />var_2017_2015: -2.034321e+00<br />Region: Latin America and Caribbean<br />country: Costa Rica","Score_2017: 5.293<br />var_2017_2015: -8.091679e+00<br />Region: Central and Eastern Europe<br />country: Croatia","Score_2017: 5.621<br />var_2017_2015: -1.195292e+00<br />Region: Western Europe<br />country: Cyprus","Score_2017: 6.609<br />var_2017_2015:  1.598773e+00<br />Region: Central and Eastern Europe<br />country: Czech Republic","Score_2017: 7.522<br />var_2017_2015: -6.642971e-02<br />Region: Western Europe<br />country: Denmark","Score_2017: 5.230<br />var_2017_2015:  7.062436e+00<br />Region: Latin America and Caribbean<br />country: Dominican Republic","Score_2017: 6.008<br />var_2017_2015:  5.522995e-01<br />Region: Latin America and Caribbean<br />country: Ecuador","Score_2017: 4.735<br />var_2017_2015:  1.289938e+01<br />Region: Middle East and Northern Africa<br />country: Egypt","Score_2017: 6.003<br />var_2017_2015: -2.071782e+00<br />Region: Latin America and Caribbean<br />country: El Salvador","Score_2017: 5.611<br />var_2017_2015:  3.352368e+00<br />Region: Central and Eastern Europe<br />country: Estonia","Score_2017: 4.460<br />var_2017_2015: -1.152481e+00<br />Region: Sub-Saharan Africa<br />country: Ethiopia","Score_2017: 7.469<br />var_2017_2015:  8.506598e-01<br />Region: Western Europe<br />country: Finland","Score_2017: 6.442<br />var_2017_2015: -2.022815e+00<br />Region: Western Europe<br />country: France","Score_2017: 4.465<br />var_2017_2015:  1.460473e+01<br />Region: Sub-Saharan Africa<br />country: Gabon","Score_2017: 4.286<br />var_2017_2015: -2.559978e-01<br />Region: Central and Eastern Europe<br />country: Georgia","Score_2017: 6.951<br />var_2017_2015:  2.977781e+00<br />Region: Western Europe<br />country: Germany","Score_2017: 4.120<br />var_2017_2015: -1.107274e+01<br />Region: Sub-Saharan Africa<br />country: Ghana","Score_2017: 5.227<br />var_2017_2015:  7.617876e+00<br />Region: Western Europe<br />country: Greece","Score_2017: 6.454<br />var_2017_2015:  5.405847e+00<br />Region: Latin America and Caribbean<br />country: Guatemala","Score_2017: 3.507<br />var_2017_2015: -4.075493e+00<br />Region: Sub-Saharan Africa<br />country: Guinea","Score_2017: 3.603<br />var_2017_2015: -2.025233e+01<br />Region: Latin America and Caribbean<br />country: Haiti","Score_2017: 5.181<br />var_2017_2015:  8.208025e+00<br />Region: Latin America and Caribbean<br />country: Honduras","Score_2017: 5.324<br />var_2017_2015:  1.091666e+01<br />Region: Central and Eastern Europe<br />country: Hungary","Score_2017: 7.504<br />var_2017_2015: -7.538661e-01<br />Region: Western Europe<br />country: Iceland","Score_2017: 4.315<br />var_2017_2015: -5.476450e+00<br />Region: Southern Asia<br />country: India","Score_2017: 5.262<br />var_2017_2015: -2.537505e+00<br />Region: Southeastern Asia<br />country: Indonesia","Score_2017: 4.692<br />var_2017_2015:  1.280391e-01<br />Region: Middle East and Northern Africa<br />country: Iran","Score_2017: 4.497<br />var_2017_2015: -3.848616e+00<br />Region: Middle East and Northern Africa<br />country: Iraq","Score_2017: 6.977<br />var_2017_2015:  5.331446e-01<br />Region: Western Europe<br />country: Ireland","Score_2017: 7.213<br />var_2017_2015: -8.931050e-01<br />Region: Middle East and Northern Africa<br />country: Israel","Score_2017: 5.964<br />var_2017_2015:  2.690018e-01<br />Region: Western Europe<br />country: Italy","Score_2017: 4.180<br />var_2017_2015:  1.436388e+01<br />Region: Sub-Saharan Africa<br />country: Ivory Coast","Score_2017: 5.311<br />var_2017_2015: -6.971451e+00<br />Region: Latin America and Caribbean<br />country: Jamaica","Score_2017: 5.920<br />var_2017_2015: -1.119090e+00<br />Region: Eastern Asia<br />country: Japan","Score_2017: 5.336<br />var_2017_2015:  2.773497e+00<br />Region: Middle East and Northern Africa<br />country: Jordan","Score_2017: 5.819<br />var_2017_2015: -6.148631e-01<br />Region: Central and Eastern Europe<br />country: Kazakhstan","Score_2017: 4.553<br />var_2017_2015:  3.032360e+00<br />Region: Sub-Saharan Africa<br />country: Kenya","Score_2017: 5.279<br />var_2017_2015: -5.546613e+00<br />Region: Central and Eastern Europe<br />country: Kosovo","Score_2017: 6.105<br />var_2017_2015: -3.018268e+00<br />Region: Middle East and Northern Africa<br />country: Kuwait","Score_2017: 5.004<br />var_2017_2015: -5.334843e+00<br />Region: Central and Eastern Europe<br />country: Kyrgyzstan","Score_2017: 5.850<br />var_2017_2015:  1.475088e+01<br />Region: Central and Eastern Europe<br />country: Latvia","Score_2017: 5.225<br />var_2017_2015:  7.976853e+00<br />Region: Middle East and Northern Africa<br />country: Lebanon","Score_2017: 3.533<br />var_2017_2015: -2.270838e+01<br />Region: Sub-Saharan Africa<br />country: Liberia","Score_2017: 5.525<br />var_2017_2015: -3.979838e+00<br />Region: Middle East and Northern Africa<br />country: Libya","Score_2017: 5.902<br />var_2017_2015:  1.182924e+00<br />Region: Central and Eastern Europe<br />country: Lithuania","Score_2017: 6.863<br />var_2017_2015: -1.194934e+00<br />Region: Western Europe<br />country: Luxembourg","Score_2017: 5.175<br />var_2017_2015:  3.355306e+00<br />Region: Central and Eastern Europe<br />country: Macedonia","Score_2017: 3.644<br />var_2017_2015: -1.005160e+00<br />Region: Sub-Saharan Africa<br />country: Madagascar","Score_2017: 3.970<br />var_2017_2015: -7.502329e+00<br />Region: Sub-Saharan Africa<br />country: Malawi","Score_2017: 6.084<br />var_2017_2015:  5.441943e+00<br />Region: Southeastern Asia<br />country: Malaysia","Score_2017: 4.190<br />var_2017_2015:  4.881103e+00<br />Region: Sub-Saharan Africa<br />country: Mali","Score_2017: 6.527<br />var_2017_2015:  3.570294e+00<br />Region: Western Europe<br />country: Malta","Score_2017: 4.292<br />var_2017_2015: -3.246172e+00<br />Region: Sub-Saharan Africa<br />country: Mauritania","Score_2017: 5.629<br />var_2017_2015:  2.775245e+00<br />Region: Sub-Saharan Africa<br />country: Mauritius","Score_2017: 6.578<br />var_2017_2015: -8.473632e+00<br />Region: Latin America and Caribbean<br />country: Mexico","Score_2017: 5.838<br />var_2017_2015: -8.660244e-01<br />Region: Central and Eastern Europe<br />country: Moldova","Score_2017: 4.955<br />var_2017_2015:  1.661878e+00<br />Region: Eastern Asia<br />country: Mongolia","Score_2017: 5.237<br />var_2017_2015:  8.667178e-01<br />Region: Central and Eastern Europe<br />country: Montenegro","Score_2017: 5.235<br />var_2017_2015:  4.428489e+00<br />Region: Middle East and Northern Africa<br />country: Morocco","Score_2017: 4.545<br />var_2017_2015:  5.525890e+00<br />Region: Southeastern Asia<br />country: Myanmar","Score_2017: 4.962<br />var_2017_2015:  9.924676e+00<br />Region: Southern Asia<br />country: Nepal","Score_2017: 7.377<br />var_2017_2015: -1.355577e-02<br />Region: Western Europe<br />country: Netherlands","Score_2017: 7.314<br />var_2017_2015:  3.843004e-01<br />Region: Australia and New Zealand<br />country: New Zealand","Score_2017: 6.071<br />var_2017_2015:  4.169528e+00<br />Region: Latin America and Caribbean<br />country: Nicaragua","Score_2017: 4.028<br />var_2017_2015:  4.759425e+00<br />Region: Sub-Saharan Africa<br />country: Niger","Score_2017: 5.074<br />var_2017_2015: -3.682614e+00<br />Region: Sub-Saharan Africa<br />country: Nigeria","Score_2017: 5.810<br />var_2017_2015:  2.019314e+00<br />Region: Western Europe<br />country: North Cyprus","Score_2017: 7.537<br />var_2017_2015:  1.994174e-01<br />Region: Western Europe<br />country: Norway","Score_2017: 5.269<br />var_2017_2015:  1.443975e+00<br />Region: Southern Asia<br />country: Pakistan","Score_2017: 4.775<br />var_2017_2015:  1.272536e+00<br />Region: Middle East and Northern Africa<br />country: Palestinian Territories","Score_2017: 6.452<br />var_2017_2015: -4.921896e+00<br />Region: Latin America and Caribbean<br />country: Panama","Score_2017: 5.493<br />var_2017_2015: -6.549846e+00<br />Region: Latin America and Caribbean<br />country: Paraguay","Score_2017: 5.715<br />var_2017_2015: -1.871563e+00<br />Region: Latin America and Caribbean<br />country: Peru","Score_2017: 5.430<br />var_2017_2015:  7.037253e+00<br />Region: Southeastern Asia<br />country: Philippines","Score_2017: 5.973<br />var_2017_2015:  3.142809e+00<br />Region: Central and Eastern Europe<br />country: Poland","Score_2017: 5.195<br />var_2017_2015:  1.822818e+00<br />Region: Western Europe<br />country: Portugal","Score_2017: 6.375<br />var_2017_2015: -3.569808e+00<br />Region: Middle East and Northern Africa<br />country: Qatar","Score_2017: 5.825<br />var_2017_2015:  1.368071e+01<br />Region: Central and Eastern Europe<br />country: Romania","Score_2017: 5.963<br />var_2017_2015:  4.321201e+00<br />Region: Central and Eastern Europe<br />country: Russia","Score_2017: 3.471<br />var_2017_2015:  1.731589e-01<br />Region: Sub-Saharan Africa<br />country: Rwanda","Score_2017: 6.344<br />var_2017_2015: -1.045081e+00<br />Region: Middle East and Northern Africa<br />country: Saudi Arabia","Score_2017: 4.535<br />var_2017_2015:  1.616291e+01<br />Region: Sub-Saharan Africa<br />country: Senegal","Score_2017: 5.395<br />var_2017_2015:  5.309389e+00<br />Region: Central and Eastern Europe<br />country: Serbia","Score_2017: 4.709<br />var_2017_2015:  4.481919e+00<br />Region: Sub-Saharan Africa<br />country: Sierra Leone","Score_2017: 6.572<br />var_2017_2015: -3.324507e+00<br />Region: Southeastern Asia<br />country: Singapore","Score_2017: 6.098<br />var_2017_2015:  1.718099e+00<br />Region: Central and Eastern Europe<br />country: Slovakia","Score_2017: 5.758<br />var_2017_2015: -1.538989e+00<br />Region: Central and Eastern Europe<br />country: Slovenia","Score_2017: 4.829<br />var_2017_2015:  4.028436e+00<br />Region: Sub-Saharan Africa<br />country: South Africa","Score_2017: 5.838<br />var_2017_2015: -2.439843e+00<br />Region: Eastern Asia<br />country: South Korea","Score_2017: 6.403<br />var_2017_2015:  1.169219e+00<br />Region: Western Europe<br />country: Spain","Score_2017: 4.440<br />var_2017_2015:  3.956920e+00<br />Region: Southern Asia<br />country: Sri Lanka","Score_2017: 4.139<br />var_2017_2015: -9.032968e+00<br />Region: Sub-Saharan Africa<br />country: Sudan","Score_2017: 7.284<br />var_2017_2015: -1.086367e+00<br />Region: Western Europe<br />country: Sweden","Score_2017: 7.494<br />var_2017_2015: -1.225781e+00<br />Region: Western Europe<br />country: Switzerland","Score_2017: 3.462<br />var_2017_2015:  1.516966e+01<br />Region: Middle East and Northern Africa<br />country: Syria","Score_2017: 5.041<br />var_2017_2015:  5.328038e+00<br />Region: Central and Eastern Europe<br />country: Tajikistan","Score_2017: 3.349<br />var_2017_2015: -1.142555e+01<br />Region: Sub-Saharan Africa<br />country: Tanzania","Score_2017: 6.424<br />var_2017_2015: -4.802512e-01<br />Region: Southeastern Asia<br />country: Thailand","Score_2017: 3.495<br />var_2017_2015:  2.310672e+01<br />Region: Sub-Saharan Africa<br />country: Togo","Score_2017: 6.168<br />var_2017_2015:  3.587102e-06<br />Region: Latin America and Caribbean<br />country: Trinidad and Tobago","Score_2017: 4.805<br />var_2017_2015:  1.392695e+00<br />Region: Middle East and Northern Africa<br />country: Tunisia","Score_2017: 5.500<br />var_2017_2015:  3.150788e+00<br />Region: Middle East and Northern Africa<br />country: Turkey","Score_2017: 5.822<br />var_2017_2015:  4.938717e+00<br />Region: Central and Eastern Europe<br />country: Turkmenistan","Score_2017: 4.081<br />var_2017_2015:  3.815819e+00<br />Region: Sub-Saharan Africa<br />country: Uganda","Score_2017: 4.096<br />var_2017_2015: -1.249733e+01<br />Region: Central and Eastern Europe<br />country: Ukraine","Score_2017: 6.648<br />var_2017_2015: -3.666139e+00<br />Region: Middle East and Northern Africa<br />country: United Arab Emirates","Score_2017: 6.714<br />var_2017_2015: -2.228044e+00<br />Region: Western Europe<br />country: UK","Score_2017: 6.993<br />var_2017_2015: -1.769911e+00<br />Region: North America<br />country: USA","Score_2017: 6.454<br />var_2017_2015: -4.780263e-01<br />Region: Latin America and Caribbean<br />country: Uruguay","Score_2017: 5.971<br />var_2017_2015: -5.330636e-01<br />Region: Central and Eastern Europe<br />country: Uzbekistan","Score_2017: 5.250<br />var_2017_2015: -2.290749e+01<br />Region: Latin America and Caribbean<br />country: Venezuela","Score_2017: 5.074<br />var_2017_2015: -5.335823e+00<br />Region: Southeastern Asia<br />country: Vietnam","Score_2017: 3.593<br />var_2017_2015: -1.187148e+01<br />Region: Middle East and Northern Africa<br />country: Yemen","Score_2017: 4.514<br />var_2017_2015: -1.199064e+01<br />Region: Sub-Saharan Africa<br />country: Zambia","Score_2017: 3.875<br />var_2017_2015: -1.594360e+01<br />Region: Sub-Saharan Africa<br />country: Zimbabwe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":["rgba(165,218,149,1)","rgba(236,248,162,1)","rgba(255,229,147,1)","rgba(165,218,149,1)","rgba(254,160,99,1)","rgba(223,242,152,1)","rgba(226,90,83,1)","rgba(240,118,86,1)","rgba(255,254,190,1)","rgba(255,214,133,1)","rgba(235,247,160,1)","rgba(255,241,167,1)","rgba(246,129,88,1)","rgba(151,211,149,1)","rgba(248,252,180,1)","rgba(255,231,150,1)","rgba(254,254,189,1)","rgba(162,217,149,1)","rgba(253,156,97,1)","rgba(239,248,165,1)","rgba(189,228,150,1)","rgba(50,136,189,1)","rgba(203,233,151,1)","rgba(238,248,164,1)","rgba(224,87,82,1)","rgba(180,224,149,1)","rgba(253,154,96,1)","rgba(255,253,187,1)","rgba(255,186,115,1)","rgba(215,238,151,1)","rgba(214,238,151,1)","rgba(236,111,85,1)","rgba(255,252,186,1)","rgba(255,239,164,1)","rgba(254,158,99,1)","rgba(214,64,79,1)","rgba(255,255,190,1)","rgba(254,222,138,1)","rgba(239,249,166,1)","rgba(254,223,138,1)","rgba(255,239,165,1)","rgba(230,245,153,1)","rgba(217,70,80,1)","rgba(255,177,109,1)","rgba(231,245,153,1)","rgba(214,238,151,1)","rgba(243,123,87,1)","rgba(198,231,150,1)","rgba(255,255,191,1)","rgba(255,175,109,1)","rgba(137,195,158,1)","rgba(146,205,153,1)","rgba(254,254,189,1)","rgba(255,251,184,1)","rgba(215,66,79,1)","rgba(217,239,151,1)","rgba(255,253,188,1)","rgba(238,248,164,1)","rgba(232,246,154,1)","rgba(242,121,87,1)","rgba(230,98,84,1)","rgba(254,225,141,1)","rgba(204,234,151,1)","rgba(255,251,185,1)","rgba(254,227,144,1)","rgba(255,250,183,1)","rgba(255,231,151,1)","rgba(233,246,157,1)","rgba(255,253,187,1)","rgba(255,212,131,1)","rgba(248,252,180,1)","rgba(255,230,149,1)","rgba(255,255,191,1)","rgba(140,198,157,1)","rgba(255,243,170,1)","rgba(254,228,145,1)","rgba(247,132,88,1)","rgba(254,254,189,1)","rgba(150,210,150,1)","rgba(183,225,150,1)","rgba(255,215,133,1)","rgba(205,234,151,1)","rgba(254,167,104,1)","rgba(215,239,151,1)","rgba(255,239,163,1)","rgba(254,162,101,1)","rgba(255,230,149,1)","rgba(246,252,178,1)","rgba(255,254,190,1)","rgba(255,254,190,1)","rgba(233,246,157,1)","rgba(247,252,178,1)","rgba(221,81,81,1)","rgba(225,87,82,1)","rgba(255,216,134,1)","rgba(189,228,150,1)","rgba(250,253,184,1)","rgba(255,231,151,1)","rgba(213,62,79,1)","rgba(255,253,188,1)","rgba(241,249,169,1)","rgba(255,176,109,1)","rgba(255,244,173,1)","rgba(255,235,158,1)","rgba(255,247,177,1)","rgba(254,225,140,1)","rgba(254,255,190,1)","rgba(255,184,114,1)","rgba(255,231,150,1)","rgba(254,225,141,1)","rgba(134,192,160,1)","rgba(255,187,116,1)","rgba(233,246,156,1)","rgba(255,248,179,1)","rgba(239,248,165,1)","rgba(254,163,101,1)","rgba(255,213,132,1)","rgba(255,233,155,1)","rgba(242,250,171,1)","rgba(255,230,149,1)","rgba(255,181,112,1)","rgba(229,245,152,1)","rgba(200,232,151,1)","rgba(226,90,83,1)","rgba(215,67,80,1)","rgba(133,191,161,1)","rgba(249,253,182,1)","rgba(121,179,167,1)","rgba(255,179,111,1)","rgba(136,194,159,1)","rgba(255,206,127,1)","rgba(242,250,170,1)","rgba(255,244,172,1)","rgba(255,231,150,1)","rgba(194,230,150,1)","rgba(196,230,150,1)","rgba(253,154,96,1)","rgba(253,147,92,1)","rgba(241,119,86,1)","rgba(255,175,109,1)","rgba(254,225,140,1)","rgba(255,254,189,1)","rgba(250,253,184,1)","rgba(146,204,153,1)","rgba(232,246,155,1)","rgba(174,221,149,1)"],"opacity":0.6,"size":11.3385826771654,"symbol":"circle","line":{"width":1.88976377952756,"color":["rgba(165,218,149,1)","rgba(236,248,162,1)","rgba(255,229,147,1)","rgba(165,218,149,1)","rgba(254,160,99,1)","rgba(223,242,152,1)","rgba(226,90,83,1)","rgba(240,118,86,1)","rgba(255,254,190,1)","rgba(255,214,133,1)","rgba(235,247,160,1)","rgba(255,241,167,1)","rgba(246,129,88,1)","rgba(151,211,149,1)","rgba(248,252,180,1)","rgba(255,231,150,1)","rgba(254,254,189,1)","rgba(162,217,149,1)","rgba(253,156,97,1)","rgba(239,248,165,1)","rgba(189,228,150,1)","rgba(50,136,189,1)","rgba(203,233,151,1)","rgba(238,248,164,1)","rgba(224,87,82,1)","rgba(180,224,149,1)","rgba(253,154,96,1)","rgba(255,253,187,1)","rgba(255,186,115,1)","rgba(215,238,151,1)","rgba(214,238,151,1)","rgba(236,111,85,1)","rgba(255,252,186,1)","rgba(255,239,164,1)","rgba(254,158,99,1)","rgba(214,64,79,1)","rgba(255,255,190,1)","rgba(254,222,138,1)","rgba(239,249,166,1)","rgba(254,223,138,1)","rgba(255,239,165,1)","rgba(230,245,153,1)","rgba(217,70,80,1)","rgba(255,177,109,1)","rgba(231,245,153,1)","rgba(214,238,151,1)","rgba(243,123,87,1)","rgba(198,231,150,1)","rgba(255,255,191,1)","rgba(255,175,109,1)","rgba(137,195,158,1)","rgba(146,205,153,1)","rgba(254,254,189,1)","rgba(255,251,184,1)","rgba(215,66,79,1)","rgba(217,239,151,1)","rgba(255,253,188,1)","rgba(238,248,164,1)","rgba(232,246,154,1)","rgba(242,121,87,1)","rgba(230,98,84,1)","rgba(254,225,141,1)","rgba(204,234,151,1)","rgba(255,251,185,1)","rgba(254,227,144,1)","rgba(255,250,183,1)","rgba(255,231,151,1)","rgba(233,246,157,1)","rgba(255,253,187,1)","rgba(255,212,131,1)","rgba(248,252,180,1)","rgba(255,230,149,1)","rgba(255,255,191,1)","rgba(140,198,157,1)","rgba(255,243,170,1)","rgba(254,228,145,1)","rgba(247,132,88,1)","rgba(254,254,189,1)","rgba(150,210,150,1)","rgba(183,225,150,1)","rgba(255,215,133,1)","rgba(205,234,151,1)","rgba(254,167,104,1)","rgba(215,239,151,1)","rgba(255,239,163,1)","rgba(254,162,101,1)","rgba(255,230,149,1)","rgba(246,252,178,1)","rgba(255,254,190,1)","rgba(255,254,190,1)","rgba(233,246,157,1)","rgba(247,252,178,1)","rgba(221,81,81,1)","rgba(225,87,82,1)","rgba(255,216,134,1)","rgba(189,228,150,1)","rgba(250,253,184,1)","rgba(255,231,151,1)","rgba(213,62,79,1)","rgba(255,253,188,1)","rgba(241,249,169,1)","rgba(255,176,109,1)","rgba(255,244,173,1)","rgba(255,235,158,1)","rgba(255,247,177,1)","rgba(254,225,140,1)","rgba(254,255,190,1)","rgba(255,184,114,1)","rgba(255,231,150,1)","rgba(254,225,141,1)","rgba(134,192,160,1)","rgba(255,187,116,1)","rgba(233,246,156,1)","rgba(255,248,179,1)","rgba(239,248,165,1)","rgba(254,163,101,1)","rgba(255,213,132,1)","rgba(255,233,155,1)","rgba(242,250,171,1)","rgba(255,230,149,1)","rgba(255,181,112,1)","rgba(229,245,152,1)","rgba(200,232,151,1)","rgba(226,90,83,1)","rgba(215,67,80,1)","rgba(133,191,161,1)","rgba(249,253,182,1)","rgba(121,179,167,1)","rgba(255,179,111,1)","rgba(136,194,159,1)","rgba(255,206,127,1)","rgba(242,250,170,1)","rgba(255,244,172,1)","rgba(255,231,150,1)","rgba(194,230,150,1)","rgba(196,230,150,1)","rgba(253,154,96,1)","rgba(253,147,92,1)","rgba(241,119,86,1)","rgba(255,175,109,1)","rgba(254,225,140,1)","rgba(255,254,189,1)","rgba(250,253,184,1)","rgba(146,204,153,1)","rgba(232,246,155,1)","rgba(174,221,149,1)"]}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-20],"y":[1],"name":"99_a8a2fffbb3613e3b613c01cd40d23988","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"skip","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#3288BD"],[0.0526315789473684,"#609FB1"],[0.105263157894737,"#7EB8A5"],[0.157894736842105,"#95D197"],[0.210526315789474,"#AEDD95"],[0.263157894736842,"#C6E896"],[0.31578947368421,"#DEF298"],[0.368421052631579,"#EBF7A0"],[0.421052631578947,"#F3FAAD"],[0.473684210526316,"#FBFDB9"],[0.526315789473684,"#FFFAB7"],[0.578947368421053,"#FFF0A6"],[0.631578947368421,"#FFE696"],[0.684210526315789,"#FFD886"],[0.736842105263158,"#FFBE75"],[0.789473684210526,"#FEA466"],[0.842105263157895,"#FA8959"],[0.894736842105263,"#EE7256"],[0.947368421052632,"#E25952"],[1,"#D53E4F"]],"colorbar":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"thickness":23.04,"title":"Score_2017","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"tickmode":"array","ticktext":["3","4","5","6","7"],"tickvals":[0.0205095043925482,0.236398959296772,0.452288414200997,0.668177869105221,0.884067324009445],"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"ticklen":2,"len":0.5}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":33.0471841704718,"r":7.30593607305936,"b":47.0015220700152,"l":206.75799086758},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-25.2081996206233,25.4074343238385],"tickmode":"array","ticktext":["-20","-10","0","10","20"],"tickvals":[-20,-10,0,10,20],"categoryorder":"array","categoryarray":["-20","-10","0","10","20"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Happiness variation between 2015 and 2017 [%]","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Australia and New Zealand","Central and Eastern Europe","Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Southern Asia","Sub-Saharan Africa","Western Europe"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Region","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"33e84df4c705":{"colour":{},"x":{},"y":{},"text":{},"type":"scatter"}},"cur_data":"33e84df4c705","visdat":{"33e84df4c705":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

We can see that, in general, the regions that have high happiness score
tend to be more stable, while the ones with the highest variations tend
to also have a low score

# Are “Happy” People Really Happy?

In this section, we look into the correlation between the happiness
score and behavioral indicators that are generally associated to
happiness. In order to do this, we will use the Kaggle dataset [65 World
Indexes](https://www.kaggle.com/joniarroba/65-world-indexes-gathered).

``` r
kaggle <- read.csv("../input/65-world-indexes-gathered/Kaggle.csv")
str(kaggle)
colnames(kaggle)[1] <- "Country"
merged <- merge(happy, kaggle, by.x = "Country", by.y = "Country")
str(merged)
```

## Homicide Rate

We would think that happy people would tend to cause less harm to other
people. The homicide rate is a good indicators of
this.

``` r
ggplot(data=merged, aes(y=Homicide.rate.per.100k.people.2008.2012, x=Score_2015))+
  theme_bw() +
  geom_point(aes(color=Region), size=3, alpha=0.6)   +
  geom_smooth() +
  ylab("Homicide Rate per 100k people 2008-2012")+
  xlab("Happiness Score in 2015")
```

![](are-happy-people-really-happy_files/figure-gfm/homicide_rate-1.png)<!-- -->

The homicide rate is pretty flat as a function of the happiness score,
and seems to have more a dependence on the geographical region.

## Suicide Rate

When we look at the suicide rate, this is not flat as a function of the
happiness score, and the trends are different for the male suicide rate
and the female
one.

``` r
srf <- ggplot(data=merged, aes(y=Female.Suicide.Rate.100k.people, x=Score_2015))+
  theme_bw() +
  geom_point(aes(color=Region), size=3, alpha=0.6) + geom_smooth()+
  ylab("Female Suicide Rate per 100k people")+
  xlab("Happiness Score in 2015")

srm <- ggplot(data=merged, aes(y=MaleSuicide.Rate.100k.people, x=Score_2015))+
  theme_bw() +
  geom_point(aes(color=Region), size=3, alpha=0.6) +
  geom_smooth() +
  ylab("Male Suicide Rate per 100k people")+
  xlab("Happiness Score in 2015")
  
grid.arrange(srf+theme(legend.position = "none"), srm+theme(legend.position = "none"), get_legend(srm), ncol = 3)
```

![](are-happy-people-really-happy_files/figure-gfm/suicide_world-1.png)<!-- -->

The trends seem to be different from region to region. Therefore, in the
next section we will look at one specific example: Western Europe.

## Suicide Rate in Western Europe

Since the suicide rate as a function of happiness score seems to have
different behaviors in different geographic region, in this section we
focus on Western Europe.

``` r
europe<-merged[ merged$Region=="Western Europe", ]

srfe <- ggplot(data=europe, aes(y=Female.Suicide.Rate.100k.people, x=Score_2015))+
    theme_bw() +
    geom_smooth()+
    scale_color_distiller(palette = "Spectral")+
    geom_text(aes(label=Country)) +
    xlab("Happiness Score in 2015") +
    geom_point(aes(color=GDP_2015), size=4, alpha=0.6) + 
    ylab("Female Suicide Rate per 100k people")
 
srme <-ggplot(data=europe, aes(y=MaleSuicide.Rate.100k.people, x=Score_2015))+
    theme_bw() +
    geom_smooth()+
    scale_color_distiller(palette = "Spectral")+
    geom_text(aes(label=Country)) +
    xlab("Happiness Score in 2015") +
    geom_point(aes(color=GDP_2015), size=4, alpha=0.6) + 
    ylab("Male Suicide Rate per 100k people")
    
grid.arrange(srfe+theme(legend.position="none"), srme+theme(legend.position="none"), get_legend(srme), ncol = 3,widths=c(3,3,1))
```

![](are-happy-people-really-happy_files/figure-gfm/suicide_europe-1.png)<!-- -->

From these plots we can notice that, in Western Europe, the suicide rate
has a correlation with the happiness score opposite than what we would
expect: **the higher the happines score, the higher the suicide rate**.
This is the case for both male suicide rate and female suicide rate:
these two distributions have different normalization (the male suicide
rate is higher than the female suicide rate) but very similar shape in
the dependence from the happiness score.

To understand this result, we need some additional information on other
differences between the countries considered. One option is to look at
the temperature in those countries. To have an indication of the
temperature, we will look at the average temperature in January 2013
(the most recent year available in the weather dataset). This means
that, in our plot, the happiness rate and the temperature will
correspond to two different years (2015 and 2013 respectively). Given
the short time distance, it is safe to assume that the average January
2013 temperature is a good guess also for the average 2015 temperature.
To have data about the temperature, we will use the Kaggle [Climate
Change: Earth Surface Temperature
Data](https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data)
dataset.

``` r
weather_all <- read.csv("../input/climate-change-earth-surface-temperature-data/GlobalLandTemperaturesByCountry.csv")
str(weather_all)
weather_2013_01 <- weather_all[weather_all$dt=="2013-01-01",]
weather_2013_01[weather_2013_01$Country=="Denmark",]<-weather_2013_01[weather_2013_01$Country=="Denmark (Europe)",]
str(weather_2013_01)
merged <- merge(merged, weather_2013_01, by.x = "Country", by.y = "Country")
str(merged)
```

First of all, let’s look at the dependence of the happiness score on the
average January temperature

``` r
europe<-merged[ merged$Region=="Western Europe", ]
happy_temp  <- ggplot(data=europe, aes(y=AverageTemperature, x=Score_2015))+
  theme_bw() +
  geom_point(aes(color=GDP_2015), size=4, alpha=0.6) +
  geom_smooth() +
  scale_color_distiller(palette = "Spectral")+
  geom_text(aes(label=Country)) +
  ylab("Average Temperature Jan 2013")+
  xlab("Happiness Score in 2015")
happy_temp  
```

![](are-happy-people-really-happy_files/figure-gfm/happiness_temp-1.png)<!-- -->

The happiness score is higher in “cold countries”. But, if we look at
the dependence of the suicide rate from the January temperature, this is
what we
obtain:

``` r
pm <- ggplot(data=europe, aes(y=MaleSuicide.Rate.100k.people, x=AverageTemperature))+
  geom_point(aes(color=Score_2015), size=4, alpha=0.6) +
  theme_bw() +
  geom_smooth() +
  geom_text(aes(label=Country)) +
  scale_color_distiller(palette = "Spectral")+
  ylab("Male Suicide Rate per 100k people")+
  xlab("Average Temperature Jan 2013")

pf <- ggplot(data=europe, aes(y=Female.Suicide.Rate.100k.people, x=AverageTemperature))+
  geom_point(aes(color=Score_2015), size=4, alpha=0.6) +
  theme_bw() +
  geom_smooth() + 
  geom_text(aes(label=Country)) +
  scale_color_distiller(palette = "Spectral")+
  ylab("Female Suicide Rate per 100k people")+
  xlab("Average Temperature Jan 2013")

grid.arrange(pf+  theme(legend.position = "none"), pm+  theme(legend.position = "none"), get_legend(pf), ncol = 3, widths=c(3,3,1))
```

<img src="are-happy-people-really-happy_files/figure-gfm/suicide_europe_temp-1.png" style="display: block; margin: auto;" />

We can see how, despite being the ones with the highest happiness score,
colder countries are also the ones with the highest suicide rate.

# Conclusion

The happiness score provided, seems to be an indicator of
socio-economical satisfaction rather than absolute “happiness” in the
common sense of the word. This is highlighted by the fact that countries
with a high happiness score don’t correspond, for example, to the
countries with the lowest suicide rate. Taking as example the countries
in Western Europe, there is actually an anti-correlation between
happiness score and suicide rate. This is likely not related to the
happiness score itself, but to other factors that affect both
socio-economical and behavioral indicators. An example of these factors
is the temperature: in cold Western-Europe countries, we have both an
higher happiness score but also an higher suicide rate.

This is clearly not claiming that low temperature is the cause of the
higher suicide rate in some Western Europe countries rather than in
other. But it is indicating that factors different than the ones
considered in the happiness score might need to be taken into account to
provide a happiness rank that is closer to the typical notion of
happiness.
