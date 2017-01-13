Metapopulation Age Until Extinction
-----------------------------------

![](C:\Users\Ruth\git\ruthubc\DispersalSimulation\PaperOutput\ResultsGraphs_DispersalSim_files/figure-markdown_strict/metapop-1.png)

1.  *Figure :Metapopulation survival against competition type, where
    competition type ranges from 0 to 1, with 0 being full scramble
    competition and 1 is full contest competition. Environmental
    variance is 0 and number of offspring set to 6. No generations
    removed*

As competition changes from scramble to contest competition the
metapopulation age increases, but this interacts with the body size
needed to disperse, unless the populations survive to 500 generations,
in which cases obviously

#### Why dispersal increases metapopulation survival?

The simulation only starts with x number of colonies, but there are
‘spaces’ for 200. More dispersal means that these spaces get filled up.

#### How does this interact with competition?

If food is shared via contest competition then normally some individuals
will get enough food to disperse, even if individuals have to be large
to disperse. However if food is shared via contest competition then this
is not the case.

Population survival
-------------------

![](C:\Users\Ruth\git\ruthubc\DispersalSimulation\PaperOutput\ResultsGraphs_DispersalSim_files/figure-markdown_strict/popSurv-1.png)

1.  *Figure :Population survival against competition type, where
    competition type ranges from 0 to 1, with 0 being full scramble
    competition and 1 is full contest competition. Environmental
    variance is 0 and number of offspring set to 6. All generations
    included. Survival calculated from survival function*

As competition increases the population survival time increases. But the
size needed to disperse surprisingly does not have a massive effect on
population survival esp compared to competition.

Metapopulation survival against population survival
---------------------------------------------------

![](C:\Users\Ruth\git\ruthubc\DispersalSimulation\PaperOutput\ResultsGraphs_DispersalSim_files/figure-markdown_strict/MetSurvpopSurv-1.png)

1.  *Figure :Metapopulation against population survival. Environmental
    variance is 0 and number of offspring set to 6. All generations
    included. Survival calculated from survival function*

From this graph we can see that dispersal affects whether populations
survival affects metapopulation survival. When the size needed to
disperse is 0.2 (i.e. low) then the metapopulation survives to 500
generations regardless of the survival of the populations. The higher
the disperal size is, the more the population survival affects the
metapopulation survival.

Percentage of populations that go extinct without dispersing
------------------------------------------------------------

    Warning: Removed 3 rows containing missing values (geom_point).

    Warning: Removed 1 rows containing missing values (geom_path).

![](C:\Users\Ruth\git\ruthubc\DispersalSimulation\PaperOutput\ResultsGraphs_DispersalSim_files/figure-markdown_strict/popsExtNoDisp-1.png)
(@) *Figure :Percentage of colonies that go extinct without dispersing*

Conclusion
----------

Competition affects population survival and therefore metapopulation
survival, but low population survival is off-set by disperal to increase
metapopulation survival.

Metapopulation and population survival and environmental variance
=================================================================

### TODO: could remove come layers of comp variables

Metapopulation Survival
-----------------------

![](C:\Users\Ruth\git\ruthubc\DispersalSimulation\PaperOutput\ResultsGraphs_DispersalSim_files/figure-markdown_strict/metapopVar-1.png)

1.  *Figure :Metapopulation survival environmental variance. Adult
    dispersal limit is set to 0.8 and number of offspring set to 6. All
    generations included*

Intermediate environmental variance increases metapopulation survival
for some competition measures when dispersal is restricted.
