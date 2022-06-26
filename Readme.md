# Proposal for DataChallenge

**Patterns & Trends in Environmental Data / Computational Movement Analysis Geo 880**

| Semester:      | FS22                                                                       |
|------------------------|------------------------------------------------|
| **Data:**      | Roe Deer Movement Data                                                     |
| **Title:**     | Examining the impact of recreational outdoor activity on deer movement     |
| **Student 1:** | Nikita Krähenbühl                                                          |
| **Submission** | <https://nikitakrahenbuhl.github.io/datachallenge-n/semester_project.html> |

Below you will find the details of the proposal. The report itself can be viewed via:

<https://nikitakrahenbuhl.github.io/datachallenge-n/semester_project.html>

## Abstract

<!-- (50-60 words) -->

Outdoor recreation, particularly mountain biking is growing rapidly. This generates additional pressure for green spaces to provide both a recreational value and a space for wildlife to retreat to. In this project I examine deer movement data and recreational routes to see if recreational activities influence the movement variability and extent of deer.

## Research Questions

<!-- (50-60 words) -->

-   How is the movement of deer affected by recreational activities such as hiking and mountainbiking

## Results / products

<!-- What do you expect, anticipate? -->

-   Avoidance of routes/infrastructure for recreation
-   Increased movement during the weekend due to disturbance

## Data

<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->

-   The given datasets
-   Trailforks DB
-   Strava Segments for MTB
-   SwitzerlandMobility Routes or OSM Routing data
-   OSM Public traces

## Analytical concepts

<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->

-   Zoning - are the maximum boundaries influenced by infrastructure/routes or by other deer
-   Changes in movement during the weekend (comparison of average speed/distance)
-   Avoidance of popular routes - buffer

## R concepts

<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->

-   sf
-   difftime

## Risk analysis

<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->

-   Some other major driver for deer movement - i.e. interaction with other deer

## Questions?

<!-- Which questions would you like to discuss at the coaching session? -->

-   Is this feasible or is the scope/additional data requirement too large?
-   Do you have access to visitor data for Sihlwald - I am sure they have counters?
-   Is there some characteristic of deer that might be a problem with my question (i.e. Deer really hate weekends anyways because they are really hard working?)
-   Any experience with path data - OSM? Open Data
