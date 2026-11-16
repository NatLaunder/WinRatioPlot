# Winnify - improved analysis under the Generalised Pairwise Comparisons Framework

Winnify is a quality of life package for conducting analyses for prioritised outcomes under the generalised pairwise comparisons framework (e.g. Win Ratio, Win Odds, etc).

It provides wrappers for other functions, including the WINS package and any rank-based regression approach with coef() and confint() methods. These wrappers can be used with the core winnify()
function to show treatment effects as successively less important outcomes are added to the 
prioritised outcomes analysis.

The package also supports plotting of results produced by winnify() using the approach pioneered by
the [MSU-TELEMED trial](https://www.doi.org/10.1056/EVIDoa2500217) and recommended by [a tutorial paper published by the same group](https://doi.org/10.1177/17474930261475853)