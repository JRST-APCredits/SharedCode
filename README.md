This repository contains the codes for the JRST-AP Skip Project (Fischer et al., under review)
[To do: add paper's citation]

## Goals of the project
Building upon broad policy pushes to expand access to AP, millions of students from a very broad range of high schools take and pass Advanced Placement (AP) examinations each year, with particularly high rates among students attending large research universities. However, university departmental policies to allow students to replace introductory gateway courses with “AP credit” greatly vary across courses within universities and across universities within courses, suggesting no strong empirical basis of these policies. Some policies seem liberal (i.e., exempting many students to address teaching / classroom shortages), while others appear overly conservative (i.e, exempting almost no students to increase tuition dollars or teaching assignments for graduate students). Further, research universities tend to have conservative policies which cumulatively across departments can add a semester of study to the undergraduate degree, causing a significant and potentially unnecessary financial burden. Department policy setters claim to institute such policies because they don’t believe the courses are equivalent or that they were taken too long ago, and students are at risk for failing courses that build upon the potentially-exempted course. However, little evidence exists to support these claims. We intend to provide administrators with evidence-based guidance on AP credit policies: under which circumstances is there no risk for exempting students, what is the size of the risk when it occurs, and for whom are there advantages and disadvantages of being exempted from a foundation course?

## Sample Description
This study examines students taking introductory three science gateway course series: Biology, Chemistry, and Physics. These science course series are selected because they have high AP enrollments in high schools and high course enrollments in research universities. Further, they almost universally involved fixed orders of courses (e.g., Physics 1 to Physics 2) and high failure rates: for both students and departments, AP policies constitute a high stakes decision.
The AP program was heavily re-designed approximately 5 years ago, improving the learning benefits for students as well as the validity of the scores for predicting college performance. The redesign was sequentially rolled out across courses: 2013 for Biology, 2014 for Chemistry, and 2015 for Physics. This study only focuses on cohorts of students who took revised AP science examinations (AP Calculus, another high enrollment gateway course series, is excluded from the analysis because the redesign only occurred in 2017 and because many universities have instituted more complex placement exam-based policies).

## File Structure
For an overview of the variables used and naming convention, see 'AP Skip Project - Data Description' file. 

Step-by-step analysis:
1. Descriptive statistics: 'Institution Analysis.R'
2. Check missing data: 'MissingDataAnalysis.R'
3. Run analyses + visualizations: 'Institution Analysis.R'
4. Check ICC for regression models: 'ICCAnalysis.R'
5. Run metaanalysis to pool results from all institutions: 'MetaAnalysis.R'
