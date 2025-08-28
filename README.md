# Hypothesis Tests Visualised Project (HTVP)

The purpose of HTVP is to create bite-sized, interactive experiences that enable students to build their conceptual understanding of hypothesis tests and to see hypothesis testing in action.
These webpages are NOT designed to be a replacement for traditional forms of learning (e.g. lectures, textbooks, lab tutorials); instead, they are designed to enhance existing modes of teaching
by facilitating active learning.

## Accessing HTVP

There are three main ways to access the site, each with its own advantages and drawbacks. The table below outlines the primary differences between each approach. The exact installation 
instructions can be found below.
  
| Method | How to Access | Advantages | Disadvantages | Intended Audience |
|:-------------:|:-------------:|:-------------:|:-------------:|:-------------:|
| Package Install | R console | • Site is fast. <br> • Site is most up-to-date. | • Technical knowledge needed on how to use the R console. | • Students using R (and RStudio).<br> • People with IT experience |
| Shinylive | [URL](tjelton.github.io/Hypothesis-Tests-Visualised/) | • Simple URL. | • First load is slow (can take around 30 seconds). <br> • Site is slower than the package install option. | • Students studying a course in hypothesis tests without technical experience. |
| Shinyapps.io | URL - coming soon | • Simple URL. | • Fast load. | • Using the free version of Shinyapps.io. Site will be slow with concurrent users. | • People wanting to quickly view the site. <br> • Not suitable for classrooms because of the many concurrent users. |

### Package Install

This is the most reliable method of accessing the HTVP site, but this method requires a bit of setup, which can be confusing for people who have never used R or RStudio before.

Before you start, you should download R. Below I will have a video tutorial on how to follow these steps, where I will use the RStudio IDE. This is not strictly needed, however, if you want 
to recreate the exact environment I am using, follow the steps here: https://posit.co/download/rstudio-desktop/

These are the steps to view the site from a package install:

1. In the console, install the "remotes" package (if you have not previously installed it) by running:

```
install.packages("remotes")
```

2. In the console, run:

```
remotes::install_github("tjelton/Hypothesis-Tests-Visualised")
```

3. In the console, run the following to load the package:

```
library(HypothesisTestsVisualised)
```

4. Finally, to run the app in your console, run:

```
run_HTVP_app()
```
   
A video showing these steps can be found [here](https://www.youtube.com/watch?v=4BCYAROzi-U)

### Shinylive

Shinylive is a way of using HTVP from your browser. When visiting the URL, it will take a while to load (could take over 30 seconds). It is run through your computer, and so is a good free 
way of visting the site.

The URL is: [tjelton.github.io/Hypothesis-Tests-Visualised/](https://tjelton.github.io/Hypothesis-Tests-Visualised/)

### shinyapps.io - Coming soon

shinyapps.io is a common method used to easily share shinyapps (HTVP is a shiny app). However, on the free account, the usage is limited. There is a limit to the number of people that can access
the site concurrently, and the site can only be accessed for 25 hours every month (for all users).

This option is provided primarily for those that want to quickly view the site. It is not suitable for large class sizes.

## Found an Issue?

Did you find a bug? Is one of the statistical explanations incorrect, or is the math not quite what you would expect?

I'd apprecaite you letting me know! Please create a Github issue.

## Contributing

Thank you for your interest! At this time, this project is not accepting contributions.  Please feel free to open issues for bug reports or feature requests. Contributions may be considered in the future.

## AI Disclaimer

The project was created with the assistance of generative AI tools. This includes coding and content writing. 





