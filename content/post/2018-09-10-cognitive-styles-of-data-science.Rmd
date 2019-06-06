---
title: Cognitive styles of data science
author: Richard Careaga
date: '2018-09-10'
slug: cognitive-styles-of-data-science
categories:
  - Data Science
tags:
  - meta
  - tools
---
Yihui Xie of RMarkdown fame has a long, thoughtful post (https://goo.gl/WjA7t8) on a debate for and against working in various types of interactive notebooks, such as Juypter for the Python platform, IDEs like RStudio in its so-called "notebook mode" and the old school heavy code commenting, among other approaches. Although that's the primary subject, he weaves in some astute observations of the differences among "data scientists" based on whether their perspective grows from data origins or is aimed at ultimately producing software.

He associates R with a data orientation and Python with a software engineering orientation:

>  I feel a major difference between the R culture and Python culture is that Python users seem to create code more often, whereas R users often use code. There seems to be a strong atmosphere of software engineering in the Python world: in the beginning was the custom class (with methods). For R users, in the beginning was the data.

This puts a finger on something that has been lurking in my own thinking. *Data science* caught on as a catchy phrase to describe the ways we have invented to deal in useful ways with what millenials grew up on -- the vast increase of interconnectedness possibilities, digital signal encoding all kinds of phenomena in petabyte increments, accompanied by varying signal-to-noise ratios. The term data science today is on a par with *natural philosophy* in the Enlightenment, before the study of the world around us settled into discrete and specialized aspects (think of genetic neuroscience, perhaps). The aim of data science is, of course, how we extract meaning from all of these petabyte rivers flowing around us.

Data science is inescapably interdisciplinary. No one of us can wield all the tools necessary. There must be the seekers after truth on the ground, to anchor results in the real world. They are seldom the same as the mathematicians who influence the statisticians who set the computer science types to design algorithms and assess their running times, who may also, but not always, be the computer engineer working to make that feasible by distributed processing or the software engineer writing efficient, fault-tolerant code to implement it. The software engineer may turn to a designer for creating the most human-effective ways of getting data in and taking meaning out. Analysts may be constantly asking what we could be missing or whether it's possible that an organization has selected a metric for control that is being gamed. It's no wonder that we can't agree on the "best" tools or that there is an explosion of infrastructure software as service.

What attracted me to science as an undergraduate was curiosity. I was drawn to programming in my early career by long frustration with manual calculation (it wasn't before I was 40 that I discoverd that I'm mildly numerically dyslexic, prone to transposition errors). In programming there was, at the beginner's level, the comfort of certainty. A garage startup during the dotcom boom (no V.C. ever knocked) gave the wonderful experience of what was then a high-speed connection (a fractional T-1 line), and the trial-and-error of getting my domain name records right, servers properly configured and operating and becoming modestly well known for troubleshooting Netscape http/https, mail, news, proxy, certificate and LDAP software. 

After my initial exposure to ER data science during the 2007-2008 financial panic, I read Tukey's classic *Exploratory Data Analysis.* It was something of a second childhood -- **this stuff is fun!** That's an aspect that will always attract me, poking under rocks to see what crawls out. As I spent more time in projects aiming to improve electronic discovery in litigation, analysis came to the fore. A copy of email file folders of 148 Enron executives converted from Lotus Notes and Microsoft Outlook into 500,000 plain text files. The Old Law tradition was to have drones read (or at least scan) every line, looking for smoking guns. That's dumb. A good half were duplicates, half of the rest consisted of usual outside email gunk. Continuing to narrow, I found only about 35,000 emails between senders and receivers who engaged in mutual correspondence (more detail at https://goo.gl/f1jyMK) and introduced me to graph (network) analysis and measures of closeness, centrality, identification of cliques.

All of which finally comes to the point that the right tool depends on what you are doing at the moment. It can be as simple as a plain text log of what you saw, where you saw it, what you tried and ideas or as (relatively) formal as a package. Unlike Middle Earth, we lack one ring to rule them all.
