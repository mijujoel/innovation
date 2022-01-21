# WHAT IS IT? 

This model focuses on the diffusion and adoption of new ideas or technologies based on "internal influences" (e.g., word-of-mouth) and/or "external influences" (e.g., mass media). This has been the traditional focus of research into the "diffusion of innovations" (see references below).  The LOGISTIC curve is often used to describe internal influences (e.g., the "Bass Model"). In this model, there are a small group of "innovators," or "geeks," who must have everything as soon as it is introduced.  They spread the word, which may take some time because other potential users need to be convinced of the efficacy of the new idea or technology.  This slow rise in the number of adopters reaches a "tipping point" in which a lot of "potentials" accept the new idea or technology.  After a while, the rate of new adoption decreases and slow reduces to a trickle.  The logistic curve is a good representation of this phenomenon.

On the other hand, advertisements through mass media hit the market all at once and are said to lead to more rapid acceptance.  A NEGATIVE EXPONENTIAL curve is often used to represent this behavior.  Think of the "Super Bowl" phenomenon, where companies spend millions to reach a huge audience.

These two models were developed long before the Internet, so there is a need to re-examine how "focused" ads reach the public.  This is still an open question and is being actively supported as a business model by Google and others, leading to issues of privacy, security and a backlash from users.  The current version of this INNOVATION DIFFUSION model looks at the two extremes - word-of-mouth vs mass media - with the intent of examining other models of diffusion.

In addition, the model examines the influences of two new technologies or ideas.  This is the case of VHS vs BETA, the MAC vs the PC, or perhaps cultural ideas that compete for the same population.  The latter problem is actually rather complex since "memes" are a gross oversimplification of how culture works. 

# MODEL OVERVIEW

In the model, individuals are divided into five groups: "Innovators" who MUST have the latest technology at all costs; "Early Adopters" who need a little prompting; "Early Majority" who may need further prompting; "Late Majority" who don't like change; and "Laggards" who will rarely, if ever, adopt anything new.  The percentages of each type are set by sliders (see left side of model interface).  If the sum of these percentages does not equal 100, the model will stop during the Setup procedure and indicate the error in the "Observer" panel at the bottom of the interface.

Each of these five types of people has an "Acceptance" factor - how easily they will adopt the technology on a scale from 1 - 25, with 1 being reserved for the definite acceptance of an innovator; 6 - 10 for "Early Adopters; 11 - 15 for "Early Majority";
16 - 20 for "Late Majority"; and 21 - 25 for "Laggards." These ranges can be altered by editing the sliders. Innovators have already adopted one of the two technologies at the beginning of the model.  Again, sliders are used to set these values.  Note that these values are multiplied by a coefficient of acceptance, so that

    1.0/acceptance_factor * acceptance_coefficient 

determines whether a technology is adopted. These parameters are described below.

Individuals move through the state space, where they meet and interact with other individuals who may have adopted a new technology, adopted an even newer "disruptive" technology, or are "potential" adopters like themselves. If a "potential" interacts with an "adopter," he or she MAY become an "adopter" based on the acceptance probabilities and the category of user to which they belong (e.g., "Early Majority").  The same holds true for "potentials" interacting with "disrupters." "Adopters" and "disruptors" may interact as well, either with their own "kind" or the competing "kind."

There are also parameters for "adopters" to "renege" and return to "potentials" or to "defect" to the even newer "disrupter" technology and discard their "adopter" innovation. Analogous parameters are available for "disrupters."  Switches are available to restrict the model to a single technology or idea competing with the status quo, or to examine two new innovations hitting a population simultaneously or after some lag period.

External influence impacts may also be studied. The impact of mass media is based on the length of time a user spends in a particular region (using different shades of a color to represent "strength" of the influence, as well as different colors to represent two different innovations - "adopter" technologies and "disrupter" technologies.

Future versions of this model will look at intermediate approaches to the spread of technologies or ideas - i.e., "targeted broadcasts," where specific individuals in the population will receive the message simulataneously.  In the current model, interactions are either one-on-one or broadcast to a group within a specified area. The area can be a subset of the population, but it is designated by geographical space, not individuals with particular attributes.


# MODEL SETUP

The model interface is divided into eight parts. 

## Runtime Parameters

  (a) SETUP button to initial the run, 3 STEP buttons, and an ON/OFF button. The 3 STEP buttons move the model the designated number of "ticks" forward. The ON/OFF button is the usual "GO FOREVER" control.

  (b) Slider to stop the simulation after a certain number of "ticks."

  (c) If the random seed switch is on, a new seed is chosen for each run.  If it is off, the slider value is used for the seed. This latter case is used to replicated the results exactly from one run to another (as long as other parameters are not changed). If the slider is changed during a run, it has no effect until SETUP is pressed again. 
  
  (d) Ability to create a movie out of the simulation.  Press the MOVIE button, choose a name for the movie, and the simulation executes for the number of frames set in the slider. Use a ".mov" extension.  It works with Quicktime.

## Demographic Parameters

  (a) Population size - In this version of the model, the population size remains constant.

  (b) This population must be divided into five categories. The output display labeled "Sum" indicates the percentage of the total that has been set by the 5 sliders below it.  This must equal 100% before pressing SETUP.  People who have already adopted the technology or idea at the beginning of the model are "Innovators."  "Early Adopters" are interested in new technologies or ideas but need some prompting.  "Early Majority" and "Late Majority" are somewhat more reluctant to change.  "Laggards" will rarely, if ever, adopt anything new.

  (c) Acceptance Factor - each group will adopt technologies or ideas based on an adoption factor (discussed below) multiplied by an "Acceptance Factor" set here.  Higher numbers mean less chance of adopting.  The categories are non-overlapping. "Innovators" have already adopted the technology or idea at the beginning of the model; their value is fixed at 1, the "Adoption Factor."

## CONVERSATIONS
 
  (a) The model is based on the movement of people, and so their movement per "tick" is adjustable by the modeler.

  (b) If they meet another person, there is a probability of interacting, based on the "prob-conversation" parameter.

  (c) The conversation will last a certain amount of time, meaning that the two people will not move for that length of time.  A square object surrounds the pair during this length of time.  If a person adopts the innovation based on the various modeling parameters set for the conversation, and the link switch has been turned on, there will be a link drawn between the two people that remains during the remainder of the simulation.  This is an indicator of "friendship" or "acquaintanceship."  If the population size is high, this will result in an output display resembling a Jackson Pollack painting, so only use it for small population sizes.  The intent of this link is to eventually expand the model to look at the formation of social networks, including fading of the link over time...

  (d) If the population is not too high, it is useful to observe connections between individuals who have met AND led to acceptance of an innovation/idea.  The links do not "fade" over time in the current model. Note that there is a "memory" attribute for each 	individual, but that records all meetings, whereas the link is only drawn if a successful "transaction" has occurred.

## INTERNAL DIFFUSION PARAMETERS
	
  (a) Link-switch: A switch to turn links between people on or off.  If on, a link is drawn between two people who have met, conversed, and an "adoption" of a technology or idea has occurred.

  (b) For each technology, there is one switch and three sliders:

    - Internal-Tech switch - turn on internal disffusion for this technology.
	
    - Acceptance Coefficient - Multiplier for that technology. This value divided
      by the Accpetance Factor for each category of user determines the probability of
      adopting the technology in question.  
	
    - Renege Factor - Giving up on the new technology and returing to "Potential"
      status.  
	
    - Switch Factor - Probability of changing between the two technologies.

  (c) Change-agent - display showing percent of Innovators for each of the two technologies at the start of the run. If only one technology is on, all of the change agents have adopted that technology at time 0.
  	
  
## EXTERNAL INFLUENCE
	
  (a) Switch to turn on one, none or both external diffusion models.

  (b) Rectangular areas are set with the x-, y- and s- sliders. x- and y- set the center of the area for external diffusion. The s- parameter is the size of the rectange.  So a (0,0) position is the center of the display, while an "s" parameter 	= 100 will cover the entire display.  There is a "triplet" for each technology; they should not overlap.  

  (c) "Smoothness" is an idea borrowed from a Community model on the "Fitness Landscape."  The reference is provided below. This varies the degree to which a user adopts the new technology or idea - with colors varying from dark to light to         indicate more influence.  Technology 1 uses purple; brown is used for technology 2.  Netlogo's color palette ranges from black through dark shades of the chosen color to light shades and then white.

  (d)	Listening-time is the number of ticks the user hears the message while in the rectangular space.  

  (e) Adoption probability is set for each technology - the probability of adopting that technology during the listening time.

## DISPLAY

  (a) Internal diffusion depicts people and "conversations" that take place at each tick step.  Links may be drawn if one of the "potentials" adopts a technology.

  (b) External diffusion depicted as a rectangle with varying amounts of "advertising capability" - lighter shade better.

## ADOPTION CATEGORY HISTOGRAMS

  (a) "Initial Adoption Type" - Used to illustrate how many people are in each category at the beginning of a run.

  (b) "Category of Adopters" - Depicts the number of each category that have adopted either of the two technologies.

## RESULTS

  (a) Populations - plots potentials and adopter/disruptor population over time.

  (b) Groups - breaks down adopters and disruptors by category of user.

  (c) Memory - when two individuals meet, a reference to that person is stored in memory.  Right now, it remains there for the duration of the simulation; forgetfulness will be added in a future release.

  (d) Monitors - Mean no. of partners in memory is tabulated, along with a measure of "familiarity" among the people in the simulation as measured by the total number of people already met before ("friends") divided by the number of conversation starts.  
Actual conversation stats are also included - number of contacts between people, how many lead to a converstation starting, how many previous conversations end during this tick, and how many conversations continue on.
    

# HOW IT WORKS 

## Demography

The population size is set by the modeler and does not change during each run.  What changes are the percentages of each type of user:

  (a) "Innovators" in the typical parlance of innovation theory who start out the simulation using the new innovation.

  (b) "Early Adopters" who will decide to adopt if the idea or technology sounds good.

  (c) "Early Majority" who are a little hesitant and wait to see what others are doing.
 
  (d) "Late Majority" who are very hesitant.
 
  (e) "Laggards" who basically wait until the technology is no longer considered innovative.

Note that (a) through (e) correspond to the categories commonly seen in the literature. These people will be created by pressing the SETUP button and will move around the display interacting with other folks ("internal diffusion") or examining mass media or other general forms of information about the innovation ("external diffusion").  A
"link-switch" can be turned on to add a communications channel between any two nodes. In this version of the model, the link remains in place for the duration of the simulation.

## Individual Behavior

Individuals who are not currently in a "conversation" will perform a number of steps:

  (a) Think about their current status, possibly changing their mind after accepting a new technology: they may return to a "Potential," whereby they are subject to the influences of internal or external factors. If they are a technologist ("Adopter" or "Disrupter"), they may become a "Disrupter" if they are currently an "Adopter" or an "Adopter" if currently a "Disrupter." Note that these changes are based on "personal reflection" rather than due to external or internal influences. These changes are based on slider values "prob-tech1-to-potential," "prob-tech1-to-tech2," "prob-tech2-to-potential" and "prob-tech2-to-tech1."

  (b) Move a little based on the "movement" slider set in the interface. 

  (c) Check to see if they are on a "media center" patch, in which case they may accept a new technology after hearing a "mass media presentation."

  (d) Finally, they check to see if someone is around to talk to about technologies. The "movement" parameter" and "prob-conversation" sliders help determine how often
     conversations occur - just meeting someone does not guarantee that a conversation about technology occurs. If a conversation does occur, one individual is the initiator and the other the recipient of information.

  (e) If an individual is already paired, he or she will update his or her status. Whether to accept the new technology is based on the slider probability for each technology. This is only checked at the end of the conversation, not once per time unit. These sliders are "tech1-coefficient-acceptance" and "tech2-coefficient-acceptance." However, each of these values is divided by a factor set for each individual's type (see Demographic Parameters below). For example, an "Innovator" may accept the new technology at the general "coefficient-acceptance"      level, where as a "laggard" may be given a high resistance factor, say 10, meaning that acceptance is 1/10th of the general "coefficient-acceptance" level.

## Statistics

Output graphs provide a number of summaries for the modeler's use.  In addition, debugging statements are printed to the screen.  These may be saved in other tools (e.g., Excel) for further analysis.

# HOW TO USE IT

The model was originally built as a guide to reading through the literature on the diffusion of innovations; some references are provided below. The basic idea is that internal diffusion will follow a logistic curve based on the notion that as word spreads, there will be a threshold until the new technology or innovation "takes off."
After a number of people have accepted the new technology or idea, the adoption rate slows down because the remaining users just don't see the point.  So there is a "life cycle" to technologies or ideas, and, after a while, new concepts are needed to begin the process anew. As a result, we see iPhones and other technologies going through "new
releases," whereby some folks just have to have the latest, while others wait to see what the fuss is about.  It would be interesting to investigate how many cycles a particular technology can go through before people get "bored."  It's not clear that "ideas" go through the same life cycle.  This model could be used to study "memes" since it focuses on horizontal transmission of units.  But this raises all sorts of complications that beg for further analysis.  As well as extensions to include vertical transmission of ideas and technologies (i.e., parents to offspring, one generation to the next, ...).

# THINGS TO TRY
Note change in rate of technology adoption based on parameters. The "expected" behavior is a logistic curve for a single technology introduced by word-of-mouth. A strictly "mass media" introduction will spread more rapidly, shooting up to the maximum after a small delay (depending upon parameter settings) - a negative exponential model.

Adding a second technology after a short delay leads to all sorts of interesting behavior. In some cases, the newer technology will not be able to gain a foothold. If reneging is added, or if "Adopters" can switch to the "Disrupter" technology, the technology curves become quite complex. According to theory, at least, a "Disrupter" technology often needs to hit a specific niche in order to gain a foothold once an "Adopter" technology has saturated the market (e.g., "WII" vs Playstation 3 vs. Xbox 360 marketing strategies).

# EXTENDING THE MODEL 

As social media has grown over the past decade, topics such as tipping points, crowdsourcing, focused advertising, tweeting, etc. highlight some possible ways to make use of the basic framework of this model and extend it into several different areas.  These are described below as possible extensions.

  (a) Change agents - Rogers  (see references) talks about the importance of change agents to facilitate adoption of new technologies or ideas. In the current model, the innovator class provides the change agents - either type 1 or 2 or split between the two depending on the change-agent parameter(a percentage of type 1 is set by the user, so that type 2 is 1 - that percentage; ignored if only one technology/idea used).  In a future model, other characteristics can be added to these agents so they can talk to multiple people at once or have some other characteristics that facilitate change.
   
  (b) Internet - The classic diffusion models differentiate individual contacts from mass media. The Internet, however, is really a combination of the two, providing "intimate" conversations with a single user, but marketing to a wide audience.

  (c) Social Networking - links can be placed between any two nodes, similar to SNA software.  Calculations could be added for degree of centrality and other measures used in Social Network Analysis (SNA). 

  (d) Make more use of memory - acceptance changes with familiarity of partner.

  (e) Mixed mode diffusion - e.g., web-based diffusion seems like a mixture of internal and external diffusion models.

  (f) Calibrate with math models of diffusion - would like to see how the various combinations of parameters relate to various mathematical models of diffusion.

  (g) Radical vs. Incremental Innovation - idea of an "innovation" and a "disruption" examine these distinctions through variable values but not through changes in the logic. iPhones, tablets, cloud provisioning are all impacting the concept of the desktop computer, building off of the old (1960s) concepts of time-sharing with dumb terminals accessing mainframes.

  (h) Extensions to radical vs. incremental innovation: Henderson & Clark (1990) note that the distinction between radical and incremental change is incomplete, creating a 2x2 matrix that adds the categories of (1) "modular innovation" (where "core concepts" are overturned as in radical innovation but linkages between components and core concepts are left unchanged, as in incremental innovation) and "architectural innovation" (where core concepts are reinformed as in incremental innovation but linkages are changed as in radical innovation).  The iPhone can arguably be considered an architectural innovation from Apple's standpoint - it takes the core ideas of
graphics, ease of use, a closed OS, etc. - just like Apple computers - but it adds a new twist that appears radical to the cell phone industry.  How to account for the diffusion of such ideas is not yet clear.  Indeed, the percentages of "early adopters" for Apple technology may have to be considered a separate category from the general population of "early adopters."


# NETLOGO FEATURES 

Early versions of the model set the "adoption category" of individuals directly in code. The technique was suggested to me on the Community Web Pages, and so I have included it here because I thought it was an interesting programming construct. (It is actually not used for this purpose any more, so the code segment below has not been updated to the latest version of Netlogo.) 

The idea is that some folks are "innovators" and will try anything new. Some will never adopt anything new, and others are spread out into other groups. Rogers (see references below) discusses a normal distribution with 5 categories such as 1 & 2 (the early adopters) = 5 (adopt with a lot of resistance) and two middle categories, 3 & 4. This is referred to as the "kind-structure" in the model. Category 1 is set by the number of "adopters" + "disrupters" - those who already have the new innovation at the start of the simulation. To determine the rest of the population, an array is set up in the initialization part of the model: 

    ;The first entry is the category type; the second is a probability. 
    set kind-structure [ [ 2 12 ] [ 3 47 ] [ 4 81 ] [ 5 100 ] ]  

This is used in the following reporter routine:   

    to-report determine-kind [ probability-index ]  
      let this_kind first ( first ( filter [ last ? >= probability-index ] 
            kind-structure ) )
      print this_kind+":"+probability-index+":"+filter 
            [ last ? >= probability-index ] kind-structure  
      report (this_kind)  
    end  

The "let" statement was graciously explained to me through the community web pages. This routine is called during setup in this model to determine the kind of adopter:   

    to setup-people   
       cct-people initial-potentials [  
         set total-population initial-potentials + total-population  
         setup-people-parms  
         setup-potential  
         set kind determine-kind ( random 100 )  
       ]   
    end  

A good use of the "kind" attribute is to assign different adoption rates based on the kind of person under consideration. 

# CREDITS AND REFERENCES 

## Other Models 

The framework for this model is based on two great models on the Netlogo web site: 

  (a) AIDS - Copyright 1997 Uri Wilensky. All rights reserved. This provided the basic idea of pairing individuals on the same patch. 

  (b) Fitness Landscape - Copyright 2006 David McAvity. This model, from the Community Models section of the Netlogo web site, was created at the Evergeen State College, in Olympia Washington as part of a series of applets to illustrate principles in physics and biology. 

## References 

This model is actually a small portion of an ongoing effort to understand the "Innovation Process" from the invention of ideas and technologies to their development, diffusion, use and obsolescence. The overall model is being developed through System Dynamics, but agent-based modeling has been very helpful in clarifying ideas concerning the competition and spread of technologies. The background for this work comes from three sources: 

  (a) Mahajan, Vijay and Robert A. Peterson.   1985.  MODELS FOR INNOVATION DIFFUSION. Quantitative Applications in the Social Sciences, Sage Publications, 88 pages. 

  (b) Bass, F. M. 1969. "A new product growth model for consumer durables". Management Science 15: 215-227. 

  (c) Rogers, Everett M.   2003.  DIFFUSION OF INNOVATIONS (5th Edition).  Free Press, 512 pp. 

  (d) Henderson, Rebecca M. and Kim B. Clark.   1990. "Architectural Innovation: The Reconfiguration of Existing Product technologies and the Failure of Established Firms."  Administrative Science Quarterly 35:9-30.  

## Version

This is version 23 of innovation.nlogo model, updated to execute under Netlogo 6.2 and 
run in Netlogo Web.

# COPYRIGHT INFORMATION

Copyright 2009-2022 The MITRE Corporation. All rights reserved.

Approved for Public Release: Distribution Unlimited (Tracking #09-1575) 

"The author's affiliation with The MITRE Corporation is provided for identification purposes only, and is not intended to convey or imply MITRE's concurrence with, or support for, the positions, opinions or viewpoints expressed by the author." 

The model may be freely used, modified and redistributed provided the copyright and Public Release statements are included and the resulting models are not used for profit. Contact Michael Samuels at mijujoel@ieee.org if you have questions or comments.
   
  
========================  
Last updated: 2022/01/07 
