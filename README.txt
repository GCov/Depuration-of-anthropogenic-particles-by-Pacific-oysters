This repository contains the code and data for the publication:

"Depuration of anthropogenic particles by Pacific oysters (Crassostrea gigas): 
feasibility and efficacy"


Authors:

Garth A. Coverntona, Maggie Dietterle, Christopher M. Pearce, 
Helen J. Gurney-Smith, John F. Dower, Sarah E. Dudas

The code for the data analysis is contained in the "AP Oyster Depuration Project" 
markdown file. There is also a knitted document walking through the analysis, entitled
"AP-Oyster-Depuration-Project.docx".

The figures, as seen in the manuscript, were created using the "Figure for Publication"
r script. Note that this code depends on running the code in the "AP Oyster Depuration 
Project" file first.

Below is a description of the csv files included in the repository, which contain the
data for the analysis. 

algae.csv		The particle data for the algae samples.
algaeblanks.csv		The particle data for the algae procedural blank samples.
depblanks.csv		The particle data for the oyster procedural blank samples.
depwater.csv		The particle data for the water samples.
oysterdep.csv		The particle data for the oyster samples.
waterblanks.csv		The particle data for the water procedural blank samples.

The column names used in the spreadsheets are described below:

Abbreviation		Description
sampleday		The day the sample was collected, with day 0 the first day of the
			experiment
sampledate		The date the sample was collected.
table			An identifier for the different tables in which the tanks were 
			held.
tank			An identifer for each tank whithin each table.
blankrun		An identifier for the batch of samples procedural blanks were run 
			alongside.
width			Oyster shell width (longest dimension).
length			Oyster shell length.
depth			Oyster shell depth.
cont.weight		Dry weight of the beaker without the sample.
cont+dryweight		Dry weight of the beaker and the sample after drying.
dry.weight		Dry weight of the oyster tissue.
shell.dry.weight	Dry weight of the oyster shell.
CI			Oyster condition index.
time.in.oven		Date and time samples went into the drying oven.
time.out.oven		Date and time samples were removed from the drying oven.
dat.filter		The date samples were filtered on.
dat.count		The date particles were counted in a sample.
observer		An identifer for the person counting particles in a sample.
size.cat		The size range of anthropogenic particles.
red.fib			red fibres
yell.fib		yellow fibres
green.fib		green fibres
turq.fib		turquois fibres
blu.fib			blue fibres
purp.fib		purple fibres
gray.fib		gray fibres
clear.fib		clear/transparent fibres
pink.fib		pink fibres
brown.fib		brown fibres
orang.fib		orange fibres	
black.fib		black fibres
red.frag		red fragments
yell.frag		yellow fragments
green.frag		green fragments
turq.frag		turquoise fragments
blu.frag		blue fragments
purp.frag		purple fragments
gray.frag		gray fragments
clear.frag		clear/transparent fragments
pink.frag		pink fragments
brown.frag		brown fragments
orang.frag		orange fragments
black.frag		black fragements
whit.spher		white microspheres
clear.spher		clear/transparent microspheres
	