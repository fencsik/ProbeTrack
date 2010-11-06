# Explicit rules

all : tab0201.txt fig0201.pdf fig0202.pdf \
	tab1001.txt fig1001.pdf #\
#	 tab1101.txt fig1101.pdf fig1102.pdf

RAWDATA = rawdata.r data/*
DATA = $(RAWDATA) data00.rda

rawdata.rda : $(RAWDATA)
data00.rda : $(RAWDATA) rawdata.rda data00.r
data01.rda : $(DATA) data01.r
data02.rda : $(DATA) data01.rda data02.r
data10.rda : $(DATA) data10.r
# data11.rda : $(DATA) data10.rda
tab0201.txt : data02.rda tab0201.r
fig0201.pdf : data02.rda fig0201.r
fig0202.pdf : data02.rda fig0202.r
tab1001.txt : data10.rda tab1001.r
fig1001.pdf : data10.rda fig1001.r ../probetrack02/data10.rda
# tab1101.txt : data11.rda tab1101.r
# fig1101.pdf : data11.rda fig1101.r
# fig1102.pdf : data11.rda fig1102.r

# Commands and implicit rules

R = /usr/bin/R --slave --no-save --no-restore --no-readline --file=$<

%.rda: %.r; $(R)
%.pdf: %.r; $(R)
%.txt: %.r; $(R)

.PHONY : clean
clean :
	-rm -f *.rda fig*.pdf tab*.txt

# Local Variables:
# mode:makefile-gmake
# End:
