# Explicit rules

all : tab0201.txt fig0201.pdf fig0202.pdf \
	tab1001.txt fig1001.pdf fig1002.pdf \
	tab1101.txt fig1101.pdf fig1102.pdf

rawdata.rda : data/*
data00.rda : rawdata.rda
data01.rda : data00.rda
data02.rda : data01.rda
data10.rda : data00.rda
data11.rda : data10.rda
tab0201.txt : data02.rda
fig0201.pdf : data02.rda
fig0202.pdf : data02.rda
tab1001.txt : data10.rda
fig1001.pdf : data10.rda
fig1002.pdf : data10.rda
tab1101.txt : data11.rda
fig1101.pdf : data11.rda
fig1102.pdf : data11.rda

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
