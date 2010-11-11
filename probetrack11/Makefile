# Explicit rules

all : tab0201.txt fig0201.pdf \
      tab0301.txt fig0301.pdf \
      tab1001.txt fig1001.pdf

rawdata.rda : rawdata.r data/*
data00.rda : rawdata.rda data00.r
data01.rda : data00.rda data01.r

data02.rda : data01.rda data02.r
tab0201.txt : data02.rda tab0201.r
fig0201.pdf : data02.rda fig0201.r

data03.rda : data00.rda data03.r
tab0301.txt : data03.rda tab0301.r
fig0301.pdf : data03.rda fig0301.r

data10.rda : data00.rda data10.r
tab1001.txt : data10.rda tab1001.r
fig1001.pdf : data10.rda fig1001.r

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
