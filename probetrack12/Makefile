# Explicit rules

all : tab0201.txt \
      tab1001.txt

RAWDATA = rawdata.r data/*
DATA = $(RAWDATA) data00.rda

rawdata.rda : $(RAWDATA)
data00.rda : $(RAWDATA) data00.r
data01.rda : $(DATA) data01.r
data02.rda : $(DATA) data01.rda data02.r
data10.rda : $(DATA) data10.r
tab0201.txt : data02.rda tab0201.r
tab1001.txt : data10.rda tab1001.r

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
