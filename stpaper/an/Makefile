# Explicit rules

all:    ProbeTrack01.pdf ProbeTrack02.pdf \
	ProbeTrack03-1.pdf ProbeTrack03-2.pdf \
	ProbeTrack06b-1.pdf ProbeTrack06b-2.pdf ProbeTrack06b-3.pdf \
	ProbeTrack09.pdf ProbeTrack10.pdf

ProbeTrack01 = ../../probetrack01
ProbeTrack02 = ../../probetrack02
ProbeTrack03 = ../../probetrack03
ProbeTrack06 = ../../probetrack06b
ProbeTrack09 = ../../probetrack09
ProbeTrack10 = ../../probetrack10

ProbeTrack01.pdf: $(ProbeTrack01)/data02.rda $(ProbeTrack01)/data11.rda
ProbeTrack02.pdf: $(ProbeTrack02)/data02.rda $(ProbeTrack02)/data11.rda
ProbeTrack03-1.pdf: $(ProbeTrack03)/data02.rda $(ProbeTrack03)/data11.rda
ProbeTrack03-2.pdf: $(ProbeTrack03)/data02.rda $(ProbeTrack03)/data12.rda
ProbeTrack06b-1.pdf: $(ProbeTrack06)/data02.rda $(ProbeTrack06)/data11.rda
ProbeTrack06b-2.pdf: $(ProbeTrack06)/data02.rda $(ProbeTrack06)/data11.rda
ProbeTrack06b-3.pdf: $(ProbeTrack06)/data02.rda $(ProbeTrack06)/data11.rda
ProbeTrack09.pdf: $(ProbeTrack09)/data02.rda $(ProbeTrack09)/data11.rda
ProbeTrack10.pdf: $(ProbeTrack10)/data02.rda $(ProbeTrack10)/data11.rda

# Commands and implicit rules

R = /usr/bin/R --slave --no-save --no-restore --no-readline --file=$<

%.rda: %.r; $(R)
%.pdf: %.r; $(R)
%.txt: %.r; $(R)

.PHONY : clean
clean :
	-rm -f ProbeTrack*.pdf

# Local Variables:
# mode:makefile-gmake
# End:
