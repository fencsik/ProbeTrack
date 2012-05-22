# Explicit rules

all: an0101.txt an0102.txt an0103.txt an0104.txt an0110.txt \
	fig0101.pdf fig0102.pdf fig0103.pdf fig0104.pdf \
	an0201.txt an0202.txt fig0201.pdf fig0202.pdf fig0203.pdf \
	an0301.txt an0302.txt an0303.txt an0304.txt \
	fig0301.pdf fig0302.pdf fig0303.pdf fig0304.pdf \
	an0401.txt an0402.txt an0403.txt fig0401.pdf fig0402.pdf fig0404.pdf \
	an0501.txt an0502.txt fig0501.pdf fig0502.pdf fig0504.pdf \
	an0601.txt an0602.txt fig0601.pdf fig0602.pdf \
	an0801.txt fig0801.pdf fig0802.pdf \
	an0901.txt fig0901.pdf \
	an1001.txt an1002.txt fig1001.pdf fig1002.pdf fig1003.pdf \
	an1101.txt fig1101.pdf fig1102.pdf

rawdata.rda: rawdata.txt rawdata.r
rawdata.txt: ; # override implicit rule for .txt files
data01.rda: rawdata.rda
data02.rda: data01.rda
data03.rda: rawdata.rda
data04.rda: data03.rda
data05.rda: data01.rda
data06.rda: data03.rda
data08.rda: data03.rda
data09.rda: data03.rda
data10.rda: rawdata.rda
data11.rda: data10.rda
an0101.txt: data01.rda
an0102.txt: data01.rda
an0103.txt: data01.rda
an0103.rda: data01.rda
an0104.txt: data01.rda
an0104.rda: data01.rda
an0110.txt: data01.rda
an0201.txt: data02.rda
an0202.txt: data02.rda
an0202.rda: data02.rda
an0301.txt: data03.rda
an0302.txt: data03.rda
an0303.txt: data03.rda
an0303.rda: data03.rda
an0304.txt: data03.rda
an0304.rda: data03.rda
an0401.txt: data04.rda
an0402.txt: data04.rda
an0402.rda: data04.rda
an0403.txt: data04.rda
an0501.txt: data05.rda
an0502.txt: data05.rda
an0502.rda: data05.rda
an0601.txt: data06.rda
an0602.txt: data06.rda
an0602.rda: data06.rda
an0801.txt: data08.rda
an0901.txt: data09.rda
an1001.txt: data10.rda
an1002.txt: data10.rda
an1002.rda: data10.rda
an1101.txt: data11.rda
fig0101.pdf: data01.rda an0103.rda
fig0102.pdf: data01.rda an0104.rda
fig0103.pdf: data01.rda
fig0104.pdf: data01.rda
fig0201.pdf: data02.rda an0202.rda
fig0202.pdf: data02.rda
fig0203.pdf: data02.rda an0202.rda
fig0301.pdf: data03.rda an0303.rda
fig0302.pdf: data03.rda an0304.rda
fig0303.pdf: data03.rda
fig0304.pdf: data03.rda
fig0401.pdf: data04.rda
fig0402.pdf: data04.rda an0402.rda
fig0404.pdf: data04.rda data03.rda
fig0501.pdf: data05.rda
fig0502.pdf: data05.rda an0502.rda
fig0504.pdf: data05.rda data01.rda
fig0601.pdf: data06.rda
fig0602.pdf: data06.rda an0602.rda
fig0801.pdf: data08.rda
fig0802.pdf: data08.rda
fig0901.pdf: data09.rda
fig1001.pdf: data10.rda an1002.rda
fig1002.pdf: data10.rda
fig1003.pdf: data10.rda
fig1101.pdf: data11.rda
fig1102.pdf: data11.rda
weibullTest04.pdf: data03.rda
weibullTest06.pdf: data03.rda

# Commands and implicit rules

R = /usr/bin/R --slave --no-save --no-restore --no-readline --file=$<

%.rda: %.r; $(R)
%.pdf: %.r; $(R)
%.txt: %.r; $(R)

.PHONY: clean
clean:
	-rm -f *.rda fig*.pdf an*.txt

# Local Variables:
# mode:makefile-gmake
# End:
