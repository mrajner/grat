FILES    := $(shell ls *.3.txt)
MANPAGES := $(FILES:.txt=)

A2X := a2x --format=manpage

all: $(MANPAGES)

%.3:%.3.txt
	$(A2X) $(FILES)
