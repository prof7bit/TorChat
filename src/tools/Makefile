BINDIR = ../../bin
FPCOPT = \
	-Fu../client \
	-Fu../client/lnet-0.6.5 \
	-FU./lib \
	-FE. \
	-g- \
	-CX \
	-XX \
	-Xs \
	-O3 \
	-k--reduce-memory-overheads \
	-k--no-keep-memory \
	$(OPT) \

all:
	mkdir -p lib
	fpc $(FPCOPT) makezip.lpr

run:
	./makezip

clean:
	$(MAKE) -C ../client clean
	$(RM) -r lib
	$(RM) -r libpurpletorchat
	$(RM) makezip
