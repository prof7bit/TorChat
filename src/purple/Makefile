FPCOPT = \
	-Fu../client \
	-FU./lib \
	-FE../../bin \
	-g- \
	-CX \
	-XX \
	-Xs \
	-O3 \
	$(OPT) \

all:
	mkdir -p lib
	fpc $(FPCOPT) purpletorchat.lpr

install:

clean:
	$(MAKE) -C ../client clean
	$(RM) -r lib
			