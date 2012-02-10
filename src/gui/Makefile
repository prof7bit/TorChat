PREFIX_DEFAULT = /usr/local
LPR = torchat.lpr
EXE = ../../bin/torchat
LAZ = $(shell which lazbuild)
ASSERT_LAZ = $(if $(LAZ), ,lazbuild_not_found)
PREF = $(if $(PREFIX), $(PREFIX), $(PREFIX_DEFAULT))

all: $(ASSERT_LAZ)
	$(LAZ) $(LPR)
	upx $(EXE)
	@echo "*** $(EXE) successfully built :-)"
	
clean:
	rm -rf lib *.res $(EXE)
	
install:
	@echo "sorry, install not implemented yet"
	@exit 2
##	install -s $(EXE) $(PREF)/bin/
	
uninstall:
	@echo "sorry, uninstall not implemented yet"
	@exit 2
##	rm $(PREF)/bin/$(EXE)
	
lazbuild_not_found:
	@echo "\n\
	*** ERROR \n\
	The lazbuild binary could not be found. In order to build this program\n\
	you need to have the Free Pascal compiler and the LCL units installed.\n\
	On a Debian or Ubuntu system this should be satisfied by\n\
	\n\
	  sudo apt-get install lcl\n\
	\n\
	which in turn would install the compiler along with all required tools,\n\
	units and other dependencies. Note that these are only build dependencies,\n\
	they are not needed to run the compiled program\n\
	"
	@exit 2
