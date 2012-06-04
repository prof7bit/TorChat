MPURPLE = $(MAKE) -C src/purple
MGUI = $(MAKE) -C src/gui
MTOOLS = $(MAKE) -C src/tools

all: purple

install: installpurple

purple:
	$(MPURPLE) all
	
gui:
	$(MGUI) all

installpurple:
	$(MPURPLE) install
	
installgui:
	$(MGUI) install
		
clean:
	$(MPURPLE) clean
	$(MGUI) clean
	$(MTOOLS) clean
	$(RM) bin/*.dll 
	$(RM) bin/*.so
	$(RM) bin/*.tar.bz2
	$(RM) bin/torchat
	$(RM) bin/torchat.exe 
	$(RM) bin/*.map 


## this is only for myself for making the
## archives that I upload to github
demozip: clean
	$(MPURPLE) demo
	$(MTOOLS) all
	$(MTOOLS) run
