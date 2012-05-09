MPURPLE = $(MAKE) -C src/purple
MGUI = $(MAKE) -C src/gui

all: purple gui

install: installpurple installgui

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
	$(RM) bin/*.exe 
	$(RM) bin/*.so
	$(RM) bin/torchat
	
