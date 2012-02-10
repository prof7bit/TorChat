SUBDIRS = src/gui
SUBMAKE = $(foreach DIR, $(SUBDIRS), $(MAKE) $(MAKEFLAGS) -C $(DIR) $(1) &&) exit 0;

all:
	$(call SUBMAKE, all)
	@echo "*** Success! :-)"
	
install:
	$(call SUBMAKE, install)
	
uninstall:
	$(call SUBMAKE, uninstall)
	
clean:
	$(call SUBMAKE, clean)

