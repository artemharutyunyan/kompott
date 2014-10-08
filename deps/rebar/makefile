### Local (package specific) variables 
PACKAGE_NAME=rebar
PACKAGE_VERSION=2.0.0
PACKAGE_EXT=tar.gz

include ../makefile.deps

$(CANARY).post-install:
	$(LN) -sf $(PACKAGE_SOURCE_ROOT)/rebar $(ERL_DEPS_TARGET_BIN)/rebar  
	$(TOUCH) $@

$(CANARY).clean:
	$(RM) $(PACKAGE_SOURCE_ROOT)/rebar $(ERL_DEPS_TARGET_BIN)/rebar 
