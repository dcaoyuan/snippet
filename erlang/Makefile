APPNAME = ewp
VERSION = 0.1

.PHONY: doc

doc:
	erl -noshell -run edoc_run application "'$(APPNAME)'" '"."' '[{vsn,"$(VERSION)"}, no_packages, {subpackages, true}, {exclude_packages, ["deprecated"]}]'
