# A helper for detecting g<util>, else falling back to <util>
findgutil = $(shell command -v g$(1) 2>/dev/null || command -v $(1))
# Prefer gsed/gecho when present
SED := $(call findgutil,sed)
ECHO := $(call findgutil,echo)

SCRIPTS_DIR := scripts
EXTRACT_EXAMPLE := $(SCRIPTS_DIR)/extract_example.sh
EXTRACT_TEX_PROGRAMS := $(SCRIPTS_DIR)/extract_tex_programs.sh
POLYML_PP := $(SCRIPTS_DIR)/polyml_pp.sh

OCAMLC ?= ocamlc

# We could run
# OCAMLFLAGS = -principal -w +41+18 -warn-error +41+18
# However, we keep the warning information in the source
# and ignore it while producing the PDF.
OCAMLFLAGS ?= -principal -w +41+18 -pp $(POLYML_PP)
TEX_SOURCE ?= ../suspended.tex

.PHONY: default all clean
default: suspended.ok
all: suspended.err

clean::
	rm -f *.cml*
	rm prelude.cmi

prelude.cmi: prelude.mli
	$(OCAMLC) -c $<

suspended.cml: $(TEX_SOURCE)
	SED=$(SED) "$(EXTRACT_TEX_PROGRAMS)" "$(TEX_SOURCE)" "$@"

suspended.all: suspended.cml Makefile
	# Explanation: 
	# Match all `let $name` in suspended.cml, echoing `$name.cml`
	cat $< | \
	$(SED) -n -e 's/^let \([a-z][a-z_0-9]*\).*/\1.cml/p' | \
	cat > $@

.PRECIOUS: %.cml
%.cml: suspended.cml
	SED=$(SED) "$(EXTRACT_EXAMPLE)" "Prelude" "$(basename $@)" "$<" > "$@"

.PRECIOUS: %.cmli
%.cmli: %.cml prelude.cmi
	# We use a tmp file here to avoid a race condition between grep and ocamlc.
	# Originally, we grep "$@" after redirecting stdout/stderr of ocamlc to "$@".
	# It wasn't necessarily the case that the output redirection would finish
	# before grep was executed, incorrectly labeling files as OK (instead of
	# Warning). 
	@tmp="$@.tmp"; \
	if $(OCAMLC) ${OCAMLFLAGS} -i -impl $< >"$$tmp" 2>&1; \
	then if grep Warning "$$tmp"; \
		then status="Warning"; \
		else status='     OK'; \
	     fi; \
	else status=' FAILED'; \
	fi; \
	cat "$$tmp" > "$@"; \
	$(ECHO) -n "$$status" >> "$@"; \
	rm -f "$$tmp"

	cat $< | \
	$(SED) -n -e 's/.*=\([0-9]\)=.*/ --  \1/p' | \
	$(SED)  -e 's/0/Green/' -e 's/1/Red/' -e 's/2/Orange/' >> $@

CML := $(shell [ -f suspended.all ] && cat suspended.all)
suspended.flags: suspended.all $(CML) $(CML:.cml=.cmli)
	# Explanation:
	# Iterate through all `cmli` files, output the name + status
	for i in $(CML:.cml=.cmli); \
	do $(ECHO) -n $$i':	 '; tail -1 $$i; done > $@

suspended.err: suspended.flags
	# Delete all OK outputs
	sed -e '/OK.*Green/d' \
	    -e '/Warning.*Orange/d' \
	    -e '/FAILED.*Red/d'\
	< $< > $@

suspended.ok: suspended.err
	if test -s $<; then cat $< && false; else touch $@; fi

