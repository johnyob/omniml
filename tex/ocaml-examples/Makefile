SRC := $(wildcard *.ml)
SRC_OUT := $(SRC:.ml=.out)

# We could run
# OCAMLFLAGS = -principal -w +41+18 -warn-error +41+18
# However, we keep the warning information in the source
# and ignore it while producing the PDF.

OCAMLFLAGS = -principal -w +41+18

# A helper for detecting g<util>, else falling back to <util>
findgutil = $(shell command -v g$(1) 2>/dev/null || command -v $(1))

SED := $(call findgutil,sed)
ECHO := $(call findgutil,echo)

default: suspended.ok
all: suspended.err

types.mli: types.ml
	ocamlc -i $< > $@

types.cmi: types.mli
	ocamlc -c types.mli

suspended.cml: ../suspended.tex Makefile
	cat $< | \
	$(SED) -n -e '/\\begin{program}[[].*check.*[]]/,/\\end{program}/p' | \
	$(SED) -e '/\\begin{program}/d' -e '/\\end{program}/d' | \
	$(SED) -e 's/.\\ocamlflags *\([0-9]\)[0-9]. */  (*=\1=*)/' | \
	$(SED) -e 's/[$$]$$//' -e 's/^[$$]//' | \
	$(SED) -e 's/°[^°]*°//' \
	     -e 's/[$$][^$$]*[$$]//' \
	     -e 's/_\([0-9]\)/\1/g' | \
	cat > $@
	echo 'let _end_ = ()' >> $@

suspended.all: suspended.cml Makefile
	cat $< | \
	$(SED) -n -e 's/^let \([a-z][a-z_0-9]*\).*/\1.cml/p' | \
	cat > $@

.PRECIOUS: %.cml
.PRECIOUS: %.cmli

%.cml: suspended.cml Makefile
	echo 'open Types' > $@
	PAT="let $(shell basename $@ .cml) "; \
	cat suspended.cml | \
	$(SED) -e  "0,/^$${PAT}/s/^$${PAT}/#$${PAT}/" | \
	$(SED) -n -e  "/#$${PAT}/,/^let /p" | \
	$(SED) -e  's/^#//' -e  '$$d' | \
	cat >> $@|| (rm $i && false)

CML = $(shell cat suspended.all)

%.cmli: %.cml types.cmi
	# We use a tmp file here to avoid a race condition between grep and ocamlc.
	# Originally, we grep "$@" after redirecting stdout/stderr of ocamlc to "$@".
	# It wasn't necessarily the case that the output redirection would finish
	# before grep was executed, incorrectly labeling files as OK (instead of
	# Warning). 
	@tmp="$@.tmp"; \
	if ocamlc ${OCAMLFLAGS} -i -impl $< >"$$tmp" 2>&1; \
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

%!: %
	cat $<

DR = $(HOME)/ocaml/lang/semiun/su/_build/default/src/main.exe
%.oml: %.cml
	cat types.mli $< | grep -v 'open' > $@

%.omli: %.oml
	$(DR) $<

# all: $(SRC_OUT)

suspended.flags: suspended.all $(CML) $(CML:.cml=.cmli)
	for i in $(CML:.cml=.cmli); \
	do $(ECHO) -n $$i':	 '; tail -1 $$i; done > $@

suspended.err: suspended.flags
	sed -e '/OK.*Green/d' \
	    -e '/Warning.*Orange/d' \
	    -e '/FAILED.*Red/d'\
	< $< > $@

suspended.ok: suspended.err
	if test -s $<; then cat $< && false; else touch $@; fi


all: suspended.err


clean::
	rm -f *.cml*
	rm types.cmi
