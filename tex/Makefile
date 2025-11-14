PRJ = suspended suspended.final suspended.draft wits

LATEXMK = latexmk -bibtex -pdf -outdir=_build -auxdir=_build

SOURCES=$(shell find . -name '*.tex' -or -name '*.bib' -or -name '*.sty' -or -name '*.cfg')

BUILD=_build

default: suspended.pdf

draft: suspended.draft.pdf

final: suspended.final.pdf

wits: wits.pdf

wits-final: wits.final.pdf

wits-submit: wits.submit.pdf

toplascl: toplascl.pdf

all: $(addsuffix .pdf,${PRJ})

%.log: %.pdf
	@cat $(BUILD)/$@ 

%.wlog: %.pdf
	@$(MAKE) $*.log | grep -E '(Warning:|pdfTeX warning)' 

%.werr: %.pdf
	@$(MAKE) $*.wlog | grep -v -f warnings.ok

%.ok: %.pdf Makefile
	if $(MAKE) $*.werr | grep -v Font | grep -i 'warning'; then false; fi
	touch $@

.PRECIOUS: %.pdf

%.pdf: %.tex ${SOURCES}
	@mkdir -p $(BUILD)
	$(LATEXMK) $* || (cp $(BUILD)/$@ . && false)
	cp $(BUILD)/$@ .

%.wpdf: %.tex ${SOURCES}
	$(LATEXMK) -pvc $<

%.view: %.pdf
	open $< &

arxiv.zip: suspended.final.ok
	sh arxiv.sh

arxiv.zip.force: arxiv.clean arxiv.zip

arxiv.clean:
	rm -rf arxiv arxiv.zip

check: ocaml-examples/suspended.ok suspended.tex
	make -C ocaml-examples suspended.ok

clean:
	rm -rf $(BUILD)

cleaner vacuum: clean arxiv.clean
	rm -f suspended.pdf
	rm -f suspended.ok
	rm -f suspended.*.pdf
	rm -f wits.pdf
	rm -f wits.final.pdf
	rm -f wits.submit.pdf
	rm -f toplascl.pdf

clean-alistair: vacuum
	rm -rf *.toc *.aux $(addsuffix .bbl,${PRJ}) *.blg *.log *~* *.bak *.out cut.sh .latexrun.db* *.fls *.rel _region_.* *.synctex.gz *.fdb_latexmk *.ist *.glo
