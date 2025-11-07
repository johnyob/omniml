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

all: $(addsuffix .pdf,${PRJ})

%.log: %.pdf
	cat $(BUILD)/$@ | grep -v '\vspace'

%.wlog: %.pdf
	$(MAKE) $*.log | grep -E '(Warning:|pdfTeX warning)' 


.PRECIOUS: %.pdf

%.pdf: %.tex ${SOURCES}
	@mkdir -p $(BUILD)
	$(LATEXMK) $* || (cp $(BUILD)/$@ . && false)
	cp $(BUILD)/$@ .

%.wpdf: %.tex ${SOURCES}
	$(LATEXMK) -pvc $<

%.view: %.pdf
	open $< &

check: ocaml-examples/suspended.ok suspended.tex
	make -C ocaml-examples suspended.ok

clean:
	rm -rf $(BUILD)

cleaner vacuum: clean
	rm -f suspended.pdf
	rm -f suspended.*.pdf
	rm -f wits.pdf
	rm -f wits.final.pdf

clean-alistair: vacuum
	rm -rf arxiv/
	rm -rf *.toc *.aux $(addsuffix .bbl,${PRJ}) *.blg *.log *~* *.bak *.out cut.sh .latexrun.db* *.fls *.rel _region_.* *.synctex.gz *.fdb_latexmk *.ist *.glo
