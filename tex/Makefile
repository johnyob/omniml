PRJ = suspended suspended.final suspended.draft wits

LATEXMK = latexmk -bibtex -pdf -outdir=_build -auxdir=_build
LATEXMK_WATCH = $(LATEXMK) -pvc -interaction=nonstopmode

SOURCES=$(shell find . -name '*.tex' -or -name '*.bib' -or -name '*.sty' -or -name '*.cfg')

BUILD=_build

default: suspended.pdf

draft: suspended.draft.pdf

final: suspended.final.pdf

wits: wits.pdf

wits-final: wits.final.pdf

wits-submit: wits.submit.pdf

toplascl: toplascl.pdf

LATEXDIFF=latexdiff --allow-spaces --type=CCHANGEBAR --config "PICTUREENV=(?:picture|tikzpicture|DIFnomarkup|verbatim|tabular|figure|mathpar|mathparfig|program|thebibliography)[\w\d*@]*" --exclude-safecmd="ocaml" --exclude-textcmd="llabel" --exclude-textcmd="lref"

toplas-diff.tex: suspended.tex
	cp suspended.tex /tmp/suspended-new.tex
	git show before-toplas-reviews-1:tex/suspended.tex > /tmp/suspended-old.tex
	sed -i 's/\\jobname.cfg/suspended.final.cfg/' /tmp/suspended-{new,old}.tex
	# Didier's "\llabel A", "lref C" etc. confuse latexdiff; get rid of them
	sed -i 's/\\llabel \([A-Za-z]*\)/\\llabel{\1}/' /tmp/suspended-{new,old}.tex
	sed -i 's/\\lref \([A-Za-z]*\)/\lref{\1}/' /tmp/suspended-{new,old}.tex
	$(LATEXDIFF) /tmp/suspended-old.tex /tmp/suspended-new.tex > toplas-diff.tex

all: $(addsuffix .pdf,${PRJ})

%.log: %.pdf
	@cat $(BUILD)/$@ 

%.wlog: %.pdf
	@$(MAKE) $*.log | grep -E '(Warning:|pdfTeX warning|Overfull)' 

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
	$(LATEXMK_WATCH) $<

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
