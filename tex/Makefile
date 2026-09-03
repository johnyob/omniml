PRJ = suspended suspended.final suspended.draft wits

include Makefile.latex

default: suspended.pdf

draft: suspended.draft.pdf

final: suspended.final.pdf

wits: wits.pdf

wits-final: wits.final.pdf

wits-submit: wits.submit.pdf

toplascl: toplascl.pdf

LATEXDIFF=latexdiff --allow-spaces --type=CCHANGEBAR --config "PICTUREENV=(?:picture|tikzpicture|DIFnomarkup|verbatim|tabular|figure|mathpar|mathparfig|program|thebibliography)[\w\d*@]*" --exclude-safecmd="ocaml" --exclude-textcmd="llabel" --exclude-textcmd="lref"

SED=gsed

toplas-diff.tex: suspended.tex
	cp suspended.tex /tmp/suspended-new.tex
	git show before-toplas-reviews-1:tex/suspended.tex > /tmp/suspended-old.tex
	$(SED) -i 's/\\jobname.cfg/suspended.final.cfg/' /tmp/suspended-{new,old}.tex
# Didier's "\llabel A", "lref C" etc. confuse latexdiff; get rid of them
	$(SED) -i 's/\\llabel \([A-Za-z]*\)/\\llabel{\1}/' /tmp/suspended-{new,old}.tex
	$(SED) -i 's/\\lref \([A-Za-z]*\)/\lref{\1}/' /tmp/suspended-{new,old}.tex
	$(LATEXDIFF) /tmp/suspended-old.tex /tmp/suspended-new.tex > toplas-diff.tex

all: $(addsuffix .pdf,${PRJ})

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
	rm -f toplas-source.zip

clean-alistair: vacuum
	rm -rf *.toc *.aux $(addsuffix .bbl,${PRJ}) *.blg *.log *~* *.bak *.out cut.sh .latexrun.db* *.fls *.rel _region_.* *.synctex.gz *.fdb_latexmk *.ist *.glo

TOPLAS_SOURCES = \
	suspended.final.tex \
	suspended.final.cfg \
	suspended.bib \
	acmart.cls \
	ACM-Reference-Format.bst \
	warnings.ok \
	$(filter-out letterbib.sty,$(wildcard *.sty))

TOPLAS_ZIP_BUILD = $(BUILD)/toplas-zip

toplas-source.zip: Makefile Makefile.latex Makefile.toplas $(TOPLAS_SOURCES) suspended.final.pdf
	rm -rf $(TOPLAS_ZIP_BUILD)
	mkdir -p $(TOPLAS_ZIP_BUILD)
	cp -L Makefile.latex $(TOPLAS_SOURCES) $(TOPLAS_ZIP_BUILD)/
	cp Makefile.toplas $(TOPLAS_ZIP_BUILD)/Makefile
	rm -f $@
	cd $(TOPLAS_ZIP_BUILD) && zip -q ../../$@ *
	rm -rf $(TOPLAS_ZIP_BUILD)
