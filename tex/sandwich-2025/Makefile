PRJ = slides

SOURCES = 

all: slides.pdf

%.pdf: %.tex ${SOURCES}
	latexmk -xelatex $<

clean: 
	rm -rf *.toc *.aux *.nav *.snm $(addsuffix .bbl,${PRJ}) *.blg *.log *~* *.bak *.out $(addsuffix .pdf,${PRJ}) cut.sh .latexrun.db* *.fls *.rel _region_.* *.synctex.gz *.fdb_latexmk *.ist *.glo

