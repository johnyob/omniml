MAIN = main

all: $(MAIN).pdf


%.pdf: %.typ
	typst compile $<


clean:
	rm -f $(MAIN).pdf
