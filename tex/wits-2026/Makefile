.PHONY: all
all: slides.html

.PHONY: present
present: clean slides.html
	open slides.html

slides.html: slides.md omniml.css
	slipshow compile slides.md --theme omniml.css --mathjax https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js

clean:
	rm slides.html
