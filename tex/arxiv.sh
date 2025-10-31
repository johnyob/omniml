#!/bin/bash

## Gabriel Scherer's arxival script
#
# This is a script to produce an 'arxiv.zip' archive that is ready for
# arxiv submission, so that re-submitting updated version is very
# easy.
#
# The script needs to be fine-tuned for each paper due to their
# varying dependencies etc.

MAIN=suspended
BIB=suspended

# By convention, we use arxiv/ as working directory,
# and zip it at the end.
#
# Leaving arxiv/ around is convenient as sometimes you want to try to
# build from there. To avoid reproducibility issues, the script fails
# if this directory already exists.

mkdir arxiv || { echo "error, you need to (rmdir arxiv)"; exit 1; }
rm -f arxiv.zip

cp $MAIN.tex arxiv/
cp $MAIN.final.cfg arxiv/$MAIN.cfg
# HACK above: to avoid issues with arxiv's detection and processing,
# of .tex files, we do not want to submit suspended.final.tex that
# inputs suspended.tex, so we only submit suspended.tex, but with the
# .final.cfg configuration -- and we use $MAIN.final.bbl below.

# The arxiv convention is that the submitter runs 'bibtex'
# (or whatever), and they just build from the '.bbl' files
# themselves. I still include the .bib file in the archive for
# reference.
cp $BIB.bib ACM-Reference-Format.bst arxiv/ # (unused) source .bib, for reference
stat _build/$MAIN.final.bbl > /dev/null || { echo "you need to run bibtex first"; exit 1; }
cp _build/$MAIN.final.bbl arxiv/

# Copy all local packages, class files, etc. that are necessary to build.
cp *.sty arxiv/
cp acmart.cls arxiv/

# By convention, I write a script arxiv/build.sh,
# so that (cd arxiv; sh build.sh) should always
# work and produce a PDF. This is useful to check
# that I didn't forget some build dependencies.
(
    echo "pdflatex $MAIN.tex"
    echo "pdflatex $MAIN.tex"
    echo "pdflatex $MAIN.tex"
) > arxiv/build.sh

zip -r arxiv arxiv
echo "feel free to test the packed source in arxiv/, archive is arxiv.zip"
