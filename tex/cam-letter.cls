%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cam-letter.cls
%% 
%% Author: Alistair O'Brien, November 2025. 
%%
%% A document class for letters with the University of Cambridge letterhead. 
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Class identification.
%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{cam-letter}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Package options 
%%

\RequirePackage{kvoptions}

% Font sizes
\DeclareVoidOption{10pt}{\PassOptionsToClass{10pt}{letter}}
\DeclareVoidOption{11pt}{\PassOptionsToClass{11pt}{letter}}
\DeclareVoidOption{12pt}{\PassOptionsToClass{12pt}{letter}}

\ProcessKeyvalOptions*


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Using letter class as base.
%%

\LoadClass[letterpaper]{letter}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% For consistent vertical spacing 
%%

\raggedbottom

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Additional packages
%%

\RequirePackage{calc}
\RequirePackage{graphicx}
\RequirePackage{geometry}
\RequirePackage{hyperref}
\RequirePackage{ifthen}
\RequirePackage{lastpage}
\RequirePackage[absolute,overlay]{textpos}

% Configure hyperref for link styling
\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue,
  filecolor=magenta,
  citecolor=green,
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Configure geometry
%%

\geometry{
  paper=letterpaper, 
  lmargin=2.2in,
  rmargin=0.5in,
  marginparwidth=0.3in,
  bottom=0.6in,
  top=1.3in,
  hcentering=false,
  vcentering=false,
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Variable definitions and default values. 
%%

\newcommand{\camname}[1]{\def\camletter@name{#1}}
\newcommand{\camphone}[1]{\def\camletter@phone{#1}}
\newcommand{\camdepartment}[1]{\def\camletter@department{#1}}
\newcommand{\camsubject}[1]{\def\camletter@subject{Subject: #1}}
\newcommand{\camaddress}[1]{\def\camletter@address{#1}}
\newcommand{\camwebsite}[1]{\def\camletter@website{#1}}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Letter header and footer
%%

\def\camletter@header{
  \begin{textblock*}{4in}(0.3in, 0.3in)
    \includegraphics[width=1in]{camshield.pdf}
  \end{textblock*}
}

\def\camletter@footer{
  \begin{textblock*}{4in}(0.33in, \dimexpr\paperheight-\footskip-2in)
    \begin{minipage}[b][2in][b]{\linewidth}
      \raggedright{
      \textbf{UNIVERSITY OF\\CAMBRIDGE}\\
      \vspace{0.3in}
      \fontsize{7.5}{7.5}\selectfont{
      \textbf{\camletter@department}}\\
      \camletter@address\\
      \vspace{0.1in}
      \camletter@phone\\
      \href{https://\camletter@website}{\camletter@website}
      }
    \end{minipage}
  \end{textblock*}
  \begin{textblock*}{0.5in}
    (\dimexpr\paperwidth-0.5in, 
     \dimexpr\paperheight-\footskip-0.5in)
    \begin{minipage}[b][0.5in][b]{\linewidth}
      \thepage
    \end{minipage}
  \end{textblock*}
}


% Redefine the headers and footers
\renewcommand{\ps@plain}{%
  \renewcommand{\@oddhead}{\camletter@header} 
  \renewcommand{\@oddfoot}{\camletter@footer} 
  \renewcommand{\@evenhead}{\camletter@header} 
  \renewcommand{\@evenfoot}{\camletter@footer} 
}

\renewcommand{\ps@empty}{
  \renewcommand{\@oddhead}{\camletter@header} 
  \renewcommand{\@oddfoot}{\camletter@footer} 
  \renewcommand{\@evenhead}{\camletter@header} 
  \renewcommand{\@evenfoot}{\camletter@footer} 
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Letter opening and closing commands
%%

\renewcommand{\opening}[1]
{\thispagestyle{plain}%
  \textbf{\@date}%
  \par\medskip%
  {\raggedright \toname \\ \toaddress \par}%
  \ifx\toaddress\empty \else \medskip \fi%
  \vspace{1\parskip}%
  \ifthenelse%
    {\equal{\camletter@subject}{}}%
    {}%
    {\textbf{\camletter@subject}\par}%
  \vspace{1\parskip}%
  #1%
  \par\nobreak
}

\renewcommand{\closing}[1]
{\par\nobreak%
 \vspace{\parskip}%
 \stopbreaks\noindent%
 \parbox{1.\textwidth}{%
   \raggedright\ignorespaces #1\\[3\medskipamount]%
 }%
 \vfill%
 \fromname%
 \parbox{1.\textwidth}{%
  \raggedright\noindent \camletter@name%
 }%
 \par%
 \vspace{0.2in}
 \fromaddress
 \vspace{-0.2in}
}

\parindent 0pt 
\parskip 10pt plus2pt minus2pt
\pagestyle{empty}
