\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{uebungszettel}[2013/03/29 LaTeX class]

\LoadClass[a4paper,ngerman]{scrartcl}

\RequirePackage[utf8]{inputenc}
\RequirePackage[ngerman]{babel}
\RequirePackage{amsmath,amsthm,amssymb,amscd,color,graphicx,environ}
\RequirePackage{framed}
\RequirePackage[protrusion=true,expansion=true]{microtype}
\RequirePackage{lmodern}
\RequirePackage{hyperref}
\RequirePackage{float}
\RequirePackage{multicol}

\RequirePackage{geometry}
\geometry{tmargin=2cm,bmargin=2cm,lmargin=3cm,rmargin=3cm}
\pagestyle{empty}

\setlength\parskip{\medskipamount}
\setlength\parindent{0pt}

\setlength{\unitlength}{1cm}

\newif\ifloesungen\loesungenfalse
\newif\ifpruefung\pruefungfalse

\newlength{\titleskip}
\setlength{\titleskip}{1.3em}
\renewcommand{\maketitle}[2]{%
  \ifloesungen\else\thispagestyle{empty}\fi%
\begin{picture}(0,0)
 \put(0,-1){  \vbox{  Mathesch\"ulerzirkel\\
   Universit\"at Augsburg\\
  Schuljahr 2013/2013\\
  #1 }}
 \put(13,-2){\includegraphics[scale=0.10]{gregor} }
\end{picture} \vspace*{1.7cm}
  \begin{center}\Large \textbf{Zirkelzettel vom #2}
  \end{center}
  \vspace{\titleskip}}

\renewcommand*\theenumi{\alph{enumi}}
\renewcommand{\labelenumi}{\theenumi)}

\newlength{\aufgabenskip}
\setlength{\aufgabenskip}{1.0em}
\newcounter{aufgabennummer}
\newenvironment{aufgabe}[1]{
  \addtocounter{aufgabennummer}{1}
  \textbf{Aufgabe \theaufgabennummer.} \emph{#1} \par
}{\vspace{\aufgabenskip}}
\newenvironment{aufgabeE}[1]{\begin{aufgabe}{#1}\begin{enumerate}}{\end{enumerate}\end{aufgabe}}

\definecolor{shadecolor}{rgb}{.93,.93,.93}




\clubpenalty=10000
\widowpenalty=10000
\displaywidowpenalty=10000


