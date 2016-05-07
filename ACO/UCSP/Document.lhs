%include format.tex
\input{header}


\usepackage{showframe}


 
\provideboolean{showName}
\setboolean{showName}{false}

\ifthenelse{ \boolean{showName} }
           { \input{private} }
           { \def\myId{\rule{6em}{1pt}}
             \def\myName{Dmitry K.}
           }
\input{title}
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
\begin{document}
 
\begin{titlepage}
\maketitle
\thispagestyle{empty}
\end{titlepage}



\input{abstract}
%include Definitions.lhs
\section{Implementation}
%include Graph.lhs



\section{\red{Questions}}

\begin{enumerate}
 \item Would it be possible to handle (\ref{eq:totalN}) routes?
 \item Is it OK that a broken restriction results in $0$ in (\ref{eq:eta}),
       or should there be a grade of ``validness''?
 \item Is the definition OK in general?
\end{enumerate}
 
\end{document}

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-copy-literate-src-flag: t
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
