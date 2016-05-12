
\input{header}
%include format.fmt

%if showframe
  \usepackage{showframe}
%endif


\provideboolean{showName}
\setboolean{showName}{false}

\ifthenelse{ \boolean{showName} }
           { \input{private} }
           { \def\myId{\rule{6em}{1pt}}
             \def\myName{Dmitry K.}
           }
\input{Title}
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
\begin{document}
 
\begin{titlepage}
\maketitle
\thispagestyle{empty}
\end{titlepage}


\input{Abstract}
\bigskip
\tableofcontents
\newpage

%include Definitions.lhs
%include Implementation.lhs

\section{Tests}

Here are presented some \emph{ACO} test runs with the following data.


\newcommand{\localSectionCmd}[1]{\subsubsection{#1}}

\subsection{Data \textnumero1}
%include TestData1.lhs

\subsection{Tests Execution}
%include TestExec.lhs





% \section{\red{Questions}}
% \begin{enumerate}
%  \item 
% \end{enumerate}
 
\end{document}

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-copy-literate-src-flag: t
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
