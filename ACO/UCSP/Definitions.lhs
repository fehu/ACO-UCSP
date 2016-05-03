\documentclass{article}

%include polycode.fmt

 % format family = "\textbf{family}"
 % include forall.fmt

\usepackage[english]{babel}
\usepackage{tikz, caption}
\usepackage[inline, shortlabels]{enumitem}
\usetikzlibrary{calc}


%\newcommand{\abstract}{\textbf{\textit{\large{Abstract}}}}
 
%if False
\begin{code}

module ACO.UCSP.Definitions where



\end{code}
%endif

\begin{document}

\abstract

The \emph{University Classes Schedule Problem} (\textbf{UCSP})
consists of finding all the \emph{required disciplines} for each \emph{group}
at some academic period. It doesn't really matter whether the
\emph{disciplines} are chosen by the students or assigned by the
institution. Anyway, the \textbf{primary task} for the ``ants'' is to encounter
\textbf{valid} configurations of \emph{classes}, such that provide exactly
the \emph{required time} of each \emph{required discipline} for each \emph{group}.
The \textbf{secondary task} is to encounter the solution, that provides the best
\emph{satisfaction} by the represented persons and the institution.

\section{Problem Graph}

\begin{figure}[h]
  \centering
  \fbox{\input{Graph.tikz}} 
  \caption{Problem graph schematic, representing \textbf{G}roups,
    \textbf{D}isciplines, \textbf{T}ime/day, \textbf{P}rofessors,
     Class\textbf{R}ooms.}
  \label{fig:graph}
\end{figure}
 
\subsection{Classes}
 
A \emph{class} is an event, that links together the following
types of entities, denoted as \emph{roles}:
\begin{enumerate}
 \item groups
 \item disciplines
 \item day/time
 \item professors
 \item classrooms
\end{enumerate}

Each of the roles must have a finite domain, therefore ensuring
finite number of unique permutations.
 
\noindent
``A class of \emph{discipline} was assigned for \emph{group}.
  It's taught by \emph{professor} in \emph{classroom} at \emph{time}
  on \emph{day}.''

\subsection{Graph Nodes}

The problem graph nodes are \underline{different} \emph{permutations} of
\emph{role domains}. They are grouped into \emph{layers},
depending on the corresponding \emph{role}.



\begin{code}

data Role = Groups | Disciplines | Time | Professors | Classrooms


            
\end{code}


 
\end{document}



%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% eval: (haskell-indentation-mode)
%%% eval: (when (not (haskell-session-maybe)) (haskell-session-change))
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
