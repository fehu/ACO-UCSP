\documentclass{article}

%include polycode.fmt

%format family = "\textbf{family}"

%format `union` = "\cup"
%format `compare` = "\lesseqqgtr"
%format `member` = "\in"

%format <|> = "\mathrel{\mathord{<}\mathord{|}\mathord{>}}"
%format <$> = "\mathrel{\mathord{<}\mathord{\$}\mathord{>}}"
%format <*> = "\mathrel{\mathord{<}\mathord{*}\mathord{>}}"
%format &&& = "\mathrel{\mathord{\&}\mathord{\&}\mathord{\&}}"


\usepackage[english]{babel}
\usepackage{tikz, caption, amsmath, xcolor, subcaption, graphicx,
            ifthen, pgffor, calc}
\usepackage[inline, shortlabels]{enumitem}
\usepackage[export]{adjustbox}
\usetikzlibrary{calc, chains, fit, positioning, decorations.pathreplacing}


\usepackage{showframe}


\newcommand{\red}[1]{{\color{red} #1}}
\newcommand{\TODO}{\red{\Large TODO}}
 
 
%if False
\begin{code}

module ACO.UCSP.Definitions where

import Data.Set (Set)
import qualified Data.Set as Set 

import Data.List (permutations)

\end{code}
%endif

\provideboolean{showName}
\setboolean{showName}{false}

\ifthenelse{ \boolean{showName} }
           { \input{private} }
           { \def\myId{\rule{6em}{1pt}}
             \def\myName{Dmitry K.}
           }
\input{title}
 
 
\begin{document}
 
\begin{titlepage}
\maketitle
\thispagestyle{empty}
\end{titlepage}

 
\begin{abstract}
  
The \emph{University Classes Schedule Problem} (\textbf{UCSP})
consists in finding all the \emph{required disciplines} for each \emph{group}
at some academic period. It doesn't really matter whether the
\emph{disciplines} are chosen by the students or assigned by the
institution. Anyway, the \textbf{primary task} for the ``ants'' is to encounter
\textbf{valid} configurations of \emph{classes}, such that provide exactly
the \emph{required time} of each \emph{required discipline} for each \emph{group}.
The \textbf{secondary task} is to encounter the solution, that provides the best
\emph{satisfaction} by the represented persons and the institution.
 
\end{abstract}
 
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

Each of the roles must have a finite and non-empty domain, therefore ensuring
finite number of unique permutations.

\begin{figure}[h]
  \centering
  \input{Class.tikz} 
  \caption{\emph{Class} structure.}
  \label{fig:class}
\end{figure}


 
\begin{code}

-- Used as \textbf{kind} (see \emph{data type promotion})
data Role = Groups | Disciplines | Time | Professors | Classrooms

-- 'Role' kind container
data Role' (r :: Role) = Role'

    
\end{code}

\subsection{Graph Nodes}

The problem graph nodes are \underline{different} \emph{permutations} of
\emph{role domains}. They are grouped into \emph{layers},
depending on the corresponding \emph{role}.

The nodes at some layer have exactly the same underlying size and it's the
power of it's domain set.

\begin{code}

type family RoleValue (r :: Role) :: *
 
class HasDomain a v | a -> v
  where  domain       :: a -> Set v
         domainPower  :: a -> Int

newtype Node (r :: Role) = Node [RoleValue r]

mkNodes ::  HasDomain (Role' r) (RoleValue r) =>
            Role' r ->  [Node r]


mkNodes = map Node . permutations . Set.toList . domain

\end{code}


\subsubsection{Timetable}
A \emph{timetable} holds schedule for one week,
that repeats throughout the academic period.
The \emph{timetable} is actually a table:
the columns represent days of week; the rows --- discrete time intervals.
Actual timetable structure may vary, as can be seen in figure
\ref{fig:timetables}.

\begin{figure}[h]
  \centering

  \begin{subfigure}{\textwidth}
    \centering
    \begin{tabular}{||c||c||c||c||c||c||c||}
      \hline
      ~ & Mon & Tue & Wed & Thu & Fri & Sat \\ \hline
      08:30 -- 09:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      09:00 -- 09:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      09:30 -- 10:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:00 -- 10:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:30 -- 11:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      11:00 -- 11:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      11:30 -- 12:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      \vdots \qquad\quad \vdots & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
    \end{tabular}

    \caption{Timetable without recesses.}
  \end{subfigure}

  \begin{subfigure}{\textwidth}
    \centering
    \begin{tabular}{||c||c||c||c||c||c||c||}
      \hline
      ~ & Mon & Tue & Wed & Thu & Fri & Sat \\ \hline
      08:30 -- 09:10 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      09:15 -- 09:55 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:05 -- 10:45 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:50 -- 11:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      11:40 -- 12:20 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      12:25 -- 13:05 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      13:15 -- 13:55 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      \vdots \qquad\quad \vdots & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
    \end{tabular}

    \caption{Timetable with recesses.}
  \end{subfigure}

  \caption{Possible \emph{timetable} structures. }
  \label{fig:timetables}
\end{figure}

 
\subsection{Graph Edges}

The edges are possible routes, that can be taken by an ``ant''. They connect
nodes, belonging to \emph{different layers}.

\begin{align*}  
   \forall & a \in \mathrm{Layer}_A \\
   \forall & b \in \mathrm{Layer}_B \\
   &\mbox {if } \mathrm{Layer}_A \text{ and } \mathrm{Layer}_B \text{ are neighbors} \\
   &\quad \exists \text{ an edge between } a \text{ and } b.
\end{align*}

A selection of some sub-route, connecting some nodes $A_i$ and $B_j$ (from some layers
$A$ and $B$) means that the ant ``proposes'' a (partial) solution, that is
described by the nodes' underlying values. The ``ant'' agent must be capable
of selecting exactly one node of each role. The selection order doesn't matter.


\medskip\noindent
A complete route (through all the layers) describes 
a \emph{solution candidate}: some schedule, that holds a
list of \emph{classes}.

\begin{figure}[h]
  \centering
  \input{Route.tikz}  
  \caption{\emph{Route} decomposition.}
  \label{fig:route}
\end{figure}


\begin{code}
  
\end{code}

\subsection{Assessing Candidates}

$$ \eta = \eta( \lbrace r_i \rbrace_{i=1}^{n-1}, r_n ) =
\begin{cases}
  0 & \mbox{if }  \text{any restriction is broken} \\
  \mathrm{preference(\lbrace r_i \rbrace_{i=1}^n)} & \mbox{otherwise}
\end{cases}
$$
\qquad where $r_i$ is some some sub-route.
 
\subsubsection{Restrictions}

There are two kinds of restrictions: over \emph{time} and over
\emph{capabilities.}

The capabilities represent:
\begin{itemize}[leftmargin=2.5cm]
  \item[Group:] Disciplines needed (searched).
  \item[Professors:] Known disciplines (that can be taught).
  \item[Classrooms:] Special requirements (labs etc.); students capacity.
\end{itemize}

\medskip
Time restriction require the schedule to be \emph{time consistent}:
no group, professor and classroom can have two different classes,
assigned at the same day/time.

\medskip\noindent
To be completed $\dots$

\subsubsection{Preferences}
To be done $\dots$



 

          
% \section{Implementation}


 
% Uses \emph{type-level} natural numbers to denote the domain sizes and
%       ensure correct dimensions for all the operations over the domains
%       and nodes.
 

% type family NatPrev (n :: Nat) :: Maybe Nat
%   where NatPrev 0 = Nothing
%         NatPrev n = Just (n-1)

 
% data family IList' (prev :: Maybe Nat) a :: *
     
% data instance IList' Nothing a   = INil
% data instance IList' (Just n) a  = ISucc a (IList' (NatPrev n) a)

% type IList (n :: Nat) = IList' (NatPrev n)
                                   
% type (=|) = IList 0
% type (:.) a l = ISucc a l



 
% class KnownDomain a (len :: Nat) val | a -> val
%   where  -- The uniqueness of domain values cannot be checked at
%          -- compile time, therefore it's programmer's responsibility to
%          -- ensure it.
%          domain :: a -> IList len val

%         --------------------------------------------------------

% type family RoleValue (r :: Role) :: *
% type family RolePower (r :: Role) :: Nat

% type RoleDomain (r :: Role) = KnownDomain  (Role' r)
%                                            (RolePower r)
%                                            (RoleValue r) 


% data Node (r :: Role) = Node ()

            

% \medskip\noindent
% To be completed $\dots$
 
\end{document}



%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% eval: (haskell-indentation-mode)
%%% eval: (when (not (haskell-session-maybe)) (haskell-session-change))
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
