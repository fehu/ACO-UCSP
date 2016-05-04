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
\usepackage{tikz, caption, amsmath}
\usepackage[inline, shortlabels]{enumitem}
\usetikzlibrary{calc}


%\newcommand{\abstract}{\textbf{\textit{\large{Abstract}}}}
 
%if False
\begin{code}

module ACO.UCSP.Definitions where

import Data.Set (Set)

-- import TypeNum.Nat
import GHC.TypeLits

\end{code}
%endif


\title{ \input{title} }
\author{ \input {author} }
\date{\today}

 
\begin{document}
\begin{titlepage}
\maketitle
\thispagestyle{empty}
\end{titlepage}
 
\begin{abstract}
  
The \emph{University Classes Schedule Problem} (\textbf{UCSP})
consists of finding all the \emph{required disciplines} for each \emph{group}
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
 
\noindent
``A class of \emph{discipline} was assigned for \emph{group}.
  It's taught by \emph{professor} in \emph{classroom} at \emph{time}
  on \emph{day}.''

\begin{code}

data Role = Groups | Disciplines | Time | Professors | Classrooms

data Role' (r :: Role) = Role'

    
\end{code}

\subsection{Graph Nodes}

The problem graph nodes are \underline{different} \emph{permutations} of
\emph{role domains}. They are grouped into \emph{layers},
depending on the corresponding \emph{role}.

The nodes at some layer have exactly the same underlying size and it's the
power of it's domain set.



\subsection{Graph Edges}

The edges are possible routes, that can be taken by an ``ant''. They connect
nodes, belonging to \emph{different layers}.

\begin{align*}  
   \forall & a \in \mathrm{Layer}_A \\
   \forall & b \in \mathrm{Layer}_B \\
   & \mbox {if } \mathrm{Layer}_A \text{ and } \mathrm{Layer}_B \text{ are neighbors} \\
   &\quad  \exists \mbox{ an edge between } a \text{ and } b.
\end{align*}

A selection of some sub-route, connecting some nodes $A_i$ and $B_j$ (from some layers
$A$ and $B$) means that the ant ``proposes'' a (partial) solution, that is
described by the nodes' underlying values.

\medskip\noindent
A complete route (through all the layers) describes some schedule ---
a \emph{solution candidate}.


\subsection{Assessing Candidates}

$$ \eta = \eta(\lbrace r_i \rbrace_{i=1}^{n-1}, r_n ) =
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

\subsection{Notes}
\begin{itemize}
  \item Each role has a domain of known size.
  \item The relations must respect the powers of nodes underlying domains.
\end{itemize}
 

          
\section{Implementation}

Uses \emph{type-level} natural numbers to denote the domain sizes and
      ensure correct dimensions for all the operations over the domains
      and nodes.
 
\begin{code}

data family IList (n :: Nat) a :: *
-- data instance IList 0 a = INil -- TODO
-- data instance IList (n) a = a :. (IList n a)
 
type (=|) = IList 0

 
class KnownDomain a (len :: Nat) val | a -> val
  where  -- The uniqueness of domain values cannot be checked at
         -- compile time, therefore it's programmer's responsibility to
         -- ensure it.
         domain :: a -> IList len val

--         --------------------------------------------------------

type family RoleValue (r :: Role) :: *
type family RolePower (r :: Role) :: Nat

type RoleDomain (r :: Role) = KnownDomain (Role' r) (RolePower r) (RoleValue r) 


data Node (r :: Role) = Node ()

            
\end{code}

\medskip\noindent
To be completed $\dots$
 
\end{document}



%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% eval: (haskell-indentation-mode)
%%% eval: (when (not (haskell-session-maybe)) (haskell-session-change))
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
