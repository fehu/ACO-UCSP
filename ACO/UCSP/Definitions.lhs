%include subfile-begin.tex


\section{Problem}

%if False
\begin{code}

module ACO.UCSP.Definitions where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (permutations)
import Data.Typeable (Typeable)
import Data.Function (on)

\end{code}
%endif



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
 \item group-discipline pairs
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
data Role = Groups | DayTime | Professors | Classrooms deriving Typeable

-- 'Role' kind container
data Role' (r :: Role) = Role' deriving Typeable

\end{code}

\subsection{Graph Nodes}

The problem graph nodes are \underline{different} \emph{permutations} of
\emph{role domains}. They are grouped into \emph{layers},
depending on the corresponding \emph{role}.

The nodes at some layer have exactly the same underlying size and it's the
power of it's domain set.

\begin{code}

type family RoleValue (r :: Role) :: *

newtype Node (r :: Role) = Node (String, [RoleValue r])

nodeId (Node (id,_)) = id

instance Eq   (Node r)  where (==)     = (==)     `on` nodeId
instance Ord  (Node r)  where compare  = compare  `on` nodeId
  
-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

class HasDomain a v | a -> v
  where  domain       :: a -> Set v
         domainPower  :: a -> Int

type RoleDomain r = HasDomain (Role' r ) (RoleValue r)

mkNodes :: RoleDomain r => String ->  Role' r -> [Node r]
mkNodes name  =  map Node
              .  zip (map ((name ++) . show) [1..])
              .  permutations . Set.toList . domain

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


\begin{code}

class (Eq t, Ord t, Enum t, Bounded t) =>
  DiscreteTime t where  timeQuantum  :: t    -> Int

                        toMinutes    :: t    -> Int
                        fromMinutes  :: Int  -> Maybe t

class (DiscreteTime t, Enum d, Bounded d) =>
  Timetable tt t d ev  | tt -> t
                       , tt -> d
                       , tt -> ev
  where  listEvents  :: tt             -> [((d,t), ev)]
         newTTable   :: [((d,t), ev)]  -> tt

         eventsOn    :: tt  -> d       -> [(t,ev)]
         eventsAt    :: tt  -> t       -> [(d,ev)]
         eventAt     :: tt  -> d -> t  -> Maybe ev

\end{code}

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





\section{Formalization}
 \label{sec:formal}


Let's denote
\begin{itemize}[leftmargin=2cm]
 \item[$N_G$ ---] number of groups;
 \item[$N_P$ ---] number of professors;
 \item[$N_R$ ---] number of classrooms;
 \item[$N_D$ ---] number of disciplines;
 \item[$N_T$ ---] number of \emph{time periods} per week:\\
                  $\mathrm{number~of} \mathit{~time~periods} \mathrm{~per~day}
                   \times \mathrm{number~of} \mathit{~days}$;
 \item[$N_d^g$ ---] number of \emph{time periods} of discipline $d$,
                    assigned for group $g$;
   \\
 \item[$G =$] $\lbrace g_i \rbrace_{i=1}^{N_G}$ --- set of groups;
 \item[$D =$] $\lbrace d_i \rbrace_{i=1}^{N_D}$ --- set of disciplines;
 \item[$P =$] $\lbrace p_i \rbrace_{i=1}^{N_P}$ --- set of professors;
 \item[$R =$] $\lbrace r_i \rbrace_{i=1}^{N_R}$ --- set of classrooms;
   \\
 \item[$D_g =$] $\lbrace d ~||~ N_d^g \not= 0 \rbrace_{d \in D}$ --- set of
   disciplines, assigned to group $g$;
 \item[$N_\Sigma =$] $\sum\limits_{g \in G}~\sum\limits_{d \in D_g} N_d^g$ ---
   total number of classes time periods per week.
\end{itemize}


\subsection{Problem Dimensions}

\subsubsection{Groups and Disciplines}

Let $G'$ be a list of pairs $\langle\mathrm{group},\mathrm{discipline}\rangle$
of length $N_\Sigma$, such that
$\forall \langle g,d \rangle \in G' \implies
 \mathrm{count}_{G'}(\langle g,d \rangle) = N_d^g$.
There are $N_\Sigma!$ unique permutations.

% Let $G'$ be a list of groups of length $N_\Sigma$,
% such that $$ \forall g \in G' \implies \mathrm{count}(g) =
% \sum\limits_{d \in D} N_d^g $$

% Unique groups permutations:
% \begin{align*}
%   & \mathlarger{\prod}\limits_{i=1}^{N_G} \dbinom{ N_G - \sum\limits_{j=1}^{i-1} n_j }{n_i} \\
% = & \binom{N}{n_1} \binom{N-n_1}{n_2} \binom{N-n_1-n_2}{n_3}\dots
%     \binom{N-n_1-\dots-n_{N-1}}{n_N}
% \end{align*}
% \qquad where $n_i = \sum\limits_{d \in D} N_d^{g_i}$.

\subsubsection{Professors and Classrooms}

With no optimization applied, exists $\dbinom{N_\Sigma + N - 1}{N_\Sigma - 1}$
(combinations with repetitions), where $N=N_P$ or $N_R$.

Some invalid instances can be discarded, such that, for example, don't have
enough professors capable of teaching some discipline; or classrooms
configurations that won't fit all the students etc.

\subsubsection{Day and Time}

In general case, any day and time may be assigned for any class period,
including repetitions, that yields $\dbinom{N_\Sigma + N_T - 1}{N_\Sigma - 1}$
possible combinations.

This number may be diminished by
\begin{itemize}
 \item joining class periods;
 \item requiring a minimum entropy.
\end{itemize}


\crule{0.75}
\bigskip\noindent
Total combinations (worst case):
\begin{equation}
 \label{eq:totalN}
 \dbinom{N_\Sigma + N_P - 1}{N_\Sigma - 1}
 \dbinom{N_\Sigma + N_R - 1}{N_\Sigma - 1}
 \dbinom{N_\Sigma + N_T - 1}{N_\Sigma - 1}
 N_\Sigma!
\end{equation}




\subsection{Assessing Candidates}
\begin{equation}
 \label{eq:eta}
 \eta = \eta( \lbrace r_i \rbrace_{i=1}^{n-1}, r_n ) =
 \begin{cases}
  0 & \mbox{if }  \text{any restriction is broken} \\
  \mathrm{pref}(\lbrace r_i \rbrace_{i=1}^n) & \mbox{otherwise}
 \end{cases}
\end{equation}

\qquad where $r_i$ is some some sub-route.

\subsubsection{Restrictions}

There are two kinds of restrictions: over \emph{time} and over
\emph{capabilities.}

Time restriction require the schedule to be \emph{time consistent}:
no group, professor and classroom can have two different classes,
assigned at the same day/time. The capabilities represent:
\begin{itemize}[leftmargin=3.5cm]
  \item[Group:] Disciplines needed (searched).
  \item[Professors:] Known disciplines (that can be taught).
  \item[Classrooms:] Special requirements (labs etc.); students capacity.
\end{itemize}

\emph{Note: group capabilities are incorporated into nodes generation.}

\subsubsection{Preferences}

Preferences create an order over \emph{valid candidates}, that permits
the algorithm to optimize them. The preferences might vary for each
entity (group, professor, classroom), but they all must have a form of
function: $$ \mathrm{pref'}[E] : \langle \mathrm{discipline},\mathrm{day/time} \rangle
\mapsto [0,1]$$

The preference value for a \emph{complete route}:
$$\mathrm{pref}(r) =
 \dfrac{\mathrm{pref'}[G](r) + \mathrm{pref'}[P](r) + \mathrm{pref'}[R](r)}
       {3}$$


%include subfile-end.tex

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:
