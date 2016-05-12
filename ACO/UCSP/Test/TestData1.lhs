%include subfile-begin.tex

\providecommand{\localSectionCmd}[1]{\section{#1}}
 
%if False
\begin{code}

module ACO.UCSP.Test.TestData1 where

import ACO.UCSP.Definitions
import ACO.UCSP.Implementation

import Data.Set (Set)
import qualified Data.Set as Set


set :: (Ord a) => [a] -> Set a
set = Set.fromList

\end{code}
%endif

\localSectionCmd{Disciplines}

Here are defined some 10 disciplines.

%{
%format $ = "{}"
%format set = "{}"
\begin{code}

labComputers    = Requirement "Computer Lab"
labElectronics  = Requirement "Electronics Lab"

dA1  = Discipline "A1" 60 Set.empty
dA2  = Discipline "A2" 60 Set.empty
dA3  = Discipline "A3" 60 Set.empty
dA4  = Discipline "A4" 60 Set.empty
                   
dB1  = Discipline "B1" 30 Set.empty
dB2  = Discipline "B2" 30 Set.empty
dB3  = Discipline "B3" 90 Set.empty

dC1  = Discipline "C1" 60 $ set [labComputers]
dC2  = Discipline "C2" 60 $ set [labComputers]
dC3  = Discipline "C3" 60 $ set [labElectronics]

dataDisciplines =  [  dA1, dA2, dA3, dA4
                   ,  dB1, dB2, dB3
                   ,  dC1, dC2, dC3
                   ]

\end{code}
%}


 
\localSectionCmd{Groups}

 
Here are defined some 7 groups.

%{
%format $ = "{}"
%format set = "{}"
\begin{code}

dataGroups =  [  Group "1-1"  20 $ set [dA1, dA2, dC2]
              ,  Group "1-2"  18 $ set [dA1, dA2, dC2]
              ,  Group "2-1"  14 $ set [dA1, dA3, dC3, dB1]
              ,  Group "2-2"  15 $ set [dA1, dA3, dC3, dB2]
              ,  Group "3-1"  12 $ set [dC1, dA1, dA2]
              ,  Group "3-2"  11 $ set [dC1, dA1, dA2]
              ,  Group "4"    40 $ set [dB3, dA4]
              ]

instance HasDomain GroupsData Group
  where  domainPower _ = length dataGroups
         domain _ = Set.fromList dataGroups
  
\end{code}
%}

 
\localSectionCmd{Professors}

Here are defined some 4 professors.

%{
%format $ = "{}"
%format set = "{}"
\begin{code}

dataProfessors =  [  Professor "01" $ set [dA1, dA2, dC1, dC2]
                  ,  Professor "02" $ set [dA2, dA3, dC2, dC3]
                  ,  Professor "03" $ set [dA1, dA2, dA3, dA4]
                  ,  Professor "04" $ set [dB1, dB2, dB3] 
                  ]

instance HasDomain (Role' Professors) Professor
  where  domainPower _ = length dataProfessors
         domain _ = Set.fromList dataProfessors
  
\end{code}
%}


\localSectionCmd{Classrooms}

Here are defined some 5 classrooms.
 
\begin{code}

dataRooms =  [  Classroom "A01" 20 Set.empty
             ,  Classroom "A02" 20 Set.empty
             ,  Classroom "LC1" 20 $ set [labComputers]
             ,  Classroom "LE1" 15 $ set [labElectronics]
             ,  Classroom "B01" 50 Set.empty
             ]

instance HasDomain (Role' Classrooms) Classroom
  where  domainPower _ = length dataRooms
         domain _ = Set.fromList dataRooms
  
\end{code}



 
%include subfile-end.tex

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% lhs-showframe-flag: t
%%% eval: (interactive-haskell-mode)
%%% eval: (haskell-indentation-mode)
%%% End:
