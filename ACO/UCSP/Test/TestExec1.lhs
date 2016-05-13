%include subfile-begin.tex

%format nClasses = "N_\Sigma"
%format nTime    = "N_T"
%format nProfessors = "N_P"
%format nRooms = "N_R"

%format fact x = x "!"
%format binom (n) (k) = "\dbinom{" n "}{" k "}"

\providecommand{\localSectionCmd}[1]{\section{#1}}

\input{TestEval1.tex}
 
%if False
\begin{code}
  
module ACO.UCSP.Test.TestExec1 where

import ACO.UCSP.Definitions
import ACO.UCSP.Implementation
import ACO.UCSP.Test.TestData1

import Data.IORef

import qualified Data.Set as Set

import System.Random

\end{code}
%endif

\localSectionCmd{Stop Criteria}

The algorithm should stop after given number of iterations run.

\begin{code}
stop n ex = do  n' <- readIORef $ exRuns ex
                putStrLn "Stop?"
                return $ n' >= n
\end{code}
 
\localSectionCmd{ACO Setup}

Here follow different \emph{ACO} parameters configuration parts.

\begin{code}
 
acoParams1 = ParamsACO  {  alpha   = 0.14
                        ,  beta    = 0.16
                        ,  pherQ   = 10
                        ,  pherQ0  = 0.1
                        ,  rho     = 0.5
                        } 

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

mkAcoPop aPop = do  gen  <- newStdGen
                    ref  <- newIORef gen
                    return $ aPop ref

acoPopulation1 = GenPopulation  (domainPower (Role' :: Role' Groups))
                                True
                                

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

obligations =  [  forRole [professorCanTeach]
               ,  forRole [roomSatisfies]
               ]

relations = (,) obligations

aco params preferences = ACO params (relations preferences)




\end{code}



Test params:

\begin{code}
aco' = aco acoParams1 [] <$> mkAcoPop acoPopulation1

exec = newExecACO =<< aco'

run = flip execACO (stop 100) =<< exec
  
\end{code}

\begin{center}  
\red{\Huge FAIL }
\medskip

\red{not enough memory}
\end{center}

\localSectionCmd{\red{Memory usage}}
 
Let's evaluate the number of nodes the problem graph has with
formula \ref{eq:totalN}.

\emph{At the moment the required classes time isn't used,
     and every discipline is assigned one fixed class per group.}

\begin{code}

assessEqual theoretical r =
  let evaluated = domainPower r
  in  if theoretical /= evaluated
      then error $ show theoretical ++ " /= " ++ show evaluated
      else toInteger $ theoretical


nClasses = assessEqual  (sum $ map (Set.size . groupDisciplines) dataGroups)
                        (Role' :: Role' Groups)

nTime = assessEqual  (6 * (22-8) * 2)
                     (Role' :: Role' DayTime)

nProfessors = assessEqual  (length dataProfessors)
                           (Role' :: Role' Professors)

nRooms = assessEqual  (length dataRooms)
                      (Role' :: Role' Classrooms)
                                       
\end{code}

\begin{align*}
 N_\Sigma  &= \NClasses    & % EVALED
 N_T      &= \NTime       & % EVALED
 N_P      &= \NProfessors & % EVALED
 N_R      &= \NRooms        % EVALED
\end{align*}
         
\red{\crule{0.5}}

%{
%format quot (x) (y) = "\dfrac{" x "}{" y "}" 
%format * = "\,"

\begin{code}
fact x = product [1..x]
\end{code}

\begin{code}
binom n k = quot (fact n) (fact k * fact (n-k))
\end{code}

\bigskip
\begin{code}
 
nCombinations = binom (nClasses + nProfessors -1) (nClasses - 1) * binom (nClasses + nRooms -1) (nClasses - 1) * binom (nClasses + nTime -1) (nClasses - 1) * fact nClasses

\end{code}
%}

$$= \NCombinations$$

 
 
%include subfile-end.tex

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% lhs-showframe-flag: t
%%% lhs-exec-before: ("ACO/UCSP/Test/TestEval1")
%%% eval: (interactive-haskell-mode)
%%% eval: (haskell-indentation-mode)
%%% End:
