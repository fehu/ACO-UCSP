%include subfile-begin.tex

\providecommand{\localSectionCmd}[1]{\section{#1}}
 
%if False
\begin{code}
  
module ACO.UCSP.Test.TestExec1 where

import ACO.UCSP.Definitions
import ACO.UCSP.Implementation
import ACO.UCSP.Test.TestData1

import Data.IORef

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

\red{it doesn't work}
\end{center}

%include subfile-end.tex

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% lhs-showframe-flag: t
%%% eval: (interactive-haskell-mode)
%%% eval: (haskell-indentation-mode)
%%% End:
