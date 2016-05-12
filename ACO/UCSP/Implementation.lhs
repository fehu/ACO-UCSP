%include subfile-begin.tex

%format assessRoute  = "\eta"
%format assessRoute' = "\eta\prime"
%format pherQ       = "\mathcal{Q}"
%format evalRoute   = "\xi"
%format lastPartPheromone = "\tau"

%format alpha setup = "\alpha"
%format beta  setup = "\beta"

\section{Implementation}

% Here follows most of the program's code.

%if False
\begin{code}

module ACO.UCSP.Implementation where

import ACO.UCSP.Definitions

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (zip4)

import Data.Ix (Ix)
import Data.Function (on)
import Data.Typeable
import Data.Maybe (fromJust, fromMaybe)
import Data.IORef

import Control.Arrow
import Control.Applicative

import GHC.Exts

\end{code}
%endif


\subsection{Entities}
Here follows definition of the input data, as stated in Section~\ref{sec:formal}.

\begin{code}

data Discipline = Dicipline  { disciplineId    :: String
                             , disciplineTime  :: Int
                             , disciplineReqs  :: Set Requirement
                             }

newtype Requirement = Requirement String
  deriving (Show, Eq, Ord)

instance Show  Discipline  where show     = disciplineId
instance Eq    Discipline  where (==)     = (==)     `on` disciplineId
instance Ord   Discipline  where compare  = compare  `on` disciplineId

\end{code}

\bigskip

\begin{code}

data Group = Group  {  groupId           :: String
                    ,  groupSize         :: Int
                    ,  groupDisciplines  :: Set Discipline
                    }

instance Show  Group  where show     = groupId
instance Eq    Group  where (==)     = (==)     `on` groupId
instance Ord   Group  where compare  = compare  `on` groupId

\end{code}

\bigskip

\begin{code}

data Professor = Professor  {  professorId  :: String
                            ,  canTeach     :: Set Discipline
                            }

instance Show  Professor  where show     = professorId
instance Eq    Professor  where (==)     = (==)     `on` professorId
instance Ord   Professor  where compare  = compare  `on` professorId

\end{code}

\bigskip

\begin{code}

data Classroom = Classroom  {  roomId         :: String
                            ,  roomCapacity   :: Int
                            ,  roomEquipment  :: Set Requirement
                            }

instance Show  Classroom  where show     = roomId
instance Eq    Classroom  where (==)     = (==)     `on` roomId
instance Ord   Classroom  where compare  = compare  `on` roomId

\end{code}

\subsubsection{Timetable}

Timetable is defined over \emph{Mon}--\emph{Sat}, from \emph{8:00} till
\emph{22:00} with \emph{30 minutes} discretization.

\begin{code}

newtype Time = Time Int
  deriving (Eq, Ord)

timeQ    = 30
timeMin  = 60 * 8
timeMax  = 60 * 22

timeDMin = 0
timeDMax = (timeMax - timeMin) `quot` timeQ

instance Enum Time where
  fromEnum (Time t) = t
  toEnum i = if   i >= timeDMin
              &&  i <= timeDMax
             then Time i
             else error $ "wrong discrete time: " ++ show i

instance Bounded Time where  minBound  = Time timeDMin
                             maxBound  = Time timeDMax

instance DiscreteTime Time where
  toMinutes (Time t) = timeMin + timeQ * t

  timeQuantum  _  = 30
  fromMinutes  m  = if    m >= timeMin
                      &&  m <= timeMax
                      &&  m `rem` timeQ == 0
                    then Just . Time $ (m - timeMin) `quot` timeQ
                    else Nothing

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

-- redefined 'System.Time.Day' --- no 'Sunday'
data Day  =  Monday | Tuesday | Wednesday
          | Thursday | Friday | Saturday
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

type DaySchedule = Map Time Class
newtype WeekSchedule = WeekSchedule (Map Day DaySchedule)


groupWith' :: (Ord k) => (a -> k) -> (a -> v) -> [a] -> Map k [v]
groupWith' f g es =
  let  groupIn []      = id
       groupIn (x:xs)  = Map.insertWith (++) (f x) [g x]
  in es `groupIn` Map.empty


instance Timetable WeekSchedule Time Day Class where
  listEvents (WeekSchedule ws) = do
    (day, classes)  <- Map.assocs ws
    (time, class')  <- Map.assocs classes
    return ((day,time), class')

  newTTable = WeekSchedule  . Map.map Map.fromList
                            . groupWith'  (fst . fst)
                                          (first snd)

\end{code}

\subsubsection{Classes}

A \emph{Class} entity links a \emph{discipline}, \emph{group},
\emph{professor}, \emph{classroom} and some \emph{day}-\emph{time}.

\begin{code}

data Class = Class  {  classDiscipline  :: Discipline
                    ,  classGroup       :: Group
                    ,  classProfessor   :: Professor
                    ,  classRoom        :: Classroom
                    ,  classDay         :: Day
                    ,  classBegins      :: Time
                    }

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

-- buildclasses  ::  Node DayTime
--               ->  Node Groups
--               ->  Node Professors
--               ->  Node Classrooms
--               ->  [Class]

-- buildClasses (Node dts) (Node grs) (Node prs) (Node crs) =
--   let  l   = length dts
--        ls  = [length grs, length prs, length crs]
--   in  if (l /= ) `any` ls
--       then error $ "wrong dimensions: " ++ show (l:ls)
--       else do  ((d,t), (gr,di), pr, cr) <- zip4 dts grs prs crs
--                return Class  {  classDiscipline  = di
--                              ,  classGroup       = gr
--                              ,  classProfessor   = pr
--                              ,  classRoom        = cr
--                              ,  classDay         = d
--                              ,  classBegins      = t
--                }

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

type instance RoleValue DayTime     = (Day, Time)
type instance RoleValue Groups      = (Group, Discipline)
type instance RoleValue Professors  = Professor
type instance RoleValue Classrooms  = Classroom

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

class RoleExtra (r :: Role) where
  roleIx  :: Role' r -> Int
  mbRole  :: Role' r -> PartClass -> Maybe (RoleValue r)
  classRole  :: Role' r -> Class -> RoleValue r

instance RoleExtra Groups      where  roleIx _     = 0
                                      mbRole _ r   =  (,) <$>
                                                      mbGroup r <*>
                                                      mbDiscipline r
                                      classRole _  =  classGroup &&&
                                                      classDiscipline

instance RoleExtra DayTime     where  roleIx _     = 1
                                      mbRole _     = mbDayTime
                                      classRole _  =  classDay &&&
                                                      classBegins

instance RoleExtra Professors  where  roleIx _     = 2
                                      mbRole _     = mbProfessor
                                      classRole _  = classProfessor

instance RoleExtra Classrooms  where  roleIx _     = 3
                                      mbRole _     = mbRoom
                                      classRole _  = classRoom

\end{code}


Meanwhile a \textbf{PartClass} stands for a partially defined \emph{Class} and
a \emph{Route} --- for a sequence of \emph{PartClasses}.

\begin{code}

data PartClass = PartClass  {  mbDiscipline  :: Maybe Discipline
                            ,  mbGroup       :: Maybe Group
                            ,  mbProfessor   :: Maybe Professor
                            ,  mbRoom        :: Maybe Classroom
                            ,  mbDayTime     :: Maybe (Day, Time)
                            }

toFullClass r = do  di     <- mbDiscipline r
                    g      <- mbGroup r
                    p      <- mbProfessor r
                    cr     <- mbRoom r
                    (d,t)  <- mbDayTime r

                    return $ Class di g p cr d t

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

data Route = Route  {  routeParts      :: [PartClass]
                    ,  hasDisciplines  :: Bool
                    ,  hasGroups       :: Bool
                    ,  hasProfessors   :: Bool
                    ,  hasRooms        :: Bool
                    ,  hasDayTime      :: Bool
                    }

class UpdRoute (r :: Role) where updRoute :: Node r -> Route -> Route

updRoute' upd (Node xs) r =
    do  (pc, x) <- routeParts r `zip` xs
        [upd pc x]

instance UpdRoute Groups where
  updRoute n r = r  {
    hasDisciplines  = True,
    hasGroups       = True,
    routeParts      = updRoute' (\pc (g,d) -> pc  {  mbGroup = Just g
                                                  ,  mbDiscipline = Just d
                                                  }) n r
                    }
instance UpdRoute DayTime where
  updRoute n r = r  {
    hasDayTime = True,
    routeParts = updRoute' (\pc x -> pc { mbDayTime = Just x} ) n r
    }
instance UpdRoute Professors where
  updRoute n r = r  {
    hasProfessors = True,
    routeParts = updRoute' (\pc x -> pc { mbProfessor = Just x} ) n r
    }
instance UpdRoute Classrooms where
  updRoute n r = r  {
    hasRooms = True,
    routeParts = updRoute' (\pc x -> pc { mbRoom = Just x} ) n r
    }

\end{code}

\subsection{Relations}

\subsubsection{Restrictions}

Classes must be \emph{time consistent} for each \emph{group},
\emph{professor} and \emph{classroom}.

\begin{code}


timeConsistent :: Route -> Bool
timeConsistent r =
  let  test :: (Ord a) => (Route -> Bool) -> (PartClass -> a) -> Maybe Bool
       test b sel = if b r  then     timeConsistent' (routeParts r) sel
                                <|>  Just False
                            else Nothing
       bs =  [  test hasGroups mbGroup
             ,  test hasProfessors mbProfessor
             ,  test hasRooms mbRoom
             ]
  in hasDayTime r && fromMaybe False (foldr (<|>) Nothing bs)

timeConsistent' :: (Ord a)  => [PartClass] -> (PartClass -> a)
                            -> Maybe Bool
timeConsistent' pcs select = foldr f Nothing byRole
  where byRole = groupWith select pcs
        f xs acc = (||) <$> acc <*> timeIntersect xs

mbAllJust :: [Maybe a] -> Maybe [a]
mbAllJust l = inner l []
  where  inner (Just x : xs)  acc  = inner xs (x:acc)
         inner []             acc  = Just acc
         inner _              _    = Nothing

timeIntersect :: [PartClass] -> Maybe Bool
timeIntersect = fmap hasRepetitions . mbAllJust . map mbDayTime


hasRepetitions (x:xs)  = x `elem` xs || hasRepetitions xs
hasRepetitions []      = False

\end{code}

Obligations:

\begin{code}

data Obligation (r :: Role) = Obligation {
    obligationName    :: String
  , assessObligation  :: RoleValue r -> PartClass -> Maybe Bool
  }

professorCanTeach :: Obligation Professors
professorCanTeach  = Obligation "Can teach"
                   $ \ p c -> fmap (`elem` canTeach p) (mbDiscipline c)

roomSatisfies :: Obligation Classrooms
roomSatisfies  = Obligation "Room Capacity and Special Requirements"
               $ \ r c  ->  do  gr <- mbGroup c
                                di <- mbDiscipline c

                                return $ roomCapacity r >= groupSize gr
                                       &&  all  (`Set.member` roomEquipment r)
                                                (disciplineReqs di)

\end{code}

\subsubsection{Preferences}

\begin{code}

data Preference (r :: Role) = Preference {
    preferenceName    :: String
  , assessPreference  ::  RoleValue r  -> Discipline
                      ->  (Day,Time)   -> InUnitInterval
  }

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

newtype InUnitInterval = InUnitInterval Float

inUnitInterval n =  if 0 <= n && n <= 1
                    then Just $ InUnitInterval n
                    else Nothing

inUnitInterval' = fromJust . inUnitInterval

fromUnitInterval (InUnitInterval n) = n

\end{code}


\subsubsection{Assessment}

\begin{code}

data ByRole v = forall r . (RoleExtra r) => ByRole  (Role' r) [v r]

type SomeObligations = ByRole Obligation
type SomePreferences = ByRole Preference

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

assessPart  ::  SomeObligations  -> SomePreferences
            ->  PartClass        -> InUnitInterval
assessPart obligations preferences pc =
  inUnitInterval' $  if satisfies obligations
                     then mean $ assess preferences
                     else 0
  where  satisfies (ByRole r os) = case r `mbRole` pc of
           Just rr ->  all  ( fromMaybe False
                            . ($ pc) . ($ rr)
                            .  assessObligation
                            )  os
           Nothing -> True

         mean xs = sum xs / fromIntegral (length xs)
         assess _ = []


assessRoute :: SomeObligations -> SomePreferences -> Route -> InUnitInterval

assessRoute obligations preferences route = undefined
  where isValid = timeConsistent

\end{code}



\begin{spec}
  assessRoute obligations preferences route =
  let satisfies c (ByRole r os) = all  (  ($ c) . ($ r `routeRole` c)
                                       .  assessObligation
                                       )  os
      mean xs = sum xs / fromIntegral (length xs)
      assess (ByRole r ps) c = map  (  fromUnitInterval
                                    .  ($ (classDay c, classBegins c))
                                    .  ($ classDiscipline c)
                                    .  ($ r `routeRole` c)
                                    .  assessPreference
                                    )  ps
  in inUnitInterval' $  if route `satisfies` obligations
                        then mean $ preferences `assess` route
                        else 0
\end{spec}


\subsection{ACO}

\begin{code}

data SetupACO = SetupACO  {  alpha  :: Float
                          ,  beta   :: Float
                          ,  pherQ  :: Float
                          ,  rho    :: Float
                          }

newtype Pheromone = Pheromone Float

data NodesACO = NodesACO ()
type RelationsACO = (SomeObligations, SomePreferences)

data ACO = AO  {  setupACO      :: SetupACO
               ,  relationsACO  :: RelationsACO
               }

\end{code}

\subsubsection{Graph}


The \textbf{problem graph} is defined by the nodes of each \emph{role};
while the edges hold the \emph{pheromone}.
If the memory permits it, the graph should hold all the permutations of
\emph{role}s domains.

\begin{code}

type NodeSet r = Set (Node r)

type NodeKey = (AnyRole, String)
type PheromoneBetween = Map (AnyRole, AnyRole) Pheromone

data Graph = Graph  {  groupsNodes       :: NodeSet Groups
                    ,  temporalNodes     :: NodeSet DayTime
                    ,  professorsNodes   :: NodeSet Professors
                    ,  classroomsNodes   :: NodeSet Classrooms
                    ,  currentPheromone  :: IORef PheromoneBetween
                    }

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

data AnyRole = forall r . (Typeable r, RoleExtra r) => AnyRole (Role' r)
roleIx' (AnyRole r) = roleIx r

instance Eq   AnyRole where (==)     = (==)     `on` roleIx'
instance Ord  AnyRole where compare  = compare  `on` roleIx'

\end{code}

\subsubsection{Evaluation}

Route \emph{probabilistic evaluation} function:
%{
%format ** = "^"
%format *  = "\cdot"

\begin{code}

evalRoutes  ::  ACO -> PheromoneBetween -> [Route]
            ->  [(InUnitInterval, Route)]
evalRoutes aco ph rs  =    first (fromJust . inUnitInterval . (/ psum))
                      <$>  zip ps rs
  where  ps    = map p rs
         psum  = sum ps
         p r   = (lastPartPheromone r) ** alpha setup * (assessRoute' r) ** beta setup
         assessRoute' = fromUnitInterval . uncurry assessRoute (relationsACO aco)
         lastPartPheromone r = undefined -- TODO

\end{code}
%}
%if False
\begin{code}
         setup = setupACO aco
\end{code}
%endif

%include subfile-end.tex

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% lhs-showframe-flag: t
%%% eval: (haskell-indentation-mode)
%%% End: