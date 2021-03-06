%include subfile-begin.tex


%format assessRoute = "\eta"
%format evalRoutes  = "\xi"
%format pheromoneByAnt = "\Delta\tau_r"
%format updatePheromone = "\widetilde{\Delta\tau}"

%format (paramsACO aco) = "setup"

%format alpha  setup = "\alpha"
%format beta   setup = "\beta"

%format (pherQ  setup) = "\mathcal{Q}"
%format (pherQ0 setup) = "\mathcal{Q}_0"
%format (rho    setup) = "\rho"

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
import Data.Maybe
import Data.IORef
import Data.Tuple (swap)

import Control.Arrow
import Control.Applicative
import Control.Monad

import System.Random

import GHC.Exts

\end{code}
%endif


\subsection{Entities}
Here follows definition of the input data, as stated in Section~\ref{sec:formal}.

\begin{code}

data Discipline = Discipline  { disciplineId    :: String
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
timeMax  = 60 * 21 + 30

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

type instance RoleValue DayTime     = (Day, Time)
type instance RoleValue Groups      = (Group, Discipline)
type instance RoleValue Professors  = Professor
type instance RoleValue Classrooms  = Classroom

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

class RoleExtra (r :: Role) where
  roleIx     :: Role' r -> Int
  roleName   :: Role' r -> String
  mbRole     :: Role' r -> PartClass -> Maybe (RoleValue r)
  classRole  :: Role' r -> Class -> RoleValue r

instance RoleExtra Groups      where  roleIx _     = 0
                                      roleName _   = "Groups"
                                      mbRole _ r   =  (,) <$>
                                                      mbGroup r <*>
                                                      mbDiscipline r
                                      classRole _  =  classGroup &&&
                                                      classDiscipline

instance RoleExtra DayTime     where  roleIx _     = 1
                                      roleName _   = "DayTime"
                                      mbRole _     = mbDayTime
                                      classRole _  =  classDay &&&
                                                      classBegins

instance RoleExtra Professors  where  roleIx _     = 2
                                      roleName _   = "Professors"
                                      mbRole _     = mbProfessor
                                      classRole _  = classProfessor

instance RoleExtra Classrooms  where  roleIx _     = 3
                                      roleName _   = "Classrooms"
                                      mbRole _     = mbRoom
                                      classRole _  = classRoom


instance (RoleExtra r) => Show (Role' r) where show = roleName
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

data Route = Route  {  routeParts         :: [PartClass]
                    ,  mbGroupsNode       :: ! (Maybe (Node Groups))
                    ,  mbDayTimeNode      :: ! (Maybe (Node DayTime))
                    ,  mbProfessorsNode   :: ! (Maybe (Node Professors))
                    ,  mbRoomsNode        :: ! (Maybe (Node Classrooms))
                    ,  assessHistory      :: ! [InUnitInterval]
                    }


hasDisciplines  = isJust . mbGroupsNode
hasGroups       = isJust . mbGroupsNode
hasProfessors   = isJust . mbProfessorsNode
hasRooms        = isJust . mbRoomsNode
hasDayTime      = isJust . mbDayTimeNode

emptyRoute = Route [] Nothing Nothing Nothing Nothing []

class UpdRoute (r :: Role) where updRoute :: Node r -> Route -> Route

updRoute' upd (Node (_, xs)) r =
    do  (pc, x) <- routeParts r `zip` xs
        [upd pc x]

instance UpdRoute Groups where
  updRoute n r = r  {
    mbGroupsNode  = Just n,
    routeParts    = updRoute' (\pc (g,d) -> pc  {  mbGroup = Just g
                                                ,  mbDiscipline = Just d
                                                }) n r
                    }
instance UpdRoute DayTime where
  updRoute n r = r  {
    mbDayTimeNode  = Just n,
    routeParts = updRoute' (\pc x -> pc { mbDayTime = Just x} ) n r
    }
instance UpdRoute Professors where
  updRoute n r = r  {
    mbProfessorsNode  = Just n,
    routeParts = updRoute' (\pc x -> pc { mbProfessor = Just x} ) n r
    }
instance UpdRoute Classrooms where
  updRoute n r = r  {
    mbRoomsNode = Just n,
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
timeConsistent' pcs select = foldr f Nothing forRole
  where forRole = groupWith select pcs
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

                                return  $ roomCapacity r >= groupSize gr
                                        && all  (`Set.member` roomEquipment r)
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

data ForRole v = forall r . (RoleExtra r) => ForRole  (Role' r) [v r]

instance Show (ForRole v) where
  show (ForRole r _) = "Data for role " ++ show r

forRole vs = ForRole Role' vs

type SomeObligations = [ForRole Obligation]
type SomePreferences = [ForRole Preference]

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

assessPart  ::  SomeObligations  -> SomePreferences
            ->  PartClass        -> InUnitInterval
assessPart obligations preferences pc =
  inUnitInterval' $  if all satisfies obligations
                     then mean $ concatMap assess preferences
                     else 0
  where  satisfies (ForRole r os) = case r `mbRole` pc of
           Just rr ->  all  ( fromMaybe False
                            . ($ pc) . ($ rr)
                            .  assessObligation
                            )  os
           Nothing -> True
         assess (ForRole r ps) = fromMaybe [] $
           do  dt  <- mbDayTime pc
               di  <- mbDiscipline pc
               rv  <- mbRole r pc
               return $ map  ( fromUnitInterval
                             . ($ (rv,di,dt))
                             . uncurry3
                             . assessPreference
                             ) ps

assessRoute :: SomeObligations -> SomePreferences -> Route -> InUnitInterval

assessRoute obligations preferences route = fromJust . inUnitInterval $
  if timeConsistent route && notElem 0 assessed  then mean assessed
                                                 else 0
  where assessed  =    fromUnitInterval . assessPart obligations preferences
                  <$>  routeParts route

\end{code}



\subsection{ACO}

\begin{code}

data ParamsACO = ParamsACO  {  alpha   :: Float
                            ,  beta    :: Float
                            ,  pherQ   :: Float
                            ,  pherQ0  :: Float
                            ,  rho     :: Float
                            }
  deriving Show

type RelationsACO = (SomeObligations, SomePreferences)


data  PopulationACO = forall gen . RandomGen gen =>
      GenPopulation Int GenUnique (IORef gen)

type GenUnique = Bool

instance Show PopulationACO where
  show (GenPopulation size unique _)  =   "GenPopulation "
                                      ++  show size
                                      ++  if unique  then " unique"
                                                     else ""


data ACO = ACO  {  paramsACO      :: ParamsACO
                ,  relationsACO   :: RelationsACO
                ,  populationACO  :: PopulationACO
                }
  deriving Show

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

newtype Pheromone = Pheromone Float deriving (Show, Eq, Ord)
pheromoneQuantity (Pheromone n) = n

mapPheromone f (Pheromone n) = Pheromone (f n)
mapPheromone2 f (Pheromone x) (Pheromone y) = Pheromone (f x y)

instance Num Pheromone where  (+)  = mapPheromone2 (+)
                              (-)  = mapPheromone2 (-)
                              (*)  = mapPheromone2 (*)

                              abs     = mapPheromone abs
                              signum  = mapPheromone signum
                              fromInteger = Pheromone . fromInteger

\end{code}

\subsubsection{Graph}


The \textbf{problem graph} is defined by the nodes of each \emph{role};
while the edges hold the \emph{pheromone}.
If the memory permits it, the graph should hold all the permutations of
\emph{role}s domains.

\begin{code}

type NodeSet r = Set (Node r)

type PheromoneBetween  = Map (AnyNode, AnyNode) Pheromone
type PheromoneCache    = Map (AnyNode, AnyNode) (IORef Pheromone)

data Graph = Graph  {  groupsNodes      :: NodeSet Groups
                    ,  temporalNodes    :: NodeSet DayTime
                    ,  professorsNodes  :: NodeSet Professors
                    ,  classroomsNodes  :: NodeSet Classrooms
                    ,  pheromoneCache   :: PheromoneCache
                    }

currentPheromone :: Graph -> IO PheromoneBetween
currentPheromone = mapM readIORef . pheromoneCache

phKey (x,y) = (x `min` y, x `max` y)

-- Lazy update
updPheromone  ::  Graph
              ->  (AnyNode, AnyNode)
              ->  (Pheromone -> Pheromone)
              ->  IO ()
updPheromone g k upd =
    case phKey k `Map.lookup` pheromoneCache g of
        Just ref  -> modifyIORef ref upd
        _         -> error $ "no pheromone cache for " ++ show k


data ExecACO = ExecACO  {  exACO    :: ACO
                        ,  exGraph  :: Graph
                        ,  exRuns   :: IORef Int
                        }

-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

data  AnyNode = forall r . (Typeable r, RoleExtra r) =>
      AnyNode (Role' r) (Node r)
nodeRoleIx  (AnyNode r _)  = roleIx r
nodeId'     (AnyNode _ n)  = nodeId n
nodeId'' = nodeRoleIx &&& nodeId'

anyNode n = AnyNode Role' n

instance Eq   AnyNode where (==)     = (==)     `on` nodeId''
instance Ord  AnyNode where compare  = compare  `on` nodeId''

instance Show AnyNode where
    show (AnyNode r n) = "Node-" ++ show r ++ ":" ++ nodeId n


routeNodes :: Route -> [AnyNode]
routeNodes r = mapMaybe ($ r)  [  packNode . mbRoomsNode
                               ,  packNode . mbProfessorsNode
                               ,  packNode . mbDayTimeNode
                               ,  packNode . mbGroupsNode
                               ]
  where  packNode ::  (Typeable r, RoleExtra r) =>
                      Maybe (Node r) -> Maybe AnyNode
         packNode = fmap (AnyNode Role')

\end{code}

\subsubsection{Evaluation}

Route \emph{probabilistic evaluation} function:

%{
%format ** = "^"
%format *  = "\cdot"
%format assessed = "\eta\prime"
%format lastPartPheromone r = "\tau"
\begin{code}

evalRoutes  ::  ACO -> PheromoneBetween -> [Route]
            ->  [(InUnitInterval, Route)]
evalRoutes aco ph rs  =
  first (fromJust . inUnitInterval . (/ psum))
          <$>  zip ps rs'

  where  (rs', ps) = unzip $ map p rs
         psum  = sum ps
         p r   = let (r', assessed) = assessRoute' r
                 in  (r', lastPartPheromone r ** alpha setup * assessed ** beta setup)

         assessRoute' r = let v = uncurry assessRoute (relationsACO aco) r
                          in  (  r { assessHistory = v : assessHistory r }
                              ,  fromUnitInterval v
                              )

         lastPartPheromone r = case routeNodes r of
                x:y:_  ->  maybe (pherQ0 setup) pheromoneQuantity
                           $ phKey (x,y) `Map.lookup` ph
                _      -> pherQ0 setup

\end{code}
%}

%if False
\begin{code}
         setup = paramsACO aco
\end{code}
%endif


Pheromone secretion for each neighboring nodes pair in a route:

\begin{code}

pheromoneByAnt :: ACO -> Route -> [((AnyNode, AnyNode), Pheromone)]
pheromoneByAnt aco r =
    let  edgs  =  lPairs $ routeNodes r
         hist  =  assessHistory r
         w     =  pherQ (paramsACO aco) / sum (map fromUnitInterval hist)
         weight = Pheromone . (* w) . fromUnitInterval
    in  if length edgs /= length hist
        then error "[BUG] wrong assess history length"
        else edgs `zip` map weight hist


lPairs (x0:x1:xs)  = (x0,x1) : lPairs (x1:xs)
lPairs _           = []

\end{code}

Pheromone update (secretion and vaporization):

%{
%format *  = "\cdot"
%format Pheromone rho = rho
%format + = "{\Large +}~~"
% format ph = "\tau_i"
% format  = "\cdot~~"

\begin{code}

updatePheromone :: ExecACO -> [Route] -> IO ()
updatePheromone ExecACO{ exACO=aco, exGraph=graph } rs  =   forM_ rs update
                                                        >>  vaporize
  where  update r = sequence_ $ do  (i,ph) <- pheromoneByAnt aco r
                                    [updPheromone graph i (+ ph * Pheromone (rho (paramsACO aco)))]

         vaporize = sequence_ $ do  ref <-  Map.elems $
                                            pheromoneCache graph
                                    [modifyIORef' ref (* Pheromone (1 - rho (paramsACO aco)))] -- strict

\end{code}
%}


\subsubsection{Execution}

\begin{code}

type StopCriteria = ExecACO -> IO Bool

execACO :: ExecACO -> StopCriteria -> IO [Route]
execACO ex@ExecACO{ exACO=aco, exGraph=graph } stop = result
  where

\end{code}

\begin{enumerate}
\item Generate initial population by selectting randomly some
 nodes at \emph{Group--Discipline} layer:
\label{it:first-exec}
\begin{code}
        initialPopulation = case populationACO aco of
            GenPopulation size unique genRef -> do
                gen <- readIORef genRef
                let  rand' = if unique  then  randChoosesUnique
                                        else  randChoices
                     gdNodes   = Set.toList $ groupsNodes graph
                     (g,rand)  = rand' gen size gdNodes
                putStrLn "got initial sub-routes"
                writeIORef genRef g
                putStrLn "Generated Initial Population" -- DEBUG
                return $ map (`updRoute` emptyRoute) rand
\end{code}

\item $\forall ~\mathrm{layer}, \mathrm{route} ~\mathbf{do:}$
  \begin{enumerate}
    \item Generate all the possible \emph{route} continuations by
      updating the \emph{route} with \emph{layer}'s nodes.
    \item Assess continuations with $\xi$ and select the node according to the
      assessed probabilities.
  \end{enumerate}

\begin{code}
        nextRoute  ::  (UpdRoute r, RandomGen gen) =>
                       PheromoneBetween -> NodeSet r -> gen
                   ->  Route -> (gen, Route)
        nextRoute ph nset gen r =
          let  candidates = (`updRoute` r) <$> Set.toList nset
               evCandidates = evalRoutes aco ph candidates
          in second snd $ randCoiceWithProb gen fst evCandidates

        nextRoutes _ acc _ g [] = (g,acc)
        nextRoutes ph acc nset gen (r:rs) =
          let (g', next) = nextRoute ph nset gen r
          in nextRoutes ph (next:acc) nset g' rs

        routesIO = do  ph  <- currentPheromone graph
                       g0  <- getStdGen
                       putStrLn "calling iiiinialPopulation (IO)"
                       p0  <- initialPopulation
                       putStrLn $ "Got initialPopulation " ++ show (length p0)
                       
                       let  next  :: (RandomGen gen, UpdRoute r) =>
                                     (Graph -> NodeSet r) -> gen
                                  ->  [Route] -> (gen, [Route])
                            next     = nextRoutes ph [] . ($ graph)

                            (g1,p1)  = next temporalNodes g0 p0
                            (g2,p2)  = next professorsNodes g1 p1
                            (g3,p3)  = next classroomsNodes g2 p3

                       setStdGen g3
                       putStrLn "routesIO" -- DEBUG
                       return p3

\end{code}

\item Update pheromone and counter:
\begin{code}

        updateStates rs = do  updatePheromone ex rs
                              exRuns ex `modifyIORef` (+1)

\end{code}

\item \textbf{Return} best routes \textbf{if} \emph{stop criteria} applies,
\textbf{go to \ref{it:first-exec}} otherwise.
\begin{code}
        result = do  putStrLn "calling routesIO"
                     routes <- routesIO
                     putStrLn "Got routes"
                     updateStates routes
                     putStrLn "updated states"

                     stop' <- stop ex
                     if stop'  then  return routes
                               else  execACO ex stop
\end{code}

\end{enumerate}

\crule{0.5}

\begin{code}

randChoice gen xs =
  if null xs  then error "randChoice: empty list"
              else first (xs !!) $ randomR (0,length xs -1) gen

randChoices gen count xs =
  if length xs < count
  then randChoices gen (length xs) xs
  else  swap $ foldr rand ([],gen) [1..count]

        where rand _ (acc,g) = first (:acc) $ randChoice g xs

randUniqueIndices gen count length =
  let  ixSet  = Set.fromList [1..length]
  in if count > length  then  error "randUniqueIndices: count > length"
                        else  inner gen ixSet count []
  where  inner g _ 0 acc  = (g, acc)
         inner g s c acc  =  let  (i, g')  = randomR (0, Set.size s - 1) g
                                  v        = Set.elemAt i s
                                  s'       = Set.delete v s
                             in inner g' s' (c-1) (v:acc)


randChoosesUnique gen count xs  =
  if length xs < count
      then randChoosesUnique gen (length xs) xs
      else second (map (xs !!)) $ randUniqueIndices gen count (length xs)

randCoiceWithProb  ::  (RandomGen gen) =>
                       gen -> (a -> InUnitInterval) -> [a] -> (gen, a)
randCoiceWithProb gen probOf xs =
  let  (r,g') = random gen

       accumulating acc _ [] = acc
       accumulating acc f (x:xs) = case f acc x of
                                     Just acc'  -> accumulating acc' f xs
                                     _          -> acc
       f (p,_) x =  let p' = p + fromUnitInterval (probOf x)
                    in if p' > r  then Nothing
                                  else Just (p',x)
  in (g', snd $ accumulating (0,head xs) f (tail xs))


\end{code}


\subsubsection{Creation}

Here follows creation of an '\emph{ExecACO}' instance.

\begin{code}
newExecACO ::  (  HasDomain GroupsData Group
               ,  RoleDomain DayTime
               ,  RoleDomain Professors
               ,  RoleDomain Classrooms
               ) =>
               ACO -> IO ExecACO
newExecACO aco = do
  putStrLn "newExecACO"
  let  gs  = mkNodes "G" (Role' :: Role' Groups)
       ts  = [] -- mkNodes "T" (Role' :: Role' DayTime)
       ps  = [] -- mkNodes "P" (Role' :: Role' Professors)
       rs  = [] -- mkNodes "R" (Role' :: Role' Classrooms)

       ks  = []

       -- ks  = concat  [  pairs gs ts
       --               ,  pairs ts ps
       --               ,  pairs ps rs
       --               ]  

       pairs xs ys = do  x  <- xs
                         y  <- ys
                         return (anyNode x, anyNode y)

  putStrLn $ "G nodes:" ++ show (length gs)
  putStrLn $ "T nodes:" ++ show (length ts)
  putStrLn $ "P nodes:" ++ show (length ps)
  putStrLn $ "R nodes:" ++ show (length rs)

  putStrLn "generating ks pairs"
  putStrLn $ "ks size = " ++ show (length ks)

  cache <- sequence $ do  k <- ks
                          [ (,) k <$> newIORef (Pheromone (pherQ0 (paramsACO aco))) ]

  let graph = Graph  (Set.fromList gs)
                     (Set.fromList ts)
                     (Set.fromList ps)
                     (Set.fromList rs)
                     (Map.fromList cache)
  countRef <- newIORef 0

  return $ ExecACO aco graph countRef
\end{code}

\medskip\noindent
\emph{Groups} nodes are created respecting groups' disciplines.

\begin{code}

data GroupsData = GroupsData

instance (HasDomain GroupsData Group) =>
  HasDomain (Role' Groups) (Group, Discipline)

  where  domain _  =    Set.unions
                   $    (\g -> Set.map ((,) g) $ groupDisciplines g)
                   <$>  Set.toList (domain GroupsData)
         domainPower = Set.size . domain

\end{code}

\medskip\noindent
\emph{Day--Time} domain is determined by the \emph{timetable}.

\begin{code}

instance HasDomain (Role' DayTime) (Day, Time)
  where  domainPower _ = 6 * (timeDMax + 1)
         domain _ = Set.fromList $ do  d  <- [minBound..]
                                       t  <- [minBound.. maxBound] 
                                       return (d,t)

\end{code}

%include subfile-end.tex

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% lhs-showframe-flag: t
%%% eval: (interactive-haskell-mode)
%%% eval: (haskell-indentation-mode)
%%% End:
