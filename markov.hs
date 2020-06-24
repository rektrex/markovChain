import System.IO
import qualified Control.Monad.Random as R
import qualified Data.Map as M

type Source = String
type Target = String
type Frequency = Rational

type Transitions = [(Source, Target)]
type TargetF = (Target, Frequency)

type TransitionMatrix = M.Map Source [TargetF]

generateSequence :: (R.MonadRandom m)
                 => TransitionMatrix
                 -> String
                 -> m String
generateSequence tm s
  | not (null s) && last s == '.' = return s
  | otherwise = do
      s' <- R.fromList $ tm M.! s
      ss <- generateSequence tm s'
      return $ if null s
                  then ss
                  else s ++ " " ++ ss

addTargetF :: TargetF -> [TargetF] -> [TargetF]
addTargetF (t, f) ts = case lookup t ts of
                         Nothing -> (t, f) : ts
                         Just n -> (t, n+f) : filter notT ts where
                           notT (r, _) = r /= t

addTargetFs :: [TargetF] -> [TargetF] -> [TargetF]
addTargetFs tsA tsB = foldr addTargetF tsB tsA

transitionsToMatrix :: Transitions -> TransitionMatrix
transitionsToMatrix = foldr insert M.empty
  where
    insert t = M.insertWith addTargetFs (fst t) [(snd t, 1.0)]

getTransitions :: [String] -> Transitions
getTransitions [] = []
getTransitions (s:ss) = zip ("":ws) ws ++ getTransitions ss
  where ws = words s

main :: IO ()
main = do
  sentences <- lines <$> readFile "sentences"
  s <- generateSequence (transitionsToMatrix $ getTransitions sentences) ""
  print s
