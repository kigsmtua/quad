module Main where

import RIO
import Core
import qualified Docker

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
    Step
      {
          name = StepName name,
          image = Docker.Image image,
          commands = NonEmpty.Partial.fromList commands
      }

makePipeline :: [Text] -> Pipeline
makePipeline steps =
    Pipeline
     {
         steps = NonEmpty.Partial.fromList steps
     }


testPipeline :: Pipeline
testPipeline = makePipeline
  [
      makeStep "First Step" "ubuntu" ["uname -r", "date"]
      makeStep "Second Step" "ubuntu" ["uname -r", "time"]
  ]

testBuild :: Build
testBuild = Build
  {
      pipeline = testPipeline,
      state = BuildReady,
      completedSteps = mempty -- Does this even come across as some all round works ?
  }

main :: IO()
main = pure()