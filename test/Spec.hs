module Main where

-- Does this even come close to come up and see if this is
-- WHat
import RIO ( Applicative(pure), IO, Text )
import Core
    ( Build(..),
      BuildState(BuildReady),
      Image(Image),
      Pipeline(..),
      Step(..),
      StepName(StepName) )

import qualified RIO.NonEmpty.Partial as NonEmpty.Partial

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
    Step {
        name = StepName name,
        image = Image name,
        commands =  NonEmpty.Partial.fromList commands
    }



makePipeline :: [Step]
makePipeline steps =
    Pipeline {steps = NonEmpty.Partial.fromList steps steps}

testPipeline :: Pipeline
testPipeline = makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]


testBuild :: Build
testBuild = Build
    { pipeline = testPipeline
    , state = BuildReady
    , completedSteps = mempty -- Monoid class and this is what the class looks like
    }

main :: IO()
main = pure()