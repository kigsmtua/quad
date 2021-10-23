module Core where

import RIO
import qualified RIO.Map as Map -- Does this even come here as here as just values come back up for usage
import qualified RIO.List as List

data Pipeline = Pipeline {
    steps:: NonEmpty [Step]
} deriving (Show, Eq)


data Step = Step {
    name:: StepName,
    commands:: NonEmpty [Text],
    image:: Image
} deriving (Show, Eq)

data Build = Build {
    pipeline:: Pipeline,
    state:: BuildState,
    completedSteps:: Map StepName StepResult

} deriving(Show, Eq)

data StepResult = StepFailed ContainerExitCode | StepSucceeded deriving(Eq, Show)


newtype ContainerExitCode = ContainerExitCode Int deriving(Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode exitCode) = exitCode


exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exitCode =
    if exitCodeToInt exitCode == 0 then StepSucceeded
    else StepFailed exitCode


data BuildState = BuildReady | BuildRunning BuildRunningState | BuildFinished BuildResult deriving(Show, Eq)

data BuildRunningState = BuildRunningState { step:: StepName} deriving(Eq, Show)

data BuildResult = BuildSucceeded | BuildFailed deriving(Show, Eq)

newtype StepName = StepName Text deriving(Eq, Show, Ord)

newtype Image = Image Text deriving( Eq, Show)


stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step


imageNameToText :: Image  -> Text
imageNameToText (Image image) = image


-- Maybe and nothing
-- Does this thing comee back up for usage
-- Can this even come back up for usage and using ?
buildHasNextStep :: Build -> Either  BuildResult Step
buildHasNextStep build =
    if allSucceeded
        then case nextStep of
            Just step -> Right step
            Nothing -> Left BuildSucceeded
        else Left BuildFailed
    where
        allSucceeded = List.all (StepSucceeded == ) build.completedSteps -- The completed steps come along here as is
        nextStep = List.find f build.pipeline.steps --- Build success stage
        f steps = not $ Map.member step.name build.completedSteps -- This is not ordered and cant even become here and look at something here and see it


progress:: Build -> IO  Build
progress build =
  case build.state of
    BuildReady ->
        case buildHasNextStep build of
            Left result ->
                pure $ build {state = BuildFinished result}
            Right step ->
                pure $ build {state = BuildRunning BuildRunningState {step = step.name }}
    BuildRunning state -> do
        let exitCode = ContainerExitCode 0
            result = exitCodeToStepResult exitCode
        pure build
          {
            state = BuildReady,
            completedSteps = Map.insert state.step result build.completedSteps
          }

    BuildFinished _ -> -- TODO
      pure  build