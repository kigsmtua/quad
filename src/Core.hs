module Core where

import qualified RIO.List as List
import qualified RIO.Map as Map
import RIO
import qualified Docker

data Pipeline = Pipeline {
    steps :: NonEmpty Step
} deriving (Eq,Show)



data Step = Step {
    name :: StepName,
    commands:: NonEmpty Text,
    image :: Docker.Image
} deriving (Eq, Show)


data Build = Build {
    pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult
} deriving(Eq, Show)


data StepResult = StepFailed Docker.ContainerExitCode | StepSucceeded deriving(Eq, Show)

--- DOE
exitcodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitcodeToStepResult exitCode =
    if Docker.exitCodeToInt exitCode ==  0
        then StepSucceeded
        else StepFailed exitCode

data BuildState  = BuildReady | BuildRunning BuildRunningState | BuildFinished BuildResult deriving(Eq, Show)
data BuildResult = BuildSuceeded | BuildFailed deriving(Eq,Show)

data BuildRunningState = BuildRunningState {
    step :: StepName
} deriving (Eq, Show)

newtype StepName = StepName Text  deriving(Eq,Show, Ord)



stepNameToText :: StepName  -> Text
stepNameToText(StepName step) = step



buildHasNextStep :: Build -> Either  BuildResult Step
buildHasNextStep build =
    if allSucceeded
        then case nextStep of
            Just step -> Right step
            Nothing -> Left  BuildSuceeded
        else Left BuildFailed
    where
        --- This is not compiling and this is a problem
        allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
        nextStep = List.find f build.pipeline.steps
        f = not $ Map.member step.name build.completedSteps


progress :: Build -> IO Build
progress build  =
    case build.state of
        BuildReady ->
            case buildHasNextStep build of
                Left result ->
                    pure $ build { state = BuildFinished result}
                Right step -> do
                    let s = BuildRunningState {step = step.name}
                    pure $ build{state = BuildRunning s}
        BuildRunning state  -> do
            --- We need to get the state and status of this project here
            let exitCode = Docker.ContainerExitCode 0
                result = exitcodeToStepResult exitCode
            pure build
                {
                    state = BuildReady,
                    completedSteps = Map.insert state.step result build.completedSteps
                }
        BuildFinished _ ->
            pure build