
module Data.Repa.Merge.Transition
        (Transition (..))
where
import Data.Repa.Query.Graph


-- | Machine transition.
data Transition lState nStream aExp bVal uVal
        -- | Read from named input stream.
        = TrPull    
        { -- | Read from this input stream.
          trPullInput           :: nStream

          -- | Move to this state if we could get an element.
        , trPullSome            :: lState

          -- | Move to this state if we could not get an element.
        , trPullNone            :: lState }


        -- | Release a currently held value.
        | TrRelease 
        { -- | Release value pulled from this stream.
          trReleaseInput        :: nStream

          -- | Move to this state after releasing.
        , trReleaseNext         :: lState }


        -- | Close an input.
        | TrClose  
        { trCloseInput          :: nStream
        , trCloseNext           :: lState }

        -- | Output a value.
        | TrOut
        { trOutStream           :: nStream
        , trOutFun              :: Exp aExp bVal uVal
        , trOutNext             :: lState }

        -- | Signal that output to a stream is done.
        | TrDone
        { trDoneStream          :: nStream
        , trDoneNext            :: lState }

        -- | Branch based on a function.
        | TrIf
        { trIfInput             :: nStream
        , trIfFun               :: Exp aExp bVal uVal
        , trIfTrue              :: lState
        , trIfFalse             :: lState }

        -- | Update the state associated with a channel.
        | TrUpdate
        { trUpdateStream        :: nStream
        , trUpdateFun           :: Exp aExp bVal uVal
        , trUpdateNext          :: lState }

        -- | Skip to the next state.
        | TrSkip
        { trSkipNext            :: lState }

        -- | Halt the program.
        | TrHalt
        deriving Show
