module MySyscall (
    SyscallCode(..),
    execSyscall) where

-- import VM(
--     Context(..),
--     getTrueValueFromParam,
--     )

import VM

data SyscallCode = SCExit
                    | SCEasyPrint
                    deriving (Show, Eq)

callExit :: Maybe Context -> Maybe Context
callExit Nothing = Nothing
callExit (Just ctx) = Just ctx { exit = True }

callEasyPrint :: Maybe Context -> IO()
callEasyPrint ctx = case getTrueValueFromParam ctx (Reg EAX) of
    Nothing -> putStrLn "Register error"
    Just val -> print val

-- | executes a syscall from its code. (use SyscallCode datatype)
execSyscall :: Maybe Context -> SyscallCode -> (Maybe Context, Maybe (IO()))
execSyscall Nothing _ = (Nothing, Nothing)
-- printf
execSyscall (Just ctx) SCEasyPrint = (Just ctx, Just (callEasyPrint (Just ctx)))
-- exit
execSyscall (Just ctx) SCExit = (callExit (Just ctx), Nothing)

