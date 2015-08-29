{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Twelf.Reconstruct
       ( reconstruct
       ) where

import Control.Monad.Catch
import Control.Monad.Trans
import qualified Data.Text as T

import Language.Twelf.AST
import Language.Twelf.Parser
import Language.Twelf.TwelfServer

reconstructDecl :: (Functor m, MonadIO m) => Decl -> TwelfMonadT m Decl
reconstructDecl d = (either (error . show) fst . parse) . T.pack <$> cmd
    where parse = parseDecl initParserState "stdin"
          cmd   = runTwelfCmd $ "readDecl\n" ++ show d ++ "\n"

reconstruct :: (Functor m, MonadIO m, MonadMask m) => String -> Bool -> [Decl] -> m [Decl]
reconstruct bin debug ds = withTwelfServer bin debug $ do
  _ <- runTwelfCmd "set Print.implicit true"
  mapM reconstruct' ds
    where reconstruct' :: (Functor m, MonadIO m) => Decl -> TwelfMonadT m Decl
          reconstruct' d@(DDecl _ _)         = reconstructDecl d
          reconstruct' d@(DDefn _ _ _ _)     = reconstructDecl d
          reconstruct' d@(DMode _ _)         = reconstructDecl d
          reconstruct' d                     = return d
