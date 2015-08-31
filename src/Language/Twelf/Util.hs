module Language.Twelf.Util where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E

import Language.Twelf.Parser
import Language.Twelf.AST

parseFiles :: [String] -> IO [Decl]
parseFiles = go (initParserState, [])
  where go (_, ds) [] = return ds
        go (st, ds) (file:files) = do
          bs <- BS.readFile file
          case (parseSig st file (E.decodeUtf8 bs)) of
            Left e -> error $ show e
            Right (d, st') -> go (st', ds ++ d) files
