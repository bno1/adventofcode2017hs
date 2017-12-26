{-# LANGUAGE FlexibleContexts #-}

module Common.Parsers
    ( parseErrToStr
    , parseIntegral
    ) where

import qualified Text.Parsec as P

parseErrToStr :: Either P.ParseError a -> Either String a
parseErrToStr (Left err) = Left $ show err
parseErrToStr (Right a) = Right a

parseIntegral :: (P.Stream s m Char, Read a, Integral a) => P.ParsecT s u m a
parseIntegral = do
    sign <- P.option '+' $ P.oneOf "+-"
    magnitude <- P.many1 P.digit

    return $ case sign of
        '+' -> read magnitude
        '-' -> -read magnitude
        _ -> undefined

