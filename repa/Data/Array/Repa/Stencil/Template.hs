{-# LANGUAGE TemplateHaskell, QuasiQuotes, ParallelListComp #-}

-- | Template
module Data.Array.Repa.Stencil.Template
        (stencil2)
where
import Data.Array.Repa.Index
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.List      as List

-- | QuasiQuoter for producing a static stencil defintion.
--
--   A definition like
--
--   @
--     [stencil2|  0 1 0
--                 1 0 1
--                 0 1 0 |]
--   @
--
--   Is converted to:
--
--   @
--     makeStencil2 (Z:.3:.3)
--        (\\ix -> case ix of
--                  Z :. -1 :.  0  -> Just 1
--                  Z :.  0 :. -1  -> Just 1
--                  Z :.  0 :.  1  -> Just 1
--                  Z :.  1 :.  0  -> Just 1
--                  _              -> Nothing)
--   @
--
stencil2 :: QuasiQuoter
stencil2 = QuasiQuoter
                { quoteExp      = parseStencil2
                , quotePat      = undefined
                , quoteType     = undefined
                , quoteDec      = undefined }


-- | Parse a stencil definition.
--   TODO: make this more robust.
parseStencil2 :: String -> Q Exp
parseStencil2 str
 = let
        -- Determine the extent of the stencil based on the layout.
        -- TODO: make this more robust. In particular, handle blank
        --       lines at the start of the definition.
        line1 : _       = lines str
        sizeX           = fromIntegral $ length $ lines str
        sizeY           = fromIntegral $ length $ words line1

        -- TODO: this probably doesn't work for stencils who's extents are even.
        minX            = negate (sizeX `div` 2)
        minY            = negate (sizeY `div` 2)
        maxX            = sizeX `div` 2
        maxY            = sizeY `div` 2

        -- List of coefficients for the stencil.
        coeffs          = (List.map read $ words str) :: [Integer]

   in   makeStencil2' sizeX sizeY
         $ filter (\(_, _, v) -> v /= 0)
         $ [ (fromIntegral y, fromIntegral x, fromIntegral v)
                | y     <- [minX, minX + (1 :: Integer) .. maxX]
                , x     <- [minY, minY + (1 :: Integer) .. maxY]
                | v     <- coeffs ]


makeStencil2'
        :: Integer -> Integer
        -> [(Integer, Integer, Integer)]
        -> Q Exp

makeStencil2' sizeX sizeY coeffs
 = do   ix'             <- newName "ix"
        z'              <- [p| Z |]
        coeffs'         <- newName "coeffs"

        let fnCoeffs
                = LamE  [VarP ix']
                $ CaseE (VarE (mkName "ix"))
                $   [ Match     (InfixP (InfixP z' (mkName ":.") (LitP (IntegerL oy)))
                                        (mkName ":.") (LitP (IntegerL ox)))
                                (NormalB $ ConE (mkName "Just") `AppE` LitE (IntegerL v))
                                [] | (oy, ox, v) <- coeffs ]
                    ++ [Match WildP
                                (NormalB $ ConE (mkName "Nothing")) []]

        return
         $ AppE (VarE (mkName "makeStencil2") 
                        `AppE` (LitE (IntegerL sizeX)) 
                        `AppE` (LitE (IntegerL sizeY)))
         $ LetE [ PragmaD (InlineP (mkName "coeffs") Inline FunLike (BeforePhase 0))
                , ValD    (VarP    coeffs')          (NormalB fnCoeffs) [] ]
                (VarE (mkName "coeffs"))

