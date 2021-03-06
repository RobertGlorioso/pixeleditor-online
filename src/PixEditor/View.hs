{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PixEditor.View where

import qualified Data.Map as M
import qualified Miso.Svg as Svg
import qualified Data.Sequence as S
import GHCJS.Types
import Data.JSString (JSString,unpack,pack)
import Miso
import Miso.Svg hiding (onMouseOver, onMouseUp, onMouseDown, onClick, style_, width_, height_, id_)
import Miso.String hiding (zip, zipWith, length, replicate)
import Safe

import PixEditor.View.Action
import PixEditor.View.Model
import PixEditor.Grid.Data
import PixEditor.Grid.Zipper

viewModel :: Model -> View Action
viewModel (Model (i,j) _ c (a,b) pixelSize l y gridSwitch gridstore title ) =
  div_
  [ width_ "100%",
    toStyle [ 
      ("display","grid")
      ,("grid-template-columns", "280px 270px 1fr 1fr")
      ,("grid-template-rows", "360px 130px 80px 1fr 1fr")
      ,("grid-gap", "1rem")
      ]
  ]
  [ spectrum
  , canvas
  , controls
  , display
  , colors
  , sizing
  , dpad
  , div_ [ ] [ canvas_ [ id_ "hiddenCanvas", hidden_ "true", width_ "500", height_ "500" ] []]
  ]    
  where
    (d,e) = size y

    toStyle = style_ . M.fromList

    spectrum = div_ [toStyle [("grid-row", "1/2"), ("grid-column","1/3")]] [canvas_ [ id_ "spectrum", width_ "550", height_ "360", onLoad Begin, onClick PickSpectrum] []]
    canvas = div_ [toStyle  [ ("grid-row", "1/2"), ("grid-column","3/4"), ("display", "flex"), ("align-items", "center") ] ] [
      svg_ [ toStyle [("border-style","solid"), ("display", "block"), ("margin", "auto")]
           , height_ $ pack (show $ d) <> "0px"
           , width_ $ pack (show $ e) <> "0px"
           ]
      [g_ [] [cell
              (case gridSwitch of
                  FillSwitch -> [onClick $ Fill (s,r)]
                  PaintSwitch -> [onMouseDown $ Selected (s,r)] --, onClick $ SwitchGrid DragSwitch]
                  --DragSwitch -> [Miso.onMouseOver $ Selected (k,j), Miso.onMouseUp $ SwitchGrid PaintSwitch]
                  --NoopSwitch -> []
              )
              (s,r)
              (r*10)
              (s*10)
              ( Just $ y ! (s,r) )
              (if (a,b) == (s,r) then "5" else "1")
             | s <- [0..(d-1)], r <- [0..(e-1)] ]
      ]
      ]               
    cell :: (Show a) => [Attribute Action] --action(s) to do when interacted with
         -> a --an HTML id
         -> Int --horiz pos
         -> Int --vert pos
         -> Maybe JSString --color
         -> MisoString --border width
         -> View Action
    cell s i x y r w =
      rect_ ( s ++ [ x_ (pack $ show x)
            , y_ (pack $ show y)
            , id_ (pack $ show i)
            , width_ "9"
            , height_ "9"
            , fill_ $ maybe "rgba(0,0,0,0)" id r
            , toStyle [
                ("stroke-width", w)
                , ("stroke", "black")
                ] 
            ] ) []

    colorCircle c x y = g_ [] [ellipse_ [
                                  cx_ x
                                  , cy_ y
                                  , onClick (PickColor c)
                                  , style_ $ M.fromList [("fill", c)]
                                  , rx_ "15"
                                  , ry_ "15" ] []
                              ]

    colors = div_ [ toStyle [("grid-row","2/3"),("grid-column","1/2") ] ] [
      svg_ [toStyle [ ("border-style", "solid") ] , height_ "130px", width_ "280px"]
      [colorCircle "rgba(0,255,0,255)" "20" "28"
      , colorCircle "rgba(255,255,0,255)" "60" "24"
      , colorCircle "rgba(255,0,0,255)" "100" "21"
      , colorCircle "rgba(255,0,255,255)" "140" "20"
      , colorCircle "rgba(0,0,255,255)" "180" "21"
      , colorCircle "rgba(0,255,255,255)" "220" "24"
      , colorCircle "rgba(128,128,128,255)" "260" "28"
      , colorCircle c "140" "88"
      
      ]
      
      ]

    dpad = div_ [ toStyle [("grid-row", "2/3"),("grid-column", "3/4")] ] [
      svg_ [ height_ "300px", width_ "300px" ] $
        [ellipse_ [cx_ "170"
                  , cy_ "90"
                  , fill_ "gray"
                  , rx_ "100"
                  , ry_ "80" ] []
        ] ++  zipWith (\(x,y) (m,n) ->
                        ellipse_ [cx_ . pack . show $ x
                                 , cy_ . pack . show $ y
                                 , fill_ "green"
                                 , onMouseDown $ Selected (a + m, b + n)
                                 , rx_ "10"
                                 , ry_ "10"
                                 ] []) ((\x -> (80 * cos x + 170 , 60 * sin x + 90)) <$> [3 * pi / 4, pi .. 3 * pi]) [(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0)]
          
      ]

    
    controls = div_ [ toStyle [("grid-row","3/4"),("grid-column","1/4")] ]
      [input_ [type_ "button", onClick Paint, value_ "Draw"] []
      , input_ [ type_ "button", onClick UpdatePic, value_ "Save"] []
      , input_ [ type_ "text", onInput Rename, id_ "title",  value_ title] []
      , select_ [ onInput Review, value_ "View a drawing" ] (fmap picoption $ M.keys gridstore)
      , input_ [ type_ "file", id_ "fileReader", accept_ "image/*", onChange ReadFile ] []
      , div_ [] [ text "fill" ]
      , let isFill = gridSwitch == FillSwitch in input_ [ type_ "checkbox", onClick (SwitchGrid $ if isFill then PaintSwitch else FillSwitch), checked_ isFill] []
      , input_ [type_ "number", onInput (PickColor . setOpacity c) , value_ ( maybe "0" findOpacity $ cursor y) ] []
      
      ]

    setOpacity x n
      | Miso.String.take 4 x == "rgba" =  ( <> n <> ")" ) . Miso.String.reverse . Miso.String.dropWhile (/=',') . Miso.String.reverse . Miso.String.init $ x
      | True                           =  ( "rgba" <> ) . Miso.String.dropWhile (/='(') . ( <> "," <> n <> ")" ) . Miso.String.init $ x 

    findOpacity x
      | Miso.String.take 4 x == "rgba" = (!!3) . Miso.String.split (==',') . Miso.String.tail . Miso.String.init $ x
      | True                           = "255"
    sizing = div_ [ toStyle [("grid-row", "2/3"),("grid-column","2/3") ] ] [
      input_ [type_ "text", onInput resizeC, value_ (pack $ show e) ] []
      ,input_ [type_ "text", onInput resizeR, value_ (pack $ show d) ] []
      ,input_ [type_ "number", onInput resizeP, value_ (pack $ show pixelSize) ] []
      ]

    resizeP t@(readMay.unpack -> Nothing :: Maybe Int) = Id
    resizeP t@(readMay.unpack -> Just x :: Maybe Int)
      | 15 > x && x > 0 = PixelResize x
      | True = Id

    resizeR t@(readMay.unpack -> Nothing :: Maybe Int) = Id
    resizeR t@(readMay.unpack -> Just x :: Maybe Int)
      | 42 > x && x > 0 = RedrawGrid $ YY $ resize (Y (S.replicate e c) (Just c) 0)  (_unyy y) x
      | True = Id

    resizeC t@(readMay.unpack -> Nothing :: Maybe Int) = Id
    resizeC t@(readMay.unpack -> Just x :: Maybe Int)
      | 42 > x && x > 0 = RedrawGrid $ YY $ fmap (flip (resize c) x) (_unyy y)
      | True = Id

    display = div_ [ toStyle [("grid-row","4/5"),("grid-column","1/4") ] ] [pixelpics $ M.keys gridstore]  

    picoption s = option_ [ value_ s ] [ text s ]
    pixelpics ss = div_ [] $ Prelude.concatMap (\s -> [
      div_ [ ] [text s]
      , canvas_ [ id_ s, width_ (pack $ show (pixelSize*e)), height_ (pack $ show (pixelSize*d))] []
      ]) ss    
    
onChange :: action -> Attribute action
onChange r = on "change" emptyDecoder (const r)
