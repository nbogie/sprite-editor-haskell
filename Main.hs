module Main where
import Debug.Trace
import Graphics.Gloss
import qualified Data.Map as M
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array
import Data.Time.Clock -- for naming new sprites
import Data.List (nub, foldl')

main = do
  a <- getArgs
  contents <- readFile "sprites.dat"
  let sprites =  readCharsMap (lines contents)

  let step = case a of
              (i:[]) -> (read i) :: Int
              _      -> error "bad args"
  guimain step sprites

colorFor :: Int -> Color
colorFor i = colrs !! (i `mod` (length colrs))
  where colrs = [black, white, green, yellow, red, blue, orange, chartreuse, azure, aquamarine, rose, cyan, magenta]
type MySprite = (String, GridArray)
type GridArray = Array (Int, Int) Int

guimain :: Int -> M.Map String MySprite -> IO ()
guimain step sprites = do
  playIO
          (InWindow "Sprite test" --name of the window
            (700,800) -- initial size of the window
            (0, 0) -- initial position of the window
          )
          backgroundColor   -- background colour
          30 -- number of simulation steps to take for each second of real time
          (initGS sprites) -- the initial world
          (return . drawState) -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          (\i w -> return $ updateGame i w)

data Axis = VertSym | HorizSym | BothSym | NoSym deriving (Show, Eq, Bounded, Ord, Enum) 
incAxis :: Axis -> Axis
incAxis = toEnum . (`mod` (1+ fromEnum (maxBound::Axis))) . (+1). fromEnum

updateGame x gs = incCount gs

incCount gs = gs { frame = frame gs + 1 }

initGS sprs = GS { mirrorAxis = NoSym, frame = 0, cursorPos = (0,0), curSprite = initSprite, sprites = sprs}
initSprite = ("untitled", array ((0,0), (7,7)) [((x,y),0) | x<- [0..7], y <- [0..7]])

backgroundColor = colorSea 

saveSprite :: GS -> IO GS
saveSprite gs = do
  time <- getCurrentTime
  let newName = show $ utctDayTime time
  let spr = renameSprite newName $ curSprite gs
  let newSprites = M.insert newName spr (sprites gs)
  return $ gs { sprites = newSprites}

renameSprite nn (_, ar) = (nn, ar)

switchSprite :: GS -> GS
switchSprite gs = gs { curSprite = head $ M.elems $ sprites gs }
modAxis f gs = gs { mirrorAxis = f $ mirrorAxis gs }
handleInput :: Event -> GS -> IO GS
handleInput (EventKey k Down _ _) gs = handleDown k gs
handleInput _ gs = return gs
handleDown k gs = case k of
  (SpecialKey KeyDown)  -> changeCursor CDown
  (SpecialKey KeyUp)    -> changeCursor CUp
  (SpecialKey KeyLeft)  -> changeCursor CLeft
  (SpecialKey KeyRight) -> changeCursor CRight
  (Char x) -> case x of
     'l' -> return $ switchSprite gs
--      '=' -> darken
--     '-' -> lighten
     'm' -> return $ mirrorSprite gs
     'w' -> return $ wipeSprite gs
     's' -> saveSprite gs
     'a' -> return $ modAxis incAxis gs
     'S' -> (writeSprites $ sprites gs) >> return gs
     other -> return $ if (other:"") `elem` (map show [0..9]) then modColor (read $ other:"") else gs
  where 
    modColor :: Int -> GS
    modColor cIx = modSprite (paintColorAt (mirrorAxis gs) (cursorPos gs) cIx) gs
    mirrorSprite gs = modSprite mirror gs
    changeCursor d = return $ gs { cursorPos = capPos ((0,0), (7,7))$ changePos d $ cursorPos gs } 

wipeSprite gs = modSprite wipe gs
wipe :: MySprite -> MySprite
wipe (n,a) = renameSprite n $ initSprite
mirror :: MySprite -> MySprite
mirror (n, ar) = (n, arflip)
  where arflip = ar // [((x,y), ar ! (7-x, y)) | y <- [0..7], x <- [0..7]]

capPos ((x0,y0), (x1,y1)) (x,y) = (cap x x0 x1, cap y y0 y1)
  where
    cap n low hi | n < low = low
                 | n > hi  = hi
                 | otherwise = n


paintColorAt NoSym p c spr = setColorAt p c spr
paintColorAt axis p c spr = foldl' f spr (mirroredPositions p axis)
  where
    f s pos = setColorAt pos c s
mirroredPositions (x,y) HorizSym = [(x,y), ((7-x), y)]
mirroredPositions (x,y) VertSym = [(x,y), (x, (7-y))]
mirroredPositions (x,y) BothSym = [(x,y), (x, (7-y)), ((7-x), y), ((7-x), (7-y))]

setColorAt :: (Int, Int) -> Int -> MySprite -> MySprite
setColorAt p c (n,ar) = (n, ar // [(p, c)])

data CursorDir = CLeft | CRight | CDown | CUp deriving (Eq, Show)

changePos CLeft (x,y)  = (x-1, y)
changePos CRight (x,y) = (x+1, y)
changePos CDown (x,y)  = (x, y-1)
changePos CUp (x,y)    = (x, y+1)

type SpriteMap = Map String MySprite

data GS = GS { frame :: Int
             , cursorPos :: (Int, Int)
             , curSprite :: MySprite
             , sprites :: SpriteMap
             , mirrorAxis :: Axis
             } deriving (Show, Eq)

modSprite f gs = gs { curSprite = f $ curSprite gs }

drawState :: GS -> Picture
drawState gs = Pictures $ 
   [ translate (200)  (200)  $ drawCanvas (i `div` 20) 20 gs
   , translate (-200) (200)  $ drawAllSprites gs
   , translate (-200) (-200) $ textWithSprites (sprites gs) "HASKELL"
   ,  drawLines colorSeaGlass (-300,0) messages]
   
  where 
    i = frame gs
    messages = ["hello", "world", show i, show $ mirrorAxis gs]
   
drawCanvas _ sz gs = Pictures [Color black $ rectangleWire (8*szf) (8*szf)
                              , drawSprite 0 sz (curSprite gs)
                              ,  drawCursor $ cursorPos gs]
  where szf = fromIntegral sz
                             
drawAllSprites :: GS -> Picture
drawAllSprites gs = drawSpritesAt posns (M.elems $ sprites gs) 6
  where
        posns = [(a-3,b-3) | a <- [0..5], b<-[0..5]]

drawSpritesAt posns sprs sz = Pictures $ map (\((x,y),s) -> translate (x*sprSize + 5) (y*sprSize + 5) $ drawSprite 0 sz s) $ zip posns sprs
  where sprSize = (fromIntegral sz)*8

drawSprite :: Int -> Int -> MySprite -> Picture
drawSprite step size (sprName, spr) = 
  Pictures $ [cubeAt ((x-4,y-4), c) step size | x <- [0..7], y <-[0..7], let c = spr ! (x,y)]

cubeAt ((x,y),cIx) step size = case cIx of
      0 -> Pictures []  
      _ -> translate (f x) (f y) $ cubeSolid c edgeLen
            where c = colorFor $ cIx
                  f n = fromIntegral $ size * (fromIntegral n)
                  edgeLen = round $ (fromIntegral size) * 0.9
tileWidth :: Int
tileWidth = 20

drawCursor (x,y) = Color yellow $ translate (m x) (m y) $ rectangleWire (fi $ tileWidth - 4) (fi $ tileWidth - 4)
  where m = fromIntegral .  (tileWidth *) . (+ (-4))
        fi = fromIntegral
cubeSolid c w =  Rotate 0 $ Pictures [Color black $ rectangleSolid (f w) (f w), Color c $ rectangleSolid (f w2) (f w2)]
  where f  = fromIntegral
        w2 | w < 8 = w - 1
           | otherwise = w - 4
help :: [String]
help = [ "---- Keys -------------"
       , "u - Undo move"
       , "n - New board (limited)" ]


colorSea      = makeColor8 46 90 107 255
colorSeaGlass = makeColor8 163 204 188 255
colorGoldfish = makeColor8 255 147 84 255
colorAnnajak  = makeColor8 252 223 184 255
colorForFish _fc = colorSeaGlass
type UiPosition = (Float, Float)

drawLines :: Color -> (Float, Float) -> [String] -> Picture
drawLines c (x,y) ls = Translate x y $  Color c $
               Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,row) = textAt 10 (row*(-20)) l

textAt x y content = Translate x y (Scale 0.12 0.12 (Text content))

-- color scheme
-- http://www.colourlovers.com/palette/437321/Sweet_penguin
--

textWithSprites :: SpriteMap -> String -> Picture
textWithSprites sprMap msg = drawSpritesAt posns sprs 8
  where
    posns = zip [0..] (cycle [0])
    sprs = map ((sprMap M.!) . (:"")) msg

readCharsMap :: [String] -> M.Map String MySprite
readCharsMap = M.fromList . readChars
readChars :: [String] -> [(String, MySprite)]
readChars [] = []
readChars ls = thing : readChars remainder
  where (thing, remainder) = eat ls
        eat str = (exName $ readChar $ take 9 ls, drop 9 ls)
        exName (n,r) = (n, (n,r))

readChar :: [String] -> MySprite
readChar ls = case ls of
  (name:rest) -> (name, toArr $ map readCharLine rest)
  other       -> error $ "bad char data" ++ (unlines other)
  where
    toArr = array ((0,0), (7,7)) . zip pts . concat 
    pts = [(x,y) | y <- [7,6..0], x <- [0..7]]

readCharLine :: String -> [Int]
readCharLine = map (read . (:""))
writeSprites  :: SpriteMap -> IO () 
writeSprites sprMap = do
  let content = concatMap writeSprite (M.toList sprMap)
  writeFile "sprites.dat" content
  
writeSprite :: (String, MySprite) -> String
writeSprite (name, (_, ar)) = unlines $ name : write ar
   where
   write :: GridArray -> [String]
   write ar = wrapAt 8 [head $ show c | y <- [7,6..0], x <- [0..7], let c = ar ! (x,y)]
   wrapAt n [] = []
   wrapAt n xs = take n xs : wrapAt n (drop n xs)
