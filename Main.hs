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
import Data.Maybe (fromMaybe)
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

initGS sprs = GS { mirrorAxis = NoSym
, frame = 0
, board = M.empty
, boardCursorPos = (0,0)
, libCursorPos = (0,0)
, cursorPos = (0,0)
, spriteBackups = []
, curSprite = initSprite
, sprites = sprs}
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

deleteSprite :: GS -> GS
deleteSprite gs = case spriteAtLibCursor gs of
  Just (name,spr) -> gs { sprites = M.delete name (sprites gs) }
  Nothing -> gs
switchSprite :: GS -> GS
switchSprite gs = gs { curSprite = fromMaybe (curSprite gs) (spriteAtLibCursor gs) }
spriteAtLibCursor gs = spriteAtPos (libCursorPos gs) gs
spriteAtPos p gs = if ix < length sprs then Just (sprs !! ix) else Nothing
  where sprs = M.elems (sprites gs)
        ix = libCursorPosToSpriteNum p

modAxis f gs = gs { mirrorAxis = f $ mirrorAxis gs }
handleInput :: Event -> GS -> IO GS
handleInput (EventKey k Down mods _) gs = handleDown k mods gs
handleInput _ gs = return gs
handleDown k mods gs  = let cursorType = if (shift mods == Down) then LibraryCursor else if (ctrl mods == Down) then BoardCursor else SpriteCursor
  in case k of
  (SpecialKey KeyDown)  -> changeCursor cursorType CDown
  (SpecialKey KeyUp)    -> changeCursor cursorType CUp
  (SpecialKey KeyLeft)  -> changeCursor cursorType CLeft
  (SpecialKey KeyRight) -> changeCursor cursorType CRight
  (Char x) -> case x of
     'l' -> return $ switchSprite gs
     'X' -> return $ deleteSprite gs
--      '=' -> darken
--     '-' -> lighten
     'm' -> return $ modSpriteU mirror gs
     'r' -> return $ modSpriteU rotateSprite gs
     'h' -> return $ modSpriteU (shadowSprite (colorAtCanvasCursor gs) 1) gs
     'u' -> return $ undo gs
     'p' -> return $ placeSpriteOnBoard gs
     'w' -> return $ wipeSprite gs
     'z' -> return $ modSpriteU (wipeColor (colorAtCanvasCursor gs)) gs
     's' -> saveSprite gs
     'a' -> return $ modAxis incAxis gs
     'S' -> (writeSprites $ sprites gs) >> return gs
     other -> return $ if (other:"") `elem` (map show [0..9]) then modColor (read $ other:"") else gs
  where 
    modColor :: Int -> GS
    modColor cIx = modSpriteU (paintColorAt (mirrorAxis gs) (cursorPos gs) cIx) gs
    changeCursor :: CursorType -> CursorDir -> IO GS
    changeCursor SpriteCursor d  = return $ gs { cursorPos    = capPos ((0,0), (7,7)) $ changePos d $ cursorPos gs } 
    changeCursor LibraryCursor d = return $ gs { libCursorPos = capPos ((0,0), (7,7)) $ changePos d $ libCursorPos gs } 
    changeCursor BoardCursor d = return $ gs { boardCursorPos = capPos boardConstraints $ changePos d $ boardCursorPos gs } 
colorAtCanvasCursor gs = ar ! cursorPos gs
  where
    (_, ar) = curSprite gs
boardConstraints = ((0,0), (7,7))
data CursorType = LibraryCursor | SpriteCursor | BoardCursor deriving (Show, Eq)
wipeSprite gs = modSpriteU wipe gs
wipe :: MySprite -> MySprite
wipe (n,a) = renameSprite n $ initSprite
mirror :: MySprite -> MySprite
mirror (n, ar) = (n, arflip)
  where arflip = ar // [((x,y), ar ! (7-x, y)) | y <- [0..7], x <- [0..7]]
rotateSprite (n, ar) = (n, arflip)
  where arflip = ar // [((y, x), ar ! (x, y)) | y <- [0..7], x <- [0..7]]

undo gs = case spriteBackups gs of
  (prev:rest) -> gs { curSprite = prev, spriteBackups = rest }
  []          -> gs

wipeColor :: Int -> MySprite -> MySprite
wipeColor b (name, ar) = (name, ar')
  where
    ar' = ar // [(p, 0) | p <- allPosns, ar ! p == b]

shadowSprite :: Int -> Int -> MySprite -> MySprite
shadowSprite a b = applyShadow a b . wipeColor b 
applyShadow a b (name, ar) = (name, ar')
  where 
        ar' :: GridArray
        ar' = ar // [(p, b) | p <- safeShadowPosns]
        safeShadowPosns = filter isEmpty . filter isInSprite $ shadowPosns
        shadowPosns = map (vecadd (1,-1)) aPosns
        isEmpty pos = ar ! pos == 0
        aPosns = [p | p<- allPosns, ar ! p == a]
        isInSprite pos = inRange ((0,0), (7,7)) pos

allPosns = [(x,y) | y <- [0..7], x <- [0..7]]

placeSpriteOnBoard :: GS -> GS
placeSpriteOnBoard gs = gs { board = M.insert (boardCursorPos gs) (curSprite gs) (board gs) }
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
             , libCursorPos :: (Int, Int)
             , boardCursorPos :: (Int, Int)
             , board :: Map (Int, Int) MySprite
             , spriteBackups :: [MySprite]
             , curSprite :: MySprite
             , sprites :: SpriteMap
             , mirrorAxis :: Axis
             } deriving (Show, Eq)

modSprite f gs = gs { curSprite = f $ curSprite gs }
modSpriteU f gs = modSprite f $ gs { spriteBackups = curSprite gs : spriteBackups gs } 


drawState :: GS -> Picture
drawState gs = Pictures $ 
   [ translate (200)  (200)  $ drawCanvas (i `div` 20) 20 gs
   , translate (200)  (-100) $ drawSprite 0 5 (curSprite gs)
   , translate (-200) (200)  $ drawAllSprites gs
   , translate (-200) (-200) $ drawBoard gs
   , translate (fromIntegral (frame gs `mod` 800) - 300) (-300) $ drawSprite 0 5 (curSprite gs)
   , translate (-200 - (frameAsFloat gs)) (-200) $ textWithSprites (sprites gs) "HASKELL"
   ,  drawLines colorSeaGlass (-300,0) messages]
   
  where 
    i = frame gs
    messages = ["hello", "world", show i, show $ mirrorAxis gs, show $ libCursorPosToSpriteNum (libCursorPos gs) ]

drawBoard gs = Pictures $ [ drawCursor (boardCursorPos gs `vecadd` (-0, -0)) (8*spritesSize)
                          , drawSpritesAt (map toFloat $ M.keys $ board gs) (M.elems $ board gs) spritesSize
                          ]
  where spritesSize = 8
        toFloat (x,y) = (fi x, fi y)
        fi = fromIntegral
  
frameAsFloat gs = fromIntegral $ frame gs
drawCanvas _ sz gs = Pictures [Color black $ rectangleWire (8*szf) (8*szf)
                              , drawSprite 0 sz (curSprite gs)
                              ,  drawCanvasCursor $ cursorPos gs]
  where szf = fromIntegral sz
                             
drawAllSprites :: GS -> Picture
drawAllSprites gs = Pictures [ drawSpritesAt posns (M.elems $ sprites gs) spritesSize
                             , drawCursor (libCursorPos gs `vecadd` (-3, -3)) (8*spritesSize)
                             ]
  where
        posns = [(a-3,b-3) | a <- [0..7], b<-[0..7]]
        spritesSize = 4

vecadd (x,y) (a,b) = (x+a, y+b)

drawSpritesAt ::  [(Float, Float)] -> [MySprite] -> Int -> Picture
drawSpritesAt posns sprs sz = Pictures $ map (\((x,y),s) -> translate (x*sprSize + 5) (y*sprSize + 5) $ drawSprite 0 sz s) $ zip posns sprs
  where sprSize = (fromIntegral sz)*8

drawSprite :: Int -> Int -> MySprite -> Picture
drawSprite step size (sprName, spr) = 
  Pictures $ [cubeAt ((x-4,y-4), c) step size | x <- [0..7], y <-[0..7], let c = spr ! (x,y)]

cubeAt ((x,y),cIx) step size = case cIx of
      0 -> Pictures []  
      _ -> translate (f x) (f y) $ cubeSolid c edgeLen
            where c = colorFor $ (cIx + step)
                  f n = fromIntegral $ size * (fromIntegral n)
                  edgeLen = round $ (fromIntegral size) * 0.9
tileWidth :: Int
tileWidth = 20

drawCursor (x,y) sz = Color yellow $ translate (m x) (m y) $ rectangleWire (fi $ sz) (fi $ sz)
  where m = fi .  (sz *)
        fi = fromIntegral

drawCanvasCursor (x,y) = Color yellow $ translate (m x) (m y) $ rectangleWire (fi $ tileWidth - 4) (fi $ tileWidth - 4)
  where m = fi .  (tileWidth *) . (+ (-4))
        fi = fromIntegral

cubeSolid c w =  Rotate 0 $ Pictures [Color black $ rectangleSolid (f w) (f w), Color c $ rectangleSolid (f w2) (f w2)]
  where f  = fromIntegral
        w2 | w < 8 = w - 1
           | otherwise = w - 4

colorSea      = makeColor8 46 90 107 255
colorSeaGlass = makeColor8 163 204 188 255
colorForFish _fc = colorSeaGlass

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

libCursorPosToSpriteNum (x,y) = 8*x + y
