module Room where
import Item
import Direction
import Data.List
import System.Random

data RoomName
  = DodgerStadium
  | YankeeStadium
  | FenwayPark
  | WrigleyField
  | PetcoPark
  | AmericanFamilyField
  | OraclePark
  | TruistPark
  | ChaseField
  deriving (Eq, Ord)

instance Show RoomName where
  show DodgerStadium = "Dodger Stadium"
  show YankeeStadium = "Yankee Stadium"
  show FenwayPark = "Fenway Park"
  show WrigleyField = "Wrigley Field"
  show PetcoPark = "Petco Park"
  show AmericanFamilyField = "American Family Field"
  show OraclePark = "Oracle Park"
  show TruistPark = "Truist Park"
  show ChaseField = "Chase Field"
  
type Exit = (Direction, RoomName, Maybe ItemName)
data Room = Room {
    rname :: RoomName,
    desc :: String,
    exits :: [Exit],
    objects:: [ItemName]
} deriving (Show, Eq)

dodgerStadium :: Room
dodgerStadium = Room DodgerStadium 
    "You are at Dodger Stadium, home of the Los Angeles Dodgers." 
    [(E, YankeeStadium, Nothing), (S, OraclePark, Nothing),
     (W, PetcoPark, Nothing)] 
    [MookieBetts, AaronJudge, RafaelDevers]

yankeeStadium :: Room
yankeeStadium = Room YankeeStadium 
    "You are at Yankee Stadium, home of the New York Yankees." 
    [(W, DodgerStadium, Nothing), (E, FenwayPark, Just FenwayTicket),
     (S, TruistPark, Nothing)] 
    [ClaytonKershaw, MannyMachado, ZacGallen, FenwayTicket]


fenwayPark :: Room
fenwayPark = Room FenwayPark 
    "You are at Fenway Park, home of the Boston Red Sox." 
    [(W, YankeeStadium, Nothing), (S, WrigleyField, Just WrigleyKey),
     (N, AmericanFamilyField, Nothing)] 
    [FreddieFreeman, GerritCole, ChristianYelich]


wrigleyField :: Room
wrigleyField = Room WrigleyField 
    "You are at Wrigley Field, home of the Chicago Cubs." 
    [(N, FenwayPark, Just FenwayTicket), (S, ChaseField, Nothing),
     (W, OraclePark, Nothing)] 
    [ShoheiOhtani, JuanSoto, ThairoEstrada]


petcoPark :: Room
petcoPark = Room PetcoPark 
    "You are at Petco Park, home of the San Diego Padres." 
    [(E, DodgerStadium, Nothing), (N, TruistPark, Nothing)] 
    [DansbySwanson, RonaldAcunaJr, ChrisSale, WrigleyKey]

americanFamilyField :: Room
americanFamilyField = Room AmericanFamilyField 
    "You are at American Family Field, home of the Milwaukee Brewers." 
    [(S, FenwayPark, Just FenwayTicket), (W, ChaseField, Nothing),
     (E, TruistPark, Nothing)] 
    [GiancarloStanton, CodyBellinger, CorbinBurnes]

oraclePark :: Room
oraclePark = Room OraclePark 
    "You are at Oracle Park, home of the San Francisco Giants." 
    [(N, DodgerStadium, Nothing), (E, WrigleyField, Just WrigleyKey),
     (S, ChaseField, Nothing)] 
    [FernandoTatisJr, JocPederson, MattOlson]


truistPark :: Room
truistPark = Room TruistPark 
    "You are at Truist Park, home of the Atlanta Braves." 
    [(N, YankeeStadium, Nothing), (S, PetcoPark, Nothing),
     (W, AmericanFamilyField, Nothing)] 
    [TrevorStory, WillyAdames, CorbinCarroll]

chaseField :: Room
chaseField = Room ChaseField 
    "You are at Chase Field, home of the Arizona Diamondbacks." 
    [(N, WrigleyField, Just WrigleyKey), (W, OraclePark, Nothing),
     (E, AmericanFamilyField, Nothing)] 
    [LoganWebb, SpencerStrider, JustinSteele]



roomNames :: [RoomName]
roomNames = [rname room | room <- allRooms]

addItem :: ItemName -> Room -> Room
addItem item room = room {objects = item : objects room}

removeItem :: ItemName -> Room -> Room
removeItem item room = room {objects = delete item (objects room)}

allRooms :: [Room]
allRooms = [dodgerStadium, yankeeStadium, fenwayPark, wrigleyField, petcoPark,
 americanFamilyField, oraclePark, truistPark, chaseField]

hasObjects :: Room -> Bool
hasObjects room = not (null (objects room))