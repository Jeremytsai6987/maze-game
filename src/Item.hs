module Item where
import qualified Data.Map as M

-- Teams
data TeamName
  = Dodgers
  | Yankees
  | RedSox
  | Cubs
  | Padres
  | Brewers
  | Giants
  | Braves
  | DBacks
  | NoTeam
  deriving (Eq, Ord, Show)

-- Players
data ItemName
  = MookieBetts  -- Dodgers
  | ClaytonKershaw
  | FreddieFreeman
  | ShoheiOhtani 
  | AaronJudge   -- Yankees
  | GerritCole
  | GiancarloStanton
  | RafaelDevers -- Red Sox
  | ChrisSale
  | TrevorStory
  | CodyBellinger -- Cubs
  | DansbySwanson
  | JustinSteele
  | MannyMachado -- Padres
  | JuanSoto
  | FernandoTatisJr
  | CorbinBurnes -- Brewers
  | WillyAdames
  | ChristianYelich
  | LoganWebb    -- Giants
  | JocPederson
  | ThairoEstrada
  | RonaldAcunaJr -- Braves
  | MattOlson
  | SpencerStrider
  | ZacGallen     -- DBacks
  | CorbinCarroll
  | KetelMarte
  | FenwayTicket -- Special item
  | WrigleyKey
  deriving (Eq, Ord)

instance Show ItemName where
  show MookieBetts = "Mookie Betts"
  show ClaytonKershaw = "Clayton Kershaw"
  show FreddieFreeman = "Freddie Freeman"
  show ShoheiOhtani = "Shohei Ohtani"
  show AaronJudge = "Aaron Judge"
  show GerritCole = "Gerrit Cole"
  show GiancarloStanton = "Giancarlo Stanton"
  show RafaelDevers = "Rafael Devers"
  show ChrisSale = "Chris Sale"
  show TrevorStory = "Trevor Story"
  show CodyBellinger = "Cody Bellinger"
  show DansbySwanson = "Dansby Swanson"
  show JustinSteele = "Justin Steele"
  show MannyMachado = "Manny Machado"
  show JuanSoto = "Juan Soto"
  show FernandoTatisJr = "Fernando Tatis Jr."
  show CorbinBurnes = "Corbin Burnes"
  show WillyAdames = "Willy Adames"
  show ChristianYelich = "Christian Yelich"
  show LoganWebb = "Logan Webb"
  show JocPederson = "Joc Pederson"
  show ThairoEstrada = "Thairo Estrada"
  show RonaldAcunaJr = "Ronald Acuna Jr."
  show MattOlson = "Matt Olson"
  show SpencerStrider = "Spencer Strider"
  show ZacGallen = "Zac Gallen"
  show CorbinCarroll = "Corbin Carroll"
  show KetelMarte = "Ketel Marte"
  show FenwayTicket = "Fenway Ticket"
  show WrigleyKey = "Wrigley Key"

type Universe = M.Map ItemName Item

-- Item structure
data Item = Item {
    iname :: ItemName,
    salary :: Integer,
    team :: TeamName
}
    deriving (Show, Eq)

-- Players by team with realistic salaries
mookieBetts = Item MookieBetts 35 Dodgers
claytonKershaw = Item ClaytonKershaw 31 Dodgers
freddieFreeman = Item FreddieFreeman 27 Dodgers
shoheiOhtani = Item ShoheiOhtani 99 Dodgers -- Special rule: Instant win!

aaronJudge = Item AaronJudge 40 Yankees
gerritCole = Item GerritCole 36 Yankees
giancarloStanton = Item GiancarloStanton 32 Yankees

rafaelDevers = Item RafaelDevers 31 RedSox
chrisSale = Item ChrisSale 27 RedSox
trevorStory = Item TrevorStory 23 RedSox

codyBellinger = Item CodyBellinger 17 Cubs
dansbySwanson = Item DansbySwanson 25 Cubs
justinSteele = Item JustinSteele 7 Cubs

mannyMachado = Item MannyMachado 35 Padres
juanSoto = Item JuanSoto 23 Padres
fernandoTatisJr = Item FernandoTatisJr 25 Padres

corbinBurnes = Item CorbinBurnes 12 Brewers
willyAdames = Item WillyAdames 85 Brewers
christianYelich = Item ChristianYelich 26 Brewers

loganWebb = Item LoganWebb 23 Giants
jocPederson = Item JocPederson 19 Giants
thairoEstrada = Item ThairoEstrada 60 Giants

ronaldAcunaJr = Item RonaldAcunaJr 17 Braves
mattOlson = Item MattOlson 22 Braves
spencerStrider = Item SpencerStrider 12 Braves

zacGallen = Item ZacGallen 65 DBacks
corbinCarroll = Item CorbinCarroll 14 DBacks
ketelMarte = Item KetelMarte 11 DBacks


-- Universe of players
mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList [(iname item, item) | item <- items]

univ :: Universe
univ = mkUniverse [mookieBetts, claytonKershaw, freddieFreeman,
  aaronJudge, gerritCole, giancarloStanton,
  rafaelDevers, chrisSale, trevorStory,
  codyBellinger, dansbySwanson, justinSteele,
  mannyMachado, juanSoto, fernandoTatisJr,
  corbinBurnes, willyAdames, christianYelich,
  loganWebb, jocPederson, thairoEstrada,
  ronaldAcunaJr, mattOlson, spencerStrider,
  zacGallen, corbinCarroll, ketelMarte,
  shoheiOhtani, fenwayTicket, wrigleyKey]

fenwayTicket :: Item
fenwayTicket = Item FenwayTicket 1 NoTeam

wrigleyKey :: Item
wrigleyKey = Item WrigleyKey 1 NoTeam

itemNames :: [ItemName]
itemNames = M.keys univ
