module Names where

import Namegen

data Person = Person { personName :: Name
                     , personSex :: Sex
                     , personAge :: Int
                     }

data Name = StdBName String String String | StdGName String String String
data Sex = Male | Female

printPerson :: Person -> IO ()
printPerson (Person a b c) = printName a

printName :: Name -> IO ()
printName (StdBName a b c) = print (a ++ " " ++ b ++ c)
printName (StdGName a b c) = print (a ++ " " ++ b ++ c)

makePerson :: Int -> Sex -> Int -> Int -> Int -> Int -> Person
makePerson age Male   s1 s2 s3 s4 = do
  let n1lang = fromSamples bnames
  let n2lang = fromSamples patronymsuffixes
  Person { personName = (StdBName (generateName n1lang s1) (generateName n1lang s2) (generateName n2lang s3))
         , personSex  = Male
         , personAge  = age
         }
makePerson age Female s1 s2 s3 s4 = do
  let n1lang = fromSamples gnames
  let n2lang = fromSamples bnames
  let n3lang = fromSamples patronymsuffixes
  Person { personName = (StdGName (generateName n1lang s1) (generateName n2lang s2) (generateName n3lang s3))
         , personSex  = Female
         , personAge  = age
         }
  

gnames = ["Aeshma", "Thelema", "Agiel", "Angra", "Haborym", "Aka", "Ala", "Amdusias", "Andhaka", "Allu", "Armaros", "Arunasura", "Asbel", "Asura", "Azazel", "Azrael", "Bael", "Bakasura", "Beleth", "Berith", "Bhuta", "Boruta", "Bushyasta", "Burza", "Behexen", "Bakka", "Choron", "Corsa", "Cadveria", "Catamenia", "Celestia", "Chakal", "Gorga", "Daeva", "Deva", "Decarabia", "Darza", "Diocletia", "Ephel", "Glasya", "Guayota", "Gehenna", "Goatwhore", "Grima", "Haag", "Hauras", "Raya", "Hel", "Hecate", "Horna", "Horde", "Ipes", "Jikininki", "Iskra", "Kali", "Kabandha", "Kasadya", "Kumbhakarna", "Kalessin", "Katatonia", "Lila", "Lilith", "Lilim", "Occulta", "Ludicra", "Mara", "Maricha", "Mystic", "Naamah", "Ninurta", "Nacth", "Nahemah", "Natte", "Negura", "Norda", "Ophthalamia", "Penemue", "Presta", "Rasalka", "Rakshasa", "Rangda", "Ravan", "Rudra", "Semyaza", "Sitri", "Succuba", "Sepultura", "Slavia", "Susperia", "Tuchulcha", "Vapula", "Vesania", "Ravana", "Jilaiya", "Orusula", "Ursula", "Nimue", "Batibat", "Hecate", "Lezabel", "Lamia", "Lilith", "Pandora", "Mara", "Qarinah", "Ardat", "Lili", "Empusa", "Lamashtu", "Lilitu", "Yuki", "Jezebeth", "Kasdeya", "Prosperine"]

bnames = ["Ahriman", "Desolat", "Abhorat", "Infern", "Infernus", "Amarath", "Ayyavazh", "Aamon", "Amon", "Abbadon", "Apollyon", "Abezethibou", "Agaliarept", "Agrat", "Agares", "Agiel", "Angra", "Akem", "Akoman", "Akvan", "Alal", "Alastor", "Alloces", "Allocer", "Allu", "Amaymon", "Ancitif", "Anamalech", "Andras", "Andrealphus", "Andromalius", "Anzu", "Abzu", "Archon", "Asag", "Asakku", "Asmodai", "Asmodeus", "Astaroth", "Azazel", "Azi", "Abigor", "Abomin", "Aboryim", "Acheron", "Aeturn", "Agalloch", "Agath", "Alcest", "Nathrakh", "Antaeus", "Arcturus", "Armageddon", "Arvas", "Asgaroth", "Azaghal", "Anagnorisis", "Astennu", "Alver", "Ares", "Baal", "Balam", "Balberith", "Barbas", "Barbatos", "Barong", "Bathin", "Bathym", "Beherit", "Bifrons", "Botis", "Buer", "Bukavac", "Bune", "Bahimiron", "Sagoth", "Barathrum", "Beherit", "Borknar", "Bakken", "Caim", "Cain", "Charon", "Chemosh", "Choronzon", "Crocell", "Procell", "Culsu", "Cimenjes", "Kimaris", "Carach", "Angren", "Gorgor", "Cor", "Cultus", "Caligula", "Culto", "Csihar", "Dagon", "Dajjal", "Danjal", "Demiurge", "Demo", "Drekavac", "Dzoavits", "Darzamat", "Diaboli", "Dimmu", "Borgir", "Drottnar", "Drudkh", "Eblis", "Eligos", "Eisheth", "Eisen", "Ewigkiet", "Fenriz", "Focalor", "Goras", "Foras", "Forneus", "Fornicaticus", "Furcas", "Furfur", "Falkenbach", "Faust", "Fimbul", "Finn", "Fleurety", "Forest", "Frost", "Gaal", "Gaap", "Gaderel", "Gaki", "Gamigin", "Ghoul", "Gremory", "Gamorah", "Gedorah", "Grigori", "Gualichu", "Gusoyn", "Gaashiskagg", "Gallhammer", "Goatlord", "Gorgoroth", "Grim", "Haagenti", "Halphas", "Malthus", "Haures", "Havres", "Hantu", "Hel", "Holocausto", "Hortus", "Ifrit", "Ipos", "Ihsahn", "Ildjarn", "Infernal", "Infernum", "Incantion", "Inquinok", "Judas", "Kroni", "Krampus", "Kampfar", "Kekal", "Khold", "King", "Korovakill", "Krieg", "Kultus", "Lechies", "Leyak", "Lempo", "Laraje", "Leviathan", "Lengsel", "Kaos", "Lux", "Malphas", "Mammon", "Morax", "Masih", "Mephisto", "Mephistoles", "Mephistopheles", "Moloch", "Merihem", "Manegarm", "Manes", "Marduk", "Melechesh", "Mithotyn", "Moribund", "Mutilator", "Morgul", "Naberius", "Cerebere", "Nametar", "Naglfar", "Nargaroth", "Nazxul", "Necro", "Nefastus", "Nifel", "Nortt", "Onoskelis", "Orcus", "Orias", "Orobas", "Ode", "Ordog", "Oranssi", "Pazuzu", "Orctustus", "Oathean", "Paamon", "Pelesit", "Phenex", "Pithius", "Pocong", "Pontianak", "Puloman", "Peccatum", "Rahab", "Raum", "Ranove", "Ragnarok", "Ramsees", "Ragnarok", "Sabnock", "Saleos", "Samael", "Seir", "Shedim", "Sthenno", "Stolas", "Solas", "Suanggi", "Surgat", "Sabbat", "Salem", "Sammath", "Sarco", "Sargeist", "Setherial", "Sigh", "Skitliv", "Sodom", "Solefald", "Sturm", "Sabnock", "Saleos", "Tannin",  "Toyol", "Ukobach", "Valac", "Valefar", "Malephar", "Vanth", "Vassago", "Vassage", "Wendigo", "Vepar", "Yeqon", "Zagan", "Zepar", "Ziminiar", "Zozo", "Taake", "Thronar", "Thyr", "Tiamat", "Tormentor", "Tsatt", "Tsjuder", "Tvangeste", "Ulver", "Urgehal", "Valhall", "Ved", "Vince", "Venom", "Vital", "Vikal", "Vlad", "Von", "Vondur", "Vorkreist", "Vreid", "Watain", "Welt", "Wolf", "Wykked", "Wyrd", "Xasthur", "Zyklon", "Abaddon", "Abraxas", "Adramelech", "Agares", "Ahriman", "Alastor", "Alloces", "Amdusias", "Amduscias", "Amon", "Azazel", "Diabolos", "Abigor", "Seth", "Chernobog", "Addanc", "Baal", "Bereth", "Apep", "Morfran", "Zagan"]


patronymsuffixes = ["son", "sen", "poika", "oval", "fi", "escu", "ovich", "evich", "ich", "ovych", "yovych", "zoon", "arap", "heim", "gaard", "mann", "burgen", "fried"]
