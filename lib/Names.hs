module Names where

import Namegen

data Person = Person { personName :: Name
                     , personSex :: Sex
                     , personAge :: Int
                     }

data Name = StdBName String String String        | StdGName String String String
          | SglBName String                      | SglGName String
          | TitBName String String String String | TitGName String String String String
          | AbvBName String String String String | AbvGName String String String String
          | SdMBName String String String String | SdMGName String String String String
          | TtEBName String String String String | TtEGName String String String String
data Sex = Male | Female

printPerson :: Person -> IO ()
printPerson (Person a b c) = printName a

printName :: Name -> IO ()
printName (StdBName a b c)   = print (a ++ " " ++ b ++ c)
printName (StdGName a b c)   = print (a ++ " " ++ b ++ c)
printName (SglBName a)       = print a
printName (SglGName a)       = print a
printName (TitBName a b c d) = print (a ++ b ++ " " ++ c ++ d)
printName (TitGName a b c d) = print (a ++ b ++ " " ++ c ++ d)
printName (AbvBName a b c d) = print (a ++ " " ++ b ++ " " ++ c ++ d)
printName (AbvGName a b c d) = print (a ++ " " ++ b ++ " " ++ c ++ d)
printName (SdMBName a b c d) = print (a ++ b ++ c ++ d)
printName (SdMGName a b c d) = print (a ++ b ++ c ++ d)
printName (TtEBName a b c d) = print (a ++ " " ++ b ++ c ++ d)
printName (TtEGName a b c d) = print (a ++ " " ++ b ++ c ++ d)

makePerson :: Int -> Sex -> Int -> Int -> Int -> Int -> Person
makePerson age Male   s1 s2 s3 1 = do
  let n1lang = fromSamples bnames
  Person { personName = (SglBName (generateName n1lang s1))
         , personSex  = Male
         , personAge  = age
         }
makePerson age Female s1 s2 s3 1 = do
  let n1lang = fromSamples gnames
  Person { personName = (SglGName (generateName n1lang s1))
         , personSex  = Female
         , personAge  = age
         }
makePerson age Male   s1 s2 s3 2 = do
  let n1lang = fromSamples pretitlenames
  let n2lang = fromSamples bnames
  let n3lang = fromSamples patronymsuffixes
  Person { personName = (TitBName (generateName n1lang s1) (generateName n2lang s2) (generateName n2lang s3) (generateName n3lang s1))
         , personSex  = Male
         , personAge  = age
         }
makePerson age Female  s1 s2 s3 2 = do
  let n1lang = fromSamples pretitlenames
  let n2lang = fromSamples gnames
  let n3lang = fromSamples bnames
  let n4lang = fromSamples matronymsuffixes
  Person { personName = (TitGName (generateName n1lang s1) (generateName n2lang s2) (generateName n3lang s3) (generateName n4lang s1))
         , personSex  = Female
         , personAge  = age
         }
makePerson age Male   s1 s2 s3 3 = do
  let n1lang = fromSamples initials
  let n2lang = fromSamples bnames
  let n3lang = fromSamples patronymsuffixes
  Person { personName = (AbvBName (generateName n1lang s1) (generateName n1lang s2) (generateName n2lang s3) (generateName n3lang s1))
         , personSex  = Male
         , personAge  = age
         }
makePerson age Female  s1 s2 s3 3 = do
  let n1lang = fromSamples initials
  let n2lang = fromSamples bnames
  let n3lang = fromSamples matronymsuffixes
  Person { personName = (AbvGName (generateName n1lang s1) (generateName n1lang s2) (generateName n2lang s3) (generateName n3lang s1))
         , personSex  = Female
         , personAge  = age
         }
makePerson age Male   s1 s2 s3 4 = do
  let n1lang = fromSamples bnames
  let n2lang = fromSamples middlenames
  let n3lang = fromSamples patronymsuffixes
  Person { personName = (SdMBName (generateName n1lang s1) (generateName n2lang s2) (generateName n1lang s2) (generateName n3lang s3))
         , personSex  = Male
         , personAge  = age
         }
makePerson age Female s1 s2 s3 4 = do
  let n1lang = fromSamples gnames
  let n2lang = fromSamples middlenames
  let n3lang = fromSamples bnames
  let n4lang = fromSamples matronymsuffixes
  Person { personName = (SdMGName (generateName n1lang s1) (generateName n2lang s2) (generateName n1lang s2) (generateName n4lang s3))
         , personSex  = Female
         , personAge  = age
         }
makePerson age Male   s1 s2 s3 5 = do
  let n1lang = fromSamples bnames
  let n2lang = fromSamples patronymsuffixes
  let n3lang = fromSamples suftitlenames
  Person { personName = (TtEBName (generateName n1lang s1) (generateName n1lang s2) (generateName n2lang s2) (generateName n3lang s3))
         , personSex  = Male
         , personAge  = age
         }
makePerson age Female s1 s2 s3 5 = do
  let n1lang = fromSamples gnames
  let n2lang = fromSamples bnames
  let n3lang = fromSamples matronymsuffixes
  let n4lang = fromSamples suftitlenames
  Person { personName = (TtEGName (generateName n1lang s1) (generateName n2lang s2) (generateName n3lang s2) (generateName n4lang s3))
         , personSex  = Female
         , personAge  = age
         }
makePerson age Male   s1 s2 s3 _ = do
  let n1lang = fromSamples bnames
  let n2lang = fromSamples patronymsuffixes
  Person { personName = (StdBName (generateName n1lang s1) (generateName n1lang s2) (generateName n2lang s3))
         , personSex  = Male
         , personAge  = age
         }
makePerson age Female s1 s2 s3 _ = do
  let n1lang = fromSamples gnames
  let n2lang = fromSamples bnames
  let n3lang = fromSamples matronymsuffixes
  Person { personName = (StdGName (generateName n1lang s1) (generateName n2lang s2) (generateName n3lang s3))
         , personSex  = Female
         , personAge  = age
         }
  

gnames = ["Aeshma", "Thelema", "Agiel", "Angra", "Haborym", "Aka", "Ala", "Amdusias", "Andhaka", "Allu", "Armaros", "Arunasura", "Asbel", "Asura", "Azazel", "Azrael", "Bael", "Bakasura", "Beleth", "Berith", "Bhuta", "Boruta", "Bushyasta", "Burza", "Behexen", "Bakka", "Choron", "Corsa", "Cadveria", "Catamenia", "Celestia", "Chakal", "Gorga", "Daeva", "Deva", "Decarabia", "Darza", "Diocletia", "Ephel", "Glasya", "Guayota", "Gehenna", "Goatwhore", "Grima", "Haag", "Hauras", "Raya", "Hel", "Hecate", "Horna", "Horde", "Ipes", "Jikininki", "Iskra", "Kali", "Kabandha", "Kasadya", "Kumbhakarna", "Kalessin", "Katatonia", "Lila", "Lilith", "Lilim", "Occulta", "Ludicra", "Mara", "Maricha", "Mystic", "Naamah", "Ninurta", "Nacth", "Nahemah", "Natte", "Negura", "Norda", "Ophthalamia", "Penemue", "Presta", "Rasalka", "Rakshasa", "Rangda", "Ravan", "Rudra", "Semyaza", "Sitri", "Succuba", "Sepultura", "Slavia", "Susperia", "Tuchulcha", "Vapula", "Vesania", "Ravana", "Jilaiya", "Orusula", "Ursula", "Nimue", "Batibat", "Hecate", "Lezabel", "Lamia", "Lilith", "Pandora", "Mara", "Qarinah", "Ardat", "Lili", "Empusa", "Lamashtu", "Lilitu", "Yuki", "Jezebeth", "Kasdeya", "Prosperine"]

bnames = ["Ahriman", "Desolat", "Abhorat", "Infern", "Infernus", "Amarath", "Ayyavazh", "Aamon", "Amon", "Abbadon", "Apollyon", "Abezethibou", "Agaliarept", "Agrat", "Agares", "Agiel", "Angra", "Akem", "Akoman", "Akvan", "Alal", "Alastor", "Alloces", "Allocer", "Allu", "Amaymon", "Ancitif", "Anamalech", "Andras", "Andrealphus", "Andromalius", "Anzu", "Abzu", "Archon", "Asag", "Asakku", "Asmodai", "Asmodeus", "Astaroth", "Azazel", "Azi", "Abigor", "Abomin", "Aboryim", "Acheron", "Aeturn", "Agalloch", "Agath", "Alcest", "Nathrakh", "Antaeus", "Arcturus", "Armageddon", "Arvas", "Asgaroth", "Azaghal", "Anagnorisis", "Astennu", "Alver", "Ares", "Baal", "Balam", "Balberith", "Barbas", "Barbatos", "Barong", "Bathin", "Bathym", "Beherit", "Bifrons", "Botis", "Buer", "Bukavac", "Bune", "Bahimiron", "Sagoth", "Barathrum", "Beherit", "Borknar", "Bakken", "Caim", "Cain", "Charon", "Chemosh", "Choronzon", "Crocell", "Procell", "Culsu", "Cimenjes", "Kimaris", "Carach", "Angren", "Gorgor", "Cor", "Cultus", "Caligula", "Culto", "Csihar", "Dagon", "Dajjal", "Danjal", "Demiurge", "Demo", "Drekavac", "Dzoavits", "Darzamat", "Diaboli", "Dimmu", "Borgir", "Drottnar", "Drudkh", "Eblis", "Eligos", "Eisheth", "Eisen", "Ewigkiet", "Fenriz", "Focalor", "Goras", "Foras", "Forneus", "Fornicaticus", "Furcas", "Furfur", "Falkenbach", "Faust", "Fimbul", "Finn", "Fleurety", "Forest", "Frost", "Gaal", "Gaap", "Gaderel", "Gaki", "Gamigin", "Ghoul", "Gremory", "Gamorah", "Gedorah", "Grigori", "Gualichu", "Gusoyn", "Gaashiskagg", "Gallhammer", "Goatlord", "Gorgoroth", "Grim", "Haagenti", "Halphas", "Malthus", "Haures", "Havres", "Hantu", "Hel", "Holocausto", "Hortus", "Ifrit", "Ipos", "Ihsahn", "Ildjarn", "Infernal", "Infernum", "Incantion", "Inquinok", "Judas", "Kroni", "Krampus", "Kampfar", "Kekal", "Khold", "King", "Korovakill", "Krieg", "Kultus", "Lechies", "Leyak", "Lempo", "Laraje", "Leviathan", "Lengsel", "Kaos", "Lux", "Malphas", "Mammon", "Morax", "Masih", "Mephisto", "Mephistoles", "Mephistopheles", "Moloch", "Merihem", "Manegarm", "Manes", "Marduk", "Melechesh", "Mithotyn", "Moribund", "Mutilator", "Morgul", "Naberius", "Cerebere", "Nametar", "Naglfar", "Nargaroth", "Nazxul", "Necro", "Nefastus", "Nifel", "Nortt", "Onoskelis", "Orcus", "Orias", "Orobas", "Ode", "Ordog", "Oranssi", "Pazuzu", "Orctustus", "Oathean", "Paamon", "Pelesit", "Phenex", "Pithius", "Pocong", "Pontianak", "Puloman", "Peccatum", "Rahab", "Raum", "Ranove", "Ragnarok", "Ramsees", "Ragnarok", "Sabnock", "Saleos", "Samael", "Seir", "Shedim", "Sthenno", "Stolas", "Solas", "Suanggi", "Surgat", "Sabbat", "Salem", "Sammath", "Sarco", "Sargeist", "Setherial", "Sigh", "Skitliv", "Sodom", "Solefald", "Sturm", "Sabnock", "Saleos", "Tannin",  "Toyol", "Ukobach", "Valac", "Valefar", "Malephar", "Vanth", "Vassago", "Vassage", "Wendigo", "Vepar", "Yeqon", "Zagan", "Zepar", "Ziminiar", "Zozo", "Taake", "Thronar", "Thyr", "Tiamat", "Tormentor", "Tsatt", "Tsjuder", "Tvangeste", "Ulver", "Urgehal", "Valhall", "Ved", "Vince", "Venom", "Vital", "Vikal", "Vlad", "Von", "Vondur", "Vorkreist", "Vreid", "Watain", "Welt", "Wolf", "Wykked", "Wyrd", "Xasthur", "Zyklon", "Abaddon", "Abraxas", "Adramelech", "Agares", "Ahriman", "Alastor", "Alloces", "Amdusias", "Amduscias", "Amon", "Azazel", "Diabolos", "Abigor", "Seth", "Chernobog", "Addanc", "Baal", "Bereth", "Apep", "Morfran", "Zagan"]


patronymsuffixes = ["aj", "ov", "ev", "ski", "in", "son", "ska", "sen", "poika", "oval", "fi", "escu", "ovich", "evich", "ich", "ovych", "yovych", "zoon", "arap", "heim", "gaard", "mann", "burgen", "fried", "os", "s", "aitos", "utis", "ytis", "enas", "unas", "inis", "ynis", "onis", "ius", "elis", "ak", "escu", "eanu", "aru", "enko", "yn", "nyj", "uk", "ych", "iv", "zadeh", "pur", "tabar", "nezhad", "bhai", "ge"]

matronymsuffixes = ["ova", "eva", "ska", "ina", "na", "ou", "in", "a", "e", "iene", "te", "yte", "aite", "ovna", "evna", "yevna", "ivna", "bai", "ben"]

pretitlenames = ["Agha ", "Abd Al ", "Abdul ", "Abu ", "Sheikh ", "Hadj ", "Master ", "Sir ", "Lord ", "Dr. ", "Prof. ", "His Holiness ", "His Eminence ", "His Lordship ", "The Reverend ", "Elder ", "Rabbi ", "Earl of ", "Duke of ", "Marquis ", "Viscount ", "Baron ", "Herr ", "Frau", "Dom ", "Don "]

suftitlenames = [" The Mutilated", "uddin", "-al-din", "allah", " Khan"]

middlenames = [" Von ", " Van Der ", " El-", " of ", " ov ", " bin ", " ibn ", " Al-", " lal ", " Singh ", " Kaur ", " De ", " Du ", " av ", " af ", " ver ", " A ", " A. ", " B. ", " C. ", " D. ", " E. ", " F. ", " G. ", " H. ", " I. ", " J. ", " K. ", " L. ", " M. ", " N. ", " O. ", " P. ", " Q. ", " R. ", " S. ", " T. ", " U. ", " V. ", " X. ", " Y. ", " Z. "]

initials = ["A.", "B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.", "J.", "K.", "L.", "M.", "N.", "O.", "P.", "Q.", "R.", "S.", "T.", "U.", "V.", "X.", "Y.", "Z."]

