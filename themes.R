function themes

gen abstract = 0
replace abstract = 1 if index(lower(title), "abstract")
replace abstract = 1 if index(lower(title), "composition")

gen animals = 0 
replace animals = 1 if index(lower(title), "horse")
replace animals = 1 if index(lower(title), "cow ")
replace animals = 1 if index(lower(title), "cows")
replace animals = 1 if index(lower(title), "cattle")
replace animals = 1 if index(lower(title), "cat ")
replace animals = 1 if index(lower(title), "cats")
replace animals = 1 if index(lower(title), "dog")
replace animals = 1 if index(lower(title), "dogs")
replace animals = 1 if index(lower(title), "sheep")
replace animals = 1 if index(lower(title), "bird")


gen landscape = 0
replace landscape = 1 if index(lower(title), "landscape")
replace landscape = 1 if index(lower(title), "country landscape")
replace landscape = 1 if index(lower(title), "coastal landscape")
replace landscape = 1 if index(lower(title), "seascape")
replace landscape = 1 if index(lower(title), "sea ")
replace landscape = 1 if index(lower(title), "mountain") 
replace landscape = 1 if index(lower(title), "river")
replace landscape = 1 if index(lower(title), "lake")
replace landscape = 1 if index(lower(title), "valley")

gen nude = 0
replace nude = 1 if index(lower(title), "nude")

gen people = 0
replace people = 1 if index(lower(title), "people")
replace people = 1 if index(lower(title), "personnage")
replace people = 1 if index(lower(title), "family")
replace people = 1 if index(lower(title), "boy")
replace people = 1 if index(lower(title), "girl")
replace people = 1 if index(lower(title), "man ")
replace people = 1 if index(lower(title), "men ")
replace people = 1 if index(lower(title), "woman")
replace people = 1 if index(lower(title), "women")
replace people = 1 if index(lower(title), "child")
replace people = 1 if index(lower(title), "couple")
replace people = 1 if index(lower(title), "mother")
replace people = 1 if index(lower(title), "father")
replace people = 1 if index(lower(title), "lady") 

gen portrait = 0
replace portrait = 1 if index(lower(title), "portrait")
replace portrait = 1 if index(lower(title), "head")
replace portrait = 1 if index(lower(title), "face")
replace portrait = 1 if index(lower(title), "profile")
replace portrait = 1 if index(lower(title), "bride")
replace portrait = 1 if index(lower(title), "figure")

gen religion = 0
replace religion = 1 if index(lower(title), "jesus")
replace religion = 1 if index(lower(title), "christ")
replace religion = 1 if index(lower(title), "apostle")
replace religion = 1 if index(lower(title), "angel")
replace religion = 1 if index(lower(title), "saint ")
replace religion = 1 if index(lower(title), "madonna")
replace religion = 1 if index(lower(title), "holy")
replace religion = 1 if index(lower(title), "mary magdalene")
replace religion = 1 if index(lower(title), "annunciation")
replace religion = 1 if index(lower(title), "adoration")
replace religion = 1 if index(lower(title), "adam and eve")
replace religion = 1 if index(lower(title), "crucifixion")
replace religion = 1 if index(lower(title), "last supper") 

gen selfportrait = 0
replace selfportrait = 1 if index(lower(title), "self-portrait")
replace selfportrait = 1 if index(lower(title), "self portrait") 

gen still_life = 0
replace still_life = 1 if index(lower(title), "still life")
replace still_life = 1 if index(lower(title), "still-life")
replace still_life = 1 if index(lower(title), "bouquet")
replace still_life = 1 if index(lower(title), "rose")
replace still_life = 1 if index(lower(title), "flower")
replace still_life = 1 if index(lower(title), "flora")
replace still_life = 1 if index(lower(title), "fish")
replace still_life = 1 if index(lower(title), "violet")

gen untitled = 0
replace untitled = 1 if index(lower(title), "untitled")

gen urban = 0 
replace urban = 1 if index(lower(title), "city")
replace urban = 1 if index(lower(title), "town")
replace urban = 1 if index(lower(title), "village")
replace urban = 1 if index(lower(title), "street")
replace urban = 1 if index(lower(title), "market")
replace urban = 1 if index(lower(title), "harbour")
replace urban = 1 if index(lower(title), "port ")


gen themecode = 12
replace themecode = 1 if abstract == 1
replace themecode = 2 if animals == 1
replace themecode = 3 if landscape == 1
replace themecode = 4 if nude == 1
replace themecode = 5 if people == 1
replace themecode = 6 if portrait == 1
replace themecode = 7 if religion == 1
replace themecode = 8 if selfportrait == 1
replace themecode = 9 if still_life == 1
replace themecode = 10 if untitled == 1
replace themecode = 11 if urban == 1

drop abstract animals landscape nude people portrait religion selfportrait still_life untitled urban
