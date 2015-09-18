
function materials

gen paper = 0
replace paper = 1 if index(lower(medium), "paper")
replace paper = 1 if index(lower(medium), "card")

gen board = 0 
replace board = 1 if index(lower(medium), "board")
replace board = 1 if index(lower(medium), "panel")

gen canvas = 0
replace canvas = 1 if index(lower(medium), "canvas")

gen print1 = 0
replace print1 = 1 if index(lower(medium), "print")
replace print1 = 1 if index(lower(medium), "screen")

gen print2 = 0
replace print2 = 1 if index(lower(medium), "lithograph")
replace print2 = 1 if index(lower(medium), "linocut")

gen bronze =0
replace bronze = 1 if index(lower(medium), "bronze")

gen ceramic =0
replace ceramic = 1 if index(lower(medium), "ceramic")

gen material = 8
replace material = 1 if paper == 1
replace material = 2 if board == 1
replace material = 3 if canvas == 1
replace material = 4 if print1 == 1
replace material = 5 if print2 == 1
replace material = 6 if bronze == 1
replace material = 7 if ceramic == 1


drop paper board canvas print1 print2 bronze ceramic