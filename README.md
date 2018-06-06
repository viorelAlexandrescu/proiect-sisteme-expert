scop -> atr.

r->id (unde id este numarul regulii)
atr_concluzie->valoare fc -> nr (concluzia; nr este factorul de certitudine)
premise -> ( (premisele intre paranteze, atributele separate cu +)
     atr ?=? valoare + (pentru atribute cu valori multiple)
     atr +(pentru atribute booleene, valoare true)
     nu atr (pentru atribute booleene, valoare false)
)
.

// schimba  la atr_concluzie->valoare; -> devine ?=?

interog -> atribut 
text_interog -> 'continut intrebare' 
list_optiuni -> (
-> val1 
-> val2 
-> val3 
).
============================ 37

Pentru a lua maxim pe interfata grafica, aceasta trebuie sa indeplineasca obiectivele amintite si la laborator:

	- sa pastreze toate functionalitatile din consola 
	(absolut toate optiunile din meniu si informatiile afisate de catre sistemul expert)

	- sa afiseze intrebarile dar si solutiile intr-o maniera interactiva

	- pentru fiecare solutie sa se afiseze imaginea, descrierea, specificatiile 
	(proprietatile din etapa1 - transimse din baza de cunostinte prolog)

	- pentru fiecare solutie sa se poata afisa demonstratia
	
	- sa verifice eventualul input al utilizatorului 
	(in cazul in care foloseste textbox-uri si nu butoane). Utilizatorul sa poata seta fc pentru raspunsuri.

	- sa poate fi accesate cunostintele sistemului (afisarea faptelor) si istoricul raspunsurilor la intrebari