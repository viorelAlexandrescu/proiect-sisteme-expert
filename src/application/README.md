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