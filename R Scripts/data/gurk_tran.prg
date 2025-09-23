workfile gurk_tran q  1971:4 2030:04 
pagerename untitled quarterly
'---------------------------------------------------
'Import daily data   Gurkanyak et al. 
'----------------------------------------------------
pagecreate(page = daily5) d(1,5) 31/12/1971 31/12/2030
read( b11, s=daily ) "feds200628.xls" 36
copy(c= l)  daily5\* quarterly\*
pageselect quarterly

