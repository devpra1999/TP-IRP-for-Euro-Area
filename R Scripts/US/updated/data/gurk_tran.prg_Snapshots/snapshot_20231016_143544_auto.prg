workfile gurk_tran q  1971:4 2030:04 
pagerename untitled quarterly
'---------------------------------------------------
'Import daily data   Gurkanyak et al. 
'----------------------------------------------------
 pagecreate(page = daily5) d(1,5) 31/12/1971 31/12/2030
read( b2, s=daily ) "feds200628.xls" 36

copy(c= l)  daily5\sveny10* monthly\sveny10*
copy(c= l)  daily5\sveny02* monthly\sveny02*
copy(c= l)  daily5\tipsy10* monthly\tipsy10*
copy(c= l)  daily5\tipsy02* monthly\tipsy02*
pageselect monthly
'--------------------------------
'data transformation   
'----------------------------------
smpl @all
genr infl_US=(cpiaucns/cpiaucns(-12))-1
genr infl_EU=(cpi_euro/cpi_euro(-12))-1
genr infl_ita=(cpi_ita/cpi_ita(-12))-1
smpl 2023:1 2023:1
infl_ita=0.086
smpl 2023:2 2023:2
infl_ita=0.085
smpl @all

genr dinfl_US=infl_US-infl_US(-1) 
genr d12infl_US=infl_US-infl_US(-12) 
genr ret_sp=((SP+SPDIV/12)/SP(-1))-1
smpl 1962:1 1962:1
genr P_SP=1
smpl 1962:2 @last
genr P_SP=P_SP(-1)*(1+ret_sp)
smpl 1962:1 @last
genr lp_sp=log(p_sp) 
genr ret_sp_l= lp_sp-lp_sp(-1) 
genr infl_tgt=infl_tgt*0.01
genr infl_EU=(cpi_euro/cpi_euro(-12))-1
genr spread_US= baa-aaa
genr spread_EU=trit10t-trbd10t
genr spread_US1=baa-gs10
genr EA_probinflh3=infl3+infl4+infllarge5
genr EA_avinfl=1*infl1+2*infl2+3*infl3+4*infl4+5*infllarge5
genr tsus=gs10-tb3ms
genr ts_expmon=tsus-tp10
smpl 1999:1 2019:9
eurostr=eonia
smpl 1999:1 2022:12
genr infl_usbe2=sveny02-tipsy02
genr infl_usbe10=sveny10-tipsy10

%type="FIGURE_EURO_2"
smpl 2009:10 2023:4
delete {%type}*
	graph {%type}.line EA_probinflh3  EA_avinfl
	{%type}.axis grid zeroline
	{%type}.option linepat	' need to set linepat
	'{%type}.draw(line,left,rgb(180,180,180)) 0
	{%type}.elem(1) lcolor(blue) lpat(solid)  lwidth(2.25) 'symbol(circle) symbolsize(s)
	{%type}.elem(2) lcolor(red) lpat(solid) lwidth(2.25)
	{%type}.name(1) Percent Change HICP over 3 per cent 
	{%type}.name(2) Average Inflation 
	{%type}.addtext(t)  "HICP over 5 years"
	{%type}.axis(r) grid zeroline range(0,5)
		'{%type}.legend columns(1) position(r)
             '  {%type}.draw(shade,bottom) 1980:1 2022:12
              {%type}.draw(line,bottom) 1980
			'{%type}.draw(line,bottom) 1999
%type="figure_trends_us_1"
smpl 1954:7 2023:4
delete {%type}*
	graph {%type}.line infl_US  0.01*fedfund infl_tgt
	{%type}.axis grid zeroline
	{%type}.option linepat	' need to set linepat
	'{%type}.draw(line,left,rgb(180,180,180)) 0
	{%type}.elem(1) lcolor(red) lpat(solid)  lwidth(2.25) 'symbol(circle) symbolsize(s)
	{%type}.elem(2) lcolor(black) lpat(solid) lwidth(1.25)
    {%type}.elem(3) lcolor(blue) lpat(solid) lwidth(1.75)
	{%type}.name(2) Federal Funds Rate
	{%type}.name(1) Annual Inflation
	{%type}.name(3) Federal Reserve Inflation Target 
	{%type}.addtext(t)  "Trends in Inflation and  Monetary Policy. US"
	{%type}.axis(l) grid zeroline range(-0.05,0.20)
		'{%type}.legend columns(1) position(r)
             {%type}.draw(shade,bottom) 1983:1 2023:4
              '{%type}.draw(line,bottom) 1980
			'{%type}.draw(line,bottom) 1999

%type="figure_trends_us_2"
smpl 1999:1 2023:4
delete {%type}*
	graph {%type}.line infl_US  0.01*(baa-gs10)
	{%type}.axis grid zeroline
	{%type}.option linepat	' need to set linepat
	'{%type}.draw(line,left,rgb(180,180,180)) 0
	{%type}.elem(1) lcolor(red) lpat(solid)  lwidth(2.25) 'symbol(circle) symbolsize(s)
	{%type}.elem(2) lcolor(blue) lpat(dash1) lwidth(1.25)
	{%type}.elem(3) lcolor(blue) lpat(dash2) lwidth(2.25)
	{%type}.name(2) 10-Year YTM-BAA
	{%type}.name(3) BAA YIeld
	{%type}.name(1) Annual Inflation
	{%type}.addtext(t)  "Trends in Inflation and  BAA-10Y spread. US"
	{%type}.axis(l) grid zeroline range(-0.02,0.15)
		'{%type}.legend columns(1) position(r)
             '  {%type}.draw(shade,bottom) 1980:1 2022:12
              {%type}.draw(line,bottom) 1980
			'{%type}.draw(line,bottom) 1999
%type="figure_us_be"
smpl 1999:1 2023:6
delete {%type}*
	graph {%type}.line infl_US  0.01*infl_usbe2 0.01*infl_usbe10 0.01*mich
	{%type}.axis grid zeroline
	{%type}.option linepat	' need to set linepat
	'{%type}.draw(line,left,rgb(180,180,180)) 0
	{%type}.elem(1) lcolor(red) lpat(solid)  lwidth(2.25) 'symbol(circle) symbolsize(s)
	{%type}.elem(2) lcolor(blue) lpat(dash1) lwidth(1.25)
	{%type}.elem(3) lcolor(blue) lpat(dash2) lwidth(2.25)
    {%type}.elem(4) lcolor(green) lpat(dash2) lwidth(2.25)

	{%type}.name(1) Annual US  CPI Inflation
	{%type}.name(2) break-even inflation 2-year
	{%type}.name(3) break-even inflation 10-year
     {%type}.name(4) Michigan survey of annual Inflation Expectations

	{%type}.addtext(t)  "Trends in Inflation and  Inflation Expectations. US"
	{%type}.axis(l) grid zeroline range(-0.05,0.10)
		'{%type}.legend columns(1) position(r)
             '  {%type}.draw(shade,bottom) 1980:1 2022:12
              {%type}.draw(line,bottom) 1980
			'{%type}.draw(line,bottom) 1999

%type="figure_trends_eu_1"
smpl 1999:1 2023:4
delete {%type}*
	graph {%type}.line (infl_eu)   0.01*ecbdfr 0.01*ecbmlfr
	{%type}.axis grid zeroline
	{%type}.option linepat	' need to set linepat
	'{%type}.draw(line,left,rgb(180,180,180)) 0
	{%type}.elem(1) lcolor(red) lpat(solid)  lwidth(2.25) 'symbol(circle) symbolsize(s)
	{%type}.elem(2) lcolor(black) lpat(solid) lwidth(1.25)
{%type}.elem(3) lcolor(black) lpat(solid) lwidth(1.25)
		{%type}.name(1) Annual Inflation
      {%type}.name(2) ECB Deposit Facility Rate 
{%type}.name(3) ECB Main Lending Facility Rate 
	{%type}.addtext(t)  "Trends in Inflation and Monetary Policy. Euro Area"
	{%type}.axis(l) grid zeroline range(-0.05,0.15)
		'{%type}.legend columns(1) position(r)
               ' {%type}.draw(shade,bottom) 1970 1985
             ' {%type}.draw(line,bottom) 1991
			'{%type}.draw(line,bottom) 1999

%type="figure_trends_eu_2"
smpl 1999:1 2023:4
delete {%type}*
	graph {%type}.line infl_eu 0.01*(trit10t-trbd10t) 
	{%type}.axis grid zeroline
	{%type}.option linepat	' need to set linepat
	'{%type}.draw(line,left,rgb(180,180,180)) 0
	{%type}.elem(1) lcolor(red) lpat(solid)  lwidth(2.25) 'symbol(circle) symbolsize(s)
	{%type}.elem(2) lcolor(blue) lpat(dash1) lwidth(1.25)
	{%type}.elem(3) lcolor(green) lpat(dash2) lwidth(1.25)
	{%type}.name(2) 10-Year YTM Italy-10-Year YTM Germany
	{%type}.name(3) 10-Year YTM Italy
	{%type}.name(1) Annual Inflation
    	{%type}.addtext(t)  "Trends in Inflation and BTP-Bund spread. Euro Area"
	{%type}.axis(l) grid zeroline range(-0.02,0.15)
		'{%type}.legend columns(1) position(r)
               ' {%type}.draw(shade,bottom) 1970 1985
             ' {%type}.draw(line,bottom) 1991
			'{%type}.draw(line,bottom) 1999

%type="figure_icdem_eu"
smpl 1999:1 2023:4
delete {%type}*
	graph {%type}.line infl_eu 0.01*icdem10
	{%type}.axis grid zeroline
	{%type}.option linepat	' need to set linepat
	'{%type}.draw(line,left,rgb(180,180,180)) 0
	{%type}.elem(1) lcolor(red) lpat(solid)  lwidth(2.25) 'symbol(circle) symbolsize(s)
	{%type}.elem(2) lcolor(blue) lpat(dash1) lwidth(1.25)
	{%type}.elem(3) lcolor(green) lpat(dash2) lwidth(2.25)
	{%type}.name(2) 10-Year Fixed IR on Swaps
	{%type}.name(3) 10-Year YTM Italy
	{%type}.name(1) Annual Inflation
	{%type}.addtext(t)  "Trends in Inflation and 10Y.Euro Area"
	{%type}.axis(l) grid zeroline range(-0.02,0.15)
		'{%type}.legend columns(1) position(r)
               ' {%type}.draw(shade,bottom) 1970 1985
             ' {%type}.draw(line,bottom) 1991
			'{%type}.draw(line,bottom) 1999

smpl 1999:1 2023:4
equation eq_icdem10.ls (icdem10-icdem10(-12))=c(1)+c(2)*(icdem10(-12)-100*infl_eu(-12))+c(4)*100*(infl_eu-infl_eu(-12))

equation eq_it.ls (trit10t-trit10t(-12))=c(1)+c(2)*(trit10t(-12)-icdem10(-12)-c(4)*100*infl_eu)+c(3)*(icdem10-icdem10(-12))

equation eq_bd.ls (trbd10t-trbd10t(-12))=c(1)+c(2)*(trbd10t(-12)-icdem10(-12)-c(4)*100*infl_eu)+c(3)*(icdem10-icdem10(-12))


