cd "/Users/fumiyau/Dropbox (Princeton)/4.EAMwithCredentials/Explaining-Homogamy-Decline/"

use "1.Data/Figure3.dta",clear

bysort HusEdu marco1: egen HusEduSum=sum(freq)
bysort WifEdu marco1: egen WifEduSum=sum(freq)
bysort marco1: egen HusWifSum=sum(freq)
sort HusEdu marco1
gen exp=(HusEduSum*WifEduSum)/HusWifSum
keep HusEdu WifEdu marco1 freq exp

gen eam=0
replace eam=1 if WifEdu==HusEdu
replace eam=2 if HusEdu>WifEdu
replace eam=3 if HusEdu<WifEdu

bysort eam marco1: egen freqx=sum(freq)
bysort eam marco1: egen expx=sum(exp)
bysort eam marco1: gen n=_n
keep if n==1
keep freqx expx eam marco1
bysort marco1: egen freqp=sum(freqx)
bysort marco1: egen expp=sum(expx)

gen id=marco1
reshape wide marco1 expx freqx, i(id) j(eam)
keep marco11 freqx1 expx1 freqx2 expx2 freqx3 expx3
gen freqsum=freqx1+freqx2+freqx3
gen expsum=expx1+expx2+expx3
forvalues i=1/3{
gen freqp`i'=(freqx`i'/freqsum)*100
gen expp`i'=(expx`i'/expsum)*100
qui reg freqp`i' i.marco11
qui estpost margins marco11 
est sto modelf`i'
qui reg expp`i' i.marco11
qui estpost margins marco11 
est sto modele`i'
}
label variable freqp1 "Homogamy"
label variable freqp2 "Hypergamy"
label variable freqp3 "Hypogamy"
label variable expp1 "Homogamy"
label variable expp2 "Hypergamy"
label variable expp3 "Hypogamy"

label variable marco11 "Marriage cohorts"
label define yearl 1"1970-1984" 2"1985-1999"3"2000-2015",replace
label values marco11 yearl 

coefplot (modelf1, label(Homogamy) lc(eltblue) lp(shortdash) recast(connected) ms(O) mc(eltblue)) ///
(modelf2, label(Hypergamy) lc(orange) lp(shortdash) recast(connected) ms(Oh) mc(orange)) ///
(modelf3, label(Hypogamy) lc(purple) lp(shortdash) recast(connected) ms(S) mc(purple)) ///
, vert offset(0) noci scheme(plotplain) legend(row(1) size(medium) rowgap(small) position(6) )  ///
ylabel(0(10)60, nogrid ) xlabel(,nogrid angle(45)) ytitle(%) xtitle() title((a) observed) ///
saving(4.Fig/Fig3a.gph,replace)

coefplot (modele1, label(Homogamy) lc(eltblue) lp(shortdash) recast(connected) ms(O) mc(eltblue)) ///
(modele2, label(Hypergamy) lc(orange) lp(shortdash) recast(connected) ms(Oh) mc(orange)) ///
(modele3, label(Hypogamy) lc(purple) lp(shortdash) recast(connected) ms(S) mc(purple)) ///
, vert offset(0) noci scheme(plotplain) legend(row(1) size(medium) rowgap(small) position(6) )  ///
ylabel(0(10)60, nogrid) xlabel(,nogrid angle(45)) ytitle(%) xtitle() title((b) expected) ///
saving(4.Fig/Fig3b.gph,replace)

grc1leg 4.Fig/Fig3a.gph 4.Fig/Fig3b.gph , legendfrom(4.Fig/Fig3a.gph) 

graph export 4.Fig/Figure3.pdf,replace
