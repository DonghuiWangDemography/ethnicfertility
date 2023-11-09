*use pooled data from 00-15: individual determinants of nborn 
*创建日期： 03032022 
*更新日期： 03042022 
*更新日期：04282022 : first revision 1. add level 2 measurement; 2.  null model 3.pooled model with full interaction of time 


/*===============说明================================
结果文档： 1. eth_fer_m2.csv

=======================================================
*/

// net install cleanplots, from("https://tdmize.github.io/data/cleanplots")   //改变绘图scheme
// set scheme cleanplots

cd "/Users/priscillawang/Dropbox/microcensus/data" 
*cd "C:\Users\wdhec\Dropbox\microcensus\data"  //laptop 

global date "042820222"
log using fertility-analysis03_$date, replace 


use append_sample5.dta, clear    // [此处数据版本换成5年普查pool到一起的数据]

keep if inrange(age,15, 50)
keep if gender==2
keep if inrange(nationality, 1,56)
drop if nationality==1 
drop if year ==1990
drop if marry==1 

g       nborn = offspring_female+offspring_male
replace nborn=0 if marry==1 & nborn==.

*last yr's birth 
 g lbirth=(twelve_birth ==1)
 
 
*-------level 1 ---------------------
*living condition 
 g kitchen=(hs_kitchen==1 | hs_kitchen==2)
 g toilet=(inrange(hs_toilet,1,3))
 drop if hs_kitchen==. | hs_toilet==.
 
 la var kitchen "室内有厨房"
 la var toilet "室内有厕所"
 *edu 
 recode edu_attainment (1/2=1 "小学及以下") (3=2 "初中") (4=3 "高中/中专/中职") (5/8=4 "大学及以上"), gen(edu)
 g inwork=(work==1 | work==2)
 
 lab define newlabel 0 "无工作" , modify
 lab val occupation newlabel
 drop if occupation == -1 
 recode  occupation (1/2 =1 "单位负责人或专业技术人员") (3 =2  "办事人员") (4 =3 "商业/服务业人员") (5 = 4 "农业生产人员" ) (6/8=5 "其他"), gen (occup)

 *marstat 
 recode marry  (1=1 "单身") (2/3=2 "在婚有配偶") (4/5 =3 "离婚/丧偶"), gen(mar)

 
 g mig2=(inrange(mig_state,6,9))
 la var mig2 "省际迁移"
 
 g ruralland2=(ruralland==1)
 
 
 drop if hukou_type==0
 g ruralhukou= (hukou_type==1)
 la var ruralhukou "农村户口"
 
 g agesq= age* age 
 
 replace ruralhukou=ruralland2 if year==20151 | year==20152
 
 *keep nationalitites that consistently appear across the sample
 bysort nationality year: gen nyear=_n ==1
 bysort nationality: egen tyear=total(nyear)
 
 *tab nationality if tyear<6 
 drop if tyear <6 
 *calculate number of observations per nationality by year 
 *tab nationality year 
 *ssc install unique 
 *unique nationality 
 clonevar minzu =nationality
tempfile ind
save `ind.dta'

*-----level2------------------------
import excel "少数民族指标.xlsx", sheet("huang") firstrow clear 
 rename 城镇化 urban00 
 rename 非农 nonag00 
 rename 性别年龄 sexratio00
 drop if sexratio00==.

 rename 健康 health00
 destring health00, replace 
 rename 教育 eduh00
 
tempfile huang
save `huang.dta' 

*edu , ag, 
import excel "少数民族指标.xlsx", sheet("dispersion") firstrow clear
tempfile disper
save `disper.dta' 

import excel "少数民族指标.xlsx" , sheet("demo") firstrow clear 
merge 1:1 民族 using `huang.dta', keep(matched) nogen 

merge 1:1 民族 using `disper.dta'
 keep if _merge ==3 
 drop _merge 
 rename 编号 nationality 
merge 1:m nationality  using `ind.dta'
 keep if _merge ==3
 drop _merge 
 unique nationality 
 rename 离散度 disper 

 *standardized variable 
 *nonag00 urban00 edu00  imr00 disper
 egen stnonag00=std(nonag00)
 egen sturban00=std(urban00)
 egen stedu00=std(edu00)
 egen stimr00=std(imr00)
 egen stdisper=std(disper)
 
 *!!! ssc install table1 , replace  // install table1 if not  
//  table1, by(year) vars(nborn contn \ edu cat \ occup cat \ mar cat \ mig2 bin \ kitchen bin \ toilet bin  ) percent format(%2.3f) saving(descriptive.xls, replace


*-----------------------------------------------------------------------   
*1. null model 
 levelsof year, local(year)
 foreach x of local year{
 display "year ===>`x' "
 xtmixed nborn if year==`x' || nationality : 
 estimate store nc`x'
 estat icc 
 scalar icc`x'=r(icc2)
 }

 esttab nc2000 nc2005 nc20101 nc20102 nc20151 nc20152  using eth_fer_null.csv , b(3) se(3) la  mtitles(2000 2005 20101 20102 20151 20152 )    ///
   transform(ln*: exp(@) exp(@))   ///
    eqlabels("" "民族层面随机截距" "残差项" , none) ///
     varlabels(,elist(weight:_cons "{break}{hline @width}"))  ///
     varwidth(13) replace 	   

	 
  mat icc=J(6,1,-99)
  mat icc[1,1]= icc2000
  mat icc[2,1]= icc2005
  mat icc[3,1]= icc20101
  mat icc[4,1]= icc20102
  mat icc[5,1]= icc20151
  mat icc[6,1]= icc20152 
  
  mat list icc	 
	 
	 
	 
*2.m1 with level 1 predictor 
  levelsof year,local(year)
  
  foreach x of local year {
  display "year ===>`x' "
  xtmixed nborn  i.edu age agesq ruralhukou  i.mar  mig2 i.occup kitchen toilet  if year==`x' || nationality: 
  estimate store c`x'
  estat icc 
  scalar icc`x'=r(icc2)
  
  preserve 
  predict r*, reffects reses(sd*) 
  keep if year==`x'
  keep nationality r1 sd1
  duplicates drop 
  
  sort r1
  g nrank=_n
  serrbar r1 sd1 nrank, scale(1.96) mvopts(mlabel(nationality) mlabangle(45) mlabsize(*0.55) mlabposition(6) mlabgap(*10)) ///
        ytitle (随机截距预测值) xtitle(民族排名) title(`x' 年)
   graph save re`x', replace 	
  restore 
  
 }
   
*  ssc install estout, replace   // install estout if not 
 esttab c2000 c2005 c20101 c20102 c20151 c20152  using eth_fer_m1.csv , b(3) se(3) la  mtitles(2000 2005 20101 20102 20151 20152 )    ///
   transform(ln*: exp(@) exp(@))   ///
    eqlabels("" "民族层面随机截距" "残差项" , none) ///
     varlabels(,elist(weight:_cons "{break}{hline @width}"))  ///
     varwidth(13) replace 	   
	 
  mat icc=J(6,1,-99)
  mat icc[1,1]= icc2000
  mat icc[2,1]= icc2005
  mat icc[3,1]= icc20101
  mat icc[4,1]= icc20102
  mat icc[5,1]= icc20151
  mat icc[6,1]= icc20152 
  
  mat list icc

*--------------
*3. m2: model with level 1+ level2 predictor 
  
  levelsof year,local(year)
  foreach x of local year {
    *standardized version 
  display "standardized year ===>`x' "
  xtmixed nborn  i.edu age agesq ruralhukou  i.mar  mig2 i.occup kitchen toilet stnonag00 sturban00 stedu00  stimr00 stdisper  if year==`x' || nationality: 
 
  display "year ===>`x' "
  xtmixed nborn  i.edu age agesq ruralhukou  i.mar  mig2 i.occup kitchen toilet nonag00 urban00 edu00  imr00 disper  if year==`x' || nationality:
  estat icc 
  scalar icc`x'=r(icc2)
  estimate store c`x'
  
//   preserve 
//   predict r*, reffects reses(sd*) 
//   keep if year==`x'
//   keep minzu r1 sd1
//   duplicates drop 
  
//   sort r1
//   g nrank=_n
//   serrbar r1 sd1 nrank, scale(1.96) mvopts(mlabel(minzu) mlabangle(90) mlabsize(*0.8) mlabposition(6) mlabgap(*15)) ///
//         ytitle (随机截距预测值) xtitle(民族排名) title(`x' 年)
//    graph save re`x', replace 	
//   restore 
  
  }  
  
 esttab c2000 c2005 c20101 c20102 c20151 c20152  using eth_fer_m2.csv , b(3) se(3) la  mtitles(2000 2005 20101 20102 20151 20152 )    ///
   transform(ln*: exp(@) exp(@))   ///
    eqlabels("" "民族层面随机截距" "残差项" , none) ///
     varlabels(,elist(weight:_cons "{break}{hline @width}"))  ///
     varwidth(13) replace   
  
 
  mat icc=J(6,1,-99)
  mat icc[1,1]= icc2000
  mat icc[2,1]= icc2005
  mat icc[3,1]= icc20101
  mat icc[4,1]= icc20102
  mat icc[5,1]= icc20151
  mat icc[6,1]= icc20152 
  
  mat list icc 
 
  
  
// *model3: interaction with time    
xtmixed nborn  i.edu##i.year c.age##i.year c.agesq##i.year i.ruralhukou##i.year  i.mar##i.year  mig2##i.year i.occup##i.year kitchen##i.year toilet##i.year c.nonag00##i.year c.urban00##i.year c.edu00##i.year c.imr00##i.year c.disper##i.year || nationality: 
 estimate store m3
 esttab m3 using eth_fer_m3.csv ,  b(3) se(3) la  replace 
  

*model4: logistics with ever had birth as dv 
  levelsof year,local(year)
  foreach x of local year {
  display "year ===>`x' "
  melogit  lbirth  i.edu age agesq ruralhukou  i.mar  mig2 i.occup kitchen toilet nonag00 urban00 edu00  imr00 disper  if year==`x' || nationality: 
  estimate store m4`x'

  } 

 esttab m42000 m42005 m420101 m420102 m420151 m420152  using eth_fer_m4.csv , b(3) se(3) la  mtitles(2000 2005 20101 20102 20151 20152 )    ///
   transform(ln*: exp(@) exp(@))   ///
    eqlabels("" "民族层面随机截距" "残差项" , none) ///
     varlabels(,elist(weight:_cons "{break}{hline @width}"))  ///
     varwidth(13) replace   
	 
 
  mat icc=J(6,1,-99)
  mat icc[1,1]= icc2000
  mat icc[2,1]= icc2005
  mat icc[3,1]= icc20101
  mat icc[4,1]= icc20102
  mat icc[5,1]= icc20151
  mat icc[6,1]= icc20152 
  
  mat list icc 	 
	 
    
log close 

*20101, 20152

 graph use re20152.gph , scheme(cleanplots)

 levelsof year,local(year)
  foreach x of local year {
  graph use re`x'.gph, scheme(cleanplots)
   graph save re`x', replace 	
  }

//   graph use re20152.gph 
  
  
//   graph combine re2000.gph re2005.gph re20101.gph re20152.gph
