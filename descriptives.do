*updated descriptives 

/*===============说明================================
结果文档： 1. descriptive.xls
=======================================================
*/

// net install cleanplots, from("https://tdmize.github.io/data/cleanplots")   //改变绘图scheme
// set scheme cleanplots

cd "/Users/priscillawang/Dropbox/microcensus/data" 
*cd "C:\Users\wdhec\Dropbox\microcensus\data"  //laptop 



use append_sample5.dta, clear    // [此处数据版本换成5年普查pool到一起的数据]

keep if inrange(age,15, 50)
keep if gender==2
keep if inrange(nationality, 1,56)
drop if nationality==1 
drop if year ==1990
drop if marry==1 
// drop if year==20102 
// drop if year==20151

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
 recode marry  (2/3=1 "在婚有配偶") (4/5 =2 "离婚/丧偶"), gen(mar)

 
 g mig2=(inrange(mig_state,6,9))
 la var mig2 "省际迁移"
 
 g ruralland2=(ruralland==1)
 
 
 drop if hukou_type==0
 g ruralhukou= (hukou_type==1)
 la var ruralhukou "农业户口"
 
 g agesq= age* age 
 
 replace ruralhukou=ruralland2 if year==20151 | year==20152
 
 *keep nationalitites that consistently appear across the sample
 bysort nationality year: gen nyear=_n ==1
 bysort nationality: egen tyear=total(nyear)
 
 *tab nationality if tyear<6 
 drop if tyear <6 

 clonevar minzu =nationality
 
 
   table1, by(year) vars(nborn contn \ edu cat  \ age contn \ agesq conts \ ruralhukou bin  \ occup cat  \ mar cat  \ mig2 bin  \ kitchen bin  \ toilet bin ) percent  cformat(%9.2f) 

 
  *!!! ssc install table1 , replace  // install table1 if not  
  table1, by(year) vars(nborn contn \ edu cat  \ age contn \ agesq conts \ ruralhukou bin  \ occup cat  \ mar cat  \ mig2 bin  \ kitchen bin  \ toilet bin ) percent  cformat(%9.2f) saving(descriptive.xls, replace)
