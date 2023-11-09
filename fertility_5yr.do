*ethnic fertility indicators, 5year age group 
*created on 03192022

cd "/Users/priscillawang/Dropbox/microcensus/data/"   //mac 
*cd "C:\Users\wdhec\Dropbox\microcensus\data"  //laptop 

*===========================

* program to calcuate asfr,tfr, ppr,mean ageof 1st childbearing(mage1st) 
 capture program drop fertility
 
 program define fertility   
 
 levelsof `1',local(grp) 
 
 foreach i  of local grp {
 preserve 
 keep if `1' ==`i'
 collapse (count) id , by(agegp)
 tempfile agegp
 save `agegp'
 restore 
 
 preserve 
 keep if `1'==`i'
 collapse (sum) lbirth p1 p2 p3 pt, by(agegp)
 merge 1:1 agegp using `agegp' ,nogen 
 rename id npop 
 
 g asfr=lbirth/npop 
  
  g ppr1=p1/npop 
  g ppr2=p2/npop
  g ppr3=p3/npop 
  g pprt=pt/npop 

  *mean age at first child  
  egen mage1_num= total(agegp*ppr1)  //numerator 
  egen mage1_deno =total(ppr1)
  g mage1st = mage1_num/ mage1_deno
  egen tfr=total(asfr*5)
  
  keep agegp asfr ppr* mage1st tfr 
  
  rename asfr asfr_e`i'
  rename ppr1 ppr1_e`i'
  rename ppr2 ppr2_e`i'
  rename ppr3 ppr3_e`i'
  rename pprt pprt_e`i'
  rename mage1st mage1st_e`i'
  rename tfr tfr_e`i'
  
  mkmat   *_e`i', mat(e`i')
  mat list e`i' 
 restore 
 
 }
 
 clear 
 foreach i of local grp{
 svmat e`i',names(col) 
 
}
 gen agegp = _n 
 
 la def agegp 1  "15-19" 2 "20-24" 3 "25-29" 4 "30-34" 5"35-39" 6 "40-44" 7"45-49", modify
 la val agegp agegp 
 end 
 

 
*program to recode ethnic cluster
 /* 
1 汉族;2 蒙古族;3 回族;4 藏族;5 维吾尔族;6 苗族;7 彝族;8 壮族;9 布依 族;10 朝鲜族;11 满族;12 侗族;13 瑶族;14 白族;15 土家族;16 哈尼族;17 哈 萨克族;18 傣族;19 黎族;20 傈僳族;21 佤族;22 畲族;23 高山族;24 拉祜族;25 水族;26 东乡族;27 纳西族;28 景颇族;29 柯尔克孜族;30 土族;31 达斡尔族;32 仫佬族;33 羌族;34 布朗族;35 撒拉族;36 毛南族;37 仡佬族;38 锡伯族;39 阿昌 族;40 普米族;41 塔吉克族;42 怒族;43 乌孜别克族;44 俄罗斯族;45 鄂温克族; 46 德昂族;47 保安族;48 裕固族;49 京族;50 塔塔尔族;51 独龙族;52 鄂伦春族; 53 赫哲族;54 门巴族;55 珞巴族;56 基诺族;57 其他未识别民族;58 外国人加入中 国籍

1.藏族；裕固族，门巴族，珞巴族，土族
2.回族；撒拉族、东乡族、保安族
3.满族；赫哲族和锡伯族
4.蒙古族；鄂伦春族、鄂温克族、达斡尔族
5.朝鲜族
6.维吾尔族
7.哈萨克族、乌孜别克族、塔吉克族、柯尔克孜族、塔塔尔族、俄罗斯族 
8.南方各族群：瑶族，彝族，土家族，畲族，傈僳族，毛南族，侗族，布依族，仡佬族，东乡族，白族，哈尼族，傣族，傈僳族，佤族，畲族，高山族，拉枯族，纳西族，景颇族，仫佬族，布朗族，水族，羌族，阿昌族，普米族，怒族，德昂族，京族，独龙族，基诺族
9.壮族
10.苗族
11.彝族
*/

*label ethnic
capture program drop eth

program define eth
 *recode 
 #delimit ; 
 recode `1' (4 48 54 55 30 =  1) 
           (3 35 26 47    =  2) 
		   (11 53 38      =  3) 
		   (2 52 45 31    =  4)
		   (10            =  5)     
		   (5             =  6)  
		   (17 43 41 29 50 44 = 7)  
		   (8             =  9 ) 
		   (6 = 10)  (7 = 11)
		   (1 = 0 ) (else = 8 ), gen(ep)		   
           ;
#delimit cr 	


 #delimit ; 
  la def ep 1 "藏族,裕固族,门巴族,珞巴族,土族"  
            2 "回族,撒拉族,东乡族,保安族" 
            3 "满族,赫哲族,锡伯族"
		    4 "蒙古族,鄂伦春族,鄂温克族,达斡尔族"
		    5 "朝鲜族"
		    6 "维吾尔族"
		    7 "哈萨克族,乌孜别克族,塔吉克族,柯尔克孜族,塔塔尔族,俄罗斯族"
		    8 "南方各族群"
		    9 "壮族"
		    10 "苗族"
		    11 "彝族"
			0 "汉族" , modify 
		 ;
 #delimit cr 	
  la val ep ep 
 end 
 
*======================================================================================
 *for each census, need to calculate 
 *lbirth : number of live birth last year(acount for multiple birth if possible)
 *p1-p3 : birth order of last year's birth  
 
*82 
 use 1982census.dta, clear
 
  keep if inrange(age,15, 49) //age
  
  *note : age need to be recorded as midpoint so that to calculate mean age at first birth  
  recode age (15/19=17 "15-19") (20/24=22 "20-24") (25/29=27 "25-29") (30/34=32 "30-34") (35/39=37 "35-39") ///
             (40/44=42 "40-44") (45/50=47 "45-49"), gen(agegp)
			 
  keep if sex==2               //female only 
  keep if inrange(nationty, 1, 56)
 
 g lbirth = (inrange(bthord81,1,5))

   *birth order of last year
  g p1=(bthord81==1)
  g p2=(bthord81==2)
  g p3=(inrange(bthord81,3,5))
  egen pt=rowtotal(p1 p2 p3)  //had birth 

  keep nationty  agegp lbirth  p1 p2 p3 pt   
  gen id=_n 

save temp82,replace  

use temp82,clear
  eth nationty
  fertility ep 
  save fer82ep_5yr,replace   
 
*single 
 use temp82, clear 
 fertility nationty
save fer82single_5yr,replace 

erase temp82.dta  



*---------------
 *90
//  use 1990.dta, clear  //544m      n=11568585 age mssing 
//  list age_c age_y age_m  relation in 1/10
   
*misschk age_c age_y age_m. //20% missing 


// use 1990census.dta,clear  //n=11475065 age is very strange 
// list age relation if county==1 & sex==1 & nation==13

use 1990_ipums.dta, clear  //N=11835947
  keep if inrange(age,15, 49) //age
  recode age (15/19=17 "15-19") (20/24=22 "20-24") (25/29=27 "25-29") (30/34=32 "30-34") (35/39=37 "35-39") ///
             (40/44=42 "40-44") (45/50=47 "45-49"), gen(agegp)
  keep if sex==2               //female only 
  keep if inrange(ethniccn, 1, 56) 
 
  g lbirth=birthslyr
  replace lbirth = 0 if marst==0 & lbirth==. 
  
  g nbe=chborn 
  recode nbe (0=0)  (1=1) (2=2) (3/9=3),gen(nbeb)
  
 *birth order of last year
 g p1=(inrange(lbirth, 1, 2) & nbeb==1)
 g p2=(inrange(lbirth, 1, 2) & nbeb==2)
 g p3=(inrange(lbirth, 1, 2) & nbeb==3)
 g pt=lbirth 
 
 keep ethniccn agegp lbirth  p1 p2 p3 pt   

 gen id=_n
save temp90,replace 

use temp90,clear 
 eth ethniccn 
  fertility ep
  save fer90ep_5yr,replace 
  
*single ethnic   
use temp90,clear 
  fertility ethniccn
  save fer90single_5yr, replace 

erase temp90.dta  



*------------------- 
 *2000
use 2000.dta,clear 
replace age=int(age)
 keep if inrange(age,15, 49) //age
  recode age (15/19=17 "15-19") (20/24=22 "20-24") (25/29=27 "25-29") (30/34=32 "30-34") (35/39=37 "35-39") ///
             (40/44=42 "40-44") (45/50=47 "45-49"), gen(agegp)
 
 keep if r3==2               //female only 
 keep if inrange(r5,1,56)
 
 g b1st=(inrange(r263,1,2))
 g b2nd=(inrange(r265,1,2))
 
 g       lbirth=1 if b1st==1  // one birth 
 replace lbirth=2 if b2nd==1  // 2 birth 
 replace lbirth=0 if lbirth==.
  
 g nbe = r251+r252  //number of birth everborn 
 replace nbe=0 if r23==1 & nbe==. 

 recode nbe (0 =0 ) (1=1) (2=2) (3/max=3) (.=.),gen(nbeb)

 *r261: if given birth last year 1 not 2 yes
  g p1=(nbeb==1 & inrange(lbirth, 1, 2)) 
  g p2=(nbeb==2 & inrange(lbirth, 1, 2))
  g p3=(nbeb==3 & inrange(lbirth, 1, 2))
  g pt= lbirth 
  drop if nbe==.  
 keep r5 agegp lbirth  p1 p2 p3 pt   
  g id=_n
save temp00,replace 

  eth r5 
  fertility ep   
save fer00ep_5yr, replace 

*single ethnic group 
use temp00,clear 
 fertility r5 
save fer00single_5yr, replace 

erase temp00.dta 

  
 *05
 use 2005.dta, clear 
  keep if inrange(r5,1,56)   // remove foreigners and unidentified ethnic groups  
  keep if inrange(age,15, 49) //age 
  keep if r3==2               //female only 
  recode age (15/19=17 "15-19") (20/24=22 "20-24") (25/29=27 "25-29") (30/34=32 "30-34") (35/39=37 "35-39") ///
             (40/44=42 "40-44") (45/50=47 "45-49"), gen(agegp)
  g b1st=(inrange(r3502,1,2))  // first birth if sex is valid
  g b2nd=(inrange(r3504,1,2))  // second birth if sex is valid 
  
  *calcuate ASFR
  g       lbirth=1 if b1st==1  // one birth 
  replace lbirth=2 if b2nd==1  // 2 birth 
  replace lbirth=0 if lbirth==. 
  
  *PPR
  g nbe = r3301+r3302  //number of birth everborn 
  replace nbe=0 if r31==1 &nbe==.
  recode nbe (0 =0 ) (1=1) (2=2) (3/max=3)(.=.),gen(nbeb)
  *r35: if given birth last year 1 not 2 yes
  g p1=(nbeb==1  & inrange(lbirth, 1, 2)) 
  g p2=(nbeb==2  & inrange(lbirth, 1, 2))
  g p3=(nbeb==3  & inrange(lbirth, 1, 2))
  g pt=lbirth

 keep r5 agegp lbirth  p1 p2 p3 pt   
  g id=_n
  
save temp05.dta, replace  
  eth r5 
  fertility ep
  save fer05ep_5yr.dta, replace 

use temp05.dta, clear 
  fertility r5
  save fer05single_5yr.dta, replace 
  
erase temp05.dta 



