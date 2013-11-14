  
      subroutine rrqqbar2gght5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh51J1  
      doubleprecision rrqqbar2ggh51J2  
      doubleprecision rrqqbar2gght5s1e1  
      doubleprecision rrqqbar2gght5s1e0  
      doubleprecision rrqqbar2gght5s1em1  
      doubleprecision rrqqbar2gght5s1em2  
      doubleprecision rrqqbar2gght5s1em3  
      doubleprecision rrqqbar2gght5s1em4  
      doubleprecision rrqqbar2gght5s2e1  
      doubleprecision rrqqbar2gght5s2e0  
      doubleprecision rrqqbar2gght5s2em1  
      doubleprecision rrqqbar2gght5s2em2  
      doubleprecision rrqqbar2gght5s2em3  
      doubleprecision rrqqbar2gght5s2em4  
      doubleprecision rrqqbar2gght5s3e1  
      doubleprecision rrqqbar2gght5s3e0  
      doubleprecision rrqqbar2gght5s3em1  
      doubleprecision rrqqbar2gght5s3em2  
      doubleprecision rrqqbar2gght5s3em3  
      doubleprecision rrqqbar2gght5s3em4  
      doubleprecision rrqqbar2gght5s4e1  
      doubleprecision rrqqbar2gght5s4e0  
      doubleprecision rrqqbar2gght5s4em1  
      doubleprecision rrqqbar2gght5s4em2  
      doubleprecision rrqqbar2gght5s4em3  
      doubleprecision rrqqbar2gght5s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2gght5s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2gght5s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2gght5s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2gght5s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2gght5s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2gght5s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2gght5s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2gght5s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2gght5s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2gght5s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2gght5s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2gght5s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght5s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = t7 * pi
      t9 = pi ** 2
      t11 = lh ** 2
      t13 = 0.30D2 * t9 - 0.180D3 * t11
      t16 = 0.1D1 / t1
      t18 = s ** 2
      t19 = 0.1D1 / t18
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t22 = x3 * t21
      t23 = x4 * pi
      t24 = Sin(t23)
      t25 = t24 ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t28 = t25 * t27
      t29 = -0.1D1 + x3
      t30 = 0.1D1 / t29
      t31 = t28 * t30
      t34 = log(-0.4D1 * t22 * t31)
      t36 = cos(t23)
      t38 = Sqrt(-x3 * t29)
      t43 = 0.1D1 / (-x3 - z + 0.2D1 * t36 * t38 * z)
      t47 = log(0.4D1 * t22 * t28)
      t52 = t47 ** 2
      t54 = t34 ** 2
      t69 = 0.240D3 * zeta3 + 0.120D3 * t11 * lh - 0.60D2 * lh * t9
      t89 = 0.1D1 / x3
      t92 = t21 * t25
      t93 = t92 * t27
      t95 = log(0.4D1 * t93)
      t96 = t95 ** 2
      t97 = t96 * pi
      t102 = t96 * t95 * pi
      t104 = t95 * pi
      t116 = t9 ** 2
      t117 = t11 ** 2
      t125 = t96 ** 2
      t133 = pi * t16
      t134 = x1 ** 2
      t135 = t22 * t134
      t138 = log(-0.4D1 * t135 * t31)
      t139 = t138 * z
      t141 = t138 ** 2
      t151 = log(0.4D1 * t22 * t134 * t25 * t27)
      t153 = t151 ** 2
      t160 = lh * t19
      t161 = z * t3
      t170 = t13 * t19
      t173 = t7 + z * t7 * t43
      t178 = 0.1D1 / x1
      t181 = t21 * t134
      t184 = log(0.4D1 * t181 * t28)
      t189 = t184 ** 2
      t199 = t69 * t19
      t201 = t133 * t199 * t7
      t212 = x2 ** 2
      t213 = x3 * t212
      t214 = t213 * t134
      t219 = log(-0.4D1 * t214 * t92 * t27 * t30)
      t226 = log(0.4D1 * t214 * t93)
      t232 = -t173
      t238 = 0.1D1 / x2
      t239 = t238 * t178
      t242 = t212 * t134
      t245 = log(0.4D1 * t242 * t93)
      t247 = t245 ** 2
      t265 = t213 * t21
      t268 = log(-0.4D1 * t265 * t31)
      t269 = t268 * z
      t271 = t268 ** 2
      t279 = log(0.4D1 * t213 * t93)
      t281 = t279 ** 2
      t302 = t212 * t21
      t305 = log(0.4D1 * t302 * t28)
      t310 = t305 ** 2
      t330 = ((0.180D3 * t4 * lh + t8 * t13) * t16 * t19 * (t34 * z * t4
     #3 + t47) - 0.90D2 * t8 * t16 * t19 * (t52 * t47 / 0.6D1 + t54 * t3
     #4 * z * t43 / 0.6D1) + (t4 * t13 + t8 * t69) * t16 * t19 * (-z * t
     #43 - 0.1D1) + (0.180D3 * t8 * lh - 0.90D2 * t4) * t16 * t19 * (-t5
     #4 * z * t43 / 0.2D1 - t52 / 0.2D1)) * t89 / 0.2880D4 - (0.90D2 * t
     #97 * lh + pi * t69 + 0.15D2 * t102 - t104 * t13) * t16 * t19 * t3 
     #/ 0.2880D4 - (-0.30D2 * t102 * lh + t97 * t13 / 0.2D1 - t104 * t69
     # + pi * (-t116 - 0.60D2 * t117 - 0.480D3 * lh * zeta3 + 0.60D2 * t
     #11 * t9) - 0.15D2 / 0.4D1 * t125 * pi) * t16 * t19 * t7 / 0.2880D4
     # - (-0.90D2 * t133 * t19 * ((-t139 * t3 + t141 * z * t7 / 0.2D1) *
     # t43 - t151 * t3 + t153 * t7 / 0.2D1) + 0.180D3 * t133 * t160 * ((
     #t161 - t139 * t7) * t43 + t3 - t151 * t7) + t133 * t170 * t173) * 
     #t89 * t178 / 0.1440D4 + (t133 * t170 * (-t3 + t184 * t7) - 0.90D2 
     #* t133 * t19 * (-t189 * t3 / 0.2D1 + t189 * t184 * t7 / 0.6D1) - t
     #201 + 0.180D3 * t133 * t160 * (t184 * t3 - t189 * t7 / 0.2D1)) * t
     #178 / 0.1440D4 + (-0.90D2 * t133 * t19 * (-(t161 - t219 * z * t7) 
     #* t43 - t3 + t226 * t7) + 0.180D3 * t133 * t160 * t232) * t89 * t2
     #39 / 0.720D3 + (-0.90D2 * t133 * t19 * (t245 * t3 - t247 * t7 / 0.
     #2D1) + 0.180D3 * t133 * t160 * (-t3 + t245 * t7) - t133 * t170 * t
     #7) * t238 * t178 / 0.720D3 + (-0.90D2 * t133 * t19 * (-(-t269 * t3
     # + t271 * z * t7 / 0.2D1) * t43 + t279 * t3 - t281 * t7 / 0.2D1) +
     # 0.180D3 * t133 * t160 * (-(t161 - t269 * t7) * t43 - t3 + t279 * 
     #t7) + t133 * t170 * t232) * t89 * t238 / 0.1440D4 - (t133 * t170 *
     # (t3 - t305 * t7) - 0.90D2 * t133 * t19 * (t310 * t3 / 0.2D1 - t31
     #0 * t305 * t7 / 0.6D1) + t201 + 0.180D3 * t133 * t160 * (-t305 * t
     #3 + t310 * t7 / 0.2D1)) * t238 / 0.1440D4
      t331 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t330)
      t333 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t330)
      t335 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t330)
      t337 = t2 * x1
      t338 = -0.1D1 + x1
      t339 = x1 * z
      t340 = 0.1D1 - x1 + t339
      t341 = 0.1D1 / t340
      t343 = t2 * t338 * t341
      t344 = s * t26
      t346 = t338 * x1 * t341
      t347 = t344 * t346
      t349 = t181 * x3 * t341
      t350 = t338 ** 2
      t355 = log(-0.4D1 * t349 * t28 * t350 * t30)
      t356 = t355 * z
      t357 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t343
     #, t337, 0.0D0, -t347)
      t360 = t355 ** 2
      t362 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t343
     #, t337, 0.0D0, -t347)
      t363 = t340 * t362
      t367 = x3 * x1
      t368 = 0.2D1 * t367
      t369 = t367 * z
      t370 = 0.3D1 * t369
      t371 = x3 * t134
      t372 = x3 * t340
      t374 = Sqrt(-t372 * t29)
      t378 = x1 * t20
      t379 = x3 * t20
      t380 = t379 * x1
      t382 = 0.2D1 * t371 * z
      t383 = t371 * t20
      t384 = -z + t368 + t339 - t370 - t371 - x3 + 0.2D1 * t36 * t374 * 
     #z - t378 + t380 + t382 - t383
      t385 = 0.1D1 / t384
      t388 = t28 * t341 * t350
      t391 = log(0.4D1 * t135 * t388)
      t393 = t391 ** 2
      t400 = z * t340
      t401 = t400 * t357
      t412 = -t400 * t362 * t385 - t362
      t420 = t27 * t341
      t424 = log(0.4D1 * t181 * t25 * t420 * t350)
      t429 = t424 ** 2
      t454 = log(0.4D1 * t213 * t181 * t388)
      t461 = log(-0.4D1 * t349 * t28 * t212 * t350 * t30)
      t478 = t242 * t21
      t481 = log(0.4D1 * t478 * t388)
      t483 = t481 ** 2
      t501 = -(-0.90D2 * t133 * t19 * (-(-t356 * t340 * t357 + t360 * z 
     #* t363 / 0.2D1) * t385 + t391 * t357 - t393 * t362 / 0.2D1) + 0.18
     #0D3 * t133 * t160 * (-(t401 - t356 * t363) * t385 - t357 + t391 * 
     #t362) + t133 * t170 * t412) * t89 * t178 / 0.1440D4 + (t133 * t170
     # * (t357 - t424 * t362) - 0.90D2 * t133 * t19 * (t429 * t357 / 0.2
     #D1 - t429 * t424 * t362 / 0.6D1) + t133 * t199 * t362 + 0.180D3 * 
     #t133 * t160 * (-t424 * t357 + t429 * t362 / 0.2D1)) * t178 / 0.144
     #0D4 + (-0.90D2 * t133 * t19 * (t357 - t454 * t362 + (t401 - t461 *
     # z * t363) * t385) - 0.180D3 * t133 * t160 * t412) * t89 * t239 / 
     #0.720D3 + (-0.90D2 * t133 * t19 * (-t481 * t357 + t483 * t362 / 0.
     #2D1) + 0.180D3 * t133 * t160 * (t357 - t481 * t362) + t133 * t170 
     #* t362) * t238 * t178 / 0.720D3
      t502 = FJET(XB1, XB2, s, 0.0D0, t337, -t343, 0.0D0, -t347, t501)
      t505 = x2 * t1 * s
      t506 = -0.1D1 + x2
      t508 = t506 * t1 * s
      t509 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t505, -t508,
     # 0.0D0, 0.0D0, 0.0D0)
      t510 = t27 * t506
      t514 = log(-0.4D1 * t214 * t92 * t510)
      t515 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t505, -t508,
     # 0.0D0, 0.0D0, 0.0D0)
      t527 = (-0.90D2 * t133 * t19 * (t509 - t514 * t515) + 0.180D3 * t1
     #33 * t160 * t515) * t89 * t239 / 0.720D3
      t528 = t28 * t506
      t531 = log(-0.4D1 * t478 * t528)
      t533 = t531 ** 2
      t546 = t133 * t170 * t515
      t550 = (-0.90D2 * t133 * t19 * (-t531 * t509 + t533 * t515 / 0.2D1
     #) + 0.180D3 * t133 * t160 * (t509 - t531 * t515) + t546) * t238 * 
     #t178 / 0.720D3
      t553 = log(-0.4D1 * t265 * t528)
      t555 = t553 ** 2
      t570 = (-0.90D2 * t133 * t19 * (-t553 * t509 + t555 * t515 / 0.2D1
     #) + 0.180D3 * t133 * t160 * (t509 - t553 * t515) + t546) * t89 * t
     #238 / 0.1440D4
      t573 = log(-0.4D1 * t302 * t528)
      t575 = -t509 + t573 * t515
      t578 = t573 ** 2
      t584 = -t578 * t509 / 0.2D1 + t578 * t573 * t515 / 0.6D1
      t589 = t133 * t199 * t515
      t593 = t573 * t509 - t578 * t515 / 0.2D1
      t600 = t527 + t550 + t570 - (t133 * t170 * t575 - 0.90D2 * t133 * 
     #t19 * t584 - t589 + 0.180D3 * t133 * t160 * t593) * t238 / 0.1440D
     #4
      t601 = FJET(XB1, XB2, s, 0.0D0, t505, 0.0D0, -t508, 0.0D0, t600)
      t603 = x2 * x3
      t606 = Sqrt(x3 * t506 * t29)
      t607 = t36 * t606
      t609 = 0.2D1 * t607 * x2
      t611 = 0.1D1 - x3 + t603
      t612 = 0.1D1 / t611
      t614 = t2 * (0.1D1 - x3 - x2 + t603 + t213 + t609) * t612
      t619 = t2 * x2 * (-0.1D1 + t603 + 0.2D1 * t607) * t612
      t620 = t506 * t134
      t621 = t620 * t22
      t623 = t611 ** 2
      t624 = 0.1D1 / t623
      t629 = log(0.4D1 * t621 * t28 * t212 * t29 * t624)
      t630 = x2 * z
      t631 = t603 * z
      t632 = t213 * z
      t638 = 0.1D1 / (x2 - t630 - t631 + t632 + x3 - t213 + z - t609 - 0
     #.2D1 * t607 * z + 0.2D1 * t607 * t630)
      t640 = -x2 + t630 - z
      t641 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t619, t614,
     # 0.0D0, 0.0D0, 0.0D0)
      t642 = t640 * t641
      t645 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t619, t614,
     # 0.0D0, 0.0D0, 0.0D0)
      t646 = t638 * t640 * t645
      t653 = t19 * t638 * t642
      t665 = log(0.4D1 * t213 * t92 * t510 * t29 * t624)
      t666 = t665 ** 2
      t670 = t665 * t638
      t688 = (-0.90D2 * t133 * t19 * (-t629 * t638 * t642 + t646) + 0.18
     #0D3 * t133 * lh * t653) * t89 * t239 / 0.720D3 + (-0.90D2 * t133 *
     # t19 * (t666 * t638 * t642 / 0.2D1 - t670 * t640 * t645) + 0.180D3
     # * t133 * t160 * (-t670 * t642 + t646) + t133 * t13 * t653) * t89 
     #* t238 / 0.1440D4
      t689 = FJET(XB1, XB2, s, 0.0D0, t614, 0.0D0, -t619, 0.0D0, t688)
      t692 = t1 * t338
      t694 = t506 * s * t692 * t341
      t696 = x2 * s * t692
      t698 = t344 * t506 * t346
      t699 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t696, t694,
     # t337, 0.0D0, t698)
      t706 = log(-0.4D1 * t621 * t341 * t25 * t27 * t212 * t350)
      t707 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t696, t694,
     # t337, 0.0D0, t698)
      t718 = (-0.90D2 * t133 * t19 * (-t699 + t706 * t707) - 0.180D3 * t
     #133 * t160 * t707) * t89 * t239
      t724 = log(-0.4D1 * t242 * t92 * t420 * t350 * t506)
      t726 = t724 ** 2
      t729 = t724 * t699 - t726 * t707 / 0.2D1
      t734 = -t699 + t724 * t707
      t739 = t133 * t170 * t707
      t744 = t718 / 0.720D3 + (-0.90D2 * t133 * t19 * t729 + 0.180D3 * t
     #133 * t160 * t734 - t739) * t238 * t178 / 0.720D3
      t745 = FJET(XB1, XB2, s, 0.0D0, t694, t337, -t696, t698, t744)
      t747 = FJET(XB1, XB2, s, 0.0D0, -t508, 0.0D0, t505, 0.0D0, t600)
      t749 = FJET(XB1, XB2, s, 0.0D0, -t343, t337, 0.0D0, -t347, t501)
      t751 = FJET(XB1, XB2, s, 0.0D0, -t619, 0.0D0, t614, 0.0D0, t688)
      t753 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t330)
      t755 = FJET(XB1, XB2, s, t337, 0.0D0, 0.0D0, -t343, -t347, t501)
      t757 = t331 * t330 + t333 * t330 + t335 * t330 + t502 * t501 + t60
     #1 * t600 + t689 * t688 + t745 * t744 + t747 * t600 + t749 * t501 +
     # t751 * t688 + t753 * t330 + t755 * t501
      t758 = FJET(XB1, XB2, s, t337, -t696, 0.0D0, t694, t698, t744)
      t774 = t527 + t550 + t570 - (t133 * t170 * t575 - 0.90D2 * t133 * 
     #t19 * t584 - t589 + 0.180D3 * t133 * t160 * t593) * t238 / 0.1440D
     #4
      t775 = FJET(XB1, XB2, s, t505, 0.0D0, -t508, 0.0D0, 0.0D0, t774)
      t777 = FJET(XB1, XB2, s, t614, 0.0D0, -t619, 0.0D0, 0.0D0, t688)
      t779 = FJET(XB1, XB2, s, t694, 0.0D0, -t696, t337, t698, t744)
      t782 = t337 * t603 * t612
      t783 = t2 * t338
      t784 = t213 * x1
      t787 = Sqrt(t372 * t506 * t29)
      t788 = t36 * t787
      t790 = 0.2D1 * t788 * x2
      t791 = t213 * t339
      t795 = t783 * (-t784 + t213 - x2 + t603 + 0.1D1 - x3 + t790 + t791
     #) * t341 * t612
      t799 = t29 * s * t1 * x1 * t612
      t805 = t783 * x2 * (-0.1D1 + t603 + x1 - t367 - t339 + t369 + 0.2D
     #1 * t788) * t341 * t612
      t806 = x2 * x1
      t807 = t806 * z
      t808 = x2 - t806 + z - t630 + t807
      t809 = t808 * t340
      t810 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t805, -t795,
     # -t799, t782, t698)
      t820 = log(0.4D1 * t620 * t22 * t341 * t28 * t212 * t350 * t29 * t
     #624)
      t822 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t805, -t795,
     # -t799, t782, t698)
      t830 = t134 * x2
      t831 = t213 + t630 + 0.2D1 * t788 * t807 + t339 + t368 - t371 - t3
     #78 - z - x3 + 0.2D1 * t806 - t830 - t370 + t380 + t382 - t383 + t6
     #31 - t632
      t841 = t20 * x2
      t853 = -x2 + t791 - 0.2D1 * t788 * t806 - 0.2D1 * t788 * t630 + 0.
     #2D1 * t603 * t339 - t379 * t806 - 0.2D1 * t371 * t630 + t371 * t84
     #1 - t784 + t790 - 0.3D1 * t807 + t841 * x1 + 0.2D1 * t830 * z - t1
     #34 * t20 * x2 - t603 * x1 + t371 * x2 + 0.2D1 * t788 * z
      t855 = 0.1D1 / (t831 + t853)
      t864 = 0.90D2 * t133 * t19 * (t809 * t810 - t820 * t808 * t340 * t
     #822) * t855 - 0.180D3 * t133 * t160 * t809 * t822 * t855
      t867 = t864 * t89 * t239 / 0.720D3
      t868 = FJET(XB1, XB2, s, t782, -t795, -t799, t805, t698, t867)
      t871 = t89 * t238 * t178
      t874 = FJET(XB1, XB2, s, t805, -t799, -t795, t782, t698, t867)
      t878 = FJET(XB1, XB2, s, -t508, 0.0D0, t505, 0.0D0, 0.0D0, t774)
      t880 = FJET(XB1, XB2, s, -t343, 0.0D0, 0.0D0, t337, -t347, t501)
      t894 = t718 / 0.720D3 + (-0.90D2 * t133 * t19 * t729 + 0.180D3 * t
     #133 * t160 * t734 - t739) * t238 * t178 / 0.720D3
      t895 = FJET(XB1, XB2, s, -t696, t337, t694, 0.0D0, t698, t894)
      t897 = FJET(XB1, XB2, s, -t619, 0.0D0, t614, 0.0D0, 0.0D0, t688)
      t899 = FJET(XB1, XB2, s, -t799, t805, t782, -t795, t698, t867)
      t903 = FJET(XB1, XB2, s, -t795, t782, t805, -t799, t698, t867)
      t907 = t758 * t744 + t775 * t774 + t777 * t688 + t779 * t744 + t86
     #8 * t864 * t871 / 0.720D3 + t874 * t864 * t871 / 0.720D3 + t878 * 
     #t774 + t880 * t501 + t895 * t894 + t897 * t688 + t899 * t864 * t87
     #1 / 0.720D3 + t903 * t864 * t871 / 0.720D3
      rrqqbar2gght5s1e1 = t757 + t907

      end function



      doubleprecision function rrqqbar2gght5s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t5 * t7
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t23 = log(-0.4D1 * t11 * t20)
      t24 = t23 ** 2
      t26 = cos(t12)
      t28 = Sqrt(-x3 * t18)
      t33 = 0.1D1 / (-x3 - z + 0.2D1 * t26 * t28 * z)
      t37 = log(0.4D1 * t11 * t17)
      t38 = t37 ** 2
      t46 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t47 = t46 * pi
      t58 = pi ** 2
      t60 = lh ** 2
      t62 = 0.30D2 * t58 - 0.180D3 * t60
      t71 = 0.1D1 / x3
      t74 = t10 * t14
      t75 = t74 * t16
      t77 = log(0.4D1 * t75)
      t78 = t77 * pi
      t81 = t77 ** 2
      t82 = t81 * pi
      t108 = pi * t5
      t109 = z * t46
      t110 = x2 ** 2
      t111 = x3 * t110
      t112 = t111 * t10
      t115 = log(-0.4D1 * t112 * t20)
      t122 = log(0.4D1 * t111 * t75)
      t128 = lh * t7
      t131 = -z * t3 * t33 - t3
      t137 = 0.1D1 / x2
      t140 = t110 * t10
      t143 = log(0.4D1 * t140 * t17)
      t145 = t143 ** 2
      t157 = t62 * t7
      t159 = t108 * t157 * t3
      t163 = x1 ** 2
      t164 = t11 * t163
      t167 = log(-0.4D1 * t164 * t20)
      t176 = log(0.4D1 * t11 * t163 * t14 * t16)
      t188 = 0.1D1 / x1
      t191 = t108 * t7
      t193 = t137 * t188
      t197 = t110 * t163
      t200 = log(0.4D1 * t197 * t75)
      t213 = t10 * t163
      t216 = log(0.4D1 * t213 * t17)
      t218 = t216 ** 2
      t233 = (-0.90D2 * t4 * t8 * (-t24 * z * t33 / 0.2D1 - t38 / 0.2D1)
     # + (0.180D3 * t4 * lh - 0.90D2 * t47) * t5 * t7 * (t23 * z * t33 +
     # t37) + (0.180D3 * t47 * lh + t4 * t62) * t5 * t7 * (-z * t33 - 0.
     #1D1)) * t71 / 0.2880D4 - (-0.180D3 * t78 * lh - 0.45D2 * t82 + pi 
     #* t62) * t5 * t7 * t46 / 0.2880D4 - (0.90D2 * t82 * lh + pi * (0.2
     #40D3 * zeta3 + 0.120D3 * t60 * lh - 0.60D2 * lh * t58) + 0.15D2 * 
     #t81 * t77 * pi - t78 * t62) * t5 * t7 * t3 / 0.2880D4 + (-0.90D2 *
     # t108 * t7 * (-(t109 - t115 * z * t3) * t33 - t46 + t122 * t3) + 0
     #.180D3 * t108 * t128 * t131) * t71 * t137 / 0.1440D4 - (-0.90D2 * 
     #t108 * t7 * (-t143 * t46 + t145 * t3 / 0.2D1) + 0.180D3 * t108 * t
     #128 * (t46 - t143 * t3) + t159) * t137 / 0.1440D4 - (-0.90D2 * t10
     #8 * t7 * ((t109 - t167 * z * t3) * t33 + t46 - t176 * t3) - 0.180D
     #3 * t108 * t128 * t131) * t71 * t188 / 0.1440D4 - t191 * t131 * t7
     #1 * t193 / 0.8D1 + (-0.90D2 * t108 * t7 * (-t46 + t200 * t3) - 0.1
     #80D3 * t108 * t128 * t3) * t137 * t188 / 0.720D3 + (-0.90D2 * t108
     # * t7 * (t216 * t46 - t218 * t3 / 0.2D1) + 0.180D3 * t108 * t128 *
     # (-t46 + t216 * t3) - t159) * t188 / 0.1440D4
      t234 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t233)
      t236 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t233)
      t238 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t233)
      t240 = t2 * x1
      t241 = -0.1D1 + x1
      t242 = x1 * z
      t243 = 0.1D1 - x1 + t242
      t244 = 0.1D1 / t243
      t246 = t2 * t241 * t244
      t247 = s * t15
      t249 = t241 * x1 * t244
      t250 = t247 * t249
      t251 = z * t243
      t252 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t246
     #, t240, 0.0D0, -t250)
      t256 = t241 ** 2
      t261 = log(-0.4D1 * t213 * x3 * t244 * t17 * t256 * t19)
      t263 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t246
     #, t240, 0.0D0, -t250)
      t267 = x3 * x1
      t268 = 0.2D1 * t267
      t269 = t267 * z
      t270 = 0.3D1 * t269
      t271 = x3 * t163
      t272 = x3 * t243
      t274 = Sqrt(-t272 * t18)
      t278 = x1 * t9
      t279 = x3 * t9
      t280 = t279 * x1
      t282 = 0.2D1 * t271 * z
      t283 = t271 * t9
      t284 = -z + t268 + t242 - t270 - t271 - x3 + 0.2D1 * t26 * t274 * 
     #z - t278 + t280 + t282 - t283
      t285 = 0.1D1 / t284
      t288 = t17 * t244 * t256
      t291 = log(0.4D1 * t164 * t288)
      t299 = -t251 * t263 * t285 - t263
      t312 = t197 * t10
      t315 = log(0.4D1 * t312 * t288)
      t329 = t16 * t244
      t333 = log(0.4D1 * t213 * t14 * t329 * t256)
      t335 = t333 ** 2
      t352 = -(-0.90D2 * t108 * t7 * (-(t251 * t252 - t261 * z * t243 * 
     #t263) * t285 - t252 + t291 * t263) + 0.180D3 * t108 * t128 * t299)
     # * t71 * t188 / 0.1440D4 + t191 * t299 * t71 * t193 / 0.8D1 + (-0.
     #90D2 * t108 * t7 * (t252 - t315 * t263) + 0.180D3 * t108 * t128 * 
     #t263) * t137 * t188 / 0.720D3 + (-0.90D2 * t108 * t7 * (-t333 * t2
     #52 + t335 * t263 / 0.2D1) + 0.180D3 * t108 * t128 * (t252 - t333 *
     # t263) + t108 * t157 * t263) * t188 / 0.1440D4
      t353 = FJET(XB1, XB2, s, 0.0D0, t240, -t246, 0.0D0, -t250, t352)
      t356 = x2 * t1 * s
      t357 = -0.1D1 + x2
      t359 = t357 * t1 * s
      t360 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t356, -t359,
     # 0.0D0, 0.0D0, 0.0D0)
      t361 = t17 * t357
      t364 = log(-0.4D1 * t112 * t361)
      t365 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t356, -t359,
     # 0.0D0, 0.0D0, 0.0D0)
      t373 = 0.180D3 * t108 * t128 * t365
      t377 = (-0.90D2 * t108 * t7 * (t360 - t364 * t365) + t373) * t71 *
     # t137 / 0.1440D4
      t380 = log(-0.4D1 * t140 * t361)
      t382 = t380 ** 2
      t385 = t380 * t360 - t382 * t365 / 0.2D1
      t390 = -t360 + t380 * t365
      t395 = t108 * t157 * t365
      t402 = t191 * t365 * t71 * t193 / 0.8D1
      t405 = log(-0.4D1 * t312 * t361)
      t414 = (-0.90D2 * t108 * t7 * (t360 - t405 * t365) + t373) * t137 
     #* t188 / 0.720D3
      t415 = t377 - (-0.90D2 * t108 * t7 * t385 + 0.180D3 * t108 * t128 
     #* t390 - t395) * t137 / 0.1440D4 - t402 + t414
      t416 = FJET(XB1, XB2, s, 0.0D0, t356, 0.0D0, -t359, 0.0D0, t415)
      t418 = x2 * x3
      t421 = Sqrt(x3 * t357 * t18)
      t422 = t26 * t421
      t424 = 0.2D1 * t422 * x2
      t426 = 0.1D1 - x3 + t418
      t427 = 0.1D1 / t426
      t429 = t2 * (0.1D1 - x3 - x2 + t418 + t111 + t424) * t427
      t434 = t2 * x2 * (-0.1D1 + t418 + 0.2D1 * t422) * t427
      t437 = t426 ** 2
      t443 = log(0.4D1 * t111 * t74 * t16 * t357 * t18 / t437)
      t444 = x2 * z
      t445 = t418 * z
      t446 = t111 * z
      t452 = 0.1D1 / (x2 - t444 - t445 + t446 + x3 - t111 + z - t424 - 0
     #.2D1 * t422 * z + 0.2D1 * t422 * t444)
      t454 = -x2 + t444 - z
      t455 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t434, t429,
     # 0.0D0, 0.0D0, 0.0D0)
      t456 = t454 * t455
      t459 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t434, t429,
     # 0.0D0, 0.0D0, 0.0D0)
      t466 = t7 * t452
      t476 = t71 * t137 * t188
      t480 = (-0.90D2 * t108 * t7 * (-t443 * t452 * t456 + t452 * t454 *
     # t459) + 0.180D3 * t108 * lh * t466 * t456) * t71 * t137 / 0.1440D
     #4 - t108 * t466 * t456 * t476 / 0.8D1
      t481 = FJET(XB1, XB2, s, 0.0D0, t429, 0.0D0, -t434, 0.0D0, t480)
      t484 = t1 * t241
      t486 = t357 * s * t484 * t244
      t488 = x2 * s * t484
      t490 = t247 * t357 * t249
      t491 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t488, t486,
     # t240, 0.0D0, t490)
      t495 = t191 * t491 * t71 * t193 / 0.8D1
      t496 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t488, t486,
     # t240, 0.0D0, t490)
      t502 = log(-0.4D1 * t197 * t74 * t329 * t256 * t357)
      t504 = -t496 + t502 * t491
      t510 = 0.180D3 * t108 * t128 * t491
      t515 = t495 + (-0.90D2 * t108 * t7 * t504 - t510) * t137 * t188 / 
     #0.720D3
      t516 = FJET(XB1, XB2, s, 0.0D0, t486, t240, -t488, t490, t515)
      t518 = FJET(XB1, XB2, s, 0.0D0, -t359, 0.0D0, t356, 0.0D0, t415)
      t520 = FJET(XB1, XB2, s, 0.0D0, -t246, t240, 0.0D0, -t250, t352)
      t522 = FJET(XB1, XB2, s, 0.0D0, -t434, 0.0D0, t429, 0.0D0, t480)
      t524 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t233)
      t526 = FJET(XB1, XB2, s, t240, 0.0D0, 0.0D0, -t246, -t250, t352)
      t528 = t234 * t233 + t236 * t233 + t238 * t233 + t353 * t352 + t41
     #6 * t415 + t481 * t480 + t516 * t515 + t518 * t415 + t520 * t352 +
     # t522 * t480 + t524 * t233 + t526 * t352
      t529 = FJET(XB1, XB2, s, t240, -t488, 0.0D0, t486, t490, t515)
      t542 = t377 - (-0.90D2 * t108 * t7 * t385 + 0.180D3 * t108 * t128 
     #* t390 - t395) * t137 / 0.1440D4 - t402 + t414
      t543 = FJET(XB1, XB2, s, t356, 0.0D0, -t359, 0.0D0, 0.0D0, t542)
      t545 = FJET(XB1, XB2, s, t429, 0.0D0, -t434, 0.0D0, 0.0D0, t480)
      t547 = FJET(XB1, XB2, s, t486, 0.0D0, -t488, t240, t490, t515)
      t550 = t240 * t418 * t427
      t551 = t2 * t241
      t552 = t111 * x1
      t555 = Sqrt(t272 * t357 * t18)
      t556 = t26 * t555
      t558 = 0.2D1 * t556 * x2
      t559 = t111 * t242
      t563 = t551 * (-t552 + t111 - x2 + t418 + 0.1D1 - x3 + t558 + t559
     #) * t244 * t427
      t567 = t18 * s * t1 * x1 * t427
      t573 = t551 * x2 * (-0.1D1 + t418 + x1 - t267 - t242 + t269 + 0.2D
     #1 * t556) * t244 * t427
      t574 = x2 * x1
      t575 = t574 * z
      t576 = x2 - t574 + z - t444 + t575
      t580 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t573, -t563,
     # -t567, t550, t490)
      t584 = t163 * x2
      t585 = t445 - t446 + t242 - x3 - t270 + t280 + t282 - t283 + 0.2D1
     # * t556 * t575 + t268 - t271 - t278 - z + t111 + t444 + 0.2D1 * t5
     #74 - t584
      t595 = t9 * x2
      t607 = t559 - 0.2D1 * t556 * t574 - 0.2D1 * t556 * t444 + 0.2D1 * 
     #t418 * t242 - t279 * t574 - 0.2D1 * t271 * t444 + t271 * t595 - x2
     # - t552 + t558 - 0.3D1 * t575 + t595 * x1 + 0.2D1 * t584 * z - t16
     #3 * t9 * x2 - t418 * x1 + t271 * x2 + 0.2D1 * t556 * z
      t609 = 0.1D1 / (t585 + t607)
      t613 = t108 * t7 * t576 * t243 * t580 * t609 * t476 / 0.8D1
      t614 = FJET(XB1, XB2, s, t550, -t563, -t567, t573, t490, t613)
      t616 = t8 * t576
      t620 = t243 * t580 * t609 * t476
      t623 = FJET(XB1, XB2, s, t573, -t567, -t563, t550, t490, t613)
      t628 = FJET(XB1, XB2, s, -t359, 0.0D0, t356, 0.0D0, 0.0D0, t542)
      t630 = FJET(XB1, XB2, s, -t246, 0.0D0, 0.0D0, t240, -t250, t352)
      t640 = t495 + (-0.90D2 * t108 * t7 * t504 - t510) * t137 * t188 / 
     #0.720D3
      t641 = FJET(XB1, XB2, s, -t488, t240, t486, 0.0D0, t490, t640)
      t643 = FJET(XB1, XB2, s, -t434, 0.0D0, t429, 0.0D0, 0.0D0, t480)
      t645 = FJET(XB1, XB2, s, -t567, t573, t550, -t563, t490, t613)
      t650 = FJET(XB1, XB2, s, -t563, t550, t573, -t567, t490, t613)
      t655 = t529 * t515 + t543 * t542 + t545 * t480 + t547 * t515 + t61
     #4 * pi * t616 * t620 / 0.8D1 + t623 * pi * t616 * t620 / 0.8D1 + t
     #628 * t542 + t630 * t352 + t640 * t641 + t643 * t480 + t645 * pi *
     # t616 * t620 / 0.8D1 + t650 * pi * t616 * t620 / 0.8D1
      rrqqbar2gght5s1e0 = t528 + t655

      end function



      doubleprecision function rrqqbar2gght5s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t5 * t7
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t23 = log(-0.4D1 * t11 * t17 / t18)
      t25 = cos(t12)
      t27 = Sqrt(-x3 * t18)
      t32 = 0.1D1 / (-x3 - z + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t11 * t17)
      t43 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t53 = 0.1D1 / x3
      t61 = log(0.4D1 * t10 * t14 * t16)
      t62 = t61 * pi
      t71 = t61 ** 2
      t74 = pi ** 2
      t76 = lh ** 2
      t85 = pi * t5
      t86 = t85 * t7
      t89 = -z * t3 * t32 - t3
      t91 = 0.1D1 / x2
      t95 = x2 ** 2
      t96 = t95 * t10
      t99 = log(0.4D1 * t96 * t17)
      t105 = lh * t7
      t108 = 0.180D3 * t85 * t105 * t3
      t114 = 0.1D1 / x1
      t118 = x1 ** 2
      t119 = t10 * t118
      t122 = log(0.4D1 * t119 * t17)
      t136 = (-0.90D2 * t4 * t8 * (t23 * z * t32 + t36) + (0.180D3 * t4 
     #* lh - 0.90D2 * t43 * pi) * t5 * t7 * (-z * t32 - 0.1D1)) * t53 / 
     #0.2880D4 - (0.180D3 * pi * lh + 0.90D2 * t62) * t5 * t7 * t43 / 0.
     #2880D4 - (-0.180D3 * t62 * lh - 0.45D2 * t71 * pi + pi * (0.30D2 *
     # t74 - 0.180D3 * t76)) * t5 * t7 * t3 / 0.2880D4 - t86 * t89 * t53
     # * t91 / 0.16D2 - (-0.90D2 * t85 * t7 * (t43 - t99 * t3) + t108) *
     # t91 / 0.1440D4 + t4 * t5 * t7 * t91 * t114 / 0.8D1 + (-0.90D2 * t
     #85 * t7 * (-t43 + t122 * t3) - t108) * t114 / 0.1440D4 - t86 * t89
     # * t53 * t114 / 0.16D2
      t137 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t136)
      t139 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t136)
      t141 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t136)
      t143 = t2 * x1
      t144 = -0.1D1 + x1
      t145 = x1 * z
      t146 = 0.1D1 - x1 + t145
      t147 = 0.1D1 / t146
      t149 = t2 * t144 * t147
      t150 = s * t15
      t152 = t144 * x1 * t147
      t153 = t150 * t152
      t154 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t149
     #, t143, 0.0D0, -t153)
      t159 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t149
     #, t143, 0.0D0, -t153)
      t162 = t144 ** 2
      t166 = log(0.4D1 * t119 * t14 * t16 * t147 * t162)
      t179 = x3 * x1
      t183 = x3 * t118
      t186 = Sqrt(-x3 * t146 * t18)
      t196 = -z + 0.2D1 * t179 + t145 - 0.3D1 * t179 * z - t183 - x3 + 0
     #.2D1 * t25 * t186 * z - x1 * t9 + x3 * t9 * x1 + 0.2D1 * t183 * z 
     #- t183 * t9
      t205 = -t86 * t154 * t91 * t114 / 0.8D1 + (-0.90D2 * t85 * t7 * (t
     #159 - t166 * t154) + 0.180D3 * t85 * t105 * t154) * t114 / 0.1440D
     #4 + t86 * (-z * t146 * t154 / t196 - t154) * t53 * t114 / 0.16D2
      t206 = FJET(XB1, XB2, s, 0.0D0, t143, -t149, 0.0D0, -t153, t205)
      t209 = x2 * t1 * s
      t210 = -0.1D1 + x2
      t212 = t210 * t1 * s
      t213 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t209, -t212,
     # 0.0D0, 0.0D0, 0.0D0)
      t217 = t86 * t213 * t53 * t91 / 0.16D2
      t218 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t209, -t212,
     # 0.0D0, 0.0D0, 0.0D0)
      t222 = log(-0.4D1 * t96 * t17 * t210)
      t224 = -t218 + t222 * t213
      t230 = 0.180D3 * t85 * t105 * t213
      t237 = t86 * t213 * t91 * t114 / 0.8D1
      t238 = -t217 - (-0.90D2 * t85 * t7 * t224 - t230) * t91 / 0.1440D4
     # - t237
      t239 = FJET(XB1, XB2, s, 0.0D0, t209, 0.0D0, -t212, 0.0D0, t238)
      t241 = x2 * x3
      t242 = t95 * x3
      t245 = Sqrt(x3 * t210 * t18)
      t246 = t25 * t245
      t248 = 0.2D1 * t246 * x2
      t251 = 0.1D1 / (0.1D1 - x3 + t241)
      t253 = t2 * (0.1D1 - x3 - x2 + t241 + t242 + t248) * t251
      t258 = t2 * x2 * (-0.1D1 + t241 + 0.2D1 * t246) * t251
      t259 = x2 * z
      t267 = 0.1D1 / (x2 - t259 - t241 * z + t242 * z + x3 - t242 + z - 
     #t248 - 0.2D1 * t246 * z + 0.2D1 * t246 * t259)
      t270 = -x2 + t259 - z
      t271 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t258, t253,
     # 0.0D0, 0.0D0, 0.0D0)
      t276 = t85 * t7 * t267 * t270 * t271 * t53 * t91 / 0.16D2
      t277 = FJET(XB1, XB2, s, 0.0D0, t253, 0.0D0, -t258, 0.0D0, -t276)
      t283 = t267 * t270 * t271 * t53 * t91
      t287 = t1 * t144
      t289 = t210 * s * t287 * t147
      t291 = x2 * s * t287
      t293 = t150 * t210 * t152
      t294 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t291, t289,
     # t143, 0.0D0, t293)
      t298 = t86 * t294 * t91 * t114 / 0.8D1
      t299 = FJET(XB1, XB2, s, 0.0D0, t289, t143, -t291, t293, t298)
      t304 = t7 * t294 * t91 * t114
      t307 = FJET(XB1, XB2, s, 0.0D0, -t212, 0.0D0, t209, 0.0D0, t238)
      t309 = FJET(XB1, XB2, s, 0.0D0, -t149, t143, 0.0D0, -t153, t205)
      t311 = FJET(XB1, XB2, s, 0.0D0, -t258, 0.0D0, t253, 0.0D0, -t276)
      t316 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t136)
      t318 = FJET(XB1, XB2, s, t143, 0.0D0, 0.0D0, -t149, -t153, t205)
      t320 = FJET(XB1, XB2, s, t143, -t291, 0.0D0, t289, t293, t298)
      t332 = -t217 - (-0.90D2 * t85 * t7 * t224 - t230) * t91 / 0.1440D4
     # - t237
      t333 = FJET(XB1, XB2, s, t209, 0.0D0, -t212, 0.0D0, 0.0D0, t332)
      t335 = FJET(XB1, XB2, s, t253, 0.0D0, -t258, 0.0D0, 0.0D0, -t276)
      t340 = FJET(XB1, XB2, s, t289, 0.0D0, -t291, t143, t293, t298)
      t345 = FJET(XB1, XB2, s, -t212, 0.0D0, t209, 0.0D0, 0.0D0, t332)
      t347 = FJET(XB1, XB2, s, -t149, 0.0D0, 0.0D0, t143, -t153, t205)
      t349 = FJET(XB1, XB2, s, -t291, t143, t289, 0.0D0, t293, t298)
      t354 = FJET(XB1, XB2, s, -t258, 0.0D0, t253, 0.0D0, 0.0D0, -t276)
      rrqqbar2gght5s1em1 = t137 * t136 + t139 * t136 + t141 * t136 + t20
     #6 * t205 + t239 * t238 - t277 * pi * t8 * t283 / 0.16D2 + t299 * p
     #i * t5 * t304 / 0.8D1 + t307 * t238 + t309 * t205 - t311 * pi * t8
     # * t283 / 0.16D2 + t316 * t136 + t318 * t205 + t320 * pi * t5 * t3
     #04 / 0.8D1 + t333 * t332 - t335 * pi * t8 * t283 / 0.16D2 + t340 *
     # pi * t5 * t304 / 0.8D1 + t345 * t332 + t347 * t205 + t349 * pi * 
     #t5 * t304 / 0.8D1 - t354 * pi * t8 * t283 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t4 = t3 * pi
      t5 = 0.1D1 / t1
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = x4 * pi
      t10 = cos(t9)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t26 = t5 * t8
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t35 = pi * t5
      t36 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t42 = z ** 2
      t44 = Sin(t9)
      t45 = t44 ** 2
      t47 = t1 ** 2
      t48 = t47 ** 2
      t51 = log(0.4D1 / t42 * t45 * t48)
      t59 = -t4 * t5 * t8 * (-z / (-x3 - z + 0.2D1 * t10 * t13 * z) - 0.
     #1D1) / x3 / 0.32D2 + t4 * t26 * t27 / 0.16D2 + t4 * t26 * t31 / 0.
     #16D2 + t35 * t8 * t36 / 0.32D2 - (0.180D3 * pi * lh + 0.90D2 * t51
     # * pi) * t5 * t8 * t3 / 0.2880D4
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t59)
      t62 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t59)
      t64 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t59)
      t66 = t2 * x1
      t67 = -0.1D1 + x1
      t70 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t72 = t2 * t67 * t70
      t76 = s * t47 * t67 * x1 * t70
      t77 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t72, 
     #t66, 0.0D0, -t76)
      t79 = t8 * t77 * t31
      t81 = t35 * t79 / 0.16D2
      t82 = FJET(XB1, XB2, s, 0.0D0, t66, -t72, 0.0D0, -t76, -t81)
      t88 = x2 * t1 * s
      t91 = (-0.1D1 + x2) * t1 * s
      t92 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t88, -t91, 0.
     #0D0, 0.0D0, 0.0D0)
      t94 = t8 * t92 * t27
      t96 = t35 * t94 / 0.16D2
      t97 = FJET(XB1, XB2, s, 0.0D0, t88, 0.0D0, -t91, 0.0D0, -t96)
      t102 = FJET(XB1, XB2, s, 0.0D0, -t91, 0.0D0, t88, 0.0D0, -t96)
      t107 = FJET(XB1, XB2, s, 0.0D0, -t72, t66, 0.0D0, -t76, -t81)
      t112 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t59)
      t114 = FJET(XB1, XB2, s, t66, 0.0D0, 0.0D0, -t72, -t76, -t81)
      t119 = FJET(XB1, XB2, s, t88, 0.0D0, -t91, 0.0D0, 0.0D0, -t96)
      t124 = FJET(XB1, XB2, s, -t91, 0.0D0, t88, 0.0D0, 0.0D0, -t96)
      t129 = FJET(XB1, XB2, s, -t72, 0.0D0, 0.0D0, t66, -t76, -t81)
      rrqqbar2gght5s1em2 = t60 * t59 + t62 * t59 + t64 * t59 - t82 * pi 
     #* t5 * t79 / 0.16D2 - t97 * pi * t5 * t94 / 0.16D2 - t102 * pi * t
     #5 * t94 / 0.16D2 - t107 * pi * t5 * t79 / 0.16D2 + t112 * t59 - t1
     #14 * pi * t5 * t79 / 0.16D2 - t119 * pi * t5 * t94 / 0.16D2 - t124
     # * pi * t5 * t94 / 0.16D2 - t129 * pi * t5 * t79 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t10 = t3 * pi * t5 * t7 / 0.32D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = pi * t5 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s1em3 = t11 * t3 * t14 / 0.32D2 + t16 * t3 * t14 / 0.
     #32D2 + t19 * t3 * t14 / 0.32D2 + t22 * t3 * t14 / 0.32D2

      end function



      doubleprecision function rrqqbar2gght5s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght5s1em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght5s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = -0.1D1 + x3
      t4 = t2 * t3
      t5 = t2 * x3
      t6 = 0.1D1 / t1
      t7 = pi * t6
      t8 = pi ** 2
      t10 = lh ** 2
      t12 = 0.30D2 * t8 - 0.180D3 * t10
      t13 = s ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = x3 * t18
      t20 = x4 * pi
      t21 = Sin(t20)
      t22 = t21 ** 2
      t23 = t1 ** 2
      t24 = t23 ** 2
      t25 = t22 * t24
      t26 = t25 * t3
      t29 = log(-0.4D1 * t19 * t26)
      t30 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t32 = t16 - t29 * t30
      t35 = t29 ** 2
      t41 = t35 * t16 / 0.2D1 - t35 * t29 * t30 / 0.6D1
      t50 = 0.240D3 * zeta3 + 0.120D3 * t10 * lh - 0.60D2 * lh * t8
      t51 = t50 * t14
      t53 = t7 * t51 * t30
      t54 = lh * t14
      t58 = -t29 * t16 + t35 * t30 / 0.2D1
      t63 = 0.1D1 / x3
      t66 = x1 ** 2
      t67 = x3 * t66
      t68 = t67 * t18
      t71 = log(-0.4D1 * t68 * t26)
      t73 = t71 ** 2
      t86 = t7 * t15 * t30
      t89 = 0.1D1 / x1
      t91 = (-0.90D2 * t7 * t14 * (-t71 * t16 + t73 * t30 / 0.2D1) + 0.1
     #80D3 * t7 * t54 * (t16 - t71 * t30) + t86) * t63 * t89 / 0.720D3
      t92 = t67 * x2
      t93 = t18 * t22
      t98 = log(-0.4D1 * t92 * t93 * t24 * t3)
      t109 = 0.1D1 / x2
      t110 = t109 * t89
      t112 = (-0.90D2 * t7 * t14 * (t16 - t98 * t30) + 0.180D3 * t7 * t5
     #4 * t30) * t63 * t110 / 0.720D3
      t113 = x2 * x3
      t114 = t113 * t18
      t117 = log(-0.4D1 * t114 * t26)
      t119 = t117 ** 2
      t134 = (-0.90D2 * t7 * t14 * (-t117 * t16 + t119 * t30 / 0.2D1) + 
     #0.180D3 * t7 * t54 * (t16 - t117 * t30) + t86) * t63 * t109 / 0.14
     #40D4
      t135 = -(-t7 * t15 * t32 + 0.90D2 * t7 * t14 * t41 - t53 - 0.180D3
     # * t7 * t54 * t58) * t63 / 0.1440D4 + t91 + t112 + t134
      t136 = FJET(XB1, XB2, s, -t4, 0.0D0, t5, 0.0D0, 0.0D0, t135)
      t139 = x2 * t1 * s
      t140 = -0.1D1 + x2
      t142 = t140 * t1 * s
      t143 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t142, t139,
     # 0.0D0, 0.0D0, 0.0D0)
      t144 = t140 ** 2
      t145 = t24 * t144
      t149 = log(0.4D1 * t92 * t93 * t145)
      t150 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t142, t139,
     # 0.0D0, 0.0D0, 0.0D0)
      t162 = (-0.90D2 * t7 * t14 * (t143 - t149 * t150) + 0.180D3 * t7 *
     # t54 * t150) * t63 * t110 / 0.720D3
      t163 = x2 * t66
      t164 = t163 * t18
      t165 = t25 * t144
      t168 = log(0.4D1 * t164 * t165)
      t170 = t168 ** 2
      t183 = t7 * t15 * t150
      t187 = (-0.90D2 * t7 * t14 * (-t168 * t143 + t170 * t150 / 0.2D1) 
     #+ 0.180D3 * t7 * t54 * (t143 - t168 * t150) + t183) * t109 * t89 /
     # 0.720D3
      t190 = log(0.4D1 * t114 * t165)
      t192 = t190 ** 2
      t207 = (-0.90D2 * t7 * t14 * (-t190 * t143 + t192 * t150 / 0.2D1) 
     #+ 0.180D3 * t7 * t54 * (t143 - t190 * t150) + t183) * t63 * t109 /
     # 0.1440D4
      t208 = x2 * t18
      t211 = log(0.4D1 * t208 * t165)
      t213 = t143 - t211 * t150
      t216 = t211 ** 2
      t222 = t216 * t143 / 0.2D1 - t216 * t211 * t150 / 0.6D1
      t227 = t7 * t51 * t150
      t231 = -t211 * t143 + t216 * t150 / 0.2D1
      t238 = t162 + t187 + t207 - (-t7 * t15 * t213 + 0.90D2 * t7 * t14 
     #* t222 - t227 - 0.180D3 * t7 * t54 * t231) * t109 / 0.1440D4
      t239 = FJET(XB1, XB2, s, t139, 0.0D0, -t142, 0.0D0, 0.0D0, t238)
      t241 = t3 * s
      t242 = -0.1D1 + x1
      t243 = t1 * t242
      t244 = t241 * t243
      t245 = t1 * x1
      t246 = t241 * t245
      t248 = x3 * s * t243
      t249 = x3 * x1
      t250 = t2 * t249
      t252 = x1 * z
      t253 = 0.1D1 - x1 + t252
      t254 = 0.1D1 / t253
      t255 = t24 * t254
      t256 = t242 ** 2
      t257 = t256 * t3
      t261 = log(-0.4D1 * t67 * t93 * t255 * t257)
      t262 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t244, -t248,
     # -t246, t250, 0.0D0)
      t264 = t261 ** 2
      t265 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t244, -t248,
     # -t246, t250, 0.0D0)
      t282 = t66 * t18
      t283 = x3 * t254
      t284 = t282 * t283
      t285 = x2 * t22
      t291 = log(-0.4D1 * t284 * t285 * t24 * t256 * t3)
      t304 = (-0.90D2 * t7 * t14 * (t261 * t262 - t264 * t265 / 0.2D1) +
     # 0.180D3 * t7 * t54 * (-t262 + t261 * t265) - t7 * t15 * t265) * t
     #63 * t89 / 0.720D3 + (-0.90D2 * t7 * t14 * (-t262 + t291 * t265) -
     # 0.180D3 * t7 * t54 * t265) * t63 * t110 / 0.720D3
      t305 = FJET(XB1, XB2, s, t244, -t246, -t248, t250, 0.0D0, t304)
      t321 = t162 + t187 + t207 - (-t7 * t15 * t213 + 0.90D2 * t7 * t14 
     #* t222 - t227 - 0.180D3 * t7 * t54 * t231) * t109 / 0.1440D4
      t322 = FJET(XB1, XB2, s, 0.0D0, t139, 0.0D0, -t142, 0.0D0, t321)
      t324 = 0.3D1 * t113
      t325 = x2 ** 2
      t326 = t325 * x3
      t327 = cos(t20)
      t329 = Sqrt(-t113 * t3)
      t330 = t327 * t329
      t331 = 0.2D1 * t330
      t333 = 0.2D1 * t330 * x2
      t335 = -0.1D1 + t113
      t336 = 0.1D1 / t335
      t338 = t2 * (-x2 - x3 + t324 - t326 - t331 + t333) * t336
      t342 = t2 * t140 * (-t113 - 0.1D1 + x3 + t331) * t336
      t344 = t335 ** 2
      t345 = 0.1D1 / t344
      t350 = log(-0.4D1 * t113 * t93 * t145 * t3 * t345)
      t351 = x2 * z
      t352 = 0.1D1 - x2 + t351
      t353 = t350 * t352
      t354 = t326 * z
      t355 = 0.2D1 * t113
      t356 = t113 * z
      t360 = 0.1D1 / (-t354 + t326 + x2 - t355 - t351 + t356 - t333 + 0.
     #2D1 * t330 * t351 + t331 - 0.1D1)
      t361 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t342, t338,
     # 0.0D0, 0.0D0, 0.0D0)
      t364 = t350 ** 2
      t366 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t342, t338,
     # 0.0D0, 0.0D0, 0.0D0)
      t367 = t360 * t366
      t375 = t352 * t360 * t361
      t383 = t14 * t352 * t367
      t389 = t113 * t282
      t395 = log(-0.4D1 * t389 * t25 * t144 * t3 * t345)
      t409 = (-0.90D2 * t7 * t14 * (-t353 * t360 * t361 + t364 * t352 * 
     #t367 / 0.2D1) + 0.180D3 * t7 * t54 * (t375 - t353 * t367) + t7 * t
     #12 * t383) * t63 * t109 / 0.1440D4 + (-0.90D2 * t7 * t14 * (t375 -
     # t395 * t352 * t367) + 0.180D3 * t7 * lh * t383) * t63 * t110 / 0.
     #720D3
      t410 = FJET(XB1, XB2, s, 0.0D0, t338, 0.0D0, -t342, 0.0D0, t409)
      t414 = t2 * t242 * x2 * t254
      t416 = t140 * s * t243
      t417 = t2 * x1
      t422 = s * t23 * x2 * t242 * x1 * t254
      t427 = log(0.4D1 * t284 * t285 * t145 * t256)
      t428 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t416, -t414,
     # t417, 0.0D0, -t422)
      t430 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t416, -t414,
     # t417, 0.0D0, -t422)
      t446 = log(0.4D1 * t163 * t93 * t255 * t256 * t144)
      t447 = t446 ** 2
      t466 = (-0.90D2 * t7 * t14 * (t427 * t428 - t430) - 0.180D3 * t7 *
     # t54 * t428) * t63 * t110 / 0.720D3 + (-0.90D2 * t7 * t14 * (-t447
     # * t428 / 0.2D1 + t446 * t430) + 0.180D3 * t7 * t54 * (t446 * t428
     # - t430) - t7 * t15 * t428) * t109 * t89 / 0.720D3
      t467 = FJET(XB1, XB2, s, -t414, 0.0D0, t416, t417, -t422, t466)
      t469 = t2 * t242
      t471 = t25 * t254 * t256
      t474 = log(0.4D1 * t68 * t471)
      t475 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t469, 0.0D0
     #, t417, 0.0D0, 0.0D0)
      t477 = t474 ** 2
      t478 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t469, 0.0D0
     #, t417, 0.0D0, 0.0D0)
      t491 = t7 * t15 * t478
      t494 = (-0.90D2 * t7 * t14 * (-t474 * t475 + t477 * t478 / 0.2D1) 
     #+ 0.180D3 * t7 * t54 * (t475 - t474 * t478) + t491) * t63 * t89
      t499 = log(0.4D1 * t282 * t22 * t255 * t256)
      t501 = t475 - t499 * t478
      t504 = t499 ** 2
      t510 = t504 * t475 / 0.2D1 - t504 * t499 * t478 / 0.6D1
      t515 = t7 * t51 * t478
      t519 = -t499 * t475 + t504 * t478 / 0.2D1
      t527 = log(0.4D1 * t389 * t471)
      t538 = (-0.90D2 * t7 * t14 * (t475 - t527 * t478) + 0.180D3 * t7 *
     # t54 * t478) * t63 * t110
      t541 = log(0.4D1 * t164 * t471)
      t543 = t541 ** 2
      t557 = (-0.90D2 * t7 * t14 * (-t541 * t475 + t543 * t478 / 0.2D1) 
     #+ 0.180D3 * t7 * t54 * (t475 - t541 * t478) + t491) * t109 * t89
      t559 = t494 / 0.720D3 - (-t7 * t15 * t501 + 0.90D2 * t7 * t14 * t5
     #10 - t515 - 0.180D3 * t7 * t54 * t519) * t89 / 0.720D3 + t538 / 0.
     #720D3 + t557 / 0.720D3
      t560 = FJET(XB1, XB2, s, -t469, t417, 0.0D0, 0.0D0, 0.0D0, t559)
      t576 = -(-t7 * t15 * t32 + 0.90D2 * t7 * t14 * t41 - t53 - 0.180D3
     # * t7 * t54 * t58) * t63 / 0.1440D4 + t91 + t112 + t134
      t577 = FJET(XB1, XB2, s, 0.0D0, t5, 0.0D0, -t4, 0.0D0, t576)
      t579 = FJET(XB1, XB2, s, t5, 0.0D0, -t4, 0.0D0, 0.0D0, t135)
      t581 = FJET(XB1, XB2, s, -t248, t250, t244, -t246, 0.0D0, t304)
      t583 = FJET(XB1, XB2, s, t416, t417, -t414, 0.0D0, -t422, t466)
      t585 = FJET(XB1, XB2, s, t338, 0.0D0, -t342, 0.0D0, 0.0D0, t409)
      t587 = FJET(XB1, XB2, s, 0.0D0, -t4, 0.0D0, t5, 0.0D0, t576)
      t603 = t494 / 0.720D3 - (-t7 * t15 * t501 + 0.90D2 * t7 * t14 * t5
     #10 - t515 - 0.180D3 * t7 * t54 * t519) * t89 / 0.720D3 + t538 / 0.
     #720D3 + t557 / 0.720D3
      t604 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t417, -t469, 0.0D0, t603)
      t606 = FJET(XB1, XB2, s, t417, -t469, 0.0D0, 0.0D0, 0.0D0, t559)
      t608 = t249 * z
      t612 = Sqrt(-x3 * t253 * x2 * t3)
      t613 = t327 * t612
      t614 = 0.2D1 * t613
      t619 = t469 * t140 * (-t113 - 0.1D1 + x3 + x1 - t249 - t252 + t608
     # + t614) * t254 * t336
      t621 = t241 * t245 * t336
      t622 = t113 * x1
      t624 = t326 * x1
      t626 = 0.2D1 * t613 * x2
      t627 = t113 * t252
      t629 = t326 * t252
      t630 = t249 - t608 - 0.2D1 * t622 + t624 - t326 + t324 + t626 - x2
     # - x3 + 0.2D1 * t627 - t629 - t614
      t633 = t469 * t630 * t254 * t336
      t636 = t417 * x3 * t140 * t336
      t637 = x2 * x1
      t638 = t637 * z
      t639 = x1 - t252 - t637 + t638 + x2 - t351 - 0.1D1
      t640 = t253 * t639
      t644 = t17 * x2
      t648 = t66 * t17
      t652 = 0.1D1 + 0.2D1 * t252 + t355 - t326 + t351 + t354 - t356 - 0
     #.3D1 * t622 + t624 + t626 - 0.3D1 * t638 + t644 * x1 + 0.2D1 * t16
     #3 * z - t648 * x2 + t92 + 0.2D1 * t613 * x1
      t671 = -t614 + 0.2D1 * t637 - 0.2D1 * t66 * z + t648 - t163 + 0.2D
     #1 * t613 * t638 + t66 + 0.4D1 * t627 - t629 - 0.2D1 * t613 * t637 
     #- 0.2D1 * t613 * t252 - 0.2D1 * t613 * t351 - x3 * t17 * t637 - 0.
     #2D1 * t67 * t351 + t67 * t644 - x2 - 0.2D1 * x1
      t673 = 0.1D1 / (t652 + t671)
      t674 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t619, -t633,
     # t621, t636, -t422)
      t683 = log(-0.4D1 * t282 * t283 * x2 * t165 * t257 * t345)
      t686 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t619, -t633,
     # t621, t636, -t422)
      t698 = -0.90D2 * t7 * t14 * (-t640 * t673 * t674 + t683 * t253 * t
     #639 * t673 * t686) - 0.180D3 * t7 * t54 * t640 * t673 * t686
      t701 = t698 * t63 * t110 / 0.720D3
      t702 = FJET(XB1, XB2, s, t619, t621, -t633, t636, -t422, t701)
      t705 = t63 * t109 * t89
      t708 = t136 * t135 + t239 * t238 + t305 * t304 + t322 * t321 + t41
     #0 * t409 + t467 * t466 + t560 * t559 + t577 * t576 + t579 * t135 +
     # t581 * t304 + t583 * t466 + t585 * t409 + t587 * t576 + t604 * t6
     #03 + t606 * t559 + t702 * t698 * t705 / 0.720D3
      t709 = FJET(XB1, XB2, s, t636, -t633, t621, t619, -t422, t701)
      t713 = FJET(XB1, XB2, s, -t633, t636, t619, t621, -t422, t701)
      t717 = FJET(XB1, XB2, s, 0.0D0, -t342, 0.0D0, t338, 0.0D0, t409)
      t719 = FJET(XB1, XB2, s, t621, t619, t636, -t633, -t422, t701)
      t723 = FJET(XB1, XB2, s, t417, t416, 0.0D0, -t414, -t422, t466)
      t725 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t728 = log(0.4D1 * t19 * t25)
      t729 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t734 = t728 ** 2
      t745 = t7 * t51 * t729
      t756 = t93 * t24
      t758 = log(0.4D1 * t756)
      t759 = t758 ** 2
      t760 = t759 * pi
      t765 = t759 * t758 * pi
      t767 = t758 * pi
      t779 = t8 ** 2
      t780 = t10 ** 2
      t788 = t759 ** 2
      t800 = log(0.4D1 * t19 * t66 * t22 * t24)
      t802 = t800 ** 2
      t815 = t7 * t15 * t729
      t822 = log(0.4D1 * t282 * t25)
      t827 = t822 ** 2
      t849 = log(0.4D1 * t92 * t756)
      t864 = log(0.4D1 * t163 * t756)
      t866 = t864 ** 2
      t884 = log(0.4D1 * t113 * t756)
      t886 = t884 ** 2
      t904 = log(0.4D1 * t208 * t25)
      t909 = t904 ** 2
      t929 = -(t7 * t15 * (t725 - t728 * t729) - 0.90D2 * t7 * t14 * (t7
     #34 * t725 / 0.2D1 - t734 * t728 * t729 / 0.6D1) + t745 + 0.180D3 *
     # t7 * t54 * (-t728 * t725 + t734 * t729 / 0.2D1)) * t63 / 0.1440D4
     # - (0.90D2 * t760 * lh + pi * t50 + 0.15D2 * t765 - t767 * t12) * 
     #t6 * t14 * t725 / 0.1440D4 - (-0.30D2 * t765 * lh + t760 * t12 / 0
     #.2D1 - t767 * t50 + pi * (-t779 - 0.60D2 * t780 - 0.480D3 * lh * z
     #eta3 + 0.60D2 * t10 * t8) - 0.15D2 / 0.4D1 * t788 * pi) * t6 * t14
     # * t729 / 0.1440D4 + (-0.90D2 * t7 * t14 * (t800 * t725 - t802 * t
     #729 / 0.2D1) + 0.180D3 * t7 * t54 * (-t725 + t800 * t729) - t815) 
     #* t63 * t89 / 0.720D3 - (t7 * t15 * (t725 - t822 * t729) - 0.90D2 
     #* t7 * t14 * (t827 * t725 / 0.2D1 - t827 * t822 * t729 / 0.6D1) + 
     #t745 + 0.180D3 * t7 * t54 * (-t822 * t725 + t827 * t729 / 0.2D1)) 
     #* t89 / 0.720D3 + (-0.90D2 * t7 * t14 * (-t725 + t849 * t729) - 0.
     #180D3 * t7 * t54 * t729) * t63 * t110 / 0.720D3 + (-0.90D2 * t7 * 
     #t14 * (t864 * t725 - t866 * t729 / 0.2D1) + 0.180D3 * t7 * t54 * (
     #-t725 + t864 * t729) - t815) * t109 * t89 / 0.720D3 + (-0.90D2 * t
     #7 * t14 * (t884 * t725 - t886 * t729 / 0.2D1) + 0.180D3 * t7 * t54
     # * (-t725 + t884 * t729) - t815) * t63 * t109 / 0.1440D4 - (t7 * t
     #15 * (t725 - t904 * t729) - 0.90D2 * t7 * t14 * (t909 * t725 / 0.2
     #D1 - t909 * t904 * t729 / 0.6D1) + t745 + 0.180D3 * t7 * t54 * (-t
     #904 * t725 + t909 * t729 / 0.2D1)) * t109 / 0.1440D4
      t930 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t929)
      t932 = FJET(XB1, XB2, s, 0.0D0, -t142, 0.0D0, t139, 0.0D0, t321)
      t934 = FJET(XB1, XB2, s, -t142, 0.0D0, t139, 0.0D0, 0.0D0, t238)
      t936 = FJET(XB1, XB2, s, 0.0D0, -t414, t417, t416, -t422, t466)
      t938 = FJET(XB1, XB2, s, t250, -t248, -t246, t244, 0.0D0, t304)
      t940 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t469, t417, 0.0D0, t603)
      t942 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t929)
      t944 = FJET(XB1, XB2, s, -t342, 0.0D0, t338, 0.0D0, 0.0D0, t409)
      t946 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t929)
      t948 = FJET(XB1, XB2, s, -t246, t244, t250, -t248, 0.0D0, t304)
      t950 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t929)
      t952 = t709 * t698 * t705 / 0.720D3 + t713 * t698 * t705 / 0.720D3
     # + t717 * t409 + t719 * t698 * t705 / 0.720D3 + t723 * t466 + t930
     # * t929 + t932 * t321 + t934 * t238 + t936 * t466 + t938 * t304 + 
     #t940 * t603 + t942 * t929 + t944 * t409 + t946 * t929 + t948 * t30
     #4 + t950 * t929
      rrqqbar2gght5s2e1 = t708 + t952

      end function



      doubleprecision function rrqqbar2gght5s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + x2
      t3 = -0.1D1 + z
      t4 = -0.1D1 + x1
      t5 = t3 * t4
      t6 = t1 * s * t5
      t7 = t3 * s
      t8 = t7 * x1
      t10 = x1 * z
      t11 = 0.1D1 - x1 + t10
      t12 = 0.1D1 / t11
      t14 = t7 * t4 * x2 * t12
      t15 = t3 ** 2
      t20 = s * t15 * x2 * t4 * x1 * t12
      t21 = 0.1D1 / t3
      t22 = pi * t21
      t23 = s ** 2
      t24 = 0.1D1 / t23
      t25 = t22 * t24
      t26 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t6, -t14, t8,
     # 0.0D0, -t20)
      t27 = 0.1D1 / x3
      t29 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t31 = t29 * t30
      t35 = x1 ** 2
      t36 = t35 * x2
      t37 = z ** 2
      t38 = 0.1D1 / t37
      t39 = x4 * pi
      t40 = Sin(t39)
      t41 = t40 ** 2
      t42 = t38 * t41
      t44 = t15 ** 2
      t45 = t44 * t12
      t46 = t4 ** 2
      t47 = t1 ** 2
      t52 = log(0.4D1 * t36 * t42 * t45 * t46 * t47)
      t54 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t6, -t14, t8,
     # 0.0D0, -t20)
      t59 = lh * t24
      t67 = t25 * t26 * t27 * t31 / 0.8D1 + (-0.90D2 * t22 * t24 * (t52 
     #* t26 - t54) - 0.180D3 * t22 * t59 * t26) * t29 * t30 / 0.720D3
      t68 = FJET(XB1, XB2, s, t6, t8, -t14, 0.0D0, -t20, t67)
      t70 = x2 * x3
      t71 = 0.3D1 * t70
      t72 = x2 ** 2
      t73 = t72 * x3
      t74 = cos(t39)
      t75 = -0.1D1 + x3
      t77 = Sqrt(-t70 * t75)
      t78 = t74 * t77
      t79 = 0.2D1 * t78
      t81 = 0.2D1 * t78 * x2
      t83 = -0.1D1 + t70
      t84 = 0.1D1 / t83
      t86 = t7 * (-x2 - x3 + t71 - t73 - t79 + t81) * t84
      t90 = t7 * t1 * (-t70 - 0.1D1 + x3 + t79) * t84
      t91 = x2 * z
      t92 = 0.1D1 - x2 + t91
      t93 = t24 * t92
      t95 = t73 * z
      t96 = 0.2D1 * t70
      t97 = t70 * z
      t101 = 0.1D1 / (-t95 + t73 + x2 - t96 - t91 + t97 - t81 + 0.2D1 * 
     #t78 * t91 + t79 - 0.1D1)
      t102 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t90, t86, 0
     #.0D0, 0.0D0, 0.0D0)
      t103 = t101 * t102
      t105 = t27 * t29 * t30
      t110 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t90, t86, 0
     #.0D0, 0.0D0, 0.0D0)
      t114 = t83 ** 2
      t120 = log(-0.4D1 * t70 * t42 * t44 * t47 * t75 / t114)
      t135 = -t22 * t93 * t103 * t105 / 0.8D1 + (-0.90D2 * t22 * t24 * (
     #t92 * t101 * t110 - t120 * t92 * t103) + 0.180D3 * t22 * lh * t93 
     #* t103) * t27 * t29 / 0.1440D4
      t136 = FJET(XB1, XB2, s, t86, 0.0D0, -t90, 0.0D0, 0.0D0, t135)
      t138 = FJET(XB1, XB2, s, 0.0D0, -t14, t8, t6, -t20, t67)
      t140 = x3 * x1
      t141 = t7 * t140
      t143 = x3 * s * t5
      t144 = t75 * s
      t145 = t3 * x1
      t146 = t144 * t145
      t147 = t144 * t5
      t148 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t147, -t143,
     # -t146, t141, 0.0D0)
      t149 = x3 * t35
      t155 = log(-0.4D1 * t149 * t42 * t45 * t46 * t75)
      t156 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t147, -t143,
     # -t146, t141, 0.0D0)
      t173 = (-0.90D2 * t22 * t24 * (-t148 + t155 * t156) - 0.180D3 * t2
     #2 * t59 * t156) * t27 * t30 / 0.720D3 + t25 * t156 * t27 * t31 / 0
     #.8D1
      t174 = FJET(XB1, XB2, s, t141, -t143, -t146, t147, 0.0D0, t173)
      t176 = t7 * t75
      t177 = t7 * x3
      t178 = x3 * t38
      t179 = t41 * t44
      t180 = t179 * t75
      t183 = log(-0.4D1 * t178 * t180)
      t184 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t176, t177,
     # 0.0D0, 0.0D0, 0.0D0)
      t186 = t183 ** 2
      t187 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t176, t177,
     # 0.0D0, 0.0D0, 0.0D0)
      t190 = -t183 * t184 + t186 * t187 / 0.2D1
      t195 = t184 - t183 * t187
      t199 = pi ** 2
      t201 = lh ** 2
      t203 = 0.30D2 * t199 - 0.180D3 * t201
      t204 = t203 * t24
      t206 = t22 * t204 * t187
      t210 = t70 * t38
      t213 = log(-0.4D1 * t210 * t180)
      t221 = 0.180D3 * t22 * t59 * t187
      t225 = (-0.90D2 * t22 * t24 * (t184 - t213 * t187) + t221) * t27 *
     # t29 / 0.1440D4
      t226 = t149 * t38
      t229 = log(-0.4D1 * t226 * t180)
      t238 = (-0.90D2 * t22 * t24 * (t184 - t229 * t187) + t221) * t27 *
     # t30 / 0.720D3
      t242 = t25 * t187 * t27 * t31 / 0.8D1
      t243 = -(0.90D2 * t22 * t24 * t190 - 0.180D3 * t22 * t59 * t195 - 
     #t206) * t27 / 0.1440D4 + t225 + t238 - t242
      t244 = FJET(XB1, XB2, s, -t176, 0.0D0, t177, 0.0D0, 0.0D0, t243)
      t246 = FJET(XB1, XB2, s, t147, -t146, -t143, t141, 0.0D0, t173)
      t249 = x2 * t3 * s
      t251 = t1 * t3 * s
      t252 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t251, t249,
     # 0.0D0, 0.0D0, 0.0D0)
      t256 = t25 * t252 * t27 * t31 / 0.8D1
      t257 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t251, t249,
     # 0.0D0, 0.0D0, 0.0D0)
      t258 = t36 * t38
      t259 = t179 * t47
      t262 = log(0.4D1 * t258 * t259)
      t270 = 0.180D3 * t22 * t59 * t252
      t274 = (-0.90D2 * t22 * t24 * (t257 - t262 * t252) + t270) * t29 *
     # t30 / 0.720D3
      t277 = log(0.4D1 * t210 * t259)
      t286 = (-0.90D2 * t22 * t24 * (t257 - t277 * t252) + t270) * t27 *
     # t29 / 0.1440D4
      t287 = x2 * t38
      t290 = log(0.4D1 * t287 * t259)
      t292 = t290 ** 2
      t295 = t290 * t257 - t292 * t252 / 0.2D1
      t300 = -t257 + t290 * t252
      t305 = t22 * t204 * t252
      t309 = -t256 + t274 + t286 - (-0.90D2 * t22 * t24 * t295 + 0.180D3
     # * t22 * t59 * t300 - t305) * t29 / 0.1440D4
      t310 = FJET(XB1, XB2, s, 0.0D0, t249, 0.0D0, -t251, 0.0D0, t309)
      t312 = FJET(XB1, XB2, s, -t90, 0.0D0, t86, 0.0D0, 0.0D0, t135)
      t314 = FJET(XB1, XB2, s, 0.0D0, t86, 0.0D0, -t90, 0.0D0, t135)
      t327 = -(0.90D2 * t22 * t24 * t190 - 0.180D3 * t22 * t59 * t195 - 
     #t206) * t27 / 0.1440D4 + t225 + t238 - t242
      t328 = FJET(XB1, XB2, s, 0.0D0, -t176, 0.0D0, t177, 0.0D0, t327)
      t330 = FJET(XB1, XB2, s, 0.0D0, t177, 0.0D0, -t176, 0.0D0, t327)
      t332 = FJET(XB1, XB2, s, t177, 0.0D0, -t176, 0.0D0, 0.0D0, t243)
      t334 = t7 * t4
      t335 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t334, 0.0D0
     #, t8, 0.0D0, 0.0D0)
      t337 = t179 * t12 * t46
      t340 = log(0.4D1 * t226 * t337)
      t341 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t334, 0.0D0
     #, t8, 0.0D0, 0.0D0)
      t349 = 0.180D3 * t22 * t59 * t341
      t353 = (-0.90D2 * t22 * t24 * (t335 - t340 * t341) + t349) * t27 *
     # t30 / 0.720D3
      t357 = t25 * t341 * t27 * t31 / 0.8D1
      t360 = log(0.4D1 * t258 * t337)
      t369 = (-0.90D2 * t22 * t24 * (t335 - t360 * t341) + t349) * t29 *
     # t30 / 0.720D3
      t370 = t38 * t35
      t375 = log(0.4D1 * t370 * t41 * t45 * t46)
      t377 = t375 ** 2
      t380 = -t375 * t335 + t377 * t341 / 0.2D1
      t385 = t335 - t375 * t341
      t390 = t22 * t204 * t341
      t394 = t353 - t357 + t369 - (0.90D2 * t22 * t24 * t380 - 0.180D3 *
     # t22 * t59 * t385 - t390) * t30 / 0.720D3
      t395 = FJET(XB1, XB2, s, -t334, t8, 0.0D0, 0.0D0, 0.0D0, t394)
      t397 = FJET(XB1, XB2, s, -t14, 0.0D0, t6, t8, -t20, t67)
      t410 = t353 - t357 + t369 - (0.90D2 * t22 * t24 * t380 - 0.180D3 *
     # t22 * t59 * t385 - t390) * t30 / 0.720D3
      t411 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t334, t8, 0.0D0, t410)
      t413 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t8, -t334, 0.0D0, t410)
      t415 = t68 * t67 + t136 * t135 + t138 * t67 + t174 * t173 + t244 *
     # t243 + t246 * t173 + t310 * t309 + t312 * t135 + t314 * t135 + t3
     #28 * t327 + t330 * t327 + t332 * t243 + t395 * t394 + t397 * t67 +
     # t411 * t410 + t413 * t410
      t427 = -t256 + t274 + t286 - (-0.90D2 * t22 * t24 * t295 + 0.180D3
     # * t22 * t59 * t300 - t305) * t29 / 0.1440D4
      t428 = FJET(XB1, XB2, s, t249, 0.0D0, -t251, 0.0D0, 0.0D0, t427)
      t430 = FJET(XB1, XB2, s, -t146, t147, t141, -t143, 0.0D0, t173)
      t432 = FJET(XB1, XB2, s, t8, t6, 0.0D0, -t14, -t20, t67)
      t434 = FJET(XB1, XB2, s, t8, -t334, 0.0D0, 0.0D0, 0.0D0, t394)
      t438 = log(0.4D1 * t178 * t179)
      t439 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t7, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t441 = t438 ** 2
      t442 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t7, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t455 = t22 * t204 * t442
      t459 = t42 * t44
      t461 = log(0.4D1 * t459)
      t462 = t461 * pi
      t465 = t461 ** 2
      t466 = t465 * pi
      t494 = log(0.4D1 * t70 * t459)
      t502 = 0.180D3 * t22 * t59 * t442
      t509 = log(0.4D1 * t287 * t179)
      t511 = t509 ** 2
      t530 = log(0.4D1 * t178 * t35 * t41 * t44)
      t546 = log(0.4D1 * t36 * t459)
      t558 = log(0.4D1 * t370 * t179)
      t560 = t558 ** 2
      t575 = -(-0.90D2 * t22 * t24 * (-t438 * t439 + t441 * t442 / 0.2D1
     #) + 0.180D3 * t22 * t59 * (t439 - t438 * t442) + t455) * t27 / 0.1
     #440D4 - (-0.180D3 * t462 * lh - 0.45D2 * t466 + pi * t203) * t21 *
     # t24 * t439 / 0.1440D4 - (0.90D2 * t466 * lh + pi * (0.240D3 * zet
     #a3 + 0.120D3 * t201 * lh - 0.60D2 * lh * t199) + 0.15D2 * t465 * t
     #461 * pi - t462 * t203) * t21 * t24 * t442 / 0.1440D4 + (-0.90D2 *
     # t22 * t24 * (-t439 + t494 * t442) - t502) * t27 * t29 / 0.1440D4 
     #- (-0.90D2 * t22 * t24 * (-t509 * t439 + t511 * t442 / 0.2D1) + 0.
     #180D3 * t22 * t59 * (t439 - t509 * t442) + t455) * t29 / 0.1440D4 
     #+ (-0.90D2 * t22 * t24 * (-t439 + t530 * t442) - t502) * t27 * t30
     # / 0.720D3 + t25 * t442 * t27 * t31 / 0.8D1 + (-0.90D2 * t22 * t24
     # * (-t439 + t546 * t442) - t502) * t29 * t30 / 0.720D3 - (-0.90D2 
     #* t22 * t24 * (-t558 * t439 + t560 * t442 / 0.2D1) + 0.180D3 * t22
     # * t59 * (t439 - t558 * t442) + t455) * t30 / 0.720D3
      t576 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t7, 0.0D0, t575)
      t578 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t7, 0.0D0, 0.0D0, t575)
      t580 = t140 * z
      t584 = Sqrt(-x3 * t11 * x2 * t75)
      t585 = t74 * t584
      t586 = 0.2D1 * t585
      t591 = t334 * t1 * (-t70 - 0.1D1 + x3 + x1 - t140 - t10 + t580 + t
     #586) * t12 * t84
      t593 = t144 * t145 * t84
      t594 = t70 * x1
      t596 = t73 * x1
      t598 = 0.2D1 * t585 * x2
      t599 = t70 * t10
      t601 = t73 * t10
      t602 = t140 - t580 - 0.2D1 * t594 + t596 - t73 + t71 + t598 - x2 -
     # x3 + 0.2D1 * t599 - t601 - t586
      t605 = t334 * t602 * t12 * t84
      t608 = t8 * x3 * t1 * t84
      t610 = x2 * x1
      t611 = t610 * z
      t612 = x1 - t10 - t610 + t611 + x2 - t91 - 0.1D1
      t626 = t37 * x2
      t633 = 0.1D1 + 0.4D1 * t599 - t601 - 0.2D1 * t585 * t610 - 0.2D1 *
     # t585 * t10 - 0.2D1 * t585 * t91 - x3 * t37 * t610 - 0.2D1 * t149 
     #* t91 + t35 + t149 * t626 - 0.3D1 * t594 + t596 + t598 - 0.3D1 * t
     #611 + t626 * x1 + 0.2D1 * t36 * z
      t634 = t35 * t37
      t646 = -t634 * x2 - t97 + t149 * x2 + t95 + 0.2D1 * t585 * x1 - 0.
     #2D1 * x1 - x2 + 0.2D1 * t585 * t611 + t96 + 0.2D1 * t10 - t586 - t
     #73 + 0.2D1 * t610 + t91 - 0.2D1 * t35 * z + t634 - t36
      t648 = 0.1D1 / (t633 + t646)
      t649 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t591, -t605,
     # t593, t608, -t20)
      t653 = t22 * t24 * t11 * t612 * t648 * t649 * t105 / 0.8D1
      t654 = FJET(XB1, XB2, s, t591, t593, -t605, t608, -t20, t653)
      t657 = t21 * t24 * t11
      t661 = t612 * t648 * t649 * t105
      t664 = FJET(XB1, XB2, s, t593, t591, t608, -t605, -t20, t653)
      t669 = FJET(XB1, XB2, s, -t605, t608, t591, t593, -t20, t653)
      t674 = FJET(XB1, XB2, s, t608, -t605, t593, t591, -t20, t653)
      t679 = FJET(XB1, XB2, s, -t251, 0.0D0, t249, 0.0D0, 0.0D0, t427)
      t681 = FJET(XB1, XB2, s, -t143, t141, t147, -t146, 0.0D0, t173)
      t683 = FJET(XB1, XB2, s, 0.0D0, -t251, 0.0D0, t249, 0.0D0, t309)
      t685 = FJET(XB1, XB2, s, t7, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t575)
      t687 = FJET(XB1, XB2, s, 0.0D0, -t90, 0.0D0, t86, 0.0D0, t135)
      t689 = FJET(XB1, XB2, s, 0.0D0, t7, 0.0D0, 0.0D0, 0.0D0, t575)
      t691 = t427 * t428 + t430 * t173 + t432 * t67 + t434 * t394 + t576
     # * t575 + t578 * t575 + t654 * pi * t657 * t661 / 0.8D1 + t664 * p
     #i * t657 * t661 / 0.8D1 + t669 * pi * t657 * t661 / 0.8D1 + t674 *
     # pi * t657 * t661 / 0.8D1 + t679 * t427 + t681 * t173 + t683 * t30
     #9 + t685 * t575 + t687 * t135 + t689 * t575
      rrqqbar2gght5s2e0 = t415 + t691

      end function



      doubleprecision function rrqqbar2gght5s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x2 * x3
      t5 = x2 ** 2
      t6 = t5 * x3
      t7 = x4 * pi
      t8 = cos(t7)
      t9 = -0.1D1 + x3
      t11 = Sqrt(-t3 * t9)
      t12 = t8 * t11
      t13 = 0.2D1 * t12
      t15 = 0.2D1 * t12 * x2
      t18 = 0.1D1 / (-0.1D1 + t3)
      t20 = t2 * (-x2 - x3 + 0.3D1 * t3 - t6 - t13 + t15) * t18
      t21 = -0.1D1 + x2
      t25 = t2 * t21 * (-t3 - 0.1D1 + x3 + t13) * t18
      t26 = 0.1D1 / t1
      t27 = pi * t26
      t28 = s ** 2
      t29 = 0.1D1 / t28
      t30 = x2 * z
      t31 = 0.1D1 - x2 + t30
      t40 = 0.1D1 / (-t6 * z + t6 + x2 - 0.2D1 * t3 - t30 + t3 * z - t15
     # + 0.2D1 * t12 * t30 + t13 - 0.1D1)
      t41 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t25, t20, 0.
     #0D0, 0.0D0, 0.0D0)
      t43 = 0.1D1 / x3
      t44 = 0.1D1 / x2
      t48 = t27 * t29 * t31 * t40 * t41 * t43 * t44 / 0.16D2
      t49 = FJET(XB1, XB2, s, 0.0D0, t20, 0.0D0, -t25, 0.0D0, -t48)
      t51 = t26 * t29
      t56 = t31 * t40 * t41 * t43 * t44
      t60 = -0.1D1 + x1
      t61 = t1 * t60
      t62 = t21 * s * t61
      t63 = t2 * x1
      t67 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t69 = t2 * t60 * x2 * t67
      t70 = t1 ** 2
      t75 = s * t70 * x2 * t60 * x1 * t67
      t76 = t27 * t29
      t77 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t62, -t69, t6
     #3, 0.0D0, -t75)
      t79 = 0.1D1 / x1
      t82 = t76 * t77 * t44 * t79 / 0.8D1
      t83 = FJET(XB1, XB2, s, t62, t63, -t69, 0.0D0, -t75, t82)
      t88 = t29 * t77 * t44 * t79
      t92 = t2 * x1 * x3
      t94 = x3 * s * t61
      t95 = t9 * s
      t97 = t95 * t1 * x1
      t98 = t95 * t61
      t99 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t98, -t94, -t
     #97, t92, 0.0D0)
      t103 = t76 * t99 * t43 * t79 / 0.8D1
      t104 = FJET(XB1, XB2, s, t92, -t94, -t97, t98, 0.0D0, t103)
      t109 = t29 * t99 * t43 * t79
      t112 = FJET(XB1, XB2, s, t98, -t97, -t94, t92, 0.0D0, t103)
      t117 = FJET(XB1, XB2, s, -t69, 0.0D0, t62, t63, -t75, t82)
      t122 = FJET(XB1, XB2, s, -t94, t92, t98, -t97, 0.0D0, t103)
      t127 = FJET(XB1, XB2, s, 0.0D0, -t69, t63, t62, -t75, t82)
      t132 = FJET(XB1, XB2, s, -t97, t98, t92, -t94, 0.0D0, t103)
      t137 = FJET(XB1, XB2, s, t63, t62, 0.0D0, -t69, -t75, t82)
      t142 = t2 * t9
      t143 = t2 * x3
      t144 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t142, t143,
     # 0.0D0, 0.0D0, 0.0D0)
      t145 = z ** 2
      t146 = 0.1D1 / t145
      t147 = x3 * t146
      t148 = Sin(t7)
      t149 = t148 ** 2
      t150 = t70 ** 2
      t151 = t149 * t150
      t155 = log(-0.4D1 * t147 * t151 * t9)
      t156 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t142, t143,
     # 0.0D0, 0.0D0, 0.0D0)
      t158 = t144 - t155 * t156
      t162 = lh * t29
      t165 = 0.180D3 * t27 * t162 * t156
      t169 = t156 * t43
      t172 = t76 * t169 * t44 / 0.16D2
      t175 = t76 * t169 * t79 / 0.8D1
      t176 = -(0.90D2 * t27 * t29 * t158 - t165) * t43 / 0.1440D4 - t172
     # - t175
      t177 = FJET(XB1, XB2, s, -t142, 0.0D0, t143, 0.0D0, 0.0D0, t176)
      t180 = x2 * t1 * s
      t182 = t21 * t1 * s
      t183 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t182, t180,
     # 0.0D0, 0.0D0, 0.0D0)
      t187 = t76 * t183 * t44 * t79 / 0.8D1
      t191 = t76 * t183 * t43 * t44 / 0.16D2
      t192 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t182, t180,
     # 0.0D0, 0.0D0, 0.0D0)
      t193 = x2 * t146
      t194 = t21 ** 2
      t198 = log(0.4D1 * t193 * t151 * t194)
      t200 = -t192 + t198 * t183
      t206 = 0.180D3 * t27 * t162 * t183
      t210 = -t187 - t191 - (-0.90D2 * t27 * t29 * t200 - t206) * t44 / 
     #0.1440D4
      t211 = FJET(XB1, XB2, s, 0.0D0, t180, 0.0D0, -t182, 0.0D0, t210)
      t220 = -(0.90D2 * t27 * t29 * t158 - t165) * t43 / 0.1440D4 - t172
     # - t175
      t221 = FJET(XB1, XB2, s, 0.0D0, t143, 0.0D0, -t142, 0.0D0, t220)
      t223 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t226 = log(0.4D1 * t147 * t151)
      t227 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0
     #.0D0, 0.0D0, 0.0D0)
      t235 = 0.180D3 * t27 * t162 * t227
      t244 = log(0.4D1 * t146 * t149 * t150)
      t245 = t244 * pi
      t254 = t244 ** 2
      t257 = pi ** 2
      t259 = lh ** 2
      t268 = t227 * t43
      t274 = log(0.4D1 * t193 * t151)
      t287 = x1 ** 2
      t288 = t146 * t287
      t291 = log(0.4D1 * t288 * t151)
      t303 = -(-0.90D2 * t27 * t29 * (t223 - t226 * t227) + t235) * t43 
     #/ 0.1440D4 - (0.180D3 * pi * lh + 0.90D2 * t245) * t26 * t29 * t22
     #3 / 0.1440D4 - (-0.180D3 * t245 * lh - 0.45D2 * t254 * pi + pi * (
     #0.30D2 * t257 - 0.180D3 * t259)) * t26 * t29 * t227 / 0.1440D4 + t
     #76 * t268 * t44 / 0.16D2 - (-0.90D2 * t27 * t29 * (t223 - t274 * t
     #227) + t235) * t44 / 0.1440D4 + t76 * t227 * t44 * t79 / 0.8D1 - (
     #-0.90D2 * t27 * t29 * (t223 - t291 * t227) + t235) * t79 / 0.720D3
     # + t76 * t268 * t79 / 0.8D1
      t304 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t303)
      t306 = FJET(XB1, XB2, s, 0.0D0, -t25, 0.0D0, t20, 0.0D0, -t48)
      t311 = -t49 * pi * t51 * t56 / 0.16D2 + t83 * pi * t26 * t88 / 0.8
     #D1 + t104 * pi * t26 * t109 / 0.8D1 + t112 * pi * t26 * t109 / 0.8
     #D1 + t117 * pi * t26 * t88 / 0.8D1 + t122 * pi * t26 * t109 / 0.8D
     #1 + t127 * pi * t26 * t88 / 0.8D1 + t132 * pi * t26 * t109 / 0.8D1
     # + t137 * pi * t26 * t88 / 0.8D1 + t177 * t176 + t211 * t210 + t22
     #1 * t220 + t304 * t303 - t306 * pi * t51 * t56 / 0.16D2
      t312 = FJET(XB1, XB2, s, -t25, 0.0D0, t20, 0.0D0, 0.0D0, -t48)
      t317 = t2 * t60
      t318 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t317, 0.0D0
     #, t63, 0.0D0, 0.0D0)
      t322 = t76 * t318 * t44 * t79 / 0.8D1
      t323 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t317, 0.0D0
     #, t63, 0.0D0, 0.0D0)
      t326 = t60 ** 2
      t330 = log(0.4D1 * t288 * t149 * t150 * t67 * t326)
      t332 = -t323 + t330 * t318
      t338 = 0.180D3 * t27 * t162 * t318
      t345 = t76 * t318 * t43 * t79 / 0.8D1
      t346 = -t322 - (-0.90D2 * t27 * t29 * t332 - t338) * t79 / 0.720D3
     # - t345
      t347 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t317, t63, 0.0D0, t346)
      t349 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t63, -t317, 0.0D0, t346)
      t351 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t303)
      t353 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t303)
      t355 = FJET(XB1, XB2, s, t143, 0.0D0, -t142, 0.0D0, 0.0D0, t176)
      t364 = -t187 - t191 - (-0.90D2 * t27 * t29 * t200 - t206) * t44 / 
     #0.1440D4
      t365 = FJET(XB1, XB2, s, -t182, 0.0D0, t180, 0.0D0, 0.0D0, t364)
      t374 = -t322 - (-0.90D2 * t27 * t29 * t332 - t338) * t79 / 0.720D3
     # - t345
      t375 = FJET(XB1, XB2, s, t63, -t317, 0.0D0, 0.0D0, 0.0D0, t374)
      t377 = FJET(XB1, XB2, s, -t317, t63, 0.0D0, 0.0D0, 0.0D0, t374)
      t379 = FJET(XB1, XB2, s, 0.0D0, -t182, 0.0D0, t180, 0.0D0, t210)
      t381 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t303)
      t383 = FJET(XB1, XB2, s, t180, 0.0D0, -t182, 0.0D0, 0.0D0, t364)
      t385 = FJET(XB1, XB2, s, 0.0D0, -t142, 0.0D0, t143, 0.0D0, t220)
      t387 = FJET(XB1, XB2, s, t20, 0.0D0, -t25, 0.0D0, 0.0D0, -t48)
      t392 = -t312 * pi * t51 * t56 / 0.16D2 + t347 * t346 + t349 * t346
     # + t351 * t303 + t353 * t303 + t355 * t176 + t365 * t364 + t375 * 
     #t374 + t377 * t374 + t379 * t210 + t381 * t303 + t383 * t364 + t38
     #5 * t220 - t387 * pi * t51 * t56 / 0.16D2
      rrqqbar2gght5s2em1 = t311 + t392

      end function



      doubleprecision function rrqqbar2gght5s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.
     #0D0, 0.0D0, 0.0D0)
      t27 = z ** 2
      t30 = Sin(x4 * pi)
      t31 = t30 ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t31 * t34)
      t44 = t4 * t8 * t9 / 0.16D2 + t4 * t8 * t13 / 0.16D2 + t4 * t8 * t
     #17 / 0.8D1 + t4 * t6 * t21 / 0.16D2 - (0.180D3 * pi * lh + 0.90D2 
     #* t37 * pi) * t3 * t8 / 0.1440D4
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t44)
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t44)
      t49 = t2 * x1
      t51 = t2 * (-0.1D1 + x1)
      t52 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t51, 0.0D0, 
     #t49, 0.0D0, 0.0D0)
      t54 = t6 * t52 * t17
      t56 = t4 * t54 / 0.8D1
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t49, -t51, 0.0D0, -t56)
      t62 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t51, t49, 0.0D0, -t56)
      t67 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t44)
      t69 = t2 * x3
      t71 = t2 * (-0.1D1 + x3)
      t72 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, t69, 0.
     #0D0, 0.0D0, 0.0D0)
      t74 = t6 * t72 * t9
      t76 = t4 * t74 / 0.16D2
      t77 = FJET(XB1, XB2, s, 0.0D0, t69, 0.0D0, -t71, 0.0D0, -t76)
      t83 = x2 * t1 * s
      t86 = (-0.1D1 + x2) * t1 * s
      t87 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t86, t83, 0.
     #0D0, 0.0D0, 0.0D0)
      t89 = t6 * t87 * t13
      t91 = t4 * t89 / 0.16D2
      t92 = FJET(XB1, XB2, s, 0.0D0, t83, 0.0D0, -t86, 0.0D0, -t91)
      t97 = FJET(XB1, XB2, s, 0.0D0, -t86, 0.0D0, t83, 0.0D0, -t91)
      t102 = FJET(XB1, XB2, s, 0.0D0, -t71, 0.0D0, t69, 0.0D0, -t76)
      t107 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t44)
      t109 = FJET(XB1, XB2, s, t49, -t51, 0.0D0, 0.0D0, 0.0D0, -t56)
      t114 = FJET(XB1, XB2, s, t69, 0.0D0, -t71, 0.0D0, 0.0D0, -t76)
      t119 = FJET(XB1, XB2, s, t83, 0.0D0, -t86, 0.0D0, 0.0D0, -t91)
      t124 = FJET(XB1, XB2, s, -t71, 0.0D0, t69, 0.0D0, 0.0D0, -t76)
      t129 = FJET(XB1, XB2, s, -t86, 0.0D0, t83, 0.0D0, 0.0D0, -t91)
      t134 = FJET(XB1, XB2, s, -t51, t49, 0.0D0, 0.0D0, 0.0D0, -t56)
      rrqqbar2gght5s2em2 = t45 * t44 + t47 * t44 - t57 * pi * t3 * t54 /
     # 0.8D1 - t62 * pi * t3 * t54 / 0.8D1 + t67 * t44 - t77 * pi * t3 *
     # t74 / 0.16D2 - t92 * pi * t3 * t89 / 0.16D2 - t97 * pi * t3 * t89
     # / 0.16D2 - t102 * pi * t3 * t74 / 0.16D2 + t107 * t44 - t109 * pi
     # * t3 * t54 / 0.8D1 - t114 * pi * t3 * t74 / 0.16D2 - t119 * pi * 
     #t3 * t89 / 0.16D2 - t124 * pi * t3 * t74 / 0.16D2 - t129 * pi * t3
     # * t89 / 0.16D2 - t134 * pi * t3 * t54 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght5s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t10 = pi * t3 * t6 * t7 / 0.16D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = t3 * t6 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s2em3 = t11 * pi * t14 / 0.16D2 + t16 * pi * t14 / 0.
     #16D2 + t19 * pi * t14 / 0.16D2 + t22 * pi * t14 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght5s2em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght5s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x1 ** 2
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t21 = log(0.4D1 * t10 * t15 * t17)
      t22 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t24 = t21 ** 2
      t25 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t28 = t11 * t9
      t29 = t28 * x3
      t30 = t14 * t17
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t33 = t30 * t32
      t36 = log(-0.4D1 * t29 * t33)
      t37 = t36 * z
      t39 = t36 ** 2
      t44 = cos(t12)
      t45 = x3 * z
      t47 = Sqrt(-t45 * t31)
      t51 = 0.1D1 / (-z - x3 + 0.2D1 * t47 * t44)
      t57 = lh * t6
      t59 = z * t22
      t67 = pi ** 2
      t69 = lh ** 2
      t71 = 0.30D2 * t67 - 0.180D3 * t69
      t72 = t71 * t6
      t79 = 0.1D1 / x3
      t81 = 0.1D1 / x1
      t86 = log(0.4D1 * t28 * t30)
      t91 = t86 ** 2
      t106 = 0.240D3 * zeta3 + 0.120D3 * t69 * lh - 0.60D2 * lh * t67
      t107 = t106 * t6
      t120 = x2 ** 2
      t125 = log(-0.4D1 * t29 * t30 * t120 * t32)
      t130 = x3 * t120
      t131 = t130 * t11
      t132 = t9 * t14
      t133 = -0.1D1 + x2
      t138 = log(-0.4D1 * t131 * t132 * t17 * t133)
      t140 = t132 * t17
      t143 = log(0.4D1 * t131 * t140)
      t149 = t4 * lh
      t150 = t6 * z
      t152 = t150 * t25 * t51
      t157 = 0.1D1 / x2
      t158 = t157 * t81
      t161 = t120 * t11
      t163 = t30 * t133
      t166 = log(-0.4D1 * t161 * t9 * t163)
      t168 = t166 ** 2
      t173 = log(0.4D1 * t161 * t140)
      t175 = t173 ** 2
      t192 = pi * t22
      t193 = t3 * lh
      t196 = pi * t25
      t197 = t3 * t71
      t203 = log(-0.4D1 * t10 * t33)
      t208 = log(0.4D1 * t10 * t30)
      t211 = t6 * t25
      t212 = t208 ** 2
      t214 = t203 ** 2
      t247 = log(0.4D1 * t130 * t140)
      t249 = t247 ** 2
      t253 = t17 * t120
      t257 = log(-0.4D1 * t10 * t14 * t253 * t32)
      t258 = t257 * z
      t260 = t257 ** 2
      t269 = log(-0.4D1 * t130 * t9 * t163)
      t271 = t269 ** 2
      t287 = t4 * t71
      t293 = t6 * t22
      t300 = t9 * t120
      t303 = log(-0.4D1 * t300 * t163)
      t304 = t303 ** 2
      t307 = log(0.4D1 * t300 * t30)
      t308 = t307 ** 2
      t331 = log(0.4D1 * t140)
      t332 = t331 ** 2
      t333 = t332 * pi
      t338 = t332 * t331 * pi
      t340 = t331 * pi
      t351 = t67 ** 2
      t352 = t69 ** 2
      t360 = t332 ** 2
      t367 = -(-0.90D2 * t4 * t6 * (-t21 * t22 + t24 * t25 / 0.2D1 + (-t
     #37 * t22 + t39 * z * t25 / 0.2D1) * t51) + 0.180D3 * t4 * t57 * (t
     #22 - t21 * t25 + (t59 - t37 * t25) * t51) + t4 * t72 * (t25 + z * 
     #t25 * t51)) * t79 * t81 / 0.1440D4 + (t4 * t72 * (-t22 + t86 * t25
     #) - 0.90D2 * t4 * t6 * (-t91 * t22 / 0.2D1 + t91 * t86 * t25 / 0.6
     #D1) - t4 * t107 * t25 + 0.180D3 * t4 * t57 * (t86 * t22 - t91 * t2
     #5 / 0.2D1)) * t81 / 0.1440D4 + (-0.90D2 * t4 * t6 * (-(t59 - t125 
     #* z * t25) * t51 - t138 * t25 + t143 * t25) - 0.180D3 * t149 * t15
     #2) * t79 * t158 / 0.720D3 - (-0.90D2 * t4 * t6 * (t166 * t22 - t16
     #8 * t25 / 0.2D1 - t173 * t22 + t175 * t25 / 0.2D1) + 0.180D3 * t4 
     #* t57 * (t166 * t25 - t173 * t25)) * t157 * t81 / 0.720D3 - ((0.18
     #0D3 * t192 * t193 + t196 * t197) * t6 * (-t203 * z * t51 - t208) -
     # 0.90D2 * t4 * t211 * (-t212 * t208 / 0.6D1 - t214 * t203 * z * t5
     #1 / 0.6D1) + (t192 * t197 + t196 * t3 * t106) * t6 * (z * t51 + 0.
     #1D1) + (0.180D3 * t196 * t193 - 0.90D2 * t192 * t3) * t6 * (t214 *
     # z * t51 / 0.2D1 + t212 / 0.2D1)) * t79 / 0.2880D4 + (-0.90D2 * t4
     # * t6 * (t247 * t22 - t249 * t25 / 0.2D1 - (-t258 * t22 + t260 * z
     # * t25 / 0.2D1) * t51 - t269 * t22 + t271 * t25 / 0.2D1) + 0.180D3
     # * t4 * t57 * (t247 * t25 - (t59 - t258 * t25) * t51 - t269 * t25)
     # - t287 * t152) * t79 * t157 / 0.1440D4 + ((-0.90D2 * t4 * t293 + 
     #0.180D3 * t4 * t57 * t25) * (t304 / 0.2D1 - t308 / 0.2D1) - 0.90D2
     # * t4 * t211 * (t308 * t307 / 0.6D1 - t304 * t303 / 0.6D1) + (0.18
     #0D3 * t4 * t57 * t22 + t4 * t72 * t25) * (-t303 + t307)) * t157 / 
     #0.1440D4 - (0.90D2 * t333 * lh + pi * t106 + 0.15D2 * t338 - t340 
     #* t71) * t3 * t293 / 0.2880D4 - (-0.30D2 * t338 * lh + t333 * t71 
     #/ 0.2D1 - t340 * t106 + pi * (-t351 - 0.60D2 * t352 - 0.480D3 * lh
     # * zeta3 + 0.60D2 * t69 * t67) - 0.15D2 / 0.4D1 * t360 * pi) * t3 
     #* t211 / 0.2880D4
      t368 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t367)
      t370 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t367)
      t372 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t367)
      t375 = x1 * z
      t376 = -z - x1 + t375
      t377 = 0.1D1 / t376
      t379 = t2 * x1 * t133 * t377
      t380 = -0.1D1 + x1
      t381 = t2 * t380
      t384 = x2 * s * t1 * x1
      t385 = s * t16
      t388 = t380 * x1 * t377
      t389 = t385 * t133 * t388
      t390 = t133 * t11
      t391 = 0.1D1 / t7
      t392 = t391 * x3
      t394 = t380 ** 2
      t400 = log(0.4D1 * t390 * t392 * t30 * t120 * t394 * t377)
      t401 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t384, t379, 
     #-t381, 0.0D0, -t389)
      t403 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t384, t379, 
     #-t381, 0.0D0, -t389)
      t416 = t17 * t377
      t421 = log(0.4D1 * t161 * t391 * t14 * t416 * t394 * t133)
      t423 = t421 ** 2
      t441 = (-0.90D2 * t4 * t6 * (t400 * t401 - t403) - 0.180D3 * t4 * 
     #t57 * t401) * t79 * t158 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t421 *
     # t403 + t423 * t401 / 0.2D1) + 0.180D3 * t4 * t57 * (t403 - t421 *
     # t401) + t4 * t72 * t401) * t157 * t81 / 0.720D3
      t442 = FJET(XB1, XB2, s, 0.0D0, t379, -t381, t384, -t389, t441)
      t444 = x2 * x3
      t445 = 0.1D1 - x3 + t444
      t446 = 0.1D1 / t445
      t447 = t444 * t446
      t448 = t2 * t447
      t450 = t2 * t31 * t446
      t454 = t445 ** 2
      t455 = 0.1D1 / t454
      t460 = log(0.4D1 * t133 * t9 * x3 * t14 * t253 * t31 * t455)
      t461 = t460 ** 2
      t463 = t133 * t31
      t465 = Sqrt(t45 * t463)
      t469 = 0.1D1 / (-z - x3 + t444 + 0.2D1 * t44 * t465)
      t470 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t450, t448, 0.0D0)
      t471 = t469 * t470
      t474 = t460 * z
      t475 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t450, t448, 0.0D0)
      t484 = z * t469 * t475
      t489 = t150 * t471
      t501 = log(0.4D1 * t390 * t10 * t30 * t120 * t31 * t455)
      t514 = (-0.90D2 * t4 * t6 * (t461 * z * t471 / 0.2D1 - t474 * t469
     # * t475) + 0.180D3 * t4 * t57 * (-t474 * t471 + t484) + t287 * t48
     #9) * t79 * t157 / 0.1440D4 + (-0.90D2 * t4 * t6 * (t484 - t501 * z
     # * t471) + 0.180D3 * t149 * t489) * t79 * t158 / 0.720D3
      t515 = FJET(XB1, XB2, s, 0.0D0, t448, 0.0D0, -t450, 0.0D0, t514)
      t518 = t2 * x1 * t377
      t519 = t385 * t388
      t521 = t377 * t394
      t522 = t30 * t521
      t525 = log(-0.4D1 * t392 * t11 * t522)
      t526 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t518
     #, -t381, 0.0D0, t519)
      t533 = log(0.4D1 * t392 * t15 * t416 * t394 * t32)
      t534 = t533 * z
      t537 = t533 ** 2
      t539 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t518
     #, -t381, 0.0D0, t519)
      t540 = t376 * t539
      t544 = x3 * x1
      t545 = t544 * z
      t546 = x1 * t7
      t547 = x3 * t7
      t548 = t547 * x1
      t549 = x3 * t11
      t551 = 0.2D1 * t549 * z
      t552 = t549 * t7
      t553 = x3 * t376
      t555 = Sqrt(t553 * t31)
      t560 = 0.1D1 / (-t375 - t545 + t546 + t548 - t45 + t551 - t552 - t
     #549 + 0.2D1 * t44 * t555 * z - t7)
      t562 = t525 ** 2
      t569 = z * t376
      t570 = t569 * t526
      t581 = -t539 + t569 * t539 * t560
      t588 = t391 * t11
      t593 = log(-0.4D1 * t588 * t14 * t416 * t394)
      t598 = t593 ** 2
      t620 = t130 * t588
      t625 = log(0.4D1 * t620 * t30 * t521 * t32)
      t632 = log(-0.4D1 * t620 * t522)
      t649 = log(-0.4D1 * t161 * t391 * t522)
      t651 = t649 ** 2
      t669 = -(-0.90D2 * t4 * t6 * (t525 * t526 + (-t534 * t376 * t526 +
     # t537 * z * t540 / 0.2D1) * t560 - t562 * t539 / 0.2D1) + 0.180D3 
     #* t4 * t57 * (-t526 + (t570 - t534 * t540) * t560 + t525 * t539) +
     # t4 * t72 * t581) * t79 * t81 / 0.1440D4 + (t4 * t72 * (t526 - t59
     #3 * t539) - 0.90D2 * t4 * t6 * (t598 * t526 / 0.2D1 - t598 * t593 
     #* t539 / 0.6D1) + t4 * t107 * t539 + 0.180D3 * t4 * t57 * (-t593 *
     # t526 + t598 * t539 / 0.2D1)) * t81 / 0.1440D4 + (-0.90D2 * t4 * t
     #6 * (-(t570 - t625 * z * t540) * t560 + t526 - t632 * t539) - 0.18
     #0D3 * t4 * t57 * t581) * t79 * t158 / 0.720D3 - (-0.90D2 * t4 * t6
     # * (t649 * t526 - t651 * t539 / 0.2D1) + 0.180D3 * t4 * t57 * (-t5
     #26 + t649 * t539) - t4 * t72 * t539) * t157 * t81 / 0.720D3
      t670 = FJET(XB1, XB2, s, 0.0D0, -t381, -t518, 0.0D0, t519, t669)
      t672 = FJET(XB1, XB2, s, 0.0D0, -t518, -t381, 0.0D0, t519, t669)
      t674 = FJET(XB1, XB2, s, 0.0D0, -t450, 0.0D0, t448, 0.0D0, t514)
      t676 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t367)
      t678 = FJET(XB1, XB2, s, t384, -t381, t379, 0.0D0, -t389, t441)
      t680 = FJET(XB1, XB2, s, t379, 0.0D0, t384, -t381, -t389, t441)
      t682 = FJET(XB1, XB2, s, t448, 0.0D0, -t450, 0.0D0, 0.0D0, t514)
      t687 = t31 * s * t1 * t380 * t446
      t688 = t2 * x1
      t690 = Sqrt(-t553 * t463)
      t691 = t44 * t690
      t697 = t688 * x2 * (-x3 + t444 - z + t45 - x1 + t544 + t375 - t545
     # + 0.2D1 * t691) * t377 * t446
      t698 = t381 * t447
      t699 = t130 * x1
      t701 = t130 * t375
      t707 = t688 * (t699 + t130 * z - x2 + t444 + 0.1D1 - x3 - t701 + 0
     #.2D1 * t691 * x2) * t377 * t446
      t708 = x2 * x1
      t709 = t708 * z
      t710 = -z - t708 + t709
      t713 = t7 * x2
      t715 = t11 * x2
      t721 = t545 - t548 - t551 + t552 + t7 + t375 + 0.2D1 * t691 * t709
     # - t699 + t709 - t713 * x1 - 0.2D1 * t715 * z + t11 * t7 * x2 - t4
     #44 * z
      t735 = t444 * x1 - t549 * x2 - 0.2D1 * t691 * z + t715 - t546 + t4
     #5 + t549 + t701 - 0.2D1 * t691 * t708 - 0.2D1 * t444 * t375 + t547
     # * t708 + 0.2D1 * t549 * x2 * z - t549 * t713
      t737 = 0.1D1 / (t721 + t735)
      t738 = t710 * t737
      t739 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t697, -t707,
     # t687, -t698, -t389)
      t749 = log(-0.4D1 * t390 * t392 * t14 * t253 * t394 * t31 * t377 *
     # t455)
      t751 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t697, -t707,
     # t687, -t698, -t389)
      t764 = -0.90D2 * t4 * t6 * (t738 * t739 - t749 * t710 * t737 * t75
     #1) * t376 + 0.180D3 * t4 * t57 * t738 * t751 * t376
      t767 = t764 * t79 * t158 / 0.720D3
      t768 = FJET(XB1, XB2, s, t687, t697, -t698, -t707, -t389, t767)
      t771 = t79 * t157 * t81
      t774 = FJET(XB1, XB2, s, t697, t687, -t707, -t698, -t389, t767)
      t778 = FJET(XB1, XB2, s, -t381, 0.0D0, 0.0D0, -t518, t519, t669)
      t780 = FJET(XB1, XB2, s, -t381, t384, 0.0D0, t379, -t389, t441)
      t782 = FJET(XB1, XB2, s, -t518, 0.0D0, 0.0D0, -t381, t519, t669)
      t784 = FJET(XB1, XB2, s, -t450, 0.0D0, t448, 0.0D0, 0.0D0, t514)
      t786 = FJET(XB1, XB2, s, -t707, -t698, t697, t687, -t389, t767)
      t790 = FJET(XB1, XB2, s, -t698, -t707, t687, t697, -t389, t767)
      rrqqbar2gght5s3e1 = t368 * t367 + t370 * t367 + t372 * t367 + t442
     # * t441 + t515 * t514 + t670 * t669 + t672 * t669 + t674 * t514 + 
     #t676 * t367 + t678 * t441 + t680 * t441 + t682 * t514 + t768 * t76
     #4 * t771 / 0.720D3 + t774 * t764 * t771 / 0.720D3 + t778 * t669 + 
     #t780 * t441 + t782 * t669 + t784 * t514 + t786 * t764 * t771 / 0.7
     #20D3 + t790 * t764 * t771 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght5s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x1 ** 2
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t22 = log(0.4D1 * t11 * t16 * t18)
      t23 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t25 = z * t7
      t26 = t12 * t10
      t28 = t15 * t18
      t29 = -0.1D1 + x3
      t30 = 0.1D1 / t29
      t31 = t28 * t30
      t34 = log(-0.4D1 * t26 * x3 * t31)
      t38 = cos(t13)
      t39 = x3 * z
      t41 = Sqrt(-t39 * t29)
      t45 = 0.1D1 / (-z - x3 + 0.2D1 * t38 * t41)
      t51 = lh * t6
      t59 = 0.1D1 / x3
      t61 = 0.1D1 / x1
      t64 = t6 * z
      t65 = t4 * t64
      t66 = t23 * t45
      t67 = 0.1D1 / x2
      t69 = t59 * t67 * t61
      t73 = t4 * t6
      t74 = x2 ** 2
      t75 = t74 * t12
      t77 = -0.1D1 + x2
      t78 = t28 * t77
      t81 = log(-0.4D1 * t75 * t10 * t78)
      t84 = t10 * t15 * t18
      t87 = log(0.4D1 * t75 * t84)
      t96 = log(0.4D1 * t26 * t28)
      t98 = t96 ** 2
      t110 = pi ** 2
      t112 = lh ** 2
      t114 = 0.30D2 * t110 - 0.180D3 * t112
      t115 = t114 * t6
      t121 = t6 * t23
      t124 = log(-0.4D1 * t11 * t31)
      t125 = t124 ** 2
      t130 = log(0.4D1 * t11 * t28)
      t131 = t130 ** 2
      t137 = pi * t23
      t138 = t3 * lh
      t141 = pi * t7
      t163 = log(0.4D1 * t84)
      t164 = t163 * pi
      t167 = t163 ** 2
      t168 = t167 * pi
      t173 = t6 * t7
      t193 = x3 * t74
      t196 = log(0.4D1 * t193 * t84)
      t199 = t18 * t74
      t203 = log(-0.4D1 * t11 * t15 * t199 * t30)
      t211 = log(-0.4D1 * t193 * t10 * t78)
      t217 = t4 * lh
      t225 = t10 * t74
      t228 = log(-0.4D1 * t225 * t78)
      t229 = t228 ** 2
      t232 = log(0.4D1 * t225 * t28)
      t233 = t232 ** 2
      t250 = -(-0.90D2 * t4 * t6 * (t7 - t22 * t23 + (t25 - t34 * z * t2
     #3) * t45) + 0.180D3 * t4 * t51 * (t23 + z * t23 * t45)) * t59 * t6
     #1 / 0.1440D4 + t65 * t66 * t69 / 0.8D1 + t73 * (t81 * t23 - t87 * 
     #t23) * t67 * t61 / 0.8D1 + (-0.90D2 * t4 * t6 * (t96 * t7 - t98 * 
     #t23 / 0.2D1) + 0.180D3 * t4 * t51 * (-t7 + t96 * t23) - t4 * t115 
     #* t23) * t61 / 0.1440D4 - (-0.90D2 * t4 * t121 * (t125 * z * t45 /
     # 0.2D1 + t131 / 0.2D1) + (0.180D3 * t137 * t138 - 0.90D2 * t141 * 
     #t3) * t6 * (-t124 * z * t45 - t130) + (0.180D3 * t141 * t138 + t13
     #7 * t3 * t114) * t6 * (z * t45 + 0.1D1)) * t59 / 0.2880D4 - (-0.18
     #0D3 * t164 * lh - 0.45D2 * t168 + pi * t114) * t3 * t173 / 0.2880D
     #4 - (0.90D2 * t168 * lh + pi * (0.240D3 * zeta3 + 0.120D3 * t112 *
     # lh - 0.60D2 * lh * t110) + 0.15D2 * t167 * t163 * pi - t164 * t11
     #4) * t3 * t121 / 0.2880D4 + (-0.90D2 * t4 * t6 * (t196 * t23 - (t2
     #5 - t203 * z * t23) * t45 - t211 * t23) - 0.180D3 * t217 * t64 * t
     #66) * t59 * t67 / 0.1440D4 + (-0.90D2 * t4 * t121 * (t229 / 0.2D1 
     #- t233 / 0.2D1) + (-0.90D2 * t4 * t173 + 0.180D3 * t4 * t51 * t23)
     # * (-t228 + t232)) * t67 / 0.1440D4
      t251 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t250)
      t253 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t250)
      t255 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t250)
      t258 = x1 * z
      t259 = -z - x1 + t258
      t260 = 0.1D1 / t259
      t262 = t2 * x1 * t77 * t260
      t263 = -0.1D1 + x1
      t264 = t2 * t263
      t267 = x2 * s * t1 * x1
      t268 = s * t17
      t271 = t263 * x1 * t260
      t272 = t268 * t77 * t271
      t273 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t267, t262, 
     #-t264, 0.0D0, -t272)
      t275 = t67 * t61
      t279 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t267, t262, 
     #-t264, 0.0D0, -t272)
      t280 = 0.1D1 / t8
      t283 = t18 * t260
      t284 = t263 ** 2
      t289 = log(0.4D1 * t75 * t280 * t15 * t283 * t284 * t77)
      t302 = t73 * t273 * t59 * t275 / 0.8D1 - (-0.90D2 * t4 * t6 * (t27
     #9 - t289 * t273) + 0.180D3 * t4 * t51 * t273) * t67 * t61 / 0.720D
     #3
      t303 = FJET(XB1, XB2, s, 0.0D0, t262, -t264, t267, -t272, t302)
      t305 = x2 * x3
      t306 = 0.1D1 - x3 + t305
      t307 = 0.1D1 / t306
      t308 = t305 * t307
      t309 = t2 * t308
      t311 = t2 * t29 * t307
      t312 = t77 * t29
      t314 = Sqrt(t39 * t312)
      t318 = 0.1D1 / (-z - x3 + t305 + 0.2D1 * t38 * t314)
      t319 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t311, t309, 0.0D0)
      t320 = t318 * t319
      t327 = t306 ** 2
      t333 = log(0.4D1 * t77 * t10 * x3 * t15 * t199 * t29 / t327)
      t337 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t311, t309, 0.0D0)
      t350 = -t65 * t320 * t69 / 0.8D1 + (-0.90D2 * t4 * t6 * (-t333 * z
     # * t320 + z * t318 * t337) + 0.180D3 * t217 * t64 * t320) * t59 * 
     #t67 / 0.1440D4
      t351 = FJET(XB1, XB2, s, 0.0D0, t309, 0.0D0, -t311, 0.0D0, t350)
      t354 = t2 * x1 * t260
      t355 = t268 * t271
      t356 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t354
     #, -t264, 0.0D0, t355)
      t357 = z * t259
      t359 = x3 * t280
      t365 = log(0.4D1 * t359 * t16 * t283 * t284 * t30)
      t367 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t354
     #, -t264, 0.0D0, t355)
      t371 = x3 * x1
      t372 = t371 * z
      t373 = x1 * t8
      t374 = x3 * t8
      t375 = t374 * x1
      t376 = x3 * t12
      t378 = 0.2D1 * z * t376
      t379 = t376 * t8
      t380 = x3 * t259
      t382 = Sqrt(t380 * t29)
      t387 = 0.1D1 / (-t258 - t372 + t373 + t375 - t39 + t378 - t379 - t
     #376 + 0.2D1 * t38 * t382 * z - t8)
      t391 = t28 * t260 * t284
      t394 = log(-0.4D1 * t359 * t12 * t391)
      t402 = -t367 + t357 * t367 * t387
      t418 = log(-0.4D1 * t75 * t280 * t391)
      t436 = log(-0.4D1 * t280 * t12 * t15 * t283 * t284)
      t438 = t436 ** 2
      t455 = -(-0.90D2 * t4 * t6 * (-t356 + (t357 * t356 - t365 * z * t2
     #59 * t367) * t387 + t394 * t367) + 0.180D3 * t4 * t51 * t402) * t5
     #9 * t61 / 0.1440D4 + t73 * t402 * t59 * t275 / 0.8D1 - (-0.90D2 * 
     #t4 * t6 * (-t356 + t418 * t367) - 0.180D3 * t4 * t51 * t367) * t67
     # * t61 / 0.720D3 + (-0.90D2 * t4 * t6 * (-t436 * t356 + t438 * t36
     #7 / 0.2D1) + 0.180D3 * t4 * t51 * (t356 - t436 * t367) + t4 * t115
     # * t367) * t61 / 0.1440D4
      t456 = FJET(XB1, XB2, s, 0.0D0, -t264, -t354, 0.0D0, t355, t455)
      t458 = FJET(XB1, XB2, s, 0.0D0, -t354, -t264, 0.0D0, t355, t455)
      t460 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t309, 0.0D0, t350)
      t462 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t250)
      t464 = FJET(XB1, XB2, s, t267, -t264, t262, 0.0D0, -t272, t302)
      t466 = FJET(XB1, XB2, s, t262, 0.0D0, t267, -t264, -t272, t302)
      t468 = FJET(XB1, XB2, s, t309, 0.0D0, -t311, 0.0D0, 0.0D0, t350)
      t473 = t29 * s * t1 * t263 * t307
      t474 = t2 * x1
      t476 = Sqrt(-t380 * t312)
      t477 = t38 * t476
      t483 = t474 * x2 * (-x3 + t305 - z + t39 - x1 + t371 + t258 - t372
     # + 0.2D1 * t477) * t260 * t307
      t484 = t264 * t308
      t485 = t193 * x1
      t487 = t193 * t258
      t493 = t474 * (t485 + t193 * z - x2 + t305 + 0.1D1 - x3 - t487 + 0
     #.2D1 * t477 * x2) * t260 * t307
      t494 = x2 * x1
      t495 = t494 * z
      t496 = -z - t494 + t495
      t498 = t12 * x2
      t509 = t8 * x2
      t511 = t498 + t372 - t375 - t378 + t379 + 0.2D1 * t477 * t495 + t2
     #58 + t487 - 0.2D1 * t477 * t494 - 0.2D1 * t305 * t258 + t374 * t49
     #4 + 0.2D1 * t376 * x2 * z - t376 * t509
      t522 = t8 - t485 + t495 - t509 * x1 - 0.2D1 * t498 * z + t12 * t8 
     #* x2 - t305 * z + t305 * x1 - t376 * x2 - 0.2D1 * t477 * z - t373 
     #+ t39 + t376
      t524 = 0.1D1 / (t511 + t522)
      t527 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t483, -t493,
     # t473, -t484, -t272)
      t531 = t4 * t6 * t496 * t524 * t527 * t259 * t69 / 0.8D1
      t532 = FJET(XB1, XB2, s, t473, t483, -t484, -t493, -t272, -t531)
      t535 = t3 * t6 * t496
      t539 = t524 * t527 * t259 * t69
      t542 = FJET(XB1, XB2, s, t483, t473, -t493, -t484, -t272, -t531)
      t547 = FJET(XB1, XB2, s, -t264, 0.0D0, 0.0D0, -t354, t355, t455)
      t549 = FJET(XB1, XB2, s, -t264, t267, 0.0D0, t262, -t272, t302)
      t551 = FJET(XB1, XB2, s, -t354, 0.0D0, 0.0D0, -t264, t355, t455)
      t553 = FJET(XB1, XB2, s, -t311, 0.0D0, t309, 0.0D0, 0.0D0, t350)
      t555 = FJET(XB1, XB2, s, -t493, -t484, t483, t473, -t272, -t531)
      t560 = FJET(XB1, XB2, s, -t484, -t493, t473, t483, -t272, -t531)
      rrqqbar2gght5s3e0 = t251 * t250 + t253 * t250 + t255 * t250 + t303
     # * t302 + t351 * t350 + t456 * t455 + t458 * t455 + t460 * t350 + 
     #t462 * t250 + t464 * t302 + t466 * t302 + t468 * t350 - t532 * pi 
     #* t535 * t539 / 0.8D1 - t542 * pi * t535 * t539 / 0.8D1 + t547 * t
     #455 + t549 * t302 + t551 * t455 + t553 * t350 - t555 * pi * t535 *
     # t539 / 0.8D1 - t560 * pi * t535 * t539 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght5s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t24 = log(-0.4D1 * t12 * t18 / t19)
      t26 = cos(t13)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t19)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t12 * t18)
      t46 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t56 = 0.1D1 / x3
      t59 = x1 ** 2
      t63 = log(0.4D1 * t11 * t59 * t18)
      t69 = lh * t6
      t74 = 0.1D1 / x1
      t77 = t4 * t6
      t86 = t4 * t6 * z
      t88 = 0.1D1 / x2
      t89 = t56 * t88
      t93 = x2 ** 2
      t94 = t11 * t93
      t95 = -0.1D1 + x2
      t99 = log(-0.4D1 * t94 * t18 * t95)
      t102 = log(0.4D1 * t94 * t18)
      t113 = log(0.4D1 * t11 * t15 * t17)
      t114 = t113 * pi
      t123 = t113 ** 2
      t126 = pi ** 2
      t128 = lh ** 2
      t136 = -(-0.90D2 * t4 * t8 * (-t24 * z * t33 - t37) + (0.180D3 * p
     #i * t7 * t3 * lh - 0.90D2 * pi * t46 * t3) * t6 * (z * t33 + 0.1D1
     #)) * t56 / 0.2880D4 + (-0.90D2 * t4 * t6 * (-t46 + t63 * t7) - 0.1
     #80D3 * t4 * t69 * t7) * t74 / 0.1440D4 + t77 * (t7 + z * t7 * t33)
     # * t56 * t74 / 0.16D2 + t86 * t7 * t33 * t89 / 0.16D2 - t77 * t7 *
     # (-t99 + t102) * t88 / 0.16D2 - (0.180D3 * pi * lh + 0.90D2 * t114
     #) * t3 * t6 * t46 / 0.2880D4 - (-0.180D3 * t114 * lh - 0.45D2 * t1
     #23 * pi + pi * (0.30D2 * t126 - 0.180D3 * t128)) * t3 * t8 / 0.288
     #0D4
      t137 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t136)
      t139 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t136)
      t141 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t136)
      t144 = x1 * z
      t145 = -z - x1 + t144
      t146 = 0.1D1 / t145
      t148 = t2 * x1 * t95 * t146
      t149 = -0.1D1 + x1
      t150 = t2 * t149
      t153 = x2 * s * t1 * x1
      t154 = s * t16
      t157 = t149 * x1 * t146
      t158 = t154 * t95 * t157
      t159 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t153, t148, 
     #-t150, 0.0D0, -t158)
      t163 = t77 * t159 * t88 * t74 / 0.8D1
      t164 = FJET(XB1, XB2, s, 0.0D0, t148, -t150, t153, -t158, t163)
      t169 = t6 * t159 * t88 * t74
      t172 = x2 * x3
      t174 = 0.1D1 / (0.1D1 - x3 + t172)
      t176 = t2 * t172 * t174
      t178 = t2 * t19 * t174
      t181 = Sqrt(t27 * t95 * t19)
      t185 = 0.1D1 / (-z - x3 + t172 + 0.2D1 * t26 * t181)
      t186 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t178, t176, 0.0D0)
      t190 = t86 * t185 * t186 * t89 / 0.16D2
      t191 = FJET(XB1, XB2, s, 0.0D0, t176, 0.0D0, -t178, 0.0D0, -t190)
      t193 = t3 * t6
      t198 = z * t185 * t186 * t56 * t88
      t202 = t2 * x1 * t146
      t203 = t154 * t157
      t204 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t202
     #, -t150, 0.0D0, t203)
      t209 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t202
     #, -t150, 0.0D0, t203)
      t214 = t149 ** 2
      t218 = log(-0.4D1 / t9 * t59 * t15 * t17 * t146 * t214)
      t236 = x3 * t59
      t242 = Sqrt(x3 * t145 * t19)
      t255 = -t77 * t204 * t88 * t74 / 0.8D1 + (-0.90D2 * t4 * t6 * (t20
     #9 - t218 * t204) + 0.180D3 * t4 * t69 * t204) * t74 / 0.1440D4 + t
     #77 * (-t204 + z * t145 * t204 / (-t144 - x3 * x1 * z + x1 * t9 + x
     #3 * t9 * x1 - t27 + 0.2D1 * t236 * z - t236 * t9 - t236 + 0.2D1 * 
     #t26 * t242 * z - t9)) * t56 * t74 / 0.16D2
      t256 = FJET(XB1, XB2, s, 0.0D0, -t150, -t202, 0.0D0, t203, t255)
      t258 = FJET(XB1, XB2, s, 0.0D0, -t202, -t150, 0.0D0, t203, t255)
      t260 = FJET(XB1, XB2, s, 0.0D0, -t178, 0.0D0, t176, 0.0D0, -t190)
      t265 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t136)
      t267 = FJET(XB1, XB2, s, t153, -t150, t148, 0.0D0, -t158, t163)
      t272 = FJET(XB1, XB2, s, t148, 0.0D0, t153, -t150, -t158, t163)
      t277 = FJET(XB1, XB2, s, t176, 0.0D0, -t178, 0.0D0, 0.0D0, -t190)
      t282 = FJET(XB1, XB2, s, -t150, 0.0D0, 0.0D0, -t202, t203, t255)
      t284 = FJET(XB1, XB2, s, -t150, t153, 0.0D0, t148, -t158, t163)
      t289 = FJET(XB1, XB2, s, -t202, 0.0D0, 0.0D0, -t150, t203, t255)
      t291 = FJET(XB1, XB2, s, -t178, 0.0D0, t176, 0.0D0, 0.0D0, -t190)
      rrqqbar2gght5s3em1 = t137 * t136 + t139 * t136 + t141 * t136 + t16
     #4 * pi * t3 * t169 / 0.8D1 - t191 * pi * t193 * t198 / 0.16D2 + t2
     #56 * t255 + t258 * t255 - t260 * pi * t193 * t198 / 0.16D2 + t265 
     #* t136 + t267 * pi * t3 * t169 / 0.8D1 + t272 * pi * t3 * t169 / 0
     #.8D1 - t277 * pi * t193 * t198 / 0.16D2 + t282 * t255 + t284 * pi 
     #* t3 * t169 / 0.8D1 + t289 * t255 - t291 * pi * t193 * t198 / 0.16
     #D2

      end function



      doubleprecision function rrqqbar2gght5s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x1
      t14 = x4 * pi
      t15 = cos(t14)
      t19 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t31 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t37 = z ** 2
      t40 = Sin(t14)
      t41 = t40 ** 2
      t43 = t1 ** 2
      t44 = t43 ** 2
      t47 = log(0.4D1 / t37 / z * t41 * t44)
      t54 = t4 * t8 * t9 / 0.16D2 + t4 * t6 * t7 * (z / (-z - x3 + 0.2D1
     # * t15 * t19) + 0.1D1) / x3 / 0.32D2 + t4 * t6 * t31 / 0.32D2 - (0
     #.180D3 * pi * lh + 0.90D2 * t47 * pi) * t3 * t8 / 0.2880D4
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t54)
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t54)
      t59 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t54)
      t61 = -0.1D1 + x1
      t62 = t2 * t61
      t65 = 0.1D1 / (-z - x1 + x1 * z)
      t67 = t2 * x1 * t65
      t71 = s * t43 * x1 * t61 * t65
      t72 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t67, 
     #-t62, 0.0D0, t71)
      t74 = t6 * t72 * t9
      t76 = t4 * t74 / 0.16D2
      t77 = FJET(XB1, XB2, s, 0.0D0, -t62, -t67, 0.0D0, t71, -t76)
      t82 = FJET(XB1, XB2, s, 0.0D0, -t67, -t62, 0.0D0, t71, -t76)
      t87 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t54)
      t89 = FJET(XB1, XB2, s, -t62, 0.0D0, 0.0D0, -t67, t71, -t76)
      t94 = FJET(XB1, XB2, s, -t67, 0.0D0, 0.0D0, -t62, t71, -t76)
      rrqqbar2gght5s3em2 = t55 * t54 + t57 * t54 + t59 * t54 - t77 * pi 
     #* t3 * t74 / 0.16D2 - t82 * pi * t3 * t74 / 0.16D2 + t87 * t54 - t
     #89 * pi * t3 * t74 / 0.16D2 - t94 * pi * t3 * t74 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t10 = pi * t3 * t6 * t7 / 0.32D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = t3 * t6 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s3em3 = t11 * pi * t14 / 0.32D2 + t16 * pi * t14 / 0.
     #32D2 + t19 * pi * t14 / 0.32D2 + t22 * pi * t14 / 0.32D2

      end function



      doubleprecision function rrqqbar2gght5s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght5s3em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght5s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = -0.1D1 + x2
      t7 = x2 * x3
      t8 = -0.1D1 + t7
      t9 = 0.1D1 / t8
      t10 = x3 * t5 * t9
      t11 = t4 * t10
      t12 = t2 * x1
      t13 = x3 * z
      t14 = x3 * x1
      t15 = t14 * z
      t16 = t7 * z
      t18 = t7 * x1
      t20 = x2 ** 2
      t21 = t20 * x3
      t22 = t21 * x1
      t24 = x4 * pi
      t25 = cos(t24)
      t26 = x1 * z
      t27 = -z - x1 + t26
      t29 = -0.1D1 + x3
      t30 = x2 * t29
      t32 = Sqrt(x3 * t27 * t30)
      t33 = t25 * t32
      t38 = t21 * t26
      t39 = 0.2D1 * t33
      t40 = -t13 - t14 + t15 + 0.2D1 * t16 + 0.2D1 * t18 - t22 - t21 * z
     # + t7 - x2 + 0.2D1 * t33 * x2 - 0.2D1 * t7 * t26 + t38 - t39
      t41 = 0.1D1 / t27
      t44 = t12 * t40 * t41 * t9
      t45 = t29 * s
      t46 = t1 * t3
      t48 = t45 * t46 * t9
      t53 = t12 * t5 * (-t7 - z + t13 - x1 + t14 + t26 - t15 + t39) * t4
     #1 * t9
      t54 = t1 ** 2
      t59 = s * t54 * x2 * x1 * t3 * t41
      t60 = 0.1D1 / t1
      t61 = pi * t60
      t62 = s ** 2
      t63 = 0.1D1 / t62
      t64 = t61 * t63
      t65 = x2 * x1
      t66 = t65 * z
      t67 = z + x1 - t26 - t65 + t66
      t68 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t53, -t44, -t
     #48, -t11, t59)
      t70 = x1 ** 2
      t71 = z ** 2
      t72 = 0.1D1 / t71
      t73 = t70 * t72
      t74 = Sin(t24)
      t75 = t74 ** 2
      t78 = t5 ** 2
      t79 = t54 ** 2
      t80 = t78 * t79
      t81 = t8 ** 2
      t82 = 0.1D1 / t81
      t84 = t3 ** 2
      t90 = log(0.4D1 * t73 * t7 * t75 * t80 * t82 * t84 * t29 * t41)
      t92 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t53, -t44, -t
     #48, -t11, t59)
      t95 = t71 * x2
      t97 = t70 * x2
      t100 = t70 * t71
      t106 = x3 * t70
      t107 = t106 * x2
      t109 = -t95 * x1 - 0.2D1 * t97 * z + t100 * x2 + 0.2D1 * x1 * t71 
     #+ 0.2D1 * t70 * z - t100 + t66 - t16 - t18 - t107 + t22 - 0.2D1 * 
     #t26
      t126 = -0.2D1 * t33 * t65 - 0.2D1 * t33 * t26 - t71 + 0.2D1 * t33 
     #* z + 0.2D1 * t33 * x1 - t70 + 0.2D1 * t33 * t66 + x3 * t71 * t65 
     #+ 0.2D1 * t106 * x2 * z - t106 * t95 - t38 + t97
      t128 = 0.1D1 / (t109 + t126)
      t133 = lh * t63
      t140 = -0.90D2 * t64 * (t67 * t68 - t90 * t67 * t92) * t128 * t27 
     #+ 0.180D3 * t61 * t133 * t67 * t92 * t128 * t27
      t141 = 0.1D1 / x3
      t143 = 0.1D1 / x2
      t144 = 0.1D1 / x1
      t145 = t143 * t144
      t147 = t140 * t141 * t145 / 0.720D3
      t148 = FJET(XB1, XB2, s, -t11, -t44, -t48, t53, t59, t147)
      t151 = t141 * t143 * t144
      t154 = FJET(XB1, XB2, s, t53, -t48, -t44, -t11, t59, t147)
      t158 = t2 * x3
      t159 = t2 * t29
      t161 = 0.1D1 / t71 / z
      t163 = t75 * t79
      t164 = t163 * t29
      t167 = log(-0.4D1 * t106 * t161 * t164)
      t168 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t159, t158, 0.0D0)
      t170 = t167 ** 2
      t171 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t159, t158, 0.0D0)
      t183 = pi ** 2
      t185 = lh ** 2
      t187 = 0.30D2 * t183 - 0.180D3 * t185
      t188 = t187 * t63
      t190 = t61 * t188 * t171
      t195 = t161 * t75
      t200 = log(-0.4D1 * t107 * t195 * t79 * t29)
      t213 = x3 * t161
      t216 = log(-0.4D1 * t213 * t164)
      t221 = t216 ** 2
      t236 = 0.240D3 * zeta3 + 0.120D3 * t185 * lh - 0.60D2 * lh * t183
      t237 = t236 * t63
      t250 = t7 * t161
      t253 = log(-0.4D1 * t250 * t164)
      t255 = t253 ** 2
      t271 = (-0.90D2 * t61 * t63 * (-t167 * t168 + t170 * t171 / 0.2D1)
     # + 0.180D3 * t61 * t133 * (t168 - t167 * t171) + t190) * t141 * t1
     #44 / 0.720D3 + (-0.90D2 * t61 * t63 * (t168 - t200 * t171) + 0.180
     #D3 * t61 * t133 * t171) * t141 * t145 / 0.720D3 + (t61 * t188 * (t
     #168 - t216 * t171) - 0.90D2 * t61 * t63 * (t221 * t168 / 0.2D1 - t
     #221 * t216 * t171 / 0.6D1) + t61 * t237 * t171 + 0.180D3 * t61 * t
     #133 * (-t216 * t168 + t221 * t171 / 0.2D1)) * t141 / 0.1440D4 + (-
     #0.90D2 * t61 * t63 * (-t253 * t168 + t255 * t171 / 0.2D1) + 0.180D
     #3 * t61 * t133 * (t168 - t253 * t171) + t190) * t141 * t143 / 0.14
     #40D4
      t272 = FJET(XB1, XB2, s, 0.0D0, t158, 0.0D0, -t159, 0.0D0, t271)
      t276 = t41 * t84
      t277 = t163 * t276
      t280 = log(-0.4D1 * x3 * t72 * t70 * t277)
      t281 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t12, 0.0D0, 
     #-t4, 0.0D0, 0.0D0)
      t283 = t280 ** 2
      t284 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t12, 0.0D0, 
     #-t4, 0.0D0, 0.0D0)
      t297 = t61 * t188 * t284
      t300 = (-0.90D2 * t61 * t63 * (-t280 * t281 + t283 * t284 / 0.2D1)
     # + 0.180D3 * t61 * t133 * (t281 - t280 * t284) + t297) * t141 * t1
     #44
      t302 = t79 * t41
      t306 = log(-0.4D1 * t73 * t75 * t302 * t84)
      t308 = t281 - t306 * t284
      t311 = t306 ** 2
      t317 = t311 * t281 / 0.2D1 - t311 * t306 * t284 / 0.6D1
      t322 = t61 * t237 * t284
      t326 = -t306 * t281 + t311 * t284 / 0.2D1
      t332 = t7 * t73
      t335 = log(-0.4D1 * t332 * t277)
      t346 = (-0.90D2 * t61 * t63 * (t281 - t335 * t284) + 0.180D3 * t61
     # * t133 * t284) * t141 * t145
      t350 = log(-0.4D1 * t97 * t72 * t277)
      t352 = t350 ** 2
      t366 = (-0.90D2 * t61 * t63 * (-t350 * t281 + t352 * t284 / 0.2D1)
     # + 0.180D3 * t61 * t133 * (t281 - t350 * t284) + t297) * t143 * t1
     #44
      t368 = t300 / 0.720D3 - (-t61 * t188 * t308 + 0.90D2 * t61 * t63 *
     # t317 - t322 - 0.180D3 * t61 * t133 * t326) * t144 / 0.720D3 + t34
     #6 / 0.720D3 + t366 / 0.720D3
      t369 = FJET(XB1, XB2, s, -t4, t12, 0.0D0, 0.0D0, 0.0D0, t368)
      t371 = t45 * t46
      t373 = t45 * t1 * x1
      t375 = x3 * s * t46
      t376 = t2 * t14
      t385 = log(0.4D1 * t41 * t75 * t72 * t79 * t70 * t84 * x3 * t29)
      t386 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, t376,
     # t371, -t375, 0.0D0)
      t388 = t385 ** 2
      t389 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, t376,
     # t371, -t375, 0.0D0)
      t407 = t75 * t72
      t415 = log(0.4D1 * x2 * t41 * t407 * t79 * t70 * t84 * x3 * t29)
      t428 = (-0.90D2 * t61 * t63 * (t385 * t386 - t388 * t389 / 0.2D1) 
     #+ 0.180D3 * t61 * t133 * (-t386 + t385 * t389) - t61 * t188 * t389
     #) * t141 * t144 / 0.720D3 + (-0.90D2 * t61 * t63 * (-t386 + t415 *
     # t389) - 0.180D3 * t61 * t133 * t389) * t141 * t145 / 0.720D3
      t429 = FJET(XB1, XB2, s, t371, -t373, -t375, t376, 0.0D0, t428)
      t431 = t2 * t10
      t433 = t2 * t29 * t9
      t440 = log(-0.4D1 * t213 * x2 * t75 * t80 * t82 * t29)
      t441 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t433, t431, 0.0D0)
      t443 = t440 ** 2
      t444 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t433, t431, 0.0D0)
      t450 = Sqrt(-t13 * t30)
      t454 = 0.1D1 / (-z - t7 + 0.2D1 * t25 * t450)
      t458 = t61 * lh
      t462 = z * t454
      t468 = t63 * t444 * t462
      t474 = t70 * t161
      t482 = log(-0.4D1 * t474 * t7 * t75 * t78 * t79 * t82 * t29)
      t495 = (-0.90D2 * t64 * (-t440 * t441 + t443 * t444 / 0.2D1) * z *
     # t454 + 0.180D3 * t458 * t63 * (t441 - t440 * t444) * t462 + t61 *
     # t187 * t468) * t141 * t143 / 0.1440D4 + (-0.90D2 * t64 * (t441 - 
     #t482 * t444) * z * t454 + 0.180D3 * t458 * t468) * t141 * t145 / 0
     #.720D3
      t496 = FJET(XB1, XB2, s, 0.0D0, t431, 0.0D0, t433, 0.0D0, t495)
      t499 = t2 * x1 * t5
      t501 = t2 * t65 * t41
      t502 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t499, -t501
     #, -t4, 0.0D0, t59)
      t507 = log(-0.4D1 * t332 * t163 * t276 * t78)
      t508 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t499, -t501
     #, -t4, 0.0D0, t59)
      t519 = (-0.90D2 * t61 * t63 * (-t502 + t507 * t508) - 0.180D3 * t6
     #1 * t133 * t508) * t141 * t145
      t525 = log(-0.4D1 * t97 * t407 * t302 * t84 * t78)
      t527 = t525 ** 2
      t530 = -t525 * t502 + t527 * t508 / 0.2D1
      t535 = t502 - t525 * t508
      t540 = t61 * t188 * t508
      t545 = t519 / 0.720D3 + (0.90D2 * t61 * t63 * t530 - 0.180D3 * t61
     # * t133 * t535 - t540) * t143 * t144 / 0.720D3
      t546 = FJET(XB1, XB2, s, -t499, -t4, -t501, 0.0D0, t59, t545)
      t548 = FJET(XB1, XB2, s, 0.0D0, t433, 0.0D0, t431, 0.0D0, t495)
      t550 = FJET(XB1, XB2, s, t433, 0.0D0, t431, 0.0D0, 0.0D0, t495)
      t552 = FJET(XB1, XB2, s, -t48, t53, -t11, -t44, t59, t147)
      t556 = FJET(XB1, XB2, s, -t44, -t11, t53, -t48, t59, t147)
      t564 = log(0.4D1 * t213 * t70 * t75 * t79)
      t565 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t567 = t564 ** 2
      t568 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t2, 0.0D0, 0.0D0)
      t588 = log(0.4D1 * t474 * t163)
      t593 = t588 ** 2
      t604 = t61 * t237 * t568
      t618 = log(0.4D1 * t107 * t195 * t80)
      t620 = t195 * t79
      t623 = log(0.4D1 * t107 * t620)
      t631 = t163 * t78
      t634 = log(0.4D1 * t97 * t161 * t631)
      t636 = t634 ** 2
      t641 = log(0.4D1 * t97 * t620)
      t643 = t641 ** 2
      t662 = log(0.4D1 * t213 * t163)
      t667 = t662 ** 2
      t689 = log(0.4D1 * t7 * t620)
      t691 = t689 ** 2
      t696 = log(0.4D1 * t250 * t631)
      t698 = t696 ** 2
      t715 = t568 * pi
      t718 = t565 * pi
      t722 = x2 * t161
      t725 = log(0.4D1 * t722 * t163)
      t726 = t725 ** 2
      t729 = log(0.4D1 * t722 * t631)
      t730 = t729 ** 2
      t755 = log(0.4D1 * t620)
      t756 = t755 ** 2
      t759 = t756 * t755
      t776 = t183 ** 2
      t777 = t185 ** 2
      t787 = t756 ** 2
      t797 = (-0.90D2 * t61 * t63 * (t564 * t565 - t567 * t568 / 0.2D1) 
     #+ 0.180D3 * t61 * t133 * (-t565 + t564 * t568) - t61 * t188 * t568
     #) * t141 * t144 / 0.720D3 - (t61 * t188 * (t565 - t588 * t568) - 0
     #.90D2 * t61 * t63 * (t593 * t565 / 0.2D1 - t593 * t588 * t568 / 0.
     #6D1) + t604 + 0.180D3 * t61 * t133 * (-t588 * t565 + t593 * t568 /
     # 0.2D1)) * t144 / 0.720D3 - t64 * (-t618 * t568 + t623 * t568) * t
     #141 * t145 / 0.8D1 + (-0.90D2 * t61 * t63 * (-t634 * t565 + t636 *
     # t568 / 0.2D1 + t641 * t565 - t643 * t568 / 0.2D1) + 0.180D3 * t61
     # * t133 * (-t634 * t568 + t641 * t568)) * t143 * t144 / 0.720D3 + 
     #(t61 * t188 * (-t565 + t662 * t568) - 0.90D2 * t61 * t63 * (-t667 
     #* t565 / 0.2D1 + t667 * t662 * t568 / 0.6D1) - t604 + 0.180D3 * t6
     #1 * t133 * (t662 * t565 - t667 * t568 / 0.2D1)) * t141 / 0.1440D4 
     #+ (-0.90D2 * t61 * t63 * (t689 * t565 - t691 * t568 / 0.2D1 - t696
     # * t565 + t698 * t568 / 0.2D1) + 0.180D3 * t61 * t133 * (t689 * t5
     #68 - t696 * t568)) * t141 * t143 / 0.1440D4 - ((0.180D3 * t715 * l
     #h - 0.90D2 * t718) * t60 * t63 * (t726 / 0.2D1 - t730 / 0.2D1) - 0
     #.90D2 * t61 * t63 * t568 * (t730 * t729 / 0.6D1 - t726 * t725 / 0.
     #6D1) + (0.180D3 * t718 * lh + t715 * t187) * t60 * t63 * (-t725 + 
     #t729)) * t143 / 0.1440D4 - (0.180D3 * (t756 * t565 / 0.2D1 - t759 
     #* t568 / 0.6D1) * pi * lh + (-t755 * t565 + t756 * t568 / 0.2D1) *
     # pi * t187 + (t565 - t755 * t568) * pi * t236 + t715 * (-t776 - 0.
     #60D2 * t777 - 0.480D3 * lh * zeta3 + 0.60D2 * t185 * t183) - 0.90D
     #2 * (-t759 * t565 / 0.6D1 + t787 * t568 / 0.24D2) * pi) * t60 * t6
     #3 / 0.1440D4
      t798 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t797)
      t800 = FJET(XB1, XB2, s, t12, -t4, 0.0D0, 0.0D0, 0.0D0, t368)
      t802 = FJET(XB1, XB2, s, t431, 0.0D0, t433, 0.0D0, 0.0D0, t495)
      t804 = t148 * t140 * t151 / 0.720D3 + t154 * t140 * t151 / 0.720D3
     # + t272 * t271 + t369 * t368 + t429 * t428 + t496 * t495 + t546 * 
     #t545 + t548 * t495 + t550 * t495 + t552 * t140 * t151 / 0.720D3 + 
     #t556 * t140 * t151 / 0.720D3 + t798 * t797 + t800 * t368 + t802 * 
     #t495
      t805 = FJET(XB1, XB2, s, 0.0D0, -t159, 0.0D0, t158, 0.0D0, t271)
      t807 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t797)
      t809 = FJET(XB1, XB2, s, -t4, -t499, 0.0D0, -t501, t59, t545)
      t811 = FJET(XB1, XB2, s, -t501, 0.0D0, -t499, -t4, t59, t545)
      t813 = FJET(XB1, XB2, s, -t373, t371, t376, -t375, 0.0D0, t428)
      t815 = FJET(XB1, XB2, s, t376, -t375, -t373, t371, 0.0D0, t428)
      t817 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t797)
      t831 = t519 / 0.720D3 + (0.90D2 * t61 * t63 * t530 - 0.180D3 * t61
     # * t133 * t535 - t540) * t143 * t144 / 0.720D3
      t832 = FJET(XB1, XB2, s, 0.0D0, -t501, -t4, -t499, t59, t831)
      t834 = FJET(XB1, XB2, s, -t375, t376, t371, -t373, 0.0D0, t428)
      t836 = FJET(XB1, XB2, s, -t159, 0.0D0, t158, 0.0D0, 0.0D0, t271)
      t838 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t797)
      t854 = t300 / 0.720D3 - (-t61 * t188 * t308 + 0.90D2 * t61 * t63 *
     # t317 - t322 - 0.180D3 * t61 * t133 * t326) * t144 / 0.720D3 + t34
     #6 / 0.720D3 + t366 / 0.720D3
      t855 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t12, -t4, 0.0D0, t854)
      t857 = FJET(XB1, XB2, s, t158, 0.0D0, -t159, 0.0D0, 0.0D0, t271)
      t859 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t12, 0.0D0, t854)
      t861 = t805 * t271 + t807 * t797 + t809 * t545 + t811 * t545 + t81
     #3 * t428 + t815 * t428 + t817 * t797 + t832 * t831 + t834 * t428 +
     # t836 * t271 + t838 * t797 + t855 * t854 + t857 * t271 + t859 * t8
     #54
      rrqqbar2gght5s4e1 = t804 + t861

      end function



      doubleprecision function rrqqbar2gght5s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x1 ** 2
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t22 = log(0.4D1 * t11 * t12 * t15 * t18)
      t23 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t29 = lh * t6
      t34 = 0.1D1 / x3
      t36 = 0.1D1 / x1
      t39 = t4 * t6
      t40 = x2 * t12
      t42 = t15 * t18
      t43 = -0.1D1 + x2
      t44 = t43 ** 2
      t45 = t42 * t44
      t48 = log(0.4D1 * t40 * t10 * t45)
      t51 = t10 * t15 * t18
      t54 = log(0.4D1 * t40 * t51)
      t57 = 0.1D1 / x2
      t65 = log(0.4D1 * t10 * t12 * t42)
      t67 = t65 ** 2
      t79 = pi ** 2
      t81 = lh ** 2
      t83 = 0.30D2 * t79 - 0.180D3 * t81
      t84 = t83 * t6
      t86 = t4 * t84 * t23
      t92 = log(0.4D1 * t11 * t42)
      t94 = t92 ** 2
      t110 = log(0.4D1 * t51)
      t112 = t110 ** 2
      t119 = t23 * pi
      t143 = x2 * x3
      t146 = log(0.4D1 * t143 * t51)
      t148 = t143 * t10
      t151 = log(0.4D1 * t148 * t45)
      t159 = x2 * t10
      t162 = log(0.4D1 * t159 * t42)
      t163 = t162 ** 2
      t166 = log(0.4D1 * t159 * t45)
      t167 = t166 ** 2
      t185 = (-0.90D2 * t4 * t6 * (-t7 + t22 * t23) - 0.180D3 * t4 * t29
     # * t23) * t34 * t36 / 0.720D3 - t39 * (-t48 * t23 + t54 * t23) * t
     #57 * t36 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t65 * t7 + t67 * t23 / 0
     #.2D1) + 0.180D3 * t4 * t29 * (t7 - t65 * t23) + t86) * t36 / 0.720
     #D3 + (-0.90D2 * t4 * t6 * (t92 * t7 - t94 * t23 / 0.2D1) + 0.180D3
     # * t4 * t29 * (-t7 + t92 * t23) - t86) * t34 / 0.1440D4 - (0.180D3
     # * (-t110 * t7 + t112 * t23 / 0.2D1) * pi * lh + t119 * (0.240D3 *
     # zeta3 + 0.120D3 * t81 * lh - 0.60D2 * lh * t79) - 0.90D2 * (t112 
     #* t7 / 0.2D1 - t112 * t110 * t23 / 0.6D1) * pi + (t7 - t110 * t23)
     # * pi * t83) * t3 * t6 / 0.1440D4 - t39 * (t146 * t23 - t151 * t23
     #) * t34 * t57 / 0.16D2 - (-0.90D2 * t4 * t6 * t23 * (t163 / 0.2D1 
     #- t167 / 0.2D1) + (0.180D3 * t119 * lh - 0.90D2 * t7 * pi) * t3 * 
     #t6 * (-t162 + t166)) * t57 / 0.1440D4
      t186 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t185)
      t188 = -0.1D1 + x3
      t189 = -0.1D1 + t143
      t190 = 0.1D1 / t189
      t192 = t2 * t188 * t190
      t194 = x3 * t43 * t190
      t195 = t2 * t194
      t196 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t192, t195, 0.0D0)
      t200 = t189 ** 2
      t206 = log(-0.4D1 * t11 * x2 * t15 * t44 * t18 / t200 * t188)
      t207 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t192, t195, 0.0D0)
      t211 = cos(t13)
      t212 = x3 * z
      t213 = x2 * t188
      t215 = Sqrt(-t212 * t213)
      t219 = 0.1D1 / (-z - t143 + 0.2D1 * t211 * t215)
      t224 = t6 * t207
      t225 = z * t219
      t235 = t34 * t57 * t36
      t239 = (-0.90D2 * t39 * (t196 - t206 * t207) * z * t219 + 0.180D3 
     #* t4 * lh * t224 * t225) * t34 * t57 / 0.1440D4 - t4 * t224 * t225
     # * t235 / 0.8D1
      t240 = FJET(XB1, XB2, s, t192, 0.0D0, t195, 0.0D0, 0.0D0, t239)
      t242 = x2 * x1
      t243 = x1 * z
      t244 = -z - x1 + t243
      t245 = 0.1D1 / t244
      t247 = t2 * t242 * t245
      t248 = -0.1D1 + x1
      t249 = t2 * t248
      t251 = t2 * x1 * t43
      t256 = s * t17 * x2 * x1 * t248 * t245
      t257 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t251, -t247
     #, -t249, 0.0D0, t256)
      t259 = t57 * t36
      t262 = t39 * t257 * t34 * t259 / 0.8D1
      t263 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t251, -t247
     #, -t249, 0.0D0, t256)
      t264 = 0.1D1 / t8
      t267 = t18 * t245
      t268 = t248 ** 2
      t273 = log(-0.4D1 * t40 * t264 * t15 * t267 * t268 * t44)
      t275 = -t263 + t273 * t257
      t281 = 0.180D3 * t4 * t29 * t257
      t286 = t262 + (-0.90D2 * t4 * t6 * t275 - t281) * t57 * t36 / 0.72
     #0D3
      t287 = FJET(XB1, XB2, s, 0.0D0, -t247, -t249, -t251, t256, t286)
      t289 = FJET(XB1, XB2, s, 0.0D0, t195, 0.0D0, t192, 0.0D0, t239)
      t291 = t188 * s
      t292 = t1 * t248
      t293 = t291 * t292
      t295 = t291 * t1 * x1
      t297 = x3 * s * t292
      t298 = x3 * x1
      t299 = t2 * t298
      t300 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t295, t299,
     # t293, -t297, 0.0D0)
      t309 = log(0.4D1 * t245 * t15 * t264 * t18 * t12 * t268 * x3 * t18
     #8)
      t310 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t295, t299,
     # t293, -t297, 0.0D0)
      t327 = (-0.90D2 * t4 * t6 * (-t300 + t309 * t310) - 0.180D3 * t4 *
     # t29 * t310) * t34 * t36 / 0.720D3 + t39 * t310 * t34 * t259 / 0.8
     #D1
      t328 = FJET(XB1, XB2, s, t293, -t295, -t297, t299, 0.0D0, t327)
      t330 = t2 * x1
      t331 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t330, 0.0D0,
     # -t249, 0.0D0, 0.0D0)
      t335 = t42 * t245 * t268
      t338 = log(-0.4D1 * x3 * t264 * t12 * t335)
      t339 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t330, 0.0D0,
     # -t249, 0.0D0, 0.0D0)
      t347 = 0.180D3 * t4 * t29 * t339
      t351 = (-0.90D2 * t4 * t6 * (t331 - t338 * t339) + t347) * t34 * t
     #36 / 0.720D3
      t355 = t39 * t339 * t34 * t259 / 0.8D1
      t359 = log(-0.4D1 * t40 * t264 * t335)
      t368 = (-0.90D2 * t4 * t6 * (t331 - t359 * t339) + t347) * t57 * t
     #36 / 0.720D3
      t374 = log(-0.4D1 * t264 * t12 * t15 * t267 * t268)
      t376 = t374 ** 2
      t379 = -t374 * t331 + t376 * t339 / 0.2D1
      t384 = t331 - t374 * t339
      t389 = t4 * t84 * t339
      t393 = t351 - t355 + t368 - (0.90D2 * t4 * t6 * t379 - 0.180D3 * t
     #4 * t29 * t384 - t389) * t36 / 0.720D3
      t394 = FJET(XB1, XB2, s, -t249, t330, 0.0D0, 0.0D0, 0.0D0, t393)
      t396 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t185)
      t409 = t351 - t355 + t368 - (0.90D2 * t4 * t6 * t379 - 0.180D3 * t
     #4 * t29 * t384 - t389) * t36 / 0.720D3
      t410 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t249, t330, 0.0D0, t409)
      t412 = t2 * t188
      t413 = t2 * x3
      t414 = t42 * t188
      t417 = log(-0.4D1 * t11 * t414)
      t418 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t412, t413, 0.0D0)
      t420 = t417 ** 2
      t421 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t412, t413, 0.0D0)
      t438 = x3 * t12
      t442 = log(-0.4D1 * t438 * t10 * t414)
      t450 = 0.180D3 * t4 * t29 * t421
      t461 = log(-0.4D1 * t148 * t414)
      t471 = (-0.90D2 * t4 * t6 * (-t417 * t418 + t420 * t421 / 0.2D1) +
     # 0.180D3 * t4 * t29 * (t418 - t417 * t421) + t4 * t84 * t421) * t3
     #4 / 0.1440D4 + (-0.90D2 * t4 * t6 * (t418 - t442 * t421) + t450) *
     # t34 * t36 / 0.720D3 - t39 * t421 * t34 * t259 / 0.8D1 + (-0.90D2 
     #* t4 * t6 * (t418 - t461 * t421) + t450) * t34 * t57 / 0.1440D4
      t472 = FJET(XB1, XB2, s, -t412, 0.0D0, t413, 0.0D0, 0.0D0, t471)
      t474 = FJET(XB1, XB2, s, 0.0D0, t413, 0.0D0, -t412, 0.0D0, t471)
      t476 = FJET(XB1, XB2, s, 0.0D0, t192, 0.0D0, t195, 0.0D0, t239)
      t478 = FJET(XB1, XB2, s, t413, 0.0D0, -t412, 0.0D0, 0.0D0, t471)
      t480 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t185)
      t483 = t291 * t292 * t190
      t484 = t298 * z
      t487 = Sqrt(x3 * t244 * t213)
      t488 = t211 * t487
      t489 = 0.2D1 * t488
      t494 = t330 * t43 * (-t143 - z + t212 - x1 + t298 + t243 - t484 + 
     #t489) * t245 * t190
      t495 = t249 * t194
      t496 = t143 * z
      t498 = t143 * x1
      t500 = x2 ** 2
      t501 = t500 * x3
      t502 = t501 * x1
      t508 = t501 * t243
      t509 = -t212 - t298 + t484 + 0.2D1 * t496 + 0.2D1 * t498 - t502 - 
     #t501 * z + t143 - x2 + 0.2D1 * t488 * x2 - 0.2D1 * t143 * t243 + t
     #508 - t489
      t512 = t330 * t509 * t245 * t190
      t513 = t242 * z
      t514 = z + x1 - t243 - t242 + t513
      t516 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t494, -t512,
     # -t483, -t495, t256)
      t519 = t8 * x2
      t523 = t12 * t8
      t531 = -t519 * x1 - 0.2D1 * t40 * z + t523 * x2 + 0.2D1 * x1 * t8 
     #+ 0.2D1 * t12 * z - t523 + t513 - t496 - t498 - t438 * x2 + t502 -
     # 0.2D1 * t243
      t548 = -0.2D1 * t488 * t242 - 0.2D1 * t488 * t243 - t8 + 0.2D1 * t
     #488 * z + 0.2D1 * t488 * x1 - t12 + 0.2D1 * t488 * t513 + x3 * t8 
     #* t242 + 0.2D1 * t438 * x2 * z - t438 * t519 - t508 + t40
      t550 = 0.1D1 / (t531 + t548)
      t554 = t4 * t6 * t514 * t516 * t550 * t244 * t235 / 0.8D1
      t555 = FJET(XB1, XB2, s, -t483, t494, -t495, -t512, t256, -t554)
      t558 = t3 * t6 * t514
      t562 = t516 * t550 * t244 * t235
      t565 = t186 * t185 + t240 * t239 + t287 * t286 + t289 * t239 + t32
     #8 * t327 + t394 * t393 + t396 * t185 + t410 * t409 + t472 * t471 +
     # t474 * t471 + t476 * t239 + t478 * t471 + t480 * t185 - t555 * pi
     # * t558 * t562 / 0.8D1
      t566 = FJET(XB1, XB2, s, t494, -t483, -t512, -t495, t256, -t554)
      t571 = FJET(XB1, XB2, s, t195, 0.0D0, t192, 0.0D0, 0.0D0, t239)
      t573 = FJET(XB1, XB2, s, 0.0D0, -t412, 0.0D0, t413, 0.0D0, t471)
      t575 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t330, -t249, 0.0D0, t409)
      t577 = FJET(XB1, XB2, s, t299, -t297, -t295, t293, 0.0D0, t327)
      t579 = FJET(XB1, XB2, s, t330, -t249, 0.0D0, 0.0D0, 0.0D0, t393)
      t589 = t262 + (-0.90D2 * t4 * t6 * t275 - t281) * t57 * t36 / 0.72
     #0D3
      t590 = FJET(XB1, XB2, s, -t249, -t251, 0.0D0, -t247, t256, t589)
      t592 = FJET(XB1, XB2, s, -t251, -t249, -t247, 0.0D0, t256, t589)
      t594 = FJET(XB1, XB2, s, -t297, t299, t293, -t295, 0.0D0, t327)
      t596 = FJET(XB1, XB2, s, -t295, t293, t299, -t297, 0.0D0, t327)
      t598 = FJET(XB1, XB2, s, -t247, 0.0D0, -t251, -t249, t256, t589)
      t600 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t185)
      t602 = FJET(XB1, XB2, s, -t495, -t512, -t483, t494, t256, -t554)
      t607 = FJET(XB1, XB2, s, -t512, -t495, t494, -t483, t256, -t554)
      t612 = -t566 * pi * t558 * t562 / 0.8D1 + t571 * t239 + t573 * t47
     #1 + t575 * t409 + t577 * t327 + t579 * t393 + t590 * t589 + t592 *
     # t589 + t594 * t327 + t596 * t327 + t598 * t589 + t600 * t185 - t6
     #02 * pi * t558 * t562 / 0.8D1 - t607 * pi * t558 * t562 / 0.8D1
      rrqqbar2gght5s4e0 = t565 + t612

      end function



      doubleprecision function rrqqbar2gght5s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t27 = lh * t6
      t30 = 0.180D3 * t4 * t27 * t21
      t32 = 0.1D1 / x3
      t35 = x1 ** 2
      t39 = log(0.4D1 * t10 * t35 * t17)
      t46 = 0.1D1 / x1
      t49 = t4 * t6
      t54 = x2 * t10
      t57 = log(0.4D1 * t54 * t17)
      t58 = -0.1D1 + x2
      t59 = t58 ** 2
      t63 = log(0.4D1 * t54 * t17 * t59)
      t66 = 0.1D1 / x2
      t73 = log(0.4D1 * t10 * t14 * t16)
      t80 = t73 ** 2
      t87 = pi ** 2
      t89 = lh ** 2
      t97 = (-0.90D2 * t4 * t6 * (-t7 + t20 * t21) - t30) * t32 / 0.1440
     #D4 - (-0.90D2 * t4 * t6 * (t7 - t39 * t21) + t30) * t46 / 0.720D3 
     #+ t49 * t21 * t32 * t46 / 0.8D1 + t49 * t21 * (-t57 + t63) * t66 /
     # 0.16D2 - (0.180D3 * (t7 - t73 * t21) * pi * lh - 0.90D2 * (-t73 *
     # t7 + t80 * t21 / 0.2D1) * pi + t21 * pi * (0.30D2 * t87 - 0.180D3
     # * t89)) * t3 * t6 / 0.1440D4
      t98 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t97)
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t97)
      t102 = t2 * x1
      t103 = -0.1D1 + x1
      t104 = t2 * t103
      t105 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t102, 0.0D0,
     # -t104, 0.0D0, 0.0D0)
      t109 = t49 * t105 * t66 * t46 / 0.8D1
      t110 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t102, 0.0D0,
     # -t104, 0.0D0, 0.0D0)
      t116 = 0.1D1 / (-z - x1 + x1 * z)
      t118 = t103 ** 2
      t122 = log(-0.4D1 / t8 * t35 * t14 * t16 * t116 * t118)
      t124 = -t110 + t122 * t105
      t130 = 0.180D3 * t4 * t27 * t105
      t137 = t49 * t105 * t32 * t46 / 0.8D1
      t138 = -t109 - (-0.90D2 * t4 * t6 * t124 - t130) * t46 / 0.720D3 -
     # t137
      t139 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t102, -t104, 0.0D0, t138)
      t141 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t104, t102, 0.0D0, t138)
      t143 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t97)
      t145 = t2 * x3
      t146 = -0.1D1 + x3
      t147 = t2 * t146
      t148 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t147, t145, 0.0D0)
      t152 = log(-0.4D1 * t11 * t17 * t146)
      t153 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, -t147, t145, 0.0D0)
      t165 = t153 * t32
      t172 = (-0.90D2 * t4 * t6 * (t148 - t152 * t153) + 0.180D3 * t4 * 
     #t27 * t153) * t32 / 0.1440D4 - t49 * t165 * t46 / 0.8D1 - t49 * t1
     #65 * t66 / 0.16D2
      t173 = FJET(XB1, XB2, s, 0.0D0, t145, 0.0D0, -t147, 0.0D0, t172)
      t175 = x2 * x3
      t177 = 0.1D1 / (-0.1D1 + t175)
      t179 = t2 * t146 * t177
      t182 = t2 * x3 * t58 * t177
      t183 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t179, t182, 0.0D0)
      t186 = cos(t12)
      t190 = Sqrt(-x3 * z * x2 * t146)
      t194 = 0.1D1 / (-z - t175 + 0.2D1 * t186 * t190)
      t199 = t4 * t6 * t183 * z * t194 * t32 * t66 / 0.16D2
      t200 = FJET(XB1, XB2, s, 0.0D0, t179, 0.0D0, t182, 0.0D0, -t199)
      t202 = t3 * t6
      t207 = t183 * z * t194 * t32 * t66
      t210 = FJET(XB1, XB2, s, 0.0D0, t182, 0.0D0, t179, 0.0D0, -t199)
      t215 = FJET(XB1, XB2, s, 0.0D0, -t147, 0.0D0, t145, 0.0D0, t172)
      t219 = t2 * x1 * x2 * t116
      t221 = t2 * x1 * t58
      t226 = s * t15 * x2 * x1 * t103 * t116
      t227 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t221, -t219
     #, -t104, 0.0D0, t226)
      t231 = t49 * t227 * t66 * t46 / 0.8D1
      t232 = FJET(XB1, XB2, s, 0.0D0, -t219, -t104, -t221, t226, t231)
      t237 = t6 * t227 * t66 * t46
      t240 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t97)
      t249 = -t109 - (-0.90D2 * t4 * t6 * t124 - t130) * t46 / 0.720D3 -
     # t137
      t250 = FJET(XB1, XB2, s, t102, -t104, 0.0D0, 0.0D0, 0.0D0, t249)
      t252 = t98 * t97 + t100 * t97 + t139 * t138 + t141 * t138 + t143 *
     # t97 + t173 * t172 - t200 * pi * t202 * t207 / 0.16D2 - t210 * pi 
     #* t202 * t207 / 0.16D2 + t215 * t172 + t232 * pi * t3 * t237 / 0.8
     #D1 + t240 * t97 + t250 * t249
      t253 = FJET(XB1, XB2, s, t145, 0.0D0, -t147, 0.0D0, 0.0D0, t172)
      t256 = t2 * x1 * x3
      t258 = t1 * t103
      t259 = x3 * s * t258
      t260 = t146 * s
      t262 = t260 * t1 * x1
      t263 = t260 * t258
      t264 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t262, t256,
     # t263, -t259, 0.0D0)
      t268 = t49 * t264 * t32 * t46 / 0.8D1
      t269 = FJET(XB1, XB2, s, t256, -t259, -t262, t263, 0.0D0, t268)
      t274 = t6 * t264 * t32 * t46
      t277 = FJET(XB1, XB2, s, t263, -t262, -t259, t256, 0.0D0, t268)
      t282 = FJET(XB1, XB2, s, t179, 0.0D0, t182, 0.0D0, 0.0D0, -t199)
      t287 = FJET(XB1, XB2, s, t182, 0.0D0, t179, 0.0D0, 0.0D0, -t199)
      t292 = FJET(XB1, XB2, s, -t104, t102, 0.0D0, 0.0D0, 0.0D0, t249)
      t294 = FJET(XB1, XB2, s, -t104, -t221, 0.0D0, -t219, t226, t231)
      t299 = FJET(XB1, XB2, s, -t147, 0.0D0, t145, 0.0D0, 0.0D0, t172)
      t301 = FJET(XB1, XB2, s, -t221, -t104, -t219, 0.0D0, t226, t231)
      t306 = FJET(XB1, XB2, s, -t259, t256, t263, -t262, 0.0D0, t268)
      t311 = FJET(XB1, XB2, s, -t262, t263, t256, -t259, 0.0D0, t268)
      t316 = FJET(XB1, XB2, s, -t219, 0.0D0, -t221, -t104, t226, t231)
      t321 = t253 * t172 + t269 * pi * t3 * t274 / 0.8D1 + t277 * pi * t
     #3 * t274 / 0.8D1 - t282 * pi * t202 * t207 / 0.16D2 - t287 * pi * 
     #t202 * t207 / 0.16D2 + t292 * t249 + t294 * pi * t3 * t237 / 0.8D1
     # + t299 * t172 + t301 * pi * t3 * t237 / 0.8D1 + t306 * pi * t3 * 
     #t274 / 0.8D1 + t311 * pi * t3 * t274 / 0.8D1 + t316 * pi * t3 * t2
     #37 / 0.8D1
      rrqqbar2gght5s4em1 = t252 + t321

      end function



      doubleprecision function rrqqbar2gght5s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x3
      t20 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t2, 0.0D0, 0.0D0)
      t21 = z ** 2
      t25 = Sin(x4 * pi)
      t26 = t25 ** 2
      t28 = t1 ** 2
      t29 = t28 ** 2
      t32 = log(0.4D1 / t21 / z * t26 * t29)
      t41 = t4 * t8 * t9 / 0.8D1 + t4 * t8 * t13 / 0.16D2 - (0.180D3 * t
     #7 * pi * lh - 0.90D2 * (t20 - t32 * t7) * pi) * t3 * t6 / 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t46 = t2 * x1
      t48 = t2 * (-0.1D1 + x1)
      t49 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t46, 0.0D0, -
     #t48, 0.0D0, 0.0D0)
      t51 = t6 * t49 * t9
      t53 = t4 * t51 / 0.8D1
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t46, -t48, 0.0D0, -t53)
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t48, t46, 0.0D0, -t53)
      t64 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t66 = t2 * x3
      t68 = t2 * (-0.1D1 + x3)
      t69 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # -t68, t66, 0.0D0)
      t71 = t6 * t69 * t13
      t73 = t4 * t71 / 0.16D2
      t74 = FJET(XB1, XB2, s, 0.0D0, t66, 0.0D0, -t68, 0.0D0, -t73)
      t79 = FJET(XB1, XB2, s, 0.0D0, -t68, 0.0D0, t66, 0.0D0, -t73)
      t84 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t86 = FJET(XB1, XB2, s, t46, -t48, 0.0D0, 0.0D0, 0.0D0, -t53)
      t91 = FJET(XB1, XB2, s, t66, 0.0D0, -t68, 0.0D0, 0.0D0, -t73)
      t96 = FJET(XB1, XB2, s, -t48, t46, 0.0D0, 0.0D0, 0.0D0, -t53)
      t101 = FJET(XB1, XB2, s, -t68, 0.0D0, t66, 0.0D0, 0.0D0, -t73)
      rrqqbar2gght5s4em2 = t42 * t41 + t44 * t41 - t54 * pi * t3 * t51 /
     # 0.8D1 - t59 * pi * t3 * t51 / 0.8D1 + t64 * t41 - t74 * pi * t3 *
     # t71 / 0.16D2 - t79 * pi * t3 * t71 / 0.16D2 + t84 * t41 - t86 * p
     #i * t3 * t51 / 0.8D1 - t91 * pi * t3 * t71 / 0.16D2 - t96 * pi * t
     #3 * t51 / 0.8D1 - t101 * pi * t3 * t71 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t10 = pi * t3 * t6 * t7 / 0.16D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = t3 * t6 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s4em3 = t11 * pi * t14 / 0.16D2 + t16 * pi * t14 / 0.
     #16D2 + t19 * pi * t14 / 0.16D2 + t22 * pi * t14 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght5s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh51J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 ** 2
      t3 = S13 * t1 * S23
      t5 = S13 ** 2
      t7 = t5 * S13 * S23
      t10 = S14 ** 2
      t21 = t10 * S14 * S24
      t23 = S24 ** 2
      t25 = S14 * t23 * S24
      t42 = S12 ** 2
      rrqqbar2ggh51J1 = (((0.16D2 / 0.3D1 * t3 + 0.16D2 / 0.3D1 * t7 + 0
     #.16D2 / 0.3D1 * S13 * S23 * t10 + 0.32D2 / 0.3D1 * t5 * S23 * S14)
     # / (S12 + S14 + S24) + (0.16D2 / 0.3D1 * t21 + 0.16D2 / 0.3D1 * t2
     #5 + 0.32D2 / 0.3D1 * S14 * t23 * S23 + 0.16D2 / 0.3D1 * S14 * S24 
     #* t1) / (S12 + S13 + S23)) / S12 + (0.16D2 / 0.3D1 * t3 + 0.16D2 /
     # 0.3D1 * t7 + 0.16D2 / 0.3D1 * t21 + 0.16D2 / 0.3D1 * t25) / t42) 
     #/ pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh51J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 ** 2
      t3 = t1 * S23 * S14
      t5 = S23 ** 2
      t7 = S13 * t5 * S14
      t10 = 0.32D2 / 0.3D1 * t1 * t5
      t13 = 0.16D2 / 0.3D1 * t5 * S13 * S23
      t16 = 0.16D2 / 0.3D1 * S13 * t1 * S23
      t18 = S14 ** 2
      t20 = 0.16D2 / 0.3D1 * S13 * S23 * t18
      t27 = 0.16D2 / 0.3D1 * S14 * S24 * t5
      t28 = S24 ** 2
      t30 = 0.32D2 / 0.3D1 * t18 * t28
      t33 = 0.16D2 / 0.3D1 * t18 * S14 * S24
      t35 = t18 * S24 * S23
      t38 = S14 * t28 * S23
      t42 = 0.16D2 / 0.3D1 * S14 * t28 * S24
      t54 = t27 + 0.16D2 / 0.3D1 * t3 + 0.16D2 / 0.3D1 * t38 + 0.16D2 / 
     #0.3D1 * t35 - t10 - t16 + 0.16D2 / 0.3D1 * t7 + t20 - t33 - t13 - 
     #t42 - t30
      t55 = S12 ** 2
      rrqqbar2ggh51J2 = (((-0.16D2 * t3 - 0.16D2 * t7 - t10 - t13 - t16 
     #- t20) / (S12 + S14 + S24) + (-t27 - t30 - t33 - 0.16D2 * t35 - 0.
     #16D2 * t38 - t42) / (S12 + S13 + S23)) / S12 + t54 / t55) / pi * w
     #d / z

      end function
  
 