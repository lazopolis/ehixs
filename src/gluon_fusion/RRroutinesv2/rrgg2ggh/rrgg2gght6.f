  
      subroutine rrgg2gght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh61J1  
      doubleprecision rrgg2ggh61J2  
      doubleprecision rrgg2ggh61J3  
      doubleprecision rrgg2ggh61J4  
      doubleprecision rrgg2ggh61J5  
      doubleprecision rrgg2ggh61J6  
      doubleprecision rrgg2ggh61J7  
      doubleprecision rrgg2gght6s1e1  
      doubleprecision rrgg2gght6s1e0  
      doubleprecision rrgg2gght6s1em1  
      doubleprecision rrgg2gght6s1em2  
      doubleprecision rrgg2gght6s1em3  
      doubleprecision rrgg2gght6s1em4  
      doubleprecision rrgg2gght6s2e1  
      doubleprecision rrgg2gght6s2e0  
      doubleprecision rrgg2gght6s2em1  
      doubleprecision rrgg2gght6s2em2  
      doubleprecision rrgg2gght6s2em3  
      doubleprecision rrgg2gght6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght6s1e1
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t6 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = lh ** 2
      t11 = pi ** 2
      t13 = -0.180D3 * t9 + 0.30D2 * t11
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = x4 * pi
      t21 = Sin(t20)
      t22 = t21 ** 2
      t23 = x3 * t22
      t24 = z ** 2
      t25 = 0.1D1 / t24
      t28 = log(0.4D1 * t23 * t25)
      t29 = 0.1D1 / z
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t23 * t25 * t32)
      t37 = cos(t20)
      t38 = z * t37
      t40 = Sqrt(-x3 * t31)
      t44 = 0.1D1 / (-x3 - z + 0.2D1 * t38 * t40)
      t50 = t36 ** 2
      t53 = t28 ** 2
      t68 = -0.60D2 * lh * t11 + 0.240D3 * zeta3 + 0.120D3 * t9 * lh
      t70 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t90 = 0.1D1 / x3
      t93 = t25 * t22
      t95 = log(0.4D1 * t93)
      t96 = t95 ** 2
      t99 = t96 * t95
      t116 = t11 ** 2
      t117 = t9 ** 2
      t123 = rrgg2ggh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t131 = t96 ** 2
      t136 = t29 * pi
      t139 = t18 * pi
      t140 = x1 ** 2
      t141 = x3 * t140
      t142 = t93 * t32
      t145 = log(-0.4D1 * t141 * t142)
      t147 = t145 ** 2
      t154 = log(0.4D1 * t141 * t93)
      t156 = t154 ** 2
      t164 = lh * t18
      t175 = t13 * t18
      t179 = pi * (-t8 * t44 - t8 * t29)
      t180 = t175 * t179
      t183 = 0.1D1 / x1
      t186 = t140 * t22
      t187 = t186 * t25
      t189 = log(0.4D1 * t187)
      t194 = t18 * t29
      t195 = t189 ** 2
      t206 = t68 * t18
      t207 = t136 * t8
      t208 = t206 * t207
      t219 = x2 ** 2
      t220 = t219 * x3
      t221 = t220 * t187
      t223 = log(0.4D1 * t221)
      t227 = t220 * t140
      t230 = log(-0.4D1 * t227 * t142)
      t241 = 0.1D1 / x2
      t242 = t183 * t241
      t245 = t219 * t140
      t248 = log(0.4D1 * t245 * t93)
      t250 = t248 ** 2
      t270 = log(-0.4D1 * t220 * t142)
      t272 = t270 ** 2
      t279 = log(0.4D1 * t220 * t93)
      t281 = t279 ** 2
      t303 = t219 * t22
      t306 = log(0.4D1 * t303 * t25)
      t312 = t306 ** 2
      t334 = ((0.180D3 * t3 * lh - 0.90D2 * t6 + t8 * t13) * t18 * pi * 
     #(-t28 * t29 - t36 * t44) - 0.90D2 * t8 * t18 * pi * (-t50 * t36 * 
     #t44 / 0.6D1 - t53 * t28 * t29 / 0.6D1) + (0.180D3 * t6 * lh + t8 *
     # t68 - 0.90D2 * t70 + t3 * t13) * t18 * pi * (t29 + t44) + (0.180D
     #3 * t8 * lh - 0.90D2 * t3) * t18 * pi * (t53 * t29 / 0.2D1 + t50 *
     # t44 / 0.2D1)) * t90 / 0.2880D4 + (0.180D3 * (t96 * t3 / 0.2D1 + t
     #70 - t99 * t8 / 0.6D1 - t95 * t6) * lh + (t6 - t95 * t3 + t96 * t8
     # / 0.2D1) * t13 + (t3 - t95 * t8) * t68 + t8 * (-0.480D3 * lh * ze
     #ta3 - t116 - 0.60D2 * t117 + 0.60D2 * t9 * t11) - 0.90D2 * t123 + 
     #0.15D2 * t99 * t3 - 0.45D2 * t96 * t6 + 0.90D2 * t95 * t70 - 0.15D
     #2 / 0.4D1 * t131 * t8) * t18 * t136 / 0.2880D4 - (-0.90D2 * t139 *
     # (-(t6 - t145 * t3 + t147 * t8 / 0.2D1) * t44 - (t6 - t154 * t3 + 
     #t156 * t8 / 0.2D1) * t29) + 0.180D3 * t164 * pi * (-(t3 - t145 * t
     #8) * t44 - (t3 - t154 * t8) * t29) + t180) * t90 * t183 / 0.1440D4
     # - (t175 * t136 * (-t3 + t189 * t8) - 0.90D2 * t194 * pi * (-t195 
     #* t3 / 0.2D1 - t70 + t195 * t189 * t8 / 0.6D1 + t189 * t6) - t208 
     #+ 0.180D3 * t164 * t136 * (-t6 + t189 * t3 - t195 * t8 / 0.2D1)) *
     # t183 / 0.1440D4 - (-0.90D2 * t139 * (-(t3 - t223 * t8) * t29 - (t
     #3 - t230 * t8) * t44) + 0.180D3 * t164 * t179) * t90 * t242 / 0.72
     #0D3 + (-0.90D2 * t139 * (t6 - t248 * t3 + t250 * t8 / 0.2D1) * t29
     # + 0.180D3 * t164 * pi * (t3 - t248 * t8) * t29 + t175 * t207) * t
     #183 * t241 / 0.720D3 - (-0.90D2 * t139 * (-(t6 - t270 * t3 + t272 
     #* t8 / 0.2D1) * t44 - (t6 - t279 * t3 + t281 * t8 / 0.2D1) * t29) 
     #+ 0.180D3 * t164 * pi * (-(t3 - t270 * t8) * t44 - (t3 - t279 * t8
     #) * t29) + t180) * t90 * t241 / 0.1440D4 + (t175 * pi * (t3 - t306
     # * t8) * t29 - 0.90D2 * t139 * (t312 * t3 / 0.2D1 + t70 - t312 * t
     #306 * t8 / 0.6D1 - t306 * t6) * t29 + t208 + 0.180D3 * t164 * pi *
     # (t6 - t306 * t3 + t312 * t8 / 0.2D1) * t29) * t241 / 0.1440D4
      t335 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t334)
      t337 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t334)
      t339 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t334)
      t341 = t2 * x1
      t342 = -0.1D1 + x1
      t343 = x1 * z
      t344 = 0.1D1 - x1 + t343
      t345 = 0.1D1 / t344
      t347 = t2 * t342 * t345
      t348 = t1 ** 2
      t349 = s * t348
      t351 = x1 * t342 * t345
      t352 = t349 * t351
      t353 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t347, t
     #341, 0.0D0, -t352)
      t355 = t141 * t22
      t356 = t342 ** 2
      t357 = t25 * t356
      t359 = t357 * t345 * t32
      t362 = log(-0.4D1 * t355 * t359)
      t363 = t362 * t344
      t364 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t347, t
     #341, 0.0D0, -t352)
      t366 = t362 ** 2
      t368 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t347, t
     #341, 0.0D0, -t352)
      t372 = x3 * x1
      t373 = 0.2D1 * t372
      t374 = x3 * t344
      t376 = Sqrt(-t374 * t31)
      t379 = x1 * t24
      t380 = t372 * t24
      t382 = 0.2D1 * t141 * z
      t383 = t140 * t24
      t384 = t383 * x3
      t385 = t372 * z
      t386 = 0.3D1 * t385
      t387 = -z - t141 + t373 - x3 + t343 + 0.2D1 * t38 * t376 - t379 + 
     #t380 + t382 - t384 - t386
      t388 = 0.1D1 / t387
      t390 = t29 * t353
      t391 = t357 * t345
      t394 = log(0.4D1 * t355 * t391)
      t395 = t394 * t29
      t397 = t394 ** 2
      t404 = t344 * t364
      t408 = t29 * t364
      t418 = pi * (t29 * t368 + t344 * t368 * t388)
      t426 = log(0.4D1 * t186 * t391)
      t431 = t426 ** 2
      t434 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t347, t
     #341, 0.0D0, -t352)
      t443 = t136 * t368
      t455 = t356 * t345
      t459 = log(0.4D1 * t227 * t93 * t455)
      t462 = t220 * t186
      t465 = log(-0.4D1 * t462 * t359)
      t479 = t245 * t22
      t482 = log(0.4D1 * t479 * t391)
      t483 = t482 * t29
      t485 = t482 ** 2
      t502 = -(-0.90D2 * t139 * ((t344 * t353 - t363 * t364 + t366 * t34
     #4 * t368 / 0.2D1) * t388 + t390 - t395 * t364 + t397 * t29 * t368 
     #/ 0.2D1) + 0.180D3 * t164 * pi * ((t404 - t363 * t368) * t388 + t4
     #08 - t395 * t368) + t175 * t418) * t90 * t183 / 0.1440D4 - (t175 *
     # t136 * (t364 - t426 * t368) - 0.90D2 * t194 * pi * (t431 * t364 /
     # 0.2D1 + t434 - t431 * t426 * t368 / 0.6D1 - t426 * t353) + t206 *
     # t443 + 0.180D3 * t164 * t136 * (t353 - t426 * t364 + t431 * t368 
     #/ 0.2D1)) * t183 / 0.1440D4 - (-0.90D2 * t139 * (t408 - t459 * t29
     # * t368 + (t404 - t465 * t344 * t368) * t388) + 0.180D3 * t164 * t
     #418) * t90 * t242 / 0.720D3 + (-0.90D2 * t139 * (-t390 + t483 * t3
     #64 - t485 * t29 * t368 / 0.2D1) + 0.180D3 * t164 * pi * (-t408 + t
     #483 * t368) - t175 * t443) * t183 * t241 / 0.720D3
      t503 = FJET(XB1, XB2, s, 0.0D0, t341, -t347, 0.0D0, -t352, t502)
      t505 = x2 * s
      t506 = t505 * t1
      t507 = -0.1D1 + x2
      t508 = t507 * s
      t509 = t508 * t1
      t510 = x2 * z
      t512 = 0.1D1 / (t510 - z - x2)
      t513 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, t506, -t509, 0.
     #0D0, 0.0D0, 0.0D0)
      t514 = t512 * t513
      t515 = t93 * t507
      t518 = log(-0.4D1 * t227 * t515)
      t520 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t506, -t509, 0.
     #0D0, 0.0D0, 0.0D0)
      t526 = pi * t512 * t520
      t533 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, t506, -t509, 0.
     #0D0, 0.0D0, 0.0D0)
      t534 = t512 * t533
      t537 = log(-0.4D1 * t245 * t515)
      t538 = t537 * t512
      t540 = t537 ** 2
      t552 = t175 * t526
      t559 = log(-0.4D1 * t220 * t515)
      t560 = t559 * t512
      t562 = t559 ** 2
      t578 = t25 * t507
      t581 = log(-0.4D1 * t303 * t578)
      t582 = t581 * t512
      t587 = t581 ** 2
      t588 = t587 * t512
      t591 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, s, t506, -t509, 0.
     #0D0, 0.0D0, 0.0D0)
      t612 = -(-0.90D2 * t139 * (-t514 + t518 * t512 * t520) - 0.180D3 *
     # t164 * t526) * t90 * t242 / 0.720D3 + (-0.90D2 * t139 * (t534 - t
     #538 * t513 + t540 * t512 * t520 / 0.2D1) + 0.180D3 * t164 * pi * (
     #t514 - t538 * t520) + t552) * t183 * t241 / 0.720D3 - (-0.90D2 * t
     #139 * (-t534 + t560 * t513 - t562 * t512 * t520 / 0.2D1) + 0.180D3
     # * t164 * pi * (-t514 + t560 * t520) - t552) * t90 * t241 / 0.1440
     #D4 + (t175 * pi * (t514 - t582 * t520) - 0.90D2 * t139 * (t588 * t
     #513 / 0.2D1 + t512 * t591 - t587 * t581 * t512 * t520 / 0.6D1 - t5
     #82 * t533) + t206 * t526 + 0.180D3 * t164 * pi * (t534 - t582 * t5
     #13 + t588 * t520 / 0.2D1)) * t241 / 0.1440D4
      t613 = FJET(XB1, XB2, s, 0.0D0, t506, 0.0D0, -t509, 0.0D0, t612)
      t615 = x2 * x3
      t618 = Sqrt(x3 * t507 * t31)
      t619 = t37 * t618
      t621 = 0.2D1 * t619 * x2
      t623 = 0.1D1 - x3 + t615
      t624 = 0.1D1 / t623
      t626 = t2 * (0.1D1 - x3 - x2 + t615 + t220 + t621) * t624
      t631 = t2 * x2 * (-0.1D1 + t615 + 0.2D1 * t619) * t624
      t633 = z * t219 * x3
      t634 = t615 * z
      t641 = 0.1D1 / (t633 - t510 - t634 + x3 - t220 + z + x2 - t621 - 0
     #.2D1 * t38 * t618 + 0.2D1 * t38 * t618 * x2)
      t642 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t631, t626, 0.
     #0D0, 0.0D0, 0.0D0)
      t643 = t641 * t642
      t644 = t623 ** 2
      t645 = 0.1D1 / t644
      t647 = t578 * t31 * t645
      t650 = log(0.4D1 * t462 * t647)
      t652 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t631, t626, 0.
     #0D0, 0.0D0, 0.0D0)
      t658 = pi * t641 * t652
      t665 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t631, t626, 0.
     #0D0, 0.0D0, 0.0D0)
      t670 = log(0.4D1 * t220 * t22 * t647)
      t671 = t670 * t641
      t673 = t670 ** 2
      t690 = -(-0.90D2 * t139 * (-t643 + t650 * t641 * t652) - 0.180D3 *
     # t164 * t658) * t90 * t242 / 0.720D3 - (-0.90D2 * t139 * (-t641 * 
     #t665 + t671 * t642 - t673 * t641 * t652 / 0.2D1) + 0.180D3 * t164 
     #* pi * (-t643 + t671 * t652) - t175 * t658) * t90 * t241 / 0.1440D
     #4
      t691 = FJET(XB1, XB2, s, 0.0D0, t626, 0.0D0, -t631, 0.0D0, t690)
      t693 = t1 * t342
      t695 = t508 * t693 * t345
      t696 = t505 * t693
      t698 = t349 * t507 * t351
      t699 = x1 * x2
      t700 = t699 * z
      t702 = 0.1D1 / (t700 + x2 - t699 + z - t510)
      t703 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t696, t695, t3
     #41, 0.0D0, t698)
      t704 = t702 * t703
      t706 = t357 * t345 * t507
      t709 = log(-0.4D1 * t462 * t706)
      t711 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t696, t695, t3
     #41, 0.0D0, t698)
      t717 = pi * t702 * t711
      t723 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t696, t695, t3
     #41, 0.0D0, t698)
      t727 = log(-0.4D1 * t479 * t706)
      t728 = t727 * t702
      t730 = t727 ** 2
      t747 = -(-0.90D2 * t139 * (-t704 + t709 * t702 * t711) - 0.180D3 *
     # t164 * t717) * t90 * t242 / 0.720D3 + (-0.90D2 * t139 * (t702 * t
     #723 - t728 * t703 + t730 * t702 * t711 / 0.2D1) + 0.180D3 * t164 *
     # pi * (t704 - t728 * t711) + t175 * t717) * t183 * t241 / 0.720D3
      t748 = FJET(XB1, XB2, s, 0.0D0, t695, t341, -t696, t698, t747)
      t750 = FJET(XB1, XB2, s, 0.0D0, -t509, 0.0D0, t506, 0.0D0, t612)
      t752 = FJET(XB1, XB2, s, 0.0D0, -t347, t341, 0.0D0, -t352, t502)
      t754 = FJET(XB1, XB2, s, 0.0D0, -t631, 0.0D0, t626, 0.0D0, t690)
      t756 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t334)
      t758 = FJET(XB1, XB2, s, t341, 0.0D0, 0.0D0, -t347, -t352, t502)
      t760 = t335 * t334 + t337 * t334 + t339 * t334 + t503 * t502 + t61
     #3 * t612 + t691 * t690 + t748 * t747 + t750 * t612 + t752 * t502 +
     # t754 * t690 + t756 * t334 + t758 * t502
      t761 = FJET(XB1, XB2, s, t341, -t696, 0.0D0, t695, t698, t747)
      t763 = FJET(XB1, XB2, s, t506, 0.0D0, -t509, 0.0D0, 0.0D0, t612)
      t765 = FJET(XB1, XB2, s, t626, 0.0D0, -t631, 0.0D0, 0.0D0, t690)
      t767 = FJET(XB1, XB2, s, t695, 0.0D0, -t696, t341, t698, t747)
      t770 = t341 * t615 * t624
      t771 = t2 * t342
      t772 = t343 * t220
      t773 = t507 * t31
      t775 = Sqrt(t374 * t773)
      t776 = t37 * t775
      t778 = 0.2D1 * t776 * x2
      t780 = x1 * t219 * x3
      t784 = t771 * (-x2 + t615 + t772 + t220 + 0.1D1 - x3 + t778 - t780
     #) * t345 * t624
      t788 = t31 * s * t1 * x1 * t624
      t794 = t771 * x2 * (-0.1D1 + t615 + x1 - t372 - t343 + t385 + 0.2D
     #1 * t776) * t345 * t624
      t801 = x2 * t140
      t809 = 0.2D1 * t38 * t775 * x1 * x2 + 0.2D1 * t699 - x3 - t633 + t
     #634 + t778 - t780 + t699 * t24 + 0.2D1 * t801 * z - t801 * t24 + t
     #141 * x2 - t372 * x2 + 0.2D1 * t38 * t775 + t220 - z - t801 + t380
      t823 = t382 - t384 - t386 + t510 + t343 - t141 + t373 - t379 - x2 
     #+ t772 + 0.2D1 * t372 * t510 - t372 * x2 * t24 - 0.2D1 * t141 * t5
     #10 + t383 * t615 - 0.2D1 * t776 * t699 - 0.2D1 * t38 * t775 * x2 -
     # 0.3D1 * t700
      t825 = 0.1D1 / (t809 + t823)
      t826 = t344 * t825
      t827 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, t794, -t784, -t
     #788, t770, t698)
      t833 = log(0.4D1 * t221 * t455 * t773 * t645)
      t835 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t794, -t784, -t
     #788, t770, t698)
      t838 = -t826 * t827 + t833 * t344 * t825 * t835
      t844 = 0.180D3 * t164 * pi * t826 * t835
      t845 = -0.90D2 * t139 * t838 - t844
      t848 = t845 * t90 * t242 / 0.720D3
      t849 = FJET(XB1, XB2, s, t770, -t784, -t788, t794, t698, -t848)
      t852 = t90 * t183 * t241
      t855 = FJET(XB1, XB2, s, t794, -t788, -t784, t770, t698, -t848)
      t859 = FJET(XB1, XB2, s, -t509, 0.0D0, t506, 0.0D0, 0.0D0, t612)
      t861 = FJET(XB1, XB2, s, -t347, 0.0D0, 0.0D0, t341, -t352, t502)
      t863 = FJET(XB1, XB2, s, -t696, t341, t695, 0.0D0, t698, t747)
      t865 = FJET(XB1, XB2, s, -t631, 0.0D0, t626, 0.0D0, 0.0D0, t690)
      t870 = -0.90D2 * t139 * t838 - t844
      t873 = t870 * t90 * t242 / 0.720D3
      t874 = FJET(XB1, XB2, s, -t788, t794, t770, -t784, t698, -t873)
      t878 = FJET(XB1, XB2, s, -t784, t770, t794, -t788, t698, -t873)
      t882 = t761 * t747 + t763 * t612 + t765 * t690 + t767 * t747 - t84
     #9 * t845 * t852 / 0.720D3 - t855 * t845 * t852 / 0.720D3 + t859 * 
     #t612 + t861 * t502 + t863 * t747 + t865 * t690 - t874 * t870 * t85
     #2 / 0.720D3 - t878 * t870 * t852 / 0.720D3
      rrgg2gght6s1e1 = t760 + t882

      end function



      doubleprecision function rrgg2gght6s1e0
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = t16 ** 2
      t18 = 0.1D1 / z
      t20 = -0.1D1 + x3
      t21 = 0.1D1 / t20
      t25 = log(-0.4D1 * t11 * t13 * t21)
      t26 = t25 ** 2
      t27 = cos(t8)
      t28 = z * t27
      t30 = Sqrt(-x3 * t20)
      t34 = 0.1D1 / (-x3 - z + 0.2D1 * t28 * t30)
      t43 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t54 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t56 = lh ** 2
      t58 = pi ** 2
      t60 = -0.180D3 * t56 + 0.30D2 * t58
      t68 = 0.1D1 / x3
      t71 = t13 * t10
      t73 = log(0.4D1 * t71)
      t75 = t73 ** 2
      t90 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t102 = t18 * pi
      t105 = t6 * pi
      t106 = x2 ** 2
      t107 = t106 * x3
      t108 = t71 * t21
      t111 = log(-0.4D1 * t107 * t108)
      t117 = log(0.4D1 * t107 * t71)
      t124 = t6 * lh
      t127 = -t3 * t34 - t3 * t18
      t130 = 0.180D3 * t124 * pi * t127
      t133 = 0.1D1 / x2
      t136 = t106 * t10
      t139 = log(0.4D1 * t136 * t13)
      t141 = t139 ** 2
      t154 = t60 * t6
      t156 = pi * t3 * t18
      t157 = t154 * t156
      t161 = x1 ** 2
      t162 = x3 * t161
      t165 = log(-0.4D1 * t162 * t108)
      t171 = log(0.4D1 * t162 * t71)
      t180 = 0.1D1 / x1
      t185 = t68 * t180 * t133
      t188 = t106 * t161
      t191 = log(0.4D1 * t188 * t71)
      t203 = t6 * t18
      t204 = t161 * t10
      t207 = log(0.4D1 * t204 * t13)
      t209 = t207 ** 2
      t224 = (-0.90D2 * t3 * t6 * pi * (t17 * t18 / 0.2D1 + t26 * t34 / 
     #0.2D1) + (0.180D3 * t3 * lh - 0.90D2 * t43) * t6 * pi * (-t16 * t1
     #8 - t25 * t34) + (0.180D3 * t43 * lh - 0.90D2 * t54 + t3 * t60) * 
     #t6 * pi * (t18 + t34)) * t68 / 0.2880D4 + (0.180D3 * (t54 - t73 * 
     #t43 + t75 * t3 / 0.2D1) * lh + t3 * (-0.60D2 * lh * t58 + 0.240D3 
     #* zeta3 + 0.120D3 * t56 * lh) - 0.45D2 * t75 * t43 - 0.90D2 * t90 
     #+ 0.15D2 * t75 * t73 * t3 + 0.90D2 * t73 * t54 + (t43 - t73 * t3) 
     #* t60) * t6 * t102 / 0.2880D4 - (-0.90D2 * t105 * (-(t43 - t111 * 
     #t3) * t34 - (t43 - t117 * t3) * t18) + t130) * t68 * t133 / 0.1440
     #D4 + (-0.90D2 * t105 * (t54 - t139 * t43 + t141 * t3 / 0.2D1) * t1
     #8 + 0.180D3 * t124 * pi * (t43 - t139 * t3) * t18 + t157) * t133 /
     # 0.1440D4 - (-0.90D2 * t105 * (-(t43 - t165 * t3) * t34 - (t43 - t
     #171 * t3) * t18) + t130) * t68 * t180 / 0.1440D4 + t105 * t127 * t
     #185 / 0.8D1 + (-0.90D2 * t105 * (t43 - t191 * t3) * t18 + 0.180D3 
     #* t124 * t156) * t180 * t133 / 0.720D3 - (-0.90D2 * t203 * pi * (-
     #t54 + t207 * t43 - t209 * t3 / 0.2D1) + 0.180D3 * t124 * t102 * (-
     #t43 + t207 * t3) - t157) * t180 / 0.1440D4
      t225 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t224)
      t227 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t224)
      t229 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t224)
      t231 = t2 * x1
      t232 = -0.1D1 + x1
      t233 = x1 * z
      t234 = 0.1D1 - x1 + t233
      t235 = 0.1D1 / t234
      t237 = t2 * t232 * t235
      t238 = t1 ** 2
      t239 = s * t238
      t241 = x1 * t232 * t235
      t242 = t239 * t241
      t243 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t237, t
     #231, 0.0D0, -t242)
      t245 = t162 * t10
      t246 = t232 ** 2
      t247 = t13 * t246
      t252 = log(-0.4D1 * t245 * t247 * t235 * t21)
      t254 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t237, t
     #231, 0.0D0, -t242)
      t257 = x3 * x1
      t258 = 0.2D1 * t257
      t259 = x3 * t234
      t261 = Sqrt(-t259 * t20)
      t264 = x1 * t12
      t265 = t257 * t12
      t267 = 0.2D1 * t162 * z
      t268 = t161 * t12
      t269 = t268 * x3
      t270 = t257 * z
      t271 = 0.3D1 * t270
      t272 = -z - t162 + t258 - x3 + t233 + 0.2D1 * t28 * t261 - t264 + 
     #t265 + t267 - t269 - t271
      t273 = 0.1D1 / t272
      t275 = t18 * t243
      t276 = t247 * t235
      t279 = log(0.4D1 * t245 * t276)
      t288 = t18 * t254 + t234 * t254 * t273
      t299 = t188 * t10
      t302 = log(0.4D1 * t299 * t276)
      t308 = t102 * t254
      t315 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t237, t
     #231, 0.0D0, -t242)
      t318 = log(0.4D1 * t204 * t276)
      t320 = t318 ** 2
      t336 = -(-0.90D2 * t105 * ((t234 * t243 - t252 * t234 * t254) * t2
     #73 + t275 - t279 * t18 * t254) + 0.180D3 * t124 * pi * t288) * t68
     # * t180 / 0.1440D4 + t105 * t288 * t185 / 0.8D1 + (-0.90D2 * t105 
     #* (-t275 + t302 * t18 * t254) - 0.180D3 * t124 * t308) * t180 * t1
     #33 / 0.720D3 - (-0.90D2 * t203 * pi * (t315 - t318 * t243 + t320 *
     # t254 / 0.2D1) + 0.180D3 * t124 * t102 * (t243 - t318 * t254) + t1
     #54 * t308) * t180 / 0.1440D4
      t337 = FJET(XB1, XB2, s, 0.0D0, t231, -t237, 0.0D0, -t242, t336)
      t339 = x2 * s
      t340 = t339 * t1
      t341 = -0.1D1 + x2
      t342 = t341 * s
      t343 = t1 * t342
      t344 = x2 * z
      t346 = 0.1D1 / (t344 - z - x2)
      t347 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t348 = t346 * t347
      t349 = t71 * t341
      t352 = log(-0.4D1 * t107 * t349)
      t354 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t360 = pi * t346 * t354
      t362 = 0.180D3 * t124 * t360
      t367 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t369 = t13 * t341
      t372 = log(-0.4D1 * t136 * t369)
      t373 = t372 * t346
      t375 = t372 ** 2
      t393 = t180 * t133
      t399 = log(-0.4D1 * t188 * t349)
      t409 = -(-0.90D2 * t105 * (-t348 + t352 * t346 * t354) - t362) * t
     #68 * t133 / 0.1440D4 + (-0.90D2 * t105 * (t346 * t367 - t373 * t34
     #7 + t375 * t346 * t354 / 0.2D1) + 0.180D3 * t124 * pi * (t348 - t3
     #73 * t354) + t154 * t360) * t133 / 0.1440D4 - t105 * t346 * t354 *
     # t68 * t393 / 0.8D1 + (-0.90D2 * t105 * (t348 - t399 * t346 * t354
     #) + t362) * t180 * t133 / 0.720D3
      t410 = FJET(XB1, XB2, s, 0.0D0, t340, 0.0D0, -t343, 0.0D0, t409)
      t412 = x2 * x3
      t415 = Sqrt(x3 * t341 * t20)
      t416 = t27 * t415
      t418 = 0.2D1 * t416 * x2
      t420 = 0.1D1 - x3 + t412
      t421 = 0.1D1 / t420
      t423 = t2 * (0.1D1 - x3 - x2 + t412 + t107 + t418) * t421
      t428 = t2 * x2 * (-0.1D1 + t412 + 0.2D1 * t416) * t421
      t430 = z * t106 * x3
      t431 = t412 * z
      t438 = 0.1D1 / (t430 - t344 - t431 + x3 - t107 + z + x2 - t418 - 0
     #.2D1 * t28 * t415 + 0.2D1 * t28 * t415 * x2)
      t439 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t428, t423, 0.
     #0D0, 0.0D0, 0.0D0)
      t442 = t420 ** 2
      t448 = log(0.4D1 * t107 * t10 * t369 * t20 / t442)
      t450 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t428, t423, 0.
     #0D0, 0.0D0, 0.0D0)
      t468 = -(-0.90D2 * t105 * (-t438 * t439 + t448 * t438 * t450) - 0.
     #180D3 * t124 * pi * t438 * t450) * t68 * t133 / 0.1440D4 - t105 * 
     #t438 * t450 * t68 * t393 / 0.8D1
      t469 = FJET(XB1, XB2, s, 0.0D0, t423, 0.0D0, -t428, 0.0D0, t468)
      t471 = t1 * t232
      t473 = t342 * t471 * t235
      t474 = t339 * t471
      t476 = t239 * t341 * t241
      t477 = x1 * x2
      t478 = t477 * z
      t480 = 0.1D1 / (t478 + x2 - t477 + z - t344)
      t482 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t474, t473, t2
     #31, 0.0D0, t476)
      t487 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t474, t473, t2
     #31, 0.0D0, t476)
      t494 = log(-0.4D1 * t299 * t13 * t235 * t246 * t341)
      t508 = -t105 * t480 * t482 * t68 * t393 / 0.8D1 + (-0.90D2 * t105 
     #* (t480 * t487 - t494 * t480 * t482) + 0.180D3 * t124 * pi * t480 
     #* t482) * t180 * t133 / 0.720D3
      t509 = FJET(XB1, XB2, s, 0.0D0, t473, t231, -t474, t476, t508)
      t511 = FJET(XB1, XB2, s, 0.0D0, -t343, 0.0D0, t340, 0.0D0, t409)
      t513 = FJET(XB1, XB2, s, 0.0D0, -t237, t231, 0.0D0, -t242, t336)
      t515 = FJET(XB1, XB2, s, 0.0D0, -t428, 0.0D0, t423, 0.0D0, t468)
      t517 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t224)
      t519 = FJET(XB1, XB2, s, t231, 0.0D0, 0.0D0, -t237, -t242, t336)
      t521 = t225 * t224 + t227 * t224 + t229 * t224 + t337 * t336 + t41
     #0 * t409 + t469 * t468 + t509 * t508 + t511 * t409 + t513 * t336 +
     # t515 * t468 + t517 * t224 + t519 * t336
      t522 = FJET(XB1, XB2, s, t231, -t474, 0.0D0, t473, t476, t508)
      t524 = FJET(XB1, XB2, s, t340, 0.0D0, -t343, 0.0D0, 0.0D0, t409)
      t526 = FJET(XB1, XB2, s, t423, 0.0D0, -t428, 0.0D0, 0.0D0, t468)
      t528 = FJET(XB1, XB2, s, t473, 0.0D0, -t474, t231, t476, t508)
      t531 = t231 * t412 * t421
      t532 = t2 * t232
      t533 = t233 * t107
      t536 = Sqrt(t259 * t341 * t20)
      t537 = t27 * t536
      t539 = 0.2D1 * t537 * x2
      t541 = x1 * t106 * x3
      t545 = t532 * (-x2 + t412 + t533 + t107 + 0.1D1 - x3 + t539 - t541
     #) * t235 * t421
      t549 = t20 * s * t1 * x1 * t421
      t555 = t532 * x2 * (-0.1D1 + t412 + x1 - t257 - t233 + t270 + 0.2D
     #1 * t537) * t235 * t421
      t558 = x2 * t161
      t561 = t233 - t162 + t258 - t264 + t344 + t107 + t265 + t267 - t26
     #9 - t271 - t430 + t431 - 0.3D1 * t478 + t539 - t541 + t477 * t12 +
     # 0.2D1 * t558 * z
      t584 = -t558 * t12 + t162 * x2 - t257 * x2 + 0.2D1 * t28 * t536 + 
     #0.2D1 * t28 * t536 * x1 * x2 - x3 - z + t533 + 0.2D1 * t257 * t344
     # - t257 * x2 * t12 - 0.2D1 * t162 * t344 + t268 * t412 - 0.2D1 * t
     #537 * t477 - 0.2D1 * t28 * t536 * x2 - x2 - t558 + 0.2D1 * t477
      t586 = 0.1D1 / (t561 + t584)
      t589 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t555, -t545, -t
     #549, t531, t476)
      t593 = t105 * t234 * t586 * t589 * t68 * t393 / 0.8D1
      t594 = FJET(XB1, XB2, s, t531, -t545, -t549, t555, t476, -t593)
      t596 = pi * t234
      t599 = t586 * t589 * t185
      t602 = FJET(XB1, XB2, s, t555, -t549, -t545, t531, t476, -t593)
      t607 = FJET(XB1, XB2, s, -t343, 0.0D0, t340, 0.0D0, 0.0D0, t409)
      t609 = FJET(XB1, XB2, s, -t237, 0.0D0, 0.0D0, t231, -t242, t336)
      t611 = FJET(XB1, XB2, s, -t474, t231, t473, 0.0D0, t476, t508)
      t613 = FJET(XB1, XB2, s, -t428, 0.0D0, t423, 0.0D0, 0.0D0, t468)
      t615 = FJET(XB1, XB2, s, -t549, t555, t531, -t545, t476, -t593)
      t620 = FJET(XB1, XB2, s, -t545, t531, t555, -t549, t476, -t593)
      t625 = t522 * t508 + t524 * t409 + t526 * t468 + t528 * t508 - t59
     #4 * t6 * t596 * t599 / 0.8D1 - t602 * t6 * t596 * t599 / 0.8D1 + t
     #607 * t409 + t609 * t336 + t611 * t508 + t613 * t468 - t615 * t6 *
     # t596 * t599 / 0.8D1 - t620 * t6 * t596 * t599 / 0.8D1
      rrgg2gght6s1e0 = t521 + t625

      end function



      doubleprecision function rrgg2gght6s1em1
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = 0.1D1 / z
      t19 = -0.1D1 + x3
      t24 = log(-0.4D1 * t11 * t13 / t19)
      t25 = cos(t8)
      t26 = z * t25
      t28 = Sqrt(-x3 * t19)
      t32 = 0.1D1 / (-x3 - z + 0.2D1 * t26 * t28)
      t40 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t48 = 0.1D1 / x3
      t53 = log(0.4D1 * t13 * t10)
      t58 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t62 = t53 ** 2
      t65 = lh ** 2
      t67 = pi ** 2
      t73 = t17 * pi
      t76 = t6 * pi
      t80 = (-t3 * t32 - t3 * t17) * t48
      t81 = 0.1D1 / x2
      t85 = x2 ** 2
      t86 = t85 * t10
      t89 = log(0.4D1 * t86 * t13)
      t95 = t6 * lh
      t99 = 0.180D3 * t95 * pi * t3 * t17
      t104 = 0.1D1 / x1
      t109 = t6 * t17
      t110 = x1 ** 2
      t111 = t110 * t10
      t114 = log(0.4D1 * t111 * t13)
      t126 = (-0.90D2 * t3 * t6 * pi * (-t16 * t17 - t24 * t32) + (0.180
     #D3 * t3 * lh - 0.90D2 * t40) * t6 * pi * (t17 + t32)) * t48 / 0.28
     #80D4 + (0.180D3 * (t40 - t53 * t3) * lh - 0.90D2 * t58 + 0.90D2 * 
     #t53 * t40 - 0.45D2 * t62 * t3 + t3 * (-0.180D3 * t65 + 0.30D2 * t6
     #7)) * t6 * t73 / 0.2880D4 + t76 * t80 * t81 / 0.16D2 + (-0.90D2 * 
     #t76 * (t40 - t89 * t3) * t17 + t99) * t81 / 0.1440D4 - t76 * t3 * 
     #t17 * t104 * t81 / 0.8D1 - (-0.90D2 * t109 * pi * (-t40 + t114 * t
     #3) - t99) * t104 / 0.1440D4 + t76 * t80 * t104 / 0.16D2
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t126)
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t126)
      t131 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t126)
      t133 = t2 * x1
      t134 = -0.1D1 + x1
      t135 = x1 * z
      t136 = 0.1D1 - x1 + t135
      t137 = 0.1D1 / t136
      t139 = t2 * t134 * t137
      t140 = t1 ** 2
      t141 = s * t140
      t143 = x1 * t134 * t137
      t144 = t141 * t143
      t146 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t139, t
     #133, 0.0D0, -t144)
      t151 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t139, t
     #133, 0.0D0, -t144)
      t153 = t134 ** 2
      t157 = log(0.4D1 * t111 * t13 * t137 * t153)
      t171 = x3 * t110
      t172 = x3 * x1
      t176 = Sqrt(-x3 * t136 * t19)
      t187 = -z - t171 + 0.2D1 * t172 - x3 + t135 + 0.2D1 * t26 * t176 -
     # x1 * t12 + t172 * t12 + 0.2D1 * t171 * z - t110 * t12 * x3 - 0.3D
     #1 * t172 * z
      t195 = t76 * t17 * t146 * t104 * t81 / 0.8D1 - (-0.90D2 * t109 * p
     #i * (t151 - t157 * t146) + 0.180D3 * t95 * t73 * t146) * t104 / 0.
     #1440D4 + t76 * (t17 * t146 + t136 * t146 / t187) * t48 * t104 / 0.
     #16D2
      t196 = FJET(XB1, XB2, s, 0.0D0, t133, -t139, 0.0D0, -t144, t195)
      t198 = x2 * s
      t199 = t198 * t1
      t200 = -0.1D1 + x2
      t201 = t200 * s
      t202 = t201 * t1
      t203 = x2 * z
      t205 = 0.1D1 / (t203 - z - x2)
      t206 = t76 * t205
      t207 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t199, -t202, 0.
     #0D0, 0.0D0, 0.0D0)
      t212 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, t199, -t202, 0.
     #0D0, 0.0D0, 0.0D0)
      t217 = log(-0.4D1 * t86 * t13 * t200)
      t234 = -t206 * t207 * t48 * t81 / 0.16D2 + (-0.90D2 * t76 * (t205 
     #* t212 - t217 * t205 * t207) + 0.180D3 * t95 * pi * t205 * t207) *
     # t81 / 0.1440D4 - t206 * t207 * t104 * t81 / 0.8D1
      t235 = FJET(XB1, XB2, s, 0.0D0, t199, 0.0D0, -t202, 0.0D0, t234)
      t237 = x2 * x3
      t238 = t85 * x3
      t241 = Sqrt(x3 * t200 * t19)
      t242 = t25 * t241
      t244 = 0.2D1 * t242 * x2
      t247 = 0.1D1 / (0.1D1 - x3 + t237)
      t249 = t2 * (0.1D1 - x3 - x2 + t237 + t238 + t244) * t247
      t254 = t2 * x2 * (-0.1D1 + t237 + 0.2D1 * t242) * t247
      t264 = 0.1D1 / (z * t85 * x3 - t203 - t237 * z + x3 - t238 + z + x
     #2 - t244 - 0.2D1 * t26 * t241 + 0.2D1 * t26 * t241 * x2)
      t266 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t254, t249, 0.
     #0D0, 0.0D0, 0.0D0)
      t270 = t76 * t264 * t266 * t48 * t81 / 0.16D2
      t271 = FJET(XB1, XB2, s, 0.0D0, t249, 0.0D0, -t254, 0.0D0, -t270)
      t276 = t264 * t266 * t48 * t81
      t279 = t1 * t134
      t281 = t201 * t279 * t137
      t282 = t198 * t279
      t284 = t141 * t200 * t143
      t285 = x1 * x2
      t288 = 0.1D1 / (t285 * z + x2 - t285 + z - t203)
      t290 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t282, t281, t1
     #33, 0.0D0, t284)
      t294 = t76 * t288 * t290 * t104 * t81 / 0.8D1
      t295 = FJET(XB1, XB2, s, 0.0D0, t281, t133, -t282, t284, -t294)
      t300 = t288 * t290 * t104 * t81
      t303 = FJET(XB1, XB2, s, 0.0D0, -t202, 0.0D0, t199, 0.0D0, t234)
      t305 = FJET(XB1, XB2, s, 0.0D0, -t139, t133, 0.0D0, -t144, t195)
      t307 = FJET(XB1, XB2, s, 0.0D0, -t254, 0.0D0, t249, 0.0D0, -t270)
      t312 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t126)
      t314 = FJET(XB1, XB2, s, t133, 0.0D0, 0.0D0, -t139, -t144, t195)
      t316 = FJET(XB1, XB2, s, t133, -t282, 0.0D0, t281, t284, -t294)
      t321 = FJET(XB1, XB2, s, t199, 0.0D0, -t202, 0.0D0, 0.0D0, t234)
      t323 = FJET(XB1, XB2, s, t249, 0.0D0, -t254, 0.0D0, 0.0D0, -t270)
      t328 = FJET(XB1, XB2, s, t281, 0.0D0, -t282, t133, t284, -t294)
      t333 = FJET(XB1, XB2, s, -t202, 0.0D0, t199, 0.0D0, 0.0D0, t234)
      t335 = FJET(XB1, XB2, s, -t139, 0.0D0, 0.0D0, t133, -t144, t195)
      t337 = FJET(XB1, XB2, s, -t282, t133, t281, 0.0D0, t284, -t294)
      t342 = FJET(XB1, XB2, s, -t254, 0.0D0, t249, 0.0D0, 0.0D0, -t270)
      rrgg2gght6s1em1 = t127 * t126 + t129 * t126 + t131 * t126 + t196 *
     # t195 + t235 * t234 - t271 * t6 * pi * t276 / 0.16D2 - t295 * t6 *
     # pi * t300 / 0.8D1 + t303 * t234 + t305 * t195 - t307 * t6 * pi * 
     #t276 / 0.16D2 + t312 * t126 + t314 * t195 - t316 * t6 * pi * t300 
     #/ 0.8D1 + t321 * t234 - t323 * t6 * pi * t276 / 0.16D2 - t328 * t6
     # * pi * t300 / 0.8D1 + t333 * t234 + t335 * t195 - t337 * t6 * pi 
     #* t300 / 0.8D1 - t342 * t6 * pi * t276 / 0.16D2

      end function



      doubleprecision function rrgg2gght6s1em2
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t8 = 0.1D1 / z
      t9 = x4 * pi
      t10 = cos(t9)
      t14 = Sqrt(-x3 * (-0.1D1 + x3))
      t25 = t6 * pi
      t26 = t3 * t8
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t37 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t39 = z ** 2
      t41 = Sin(t9)
      t42 = t41 ** 2
      t45 = log(0.4D1 / t39 * t42)
      t53 = -t3 * t6 * pi * (t8 + 0.1D1 / (-x3 - z + 0.2D1 * z * t10 * t
     #14)) / x3 / 0.32D2 - t25 * t26 * t27 / 0.16D2 - t25 * t26 * t31 / 
     #0.16D2 + (0.180D3 * t3 * lh - 0.90D2 * t37 + 0.90D2 * t45 * t3) * 
     #t6 * t8 * pi / 0.2880D4
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t53)
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t53)
      t58 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t53)
      t60 = t2 * x1
      t61 = -0.1D1 + x1
      t64 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t66 = t2 * t61 * t64
      t67 = t1 ** 2
      t71 = s * t67 * x1 * t61 * t64
      t72 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t66, t60
     #, 0.0D0, -t71)
      t74 = t8 * t72 * t31
      t76 = t25 * t74 / 0.16D2
      t77 = FJET(XB1, XB2, s, 0.0D0, t60, -t66, 0.0D0, -t71, t76)
      t83 = x2 * s * t1
      t86 = (-0.1D1 + x2) * s * t1
      t90 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t83, -t86, 0.0D0
     #, 0.0D0, 0.0D0)
      t92 = 0.1D1 / (x2 * z - z - x2) * t90 * t27
      t94 = t25 * t92 / 0.16D2
      t95 = FJET(XB1, XB2, s, 0.0D0, t83, 0.0D0, -t86, 0.0D0, -t94)
      t100 = FJET(XB1, XB2, s, 0.0D0, -t86, 0.0D0, t83, 0.0D0, -t94)
      t105 = FJET(XB1, XB2, s, 0.0D0, -t66, t60, 0.0D0, -t71, t76)
      t110 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t53)
      t112 = FJET(XB1, XB2, s, t60, 0.0D0, 0.0D0, -t66, -t71, t76)
      t117 = FJET(XB1, XB2, s, t83, 0.0D0, -t86, 0.0D0, 0.0D0, -t94)
      t122 = FJET(XB1, XB2, s, -t86, 0.0D0, t83, 0.0D0, 0.0D0, -t94)
      t127 = FJET(XB1, XB2, s, -t66, 0.0D0, 0.0D0, t60, -t71, t76)
      rrgg2gght6s1em2 = t54 * t53 + t56 * t53 + t58 * t53 + t77 * t6 * p
     #i * t74 / 0.16D2 - t95 * t6 * pi * t92 / 0.16D2 - t100 * t6 * pi *
     # t92 / 0.16D2 + t105 * t6 * pi * t74 / 0.16D2 + t110 * t53 + t112 
     #* t6 * pi * t74 / 0.16D2 - t117 * t6 * pi * t92 / 0.16D2 - t122 * 
     #t6 * pi * t92 / 0.16D2 + t127 * t6 * pi * t74 / 0.16D2

      end function



      doubleprecision function rrgg2gght6s1em3
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / z
      t11 = t5 * pi * t7 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = pi * t7 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2gght6s1em3 = -t12 * t5 * t15 / 0.32D2 - t17 * t5 * t15 / 0.32
     #D2 - t20 * t5 * t15 / 0.32D2 - t23 * t5 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2gght6s1em4
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght6s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght6s2e1
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = 0.1D1 / z
      t8 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = x4 * pi
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = t14 * t17
      t21 = log(0.4D1 * t11 * t18)
      t22 = t21 * t7
      t23 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t25 = t21 ** 2
      t27 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t30 = -0.1D1 + x3
      t31 = 0.1D1 / t30
      t32 = t18 * t31
      t35 = log(-0.4D1 * t11 * t32)
      t37 = t35 ** 2
      t41 = cos(t15)
      t42 = x3 * z
      t44 = Sqrt(-t42 * t30)
      t48 = 0.1D1 / (-z - x3 + 0.2D1 * t41 * t44)
      t53 = lh * t5
      t54 = t7 * t23
      t63 = lh ** 2
      t65 = pi ** 2
      t67 = -0.180D3 * t63 + 0.30D2 * t65
      t68 = t67 * t5
      t70 = t7 * t27
      t75 = 0.1D1 / x3
      t77 = 0.1D1 / x1
      t80 = t7 * pi
      t81 = t10 * t17
      t82 = t81 * t14
      t84 = log(0.4D1 * t82)
      t89 = t5 * t7
      t90 = t84 ** 2
      t93 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t107 = -0.60D2 * lh * t65 + 0.240D3 * zeta3 + 0.120D3 * t63 * lh
      t108 = t107 * t5
      t121 = x2 ** 2
      t122 = x3 * t121
      t125 = log(0.4D1 * t122 * t82)
      t128 = t122 * t10
      t129 = -0.1D1 + x2
      t130 = t18 * t129
      t133 = log(-0.4D1 * t128 * t130)
      t138 = log(-0.4D1 * t128 * t32)
      t145 = pi * t27
      t146 = t145 * t48
      t151 = 0.1D1 / x2
      t152 = t77 * t151
      t155 = t121 * t10
      t158 = log(0.4D1 * t155 * t18)
      t159 = t158 * t7
      t161 = t158 ** 2
      t167 = log(-0.4D1 * t155 * t130)
      t168 = t167 * t7
      t170 = t167 ** 2
      t189 = pi * t23
      t194 = x3 * t14
      t198 = log(-0.4D1 * t194 * t17 * t31)
      t202 = log(0.4D1 * t194 * t17)
      t206 = t202 ** 2
      t209 = t198 ** 2
      t242 = log(-0.4D1 * t122 * t130)
      t243 = t242 * t7
      t245 = t242 ** 2
      t251 = log(-0.4D1 * t122 * t32)
      t253 = t251 ** 2
      t260 = log(0.4D1 * t122 * t18)
      t261 = t260 * t7
      t263 = t260 ** 2
      t289 = t14 * t121
      t292 = log(0.4D1 * t289 * t17)
      t293 = t292 ** 2
      t294 = t17 * t129
      t297 = log(-0.4D1 * t289 * t294)
      t298 = t297 ** 2
      t323 = log(0.4D1 * t18)
      t324 = t323 ** 2
      t325 = t324 * t7
      t330 = t324 * t323 * t7
      t333 = t323 * t7
      t348 = t65 ** 2
      t349 = t63 ** 2
      t355 = rrgg2ggh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t364 = t324 ** 2
      t372 = (-0.90D2 * t6 * (t9 - t22 * t23 + t25 * t7 * t27 / 0.2D1 + 
     #(t8 - t35 * t23 + t37 * t27 / 0.2D1) * t48) + 0.180D3 * t53 * pi *
     # (t54 - t22 * t27 + (t23 - t35 * t27) * t48) + t68 * pi * (t27 * t
     #48 + t70)) * t75 * t77 / 0.1440D4 + (t68 * t80 * (t23 - t84 * t27)
     # - 0.90D2 * t89 * pi * (t90 * t23 / 0.2D1 + t93 - t90 * t84 * t27 
     #/ 0.6D1 - t84 * t8) + t108 * t80 * t27 + 0.180D3 * t53 * t80 * (t8
     # - t84 * t23 + t90 * t27 / 0.2D1)) * t77 / 0.1440D4 - (-0.90D2 * t
     #6 * (t125 * t7 * t27 - t133 * t7 * t27 - (t23 - t138 * t27) * t48)
     # - 0.180D3 * t53 * t146) * t75 * t152 / 0.720D3 + (-0.90D2 * t6 * 
     #(-t159 * t23 + t161 * t7 * t27 / 0.2D1 + t168 * t23 - t170 * t7 * 
     #t27 / 0.2D1) + 0.180D3 * t53 * pi * (-t159 * t27 + t168 * t27)) * 
     #t77 * t151 / 0.720D3 - ((-0.90D2 * t6 * t8 + 0.180D3 * t53 * t189 
     #+ t68 * t145) * (t198 * t48 + t202 * t7) - 0.90D2 * t6 * t27 * (t2
     #06 * t202 * t7 / 0.6D1 + t209 * t198 * t48 / 0.6D1) + (t68 * t189 
     #- 0.90D2 * t6 * t93 + t108 * t145 + 0.180D3 * t53 * pi * t8) * (-t
     #48 - t7) + (-0.90D2 * t6 * t23 + 0.180D3 * t53 * t145) * (-t209 * 
     #t48 / 0.2D1 - t206 * t7 / 0.2D1)) * t75 / 0.2880D4 + (-0.90D2 * t6
     # * (t243 * t23 - t245 * t7 * t27 / 0.2D1 + (t8 - t251 * t23 + t253
     # * t27 / 0.2D1) * t48 - t261 * t23 + t263 * t7 * t27 / 0.2D1) + 0.
     #180D3 * t53 * pi * (t243 * t27 + (t23 - t251 * t27) * t48 - t261 *
     # t27) + t68 * t146) * t75 * t151 / 0.1440D4 + ((0.180D3 * t70 * lh
     # - 0.90D2 * t54) * t5 * pi * (t293 / 0.2D1 - t298 / 0.2D1) - 0.90D
     #2 * t70 * t6 * (t298 * t297 / 0.6D1 - t293 * t292 / 0.6D1) + (0.18
     #0D3 * t54 * lh - 0.90D2 * t9 + t70 * t67) * t5 * pi * (-t292 + t29
     #7)) * t151 / 0.1440D4 + (0.180D3 * (t325 * t23 / 0.2D1 + t7 * t93 
     #- t330 * t27 / 0.6D1 - t333 * t8) * lh + (t9 - t333 * t23 + t325 *
     # t27 / 0.2D1) * t67 + (t54 - t333 * t27) * t107 + t70 * (-0.480D3 
     #* lh * zeta3 - t348 - 0.60D2 * t349 + 0.60D2 * t63 * t65) - 0.90D2
     # * t7 * t355 + 0.15D2 * t330 * t23 - 0.45D2 * t325 * t8 + 0.90D2 *
     # t333 * t93 - 0.15D2 / 0.4D1 * t364 * t7 * t27) * t5 * pi / 0.2880
     #D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t372)
      t375 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t372)
      t377 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t372)
      t379 = x2 * x3
      t380 = 0.1D1 - x3 + t379
      t381 = 0.1D1 / t380
      t382 = t379 * t381
      t383 = t2 * t382
      t385 = t2 * t30 * t381
      t386 = t129 * t30
      t388 = Sqrt(t42 * t386)
      t392 = 0.1D1 / (-z - x3 + t379 + 0.2D1 * t41 * t388)
      t393 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t385, t383, 0.0D0)
      t396 = t380 ** 2
      t397 = 0.1D1 / t396
      t398 = t30 * t397
      t402 = log(0.4D1 * t122 * t14 * t294 * t398)
      t403 = t402 * t392
      t404 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t385, t383, 0.0D0)
      t406 = t402 ** 2
      t408 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t385, t383, 0.0D0)
      t411 = -t392 * t393 + t403 * t404 - t406 * t392 * t408 / 0.2D1
      t414 = t392 * t404
      t416 = -t414 + t403 * t408
      t421 = pi * t392 * t408
      t422 = t68 * t421
      t427 = t122 * t81
      t432 = log(0.4D1 * t427 * t14 * t129 * t398)
      t443 = (-0.90D2 * t6 * (t414 - t432 * t392 * t408) + 0.180D3 * t53
     # * t421) * t75 * t152 / 0.720D3
      t444 = (-0.90D2 * t6 * t411 + 0.180D3 * t53 * pi * t416 - t422) * 
     #t75 * t151 / 0.1440D4 - t443
      t445 = FJET(XB1, XB2, s, 0.0D0, t383, 0.0D0, -t385, 0.0D0, t444)
      t448 = t1 * x1
      t449 = x1 * z
      t450 = -z - x1 + t449
      t451 = 0.1D1 / t450
      t453 = t129 * s * t448 * t451
      t454 = -0.1D1 + x1
      t455 = t2 * t454
      t457 = x2 * s * t448
      t458 = t1 ** 2
      t459 = s * t458
      t462 = x1 * t454 * t451
      t463 = t459 * t129 * t462
      t464 = x1 * x2
      t465 = t464 * z
      t467 = 0.1D1 / (-t464 - z + t465)
      t468 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, t457, t453, -t4
     #55, 0.0D0, -t463)
      t469 = t467 * t468
      t470 = 0.1D1 / t12
      t471 = t454 ** 2
      t472 = t470 * t471
      t474 = t472 * t451 * t129
      t477 = log(0.4D1 * t427 * t474)
      t479 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t457, t453, -t4
     #55, 0.0D0, -t463)
      t485 = pi * t467 * t479
      t491 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, t457, t453, -t4
     #55, 0.0D0, -t463)
      t493 = t155 * t17
      t496 = log(0.4D1 * t493 * t474)
      t497 = t496 * t467
      t499 = t496 ** 2
      t516 = -(-0.90D2 * t6 * (t469 - t477 * t467 * t479) + 0.180D3 * t5
     #3 * t485) * t75 * t152 / 0.720D3 + (-0.90D2 * t6 * (-t467 * t491 +
     # t497 * t468 - t499 * t467 * t479 / 0.2D1) + 0.180D3 * t53 * pi * 
     #(-t469 + t497 * t479) - t68 * t485) * t77 * t151 / 0.720D3
      t517 = FJET(XB1, XB2, s, 0.0D0, t453, -t455, t457, -t463, t516)
      t520 = t2 * x1 * t451
      t521 = t459 * t462
      t522 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t520, -
     #t455, 0.0D0, t521)
      t524 = t11 * t17
      t526 = t472 * t451 * t31
      t529 = log(0.4D1 * t524 * t526)
      t530 = t529 * t450
      t531 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t520, -
     #t455, 0.0D0, t521)
      t533 = t529 ** 2
      t535 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t520, -
     #t455, 0.0D0, t521)
      t539 = x3 * x1
      t540 = t539 * z
      t541 = x1 * t12
      t542 = t539 * t12
      t543 = t10 * t12
      t544 = t543 * x3
      t546 = 0.2D1 * t11 * z
      t547 = z * t41
      t548 = x3 * t450
      t550 = Sqrt(t548 * t30)
      t554 = 0.1D1 / (-t449 - t540 - t42 + t541 + t542 - t544 - t11 + t5
     #46 - t12 + 0.2D1 * t547 * t550)
      t556 = t7 * t522
      t557 = t472 * t451
      t560 = log(-0.4D1 * t524 * t557)
      t561 = t560 * t7
      t563 = t560 ** 2
      t570 = t450 * t531
      t574 = t7 * t531
      t583 = -t7 * t535 + t450 * t535 * t554
      t589 = (-0.90D2 * t6 * ((t450 * t522 - t530 * t531 + t533 * t450 *
     # t535 / 0.2D1) * t554 - t556 + t561 * t531 - t563 * t7 * t535 / 0.
     #2D1) + 0.180D3 * t53 * pi * ((t570 - t530 * t535) * t554 - t574 + 
     #t561 * t535) + t68 * pi * t583) * t75 * t77 / 0.1440D4
      t592 = log(-0.4D1 * t81 * t557)
      t594 = -t531 + t592 * t535
      t597 = t592 ** 2
      t600 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t520, -
     #t455, 0.0D0, t521)
      t605 = -t597 * t531 / 0.2D1 - t600 + t597 * t592 * t535 / 0.6D1 + 
     #t592 * t522
      t609 = t80 * t535
      t610 = t108 * t609
      t614 = -t522 + t592 * t531 - t597 * t535 / 0.2D1
      t623 = log(0.4D1 * t427 * t526)
      t629 = t471 * t451
      t633 = log(-0.4D1 * t128 * t17 * t470 * t629)
      t646 = (-0.90D2 * t6 * (-(t570 - t623 * t450 * t535) * t554 + t574
     # - t633 * t7 * t535) - 0.180D3 * t53 * pi * t583) * t75 * t152 / 0
     #.720D3
      t649 = log(-0.4D1 * t493 * t557)
      t650 = t649 * t7
      t652 = t649 ** 2
      t656 = -t556 + t650 * t531 - t652 * t7 * t535 / 0.2D1
      t660 = -t574 + t650 * t535
      t664 = t68 * t609
      t668 = (-0.90D2 * t6 * t656 + 0.180D3 * t53 * pi * t660 - t664) * 
     #t77 * t151 / 0.720D3
      t669 = t589 + (t68 * t80 * t594 - 0.90D2 * t89 * pi * t605 - t610 
     #+ 0.180D3 * t53 * t80 * t614) * t77 / 0.1440D4 - t646 + t668
      t670 = FJET(XB1, XB2, s, 0.0D0, -t455, -t520, 0.0D0, t521, t669)
      t672 = FJET(XB1, XB2, s, 0.0D0, -t520, -t455, 0.0D0, t521, t669)
      t674 = FJET(XB1, XB2, s, 0.0D0, -t385, 0.0D0, t383, 0.0D0, t444)
      t676 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t372)
      t678 = FJET(XB1, XB2, s, t457, -t455, t453, 0.0D0, -t463, t516)
      t691 = (-0.90D2 * t6 * t411 + 0.180D3 * t53 * pi * t416 - t422) * 
     #t75 * t151 / 0.1440D4 - t443
      t692 = FJET(XB1, XB2, s, t383, 0.0D0, -t385, 0.0D0, 0.0D0, t691)
      t694 = FJET(XB1, XB2, s, t453, 0.0D0, t457, -t455, -t463, t516)
      t699 = t30 * s * t1 * t454 * t381
      t700 = t2 * x1
      t702 = Sqrt(-t548 * t386)
      t703 = t41 * t702
      t709 = t700 * x2 * (-x3 + t379 - z + t42 - x1 + t539 + t449 - t540
     # + 0.2D1 * t703) * t451 * t381
      t710 = t455 * t382
      t711 = t449 * t122
      t715 = x1 * t121 * x3
      t716 = t703 * x2
      t721 = t700 * (-x2 + t379 - t711 + 0.1D1 - x3 + z * t121 * x3 + t7
     #15 + 0.2D1 * t716) * t451 * t381
      t722 = x2 * z
      t734 = t465 + t12 + t711 - 0.2D1 * t539 * t722 + t539 * x2 * t12 +
     # 0.2D1 * t11 * t722 - t543 * t379 - 0.2D1 * x1 * t41 * t702 * x2 +
     # t42 - t541 + t11 + t449 - t715
      t738 = x2 * t10
      t747 = -0.2D1 * t547 * t702 - t464 * t12 - 0.2D1 * t738 * z + t738
     # * t12 - t11 * x2 - t379 * z + t539 * x2 + 0.2D1 * t449 * t716 + t
     #738 + t540 - t542 + t544 - t546
      t749 = 0.1D1 / (t734 + t747)
      t750 = t450 * t749
      t751 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, t709, -t721, t6
     #99, -t710, -t463)
      t759 = log(-0.4D1 * t122 * t81 * t470 * t629 * t386 * t397)
      t761 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t709, -t721, t6
     #99, -t710, -t463)
      t764 = -t750 * t751 + t759 * t450 * t749 * t761
      t770 = 0.180D3 * t53 * pi * t750 * t761
      t771 = -0.90D2 * t6 * t764 - t770
      t774 = t771 * t75 * t152 / 0.720D3
      t775 = FJET(XB1, XB2, s, t699, t709, -t710, -t721, -t463, -t774)
      t778 = t75 * t77 * t151
      t781 = FJET(XB1, XB2, s, t709, t699, -t721, -t710, -t463, -t774)
      t798 = (t68 * t80 * t594 - 0.90D2 * t89 * pi * t605 - t610 + 0.180
     #D3 * t53 * t80 * t614) * t77 / 0.1440D4
      t799 = t589 + t798 - t646 + t668
      t800 = FJET(XB1, XB2, s, -t455, 0.0D0, 0.0D0, -t520, t521, t799)
      t802 = FJET(XB1, XB2, s, -t455, t457, 0.0D0, t453, -t463, t516)
      t815 = t589 + t798 - t646 + (-0.90D2 * t6 * t656 + 0.180D3 * t53 *
     # pi * t660 - t664) * t77 * t151 / 0.720D3
      t816 = FJET(XB1, XB2, s, -t520, 0.0D0, 0.0D0, -t455, t521, t815)
      t818 = FJET(XB1, XB2, s, -t385, 0.0D0, t383, 0.0D0, 0.0D0, t691)
      t823 = -0.90D2 * t6 * t764 - t770
      t826 = t823 * t75 * t152 / 0.720D3
      t827 = FJET(XB1, XB2, s, -t721, -t710, t709, t699, -t463, -t826)
      t831 = FJET(XB1, XB2, s, -t710, -t721, t699, t709, -t463, -t826)
      rrgg2gght6s2e1 = t373 * t372 + t375 * t372 + t377 * t372 + t445 * 
     #t444 + t517 * t516 + t670 * t669 + t672 * t669 + t674 * t444 + t67
     #6 * t372 + t678 * t516 + t692 * t691 + t694 * t516 - t775 * t771 *
     # t778 / 0.720D3 - t781 * t771 * t778 / 0.720D3 + t800 * t799 + t80
     #2 * t516 + t816 * t815 + t818 * t691 - t827 * t823 * t778 / 0.720D
     #3 - t831 * t823 * t778 / 0.720D3

      end function



      doubleprecision function rrgg2gght6s2e0
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = 0.1D1 / z
      t8 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = x4 * pi
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = t14 * t17
      t21 = log(0.4D1 * t11 * t18)
      t23 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t25 = -0.1D1 + x3
      t26 = 0.1D1 / t25
      t27 = t18 * t26
      t30 = log(-0.4D1 * t11 * t27)
      t33 = cos(t15)
      t34 = x3 * z
      t36 = Sqrt(-t34 * t25)
      t40 = 0.1D1 / (-z - x3 + 0.2D1 * t33 * t36)
      t45 = lh * t5
      t47 = t7 * t23
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t60 = 0.1D1 / x2
      t61 = t55 * t60
      t65 = x2 ** 2
      t66 = t65 * t10
      t69 = log(0.4D1 * t66 * t18)
      t72 = -0.1D1 + x2
      t73 = t18 * t72
      t76 = log(-0.4D1 * t66 * t73)
      t84 = t5 * t7
      t85 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t86 = t10 * t17
      t89 = log(0.4D1 * t86 * t14)
      t91 = t89 ** 2
      t98 = t7 * pi
      t104 = lh ** 2
      t106 = pi ** 2
      t108 = -0.180D3 * t104 + 0.30D2 * t106
      t109 = t108 * t5
      t115 = x3 * t14
      t119 = log(-0.4D1 * t115 * t17 * t26)
      t120 = t119 ** 2
      t124 = log(0.4D1 * t115 * t17)
      t125 = t124 ** 2
      t134 = pi * t23
      t156 = log(0.4D1 * t18)
      t157 = t156 * t7
      t159 = t156 ** 2
      t160 = t159 * t7
      t175 = rrgg2ggh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t191 = x3 * t65
      t194 = log(-0.4D1 * t191 * t73)
      t199 = log(-0.4D1 * t191 * t27)
      t205 = log(0.4D1 * t191 * t18)
      t218 = t14 * t65
      t221 = log(0.4D1 * t218 * t17)
      t222 = t221 ** 2
      t223 = t17 * t72
      t226 = log(-0.4D1 * t218 * t223)
      t227 = t226 ** 2
      t244 = (-0.90D2 * t6 * (t9 - t21 * t7 * t23 + (t8 - t30 * t23) * t
     #40) + 0.180D3 * t45 * pi * (t23 * t40 + t47)) * t53 * t55 / 0.1440
     #D4 - t6 * t23 * t40 * t53 * t61 / 0.8D1 - t6 * (-t69 * t7 * t23 + 
     #t76 * t7 * t23) * t55 * t60 / 0.8D1 + (-0.90D2 * t84 * pi * (t85 -
     # t89 * t8 + t91 * t23 / 0.2D1) + 0.180D3 * t45 * t98 * (t8 - t89 *
     # t23) + t109 * t98 * t23) * t55 / 0.1440D4 - (-0.90D2 * t6 * t23 *
     # (-t120 * t40 / 0.2D1 - t125 * t7 / 0.2D1) + (-0.90D2 * t6 * t8 + 
     #0.180D3 * t45 * t134) * (t119 * t40 + t124 * t7) + (-0.90D2 * t6 *
     # t85 + 0.180D3 * t45 * pi * t8 + t109 * t134) * (-t40 - t7)) * t53
     # / 0.2880D4 + (0.180D3 * (t7 * t85 - t157 * t8 + t160 * t23 / 0.2D
     #1) * lh + t47 * (-0.60D2 * lh * t106 + 0.240D3 * zeta3 + 0.120D3 *
     # t104 * lh) - 0.45D2 * t160 * t8 - 0.90D2 * t7 * t175 + 0.15D2 * t
     #159 * t156 * t7 * t23 + 0.90D2 * t157 * t85 + (t9 - t157 * t23) * 
     #t108) * t5 * pi / 0.2880D4 + (-0.90D2 * t6 * (t194 * t7 * t23 + (t
     #8 - t199 * t23) * t40 - t205 * t7 * t23) + 0.180D3 * t45 * t134 * 
     #t40) * t53 * t60 / 0.1440D4 + (-0.90D2 * t47 * t6 * (t222 / 0.2D1 
     #- t227 / 0.2D1) + (0.180D3 * t47 * lh - 0.90D2 * t9) * t5 * pi * (
     #-t221 + t226)) * t60 / 0.1440D4
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t244)
      t247 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t244)
      t249 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t244)
      t251 = x2 * x3
      t252 = 0.1D1 - x3 + t251
      t253 = 0.1D1 / t252
      t254 = t251 * t253
      t255 = t2 * t254
      t257 = t2 * t25 * t253
      t258 = t72 * t25
      t260 = Sqrt(t34 * t258)
      t264 = 0.1D1 / (-z - x3 + t251 + 0.2D1 * t33 * t260)
      t266 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t257, t255, 0.0D0)
      t270 = t6 * t264 * t266 * t53 * t61 / 0.8D1
      t271 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t257, t255, 0.0D0)
      t274 = t252 ** 2
      t280 = log(0.4D1 * t191 * t14 * t223 * t25 / t274)
      t283 = -t264 * t271 + t280 * t264 * t266
      t289 = 0.180D3 * t45 * pi * t264 * t266
      t294 = t270 + (-0.90D2 * t6 * t283 - t289) * t53 * t60 / 0.1440D4
      t295 = FJET(XB1, XB2, s, 0.0D0, t255, 0.0D0, -t257, 0.0D0, t294)
      t298 = t1 * x1
      t299 = x1 * z
      t300 = -z - x1 + t299
      t301 = 0.1D1 / t300
      t303 = t72 * s * t298 * t301
      t304 = -0.1D1 + x1
      t305 = t2 * t304
      t307 = x2 * s * t298
      t308 = t1 ** 2
      t309 = s * t308
      t312 = x1 * t304 * t301
      t313 = t309 * t72 * t312
      t314 = x1 * x2
      t315 = t314 * z
      t317 = 0.1D1 / (-t314 - z + t315)
      t319 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t307, t303, -t3
     #05, 0.0D0, -t313)
      t324 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, t307, t303, -t3
     #05, 0.0D0, -t313)
      t326 = t66 * t17
      t327 = 0.1D1 / t12
      t329 = t304 ** 2
      t334 = log(0.4D1 * t326 * t327 * t301 * t329 * t72)
      t348 = t6 * t317 * t319 * t53 * t61 / 0.8D1 + (-0.90D2 * t6 * (-t3
     #17 * t324 + t334 * t317 * t319) - 0.180D3 * t45 * pi * t317 * t319
     #) * t55 * t60 / 0.720D3
      t349 = FJET(XB1, XB2, s, 0.0D0, t303, -t305, t307, -t313, t348)
      t352 = t2 * x1 * t301
      t353 = t309 * t312
      t354 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t352, -
     #t305, 0.0D0, t353)
      t356 = t11 * t17
      t357 = t327 * t329
      t362 = log(0.4D1 * t356 * t357 * t301 * t26)
      t364 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t352, -
     #t305, 0.0D0, t353)
      t367 = x3 * x1
      t368 = t367 * z
      t369 = x1 * t12
      t370 = t367 * t12
      t371 = t10 * t12
      t372 = t371 * x3
      t374 = 0.2D1 * t11 * z
      t375 = z * t33
      t376 = x3 * t300
      t378 = Sqrt(t376 * t25)
      t382 = 0.1D1 / (-t299 - t368 - t34 + t369 + t370 - t372 - t11 + t3
     #74 - t12 + 0.2D1 * t375 * t378)
      t384 = t7 * t354
      t385 = t357 * t301
      t388 = log(-0.4D1 * t356 * t385)
      t397 = -t7 * t364 + t300 * t364 * t382
      t404 = (-0.90D2 * t6 * ((t300 * t354 - t362 * t300 * t364) * t382 
     #- t384 + t388 * t7 * t364) + 0.180D3 * t45 * pi * t397) * t53 * t5
     #5 / 0.1440D4
      t408 = t53 * t55 * t60
      t410 = -t6 * t397 * t408 / 0.8D1
      t413 = log(-0.4D1 * t326 * t385)
      t416 = -t384 + t413 * t7 * t364
      t419 = t98 * t364
      t421 = 0.180D3 * t45 * t419
      t425 = (-0.90D2 * t6 * t416 - t421) * t55 * t60 / 0.720D3
      t426 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t352, -
     #t305, 0.0D0, t353)
      t429 = log(-0.4D1 * t86 * t385)
      t431 = t429 ** 2
      t434 = -t426 + t429 * t354 - t431 * t364 / 0.2D1
      t439 = -t354 + t429 * t364
      t443 = t109 * t419
      t447 = t404 + t410 + t425 + (-0.90D2 * t84 * pi * t434 + 0.180D3 *
     # t45 * t98 * t439 - t443) * t55 / 0.1440D4
      t448 = FJET(XB1, XB2, s, 0.0D0, -t305, -t352, 0.0D0, t353, t447)
      t450 = FJET(XB1, XB2, s, 0.0D0, -t352, -t305, 0.0D0, t353, t447)
      t452 = FJET(XB1, XB2, s, 0.0D0, -t257, 0.0D0, t255, 0.0D0, t294)
      t454 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t244)
      t456 = FJET(XB1, XB2, s, t307, -t305, t303, 0.0D0, -t313, t348)
      t465 = t270 + (-0.90D2 * t6 * t283 - t289) * t53 * t60 / 0.1440D4
      t466 = FJET(XB1, XB2, s, t255, 0.0D0, -t257, 0.0D0, 0.0D0, t465)
      t468 = FJET(XB1, XB2, s, t303, 0.0D0, t307, -t305, -t313, t348)
      t473 = t25 * s * t1 * t304 * t253
      t474 = t2 * x1
      t476 = Sqrt(-t376 * t258)
      t477 = t33 * t476
      t483 = t474 * x2 * (-x3 + t251 - z + t34 - x1 + t367 + t299 - t368
     # + 0.2D1 * t477) * t301 * t253
      t484 = t305 * t254
      t485 = t299 * t191
      t489 = x1 * t65 * x3
      t490 = t477 * x2
      t495 = t474 * (-x2 + t251 - t485 + 0.1D1 - x3 + z * t65 * x3 + t48
     #9 + 0.2D1 * t490) * t301 * t253
      t496 = x2 * z
      t510 = x2 * t10
      t511 = t299 + t34 - t369 + t11 + t485 - 0.2D1 * t367 * t496 + t367
     # * x2 * t12 + 0.2D1 * t11 * t496 - t371 * t251 - 0.2D1 * x1 * t33 
     #* t476 * x2 + 0.2D1 * t299 * t490 + t510 + t12
      t521 = -t489 - 0.2D1 * t375 * t476 - t314 * t12 - 0.2D1 * t510 * z
     # + t510 * t12 - t11 * x2 - t251 * z + t367 * x2 + t315 + t368 - t3
     #70 + t372 - t374
      t523 = 0.1D1 / (t511 + t521)
      t526 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t483, -t495, t4
     #73, -t484, -t313)
      t530 = t6 * t300 * t523 * t526 * t53 * t61 / 0.8D1
      t531 = FJET(XB1, XB2, s, t473, t483, -t484, -t495, -t313, -t530)
      t533 = pi * t300
      t536 = t523 * t526 * t408
      t539 = FJET(XB1, XB2, s, t483, t473, -t495, -t484, -t313, -t530)
      t554 = (-0.90D2 * t84 * pi * t434 + 0.180D3 * t45 * t98 * t439 - t
     #443) * t55 / 0.1440D4
      t555 = t404 + t410 + t425 + t554
      t556 = FJET(XB1, XB2, s, -t305, 0.0D0, 0.0D0, -t352, t353, t555)
      t558 = FJET(XB1, XB2, s, -t305, t307, 0.0D0, t303, -t313, t348)
      t567 = t404 + t410 + (-0.90D2 * t6 * t416 - t421) * t55 * t60 / 0.
     #720D3 + t554
      t568 = FJET(XB1, XB2, s, -t352, 0.0D0, 0.0D0, -t305, t353, t567)
      t570 = FJET(XB1, XB2, s, -t257, 0.0D0, t255, 0.0D0, 0.0D0, t465)
      t572 = FJET(XB1, XB2, s, -t495, -t484, t483, t473, -t313, -t530)
      t577 = FJET(XB1, XB2, s, -t484, -t495, t473, t483, -t313, -t530)
      rrgg2gght6s2e0 = t245 * t244 + t247 * t244 + t249 * t244 + t295 * 
     #t294 + t349 * t348 + t448 * t447 + t450 * t447 + t452 * t294 + t45
     #4 * t244 + t456 * t348 + t466 * t465 + t468 * t348 - t531 * t5 * t
     #533 * t536 / 0.8D1 - t539 * t5 * t533 * t536 / 0.8D1 + t556 * t555
     # + t558 * t348 + t568 * t567 + t570 * t465 - t572 * t5 * t533 * t5
     #36 / 0.8D1 - t577 * t5 * t533 * t536 / 0.8D1

      end function



      doubleprecision function rrgg2gght6s2em1
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = -0.1D1 + x3
      t20 = log(-0.4D1 * t11 * t14 / t15)
      t21 = cos(t12)
      t22 = x3 * z
      t24 = Sqrt(-t22 * t15)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t24)
      t32 = log(0.4D1 * t11 * t14)
      t33 = 0.1D1 / z
      t39 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t42 = lh * t5
      t50 = 0.1D1 / x3
      t53 = t5 * t33
      t54 = x1 ** 2
      t55 = t54 * t14
      t58 = log(0.4D1 * t55 * t10)
      t64 = t33 * pi
      t69 = 0.1D1 / x1
      t73 = t33 * t7
      t81 = 0.1D1 / x2
      t86 = x2 ** 2
      t87 = t10 * t86
      t90 = log(0.4D1 * t87 * t14)
      t91 = -0.1D1 + x2
      t95 = log(-0.4D1 * t87 * t14 * t91)
      t104 = log(0.4D1 * t10 * t14)
      t105 = t104 * t33
      t110 = rrgg2ggh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t115 = t104 ** 2
      t119 = lh ** 2
      t121 = pi ** 2
      t129 = -(-0.90D2 * t6 * t7 * (t20 * t28 + t32 * t33) + (-0.90D2 * 
     #t6 * t39 + 0.180D3 * t42 * pi * t7) * (-t28 - t33)) * t50 / 0.2880
     #D4 + (-0.90D2 * t53 * pi * (t39 - t58 * t7) + 0.180D3 * t42 * t64 
     #* t7) * t69 / 0.1440D4 - t6 * (t7 * t28 + t73) * t50 * t69 / 0.16D
     #2 - t6 * t7 * t28 * t50 * t81 / 0.16D2 - t73 * t5 * pi * (-t90 + t
     #95) * t81 / 0.16D2 + (0.180D3 * (t33 * t39 - t105 * t7) * lh - 0.9
     #0D2 * t33 * t110 + 0.90D2 * t105 * t39 - 0.45D2 * t115 * t33 * t7 
     #+ t73 * (-0.180D3 * t119 + 0.30D2 * t121)) * t5 * pi / 0.2880D4
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t129)
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t129)
      t134 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t129)
      t136 = x2 * x3
      t138 = 0.1D1 / (0.1D1 - x3 + t136)
      t140 = t2 * t136 * t138
      t142 = t2 * t15 * t138
      t145 = Sqrt(t22 * t91 * t15)
      t149 = 0.1D1 / (-z - x3 + t136 + 0.2D1 * t21 * t145)
      t151 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t142, t140, 0.0D0)
      t155 = t6 * t149 * t151 * t50 * t81 / 0.16D2
      t156 = FJET(XB1, XB2, s, 0.0D0, t140, 0.0D0, -t142, 0.0D0, t155)
      t161 = t149 * t151 * t50 * t81
      t165 = t1 * x1
      t166 = x1 * z
      t167 = -z - x1 + t166
      t168 = 0.1D1 / t167
      t170 = t91 * s * t165 * t168
      t171 = -0.1D1 + x1
      t172 = t2 * t171
      t174 = x2 * s * t165
      t175 = t1 ** 2
      t176 = s * t175
      t179 = x1 * t171 * t168
      t180 = t176 * t91 * t179
      t181 = x1 * x2
      t184 = 0.1D1 / (-t181 - z + t181 * z)
      t186 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, t174, t170, -t1
     #72, 0.0D0, -t180)
      t190 = t6 * t184 * t186 * t69 * t81 / 0.8D1
      t191 = FJET(XB1, XB2, s, 0.0D0, t170, -t172, t174, -t180, t190)
      t196 = t184 * t186 * t69 * t81
      t200 = t2 * x1 * t168
      t201 = t176 * t179
      t203 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t200, -
     #t172, 0.0D0, t201)
      t207 = t6 * t33 * t203 * t69 * t81 / 0.8D1
      t208 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t200, -
     #t172, 0.0D0, t201)
      t211 = t171 ** 2
      t215 = log(-0.4D1 * t55 / t8 * t168 * t211)
      t217 = -t208 + t215 * t203
      t223 = 0.180D3 * t42 * t64 * t203
      t229 = x3 * x1
      t235 = x3 * t54
      t241 = Sqrt(x3 * t167 * t15)
      t251 = t6 * (-t33 * t203 + t167 * t203 / (-t166 - t229 * z - t22 +
     # x1 * t8 + t229 * t8 - t54 * t8 * x3 - t235 + 0.2D1 * t235 * z - t
     #8 + 0.2D1 * z * t21 * t241)) * t50 * t69 / 0.16D2
      t252 = t207 + (-0.90D2 * t53 * pi * t217 - t223) * t69 / 0.1440D4 
     #- t251
      t253 = FJET(XB1, XB2, s, 0.0D0, -t172, -t200, 0.0D0, t201, t252)
      t255 = FJET(XB1, XB2, s, 0.0D0, -t200, -t172, 0.0D0, t201, t252)
      t257 = FJET(XB1, XB2, s, 0.0D0, -t142, 0.0D0, t140, 0.0D0, t155)
      t262 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t129)
      t264 = FJET(XB1, XB2, s, t174, -t172, t170, 0.0D0, -t180, t190)
      t269 = FJET(XB1, XB2, s, t140, 0.0D0, -t142, 0.0D0, 0.0D0, t155)
      t274 = FJET(XB1, XB2, s, t170, 0.0D0, t174, -t172, -t180, t190)
      t286 = t207 + (-0.90D2 * t53 * pi * t217 - t223) * t69 / 0.1440D4 
     #- t251
      t287 = FJET(XB1, XB2, s, -t172, 0.0D0, 0.0D0, -t200, t201, t286)
      t289 = FJET(XB1, XB2, s, -t172, t174, 0.0D0, t170, -t180, t190)
      t294 = FJET(XB1, XB2, s, -t200, 0.0D0, 0.0D0, -t172, t201, t286)
      t296 = FJET(XB1, XB2, s, -t142, 0.0D0, t140, 0.0D0, 0.0D0, t155)
      rrgg2gght6s2em1 = t130 * t129 + t132 * t129 + t134 * t129 + t156 *
     # t5 * pi * t161 / 0.16D2 + t191 * t5 * pi * t196 / 0.8D1 + t253 * 
     #t252 + t255 * t252 + t257 * t5 * pi * t161 / 0.16D2 + t262 * t129 
     #+ t264 * t5 * pi * t196 / 0.8D1 + t269 * t5 * pi * t161 / 0.16D2 +
     # t274 * t5 * pi * t196 / 0.8D1 + t287 * t286 + t289 * t5 * pi * t1
     #96 / 0.8D1 + t294 * t286 + t296 * t5 * pi * t161 / 0.16D2

      end function



      doubleprecision function rrgg2gght6s2em2
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

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
      t3 = 0.1D1 / z
      t4 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t5 = t3 * t4
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t8 * pi
      t10 = 0.1D1 / x1
      t14 = x4 * pi
      t15 = cos(t14)
      t19 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t32 = rrgg2ggh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t35 = z ** 2
      t38 = Sin(t14)
      t39 = t38 ** 2
      t42 = log(0.4D1 / t35 / z * t39)
      t50 = -t5 * t9 * t10 / 0.16D2 + t9 * t4 * (-0.1D1 / (-z - x3 + 0.2
     #D1 * t15 * t19) - t3) / x3 / 0.32D2 + (0.180D3 * lh * t5 - 0.90D2 
     #* t3 * t32 + 0.90D2 * t42 * t3 * t4) * t8 * pi / 0.2880D4
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t50)
      t55 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t50)
      t57 = -0.1D1 + x1
      t58 = t2 * t57
      t61 = 0.1D1 / (-z - x1 + x1 * z)
      t63 = t2 * x1 * t61
      t64 = t1 ** 2
      t68 = s * t64 * x1 * t57 * t61
      t69 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t63, -t5
     #8, 0.0D0, t68)
      t71 = t3 * t69 * t10
      t73 = t9 * t71 / 0.16D2
      t74 = FJET(XB1, XB2, s, 0.0D0, -t58, -t63, 0.0D0, t68, t73)
      t79 = FJET(XB1, XB2, s, 0.0D0, -t63, -t58, 0.0D0, t68, t73)
      t84 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t50)
      t86 = FJET(XB1, XB2, s, -t58, 0.0D0, 0.0D0, -t63, t68, t73)
      t91 = FJET(XB1, XB2, s, -t63, 0.0D0, 0.0D0, -t58, t68, t73)
      rrgg2gght6s2em2 = t51 * t50 + t53 * t50 + t55 * t50 + t74 * t8 * p
     #i * t71 / 0.16D2 + t79 * t8 * pi * t71 / 0.16D2 + t84 * t50 + t86 
     #* t8 * pi * t71 / 0.16D2 + t91 * t8 * pi * t71 / 0.16D2

      end function



      doubleprecision function rrgg2gght6s2em3
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = 0.1D1 / z
      t4 = rrgg2ggh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t11 = t3 * t4 * t8 * pi / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t4 * t8 * pi
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2gght6s2em3 = -t12 * t3 * t15 / 0.32D2 - t17 * t3 * t15 / 0.32
     #D2 - t20 * t3 * t15 / 0.32D2 - t23 * t3 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2gght6s2em4
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
      doubleprecision rrgg2ggh61J1
      doubleprecision rrgg2ggh61J2
      doubleprecision rrgg2ggh61J3
      doubleprecision rrgg2ggh61J4
      doubleprecision rrgg2ggh61J5
      doubleprecision rrgg2ggh61J6
      doubleprecision rrgg2ggh61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh61J1
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S14 ** 2
      t27 = 0.18D2 * t26
      t28 = S23 ** 2
      t29 = 0.18D2 * t28
      t30 = S14 * S23
      t31 = 0.36D2 * t30
      t32 = t27 + t29 - t31
      t36 = 0.216D3 * S24
      t48 = S24 * t9
      t54 = 0.1D1 + t3
      t57 = 0.1D1 + t48
      t62 = 0.36D2 * S23
      t63 = 0.36D2 * S14
      t64 = S13 * S14
      t65 = S13 * S23
      t67 = 0.36D2 * t64 - 0.36D2 * t65
      t71 = S14 * S24
      t72 = S23 * S24
      t74 = -0.36D2 * t71 + 0.36D2 * t72
      t97 = S13 * t26
      t98 = 0.18D2 * t97
      t99 = S13 * t28
      t101 = t64 * S23
      t108 = t26 * S24
      t110 = t30 * S24
      t112 = t28 * S24
      t113 = 0.18D2 * t112
      t125 = S13 ** 2
      t127 = 0.27D2 / 0.2D1 * t64
      t130 = S24 ** 2
      t132 = 0.27D2 / 0.2D1 * t72
      t138 = t125 * S23
      t141 = t125 * S14
      t142 = 0.9D1 * t141
      t143 = t125 * S13
      t148 = S14 * t130
      t151 = t130 * S23
      t152 = 0.9D1 * t151
      t154 = t130 * S24
      t159 = S12 ** 2
      t160 = 0.1D1 / t159
      t176 = 0.18D2 * S14
      t177 = 0.18D2 * S23
      t185 = 0.27D2 / 0.8D1 * t125
      t193 = 0.27D2 / 0.8D1 * t130
      t229 = 0.27D2 / 0.8D1 * t143
      t232 = 0.27D2 * t101
      t245 = 0.27D2 * t110
      t246 = 0.27D2 / 0.8D1 * t154
      t295 = 0.27D2 / 0.4D1 * t151
      t308 = 0.27D2 / 0.4D1 * t141
      t326 = S14 * t28
      t329 = t26 * S23
      t334 = t28 * S23
      t336 = t154 * S14
      t338 = t130 * t28
      t340 = t154 * S23
      t341 = 0.27D2 / 0.8D1 * t340
      t342 = t26 * S14
      t343 = S13 * t342
      t345 = t108 * S23
      t347 = t71 * t28
      t350 = 0.27D2 * t148 * S23
      t351 = t97 * S23
      t353 = t64 * t28
      t356 = 0.27D2 * t141 * S23
      t357 = t130 ** 2
      t358 = 0.9D1 / 0.8D1 * t357
      t359 = S24 * t334
      t361 = t130 * t26
      t363 = S13 * t334
      t365 = t143 * S14
      t367 = t125 * t26
      t369 = t143 * S23
      t371 = t125 * t28
      t373 = S24 * t342
      t375 = t125 ** 2
      t376 = 0.9D1 / 0.8D1 * t375
      t377 = -0.9D1 / 0.2D1 * t336 + 0.63D2 / 0.8D1 * t338 + t341 - 0.18
     #D2 * t343 + 0.135D3 / 0.2D1 * t345 - 0.117D3 / 0.2D1 * t347 - t350
     # + 0.135D3 / 0.2D1 * t351 - 0.117D3 / 0.2D1 * t353 - t356 + t358 +
     # 0.189D3 / 0.8D1 * t359 + 0.27D2 / 0.2D1 * t361 + 0.189D3 / 0.8D1 
     #* t363 - 0.9D1 / 0.2D1 * t365 + 0.27D2 / 0.2D1 * t367 + 0.27D2 / 0
     #.8D1 * t369 + 0.63D2 / 0.8D1 * t371 - 0.18D2 * t373 + t376
      t379 = 0.27D2 * t108 - 0.45D2 / 0.16D2 * t99 + 0.1845D4 / 0.16D2 *
     # t148 + 0.63D2 / 0.16D2 * t154 + 0.9D1 * t101 - 0.99D2 * t141 - 0.
     #171D3 / 0.8D1 * t97 - 0.36D2 * t326 - 0.603D3 / 0.16D2 * t151 + 0.
     #18D2 * t329 - 0.333D3 / 0.8D1 * t112 + 0.1197D4 / 0.16D2 * t110 - 
     #0.585D3 / 0.4D1 * t143 + 0.18D2 * t334 + t377 * t9
      t399 = 0.27D2 / 0.8D1 * t365
      t411 = 0.189D3 / 0.8D1 * t373 - t350 + 0.27D2 / 0.8D1 * t336 - 0.1
     #17D3 / 0.2D1 * t345 + 0.135D3 / 0.2D1 * t347 + t399 - 0.117D3 / 0.
     #2D1 * t351 + 0.135D3 / 0.2D1 * t353 - t356 - 0.9D1 / 0.2D1 * t340 
     #+ 0.63D2 / 0.8D1 * t367 - 0.9D1 / 0.2D1 * t369 + 0.27D2 / 0.2D1 * 
     #t371 - 0.18D2 * t363 + 0.189D3 / 0.8D1 * t343 + t376 + 0.27D2 / 0.
     #2D1 * t338 + t358 - 0.18D2 * t359 + 0.63D2 / 0.8D1 * t361
      t413 = -0.99D2 * t151 + 0.18D2 * t326 + 0.9D1 * t110 - 0.333D3 / 0
     #.8D1 * t97 + 0.1845D4 / 0.16D2 * t138 + 0.1197D4 / 0.16D2 * t101 -
     # 0.603D3 / 0.16D2 * t141 + 0.18D2 * t342 - 0.36D2 * t329 + 0.27D2 
     #* t99 + 0.63D2 / 0.16D2 * t143 - 0.585D3 / 0.4D1 * t154 - 0.45D2 /
     # 0.16D2 * t108 - 0.171D3 / 0.8D1 * t112 + t411 * t2
      t419 = ((0.18D2 - 0.9D1 / 0.8D1 * t48) * t4 + (0.18D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.189D3 / 0.8D1 * S24 + 0.189D3 / 0.8D1 * S13) * 
     #t23 * S34 + ((-0.45D2 / 0.16D2 * S13 - 0.333D3 / 0.8D1 * S24 + t17
     #6 + (t193 + 0.27D2 / 0.8D1 * t72 - 0.9D1 / 0.2D1 * t71) * t9) * t4
     # + (-0.45D2 / 0.16D2 * S24 + t177 - 0.333D3 / 0.8D1 * S13 + (-0.9D
     #1 / 0.2D1 * t65 + 0.27D2 / 0.8D1 * t64 + t185) * t2) * t6 - 0.63D2
     # / 0.8D1 * t72 + 0.63D2 / 0.8D1 * t125 + 0.63D2 / 0.8D1 * t130 - 0
     #.63D2 / 0.8D1 * t64) * t23 + ((0.1197D4 / 0.16D2 * t71 - 0.333D3 /
     # 0.4D1 * t72 - 0.603D3 / 0.16D2 * t130 - 0.45D2 * t64 + 0.333D3 / 
     #0.8D1 * t65 + (0.9D1 * t148 - t295 - t246 - 0.63D2 / 0.8D1 * t112 
     #- 0.27D2 / 0.2D1 * t108 + t245) * t9) * t4 + (-0.603D3 / 0.16D2 * 
     #t125 + 0.333D3 / 0.8D1 * t71 - 0.333D3 / 0.4D1 * t64 + 0.1197D4 / 
     #0.16D2 * t65 - 0.45D2 * t72 + (-0.63D2 / 0.8D1 * t97 - t229 - t308
     # + 0.9D1 * t138 - 0.27D2 / 0.2D1 * t99 + t232) * t2) * t6 - t308 +
     # 0.27D2 / 0.8D1 * t97 - t295 + 0.27D2 / 0.8D1 * t112 + t229 + t246
     #) * S34 + t379 * t4 + t413 * t6 + 0.27D2 / 0.8D1 * t367 + t376 + t
     #358 - t341 - 0.9D1 / 0.8D1 * t359 - 0.9D1 / 0.8D1 * t343 + 0.27D2 
     #/ 0.8D1 * t338 - t399
      t427 = t28 ** 2
      t430 = t26 ** 2
      t451 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.18D2 * t23 + (t25 + t32 * t9) * t
     #4 + (t36 + t32 * t2) * t6) * t13) * t15 * t17 + (((0.18D2 - 0.18D2
     # * t3) * t4 + (0.18D2 - 0.18D2 * t48) * t6) * S12 + (0.36D2 * t54 
     #* t4 + 0.36D2 * t57 * t6) * S34 + (t62 - t25 + t63 + t67 * t2) * t
     #4 + (-t36 + t62 + t63 + t74 * t9) * t6 + ((-0.18D2 * t54 * t4 - 0.
     #18D2 * t57 * t6) * t23 + ((t63 - t62 - t67 * t2) * t4 + (t62 - t63
     # - t74 * t9) * t6) * S34 + (-t29 + t31 + t27 - 0.216D3 * t64 + (-t
     #98 - 0.18D2 * t99 + 0.36D2 * t101) * t2) * t4 + (t29 - 0.216D3 * t
     #72 - t27 + t31 + (-0.18D2 * t108 + 0.36D2 * t110 - t113) * t9) * t
     #6) * t13 + ((0.9D1 / 0.2D1 * S13 * t4 + 0.9D1 / 0.2D1 * t7) * t23 
     #+ ((-0.9D1 * t65 - 0.9D1 / 0.2D1 * t125 + t127) * t4 + (-0.9D1 / 0
     #.2D1 * t130 + t132 - 0.9D1 * t71) * t6) * S34 + (t98 + 0.9D1 / 0.2
     #D1 * t138 + 0.9D1 / 0.2D1 * t99 + t142 + 0.9D1 * t143 - 0.27D2 / 0
     #.2D1 * t101) * t4 + (0.9D1 / 0.2D1 * t148 + 0.9D1 / 0.2D1 * t108 +
     # t113 + t152 - 0.27D2 / 0.2D1 * t110 + 0.9D1 * t154) * t6) * t160)
     # * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0.9D1 / 0.8D1 * S24 *
     # t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t159 + ((0.171D3 / 0.2D1
     # * S13 + t176 + t22 + t177 + 0.171D3 / 0.2D1 * S24) * S34 + (t176 
     #- 0.171D3 / 0.8D1 * S13 + 0.27D2 * S24 + (-0.9D1 / 0.2D1 * t64 + 0
     #.27D2 / 0.8D1 * t65 + t185) * t9) * t4 + (0.27D2 * S13 - 0.171D3 /
     # 0.8D1 * S24 + t177 + (0.27D2 / 0.8D1 * t71 + t193 - 0.9D1 / 0.2D1
     # * t72) * t2) * t6 + 0.27D2 / 0.2D1 * t125 - t127 - t132 + 0.27D2 
     #/ 0.2D1 * t130) * S12 + (-0.189D3 / 0.2D1 * S13 - t63 - 0.72D2 * S
     #34 - t62 - 0.189D3 / 0.2D1 * S24) * t23 + ((0.1197D4 / 0.16D2 * S2
     #4 - t63 + 0.9D1 * S13) * t4 + (0.1197D4 / 0.16D2 * S13 - t62 + 0.9
     #D1 * S24) * t6 + 0.27D2 * t72 - 0.27D2 * t125 - 0.27D2 * t130 + 0.
     #27D2 * t64) * S34 + (0.54D2 * t71 + t27 - 0.45D2 * t65 + 0.1845D4 
     #/ 0.16D2 * t130 + 0.1197D4 / 0.16D2 * t72 + 0.9D1 * t64 - 0.99D2 *
     # t125 - t31 + t29 + (-0.27D2 / 0.2D1 * t97 + t142 - t229 - 0.63D2 
     #/ 0.8D1 * t99 - 0.27D2 / 0.4D1 * t138 + t232) * t9) * t4 + (0.1845
     #D4 / 0.16D2 * t125 + t29 + t27 - 0.45D2 * t71 + 0.1197D4 / 0.16D2 
     #* t64 + 0.54D2 * t65 + 0.9D1 * t72 - 0.99D2 * t130 - t31 + (-0.27D
     #2 / 0.2D1 * t112 - 0.27D2 / 0.4D1 * t148 + t245 - t246 + t152 - 0.
     #63D2 / 0.8D1 * t108) * t2) * t6 + t152 - 0.9D1 / 0.2D1 * t143 - 0.
     #9D1 / 0.2D1 * t97 - 0.9D1 / 0.2D1 * t154 - 0.9D1 / 0.2D1 * t112 + 
     #t142 + t419 * t13 + ((-0.18D2 * t363 * S14 - 0.18D2 * t65 * t342 +
     # 0.27D2 * t99 * t26 + 0.9D1 * S13 * t427 + 0.9D1 * S13 * t430) * t
     #9 * t4 + (0.27D2 * t112 * t26 - 0.18D2 * t72 * t342 - 0.18D2 * t35
     #9 * S14 + 0.9D1 * S24 * t427 + 0.9D1 * S24 * t430) * t2 * t6) * t1
     #60
      rrgg2ggh61J1 = t451 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh61J2
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S14 ** 2
      t27 = 0.18D2 * t26
      t28 = S23 ** 2
      t29 = 0.18D2 * t28
      t30 = S14 * S23
      t31 = 0.36D2 * t30
      t32 = t27 + t29 - t31
      t36 = 0.216D3 * S24
      t48 = S24 * t9
      t62 = 0.27D2 * S23
      t63 = 0.36D2 * S14
      t64 = S13 * S14
      t65 = S13 * S23
      t67 = 0.36D2 * t64 - 0.36D2 * t65
      t71 = 0.36D2 * S23
      t72 = 0.27D2 * S14
      t73 = S14 * S24
      t74 = S23 * S24
      t76 = -0.36D2 * t73 + 0.36D2 * t74
      t101 = 0.45D2 * t30
      t103 = S13 * t26
      t104 = 0.18D2 * t103
      t105 = S13 * t28
      t107 = t64 * S23
      t115 = t26 * S24
      t117 = t30 * S24
      t119 = t28 * S24
      t120 = 0.18D2 * t119
      t131 = S13 ** 2
      t137 = S24 ** 2
      t144 = t131 * S13
      t146 = t131 * S23
      t148 = t131 * S14
      t149 = 0.9D1 * t148
      t152 = S14 * t137
      t154 = t137 * S23
      t155 = 0.9D1 * t154
      t156 = t137 * S24
      t162 = S12 ** 2
      t163 = 0.1D1 / t162
      t179 = 0.18D2 * S14
      t180 = 0.18D2 * S23
      t188 = 0.27D2 / 0.8D1 * t131
      t196 = 0.27D2 / 0.8D1 * t137
      t234 = 0.27D2 / 0.8D1 * t144
      t237 = 0.27D2 * t107
      t250 = 0.27D2 * t117
      t251 = 0.27D2 / 0.8D1 * t156
      t300 = 0.27D2 / 0.4D1 * t154
      t302 = 0.27D2 / 0.2D1 * t115
      t313 = 0.27D2 / 0.4D1 * t148
      t315 = 0.27D2 / 0.2D1 * t105
      t330 = S14 * t28
      t333 = t26 * S23
      t338 = t28 * S23
      t340 = t156 * S14
      t342 = t137 * t28
      t344 = t156 * S23
      t345 = 0.27D2 / 0.8D1 * t344
      t346 = t26 * S14
      t347 = S13 * t346
      t349 = t115 * S23
      t351 = t73 * t28
      t354 = 0.27D2 * t152 * S23
      t355 = t103 * S23
      t357 = t64 * t28
      t360 = 0.27D2 * t148 * S23
      t361 = t137 ** 2
      t362 = 0.9D1 / 0.8D1 * t361
      t363 = S24 * t338
      t365 = t137 * t26
      t367 = S13 * t338
      t369 = t144 * S14
      t371 = t131 * t26
      t373 = t144 * S23
      t375 = t131 * t28
      t377 = S24 * t346
      t379 = t131 ** 2
      t380 = 0.9D1 / 0.8D1 * t379
      t381 = -0.9D1 / 0.2D1 * t340 + 0.63D2 / 0.8D1 * t342 + t345 - 0.18
     #D2 * t347 + 0.135D3 / 0.2D1 * t349 - 0.117D3 / 0.2D1 * t351 - t354
     # + 0.135D3 / 0.2D1 * t355 - 0.117D3 / 0.2D1 * t357 - t360 + t362 +
     # 0.189D3 / 0.8D1 * t363 + 0.27D2 / 0.2D1 * t365 + 0.189D3 / 0.8D1 
     #* t367 - 0.9D1 / 0.2D1 * t369 + 0.27D2 / 0.2D1 * t371 + 0.27D2 / 0
     #.8D1 * t373 + 0.63D2 / 0.8D1 * t375 - 0.18D2 * t377 + t380
      t383 = -t302 - 0.45D2 / 0.16D2 * t105 + 0.693D3 / 0.4D1 * t152 - 0
     #.171D3 / 0.8D1 * t156 + 0.9D1 * t107 - 0.99D2 * t148 - 0.171D3 / 0
     #.8D1 * t103 - 0.36D2 * t330 - 0.657D3 / 0.8D1 * t154 + 0.18D2 * t3
     #33 - 0.243D3 / 0.4D1 * t119 + 0.531D3 / 0.4D1 * t117 - 0.585D3 / 0
     #.4D1 * t144 + 0.18D2 * t338 + t381 * t9
      t402 = 0.27D2 / 0.8D1 * t369
      t414 = 0.189D3 / 0.8D1 * t377 - t354 + 0.27D2 / 0.8D1 * t340 - 0.1
     #17D3 / 0.2D1 * t349 + 0.135D3 / 0.2D1 * t351 + t402 - 0.117D3 / 0.
     #2D1 * t355 + 0.135D3 / 0.2D1 * t357 - t360 - 0.9D1 / 0.2D1 * t344 
     #+ 0.63D2 / 0.8D1 * t371 - 0.9D1 / 0.2D1 * t373 + 0.27D2 / 0.2D1 * 
     #t375 - 0.18D2 * t367 + 0.189D3 / 0.8D1 * t347 + t380 + 0.27D2 / 0.
     #2D1 * t342 + t362 - 0.18D2 * t363 + 0.63D2 / 0.8D1 * t365
      t416 = -0.99D2 * t154 + 0.18D2 * t330 + 0.9D1 * t117 - 0.243D3 / 0
     #.4D1 * t103 + 0.693D3 / 0.4D1 * t146 + 0.531D3 / 0.4D1 * t107 - 0.
     #657D3 / 0.8D1 * t148 + 0.18D2 * t346 - 0.36D2 * t333 - t315 - 0.17
     #1D3 / 0.8D1 * t144 - 0.585D3 / 0.4D1 * t156 - 0.45D2 / 0.16D2 * t1
     #15 - 0.171D3 / 0.8D1 * t119 + t414 * t2
      t422 = ((0.18D2 - 0.9D1 / 0.8D1 * t48) * t4 + (0.18D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.189D3 / 0.8D1 * S24 + 0.189D3 / 0.8D1 * S13) * 
     #t23 * S34 + ((-0.45D2 / 0.16D2 * S13 - 0.243D3 / 0.4D1 * S24 + t17
     #9 + (t196 + 0.27D2 / 0.8D1 * t74 - 0.9D1 / 0.2D1 * t73) * t9) * t4
     # + (-0.45D2 / 0.16D2 * S24 + t180 - 0.243D3 / 0.4D1 * S13 + (-0.9D
     #1 / 0.2D1 * t65 + 0.27D2 / 0.8D1 * t64 + t188) * t2) * t6 - 0.63D2
     # / 0.8D1 * t74 + 0.63D2 / 0.8D1 * t131 + 0.63D2 / 0.8D1 * t137 - 0
     #.63D2 / 0.8D1 * t64) * t23 + ((0.531D3 / 0.4D1 * t73 - 0.243D3 / 0
     #.2D1 * t74 - 0.657D3 / 0.8D1 * t137 - 0.45D2 * t64 + 0.333D3 / 0.8
     #D1 * t65 + (0.9D1 * t152 - t300 - t251 - 0.63D2 / 0.8D1 * t119 - t
     #302 + t250) * t9) * t4 + (-0.657D3 / 0.8D1 * t131 + 0.333D3 / 0.8D
     #1 * t73 - 0.243D3 / 0.2D1 * t64 + 0.531D3 / 0.4D1 * t65 - 0.45D2 *
     # t74 + (-0.63D2 / 0.8D1 * t103 - t234 - t313 + 0.9D1 * t146 - t315
     # + t237) * t2) * t6 - t313 + 0.27D2 / 0.8D1 * t103 - t300 + 0.27D2
     # / 0.8D1 * t119 + t234 + t251) * S34 + t383 * t4 + t416 * t6 + 0.2
     #7D2 / 0.8D1 * t371 + t380 + t362 - t345 - 0.9D1 / 0.8D1 * t363 - 0
     #.9D1 / 0.8D1 * t347 + 0.27D2 / 0.8D1 * t342 - t402
      t430 = t28 ** 2
      t433 = t26 ** 2
      t454 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.18D2 * t23 + (t25 + t32 * t9) * t
     #4 + (t36 + t32 * t2) * t6) * t13) * t15 * t17 + (((0.18D2 - 0.18D2
     # * t3) * t4 + (0.18D2 - 0.18D2 * t48) * t6) * S12 + ((0.45D2 + 0.3
     #6D2 * t3) * t4 + (0.45D2 + 0.36D2 * t48) * t6) * S34 + (t62 - t25 
     #+ t63 + t67 * t2) * t4 + (-t36 + t71 + t72 + t76 * t9) * t6 + (((-
     #0.9D1 - 0.18D2 * t3) * t4 + (-0.9D1 - 0.18D2 * t48) * t6) * t23 + 
     #((t72 - 0.54D2 * S23 - t67 * t2) * t4 + (t62 - 0.54D2 * S14 - t76 
     #* t9) * t6) * S34 + (-0.9D1 * t28 + t101 + t27 - 0.216D3 * t64 + (
     #-t104 - 0.18D2 * t105 + 0.36D2 * t107) * t2) * t4 + (t29 - 0.216D3
     # * t74 - 0.9D1 * t26 + t101 + (-0.18D2 * t115 + 0.36D2 * t117 - t1
     #20) * t9) * t6) * t13 + ((0.9D1 / 0.2D1 * S13 * t4 + 0.9D1 / 0.2D1
     # * t7) * t23 + ((-0.9D1 / 0.2D1 * t131 - 0.9D1 * t65) * t4 + (-0.9
     #D1 * t73 - 0.9D1 / 0.2D1 * t137) * t6) * S34 + (0.9D1 / 0.2D1 * t1
     #05 + 0.9D1 * t144 + t104 + 0.9D1 / 0.2D1 * t146 + t149) * t4 + (0.
     #9D1 / 0.2D1 * t152 + t155 + t120 + 0.9D1 * t156 + 0.9D1 / 0.2D1 * 
     #t115) * t6) * t163) * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0.
     #9D1 / 0.8D1 * S24 * t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t162 
     #+ ((0.171D3 / 0.2D1 * S13 + t179 + t22 + t180 + 0.171D3 / 0.2D1 * 
     #S24) * S34 + (t179 - 0.171D3 / 0.8D1 * S13 - 0.27D2 / 0.2D1 * S24 
     #+ (-0.9D1 / 0.2D1 * t64 + 0.27D2 / 0.8D1 * t65 + t188) * t9) * t4 
     #+ (-0.27D2 / 0.2D1 * S13 - 0.171D3 / 0.8D1 * S24 + t180 + (0.27D2 
     #/ 0.8D1 * t73 + t196 - 0.9D1 / 0.2D1 * t74) * t2) * t6 + 0.27D2 / 
     #0.2D1 * t131 - 0.27D2 / 0.2D1 * t64 - 0.27D2 / 0.2D1 * t74 + 0.27D
     #2 / 0.2D1 * t137) * S12 + (-0.189D3 / 0.2D1 * S13 - t63 - 0.72D2 *
     # S34 - t71 - 0.189D3 / 0.2D1 * S24) * t23 + ((0.531D3 / 0.4D1 * S2
     #4 - t63 + 0.9D1 * S13) * t4 + (0.531D3 / 0.4D1 * S13 - t71 + 0.9D1
     # * S24) * t6 + 0.27D2 * t74 - 0.27D2 * t131 - 0.27D2 * t137 + 0.27
     #D2 * t64) * S34 + (-0.27D2 * t73 + t27 - 0.45D2 * t65 + 0.693D3 / 
     #0.4D1 * t137 + 0.531D3 / 0.4D1 * t74 + 0.9D1 * t64 - 0.99D2 * t131
     # - t31 + t29 + (-0.27D2 / 0.2D1 * t103 + t149 - t234 - 0.63D2 / 0.
     #8D1 * t105 - 0.27D2 / 0.4D1 * t146 + t237) * t9) * t4 + (0.693D3 /
     # 0.4D1 * t131 + t29 + t27 - 0.45D2 * t73 + 0.531D3 / 0.4D1 * t64 -
     # 0.27D2 * t65 + 0.9D1 * t74 - 0.99D2 * t137 - t31 + (-0.27D2 / 0.2
     #D1 * t119 - 0.27D2 / 0.4D1 * t152 + t250 - t251 + t155 - 0.63D2 / 
     #0.8D1 * t115) * t2) * t6 + t155 - 0.9D1 / 0.2D1 * t144 - 0.9D1 / 0
     #.2D1 * t103 - 0.9D1 / 0.2D1 * t156 - 0.9D1 / 0.2D1 * t119 + t149 +
     # t422 * t13 + ((-0.18D2 * t367 * S14 - 0.18D2 * t65 * t346 + 0.27D
     #2 * t105 * t26 + 0.9D1 * S13 * t430 + 0.9D1 * S13 * t433) * t9 * t
     #4 + (0.27D2 * t119 * t26 - 0.18D2 * t74 * t346 - 0.18D2 * t363 * S
     #14 + 0.9D1 * S24 * t430 + 0.9D1 * S24 * t433) * t2 * t6) * t163
      rrgg2ggh61J2 = t454 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh61J3
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S14 ** 2
      t27 = 0.18D2 * t26
      t28 = S23 ** 2
      t29 = 0.54D2 * t28
      t30 = S14 * S23
      t31 = 0.36D2 * t30
      t36 = 0.216D3 * S24
      t37 = 0.54D2 * t26
      t38 = 0.18D2 * t28
      t51 = S24 * t9
      t65 = 0.27D2 * S23
      t66 = 0.36D2 * S14
      t67 = S13 * S14
      t68 = S13 * S23
      t70 = 0.36D2 * t67 - 0.36D2 * t68
      t74 = 0.36D2 * S23
      t75 = 0.27D2 * S14
      t76 = S14 * S24
      t77 = S23 * S24
      t79 = -0.36D2 * t76 + 0.36D2 * t77
      t103 = 0.45D2 * t30
      t105 = S13 * t26
      t106 = 0.18D2 * t105
      t107 = S13 * t28
      t109 = t67 * S23
      t116 = t26 * S24
      t118 = t30 * S24
      t120 = t28 * S24
      t121 = 0.18D2 * t120
      t133 = S13 ** 2
      t135 = 0.27D2 / 0.2D1 * t67
      t138 = S24 ** 2
      t140 = 0.27D2 / 0.2D1 * t77
      t146 = t133 * S23
      t149 = t133 * S14
      t150 = 0.9D1 * t149
      t151 = t133 * S13
      t156 = S14 * t138
      t159 = t138 * S23
      t160 = 0.9D1 * t159
      t162 = t138 * S24
      t167 = S12 ** 2
      t168 = 0.1D1 / t167
      t179 = 0.18D2 * S24
      t180 = 0.18D2 * S13
      t184 = 0.18D2 * S14
      t185 = 0.18D2 * S23
      t193 = 0.27D2 / 0.8D1 * t133
      t201 = 0.27D2 / 0.8D1 * t138
      t235 = 0.27D2 / 0.8D1 * t151
      t238 = 0.27D2 * t109
      t251 = 0.27D2 * t118
      t252 = 0.27D2 / 0.8D1 * t162
      t303 = 0.27D2 / 0.4D1 * t159
      t316 = 0.27D2 / 0.4D1 * t149
      t334 = S14 * t28
      t337 = t26 * S23
      t342 = t28 * S23
      t344 = t162 * S14
      t346 = t138 * t28
      t348 = t162 * S23
      t349 = 0.27D2 / 0.8D1 * t348
      t350 = t26 * S14
      t351 = S13 * t350
      t353 = t116 * S23
      t355 = t76 * t28
      t358 = 0.27D2 * t156 * S23
      t359 = t105 * S23
      t361 = t67 * t28
      t364 = 0.27D2 * t149 * S23
      t365 = t138 ** 2
      t366 = 0.9D1 / 0.8D1 * t365
      t367 = S24 * t342
      t369 = t138 * t26
      t371 = S13 * t342
      t373 = t151 * S14
      t375 = t133 * t26
      t377 = t151 * S23
      t379 = t133 * t28
      t381 = S24 * t350
      t383 = t133 ** 2
      t384 = 0.9D1 / 0.8D1 * t383
      t385 = -0.9D1 / 0.2D1 * t344 + 0.63D2 / 0.8D1 * t346 + t349 - 0.18
     #D2 * t351 + 0.171D3 / 0.2D1 * t353 - 0.189D3 / 0.2D1 * t355 - t358
     # + 0.171D3 / 0.2D1 * t359 - 0.189D3 / 0.2D1 * t361 - t364 + t366 +
     # 0.333D3 / 0.8D1 * t367 + 0.27D2 / 0.2D1 * t369 + 0.333D3 / 0.8D1 
     #* t371 - 0.9D1 / 0.2D1 * t373 + 0.27D2 / 0.2D1 * t375 + 0.27D2 / 0
     #.8D1 * t377 + 0.63D2 / 0.8D1 * t379 - 0.18D2 * t381 + t384
      t387 = -0.54D2 * t116 + 0.243D3 / 0.16D2 * t107 + 0.4131D4 / 0.16D
     #2 * t156 - 0.27D2 / 0.16D2 * t162 + 0.18D2 * t109 - 0.99D2 * t149 
     #- 0.171D3 / 0.8D1 * t105 - 0.36D2 * t334 - 0.2169D4 / 0.16D2 * t15
     #9 + 0.18D2 * t337 - 0.927D3 / 0.8D1 * t120 + 0.3339D4 / 0.16D2 * t
     #118 - 0.585D3 / 0.4D1 * t151 + 0.54D2 * t342 + t385 * t9
      t407 = 0.27D2 / 0.8D1 * t373
      t419 = 0.333D3 / 0.8D1 * t381 - t358 + 0.27D2 / 0.8D1 * t344 - 0.1
     #89D3 / 0.2D1 * t353 + 0.171D3 / 0.2D1 * t355 + t407 - 0.189D3 / 0.
     #2D1 * t359 + 0.171D3 / 0.2D1 * t361 - t364 - 0.9D1 / 0.2D1 * t348 
     #+ 0.63D2 / 0.8D1 * t375 - 0.9D1 / 0.2D1 * t377 + 0.27D2 / 0.2D1 * 
     #t379 - 0.18D2 * t371 + 0.333D3 / 0.8D1 * t351 + t384 + 0.27D2 / 0.
     #2D1 * t346 + t366 - 0.18D2 * t367 + 0.63D2 / 0.8D1 * t369
      t421 = -0.99D2 * t159 + 0.18D2 * t334 + 0.18D2 * t118 - 0.927D3 / 
     #0.8D1 * t105 + 0.4131D4 / 0.16D2 * t146 + 0.3339D4 / 0.16D2 * t109
     # - 0.2169D4 / 0.16D2 * t149 + 0.54D2 * t350 - 0.36D2 * t337 - 0.54
     #D2 * t107 - 0.27D2 / 0.16D2 * t151 - 0.585D3 / 0.4D1 * t162 + 0.24
     #3D3 / 0.16D2 * t116 - 0.171D3 / 0.8D1 * t120 + t419 * t2
      t427 = ((0.54D2 - 0.9D1 / 0.8D1 * t51) * t4 + (0.54D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.333D3 / 0.8D1 * S24 + 0.333D3 / 0.8D1 * S13) * 
     #t23 * S34 + ((0.243D3 / 0.16D2 * S13 - 0.927D3 / 0.8D1 * S24 + 0.5
     #4D2 * S14 + (t201 + 0.27D2 / 0.8D1 * t77 - 0.9D1 / 0.2D1 * t76) * 
     #t9) * t4 + (0.243D3 / 0.16D2 * S24 + 0.54D2 * S23 - 0.927D3 / 0.8D
     #1 * S13 + (-0.9D1 / 0.2D1 * t68 + 0.27D2 / 0.8D1 * t67 + t193) * t
     #2) * t6 - 0.63D2 / 0.8D1 * t77 + 0.63D2 / 0.8D1 * t133 + 0.63D2 / 
     #0.8D1 * t138 - 0.63D2 / 0.8D1 * t67) * t23 + ((0.3627D4 / 0.16D2 *
     # t76 - 0.1215D4 / 0.4D1 * t77 - 0.2169D4 / 0.16D2 * t138 - 0.54D2 
     #* t67 + 0.621D3 / 0.8D1 * t68 + (0.9D1 * t156 - t303 - t252 - 0.63
     #D2 / 0.8D1 * t120 - 0.27D2 / 0.2D1 * t116 + t251) * t9) * t4 + (-0
     #.2169D4 / 0.16D2 * t133 + 0.621D3 / 0.8D1 * t76 - 0.1215D4 / 0.4D1
     # * t67 + 0.3627D4 / 0.16D2 * t68 - 0.54D2 * t77 + (-0.63D2 / 0.8D1
     # * t105 - t235 - t316 + 0.9D1 * t146 - 0.27D2 / 0.2D1 * t107 + t23
     #8) * t2) * t6 - t316 + 0.27D2 / 0.8D1 * t105 - t303 + 0.27D2 / 0.8
     #D1 * t120 + t235 + t252) * S34 + t387 * t4 + t421 * t6 + 0.27D2 / 
     #0.8D1 * t375 + t384 + t366 - t349 - 0.9D1 / 0.8D1 * t367 - 0.9D1 /
     # 0.8D1 * t351 + 0.27D2 / 0.8D1 * t346 - t407
      t435 = t28 ** 2
      t438 = t26 ** 2
      t459 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.54D2 * t23 + (t25 + (t27 + t29 - 
     #t31) * t9) * t4 + (t36 + (t37 - t31 + t38) * t2) * t6) * t13) * t1
     #5 * t17 + (((0.18D2 - 0.18D2 * t3) * t4 + (0.18D2 - 0.18D2 * t51) 
     #* t6) * S12 + ((0.45D2 + 0.36D2 * t3) * t4 + (0.45D2 + 0.36D2 * t5
     #1) * t6) * S34 + (t65 - t25 + t66 + t70 * t2) * t4 + (-t36 + t74 +
     # t75 + t79 * t9) * t6 + (((-0.54D2 - 0.18D2 * t3) * t4 + (-0.54D2 
     #- 0.18D2 * t51) * t6) * t23 + ((t75 - 0.108D3 * S23 - t70 * t2) * 
     #t4 + (t65 - 0.108D3 * S14 - t79 * t9) * t6) * S34 + (-t29 + t103 +
     # t27 - 0.216D3 * t67 + (-t106 - 0.18D2 * t107 + 0.36D2 * t109) * t
     #2) * t4 + (t38 - 0.216D3 * t77 - t37 + t103 + (-0.18D2 * t116 + 0.
     #36D2 * t118 - t121) * t9) * t6) * t13 + ((0.9D1 / 0.2D1 * S13 * t4
     # + 0.9D1 / 0.2D1 * t7) * t23 + ((-0.9D1 * t68 - 0.9D1 / 0.2D1 * t1
     #33 - t135) * t4 + (-0.9D1 / 0.2D1 * t138 - t140 - 0.9D1 * t76) * t
     #6) * S34 + (t106 + 0.9D1 / 0.2D1 * t146 + 0.9D1 / 0.2D1 * t107 + t
     #150 + 0.9D1 * t151 + 0.27D2 / 0.2D1 * t109) * t4 + (0.9D1 / 0.2D1 
     #* t156 + 0.9D1 / 0.2D1 * t116 + t121 + t160 + 0.27D2 / 0.2D1 * t11
     #8 + 0.9D1 * t162) * t6) * t168) * s * z + (-0.9D1 / 0.8D1 * S13 * 
     #t9 * t4 - 0.9D1 / 0.8D1 * S24 * t2 * t6 - t179 - t180) * t167 + ((
     #0.207D3 / 0.2D1 * S13 + t184 + t22 + t185 + 0.207D3 / 0.2D1 * S24)
     # * S34 + (t184 - 0.171D3 / 0.8D1 * S13 - 0.54D2 * S24 + (-0.9D1 / 
     #0.2D1 * t67 + 0.27D2 / 0.8D1 * t68 + t193) * t9) * t4 + (-0.54D2 *
     # S13 - 0.171D3 / 0.8D1 * S24 + t185 + (0.27D2 / 0.8D1 * t76 + t201
     # - 0.9D1 / 0.2D1 * t77) * t2) * t6 + 0.27D2 / 0.2D1 * t133 - t135 
     #- t140 + 0.27D2 / 0.2D1 * t138) * S12 + (-0.261D3 / 0.2D1 * S13 - 
     #t66 - 0.72D2 * S34 - t74 - 0.261D3 / 0.2D1 * S24) * t23 + ((0.3339
     #D4 / 0.16D2 * S24 - t66 + t180) * t4 + (0.3339D4 / 0.16D2 * S13 - 
     #t74 + t179) * t6 + 0.27D2 * t77 - 0.27D2 * t133 - 0.27D2 * t138 + 
     #0.27D2 * t67) * S34 + (-0.108D3 * t76 + t27 - 0.54D2 * t68 + 0.413
     #1D4 / 0.16D2 * t138 + 0.3627D4 / 0.16D2 * t77 + 0.9D1 * t67 - 0.99
     #D2 * t133 - t31 + t29 + (-0.27D2 / 0.2D1 * t105 + t150 - t235 - 0.
     #63D2 / 0.8D1 * t107 - 0.27D2 / 0.4D1 * t146 + t238) * t9) * t4 + (
     #0.4131D4 / 0.16D2 * t133 + t38 + t37 - 0.54D2 * t76 + 0.3627D4 / 0
     #.16D2 * t67 - 0.108D3 * t68 + 0.9D1 * t77 - 0.99D2 * t138 - t31 + 
     #(-0.27D2 / 0.2D1 * t120 - 0.27D2 / 0.4D1 * t156 + t251 - t252 + t1
     #60 - 0.63D2 / 0.8D1 * t116) * t2) * t6 + t160 - 0.9D1 / 0.2D1 * t1
     #51 - 0.9D1 / 0.2D1 * t105 - 0.9D1 / 0.2D1 * t162 - 0.9D1 / 0.2D1 *
     # t120 + t150 + t427 * t13 + ((-0.18D2 * t371 * S14 - 0.18D2 * t68 
     #* t350 + 0.27D2 * t107 * t26 + 0.9D1 * S13 * t435 + 0.9D1 * S13 * 
     #t438) * t9 * t4 + (0.27D2 * t120 * t26 - 0.18D2 * t77 * t350 - 0.1
     #8D2 * t367 * S14 + 0.9D1 * S24 * t435 + 0.9D1 * S24 * t438) * t2 *
     # t6) * t168
      rrgg2ggh61J3 = t459 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh61J4
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S14 ** 2
      t27 = 0.18D2 * t26
      t28 = S23 ** 2
      t29 = 0.90D2 * t28
      t30 = S14 * S23
      t31 = 0.36D2 * t30
      t36 = 0.216D3 * S24
      t37 = 0.90D2 * t26
      t38 = 0.18D2 * t28
      t51 = S24 * t9
      t65 = 0.27D2 * S23
      t66 = 0.36D2 * S14
      t67 = S13 * S14
      t68 = S13 * S23
      t70 = 0.36D2 * t67 - 0.36D2 * t68
      t74 = 0.36D2 * S23
      t75 = 0.27D2 * S14
      t76 = S14 * S24
      t77 = S23 * S24
      t79 = -0.36D2 * t76 + 0.36D2 * t77
      t104 = 0.45D2 * t30
      t106 = S13 * t26
      t107 = 0.18D2 * t106
      t108 = S13 * t28
      t110 = t67 * S23
      t118 = t26 * S24
      t120 = t30 * S24
      t122 = t28 * S24
      t123 = 0.18D2 * t122
      t135 = S13 ** 2
      t137 = 0.27D2 * t67
      t140 = S24 ** 2
      t142 = 0.27D2 * t77
      t148 = t135 * S23
      t151 = t135 * S14
      t152 = 0.9D1 * t151
      t153 = t135 * S13
      t155 = 0.27D2 * t110
      t158 = S14 * t140
      t161 = t140 * S23
      t162 = 0.9D1 * t161
      t163 = 0.27D2 * t120
      t164 = t140 * S24
      t169 = S12 ** 2
      t170 = 0.1D1 / t169
      t186 = 0.18D2 * S14
      t187 = 0.18D2 * S23
      t195 = 0.27D2 / 0.8D1 * t135
      t203 = 0.27D2 / 0.8D1 * t140
      t239 = 0.27D2 / 0.8D1 * t153
      t254 = 0.27D2 / 0.8D1 * t164
      t305 = 0.27D2 / 0.4D1 * t161
      t318 = 0.27D2 / 0.4D1 * t151
      t335 = S14 * t28
      t338 = t26 * S23
      t343 = t28 * S23
      t345 = t164 * S14
      t347 = t140 * t28
      t349 = t164 * S23
      t350 = 0.27D2 / 0.8D1 * t349
      t351 = t26 * S14
      t352 = S13 * t351
      t354 = t118 * S23
      t356 = t76 * t28
      t359 = 0.27D2 * t158 * S23
      t360 = t106 * S23
      t362 = t67 * t28
      t365 = 0.27D2 * t151 * S23
      t366 = t140 ** 2
      t367 = 0.9D1 / 0.8D1 * t366
      t368 = S24 * t343
      t370 = t140 * t26
      t372 = S13 * t343
      t374 = t153 * S14
      t376 = t135 * t26
      t378 = t153 * S23
      t380 = t135 * t28
      t382 = S24 * t351
      t384 = t135 ** 2
      t385 = 0.9D1 / 0.8D1 * t384
      t386 = -0.9D1 / 0.2D1 * t345 + 0.63D2 / 0.8D1 * t347 + t350 - 0.18
     #D2 * t352 + 0.207D3 / 0.2D1 * t354 - 0.261D3 / 0.2D1 * t356 - t359
     # + 0.207D3 / 0.2D1 * t360 - 0.261D3 / 0.2D1 * t362 - t365 + t367 +
     # 0.477D3 / 0.8D1 * t368 + 0.27D2 / 0.2D1 * t370 + 0.477D3 / 0.8D1 
     #* t372 - 0.9D1 / 0.2D1 * t374 + 0.27D2 / 0.2D1 * t376 + 0.27D2 / 0
     #.8D1 * t378 + 0.63D2 / 0.8D1 * t380 - 0.18D2 * t382 + t385
      t388 = -0.189D3 / 0.2D1 * t118 + 0.531D3 / 0.16D2 * t108 + 0.2745D
     #4 / 0.8D1 * t158 + 0.18D2 * t164 + t155 - 0.99D2 * t151 - 0.171D3 
     #/ 0.8D1 * t106 - 0.36D2 * t335 - 0.189D3 * t161 + 0.18D2 * t338 - 
     #0.171D3 * t122 + 0.2277D4 / 0.8D1 * t120 - 0.585D3 / 0.4D1 * t153 
     #+ 0.90D2 * t343 + t386 * t9
      t407 = 0.27D2 / 0.8D1 * t374
      t419 = 0.477D3 / 0.8D1 * t382 - t359 + 0.27D2 / 0.8D1 * t345 - 0.2
     #61D3 / 0.2D1 * t354 + 0.207D3 / 0.2D1 * t356 + t407 - 0.261D3 / 0.
     #2D1 * t360 + 0.207D3 / 0.2D1 * t362 - t365 - 0.9D1 / 0.2D1 * t349 
     #+ 0.63D2 / 0.8D1 * t376 - 0.9D1 / 0.2D1 * t378 + 0.27D2 / 0.2D1 * 
     #t380 - 0.18D2 * t372 + 0.477D3 / 0.8D1 * t352 + t385 + 0.27D2 / 0.
     #2D1 * t347 + t367 - 0.18D2 * t368 + 0.63D2 / 0.8D1 * t370
      t421 = -0.99D2 * t161 + 0.18D2 * t335 + t163 - 0.171D3 * t106 + 0.
     #2745D4 / 0.8D1 * t148 + 0.2277D4 / 0.8D1 * t110 - 0.189D3 * t151 +
     # 0.90D2 * t351 - 0.36D2 * t338 - 0.189D3 / 0.2D1 * t108 + 0.18D2 *
     # t153 - 0.585D3 / 0.4D1 * t164 + 0.531D3 / 0.16D2 * t118 - 0.171D3
     # / 0.8D1 * t122 + t419 * t2
      t427 = ((0.90D2 - 0.9D1 / 0.8D1 * t51) * t4 + (0.90D2 - 0.9D1 / 0.
     #8D1 * t3) * t6 + 0.477D3 / 0.8D1 * S24 + 0.477D3 / 0.8D1 * S13) * 
     #t23 * S34 + ((0.531D3 / 0.16D2 * S13 - 0.171D3 * S24 + 0.90D2 * S1
     #4 + (t203 + 0.27D2 / 0.8D1 * t77 - 0.9D1 / 0.2D1 * t76) * t9) * t4
     # + (0.531D3 / 0.16D2 * S24 + 0.90D2 * S23 - 0.171D3 * S13 + (-0.9D
     #1 / 0.2D1 * t68 + 0.27D2 / 0.8D1 * t67 + t195) * t2) * t6 - 0.63D2
     # / 0.8D1 * t77 + 0.63D2 / 0.8D1 * t135 + 0.63D2 / 0.8D1 * t140 - 0
     #.63D2 / 0.8D1 * t67) * t23 + ((0.2565D4 / 0.8D1 * t76 - 0.486D3 * 
     #t77 - 0.189D3 * t140 - 0.63D2 * t67 + 0.909D3 / 0.8D1 * t68 + (0.9
     #D1 * t158 - t305 - t254 - 0.63D2 / 0.8D1 * t122 - 0.27D2 / 0.2D1 *
     # t118 + t163) * t9) * t4 + (-0.189D3 * t135 + 0.909D3 / 0.8D1 * t7
     #6 - 0.486D3 * t67 + 0.2565D4 / 0.8D1 * t68 - 0.63D2 * t77 + (-0.63
     #D2 / 0.8D1 * t106 - t239 - t318 + 0.9D1 * t148 - 0.27D2 / 0.2D1 * 
     #t108 + t155) * t2) * t6 - t318 + 0.27D2 / 0.8D1 * t106 - t305 + 0.
     #27D2 / 0.8D1 * t122 + t239 + t254) * S34 + t388 * t4 + t421 * t6 +
     # 0.27D2 / 0.8D1 * t376 + t385 + t367 - t350 - 0.9D1 / 0.8D1 * t368
     # - 0.9D1 / 0.8D1 * t352 + 0.27D2 / 0.8D1 * t347 - t407
      t435 = t28 ** 2
      t438 = t26 ** 2
      t459 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.90D2 * t23 + (t25 + (t27 + t29 - 
     #t31) * t9) * t4 + (t36 + (t37 - t31 + t38) * t2) * t6) * t13) * t1
     #5 * t17 + (((0.18D2 - 0.18D2 * t3) * t4 + (0.18D2 - 0.18D2 * t51) 
     #* t6) * S12 + ((0.45D2 + 0.36D2 * t3) * t4 + (0.45D2 + 0.36D2 * t5
     #1) * t6) * S34 + (t65 - t25 + t66 + t70 * t2) * t4 + (-t36 + t74 +
     # t75 + t79 * t9) * t6 + (((-0.99D2 - 0.18D2 * t3) * t4 + (-0.99D2 
     #- 0.18D2 * t51) * t6) * t23 + ((t75 - 0.162D3 * S23 - t70 * t2) * 
     #t4 + (t65 - 0.162D3 * S14 - t79 * t9) * t6) * S34 + (-0.99D2 * t28
     # + t104 + t27 - 0.216D3 * t67 + (-t107 - 0.18D2 * t108 + 0.36D2 * 
     #t110) * t2) * t4 + (t38 - 0.216D3 * t77 - 0.99D2 * t26 + t104 + (-
     #0.18D2 * t118 + 0.36D2 * t120 - t123) * t9) * t6) * t13 + ((0.9D1 
     #/ 0.2D1 * S13 * t4 + 0.9D1 / 0.2D1 * t7) * t23 + ((-0.9D1 * t68 - 
     #0.9D1 / 0.2D1 * t135 - t137) * t4 + (-0.9D1 / 0.2D1 * t140 - t142 
     #- 0.9D1 * t76) * t6) * S34 + (t107 + 0.9D1 / 0.2D1 * t148 + 0.9D1 
     #/ 0.2D1 * t108 + t152 + 0.9D1 * t153 + t155) * t4 + (0.9D1 / 0.2D1
     # * t158 + 0.9D1 / 0.2D1 * t118 + t123 + t162 + t163 + 0.9D1 * t164
     #) * t6) * t170) * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0.9D1 
     #/ 0.8D1 * S24 * t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t169 + ((
     #0.243D3 / 0.2D1 * S13 + t186 + t22 + t187 + 0.243D3 / 0.2D1 * S24)
     # * S34 + (t186 - 0.171D3 / 0.8D1 * S13 - 0.189D3 / 0.2D1 * S24 + (
     #-0.9D1 / 0.2D1 * t67 + 0.27D2 / 0.8D1 * t68 + t195) * t9) * t4 + (
     #-0.189D3 / 0.2D1 * S13 - 0.171D3 / 0.8D1 * S24 + t187 + (0.27D2 / 
     #0.8D1 * t76 + t203 - 0.9D1 / 0.2D1 * t77) * t2) * t6 + 0.27D2 / 0.
     #2D1 * t135 - 0.27D2 / 0.2D1 * t67 - 0.27D2 / 0.2D1 * t77 + 0.27D2 
     #/ 0.2D1 * t140) * S12 + (-0.333D3 / 0.2D1 * S13 - t66 - 0.72D2 * S
     #34 - t74 - 0.333D3 / 0.2D1 * S24) * t23 + ((0.2277D4 / 0.8D1 * S24
     # - t66 + 0.27D2 * S13) * t4 + (0.2277D4 / 0.8D1 * S13 - t74 + 0.27
     #D2 * S24) * t6 + t142 - 0.27D2 * t135 - 0.27D2 * t140 + t137) * S3
     #4 + (-0.189D3 * t76 + t27 - 0.63D2 * t68 + 0.2745D4 / 0.8D1 * t140
     # + 0.2565D4 / 0.8D1 * t77 + 0.9D1 * t67 - 0.99D2 * t135 - t31 + t2
     #9 + (-0.27D2 / 0.2D1 * t106 + t152 - t239 - 0.63D2 / 0.8D1 * t108 
     #- 0.27D2 / 0.4D1 * t148 + t155) * t9) * t4 + (0.2745D4 / 0.8D1 * t
     #135 + t38 + t37 - 0.63D2 * t76 + 0.2565D4 / 0.8D1 * t67 - 0.189D3 
     #* t68 + 0.9D1 * t77 - 0.99D2 * t140 - t31 + (-0.27D2 / 0.2D1 * t12
     #2 - 0.27D2 / 0.4D1 * t158 + t163 - t254 + t162 - 0.63D2 / 0.8D1 * 
     #t118) * t2) * t6 + t162 - 0.9D1 / 0.2D1 * t153 - 0.9D1 / 0.2D1 * t
     #106 - 0.9D1 / 0.2D1 * t164 - 0.9D1 / 0.2D1 * t122 + t152 + t427 * 
     #t13 + ((-0.18D2 * t372 * S14 - 0.18D2 * t68 * t351 + 0.27D2 * t108
     # * t26 + 0.9D1 * S13 * t435 + 0.9D1 * S13 * t438) * t9 * t4 + (0.2
     #7D2 * t122 * t26 - 0.18D2 * t77 * t351 - 0.18D2 * t368 * S14 + 0.9
     #D1 * S24 * t435 + 0.9D1 * S24 * t438) * t2 * t6) * t170
      rrgg2ggh61J4 = t459 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh61J5
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.36D2 * S34
      t23 = S34 ** 2
      t25 = 0.216D3 * S13
      t26 = S14 ** 2
      t27 = 0.18D2 * t26
      t28 = S23 ** 2
      t29 = 0.126D3 * t28
      t30 = S14 * S23
      t31 = 0.36D2 * t30
      t36 = 0.216D3 * S24
      t37 = 0.126D3 * t26
      t38 = 0.18D2 * t28
      t51 = S24 * t9
      t65 = 0.27D2 * S23
      t66 = 0.36D2 * S14
      t67 = S13 * S14
      t68 = S13 * S23
      t70 = 0.36D2 * t67 - 0.36D2 * t68
      t74 = 0.36D2 * S23
      t75 = 0.27D2 * S14
      t76 = S14 * S24
      t77 = S23 * S24
      t79 = -0.36D2 * t76 + 0.36D2 * t77
      t104 = 0.45D2 * t30
      t106 = S13 * t26
      t107 = 0.18D2 * t106
      t108 = S13 * t28
      t110 = t67 * S23
      t111 = 0.36D2 * t110
      t118 = t26 * S24
      t120 = t30 * S24
      t121 = 0.36D2 * t120
      t122 = t28 * S24
      t123 = 0.18D2 * t122
      t135 = S13 ** 2
      t140 = S24 ** 2
      t148 = t135 * S23
      t151 = t135 * S14
      t152 = 0.9D1 * t151
      t153 = t135 * S13
      t158 = S14 * t140
      t161 = t140 * S23
      t162 = 0.9D1 * t161
      t164 = t140 * S24
      t169 = S12 ** 2
      t170 = 0.1D1 / t169
      t186 = 0.18D2 * S14
      t187 = 0.18D2 * S23
      t195 = 0.27D2 / 0.8D1 * t135
      t203 = 0.27D2 / 0.8D1 * t140
      t241 = 0.27D2 / 0.8D1 * t153
      t244 = 0.27D2 * t110
      t257 = 0.27D2 * t120
      t258 = 0.27D2 / 0.8D1 * t164
      t309 = 0.27D2 / 0.4D1 * t161
      t322 = 0.27D2 / 0.4D1 * t151
      t339 = S14 * t28
      t342 = t26 * S23
      t347 = t28 * S23
      t349 = t164 * S14
      t351 = t140 * t28
      t353 = t164 * S23
      t354 = 0.27D2 / 0.8D1 * t353
      t355 = t26 * S14
      t356 = S13 * t355
      t358 = t118 * S23
      t360 = t76 * t28
      t363 = 0.27D2 * t158 * S23
      t364 = t106 * S23
      t366 = t67 * t28
      t369 = 0.27D2 * t151 * S23
      t370 = t140 ** 2
      t371 = 0.9D1 / 0.8D1 * t370
      t372 = S24 * t347
      t374 = t140 * t26
      t376 = S13 * t347
      t378 = t153 * S14
      t380 = t135 * t26
      t382 = t153 * S23
      t384 = t135 * t28
      t386 = S24 * t355
      t388 = t135 ** 2
      t389 = 0.9D1 / 0.8D1 * t388
      t390 = -0.9D1 / 0.2D1 * t349 + 0.63D2 / 0.8D1 * t351 + t354 - 0.18
     #D2 * t356 + 0.243D3 / 0.2D1 * t358 - 0.333D3 / 0.2D1 * t360 - t363
     # + 0.243D3 / 0.2D1 * t364 - 0.333D3 / 0.2D1 * t366 - t369 + t371 +
     # 0.621D3 / 0.8D1 * t372 + 0.27D2 / 0.2D1 * t374 + 0.621D3 / 0.8D1 
     #* t376 - 0.9D1 / 0.2D1 * t378 + 0.27D2 / 0.2D1 * t380 + 0.27D2 / 0
     #.8D1 * t382 + 0.63D2 / 0.8D1 * t384 - 0.18D2 * t386 + t389
      t392 = -0.135D3 * t118 + 0.819D3 / 0.16D2 * t108 + 0.6849D4 / 0.16
     #D2 * t158 + 0.603D3 / 0.16D2 * t164 + t111 - 0.99D2 * t151 - 0.171
     #D3 / 0.8D1 * t106 - 0.36D2 * t339 - 0.3879D4 / 0.16D2 * t161 + 0.1
     #8D2 * t342 - 0.1809D4 / 0.8D1 * t122 + 0.5769D4 / 0.16D2 * t120 - 
     #0.585D3 / 0.4D1 * t153 + 0.126D3 * t347 + t390 * t9
      t411 = 0.27D2 / 0.8D1 * t378
      t423 = 0.621D3 / 0.8D1 * t386 - t363 + 0.27D2 / 0.8D1 * t349 - 0.3
     #33D3 / 0.2D1 * t358 + 0.243D3 / 0.2D1 * t360 + t411 - 0.333D3 / 0.
     #2D1 * t364 + 0.243D3 / 0.2D1 * t366 - t369 - 0.9D1 / 0.2D1 * t353 
     #+ 0.63D2 / 0.8D1 * t380 - 0.9D1 / 0.2D1 * t382 + 0.27D2 / 0.2D1 * 
     #t384 - 0.18D2 * t376 + 0.621D3 / 0.8D1 * t356 + t389 + 0.27D2 / 0.
     #2D1 * t351 + t371 - 0.18D2 * t372 + 0.63D2 / 0.8D1 * t374
      t425 = -0.99D2 * t161 + 0.18D2 * t339 + t121 - 0.1809D4 / 0.8D1 * 
     #t106 + 0.6849D4 / 0.16D2 * t148 + 0.5769D4 / 0.16D2 * t110 - 0.387
     #9D4 / 0.16D2 * t151 + 0.126D3 * t355 - 0.36D2 * t342 - 0.135D3 * t
     #108 + 0.603D3 / 0.16D2 * t153 - 0.585D3 / 0.4D1 * t164 + 0.819D3 /
     # 0.16D2 * t118 - 0.171D3 / 0.8D1 * t122 + t423 * t2
      t431 = ((0.126D3 - 0.9D1 / 0.8D1 * t51) * t4 + (0.126D3 - 0.9D1 / 
     #0.8D1 * t3) * t6 + 0.621D3 / 0.8D1 * S24 + 0.621D3 / 0.8D1 * S13) 
     #* t23 * S34 + ((0.819D3 / 0.16D2 * S13 - 0.1809D4 / 0.8D1 * S24 + 
     #0.126D3 * S14 + (t203 + 0.27D2 / 0.8D1 * t77 - 0.9D1 / 0.2D1 * t76
     #) * t9) * t4 + (0.819D3 / 0.16D2 * S24 + 0.126D3 * S23 - 0.1809D4 
     #/ 0.8D1 * S13 + (-0.9D1 / 0.2D1 * t68 + 0.27D2 / 0.8D1 * t67 + t19
     #5) * t2) * t6 - 0.63D2 / 0.8D1 * t77 + 0.63D2 / 0.8D1 * t135 + 0.6
     #3D2 / 0.8D1 * t140 - 0.63D2 / 0.8D1 * t67) * t23 + ((0.6633D4 / 0.
     #16D2 * t76 - 0.2673D4 / 0.4D1 * t77 - 0.3879D4 / 0.16D2 * t140 - 0
     #.72D2 * t67 + 0.1197D4 / 0.8D1 * t68 + (0.9D1 * t158 - t309 - t258
     # - 0.63D2 / 0.8D1 * t122 - 0.27D2 / 0.2D1 * t118 + t257) * t9) * t
     #4 + (-0.3879D4 / 0.16D2 * t135 + 0.1197D4 / 0.8D1 * t76 - 0.2673D4
     # / 0.4D1 * t67 + 0.6633D4 / 0.16D2 * t68 - 0.72D2 * t77 + (-0.63D2
     # / 0.8D1 * t106 - t241 - t322 + 0.9D1 * t148 - 0.27D2 / 0.2D1 * t1
     #08 + t244) * t2) * t6 - t322 + 0.27D2 / 0.8D1 * t106 - t309 + 0.27
     #D2 / 0.8D1 * t122 + t241 + t258) * S34 + t392 * t4 + t425 * t6 + 0
     #.27D2 / 0.8D1 * t380 + t389 + t371 - t354 - 0.9D1 / 0.8D1 * t372 -
     # 0.9D1 / 0.8D1 * t356 + 0.27D2 / 0.8D1 * t351 - t411
      t439 = t28 ** 2
      t442 = t26 ** 2
      t463 = (-0.72D2 * t3 * t4 - 0.72D2 * t7 * t9) * t13 * t15 * s * t1
     #7 * z + (0.18D2 * S12 - t22 + (0.126D3 * t23 + (t25 + (t27 + t29 -
     # t31) * t9) * t4 + (t36 + (t37 - t31 + t38) * t2) * t6) * t13) * t
     #15 * t17 + (((0.18D2 - 0.18D2 * t3) * t4 + (0.18D2 - 0.18D2 * t51)
     # * t6) * S12 + ((0.45D2 + 0.36D2 * t3) * t4 + (0.45D2 + 0.36D2 * t
     #51) * t6) * S34 + (t65 - t25 + t66 + t70 * t2) * t4 + (-t36 + t74 
     #+ t75 + t79 * t9) * t6 + (((-0.144D3 - 0.18D2 * t3) * t4 + (-0.144
     #D3 - 0.18D2 * t51) * t6) * t23 + ((t75 - 0.216D3 * S23 - t70 * t2)
     # * t4 + (t65 - 0.216D3 * S14 - t79 * t9) * t6) * S34 + (-0.144D3 *
     # t28 + t104 + t27 - 0.216D3 * t67 + (-t107 - 0.18D2 * t108 + t111)
     # * t2) * t4 + (t38 - 0.216D3 * t77 - 0.144D3 * t26 + t104 + (-0.18
     #D2 * t118 + t121 - t123) * t9) * t6) * t13 + ((0.9D1 / 0.2D1 * S13
     # * t4 + 0.9D1 / 0.2D1 * t7) * t23 + ((-0.9D1 * t68 - 0.9D1 / 0.2D1
     # * t135 - 0.81D2 / 0.2D1 * t67) * t4 + (-0.9D1 / 0.2D1 * t140 - 0.
     #81D2 / 0.2D1 * t77 - 0.9D1 * t76) * t6) * S34 + (t107 + 0.9D1 / 0.
     #2D1 * t148 + 0.9D1 / 0.2D1 * t108 + t152 + 0.9D1 * t153 + 0.81D2 /
     # 0.2D1 * t110) * t4 + (0.9D1 / 0.2D1 * t158 + 0.9D1 / 0.2D1 * t118
     # + t123 + t162 + 0.81D2 / 0.2D1 * t120 + 0.9D1 * t164) * t6) * t17
     #0) * s * z + (-0.9D1 / 0.8D1 * S13 * t9 * t4 - 0.9D1 / 0.8D1 * S24
     # * t2 * t6 - 0.18D2 * S24 - 0.18D2 * S13) * t169 + ((0.279D3 / 0.2
     #D1 * S13 + t186 + t22 + t187 + 0.279D3 / 0.2D1 * S24) * S34 + (t18
     #6 - 0.171D3 / 0.8D1 * S13 - 0.135D3 * S24 + (-0.9D1 / 0.2D1 * t67 
     #+ 0.27D2 / 0.8D1 * t68 + t195) * t9) * t4 + (-0.135D3 * S13 - 0.17
     #1D3 / 0.8D1 * S24 + t187 + (0.27D2 / 0.8D1 * t76 + t203 - 0.9D1 / 
     #0.2D1 * t77) * t2) * t6 + 0.27D2 / 0.2D1 * t135 - 0.27D2 / 0.2D1 *
     # t67 - 0.27D2 / 0.2D1 * t77 + 0.27D2 / 0.2D1 * t140) * S12 + (-0.4
     #05D3 / 0.2D1 * S13 - t66 - 0.72D2 * S34 - t74 - 0.405D3 / 0.2D1 * 
     #S24) * t23 + ((0.5769D4 / 0.16D2 * S24 - t66 + 0.36D2 * S13) * t4 
     #+ (0.5769D4 / 0.16D2 * S13 - t74 + 0.36D2 * S24) * t6 + 0.27D2 * t
     #77 - 0.27D2 * t135 - 0.27D2 * t140 + 0.27D2 * t67) * S34 + (-0.270
     #D3 * t76 + t27 - 0.72D2 * t68 + 0.6849D4 / 0.16D2 * t140 + 0.6633D
     #4 / 0.16D2 * t77 + 0.9D1 * t67 - 0.99D2 * t135 - t31 + t29 + (-0.2
     #7D2 / 0.2D1 * t106 + t152 - t241 - 0.63D2 / 0.8D1 * t108 - 0.27D2 
     #/ 0.4D1 * t148 + t244) * t9) * t4 + (0.6849D4 / 0.16D2 * t135 + t3
     #8 + t37 - 0.72D2 * t76 + 0.6633D4 / 0.16D2 * t67 - 0.270D3 * t68 +
     # 0.9D1 * t77 - 0.99D2 * t140 - t31 + (-0.27D2 / 0.2D1 * t122 - 0.2
     #7D2 / 0.4D1 * t158 + t257 - t258 + t162 - 0.63D2 / 0.8D1 * t118) *
     # t2) * t6 + t162 - 0.9D1 / 0.2D1 * t153 - 0.9D1 / 0.2D1 * t106 - 0
     #.9D1 / 0.2D1 * t164 - 0.9D1 / 0.2D1 * t122 + t152 + t431 * t13 + (
     #(-0.18D2 * t376 * S14 - 0.18D2 * t68 * t355 + 0.27D2 * t108 * t26 
     #+ 0.9D1 * S13 * t439 + 0.9D1 * S13 * t442) * t9 * t4 + (0.27D2 * t
     #122 * t26 - 0.18D2 * t77 * t355 - 0.18D2 * t372 * S14 + 0.9D1 * S2
     #4 * t439 + 0.9D1 * S24 * t442) * t2 * t6) * t170
      rrgg2ggh61J5 = t463 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh61J6
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t3 = S13 * t2
      t4 = S13 + S14 + S34
      t6 = S23 + S24 + S34
      t7 = S24 * t6
      t9 = 0.1D1 / (S12 + S13 + S23)
      t13 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t22 = 0.180D3 * S34
      t23 = S34 ** 2
      t25 = 0.1080D4 * S13
      t26 = S14 ** 2
      t27 = 0.90D2 * t26
      t28 = S23 ** 2
      t29 = 0.54D2 * t28
      t30 = S14 * S23
      t31 = 0.180D3 * t30
      t36 = 0.1080D4 * S24
      t37 = 0.54D2 * t26
      t38 = 0.90D2 * t28
      t51 = S24 * t9
      t65 = 0.189D3 * S23
      t66 = 0.180D3 * S14
      t67 = S13 * S23
      t68 = S13 * S14
      t70 = 0.180D3 * t67 - 0.180D3 * t68
      t74 = 0.189D3 * S14
      t75 = 0.180D3 * S23
      t76 = S14 * S24
      t77 = S23 * S24
      t79 = 0.180D3 * t76 - 0.180D3 * t77
      t91 = 0.54D2 * S23
      t96 = 0.54D2 * S14
      t105 = 0.171D3 * t30
      t106 = t68 * S23
      t108 = S13 * t28
      t110 = S13 * t26
      t111 = 0.90D2 * t110
      t118 = t28 * S24
      t119 = 0.90D2 * t118
      t120 = t30 * S24
      t122 = t26 * S24
      t134 = 0.135D3 * t68
      t135 = S13 ** 2
      t141 = 0.135D3 * t77
      t142 = S24 ** 2
      t148 = t135 * S13
      t150 = t135 * S14
      t151 = 0.45D2 * t150
      t152 = t135 * S23
      t154 = 0.135D3 * t106
      t159 = t142 * S24
      t161 = 0.135D3 * t120
      t162 = t142 * S23
      t163 = 0.45D2 * t162
      t164 = S14 * t142
      t169 = S12 ** 2
      t170 = 0.1D1 / t169
      t186 = 0.90D2 * S14
      t187 = 0.90D2 * S23
      t195 = 0.135D3 / 0.8D1 * t135
      t203 = 0.135D3 / 0.8D1 * t142
      t240 = 0.135D3 / 0.8D1 * t148
      t252 = 0.135D3 / 0.8D1 * t159
      t302 = 0.135D3 / 0.4D1 * t162
      t317 = 0.135D3 / 0.4D1 * t150
      t328 = S14 * t28
      t333 = t26 * S23
      t339 = t28 * S23
      t344 = t142 ** 2
      t345 = 0.45D2 / 0.8D1 * t344
      t346 = t135 ** 2
      t347 = 0.45D2 / 0.8D1 * t346
      t348 = t110 * S23
      t350 = t68 * t28
      t353 = 0.135D3 * t150 * S23
      t354 = t122 * S23
      t356 = t76 * t28
      t359 = 0.135D3 * t164 * S23
      t360 = t26 * S14
      t361 = S24 * t360
      t363 = t142 * t26
      t365 = t159 * S23
      t366 = 0.135D3 / 0.8D1 * t365
      t367 = t135 * t26
      t369 = t148 * S23
      t371 = t135 * t28
      t373 = S13 * t360
      t375 = S24 * t339
      t377 = t142 * t28
      t379 = t159 * S14
      t381 = S13 * t339
      t383 = t148 * S14
      t385 = -t345 - t347 - 0.531D3 / 0.2D1 * t348 + 0.297D3 / 0.2D1 * t
     #350 + t353 - 0.531D3 / 0.2D1 * t354 + 0.297D3 / 0.2D1 * t356 + t35
     #9 + 0.90D2 * t361 - 0.135D3 / 0.2D1 * t363 - t366 - 0.135D3 / 0.2D
     #1 * t367 - 0.135D3 / 0.8D1 * t369 - 0.315D3 / 0.8D1 * t371 + 0.90D
     #2 * t373 - 0.369D3 / 0.8D1 * t375 - 0.315D3 / 0.8D1 * t377 + 0.45D
     #2 / 0.2D1 * t379 - 0.369D3 / 0.8D1 * t381 + 0.45D2 / 0.2D1 * t383
      t387 = -0.63D2 / 0.2D1 * t118 + 0.180D3 * t328 + 0.495D3 * t150 + 
     #0.855D3 / 0.8D1 * t110 - 0.279D3 / 0.4D1 * t162 - 0.90D2 * t333 - 
     #0.1431D4 / 0.8D1 * t164 - 0.675D3 / 0.2D1 * t122 + 0.135D3 / 0.4D1
     # * t159 + 0.1377D4 / 0.16D2 * t108 + 0.54D2 * t339 - 0.9D1 * t106 
     #- 0.99D2 / 0.8D1 * t120 + 0.2925D4 / 0.4D1 * t148 + t385 * t9
      t413 = 0.135D3 / 0.8D1 * t383
      t419 = 0.90D2 * t375 - 0.315D3 / 0.8D1 * t363 + 0.297D3 / 0.2D1 * 
     #t354 - 0.531D3 / 0.2D1 * t356 + t359 + 0.297D3 / 0.2D1 * t348 - 0.
     #531D3 / 0.2D1 * t350 + t353 - 0.369D3 / 0.8D1 * t373 - 0.135D3 / 0
     #.8D1 * t379 - 0.135D3 / 0.2D1 * t377 + 0.45D2 / 0.2D1 * t365 - t41
     #3 - 0.315D3 / 0.8D1 * t367 + 0.45D2 / 0.2D1 * t369 + 0.90D2 * t381
     # - 0.369D3 / 0.8D1 * t361 - 0.135D3 / 0.2D1 * t371 - t345 - t347
      t421 = 0.855D3 / 0.8D1 * t118 + 0.1377D4 / 0.16D2 * t122 + 0.135D3
     # / 0.4D1 * t148 + 0.2925D4 / 0.4D1 * t159 + 0.54D2 * t360 - 0.90D2
     # * t328 + 0.180D3 * t333 + 0.495D3 * t162 - 0.9D1 * t120 - 0.63D2 
     #/ 0.2D1 * t110 - 0.279D3 / 0.4D1 * t150 - 0.1431D4 / 0.8D1 * t152 
     #- 0.675D3 / 0.2D1 * t108 - 0.99D2 / 0.8D1 * t106 + t419 * t2
      t427 = ((0.54D2 + 0.45D2 / 0.8D1 * t51) * t4 + (0.54D2 + 0.45D2 / 
     #0.8D1 * t3) * t6 - 0.369D3 / 0.8D1 * S24 - 0.369D3 / 0.8D1 * S13) 
     #* t23 * S34 + ((t96 + 0.1377D4 / 0.16D2 * S13 - 0.63D2 / 0.2D1 * S
     #24 + (0.45D2 / 0.2D1 * t76 - 0.135D3 / 0.8D1 * t77 - t203) * t9) *
     # t4 + (-0.63D2 / 0.2D1 * S13 + 0.1377D4 / 0.16D2 * S24 + t91 + (0.
     #45D2 / 0.2D1 * t67 - t195 - 0.135D3 / 0.8D1 * t68) * t2) * t6 + 0.
     #315D3 / 0.8D1 * t77 + 0.315D3 / 0.8D1 * t68 - 0.315D3 / 0.8D1 * t1
     #42 - 0.315D3 / 0.8D1 * t135) * t23 + ((-0.513D3 / 0.8D1 * t67 - 0.
     #351D3 * t77 - 0.279D3 / 0.4D1 * t142 + 0.189D3 * t68 + 0.477D3 / 0
     #.8D1 * t76 + (t302 + t252 - t161 + 0.315D3 / 0.8D1 * t118 - 0.45D2
     # * t164 + 0.135D3 / 0.2D1 * t122) * t9) * t4 + (-0.279D3 / 0.4D1 *
     # t135 + 0.477D3 / 0.8D1 * t67 - 0.513D3 / 0.8D1 * t76 + 0.189D3 * 
     #t77 - 0.351D3 * t68 + (0.135D3 / 0.2D1 * t108 + t240 + 0.315D3 / 0
     #.8D1 * t110 + t317 - 0.45D2 * t152 - t154) * t2) * t6 - 0.135D3 / 
     #0.8D1 * t118 + t317 - 0.135D3 / 0.8D1 * t110 + t302 - t240 - t252)
     # * S34 + t387 * t4 + t421 * t6 - t347 - t345 + t366 - 0.135D3 / 0.
     #8D1 * t377 - 0.135D3 / 0.8D1 * t367 + t413 + 0.45D2 / 0.8D1 * t375
     # + 0.45D2 / 0.8D1 * t373
      t435 = t28 ** 2
      t438 = t26 ** 2
      t459 = (0.360D3 * t3 * t4 + 0.360D3 * t7 * t9) * t13 * t15 * s * t
     #17 * z + (-0.90D2 * S12 + t22 + (0.54D2 * t23 + (-t25 + (-t27 + t2
     #9 + t31) * t9) * t4 + (-t36 + (t37 + t31 - t38) * t2) * t6) * t13)
     # * t15 * t17 + (((-0.90D2 + 0.90D2 * t3) * t4 + (-0.90D2 + 0.90D2 
     #* t51) * t6) * S12 + ((-0.171D3 - 0.180D3 * t3) * t4 + (-0.171D3 -
     # 0.180D3 * t51) * t6) * S34 + (t25 - t65 - t66 + t70 * t2) * t4 + 
     #(-t74 + t36 - t75 + t79 * t9) * t6 + (((-0.81D2 + 0.90D2 * t3) * t
     #4 + (-0.81D2 + 0.90D2 * t51) * t6) * t23 + ((-t74 - t91 - t70 * t2
     #) * t4 + (-t96 - t65 - t79 * t9) * t6) * S34 + (0.1080D4 * t68 - t
     #27 - 0.81D2 * t28 - t105 + (-0.180D3 * t106 + 0.90D2 * t108 + t111
     #) * t2) * t4 + (-0.81D2 * t26 - t38 - t105 + 0.1080D4 * t77 + (t11
     #9 - 0.180D3 * t120 + 0.90D2 * t122) * t9) * t6) * t13 + ((-0.45D2 
     #/ 0.2D1 * S13 * t4 - 0.45D2 / 0.2D1 * t7) * t23 + ((-t134 + 0.45D2
     # / 0.2D1 * t135 + 0.45D2 * t67) * t4 + (0.45D2 * t76 - t141 + 0.45
     #D2 / 0.2D1 * t142) * t6) * S34 + (-0.45D2 * t148 - t111 - t151 - 0
     #.45D2 / 0.2D1 * t152 + t154 - 0.45D2 / 0.2D1 * t108) * t4 + (-0.45
     #D2 / 0.2D1 * t122 - t119 - 0.45D2 * t159 + t161 - t163 - 0.45D2 / 
     #0.2D1 * t164) * t6) * t170) * s * z + (0.45D2 / 0.8D1 * S13 * t9 *
     # t4 + 0.45D2 / 0.8D1 * S24 * t2 * t6 + 0.90D2 * S24 + 0.90D2 * S13
     #) * t169 + ((-0.711D3 / 0.2D1 * S13 - t186 - t22 - t187 - 0.711D3 
     #/ 0.2D1 * S24) * S34 + (-t186 + 0.855D3 / 0.8D1 * S13 - 0.675D3 / 
     #0.2D1 * S24 + (0.45D2 / 0.2D1 * t68 - 0.135D3 / 0.8D1 * t67 - t195
     #) * t9) * t4 + (-0.675D3 / 0.2D1 * S13 + 0.855D3 / 0.8D1 * S24 - t
     #187 + (-0.135D3 / 0.8D1 * t76 - t203 + 0.45D2 / 0.2D1 * t77) * t2)
     # * t6 - 0.135D3 / 0.2D1 * t135 + 0.135D3 / 0.2D1 * t68 + 0.135D3 /
     # 0.2D1 * t77 - 0.135D3 / 0.2D1 * t142) * S12 + (0.657D3 / 0.2D1 * 
     #S13 + t66 + 0.360D3 * S34 + t75 + 0.657D3 / 0.2D1 * S24) * t23 + (
     #(t66 - 0.99D2 / 0.8D1 * S24 - 0.9D1 * S13) * t4 + (-0.9D1 * S24 - 
     #0.99D2 / 0.8D1 * S13 + t75) * t6 + 0.135D3 * t135 - t141 + 0.135D3
     # * t142 - t134) * S34 + (t29 - 0.675D3 * t76 - 0.1431D4 / 0.8D1 * 
     #t142 + 0.189D3 * t67 - t27 - 0.45D2 * t68 + 0.477D3 / 0.8D1 * t77 
     #+ t31 + 0.495D3 * t135 + (0.315D3 / 0.8D1 * t108 - t151 - t154 + 0
     #.135D3 / 0.2D1 * t110 + t240 + 0.135D3 / 0.4D1 * t152) * t9) * t4 
     #+ (-0.675D3 * t67 + 0.477D3 / 0.8D1 * t68 - 0.1431D4 / 0.8D1 * t13
     #5 + 0.189D3 * t76 + t37 - t38 - 0.45D2 * t77 + t31 + 0.495D3 * t14
     #2 + (-t161 + t252 + 0.135D3 / 0.2D1 * t118 + 0.135D3 / 0.4D1 * t16
     #4 + 0.315D3 / 0.8D1 * t122 - t163) * t2) * t6 + 0.45D2 / 0.2D1 * t
     #110 - t163 + 0.45D2 / 0.2D1 * t118 + 0.45D2 / 0.2D1 * t148 - t151 
     #+ 0.45D2 / 0.2D1 * t159 + t427 * t13 + ((0.90D2 * t381 * S14 + 0.9
     #0D2 * t67 * t360 - 0.135D3 * t108 * t26 - 0.45D2 * S13 * t435 - 0.
     #45D2 * S13 * t438) * t9 * t4 + (-0.135D3 * t118 * t26 + 0.90D2 * t
     #77 * t360 + 0.90D2 * t375 * S14 - 0.45D2 * S24 * t435 - 0.45D2 * S
     #24 * t438) * t2 * t6) * t170
      rrgg2ggh61J6 = t459 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh61J7
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
      t1 = S34 ** 2
      t2 = S23 ** 2
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = S13 + S14 + S34
      t8 = S23 + S24 + S34
      t9 = S14 ** 2
      t12 = 0.1D1 / (S12 + S14 + S24)
      t16 = 0.1D1 / S12
      t18 = s ** 2
      t19 = z ** 2
      t22 = 0.45D2 * S13
      t23 = 0.45D2 * S14
      t25 = 0.45D2 * S23
      t26 = 0.45D2 * S24
      t33 = 0.270D3 * S13
      t37 = 0.270D3 * S24
      t40 = 0.180D3 * S23
      t43 = 0.180D3 * S14
      t48 = S14 * S23
      t49 = 0.45D2 * t48
      t61 = S24 + S13
      t68 = 0.90D2 * S24
      t71 = 0.90D2 * S13
      t77 = S23 * S24
      t79 = S13 * S23
      t81 = S24 ** 2
      t85 = S13 * S14
      t87 = S13 ** 2
      t89 = S14 * S24
      t107 = 0.180D3 * t79
      t110 = 0.180D3 * t89
      t122 = t48 * S24
      t128 = t85 * S23
      t134 = t2 * S23
      t136 = t85 * t2
      t140 = S24 * t9
      t141 = t140 * S23
      t143 = t89 * t2
      t147 = S13 * t9
      t148 = t147 * S23
      t162 = t9 * S14
      rrgg2ggh61J7 = ((0.180D3 * t1 + 0.180D3 * t2 * t4 * t6 + 0.180D3 *
     # t8 * t9 * t12) * t16 * t18 * t19 + ((-t22 - t23 - 0.90D2 * S34 - 
     #t25 - t26) * S34 + 0.45D2 * t6 * S23 + 0.45D2 * t8 * S14 + ((-t33 
     #- 0.270D3 * S14 - 0.540D3 * S34 - 0.270D3 * S23 - t37) * t1 + ((-t
     #40 + t23) * t6 + (-t43 + t25) * t8) * S34 + (-t49 - 0.270D3 * t2) 
     #* t6 + (-t49 - 0.270D3 * t9) * t8) * t16) * s * z + 0.90D2 * t61 *
     # S34 * S12 - 0.180D3 * t61 * t1 + ((t68 + t22) * t6 + (t26 + t71) 
     #* t8) * S34 + (0.180D3 * t2 + 0.180D3 * t77 - 0.45D2 * t79 + 0.135
     #D3 * t81) * t6 + (0.180D3 * t85 + 0.135D3 * t87 - 0.45D2 * t89 + 0
     #.180D3 * t9) * t8 + ((t33 + t43 + 0.360D3 * S34 + t40 + t37) * t1 
     #* S34 + ((t43 - 0.180D3 * S24 + t71) * t6 + (t40 + t68 - 0.180D3 *
     # S13) * t8) * t1 + ((-0.45D2 * t85 + t107 - 0.720D3 * t77 - 0.45D2
     # * t81 + t110) * t6 + (t107 - 0.45D2 * t77 + t110 - 0.45D2 * t87 -
     # 0.720D3 * t85) * t8) * S34 + (0.225D3 * t81 * S24 + 0.90D2 * t122
     # + 0.90D2 * S13 * t2 - 0.45D2 * t81 * S23 + 0.45D2 * t128 - 0.180D
     #3 * t2 * S24 + 0.135D3 * S14 * t81 + 0.180D3 * t134 + (-0.180D3 * 
     #t136 + 0.90D2 * S13 * t134 + 0.90D2 * t141 - 0.180D3 * t143 + 0.90
     #D2 * S24 * t134 + 0.90D2 * t148) * t4) * t6 + (0.45D2 * t122 + 0.2
     #25D3 * t87 * S13 + 0.135D3 * t87 * S23 + 0.90D2 * t140 - 0.45D2 * 
     #t87 * S14 + 0.180D3 * t162 + 0.90D2 * t128 - 0.180D3 * t147 + (0.9
     #0D2 * S24 * t162 - 0.180D3 * t148 - 0.180D3 * t141 + 0.90D2 * S13 
     #* t162 + 0.90D2 * t136 + 0.90D2 * t143) * t12) * t8) * t16) / pi *
     # wd / z

      end function
  
 