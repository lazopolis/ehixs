  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = lh * t6
      t8 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t12 = lh ** 2
      t14 = 0.3141592653589793D1 ** 2
      t16 = 0.180D3 * t12 - 0.30D2 * t14
      t17 = t16 * t6
      t18 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t20 = t4 * t17 * t18
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = x3 * t23
      t25 = t1 ** 2
      t26 = t25 ** 2
      t27 = x4 * 0.3141592653589793D1
      t28 = Sin(t27)
      t29 = t28 ** 2
      t30 = t26 * t29
      t33 = log(0.4D1 * t24 * t30)
      t34 = -0.1D1 + x3
      t35 = 0.1D1 / t34
      t36 = t30 * t35
      t39 = log(-0.4D1 * t24 * t36)
      t41 = cos(t27)
      t42 = x3 * t34
      t43 = Sqrt(-t42)
      t48 = 0.1D1 / (-x3 - z + 0.2D1 * t41 * t43 * z)
      t53 = t39 ** 2
      t57 = t33 ** 2
      t70 = 0.60D2 * lh * t14 - 0.2884936567583026D3 - 0.120D3 * t12 * l
     #h
      t71 = t70 * t6
      t73 = t4 * t71 * t18
      t91 = 0.1D1 / x3
      t95 = t23 * t26 * t29
      t97 = log(0.4D1 * t95)
      t98 = t97 ** 2
      t99 = t98 * t3
      t100 = 0.3141592653589793D1 * lh
      t105 = t98 * t97 * t3
      t108 = t97 * t3
      t109 = 0.3141592653589793D1 * t16
      t122 = t14 ** 2
      t123 = t12 ** 2
      t129 = t98 ** 2
      t137 = x1 ** 2
      t138 = t24 * t137
      t141 = log(-0.4D1 * t138 * t36)
      t142 = t141 * z
      t144 = t141 ** 2
      t150 = t137 * t26
      t154 = log(0.4D1 * t24 * t150 * t29)
      t156 = t154 ** 2
      t163 = z * t8
      t174 = t18 + z * t18 * t48
      t176 = t4 * t17 * t174
      t179 = 0.1D1 / x1
      t182 = t23 * t137
      t185 = log(0.4D1 * t182 * t30)
      t190 = t185 ** 2
      t210 = x2 ** 2
      t211 = x3 * t210
      t212 = t211 * t137
      t215 = log(0.4D1 * t212 * t95)
      t221 = log(-0.4D1 * t212 * t30 * t23 * t35)
      t235 = 0.1D1 / x2
      t236 = t235 * t179
      t239 = t210 * t137
      t242 = log(0.4D1 * t239 * t95)
      t244 = t242 ** 2
      t262 = log(0.4D1 * t211 * t95)
      t264 = t262 ** 2
      t267 = t211 * t26
      t268 = t29 * t23
      t272 = log(-0.4D1 * t267 * t268 * t35)
      t273 = t272 * z
      t275 = t272 ** 2
      t297 = t210 * t26
      t300 = log(0.4D1 * t297 * t268)
      t305 = t300 ** 2
      t325 = ((-0.180D3 * t4 * t7 * t8 + t20) * (-t33 - t39 * z * t48) +
     # 0.90D2 * t4 * t6 * t18 * (-t53 * t39 * z * t48 / 0.6D1 - t57 * t3
     #3 / 0.6D1) + (t4 * t17 * t8 + t73) * (0.1D1 + z * t48) + (0.90D2 *
     # t4 * t6 * t8 - 0.180D3 * t4 * t7 * t18) * (t57 / 0.2D1 + t53 * z 
     #* t48 / 0.2D1)) * t91 / 0.2880D4 + (-0.90D2 * t99 * t100 + t4 * t7
     #0 - 0.15D2 * t105 * 0.3141592653589793D1 - t108 * t109) * t6 * t8 
     #/ 0.2880D4 + (0.30D2 * t105 * t100 + t99 * t109 / 0.2D1 - t108 * 0
     #.3141592653589793D1 * t70 + t4 * (0.5769873135166051D3 * lh + t122
     # + 0.60D2 * t123 - 0.60D2 * t12 * t14) + 0.15D2 / 0.4D1 * t129 * t
     #3 * 0.3141592653589793D1) * t6 * t18 / 0.2880D4 + (0.90D2 * t4 * t
     #6 * ((-t142 * t8 + t144 * z * t18 / 0.2D1) * t48 - t154 * t8 + t15
     #6 * t18 / 0.2D1) - 0.180D3 * t4 * t7 * ((t163 - t142 * t18) * t48 
     #+ t8 - t154 * t18) + t176) * t91 * t179 / 0.1440D4 + (t4 * t17 * (
     #t8 - t185 * t18) + 0.90D2 * t4 * t6 * (t190 * t8 / 0.2D1 - t190 * 
     #t185 * t18 / 0.6D1) + t73 - 0.180D3 * t4 * t7 * (-t185 * t8 + t190
     # * t18 / 0.2D1)) * t179 / 0.1440D4 + (0.90D2 * t4 * t6 * (t8 - t21
     #5 * t18 + (t163 - t221 * z * t18) * t48) - 0.180D3 * t4 * t7 * t17
     #4) * t91 * t236 / 0.720D3 - (0.90D2 * t4 * t6 * (t242 * t8 - t244 
     #* t18 / 0.2D1) - 0.180D3 * t4 * t7 * (-t8 + t242 * t18) - t20) * t
     #235 * t179 / 0.720D3 + (0.90D2 * t4 * t6 * (-t262 * t8 + t264 * t1
     #8 / 0.2D1 + (-t273 * t8 + t275 * z * t18 / 0.2D1) * t48) - 0.180D3
     # * t4 * t7 * (t8 - t262 * t18 + (t163 - t273 * t18) * t48) + t176)
     # * t91 * t235 / 0.1440D4 - (t4 * t17 * (-t8 + t300 * t18) + 0.90D2
     # * t4 * t6 * (-t305 * t8 / 0.2D1 + t305 * t300 * t18 / 0.6D1) - t7
     #3 - 0.180D3 * t4 * t7 * (t300 * t8 - t305 * t18 / 0.2D1)) * t235 /
     # 0.1440D4
      t326 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t325)
      t328 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t325)
      t330 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t325)
      t332 = t2 * x1
      t333 = -0.1D1 + x1
      t334 = x1 * z
      t335 = 0.1D1 - x1 + t334
      t336 = 0.1D1 / t335
      t338 = t2 * t333 * t336
      t339 = s * t25
      t341 = x1 * t333 * t336
      t342 = t339 * t341
      t344 = t333 ** 2
      t350 = log(-0.4D1 * t24 * t150 * t29 * t344 * t336 * t35)
      t351 = t350 * z
      t352 = -t333
      t353 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t352, 0.10D1, 0
     #.10D1, x4)
      t355 = t350 ** 2
      t357 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t352, 0.10D1, 0
     #.10D1, x4)
      t362 = x3 * x1
      t363 = t362 * z
      t364 = 0.3D1 * t363
      t365 = x1 * t22
      t366 = x3 * t22
      t367 = t366 * x1
      t368 = x3 * t137
      t370 = 0.2D1 * t368 * z
      t371 = t368 * t22
      t372 = x3 * t335
      t374 = Sqrt(-t372 * t34)
      t378 = 0.2D1 * t362
      t379 = -z + t334 - t364 - t365 + t367 + t370 - t371 - x3 + 0.2D1 *
     # t41 * t374 * z + t378 - t368
      t380 = 0.1D1 / t379
      t382 = t344 * t336
      t386 = log(0.4D1 * t138 * t30 * t382)
      t388 = t386 ** 2
      t395 = z * t353
      t408 = -z * t357 * t335 * t380 - t357
      t414 = (0.90D2 * t4 * t6 * (-(-t351 * t353 + t355 * z * t357 / 0.2
     #D1) * t335 * t380 + t386 * t353 - t388 * t357 / 0.2D1) - 0.180D3 *
     # t4 * t7 * (-(t395 - t351 * t357) * t335 * t380 - t353 + t386 * t3
     #57) + t4 * t17 * t408) * t91 * t179 / 0.1440D4
      t420 = log(0.4D1 * t182 * t26 * t29 * t336 * t344)
      t422 = -t353 + t420 * t357
      t425 = t420 ** 2
      t431 = -t425 * t353 / 0.2D1 + t425 * t420 * t357 / 0.6D1
      t436 = t4 * t71 * t357
      t440 = t420 * t353 - t425 * t357 / 0.2D1
      t448 = t268 * t382
      t451 = log(0.4D1 * t211 * t150 * t448)
      t453 = t344 * t26
      t461 = log(-0.4D1 * t453 * t35 * x3 * t268 * t336 * t210 * t137)
      t477 = (0.90D2 * t4 * t6 * (-t353 + t451 * t357 - (t395 - t461 * z
     # * t357) * t335 * t380) - 0.180D3 * t4 * t7 * t408) * t91 * t236 /
     # 0.720D3
      t478 = t239 * t26
      t481 = log(0.4D1 * t478 * t448)
      t483 = t481 ** 2
      t500 = (0.90D2 * t4 * t6 * (-t481 * t353 + t483 * t357 / 0.2D1) - 
     #0.180D3 * t4 * t7 * (t353 - t481 * t357) + t4 * t17 * t357) * t235
     # * t179 / 0.720D3
      t501 = t414 + (t4 * t17 * t422 + 0.90D2 * t4 * t6 * t431 - t436 - 
     #0.180D3 * t4 * t7 * t440) * t179 / 0.1440D4 + t477 - t500
      t502 = FJET(XB1, XB2, s, 0.0D0, t332, -t338, 0.0D0, -t342, t501)
      t505 = x2 * t1 * s
      t506 = -0.1D1 + x2
      t508 = t506 * t1 * s
      t509 = -t506
      t510 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t509, 0
     #.10D1, x4)
      t511 = t23 * t506
      t515 = log(-0.4D1 * t212 * t30 * t511)
      t516 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t509, 0
     #.10D1, x4)
      t529 = t268 * t506
      t532 = log(-0.4D1 * t478 * t529)
      t534 = t532 ** 2
      t547 = t4 * t17 * t516
      t554 = log(-0.4D1 * t267 * t529)
      t556 = t554 ** 2
      t574 = log(-0.4D1 * t297 * t529)
      t579 = t574 ** 2
      t601 = (0.90D2 * t4 * t6 * (-t510 + t515 * t516) + 0.180D3 * t4 * 
     #t7 * t516) * t91 * t236 / 0.720D3 - (0.90D2 * t4 * t6 * (-t532 * t
     #510 + t534 * t516 / 0.2D1) - 0.180D3 * t4 * t7 * (t510 - t532 * t5
     #16) + t547) * t235 * t179 / 0.720D3 + (0.90D2 * t4 * t6 * (t554 * 
     #t510 - t556 * t516 / 0.2D1) - 0.180D3 * t4 * t7 * (-t510 + t554 * 
     #t516) - t547) * t91 * t235 / 0.1440D4 - (t4 * t17 * (t510 - t574 *
     # t516) + 0.90D2 * t4 * t6 * (t579 * t510 / 0.2D1 - t579 * t574 * t
     #516 / 0.6D1) + t4 * t71 * t516 - 0.180D3 * t4 * t7 * (-t574 * t510
     # + t579 * t516 / 0.2D1)) * t235 / 0.1440D4
      t602 = FJET(XB1, XB2, s, 0.0D0, t505, 0.0D0, -t508, 0.0D0, t601)
      t604 = x2 * x3
      t607 = Sqrt(x3 * t506 * t34)
      t608 = t41 * t607
      t610 = 0.2D1 * t608 * x2
      t612 = 0.1D1 - x3 + t604
      t613 = 0.1D1 / t612
      t615 = t2 * (0.1D1 - x3 - x2 + t604 + t211 + t610) * t613
      t620 = t2 * x2 * (-0.1D1 + t604 + 0.2D1 * t608) * t613
      t621 = x2 * z
      t622 = -x2 + t621 - z
      t623 = t34 * t613
      t624 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t509, -
     #t623, x4)
      t625 = t622 * t624
      t626 = t612 ** 2
      t627 = 0.1D1 / t626
      t631 = t506 * t210 * t137
      t632 = t268 * t631
      t635 = log(0.4D1 * t627 * t26 * t42 * t632)
      t637 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t509, -
     #t623, x4)
      t641 = t604 * z
      t642 = t211 * z
      t648 = 0.1D1 / (x2 - t621 - t641 + t642 + x3 - t211 + z - t610 - 0
     #.2D1 * t608 * z + 0.2D1 * t608 * t621)
      t652 = t4 * lh
      t655 = t6 * t622 * t637 * t648
      t667 = log(0.4D1 * t211 * t30 * t511 * t34 * t627)
      t668 = t667 * t622
      t670 = t667 ** 2
      t691 = (-0.90D2 * t4 * t6 * (t625 - t635 * t622 * t637) * t648 + 0
     #.180D3 * t652 * t655) * t91 * t236 / 0.720D3 + (-0.90D2 * t4 * t6 
     #* (-t668 * t624 + t670 * t622 * t637 / 0.2D1) * t648 + 0.180D3 * t
     #652 * t6 * (t625 - t668 * t637) * t648 - t4 * t16 * t655) * t91 * 
     #t235 / 0.1440D4
      t692 = FJET(XB1, XB2, s, 0.0D0, t615, 0.0D0, -t620, 0.0D0, t691)
      t695 = t1 * t333
      t697 = t506 * s * t695 * t336
      t699 = x2 * s * t695
      t701 = t339 * t506 * t341
      t702 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t352, t509, 0.1
     #0D1, x4)
      t707 = log(-0.4D1 * t453 * x3 * t336 * t632)
      t708 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t352, t509, 0.1
     #0D1, x4)
      t719 = (0.90D2 * t4 * t6 * (t702 - t707 * t708) - 0.180D3 * t4 * t
     #7 * t708) * t91 * t236
      t726 = log(-0.4D1 * t239 * t30 * t23 * t336 * t344 * t506)
      t728 = t726 ** 2
      t731 = t726 * t702 - t728 * t708 / 0.2D1
      t736 = -t702 + t726 * t708
      t741 = t4 * t17 * t708
      t746 = t719 / 0.720D3 - (0.90D2 * t4 * t6 * t731 - 0.180D3 * t4 * 
     #t7 * t736 - t741) * t235 * t179 / 0.720D3
      t747 = FJET(XB1, XB2, s, 0.0D0, t697, t332, -t699, t701, t746)
      t749 = FJET(XB1, XB2, s, 0.0D0, -t508, 0.0D0, t505, 0.0D0, t601)
      t751 = FJET(XB1, XB2, s, 0.0D0, -t338, t332, 0.0D0, -t342, t501)
      t753 = FJET(XB1, XB2, s, 0.0D0, -t620, 0.0D0, t615, 0.0D0, t691)
      t755 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t325)
      t771 = t414 + (t4 * t17 * t422 + 0.90D2 * t4 * t6 * t431 - t436 - 
     #0.180D3 * t4 * t7 * t440) * t179 / 0.1440D4 + t477 - t500
      t772 = FJET(XB1, XB2, s, t332, 0.0D0, 0.0D0, -t338, -t342, t771)
      t774 = t326 * t325 + t328 * t325 + t330 * t325 + t502 * t501 + t60
     #2 * t601 + t692 * t691 + t747 * t746 + t749 * t601 + t751 * t501 +
     # t753 * t691 + t755 * t325 + t772 * t771
      t775 = FJET(XB1, XB2, s, t332, -t699, 0.0D0, t697, t701, t746)
      t777 = FJET(XB1, XB2, s, t505, 0.0D0, -t508, 0.0D0, 0.0D0, t601)
      t779 = FJET(XB1, XB2, s, t615, 0.0D0, -t620, 0.0D0, 0.0D0, t691)
      t781 = FJET(XB1, XB2, s, t697, 0.0D0, -t699, t332, t701, t746)
      t784 = t332 * t604 * t613
      t785 = t2 * t333
      t786 = t211 * x1
      t787 = t211 * t334
      t790 = Sqrt(t372 * t506 * t34)
      t791 = t41 * t790
      t793 = 0.2D1 * t791 * x2
      t797 = t785 * (t211 - t786 - x2 + t604 + 0.1D1 - x3 + t787 + t793)
     # * t336 * t613
      t801 = t34 * s * t1 * x1 * t613
      t807 = t785 * x2 * (-0.1D1 + t604 + x1 - t362 - t334 + t363 + 0.2D
     #1 * t791) * t336 * t613
      t809 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t352, t509, -t6
     #23, x4)
      t818 = log(0.4D1 * t627 * t344 * t26 * x3 * t34 * t268 * t336 * t6
     #31)
      t819 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t352, t509, -t6
     #23, x4)
      t823 = x2 * x1
      t824 = t823 * z
      t830 = x2 * t137
      t839 = -t786 + t793 - 0.3D1 * t824 - t604 * x1 + t368 * x2 + t823 
     #* t22 + 0.2D1 * t830 * z - t830 * t22 + 0.2D1 * t791 * z + t641 - 
     #t642 + t211 + t621 + 0.2D1 * t823 - t830 + 0.2D1 * t791 * t824 - z
      t851 = -x3 + t334 - t365 + t378 - t368 - x2 - t364 + t367 + t370 -
     # t371 + t787 - 0.2D1 * t791 * t823 - 0.2D1 * t791 * t621 + 0.2D1 *
     # t604 * t334 - t366 * t823 - 0.2D1 * t368 * t621 + t368 * t22 * x2
      t854 = (x2 - t823 + z - t621 + t824) / (t839 + t851)
      t863 = 0.90D2 * t4 * t6 * (t809 - t818 * t819) * t335 * t854 - 0.1
     #80D3 * t4 * t7 * t819 * t335 * t854
      t866 = t863 * t91 * t236 / 0.720D3
      t867 = FJET(XB1, XB2, s, t784, -t797, -t801, t807, t701, t866)
      t870 = t91 * t235 * t179
      t873 = FJET(XB1, XB2, s, t807, -t801, -t797, t784, t701, t866)
      t877 = FJET(XB1, XB2, s, -t508, 0.0D0, t505, 0.0D0, 0.0D0, t601)
      t879 = FJET(XB1, XB2, s, -t338, 0.0D0, 0.0D0, t332, -t342, t771)
      t893 = t719 / 0.720D3 - (0.90D2 * t4 * t6 * t731 - 0.180D3 * t4 * 
     #t7 * t736 - t741) * t235 * t179 / 0.720D3
      t894 = FJET(XB1, XB2, s, -t699, t332, t697, 0.0D0, t701, t893)
      t896 = FJET(XB1, XB2, s, -t620, 0.0D0, t615, 0.0D0, 0.0D0, t691)
      t898 = FJET(XB1, XB2, s, -t801, t807, t784, -t797, t701, t866)
      t902 = FJET(XB1, XB2, s, -t797, t784, t807, -t801, t701, t866)
      t906 = t775 * t746 + t777 * t601 + t779 * t691 + t781 * t746 + t86
     #7 * t863 * t870 / 0.720D3 + t873 * t863 * t870 / 0.720D3 + t877 * 
     #t601 + t879 * t771 + t894 * t893 + t896 * t691 + t898 * t863 * t87
     #0 / 0.720D3 + t902 * t863 * t870 / 0.720D3
      rrqqbar2gght5s1e1 = t774 + t906

      end function



      doubleprecision function rrqqbar2gght5s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = x3 * t10
      t12 = t1 ** 2
      t13 = t12 ** 2
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = t20 ** 2
      t22 = -0.1D1 + x3
      t23 = 0.1D1 / t22
      t24 = t17 * t23
      t27 = log(-0.4D1 * t11 * t24)
      t28 = t27 ** 2
      t30 = cos(t14)
      t32 = Sqrt(-x3 * t22)
      t37 = 0.1D1 / (-x3 - z + 0.2D1 * t30 * t32 * z)
      t44 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t48 = lh * t6
      t51 = 0.180D3 * t4 * t48 * t7
      t60 = lh ** 2
      t62 = 0.3141592653589793D1 ** 2
      t64 = 0.180D3 * t60 - 0.30D2 * t62
      t65 = t64 * t6
      t67 = t4 * t65 * t7
      t73 = 0.1D1 / x3
      t77 = t10 * t13 * t16
      t79 = log(0.4D1 * t77)
      t80 = t79 * t3
      t81 = 0.3141592653589793D1 * lh
      t84 = t79 ** 2
      t85 = t84 * t3
      t111 = x2 ** 2
      t112 = t111 * x3
      t115 = log(0.4D1 * t112 * t77)
      t117 = z * t44
      t118 = t112 * t13
      t119 = t16 * t10
      t123 = log(-0.4D1 * t118 * t119 * t23)
      t134 = t7 + z * t7 * t37
      t137 = 0.180D3 * t4 * t48 * t134
      t140 = 0.1D1 / x2
      t143 = t111 * t13
      t146 = log(0.4D1 * t143 * t119)
      t148 = t146 ** 2
      t163 = x1 ** 2
      t164 = t11 * t163
      t167 = log(-0.4D1 * t164 * t24)
      t172 = t163 * t13
      t176 = log(0.4D1 * t11 * t172 * t16)
      t184 = 0.1D1 / x1
      t187 = t4 * t6
      t189 = t140 * t184
      t193 = t111 * t163
      t196 = log(0.4D1 * t193 * t77)
      t206 = t10 * t163
      t209 = log(0.4D1 * t206 * t17)
      t211 = t209 ** 2
      t226 = (0.90D2 * t4 * t6 * t7 * (t21 / 0.2D1 + t28 * z * t37 / 0.2
     #D1) + (0.90D2 * t4 * t6 * t44 - t51) * (-t20 - t27 * z * t37) + (-
     #0.180D3 * t4 * t48 * t44 + t67) * (0.1D1 + z * t37)) * t73 / 0.288
     #0D4 + (0.180D3 * t80 * t81 + 0.45D2 * t85 * 0.3141592653589793D1 +
     # t4 * t64) * t6 * t44 / 0.2880D4 + (-0.90D2 * t85 * t81 + t4 * (0.
     #60D2 * lh * t62 - 0.2884936567583026D3 - 0.120D3 * t60 * lh) - 0.1
     #5D2 * t84 * t79 * t3 * 0.3141592653589793D1 - t80 * 0.314159265358
     #9793D1 * t64) * t6 * t7 / 0.2880D4 + (0.90D2 * t4 * t6 * (t44 - t1
     #15 * t7 + (t117 - t123 * z * t7) * t37) - t137) * t73 * t140 / 0.1
     #440D4 - (0.90D2 * t4 * t6 * (t146 * t44 - t148 * t7 / 0.2D1) - 0.1
     #80D3 * t4 * t48 * (-t44 + t146 * t7) - t67) * t140 / 0.1440D4 + (0
     #.90D2 * t4 * t6 * ((t117 - t167 * z * t7) * t37 + t44 - t176 * t7)
     # - t137) * t73 * t184 / 0.1440D4 + t187 * t134 * t73 * t189 / 0.8D
     #1 - (0.90D2 * t4 * t6 * (-t44 + t196 * t7) + t51) * t140 * t184 / 
     #0.720D3 + (0.90D2 * t4 * t6 * (-t209 * t44 + t211 * t7 / 0.2D1) - 
     #0.180D3 * t4 * t48 * (t44 - t209 * t7) + t67) * t184 / 0.1440D4
      t227 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t226)
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t226)
      t231 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t226)
      t233 = t2 * x1
      t234 = -0.1D1 + x1
      t235 = x1 * z
      t236 = 0.1D1 - x1 + t235
      t237 = 0.1D1 / t236
      t239 = t2 * t234 * t237
      t240 = s * t12
      t242 = x1 * t234 * t237
      t243 = t240 * t242
      t244 = -t234
      t245 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t244, 0.10D1, 0
     #.10D1, x4)
      t248 = t234 ** 2
      t254 = log(-0.4D1 * t11 * t172 * t16 * t248 * t237 * t23)
      t256 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t244, 0.10D1, 0
     #.10D1, x4)
      t260 = x3 * x1
      t261 = t260 * z
      t262 = 0.3D1 * t261
      t263 = x1 * t9
      t264 = x3 * t9
      t265 = t264 * x1
      t266 = x3 * t163
      t268 = 0.2D1 * t266 * z
      t269 = t266 * t9
      t270 = x3 * t236
      t272 = Sqrt(-t270 * t22)
      t276 = 0.2D1 * t260
      t277 = -z + t235 - t262 - t263 + t265 + t268 - t269 - x3 + 0.2D1 *
     # t30 * t272 * z + t276 - t266
      t278 = 0.1D1 / t277
      t280 = t248 * t237
      t284 = log(0.4D1 * t164 * t17 * t280)
      t293 = -z * t256 * t236 * t278 - t256
      t300 = (0.90D2 * t4 * t6 * (-(z * t245 - t254 * z * t256) * t236 *
     # t278 - t245 + t284 * t256) - 0.180D3 * t4 * t48 * t293) * t73 * t
     #184 / 0.1440D4
      t304 = t187 * t293 * t73 * t189 / 0.8D1
      t305 = t193 * t13
      t309 = log(0.4D1 * t305 * t119 * t280)
      t321 = (0.90D2 * t4 * t6 * (t245 - t309 * t256) - 0.180D3 * t4 * t
     #48 * t256) * t140 * t184 / 0.720D3
      t327 = log(0.4D1 * t206 * t13 * t16 * t237 * t248)
      t329 = t327 ** 2
      t332 = t327 * t245 - t329 * t256 / 0.2D1
      t337 = -t245 + t327 * t256
      t342 = t4 * t65 * t256
      t346 = t300 + t304 - t321 + (0.90D2 * t4 * t6 * t332 - 0.180D3 * t
     #4 * t48 * t337 - t342) * t184 / 0.1440D4
      t347 = FJET(XB1, XB2, s, 0.0D0, t233, -t239, 0.0D0, -t243, t346)
      t350 = x2 * t1 * s
      t351 = -0.1D1 + x2
      t353 = t351 * t1 * s
      t354 = -t351
      t355 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t354, 0
     #.10D1, x4)
      t356 = t119 * t351
      t359 = log(-0.4D1 * t118 * t356)
      t360 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t354, 0
     #.10D1, x4)
      t368 = 0.180D3 * t4 * t48 * t360
      t375 = log(-0.4D1 * t143 * t356)
      t377 = t375 ** 2
      t400 = log(-0.4D1 * t305 * t356)
      t410 = (0.90D2 * t4 * t6 * (-t355 + t359 * t360) + t368) * t73 * t
     #140 / 0.1440D4 - (0.90D2 * t4 * t6 * (-t375 * t355 + t377 * t360 /
     # 0.2D1) - 0.180D3 * t4 * t48 * (t355 - t375 * t360) + t4 * t65 * t
     #360) * t140 / 0.1440D4 - t187 * t360 * t73 * t189 / 0.8D1 - (0.90D
     #2 * t4 * t6 * (t355 - t400 * t360) - t368) * t140 * t184 / 0.720D3
      t411 = FJET(XB1, XB2, s, 0.0D0, t350, 0.0D0, -t353, 0.0D0, t410)
      t413 = x2 * x3
      t416 = Sqrt(x3 * t351 * t22)
      t417 = t30 * t416
      t419 = 0.2D1 * t417 * x2
      t421 = 0.1D1 - x3 + t413
      t422 = 0.1D1 / t421
      t424 = t2 * (0.1D1 - x3 - x2 + t413 + t112 + t419) * t422
      t429 = t2 * x2 * (-0.1D1 + t413 + 0.2D1 * t417) * t422
      t430 = x2 * z
      t431 = -x2 + t430 - z
      t432 = t22 * t422
      t433 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t354, -
     #t432, x4)
      t437 = t421 ** 2
      t443 = log(0.4D1 * t112 * t17 * t10 * t351 * t22 / t437)
      t445 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t354, -
     #t432, x4)
      t449 = t413 * z
      t450 = t112 * z
      t456 = 0.1D1 / (x2 - t430 - t449 + t450 + x3 - t112 + z - t419 - 0
     #.2D1 * t417 * z + 0.2D1 * t417 * t430)
      t461 = t6 * t431
      t462 = t445 * t456
      t472 = t73 * t140 * t184
      t476 = (-0.90D2 * t4 * t6 * (t431 * t433 - t443 * t431 * t445) * t
     #456 + 0.180D3 * t4 * lh * t461 * t462) * t73 * t140 / 0.1440D4 - t
     #4 * t461 * t462 * t472 / 0.8D1
      t477 = FJET(XB1, XB2, s, 0.0D0, t424, 0.0D0, -t429, 0.0D0, t476)
      t480 = t1 * t234
      t482 = t351 * s * t480 * t237
      t484 = x2 * s * t480
      t486 = t240 * t351 * t242
      t487 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t244, t354, 0.1
     #0D1, x4)
      t491 = t187 * t487 * t73 * t189 / 0.8D1
      t492 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t244, t354, 0.1
     #0D1, x4)
      t499 = log(-0.4D1 * t193 * t17 * t10 * t237 * t248 * t351)
      t501 = -t492 + t499 * t487
      t507 = 0.180D3 * t4 * t48 * t487
      t512 = t491 - (0.90D2 * t4 * t6 * t501 + t507) * t140 * t184 / 0.7
     #20D3
      t513 = FJET(XB1, XB2, s, 0.0D0, t482, t233, -t484, t486, t512)
      t515 = FJET(XB1, XB2, s, 0.0D0, -t353, 0.0D0, t350, 0.0D0, t410)
      t517 = FJET(XB1, XB2, s, 0.0D0, -t239, t233, 0.0D0, -t243, t346)
      t519 = FJET(XB1, XB2, s, 0.0D0, -t429, 0.0D0, t424, 0.0D0, t476)
      t521 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t226)
      t534 = t300 + t304 - t321 + (0.90D2 * t4 * t6 * t332 - 0.180D3 * t
     #4 * t48 * t337 - t342) * t184 / 0.1440D4
      t535 = FJET(XB1, XB2, s, t233, 0.0D0, 0.0D0, -t239, -t243, t534)
      t537 = t227 * t226 + t229 * t226 + t231 * t226 + t347 * t346 + t41
     #1 * t410 + t477 * t476 + t513 * t512 + t515 * t410 + t517 * t346 +
     # t519 * t476 + t521 * t226 + t535 * t534
      t538 = FJET(XB1, XB2, s, t233, -t484, 0.0D0, t482, t486, t512)
      t540 = FJET(XB1, XB2, s, t350, 0.0D0, -t353, 0.0D0, 0.0D0, t410)
      t542 = FJET(XB1, XB2, s, t424, 0.0D0, -t429, 0.0D0, 0.0D0, t476)
      t544 = FJET(XB1, XB2, s, t482, 0.0D0, -t484, t233, t486, t512)
      t547 = t233 * t413 * t422
      t548 = t2 * t234
      t549 = t112 * x1
      t550 = t112 * t235
      t553 = Sqrt(t270 * t351 * t22)
      t554 = t30 * t553
      t556 = 0.2D1 * t554 * x2
      t560 = t548 * (t112 - t549 - x2 + t413 + 0.1D1 - x3 + t550 + t556)
     # * t237 * t422
      t564 = t22 * s * t1 * x1 * t422
      t570 = t548 * x2 * (-0.1D1 + t413 + x1 - t260 - t235 + t261 + 0.2D
     #1 * t554) * t237 * t422
      t571 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t244, t354, -t4
     #32, x4)
      t575 = x2 * x1
      t576 = t575 * z
      t577 = x2 - t575 + z - t430 + t576
      t589 = -x2 + t235 - t263 + t276 - t266 + t112 + t430 - 0.2D1 * t55
     #4 * t575 - 0.2D1 * t554 * t430 + 0.2D1 * t413 * t235 - t264 * t575
     # - 0.2D1 * t266 * t430 + t266 * t9 * x2 + t550 - z - x3 + t449
      t590 = x2 * t163
      t603 = -t450 - t262 + t265 + t268 - t269 - t590 + 0.2D1 * t554 * t
     #576 + 0.2D1 * t575 - t549 + t556 - 0.3D1 * t576 - t413 * x1 + t266
     # * x2 + t575 * t9 + 0.2D1 * t590 * z - t590 * t9 + 0.2D1 * t554 * 
     #z
      t605 = 0.1D1 / (t589 + t603)
      t609 = t4 * t6 * t571 * t236 * t577 * t605 * t472 / 0.8D1
      t610 = FJET(XB1, XB2, s, t547, -t560, -t564, t570, t486, t609)
      t613 = 0.3141592653589793D1 * t6 * t571
      t617 = t236 * t577 * t605 * t472
      t620 = FJET(XB1, XB2, s, t570, -t564, -t560, t547, t486, t609)
      t625 = FJET(XB1, XB2, s, -t353, 0.0D0, t350, 0.0D0, 0.0D0, t410)
      t627 = FJET(XB1, XB2, s, -t239, 0.0D0, 0.0D0, t233, -t243, t534)
      t637 = t491 - (0.90D2 * t4 * t6 * t501 + t507) * t140 * t184 / 0.7
     #20D3
      t638 = FJET(XB1, XB2, s, -t484, t233, t482, 0.0D0, t486, t637)
      t640 = FJET(XB1, XB2, s, -t429, 0.0D0, t424, 0.0D0, 0.0D0, t476)
      t642 = FJET(XB1, XB2, s, -t564, t570, t547, -t560, t486, t609)
      t647 = FJET(XB1, XB2, s, -t560, t547, t570, -t564, t486, t609)
      t652 = t538 * t512 + t540 * t410 + t542 * t476 + t544 * t512 + t61
     #0 * t3 * t613 * t617 / 0.8D1 + t620 * t3 * t613 * t617 / 0.8D1 + t
     #625 * t410 + t627 * t534 + t638 * t637 + t640 * t476 + t642 * t3 *
     # t613 * t617 / 0.8D1 + t647 * t3 * t613 * t617 / 0.8D1
      rrqqbar2gght5s1e0 = t537 + t652

      end function



      doubleprecision function rrqqbar2gght5s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = x3 * t10
      t12 = t1 ** 2
      t13 = t12 ** 2
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = -0.1D1 + x3
      t26 = log(-0.4D1 * t11 * t17 / t21)
      t28 = cos(t14)
      t30 = Sqrt(-x3 * t21)
      t35 = 0.1D1 / (-x3 - z + 0.2D1 * t28 * t30 * z)
      t41 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t45 = lh * t6
      t48 = 0.180D3 * t4 * t45 * t7
      t54 = 0.1D1 / x3
      t62 = log(0.4D1 * t10 * t13 * t16)
      t63 = t62 * t3
      t73 = t62 ** 2
      t77 = lh ** 2
      t79 = 0.3141592653589793D1 ** 2
      t87 = t4 * t6
      t91 = (t7 + z * t7 * t35) * t54
      t92 = 0.1D1 / x2
      t96 = x2 ** 2
      t97 = t96 * t13
      t98 = t16 * t10
      t101 = log(0.4D1 * t97 * t98)
      t111 = 0.1D1 / x1
      t115 = x1 ** 2
      t116 = t10 * t115
      t119 = log(0.4D1 * t116 * t17)
      t131 = (0.90D2 * t4 * t6 * t7 * (-t20 - t26 * z * t35) + (0.90D2 *
     # t4 * t6 * t41 - t48) * (0.1D1 + z * t35)) * t54 / 0.2880D4 + (-0.
     #180D3 * t4 * lh - 0.90D2 * t63 * 0.3141592653589793D1) * t6 * t41 
     #/ 0.2880D4 + (0.180D3 * t63 * 0.3141592653589793D1 * lh + 0.45D2 *
     # t73 * t3 * 0.3141592653589793D1 + t4 * (0.180D3 * t77 - 0.30D2 * 
     #t79)) * t6 * t7 / 0.2880D4 + t87 * t91 * t92 / 0.16D2 - (0.90D2 * 
     #t4 * t6 * (-t41 + t101 * t7) + t48) * t92 / 0.1440D4 + t87 * t7 * 
     #t92 * t111 / 0.8D1 + (0.90D2 * t4 * t6 * (t41 - t119 * t7) - t48) 
     #* t111 / 0.1440D4 + t87 * t91 * t111 / 0.16D2
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t131)
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t131)
      t136 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t131)
      t138 = t2 * x1
      t139 = -0.1D1 + x1
      t140 = x1 * z
      t141 = 0.1D1 - x1 + t140
      t142 = 0.1D1 / t141
      t144 = t2 * t139 * t142
      t145 = s * t12
      t147 = x1 * t139 * t142
      t148 = t145 * t147
      t149 = -t139
      t150 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t149, 0.10D1, 0
     #.10D1, x4)
      t154 = t87 * t150 * t92 * t111 / 0.8D1
      t155 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t149, 0.10D1, 0
     #.10D1, x4)
      t158 = t139 ** 2
      t162 = log(0.4D1 * t116 * t13 * t16 * t142 * t158)
      t164 = -t155 + t162 * t150
      t170 = 0.180D3 * t4 * t45 * t150
      t175 = x3 * x1
      t181 = x3 * t115
      t187 = Sqrt(-x3 * t141 * t21)
      t192 = -z + t140 - 0.3D1 * t175 * z - x1 * t9 + x3 * t9 * x1 + 0.2
     #D1 * t181 * z - t181 * t9 - x3 + 0.2D1 * t28 * t187 * z + 0.2D1 * 
     #t175 - t181
      t200 = t87 * (-z * t150 * t141 / t192 - t150) * t54 * t111 / 0.16D
     #2
      t201 = -t154 + (0.90D2 * t4 * t6 * t164 + t170) * t111 / 0.1440D4 
     #+ t200
      t202 = FJET(XB1, XB2, s, 0.0D0, t138, -t144, 0.0D0, -t148, t201)
      t205 = x2 * t1 * s
      t206 = -0.1D1 + x2
      t208 = t206 * t1 * s
      t209 = -t206
      t210 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t209, 0
     #.10D1, x4)
      t215 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t209, 0
     #.10D1, x4)
      t219 = log(-0.4D1 * t97 * t98 * t206)
      t235 = -t87 * t210 * t54 * t92 / 0.16D2 - (0.90D2 * t4 * t6 * (t21
     #5 - t219 * t210) - 0.180D3 * t4 * t45 * t210) * t92 / 0.1440D4 - t
     #87 * t210 * t92 * t111 / 0.8D1
      t236 = FJET(XB1, XB2, s, 0.0D0, t205, 0.0D0, -t208, 0.0D0, t235)
      t238 = x2 * x3
      t239 = t96 * x3
      t242 = Sqrt(x3 * t206 * t21)
      t243 = t28 * t242
      t245 = 0.2D1 * t243 * x2
      t248 = 0.1D1 / (0.1D1 - x3 + t238)
      t250 = t2 * (0.1D1 - x3 - x2 + t238 + t239 + t245) * t248
      t255 = t2 * x2 * (-0.1D1 + t238 + 0.2D1 * t243) * t248
      t256 = x2 * z
      t257 = -x2 + t256 - z
      t261 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t209, -
     #t21 * t248, x4)
      t269 = 0.1D1 / (x2 - t256 - t238 * z + t239 * z + x3 - t239 + z - 
     #t245 - 0.2D1 * t243 * z + 0.2D1 * t243 * t256)
      t274 = t4 * t6 * t257 * t261 * t269 * t54 * t92 / 0.16D2
      t275 = FJET(XB1, XB2, s, 0.0D0, t250, 0.0D0, -t255, 0.0D0, -t274)
      t277 = 0.3141592653589793D1 * t6
      t282 = t257 * t261 * t269 * t54 * t92
      t286 = t1 * t139
      t288 = t206 * s * t286 * t142
      t290 = x2 * s * t286
      t292 = t145 * t206 * t147
      t293 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t149, t209, 0.1
     #0D1, x4)
      t297 = t87 * t293 * t92 * t111 / 0.8D1
      t298 = FJET(XB1, XB2, s, 0.0D0, t288, t138, -t290, t292, t297)
      t303 = t6 * t293 * t92 * t111
      t306 = FJET(XB1, XB2, s, 0.0D0, -t208, 0.0D0, t205, 0.0D0, t235)
      t308 = FJET(XB1, XB2, s, 0.0D0, -t144, t138, 0.0D0, -t148, t201)
      t310 = FJET(XB1, XB2, s, 0.0D0, -t255, 0.0D0, t250, 0.0D0, -t274)
      t315 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t131)
      t324 = -t154 + (0.90D2 * t4 * t6 * t164 + t170) * t111 / 0.1440D4 
     #+ t200
      t325 = FJET(XB1, XB2, s, t138, 0.0D0, 0.0D0, -t144, -t148, t324)
      t327 = FJET(XB1, XB2, s, t138, -t290, 0.0D0, t288, t292, t297)
      t332 = FJET(XB1, XB2, s, t205, 0.0D0, -t208, 0.0D0, 0.0D0, t235)
      t334 = FJET(XB1, XB2, s, t250, 0.0D0, -t255, 0.0D0, 0.0D0, -t274)
      t339 = FJET(XB1, XB2, s, t288, 0.0D0, -t290, t138, t292, t297)
      t344 = FJET(XB1, XB2, s, -t208, 0.0D0, t205, 0.0D0, 0.0D0, t235)
      t346 = FJET(XB1, XB2, s, -t144, 0.0D0, 0.0D0, t138, -t148, t324)
      t348 = FJET(XB1, XB2, s, -t290, t138, t288, 0.0D0, t292, t297)
      t353 = FJET(XB1, XB2, s, -t255, 0.0D0, t250, 0.0D0, 0.0D0, -t274)
      rrqqbar2gght5s1em1 = t132 * t131 + t134 * t131 + t136 * t131 + t20
     #2 * t201 + t236 * t235 - t275 * t3 * t277 * t282 / 0.16D2 + t298 *
     # t3 * 0.3141592653589793D1 * t303 / 0.8D1 + t306 * t235 + t308 * t
     #201 - t310 * t3 * t277 * t282 / 0.16D2 + t315 * t131 + t325 * t324
     # + t327 * t3 * 0.3141592653589793D1 * t303 / 0.8D1 + t332 * t235 -
     # t334 * t3 * t277 * t282 / 0.16D2 + t339 * t3 * 0.3141592653589793
     #D1 * t303 / 0.8D1 + t344 * t235 + t346 * t324 + t348 * t3 * 0.3141
     #592653589793D1 * t303 / 0.8D1 - t353 * t3 * t277 * t282 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t26 = t6 * t8
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t35 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 
     #0.10D1, x4)
      t41 = z ** 2
      t43 = t1 ** 2
      t44 = t43 ** 2
      t46 = Sin(t9)
      t47 = t46 ** 2
      t50 = log(0.4D1 / t41 * t44 * t47)
      t58 = t4 * t6 * t8 * (0.1D1 + z / (-x3 - z + 0.2D1 * t10 * t13 * z
     #)) / x3 / 0.32D2 + t4 * t26 * t27 / 0.16D2 + t4 * t26 * t31 / 0.16
     #D2 + t4 * t6 * t35 / 0.32D2 + (-0.180D3 * t4 * lh - 0.90D2 * t50 *
     # t3 * 0.3141592653589793D1) * t6 * t8 / 0.2880D4
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t58)
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t58)
      t63 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t58)
      t65 = t2 * x1
      t66 = -0.1D1 + x1
      t69 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t71 = t2 * t66 * t69
      t75 = s * t43 * x1 * t66 * t69
      t77 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, -t66, 0.10D1, 0.
     #10D1, x4)
      t79 = t6 * t77 * t31
      t81 = t4 * t79 / 0.16D2
      t82 = FJET(XB1, XB2, s, 0.0D0, t65, -t71, 0.0D0, -t75, -t81)
      t88 = x2 * t1 * s
      t89 = -0.1D1 + x2
      t91 = t89 * t1 * s
      t93 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, -t89, 0.
     #10D1, x4)
      t95 = t6 * t93 * t27
      t97 = t4 * t95 / 0.16D2
      t98 = FJET(XB1, XB2, s, 0.0D0, t88, 0.0D0, -t91, 0.0D0, -t97)
      t103 = FJET(XB1, XB2, s, 0.0D0, -t91, 0.0D0, t88, 0.0D0, -t97)
      t108 = FJET(XB1, XB2, s, 0.0D0, -t71, t65, 0.0D0, -t75, -t81)
      t113 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t58)
      t115 = FJET(XB1, XB2, s, t65, 0.0D0, 0.0D0, -t71, -t75, -t81)
      t120 = FJET(XB1, XB2, s, t88, 0.0D0, -t91, 0.0D0, 0.0D0, -t97)
      t125 = FJET(XB1, XB2, s, -t91, 0.0D0, t88, 0.0D0, 0.0D0, -t97)
      t130 = FJET(XB1, XB2, s, -t71, 0.0D0, 0.0D0, t65, -t75, -t81)
      rrqqbar2gght5s1em2 = t59 * t58 + t61 * t58 + t63 * t58 - t82 * t3 
     #* 0.3141592653589793D1 * t79 / 0.16D2 - t98 * t3 * 0.3141592653589
     #793D1 * t95 / 0.16D2 - t103 * t3 * 0.3141592653589793D1 * t95 / 0.
     #16D2 - t108 * t3 * 0.3141592653589793D1 * t79 / 0.16D2 + t113 * t5
     #8 - t115 * t3 * 0.3141592653589793D1 * t79 / 0.16D2 - t120 * t3 * 
     #0.3141592653589793D1 * t95 / 0.16D2 - t125 * t3 * 0.31415926535897
     #93D1 * t95 / 0.16D2 - t130 * t3 * 0.3141592653589793D1 * t79 / 0.1
     #6D2

      end function



      doubleprecision function rrqqbar2gght5s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t10 = t3 * 0.3141592653589793D1 * t6 * t7 / 0.32D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = 0.3141592653589793D1 * t6 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s1em3 = t11 * t3 * t14 / 0.32D2 + t16 * t3 * t14 / 0.
     #32D2 + t19 * t3 * t14 / 0.32D2 + t22 * t3 * t14 / 0.32D2

      end function



      doubleprecision function rrqqbar2gght5s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      rrqqbar2gght5s1em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght5s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t3 = x2 * t1 * s
      t4 = -0.1D1 + x2
      t6 = t4 * t1 * s
      t7 = 0.1D1 / t1
      t8 = t7 * 0.3141592653589793D1
      t9 = s ** 2
      t10 = 0.1D1 / t9
      t11 = t8 * t10
      t12 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t14 = 0.180D3 * lh
      t15 = x1 ** 2
      t16 = x3 * t15
      t17 = t16 * x2
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = x4 * 0.3141592653589793D1
      t21 = Sin(t20)
      t22 = t21 ** 2
      t23 = t19 * t22
      t24 = z ** 2
      t25 = 0.1D1 / t24
      t26 = t4 ** 2
      t27 = t25 * t26
      t31 = log(0.4D1 * t17 * t23 * t27)
      t34 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t37 = 0.1D1 / x3
      t39 = 0.1D1 / x2
      t40 = 0.1D1 / x1
      t41 = t39 * t40
      t45 = x2 * t15
      t46 = t45 * t19
      t47 = t22 * t25
      t48 = t47 * t26
      t51 = log(0.4D1 * t46 * t48)
      t53 = t51 ** 2
      t60 = lh * t10
      t66 = lh ** 2
      t67 = 0.180D3 * t66
      t68 = 0.3141592653589793D1 ** 2
      t69 = 0.30D2 * t68
      t70 = -t67 + t69
      t71 = t70 * t10
      t73 = t8 * t71 * t34
      t78 = x2 * x3
      t79 = t78 * t19
      t82 = log(0.4D1 * t79 * t48)
      t84 = t82 ** 2
      t100 = x2 * t19
      t103 = log(0.4D1 * t100 * t48)
      t106 = t103 ** 2
      t113 = 0.60D2 * lh * t68
      t115 = 0.120D3 * t66 * lh
      t126 = t11 * (-0.90D2 * t12 + (t14 + 0.90D2 * t31) * t34) * t37 * 
     #t41 / 0.720D3 + (-0.90D2 * t8 * t10 * (-t51 * t12 + t53 * t34 / 0.
     #2D1) + 0.180D3 * t8 * t60 * (t12 - t51 * t34) + t73) * t39 * t40 /
     # 0.720D3 + (-0.90D2 * t8 * t10 * (-t82 * t12 + t84 * t34 / 0.2D1) 
     #+ 0.180D3 * t8 * t60 * (t12 - t82 * t34) + t73) * t37 * t39 / 0.14
     #40D4 + t8 * t10 * ((-0.180D3 * t103 * lh - 0.45D2 * t106 - t67 + t
     #69) * t12 + (0.90D2 * t106 * lh - t113 + 0.2884936567583026D3 + t1
     #15 + 0.15D2 * t106 * t103 - t103 * t70) * t34) * t39 / 0.1440D4
      t127 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t6, 0.0D0, t126)
      t129 = t1 * s
      t130 = x3 * t25
      t133 = log(0.4D1 * t130 * t23)
      t136 = t133 ** 2
      t139 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t145 = -t70
      t148 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t156 = t25 * t19 * t22
      t158 = log(0.4D1 * t156)
      t159 = t158 ** 2
      t160 = t159 * t7
      t161 = 0.3141592653589793D1 * lh
      t164 = t113 - 0.2884936567583026D3 - t115
      t167 = t159 * t158 * t7
      t170 = t158 * t7
      t171 = 0.3141592653589793D1 * t145
      t184 = t68 ** 2
      t185 = t66 ** 2
      t191 = t159 ** 2
      t199 = t15 * t19
      t203 = log(0.4D1 * t130 * t199 * t22)
      t205 = t203 ** 2
      t218 = t8 * t71 * t148
      t223 = t25 * t15
      t226 = log(0.4D1 * t223 * t23)
      t231 = t226 ** 2
      t242 = -t164 * t10
      t258 = log(0.4D1 * t17 * t156)
      t269 = log(0.4D1 * t45 * t156)
      t271 = t269 ** 2
      t289 = log(0.4D1 * t78 * t156)
      t291 = t289 ** 2
      t309 = log(0.4D1 * t100 * t47)
      t312 = t309 ** 2
      t328 = t8 * t10 * ((0.180D3 * t133 * lh + 0.45D2 * t136 + t67 - t6
     #9) * t139 + (-0.90D2 * t136 * lh + t113 - 0.2884936567583026D3 - t
     #115 - 0.15D2 * t136 * t133 - t133 * t145) * t148) * t37 / 0.1440D4
     # + (-0.90D2 * t160 * t161 + t8 * t164 - 0.15D2 * t167 * 0.31415926
     #53589793D1 - t170 * t171) * t10 * t139 / 0.1440D4 + (0.30D2 * t167
     # * t161 + t160 * t171 / 0.2D1 - t170 * 0.3141592653589793D1 * t164
     # + t8 * (0.5769873135166051D3 * lh + t184 + 0.60D2 * t185 - 0.60D2
     # * t66 * t68) + 0.15D2 / 0.4D1 * t191 * t7 * 0.3141592653589793D1)
     # * t10 * t148 / 0.1440D4 + (-0.90D2 * t8 * t10 * (t203 * t139 - t2
     #05 * t148 / 0.2D1) + 0.180D3 * t8 * t60 * (-t139 + t203 * t148) - 
     #t218) * t37 * t40 / 0.720D3 + (t8 * t71 * (-t139 + t226 * t148) - 
     #0.90D2 * t8 * t10 * (-t231 * t139 / 0.2D1 + t231 * t226 * t148 / 0
     #.6D1) - t8 * t242 * t148 + 0.180D3 * t8 * t60 * (t226 * t139 - t23
     #1 * t148 / 0.2D1)) * t40 / 0.720D3 + t11 * (0.90D2 * t139 - (t14 +
     # 0.90D2 * t258) * t148) * t37 * t41 / 0.720D3 + (-0.90D2 * t8 * t1
     #0 * (t269 * t139 - t271 * t148 / 0.2D1) + 0.180D3 * t8 * t60 * (-t
     #139 + t269 * t148) - t218) * t39 * t40 / 0.720D3 + (-0.90D2 * t8 *
     # t10 * (t289 * t139 - t291 * t148 / 0.2D1) + 0.180D3 * t8 * t60 * 
     #(-t139 + t289 * t148) - t218) * t37 * t39 / 0.1440D4 + t8 * t10 * 
     #((0.180D3 * t309 * lh + 0.45D2 * t312 + t67 - t69) * t139 + (-0.90
     #D2 * t312 * lh + t113 - 0.2884936567583026D3 - t115 - 0.15D2 * t31
     #2 * t309 - t309 * t145) * t148) * t39 / 0.1440D4
      t329 = FJET(XB1, XB2, s, t129, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t328)
      t331 = -0.1D1 + x1
      t333 = x1 * z
      t334 = 0.1D1 - x1 + t333
      t335 = 0.1D1 / t334
      t337 = t129 * t331 * x2 * t335
      t339 = t1 * t331
      t340 = t4 * s * t339
      t341 = t129 * x1
      t346 = s * t18 * x2 * x1 * t331 * t335
      t347 = -t331
      t348 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t347, x2, 0.10D
     #1, x4)
      t350 = t78 * t199
      t351 = t331 ** 2
      t352 = t351 * t335
      t357 = log(0.4D1 * t350 * t47 * t352 * t26)
      t360 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t347, x2, 0.10D
     #1, x4)
      t365 = t11 * (0.90D2 * t348 - (t14 + 0.90D2 * t357) * t360) * t37 
     #* t41
      t367 = t25 * t335
      t372 = log(0.4D1 * t45 * t23 * t367 * t351 * t26)
      t374 = t372 ** 2
      t377 = -t372 * t348 + t374 * t360 / 0.2D1
      t382 = t348 - t372 * t360
      t387 = t8 * t71 * t360
      t392 = t365 / 0.720D3 + (0.90D2 * t8 * t10 * t377 - 0.180D3 * t8 *
     # t60 * t382 - t387) * t39 * t40 / 0.720D3
      t393 = FJET(XB1, XB2, s, -t337, 0.0D0, t340, t341, -t346, t392)
      t395 = t129 * t331
      t396 = x3 * x1
      t397 = t396 * z
      t398 = cos(t20)
      t400 = -0.1D1 + x3
      t403 = Sqrt(-x3 * t334 * x2 * t400)
      t404 = t398 * t403
      t405 = 0.2D1 * t404
      t408 = -0.1D1 + t78
      t409 = 0.1D1 / t408
      t412 = t395 * t4 * (-t78 - 0.1D1 + x3 + x1 - t396 - t333 + t397 + 
     #t405) * t335 * t409
      t413 = t400 * s
      t414 = t1 * x1
      t416 = t413 * t414 * t409
      t417 = x2 ** 2
      t418 = t417 * x3
      t419 = t78 * x1
      t421 = t418 * x1
      t422 = 0.3D1 * t78
      t423 = t78 * t333
      t425 = t418 * t333
      t427 = 0.2D1 * t404 * x2
      t428 = -t418 - t397 - 0.2D1 * t419 + t421 + t422 - x2 - x3 + t396 
     #+ 0.2D1 * t423 - t425 - t405 + t427
      t431 = t395 * t428 * t335 * t409
      t434 = t341 * x3 * t4 * t409
      t435 = t400 * t409
      t436 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t347, x2, t435,
     # x4)
      t438 = t418 * z
      t440 = x2 * x1
      t445 = x2 * z
      t458 = 0.1D1 + t438 + 0.4D1 * t423 - t425 - 0.2D1 * t404 * t440 - 
     #0.2D1 * t404 * t333 - 0.2D1 * t404 * t445 - x3 * t24 * t440 - 0.2D
     #1 * t16 * t445 + t16 * t24 * x2 - x2 - t405 + 0.2D1 * t440 - 0.2D1
     # * t15 * z + t15 * t24 - t45
      t460 = t440 * z
      t469 = t78 * z
      t473 = 0.2D1 * t78
      t474 = -0.3D1 * t419 + t421 + t427 - 0.3D1 * t460 + t17 + t440 * t
     #24 + 0.2D1 * t45 * z - t45 * t24 + 0.2D1 * t404 * x1 - 0.2D1 * x1 
     #+ t15 - t469 + 0.2D1 * t404 * t460 + 0.2D1 * t333 + t473 - t418 + 
     #t445
      t476 = 0.1D1 / (t458 + t474)
      t480 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t347, x2, t435,
     # x4)
      t482 = t351 * t19
      t483 = t78 * t400
      t485 = t408 ** 2
      t486 = 0.1D1 / t485
      t492 = log(-0.4D1 * t482 * t483 * t48 * t335 * t486 * t15)
      t498 = -0.180D3 * t334 * t436 * t476 * lh + 0.90D2 * (t334 * t480 
     #- t492 * t334 * t436) * t476
      t501 = x1 - t333 - t440 + t460 - t445 - 0.1D1 + x2
      t505 = t8 * t10 * t498 * t501 * t37 * t41 / 0.720D3
      t506 = FJET(XB1, XB2, s, t412, t416, -t431, t434, -t346, t505)
      t508 = 0.3141592653589793D1 * t10
      t513 = t498 * t501 * t37 * t39 * t40
      t520 = log(0.4D1 * t130 * t15 * t23 * t352)
      t521 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t347, 0.0D0, 0.
     #10D1, x4)
      t523 = t520 ** 2
      t524 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t347, 0.0D0, 0.
     #10D1, x4)
      t537 = t8 * t71 * t524
      t546 = log(0.4D1 * t223 * t19 * t22 * t335 * t351)
      t551 = t546 ** 2
      t573 = t47 * t352
      t576 = log(0.4D1 * t350 * t573)
      t586 = log(0.4D1 * t46 * t573)
      t588 = t586 ** 2
      t604 = (-0.90D2 * t8 * t10 * (-t520 * t521 + t523 * t524 / 0.2D1) 
     #+ 0.180D3 * t8 * t60 * (t521 - t520 * t524) + t537) * t37 * t40 / 
     #0.720D3 + (t8 * t71 * (t521 - t546 * t524) - 0.90D2 * t8 * t10 * (
     #t551 * t521 / 0.2D1 - t551 * t546 * t524 / 0.6D1) + t8 * t242 * t5
     #24 + 0.180D3 * t8 * t60 * (-t546 * t521 + t551 * t524 / 0.2D1)) * 
     #t40 / 0.720D3 + t11 * (-0.90D2 * t521 + (t14 + 0.90D2 * t576) * t5
     #24) * t37 * t41 / 0.720D3 + (-0.90D2 * t8 * t10 * (-t586 * t521 + 
     #t588 * t524 / 0.2D1) + 0.180D3 * t8 * t60 * (t521 - t586 * t524) +
     # t537) * t39 * t40 / 0.720D3
      t605 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t341, -t395, 0.0D0, t604)
      t608 = t129 * t331 * x3
      t609 = t129 * t396
      t610 = t413 * t339
      t611 = t413 * t414
      t617 = log(-0.4D1 * t16 * t23 * t367 * t351 * t400)
      t618 = -t400
      t619 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t347, 0.0D0, t6
     #18, x4)
      t621 = t617 ** 2
      t622 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t347, 0.0D0, t6
     #18, x4)
      t625 = -t617 * t619 + t621 * t622 / 0.2D1
      t630 = t619 - t617 * t622
      t635 = t8 * t71 * t622
      t649 = log(-0.4D1 * t482 * t335 * x2 * x3 * t400 * t47 * t15)
      t655 = t11 * (-0.180D3 * t622 * lh + 0.90D2 * t619 - 0.90D2 * t649
     # * t622) * t37 * t41
      t657 = (0.90D2 * t8 * t10 * t625 - 0.180D3 * t8 * t60 * t630 - t63
     #5) * t37 * t40 / 0.720D3 + t655 / 0.720D3
      t658 = FJET(XB1, XB2, s, -t608, t609, t610, -t611, 0.0D0, t657)
      t672 = t365 / 0.720D3 + (0.90D2 * t8 * t10 * t377 - 0.180D3 * t8 *
     # t60 * t382 - t387) * t39 * t40 / 0.720D3
      t673 = FJET(XB1, XB2, s, t340, t341, -t337, 0.0D0, -t346, t672)
      t675 = t129 * t400
      t676 = t129 * x3
      t678 = t47 * t400
      t681 = log(-0.4D1 * x3 * t19 * t678)
      t684 = t681 ** 2
      t687 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t618, x4)
      t695 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t618, x4)
      t705 = log(-0.4D1 * t16 * t19 * t678)
      t707 = t705 ** 2
      t720 = t8 * t71 * t695
      t730 = log(-0.4D1 * t17 * t23 * t25 * t400)
      t741 = log(-0.4D1 * t79 * t678)
      t743 = t741 ** 2
      t759 = t8 * t10 * ((-0.180D3 * t681 * lh - 0.45D2 * t684 - t67 + t
     #69) * t687 + (0.90D2 * t684 * lh - t113 + 0.2884936567583026D3 + t
     #115 + 0.15D2 * t684 * t681 - t681 * t70) * t695) * t37 / 0.1440D4 
     #+ (-0.90D2 * t8 * t10 * (-t705 * t687 + t707 * t695 / 0.2D1) + 0.1
     #80D3 * t8 * t60 * (t687 - t705 * t695) + t720) * t37 * t40 / 0.720
     #D3 + t11 * (-0.90D2 * t687 + (t14 + 0.90D2 * t730) * t695) * t37 *
     # t41 / 0.720D3 + (-0.90D2 * t8 * t10 * (-t741 * t687 + t743 * t695
     # / 0.2D1) + 0.180D3 * t8 * t60 * (t687 - t741 * t695) + t720) * t3
     #7 * t39 / 0.1440D4
      t760 = FJET(XB1, XB2, s, 0.0D0, -t675, 0.0D0, t676, 0.0D0, t759)
      t762 = FJET(XB1, XB2, s, 0.0D0, -t6, 0.0D0, t3, 0.0D0, t126)
      t764 = FJET(XB1, XB2, s, 0.0D0, -t337, t341, t340, -t346, t672)
      t766 = FJET(XB1, XB2, s, -t611, t610, t609, -t608, 0.0D0, t657)
      t768 = FJET(XB1, XB2, s, -t675, 0.0D0, t676, 0.0D0, 0.0D0, t759)
      t770 = FJET(XB1, XB2, s, 0.0D0, t676, 0.0D0, -t675, 0.0D0, t759)
      t772 = FJET(XB1, XB2, s, t341, -t395, 0.0D0, 0.0D0, 0.0D0, t604)
      t774 = FJET(XB1, XB2, s, -t431, t434, t412, t416, -t346, t505)
      t779 = Sqrt(-t483)
      t780 = t398 * t779
      t781 = 0.2D1 * t780
      t785 = t129 * t4 * (-t78 - 0.1D1 + x3 + t781) * t409
      t787 = 0.2D1 * t780 * x2
      t790 = t129 * (-x2 - x3 + t422 - t418 - t781 + t787) * t409
      t791 = -0.1D1 + x2 - t445
      t793 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t43
     #5, x4)
      t800 = log(-0.4D1 * t350 * t47 * t26 * t400 * t486)
      t803 = -t791
      t805 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t43
     #5, x4)
      t813 = 0.1D1 / (-t438 + t418 - t445 + t469 - t787 + x2 - t473 + 0.
     #2D1 * t780 * t445 - 0.1D1 + t781)
      t823 = log(-0.4D1 * t78 * t23 * t27 * t400 * t486)
      t824 = t823 * t803
      t826 = t823 ** 2
      t852 = t8 * t10 * (0.90D2 * t791 * t793 + (t14 + 0.90D2 * t800) * 
     #t803 * t805) * t813 * t37 * t41 / 0.720D3 + (-0.90D2 * t8 * t10 * 
     #(-t824 * t793 + t826 * t803 * t805 / 0.2D1) * t813 + 0.180D3 * t8 
     #* lh * t10 * (t803 * t793 - t824 * t805) * t813 + t8 * t70 * t10 *
     # t803 * t805 * t813) * t37 * t39 / 0.1440D4
      t853 = FJET(XB1, XB2, s, -t785, 0.0D0, t790, 0.0D0, 0.0D0, t852)
      t855 = t127 * t126 + t329 * t328 + t393 * t392 + t506 * t7 * t508 
     #* t513 / 0.720D3 + t605 * t604 + t658 * t657 + t673 * t672 + t760 
     #* t759 + t762 * t126 + t764 * t672 + t766 * t657 + t768 * t759 + t
     #770 * t759 + t772 * t604 + t774 * t7 * t508 * t513 / 0.720D3 + t85
     #3 * t852
      t856 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t129, 0.0D0, t328)
      t858 = FJET(XB1, XB2, s, 0.0D0, t129, 0.0D0, 0.0D0, 0.0D0, t328)
      t860 = FJET(XB1, XB2, s, t416, t412, t434, -t431, -t346, t505)
      t865 = FJET(XB1, XB2, s, t790, 0.0D0, -t785, 0.0D0, 0.0D0, t852)
      t867 = FJET(XB1, XB2, s, t3, 0.0D0, -t6, 0.0D0, 0.0D0, t126)
      t869 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t129, 0.0D0, 0.0D0, t328)
      t871 = FJET(XB1, XB2, s, -t395, t341, 0.0D0, 0.0D0, 0.0D0, t604)
      t873 = FJET(XB1, XB2, s, -t6, 0.0D0, t3, 0.0D0, 0.0D0, t126)
      t875 = FJET(XB1, XB2, s, 0.0D0, -t785, 0.0D0, t790, 0.0D0, t852)
      t877 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t395, t341, 0.0D0, t604)
      t879 = FJET(XB1, XB2, s, t434, -t431, t416, t412, -t346, t505)
      t884 = FJET(XB1, XB2, s, t676, 0.0D0, -t675, 0.0D0, 0.0D0, t759)
      t898 = (0.90D2 * t8 * t10 * t625 - 0.180D3 * t8 * t60 * t630 - t63
     #5) * t37 * t40 / 0.720D3 + t655 / 0.720D3
      t899 = FJET(XB1, XB2, s, t610, -t611, -t608, t609, 0.0D0, t898)
      t901 = FJET(XB1, XB2, s, 0.0D0, t790, 0.0D0, -t785, 0.0D0, t852)
      t903 = FJET(XB1, XB2, s, t609, -t608, -t611, t610, 0.0D0, t898)
      t905 = FJET(XB1, XB2, s, t341, t340, 0.0D0, -t337, -t346, t672)
      t907 = t856 * t328 + t858 * t328 + t860 * t7 * t508 * t513 / 0.720
     #D3 + t865 * t852 + t867 * t126 + t869 * t328 + t871 * t604 + t873 
     #* t126 + t875 * t852 + t877 * t604 + t879 * t7 * t508 * t513 / 0.7
     #20D3 + t884 * t759 + t899 * t898 + t901 * t852 + t903 * t898 + t90
     #5 * t672
      rrqqbar2gght5s2e1 = t855 + t907

      end function



      doubleprecision function rrqqbar2gght5s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = -0.1D1 + x1
      t5 = x1 * z
      t6 = 0.1D1 - x1 + t5
      t7 = 0.1D1 / t6
      t9 = t2 * t3 * x2 * t7
      t10 = t2 * x1
      t11 = -0.1D1 + x2
      t13 = t1 * t3
      t14 = t11 * s * t13
      t15 = t1 ** 2
      t20 = s * t15 * x2 * x1 * t3 * t7
      t21 = 0.1D1 / t1
      t22 = t21 * 0.3141592653589793D1
      t23 = s ** 2
      t24 = 0.1D1 / t23
      t25 = t22 * t24
      t26 = -t3
      t27 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t26, x2, 0.10D1,
     # x4)
      t28 = 0.1D1 / x3
      t30 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t32 = t30 * t31
      t35 = t25 * t27 * t28 * t32 / 0.8D1
      t36 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t26, x2, 0.10D1,
     # x4)
      t37 = x1 ** 2
      t38 = x2 * t37
      t39 = t15 ** 2
      t40 = x4 * 0.3141592653589793D1
      t41 = Sin(t40)
      t42 = t41 ** 2
      t43 = t39 * t42
      t45 = z ** 2
      t46 = 0.1D1 / t45
      t47 = t46 * t7
      t48 = t3 ** 2
      t49 = t11 ** 2
      t54 = log(0.4D1 * t38 * t43 * t47 * t48 * t49)
      t56 = -t36 + t54 * t27
      t60 = lh * t24
      t63 = 0.180D3 * t22 * t60 * t27
      t68 = t35 + (-0.90D2 * t22 * t24 * t56 - t63) * t30 * t31 / 0.720D
     #3
      t69 = FJET(XB1, XB2, s, 0.0D0, -t9, t10, t14, -t20, t68)
      t71 = t2 * t3
      t72 = x2 * x3
      t73 = x3 * x1
      t74 = t73 * z
      t75 = cos(t40)
      t77 = -0.1D1 + x3
      t80 = Sqrt(-x3 * t6 * x2 * t77)
      t81 = t75 * t80
      t82 = 0.2D1 * t81
      t85 = -0.1D1 + t72
      t86 = 0.1D1 / t85
      t89 = t71 * t11 * (-t72 - 0.1D1 + x3 + x1 - t73 - t5 + t74 + t82) 
     #* t7 * t86
      t90 = t77 * s
      t91 = t1 * x1
      t93 = t90 * t91 * t86
      t94 = x2 ** 2
      t95 = t94 * x3
      t96 = t72 * x1
      t98 = t95 * x1
      t99 = 0.3D1 * t72
      t100 = t72 * t5
      t102 = t95 * t5
      t104 = 0.2D1 * t81 * x2
      t105 = -t95 - t74 - 0.2D1 * t96 + t98 + t99 - x2 - x3 + t73 + 0.2D
     #1 * t100 - t102 - t82 + t104
      t108 = t71 * t105 * t7 * t86
      t111 = t10 * x3 * t11 * t86
      t113 = t77 * t86
      t114 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t26, x2, t113, 
     #x4)
      t118 = x2 * z
      t119 = x2 * x1
      t124 = t95 * z
      t125 = t72 * z
      t131 = 0.1D1 - x2 - 0.2D1 * x1 + t118 - t95 + 0.2D1 * t119 - 0.2D1
     # * t37 * z + t37 * t45 - t38 + t124 - t125 + t37 - t102 + 0.4D1 * 
     #t100 - 0.2D1 * t81 * t118 - 0.2D1 * t81 * t5
      t136 = x3 * t37
      t143 = t119 * z
      t154 = 0.2D1 * t72
      t155 = -0.2D1 * t81 * t119 - x3 * t45 * t119 + t136 * t45 * x2 - 0
     #.2D1 * t136 * t118 + 0.2D1 * t5 - t82 + t98 - 0.3D1 * t96 + t104 -
     # 0.3D1 * t143 + 0.2D1 * t81 * x1 - t38 * t45 + 0.2D1 * t38 * z + t
     #119 * t45 + t136 * x2 + 0.2D1 * t81 * t143 + t154
      t157 = 0.1D1 / (t131 + t155)
      t158 = x1 - t5 - t119 + t143 - t118 - 0.1D1 + x2
      t161 = t28 * t30 * t31
      t164 = t22 * t24 * t6 * t114 * t157 * t158 * t161 / 0.8D1
      t165 = FJET(XB1, XB2, s, t89, t93, -t108, t111, -t20, t164)
      t168 = 0.3141592653589793D1 * t24 * t6
      t172 = t114 * t157 * t158 * t161
      t175 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, 0.1
     #0D1, x4)
      t176 = x3 * t46
      t178 = t48 * t7
      t182 = log(0.4D1 * t176 * t37 * t43 * t178)
      t183 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, 0.1
     #0D1, x4)
      t191 = 0.180D3 * t22 * t60 * t183
      t200 = t38 * t39
      t201 = t42 * t46
      t205 = log(0.4D1 * t200 * t201 * t178)
      t215 = t46 * t37
      t221 = log(0.4D1 * t215 * t39 * t42 * t7 * t48)
      t223 = t221 ** 2
      t235 = lh ** 2
      t236 = 0.180D3 * t235
      t237 = 0.3141592653589793D1 ** 2
      t238 = 0.30D2 * t237
      t239 = -t236 + t238
      t240 = t239 * t24
      t246 = (-0.90D2 * t22 * t24 * (t175 - t182 * t183) + t191) * t28 *
     # t31 / 0.720D3 - t25 * t183 * t28 * t32 / 0.8D1 + (-0.90D2 * t22 *
     # t24 * (t175 - t205 * t183) + t191) * t30 * t31 / 0.720D3 + (-0.90
     #D2 * t22 * t24 * (-t221 * t175 + t223 * t183 / 0.2D1) + 0.180D3 * 
     #t22 * t60 * (t175 - t221 * t183) + t22 * t240 * t183) * t31 / 0.72
     #0D3
      t247 = FJET(XB1, XB2, s, -t71, t10, 0.0D0, 0.0D0, 0.0D0, t246)
      t249 = 0.180D3 * lh
      t252 = log(0.4D1 * t176 * t43)
      t255 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t259 = t252 ** 2
      t262 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t270 = t46 * t39 * t42
      t272 = log(0.4D1 * t270)
      t273 = t272 * t21
      t274 = 0.3141592653589793D1 * lh
      t277 = t272 ** 2
      t278 = t277 * t21
      t281 = -t239
      t307 = log(0.4D1 * t72 * t270)
      t315 = 0.180D3 * t22 * t60 * t262
      t320 = x2 * t39
      t323 = log(0.4D1 * t320 * t201)
      t329 = t323 ** 2
      t342 = log(0.4D1 * t176 * t37 * t39 * t42)
      t358 = log(0.4D1 * t38 * t270)
      t370 = log(0.4D1 * t215 * t43)
      t372 = t370 ** 2
      t389 = t22 * t24 * ((-t249 - 0.90D2 * t252) * t255 + (0.180D3 * t2
     #52 * lh + 0.45D2 * t259 + t236 - t238) * t262) * t28 / 0.1440D4 + 
     #(0.180D3 * t273 * t274 + 0.45D2 * t278 * 0.3141592653589793D1 + t2
     #2 * t281) * t24 * t255 / 0.1440D4 + (-0.90D2 * t278 * t274 + t22 *
     # (0.60D2 * lh * t237 - 0.2884936567583026D3 - 0.120D3 * t235 * lh)
     # - 0.15D2 * t277 * t272 * t21 * 0.3141592653589793D1 - t273 * 0.31
     #41592653589793D1 * t281) * t24 * t262 / 0.1440D4 + (-0.90D2 * t22 
     #* t24 * (-t255 + t307 * t262) - t315) * t28 * t30 / 0.1440D4 + t22
     # * t24 * ((-t249 - 0.90D2 * t323) * t255 + (0.180D3 * t323 * lh + 
     #0.45D2 * t329 + t236 - t238) * t262) * t30 / 0.1440D4 + (-0.90D2 *
     # t22 * t24 * (-t255 + t342 * t262) - t315) * t28 * t31 / 0.720D3 +
     # t25 * t262 * t28 * t32 / 0.8D1 + (-0.90D2 * t22 * t24 * (-t255 + 
     #t358 * t262) - t315) * t30 * t31 / 0.720D3 + (-0.90D2 * t22 * t24 
     #* (t370 * t255 - t372 * t262 / 0.2D1) + 0.180D3 * t22 * t60 * (-t2
     #55 + t370 * t262) - t22 * t240 * t262) * t31 / 0.720D3
      t390 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t389)
      t392 = t2 * t73
      t394 = t2 * t3 * x3
      t395 = t90 * t91
      t396 = t90 * t13
      t397 = -t77
      t398 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, t39
     #7, x4)
      t404 = log(-0.4D1 * t136 * t43 * t47 * t48 * t77)
      t405 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t26, 0.0D0, t39
     #7, x4)
      t407 = -t398 + t404 * t405
      t413 = 0.180D3 * t22 * t60 * t405
      t421 = t25 * t405 * t28 * t32 / 0.8D1
      t422 = (-0.90D2 * t22 * t24 * t407 - t413) * t28 * t31 / 0.720D3 +
     # t421
      t423 = FJET(XB1, XB2, s, t392, -t394, -t395, t396, 0.0D0, t422)
      t425 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t389)
      t427 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t389)
      t429 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t389)
      t439 = (-0.90D2 * t22 * t24 * t407 - t413) * t28 * t31 / 0.720D3 +
     # t421
      t440 = FJET(XB1, XB2, s, -t395, t396, t392, -t394, 0.0D0, t439)
      t443 = Sqrt(-t72 * t77)
      t444 = t75 * t443
      t445 = 0.2D1 * t444
      t447 = 0.2D1 * t444 * x2
      t450 = t2 * (-x2 - x3 + t99 - t95 - t445 + t447) * t86
      t454 = t2 * t11 * (-t72 - 0.1D1 + x3 + t445) * t86
      t455 = 0.1D1 - x2 + t118
      t456 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t11
     #3, x4)
      t460 = t85 ** 2
      t466 = log(-0.4D1 * t72 * t43 * t46 * t49 * t77 / t460)
      t468 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t11
     #3, x4)
      t475 = 0.1D1 / (-t124 + t95 - t118 + t125 - t447 + x2 - t154 + 0.2
     #D1 * t444 * t118 - 0.1D1 + t445)
      t481 = t468 * t475
      t496 = (-0.90D2 * t22 * t24 * (t455 * t456 - t466 * t455 * t468) *
     # t475 + 0.180D3 * t22 * lh * t24 * t455 * t481) * t28 * t30 / 0.14
     #40D4 - t22 * t24 * t455 * t481 * t161 / 0.8D1
      t497 = FJET(XB1, XB2, s, 0.0D0, t450, 0.0D0, -t454, 0.0D0, t496)
      t499 = t2 * t77
      t500 = t2 * x3
      t502 = t201 * t77
      t505 = log(-0.4D1 * x3 * t39 * t502)
      t508 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t397, x4)
      t512 = t505 ** 2
      t515 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t397, x4)
      t522 = t72 * t39
      t525 = log(-0.4D1 * t522 * t502)
      t533 = 0.180D3 * t22 * t60 * t515
      t541 = log(-0.4D1 * t136 * t39 * t502)
      t555 = t22 * t24 * ((t249 + 0.90D2 * t505) * t508 + (-0.180D3 * t5
     #05 * lh - 0.45D2 * t512 - t236 + t238) * t515) * t28 / 0.1440D4 + 
     #(-0.90D2 * t22 * t24 * (t508 - t525 * t515) + t533) * t28 * t30 / 
     #0.1440D4 + (-0.90D2 * t22 * t24 * (t508 - t541 * t515) + t533) * t
     #28 * t31 / 0.720D3 - t25 * t515 * t28 * t32 / 0.8D1
      t556 = FJET(XB1, XB2, s, 0.0D0, -t499, 0.0D0, t500, 0.0D0, t555)
      t566 = t35 + (-0.90D2 * t22 * t24 * t56 - t63) * t30 * t31 / 0.720
     #D3
      t567 = FJET(XB1, XB2, s, -t9, 0.0D0, t14, t10, -t20, t566)
      t569 = FJET(XB1, XB2, s, 0.0D0, -t454, 0.0D0, t450, 0.0D0, t496)
      t572 = x2 * t1 * s
      t574 = t11 * t1 * s
      t575 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t576 = t201 * t49
      t579 = log(0.4D1 * t522 * t576)
      t580 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.1
     #0D1, x4)
      t588 = 0.180D3 * t22 * t60 * t580
      t595 = log(0.4D1 * t320 * t576)
      t601 = t595 ** 2
      t616 = log(0.4D1 * t200 * t576)
      t626 = (-0.90D2 * t22 * t24 * (t575 - t579 * t580) + t588) * t28 *
     # t30 / 0.1440D4 + t22 * t24 * ((t249 + 0.90D2 * t595) * t575 + (-0
     #.180D3 * t595 * lh - 0.45D2 * t601 - t236 + t238) * t580) * t30 / 
     #0.1440D4 - t25 * t580 * t28 * t32 / 0.8D1 + (-0.90D2 * t22 * t24 *
     # (t575 - t616 * t580) + t588) * t30 * t31 / 0.720D3
      t627 = FJET(XB1, XB2, s, 0.0D0, t572, 0.0D0, -t574, 0.0D0, t626)
      t629 = FJET(XB1, XB2, s, t14, t10, -t9, 0.0D0, -t20, t68)
      t631 = FJET(XB1, XB2, s, t572, 0.0D0, -t574, 0.0D0, 0.0D0, t626)
      t633 = t69 * t68 + t165 * t21 * t168 * t172 / 0.8D1 + t247 * t246 
     #+ t390 * t389 + t423 * t422 + t425 * t389 + t427 * t389 + t429 * t
     #389 + t440 * t439 + t497 * t496 + t556 * t555 + t567 * t566 + t569
     # * t496 + t627 * t626 + t629 * t68 + t631 * t626
      t634 = FJET(XB1, XB2, s, t396, -t395, -t394, t392, 0.0D0, t422)
      t636 = FJET(XB1, XB2, s, t111, -t108, t93, t89, -t20, t164)
      t641 = FJET(XB1, XB2, s, -t108, t111, t89, t93, -t20, t164)
      t646 = FJET(XB1, XB2, s, t10, t14, 0.0D0, -t9, -t20, t68)
      t648 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t71, t10, 0.0D0, t246)
      t650 = FJET(XB1, XB2, s, -t499, 0.0D0, t500, 0.0D0, 0.0D0, t555)
      t652 = FJET(XB1, XB2, s, 0.0D0, t500, 0.0D0, -t499, 0.0D0, t555)
      t654 = FJET(XB1, XB2, s, t93, t89, t111, -t108, -t20, t164)
      t659 = FJET(XB1, XB2, s, t500, 0.0D0, -t499, 0.0D0, 0.0D0, t555)
      t661 = FJET(XB1, XB2, s, 0.0D0, -t574, 0.0D0, t572, 0.0D0, t626)
      t663 = FJET(XB1, XB2, s, t450, 0.0D0, -t454, 0.0D0, 0.0D0, t496)
      t665 = FJET(XB1, XB2, s, -t574, 0.0D0, t572, 0.0D0, 0.0D0, t626)
      t667 = FJET(XB1, XB2, s, t10, -t71, 0.0D0, 0.0D0, 0.0D0, t246)
      t669 = FJET(XB1, XB2, s, -t394, t392, t396, -t395, 0.0D0, t439)
      t671 = FJET(XB1, XB2, s, -t454, 0.0D0, t450, 0.0D0, 0.0D0, t496)
      t673 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t10, -t71, 0.0D0, t246)
      t675 = t634 * t422 + t636 * t21 * t168 * t172 / 0.8D1 + t641 * t21
     # * t168 * t172 / 0.8D1 + t646 * t68 + t648 * t246 + t650 * t555 + 
     #t652 * t555 + t654 * t21 * t168 * t172 / 0.8D1 + t659 * t555 + t66
     #1 * t626 + t663 * t496 + t665 * t626 + t667 * t246 + t669 * t439 +
     # t671 * t496 + t673 * t246
      rrqqbar2gght5s2e0 = t633 + t675

      end function



      doubleprecision function rrqqbar2gght5s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = t2 * x1
      t6 = 0.1D1 / t1
      t7 = t6 * 0.3141592653589793D1
      t8 = s ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = -t3
      t12 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t11, 0.0D0, 0.10
     #D1, x4)
      t13 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, t11, 0.0D0, 0.10
     #D1, x4)
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t22 = x1 ** 2
      t23 = t21 * t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t27 = x4 * 0.3141592653589793D1
      t28 = Sin(t27)
      t29 = t28 ** 2
      t32 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t34 = t3 ** 2
      t38 = log(0.4D1 * t23 * t25 * t29 * t32 * t34)
      t44 = lh * t9
      t51 = 0.1D1 / x3
      t56 = -t10 * t12 * t13 * t15 / 0.8D1 + (-0.90D2 * t7 * t9 * (t19 -
     # t38 * t12) + 0.180D3 * t7 * t44 * t12) * t15 / 0.720D3 - t10 * t1
     #2 * t51 * t15 / 0.8D1
      t57 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t56)
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t56)
      t62 = x2 * t1 * s
      t63 = -0.1D1 + x2
      t65 = t63 * t1 * s
      t66 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t71 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t73 = 0.180D3 * lh
      t74 = x2 * t25
      t75 = t29 * t21
      t76 = t63 ** 2
      t80 = log(0.4D1 * t74 * t75 * t76)
      t93 = -t10 * t66 * t51 * t13 / 0.16D2 + t7 * t9 * (-0.90D2 * t71 +
     # (t73 + 0.90D2 * t80) * t66) * t13 / 0.1440D4 - t10 * t66 * t13 * 
     #t15 / 0.8D1
      t94 = FJET(XB1, XB2, s, t62, 0.0D0, -t65, 0.0D0, 0.0D0, t93)
      t96 = t2 * x3
      t97 = -0.1D1 + x3
      t98 = t2 * t97
      t99 = -t97
      t100 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t99, x4)
      t106 = log(-0.4D1 * x3 * t25 * t75 * t97)
      t109 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #t99, x4)
      t116 = t109 * t51
      t123 = t7 * t9 * (-0.90D2 * t100 + (t73 + 0.90D2 * t106) * t109) *
     # t51 / 0.1440D4 - t10 * t116 * t13 / 0.16D2 - t10 * t116 * t15 / 0
     #.8D1
      t124 = FJET(XB1, XB2, s, 0.0D0, t96, 0.0D0, -t98, 0.0D0, t123)
      t126 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t127 = 0.90D2 * t126
      t129 = t25 * t29
      t132 = log(0.4D1 * x3 * t21 * t129)
      t135 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 
     #0.10D1, x4)
      t147 = log(0.4D1 * t21 * t25 * t29)
      t148 = t147 * t6
      t158 = t147 ** 2
      t162 = lh ** 2
      t164 = 0.3141592653589793D1 ** 2
      t172 = t135 * t51
      t178 = log(0.4D1 * t74 * t75)
      t193 = log(0.4D1 * t23 * t129)
      t208 = t7 * t9 * (t127 + (-t73 - 0.90D2 * t132) * t135) * t51 / 0.
     #1440D4 + (-0.180D3 * t7 * lh - 0.90D2 * t148 * 0.3141592653589793D
     #1) * t9 * t126 / 0.1440D4 + (0.180D3 * t148 * 0.3141592653589793D1
     # * lh + 0.45D2 * t158 * t6 * 0.3141592653589793D1 + t7 * (0.180D3 
     #* t162 - 0.30D2 * t164)) * t9 * t135 / 0.1440D4 + t10 * t172 * t13
     # / 0.16D2 + t7 * t9 * (t127 + (-t73 - 0.90D2 * t178) * t135) * t13
     # / 0.1440D4 + t10 * t135 * t13 * t15 / 0.8D1 + (-0.90D2 * t7 * t9 
     #* (-t126 + t193 * t135) - 0.180D3 * t7 * t44 * t135) * t15 / 0.720
     #D3 + t10 * t172 * t15 / 0.8D1
      t209 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t208)
      t211 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t65, 0.0D0, t93)
      t213 = FJET(XB1, XB2, s, -t98, 0.0D0, t96, 0.0D0, 0.0D0, t123)
      t215 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t208)
      t217 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t56)
      t219 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t208)
      t223 = t2 * t3 * x2 * t32
      t225 = t1 * t3
      t226 = t63 * s * t225
      t231 = s * t24 * x2 * x1 * t3 * t32
      t232 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t11, x2, 0.10D1
     #, x4)
      t236 = t10 * t232 * t13 * t15 / 0.8D1
      t237 = FJET(XB1, XB2, s, -t223, 0.0D0, t226, t5, -t231, t236)
      t242 = t9 * t232 * t13 * t15
      t246 = t2 * x1 * x3
      t248 = t2 * t3 * x3
      t249 = t97 * s
      t251 = t249 * t1 * x1
      t252 = t249 * t225
      t253 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, t11, 0.0D0, t99
     #, x4)
      t257 = t10 * t253 * t51 * t15 / 0.8D1
      t258 = FJET(XB1, XB2, s, t246, -t248, -t251, t252, 0.0D0, t257)
      t263 = t9 * t253 * t51 * t15
      t266 = FJET(XB1, XB2, s, -t248, t246, t252, -t251, 0.0D0, t257)
      t271 = FJET(XB1, XB2, s, t5, t226, 0.0D0, -t223, -t231, t236)
      t276 = t57 * t56 + t59 * t56 + t94 * t93 + t124 * t123 + t209 * t2
     #08 + t211 * t93 + t213 * t123 + t215 * t208 + t217 * t56 + t219 * 
     #t208 + t237 * t6 * 0.3141592653589793D1 * t242 / 0.8D1 + t258 * t6
     # * 0.3141592653589793D1 * t263 / 0.8D1 + t266 * t6 * 0.31415926535
     #89793D1 * t263 / 0.8D1 + t271 * t6 * 0.3141592653589793D1 * t242 /
     # 0.8D1
      t277 = FJET(XB1, XB2, s, -t251, t252, t246, -t248, 0.0D0, t257)
      t282 = FJET(XB1, XB2, s, t226, t5, -t223, 0.0D0, -t231, t236)
      t287 = FJET(XB1, XB2, s, 0.0D0, -t223, t5, t226, -t231, t236)
      t292 = FJET(XB1, XB2, s, t252, -t251, -t248, t246, 0.0D0, t257)
      t297 = x2 * x3
      t299 = x2 ** 2
      t300 = t299 * x3
      t301 = cos(t27)
      t303 = Sqrt(-t297 * t97)
      t304 = t301 * t303
      t305 = 0.2D1 * t304
      t307 = 0.2D1 * t304 * x2
      t310 = 0.1D1 / (-0.1D1 + t297)
      t312 = t2 * (-x2 - x3 + 0.3D1 * t297 - t300 - t305 + t307) * t310
      t316 = t2 * t63 * (-t297 - 0.1D1 + x3 + t305) * t310
      t317 = x2 * z
      t318 = 0.1D1 - x2 + t317
      t322 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t97
     # * t310, x4)
      t329 = 0.1D1 / (-t300 * z + t300 - t317 + t297 * z - t307 + x2 - 0
     #.2D1 * t297 + 0.2D1 * t304 * t317 - 0.1D1 + t305)
      t334 = t7 * t9 * t318 * t322 * t329 * t51 * t13 / 0.16D2
      t335 = FJET(XB1, XB2, s, t312, 0.0D0, -t316, 0.0D0, 0.0D0, -t334)
      t337 = 0.3141592653589793D1 * t9
      t342 = t318 * t322 * t329 * t51 * t13
      t345 = FJET(XB1, XB2, s, 0.0D0, -t316, 0.0D0, t312, 0.0D0, -t334)
      t350 = FJET(XB1, XB2, s, -t316, 0.0D0, t312, 0.0D0, 0.0D0, -t334)
      t355 = FJET(XB1, XB2, s, 0.0D0, t312, 0.0D0, -t316, 0.0D0, -t334)
      t360 = FJET(XB1, XB2, s, t96, 0.0D0, -t98, 0.0D0, 0.0D0, t123)
      t362 = FJET(XB1, XB2, s, 0.0D0, -t98, 0.0D0, t96, 0.0D0, t123)
      t364 = FJET(XB1, XB2, s, -t65, 0.0D0, t62, 0.0D0, 0.0D0, t93)
      t366 = FJET(XB1, XB2, s, 0.0D0, -t65, 0.0D0, t62, 0.0D0, t93)
      t368 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t56)
      t370 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t208)
      t372 = t277 * t6 * 0.3141592653589793D1 * t263 / 0.8D1 + t282 * t6
     # * 0.3141592653589793D1 * t242 / 0.8D1 + t287 * t6 * 0.31415926535
     #89793D1 * t242 / 0.8D1 + t292 * t6 * 0.3141592653589793D1 * t263 /
     # 0.8D1 - t335 * t6 * t337 * t342 / 0.16D2 - t345 * t6 * t337 * t34
     #2 / 0.16D2 - t350 * t6 * t337 * t342 / 0.16D2 - t355 * t6 * t337 *
     # t342 / 0.16D2 + t360 * t123 + t362 * t123 + t364 * t93 + t366 * t
     #93 + t368 * t56 + t370 * t208
      rrqqbar2gght5s2em1 = t276 + t372

      end function



      doubleprecision function rrqqbar2gght5s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0
     #.10D1, x4)
      t27 = z ** 2
      t29 = t1 ** 2
      t30 = t29 ** 2
      t33 = Sin(x4 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t30 * t34)
      t45 = t4 * t8 * t9 / 0.16D2 + t4 * t8 * t13 / 0.16D2 + t4 * t8 * t
     #17 / 0.8D1 + t4 * t6 * t21 / 0.16D2 + (-0.180D3 * t4 * lh - 0.90D2
     # * t37 * t3 * 0.3141592653589793D1) * t6 * t7 / 0.1440D4
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t45)
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t45)
      t50 = t2 * x1
      t51 = -0.1D1 + x1
      t52 = t2 * t51
      t54 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, -t51, 0.0D0, 0.1
     #0D1, x4)
      t56 = t6 * t54 * t17
      t58 = t4 * t56 / 0.8D1
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t50, -t52, 0.0D0, -t58)
      t64 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t52, t50, 0.0D0, -t58)
      t69 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t45)
      t71 = t2 * x3
      t72 = -0.1D1 + x3
      t73 = t2 * t72
      t75 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, -
     #t72, x4)
      t77 = t6 * t75 * t9
      t79 = t4 * t77 / 0.16D2
      t80 = FJET(XB1, XB2, s, 0.0D0, t71, 0.0D0, -t73, 0.0D0, -t79)
      t86 = x2 * t1 * s
      t89 = (-0.1D1 + x2) * t1 * s
      t90 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10
     #D1, x4)
      t92 = t6 * t90 * t13
      t94 = t4 * t92 / 0.16D2
      t95 = FJET(XB1, XB2, s, 0.0D0, t86, 0.0D0, -t89, 0.0D0, -t94)
      t100 = FJET(XB1, XB2, s, 0.0D0, -t89, 0.0D0, t86, 0.0D0, -t94)
      t105 = FJET(XB1, XB2, s, 0.0D0, -t73, 0.0D0, t71, 0.0D0, -t79)
      t110 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t45)
      t112 = FJET(XB1, XB2, s, t50, -t52, 0.0D0, 0.0D0, 0.0D0, -t58)
      t117 = FJET(XB1, XB2, s, t71, 0.0D0, -t73, 0.0D0, 0.0D0, -t79)
      t122 = FJET(XB1, XB2, s, t86, 0.0D0, -t89, 0.0D0, 0.0D0, -t94)
      t127 = FJET(XB1, XB2, s, -t73, 0.0D0, t71, 0.0D0, 0.0D0, -t79)
      t132 = FJET(XB1, XB2, s, -t52, t50, 0.0D0, 0.0D0, 0.0D0, -t58)
      t137 = FJET(XB1, XB2, s, -t89, 0.0D0, t86, 0.0D0, 0.0D0, -t94)
      rrqqbar2gght5s2em2 = t46 * t45 + t48 * t45 - t59 * t3 * 0.31415926
     #53589793D1 * t56 / 0.8D1 - t64 * t3 * 0.3141592653589793D1 * t56 /
     # 0.8D1 + t69 * t45 - t80 * t3 * 0.3141592653589793D1 * t77 / 0.16D
     #2 - t95 * t3 * 0.3141592653589793D1 * t92 / 0.16D2 - t100 * t3 * 0
     #.3141592653589793D1 * t92 / 0.16D2 - t105 * t3 * 0.314159265358979
     #3D1 * t77 / 0.16D2 + t110 * t45 - t112 * t3 * 0.3141592653589793D1
     # * t56 / 0.8D1 - t117 * t3 * 0.3141592653589793D1 * t77 / 0.16D2 -
     # t122 * t3 * 0.3141592653589793D1 * t92 / 0.16D2 - t127 * t3 * 0.3
     #141592653589793D1 * t77 / 0.16D2 - t132 * t3 * 0.3141592653589793D
     #1 * t56 / 0.8D1 - t137 * t3 * 0.3141592653589793D1 * t92 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t10 = t3 * 0.3141592653589793D1 * t6 * t7 / 0.16D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = 0.3141592653589793D1 * t6 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s2em3 = t11 * t3 * t14 / 0.16D2 + t16 * t3 * t14 / 0.
     #16D2 + t19 * t3 * t14 / 0.16D2 + t22 * t3 * t14 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      rrqqbar2gght5s2em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght5s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = lh * t6
      t8 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t12 = lh ** 2
      t14 = 0.3141592653589793D1 ** 2
      t16 = -0.180D3 * t12 + 0.30D2 * t14
      t17 = t16 * t6
      t18 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t22 = z ** 2
      t24 = 0.1D1 / t22 / z
      t25 = x3 * t24
      t26 = t1 ** 2
      t27 = t26 ** 2
      t28 = x4 * 0.3141592653589793D1
      t29 = Sin(t28)
      t30 = t29 ** 2
      t31 = t27 * t30
      t34 = log(0.4D1 * t25 * t31)
      t35 = -0.1D1 + x3
      t36 = 0.1D1 / t35
      t40 = log(-0.4D1 * t25 * t31 * t36)
      t42 = cos(t28)
      t43 = x3 * z
      t45 = Sqrt(-t43 * t35)
      t49 = 0.1D1 / (-z - x3 + 0.2D1 * t42 * t45)
      t54 = t40 ** 2
      t58 = t34 ** 2
      t71 = -0.60D2 * lh * t14 + 0.2884936567583026D3 + 0.120D3 * t12 * 
     #lh
      t72 = t71 * t6
      t74 = t4 * t72 * t18
      t92 = 0.1D1 / x3
      t96 = t24 * t27 * t30
      t98 = log(0.4D1 * t96)
      t99 = t98 ** 2
      t100 = t99 * t3
      t101 = 0.3141592653589793D1 * lh
      t106 = t99 * t98 * t3
      t109 = t98 * t3
      t110 = 0.3141592653589793D1 * t16
      t123 = t14 ** 2
      t124 = t12 ** 2
      t130 = t99 ** 2
      t138 = x1 ** 2
      t139 = x3 * t138
      t142 = log(0.4D1 * t139 * t96)
      t144 = t27 * x3
      t145 = t144 * t36
      t146 = t30 * t24
      t150 = log(-0.4D1 * t145 * t146 * t138)
      t152 = t150 ** 2
      t158 = t142 ** 2
      t175 = t18 * z * t49
      t181 = 0.1D1 / x1
      t184 = t138 * t27
      t187 = log(0.4D1 * t184 * t146)
      t192 = t187 ** 2
      t212 = x2 ** 2
      t213 = t212 * t138
      t217 = log(-0.4D1 * t145 * t146 * t213)
      t222 = x3 * t212
      t223 = t222 * t138
      t226 = log(0.4D1 * t223 * t96)
      t228 = -0.1D1 + x2
      t233 = log(-0.4D1 * t223 * t31 * t24 * t228)
      t234 = -t228
      t235 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t234, 0.
     #10D1, x4)
      t237 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t234, 0.
     #10D1, x4)
      t242 = -t175 + t235 - t18
      t248 = 0.1D1 / x2
      t249 = t248 * t181
      t252 = t213 * t27
      t256 = log(-0.4D1 * t252 * t146 * t228)
      t260 = log(0.4D1 * t213 * t96)
      t262 = t256 ** 2
      t265 = t260 ** 2
      t278 = t18 - t235
      t286 = t31 * t228
      t289 = log(-0.4D1 * t222 * t24 * t286)
      t291 = t289 ** 2
      t297 = log(-0.4D1 * t145 * t146 * t212)
      t299 = t297 ** 2
      t307 = log(0.4D1 * t222 * t96)
      t309 = t307 ** 2
      t332 = t24 * t212
      t335 = log(0.4D1 * t332 * t31)
      t339 = log(-0.4D1 * t332 * t286)
      t344 = t339 ** 2
      t350 = t335 ** 2
      t375 = ((0.180D3 * t4 * t7 * t8 + t4 * t17 * t18) * (t34 + t40 * z
     # * t49) - 0.90D2 * t4 * t6 * t18 * (t54 * t40 * z * t49 / 0.6D1 + 
     #t58 * t34 / 0.6D1) + (t4 * t17 * t8 + t74) * (-0.1D1 - z * t49) + 
     #(-0.90D2 * t4 * t6 * t8 + 0.180D3 * t4 * t7 * t18) * (-t58 / 0.2D1
     # - t54 * z * t49 / 0.2D1)) * t92 / 0.2880D4 - (0.90D2 * t100 * t10
     #1 + t4 * t71 + 0.15D2 * t106 * 0.3141592653589793D1 - t109 * t110)
     # * t6 * t8 / 0.2880D4 - (-0.30D2 * t106 * t101 + t100 * t110 / 0.2
     #D1 - t109 * 0.3141592653589793D1 * t71 + t4 * (-0.5769873135166051
     #D3 * lh - t123 - 0.60D2 * t124 + 0.60D2 * t12 * t14) - 0.15D2 / 0.
     #4D1 * t130 * t3 * 0.3141592653589793D1) * t6 * t18 / 0.2880D4 + (-
     #0.90D2 * t4 * t6 * (t142 * t8 - (-t150 * t8 + t152 * t18 / 0.2D1) 
     #* z * t49 - t158 * t18 / 0.2D1) + 0.180D3 * t4 * t7 * (-t8 - (t8 -
     # t150 * t18) * z * t49 + t142 * t18) + t4 * t17 * (-t18 - t175)) *
     # t92 * t181 / 0.1440D4 - (t4 * t17 * (t8 - t187 * t18) - 0.90D2 * 
     #t4 * t6 * (t192 * t8 / 0.2D1 - t192 * t187 * t18 / 0.6D1) + t74 + 
     #0.180D3 * t4 * t7 * (-t187 * t8 + t192 * t18 / 0.2D1)) * t181 / 0.
     #1440D4 + (-0.90D2 * t4 * t6 * (-(t8 - t217 * t18) * z * t49 + t226
     # * t18 - t233 * t235 - t8 + t237) + 0.180D3 * t4 * t7 * t242) * t9
     #2 * t249 / 0.720D3 - (-0.90D2 * t4 * t6 * (t256 * t237 - t260 * t8
     # - t262 * t235 / 0.2D1 + t265 * t18 / 0.2D1) + 0.180D3 * t4 * t7 *
     # (-t237 + t8 + t256 * t235 - t260 * t18) + t4 * t17 * t278) * t248
     # * t181 / 0.720D3 + (-0.90D2 * t4 * t6 * (-t289 * t237 + t291 * t2
     #35 / 0.2D1 - (-t297 * t8 + t299 * t18 / 0.2D1) * z * t49 + t307 * 
     #t8 - t309 * t18 / 0.2D1) + 0.180D3 * t4 * t7 * (t237 - t289 * t235
     # - (t8 - t297 * t18) * z * t49 - t8 + t307 * t18) + t4 * t17 * t24
     #2) * t92 * t248 / 0.1440D4 - (t4 * t17 * (t8 - t335 * t18 - t237 +
     # t339 * t235) - 0.90D2 * t4 * t6 * (-t344 * t237 / 0.2D1 + t344 * 
     #t339 * t235 / 0.6D1 + t350 * t8 / 0.2D1 - t350 * t335 * t18 / 0.6D
     #1) + t4 * t72 * t278 + 0.180D3 * t4 * t7 * (-t335 * t8 + t350 * t1
     #8 / 0.2D1 + t339 * t237 - t344 * t235 / 0.2D1)) * t248 / 0.1440D4
      t376 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t375)
      t378 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t375)
      t380 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t375)
      t383 = x1 * z
      t384 = -z - x1 + t383
      t385 = 0.1D1 / t384
      t387 = t2 * x1 * t228 * t385
      t388 = -0.1D1 + x1
      t389 = t2 * t388
      t392 = x2 * s * t1 * x1
      t393 = s * t26
      t396 = x1 * t388 * t385
      t397 = t393 * t228 * t396
      t398 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, t234, 0.10D
     #1, x4)
      t399 = t388 ** 2
      t403 = 0.1D1 / t22
      t406 = t212 * t385 * t138
      t410 = log(0.4D1 * t399 * t27 * x3 * t30 * t403 * t228 * t406)
      t411 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t234, 0.10D
     #1, x4)
      t424 = t403 * t385
      t429 = log(0.4D1 * t213 * t31 * t424 * t399 * t228)
      t431 = t429 ** 2
      t449 = (-0.90D2 * t4 * t6 * (-t398 + t410 * t411) - 0.180D3 * t4 *
     # t7 * t411) * t92 * t249 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t429 *
     # t398 + t431 * t411 / 0.2D1) + 0.180D3 * t4 * t7 * (t398 - t429 * 
     #t411) + t4 * t17 * t411) * t248 * t181 / 0.720D3
      t450 = FJET(XB1, XB2, s, 0.0D0, t387, -t389, t392, -t397, t449)
      t452 = x2 * x3
      t453 = 0.1D1 - x3 + t452
      t454 = 0.1D1 / t453
      t455 = t452 * t454
      t456 = t2 * t455
      t457 = t35 * t454
      t458 = t2 * t457
      t459 = t4 * t6
      t460 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t234, -t
     #457, x4)
      t461 = t453 ** 2
      t462 = 0.1D1 / t461
      t465 = t462 * t27 * x3 * t35
      t466 = t228 * t212
      t471 = log(0.4D1 * t465 * t146 * t466 * t138)
      t472 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t234, -t
     #457, x4)
      t475 = t228 * t35
      t477 = Sqrt(t43 * t475)
      t481 = 0.1D1 / (-z - x3 + t452 + 0.2D1 * t42 * t477)
      t486 = t4 * lh
      t488 = t481 * z
      t489 = t6 * t472 * t488
      t499 = log(0.4D1 * t465 * t146 * t466)
      t501 = t499 ** 2
      t521 = (-0.90D2 * t459 * (t460 - t471 * t472) * t481 * z + 0.180D3
     # * t486 * t489) * t92 * t249 / 0.720D3 + (-0.90D2 * t459 * (-t499 
     #* t460 + t501 * t472 / 0.2D1) * t481 * z + 0.180D3 * t486 * t6 * (
     #t460 - t499 * t472) * t488 + t4 * t16 * t489) * t92 * t248 / 0.144
     #0D4
      t522 = FJET(XB1, XB2, s, 0.0D0, t456, 0.0D0, -t458, 0.0D0, t521)
      t525 = t2 * x1 * t385
      t526 = t393 * t396
      t533 = log(0.4D1 * t139 * t31 * t403 * t399 * t385 * t36)
      t534 = t533 * z
      t535 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t537 = t533 ** 2
      t539 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t544 = x3 * x1
      t545 = t544 * z
      t546 = x1 * t22
      t547 = x3 * t22
      t548 = t547 * x1
      t550 = 0.2D1 * t139 * z
      t551 = t139 * t22
      t552 = x3 * t384
      t554 = Sqrt(t552 * t35)
      t559 = 0.1D1 / (-t383 - t545 + t546 + t548 - t43 - t139 + t550 - t
     #551 + 0.2D1 * t42 * t554 * z - t22)
      t562 = t30 * t403
      t563 = t399 * t385
      t564 = t562 * t563
      t567 = log(-0.4D1 * t139 * t27 * t564)
      t569 = t567 ** 2
      t576 = z * t535
      t589 = -z * t539 * t384 * t559 + t539
      t595 = (-0.90D2 * t4 * t6 * (-(-t534 * t535 + t537 * z * t539 / 0.
     #2D1) * t384 * t559 - t567 * t535 + t569 * t539 / 0.2D1) + 0.180D3 
     #* t4 * t7 * (-(t576 - t534 * t539) * t384 * t559 + t535 - t567 * t
     #539) + t4 * t17 * t589) * t92 * t181 / 0.1440D4
      t600 = log(-0.4D1 * t184 * t30 * t424 * t399)
      t602 = -t535 + t600 * t539
      t605 = t600 ** 2
      t611 = -t605 * t535 / 0.2D1 + t605 * t600 * t539 / 0.6D1
      t616 = t4 * t72 * t539
      t620 = t600 * t535 - t605 * t539 / 0.2D1
      t627 = t222 * t184
      t630 = log(-0.4D1 * t627 * t564)
      t636 = log(0.4D1 * t627 * t562 * t563 * t36)
      t652 = (-0.90D2 * t4 * t6 * (t535 - t630 * t539 - (t576 - t636 * z
     # * t539) * t384 * t559) + 0.180D3 * t4 * t7 * t589) * t92 * t249 /
     # 0.720D3
      t655 = log(-0.4D1 * t252 * t564)
      t657 = t655 ** 2
      t660 = t655 * t535 - t657 * t539 / 0.2D1
      t665 = -t535 + t655 * t539
      t670 = t4 * t17 * t539
      t674 = (-0.90D2 * t4 * t6 * t660 + 0.180D3 * t4 * t7 * t665 - t670
     #) * t248 * t181 / 0.720D3
      t675 = t595 - (t4 * t17 * t602 - 0.90D2 * t4 * t6 * t611 - t616 + 
     #0.180D3 * t4 * t7 * t620) * t181 / 0.1440D4 + t652 - t674
      t676 = FJET(XB1, XB2, s, 0.0D0, -t389, -t525, 0.0D0, t526, t675)
      t678 = FJET(XB1, XB2, s, 0.0D0, -t525, -t389, 0.0D0, t526, t675)
      t680 = FJET(XB1, XB2, s, 0.0D0, -t458, 0.0D0, t456, 0.0D0, t521)
      t682 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t375)
      t684 = FJET(XB1, XB2, s, t392, -t389, t387, 0.0D0, -t397, t449)
      t686 = FJET(XB1, XB2, s, t387, 0.0D0, t392, -t389, -t397, t449)
      t688 = FJET(XB1, XB2, s, t456, 0.0D0, -t458, 0.0D0, 0.0D0, t521)
      t693 = t35 * s * t1 * t388 * t454
      t694 = t2 * x1
      t696 = Sqrt(-t552 * t475)
      t697 = t42 * t696
      t703 = t694 * x2 * (-x3 + t452 - z + t43 - x1 + t544 + t383 - t545
     # + 0.2D1 * t697) * t385 * t454
      t704 = t389 * t455
      t705 = t222 * x1
      t709 = t222 * t383
      t713 = t694 * (t705 + t222 * z - x2 + t452 + 0.2D1 * t697 * x2 + 0
     #.1D1 - x3 - t709) * t385 * t454
      t721 = log(-0.4D1 * t462 * t399 * t144 * t35 * t562 * t228 * t406)
      t722 = x2 * t138
      t724 = x2 * x1
      t731 = t724 * z
      t734 = t722 - t139 * x2 - t724 * t22 - 0.2D1 * t722 * z + t722 * t
     #22 - 0.2D1 * t697 * z - t546 + t43 + t139 - t705 + t731 - t452 * z
     # + t452 * x1
      t747 = t383 + t545 - t548 - t550 + t551 + 0.2D1 * t697 * t731 + t2
     #2 + t709 - 0.2D1 * t452 * t383 + t547 * t724 + 0.2D1 * t139 * x2 *
     # z - t139 * t22 * x2 - 0.2D1 * t697 * t724
      t749 = 0.1D1 / (t734 + t747)
      t751 = -z - t724 + t731
      t753 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t234, -t457
     #, x4)
      t756 = t749 * t751
      t757 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, t234, -t457
     #, x4)
      t769 = -0.90D2 * t4 * t6 * (-t721 * t749 * t751 * t384 * t753 + t7
     #56 * t384 * t757) + 0.180D3 * t4 * t7 * t756 * t384 * t753
      t772 = t769 * t92 * t249 / 0.720D3
      t773 = FJET(XB1, XB2, s, t693, t703, -t704, -t713, -t397, t772)
      t776 = t92 * t248 * t181
      t779 = FJET(XB1, XB2, s, t703, t693, -t713, -t704, -t397, t772)
      t796 = (t4 * t17 * t602 - 0.90D2 * t4 * t6 * t611 - t616 + 0.180D3
     # * t4 * t7 * t620) * t181 / 0.1440D4
      t797 = t595 - t796 + t652 - t674
      t798 = FJET(XB1, XB2, s, -t389, 0.0D0, 0.0D0, -t525, t526, t797)
      t800 = FJET(XB1, XB2, s, -t389, t392, 0.0D0, t387, -t397, t449)
      t814 = t595 - t796 + t652 - (-0.90D2 * t4 * t6 * t660 + 0.180D3 * 
     #t4 * t7 * t665 - t670) * t248 * t181 / 0.720D3
      t815 = FJET(XB1, XB2, s, -t525, 0.0D0, 0.0D0, -t389, t526, t814)
      t817 = FJET(XB1, XB2, s, -t458, 0.0D0, t456, 0.0D0, 0.0D0, t521)
      t819 = FJET(XB1, XB2, s, -t713, -t704, t703, t693, -t397, t772)
      t823 = FJET(XB1, XB2, s, -t704, -t713, t693, t703, -t397, t772)
      rrqqbar2gght5s3e1 = t376 * t375 + t378 * t375 + t380 * t375 + t450
     # * t449 + t522 * t521 + t676 * t675 + t678 * t675 + t680 * t521 + 
     #t682 * t375 + t684 * t449 + t686 * t449 + t688 * t521 + t773 * t76
     #9 * t776 / 0.720D3 + t779 * t769 * t776 / 0.720D3 + t798 * t797 + 
     #t800 * t449 + t815 * t814 + t817 * t521 + t819 * t769 * t776 / 0.7
     #20D3 + t823 * t769 * t776 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght5s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = t14 * t17
      t21 = log(0.4D1 * t12 * t18)
      t22 = t21 ** 2
      t23 = -0.1D1 + x3
      t24 = 0.1D1 / t23
      t28 = log(-0.4D1 * t12 * t18 * t24)
      t29 = t28 ** 2
      t31 = cos(t15)
      t32 = x3 * z
      t34 = Sqrt(-t32 * t23)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t31 * t34)
      t45 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t49 = lh * t6
      t61 = lh ** 2
      t63 = 0.3141592653589793D1 ** 2
      t65 = -0.180D3 * t61 + 0.30D2 * t63
      t66 = t65 * t6
      t68 = t4 * t66 * t7
      t74 = 0.1D1 / x3
      t78 = t11 * t14 * t17
      t80 = log(0.4D1 * t78)
      t81 = t80 * t3
      t82 = 0.3141592653589793D1 * lh
      t85 = t80 ** 2
      t86 = t85 * t3
      t112 = 0.1D1 - x2
      t113 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t112, 0.
     #10D1, x4)
      t114 = x2 ** 2
      t115 = x3 * t114
      t117 = -t112
      t118 = t18 * t117
      t121 = log(-0.4D1 * t115 * t11 * t118)
      t122 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t112, 0.
     #10D1, x4)
      t125 = t14 * x3 * t24
      t126 = t17 * t11
      t130 = log(-0.4D1 * t125 * t126 * t114)
      t137 = log(0.4D1 * t115 * t78)
      t144 = t7 * z * t38
      t145 = -t144 + t122 - t7
      t151 = 0.1D1 / x2
      t154 = t11 * t114
      t157 = log(0.4D1 * t154 * t18)
      t159 = t157 ** 2
      t164 = log(-0.4D1 * t154 * t118)
      t166 = t164 ** 2
      t179 = t7 - t122
      t185 = x1 ** 2
      t189 = log(-0.4D1 * t125 * t126 * t185)
      t194 = x3 * t185
      t197 = log(0.4D1 * t194 * t78)
      t209 = 0.1D1 / x1
      t212 = t4 * t6
      t214 = t151 * t209
      t218 = t114 * t185
      t219 = t218 * t14
      t223 = log(-0.4D1 * t219 * t126 * t117)
      t227 = log(0.4D1 * t218 * t78)
      t240 = t185 * t14
      t243 = log(0.4D1 * t240 * t126)
      t245 = t243 ** 2
      t260 = (-0.90D2 * t4 * t6 * t7 * (-t22 / 0.2D1 - t29 * z * t38 / 0
     #.2D1) + (-0.90D2 * t4 * t6 * t45 + 0.180D3 * t4 * t49 * t7) * (t21
     # + t28 * z * t38) + (0.180D3 * t4 * t49 * t45 + t68) * (-0.1D1 - z
     # * t38)) * t74 / 0.2880D4 - (-0.180D3 * t81 * t82 - 0.45D2 * t86 *
     # 0.3141592653589793D1 + t4 * t65) * t6 * t45 / 0.2880D4 - (0.90D2 
     #* t86 * t82 + t4 * (-0.60D2 * lh * t63 + 0.2884936567583026D3 + 0.
     #120D3 * t61 * lh) + 0.15D2 * t85 * t80 * t3 * 0.3141592653589793D1
     # - t81 * 0.3141592653589793D1 * t65) * t6 * t7 / 0.2880D4 + (-0.90
     #D2 * t4 * t6 * (t113 - t121 * t122 - (t45 - t130 * t7) * z * t38 -
     # t45 + t137 * t7) + 0.180D3 * t4 * t49 * t145) * t74 * t151 / 0.14
     #40D4 - (-0.90D2 * t4 * t6 * (-t157 * t45 + t159 * t7 / 0.2D1 + t16
     #4 * t113 - t166 * t122 / 0.2D1) + 0.180D3 * t4 * t49 * (t45 - t157
     # * t7 - t113 + t164 * t122) + t4 * t66 * t179) * t151 / 0.1440D4 +
     # (-0.90D2 * t4 * t6 * (-t45 - (t45 - t189 * t7) * z * t38 + t197 *
     # t7) + 0.180D3 * t4 * t49 * (-t7 - t144)) * t74 * t209 / 0.1440D4 
     #- t212 * t145 * t74 * t214 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t113 +
     # t45 + t223 * t122 - t227 * t7) + 0.180D3 * t4 * t49 * t179) * t15
     #1 * t209 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t243 * t45 + t245 * t7
     # / 0.2D1) + 0.180D3 * t4 * t49 * (t45 - t243 * t7) + t68) * t209 /
     # 0.1440D4
      t261 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t260)
      t263 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t260)
      t265 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t260)
      t268 = x1 * z
      t269 = -z - x1 + t268
      t270 = 0.1D1 / t269
      t272 = t2 * x1 * t117 * t270
      t273 = -0.1D1 + x1
      t274 = t2 * t273
      t277 = x2 * s * t1 * x1
      t278 = s * t13
      t281 = x1 * t273 * t270
      t282 = t278 * t117 * t281
      t283 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t112, 0.10D
     #1, x4)
      t288 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, t112, 0.10D
     #1, x4)
      t290 = 0.1D1 / t9
      t291 = t290 * t270
      t292 = t273 ** 2
      t297 = log(0.4D1 * t218 * t18 * t291 * t292 * t117)
      t310 = t212 * t283 * t74 * t214 / 0.8D1 - (-0.90D2 * t4 * t6 * (t2
     #88 - t297 * t283) + 0.180D3 * t4 * t49 * t283) * t151 * t209 / 0.7
     #20D3
      t311 = FJET(XB1, XB2, s, 0.0D0, t272, -t274, t277, -t282, t310)
      t313 = x2 * x3
      t314 = 0.1D1 - x3 + t313
      t315 = 0.1D1 / t314
      t316 = t313 * t315
      t317 = t2 * t316
      t318 = t23 * t315
      t319 = t2 * t318
      t320 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t112, -t
     #318, x4)
      t321 = t314 ** 2
      t330 = log(0.4D1 / t321 * t14 * x3 * t23 * t126 * t117 * t114)
      t331 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t112, -t
     #318, x4)
      t334 = t117 * t23
      t336 = Sqrt(t32 * t334)
      t340 = 0.1D1 / (-z - x3 + t313 + 0.2D1 * t31 * t336)
      t346 = t6 * t331
      t347 = t340 * z
      t357 = t74 * t151 * t209
      t361 = (-0.90D2 * t212 * (t320 - t330 * t331) * t340 * z + 0.180D3
     # * t4 * lh * t346 * t347) * t74 * t151 / 0.1440D4 - t4 * t346 * t3
     #47 * t357 / 0.8D1
      t362 = FJET(XB1, XB2, s, 0.0D0, t317, 0.0D0, -t319, 0.0D0, t361)
      t365 = t2 * x1 * t270
      t366 = t278 * t281
      t367 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t375 = log(0.4D1 * t194 * t18 * t290 * t292 * t270 * t24)
      t377 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t381 = x3 * x1
      t382 = t381 * z
      t383 = x1 * t9
      t384 = x3 * t9
      t385 = t384 * x1
      t387 = 0.2D1 * t194 * z
      t388 = t194 * t9
      t389 = x3 * t269
      t391 = Sqrt(t389 * t23)
      t396 = 0.1D1 / (-t268 - t382 + t383 + t385 - t32 - t194 + t387 - t
     #388 + 0.2D1 * t31 * t391 * z - t9)
      t401 = t17 * t290 * t292 * t270
      t404 = log(-0.4D1 * t194 * t14 * t401)
      t413 = -z * t377 * t269 * t396 + t377
      t420 = (-0.90D2 * t4 * t6 * (-(z * t367 - t375 * z * t377) * t269 
     #* t396 + t367 - t404 * t377) + 0.180D3 * t4 * t49 * t413) * t74 * 
     #t209 / 0.1440D4
      t424 = t212 * t413 * t74 * t214 / 0.8D1
      t427 = log(-0.4D1 * t219 * t401)
      t429 = -t367 + t427 * t377
      t435 = 0.180D3 * t4 * t49 * t377
      t439 = (-0.90D2 * t4 * t6 * t429 - t435) * t151 * t209 / 0.720D3
      t444 = log(-0.4D1 * t240 * t17 * t291 * t292)
      t446 = t444 ** 2
      t449 = t444 * t367 - t446 * t377 / 0.2D1
      t454 = -t367 + t444 * t377
      t459 = t4 * t66 * t377
      t463 = t420 - t424 - t439 - (-0.90D2 * t4 * t6 * t449 + 0.180D3 * 
     #t4 * t49 * t454 - t459) * t209 / 0.1440D4
      t464 = FJET(XB1, XB2, s, 0.0D0, -t274, -t365, 0.0D0, t366, t463)
      t466 = FJET(XB1, XB2, s, 0.0D0, -t365, -t274, 0.0D0, t366, t463)
      t468 = FJET(XB1, XB2, s, 0.0D0, -t319, 0.0D0, t317, 0.0D0, t361)
      t470 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t260)
      t472 = FJET(XB1, XB2, s, t277, -t274, t272, 0.0D0, -t282, t310)
      t474 = FJET(XB1, XB2, s, t272, 0.0D0, t277, -t274, -t282, t310)
      t476 = FJET(XB1, XB2, s, t317, 0.0D0, -t319, 0.0D0, 0.0D0, t361)
      t481 = t23 * s * t1 * t273 * t315
      t482 = t2 * x1
      t484 = Sqrt(-t389 * t334)
      t485 = t31 * t484
      t491 = t482 * x2 * (-x3 + t313 - z + t32 - x1 + t381 + t268 - t382
     # + 0.2D1 * t485) * t270 * t315
      t492 = t274 * t316
      t493 = t115 * x1
      t497 = t115 * t268
      t501 = t482 * (t493 + t115 * z - x2 + t313 + 0.2D1 * t485 * x2 + 0
     #.1D1 - x3 - t497) * t270 * t315
      t502 = x2 * t185
      t503 = x2 * x1
      t504 = t503 * z
      t514 = t502 - t493 + t504 - t313 * z + t313 * x1 - t194 * x2 - t50
     #3 * t9 - 0.2D1 * t502 * z + t502 * t9 - 0.2D1 * t485 * z + t9 + t3
     #82 - t385
      t527 = -t387 + t388 - t383 + t32 + t194 + 0.2D1 * t485 * t504 + t2
     #68 + t497 - 0.2D1 * t313 * t268 + t384 * t503 + 0.2D1 * t194 * x2 
     #* z - t194 * t9 * x2 - 0.2D1 * t485 * t503
      t529 = 0.1D1 / (t514 + t527)
      t531 = -z - t503 + t504
      t534 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t112, -t318
     #, x4)
      t538 = t4 * t6 * t529 * t531 * t269 * t534 * t357 / 0.8D1
      t539 = FJET(XB1, XB2, s, t481, t491, -t492, -t501, -t282, -t538)
      t542 = 0.3141592653589793D1 * t6 * t529
      t546 = t531 * t269 * t534 * t357
      t549 = FJET(XB1, XB2, s, t491, t481, -t501, -t492, -t282, -t538)
      t564 = (-0.90D2 * t4 * t6 * t449 + 0.180D3 * t4 * t49 * t454 - t45
     #9) * t209 / 0.1440D4
      t565 = t420 - t424 - t439 - t564
      t566 = FJET(XB1, XB2, s, -t274, 0.0D0, 0.0D0, -t365, t366, t565)
      t568 = FJET(XB1, XB2, s, -t274, t277, 0.0D0, t272, -t282, t310)
      t578 = t420 - t424 - (-0.90D2 * t4 * t6 * t429 - t435) * t151 * t2
     #09 / 0.720D3 - t564
      t579 = FJET(XB1, XB2, s, -t365, 0.0D0, 0.0D0, -t274, t366, t578)
      t581 = FJET(XB1, XB2, s, -t319, 0.0D0, t317, 0.0D0, 0.0D0, t361)
      t583 = FJET(XB1, XB2, s, -t501, -t492, t491, t481, -t282, -t538)
      t588 = FJET(XB1, XB2, s, -t492, -t501, t481, t491, -t282, -t538)
      rrqqbar2gght5s3e0 = t261 * t260 + t263 * t260 + t265 * t260 + t311
     # * t310 + t362 * t361 + t464 * t463 + t466 * t463 + t468 * t361 + 
     #t470 * t260 + t472 * t310 + t474 * t310 + t476 * t361 - t539 * t3 
     #* t542 * t546 / 0.8D1 - t549 * t3 * t542 * t546 / 0.8D1 + t566 * t
     #565 + t568 * t310 + t579 * t578 + t581 * t361 - t583 * t3 * t542 *
     # t546 / 0.8D1 - t588 * t3 * t542 * t546 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght5s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = t14 * t17
      t21 = log(0.4D1 * t12 * t18)
      t22 = -0.1D1 + x3
      t27 = log(-0.4D1 * t12 * t18 / t22)
      t29 = cos(t15)
      t30 = x3 * z
      t32 = Sqrt(-t30 * t22)
      t36 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t32)
      t42 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t46 = lh * t6
      t49 = 0.180D3 * t4 * t46 * t7
      t55 = 0.1D1 / x3
      t63 = log(0.4D1 * t11 * t14 * t17)
      t64 = t63 * t3
      t74 = t63 ** 2
      t78 = lh ** 2
      t80 = 0.3141592653589793D1 ** 2
      t88 = t4 * t6
      t90 = t7 * z * t36
      t91 = 0.1D1 - x2
      t92 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t91, 0.10
     #D1, x4)
      t95 = 0.1D1 / x2
      t99 = x2 ** 2
      t100 = t11 * t99
      t103 = log(0.4D1 * t100 * t18)
      t105 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t91, 0.1
     #0D1, x4)
      t106 = -t91
      t110 = log(-0.4D1 * t100 * t18 * t106)
      t116 = t7 - t92
      t124 = 0.1D1 / x1
      t128 = x1 ** 2
      t129 = t128 * t14
      t133 = log(0.4D1 * t129 * t17 * t11)
      t147 = (-0.90D2 * t4 * t6 * t7 * (t21 + t27 * z * t36) + (-0.90D2 
     #* t4 * t6 * t42 + t49) * (-0.1D1 - z * t36)) * t55 / 0.2880D4 - (0
     #.180D3 * t4 * lh + 0.90D2 * t64 * 0.3141592653589793D1) * t6 * t42
     # / 0.2880D4 - (-0.180D3 * t64 * 0.3141592653589793D1 * lh - 0.45D2
     # * t74 * t3 * 0.3141592653589793D1 + t4 * (-0.180D3 * t78 + 0.30D2
     # * t80)) * t6 * t7 / 0.2880D4 - t88 * (-t90 + t92 - t7) * t55 * t9
     #5 / 0.16D2 - (-0.90D2 * t4 * t6 * (t42 - t103 * t7 - t105 + t110 *
     # t92) + 0.180D3 * t4 * t46 * t116) * t95 / 0.1440D4 + t88 * t116 *
     # t95 * t124 / 0.8D1 - (-0.90D2 * t4 * t6 * (t42 - t133 * t7) + t49
     #) * t124 / 0.1440D4 - t88 * (-t7 - t90) * t55 * t124 / 0.16D2
      t148 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t147)
      t150 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t147)
      t152 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t147)
      t155 = x1 * z
      t156 = -z - x1 + t155
      t157 = 0.1D1 / t156
      t159 = t2 * x1 * t106 * t157
      t160 = -0.1D1 + x1
      t161 = t2 * t160
      t164 = x2 * s * t1 * x1
      t165 = s * t13
      t168 = x1 * t160 * t157
      t169 = t165 * t106 * t168
      t170 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, t91, 0.10D1
     #, x4)
      t174 = t88 * t170 * t95 * t124 / 0.8D1
      t175 = FJET(XB1, XB2, s, 0.0D0, t159, -t161, t164, -t169, t174)
      t180 = t6 * t170 * t95 * t124
      t183 = x2 * x3
      t185 = 0.1D1 / (0.1D1 - x3 + t183)
      t187 = t2 * t183 * t185
      t188 = t22 * t185
      t189 = t2 * t188
      t190 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t91, -t1
     #88, x4)
      t195 = Sqrt(t30 * t106 * t22)
      t199 = 0.1D1 / (-z - x3 + t183 + 0.2D1 * t29 * t195)
      t204 = t4 * t6 * t190 * t199 * z * t55 * t95 / 0.16D2
      t205 = FJET(XB1, XB2, s, 0.0D0, t187, 0.0D0, -t189, 0.0D0, -t204)
      t207 = 0.3141592653589793D1 * t6
      t212 = t190 * t199 * z * t55 * t95
      t216 = t2 * x1 * t157
      t217 = t165 * t168
      t218 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t222 = t88 * t218 * t95 * t124 / 0.8D1
      t223 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.1
     #0D1, x4)
      t227 = t160 ** 2
      t231 = log(-0.4D1 * t129 * t17 / t9 * t157 * t227)
      t233 = -t223 + t231 * t218
      t239 = 0.180D3 * t4 * t46 * t218
      t249 = x3 * t128
      t255 = Sqrt(x3 * t156 * t22)
      t267 = t88 * (-z * t218 * t156 / (-t155 - x3 * x1 * z + x1 * t9 + 
     #x3 * t9 * x1 - t30 - t249 + 0.2D1 * t249 * z - t249 * t9 + 0.2D1 *
     # t29 * t255 * z - t9) + t218) * t55 * t124 / 0.16D2
      t268 = -t222 - (-0.90D2 * t4 * t6 * t233 - t239) * t124 / 0.1440D4
     # - t267
      t269 = FJET(XB1, XB2, s, 0.0D0, -t161, -t216, 0.0D0, t217, t268)
      t271 = FJET(XB1, XB2, s, 0.0D0, -t216, -t161, 0.0D0, t217, t268)
      t273 = FJET(XB1, XB2, s, 0.0D0, -t189, 0.0D0, t187, 0.0D0, -t204)
      t278 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t147)
      t280 = FJET(XB1, XB2, s, t164, -t161, t159, 0.0D0, -t169, t174)
      t285 = FJET(XB1, XB2, s, t159, 0.0D0, t164, -t161, -t169, t174)
      t290 = FJET(XB1, XB2, s, t187, 0.0D0, -t189, 0.0D0, 0.0D0, -t204)
      t302 = -t222 - (-0.90D2 * t4 * t6 * t233 - t239) * t124 / 0.1440D4
     # - t267
      t303 = FJET(XB1, XB2, s, -t161, 0.0D0, 0.0D0, -t216, t217, t302)
      t305 = FJET(XB1, XB2, s, -t161, t164, 0.0D0, t159, -t169, t174)
      t310 = FJET(XB1, XB2, s, -t216, 0.0D0, 0.0D0, -t161, t217, t302)
      t312 = FJET(XB1, XB2, s, -t189, 0.0D0, t187, 0.0D0, 0.0D0, -t204)
      rrqqbar2gght5s3em1 = t148 * t147 + t150 * t147 + t152 * t147 + t17
     #5 * t3 * 0.3141592653589793D1 * t180 / 0.8D1 - t205 * t3 * t207 * 
     #t212 / 0.16D2 + t269 * t268 + t271 * t268 - t273 * t3 * t207 * t21
     #2 / 0.16D2 + t278 * t147 + t280 * t3 * 0.3141592653589793D1 * t180
     # / 0.8D1 + t285 * t3 * 0.3141592653589793D1 * t180 / 0.8D1 - t290 
     #* t3 * t207 * t212 / 0.16D2 + t303 * t302 + t305 * t3 * 0.31415926
     #53589793D1 * t180 / 0.8D1 + t310 * t302 - t312 * t3 * t207 * t212 
     #/ 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t8 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t14 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t27 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.1D1 - x
     #2, 0.10D1, x4)
      t35 = 0.1D1 / x1
      t39 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0
     #.10D1, x4)
      t45 = z ** 2
      t48 = t1 ** 2
      t49 = t48 ** 2
      t51 = Sin(t9)
      t52 = t51 ** 2
      t55 = log(0.4D1 / t45 / z * t49 * t52)
      t63 = -t4 * t6 * t8 * (-0.1D1 - z / (-z - x3 + 0.2D1 * t10 * t14))
     # / x3 / 0.32D2 + t4 * t6 * (t8 - t27) / x2 / 0.16D2 + t4 * t6 * t8
     # * t35 / 0.16D2 + t4 * t6 * t39 / 0.32D2 - (0.180D3 * t4 * lh + 0.
     #90D2 * t55 * t3 * 0.3141592653589793D1) * t6 * t8 / 0.2880D4
      t64 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t63)
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t63)
      t68 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t63)
      t70 = -0.1D1 + x1
      t71 = t2 * t70
      t74 = 0.1D1 / (-z - x1 + x1 * z)
      t76 = t2 * x1 * t74
      t80 = s * t48 * x1 * t70 * t74
      t81 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10
     #D1, x4)
      t83 = t6 * t81 * t35
      t85 = t4 * t83 / 0.16D2
      t86 = FJET(XB1, XB2, s, 0.0D0, -t71, -t76, 0.0D0, t80, -t85)
      t91 = FJET(XB1, XB2, s, 0.0D0, -t76, -t71, 0.0D0, t80, -t85)
      t96 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t63)
      t98 = FJET(XB1, XB2, s, -t71, 0.0D0, 0.0D0, -t76, t80, -t85)
      t103 = FJET(XB1, XB2, s, -t76, 0.0D0, 0.0D0, -t71, t80, -t85)
      rrqqbar2gght5s3em2 = t64 * t63 + t66 * t63 + t68 * t63 - t86 * t3 
     #* 0.3141592653589793D1 * t83 / 0.16D2 - t91 * t3 * 0.3141592653589
     #793D1 * t83 / 0.16D2 + t96 * t63 - t98 * t3 * 0.3141592653589793D1
     # * t83 / 0.16D2 - t103 * t3 * 0.3141592653589793D1 * t83 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t10 = t3 * 0.3141592653589793D1 * t6 * t7 / 0.32D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = 0.3141592653589793D1 * t6 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s3em3 = t11 * t3 * t14 / 0.32D2 + t16 * t3 * t14 / 0.
     #32D2 + t19 * t3 * t14 / 0.32D2 + t22 * t3 * t14 / 0.32D2

      end function



      doubleprecision function rrqqbar2gght5s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      rrqqbar2gght5s3em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght5s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.1D1 / t1
      t7 = t6 * 0.3141592653589793D1
      t8 = s ** 2
      t9 = 0.1D1 / t8
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = t1 ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t21 = t4 ** 2
      t22 = x1 * z
      t23 = -z - x1 + t22
      t24 = 0.1D1 / t23
      t25 = t21 * t24
      t26 = t20 * t25
      t29 = log(-0.4D1 * t14 * t26)
      t30 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t32 = t29 ** 2
      t33 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t40 = lh * t9
      t46 = lh ** 2
      t48 = 0.3141592653589793D1 ** 2
      t50 = -0.180D3 * t46 + 0.30D2 * t48
      t51 = t50 * t9
      t53 = t7 * t51 * t33
      t55 = 0.1D1 / x3
      t57 = 0.1D1 / x1
      t59 = t10 * t13
      t61 = t19 * t24
      t65 = log(-0.4D1 * t59 * t17 * t61 * t21)
      t70 = t65 ** 2
      t84 = -0.60D2 * lh * t48 + 0.2884936567583026D3 + 0.120D3 * t46 * 
     #lh
      t85 = t84 * t9
      t97 = x2 * x3
      t98 = t97 * t59
      t101 = log(-0.4D1 * t98 * t26)
      t112 = 0.1D1 / x2
      t113 = t112 * t57
      t115 = x2 * t10
      t116 = t115 * t13
      t119 = log(-0.4D1 * t116 * t26)
      t121 = t119 ** 2
      t137 = (-0.90D2 * t7 * t9 * (-t29 * t30 + t32 * t33 / 0.2D1) + 0.1
     #80D3 * t7 * t40 * (t30 - t29 * t33) + t53) * t55 * t57 / 0.720D3 -
     # (t7 * t51 * (-t30 + t65 * t33) - 0.90D2 * t7 * t9 * (-t70 * t30 /
     # 0.2D1 + t70 * t65 * t33 / 0.6D1) - t7 * t85 * t33 + 0.180D3 * t7 
     #* t40 * (t65 * t30 - t70 * t33 / 0.2D1)) * t57 / 0.720D3 + (-0.90D
     #2 * t7 * t9 * (t30 - t101 * t33) + 0.180D3 * t7 * t40 * t33) * t55
     # * t113 / 0.720D3 + (-0.90D2 * t7 * t9 * (-t119 * t30 + t121 * t33
     # / 0.2D1) + 0.180D3 * t7 * t40 * (t30 - t119 * t33) + t53) * t112 
     #* t57 / 0.720D3
      t138 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t137)
      t140 = -0.1D1 + x3
      t141 = t140 * s
      t143 = t141 * t1 * x1
      t144 = t1 * t4
      t145 = t141 * t144
      t146 = x3 * x1
      t147 = t2 * t146
      t149 = t2 * t4 * x3
      t154 = x3 * t140
      t158 = log(0.4D1 * t24 * t17 * t19 * t13 * t10 * t21 * t154)
      t159 = -t140
      t160 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t159
     #, x4)
      t162 = t158 ** 2
      t163 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t159
     #, x4)
      t166 = -t158 * t160 + t162 * t163 / 0.2D1
      t171 = t160 - t158 * t163
      t176 = t7 * t51 * t163
      t187 = log(0.4D1 * x2 * t24 * t20 * t59 * t21 * x3 * t140)
      t198 = (-0.90D2 * t7 * t9 * (-t160 + t187 * t163) - 0.180D3 * t7 *
     # t40 * t163) * t55 * t113
      t200 = (0.90D2 * t7 * t9 * t166 - 0.180D3 * t7 * t40 * t171 - t176
     #) * t55 * t57 / 0.720D3 + t198 / 0.720D3
      t201 = FJET(XB1, XB2, s, -t143, t145, t147, -t149, 0.0D0, t200)
      t203 = x2 * x1
      t205 = t2 * t203 * t24
      t206 = -0.1D1 + x2
      t208 = t2 * x1 * t206
      t213 = s * t12 * x2 * x1 * t4 * t24
      t214 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t215 = t206 ** 2
      t220 = log(-0.4D1 * t98 * t20 * t25 * t215)
      t221 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t232 = (-0.90D2 * t7 * t9 * (-t214 + t220 * t221) - 0.180D3 * t7 *
     # t40 * t221) * t55 * t113
      t233 = t13 * t17
      t239 = log(-0.4D1 * t115 * t233 * t61 * t21 * t215)
      t241 = t239 ** 2
      t244 = t239 * t214 - t241 * t221 / 0.2D1
      t249 = -t214 + t239 * t221
      t254 = t7 * t51 * t221
      t259 = t232 / 0.720D3 + (-0.90D2 * t7 * t9 * t244 + 0.180D3 * t7 *
     # t40 * t249 - t254) * t112 * t57 / 0.720D3
      t260 = FJET(XB1, XB2, s, 0.0D0, -t205, -t5, -t208, t213, t259)
      t262 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t137)
      t264 = t2 * t140
      t265 = t2 * x3
      t267 = 0.1D1 / t18 / z
      t268 = t17 * t267
      t272 = log(-0.4D1 * t14 * t268 * t140)
      t273 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #159, x4)
      t275 = t272 ** 2
      t276 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #159, x4)
      t289 = t7 * t51 * t276
      t294 = t11 * x2
      t299 = log(-0.4D1 * t294 * t233 * t267 * t140)
      t312 = x3 * t267
      t313 = t233 * t140
      t316 = log(-0.4D1 * t312 * t313)
      t321 = t316 ** 2
      t343 = t97 * t267
      t346 = log(-0.4D1 * t343 * t313)
      t348 = t346 ** 2
      t364 = (-0.90D2 * t7 * t9 * (-t272 * t273 + t275 * t276 / 0.2D1) +
     # 0.180D3 * t7 * t40 * (t273 - t272 * t276) + t289) * t55 * t57 / 0
     #.720D3 + (-0.90D2 * t7 * t9 * (t273 - t299 * t276) + 0.180D3 * t7 
     #* t40 * t276) * t55 * t113 / 0.720D3 + (t7 * t51 * (t273 - t316 * 
     #t276) - 0.90D2 * t7 * t9 * (t321 * t273 / 0.2D1 - t321 * t316 * t2
     #76 / 0.6D1) + t7 * t85 * t276 + 0.180D3 * t7 * t40 * (-t316 * t273
     # + t321 * t276 / 0.2D1)) * t55 / 0.1440D4 + (-0.90D2 * t7 * t9 * (
     #-t346 * t273 + t348 * t276 / 0.2D1) + 0.180D3 * t7 * t40 * (t273 -
     # t346 * t276) + t289) * t55 * t112 / 0.1440D4
      t365 = FJET(XB1, XB2, s, -t264, 0.0D0, t265, 0.0D0, 0.0D0, t364)
      t367 = FJET(XB1, XB2, s, 0.0D0, -t264, 0.0D0, t265, 0.0D0, t364)
      t369 = x3 * z
      t370 = t146 * z
      t371 = cos(t15)
      t373 = x2 * t140
      t375 = Sqrt(x3 * t23 * t373)
      t376 = t371 * t375
      t377 = 0.2D1 * t376
      t380 = -0.1D1 + t97
      t381 = 0.1D1 / t380
      t384 = t3 * t206 * (-t97 - z + t369 - x1 + t146 + t22 - t370 + t37
     #7) * t24 * t381
      t386 = t141 * t144 * t381
      t387 = t97 * z
      t389 = t97 * x1
      t391 = x2 ** 2
      t392 = t391 * x3
      t393 = t392 * x1
      t397 = t392 * t22
      t400 = t370 + 0.2D1 * t387 + 0.2D1 * t389 - t393 - t392 * z + t97 
     #- x2 - t369 - t146 - 0.2D1 * t97 * t22 + t397 + 0.2D1 * t376 * x2 
     #- t377
      t403 = t3 * t400 * t24 * t381
      t405 = x3 * t206 * t381
      t406 = t5 * t405
      t410 = t215 * t17
      t412 = t380 ** 2
      t413 = 0.1D1 / t412
      t419 = log(0.4D1 * t21 * t13 * t97 * t140 * t410 * t19 * t24 * t41
     #3 * t10)
      t420 = t203 * z
      t421 = z + x1 - t22 - t203 + t420
      t433 = -t10 + t420 - t387 - t389 - t294 + t393 - t203 * t18 - 0.2D
     #1 * t115 * z + t115 * t18 - 0.2D1 * t376 * t203 - 0.2D1 * t376 * t
     #22 + 0.2D1 * t376 * t420
      t451 = -0.2D1 * t22 + x3 * t18 * t203 + 0.2D1 * t11 * x2 * z - t11
     # * t18 * x2 - t397 + 0.2D1 * t376 * z + 0.2D1 * t376 * x1 + 0.2D1 
     #* x1 * t18 + 0.2D1 * t10 * z - t10 * t18 + t115 - t18
      t453 = 0.1D1 / (t433 + t451)
      t455 = t140 * t381
      t456 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t455, x
     #4)
      t459 = t421 * t453
      t460 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t455, x
     #4)
      t472 = -0.90D2 * t7 * t9 * (-t419 * t421 * t453 * t23 * t456 + t45
     #9 * t23 * t460) + 0.180D3 * t7 * t40 * t459 * t23 * t456
      t475 = t472 * t55 * t113 / 0.720D3
      t476 = FJET(XB1, XB2, s, t384, -t386, -t403, -t406, t213, t475)
      t479 = t55 * t112 * t57
      t494 = t232 / 0.720D3 + (-0.90D2 * t7 * t9 * t244 + 0.180D3 * t7 *
     # t40 * t249 - t254) * t112 * t57 / 0.720D3
      t495 = FJET(XB1, XB2, s, -t208, -t5, -t205, 0.0D0, t213, t494)
      t497 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t500 = log(0.4D1 * t312 * t233)
      t501 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t506 = t500 ** 2
      t517 = t7 * t85 * t501
      t529 = t267 * t13 * t17
      t531 = log(0.4D1 * t529)
      t532 = t531 ** 2
      t533 = t532 * t6
      t534 = 0.3141592653589793D1 * lh
      t539 = t532 * t531 * t6
      t542 = t531 * t6
      t543 = 0.3141592653589793D1 * t50
      t556 = t48 ** 2
      t557 = t46 ** 2
      t563 = t532 ** 2
      t573 = log(0.4D1 * t11 * t529)
      t575 = t573 ** 2
      t595 = log(0.4D1 * t59 * t268)
      t600 = t595 ** 2
      t622 = log(0.4D1 * t294 * t529)
      t624 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t629 = log(0.4D1 * t294 * t233 * t267 * t215)
      t630 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t636 = t630 - t501
      t646 = log(0.4D1 * t115 * t529)
      t648 = t646 ** 2
      t654 = log(0.4D1 * t116 * t268 * t215)
      t656 = t654 ** 2
      t670 = t7 * t51 * t636
      t675 = t233 * t215
      t678 = log(0.4D1 * t343 * t675)
      t680 = t678 ** 2
      t685 = log(0.4D1 * t97 * t529)
      t687 = t685 ** 2
      t704 = x2 * t267
      t707 = log(0.4D1 * t704 * t675)
      t711 = log(0.4D1 * t704 * t233)
      t716 = t711 ** 2
      t722 = t707 ** 2
      t748 = (t7 * t51 * (-t497 + t500 * t501) - 0.90D2 * t7 * t9 * (-t5
     #06 * t497 / 0.2D1 + t506 * t500 * t501 / 0.6D1) - t517 + 0.180D3 *
     # t7 * t40 * (t500 * t497 - t506 * t501 / 0.2D1)) * t55 / 0.1440D4 
     #- (0.90D2 * t533 * t534 + t7 * t84 + 0.15D2 * t539 * 0.31415926535
     #89793D1 - t542 * t543) * t9 * t497 / 0.1440D4 - (-0.30D2 * t539 * 
     #t534 + t533 * t543 / 0.2D1 - t542 * 0.3141592653589793D1 * t84 + t
     #7 * (-0.5769873135166051D3 * lh - t556 - 0.60D2 * t557 + 0.60D2 * 
     #t46 * t48) - 0.15D2 / 0.4D1 * t563 * t6 * 0.3141592653589793D1) * 
     #t9 * t501 / 0.1440D4 + (-0.90D2 * t7 * t9 * (t573 * t497 - t575 * 
     #t501 / 0.2D1) + 0.180D3 * t7 * t40 * (-t497 + t573 * t501) - t7 * 
     #t51 * t501) * t55 * t57 / 0.720D3 - (t7 * t51 * (t497 - t595 * t50
     #1) - 0.90D2 * t7 * t9 * (t600 * t497 / 0.2D1 - t600 * t595 * t501 
     #/ 0.6D1) + t517 + 0.180D3 * t7 * t40 * (-t595 * t497 + t600 * t501
     # / 0.2D1)) * t57 / 0.720D3 + (-0.90D2 * t7 * t9 * (-t497 + t622 * 
     #t501 + t624 - t629 * t630) + 0.180D3 * t7 * t40 * t636) * t55 * t1
     #13 / 0.720D3 + (-0.90D2 * t7 * t9 * (t646 * t497 - t648 * t501 / 0
     #.2D1 - t654 * t624 + t656 * t630 / 0.2D1) + 0.180D3 * t7 * t40 * (
     #-t497 + t646 * t501 + t624 - t654 * t630) + t670) * t112 * t57 / 0
     #.720D3 + (-0.90D2 * t7 * t9 * (-t678 * t624 + t680 * t630 / 0.2D1 
     #+ t685 * t497 - t687 * t501 / 0.2D1) + 0.180D3 * t7 * t40 * (t624 
     #- t678 * t630 - t497 + t685 * t501) + t670) * t55 * t112 / 0.1440D
     #4 - (t7 * t51 * (-t624 + t707 * t630 + t497 - t711 * t501) - 0.90D
     #2 * t7 * t9 * (t716 * t497 / 0.2D1 - t716 * t711 * t501 / 0.6D1 - 
     #t722 * t624 / 0.2D1 + t722 * t707 * t630 / 0.6D1) - t7 * t85 * t63
     #6 + 0.180D3 * t7 * t40 * (t707 * t624 - t722 * t630 / 0.2D1 - t711
     # * t497 + t716 * t501 / 0.2D1)) * t112 / 0.1440D4
      t749 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t748)
      t751 = t2 * t405
      t752 = t2 * t455
      t754 = t13 * x2 * t154
      t755 = t267 * t413
      t759 = log(-0.4D1 * t754 * t410 * t755)
      t760 = t759 * z
      t761 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t455
     #, x4)
      t763 = t759 ** 2
      t765 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t455
     #, x4)
      t771 = Sqrt(-t369 * t373)
      t775 = 0.1D1 / (-z - t97 + 0.2D1 * t371 * t771)
      t779 = t7 * lh
      t780 = z * t761
      t790 = t9 * z * t765 * t775
      t800 = log(-0.4D1 * t754 * t410 * t755 * t10)
      t814 = (-0.90D2 * t7 * t9 * (-t760 * t761 + t763 * z * t765 / 0.2D
     #1) * t775 + 0.180D3 * t779 * t9 * (t780 - t760 * t765) * t775 + t7
     # * t50 * t790) * t55 * t112 / 0.1440D4 + (-0.90D2 * t7 * t9 * (t78
     #0 - t800 * z * t765) * t775 + 0.180D3 * t779 * t790) * t55 * t113 
     #/ 0.720D3
      t815 = FJET(XB1, XB2, s, 0.0D0, t751, 0.0D0, t752, 0.0D0, t814)
      t817 = FJET(XB1, XB2, s, -t205, 0.0D0, -t208, -t5, t213, t494)
      t819 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t137)
      t821 = FJET(XB1, XB2, s, -t386, t384, -t406, -t403, t213, t475)
      t825 = FJET(XB1, XB2, s, -t403, -t406, t384, -t386, t213, t475)
      t829 = t138 * t137 + t201 * t200 + t260 * t259 + t262 * t137 + t36
     #5 * t364 + t367 * t364 + t476 * t472 * t479 / 0.720D3 + t495 * t49
     #4 + t749 * t748 + t815 * t814 + t817 * t494 + t819 * t137 + t821 *
     # t472 * t479 / 0.720D3 + t825 * t472 * t479 / 0.720D3
      t830 = FJET(XB1, XB2, s, -t406, -t403, -t386, t384, t213, t475)
      t834 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t748)
      t836 = FJET(XB1, XB2, s, t752, 0.0D0, t751, 0.0D0, 0.0D0, t814)
      t838 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t137)
      t852 = (0.90D2 * t7 * t9 * t166 - 0.180D3 * t7 * t40 * t171 - t176
     #) * t55 * t57 / 0.720D3 + t198 / 0.720D3
      t853 = FJET(XB1, XB2, s, t145, -t143, -t149, t147, 0.0D0, t852)
      t855 = FJET(XB1, XB2, s, t751, 0.0D0, t752, 0.0D0, 0.0D0, t814)
      t857 = FJET(XB1, XB2, s, 0.0D0, t752, 0.0D0, t751, 0.0D0, t814)
      t859 = FJET(XB1, XB2, s, 0.0D0, t265, 0.0D0, -t264, 0.0D0, t364)
      t861 = FJET(XB1, XB2, s, -t5, -t208, 0.0D0, -t205, t213, t494)
      t863 = FJET(XB1, XB2, s, -t149, t147, t145, -t143, 0.0D0, t200)
      t865 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t748)
      t867 = FJET(XB1, XB2, s, t265, 0.0D0, -t264, 0.0D0, 0.0D0, t364)
      t869 = FJET(XB1, XB2, s, t147, -t149, -t143, t145, 0.0D0, t852)
      t871 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t748)
      t873 = t830 * t472 * t479 / 0.720D3 + t834 * t748 + t836 * t814 + 
     #t838 * t137 + t853 * t852 + t855 * t814 + t857 * t814 + t859 * t36
     #4 + t861 * t494 + t863 * t200 + t865 * t748 + t867 * t364 + t869 *
     # t852 + t871 * t748
      rrqqbar2gght5s4e1 = t829 + t873

      end function



      doubleprecision function rrqqbar2gght5s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x2 * x1
      t4 = x1 * z
      t5 = -z - x1 + t4
      t6 = 0.1D1 / t5
      t8 = t2 * t3 * t6
      t9 = -0.1D1 + x1
      t10 = t2 * t9
      t11 = -0.1D1 + x2
      t13 = t2 * x1 * t11
      t14 = t1 ** 2
      t19 = s * t14 * x2 * x1 * t9 * t6
      t20 = 0.1D1 / t1
      t21 = t20 * 0.3141592653589793D1
      t22 = s ** 2
      t23 = 0.1D1 / t22
      t24 = t21 * t23
      t25 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 
     #x4)
      t26 = 0.1D1 / x3
      t28 = 0.1D1 / x2
      t29 = 0.1D1 / x1
      t30 = t28 * t29
      t33 = t24 * t25 * t26 * t30 / 0.8D1
      t34 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 
     #x4)
      t35 = x1 ** 2
      t36 = x2 * t35
      t37 = t14 ** 2
      t38 = x4 * 0.3141592653589793D1
      t39 = Sin(t38)
      t40 = t39 ** 2
      t41 = t37 * t40
      t43 = z ** 2
      t44 = 0.1D1 / t43
      t45 = t44 * t6
      t46 = t9 ** 2
      t47 = t11 ** 2
      t52 = log(-0.4D1 * t36 * t41 * t45 * t46 * t47)
      t54 = -t34 + t52 * t25
      t58 = lh * t23
      t61 = 0.180D3 * t21 * t58 * t25
      t66 = t33 + (-0.90D2 * t21 * t23 * t54 - t61) * t28 * t29 / 0.720D
     #3
      t67 = FJET(XB1, XB2, s, 0.0D0, -t8, -t10, -t13, t19, t66)
      t69 = -0.1D1 + x3
      t70 = x2 * x3
      t71 = -0.1D1 + t70
      t72 = 0.1D1 / t71
      t73 = t69 * t72
      t74 = t2 * t73
      t76 = x3 * t11 * t72
      t77 = t2 * t76
      t78 = t23 * z
      t80 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t73, 
     #x4)
      t81 = cos(t38)
      t82 = x3 * z
      t83 = x2 * t69
      t85 = Sqrt(-t82 * t83)
      t89 = 0.1D1 / (-z - t70 + 0.2D1 * t81 * t85)
      t90 = t80 * t89
      t92 = t26 * t28 * t29
      t96 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t73, 
     #x4)
      t99 = x3 * t69
      t103 = 0.1D1 / t43 / z
      t104 = t71 ** 2
      t110 = log(-0.4D1 * t37 * x2 * t99 * t47 * t40 * t103 / t104)
      t126 = -t21 * t78 * t90 * t92 / 0.8D1 + (-0.90D2 * t21 * t23 * (z 
     #* t96 - t110 * z * t80) * t89 + 0.180D3 * t21 * lh * t78 * t90) * 
     #t26 * t28 / 0.1440D4
      t127 = FJET(XB1, XB2, s, t74, 0.0D0, t77, 0.0D0, 0.0D0, t126)
      t129 = t2 * t69
      t130 = t2 * x3
      t131 = -t69
      t132 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #131, x4)
      t133 = t70 * t103
      t134 = t41 * t69
      t137 = log(-0.4D1 * t133 * t134)
      t138 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #131, x4)
      t146 = 0.180D3 * t21 * t58 * t138
      t151 = x3 * t103
      t154 = log(-0.4D1 * t151 * t134)
      t156 = t154 ** 2
      t168 = lh ** 2
      t170 = 0.3141592653589793D1 ** 2
      t172 = -0.180D3 * t168 + 0.30D2 * t170
      t173 = t172 * t23
      t179 = x3 * t35
      t180 = t179 * t37
      t181 = t40 * t103
      t185 = log(-0.4D1 * t180 * t181 * t69)
      t199 = (-0.90D2 * t21 * t23 * (t132 - t137 * t138) + t146) * t26 *
     # t28 / 0.1440D4 + (-0.90D2 * t21 * t23 * (-t154 * t132 + t156 * t1
     #38 / 0.2D1) + 0.180D3 * t21 * t58 * (t132 - t154 * t138) + t21 * t
     #173 * t138) * t26 / 0.1440D4 + (-0.90D2 * t21 * t23 * (t132 - t185
     # * t138) + t146) * t26 * t29 / 0.720D3 - t24 * t138 * t26 * t30 / 
     #0.8D1
      t200 = FJET(XB1, XB2, s, 0.0D0, -t129, 0.0D0, t130, 0.0D0, t199)
      t202 = FJET(XB1, XB2, s, 0.0D0, t74, 0.0D0, t77, 0.0D0, t126)
      t204 = FJET(XB1, XB2, s, t130, 0.0D0, -t129, 0.0D0, 0.0D0, t199)
      t206 = FJET(XB1, XB2, s, 0.0D0, t77, 0.0D0, t74, 0.0D0, t126)
      t208 = t2 * x1
      t209 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10
     #D1, x4)
      t212 = t40 * t44 * t46 * t6
      t215 = log(-0.4D1 * t180 * t212)
      t216 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10
     #D1, x4)
      t224 = 0.180D3 * t21 * t58 * t216
      t233 = t36 * t37
      t236 = log(-0.4D1 * t233 * t212)
      t246 = t35 * t37
      t251 = log(-0.4D1 * t246 * t40 * t45 * t46)
      t253 = t251 ** 2
      t270 = (-0.90D2 * t21 * t23 * (t209 - t215 * t216) + t224) * t26 *
     # t29 / 0.720D3 - t24 * t216 * t26 * t30 / 0.8D1 + (-0.90D2 * t21 *
     # t23 * (t209 - t236 * t216) + t224) * t28 * t29 / 0.720D3 - (-0.90
     #D2 * t21 * t23 * (t251 * t209 - t253 * t216 / 0.2D1) + 0.180D3 * t
     #21 * t58 * (-t209 + t251 * t216) - t21 * t173 * t216) * t29 / 0.72
     #0D3
      t271 = FJET(XB1, XB2, s, t208, -t10, 0.0D0, 0.0D0, 0.0D0, t270)
      t273 = FJET(XB1, XB2, s, 0.0D0, t130, 0.0D0, -t129, 0.0D0, t199)
      t277 = log(0.4D1 * t151 * t41)
      t278 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t280 = t277 ** 2
      t281 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0
     #.10D1, x4)
      t294 = t21 * t173 * t281
      t299 = t103 * t37 * t40
      t301 = log(0.4D1 * t299)
      t302 = t301 * t20
      t303 = 0.3141592653589793D1 * lh
      t306 = t301 ** 2
      t307 = t306 * t20
      t333 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t334 = t41 * t47
      t337 = log(0.4D1 * t133 * t334)
      t338 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10
     #D1, x4)
      t342 = log(0.4D1 * t70 * t299)
      t348 = t338 - t281
      t351 = 0.180D3 * t21 * t58 * t348
      t356 = x2 * t103
      t359 = log(0.4D1 * t356 * t334)
      t361 = t359 ** 2
      t366 = log(0.4D1 * t356 * t41)
      t368 = t366 ** 2
      t389 = log(0.4D1 * t179 * t299)
      t408 = log(0.4D1 * t36 * t299)
      t413 = log(0.4D1 * t233 * t181 * t47)
      t425 = log(0.4D1 * t246 * t181)
      t427 = t425 ** 2
      t442 = (-0.90D2 * t21 * t23 * (t277 * t278 - t280 * t281 / 0.2D1) 
     #+ 0.180D3 * t21 * t58 * (-t278 + t277 * t281) - t294) * t26 / 0.14
     #40D4 - (-0.180D3 * t302 * t303 - 0.45D2 * t307 * 0.314159265358979
     #3D1 + t21 * t172) * t23 * t278 / 0.1440D4 - (0.90D2 * t307 * t303 
     #+ t21 * (-0.60D2 * lh * t170 + 0.2884936567583026D3 + 0.120D3 * t1
     #68 * lh) + 0.15D2 * t306 * t301 * t20 * 0.3141592653589793D1 - t30
     #2 * 0.3141592653589793D1 * t172) * t23 * t281 / 0.1440D4 + (-0.90D
     #2 * t21 * t23 * (t333 - t337 * t338 - t278 + t342 * t281) + t351) 
     #* t26 * t28 / 0.1440D4 - (-0.90D2 * t21 * t23 * (t359 * t333 - t36
     #1 * t338 / 0.2D1 - t366 * t278 + t368 * t281 / 0.2D1) + 0.180D3 * 
     #t21 * t58 * (-t333 + t359 * t338 + t278 - t366 * t281) - t21 * t17
     #3 * t348) * t28 / 0.1440D4 + (-0.90D2 * t21 * t23 * (-t278 + t389 
     #* t281) - 0.180D3 * t21 * t58 * t281) * t26 * t29 / 0.720D3 - t24 
     #* t348 * t26 * t30 / 0.8D1 + (-0.90D2 * t21 * t23 * (-t278 + t408 
     #* t281 + t333 - t413 * t338) + t351) * t28 * t29 / 0.720D3 - (-0.9
     #0D2 * t21 * t23 * (-t425 * t278 + t427 * t281 / 0.2D1) + 0.180D3 *
     # t21 * t58 * (t278 - t425 * t281) + t294) * t29 / 0.720D3
      t443 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t442)
      t445 = x3 * x1
      t446 = t445 * z
      t449 = Sqrt(x3 * t5 * t83)
      t450 = t81 * t449
      t451 = 0.2D1 * t450
      t456 = t208 * t11 * (-t70 - z + t82 - x1 + t445 + t4 - t446 + t451
     #) * t6 * t72
      t457 = t69 * s
      t458 = t1 * t9
      t460 = t457 * t458 * t72
      t461 = t70 * z
      t463 = t70 * x1
      t465 = x2 ** 2
      t466 = t465 * x3
      t467 = t466 * x1
      t471 = t466 * t4
      t474 = t446 + 0.2D1 * t461 + 0.2D1 * t463 - t467 - t466 * z + t70 
     #- x2 - t82 - t445 - 0.2D1 * t70 * t4 + t471 + 0.2D1 * t450 * x2 - 
     #t451
      t477 = t208 * t474 * t6 * t72
      t478 = t10 * t76
      t479 = t3 * z
      t480 = z + x1 - t4 - t3 + t479
      t493 = -t35 + t479 - t461 - t463 - t179 * x2 + t467 - t3 * t43 - 0
     #.2D1 * t36 * z + t36 * t43 - 0.2D1 * t450 * t3 - 0.2D1 * t450 * t4
     # + 0.2D1 * t450 * t479
      t511 = -0.2D1 * t4 + x3 * t43 * t3 + 0.2D1 * t179 * x2 * z - t179 
     #* t43 * x2 - t471 + 0.2D1 * t450 * z + 0.2D1 * t450 * x1 + 0.2D1 *
     # x1 * t43 + 0.2D1 * t35 * z - t35 * t43 + t36 - t43
      t513 = 0.1D1 / (t493 + t511)
      t516 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t73, x4
     #)
      t520 = t21 * t23 * t480 * t513 * t5 * t516 * t92 / 0.8D1
      t521 = FJET(XB1, XB2, s, t456, -t460, -t477, -t478, t19, -t520)
      t524 = 0.3141592653589793D1 * t23 * t480
      t528 = t513 * t5 * t516 * t92
      t539 = t33 + (-0.90D2 * t21 * t23 * t54 - t61) * t28 * t29 / 0.720
     #D3
      t540 = FJET(XB1, XB2, s, -t13, -t10, -t8, 0.0D0, t19, t539)
      t542 = FJET(XB1, XB2, s, -t8, 0.0D0, -t13, -t10, t19, t539)
      t544 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t10, t208, 0.0D0, t270)
      t546 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t208, -t10, 0.0D0, t270)
      t548 = t67 * t66 + t127 * t126 + t200 * t199 + t202 * t126 + t204 
     #* t199 + t206 * t126 + t271 * t270 + t273 * t199 + t443 * t442 - t
     #521 * t20 * t524 * t528 / 0.8D1 + t540 * t539 + t542 * t539 + t544
     # * t270 + t546 * t270
      t550 = t2 * t9 * x3
      t551 = t2 * t445
      t552 = t457 * t458
      t554 = t457 * t1 * x1
      t555 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t131
     #, x4)
      t563 = log(0.4D1 * t6 * t40 * t44 * t37 * t35 * t46 * t99)
      t564 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t131
     #, x4)
      t566 = t555 - t563 * t564
      t572 = 0.180D3 * t21 * t58 * t564
      t580 = t24 * t564 * t26 * t30 / 0.8D1
      t581 = (0.90D2 * t21 * t23 * t566 - t572) * t26 * t29 / 0.720D3 + 
     #t580
      t582 = FJET(XB1, XB2, s, -t550, t551, t552, -t554, 0.0D0, t581)
      t584 = FJET(XB1, XB2, s, -t554, t552, t551, -t550, 0.0D0, t581)
      t586 = FJET(XB1, XB2, s, -t129, 0.0D0, t130, 0.0D0, 0.0D0, t199)
      t588 = FJET(XB1, XB2, s, t77, 0.0D0, t74, 0.0D0, 0.0D0, t126)
      t590 = FJET(XB1, XB2, s, -t477, -t478, t456, -t460, t19, -t520)
      t595 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t442)
      t605 = (0.90D2 * t21 * t23 * t566 - t572) * t26 * t29 / 0.720D3 + 
     #t580
      t606 = FJET(XB1, XB2, s, t552, -t554, -t550, t551, 0.0D0, t605)
      t608 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t442)
      t610 = FJET(XB1, XB2, s, t551, -t550, -t554, t552, 0.0D0, t605)
      t612 = FJET(XB1, XB2, s, -t10, t208, 0.0D0, 0.0D0, 0.0D0, t270)
      t614 = FJET(XB1, XB2, s, -t478, -t477, -t460, t456, t19, -t520)
      t619 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t442)
      t621 = FJET(XB1, XB2, s, -t460, t456, -t478, -t477, t19, -t520)
      t626 = FJET(XB1, XB2, s, -t10, -t13, 0.0D0, -t8, t19, t539)
      t628 = t582 * t581 + t584 * t581 + t586 * t199 + t588 * t126 - t59
     #0 * t20 * t524 * t528 / 0.8D1 + t595 * t442 + t606 * t605 + t608 *
     # t442 + t610 * t605 + t612 * t270 - t614 * t20 * t524 * t528 / 0.8
     #D1 + t619 * t442 - t621 * t20 * t524 * t528 / 0.8D1 + t626 * t539
      rrqqbar2gght5s4e0 = t548 + t628

      end function



      doubleprecision function rrqqbar2gght5s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = t1 ** 2
      t13 = t12 ** 2
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #10D1, x4)
      t27 = lh * t6
      t30 = 0.180D3 * t4 * t27 * t21
      t32 = 0.1D1 / x3
      t40 = log(0.4D1 * t10 * t13 * t16)
      t41 = t40 * t3
      t51 = t40 ** 2
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t65 = t4 * t6
      t66 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t67 = t66 - t21
      t69 = 0.1D1 / x2
      t73 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t74 = x2 * t10
      t75 = -0.1D1 + x2
      t76 = t75 ** 2
      t80 = log(0.4D1 * t74 * t17 * t76)
      t84 = log(0.4D1 * t74 * t17)
      t98 = 0.1D1 / x1
      t102 = x1 ** 2
      t103 = t102 * t13
      t107 = log(0.4D1 * t103 * t16 * t10)
      t120 = (-0.90D2 * t4 * t6 * (-t7 + t20 * t21) - t30) * t32 / 0.144
     #0D4 - (0.180D3 * t4 * lh + 0.90D2 * t41 * 0.3141592653589793D1) * 
     #t6 * t7 / 0.1440D4 - (-0.180D3 * t41 * 0.3141592653589793D1 * lh -
     # 0.45D2 * t51 * t3 * 0.3141592653589793D1 + t4 * (-0.180D3 * t55 +
     # 0.30D2 * t57)) * t6 * t21 / 0.1440D4 - t65 * t67 * t32 * t69 / 0.
     #16D2 - (-0.90D2 * t4 * t6 * (-t73 + t80 * t66 + t7 - t84 * t21) - 
     #0.180D3 * t4 * t27 * t67) * t69 / 0.1440D4 - t65 * t67 * t69 * t98
     # / 0.8D1 - (-0.90D2 * t4 * t6 * (t7 - t107 * t21) + t30) * t98 / 0
     #.720D3 + t65 * t21 * t32 * t98 / 0.8D1
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t120)
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t120)
      t125 = t2 * x1
      t126 = -0.1D1 + x1
      t127 = t2 * t126
      t128 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10
     #D1, x4)
      t133 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10
     #D1, x4)
      t138 = 0.1D1 / (-z - x1 + x1 * z)
      t140 = t126 ** 2
      t144 = log(-0.4D1 * t103 * t16 / t8 * t138 * t140)
      t160 = -t65 * t128 * t69 * t98 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t1
     #33 + t144 * t128) - 0.180D3 * t4 * t27 * t128) * t98 / 0.720D3 - t
     #65 * t128 * t32 * t98 / 0.8D1
      t161 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t125, -t127, 0.0D0, t160)
      t163 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t127, t125, 0.0D0, t160)
      t165 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t120)
      t167 = t2 * x3
      t168 = -0.1D1 + x3
      t169 = t2 * t168
      t170 = -t168
      t171 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #170, x4)
      t172 = t171 * t32
      t176 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t
     #170, x4)
      t180 = log(-0.4D1 * t11 * t17 * t168)
      t195 = -t65 * t172 * t69 / 0.16D2 + (-0.90D2 * t4 * t6 * (t176 - t
     #180 * t171) + 0.180D3 * t4 * t27 * t171) * t32 / 0.1440D4 - t65 * 
     #t172 * t98 / 0.8D1
      t196 = FJET(XB1, XB2, s, 0.0D0, t167, 0.0D0, -t169, 0.0D0, t195)
      t198 = x2 * x3
      t200 = 0.1D1 / (-0.1D1 + t198)
      t201 = t168 * t200
      t202 = t2 * t201
      t205 = t2 * x3 * t75 * t200
      t208 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t201
     #, x4)
      t209 = cos(t14)
      t213 = Sqrt(-x3 * z * x2 * t168)
      t217 = 0.1D1 / (-z - t198 + 0.2D1 * t209 * t213)
      t222 = t4 * t6 * z * t208 * t217 * t32 * t69 / 0.16D2
      t223 = FJET(XB1, XB2, s, 0.0D0, t202, 0.0D0, t205, 0.0D0, -t222)
      t225 = 0.3141592653589793D1 * t6
      t230 = z * t208 * t217 * t32 * t69
      t233 = FJET(XB1, XB2, s, 0.0D0, t205, 0.0D0, t202, 0.0D0, -t222)
      t238 = FJET(XB1, XB2, s, 0.0D0, -t169, 0.0D0, t167, 0.0D0, t195)
      t242 = t2 * x1 * x2 * t138
      t244 = t2 * x1 * t75
      t249 = s * t12 * x2 * x1 * t126 * t138
      t250 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t254 = t65 * t250 * t69 * t98 / 0.8D1
      t255 = FJET(XB1, XB2, s, 0.0D0, -t242, -t127, -t244, t249, t254)
      t260 = t6 * t250 * t69 * t98
      t263 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t120)
      t265 = FJET(XB1, XB2, s, t125, -t127, 0.0D0, 0.0D0, 0.0D0, t160)
      t267 = t121 * t120 + t123 * t120 + t161 * t160 + t163 * t160 + t16
     #5 * t120 + t196 * t195 - t223 * t3 * t225 * t230 / 0.16D2 - t233 *
     # t3 * t225 * t230 / 0.16D2 + t238 * t195 + t255 * t3 * 0.314159265
     #3589793D1 * t260 / 0.8D1 + t263 * t120 + t265 * t160
      t268 = FJET(XB1, XB2, s, t167, 0.0D0, -t169, 0.0D0, 0.0D0, t195)
      t271 = t2 * x1 * x3
      t273 = t2 * t126 * x3
      t274 = t168 * s
      t276 = t274 * t1 * x1
      t278 = t274 * t1 * t126
      t279 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t170
     #, x4)
      t283 = t65 * t279 * t32 * t98 / 0.8D1
      t284 = FJET(XB1, XB2, s, t271, -t273, -t276, t278, 0.0D0, t283)
      t289 = t6 * t279 * t32 * t98
      t292 = FJET(XB1, XB2, s, t278, -t276, -t273, t271, 0.0D0, t283)
      t297 = FJET(XB1, XB2, s, t202, 0.0D0, t205, 0.0D0, 0.0D0, -t222)
      t302 = FJET(XB1, XB2, s, t205, 0.0D0, t202, 0.0D0, 0.0D0, -t222)
      t307 = FJET(XB1, XB2, s, -t127, t125, 0.0D0, 0.0D0, 0.0D0, t160)
      t309 = FJET(XB1, XB2, s, -t127, -t244, 0.0D0, -t242, t249, t254)
      t314 = FJET(XB1, XB2, s, -t169, 0.0D0, t167, 0.0D0, 0.0D0, t195)
      t316 = FJET(XB1, XB2, s, -t244, -t127, -t242, 0.0D0, t249, t254)
      t321 = FJET(XB1, XB2, s, -t273, t271, t278, -t276, 0.0D0, t283)
      t326 = FJET(XB1, XB2, s, -t276, t278, t271, -t273, 0.0D0, t283)
      t331 = FJET(XB1, XB2, s, -t242, 0.0D0, -t244, -t127, t249, t254)
      t336 = t268 * t195 + t284 * t3 * 0.3141592653589793D1 * t289 / 0.8
     #D1 + t292 * t3 * 0.3141592653589793D1 * t289 / 0.8D1 - t297 * t3 *
     # t225 * t230 / 0.16D2 - t302 * t3 * t225 * t230 / 0.16D2 + t307 * 
     #t160 + t309 * t3 * 0.3141592653589793D1 * t260 / 0.8D1 + t314 * t1
     #95 + t316 * t3 * 0.3141592653589793D1 * t260 / 0.8D1 + t321 * t3 *
     # 0.3141592653589793D1 * t289 / 0.8D1 + t326 * t3 * 0.3141592653589
     #793D1 * t289 / 0.8D1 + t331 * t3 * 0.3141592653589793D1 * t260 / 0
     #.8D1
      rrqqbar2gght5s4em1 = t267 + t336

      end function



      doubleprecision function rrqqbar2gght5s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t4 = t3 * 0.3141592653589793D1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t20 = 0.1D1 / x1
      t24 = rrqqbar2ggh51J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.
     #10D1, x4)
      t30 = z ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = Sin(x4 * 0.3141592653589793D1)
      t38 = t37 ** 2
      t41 = log(0.4D1 / t30 / z * t34 * t38)
      t49 = t4 * t8 * t9 / 0.16D2 + t4 * t6 * (-t13 + t7) / x2 / 0.16D2 
     #+ t4 * t8 * t20 / 0.8D1 + t4 * t6 * t24 / 0.16D2 - (0.180D3 * t4 *
     # lh + 0.90D2 * t41 * t3 * 0.3141592653589793D1) * t6 * t7 / 0.1440
     #D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t54 = t2 * x1
      t56 = t2 * (-0.1D1 + x1)
      t57 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D
     #1, x4)
      t59 = t6 * t57 * t20
      t61 = t4 * t59 / 0.8D1
      t62 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t54, -t56, 0.0D0, -t61)
      t67 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t56, t54, 0.0D0, -t61)
      t72 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t49)
      t74 = t2 * x3
      t75 = -0.1D1 + x3
      t76 = t2 * t75
      t78 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, -t
     #75, x4)
      t80 = t6 * t78 * t9
      t82 = t4 * t80 / 0.16D2
      t83 = FJET(XB1, XB2, s, 0.0D0, t74, 0.0D0, -t76, 0.0D0, -t82)
      t88 = FJET(XB1, XB2, s, 0.0D0, -t76, 0.0D0, t74, 0.0D0, -t82)
      t93 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t95 = FJET(XB1, XB2, s, t54, -t56, 0.0D0, 0.0D0, 0.0D0, -t61)
      t100 = FJET(XB1, XB2, s, t74, 0.0D0, -t76, 0.0D0, 0.0D0, -t82)
      t105 = FJET(XB1, XB2, s, -t76, 0.0D0, t74, 0.0D0, 0.0D0, -t82)
      t110 = FJET(XB1, XB2, s, -t56, t54, 0.0D0, 0.0D0, 0.0D0, -t61)
      rrqqbar2gght5s4em2 = t50 * t49 + t52 * t49 - t62 * t3 * 0.31415926
     #53589793D1 * t59 / 0.8D1 - t67 * t3 * 0.3141592653589793D1 * t59 /
     # 0.8D1 + t72 * t49 - t83 * t3 * 0.3141592653589793D1 * t80 / 0.16D
     #2 - t88 * t3 * 0.3141592653589793D1 * t80 / 0.16D2 + t93 * t49 - t
     #95 * t3 * 0.3141592653589793D1 * t59 / 0.8D1 - t100 * t3 * 0.31415
     #92653589793D1 * t80 / 0.16D2 - t105 * t3 * 0.3141592653589793D1 * 
     #t80 / 0.16D2 - t110 * t3 * 0.3141592653589793D1 * t59 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght5s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqqbar2ggh51J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t10 = t3 * 0.3141592653589793D1 * t6 * t7 / 0.16D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = 0.3141592653589793D1 * t6 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t19 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      t22 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2gght5s4em3 = t11 * t3 * t14 / 0.16D2 + t16 * t3 * t14 / 0.
     #16D2 + t19 * t3 * t14 / 0.16D2 + t22 * t3 * t14 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght5s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh51J1
      doubleprecision rrqqbar2ggh51J2
      rrqqbar2gght5s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh51J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = x3 ** 2
      t11 = x3 * t10
      t14 = s * t3
      t16 = z + x1 * t3
      t18 = x1 / t16
      t19 = 0.1D1 - x2
      t20 = x3 * t19
      t22 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t20 * t16 * x2 * t22)
      t31 = 0.2D1 * t25 * t29
      t32 = t20 * t16 + x2 * t22 - t31
      t33 = t18 * t32
      t35 = t7 * x3
      t37 = s - t14 * t33 - t14 * t35
      t41 = t22 * t19 * t16 + x2 * x3 + t31
      t46 = s - t14 * t18 * t41 - t14 * t7 * t22
      t47 = t37 * t46
      t51 = x1 ** 2
      t53 = t16 ** 2
      t56 = t51 * x1 / t53 / t16
      t57 = t32 ** 2
      t58 = t57 * t32
      t63 = t2 * s * t5
      t64 = t63 * t56
      t66 = x3 * t37
      t67 = t41 ** 2
      t76 = t63 * t18
      t77 = t41 * t9
      t78 = t22 ** 2
      t79 = t78 * t22
      t83 = t67 * t41
      t85 = t22 * t46
      t115 = -t6 * t9 * t11 * t47 * t33 - t6 * t35 * t47 * t56 * t58 - t
     #64 * t32 * t7 * t66 * t67 - 0.2D1 * t64 * t57 * t7 * t66 * t41 - t
     #76 * t77 * t79 * t46 - t64 * t83 * t7 * t85 - t6 * t56 * t83 * t37
     # * t46 * t7 * t22 - t6 * t18 * t41 * t37 * t46 * t9 * t79 - t76 * 
     #t32 * t9 * t11 * t37 - t76 * t77 * t85 * t10 - t64 * t58 * t7 * t6
     #6 - 0.2D1 * t76 * t77 * t78 * t46 * x3
      rrqqbar2ggh51J1 = -0.16D2 / 0.3D1 * wd * t115 / t1 / t37 / t46 / z
     # / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2ggh51J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t2 * s * t6
      t8 = x1 ** 2
      t9 = t8 * x1
      t11 = z + x1 * t4
      t12 = t11 ** 2
      t14 = 0.1D1 / t12 / t11
      t15 = t9 * t14
      t16 = t7 * t15
      t17 = 0.1D1 - x2
      t18 = x3 * t17
      t20 = 0.1D1 - x3
      t23 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(t18 * t11 * x2 * t20)
      t29 = 0.2D1 * t23 * t27
      t30 = t18 * t11 + x2 * t20 - t29
      t31 = t30 ** 2
      t32 = 0.1D1 - x1
      t34 = s * t4
      t36 = x1 / t11
      t37 = t36 * t30
      t39 = t32 * x3
      t41 = s - t34 * t37 - t34 * t39
      t42 = x3 * t41
      t46 = t20 * t17 * t11 + x2 * x3 + t29
      t51 = t7 * t36
      t52 = t32 ** 2
      t53 = t52 * t32
      t55 = x3 ** 2
      t56 = t55 * x3
      t60 = t31 * t30
      t64 = t2 * t6
      t66 = t64 * t39 * t41
      t67 = t36 * t46
      t71 = s - t34 * t67 - t34 * t32 * t20
      t72 = t71 * t9
      t74 = t46 ** 2
      t82 = 0.1D1 / t12
      t83 = t8 * t82
      t84 = t7 * t83
      t86 = t55 * t41
      t91 = t52 * t55
      t101 = t41 * t71
      t106 = t64 * t67
      t115 = t20 ** 2
      t120 = t74 * t52
      t121 = t20 * t71
      t126 = 0.3D1 * t16 * t31 * t32 * t42 * t46 + t51 * t30 * t53 * t56
     # * t41 + t16 * t60 * t32 * t42 - t66 * t72 * t14 * t30 * t74 + t16
     # * t30 * t32 * t42 * t74 + 0.3D1 * t84 * t30 * t52 * t86 * t46 - t
     #64 * t91 * t41 * t71 * t8 * t82 * t30 * t46 - t64 * t83 * t74 * t1
     #01 * t52 * t20 * x3 - t106 * t101 * t53 * t20 * t55 - t66 * t72 * 
     #t14 * t31 * t46 - t106 * t101 * t53 * t115 * x3 + 0.3D1 * t84 * t1
     #20 * t121 * x3
      t127 = t115 * t71
      t158 = t115 * t20
      t163 = t74 * t46
      t172 = t46 * t53
      t183 = 0.2D1 * t84 * t120 * t127 + 0.2D1 * t64 * t91 * t101 * t83 
     #* t31 + t64 * t39 * t101 * t15 * t60 + t64 * t53 * t56 * t101 * t3
     #7 + 0.2D1 * t84 * t31 * t52 * t86 + 0.2D1 * t64 * t83 * t74 * t41 
     #* t71 * t52 * t115 + t64 * t36 * t46 * t41 * t71 * t53 * t158 + t6
     #4 * t15 * t163 * t41 * t71 * t32 * t20 + t16 * t163 * t32 * t121 +
     # t51 * t172 * t158 * t71 + 0.3D1 * t51 * t172 * t127 * x3 + t51 * 
     #t172 * t121 * t55
      rrqqbar2ggh51J2 = -0.16D2 / 0.3D1 * wd * (t126 + t183) / t1 / t41 
     #/ t71 / z / 0.3141592653589793D1

      end function
  
 