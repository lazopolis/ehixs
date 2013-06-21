  
      subroutine bbarbbarH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH41J1  
      doubleprecision bbarbbarH41J2  
      doubleprecision bbarbbarH41J3  
      doubleprecision bbarbbarH4n1e1  
      doubleprecision bbarbbarH4n1e0  
      doubleprecision bbarbbarH4n1em1  
      doubleprecision bbarbbarH4n1em2  
      doubleprecision bbarbbarH4n1em3  
      doubleprecision bbarbbarH4n1em4  
      doubleprecision bbarbbarH4n2e1  
      doubleprecision bbarbbarH4n2e0  
      doubleprecision bbarbbarH4n2em1  
      doubleprecision bbarbbarH4n2em2  
      doubleprecision bbarbbarH4n2em3  
      doubleprecision bbarbbarH4n2em4  
      doubleprecision bbarbbarH4n3e1  
      doubleprecision bbarbbarH4n3e0  
      doubleprecision bbarbbarH4n3em1  
      doubleprecision bbarbbarH4n3em2  
      doubleprecision bbarbbarH4n3em3  
      doubleprecision bbarbbarH4n3em4  
      doubleprecision bbarbbarH4n4e1  
      doubleprecision bbarbbarH4n4e0  
      doubleprecision bbarbbarH4n4em1  
      doubleprecision bbarbbarH4n4em2  
      doubleprecision bbarbbarH4n4em3  
      doubleprecision bbarbbarH4n4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH4n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbarbbarH4n3e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbarbbarH4n4e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH4n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbarbbarH4n3e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbarbbarH4n4e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH4n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbarbbarH4n3em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbarbbarH4n4em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH4n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbarbbarH4n3em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbarbbarH4n4em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH4n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbarbbarH4n3em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbarbbarH4n4em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH4n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bbarbbarH4n3em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bbarbbarH4n4em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t6 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 ** 2
      t18 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t21 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = -0.180D3 * t26 + 0.30D2 * t28
      t31 = t4 * t30
      t48 = -0.60D2 * lh * t28 + 0.2884936567583026D3 + 0.120D3 * t26 * 
     #lh
      t49 = t4 * t48
      t50 = t49 * t18
      t52 = 0.1D1 / x1
      t55 = z * t21
      t56 = t7 * x3
      t57 = t10 * t13
      t58 = -0.1D1 + x3
      t59 = 0.1D1 / t58
      t60 = t57 * t59
      t63 = log(-0.4D1 * t56 * t60)
      t64 = t63 * z
      t67 = cos(t8)
      t69 = Sqrt(-x3 * t58)
      t74 = 0.1D1 / (-z - x3 + 0.2D1 * t67 * t69 * z)
      t78 = log(0.4D1 * t56 * t57)
      t84 = t78 ** 2
      t87 = z * t6
      t89 = t63 ** 2
      t100 = z * t18 * t74 + t18
      t103 = 0.1D1 / x3
      t107 = x2 ** 2
      t108 = x3 * t107
      t109 = t108 * t7
      t112 = log(-0.4D1 * t109 * t60)
      t117 = t108 * t14
      t119 = log(0.4D1 * t117)
      t124 = -t100
      t129 = 0.1D1 / x2
      t130 = t129 * t52
      t133 = t107 * t7
      t136 = log(0.4D1 * t133 * t57)
      t141 = t136 ** 2
      t148 = t31 * t18
      t154 = log(0.4D1 * t57)
      t155 = t154 * t4
      t158 = t154 ** 2
      t159 = t158 * t4
      t168 = t158 * t154 * t4
      t173 = t158 ** 2
      t178 = t28 ** 2
      t179 = t26 ** 2
      t192 = t18 * t4
      t195 = t21 * t4
      t198 = x3 * t10
      t201 = log(0.4D1 * t198 * t13)
      t202 = t201 ** 2
      t206 = log(-0.4D1 * t198 * t13 * t59)
      t207 = t206 ** 2
      t215 = t6 * t4
      t240 = t107 * t10
      t243 = log(0.4D1 * t240 * t13)
      t245 = t243 ** 2
      t268 = log(0.4D1 * t108 * t57)
      t272 = log(-0.4D1 * t108 * t60)
      t273 = t272 * z
      t281 = t268 ** 2
      t285 = t272 ** 2
      t299 = -(0.180D3 * t5 * (-t6 - t17 * t18 / 0.2D1 + t16 * t21) + t3
     #1 * (-t21 + t16 * t18) - 0.90D2 * t4 * (t16 * t6 + t17 * t16 * t18
     # / 0.6D1 - t17 * t21 / 0.2D1) - t50) * t52 / 0.5760D4 + (0.180D3 *
     # t5 * ((t55 - t64 * t18) * t74 + t21 - t78 * t18) - 0.90D2 * t4 * 
     #(t6 - t78 * t21 + t84 * t18 / 0.2D1 + (t87 - t64 * t21 + t89 * z *
     # t18 / 0.2D1) * t74) + t31 * t100) * t103 * t52 / 0.5760D4 - (-0.9
     #0D2 * t4 * (-(t55 - t112 * z * t18) * t74 - t21 + t119 * t18) + 0.
     #180D3 * t5 * t124) * t103 * t130 / 0.2880D4 - (0.180D3 * t5 * (-t2
     #1 + t136 * t18) - 0.90D2 * t4 * (-t6 - t141 * t18 / 0.2D1 + t136 *
     # t21) - t148) * t129 * t52 / 0.2880D4 + (-0.180D3 * t155 * lh + t3
     #1 - 0.45D2 * t159) * t6 / 0.11520D5 + (-t155 * t30 + 0.90D2 * t159
     # * lh + t49 + 0.15D2 * t168) * t21 / 0.11520D5 + (-0.15D2 / 0.4D1 
     #* t173 * t4 - t155 * t48 + t4 * (-0.5769873135166051D3 * lh - t178
     # - 0.60D2 * t179 + 0.60D2 * t26 * t28) + t159 * t30 / 0.2D1 - 0.30
     #D2 * t168 * lh) * t18 / 0.11520D5 + ((0.180D3 * t192 * lh - 0.90D2
     # * t195) * (t202 / 0.2D1 + t207 * z * t74 / 0.2D1) + (0.180D3 * t1
     #95 * lh + t148 - 0.90D2 * t215) * (-t206 * z * t74 - t201) - 0.90D
     #2 * t192 * (-t202 * t201 / 0.6D1 - t207 * t206 * z * t74 / 0.6D1) 
     #+ (t195 * t30 + 0.180D3 * t215 * lh + t50) * (z * t74 + 0.1D1)) * 
     #t103 / 0.11520D5 - (0.180D3 * t5 * (-t6 + t243 * t21 - t245 * t18 
     #/ 0.2D1) + t31 * (t243 * t18 - t21) - 0.90D2 * t4 * (t243 * t6 - t
     #245 * t21 / 0.2D1 + t245 * t243 * t18 / 0.6D1) - t50) * t129 / 0.5
     #760D4 - (0.180D3 * t5 * (-t21 + t268 * t18 - (t55 - t273 * t18) * 
     #t74) - 0.90D2 * t4 * (-t6 + t268 * t21 - t281 * t18 / 0.2D1 - (t87
     # - t273 * t21 + t285 * z * t18 / 0.2D1) * t74) + t31 * t124) * t10
     #3 * t129 / 0.5760D4
      t300 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t299)
      t302 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t299)
      t304 = t2 * x1
      t305 = -0.1D1 + x1
      t306 = x1 * z
      t307 = 0.1D1 - x1 + t306
      t308 = 0.1D1 / t307
      t310 = t2 * t305 * t308
      t311 = t1 ** 2
      t312 = s * t311
      t314 = x1 * t305 * t308
      t315 = t312 * t314
      t316 = -t305
      t317 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, t316, 0.10D1, 0.10D1,
     # x4)
      t318 = t13 * t308
      t319 = t305 ** 2
      t320 = t318 * t319
      t323 = log(0.4D1 * t11 * t320)
      t324 = t323 ** 2
      t325 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t316, 0.10D1, 0.10D1,
     # x4)
      t328 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t316, 0.10D1, 0.10D1,
     # x4)
      t349 = t56 * t10
      t352 = log(0.4D1 * t349 * t320)
      t354 = z * t328
      t356 = t318 * t319 * t59
      t359 = log(-0.4D1 * t349 * t356)
      t360 = t359 * z
      t364 = x1 * x3
      t365 = 0.2D1 * t364
      t366 = x1 * t12
      t367 = x3 * t12
      t368 = t367 * x1
      t370 = 0.2D1 * t56 * z
      t371 = t56 * t12
      t372 = t364 * z
      t373 = 0.3D1 * t372
      t374 = x3 * t307
      t376 = Sqrt(-t374 * t58)
      t380 = -t56 - z + t365 - x3 + t306 - t366 + t368 + t370 - t371 - t
     #373 + 0.2D1 * t67 * t376 * z
      t381 = 0.1D1 / t380
      t388 = t359 ** 2
      t396 = t352 ** 2
      t405 = -z * t325 * t307 * t381 - t325
      t411 = t108 * t11
      t414 = log(-0.4D1 * t411 * t356)
      t420 = t308 * t319
      t424 = log(0.4D1 * t109 * t57 * t420)
      t436 = t133 * t10
      t439 = log(0.4D1 * t436 * t320)
      t444 = t439 ** 2
      t456 = -(0.180D3 * t5 * (t317 + t324 * t325 / 0.2D1 - t323 * t328)
     # + t31 * (t328 - t323 * t325) - 0.90D2 * t4 * (-t323 * t317 - t324
     # * t323 * t325 / 0.6D1 + t324 * t328 / 0.2D1) + t49 * t325) * t52 
     #/ 0.5760D4 + (0.180D3 * t5 * (-t328 + t352 * t325 - (t354 - t360 *
     # t325) * t307 * t381) - 0.90D2 * t4 * (-(z * t317 - t360 * t328 + 
     #t388 * z * t325 / 0.2D1) * t307 * t381 - t317 + t352 * t328 - t396
     # * t325 / 0.2D1) + t31 * t405) * t103 * t52 / 0.5760D4 - (-0.90D2 
     #* t4 * (t328 + (t354 - t414 * z * t325) * t307 * t381 - t424 * t32
     #5) - 0.180D3 * t5 * t405) * t103 * t130 / 0.2880D4 - (0.180D3 * t5
     # * (t328 - t439 * t325) - 0.90D2 * t4 * (t317 + t444 * t325 / 0.2D
     #1 - t439 * t328) + t31 * t325) * t129 * t52 / 0.2880D4
      t457 = FJET(XB1, XB2, s, 0.0D0, t304, -t310, 0.0D0, -t315, t456)
      t459 = x2 * x3
      t460 = -0.1D1 + x2
      t463 = Sqrt(x3 * t460 * t58)
      t464 = t67 * t463
      t466 = 0.2D1 * t464 * x2
      t468 = 0.1D1 - x3 + t459
      t469 = 0.1D1 / t468
      t471 = t2 * (0.1D1 - x3 - x2 + t459 + t108 + t466) * t469
      t476 = t2 * x2 * (-0.1D1 + t459 + 0.2D1 * t464) * t469
      t477 = x2 * z
      t478 = -z + t477 - x2
      t479 = -t460
      t480 = t58 * t469
      t481 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t479, -t480, 
     #x4)
      t482 = t478 * t481
      t483 = t13 * t460
      t484 = t468 ** 2
      t485 = 0.1D1 / t484
      t486 = t58 * t485
      t490 = log(0.4D1 * t411 * t483 * t486)
      t492 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t479, -t480, 
     #x4)
      t496 = t459 * z
      t497 = t108 * z
      t503 = 0.1D1 / (z - t477 + x2 + x3 - t108 - t496 + t497 - t466 - 0
     #.2D1 * t464 * z + 0.2D1 * t464 * t477)
      t507 = t478 * t492 * t503
      t519 = log(0.4D1 * t108 * t13 * t10 * t460 * t486)
      t520 = t519 * t478
      t526 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t479, -t480, 
     #x4)
      t529 = t519 ** 2
      t542 = -(-0.90D2 * t4 * (t482 - t490 * t478 * t492) * t503 + 0.180
     #D3 * t5 * t507) * t103 * t130 / 0.2880D4 - (0.180D3 * t5 * (t482 -
     # t520 * t492) * t503 - 0.90D2 * t4 * (t478 * t526 - t520 * t481 + 
     #t529 * t478 * t492 / 0.2D1) * t503 + t31 * t507) * t103 * t129 / 0
     #.5760D4
      t543 = FJET(XB1, XB2, s, 0.0D0, t471, 0.0D0, -t476, 0.0D0, t542)
      t545 = t460 * s
      t546 = t1 * t305
      t548 = t545 * t546 * t308
      t549 = x2 * s
      t550 = t549 * t546
      t552 = t312 * t460 * t314
      t554 = t318 * t319 * t460
      t557 = log(-0.4D1 * t411 * t554)
      t558 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t316, t479, 0.10D1, x
     #4)
      t560 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t316, t479, 0.10D1, x
     #4)
      t571 = log(-0.4D1 * t436 * t554)
      t576 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, t316, t479, 0.10D1, x
     #4)
      t577 = t571 ** 2
      t589 = -(-0.90D2 * t4 * (t557 * t558 - t560) - 0.180D3 * t5 * t558
     #) * t103 * t130 / 0.2880D4 - (0.180D3 * t5 * (t571 * t558 - t560) 
     #- 0.90D2 * t4 * (-t576 - t577 * t558 / 0.2D1 + t571 * t560) - t31 
     #* t558) * t129 * t52 / 0.2880D4
      t590 = FJET(XB1, XB2, s, 0.0D0, t548, t304, -t550, t552, t589)
      t592 = t545 * t1
      t593 = t549 * t1
      t594 = t57 * t460
      t597 = log(-0.4D1 * t109 * t594)
      t598 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t479, 0.10D1,
     # x4)
      t600 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t479, 0.10D1,
     # x4)
      t612 = log(-0.4D1 * t133 * t594)
      t618 = t612 ** 2
      t621 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t479, 0.10D1,
     # x4)
      t625 = t31 * t598
      t632 = log(-0.4D1 * t240 * t483)
      t634 = t632 ** 2
      t658 = log(-0.4D1 * t108 * t594)
      t663 = t658 ** 2
      t674 = -(-0.90D2 * t4 * (-t597 * t598 + t600) + 0.180D3 * t5 * t59
     #8) * t103 * t130 / 0.2880D4 - (0.180D3 * t5 * (t600 - t612 * t598)
     # - 0.90D2 * t4 * (-t612 * t600 + t618 * t598 / 0.2D1 + t621) + t62
     #5) * t129 * t52 / 0.2880D4 - (0.180D3 * t5 * (t621 - t632 * t600 +
     # t634 * t598 / 0.2D1) + t31 * (-t632 * t598 + t600) - 0.90D2 * t4 
     #* (-t632 * t621 + t634 * t600 / 0.2D1 - t634 * t632 * t598 / 0.6D1
     #) + t49 * t598) * t129 / 0.5760D4 - (0.180D3 * t5 * (t600 - t658 *
     # t598) - 0.90D2 * t4 * (t663 * t598 / 0.2D1 - t658 * t600 + t621) 
     #+ t625) * t103 * t129 / 0.5760D4
      t675 = FJET(XB1, XB2, s, 0.0D0, -t592, 0.0D0, t593, 0.0D0, t674)
      t677 = FJET(XB1, XB2, s, 0.0D0, -t310, t304, 0.0D0, -t315, t456)
      t679 = FJET(XB1, XB2, s, t593, 0.0D0, -t592, 0.0D0, 0.0D0, t674)
      t682 = t304 * t459 * t469
      t683 = t2 * t305
      t684 = t108 * x1
      t685 = t460 * t58
      t687 = Sqrt(t374 * t685)
      t688 = t67 * t687
      t690 = 0.2D1 * t688 * x2
      t691 = t108 * t306
      t695 = t683 * (t108 - x2 + t459 + 0.1D1 - x3 - t684 + t690 + t691)
     # * t308 * t469
      t699 = t58 * s * t1 * x1 * t469
      t705 = t683 * x2 * (-0.1D1 + t459 + x1 - t364 - t306 + t372 + 0.2D
     #1 * t688) * t308 * t469
      t706 = x2 * x1
      t707 = t706 * z
      t708 = z - t477 - t706 + x2 + t707
      t710 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t316, t479, -t480, x4
     #)
      t716 = log(0.4D1 * t117 * t420 * t685 * t485)
      t718 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t316, t479, -t480, x4
     #)
      t719 = t708 * t718
      t736 = t496 - t497 + t108 + t477 - x2 + t691 - 0.2D1 * t688 * t477
     # - 0.2D1 * t688 * t706 - t367 * t706 - 0.2D1 * t56 * t477 + t56 * 
     #t12 * x2 + 0.2D1 * t459 * t306 + 0.2D1 * t688 * t707 + t306 - t56 
     #+ t365 - t366
      t741 = x2 * t7
      t748 = -t684 + t690 - 0.3D1 * t707 + t56 * x2 - t459 * x1 + t706 *
     # t12 + 0.2D1 * t741 * z - t741 * t12 + 0.2D1 * t688 * z - x3 + 0.2
     #D1 * t706 - t741 + t368 + t370 - t371 - t373 - z
      t750 = 0.1D1 / (t736 + t748)
      t757 = 0.90D2 * t4 * (t307 * t708 * t710 - t716 * t307 * t719) * t
     #750 - 0.180D3 * t5 * t307 * t719 * t750
      t760 = t757 * t103 * t130 / 0.2880D4
      t761 = FJET(XB1, XB2, s, t682, -t695, -t699, t705, t552, -t760)
      t764 = t103 * t129 * t52
      t767 = FJET(XB1, XB2, s, t705, -t699, -t695, t682, t552, -t760)
      t771 = FJET(XB1, XB2, s, -t550, t304, t548, 0.0D0, t552, t589)
      t773 = FJET(XB1, XB2, s, -t476, 0.0D0, t471, 0.0D0, 0.0D0, t542)
      bbarbbarH4n1e1 = t300 * t299 + t302 * t299 + t457 * t456 + t543 * 
     #t542 + t590 * t589 + t675 * t674 + t677 * t456 + t679 * t674 - t76
     #1 * t757 * t764 / 0.2880D4 - t767 * t757 * t764 / 0.2880D4 + t771 
     #* t589 + t773 * t542

      end function



      doubleprecision function bbarbbarH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t6 = z * t5
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = -0.1D1 + x3
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t20 = log(-0.4D1 * t8 * t17)
      t22 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t25 = cos(t9)
      t27 = Sqrt(-x3 * t15)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t8 * t14)
      t41 = t4 * lh
      t44 = z * t22 * t32 + t22
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t53 = -t44
      t55 = 0.1D1 / x2
      t57 = t48 * t55 * t50
      t60 = x2 ** 2
      t61 = t60 * t7
      t64 = log(0.4D1 * t61 * t14)
      t69 = t22 * t4
      t71 = 0.180D3 * t69 * lh
      t76 = t7 * t11
      t79 = log(0.4D1 * t76 * t13)
      t84 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t85 = t79 ** 2
      t92 = lh ** 2
      t94 = 0.3141592653589793D1 ** 2
      t96 = -0.180D3 * t92 + 0.30D2 * t94
      t97 = t4 * t96
      t98 = t97 * t22
      t102 = t5 * t4
      t105 = x3 * t11
      t109 = log(-0.4D1 * t105 * t13 * t16)
      t114 = log(0.4D1 * t105 * t13)
      t117 = t114 ** 2
      t118 = t109 ** 2
      t136 = t60 * x3
      t139 = log(0.4D1 * t136 * t14)
      t143 = log(-0.4D1 * t136 * t17)
      t157 = t60 * t11
      t160 = log(0.4D1 * t157 * t13)
      t166 = t160 ** 2
      t177 = log(0.4D1 * t14)
      t178 = t177 * t4
      t185 = t177 ** 2
      t186 = t185 * t4
      t206 = (-0.90D2 * t4 * ((t6 - t20 * z * t22) * t32 + t5 - t36 * t2
     #2) + 0.180D3 * t41 * t44) * t48 * t50 / 0.5760D4 + t4 * t53 * t57 
     #/ 0.32D2 - (-0.90D2 * t4 * (-t5 + t64 * t22) - t71) * t55 * t50 / 
     #0.2880D4 - (0.180D3 * t41 * (-t5 + t79 * t22) - 0.90D2 * t4 * (-t8
     #4 - t85 * t22 / 0.2D1 + t79 * t5) - t98) * t50 / 0.5760D4 + ((t71 
     #- 0.90D2 * t102) * (-t109 * z * t32 - t114) - 0.90D2 * t69 * (t117
     # / 0.2D1 + t118 * z * t32 / 0.2D1) + (0.180D3 * t102 * lh + t98 - 
     #0.90D2 * t84 * t4) * (z * t32 + 0.1D1)) * t48 / 0.11520D5 - (-0.90
     #D2 * t4 * (-t5 + t139 * t22 - (t6 - t143 * z * t22) * t32) + 0.180
     #D3 * t41 * t53) * t48 * t55 / 0.5760D4 - (0.180D3 * t41 * (t160 * 
     #t22 - t5) - 0.90D2 * t4 * (-t84 + t160 * t5 - t166 * t22 / 0.2D1) 
     #- t98) * t55 / 0.5760D4 + (0.180D3 * t41 + 0.90D2 * t178) * t84 / 
     #0.11520D5 + (-0.180D3 * t178 * lh + t97 - 0.45D2 * t186) * t5 / 0.
     #11520D5 + (-t178 * t96 + 0.90D2 * t186 * lh + t4 * (-0.60D2 * lh *
     # t94 + 0.2884936567583026D3 + 0.120D3 * t92 * lh) + 0.15D2 * t185 
     #* t177 * t4) * t22 / 0.11520D5
      t207 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t206)
      t209 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t206)
      t211 = t2 * x1
      t212 = -0.1D1 + x1
      t213 = x1 * z
      t214 = 0.1D1 - x1 + t213
      t215 = 0.1D1 / t214
      t217 = t2 * t212 * t215
      t218 = t1 ** 2
      t219 = s * t218
      t221 = x1 * t212 * t215
      t222 = t219 * t221
      t223 = -t212
      t224 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t223, 0.10D1, 0.10D1,
     # x4)
      t225 = t8 * t11
      t226 = t13 * t215
      t227 = t212 ** 2
      t228 = t226 * t227
      t231 = log(0.4D1 * t225 * t228)
      t232 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t223, 0.10D1, 0.10D1,
     # x4)
      t239 = log(-0.4D1 * t225 * t226 * t227 * t16)
      t244 = x1 * x3
      t245 = 0.2D1 * t244
      t246 = x1 * t12
      t247 = x3 * t12
      t248 = t247 * x1
      t250 = 0.2D1 * t8 * z
      t251 = t8 * t12
      t252 = t244 * z
      t253 = 0.3D1 * t252
      t254 = x3 * t214
      t256 = Sqrt(-t254 * t15)
      t260 = -t8 - z + t245 - x3 + t213 - t246 + t248 + t250 - t251 - t2
     #53 + 0.2D1 * t25 * t256 * z
      t261 = 0.1D1 / t260
      t269 = -z * t232 * t214 * t261 - t232
      t280 = t61 * t11
      t283 = log(0.4D1 * t280 * t228)
      t296 = log(0.4D1 * t76 * t228)
      t301 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, t223, 0.10D1, 0.10D1,
     # x4)
      t302 = t296 ** 2
      t313 = (-0.90D2 * t4 * (-t224 + t231 * t232 - (z * t224 - t239 * z
     # * t232) * t214 * t261) + 0.180D3 * t41 * t269) * t48 * t50 / 0.57
     #60D4 - t4 * t269 * t57 / 0.32D2 - (-0.90D2 * t4 * (t224 - t283 * t
     #232) + 0.180D3 * t41 * t232) * t55 * t50 / 0.2880D4 - (0.180D3 * t
     #41 * (t224 - t296 * t232) - 0.90D2 * t4 * (t301 + t302 * t232 / 0.
     #2D1 - t296 * t224) + t97 * t232) * t50 / 0.5760D4
      t314 = FJET(XB1, XB2, s, 0.0D0, t211, -t217, 0.0D0, -t222, t313)
      t316 = x2 * x3
      t317 = -0.1D1 + x2
      t320 = Sqrt(x3 * t317 * t15)
      t321 = t25 * t320
      t323 = 0.2D1 * t321 * x2
      t325 = 0.1D1 - x3 + t316
      t326 = 0.1D1 / t325
      t328 = t2 * (0.1D1 - x3 - x2 + t316 + t136 + t323) * t326
      t333 = t2 * x2 * (-0.1D1 + t316 + 0.2D1 * t321) * t326
      t334 = x2 * z
      t335 = -z + t334 - x2
      t337 = -t317
      t338 = t15 * t326
      t339 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t337, -t338, 
     #x4)
      t341 = t316 * z
      t342 = t136 * z
      t348 = 0.1D1 / (z - t334 + x2 + x3 - t136 - t341 + t342 - t323 - 0
     #.2D1 * t321 * z + 0.2D1 * t321 * t334)
      t350 = t55 * t50
      t354 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t337, -t338, 
     #x4)
      t358 = t325 ** 2
      t364 = log(0.4D1 * t136 * t13 * t11 * t317 * t15 / t358)
      t379 = t4 * t335 * t339 * t348 * t48 * t350 / 0.32D2 - (-0.90D2 * 
     #t4 * (t335 * t354 - t364 * t335 * t339) * t348 + 0.180D3 * t41 * t
     #335 * t339 * t348) * t48 * t55 / 0.5760D4
      t380 = FJET(XB1, XB2, s, 0.0D0, t328, 0.0D0, -t333, 0.0D0, t379)
      t382 = t317 * s
      t383 = t1 * t212
      t385 = t382 * t383 * t215
      t386 = x2 * s
      t387 = t386 * t383
      t389 = t219 * t317 * t221
      t390 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t223, t337, 0.10D1, x
     #4)
      t398 = log(-0.4D1 * t280 * t226 * t227 * t317)
      t400 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t223, t337, 0.10D1, x
     #4)
      t410 = -t4 * t390 * t57 / 0.32D2 - (-0.90D2 * t4 * (t398 * t390 - 
     #t400) - 0.180D3 * t41 * t390) * t55 * t50 / 0.2880D4
      t411 = FJET(XB1, XB2, s, 0.0D0, t385, t211, -t387, t389, t410)
      t413 = t382 * t1
      t414 = t386 * t1
      t415 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t337, 0.10D1,
     # x4)
      t419 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t337, 0.10D1,
     # x4)
      t420 = t14 * t317
      t423 = log(-0.4D1 * t61 * t420)
      t429 = 0.180D3 * t41 * t415
      t436 = log(-0.4D1 * t136 * t420)
      t448 = log(-0.4D1 * t157 * t13 * t317)
      t453 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, t337, 0.10D1,
     # x4)
      t455 = t448 ** 2
      t465 = t4 * t415 * t57 / 0.32D2 - (-0.90D2 * t4 * (t419 - t423 * t
     #415) + t429) * t55 * t50 / 0.2880D4 - (-0.90D2 * t4 * (t419 - t436
     # * t415) + t429) * t48 * t55 / 0.5760D4 - (0.180D3 * t41 * (-t448 
     #* t415 + t419) - 0.90D2 * t4 * (t453 - t448 * t419 + t455 * t415 /
     # 0.2D1) + t97 * t415) * t55 / 0.5760D4
      t466 = FJET(XB1, XB2, s, 0.0D0, -t413, 0.0D0, t414, 0.0D0, t465)
      t468 = FJET(XB1, XB2, s, 0.0D0, -t217, t211, 0.0D0, -t222, t313)
      t470 = FJET(XB1, XB2, s, t414, 0.0D0, -t413, 0.0D0, 0.0D0, t465)
      t473 = t211 * t316 * t326
      t474 = t2 * t212
      t475 = t136 * x1
      t478 = Sqrt(t254 * t317 * t15)
      t479 = t25 * t478
      t481 = 0.2D1 * t479 * x2
      t482 = t136 * t213
      t486 = t474 * (t136 - x2 + t316 + 0.1D1 - x3 - t475 + t481 + t482)
     # * t215 * t326
      t490 = t15 * s * t1 * x1 * t326
      t496 = t474 * x2 * (-0.1D1 + t316 + x1 - t244 - t213 + t252 + 0.2D
     #1 * t479) * t215 * t326
      t498 = x2 * x1
      t499 = t498 * z
      t500 = z - t334 - t498 + x2 + t499
      t501 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t223, t337, -t338, x4
     #)
      t510 = x2 * t7
      t516 = 0.2D1 * t479 * t499 - x3 - z + t341 - t342 + t248 + t250 - 
     #t251 - t253 - 0.3D1 * t499 + t8 * x2 - t316 * x1 + t498 * t12 + 0.
     #2D1 * t510 * z - t510 * t12 + 0.2D1 * t479 * z - t475
      t529 = t481 - t510 + 0.2D1 * t498 + t482 - 0.2D1 * t479 * t334 - 0
     #.2D1 * t479 * t498 - t247 * t498 - 0.2D1 * t8 * t334 + t8 * t12 * 
     #x2 + 0.2D1 * t316 * t213 + t213 - t8 + t245 - t246 + t136 + t334 -
     # x2
      t531 = 0.1D1 / (t516 + t529)
      t535 = t4 * t214 * t500 * t501 * t531 * t48 * t350 / 0.32D2
      t536 = FJET(XB1, XB2, s, t473, -t486, -t490, t496, t389, -t535)
      t538 = t214 * t500
      t541 = t501 * t531 * t57
      t544 = FJET(XB1, XB2, s, t496, -t490, -t486, t473, t389, -t535)
      t549 = FJET(XB1, XB2, s, -t387, t211, t385, 0.0D0, t389, t410)
      t551 = FJET(XB1, XB2, s, -t333, 0.0D0, t328, 0.0D0, 0.0D0, t379)
      bbarbbarH4n1e0 = t207 * t206 + t209 * t206 + t314 * t313 + t380 * 
     #t379 + t411 * t410 + t466 * t465 + t468 * t313 + t470 * t465 - t53
     #6 * t4 * t538 * t541 / 0.32D2 - t544 * t4 * t538 * t541 / 0.32D2 +
     # t549 * t410 + t551 * t379

      end function



      doubleprecision function bbarbbarH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t21 = t16 * t4
      t23 = 0.180D3 * t21 * lh
      t25 = 0.1D1 / x1
      t29 = cos(t7)
      t30 = -0.1D1 + x3
      t32 = Sqrt(-x3 * t30)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t32 * z)
      t39 = z * t16 * t37 + t16
      t41 = 0.1D1 / x3
      t42 = t41 * t25
      t45 = 0.1D1 / x2
      t46 = t45 * t25
      t51 = t41 * t45
      t54 = x2 ** 2
      t55 = t54 * t9
      t58 = log(0.4D1 * t55 * t12)
      t66 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t69 = t4 * lh
      t73 = log(0.4D1 * t12 * t9)
      t74 = t73 * t4
      t81 = lh ** 2
      t83 = 0.3141592653589793D1 ** 2
      t87 = t73 ** 2
      t93 = x3 * t9
      t98 = log(-0.4D1 * t93 * t12 / t30)
      t103 = log(0.4D1 * t93 * t12)
      t116 = -(-0.90D2 * t4 * (-t5 + t15 * t16) - t23) * t25 / 0.5760D4 
     #- t4 * t39 * t42 / 0.64D2 - t21 * t46 / 0.32D2 - t4 * t39 * t51 / 
     #0.64D2 - (-0.90D2 * t4 * (t58 * t16 - t5) - t23) * t45 / 0.5760D4 
     #- t66 * t4 / 0.128D3 + (0.180D3 * t69 + 0.90D2 * t74) * t5 / 0.115
     #20D5 + (-0.180D3 * t74 * lh + t4 * (-0.180D3 * t81 + 0.30D2 * t83)
     # - 0.45D2 * t87 * t4) * t16 / 0.11520D5 + (-0.90D2 * t21 * (-t98 *
     # z * t37 - t103) + (t23 - 0.90D2 * t5 * t4) * (z * t37 + 0.1D1)) *
     # t41 / 0.11520D5
      t117 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t116)
      t119 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t116)
      t121 = t2 * x1
      t122 = -0.1D1 + x1
      t123 = x1 * z
      t124 = 0.1D1 - x1 + t123
      t125 = 0.1D1 / t124
      t127 = t2 * t122 * t125
      t128 = t1 ** 2
      t129 = s * t128
      t131 = x1 * t122 * t125
      t132 = t129 * t131
      t133 = -t122
      t134 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t133, 0.10D1, 0.10D1,
     # x4)
      t136 = t122 ** 2
      t140 = log(0.4D1 * t10 * t12 * t125 * t136)
      t141 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t133, 0.10D1, 0.10D1,
     # x4)
      t152 = t6 * x3
      t153 = x1 * x3
      t165 = Sqrt(-x3 * t124 * t30)
      t169 = -t152 - z + 0.2D1 * t153 - x3 + t123 - x1 * t11 + x3 * t11 
     #* x1 + 0.2D1 * t152 * z - t152 * t11 - 0.3D1 * t153 * z + 0.2D1 * 
     #t29 * t165 * z
      t180 = -(-0.90D2 * t4 * (t134 - t140 * t141) + 0.180D3 * t69 * t14
     #1) * t25 / 0.5760D4 - t4 * (-z * t141 * t124 / t169 - t141) * t42 
     #/ 0.64D2 + t4 * t141 * t46 / 0.32D2
      t181 = FJET(XB1, XB2, s, 0.0D0, t121, -t127, 0.0D0, -t132, t180)
      t183 = x2 * x3
      t184 = t54 * x3
      t185 = -0.1D1 + x2
      t188 = Sqrt(x3 * t185 * t30)
      t189 = t29 * t188
      t191 = 0.2D1 * t189 * x2
      t194 = 0.1D1 / (0.1D1 - x3 + t183)
      t196 = t2 * (0.1D1 - x3 - x2 + t183 + t184 + t191) * t194
      t201 = t2 * x2 * (-0.1D1 + t183 + 0.2D1 * t189) * t194
      t202 = x2 * z
      t203 = -z + t202 - x2
      t205 = -t185
      t207 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t205, -t30 * 
     #t194, x4)
      t216 = 0.1D1 / (z - t202 + x2 + x3 - t184 - t183 * z + t184 * z - 
     #t191 - 0.2D1 * t189 * z + 0.2D1 * t189 * t202)
      t220 = t4 * t203 * t207 * t216 * t41 * t45 / 0.64D2
      t221 = FJET(XB1, XB2, s, 0.0D0, t196, 0.0D0, -t201, 0.0D0, t220)
      t225 = t207 * t216 * t51
      t228 = t185 * s
      t229 = t1 * t122
      t231 = t228 * t229 * t125
      t232 = x2 * s
      t233 = t232 * t229
      t235 = t129 * t185 * t131
      t236 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t133, t205, 0.10D1, x
     #4)
      t239 = t4 * t236 * t46 / 0.32D2
      t240 = FJET(XB1, XB2, s, 0.0D0, t231, t121, -t233, t235, -t239)
      t243 = t236 * t45 * t25
      t246 = t228 * t1
      t247 = t232 * t1
      t248 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, t205, 0.10D1,
     # x4)
      t249 = t4 * t248
      t257 = log(-0.4D1 * t55 * t12 * t185)
      t259 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, t205, 0.10D1,
     # x4)
      t268 = t249 * t46 / 0.32D2 + t249 * t51 / 0.64D2 - (-0.90D2 * t4 *
     # (-t257 * t248 + t259) + 0.180D3 * t69 * t248) * t45 / 0.5760D4
      t269 = FJET(XB1, XB2, s, 0.0D0, -t246, 0.0D0, t247, 0.0D0, t268)
      t271 = FJET(XB1, XB2, s, 0.0D0, -t127, t121, 0.0D0, -t132, t180)
      t273 = FJET(XB1, XB2, s, t247, 0.0D0, -t246, 0.0D0, 0.0D0, t268)
      t275 = FJET(XB1, XB2, s, -t233, t121, t231, 0.0D0, t235, -t239)
      t279 = FJET(XB1, XB2, s, -t201, 0.0D0, t196, 0.0D0, 0.0D0, t220)
      bbarbbarH4n1em1 = t117 * t116 + t119 * t116 + t181 * t180 + t221 *
     # t4 * t203 * t225 / 0.64D2 - t240 * t4 * t243 / 0.32D2 + t269 * t2
     #68 + t271 * t180 + t273 * t268 - t275 * t4 * t243 / 0.32D2 + t279 
     #* t4 * t203 * t225 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1
     #, x4)
      t18 = z ** 2
      t20 = x4 * 0.3141592653589793D1
      t21 = Sin(t20)
      t22 = t21 ** 2
      t25 = log(0.4D1 / t18 * t22)
      t31 = cos(t20)
      t34 = Sqrt(-x3 * (-0.1D1 + x3))
      t46 = -t6 * t7 / 0.64D2 - t6 * t10 / 0.64D2 - t13 * t5 / 0.128D3 +
     # (0.180D3 * t5 * lh + 0.90D2 * t25 * t5) * t3 / 0.11520D5 - t6 * (
     #z / (-z - x3 + 0.2D1 * t31 * t34 * z) + 0.1D1) / x3 / 0.128D3
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t46)
      t49 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t46)
      t51 = t2 * x1
      t52 = -0.1D1 + x1
      t55 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t57 = t2 * t52 * t55
      t58 = t1 ** 2
      t62 = s * t58 * x1 * t52 * t55
      t64 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, -t52, 0.10D1, 0.10D1, 
     #x4)
      t67 = t5 * t64 * t7 / 0.64D2
      t68 = FJET(XB1, XB2, s, 0.0D0, t51, -t57, 0.0D0, -t62, t67)
      t70 = t64 * t7
      t73 = -0.1D1 + x2
      t75 = t73 * s * t1
      t77 = x2 * s * t1
      t79 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, -t73, 0.10D1, 
     #x4)
      t82 = t5 * t79 * t10 / 0.64D2
      t83 = FJET(XB1, XB2, s, 0.0D0, -t75, 0.0D0, t77, 0.0D0, t82)
      t85 = t79 * t10
      t88 = FJET(XB1, XB2, s, 0.0D0, -t57, t51, 0.0D0, -t62, t67)
      t92 = FJET(XB1, XB2, s, t77, 0.0D0, -t75, 0.0D0, 0.0D0, t82)
      bbarbbarH4n1em2 = t47 * t46 + t49 * t46 + t68 * t5 * t70 / 0.64D2 
     #+ t83 * t5 * t85 / 0.64D2 + t88 * t5 * t70 / 0.64D2 + t92 * t5 * t
     #85 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t2 = s * (-0.1D1 + z)
      t3 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1,
     # x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = t3 * t5 / 0.128D3
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t7)
      bbarbbarH4n1em3 = -t8 * t3 * t5 / 0.128D3 - t11 * t3 * t5 / 0.128D
     #3

      end function



      doubleprecision function bbarbbarH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      bbarbbarH4n1em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH4n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t6 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, 
     #x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1,
     # x4)
      t19 = t16 ** 2
      t20 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1,
     # x4)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = -0.180D3 * t26 + 0.30D2 * t28
      t31 = t4 * t30
      t48 = -0.60D2 * lh * t28 + 0.2884936567583026D3 + 0.120D3 * t26 * 
     #lh
      t49 = t4 * t48
      t50 = t49 * t20
      t52 = 0.1D1 / x1
      t55 = x3 * t7
      t56 = t13 * t10
      t59 = log(0.4D1 * t55 * t56)
      t65 = t59 ** 2
      t71 = t31 * t20
      t73 = 0.1D1 / x3
      t77 = x2 * x3
      t78 = t77 * t14
      t80 = log(0.4D1 * t78)
      t89 = 0.1D1 / x2
      t90 = t89 * t52
      t93 = x2 * t7
      t96 = log(0.4D1 * t93 * t56)
      t102 = t96 ** 2
      t113 = log(0.4D1 * t56)
      t114 = t113 * t4
      t117 = t113 ** 2
      t118 = t117 * t4
      t127 = t117 * t113 * t4
      t132 = t117 ** 2
      t137 = t28 ** 2
      t138 = t26 ** 2
      t151 = x3 * t10
      t154 = log(0.4D1 * t151 * t13)
      t156 = t154 ** 2
      t177 = x2 * t10
      t180 = log(0.4D1 * t177 * t13)
      t182 = t180 ** 2
      t205 = log(0.4D1 * t77 * t56)
      t211 = t205 ** 2
      t221 = -(0.180D3 * t5 * (-t6 + t16 * t17 - t19 * t20 / 0.2D1) + t3
     #1 * (-t17 + t16 * t20) - 0.90D2 * t4 * (t16 * t6 - t19 * t17 / 0.2
     #D1 + t19 * t16 * t20 / 0.6D1) - t50) * t52 / 0.2880D4 + (0.180D3 *
     # t5 * (t17 - t59 * t20) - 0.90D2 * t4 * (t6 - t59 * t17 + t65 * t2
     #0 / 0.2D1) + t71) * t73 * t52 / 0.2880D4 - (-0.90D2 * t4 * (-t17 +
     # t80 * t20) - 0.180D3 * t5 * t20) * t73 * t90 / 0.2880D4 + (0.180D
     #3 * t5 * (t17 - t96 * t20) - 0.90D2 * t4 * (t6 - t96 * t17 + t102 
     #* t20 / 0.2D1) + t71) * t89 * t52 / 0.2880D4 + (-0.180D3 * t114 * 
     #lh + t31 - 0.45D2 * t118) * t6 / 0.5760D4 + (-t114 * t30 + 0.90D2 
     #* t118 * lh + t49 + 0.15D2 * t127) * t17 / 0.5760D4 + (-0.15D2 / 0
     #.4D1 * t132 * t4 - t114 * t48 + t4 * (-0.5769873135166051D3 * lh -
     # t137 - 0.60D2 * t138 + 0.60D2 * t26 * t28) + t118 * t30 / 0.2D1 -
     # 0.30D2 * t127 * lh) * t20 / 0.5760D4 + (0.180D3 * t5 * (t6 - t154
     # * t17 + t156 * t20 / 0.2D1) + t31 * (t17 - t154 * t20) - 0.90D2 *
     # t4 * (-t154 * t6 + t156 * t17 / 0.2D1 - t156 * t154 * t20 / 0.6D1
     #) + t50) * t73 / 0.5760D4 + (0.180D3 * t5 * (t6 - t180 * t17 + t18
     #2 * t20 / 0.2D1) + t31 * (t17 - t180 * t20) - 0.90D2 * t4 * (-t180
     # * t6 + t182 * t17 / 0.2D1 - t182 * t180 * t20 / 0.6D1) + t50) * t
     #89 / 0.5760D4 + (0.180D3 * t5 * (t17 - t205 * t20) - 0.90D2 * t4 *
     # (t6 - t205 * t17 + t211 * t20 / 0.2D1) + t71) * t73 * t89 / 0.576
     #0D4
      t222 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t221)
      t224 = t2 * x1
      t225 = -0.1D1 + x1
      t226 = t2 * t225
      t227 = -t225
      t228 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, t227, 0.0D0, 0.10D1, 
     #x4)
      t229 = x1 * z
      t230 = 0.1D1 - x1 + t229
      t231 = 0.1D1 / t230
      t232 = t13 * t231
      t233 = t225 ** 2
      t234 = t232 * t233
      t237 = log(0.4D1 * t11 * t234)
      t238 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t227, 0.0D0, 0.10D1, 
     #x4)
      t240 = t237 ** 2
      t241 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t227, 0.0D0, 0.10D1, 
     #x4)
      t262 = t55 * t10
      t265 = log(0.4D1 * t262 * t234)
      t271 = t265 ** 2
      t277 = t31 * t241
      t281 = t55 * x2
      t282 = t231 * t233
      t286 = log(0.4D1 * t281 * t56 * t282)
      t296 = t93 * t10
      t299 = log(0.4D1 * t296 * t234)
      t304 = t299 ** 2
      t315 = -(0.180D3 * t5 * (t228 - t237 * t238 + t240 * t241 / 0.2D1)
     # + t31 * (t238 - t237 * t241) - 0.90D2 * t4 * (-t237 * t228 + t240
     # * t238 / 0.2D1 - t240 * t237 * t241 / 0.6D1) + t49 * t241) * t52 
     #/ 0.2880D4 + (0.180D3 * t5 * (-t238 + t265 * t241) - 0.90D2 * t4 *
     # (-t228 + t265 * t238 - t271 * t241 / 0.2D1) - t277) * t73 * t52 /
     # 0.2880D4 - (-0.90D2 * t4 * (t238 - t286 * t241) + 0.180D3 * t5 * 
     #t241) * t73 * t90 / 0.2880D4 + (0.180D3 * t5 * (t299 * t241 - t238
     #) - 0.90D2 * t4 * (-t228 - t304 * t241 / 0.2D1 + t299 * t238) - t2
     #77) * t89 * t52 / 0.2880D4
      t316 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t224, -t226, 0.0D0, t315)
      t318 = t2 * x3
      t319 = -0.1D1 + x3
      t320 = t2 * t319
      t321 = -t319
      t322 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t321, 
     #x4)
      t326 = log(-0.4D1 * t151 * t13 * t319)
      t327 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t321, 
     #x4)
      t329 = t326 ** 2
      t330 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t321, 
     #x4)
      t352 = t56 * t319
      t355 = log(-0.4D1 * t55 * t352)
      t360 = t355 ** 2
      t367 = t31 * t330
      t374 = log(-0.4D1 * t281 * t352)
      t387 = log(-0.4D1 * t77 * t352)
      t393 = t387 ** 2
      t403 = (0.180D3 * t5 * (-t322 + t326 * t327 - t329 * t330 / 0.2D1)
     # + t31 * (-t327 + t326 * t330) - 0.90D2 * t4 * (t326 * t322 - t329
     # * t327 / 0.2D1 + t329 * t326 * t330 / 0.6D1) - t49 * t330) * t73 
     #/ 0.5760D4 + (0.180D3 * t5 * (t355 * t330 - t327) - 0.90D2 * t4 * 
     #(-t322 - t360 * t330 / 0.2D1 + t355 * t327) - t367) * t73 * t52 / 
     #0.2880D4 - (-0.90D2 * t4 * (-t374 * t330 + t327) + 0.180D3 * t5 * 
     #t330) * t73 * t90 / 0.2880D4 + (0.180D3 * t5 * (t387 * t330 - t327
     #) - 0.90D2 * t4 * (-t322 + t387 * t327 - t393 * t330 / 0.2D1) - t3
     #67) * t73 * t89 / 0.5760D4
      t404 = FJET(XB1, XB2, s, 0.0D0, t318, 0.0D0, -t320, 0.0D0, t403)
      t407 = x2 * s * t1
      t408 = -0.1D1 + x2
      t409 = t408 * s
      t410 = t409 * t1
      t411 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t412 = t408 ** 2
      t413 = t13 * t412
      t416 = log(0.4D1 * t177 * t413)
      t417 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t419 = t416 ** 2
      t420 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t442 = t56 * t412
      t445 = log(0.4D1 * t77 * t442)
      t451 = t445 ** 2
      t457 = t31 * t420
      t464 = log(0.4D1 * t281 * t442)
      t477 = log(0.4D1 * t93 * t442)
      t483 = t477 ** 2
      t493 = (0.180D3 * t5 * (-t411 + t416 * t417 - t419 * t420 / 0.2D1)
     # + t31 * (-t417 + t416 * t420) - 0.90D2 * t4 * (t416 * t411 - t419
     # * t417 / 0.2D1 + t419 * t416 * t420 / 0.6D1) - t49 * t420) * t89 
     #/ 0.5760D4 + (0.180D3 * t5 * (-t417 + t445 * t420) - 0.90D2 * t4 *
     # (t445 * t417 - t411 - t451 * t420 / 0.2D1) - t457) * t73 * t89 / 
     #0.5760D4 - (-0.90D2 * t4 * (t417 - t464 * t420) + 0.180D3 * t5 * t
     #420) * t73 * t90 / 0.2880D4 + (0.180D3 * t5 * (-t417 + t477 * t420
     #) - 0.90D2 * t4 * (-t411 + t477 * t417 - t483 * t420 / 0.2D1) - t4
     #57) * t89 * t52 / 0.2880D4
      t494 = FJET(XB1, XB2, s, 0.0D0, t407, 0.0D0, -t410, 0.0D0, t493)
      t496 = 0.3D1 * t77
      t497 = x2 ** 2
      t498 = t497 * x3
      t499 = cos(t8)
      t501 = Sqrt(-t77 * t319)
      t502 = t499 * t501
      t503 = 0.2D1 * t502
      t505 = 0.2D1 * t502 * x2
      t507 = t77 - 0.1D1
      t508 = 0.1D1 / t507
      t510 = t2 * (-x2 + t496 - t498 - x3 - t503 + t505) * t508
      t514 = t2 * t408 * (-t77 - 0.1D1 + x3 + t503) * t508
      t515 = x2 * z
      t516 = 0.1D1 - x2 + t515
      t517 = t319 * t508
      t518 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t517, x4)
      t519 = t516 * t518
      t521 = t507 ** 2
      t522 = 0.1D1 / t521
      t524 = t413 * t319 * t522
      t527 = log(-0.4D1 * t77 * t10 * t524)
      t528 = t527 * t516
      t529 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t517, x4)
      t532 = t498 * z
      t533 = t77 * z
      t534 = 0.2D1 * t77
      t538 = 0.1D1 / (t498 - t532 - t515 + t533 + x2 - t505 - t534 + 0.2
     #D1 * t502 * t515 + t503 - 0.1D1)
      t542 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t517, x4)
      t545 = t527 ** 2
      t554 = t516 * t529 * t538
      t560 = t77 * t11
      t563 = log(-0.4D1 * t560 * t524)
      t576 = (-0.180D3 * t5 * (t519 - t528 * t529) * t538 + 0.90D2 * t4 
     #* (t516 * t542 - t528 * t518 + t545 * t516 * t529 / 0.2D1) * t538 
     #- t31 * t554) * t73 * t89 / 0.5760D4 - (-0.90D2 * t4 * (t519 - t56
     #3 * t516 * t529) * t538 + 0.180D3 * t5 * t554) * t73 * t90 / 0.288
     #0D4
      t577 = FJET(XB1, XB2, s, 0.0D0, t510, 0.0D0, -t514, 0.0D0, t576)
      t581 = t2 * t225 * x2 * t231
      t582 = t1 * t225
      t583 = t409 * t582
      t584 = t1 ** 2
      t589 = s * t584 * x2 * t225 * x1 * t231
      t590 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t227, x2, 0.10D1, x4)
      t592 = t232 * t233 * t412
      t595 = log(0.4D1 * t560 * t592)
      t596 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t227, x2, 0.10D1, x4)
      t608 = log(0.4D1 * t296 * t592)
      t613 = t608 ** 2
      t616 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, t227, x2, 0.10D1, x4)
      t626 = -(-0.90D2 * t4 * (-t590 + t595 * t596) - 0.180D3 * t5 * t59
     #6) * t73 * t90 / 0.2880D4 + (0.180D3 * t5 * (t590 - t608 * t596) -
     # 0.90D2 * t4 * (t613 * t596 / 0.2D1 + t616 - t608 * t590) + t31 * 
     #t596) * t89 * t52 / 0.2880D4
      t627 = FJET(XB1, XB2, s, 0.0D0, -t581, t224, t583, -t589, t626)
      t629 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t221)
      t631 = x1 * x3
      t632 = t2 * t631
      t634 = t2 * t225 * x3
      t635 = t319 * s
      t636 = t1 * x1
      t637 = t635 * t636
      t638 = t635 * t582
      t640 = t232 * t233 * t319
      t643 = log(-0.4D1 * t262 * t640)
      t644 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t227, 0.0D0, t321, x4
     #)
      t646 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t227, 0.0D0, t321, x4
     #)
      t650 = t643 ** 2
      t653 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, t227, 0.0D0, t321, x4
     #)
      t664 = log(-0.4D1 * t560 * t640)
      t675 = (0.180D3 * t5 * (-t643 * t644 + t646) - 0.90D2 * t4 * (t650
     # * t644 / 0.2D1 + t653 - t643 * t646) + t31 * t644) * t73 * t52 / 
     #0.2880D4 - (-0.90D2 * t4 * (-t646 + t664 * t644) - 0.180D3 * t5 * 
     #t644) * t73 * t90 / 0.2880D4
      t676 = FJET(XB1, XB2, s, t632, -t634, -t637, t638, 0.0D0, t675)
      t678 = FJET(XB1, XB2, s, t583, t224, -t581, 0.0D0, -t589, t626)
      t680 = FJET(XB1, XB2, s, t638, -t637, -t634, t632, 0.0D0, t675)
      t684 = t224 * x3 * t408 * t508
      t688 = Sqrt(-x3 * t230 * x2 * t319)
      t689 = t499 * t688
      t690 = 0.2D1 * t689
      t691 = t498 * x1
      t692 = t631 * z
      t693 = t77 * x1
      t695 = t498 * t229
      t696 = t77 * t229
      t699 = 0.2D1 * t689 * x2
      t700 = -t498 - t690 + t631 + t496 - x2 - x3 + t691 - t692 - 0.2D1 
     #* t693 - t695 + 0.2D1 * t696 + t699
      t703 = t226 * t700 * t231 * t508
      t705 = t635 * t636 * t508
      t710 = t226 * t408 * (-t77 - 0.1D1 + x3 + x1 - t631 - t229 + t692 
     #+ t690) * t231 * t508
      t716 = log(-0.4D1 * t78 * t282 * t412 * t319 * t522)
      t718 = x2 * x1
      t719 = t718 * z
      t720 = x1 - t229 - t718 + t719 - t515 + x2 - 0.1D1
      t737 = 0.1D1 + 0.2D1 * t689 * t719 - t695 + 0.4D1 * t696 + t55 * t
     #12 * x2 - 0.2D1 * t55 * t515 - x3 * t12 * t718 - 0.2D1 * t689 * t7
     #18 + t7 - 0.2D1 * t689 * t229 - 0.2D1 * t689 * t515 + t534 + t691 
     #- x2 - 0.3D1 * t693 + t699
      t751 = -0.3D1 * t719 + t281 + 0.2D1 * t689 * x1 - t93 * t12 + 0.2D
     #1 * t93 * z + t718 * t12 - t690 - 0.2D1 * t7 * z + 0.2D1 * t718 - 
     #t93 + t7 * t12 - 0.2D1 * x1 + t532 - t533 - t498 + t515 + 0.2D1 * 
     #t229
      t753 = 0.1D1 / (t737 + t751)
      t755 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t227, x2, t517, x4)
      t756 = t720 * t753 * t755
      t759 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t227, x2, t517, x4)
      t768 = -0.90D2 * t4 * (t716 * t230 * t756 - t230 * t720 * t753 * t
     #759) - 0.180D3 * t5 * t230 * t756
      t771 = t768 * t73 * t90 / 0.2880D4
      t772 = FJET(XB1, XB2, s, t684, -t703, t705, t710, -t589, -t771)
      t775 = t73 * t89 * t52
      t778 = FJET(XB1, XB2, s, t710, t705, -t703, t684, -t589, -t771)
      t782 = FJET(XB1, XB2, s, -t226, t224, 0.0D0, 0.0D0, 0.0D0, t315)
      t784 = FJET(XB1, XB2, s, -t320, 0.0D0, t318, 0.0D0, 0.0D0, t403)
      t786 = FJET(XB1, XB2, s, -t410, 0.0D0, t407, 0.0D0, 0.0D0, t493)
      t788 = FJET(XB1, XB2, s, -t514, 0.0D0, t510, 0.0D0, 0.0D0, t576)
      bbarbbarH4n2e1 = t222 * t221 + t316 * t315 + t404 * t403 + t494 * 
     #t493 + t577 * t576 + t627 * t626 + t629 * t221 + t676 * t675 + t67
     #8 * t626 + t680 * t675 - t772 * t768 * t775 / 0.2880D4 - t778 * t7
     #68 * t775 / 0.2880D4 + t782 * t315 + t784 * t403 + t786 * t493 + t
     #788 * t576

      end function



      doubleprecision function bbarbbarH4n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, 
     #x4)
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t16 = log(0.4D1 * t7 * t13)
      t17 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1,
     # x4)
      t22 = t4 * lh
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x3
      t28 = 0.1D1 / x1
      t32 = 0.1D1 / x2
      t34 = t26 * t32 * t28
      t37 = x2 * t6
      t40 = log(0.4D1 * t37 * t13)
      t49 = t6 * t12
      t52 = log(0.4D1 * t49 * t9)
      t57 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1,
     # x4)
      t59 = t52 ** 2
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t69 = -0.180D3 * t65 + 0.30D2 * t67
      t70 = t4 * t69
      t71 = t70 * t17
      t75 = x3 * t12
      t78 = log(0.4D1 * t75 * t9)
      t84 = t78 ** 2
      t93 = x2 * x3
      t96 = log(0.4D1 * t93 * t13)
      t105 = x2 * t12
      t108 = log(0.4D1 * t105 * t9)
      t114 = t108 ** 2
      t125 = log(0.4D1 * t13)
      t126 = t125 * t4
      t133 = t125 ** 2
      t134 = t133 * t4
      t154 = (-0.90D2 * t4 * (t5 - t16 * t17) + t24) * t26 * t28 / 0.288
     #0D4 - t4 * t17 * t34 / 0.32D2 + (-0.90D2 * t4 * (t5 - t40 * t17) +
     # t24) * t32 * t28 / 0.2880D4 - (0.180D3 * t22 * (-t5 + t52 * t17) 
     #- 0.90D2 * t4 * (-t57 + t52 * t5 - t59 * t17 / 0.2D1) - t71) * t28
     # / 0.2880D4 + (0.180D3 * t22 * (t5 - t78 * t17) - 0.90D2 * t4 * (t
     #57 - t78 * t5 + t84 * t17 / 0.2D1) + t71) * t26 / 0.5760D4 + (-0.9
     #0D2 * t4 * (t5 - t96 * t17) + t24) * t26 * t32 / 0.5760D4 + (0.180
     #D3 * t22 * (t5 - t108 * t17) - 0.90D2 * t4 * (t57 - t108 * t5 + t1
     #14 * t17 / 0.2D1) + t71) * t32 / 0.5760D4 + (0.180D3 * t22 + 0.90D
     #2 * t126) * t57 / 0.5760D4 + (-0.180D3 * t126 * lh + t70 - 0.45D2 
     #* t134) * t5 / 0.5760D4 + (-t126 * t69 + 0.90D2 * t134 * lh + t4 *
     # (-0.60D2 * lh * t67 + 0.2884936567583026D3 + 0.120D3 * t65 * lh) 
     #+ 0.15D2 * t133 * t125 * t4) * t17 / 0.5760D4
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t154)
      t157 = t2 * x1
      t158 = -0.1D1 + x1
      t159 = t2 * t158
      t160 = -t158
      t161 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t160, 0.0D0, 0.10D1, 
     #x4)
      t162 = t7 * t12
      t163 = x1 * z
      t164 = 0.1D1 - x1 + t163
      t165 = 0.1D1 / t164
      t166 = t9 * t165
      t167 = t158 ** 2
      t168 = t166 * t167
      t171 = log(0.4D1 * t162 * t168)
      t172 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t160, 0.0D0, 0.10D1, 
     #x4)
      t178 = 0.180D3 * t22 * t172
      t186 = t37 * t12
      t189 = log(0.4D1 * t186 * t168)
      t200 = log(0.4D1 * t49 * t168)
      t205 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, t160, 0.0D0, 0.10D1, 
     #x4)
      t207 = t200 ** 2
      t217 = (-0.90D2 * t4 * (-t161 + t171 * t172) - t178) * t26 * t28 /
     # 0.2880D4 + t4 * t172 * t34 / 0.32D2 + (-0.90D2 * t4 * (t189 * t17
     #2 - t161) - t178) * t32 * t28 / 0.2880D4 - (0.180D3 * t22 * (t161 
     #- t200 * t172) - 0.90D2 * t4 * (t205 - t200 * t161 + t207 * t172 /
     # 0.2D1) + t70 * t172) * t28 / 0.2880D4
      t218 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t157, -t159, 0.0D0, t217)
      t220 = t2 * x3
      t221 = -0.1D1 + x3
      t222 = t2 * t221
      t223 = t13 * t221
      t226 = log(-0.4D1 * t7 * t223)
      t227 = -t221
      t228 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t227, 
     #x4)
      t230 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t227, 
     #x4)
      t235 = 0.180D3 * t22 * t228
      t246 = log(-0.4D1 * t75 * t9 * t221)
      t251 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t227, 
     #x4)
      t253 = t246 ** 2
      t265 = log(-0.4D1 * t93 * t223)
      t274 = (-0.90D2 * t4 * (t226 * t228 - t230) - t235) * t26 * t28 / 
     #0.2880D4 + t4 * t228 * t34 / 0.32D2 + (0.180D3 * t22 * (-t230 + t2
     #46 * t228) - 0.90D2 * t4 * (-t251 + t246 * t230 - t253 * t228 / 0.
     #2D1) - t70 * t228) * t26 / 0.5760D4 + (-0.90D2 * t4 * (t265 * t228
     # - t230) - t235) * t26 * t32 / 0.5760D4
      t275 = FJET(XB1, XB2, s, 0.0D0, t220, 0.0D0, -t222, 0.0D0, t274)
      t278 = x2 * s * t1
      t279 = -0.1D1 + x2
      t280 = t279 * s
      t281 = t280 * t1
      t282 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t283 = t279 ** 2
      t284 = t13 * t283
      t287 = log(0.4D1 * t93 * t284)
      t288 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t294 = 0.180D3 * t22 * t288
      t299 = t9 * t283
      t302 = log(0.4D1 * t105 * t299)
      t307 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t309 = t302 ** 2
      t324 = log(0.4D1 * t37 * t284)
      t333 = (-0.90D2 * t4 * (-t282 + t287 * t288) - t294) * t26 * t32 /
     # 0.5760D4 + (0.180D3 * t22 * (-t282 + t302 * t288) - 0.90D2 * t4 *
     # (-t307 + t302 * t282 - t309 * t288 / 0.2D1) - t70 * t288) * t32 /
     # 0.5760D4 + t4 * t288 * t34 / 0.32D2 + (-0.90D2 * t4 * (-t282 + t3
     #24 * t288) - t294) * t32 * t28 / 0.2880D4
      t334 = FJET(XB1, XB2, s, 0.0D0, t278, 0.0D0, -t281, 0.0D0, t333)
      t336 = 0.3D1 * t93
      t337 = x2 ** 2
      t338 = t337 * x3
      t339 = cos(t10)
      t341 = Sqrt(-t93 * t221)
      t342 = t339 * t341
      t343 = 0.2D1 * t342
      t345 = 0.2D1 * t342 * x2
      t347 = t93 - 0.1D1
      t348 = 0.1D1 / t347
      t350 = t2 * (-x2 + t336 - t338 - x3 - t343 + t345) * t348
      t354 = t2 * t279 * (-t93 - 0.1D1 + x3 + t343) * t348
      t355 = x2 * z
      t356 = 0.1D1 - x2 + t355
      t358 = t221 * t348
      t359 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t358, x4)
      t361 = t338 * z
      t362 = t93 * z
      t363 = 0.2D1 * t93
      t367 = 0.1D1 / (t338 - t361 - t355 + t362 + x2 - t345 - t363 + 0.2
     #D1 * t342 * t355 + t343 - 0.1D1)
      t369 = t32 * t28
      t373 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t358, x4)
      t376 = t347 ** 2
      t382 = log(-0.4D1 * t93 * t12 * t299 * t221 / t376)
      t397 = t4 * t356 * t359 * t367 * t26 * t369 / 0.32D2 + (0.90D2 * t
     #4 * (t356 * t373 - t382 * t356 * t359) * t367 - 0.180D3 * t22 * t3
     #56 * t359 * t367) * t26 * t32 / 0.5760D4
      t398 = FJET(XB1, XB2, s, 0.0D0, t350, 0.0D0, -t354, 0.0D0, t397)
      t402 = t2 * t158 * x2 * t165
      t403 = t1 * t158
      t404 = t280 * t403
      t405 = t1 ** 2
      t410 = s * t405 * x2 * t158 * x1 * t165
      t411 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t160, x2, 0.10D1, x4)
      t415 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t160, x2, 0.10D1, x4)
      t420 = log(0.4D1 * t186 * t166 * t167 * t283)
      t431 = -t4 * t411 * t34 / 0.32D2 + (-0.90D2 * t4 * (t415 - t420 * 
     #t411) + 0.180D3 * t22 * t411) * t32 * t28 / 0.2880D4
      t432 = FJET(XB1, XB2, s, 0.0D0, -t402, t157, t404, -t410, t431)
      t434 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t154)
      t436 = x1 * x3
      t437 = t2 * t436
      t439 = t2 * t158 * x3
      t440 = t221 * s
      t441 = t1 * x1
      t442 = t440 * t441
      t443 = t440 * t403
      t448 = log(-0.4D1 * t162 * t166 * t167 * t221)
      t449 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t160, 0.0D0, t227, x4
     #)
      t451 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t160, 0.0D0, t227, x4
     #)
      t464 = (-0.90D2 * t4 * (-t448 * t449 + t451) + 0.180D3 * t22 * t44
     #9) * t26 * t28 / 0.2880D4 - t4 * t449 * t34 / 0.32D2
      t465 = FJET(XB1, XB2, s, t437, -t439, -t442, t443, 0.0D0, t464)
      t467 = FJET(XB1, XB2, s, t404, t157, -t402, 0.0D0, -t410, t431)
      t469 = FJET(XB1, XB2, s, t443, -t442, -t439, t437, 0.0D0, t464)
      t473 = t157 * x3 * t279 * t348
      t477 = Sqrt(-x3 * t164 * x2 * t221)
      t478 = t339 * t477
      t479 = 0.2D1 * t478
      t480 = t338 * x1
      t481 = t436 * z
      t482 = t93 * x1
      t484 = t338 * t163
      t485 = t93 * t163
      t488 = 0.2D1 * t478 * x2
      t489 = -t338 - t479 + t436 + t336 - x2 - x3 + t480 - t481 - 0.2D1 
     #* t482 - t484 + 0.2D1 * t485 + t488
      t492 = t159 * t489 * t165 * t348
      t494 = t440 * t441 * t348
      t499 = t159 * t279 * (-t93 - 0.1D1 + x3 + x1 - t436 - t163 + t481 
     #+ t479) * t165 * t348
      t501 = x2 * x1
      t502 = t501 * z
      t503 = x1 - t163 - t501 + t502 - t355 + x2 - 0.1D1
      t522 = 0.1D1 - x2 - 0.2D1 * x1 - 0.2D1 * t478 * t501 - x3 * t8 * t
     #501 - 0.2D1 * t7 * t355 + t7 * t8 * x2 - t484 + 0.4D1 * t485 - 0.2
     #D1 * t478 * t355 - 0.2D1 * t478 * t163 + t361 + 0.2D1 * t501 - 0.2
     #D1 * t6 * z + t6 * t8 - t37
      t535 = -t479 + 0.2D1 * t478 * x1 + t480 - 0.3D1 * t482 + t488 - 0.
     #3D1 * t502 + x2 * t7 + t501 * t8 + 0.2D1 * t37 * z - t37 * t8 + 0.
     #2D1 * t478 * t502 + t355 + t363 - t338 - t362 + 0.2D1 * t163 + t6
      t537 = 0.1D1 / (t522 + t535)
      t540 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t160, x2, t358, x4)
      t544 = t4 * t164 * t503 * t537 * t540 * t26 * t369 / 0.32D2
      t545 = FJET(XB1, XB2, s, t473, -t492, t494, t499, -t410, -t544)
      t547 = t164 * t503
      t550 = t537 * t540 * t34
      t553 = FJET(XB1, XB2, s, t499, t494, -t492, t473, -t410, -t544)
      t558 = FJET(XB1, XB2, s, -t159, t157, 0.0D0, 0.0D0, 0.0D0, t217)
      t560 = FJET(XB1, XB2, s, -t222, 0.0D0, t220, 0.0D0, 0.0D0, t274)
      t562 = FJET(XB1, XB2, s, -t281, 0.0D0, t278, 0.0D0, 0.0D0, t333)
      t564 = FJET(XB1, XB2, s, -t354, 0.0D0, t350, 0.0D0, 0.0D0, t397)
      bbarbbarH4n2e0 = t155 * t154 + t218 * t217 + t275 * t274 + t334 * 
     #t333 + t398 * t397 + t432 * t431 + t434 * t154 + t465 * t464 + t46
     #7 * t431 + t469 * t464 - t545 * t4 * t547 * t550 / 0.32D2 - t553 *
     # t4 * t547 * t550 / 0.32D2 + t558 * t217 + t560 * t274 + t562 * t3
     #33 + t564 * t397

      end function



      doubleprecision function bbarbbarH4n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, 
     #x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1,
     # x4)
      t21 = t4 * lh
      t23 = 0.180D3 * t21 * t16
      t25 = 0.1D1 / x1
      t28 = t4 * t16
      t29 = 0.1D1 / x3
      t30 = t29 * t25
      t33 = 0.1D1 / x2
      t34 = t33 * t25
      t37 = t29 * t33
      t40 = x2 * t9
      t43 = log(0.4D1 * t40 * t12)
      t51 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1,
     # x4)
      t57 = log(0.4D1 * t9 * t12)
      t58 = t57 * t4
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t71 = t57 ** 2
      t77 = x3 * t9
      t80 = log(0.4D1 * t77 * t12)
      t88 = -(-0.90D2 * t4 * (-t5 + t15 * t16) - t23) * t25 / 0.2880D4 -
     # t28 * t30 / 0.32D2 - t28 * t34 / 0.32D2 - t28 * t37 / 0.64D2 + (-
     #0.90D2 * t4 * (t5 - t43 * t16) + t23) * t33 / 0.5760D4 - t4 * t51 
     #/ 0.64D2 + (0.180D3 * t21 + 0.90D2 * t58) * t5 / 0.5760D4 + (-0.18
     #0D3 * t58 * lh + t4 * (-0.180D3 * t65 + 0.30D2 * t67) - 0.45D2 * t
     #71 * t4) * t16 / 0.5760D4 + (-0.90D2 * t4 * (t5 - t80 * t16) + t23
     #) * t29 / 0.5760D4
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t88)
      t91 = t2 * x1
      t92 = -0.1D1 + x1
      t93 = t2 * t92
      t94 = -t92
      t95 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, t94, 0.0D0, 0.10D1, x4
     #)
      t98 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t100 = t92 ** 2
      t104 = log(0.4D1 * t10 * t12 * t98 * t100)
      t105 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t94, 0.0D0, 0.10D1, x
     #4)
      t115 = t4 * t105
      t120 = -(-0.90D2 * t4 * (t95 - t104 * t105) + 0.180D3 * t21 * t105
     #) * t25 / 0.2880D4 + t115 * t30 / 0.32D2 + t115 * t34 / 0.32D2
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t91, -t93, 0.0D0, t120)
      t123 = t2 * x3
      t124 = -0.1D1 + x3
      t125 = t2 * t124
      t126 = -t124
      t127 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t126, 
     #x4)
      t128 = t4 * t127
      t133 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t126, 
     #x4)
      t137 = log(-0.4D1 * t77 * t12 * t124)
      t147 = t128 * t37 / 0.64D2 + t128 * t30 / 0.32D2 + (-0.90D2 * t4 *
     # (-t133 + t137 * t127) - 0.180D3 * t21 * t127) * t29 / 0.5760D4
      t148 = FJET(XB1, XB2, s, 0.0D0, t123, 0.0D0, -t125, 0.0D0, t147)
      t151 = x2 * s * t1
      t152 = -0.1D1 + x2
      t153 = t152 * s
      t154 = t153 * t1
      t155 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t156 = t4 * t155
      t159 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x
     #4)
      t160 = t152 ** 2
      t164 = log(0.4D1 * t40 * t12 * t160)
      t176 = t156 * t37 / 0.64D2 + (-0.90D2 * t4 * (-t159 + t164 * t155)
     # - 0.180D3 * t21 * t155) * t33 / 0.5760D4 + t156 * t34 / 0.32D2
      t177 = FJET(XB1, XB2, s, 0.0D0, t151, 0.0D0, -t154, 0.0D0, t176)
      t179 = x2 * x3
      t181 = x2 ** 2
      t182 = t181 * x3
      t183 = cos(t7)
      t185 = Sqrt(-t179 * t124)
      t186 = t183 * t185
      t187 = 0.2D1 * t186
      t189 = 0.2D1 * t186 * x2
      t192 = 0.1D1 / (t179 - 0.1D1)
      t194 = t2 * (-x2 + 0.3D1 * t179 - t182 - x3 - t187 + t189) * t192
      t198 = t2 * t152 * (-t179 - 0.1D1 + x3 + t187) * t192
      t199 = x2 * z
      t200 = 0.1D1 - x2 + t199
      t203 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t124 * t1
     #92, x4)
      t211 = 0.1D1 / (t182 - t182 * z - t199 + t179 * z + x2 - t189 - 0.
     #2D1 * t179 + 0.2D1 * t186 * t199 + t187 - 0.1D1)
      t215 = t4 * t200 * t203 * t211 * t29 * t33 / 0.64D2
      t216 = FJET(XB1, XB2, s, 0.0D0, t194, 0.0D0, -t198, 0.0D0, t215)
      t220 = t203 * t211 * t37
      t225 = t2 * t92 * x2 * t98
      t226 = t1 * t92
      t227 = t153 * t226
      t228 = t1 ** 2
      t233 = s * t228 * x2 * t92 * x1 * t98
      t234 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t94, x2, 0.10D1, x4)
      t237 = t4 * t234 * t34 / 0.32D2
      t238 = FJET(XB1, XB2, s, 0.0D0, -t225, t91, t227, -t233, -t237)
      t241 = t234 * t33 * t25
      t244 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t88)
      t247 = t2 * x1 * x3
      t249 = t2 * t92 * x3
      t250 = t124 * s
      t252 = t250 * t1 * x1
      t253 = t250 * t226
      t254 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, t94, 0.0D0, t126, x4)
      t257 = t4 * t254 * t30 / 0.32D2
      t258 = FJET(XB1, XB2, s, t247, -t249, -t252, t253, 0.0D0, -t257)
      t261 = t254 * t29 * t25
      t264 = FJET(XB1, XB2, s, t227, t91, -t225, 0.0D0, -t233, -t237)
      t268 = FJET(XB1, XB2, s, t253, -t252, -t249, t247, 0.0D0, -t257)
      t272 = FJET(XB1, XB2, s, -t93, t91, 0.0D0, 0.0D0, 0.0D0, t120)
      t274 = FJET(XB1, XB2, s, -t125, 0.0D0, t123, 0.0D0, 0.0D0, t147)
      t276 = FJET(XB1, XB2, s, -t154, 0.0D0, t151, 0.0D0, 0.0D0, t176)
      t278 = FJET(XB1, XB2, s, -t198, 0.0D0, t194, 0.0D0, 0.0D0, t215)
      bbarbbarH4n2em1 = t89 * t88 + t121 * t120 + t148 * t147 + t177 * t
     #176 + t216 * t4 * t200 * t220 / 0.64D2 - t238 * t4 * t241 / 0.32D2
     # + t244 * t88 - t258 * t4 * t261 / 0.32D2 - t264 * t4 * t241 / 0.3
     #2D2 - t268 * t4 * t261 / 0.32D2 + t272 * t120 + t274 * t147 + t276
     # * t176 + t278 * t4 * t200 * t220 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, 
     #x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1,
     # x4)
      t18 = z ** 2
      t21 = Sin(x4 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t25 = log(0.4D1 / t18 * t22)
      t31 = 0.1D1 / x3
      t34 = -t6 * t7 / 0.32D2 - t6 * t10 / 0.64D2 - t4 * t13 / 0.64D2 + 
     #(0.180D3 * t4 * lh + 0.90D2 * t25 * t4) * t5 / 0.5760D4 - t6 * t31
     # / 0.64D2
      t35 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t34)
      t37 = t2 * x1
      t38 = -0.1D1 + x1
      t39 = t2 * t38
      t41 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, -t38, 0.0D0, 0.10D1, x
     #4)
      t44 = t4 * t41 * t7 / 0.32D2
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t37, -t39, 0.0D0, t44)
      t47 = t41 * t7
      t50 = t2 * x3
      t51 = -0.1D1 + x3
      t52 = t2 * t51
      t54 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, -t51, x
     #4)
      t57 = t4 * t54 * t31 / 0.64D2
      t58 = FJET(XB1, XB2, s, 0.0D0, t50, 0.0D0, -t52, 0.0D0, t57)
      t60 = t54 * t31
      t64 = x2 * s * t1
      t67 = (-0.1D1 + x2) * s * t1
      t68 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4
     #)
      t71 = t4 * t68 * t10 / 0.64D2
      t72 = FJET(XB1, XB2, s, 0.0D0, t64, 0.0D0, -t67, 0.0D0, t71)
      t74 = t68 * t10
      t77 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t34)
      t79 = FJET(XB1, XB2, s, -t39, t37, 0.0D0, 0.0D0, 0.0D0, t44)
      t83 = FJET(XB1, XB2, s, -t52, 0.0D0, t50, 0.0D0, 0.0D0, t57)
      t87 = FJET(XB1, XB2, s, -t67, 0.0D0, t64, 0.0D0, 0.0D0, t71)
      bbarbbarH4n2em2 = t35 * t34 + t45 * t4 * t47 / 0.32D2 + t58 * t4 *
     # t60 / 0.64D2 + t72 * t4 * t74 / 0.64D2 + t77 * t34 + t79 * t4 * t
     #47 / 0.32D2 + t83 * t4 * t60 / 0.64D2 + t87 * t4 * t74 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, 
     #x4)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t7)
      bbarbbarH4n2em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      bbarbbarH4n2em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH4n3e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = 0.1D1 / t3 / z
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = log(0.4D1 * t9)
      t12 = t11 ** 2
      t13 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t16 = t12 * t11
      t17 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t20 = t12 ** 2
      t21 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t25 = s ** 2
      t26 = 0.1D1 / t25
      t32 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t38 = -0.60D2 * lh * t32 + 0.2884936567583026D3 + 0.120D3 * t35 * 
     #lh
      t41 = t21 * t26
      t43 = t32 ** 2
      t44 = t35 ** 2
      t56 = 0.180D3 * t35
      t57 = 0.30D2 * t32
      t58 = -t56 + t57
      t72 = t17 * t26
      t75 = x3 * t5
      t78 = log(0.4D1 * t75 * t8)
      t79 = t78 ** 2
      t80 = -0.1D1 + x3
      t81 = 0.1D1 / t80
      t85 = log(-0.4D1 * t75 * t8 * t81)
      t86 = t85 ** 2
      t88 = cos(t6)
      t89 = x3 * z
      t91 = Sqrt(-t89 * t80)
      t95 = 0.1D1 / (-z - x3 + 0.2D1 * t88 * t91)
      t103 = t13 * t26
      t127 = 0.1D1 / x3
      t130 = t26 * lh
      t131 = x1 ** 2
      t132 = t131 * t8
      t133 = t132 * t5
      t135 = log(0.4D1 * t133)
      t137 = t135 ** 2
      t143 = -t58
      t144 = t26 * t143
      t158 = -t26 * t38
      t161 = 0.1D1 / x1
      t165 = t131 * x3
      t166 = t9 * t81
      t169 = log(-0.4D1 * t165 * t166)
      t170 = t169 * z
      t176 = log(0.4D1 * t165 * t9)
      t181 = z * t13
      t183 = t169 ** 2
      t190 = t176 ** 2
      t197 = t21 * z * t95
      t204 = 0.1D1 - x2
      t205 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t204, 0.10D1, 
     #x4)
      t206 = x2 ** 2
      t207 = x3 * t206
      t208 = t207 * t131
      t211 = log(-0.4D1 * t208 * t166)
      t216 = -t204
      t217 = t9 * t216
      t220 = log(-0.4D1 * t208 * t217)
      t221 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t204, 0.10D1, 
     #x4)
      t225 = log(0.4D1 * t207 * t133)
      t235 = 0.1D1 / x2
      t236 = t235 * t161
      t239 = t206 * t131
      t242 = log(0.4D1 * t239 * t9)
      t243 = t242 ** 2
      t250 = t21 * t58
      t251 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t204, 0.10D1, 
     #x4)
      t252 = 0.90D2 * t251
      t253 = 0.90D2 * t13
      t254 = 0.180D3 * lh
      t257 = log(-0.4D1 * t239 * t217)
      t265 = t257 ** 2
      t273 = t5 * t206
      t274 = t8 * t216
      t277 = log(-0.4D1 * t273 * t274)
      t279 = t277 ** 2
      t284 = log(0.4D1 * t273 * t8)
      t286 = t284 ** 2
      t320 = log(-0.4D1 * t207 * t217)
      t326 = t320 ** 2
      t332 = log(0.4D1 * t207 * t9)
      t340 = log(-0.4D1 * t207 * t166)
      t347 = t340 ** 2
      t356 = t332 ** 2
      t361 = t127 * t235
      t364 = -(t12 * t13 / 0.2D1 - t16 * t17 / 0.6D1 + t20 * t21 / 0.24D
     #2) * t26 / 0.128D3 + (t17 - t11 * t21) * t26 * t38 / 0.11520D5 + t
     #41 * (-0.5769873135166051D3 * lh - t43 - 0.60D2 * t44 + 0.60D2 * t
     #35 * t32) / 0.11520D5 + (t13 - t11 * t17 + t12 * t21 / 0.2D1) * t2
     #6 * t58 / 0.11520D5 + (-t11 * t13 + t12 * t17 / 0.2D1 - t16 * t21 
     #/ 0.6D1) * t26 * lh / 0.64D2 - ((0.180D3 * t41 * lh - 0.90D2 * t72
     #) * (-t79 / 0.2D1 - t86 * z * t95 / 0.2D1) + (0.180D3 * t72 * lh +
     # t41 * t58 - 0.90D2 * t103) * (t85 * z * t95 + t78) - 0.90D2 * t41
     # * (t79 * t78 / 0.6D1 + t86 * t85 * z * t95 / 0.6D1) + (t72 * t58 
     #+ 0.180D3 * t103 * lh + t41 * t38) * (-z * t95 - 0.1D1)) * t127 / 
     #0.11520D5 - (-0.180D3 * t130 * (t13 - t135 * t17 + t137 * t21 / 0.
     #2D1) + t144 * (-t135 * t21 + t17) + 0.90D2 * t26 * (-t135 * t13 + 
     #t137 * t17 / 0.2D1 - t137 * t135 * t21 / 0.6D1) + t158 * t21) * t1
     #61 / 0.5760D4 + (-0.180D3 * t130 * (-t17 - (z * t17 - t170 * t21) 
     #* t95 + t176 * t21) + 0.90D2 * t26 * (-(t181 - t170 * t17 + t183 *
     # z * t21 / 0.2D1) * t95 - t13 + t176 * t17 - t190 * t21 / 0.2D1) +
     # t144 * (-t21 - t197)) * t127 * t161 / 0.5760D4 - (-0.90D2 * t26 *
     # (-t17 + t205 - (t17 - t211 * t21) * z * t95 - t220 * t221 + t225 
     #* t21) + 0.180D3 * t130 * (t221 - t21 - t197)) * t127 * t236 / 0.2
     #880D4 - t26 * (0.45D2 * t243 * t21 - 0.180D3 * (t17 - t242 * t21) 
     #* lh - t250 - t252 + t253 + (t254 + 0.90D2 * t257) * t205 - 0.90D2
     # * t242 * t17 + (-0.180D3 * t257 * lh - t56 + t57 - 0.45D2 * t265)
     # * t221) * t236 / 0.2880D4 + (0.180D3 * t130 * (-t251 + t277 * t20
     #5 - t279 * t221 / 0.2D1 + t13 - t284 * t17 + t286 * t21 / 0.2D1) +
     # t26 * t58 * (t17 - t284 * t21 - t205 + t277 * t221) - 0.90D2 * t2
     #6 * (t277 * t251 - t279 * t205 / 0.2D1 + t279 * t277 * t221 / 0.6D
     #1 - t284 * t13 + t286 * t17 / 0.2D1 - t286 * t284 * t21 / 0.6D1) +
     # t26 * t38 * (t21 - t221)) * t235 / 0.5760D4 - t26 * (-(-t254 - 0.
     #90D2 * t320) * t205 - (0.180D3 * t320 * lh + t56 - t57 + 0.45D2 * 
     #t326) * t221 - 0.180D3 * (t17 - t332 * t21) * lh - t250 + (0.90D2 
     #* t181 + (-t254 - 0.90D2 * t340) * z * t17 + (0.180D3 * t340 * lh 
     #+ t56 - t57 + 0.45D2 * t347) * z * t21) * t95 + t253 - 0.90D2 * t3
     #32 * t17 - t252 + 0.45D2 * t356 * t21) * t361 / 0.5760D4
      t365 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t364)
      t367 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t364)
      t370 = t1 * x1
      t371 = x1 * z
      t372 = -z - x1 + t371
      t373 = 0.1D1 / t372
      t375 = t216 * s * t370 * t373
      t376 = -0.1D1 + x1
      t377 = t2 * t376
      t379 = x2 * s * t370
      t380 = t1 ** 2
      t381 = s * t380
      t384 = x1 * t376 * t373
      t385 = t381 * t216 * t384
      t386 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, t204, 0.10D1, x4)
      t387 = t207 * t132
      t388 = 0.1D1 / t3
      t389 = t388 * t373
      t390 = t376 ** 2
      t392 = t389 * t390 * t216
      t395 = log(0.4D1 * t387 * t392)
      t396 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, t204, 0.10D1, x4)
      t406 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, x1, t204, 0.10D1, x4)
      t408 = t239 * t8
      t411 = log(0.4D1 * t408 * t392)
      t417 = t411 ** 2
      t425 = -(-0.90D2 * t26 * (-t386 + t395 * t396) - 0.180D3 * t130 * 
     #t396) * t127 * t236 / 0.2880D4 - t26 * (0.90D2 * t406 - (t254 + 0.
     #90D2 * t411) * t386 - (-0.180D3 * t411 * lh - t56 + t57 - 0.45D2 *
     # t417) * t396) * t236 / 0.2880D4
      t426 = FJET(XB1, XB2, s, 0.0D0, t375, -t377, t379, -t385, t425)
      t429 = t2 * x1 * t373
      t430 = t381 * t384
      t431 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t432 = t389 * t390
      t435 = log(-0.4D1 * t132 * t432)
      t436 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t438 = t435 ** 2
      t439 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t462 = t165 * t8
      t464 = t389 * t390 * t81
      t467 = log(0.4D1 * t462 * t464)
      t468 = t467 * z
      t472 = x1 * x3
      t473 = t472 * z
      t475 = 0.2D1 * t165 * z
      t476 = x1 * t3
      t477 = x3 * t3
      t478 = t477 * x1
      t479 = t165 * t3
      t480 = x3 * t372
      t482 = Sqrt(t480 * t80)
      t487 = 0.1D1 / (-t371 - t473 - t89 - t165 + t475 + t476 + t478 - t
     #479 + 0.2D1 * t88 * t482 * z - t3)
      t491 = log(-0.4D1 * t462 * t432)
      t499 = t467 ** 2
      t506 = t491 ** 2
      t512 = z * t372
      t515 = t439 - t512 * t439 * t487
      t522 = t373 * t390
      t526 = log(-0.4D1 * t208 * t8 * t388 * t522)
      t531 = log(0.4D1 * t387 * t464)
      t548 = log(-0.4D1 * t408 * t432)
      t551 = t548 ** 2
      t563 = -(-0.180D3 * t130 * (-t431 + t435 * t436 - t438 * t439 / 0.
     #2D1) + t144 * (t435 * t439 - t436) + 0.90D2 * t26 * (t435 * t431 -
     # t438 * t436 / 0.2D1 + t438 * t435 * t439 / 0.6D1) - t158 * t439) 
     #* t161 / 0.5760D4 + (-0.180D3 * t130 * (-(z * t436 - t468 * t439) 
     #* t372 * t487 - t491 * t439 + t436) + 0.90D2 * t26 * (-t491 * t436
     # + t431 - (z * t431 - t468 * t436 + t499 * z * t439 / 0.2D1) * t37
     #2 * t487 + t506 * t439 / 0.2D1) + t144 * t515) * t127 * t161 / 0.5
     #760D4 - (-0.90D2 * t26 * (-t526 * t439 - (t512 * t436 - t531 * z *
     # t372 * t439) * t487 + t436) + 0.180D3 * t130 * t515) * t127 * t23
     #6 / 0.2880D4 - t26 * (-(0.180D3 * t548 * lh + t56 - t57 + 0.45D2 *
     # t551) * t439 - (-t254 - 0.90D2 * t548) * t436 - 0.90D2 * t431) * 
     #t236 / 0.2880D4
      t564 = FJET(XB1, XB2, s, 0.0D0, -t377, -t429, 0.0D0, t430, t563)
      t566 = FJET(XB1, XB2, s, 0.0D0, -t429, -t377, 0.0D0, t430, t563)
      t568 = x2 * x3
      t569 = 0.1D1 - x3 + t568
      t570 = 0.1D1 / t569
      t571 = t80 * t570
      t572 = t2 * t571
      t573 = t568 * t570
      t574 = t2 * t573
      t575 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t204, -t571, x
     #4)
      t578 = t569 ** 2
      t579 = 0.1D1 / t578
      t580 = t80 * t579
      t584 = log(0.4D1 * t387 * t5 * t216 * t580)
      t586 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t204, -t571, x
     #4)
      t590 = t216 * t80
      t592 = Sqrt(t89 * t590)
      t596 = 0.1D1 / (-z - x3 + t568 + 0.2D1 * t88 * t592)
      t607 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t204, -t571, x
     #4)
      t616 = log(0.4D1 * t207 * t5 * t274 * t580)
      t617 = t616 * z
      t624 = t616 ** 2
      t635 = -(-0.90D2 * t26 * (z * t575 - t584 * z * t586) * t596 + 0.1
     #80D3 * t130 * z * t586 * t596) * t127 * t236 / 0.2880D4 + t26 * (0
     #.90D2 * z * t607 + (-0.180D3 * z * lh - 0.90D2 * t617) * t575 + (0
     #.180D3 * t617 * lh + z * t143 + 0.45D2 * t624 * z) * t586) * t596 
     #* t127 * t235 / 0.5760D4
      t636 = FJET(XB1, XB2, s, 0.0D0, -t572, 0.0D0, t574, 0.0D0, t635)
      t638 = FJET(XB1, XB2, s, t379, -t377, t375, 0.0D0, -t385, t425)
      t640 = FJET(XB1, XB2, s, t574, 0.0D0, -t572, 0.0D0, 0.0D0, t635)
      t642 = t2 * x1
      t644 = Sqrt(-t480 * t590)
      t645 = t88 * t644
      t651 = t642 * x2 * (-x3 + t568 - z + t89 - x1 + t472 + t371 - t473
     # + 0.2D1 * t645) * t373 * t570
      t655 = t80 * s * t1 * t376 * t570
      t658 = t207 * x1
      t660 = t207 * t371
      t664 = t642 * (-x2 + t568 + 0.2D1 * t645 * x2 + 0.1D1 - x3 + t658 
     #+ t207 * z - t660) * t373 * t570
      t665 = t377 * t573
      t666 = x2 * x1
      t667 = t666 * z
      t668 = -z - t666 + t667
      t670 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, t204, -t571, x4)
      t678 = log(-0.4D1 * t207 * t132 * t388 * t522 * t590 * t579)
      t680 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, t204, -t571, x4)
      t681 = t668 * t680
      t697 = 0.2D1 * t645 * t667 + t660 - 0.2D1 * t645 * t666 + t477 * t
     #666 - t165 * t3 * x2 + 0.2D1 * t165 * x2 * z - 0.2D1 * t568 * t371
     # + t473 + t479 - t478 - t475 + t165 + t371
      t704 = x2 * t131
      t708 = t89 - t476 - t658 + t667 - t165 * x2 - 0.2D1 * t645 * z + t
     #568 * x1 - t568 * z - t666 * t3 + t704 * t3 - 0.2D1 * t704 * z + t
     #3 + t704
      t710 = 0.1D1 / (t697 + t708)
      t717 = -0.90D2 * t26 * (t372 * t668 * t670 - t678 * t372 * t681) *
     # t710 + 0.180D3 * t130 * t372 * t681 * t710
      t720 = t717 * t127 * t236 / 0.2880D4
      t721 = FJET(XB1, XB2, s, t651, t655, -t664, -t665, -t385, -t720)
      t723 = t361 * t161
      t726 = FJET(XB1, XB2, s, -t665, -t664, t655, t651, -t385, -t720)
      bbarbbarH4n3e1 = t365 * t364 + t367 * t364 + t426 * t425 + t564 * 
     #t563 + t566 * t563 + t636 * t635 + t638 * t425 + t640 * t635 - t72
     #1 * t717 * t723 / 0.2880D4 - t726 * t717 * t723 / 0.2880D4

      end function



      doubleprecision function bbarbbarH4n3e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, 
     #x4)
      t6 = z * t5
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t16 = -0.1D1 + x3
      t17 = 0.1D1 / t16
      t18 = t15 * t17
      t21 = log(-0.4D1 * t8 * t18)
      t23 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t26 = cos(t9)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t16)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t8 * t15)
      t42 = t4 * lh
      t44 = t23 * z * t33
      t49 = 0.1D1 / x3
      t51 = 0.1D1 / x1
      t54 = 0.1D1 - x2
      t55 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4
     #)
      t58 = 0.1D1 / x2
      t59 = t49 * t58
      t60 = t59 * t51
      t64 = 0.180D3 * t23 * lh
      t65 = x2 ** 2
      t66 = t65 * t7
      t69 = log(0.4D1 * t66 * t15)
      t72 = 0.90D2 * t5
      t73 = 0.180D3 * lh
      t74 = -t54
      t75 = t15 * t74
      t78 = log(-0.4D1 * t66 * t75)
      t82 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4
     #)
      t83 = 0.90D2 * t82
      t86 = t58 * t51
      t89 = t7 * t11
      t92 = log(0.4D1 * t89 * t14)
      t97 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t99 = t92 ** 2
      t105 = lh ** 2
      t107 = 0.3141592653589793D1 ** 2
      t109 = 0.180D3 * t105 - 0.30D2 * t107
      t110 = t4 * t109
      t115 = t23 * t4
      t118 = t5 * t4
      t121 = x3 * t14
      t125 = log(-0.4D1 * t121 * t11 * t17)
      t130 = log(0.4D1 * t121 * t11)
      t133 = t130 ** 2
      t134 = t125 ** 2
      t143 = -t109
      t155 = x3 * t65
      t158 = log(-0.4D1 * t155 * t18)
      t167 = log(0.4D1 * t155 * t15)
      t172 = log(-0.4D1 * t155 * t75)
      t180 = t14 * t65
      t183 = log(0.4D1 * t180 * t11)
      t185 = t11 * t74
      t188 = log(-0.4D1 * t180 * t185)
      t193 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x
     #4)
      t195 = t188 ** 2
      t199 = t183 ** 2
      t212 = log(0.4D1 * t15)
      t219 = t212 ** 2
      t242 = (0.90D2 * t4 * (-t5 - (t6 - t21 * z * t23) * t33 + t37 * t2
     #3) - 0.180D3 * t42 * (-t23 - t44)) * t49 * t51 / 0.5760D4 + t4 * (
     #t55 - t23 - t44) * t60 / 0.32D2 - t4 * (-t64 - 0.90D2 * t69 * t23 
     #+ t72 + (t73 + 0.90D2 * t78) * t55 - t83) * t86 / 0.2880D4 - (-0.1
     #80D3 * t42 * (-t92 * t23 + t5) + 0.90D2 * t4 * (t97 - t92 * t5 + t
     #99 * t23 / 0.2D1) + t110 * t23) * t51 / 0.5760D4 - ((0.180D3 * t11
     #5 * lh - 0.90D2 * t118) * (t125 * z * t33 + t130) - 0.90D2 * t115 
     #* (-t133 / 0.2D1 - t134 * z * t33 / 0.2D1) + (0.180D3 * t118 * lh 
     #+ t115 * t143 - 0.90D2 * t97 * t4) * (-z * t33 - 0.1D1)) * t49 / 0
     #.11520D5 - t4 * ((0.90D2 * t6 + (-t73 - 0.90D2 * t158) * z * t23) 
     #* t33 - t64 + t72 - 0.90D2 * t167 * t23 - t83 - (-t73 - 0.90D2 * t
     #172) * t55) * t59 / 0.5760D4 + (0.180D3 * t42 * (t5 - t183 * t23 -
     # t82 + t188 * t55) - 0.90D2 * t4 * (-t193 + t188 * t82 - t195 * t5
     #5 / 0.2D1 + t97 - t183 * t5 + t199 * t23 / 0.2D1) + t4 * t143 * (t
     #23 - t55)) * t58 / 0.5760D4 + (t5 - t212 * t23) * t4 * t143 / 0.11
     #520D5 + (t97 - t212 * t5 + t219 * t23 / 0.2D1) * t4 * lh / 0.64D2 
     #+ t115 * (-0.60D2 * lh * t107 + 0.2884936567583026D3 + 0.120D3 * t
     #105 * lh) / 0.11520D5 - (-t212 * t97 + t219 * t5 / 0.2D1 - t219 * 
     #t212 * t23 / 0.6D1) * t4 / 0.128D3
      t243 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t242)
      t245 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t242)
      t248 = t1 * x1
      t249 = x1 * z
      t250 = -z - x1 + t249
      t251 = 0.1D1 / t250
      t253 = t74 * s * t248 * t251
      t254 = -0.1D1 + x1
      t255 = t2 * t254
      t257 = x2 * s * t248
      t258 = t1 ** 2
      t259 = s * t258
      t262 = x1 * t254 * t251
      t263 = t259 * t74 * t262
      t264 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t268 = t66 * t11
      t270 = 0.1D1 / t12 * t251
      t271 = t254 ** 2
      t276 = log(0.4D1 * t268 * t270 * t271 * t74)
      t280 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t286 = -t4 * t264 * t60 / 0.32D2 - t4 * (-(t73 + 0.90D2 * t276) * 
     #t264 + 0.90D2 * t280) * t86 / 0.2880D4
      t287 = FJET(XB1, XB2, s, 0.0D0, t253, -t255, t257, -t263, t286)
      t290 = t2 * x1 * t251
      t291 = t259 * t262
      t292 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t294 = t8 * t11
      t299 = log(0.4D1 * t294 * t270 * t271 * t17)
      t301 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t305 = x1 * x3
      t306 = t305 * z
      t308 = 0.2D1 * t8 * z
      t309 = x1 * t12
      t310 = x3 * t12
      t311 = t310 * x1
      t312 = t8 * t12
      t313 = x3 * t250
      t315 = Sqrt(t313 * t16)
      t320 = 0.1D1 / (-t249 - t306 - t27 - t8 + t308 + t309 + t311 - t31
     #2 + 0.2D1 * t26 * t315 * z - t12)
      t322 = t270 * t271
      t325 = log(-0.4D1 * t294 * t322)
      t333 = t301 - z * t250 * t301 * t320
      t346 = log(-0.4D1 * t268 * t322)
      t356 = log(-0.4D1 * t89 * t322)
      t361 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t363 = t356 ** 2
      t373 = (0.90D2 * t4 * (-(z * t292 - t299 * z * t301) * t250 * t320
     # - t325 * t301 + t292) - 0.180D3 * t42 * t333) * t49 * t51 / 0.576
     #0D4 + t4 * t333 * t60 / 0.32D2 - t4 * (-0.90D2 * t292 - (-t73 - 0.
     #90D2 * t346) * t301) * t86 / 0.2880D4 - (-0.180D3 * t42 * (t356 * 
     #t301 - t292) + 0.90D2 * t4 * (-t361 + t356 * t292 - t363 * t301 / 
     #0.2D1) - t110 * t301) * t51 / 0.5760D4
      t374 = FJET(XB1, XB2, s, 0.0D0, -t255, -t290, 0.0D0, t291, t373)
      t376 = FJET(XB1, XB2, s, 0.0D0, -t290, -t255, 0.0D0, t291, t373)
      t378 = x2 * x3
      t379 = 0.1D1 - x3 + t378
      t380 = 0.1D1 / t379
      t381 = t16 * t380
      t382 = t2 * t381
      t383 = t378 * t380
      t384 = t2 * t383
      t386 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t381, x4
     #)
      t388 = t74 * t16
      t390 = Sqrt(t27 * t388)
      t395 = 0.1D1 / (-z - x3 + t378 + 0.2D1 * t26 * t390) * t49
      t399 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t381, x4
     #)
      t405 = t379 ** 2
      t411 = log(0.4D1 * t155 * t14 * t185 * t16 / t405)
      t421 = t4 * z * t386 * t395 * t86 / 0.32D2 + t4 * (0.90D2 * z * t3
     #99 + (-0.180D3 * z * lh - 0.90D2 * t411 * z) * t386) * t395 * t58 
     #/ 0.5760D4
      t422 = FJET(XB1, XB2, s, 0.0D0, -t382, 0.0D0, t384, 0.0D0, t421)
      t424 = FJET(XB1, XB2, s, t257, -t255, t253, 0.0D0, -t263, t286)
      t426 = FJET(XB1, XB2, s, t384, 0.0D0, -t382, 0.0D0, 0.0D0, t421)
      t428 = t2 * x1
      t430 = Sqrt(-t313 * t388)
      t431 = t26 * t430
      t437 = t428 * x2 * (-x3 + t378 - z + t27 - x1 + t305 + t249 - t306
     # + 0.2D1 * t431) * t251 * t380
      t441 = t16 * s * t1 * t254 * t380
      t444 = t155 * x1
      t446 = t155 * t249
      t450 = t428 * (-x2 + t378 + 0.2D1 * t431 * x2 + 0.1D1 - x3 + t444 
     #+ t155 * z - t446) * t251 * t380
      t451 = t255 * t383
      t453 = x2 * x1
      t454 = t453 * z
      t455 = -z - t453 + t454
      t456 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, t54, -t381, x4)
      t464 = t27 + t8 - t309 - t444 + t454 + t12 + t306 - t308 - t311 + 
     #t312 + t446 - 0.2D1 * t431 * t453 + 0.2D1 * t8 * x2 * z
      t478 = x2 * t7
      t482 = t310 * t453 - t8 * t12 * x2 - 0.2D1 * t378 * t249 + 0.2D1 *
     # t431 * t454 - 0.2D1 * z * t431 - t8 * x2 - t378 * z + t378 * x1 -
     # t453 * t12 - 0.2D1 * t478 * z + t478 * t12 + t478 + t249
      t484 = 0.1D1 / (t464 + t482)
      t488 = t4 * t250 * t455 * t456 * t484 * t49 * t86 / 0.32D2
      t489 = FJET(XB1, XB2, s, t437, t441, -t450, -t451, -t263, t488)
      t491 = t250 * t455
      t494 = t456 * t484 * t60
      t497 = FJET(XB1, XB2, s, -t451, -t450, t441, t437, -t263, t488)
      bbarbbarH4n3e0 = t243 * t242 + t245 * t242 + t287 * t286 + t374 * 
     #t373 + t376 * t373 + t422 * t421 + t424 * t286 + t426 * t421 + t48
     #9 * t4 * t491 * t494 / 0.32D2 + t497 * t4 * t491 * t494 / 0.32D2

      end function



      doubleprecision function bbarbbarH4n3em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = x1 ** 2
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t5 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t15 = log(0.4D1 * t9 * t12)
      t16 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t18 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t22 = t16 * t4
      t24 = 0.180D3 * t22 * lh
      t26 = 0.1D1 / x1
      t30 = cos(t6)
      t31 = x3 * z
      t32 = -0.1D1 + x3
      t34 = Sqrt(-t31 * t32)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t34)
      t39 = t16 * z * t38
      t42 = 0.1D1 / x3
      t43 = t42 * t26
      t46 = 0.1D1 - x2
      t47 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4
     #)
      t48 = t16 - t47
      t51 = 0.1D1 / x2
      t52 = t51 * t26
      t58 = t42 * t51
      t61 = x2 ** 2
      t62 = t12 * t61
      t65 = log(0.4D1 * t62 * t8)
      t67 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4
     #)
      t68 = -t46
      t72 = log(-0.4D1 * t62 * t8 * t68)
      t77 = t4 * lh
      t85 = log(0.4D1 * t8 * t12)
      t91 = lh ** 2
      t93 = 0.3141592653589793D1 ** 2
      t98 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t100 = t85 ** 2
      t106 = x3 * t12
      t111 = log(-0.4D1 * t106 * t8 / t32)
      t116 = log(0.4D1 * t106 * t8)
      t129 = -(0.90D2 * t4 * (-t15 * t16 + t18) - t24) * t26 / 0.5760D4 
     #+ t4 * (-t16 - t39) * t43 / 0.64D2 - t4 * t48 * t52 / 0.32D2 - t4 
     #* (0.90D2 * t16 + 0.90D2 * t39 - 0.90D2 * t47) * t58 / 0.5760D4 + 
     #(-0.90D2 * t4 * (t18 - t65 * t16 - t67 + t72 * t47) + 0.180D3 * t7
     #7 * t48) * t51 / 0.5760D4 + (t18 - t85 * t16) * t4 * lh / 0.64D2 +
     # t22 * (-0.180D3 * t91 + 0.30D2 * t93) / 0.11520D5 - (t98 - t85 * 
     #t18 + t100 * t16 / 0.2D1) * t4 / 0.128D3 - (-0.90D2 * t22 * (t111 
     #* z * t38 + t116) + (t24 - 0.90D2 * t18 * t4) * (-z * t38 - 0.1D1)
     #) * t42 / 0.11520D5
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t129)
      t132 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t129)
      t135 = t1 * x1
      t136 = x1 * z
      t137 = -z - x1 + t136
      t138 = 0.1D1 / t137
      t140 = t68 * s * t135 * t138
      t141 = -0.1D1 + x1
      t142 = t2 * t141
      t144 = x2 * s * t135
      t145 = t1 ** 2
      t146 = s * t145
      t149 = x1 * t141 * t138
      t150 = t146 * t68 * t149
      t151 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, t46, 0.10D1, x4)
      t154 = t4 * t151 * t52 / 0.32D2
      t155 = FJET(XB1, XB2, s, 0.0D0, t140, -t142, t144, -t150, -t154)
      t158 = t151 * t51 * t26
      t162 = t2 * x1 * t138
      t163 = t146 * t149
      t166 = t141 ** 2
      t170 = log(-0.4D1 * t9 / t10 * t138 * t166)
      t171 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t173 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x
     #4)
      t185 = x3 * t5
      t194 = Sqrt(x3 * t137 * t32)
      t209 = -(0.90D2 * t4 * (t170 * t171 - t173) + 0.180D3 * t77 * t171
     #) * t26 / 0.5760D4 + t4 * (t171 - z * t137 * t171 / (-t136 - x3 * 
     #x1 * z - t31 - t185 + 0.2D1 * t185 * z + x1 * t10 + x3 * t10 * x1 
     #- t185 * t10 + 0.2D1 * t30 * t194 * z - t10)) * t43 / 0.64D2 + t4 
     #* t171 * t52 / 0.32D2
      t210 = FJET(XB1, XB2, s, 0.0D0, -t142, -t162, 0.0D0, t163, t209)
      t212 = FJET(XB1, XB2, s, 0.0D0, -t162, -t142, 0.0D0, t163, t209)
      t214 = x2 * x3
      t216 = 0.1D1 / (0.1D1 - x3 + t214)
      t217 = t32 * t216
      t218 = t2 * t217
      t220 = t2 * t214 * t216
      t222 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, -t217, x4
     #)
      t226 = Sqrt(t31 * t68 * t32)
      t230 = 0.1D1 / (-z - x3 + t214 + 0.2D1 * t30 * t226)
      t234 = t4 * z * t222 * t230 * t42 * t51 / 0.64D2
      t235 = FJET(XB1, XB2, s, 0.0D0, -t218, 0.0D0, t220, 0.0D0, t234)
      t239 = t222 * t230 * t58
      t242 = FJET(XB1, XB2, s, t144, -t142, t140, 0.0D0, -t150, -t154)
      t246 = FJET(XB1, XB2, s, t220, 0.0D0, -t218, 0.0D0, 0.0D0, t234)
      bbarbbarH4n3em1 = t130 * t129 + t132 * t129 - t155 * t4 * t158 / 0
     #.32D2 + t210 * t209 + t212 * t209 + t235 * t4 * z * t239 / 0.64D2 
     #- t242 * t4 * t158 / 0.32D2 + t246 * t4 * z * t239 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n3em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, 
     #x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = 0.1D1 / x1
      t11 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.1D1 - x2, 0.1
     #0D1, x4)
      t19 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1,
     # x4)
      t20 = z ** 2
      t23 = x4 * 0.3141592653589793D1
      t24 = Sin(t23)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t33 = cos(t23)
      t37 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t48 = -t6 * t7 / 0.64D2 - t5 * (t3 - t11) / x2 / 0.64D2 + t6 * lh 
     #/ 0.64D2 - (t19 - t28 * t3) * t5 / 0.128D3 + t6 * (-z / (-z - x3 +
     # 0.2D1 * t33 * t37) - 0.1D1) / x3 / 0.128D3
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t51 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t48)
      t53 = -0.1D1 + x1
      t54 = t2 * t53
      t57 = 0.1D1 / (-z - x1 + x1 * z)
      t59 = t2 * x1 * t57
      t60 = t1 ** 2
      t64 = s * t60 * x1 * t53 * t57
      t65 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4
     #)
      t68 = t5 * t65 * t7 / 0.64D2
      t69 = FJET(XB1, XB2, s, 0.0D0, -t54, -t59, 0.0D0, t64, t68)
      t71 = t65 * t7
      t74 = FJET(XB1, XB2, s, 0.0D0, -t59, -t54, 0.0D0, t64, t68)
      bbarbbarH4n3em2 = t49 * t48 + t51 * t48 + t69 * t5 * t71 / 0.64D2 
     #+ t74 * t5 * t71 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n3em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t2 = s * (-0.1D1 + z)
      t3 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, 
     #x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = t3 * t5 / 0.128D3
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t7)
      bbarbbarH4n3em3 = -t8 * t3 * t5 / 0.128D3 - t11 * t3 * t5 / 0.128D
     #3

      end function



      doubleprecision function bbarbbarH4n3em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      bbarbbarH4n3em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH4n4e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t6 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x
     #4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t17 = log(0.4D1 * t15)
      t18 = t17 ** 2
      t19 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t22 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t27 = lh ** 2
      t29 = 0.3141592653589793D1 ** 2
      t31 = 0.180D3 * t27 - 0.30D2 * t29
      t32 = t4 * t31
      t49 = -0.2884936567583026D3 - 0.120D3 * t27 * lh + 0.60D2 * lh * t
     #29
      t50 = t4 * t49
      t51 = t50 * t19
      t53 = 0.1D1 / x1
      t56 = t7 * x3
      t57 = t14 * t10
      t60 = log(0.4D1 * t56 * t57)
      t66 = t60 ** 2
      t74 = 0.1D1 / x3
      t78 = t56 * x2
      t79 = -0.1D1 + x2
      t80 = t79 ** 2
      t81 = t57 * t80
      t84 = log(0.4D1 * t78 * t81)
      t85 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t87 = x2 * x3
      t90 = log(0.4D1 * t87 * t15)
      t92 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t96 = t85 - t19
      t101 = 0.1D1 / x2
      t102 = t101 * t53
      t105 = x2 * t7
      t108 = log(0.4D1 * t105 * t81)
      t112 = log(0.4D1 * t105 * t57)
      t118 = t112 ** 2
      t121 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4
     #)
      t123 = t108 ** 2
      t129 = t32 * t96
      t135 = log(0.4D1 * t57)
      t136 = t135 * t4
      t139 = t135 ** 2
      t140 = t139 * t4
      t149 = t139 * t135 * t4
      t154 = t139 ** 2
      t159 = t29 ** 2
      t160 = t27 ** 2
      t176 = log(0.4D1 * x3 * t14 * t10)
      t178 = t176 ** 2
      t199 = x2 * t14
      t200 = t10 * t80
      t203 = log(0.4D1 * t199 * t200)
      t205 = t203 ** 2
      t210 = log(0.4D1 * t199 * t10)
      t212 = t210 ** 2
      t244 = log(0.4D1 * t87 * t81)
      t248 = log(0.4D1 * t87 * t57)
      t254 = t248 ** 2
      t258 = t244 ** 2
      t268 = (-0.180D3 * t5 * (-t6 - t18 * t19 / 0.2D1 + t17 * t22) + t3
     #2 * (-t22 + t17 * t19) + 0.90D2 * t4 * (t17 * t6 + t18 * t17 * t19
     # / 0.6D1 - t18 * t22 / 0.2D1) - t51) * t53 / 0.2880D4 + (-0.180D3 
     #* t5 * (t60 * t19 - t22) + 0.90D2 * t4 * (-t6 + t60 * t22 - t66 * 
     #t19 / 0.2D1) - t32 * t19) * t74 * t53 / 0.2880D4 + (0.90D2 * t4 * 
     #(-t84 * t85 - t22 + t90 * t19 + t92) - 0.180D3 * t5 * t96) * t74 *
     # t102 / 0.2880D4 + (-0.180D3 * t5 * (t92 - t108 * t85 - t22 + t112
     # * t19) + 0.90D2 * t4 * (-t6 + t112 * t22 - t118 * t19 / 0.2D1 + t
     #121 - t108 * t92 + t123 * t85 / 0.2D1) + t129) * t101 * t53 / 0.28
     #80D4 - (0.180D3 * t136 * lh + t32 + 0.45D2 * t140) * t6 / 0.5760D4
     # - (-t136 * t31 - 0.90D2 * t140 * lh + t50 - 0.15D2 * t149) * t22 
     #/ 0.5760D4 - (0.15D2 / 0.4D1 * t154 * t4 - t136 * t49 + t4 * (0.57
     #69873135166051D3 * lh + t159 + 0.60D2 * t160 - 0.60D2 * t27 * t29)
     # + t140 * t31 / 0.2D1 + 0.30D2 * t149 * lh) * t19 / 0.5760D4 + (-0
     #.180D3 * t5 * (-t6 + t176 * t22 - t178 * t19 / 0.2D1) + t32 * (t17
     #6 * t19 - t22) + 0.90D2 * t4 * (t176 * t6 - t178 * t22 / 0.2D1 + t
     #178 * t176 * t19 / 0.6D1) - t51) * t74 / 0.5760D4 - (-0.180D3 * t5
     # * (-t121 + t203 * t92 - t205 * t85 / 0.2D1 + t6 - t210 * t22 + t2
     #12 * t19 / 0.2D1) + t32 * (t22 - t210 * t19 - t92 + t203 * t85) + 
     #0.90D2 * t4 * (t203 * t121 - t205 * t92 / 0.2D1 + t205 * t203 * t8
     #5 / 0.6D1 - t210 * t6 + t212 * t22 / 0.2D1 - t212 * t210 * t19 / 0
     #.6D1) - t50 * t96) * t101 / 0.5760D4 + (-0.180D3 * t5 * (-t244 * t
     #85 + t92 + t248 * t19 - t22) + 0.90D2 * t4 * (t248 * t22 - t254 * 
     #t19 / 0.2D1 + t121 - t244 * t92 - t6 + t258 * t85 / 0.2D1) + t129)
     # * t74 * t101 / 0.5760D4
      t269 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t268)
      t271 = -0.1D1 + x1
      t272 = t2 * t271
      t273 = t2 * x1
      t274 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t275 = 0.1D1 / t12
      t276 = x1 * z
      t277 = -z - x1 + t276
      t278 = 0.1D1 / t277
      t279 = t275 * t278
      t280 = t271 ** 2
      t281 = t279 * t280
      t284 = log(-0.4D1 * t11 * t281)
      t285 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t287 = t284 ** 2
      t288 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t309 = t56 * t10
      t312 = log(-0.4D1 * t309 * t281)
      t317 = t312 ** 2
      t324 = t32 * t288
      t329 = t280 * t278
      t333 = log(-0.4D1 * t78 * t10 * t275 * t329)
      t343 = t105 * t10
      t346 = log(-0.4D1 * t343 * t281)
      t352 = t346 ** 2
      t362 = (-0.180D3 * t5 * (t274 - t284 * t285 + t287 * t288 / 0.2D1)
     # + t32 * (-t284 * t288 + t285) + 0.90D2 * t4 * (-t284 * t274 + t28
     #7 * t285 / 0.2D1 - t287 * t284 * t288 / 0.6D1) + t50 * t288) * t53
     # / 0.2880D4 + (-0.180D3 * t5 * (-t312 * t288 + t285) + 0.90D2 * t4
     # * (t274 + t317 * t288 / 0.2D1 - t312 * t285) + t324) * t74 * t53 
     #/ 0.2880D4 + (0.90D2 * t4 * (-t333 * t288 + t285) - 0.180D3 * t5 *
     # t288) * t74 * t102 / 0.2880D4 + (-0.180D3 * t5 * (t285 - t346 * t
     #288) + 0.90D2 * t4 * (-t346 * t285 + t352 * t288 / 0.2D1 + t274) +
     # t324) * t101 * t53 / 0.2880D4
      t363 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t272, t273, 0.0D0, t362)
      t365 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t268)
      t367 = -0.1D1 + x3
      t368 = t87 - 0.1D1
      t369 = 0.1D1 / t368
      t370 = t367 * t369
      t371 = t2 * t370
      t373 = x3 * t79 * t369
      t374 = t2 * t373
      t375 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t370, x4)
      t376 = z * t375
      t377 = t87 * t11
      t379 = t368 ** 2
      t380 = 0.1D1 / t379
      t381 = t367 * t380
      t385 = log(-0.4D1 * t377 * t14 * t80 * t381)
      t387 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t370, x4)
      t391 = cos(t8)
      t392 = x3 * z
      t393 = x2 * t367
      t395 = Sqrt(-t392 * t393)
      t399 = 0.1D1 / (-z - t87 + 0.2D1 * t391 * t395)
      t403 = z * t387 * t399
      t414 = log(-0.4D1 * t87 * t14 * t200 * t381)
      t415 = t414 * z
      t421 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t370, x4)
      t424 = t414 ** 2
      t437 = (0.90D2 * t4 * (t376 - t385 * z * t387) * t399 - 0.180D3 * 
     #t5 * t403) * t74 * t102 / 0.2880D4 + (-0.180D3 * t5 * (t376 - t415
     # * t387) * t399 + 0.90D2 * t4 * (z * t421 - t415 * t375 + t424 * z
     # * t387 / 0.2D1) * t399 + t32 * t403) * t74 * t101 / 0.5760D4
      t438 = FJET(XB1, XB2, s, 0.0D0, t371, 0.0D0, t374, 0.0D0, t437)
      t440 = t2 * t367
      t441 = t2 * x3
      t442 = -t367
      t443 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t442, x
     #4)
      t444 = t57 * t367
      t447 = log(-0.4D1 * t56 * t444)
      t448 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t442, x
     #4)
      t453 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t442, x
     #4)
      t454 = t447 ** 2
      t461 = t32 * t448
      t468 = log(-0.4D1 * t78 * t444)
      t482 = log(-0.4D1 * t57 * x3 * t367)
      t484 = t482 ** 2
      t508 = log(-0.4D1 * t87 * t444)
      t514 = t508 ** 2
      t524 = (-0.180D3 * t5 * (t443 - t447 * t448) + 0.90D2 * t4 * (t453
     # + t454 * t448 / 0.2D1 - t447 * t443) + t461) * t74 * t53 / 0.2880
     #D4 + (0.90D2 * t4 * (t443 - t468 * t448) - 0.180D3 * t5 * t448) * 
     #t74 * t102 / 0.2880D4 + (-0.180D3 * t5 * (t453 - t482 * t443 + t48
     #4 * t448 / 0.2D1) + t32 * (-t482 * t448 + t443) + 0.90D2 * t4 * (-
     #t482 * t453 + t484 * t443 / 0.2D1 - t484 * t482 * t448 / 0.6D1) + 
     #t50 * t448) * t74 / 0.5760D4 + (-0.180D3 * t5 * (t443 - t508 * t44
     #8) + 0.90D2 * t4 * (-t508 * t443 + t514 * t448 / 0.2D1 + t453) + t
     #461) * t74 * t101 / 0.5760D4
      t525 = FJET(XB1, XB2, s, 0.0D0, -t440, 0.0D0, t441, 0.0D0, t524)
      t527 = x2 * x1
      t529 = t2 * t527 * t278
      t531 = t1 * x1
      t532 = t79 * s * t531
      t533 = t1 ** 2
      t538 = s * t533 * x2 * x1 * t271 * t278
      t539 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t542 = t275 * t280 * t278 * t80
      t545 = log(-0.4D1 * t377 * t542)
      t546 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t558 = log(-0.4D1 * t343 * t542)
      t563 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t565 = t558 ** 2
      t576 = (0.90D2 * t4 * (-t539 + t545 * t546) + 0.180D3 * t5 * t546)
     # * t74 * t102 / 0.2880D4 + (-0.180D3 * t5 * (t558 * t546 - t539) +
     # 0.90D2 * t4 * (-t563 + t558 * t539 - t565 * t546 / 0.2D1) - t32 *
     # t546) * t101 * t53 / 0.2880D4
      t577 = FJET(XB1, XB2, s, 0.0D0, -t529, -t272, -t532, t538, t576)
      t579 = FJET(XB1, XB2, s, t273, -t272, 0.0D0, 0.0D0, 0.0D0, t362)
      t581 = FJET(XB1, XB2, s, t441, 0.0D0, -t440, 0.0D0, 0.0D0, t524)
      t583 = FJET(XB1, XB2, s, t374, 0.0D0, t371, 0.0D0, 0.0D0, t437)
      t585 = x1 * x3
      t586 = t585 * z
      t589 = Sqrt(x3 * t277 * t393)
      t590 = t391 * t589
      t591 = 0.2D1 * t590
      t596 = t273 * t79 * (-t87 - z + t392 - x1 + t585 + t276 - t586 + t
     #591) * t278 * t369
      t597 = t367 * s
      t598 = t1 * t271
      t600 = t597 * t598 * t369
      t601 = x2 ** 2
      t602 = t601 * x3
      t603 = t602 * x1
      t605 = t87 * z
      t607 = t87 * x1
      t611 = t602 * t276
      t614 = -t392 - t585 + t87 - t591 - x2 - t603 - t602 * z + t586 + 0
     #.2D1 * t605 + 0.2D1 * t607 + 0.2D1 * t590 * x2 + t611 - 0.2D1 * t8
     #7 * t276
      t617 = t273 * t614 * t278 * t369
      t618 = t272 * t373
      t619 = t527 * z
      t620 = z + x1 - t276 - t527 + t619
      t622 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, x2, t370, x4)
      t631 = log(0.4D1 * t87 * t11 * t275 * t329 * t80 * t367 * t380)
      t633 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, x2, t370, x4)
      t634 = t620 * t633
      t648 = 0.2D1 * t590 * z + 0.2D1 * t590 * x1 - t7 - 0.2D1 * t276 - 
     #t78 + t603 + t619 - t605 - t607 + 0.2D1 * t590 * t619 - t527 * t12
     # - 0.2D1 * t105 * z
      t666 = t105 * t12 - 0.2D1 * t590 * t527 - 0.2D1 * t590 * t276 + 0.
     #2D1 * x1 * t12 + 0.2D1 * t7 * z - t7 * t12 + t105 - t12 + x3 * t12
     # * t527 + 0.2D1 * t56 * x2 * z - t56 * t12 * x2 - t611
      t668 = 0.1D1 / (t648 + t666)
      t675 = 0.90D2 * t4 * (t277 * t620 * t622 - t631 * t277 * t634) * t
     #668 - 0.180D3 * t5 * t277 * t634 * t668
      t678 = t675 * t74 * t102 / 0.2880D4
      t679 = FJET(XB1, XB2, s, t596, -t600, -t617, -t618, t538, t678)
      t682 = t74 * t101 * t53
      t686 = t2 * t271 * x3
      t687 = t2 * t585
      t688 = t597 * t598
      t689 = t597 * t531
      t691 = t279 * t280 * t367
      t694 = log(0.4D1 * t309 * t691)
      t695 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t442, x4)
      t697 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t442, x4)
      t701 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t442, x4)
      t703 = t694 ** 2
      t715 = log(0.4D1 * t377 * t691)
      t726 = (-0.180D3 * t5 * (t694 * t695 - t697) + 0.90D2 * t4 * (-t70
     #1 + t694 * t697 - t703 * t695 / 0.2D1) - t32 * t695) * t74 * t53 /
     # 0.2880D4 + (0.90D2 * t4 * (-t697 + t715 * t695) + 0.180D3 * t5 * 
     #t695) * t74 * t102 / 0.2880D4
      t727 = FJET(XB1, XB2, s, -t686, t687, t688, -t689, 0.0D0, t726)
      t729 = FJET(XB1, XB2, s, -t532, -t272, -t529, 0.0D0, t538, t576)
      t731 = FJET(XB1, XB2, s, -t689, t688, t687, -t686, 0.0D0, t726)
      t733 = FJET(XB1, XB2, s, -t618, -t617, -t600, t596, t538, t678)
      bbarbbarH4n4e1 = t269 * t268 + t363 * t362 + t365 * t268 + t438 * 
     #t437 + t525 * t524 + t577 * t576 + t579 * t362 + t581 * t524 + t58
     #3 * t437 + t679 * t675 * t682 / 0.2880D4 + t727 * t726 + t729 * t5
     #76 + t731 * t726 + t733 * t675 * t682 / 0.2880D4

      end function



      doubleprecision function bbarbbarH4n4e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = x1 ** 2
      t6 = x3 * t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t16 = log(0.4D1 * t6 * t13)
      t17 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t19 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t23 = t4 * lh
      t27 = 0.1D1 / x3
      t29 = 0.1D1 / x1
      t32 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t33 = t32 - t17
      t35 = 0.1D1 / x2
      t37 = t27 * t35 * t29
      t40 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t41 = x2 * t5
      t42 = -0.1D1 + x2
      t43 = t42 ** 2
      t44 = t13 * t43
      t47 = log(0.4D1 * t41 * t44)
      t51 = log(0.4D1 * t41 * t13)
      t57 = 0.180D3 * t23 * t33
      t62 = t5 * t12
      t65 = log(0.4D1 * t62 * t9)
      t70 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t71 = t65 ** 2
      t78 = lh ** 2
      t80 = 0.3141592653589793D1 ** 2
      t82 = 0.180D3 * t78 - 0.30D2 * t80
      t83 = t4 * t82
      t84 = t83 * t17
      t91 = log(0.4D1 * x3 * t9 * t12)
      t97 = t91 ** 2
      t106 = x2 * x3
      t109 = log(0.4D1 * t106 * t44)
      t113 = log(0.4D1 * t106 * t13)
      t122 = x2 * t9
      t125 = log(0.4D1 * t122 * t12)
      t127 = t12 * t43
      t130 = log(0.4D1 * t122 * t127)
      t135 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4
     #)
      t137 = t130 ** 2
      t141 = t125 ** 2
      t154 = log(0.4D1 * t13)
      t155 = t154 * t4
      t162 = t154 ** 2
      t163 = t162 * t4
      t183 = (0.90D2 * t4 * (t16 * t17 - t19) + 0.180D3 * t23 * t17) * t
     #27 * t29 / 0.2880D4 + t4 * t33 * t37 / 0.32D2 + (0.90D2 * t4 * (t4
     #0 - t47 * t32 - t19 + t51 * t17) - t57) * t35 * t29 / 0.2880D4 + (
     #-0.180D3 * t23 * (-t19 + t65 * t17) + 0.90D2 * t4 * (-t70 - t71 * 
     #t17 / 0.2D1 + t65 * t19) - t84) * t29 / 0.2880D4 + (-0.180D3 * t23
     # * (t91 * t17 - t19) + 0.90D2 * t4 * (-t70 + t91 * t19 - t97 * t17
     # / 0.2D1) - t84) * t27 / 0.5760D4 + (0.90D2 * t4 * (-t109 * t32 + 
     #t40 + t113 * t17 - t19) - t57) * t27 * t35 / 0.5760D4 - (-0.180D3 
     #* t23 * (t19 - t125 * t17 - t40 + t130 * t32) + 0.90D2 * t4 * (-t1
     #35 + t130 * t40 - t137 * t32 / 0.2D1 + t70 - t125 * t19 + t141 * t
     #17 / 0.2D1) - t83 * t33) * t35 / 0.5760D4 - (-0.180D3 * t23 - 0.90
     #D2 * t155) * t70 / 0.5760D4 - (0.180D3 * t155 * lh + t83 + 0.45D2 
     #* t163) * t19 / 0.5760D4 - (-t155 * t82 - 0.90D2 * t163 * lh + t4 
     #* (-0.2884936567583026D3 - 0.120D3 * t78 * lh + 0.60D2 * lh * t80)
     # - 0.15D2 * t162 * t154 * t4) * t17 / 0.5760D4
      t184 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t183)
      t186 = -0.1D1 + x1
      t187 = t2 * t186
      t188 = t2 * x1
      t189 = t6 * t12
      t191 = x1 * z
      t192 = -z - x1 + t191
      t193 = 0.1D1 / t192
      t194 = 0.1D1 / t7 * t193
      t195 = t186 ** 2
      t196 = t194 * t195
      t199 = log(-0.4D1 * t189 * t196)
      t200 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t202 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t207 = 0.180D3 * t23 * t200
      t215 = t41 * t12
      t218 = log(-0.4D1 * t215 * t196)
      t229 = log(-0.4D1 * t62 * t196)
      t234 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t236 = t229 ** 2
      t246 = (0.90D2 * t4 * (-t199 * t200 + t202) - t207) * t27 * t29 / 
     #0.2880D4 + t4 * t200 * t37 / 0.32D2 + (0.90D2 * t4 * (t202 - t218 
     #* t200) - t207) * t35 * t29 / 0.2880D4 + (-0.180D3 * t23 * (-t229 
     #* t200 + t202) + 0.90D2 * t4 * (t234 - t229 * t202 + t236 * t200 /
     # 0.2D1) + t83 * t200) * t29 / 0.2880D4
      t247 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t187, t188, 0.0D0, t246)
      t249 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t183)
      t251 = -0.1D1 + x3
      t252 = t106 - 0.1D1
      t253 = 0.1D1 / t252
      t254 = t251 * t253
      t255 = t2 * t254
      t257 = x3 * t42 * t253
      t258 = t2 * t257
      t260 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t254, x4)
      t262 = cos(t10)
      t263 = x3 * z
      t264 = x2 * t251
      t266 = Sqrt(-t263 * t264)
      t270 = 0.1D1 / (-z - t106 + 0.2D1 * t262 * t266)
      t272 = t35 * t29
      t276 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t254, x4)
      t279 = t252 ** 2
      t285 = log(-0.4D1 * t106 * t9 * t127 * t251 / t279)
      t300 = t4 * z * t260 * t270 * t27 * t272 / 0.32D2 + (0.90D2 * t4 *
     # (z * t276 - t285 * z * t260) * t270 - 0.180D3 * t23 * z * t260 * 
     #t270) * t27 * t35 / 0.5760D4
      t301 = FJET(XB1, XB2, s, 0.0D0, t255, 0.0D0, t258, 0.0D0, t300)
      t303 = t2 * t251
      t304 = t2 * x3
      t305 = -t251
      t306 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t305, x
     #4)
      t307 = t13 * t251
      t310 = log(-0.4D1 * t6 * t307)
      t311 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t305, x
     #4)
      t317 = 0.180D3 * t23 * t311
      t327 = log(-0.4D1 * t106 * t307)
      t339 = log(-0.4D1 * t13 * x3 * t251)
      t344 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t305, x
     #4)
      t346 = t339 ** 2
      t356 = (0.90D2 * t4 * (t306 - t310 * t311) - t317) * t27 * t29 / 0
     #.2880D4 + t4 * t311 * t37 / 0.32D2 + (0.90D2 * t4 * (t306 - t327 *
     # t311) - t317) * t27 * t35 / 0.5760D4 + (-0.180D3 * t23 * (-t339 *
     # t311 + t306) + 0.90D2 * t4 * (t344 - t339 * t306 + t346 * t311 / 
     #0.2D1) + t83 * t311) * t27 / 0.5760D4
      t357 = FJET(XB1, XB2, s, 0.0D0, -t303, 0.0D0, t304, 0.0D0, t356)
      t359 = x2 * x1
      t361 = t2 * t359 * t193
      t363 = t1 * x1
      t364 = t42 * s * t363
      t365 = t1 ** 2
      t370 = s * t365 * x2 * x1 * t186 * t193
      t371 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t379 = log(-0.4D1 * t215 * t194 * t195 * t43)
      t381 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t391 = -t4 * t371 * t37 / 0.32D2 + (0.90D2 * t4 * (t379 * t371 - t
     #381) + 0.180D3 * t23 * t371) * t35 * t29 / 0.2880D4
      t392 = FJET(XB1, XB2, s, 0.0D0, -t361, -t187, -t364, t370, t391)
      t394 = FJET(XB1, XB2, s, t188, -t187, 0.0D0, 0.0D0, 0.0D0, t246)
      t396 = FJET(XB1, XB2, s, t304, 0.0D0, -t303, 0.0D0, 0.0D0, t356)
      t398 = FJET(XB1, XB2, s, t258, 0.0D0, t255, 0.0D0, 0.0D0, t300)
      t400 = x1 * x3
      t401 = t400 * z
      t404 = Sqrt(x3 * t192 * t264)
      t405 = t262 * t404
      t406 = 0.2D1 * t405
      t411 = t188 * t42 * (-t106 - z + t263 - x1 + t400 + t191 - t401 + 
     #t406) * t193 * t253
      t412 = t251 * s
      t413 = t1 * t186
      t415 = t412 * t413 * t253
      t416 = x2 ** 2
      t417 = t416 * x3
      t418 = t417 * x1
      t420 = t106 * z
      t422 = t106 * x1
      t426 = t417 * t191
      t429 = -t263 - t400 + t106 - t406 - x2 - t418 - t417 * z + t401 + 
     #0.2D1 * t420 + 0.2D1 * t422 + 0.2D1 * t405 * x2 + t426 - 0.2D1 * t
     #106 * t191
      t432 = t188 * t429 * t193 * t253
      t433 = t187 * t257
      t435 = t359 * z
      t436 = z + x1 - t191 - t359 + t435
      t437 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, x2, t254, x4)
      t451 = 0.2D1 * t405 * z + 0.2D1 * t405 * x1 - t5 - 0.2D1 * t191 - 
     #t6 * x2 + t418 + t435 - t420 - t422 + 0.2D1 * t405 * t435 - t359 *
     # t7 - 0.2D1 * t41 * z
      t469 = t41 * t7 - 0.2D1 * t405 * t359 - 0.2D1 * t405 * t191 + 0.2D
     #1 * x1 * t7 + 0.2D1 * t5 * z - t5 * t7 + t41 - t7 + x3 * t7 * t359
     # + 0.2D1 * t6 * x2 * z - t6 * t7 * x2 - t426
      t471 = 0.1D1 / (t451 + t469)
      t475 = t4 * t192 * t436 * t437 * t471 * t27 * t272 / 0.32D2
      t476 = FJET(XB1, XB2, s, t411, -t415, -t432, -t433, t370, t475)
      t478 = t192 * t436
      t481 = t437 * t471 * t37
      t485 = t2 * t186 * x3
      t486 = t2 * t400
      t487 = t412 * t413
      t488 = t412 * t363
      t493 = log(0.4D1 * t189 * t194 * t195 * t251)
      t494 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t305, x4)
      t496 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t305, x4)
      t509 = (0.90D2 * t4 * (t493 * t494 - t496) + 0.180D3 * t23 * t494)
     # * t27 * t29 / 0.2880D4 - t4 * t494 * t37 / 0.32D2
      t510 = FJET(XB1, XB2, s, -t485, t486, t487, -t488, 0.0D0, t509)
      t512 = FJET(XB1, XB2, s, -t364, -t187, -t361, 0.0D0, t370, t391)
      t514 = FJET(XB1, XB2, s, -t488, t487, t486, -t485, 0.0D0, t509)
      t516 = FJET(XB1, XB2, s, -t433, -t432, -t415, t411, t370, t475)
      bbarbbarH4n4e0 = t184 * t183 + t247 * t246 + t249 * t183 + t301 * 
     #t300 + t357 * t356 + t392 * t391 + t394 * t246 + t396 * t356 + t39
     #8 * t300 + t476 * t4 * t478 * t481 / 0.32D2 + t510 * t509 + t512 *
     # t391 + t514 * t509 + t516 * t4 * t478 * t481 / 0.32D2

      end function



      doubleprecision function bbarbbarH4n4em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x
     #4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t22 = t4 * lh
      t24 = 0.180D3 * t17 * t22
      t26 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t31 = t30 * t26
      t34 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t35 = t34 - t17
      t36 = t4 * t35
      t37 = 0.1D1 / x2
      t38 = t37 * t26
      t41 = t30 * t37
      t44 = x2 * t13
      t47 = log(0.4D1 * t44 * t9)
      t49 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t50 = -0.1D1 + x2
      t51 = t50 ** 2
      t55 = log(0.4D1 * t44 * t9 * t51)
      t66 = bbarbbarH41J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t70 = t13 * t9
      t72 = log(0.4D1 * t70)
      t73 = t72 * t4
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t86 = t72 ** 2
      t95 = log(0.4D1 * x3 * t13 * t9)
      t103 = (0.90D2 * t4 * (-t5 + t16 * t17) + t24) * t26 / 0.2880D4 - 
     #t4 * t17 * t31 / 0.32D2 + t36 * t38 / 0.32D2 + t36 * t41 / 0.64D2 
     #- (0.90D2 * t4 * (t5 - t47 * t17 - t49 + t55 * t34) + 0.180D3 * t2
     #2 * t35) * t37 / 0.5760D4 - t4 * t66 / 0.64D2 - (-0.180D3 * t22 - 
     #0.90D2 * t73) * t5 / 0.5760D4 - (0.180D3 * t73 * lh + t4 * (0.180D
     #3 * t80 - 0.30D2 * t82) + 0.45D2 * t86 * t4) * t17 / 0.5760D4 + (0
     #.90D2 * t4 * (t95 * t17 - t5) + t24) * t30 / 0.5760D4
      t104 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t103)
      t106 = -0.1D1 + x1
      t107 = t2 * t106
      t108 = t2 * x1
      t112 = 0.1D1 / (-z - x1 + x1 * z)
      t114 = t106 ** 2
      t118 = log(-0.4D1 * t10 / t11 * t112 * t114)
      t119 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t121 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4
     #)
      t130 = t4 * t119
      t135 = (0.90D2 * t4 * (-t118 * t119 + t121) - 0.180D3 * t22 * t119
     #) * t26 / 0.2880D4 + t130 * t31 / 0.32D2 + t130 * t38 / 0.32D2
      t136 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t107, t108, 0.0D0, t135)
      t138 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t103)
      t140 = -0.1D1 + x3
      t141 = x2 * x3
      t143 = 0.1D1 / (t141 - 0.1D1)
      t144 = t140 * t143
      t145 = t2 * t144
      t148 = t2 * x3 * t50 * t143
      t150 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t144, x4)
      t152 = cos(t7)
      t156 = Sqrt(-x3 * z * x2 * t140)
      t160 = 0.1D1 / (-z - t141 + 0.2D1 * t152 * t156)
      t164 = t4 * z * t150 * t160 * t30 * t37 / 0.64D2
      t165 = FJET(XB1, XB2, s, 0.0D0, t145, 0.0D0, t148, 0.0D0, t164)
      t169 = t150 * t160 * t41
      t172 = t2 * t140
      t173 = t2 * x3
      t174 = -t140
      t175 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t174, x
     #4)
      t176 = t4 * t175
      t184 = log(-0.4D1 * t70 * x3 * t140)
      t186 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t174, x
     #4)
      t195 = t176 * t31 / 0.32D2 + t176 * t41 / 0.64D2 + (0.90D2 * t4 * 
     #(-t184 * t175 + t186) - 0.180D3 * t22 * t175) * t30 / 0.5760D4
      t196 = FJET(XB1, XB2, s, 0.0D0, -t172, 0.0D0, t173, 0.0D0, t195)
      t200 = t2 * x1 * x2 * t112
      t202 = t1 * x1
      t203 = t50 * s * t202
      t204 = t1 ** 2
      t209 = s * t204 * x2 * x1 * t106 * t112
      t210 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t213 = t4 * t210 * t38 / 0.32D2
      t214 = FJET(XB1, XB2, s, 0.0D0, -t200, -t107, -t203, t209, -t213)
      t217 = t210 * t37 * t26
      t220 = FJET(XB1, XB2, s, t108, -t107, 0.0D0, 0.0D0, 0.0D0, t135)
      t222 = FJET(XB1, XB2, s, t173, 0.0D0, -t172, 0.0D0, 0.0D0, t195)
      t224 = FJET(XB1, XB2, s, t148, 0.0D0, t145, 0.0D0, 0.0D0, t164)
      t230 = t2 * t106 * x3
      t232 = t2 * x1 * x3
      t233 = t140 * s
      t235 = t233 * t1 * t106
      t236 = t233 * t202
      t237 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t174, x4)
      t240 = t4 * t237 * t31 / 0.32D2
      t241 = FJET(XB1, XB2, s, -t230, t232, t235, -t236, 0.0D0, -t240)
      t244 = t237 * t30 * t26
      t247 = FJET(XB1, XB2, s, -t203, -t107, -t200, 0.0D0, t209, -t213)
      t251 = FJET(XB1, XB2, s, -t236, t235, t232, -t230, 0.0D0, -t240)
      bbarbbarH4n4em1 = t104 * t103 + t136 * t135 + t138 * t103 + t165 *
     # t4 * z * t169 / 0.64D2 + t196 * t195 - t214 * t4 * t217 / 0.32D2 
     #+ t220 * t135 + t222 * t195 + t224 * t4 * z * t169 / 0.64D2 - t241
     # * t4 * t244 / 0.32D2 - t247 * t4 * t217 / 0.32D2 - t251 * t4 * t2
     #44 / 0.32D2

      end function



      doubleprecision function bbarbbarH4n4em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x
     #4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t16 = bbarbbarH41J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, 
     #x4)
      t21 = z ** 2
      t25 = Sin(x4 * 0.3141592653589793D1)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t21 / z * t26)
      t35 = 0.1D1 / x3
      t38 = -t6 * t7 / 0.32D2 - t4 * (t5 - t10) / x2 / 0.64D2 - t4 * t16
     # / 0.64D2 - (-0.180D3 * t4 * lh - 0.90D2 * t29 * t4) * t5 / 0.5760
     #D4 - t6 * t35 / 0.64D2
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t42 = t2 * (-0.1D1 + x1)
      t43 = t2 * x1
      t44 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t47 = t4 * t44 * t7 / 0.32D2
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t42, t43, 0.0D0, t47)
      t50 = t44 * t7
      t53 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t55 = -0.1D1 + x3
      t56 = t2 * t55
      t57 = t2 * x3
      t59 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, -t55, x4
     #)
      t62 = t4 * t59 * t35 / 0.64D2
      t63 = FJET(XB1, XB2, s, 0.0D0, -t56, 0.0D0, t57, 0.0D0, t62)
      t65 = t59 * t35
      t68 = FJET(XB1, XB2, s, t43, -t42, 0.0D0, 0.0D0, 0.0D0, t47)
      t72 = FJET(XB1, XB2, s, t57, 0.0D0, -t56, 0.0D0, 0.0D0, t62)
      bbarbbarH4n4em2 = t39 * t38 + t48 * t4 * t50 / 0.32D2 + t53 * t38 
     #+ t63 * t4 * t65 / 0.64D2 + t68 * t4 * t50 / 0.32D2 + t72 * t4 * t
     #65 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n4em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbarbbarH41J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x
     #4)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t7)
      bbarbbarH4n4em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2

      end function



      doubleprecision function bbarbbarH4n4em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH41J1
      doubleprecision bbarbbarH41J2
      doubleprecision bbarbbarH41J3
      bbarbbarH4n4em4 = 0.0D0

      end function
  
 

      doubleprecision function bbarbbarH41J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t8 = 0.1D1 - x3
      t9 = t8 ** 2
      t10 = s * t2
      t12 = z + t2 * x1
      t13 = 0.1D1 / t12
      t14 = x1 * t13
      t15 = 0.1D1 - x2
      t20 = cos(x4 * 0.3141592653589793D1)
      t21 = x3 * t15
      t25 = Sqrt(t21 * t12 * x2 * t8)
      t27 = 0.2D1 * t20 * t25
      t28 = t8 * t15 * t12 + x2 * x3 + t27
      t29 = t14 * t28
      t31 = t5 * t8
      t33 = s - t10 * t29 - t10 * t31
      t37 = t21 * t12 + x2 * t8 - t27
      t38 = t14 * t37
      t40 = x3 * t5
      t42 = s - t10 * t38 - t10 * t40
      t46 = x1 ** 2
      t48 = t12 ** 2
      t49 = 0.1D1 / t48
      t50 = t37 ** 2
      t52 = t33 * t42
      t56 = s * t1
      t57 = t56 * t33
      t74 = t2 * t5 * t8
      t97 = t42 * t56
      t105 = -0.2D1 * t4 * t6 * t9 * t33 * t42 - 0.2D1 * t4 * t46 * t49 
     #* t50 * t52 + 0.2D1 * t57 * t3 * t5 * t8 * x1 * t13 * t28 + 0.2D1 
     #* t1 * t2 * x1 * t13 * t37 * t52 + 0.2D1 * t42 * t1 * t33 * t74 - 
     #0.4D1 * t4 * t31 * t52 * t29 - 0.4D1 * t4 * t14 * t37 * t33 * t42 
     #* t5 * x3 + t57 * t3 * t6 * t9 + 0.2D1 * t56 * t3 * t14 * t37 * t4
     #2 * t40 - t97 * t2 * t38 + t97 * t3 * t46 * t49 * t50 - t57 * t74
      bbarbbarH41J1 = -0.16D2 / 0.3D1 * wd * t105 / t33 / t42

      end function
  
   
 

      doubleprecision function bbarbbarH41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t6 = z + t2 * x1
      t7 = 0.1D1 / t6
      t8 = x1 * t7
      t10 = 0.1D1 - x2
      t11 = x3 * t10
      t13 = 0.1D1 - x3
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t11 * t6 * x2 * t13)
      t22 = 0.2D1 * t16 * t20
      t23 = t11 * t6 + x2 * t13 - t22
      t24 = s * t2
      t28 = t13 * t10 * t6 + x2 * x3 + t22
      t29 = t8 * t28
      t31 = 0.1D1 - x1
      t32 = t31 * t13
      t34 = s - t24 * t29 - t24 * t32
      t35 = t23 * t34
      t36 = t8 * t23
      t38 = t31 * x3
      t40 = s - t24 * t36 - t24 * t38
      t46 = s * t1
      t47 = t46 * t34
      t51 = t31 ** 2
      t59 = t13 ** 2
      t75 = x1 ** 2
      t76 = t6 ** 2
      t78 = t75 / t76
      t89 = t40 * t46
      t91 = t23 ** 2
      bbarbbarH41J2 = -0.16D2 / 0.3D1 * wd * (0.4D1 * t4 * t8 * t35 * t4
     #0 * t31 * x3 - t47 * t2 * t31 * t13 + 0.3D1 * t4 * t51 * t13 * t34
     # * t40 * x3 - t47 * t3 * t51 * t59 - 0.2D1 * t46 * t3 * t8 * t23 *
     # t40 * t38 - 0.2D1 * t47 * t3 * t31 * t13 * x1 * t7 * t28 + 0.3D1 
     #* t4 * t78 * t35 * t40 * t28 + 0.4D1 * t4 * t32 * t34 * t40 * t29 
     #- t89 * t3 * t78 * t91 - t89 * t2 * t36) / t34 / t40

      end function
  
   
 

      doubleprecision function bbarbbarH41J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = s * t3
      t6 = z + x1 * t3
      t7 = 0.1D1 / t6
      t8 = x1 * t7
      t9 = 0.1D1 - x3
      t10 = 0.1D1 - x2
      t15 = cos(x4 * 0.3141592653589793D1)
      t16 = x3 * t10
      t20 = Sqrt(t16 * t6 * x2 * t9)
      t22 = 0.2D1 * t15 * t20
      t23 = t9 * t10 * t6 + x2 * x3 + t22
      t24 = t8 * t23
      t26 = 0.1D1 - x1
      t27 = t26 * t9
      t29 = s - t4 * t24 - t4 * t27
      t31 = t3 ** 2
      t38 = t1 * t31
      t42 = t16 * t6 + x2 * t9 - t22
      t45 = t26 * x3
      t47 = s - t4 * t8 * t42 - t4 * t45
      t52 = t26 ** 2
      t64 = t42 * t29
      t70 = x1 ** 2
      t71 = t6 ** 2
      bbarbbarH41J3 = -0.16D2 / 0.3D1 * wd * (-t2 * t29 * t31 * t26 * t9
     # * x1 * t7 * t23 + 0.2D1 * t38 * t27 * t29 * t47 * t24 + t38 * t52
     # * t9 * t29 * t47 * x3 - t2 * t31 * t8 * t42 * t47 * t45 + 0.2D1 *
     # t38 * t8 * t64 * t47 * t26 * x3 + t38 * t70 / t71 * t64 * t47 * t
     #23) / t29 / t47

      end function
  
 