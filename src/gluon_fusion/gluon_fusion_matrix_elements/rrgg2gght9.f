  
      subroutine rrgg2gght9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh91J1  
      doubleprecision rrgg2ggh91J2  
      doubleprecision rrgg2ggh91J3  
      doubleprecision rrgg2ggh91J4  
      doubleprecision rrgg2ggh91J5  
      doubleprecision rrgg2ggh91J6  
      doubleprecision rrgg2gght9s1e1  
      doubleprecision rrgg2gght9s1e0  
      doubleprecision rrgg2gght9s1em1  
      doubleprecision rrgg2gght9s1em2  
      doubleprecision rrgg2gght9s1em3  
      doubleprecision rrgg2gght9s1em4  
      doubleprecision rrgg2gght9s2e1  
      doubleprecision rrgg2gght9s2e0  
      doubleprecision rrgg2gght9s2em1  
      doubleprecision rrgg2gght9s2em2  
      doubleprecision rrgg2gght9s2em3  
      doubleprecision rrgg2gght9s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght9s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght9s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght9s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght9s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght9s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght9s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght9s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t5 = x1 * z
      t6 = 0.1D1 - x1 + t5
      t7 = 0.1D1 / t6
      t9 = t2 * t3 * x2 * t7
      t10 = t2 * x1
      t11 = -0.1D1 + x2
      t12 = t11 * s
      t13 = t1 * t3
      t14 = t12 * t13
      t15 = t1 ** 2
      t20 = s * t15 * x2 * x1 * t3 * t7
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = 0.3141592653589793D1 * t22
      t24 = -t3
      t25 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t24, x2, 0.0D0, x4)
      t26 = x2 * x3
      t27 = x1 ** 2
      t28 = x4 * 0.3141592653589793D1
      t29 = Sin(t28)
      t30 = t29 ** 2
      t31 = t27 * t30
      t32 = t26 * t31
      t33 = z ** 2
      t34 = 0.1D1 / t33
      t35 = t34 * t7
      t36 = t3 ** 2
      t37 = t36 * t11
      t38 = t35 * t37
      t41 = log(-0.4D1 * t32 * t38)
      t42 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t24, x2, 0.0D0, x4)
      t47 = 0.3141592653589793D1 * lh
      t48 = t22 * t42
      t52 = 0.1D1 / x3
      t54 = 0.1D1 / x1
      t55 = 0.1D1 / x2
      t56 = t54 * t55
      t57 = (-0.90D2 * t23 * (-t25 + t41 * t42) - 0.180D3 * t47 * t48) *
     # t52 * t56
      t58 = x2 * t27
      t59 = t58 * t30
      t62 = log(-0.4D1 * t59 * t38)
      t64 = t62 ** 2
      t67 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, t24, x2, 0.0D0, x4)
      t68 = t62 * t25 - t64 * t42 / 0.2D1 - t67
      t72 = -t25 + t62 * t42
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t80 = -0.180D3 * t76 + 0.30D2 * t78
      t81 = 0.3141592653589793D1 * t80
      t82 = t81 * t48
      t87 = -t57 / 0.720D3 - (-0.90D2 * t23 * t68 + 0.180D3 * t47 * t22 
     #* t72 - t82) * t54 * t55 / 0.720D3
      t88 = FJET(XB1, XB2, s, 0.0D0, -t9, t10, t14, -t20, t87)
      t90 = 0.2D1 * t26
      t91 = cos(t28)
      t93 = -0.1D1 + x3
      t96 = Sqrt(x2 * t11 * x3 * t93)
      t98 = 0.2D1 * t91 * t96
      t100 = t2 * (0.1D1 - x2 - x3 + t90 + t98)
      t102 = t2 * (-x3 + t90 - x2 + t98)
      t104 = t34 * t11
      t108 = log(0.4D1 * t26 * t30 * t104 * t93)
      t109 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t111 = t108 ** 2
      t112 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t115 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t124 = t22 * t112
      t130 = t26 * t27
      t131 = t34 * t30
      t136 = log(0.4D1 * t130 * t131 * t11 * t93)
      t147 = -(-0.90D2 * t23 * (t108 * t109 - t111 * t112 / 0.2D1 - t115
     #) + 0.180D3 * t47 * t22 * (-t109 + t108 * t112) - t81 * t124) * t5
     #2 * t55 / 0.1440D4 - (-0.90D2 * t23 * (-t109 + t136 * t112) - 0.18
     #0D3 * t47 * t124) * t52 * t56 / 0.720D3
      t148 = FJET(XB1, XB2, s, 0.0D0, t100, 0.0D0, -t102, 0.0D0, t147)
      t150 = t2 * t3
      t151 = x3 * t27
      t152 = t151 * t30
      t154 = t34 * t36 * t7
      t157 = log(0.4D1 * t152 * t154)
      t158 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.0D0,
     # x4)
      t160 = t157 ** 2
      t161 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.0D0,
     # x4)
      t164 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.0D0,
     # x4)
      t173 = t22 * t161
      t174 = t81 * t173
      t177 = (-0.90D2 * t23 * (t157 * t158 - t160 * t161 / 0.2D1 - t164)
     # + 0.180D3 * t47 * t22 * (-t158 + t157 * t161) - t174) * t52 * t54
      t180 = log(0.4D1 * t31 * t154)
      t182 = -t158 + t180 * t161
      t185 = t180 ** 2
      t188 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, 0.0D0,
     # x4)
      t193 = -t185 * t158 / 0.2D1 - t188 + t185 * t180 * t161 / 0.6D1 + 
     #t180 * t164
      t200 = -0.60D2 * lh * t78 + 0.2884936567583026D3 + 0.120D3 * t76 *
     # lh
      t201 = 0.3141592653589793D1 * t200
      t202 = t201 * t173
      t206 = t180 * t158 - t185 * t161 / 0.2D1 - t164
      t213 = t131 * t7 * t36
      t216 = log(0.4D1 * t130 * t213)
      t225 = (-0.90D2 * t23 * (t158 - t216 * t161) + 0.180D3 * t47 * t17
     #3) * t52 * t56
      t228 = log(0.4D1 * t59 * t154)
      t230 = t228 ** 2
      t243 = (-0.90D2 * t23 * (-t228 * t158 + t230 * t161 / 0.2D1 + t164
     #) + 0.180D3 * t47 * t22 * (t158 - t228 * t161) + t174) * t54 * t55
      t245 = t177 / 0.720D3 + (t81 * t22 * t182 - 0.90D2 * t23 * t193 - 
     #t202 + 0.180D3 * t47 * t22 * t206) * t54 / 0.720D3 - t225 / 0.720D
     #3 - t243 / 0.720D3
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t10, -t150, 0.0D0, t245)
      t248 = t2 * t93
      t249 = t2 * x3
      t250 = t131 * t93
      t253 = log(-0.4D1 * t151 * t250)
      t254 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t256 = t253 ** 2
      t257 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t260 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t269 = t22 * t257
      t270 = t81 * t269
      t277 = log(-0.4D1 * t130 * t250)
      t288 = x3 * t30
      t292 = log(-0.4D1 * t288 * t34 * t93)
      t297 = t292 ** 2
      t300 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t321 = log(-0.4D1 * t26 * t250)
      t323 = t321 ** 2
      t338 = (-0.90D2 * t23 * (t253 * t254 - t256 * t257 / 0.2D1 - t260)
     # + 0.180D3 * t47 * t22 * (-t254 + t253 * t257) - t270) * t52 * t54
     # / 0.720D3 - (-0.90D2 * t23 * (t254 - t277 * t257) + 0.180D3 * t47
     # * t269) * t52 * t56 / 0.720D3 - (t81 * t22 * (t254 - t292 * t257)
     # - 0.90D2 * t23 * (t297 * t254 / 0.2D1 + t300 - t297 * t292 * t257
     # / 0.6D1 - t292 * t260) + t201 * t269 + 0.180D3 * t47 * t22 * (-t2
     #92 * t254 + t297 * t257 / 0.2D1 + t260)) * t52 / 0.1440D4 - (-0.90
     #D2 * t23 * (-t321 * t254 + t323 * t257 / 0.2D1 + t260) + 0.180D3 *
     # t47 * t22 * (t254 - t321 * t257) + t270) * t52 * t55 / 0.1440D4
      t339 = FJET(XB1, XB2, s, 0.0D0, -t248, 0.0D0, t249, 0.0D0, t338)
      t341 = FJET(XB1, XB2, s, 0.0D0, -t102, 0.0D0, t100, 0.0D0, t147)
      t343 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t150, t10, 0.0D0, t245)
      t346 = x2 * s * t1
      t347 = t12 * t1
      t348 = t131 * t11
      t351 = log(-0.4D1 * t26 * t348)
      t352 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t354 = t351 ** 2
      t355 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t358 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t367 = t22 * t355
      t368 = t81 * t367
      t373 = x2 * t30
      t376 = log(-0.4D1 * t373 * t104)
      t381 = t376 ** 2
      t384 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t405 = log(-0.4D1 * t130 * t348)
      t418 = log(-0.4D1 * t58 * t348)
      t420 = t418 ** 2
      t435 = -(-0.90D2 * t23 * (-t351 * t352 + t354 * t355 / 0.2D1 + t35
     #8) + 0.180D3 * t47 * t22 * (t352 - t351 * t355) + t368) * t52 * t5
     #5 / 0.1440D4 - (t81 * t22 * (t352 - t376 * t355) - 0.90D2 * t23 * 
     #(t381 * t352 / 0.2D1 + t384 - t381 * t376 * t355 / 0.6D1 - t376 * 
     #t358) + t201 * t367 + 0.180D3 * t47 * t22 * (-t376 * t352 + t381 *
     # t355 / 0.2D1 + t358)) * t55 / 0.1440D4 - (-0.90D2 * t23 * (t352 -
     # t405 * t355) + 0.180D3 * t47 * t367) * t52 * t56 / 0.720D3 - (-0.
     #90D2 * t23 * (-t418 * t352 + t420 * t355 / 0.2D1 + t358) + 0.180D3
     # * t47 * t22 * (t352 - t418 * t355) + t368) * t54 * t55 / 0.720D3
      t436 = FJET(XB1, XB2, s, 0.0D0, t346, 0.0D0, -t347, 0.0D0, t435)
      t438 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t441 = log(0.4D1 * t288 * t34)
      t442 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t447 = t441 ** 2
      t450 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t454 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t459 = t22 * t442
      t460 = t201 * t459
      t472 = log(0.4D1 * t131)
      t473 = t472 ** 2
      t474 = t473 * 0.3141592653589793D1
      t478 = t473 * t472 * 0.3141592653589793D1
      t480 = t472 * 0.3141592653589793D1
      t499 = rrgg2ggh91J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t508 = t78 ** 2
      t509 = t76 ** 2
      t515 = t473 ** 2
      t524 = log(0.4D1 * t151 * t131)
      t526 = t524 ** 2
      t537 = t81 * t459
      t542 = t31 * t34
      t544 = log(0.4D1 * t542)
      t549 = t544 ** 2
      t571 = log(0.4D1 * t26 * t542)
      t584 = log(0.4D1 * t58 * t131)
      t586 = t584 ** 2
      t603 = log(0.4D1 * t26 * t131)
      t605 = t603 ** 2
      t622 = log(0.4D1 * t373 * t34)
      t627 = t622 ** 2
      t647 = -(t81 * t22 * (-t438 + t441 * t442) - 0.90D2 * t23 * (-t447
     # * t438 / 0.2D1 - t450 + t447 * t441 * t442 / 0.6D1 + t441 * t454)
     # - t460 + 0.180D3 * t47 * t22 * (t441 * t438 - t447 * t442 / 0.2D1
     # - t454)) * t52 / 0.1440D4 + (0.90D2 * t474 * lh + t201 + 0.15D2 *
     # t478 - t480 * t80) * t22 * t438 / 0.1440D4 + (-0.180D3 * t480 * l
     #h - 0.45D2 * t474 + t81) * t22 * t454 / 0.1440D4 + (0.180D3 * t47 
     #+ 0.90D2 * t480) * t22 * t450 / 0.1440D4 - t23 * t499 / 0.16D2 + (
     #-0.30D2 * t478 * lh + t474 * t80 / 0.2D1 - t480 * t200 + 0.3141592
     #653589793D1 * (-0.5769873135166051D3 * lh - t508 - 0.60D2 * t509 +
     # 0.60D2 * t76 * t78) - 0.15D2 / 0.4D1 * t515 * 0.3141592653589793D
     #1) * t22 * t442 / 0.1440D4 + (-0.90D2 * t23 * (-t524 * t438 + t526
     # * t442 / 0.2D1 + t454) + 0.180D3 * t47 * t22 * (t438 - t524 * t44
     #2) + t537) * t52 * t54 / 0.720D3 + (t81 * t22 * (t438 - t544 * t44
     #2) - 0.90D2 * t23 * (t549 * t438 / 0.2D1 + t450 - t549 * t544 * t4
     #42 / 0.6D1 - t544 * t454) + t460 + 0.180D3 * t47 * t22 * (-t544 * 
     #t438 + t549 * t442 / 0.2D1 + t454)) * t54 / 0.720D3 - (-0.90D2 * t
     #23 * (-t438 + t571 * t442) - 0.180D3 * t47 * t459) * t52 * t56 / 0
     #.720D3 - (-0.90D2 * t23 * (t584 * t438 - t586 * t442 / 0.2D1 - t45
     #4) + 0.180D3 * t47 * t22 * (-t438 + t584 * t442) - t537) * t54 * t
     #55 / 0.720D3 - (-0.90D2 * t23 * (t603 * t438 - t605 * t442 / 0.2D1
     # - t454) + 0.180D3 * t47 * t22 * (-t438 + t603 * t442) - t537) * t
     #52 * t55 / 0.1440D4 - (t81 * t22 * (-t438 + t622 * t442) - 0.90D2 
     #* t23 * (-t627 * t438 / 0.2D1 - t450 + t627 * t622 * t442 / 0.6D1 
     #+ t622 * t454) - t460 + 0.180D3 * t47 * t22 * (t622 * t438 - t627 
     #* t442 / 0.2D1 - t454)) * t55 / 0.1440D4
      t648 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t647)
      t650 = FJET(XB1, XB2, s, t249, 0.0D0, -t248, 0.0D0, 0.0D0, t338)
      t652 = FJET(XB1, XB2, s, 0.0D0, -t347, 0.0D0, t346, 0.0D0, t435)
      t654 = FJET(XB1, XB2, s, -t102, 0.0D0, t100, 0.0D0, 0.0D0, t147)
      t669 = t177 / 0.720D3 + (t81 * t22 * t182 - 0.90D2 * t23 * t193 - 
     #t202 + 0.180D3 * t47 * t22 * t206) * t54 / 0.720D3 - t225 / 0.720D
     #3 - t243 / 0.720D3
      t670 = FJET(XB1, XB2, s, t10, -t150, 0.0D0, 0.0D0, 0.0D0, t669)
      t672 = FJET(XB1, XB2, s, -t150, t10, 0.0D0, 0.0D0, 0.0D0, t669)
      t674 = x3 * x1
      t675 = t674 * z
      t676 = t674 * x2
      t678 = t674 * x2 * z
      t683 = Sqrt(x3 * t11 * t6 * x2 * t93)
      t685 = 0.2D1 * t91 * t683
      t689 = t2 * t3 * (-x3 + t674 - t675 + t90 - t676 + t678 - x2 + t68
     #5) * t7
      t690 = t2 * t674
      t691 = x1 * x2
      t693 = 0.1D1 - x1 + t5 - x2 + t691 - t691 * z - x3 + t674 - t675 +
     # t90 - t676 + t678 + t685
      t696 = t2 * t3 * t693 * t7
      t697 = t93 * s
      t699 = t697 * t1 * x1
      t700 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t24, x2, x3, x4)
      t705 = log(0.4D1 * t32 * t35 * t37 * t93)
      t706 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t24, x2, x3, x4)
      t708 = t700 - t705 * t706
      t713 = 0.180D3 * t47 * t22 * t706
      t714 = -0.90D2 * t23 * t708 + t713
      t717 = t714 * t52 * t56 / 0.720D3
      t718 = FJET(XB1, XB2, s, t689, t690, -t696, -t699, -t20, -t717)
      t721 = t52 * t54 * t55
      t724 = FJET(XB1, XB2, s, t690, t689, -t699, -t696, -t20, -t717)
      t728 = FJET(XB1, XB2, s, -t699, -t696, t690, t689, -t20, -t717)
      t732 = t88 * t87 + t148 * t147 + t246 * t245 + t339 * t338 + t341 
     #* t147 + t343 * t245 + t436 * t435 + t648 * t647 + t650 * t338 + t
     #652 * t435 + t654 * t147 + t670 * t669 + t672 * t669 - t718 * t714
     # * t721 / 0.720D3 - t724 * t714 * t721 / 0.720D3 - t728 * t714 * t
     #721 / 0.720D3
      t736 = -0.90D2 * t23 * t708 + t713
      t740 = FJET(XB1, XB2, s, -t696, -t699, t689, t690, -t20, -t736 * t
     #52 * t56 / 0.720D3)
      t745 = x3 * s * t13
      t746 = t697 * t13
      t751 = log(-0.4D1 * t152 * t35 * t36 * t93)
      t752 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, x3, x4
     #)
      t754 = t751 ** 2
      t755 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, x3, x4
     #)
      t758 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, t24, 0.0D0, x3, x4
     #)
      t767 = t22 * t755
      t776 = log(-0.4D1 * t213 * t151 * x2 * t93)
      t787 = (-0.90D2 * t23 * (-t751 * t752 + t754 * t755 / 0.2D1 + t758
     #) + 0.180D3 * t47 * t22 * (t752 - t751 * t755) + t81 * t767) * t52
     # * t54 / 0.720D3 - (-0.90D2 * t23 * (-t752 + t776 * t755) - 0.180D
     #3 * t47 * t767) * t52 * t56 / 0.720D3
      t788 = FJET(XB1, XB2, s, -t745, t690, t746, -t699, 0.0D0, t787)
      t790 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t647)
      t803 = -t57 / 0.720D3 - (-0.90D2 * t23 * t68 + 0.180D3 * t47 * t22
     # * t72 - t82) * t54 * t55 / 0.720D3
      t804 = FJET(XB1, XB2, s, -t9, 0.0D0, t14, t10, -t20, t803)
      t806 = FJET(XB1, XB2, s, t14, t10, -t9, 0.0D0, -t20, t87)
      t808 = FJET(XB1, XB2, s, t346, 0.0D0, -t347, 0.0D0, 0.0D0, t435)
      t810 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t647)
      t812 = FJET(XB1, XB2, s, -t248, 0.0D0, t249, 0.0D0, 0.0D0, t338)
      t814 = FJET(XB1, XB2, s, -t699, t746, t690, -t745, 0.0D0, t787)
      t816 = FJET(XB1, XB2, s, t690, -t745, -t699, t746, 0.0D0, t787)
      t818 = FJET(XB1, XB2, s, -t347, 0.0D0, t346, 0.0D0, 0.0D0, t435)
      t820 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t647)
      t822 = FJET(XB1, XB2, s, 0.0D0, t249, 0.0D0, -t248, 0.0D0, t338)
      t824 = FJET(XB1, XB2, s, t10, t14, 0.0D0, -t9, -t20, t87)
      t826 = FJET(XB1, XB2, s, t100, 0.0D0, -t102, 0.0D0, 0.0D0, t147)
      t828 = FJET(XB1, XB2, s, t746, -t699, -t745, t690, 0.0D0, t787)
      t830 = -t740 * t736 * t721 / 0.720D3 + t788 * t787 + t790 * t647 +
     # t804 * t803 + t806 * t87 + t808 * t435 + t810 * t647 + t812 * t33
     #8 + t814 * t787 + t816 * t787 + t818 * t435 + t820 * t647 + t822 *
     # t338 + t824 * t87 + t826 * t147 + t828 * t787
      rrgg2gght9s1e1 = t732 + t830

      end function



      doubleprecision function rrgg2gght9s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * x1
      t4 = t2 * t3
      t5 = -0.1D1 + x1
      t6 = t3 * z
      t7 = x2 * x3
      t8 = 0.2D1 * t7
      t9 = t3 * x2
      t11 = t3 * x2 * z
      t12 = x4 * 0.3141592653589793D1
      t13 = cos(t12)
      t14 = -0.1D1 + x2
      t16 = x1 * z
      t17 = 0.1D1 - x1 + t16
      t19 = -0.1D1 + x3
      t22 = Sqrt(x3 * t14 * t17 * x2 * t19)
      t24 = 0.2D1 * t13 * t22
      t27 = 0.1D1 / t17
      t29 = t2 * t5 * (-x3 + t3 - t6 + t8 - t9 + t11 - x2 + t24) * t27
      t30 = t19 * s
      t32 = t30 * t1 * x1
      t33 = x1 * x2
      t35 = 0.1D1 - x1 + t16 - x2 + t33 - t33 * z - x3 + t3 - t6 + t8 - 
     #t9 + t11 + t24
      t38 = t2 * t5 * t35 * t27
      t39 = t1 ** 2
      t44 = s * t39 * x2 * x1 * t5 * t27
      t45 = s ** 2
      t46 = 0.1D1 / t45
      t47 = 0.3141592653589793D1 * t46
      t48 = -t5
      t49 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t48, x2, x3, x4)
      t51 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x2
      t55 = t51 * t52 * t54
      t57 = t47 * t49 * t55 / 0.8D1
      t58 = FJET(XB1, XB2, s, t4, t29, -t32, -t38, -t44, t57)
      t63 = t49 * t51 * t52 * t54
      t66 = FJET(XB1, XB2, s, -t32, -t38, t4, t29, -t44, t57)
      t71 = FJET(XB1, XB2, s, -t38, -t32, t29, t4, -t44, t57)
      t76 = FJET(XB1, XB2, s, t29, t4, -t38, -t32, -t44, t57)
      t81 = t2 * t19
      t82 = t2 * x3
      t83 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t84 = Sin(t12)
      t85 = t84 ** 2
      t86 = z ** 2
      t87 = 0.1D1 / t86
      t88 = t85 * t87
      t89 = t88 * t19
      t92 = log(-0.4D1 * t7 * t89)
      t93 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t98 = 0.3141592653589793D1 * lh
      t99 = t46 * t93
      t101 = 0.180D3 * t98 * t99
      t106 = x3 * t85
      t110 = log(-0.4D1 * t106 * t87 * t19)
      t112 = t110 ** 2
      t115 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t124 = lh ** 2
      t126 = 0.3141592653589793D1 ** 2
      t128 = -0.180D3 * t124 + 0.30D2 * t126
      t129 = 0.3141592653589793D1 * t128
      t134 = x1 ** 2
      t135 = x3 * t134
      t138 = log(-0.4D1 * t135 * t89)
      t150 = -(-0.90D2 * t47 * (t83 - t92 * t93) + t101) * t51 * t54 / 0
     #.1440D4 - (-0.90D2 * t47 * (-t110 * t83 + t112 * t93 / 0.2D1 + t11
     #5) + 0.180D3 * t98 * t46 * (t83 - t110 * t93) + t129 * t99) * t51 
     #/ 0.1440D4 + (-0.90D2 * t47 * (-t83 + t138 * t93) - t101) * t51 * 
     #t52 / 0.720D3 + t47 * t93 * t55 / 0.8D1
      t151 = FJET(XB1, XB2, s, -t81, 0.0D0, t82, 0.0D0, 0.0D0, t150)
      t153 = t14 * s
      t154 = t153 * t1
      t156 = x2 * s * t1
      t157 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t161 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t162 = x2 * t134
      t163 = t88 * t14
      t166 = log(-0.4D1 * t162 * t163)
      t171 = t46 * t157
      t173 = 0.180D3 * t98 * t171
      t180 = log(-0.4D1 * t7 * t163)
      t189 = x2 * t85
      t190 = t87 * t14
      t193 = log(-0.4D1 * t189 * t190)
      t195 = t193 ** 2
      t198 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t211 = t47 * t157 * t55 / 0.8D1 - (-0.90D2 * t47 * (t161 - t166 * 
     #t157) + t173) * t52 * t54 / 0.720D3 - (-0.90D2 * t47 * (t161 - t18
     #0 * t157) + t173) * t51 * t54 / 0.1440D4 - (-0.90D2 * t47 * (-t193
     # * t161 + t195 * t157 / 0.2D1 + t198) + 0.180D3 * t98 * t46 * (t16
     #1 - t193 * t157) + t129 * t171) * t54 / 0.1440D4
      t212 = FJET(XB1, XB2, s, 0.0D0, -t154, 0.0D0, t156, 0.0D0, t211)
      t216 = log(0.4D1 * t106 * t87)
      t217 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t219 = t216 ** 2
      t220 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t223 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t232 = t46 * t220
      t233 = t129 * t232
      t238 = log(0.4D1 * t88)
      t239 = t238 * 0.3141592653589793D1
      t242 = t238 ** 2
      t243 = t242 * 0.3141592653589793D1
      t249 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t276 = log(0.4D1 * t7 * t88)
      t282 = 0.180D3 * t98 * t232
      t289 = log(0.4D1 * t189 * t87)
      t291 = t289 ** 2
      t307 = log(0.4D1 * t135 * t88)
      t321 = log(0.4D1 * t162 * t88)
      t330 = t134 * t85
      t333 = log(0.4D1 * t330 * t87)
      t335 = t333 ** 2
      t349 = -(-0.90D2 * t47 * (t216 * t217 - t219 * t220 / 0.2D1 - t223
     #) + 0.180D3 * t98 * t46 * (-t217 + t216 * t220) - t233) * t51 / 0.
     #1440D4 + (-0.180D3 * t239 * lh - 0.45D2 * t243 + t129) * t46 * t21
     #7 / 0.1440D4 - t47 * t249 / 0.16D2 + (0.90D2 * t243 * lh + 0.31415
     #92653589793D1 * (-0.60D2 * lh * t126 + 0.2884936567583026D3 + 0.12
     #0D3 * t124 * lh) + 0.15D2 * t242 * t238 * 0.3141592653589793D1 - t
     #239 * t128) * t46 * t220 / 0.1440D4 + (0.180D3 * t98 + 0.90D2 * t2
     #39) * t46 * t223 / 0.1440D4 - (-0.90D2 * t47 * (-t217 + t276 * t22
     #0) - t282) * t51 * t54 / 0.1440D4 - (-0.90D2 * t47 * (t289 * t217 
     #- t291 * t220 / 0.2D1 - t223) + 0.180D3 * t98 * t46 * (-t217 + t28
     #9 * t220) - t233) * t54 / 0.1440D4 + (-0.90D2 * t47 * (t217 - t307
     # * t220) + t282) * t51 * t52 / 0.720D3 - t47 * t220 * t55 / 0.8D1 
     #- (-0.90D2 * t47 * (-t217 + t321 * t220) - t282) * t52 * t54 / 0.7
     #20D3 + (-0.90D2 * t47 * (-t333 * t217 + t335 * t220 / 0.2D1 + t223
     #) + 0.180D3 * t98 * t46 * (t217 - t333 * t220) + t233) * t52 / 0.7
     #20D3
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t349)
      t355 = Sqrt(x2 * t14 * x3 * t19)
      t357 = 0.2D1 * t13 * t355
      t359 = t2 * (-x3 + t8 - x2 + t357)
      t361 = t2 * (0.1D1 - x2 - x3 + t8 + t357)
      t362 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t366 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t371 = log(0.4D1 * t7 * t85 * t190 * t19)
      t383 = -t47 * t362 * t55 / 0.8D1 - (-0.90D2 * t47 * (-t366 + t371 
     #* t362) - 0.180D3 * t98 * t46 * t362) * t51 * t54 / 0.1440D4
      t384 = FJET(XB1, XB2, s, 0.0D0, -t359, 0.0D0, t361, 0.0D0, t383)
      t386 = FJET(XB1, XB2, s, 0.0D0, t82, 0.0D0, -t81, 0.0D0, t150)
      t388 = t2 * t5
      t389 = t2 * x1
      t390 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t48, 0.0D0, 0.0D0,
     # x4)
      t391 = t135 * t85
      t392 = t5 ** 2
      t394 = t87 * t392 * t27
      t397 = log(0.4D1 * t391 * t394)
      t398 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t48, 0.0D0, 0.0D0,
     # x4)
      t403 = t46 * t398
      t405 = 0.180D3 * t98 * t403
      t409 = (-0.90D2 * t47 * (-t390 + t397 * t398) - t405) * t51 * t52 
     #/ 0.720D3
      t412 = t47 * t398 * t55 / 0.8D1
      t413 = t162 * t85
      t416 = log(0.4D1 * t413 * t394)
      t424 = (-0.90D2 * t47 * (t390 - t416 * t398) + t405) * t52 * t54 /
     # 0.720D3
      t427 = log(0.4D1 * t330 * t394)
      t429 = t427 ** 2
      t432 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, t48, 0.0D0, 0.0D0,
     # x4)
      t433 = -t427 * t390 + t429 * t398 / 0.2D1 + t432
      t437 = t390 - t427 * t398
      t441 = t129 * t403
      t445 = t409 + t412 - t424 + (0.90D2 * t47 * t433 - 0.180D3 * t98 *
     # t46 * t437 - t441) * t52 / 0.720D3
      t446 = FJET(XB1, XB2, s, -t388, t389, 0.0D0, 0.0D0, 0.0D0, t445)
      t448 = FJET(XB1, XB2, s, 0.0D0, -t81, 0.0D0, t82, 0.0D0, t150)
      t450 = FJET(XB1, XB2, s, -t359, 0.0D0, t361, 0.0D0, 0.0D0, t383)
      t452 = FJET(XB1, XB2, s, 0.0D0, t156, 0.0D0, -t154, 0.0D0, t211)
      t456 = t2 * t5 * x2 * t27
      t457 = t1 * t5
      t458 = t153 * t457
      t459 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t48, x2, 0.0D0, x4
     #)
      t462 = t47 * t459 * t55 / 0.8D1
      t463 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t48, x2, 0.0D0, x4
     #)
      t464 = t87 * t27
      t469 = log(-0.4D1 * t413 * t464 * t392 * t14)
      t471 = t463 - t469 * t459
      t476 = 0.180D3 * t98 * t46 * t459
      t481 = -t462 - (0.90D2 * t47 * t471 - t476) * t52 * t54 / 0.720D3
      t482 = FJET(XB1, XB2, s, -t456, 0.0D0, t458, t389, -t44, t481)
      t491 = -t462 - (0.90D2 * t47 * t471 - t476) * t52 * t54 / 0.720D3
      t492 = FJET(XB1, XB2, s, t389, t458, 0.0D0, -t456, -t44, t491)
      t504 = t409 + t412 - t424 + (0.90D2 * t47 * t433 - 0.180D3 * t98 *
     # t46 * t437 - t441) * t52 / 0.720D3
      t505 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t388, t389, 0.0D0, t504)
      t507 = t58 * 0.3141592653589793D1 * t46 * t63 / 0.8D1 + t66 * 0.31
     #41592653589793D1 * t46 * t63 / 0.8D1 + t71 * 0.3141592653589793D1 
     #* t46 * t63 / 0.8D1 + t76 * 0.3141592653589793D1 * t46 * t63 / 0.8
     #D1 + t151 * t150 + t212 * t211 + t350 * t349 + t384 * t383 + t386 
     #* t150 + t446 * t445 + t448 * t150 + t450 * t383 + t452 * t211 + t
     #482 * t481 + t492 * t491 + t505 * t504
      t509 = x3 * s * t457
      t510 = t30 * t457
      t511 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t48, 0.0D0, x3, x4
     #)
      t516 = log(-0.4D1 * t391 * t464 * t392 * t19)
      t517 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t48, 0.0D0, x3, x4
     #)
      t532 = (-0.90D2 * t47 * (t511 - t516 * t517) + 0.180D3 * t98 * t46
     # * t517) * t51 * t52 / 0.720D3 - t47 * t517 * t55 / 0.8D1
      t533 = FJET(XB1, XB2, s, -t509, t4, t510, -t32, 0.0D0, t532)
      t535 = FJET(XB1, XB2, s, t389, -t388, 0.0D0, 0.0D0, 0.0D0, t445)
      t537 = FJET(XB1, XB2, s, t510, -t32, -t509, t4, 0.0D0, t532)
      t539 = FJET(XB1, XB2, s, t4, -t509, -t32, t510, 0.0D0, t532)
      t541 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t349)
      t543 = FJET(XB1, XB2, s, -t32, t510, t4, -t509, 0.0D0, t532)
      t545 = FJET(XB1, XB2, s, 0.0D0, t361, 0.0D0, -t359, 0.0D0, t383)
      t547 = FJET(XB1, XB2, s, t82, 0.0D0, -t81, 0.0D0, 0.0D0, t150)
      t549 = FJET(XB1, XB2, s, 0.0D0, -t456, t389, t458, -t44, t491)
      t551 = FJET(XB1, XB2, s, t458, t389, -t456, 0.0D0, -t44, t491)
      t553 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t389, -t388, 0.0D0, t504)
      t555 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t349)
      t557 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t349)
      t559 = FJET(XB1, XB2, s, -t154, 0.0D0, t156, 0.0D0, 0.0D0, t211)
      t561 = FJET(XB1, XB2, s, t156, 0.0D0, -t154, 0.0D0, 0.0D0, t211)
      t563 = FJET(XB1, XB2, s, t361, 0.0D0, -t359, 0.0D0, 0.0D0, t383)
      t565 = t533 * t532 + t535 * t445 + t537 * t532 + t539 * t532 + t54
     #1 * t349 + t543 * t532 + t545 * t383 + t547 * t150 + t549 * t491 +
     # t551 * t491 + t553 * t504 + t555 * t349 + t557 * t349 + t559 * t2
     #11 + t561 * t211 + t563 * t383
      rrgg2gght9s1e0 = t507 + t565

      end function



      doubleprecision function rrgg2gght9s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = s ** 2
      t20 = 0.1D1 / t19
      t21 = 0.3141592653589793D1 * t20
      t22 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4)
      t23 = 0.1D1 / x3
      t25 = 0.1D1 / x2
      t26 = t22 * t23 * t25
      t28 = t21 * t26 / 0.16D2
      t29 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, -t28)
      t34 = t2 * x1
      t35 = t7 * s
      t36 = -0.1D1 + x1
      t37 = t1 * t36
      t38 = t35 * t37
      t42 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t44 = t2 * t36 * x2 * t42
      t45 = t1 ** 2
      t50 = s * t45 * x2 * x1 * t36 * t42
      t51 = -t36
      t52 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t51, x2, 0.0D0, x4)
      t53 = 0.1D1 / x1
      t55 = t52 * t53 * t25
      t57 = t21 * t55 / 0.8D1
      t58 = FJET(XB1, XB2, s, t34, t38, 0.0D0, -t44, -t50, -t57)
      t63 = FJET(XB1, XB2, s, t18, 0.0D0, -t16, 0.0D0, 0.0D0, -t28)
      t68 = t9 * s
      t69 = t68 * t37
      t71 = t68 * t1 * x1
      t73 = x3 * s * t37
      t75 = t2 * x1 * x3
      t76 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t51, 0.0D0, x3, x4)
      t78 = t76 * t23 * t53
      t80 = t21 * t78 / 0.8D1
      t81 = FJET(XB1, XB2, s, t69, -t71, -t73, t75, 0.0D0, -t80)
      t86 = FJET(XB1, XB2, s, t75, -t73, -t71, t69, 0.0D0, -t80)
      t91 = FJET(XB1, XB2, s, t38, t34, -t44, 0.0D0, -t50, -t57)
      t96 = FJET(XB1, XB2, s, 0.0D0, t18, 0.0D0, -t16, 0.0D0, -t28)
      t101 = FJET(XB1, XB2, s, -t73, t75, t69, -t71, 0.0D0, -t80)
      t106 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, -t28)
      t111 = FJET(XB1, XB2, s, -t44, 0.0D0, t38, t34, -t50, -t57)
      t116 = FJET(XB1, XB2, s, -t71, t69, t75, -t73, 0.0D0, -t80)
      t121 = FJET(XB1, XB2, s, 0.0D0, -t44, t34, t38, -t50, -t57)
      t126 = t2 * t9
      t127 = t2 * x3
      t128 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t129 = t128 * t23
      t133 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t134 = Sin(t5)
      t135 = t134 ** 2
      t136 = x3 * t135
      t137 = z ** 2
      t138 = 0.1D1 / t137
      t142 = log(-0.4D1 * t136 * t138 * t9)
      t147 = 0.3141592653589793D1 * lh
      t157 = t21 * t129 * t25 / 0.16D2 - (-0.90D2 * t21 * (t133 - t142 *
     # t128) + 0.180D3 * t147 * t20 * t128) * t23 / 0.1440D4 + t21 * t12
     #9 * t53 / 0.8D1
      t158 = FJET(XB1, XB2, s, -t126, 0.0D0, t127, 0.0D0, 0.0D0, t157)
      t160 = t35 * t1
      t162 = x2 * s * t1
      t163 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t172 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t173 = x2 * t135
      t177 = log(-0.4D1 * t173 * t138 * t7)
      t188 = t21 * t163 * t53 * t25 / 0.8D1 + t21 * t163 * t23 * t25 / 0
     #.16D2 - (-0.90D2 * t21 * (t172 - t177 * t163) + 0.180D3 * t147 * t
     #20 * t163) * t25 / 0.1440D4
      t189 = FJET(XB1, XB2, s, -t160, 0.0D0, t162, 0.0D0, 0.0D0, t188)
      t191 = -t29 * 0.3141592653589793D1 * t20 * t26 / 0.16D2 - t58 * 0.
     #3141592653589793D1 * t20 * t55 / 0.8D1 - t63 * 0.3141592653589793D
     #1 * t20 * t26 / 0.16D2 - t81 * 0.3141592653589793D1 * t20 * t78 / 
     #0.8D1 - t86 * 0.3141592653589793D1 * t20 * t78 / 0.8D1 - t91 * 0.3
     #141592653589793D1 * t20 * t55 / 0.8D1 - t96 * 0.3141592653589793D1
     # * t20 * t26 / 0.16D2 - t101 * 0.3141592653589793D1 * t20 * t78 / 
     #0.8D1 - t106 * 0.3141592653589793D1 * t20 * t26 / 0.16D2 - t111 * 
     #0.3141592653589793D1 * t20 * t55 / 0.8D1 - t116 * 0.31415926535897
     #93D1 * t20 * t78 / 0.8D1 - t121 * 0.3141592653589793D1 * t20 * t55
     # / 0.8D1 + t158 * t157 + t189 * t188
      t192 = FJET(XB1, XB2, s, t127, 0.0D0, -t126, 0.0D0, 0.0D0, t157)
      t194 = FJET(XB1, XB2, s, 0.0D0, t127, 0.0D0, -t126, 0.0D0, t157)
      t196 = FJET(XB1, XB2, s, 0.0D0, t162, 0.0D0, -t160, 0.0D0, t188)
      t198 = t2 * t36
      t199 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, t51, 0.0D0, 0.0D0,
     # x4)
      t203 = t21 * t199 * t53 * t25 / 0.8D1
      t204 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, t51, 0.0D0, 0.0D0,
     # x4)
      t205 = x1 ** 2
      t206 = t205 * t135
      t208 = t36 ** 2
      t212 = log(0.4D1 * t206 * t138 * t42 * t208)
      t214 = -t204 + t212 * t199
      t219 = 0.180D3 * t147 * t20 * t199
      t226 = t21 * t199 * t23 * t53 / 0.8D1
      t227 = t203 + (-0.90D2 * t21 * t214 - t219) * t53 / 0.720D3 + t226
      t228 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t34, -t198, 0.0D0, t227)
      t230 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t233 = log(0.4D1 * t136 * t138)
      t234 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t241 = 0.180D3 * t147 * t20 * t234
      t248 = log(0.4D1 * t138 * t135)
      t249 = t248 * 0.3141592653589793D1
      t257 = t248 ** 2
      t260 = lh ** 2
      t262 = 0.3141592653589793D1 ** 2
      t270 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t273 = t234 * t23
      t279 = log(0.4D1 * t173 * t138)
      t293 = log(0.4D1 * t206 * t138)
      t304 = -(-0.90D2 * t21 * (-t230 + t233 * t234) - t241) * t23 / 0.1
     #440D4 + (0.180D3 * t147 + 0.90D2 * t249) * t20 * t230 / 0.1440D4 +
     # (-0.180D3 * t249 * lh - 0.45D2 * t257 * 0.3141592653589793D1 + 0.
     #3141592653589793D1 * (-0.180D3 * t260 + 0.30D2 * t262)) * t20 * t2
     #34 / 0.1440D4 - t21 * t270 / 0.16D2 - t21 * t273 * t25 / 0.16D2 - 
     #(-0.90D2 * t21 * (-t230 + t279 * t234) - t241) * t25 / 0.1440D4 - 
     #t21 * t234 * t53 * t25 / 0.8D1 + (-0.90D2 * t21 * (t230 - t293 * t
     #234) + t241) * t53 / 0.720D3 - t21 * t273 * t53 / 0.8D1
      t305 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t304)
      t313 = t203 + (-0.90D2 * t21 * t214 - t219) * t53 / 0.720D3 + t226
      t314 = FJET(XB1, XB2, s, -t198, t34, 0.0D0, 0.0D0, 0.0D0, t313)
      t316 = FJET(XB1, XB2, s, t162, 0.0D0, -t160, 0.0D0, 0.0D0, t188)
      t318 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t304)
      t320 = FJET(XB1, XB2, s, t34, -t198, 0.0D0, 0.0D0, 0.0D0, t313)
      t322 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t198, t34, 0.0D0, t227)
      t324 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t304)
      t326 = FJET(XB1, XB2, s, 0.0D0, -t126, 0.0D0, t127, 0.0D0, t157)
      t328 = FJET(XB1, XB2, s, 0.0D0, -t160, 0.0D0, t162, 0.0D0, t188)
      t330 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t304)
      t332 = t192 * t157 + t194 * t157 + t196 * t188 + t228 * t227 + t30
     #5 * t304 + t314 * t313 + t316 * t188 + t318 * t304 + t320 * t313 +
     # t322 * t227 + t324 * t304 + t326 * t157 + t328 * t188 + t330 * t3
     #04
      rrgg2gght9s1em1 = t191 + t332

      end function



      doubleprecision function rrgg2gght9s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t24 = z ** 2
      t27 = Sin(x4 * 0.3141592653589793D1)
      t28 = t27 ** 2
      t31 = log(0.4D1 / t24 * t28)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 + (0.180D3 * 0.3141592653589793D1 
     #* lh + 0.90D2 * t31 * 0.3141592653589793D1) * t4 * t6 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t44 = -0.1D1 + x1
      t45 = t2 * t44
      t47 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, -t44, 0.0D0, 0.0D0,
     # x4)
      t50 = t5 * t47 * t15 / 0.8D1
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t50)
      t54 = t4 * t47 * t15
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t50)
      t61 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t63 = t2 * x3
      t65 = t2 * (-0.1D1 + x3)
      t66 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t69 = t5 * t66 * t7 / 0.16D2
      t70 = FJET(XB1, XB2, s, 0.0D0, t63, 0.0D0, -t65, 0.0D0, t69)
      t73 = t4 * t66 * t7
      t77 = x2 * s * t1
      t80 = (-0.1D1 + x2) * s * t1
      t81 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t84 = t5 * t81 * t11 / 0.16D2
      t85 = FJET(XB1, XB2, s, 0.0D0, t77, 0.0D0, -t80, 0.0D0, t84)
      t88 = t4 * t81 * t11
      t91 = FJET(XB1, XB2, s, 0.0D0, -t65, 0.0D0, t63, 0.0D0, t69)
      t95 = FJET(XB1, XB2, s, 0.0D0, -t80, 0.0D0, t77, 0.0D0, t84)
      t99 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t101 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t50)
      t105 = FJET(XB1, XB2, s, t63, 0.0D0, -t65, 0.0D0, 0.0D0, t69)
      t109 = FJET(XB1, XB2, s, t77, 0.0D0, -t80, 0.0D0, 0.0D0, t84)
      t113 = FJET(XB1, XB2, s, -t65, 0.0D0, t63, 0.0D0, 0.0D0, t69)
      t117 = FJET(XB1, XB2, s, -t80, 0.0D0, t77, 0.0D0, 0.0D0, t84)
      t121 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t50)
      rrgg2gght9s1em2 = t39 * t38 + t41 * t38 + t51 * 0.3141592653589793
     #D1 * t54 / 0.8D1 + t57 * 0.3141592653589793D1 * t54 / 0.8D1 + t61 
     #* t38 + t70 * 0.3141592653589793D1 * t73 / 0.16D2 + t85 * 0.314159
     #2653589793D1 * t88 / 0.16D2 + t91 * 0.3141592653589793D1 * t73 / 0
     #.16D2 + t95 * 0.3141592653589793D1 * t88 / 0.16D2 + t99 * t38 + t1
     #01 * 0.3141592653589793D1 * t54 / 0.8D1 + t105 * 0.314159265358979
     #3D1 * t73 / 0.16D2 + t109 * 0.3141592653589793D1 * t88 / 0.16D2 + 
     #t113 * 0.3141592653589793D1 * t73 / 0.16D2 + t117 * 0.314159265358
     #9793D1 * t88 / 0.16D2 + t121 * 0.3141592653589793D1 * t54 / 0.8D1

      end function



      doubleprecision function rrgg2gght9s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = 0.3141592653589793D1 * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght9s1em3 = -t9 * 0.3141592653589793D1 * t11 / 0.16D2 - t13 
     #* 0.3141592653589793D1 * t11 / 0.16D2 - t16 * 0.3141592653589793D1
     # * t11 / 0.16D2 - t19 * 0.3141592653589793D1 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght9s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      rrgg2gght9s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght9s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
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
      t13 = t12 * 0.3141592653589793D1
      t16 = 0.3141592653589793D1 ** 2
      t19 = lh ** 2
      t22 = -0.60D2 * lh * t16 + 0.2884936567583026D3 + 0.120D3 * t19 * 
     #lh
      t23 = 0.3141592653589793D1 * t22
      t25 = t12 * t11 * 0.3141592653589793D1
      t27 = t11 * 0.3141592653589793D1
      t30 = -0.180D3 * t19 + 0.30D2 * t16
      t33 = s ** 2
      t34 = 0.1D1 / t33
      t36 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t42 = 0.3141592653589793D1 * t30
      t45 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t48 = 0.3141592653589793D1 * lh
      t53 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t56 = 0.3141592653589793D1 * t34
      t57 = rrgg2ggh91J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t66 = t16 ** 2
      t67 = t19 ** 2
      t73 = t12 ** 2
      t78 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t81 = x1 ** 2
      t82 = x3 * t81
      t85 = log(0.4D1 * t82 * t9)
      t87 = t85 ** 2
      t98 = t34 * t78
      t101 = 0.1D1 / x3
      t103 = 0.1D1 / x1
      t106 = t81 * t8
      t107 = t106 * t5
      t109 = log(0.4D1 * t107)
      t114 = t109 ** 2
      t124 = t23 * t98
      t135 = x2 * x3
      t138 = log(0.4D1 * t135 * t107)
      t140 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t141 = t135 * t81
      t142 = -0.1D1 + x2
      t143 = t9 * t142
      t146 = log(-0.4D1 * t141 * t143)
      t147 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t152 = -t147 + t78
      t153 = t34 * t152
      t158 = 0.1D1 / x2
      t159 = t103 * t158
      t162 = x2 * t81
      t165 = log(0.4D1 * t162 * t9)
      t169 = log(-0.4D1 * t162 * t143)
      t171 = t165 ** 2
      t174 = t169 ** 2
      t177 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t192 = x3 * t5
      t195 = log(0.4D1 * t192 * t8)
      t200 = t195 ** 2
      t222 = log(0.4D1 * t135 * t9)
      t224 = t222 ** 2
      t229 = log(-0.4D1 * t135 * t143)
      t231 = t229 ** 2
      t244 = -t34 * t152
      t250 = x2 * t5
      t253 = log(0.4D1 * t250 * t8)
      t255 = t8 * t142
      t258 = log(-0.4D1 * t250 * t255)
      t263 = t258 ** 2
      t266 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t271 = t253 ** 2
      t295 = (0.90D2 * t13 * lh + t23 + 0.15D2 * t25 - t27 * t30) * t34 
     #* t36 / 0.1440D4 + (-0.180D3 * t27 * lh - 0.45D2 * t13 + t42) * t3
     #4 * t45 / 0.1440D4 + (0.180D3 * t48 + 0.90D2 * t27) * t34 * t53 / 
     #0.1440D4 - t56 * t57 / 0.16D2 + (-0.30D2 * t25 * lh + t13 * t30 / 
     #0.2D1 - t27 * t22 + 0.3141592653589793D1 * (-0.5769873135166051D3 
     #* lh - t66 - 0.60D2 * t67 + 0.60D2 * t19 * t16) - 0.15D2 / 0.4D1 *
     # t73 * 0.3141592653589793D1) * t34 * t78 / 0.1440D4 - (-0.90D2 * t
     #56 * (t85 * t36 - t87 * t78 / 0.2D1 - t45) + 0.180D3 * t48 * t34 *
     # (-t36 + t85 * t78) - t42 * t98) * t101 * t103 / 0.720D3 - (t42 * 
     #t34 * (-t36 + t109 * t78) - 0.90D2 * t56 * (-t114 * t36 / 0.2D1 - 
     #t53 + t114 * t109 * t78 / 0.6D1 + t109 * t45) - t124 + 0.180D3 * t
     #48 * t34 * (t109 * t36 - t114 * t78 / 0.2D1 - t45)) * t103 / 0.720
     #D3 + (-0.90D2 * t56 * (-t138 * t78 - t140 + t146 * t147 + t36) + 0
     #.180D3 * t48 * t153) * t101 * t159 / 0.720D3 + (-0.90D2 * t56 * (-
     #t165 * t36 + t169 * t140 + t171 * t78 / 0.2D1 - t174 * t147 / 0.2D
     #1 + t45 - t177) + 0.180D3 * t48 * t34 * (t36 + t169 * t147 - t165 
     #* t78 - t140) + t42 * t153) * t103 * t158 / 0.720D3 + (t42 * t34 *
     # (t36 - t195 * t78) - 0.90D2 * t56 * (t200 * t36 / 0.2D1 + t53 - t
     #200 * t195 * t78 / 0.6D1 - t195 * t45) + t124 + 0.180D3 * t48 * t3
     #4 * (-t195 * t36 + t200 * t78 / 0.2D1 + t45)) * t101 / 0.1440D4 - 
     #(-0.90D2 * t56 * (t222 * t36 - t224 * t78 / 0.2D1 - t45 - t229 * t
     #140 + t231 * t147 / 0.2D1 + t177) + 0.180D3 * t48 * t34 * (-t36 + 
     #t222 * t78 + t140 - t229 * t147) + t42 * t244) * t101 * t158 / 0.1
     #440D4 - (t42 * t34 * (-t36 + t253 * t78 + t140 - t258 * t147) - 0.
     #90D2 * t56 * (t263 * t140 / 0.2D1 + t266 - t263 * t258 * t147 / 0.
     #6D1 - t258 * t177 - t271 * t36 / 0.2D1 - t53 + t271 * t253 * t78 /
     # 0.6D1 + t253 * t45) + t23 * t244 + 0.180D3 * t48 * t34 * (t253 * 
     #t36 - t271 * t78 / 0.2D1 - t45 - t258 * t140 + t263 * t147 / 0.2D1
     # + t177)) * t158 / 0.1440D4
      t296 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t295)
      t298 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t295)
      t300 = t2 * x1
      t301 = -0.1D1 + x1
      t302 = t2 * t301
      t303 = t82 * t8
      t304 = 0.1D1 / t3
      t305 = t301 ** 2
      t307 = x1 * z
      t308 = -z - x1 + t307
      t309 = 0.1D1 / t308
      t310 = t304 * t305 * t309
      t313 = log(-0.4D1 * t303 * t310)
      t314 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t316 = t313 ** 2
      t317 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t320 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t329 = t34 * t317
      t330 = t42 * t329
      t336 = log(-0.4D1 * t106 * t310)
      t341 = t336 ** 2
      t344 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t364 = t8 * t304 * t305 * t309
      t367 = log(-0.4D1 * t141 * t364)
      t377 = t162 * t8
      t380 = log(-0.4D1 * t377 * t310)
      t382 = t380 ** 2
      t397 = -(-0.90D2 * t56 * (-t314 * t313 + t316 * t317 / 0.2D1 + t32
     #0) + 0.180D3 * t48 * t34 * (t314 - t313 * t317) + t330) * t101 * t
     #103 / 0.720D3 - (t42 * t34 * (t314 - t336 * t317) - 0.90D2 * t56 *
     # (t341 * t314 / 0.2D1 + t344 - t341 * t336 * t317 / 0.6D1 - t336 *
     # t320) + t23 * t329 + 0.180D3 * t48 * t34 * (-t336 * t314 + t341 *
     # t317 / 0.2D1 + t320)) * t103 / 0.720D3 + (-0.90D2 * t56 * (-t314 
     #+ t367 * t317) - 0.180D3 * t48 * t329) * t101 * t159 / 0.720D3 + (
     #-0.90D2 * t56 * (t380 * t314 - t382 * t317 / 0.2D1 - t320) + 0.180
     #D3 * t48 * t34 * (-t314 + t380 * t317) - t330) * t103 * t158 / 0.7
     #20D3
      t398 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t300, -t302, 0.0D0, t397)
      t400 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t302, t300, 0.0D0, t397)
      t402 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t295)
      t404 = t2 * x3
      t405 = -0.1D1 + x3
      t406 = t2 * t405
      t407 = t9 * t405
      t410 = log(-0.4D1 * t82 * t407)
      t411 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t413 = t410 ** 2
      t414 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t417 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t426 = t34 * t414
      t434 = log(-0.4D1 * t141 * t407)
      t440 = log(0.4D1 * t141 * t9 * t142 * t405)
      t441 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t443 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t447 = t441 - t414
      t459 = log(0.4D1 * t135 * t5 * t255 * t405)
      t463 = log(-0.4D1 * t135 * t407)
      t465 = t463 ** 2
      t468 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t469 = t459 ** 2
      t491 = log(-0.4D1 * t192 * t8 * t405)
      t496 = t491 ** 2
      t499 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t518 = -(-0.90D2 * t56 * (-t410 * t411 + t413 * t414 / 0.2D1 + t41
     #7) + 0.180D3 * t48 * t34 * (t411 - t410 * t414) + t42 * t426) * t1
     #01 * t103 / 0.720D3 + (-0.90D2 * t56 * (-t411 + t434 * t414 - t440
     # * t441 + t443) + 0.180D3 * t48 * t34 * t447) * t101 * t159 / 0.72
     #0D3 - (-0.90D2 * t56 * (t459 * t443 - t463 * t411 + t465 * t414 / 
     #0.2D1 + t417 - t468 - t469 * t441 / 0.2D1) + 0.180D3 * t48 * t34 *
     # (t411 - t463 * t414 - t443 + t459 * t441) - t42 * t34 * t447) * t
     #101 * t158 / 0.1440D4 + (t42 * t34 * (-t411 + t491 * t414) - 0.90D
     #2 * t56 * (-t496 * t411 / 0.2D1 - t499 + t496 * t491 * t414 / 0.6D
     #1 + t491 * t417) - t23 * t426 + 0.180D3 * t48 * t34 * (t491 * t411
     # - t496 * t414 / 0.2D1 - t417)) * t101 / 0.1440D4
      t519 = FJET(XB1, XB2, s, 0.0D0, t404, 0.0D0, -t406, 0.0D0, t518)
      t521 = FJET(XB1, XB2, s, 0.0D0, -t406, 0.0D0, t404, 0.0D0, t518)
      t523 = x1 * x2
      t525 = t2 * t523 * t309
      t527 = t1 * x1
      t528 = t142 * s * t527
      t529 = t1 ** 2
      t534 = s * t529 * x2 * x1 * t301 * t309
      t535 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t536 = t135 * t106
      t537 = t304 * t309
      t538 = t305 * t142
      t539 = t537 * t538
      t542 = log(0.4D1 * t536 * t539)
      t543 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t548 = t34 * t543
      t556 = log(0.4D1 * t377 * t539)
      t558 = t556 ** 2
      t561 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t575 = (-0.90D2 * t56 * (t535 - t542 * t543) + 0.180D3 * t48 * t54
     #8) * t101 * t159 / 0.720D3 + (-0.90D2 * t56 * (-t556 * t535 + t558
     # * t543 / 0.2D1 + t561) + 0.180D3 * t48 * t34 * (t535 - t556 * t54
     #3) + t42 * t548) * t103 * t158 / 0.720D3
      t576 = FJET(XB1, XB2, s, 0.0D0, -t525, -t302, -t528, t534, t575)
      t578 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t295)
      t580 = FJET(XB1, XB2, s, t300, -t302, 0.0D0, 0.0D0, 0.0D0, t397)
      t582 = FJET(XB1, XB2, s, t404, 0.0D0, -t406, 0.0D0, 0.0D0, t518)
      t584 = x3 * x1
      t585 = t2 * t584
      t587 = t1 * t301
      t588 = x3 * s * t587
      t589 = t405 * s
      t590 = t589 * t527
      t591 = t589 * t587
      t596 = log(0.4D1 * t303 * t537 * t305 * t405)
      t597 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t599 = t596 ** 2
      t600 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t603 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t604 = t596 * t597 - t599 * t600 / 0.2D1 - t603
      t608 = -t597 + t596 * t600
      t612 = t34 * t600
      t613 = t42 * t612
      t621 = log(0.4D1 * t364 * t82 * x2 * t405)
      t630 = (-0.90D2 * t56 * (t597 - t621 * t600) + 0.180D3 * t48 * t61
     #2) * t101 * t159
      t632 = -(-0.90D2 * t56 * t604 + 0.180D3 * t48 * t34 * t608 - t613)
     # * t101 * t103 / 0.720D3 + t630 / 0.720D3
      t633 = FJET(XB1, XB2, s, t585, -t588, -t590, t591, 0.0D0, t632)
      t635 = t296 * t295 + t298 * t295 + t397 * t398 + t400 * t397 + t40
     #2 * t295 + t519 * t518 + t521 * t518 + t576 * t575 + t578 * t295 +
     # t580 * t397 + t582 * t518 + t633 * t632
      t636 = FJET(XB1, XB2, s, t591, -t590, -t588, t585, 0.0D0, t632)
      t638 = x2 * z
      t640 = x3 * z
      t641 = t584 * z
      t642 = t135 * z
      t643 = t584 * x2
      t644 = t584 * t638
      t645 = cos(t6)
      t650 = Sqrt(-x3 * t142 * t308 * x2 * t405)
      t652 = 0.2D1 * t645 * t650
      t653 = z + x1 - t307 - t638 - t523 + t523 * z - t640 - t584 + t641
     # + t642 + t643 - t644 + t135 + t652
      t656 = t2 * x1 * t653 * t309
      t660 = t2 * x1 * (-t640 - t584 + t641 + t642 + t643 - t644 - x2 + 
     #t135 + t652) * t309
      t661 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t666 = log(-0.4D1 * t536 * t537 * t538 * t405)
      t667 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t669 = -t661 + t666 * t667
      t674 = 0.180D3 * t48 * t34 * t667
      t675 = -0.90D2 * t56 * t669 - t674
      t678 = t675 * t101 * t159 / 0.720D3
      t679 = FJET(XB1, XB2, s, t591, -t656, -t588, t660, t534, t678)
      t682 = t101 * t103 * t158
      t685 = FJET(XB1, XB2, s, t660, -t588, -t656, t591, t534, t678)
      t689 = FJET(XB1, XB2, s, -t302, t300, 0.0D0, 0.0D0, 0.0D0, t397)
      t691 = FJET(XB1, XB2, s, -t302, -t528, 0.0D0, -t525, t534, t575)
      t693 = FJET(XB1, XB2, s, -t406, 0.0D0, t404, 0.0D0, 0.0D0, t518)
      t706 = -(-0.90D2 * t56 * t604 + 0.180D3 * t48 * t34 * t608 - t613)
     # * t101 * t103 / 0.720D3 + t630 / 0.720D3
      t707 = FJET(XB1, XB2, s, -t588, t585, t591, -t590, 0.0D0, t706)
      t709 = FJET(XB1, XB2, s, -t588, t660, t591, -t656, t534, t678)
      t713 = FJET(XB1, XB2, s, -t528, -t302, -t525, 0.0D0, t534, t575)
      t715 = FJET(XB1, XB2, s, -t590, t591, t585, -t588, 0.0D0, t706)
      t717 = FJET(XB1, XB2, s, -t525, 0.0D0, -t528, -t302, t534, t575)
      t722 = -0.90D2 * t56 * t669 - t674
      t726 = FJET(XB1, XB2, s, -t656, t591, t660, -t588, t534, t722 * t1
     #01 * t159 / 0.720D3)
      t730 = t636 * t632 + t679 * t675 * t682 / 0.720D3 + t685 * t675 * 
     #t682 / 0.720D3 + t689 * t397 + t691 * t575 + t693 * t518 + t707 * 
     #t706 + t709 * t675 * t682 / 0.720D3 + t713 * t575 + t715 * t706 + 
     #t717 * t575 + t726 * t722 * t682 / 0.720D3
      rrgg2gght9s2e1 = t635 + t730

      end function



      doubleprecision function rrgg2gght9s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = z ** 2
      t8 = 0.1D1 / t6 / z
      t9 = x3 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t15 = log(0.4D1 * t9 * t12)
      t16 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t18 = t15 ** 2
      t19 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t22 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t26 = 0.3141592653589793D1 * lh
      t32 = lh ** 2
      t34 = 0.3141592653589793D1 ** 2
      t36 = -0.180D3 * t32 + 0.30D2 * t34
      t37 = 0.3141592653589793D1 * t36
      t38 = t4 * t19
      t39 = t37 * t38
      t41 = 0.1D1 / x3
      t44 = t8 * t12
      t46 = log(0.4D1 * t44)
      t47 = t46 * 0.3141592653589793D1
      t50 = t46 ** 2
      t51 = t50 * 0.3141592653589793D1
      t57 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t82 = x2 * x3
      t85 = log(0.4D1 * t82 * t44)
      t87 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t88 = -0.1D1 + x2
      t89 = t44 * t88
      t92 = log(-0.4D1 * t82 * t89)
      t93 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t98 = -t19 + t93
      t99 = t4 * t98
      t104 = 0.1D1 / x2
      t107 = x2 * t8
      t110 = log(0.4D1 * t107 * t12)
      t112 = t110 ** 2
      t115 = t12 * t88
      t118 = log(-0.4D1 * t107 * t115)
      t120 = t118 ** 2
      t123 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t137 = x1 ** 2
      t138 = x3 * t137
      t141 = log(0.4D1 * t138 * t44)
      t150 = 0.1D1 / x1
      t153 = -t98
      t156 = t41 * t150 * t104
      t159 = x2 * t137
      t162 = log(-0.4D1 * t159 * t89)
      t166 = log(0.4D1 * t159 * t44)
      t178 = t137 * t12
      t181 = log(0.4D1 * t178 * t8)
      t183 = t181 ** 2
      t197 = (-0.90D2 * t5 * (-t15 * t16 + t18 * t19 / 0.2D1 + t22) + 0.
     #180D3 * t26 * t4 * (t16 - t15 * t19) + t39) * t41 / 0.1440D4 + (-0
     #.180D3 * t47 * lh - 0.45D2 * t51 + t37) * t4 * t16 / 0.1440D4 - t5
     # * t57 / 0.16D2 + (0.90D2 * t51 * lh + 0.3141592653589793D1 * (-0.
     #60D2 * lh * t34 + 0.2884936567583026D3 + 0.120D3 * t32 * lh) + 0.1
     #5D2 * t50 * t46 * 0.3141592653589793D1 - t47 * t36) * t4 * t19 / 0
     #.1440D4 + (0.180D3 * t26 + 0.90D2 * t47) * t4 * t22 / 0.1440D4 - (
     #-0.90D2 * t5 * (-t16 + t85 * t19 + t87 - t92 * t93) + 0.180D3 * t2
     #6 * t99) * t41 * t104 / 0.1440D4 - (-0.90D2 * t5 * (t110 * t16 - t
     #112 * t19 / 0.2D1 - t22 - t118 * t87 + t120 * t93 / 0.2D1 + t123) 
     #+ 0.180D3 * t26 * t4 * (-t16 + t110 * t19 + t87 - t118 * t93) + t3
     #7 * t99) * t104 / 0.1440D4 - (-0.90D2 * t5 * (-t16 + t141 * t19) -
     # 0.180D3 * t26 * t38) * t41 * t150 / 0.720D3 - t5 * t153 * t156 / 
     #0.8D1 + (-0.90D2 * t5 * (t16 + t162 * t93 - t166 * t19 - t87) + 0.
     #180D3 * t26 * t4 * t153) * t150 * t104 / 0.720D3 - (-0.90D2 * t5 *
     # (t181 * t16 - t183 * t19 / 0.2D1 - t22) + 0.180D3 * t26 * t4 * (-
     #t16 + t181 * t19) - t39) * t150 / 0.720D3
      t198 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t197)
      t200 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t197)
      t202 = t2 * x1
      t203 = -0.1D1 + x1
      t204 = t2 * t203
      t205 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t206 = t138 * t12
      t207 = 0.1D1 / t6
      t208 = t203 ** 2
      t210 = x1 * z
      t211 = -z - x1 + t210
      t212 = 0.1D1 / t211
      t213 = t207 * t208 * t212
      t216 = log(-0.4D1 * t206 * t213)
      t217 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t222 = t4 * t217
      t224 = 0.180D3 * t26 * t222
      t232 = t159 * t12
      t235 = log(-0.4D1 * t232 * t213)
      t246 = log(-0.4D1 * t178 * t213)
      t248 = t246 ** 2
      t251 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t264 = -(-0.90D2 * t5 * (t205 - t216 * t217) + t224) * t41 * t150 
     #/ 0.720D3 + t5 * t217 * t156 / 0.8D1 + (-0.90D2 * t5 * (-t205 + t2
     #35 * t217) - t224) * t150 * t104 / 0.720D3 - (-0.90D2 * t5 * (-t24
     #6 * t205 + t248 * t217 / 0.2D1 + t251) + 0.180D3 * t26 * t4 * (t20
     #5 - t246 * t217) + t37 * t222) * t150 / 0.720D3
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t202, -t204, 0.0D0, t264)
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t204, t202, 0.0D0, t264)
      t269 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t197)
      t271 = t2 * x3
      t272 = -0.1D1 + x3
      t273 = t2 * t272
      t277 = log(-0.4D1 * t9 * t12 * t272)
      t278 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t280 = t277 ** 2
      t281 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t284 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t293 = t4 * t281
      t298 = t44 * t272
      t301 = log(-0.4D1 * t82 * t298)
      t303 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t308 = log(0.4D1 * t82 * t8 * t115 * t272)
      t309 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t314 = -t309 + t281
      t324 = log(-0.4D1 * t138 * t298)
      t339 = (-0.90D2 * t5 * (t277 * t278 - t280 * t281 / 0.2D1 - t284) 
     #+ 0.180D3 * t26 * t4 * (-t278 + t277 * t281) - t37 * t293) * t41 /
     # 0.1440D4 - (-0.90D2 * t5 * (t278 - t301 * t281 - t303 + t308 * t3
     #09) + 0.180D3 * t26 * t4 * t314) * t41 * t104 / 0.1440D4 - (-0.90D
     #2 * t5 * (t278 - t324 * t281) + 0.180D3 * t26 * t293) * t41 * t150
     # / 0.720D3 + t5 * t314 * t156 / 0.8D1
      t340 = FJET(XB1, XB2, s, 0.0D0, t271, 0.0D0, -t273, 0.0D0, t339)
      t342 = FJET(XB1, XB2, s, 0.0D0, -t273, 0.0D0, t271, 0.0D0, t339)
      t344 = x1 * x2
      t346 = t2 * t344 * t212
      t348 = t1 * x1
      t349 = t88 * s * t348
      t350 = t1 ** 2
      t355 = s * t350 * x2 * x1 * t203 * t212
      t356 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t360 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t361 = t207 * t212
      t366 = log(0.4D1 * t232 * t361 * t208 * t88)
      t378 = -t5 * t356 * t156 / 0.8D1 + (-0.90D2 * t5 * (t360 - t366 * 
     #t356) + 0.180D3 * t26 * t4 * t356) * t150 * t104 / 0.720D3
      t379 = FJET(XB1, XB2, s, 0.0D0, -t346, -t204, -t349, t355, t378)
      t381 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t197)
      t383 = FJET(XB1, XB2, s, t202, -t204, 0.0D0, 0.0D0, 0.0D0, t264)
      t385 = FJET(XB1, XB2, s, t271, 0.0D0, -t273, 0.0D0, 0.0D0, t339)
      t387 = x3 * x1
      t388 = t2 * t387
      t390 = t1 * t203
      t391 = x3 * s * t390
      t392 = t272 * s
      t393 = t392 * t348
      t394 = t392 * t390
      t395 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t400 = log(0.4D1 * t206 * t361 * t208 * t272)
      t401 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t403 = -t395 + t400 * t401
      t408 = 0.180D3 * t26 * t4 * t401
      t415 = t5 * t401 * t156 / 0.8D1
      t416 = -(-0.90D2 * t5 * t403 - t408) * t41 * t150 / 0.720D3 - t415
      t417 = FJET(XB1, XB2, s, t388, -t391, -t393, t394, 0.0D0, t416)
      t419 = t198 * t197 + t200 * t197 + t265 * t264 + t267 * t264 + t26
     #9 * t197 + t340 * t339 + t342 * t339 + t379 * t378 + t381 * t197 +
     # t383 * t264 + t385 * t339 + t417 * t416
      t420 = FJET(XB1, XB2, s, t394, -t393, -t391, t388, 0.0D0, t416)
      t422 = x2 * z
      t424 = x3 * z
      t425 = t387 * z
      t426 = t82 * z
      t427 = t387 * x2
      t428 = t387 * t422
      t429 = cos(t10)
      t434 = Sqrt(-x3 * t88 * t211 * x2 * t272)
      t436 = 0.2D1 * t429 * t434
      t437 = z + x1 - t210 - t422 - t344 + t344 * z - t424 - t387 + t425
     # + t426 + t427 - t428 + t82 + t436
      t440 = t2 * x1 * t437 * t212
      t444 = t2 * x1 * (-t424 - t387 + t425 + t426 + t427 - t428 - x2 + 
     #t82 + t436) * t212
      t445 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t448 = t5 * t445 * t156 / 0.8D1
      t449 = FJET(XB1, XB2, s, t394, -t440, -t391, t444, t355, t448)
      t454 = t445 * t41 * t150 * t104
      t457 = FJET(XB1, XB2, s, t444, -t391, -t440, t394, t355, t448)
      t462 = FJET(XB1, XB2, s, -t204, t202, 0.0D0, 0.0D0, 0.0D0, t264)
      t464 = FJET(XB1, XB2, s, -t204, -t349, 0.0D0, -t346, t355, t378)
      t466 = FJET(XB1, XB2, s, -t273, 0.0D0, t271, 0.0D0, 0.0D0, t339)
      t475 = -(-0.90D2 * t5 * t403 - t408) * t41 * t150 / 0.720D3 - t415
      t476 = FJET(XB1, XB2, s, -t391, t388, t394, -t393, 0.0D0, t475)
      t478 = FJET(XB1, XB2, s, -t391, t444, t394, -t440, t355, t448)
      t483 = FJET(XB1, XB2, s, -t349, -t204, -t346, 0.0D0, t355, t378)
      t485 = FJET(XB1, XB2, s, -t393, t394, t388, -t391, 0.0D0, t475)
      t487 = FJET(XB1, XB2, s, -t346, 0.0D0, -t349, -t204, t355, t378)
      t489 = FJET(XB1, XB2, s, -t440, t394, t444, -t391, t355, t448)
      t494 = t420 * t416 + t449 * 0.3141592653589793D1 * t4 * t454 / 0.8
     #D1 + t457 * 0.3141592653589793D1 * t4 * t454 / 0.8D1 + t462 * t264
     # + t464 * t378 + t466 * t339 + t476 * t475 + t478 * 0.314159265358
     #9793D1 * t4 * t454 / 0.8D1 + t483 * t378 + t485 * t475 + t487 * t3
     #78 + t489 * 0.3141592653589793D1 * t4 * t454 / 0.8D1
      rrgg2gght9s2e0 = t419 + t494

      end function



      doubleprecision function rrgg2gght9s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * 0.3141592653589793D1)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t22 = 0.3141592653589793D1 * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t33 = log(0.4D1 * t9 * t13)
      t34 = t33 * 0.3141592653589793D1
      t42 = t33 ** 2
      t45 = lh ** 2
      t47 = 0.3141592653589793D1 ** 2
      t55 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t58 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t59 = -t17 + t58
      t61 = 0.1D1 / x2
      t65 = x2 * t9
      t68 = log(0.4D1 * t65 * t13)
      t70 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t71 = -0.1D1 + x2
      t75 = log(-0.4D1 * t65 * t13 * t71)
      t87 = 0.1D1 / x1
      t92 = x1 ** 2
      t93 = t92 * t13
      t96 = log(0.4D1 * t93 * t9)
      t108 = (-0.90D2 * t5 * (t6 - t16 * t17) + t25) * t27 / 0.1440D4 + 
     #(0.180D3 * t22 + 0.90D2 * t34) * t4 * t6 / 0.1440D4 + (-0.180D3 * 
     #lh * t34 - 0.45D2 * t42 * 0.3141592653589793D1 + 0.314159265358979
     #3D1 * (-0.180D3 * t45 + 0.30D2 * t47)) * t4 * t17 / 0.1440D4 - t5 
     #* t55 / 0.16D2 + t5 * t59 * t27 * t61 / 0.16D2 - (-0.90D2 * t5 * (
     #-t6 + t68 * t17 + t70 - t75 * t58) + 0.180D3 * t22 * t4 * t59) * t
     #61 / 0.1440D4 + t5 * t59 * t87 * t61 / 0.8D1 - (-0.90D2 * t5 * (-t
     #6 + t96 * t17) - t25) * t87 / 0.720D3 - t5 * t17 * t27 * t87 / 0.8
     #D1
      t109 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t108)
      t111 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t108)
      t113 = t2 * x1
      t114 = -0.1D1 + x1
      t115 = t2 * t114
      t116 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t121 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t125 = 0.1D1 / (-z - x1 + x1 * z)
      t127 = t114 ** 2
      t131 = log(-0.4D1 * t93 / t7 * t125 * t127)
      t146 = t5 * t116 * t87 * t61 / 0.8D1 - (-0.90D2 * t5 * (t121 - t13
     #1 * t116) + 0.180D3 * t22 * t4 * t116) * t87 / 0.720D3 + t5 * t116
     # * t27 * t87 / 0.8D1
      t147 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t113, -t115, 0.0D0, t146)
      t149 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t115, t113, 0.0D0, t146)
      t151 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t108)
      t153 = t2 * x3
      t154 = -0.1D1 + x3
      t155 = t2 * t154
      t156 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t160 = log(-0.4D1 * t10 * t13 * t154)
      t161 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t172 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t182 = (-0.90D2 * t5 * (-t156 + t160 * t161) - 0.180D3 * t22 * t4 
     #* t161) * t27 / 0.1440D4 + t5 * (-t172 + t161) * t27 * t61 / 0.16D
     #2 + t5 * t161 * t27 * t87 / 0.8D1
      t183 = FJET(XB1, XB2, s, 0.0D0, t153, 0.0D0, -t155, 0.0D0, t182)
      t185 = FJET(XB1, XB2, s, 0.0D0, -t155, 0.0D0, t153, 0.0D0, t182)
      t189 = t2 * x1 * x2 * t125
      t191 = t1 * x1
      t192 = t71 * s * t191
      t193 = t1 ** 2
      t198 = s * t193 * x2 * x1 * t114 * t125
      t199 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t201 = t199 * t87 * t61
      t203 = t5 * t201 / 0.8D1
      t204 = FJET(XB1, XB2, s, 0.0D0, -t189, -t115, -t192, t198, -t203)
      t209 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t108)
      t211 = FJET(XB1, XB2, s, t113, -t115, 0.0D0, 0.0D0, 0.0D0, t146)
      t213 = FJET(XB1, XB2, s, t153, 0.0D0, -t155, 0.0D0, 0.0D0, t182)
      t216 = t2 * x1 * x3
      t218 = t1 * t114
      t219 = x3 * s * t218
      t220 = t154 * s
      t221 = t220 * t191
      t222 = t220 * t218
      t223 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t225 = t223 * t27 * t87
      t227 = t5 * t225 / 0.8D1
      t228 = FJET(XB1, XB2, s, t216, -t219, -t221, t222, 0.0D0, -t227)
      t233 = FJET(XB1, XB2, s, t222, -t221, -t219, t216, 0.0D0, -t227)
      t238 = FJET(XB1, XB2, s, -t115, t113, 0.0D0, 0.0D0, 0.0D0, t146)
      t240 = FJET(XB1, XB2, s, -t115, -t192, 0.0D0, -t189, t198, -t203)
      t245 = FJET(XB1, XB2, s, -t155, 0.0D0, t153, 0.0D0, 0.0D0, t182)
      t247 = FJET(XB1, XB2, s, -t219, t216, t222, -t221, 0.0D0, -t227)
      t252 = FJET(XB1, XB2, s, -t192, -t115, -t189, 0.0D0, t198, -t203)
      t257 = FJET(XB1, XB2, s, -t221, t222, t216, -t219, 0.0D0, -t227)
      t262 = FJET(XB1, XB2, s, -t189, 0.0D0, -t192, -t115, t198, -t203)
      rrgg2gght9s2em1 = t109 * t108 + t111 * t108 + t146 * t147 + t149 *
     # t146 + t151 * t108 + t183 * t182 + t185 * t182 - t204 * 0.3141592
     #653589793D1 * t4 * t201 / 0.8D1 + t209 * t108 + t211 * t146 + t213
     # * t182 - t228 * 0.3141592653589793D1 * t4 * t225 / 0.8D1 - t233 *
     # 0.3141592653589793D1 * t4 * t225 / 0.8D1 + t238 * t146 - t240 * 0
     #.3141592653589793D1 * t4 * t201 / 0.8D1 + t245 * t182 - t247 * 0.3
     #141592653589793D1 * t4 * t225 / 0.8D1 - t252 * 0.3141592653589793D
     #1 * t4 * t201 / 0.8D1 - t257 * 0.3141592653589793D1 * t4 * t225 / 
     #0.8D1 - t262 * 0.3141592653589793D1 * t4 * t201 / 0.8D1

      end function



      doubleprecision function rrgg2gght9s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.3141592653589793D1 * t4
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t7 = 0.1D1 / x3
      t11 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t17 = 0.1D1 / x1
      t21 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t26 = z ** 2
      t30 = Sin(x4 * 0.3141592653589793D1)
      t31 = t30 ** 2
      t34 = log(0.4D1 / t26 / z * t31)
      t41 = -t5 * t6 * t7 / 0.16D2 + t5 * (-t6 + t11) / x2 / 0.16D2 - t5
     # * t6 * t17 / 0.8D1 - t5 * t21 / 0.16D2 + (0.180D3 * 0.31415926535
     #89793D1 * lh + 0.90D2 * 0.3141592653589793D1 * t34) * t4 * t6 / 0.
     #1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t46 = t2 * x1
      t48 = t2 * (-0.1D1 + x1)
      t49 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, x
     #4)
      t52 = t5 * t49 * t17 / 0.8D1
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t46, -t48, 0.0D0, t52)
      t56 = t4 * t49 * t17
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t48, t46, 0.0D0, t52)
      t63 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t65 = t2 * x3
      t67 = t2 * (-0.1D1 + x3)
      t68 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t71 = t5 * t68 * t7 / 0.16D2
      t72 = FJET(XB1, XB2, s, 0.0D0, t65, 0.0D0, -t67, 0.0D0, t71)
      t75 = t4 * t68 * t7
      t78 = FJET(XB1, XB2, s, 0.0D0, -t67, 0.0D0, t65, 0.0D0, t71)
      t82 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t84 = FJET(XB1, XB2, s, t46, -t48, 0.0D0, 0.0D0, 0.0D0, t52)
      t88 = FJET(XB1, XB2, s, t65, 0.0D0, -t67, 0.0D0, 0.0D0, t71)
      t92 = FJET(XB1, XB2, s, -t67, 0.0D0, t65, 0.0D0, 0.0D0, t71)
      t96 = FJET(XB1, XB2, s, -t48, t46, 0.0D0, 0.0D0, 0.0D0, t52)
      rrgg2gght9s2em2 = t42 * t41 + t44 * t41 + t53 * 0.3141592653589793
     #D1 * t56 / 0.8D1 + t59 * 0.3141592653589793D1 * t56 / 0.8D1 + t63 
     #* t41 + t72 * 0.3141592653589793D1 * t75 / 0.16D2 + t78 * 0.314159
     #2653589793D1 * t75 / 0.16D2 + t82 * t41 + t84 * 0.3141592653589793
     #D1 * t56 / 0.8D1 + t88 * 0.3141592653589793D1 * t75 / 0.16D2 + t92
     # * 0.3141592653589793D1 * t75 / 0.16D2 + t96 * 0.3141592653589793D
     #1 * t56 / 0.8D1

      end function



      doubleprecision function rrgg2gght9s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = 0.3141592653589793D1 * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght9s2em3 = -t9 * 0.3141592653589793D1 * t11 / 0.16D2 - t13 
     #* 0.3141592653589793D1 * t11 / 0.16D2 - t16 * 0.3141592653589793D1
     # * t11 / 0.16D2 - t19 * 0.3141592653589793D1 * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght9s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6
      rrgg2gght9s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh91J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = t1 * s * z
      t4 = 0.1D1 - z
      t5 = s * t4
      t6 = x1 * t4
      t7 = z + t6
      t8 = 0.1D1 / t7
      t9 = x1 * t8
      t10 = 0.1D1 - x2
      t11 = x3 * t10
      t13 = 0.1D1 - x3
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t11 * t7 * x2 * t13)
      t22 = 0.2D1 * t16 * t20
      t23 = t11 * t7 + x2 * t13 - t22
      t26 = 0.1D1 - x1
      t29 = s - t5 * t9 * t23 - t5 * t26 * x3
      t30 = t3 * t29
      t31 = t4 ** 2
      t32 = t26 ** 2
      t34 = t13 ** 2
      t39 = x1 ** 2
      t40 = t31 * t39
      t41 = t7 ** 2
      t42 = 0.1D1 / t41
      t43 = t23 ** 2
      t44 = t42 * t43
      t50 = t13 * t10 * t7 + x2 * x3 + t22
      t51 = t50 ** 2
      t55 = t1 ** 2
      t56 = t31 ** 2
      t57 = t55 * t56
      t58 = t39 ** 2
      t59 = t41 ** 2
      t62 = t43 ** 2
      t75 = t3 * t29 * t31
      t100 = t32 ** 2
      t101 = t34 ** 2
      t122 = -0.4D1 * t30 * t31 * t32 * t34 - 0.2D1 * t30 - t30 * t40 * 
     #t44 - t30 * t40 * t42 * t51 - 0.2D1 * t57 * t58 / t59 * t62 + 0.4D
     #1 * t57 * x1 * t8 * t23 * t32 * t26 * t34 * t13 + 0.2D1 * t75 * t3
     #9 * t42 * t23 * t50 + 0.3D1 * t75 * t9 * t23 * t26 * t13 - 0.6D1 *
     # t57 * t32 * t34 * t39 * t44 + t3 * t4 * t9 * t23 * t29 - 0.3D1 * 
     #t75 * t9 * t50 * t26 * t13 - 0.2D1 * t57 * t100 * t101 + 0.4D1 * t
     #57 * t39 * x1 / t41 / t7 * t43 * t23 * t26 * t13 - t30 * t6 * t8 *
     # t50 + 0.2D1 * t30 * t4 * t26 * t13
      rrgg2ggh91J1 = -0.9D1 / 0.2D1 * wd * t122 / t29 / s / z / 0.314159
     #2653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh91J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = t1 * s * z
      t4 = 0.1D1 - z
      t5 = s * t4
      t6 = x1 * t4
      t7 = z + t6
      t8 = 0.1D1 / t7
      t9 = x1 * t8
      t10 = 0.1D1 - x2
      t11 = x3 * t10
      t13 = 0.1D1 - x3
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t11 * t7 * x2 * t13)
      t22 = 0.2D1 * t16 * t20
      t23 = t11 * t7 + x2 * t13 - t22
      t26 = 0.1D1 - x1
      t29 = s - t5 * t9 * t23 - t5 * t26 * x3
      t30 = t3 * t29
      t31 = t4 ** 2
      t32 = t26 ** 2
      t34 = t13 ** 2
      t37 = 0.4D1 * t30 * t31 * t32 * t34
      t38 = 0.2D1 * t30
      t39 = x1 ** 2
      t40 = t31 * t39
      t41 = t7 ** 2
      t42 = 0.1D1 / t41
      t43 = t23 ** 2
      t44 = t42 * t43
      t46 = t30 * t40 * t44
      t50 = t13 * t10 * t7 + x2 * x3 + t22
      t51 = t50 ** 2
      t54 = t30 * t40 * t42 * t51
      t55 = t1 ** 2
      t56 = t31 ** 2
      t57 = t55 * t56
      t58 = t39 ** 2
      t59 = t41 ** 2
      t62 = t43 ** 2
      t65 = 0.2D1 * t57 * t58 / t59 * t62
      t73 = 0.4D1 * t57 * x1 * t8 * t23 * t32 * t26 * t34 * t13
      t75 = t3 * t29 * t31
      t80 = 0.2D1 * t75 * t39 * t42 * t23 * t50
      t84 = t75 * t9 * t23 * t26 * t13
      t90 = 0.6D1 * t57 * t32 * t34 * t39 * t44
      t94 = t3 * t4 * t9 * t23 * t29
      t98 = t75 * t9 * t50 * t26 * t13
      t100 = t32 ** 2
      t101 = t34 ** 2
      t104 = 0.2D1 * t57 * t100 * t101
      t114 = 0.4D1 * t57 * t39 * x1 / t41 / t7 * t43 * t23 * t26 * t13
      t117 = t30 * t6 * t8 * t50
      t121 = 0.2D1 * t30 * t4 * t26 * t13
      t122 = -t37 - t38 - t46 - t54 - t65 + t73 + t80 + 0.3D1 * t84 - t9
     #0 + t94 - 0.3D1 * t98 - t104 + t114 - t117 + t121
      t127 = -t121 - t73 + t65 - t94 + t90 - t114 + t38 + t117 - 0.6D1 *
     # t84 + 0.6D1 * t98 - t80 + t46 + t104 + t54 + t37
      rrgg2ggh91J2 = -0.9D1 / 0.2D1 * (0.2D1 * wd * t122 + wd * t127) / 
     #t29 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh91J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = t1 * s * z
      t4 = 0.1D1 - z
      t5 = s * t4
      t6 = x1 * t4
      t7 = z + t6
      t8 = 0.1D1 / t7
      t9 = x1 * t8
      t10 = 0.1D1 - x2
      t11 = x3 * t10
      t13 = 0.1D1 - x3
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t11 * t7 * x2 * t13)
      t22 = 0.2D1 * t16 * t20
      t23 = t11 * t7 + x2 * t13 - t22
      t26 = 0.1D1 - x1
      t29 = s - t5 * t9 * t23 - t5 * t26 * x3
      t30 = t3 * t29
      t31 = t4 ** 2
      t32 = t26 ** 2
      t34 = t13 ** 2
      t37 = 0.4D1 * t30 * t31 * t32 * t34
      t38 = 0.2D1 * t30
      t39 = x1 ** 2
      t40 = t31 * t39
      t41 = t7 ** 2
      t42 = 0.1D1 / t41
      t43 = t23 ** 2
      t44 = t42 * t43
      t46 = t30 * t40 * t44
      t50 = t13 * t10 * t7 + x2 * x3 + t22
      t51 = t50 ** 2
      t54 = t30 * t40 * t42 * t51
      t55 = t1 ** 2
      t56 = t31 ** 2
      t57 = t55 * t56
      t58 = t39 ** 2
      t59 = t41 ** 2
      t62 = t43 ** 2
      t65 = 0.2D1 * t57 * t58 / t59 * t62
      t73 = 0.4D1 * t57 * x1 * t8 * t23 * t32 * t26 * t34 * t13
      t75 = t3 * t29 * t31
      t80 = 0.2D1 * t75 * t39 * t42 * t23 * t50
      t84 = t75 * t9 * t23 * t26 * t13
      t90 = 0.6D1 * t57 * t32 * t34 * t39 * t44
      t94 = t3 * t4 * t9 * t23 * t29
      t98 = t75 * t9 * t50 * t26 * t13
      t100 = t32 ** 2
      t101 = t34 ** 2
      t104 = 0.2D1 * t57 * t100 * t101
      t114 = 0.4D1 * t57 * t39 * x1 / t41 / t7 * t43 * t23 * t26 * t13
      t117 = t30 * t6 * t8 * t50
      t121 = 0.2D1 * t30 * t4 * t26 * t13
      t122 = -t37 - t38 - t46 - t54 - t65 + t73 + t80 + 0.3D1 * t84 - t9
     #0 + t94 - 0.3D1 * t98 - t104 + t114 - t117 + t121
      t127 = -t121 - t73 + t65 - t94 + t90 - t114 + t38 + t117 - 0.6D1 *
     # t84 + 0.6D1 * t98 - t80 + t46 + t104 + t54 + t37
      rrgg2ggh91J3 = -0.9D1 / 0.2D1 * (0.3D1 * wd * t122 + 0.2D1 * wd * 
     #t127) / t29 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh91J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = t1 * s * z
      t4 = 0.1D1 - z
      t5 = s * t4
      t6 = x1 * t4
      t7 = z + t6
      t8 = 0.1D1 / t7
      t9 = x1 * t8
      t10 = 0.1D1 - x2
      t11 = x3 * t10
      t13 = 0.1D1 - x3
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t11 * t7 * x2 * t13)
      t22 = 0.2D1 * t16 * t20
      t23 = t11 * t7 + x2 * t13 - t22
      t26 = 0.1D1 - x1
      t29 = s - t5 * t9 * t23 - t5 * t26 * x3
      t30 = t3 * t29
      t31 = t4 ** 2
      t32 = t26 ** 2
      t34 = t13 ** 2
      t37 = 0.4D1 * t30 * t31 * t32 * t34
      t38 = 0.2D1 * t30
      t39 = x1 ** 2
      t40 = t31 * t39
      t41 = t7 ** 2
      t42 = 0.1D1 / t41
      t43 = t23 ** 2
      t44 = t42 * t43
      t46 = t30 * t40 * t44
      t50 = t13 * t10 * t7 + x2 * x3 + t22
      t51 = t50 ** 2
      t54 = t30 * t40 * t42 * t51
      t55 = t1 ** 2
      t56 = t31 ** 2
      t57 = t55 * t56
      t58 = t39 ** 2
      t59 = t41 ** 2
      t62 = t43 ** 2
      t65 = 0.2D1 * t57 * t58 / t59 * t62
      t73 = 0.4D1 * t57 * x1 * t8 * t23 * t32 * t26 * t34 * t13
      t75 = t3 * t29 * t31
      t80 = 0.2D1 * t75 * t39 * t42 * t23 * t50
      t84 = t75 * t9 * t23 * t26 * t13
      t90 = 0.6D1 * t57 * t32 * t34 * t39 * t44
      t94 = t3 * t4 * t9 * t23 * t29
      t98 = t75 * t9 * t50 * t26 * t13
      t100 = t32 ** 2
      t101 = t34 ** 2
      t104 = 0.2D1 * t57 * t100 * t101
      t114 = 0.4D1 * t57 * t39 * x1 / t41 / t7 * t43 * t23 * t26 * t13
      t117 = t30 * t6 * t8 * t50
      t121 = 0.2D1 * t30 * t4 * t26 * t13
      t122 = -t37 - t38 - t46 - t54 - t65 + t73 + t80 + 0.3D1 * t84 - t9
     #0 + t94 - 0.3D1 * t98 - t104 + t114 - t117 + t121
      t127 = -t121 - t73 + t65 - t94 + t90 - t114 + t38 + t117 - 0.6D1 *
     # t84 + 0.6D1 * t98 - t80 + t46 + t104 + t54 + t37
      rrgg2ggh91J4 = -0.9D1 / 0.2D1 * (0.4D1 * wd * t122 + 0.3D1 * wd * 
     #t127) / t29 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh91J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = t1 * s * z
      t4 = 0.1D1 - z
      t5 = s * t4
      t6 = x1 * t4
      t7 = z + t6
      t8 = 0.1D1 / t7
      t9 = x1 * t8
      t10 = 0.1D1 - x2
      t11 = x3 * t10
      t13 = 0.1D1 - x3
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t11 * t7 * x2 * t13)
      t22 = 0.2D1 * t16 * t20
      t23 = t11 * t7 + x2 * t13 - t22
      t26 = 0.1D1 - x1
      t29 = s - t5 * t9 * t23 - t5 * t26 * x3
      t30 = t3 * t29
      t31 = t4 ** 2
      t32 = t26 ** 2
      t34 = t13 ** 2
      t37 = 0.4D1 * t30 * t31 * t32 * t34
      t38 = 0.2D1 * t30
      t39 = x1 ** 2
      t40 = t31 * t39
      t41 = t7 ** 2
      t42 = 0.1D1 / t41
      t43 = t23 ** 2
      t44 = t42 * t43
      t46 = t30 * t40 * t44
      t50 = t13 * t10 * t7 + x2 * x3 + t22
      t51 = t50 ** 2
      t54 = t30 * t40 * t42 * t51
      t55 = t1 ** 2
      t56 = t31 ** 2
      t57 = t55 * t56
      t58 = t39 ** 2
      t59 = t41 ** 2
      t62 = t43 ** 2
      t65 = 0.2D1 * t57 * t58 / t59 * t62
      t73 = 0.4D1 * t57 * x1 * t8 * t23 * t32 * t26 * t34 * t13
      t75 = t3 * t29 * t31
      t80 = 0.2D1 * t75 * t39 * t42 * t23 * t50
      t84 = t75 * t9 * t23 * t26 * t13
      t90 = 0.6D1 * t57 * t32 * t34 * t39 * t44
      t94 = t3 * t4 * t9 * t23 * t29
      t98 = t75 * t9 * t50 * t26 * t13
      t100 = t32 ** 2
      t101 = t34 ** 2
      t104 = 0.2D1 * t57 * t100 * t101
      t114 = 0.4D1 * t57 * t39 * x1 / t41 / t7 * t43 * t23 * t26 * t13
      t117 = t30 * t6 * t8 * t50
      t121 = 0.2D1 * t30 * t4 * t26 * t13
      t122 = -t37 - t38 - t46 - t54 - t65 + t73 + t80 + 0.3D1 * t84 - t9
     #0 + t94 - 0.3D1 * t98 - t104 + t114 - t117 + t121
      t127 = -t121 - t73 + t65 - t94 + t90 - t114 + t38 + t117 - 0.6D1 *
     # t84 + 0.6D1 * t98 - t80 + t46 + t104 + t54 + t37
      rrgg2ggh91J5 = -0.9D1 / 0.2D1 * (0.5D1 * wd * t122 + 0.4D1 * wd * 
     #t127) / t29 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh91J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = t1 * s * z
      t4 = 0.1D1 - z
      t5 = s * t4
      t6 = x1 * t4
      t7 = z + t6
      t8 = 0.1D1 / t7
      t9 = x1 * t8
      t10 = 0.1D1 - x2
      t11 = x3 * t10
      t13 = 0.1D1 - x3
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t11 * t7 * x2 * t13)
      t22 = 0.2D1 * t16 * t20
      t23 = t11 * t7 + x2 * t13 - t22
      t26 = 0.1D1 - x1
      t29 = s - t5 * t9 * t23 - t5 * t26 * x3
      t30 = t3 * t29
      t35 = t1 ** 2
      t36 = t4 ** 2
      t37 = t36 ** 2
      t38 = t35 * t37
      t41 = t26 ** 2
      t43 = t13 ** 2
      t49 = x1 ** 2
      t50 = t49 ** 2
      t51 = t7 ** 2
      t52 = t51 ** 2
      t55 = t23 ** 2
      t56 = t55 ** 2
      t66 = 0.1D1 / t51
      t67 = t66 * t55
      t85 = t13 * t10 * t7 + x2 * x3 + t22
      t90 = t3 * t29 * t36
      t106 = t36 * t49
      t109 = t41 ** 2
      t110 = t43 ** 2
      t114 = t85 ** 2
      t122 = -0.2D1 * t30 * t4 * t26 * t13 - 0.4D1 * t38 * x1 * t8 * t23
     # * t41 * t26 * t43 * t13 + 0.2D1 * t38 * t50 / t52 * t56 - t3 * t4
     # * t9 * t23 * t29 + 0.6D1 * t38 * t41 * t43 * t49 * t67 - 0.4D1 * 
     #t38 * t49 * x1 / t51 / t7 * t55 * t23 * t26 * t13 + 0.2D1 * t30 + 
     #t30 * t6 * t8 * t85 - 0.6D1 * t90 * t9 * t23 * t26 * t13 + 0.6D1 *
     # t90 * t9 * t85 * t26 * t13 - 0.2D1 * t90 * t49 * t66 * t23 * t85 
     #+ t30 * t106 * t67 + 0.2D1 * t38 * t109 * t110 + t30 * t106 * t66 
     #* t114 + 0.4D1 * t30 * t36 * t41 * t43
      rrgg2ggh91J6 = -0.45D2 / 0.2D1 * wd * t122 / t29 / s / z / 0.31415
     #92653589793D1

      end function
  
 