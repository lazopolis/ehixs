  
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + x3
      t2 = t1 * s
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = s * t3
      t7 = -0.1D1 + x1
      t8 = x1 * z
      t9 = x1 * x2
      t11 = x3 * x1
      t12 = t11 * z
      t13 = x2 * x3
      t14 = 0.2D1 * t13
      t15 = t11 * x2
      t17 = t11 * x2 * z
      t18 = x4 * pi
      t19 = cos(t18)
      t20 = -0.1D1 + x2
      t22 = 0.1D1 - x1 + t8
      t26 = Sqrt(x3 * t20 * t22 * x2 * t1)
      t28 = 0.2D1 * t19 * t26
      t29 = 0.1D1 - x1 + t8 - x2 + t9 - t9 * z - x3 + t11 - t12 + t14 - 
     #t15 + t17 + t28
      t31 = 0.1D1 / t22
      t33 = t6 * t7 * t29 * t31
      t34 = t6 * t11
      t38 = t6 * t7 * (-x3 + t11 - t12 + t14 - t15 + t17 - x2 + t28) * t
     #31
      t39 = t3 ** 2
      t44 = s * t39 * x2 * x1 * t7 * t31
      t45 = s ** 2
      t46 = 0.1D1 / t45
      t47 = pi * t46
      t48 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t38, -t33, t34, 
     #-t5, -t44)
      t49 = x1 ** 2
      t50 = Sin(t18)
      t51 = t50 ** 2
      t52 = t49 * t51
      t53 = t13 * t52
      t54 = z ** 2
      t55 = 0.1D1 / t54
      t56 = t55 * t31
      t57 = t7 ** 2
      t58 = t57 * t20
      t63 = log(0.4D1 * t53 * t56 * t58 * t1)
      t64 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t38, -t33, t34, 
     #-t5, -t44)
      t69 = pi * lh
      t73 = 0.90D2 * t47 * (t48 - t63 * t64) - 0.180D3 * t69 * t46 * t64
      t74 = 0.1D1 / x3
      t76 = 0.1D1 / x1
      t77 = 0.1D1 / x2
      t78 = t76 * t77
      t80 = t73 * t74 * t78 / 0.720D3
      t81 = FJET(XB1, XB2, s, -t5, -t33, t34, t38, -t44, t80)
      t84 = t74 * t76 * t77
      t87 = FJET(XB1, XB2, s, t34, t38, -t5, -t33, -t44, t80)
      t94 = Sqrt(x2 * t20 * x3 * t1)
      t96 = 0.2D1 * t19 * t94
      t98 = t6 * (0.1D1 - x2 - x3 + t14 + t96)
      t100 = t6 * (-x3 + t14 - x2 + t96)
      t102 = t55 * t20
      t106 = log(0.4D1 * t13 * t51 * t102 * t1)
      t107 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t100, t98, 0.0
     #D0, 0.0D0, 0.0D0)
      t109 = t106 ** 2
      t110 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t100, t98, 0.0
     #D0, 0.0D0, 0.0D0)
      t113 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t100, t98, 0.0
     #D0, 0.0D0, 0.0D0)
      t122 = pi ** 2
      t124 = lh ** 2
      t126 = -0.30D2 * t122 + 0.180D3 * t124
      t127 = pi * t126
      t128 = t46 * t110
      t134 = t13 * t49
      t135 = t55 * t51
      t140 = log(0.4D1 * t134 * t135 * t20 * t1)
      t151 = (0.90D2 * t47 * (t106 * t107 - t109 * t110 / 0.2D1 - t113) 
     #- 0.180D3 * t69 * t46 * (-t107 + t106 * t110) - t127 * t128) * t74
     # * t77 / 0.1440D4 + (0.90D2 * t47 * (-t107 + t140 * t110) + 0.180D
     #3 * t69 * t128) * t74 * t78 / 0.720D3
      t152 = FJET(XB1, XB2, s, 0.0D0, t98, 0.0D0, -t100, 0.0D0, t151)
      t154 = t6 * x1
      t155 = t6 * t7
      t156 = x3 * t49
      t157 = t156 * t51
      t159 = t55 * t57 * t31
      t162 = log(0.4D1 * t157 * t159)
      t163 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t155, 0
     #.0D0, t154, 0.0D0)
      t165 = t162 ** 2
      t166 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t155, 0
     #.0D0, t154, 0.0D0)
      t169 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t155, 0
     #.0D0, t154, 0.0D0)
      t178 = t46 * t166
      t179 = t127 * t178
      t182 = (0.90D2 * t47 * (-t162 * t163 + t165 * t166 / 0.2D1 + t169)
     # - 0.180D3 * t69 * t46 * (t163 - t162 * t166) + t179) * t74 * t76
      t185 = log(0.4D1 * t52 * t159)
      t187 = t163 - t185 * t166
      t190 = t185 ** 2
      t193 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t155, 0
     #.0D0, t154, 0.0D0)
      t198 = t190 * t163 / 0.2D1 + t193 - t190 * t185 * t166 / 0.6D1 - t
     #185 * t169
      t206 = -0.240D3 * zeta3 - 0.120D3 * t124 * lh + 0.60D2 * lh * t122
      t207 = pi * t206
      t208 = t207 * t178
      t212 = -t185 * t163 + t190 * t166 / 0.2D1 + t169
      t218 = t57 * t31
      t222 = log(0.4D1 * t134 * t135 * t218)
      t231 = (0.90D2 * t47 * (t163 - t222 * t166) - 0.180D3 * t69 * t178
     #) * t74 * t78
      t232 = x2 * t49
      t233 = t232 * t51
      t236 = log(0.4D1 * t233 * t159)
      t238 = t236 ** 2
      t251 = (0.90D2 * t47 * (t236 * t163 - t238 * t166 / 0.2D1 - t169) 
     #- 0.180D3 * t69 * t46 * (-t163 + t236 * t166) - t179) * t76 * t77
      t253 = t182 / 0.720D3 - (-t127 * t46 * t187 - 0.90D2 * t47 * t198 
     #- t208 + 0.180D3 * t69 * t46 * t212) * t76 / 0.720D3 + t231 / 0.72
     #0D3 - t251 / 0.720D3
      t254 = FJET(XB1, XB2, s, t154, -t155, 0.0D0, 0.0D0, 0.0D0, t253)
      t256 = t3 * t7
      t257 = t2 * t256
      t259 = t6 * t7 * x3
      t264 = log(-0.4D1 * t157 * t56 * t57 * t1)
      t265 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t259, t257, t3
     #4, -t5, 0.0D0)
      t267 = t264 ** 2
      t268 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t259, t257, t3
     #4, -t5, 0.0D0)
      t271 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t259, t257, t3
     #4, -t5, 0.0D0)
      t272 = -t264 * t265 + t267 * t268 / 0.2D1 + t271
      t276 = t265 - t264 * t268
      t280 = t46 * t268
      t281 = t127 * t280
      t287 = t13 * t135
      t290 = log(-0.4D1 * t218 * t1 * t49 * t287)
      t299 = (0.90D2 * t47 * (-t265 + t290 * t268) + 0.180D3 * t69 * t28
     #0) * t74 * t78
      t301 = (-0.90D2 * t47 * t272 + 0.180D3 * t69 * t46 * t276 - t281) 
     #* t74 * t76 / 0.720D3 + t299 / 0.720D3
      t302 = FJET(XB1, XB2, s, -t5, t257, t34, -t259, 0.0D0, t301)
      t304 = t6 * t1
      t305 = t6 * x3
      t306 = t135 * t1
      t309 = log(-0.4D1 * t156 * t306)
      t310 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t305, -t304, 0.
     #0D0, 0.0D0, 0.0D0)
      t312 = t309 ** 2
      t313 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t305, -t304, 0.
     #0D0, 0.0D0, 0.0D0)
      t316 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, t305, -t304, 0.
     #0D0, 0.0D0, 0.0D0)
      t325 = t46 * t313
      t326 = t127 * t325
      t333 = log(-0.4D1 * t134 * t306)
      t344 = x3 * t51
      t348 = log(-0.4D1 * t344 * t55 * t1)
      t353 = t348 ** 2
      t356 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, t305, -t304, 0.
     #0D0, 0.0D0, 0.0D0)
      t377 = log(-0.4D1 * t13 * t306)
      t379 = t377 ** 2
      t394 = (0.90D2 * t47 * (-t309 * t310 + t312 * t313 / 0.2D1 + t316)
     # - 0.180D3 * t69 * t46 * (t310 - t309 * t313) + t326) * t74 * t76 
     #/ 0.720D3 + (0.90D2 * t47 * (t310 - t333 * t313) - 0.180D3 * t69 *
     # t325) * t74 * t78 / 0.720D3 + (t127 * t46 * (t310 - t348 * t313) 
     #+ 0.90D2 * t47 * (t353 * t310 / 0.2D1 + t356 - t353 * t348 * t313 
     #/ 0.6D1 - t348 * t316) + t207 * t325 - 0.180D3 * t69 * t46 * (-t34
     #8 * t310 + t353 * t313 / 0.2D1 + t316)) * t74 / 0.1440D4 + (0.90D2
     # * t47 * (-t377 * t310 + t379 * t313 / 0.2D1 + t316) - 0.180D3 * t
     #69 * t46 * (t310 - t377 * t313) + t326) * t74 * t77 / 0.1440D4
      t395 = FJET(XB1, XB2, s, 0.0D0, -t304, 0.0D0, t305, 0.0D0, t394)
      t410 = t182 / 0.720D3 - (-t127 * t46 * t187 - 0.90D2 * t47 * t198 
     #- t208 + 0.180D3 * t69 * t46 * t212) * t76 / 0.720D3 + t231 / 0.72
     #0D3 - t251 / 0.720D3
      t411 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t154, -t155, 0.0D0, t410)
      t413 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0.0D
     #0, 0.0D0, 0.0D0)
      t416 = log(0.4D1 * t344 * t55)
      t417 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0.0D
     #0, 0.0D0, 0.0D0)
      t422 = t416 ** 2
      t425 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0.0D
     #0, 0.0D0, 0.0D0)
      t429 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0.0D
     #0, 0.0D0, 0.0D0)
      t434 = t46 * t417
      t435 = t207 * t434
      t447 = log(0.4D1 * t135)
      t448 = t447 ** 2
      t449 = t448 * pi
      t453 = t448 * t447 * pi
      t455 = t447 * pi
      t474 = rrgg2ggh91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0.0D
     #0, 0.0D0, 0.0D0)
      t482 = t122 ** 2
      t483 = t124 ** 2
      t491 = t448 ** 2
      t500 = log(0.4D1 * t156 * t135)
      t502 = t500 ** 2
      t513 = t127 * t434
      t518 = t52 * t55
      t520 = log(0.4D1 * t518)
      t525 = t520 ** 2
      t547 = log(0.4D1 * t13 * t518)
      t560 = log(0.4D1 * t232 * t135)
      t562 = t560 ** 2
      t578 = log(0.4D1 * t287)
      t580 = t578 ** 2
      t595 = x2 * t51
      t598 = log(0.4D1 * t595 * t55)
      t603 = t598 ** 2
      t623 = (t127 * t46 * (-t413 + t416 * t417) + 0.90D2 * t47 * (-t422
     # * t413 / 0.2D1 - t425 + t422 * t416 * t417 / 0.6D1 + t416 * t429)
     # - t435 - 0.180D3 * t69 * t46 * (t416 * t413 - t422 * t417 / 0.2D1
     # - t429)) * t74 / 0.1440D4 - (-0.90D2 * t449 * lh + t207 - 0.15D2 
     #* t453 - t455 * t126) * t46 * t413 / 0.1440D4 - (0.180D3 * t455 * 
     #lh + 0.45D2 * t449 + t127) * t46 * t429 / 0.1440D4 - (-0.180D3 * t
     #69 - 0.90D2 * t455) * t46 * t425 / 0.1440D4 - t47 * t474 / 0.16D2 
     #- (0.30D2 * t453 * lh + t449 * t126 / 0.2D1 - t455 * t206 + pi * (
     #t482 + 0.60D2 * t483 + 0.480D3 * lh * zeta3 - 0.60D2 * t124 * t122
     #) + 0.15D2 / 0.4D1 * t491 * pi) * t46 * t417 / 0.1440D4 + (0.90D2 
     #* t47 * (t500 * t413 - t502 * t417 / 0.2D1 - t429) - 0.180D3 * t69
     # * t46 * (-t413 + t500 * t417) - t513) * t74 * t76 / 0.720D3 - (t1
     #27 * t46 * (t413 - t520 * t417) + 0.90D2 * t47 * (t525 * t413 / 0.
     #2D1 + t425 - t525 * t520 * t417 / 0.6D1 - t520 * t429) + t435 - 0.
     #180D3 * t69 * t46 * (-t520 * t413 + t525 * t417 / 0.2D1 + t429)) *
     # t76 / 0.720D3 + (0.90D2 * t47 * (-t413 + t547 * t417) + 0.180D3 *
     # t69 * t434) * t74 * t78 / 0.720D3 - (0.90D2 * t47 * (-t560 * t413
     # + t562 * t417 / 0.2D1 + t429) - 0.180D3 * t69 * t46 * (t413 - t56
     #0 * t417) + t513) * t76 * t77 / 0.720D3 + (0.90D2 * t47 * (t578 * 
     #t413 - t580 * t417 / 0.2D1 - t429) - 0.180D3 * t69 * t46 * (-t413 
     #+ t578 * t417) - t513) * t74 * t77 / 0.1440D4 - (t127 * t46 * (t41
     #3 - t598 * t417) + 0.90D2 * t47 * (t603 * t413 / 0.2D1 + t425 - t6
     #03 * t598 * t417 / 0.6D1 - t598 * t429) + t435 - 0.180D3 * t69 * t
     #46 * (-t598 * t413 + t603 * t417 / 0.2D1 + t429)) * t77 / 0.1440D4
      t624 = FJET(XB1, XB2, s, t6, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t623)
      t627 = x2 * s * t3
      t628 = t20 * s
      t629 = t628 * t3
      t630 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t627, -t629, 0.
     #0D0, 0.0D0, 0.0D0)
      t631 = t135 * t20
      t634 = log(-0.4D1 * t134 * t631)
      t635 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t627, -t629, 0.
     #0D0, 0.0D0, 0.0D0)
      t640 = t46 * t635
      t646 = (0.90D2 * t47 * (t630 - t634 * t635) - 0.180D3 * t69 * t640
     #) * t74 * t78 / 0.720D3
      t649 = log(-0.4D1 * t232 * t631)
      t651 = t649 ** 2
      t654 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, t627, -t629, 0.
     #0D0, 0.0D0, 0.0D0)
      t663 = t127 * t640
      t667 = (0.90D2 * t47 * (t649 * t630 - t651 * t635 / 0.2D1 - t654) 
     #- 0.180D3 * t69 * t46 * (-t630 + t649 * t635) - t663) * t76 * t77 
     #/ 0.720D3
      t670 = log(-0.4D1 * t13 * t631)
      t672 = t670 ** 2
      t686 = (0.90D2 * t47 * (-t670 * t630 + t672 * t635 / 0.2D1 + t654)
     # - 0.180D3 * t69 * t46 * (t630 - t670 * t635) + t663) * t74 * t77 
     #/ 0.1440D4
      t689 = log(-0.4D1 * t595 * t102)
      t691 = t630 - t689 * t635
      t694 = t689 ** 2
      t697 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, t627, -t629, 0.
     #0D0, 0.0D0, 0.0D0)
      t702 = t694 * t630 / 0.2D1 + t697 - t694 * t689 * t635 / 0.6D1 - t
     #689 * t654
      t705 = t207 * t640
      t709 = -t689 * t630 + t694 * t635 / 0.2D1 + t654
      t716 = t646 - t667 + t686 - (-t127 * t46 * t691 - 0.90D2 * t47 * t
     #702 - t705 + 0.180D3 * t69 * t46 * t709) * t77 / 0.1440D4
      t717 = FJET(XB1, XB2, s, t627, 0.0D0, -t629, 0.0D0, 0.0D0, t716)
      t721 = t6 * t7 * x2 * t31
      t722 = t628 * t256
      t723 = t56 * t58
      t726 = log(-0.4D1 * t53 * t723)
      t727 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t721, t722, 0.
     #0D0, t154, -t44)
      t729 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t721, t722, 0.
     #0D0, t154, -t44)
      t733 = t46 * t727
      t741 = log(-0.4D1 * t233 * t723)
      t743 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t721, t722, 0.
     #0D0, t154, -t44)
      t744 = t741 ** 2
      t760 = (0.90D2 * t47 * (t726 * t727 - t729) + 0.180D3 * t69 * t733
     #) * t74 * t78 / 0.720D3 - (0.90D2 * t47 * (-t741 * t729 + t743 + t
     #744 * t727 / 0.2D1) - 0.180D3 * t69 * t46 * (t729 - t741 * t727) +
     # t127 * t733) * t76 * t77 / 0.720D3
      t761 = FJET(XB1, XB2, s, 0.0D0, -t721, t154, t722, -t44, t760)
      t763 = FJET(XB1, XB2, s, -t721, 0.0D0, t722, t154, -t44, t760)
      t765 = FJET(XB1, XB2, s, t154, t722, 0.0D0, -t721, -t44, t760)
      t778 = (-0.90D2 * t47 * t272 + 0.180D3 * t69 * t46 * t276 - t281) 
     #* t74 * t76 / 0.720D3 + t299 / 0.720D3
      t779 = FJET(XB1, XB2, s, t34, -t259, -t5, t257, 0.0D0, t778)
      t781 = FJET(XB1, XB2, s, -t155, t154, 0.0D0, 0.0D0, 0.0D0, t253)
      t796 = t646 - t667 + t686 - (-t127 * t46 * t691 - 0.90D2 * t47 * t
     #702 - t705 + 0.180D3 * t69 * t46 * t709) * t77 / 0.1440D4
      t797 = FJET(XB1, XB2, s, 0.0D0, -t629, 0.0D0, t627, 0.0D0, t796)
      t799 = FJET(XB1, XB2, s, t722, t154, -t721, 0.0D0, -t44, t760)
      t801 = t81 * t73 * t84 / 0.720D3 + t87 * t73 * t84 / 0.720D3 + t15
     #2 * t151 + t254 * t253 + t302 * t301 + t395 * t394 + t411 * t410 +
     # t624 * t623 + t717 * t716 + t761 * t760 + t763 * t760 + t765 * t7
     #60 + t779 * t778 + t781 * t253 + t797 * t796 + t799 * t760
      t802 = FJET(XB1, XB2, s, 0.0D0, -t100, 0.0D0, t98, 0.0D0, t151)
      t804 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t6, 0.0D0, t623)
      t806 = FJET(XB1, XB2, s, -t33, -t5, t38, t34, -t44, t80)
      t810 = FJET(XB1, XB2, s, t38, t34, -t33, -t5, -t44, t80)
      t814 = FJET(XB1, XB2, s, -t100, 0.0D0, t98, 0.0D0, 0.0D0, t151)
      t816 = FJET(XB1, XB2, s, -t629, 0.0D0, t627, 0.0D0, 0.0D0, t716)
      t818 = FJET(XB1, XB2, s, -t259, t34, t257, -t5, 0.0D0, t301)
      t820 = FJET(XB1, XB2, s, t257, -t5, -t259, t34, 0.0D0, t778)
      t822 = FJET(XB1, XB2, s, t305, 0.0D0, -t304, 0.0D0, 0.0D0, t394)
      t824 = FJET(XB1, XB2, s, 0.0D0, t627, 0.0D0, -t629, 0.0D0, t796)
      t826 = FJET(XB1, XB2, s, -t304, 0.0D0, t305, 0.0D0, 0.0D0, t394)
      t828 = FJET(XB1, XB2, s, 0.0D0, t6, 0.0D0, 0.0D0, 0.0D0, t623)
      t830 = FJET(XB1, XB2, s, 0.0D0, t305, 0.0D0, -t304, 0.0D0, t394)
      t832 = FJET(XB1, XB2, s, t98, 0.0D0, -t100, 0.0D0, 0.0D0, t151)
      t834 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t155, t154, 0.0D0, t410)
      t836 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t6, 0.0D0, 0.0D0, t623)
      t838 = t802 * t151 + t804 * t623 + t806 * t73 * t84 / 0.720D3 + t8
     #10 * t73 * t84 / 0.720D3 + t814 * t151 + t816 * t716 + t818 * t301
     # + t820 * t778 + t822 * t394 + t824 * t796 + t826 * t394 + t828 * 
     #t623 + t830 * t394 + t832 * t151 + t834 * t410 + t836 * t623
      rrgg2gght9s1e1 = t801 + t838

      end function



      doubleprecision function rrgg2gght9s1e0
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t10 = x2 * x3
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t17 = t16 * t4
      t20 = log(-0.4D1 * t10 * t17)
      t21 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t26 = pi * lh
      t27 = t7 * t21
      t29 = 0.180D3 * t26 * t27
      t31 = 0.1D1 / x3
      t33 = 0.1D1 / x2
      t36 = x3 * t13
      t40 = log(-0.4D1 * t36 * t15 * t4)
      t42 = t40 ** 2
      t45 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t54 = pi ** 2
      t56 = lh ** 2
      t58 = -0.30D2 * t54 + 0.180D3 * t56
      t59 = pi * t58
      t64 = x1 ** 2
      t65 = x3 * t64
      t68 = log(-0.4D1 * t65 * t17)
      t75 = 0.1D1 / x1
      t80 = t31 * t75 * t33
      t83 = (0.90D2 * t8 * (t9 - t20 * t21) - t29) * t31 * t33 / 0.1440D
     #4 + (0.90D2 * t8 * (-t40 * t9 + t42 * t21 / 0.2D1 + t45) - 0.180D3
     # * t26 * t7 * (t9 - t40 * t21) + t59 * t27) * t31 / 0.1440D4 + (0.
     #90D2 * t8 * (t9 - t68 * t21) - t29) * t31 * t75 / 0.720D3 + t8 * t
     #21 * t80 / 0.8D1
      t84 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t83)
      t86 = -0.1D1 + x2
      t87 = t86 * s
      t88 = -0.1D1 + x1
      t89 = t1 * t88
      t90 = t87 * t89
      t91 = t2 * x1
      t93 = x1 * z
      t94 = 0.1D1 - x1 + t93
      t95 = 0.1D1 / t94
      t97 = t2 * t88 * x2 * t95
      t98 = t1 ** 2
      t103 = s * t98 * x2 * x1 * t88 * t95
      t104 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t97, t90, 0.0D
     #0, t91, -t103)
      t108 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t97, t90, 0.0D
     #0, t91, -t103)
      t109 = x2 * t64
      t110 = t109 * t13
      t111 = t15 * t86
      t112 = t88 ** 2
      t117 = log(-0.4D1 * t110 * t111 * t112 * t95)
      t129 = -t8 * t104 * t80 / 0.8D1 - (0.90D2 * t8 * (t108 - t117 * t1
     #04) - 0.180D3 * t26 * t7 * t104) * t75 * t33 / 0.720D3
      t130 = FJET(XB1, XB2, s, t90, t91, -t97, 0.0D0, -t103, t129)
      t132 = 0.2D1 * t10
      t133 = cos(t11)
      t137 = Sqrt(x2 * t86 * x3 * t4)
      t139 = 0.2D1 * t133 * t137
      t141 = t2 * (-x3 + t132 - x2 + t139)
      t143 = t2 * (0.1D1 - x2 - x3 + t132 + t139)
      t144 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t141, t143, 0.
     #0D0, 0.0D0, 0.0D0)
      t148 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t141, t143, 0.
     #0D0, 0.0D0, 0.0D0)
      t153 = log(0.4D1 * t10 * t13 * t111 * t4)
      t165 = -t8 * t144 * t80 / 0.8D1 + (0.90D2 * t8 * (-t148 + t153 * t
     #144) + 0.180D3 * t26 * t7 * t144) * t31 * t33 / 0.1440D4
      t166 = FJET(XB1, XB2, s, -t141, 0.0D0, t143, 0.0D0, 0.0D0, t165)
      t168 = t87 * t1
      t170 = x2 * s * t1
      t171 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t170, -t168, 0.
     #0D0, 0.0D0, 0.0D0)
      t172 = t16 * t86
      t175 = log(-0.4D1 * t10 * t172)
      t176 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t170, -t168, 0.
     #0D0, 0.0D0, 0.0D0)
      t181 = t7 * t176
      t183 = 0.180D3 * t26 * t181
      t187 = (0.90D2 * t8 * (t171 - t175 * t176) - t183) * t31 * t33 / 0
     #.1440D4
      t188 = x2 * t13
      t191 = log(-0.4D1 * t188 * t111)
      t193 = t191 ** 2
      t196 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, t170, -t168, 0.
     #0D0, 0.0D0, 0.0D0)
      t197 = -t191 * t171 + t193 * t176 / 0.2D1 + t196
      t201 = t171 - t191 * t176
      t205 = t59 * t181
      t211 = t8 * t176 * t80 / 0.8D1
      t214 = log(-0.4D1 * t109 * t172)
      t222 = (0.90D2 * t8 * (-t171 + t214 * t176) + t183) * t75 * t33 / 
     #0.720D3
      t223 = t187 - (-0.90D2 * t8 * t197 + 0.180D3 * t26 * t7 * t201 - t
     #205) * t33 / 0.1440D4 + t211 - t222
      t224 = FJET(XB1, XB2, s, -t168, 0.0D0, t170, 0.0D0, 0.0D0, t223)
      t226 = x3 * x1
      t227 = t2 * t226
      t229 = t2 * t88 * x3
      t230 = t4 * s
      t232 = t230 * t1 * x1
      t233 = t230 * t89
      t234 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t229, t233, t2
     #27, -t232, 0.0D0)
      t235 = t65 * t13
      t241 = log(-0.4D1 * t235 * t15 * t95 * t112 * t4)
      t242 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t229, t233, t2
     #27, -t232, 0.0D0)
      t244 = -t234 + t241 * t242
      t249 = 0.180D3 * t26 * t7 * t242
      t256 = t8 * t242 * t80 / 0.8D1
      t257 = (0.90D2 * t8 * t244 + t249) * t31 * t75 / 0.720D3 - t256
      t258 = FJET(XB1, XB2, s, t227, -t229, -t232, t233, 0.0D0, t257)
      t260 = FJET(XB1, XB2, s, -t97, 0.0D0, t90, t91, -t103, t129)
      t262 = x1 * x2
      t264 = t226 * z
      t265 = t226 * x2
      t267 = t226 * x2 * z
      t272 = Sqrt(x3 * t86 * t94 * x2 * t4)
      t274 = 0.2D1 * t133 * t272
      t275 = 0.1D1 - x1 + t93 - x2 + t262 - t262 * z - x3 + t226 - t264 
     #+ t132 - t265 + t267 + t274
      t278 = t2 * t88 * t275 * t95
      t282 = t2 * t88 * (-x3 + t226 - t264 + t132 - t265 + t267 - x2 + t
     #274) * t95
      t283 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t282, -t278, t2
     #27, -t232, -t103)
      t286 = t8 * t283 * t80 / 0.8D1
      t287 = FJET(XB1, XB2, s, -t232, -t278, t227, t282, -t103, t286)
      t292 = t283 * t31 * t75 * t33
      t295 = FJET(XB1, XB2, s, t170, 0.0D0, -t168, 0.0D0, 0.0D0, t223)
      t297 = FJET(XB1, XB2, s, 0.0D0, -t97, t91, t90, -t103, t129)
      t299 = FJET(XB1, XB2, s, t143, 0.0D0, -t141, 0.0D0, 0.0D0, t165)
      t308 = (0.90D2 * t8 * t244 + t249) * t31 * t75 / 0.720D3 - t256
      t309 = FJET(XB1, XB2, s, -t229, t227, t233, -t232, 0.0D0, t308)
      t311 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t83)
      t313 = t2 * t88
      t314 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t313, 0
     #.0D0, t91, 0.0D0)
      t316 = t15 * t112 * t95
      t319 = log(0.4D1 * t235 * t316)
      t320 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t313, 0
     #.0D0, t91, 0.0D0)
      t325 = t7 * t320
      t327 = 0.180D3 * t26 * t325
      t331 = (0.90D2 * t8 * (t314 - t319 * t320) - t327) * t31 * t75 / 0
     #.720D3
      t334 = t8 * t320 * t80 / 0.8D1
      t337 = log(0.4D1 * t110 * t316)
      t345 = (0.90D2 * t8 * (-t314 + t337 * t320) + t327) * t75 * t33 / 
     #0.720D3
      t346 = t64 * t13
      t349 = log(0.4D1 * t346 * t316)
      t351 = t349 ** 2
      t354 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t313, 0
     #.0D0, t91, 0.0D0)
      t355 = -t349 * t314 + t351 * t320 / 0.2D1 + t354
      t359 = t314 - t349 * t320
      t363 = t59 * t325
      t367 = t331 + t334 - t345 - (-0.90D2 * t8 * t355 + 0.180D3 * t26 *
     # t7 * t359 - t363) * t75 / 0.720D3
      t368 = FJET(XB1, XB2, s, t91, -t313, 0.0D0, 0.0D0, 0.0D0, t367)
      t372 = log(0.4D1 * t36 * t15)
      t373 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t375 = t372 ** 2
      t376 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t379 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t388 = t7 * t376
      t389 = t59 * t388
      t394 = log(0.4D1 * t16)
      t395 = t394 * pi
      t398 = t394 ** 2
      t399 = t398 * pi
      t405 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t433 = log(0.4D1 * t10 * t16)
      t439 = 0.180D3 * t26 * t388
      t446 = log(0.4D1 * t188 * t15)
      t448 = t446 ** 2
      t464 = log(0.4D1 * t65 * t16)
      t478 = log(0.4D1 * t109 * t16)
      t489 = log(0.4D1 * t346 * t15)
      t491 = t489 ** 2
      t505 = (0.90D2 * t8 * (t372 * t373 - t375 * t376 / 0.2D1 - t379) -
     # 0.180D3 * t26 * t7 * (-t373 + t372 * t376) - t389) * t31 / 0.1440
     #D4 - (0.180D3 * t395 * lh + 0.45D2 * t399 + t59) * t7 * t373 / 0.1
     #440D4 - t8 * t405 / 0.16D2 - (-0.90D2 * t399 * lh + pi * (-0.240D3
     # * zeta3 - 0.120D3 * t56 * lh + 0.60D2 * lh * t54) - 0.15D2 * t398
     # * t394 * pi - t395 * t58) * t7 * t376 / 0.1440D4 - (-0.180D3 * t2
     #6 - 0.90D2 * t395) * t7 * t379 / 0.1440D4 + (0.90D2 * t8 * (-t373 
     #+ t433 * t376) + t439) * t31 * t33 / 0.1440D4 - (0.90D2 * t8 * (-t
     #446 * t373 + t448 * t376 / 0.2D1 + t379) - 0.180D3 * t26 * t7 * (t
     #373 - t446 * t376) + t389) * t33 / 0.1440D4 + (0.90D2 * t8 * (-t37
     #3 + t464 * t376) + t439) * t31 * t75 / 0.720D3 - t8 * t376 * t80 /
     # 0.8D1 - (0.90D2 * t8 * (t373 - t478 * t376) - t439) * t75 * t33 /
     # 0.720D3 - (0.90D2 * t8 * (-t489 * t373 + t491 * t376 / 0.2D1 + t3
     #79) - 0.180D3 * t26 * t7 * (t373 - t489 * t376) + t389) * t75 / 0.
     #720D3
      t506 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t505)
      t518 = t187 - (-0.90D2 * t8 * t197 + 0.180D3 * t26 * t7 * t201 - t
     #205) * t33 / 0.1440D4 + t211 - t222
      t519 = FJET(XB1, XB2, s, 0.0D0, t170, 0.0D0, -t168, 0.0D0, t518)
      t521 = FJET(XB1, XB2, s, t91, t90, 0.0D0, -t97, -t103, t129)
      t523 = t84 * t83 + t130 * t129 + t165 * t166 + t224 * t223 + t258 
     #* t257 + t260 * t129 + t287 * pi * t7 * t292 / 0.8D1 + t295 * t223
     # + t297 * t129 + t299 * t165 + t309 * t308 + t311 * t83 + t368 * t
     #367 + t506 * t505 + t519 * t518 + t521 * t129
      t524 = FJET(XB1, XB2, s, t227, t282, -t232, -t278, -t103, t286)
      t529 = FJET(XB1, XB2, s, t282, t227, -t278, -t232, -t103, t286)
      t534 = FJET(XB1, XB2, s, -t278, -t232, t282, t227, -t103, t286)
      t539 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t83)
      t541 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t505)
      t543 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t505)
      t545 = FJET(XB1, XB2, s, t233, -t232, -t229, t227, 0.0D0, t257)
      t547 = FJET(XB1, XB2, s, 0.0D0, -t168, 0.0D0, t170, 0.0D0, t518)
      t549 = FJET(XB1, XB2, s, 0.0D0, t143, 0.0D0, -t141, 0.0D0, t165)
      t561 = t331 + t334 - t345 - (-0.90D2 * t8 * t355 + 0.180D3 * t26 *
     # t7 * t359 - t363) * t75 / 0.720D3
      t562 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t313, t91, 0.0D0, t561)
      t564 = FJET(XB1, XB2, s, -t313, t91, 0.0D0, 0.0D0, 0.0D0, t367)
      t566 = FJET(XB1, XB2, s, 0.0D0, -t141, 0.0D0, t143, 0.0D0, t165)
      t568 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t505)
      t570 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t83)
      t572 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t91, -t313, 0.0D0, t561)
      t574 = FJET(XB1, XB2, s, -t232, t233, t227, -t229, 0.0D0, t308)
      t576 = t524 * pi * t7 * t292 / 0.8D1 + t529 * pi * t7 * t292 / 0.8
     #D1 + t534 * pi * t7 * t292 / 0.8D1 + t539 * t83 + t541 * t505 + t5
     #43 * t505 + t545 * t257 + t547 * t518 + t549 * t165 + t562 * t561 
     #+ t564 * t367 + t566 * t165 + t568 * t505 + t570 * t83 + t572 * t5
     #61 + t574 * t308
      rrgg2gght9s1e0 = t523 + t576

      end function



      doubleprecision function rrgg2gght9s1em1
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t5, 0.0D0
     #, t3, 0.0D0)
      t10 = 0.1D1 / x1
      t12 = 0.1D1 / x2
      t15 = t8 * t9 * t10 * t12 / 0.8D1
      t16 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t5, 0.0D
     #0, t3, 0.0D0)
      t17 = x1 ** 2
      t18 = x4 * pi
      t19 = Sin(t18)
      t20 = t19 ** 2
      t21 = t17 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t26 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t28 = t4 ** 2
      t32 = log(0.4D1 * t21 * t23 * t26 * t28)
      t34 = t16 - t32 * t9
      t37 = pi * lh
      t40 = 0.180D3 * t37 * t7 * t9
      t44 = 0.1D1 / x3
      t48 = t8 * t9 * t44 * t10 / 0.8D1
      t49 = t15 - (-0.90D2 * t8 * t34 + t40) * t10 / 0.720D3 + t48
      t50 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t49)
      t58 = t15 - (-0.90D2 * t8 * t34 + t40) * t10 / 0.720D3 + t48
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t58)
      t61 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t62 = x3 * t20
      t65 = log(0.4D1 * t62 * t23)
      t66 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t73 = 0.180D3 * t37 * t7 * t66
      t80 = log(0.4D1 * t23 * t20)
      t81 = t80 * pi
      t89 = t80 ** 2
      t92 = pi ** 2
      t94 = lh ** 2
      t102 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t105 = t66 * t44
      t109 = x2 * t20
      t112 = log(0.4D1 * t109 * t23)
      t126 = log(0.4D1 * t21 * t23)
      t137 = (0.90D2 * t8 * (-t61 + t65 * t66) + t73) * t44 / 0.1440D4 -
     # (-0.180D3 * t37 - 0.90D2 * t81) * t7 * t61 / 0.1440D4 - (0.180D3 
     #* t81 * lh + 0.45D2 * t89 * pi + pi * (-0.30D2 * t92 + 0.180D3 * t
     #94)) * t7 * t66 / 0.1440D4 - t8 * t102 / 0.16D2 - t8 * t105 * t12 
     #/ 0.16D2 - (0.90D2 * t8 * (t61 - t112 * t66) - t73) * t12 / 0.1440
     #D4 - t8 * t66 * t10 * t12 / 0.8D1 - (0.90D2 * t8 * (t61 - t126 * t
     #66) - t73) * t10 / 0.720D3 - t8 * t105 * t10 / 0.8D1
      t138 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t137)
      t140 = -0.1D1 + x3
      t141 = t2 * t140
      t142 = t2 * x3
      t143 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t142, -t141, 0.
     #0D0, 0.0D0, 0.0D0)
      t144 = t143 * t44
      t148 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t142, -t141, 0.
     #0D0, 0.0D0, 0.0D0)
      t152 = log(-0.4D1 * t62 * t23 * t140)
      t166 = t8 * t144 * t12 / 0.16D2 + (0.90D2 * t8 * (t148 - t152 * t1
     #43) - 0.180D3 * t37 * t7 * t143) * t44 / 0.1440D4 + t8 * t144 * t1
     #0 / 0.8D1
      t167 = FJET(XB1, XB2, s, 0.0D0, -t141, 0.0D0, t142, 0.0D0, t166)
      t169 = -0.1D1 + x2
      t170 = t169 * s
      t171 = t170 * t1
      t173 = x2 * s * t1
      t174 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t173, -t171, 0.
     #0D0, 0.0D0, 0.0D0)
      t178 = t8 * t174 * t10 * t12 / 0.8D1
      t182 = t8 * t174 * t44 * t12 / 0.16D2
      t183 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t173, -t171, 0.
     #0D0, 0.0D0, 0.0D0)
      t187 = log(-0.4D1 * t109 * t23 * t169)
      t189 = -t183 + t187 * t174
      t194 = 0.180D3 * t37 * t7 * t174
      t198 = t178 + t182 - (0.90D2 * t8 * t189 + t194) * t12 / 0.1440D4
      t199 = FJET(XB1, XB2, s, 0.0D0, -t171, 0.0D0, t173, 0.0D0, t198)
      t203 = t2 * t4 * x2 * t26
      t204 = t1 * t4
      t205 = t170 * t204
      t206 = t1 ** 2
      t211 = s * t206 * x2 * x1 * t4 * t26
      t212 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t203, t205, 0.
     #0D0, t3, -t211)
      t214 = t212 * t10 * t12
      t216 = t8 * t214 / 0.8D1
      t217 = FJET(XB1, XB2, s, 0.0D0, -t203, t3, t205, -t211, -t216)
      t222 = FJET(XB1, XB2, s, t3, t205, 0.0D0, -t203, -t211, -t216)
      t228 = 0.2D1 * x2 * x3
      t229 = cos(t18)
      t233 = Sqrt(x2 * t169 * x3 * t140)
      t235 = 0.2D1 * t229 * t233
      t237 = t2 * (0.1D1 - x2 - x3 + t228 + t235)
      t239 = t2 * (-x3 + t228 - x2 + t235)
      t240 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t239, t237, 0.
     #0D0, 0.0D0, 0.0D0)
      t242 = t240 * t44 * t12
      t244 = t8 * t242 / 0.16D2
      t245 = FJET(XB1, XB2, s, t237, 0.0D0, -t239, 0.0D0, 0.0D0, -t244)
      t250 = FJET(XB1, XB2, s, 0.0D0, -t239, 0.0D0, t237, 0.0D0, -t244)
      t255 = FJET(XB1, XB2, s, t205, t3, -t203, 0.0D0, -t211, -t216)
      t260 = t140 * s
      t261 = t260 * t204
      t263 = t260 * t1 * x1
      t265 = t2 * t4 * x3
      t267 = t2 * x1 * x3
      t268 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t265, t261, t2
     #67, -t263, 0.0D0)
      t270 = t268 * t44 * t10
      t272 = t8 * t270 / 0.8D1
      t273 = FJET(XB1, XB2, s, t261, -t263, -t265, t267, 0.0D0, -t272)
      t278 = FJET(XB1, XB2, s, 0.0D0, t237, 0.0D0, -t239, 0.0D0, -t244)
      t283 = FJET(XB1, XB2, s, -t239, 0.0D0, t237, 0.0D0, 0.0D0, -t244)
      t288 = FJET(XB1, XB2, s, -t265, t267, t261, -t263, 0.0D0, -t272)
      t293 = t50 * t49 + t59 * t58 + t138 * t137 + t167 * t166 + t199 * 
     #t198 - t217 * pi * t7 * t214 / 0.8D1 - t222 * pi * t7 * t214 / 0.8
     #D1 - t245 * pi * t7 * t242 / 0.16D2 - t250 * pi * t7 * t242 / 0.16
     #D2 - t255 * pi * t7 * t214 / 0.8D1 - t273 * pi * t7 * t270 / 0.8D1
     # - t278 * pi * t7 * t242 / 0.16D2 - t283 * pi * t7 * t242 / 0.16D2
     # - t288 * pi * t7 * t270 / 0.8D1
      t294 = FJET(XB1, XB2, s, -t203, 0.0D0, t205, t3, -t211, -t216)
      t299 = FJET(XB1, XB2, s, -t263, t261, t267, -t265, 0.0D0, -t272)
      t304 = FJET(XB1, XB2, s, t267, -t265, -t263, t261, 0.0D0, -t272)
      t309 = FJET(XB1, XB2, s, -t141, 0.0D0, t142, 0.0D0, 0.0D0, t166)
      t311 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t137)
      t313 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t58)
      t315 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t49)
      t317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t137)
      t325 = t178 + t182 - (0.90D2 * t8 * t189 + t194) * t12 / 0.1440D4
      t326 = FJET(XB1, XB2, s, t173, 0.0D0, -t171, 0.0D0, 0.0D0, t325)
      t328 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t137)
      t330 = FJET(XB1, XB2, s, t142, 0.0D0, -t141, 0.0D0, 0.0D0, t166)
      t332 = FJET(XB1, XB2, s, -t171, 0.0D0, t173, 0.0D0, 0.0D0, t325)
      t334 = FJET(XB1, XB2, s, 0.0D0, t173, 0.0D0, -t171, 0.0D0, t198)
      t336 = FJET(XB1, XB2, s, 0.0D0, t142, 0.0D0, -t141, 0.0D0, t166)
      t338 = -t294 * pi * t7 * t214 / 0.8D1 - t299 * pi * t7 * t270 / 0.
     #8D1 - t304 * pi * t7 * t270 / 0.8D1 + t309 * t166 + t311 * t137 + 
     #t313 * t58 + t315 * t49 + t317 * t137 + t326 * t325 + t328 * t137 
     #+ t330 * t166 + t332 * t325 + t334 * t198 + t336 * t166
      rrgg2gght9s1em1 = t293 + t338

      end function



      doubleprecision function rrgg2gght9s1em2
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = z ** 2
      t27 = Sin(x4 * pi)
      t28 = t27 ** 2
      t31 = log(0.4D1 / t24 * t28)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t
     #31 * pi) * t4 * t6 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t45 = t2 * (-0.1D1 + x1)
      t46 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t45, 0.0
     #D0, t43, 0.0D0)
      t49 = t5 * t46 * t15 / 0.8D1
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t49)
      t53 = t4 * t46 * t15
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t49)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t62 = t2 * x3
      t64 = t2 * (-0.1D1 + x3)
      t65 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t62, -t64, 0.0D0
     #, 0.0D0, 0.0D0)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t64, 0.0D0, t68)
      t72 = t4 * t65 * t7
      t76 = x2 * s * t1
      t79 = (-0.1D1 + x2) * s * t1
      t80 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t76, -t79, 0.0D0
     #, 0.0D0, 0.0D0)
      t83 = t5 * t80 * t11 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t76, 0.0D0, -t79, 0.0D0, t83)
      t87 = t4 * t80 * t11
      t90 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t62, 0.0D0, t68)
      t94 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t76, 0.0D0, t83)
      t98 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t100 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t49)
      t104 = FJET(XB1, XB2, s, t62, 0.0D0, -t64, 0.0D0, 0.0D0, t68)
      t108 = FJET(XB1, XB2, s, t76, 0.0D0, -t79, 0.0D0, 0.0D0, t83)
      t112 = FJET(XB1, XB2, s, -t64, 0.0D0, t62, 0.0D0, 0.0D0, t68)
      t116 = FJET(XB1, XB2, s, -t79, 0.0D0, t76, 0.0D0, 0.0D0, t83)
      t120 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t49)
      rrgg2gght9s1em2 = t39 * t38 + t41 * t38 + t50 * pi * t53 / 0.8D1 +
     # t56 * pi * t53 / 0.8D1 + t60 * t38 + t69 * pi * t72 / 0.16D2 + t8
     #4 * pi * t87 / 0.16D2 + t90 * pi * t72 / 0.16D2 + t94 * pi * t87 /
     # 0.16D2 + t98 * t38 + t100 * pi * t53 / 0.8D1 + t104 * pi * t72 / 
     #0.16D2 + t108 * pi * t87 / 0.16D2 + t112 * pi * t72 / 0.16D2 + t11
     #6 * pi * t87 / 0.16D2 + t120 * pi * t53 / 0.8D1

      end function



      doubleprecision function rrgg2gght9s1em3
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t4 = 0.1D1 / t3
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght9s1em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0.16D
     #2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght9s1em4
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght9s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght9s2e1
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t28 = pi * lh
      t34 = pi ** 2
      t36 = lh ** 2
      t38 = -0.30D2 * t34 + 0.180D3 * t36
      t39 = pi * t38
      t40 = t4 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t6 * t13
      t49 = t48 * t10
      t51 = log(0.4D1 * t49)
      t56 = t51 ** 2
      t59 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t72 = -0.240D3 * zeta3 - 0.120D3 * t36 * lh + 0.60D2 * lh * t34
      t73 = pi * t72
      t74 = t73 * t40
      t85 = x2 * x3
      t88 = log(0.4D1 * t85 * t49)
      t90 = t85 * t6
      t91 = -0.1D1 + x2
      t92 = t14 * t91
      t95 = log(-0.4D1 * t90 * t92)
      t100 = 0.1D1 / x2
      t101 = t43 * t45 * t100
      t104 = x2 * t6
      t107 = log(-0.4D1 * t104 * t92)
      t109 = t107 ** 2
      t114 = log(0.4D1 * t104 * t14)
      t116 = t114 ** 2
      t132 = x3 * t10
      t135 = log(0.4D1 * t132 * t13)
      t140 = t135 ** 2
      t162 = log(0.4D1 * t85 * t14)
      t164 = t162 ** 2
      t169 = log(-0.4D1 * t85 * t92)
      t171 = t169 ** 2
      t192 = x2 * t10
      t196 = log(-0.4D1 * t192 * t13 * t91)
      t197 = t196 ** 2
      t200 = log(0.4D1 * t192 * t13)
      t201 = t200 ** 2
      t224 = log(0.4D1 * t14)
      t225 = t224 ** 2
      t226 = t225 * pi
      t230 = t225 * t224 * pi
      t232 = t224 * pi
      t251 = rrgg2ggh91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t259 = t34 ** 2
      t260 = t36 ** 2
      t268 = t225 ** 2
      t275 = -(0.90D2 * t5 * (-t17 * t18 + t20 * t21 / 0.2D1 + t24) - 0.
     #180D3 * t28 * t4 * (t18 - t17 * t21) + t41) * t43 * t45 / 0.720D3 
     #- (t39 * t4 * (t18 - t51 * t21) + 0.90D2 * t5 * (t56 * t18 / 0.2D1
     # + t59 - t56 * t51 * t21 / 0.6D1 - t51 * t24) + t74 - 0.180D3 * t2
     #8 * t4 * (-t51 * t18 + t56 * t21 / 0.2D1 + t24)) * t45 / 0.720D3 -
     # t5 * (-t88 * t21 + t95 * t21) * t101 / 0.8D1 - (0.90D2 * t5 * (t1
     #07 * t18 - t109 * t21 / 0.2D1 - t114 * t18 + t116 * t21 / 0.2D1) -
     # 0.180D3 * t28 * t4 * (t107 * t21 - t114 * t21)) * t45 * t100 / 0.
     #720D3 - (t39 * t4 * (t18 - t135 * t21) + 0.90D2 * t5 * (t140 * t18
     # / 0.2D1 + t59 - t140 * t135 * t21 / 0.6D1 - t135 * t24) + t74 - 0
     #.180D3 * t28 * t4 * (-t135 * t18 + t140 * t21 / 0.2D1 + t24)) * t4
     #3 / 0.1440D4 - (0.90D2 * t5 * (-t162 * t18 + t164 * t21 / 0.2D1 + 
     #t169 * t18 - t171 * t21 / 0.2D1) - 0.180D3 * t28 * t4 * (-t162 * t
     #21 + t169 * t21)) * t43 * t100 / 0.1440D4 - ((0.90D2 * t5 * t18 - 
     #0.180D3 * t28 * t40) * (-t197 / 0.2D1 + t201 / 0.2D1) + 0.90D2 * t
     #5 * t21 * (-t201 * t200 / 0.6D1 + t197 * t196 / 0.6D1) + (-0.180D3
     # * t28 * t4 * t18 + t41 + 0.90D2 * t5 * t24) * (t196 - t200)) * t1
     #00 / 0.1440D4 - (-0.90D2 * t226 * lh + t73 - 0.15D2 * t230 - t232 
     #* t38) * t4 * t18 / 0.1440D4 - (0.180D3 * t232 * lh + 0.45D2 * t22
     #6 + t39) * t4 * t24 / 0.1440D4 - (-0.180D3 * t28 - 0.90D2 * t232) 
     #* t4 * t59 / 0.1440D4 - t5 * t251 / 0.16D2 - (0.30D2 * t230 * lh +
     # t226 * t38 / 0.2D1 - t232 * t72 + pi * (t259 + 0.60D2 * t260 + 0.
     #480D3 * lh * zeta3 - 0.60D2 * t36 * t34) + 0.15D2 / 0.4D1 * t268 *
     # pi) * t4 * t21 / 0.1440D4
      t276 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t275)
      t278 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t275)
      t280 = t2 * x1
      t281 = -0.1D1 + x1
      t282 = t2 * t281
      t283 = t7 * t13
      t284 = 0.1D1 / t8
      t285 = t281 ** 2
      t287 = x1 * z
      t288 = -z - x1 + t287
      t289 = 0.1D1 / t288
      t290 = t284 * t285 * t289
      t293 = log(-0.4D1 * t283 * t290)
      t294 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280, 0.
     #0D0, -t282, 0.0D0)
      t296 = t293 ** 2
      t297 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280, 0.
     #0D0, -t282, 0.0D0)
      t300 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280, 0.
     #0D0, -t282, 0.0D0)
      t309 = t4 * t297
      t310 = t39 * t309
      t313 = (0.90D2 * t5 * (t293 * t294 - t296 * t297 / 0.2D1 - t300) -
     # 0.180D3 * t28 * t4 * (-t294 + t293 * t297) - t310) * t43 * t45
      t316 = log(-0.4D1 * t48 * t290)
      t318 = -t294 + t316 * t297
      t321 = t316 ** 2
      t324 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280, 0.
     #0D0, -t282, 0.0D0)
      t329 = -t321 * t294 / 0.2D1 - t324 + t321 * t316 * t297 / 0.6D1 + 
     #t316 * t300
      t332 = t73 * t309
      t336 = t316 * t294 - t321 * t297 / 0.2D1 - t300
      t347 = log(-0.4D1 * t90 * t13 * t284 * t285 * t289)
      t356 = t45 * t100
      t357 = (0.90D2 * t5 * (-t294 + t347 * t297) + 0.180D3 * t28 * t309
     #) * t43 * t356
      t358 = t104 * t13
      t361 = log(-0.4D1 * t358 * t290)
      t363 = t361 ** 2
      t376 = (0.90D2 * t5 * (t361 * t294 - t363 * t297 / 0.2D1 - t300) -
     # 0.180D3 * t28 * t4 * (-t294 + t361 * t297) - t310) * t45 * t100
      t378 = -t313 / 0.720D3 - (t39 * t4 * t318 + 0.90D2 * t5 * t329 - t
     #332 - 0.180D3 * t28 * t4 * t336) * t45 / 0.720D3 - t357 / 0.720D3 
     #- t376 / 0.720D3
      t379 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t280, -t282, 0.0D0, t378)
      t381 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t282, t280, 0.0D0, t378)
      t383 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t275)
      t385 = t2 * x3
      t386 = -0.1D1 + x3
      t387 = t2 * t386
      t388 = t14 * t386
      t391 = log(-0.4D1 * t7 * t388)
      t392 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #385, -t387, 0.0D0)
      t394 = t391 ** 2
      t395 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #385, -t387, 0.0D0)
      t398 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #385, -t387, 0.0D0)
      t407 = t4 * t395
      t412 = (0.90D2 * t5 * (t391 * t392 - t394 * t395 / 0.2D1 - t398) -
     # 0.180D3 * t28 * t4 * (-t392 + t391 * t395) - t39 * t407) * t43 * 
     #t45 / 0.720D3
      t415 = log(-0.4D1 * t90 * t388)
      t421 = log(0.4D1 * t90 * t14 * t91 * t386)
      t426 = t5 * (t415 * t395 - t421 * t395) * t101 / 0.8D1
      t427 = t13 * t386
      t430 = log(-0.4D1 * t132 * t427)
      t432 = -t392 + t430 * t395
      t435 = t430 ** 2
      t438 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #385, -t387, 0.0D0)
      t443 = -t435 * t392 / 0.2D1 - t438 + t435 * t430 * t395 / 0.6D1 + 
     #t430 * t398
      t446 = t73 * t407
      t450 = t430 * t392 - t435 * t395 / 0.2D1 - t398
      t461 = log(-0.4D1 * t192 * t13 * x3 * t386)
      t463 = t461 ** 2
      t470 = log(0.4D1 * t85 * t10 * t427 * t91)
      t472 = t470 ** 2
      t487 = (0.90D2 * t5 * (t461 * t392 - t463 * t395 / 0.2D1 - t470 * 
     #t392 + t472 * t395 / 0.2D1) - 0.180D3 * t28 * t4 * (t461 * t395 - 
     #t470 * t395)) * t43 * t100 / 0.1440D4
      t488 = -t412 - t426 - (t39 * t4 * t432 + 0.90D2 * t5 * t443 - t446
     # - 0.180D3 * t28 * t4 * t450) * t43 / 0.1440D4 - t487
      t489 = FJET(XB1, XB2, s, 0.0D0, t385, 0.0D0, -t387, 0.0D0, t488)
      t491 = FJET(XB1, XB2, s, 0.0D0, -t387, 0.0D0, t385, 0.0D0, t488)
      t493 = x1 * x2
      t495 = t2 * t493 * t289
      t497 = t1 * x1
      t498 = t91 * s * t497
      t499 = t1 ** 2
      t504 = s * t499 * x2 * x1 * t281 * t289
      t505 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t495, -t498, 0
     #.0D0, -t282, t504)
      t506 = t85 * t48
      t507 = t284 * t289
      t508 = t285 * t91
      t509 = t507 * t508
      t512 = log(0.4D1 * t506 * t509)
      t513 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t495, -t498, 0
     #.0D0, -t282, t504)
      t518 = t4 * t513
      t526 = log(0.4D1 * t358 * t509)
      t528 = t526 ** 2
      t531 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t495, -t498, 0
     #.0D0, -t282, t504)
      t545 = -(0.90D2 * t5 * (t505 - t512 * t513) - 0.180D3 * t28 * t518
     #) * t43 * t356 / 0.720D3 - (0.90D2 * t5 * (-t526 * t505 + t528 * t
     #513 / 0.2D1 + t531) - 0.180D3 * t28 * t4 * (t505 - t526 * t513) + 
     #t39 * t518) * t45 * t100 / 0.720D3
      t546 = FJET(XB1, XB2, s, 0.0D0, -t495, -t282, -t498, t504, t545)
      t548 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t275)
      t563 = -t313 / 0.720D3 - (t39 * t4 * t318 + 0.90D2 * t5 * t329 - t
     #332 - 0.180D3 * t28 * t4 * t336) * t45 / 0.720D3 - t357 / 0.720D3 
     #- t376 / 0.720D3
      t564 = FJET(XB1, XB2, s, t280, -t282, 0.0D0, 0.0D0, 0.0D0, t563)
      t579 = -t412 - t426 - (t39 * t4 * t432 + 0.90D2 * t5 * t443 - t446
     # - 0.180D3 * t28 * t4 * t450) * t43 / 0.1440D4 - t487
      t580 = FJET(XB1, XB2, s, t385, 0.0D0, -t387, 0.0D0, 0.0D0, t579)
      t582 = x3 * x1
      t583 = t2 * t582
      t585 = t2 * t281 * x3
      t586 = t386 * s
      t587 = t586 * t497
      t589 = t586 * t1 * t281
      t591 = t507 * t285 * t386
      t594 = log(0.4D1 * t283 * t591)
      t595 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t583, -t587, -t
     #585, t589, 0.0D0)
      t597 = t594 ** 2
      t598 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t583, -t587, -t
     #585, t589, 0.0D0)
      t601 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, t583, -t587, -t
     #585, t589, 0.0D0)
      t610 = t4 * t598
      t617 = log(0.4D1 * t506 * t591)
      t628 = -(0.90D2 * t5 * (-t594 * t595 + t597 * t598 / 0.2D1 + t601)
     # - 0.180D3 * t28 * t4 * (t595 - t594 * t598) + t39 * t610) * t43 *
     # t45 / 0.720D3 - (0.90D2 * t5 * (t595 - t617 * t598) - 0.180D3 * t
     #28 * t610) * t43 * t356 / 0.720D3
      t629 = FJET(XB1, XB2, s, t583, -t585, -t587, t589, 0.0D0, t628)
      t631 = t276 * t275 + t278 * t275 + t379 * t378 + t381 * t378 + t38
     #3 * t275 + t489 * t488 + t491 * t488 + t546 * t545 + t548 * t275 +
     # t564 * t563 + t580 * t579 + t629 * t628
      t632 = FJET(XB1, XB2, s, t589, -t587, -t585, t583, 0.0D0, t628)
      t634 = x2 * z
      t636 = x3 * z
      t637 = t582 * z
      t638 = t85 * z
      t639 = t582 * x2
      t640 = t582 * t634
      t641 = cos(t11)
      t646 = Sqrt(-x3 * t91 * t288 * x2 * t386)
      t648 = 0.2D1 * t641 * t646
      t649 = z + x1 - t287 - t634 - t493 + t493 * z - t636 - t582 + t637
     # + t638 + t639 - t640 + t85 + t648
      t652 = t2 * x1 * t649 * t289
      t656 = t2 * x1 * (-t636 - t582 + t637 + t638 + t639 - t640 - x2 + 
     #t85 + t648) * t289
      t657 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t656, -t652, -t
     #585, t589, t504)
      t662 = log(-0.4D1 * t506 * t507 * t508 * t386)
      t663 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t656, -t652, -t
     #585, t589, t504)
      t665 = -t657 + t662 * t663
      t670 = 0.180D3 * t28 * t4 * t663
      t671 = 0.90D2 * t5 * t665 + t670
      t674 = t671 * t43 * t356 / 0.720D3
      t675 = FJET(XB1, XB2, s, t589, -t652, -t585, t656, t504, -t674)
      t679 = FJET(XB1, XB2, s, t656, -t585, -t652, t589, t504, -t674)
      t683 = FJET(XB1, XB2, s, -t282, t280, 0.0D0, 0.0D0, 0.0D0, t563)
      t685 = FJET(XB1, XB2, s, -t282, -t498, 0.0D0, -t495, t504, t545)
      t687 = FJET(XB1, XB2, s, -t387, 0.0D0, t385, 0.0D0, 0.0D0, t579)
      t689 = FJET(XB1, XB2, s, -t585, t583, t589, -t587, 0.0D0, t628)
      t691 = FJET(XB1, XB2, s, -t585, t656, t589, -t652, t504, -t674)
      t695 = FJET(XB1, XB2, s, -t498, -t282, -t495, 0.0D0, t504, t545)
      t697 = FJET(XB1, XB2, s, -t587, t589, t583, -t585, 0.0D0, t628)
      t699 = FJET(XB1, XB2, s, -t495, 0.0D0, -t498, -t282, t504, t545)
      t704 = 0.90D2 * t5 * t665 + t670
      t708 = FJET(XB1, XB2, s, -t652, t589, t656, -t585, t504, -t704 * t
     #43 * t356 / 0.720D3)
      t712 = t632 * t628 - t675 * t671 * t101 / 0.720D3 - t679 * t671 * 
     #t101 / 0.720D3 + t683 * t563 + t685 * t545 + t687 * t579 + t689 * 
     #t628 - t691 * t671 * t101 / 0.720D3 + t695 * t545 + t697 * t628 + 
     #t699 * t545 - t708 * t704 * t101 / 0.720D3
      rrgg2gght9s2e1 = t631 + t712

      end function



      doubleprecision function rrgg2gght9s2e0
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t18 = log(0.4D1 * t8 * t15)
      t19 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = pi * lh
      t25 = t4 * t19
      t27 = 0.180D3 * t24 * t25
      t29 = 0.1D1 / x3
      t31 = 0.1D1 / x1
      t34 = x2 * t7
      t35 = -0.1D1 + x2
      t36 = t15 * t35
      t39 = log(-0.4D1 * t36 * t34)
      t43 = log(0.4D1 * t34 * t15)
      t47 = 0.1D1 / x2
      t51 = t7 * t14
      t54 = log(0.4D1 * t51 * t11)
      t56 = t54 ** 2
      t59 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t68 = pi ** 2
      t70 = lh ** 2
      t72 = -0.30D2 * t68 + 0.180D3 * t70
      t73 = pi * t72
      t74 = t73 * t25
      t78 = x3 * t11
      t81 = log(0.4D1 * t78 * t14)
      t83 = t81 ** 2
      t98 = log(0.4D1 * t15)
      t99 = t98 * pi
      t102 = t98 ** 2
      t103 = t102 * pi
      t109 = rrgg2ggh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t135 = x2 * x3
      t138 = log(0.4D1 * t135 * t15)
      t142 = log(-0.4D1 * t135 * t36)
      t149 = x2 * t11
      t153 = log(-0.4D1 * t149 * t14 * t35)
      t154 = t153 ** 2
      t157 = log(0.4D1 * t149 * t14)
      t158 = t157 ** 2
      t172 = -(0.90D2 * t5 * (t6 - t18 * t19) - t27) * t29 * t31 / 0.720
     #D3 - t5 * (t39 * t19 - t43 * t19) * t31 * t47 / 0.8D1 - (0.90D2 * 
     #t5 * (-t54 * t6 + t56 * t19 / 0.2D1 + t59) - 0.180D3 * t24 * t4 * 
     #(t6 - t54 * t19) + t74) * t31 / 0.720D3 - (0.90D2 * t5 * (-t81 * t
     #6 + t83 * t19 / 0.2D1 + t59) - 0.180D3 * t24 * t4 * (t6 - t81 * t1
     #9) + t74) * t29 / 0.1440D4 - (0.180D3 * t99 * lh + 0.45D2 * t103 +
     # t73) * t4 * t6 / 0.1440D4 - t5 * t109 / 0.16D2 - (-0.90D2 * t103 
     #* lh + pi * (-0.240D3 * zeta3 - 0.120D3 * t70 * lh + 0.60D2 * lh *
     # t68) - 0.15D2 * t102 * t98 * pi - t99 * t72) * t4 * t19 / 0.1440D
     #4 - (-0.180D3 * t24 - 0.90D2 * t99) * t4 * t59 / 0.1440D4 - t5 * (
     #-t138 * t19 + t142 * t19) * t29 * t47 / 0.16D2 - (0.90D2 * t5 * t1
     #9 * (-t154 / 0.2D1 + t158 / 0.2D1) + (0.90D2 * t5 * t6 - t27) * (t
     #153 - t157)) * t47 / 0.1440D4
      t173 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t172)
      t175 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t172)
      t177 = t2 * x1
      t178 = -0.1D1 + x1
      t179 = t2 * t178
      t180 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t177, 0.
     #0D0, -t179, 0.0D0)
      t181 = t8 * t14
      t182 = 0.1D1 / t9
      t183 = t178 ** 2
      t185 = x1 * z
      t186 = -z - x1 + t185
      t187 = 0.1D1 / t186
      t188 = t182 * t183 * t187
      t191 = log(-0.4D1 * t181 * t188)
      t192 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t177, 0.
     #0D0, -t179, 0.0D0)
      t197 = t4 * t192
      t199 = 0.180D3 * t24 * t197
      t203 = (0.90D2 * t5 * (-t180 + t191 * t192) + t199) * t29 * t31 / 
     #0.720D3
      t206 = t29 * t31 * t47
      t208 = t5 * t192 * t206 / 0.8D1
      t209 = t34 * t14
      t212 = log(-0.4D1 * t209 * t188)
      t220 = (0.90D2 * t5 * (-t180 + t212 * t192) + t199) * t31 * t47 / 
     #0.720D3
      t223 = log(-0.4D1 * t51 * t188)
      t225 = t223 ** 2
      t228 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t177, 0.
     #0D0, -t179, 0.0D0)
      t229 = t223 * t180 - t225 * t192 / 0.2D1 - t228
      t233 = -t180 + t223 * t192
      t237 = t73 * t197
      t241 = -t203 + t208 - t220 - (0.90D2 * t5 * t229 - 0.180D3 * t24 *
     # t4 * t233 - t237) * t31 / 0.720D3
      t242 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t177, -t179, 0.0D0, t241)
      t244 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t179, t177, 0.0D0, t241)
      t246 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t172)
      t248 = t2 * x3
      t249 = -0.1D1 + x3
      t250 = t2 * t249
      t251 = t14 * t249
      t254 = log(-0.4D1 * t78 * t251)
      t255 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #248, -t250, 0.0D0)
      t257 = t254 ** 2
      t258 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #248, -t250, 0.0D0)
      t261 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #248, -t250, 0.0D0)
      t262 = t254 * t255 - t257 * t258 / 0.2D1 - t261
      t266 = -t255 + t254 * t258
      t270 = t4 * t258
      t271 = t73 * t270
      t278 = log(-0.4D1 * t8 * t15 * t249)
      t288 = (0.90D2 * t5 * (-t255 + t278 * t258) + 0.180D3 * t24 * t270
     #) * t29 * t31 / 0.720D3
      t293 = log(-0.4D1 * t149 * t14 * x3 * t249)
      t299 = log(0.4D1 * t135 * t11 * t251 * t35)
      t305 = t5 * (t293 * t258 - t299 * t258) * t29 * t47 / 0.16D2
      t306 = -(0.90D2 * t5 * t262 - 0.180D3 * t24 * t4 * t266 - t271) * 
     #t29 / 0.1440D4 - t288 - t305
      t307 = FJET(XB1, XB2, s, 0.0D0, t248, 0.0D0, -t250, 0.0D0, t306)
      t309 = FJET(XB1, XB2, s, 0.0D0, -t250, 0.0D0, t248, 0.0D0, t306)
      t311 = x1 * x2
      t313 = t2 * t311 * t187
      t315 = t1 * x1
      t316 = t35 * s * t315
      t317 = t1 ** 2
      t322 = s * t317 * x2 * x1 * t178 * t187
      t323 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t313, -t316, 0
     #.0D0, -t179, t322)
      t327 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t313, -t316, 0
     #.0D0, -t179, t322)
      t328 = t182 * t187
      t333 = log(0.4D1 * t209 * t328 * t183 * t35)
      t345 = -t5 * t323 * t206 / 0.8D1 - (0.90D2 * t5 * (t327 - t333 * t
     #323) - 0.180D3 * t24 * t4 * t323) * t31 * t47 / 0.720D3
      t346 = FJET(XB1, XB2, s, 0.0D0, -t313, -t179, -t316, t322, t345)
      t348 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t172)
      t360 = -t203 + t208 - t220 - (0.90D2 * t5 * t229 - 0.180D3 * t24 *
     # t4 * t233 - t237) * t31 / 0.720D3
      t361 = FJET(XB1, XB2, s, t177, -t179, 0.0D0, 0.0D0, 0.0D0, t360)
      t373 = -(0.90D2 * t5 * t262 - 0.180D3 * t24 * t4 * t266 - t271) * 
     #t29 / 0.1440D4 - t288 - t305
      t374 = FJET(XB1, XB2, s, t248, 0.0D0, -t250, 0.0D0, 0.0D0, t373)
      t376 = x3 * x1
      t377 = t2 * t376
      t379 = t2 * t178 * x3
      t380 = t249 * s
      t381 = t380 * t315
      t383 = t380 * t1 * t178
      t384 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, t377, -t381, -t
     #379, t383, 0.0D0)
      t389 = log(0.4D1 * t181 * t328 * t183 * t249)
      t390 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t377, -t381, -t
     #379, t383, 0.0D0)
      t405 = -(0.90D2 * t5 * (t384 - t389 * t390) - 0.180D3 * t24 * t4 *
     # t390) * t29 * t31 / 0.720D3 - t5 * t390 * t206 / 0.8D1
      t406 = FJET(XB1, XB2, s, t377, -t379, -t381, t383, 0.0D0, t405)
      t408 = t173 * t172 + t175 * t172 + t242 * t241 + t244 * t241 + t24
     #6 * t172 + t307 * t306 + t309 * t306 + t346 * t345 + t348 * t172 +
     # t361 * t360 + t374 * t373 + t406 * t405
      t409 = FJET(XB1, XB2, s, t383, -t381, -t379, t377, 0.0D0, t405)
      t411 = x2 * z
      t413 = x3 * z
      t414 = t376 * z
      t415 = t135 * z
      t416 = t376 * x2
      t417 = t376 * t411
      t418 = cos(t12)
      t423 = Sqrt(-x3 * t35 * t186 * x2 * t249)
      t425 = 0.2D1 * t418 * t423
      t426 = z + x1 - t185 - t411 - t311 + t311 * z - t413 - t376 + t414
     # + t415 + t416 - t417 + t135 + t425
      t429 = t2 * x1 * t426 * t187
      t433 = t2 * x1 * (-t413 - t376 + t414 + t415 + t416 - t417 - x2 + 
     #t135 + t425) * t187
      t434 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t433, -t429, -t
     #379, t383, t322)
      t437 = t5 * t434 * t206 / 0.8D1
      t438 = FJET(XB1, XB2, s, t383, -t429, -t379, t433, t322, t437)
      t443 = t434 * t29 * t31 * t47
      t446 = FJET(XB1, XB2, s, t433, -t379, -t429, t383, t322, t437)
      t451 = FJET(XB1, XB2, s, -t179, t177, 0.0D0, 0.0D0, 0.0D0, t360)
      t453 = FJET(XB1, XB2, s, -t179, -t316, 0.0D0, -t313, t322, t345)
      t455 = FJET(XB1, XB2, s, -t250, 0.0D0, t248, 0.0D0, 0.0D0, t373)
      t457 = FJET(XB1, XB2, s, -t379, t377, t383, -t381, 0.0D0, t405)
      t459 = FJET(XB1, XB2, s, -t379, t433, t383, -t429, t322, t437)
      t464 = FJET(XB1, XB2, s, -t316, -t179, -t313, 0.0D0, t322, t345)
      t466 = FJET(XB1, XB2, s, -t381, t383, t377, -t379, 0.0D0, t405)
      t468 = FJET(XB1, XB2, s, -t313, 0.0D0, -t316, -t179, t322, t345)
      t470 = FJET(XB1, XB2, s, -t429, t383, t433, -t379, t322, t437)
      t475 = t409 * t405 + t438 * pi * t4 * t443 / 0.8D1 + t446 * pi * t
     #4 * t443 / 0.8D1 + t451 * t360 + t453 * t345 + t455 * t373 + t457 
     #* t405 + t459 * pi * t4 * t443 / 0.8D1 + t464 * t345 + t466 * t405
     # + t468 * t345 + t470 * pi * t4 * t443 / 0.8D1
      rrgg2gght9s2e0 = t408 + t475

      end function



      doubleprecision function rrgg2gght9s2em1
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * pi)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t30 = x1 ** 2
      t31 = t30 * t13
      t34 = log(0.4D1 * t31 * t9)
      t40 = 0.1D1 / x1
      t47 = x2 * t9
      t48 = -0.1D1 + x2
      t52 = log(-0.4D1 * t47 * t13 * t48)
      t55 = log(0.4D1 * t47 * t13)
      t58 = 0.1D1 / x2
      t65 = log(0.4D1 * t9 * t13)
      t66 = t65 * pi
      t74 = t65 ** 2
      t77 = pi ** 2
      t79 = lh ** 2
      t87 = rrgg2ggh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t90 = -(0.90D2 * t5 * (t6 - t16 * t17) - t25) * t27 / 0.1440D4 - (
     #0.90D2 * t5 * (t6 - t34 * t17) - t25) * t40 / 0.720D3 - t5 * t17 *
     # t27 * t40 / 0.8D1 - t5 * t17 * (t52 - t55) * t58 / 0.16D2 - (-0.1
     #80D3 * t22 - 0.90D2 * t66) * t4 * t6 / 0.1440D4 - (0.180D3 * t66 *
     # lh + 0.45D2 * t74 * pi + pi * (-0.30D2 * t77 + 0.180D3 * t79)) * 
     #t4 * t17 / 0.1440D4 - t5 * t87 / 0.16D2
      t91 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t90)
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t90)
      t95 = t2 * x1
      t96 = -0.1D1 + x1
      t97 = t2 * t96
      t98 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t95, 0.0D
     #0, -t97, 0.0D0)
      t102 = t5 * t98 * t40 * t58 / 0.8D1
      t103 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t95, 0.0
     #D0, -t97, 0.0D0)
      t107 = 0.1D1 / (-z - x1 + x1 * z)
      t109 = t96 ** 2
      t113 = log(-0.4D1 * t31 / t7 * t107 * t109)
      t115 = -t103 + t113 * t98
      t120 = 0.180D3 * t22 * t4 * t98
      t127 = t5 * t98 * t27 * t40 / 0.8D1
      t128 = t102 - (0.90D2 * t5 * t115 + t120) * t40 / 0.720D3 + t127
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t95, -t97, 0.0D0, t128)
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t97, t95, 0.0D0, t128)
      t133 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t90)
      t135 = t2 * x3
      t136 = -0.1D1 + x3
      t137 = t2 * t136
      t138 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #135, -t137, 0.0D0)
      t142 = t5 * t138 * t27 * t40 / 0.8D1
      t143 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #135, -t137, 0.0D0)
      t147 = log(-0.4D1 * t10 * t13 * t136)
      t149 = -t143 + t147 * t138
      t154 = 0.180D3 * t22 * t4 * t138
      t158 = t142 - (0.90D2 * t5 * t149 + t154) * t27 / 0.1440D4
      t159 = FJET(XB1, XB2, s, 0.0D0, t135, 0.0D0, -t137, 0.0D0, t158)
      t161 = FJET(XB1, XB2, s, 0.0D0, -t137, 0.0D0, t135, 0.0D0, t158)
      t165 = t2 * x1 * x2 * t107
      t167 = t1 * x1
      t168 = t48 * s * t167
      t169 = t1 ** 2
      t174 = s * t169 * x2 * x1 * t96 * t107
      t175 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t165, -t168, 0
     #.0D0, -t97, t174)
      t177 = t175 * t40 * t58
      t179 = t5 * t177 / 0.8D1
      t180 = FJET(XB1, XB2, s, 0.0D0, -t165, -t97, -t168, t174, -t179)
      t185 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t90)
      t193 = t102 - (0.90D2 * t5 * t115 + t120) * t40 / 0.720D3 + t127
      t194 = FJET(XB1, XB2, s, t95, -t97, 0.0D0, 0.0D0, 0.0D0, t193)
      t202 = t142 - (0.90D2 * t5 * t149 + t154) * t27 / 0.1440D4
      t203 = FJET(XB1, XB2, s, t135, 0.0D0, -t137, 0.0D0, 0.0D0, t202)
      t206 = t2 * x1 * x3
      t208 = t2 * t96 * x3
      t209 = t136 * s
      t210 = t209 * t167
      t212 = t209 * t1 * t96
      t213 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, t206, -t210, -t
     #208, t212, 0.0D0)
      t215 = t213 * t27 * t40
      t217 = t5 * t215 / 0.8D1
      t218 = FJET(XB1, XB2, s, t206, -t208, -t210, t212, 0.0D0, -t217)
      t223 = FJET(XB1, XB2, s, t212, -t210, -t208, t206, 0.0D0, -t217)
      t228 = FJET(XB1, XB2, s, -t97, t95, 0.0D0, 0.0D0, 0.0D0, t193)
      t230 = FJET(XB1, XB2, s, -t97, -t168, 0.0D0, -t165, t174, -t179)
      t235 = FJET(XB1, XB2, s, -t137, 0.0D0, t135, 0.0D0, 0.0D0, t202)
      t237 = FJET(XB1, XB2, s, -t208, t206, t212, -t210, 0.0D0, -t217)
      t242 = FJET(XB1, XB2, s, -t168, -t97, -t165, 0.0D0, t174, -t179)
      t247 = FJET(XB1, XB2, s, -t210, t212, t206, -t208, 0.0D0, -t217)
      t252 = FJET(XB1, XB2, s, -t165, 0.0D0, -t168, -t97, t174, -t179)
      rrgg2gght9s2em1 = t91 * t90 + t93 * t90 + t129 * t128 + t131 * t12
     #8 + t133 * t90 + t159 * t158 + t161 * t158 - t180 * pi * t4 * t177
     # / 0.8D1 + t185 * t90 + t194 * t193 + t203 * t202 - t218 * pi * t4
     # * t215 / 0.8D1 - t223 * pi * t4 * t215 / 0.8D1 + t228 * t193 - t2
     #30 * pi * t4 * t177 / 0.8D1 + t235 * t202 - t237 * pi * t4 * t215 
     #/ 0.8D1 - t242 * pi * t4 * t177 / 0.8D1 - t247 * pi * t4 * t215 / 
     #0.8D1 - t252 * pi * t4 * t177 / 0.8D1

      end function



      doubleprecision function rrgg2gght9s2em2
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t15 = rrgg2ggh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = z ** 2
      t24 = Sin(x4 * pi)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t35 = -t5 * t6 * t7 / 0.8D1 - t5 * t6 * t11 / 0.16D2 - t5 * t15 / 
     #0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t28 * pi) * t4 * t6 / 0.14
     #40D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t40 = t2 * x1
      t42 = t2 * (-0.1D1 + x1)
      t43 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t40, 0.0D
     #0, -t42, 0.0D0)
      t46 = t5 * t43 * t7 / 0.8D1
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t40, -t42, 0.0D0, t46)
      t50 = t4 * t43 * t7
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t42, t40, 0.0D0, t46)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t59 = t2 * x3
      t61 = t2 * (-0.1D1 + x3)
      t62 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t5
     #9, -t61, 0.0D0)
      t65 = t5 * t62 * t11 / 0.16D2
      t66 = FJET(XB1, XB2, s, 0.0D0, t59, 0.0D0, -t61, 0.0D0, t65)
      t69 = t4 * t62 * t11
      t72 = FJET(XB1, XB2, s, 0.0D0, -t61, 0.0D0, t59, 0.0D0, t65)
      t76 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t78 = FJET(XB1, XB2, s, t40, -t42, 0.0D0, 0.0D0, 0.0D0, t46)
      t82 = FJET(XB1, XB2, s, t59, 0.0D0, -t61, 0.0D0, 0.0D0, t65)
      t86 = FJET(XB1, XB2, s, -t42, t40, 0.0D0, 0.0D0, 0.0D0, t46)
      t90 = FJET(XB1, XB2, s, -t61, 0.0D0, t59, 0.0D0, 0.0D0, t65)
      rrgg2gght9s2em2 = t36 * t35 + t38 * t35 + t47 * pi * t50 / 0.8D1 +
     # t53 * pi * t50 / 0.8D1 + t57 * t35 + t66 * pi * t69 / 0.16D2 + t7
     #2 * pi * t69 / 0.16D2 + t76 * t35 + t78 * pi * t50 / 0.8D1 + t82 *
     # pi * t69 / 0.16D2 + t86 * pi * t50 / 0.8D1 + t90 * pi * t69 / 0.1
     #6D2

      end function



      doubleprecision function rrgg2gght9s2em3
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

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
      t4 = 0.1D1 / t3
      t6 = rrgg2ggh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gght9s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0.16D
     #2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gght9s2em4
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
      doubleprecision rrgg2ggh91J1
      doubleprecision rrgg2ggh91J2
      doubleprecision rrgg2ggh91J3
      doubleprecision rrgg2ggh91J4
      doubleprecision rrgg2ggh91J5
      doubleprecision rrgg2ggh91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght9s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh91J1
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
      t7 = S24 ** 2
      t13 = S14 ** 2
      t15 = S13 ** 2
      t18 = 0.1D1 / S12
      t26 = t15 ** 2
      t33 = t7 ** 2
      rrgg2ggh91J1 = ((0.9D1 * S12 + 0.9D1 * S24 + 0.9D1 / 0.2D1 * S13 -
     # 0.9D1 / 0.2D1 * S14 + (0.27D2 / 0.2D1 * S14 * S24 + 0.18D2 * t7 -
     # 0.9D1 * S13 * S14 - 0.27D2 / 0.2D1 * S13 * S24 + 0.9D1 / 0.2D1 * 
     #t13 + 0.9D1 / 0.2D1 * t15) * t18) * s * z + (-0.18D2 * S13 * t7 * 
     #S24 + 0.9D1 * t26 + 0.27D2 * t7 * t15 - 0.18D2 * t15 * S13 * S24 +
     # 0.9D1 * t33) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh91J2
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
      t5 = S24 ** 2
      t7 = S13 ** 2
      t11 = S14 ** 2
      t14 = 0.1D1 / S12
      t22 = t7 ** 2
      t29 = t5 ** 2
      rrgg2ggh91J2 = ((0.9D1 * S12 + 0.9D1 * S24 + 0.9D1 / 0.2D1 * S13 -
     # 0.9D1 / 0.2D1 * S14 + (0.18D2 * t5 + 0.9D1 / 0.2D1 * t7 - 0.9D1 *
     # S13 * S14 + 0.9D1 / 0.2D1 * t11) * t14) * s * z + (-0.18D2 * S13 
     #* t5 * S24 + 0.9D1 * t22 + 0.27D2 * t5 * t7 - 0.18D2 * S13 * t7 * 
     #S24 + 0.9D1 * t29) / (S12 + S13 + S23) * t14) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh91J3
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
      t5 = S24 ** 2
      t9 = S14 ** 2
      t11 = S13 ** 2
      t18 = 0.1D1 / S12
      t26 = t11 ** 2
      t33 = t5 ** 2
      rrgg2ggh91J3 = ((0.9D1 * S12 + 0.9D1 * S24 + 0.9D1 / 0.2D1 * S13 -
     # 0.9D1 / 0.2D1 * S14 + (0.18D2 * t5 + 0.27D2 / 0.2D1 * S13 * S24 +
     # 0.9D1 / 0.2D1 * t9 + 0.9D1 / 0.2D1 * t11 - 0.27D2 / 0.2D1 * S14 *
     # S24 - 0.9D1 * S13 * S14) * t18) * s * z + (-0.18D2 * S13 * t5 * S
     #24 + 0.9D1 * t26 + 0.27D2 * t5 * t11 - 0.18D2 * t11 * S13 * S24 + 
     #0.9D1 * t33) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh91J4
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
      t5 = S24 ** 2
      t9 = S14 ** 2
      t11 = S13 ** 2
      t18 = 0.1D1 / S12
      t26 = t11 ** 2
      t33 = t5 ** 2
      rrgg2ggh91J4 = ((0.9D1 * S12 + 0.9D1 * S24 + 0.9D1 / 0.2D1 * S13 -
     # 0.9D1 / 0.2D1 * S14 + (0.18D2 * t5 + 0.27D2 * S13 * S24 + 0.9D1 /
     # 0.2D1 * t9 + 0.9D1 / 0.2D1 * t11 - 0.27D2 * S14 * S24 - 0.9D1 * S
     #13 * S14) * t18) * s * z + (-0.18D2 * S13 * t5 * S24 + 0.9D1 * t26
     # + 0.27D2 * t5 * t11 - 0.18D2 * t11 * S13 * S24 + 0.9D1 * t33) / (
     #S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh91J5
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
      t5 = S24 ** 2
      t9 = S14 ** 2
      t11 = S13 ** 2
      t18 = 0.1D1 / S12
      t26 = t11 ** 2
      t33 = t5 ** 2
      rrgg2ggh91J5 = ((0.9D1 * S12 + 0.9D1 * S24 + 0.9D1 / 0.2D1 * S13 -
     # 0.9D1 / 0.2D1 * S14 + (0.18D2 * t5 + 0.81D2 / 0.2D1 * S13 * S24 +
     # 0.9D1 / 0.2D1 * t9 + 0.9D1 / 0.2D1 * t11 - 0.81D2 / 0.2D1 * S14 *
     # S24 - 0.9D1 * S13 * S14) * t18) * s * z + (-0.18D2 * S13 * t5 * S
     #24 + 0.9D1 * t26 + 0.27D2 * t5 * t11 - 0.18D2 * t11 * S13 * S24 + 
     #0.9D1 * t33) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh91J6
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
      t7 = S13 ** 2
      t11 = S14 ** 2
      t15 = S24 ** 2
      t18 = 0.1D1 / S12
      t23 = t15 ** 2
      t25 = t7 ** 2
      rrgg2ggh91J6 = ((-0.45D2 * S12 - 0.45D2 / 0.2D1 * S13 - 0.45D2 * S
     #24 + 0.45D2 / 0.2D1 * S14 + (0.135D3 * S13 * S24 - 0.45D2 / 0.2D1 
     #* t7 + 0.45D2 * S13 * S14 - 0.45D2 / 0.2D1 * t11 - 0.135D3 * S14 *
     # S24 - 0.90D2 * t15) * t18) * s * z + (-0.45D2 * t23 - 0.45D2 * t2
     #5 + 0.90D2 * t15 * S13 * S24 - 0.135D3 * t7 * t15 + 0.90D2 * S13 *
     # t7 * S24) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
 