  
      subroutine rrgq2qght8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh81J1  
      doubleprecision rrgq2qgh81J2  
      doubleprecision rrgq2qgh81J3  
      doubleprecision rrgq2qgh81J4  
      doubleprecision rrgq2qgh81J5  
      doubleprecision rrgq2qgh81J6  
      doubleprecision rrgq2qgh81J7  
      doubleprecision rrgq2qgh82J1  
      doubleprecision rrgq2qgh82J2  
      doubleprecision rrgq2qgh82J3  
      doubleprecision rrgq2qgh82J4  
      doubleprecision rrgq2qgh82J5  
      doubleprecision rrgq2qgh82J6  
      doubleprecision rrgq2qgh83J1  
      doubleprecision rrgq2qgh83J2  
      doubleprecision rrgq2qgh83J3  
      doubleprecision rrgq2qgh83J4  
      doubleprecision rrgq2qgh83J5  
      doubleprecision rrgq2qgh83J6  
      doubleprecision rrgq2qgh83J7  
      doubleprecision rrgq2qgh84J1  
      doubleprecision rrgq2qgh84J2  
      doubleprecision rrgq2qgh84J3  
      doubleprecision rrgq2qgh84J4  
      doubleprecision rrgq2qgh84J5  
      doubleprecision rrgq2qgh84J6  
      doubleprecision rrgq2qght8s1e1  
      doubleprecision rrgq2qght8s1e0  
      doubleprecision rrgq2qght8s1em1  
      doubleprecision rrgq2qght8s1em2  
      doubleprecision rrgq2qght8s1em3  
      doubleprecision rrgq2qght8s1em4  
      doubleprecision rrgq2qght8s2e1  
      doubleprecision rrgq2qght8s2e0  
      doubleprecision rrgq2qght8s2em1  
      doubleprecision rrgq2qght8s2em2  
      doubleprecision rrgq2qght8s2em3  
      doubleprecision rrgq2qght8s2em4  
      doubleprecision rrgq2qght8s3e1  
      doubleprecision rrgq2qght8s3e0  
      doubleprecision rrgq2qght8s3em1  
      doubleprecision rrgq2qght8s3em2  
      doubleprecision rrgq2qght8s3em3  
      doubleprecision rrgq2qght8s3em4  
      doubleprecision rrgq2qght8s4e1  
      doubleprecision rrgq2qght8s4e0  
      doubleprecision rrgq2qght8s4em1  
      doubleprecision rrgq2qght8s4em2  
      doubleprecision rrgq2qght8s4em3  
      doubleprecision rrgq2qght8s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght8s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * 0.3141592653589793D1
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t9 = t1 ** 2
      t10 = t9 ** 2
      t11 = t8 * t10
      t13 = log(0.4D1 * t11)
      t14 = t13 ** 2
      t15 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t18 = t14 * t13
      t19 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t22 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t23 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t26 = 0.1D1 / t1
      t28 = s ** 2
      t29 = 0.1D1 / t28
      t30 = t29 * 0.3141592653589793D1
      t31 = t30 * lh
      t39 = lh ** 2
      t41 = 0.3141592653589793D1 ** 2
      t43 = -0.180D3 * t39 + 0.30D2 * t41
      t44 = t30 * t43
      t54 = -0.60D2 * lh * t41 + 0.2884936567583026D3 + 0.120D3 * t39 * 
     #lh
      t55 = t30 * t54
      t58 = t26 * t29
      t60 = t41 ** 2
      t61 = t39 ** 2
      t66 = 0.3141592653589793D1 * (-0.5769873135166051D3 * lh - t60 - 0
     #.60D2 * t61 + 0.60D2 * t39 * t41)
      t75 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t76 = t14 ** 2
      t83 = x1 ** 2
      t84 = x3 * t83
      t87 = log(0.4D1 * t84 * t11)
      t89 = t87 ** 2
      t92 = z * t23
      t93 = t84 * t7
      t94 = t4 * t10
      t95 = -0.1D1 + x3
      t96 = 0.1D1 / t95
      t97 = t94 * t96
      t100 = log(-0.4D1 * t93 * t97)
      t101 = t100 * z
      t103 = t100 ** 2
      t104 = t103 * z
      t108 = cos(t5)
      t110 = Sqrt(-x3 * t95)
      t115 = 0.1D1 / (-x3 - z + 0.2D1 * t108 * t110 * z)
      t121 = 0.3141592653589793D1 * lh
      t123 = z * t15
      t131 = 0.3141592653589793D1 * t43
      t134 = -t19 - z * t19 * t115
      t136 = t58 * t131 * t134
      t138 = 0.1D1 / x3
      t140 = 0.1D1 / x1
      t143 = t83 * t7
      t146 = log(0.4D1 * t143 * t94)
      t152 = t146 ** 2
      t155 = t152 * t146
      t162 = 0.3141592653589793D1 * t54
      t164 = t58 * t162 * t19
      t175 = x2 ** 2
      t176 = x3 * t175
      t177 = t176 * t83
      t180 = log(0.4D1 * t177 * t11)
      t186 = log(-0.4D1 * t177 * t8 * t10 * t96)
      t187 = t186 * z
      t200 = 0.1D1 / x2
      t201 = t200 * t140
      t204 = t175 * t83
      t207 = log(0.4D1 * t204 * t11)
      t209 = t207 ** 2
      t222 = t58 * t131 * t19
      t234 = x3 * t7
      t237 = log(-0.4D1 * t234 * t97)
      t242 = log(0.4D1 * t234 * t94)
      t243 = t237 * z * t115 + t242
      t246 = t242 ** 2
      t248 = t237 ** 2
      t253 = t246 * t242 / 0.6D1 + t248 * t237 * z * t115 / 0.6D1
      t267 = -z * t115 - 0.1D1
      t279 = -t248 * z * t115 / 0.2D1 - t246 / 0.2D1
      t284 = t176 * t7
      t287 = log(-0.4D1 * t284 * t97)
      t288 = t287 * z
      t290 = t287 ** 2
      t291 = t290 * z
      t298 = log(0.4D1 * t176 * t11)
      t300 = t298 ** 2
      t319 = t175 * t7
      t322 = log(0.4D1 * t319 * t94)
      t328 = t322 ** 2
      t331 = t328 * t322
      t348 = -(t14 * t15 / 0.2D1 - t18 * t19 / 0.6D1 + t22 - t13 * t23) 
     #* t26 * t31 / 0.16D2 - (t23 - t13 * t15 + t14 * t19 / 0.2D1) * t26
     # * t44 / 0.2880D4 - (t15 - t13 * t19) * t26 * t55 / 0.2880D4 - t58
     # * t66 * t19 / 0.2880D4 + (-t18 * t15 / 0.6D1 + t14 * t23 / 0.2D1 
     #- t13 * t22 + t75 + t76 * t19 / 0.24D2) * t26 * t30 / 0.32D2 + (-0
     #.90D2 * t58 * 0.3141592653589793D1 * (-t23 + t87 * t15 - t89 * t19
     # / 0.2D1 - (t92 - t101 * t15 + t104 * t19 / 0.2D1) * t115) + 0.180
     #D3 * t58 * t121 * (t87 * t19 - t15 - (t123 - t101 * t19) * t115) +
     # t136) * t138 * t140 / 0.1440D4 + (t58 * t131 * (-t15 + t146 * t19
     #) - 0.90D2 * t58 * 0.3141592653589793D1 * (t146 * t23 - t152 * t15
     # / 0.2D1 + t155 * t19 / 0.6D1 - t22) - t164 + 0.180D3 * t58 * t121
     # * (-t23 + t146 * t15 - t152 * t19 / 0.2D1)) * t140 / 0.1440D4 + (
     #-0.90D2 * t58 * 0.3141592653589793D1 * (t180 * t19 - t15 - (t123 -
     # t187 * t19) * t115) + 0.180D3 * t58 * t121 * t134) * t138 * t201 
     #/ 0.720D3 + (-0.90D2 * t58 * 0.3141592653589793D1 * (-t23 + t207 *
     # t15 - t209 * t19 / 0.2D1) + 0.180D3 * t58 * t121 * (-t15 + t207 *
     # t19) - t222) * t200 * t140 / 0.720D3 + ((-0.90D2 * t58 * 0.314159
     #2653589793D1 * t23 + 0.180D3 * t58 * t121 * t15 + t222) * t243 - 0
     #.90D2 * t58 * 0.3141592653589793D1 * t19 * t253 + (t58 * t131 * t1
     #5 - 0.90D2 * t58 * 0.3141592653589793D1 * t22 + t164 + 0.180D3 * t
     #58 * t121 * t23) * t267 + (-0.90D2 * t58 * 0.3141592653589793D1 * 
     #t15 + 0.180D3 * t58 * t121 * t19) * t279) * t138 / 0.2880D4 + (-0.
     #90D2 * t58 * 0.3141592653589793D1 * (-(t92 - t288 * t15 + t291 * t
     #19 / 0.2D1) * t115 - t23 + t298 * t15 - t300 * t19 / 0.2D1) + 0.18
     #0D3 * t58 * t121 * (-t15 - (t123 - t288 * t19) * t115 + t298 * t19
     #) + t136) * t138 * t200 / 0.1440D4 - (t58 * t131 * (t15 - t322 * t
     #19) - 0.90D2 * t58 * 0.3141592653589793D1 * (-t322 * t23 + t328 * 
     #t15 / 0.2D1 - t331 * t19 / 0.6D1 + t22) + t164 + 0.180D3 * t58 * t
     #121 * (t23 - t322 * t15 + t328 * t19 / 0.2D1)) * t200 / 0.1440D4
      t349 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t348)
      t351 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t352 = z * t351
      t353 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t355 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t367 = z * t353
      t378 = -z * t355 * t115 - t355
      t380 = t58 * t131 * t378
      t390 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t400 = t58 * t162 * t355
      t441 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t454 = t58 * t131 * t355
      t560 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-(t352 - t288 * t3
     #53 + t291 * t355 / 0.2D1) * t115 + t298 * t353 - t351 - t300 * t35
     #5 / 0.2D1) + 0.180D3 * t58 * t121 * (-t353 - (t367 - t288 * t355) 
     #* t115 + t298 * t355) + t380) * t138 * t200 / 0.1440D4 - (t58 * t1
     #31 * (t353 - t322 * t355) - 0.90D2 * t58 * 0.3141592653589793D1 * 
     #(-t322 * t351 + t390 - t331 * t355 / 0.6D1 + t328 * t353 / 0.2D1) 
     #+ t400 + 0.180D3 * t58 * t121 * (-t322 * t353 + t351 + t328 * t355
     # / 0.2D1)) * t200 / 0.1440D4 - (t390 + t14 * t353 / 0.2D1 - t13 * 
     #t351 - t18 * t355 / 0.6D1) * t26 * t31 / 0.16D2 - (-t13 * t353 + t
     #351 + t14 * t355 / 0.2D1) * t26 * t44 / 0.2880D4 - (t353 - t13 * t
     #355) * t26 * t55 / 0.2880D4 - t58 * t66 * t355 / 0.2880D4 + (-t18 
     #* t353 / 0.6D1 + t14 * t351 / 0.2D1 + t76 * t355 / 0.24D2 + t441 -
     # t13 * t390) * t26 * t30 / 0.32D2 + ((-0.90D2 * t58 * 0.3141592653
     #589793D1 * t351 + 0.180D3 * t58 * t121 * t353 + t454) * t243 - 0.9
     #0D2 * t58 * 0.3141592653589793D1 * t355 * t253 + (t58 * t131 * t35
     #3 - 0.90D2 * t58 * 0.3141592653589793D1 * t390 + t400 + 0.180D3 * 
     #t58 * t121 * t351) * t267 + (-0.90D2 * t58 * 0.3141592653589793D1 
     #* t353 + 0.180D3 * t58 * t121 * t355) * t279) * t138 / 0.2880D4 + 
     #(-0.90D2 * t58 * 0.3141592653589793D1 * (-(t352 - t101 * t353 + t1
     #04 * t355 / 0.2D1) * t115 - t89 * t355 / 0.2D1 + t87 * t353 - t351
     #) + 0.180D3 * t58 * t121 * (-t353 + t87 * t355 - (t367 - t101 * t3
     #55) * t115) + t380) * t138 * t140 / 0.1440D4 + (t58 * t131 * (-t35
     #3 + t146 * t355) - 0.90D2 * t58 * 0.3141592653589793D1 * (t146 * t
     #351 - t390 + t155 * t355 / 0.6D1 - t152 * t353 / 0.2D1) - t400 + 0
     #.180D3 * t58 * t121 * (t146 * t353 - t351 - t152 * t355 / 0.2D1)) 
     #* t140 / 0.1440D4 + (-0.90D2 * t58 * 0.3141592653589793D1 * (-(t36
     #7 - t187 * t355) * t115 - t353 + t180 * t355) + 0.180D3 * t58 * t1
     #21 * t378) * t138 * t201 / 0.720D3 + (-0.90D2 * t58 * 0.3141592653
     #589793D1 * (t207 * t353 - t351 - t209 * t355 / 0.2D1) + 0.180D3 * 
     #t58 * t121 * (-t353 + t207 * t355) - t454) * t200 * t140 / 0.720D3
      t561 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t560)
      t563 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t565 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t566 = z * t565
      t568 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t580 = z * t563
      t590 = -z * t568 * t115 - t568
      t592 = t58 * t131 * t590
      t601 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t612 = t58 * t162 * t568
      t651 = t58 * t131 * t568
      t764 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t772 = (-0.90D2 * t58 * 0.3141592653589793D1 * (t87 * t563 - (t566
     # - t101 * t563 + t104 * t568 / 0.2D1) * t115 - t565 - t89 * t568 /
     # 0.2D1) + 0.180D3 * t58 * t121 * (-t563 + t87 * t568 - (t580 - t10
     #1 * t568) * t115) + t592) * t138 * t140 / 0.1440D4 + (t58 * t131 *
     # (-t563 + t146 * t568) - 0.90D2 * t58 * 0.3141592653589793D1 * (-t
     #601 + t155 * t568 / 0.6D1 + t146 * t565 - t152 * t563 / 0.2D1) - t
     #612 + 0.180D3 * t58 * t121 * (-t152 * t568 / 0.2D1 - t565 + t146 *
     # t563)) * t140 / 0.1440D4 + (-0.90D2 * t58 * 0.3141592653589793D1 
     #* (-t563 + t180 * t568 - (t580 - t187 * t568) * t115) + 0.180D3 * 
     #t58 * t121 * t590) * t138 * t201 / 0.720D3 + (-0.90D2 * t58 * 0.31
     #41592653589793D1 * (-t209 * t568 / 0.2D1 - t565 + t207 * t563) + 0
     #.180D3 * t58 * t121 * (-t563 + t207 * t568) - t651) * t200 * t140 
     #/ 0.720D3 + (-0.90D2 * t58 * 0.3141592653589793D1 * (-t565 - (t566
     # - t288 * t563 + t291 * t568 / 0.2D1) * t115 - t300 * t568 / 0.2D1
     # + t298 * t563) + 0.180D3 * t58 * t121 * (-(t580 - t288 * t568) * 
     #t115 + t298 * t568 - t563) + t592) * t138 * t200 / 0.1440D4 - (t58
     # * t131 * (t563 - t322 * t568) - 0.90D2 * t58 * 0.3141592653589793
     #D1 * (t601 - t331 * t568 / 0.6D1 - t322 * t565 + t328 * t563 / 0.2
     #D1) + t612 + 0.180D3 * t58 * t121 * (t328 * t568 / 0.2D1 + t565 - 
     #t322 * t563)) * t200 / 0.1440D4 - (t14 * t563 / 0.2D1 - t13 * t565
     # + t601 - t18 * t568 / 0.6D1) * t26 * t31 / 0.16D2 - (t14 * t568 /
     # 0.2D1 + t565 - t13 * t563) * t26 * t44 / 0.2880D4 - (t563 - t13 *
     # t568) * t26 * t55 / 0.2880D4 - t58 * t66 * t568 / 0.2880D4 + ((-0
     #.90D2 * t58 * 0.3141592653589793D1 * t565 + 0.180D3 * t58 * t121 *
     # t563 + t651) * t243 - 0.90D2 * t58 * 0.3141592653589793D1 * t568 
     #* t253 + (t58 * t131 * t563 - 0.90D2 * t58 * 0.3141592653589793D1 
     #* t601 + t612 + 0.180D3 * t58 * t121 * t565) * t267 + (-0.90D2 * t
     #58 * 0.3141592653589793D1 * t563 + 0.180D3 * t58 * t121 * t568) * 
     #t279) * t138 / 0.2880D4 + (-t18 * t563 / 0.6D1 + t76 * t568 / 0.24
     #D2 + t764 + t14 * t565 / 0.2D1 - t13 * t601) * t26 * t30 / 0.32D2
      t773 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t772)
      t775 = t2 * x1
      t776 = -0.1D1 + x1
      t777 = x1 * z
      t778 = 0.1D1 - x1 + t777
      t779 = 0.1D1 / t778
      t781 = t2 * t776 * t779
      t782 = s * t9
      t784 = x1 * t776 * t779
      t785 = t782 * t784
      t786 = z * t778
      t787 = -t776
      t788 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.10
     #D1, x4)
      t791 = t10 * t779
      t792 = t776 ** 2
      t797 = log(-0.4D1 * t84 * t8 * t791 * t792 * t96)
      t798 = t797 * z
      t799 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.10
     #D1, x4)
      t802 = t797 ** 2
      t803 = t802 * z
      t804 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.10
     #D1, x4)
      t805 = t778 * t804
      t809 = x3 * x1
      t810 = 0.2D1 * t809
      t811 = t809 * z
      t812 = 0.3D1 * t811
      t813 = x1 * t3
      t814 = x3 * t3
      t815 = t814 * x1
      t817 = 0.2D1 * t84 * z
      t818 = t84 * t3
      t819 = x3 * t778
      t821 = Sqrt(-t819 * t95)
      t825 = -z + t810 - t84 + t777 - t812 - t813 + t815 + t817 - t818 -
     # x3 + 0.2D1 * t108 * t821 * z
      t826 = 0.1D1 / t825
      t828 = t779 * t792
      t829 = t94 * t828
      t832 = log(0.4D1 * t93 * t829)
      t834 = t832 ** 2
      t841 = t786 * t799
      t852 = t804 + t786 * t804 * t826
      t859 = t143 * t4
      t860 = t791 * t792
      t863 = log(0.4D1 * t859 * t860)
      t869 = t863 ** 2
      t872 = t869 * t863
      t875 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.10
     #D1, x4)
      t892 = t176 * t143
      t897 = log(-0.4D1 * t892 * t94 * t828 * t96)
      t898 = t897 * z
      t904 = log(0.4D1 * t892 * t829)
      t917 = t204 * t7
      t920 = log(0.4D1 * t917 * t829)
      t921 = t920 ** 2
      t940 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-(-t786 * t788 + t
     #798 * t778 * t799 - t803 * t805 / 0.2D1) * t826 - t832 * t799 + t8
     #34 * t804 / 0.2D1 + t788) + 0.180D3 * t58 * t121 * (-(-t841 + t798
     # * t805) * t826 - t832 * t804 + t799) + t58 * t131 * t852) * t138 
     #* t140 / 0.1440D4 + (t58 * t131 * (t799 - t863 * t804) - 0.90D2 * 
     #t58 * 0.3141592653589793D1 * (-t863 * t788 + t869 * t799 / 0.2D1 -
     # t872 * t804 / 0.6D1 + t875) + t58 * t162 * t804 + 0.180D3 * t58 *
     # t121 * (t788 + t869 * t804 / 0.2D1 - t863 * t799)) * t140 / 0.144
     #0D4 + (-0.90D2 * t58 * 0.3141592653589793D1 * (t799 - (-t841 + t89
     #8 * t805) * t826 - t904 * t804) + 0.180D3 * t58 * t121 * t852) * t
     #138 * t201 / 0.720D3 + (-0.90D2 * t58 * 0.3141592653589793D1 * (t7
     #88 + t921 * t804 / 0.2D1 - t920 * t799) + 0.180D3 * t58 * t121 * (
     #t799 - t920 * t804) + t58 * t131 * t804) * t200 * t140 / 0.720D3
      t941 = FJET(XB1, XB2, s, 0.0D0, t775, -t781, 0.0D0, -t785, t940)
      t943 = x2 * s
      t944 = t943 * t1
      t945 = -0.1D1 + x2
      t946 = t945 * s
      t947 = t946 * t1
      t948 = -t945
      t949 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.10
     #D1, x4)
      t950 = t94 * t945
      t953 = log(-0.4D1 * t284 * t950)
      t954 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.10
     #D1, x4)
      t956 = t953 ** 2
      t957 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.10
     #D1, x4)
      t970 = t58 * t131 * t957
      t977 = log(-0.4D1 * t319 * t950)
      t982 = t977 ** 2
      t983 = t982 * t977
      t989 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.10
     #D1, x4)
      t1006 = t10 * t945
      t1010 = log(-0.4D1 * t177 * t8 * t1006)
      t1025 = log(-0.4D1 * t917 * t950)
      t1027 = t1025 ** 2
      t1043 = (-0.90D2 * t58 * 0.3141592653589793D1 * (t949 - t953 * t95
     #4 + t956 * t957 / 0.2D1) + 0.180D3 * t58 * t121 * (-t953 * t957 + 
     #t954) + t970) * t138 * t200 / 0.1440D4 - (t58 * t131 * (-t954 + t9
     #77 * t957) - 0.90D2 * t58 * 0.3141592653589793D1 * (t983 * t957 / 
     #0.6D1 + t977 * t949 - t982 * t954 / 0.2D1 - t989) - t58 * t162 * t
     #957 + 0.180D3 * t58 * t121 * (-t982 * t957 / 0.2D1 - t949 + t977 *
     # t954)) * t200 / 0.1440D4 + (-0.90D2 * t58 * 0.3141592653589793D1 
     #* (t954 - t1010 * t957) + 0.180D3 * t58 * t121 * t957) * t138 * t2
     #01 / 0.720D3 + (-0.90D2 * t58 * 0.3141592653589793D1 * (-t1025 * t
     #954 + t1027 * t957 / 0.2D1 + t949) + 0.180D3 * t58 * t121 * (t954 
     #- t1025 * t957) + t970) * t200 * t140 / 0.720D3
      t1044 = FJET(XB1, XB2, s, 0.0D0, t944, 0.0D0, -t947, 0.0D0, t1043)
      t1046 = x2 * x3
      t1049 = Sqrt(x3 * t945 * t95)
      t1050 = t108 * t1049
      t1052 = 0.2D1 * t1050 * x2
      t1054 = 0.1D1 - x3 + t1046
      t1055 = 0.1D1 / t1054
      t1057 = t2 * (0.1D1 - x3 - x2 + t1046 + t176 + t1052) * t1055
      t1062 = t2 * x2 * (-0.1D1 + t1046 + 0.2D1 * t1050) * t1055
      t1063 = x2 * z
      t1064 = t1063 - z - x2
      t1065 = t95 * t1055
      t1066 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1067 = t1064 * t1066
      t1068 = t945 * t95
      t1069 = t1054 ** 2
      t1070 = 0.1D1 / t1069
      t1071 = t1068 * t1070
      t1075 = log(0.4D1 * t892 * t94 * t1071)
      t1076 = t1075 * t1064
      t1077 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1081 = t1046 * z
      t1082 = t176 * z
      t1088 = 0.1D1 / (-t176 + x3 - t1063 - t1081 + t1082 + z + x2 - 0.2
     #D1 * t1050 * z + 0.2D1 * t1050 * t1063 - t1052)
      t1092 = t58 * 0.3141592653589793D1
      t1093 = lh * t1064
      t1094 = t1077 * t1088
      t1102 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1109 = log(0.4D1 * t176 * t8 * t1006 * t95 * t1070)
      t1110 = t1109 * t1064
      t1112 = t1109 ** 2
      t1113 = t1112 * t1064
      t1127 = t43 * t1064
      t1134 = (0.90D2 * t58 * 0.3141592653589793D1 * (-t1067 + t1076 * t
     #1077) * t1088 + 0.180D3 * t1092 * t1093 * t1094) * t138 * t201 / 0
     #.720D3 + (0.90D2 * t58 * 0.3141592653589793D1 * (-t1064 * t1102 + 
     #t1110 * t1066 - t1113 * t1077 / 0.2D1) * t1088 - 0.180D3 * t1092 *
     # lh * (-t1067 + t1110 * t1077) * t1088 + t1092 * t1127 * t1094) * 
     #t138 * t200 / 0.1440D4
      t1135 = FJET(XB1, XB2, s, 0.0D0, t1057, 0.0D0, -t1062, 0.0D0, t113
     #4)
      t1137 = t1 * t776
      t1139 = t946 * t1137 * t779
      t1140 = t943 * t1137
      t1142 = t782 * t945 * t784
      t1147 = log(-0.4D1 * t892 * t94 * t828 * t945)
      t1148 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1150 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1161 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1167 = log(-0.4D1 * t204 * t8 * t791 * t792 * t945)
      t1169 = t1167 ** 2
      t1187 = (-0.90D2 * t58 * 0.3141592653589793D1 * (t1147 * t1148 - t
     #1150) - 0.180D3 * t58 * t121 * t1148) * t138 * t201 / 0.720D3 + (-
     #0.90D2 * t58 * 0.3141592653589793D1 * (-t1161 + t1167 * t1150 - t1
     #169 * t1148 / 0.2D1) + 0.180D3 * t58 * t121 * (t1167 * t1148 - t11
     #50) - t58 * t131 * t1148) * t200 * t140 / 0.720D3
      t1188 = FJET(XB1, XB2, s, 0.0D0, t1139, t775, -t1140, t1142, t1187
     #)
      t1190 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1191 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1207 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1218 = t58 * t131 * t1191
      t1248 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1265 = (-0.90D2 * t58 * 0.3141592653589793D1 * (t1190 - t1010 * t
     #1191) + 0.180D3 * t58 * t121 * t1191) * t138 * t201 / 0.720D3 + (-
     #0.90D2 * t58 * 0.3141592653589793D1 * (t1027 * t1191 / 0.2D1 - t10
     #25 * t1190 + t1207) + 0.180D3 * t58 * t121 * (-t1025 * t1191 + t11
     #90) + t1218) * t200 * t140 / 0.720D3 + (-0.90D2 * t58 * 0.31415926
     #53589793D1 * (t1207 + t956 * t1191 / 0.2D1 - t953 * t1190) + 0.180
     #D3 * t58 * t121 * (-t953 * t1191 + t1190) + t1218) * t138 * t200 /
     # 0.1440D4 - (t58 * t131 * (t977 * t1191 - t1190) - 0.90D2 * t58 * 
     #0.3141592653589793D1 * (t977 * t1207 - t982 * t1190 / 0.2D1 + t983
     # * t1191 / 0.6D1 - t1248) - t58 * t162 * t1191 + 0.180D3 * t58 * t
     #121 * (-t1207 - t982 * t1191 / 0.2D1 + t977 * t1190)) * t200 / 0.1
     #440D4
      t1266 = FJET(XB1, XB2, s, 0.0D0, -t947, 0.0D0, t944, 0.0D0, t1265)
      t1268 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1270 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1273 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1274 = t778 * t1273
      t1286 = t786 * t1270
      t1297 = t1273 + t786 * t1273 * t826
      t1311 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1363 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-(-t786 * t1268 +
     # t798 * t778 * t1270 - t803 * t1274 / 0.2D1) * t826 - t832 * t1270
     # + t1268 + t834 * t1273 / 0.2D1) + 0.180D3 * t58 * t121 * (-(-t128
     #6 + t798 * t1274) * t826 - t832 * t1273 + t1270) + t58 * t131 * t1
     #297) * t138 * t140 / 0.1440D4 + (t58 * t131 * (t1270 - t863 * t127
     #3) - 0.90D2 * t58 * 0.3141592653589793D1 * (-t863 * t1268 - t872 *
     # t1273 / 0.6D1 + t1311 + t869 * t1270 / 0.2D1) + t58 * t162 * t127
     #3 + 0.180D3 * t58 * t121 * (t869 * t1273 / 0.2D1 + t1268 - t863 * 
     #t1270)) * t140 / 0.1440D4 + (-0.90D2 * t58 * 0.3141592653589793D1 
     #* (-t904 * t1273 - (-t1286 + t898 * t1274) * t826 + t1270) + 0.180
     #D3 * t58 * t121 * t1297) * t138 * t201 / 0.720D3 + (-0.90D2 * t58 
     #* 0.3141592653589793D1 * (t921 * t1273 / 0.2D1 + t1268 - t920 * t1
     #270) + 0.180D3 * t58 * t121 * (-t920 * t1273 + t1270) + t58 * t131
     # * t1273) * t200 * t140 / 0.720D3
      t1364 = FJET(XB1, XB2, s, 0.0D0, -t781, t775, 0.0D0, -t785, t1363)
      t1366 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1368 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1370 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1378 = t1064 * t1368
      t1385 = t1370 * t1088
      t1405 = (0.90D2 * t58 * 0.3141592653589793D1 * (-t1064 * t1366 + t
     #1110 * t1368 - t1113 * t1370 / 0.2D1) * t1088 - 0.180D3 * t1092 * 
     #lh * (-t1378 + t1110 * t1370) * t1088 + t1092 * t1127 * t1385) * t
     #138 * t200 / 0.1440D4 + (0.90D2 * t58 * 0.3141592653589793D1 * (-t
     #1378 + t1076 * t1370) * t1088 + 0.180D3 * t1092 * t1093 * t1385) *
     # t138 * t201 / 0.720D3
      t1406 = FJET(XB1, XB2, s, 0.0D0, -t1062, 0.0D0, t1057, 0.0D0, t140
     #5)
      t1408 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1412 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1416 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1418 = t58 * t131 * t1416
      t1427 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1432 = t58 * t162 * t1416
      t1449 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1464 = z * t1408
      t1475 = z * t1412
      t1485 = -t1416 - z * t1416 * t115
      t1487 = t58 * t131 * t1485
      t1617 = ((-0.90D2 * t58 * 0.3141592653589793D1 * t1408 + 0.180D3 *
     # t58 * t121 * t1412 + t1418) * t243 - 0.90D2 * t58 * 0.31415926535
     #89793D1 * t1416 * t253 + (t58 * t131 * t1412 - 0.90D2 * t58 * 0.31
     #41592653589793D1 * t1427 + t1432 + 0.180D3 * t58 * t121 * t1408) *
     # t267 + (-0.90D2 * t58 * 0.3141592653589793D1 * t1412 + 0.180D3 * 
     #t58 * t121 * t1416) * t279) * t138 / 0.2880D4 + (t1449 - t18 * t14
     #12 / 0.6D1 + t14 * t1408 / 0.2D1 - t13 * t1427 + t76 * t1416 / 0.2
     #4D2) * t26 * t30 / 0.32D2 + (-0.90D2 * t58 * 0.3141592653589793D1 
     #* (t87 * t1412 - t89 * t1416 / 0.2D1 - (t1464 - t101 * t1412 + t10
     #4 * t1416 / 0.2D1) * t115 - t1408) + 0.180D3 * t58 * t121 * (t87 *
     # t1416 - t1412 - (t1475 - t101 * t1416) * t115) + t1487) * t138 * 
     #t140 / 0.1440D4 + (t58 * t131 * (-t1412 + t146 * t1416) - 0.90D2 *
     # t58 * 0.3141592653589793D1 * (t155 * t1416 / 0.6D1 - t1427 + t146
     # * t1408 - t152 * t1412 / 0.2D1) - t1432 + 0.180D3 * t58 * t121 * 
     #(-t152 * t1416 / 0.2D1 + t146 * t1412 - t1408)) * t140 / 0.1440D4 
     #+ (-0.90D2 * t58 * 0.3141592653589793D1 * (-(t1475 - t187 * t1416)
     # * t115 - t1412 + t180 * t1416) + 0.180D3 * t58 * t121 * t1485) * 
     #t138 * t201 / 0.720D3 + (-0.90D2 * t58 * 0.3141592653589793D1 * (-
     #t1408 + t207 * t1412 - t209 * t1416 / 0.2D1) + 0.180D3 * t58 * t12
     #1 * (-t1412 + t207 * t1416) - t1418) * t200 * t140 / 0.720D3 - (-t
     #13 * t1412 + t14 * t1416 / 0.2D1 + t1408) * t26 * t44 / 0.2880D4 -
     # (t1412 - t13 * t1416) * t26 * t55 / 0.2880D4 + (-0.90D2 * t58 * 0
     #.3141592653589793D1 * (-(t1464 - t288 * t1412 + t291 * t1416 / 0.2
     #D1) * t115 - t300 * t1416 / 0.2D1 - t1408 + t298 * t1412) + 0.180D
     #3 * t58 * t121 * (-(t1475 - t288 * t1416) * t115 + t298 * t1416 - 
     #t1412) + t1487) * t138 * t200 / 0.1440D4 - (t58 * t131 * (t1412 - 
     #t322 * t1416) - 0.90D2 * t58 * 0.3141592653589793D1 * (t328 * t141
     #2 / 0.2D1 - t331 * t1416 / 0.6D1 - t322 * t1408 + t1427) + t1432 +
     # 0.180D3 * t58 * t121 * (-t322 * t1412 + t1408 + t328 * t1416 / 0.
     #2D1)) * t200 / 0.1440D4 - (t14 * t1412 / 0.2D1 - t13 * t1408 + t14
     #27 - t18 * t1416 / 0.6D1) * t26 * t31 / 0.16D2 - t58 * t66 * t1416
     # / 0.2880D4
      t1618 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1617)
      t1620 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1622 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1625 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1626 = t778 * t1625
      t1638 = t786 * t1622
      t1649 = t1625 + t786 * t1625 * t826
      t1665 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t1715 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-(-t786 * t1620 +
     # t798 * t778 * t1622 - t803 * t1626 / 0.2D1) * t826 + t1620 + t834
     # * t1625 / 0.2D1 - t832 * t1622) + 0.180D3 * t58 * t121 * (-(-t163
     #8 + t798 * t1626) * t826 - t832 * t1625 + t1622) + t58 * t131 * t1
     #649) * t138 * t140 / 0.1440D4 + (t58 * t131 * (t1622 - t863 * t162
     #5) - 0.90D2 * t58 * 0.3141592653589793D1 * (-t872 * t1625 / 0.6D1 
     #- t863 * t1620 + t869 * t1622 / 0.2D1 + t1665) + t58 * t162 * t162
     #5 + 0.180D3 * t58 * t121 * (-t863 * t1622 + t1620 + t869 * t1625 /
     # 0.2D1)) * t140 / 0.1440D4 + (-0.90D2 * t58 * 0.3141592653589793D1
     # * (-(-t1638 + t898 * t1626) * t826 - t904 * t1625 + t1622) + 0.18
     #0D3 * t58 * t121 * t1649) * t138 * t201 / 0.720D3 + (-0.90D2 * t58
     # * 0.3141592653589793D1 * (t1620 - t920 * t1622 + t921 * t1625 / 0
     #.2D1) + 0.180D3 * t58 * t121 * (t1622 - t920 * t1625) + t58 * t131
     # * t1625) * t200 * t140 / 0.720D3
      t1716 = FJET(XB1, XB2, s, t775, 0.0D0, 0.0D0, -t781, -t785, t1715)
      t1718 = t349 * t348 + t561 * t560 + t773 * t772 + t941 * t940 + t1
     #044 * t1043 + t1135 * t1134 + t1188 * t1187 + t1266 * t1265 + t136
     #4 * t1363 + t1406 * t1405 + t1618 * t1617 + t1716 * t1715
      t1719 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1720 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1735 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1751 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-t1719 + t1147 * 
     #t1720) - 0.180D3 * t58 * t121 * t1720) * t138 * t201 / 0.720D3 + (
     #-0.90D2 * t58 * 0.3141592653589793D1 * (-t1169 * t1720 / 0.2D1 + t
     #1167 * t1719 - t1735) + 0.180D3 * t58 * t121 * (t1167 * t1720 - t1
     #719) - t58 * t131 * t1720) * t200 * t140 / 0.720D3
      t1752 = FJET(XB1, XB2, s, t775, -t1140, 0.0D0, t1139, t1142, t1751
     #)
      t1754 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1756 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1771 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1782 = t58 * t131 * t1754
      t1807 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t1829 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-t1010 * t1754 + 
     #t1756) + 0.180D3 * t58 * t121 * t1754) * t138 * t201 / 0.720D3 + (
     #-0.90D2 * t58 * 0.3141592653589793D1 * (-t1025 * t1756 + t1027 * t
     #1754 / 0.2D1 + t1771) + 0.180D3 * t58 * t121 * (-t1025 * t1754 + t
     #1756) + t1782) * t200 * t140 / 0.720D3 + (-0.90D2 * t58 * 0.314159
     #2653589793D1 * (-t953 * t1756 + t1771 + t956 * t1754 / 0.2D1) + 0.
     #180D3 * t58 * t121 * (t1756 - t953 * t1754) + t1782) * t138 * t200
     # / 0.1440D4 - (t58 * t131 * (t977 * t1754 - t1756) - 0.90D2 * t58 
     #* 0.3141592653589793D1 * (-t1807 - t982 * t1756 / 0.2D1 + t977 * t
     #1771 + t983 * t1754 / 0.6D1) - t58 * t162 * t1754 + 0.180D3 * t58 
     #* t121 * (-t1771 - t982 * t1754 / 0.2D1 + t977 * t1756)) * t200 / 
     #0.1440D4
      t1830 = FJET(XB1, XB2, s, t944, 0.0D0, -t947, 0.0D0, 0.0D0, t1829)
      t1832 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1833 = t1064 * t1832
      t1834 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1841 = t1834 * t1088
      t1849 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t1871 = (0.90D2 * t58 * 0.3141592653589793D1 * (-t1833 + t1076 * t
     #1834) * t1088 + 0.180D3 * t1092 * t1093 * t1841) * t138 * t201 / 0
     #.720D3 + (0.90D2 * t58 * 0.3141592653589793D1 * (-t1064 * t1849 + 
     #t1110 * t1832 - t1113 * t1834 / 0.2D1) * t1088 - 0.180D3 * t1092 *
     # lh * (-t1833 + t1110 * t1834) * t1088 + t1092 * t1127 * t1841) * 
     #t138 * t200 / 0.1440D4
      t1872 = FJET(XB1, XB2, s, t1057, 0.0D0, -t1062, 0.0D0, 0.0D0, t187
     #1)
      t1874 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1876 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1887 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t1906 = (-0.90D2 * t58 * 0.3141592653589793D1 * (t1147 * t1874 - t
     #1876) - 0.180D3 * t58 * t121 * t1874) * t138 * t201 / 0.720D3 + (-
     #0.90D2 * t58 * 0.3141592653589793D1 * (-t1887 + t1167 * t1876 - t1
     #169 * t1874 / 0.2D1) + 0.180D3 * t58 * t121 * (t1167 * t1874 - t18
     #76) - t58 * t131 * t1874) * t200 * t140 / 0.720D3
      t1907 = FJET(XB1, XB2, s, t1139, 0.0D0, -t1140, t775, t1142, t1906
     #)
      t1910 = t775 * t1046 * t1055
      t1911 = t2 * t776
      t1912 = t176 * t777
      t1913 = t176 * x1
      t1915 = Sqrt(t819 * t1068)
      t1916 = t108 * t1915
      t1918 = 0.2D1 * t1916 * x2
      t1922 = t1911 * (t176 - x2 + t1046 + t1912 - t1913 + 0.1D1 - x3 + 
     #t1918) * t779 * t1055
      t1926 = t95 * s * t1 * x1 * t1055
      t1932 = t1911 * x2 * (-0.1D1 + t1046 + x1 - t809 - t777 + t811 + 0
     #.2D1 * t1916) * t779 * t1055
      t1933 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t1938 = log(0.4D1 * t176 * t859 * t860 * t1071)
      t1939 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t1942 = x2 * x1
      t1955 = x2 * t83
      t1959 = t1942 * z
      t1961 = t810 - t84 - t813 + t777 + t1912 - 0.2D1 * t1916 * t1942 -
     # 0.2D1 * t1916 * t1063 + 0.2D1 * t1046 * t777 - t814 * t1942 - 0.2
     #D1 * t84 * t1063 + t84 * t3 * x2 - t1913 + t1918 + t1942 * t3 + 0.
     #2D1 * t1955 * z - t1955 * t3 - 0.3D1 * t1959
      t1969 = -t1046 * x1 + t84 * x2 + 0.2D1 * t1916 * z + t1081 - t1082
     # - x2 - x3 - t818 - z + 0.2D1 * t1916 * t1959 + t1063 + t176 - t81
     #2 + t815 + t817 + 0.2D1 * t1942 - t1955
      t1971 = 0.1D1 / (t1961 + t1969)
      t1974 = (x2 - t1942 + z - t1063 + t1959) * t778
      t1978 = t58 * t121
      t1983 = -0.90D2 * t1092 * (-t1933 + t1938 * t1939) * t1971 * t1974
     # - 0.180D3 * t1978 * t1939 * t1971 * t1974
      t1987 = FJET(XB1, XB2, s, t1910, -t1922, -t1926, t1932, t1142, t19
     #83 * t138 * t201 / 0.720D3)
      t1990 = t138 * t200 * t140
      t1993 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t1995 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t2005 = -0.90D2 * t1092 * (t1938 * t1993 - t1995) * t1971 * t1974 
     #- 0.180D3 * t1978 * t1993 * t1971 * t1974
      t2009 = FJET(XB1, XB2, s, t1932, -t1926, -t1922, t1910, t1142, t20
     #05 * t138 * t201 / 0.720D3)
      t2013 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t2015 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t2027 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t2041 = t58 * t131 * t2013
      t2066 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, 0.1
     #0D1, x4)
      t2088 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-t1010 * t2013 + 
     #t2015) + 0.180D3 * t58 * t121 * t2013) * t138 * t201 / 0.720D3 + (
     #-0.90D2 * t58 * 0.3141592653589793D1 * (t2027 + t1027 * t2013 / 0.
     #2D1 - t1025 * t2015) + 0.180D3 * t58 * t121 * (t2015 - t1025 * t20
     #13) + t2041) * t200 * t140 / 0.720D3 + (-0.90D2 * t58 * 0.31415926
     #53589793D1 * (t956 * t2013 / 0.2D1 + t2027 - t953 * t2015) + 0.180
     #D3 * t58 * t121 * (t2015 - t953 * t2013) + t2041) * t138 * t200 / 
     #0.1440D4 - (t58 * t131 * (t977 * t2013 - t2015) - 0.90D2 * t58 * 0
     #.3141592653589793D1 * (-t2066 + t977 * t2027 + t983 * t2013 / 0.6D
     #1 - t982 * t2015 / 0.2D1) - t58 * t162 * t2013 + 0.180D3 * t58 * t
     #121 * (t977 * t2015 - t2027 - t982 * t2013 / 0.2D1)) * t200 / 0.14
     #40D4
      t2089 = FJET(XB1, XB2, s, -t947, 0.0D0, t944, 0.0D0, 0.0D0, t2088)
      t2091 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t2093 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t2096 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t2097 = t778 * t2096
      t2109 = t786 * t2093
      t2120 = t786 * t2096 * t826 + t2096
      t2136 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, t787, 0.10D1, 0.1
     #0D1, x4)
      t2186 = (-0.90D2 * t58 * 0.3141592653589793D1 * (-(-t786 * t2091 +
     # t798 * t778 * t2093 - t803 * t2097 / 0.2D1) * t826 + t2091 + t834
     # * t2096 / 0.2D1 - t832 * t2093) + 0.180D3 * t58 * t121 * (t2093 -
     # (-t2109 + t798 * t2097) * t826 - t832 * t2096) + t58 * t131 * t21
     #20) * t138 * t140 / 0.1440D4 + (t58 * t131 * (t2093 - t863 * t2096
     #) - 0.90D2 * t58 * 0.3141592653589793D1 * (-t872 * t2096 / 0.6D1 -
     # t863 * t2091 + t869 * t2093 / 0.2D1 + t2136) + t58 * t162 * t2096
     # + 0.180D3 * t58 * t121 * (-t863 * t2093 + t2091 + t869 * t2096 / 
     #0.2D1)) * t140 / 0.1440D4 + (-0.90D2 * t58 * 0.3141592653589793D1 
     #* (t2093 - (-t2109 + t898 * t2097) * t826 - t904 * t2096) + 0.180D
     #3 * t58 * t121 * t2120) * t138 * t201 / 0.720D3 + (-0.90D2 * t58 *
     # 0.3141592653589793D1 * (-t920 * t2093 + t2091 + t921 * t2096 / 0.
     #2D1) + 0.180D3 * t58 * t121 * (-t920 * t2096 + t2093) + t58 * t131
     # * t2096) * t200 * t140 / 0.720D3
      t2187 = FJET(XB1, XB2, s, -t781, 0.0D0, 0.0D0, t775, -t785, t2186)
      t2189 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t2191 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t2205 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t787, t948, 0.10D
     #1, x4)
      t2221 = (-0.90D2 * t58 * 0.3141592653589793D1 * (t1147 * t2189 - t
     #2191) - 0.180D3 * t58 * t121 * t2189) * t138 * t201 / 0.720D3 + (-
     #0.90D2 * t58 * 0.3141592653589793D1 * (t1167 * t2191 - t1169 * t21
     #89 / 0.2D1 - t2205) + 0.180D3 * t58 * t121 * (t1167 * t2189 - t219
     #1) - t58 * t131 * t2189) * t200 * t140 / 0.720D3
      t2222 = FJET(XB1, XB2, s, -t1140, t775, t1139, 0.0D0, t1142, t2221
     #)
      t2224 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t2225 = t1064 * t2224
      t2226 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t2233 = t2226 * t1088
      t2241 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t948, -t1
     #065, x4)
      t2263 = (0.90D2 * t58 * 0.3141592653589793D1 * (-t2225 + t1076 * t
     #2226) * t1088 + 0.180D3 * t1092 * t1093 * t2233) * t138 * t201 / 0
     #.720D3 + (0.90D2 * t58 * 0.3141592653589793D1 * (-t1064 * t2241 + 
     #t1110 * t2224 - t1113 * t2226 / 0.2D1) * t1088 - 0.180D3 * t1092 *
     # lh * (-t2225 + t1110 * t2226) * t1088 + t1092 * t1127 * t2233) * 
     #t138 * t200 / 0.1440D4
      t2264 = FJET(XB1, XB2, s, -t1062, 0.0D0, t1057, 0.0D0, 0.0D0, t226
     #3)
      t2266 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t2268 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t2278 = -0.90D2 * t1092 * (t1938 * t2266 - t2268) * t1971 * t1974 
     #- 0.180D3 * t1978 * t2266 * t1971 * t1974
      t2282 = FJET(XB1, XB2, s, -t1926, t1932, t1910, -t1922, t1142, t22
     #78 * t138 * t201 / 0.720D3)
      t2286 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t2288 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t787, t948, -t106
     #5, x4)
      t2298 = -0.90D2 * t1092 * (t1938 * t2286 - t2288) * t1971 * t1974 
     #- 0.180D3 * t1978 * t2286 * t1971 * t1974
      t2302 = FJET(XB1, XB2, s, -t1922, t1910, t1932, -t1926, t1142, t22
     #98 * t138 * t201 / 0.720D3)
      t2306 = t1752 * t1751 + t1830 * t1829 + t1872 * t1871 + t1907 * t1
     #906 + t1987 * t1983 * t1990 / 0.720D3 + t2009 * t2005 * t1990 / 0.
     #720D3 + t2089 * t2088 + t2187 * t2186 + t2222 * t2221 + t2264 * t2
     #263 + t2282 * t2278 * t1990 / 0.720D3 + t2302 * t2298 * t1990 / 0.
     #720D3
      rrgq2qght8s1e1 = t1718 + t2306

      end function



      doubleprecision function rrgq2qght8s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t23 = log(-0.4D1 * t12 * t20)
      t24 = t23 ** 2
      t26 = cos(t9)
      t28 = Sqrt(-x3 * t18)
      t33 = 0.1D1 / (-x3 - z + 0.2D1 * t26 * t28 * z)
      t37 = log(0.4D1 * t12 * t17)
      t38 = t37 ** 2
      t40 = -t24 * z * t33 / 0.2D1 - t38 / 0.2D1
      t44 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t48 = 0.3141592653589793D1 * lh
      t51 = 0.180D3 * t6 * t48 * t7
      t55 = t23 * z * t33 + t37
      t57 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t64 = lh ** 2
      t66 = 0.3141592653589793D1 ** 2
      t68 = -0.180D3 * t64 + 0.30D2 * t66
      t69 = 0.3141592653589793D1 * t68
      t71 = t6 * t69 * t7
      t74 = -z * t33 - 0.1D1
      t77 = 0.1D1 / x3
      t80 = t14 * t11
      t81 = t80 * t16
      t83 = log(0.4D1 * t81)
      t85 = t83 ** 2
      t90 = t5 * 0.3141592653589793D1
      t91 = t90 * lh
      t99 = 0.3141592653589793D1 * (-0.60D2 * lh * t66 + 0.2884936567583
     #026D3 + 0.120D3 * t64 * lh)
      t105 = t85 * t83
      t108 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t117 = t90 * t68
      t120 = z * t44
      t121 = x2 ** 2
      t122 = x3 * t121
      t123 = t122 * t11
      t126 = log(-0.4D1 * t123 * t20)
      t127 = t126 * z
      t133 = log(0.4D1 * t122 * t81)
      t141 = -t7 - z * t7 * t33
      t144 = 0.180D3 * t6 * t48 * t141
      t147 = 0.1D1 / x2
      t150 = t121 * t11
      t153 = log(0.4D1 * t150 * t17)
      t155 = t153 ** 2
      t170 = x1 ** 2
      t171 = x3 * t170
      t174 = log(0.4D1 * t171 * t81)
      t176 = t171 * t11
      t179 = log(-0.4D1 * t176 * t20)
      t180 = t179 * z
      t190 = 0.1D1 / x1
      t193 = t6 * 0.3141592653589793D1
      t195 = t147 * t190
      t199 = t121 * t170
      t202 = log(0.4D1 * t199 * t81)
      t212 = t170 * t11
      t215 = log(0.4D1 * t212 * t17)
      t217 = t215 ** 2
      t232 = (-0.90D2 * t6 * 0.3141592653589793D1 * t7 * t40 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * t44 + t51) * t55 + (-0.90D2 * t6 * 0
     #.3141592653589793D1 * t57 + 0.180D3 * t6 * t48 * t44 + t71) * t74)
     # * t77 / 0.2880D4 - (t57 - t83 * t44 + t85 * t7 / 0.2D1) * t3 * t9
     #1 / 0.16D2 - t6 * t99 * t7 / 0.2880D4 + (t85 * t44 / 0.2D1 - t105 
     #* t7 / 0.6D1 + t108 - t83 * t57) * t3 * t90 / 0.32D2 - (t44 - t83 
     #* t7) * t3 * t117 / 0.2880D4 + (-0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t44 - (t120 - t127 * t7) * t33 + t133 * t7) + t144) * t77 * 
     #t147 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t57 - t1
     #53 * t44 + t155 * t7 / 0.2D1) + 0.180D3 * t6 * t48 * (t44 - t153 *
     # t7) + t71) * t147 / 0.1440D4 + (-0.90D2 * t6 * 0.3141592653589793
     #D1 * (t174 * t7 - t44 - (t120 - t180 * t7) * t33) + t144) * t77 * 
     #t190 / 0.1440D4 - t193 * t141 * t77 * t195 / 0.8D1 + (-0.90D2 * t6
     # * 0.3141592653589793D1 * (-t44 + t202 * t7) - t51) * t147 * t190 
     #/ 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t57 + t215 * 
     #t44 - t217 * t7 / 0.2D1) + 0.180D3 * t6 * t48 * (-t44 + t215 * t7)
     # - t71) * t190 / 0.1440D4
      t233 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t232)
      t235 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t240 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t246 = 0.180D3 * t6 * t48 * t235
      t249 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t257 = t6 * t69 * t235
      t273 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t288 = z * t240
      t299 = -z * t235 * t33 - t235
      t302 = 0.180D3 * t6 * t48 * t299
      t362 = (-0.90D2 * t6 * 0.3141592653589793D1 * t235 * t40 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t240 + t246) * t55 + (-0.90D2 * t6
     # * 0.3141592653589793D1 * t249 + 0.180D3 * t6 * t48 * t240 + t257)
     # * t74) * t77 / 0.2880D4 - (-t83 * t240 + t249 + t85 * t235 / 0.2D
     #1) * t3 * t91 / 0.16D2 - t6 * t99 * t235 / 0.2880D4 + (t273 + t85 
     #* t240 / 0.2D1 - t83 * t249 - t105 * t235 / 0.6D1) * t3 * t90 / 0.
     #32D2 - (t240 - t83 * t235) * t3 * t117 / 0.2880D4 + (-0.90D2 * t6 
     #* 0.3141592653589793D1 * (-t240 - (t288 - t127 * t235) * t33 + t13
     #3 * t235) + t302) * t77 * t147 / 0.1440D4 - (-0.90D2 * t6 * 0.3141
     #592653589793D1 * (-t153 * t240 + t249 + t155 * t235 / 0.2D1) + 0.1
     #80D3 * t6 * t48 * (t240 - t153 * t235) + t257) * t147 / 0.1440D4 +
     # (-0.90D2 * t6 * 0.3141592653589793D1 * (-t240 + t174 * t235 - (t2
     #88 - t180 * t235) * t33) + t302) * t77 * t190 / 0.1440D4 - t193 * 
     #t299 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 *
     # (-t240 + t202 * t235) - t246) * t147 * t190 / 0.720D3 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (t215 * t240 - t249 - t217 * t235 / 
     #0.2D1) + 0.180D3 * t6 * t48 * (-t240 + t215 * t235) - t257) * t190
     # / 0.1440D4
      t363 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t362)
      t365 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t370 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t376 = 0.180D3 * t6 * t48 * t365
      t379 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t387 = t6 * t69 * t365
      t406 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t418 = z * t370
      t429 = -z * t365 * t33 - t365
      t432 = 0.180D3 * t6 * t48 * t429
      t492 = (-0.90D2 * t6 * 0.3141592653589793D1 * t365 * t40 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t370 + t376) * t55 + (-0.90D2 * t6
     # * 0.3141592653589793D1 * t379 + 0.180D3 * t6 * t48 * t370 + t387)
     # * t74) * t77 / 0.2880D4 - (t85 * t365 / 0.2D1 + t379 - t83 * t370
     #) * t3 * t91 / 0.16D2 - t6 * t99 * t365 / 0.2880D4 + (t85 * t370 /
     # 0.2D1 - t83 * t379 + t406 - t105 * t365 / 0.6D1) * t3 * t90 / 0.3
     #2D2 - (t370 - t83 * t365) * t3 * t117 / 0.2880D4 + (-0.90D2 * t6 *
     # 0.3141592653589793D1 * (-(t418 - t127 * t365) * t33 + t133 * t365
     # - t370) + t432) * t77 * t147 / 0.1440D4 - (-0.90D2 * t6 * 0.31415
     #92653589793D1 * (t155 * t365 / 0.2D1 + t379 - t153 * t370) + 0.180
     #D3 * t6 * t48 * (t370 - t153 * t365) + t387) * t147 / 0.1440D4 + (
     #-0.90D2 * t6 * 0.3141592653589793D1 * (-t370 + t174 * t365 - (t418
     # - t180 * t365) * t33) + t432) * t77 * t190 / 0.1440D4 - t193 * t4
     #29 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (
     #-t370 + t202 * t365) - t376) * t147 * t190 / 0.720D3 + (-0.90D2 * 
     #t6 * 0.3141592653589793D1 * (-t217 * t365 / 0.2D1 - t379 + t215 * 
     #t370) + 0.180D3 * t6 * t48 * (-t370 + t215 * t365) - t387) * t190 
     #/ 0.1440D4
      t493 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t492)
      t495 = t2 * x1
      t496 = -0.1D1 + x1
      t497 = x1 * z
      t498 = 0.1D1 - x1 + t497
      t499 = 0.1D1 / t498
      t501 = t2 * t496 * t499
      t502 = s * t15
      t504 = x1 * t496 * t499
      t505 = t502 * t504
      t506 = z * t498
      t507 = -t496
      t508 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.10
     #D1, x4)
      t511 = t16 * t499
      t512 = t496 ** 2
      t517 = log(-0.4D1 * t171 * t80 * t511 * t512 * t19)
      t518 = t517 * z
      t519 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.10
     #D1, x4)
      t523 = x3 * x1
      t524 = 0.2D1 * t523
      t525 = t523 * z
      t526 = 0.3D1 * t525
      t527 = x1 * t13
      t528 = x3 * t13
      t529 = t528 * x1
      t531 = 0.2D1 * t171 * z
      t532 = t171 * t13
      t533 = x3 * t498
      t535 = Sqrt(-t533 * t18)
      t539 = -z + t524 - t171 + t497 - t526 - t527 + t529 + t531 - t532 
     #- x3 + 0.2D1 * t26 * t535 * z
      t540 = 0.1D1 / t539
      t543 = t17 * t499 * t512
      t546 = log(0.4D1 * t176 * t543)
      t554 = t519 + t506 * t519 * t540
      t566 = t199 * t11
      t569 = log(0.4D1 * t566 * t543)
      t582 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.10
     #D1, x4)
      t587 = log(0.4D1 * t212 * t14 * t511 * t512)
      t588 = t587 ** 2
      t606 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-(-t506 * t508 + t5
     #18 * t498 * t519) * t540 - t546 * t519 + t508) + 0.180D3 * t6 * t4
     #8 * t554) * t77 * t190 / 0.1440D4 - t193 * t554 * t77 * t195 / 0.8
     #D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t508 - t569 * t519) +
     # 0.180D3 * t6 * t48 * t519) * t147 * t190 / 0.720D3 + (-0.90D2 * t
     #6 * 0.3141592653589793D1 * (t582 + t588 * t519 / 0.2D1 - t587 * t5
     #08) + 0.180D3 * t6 * t48 * (t508 - t587 * t519) + t6 * t69 * t519)
     # * t190 / 0.1440D4
      t607 = FJET(XB1, XB2, s, 0.0D0, t495, -t501, 0.0D0, -t505, t606)
      t609 = x2 * s
      t610 = t609 * t1
      t611 = -0.1D1 + x2
      t612 = t611 * s
      t613 = t612 * t1
      t614 = t17 * t611
      t617 = log(-0.4D1 * t123 * t614)
      t618 = -t611
      t619 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.10
     #D1, x4)
      t621 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.10
     #D1, x4)
      t628 = 0.180D3 * t6 * t48 * t619
      t635 = log(-0.4D1 * t150 * t614)
      t636 = t635 ** 2
      t639 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.10
     #D1, x4)
      t661 = log(-0.4D1 * t566 * t614)
      t671 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t617 * t619 + t621
     #) + t628) * t77 * t147 / 0.1440D4 - (-0.90D2 * t6 * 0.314159265358
     #9793D1 * (-t636 * t619 / 0.2D1 - t639 + t635 * t621) + 0.180D3 * t
     #6 * t48 * (-t621 + t635 * t619) - t6 * t69 * t619) * t147 / 0.1440
     #D4 - t193 * t619 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.31415926
     #53589793D1 * (t621 - t661 * t619) + t628) * t147 * t190 / 0.720D3
      t672 = FJET(XB1, XB2, s, 0.0D0, t610, 0.0D0, -t613, 0.0D0, t671)
      t674 = x2 * x3
      t677 = Sqrt(x3 * t611 * t18)
      t678 = t26 * t677
      t680 = 0.2D1 * t678 * x2
      t682 = 0.1D1 - x3 + t674
      t683 = 0.1D1 / t682
      t685 = t2 * (0.1D1 - x3 - x2 + t674 + t122 + t680) * t683
      t690 = t2 * x2 * (-0.1D1 + t674 + 0.2D1 * t678) * t683
      t691 = x2 * z
      t692 = t691 - z - x2
      t693 = t18 * t683
      t694 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t69
     #3, x4)
      t698 = t682 ** 2
      t704 = log(0.4D1 * t122 * t80 * t16 * t611 * t18 / t698)
      t705 = t704 * t692
      t706 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t69
     #3, x4)
      t710 = t674 * z
      t711 = t122 * z
      t717 = 0.1D1 / (-t122 + x3 - t691 - t710 + t711 + z + x2 - 0.2D1 *
     # t678 * z + 0.2D1 * t678 * t691 - t680)
      t721 = lh * t692
      t722 = t706 * t717
      t731 = t6 * 0.3141592653589793D1 * t692
      t733 = t77 * t147 * t190
      t737 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t692 * t694 + t705 
     #* t706) * t717 + 0.180D3 * t193 * t721 * t722) * t77 * t147 / 0.14
     #40D4 - t731 * t722 * t733 / 0.8D1
      t738 = FJET(XB1, XB2, s, 0.0D0, t685, 0.0D0, -t690, 0.0D0, t737)
      t740 = t1 * t496
      t742 = t612 * t740 * t499
      t743 = t609 * t740
      t745 = t502 * t611 * t504
      t746 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D1
     #, x4)
      t756 = log(-0.4D1 * t199 * t80 * t511 * t512 * t611)
      t758 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D1
     #, x4)
      t770 = t193 * t746 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t756 * t746 - t758) - 0.180D3 * t6 * t48 * t746) *
     # t147 * t190 / 0.720D3
      t771 = FJET(XB1, XB2, s, 0.0D0, t742, t495, -t743, t745, t770)
      t773 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.10
     #D1, x4)
      t775 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.10
     #D1, x4)
      t782 = 0.180D3 * t6 * t48 * t773
      t787 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.10
     #D1, x4)
      t818 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-t617 * t773 + t775
     #) + t782) * t77 * t147 / 0.1440D4 - (-0.90D2 * t6 * 0.314159265358
     #9793D1 * (-t787 - t636 * t773 / 0.2D1 + t635 * t775) + 0.180D3 * t
     #6 * t48 * (t635 * t773 - t775) - t6 * t69 * t773) * t147 / 0.1440D
     #4 - t193 * t773 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.314159265
     #3589793D1 * (-t661 * t773 + t775) + t782) * t147 * t190 / 0.720D3
      t819 = FJET(XB1, XB2, s, 0.0D0, -t613, 0.0D0, t610, 0.0D0, t818)
      t821 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.10
     #D1, x4)
      t823 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.10
     #D1, x4)
      t835 = t823 + t506 * t823 * t540
      t861 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.10
     #D1, x4)
      t877 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-(-t506 * t821 + t5
     #18 * t498 * t823) * t540 - t546 * t823 + t821) + 0.180D3 * t6 * t4
     #8 * t835) * t77 * t190 / 0.1440D4 - t193 * t835 * t77 * t195 / 0.8
     #D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t569 * t823 + t821) 
     #+ 0.180D3 * t6 * t48 * t823) * t147 * t190 / 0.720D3 + (-0.90D2 * 
     #t6 * 0.3141592653589793D1 * (t588 * t823 / 0.2D1 + t861 - t587 * t
     #821) + 0.180D3 * t6 * t48 * (t821 - t587 * t823) + t6 * t69 * t823
     #) * t190 / 0.1440D4
      t878 = FJET(XB1, XB2, s, 0.0D0, -t501, t495, 0.0D0, -t505, t877)
      t880 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t69
     #3, x4)
      t882 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t69
     #3, x4)
      t889 = t882 * t717
      t900 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t692 * t880 + t705 
     #* t882) * t717 + 0.180D3 * t193 * t721 * t889) * t77 * t147 / 0.14
     #40D4 - t731 * t889 * t733 / 0.8D1
      t901 = FJET(XB1, XB2, s, 0.0D0, -t690, 0.0D0, t685, 0.0D0, t900)
      t903 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t908 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t914 = 0.180D3 * t6 * t48 * t903
      t917 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t925 = t6 * t69 * t903
      t946 = z * t908
      t957 = -t903 - z * t903 * t33
      t960 = 0.180D3 * t6 * t48 * t957
      t983 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t1030 = (-0.90D2 * t6 * 0.3141592653589793D1 * t903 * t40 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * t908 + t914) * t55 + (-0.90D2 * t
     #6 * 0.3141592653589793D1 * t917 + 0.180D3 * t6 * t48 * t908 + t925
     #) * t74) * t77 / 0.2880D4 - t6 * t99 * t903 / 0.2880D4 - (t908 - t
     #83 * t903) * t3 * t117 / 0.2880D4 - (-t83 * t908 + t85 * t903 / 0.
     #2D1 + t917) * t3 * t91 / 0.16D2 + (-0.90D2 * t6 * 0.31415926535897
     #93D1 * (-(t946 - t127 * t903) * t33 + t133 * t903 - t908) + t960) 
     #* t77 * t147 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (
     #-t153 * t908 + t917 + t155 * t903 / 0.2D1) + 0.180D3 * t6 * t48 * 
     #(t908 - t153 * t903) + t925) * t147 / 0.1440D4 + (t85 * t908 / 0.2
     #D1 - t83 * t917 + t983 - t105 * t903 / 0.6D1) * t3 * t90 / 0.32D2 
     #+ (-0.90D2 * t6 * 0.3141592653589793D1 * (t174 * t903 - t908 - (t9
     #46 - t180 * t903) * t33) + t960) * t77 * t190 / 0.1440D4 - t193 * 
     #t957 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 *
     # (-t908 + t202 * t903) - t914) * t147 * t190 / 0.720D3 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * (-t217 * t903 / 0.2D1 + t215 * t908 
     #- t917) + 0.180D3 * t6 * t48 * (-t908 + t215 * t903) - t925) * t19
     #0 / 0.1440D4
      t1031 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1030)
      t1033 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.1
     #0D1, x4)
      t1035 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.1
     #0D1, x4)
      t1047 = t1035 + t506 * t1035 * t540
      t1072 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.1
     #0D1, x4)
      t1089 = (-0.90D2 * t6 * 0.3141592653589793D1 * (-(-t506 * t1033 + 
     #t518 * t498 * t1035) * t540 - t546 * t1035 + t1033) + 0.180D3 * t6
     # * t48 * t1047) * t77 * t190 / 0.1440D4 - t193 * t1047 * t77 * t19
     #5 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (t1033 - t569 *
     # t1035) + 0.180D3 * t6 * t48 * t1035) * t147 * t190 / 0.720D3 + (-
     #0.90D2 * t6 * 0.3141592653589793D1 * (-t587 * t1033 + t1072 + t588
     # * t1035 / 0.2D1) + 0.180D3 * t6 * t48 * (t1033 - t587 * t1035) + 
     #t6 * t69 * t1035) * t190 / 0.1440D4
      t1090 = FJET(XB1, XB2, s, t495, 0.0D0, 0.0D0, -t501, -t505, t1089)
      t1092 = t233 * t232 + t363 * t362 + t493 * t492 + t607 * t606 + t6
     #72 * t671 + t738 * t737 + t771 * t770 + t819 * t818 + t878 * t877 
     #+ t901 * t900 + t1031 * t1030 + t1090 * t1089
      t1093 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D
     #1, x4)
      t1099 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D
     #1, x4)
      t1111 = t193 * t1093 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.3141
     #592653589793D1 * (t756 * t1093 - t1099) - 0.180D3 * t6 * t48 * t10
     #93) * t147 * t190 / 0.720D3
      t1112 = FJET(XB1, XB2, s, t495, -t743, 0.0D0, t742, t745, t1111)
      t1114 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.1
     #0D1, x4)
      t1115 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.1
     #0D1, x4)
      t1123 = 0.180D3 * t6 * t48 * t1115
      t1128 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.1
     #0D1, x4)
      t1159 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t1114 - t617 * t11
     #15) + t1123) * t77 * t147 / 0.1440D4 - (-0.90D2 * t6 * 0.314159265
     #3589793D1 * (-t1128 - t636 * t1115 / 0.2D1 + t635 * t1114) + 0.180
     #D3 * t6 * t48 * (t635 * t1115 - t1114) - t6 * t69 * t1115) * t147 
     #/ 0.1440D4 - t193 * t1115 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0
     #.3141592653589793D1 * (-t661 * t1115 + t1114) + t1123) * t147 * t1
     #90 / 0.720D3
      t1160 = FJET(XB1, XB2, s, t610, 0.0D0, -t613, 0.0D0, 0.0D0, t1159)
      t1162 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t6
     #93, x4)
      t1164 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t6
     #93, x4)
      t1171 = t1164 * t717
      t1182 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t692 * t1162 + t70
     #5 * t1164) * t717 + 0.180D3 * t193 * t721 * t1171) * t77 * t147 / 
     #0.1440D4 - t731 * t1171 * t733 / 0.8D1
      t1183 = FJET(XB1, XB2, s, t685, 0.0D0, -t690, 0.0D0, 0.0D0, t1182)
      t1185 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D
     #1, x4)
      t1191 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D
     #1, x4)
      t1203 = t193 * t1185 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.3141
     #592653589793D1 * (t756 * t1185 - t1191) - 0.180D3 * t6 * t48 * t11
     #85) * t147 * t190 / 0.720D3
      t1204 = FJET(XB1, XB2, s, t742, 0.0D0, -t743, t495, t745, t1203)
      t1207 = t495 * t674 * t683
      t1208 = t2 * t496
      t1209 = t122 * t497
      t1210 = t122 * x1
      t1213 = Sqrt(t533 * t611 * t18)
      t1214 = t26 * t1213
      t1216 = 0.2D1 * t1214 * x2
      t1220 = t1208 * (t122 - x2 + t674 + t1209 - t1210 + 0.1D1 - x3 + t
     #1216) * t499 * t683
      t1224 = t18 * s * t1 * x1 * t683
      t1230 = t1208 * x2 * (-0.1D1 + t674 + x1 - t523 - t497 + t525 + 0.
     #2D1 * t1214) * t499 * t683
      t1231 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, -t693
     #, x4)
      t1233 = x2 * x1
      t1235 = x2 * t170
      t1247 = t1233 * z
      t1253 = t122 + t691 - x2 + 0.2D1 * t1233 - t1235 + 0.2D1 * t674 * 
     #t497 - t528 * t1233 - 0.2D1 * t171 * t691 + t171 * t13 * x2 + t497
     # + t1233 * t13 + 0.2D1 * t1235 * z - t1235 * t13 - 0.3D1 * t1247 -
     # t674 * x1 + t171 * x2 + 0.2D1 * t1214 * z
      t1260 = 0.2D1 * t1214 * t1247 - t1210 + t1216 + t1209 - 0.2D1 * t1
     #214 * t1233 - 0.2D1 * t1214 * t691 + t710 - t711 - x3 - t526 + t52
     #9 + t531 - t532 - z + t524 - t171 - t527
      t1262 = 0.1D1 / (t1253 + t1260)
      t1265 = x2 - t1233 + z - t691 + t1247
      t1267 = t1265 * t498 * t733
      t1270 = FJET(XB1, XB2, s, t1207, -t1220, -t1224, t1230, t745, t6 *
     # 0.3141592653589793D1 * t1231 * t1262 * t1267 / 0.8D1)
      t1276 = t1262 * t1265 * t498 * t733
      t1279 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, -t693
     #, x4)
      t1285 = FJET(XB1, XB2, s, t1230, -t1224, -t1220, t1207, t745, t6 *
     # 0.3141592653589793D1 * t1279 * t1262 * t1267 / 0.8D1)
      t1291 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.1
     #0D1, x4)
      t1292 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.1
     #0D1, x4)
      t1300 = 0.180D3 * t6 * t48 * t1292
      t1306 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, 0.1
     #0D1, x4)
      t1336 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t1291 - t617 * t12
     #92) + t1300) * t77 * t147 / 0.1440D4 - (-0.90D2 * t6 * 0.314159265
     #3589793D1 * (t635 * t1291 - t1306 - t636 * t1292 / 0.2D1) + 0.180D
     #3 * t6 * t48 * (t635 * t1292 - t1291) - t6 * t69 * t1292) * t147 /
     # 0.1440D4 - t193 * t1292 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.
     #3141592653589793D1 * (t1291 - t661 * t1292) + t1300) * t147 * t190
     # / 0.720D3
      t1337 = FJET(XB1, XB2, s, -t613, 0.0D0, t610, 0.0D0, 0.0D0, t1336)
      t1339 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.1
     #0D1, x4)
      t1341 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.1
     #0D1, x4)
      t1353 = t506 * t1341 * t540 + t1341
      t1378 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t507, 0.10D1, 0.1
     #0D1, x4)
      t1395 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t1339 - (-t506 * t
     #1339 + t518 * t498 * t1341) * t540 - t546 * t1341) + 0.180D3 * t6 
     #* t48 * t1353) * t77 * t190 / 0.1440D4 - t193 * t1353 * t77 * t195
     # / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t569 * t1341 +
     # t1339) + 0.180D3 * t6 * t48 * t1341) * t147 * t190 / 0.720D3 + (-
     #0.90D2 * t6 * 0.3141592653589793D1 * (-t587 * t1339 + t1378 + t588
     # * t1341 / 0.2D1) + 0.180D3 * t6 * t48 * (t1339 - t587 * t1341) + 
     #t6 * t69 * t1341) * t190 / 0.1440D4
      t1396 = FJET(XB1, XB2, s, -t501, 0.0D0, 0.0D0, t495, -t505, t1395)
      t1398 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D
     #1, x4)
      t1404 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t507, t618, 0.10D
     #1, x4)
      t1416 = t193 * t1398 * t77 * t195 / 0.8D1 + (-0.90D2 * t6 * 0.3141
     #592653589793D1 * (t756 * t1398 - t1404) - 0.180D3 * t6 * t48 * t13
     #98) * t147 * t190 / 0.720D3
      t1417 = FJET(XB1, XB2, s, -t743, t495, t742, 0.0D0, t745, t1416)
      t1419 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t6
     #93, x4)
      t1421 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t618, -t6
     #93, x4)
      t1428 = t1421 * t717
      t1439 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t692 * t1419 + t70
     #5 * t1421) * t717 + 0.180D3 * t193 * t721 * t1428) * t77 * t147 / 
     #0.1440D4 - t731 * t1428 * t733 / 0.8D1
      t1440 = FJET(XB1, XB2, s, -t690, 0.0D0, t685, 0.0D0, 0.0D0, t1439)
      t1442 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, -t693
     #, x4)
      t1448 = FJET(XB1, XB2, s, -t1224, t1230, t1207, -t1220, t745, t6 *
     # 0.3141592653589793D1 * t1442 * t1262 * t1267 / 0.8D1)
      t1454 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t507, t618, -t693
     #, x4)
      t1460 = FJET(XB1, XB2, s, -t1220, t1207, t1230, -t1224, t745, t6 *
     # 0.3141592653589793D1 * t1454 * t1262 * t1267 / 0.8D1)
      t1466 = t1112 * t1111 + t1160 * t1159 + t1183 * t1182 + t1204 * t1
     #203 + t1270 * t3 * t90 * t1231 * t1276 / 0.8D1 + t1285 * t3 * t90 
     #* t1279 * t1276 / 0.8D1 + t1337 * t1336 + t1396 * t1395 + t1417 * 
     #t1416 + t1440 * t1439 + t1448 * t3 * t90 * t1442 * t1276 / 0.8D1 +
     # t1460 * t3 * t90 * t1454 * t1276 / 0.8D1
      rrgq2qght8s1e0 = t1092 + t1466

      end function



      doubleprecision function rrgq2qght8s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t23 = log(-0.4D1 * t12 * t17 / t18)
      t25 = cos(t9)
      t27 = Sqrt(-x3 * t18)
      t32 = 0.1D1 / (-x3 - z + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t12 * t17)
      t37 = t23 * z * t32 + t36
      t41 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t45 = 0.3141592653589793D1 * lh
      t48 = 0.180D3 * t6 * t45 * t7
      t51 = -z * t32 - 0.1D1
      t54 = 0.1D1 / x3
      t60 = log(0.4D1 * t14 * t11 * t16)
      t64 = t5 * 0.3141592653589793D1
      t65 = t64 * lh
      t68 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t70 = t60 ** 2
      t77 = lh ** 2
      t79 = 0.3141592653589793D1 ** 2
      t82 = 0.3141592653589793D1 * (-0.180D3 * t77 + 0.30D2 * t79)
      t86 = t6 * 0.3141592653589793D1
      t90 = (-t7 - z * t7 * t32) * t54
      t91 = 0.1D1 / x2
      t95 = x2 ** 2
      t96 = t95 * t11
      t99 = log(0.4D1 * t96 * t17)
      t109 = 0.1D1 / x1
      t113 = x1 ** 2
      t114 = t113 * t11
      t117 = log(0.4D1 * t114 * t17)
      t129 = (-0.90D2 * t6 * 0.3141592653589793D1 * t7 * t37 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * t41 + t48) * t51) * t54 / 0.2880D4 -
     # (t41 - t60 * t7) * t3 * t65 / 0.16D2 + (t68 - t60 * t41 + t70 * t
     #7 / 0.2D1) * t3 * t64 / 0.32D2 - t6 * t82 * t7 / 0.2880D4 - t86 * 
     #t90 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t41 -
     # t99 * t7) + t48) * t91 / 0.1440D4 + t86 * t7 * t91 * t109 / 0.8D1
     # + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t41 + t117 * t7) - t48
     #) * t109 / 0.1440D4 - t86 * t90 * t109 / 0.16D2
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t129)
      t132 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t137 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t143 = 0.180D3 * t6 * t45 * t132
      t155 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t168 = (-z * t132 * t32 - t132) * t54
      t195 = (-0.90D2 * t6 * 0.3141592653589793D1 * t132 * t37 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t137 + t143) * t51) * t54 / 0.2880
     #D4 - (t137 - t60 * t132) * t3 * t65 / 0.16D2 + (-t60 * t137 + t155
     # + t70 * t132 / 0.2D1) * t3 * t64 / 0.32D2 - t6 * t82 * t132 / 0.2
     #880D4 - t86 * t168 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.314159265358
     #9793D1 * (t137 - t99 * t132) + t143) * t91 / 0.1440D4 + t86 * t132
     # * t91 * t109 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t
     #137 + t117 * t132) - t143) * t109 / 0.1440D4 - t86 * t168 * t109 /
     # 0.16D2
      t196 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t195)
      t198 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t203 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t209 = 0.180D3 * t6 * t45 * t198
      t222 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t234 = (-z * t198 * t32 - t198) * t54
      t261 = (-0.90D2 * t6 * 0.3141592653589793D1 * t198 * t37 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t203 + t209) * t51) * t54 / 0.2880
     #D4 - (t203 - t60 * t198) * t3 * t65 / 0.16D2 + (t70 * t198 / 0.2D1
     # + t222 - t60 * t203) * t3 * t64 / 0.32D2 - t6 * t82 * t198 / 0.28
     #80D4 - t86 * t234 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (t203 - t99 * t198) + t209) * t91 / 0.1440D4 + t86 * t198 
     #* t91 * t109 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-t2
     #03 + t117 * t198) - t209) * t109 / 0.1440D4 - t86 * t234 * t109 / 
     #0.16D2
      t262 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t261)
      t264 = t2 * x1
      t265 = -0.1D1 + x1
      t266 = x1 * z
      t267 = 0.1D1 - x1 + t266
      t268 = 0.1D1 / t267
      t270 = t2 * t265 * t268
      t271 = s * t15
      t273 = x1 * t265 * t268
      t274 = t271 * t273
      t275 = -t265
      t276 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t281 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t284 = t265 ** 2
      t288 = log(0.4D1 * t114 * t14 * t16 * t268 * t284)
      t300 = z * t267
      t301 = x3 * x1
      t303 = x3 * t113
      t314 = Sqrt(-x3 * t267 * t18)
      t318 = -z + 0.2D1 * t301 - t303 + t266 - 0.3D1 * t301 * z - x1 * t
     #13 + x3 * t13 * x1 + 0.2D1 * t303 * z - t303 * t13 - x3 + 0.2D1 * 
     #t25 * t314 * z
      t319 = 0.1D1 / t318
      t327 = -t86 * t276 * t91 * t109 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t281 - t288 * t276) + 0.180D3 * t6 * t45 * t276) *
     # t109 / 0.1440D4 - t86 * (t276 + t300 * t276 * t319) * t54 * t109 
     #/ 0.16D2
      t328 = FJET(XB1, XB2, s, 0.0D0, t264, -t270, 0.0D0, -t274, t327)
      t330 = x2 * s
      t331 = t330 * t1
      t332 = -0.1D1 + x2
      t333 = t332 * s
      t334 = t333 * t1
      t335 = -t332
      t336 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t341 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t345 = log(-0.4D1 * t96 * t17 * t332)
      t361 = -t86 * t336 * t54 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t341 + t345 * t336) - 0.180D3 * t6 * t45 * t336) 
     #* t91 / 0.1440D4 - t86 * t336 * t91 * t109 / 0.8D1
      t362 = FJET(XB1, XB2, s, 0.0D0, t331, 0.0D0, -t334, 0.0D0, t361)
      t364 = x2 * x3
      t365 = t95 * x3
      t368 = Sqrt(x3 * t332 * t18)
      t369 = t25 * t368
      t371 = 0.2D1 * t369 * x2
      t374 = 0.1D1 / (0.1D1 - x3 + t364)
      t376 = t2 * (0.1D1 - x3 - x2 + t364 + t365 + t371) * t374
      t381 = t2 * x2 * (-0.1D1 + t364 + 0.2D1 * t369) * t374
      t382 = x2 * z
      t383 = t382 - z - x2
      t385 = t6 * 0.3141592653589793D1 * t383
      t386 = t18 * t374
      t387 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, -t38
     #6, x4)
      t395 = 0.1D1 / (-t365 + x3 - t382 - t364 * z + t365 * z + z + x2 -
     # 0.2D1 * t369 * z + 0.2D1 * t369 * t382 - t371)
      t397 = t54 * t91
      t401 = FJET(XB1, XB2, s, 0.0D0, t376, 0.0D0, -t381, 0.0D0, -t385 *
     # t387 * t395 * t397 / 0.16D2)
      t406 = t395 * t54 * t91
      t410 = t1 * t265
      t412 = t333 * t410 * t268
      t413 = t330 * t410
      t415 = t271 * t332 * t273
      t416 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t275, t335, 0.10D1
     #, x4)
      t421 = FJET(XB1, XB2, s, 0.0D0, t412, t264, -t413, t415, t86 * t41
     #6 * t91 * t109 / 0.8D1)
      t425 = t91 * t109
      t429 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t435 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t450 = -t86 * t429 * t54 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t345 * t429 - t435) - 0.180D3 * t6 * t45 * t429) *
     # t91 / 0.1440D4 - t86 * t429 * t91 * t109 / 0.8D1
      t451 = FJET(XB1, XB2, s, 0.0D0, -t334, 0.0D0, t331, 0.0D0, t450)
      t453 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t458 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t477 = -t86 * t453 * t91 * t109 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t458 - t288 * t453) + 0.180D3 * t6 * t45 * t453) *
     # t109 / 0.1440D4 - t86 * (t453 + t300 * t453 * t319) * t54 * t109 
     #/ 0.16D2
      t478 = FJET(XB1, XB2, s, 0.0D0, -t270, t264, 0.0D0, -t274, t477)
      t480 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, -t38
     #6, x4)
      t485 = FJET(XB1, XB2, s, 0.0D0, -t381, 0.0D0, t376, 0.0D0, -t385 *
     # t480 * t395 * t397 / 0.16D2)
      t492 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t493 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t511 = 0.180D3 * t6 * t45 * t493
      t520 = (-t493 - z * t493 * t32) * t54
      t535 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t555 = -(t492 - t60 * t493) * t3 * t65 / 0.16D2 - t6 * t82 * t493 
     #/ 0.2880D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * t493 * t37 + (
     #-0.90D2 * t6 * 0.3141592653589793D1 * t492 + t511) * t51) * t54 / 
     #0.2880D4 - t86 * t520 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.314159265
     #3589793D1 * (t492 - t99 * t493) + t511) * t91 / 0.1440D4 + (-t60 *
     # t492 + t70 * t493 / 0.2D1 + t535) * t3 * t64 / 0.32D2 + t86 * t49
     #3 * t91 * t109 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793D1 * (-
     #t492 + t117 * t493) - t511) * t109 / 0.1440D4 - t86 * t520 * t109 
     #/ 0.16D2
      t556 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t555)
      t558 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t563 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t582 = -t86 * t558 * t91 * t109 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t563 - t288 * t558) + 0.180D3 * t6 * t45 * t558) *
     # t109 / 0.1440D4 - t86 * (t558 + t300 * t558 * t319) * t54 * t109 
     #/ 0.16D2
      t583 = FJET(XB1, XB2, s, t264, 0.0D0, 0.0D0, -t270, -t274, t582)
      t585 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t275, t335, 0.10D1
     #, x4)
      t590 = FJET(XB1, XB2, s, t264, -t413, 0.0D0, t412, t415, t86 * t58
     #5 * t91 * t109 / 0.8D1)
      t597 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t603 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t618 = -t86 * t597 * t54 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t345 * t597 - t603) - 0.180D3 * t6 * t45 * t597) *
     # t91 / 0.1440D4 - t86 * t597 * t91 * t109 / 0.8D1
      t619 = FJET(XB1, XB2, s, t331, 0.0D0, -t334, 0.0D0, 0.0D0, t618)
      t621 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, -t38
     #6, x4)
      t626 = FJET(XB1, XB2, s, t376, 0.0D0, -t381, 0.0D0, 0.0D0, -t385 *
     # t621 * t395 * t397 / 0.16D2)
      t633 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t275, t335, 0.10D1
     #, x4)
      t638 = FJET(XB1, XB2, s, t412, 0.0D0, -t413, t264, t415, t86 * t63
     #3 * t91 * t109 / 0.8D1)
      t645 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t651 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, 0.10
     #D1, x4)
      t666 = -t86 * t645 * t54 * t91 / 0.16D2 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t345 * t645 - t651) - 0.180D3 * t6 * t45 * t645) *
     # t91 / 0.1440D4 - t86 * t645 * t91 * t109 / 0.8D1
      t667 = FJET(XB1, XB2, s, -t334, 0.0D0, t331, 0.0D0, 0.0D0, t666)
      t669 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t674 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t275, 0.10D1, 0.10
     #D1, x4)
      t693 = -t86 * t669 * t91 * t109 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t674 - t288 * t669) + 0.180D3 * t6 * t45 * t669) *
     # t109 / 0.1440D4 - t86 * (t300 * t669 * t319 + t669) * t54 * t109 
     #/ 0.16D2
      t694 = FJET(XB1, XB2, s, -t270, 0.0D0, 0.0D0, t264, -t274, t693)
      t696 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t335, -t38
     #6, x4)
      t701 = FJET(XB1, XB2, s, -t381, 0.0D0, t376, 0.0D0, 0.0D0, -t385 *
     # t696 * t395 * t397 / 0.16D2)
      t708 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t275, t335, 0.10D1
     #, x4)
      t713 = FJET(XB1, XB2, s, -t413, t264, t412, 0.0D0, t415, t86 * t70
     #8 * t91 * t109 / 0.8D1)
      rrgq2qght8s1em1 = t130 * t129 + t196 * t195 + t262 * t261 + t328 *
     # t327 + t362 * t361 - t401 * t3 * t64 * t383 * t387 * t406 / 0.16D
     #2 + t421 * t3 * t5 * 0.3141592653589793D1 * t416 * t425 / 0.8D1 + 
     #t451 * t450 + t478 * t477 - t485 * t3 * t64 * t383 * t480 * t406 /
     # 0.16D2 + t556 * t555 + t583 * t582 + t590 * t3 * t5 * 0.314159265
     #3589793D1 * t585 * t425 / 0.8D1 + t619 * t618 - t626 * t3 * t64 * 
     #t383 * t621 * t406 / 0.16D2 + t638 * t3 * t5 * 0.3141592653589793D
     #1 * t633 * t425 / 0.8D1 + t667 * t666 + t694 * t693 - t701 * t3 * 
     #t64 * t383 * t696 * t406 / 0.16D2 + t713 * t3 * t5 * 0.31415926535
     #89793D1 * t708 * t425 / 0.8D1

      end function



      doubleprecision function rrgq2qght8s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = t6 * 0.3141592653589793D1
      t8 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t20 = -z / (-x3 - z + 0.2D1 * t10 * t13 * z) - 0.1D1
      t22 = 0.1D1 / x3
      t26 = 0.3141592653589793D1 * t8
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t35 = 0.3141592653589793D1 * lh
      t39 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t40 = z ** 2
      t42 = Sin(t9)
      t43 = t42 ** 2
      t45 = t1 ** 2
      t46 = t45 ** 2
      t49 = log(0.4D1 / t40 * t43 * t46)
      t53 = t5 * 0.3141592653589793D1
      t56 = -t7 * t8 * t20 * t22 / 0.32D2 + t6 * t26 * t27 / 0.16D2 + t6
     # * t26 * t31 / 0.16D2 - t6 * t35 * t8 / 0.16D2 + (t39 - t49 * t8) 
     #* t3 * t53 / 0.32D2
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t56)
      t59 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t64 = 0.3141592653589793D1 * t59
      t74 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t80 = -t7 * t59 * t20 * t22 / 0.32D2 + t6 * t64 * t27 / 0.16D2 + t
     #6 * t64 * t31 / 0.16D2 - t6 * t35 * t59 / 0.16D2 + (t74 - t49 * t5
     #9) * t3 * t53 / 0.32D2
      t81 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t80)
      t83 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t88 = 0.3141592653589793D1 * t83
      t98 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t104 = -t7 * t83 * t20 * t22 / 0.32D2 + t6 * t88 * t27 / 0.16D2 + 
     #t6 * t88 * t31 / 0.16D2 - t6 * t35 * t83 / 0.16D2 + (t98 - t49 * t
     #83) * t3 * t53 / 0.32D2
      t105 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t104)
      t107 = t2 * x1
      t108 = -0.1D1 + x1
      t111 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t113 = t2 * t108 * t111
      t117 = s * t45 * x1 * t108 * t111
      t118 = -t108
      t119 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t118, 0.10D1, 0.10
     #D1, x4)
      t121 = 0.3141592653589793D1 * t119 * t31
      t124 = FJET(XB1, XB2, s, 0.0D0, t107, -t113, 0.0D0, -t117, -t6 * t
     #121 / 0.16D2)
      t130 = x2 * s * t1
      t131 = -0.1D1 + x2
      t133 = t131 * s * t1
      t134 = -t131
      t135 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t134, 0.10
     #D1, x4)
      t137 = 0.3141592653589793D1 * t135 * t27
      t140 = FJET(XB1, XB2, s, 0.0D0, t130, 0.0D0, -t133, 0.0D0, -t6 * t
     #137 / 0.16D2)
      t145 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t134, 0.10
     #D1, x4)
      t147 = 0.3141592653589793D1 * t145 * t27
      t150 = FJET(XB1, XB2, s, 0.0D0, -t133, 0.0D0, t130, 0.0D0, -t6 * t
     #147 / 0.16D2)
      t155 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t118, 0.10D1, 0.10
     #D1, x4)
      t157 = 0.3141592653589793D1 * t155 * t31
      t160 = FJET(XB1, XB2, s, 0.0D0, -t113, t107, 0.0D0, -t117, -t6 * t
     #157 / 0.16D2)
      t165 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t170 = 0.3141592653589793D1 * t165
      t180 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t186 = -t7 * t165 * t20 * t22 / 0.32D2 + t6 * t170 * t27 / 0.16D2 
     #+ t6 * t170 * t31 / 0.16D2 - t6 * t35 * t165 / 0.16D2 + (t180 - t4
     #9 * t165) * t3 * t53 / 0.32D2
      t187 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t186)
      t189 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t118, 0.10D1, 0.10
     #D1, x4)
      t191 = 0.3141592653589793D1 * t189 * t31
      t194 = FJET(XB1, XB2, s, t107, 0.0D0, 0.0D0, -t113, -t117, -t6 * t
     #191 / 0.16D2)
      t199 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t134, 0.10
     #D1, x4)
      t201 = 0.3141592653589793D1 * t199 * t27
      t204 = FJET(XB1, XB2, s, t130, 0.0D0, -t133, 0.0D0, 0.0D0, -t6 * t
     #201 / 0.16D2)
      t209 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t134, 0.10
     #D1, x4)
      t211 = 0.3141592653589793D1 * t209 * t27
      t214 = FJET(XB1, XB2, s, -t133, 0.0D0, t130, 0.0D0, 0.0D0, -t6 * t
     #211 / 0.16D2)
      t219 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t118, 0.10D1, 0.10
     #D1, x4)
      t221 = 0.3141592653589793D1 * t219 * t31
      t224 = FJET(XB1, XB2, s, -t113, 0.0D0, 0.0D0, t107, -t117, -t6 * t
     #221 / 0.16D2)
      rrgq2qght8s1em2 = t57 * t56 + t81 * t80 + t105 * t104 - t124 * t3 
     #* t5 * t121 / 0.16D2 - t140 * t3 * t5 * t137 / 0.16D2 - t150 * t3 
     #* t5 * t147 / 0.16D2 - t160 * t3 * t5 * t157 / 0.16D2 + t187 * t18
     #6 - t194 * t3 * t5 * t191 / 0.16D2 - t204 * t3 * t5 * t201 / 0.16D
     #2 - t214 * t3 * t5 * t211 / 0.16D2 - t224 * t3 * t5 * t221 / 0.16D
     #2

      end function



      doubleprecision function rrgq2qght8s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.32D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.32D2)
      t24 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t24 / 0.32D2)
      t32 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t32 / 0.32D2)
      rrgq2qght8s1em3 = t11 * t3 * t13 * t7 / 0.32D2 + t20 * t3 * t13 * 
     #t16 / 0.32D2 + t28 * t3 * t13 * t24 / 0.32D2 + t36 * t3 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrgq2qght8s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      rrgq2qght8s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght8s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + x2
      t2 = t1 * s
      t3 = -0.1D1 + z
      t4 = -0.1D1 + x1
      t5 = t3 * t4
      t6 = t2 * t5
      t7 = s * t3
      t8 = t7 * x1
      t10 = x1 * z
      t11 = 0.1D1 - x1 + t10
      t12 = 0.1D1 / t11
      t14 = t7 * t4 * x2 * t12
      t15 = t3 ** 2
      t20 = s * t15 * x2 * x1 * t4 * t12
      t21 = 0.1D1 / t3
      t22 = s ** 2
      t23 = 0.1D1 / t22
      t24 = t21 * t23
      t25 = -t4
      t26 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, x4
     #)
      t27 = x2 * x3
      t28 = x1 ** 2
      t29 = t15 ** 2
      t31 = t27 * t28 * t29
      t32 = x4 * 0.3141592653589793D1
      t33 = Sin(t32)
      t34 = t33 ** 2
      t35 = z ** 2
      t36 = 0.1D1 / t35
      t37 = t34 * t36
      t38 = t4 ** 2
      t39 = t12 * t38
      t40 = t1 ** 2
      t45 = log(0.4D1 * t31 * t37 * t39 * t40)
      t46 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, x4
     #)
      t52 = 0.3141592653589793D1 * lh
      t57 = 0.1D1 / x3
      t59 = 0.1D1 / x2
      t60 = 0.1D1 / x1
      t61 = t59 * t60
      t63 = x2 * t28
      t64 = t29 * t34
      t66 = t36 * t12
      t71 = log(0.4D1 * t63 * t64 * t66 * t38 * t40)
      t72 = t71 ** 2
      t75 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, x4
     #)
      t86 = lh ** 2
      t88 = 0.3141592653589793D1 ** 2
      t90 = -0.180D3 * t86 + 0.30D2 * t88
      t91 = 0.3141592653589793D1 * t90
      t98 = (-0.90D2 * t24 * 0.3141592653589793D1 * (-t26 + t45 * t46) -
     # 0.180D3 * t24 * t52 * t46) * t57 * t61 / 0.720D3 + (-0.90D2 * t24
     # * 0.3141592653589793D1 * (-t72 * t46 / 0.2D1 - t75 + t71 * t26) +
     # 0.180D3 * t24 * t52 * (t71 * t46 - t26) - t24 * t91 * t46) * t59 
     #* t60 / 0.720D3
      t99 = FJET(XB1, XB2, s, t6, t8, -t14, 0.0D0, -t20, t98)
      t101 = x3 * x1
      t102 = t7 * t101
      t104 = x3 * s * t5
      t105 = -0.1D1 + x3
      t106 = t105 * s
      t107 = t3 * x1
      t108 = t106 * t107
      t109 = t106 * t5
      t110 = x3 * t28
      t112 = t38 * t105
      t116 = log(-0.4D1 * t110 * t64 * t66 * t112)
      t117 = -t105
      t118 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117, 
     #x4)
      t120 = t116 ** 2
      t121 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117, 
     #x4)
      t124 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117, 
     #x4)
      t140 = t36 * t29
      t142 = x3 * t105
      t147 = log(-0.4D1 * t12 * t34 * t140 * t63 * t142 * t38)
      t160 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (-t116 * t118 + t1
     #20 * t121 / 0.2D1 + t124) + 0.180D3 * t24 * t52 * (-t116 * t121 + 
     #t118) + t24 * t91 * t121) * t57 * t60 / 0.720D3 + (-0.90D2 * t24 *
     # 0.3141592653589793D1 * (-t118 + t147 * t121) - 0.180D3 * t24 * t5
     #2 * t121) * t57 * t61 / 0.720D3
      t161 = FJET(XB1, XB2, s, t102, -t104, -t108, t109, 0.0D0, t160)
      t163 = t7 * t4
      t164 = x2 ** 2
      t165 = t164 * x3
      t166 = 0.3D1 * t27
      t167 = t27 * t10
      t169 = t165 * t10
      t170 = t101 * z
      t171 = t27 * x1
      t173 = t165 * x1
      t174 = cos(t32)
      t178 = Sqrt(-x3 * t11 * x2 * t105)
      t179 = t174 * t178
      t180 = 0.2D1 * t179
      t182 = 0.2D1 * t179 * x2
      t183 = t101 - t165 + t166 + 0.2D1 * t167 - t169 - t170 - 0.2D1 * t
     #171 + t173 - x2 - x3 - t180 + t182
      t185 = -0.1D1 + t27
      t186 = 0.1D1 / t185
      t188 = t163 * t183 * t12 * t186
      t191 = t8 * x3 * t1 * t186
      t196 = t163 * t1 * (-t27 - 0.1D1 + x3 + x1 - t101 - t10 + t170 + t
     #180) * t12 * t186
      t198 = t106 * t107 * t186
      t199 = x2 * x1
      t205 = t165 * z
      t206 = t27 * z
      t207 = x2 * z
      t208 = 0.2D1 * t27
      t209 = t199 * z
      t213 = 0.1D1 + 0.2D1 * t199 - 0.2D1 * t28 * z + t28 * t35 - t63 + 
     #0.2D1 * t10 - t180 + t205 - t206 - t165 + t207 + t208 + 0.2D1 * t1
     #79 * t209 - 0.2D1 * x1 - x2 + t28
      t235 = t110 * x2
      t236 = -t169 + 0.4D1 * t167 - x3 * t35 * t199 - 0.2D1 * t110 * t20
     #7 + t110 * t35 * x2 - 0.2D1 * t179 * t10 - 0.2D1 * t179 * t199 - 0
     #.2D1 * t179 * t207 - 0.3D1 * t171 + t173 + t182 + t199 * t35 + 0.2
     #D1 * t63 * z - t63 * t35 + 0.2D1 * t179 * x1 - 0.3D1 * t209 + t235
      t238 = 0.1D1 / (t213 + t236)
      t239 = -0.1D1 - t207 - t199 + t209 - t10 + x2 + x1
      t240 = t238 * t239
      t241 = t105 * t186
      t242 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4)
      t244 = t185 ** 2
      t245 = 0.1D1 / t244
      t247 = t36 * t40
      t254 = log(-0.4D1 * t245 * t34 * t247 * t29 * t235 * t112 * t12)
      t255 = t254 * t238
      t256 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4)
      t264 = t24 * t52
      t269 = -0.90D2 * t24 * 0.3141592653589793D1 * (-t240 * t242 + t255
     # * t239 * t256) * t11 - 0.180D3 * t264 * t240 * t256 * t11
      t273 = FJET(XB1, XB2, s, -t188, t191, t196, t198, -t20, t269 * t57
     # * t61 / 0.720D3)
      t276 = t57 * t59 * t60
      t280 = Sqrt(-t27 * t105)
      t281 = t174 * t280
      t282 = 0.2D1 * t281
      t284 = 0.2D1 * t281 * x2
      t287 = t7 * (-x2 - x3 + t166 - t165 - t282 + t284) * t186
      t291 = t7 * t1 * (-t27 - 0.1D1 + x3 + t282) * t186
      t292 = 0.1D1 + t207 - x2
      t293 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241, 
     #x4)
      t294 = t292 * t293
      t300 = log(-0.4D1 * t31 * t37 * t40 * t105 * t245)
      t301 = t300 * t292
      t302 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241, 
     #x4)
      t309 = 0.1D1 / (t165 - t205 + x2 - t208 - t207 + t206 + 0.2D1 * t2
     #81 * t207 - t284 + t282 - 0.1D1)
      t313 = t24 * 0.3141592653589793D1
      t314 = lh * t292
      t315 = t302 * t309
      t323 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241, 
     #x4)
      t330 = log(-0.4D1 * t27 * t64 * t247 * t105 * t245)
      t331 = t330 * t292
      t333 = t330 ** 2
      t334 = t333 * t292
      t348 = t90 * t292
      t355 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t294 - t301 * t302
     #) * t309 + 0.180D3 * t313 * t314 * t315) * t57 * t61 / 0.720D3 + (
     #-0.90D2 * t24 * 0.3141592653589793D1 * (t292 * t323 - t331 * t293 
     #+ t334 * t302 / 0.2D1) * t309 + 0.180D3 * t313 * lh * (t294 - t331
     # * t302) * t309 + t313 * t348 * t315) * t57 * t59 / 0.1440D4
      t356 = FJET(XB1, XB2, s, 0.0D0, t287, 0.0D0, -t291, 0.0D0, t355)
      t358 = t7 * x3
      t359 = t7 * t105
      t360 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t362 = t37 * t105
      t365 = log(-0.4D1 * x3 * t29 * t362)
      t366 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t371 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t373 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t374 = t365 ** 2
      t375 = t374 * t365
      t388 = -0.60D2 * lh * t88 + 0.2884936567583026D3 + 0.120D3 * t86 *
     # lh
      t389 = 0.3141592653589793D1 * t388
      t402 = t27 * t29
      t405 = log(-0.4D1 * t402 * t362)
      t407 = t405 ** 2
      t420 = t24 * t91 * t366
      t428 = log(-0.4D1 * t110 * t29 * t362)
      t429 = t428 ** 2
      t450 = log(-0.4D1 * t63 * t34 * t140 * t142)
      t463 = -(t24 * t91 * (-t360 + t365 * t366) - 0.90D2 * t24 * 0.3141
     #592653589793D1 * (t365 * t371 - t373 + t375 * t366 / 0.6D1 - t374 
     #* t360 / 0.2D1) - t24 * t389 * t366 + 0.180D3 * t24 * t52 * (-t374
     # * t366 / 0.2D1 - t371 + t365 * t360)) * t57 / 0.1440D4 + (-0.90D2
     # * t24 * 0.3141592653589793D1 * (-t405 * t360 + t371 + t407 * t366
     # / 0.2D1) + 0.180D3 * t24 * t52 * (t360 - t405 * t366) + t420) * t
     #57 * t59 / 0.1440D4 - (-0.90D2 * t24 * 0.3141592653589793D1 * (-t3
     #71 - t429 * t366 / 0.2D1 + t428 * t360) + 0.180D3 * t24 * t52 * (t
     #428 * t366 - t360) - t420) * t57 * t60 / 0.720D3 + (-0.90D2 * t24 
     #* 0.3141592653589793D1 * (t360 - t450 * t366) + 0.180D3 * t24 * t5
     #2 * t366) * t57 * t61 / 0.720D3
      t464 = FJET(XB1, XB2, s, t358, 0.0D0, -t359, 0.0D0, 0.0D0, t463)
      t466 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t467 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t474 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t475 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #7, x4)
      t508 = t24 * t91 * t467
      t541 = -(t24 * t91 * (-t466 + t365 * t467) - 0.90D2 * t24 * 0.3141
     #592653589793D1 * (t375 * t467 / 0.6D1 - t474 + t365 * t475 - t374 
     #* t466 / 0.2D1) - t24 * t389 * t467 + 0.180D3 * t24 * t52 * (t365 
     #* t466 - t475 - t374 * t467 / 0.2D1)) * t57 / 0.1440D4 + (-0.90D2 
     #* t24 * 0.3141592653589793D1 * (-t405 * t466 + t407 * t467 / 0.2D1
     # + t475) + 0.180D3 * t24 * t52 * (t466 - t405 * t467) + t508) * t5
     #7 * t59 / 0.1440D4 - (-0.90D2 * t24 * 0.3141592653589793D1 * (-t42
     #9 * t467 / 0.2D1 + t428 * t466 - t475) + 0.180D3 * t24 * t52 * (-t
     #466 + t428 * t467) - t508) * t57 * t60 / 0.720D3 + (-0.90D2 * t24 
     #* 0.3141592653589793D1 * (-t450 * t467 + t466) + 0.180D3 * t24 * t
     #52 * t467) * t57 * t61 / 0.720D3
      t542 = FJET(XB1, XB2, s, -t359, 0.0D0, t358, 0.0D0, 0.0D0, t541)
      t544 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241, 
     #x4)
      t546 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241, 
     #x4)
      t548 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241, 
     #x4)
      t556 = t292 * t546
      t563 = t548 * t309
      t583 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t292 * t544 - t331
     # * t546 + t334 * t548 / 0.2D1) * t309 + 0.180D3 * t313 * lh * (t55
     #6 - t331 * t548) * t309 + t313 * t348 * t563) * t57 * t59 / 0.1440
     #D4 + (-0.90D2 * t24 * 0.3141592653589793D1 * (t556 - t301 * t548) 
     #* t309 + 0.180D3 * t313 * t314 * t563) * t57 * t61 / 0.720D3
      t584 = FJET(XB1, XB2, s, -t291, 0.0D0, t287, 0.0D0, 0.0D0, t583)
      t586 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, x
     #4)
      t587 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, x
     #4)
      t601 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, x
     #4)
      t618 = (-0.90D2 * t24 * 0.3141592653589793D1 * (-t586 + t45 * t587
     #) - 0.180D3 * t24 * t52 * t587) * t57 * t61 / 0.720D3 + (-0.90D2 *
     # t24 * 0.3141592653589793D1 * (-t72 * t587 / 0.2D1 - t601 + t71 * 
     #t586) + 0.180D3 * t24 * t52 * (t71 * t587 - t586) - t24 * t91 * t5
     #87) * t59 * t60 / 0.720D3
      t619 = FJET(XB1, XB2, s, -t14, 0.0D0, t6, t8, -t20, t618)
      t621 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4)
      t623 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4)
      t635 = -0.90D2 * t24 * 0.3141592653589793D1 * (-t240 * t621 + t255
     # * t239 * t623) * t11 - 0.180D3 * t264 * t240 * t623 * t11
      t639 = FJET(XB1, XB2, s, t196, t198, -t188, t191, -t20, t635 * t57
     # * t61 / 0.720D3)
      t644 = x2 * s * t3
      t645 = t2 * t3
      t649 = log(0.4D1 * t235 * t64 * t247)
      t650 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t652 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t664 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t665 = t63 * t29
      t666 = t37 * t40
      t669 = log(0.4D1 * t665 * t666)
      t671 = t669 ** 2
      t684 = t24 * t91 * t650
      t691 = log(0.4D1 * t402 * t666)
      t693 = t691 ** 2
      t709 = x2 * t29
      t712 = log(0.4D1 * t709 * t666)
      t718 = t712 ** 2
      t719 = t718 * t712
      t724 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t741 = (-0.90D2 * t24 * 0.3141592653589793D1 * (-t649 * t650 + t65
     #2) + 0.180D3 * t24 * t52 * t650) * t57 * t61 / 0.720D3 + (-0.90D2 
     #* t24 * 0.3141592653589793D1 * (t664 - t669 * t652 + t671 * t650 /
     # 0.2D1) + 0.180D3 * t24 * t52 * (-t669 * t650 + t652) + t684) * t5
     #9 * t60 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1 * (-t691
     # * t652 + t693 * t650 / 0.2D1 + t664) + 0.180D3 * t24 * t52 * (-t6
     #91 * t650 + t652) + t684) * t57 * t59 / 0.1440D4 + (t24 * t91 * (-
     #t712 * t650 + t652) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t712
     # * t664 - t719 * t650 / 0.6D1 + t718 * t652 / 0.2D1 + t724) + t24 
     #* t389 * t650 + 0.180D3 * t24 * t52 * (t664 + t718 * t650 / 0.2D1 
     #- t712 * t652)) * t59 / 0.1440D4
      t742 = FJET(XB1, XB2, s, 0.0D0, t644, 0.0D0, -t645, 0.0D0, t741)
      t744 = t37 * t29
      t747 = log(0.4D1 * t110 * t744)
      t748 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t750 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t751 = t747 ** 2
      t752 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t765 = t24 * t91 * t752
      t770 = t28 * t34
      t773 = log(0.4D1 * t770 * t140)
      t779 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t780 = t773 ** 2
      t783 = t780 * t773
      t791 = t24 * t389 * t752
      t804 = log(0.4D1 * t235 * t744)
      t819 = log(0.4D1 * t63 * t744)
      t821 = t819 ** 2
      t838 = log(0.4D1 * t744)
      t840 = t838 ** 2
      t845 = t23 * 0.3141592653589793D1
      t846 = t845 * t90
      t852 = t845 * t388
      t857 = log(0.4D1 * t27 * t744)
      t859 = t857 ** 2
      t877 = log(0.4D1 * t709 * t37)
      t882 = t877 ** 2
      t883 = t882 * t877
      t905 = t840 * t838
      t911 = t845 * lh
      t916 = t88 ** 2
      t917 = t86 ** 2
      t922 = t845 * (-0.5769873135166051D3 * lh - t916 - 0.60D2 * t917 +
     # 0.60D2 * t86 * t88)
      t928 = log(0.4D1 * x3 * t34 * t140)
      t933 = t928 ** 2
      t934 = t933 * t928
      t954 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t960 = t840 ** 2
      t967 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (-t747 * t748 + t7
     #50 + t751 * t752 / 0.2D1) + 0.180D3 * t24 * t52 * (-t747 * t752 + 
     #t748) + t765) * t57 * t60 / 0.720D3 - (t24 * t91 * (t748 - t773 * 
     #t752) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t773 * t750 + t779
     # + t780 * t748 / 0.2D1 - t783 * t752 / 0.6D1) + t791 + 0.180D3 * t
     #24 * t52 * (t750 + t780 * t752 / 0.2D1 - t773 * t748)) * t60 / 0.7
     #20D3 + (-0.90D2 * t24 * 0.3141592653589793D1 * (-t748 + t804 * t75
     #2) - 0.180D3 * t24 * t52 * t752) * t57 * t61 / 0.720D3 + (-0.90D2 
     #* t24 * 0.3141592653589793D1 * (-t750 + t819 * t748 - t821 * t752 
     #/ 0.2D1) + 0.180D3 * t24 * t52 * (t819 * t752 - t748) - t765) * t5
     #9 * t60 / 0.720D3 + (-t750 + t838 * t748 - t840 * t752 / 0.2D1) * 
     #t21 * t846 / 0.1440D4 + (-t748 + t838 * t752) * t21 * t852 / 0.144
     #0D4 + (-0.90D2 * t24 * 0.3141592653589793D1 * (-t750 + t857 * t748
     # - t859 * t752 / 0.2D1) + 0.180D3 * t24 * t52 * (-t748 + t857 * t7
     #52) - t765) * t57 * t59 / 0.1440D4 + (t24 * t91 * (-t748 + t877 * 
     #t752) - 0.90D2 * t24 * 0.3141592653589793D1 * (t883 * t752 / 0.6D1
     # - t779 + t877 * t750 - t882 * t748 / 0.2D1) - t791 + 0.180D3 * t2
     #4 * t52 * (-t750 + t877 * t748 - t882 * t752 / 0.2D1)) * t59 / 0.1
     #440D4 + (-t840 * t748 / 0.2D1 - t779 + t905 * t752 / 0.6D1 + t838 
     #* t750) * t21 * t911 / 0.8D1 - t752 * t21 * t922 / 0.1440D4 - (t24
     # * t91 * (t748 - t928 * t752) - 0.90D2 * t24 * 0.3141592653589793D
     #1 * (-t934 * t752 / 0.6D1 + t779 - t928 * t750 + t933 * t748 / 0.2
     #D1) + t791 + 0.180D3 * t24 * t52 * (t750 - t928 * t748 + t933 * t7
     #52 / 0.2D1)) * t57 / 0.1440D4 - (-t954 + t905 * t748 / 0.6D1 - t84
     #0 * t750 / 0.2D1 + t838 * t779 - t960 * t752 / 0.24D2) * t21 * t84
     #5 / 0.16D2
      t968 = FJET(XB1, XB2, s, 0.0D0, t7, 0.0D0, 0.0D0, 0.0D0, t967)
      t974 = log(0.4D1 * t110 * t34 * t140 * t39)
      t975 = t974 ** 2
      t976 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D1
     #, x4)
      t979 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D1
     #, x4)
      t981 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D1
     #, x4)
      t992 = t24 * t91 * t976
      t1001 = log(0.4D1 * t770 * t36 * t29 * t12 * t38)
      t1006 = t1001 ** 2
      t1010 = t1006 * t1001
      t1013 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1029 = t37 * t39
      t1032 = log(0.4D1 * t31 * t1029)
      t1046 = log(0.4D1 * t665 * t1029)
      t1048 = t1046 ** 2
      t1064 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (-t975 * t976 / 0
     #.2D1 + t974 * t979 - t981) + 0.180D3 * t24 * t52 * (t974 * t976 - 
     #t979) - t992) * t57 * t60 / 0.720D3 - (t24 * t91 * (-t979 + t1001 
     #* t976) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t1006 * t979 / 0
     #.2D1 + t1001 * t981 + t1010 * t976 / 0.6D1 - t1013) - t24 * t389 *
     # t976 + 0.180D3 * t24 * t52 * (t1001 * t979 - t1006 * t976 / 0.2D1
     # - t981)) * t60 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1 
     #* (t979 - t1032 * t976) + 0.180D3 * t24 * t52 * t976) * t57 * t61 
     #/ 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1 * (-t1046 * t979
     # + t981 + t1048 * t976 / 0.2D1) + 0.180D3 * t24 * t52 * (t979 - t1
     #046 * t976) + t992) * t59 * t60 / 0.720D3
      t1065 = FJET(XB1, XB2, s, -t163, t8, 0.0D0, 0.0D0, 0.0D0, t1064)
      t1067 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1068 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1070 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1083 = t24 * t91 * t1070
      t1095 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1142 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t1067 - t691 * t1
     #068 + t693 * t1070 / 0.2D1) + 0.180D3 * t24 * t52 * (t1068 - t691 
     #* t1070) + t1083) * t57 * t59 / 0.1440D4 + (t24 * t91 * (t1068 - t
     #712 * t1070) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t719 * t107
     #0 / 0.6D1 - t712 * t1067 + t1095 + t718 * t1068 / 0.2D1) + t24 * t
     #389 * t1070 + 0.180D3 * t24 * t52 * (t1067 - t712 * t1068 + t718 *
     # t1070 / 0.2D1)) * t59 / 0.1440D4 + (-0.90D2 * t24 * 0.31415926535
     #89793D1 * (-t649 * t1070 + t1068) + 0.180D3 * t24 * t52 * t1070) *
     # t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1 * (-t
     #669 * t1068 + t1067 + t671 * t1070 / 0.2D1) + 0.180D3 * t24 * t52 
     #* (-t669 * t1070 + t1068) + t1083) * t59 * t60 / 0.720D3
      t1143 = FJET(XB1, XB2, s, t644, 0.0D0, -t645, 0.0D0, 0.0D0, t1142)
      t1145 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1146 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1153 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1154 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1163 = t24 * t389 * t1146
      t1174 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1199 = t24 * t91 * t1146
      t1318 = -(t24 * t91 * (t1145 - t928 * t1146) - 0.90D2 * t24 * 0.31
     #41592653589793D1 * (t933 * t1145 / 0.2D1 + t1153 - t928 * t1154 - 
     #t934 * t1146 / 0.6D1) + t1163 + 0.180D3 * t24 * t52 * (t1154 + t93
     #3 * t1146 / 0.2D1 - t928 * t1145)) * t57 / 0.1440D4 - (-t1174 + t9
     #05 * t1145 / 0.6D1 - t840 * t1154 / 0.2D1 + t838 * t1153 - t960 * 
     #t1146 / 0.24D2) * t21 * t845 / 0.16D2 - (-0.90D2 * t24 * 0.3141592
     #653589793D1 * (t751 * t1146 / 0.2D1 + t1154 - t747 * t1145) + 0.18
     #0D3 * t24 * t52 * (t1145 - t747 * t1146) + t1199) * t57 * t60 / 0.
     #720D3 - (t24 * t91 * (t1145 - t773 * t1146) - 0.90D2 * t24 * 0.314
     #1592653589793D1 * (t1153 - t783 * t1146 / 0.6D1 - t773 * t1154 + t
     #780 * t1145 / 0.2D1) + t1163 + 0.180D3 * t24 * t52 * (-t773 * t114
     #5 + t780 * t1146 / 0.2D1 + t1154)) * t60 / 0.720D3 + (-0.90D2 * t2
     #4 * 0.3141592653589793D1 * (t804 * t1146 - t1145) - 0.180D3 * t24 
     #* t52 * t1146) * t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.31415926
     #53589793D1 * (-t821 * t1146 / 0.2D1 - t1154 + t819 * t1145) + 0.18
     #0D3 * t24 * t52 * (-t1145 + t819 * t1146) - t1199) * t59 * t60 / 0
     #.720D3 + (-t1154 + t838 * t1145 - t840 * t1146 / 0.2D1) * t21 * t8
     #46 / 0.1440D4 + (-0.90D2 * t24 * 0.3141592653589793D1 * (-t859 * t
     #1146 / 0.2D1 - t1154 + t857 * t1145) + 0.180D3 * t24 * t52 * (-t11
     #45 + t857 * t1146) - t1199) * t57 * t59 / 0.1440D4 + (t24 * t91 * 
     #(-t1145 + t877 * t1146) - 0.90D2 * t24 * 0.3141592653589793D1 * (-
     #t1153 + t883 * t1146 / 0.6D1 + t877 * t1154 - t882 * t1145 / 0.2D1
     #) - t1163 + 0.180D3 * t24 * t52 * (t877 * t1145 - t1154 - t882 * t
     #1146 / 0.2D1)) * t59 / 0.1440D4 - t1146 * t21 * t922 / 0.1440D4 + 
     #(-t840 * t1145 / 0.2D1 - t1153 + t905 * t1146 / 0.6D1 + t838 * t11
     #54) * t21 * t911 / 0.8D1 + (-t1145 + t838 * t1146) * t21 * t852 / 
     #0.1440D4
      t1319 = FJET(XB1, XB2, s, t7, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1318)
      t1321 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t1324 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t1325 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t1353 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (t120 * t1321 / 0
     #.2D1 + t1324 - t116 * t1325) + 0.180D3 * t24 * t52 * (-t116 * t132
     #1 + t1325) + t24 * t91 * t1321) * t57 * t60 / 0.720D3 + (-0.90D2 *
     # t24 * 0.3141592653589793D1 * (t147 * t1321 - t1325) - 0.180D3 * t
     #24 * t52 * t1321) * t57 * t61 / 0.720D3
      t1354 = FJET(XB1, XB2, s, -t104, t102, t109, -t108, 0.0D0, t1353)
      t1356 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241,
     # x4)
      t1357 = t292 * t1356
      t1358 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241,
     # x4)
      t1365 = t1358 * t309
      t1373 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241,
     # x4)
      t1395 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t1357 - t301 * t1
     #358) * t309 + 0.180D3 * t313 * t314 * t1365) * t57 * t61 / 0.720D3
     # + (-0.90D2 * t24 * 0.3141592653589793D1 * (t292 * t1373 - t331 * 
     #t1356 + t334 * t1358 / 0.2D1) * t309 + 0.180D3 * t313 * lh * (t135
     #7 - t331 * t1358) * t309 + t313 * t348 * t1365) * t57 * t59 / 0.14
     #40D4
      t1396 = FJET(XB1, XB2, s, 0.0D0, -t291, 0.0D0, t287, 0.0D0, t1395)
      t1398 = t99 * t98 + t161 * t160 + t273 * t269 * t276 / 0.720D3 + t
     #356 * t355 + t464 * t463 + t542 * t541 + t584 * t583 + t619 * t618
     # + t639 * t635 * t276 / 0.720D3 + t742 * t741 + t968 * t967 + t106
     #5 * t1064 + t1143 * t1142 + t1319 * t1318 + t1354 * t1353 + t1396 
     #* t1395
      t1399 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241,
     # x4)
      t1401 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241,
     # x4)
      t1403 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t241,
     # x4)
      t1411 = t292 * t1401
      t1418 = t1403 * t309
      t1438 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t292 * t1399 - t3
     #31 * t1401 + t334 * t1403 / 0.2D1) * t309 + 0.180D3 * t313 * lh * 
     #(t1411 - t331 * t1403) * t309 + t313 * t348 * t1418) * t57 * t59 /
     # 0.1440D4 + (-0.90D2 * t24 * 0.3141592653589793D1 * (t1411 - t301 
     #* t1403) * t309 + 0.180D3 * t313 * t314 * t1418) * t57 * t61 / 0.7
     #20D3
      t1439 = FJET(XB1, XB2, s, t287, 0.0D0, -t291, 0.0D0, 0.0D0, t1438)
      t1441 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1442 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1455 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1469 = t24 * t91 * t1442
      t1499 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1516 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t1441 - t649 * t1
     #442) + 0.180D3 * t24 * t52 * t1442) * t57 * t61 / 0.720D3 + (-0.90
     #D2 * t24 * 0.3141592653589793D1 * (t1455 + t671 * t1442 / 0.2D1 - 
     #t669 * t1441) + 0.180D3 * t24 * t52 * (-t669 * t1442 + t1441) + t1
     #469) * t59 * t60 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1
     # * (t693 * t1442 / 0.2D1 - t691 * t1441 + t1455) + 0.180D3 * t24 *
     # t52 * (-t691 * t1442 + t1441) + t1469) * t57 * t59 / 0.1440D4 + (
     #t24 * t91 * (-t712 * t1442 + t1441) - 0.90D2 * t24 * 0.31415926535
     #89793D1 * (-t712 * t1455 - t719 * t1442 / 0.6D1 + t718 * t1441 / 0
     #.2D1 + t1499) + t24 * t389 * t1442 + 0.180D3 * t24 * t52 * (t718 *
     # t1442 / 0.2D1 - t712 * t1441 + t1455)) * t59 / 0.1440D4
      t1517 = FJET(XB1, XB2, s, 0.0D0, -t645, 0.0D0, t644, 0.0D0, t1516)
      t1519 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, 
     #x4)
      t1520 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, 
     #x4)
      t1533 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, 
     #x4)
      t1551 = (-0.90D2 * t24 * 0.3141592653589793D1 * (-t1519 + t45 * t1
     #520) - 0.180D3 * t24 * t52 * t1520) * t57 * t61 / 0.720D3 + (-0.90
     #D2 * t24 * 0.3141592653589793D1 * (t71 * t1519 - t1533 - t72 * t15
     #20 / 0.2D1) + 0.180D3 * t24 * t52 * (-t1519 + t71 * t1520) - t24 *
     # t91 * t1520) * t59 * t60 / 0.720D3
      t1552 = FJET(XB1, XB2, s, t8, t6, 0.0D0, -t14, -t20, t1551)
      t1554 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4
     #)
      t1556 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4
     #)
      t1568 = -0.90D2 * t24 * 0.3141592653589793D1 * (-t240 * t1554 + t2
     #55 * t239 * t1556) * t11 - 0.180D3 * t264 * t240 * t1556 * t11
      t1572 = FJET(XB1, XB2, s, t198, t196, t191, -t188, -t20, t1568 * t
     #57 * t61 / 0.720D3)
      t1576 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t1578 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t1579 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t1592 = t24 * t91 * t1579
      t1634 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t1651 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (t428 * t1576 - t
     #1578 - t429 * t1579 / 0.2D1) + 0.180D3 * t24 * t52 * (-t1576 + t42
     #8 * t1579) - t1592) * t57 * t60 / 0.720D3 + (-0.90D2 * t24 * 0.314
     #1592653589793D1 * (t1576 - t450 * t1579) + 0.180D3 * t24 * t52 * t
     #1579) * t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D
     #1 * (t1578 - t405 * t1576 + t407 * t1579 / 0.2D1) + 0.180D3 * t24 
     #* t52 * (-t405 * t1579 + t1576) + t1592) * t57 * t59 / 0.1440D4 - 
     #(t24 * t91 * (t365 * t1579 - t1576) - 0.90D2 * t24 * 0.31415926535
     #89793D1 * (t365 * t1578 + t375 * t1579 / 0.6D1 - t374 * t1576 / 0.
     #2D1 - t1634) - t24 * t389 * t1579 + 0.180D3 * t24 * t52 * (-t374 *
     # t1579 / 0.2D1 - t1578 + t365 * t1576)) * t57 / 0.1440D4
      t1652 = FJET(XB1, XB2, s, 0.0D0, t358, 0.0D0, -t359, 0.0D0, t1651)
      t1654 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1657 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1659 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1670 = t24 * t91 * t1654
      t1678 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1726 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (-t975 * t1654 / 
     #0.2D1 + t974 * t1657 - t1659) + 0.180D3 * t24 * t52 * (-t1657 + t9
     #74 * t1654) - t1670) * t57 * t60 / 0.720D3 - (t24 * t91 * (t1001 *
     # t1654 - t1657) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t1678 + 
     #t1010 * t1654 / 0.6D1 - t1006 * t1657 / 0.2D1 + t1001 * t1659) - t
     #24 * t389 * t1654 + 0.180D3 * t24 * t52 * (-t1659 + t1001 * t1657 
     #- t1006 * t1654 / 0.2D1)) * t60 / 0.720D3 + (-0.90D2 * t24 * 0.314
     #1592653589793D1 * (-t1032 * t1654 + t1657) + 0.180D3 * t24 * t52 *
     # t1654) * t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.314159265358979
     #3D1 * (-t1046 * t1657 + t1048 * t1654 / 0.2D1 + t1659) + 0.180D3 *
     # t24 * t52 * (t1657 - t1046 * t1654) + t1670) * t59 * t60 / 0.720D
     #3
      t1727 = FJET(XB1, XB2, s, t8, -t163, 0.0D0, 0.0D0, 0.0D0, t1726)
      t1729 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, 
     #x4)
      t1731 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, 
     #x4)
      t1745 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t25, x2, 0.10D1, 
     #x4)
      t1761 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t45 * t1729 - t17
     #31) - 0.180D3 * t24 * t52 * t1729) * t57 * t61 / 0.720D3 + (-0.90D
     #2 * t24 * 0.3141592653589793D1 * (-t72 * t1729 / 0.2D1 + t71 * t17
     #31 - t1745) + 0.180D3 * t24 * t52 * (-t1731 + t71 * t1729) - t24 *
     # t91 * t1729) * t59 * t60 / 0.720D3
      t1762 = FJET(XB1, XB2, s, 0.0D0, -t14, t8, t6, -t20, t1761)
      t1764 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1766 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1769 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1780 = t24 * t91 * t1766
      t1788 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t1836 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (t974 * t1764 - t
     #975 * t1766 / 0.2D1 - t1769) + 0.180D3 * t24 * t52 * (t974 * t1766
     # - t1764) - t1780) * t57 * t60 / 0.720D3 - (t24 * t91 * (t1001 * t
     #1766 - t1764) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t1788 + t1
     #010 * t1766 / 0.6D1 + t1001 * t1769 - t1006 * t1764 / 0.2D1) - t24
     # * t389 * t1766 + 0.180D3 * t24 * t52 * (-t1769 + t1001 * t1764 - 
     #t1006 * t1766 / 0.2D1)) * t60 / 0.720D3 + (-0.90D2 * t24 * 0.31415
     #92653589793D1 * (t1764 - t1032 * t1766) + 0.180D3 * t24 * t52 * t1
     #766) * t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1
     # * (-t1046 * t1764 + t1048 * t1766 / 0.2D1 + t1769) + 0.180D3 * t2
     #4 * t52 * (t1764 - t1046 * t1766) + t1780) * t59 * t60 / 0.720D3
      t1837 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t8, -t163, 0.0D0, t1836)
      t1839 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1840 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1842 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1855 = t24 * t91 * t1842
      t1867 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1914 = (-0.90D2 * t24 * 0.3141592653589793D1 * (t1839 - t691 * t1
     #840 + t693 * t1842 / 0.2D1) + 0.180D3 * t24 * t52 * (t1840 - t691 
     #* t1842) + t1855) * t57 * t59 / 0.1440D4 + (t24 * t91 * (t1840 - t
     #712 * t1842) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t719 * t184
     #2 / 0.6D1 - t712 * t1839 + t1867 + t718 * t1840 / 0.2D1) + t24 * t
     #389 * t1842 + 0.180D3 * t24 * t52 * (t1839 - t712 * t1840 + t718 *
     # t1842 / 0.2D1)) * t59 / 0.1440D4 + (-0.90D2 * t24 * 0.31415926535
     #89793D1 * (t1840 - t649 * t1842) + 0.180D3 * t24 * t52 * t1842) * 
     #t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1 * (t18
     #39 + t671 * t1842 / 0.2D1 - t669 * t1840) + 0.180D3 * t24 * t52 * 
     #(-t669 * t1842 + t1840) + t1855) * t59 * t60 / 0.720D3
      t1915 = FJET(XB1, XB2, s, -t645, 0.0D0, t644, 0.0D0, 0.0D0, t1914)
      t1917 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1920 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1921 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1924 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1945 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1970 = t24 * t91 * t1921
      t1989 = t24 * t389 * t1921
      t2090 = (-t840 * t1917 / 0.2D1 - t1920 + t905 * t1921 / 0.6D1 + t8
     #38 * t1924) * t21 * t911 / 0.8D1 + (-t1924 + t838 * t1917 - t840 *
     # t1921 / 0.2D1) * t21 * t846 / 0.1440D4 + (-t1917 + t838 * t1921) 
     #* t21 * t852 / 0.1440D4 - t1921 * t21 * t922 / 0.1440D4 - (-t1945 
     #+ t905 * t1917 / 0.6D1 - t840 * t1924 / 0.2D1 + t838 * t1920 - t96
     #0 * t1921 / 0.24D2) * t21 * t845 / 0.16D2 - (-0.90D2 * t24 * 0.314
     #1592653589793D1 * (t1924 - t747 * t1917 + t751 * t1921 / 0.2D1) + 
     #0.180D3 * t24 * t52 * (t1917 - t747 * t1921) + t1970) * t57 * t60 
     #/ 0.720D3 - (t24 * t91 * (t1917 - t773 * t1921) - 0.90D2 * t24 * 0
     #.3141592653589793D1 * (t780 * t1917 / 0.2D1 + t1920 - t783 * t1921
     # / 0.6D1 - t773 * t1924) + t1989 + 0.180D3 * t24 * t52 * (t1924 + 
     #t780 * t1921 / 0.2D1 - t773 * t1917)) * t60 / 0.720D3 + (-0.90D2 *
     # t24 * 0.3141592653589793D1 * (-t1917 + t804 * t1921) - 0.180D3 * 
     #t24 * t52 * t1921) * t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141
     #592653589793D1 * (-t1924 - t821 * t1921 / 0.2D1 + t819 * t1917) + 
     #0.180D3 * t24 * t52 * (-t1917 + t819 * t1921) - t1970) * t59 * t60
     # / 0.720D3 - (t24 * t91 * (t1917 - t928 * t1921) - 0.90D2 * t24 * 
     #0.3141592653589793D1 * (-t934 * t1921 / 0.6D1 - t928 * t1924 + t93
     #3 * t1917 / 0.2D1 + t1920) + t1989 + 0.180D3 * t24 * t52 * (t1924 
     #- t928 * t1917 + t933 * t1921 / 0.2D1)) * t57 / 0.1440D4 + (-0.90D
     #2 * t24 * 0.3141592653589793D1 * (-t1924 + t857 * t1917 - t859 * t
     #1921 / 0.2D1) + 0.180D3 * t24 * t52 * (-t1917 + t857 * t1921) - t1
     #970) * t57 * t59 / 0.1440D4 + (t24 * t91 * (-t1917 + t877 * t1921)
     # - 0.90D2 * t24 * 0.3141592653589793D1 * (t883 * t1921 / 0.6D1 + t
     #877 * t1924 - t882 * t1917 / 0.2D1 - t1920) - t1989 + 0.180D3 * t2
     #4 * t52 * (-t1924 + t877 * t1917 - t882 * t1921 / 0.2D1)) * t59 / 
     #0.1440D4
      t2091 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t7, 0.0D0, t2090)
      t2093 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t2094 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t2099 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t2105 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t1
     #17, x4)
      t2135 = t24 * t91 * t2094
      t2168 = -(t24 * t91 * (-t2093 + t365 * t2094) - 0.90D2 * t24 * 0.3
     #141592653589793D1 * (t365 * t2099 + t375 * t2094 / 0.6D1 - t374 * 
     #t2093 / 0.2D1 - t2105) - t24 * t389 * t2094 + 0.180D3 * t24 * t52 
     #* (-t2099 - t374 * t2094 / 0.2D1 + t365 * t2093)) * t57 / 0.1440D4
     # - (-0.90D2 * t24 * 0.3141592653589793D1 * (t428 * t2093 - t429 * 
     #t2094 / 0.2D1 - t2099) + 0.180D3 * t24 * t52 * (t428 * t2094 - t20
     #93) - t2135) * t57 * t60 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653
     #589793D1 * (t2093 - t450 * t2094) + 0.180D3 * t24 * t52 * t2094) *
     # t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793D1 * (t2
     #099 - t405 * t2093 + t407 * t2094 / 0.2D1) + 0.180D3 * t24 * t52 *
     # (-t405 * t2094 + t2093) + t2135) * t57 * t59 / 0.1440D4
      t2169 = FJET(XB1, XB2, s, 0.0D0, -t359, 0.0D0, t358, 0.0D0, t2168)
      t2171 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t2172 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t2175 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t2203 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (t2171 + t120 * t
     #2172 / 0.2D1 - t116 * t2175) + 0.180D3 * t24 * t52 * (-t116 * t217
     #2 + t2175) + t24 * t91 * t2172) * t57 * t60 / 0.720D3 + (-0.90D2 *
     # t24 * 0.3141592653589793D1 * (-t2175 + t147 * t2172) - 0.180D3 * 
     #t24 * t52 * t2172) * t57 * t61 / 0.720D3
      t2204 = FJET(XB1, XB2, s, t109, -t108, -t104, t102, 0.0D0, t2203)
      t2206 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t2207 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t2209 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t2222 = t24 * t91 * t2209
      t2233 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t2242 = t24 * t389 * t2209
      t2277 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t2379 = (-0.90D2 * t24 * 0.3141592653589793D1 * (-t2206 + t857 * t
     #2207 - t859 * t2209 / 0.2D1) + 0.180D3 * t24 * t52 * (t857 * t2209
     # - t2207) - t2222) * t57 * t59 / 0.1440D4 + (t24 * t91 * (-t2207 +
     # t877 * t2209) - 0.90D2 * t24 * 0.3141592653589793D1 * (t883 * t22
     #09 / 0.6D1 - t2233 - t882 * t2207 / 0.2D1 + t877 * t2206) - t2242 
     #+ 0.180D3 * t24 * t52 * (-t2206 + t877 * t2207 - t882 * t2209 / 0.
     #2D1)) * t59 / 0.1440D4 + (t905 * t2209 / 0.6D1 - t840 * t2207 / 0.
     #2D1 - t2233 + t838 * t2206) * t21 * t911 / 0.8D1 + (-t2206 + t838 
     #* t2207 - t840 * t2209 / 0.2D1) * t21 * t846 / 0.1440D4 + (-t2207 
     #+ t838 * t2209) * t21 * t852 / 0.1440D4 - t2209 * t21 * t922 / 0.1
     #440D4 - (-t2277 + t905 * t2207 / 0.6D1 - t840 * t2206 / 0.2D1 + t8
     #38 * t2233 - t960 * t2209 / 0.24D2) * t21 * t845 / 0.16D2 - (t24 *
     # t91 * (t2207 - t928 * t2209) - 0.90D2 * t24 * 0.3141592653589793D
     #1 * (-t934 * t2209 / 0.6D1 + t2233 + t933 * t2207 / 0.2D1 - t928 *
     # t2206) + t2242 + 0.180D3 * t24 * t52 * (t2206 - t928 * t2207 + t9
     #33 * t2209 / 0.2D1)) * t57 / 0.1440D4 - (-0.90D2 * t24 * 0.3141592
     #653589793D1 * (t2206 + t751 * t2209 / 0.2D1 - t747 * t2207) + 0.18
     #0D3 * t24 * t52 * (-t747 * t2209 + t2207) + t2222) * t57 * t60 / 0
     #.720D3 - (t24 * t91 * (t2207 - t773 * t2209) - 0.90D2 * t24 * 0.31
     #41592653589793D1 * (-t783 * t2209 / 0.6D1 + t780 * t2207 / 0.2D1 +
     # t2233 - t773 * t2206) + t2242 + 0.180D3 * t24 * t52 * (t2206 - t7
     #73 * t2207 + t780 * t2209 / 0.2D1)) * t60 / 0.720D3 + (-0.90D2 * t
     #24 * 0.3141592653589793D1 * (t804 * t2209 - t2207) - 0.180D3 * t24
     # * t52 * t2209) * t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141592
     #653589793D1 * (-t2206 + t819 * t2207 - t821 * t2209 / 0.2D1) + 0.1
     #80D3 * t24 * t52 * (-t2207 + t819 * t2209) - t2222) * t59 * t60 / 
     #0.720D3
      t2380 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t7, 0.0D0, 0.0D0, t2379)
      t2382 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t2384 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t2385 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, t117,
     # x4)
      t2414 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (-t116 * t2382 + 
     #t2384 + t120 * t2385 / 0.2D1) + 0.180D3 * t24 * t52 * (-t116 * t23
     #85 + t2382) + t24 * t91 * t2385) * t57 * t60 / 0.720D3 + (-0.90D2 
     #* t24 * 0.3141592653589793D1 * (t147 * t2385 - t2382) - 0.180D3 * 
     #t24 * t52 * t2385) * t57 * t61 / 0.720D3
      t2415 = FJET(XB1, XB2, s, -t108, t109, t102, -t104, 0.0D0, t2414)
      t2417 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t2419 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t2420 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t2433 = t24 * t91 * t2420
      t2446 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, t25, 0.0D0, 0.10D
     #1, x4)
      t2489 = -(-0.90D2 * t24 * 0.3141592653589793D1 * (t974 * t2417 - t
     #2419 - t975 * t2420 / 0.2D1) + 0.180D3 * t24 * t52 * (-t2417 + t97
     #4 * t2420) - t2433) * t57 * t60 / 0.720D3 - (t24 * t91 * (-t2417 +
     # t1001 * t2420) - 0.90D2 * t24 * 0.3141592653589793D1 * (-t1006 * 
     #t2417 / 0.2D1 + t1001 * t2419 + t1010 * t2420 / 0.6D1 - t2446) - t
     #24 * t389 * t2420 + 0.180D3 * t24 * t52 * (-t1006 * t2420 / 0.2D1 
     #+ t1001 * t2417 - t2419)) * t60 / 0.720D3 + (-0.90D2 * t24 * 0.314
     #1592653589793D1 * (t2417 - t1032 * t2420) + 0.180D3 * t24 * t52 * 
     #t2420) * t57 * t61 / 0.720D3 + (-0.90D2 * t24 * 0.3141592653589793
     #D1 * (-t1046 * t2417 + t2419 + t1048 * t2420 / 0.2D1) + 0.180D3 * 
     #t24 * t52 * (-t1046 * t2420 + t2417) + t2433) * t59 * t60 / 0.720D
     #3
      t2490 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t163, t8, 0.0D0, t2489)
      t2492 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4
     #)
      t2494 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t25, x2, t241, x4
     #)
      t2506 = -0.90D2 * t24 * 0.3141592653589793D1 * (-t240 * t2492 + t2
     #55 * t239 * t2494) * t11 - 0.180D3 * t264 * t240 * t2494 * t11
      t2510 = FJET(XB1, XB2, s, t191, -t188, t198, t196, -t20, t2506 * t
     #57 * t61 / 0.720D3)
      t2514 = t1439 * t1438 + t1517 * t1516 + t1552 * t1551 + t1572 * t1
     #568 * t276 / 0.720D3 + t1652 * t1651 + t1727 * t1726 + t1762 * t17
     #61 + t1837 * t1836 + t1915 * t1914 + t2091 * t2090 + t2169 * t2168
     # + t2204 * t2203 + t2380 * t2379 + t2415 * t2414 + t2490 * t2489 +
     # t2510 * t2506 * t276 / 0.720D3
      rrgq2qght8s2e1 = t1398 + t2514

      end function



      doubleprecision function rrgq2qght8s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.1D1 / t1
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = t6 * t8
      t10 = -t4
      t11 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D1,
     # x4)
      t12 = x1 ** 2
      t13 = x3 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t19 * t21
      t23 = x1 * z
      t24 = 0.1D1 - x1 + t23
      t25 = 0.1D1 / t24
      t26 = t4 ** 2
      t27 = t25 * t26
      t31 = log(0.4D1 * t13 * t16 * t22 * t27)
      t32 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D1,
     # x4)
      t38 = 0.3141592653589793D1 * lh
      t41 = 0.180D3 * t9 * t38 * t32
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t9 * 0.3141592653589793D1
      t50 = 0.1D1 / x2
      t51 = t50 * t45
      t55 = x2 * t12
      t56 = t55 * t21
      t57 = t16 * t19
      t61 = log(0.4D1 * t56 * t57 * t27)
      t71 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D1,
     # x4)
      t72 = t12 * t16
      t78 = log(0.4D1 * t72 * t19 * t21 * t25 * t26)
      t80 = t78 ** 2
      t92 = lh ** 2
      t94 = 0.3141592653589793D1 ** 2
      t96 = -0.180D3 * t92 + 0.30D2 * t94
      t97 = 0.3141592653589793D1 * t96
      t103 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t11 + t31 * t32) 
     #- t41) * t43 * t45 / 0.720D3 - t48 * t32 * t43 * t51 / 0.8D1 + (-0
     #.90D2 * t9 * 0.3141592653589793D1 * (t11 - t61 * t32) + t41) * t50
     # * t45 / 0.720D3 - (-0.90D2 * t9 * 0.3141592653589793D1 * (-t71 + 
     #t78 * t11 - t80 * t32 / 0.2D1) + 0.180D3 * t9 * t38 * (t78 * t32 -
     # t11) - t9 * t97 * t32) * t45 / 0.720D3
      t104 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t103)
      t106 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t110 = log(0.4D1 * x3 * t16 * t22)
      t111 = t110 ** 2
      t112 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t115 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t127 = t9 * t97 * t112
      t131 = t57 * t21
      t133 = log(0.4D1 * t131)
      t137 = t8 * 0.3141592653589793D1
      t138 = t137 * t96
      t146 = 0.3141592653589793D1 * (-0.60D2 * lh * t94 + 0.288493656758
     #3026D3 + 0.120D3 * t92 * lh)
      t150 = t133 ** 2
      t153 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t154 = t150 * t133
      t162 = x2 * x3
      t165 = log(0.4D1 * t162 * t131)
      t173 = 0.180D3 * t9 * t38 * t112
      t178 = x2 * t21
      t181 = log(0.4D1 * t178 * t57)
      t183 = t181 ** 2
      t203 = t137 * lh
      t208 = log(0.4D1 * t13 * t131)
      t224 = log(0.4D1 * t55 * t131)
      t236 = log(0.4D1 * t72 * t22)
      t238 = t236 ** 2
      t253 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (t106 + t111 * t112
     # / 0.2D1 - t110 * t115) + 0.180D3 * t9 * t38 * (t115 - t110 * t112
     #) + t127) * t43 / 0.1440D4 + (-t115 + t133 * t112) * t6 * t138 / 0
     #.1440D4 - t9 * t146 * t112 / 0.1440D4 - (-t150 * t115 / 0.2D1 - t1
     #53 + t154 * t112 / 0.6D1 + t133 * t106) * t6 * t137 / 0.16D2 + (-0
     #.90D2 * t9 * 0.3141592653589793D1 * (-t115 + t165 * t112) - t173) 
     #* t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589793D1 * (t
     #181 * t115 - t106 - t183 * t112 / 0.2D1) + 0.180D3 * t9 * t38 * (-
     #t115 + t181 * t112) - t127) * t50 / 0.1440D4 + (-t106 + t133 * t11
     #5 - t150 * t112 / 0.2D1) * t6 * t203 / 0.8D1 - (-0.90D2 * t9 * 0.3
     #141592653589793D1 * (t115 - t208 * t112) + t173) * t43 * t45 / 0.7
     #20D3 + t48 * t112 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.31415926
     #53589793D1 * (-t115 + t224 * t112) - t173) * t50 * t45 / 0.720D3 -
     # (-0.90D2 * t9 * 0.3141592653589793D1 * (-t236 * t115 + t238 * t11
     #2 / 0.2D1 + t106) + 0.180D3 * t9 * t38 * (t115 - t236 * t112) + t1
     #27) * t45 / 0.720D3
      t254 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t253)
      t256 = -0.1D1 + x2
      t257 = x3 * x1
      t258 = t257 * z
      t259 = cos(t14)
      t261 = -0.1D1 + x3
      t264 = Sqrt(-x3 * t24 * x2 * t261)
      t265 = t259 * t264
      t266 = 0.2D1 * t265
      t269 = -0.1D1 + t162
      t270 = 0.1D1 / t269
      t273 = t5 * t256 * (-t162 - 0.1D1 + x3 + x1 - t257 - t23 + t258 + 
     #t266) * t25 * t270
      t274 = t261 * s
      t275 = t1 * x1
      t277 = t274 * t275 * t270
      t278 = x2 ** 2
      t279 = t278 * x3
      t280 = 0.3D1 * t162
      t281 = t162 * t23
      t283 = t279 * t23
      t284 = t162 * x1
      t286 = t279 * x1
      t288 = 0.2D1 * t265 * x2
      t289 = t257 - t279 + t280 + 0.2D1 * t281 - t283 - t258 - 0.2D1 * t
     #284 + t286 - x2 - x3 - t266 + t288
      t292 = t5 * t289 * t25 * t270
      t295 = t3 * x3 * t256 * t270
      t300 = s * t20 * x2 * x1 * t4 * t25
      t302 = x2 * x1
      t304 = x2 * z
      t315 = t279 * z
      t316 = t162 * z
      t322 = 0.1D1 - x3 * t18 * t302 - 0.2D1 * t13 * t304 + t13 * t18 * 
     #x2 - 0.2D1 * t265 * t304 - 0.2D1 * t265 * t302 - 0.2D1 * t265 * t2
     #3 + t315 - t316 + 0.4D1 * t281 - t283 - 0.3D1 * t284 + t286 + t288
     # + t302 * t18 + 0.2D1 * t55 * z
      t326 = 0.2D1 * t162
      t328 = t302 * z
      t338 = -t55 * t18 + 0.2D1 * t265 * x1 + t326 - t279 + t304 + t13 *
     # x2 - 0.3D1 * t328 + t12 + 0.2D1 * t302 - 0.2D1 * t12 * z + t12 * 
     #t18 - t55 - t266 + 0.2D1 * t23 - 0.2D1 * x1 - x2 + 0.2D1 * t265 * 
     #t328
      t340 = 0.1D1 / (t322 + t338)
      t342 = -0.1D1 - t304 - t302 + t328 - t23 + x2 + x1
      t344 = t9 * 0.3141592653589793D1 * t340 * t342
      t345 = t261 * t270
      t346 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, t345, x4)
      t349 = t43 * t50 * t45
      t353 = FJET(XB1, XB2, s, t273, t277, -t292, t295, -t300, t344 * t3
     #46 * t24 * t349 / 0.8D1)
      t355 = t137 * t340
      t362 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, t345, x4)
      t367 = FJET(XB1, XB2, s, -t292, t295, t273, t277, -t300, t344 * t3
     #62 * t24 * t349 / 0.8D1)
      t375 = t256 * s
      t376 = t375 * t1
      t378 = x2 * s * t1
      t379 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t380 = t162 * t21
      t381 = t256 ** 2
      t382 = t57 * t381
      t385 = log(0.4D1 * t380 * t382)
      t386 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t394 = 0.180D3 * t9 * t38 * t386
      t399 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t402 = log(0.4D1 * t178 * t382)
      t404 = t402 ** 2
      t427 = log(0.4D1 * t56 * t382)
      t437 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t379 - t385 * t386)
     # + t394) * t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.31415926535897
     #93D1 * (t399 - t402 * t379 + t404 * t386 / 0.2D1) + 0.180D3 * t9 *
     # t38 * (t379 - t402 * t386) + t9 * t97 * t386) * t50 / 0.1440D4 - 
     #t48 * t386 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.314159265358979
     #3D1 * (-t427 * t386 + t379) + t394) * t50 * t45 / 0.720D3
      t438 = FJET(XB1, XB2, s, -t376, 0.0D0, t378, 0.0D0, 0.0D0, t437)
      t440 = t1 * t4
      t441 = t375 * t440
      t444 = t2 * t4 * x2 * t25
      t445 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, x
     #4)
      t450 = t21 * t16
      t452 = t19 * t25
      t457 = log(0.4D1 * t55 * t450 * t452 * t26 * t381)
      t459 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, x
     #4)
      t471 = t48 * t445 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.31415926
     #53589793D1 * (t457 * t445 - t459) - 0.180D3 * t9 * t38 * t445) * t
     #50 * t45 / 0.720D3
      t472 = FJET(XB1, XB2, s, t441, t3, -t444, 0.0D0, -t300, t471)
      t474 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t476 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t483 = 0.180D3 * t9 * t38 * t474
      t491 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t519 = (-0.90D2 * t9 * 0.3141592653589793D1 * (-t385 * t474 + t476
     #) + t483) * t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589
     #793D1 * (t404 * t474 / 0.2D1 - t402 * t476 + t491) + 0.180D3 * t9 
     #* t38 * (-t402 * t474 + t476) + t9 * t97 * t474) * t50 / 0.1440D4 
     #- t48 * t474 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.3141592653589
     #793D1 * (-t427 * t474 + t476) + t483) * t50 * t45 / 0.720D3
      t520 = FJET(XB1, XB2, s, 0.0D0, -t376, 0.0D0, t378, 0.0D0, t519)
      t522 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t523 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t525 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t538 = t9 * t97 * t525
      t554 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t574 = 0.180D3 * t9 * t38 * t525
      t606 = 0.3141592653589793D1 * t43 * t51
      t633 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (t522 - t110 * t523
     # + t111 * t525 / 0.2D1) + 0.180D3 * t9 * t38 * (t523 - t110 * t525
     #) + t538) * t43 / 0.1440D4 + (-t522 + t133 * t523 - t150 * t525 / 
     #0.2D1) * t6 * t203 / 0.8D1 - t9 * t146 * t525 / 0.1440D4 - (-t150 
     #* t523 / 0.2D1 - t554 + t154 * t525 / 0.6D1 + t133 * t522) * t6 * 
     #t137 / 0.16D2 + (-t523 + t133 * t525) * t6 * t138 / 0.1440D4 + (-0
     #.90D2 * t9 * 0.3141592653589793D1 * (-t523 + t165 * t525) - t574) 
     #* t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589793D1 * (-
     #t522 + t181 * t523 - t183 * t525 / 0.2D1) + 0.180D3 * t9 * t38 * (
     #-t523 + t181 * t525) - t538) * t50 / 0.1440D4 - (-0.90D2 * t9 * 0.
     #3141592653589793D1 * (-t208 * t525 + t523) + t574) * t43 * t45 / 0
     #.720D3 + t525 * t6 * t8 * t606 / 0.8D1 + (-0.90D2 * t9 * 0.3141592
     #653589793D1 * (t224 * t525 - t523) - t574) * t50 * t45 / 0.720D3 -
     # (-0.90D2 * t9 * 0.3141592653589793D1 * (t522 + t238 * t525 / 0.2D
     #1 - t236 * t523) + 0.180D3 * t9 * t38 * (t523 - t236 * t525) + t53
     #8) * t45 / 0.720D3
      t634 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t633)
      t636 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t637 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t645 = 0.180D3 * t9 * t38 * t637
      t650 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t681 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t636 - t385 * t637)
     # + t645) * t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.31415926535897
     #93D1 * (t650 - t402 * t636 + t404 * t637 / 0.2D1) + 0.180D3 * t9 *
     # t38 * (t636 - t402 * t637) + t9 * t97 * t637) * t50 / 0.1440D4 - 
     #t48 * t637 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.314159265358979
     #3D1 * (-t427 * t637 + t636) + t645) * t50 * t45 / 0.720D3
      t682 = FJET(XB1, XB2, s, t378, 0.0D0, -t376, 0.0D0, 0.0D0, t681)
      t684 = t2 * t257
      t686 = x3 * s * t440
      t687 = t274 * t275
      t688 = t274 * t440
      t694 = log(-0.4D1 * t13 * t450 * t452 * t26 * t261)
      t695 = -t261
      t696 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695, 
     #x4)
      t698 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695, 
     #x4)
      t714 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t694 * t696 + t69
     #8) + 0.180D3 * t9 * t38 * t696) * t43 * t45 / 0.720D3 + t48 * t696
     # * t43 * t51 / 0.8D1
      t715 = FJET(XB1, XB2, s, t684, -t686, -t687, t688, 0.0D0, t714)
      t718 = Sqrt(-t162 * t261)
      t719 = t259 * t718
      t720 = 0.2D1 * t719
      t724 = t2 * t256 * (-t162 - 0.1D1 + x3 + t720) * t270
      t726 = 0.2D1 * t719 * x2
      t729 = t2 * (-x2 - x3 + t280 - t279 - t720 + t726) * t270
      t730 = 0.1D1 + t304 - x2
      t731 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345, 
     #x4)
      t735 = t269 ** 2
      t741 = log(-0.4D1 * t162 * t450 * t19 * t381 * t261 / t735)
      t742 = t741 * t730
      t743 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345, 
     #x4)
      t750 = 0.1D1 / (t279 - t315 + x2 - t326 - t304 + t316 + 0.2D1 * t7
     #19 * t304 - t726 + t720 - 0.1D1)
      t754 = lh * t730
      t755 = t743 * t750
      t764 = t9 * 0.3141592653589793D1 * t730
      t768 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t730 * t731 - t742 
     #* t743) * t750 + 0.180D3 * t48 * t754 * t755) * t43 * t50 / 0.1440
     #D4 - t764 * t755 * t349 / 0.8D1
      t769 = FJET(XB1, XB2, s, 0.0D0, -t724, 0.0D0, t729, 0.0D0, t768)
      t771 = t2 * x3
      t772 = t2 * t261
      t774 = t57 * t261
      t777 = log(-0.4D1 * x3 * t21 * t774)
      t778 = t777 ** 2
      t779 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t782 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t783 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t801 = log(-0.4D1 * t380 * t774)
      t809 = 0.180D3 * t9 * t38 * t779
      t817 = log(-0.4D1 * t13 * t21 * t774)
      t831 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t778 * t779 / 0.2
     #D1 - t782 + t777 * t783) + 0.180D3 * t9 * t38 * (t777 * t779 - t78
     #3) - t9 * t97 * t779) * t43 / 0.1440D4 + (-0.90D2 * t9 * 0.3141592
     #653589793D1 * (-t801 * t779 + t783) + t809) * t43 * t50 / 0.1440D4
     # - (-0.90D2 * t9 * 0.3141592653589793D1 * (-t783 + t817 * t779) - 
     #t809) * t43 * t45 / 0.720D3 - t48 * t779 * t43 * t51 / 0.8D1
      t832 = FJET(XB1, XB2, s, 0.0D0, t771, 0.0D0, -t772, 0.0D0, t831)
      t834 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t836 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t843 = 0.180D3 * t9 * t38 * t834
      t848 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t879 = (-0.90D2 * t9 * 0.3141592653589793D1 * (-t385 * t834 + t836
     #) + t843) * t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589
     #793D1 * (t848 + t404 * t834 / 0.2D1 - t402 * t836) + 0.180D3 * t9 
     #* t38 * (-t402 * t834 + t836) + t9 * t97 * t834) * t50 / 0.1440D4 
     #- t48 * t834 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.3141592653589
     #793D1 * (-t427 * t834 + t836) + t843) * t50 * t45 / 0.720D3
      t880 = FJET(XB1, XB2, s, 0.0D0, t378, 0.0D0, -t376, 0.0D0, t879)
      t882 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t885 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t886 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t909 = 0.180D3 * t9 * t38 * t882
      t927 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t778 * t882 / 0.2
     #D1 - t885 + t777 * t886) + 0.180D3 * t9 * t38 * (-t886 + t777 * t8
     #82) - t9 * t97 * t882) * t43 / 0.1440D4 + (-0.90D2 * t9 * 0.314159
     #2653589793D1 * (t886 - t801 * t882) + t909) * t43 * t50 / 0.1440D4
     # - (-0.90D2 * t9 * 0.3141592653589793D1 * (t817 * t882 - t886) - t
     #909) * t43 * t45 / 0.720D3 - t48 * t882 * t43 * t51 / 0.8D1
      t928 = FJET(XB1, XB2, s, t771, 0.0D0, -t772, 0.0D0, 0.0D0, t927)
      t930 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t931 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t934 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t69
     #5, x4)
      t957 = 0.180D3 * t9 * t38 * t931
      t975 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t930 - t778 * t93
     #1 / 0.2D1 + t777 * t934) + 0.180D3 * t9 * t38 * (-t934 + t777 * t9
     #31) - t9 * t97 * t931) * t43 / 0.1440D4 + (-0.90D2 * t9 * 0.314159
     #2653589793D1 * (-t801 * t931 + t934) + t957) * t43 * t50 / 0.1440D
     #4 - (-0.90D2 * t9 * 0.3141592653589793D1 * (t817 * t931 - t934) - 
     #t957) * t43 * t45 / 0.720D3 - t48 * t931 * t43 * t51 / 0.8D1
      t976 = FJET(XB1, XB2, s, 0.0D0, -t772, 0.0D0, t771, 0.0D0, t975)
      t978 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D1
     #, x4)
      t979 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D1
     #, x4)
      t987 = 0.180D3 * t9 * t38 * t979
      t1008 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D
     #1, x4)
      t1023 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t978 + t31 * t97
     #9) - t987) * t43 * t45 / 0.720D3 - t48 * t979 * t43 * t51 / 0.8D1 
     #+ (-0.90D2 * t9 * 0.3141592653589793D1 * (-t61 * t979 + t978) + t9
     #87) * t50 * t45 / 0.720D3 - (-0.90D2 * t9 * 0.3141592653589793D1 *
     # (-t80 * t979 / 0.2D1 + t78 * t978 - t1008) + 0.180D3 * t9 * t38 *
     # (-t978 + t78 * t979) - t9 * t97 * t979) * t45 / 0.720D3
      t1024 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t1023)
      t1026 = t104 * t103 + t254 * t253 + t353 * t6 * t355 * t342 * t346
     # * t24 * t349 / 0.8D1 + t367 * t6 * t355 * t342 * t362 * t24 * t34
     #9 / 0.8D1 + t438 * t437 + t472 * t471 + t520 * t519 + t634 * t633 
     #+ t682 * t681 + t715 * t714 + t769 * t768 + t832 * t831 + t880 * t
     #879 + t928 * t927 + t976 * t975 + t1024 * t1023
      t1027 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t6
     #95, x4)
      t1029 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t6
     #95, x4)
      t1030 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t6
     #95, x4)
      t1054 = 0.180D3 * t9 * t38 * t1030
      t1072 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (t777 * t1027 - t1
     #029 - t778 * t1030 / 0.2D1) + 0.180D3 * t9 * t38 * (-t1027 + t777 
     #* t1030) - t9 * t97 * t1030) * t43 / 0.1440D4 + (-0.90D2 * t9 * 0.
     #3141592653589793D1 * (t1027 - t801 * t1030) + t1054) * t43 * t50 /
     # 0.1440D4 - (-0.90D2 * t9 * 0.3141592653589793D1 * (-t1027 + t817 
     #* t1030) - t1054) * t43 * t45 / 0.720D3 - t48 * t1030 * t43 * t51 
     #/ 0.8D1
      t1073 = FJET(XB1, XB2, s, -t772, 0.0D0, t771, 0.0D0, 0.0D0, t1072)
      t1075 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345,
     # x4)
      t1077 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345,
     # x4)
      t1084 = t1077 * t750
      t1095 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t730 * t1075 - t74
     #2 * t1077) * t750 + 0.180D3 * t48 * t754 * t1084) * t43 * t50 / 0.
     #1440D4 - t764 * t1084 * t349 / 0.8D1
      t1096 = FJET(XB1, XB2, s, -t724, 0.0D0, t729, 0.0D0, 0.0D0, t1095)
      t1098 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1099 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1101 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1114 = t9 * t97 * t1101
      t1132 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1150 = 0.180D3 * t9 * t38 * t1101
      t1207 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (t1098 - t110 * t1
     #099 + t111 * t1101 / 0.2D1) + 0.180D3 * t9 * t38 * (t1099 - t110 *
     # t1101) + t1114) * t43 / 0.1440D4 + (-t1098 + t133 * t1099 - t150 
     #* t1101 / 0.2D1) * t6 * t203 / 0.8D1 - t9 * t146 * t1101 / 0.1440D
     #4 - (t154 * t1101 / 0.6D1 - t150 * t1099 / 0.2D1 - t1132 + t133 * 
     #t1098) * t6 * t137 / 0.16D2 + (-t1099 + t133 * t1101) * t6 * t138 
     #/ 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589793D1 * (t165 * t1101 
     #- t1099) - t1150) * t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.31415
     #92653589793D1 * (-t1098 + t181 * t1099 - t183 * t1101 / 0.2D1) + 0
     #.180D3 * t9 * t38 * (-t1099 + t181 * t1101) - t1114) * t50 / 0.144
     #0D4 - (-0.90D2 * t9 * 0.3141592653589793D1 * (-t208 * t1101 + t109
     #9) + t1150) * t43 * t45 / 0.720D3 + t1101 * t6 * t8 * t606 / 0.8D1
     # + (-0.90D2 * t9 * 0.3141592653589793D1 * (-t1099 + t224 * t1101) 
     #- t1150) * t50 * t45 / 0.720D3 - (-0.90D2 * t9 * 0.314159265358979
     #3D1 * (t1098 - t236 * t1099 + t238 * t1101 / 0.2D1) + 0.180D3 * t9
     # * t38 * (t1099 - t236 * t1101) + t1114) * t45 / 0.720D3
      t1208 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1207)
      t1210 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695,
     # x4)
      t1212 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695,
     # x4)
      t1228 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t694 * t1210 + t
     #1212) + 0.180D3 * t9 * t38 * t1210) * t43 * t45 / 0.720D3 + t48 * 
     #t1210 * t43 * t51 / 0.8D1
      t1229 = FJET(XB1, XB2, s, t688, -t687, -t686, t684, 0.0D0, t1228)
      t1231 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D
     #1, x4)
      t1233 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D
     #1, x4)
      t1240 = 0.180D3 * t9 * t38 * t1231
      t1261 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D
     #1, x4)
      t1276 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (t31 * t1231 - t12
     #33) - t1240) * t43 * t45 / 0.720D3 - t48 * t1231 * t43 * t51 / 0.8
     #D1 + (-0.90D2 * t9 * 0.3141592653589793D1 * (t1233 - t61 * t1231) 
     #+ t1240) * t50 * t45 / 0.720D3 - (-0.90D2 * t9 * 0.314159265358979
     #3D1 * (t78 * t1233 - t80 * t1231 / 0.2D1 - t1261) + 0.180D3 * t9 *
     # t38 * (-t1233 + t78 * t1231) - t9 * t97 * t1231) * t45 / 0.720D3
      t1277 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t1276)
      t1279 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D
     #1, x4)
      t1281 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D
     #1, x4)
      t1288 = 0.180D3 * t9 * t38 * t1279
      t1306 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, 0.10D
     #1, x4)
      t1324 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (t31 * t1279 - t12
     #81) - t1288) * t43 * t45 / 0.720D3 - t48 * t1279 * t43 * t51 / 0.8
     #D1 + (-0.90D2 * t9 * 0.3141592653589793D1 * (t1281 - t61 * t1279) 
     #+ t1288) * t50 * t45 / 0.720D3 - (-0.90D2 * t9 * 0.314159265358979
     #3D1 * (-t1306 + t78 * t1281 - t80 * t1279 / 0.2D1) + 0.180D3 * t9 
     #* t38 * (t78 * t1279 - t1281) - t9 * t97 * t1279) * t45 / 0.720D3
      t1325 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t1324)
      t1327 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, t345, x4
     #)
      t1332 = FJET(XB1, XB2, s, t277, t273, t295, -t292, -t300, t344 * t
     #1327 * t24 * t349 / 0.8D1)
      t1340 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695,
     # x4)
      t1342 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695,
     # x4)
      t1358 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t694 * t1340 + t
     #1342) + 0.180D3 * t9 * t38 * t1340) * t43 * t45 / 0.720D3 + t48 * 
     #t1340 * t43 * t51 / 0.8D1
      t1359 = FJET(XB1, XB2, s, -t686, t684, t688, -t687, 0.0D0, t1358)
      t1361 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1362 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1364 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1377 = t9 * t97 * t1364
      t1393 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1413 = 0.180D3 * t9 * t38 * t1364
      t1470 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (t1361 - t110 * t1
     #362 + t111 * t1364 / 0.2D1) + 0.180D3 * t9 * t38 * (t1362 - t110 *
     # t1364) + t1377) * t43 / 0.1440D4 + (-t1361 + t133 * t1362 - t150 
     #* t1364 / 0.2D1) * t6 * t203 / 0.8D1 - t9 * t146 * t1364 / 0.1440D
     #4 - (-t150 * t1362 / 0.2D1 - t1393 + t154 * t1364 / 0.6D1 + t133 *
     # t1361) * t6 * t137 / 0.16D2 + (-t1362 + t133 * t1364) * t6 * t138
     # / 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589793D1 * (-t1362 + t16
     #5 * t1364) - t1413) * t43 * t50 / 0.1440D4 + (-0.90D2 * t9 * 0.314
     #1592653589793D1 * (-t1361 + t181 * t1362 - t183 * t1364 / 0.2D1) +
     # 0.180D3 * t9 * t38 * (-t1362 + t181 * t1364) - t1377) * t50 / 0.1
     #440D4 - (-0.90D2 * t9 * 0.3141592653589793D1 * (t1362 - t208 * t13
     #64) + t1413) * t43 * t45 / 0.720D3 + t1364 * t6 * t8 * t606 / 0.8D
     #1 + (-0.90D2 * t9 * 0.3141592653589793D1 * (-t1362 + t224 * t1364)
     # - t1413) * t50 * t45 / 0.720D3 - (-0.90D2 * t9 * 0.31415926535897
     #93D1 * (t1361 + t238 * t1364 / 0.2D1 - t236 * t1362) + 0.180D3 * t
     #9 * t38 * (t1362 - t236 * t1364) + t1377) * t45 / 0.720D3
      t1471 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1470)
      t1473 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, 
     #x4)
      t1478 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, 
     #x4)
      t1491 = t48 * t1473 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.314159
     #2653589793D1 * (-t1478 + t457 * t1473) - 0.180D3 * t9 * t38 * t147
     #3) * t50 * t45 / 0.720D3
      t1492 = FJET(XB1, XB2, s, t3, t441, 0.0D0, -t444, -t300, t1491)
      t1494 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, 
     #x4)
      t1499 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, 
     #x4)
      t1512 = t48 * t1494 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.314159
     #2653589793D1 * (-t1499 + t457 * t1494) - 0.180D3 * t9 * t38 * t149
     #4) * t50 * t45 / 0.720D3
      t1513 = FJET(XB1, XB2, s, 0.0D0, -t444, t3, t441, -t300, t1512)
      t1515 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, 
     #x4)
      t1521 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t10, x2, 0.10D1, 
     #x4)
      t1533 = t48 * t1515 * t43 * t51 / 0.8D1 + (-0.90D2 * t9 * 0.314159
     #2653589793D1 * (t457 * t1515 - t1521) - 0.180D3 * t9 * t38 * t1515
     #) * t50 * t45 / 0.720D3
      t1534 = FJET(XB1, XB2, s, -t444, 0.0D0, t441, t3, -t300, t1533)
      t1536 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345,
     # x4)
      t1538 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345,
     # x4)
      t1545 = t1538 * t750
      t1556 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t730 * t1536 - t74
     #2 * t1538) * t750 + 0.180D3 * t48 * t754 * t1545) * t43 * t50 / 0.
     #1440D4 - t764 * t1545 * t349 / 0.8D1
      t1557 = FJET(XB1, XB2, s, t729, 0.0D0, -t724, 0.0D0, 0.0D0, t1556)
      t1559 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345,
     # x4)
      t1561 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t345,
     # x4)
      t1568 = t1561 * t750
      t1579 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t730 * t1559 - t74
     #2 * t1561) * t750 + 0.180D3 * t48 * t754 * t1568) * t43 * t50 / 0.
     #1440D4 - t764 * t1568 * t349 / 0.8D1
      t1580 = FJET(XB1, XB2, s, 0.0D0, t729, 0.0D0, -t724, 0.0D0, t1579)
      t1582 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t10, x2, t345, x4
     #)
      t1587 = FJET(XB1, XB2, s, t295, -t292, t277, t273, -t300, t344 * t
     #1582 * t24 * t349 / 0.8D1)
      t1595 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695,
     # x4)
      t1597 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t10, 0.0D0, t695,
     # x4)
      t1613 = -(-0.90D2 * t9 * 0.3141592653589793D1 * (-t694 * t1595 + t
     #1597) + 0.180D3 * t9 * t38 * t1595) * t43 * t45 / 0.720D3 + t48 * 
     #t1595 * t43 * t51 / 0.8D1
      t1614 = FJET(XB1, XB2, s, -t687, t688, t684, -t686, 0.0D0, t1613)
      t1616 = t1073 * t1072 + t1096 * t1095 + t1208 * t1207 + t1229 * t1
     #228 + t1277 * t1276 + t1325 * t1324 + t1332 * t6 * t355 * t342 * t
     #1327 * t24 * t349 / 0.8D1 + t1359 * t1358 + t1471 * t1470 + t1492 
     #* t1491 + t1513 * t1512 + t1534 * t1533 + t1557 * t1556 + t1580 * 
     #t1579 + t1587 * t6 * t355 * t342 * t1582 * t24 * t349 / 0.8D1 + t1
     #614 * t1613
      rrgq2qght8s2e0 = t1026 + t1616

      end function



      doubleprecision function rrgq2qght8s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x2
      t4 = x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x3
      t9 = Sqrt(-t4 * t7)
      t10 = t6 * t9
      t11 = 0.2D1 * t10
      t15 = 0.1D1 / (-0.1D1 + t4)
      t17 = t2 * t3 * (-t4 - 0.1D1 + x3 + t11) * t15
      t19 = x2 ** 2
      t20 = t19 * x3
      t22 = 0.2D1 * t10 * x2
      t25 = t2 * (-x2 - x3 + 0.3D1 * t4 - t20 - t11 + t22) * t15
      t26 = 0.1D1 / t1
      t27 = s ** 2
      t28 = 0.1D1 / t27
      t29 = t26 * t28
      t30 = x2 * z
      t31 = 0.1D1 + t30 - x2
      t33 = t29 * 0.3141592653589793D1 * t31
      t34 = t7 * t15
      t35 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t34, x4
     #)
      t42 = 0.1D1 / (t20 - t20 * z + x2 - 0.2D1 * t4 - t30 + t4 * z + 0.
     #2D1 * t10 * t30 - t22 + t11 - 0.1D1)
      t44 = 0.1D1 / x3
      t45 = 0.1D1 / x2
      t46 = t44 * t45
      t50 = FJET(XB1, XB2, s, -t17, 0.0D0, t25, 0.0D0, 0.0D0, -t33 * t35
     # * t42 * t46 / 0.16D2)
      t52 = t28 * 0.3141592653589793D1
      t56 = t42 * t44 * t45
      t60 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t34, x4
     #)
      t65 = FJET(XB1, XB2, s, 0.0D0, t25, 0.0D0, -t17, 0.0D0, -t33 * t60
     # * t42 * t46 / 0.16D2)
      t72 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t34, x4
     #)
      t77 = FJET(XB1, XB2, s, t25, 0.0D0, -t17, 0.0D0, 0.0D0, -t33 * t72
     # * t42 * t46 / 0.16D2)
      t84 = -0.1D1 + x1
      t85 = t2 * t84
      t86 = t2 * x1
      t87 = t29 * 0.3141592653589793D1
      t88 = -t84
      t89 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1,
     # x4)
      t91 = 0.1D1 / x1
      t95 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1,
     # x4)
      t96 = x1 ** 2
      t97 = Sin(t5)
      t98 = t97 ** 2
      t99 = t96 * t98
      t100 = z ** 2
      t101 = 0.1D1 / t100
      t103 = t1 ** 2
      t104 = t103 ** 2
      t107 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t109 = t84 ** 2
      t113 = log(0.4D1 * t99 * t101 * t104 * t107 * t109)
      t119 = 0.3141592653589793D1 * lh
      t130 = -t87 * t89 * t45 * t91 / 0.8D1 - (-0.90D2 * t29 * 0.3141592
     #653589793D1 * (-t95 + t113 * t89) - 0.180D3 * t29 * t119 * t89) * 
     #t91 / 0.720D3 - t87 * t89 * t44 * t91 / 0.8D1
      t131 = FJET(XB1, XB2, s, -t85, t86, 0.0D0, 0.0D0, 0.0D0, t130)
      t133 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t135 = t101 * t104
      t138 = log(0.4D1 * x3 * t98 * t135)
      t139 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t147 = 0.180D3 * t29 * t119 * t139
      t157 = log(0.4D1 * t99 * t135)
      t166 = t139 * t44
      t170 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t171 = t101 * t98
      t174 = log(0.4D1 * t171 * t104)
      t176 = t174 ** 2
      t183 = lh ** 2
      t185 = 0.3141592653589793D1 ** 2
      t188 = 0.3141592653589793D1 * (-0.180D3 * t183 + 0.30D2 * t185)
      t195 = x2 * t104
      t198 = log(0.4D1 * t195 * t171)
      t210 = t52 * lh
      t213 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (t133 - t138 * t13
     #9) + t147) * t44 / 0.1440D4 + t87 * t139 * t45 * t91 / 0.8D1 - (-0
     #.90D2 * t29 * 0.3141592653589793D1 * (t133 - t157 * t139) + t147) 
     #* t91 / 0.720D3 + t87 * t166 * t91 / 0.8D1 - (-t170 + t174 * t133 
     #- t176 * t139 / 0.2D1) * t26 * t52 / 0.16D2 - t29 * t188 * t139 / 
     #0.1440D4 + t87 * t166 * t45 / 0.16D2 + (-0.90D2 * t29 * 0.31415926
     #53589793D1 * (-t133 + t198 * t139) - t147) * t45 / 0.1440D4 + (-t1
     #33 + t174 * t139) * t26 * t210 / 0.8D1
      t214 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t213)
      t216 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1
     #, x4)
      t222 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1
     #, x4)
      t237 = -t87 * t216 * t45 * t91 / 0.8D1 - (-0.90D2 * t29 * 0.314159
     #2653589793D1 * (t113 * t216 - t222) - 0.180D3 * t29 * t119 * t216)
     # * t91 / 0.720D3 - t87 * t216 * t44 * t91 / 0.8D1
      t238 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t86, -t85, 0.0D0, t237)
      t240 = t2 * x3
      t241 = t2 * t7
      t246 = log(-0.4D1 * x3 * t104 * t171 * t7)
      t247 = -t7
      t248 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t250 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t261 = t248 * t44
      t268 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (t246 * t248 - t25
     #0) - 0.180D3 * t29 * t119 * t248) * t44 / 0.1440D4 - t87 * t261 * 
     #t45 / 0.16D2 - t87 * t261 * t91 / 0.8D1
      t269 = FJET(XB1, XB2, s, 0.0D0, t240, 0.0D0, -t241, 0.0D0, t268)
      t271 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t272 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t284 = t272 * t44
      t291 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (-t271 + t246 * t2
     #72) - 0.180D3 * t29 * t119 * t272) * t44 / 0.1440D4 - t87 * t284 *
     # t91 / 0.8D1 - t87 * t284 * t45 / 0.16D2
      t292 = FJET(XB1, XB2, s, t240, 0.0D0, -t241, 0.0D0, 0.0D0, t291)
      t295 = x2 * s * t1
      t296 = t3 * s
      t297 = t296 * t1
      t298 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t307 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t308 = t3 ** 2
      t312 = log(0.4D1 * t195 * t171 * t308)
      t324 = -t87 * t298 * t45 * t91 / 0.8D1 - t87 * t298 * t44 * t45 / 
     #0.16D2 + (-0.90D2 * t29 * 0.3141592653589793D1 * (t307 - t312 * t2
     #98) + 0.180D3 * t29 * t119 * t298) * t45 / 0.1440D4
      t325 = FJET(XB1, XB2, s, t295, 0.0D0, -t297, 0.0D0, 0.0D0, t324)
      t327 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t328 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t340 = t328 * t44
      t347 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (-t327 + t246 * t3
     #28) - 0.180D3 * t29 * t119 * t328) * t44 / 0.1440D4 - t87 * t340 *
     # t91 / 0.8D1 - t87 * t340 * t45 / 0.16D2
      t348 = FJET(XB1, XB2, s, -t241, 0.0D0, t240, 0.0D0, 0.0D0, t347)
      t350 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t351 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t24
     #7, x4)
      t363 = t351 * t44
      t370 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (-t350 + t246 * t3
     #51) - 0.180D3 * t29 * t119 * t351) * t44 / 0.1440D4 - t87 * t363 *
     # t91 / 0.8D1 - t87 * t363 * t45 / 0.16D2
      t371 = FJET(XB1, XB2, s, 0.0D0, -t241, 0.0D0, t240, 0.0D0, t370)
      t373 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t382 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t394 = -t87 * t373 * t45 * t91 / 0.8D1 - t87 * t373 * t44 * t45 / 
     #0.16D2 + (-0.90D2 * t29 * 0.3141592653589793D1 * (t382 - t312 * t3
     #73) + 0.180D3 * t29 * t119 * t373) * t45 / 0.1440D4
      t395 = FJET(XB1, XB2, s, -t297, 0.0D0, t295, 0.0D0, 0.0D0, t394)
      t397 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t398 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t406 = 0.180D3 * t29 * t119 * t398
      t415 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t427 = t398 * t26 * t28
      t428 = 0.3141592653589793D1 * t44
      t429 = t428 * t45
      t441 = 0.3141592653589793D1 * t45 * t91
      t452 = t428 * t91
      t455 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (t397 - t138 * t39
     #8) + t406) * t44 / 0.1440D4 + (-t397 + t174 * t398) * t26 * t210 /
     # 0.8D1 - (-t415 + t174 * t397 - t176 * t398 / 0.2D1) * t26 * t52 /
     # 0.16D2 - t29 * t188 * t398 / 0.1440D4 + t427 * t429 / 0.16D2 + (-
     #0.90D2 * t29 * 0.3141592653589793D1 * (-t397 + t198 * t398) - t406
     #) * t45 / 0.1440D4 + t427 * t441 / 0.8D1 - (-0.90D2 * t29 * 0.3141
     #592653589793D1 * (t397 - t157 * t398) + t406) * t91 / 0.720D3 + t4
     #27 * t452 / 0.8D1
      t456 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t455)
      t458 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t464 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t479 = -t87 * t458 * t44 * t45 / 0.16D2 + (-0.90D2 * t29 * 0.31415
     #92653589793D1 * (-t312 * t458 + t464) + 0.180D3 * t29 * t119 * t45
     #8) * t45 / 0.1440D4 - t87 * t458 * t45 * t91 / 0.8D1
      t480 = FJET(XB1, XB2, s, 0.0D0, -t297, 0.0D0, t295, 0.0D0, t479)
      t482 = -t50 * t26 * t52 * t31 * t35 * t56 / 0.16D2 - t65 * t26 * t
     #52 * t31 * t60 * t56 / 0.16D2 - t77 * t26 * t52 * t31 * t72 * t56 
     #/ 0.16D2 + t131 * t130 + t214 * t213 + t238 * t237 + t269 * t268 +
     # t292 * t291 + t325 * t324 + t348 * t347 + t371 * t370 + t395 * t3
     #94 + t456 * t455 + t480 * t479
      t483 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t484 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t492 = 0.180D3 * t29 * t119 * t484
      t501 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t513 = t484 * t26 * t28
      t536 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (t483 - t138 * t48
     #4) + t492) * t44 / 0.1440D4 + (-t483 + t174 * t484) * t26 * t210 /
     # 0.8D1 - (-t501 + t174 * t483 - t176 * t484 / 0.2D1) * t26 * t52 /
     # 0.16D2 - t29 * t188 * t484 / 0.1440D4 + t513 * t429 / 0.16D2 + (-
     #0.90D2 * t29 * 0.3141592653589793D1 * (-t483 + t198 * t484) - t492
     #) * t45 / 0.1440D4 + t513 * t441 / 0.8D1 - (-0.90D2 * t29 * 0.3141
     #592653589793D1 * (t483 - t157 * t484) + t492) * t91 / 0.720D3 + t5
     #13 * t452 / 0.8D1
      t537 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t536)
      t539 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1
     #, x4)
      t544 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1
     #, x4)
      t560 = -t87 * t539 * t45 * t91 / 0.8D1 - (-0.90D2 * t29 * 0.314159
     #2653589793D1 * (-t544 + t113 * t539) - 0.180D3 * t29 * t119 * t539
     #) * t91 / 0.720D3 - t87 * t539 * t44 * t91 / 0.8D1
      t561 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t85, t86, 0.0D0, t560)
      t563 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1
     #, x4)
      t569 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, 0.10D1
     #, x4)
      t584 = -t87 * t563 * t45 * t91 / 0.8D1 - (-0.90D2 * t29 * 0.314159
     #2653589793D1 * (t113 * t563 - t569) - 0.180D3 * t29 * t119 * t563)
     # * t91 / 0.720D3 - t87 * t563 * t44 * t91 / 0.8D1
      t585 = FJET(XB1, XB2, s, t86, -t85, 0.0D0, 0.0D0, 0.0D0, t584)
      t587 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t593 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t608 = -t87 * t587 * t44 * t45 / 0.16D2 + (-0.90D2 * t29 * 0.31415
     #92653589793D1 * (-t312 * t587 + t593) + 0.180D3 * t29 * t119 * t58
     #7) * t45 / 0.1440D4 - t87 * t587 * t45 * t91 / 0.8D1
      t609 = FJET(XB1, XB2, s, 0.0D0, t295, 0.0D0, -t297, 0.0D0, t608)
      t611 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t612 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t620 = 0.180D3 * t29 * t119 * t612
      t629 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t641 = t612 * t26 * t28
      t664 = -(-0.90D2 * t29 * 0.3141592653589793D1 * (t611 - t138 * t61
     #2) + t620) * t44 / 0.1440D4 + (-t611 + t174 * t612) * t26 * t210 /
     # 0.8D1 - (-t629 + t174 * t611 - t176 * t612 / 0.2D1) * t26 * t52 /
     # 0.16D2 - t29 * t188 * t612 / 0.1440D4 + t641 * t429 / 0.16D2 + (-
     #0.90D2 * t29 * 0.3141592653589793D1 * (-t611 + t198 * t612) - t620
     #) * t45 / 0.1440D4 + t641 * t441 / 0.8D1 - (-0.90D2 * t29 * 0.3141
     #592653589793D1 * (t611 - t157 * t612) + t620) * t91 / 0.720D3 + t6
     #41 * t452 / 0.8D1
      t665 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t664)
      t668 = t1 * t84
      t669 = x3 * s * t668
      t671 = t2 * x1 * x3
      t672 = t7 * s
      t673 = t672 * t668
      t675 = t672 * t1 * x1
      t676 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, t247, 
     #x4)
      t681 = FJET(XB1, XB2, s, -t669, t671, t673, -t675, 0.0D0, t87 * t6
     #76 * t44 * t91 / 0.8D1)
      t685 = t44 * t91
      t691 = t2 * t84 * x2 * t107
      t692 = t296 * t668
      t697 = s * t103 * x2 * x1 * t84 * t107
      t698 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t88, x2, 0.10D1, x
     #4)
      t703 = FJET(XB1, XB2, s, -t691, 0.0D0, t692, t86, -t697, t87 * t69
     #8 * t45 * t91 / 0.8D1)
      t707 = t45 * t91
      t711 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t88, x2, 0.10D1, x
     #4)
      t716 = FJET(XB1, XB2, s, 0.0D0, -t691, t86, t692, -t697, t87 * t71
     #1 * t45 * t91 / 0.8D1)
      t723 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, t247, 
     #x4)
      t728 = FJET(XB1, XB2, s, t673, -t675, -t669, t671, 0.0D0, t87 * t7
     #23 * t44 * t91 / 0.8D1)
      t735 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, t247, 
     #x4)
      t740 = FJET(XB1, XB2, s, t671, -t669, -t675, t673, 0.0D0, t87 * t7
     #35 * t44 * t91 / 0.8D1)
      t747 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t88, x2, 0.10D1, x
     #4)
      t752 = FJET(XB1, XB2, s, t86, t692, 0.0D0, -t691, -t697, t87 * t74
     #7 * t45 * t91 / 0.8D1)
      t759 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t34, x
     #4)
      t764 = FJET(XB1, XB2, s, 0.0D0, -t17, 0.0D0, t25, 0.0D0, -t33 * t7
     #59 * t42 * t46 / 0.16D2)
      t771 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t88, x2, 0.10D1, x
     #4)
      t776 = FJET(XB1, XB2, s, t692, t86, -t691, 0.0D0, -t697, t87 * t77
     #1 * t45 * t91 / 0.8D1)
      t783 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t88, 0.0D0, t247, 
     #x4)
      t788 = FJET(XB1, XB2, s, -t675, t673, t671, -t669, 0.0D0, t87 * t7
     #83 * t44 * t91 / 0.8D1)
      t795 = t537 * t536 + t561 * t560 + t585 * t584 + t609 * t608 + t66
     #5 * t664 + t681 * t26 * t28 * 0.3141592653589793D1 * t676 * t685 /
     # 0.8D1 + t703 * t26 * t28 * 0.3141592653589793D1 * t698 * t707 / 0
     #.8D1 + t716 * t26 * t28 * 0.3141592653589793D1 * t711 * t707 / 0.8
     #D1 + t728 * t26 * t28 * 0.3141592653589793D1 * t723 * t685 / 0.8D1
     # + t740 * t26 * t28 * 0.3141592653589793D1 * t735 * t685 / 0.8D1 +
     # t752 * t26 * t28 * 0.3141592653589793D1 * t747 * t707 / 0.8D1 - t
     #764 * t26 * t52 * t31 * t759 * t56 / 0.16D2 + t776 * t26 * t28 * 0
     #.3141592653589793D1 * t771 * t707 / 0.8D1 + t788 * t26 * t28 * 0.3
     #141592653589793D1 * t783 * t685 / 0.8D1
      rrgq2qght8s2em1 = t482 + t795

      end function



      doubleprecision function rrgq2qght8s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10D
     #1, x4)
      t4 = 0.1D1 / t1
      t5 = t3 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * 0.3141592653589793D1
      t9 = 0.1D1 / x3
      t10 = t8 * t9
      t13 = 0.1D1 / x2
      t14 = t8 * t13
      t17 = 0.1D1 / x1
      t18 = t8 * t17
      t21 = t4 * t7
      t22 = 0.3141592653589793D1 * lh
      t26 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t27 = z ** 2
      t30 = Sin(x4 * 0.3141592653589793D1)
      t31 = t30 ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t31 * t34)
      t43 = t5 * t10 / 0.16D2 + t5 * t14 / 0.16D2 + t5 * t18 / 0.8D1 - t
     #21 * t22 * t3 / 0.8D1 - (-t26 + t37 * t3) * t4 * t8 / 0.16D2
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t43)
      t46 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t47 = t46 * t4
      t57 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t63 = t47 * t10 / 0.16D2 + t47 * t14 / 0.16D2 + t47 * t18 / 0.8D1 
     #- t21 * t22 * t46 / 0.8D1 - (-t57 + t37 * t46) * t4 * t8 / 0.16D2
      t64 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t63)
      t66 = t2 * x1
      t67 = -0.1D1 + x1
      t68 = t2 * t67
      t69 = -t67
      t70 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t69, 0.0D0, 0.10D1,
     # x4)
      t72 = 0.3141592653589793D1 * t70 * t17
      t75 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t66, -t68, 0.0D0, -t21 * t72
     # / 0.8D1)
      t80 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t69, 0.0D0, 0.10D1,
     # x4)
      t82 = 0.3141592653589793D1 * t80 * t17
      t85 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t68, t66, 0.0D0, -t21 * t82
     # / 0.8D1)
      t90 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t91 = t90 * t4
      t101 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t107 = t91 * t10 / 0.16D2 + t91 * t14 / 0.16D2 + t91 * t18 / 0.8D1
     # - t21 * t22 * t90 / 0.8D1 - (-t101 + t37 * t90) * t4 * t8 / 0.16D
     #2
      t108 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t107)
      t110 = t2 * x3
      t111 = -0.1D1 + x3
      t112 = t2 * t111
      t113 = -t111
      t114 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #3, x4)
      t116 = 0.3141592653589793D1 * t114 * t9
      t119 = FJET(XB1, XB2, s, 0.0D0, t110, 0.0D0, -t112, 0.0D0, -t21 * 
     #t116 / 0.16D2)
      t125 = x2 * s * t1
      t128 = (-0.1D1 + x2) * s * t1
      t129 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t131 = 0.3141592653589793D1 * t129 * t13
      t134 = FJET(XB1, XB2, s, 0.0D0, t125, 0.0D0, -t128, 0.0D0, -t21 * 
     #t131 / 0.16D2)
      t139 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #3, x4)
      t141 = 0.3141592653589793D1 * t139 * t9
      t144 = FJET(XB1, XB2, s, 0.0D0, -t112, 0.0D0, t110, 0.0D0, -t21 * 
     #t141 / 0.16D2)
      t149 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t151 = 0.3141592653589793D1 * t149 * t13
      t154 = FJET(XB1, XB2, s, 0.0D0, -t128, 0.0D0, t125, 0.0D0, -t21 * 
     #t151 / 0.16D2)
      t159 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t160 = 0.3141592653589793D1 * t159
      t164 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t179 = t21 * t160 * t9 / 0.16D2 - (-t164 + t37 * t159) * t4 * t8 /
     # 0.16D2 + t21 * t160 * t13 / 0.16D2 - t21 * t22 * t159 / 0.8D1 + t
     #21 * t160 * t17 / 0.8D1
      t180 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t179)
      t182 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t69, 0.0D0, 0.10D1
     #, x4)
      t184 = 0.3141592653589793D1 * t182 * t17
      t187 = FJET(XB1, XB2, s, t66, -t68, 0.0D0, 0.0D0, 0.0D0, -t21 * t1
     #84 / 0.8D1)
      t192 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #3, x4)
      t194 = 0.3141592653589793D1 * t192 * t9
      t197 = FJET(XB1, XB2, s, t110, 0.0D0, -t112, 0.0D0, 0.0D0, -t21 * 
     #t194 / 0.16D2)
      t202 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t204 = 0.3141592653589793D1 * t202 * t13
      t207 = FJET(XB1, XB2, s, t125, 0.0D0, -t128, 0.0D0, 0.0D0, -t21 * 
     #t204 / 0.16D2)
      t212 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #3, x4)
      t214 = 0.3141592653589793D1 * t212 * t9
      t217 = FJET(XB1, XB2, s, -t112, 0.0D0, t110, 0.0D0, 0.0D0, -t21 * 
     #t214 / 0.16D2)
      t222 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t69, 0.0D0, 0.10D1
     #, x4)
      t224 = 0.3141592653589793D1 * t222 * t17
      t227 = FJET(XB1, XB2, s, -t68, t66, 0.0D0, 0.0D0, 0.0D0, -t21 * t2
     #24 / 0.8D1)
      t232 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t234 = 0.3141592653589793D1 * t232 * t13
      t237 = FJET(XB1, XB2, s, -t128, 0.0D0, t125, 0.0D0, 0.0D0, -t21 * 
     #t234 / 0.16D2)
      rrgq2qght8s2em2 = t44 * t43 + t64 * t63 - t75 * t4 * t7 * t72 / 0.
     #8D1 - t85 * t4 * t7 * t82 / 0.8D1 + t108 * t107 - t119 * t4 * t7 *
     # t116 / 0.16D2 - t134 * t4 * t7 * t131 / 0.16D2 - t144 * t4 * t7 *
     # t141 / 0.16D2 - t154 * t4 * t7 * t151 / 0.16D2 + t180 * t179 - t1
     #87 * t4 * t7 * t184 / 0.8D1 - t197 * t4 * t7 * t194 / 0.16D2 - t20
     #7 * t4 * t7 * t204 / 0.16D2 - t217 * t4 * t7 * t214 / 0.16D2 - t22
     #7 * t4 * t7 * t224 / 0.8D1 - t237 * t4 * t7 * t234 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10D
     #1, x4)
      t4 = 0.1D1 / t1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * 0.3141592653589793D1
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t3 * t4 * 
     #t8 / 0.16D2)
      t13 = t4 * t7
      t14 = t13 * 0.3141592653589793D1
      t16 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t16 * t4 *
     # t8 / 0.16D2)
      t23 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t27 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t23 * t4 *
     # t8 / 0.16D2)
      t30 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t34 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t13 * 0.31
     #41592653589793D1 * t30 / 0.16D2)
      rrgq2qght8s2em3 = t11 * t3 * t14 / 0.16D2 + t20 * t16 * t14 / 0.16
     #D2 + t27 * t23 * t14 / 0.16D2 + t34 * t4 * t8 * t30 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      rrgq2qght8s2em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght8s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = t17 * t19
      t22 = log(0.4D1 * t20)
      t23 = t22 ** 2
      t24 = t23 * t3
      t25 = t5 * 0.3141592653589793D1
      t26 = t25 * lh
      t29 = 0.3141592653589793D1 ** 2
      t32 = lh ** 2
      t35 = -0.60D2 * lh * t29 + 0.2884936567583026D3 + 0.120D3 * t32 * 
     #lh
      t36 = 0.3141592653589793D1 * t35
      t39 = t23 * t22 * t3
      t42 = t22 * t3
      t45 = -0.180D3 * t32 + 0.30D2 * t29
      t46 = t25 * t45
      t48 = 0.90D2 * t24 * t26 + t6 * t36 + 0.15D2 * t39 * t25 - t42 * t
     #46
      t49 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t56 = 0.3141592653589793D1 * t45
      t58 = -0.180D3 * t42 * t26 - 0.45D2 * t24 * t25 + t6 * t56
      t59 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t62 = 0.3141592653589793D1 * lh
      t63 = t6 * t62
      t67 = 0.180D3 * t63 + 0.90D2 * t42 * t25
      t68 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t78 = t29 ** 2
      t79 = t32 ** 2
      t86 = t23 ** 2
      t90 = -0.30D2 * t39 * t26 + t24 * t46 / 0.2D1 - t42 * t25 * t35 + 
     #t6 * 0.3141592653589793D1 * (-0.5769873135166051D3 * lh - t78 - 0.
     #60D2 * t79 + 0.60D2 * t32 * t29) - 0.15D2 / 0.4D1 * t86 * t3 * t25
      t91 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t94 = x1 ** 2
      t95 = x3 * t94
      t98 = log(0.4D1 * t95 * t20)
      t99 = t98 ** 2
      t103 = z * t59
      t104 = t95 * t16
      t105 = t13 * t19
      t106 = -0.1D1 + x3
      t107 = 0.1D1 / t106
      t111 = log(-0.4D1 * t104 * t105 * t107)
      t112 = t111 * z
      t114 = t111 ** 2
      t115 = t114 * z
      t119 = cos(t14)
      t120 = x3 * z
      t122 = Sqrt(-t120 * t106)
      t126 = 0.1D1 / (-z - x3 + 0.2D1 * t119 * t122)
      t133 = z * t49
      t142 = z * t91 * t126
      t147 = 0.1D1 / x3
      t149 = 0.1D1 / x1
      t152 = t94 * t16
      t155 = log(0.4D1 * t152 * t105)
      t161 = t155 ** 2
      t164 = t161 * t155
      t172 = t6 * t36 * t91
      t183 = x2 ** 2
      t184 = x3 * t183
      t185 = t184 * t94
      t190 = log(-0.4D1 * t185 * t17 * t19 * t107)
      t191 = t190 * z
      t195 = 0.1D1 - x2
      t196 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t199 = log(0.4D1 * t185 * t20)
      t201 = -t195
      t202 = t19 * t201
      t206 = log(-0.4D1 * t185 * t17 * t202)
      t207 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t213 = t142 - t207 + t91
      t219 = 0.1D1 / x2
      t220 = t219 * t149
      t223 = t183 * t94
      t224 = t223 * t16
      t228 = log(-0.4D1 * t224 * t105 * t201)
      t230 = t228 ** 2
      t235 = log(0.4D1 * t223 * t20)
      t237 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t238 = t235 ** 2
      t251 = t91 - t207
      t267 = x3 * t13
      t268 = t16 * t19
      t271 = log(0.4D1 * t267 * t268)
      t272 = t268 * t107
      t275 = log(-0.4D1 * t267 * t272)
      t278 = -t271 - t275 * z * t126
      t281 = t275 ** 2
      t285 = t271 ** 2
      t288 = -t281 * t275 * z * t126 / 0.6D1 - t285 * t271 / 0.6D1
      t302 = 0.1D1 + z * t126
      t314 = t285 / 0.2D1 + t281 * z * t126 / 0.2D1
      t321 = log(0.4D1 * t184 * t20)
      t322 = t321 ** 2
      t325 = t184 * t13
      t326 = t268 * t201
      t329 = log(-0.4D1 * t325 * t326)
      t332 = t329 ** 2
      t337 = log(-0.4D1 * t325 * t272)
      t338 = t337 * z
      t340 = t337 ** 2
      t341 = t340 * z
      t365 = t13 * t183
      t368 = log(-0.4D1 * t365 * t326)
      t372 = log(0.4D1 * t365 * t268)
      t378 = t372 ** 2
      t379 = t378 * t372
      t382 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t383 = t368 ** 2
      t386 = t383 * t368
      t411 = t6 * 0.3141592653589793D1 * t7 / 0.32D2 - t48 * t49 / 0.288
     #0D4 - t58 * t59 / 0.2880D4 - t67 * t68 / 0.2880D4 - t90 * t91 / 0.
     #2880D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t99 * t91 / 0.2D1
     # - t98 * t49 + t59 + (t103 - t112 * t49 + t115 * t91 / 0.2D1) * t1
     #26) + 0.180D3 * t6 * t62 * (-t98 * t91 + t49 + (t133 - t112 * t91)
     # * t126) + t6 * t56 * (t91 + t142)) * t147 * t149 / 0.1440D4 + (t6
     # * t56 * (-t49 + t155 * t91) - 0.90D2 * t6 * 0.3141592653589793D1 
     #* (t155 * t59 - t161 * t49 / 0.2D1 + t164 * t91 / 0.6D1 - t68) - t
     #172 + 0.180D3 * t6 * t62 * (t155 * t49 - t59 - t161 * t91 / 0.2D1)
     #) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * ((t13
     #3 - t191 * t91) * t126 + t49 - t196 - t199 * t91 + t206 * t207) + 
     #0.180D3 * t6 * t62 * t213) * t147 * t220 / 0.720D3 - (-0.90D2 * t6
     # * 0.3141592653589793D1 * (t228 * t196 + t59 - t230 * t207 / 0.2D1
     # - t235 * t49 - t237 + t238 * t91 / 0.2D1) + 0.180D3 * t6 * t62 * 
     #(-t196 + t49 + t228 * t207 - t235 * t91) + t6 * t56 * t251) * t219
     # * t149 / 0.720D3 - ((-0.90D2 * t6 * 0.3141592653589793D1 * t59 + 
     #0.180D3 * t6 * t62 * t49 + t6 * t56 * t91) * t278 - 0.90D2 * t6 * 
     #0.3141592653589793D1 * t91 * t288 + (t6 * t56 * t49 - 0.90D2 * t6 
     #* 0.3141592653589793D1 * t68 + t172 + 0.180D3 * t6 * t62 * t59) * 
     #t302 + (-0.90D2 * t6 * 0.3141592653589793D1 * t49 + 0.180D3 * t6 *
     # t62 * t91) * t314) * t147 / 0.2880D4 - (-0.90D2 * t6 * 0.31415926
     #53589793D1 * (-t237 + t322 * t91 / 0.2D1 + t329 * t196 - t321 * t4
     #9 - t332 * t207 / 0.2D1 + t59 + (t103 - t338 * t49 + t341 * t91 / 
     #0.2D1) * t126) + 0.180D3 * t6 * t62 * (-t321 * t91 - t196 + t329 *
     # t207 + t49 + (t133 - t338 * t91) * t126) + t6 * t56 * t213) * t14
     #7 * t219 / 0.1440D4 - (t6 * t56 * (-t196 + t49 + t368 * t207 - t37
     #2 * t91) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t372 * t59 - t37
     #9 * t91 / 0.6D1 - t382 - t383 * t196 / 0.2D1 + t68 + t386 * t207 /
     # 0.6D1 + t378 * t49 / 0.2D1 + t368 * t237) + t6 * t36 * t251 + 0.1
     #80D3 * t6 * t62 * (t368 * t196 + t59 - t383 * t207 / 0.2D1 - t372 
     #* t49 - t237 + t378 * t91 / 0.2D1)) * t219 / 0.1440D4
      t412 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t411)
      t414 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t417 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t418 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t419 = z * t418
      t420 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t426 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t428 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t437 = z * t420
      t447 = z * t414 * t126
      t448 = t414 - t428 + t447
      t460 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t464 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t476 = t414 - t428
      t492 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t524 = t6 * t36 * t414
      t629 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t322 * t414 / 0.2D
     #1 - t417 + t418 + (t419 - t338 * t420 + t341 * t414 / 0.2D1) * t12
     #6 + t329 * t426 - t332 * t428 / 0.2D1 - t321 * t420) + 0.180D3 * t
     #6 * t62 * (t420 - t321 * t414 + (t437 - t338 * t414) * t126 - t426
     # + t329 * t428) + t6 * t56 * t448) * t147 * t219 / 0.1440D4 - (t6 
     #* t56 * (-t426 + t368 * t428 + t420 - t372 * t414) - 0.90D2 * t6 *
     # 0.3141592653589793D1 * (t460 - t379 * t414 / 0.6D1 - t372 * t418 
     #- t464 - t383 * t426 / 0.2D1 + t378 * t420 / 0.2D1 + t386 * t428 /
     # 0.6D1 + t368 * t417) + t6 * t36 * t476 + 0.180D3 * t6 * t62 * (t3
     #78 * t414 / 0.2D1 - t417 - t383 * t428 / 0.2D1 + t368 * t426 + t41
     #8 - t372 * t420)) * t219 / 0.1440D4 + t6 * 0.3141592653589793D1 * 
     #t492 / 0.32D2 - t48 * t420 / 0.2880D4 - t58 * t418 / 0.2880D4 - t6
     #7 * t460 / 0.2880D4 - t90 * t414 / 0.2880D4 - ((-0.90D2 * t6 * 0.3
     #141592653589793D1 * t418 + 0.180D3 * t6 * t62 * t420 + t6 * t56 * 
     #t414) * t278 - 0.90D2 * t6 * 0.3141592653589793D1 * t414 * t288 + 
     #(t6 * t56 * t420 - 0.90D2 * t6 * 0.3141592653589793D1 * t460 + t52
     #4 + 0.180D3 * t6 * t62 * t418) * t302 + (-0.90D2 * t6 * 0.31415926
     #53589793D1 * t420 + 0.180D3 * t6 * t62 * t414) * t314) * t147 / 0.
     #2880D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t99 * t414 / 0.2D
     #1 + t418 - t98 * t420 + (t419 - t112 * t420 + t115 * t414 / 0.2D1)
     # * t126) + 0.180D3 * t6 * t62 * (t420 - t98 * t414 + (t437 - t112 
     #* t414) * t126) + t6 * t56 * (t414 + t447)) * t147 * t149 / 0.1440
     #D4 + (t6 * t56 * (-t420 + t155 * t414) - 0.90D2 * t6 * 0.314159265
     #3589793D1 * (t155 * t418 + t164 * t414 / 0.6D1 - t161 * t420 / 0.2
     #D1 - t460) - t524 + 0.180D3 * t6 * t62 * (-t161 * t414 / 0.2D1 - t
     #418 + t155 * t420)) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.31415926
     #53589793D1 * ((t437 - t191 * t414) * t126 + t420 - t199 * t414 + t
     #206 * t428 - t426) + 0.180D3 * t6 * t62 * t448) * t147 * t220 / 0.
     #720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t238 * t414 / 0.2D
     #1 - t417 - t230 * t428 / 0.2D1 + t228 * t426 + t418 - t235 * t420)
     # + 0.180D3 * t6 * t62 * (-t426 + t228 * t428 + t420 - t235 * t414)
     # + t6 * t56 * t476) * t219 * t149 / 0.720D3
      t630 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t629)
      t632 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t633 = z * t632
      t634 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t636 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t649 = z * t634
      t658 = z * t636 * t126
      t673 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t681 = t6 * t36 * t636
      t695 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t696 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t703 = t658 + t636 - t696
      t711 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t728 = t636 - t696
      t772 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10D
     #1, x4)
      t800 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t847 = -(-0.90D2 * t6 * 0.3141592653589793D1 * ((t633 - t112 * t63
     #4 + t115 * t636 / 0.2D1) * t126 - t98 * t634 + t99 * t636 / 0.2D1 
     #+ t632) + 0.180D3 * t6 * t62 * (t634 - t98 * t636 + (t649 - t112 *
     # t636) * t126) + t6 * t56 * (t636 + t658)) * t147 * t149 / 0.1440D
     #4 + (t6 * t56 * (-t634 + t155 * t636) - 0.90D2 * t6 * 0.3141592653
     #589793D1 * (t155 * t632 + t164 * t636 / 0.6D1 - t673 - t161 * t634
     # / 0.2D1) - t681 + 0.180D3 * t6 * t62 * (-t632 - t161 * t636 / 0.2
     #D1 + t155 * t634)) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.314159265
     #3589793D1 * (t634 + (t649 - t191 * t636) * t126 - t695 + t206 * t6
     #96 - t199 * t636) + 0.180D3 * t6 * t62 * t703) * t147 * t220 / 0.7
     #20D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t711 + t228 * t695
     # + t632 - t230 * t696 / 0.2D1 + t238 * t636 / 0.2D1 - t235 * t634)
     # + 0.180D3 * t6 * t62 * (-t695 + t228 * t696 + t634 - t235 * t636)
     # + t6 * t56 * t728) * t219 * t149 / 0.720D3 - (-0.90D2 * t6 * 0.31
     #41592653589793D1 * (-t321 * t634 + t632 + t322 * t636 / 0.2D1 - t3
     #32 * t696 / 0.2D1 + (t633 - t338 * t634 + t341 * t636 / 0.2D1) * t
     #126 + t329 * t695 - t711) + 0.180D3 * t6 * t62 * ((t649 - t338 * t
     #636) * t126 + t634 - t695 - t321 * t636 + t329 * t696) + t6 * t56 
     #* t703) * t147 * t219 / 0.1440D4 - (t6 * t56 * (-t695 + t368 * t69
     #6 + t634 - t372 * t636) - 0.90D2 * t6 * 0.3141592653589793D1 * (t3
     #86 * t696 / 0.6D1 - t772 + t673 - t379 * t636 / 0.6D1 - t372 * t63
     #2 - t383 * t695 / 0.2D1 + t378 * t634 / 0.2D1 + t368 * t711) + t6 
     #* t36 * t728 + 0.180D3 * t6 * t62 * (-t711 + t368 * t695 + t632 - 
     #t383 * t696 / 0.2D1 + t378 * t636 / 0.2D1 - t372 * t634)) * t219 /
     # 0.1440D4 + t6 * 0.3141592653589793D1 * t800 / 0.32D2 - t48 * t634
     # / 0.2880D4 - t58 * t632 / 0.2880D4 - t67 * t673 / 0.2880D4 - ((-0
     #.90D2 * t6 * 0.3141592653589793D1 * t632 + 0.180D3 * t6 * t62 * t6
     #34 + t6 * t56 * t636) * t278 - 0.90D2 * t6 * 0.3141592653589793D1 
     #* t636 * t288 + (t6 * t56 * t634 - 0.90D2 * t6 * 0.314159265358979
     #3D1 * t673 + t681 + 0.180D3 * t6 * t62 * t632) * t302 + (-0.90D2 *
     # t6 * 0.3141592653589793D1 * t634 + 0.180D3 * t6 * t62 * t636) * t
     #314) * t147 / 0.2880D4 - t90 * t636 / 0.2880D4
      t848 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t847)
      t850 = x2 * x3
      t851 = 0.1D1 - x3 + t850
      t852 = 0.1D1 / t851
      t853 = t850 * t852
      t854 = t2 * t853
      t855 = t106 * t852
      t856 = t2 * t855
      t857 = t184 * t152
      t858 = t201 * t106
      t859 = t851 ** 2
      t860 = 0.1D1 / t859
      t861 = t858 * t860
      t865 = log(0.4D1 * t857 * t105 * t861)
      t866 = t865 * z
      t868 = Sqrt(t120 * t858)
      t872 = 0.1D1 / (-z - x3 + t850 + 0.2D1 * t119 * t868)
      t873 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t855
     #, x4)
      t874 = t872 * t873
      t876 = z * t872
      t877 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t855
     #, x4)
      t878 = t876 * t877
      t883 = t6 * 0.3141592653589793D1
      t884 = lh * z
      t897 = log(0.4D1 * t184 * t17 * t202 * t106 * t860)
      t898 = t897 ** 2
      t899 = t898 * z
      t902 = t897 * z
      t905 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t855
     #, x4)
      t916 = t45 * z
      t923 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t866 * t874 - t878
     #) - 0.180D3 * t883 * t884 * t874) * t147 * t220 / 0.720D3 - (-0.90
     #D2 * t6 * 0.3141592653589793D1 * (-t899 * t874 / 0.2D1 + t902 * t8
     #72 * t877 - t876 * t905) + 0.180D3 * t6 * t62 * (t902 * t874 - t87
     #8) - t883 * t916 * t874) * t147 * t219 / 0.1440D4
      t924 = FJET(XB1, XB2, s, 0.0D0, t854, 0.0D0, -t856, 0.0D0, t923)
      t927 = t1 * x1
      t928 = x1 * z
      t929 = -z - x1 + t928
      t930 = 0.1D1 / t929
      t932 = t201 * s * t927 * t930
      t933 = -0.1D1 + x1
      t934 = t2 * t933
      t936 = x2 * s * t927
      t937 = s * t18
      t940 = x1 * t933 * t930
      t941 = t937 * t201 * t940
      t942 = 0.1D1 / t11
      t943 = t942 * t19
      t944 = t933 ** 2
      t945 = t930 * t944
      t950 = log(0.4D1 * t857 * t943 * t945 * t201)
      t951 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1, 
     #x4)
      t953 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1, 
     #x4)
      t964 = t16 * t942
      t966 = t19 * t930
      t971 = log(0.4D1 * t223 * t964 * t966 * t944 * t201)
      t972 = t971 ** 2
      t976 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1, 
     #x4)
      t992 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t950 * t951 + t95
     #3) + 0.180D3 * t6 * t62 * t951) * t147 * t220 / 0.720D3 - (-0.90D2
     # * t6 * 0.3141592653589793D1 * (t972 * t951 / 0.2D1 - t971 * t953 
     #+ t976) + 0.180D3 * t6 * t62 * (-t971 * t951 + t953) + t6 * t56 * 
     #t951) * t219 * t149 / 0.720D3
      t993 = FJET(XB1, XB2, s, 0.0D0, t932, -t934, t936, -t941, t992)
      t996 = t2 * x1 * t930
      t997 = t937 * t940
      t998 = z * t929
      t999 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t1006 = log(0.4D1 * t95 * t964 * t966 * t944 * t107)
      t1007 = t1006 * z
      t1008 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1011 = t1006 ** 2
      t1012 = t1011 * z
      t1013 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1014 = t929 * t1013
      t1018 = x3 * x1
      t1019 = t1018 * z
      t1020 = t95 * t11
      t1022 = 0.2D1 * t95 * z
      t1023 = x1 * t11
      t1024 = x3 * t11
      t1025 = t1024 * x1
      t1026 = x3 * t929
      t1028 = Sqrt(t1026 * t106)
      t1033 = 0.1D1 / (-t928 - t1019 - t1020 + t1022 - t95 - t120 + t102
     #3 + t1025 + 0.2D1 * t119 * t1028 * z - t11)
      t1035 = t943 * t945
      t1038 = log(-0.4D1 * t104 * t1035)
      t1040 = t1038 ** 2
      t1047 = t998 * t1008
      t1058 = -t1013 + t998 * t1013 * t1033
      t1065 = t152 * t942
      t1066 = t966 * t944
      t1069 = log(-0.4D1 * t1065 * t1066)
      t1075 = t1069 ** 2
      t1076 = t1075 * t1069
      t1079 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1100 = log(-0.4D1 * t857 * t1035)
      t1106 = log(0.4D1 * t857 * t943 * t945 * t107)
      t1107 = t1106 * z
      t1124 = log(-0.4D1 * t224 * t1035)
      t1125 = t1124 ** 2
      t1144 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-(-t998 * t999 + 
     #t1007 * t929 * t1008 - t1012 * t1014 / 0.2D1) * t1033 + t1038 * t1
     #008 - t999 - t1040 * t1013 / 0.2D1) + 0.180D3 * t6 * t62 * (-(-t10
     #47 + t1007 * t1014) * t1033 + t1038 * t1013 - t1008) + t6 * t56 * 
     #t1058) * t147 * t149 / 0.1440D4 + (t6 * t56 * (-t1069 * t1013 + t1
     #008) - 0.90D2 * t6 * 0.3141592653589793D1 * (-t1069 * t999 - t1076
     # * t1013 / 0.6D1 + t1079 + t1075 * t1008 / 0.2D1) + t6 * t36 * t10
     #13 + 0.180D3 * t6 * t62 * (t999 + t1075 * t1013 / 0.2D1 - t1069 * 
     #t1008)) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 *
     # (-t1008 + t1100 * t1013 - (-t1047 + t1107 * t1014) * t1033) + 0.1
     #80D3 * t6 * t62 * t1058) * t147 * t220 / 0.720D3 - (-0.90D2 * t6 *
     # 0.3141592653589793D1 * (-t999 - t1125 * t1013 / 0.2D1 + t1124 * t
     #1008) + 0.180D3 * t6 * t62 * (t1124 * t1013 - t1008) - t6 * t56 * 
     #t1013) * t219 * t149 / 0.720D3
      t1145 = FJET(XB1, XB2, s, 0.0D0, -t934, -t996, 0.0D0, t997, t1144)
      t1147 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1149 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1153 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1154 = t929 * t1153
      t1165 = t998 * t1147
      t1176 = t998 * t1153 * t1033 - t1153
      t1187 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1242 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t1038 * t1147 - t
     #1149 - (-t998 * t1149 + t1007 * t929 * t1147 - t1012 * t1154 / 0.2
     #D1) * t1033 - t1040 * t1153 / 0.2D1) + 0.180D3 * t6 * t62 * (-(-t1
     #165 + t1007 * t1154) * t1033 + t1038 * t1153 - t1147) + t6 * t56 *
     # t1176) * t147 * t149 / 0.1440D4 + (t6 * t56 * (t1147 - t1069 * t1
     #153) - 0.90D2 * t6 * 0.3141592653589793D1 * (t1187 + t1075 * t1147
     # / 0.2D1 - t1076 * t1153 / 0.6D1 - t1069 * t1149) + t6 * t36 * t11
     #53 + 0.180D3 * t6 * t62 * (t1149 + t1075 * t1153 / 0.2D1 - t1069 *
     # t1147)) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 
     #* (-(-t1165 + t1107 * t1154) * t1033 + t1100 * t1153 - t1147) + 0.
     #180D3 * t6 * t62 * t1176) * t147 * t220 / 0.720D3 - (-0.90D2 * t6 
     #* 0.3141592653589793D1 * (-t1149 - t1125 * t1153 / 0.2D1 + t1124 *
     # t1147) + 0.180D3 * t6 * t62 * (-t1147 + t1124 * t1153) - t6 * t56
     # * t1153) * t219 * t149 / 0.720D3
      t1243 = FJET(XB1, XB2, s, 0.0D0, -t996, -t934, 0.0D0, t997, t1242)
      t1245 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1246 = t872 * t1245
      t1248 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1249 = t876 * t1248
      t1265 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1282 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t866 * t1246 - t1
     #249) - 0.180D3 * t883 * t884 * t1246) * t147 * t220 / 0.720D3 - (-
     #0.90D2 * t6 * 0.3141592653589793D1 * (-t899 * t1246 / 0.2D1 + t902
     # * t872 * t1248 - t876 * t1265) + 0.180D3 * t6 * t62 * (t902 * t12
     #46 - t1249) - t883 * t916 * t1246) * t147 * t219 / 0.1440D4
      t1283 = FJET(XB1, XB2, s, 0.0D0, -t856, 0.0D0, t854, 0.0D0, t1282)
      t1285 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1289 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1293 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1304 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1309 = t6 * t36 * t1293
      t1329 = z * t1285
      t1340 = z * t1289
      t1349 = z * t1293 * t126
      t1384 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10
     #D1, x4)
      t1385 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10
     #D1, x4)
      t1391 = -t1385 + t1293 + t1349
      t1405 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10
     #D1, x4)
      t1416 = -t1385 + t1293
      t1423 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1474 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, 0.10
     #D1, x4)
      t1500 = -((-0.90D2 * t6 * 0.3141592653589793D1 * t1285 + 0.180D3 *
     # t6 * t62 * t1289 + t6 * t56 * t1293) * t278 - 0.90D2 * t6 * 0.314
     #1592653589793D1 * t1293 * t288 + (t6 * t56 * t1289 - 0.90D2 * t6 *
     # 0.3141592653589793D1 * t1304 + t1309 + 0.180D3 * t6 * t62 * t1285
     #) * t302 + (-0.90D2 * t6 * 0.3141592653589793D1 * t1289 + 0.180D3 
     #* t6 * t62 * t1293) * t314) * t147 / 0.2880D4 - (-0.90D2 * t6 * 0.
     #3141592653589793D1 * (-t98 * t1289 + t1285 + t99 * t1293 / 0.2D1 +
     # (t1329 - t112 * t1289 + t115 * t1293 / 0.2D1) * t126) + 0.180D3 *
     # t6 * t62 * (-t98 * t1293 + t1289 + (t1340 - t112 * t1293) * t126)
     # + t6 * t56 * (t1293 + t1349)) * t147 * t149 / 0.1440D4 + (t6 * t5
     #6 * (-t1289 + t155 * t1293) - 0.90D2 * t6 * 0.3141592653589793D1 *
     # (-t161 * t1289 / 0.2D1 + t155 * t1285 - t1304 + t164 * t1293 / 0.
     #6D1) - t1309 + 0.180D3 * t6 * t62 * (-t161 * t1293 / 0.2D1 + t155 
     #* t1289 - t1285)) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653
     #589793D1 * (-t199 * t1293 + t1289 + (t1340 - t191 * t1293) * t126 
     #- t1384 + t206 * t1385) + 0.180D3 * t6 * t62 * t1391) * t147 * t22
     #0 / 0.720D3 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t230 * t138
     #5 / 0.2D1 + t228 * t1384 + t238 * t1293 / 0.2D1 - t235 * t1289 + t
     #1285 - t1405) + 0.180D3 * t6 * t62 * (-t235 * t1293 + t1289 + t228
     # * t1385 - t1384) + t6 * t56 * t1416) * t219 * t149 / 0.720D3 + t6
     # * 0.3141592653589793D1 * t1423 / 0.32D2 - t90 * t1293 / 0.2880D4 
     #- (-0.90D2 * t6 * 0.3141592653589793D1 * (-t321 * t1289 + t329 * t
     #1384 + t1285 - t332 * t1385 / 0.2D1 - t1405 + (t1329 - t338 * t128
     #9 + t341 * t1293 / 0.2D1) * t126 + t322 * t1293 / 0.2D1) + 0.180D3
     # * t6 * t62 * (t1289 + (t1340 - t338 * t1293) * t126 - t321 * t129
     #3 - t1384 + t329 * t1385) + t6 * t56 * t1391) * t147 * t219 / 0.14
     #40D4 - (t6 * t56 * (t1289 - t1384 + t368 * t1385 - t372 * t1293) -
     # 0.90D2 * t6 * 0.3141592653589793D1 * (t378 * t1289 / 0.2D1 + t368
     # * t1405 + t386 * t1385 / 0.6D1 - t372 * t1285 - t383 * t1384 / 0.
     #2D1 + t1304 - t379 * t1293 / 0.6D1 - t1474) + t6 * t36 * t1416 + 0
     #.180D3 * t6 * t62 * (-t1405 + t378 * t1293 / 0.2D1 + t368 * t1384 
     #+ t1285 - t383 * t1385 / 0.2D1 - t372 * t1289)) * t219 / 0.1440D4 
     #- t58 * t1285 / 0.2880D4 - t67 * t1304 / 0.2880D4 - t48 * t1289 / 
     #0.2880D4
      t1501 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1500)
      t1503 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1504 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1518 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1535 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t1503 - t950 * t1
     #504) + 0.180D3 * t6 * t62 * t1504) * t147 * t220 / 0.720D3 - (-0.9
     #0D2 * t6 * 0.3141592653589793D1 * (t972 * t1504 / 0.2D1 + t1518 - 
     #t971 * t1503) + 0.180D3 * t6 * t62 * (t1503 - t971 * t1504) + t6 *
     # t56 * t1504) * t219 * t149 / 0.720D3
      t1536 = FJET(XB1, XB2, s, t936, -t934, t932, 0.0D0, -t941, t1535)
      t1538 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1539 = t872 * t1538
      t1542 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1545 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1552 = t876 * t1542
      t1575 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t899 * t1539 / 0
     #.2D1 + t902 * t872 * t1542 - t876 * t1545) + 0.180D3 * t6 * t62 * 
     #(t902 * t1539 - t1552) - t883 * t916 * t1539) * t147 * t219 / 0.14
     #40D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t1552 + t866 * t15
     #39) - 0.180D3 * t883 * t884 * t1539) * t147 * t220 / 0.720D3
      t1576 = FJET(XB1, XB2, s, t854, 0.0D0, -t856, 0.0D0, 0.0D0, t1575)
      t1578 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1580 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1594 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1610 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t950 * t1578 + t
     #1580) + 0.180D3 * t6 * t62 * t1578) * t147 * t220 / 0.720D3 - (-0.
     #90D2 * t6 * 0.3141592653589793D1 * (t972 * t1578 / 0.2D1 - t971 * 
     #t1580 + t1594) + 0.180D3 * t6 * t62 * (t1580 - t971 * t1578) + t6 
     #* t56 * t1578) * t219 * t149 / 0.720D3
      t1611 = FJET(XB1, XB2, s, t932, 0.0D0, t936, -t934, -t941, t1610)
      t1616 = t106 * s * t1 * t933 * t852
      t1617 = t2 * x1
      t1619 = Sqrt(-t1026 * t858)
      t1620 = t119 * t1619
      t1626 = t1617 * x2 * (-x3 + t850 - z + t120 - x1 + t1018 + t928 - 
     #t1019 + 0.2D1 * t1620) * t930 * t852
      t1627 = t934 * t853
      t1628 = t184 * t928
      t1629 = t184 * x1
      t1636 = t1617 * (-x2 + t850 - t1628 + t1629 + t184 * z + 0.1D1 - x
     #3 + 0.2D1 * t1620 * x2) * t930 * t852
      t1637 = x2 * x1
      t1638 = t1637 * z
      t1646 = x2 * t94
      t1651 = 0.2D1 * t1620 * t1638 - 0.2D1 * t1620 * z - t850 * z + t85
     #0 * x1 - t95 * x2 - t1629 - 0.2D1 * t1646 * z + t1646 * t11 + t163
     #8 - t1637 * t11 - t1022 + t1020 + t1019
      t1662 = -t1025 + t1646 + t1628 + t11 - 0.2D1 * t850 * t928 + t1024
     # * t1637 + 0.2D1 * t95 * x2 * z - t95 * t11 * x2 - 0.2D1 * t1620 *
     # t1637 + t95 + t120 - t1023 + t928
      t1664 = 0.1D1 / (t1651 + t1662)
      t1665 = -z - t1637 + t1638
      t1666 = t1664 * t1665
      t1667 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t1673 = log(-0.4D1 * t184 * t1065 * t1066 * t861)
      t1674 = t1673 * t1664
      t1675 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t1687 = 0.90D2 * t6 * 0.3141592653589793D1 * (t1666 * t1667 - t167
     #4 * t1665 * t1675) * t929 - 0.180D3 * t63 * t1666 * t1675 * t929
      t1691 = FJET(XB1, XB2, s, t1616, t1626, -t1627, -t1636, -t941, -t1
     #687 * t147 * t220 / 0.720D3)
      t1694 = t147 * t219 * t149
      t1697 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t1699 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t1711 = 0.90D2 * t6 * 0.3141592653589793D1 * (t1666 * t1697 - t167
     #4 * t1665 * t1699) * t929 - 0.180D3 * t63 * t1666 * t1699 * t929
      t1715 = FJET(XB1, XB2, s, t1626, t1616, -t1636, -t1627, -t941, -t1
     #711 * t147 * t220 / 0.720D3)
      t1719 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1720 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1723 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1728 = t929 * t1720
      t1737 = t998 * t1723
      t1748 = -t1720 + t998 * t1720 * t1033
      t1759 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1814 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1719 - t1040 * 
     #t1720 / 0.2D1 + t1038 * t1723 - (-t998 * t1719 + t1007 * t929 * t1
     #723 - t1012 * t1728 / 0.2D1) * t1033) + 0.180D3 * t6 * t62 * (-(-t
     #1737 + t1007 * t1728) * t1033 + t1038 * t1720 - t1723) + t6 * t56 
     #* t1748) * t147 * t149 / 0.1440D4 + (t6 * t56 * (-t1069 * t1720 + 
     #t1723) - 0.90D2 * t6 * 0.3141592653589793D1 * (t1759 + t1075 * t17
     #23 / 0.2D1 - t1076 * t1720 / 0.6D1 - t1069 * t1719) + t6 * t36 * t
     #1720 + 0.180D3 * t6 * t62 * (t1075 * t1720 / 0.2D1 + t1719 - t1069
     # * t1723)) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t1723 - (-t1737 + t1107 * t1728) * t1033 + t1100 * t1720) + 
     #0.180D3 * t6 * t62 * t1748) * t147 * t220 / 0.720D3 - (-0.90D2 * t
     #6 * 0.3141592653589793D1 * (-t1125 * t1720 / 0.2D1 + t1124 * t1723
     # - t1719) + 0.180D3 * t6 * t62 * (t1124 * t1720 - t1723) - t6 * t5
     #6 * t1720) * t219 * t149 / 0.720D3
      t1815 = FJET(XB1, XB2, s, -t934, 0.0D0, 0.0D0, -t996, t997, t1814)
      t1817 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1819 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1830 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, t195, 0.10D1,
     # x4)
      t1849 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t950 * t1817 + t
     #1819) + 0.180D3 * t6 * t62 * t1817) * t147 * t220 / 0.720D3 - (-0.
     #90D2 * t6 * 0.3141592653589793D1 * (t1830 + t972 * t1817 / 0.2D1 -
     # t971 * t1819) + 0.180D3 * t6 * t62 * (-t971 * t1817 + t1819) + t6
     # * t56 * t1817) * t219 * t149 / 0.720D3
      t1850 = FJET(XB1, XB2, s, -t934, t936, 0.0D0, t932, -t941, t1849)
      t1852 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1854 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1858 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1859 = t929 * t1858
      t1870 = t998 * t1852
      t1881 = -t1858 + t998 * t1858 * t1033
      t1897 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1947 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t1038 * t1852 - (
     #-t998 * t1854 + t1007 * t929 * t1852 - t1012 * t1859 / 0.2D1) * t1
     #033 - t1040 * t1858 / 0.2D1 - t1854) + 0.180D3 * t6 * t62 * (-t185
     #2 - (-t1870 + t1007 * t1859) * t1033 + t1038 * t1858) + t6 * t56 *
     # t1881) * t147 * t149 / 0.1440D4 + (t6 * t56 * (-t1069 * t1858 + t
     #1852) - 0.90D2 * t6 * 0.3141592653589793D1 * (t1075 * t1852 / 0.2D
     #1 - t1076 * t1858 / 0.6D1 - t1069 * t1854 + t1897) + t6 * t36 * t1
     #858 + 0.180D3 * t6 * t62 * (-t1069 * t1852 + t1075 * t1858 / 0.2D1
     # + t1854)) * t149 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t1852 - (-t1870 + t1107 * t1859) * t1033 + t1100 * t1858) + 
     #0.180D3 * t6 * t62 * t1881) * t147 * t220 / 0.720D3 - (-0.90D2 * t
     #6 * 0.3141592653589793D1 * (-t1125 * t1858 / 0.2D1 + t1124 * t1852
     # - t1854) + 0.180D3 * t6 * t62 * (t1124 * t1858 - t1852) - t6 * t5
     #6 * t1858) * t219 * t149 / 0.720D3
      t1948 = FJET(XB1, XB2, s, -t996, 0.0D0, 0.0D0, -t934, t997, t1947)
      t1950 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1951 = t872 * t1950
      t1954 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1957 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t195, -t85
     #5, x4)
      t1964 = t876 * t1954
      t1987 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t899 * t1951 / 0
     #.2D1 + t902 * t872 * t1954 - t876 * t1957) + 0.180D3 * t6 * t62 * 
     #(t902 * t1951 - t1964) - t883 * t916 * t1951) * t147 * t219 / 0.14
     #40D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t866 * t1951 - t196
     #4) - 0.180D3 * t883 * t884 * t1951) * t147 * t220 / 0.720D3
      t1988 = FJET(XB1, XB2, s, -t856, 0.0D0, t854, 0.0D0, 0.0D0, t1987)
      t1990 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t1992 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t2004 = 0.90D2 * t6 * 0.3141592653589793D1 * (t1666 * t1990 - t167
     #4 * t1665 * t1992) * t929 - 0.180D3 * t63 * t1666 * t1992 * t929
      t2008 = FJET(XB1, XB2, s, -t1636, -t1627, t1626, t1616, -t941, -t2
     #004 * t147 * t220 / 0.720D3)
      t2012 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t2014 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t195, -t855, 
     #x4)
      t2026 = 0.90D2 * t6 * 0.3141592653589793D1 * (t1666 * t2012 - t167
     #4 * t1665 * t2014) * t929 - 0.180D3 * t63 * t1666 * t2014 * t929
      t2030 = FJET(XB1, XB2, s, -t1627, -t1636, t1616, t1626, -t941, -t2
     #026 * t147 * t220 / 0.720D3)
      rrgq2qght8s3e1 = t412 * t411 + t630 * t629 + t848 * t847 + t924 * 
     #t923 + t993 * t992 + t1145 * t1144 + t1243 * t1242 + t1283 * t1282
     # + t1501 * t1500 + t1536 * t1535 + t1576 * t1575 + t1611 * t1610 -
     # t1691 * t1687 * t1694 / 0.720D3 - t1715 * t1711 * t1694 / 0.720D3
     # + t1815 * t1814 + t1850 * t1849 + t1948 * t1947 + t1988 * t1987 -
     # t2008 * t2004 * t1694 / 0.720D3 - t2030 * t2026 * t1694 / 0.720D3

      end function



      doubleprecision function rrgq2qght8s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t12 * t18)
      t22 = t21 ** 2
      t23 = -0.1D1 + x3
      t24 = 0.1D1 / t23
      t25 = t18 * t24
      t28 = log(-0.4D1 * t12 * t25)
      t29 = t28 ** 2
      t31 = cos(t13)
      t32 = x3 * z
      t34 = Sqrt(-t32 * t23)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t31 * t34)
      t41 = t22 / 0.2D1 + t29 * z * t38 / 0.2D1
      t45 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t49 = 0.3141592653589793D1 * lh
      t56 = -t21 - t28 * z * t38
      t58 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t69 = -0.180D3 * t65 + 0.30D2 * t67
      t70 = 0.3141592653589793D1 * t69
      t72 = t6 * t70 * t7
      t75 = 0.1D1 + z * t38
      t78 = 0.1D1 / x3
      t81 = t11 * t15
      t82 = t81 * t17
      t84 = log(0.4D1 * t82)
      t85 = t84 * t3
      t86 = t5 * 0.3141592653589793D1
      t87 = t86 * lh
      t90 = t84 ** 2
      t91 = t90 * t3
      t95 = -0.180D3 * t85 * t87 - 0.45D2 * t91 * t86 + t6 * t70
      t98 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t117 = 0.90D2 * t91 * t87 + t6 * 0.3141592653589793D1 * (-0.60D2 *
     # lh * t67 + 0.2884936567583026D3 + 0.120D3 * t65 * lh) + 0.15D2 * 
     #t90 * t84 * t3 * t86 - t85 * t86 * t69
      t124 = 0.180D3 * t6 * t49 + 0.90D2 * t85 * t86
      t127 = x2 ** 2
      t128 = x3 * t127
      t131 = log(0.4D1 * t128 * t82)
      t133 = 0.1D1 - x2
      t134 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t135 = t128 * t11
      t136 = -t133
      t137 = t18 * t136
      t140 = log(-0.4D1 * t135 * t137)
      t141 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t143 = z * t45
      t146 = log(-0.4D1 * t135 * t25)
      t147 = t146 * z
      t156 = z * t7 * t38
      t157 = t156 - t141 + t7
      t163 = 0.1D1 / x2
      t166 = t11 * t127
      t169 = log(-0.4D1 * t166 * t137)
      t171 = t169 ** 2
      t176 = log(0.4D1 * t166 * t18)
      t178 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t179 = t176 ** 2
      t192 = t7 - t141
      t198 = x1 ** 2
      t199 = x3 * t198
      t202 = log(0.4D1 * t199 * t82)
      t204 = t199 * t15
      t205 = t11 * t17
      t209 = log(-0.4D1 * t204 * t205 * t24)
      t210 = t209 * z
      t224 = 0.1D1 / x1
      t227 = t6 * 0.3141592653589793D1
      t229 = t163 * t224
      t233 = t127 * t198
      t234 = t233 * t15
      t238 = log(-0.4D1 * t234 * t205 * t136)
      t242 = log(0.4D1 * t233 * t82)
      t255 = t198 * t15
      t258 = log(0.4D1 * t255 * t205)
      t260 = t258 ** 2
      t275 = -(-0.90D2 * t6 * 0.3141592653589793D1 * t7 * t41 + (-0.90D2
     # * t6 * 0.3141592653589793D1 * t45 + 0.180D3 * t6 * t49 * t7) * t5
     #6 + (-0.90D2 * t6 * 0.3141592653589793D1 * t58 + 0.180D3 * t6 * t4
     #9 * t45 + t72) * t75) * t78 / 0.2880D4 - t95 * t45 / 0.2880D4 + t6
     # * 0.3141592653589793D1 * t98 / 0.32D2 - t117 * t7 / 0.2880D4 - t1
     #24 * t58 / 0.2880D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t13
     #1 * t7 - t134 + t140 * t141 + t45 + (t143 - t147 * t7) * t38) + 0.
     #180D3 * t6 * t49 * t157) * t78 * t163 / 0.1440D4 - (-0.90D2 * t6 *
     # 0.3141592653589793D1 * (t169 * t134 + t58 - t171 * t141 / 0.2D1 -
     # t176 * t45 - t178 + t179 * t7 / 0.2D1) + 0.180D3 * t6 * t49 * (-t
     #134 + t45 + t169 * t141 - t176 * t7) + t6 * t70 * t192) * t163 / 0
     #.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t202 * t7 + t45
     # + (t143 - t210 * t7) * t38) + 0.180D3 * t6 * t49 * (t7 + t156)) *
     # t78 * t224 / 0.1440D4 + t227 * t157 * t78 * t229 / 0.8D1 - (-0.90
     #D2 * t6 * 0.3141592653589793D1 * (-t134 + t45 + t238 * t141 - t242
     # * t7) + 0.180D3 * t6 * t49 * t192) * t163 * t224 / 0.720D3 + (-0.
     #90D2 * t6 * 0.3141592653589793D1 * (t258 * t45 - t58 - t260 * t7 /
     # 0.2D1) + 0.180D3 * t6 * t49 * (-t45 + t258 * t7) - t72) * t224 / 
     #0.1440D4
      t276 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t275)
      t278 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t283 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t292 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t300 = t6 * t70 * t278
      t308 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t317 = z * t283
      t321 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t322 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t329 = z * t278 * t38
      t330 = t278 - t322 + t329
      t340 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t355 = t278 - t322
      t409 = -(-0.90D2 * t6 * 0.3141592653589793D1 * t278 * t41 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * t283 + 0.180D3 * t6 * t49 * t278)
     # * t56 + (-0.90D2 * t6 * 0.3141592653589793D1 * t292 + 0.180D3 * t
     #6 * t49 * t283 + t300) * t75) * t78 / 0.2880D4 - t95 * t283 / 0.28
     #80D4 + t6 * 0.3141592653589793D1 * t308 / 0.32D2 - t117 * t278 / 0
     #.2880D4 - t124 * t292 / 0.2880D4 - (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (t283 - t131 * t278 + (t317 - t147 * t278) * t38 - t321 + 
     #t140 * t322) + 0.180D3 * t6 * t49 * t330) * t78 * t163 / 0.1440D4 
     #- (-0.90D2 * t6 * 0.3141592653589793D1 * (t179 * t278 / 0.2D1 - t3
     #40 - t171 * t322 / 0.2D1 + t169 * t321 + t292 - t176 * t283) + 0.1
     #80D3 * t6 * t49 * (-t321 + t169 * t322 + t283 - t176 * t278) + t6 
     #* t70 * t355) * t163 / 0.1440D4 - (-0.90D2 * t6 * 0.31415926535897
     #93D1 * (t283 - t202 * t278 + (t317 - t210 * t278) * t38) + 0.180D3
     # * t6 * t49 * (t278 + t329)) * t78 * t224 / 0.1440D4 + t227 * t330
     # * t78 * t229 / 0.8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t
     #321 + t238 * t322 + t283 - t242 * t278) + 0.180D3 * t6 * t49 * t35
     #5) * t163 * t224 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1 
     #* (-t260 * t278 / 0.2D1 - t292 + t258 * t283) + 0.180D3 * t6 * t49
     # * (-t283 + t258 * t278) - t300) * t224 / 0.1440D4
      t410 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t409)
      t412 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t417 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t426 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t434 = t6 * t70 * t412
      t442 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t450 = z * t417
      t454 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t456 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t463 = z * t412 * t38
      t464 = t463 + t412 - t456
      t472 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t489 = t412 - t456
      t543 = -(-0.90D2 * t6 * 0.3141592653589793D1 * t412 * t41 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * t417 + 0.180D3 * t6 * t49 * t412)
     # * t56 + (-0.90D2 * t6 * 0.3141592653589793D1 * t426 + 0.180D3 * t
     #6 * t49 * t417 + t434) * t75) * t78 / 0.2880D4 - t95 * t417 / 0.28
     #80D4 + t6 * 0.3141592653589793D1 * t442 / 0.32D2 - t117 * t412 / 0
     #.2880D4 - t124 * t426 / 0.2880D4 - (-0.90D2 * t6 * 0.3141592653589
     #793D1 * ((t450 - t147 * t412) * t38 + t417 - t454 - t131 * t412 + 
     #t140 * t456) + 0.180D3 * t6 * t49 * t464) * t78 * t163 / 0.1440D4 
     #- (-0.90D2 * t6 * 0.3141592653589793D1 * (-t472 + t169 * t454 + t4
     #26 - t171 * t456 / 0.2D1 + t179 * t412 / 0.2D1 - t176 * t417) + 0.
     #180D3 * t6 * t49 * (-t454 + t169 * t456 + t417 - t176 * t412) + t6
     # * t70 * t489) * t163 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (t417 - t202 * t412 + (t450 - t210 * t412) * t38) + 0.180D
     #3 * t6 * t49 * (t412 + t463)) * t78 * t224 / 0.1440D4 + t227 * t46
     #4 * t78 * t229 / 0.8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-
     #t454 + t238 * t456 + t417 - t242 * t412) + 0.180D3 * t6 * t49 * t4
     #89) * t163 * t224 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1
     # * (-t426 - t260 * t412 / 0.2D1 + t258 * t417) + 0.180D3 * t6 * t4
     #9 * (-t417 + t258 * t412) - t434) * t224 / 0.1440D4
      t544 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t543)
      t546 = x2 * x3
      t547 = 0.1D1 - x3 + t546
      t548 = 0.1D1 / t547
      t549 = t546 * t548
      t550 = t2 * t549
      t551 = t23 * t548
      t552 = t2 * t551
      t555 = t547 ** 2
      t561 = log(0.4D1 * t128 * t81 * t17 * t136 * t23 / t555)
      t562 = t561 * z
      t563 = t136 * t23
      t565 = Sqrt(t32 * t563)
      t569 = 0.1D1 / (-z - x3 + t546 + 0.2D1 * t31 * t565)
      t570 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t551
     #, x4)
      t571 = t569 * t570
      t573 = z * t569
      t574 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t551
     #, x4)
      t580 = lh * z
      t589 = t6 * 0.3141592653589793D1 * z
      t591 = t78 * t163 * t224
      t595 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t562 * t571 - t573
     # * t574) - 0.180D3 * t227 * t580 * t571) * t78 * t163 / 0.1440D4 -
     # t589 * t571 * t591 / 0.8D1
      t596 = FJET(XB1, XB2, s, 0.0D0, t550, 0.0D0, -t552, 0.0D0, t595)
      t599 = t1 * x1
      t600 = x1 * z
      t601 = -z - x1 + t600
      t602 = 0.1D1 / t601
      t604 = t136 * s * t599 * t602
      t605 = -0.1D1 + x1
      t606 = t2 * t605
      t608 = x2 * s * t599
      t609 = s * t16
      t612 = x1 * t605 * t602
      t613 = t609 * t136 * t612
      t614 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1, 
     #x4)
      t619 = 0.1D1 / t9
      t620 = t15 * t619
      t622 = t17 * t602
      t623 = t605 ** 2
      t628 = log(0.4D1 * t233 * t620 * t622 * t623 * t136)
      t630 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1, 
     #x4)
      t642 = t227 * t614 * t78 * t229 / 0.8D1 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t628 * t614 + t630) + 0.180D3 * t6 * t49 * t614) 
     #* t163 * t224 / 0.720D3
      t643 = FJET(XB1, XB2, s, 0.0D0, t604, -t606, t608, -t613, t642)
      t646 = t2 * x1 * t602
      t647 = t609 * t612
      t648 = z * t601
      t649 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t656 = log(0.4D1 * t199 * t620 * t622 * t623 * t24)
      t657 = t656 * z
      t658 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t662 = x3 * x1
      t663 = t662 * z
      t664 = t199 * t9
      t666 = 0.2D1 * t199 * z
      t667 = x1 * t9
      t668 = x3 * t9
      t669 = t668 * x1
      t670 = x3 * t601
      t672 = Sqrt(t670 * t23)
      t677 = 0.1D1 / (-t600 - t663 - t664 + t666 - t199 - t32 + t667 + t
     #669 + 0.2D1 * t31 * t672 * z - t9)
      t681 = t619 * t17 * t602 * t623
      t684 = log(-0.4D1 * t204 * t681)
      t692 = -t658 + t648 * t658 * t677
      t706 = log(-0.4D1 * t234 * t681)
      t719 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t724 = log(-0.4D1 * t255 * t619 * t622 * t623)
      t725 = t724 ** 2
      t743 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-(-t648 * t649 + t
     #657 * t601 * t658) * t677 + t684 * t658 - t649) + 0.180D3 * t6 * t
     #49 * t692) * t78 * t224 / 0.1440D4 + t227 * t692 * t78 * t229 / 0.
     #8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t706 * t658 - t649) 
     #- 0.180D3 * t6 * t49 * t658) * t163 * t224 / 0.720D3 + (-0.90D2 * 
     #t6 * 0.3141592653589793D1 * (t719 + t725 * t658 / 0.2D1 - t724 * t
     #649) + 0.180D3 * t6 * t49 * (-t724 * t658 + t649) + t6 * t70 * t65
     #8) * t224 / 0.1440D4
      t744 = FJET(XB1, XB2, s, 0.0D0, -t606, -t646, 0.0D0, t647, t743)
      t746 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t748 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t760 = t648 * t748 * t677 - t748
      t784 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t802 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-(-t648 * t746 + t
     #657 * t601 * t748) * t677 + t684 * t748 - t746) + 0.180D3 * t6 * t
     #49 * t760) * t78 * t224 / 0.1440D4 + t227 * t760 * t78 * t229 / 0.
     #8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t746 + t706 * t748)
     # - 0.180D3 * t6 * t49 * t748) * t163 * t224 / 0.720D3 + (-0.90D2 *
     # t6 * 0.3141592653589793D1 * (t784 + t725 * t748 / 0.2D1 - t724 * 
     #t746) + 0.180D3 * t6 * t49 * (t746 - t724 * t748) + t6 * t70 * t74
     #8) * t224 / 0.1440D4
      t803 = FJET(XB1, XB2, s, 0.0D0, -t646, -t606, 0.0D0, t647, t802)
      t805 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t551
     #, x4)
      t806 = t569 * t805
      t808 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t551
     #, x4)
      t824 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t562 * t806 - t573
     # * t808) - 0.180D3 * t227 * t580 * t806) * t78 * t163 / 0.1440D4 -
     # t589 * t806 * t591 / 0.8D1
      t825 = FJET(XB1, XB2, s, 0.0D0, -t552, 0.0D0, t550, 0.0D0, t824)
      t827 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t832 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t841 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t849 = t6 * t70 * t827
      t855 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t863 = z * t832
      t868 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t869 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t876 = z * t827 * t38
      t877 = -t869 + t827 + t876
      t885 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, 0.10D
     #1, x4)
      t902 = -t869 + t827
      t958 = -(-0.90D2 * t6 * 0.3141592653589793D1 * t827 * t41 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * t832 + 0.180D3 * t6 * t49 * t827)
     # * t56 + (-0.90D2 * t6 * 0.3141592653589793D1 * t841 + 0.180D3 * t
     #6 * t49 * t832 + t849) * t75) * t78 / 0.2880D4 + t6 * 0.3141592653
     #589793D1 * t855 / 0.32D2 - t124 * t841 / 0.2880D4 - t95 * t832 / 0
     #.2880D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t832 + (t863 - t
     #147 * t827) * t38 - t131 * t827 - t868 + t140 * t869) + 0.180D3 * 
     #t6 * t49 * t877) * t78 * t163 / 0.1440D4 - (-0.90D2 * t6 * 0.31415
     #92653589793D1 * (-t885 + t179 * t827 / 0.2D1 + t169 * t868 + t841 
     #- t171 * t869 / 0.2D1 - t176 * t832) + 0.180D3 * t6 * t49 * (t832 
     #- t868 + t169 * t869 - t176 * t827) + t6 * t70 * t902) * t163 / 0.
     #1440D4 - t117 * t827 / 0.2880D4 - (-0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t202 * t827 + t832 + (t863 - t210 * t827) * t38) + 0.180D
     #3 * t6 * t49 * (t827 + t876)) * t78 * t224 / 0.1440D4 + t227 * t87
     #7 * t78 * t229 / 0.8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-
     #t242 * t827 + t832 + t238 * t869 - t868) + 0.180D3 * t6 * t49 * t9
     #02) * t163 * t224 / 0.720D3 + (-0.90D2 * t6 * 0.3141592653589793D1
     # * (-t260 * t827 / 0.2D1 + t258 * t832 - t841) + 0.180D3 * t6 * t4
     #9 * (-t832 + t258 * t827) - t849) * t224 / 0.1440D4
      t959 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t958)
      t961 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1, 
     #x4)
      t966 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1, 
     #x4)
      t979 = t227 * t961 * t78 * t229 / 0.8D1 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t966 - t628 * t961) + 0.180D3 * t6 * t49 * t961) *
     # t163 * t224 / 0.720D3
      t980 = FJET(XB1, XB2, s, t608, -t606, t604, 0.0D0, -t613, t979)
      t982 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t551
     #, x4)
      t983 = t569 * t982
      t985 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t551
     #, x4)
      t1001 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t562 * t983 - t57
     #3 * t985) - 0.180D3 * t227 * t580 * t983) * t78 * t163 / 0.1440D4 
     #- t589 * t983 * t591 / 0.8D1
      t1002 = FJET(XB1, XB2, s, t550, 0.0D0, -t552, 0.0D0, 0.0D0, t1001)
      t1004 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1,
     # x4)
      t1009 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1,
     # x4)
      t1022 = t227 * t1004 * t78 * t229 / 0.8D1 - (-0.90D2 * t6 * 0.3141
     #592653589793D1 * (t1009 - t628 * t1004) + 0.180D3 * t6 * t49 * t10
     #04) * t163 * t224 / 0.720D3
      t1023 = FJET(XB1, XB2, s, t604, 0.0D0, t608, -t606, -t613, t1022)
      t1028 = t23 * s * t1 * t605 * t548
      t1029 = t2 * x1
      t1031 = Sqrt(-t670 * t563)
      t1032 = t31 * t1031
      t1038 = t1029 * x2 * (-x3 + t546 - z + t32 - x1 + t662 + t600 - t6
     #63 + 0.2D1 * t1032) * t602 * t548
      t1039 = t606 * t549
      t1040 = t128 * t600
      t1041 = t128 * x1
      t1048 = t1029 * (-x2 + t546 - t1040 + t1041 + t128 * z + 0.1D1 - x
     #3 + 0.2D1 * t1032 * x2) * t602 * t548
      t1049 = x2 * x1
      t1051 = x2 * t198
      t1055 = t1049 * z
      t1061 = -t1041 - t1049 * t9 - 0.2D1 * t1051 * z + t1051 * t9 + t10
     #55 - t546 * z + t546 * x1 - t199 * x2 - 0.2D1 * t1032 * z + t600 +
     # t199 + t32 - t667
      t1074 = t1051 + 0.2D1 * t1032 * t1055 + t9 + t663 + t664 - t666 - 
     #t669 + t1040 - 0.2D1 * t546 * t600 + t668 * t1049 + 0.2D1 * t199 *
     # x2 * z - t199 * t9 * x2 - 0.2D1 * t1032 * t1049
      t1076 = 0.1D1 / (t1061 + t1074)
      t1078 = -z - t1049 + t1055
      t1080 = t6 * 0.3141592653589793D1 * t1076 * t1078
      t1081 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, -t551, 
     #x4)
      t1086 = FJET(XB1, XB2, s, t1028, t1038, -t1039, -t1048, -t613, -t1
     #080 * t1081 * t601 * t591 / 0.8D1)
      t1088 = t86 * t1076
      t1095 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, -t551, 
     #x4)
      t1100 = FJET(XB1, XB2, s, t1038, t1028, -t1048, -t1039, -t613, -t1
     #080 * t1095 * t601 * t591 / 0.8D1)
      t1108 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1110 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1122 = -t1110 + t648 * t1110 * t677
      t1148 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1164 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-(-t648 * t1108 +
     # t657 * t601 * t1110) * t677 + t684 * t1110 - t1108) + 0.180D3 * t
     #6 * t49 * t1122) * t78 * t224 / 0.1440D4 + t227 * t1122 * t78 * t2
     #29 / 0.8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t706 * t1110 
     #- t1108) - 0.180D3 * t6 * t49 * t1110) * t163 * t224 / 0.720D3 + (
     #-0.90D2 * t6 * 0.3141592653589793D1 * (t725 * t1110 / 0.2D1 + t114
     #8 - t724 * t1108) + 0.180D3 * t6 * t49 * (-t724 * t1110 + t1108) +
     # t6 * t70 * t1110) * t224 / 0.1440D4
      t1165 = FJET(XB1, XB2, s, -t606, 0.0D0, 0.0D0, -t646, t647, t1164)
      t1167 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1,
     # x4)
      t1173 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, t133, 0.10D1,
     # x4)
      t1185 = t227 * t1167 * t78 * t229 / 0.8D1 - (-0.90D2 * t6 * 0.3141
     #592653589793D1 * (-t628 * t1167 + t1173) + 0.180D3 * t6 * t49 * t1
     #167) * t163 * t224 / 0.720D3
      t1186 = FJET(XB1, XB2, s, -t606, t608, 0.0D0, t604, -t613, t1185)
      t1188 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1190 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1202 = -t1190 + t648 * t1190 * t677
      t1229 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1244 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1188 - (-t648 *
     # t1188 + t657 * t601 * t1190) * t677 + t684 * t1190) + 0.180D3 * t
     #6 * t49 * t1202) * t78 * t224 / 0.1440D4 + t227 * t1202 * t78 * t2
     #29 / 0.8D1 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t706 * t1190 
     #- t1188) - 0.180D3 * t6 * t49 * t1190) * t163 * t224 / 0.720D3 + (
     #-0.90D2 * t6 * 0.3141592653589793D1 * (-t724 * t1188 + t725 * t119
     #0 / 0.2D1 + t1229) + 0.180D3 * t6 * t49 * (-t724 * t1190 + t1188) 
     #+ t6 * t70 * t1190) * t224 / 0.1440D4
      t1245 = FJET(XB1, XB2, s, -t646, 0.0D0, 0.0D0, -t606, t647, t1244)
      t1247 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t55
     #1, x4)
      t1248 = t569 * t1247
      t1250 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t133, -t55
     #1, x4)
      t1266 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t562 * t1248 - t5
     #73 * t1250) - 0.180D3 * t227 * t580 * t1248) * t78 * t163 / 0.1440
     #D4 - t589 * t1248 * t591 / 0.8D1
      t1267 = FJET(XB1, XB2, s, -t552, 0.0D0, t550, 0.0D0, 0.0D0, t1266)
      t1269 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, -t551, 
     #x4)
      t1274 = FJET(XB1, XB2, s, -t1048, -t1039, t1038, t1028, -t613, -t1
     #080 * t1269 * t601 * t591 / 0.8D1)
      t1282 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t133, -t551, 
     #x4)
      t1287 = FJET(XB1, XB2, s, -t1039, -t1048, t1028, t1038, -t613, -t1
     #080 * t1282 * t601 * t591 / 0.8D1)
      rrgq2qght8s3e0 = t276 * t275 + t410 * t409 + t544 * t543 + t596 * 
     #t595 + t643 * t642 + t744 * t743 + t803 * t802 + t825 * t824 + t95
     #9 * t958 + t980 * t979 + t1002 * t1001 + t1023 * t1022 - t1086 * t
     #3 * t1088 * t1078 * t1081 * t601 * t591 / 0.8D1 - t1100 * t3 * t10
     #88 * t1078 * t1095 * t601 * t591 / 0.8D1 + t1165 * t1164 + t1186 *
     # t1185 + t1245 * t1244 + t1267 * t1266 - t1274 * t3 * t1088 * t107
     #8 * t1269 * t601 * t591 / 0.8D1 - t1287 * t3 * t1088 * t1078 * t12
     #82 * t601 * t591 / 0.8D1

      end function



      doubleprecision function rrgq2qght8s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t12 * t18)
      t22 = -0.1D1 + x3
      t27 = log(-0.4D1 * t12 * t18 / t22)
      t29 = cos(t13)
      t30 = x3 * z
      t32 = Sqrt(-t30 * t22)
      t36 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t32)
      t38 = -t21 - t27 * z * t36
      t42 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t46 = 0.3141592653589793D1 * lh
      t49 = 0.180D3 * t6 * t46 * t7
      t52 = 0.1D1 + z * t36
      t55 = 0.1D1 / x3
      t58 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t67 = log(0.4D1 * t11 * t15 * t17)
      t68 = t67 * t3
      t69 = t5 * 0.3141592653589793D1
      t72 = 0.180D3 * t6 * t46 + 0.90D2 * t68 * t69
      t78 = t67 ** 2
      t82 = lh ** 2
      t84 = 0.3141592653589793D1 ** 2
      t89 = -0.180D3 * t68 * t69 * lh - 0.45D2 * t78 * t3 * t69 + t6 * 0
     #.3141592653589793D1 * (-0.180D3 * t82 + 0.30D2 * t84)
      t92 = t6 * 0.3141592653589793D1
      t94 = z * t7 * t36
      t95 = 0.1D1 - x2
      t96 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1,
     # x4)
      t99 = 0.1D1 / x2
      t103 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t104 = x2 ** 2
      t105 = t11 * t104
      t106 = -t95
      t110 = log(-0.4D1 * t105 * t18 * t106)
      t114 = log(0.4D1 * t105 * t18)
      t120 = t7 - t96
      t128 = 0.1D1 / x1
      t132 = x1 ** 2
      t133 = t132 * t15
      t137 = log(0.4D1 * t133 * t11 * t17)
      t151 = -(-0.90D2 * t6 * 0.3141592653589793D1 * t7 * t38 + (-0.90D2
     # * t6 * 0.3141592653589793D1 * t42 + t49) * t52) * t55 / 0.2880D4 
     #+ t6 * 0.3141592653589793D1 * t58 / 0.32D2 - t72 * t42 / 0.2880D4 
     #- t89 * t7 / 0.2880D4 + t92 * (t94 - t96 + t7) * t55 * t99 / 0.16D
     #2 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t103 + t42 + t110 * t
     #96 - t114 * t7) + 0.180D3 * t6 * t46 * t120) * t99 / 0.1440D4 + t9
     #2 * t120 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589793
     #D1 * (-t42 + t137 * t7) - t49) * t128 / 0.1440D4 + t92 * (t7 + t94
     #) * t55 * t128 / 0.16D2
      t152 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t151)
      t154 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t159 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t165 = 0.180D3 * t6 * t46 * t154
      t171 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t179 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t181 = z * t154 * t36
      t187 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t194 = t154 - t179
      t218 = -(-0.90D2 * t6 * 0.3141592653589793D1 * t154 * t38 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * t159 + t165) * t52) * t55 / 0.288
     #0D4 + t6 * 0.3141592653589793D1 * t171 / 0.32D2 - t72 * t159 / 0.2
     #880D4 - t89 * t154 / 0.2880D4 + t92 * (t154 - t179 + t181) * t55 *
     # t99 / 0.16D2 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t187 + t1
     #10 * t179 + t159 - t114 * t154) + 0.180D3 * t6 * t46 * t194) * t99
     # / 0.1440D4 + t92 * t194 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.
     #3141592653589793D1 * (-t159 + t137 * t154) - t165) * t128 / 0.1440
     #D4 + t92 * (t154 + t181) * t55 * t128 / 0.16D2
      t219 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t218)
      t221 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t226 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t232 = 0.180D3 * t6 * t46 * t221
      t238 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t247 = z * t221 * t36
      t248 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t254 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t261 = t221 - t248
      t285 = -(-0.90D2 * t6 * 0.3141592653589793D1 * t221 * t38 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * t226 + t232) * t52) * t55 / 0.288
     #0D4 + t6 * 0.3141592653589793D1 * t238 / 0.32D2 - t72 * t226 / 0.2
     #880D4 - t89 * t221 / 0.2880D4 + t92 * (t247 + t221 - t248) * t55 *
     # t99 / 0.16D2 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t254 + t1
     #10 * t248 + t226 - t114 * t221) + 0.180D3 * t6 * t46 * t261) * t99
     # / 0.1440D4 + t92 * t261 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.
     #3141592653589793D1 * (-t226 + t137 * t221) - t232) * t128 / 0.1440
     #D4 + t92 * (t221 + t247) * t55 * t128 / 0.16D2
      t286 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t285)
      t288 = x2 * x3
      t290 = 0.1D1 / (0.1D1 - x3 + t288)
      t292 = t2 * t288 * t290
      t293 = t22 * t290
      t294 = t2 * t293
      t296 = t6 * 0.3141592653589793D1 * z
      t299 = Sqrt(t30 * t106 * t22)
      t303 = 0.1D1 / (-z - x3 + t288 + 0.2D1 * t29 * t299)
      t304 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t306 = t55 * t99
      t310 = FJET(XB1, XB2, s, 0.0D0, t292, 0.0D0, -t294, 0.0D0, -t296 *
     # t303 * t304 * t306 / 0.16D2)
      t313 = z * t303
      t320 = t1 * x1
      t321 = x1 * z
      t322 = -z - x1 + t321
      t323 = 0.1D1 / t322
      t325 = t106 * s * t320 * t323
      t326 = -0.1D1 + x1
      t327 = t2 * t326
      t329 = x2 * s * t320
      t330 = s * t16
      t333 = x1 * t326 * t323
      t334 = t330 * t106 * t333
      t335 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t340 = FJET(XB1, XB2, s, 0.0D0, t325, -t327, t329, -t334, t92 * t3
     #35 * t99 * t128 / 0.8D1)
      t344 = t99 * t128
      t349 = t2 * x1 * t323
      t350 = t330 * t333
      t351 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t359 = t326 ** 2
      t363 = log(-0.4D1 * t133 / t9 * t17 * t323 * t359)
      t365 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t376 = z * t322
      t379 = x3 * t132
      t388 = Sqrt(x3 * t322 * t22)
      t393 = 0.1D1 / (-t321 - x3 * x1 * z - t379 * t9 + 0.2D1 * t379 * z
     # - t379 - t30 + x1 * t9 + x3 * t9 * x1 + 0.2D1 * t29 * t388 * z - 
     #t9)
      t401 = -t92 * t351 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t363 * t351 + t365) + 0.180D3 * t6 * t46 * t351) 
     #* t128 / 0.1440D4 + t92 * (-t351 + t376 * t351 * t393) * t55 * t12
     #8 / 0.16D2
      t402 = FJET(XB1, XB2, s, 0.0D0, -t327, -t349, 0.0D0, t350, t401)
      t404 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t409 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t428 = -t92 * t404 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t409 - t363 * t404) + 0.180D3 * t6 * t46 * t404) *
     # t128 / 0.1440D4 + t92 * (t376 * t404 * t393 - t404) * t55 * t128 
     #/ 0.16D2
      t429 = FJET(XB1, XB2, s, 0.0D0, -t349, -t327, 0.0D0, t350, t428)
      t431 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t436 = FJET(XB1, XB2, s, 0.0D0, -t294, 0.0D0, t292, 0.0D0, -t296 *
     # t303 * t431 * t306 / 0.16D2)
      t444 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t448 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t455 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t461 = 0.180D3 * t6 * t46 * t448
      t467 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t469 = z * t448 * t36
      t475 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t482 = -t467 + t448
      t508 = t6 * 0.3141592653589793D1 * t444 / 0.32D2 - t89 * t448 / 0.
     #2880D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * t448 * t38 + (-0.9
     #0D2 * t6 * 0.3141592653589793D1 * t455 + t461) * t52) * t55 / 0.28
     #80D4 + t92 * (-t467 + t448 + t469) * t55 * t99 / 0.16D2 - (-0.90D2
     # * t6 * 0.3141592653589793D1 * (t455 - t475 + t110 * t467 - t114 *
     # t448) + 0.180D3 * t6 * t46 * t482) * t99 / 0.1440D4 - t72 * t455 
     #/ 0.2880D4 + t92 * t482 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.3
     #141592653589793D1 * (-t455 + t137 * t448) - t461) * t128 / 0.1440D
     #4 + t92 * (t448 + t469) * t55 * t128 / 0.16D2
      t509 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t508)
      t511 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t516 = FJET(XB1, XB2, s, t329, -t327, t325, 0.0D0, -t334, t92 * t5
     #11 * t99 * t128 / 0.8D1)
      t523 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t528 = FJET(XB1, XB2, s, t292, 0.0D0, -t294, 0.0D0, 0.0D0, -t296 *
     # t303 * t523 * t306 / 0.16D2)
      t536 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t541 = FJET(XB1, XB2, s, t325, 0.0D0, t329, -t327, -t334, t92 * t5
     #36 * t99 * t128 / 0.8D1)
      t548 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t554 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t572 = -t92 * t548 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t363 * t548 + t554) + 0.180D3 * t6 * t46 * t548) 
     #* t128 / 0.1440D4 + t92 * (-t548 + t376 * t548 * t393) * t55 * t12
     #8 / 0.16D2
      t573 = FJET(XB1, XB2, s, -t327, 0.0D0, 0.0D0, -t349, t350, t572)
      t575 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t580 = FJET(XB1, XB2, s, -t327, t329, 0.0D0, t325, -t334, t92 * t5
     #75 * t99 * t128 / 0.8D1)
      t587 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t593 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t611 = -t92 * t587 * t99 * t128 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t363 * t587 + t593) + 0.180D3 * t6 * t46 * t587) 
     #* t128 / 0.1440D4 + t92 * (-t587 + t376 * t587 * t393) * t55 * t12
     #8 / 0.16D2
      t612 = FJET(XB1, XB2, s, -t349, 0.0D0, 0.0D0, -t327, t350, t611)
      t614 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t619 = FJET(XB1, XB2, s, -t294, 0.0D0, t292, 0.0D0, 0.0D0, -t296 *
     # t303 * t614 * t306 / 0.16D2)
      rrgq2qght8s3em1 = t152 * t151 + t219 * t218 + t286 * t285 - t310 *
     # t3 * t69 * t313 * t304 * t55 * t99 / 0.16D2 + t340 * t3 * t5 * 0.
     #3141592653589793D1 * t335 * t344 / 0.8D1 + t402 * t401 + t429 * t4
     #28 - t436 * t3 * t69 * t313 * t431 * t55 * t99 / 0.16D2 + t509 * t
     #508 + t516 * t3 * t5 * 0.3141592653589793D1 * t511 * t344 / 0.8D1 
     #- t528 * t3 * t69 * t313 * t523 * t55 * t99 / 0.16D2 + t541 * t3 *
     # t5 * 0.3141592653589793D1 * t536 * t344 / 0.8D1 + t573 * t572 + t
     #580 * t3 * t5 * 0.3141592653589793D1 * t575 * t344 / 0.8D1 + t612 
     #* t611 - t619 * t3 * t69 * t313 * t614 * t55 * t99 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = t6 * 0.3141592653589793D1
      t8 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t14 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t20 = 0.1D1 + z / (-z - x3 + 0.2D1 * t10 * t14)
      t22 = 0.1D1 / x3
      t26 = 0.1D1 - x2
      t27 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1,
     # x4)
      t30 = 0.1D1 / x2
      t35 = 0.1D1 / x1
      t39 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t46 = z ** 2
      t49 = Sin(t9)
      t50 = t49 ** 2
      t52 = t1 ** 2
      t53 = t52 ** 2
      t56 = log(0.4D1 / t46 / z * t50 * t53)
      t61 = 0.180D3 * t6 * 0.3141592653589793D1 * lh + 0.90D2 * t56 * t3
     # * t5 * 0.3141592653589793D1
      t64 = t7 * t8 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1 * (
     #t8 - t27) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t8 * t35 / 
     #0.16D2 + t6 * 0.3141592653589793D1 * t39 / 0.32D2 - t61 * t8 / 0.2
     #880D4
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t64)
      t67 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t72 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1,
     # x4)
      t82 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t88 = t7 * t67 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1 * 
     #(t67 - t72) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t67 * t35
     # / 0.16D2 + t6 * 0.3141592653589793D1 * t82 / 0.32D2 - t61 * t67 /
     # 0.2880D4
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t88)
      t91 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t96 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1,
     # x4)
      t106 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t112 = t7 * t91 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1 *
     # (t91 - t96) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t91 * t3
     #5 / 0.16D2 + t6 * 0.3141592653589793D1 * t106 / 0.32D2 - t61 * t91
     # / 0.2880D4
      t113 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t112)
      t115 = -0.1D1 + x1
      t116 = t2 * t115
      t119 = 0.1D1 / (-z - x1 + x1 * z)
      t121 = t2 * x1 * t119
      t125 = s * t52 * x1 * t115 * t119
      t126 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t128 = 0.3141592653589793D1 * t126 * t35
      t131 = FJET(XB1, XB2, s, 0.0D0, -t116, -t121, 0.0D0, t125, -t6 * t
     #128 / 0.16D2)
      t136 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t138 = 0.3141592653589793D1 * t136 * t35
      t141 = FJET(XB1, XB2, s, 0.0D0, -t121, -t116, 0.0D0, t125, -t6 * t
     #138 / 0.16D2)
      t146 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t151 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1
     #, x4)
      t161 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t167 = t7 * t146 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1 
     #* (-t151 + t146) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t146
     # * t35 / 0.16D2 + t6 * 0.3141592653589793D1 * t161 / 0.32D2 - t61 
     #* t146 / 0.2880D4
      t168 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t167)
      t170 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t172 = 0.3141592653589793D1 * t170 * t35
      t175 = FJET(XB1, XB2, s, -t116, 0.0D0, 0.0D0, -t121, t125, -t6 * t
     #172 / 0.16D2)
      t180 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t182 = 0.3141592653589793D1 * t180 * t35
      t185 = FJET(XB1, XB2, s, -t121, 0.0D0, 0.0D0, -t116, t125, -t6 * t
     #182 / 0.16D2)
      rrgq2qght8s3em2 = t65 * t64 + t89 * t88 + t113 * t112 - t131 * t3 
     #* t5 * t128 / 0.16D2 - t141 * t3 * t5 * t138 / 0.16D2 + t168 * t16
     #7 - t175 * t3 * t5 * t172 / 0.16D2 - t185 * t3 * t5 * t182 / 0.16D
     #2

      end function



      doubleprecision function rrgq2qght8s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.32D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.32D2)
      t24 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t24 / 0.32D2)
      t32 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t32 / 0.32D2)
      rrgq2qght8s3em3 = t11 * t3 * t13 * t7 / 0.32D2 + t20 * t3 * t13 * 
     #t16 / 0.32D2 + t28 * t3 * t13 * t24 / 0.32D2 + t36 * t3 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrgq2qght8s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      rrgq2qght8s3em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght8s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + x3
      t2 = t1 * s
      t3 = -0.1D1 + z
      t4 = -0.1D1 + x1
      t5 = t3 * t4
      t6 = t2 * t5
      t7 = t3 * x1
      t8 = t2 * t7
      t10 = x3 * s * t5
      t11 = s * t3
      t12 = x3 * x1
      t13 = t11 * t12
      t14 = 0.1D1 / t3
      t15 = s ** 2
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t18 = x1 ** 2
      t19 = x3 * t18
      t20 = t3 ** 2
      t21 = t20 ** 2
      t22 = x4 * 0.3141592653589793D1
      t23 = Sin(t22)
      t24 = t23 ** 2
      t25 = t21 * t24
      t27 = z ** 2
      t28 = 0.1D1 / t27
      t29 = x1 * z
      t30 = -z - x1 + t29
      t31 = 0.1D1 / t30
      t32 = t28 * t31
      t33 = t4 ** 2
      t38 = log(0.4D1 * t19 * t25 * t32 * t33 * t1)
      t39 = t38 ** 2
      t40 = -t1
      t41 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x4)
      t44 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x4)
      t45 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x4)
      t51 = 0.3141592653589793D1 * lh
      t57 = lh ** 2
      t59 = 0.3141592653589793D1 ** 2
      t61 = -0.180D3 * t57 + 0.30D2 * t59
      t62 = 0.3141592653589793D1 * t61
      t66 = 0.1D1 / x3
      t68 = 0.1D1 / x1
      t70 = x2 * x3
      t71 = t18 * t21
      t72 = t70 * t71
      t73 = t24 * t28
      t74 = t33 * t31
      t79 = log(0.4D1 * t72 * t73 * t74 * t1)
      t90 = 0.1D1 / x2
      t91 = t90 * t68
      t94 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t39 * t41 / 0.2D1 
     #- t44 + t38 * t45) + 0.180D3 * t17 * t51 * (-t45 + t38 * t41) - t1
     #7 * t62 * t41) * t66 * t68 / 0.720D3 + (-0.90D2 * t17 * 0.31415926
     #53589793D1 * (-t45 + t79 * t41) - 0.180D3 * t17 * t51 * t41) * t66
     # * t91 / 0.720D3
      t95 = FJET(XB1, XB2, s, t6, -t8, -t10, t13, 0.0D0, t94)
      t97 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t102 = 0.1D1 / t27 / z
      t103 = t102 * t24
      t104 = t103 * t21
      t106 = log(0.4D1 * t104)
      t107 = t106 ** 2
      t108 = t107 * t14
      t109 = t16 * 0.3141592653589793D1
      t110 = t109 * lh
      t117 = -0.60D2 * lh * t59 + 0.2884936567583026D3 + 0.120D3 * t57 *
     # lh
      t118 = 0.3141592653589793D1 * t117
      t121 = t107 * t106 * t14
      t124 = t106 * t14
      t125 = t109 * t61
      t127 = 0.90D2 * t108 * t110 + t17 * t118 + 0.15D2 * t121 * t109 - 
     #t124 * t125
      t128 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t136 = -0.180D3 * t124 * t110 - 0.45D2 * t108 * t109 + t17 * t62
      t137 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t140 = t17 * t51
      t144 = 0.180D3 * t140 + 0.90D2 * t124 * t109
      t145 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t155 = t59 ** 2
      t156 = t57 ** 2
      t163 = t107 ** 2
      t167 = -0.30D2 * t121 * t110 + t108 * t125 / 0.2D1 - t124 * t109 *
     # t117 + t17 * 0.3141592653589793D1 * (-0.5769873135166051D3 * lh -
     # t155 - 0.60D2 * t156 + 0.60D2 * t57 * t59) - 0.15D2 / 0.4D1 * t16
     #3 * t14 * t109
      t168 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t173 = log(0.4D1 * t19 * t104)
      t174 = t173 ** 2
      t193 = t18 * t24
      t197 = log(0.4D1 * t193 * t102 * t21)
      t203 = t197 ** 2
      t206 = t203 * t197
      t214 = t17 * t118 * t168
      t225 = t19 * x2
      t228 = log(0.4D1 * t225 * t104)
      t230 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t231 = -0.1D1 + x2
      t232 = t231 ** 2
      t233 = t102 * t232
      t237 = log(0.4D1 * t225 * t25 * t233)
      t238 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t244 = t238 - t168
      t252 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t253 = x2 * t18
      t254 = t253 * t21
      t258 = log(0.4D1 * t254 * t103 * t232)
      t262 = log(0.4D1 * t253 * t104)
      t263 = t262 ** 2
      t266 = t258 ** 2
      t286 = x3 * t102
      t289 = log(0.4D1 * t286 * t25)
      t295 = t289 ** 2
      t298 = t295 * t289
      t315 = t70 * t102
      t316 = t25 * t232
      t319 = log(0.4D1 * t315 * t316)
      t323 = log(0.4D1 * t70 * t104)
      t325 = t319 ** 2
      t328 = t323 ** 2
      t341 = -t244
      t348 = x2 * t102
      t351 = log(0.4D1 * t348 * t25)
      t355 = log(0.4D1 * t348 * t316)
      t360 = t355 ** 2
      t361 = t360 * t355
      t364 = t351 ** 2
      t368 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t369 = t364 * t351
      t394 = t17 * 0.3141592653589793D1 * t97 / 0.16D2 - t127 * t128 / 0
     #.1440D4 - t136 * t137 / 0.1440D4 - t144 * t145 / 0.1440D4 - t167 *
     # t168 / 0.1440D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t137 
     #- t174 * t168 / 0.2D1 + t173 * t128) + 0.180D3 * t17 * t51 * (-t12
     #8 + t173 * t168) - t17 * t62 * t168) * t66 * t68 / 0.720D3 + (t17 
     #* t62 * (-t128 + t197 * t168) - 0.90D2 * t17 * 0.3141592653589793D
     #1 * (-t145 + t197 * t137 - t203 * t128 / 0.2D1 + t206 * t168 / 0.6
     #D1) - t214 + 0.180D3 * t17 * t51 * (t197 * t128 - t137 - t203 * t1
     #68 / 0.2D1)) * t68 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793
     #D1 * (t228 * t168 + t230 - t237 * t238 - t128) + 0.180D3 * t17 * t
     #51 * t244) * t66 * t91 / 0.720D3 + (-0.90D2 * t17 * 0.314159265358
     #9793D1 * (t252 - t258 * t230 - t263 * t168 / 0.2D1 + t266 * t238 /
     # 0.2D1 + t262 * t128 - t137) + 0.180D3 * t17 * t51 * (t230 - t258 
     #* t238 + t262 * t168 - t128) + t17 * t62 * t244) * t90 * t68 / 0.7
     #20D3 - (t17 * t62 * (t128 - t289 * t168) - 0.90D2 * t17 * 0.314159
     #2653589793D1 * (t145 - t289 * t137 + t295 * t128 / 0.2D1 - t298 * 
     #t168 / 0.6D1) + t214 + 0.180D3 * t17 * t51 * (-t289 * t128 + t137 
     #+ t295 * t168 / 0.2D1)) * t66 / 0.1440D4 - (-0.90D2 * t17 * 0.3141
     #592653589793D1 * (-t252 + t137 + t319 * t230 - t323 * t128 - t325 
     #* t238 / 0.2D1 + t328 * t168 / 0.2D1) + 0.180D3 * t17 * t51 * (-t2
     #30 + t319 * t238 + t128 - t323 * t168) + t17 * t62 * t341) * t66 *
     # t90 / 0.1440D4 - (t17 * t62 * (t128 - t230 - t351 * t168 + t355 *
     # t238) - 0.90D2 * t17 * 0.3141592653589793D1 * (t361 * t238 / 0.6D
     #1 + t364 * t128 / 0.2D1 + t355 * t252 + t145 - t368 - t369 * t168 
     #/ 0.6D1 - t360 * t230 / 0.2D1 - t351 * t137) + t17 * t118 * t341 +
     # 0.180D3 * t17 * t51 * (t364 * t168 / 0.2D1 - t351 * t128 - t252 +
     # t355 * t230 + t137 - t360 * t238 / 0.2D1)) * t90 / 0.1440D4
      t395 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t11, 0.0D0, t394)
      t397 = t11 * x3
      t398 = t11 * t1
      t403 = log(-0.4D1 * t19 * t21 * t103 * t1)
      t404 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40,
     # x4)
      t406 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40,
     # x4)
      t407 = t403 ** 2
      t408 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40,
     # x4)
      t421 = t17 * t62 * t408
      t430 = log(-0.4D1 * t225 * t25 * t102 * t1)
      t443 = t25 * t1
      t446 = log(-0.4D1 * t315 * t443)
      t447 = t446 ** 2
      t466 = log(-0.4D1 * t286 * t443)
      t472 = t466 ** 2
      t473 = t472 * t466
      t476 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40,
     # x4)
      t495 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t403 * t404 + t40
     #6 + t407 * t408 / 0.2D1) + 0.180D3 * t17 * t51 * (-t403 * t408 + t
     #404) + t421) * t66 * t68 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653
     #589793D1 * (-t430 * t408 + t404) + 0.180D3 * t17 * t51 * t408) * t
     #66 * t91 / 0.720D3 - (-0.90D2 * t17 * 0.3141592653589793D1 * (-t44
     #7 * t408 / 0.2D1 - t406 + t446 * t404) + 0.180D3 * t17 * t51 * (-t
     #404 + t446 * t408) - t421) * t66 * t90 / 0.1440D4 - (t17 * t62 * (
     #-t404 + t466 * t408) - 0.90D2 * t17 * 0.3141592653589793D1 * (t466
     # * t406 + t473 * t408 / 0.6D1 - t476 - t472 * t404 / 0.2D1) - t17 
     #* t118 * t408 + 0.180D3 * t17 * t51 * (-t472 * t408 / 0.2D1 - t406
     # + t466 * t404)) * t66 / 0.1440D4
      t496 = FJET(XB1, XB2, s, 0.0D0, t397, 0.0D0, -t398, 0.0D0, t495)
      t499 = t231 * s * t7
      t500 = t11 * t4
      t501 = x2 * x1
      t503 = t11 * t501 * t31
      t508 = s * t20 * x2 * x1 * t4 * t31
      t513 = log(-0.4D1 * t72 * t73 * t74 * t232)
      t514 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t516 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t532 = log(-0.4D1 * t253 * t25 * t32 * t33 * t232)
      t534 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t535 = t532 ** 2
      t553 = (-0.90D2 * t17 * 0.3141592653589793D1 * (t513 * t514 - t516
     #) - 0.180D3 * t17 * t51 * t514) * t66 * t91 / 0.720D3 + (-0.90D2 *
     # t17 * 0.3141592653589793D1 * (t532 * t516 - t534 - t535 * t514 / 
     #0.2D1) + 0.180D3 * t17 * t51 * (-t516 + t532 * t514) - t17 * t62 *
     # t514) * t90 * t68 / 0.720D3
      t554 = FJET(XB1, XB2, s, -t499, -t500, -t503, 0.0D0, t508, t553)
      t556 = t11 * x1
      t557 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t563 = log(-0.4D1 * t19 * t24 * t28 * t21 * t74)
      t564 = t563 ** 2
      t565 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t568 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t580 = t17 * t62 * t565
      t589 = log(-0.4D1 * t193 * t28 * t21 * t31 * t33)
      t594 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t595 = t589 ** 2
      t599 = t595 * t589
      t617 = t73 * t74
      t620 = log(-0.4D1 * t72 * t617)
      t634 = log(-0.4D1 * t254 * t617)
      t635 = t634 ** 2
      t652 = (-0.90D2 * t17 * 0.3141592653589793D1 * (t557 + t564 * t565
     # / 0.2D1 - t563 * t568) + 0.180D3 * t17 * t51 * (-t563 * t565 + t5
     #68) + t580) * t66 * t68 / 0.720D3 + (t17 * t62 * (t568 - t589 * t5
     #65) - 0.90D2 * t17 * 0.3141592653589793D1 * (t594 + t595 * t568 / 
     #0.2D1 - t589 * t557 - t599 * t565 / 0.6D1) + t17 * t118 * t565 + 0
     #.180D3 * t17 * t51 * (t595 * t565 / 0.2D1 + t557 - t589 * t568)) *
     # t68 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t620 * 
     #t565 + t568) + 0.180D3 * t17 * t51 * t565) * t66 * t91 / 0.720D3 +
     # (-0.90D2 * t17 * 0.3141592653589793D1 * (t635 * t565 / 0.2D1 + t5
     #57 - t634 * t568) + 0.180D3 * t17 * t51 * (t568 - t634 * t565) + t
     #580) * t90 * t68 / 0.720D3
      t653 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t500, t556, 0.0D0, t652)
      t656 = -0.1D1 + t70
      t657 = 0.1D1 / t656
      t658 = x3 * t231 * t657
      t659 = t11 * t658
      t660 = t1 * t657
      t661 = t11 * t660
      t662 = t656 ** 2
      t663 = 0.1D1 / t662
      t665 = t663 * t24 * t233
      t670 = log(-0.4D1 * t665 * t71 * t70 * t1)
      t671 = t670 * z
      t672 = cos(t22)
      t673 = x3 * z
      t674 = x2 * t1
      t676 = Sqrt(-t673 * t674)
      t680 = 0.1D1 / (-z - t70 + 0.2D1 * t672 * t676)
      t681 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, x
     #4)
      t682 = t680 * t681
      t684 = z * t680
      t685 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, x
     #4)
      t686 = t684 * t685
      t691 = t17 * 0.3141592653589793D1
      t692 = lh * z
      t700 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, x
     #4)
      t707 = log(-0.4D1 * t665 * t21 * x2 * x3 * t1)
      t708 = t707 ** 2
      t709 = t708 * z
      t712 = t707 * z
      t724 = t61 * z
      t731 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t671 * t682 + t68
     #6) + 0.180D3 * t691 * t692 * t682) * t66 * t91 / 0.720D3 - (-0.90D
     #2 * t17 * 0.3141592653589793D1 * (-t684 * t700 - t709 * t682 / 0.2
     #D1 + t712 * t680 * t685) + 0.180D3 * t17 * t51 * (t712 * t682 - t6
     #86) - t691 * t724 * t682) * t66 * t90 / 0.1440D4
      t732 = FJET(XB1, XB2, s, 0.0D0, t659, 0.0D0, t661, 0.0D0, t731)
      t734 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t735 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t738 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t761 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t770 = t17 * t118 * t735
      t781 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t782 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t789 = t782 - t735
      t797 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t840 = -t789
      t854 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t882 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t913 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t734 - t174 * t73
     #5 / 0.2D1 + t173 * t738) + 0.180D3 * t17 * t51 * (t173 * t735 - t7
     #38) - t17 * t62 * t735) * t66 * t68 / 0.720D3 + (t17 * t62 * (-t73
     #8 + t197 * t735) - 0.90D2 * t17 * 0.3141592653589793D1 * (-t203 * 
     #t738 / 0.2D1 - t761 + t206 * t735 / 0.6D1 + t197 * t734) - t770 + 
     #0.180D3 * t17 * t51 * (-t203 * t735 / 0.2D1 - t734 + t197 * t738))
     # * t68 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t781 -
     # t237 * t782 - t738 + t228 * t735) + 0.180D3 * t17 * t51 * t789) *
     # t66 * t91 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t7
     #97 - t258 * t781 - t734 - t263 * t735 / 0.2D1 + t266 * t782 / 0.2D
     #1 + t262 * t738) + 0.180D3 * t17 * t51 * (-t258 * t782 + t262 * t7
     #35 - t738 + t781) + t17 * t62 * t789) * t90 * t68 / 0.720D3 - t127
     # * t738 / 0.1440D4 - t136 * t734 / 0.1440D4 - (-0.90D2 * t17 * 0.3
     #141592653589793D1 * (-t323 * t738 - t325 * t782 / 0.2D1 - t797 + t
     #319 * t781 + t328 * t735 / 0.2D1 + t734) + 0.180D3 * t17 * t51 * (
     #-t781 + t319 * t782 - t323 * t735 + t738) + t17 * t62 * t840) * t6
     #6 * t90 / 0.1440D4 - (t17 * t62 * (t355 * t782 - t351 * t735 + t73
     #8 - t781) - 0.90D2 * t17 * 0.3141592653589793D1 * (-t369 * t735 / 
     #0.6D1 + t761 - t854 + t361 * t782 / 0.6D1 + t355 * t797 + t364 * t
     #738 / 0.2D1 - t360 * t781 / 0.2D1 - t351 * t734) + t17 * t118 * t8
     #40 + 0.180D3 * t17 * t51 * (t364 * t735 / 0.2D1 + t734 - t351 * t7
     #38 - t797 - t360 * t782 / 0.2D1 + t355 * t781)) * t90 / 0.1440D4 +
     # t17 * 0.3141592653589793D1 * t882 / 0.16D2 - t144 * t761 / 0.1440
     #D4 - (t17 * t62 * (t738 - t289 * t735) - 0.90D2 * t17 * 0.31415926
     #53589793D1 * (-t298 * t735 / 0.6D1 + t761 - t289 * t734 + t295 * t
     #738 / 0.2D1) + t770 + 0.180D3 * t17 * t51 * (t734 + t295 * t735 / 
     #0.2D1 - t289 * t738)) * t66 / 0.1440D4 - t167 * t735 / 0.1440D4
      t914 = FJET(XB1, XB2, s, 0.0D0, t11, 0.0D0, 0.0D0, 0.0D0, t913)
      t916 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, x
     #4)
      t918 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, x
     #4)
      t919 = t680 * t918
      t922 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, x
     #4)
      t930 = t684 * t922
      t953 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (-t684 * t916 - t7
     #09 * t919 / 0.2D1 + t712 * t680 * t922) + 0.180D3 * t17 * t51 * (t
     #712 * t919 - t930) - t691 * t724 * t919) * t66 * t90 / 0.1440D4 + 
     #(-0.90D2 * t17 * 0.3141592653589793D1 * (t930 - t671 * t919) + 0.1
     #80D3 * t691 * t692 * t919) * t66 * t91 / 0.720D3
      t954 = FJET(XB1, XB2, s, t661, 0.0D0, t659, 0.0D0, 0.0D0, t953)
      t956 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t959 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t961 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t972 = t17 * t62 * t956
      t983 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t1028 = (-0.90D2 * t17 * 0.3141592653589793D1 * (t564 * t956 / 0.2
     #D1 - t563 * t959 + t961) + 0.180D3 * t17 * t51 * (-t563 * t956 + t
     #959) + t972) * t66 * t68 / 0.720D3 + (t17 * t62 * (-t589 * t956 + 
     #t959) - 0.90D2 * t17 * 0.3141592653589793D1 * (-t599 * t956 / 0.6D
     #1 - t589 * t961 + t983 + t595 * t959 / 0.2D1) + t17 * t118 * t956 
     #+ 0.180D3 * t17 * t51 * (t961 + t595 * t956 / 0.2D1 - t589 * t959)
     #) * t68 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t959 
     #- t620 * t956) + 0.180D3 * t17 * t51 * t956) * t66 * t91 / 0.720D3
     # + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t634 * t959 + t635 * 
     #t956 / 0.2D1 + t961) + 0.180D3 * t17 * t51 * (t959 - t634 * t956) 
     #+ t972) * t90 * t68 / 0.720D3
      t1029 = FJET(XB1, XB2, s, t556, -t500, 0.0D0, 0.0D0, 0.0D0, t1028)
      t1031 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1032 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1039 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1040 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1073 = t17 * t62 * t1032
      t1106 = -(t17 * t62 * (-t1031 + t466 * t1032) - 0.90D2 * t17 * 0.3
     #141592653589793D1 * (t473 * t1032 / 0.6D1 - t1039 + t466 * t1040 -
     # t472 * t1031 / 0.2D1) - t17 * t118 * t1032 + 0.180D3 * t17 * t51 
     #* (t466 * t1031 - t1040 - t472 * t1032 / 0.2D1)) * t66 / 0.1440D4 
     #- (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1040 + t446 * t1031 -
     # t447 * t1032 / 0.2D1) + 0.180D3 * t17 * t51 * (t446 * t1032 - t10
     #31) - t1073) * t66 * t90 / 0.1440D4 + (-0.90D2 * t17 * 0.314159265
     #3589793D1 * (t1040 + t407 * t1032 / 0.2D1 - t403 * t1031) + 0.180D
     #3 * t17 * t51 * (-t403 * t1032 + t1031) + t1073) * t66 * t68 / 0.7
     #20D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t1031 - t430 * t10
     #32) + 0.180D3 * t17 * t51 * t1032) * t66 * t91 / 0.720D3
      t1107 = FJET(XB1, XB2, s, -t398, 0.0D0, t397, 0.0D0, 0.0D0, t1106)
      t1109 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1110 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1112 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1125 = t17 * t62 * t1112
      t1135 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1181 = (-0.90D2 * t17 * 0.3141592653589793D1 * (t1109 - t563 * t1
     #110 + t564 * t1112 / 0.2D1) + 0.180D3 * t17 * t51 * (-t563 * t1112
     # + t1110) + t1125) * t66 * t68 / 0.720D3 + (t17 * t62 * (-t589 * t
     #1112 + t1110) - 0.90D2 * t17 * 0.3141592653589793D1 * (-t599 * t11
     #12 / 0.6D1 + t1135 - t589 * t1109 + t595 * t1110 / 0.2D1) + t17 * 
     #t118 * t1112 + 0.180D3 * t17 * t51 * (t1109 + t595 * t1112 / 0.2D1
     # - t589 * t1110)) * t68 / 0.720D3 + (-0.90D2 * t17 * 0.31415926535
     #89793D1 * (-t620 * t1112 + t1110) + 0.180D3 * t17 * t51 * t1112) *
     # t66 * t91 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t6
     #35 * t1112 / 0.2D1 - t634 * t1110 + t1109) + 0.180D3 * t17 * t51 *
     # (-t634 * t1112 + t1110) + t1125) * t90 * t68 / 0.720D3
      t1182 = FJET(XB1, XB2, s, -t500, t556, 0.0D0, 0.0D0, 0.0D0, t1181)
      t1184 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1185 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1200 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1216 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1184 + t513 * t
     #1185) - 0.180D3 * t17 * t51 * t1185) * t66 * t91 / 0.720D3 + (-0.9
     #0D2 * t17 * 0.3141592653589793D1 * (t532 * t1184 - t535 * t1185 / 
     #0.2D1 - t1200) + 0.180D3 * t17 * t51 * (-t1184 + t532 * t1185) - t
     #17 * t62 * t1185) * t90 * t68 / 0.720D3
      t1217 = FJET(XB1, XB2, s, -t500, -t499, 0.0D0, -t503, t508, t1216)
      t1219 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1220 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1234 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1251 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1219 + t513 * t
     #1220) - 0.180D3 * t17 * t51 * t1220) * t66 * t91 / 0.720D3 + (-0.9
     #0D2 * t17 * 0.3141592653589793D1 * (-t535 * t1220 / 0.2D1 - t1234 
     #+ t532 * t1219) + 0.180D3 * t17 * t51 * (-t1219 + t532 * t1220) - 
     #t17 * t62 * t1220) * t90 * t68 / 0.720D3
      t1252 = FJET(XB1, XB2, s, -t503, 0.0D0, -t499, -t500, t508, t1251)
      t1254 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1255 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1257 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1286 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1254 + t38 * t1
     #255 - t39 * t1257 / 0.2D1) + 0.180D3 * t17 * t51 * (-t1255 + t38 *
     # t1257) - t17 * t62 * t1257) * t66 * t68 / 0.720D3 + (-0.90D2 * t1
     #7 * 0.3141592653589793D1 * (t79 * t1257 - t1255) - 0.180D3 * t17 *
     # t51 * t1257) * t66 * t91 / 0.720D3
      t1287 = FJET(XB1, XB2, s, t13, -t10, -t8, t6, 0.0D0, t1286)
      t1289 = t95 * t94 + t395 * t394 + t496 * t495 + t554 * t553 + t653
     # * t652 + t732 * t731 + t914 * t913 + t954 * t953 + t1029 * t1028 
     #+ t1107 * t1106 + t1182 * t1181 + t1217 * t1216 + t1252 * t1251 + 
     #t1287 * t1286
      t1290 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, 
     #x4)
      t1293 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, 
     #x4)
      t1295 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, 
     #x4)
      t1296 = t680 * t1295
      t1303 = t684 * t1290
      t1327 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t712 * t680 * t1
     #290 - t684 * t1293 - t709 * t1296 / 0.2D1) + 0.180D3 * t17 * t51 *
     # (-t1303 + t712 * t1296) - t691 * t724 * t1296) * t66 * t90 / 0.14
     #40D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t1303 - t671 * t12
     #96) + 0.180D3 * t691 * t692 * t1296) * t66 * t91 / 0.720D3
      t1328 = FJET(XB1, XB2, s, t659, 0.0D0, t661, 0.0D0, 0.0D0, t1327)
      t1330 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1333 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1335 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1346 = t17 * t62 * t1330
      t1354 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1402 = (-0.90D2 * t17 * 0.3141592653589793D1 * (t564 * t1330 / 0.
     #2D1 - t563 * t1333 + t1335) + 0.180D3 * t17 * t51 * (t1333 - t563 
     #* t1330) + t1346) * t66 * t68 / 0.720D3 + (t17 * t62 * (-t589 * t1
     #330 + t1333) - 0.90D2 * t17 * 0.3141592653589793D1 * (t1354 + t595
     # * t1333 / 0.2D1 - t589 * t1335 - t599 * t1330 / 0.6D1) + t17 * t1
     #18 * t1330 + 0.180D3 * t17 * t51 * (-t589 * t1333 + t1335 + t595 *
     # t1330 / 0.2D1)) * t68 / 0.720D3 + (-0.90D2 * t17 * 0.314159265358
     #9793D1 * (-t620 * t1330 + t1333) + 0.180D3 * t17 * t51 * t1330) * 
     #t66 * t91 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t6
     #34 * t1333 + t1335 + t635 * t1330 / 0.2D1) + 0.180D3 * t17 * t51 *
     # (t1333 - t634 * t1330) + t1346) * t90 * t68 / 0.720D3
      t1403 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t556, -t500, 0.0D0, t1402)
      t1405 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1407 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1413 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1417 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1447 = t17 * t62 * t1405
      t1480 = -(t17 * t62 * (t466 * t1405 - t1407) - 0.90D2 * t17 * 0.31
     #41592653589793D1 * (t473 * t1405 / 0.6D1 + t466 * t1413 - t472 * t
     #1407 / 0.2D1 - t1417) - t17 * t118 * t1405 + 0.180D3 * t17 * t51 *
     # (-t1413 - t472 * t1405 / 0.2D1 + t466 * t1407)) * t66 / 0.1440D4 
     #+ (-0.90D2 * t17 * 0.3141592653589793D1 * (t1413 - t403 * t1407 + 
     #t407 * t1405 / 0.2D1) + 0.180D3 * t17 * t51 * (t1407 - t403 * t140
     #5) + t1447) * t66 * t68 / 0.720D3 + (-0.90D2 * t17 * 0.31415926535
     #89793D1 * (t1407 - t430 * t1405) + 0.180D3 * t17 * t51 * t1405) * 
     #t66 * t91 / 0.720D3 - (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1
     #413 + t446 * t1407 - t447 * t1405 / 0.2D1) + 0.180D3 * t17 * t51 *
     # (-t1407 + t446 * t1405) - t1447) * t66 * t90 / 0.1440D4
      t1481 = FJET(XB1, XB2, s, 0.0D0, -t398, 0.0D0, t397, 0.0D0, t1480)
      t1483 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1484 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1486 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1515 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1483 + t38 * t1
     #484 - t39 * t1486 / 0.2D1) + 0.180D3 * t17 * t51 * (t38 * t1486 - 
     #t1484) - t17 * t62 * t1486) * t66 * t68 / 0.720D3 + (-0.90D2 * t17
     # * 0.3141592653589793D1 * (-t1484 + t79 * t1486) - 0.180D3 * t17 *
     # t51 * t1486) * t66 * t91 / 0.720D3
      t1516 = FJET(XB1, XB2, s, -t10, t13, t6, -t8, 0.0D0, t1515)
      t1518 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1519 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1526 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1528 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t40
     #, x4)
      t1560 = t17 * t62 * t1519
      t1593 = -(t17 * t62 * (-t1518 + t466 * t1519) - 0.90D2 * t17 * 0.3
     #141592653589793D1 * (t473 * t1519 / 0.6D1 + t466 * t1526 - t1528 -
     # t472 * t1518 / 0.2D1) - t17 * t118 * t1519 + 0.180D3 * t17 * t51 
     #* (t466 * t1518 - t1526 - t472 * t1519 / 0.2D1)) * t66 / 0.1440D4 
     #- (-0.90D2 * t17 * 0.3141592653589793D1 * (t446 * t1518 - t447 * t
     #1519 / 0.2D1 - t1526) + 0.180D3 * t17 * t51 * (-t1518 + t446 * t15
     #19) - t1560) * t66 * t90 / 0.1440D4 + (-0.90D2 * t17 * 0.314159265
     #3589793D1 * (-t403 * t1518 + t1526 + t407 * t1519 / 0.2D1) + 0.180
     #D3 * t17 * t51 * (t1518 - t403 * t1519) + t1560) * t66 * t68 / 0.7
     #20D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t430 * t1519 + t1
     #518) + 0.180D3 * t17 * t51 * t1519) * t66 * t91 / 0.720D3
      t1594 = FJET(XB1, XB2, s, t397, 0.0D0, -t398, 0.0D0, 0.0D0, t1593)
      t1596 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1597 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1599 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t40, x
     #4)
      t1628 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1596 + t38 * t1
     #597 - t39 * t1599 / 0.2D1) + 0.180D3 * t17 * t51 * (-t1597 + t38 *
     # t1599) - t17 * t62 * t1599) * t66 * t68 / 0.720D3 + (-0.90D2 * t1
     #7 * 0.3141592653589793D1 * (t79 * t1599 - t1597) - 0.180D3 * t17 *
     # t51 * t1599) * t66 * t91 / 0.720D3
      t1629 = FJET(XB1, XB2, s, -t8, t6, t13, -t10, 0.0D0, t1628)
      t1631 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, 
     #x4)
      t1632 = t680 * t1631
      t1634 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, 
     #x4)
      t1635 = t684 * t1634
      t1649 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t660, 
     #x4)
      t1668 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t671 * t1632 + t
     #1635) + 0.180D3 * t691 * t692 * t1632) * t66 * t91 / 0.720D3 - (-0
     #.90D2 * t17 * 0.3141592653589793D1 * (-t709 * t1632 / 0.2D1 - t684
     # * t1649 + t712 * t680 * t1634) + 0.180D3 * t17 * t51 * (-t1635 + 
     #t712 * t1632) - t691 * t724 * t1632) * t66 * t90 / 0.1440D4
      t1669 = FJET(XB1, XB2, s, 0.0D0, t661, 0.0D0, t659, 0.0D0, t1668)
      t1671 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1672 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1687 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1703 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t1671 + t513 * t
     #1672) - 0.180D3 * t17 * t51 * t1672) * t66 * t91 / 0.720D3 + (-0.9
     #0D2 * t17 * 0.3141592653589793D1 * (t532 * t1671 - t535 * t1672 / 
     #0.2D1 - t1687) + 0.180D3 * t17 * t51 * (-t1671 + t532 * t1672) - t
     #17 * t62 * t1672) * t90 * t68 / 0.720D3
      t1704 = FJET(XB1, XB2, s, 0.0D0, -t503, -t500, -t499, t508, t1703)
      t1706 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1709 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1718 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1721 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1728 = t17 * t118 * t1709
      t1741 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1788 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1789 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1796 = -t1709 + t1789
      t1806 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1843 = -t1796
      t1861 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1885 = -t127 * t1706 / 0.1440D4 - t167 * t1709 / 0.1440D4 - (t17 
     #* t62 * (-t289 * t1709 + t1706) - 0.90D2 * t17 * 0.314159265358979
     #3D1 * (t295 * t1706 / 0.2D1 + t1718 - t298 * t1709 / 0.6D1 - t289 
     #* t1721) + t1728 + 0.180D3 * t17 * t51 * (t1721 - t289 * t1706 + t
     #295 * t1709 / 0.2D1)) * t66 / 0.1440D4 - t144 * t1718 / 0.1440D4 +
     # t17 * 0.3141592653589793D1 * t1741 / 0.16D2 - t136 * t1721 / 0.14
     #40D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t174 * t1709 / 0.
     #2D1 + t173 * t1706 - t1721) + 0.180D3 * t17 * t51 * (t173 * t1709 
     #- t1706) - t17 * t62 * t1709) * t66 * t68 / 0.720D3 + (t17 * t62 *
     # (-t1706 + t197 * t1709) - 0.90D2 * t17 * 0.3141592653589793D1 * (
     #t197 * t1721 - t203 * t1706 / 0.2D1 - t1718 + t206 * t1709 / 0.6D1
     #) - t1728 + 0.180D3 * t17 * t51 * (-t1721 - t203 * t1709 / 0.2D1 +
     # t197 * t1706)) * t68 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589
     #793D1 * (-t1706 + t1788 - t237 * t1789 + t228 * t1709) + 0.180D3 *
     # t17 * t51 * t1796) * t66 * t91 / 0.720D3 + (-0.90D2 * t17 * 0.314
     #1592653589793D1 * (-t1721 + t266 * t1789 / 0.2D1 + t1806 - t258 * 
     #t1788 + t262 * t1706 - t263 * t1709 / 0.2D1) + 0.180D3 * t17 * t51
     # * (t1788 - t1706 - t258 * t1789 + t262 * t1709) + t17 * t62 * t17
     #96) * t90 * t68 / 0.720D3 - (-0.90D2 * t17 * 0.3141592653589793D1 
     #* (t1721 + t328 * t1709 / 0.2D1 - t1806 + t319 * t1788 - t323 * t1
     #706 - t325 * t1789 / 0.2D1) + 0.180D3 * t17 * t51 * (t1706 - t1788
     # - t323 * t1709 + t319 * t1789) + t17 * t62 * t1843) * t66 * t90 /
     # 0.1440D4 - (t17 * t62 * (t355 * t1789 - t1788 + t1706 - t351 * t1
     #709) - 0.90D2 * t17 * 0.3141592653589793D1 * (-t360 * t1788 / 0.2D
     #1 - t351 * t1721 - t369 * t1709 / 0.6D1 + t355 * t1806 + t1718 - t
     #1861 + t361 * t1789 / 0.6D1 + t364 * t1706 / 0.2D1) + t17 * t118 *
     # t1843 + 0.180D3 * t17 * t51 * (t1721 - t1806 + t364 * t1709 / 0.2
     #D1 - t360 * t1789 / 0.2D1 - t351 * t1706 + t355 * t1788)) * t90 / 
     #0.1440D4
      t1886 = FJET(XB1, XB2, s, t11, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1885)
      t1890 = Sqrt(x3 * t30 * t674)
      t1891 = t672 * t1890
      t1896 = x2 ** 2
      t1897 = x3 * t1896
      t1898 = t1897 * t29
      t1899 = t12 * z
      t1900 = t70 * z
      t1902 = t70 * x1
      t1904 = t1897 * x1
      t1906 = 0.2D1 * t1891
      t1907 = -t673 - t12 + 0.2D1 * t1891 * x2 + t70 - 0.2D1 * t70 * t29
     # + t1898 + t1899 + 0.2D1 * t1900 + 0.2D1 * t1902 - t1904 - t1897 *
     # z - x2 - t1906
      t1910 = t556 * t1907 * t31 * t657
      t1911 = t500 * t658
      t1916 = t556 * t231 * (-t70 - z + t673 - x1 + t12 + t29 - t1899 + 
     #t1906) * t31 * t657
      t1918 = t2 * t5 * t657
      t1919 = t501 * z
      t1920 = z + x1 - t29 - t501 + t1919
      t1921 = t30 * t1920
      t1922 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t1933 = log(0.4D1 * t70 * t71 * t24 * t28 * t33 * t31 * t232 * t1 
     #* t663)
      t1934 = t1933 * t30
      t1935 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t1959 = -0.2D1 * t1891 * t29 - t501 * t27 - 0.2D1 * t253 * z + t25
     #3 * t27 - 0.2D1 * t1891 * t501 + x3 * t27 * t501 + 0.2D1 * t19 * x
     #2 * z - t19 * t27 * x2 - t1898 + 0.2D1 * t1891 * z + 0.2D1 * t1891
     # * x1 + t1919
      t1968 = -t1900 - t1902 - t225 + t1904 - 0.2D1 * t29 + 0.2D1 * x1 *
     # t27 + 0.2D1 * t18 * z - t18 * t27 + t253 - t27 + 0.2D1 * t1891 * 
     #t1919 - t18
      t1970 = 0.1D1 / (t1959 + t1968)
      t1978 = 0.90D2 * t17 * 0.3141592653589793D1 * (-t1921 * t1922 + t1
     #934 * t1920 * t1935) * t1970 + 0.180D3 * t140 * t1921 * t1935 * t1
     #970
      t1982 = FJET(XB1, XB2, s, -t1910, -t1911, t1916, -t1918, t508, t19
     #78 * t66 * t91 / 0.720D3)
      t1985 = t66 * t90 * t68
      t1988 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t1990 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t2002 = 0.90D2 * t17 * 0.3141592653589793D1 * (-t1921 * t1988 + t1
     #934 * t1920 * t1990) * t1970 + 0.180D3 * t140 * t1921 * t1990 * t1
     #970
      t2006 = FJET(XB1, XB2, s, -t1911, -t1910, -t1918, t1916, t508, t20
     #02 * t66 * t91 / 0.720D3)
      t2010 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t2012 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t2024 = 0.90D2 * t17 * 0.3141592653589793D1 * (-t1921 * t2010 + t1
     #934 * t1920 * t2012) * t1970 + 0.180D3 * t140 * t1921 * t2012 * t1
     #970
      t2028 = FJET(XB1, XB2, s, -t1918, t1916, -t1911, -t1910, t508, t20
     #24 * t66 * t91 / 0.720D3)
      t2032 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t2034 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t660, x4)
      t2046 = 0.90D2 * t17 * 0.3141592653589793D1 * (-t1921 * t2032 + t1
     #934 * t1920 * t2034) * t1970 + 0.180D3 * t140 * t1921 * t2034 * t1
     #970
      t2050 = FJET(XB1, XB2, s, t1916, -t1918, -t1910, -t1911, t508, t20
     #46 * t66 * t91 / 0.720D3)
      t2054 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2055 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2058 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2060 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2063 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2064 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2076 = -t2055 + t2060
      t2091 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2092 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2119 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2145 = t17 * t118 * t2060
      t2203 = -t2076
      t2233 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (-t2054 - t325 * 
     #t2055 / 0.2D1 - t323 * t2058 + t328 * t2060 / 0.2D1 + t2063 + t319
     # * t2064) + 0.180D3 * t17 * t51 * (-t2064 + t2058 + t319 * t2055 -
     # t323 * t2060) + t17 * t62 * t2076) * t66 * t90 / 0.1440D4 - (t17 
     #* t62 * (t2058 - t2064 - t351 * t2060 + t355 * t2055) - 0.90D2 * t
     #17 * 0.3141592653589793D1 * (t355 * t2054 + t361 * t2055 / 0.6D1 +
     # t2091 - t2092 + t364 * t2058 / 0.2D1 - t360 * t2064 / 0.2D1 - t36
     #9 * t2060 / 0.6D1 - t351 * t2063) + t17 * t118 * t2076 + 0.180D3 *
     # t17 * t51 * (t364 * t2060 / 0.2D1 - t2054 - t351 * t2058 - t360 *
     # t2055 / 0.2D1 + t2063 + t355 * t2064)) * t90 / 0.1440D4 + t17 * 0
     #.3141592653589793D1 * t2119 / 0.16D2 - t127 * t2058 / 0.1440D4 - t
     #136 * t2063 / 0.1440D4 - t144 * t2091 / 0.1440D4 - t167 * t2060 / 
     #0.1440D4 - (t17 * t62 * (t2058 - t289 * t2060) - 0.90D2 * t17 * 0.
     #3141592653589793D1 * (t2091 - t298 * t2060 / 0.6D1 + t295 * t2058 
     #/ 0.2D1 - t289 * t2063) + t2145 + 0.180D3 * t17 * t51 * (t295 * t2
     #060 / 0.2D1 + t2063 - t289 * t2058)) * t66 / 0.1440D4 + (-0.90D2 *
     # t17 * 0.3141592653589793D1 * (t173 * t2058 - t174 * t2060 / 0.2D1
     # - t2063) + 0.180D3 * t17 * t51 * (-t2058 + t173 * t2060) - t17 * 
     #t62 * t2060) * t66 * t68 / 0.720D3 + (t17 * t62 * (-t2058 + t197 *
     # t2060) - 0.90D2 * t17 * 0.3141592653589793D1 * (-t2091 + t206 * t
     #2060 / 0.6D1 - t203 * t2058 / 0.2D1 + t197 * t2063) - t2145 + 0.18
     #0D3 * t17 * t51 * (-t203 * t2060 / 0.2D1 - t2063 + t197 * t2058)) 
     #* t68 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t237 *
     # t2055 - t2058 + t228 * t2060 + t2064) + 0.180D3 * t17 * t51 * t22
     #03) * t66 * t91 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 
     #* (-t258 * t2064 + t262 * t2058 + t2054 - t263 * t2060 / 0.2D1 - t
     #2063 + t266 * t2055 / 0.2D1) + 0.180D3 * t17 * t51 * (t262 * t2060
     # + t2064 - t258 * t2055 - t2058) + t17 * t62 * t2203) * t90 * t68 
     #/ 0.720D3
      t2234 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t11, 0.0D0, 0.0D0, t2233)
      t2236 = t1328 * t1327 + t1403 * t1402 + t1481 * t1480 + t1516 * t1
     #515 + t1594 * t1593 + t1629 * t1628 + t1669 * t1668 + t1704 * t170
     #3 + t1886 * t1885 + t1982 * t1978 * t1985 / 0.720D3 + t2006 * t200
     #2 * t1985 / 0.720D3 + t2028 * t2024 * t1985 / 0.720D3 + t2050 * t2
     #046 * t1985 / 0.720D3 + t2234 * t2233
      rrgq2qght8s4e1 = t1289 + t2236

      end function



      doubleprecision function rrgq2qght8s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + x3
      t2 = t1 * s
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = s * t3
      t10 = x3 * x1
      t11 = t9 * t10
      t13 = x3 * s * t7
      t14 = 0.1D1 / t3
      t15 = s ** 2
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t18 = -t1
      t19 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x4)
      t20 = x1 ** 2
      t21 = x3 * t20
      t22 = t3 ** 2
      t23 = t22 ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t27 = t23 * t26
      t29 = z ** 2
      t30 = 0.1D1 / t29
      t31 = x1 * z
      t32 = -z - x1 + t31
      t33 = 0.1D1 / t32
      t34 = t30 * t33
      t35 = t6 ** 2
      t40 = log(0.4D1 * t21 * t27 * t34 * t35 * t1)
      t41 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x4)
      t47 = 0.3141592653589793D1 * lh
      t52 = 0.1D1 / x3
      t54 = 0.1D1 / x1
      t57 = t17 * 0.3141592653589793D1
      t59 = 0.1D1 / x2
      t60 = t59 * t54
      t64 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t19 + t40 * t41) -
     # 0.180D3 * t17 * t47 * t41) * t52 * t54 / 0.720D3 + t57 * t41 * t5
     #2 * t60 / 0.8D1
      t65 = FJET(XB1, XB2, s, -t5, t8, t11, -t13, 0.0D0, t64)
      t67 = t9 * x1
      t68 = -0.1D1 + x2
      t69 = x2 * x3
      t70 = x3 * z
      t71 = t10 * z
      t72 = cos(t24)
      t74 = x2 * t1
      t76 = Sqrt(x3 * t32 * t74)
      t77 = t72 * t76
      t78 = 0.2D1 * t77
      t81 = -0.1D1 + t69
      t82 = 0.1D1 / t81
      t85 = t67 * t68 * (-t69 - z + t70 - x1 + t10 + t31 - t71 + t78) * 
     #t33 * t82
      t87 = t2 * t7 * t82
      t92 = x2 ** 2
      t93 = x3 * t92
      t94 = t93 * t31
      t95 = t69 * z
      t97 = t69 * x1
      t99 = t93 * x1
      t101 = -t70 - t10 + 0.2D1 * t77 * x2 + t69 - 0.2D1 * t69 * t31 + t
     #94 + t71 + 0.2D1 * t95 + 0.2D1 * t97 - t99 - t93 * z - x2 - t78
      t104 = t67 * t101 * t33 * t82
      t105 = t9 * t6
      t107 = x3 * t68 * t82
      t108 = t105 * t107
      t113 = s * t22 * x2 * x1 * t6 * t33
      t115 = x2 * x1
      t116 = t115 * z
      t117 = z + x1 - t31 - t115 + t116
      t119 = t17 * 0.3141592653589793D1 * t32 * t117
      t120 = t1 * t82
      t121 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t120, x4)
      t125 = x2 * t20
      t142 = -0.2D1 * t77 * t31 - t115 * t29 - 0.2D1 * t125 * z + t125 *
     # t29 - 0.2D1 * t77 * t115 + x3 * t29 * t115 + 0.2D1 * t21 * x2 * z
     # - t21 * t29 * x2 - t94 + 0.2D1 * t77 * z + 0.2D1 * t77 * x1 + t11
     #6
      t152 = -t95 - t97 - t21 * x2 + t99 - 0.2D1 * t31 + 0.2D1 * x1 * t2
     #9 + 0.2D1 * t20 * z - t20 * t29 + t125 - t29 + 0.2D1 * t77 * t116 
     #- t20
      t154 = 0.1D1 / (t142 + t152)
      t157 = t52 * t59 * t54
      t161 = FJET(XB1, XB2, s, t85, -t87, -t104, -t108, t113, -t119 * t1
     #21 * t154 * t157 / 0.8D1)
      t163 = t16 * 0.3141592653589793D1
      t164 = t163 * t32
      t171 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x4
     #)
      t172 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x4
     #)
      t189 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t171 + t40 * t172
     #) - 0.180D3 * t17 * t47 * t172) * t52 * t54 / 0.720D3 + t57 * t172
     # * t52 * t60 / 0.8D1
      t190 = FJET(XB1, XB2, s, t11, -t13, -t5, t8, 0.0D0, t189)
      t192 = t9 * t107
      t193 = t9 * t120
      t194 = t81 ** 2
      t198 = 0.1D1 / t29 / z
      t199 = t68 ** 2
      t207 = log(-0.4D1 / t194 * t26 * t198 * t199 * t23 * x2 * x3 * t1)
      t208 = t207 * z
      t210 = Sqrt(-t70 * t74)
      t214 = 0.1D1 / (-z - t69 + 0.2D1 * t72 * t210)
      t215 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t216 = t214 * t215
      t218 = z * t214
      t219 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t225 = lh * z
      t234 = t17 * 0.3141592653589793D1 * z
      t238 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t208 * t216 - t21
     #8 * t219) - 0.180D3 * t57 * t225 * t216) * t52 * t59 / 0.1440D4 - 
     #t234 * t216 * t157 / 0.8D1
      t239 = FJET(XB1, XB2, s, 0.0D0, t192, 0.0D0, t193, 0.0D0, t238)
      t242 = t9 * t115 * t33
      t244 = t68 * s * t4
      t245 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t250 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t256 = log(-0.4D1 * t125 * t27 * t34 * t35 * t199)
      t269 = t57 * t245 * t52 * t60 / 0.8D1 + (-0.90D2 * t17 * 0.3141592
     #653589793D1 * (-t250 + t256 * t245) - 0.180D3 * t17 * t47 * t245) 
     #* t59 * t54 / 0.720D3
      t270 = FJET(XB1, XB2, s, 0.0D0, -t242, -t105, -t244, t113, t269)
      t272 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t274 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t275 = t214 * t274
      t291 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (-t218 * t272 + t2
     #08 * t275) - 0.180D3 * t57 * t225 * t275) * t52 * t59 / 0.1440D4 -
     # t234 * t275 * t157 / 0.8D1
      t292 = FJET(XB1, XB2, s, 0.0D0, t193, 0.0D0, t192, 0.0D0, t291)
      t294 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t120, x4)
      t299 = FJET(XB1, XB2, s, -t104, -t108, t85, -t87, t113, -t119 * t2
     #94 * t154 * t157 / 0.8D1)
      t307 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t312 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t325 = t57 * t307 * t52 * t60 / 0.8D1 + (-0.90D2 * t17 * 0.3141592
     #653589793D1 * (-t312 + t256 * t307) - 0.180D3 * t17 * t47 * t307) 
     #* t59 * t54 / 0.720D3
      t326 = FJET(XB1, XB2, s, -t242, 0.0D0, -t244, -t105, t113, t325)
      t328 = t9 * x3
      t329 = t9 * t1
      t330 = x3 * t198
      t331 = t27 * t1
      t334 = log(-0.4D1 * t330 * t331)
      t335 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18,
     # x4)
      t337 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18,
     # x4)
      t338 = t334 ** 2
      t339 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18,
     # x4)
      t351 = lh ** 2
      t353 = 0.3141592653589793D1 ** 2
      t355 = -0.180D3 * t351 + 0.30D2 * t353
      t356 = 0.3141592653589793D1 * t355
      t362 = t69 * t198
      t365 = log(-0.4D1 * t362 * t331)
      t373 = 0.180D3 * t17 * t47 * t339
      t379 = t26 * t198
      t383 = log(-0.4D1 * t21 * t23 * t379 * t1)
      t397 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t334 * t335 - t33
     #7 - t338 * t339 / 0.2D1) + 0.180D3 * t17 * t47 * (-t335 + t334 * t
     #339) - t17 * t356 * t339) * t52 / 0.1440D4 - (-0.90D2 * t17 * 0.31
     #41592653589793D1 * (-t335 + t365 * t339) - t373) * t52 * t59 / 0.1
     #440D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t335 - t383 * t33
     #9) + t373) * t52 * t54 / 0.720D3 - t57 * t339 * t52 * t60 / 0.8D1
      t398 = FJET(XB1, XB2, s, t328, 0.0D0, -t329, 0.0D0, 0.0D0, t397)
      t402 = t33 * t35
      t406 = log(-0.4D1 * t21 * t26 * t30 * t23 * t402)
      t407 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t409 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t416 = 0.180D3 * t17 * t47 * t407
      t425 = t125 * t23
      t430 = log(-0.4D1 * t425 * t26 * t30 * t402)
      t440 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t441 = t20 * t26
      t447 = log(-0.4D1 * t441 * t30 * t23 * t33 * t35)
      t448 = t447 ** 2
      t466 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t406 * t407 + t40
     #9) + t416) * t52 * t54 / 0.720D3 - t57 * t407 * t52 * t60 / 0.8D1 
     #+ (-0.90D2 * t17 * 0.3141592653589793D1 * (-t430 * t407 + t409) + 
     #t416) * t59 * t54 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D
     #1 * (t440 + t448 * t407 / 0.2D1 - t447 * t409) + 0.180D3 * t17 * t
     #47 * (-t447 * t407 + t409) + t17 * t356 * t407) * t54 / 0.720D3
      t467 = FJET(XB1, XB2, s, -t105, t67, 0.0D0, 0.0D0, 0.0D0, t466)
      t469 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x4
     #)
      t470 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x4
     #)
      t487 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t469 + t40 * t470
     #) - 0.180D3 * t17 * t47 * t470) * t52 * t54 / 0.720D3 + t57 * t470
     # * t52 * t60 / 0.8D1
      t488 = FJET(XB1, XB2, s, t8, -t5, -t13, t11, 0.0D0, t487)
      t490 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t495 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t508 = t57 * t490 * t52 * t60 / 0.8D1 + (-0.90D2 * t17 * 0.3141592
     #653589793D1 * (-t495 + t256 * t490) - 0.180D3 * t17 * t47 * t490) 
     #* t59 * t54 / 0.720D3
      t509 = FJET(XB1, XB2, s, -t105, -t244, 0.0D0, -t242, t113, t508)
      t513 = log(0.4D1 * t330 * t27)
      t514 = t513 ** 2
      t515 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t518 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t519 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t531 = t17 * t356 * t515
      t535 = t379 * t23
      t537 = log(0.4D1 * t535)
      t538 = t537 * t14
      t539 = t163 * lh
      t542 = t537 ** 2
      t543 = t542 * t14
      t547 = -0.180D3 * t538 * t539 - 0.45D2 * t543 * t163 + t17 * t356
      t550 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t569 = 0.90D2 * t543 * t539 + t17 * 0.3141592653589793D1 * (-0.60D
     #2 * lh * t353 + 0.2884936567583026D3 + 0.120D3 * t351 * lh) + 0.15
     #D2 * t542 * t537 * t14 * t163 - t538 * t163 * t355
      t576 = 0.180D3 * t17 * t47 + 0.90D2 * t538 * t163
      t579 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t580 = t27 * t199
      t583 = log(0.4D1 * t362 * t580)
      t584 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t588 = log(0.4D1 * t69 * t535)
      t594 = -t584 + t515
      t602 = x2 * t198
      t605 = log(0.4D1 * t602 * t27)
      t606 = t605 ** 2
      t609 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t613 = log(0.4D1 * t602 * t580)
      t614 = t613 ** 2
      t635 = log(0.4D1 * t21 * t535)
      t648 = -t594
      t655 = log(0.4D1 * t125 * t535)
      t660 = log(0.4D1 * t425 * t379 * t199)
      t676 = log(0.4D1 * t441 * t198 * t23)
      t677 = t676 ** 2
      t693 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t514 * t515 / 0.2
     #D1 + t518 - t513 * t519) + 0.180D3 * t17 * t47 * (t519 - t513 * t5
     #15) + t531) * t52 / 0.1440D4 - t547 * t519 / 0.1440D4 + t17 * 0.31
     #41592653589793D1 * t550 / 0.16D2 - t569 * t515 / 0.1440D4 - t576 *
     # t518 / 0.1440D4 - (-0.90D2 * t17 * 0.3141592653589793D1 * (-t579 
     #+ t519 + t583 * t584 - t588 * t515) + 0.180D3 * t17 * t47 * t594) 
     #* t52 * t59 / 0.1440D4 - (-0.90D2 * t17 * 0.3141592653589793D1 * (
     #t606 * t515 / 0.2D1 - t609 - t605 * t519 - t614 * t584 / 0.2D1 + t
     #518 + t613 * t579) + 0.180D3 * t17 * t47 * (t519 - t579 - t605 * t
     #515 + t613 * t584) + t17 * t356 * t594) * t59 / 0.1440D4 + (-0.90D
     #2 * t17 * 0.3141592653589793D1 * (-t519 + t635 * t515) - 0.180D3 *
     # t17 * t47 * t515) * t52 * t54 / 0.720D3 - t57 * t648 * t52 * t60 
     #/ 0.8D1 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t655 * t515 + t
     #579 - t660 * t584 - t519) + 0.180D3 * t17 * t47 * t648) * t59 * t5
     #4 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t677 * t51
     #5 / 0.2D1 - t518 + t676 * t519) + 0.180D3 * t17 * t47 * (-t519 + t
     #676 * t515) - t531) * t54 / 0.720D3
      t694 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t9, 0.0D0, 0.0D0, t693)
      t696 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t120, x4)
      t701 = FJET(XB1, XB2, s, -t108, -t104, -t87, t85, t113, -t119 * t6
     #96 * t154 * t157 / 0.8D1)
      t709 = t65 * t64 - t161 * t14 * t164 * t117 * t121 * t154 * t157 /
     # 0.8D1 + t190 * t189 + t239 * t238 + t270 * t269 + t292 * t291 - t
     #299 * t14 * t164 * t117 * t294 * t154 * t157 / 0.8D1 + t326 * t325
     # + t398 * t397 + t467 * t466 + t488 * t487 + t509 * t508 + t694 * 
     #t693 - t701 * t14 * t164 * t117 * t696 * t154 * t157 / 0.8D1
      t710 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t711 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t719 = 0.180D3 * t17 * t47 * t711
      t738 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t755 = (-0.90D2 * t17 * 0.3141592653589793D1 * (t710 - t406 * t711
     #) + t719) * t52 * t54 / 0.720D3 - t57 * t711 * t52 * t60 / 0.8D1 +
     # (-0.90D2 * t17 * 0.3141592653589793D1 * (t710 - t430 * t711) + t7
     #19) * t59 * t54 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 
     #* (-t447 * t710 + t738 + t448 * t711 / 0.2D1) + 0.180D3 * t17 * t4
     #7 * (-t447 * t711 + t710) + t17 * t356 * t711) * t54 / 0.720D3
      t756 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t67, -t105, 0.0D0, t755)
      t758 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t760 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t767 = 0.180D3 * t17 * t47 * t758
      t787 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t803 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t406 * t758 + t76
     #0) + t767) * t52 * t54 / 0.720D3 - t57 * t758 * t52 * t60 / 0.8D1 
     #+ (-0.90D2 * t17 * 0.3141592653589793D1 * (t760 - t430 * t758) + t
     #767) * t59 * t54 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1
     # * (t448 * t758 / 0.2D1 + t787 - t447 * t760) + 0.180D3 * t17 * t4
     #7 * (t760 - t447 * t758) + t17 * t356 * t758) * t54 / 0.720D3
      t804 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t105, t67, 0.0D0, t803)
      t806 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t807 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t810 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t822 = t17 * t356 * t807
      t828 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t836 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t837 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t844 = t807 - t837
      t855 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t886 = -t844
      t919 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t806 + t514 * t80
     #7 / 0.2D1 - t513 * t810) + 0.180D3 * t17 * t47 * (t810 - t513 * t8
     #07) + t822) * t52 / 0.1440D4 - t547 * t810 / 0.1440D4 + t17 * 0.31
     #41592653589793D1 * t828 / 0.16D2 - t569 * t807 / 0.1440D4 - t576 *
     # t806 / 0.1440D4 - (-0.90D2 * t17 * 0.3141592653589793D1 * (-t836 
     #+ t583 * t837 - t588 * t807 + t810) + 0.180D3 * t17 * t47 * t844) 
     #* t52 * t59 / 0.1440D4 - (-0.90D2 * t17 * 0.3141592653589793D1 * (
     #t606 * t807 / 0.2D1 + t806 - t605 * t810 - t855 - t614 * t837 / 0.
     #2D1 + t613 * t836) + 0.180D3 * t17 * t47 * (t613 * t837 - t605 * t
     #807 + t810 - t836) + t17 * t356 * t844) * t59 / 0.1440D4 + (-0.90D
     #2 * t17 * 0.3141592653589793D1 * (t635 * t807 - t810) - 0.180D3 * 
     #t17 * t47 * t807) * t52 * t54 / 0.720D3 - t57 * t886 * t52 * t60 /
     # 0.8D1 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t660 * t837 + t
     #655 * t807 - t810 + t836) + 0.180D3 * t17 * t47 * t886) * t59 * t5
     #4 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t677 * t80
     #7 / 0.2D1 - t806 + t676 * t810) + 0.180D3 * t17 * t47 * (-t810 + t
     #676 * t807) - t822) * t54 / 0.720D3
      t920 = FJET(XB1, XB2, s, 0.0D0, t9, 0.0D0, 0.0D0, 0.0D0, t919)
      t922 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t924 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t925 = t214 * t924
      t941 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (-t218 * t922 + t2
     #08 * t925) - 0.180D3 * t57 * t225 * t925) * t52 * t59 / 0.1440D4 -
     # t234 * t925 * t157 / 0.8D1
      t942 = FJET(XB1, XB2, s, t192, 0.0D0, t193, 0.0D0, 0.0D0, t941)
      t944 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18,
     # x4)
      t945 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18,
     # x4)
      t948 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18,
     # x4)
      t971 = 0.180D3 * t17 * t47 * t945
      t989 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (-t944 - t338 * t9
     #45 / 0.2D1 + t334 * t948) + 0.180D3 * t17 * t47 * (t334 * t945 - t
     #948) - t17 * t356 * t945) * t52 / 0.1440D4 - (-0.90D2 * t17 * 0.31
     #41592653589793D1 * (-t948 + t365 * t945) - t971) * t52 * t59 / 0.1
     #440D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t948 - t383 * t94
     #5) + t971) * t52 * t54 / 0.720D3 - t57 * t945 * t52 * t60 / 0.8D1
      t990 = FJET(XB1, XB2, s, 0.0D0, -t329, 0.0D0, t328, 0.0D0, t989)
      t992 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t993 = t214 * t992
      t995 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t120, x
     #4)
      t1011 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t208 * t993 - t2
     #18 * t995) - 0.180D3 * t57 * t225 * t993) * t52 * t59 / 0.1440D4 -
     # t234 * t993 * t157 / 0.8D1
      t1012 = FJET(XB1, XB2, s, t193, 0.0D0, t192, 0.0D0, 0.0D0, t1011)
      t1014 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1019 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1032 = t57 * t1014 * t52 * t60 / 0.8D1 + (-0.90D2 * t17 * 0.31415
     #92653589793D1 * (-t1019 + t256 * t1014) - 0.180D3 * t17 * t47 * t1
     #014) * t59 * t54 / 0.720D3
      t1033 = FJET(XB1, XB2, s, -t244, -t105, -t242, 0.0D0, t113, t1032)
      t1035 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1037 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1044 = 0.180D3 * t17 * t47 * t1035
      t1062 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1080 = (-0.90D2 * t17 * 0.3141592653589793D1 * (-t406 * t1035 + t
     #1037) + t1044) * t52 * t54 / 0.720D3 - t57 * t1035 * t52 * t60 / 0
     #.8D1 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t1037 - t430 * t10
     #35) + t1044) * t59 * t54 / 0.720D3 + (-0.90D2 * t17 * 0.3141592653
     #589793D1 * (t1062 + t448 * t1035 / 0.2D1 - t447 * t1037) + 0.180D3
     # * t17 * t47 * (-t447 * t1035 + t1037) + t17 * t356 * t1035) * t54
     # / 0.720D3
      t1081 = FJET(XB1, XB2, s, t67, -t105, 0.0D0, 0.0D0, 0.0D0, t1080)
      t1083 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x
     #4)
      t1085 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t18, x
     #4)
      t1101 = (-0.90D2 * t17 * 0.3141592653589793D1 * (t40 * t1083 - t10
     #85) - 0.180D3 * t17 * t47 * t1083) * t52 * t54 / 0.720D3 + t57 * t
     #1083 * t52 * t60 / 0.8D1
      t1102 = FJET(XB1, XB2, s, -t13, t11, t8, -t5, 0.0D0, t1101)
      t1104 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #, x4)
      t1106 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #, x4)
      t1107 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #, x4)
      t1131 = 0.180D3 * t17 * t47 * t1107
      t1149 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t334 * t1104 - t
     #1106 - t338 * t1107 / 0.2D1) + 0.180D3 * t17 * t47 * (-t1104 + t33
     #4 * t1107) - t17 * t356 * t1107) * t52 / 0.1440D4 - (-0.90D2 * t17
     # * 0.3141592653589793D1 * (t365 * t1107 - t1104) - t1131) * t52 * 
     #t59 / 0.1440D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t383 * 
     #t1107 + t1104) + t1131) * t52 * t54 / 0.720D3 - t57 * t1107 * t52 
     #* t60 / 0.8D1
      t1150 = FJET(XB1, XB2, s, -t329, 0.0D0, t328, 0.0D0, 0.0D0, t1149)
      t1152 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1154 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1155 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1168 = t17 * t356 * t1155
      t1174 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1182 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1183 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1190 = -t1183 + t1155
      t1201 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1232 = -t1190
      t1265 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (-t513 * t1152 + 
     #t1154 + t514 * t1155 / 0.2D1) + 0.180D3 * t17 * t47 * (t1152 - t51
     #3 * t1155) + t1168) * t52 / 0.1440D4 - t547 * t1152 / 0.1440D4 + t
     #17 * 0.3141592653589793D1 * t1174 / 0.16D2 - t569 * t1155 / 0.1440
     #D4 - t576 * t1154 / 0.1440D4 - (-0.90D2 * t17 * 0.3141592653589793
     #D1 * (-t1182 + t583 * t1183 + t1152 - t588 * t1155) + 0.180D3 * t1
     #7 * t47 * t1190) * t52 * t59 / 0.1440D4 - (-0.90D2 * t17 * 0.31415
     #92653589793D1 * (t606 * t1155 / 0.2D1 - t605 * t1152 - t1201 + t61
     #3 * t1182 + t1154 - t614 * t1183 / 0.2D1) + 0.180D3 * t17 * t47 * 
     #(t1152 - t1182 - t605 * t1155 + t613 * t1183) + t17 * t356 * t1190
     #) * t59 / 0.1440D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t11
     #52 + t635 * t1155) - 0.180D3 * t17 * t47 * t1155) * t52 * t54 / 0.
     #720D3 - t57 * t1232 * t52 * t60 / 0.8D1 + (-0.90D2 * t17 * 0.31415
     #92653589793D1 * (t1182 - t660 * t1183 + t655 * t1155 - t1152) + 0.
     #180D3 * t17 * t47 * t1232) * t59 * t54 / 0.720D3 + (-0.90D2 * t17 
     #* 0.3141592653589793D1 * (t676 * t1152 - t1154 - t677 * t1155 / 0.
     #2D1) + 0.180D3 * t17 * t47 * (-t1152 + t676 * t1155) - t1168) * t5
     #4 / 0.720D3
      t1266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t9, 0.0D0, t1265)
      t1268 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #, x4)
      t1271 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #, x4)
      t1272 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #, x4)
      t1295 = 0.180D3 * t17 * t47 * t1268
      t1313 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (-t338 * t1268 / 
     #0.2D1 - t1271 + t334 * t1272) + 0.180D3 * t17 * t47 * (-t1272 + t3
     #34 * t1268) - t17 * t356 * t1268) * t52 / 0.1440D4 - (-0.90D2 * t1
     #7 * 0.3141592653589793D1 * (-t1272 + t365 * t1268) - t1295) * t52 
     #* t59 / 0.1440D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (-t383 
     #* t1268 + t1272) + t1295) * t52 * t54 / 0.720D3 - t57 * t1268 * t5
     #2 * t60 / 0.8D1
      t1314 = FJET(XB1, XB2, s, 0.0D0, t328, 0.0D0, -t329, 0.0D0, t1313)
      t1316 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1317 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1319 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1332 = t17 * t356 * t1319
      t1338 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1344 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1346 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1352 = -t1346 + t1319
      t1360 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1396 = -t1352
      t1429 = -(-0.90D2 * t17 * 0.3141592653589793D1 * (t1316 - t513 * t
     #1317 + t514 * t1319 / 0.2D1) + 0.180D3 * t17 * t47 * (-t513 * t131
     #9 + t1317) + t1332) * t52 / 0.1440D4 - t576 * t1316 / 0.1440D4 + t
     #17 * 0.3141592653589793D1 * t1338 / 0.16D2 - t569 * t1319 / 0.1440
     #D4 - (-0.90D2 * t17 * 0.3141592653589793D1 * (t1317 - t1344 - t588
     # * t1319 + t583 * t1346) + 0.180D3 * t17 * t47 * t1352) * t52 * t5
     #9 / 0.1440D4 - (-0.90D2 * t17 * 0.3141592653589793D1 * (t1316 - t1
     #360 + t606 * t1319 / 0.2D1 - t614 * t1346 / 0.2D1 - t605 * t1317 +
     # t613 * t1344) + 0.180D3 * t17 * t47 * (t613 * t1346 - t1344 + t13
     #17 - t605 * t1319) + t17 * t356 * t1352) * t59 / 0.1440D4 - t547 *
     # t1317 / 0.1440D4 + (-0.90D2 * t17 * 0.3141592653589793D1 * (t635 
     #* t1319 - t1317) - 0.180D3 * t17 * t47 * t1319) * t52 * t54 / 0.72
     #0D3 - t57 * t1396 * t52 * t60 / 0.8D1 + (-0.90D2 * t17 * 0.3141592
     #653589793D1 * (t1344 - t1317 - t660 * t1346 + t655 * t1319) + 0.18
     #0D3 * t17 * t47 * t1396) * t59 * t54 / 0.720D3 + (-0.90D2 * t17 * 
     #0.3141592653589793D1 * (-t1316 - t677 * t1319 / 0.2D1 + t676 * t13
     #17) + 0.180D3 * t17 * t47 * (-t1317 + t676 * t1319) - t1332) * t54
     # / 0.720D3
      t1430 = FJET(XB1, XB2, s, t9, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1429)
      t1432 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t120, x4)
      t1437 = FJET(XB1, XB2, s, -t87, t85, -t108, -t104, t113, -t119 * t
     #1432 * t154 * t157 / 0.8D1)
      t1445 = t756 * t755 + t804 * t803 + t920 * t919 + t942 * t941 + t9
     #90 * t989 + t1012 * t1011 + t1033 * t1032 + t1081 * t1080 + t1102 
     #* t1101 + t1150 * t1149 + t1266 * t1265 + t1314 * t1313 + t1430 * 
     #t1429 - t1437 * t14 * t164 * t117 * t1432 * t154 * t157 / 0.8D1
      rrgq2qght8s4e0 = t709 + t1445

      end function



      doubleprecision function rrgq2qght8s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t27 = 0.3141592653589793D1 * lh
      t30 = 0.180D3 * t6 * t27 * t21
      t32 = 0.1D1 / x3
      t35 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t44 = log(0.4D1 * t10 * t14 * t16)
      t45 = t44 * t3
      t46 = t5 * 0.3141592653589793D1
      t49 = 0.180D3 * t6 * t27 + 0.90D2 * t45 * t46
      t55 = t44 ** 2
      t59 = lh ** 2
      t61 = 0.3141592653589793D1 ** 2
      t66 = -0.180D3 * t45 * t46 * lh - 0.45D2 * t55 * t3 * t46 + t6 * 0
     #.3141592653589793D1 * (-0.180D3 * t59 + 0.30D2 * t61)
      t69 = t6 * 0.3141592653589793D1
      t70 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t71 = -t70 + t21
      t73 = 0.1D1 / x2
      t77 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t78 = x2 * t10
      t81 = log(0.4D1 * t78 * t17)
      t83 = -0.1D1 + x2
      t84 = t83 ** 2
      t88 = log(0.4D1 * t78 * t17 * t84)
      t102 = 0.1D1 / x1
      t106 = x1 ** 2
      t107 = t106 * t14
      t111 = log(0.4D1 * t107 * t10 * t16)
      t124 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t20 * t21) + 
     #t30) * t32 / 0.1440D4 + t6 * 0.3141592653589793D1 * t35 / 0.16D2 -
     # t49 * t7 / 0.1440D4 - t66 * t21 / 0.1440D4 + t69 * t71 * t32 * t7
     #3 / 0.16D2 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t77 - t8
     #1 * t21 + t88 * t70) + 0.180D3 * t6 * t27 * t71) * t73 / 0.1440D4 
     #+ t69 * t71 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 * 0.3141592653589
     #793D1 * (-t7 + t111 * t21) - t30) * t102 / 0.720D3 + t69 * t21 * t
     #32 * t102 / 0.8D1
      t125 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t124)
      t127 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t128 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t136 = 0.180D3 * t6 * t27 * t128
      t140 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t148 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t149 = -t148 + t128
      t154 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t184 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t127 - t20 * t128)
     # + t136) * t32 / 0.1440D4 + t6 * 0.3141592653589793D1 * t140 / 0.1
     #6D2 - t49 * t127 / 0.1440D4 - t66 * t128 / 0.1440D4 + t69 * t149 *
     # t32 * t73 / 0.16D2 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t127
     # - t154 - t81 * t128 + t88 * t148) + 0.180D3 * t6 * t27 * t149) * 
     #t73 / 0.1440D4 + t69 * t149 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 *
     # 0.3141592653589793D1 * (-t127 + t111 * t128) - t136) * t102 / 0.7
     #20D3 + t69 * t128 * t32 * t102 / 0.8D1
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t184)
      t187 = t2 * x1
      t188 = -0.1D1 + x1
      t189 = t2 * t188
      t190 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t199 = 0.1D1 / (-z - x1 + x1 * z)
      t201 = t188 ** 2
      t205 = log(-0.4D1 * t107 / t8 * t16 * t199 * t201)
      t207 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t222 = -t69 * t190 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t205 * t190 + t207) + 0.180D3 * t6 * t27 * t190) 
     #* t102 / 0.720D3 - t69 * t190 * t32 * t102 / 0.8D1
      t223 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t187, -t189, 0.0D0, t222)
      t225 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t230 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t246 = -t69 * t225 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t230 - t205 * t225) + 0.180D3 * t6 * t27 * t225) *
     # t102 / 0.720D3 - t69 * t225 * t32 * t102 / 0.8D1
      t247 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t189, t187, 0.0D0, t246)
      t249 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t250 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t258 = 0.180D3 * t6 * t27 * t250
      t262 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t270 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t271 = t250 - t270
      t278 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t306 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t249 - t20 * t250)
     # + t258) * t32 / 0.1440D4 + t6 * 0.3141592653589793D1 * t262 / 0.1
     #6D2 - t49 * t249 / 0.1440D4 - t66 * t250 / 0.1440D4 + t69 * t271 *
     # t32 * t73 / 0.16D2 - (-0.90D2 * t6 * 0.3141592653589793D1 * (t88 
     #* t270 - t81 * t250 + t249 - t278) + 0.180D3 * t6 * t27 * t271) * 
     #t73 / 0.1440D4 + t69 * t271 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 *
     # 0.3141592653589793D1 * (-t249 + t111 * t250) - t258) * t102 / 0.7
     #20D3 + t69 * t250 * t32 * t102 / 0.8D1
      t307 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t306)
      t309 = t2 * x3
      t310 = -0.1D1 + x3
      t311 = t2 * t310
      t312 = -t310
      t313 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t317 = log(-0.4D1 * t11 * t17 * t310)
      t318 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t330 = t318 * t32
      t337 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t313 + t317 * t31
     #8) - 0.180D3 * t6 * t27 * t318) * t32 / 0.1440D4 - t69 * t330 * t7
     #3 / 0.16D2 - t69 * t330 * t102 / 0.8D1
      t338 = FJET(XB1, XB2, s, 0.0D0, t309, 0.0D0, -t311, 0.0D0, t337)
      t340 = x2 * x3
      t342 = 0.1D1 / (-0.1D1 + t340)
      t343 = t310 * t342
      t344 = t2 * t343
      t347 = t2 * x3 * t83 * t342
      t349 = t6 * 0.3141592653589793D1 * z
      t350 = cos(t12)
      t354 = Sqrt(-x3 * z * x2 * t310)
      t358 = 0.1D1 / (-z - t340 + 0.2D1 * t350 * t354)
      t359 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t343, x
     #4)
      t361 = t32 * t73
      t365 = FJET(XB1, XB2, s, 0.0D0, t344, 0.0D0, t347, 0.0D0, -t349 * 
     #t358 * t359 * t361 / 0.16D2)
      t368 = z * t358
      t374 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t343, x
     #4)
      t379 = FJET(XB1, XB2, s, 0.0D0, t347, 0.0D0, t344, 0.0D0, -t349 * 
     #t358 * t374 * t361 / 0.16D2)
      t387 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t389 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t400 = t387 * t32
      t407 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (t317 * t387 - t389
     #) - 0.180D3 * t6 * t27 * t387) * t32 / 0.1440D4 - t69 * t400 * t10
     #2 / 0.8D1 - t69 * t400 * t73 / 0.16D2
      t408 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t309, 0.0D0, t407)
      t412 = t2 * x1 * x2 * t199
      t414 = t1 * x1
      t415 = t83 * s * t414
      t420 = s * t15 * x2 * x1 * t188 * t199
      t421 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t426 = FJET(XB1, XB2, s, 0.0D0, -t412, -t189, -t415, t420, t69 * t
     #421 * t73 * t102 / 0.8D1)
      t430 = t73 * t102
      t434 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t435 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t436 = -t434 + t435
      t441 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t449 = 0.180D3 * t6 * t27 * t434
      t467 = -t436
      t473 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t485 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t491 = -t69 * t436 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t441 + t111 * t434) - t449) * t102 / 0.720D3 + t6
     #9 * t434 * t32 * t102 / 0.8D1 - t49 * t441 / 0.1440D4 - (-0.90D2 *
     # t6 * 0.3141592653589793D1 * (-t20 * t434 + t441) + t449) * t32 / 
     #0.1440D4 + t69 * t467 * t32 * t73 / 0.16D2 - (-0.90D2 * t6 * 0.314
     #1592653589793D1 * (t88 * t435 - t473 + t441 - t81 * t434) + 0.180D
     #3 * t6 * t27 * t467) * t73 / 0.1440D4 + t6 * 0.3141592653589793D1 
     #* t485 / 0.16D2 - t66 * t434 / 0.1440D4
      t492 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t491)
      t494 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t500 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t515 = -t69 * t494 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t205 * t494 + t500) + 0.180D3 * t6 * t27 * t494) 
     #* t102 / 0.720D3 - t69 * t494 * t32 * t102 / 0.8D1
      t516 = FJET(XB1, XB2, s, t187, -t189, 0.0D0, 0.0D0, 0.0D0, t515)
      t518 = t125 * t124 + t185 * t184 + t223 * t222 + t247 * t246 + t30
     #7 * t306 + t338 * t337 - t365 * t3 * t46 * t368 * t359 * t32 * t73
     # / 0.16D2 - t379 * t3 * t46 * t368 * t374 * t32 * t73 / 0.16D2 + t
     #408 * t407 + t426 * t3 * t5 * 0.3141592653589793D1 * t421 * t430 /
     # 0.8D1 + t492 * t491 + t516 * t515
      t519 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t520 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t532 = t520 * t32
      t539 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t519 + t317 * t52
     #0) - 0.180D3 * t6 * t27 * t520) * t32 / 0.1440D4 - t69 * t532 * t7
     #3 / 0.16D2 - t69 * t532 * t102 / 0.8D1
      t540 = FJET(XB1, XB2, s, t309, 0.0D0, -t311, 0.0D0, 0.0D0, t539)
      t543 = t2 * x1 * x3
      t545 = t1 * t188
      t546 = x3 * s * t545
      t547 = t310 * s
      t548 = t547 * t414
      t549 = t547 * t545
      t550 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t312, x
     #4)
      t555 = FJET(XB1, XB2, s, t543, -t546, -t548, t549, 0.0D0, t69 * t5
     #50 * t32 * t102 / 0.8D1)
      t559 = t32 * t102
      t563 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t343, x
     #4)
      t568 = FJET(XB1, XB2, s, t344, 0.0D0, t347, 0.0D0, 0.0D0, -t349 * 
     #t358 * t563 * t361 / 0.16D2)
      t576 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t312, x
     #4)
      t581 = FJET(XB1, XB2, s, t549, -t548, -t546, t543, 0.0D0, t69 * t5
     #76 * t32 * t102 / 0.8D1)
      t588 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t343, x
     #4)
      t593 = FJET(XB1, XB2, s, t347, 0.0D0, t344, 0.0D0, 0.0D0, -t349 * 
     #t358 * t588 * t361 / 0.16D2)
      t601 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t607 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t622 = -t69 * t601 * t73 * t102 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t205 * t601 + t607) + 0.180D3 * t6 * t27 * t601) 
     #* t102 / 0.720D3 - t69 * t601 * t32 * t102 / 0.8D1
      t623 = FJET(XB1, XB2, s, -t189, t187, 0.0D0, 0.0D0, 0.0D0, t622)
      t625 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t630 = FJET(XB1, XB2, s, -t189, -t415, 0.0D0, -t412, t420, t69 * t
     #625 * t73 * t102 / 0.8D1)
      t637 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t638 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t312
     #, x4)
      t650 = t638 * t32
      t657 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t637 + t317 * t63
     #8) - 0.180D3 * t6 * t27 * t638) * t32 / 0.1440D4 - t69 * t650 * t7
     #3 / 0.16D2 - t69 * t650 * t102 / 0.8D1
      t658 = FJET(XB1, XB2, s, -t311, 0.0D0, t309, 0.0D0, 0.0D0, t657)
      t660 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t312, x
     #4)
      t665 = FJET(XB1, XB2, s, -t546, t543, t549, -t548, 0.0D0, t69 * t6
     #60 * t32 * t102 / 0.8D1)
      t672 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t677 = FJET(XB1, XB2, s, -t415, -t189, -t412, 0.0D0, t420, t69 * t
     #672 * t73 * t102 / 0.8D1)
      t684 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t312, x
     #4)
      t689 = FJET(XB1, XB2, s, -t548, t549, t543, -t546, 0.0D0, t69 * t6
     #84 * t32 * t102 / 0.8D1)
      t696 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t701 = FJET(XB1, XB2, s, -t412, 0.0D0, -t415, -t189, t420, t69 * t
     #696 * t73 * t102 / 0.8D1)
      t708 = t540 * t539 + t555 * t3 * t5 * 0.3141592653589793D1 * t550 
     #* t559 / 0.8D1 - t568 * t3 * t46 * t368 * t563 * t32 * t73 / 0.16D
     #2 + t581 * t3 * t5 * 0.3141592653589793D1 * t576 * t559 / 0.8D1 - 
     #t593 * t3 * t46 * t368 * t588 * t32 * t73 / 0.16D2 + t623 * t622 +
     # t630 * t3 * t5 * 0.3141592653589793D1 * t625 * t430 / 0.8D1 + t65
     #8 * t657 + t665 * t3 * t5 * 0.3141592653589793D1 * t660 * t559 / 0
     #.8D1 + t677 * t3 * t5 * 0.3141592653589793D1 * t672 * t430 / 0.8D1
     # + t689 * t3 * t5 * 0.3141592653589793D1 * t684 * t559 / 0.8D1 + t
     #701 * t3 * t5 * 0.3141592653589793D1 * t696 * t430 / 0.8D1
      rrgq2qght8s4em1 = t518 + t708

      end function



      doubleprecision function rrgq2qght8s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t8 = 0.3141592653589793D1 * t7
      t9 = 0.1D1 / x3
      t13 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t16 = 0.1D1 / x2
      t20 = 0.1D1 / x1
      t24 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t31 = z ** 2
      t35 = Sin(x4 * 0.3141592653589793D1)
      t36 = t35 ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 / t31 / z * t36 * t39)
      t47 = 0.180D3 * t6 * 0.3141592653589793D1 * lh + 0.90D2 * t42 * t3
     # * t5 * 0.3141592653589793D1
      t50 = t6 * t8 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (-t13 + 
     #t7) * t16 / 0.16D2 + t6 * t8 * t20 / 0.8D1 + t6 * 0.31415926535897
     #93D1 * t24 / 0.16D2 - t47 * t7 / 0.1440D4
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t54 = 0.3141592653589793D1 * t53
      t58 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t67 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t73 = t6 * t54 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (-t58 +
     # t53) * t16 / 0.16D2 + t6 * t54 * t20 / 0.8D1 + t6 * 0.31415926535
     #89793D1 * t67 / 0.16D2 - t47 * t53 / 0.1440D4
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t73)
      t76 = t2 * x1
      t78 = t2 * (-0.1D1 + x1)
      t79 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t81 = 0.3141592653589793D1 * t79 * t20
      t84 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t76, -t78, 0.0D0, -t6 * t81 
     #/ 0.8D1)
      t89 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t91 = 0.3141592653589793D1 * t89 * t20
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t78, t76, 0.0D0, -t6 * t91 
     #/ 0.8D1)
      t99 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t100 = 0.3141592653589793D1 * t99
      t104 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t113 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t119 = t6 * t100 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (t99 
     #- t104) * t16 / 0.16D2 + t6 * t100 * t20 / 0.8D1 + t6 * 0.31415926
     #53589793D1 * t113 / 0.16D2 - t47 * t99 / 0.1440D4
      t120 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t119)
      t122 = t2 * x3
      t123 = -0.1D1 + x3
      t124 = t2 * t123
      t125 = -t123
      t126 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t125
     #, x4)
      t128 = 0.3141592653589793D1 * t126 * t9
      t131 = FJET(XB1, XB2, s, 0.0D0, t122, 0.0D0, -t124, 0.0D0, -t6 * t
     #128 / 0.16D2)
      t136 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t125
     #, x4)
      t138 = 0.3141592653589793D1 * t136 * t9
      t141 = FJET(XB1, XB2, s, 0.0D0, -t124, 0.0D0, t122, 0.0D0, -t6 * t
     #138 / 0.16D2)
      t146 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t150 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t153 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t159 = 0.3141592653589793D1 * t150
      t166 = t6 * 0.3141592653589793D1 * t146 / 0.16D2 - t47 * t150 / 0.
     #1440D4 + t6 * 0.3141592653589793D1 * (-t153 + t150) * t16 / 0.16D2
     # + t6 * t159 * t9 / 0.16D2 + t6 * t159 * t20 / 0.8D1
      t167 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t166)
      t169 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t171 = 0.3141592653589793D1 * t169 * t20
      t174 = FJET(XB1, XB2, s, t76, -t78, 0.0D0, 0.0D0, 0.0D0, -t6 * t17
     #1 / 0.8D1)
      t179 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t125
     #, x4)
      t181 = 0.3141592653589793D1 * t179 * t9
      t184 = FJET(XB1, XB2, s, t122, 0.0D0, -t124, 0.0D0, 0.0D0, -t6 * t
     #181 / 0.16D2)
      t189 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t125
     #, x4)
      t191 = 0.3141592653589793D1 * t189 * t9
      t194 = FJET(XB1, XB2, s, -t124, 0.0D0, t122, 0.0D0, 0.0D0, -t6 * t
     #191 / 0.16D2)
      t199 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t201 = 0.3141592653589793D1 * t199 * t20
      t204 = FJET(XB1, XB2, s, -t78, t76, 0.0D0, 0.0D0, 0.0D0, -t6 * t20
     #1 / 0.8D1)
      rrgq2qght8s4em2 = t51 * t50 + t74 * t73 - t84 * t3 * t5 * t81 / 0.
     #8D1 - t94 * t3 * t5 * t91 / 0.8D1 + t120 * t119 - t131 * t3 * t5 *
     # t128 / 0.16D2 - t141 * t3 * t5 * t138 / 0.16D2 + t167 * t166 - t1
     #74 * t3 * t5 * t171 / 0.8D1 - t184 * t3 * t5 * t181 / 0.16D2 - t19
     #4 * t3 * t5 * t191 / 0.16D2 - t204 * t3 * t5 * t201 / 0.8D1

      end function



      doubleprecision function rrgq2qght8s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.16D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.16D2)
      t24 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t24 / 0.16D2)
      t32 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t32 / 0.16D2)
      rrgq2qght8s4em3 = t11 * t3 * t13 * t7 / 0.16D2 + t20 * t3 * t13 * 
     #t16 / 0.16D2 + t28 * t3 * t13 * t24 / 0.16D2 + t36 * t3 * t13 * t3
     #2 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh83J7
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      rrgq2qght8s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh81J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 / t4 * t20
      t26 = s - t2 * t21 - t2 * (0.1D1 - x1) * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t32 = t29 * t30 * t1
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t39 = t20 ** 2
      t46 = t10 * t7 * t4 + x2 * x3 + t19
      t50 = t46 ** 2
      rrgq2qgh81J1 = -wd * (0.8D1 * t32 * t38 * t39 * t20 + 0.8D1 * t32 
     #* t38 * t39 * t46 + 0.8D1 * t32 * t38 * t20 * t50 - 0.8D1 * t29 * 
     #t30 * t33 / t35 * t20 * t46 + 0.8D1 * t29 * t1 * t21) / t26 / s / 
     #z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh81J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t39 = t20 ** 2
      t42 = t32 * t38 * t39 * t20
      t46 = t10 * t7 * t4 + x2 * x3 + t19
      t49 = t32 * t38 * t39 * t46
      t50 = t46 ** 2
      t53 = t32 * t38 * t20 * t50
      t54 = t29 * t30
      t56 = t33 / t35
      t59 = t54 * t56 * t20 * t46
      t61 = t29 * t1 * t21
      t69 = t23 ** 2
      rrgq2qgh81J2 = -(wd * (0.8D1 * t42 + 0.8D1 * t49 + 0.8D1 * t53 - 0
     #.8D1 * t59 + 0.8D1 * t61) + wd * (-0.16D2 * t53 - 0.8D1 * t61 - 0.
     #16D2 * t49 - 0.8D1 * t42 - 0.4D1 * t29 * t31 * t69 * t10 * x1 * t5
     # * t46 * x3 + 0.16D2 * t59 + 0.16D2 * t54 * t56 * t39)) / t26 / s 
     #/ z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh81J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t39 = t20 ** 2
      t42 = t32 * t38 * t39 * t20
      t46 = t10 * t7 * t4 + x2 * x3 + t19
      t49 = t32 * t38 * t39 * t46
      t50 = t46 ** 2
      t53 = t32 * t38 * t20 * t50
      t54 = t29 * t30
      t55 = 0.1D1 / t35
      t56 = t33 * t55
      t59 = t54 * t56 * t20 * t46
      t61 = t29 * t1 * t21
      t69 = t23 ** 2
      t71 = t29 * t31 * t69
      t72 = t10 * x1
      t73 = t5 * t46
      t77 = 0.4D1 * t71 * t72 * t73 * x3
      t92 = t55 * t50
      t96 = t10 ** 2
      t101 = t27 ** 2
      t104 = t23 * t10
      rrgq2qgh81J3 = -(wd * (0.8D1 * t42 + 0.8D1 * t49 + 0.8D1 * t53 - 0
     #.8D1 * t59 + 0.8D1 * t61) + wd * (-0.16D2 * t53 - 0.8D1 * t61 - 0.
     #16D2 * t49 - 0.8D1 * t42 - t77 + 0.16D2 * t59 + 0.16D2 * t54 * t56
     # * t39) + wd * (-0.4D1 * t29 * t30 * t23 * t72 * t73 + 0.8D1 * t29
     # * t31 * t23 * t10 * t33 * t92 - t77 + 0.4D1 * t71 * t96 * x1 * t7
     #3 - 0.4D1 * t101 * t31 * t33 * t92 * t104 + 0.4D1 * t101 * t30 * x
     #1 * t73 * t104)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh81J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t39 = t20 ** 2
      t42 = t32 * t38 * t39 * t20
      t46 = t10 * t7 * t4 + x2 * x3 + t19
      t49 = t32 * t38 * t39 * t46
      t50 = t46 ** 2
      t53 = t32 * t38 * t20 * t50
      t54 = t29 * t30
      t55 = 0.1D1 / t35
      t56 = t33 * t55
      t59 = t54 * t56 * t20 * t46
      t61 = t29 * t1 * t21
      t69 = t23 ** 2
      t71 = t29 * t31 * t69
      t72 = t10 * x1
      t73 = t5 * t46
      t77 = 0.4D1 * t71 * t72 * t73 * x3
      t92 = t55 * t50
      t96 = t10 ** 2
      t101 = t27 ** 2
      t104 = t23 * t10
      rrgq2qgh81J4 = -(wd * (0.8D1 * t42 + 0.8D1 * t49 + 0.8D1 * t53 - 0
     #.8D1 * t59 + 0.8D1 * t61) + wd * (-0.16D2 * t53 - 0.8D1 * t61 - 0.
     #16D2 * t49 - 0.8D1 * t42 - t77 + 0.16D2 * t59 + 0.16D2 * t54 * t56
     # * t39) + wd * (-0.4D1 * t29 * t30 * t23 * t72 * t73 + 0.8D1 * t29
     # * t31 * t23 * t10 * t33 * t92 - t77 + 0.4D1 * t71 * t96 * x1 * t7
     #3 - 0.4D1 * t101 * t31 * t33 * t92 * t104 + 0.4D1 * t101 * t30 * x
     #1 * t73 * t104)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh81J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t39 = t20 ** 2
      t42 = t32 * t38 * t39 * t20
      t46 = t10 * t7 * t4 + x2 * x3 + t19
      t49 = t32 * t38 * t39 * t46
      t50 = t46 ** 2
      t53 = t32 * t38 * t20 * t50
      t54 = t29 * t30
      t55 = 0.1D1 / t35
      t56 = t33 * t55
      t59 = t54 * t56 * t20 * t46
      t61 = t29 * t1 * t21
      t69 = t23 ** 2
      t71 = t29 * t31 * t69
      t72 = t10 * x1
      t73 = t5 * t46
      t77 = 0.4D1 * t71 * t72 * t73 * x3
      t92 = t55 * t50
      t96 = t10 ** 2
      t101 = t27 ** 2
      t104 = t23 * t10
      rrgq2qgh81J5 = -(wd * (0.8D1 * t42 + 0.8D1 * t49 + 0.8D1 * t53 - 0
     #.8D1 * t59 + 0.8D1 * t61) + wd * (-0.16D2 * t53 - 0.8D1 * t61 - 0.
     #16D2 * t49 - 0.8D1 * t42 - t77 + 0.16D2 * t59 + 0.16D2 * t54 * t56
     # * t39) + wd * (-0.4D1 * t29 * t30 * t23 * t72 * t73 + 0.8D1 * t29
     # * t31 * t23 * t10 * t33 * t92 - t77 + 0.4D1 * t71 * t96 * x1 * t7
     #3 - 0.4D1 * t101 * t31 * t33 * t92 * t104 + 0.4D1 * t101 * t30 * x
     #1 * t73 * t104)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh81J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t42 = t10 * t7 * t4 + x2 * x3 + t19
      t43 = t42 ** 2
      t51 = t20 ** 2
      t60 = t23 ** 2
      t62 = t29 * t31 * t60
      t63 = t10 * x1
      t64 = t5 * t42
      t68 = 0.4D1 * t62 * t63 * t64 * x3
      t69 = t29 * t30
      t70 = 0.1D1 / t35
      t71 = t33 * t70
      t89 = t70 * t43
      t93 = t10 ** 2
      t98 = t27 ** 2
      t101 = t23 * t10
      rrgq2qgh81J6 = -(wd * (-0.16D2 * t32 * t38 * t20 * t43 - 0.8D1 * t
     #29 * t1 * t21 - 0.16D2 * t32 * t38 * t51 * t42 - 0.8D1 * t32 * t38
     # * t51 * t20 - t68 + 0.16D2 * t69 * t71 * t20 * t42 + 0.16D2 * t69
     # * t71 * t51) + wd * (-0.4D1 * t29 * t30 * t23 * t63 * t64 + 0.8D1
     # * t29 * t31 * t23 * t10 * t33 * t89 - t68 + 0.4D1 * t62 * t93 * x
     #1 * t64 - 0.4D1 * t98 * t31 * t33 * t89 * t101 + 0.4D1 * t98 * t30
     # * x1 * t64 * t101)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh81J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t26 = s - t2 * x1 * t5 * (t8 * t4 + x2 * t10 - t19) - t2 * t23 * x
     #3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t33 = t10 * x1
      t37 = t10 * t7 * t4 + x2 * x3 + t19
      t38 = t5 * t37
      t42 = t30 * t1
      t45 = x1 ** 2
      t47 = t4 ** 2
      t49 = t37 ** 2
      t50 = 0.1D1 / t47 * t49
      t54 = t23 ** 2
      t56 = t29 * t42 * t54
      t61 = t10 ** 2
      t66 = t27 ** 2
      t69 = t23 * t10
      rrgq2qgh81J7 = -wd * (-0.4D1 * t29 * t30 * t23 * t33 * t38 + 0.8D1
     # * t29 * t42 * t23 * t10 * t45 * t50 - 0.4D1 * t56 * t33 * t38 * x
     #3 + 0.4D1 * t56 * t61 * x1 * t38 - 0.4D1 * t66 * t42 * t45 * t50 *
     # t69 + 0.4D1 * t66 * t30 * x1 * t38 * t69) / t26 / s / z / 0.31415
     #92653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh82J1
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
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t2 * t5 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t29 = t28 ** 2
      t34 = t18 * t15 * t11 + x2 * x3 + t27
      t35 = t34 ** 2
      t39 = t2 * t4
      t40 = 0.1D1 / t12
      t41 = t7 * t40
      t42 = t41 * t29
      t45 = s * t3
      t47 = x1 / t11
      t48 = t47 * t34
      t53 = s - t45 * t48 - t45 * (0.1D1 - x1) * t18
      t54 = s * t1
      t55 = t53 * t54
      t56 = t4 * t3
      t57 = t55 * t56
      t58 = t7 * x1
      t60 = 0.1D1 / t12 / t11
      t61 = t58 * t60
      t62 = t29 * t28
      t63 = t61 * t62
      t70 = t55 * t3
      t73 = t55 * t4
      t78 = t2 * t56
      t79 = t78 * t58
      t95 = t47 * t28
      t102 = t35 * t34
      t122 = -0.18D2 * t9 * t14 * t29 * t35 - 0.18D2 * t39 * t42 + 0.18D
     #2 * t57 * t63 + 0.54D2 * t57 * t61 * t29 * t34 - 0.36D2 * t70 * t4
     #8 + 0.36D2 * t73 * t41 * t28 * t34 - 0.27D2 * t79 * t60 * t28 * t3
     #5 + 0.63D2 * t57 * t61 * t28 * t35 - 0.27D2 * t39 * t7 * t40 * t28
     # * t34 + 0.18D2 * t73 * t42 + 0.9D1 * t70 * t95 - 0.9D1 * t2 * t3 
     #* t95 - 0.9D1 * t9 * t14 * t28 * t102 - 0.18D2 * t9 * t14 * t62 * 
     #t34 - 0.18D2 * t78 * t63 - 0.36D2 * t79 * t60 * t29 * t34 - 0.36D2
     # * t54 * t56 * t58 * t60 * t102 * t53
      rrgq2qgh82J1 = -wd * t122 / t53 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh82J2
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
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t29 = t28 ** 2
      t34 = t18 * t15 * t11 + x2 * x3 + t27
      t35 = t34 ** 2
      t39 = t2 * t4
      t40 = 0.1D1 / t12
      t41 = t7 * t40
      t42 = t41 * t29
      t45 = s * t3
      t46 = 0.1D1 / t11
      t47 = x1 * t46
      t48 = t47 * t34
      t50 = 0.1D1 - x1
      t53 = s - t45 * t48 - t45 * t50 * t18
      t54 = s * t1
      t55 = t53 * t54
      t56 = t4 * t3
      t57 = t55 * t56
      t58 = t7 * x1
      t60 = 0.1D1 / t12 / t11
      t61 = t58 * t60
      t62 = t29 * t28
      t63 = t61 * t62
      t65 = 0.18D2 * t57 * t63
      t70 = t55 * t3
      t72 = 0.36D2 * t70 * t48
      t73 = t55 * t4
      t74 = t28 * t34
      t78 = t2 * t56
      t79 = t78 * t58
      t95 = t47 * t28
      t102 = t35 * t34
      t109 = 0.18D2 * t9 * t14 * t62 * t34
      t111 = 0.18D2 * t78 * t63
      t116 = t54 * t56
      t121 = 0.36D2 * t116 * t58 * t60 * t102 * t53
      t122 = -0.18D2 * t9 * t14 * t29 * t35 - 0.18D2 * t39 * t42 + t65 +
     # 0.54D2 * t57 * t61 * t29 * t34 - t72 + 0.36D2 * t73 * t41 * t74 -
     # 0.27D2 * t79 * t60 * t28 * t35 + 0.63D2 * t57 * t61 * t28 * t35 -
     # 0.27D2 * t39 * t7 * t40 * t28 * t34 + 0.18D2 * t73 * t42 + 0.9D1 
     #* t70 * t95 - 0.9D1 * t2 * t3 * t95 - 0.9D1 * t9 * t14 * t28 * t10
     #2 - t109 - t111 - 0.36D2 * t79 * t60 * t29 * t34 - t121
      t124 = t50 ** 2
      t125 = x3 ** 2
      t126 = t125 * t124
      t129 = t46 * t34
      t135 = t46 * t28 * t126
      t146 = t50 * x3
      t147 = t40 * t29 * t146
      t177 = t72 - 0.36D2 * t116 * t126 * t53 * x1 * t129 - 0.18D2 * t78
     # * x1 * t135 - 0.18D2 * t6 * t61 * t29 * t50 * x3 * t34 - 0.18D2 *
     # t78 * t7 * t147 + t109 - 0.72D2 * t73 * t41 * t35 + t121 - 0.18D2
     # * t6 * t41 * t74 * t126 + 0.36D2 * t55 * t4 * t50 * x3 * x1 * t12
     #9 + 0.18D2 * t55 * t56 * x1 * t135 - 0.36D2 * t116 * t146 * t53 * 
     #t7 * t40 * t35 + t111 + 0.18D2 * t55 * t56 * t7 * t147 - t65
      rrgq2qgh82J2 = -(wd * t122 + wd * t177) / t53 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh82J3
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
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t29 = t28 ** 2
      t34 = t18 * t15 * t11 + x2 * x3 + t27
      t35 = t34 ** 2
      t39 = t2 * t4
      t40 = 0.1D1 / t12
      t41 = t7 * t40
      t42 = t41 * t29
      t45 = s * t3
      t46 = 0.1D1 / t11
      t47 = x1 * t46
      t48 = t47 * t34
      t50 = 0.1D1 - x1
      t53 = s - t45 * t48 - t45 * t50 * t18
      t54 = s * t1
      t55 = t53 * t54
      t56 = t4 * t3
      t57 = t55 * t56
      t58 = t7 * x1
      t60 = 0.1D1 / t12 / t11
      t61 = t58 * t60
      t62 = t29 * t28
      t63 = t61 * t62
      t65 = 0.18D2 * t57 * t63
      t70 = t55 * t3
      t72 = 0.36D2 * t70 * t48
      t73 = t55 * t4
      t74 = t28 * t34
      t78 = t2 * t56
      t79 = t78 * t58
      t95 = t47 * t28
      t102 = t35 * t34
      t109 = 0.18D2 * t9 * t14 * t62 * t34
      t111 = 0.18D2 * t78 * t63
      t116 = t54 * t56
      t121 = 0.36D2 * t116 * t58 * t60 * t102 * t53
      t122 = -0.18D2 * t9 * t14 * t29 * t35 - 0.18D2 * t39 * t42 + t65 +
     # 0.54D2 * t57 * t61 * t29 * t34 - t72 + 0.36D2 * t73 * t41 * t74 -
     # 0.27D2 * t79 * t60 * t28 * t35 + 0.63D2 * t57 * t61 * t28 * t35 -
     # 0.27D2 * t39 * t7 * t40 * t28 * t34 + 0.18D2 * t73 * t42 + 0.9D1 
     #* t70 * t95 - 0.9D1 * t2 * t3 * t95 - 0.9D1 * t9 * t14 * t28 * t10
     #2 - t109 - t111 - 0.36D2 * t79 * t60 * t29 * t34 - t121
      t124 = t50 ** 2
      t125 = x3 ** 2
      t126 = t125 * t124
      t129 = t46 * t34
      t135 = t46 * t28 * t126
      t146 = t50 * x3
      t147 = t40 * t29 * t146
      t177 = t72 - 0.36D2 * t116 * t126 * t53 * x1 * t129 - 0.18D2 * t78
     # * x1 * t135 - 0.18D2 * t6 * t61 * t29 * t50 * x3 * t34 - 0.18D2 *
     # t78 * t7 * t147 + t109 - 0.72D2 * t73 * t41 * t35 + t121 - 0.18D2
     # * t6 * t41 * t74 * t126 + 0.36D2 * t55 * t4 * t50 * x3 * x1 * t12
     #9 + 0.18D2 * t55 * t56 * x1 * t135 - 0.36D2 * t116 * t146 * t53 * 
     #t7 * t40 * t35 + t111 + 0.18D2 * t55 * t56 * t7 * t147 - t65
      rrgq2qgh82J3 = -(wd * t122 + wd * t177) / t53 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh82J4
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
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t29 = t28 ** 2
      t34 = t18 * t15 * t11 + x2 * x3 + t27
      t35 = t34 ** 2
      t39 = t2 * t4
      t40 = 0.1D1 / t12
      t41 = t7 * t40
      t42 = t41 * t29
      t45 = s * t3
      t46 = 0.1D1 / t11
      t47 = x1 * t46
      t48 = t47 * t34
      t50 = 0.1D1 - x1
      t53 = s - t45 * t48 - t45 * t50 * t18
      t54 = s * t1
      t55 = t53 * t54
      t56 = t4 * t3
      t57 = t55 * t56
      t58 = t7 * x1
      t60 = 0.1D1 / t12 / t11
      t61 = t58 * t60
      t62 = t29 * t28
      t63 = t61 * t62
      t65 = 0.18D2 * t57 * t63
      t70 = t55 * t3
      t72 = 0.36D2 * t70 * t48
      t73 = t55 * t4
      t74 = t28 * t34
      t78 = t2 * t56
      t79 = t78 * t58
      t95 = t47 * t28
      t102 = t35 * t34
      t109 = 0.18D2 * t9 * t14 * t62 * t34
      t111 = 0.18D2 * t78 * t63
      t116 = t54 * t56
      t121 = 0.36D2 * t116 * t58 * t60 * t102 * t53
      t122 = -0.18D2 * t9 * t14 * t29 * t35 - 0.18D2 * t39 * t42 + t65 +
     # 0.54D2 * t57 * t61 * t29 * t34 - t72 + 0.36D2 * t73 * t41 * t74 -
     # 0.27D2 * t79 * t60 * t28 * t35 + 0.63D2 * t57 * t61 * t28 * t35 -
     # 0.27D2 * t39 * t7 * t40 * t28 * t34 + 0.18D2 * t73 * t42 + 0.9D1 
     #* t70 * t95 - 0.9D1 * t2 * t3 * t95 - 0.9D1 * t9 * t14 * t28 * t10
     #2 - t109 - t111 - 0.36D2 * t79 * t60 * t29 * t34 - t121
      t124 = t50 ** 2
      t125 = x3 ** 2
      t126 = t125 * t124
      t129 = t46 * t34
      t135 = t46 * t28 * t126
      t146 = t50 * x3
      t147 = t40 * t29 * t146
      t177 = t72 - 0.36D2 * t116 * t126 * t53 * x1 * t129 - 0.18D2 * t78
     # * x1 * t135 - 0.18D2 * t6 * t61 * t29 * t50 * x3 * t34 - 0.18D2 *
     # t78 * t7 * t147 + t109 - 0.72D2 * t73 * t41 * t35 + t121 - 0.18D2
     # * t6 * t41 * t74 * t126 + 0.36D2 * t55 * t4 * t50 * x3 * x1 * t12
     #9 + 0.18D2 * t55 * t56 * x1 * t135 - 0.36D2 * t116 * t146 * t53 * 
     #t7 * t40 * t35 + t111 + 0.18D2 * t55 * t56 * t7 * t147 - t65
      rrgq2qgh82J4 = -(wd * t122 + wd * t177) / t53 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh82J5
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
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t29 = t28 ** 2
      t34 = t18 * t15 * t11 + x2 * x3 + t27
      t35 = t34 ** 2
      t39 = t2 * t4
      t40 = 0.1D1 / t12
      t41 = t7 * t40
      t42 = t41 * t29
      t45 = s * t3
      t46 = 0.1D1 / t11
      t47 = x1 * t46
      t48 = t47 * t34
      t50 = 0.1D1 - x1
      t53 = s - t45 * t48 - t45 * t50 * t18
      t54 = s * t1
      t55 = t53 * t54
      t56 = t4 * t3
      t57 = t55 * t56
      t58 = t7 * x1
      t60 = 0.1D1 / t12 / t11
      t61 = t58 * t60
      t62 = t29 * t28
      t63 = t61 * t62
      t65 = 0.18D2 * t57 * t63
      t70 = t55 * t3
      t72 = 0.36D2 * t70 * t48
      t73 = t55 * t4
      t74 = t28 * t34
      t78 = t2 * t56
      t79 = t78 * t58
      t95 = t47 * t28
      t102 = t35 * t34
      t109 = 0.18D2 * t9 * t14 * t62 * t34
      t111 = 0.18D2 * t78 * t63
      t116 = t54 * t56
      t121 = 0.36D2 * t116 * t58 * t60 * t102 * t53
      t122 = -0.18D2 * t9 * t14 * t29 * t35 - 0.18D2 * t39 * t42 + t65 +
     # 0.54D2 * t57 * t61 * t29 * t34 - t72 + 0.36D2 * t73 * t41 * t74 -
     # 0.27D2 * t79 * t60 * t28 * t35 + 0.63D2 * t57 * t61 * t28 * t35 -
     # 0.27D2 * t39 * t7 * t40 * t28 * t34 + 0.18D2 * t73 * t42 + 0.9D1 
     #* t70 * t95 - 0.9D1 * t2 * t3 * t95 - 0.9D1 * t9 * t14 * t28 * t10
     #2 - t109 - t111 - 0.36D2 * t79 * t60 * t29 * t34 - t121
      t124 = t50 ** 2
      t125 = x3 ** 2
      t126 = t125 * t124
      t129 = t46 * t34
      t135 = t46 * t28 * t126
      t146 = t50 * x3
      t147 = t40 * t29 * t146
      t177 = t72 - 0.36D2 * t116 * t126 * t53 * x1 * t129 - 0.18D2 * t78
     # * x1 * t135 - 0.18D2 * t6 * t61 * t29 * t50 * x3 * t34 - 0.18D2 *
     # t78 * t7 * t147 + t109 - 0.72D2 * t73 * t41 * t35 + t121 - 0.18D2
     # * t6 * t41 * t74 * t126 + 0.36D2 * t55 * t4 * t50 * x3 * x1 * t12
     #9 + 0.18D2 * t55 * t56 * x1 * t135 - 0.36D2 * t116 * t146 * t53 * 
     #t7 * t40 * t35 + t111 + 0.18D2 * t55 * t56 * t7 * t147 - t65
      rrgq2qgh82J5 = -(wd * t122 + wd * t177) / t53 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh82J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t22 = x1 * t5 * t21
      t24 = 0.1D1 - x1
      t27 = s - t2 * t22 - t2 * t24 * t7
      t28 = s ** 2
      t29 = t28 * s
      t30 = t27 * t29
      t34 = t1 ** 2
      t35 = t34 * t1
      t36 = t29 * t35
      t37 = t24 ** 2
      t38 = x3 ** 2
      t39 = t37 * t38
      t42 = t5 * t21
      t46 = t28 ** 2
      t47 = t46 * t35
      t51 = t14 * t4 + x2 * t7 - t20
      t53 = t5 * t51 * t39
      t56 = t34 ** 2
      t57 = t46 * t56
      t58 = x1 ** 2
      t59 = t58 * x1
      t60 = t4 ** 2
      t62 = 0.1D1 / t60 / t4
      t63 = t59 * t62
      t65 = t51 ** 2
      t72 = 0.1D1 / t60
      t74 = t24 * x3
      t75 = t72 * t65 * t74
      t78 = t58 ** 2
      t80 = t60 ** 2
      t82 = t65 * t51
      t88 = t58 * t72
      t89 = t21 ** 2
      t120 = t63 * t82
      t130 = 0.36D2 * t30 * t1 * t22 - 0.36D2 * t36 * t39 * t27 * x1 * t
     #42 - 0.18D2 * t47 * x1 * t53 - 0.18D2 * t57 * t63 * t65 * t24 * x3
     # * t21 - 0.18D2 * t47 * t58 * t75 + 0.18D2 * t57 * t78 / t80 * t82
     # * t21 - 0.72D2 * t30 * t34 * t88 * t89 + 0.36D2 * t36 * t59 * t62
     # * t89 * t21 * t27 - 0.18D2 * t57 * t88 * t51 * t21 * t39 + 0.36D2
     # * t30 * t34 * t24 * x3 * x1 * t42 + 0.18D2 * t30 * t35 * x1 * t53
     # - 0.36D2 * t36 * t74 * t27 * t58 * t72 * t89 + 0.18D2 * t47 * t12
     #0 + 0.18D2 * t30 * t35 * t58 * t75 - 0.18D2 * t30 * t35 * t120
      rrgq2qgh82J6 = -wd * t130 / t27 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh83J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t18 = Sqrt(x3 * t8 * t4 * x2 * t7)
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 / t4 * (t7 * t8 * t4 + x2 * x3 + 0.2D1 * t13 * t
     #18) - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t33 = t24 ** 2
      t37 = t31 * t1
      t38 = t30 * t37
      t39 = t33 * t24
      t41 = x3 ** 2
      t44 = t7 ** 2
      rrgq2qgh83J1 = -wd * (-0.8D1 * t30 * t31 * t33 * t7 * x3 + 0.8D1 *
     # t38 * t39 * t7 * t41 + 0.8D1 * t38 * t39 * t44 * x3 + 0.8D1 * t30
     # * t1 * t24 * t7 + 0.8D1 * t30 * t37 * t39 * t44 * t7) / t27 / s /
     # z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh83J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 / t4 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t33 = t24 ** 2
      t36 = t30 * t31 * t33 * t7 * x3
      t37 = t31 * t1
      t38 = t30 * t37
      t39 = t33 * t24
      t41 = x3 ** 2
      t43 = t38 * t39 * t7 * t41
      t44 = t7 ** 2
      t47 = t38 * t39 * t44 * x3
      t50 = t30 * t1 * t24 * t7
      t54 = t30 * t37 * t39 * t44 * t7
      t65 = x1 ** 2
      t68 = t4 ** 2
      rrgq2qgh83J2 = -(wd * (-0.8D1 * t36 + 0.8D1 * t43 + 0.8D1 * t47 + 
     #0.8D1 * t50 + 0.8D1 * t54) + wd * (-0.16D2 * t43 + 0.16D2 * t30 * 
     #t31 * t33 * t44 - 0.8D1 * t54 - 0.8D1 * t50 - 0.4D1 * t30 * t37 * 
     #t65 / t68 * (t14 * t4 + x2 * t7 - t20) * t24 * x3 * t21 - 0.16D2 *
     # t47 + 0.16D2 * t36)) / t27 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh83J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t33 = t24 ** 2
      t36 = t30 * t31 * t33 * t7 * x3
      t37 = t31 * t1
      t38 = t30 * t37
      t39 = t33 * t24
      t41 = x3 ** 2
      t43 = t38 * t39 * t7 * t41
      t44 = t7 ** 2
      t47 = t38 * t39 * t44 * x3
      t50 = t30 * t1 * t24 * t7
      t54 = t30 * t37 * t39 * t44 * t7
      t65 = x1 ** 2
      t67 = t30 * t37 * t65
      t68 = t4 ** 2
      t69 = 0.1D1 / t68
      t72 = t14 * t4 + x2 * t7 - t20
      t74 = t24 * x3
      t78 = 0.4D1 * t67 * t69 * t72 * t74 * t21
      t83 = t28 ** 2
      t86 = t5 * t72
      t88 = t86 * t33 * t41
      t102 = t72 ** 2
      rrgq2qgh83J3 = -(wd * (-0.8D1 * t36 + 0.8D1 * t43 + 0.8D1 * t47 + 
     #0.8D1 * t50 + 0.8D1 * t54) + wd * (-0.16D2 * t43 + 0.16D2 * t30 * 
     #t31 * t33 * t44 - 0.8D1 * t54 - 0.8D1 * t50 - t78 - 0.16D2 * t47 +
     # 0.16D2 * t36) + wd * (-0.4D1 * t83 * t37 * x1 * t88 + 0.4D1 * t83
     # * t31 * t24 * x3 * x1 * t86 - t78 - 0.4D1 * t30 * t31 * x1 * t86 
     #* t74 + 0.4D1 * t67 * t69 * t102 * t74 + 0.8D1 * t30 * t37 * x1 * 
     #t88)) / t27 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh83J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t33 = t24 ** 2
      t36 = t30 * t31 * t33 * t7 * x3
      t37 = t31 * t1
      t38 = t30 * t37
      t39 = t33 * t24
      t41 = x3 ** 2
      t43 = t38 * t39 * t7 * t41
      t44 = t7 ** 2
      t47 = t38 * t39 * t44 * x3
      t50 = t30 * t1 * t24 * t7
      t54 = t30 * t37 * t39 * t44 * t7
      t65 = x1 ** 2
      t67 = t30 * t37 * t65
      t68 = t4 ** 2
      t69 = 0.1D1 / t68
      t72 = t14 * t4 + x2 * t7 - t20
      t74 = t24 * x3
      t78 = 0.4D1 * t67 * t69 * t72 * t74 * t21
      t83 = t28 ** 2
      t86 = t5 * t72
      t88 = t86 * t33 * t41
      t102 = t72 ** 2
      rrgq2qgh83J4 = -(wd * (-0.8D1 * t36 + 0.8D1 * t43 + 0.8D1 * t47 + 
     #0.8D1 * t50 + 0.8D1 * t54) + wd * (-0.16D2 * t43 + 0.16D2 * t30 * 
     #t31 * t33 * t44 - 0.8D1 * t54 - 0.8D1 * t50 - t78 - 0.16D2 * t47 +
     # 0.16D2 * t36) + wd * (-0.4D1 * t83 * t37 * x1 * t88 + 0.4D1 * t83
     # * t31 * t24 * x3 * x1 * t86 - t78 - 0.4D1 * t30 * t31 * x1 * t86 
     #* t74 + 0.4D1 * t67 * t69 * t102 * t74 + 0.8D1 * t30 * t37 * x1 * 
     #t88)) / t27 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh83J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t33 = t24 ** 2
      t36 = t30 * t31 * t33 * t7 * x3
      t37 = t31 * t1
      t38 = t30 * t37
      t39 = t33 * t24
      t41 = x3 ** 2
      t43 = t38 * t39 * t7 * t41
      t44 = t7 ** 2
      t47 = t38 * t39 * t44 * x3
      t50 = t30 * t1 * t24 * t7
      t54 = t30 * t37 * t39 * t44 * t7
      t65 = x1 ** 2
      t67 = t30 * t37 * t65
      t68 = t4 ** 2
      t69 = 0.1D1 / t68
      t72 = t14 * t4 + x2 * t7 - t20
      t74 = t24 * x3
      t78 = 0.4D1 * t67 * t69 * t72 * t74 * t21
      t83 = t28 ** 2
      t86 = t5 * t72
      t88 = t86 * t33 * t41
      t102 = t72 ** 2
      rrgq2qgh83J5 = -(wd * (-0.8D1 * t36 + 0.8D1 * t43 + 0.8D1 * t47 + 
     #0.8D1 * t50 + 0.8D1 * t54) + wd * (-0.16D2 * t43 + 0.16D2 * t30 * 
     #t31 * t33 * t44 - 0.8D1 * t54 - 0.8D1 * t50 - t78 - 0.16D2 * t47 +
     # 0.16D2 * t36) + wd * (-0.4D1 * t83 * t37 * x1 * t88 + 0.4D1 * t83
     # * t31 * t24 * x3 * x1 * t86 - t78 - 0.4D1 * t30 * t31 * x1 * t86 
     #* t74 + 0.4D1 * t67 * t69 * t102 * t74 + 0.8D1 * t30 * t37 * x1 * 
     #t88)) / t27 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh83J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t30 * t32
      t34 = t24 ** 2
      t35 = t34 * t24
      t37 = x3 ** 2
      t42 = t7 ** 2
      t55 = x1 ** 2
      t57 = t30 * t32 * t55
      t58 = t4 ** 2
      t59 = 0.1D1 / t58
      t62 = t14 * t4 + x2 * t7 - t20
      t64 = t24 * x3
      t68 = 0.4D1 * t57 * t59 * t62 * t64 * t21
      t80 = t28 ** 2
      t83 = t5 * t62
      t85 = t83 * t34 * t37
      t99 = t62 ** 2
      rrgq2qgh83J6 = -(wd * (-0.16D2 * t33 * t35 * t7 * t37 + 0.16D2 * t
     #30 * t31 * t34 * t42 - 0.8D1 * t30 * t32 * t35 * t42 * t7 - 0.8D1 
     #* t30 * t1 * t24 * t7 - t68 - 0.16D2 * t33 * t35 * t42 * x3 + 0.16
     #D2 * t30 * t31 * t34 * t7 * x3) + wd * (-0.4D1 * t80 * t32 * x1 * 
     #t85 + 0.4D1 * t80 * t31 * t24 * x3 * x1 * t83 - t68 - 0.4D1 * t30 
     #* t31 * x1 * t83 * t64 + 0.4D1 * t57 * t59 * t99 * t64 + 0.8D1 * t
     #30 * t32 * x1 * t85)) / t27 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh83J7
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
      t5 = t4 * t3
      t9 = z + x1 * t3
      t10 = 0.1D1 / t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t9 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t9 + x2 * t14 - t23
      t25 = t10 * t24
      t26 = 0.1D1 - x1
      t27 = t26 ** 2
      t28 = x3 ** 2
      t30 = t25 * t27 * t28
      t39 = s * t3
      t44 = t14 * t11 * t9 + x2 * x3 + t23
      t49 = s - t39 * x1 * t10 * t44 - t39 * t26 * t14
      t51 = t49 * t1 * s
      t52 = x1 ** 2
      t54 = t51 * t5 * t52
      t55 = t9 ** 2
      t56 = 0.1D1 / t55
      t58 = t26 * x3
      t68 = t24 ** 2
      rrgq2qgh83J7 = -wd * (-0.4D1 * t2 * t5 * x1 * t30 + 0.4D1 * t2 * t
     #4 * t26 * x3 * x1 * t25 - 0.4D1 * t54 * t56 * t24 * t58 * t44 - 0.
     #4D1 * t51 * t4 * x1 * t25 * t58 + 0.4D1 * t54 * t56 * t68 * t58 + 
     #0.8D1 * t51 * t5 * x1 * t30) / t49 / s / z / 0.3141592653589793D1 
     #/ 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh84J1
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
      t9 = t8 ** 2
      t10 = 0.1D1 - x3
      t12 = x3 ** 2
      t13 = t12 * x3
      t17 = t10 ** 2
      t22 = t17 * t10
      t27 = s * t1
      t28 = t4 * t3
      t30 = t8 * t7
      t32 = s * t3
      t34 = z + x1 * t3
      t38 = x3 * (0.1D1 - x2)
      t42 = cos(x4 * 0.3141592653589793D1)
      t46 = Sqrt(t38 * t34 * x2 * t10)
      t54 = s - t32 * x1 / t34 * (t38 * t34 + x2 * t10 - 0.2D1 * t42 * t
     #46) - t32 * t7 * x3
      t58 = t54 * t27
      t69 = t8 * t10 * x3
      t72 = t58 * t28
      t74 = t30 * t10 * t12
      t77 = t2 * t28
      t81 = t3 * t7
      t89 = t2 * t4
      t101 = t30 * t17 * x3
      t106 = -0.9D1 * t6 * t9 * t10 * t13 - 0.18D2 * t6 * t9 * t17 * t12
     # - 0.18D2 * t6 * t9 * t22 * x3 - 0.36D2 * t27 * t28 * t30 * t13 * 
     #t54 + 0.18D2 * t58 * t28 * t30 * t22 - 0.9D1 * t2 * t3 * t7 * t10 
     #+ 0.36D2 * t58 * t4 * t69 + 0.63D2 * t72 * t74 - 0.18D2 * t77 * t3
     #0 * t22 - 0.36D2 * t58 * t81 * x3 + 0.18D2 * t58 * t4 * t8 * t17 -
     # 0.18D2 * t89 * t8 * t17 - 0.27D2 * t89 * t69 + 0.9D1 * t58 * t81 
     #* t10 - 0.27D2 * t77 * t74 - 0.36D2 * t77 * t101 + 0.54D2 * t72 * 
     #t101
      rrgq2qgh84J1 = -wd * t106 / t54 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh84J2
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
      t9 = t8 ** 2
      t10 = 0.1D1 - x3
      t12 = x3 ** 2
      t13 = t12 * x3
      t17 = t10 ** 2
      t22 = t17 * t10
      t26 = 0.18D2 * t6 * t9 * t22 * x3
      t27 = s * t1
      t28 = t4 * t3
      t29 = t27 * t28
      t30 = t8 * t7
      t32 = s * t3
      t34 = z + x1 * t3
      t35 = 0.1D1 / t34
      t36 = x1 * t35
      t37 = 0.1D1 - x2
      t38 = x3 * t37
      t42 = cos(x4 * 0.3141592653589793D1)
      t46 = Sqrt(t38 * t34 * x2 * t10)
      t48 = 0.2D1 * t42 * t46
      t52 = t7 * x3
      t54 = s - t32 * t36 * (t38 * t34 + x2 * t10 - t48) - t32 * t52
      t57 = 0.36D2 * t29 * t30 * t13 * t54
      t58 = t54 * t27
      t62 = 0.18D2 * t58 * t28 * t30 * t22
      t64 = t7 * t10
      t68 = t8 * t10
      t69 = t68 * x3
      t72 = t58 * t28
      t74 = t30 * t10 * t12
      t77 = t2 * t28
      t80 = 0.18D2 * t77 * t30 * t22
      t81 = t3 * t7
      t84 = 0.36D2 * t58 * t81 * x3
      t85 = t4 * t8
      t89 = t2 * t4
      t100 = t30 * t17
      t101 = t100 * x3
      t106 = -0.9D1 * t6 * t9 * t10 * t13 - 0.18D2 * t6 * t9 * t17 * t12
     # - t26 - t57 + t62 - 0.9D1 * t2 * t3 * t64 + 0.36D2 * t58 * t4 * t
     #69 + 0.63D2 * t72 * t74 - t80 - t84 + 0.18D2 * t58 * t85 * t17 - 0
     #.18D2 * t89 * t8 * t17 - 0.27D2 * t89 * t69 + 0.9D1 * t58 * t81 * 
     #t10 - 0.27D2 * t77 * t74 - 0.36D2 * t77 * t101 + 0.54D2 * t72 * t1
     #01
      t108 = x1 ** 2
      t109 = t34 ** 2
      t110 = 0.1D1 / t109
      t116 = t10 * t37 * t34 + x2 * x3 + t48
      t117 = t116 ** 2
      t127 = t110 * t117
      t133 = t35 * t116
      t134 = t17 * x1 * t133
      t167 = -0.36D2 * t29 * t108 * t110 * t117 * t54 * t52 - 0.72D2 * t
     #58 * t85 * t12 - 0.18D2 * t6 * t68 * x3 * t108 * t127 - 0.18D2 * t
     #77 * t8 * t134 - 0.18D2 * t6 * t100 * t36 * t116 * x3 - 0.36D2 * t
     #29 * t36 * t116 * t54 * t8 * t12 + 0.36D2 * t58 * t4 * x1 * t133 *
     # t52 + t84 - t62 + t26 + t80 - 0.18D2 * t77 * t108 * t127 * t64 + 
     #t57 + 0.18D2 * t58 * t28 * t7 * t10 * t108 * t127 + 0.18D2 * t58 *
     # t28 * t8 * t134
      rrgq2qgh84J2 = -(wd * t106 + wd * t167) / t54 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh84J3
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
      t9 = t8 ** 2
      t10 = 0.1D1 - x3
      t12 = x3 ** 2
      t13 = t12 * x3
      t17 = t10 ** 2
      t22 = t17 * t10
      t26 = 0.18D2 * t6 * t9 * t22 * x3
      t27 = s * t1
      t28 = t4 * t3
      t29 = t27 * t28
      t30 = t8 * t7
      t32 = s * t3
      t34 = z + x1 * t3
      t35 = 0.1D1 / t34
      t36 = x1 * t35
      t37 = 0.1D1 - x2
      t38 = x3 * t37
      t42 = cos(x4 * 0.3141592653589793D1)
      t46 = Sqrt(t38 * t34 * x2 * t10)
      t48 = 0.2D1 * t42 * t46
      t52 = t7 * x3
      t54 = s - t32 * t36 * (t38 * t34 + x2 * t10 - t48) - t32 * t52
      t57 = 0.36D2 * t29 * t30 * t13 * t54
      t58 = t54 * t27
      t62 = 0.18D2 * t58 * t28 * t30 * t22
      t64 = t7 * t10
      t68 = t8 * t10
      t69 = t68 * x3
      t72 = t58 * t28
      t74 = t30 * t10 * t12
      t77 = t2 * t28
      t80 = 0.18D2 * t77 * t30 * t22
      t81 = t3 * t7
      t84 = 0.36D2 * t58 * t81 * x3
      t85 = t4 * t8
      t89 = t2 * t4
      t100 = t30 * t17
      t101 = t100 * x3
      t106 = -0.9D1 * t6 * t9 * t10 * t13 - 0.18D2 * t6 * t9 * t17 * t12
     # - t26 - t57 + t62 - 0.9D1 * t2 * t3 * t64 + 0.36D2 * t58 * t4 * t
     #69 + 0.63D2 * t72 * t74 - t80 - t84 + 0.18D2 * t58 * t85 * t17 - 0
     #.18D2 * t89 * t8 * t17 - 0.27D2 * t89 * t69 + 0.9D1 * t58 * t81 * 
     #t10 - 0.27D2 * t77 * t74 - 0.36D2 * t77 * t101 + 0.54D2 * t72 * t1
     #01
      t108 = x1 ** 2
      t109 = t34 ** 2
      t110 = 0.1D1 / t109
      t116 = t10 * t37 * t34 + x2 * x3 + t48
      t117 = t116 ** 2
      t127 = t110 * t117
      t133 = t35 * t116
      t134 = t17 * x1 * t133
      t167 = -0.36D2 * t29 * t108 * t110 * t117 * t54 * t52 - 0.72D2 * t
     #58 * t85 * t12 - 0.18D2 * t6 * t68 * x3 * t108 * t127 - 0.18D2 * t
     #77 * t8 * t134 - 0.18D2 * t6 * t100 * t36 * t116 * x3 - 0.36D2 * t
     #29 * t36 * t116 * t54 * t8 * t12 + 0.36D2 * t58 * t4 * x1 * t133 *
     # t52 + t84 - t62 + t26 + t80 - 0.18D2 * t77 * t108 * t127 * t64 + 
     #t57 + 0.18D2 * t58 * t28 * t7 * t10 * t108 * t127 + 0.18D2 * t58 *
     # t28 * t8 * t134
      rrgq2qgh84J3 = -(wd * t106 + wd * t167) / t54 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh84J4
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
      t9 = t8 ** 2
      t10 = 0.1D1 - x3
      t12 = x3 ** 2
      t13 = t12 * x3
      t17 = t10 ** 2
      t22 = t17 * t10
      t26 = 0.18D2 * t6 * t9 * t22 * x3
      t27 = s * t1
      t28 = t4 * t3
      t29 = t27 * t28
      t30 = t8 * t7
      t32 = s * t3
      t34 = z + x1 * t3
      t35 = 0.1D1 / t34
      t36 = x1 * t35
      t37 = 0.1D1 - x2
      t38 = x3 * t37
      t42 = cos(x4 * 0.3141592653589793D1)
      t46 = Sqrt(t38 * t34 * x2 * t10)
      t48 = 0.2D1 * t42 * t46
      t52 = t7 * x3
      t54 = s - t32 * t36 * (t38 * t34 + x2 * t10 - t48) - t32 * t52
      t57 = 0.36D2 * t29 * t30 * t13 * t54
      t58 = t54 * t27
      t62 = 0.18D2 * t58 * t28 * t30 * t22
      t64 = t7 * t10
      t68 = t8 * t10
      t69 = t68 * x3
      t72 = t58 * t28
      t74 = t30 * t10 * t12
      t77 = t2 * t28
      t80 = 0.18D2 * t77 * t30 * t22
      t81 = t3 * t7
      t84 = 0.36D2 * t58 * t81 * x3
      t85 = t4 * t8
      t89 = t2 * t4
      t100 = t30 * t17
      t101 = t100 * x3
      t106 = -0.9D1 * t6 * t9 * t10 * t13 - 0.18D2 * t6 * t9 * t17 * t12
     # - t26 - t57 + t62 - 0.9D1 * t2 * t3 * t64 + 0.36D2 * t58 * t4 * t
     #69 + 0.63D2 * t72 * t74 - t80 - t84 + 0.18D2 * t58 * t85 * t17 - 0
     #.18D2 * t89 * t8 * t17 - 0.27D2 * t89 * t69 + 0.9D1 * t58 * t81 * 
     #t10 - 0.27D2 * t77 * t74 - 0.36D2 * t77 * t101 + 0.54D2 * t72 * t1
     #01
      t108 = x1 ** 2
      t109 = t34 ** 2
      t110 = 0.1D1 / t109
      t116 = t10 * t37 * t34 + x2 * x3 + t48
      t117 = t116 ** 2
      t127 = t110 * t117
      t133 = t35 * t116
      t134 = t17 * x1 * t133
      t167 = -0.36D2 * t29 * t108 * t110 * t117 * t54 * t52 - 0.72D2 * t
     #58 * t85 * t12 - 0.18D2 * t6 * t68 * x3 * t108 * t127 - 0.18D2 * t
     #77 * t8 * t134 - 0.18D2 * t6 * t100 * t36 * t116 * x3 - 0.36D2 * t
     #29 * t36 * t116 * t54 * t8 * t12 + 0.36D2 * t58 * t4 * x1 * t133 *
     # t52 + t84 - t62 + t26 + t80 - 0.18D2 * t77 * t108 * t127 * t64 + 
     #t57 + 0.18D2 * t58 * t28 * t7 * t10 * t108 * t127 + 0.18D2 * t58 *
     # t28 * t8 * t134
      rrgq2qgh84J4 = -(wd * t106 + wd * t167) / t54 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh84J5
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
      t9 = t8 ** 2
      t10 = 0.1D1 - x3
      t12 = x3 ** 2
      t13 = t12 * x3
      t17 = t10 ** 2
      t22 = t17 * t10
      t26 = 0.18D2 * t6 * t9 * t22 * x3
      t27 = s * t1
      t28 = t4 * t3
      t29 = t27 * t28
      t30 = t8 * t7
      t32 = s * t3
      t34 = z + x1 * t3
      t35 = 0.1D1 / t34
      t36 = x1 * t35
      t37 = 0.1D1 - x2
      t38 = x3 * t37
      t42 = cos(x4 * 0.3141592653589793D1)
      t46 = Sqrt(t38 * t34 * x2 * t10)
      t48 = 0.2D1 * t42 * t46
      t52 = t7 * x3
      t54 = s - t32 * t36 * (t38 * t34 + x2 * t10 - t48) - t32 * t52
      t57 = 0.36D2 * t29 * t30 * t13 * t54
      t58 = t54 * t27
      t62 = 0.18D2 * t58 * t28 * t30 * t22
      t64 = t7 * t10
      t68 = t8 * t10
      t69 = t68 * x3
      t72 = t58 * t28
      t74 = t30 * t10 * t12
      t77 = t2 * t28
      t80 = 0.18D2 * t77 * t30 * t22
      t81 = t3 * t7
      t84 = 0.36D2 * t58 * t81 * x3
      t85 = t4 * t8
      t89 = t2 * t4
      t100 = t30 * t17
      t101 = t100 * x3
      t106 = -0.9D1 * t6 * t9 * t10 * t13 - 0.18D2 * t6 * t9 * t17 * t12
     # - t26 - t57 + t62 - 0.9D1 * t2 * t3 * t64 + 0.36D2 * t58 * t4 * t
     #69 + 0.63D2 * t72 * t74 - t80 - t84 + 0.18D2 * t58 * t85 * t17 - 0
     #.18D2 * t89 * t8 * t17 - 0.27D2 * t89 * t69 + 0.9D1 * t58 * t81 * 
     #t10 - 0.27D2 * t77 * t74 - 0.36D2 * t77 * t101 + 0.54D2 * t72 * t1
     #01
      t108 = x1 ** 2
      t109 = t34 ** 2
      t110 = 0.1D1 / t109
      t116 = t10 * t37 * t34 + x2 * x3 + t48
      t117 = t116 ** 2
      t127 = t110 * t117
      t133 = t35 * t116
      t134 = t17 * x1 * t133
      t167 = -0.36D2 * t29 * t108 * t110 * t117 * t54 * t52 - 0.72D2 * t
     #58 * t85 * t12 - 0.18D2 * t6 * t68 * x3 * t108 * t127 - 0.18D2 * t
     #77 * t8 * t134 - 0.18D2 * t6 * t100 * t36 * t116 * x3 - 0.36D2 * t
     #29 * t36 * t116 * t54 * t8 * t12 + 0.36D2 * t58 * t4 * x1 * t133 *
     # t52 + t84 - t62 + t26 + t80 - 0.18D2 * t77 * t108 * t127 * t64 + 
     #t57 + 0.18D2 * t58 * t28 * t7 * t10 * t108 * t127 + 0.18D2 * t58 *
     # t28 * t8 * t134
      rrgq2qgh84J5 = -(wd * t106 + wd * t167) / t54 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrgq2qgh84J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = x1 ** 2
      t9 = z + x1 * t3
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t14 = 0.1D1 - x3
      t15 = 0.1D1 - x2
      t20 = cos(x4 * 0.3141592653589793D1)
      t21 = x3 * t15
      t25 = Sqrt(t21 * t9 * x2 * t14)
      t27 = 0.2D1 * t20 * t25
      t28 = t14 * t15 * t9 + x2 * x3 + t27
      t29 = t28 ** 2
      t30 = s * t3
      t31 = 0.1D1 / t9
      t32 = x1 * t31
      t38 = 0.1D1 - x1
      t39 = t38 * x3
      t41 = s - t30 * t32 * (t21 * t9 + x2 * t14 - t27) - t30 * t39
      t46 = t41 * t2
      t47 = t38 ** 2
      t49 = x3 ** 2
      t53 = t1 ** 2
      t54 = t4 ** 2
      t55 = t53 * t54
      t59 = t11 * t29
      t63 = t53 * t5
      t65 = t14 ** 2
      t67 = t31 * t28
      t68 = t65 * x1 * t67
      t71 = t47 * t38
      t94 = t65 * t14
      t98 = t47 ** 2
      t126 = -0.36D2 * t6 * t7 * t11 * t29 * t41 * t39 - 0.72D2 * t46 * 
     #t4 * t47 * t49 - 0.18D2 * t55 * t47 * t14 * x3 * t7 * t59 - 0.18D2
     # * t63 * t47 * t68 - 0.18D2 * t55 * t71 * t65 * t32 * t28 * x3 - 0
     #.36D2 * t6 * t32 * t28 * t41 * t47 * t49 + 0.36D2 * t46 * t4 * x1 
     #* t67 * t39 + 0.36D2 * t46 * t3 * t38 * x3 - 0.18D2 * t46 * t5 * t
     #71 * t94 + 0.18D2 * t55 * t98 * t94 * x3 + 0.18D2 * t63 * t71 * t9
     #4 - 0.18D2 * t63 * t7 * t59 * t38 * t14 + 0.36D2 * t6 * t71 * t49 
     #* x3 * t41 + 0.18D2 * t46 * t5 * t38 * t14 * t7 * t59 + 0.18D2 * t
     #46 * t5 * t47 * t68
      rrgq2qgh84J6 = -wd * t126 / t41 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
 