  
      subroutine rrqg2qght8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh81J1  
      doubleprecision rrqg2qgh81J2  
      doubleprecision rrqg2qgh81J3  
      doubleprecision rrqg2qgh81J4  
      doubleprecision rrqg2qgh81J5  
      doubleprecision rrqg2qgh81J6  
      doubleprecision rrqg2qgh82J1  
      doubleprecision rrqg2qgh82J2  
      doubleprecision rrqg2qgh82J3  
      doubleprecision rrqg2qgh82J4  
      doubleprecision rrqg2qgh82J5  
      doubleprecision rrqg2qgh82J6  
      doubleprecision rrqg2qgh82J7  
      doubleprecision rrqg2qgh83J1  
      doubleprecision rrqg2qgh83J2  
      doubleprecision rrqg2qgh83J3  
      doubleprecision rrqg2qgh83J4  
      doubleprecision rrqg2qgh83J5  
      doubleprecision rrqg2qgh83J6  
      doubleprecision rrqg2qgh83J7  
      doubleprecision rrqg2qgh84J1  
      doubleprecision rrqg2qgh84J2  
      doubleprecision rrqg2qgh84J3  
      doubleprecision rrqg2qgh84J4  
      doubleprecision rrqg2qgh84J5  
      doubleprecision rrqg2qgh84J6  
      doubleprecision rrqg2qght8s1e1  
      doubleprecision rrqg2qght8s1e0  
      doubleprecision rrqg2qght8s1em1  
      doubleprecision rrqg2qght8s1em2  
      doubleprecision rrqg2qght8s1em3  
      doubleprecision rrqg2qght8s1em4  
      doubleprecision rrqg2qght8s2e1  
      doubleprecision rrqg2qght8s2e0  
      doubleprecision rrqg2qght8s2em1  
      doubleprecision rrqg2qght8s2em2  
      doubleprecision rrqg2qght8s2em3  
      doubleprecision rrqg2qght8s2em4  
      doubleprecision rrqg2qght8s3e1  
      doubleprecision rrqg2qght8s3e0  
      doubleprecision rrqg2qght8s3em1  
      doubleprecision rrqg2qght8s3em2  
      doubleprecision rrqg2qght8s3em3  
      doubleprecision rrqg2qght8s3em4  
      doubleprecision rrqg2qght8s4e1  
      doubleprecision rrqg2qght8s4e0  
      doubleprecision rrqg2qght8s4em1  
      doubleprecision rrqg2qght8s4em2  
      doubleprecision rrqg2qght8s4em3  
      doubleprecision rrqg2qght8s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght8s1e1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 ** 2
      t25 = pi ** 2
      t27 = 0.60D2 * lh * t25
      t28 = 0.240D3 * zeta3
      t29 = lh ** 2
      t31 = 0.120D3 * t29 * lh
      t32 = t22 * t21
      t34 = 0.180D3 * t29
      t35 = 0.30D2 * t25
      t36 = t34 - t35
      t39 = (-0.90D2 * t22 * lh + t27 - t28 - t31 - 0.15D2 * t32 - t21 *
     # t36) * t3
      t40 = t5 * pi
      t41 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t42 = t40 * t41
      t49 = (0.180D3 * t21 * lh + 0.45D2 * t22 + t34 - t35) * t3
      t50 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t51 = t40 * t50
      t57 = (-0.180D3 * lh - 0.90D2 * t21) * t3
      t58 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t66 = t27 - t28 - t31
      t68 = t25 ** 2
      t69 = t29 ** 2
      t75 = t22 ** 2
      t78 = (0.30D2 * t32 * lh + t22 * t36 / 0.2D1 - t21 * t66 + t68 + 0
     #.60D2 * t69 + 0.480D3 * lh * zeta3 - 0.60D2 * t29 * t25 + 0.15D2 /
     # 0.4D1 * t75) * t3
      t79 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t80 = t40 * t79
      t83 = x1 ** 2
      t84 = x3 * t83
      t87 = log(0.4D1 * t84 * t19)
      t89 = t87 ** 2
      t92 = z * t50
      t93 = t84 * t15
      t94 = t12 * t18
      t95 = -0.1D1 + x3
      t96 = 0.1D1 / t95
      t97 = t94 * t96
      t100 = log(-0.4D1 * t93 * t97)
      t101 = t100 * z
      t103 = t100 ** 2
      t104 = t103 * z
      t108 = cos(t13)
      t110 = Sqrt(-x3 * t95)
      t115 = 0.1D1 / (-x3 - z + 0.2D1 * t108 * t110 * z)
      t121 = lh * t3
      t123 = z * t41
      t131 = t36 * t3
      t134 = -z * t79 * t115 - t79
      t138 = 0.1D1 / x3
      t140 = 0.1D1 / x1
      t143 = t83 * t15
      t146 = log(0.4D1 * t143 * t94)
      t152 = t146 ** 2
      t155 = t152 * t146
      t162 = t66 * t3
      t163 = t162 * t80
      t174 = x2 ** 2
      t175 = x3 * t174
      t176 = t175 * t83
      t179 = log(0.4D1 * t176 * t19)
      t185 = log(-0.4D1 * t176 * t16 * t18 * t96)
      t186 = t185 * z
      t195 = -t40 * t134
      t200 = 0.1D1 / x2
      t201 = t200 * t140
      t204 = t174 * t83
      t207 = log(0.4D1 * t204 * t19)
      t209 = t207 ** 2
      t221 = t131 * t80
      t232 = x3 * t15
      t235 = log(0.4D1 * t232 * t94)
      t238 = log(-0.4D1 * t232 * t97)
      t241 = -t235 - t238 * z * t115
      t244 = t238 ** 2
      t248 = t235 ** 2
      t251 = -t244 * t238 * z * t115 / 0.6D1 - t248 * t235 / 0.6D1
      t263 = 0.1D1 + z * t115
      t274 = t248 / 0.2D1 + t244 * z * t115 / 0.2D1
      t279 = t175 * t15
      t282 = log(-0.4D1 * t279 * t97)
      t283 = t282 * z
      t285 = t282 ** 2
      t286 = t285 * z
      t293 = log(0.4D1 * t175 * t19)
      t295 = t293 ** 2
      t315 = t174 * t15
      t318 = log(0.4D1 * t315 * t94)
      t324 = t318 ** 2
      t327 = t324 * t318
      t344 = t6 * pi * t7 / 0.32D2 + t39 * t42 / 0.2880D4 + t49 * t51 / 
     #0.2880D4 + t57 * t40 * t58 / 0.2880D4 + t78 * t80 / 0.2880D4 - (0.
     #90D2 * t6 * pi * (t87 * t41 - t50 - t89 * t79 / 0.2D1 - (t92 - t10
     #1 * t41 + t104 * t79 / 0.2D1) * t115) - 0.180D3 * t121 * t40 * (t8
     #7 * t79 - t41 - (t123 - t101 * t79) * t115) + t131 * t40 * t134) *
     # t138 * t140 / 0.1440D4 - (t131 * t40 * (-t41 + t146 * t79) + 0.90
     #D2 * t6 * pi * (-t58 + t146 * t50 - t152 * t41 / 0.2D1 + t155 * t7
     #9 / 0.6D1) - t163 - 0.180D3 * t121 * t40 * (-t50 + t146 * t41 - t1
     #52 * t79 / 0.2D1)) * t140 / 0.1440D4 + (0.90D2 * t6 * pi * (t41 - 
     #t179 * t79 + (t123 - t186 * t79) * t115) - 0.180D3 * t121 * t195) 
     #* t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (t50 - t207 * t41 + 
     #t209 * t79 / 0.2D1) - 0.180D3 * t121 * t40 * (t41 - t207 * t79) + 
     #t221) * t200 * t140 / 0.720D3 + ((0.90D2 * t6 * pi * t50 - 0.180D3
     # * t121 * t42 + t221) * t241 + 0.90D2 * t6 * pi * t79 * t251 + (t1
     #31 * t42 + 0.90D2 * t6 * pi * t58 + t163 - 0.180D3 * t121 * t51) *
     # t263 + (0.90D2 * t6 * pi * t41 - 0.180D3 * t121 * t80) * t274) * 
     #t138 / 0.2880D4 + (0.90D2 * t6 * pi * ((t92 - t283 * t41 + t286 * 
     #t79 / 0.2D1) * t115 + t50 - t293 * t41 + t295 * t79 / 0.2D1) - 0.1
     #80D3 * t121 * t40 * (t41 + (t123 - t283 * t79) * t115 - t293 * t79
     #) + t131 * t195) * t138 * t200 / 0.1440D4 + (t131 * t40 * (t41 - t
     #318 * t79) + 0.90D2 * t6 * pi * (t58 - t318 * t50 + t324 * t41 / 0
     #.2D1 - t327 * t79 / 0.6D1) + t163 - 0.180D3 * t121 * t40 * (t50 - 
     #t318 * t41 + t324 * t79 / 0.2D1)) * t200 / 0.1440D4
      t345 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t344)
      t347 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t349 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t350 = z * t349
      t352 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t363 = z * t347
      t374 = t352 + z * t352 * t115
      t375 = t40 * t374
      t387 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t395 = t40 * t352
      t396 = t162 * t395
      t407 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t411 = t40 * t347
      t414 = t40 * t349
      t427 = t131 * t395
      t532 = (0.90D2 * t6 * pi * (-t293 * t347 + (t350 - t283 * t347 + t
     #286 * t352 / 0.2D1) * t115 + t349 + t295 * t352 / 0.2D1) - 0.180D3
     # * t121 * t40 * (t347 + (t363 - t283 * t352) * t115 - t293 * t352)
     # + t131 * t375) * t138 * t200 / 0.1440D4 + (t131 * t40 * (t347 - t
     #318 * t352) + 0.90D2 * t6 * pi * (-t327 * t352 / 0.6D1 + t387 + t3
     #24 * t347 / 0.2D1 - t318 * t349) + t396 - 0.180D3 * t121 * t40 * (
     #t349 - t318 * t347 + t324 * t352 / 0.2D1)) * t200 / 0.1440D4 + t6 
     #* pi * t407 / 0.32D2 + t39 * t411 / 0.2880D4 + t49 * t414 / 0.2880
     #D4 + t57 * t40 * t387 / 0.2880D4 + t78 * t395 / 0.2880D4 + ((0.90D
     #2 * t6 * pi * t349 - 0.180D3 * t121 * t411 + t427) * t241 + 0.90D2
     # * t6 * pi * t352 * t251 + (t131 * t411 + 0.90D2 * t6 * pi * t387 
     #+ t396 - 0.180D3 * t121 * t414) * t263 + (0.90D2 * t6 * pi * t347 
     #- 0.180D3 * t121 * t395) * t274) * t138 / 0.2880D4 - (0.90D2 * t6 
     #* pi * (t87 * t347 - t89 * t352 / 0.2D1 - (t350 - t101 * t347 + t1
     #04 * t352 / 0.2D1) * t115 - t349) - 0.180D3 * t121 * t40 * (-t347 
     #+ t87 * t352 - (t363 - t101 * t352) * t115) - t131 * t40 * t374) *
     # t138 * t140 / 0.1440D4 - (t131 * t40 * (-t347 + t146 * t352) + 0.
     #90D2 * t6 * pi * (t155 * t352 / 0.6D1 - t387 - t152 * t347 / 0.2D1
     # + t146 * t349) - t396 - 0.180D3 * t121 * t40 * (-t349 + t146 * t3
     #47 - t152 * t352 / 0.2D1)) * t140 / 0.1440D4 + (0.90D2 * t6 * pi *
     # ((t363 - t186 * t352) * t115 + t347 - t179 * t352) - 0.180D3 * t1
     #21 * t375) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (t349 - t
     #207 * t347 + t209 * t352 / 0.2D1) - 0.180D3 * t121 * t40 * (t347 -
     # t207 * t352) + t427) * t200 * t140 / 0.720D3
      t533 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t532)
      t535 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t536 = z * t535
      t537 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t539 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t552 = z * t537
      t562 = -z * t539 * t115 - t539
      t575 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t583 = t40 * t539
      t584 = t162 * t583
      t604 = -t40 * t562
      t623 = t131 * t583
      t676 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t680 = t40 * t537
      t683 = t40 * t535
      t720 = -(0.90D2 * t6 * pi * (-t535 - (t536 - t101 * t537 + t104 * 
     #t539 / 0.2D1) * t115 - t89 * t539 / 0.2D1 + t87 * t537) - 0.180D3 
     #* t121 * t40 * (t87 * t539 - t537 - (t552 - t101 * t539) * t115) +
     # t131 * t40 * t562) * t138 * t140 / 0.1440D4 - (t131 * t40 * (-t53
     #7 + t146 * t539) + 0.90D2 * t6 * pi * (t155 * t539 / 0.6D1 - t575 
     #+ t146 * t535 - t152 * t537 / 0.2D1) - t584 - 0.180D3 * t121 * t40
     # * (t146 * t537 - t152 * t539 / 0.2D1 - t535)) * t140 / 0.1440D4 +
     # (0.90D2 * t6 * pi * ((t552 - t186 * t539) * t115 + t537 - t179 * 
     #t539) - 0.180D3 * t121 * t604) * t138 * t201 / 0.720D3 + (0.90D2 *
     # t6 * pi * (-t207 * t537 + t209 * t539 / 0.2D1 + t535) - 0.180D3 *
     # t121 * t40 * (t537 - t207 * t539) + t623) * t200 * t140 / 0.720D3
     # + (0.90D2 * t6 * pi * ((t536 - t283 * t537 + t286 * t539 / 0.2D1)
     # * t115 + t535 - t293 * t537 + t295 * t539 / 0.2D1) - 0.180D3 * t1
     #21 * t40 * ((t552 - t283 * t539) * t115 - t293 * t539 + t537) + t1
     #31 * t604) * t138 * t200 / 0.1440D4 + (t131 * t40 * (t537 - t318 *
     # t539) + 0.90D2 * t6 * pi * (-t327 * t539 / 0.6D1 + t575 - t318 * 
     #t535 + t324 * t537 / 0.2D1) + t584 - 0.180D3 * t121 * t40 * (-t318
     # * t537 + t324 * t539 / 0.2D1 + t535)) * t200 / 0.1440D4 + t6 * pi
     # * t676 / 0.32D2 + t39 * t680 / 0.2880D4 + t49 * t683 / 0.2880D4 +
     # t57 * t40 * t575 / 0.2880D4 + ((0.90D2 * t6 * pi * t535 - 0.180D3
     # * t121 * t680 + t623) * t241 + 0.90D2 * t6 * pi * t539 * t251 + (
     #t131 * t680 + 0.90D2 * t6 * pi * t575 + t584 - 0.180D3 * t121 * t6
     #83) * t263 + (0.90D2 * t6 * pi * t537 - 0.180D3 * t121 * t583) * t
     #274) * t138 / 0.2880D4 + t78 * t583 / 0.2880D4
      t721 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t720)
      t723 = t2 * x1
      t724 = -0.1D1 + x1
      t725 = x1 * z
      t726 = 0.1D1 - x1 + t725
      t727 = 0.1D1 / t726
      t729 = t2 * t724 * t727
      t730 = s * t17
      t732 = t724 * x1 * t727
      t733 = t730 * t732
      t734 = t724 ** 2
      t735 = t727 * t734
      t736 = t94 * t735
      t739 = log(0.4D1 * t93 * t736)
      t740 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, t
     #723, 0.0D0, -t733)
      t742 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, t
     #723, 0.0D0, -t733)
      t745 = t18 * t727
      t750 = log(-0.4D1 * t84 * t16 * t745 * t734 * t96)
      t751 = t750 * z
      t753 = t750 ** 2
      t754 = t753 * z
      t755 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, t
     #723, 0.0D0, -t733)
      t760 = x3 * x1
      t761 = t760 * z
      t762 = 0.3D1 * t761
      t763 = x1 * t11
      t764 = x3 * t11
      t765 = t764 * x1
      t767 = 0.2D1 * t84 * z
      t768 = t84 * t11
      t769 = x3 * t726
      t771 = Sqrt(-t769 * t95)
      t775 = 0.2D1 * t760
      t776 = -z + t725 - t762 - t763 + t765 + t767 - t768 - t84 + 0.2D1 
     #* t108 * t771 * z + t775 - x3
      t777 = 0.1D1 / t776
      t779 = t739 ** 2
      t787 = z * t740
      t797 = t726 * t777
      t799 = t755 + z * t755 * t797
      t806 = t143 * t12
      t807 = t745 * t734
      t810 = log(0.4D1 * t806 * t807)
      t815 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, t
     #723, 0.0D0, -t733)
      t817 = t810 ** 2
      t820 = t817 * t810
      t827 = t40 * t755
      t839 = t175 * t143
      t844 = log(-0.4D1 * t839 * t94 * t735 * t96)
      t845 = t844 * z
      t852 = log(0.4D1 * t839 * t736)
      t866 = t204 * t15
      t869 = log(0.4D1 * t866 * t736)
      t870 = t869 ** 2
      t888 = -(0.90D2 * t6 * pi * (-t739 * t740 + t742 + (z * t742 - t75
     #1 * t740 + t754 * t755 / 0.2D1) * t726 * t777 + t779 * t755 / 0.2D
     #1) - 0.180D3 * t121 * t40 * (-t739 * t755 + (t787 - t751 * t755) *
     # t726 * t777 + t740) + t131 * t40 * t799) * t138 * t140 / 0.1440D4
     # - (t131 * t40 * (t740 - t810 * t755) + 0.90D2 * t6 * pi * (t815 -
     # t810 * t742 + t817 * t740 / 0.2D1 - t820 * t755 / 0.6D1) + t162 *
     # t827 - 0.180D3 * t121 * t40 * (t817 * t755 / 0.2D1 + t742 - t810 
     #* t740)) * t140 / 0.1440D4 + (0.90D2 * t6 * pi * (-t740 - (t787 - 
     #t845 * t755) * t726 * t777 + t852 * t755) + 0.180D3 * t121 * t40 *
     # t799) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (-t870 * t755
     # / 0.2D1 - t742 + t869 * t740) - 0.180D3 * t121 * t40 * (-t740 + t
     #869 * t755) - t131 * t827) * t200 * t140 / 0.720D3
      t889 = FJET(XB1, XB2, s, 0.0D0, t723, -t729, 0.0D0, -t733, t888)
      t891 = x2 * s
      t892 = t891 * t1
      t893 = -0.1D1 + x2
      t894 = t893 * s
      t895 = t894 * t1
      t896 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0.
     #0D0, 0.0D0, 0.0D0)
      t897 = t94 * t893
      t900 = log(-0.4D1 * t279 * t897)
      t901 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0.
     #0D0, 0.0D0, 0.0D0)
      t903 = t900 ** 2
      t904 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0.
     #0D0, 0.0D0, 0.0D0)
      t916 = t40 * t904
      t917 = t131 * t916
      t924 = log(-0.4D1 * t315 * t897)
      t929 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0.
     #0D0, 0.0D0, 0.0D0)
      t931 = t924 ** 2
      t934 = t931 * t924
      t952 = t18 * t893
      t956 = log(-0.4D1 * t176 * t16 * t952)
      t970 = log(-0.4D1 * t866 * t897)
      t971 = t970 ** 2
      t988 = (0.90D2 * t6 * pi * (-t896 + t900 * t901 - t903 * t904 / 0.
     #2D1) - 0.180D3 * t121 * t40 * (-t901 + t900 * t904) - t917) * t138
     # * t200 / 0.1440D4 + (t131 * t40 * (-t901 + t924 * t904) + 0.90D2 
     #* t6 * pi * (-t929 + t924 * t896 - t931 * t901 / 0.2D1 + t934 * t9
     #04 / 0.6D1) - t162 * t916 - 0.180D3 * t121 * t40 * (-t931 * t904 /
     # 0.2D1 - t896 + t924 * t901)) * t200 / 0.1440D4 + (0.90D2 * t6 * p
     #i * (-t901 + t956 * t904) + 0.180D3 * t121 * t916) * t138 * t201 /
     # 0.720D3 + (0.90D2 * t6 * pi * (-t971 * t904 / 0.2D1 + t970 * t901
     # - t896) - 0.180D3 * t121 * t40 * (-t901 + t970 * t904) - t917) * 
     #t200 * t140 / 0.720D3
      t989 = FJET(XB1, XB2, s, 0.0D0, t892, 0.0D0, -t895, 0.0D0, t988)
      t991 = x2 * x3
      t994 = Sqrt(x3 * t893 * t95)
      t995 = t108 * t994
      t997 = 0.2D1 * t995 * x2
      t999 = 0.1D1 - x3 + t991
      t1000 = 0.1D1 / t999
      t1002 = t2 * (0.1D1 - x3 - x2 + t991 + t175 + t997) * t1000
      t1007 = t2 * x2 * (-0.1D1 + t991 + 0.2D1 * t995) * t1000
      t1008 = x2 * z
      t1009 = t1008 - x2 - z
      t1010 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1011 = t1009 * t1010
      t1012 = t893 * t95
      t1013 = t999 ** 2
      t1014 = 0.1D1 / t1013
      t1015 = t1012 * t1014
      t1019 = log(0.4D1 * t839 * t94 * t1015)
      t1020 = t1019 * t1009
      t1021 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1025 = t991 * z
      t1026 = t175 * z
      t1032 = 0.1D1 / (x3 - t175 - t1008 - t1025 + t1026 + x2 + z - t997
     # - 0.2D1 * t995 * z + 0.2D1 * t995 * t1008)
      t1036 = t121 * t5
      t1037 = pi * t1009
      t1039 = t1037 * t1021 * t1032
      t1046 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1053 = log(0.4D1 * t175 * t16 * t952 * t95 * t1014)
      t1054 = t1053 * t1009
      t1056 = t1053 ** 2
      t1057 = t1056 * t1009
      t1071 = t131 * t5
      t1077 = (-0.90D2 * t6 * pi * (t1011 - t1020 * t1021) * t1032 + 0.1
     #80D3 * t1036 * t1039) * t138 * t201 / 0.720D3 + (-0.90D2 * t6 * pi
     # * (t1009 * t1046 - t1054 * t1010 + t1057 * t1021 / 0.2D1) * t1032
     # + 0.180D3 * t1036 * pi * (t1011 - t1054 * t1021) * t1032 - t1071 
     #* t1039) * t138 * t200 / 0.1440D4
      t1078 = FJET(XB1, XB2, s, 0.0D0, t1002, 0.0D0, -t1007, 0.0D0, t107
     #7)
      t1080 = t1 * t724
      t1082 = t894 * t1080 * t727
      t1083 = t891 * t1080
      t1085 = t730 * t893 * t732
      t1086 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1091 = log(-0.4D1 * t839 * t94 * t735 * t893)
      t1092 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1098 = t40 * t1092
      t1104 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1110 = log(-0.4D1 * t204 * t16 * t745 * t734 * t893)
      t1111 = t1110 ** 2
      t1129 = (0.90D2 * t6 * pi * (t1086 - t1091 * t1092) - 0.180D3 * t1
     #21 * t1098) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (t1104 +
     # t1111 * t1092 / 0.2D1 - t1110 * t1086) - 0.180D3 * t121 * t40 * (
     #-t1110 * t1092 + t1086) + t131 * t1098) * t200 * t140 / 0.720D3
      t1130 = FJET(XB1, XB2, s, 0.0D0, t1082, t723, -t1083, t1085, t1129
     #)
      t1132 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1133 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1139 = t40 * t1133
      t1149 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1159 = t131 * t1139
      t1184 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1205 = (0.90D2 * t6 * pi * (-t1132 + t956 * t1133) + 0.180D3 * t1
     #21 * t1139) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (t970 * 
     #t1132 - t971 * t1133 / 0.2D1 - t1149) - 0.180D3 * t121 * t40 * (t9
     #70 * t1133 - t1132) - t1159) * t200 * t140 / 0.720D3 + (0.90D2 * t
     #6 * pi * (-t1149 - t903 * t1133 / 0.2D1 + t900 * t1132) - 0.180D3 
     #* t121 * t40 * (t900 * t1133 - t1132) - t1159) * t138 * t200 / 0.1
     #440D4 + (t131 * t40 * (-t1132 + t924 * t1133) + 0.90D2 * t6 * pi *
     # (-t1184 - t931 * t1132 / 0.2D1 + t924 * t1149 + t934 * t1133 / 0.
     #6D1) - t162 * t1139 - 0.180D3 * t121 * t40 * (-t1149 - t931 * t113
     #3 / 0.2D1 + t924 * t1132)) * t200 / 0.1440D4
      t1206 = FJET(XB1, XB2, s, 0.0D0, -t895, 0.0D0, t892, 0.0D0, t1205)
      t1208 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1210 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1213 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1225 = z * t1208
      t1237 = t1213 + z * t1213 * t797
      t1250 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1258 = t40 * t1213
      t1304 = -(0.90D2 * t6 * pi * (-t739 * t1208 + t1210 + (z * t1210 -
     # t751 * t1208 + t754 * t1213 / 0.2D1) * t726 * t777 + t779 * t1213
     # / 0.2D1) - 0.180D3 * t121 * t40 * ((t1225 - t751 * t1213) * t726 
     #* t777 - t739 * t1213 + t1208) + t131 * t40 * t1237) * t138 * t140
     # / 0.1440D4 - (t131 * t40 * (-t810 * t1213 + t1208) + 0.90D2 * t6 
     #* pi * (t817 * t1208 / 0.2D1 + t1250 - t810 * t1210 - t820 * t1213
     # / 0.6D1) + t162 * t1258 - 0.180D3 * t121 * t40 * (t1210 + t817 * 
     #t1213 / 0.2D1 - t810 * t1208)) * t140 / 0.1440D4 + (0.90D2 * t6 * 
     #pi * (-(t1225 - t845 * t1213) * t726 * t777 + t852 * t1213 - t1208
     #) + 0.180D3 * t121 * t40 * t1237) * t138 * t201 / 0.720D3 + (0.90D
     #2 * t6 * pi * (-t870 * t1213 / 0.2D1 + t869 * t1208 - t1210) - 0.1
     #80D3 * t121 * t40 * (-t1208 + t869 * t1213) - t131 * t1258) * t200
     # * t140 / 0.720D3
      t1305 = FJET(XB1, XB2, s, 0.0D0, -t729, t723, 0.0D0, -t733, t1304)
      t1307 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1309 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1311 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1319 = t1009 * t1309
      t1327 = t1037 * t1311 * t1032
      t1345 = (-0.90D2 * t6 * pi * (t1009 * t1307 - t1054 * t1309 + t105
     #7 * t1311 / 0.2D1) * t1032 + 0.180D3 * t1036 * pi * (t1319 - t1054
     # * t1311) * t1032 - t1071 * t1327) * t138 * t200 / 0.1440D4 + (-0.
     #90D2 * t6 * pi * (t1319 - t1020 * t1311) * t1032 + 0.180D3 * t1036
     # * t1327) * t138 * t201 / 0.720D3
      t1346 = FJET(XB1, XB2, s, 0.0D0, -t1007, 0.0D0, t1002, 0.0D0, t134
     #5)
      t1348 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1352 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1353 = t40 * t1352
      t1356 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1357 = t40 * t1356
      t1358 = t131 * t1357
      t1366 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1370 = t162 * t1357
      t1371 = t40 * t1348
      t1388 = z * t1348
      t1402 = z * t1352
      t1412 = -t1356 - z * t1356 * t115
      t1451 = -t40 * t1412
      t1526 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1533 = ((0.90D2 * t6 * pi * t1348 - 0.180D3 * t121 * t1353 + t135
     #8) * t241 + 0.90D2 * t6 * pi * t1356 * t251 + (t131 * t1353 + 0.90
     #D2 * t6 * pi * t1366 + t1370 - 0.180D3 * t121 * t1371) * t263 + (0
     #.90D2 * t6 * pi * t1352 - 0.180D3 * t121 * t1357) * t274) * t138 /
     # 0.2880D4 + t78 * t1357 / 0.2880D4 - (0.90D2 * t6 * pi * (-(t1388 
     #- t101 * t1352 + t104 * t1356 / 0.2D1) * t115 - t89 * t1356 / 0.2D
     #1 + t87 * t1352 - t1348) - 0.180D3 * t121 * t40 * (-t1352 + t87 * 
     #t1356 - (t1402 - t101 * t1356) * t115) + t131 * t40 * t1412) * t13
     #8 * t140 / 0.1440D4 - (t131 * t40 * (-t1352 + t146 * t1356) + 0.90
     #D2 * t6 * pi * (t155 * t1356 / 0.6D1 + t146 * t1348 - t1366 - t152
     # * t1352 / 0.2D1) - t1370 - 0.180D3 * t121 * t40 * (t146 * t1352 -
     # t1348 - t152 * t1356 / 0.2D1)) * t140 / 0.1440D4 + (0.90D2 * t6 *
     # pi * ((t1402 - t186 * t1356) * t115 + t1352 - t179 * t1356) - 0.1
     #80D3 * t121 * t1451) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi *
     # (-t207 * t1352 + t209 * t1356 / 0.2D1 + t1348) - 0.180D3 * t121 *
     # t40 * (-t207 * t1356 + t1352) + t1358) * t200 * t140 / 0.720D3 + 
     #t39 * t1353 / 0.2880D4 + t49 * t1371 / 0.2880D4 + (0.90D2 * t6 * p
     #i * ((t1388 - t283 * t1352 + t286 * t1356 / 0.2D1) * t115 + t1348 
     #+ t295 * t1356 / 0.2D1 - t293 * t1352) - 0.180D3 * t121 * t40 * ((
     #t1402 - t283 * t1356) * t115 - t293 * t1356 + t1352) + t131 * t145
     #1) * t138 * t200 / 0.1440D4 + (t131 * t40 * (t1352 - t318 * t1356)
     # + 0.90D2 * t6 * pi * (-t318 * t1348 - t327 * t1356 / 0.6D1 + t136
     #6 + t324 * t1352 / 0.2D1) + t1370 - 0.180D3 * t121 * t40 * (-t318 
     #* t1352 + t1348 + t324 * t1356 / 0.2D1)) * t200 / 0.1440D4 + t6 * 
     #pi * t1526 / 0.32D2 + t57 * t40 * t1366 / 0.2880D4
      t1534 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1533)
      t1536 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1538 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1540 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1554 = z * t1538
      t1565 = z * t1540 * t797 + t1540
      t1576 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t1586 = t40 * t1540
      t1632 = -(0.90D2 * t6 * pi * ((z * t1536 - t751 * t1538 + t754 * t
     #1540 / 0.2D1) * t726 * t777 + t1536 - t739 * t1538 + t779 * t1540 
     #/ 0.2D1) - 0.180D3 * t121 * t40 * (t1538 - t739 * t1540 + (t1554 -
     # t751 * t1540) * t726 * t777) + t131 * t40 * t1565) * t138 * t140 
     #/ 0.1440D4 - (t131 * t40 * (t1538 - t810 * t1540) + 0.90D2 * t6 * 
     #pi * (t1576 - t810 * t1536 + t817 * t1538 / 0.2D1 - t820 * t1540 /
     # 0.6D1) + t162 * t1586 - 0.180D3 * t121 * t40 * (-t810 * t1538 + t
     #817 * t1540 / 0.2D1 + t1536)) * t140 / 0.1440D4 + (0.90D2 * t6 * p
     #i * (-(t1554 - t845 * t1540) * t726 * t777 - t1538 + t852 * t1540)
     # + 0.180D3 * t121 * t40 * t1565) * t138 * t201 / 0.720D3 + (0.90D2
     # * t6 * pi * (t869 * t1538 - t1536 - t870 * t1540 / 0.2D1) - 0.180
     #D3 * t121 * t40 * (-t1538 + t869 * t1540) - t131 * t1586) * t200 *
     # t140 / 0.720D3
      t1633 = FJET(XB1, XB2, s, t723, 0.0D0, 0.0D0, -t729, -t733, t1632)
      t1635 = t345 * t344 + t533 * t532 + t721 * t720 + t889 * t888 + t9
     #89 * t988 + t1078 * t1077 + t1130 * t1129 + t1206 * t1205 + t1305 
     #* t1304 + t1346 * t1345 + t1534 * t1533 + t1633 * t1632
      t1636 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1638 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1643 = t40 * t1636
      t1652 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1667 = (0.90D2 * t6 * pi * (-t1091 * t1636 + t1638) - 0.180D3 * t
     #121 * t1643) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (-t1110
     # * t1638 + t1111 * t1636 / 0.2D1 + t1652) - 0.180D3 * t121 * t40 *
     # (-t1110 * t1636 + t1638) + t131 * t1643) * t200 * t140 / 0.720D3
      t1668 = FJET(XB1, XB2, s, t723, -t1083, 0.0D0, t1082, t1085, t1667
     #)
      t1670 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1671 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1677 = t40 * t1671
      t1687 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1697 = t131 * t1677
      t1724 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1743 = (0.90D2 * t6 * pi * (-t1670 + t956 * t1671) + 0.180D3 * t1
     #21 * t1677) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (-t971 *
     # t1671 / 0.2D1 + t970 * t1670 - t1687) - 0.180D3 * t121 * t40 * (-
     #t1670 + t970 * t1671) - t1697) * t200 * t140 / 0.720D3 + (0.90D2 *
     # t6 * pi * (t900 * t1670 - t1687 - t903 * t1671 / 0.2D1) - 0.180D3
     # * t121 * t40 * (-t1670 + t900 * t1671) - t1697) * t138 * t200 / 0
     #.1440D4 + (t131 * t40 * (-t1670 + t924 * t1671) + 0.90D2 * t6 * pi
     # * (-t931 * t1670 / 0.2D1 - t1724 + t924 * t1687 + t934 * t1671 / 
     #0.6D1) - t162 * t1677 - 0.180D3 * t121 * t40 * (t924 * t1670 - t16
     #87 - t931 * t1671 / 0.2D1)) * t200 / 0.1440D4
      t1744 = FJET(XB1, XB2, s, t892, 0.0D0, -t895, 0.0D0, 0.0D0, t1743)
      t1746 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1747 = t1009 * t1746
      t1748 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1756 = t1037 * t1748 * t1032
      t1763 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t1784 = (-0.90D2 * t6 * pi * (t1747 - t1020 * t1748) * t1032 + 0.1
     #80D3 * t1036 * t1756) * t138 * t201 / 0.720D3 + (-0.90D2 * t6 * pi
     # * (t1009 * t1763 - t1054 * t1746 + t1057 * t1748 / 0.2D1) * t1032
     # + 0.180D3 * t1036 * pi * (t1747 - t1054 * t1748) * t1032 - t1071 
     #* t1756) * t138 * t200 / 0.1440D4
      t1785 = FJET(XB1, XB2, s, t1002, 0.0D0, -t1007, 0.0D0, 0.0D0, t178
     #4)
      t1787 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1789 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1794 = t40 * t1787
      t1800 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t1818 = (0.90D2 * t6 * pi * (-t1091 * t1787 + t1789) - 0.180D3 * t
     #121 * t1794) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (t1800 
     #- t1110 * t1789 + t1111 * t1787 / 0.2D1) - 0.180D3 * t121 * t40 * 
     #(-t1110 * t1787 + t1789) + t131 * t1794) * t200 * t140 / 0.720D3
      t1819 = FJET(XB1, XB2, s, t1082, 0.0D0, -t1083, t723, t1085, t1818
     #)
      t1822 = t723 * t991 * t1000
      t1823 = t2 * t724
      t1824 = t175 * x1
      t1826 = Sqrt(t769 * t1012)
      t1827 = t108 * t1826
      t1829 = 0.2D1 * t1827 * x2
      t1830 = t175 * t725
      t1834 = t1823 * (-t1824 - x2 + t991 + t1829 + 0.1D1 - x3 + t175 + 
     #t1830) * t727 * t1000
      t1838 = t95 * s * t1 * x1 * t1000
      t1844 = t1823 * x2 * (-0.1D1 + t991 + x1 - t760 - t725 + t761 + 0.
     #2D1 * t1827) * t727 * t1000
      t1847 = x2 * x1
      t1855 = t11 * x2
      t1857 = t83 * x2
      t1860 = t1847 * z
      t1869 = t1830 - 0.2D1 * t1827 * t1008 - 0.2D1 * t1827 * t1847 + 0.
     #2D1 * t991 * t725 - t764 * t1847 - 0.2D1 * t84 * t1008 + t84 * t18
     #55 - x3 - x2 + 0.2D1 * t1857 * z - 0.3D1 * t1860 - t991 * x1 + t84
     # * x2 - t83 * t11 * x2 + t1855 * x1 + 0.2D1 * t1827 * z - t762
      t1873 = t765 + t767 - t768 + t1025 - t1026 + t1829 + t175 + t1008 
     #- t1824 + t725 - t763 - t84 + t775 + 0.2D1 * t1827 * t1860 - t1857
     # + 0.2D1 * t1847 - z
      t1875 = 0.1D1 / (t1869 + t1873)
      t1876 = t726 * t1875
      t1877 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t1883 = log(0.4D1 * t175 * t806 * t807 * t1015)
      t1884 = t1883 * t726
      t1885 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t1890 = z - t1008 + t1860 + x2 - t1847
      t1894 = t121 * t40
      t1899 = 0.90D2 * t6 * pi * (t1876 * t1877 - t1884 * t1875 * t1885)
     # * t1890 - 0.180D3 * t1894 * t1876 * t1885 * t1890
      t1903 = FJET(XB1, XB2, s, t1822, -t1834, -t1838, t1844, t1085, t18
     #99 * t138 * t201 / 0.720D3)
      t1906 = t138 * t200 * t140
      t1909 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t1911 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t1923 = 0.90D2 * t6 * pi * (t1876 * t1909 - t1884 * t1875 * t1911)
     # * t1890 - 0.180D3 * t1894 * t1876 * t1911 * t1890
      t1927 = FJET(XB1, XB2, s, t1844, -t1838, -t1834, t1822, t1085, t19
     #23 * t138 * t201 / 0.720D3)
      t1931 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1933 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1938 = t40 * t1931
      t1946 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t1958 = t131 * t1938
      t1983 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t892, -t895, 0
     #.0D0, 0.0D0, 0.0D0)
      t2004 = (0.90D2 * t6 * pi * (t956 * t1931 - t1933) + 0.180D3 * t12
     #1 * t1938) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (t970 * t
     #1933 - t1946 - t971 * t1931 / 0.2D1) - 0.180D3 * t121 * t40 * (-t1
     #933 + t970 * t1931) - t1958) * t200 * t140 / 0.720D3 + (0.90D2 * t
     #6 * pi * (-t903 * t1931 / 0.2D1 - t1946 + t900 * t1933) - 0.180D3 
     #* t121 * t40 * (t900 * t1931 - t1933) - t1958) * t138 * t200 / 0.1
     #440D4 + (t131 * t40 * (-t1933 + t924 * t1931) + 0.90D2 * t6 * pi *
     # (-t1983 + t924 * t1946 + t934 * t1931 / 0.6D1 - t931 * t1933 / 0.
     #2D1) - t162 * t1938 - 0.180D3 * t121 * t40 * (t924 * t1933 - t1946
     # - t931 * t1931 / 0.2D1)) * t200 / 0.1440D4
      t2005 = FJET(XB1, XB2, s, -t895, 0.0D0, t892, 0.0D0, 0.0D0, t2004)
      t2007 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t2009 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t2012 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t2024 = z * t2007
      t2036 = z * t2012 * t797 + t2012
      t2052 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t729, 
     #t723, 0.0D0, -t733)
      t2057 = t40 * t2012
      t2103 = -(0.90D2 * t6 * pi * (-t739 * t2007 + t2009 + (z * t2009 -
     # t751 * t2007 + t754 * t2012 / 0.2D1) * t726 * t777 + t779 * t2012
     # / 0.2D1) - 0.180D3 * t121 * t40 * ((t2024 - t751 * t2012) * t726 
     #* t777 + t2007 - t739 * t2012) + t131 * t40 * t2036) * t138 * t140
     # / 0.1440D4 - (t131 * t40 * (t2007 - t810 * t2012) + 0.90D2 * t6 *
     # pi * (-t810 * t2009 + t817 * t2007 / 0.2D1 - t820 * t2012 / 0.6D1
     # + t2052) + t162 * t2057 - 0.180D3 * t121 * t40 * (-t810 * t2007 +
     # t2009 + t817 * t2012 / 0.2D1)) * t140 / 0.1440D4 + (0.90D2 * t6 *
     # pi * (-t2007 + t852 * t2012 - (t2024 - t845 * t2012) * t726 * t77
     #7) + 0.180D3 * t121 * t40 * t2036) * t138 * t201 / 0.720D3 + (0.90
     #D2 * t6 * pi * (-t870 * t2012 / 0.2D1 + t869 * t2007 - t2009) - 0.
     #180D3 * t121 * t40 * (t869 * t2012 - t2007) - t131 * t2057) * t200
     # * t140 / 0.720D3
      t2104 = FJET(XB1, XB2, s, -t729, 0.0D0, 0.0D0, t723, -t733, t2103)
      t2106 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t2108 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t2113 = t40 * t2106
      t2120 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t1083, t1082,
     # t723, 0.0D0, t1085)
      t2137 = (0.90D2 * t6 * pi * (-t1091 * t2106 + t2108) - 0.180D3 * t
     #121 * t2113) * t138 * t201 / 0.720D3 + (0.90D2 * t6 * pi * (-t1110
     # * t2108 + t2120 + t1111 * t2106 / 0.2D1) - 0.180D3 * t121 * t40 *
     # (t2108 - t1110 * t2106) + t131 * t2113) * t200 * t140 / 0.720D3
      t2138 = FJET(XB1, XB2, s, -t1083, t723, t1082, 0.0D0, t1085, t2137
     #)
      t2140 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t2141 = t1009 * t2140
      t2142 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t2150 = t1037 * t2142 * t1032
      t2157 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t1007, t1002,
     # 0.0D0, 0.0D0, 0.0D0)
      t2178 = (-0.90D2 * t6 * pi * (t2141 - t1020 * t2142) * t1032 + 0.1
     #80D3 * t1036 * t2150) * t138 * t201 / 0.720D3 + (-0.90D2 * t6 * pi
     # * (t1009 * t2157 - t1054 * t2140 + t1057 * t2142 / 0.2D1) * t1032
     # + 0.180D3 * t1036 * pi * (t2141 - t1054 * t2142) * t1032 - t1071 
     #* t2150) * t138 * t200 / 0.1440D4
      t2179 = FJET(XB1, XB2, s, -t1007, 0.0D0, t1002, 0.0D0, 0.0D0, t217
     #8)
      t2181 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t2183 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t2195 = 0.90D2 * t6 * pi * (t1876 * t2181 - t1884 * t1875 * t2183)
     # * t1890 - 0.180D3 * t1894 * t1876 * t2183 * t1890
      t2199 = FJET(XB1, XB2, s, -t1838, t1844, t1822, -t1834, t1085, t21
     #95 * t138 * t201 / 0.720D3)
      t2203 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t2205 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1844, -t1834,
     # -t1838, t1822, t1085)
      t2217 = 0.90D2 * t6 * pi * (t1876 * t2203 - t1884 * t1875 * t2205)
     # * t1890 - 0.180D3 * t1894 * t1876 * t2205 * t1890
      t2221 = FJET(XB1, XB2, s, -t1834, t1822, t1844, -t1838, t1085, t22
     #17 * t138 * t201 / 0.720D3)
      t2225 = t1668 * t1667 + t1744 * t1743 + t1785 * t1784 + t1819 * t1
     #818 + t1903 * t1899 * t1906 / 0.720D3 + t1927 * t1923 * t1906 / 0.
     #720D3 + t2005 * t2004 + t2104 * t2103 + t2138 * t2137 + t2179 * t2
     #178 + t2199 * t2195 * t1906 / 0.720D3 + t2221 * t2217 * t1906 / 0.
     #720D3
      rrqg2qght8s1e1 = t1635 + t2225

      end function



      doubleprecision function rrqg2qght8s1e0
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t12 * t17)
      t21 = t20 ** 2
      t22 = -0.1D1 + x3
      t23 = 0.1D1 / t22
      t24 = t17 * t23
      t27 = log(-0.4D1 * t12 * t24)
      t28 = t27 ** 2
      t30 = cos(t9)
      t32 = Sqrt(-x3 * t22)
      t37 = 0.1D1 / (-x3 - z + 0.2D1 * t30 * t32 * z)
      t40 = t21 / 0.2D1 + t28 * z * t37 / 0.2D1
      t44 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t48 = lh * t3
      t49 = t5 * pi
      t50 = t49 * t7
      t52 = 0.180D3 * t48 * t50
      t56 = -t20 - t27 * z * t37
      t58 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t62 = t49 * t44
      t65 = lh ** 2
      t66 = 0.180D3 * t65
      t67 = pi ** 2
      t68 = 0.30D2 * t67
      t69 = t66 - t68
      t70 = t69 * t3
      t71 = t70 * t50
      t74 = 0.1D1 + z * t37
      t77 = 0.1D1 / x3
      t80 = t14 * t11
      t81 = t80 * t16
      t83 = log(0.4D1 * t81)
      t86 = t83 ** 2
      t89 = (0.180D3 * t83 * lh + 0.45D2 * t86 + t66 - t68) * t3
      t92 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t107 = (-0.90D2 * t86 * lh + 0.60D2 * lh * t67 - 0.240D3 * zeta3 -
     # 0.120D3 * t65 * lh - 0.15D2 * t86 * t83 - t83 * t69) * t3
      t113 = (-0.180D3 * lh - 0.90D2 * t83) * t3
      t117 = z * t44
      t118 = x2 ** 2
      t119 = x3 * t118
      t120 = t119 * t11
      t123 = log(-0.4D1 * t120 * t24)
      t124 = t123 * z
      t130 = log(0.4D1 * t119 * t81)
      t138 = t7 + z * t7 * t37
      t144 = 0.1D1 / x2
      t147 = t118 * t11
      t150 = log(0.4D1 * t147 * t17)
      t152 = t150 ** 2
      t167 = x1 ** 2
      t168 = x3 * t167
      t171 = log(0.4D1 * t168 * t81)
      t173 = t168 * t11
      t176 = log(-0.4D1 * t173 * t24)
      t177 = t176 * z
      t191 = 0.1D1 / x1
      t194 = t6 * pi
      t196 = t144 * t191
      t200 = t118 * t167
      t203 = log(0.4D1 * t200 * t81)
      t213 = t167 * t11
      t216 = log(0.4D1 * t213 * t17)
      t218 = t216 ** 2
      t233 = (0.90D2 * t6 * pi * t7 * t40 + (0.90D2 * t6 * pi * t44 - t5
     #2) * t56 + (0.90D2 * t6 * pi * t58 - 0.180D3 * t48 * t62 + t71) * 
     #t74) * t77 / 0.2880D4 + t89 * t62 / 0.2880D4 + t6 * pi * t92 / 0.3
     #2D2 + t107 * t50 / 0.2880D4 + t113 * t49 * t58 / 0.2880D4 + (0.90D
     #2 * t6 * pi * (t44 + (t117 - t124 * t7) * t37 - t130 * t7) - 0.180
     #D3 * t48 * t49 * t138) * t77 * t144 / 0.1440D4 + (0.90D2 * t6 * pi
     # * (t58 - t150 * t44 + t152 * t7 / 0.2D1) - 0.180D3 * t48 * t49 * 
     #(t44 - t150 * t7) + t71) * t144 / 0.1440D4 - (0.90D2 * t6 * pi * (
     #t171 * t7 - t44 - (t117 - t177 * t7) * t37) + 0.180D3 * t48 * t49 
     #* t138) * t77 * t191 / 0.1440D4 + t194 * t138 * t77 * t196 / 0.8D1
     # + (0.90D2 * t6 * pi * (t44 - t203 * t7) - t52) * t144 * t191 / 0.
     #720D3 - (0.90D2 * t6 * pi * (-t58 + t216 * t44 - t218 * t7 / 0.2D1
     #) - 0.180D3 * t48 * t49 * (-t44 + t216 * t7) - t71) * t191 / 0.144
     #0D4
      t234 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t233)
      t236 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t241 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t245 = t49 * t236
      t247 = 0.180D3 * t48 * t245
      t250 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t254 = t49 * t241
      t257 = t70 * t245
      t265 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t274 = z * t241
      t285 = t236 + z * t236 * t37
      t352 = (0.90D2 * t6 * pi * t236 * t40 + (0.90D2 * t6 * pi * t241 -
     # t247) * t56 + (0.90D2 * t6 * pi * t250 - 0.180D3 * t48 * t254 + t
     #257) * t74) * t77 / 0.2880D4 + t89 * t254 / 0.2880D4 + t6 * pi * t
     #265 / 0.32D2 + t107 * t245 / 0.2880D4 + t113 * t49 * t250 / 0.2880
     #D4 + (0.90D2 * t6 * pi * (t241 + (t274 - t124 * t236) * t37 - t130
     # * t236) - 0.180D3 * t48 * t49 * t285) * t77 * t144 / 0.1440D4 + (
     #0.90D2 * t6 * pi * (t250 - t150 * t241 + t152 * t236 / 0.2D1) - 0.
     #180D3 * t48 * t49 * (t241 - t150 * t236) + t257) * t144 / 0.1440D4
     # - (0.90D2 * t6 * pi * (-t241 + t171 * t236 - (t274 - t177 * t236)
     # * t37) + 0.180D3 * t48 * t49 * t285) * t77 * t191 / 0.1440D4 + t1
     #94 * t285 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi * (t241 - t203 
     #* t236) - t247) * t144 * t191 / 0.720D3 - (0.90D2 * t6 * pi * (-t2
     #50 + t216 * t241 - t218 * t236 / 0.2D1) - 0.180D3 * t48 * t49 * (-
     #t241 + t216 * t236) - t257) * t191 / 0.1440D4
      t353 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t352)
      t355 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t360 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t364 = t49 * t355
      t366 = 0.180D3 * t48 * t364
      t369 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t373 = t49 * t360
      t376 = t70 * t364
      t384 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t393 = z * t360
      t404 = t355 + z * t355 * t37
      t471 = (0.90D2 * t6 * pi * t355 * t40 + (0.90D2 * t6 * pi * t360 -
     # t366) * t56 + (0.90D2 * t6 * pi * t369 - 0.180D3 * t48 * t373 + t
     #376) * t74) * t77 / 0.2880D4 + t89 * t373 / 0.2880D4 + t6 * pi * t
     #384 / 0.32D2 + t107 * t364 / 0.2880D4 + t113 * t49 * t369 / 0.2880
     #D4 + (0.90D2 * t6 * pi * ((t393 - t124 * t355) * t37 - t130 * t355
     # + t360) - 0.180D3 * t48 * t49 * t404) * t77 * t144 / 0.1440D4 + (
     #0.90D2 * t6 * pi * (-t150 * t360 + t152 * t355 / 0.2D1 + t369) - 0
     #.180D3 * t48 * t49 * (t360 - t150 * t355) + t376) * t144 / 0.1440D
     #4 - (0.90D2 * t6 * pi * (t171 * t355 - t360 - (t393 - t177 * t355)
     # * t37) + 0.180D3 * t48 * t49 * t404) * t77 * t191 / 0.1440D4 + t1
     #94 * t404 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi * (t360 - t203 
     #* t355) - t366) * t144 * t191 / 0.720D3 - (0.90D2 * t6 * pi * (t21
     #6 * t360 - t218 * t355 / 0.2D1 - t369) - 0.180D3 * t48 * t49 * (-t
     #360 + t216 * t355) - t376) * t191 / 0.1440D4
      t472 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t471)
      t474 = t2 * x1
      t475 = -0.1D1 + x1
      t476 = x1 * z
      t477 = 0.1D1 - x1 + t476
      t478 = 0.1D1 / t477
      t480 = t2 * t475 * t478
      t481 = s * t15
      t483 = t475 * x1 * t478
      t484 = t481 * t483
      t485 = t475 ** 2
      t487 = t17 * t478 * t485
      t490 = log(0.4D1 * t173 * t487)
      t491 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t493 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t496 = t16 * t478
      t501 = log(-0.4D1 * t168 * t80 * t496 * t485 * t23)
      t502 = t501 * z
      t506 = x3 * x1
      t507 = t506 * z
      t508 = 0.3D1 * t507
      t509 = x1 * t13
      t510 = x3 * t13
      t511 = t510 * x1
      t513 = 0.2D1 * t168 * z
      t514 = t168 * t13
      t515 = x3 * t477
      t517 = Sqrt(-t515 * t22)
      t521 = 0.2D1 * t506
      t522 = -z + t476 - t508 - t509 + t511 + t513 - t514 - t168 + 0.2D1
     # * t30 * t517 * z + t521 - x3
      t523 = 0.1D1 / t522
      t530 = t477 * t523
      t532 = t491 + z * t491 * t530
      t545 = t200 * t11
      t548 = log(0.4D1 * t545 * t487)
      t554 = t49 * t491
      t565 = log(0.4D1 * t213 * t14 * t496 * t485)
      t566 = t565 ** 2
      t569 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t584 = -(0.90D2 * t6 * pi * (-t490 * t491 + (z * t493 - t502 * t49
     #1) * t477 * t523 + t493) - 0.180D3 * t48 * t49 * t532) * t77 * t19
     #1 / 0.1440D4 - t194 * t532 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * p
     #i * (-t493 + t548 * t491) + 0.180D3 * t48 * t554) * t144 * t191 / 
     #0.720D3 - (0.90D2 * t6 * pi * (t566 * t491 / 0.2D1 + t569 - t565 *
     # t493) - 0.180D3 * t48 * t49 * (t493 - t565 * t491) + t70 * t554) 
     #* t191 / 0.1440D4
      t585 = FJET(XB1, XB2, s, 0.0D0, t474, -t480, 0.0D0, -t484, t584)
      t587 = x2 * s
      t588 = t587 * t1
      t589 = -0.1D1 + x2
      t590 = t589 * s
      t591 = t590 * t1
      t592 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0.
     #0D0, 0.0D0, 0.0D0)
      t593 = t17 * t589
      t596 = log(-0.4D1 * t120 * t593)
      t597 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0.
     #0D0, 0.0D0, 0.0D0)
      t603 = t49 * t597
      t605 = 0.180D3 * t48 * t603
      t612 = log(-0.4D1 * t147 * t593)
      t613 = t612 ** 2
      t616 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0.
     #0D0, 0.0D0, 0.0D0)
      t637 = log(-0.4D1 * t545 * t593)
      t647 = (0.90D2 * t6 * pi * (-t592 + t596 * t597) + t605) * t77 * t
     #144 / 0.1440D4 + (0.90D2 * t6 * pi * (-t613 * t597 / 0.2D1 - t616 
     #+ t612 * t592) - 0.180D3 * t48 * t49 * (-t592 + t612 * t597) - t70
     # * t603) * t144 / 0.1440D4 - t194 * t597 * t77 * t196 / 0.8D1 + (0
     #.90D2 * t6 * pi * (-t592 + t637 * t597) + t605) * t144 * t191 / 0.
     #720D3
      t648 = FJET(XB1, XB2, s, 0.0D0, t588, 0.0D0, -t591, 0.0D0, t647)
      t650 = x2 * x3
      t653 = Sqrt(x3 * t589 * t22)
      t654 = t30 * t653
      t656 = 0.2D1 * t654 * x2
      t658 = 0.1D1 - x3 + t650
      t659 = 0.1D1 / t658
      t661 = t2 * (0.1D1 - x3 - x2 + t650 + t119 + t656) * t659
      t666 = t2 * x2 * (-0.1D1 + t650 + 0.2D1 * t654) * t659
      t667 = x2 * z
      t668 = t667 - x2 - z
      t669 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0.
     #0D0, 0.0D0, 0.0D0)
      t673 = t658 ** 2
      t679 = log(0.4D1 * t119 * t80 * t16 * t589 * t22 / t673)
      t680 = t679 * t668
      t681 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0.
     #0D0, 0.0D0, 0.0D0)
      t685 = t650 * z
      t686 = t119 * z
      t692 = 0.1D1 / (x3 - t119 - t667 - t685 + t686 + x2 + z - t656 - 0
     #.2D1 * t654 * z + 0.2D1 * t654 * t667)
      t696 = t48 * t5
      t697 = pi * t668
      t698 = t681 * t692
      t706 = t6 * t697
      t708 = t77 * t144 * t191
      t712 = (-0.90D2 * t6 * pi * (t668 * t669 - t680 * t681) * t692 + 0
     #.180D3 * t696 * t697 * t698) * t77 * t144 / 0.1440D4 - t706 * t698
     # * t708 / 0.8D1
      t713 = FJET(XB1, XB2, s, 0.0D0, t661, 0.0D0, -t666, 0.0D0, t712)
      t715 = t1 * t475
      t717 = t590 * t715 * t478
      t718 = t587 * t715
      t720 = t481 * t589 * t483
      t721 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t4
     #74, 0.0D0, t720)
      t731 = log(-0.4D1 * t200 * t80 * t496 * t485 * t589)
      t733 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t4
     #74, 0.0D0, t720)
      t745 = t194 * t721 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi * (-t7
     #31 * t721 + t733) - 0.180D3 * t48 * t49 * t721) * t144 * t191 / 0.
     #720D3
      t746 = FJET(XB1, XB2, s, 0.0D0, t717, t474, -t718, t720, t745)
      t748 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0.
     #0D0, 0.0D0, 0.0D0)
      t750 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0.
     #0D0, 0.0D0, 0.0D0)
      t755 = t49 * t748
      t757 = 0.180D3 * t48 * t755
      t762 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0.
     #0D0, 0.0D0, 0.0D0)
      t792 = (0.90D2 * t6 * pi * (t596 * t748 - t750) + t757) * t77 * t1
     #44 / 0.1440D4 + (0.90D2 * t6 * pi * (-t762 - t613 * t748 / 0.2D1 +
     # t612 * t750) - 0.180D3 * t48 * t49 * (-t750 + t612 * t748) - t70 
     #* t755) * t144 / 0.1440D4 - t194 * t748 * t77 * t196 / 0.8D1 + (0.
     #90D2 * t6 * pi * (t637 * t748 - t750) + t757) * t144 * t191 / 0.72
     #0D3
      t793 = FJET(XB1, XB2, s, 0.0D0, -t591, 0.0D0, t588, 0.0D0, t792)
      t795 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t797 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t809 = t797 + z * t797 * t530
      t827 = t49 * t797
      t834 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t851 = -(0.90D2 * t6 * pi * ((z * t795 - t502 * t797) * t477 * t52
     #3 - t490 * t797 + t795) - 0.180D3 * t48 * t49 * t809) * t77 * t191
     # / 0.1440D4 - t194 * t809 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi
     # * (-t795 + t548 * t797) + 0.180D3 * t48 * t827) * t144 * t191 / 0
     #.720D3 - (0.90D2 * t6 * pi * (t834 + t566 * t797 / 0.2D1 - t565 * 
     #t795) - 0.180D3 * t48 * t49 * (-t565 * t797 + t795) + t70 * t827) 
     #* t191 / 0.1440D4
      t852 = FJET(XB1, XB2, s, 0.0D0, -t480, t474, 0.0D0, -t484, t851)
      t854 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0.
     #0D0, 0.0D0, 0.0D0)
      t856 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0.
     #0D0, 0.0D0, 0.0D0)
      t863 = t856 * t692
      t874 = (-0.90D2 * t6 * pi * (t668 * t854 - t680 * t856) * t692 + 0
     #.180D3 * t696 * t697 * t863) * t77 * t144 / 0.1440D4 - t706 * t863
     # * t708 / 0.8D1
      t875 = FJET(XB1, XB2, s, 0.0D0, -t666, 0.0D0, t661, 0.0D0, t874)
      t877 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t882 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t886 = t49 * t877
      t888 = 0.180D3 * t48 * t886
      t891 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t895 = t49 * t882
      t898 = t70 * t886
      t904 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t913 = z * t882
      t924 = t877 + z * t877 * t37
      t993 = (0.90D2 * t6 * pi * t877 * t40 + (0.90D2 * t6 * pi * t882 -
     # t888) * t56 + (0.90D2 * t6 * pi * t891 - 0.180D3 * t48 * t895 + t
     #898) * t74) * t77 / 0.2880D4 + t6 * pi * t904 / 0.32D2 + t113 * t4
     #9 * t891 / 0.2880D4 + t89 * t895 / 0.2880D4 + (0.90D2 * t6 * pi * 
     #((t913 - t124 * t877) * t37 - t130 * t877 + t882) - 0.180D3 * t48 
     #* t49 * t924) * t77 * t144 / 0.1440D4 + (0.90D2 * t6 * pi * (-t150
     # * t882 + t891 + t152 * t877 / 0.2D1) - 0.180D3 * t48 * t49 * (t88
     #2 - t150 * t877) + t898) * t144 / 0.1440D4 + t107 * t886 / 0.2880D
     #4 - (0.90D2 * t6 * pi * (-t882 + t171 * t877 - (t913 - t177 * t877
     #) * t37) + 0.180D3 * t48 * t49 * t924) * t77 * t191 / 0.1440D4 + t
     #194 * t924 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi * (-t203 * t87
     #7 + t882) - t888) * t144 * t191 / 0.720D3 - (0.90D2 * t6 * pi * (t
     #216 * t882 - t891 - t218 * t877 / 0.2D1) - 0.180D3 * t48 * t49 * (
     #-t882 + t216 * t877) - t898) * t191 / 0.1440D4
      t994 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t993)
      t996 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t997 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #474, 0.0D0, -t484)
      t1010 = z * t997 * t530 + t997
      t1028 = t49 * t997
      t1038 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, 
     #t474, 0.0D0, -t484)
      t1052 = -(0.90D2 * t6 * pi * (t996 - t490 * t997 + (z * t996 - t50
     #2 * t997) * t477 * t523) - 0.180D3 * t48 * t49 * t1010) * t77 * t1
     #91 / 0.1440D4 - t194 * t1010 * t77 * t196 / 0.8D1 + (0.90D2 * t6 *
     # pi * (-t996 + t548 * t997) + 0.180D3 * t48 * t1028) * t144 * t191
     # / 0.720D3 - (0.90D2 * t6 * pi * (-t565 * t996 + t566 * t997 / 0.2
     #D1 + t1038) - 0.180D3 * t48 * t49 * (t996 - t565 * t997) + t70 * t
     #1028) * t191 / 0.1440D4
      t1053 = FJET(XB1, XB2, s, t474, 0.0D0, 0.0D0, -t480, -t484, t1052)
      t1055 = t234 * t233 + t353 * t352 + t472 * t471 + t585 * t584 + t6
     #48 * t647 + t713 * t712 + t746 * t745 + t793 * t792 + t852 * t851 
     #+ t875 * t874 + t994 * t993 + t1053 * t1052
      t1056 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t
     #474, 0.0D0, t720)
      t1062 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t
     #474, 0.0D0, t720)
      t1074 = t194 * t1056 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi * (-
     #t731 * t1056 + t1062) - 0.180D3 * t48 * t49 * t1056) * t144 * t191
     # / 0.720D3
      t1075 = FJET(XB1, XB2, s, t474, -t718, 0.0D0, t717, t720, t1074)
      t1077 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0
     #.0D0, 0.0D0, 0.0D0)
      t1078 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0
     #.0D0, 0.0D0, 0.0D0)
      t1084 = t49 * t1078
      t1086 = 0.180D3 * t48 * t1084
      t1092 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0
     #.0D0, 0.0D0, 0.0D0)
      t1121 = (0.90D2 * t6 * pi * (-t1077 + t596 * t1078) + t1086) * t77
     # * t144 / 0.1440D4 + (0.90D2 * t6 * pi * (t612 * t1077 - t1092 - t
     #613 * t1078 / 0.2D1) - 0.180D3 * t48 * t49 * (-t1077 + t612 * t107
     #8) - t70 * t1084) * t144 / 0.1440D4 - t194 * t1078 * t77 * t196 / 
     #0.8D1 + (0.90D2 * t6 * pi * (-t1077 + t637 * t1078) + t1086) * t14
     #4 * t191 / 0.720D3
      t1122 = FJET(XB1, XB2, s, t588, 0.0D0, -t591, 0.0D0, 0.0D0, t1121)
      t1124 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0
     #.0D0, 0.0D0, 0.0D0)
      t1126 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0
     #.0D0, 0.0D0, 0.0D0)
      t1133 = t1126 * t692
      t1144 = (-0.90D2 * t6 * pi * (t668 * t1124 - t680 * t1126) * t692 
     #+ 0.180D3 * t696 * t697 * t1133) * t77 * t144 / 0.1440D4 - t706 * 
     #t1133 * t708 / 0.8D1
      t1145 = FJET(XB1, XB2, s, t661, 0.0D0, -t666, 0.0D0, 0.0D0, t1144)
      t1147 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t
     #474, 0.0D0, t720)
      t1153 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t
     #474, 0.0D0, t720)
      t1165 = t194 * t1147 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi * (-
     #t731 * t1147 + t1153) - 0.180D3 * t48 * t49 * t1147) * t144 * t191
     # / 0.720D3
      t1166 = FJET(XB1, XB2, s, t717, 0.0D0, -t718, t474, t720, t1165)
      t1169 = t474 * t650 * t659
      t1170 = t2 * t475
      t1171 = t119 * x1
      t1174 = Sqrt(t515 * t589 * t22)
      t1175 = t30 * t1174
      t1177 = 0.2D1 * t1175 * x2
      t1178 = t119 * t476
      t1182 = t1170 * (-t1171 - x2 + t650 + t1177 + 0.1D1 - x3 + t119 + 
     #t1178) * t478 * t659
      t1186 = t22 * s * t1 * x1 * t659
      t1192 = t1170 * x2 * (-0.1D1 + t650 + x1 - t506 - t476 + t507 + 0.
     #2D1 * t1175) * t478 * t659
      t1194 = x2 * x1
      t1195 = t1194 * z
      t1198 = t167 * x2
      t1206 = t13 * x2
      t1210 = -x2 + 0.2D1 * t1175 * t1195 - t1171 + t1177 + 0.2D1 * t119
     #8 * z - 0.3D1 * t1195 - t650 * x1 + t168 * x2 - t167 * t13 * x2 + 
     #t1206 * x1 + 0.2D1 * t1175 * z + t119 + t667 + t476 - t509 - t168 
     #+ t521
      t1222 = -t508 + t511 + t513 - t514 + t685 - t686 + t1178 - 0.2D1 *
     # t1175 * t667 - 0.2D1 * t1175 * t1194 + 0.2D1 * t650 * t476 - t510
     # * t1194 - 0.2D1 * t168 * t667 + t168 * t1206 - t1198 + 0.2D1 * t1
     #194 - z - x3
      t1224 = 0.1D1 / (t1210 + t1222)
      t1226 = t6 * pi * t477 * t1224
      t1227 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1192, -t1182,
     # -t1186, t1169, t720)
      t1228 = z - t667 + t1195 + x2 - t1194
      t1233 = FJET(XB1, XB2, s, t1169, -t1182, -t1186, t1192, t720, t122
     #6 * t1227 * t1228 * t708 / 0.8D1)
      t1235 = t49 * t477
      t1242 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1192, -t1182,
     # -t1186, t1169, t720)
      t1247 = FJET(XB1, XB2, s, t1192, -t1186, -t1182, t1169, t720, t122
     #6 * t1242 * t1228 * t708 / 0.8D1)
      t1255 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0
     #.0D0, 0.0D0, 0.0D0)
      t1257 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0
     #.0D0, 0.0D0, 0.0D0)
      t1262 = t49 * t1255
      t1264 = 0.180D3 * t48 * t1262
      t1270 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t588, -t591, 0
     #.0D0, 0.0D0, 0.0D0)
      t1299 = (0.90D2 * t6 * pi * (t596 * t1255 - t1257) + t1264) * t77 
     #* t144 / 0.1440D4 + (0.90D2 * t6 * pi * (t612 * t1257 - t1270 - t6
     #13 * t1255 / 0.2D1) - 0.180D3 * t48 * t49 * (-t1257 + t612 * t1255
     #) - t70 * t1262) * t144 / 0.1440D4 - t194 * t1255 * t77 * t196 / 0
     #.8D1 + (0.90D2 * t6 * pi * (-t1257 + t637 * t1255) + t1264) * t144
     # * t191 / 0.720D3
      t1300 = FJET(XB1, XB2, s, -t591, 0.0D0, t588, 0.0D0, 0.0D0, t1299)
      t1302 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, 
     #t474, 0.0D0, -t484)
      t1304 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, 
     #t474, 0.0D0, -t484)
      t1316 = z * t1304 * t530 + t1304
      t1334 = t49 * t1304
      t1342 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, 
     #t474, 0.0D0, -t484)
      t1358 = -(0.90D2 * t6 * pi * ((z * t1302 - t502 * t1304) * t477 * 
     #t523 + t1302 - t490 * t1304) - 0.180D3 * t48 * t49 * t1316) * t77 
     #* t191 / 0.1440D4 - t194 * t1316 * t77 * t196 / 0.8D1 + (0.90D2 * 
     #t6 * pi * (t548 * t1304 - t1302) + 0.180D3 * t48 * t1334) * t144 *
     # t191 / 0.720D3 - (0.90D2 * t6 * pi * (-t565 * t1302 + t1342 + t56
     #6 * t1304 / 0.2D1) - 0.180D3 * t48 * t49 * (t1302 - t565 * t1304) 
     #+ t70 * t1334) * t191 / 0.1440D4
      t1359 = FJET(XB1, XB2, s, -t480, 0.0D0, 0.0D0, t474, -t484, t1358)
      t1361 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t
     #474, 0.0D0, t720)
      t1366 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t718, t717, t
     #474, 0.0D0, t720)
      t1379 = t194 * t1361 * t77 * t196 / 0.8D1 + (0.90D2 * t6 * pi * (t
     #1366 - t731 * t1361) - 0.180D3 * t48 * t49 * t1361) * t144 * t191 
     #/ 0.720D3
      t1380 = FJET(XB1, XB2, s, -t718, t474, t717, 0.0D0, t720, t1379)
      t1382 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0
     #.0D0, 0.0D0, 0.0D0)
      t1384 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t666, t661, 0
     #.0D0, 0.0D0, 0.0D0)
      t1391 = t1384 * t692
      t1402 = (-0.90D2 * t6 * pi * (t668 * t1382 - t680 * t1384) * t692 
     #+ 0.180D3 * t696 * t697 * t1391) * t77 * t144 / 0.1440D4 - t706 * 
     #t1391 * t708 / 0.8D1
      t1403 = FJET(XB1, XB2, s, -t666, 0.0D0, t661, 0.0D0, 0.0D0, t1402)
      t1405 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1192, -t1182,
     # -t1186, t1169, t720)
      t1410 = FJET(XB1, XB2, s, -t1186, t1192, t1169, -t1182, t720, t122
     #6 * t1405 * t1228 * t708 / 0.8D1)
      t1418 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1192, -t1182,
     # -t1186, t1169, t720)
      t1423 = FJET(XB1, XB2, s, -t1182, t1169, t1192, -t1186, t720, t122
     #6 * t1418 * t1228 * t708 / 0.8D1)
      t1431 = t1075 * t1074 + t1122 * t1121 + t1145 * t1144 + t1166 * t1
     #165 + t1233 * t3 * t1235 * t1224 * t1227 * t1228 * t708 / 0.8D1 + 
     #t1247 * t3 * t1235 * t1224 * t1242 * t1228 * t708 / 0.8D1 + t1300 
     #* t1299 + t1359 * t1358 + t1380 * t1379 + t1403 * t1402 + t1410 * 
     #t3 * t1235 * t1224 * t1405 * t1228 * t708 / 0.8D1 + t1423 * t3 * t
     #1235 * t1224 * t1418 * t1228 * t708 / 0.8D1
      rrqg2qght8s1e0 = t1055 + t1431

      end function



      doubleprecision function rrqg2qght8s1em1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t12 * t17)
      t21 = -0.1D1 + x3
      t26 = log(-0.4D1 * t12 * t17 / t21)
      t28 = cos(t9)
      t30 = Sqrt(-x3 * t21)
      t35 = 0.1D1 / (-x3 - z + 0.2D1 * t28 * t30 * z)
      t37 = -t20 - t26 * z * t35
      t41 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t45 = lh * t3
      t46 = t5 * pi
      t47 = t46 * t7
      t49 = 0.180D3 * t45 * t47
      t52 = 0.1D1 + z * t35
      t55 = 0.1D1 / x3
      t58 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t66 = log(0.4D1 * t14 * t11 * t16)
      t69 = (-0.180D3 * lh - 0.90D2 * t66) * t3
      t75 = t66 ** 2
      t77 = lh ** 2
      t79 = pi ** 2
      t82 = (0.180D3 * t66 * lh + 0.45D2 * t75 + 0.180D3 * t77 - 0.30D2 
     #* t79) * t3
      t85 = t6 * pi
      t88 = t7 + z * t7 * t35
      t90 = 0.1D1 / x2
      t94 = x2 ** 2
      t95 = t94 * t11
      t98 = log(0.4D1 * t95 * t17)
      t108 = 0.1D1 / x1
      t112 = x1 ** 2
      t113 = t112 * t11
      t116 = log(0.4D1 * t113 * t17)
      t130 = (0.90D2 * t6 * pi * t7 * t37 + (0.90D2 * t6 * pi * t41 - t4
     #9) * t52) * t55 / 0.2880D4 + t6 * pi * t58 / 0.32D2 + t69 * t46 * 
     #t41 / 0.2880D4 + t82 * t47 / 0.2880D4 + t85 * t88 * t55 * t90 / 0.
     #16D2 + (0.90D2 * t6 * pi * (t41 - t98 * t7) - t49) * t90 / 0.1440D
     #4 + t85 * t7 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * pi * (-t41 + t1
     #16 * t7) + t49) * t108 / 0.1440D4 + t85 * t88 * t55 * t108 / 0.16D
     #2
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t130)
      t133 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t138 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t142 = t46 * t133
      t144 = 0.180D3 * t45 * t142
      t150 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t161 = t133 + z * t133 * t35
      t191 = (0.90D2 * t6 * pi * t133 * t37 + (0.90D2 * t6 * pi * t138 -
     # t144) * t52) * t55 / 0.2880D4 + t6 * pi * t150 / 0.32D2 + t69 * t
     #46 * t138 / 0.2880D4 + t82 * t142 / 0.2880D4 + t85 * t161 * t55 * 
     #t90 / 0.16D2 + (0.90D2 * t6 * pi * (t138 - t98 * t133) - t144) * t
     #90 / 0.1440D4 + t85 * t133 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * p
     #i * (-t138 + t116 * t133) + t144) * t108 / 0.1440D4 + t85 * t161 *
     # t55 * t108 / 0.16D2
      t192 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t191)
      t194 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t199 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t203 = t46 * t194
      t205 = 0.180D3 * t45 * t203
      t211 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t222 = t194 + z * t194 * t35
      t252 = (0.90D2 * t6 * pi * t194 * t37 + (0.90D2 * t6 * pi * t199 -
     # t205) * t52) * t55 / 0.2880D4 + t6 * pi * t211 / 0.32D2 + t69 * t
     #46 * t199 / 0.2880D4 + t82 * t203 / 0.2880D4 + t85 * t222 * t55 * 
     #t90 / 0.16D2 + (0.90D2 * t6 * pi * (t199 - t98 * t194) - t205) * t
     #90 / 0.1440D4 + t85 * t194 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * p
     #i * (-t199 + t116 * t194) + t205) * t108 / 0.1440D4 + t85 * t222 *
     # t55 * t108 / 0.16D2
      t253 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t252)
      t255 = t2 * x1
      t256 = -0.1D1 + x1
      t257 = x1 * z
      t258 = 0.1D1 - x1 + t257
      t259 = 0.1D1 / t258
      t261 = t2 * t256 * t259
      t262 = s * t15
      t264 = t256 * x1 * t259
      t265 = t262 * t264
      t266 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t271 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t274 = t256 ** 2
      t278 = log(0.4D1 * t113 * t14 * t16 * t259 * t274)
      t291 = x3 * x1
      t297 = x3 * t112
      t303 = Sqrt(-x3 * t258 * t21)
      t308 = -z + t257 - 0.3D1 * t291 * z - x1 * t13 + x3 * t13 * x1 + 0
     #.2D1 * t297 * z - t297 * t13 - t297 + 0.2D1 * t28 * t303 * z + 0.2
     #D1 * t291 - x3
      t310 = t258 / t308
      t317 = -t85 * t266 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * pi * (t27
     #1 - t278 * t266) - 0.180D3 * t45 * t46 * t266) * t108 / 0.1440D4 -
     # t85 * (t266 + z * t266 * t310) * t55 * t108 / 0.16D2
      t318 = FJET(XB1, XB2, s, 0.0D0, t255, -t261, 0.0D0, -t265, t317)
      t320 = x2 * s
      t321 = t320 * t1
      t322 = -0.1D1 + x2
      t323 = t322 * s
      t324 = t323 * t1
      t325 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t330 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t334 = log(-0.4D1 * t95 * t17 * t322)
      t350 = -t85 * t325 * t55 * t90 / 0.16D2 + (0.90D2 * t6 * pi * (-t3
     #30 + t334 * t325) + 0.180D3 * t45 * t46 * t325) * t90 / 0.1440D4 -
     # t85 * t325 * t90 * t108 / 0.8D1
      t351 = FJET(XB1, XB2, s, 0.0D0, t321, 0.0D0, -t324, 0.0D0, t350)
      t353 = x2 * x3
      t354 = t94 * x3
      t357 = Sqrt(x3 * t322 * t21)
      t358 = t28 * t357
      t360 = 0.2D1 * t358 * x2
      t363 = 0.1D1 / (0.1D1 - x3 + t353)
      t365 = t2 * (0.1D1 - x3 - x2 + t353 + t354 + t360) * t363
      t370 = t2 * x2 * (-0.1D1 + t353 + 0.2D1 * t358) * t363
      t371 = x2 * z
      t372 = t371 - x2 - z
      t374 = t6 * pi * t372
      t375 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t370, t365, 0.
     #0D0, 0.0D0, 0.0D0)
      t383 = 0.1D1 / (x3 - t354 - t371 - t353 * z + t354 * z + x2 + z - 
     #t360 - 0.2D1 * t358 * z + 0.2D1 * t358 * t371)
      t385 = t55 * t90
      t389 = FJET(XB1, XB2, s, 0.0D0, t365, 0.0D0, -t370, 0.0D0, -t374 *
     # t375 * t383 * t385 / 0.16D2)
      t394 = t383 * t55 * t90
      t398 = t1 * t256
      t400 = t323 * t398 * t259
      t401 = t320 * t398
      t403 = t262 * t322 * t264
      t404 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t401, t400, t2
     #55, 0.0D0, t403)
      t409 = FJET(XB1, XB2, s, 0.0D0, t400, t255, -t401, t403, t85 * t40
     #4 * t90 * t108 / 0.8D1)
      t413 = t90 * t108
      t417 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t422 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t438 = -t85 * t417 * t55 * t90 / 0.16D2 + (0.90D2 * t6 * pi * (-t4
     #22 + t334 * t417) + 0.180D3 * t45 * t46 * t417) * t90 / 0.1440D4 -
     # t85 * t417 * t90 * t108 / 0.8D1
      t439 = FJET(XB1, XB2, s, 0.0D0, -t324, 0.0D0, t321, 0.0D0, t438)
      t441 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t447 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t465 = -t85 * t441 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * pi * (-t2
     #78 * t441 + t447) - 0.180D3 * t45 * t46 * t441) * t108 / 0.1440D4 
     #- t85 * (t441 + z * t441 * t310) * t55 * t108 / 0.16D2
      t466 = FJET(XB1, XB2, s, 0.0D0, -t261, t255, 0.0D0, -t265, t465)
      t468 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t370, t365, 0.
     #0D0, 0.0D0, 0.0D0)
      t473 = FJET(XB1, XB2, s, 0.0D0, -t370, 0.0D0, t365, 0.0D0, -t374 *
     # t468 * t383 * t385 / 0.16D2)
      t480 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t484 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t485 = t46 * t484
      t492 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t497 = 0.180D3 * t45 * t485
      t505 = t484 + z * t484 * t35
      t538 = t6 * pi * t480 / 0.32D2 + t82 * t485 / 0.2880D4 + (0.90D2 *
     # t6 * pi * t484 * t37 + (0.90D2 * t6 * pi * t492 - t497) * t52) * 
     #t55 / 0.2880D4 + t85 * t505 * t55 * t90 / 0.16D2 + (0.90D2 * t6 * 
     #pi * (t492 - t98 * t484) - t497) * t90 / 0.1440D4 + t69 * t46 * t4
     #92 / 0.2880D4 + t85 * t484 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * p
     #i * (-t492 + t116 * t484) + t497) * t108 / 0.1440D4 + t85 * t505 *
     # t55 * t108 / 0.16D2
      t539 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t538)
      t541 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t546 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t565 = -t85 * t541 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * pi * (t54
     #6 - t278 * t541) - 0.180D3 * t45 * t46 * t541) * t108 / 0.1440D4 -
     # t85 * (z * t541 * t310 + t541) * t55 * t108 / 0.16D2
      t566 = FJET(XB1, XB2, s, t255, 0.0D0, 0.0D0, -t261, -t265, t565)
      t568 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t401, t400, t2
     #55, 0.0D0, t403)
      t573 = FJET(XB1, XB2, s, t255, -t401, 0.0D0, t400, t403, t85 * t56
     #8 * t90 * t108 / 0.8D1)
      t580 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t585 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t601 = -t85 * t580 * t55 * t90 / 0.16D2 + (0.90D2 * t6 * pi * (-t5
     #85 + t334 * t580) + 0.180D3 * t45 * t46 * t580) * t90 / 0.1440D4 -
     # t85 * t580 * t90 * t108 / 0.8D1
      t602 = FJET(XB1, XB2, s, t321, 0.0D0, -t324, 0.0D0, 0.0D0, t601)
      t604 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t370, t365, 0.
     #0D0, 0.0D0, 0.0D0)
      t609 = FJET(XB1, XB2, s, t365, 0.0D0, -t370, 0.0D0, 0.0D0, -t374 *
     # t604 * t383 * t385 / 0.16D2)
      t616 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t401, t400, t2
     #55, 0.0D0, t403)
      t621 = FJET(XB1, XB2, s, t400, 0.0D0, -t401, t255, t403, t85 * t61
     #6 * t90 * t108 / 0.8D1)
      t628 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t633 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t321, -t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t649 = -t85 * t628 * t55 * t90 / 0.16D2 + (0.90D2 * t6 * pi * (-t6
     #33 + t334 * t628) + 0.180D3 * t45 * t46 * t628) * t90 / 0.1440D4 -
     # t85 * t628 * t90 * t108 / 0.8D1
      t650 = FJET(XB1, XB2, s, -t324, 0.0D0, t321, 0.0D0, 0.0D0, t649)
      t652 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t657 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t261, t
     #255, 0.0D0, -t265)
      t676 = -t85 * t652 * t90 * t108 / 0.8D1 - (0.90D2 * t6 * pi * (t65
     #7 - t278 * t652) - 0.180D3 * t45 * t46 * t652) * t108 / 0.1440D4 -
     # t85 * (z * t652 * t310 + t652) * t55 * t108 / 0.16D2
      t677 = FJET(XB1, XB2, s, -t261, 0.0D0, 0.0D0, t255, -t265, t676)
      t679 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t370, t365, 0.
     #0D0, 0.0D0, 0.0D0)
      t684 = FJET(XB1, XB2, s, -t370, 0.0D0, t365, 0.0D0, 0.0D0, -t374 *
     # t679 * t383 * t385 / 0.16D2)
      t691 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t401, t400, t2
     #55, 0.0D0, t403)
      t696 = FJET(XB1, XB2, s, -t401, t255, t400, 0.0D0, t403, t85 * t69
     #1 * t90 * t108 / 0.8D1)
      rrqg2qght8s1em1 = t131 * t130 + t192 * t191 + t253 * t252 + t318 *
     # t317 + t351 * t350 - t389 * t3 * t46 * t372 * t375 * t394 / 0.16D
     #2 + t409 * t3 * t5 * pi * t404 * t413 / 0.8D1 + t439 * t438 + t466
     # * t465 - t473 * t3 * t46 * t372 * t468 * t394 / 0.16D2 + t539 * t
     #538 + t566 * t565 + t573 * t3 * t5 * pi * t568 * t413 / 0.8D1 + t6
     #02 * t601 - t609 * t3 * t46 * t372 * t604 * t394 / 0.16D2 + t621 *
     # t3 * t5 * pi * t616 * t413 / 0.8D1 + t650 * t649 + t677 * t676 - 
     #t684 * t3 * t46 * t372 * t679 * t394 / 0.16D2 + t696 * t3 * t5 * p
     #i * t691 * t413 / 0.8D1

      end function



      doubleprecision function rrqg2qght8s1em2
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = t6 * pi
      t8 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = cos(t9)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t20 = 0.1D1 + z / (-x3 - z + 0.2D1 * t10 * t13 * z)
      t22 = 0.1D1 / x3
      t26 = pi * t8
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t35 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t40 = z ** 2
      t42 = Sin(t9)
      t43 = t42 ** 2
      t45 = t1 ** 2
      t46 = t45 ** 2
      t49 = log(0.4D1 / t40 * t43 * t46)
      t52 = (-0.180D3 * lh - 0.90D2 * t49) * t3
      t53 = t5 * pi
      t57 = t7 * t8 * t20 * t22 / 0.32D2 + t6 * t26 * t27 / 0.16D2 + t6 
     #* t26 * t31 / 0.16D2 + t6 * pi * t35 / 0.32D2 + t52 * t53 * t8 / 0
     #.2880D4
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t57)
      t60 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t65 = pi * t60
      t72 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t79 = t7 * t60 * t20 * t22 / 0.32D2 + t6 * t65 * t27 / 0.16D2 + t6
     # * t65 * t31 / 0.16D2 + t6 * pi * t72 / 0.32D2 + t52 * t53 * t60 /
     # 0.2880D4
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t79)
      t82 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t87 = pi * t82
      t94 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t101 = t7 * t82 * t20 * t22 / 0.32D2 + t6 * t87 * t27 / 0.16D2 + t
     #6 * t87 * t31 / 0.16D2 + t6 * pi * t94 / 0.32D2 + t52 * t53 * t82 
     #/ 0.2880D4
      t102 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t101)
      t104 = t2 * x1
      t105 = -0.1D1 + x1
      t108 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t110 = t2 * t105 * t108
      t114 = s * t45 * t105 * x1 * t108
      t115 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, t
     #104, 0.0D0, -t114)
      t117 = pi * t115 * t31
      t120 = FJET(XB1, XB2, s, 0.0D0, t104, -t110, 0.0D0, -t114, -t6 * t
     #117 / 0.16D2)
      t126 = x2 * s * t1
      t129 = (-0.1D1 + x2) * s * t1
      t130 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t129, 0.
     #0D0, 0.0D0, 0.0D0)
      t132 = pi * t130 * t27
      t135 = FJET(XB1, XB2, s, 0.0D0, t126, 0.0D0, -t129, 0.0D0, -t6 * t
     #132 / 0.16D2)
      t140 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t129, 0.
     #0D0, 0.0D0, 0.0D0)
      t142 = pi * t140 * t27
      t145 = FJET(XB1, XB2, s, 0.0D0, -t129, 0.0D0, t126, 0.0D0, -t6 * t
     #142 / 0.16D2)
      t150 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, t
     #104, 0.0D0, -t114)
      t152 = pi * t150 * t31
      t155 = FJET(XB1, XB2, s, 0.0D0, -t110, t104, 0.0D0, -t114, -t6 * t
     #152 / 0.16D2)
      t160 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t165 = pi * t160
      t172 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t179 = t7 * t160 * t20 * t22 / 0.32D2 + t6 * t165 * t27 / 0.16D2 +
     # t6 * t165 * t31 / 0.16D2 + t6 * pi * t172 / 0.32D2 + t52 * t53 * 
     #t160 / 0.2880D4
      t180 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t179)
      t182 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, t
     #104, 0.0D0, -t114)
      t184 = pi * t182 * t31
      t187 = FJET(XB1, XB2, s, t104, 0.0D0, 0.0D0, -t110, -t114, -t6 * t
     #184 / 0.16D2)
      t192 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t129, 0.
     #0D0, 0.0D0, 0.0D0)
      t194 = pi * t192 * t27
      t197 = FJET(XB1, XB2, s, t126, 0.0D0, -t129, 0.0D0, 0.0D0, -t6 * t
     #194 / 0.16D2)
      t202 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t126, -t129, 0.
     #0D0, 0.0D0, 0.0D0)
      t204 = pi * t202 * t27
      t207 = FJET(XB1, XB2, s, -t129, 0.0D0, t126, 0.0D0, 0.0D0, -t6 * t
     #204 / 0.16D2)
      t212 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, t
     #104, 0.0D0, -t114)
      t214 = pi * t212 * t31
      t217 = FJET(XB1, XB2, s, -t110, 0.0D0, 0.0D0, t104, -t114, -t6 * t
     #214 / 0.16D2)
      rrqg2qght8s1em2 = t58 * t57 + t80 * t79 + t102 * t101 - t120 * t3 
     #* t5 * t117 / 0.16D2 - t135 * t3 * t5 * t132 / 0.16D2 - t145 * t3 
     #* t5 * t142 / 0.16D2 - t155 * t3 * t5 * t152 / 0.16D2 + t180 * t17
     #9 - t187 * t3 * t5 * t184 / 0.16D2 - t197 * t3 * t5 * t194 / 0.16D
     #2 - t207 * t3 * t5 * t204 / 0.16D2 - t217 * t3 * t5 * t214 / 0.16D
     #2

      end function



      doubleprecision function rrqg2qght8s1em3
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * pi * 
     #t7 / 0.32D2)
      t13 = t5 * pi
      t16 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * pi * 
     #t16 / 0.32D2)
      t24 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t24 / 0.32D2)
      t32 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t32 / 0.32D2)
      rrqg2qght8s1em3 = t11 * t3 * t13 * t7 / 0.32D2 + t20 * t3 * t13 * 
     #t16 / 0.32D2 + t28 * t3 * t13 * t24 / 0.32D2 + t36 * t3 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrqg2qght8s1em4
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght8s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght8s2e1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t3 = -0.1D1 + x2
      t4 = x2 * x3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x3
      t9 = Sqrt(-t4 * t7)
      t10 = t6 * t9
      t11 = 0.2D1 * t10
      t14 = -0.1D1 + t4
      t15 = 0.1D1 / t14
      t17 = t2 * t3 * (-t4 - 0.1D1 + x3 + t11) * t15
      t18 = 0.3D1 * t4
      t19 = x2 ** 2
      t20 = t19 * x3
      t22 = 0.2D1 * t10 * x2
      t25 = t2 * (-x2 - x3 + t18 - t20 - t11 + t22) * t15
      t26 = 0.1D1 / t1
      t27 = s ** 2
      t28 = 0.1D1 / t27
      t29 = t26 * t28
      t30 = x2 * z
      t31 = 0.1D1 - x2 + t30
      t32 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D0
     #, 0.0D0, 0.0D0)
      t34 = t1 ** 2
      t35 = t34 ** 2
      t36 = Sin(t5)
      t37 = t36 ** 2
      t38 = t35 * t37
      t40 = z ** 2
      t41 = 0.1D1 / t40
      t42 = t3 ** 2
      t43 = t41 * t42
      t44 = t14 ** 2
      t45 = 0.1D1 / t44
      t50 = log(-0.4D1 * t4 * t38 * t43 * t7 * t45)
      t51 = t50 * t31
      t52 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D0
     #, 0.0D0, 0.0D0)
      t54 = t50 ** 2
      t55 = t54 * t31
      t56 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = t20 * z
      t62 = t4 * z
      t63 = 0.2D1 * t4
      t67 = 0.1D1 / (t20 - t61 - t30 + t62 + x2 - t63 - t22 + 0.2D1 * t1
     #0 * t30 + t11 - 0.1D1)
      t71 = lh * t26
      t72 = t71 * t28
      t73 = t31 * t52
      t80 = lh ** 2
      t81 = 0.180D3 * t80
      t82 = pi ** 2
      t83 = 0.30D2 * t82
      t84 = t81 - t83
      t85 = t84 * t26
      t86 = t85 * t28
      t87 = pi * t31
      t89 = t87 * t56 * t67
      t92 = 0.1D1 / x3
      t94 = 0.1D1 / x2
      t97 = x1 ** 2
      t99 = t4 * t97 * t35
      t100 = t37 * t41
      t106 = log(-0.4D1 * t99 * t100 * t42 * t7 * t45)
      t107 = t106 * t31
      t118 = 0.1D1 / x1
      t119 = t94 * t118
      t122 = -(0.90D2 * t29 * pi * (t31 * t32 - t51 * t52 + t55 * t56 / 
     #0.2D1) * t67 - 0.180D3 * t72 * pi * (t73 - t51 * t56) * t67 + t86 
     #* t89) * t92 * t94 / 0.1440D4 - (0.90D2 * t29 * pi * (t73 - t107 *
     # t56) * t67 - 0.180D3 * t72 * t89) * t92 * t119 / 0.720D3
      t123 = FJET(XB1, XB2, s, -t17, 0.0D0, t25, 0.0D0, 0.0D0, t122)
      t125 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D
     #0, 0.0D0, 0.0D0)
      t127 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D
     #0, 0.0D0, 0.0D0)
      t129 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D
     #0, 0.0D0, 0.0D0)
      t137 = t31 * t127
      t145 = t87 * t129 * t67
      t163 = -(0.90D2 * t29 * pi * (t31 * t125 - t51 * t127 + t55 * t129
     # / 0.2D1) * t67 - 0.180D3 * t72 * pi * (t137 - t51 * t129) * t67 +
     # t86 * t145) * t92 * t94 / 0.1440D4 - (0.90D2 * t29 * pi * (t137 -
     # t107 * t129) * t67 - 0.180D3 * t72 * t145) * t92 * t119 / 0.720D3
      t164 = FJET(XB1, XB2, s, t25, 0.0D0, -t17, 0.0D0, 0.0D0, t163)
      t166 = t7 * s
      t167 = -0.1D1 + x1
      t168 = t1 * t167
      t169 = t166 * t168
      t170 = t1 * x1
      t171 = t166 * t170
      t173 = x3 * s * t168
      t174 = x3 * x1
      t175 = t2 * t174
      t176 = x3 * t97
      t178 = x1 * z
      t179 = 0.1D1 - x1 + t178
      t180 = 0.1D1 / t179
      t181 = t41 * t180
      t182 = t167 ** 2
      t183 = t182 * t7
      t187 = log(-0.4D1 * t176 * t38 * t181 * t183)
      t188 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -t
     #171, t175, 0.0D0)
      t190 = t187 ** 2
      t191 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -t
     #171, t175, 0.0D0)
      t194 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -t
     #171, t175, 0.0D0)
      t199 = t28 * pi
      t205 = t199 * t191
      t210 = t182 * t180
      t215 = log(-0.4D1 * t99 * t100 * t210 * t7)
      t227 = -(0.90D2 * t29 * pi * (t187 * t188 - t190 * t191 / 0.2D1 - 
     #t194) - 0.180D3 * t71 * t199 * (-t188 + t187 * t191) - t85 * t205)
     # * t92 * t118 / 0.720D3 - (0.90D2 * t29 * pi * (t215 * t191 - t188
     #) + 0.180D3 * t71 * t205) * t92 * t119 / 0.720D3
      t228 = FJET(XB1, XB2, s, t169, -t171, -t173, t175, 0.0D0, t227)
      t232 = t2 * t167 * x2 * t180
      t233 = t3 * s
      t234 = t233 * t168
      t235 = t2 * x1
      t240 = s * t34 * x2 * t167 * x1 * t180
      t245 = log(0.4D1 * t99 * t100 * t210 * t42)
      t246 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t248 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t253 = t199 * t246
      t259 = t97 * x2
      t265 = log(0.4D1 * t259 * t38 * t181 * t182 * t42)
      t266 = t265 ** 2
      t270 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t285 = -(0.90D2 * t29 * pi * (t245 * t246 - t248) + 0.180D3 * t71 
     #* t253) * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (t266 * t246
     # / 0.2D1 - t265 * t248 + t270) - 0.180D3 * t71 * t199 * (-t265 * t
     #246 + t248) + t85 * t253) * t94 * t118 / 0.720D3
      t286 = FJET(XB1, XB2, s, -t232, 0.0D0, t234, t235, -t240, t285)
      t288 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -t
     #171, t175, 0.0D0)
      t290 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -t
     #171, t175, 0.0D0)
      t291 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -t
     #171, t175, 0.0D0)
      t303 = t199 * t291
      t319 = -(0.90D2 * t29 * pi * (t187 * t288 - t290 - t190 * t291 / 0
     #.2D1) - 0.180D3 * t71 * t199 * (-t288 + t187 * t291) - t85 * t303)
     # * t92 * t118 / 0.720D3 - (0.90D2 * t29 * pi * (-t288 + t215 * t29
     #1) + 0.180D3 * t71 * t303) * t92 * t119 / 0.720D3
      t320 = FJET(XB1, XB2, s, -t173, t175, t169, -t171, 0.0D0, t319)
      t322 = t233 * t1
      t324 = x2 * s * t1
      t325 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t326 = t4 * t35
      t327 = t100 * t42
      t330 = log(0.4D1 * t326 * t327)
      t331 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t333 = t330 ** 2
      t334 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t346 = t199 * t334
      t347 = t85 * t346
      t352 = x2 * t35
      t355 = log(0.4D1 * t352 * t327)
      t360 = t355 ** 2
      t361 = t360 * t355
      t367 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t373 = 0.60D2 * lh * t82
      t374 = 0.240D3 * zeta3
      t376 = 0.120D3 * t80 * lh
      t377 = t373 - t374 - t376
      t378 = t377 * t26
      t390 = t176 * x2
      t394 = log(0.4D1 * t390 * t38 * t43)
      t406 = t259 * t35
      t409 = log(0.4D1 * t406 * t327)
      t411 = t409 ** 2
      t427 = -(0.90D2 * t29 * pi * (t325 - t330 * t331 + t333 * t334 / 0
     #.2D1) - 0.180D3 * t71 * t199 * (-t330 * t334 + t331) + t347) * t92
     # * t94 / 0.1440D4 - (t85 * t199 * (t331 - t355 * t334) + 0.90D2 * 
     #t29 * pi * (-t361 * t334 / 0.6D1 - t355 * t325 + t360 * t331 / 0.2
     #D1 + t367) + t378 * t346 - 0.180D3 * t71 * t199 * (t325 + t360 * t
     #334 / 0.2D1 - t355 * t331)) * t94 / 0.1440D4 - (0.90D2 * t29 * pi 
     #* (-t394 * t334 + t331) - 0.180D3 * t71 * t346) * t92 * t119 / 0.7
     #20D3 + (0.90D2 * t29 * pi * (-t325 + t409 * t331 - t411 * t334 / 0
     #.2D1) - 0.180D3 * t71 * t199 * (t409 * t334 - t331) - t347) * t94 
     #* t118 / 0.720D3
      t428 = FJET(XB1, XB2, s, -t322, 0.0D0, t324, 0.0D0, 0.0D0, t427)
      t430 = t2 * t7
      t431 = t2 * x3
      t433 = t100 * t7
      t436 = log(-0.4D1 * x3 * t35 * t433)
      t437 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t439 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t443 = t436 ** 2
      t446 = t443 * t436
      t449 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t451 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t456 = t199 * t437
      t471 = log(-0.4D1 * t176 * t35 * t433)
      t472 = t471 ** 2
      t485 = t85 * t456
      t494 = log(-0.4D1 * t390 * t38 * t41 * t7)
      t508 = log(-0.4D1 * t326 * t433)
      t510 = t508 ** 2
      t526 = -(t85 * t199 * (-t436 * t437 + t439) + 0.90D2 * t29 * pi * 
     #(t443 * t439 / 0.2D1 - t446 * t437 / 0.6D1 - t436 * t449 + t451) +
     # t378 * t456 - 0.180D3 * t71 * t199 * (t449 + t443 * t437 / 0.2D1 
     #- t436 * t439)) * t92 / 0.1440D4 - (0.90D2 * t29 * pi * (t472 * t4
     #37 / 0.2D1 - t471 * t439 + t449) - 0.180D3 * t71 * t199 * (-t471 *
     # t437 + t439) + t485) * t92 * t118 / 0.720D3 - (0.90D2 * t29 * pi 
     #* (-t494 * t437 + t439) - 0.180D3 * t71 * t456) * t92 * t119 / 0.7
     #20D3 - (0.90D2 * t29 * pi * (-t508 * t439 + t449 + t510 * t437 / 0
     #.2D1) - 0.180D3 * t71 * t199 * (-t508 * t437 + t439) + t485) * t92
     # * t94 / 0.1440D4
      t527 = FJET(XB1, XB2, s, 0.0D0, -t430, 0.0D0, t431, 0.0D0, t526)
      t529 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D
     #0, 0.0D0, 0.0D0)
      t530 = t31 * t529
      t531 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D
     #0, 0.0D0, 0.0D0)
      t539 = t87 * t531 * t67
      t546 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0D
     #0, 0.0D0, 0.0D0)
      t567 = -(0.90D2 * t29 * pi * (t530 - t107 * t531) * t67 - 0.180D3 
     #* t72 * t539) * t92 * t119 / 0.720D3 - (0.90D2 * t29 * pi * (t31 *
     # t546 - t51 * t529 + t55 * t531 / 0.2D1) * t67 - 0.180D3 * t72 * p
     #i * (t530 - t51 * t531) * t67 + t86 * t539) * t92 * t94 / 0.1440D4
      t568 = FJET(XB1, XB2, s, 0.0D0, -t17, 0.0D0, t25, 0.0D0, t567)
      t570 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t571 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t577 = t199 * t571
      t583 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t601 = -(0.90D2 * t29 * pi * (-t570 + t245 * t571) + 0.180D3 * t71
     # * t577) * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (t583 + t26
     #6 * t571 / 0.2D1 - t265 * t570) - 0.180D3 * t71 * t199 * (-t265 * 
     #t571 + t570) + t85 * t577) * t94 * t118 / 0.720D3
      t602 = FJET(XB1, XB2, s, t235, t234, 0.0D0, -t232, -t240, t601)
      t604 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t606 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t611 = t199 * t604
      t617 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t635 = -(0.90D2 * t29 * pi * (t245 * t604 - t606) + 0.180D3 * t71 
     #* t611) * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (t617 + t266
     # * t604 / 0.2D1 - t265 * t606) - 0.180D3 * t71 * t199 * (t606 - t2
     #65 * t604) + t85 * t611) * t94 * t118 / 0.720D3
      t636 = FJET(XB1, XB2, s, t234, t235, -t232, 0.0D0, -t240, t635)
      t638 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t639 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t641 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t653 = t199 * t641
      t654 = t85 * t653
      t695 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t711 = -(0.90D2 * t29 * pi * (t638 - t471 * t639 + t472 * t641 / 0
     #.2D1) - 0.180D3 * t71 * t199 * (t639 - t471 * t641) + t654) * t92 
     #* t118 / 0.720D3 - (0.90D2 * t29 * pi * (t639 - t494 * t641) - 0.1
     #80D3 * t71 * t653) * t92 * t119 / 0.720D3 - (0.90D2 * t29 * pi * (
     #-t508 * t639 + t638 + t510 * t641 / 0.2D1) - 0.180D3 * t71 * t199 
     #* (t639 - t508 * t641) + t654) * t92 * t94 / 0.1440D4 - (t85 * t19
     #9 * (t639 - t436 * t641) + 0.90D2 * t29 * pi * (-t446 * t641 / 0.6
     #D1 + t443 * t639 / 0.2D1 - t436 * t638 + t695) + t378 * t653 - 0.1
     #80D3 * t71 * t199 * (t638 + t443 * t641 / 0.2D1 - t436 * t639)) * 
     #t92 / 0.1440D4
      t712 = FJET(XB1, XB2, s, 0.0D0, t431, 0.0D0, -t430, 0.0D0, t711)
      t714 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t715 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t721 = t199 * t715
      t730 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t741 = t85 * t721
      t770 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0.
     #0D0, 0.0D0, 0.0D0)
      t787 = -(0.90D2 * t29 * pi * (t714 - t394 * t715) - 0.180D3 * t71 
     #* t721) * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (-t411 * t71
     #5 / 0.2D1 - t730 + t409 * t714) - 0.180D3 * t71 * t199 * (t409 * t
     #715 - t714) - t741) * t94 * t118 / 0.720D3 - (0.90D2 * t29 * pi * 
     #(-t330 * t714 + t730 + t333 * t715 / 0.2D1) - 0.180D3 * t71 * t199
     # * (-t330 * t715 + t714) + t741) * t92 * t94 / 0.1440D4 - (t85 * t
     #199 * (-t355 * t715 + t714) + 0.90D2 * t29 * pi * (t360 * t714 / 0
     #.2D1 - t361 * t715 / 0.6D1 + t770 - t355 * t730) + t378 * t721 - 0
     #.180D3 * t71 * t199 * (t360 * t715 / 0.2D1 - t355 * t714 + t730)) 
     #* t94 / 0.1440D4
      t788 = FJET(XB1, XB2, s, 0.0D0, t324, 0.0D0, -t322, 0.0D0, t787)
      t790 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t792 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t797 = t199 * t790
      t803 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t234, -t232, t2
     #35, 0.0D0, -t240)
      t821 = -(0.90D2 * t29 * pi * (t245 * t790 - t792) + 0.180D3 * t71 
     #* t797) * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (t803 - t265
     # * t792 + t266 * t790 / 0.2D1) - 0.180D3 * t71 * t199 * (-t265 * t
     #790 + t792) + t85 * t797) * t94 * t118 / 0.720D3
      t822 = FJET(XB1, XB2, s, 0.0D0, -t232, t235, t234, -t240, t821)
      t824 = t2 * t167
      t826 = t41 * t35
      t830 = log(0.4D1 * t176 * t37 * t826 * t210)
      t831 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, t
     #235, 0.0D0, 0.0D0)
      t833 = t830 ** 2
      t834 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, t
     #235, 0.0D0, 0.0D0)
      t837 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, t
     #235, 0.0D0, 0.0D0)
      t847 = t199 * t834
      t848 = t85 * t847
      t852 = t97 * t37
      t858 = log(0.4D1 * t852 * t41 * t35 * t180 * t182)
      t863 = t858 ** 2
      t864 = t863 * t858
      t867 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, t
     #235, 0.0D0, 0.0D0)
      t885 = t100 * t210
      t888 = log(0.4D1 * t99 * t885)
      t901 = log(0.4D1 * t406 * t885)
      t903 = t901 ** 2
      t919 = -(0.90D2 * t29 * pi * (-t830 * t831 + t833 * t834 / 0.2D1 +
     # t837) - 0.180D3 * t71 * t199 * (t831 - t830 * t834) + t848) * t92
     # * t118 / 0.720D3 - (t85 * t199 * (t831 - t858 * t834) + 0.90D2 * 
     #t29 * pi * (-t864 * t834 / 0.6D1 + t867 - t858 * t837 + t863 * t83
     #1 / 0.2D1) + t378 * t847 - 0.180D3 * t71 * t199 * (-t858 * t831 + 
     #t837 + t863 * t834 / 0.2D1)) * t118 / 0.720D3 - (0.90D2 * t29 * pi
     # * (-t888 * t834 + t831) - 0.180D3 * t71 * t847) * t92 * t119 / 0.
     #720D3 + (0.90D2 * t29 * pi * (t901 * t831 - t837 - t903 * t834 / 0
     #.2D1) - 0.180D3 * t71 * t199 * (-t831 + t901 * t834) - t848) * t94
     # * t118 / 0.720D3
      t920 = FJET(XB1, XB2, s, t235, -t824, 0.0D0, 0.0D0, 0.0D0, t919)
      t922 = t100 * t35
      t925 = log(0.4D1 * t176 * t922)
      t926 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t928 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t929 = t925 ** 2
      t930 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t942 = t199 * t930
      t943 = t85 * t942
      t950 = log(0.4D1 * t852 * t826)
      t955 = t950 ** 2
      t958 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t960 = t955 * t950
      t967 = t378 * t942
      t980 = log(0.4D1 * t390 * t922)
      t994 = log(0.4D1 * t259 * t922)
      t995 = t994 ** 2
      t1013 = log(0.4D1 * t922)
      t1014 = t1013 ** 2
      t1017 = t1014 * t1013
      t1021 = (-0.90D2 * t1014 * lh + t373 - t374 - t376 - 0.15D2 * t101
     #7 - t1013 * t84) * t26
      t1029 = (0.180D3 * t1013 * lh + 0.45D2 * t1014 + t81 - t83) * t26
      t1035 = log(0.4D1 * t4 * t922)
      t1036 = t1035 ** 2
      t1055 = log(0.4D1 * t352 * t100)
      t1060 = t1055 ** 2
      t1061 = t1060 * t1055
      t1081 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1088 = (-0.180D3 * lh - 0.90D2 * t1013) * t26
      t1095 = log(0.4D1 * x3 * t37 * t826)
      t1100 = t1095 ** 2
      t1101 = t1100 * t1095
      t1126 = t82 ** 2
      t1127 = t80 ** 2
      t1133 = t1014 ** 2
      t1136 = (0.30D2 * t1017 * lh + t1014 * t84 / 0.2D1 - t1013 * t377 
     #+ t1126 + 0.60D2 * t1127 + 0.480D3 * lh * zeta3 - 0.60D2 * t80 * t
     #82 + 0.15D2 / 0.4D1 * t1133) * t26
      t1139 = -(0.90D2 * t29 * pi * (t925 * t926 - t928 - t929 * t930 / 
     #0.2D1) - 0.180D3 * t71 * t199 * (-t926 + t925 * t930) - t943) * t9
     #2 * t118 / 0.720D3 - (t85 * t199 * (-t926 + t950 * t930) + 0.90D2 
     #* t29 * pi * (-t955 * t926 / 0.2D1 - t958 + t950 * t928 + t960 * t
     #930 / 0.6D1) - t967 - 0.180D3 * t71 * t199 * (-t928 - t955 * t930 
     #/ 0.2D1 + t950 * t926)) * t118 / 0.720D3 - (0.90D2 * t29 * pi * (t
     #980 * t930 - t926) + 0.180D3 * t71 * t942) * t92 * t119 / 0.720D3 
     #+ (0.90D2 * t29 * pi * (t995 * t930 / 0.2D1 - t994 * t926 + t928) 
     #- 0.180D3 * t71 * t199 * (-t994 * t930 + t926) + t943) * t94 * t11
     #8 / 0.720D3 + t1021 * t199 * t926 / 0.1440D4 + t1029 * t199 * t928
     # / 0.1440D4 - (0.90D2 * t29 * pi * (-t928 - t1036 * t930 / 0.2D1 +
     # t1035 * t926) - 0.180D3 * t71 * t199 * (t1035 * t930 - t926) - t9
     #43) * t92 * t94 / 0.1440D4 - (t85 * t199 * (-t926 + t1055 * t930) 
     #+ 0.90D2 * t29 * pi * (-t958 + t1061 * t930 / 0.6D1 + t1055 * t928
     # - t1060 * t926 / 0.2D1) - t967 - 0.180D3 * t71 * t199 * (-t928 + 
     #t1055 * t926 - t1060 * t930 / 0.2D1)) * t94 / 0.1440D4 + t29 * pi 
     #* t1081 / 0.16D2 + t1088 * t199 * t958 / 0.1440D4 - (t85 * t199 * 
     #(-t926 + t1095 * t930) + 0.90D2 * t29 * pi * (-t958 + t1101 * t930
     # / 0.6D1 + t1095 * t928 - t1100 * t926 / 0.2D1) - t967 - 0.180D3 *
     # t71 * t199 * (-t928 + t1095 * t926 - t1100 * t930 / 0.2D1)) * t92
     # / 0.1440D4 + t1136 * t942 / 0.1440D4
      t1140 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1139)
      t1142 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1144 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1150 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1153 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1159 = t199 * t1142
      t1183 = t85 * t1159
      t1215 = -(t85 * t199 * (-t436 * t1142 + t1144) + 0.90D2 * t29 * pi
     # * (t443 * t1144 / 0.2D1 + t1150 - t446 * t1142 / 0.6D1 - t436 * t
     #1153) + t378 * t1159 - 0.180D3 * t71 * t199 * (t1153 + t443 * t114
     #2 / 0.2D1 - t436 * t1144)) * t92 / 0.1440D4 - (0.90D2 * t29 * pi *
     # (t510 * t1142 / 0.2D1 + t1153 - t508 * t1144) - 0.180D3 * t71 * t
     #199 * (t1144 - t508 * t1142) + t1183) * t92 * t94 / 0.1440D4 - (0.
     #90D2 * t29 * pi * (t472 * t1142 / 0.2D1 - t471 * t1144 + t1153) - 
     #0.180D3 * t71 * t199 * (-t471 * t1142 + t1144) + t1183) * t92 * t1
     #18 / 0.720D3 - (0.90D2 * t29 * pi * (-t494 * t1142 + t1144) - 0.18
     #0D3 * t71 * t1159) * t92 * t119 / 0.720D3
      t1216 = FJET(XB1, XB2, s, t431, 0.0D0, -t430, 0.0D0, 0.0D0, t1215)
      t1218 = t123 * t122 + t164 * t163 + t228 * t227 + t286 * t285 + t3
     #20 * t319 + t428 * t427 + t527 * t526 + t568 * t567 + t602 * t601 
     #+ t636 * t635 + t712 * t711 + t788 * t787 + t822 * t821 + t920 * t
     #919 + t1140 * t1139 + t1216 * t1215
      t1219 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1220 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1223 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1234 = t199 * t1220
      t1235 = t85 * t1234
      t1246 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1289 = -(0.90D2 * t29 * pi * (t1219 + t833 * t1220 / 0.2D1 - t830
     # * t1223) - 0.180D3 * t71 * t199 * (t1223 - t830 * t1220) + t1235)
     # * t92 * t118 / 0.720D3 - (t85 * t199 * (t1223 - t858 * t1220) + 0
     #.90D2 * t29 * pi * (-t864 * t1220 / 0.6D1 - t858 * t1219 + t1246 +
     # t863 * t1223 / 0.2D1) + t378 * t1234 - 0.180D3 * t71 * t199 * (t1
     #219 - t858 * t1223 + t863 * t1220 / 0.2D1)) * t118 / 0.720D3 - (0.
     #90D2 * t29 * pi * (-t888 * t1220 + t1223) - 0.180D3 * t71 * t1234)
     # * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (t901 * t1223 - t12
     #19 - t903 * t1220 / 0.2D1) - 0.180D3 * t71 * t199 * (t901 * t1220 
     #- t1223) - t1235) * t94 * t118 / 0.720D3
      t1290 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t235, -t824, 0.0D0, t1289)
      t1292 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1294 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1300 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1303 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t430, t431, 0
     #.0D0, 0.0D0, 0.0D0)
      t1309 = t199 * t1292
      t1333 = t85 * t1309
      t1365 = -(t85 * t199 * (-t436 * t1292 + t1294) + 0.90D2 * t29 * pi
     # * (-t446 * t1292 / 0.6D1 + t1300 + t443 * t1294 / 0.2D1 - t436 * 
     #t1303) + t378 * t1309 - 0.180D3 * t71 * t199 * (t1303 + t443 * t12
     #92 / 0.2D1 - t436 * t1294)) * t92 / 0.1440D4 - (0.90D2 * t29 * pi 
     #* (t1303 - t508 * t1294 + t510 * t1292 / 0.2D1) - 0.180D3 * t71 * 
     #t199 * (t1294 - t508 * t1292) + t1333) * t92 * t94 / 0.1440D4 - (0
     #.90D2 * t29 * pi * (t1303 + t472 * t1292 / 0.2D1 - t471 * t1294) -
     # 0.180D3 * t71 * t199 * (-t471 * t1292 + t1294) + t1333) * t92 * t
     #118 / 0.720D3 - (0.90D2 * t29 * pi * (t1294 - t494 * t1292) - 0.18
     #0D3 * t71 * t1309) * t92 * t119 / 0.720D3
      t1366 = FJET(XB1, XB2, s, -t430, 0.0D0, t431, 0.0D0, 0.0D0, t1365)
      t1368 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1371 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1372 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1383 = t199 * t1368
      t1384 = t85 * t1383
      t1393 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1403 = t378 * t1383
      t1414 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1518 = -(0.90D2 * t29 * pi * (-t1036 * t1368 / 0.2D1 - t1371 + t1
     #035 * t1372) - 0.180D3 * t71 * t199 * (t1035 * t1368 - t1372) - t1
     #384) * t92 * t94 / 0.1440D4 - (t85 * t199 * (-t1372 + t1055 * t136
     #8) + 0.90D2 * t29 * pi * (-t1393 + t1061 * t1368 / 0.6D1 - t1060 *
     # t1372 / 0.2D1 + t1055 * t1371) - t1403 - 0.180D3 * t71 * t199 * (
     #-t1371 + t1055 * t1372 - t1060 * t1368 / 0.2D1)) * t94 / 0.1440D4 
     #+ t29 * pi * t1414 / 0.16D2 + t1021 * t199 * t1372 / 0.1440D4 + t1
     #029 * t199 * t1371 / 0.1440D4 + t1088 * t199 * t1393 / 0.1440D4 + 
     #t1136 * t1383 / 0.1440D4 - (t85 * t199 * (-t1372 + t1095 * t1368) 
     #+ 0.90D2 * t29 * pi * (-t1393 + t1101 * t1368 / 0.6D1 - t1100 * t1
     #372 / 0.2D1 + t1095 * t1371) - t1403 - 0.180D3 * t71 * t199 * (-t1
     #371 + t1095 * t1372 - t1100 * t1368 / 0.2D1)) * t92 / 0.1440D4 - (
     #0.90D2 * t29 * pi * (-t1371 + t925 * t1372 - t929 * t1368 / 0.2D1)
     # - 0.180D3 * t71 * t199 * (-t1372 + t925 * t1368) - t1384) * t92 *
     # t118 / 0.720D3 - (t85 * t199 * (-t1372 + t950 * t1368) + 0.90D2 *
     # t29 * pi * (-t1393 + t960 * t1368 / 0.6D1 - t955 * t1372 / 0.2D1 
     #+ t950 * t1371) - t1403 - 0.180D3 * t71 * t199 * (-t1371 + t950 * 
     #t1372 - t955 * t1368 / 0.2D1)) * t118 / 0.720D3 - (0.90D2 * t29 * 
     #pi * (t980 * t1368 - t1372) + 0.180D3 * t71 * t1383) * t92 * t119 
     #/ 0.720D3 + (0.90D2 * t29 * pi * (t1371 + t995 * t1368 / 0.2D1 - t
     #994 * t1372) - 0.180D3 * t71 * t199 * (t1372 - t994 * t1368) + t13
     #84) * t94 * t118 / 0.720D3
      t1519 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1518)
      t1521 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0
     #D0, 0.0D0, 0.0D0)
      t1522 = t31 * t1521
      t1523 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0
     #D0, 0.0D0, 0.0D0)
      t1531 = t87 * t1523 * t67
      t1538 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t17, t25, 0.0
     #D0, 0.0D0, 0.0D0)
      t1559 = -(0.90D2 * t29 * pi * (t1522 - t107 * t1523) * t67 - 0.180
     #D3 * t72 * t1531) * t92 * t119 / 0.720D3 - (0.90D2 * t29 * pi * (t
     #31 * t1538 - t51 * t1521 + t55 * t1523 / 0.2D1) * t67 - 0.180D3 * 
     #t72 * pi * (t1522 - t51 * t1523) * t67 + t86 * t1531) * t92 * t94 
     #/ 0.1440D4
      t1560 = FJET(XB1, XB2, s, 0.0D0, t25, 0.0D0, -t17, 0.0D0, t1559)
      t1562 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1563 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1565 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1577 = t199 * t1565
      t1578 = t85 * t1577
      t1587 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1635 = -(0.90D2 * t29 * pi * (t1562 - t330 * t1563 + t333 * t1565
     # / 0.2D1) - 0.180D3 * t71 * t199 * (-t330 * t1565 + t1563) + t1578
     #) * t92 * t94 / 0.1440D4 - (t85 * t199 * (t1563 - t355 * t1565) + 
     #0.90D2 * t29 * pi * (t1587 - t361 * t1565 / 0.6D1 - t355 * t1562 +
     # t360 * t1563 / 0.2D1) + t378 * t1577 - 0.180D3 * t71 * t199 * (t1
     #562 + t360 * t1565 / 0.2D1 - t355 * t1563)) * t94 / 0.1440D4 - (0.
     #90D2 * t29 * pi * (-t394 * t1565 + t1563) - 0.180D3 * t71 * t1577)
     # * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (-t411 * t1565 / 0.
     #2D1 - t1562 + t409 * t1563) - 0.180D3 * t71 * t199 * (t409 * t1565
     # - t1563) - t1578) * t94 * t118 / 0.720D3
      t1636 = FJET(XB1, XB2, s, t324, 0.0D0, -t322, 0.0D0, 0.0D0, t1635)
      t1638 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1639 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1642 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1653 = t199 * t1639
      t1654 = t85 * t1653
      t1662 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t1708 = -(0.90D2 * t29 * pi * (t1638 + t833 * t1639 / 0.2D1 - t830
     # * t1642) - 0.180D3 * t71 * t199 * (t1642 - t830 * t1639) + t1654)
     # * t92 * t118 / 0.720D3 - (t85 * t199 * (-t858 * t1639 + t1642) + 
     #0.90D2 * t29 * pi * (t1662 - t864 * t1639 / 0.6D1 - t858 * t1638 +
     # t863 * t1642 / 0.2D1) + t378 * t1653 - 0.180D3 * t71 * t199 * (t1
     #638 + t863 * t1639 / 0.2D1 - t858 * t1642)) * t118 / 0.720D3 - (0.
     #90D2 * t29 * pi * (-t888 * t1639 + t1642) - 0.180D3 * t71 * t1653)
     # * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (t901 * t1642 - t90
     #3 * t1639 / 0.2D1 - t1638) - 0.180D3 * t71 * t199 * (-t1642 + t901
     # * t1639) - t1654) * t94 * t118 / 0.720D3
      t1709 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t824, t235, 0.0D0, t1708)
      t1711 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1712 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1719 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1720 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1728 = t199 * t1712
      t1729 = t378 * t1728
      t1754 = t85 * t1728
      t1854 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1861 = -(t85 * t199 * (-t1711 + t1095 * t1712) + 0.90D2 * t29 * p
     #i * (-t1100 * t1711 / 0.2D1 - t1719 + t1095 * t1720 + t1101 * t171
     #2 / 0.6D1) - t1729 - 0.180D3 * t71 * t199 * (-t1720 + t1095 * t171
     #1 - t1100 * t1712 / 0.2D1)) * t92 / 0.1440D4 + t1136 * t1728 / 0.1
     #440D4 - (0.90D2 * t29 * pi * (-t929 * t1712 / 0.2D1 + t925 * t1711
     # - t1720) - 0.180D3 * t71 * t199 * (t925 * t1712 - t1711) - t1754)
     # * t92 * t118 / 0.720D3 - (t85 * t199 * (t950 * t1712 - t1711) + 0
     #.90D2 * t29 * pi * (-t1719 + t950 * t1720 + t960 * t1712 / 0.6D1 -
     # t955 * t1711 / 0.2D1) - t1729 - 0.180D3 * t71 * t199 * (-t955 * t
     #1712 / 0.2D1 + t950 * t1711 - t1720)) * t118 / 0.720D3 - (0.90D2 *
     # t29 * pi * (-t1711 + t980 * t1712) + 0.180D3 * t71 * t1728) * t92
     # * t119 / 0.720D3 + (0.90D2 * t29 * pi * (-t994 * t1711 + t995 * t
     #1712 / 0.2D1 + t1720) - 0.180D3 * t71 * t199 * (-t994 * t1712 + t1
     #711) + t1754) * t94 * t118 / 0.720D3 + t1021 * t199 * t1711 / 0.14
     #40D4 - (0.90D2 * t29 * pi * (t1035 * t1711 - t1720 - t1036 * t1712
     # / 0.2D1) - 0.180D3 * t71 * t199 * (-t1711 + t1035 * t1712) - t175
     #4) * t92 * t94 / 0.1440D4 - (t85 * t199 * (-t1711 + t1055 * t1712)
     # + 0.90D2 * t29 * pi * (-t1060 * t1711 / 0.2D1 - t1719 + t1055 * t
     #1720 + t1061 * t1712 / 0.6D1) - t1729 - 0.180D3 * t71 * t199 * (t1
     #055 * t1711 - t1060 * t1712 / 0.2D1 - t1720)) * t94 / 0.1440D4 + t
     #1088 * t199 * t1719 / 0.1440D4 + t29 * pi * t1854 / 0.16D2 + t1029
     # * t199 * t1720 / 0.1440D4
      t1862 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1861)
      t1864 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1866 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1871 = t199 * t1864
      t1881 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1891 = t85 * t1871
      t1921 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, -t322, t324, 0
     #.0D0, 0.0D0, 0.0D0)
      t1937 = -(0.90D2 * t29 * pi * (-t394 * t1864 + t1866) - 0.180D3 * 
     #t71 * t1871) * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (t409 *
     # t1866 - t411 * t1864 / 0.2D1 - t1881) - 0.180D3 * t71 * t199 * (-
     #t1866 + t409 * t1864) - t1891) * t94 * t118 / 0.720D3 - (0.90D2 * 
     #t29 * pi * (t1881 + t333 * t1864 / 0.2D1 - t330 * t1866) - 0.180D3
     # * t71 * t199 * (-t330 * t1864 + t1866) + t1891) * t92 * t94 / 0.1
     #440D4 - (t85 * t199 * (-t355 * t1864 + t1866) + 0.90D2 * t29 * pi 
     #* (-t361 * t1864 / 0.6D1 - t355 * t1881 + t360 * t1866 / 0.2D1 + t
     #1921) + t378 * t1871 - 0.180D3 * t71 * t199 * (-t355 * t1866 + t36
     #0 * t1864 / 0.2D1 + t1881)) * t94 / 0.1440D4
      t1938 = FJET(XB1, XB2, s, 0.0D0, -t322, 0.0D0, t324, 0.0D0, t1937)
      t1940 = t174 * z
      t1941 = t4 * x1
      t1943 = t20 * x1
      t1944 = t4 * t178
      t1946 = t20 * t178
      t1950 = Sqrt(-x3 * t179 * x2 * t7)
      t1951 = t6 * t1950
      t1952 = 0.2D1 * t1951
      t1954 = 0.2D1 * t1951 * x2
      t1955 = -t1940 - 0.2D1 * t1941 + t1943 + t18 + t174 - x2 - x3 - t2
     #0 + 0.2D1 * t1944 - t1946 - t1952 + t1954
      t1958 = t824 * t1955 * t180 * t15
      t1961 = t235 * x3 * t3 * t15
      t1966 = t824 * t3 * (-t4 - 0.1D1 + x3 + x1 - t174 - t178 + t1940 +
     # t1952) * t180 * t15
      t1968 = t166 * t170 * t15
      t1969 = t29 * pi
      t1970 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t1981 = log(-0.4D1 * t37 * x2 * x3 * t180 * t97 * t43 * t45 * t183
     # * t35)
      t1982 = t1981 * t179
      t1983 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t1986 = x2 * x1
      t1987 = t1986 * z
      t1988 = -0.1D1 + t1987 - t1986 + x1 + x2 - t178 - t30
      t1991 = t97 * t40
      t1993 = t40 * x2
      t2001 = 0.1D1 - t20 + t30 + t63 + 0.2D1 * t178 + t390 - t1991 * x2
     # + t1993 * x1 + 0.2D1 * t1951 * x1 - 0.3D1 * t1987 + 0.2D1 * t259 
     #* z + t1954 - 0.3D1 * t1941 + t1943 + t61 - t62
      t2020 = t97 + 0.4D1 * t1944 - t1946 - 0.2D1 * t97 * z + t1991 + 0.
     #2D1 * t1986 - t259 - 0.2D1 * t1951 * t1986 - 0.2D1 * t1951 * t30 -
     # 0.2D1 * t1951 * t178 - 0.2D1 * t176 * t30 - x3 * t40 * t1986 + t1
     #76 * t1993 - 0.2D1 * x1 + 0.2D1 * t1951 * t1987 - x2 - t1952
      t2022 = 0.1D1 / (t2001 + t2020)
      t2026 = t71 * t199
      t2028 = t1988 * t2022
      t2032 = -0.90D2 * t1969 * (t179 * t1970 - t1982 * t1983) * t1988 *
     # t2022 + 0.180D3 * t2026 * t179 * t1983 * t2028
      t2036 = FJET(XB1, XB2, s, -t1958, t1961, t1966, t1968, -t240, -t20
     #32 * t92 * t119 / 0.720D3)
      t2039 = t92 * t94 * t118
      t2042 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -
     #t171, t175, 0.0D0)
      t2043 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -
     #t171, t175, 0.0D0)
      t2046 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -
     #t171, t175, 0.0D0)
      t2057 = t199 * t2043
      t2073 = -(0.90D2 * t29 * pi * (-t2042 - t190 * t2043 / 0.2D1 + t18
     #7 * t2046) - 0.180D3 * t71 * t199 * (-t2046 + t187 * t2043) - t85 
     #* t2057) * t92 * t118 / 0.720D3 - (0.90D2 * t29 * pi * (t215 * t20
     #43 - t2046) + 0.180D3 * t71 * t2057) * t92 * t119 / 0.720D3
      t2074 = FJET(XB1, XB2, s, t175, -t173, -t171, t169, 0.0D0, t2073)
      t2076 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t2078 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t2089 = -0.90D2 * t1969 * (t179 * t2076 - t1982 * t2078) * t1988 *
     # t2022 + 0.180D3 * t2026 * t179 * t2078 * t2028
      t2093 = FJET(XB1, XB2, s, t1961, -t1958, t1968, t1966, -t240, -t20
     #89 * t92 * t119 / 0.720D3)
      t2097 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -
     #t171, t175, 0.0D0)
      t2099 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -
     #t171, t175, 0.0D0)
      t2100 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t169, -t173, -
     #t171, t175, 0.0D0)
      t2112 = t199 * t2100
      t2128 = -(0.90D2 * t29 * pi * (t187 * t2097 - t2099 - t190 * t2100
     # / 0.2D1) - 0.180D3 * t71 * t199 * (t187 * t2100 - t2097) - t85 * 
     #t2112) * t92 * t118 / 0.720D3 - (0.90D2 * t29 * pi * (t215 * t2100
     # - t2097) + 0.180D3 * t71 * t2112) * t92 * t119 / 0.720D3
      t2129 = FJET(XB1, XB2, s, -t171, t169, t175, -t173, 0.0D0, t2128)
      t2131 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t2133 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t2144 = -0.90D2 * t1969 * (t179 * t2131 - t1982 * t2133) * t1988 *
     # t2022 + 0.180D3 * t2026 * t179 * t2133 * t2028
      t2148 = FJET(XB1, XB2, s, t1968, t1966, t1961, -t1958, -t240, -t21
     #44 * t92 * t119 / 0.720D3)
      t2152 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t2154 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1966, -t1958,
     # t1968, t1961, -t240)
      t2165 = -0.90D2 * t1969 * (t179 * t2152 - t1982 * t2154) * t1988 *
     # t2022 + 0.180D3 * t2026 * t179 * t2154 * t2028
      t2169 = FJET(XB1, XB2, s, t1966, t1968, -t1958, t1961, -t240, -t21
     #65 * t92 * t119 / 0.720D3)
      t2173 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2177 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2181 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2185 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2189 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2190 = t199 * t2189
      t2205 = t85 * t2190
      t2223 = t378 * t2190
      t2323 = t29 * pi * t2173 / 0.16D2 + t1021 * t199 * t2177 / 0.1440D
     #4 + t1029 * t199 * t2181 / 0.1440D4 + t1088 * t199 * t2185 / 0.144
     #0D4 + t1136 * t2190 / 0.1440D4 - (0.90D2 * t29 * pi * (-t2181 - t9
     #29 * t2189 / 0.2D1 + t925 * t2177) - 0.180D3 * t71 * t199 * (-t217
     #7 + t925 * t2189) - t2205) * t92 * t118 / 0.720D3 - (t85 * t199 * 
     #(-t2177 + t950 * t2189) + 0.90D2 * t29 * pi * (t950 * t2181 - t955
     # * t2177 / 0.2D1 + t960 * t2189 / 0.6D1 - t2185) - t2223 - 0.180D3
     # * t71 * t199 * (-t2181 + t950 * t2177 - t955 * t2189 / 0.2D1)) * 
     #t118 / 0.720D3 - (0.90D2 * t29 * pi * (-t2177 + t980 * t2189) + 0.
     #180D3 * t71 * t2190) * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi *
     # (t2181 - t994 * t2177 + t995 * t2189 / 0.2D1) - 0.180D3 * t71 * t
     #199 * (-t994 * t2189 + t2177) + t2205) * t94 * t118 / 0.720D3 - (t
     #85 * t199 * (-t2177 + t1095 * t2189) + 0.90D2 * t29 * pi * (t1095 
     #* t2181 - t1100 * t2177 / 0.2D1 + t1101 * t2189 / 0.6D1 - t2185) -
     # t2223 - 0.180D3 * t71 * t199 * (-t2181 + t1095 * t2177 - t1100 * 
     #t2189 / 0.2D1)) * t92 / 0.1440D4 - (0.90D2 * t29 * pi * (-t2181 + 
     #t1035 * t2177 - t1036 * t2189 / 0.2D1) - 0.180D3 * t71 * t199 * (t
     #1035 * t2189 - t2177) - t2205) * t92 * t94 / 0.1440D4 - (t85 * t19
     #9 * (-t2177 + t1055 * t2189) + 0.90D2 * t29 * pi * (t1055 * t2181 
     #- t1060 * t2177 / 0.2D1 + t1061 * t2189 / 0.6D1 - t2185) - t2223 -
     # 0.180D3 * t71 * t199 * (-t2181 + t1055 * t2177 - t1060 * t2189 / 
     #0.2D1)) * t94 / 0.1440D4
      t2324 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t2323)
      t2326 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t2327 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t2330 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t2341 = t199 * t2327
      t2342 = t85 * t2341
      t2351 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t824, 0.0D0, 
     #t235, 0.0D0, 0.0D0)
      t2396 = -(0.90D2 * t29 * pi * (t2326 + t833 * t2327 / 0.2D1 - t830
     # * t2330) - 0.180D3 * t71 * t199 * (t2330 - t830 * t2327) + t2342)
     # * t92 * t118 / 0.720D3 - (t85 * t199 * (t2330 - t858 * t2327) + 0
     #.90D2 * t29 * pi * (-t858 * t2326 + t2351 - t864 * t2327 / 0.6D1 +
     # t863 * t2330 / 0.2D1) + t378 * t2341 - 0.180D3 * t71 * t199 * (-t
     #858 * t2330 + t2326 + t863 * t2327 / 0.2D1)) * t118 / 0.720D3 - (0
     #.90D2 * t29 * pi * (t2330 - t888 * t2327) - 0.180D3 * t71 * t2341)
     # * t92 * t119 / 0.720D3 + (0.90D2 * t29 * pi * (-t2326 + t901 * t2
     #330 - t903 * t2327 / 0.2D1) - 0.180D3 * t71 * t199 * (-t2330 + t90
     #1 * t2327) - t2342) * t94 * t118 / 0.720D3
      t2397 = FJET(XB1, XB2, s, -t824, t235, 0.0D0, 0.0D0, 0.0D0, t2396)
      t2399 = t1290 * t1289 + t1366 * t1365 + t1519 * t1518 + t1560 * t1
     #559 + t1636 * t1635 + t1709 * t1708 + t1862 * t1861 + t1938 * t193
     #7 - t2036 * t2032 * t2039 / 0.720D3 + t2074 * t2073 - t2093 * t208
     #9 * t2039 / 0.720D3 + t2129 * t2128 - t2148 * t2144 * t2039 / 0.72
     #0D3 - t2169 * t2165 * t2039 / 0.720D3 + t2324 * t2323 + t2397 * t2
     #396
      rrqg2qght8s2e1 = t1218 + t2399

      end function



      doubleprecision function rrqg2qght8s2e0
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t5 = x3 * x1
      t6 = t5 * z
      t7 = x2 * x3
      t8 = t7 * x1
      t10 = x2 ** 2
      t11 = t10 * x3
      t12 = t11 * x1
      t13 = 0.3D1 * t7
      t14 = x1 * z
      t15 = t7 * t14
      t17 = t11 * t14
      t18 = x4 * pi
      t19 = cos(t18)
      t20 = 0.1D1 - x1 + t14
      t22 = -0.1D1 + x3
      t25 = Sqrt(-x3 * t20 * x2 * t22)
      t26 = t19 * t25
      t27 = 0.2D1 * t26
      t29 = 0.2D1 * t26 * x2
      t30 = -t6 - 0.2D1 * t8 + t12 + t13 + t5 - x2 - x3 - t11 + 0.2D1 * 
     #t15 - t17 - t27 + t29
      t31 = 0.1D1 / t20
      t33 = -0.1D1 + t7
      t34 = 0.1D1 / t33
      t36 = t4 * t30 * t31 * t34
      t37 = t2 * x1
      t38 = -0.1D1 + x2
      t41 = t37 * x3 * t38 * t34
      t46 = t4 * t38 * (-t7 - 0.1D1 + x3 + x1 - t5 - t14 + t6 + t27) * t
     #31 * t34
      t47 = t22 * s
      t48 = t1 * x1
      t50 = t47 * t48 * t34
      t51 = t1 ** 2
      t56 = s * t51 * x2 * t3 * x1 * t31
      t57 = 0.1D1 / t1
      t58 = s ** 2
      t59 = 0.1D1 / t58
      t60 = t57 * t59
      t61 = pi * t20
      t62 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t46, -t36, t50, 
     #t41, -t56)
      t65 = x2 * x1
      t66 = t65 * z
      t67 = x2 * z
      t68 = -0.1D1 + t66 - t65 + x1 + x2 - t14 - t67
      t69 = t7 * z
      t70 = x1 ** 2
      t71 = x3 * t70
      t73 = t11 * z
      t74 = z ** 2
      t75 = t70 * t74
      t77 = t74 * x2
      t83 = t70 * x2
      t86 = 0.2D1 * t7
      t88 = 0.1D1 - t69 + t71 * x2 + t73 - t75 * x2 + t77 * x1 + 0.2D1 *
     # t26 * x1 + t75 - 0.3D1 * t8 + t12 + t29 - 0.3D1 * t66 + 0.2D1 * t
     #83 * z + t86 - t11 + 0.2D1 * t14
      t107 = -t27 + 0.2D1 * t65 + t67 - t83 - 0.2D1 * t70 * z - 0.2D1 * 
     #x1 + 0.4D1 * t15 - t17 - 0.2D1 * t26 * t65 - 0.2D1 * t26 * t14 - 0
     #.2D1 * t26 * t67 - x3 * t74 * t65 - 0.2D1 * t71 * t67 + t71 * t77 
     #+ 0.2D1 * t26 * t66 - x2 + t70
      t109 = 0.1D1 / (t88 + t107)
      t111 = 0.1D1 / x3
      t112 = 0.1D1 / x2
      t114 = 0.1D1 / x1
      t115 = t111 * t112 * t114
      t116 = t68 * t109 * t115
      t119 = FJET(XB1, XB2, s, -t36, t41, t46, t50, -t56, t60 * t61 * t6
     #2 * t116 / 0.8D1)
      t121 = t59 * pi
      t122 = t121 * t20
      t129 = t47 * t48
      t130 = t1 * t3
      t131 = t47 * t130
      t132 = t2 * t5
      t134 = x3 * s * t130
      t135 = t51 ** 2
      t136 = Sin(t18)
      t137 = t136 ** 2
      t138 = t135 * t137
      t140 = 0.1D1 / t74
      t141 = t140 * t31
      t142 = t3 ** 2
      t147 = log(-0.4D1 * t71 * t138 * t141 * t142 * t22)
      t148 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -t
     #129, t132, 0.0D0)
      t150 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -t
     #129, t132, 0.0D0)
      t155 = lh * t57
      t163 = t60 * pi
      t165 = t112 * t114
      t169 = -(0.90D2 * t60 * pi * (t147 * t148 - t150) + 0.180D3 * t155
     # * t121 * t148) * t111 * t114 / 0.720D3 + t163 * t148 * t111 * t16
     #5 / 0.8D1
      t170 = FJET(XB1, XB2, s, -t129, t131, t132, -t134, 0.0D0, t169)
      t172 = t38 * s
      t173 = t172 * t1
      t175 = x2 * s * t1
      t176 = t7 * t135
      t177 = t137 * t140
      t178 = t38 ** 2
      t179 = t177 * t178
      t182 = log(0.4D1 * t176 * t179)
      t183 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t185 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t190 = t121 * t183
      t192 = 0.180D3 * t155 * t190
      t197 = x2 * t135
      t200 = log(0.4D1 * t197 * t179)
      t202 = t200 ** 2
      t205 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t215 = lh ** 2
      t216 = 0.180D3 * t215
      t217 = pi ** 2
      t218 = 0.30D2 * t217
      t219 = t216 - t218
      t220 = t219 * t57
      t229 = t83 * t135
      t232 = log(0.4D1 * t229 * t179)
      t242 = -(0.90D2 * t60 * pi * (-t182 * t183 + t185) - t192) * t111 
     #* t112 / 0.1440D4 - (0.90D2 * t60 * pi * (-t200 * t185 + t202 * t1
     #83 / 0.2D1 + t205) - 0.180D3 * t155 * t121 * (-t200 * t183 + t185)
     # + t220 * t190) * t112 / 0.1440D4 - t163 * t183 * t111 * t165 / 0.
     #8D1 + (0.90D2 * t60 * pi * (-t185 + t232 * t183) + t192) * t112 * 
     #t114 / 0.720D3
      t243 = FJET(XB1, XB2, s, 0.0D0, -t173, 0.0D0, t175, 0.0D0, t242)
      t247 = t2 * t3 * x2 * t31
      t248 = t172 * t130
      t249 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t3
     #7, 0.0D0, -t56)
      t259 = log(0.4D1 * t83 * t138 * t141 * t142 * t178)
      t261 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t3
     #7, 0.0D0, -t56)
      t273 = t163 * t249 * t111 * t165 / 0.8D1 + (0.90D2 * t60 * pi * (-
     #t259 * t249 + t261) - 0.180D3 * t155 * t121 * t249) * t112 * t114 
     #/ 0.720D3
      t274 = FJET(XB1, XB2, s, -t247, 0.0D0, t248, t37, -t56, t273)
      t276 = t2 * x3
      t277 = t2 * t22
      t278 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0.
     #0D0, 0.0D0, 0.0D0)
      t280 = t177 * t22
      t283 = log(-0.4D1 * x3 * t135 * t280)
      t284 = t283 ** 2
      t285 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0.
     #0D0, 0.0D0, 0.0D0)
      t288 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0.
     #0D0, 0.0D0, 0.0D0)
      t299 = t121 * t285
      t306 = log(-0.4D1 * t176 * t280)
      t313 = 0.180D3 * t155 * t299
      t321 = log(-0.4D1 * t71 * t135 * t280)
      t335 = -(0.90D2 * t60 * pi * (t278 + t284 * t285 / 0.2D1 - t283 * 
     #t288) - 0.180D3 * t155 * t121 * (-t283 * t285 + t288) + t220 * t29
     #9) * t111 / 0.1440D4 - (0.90D2 * t60 * pi * (t288 - t306 * t285) -
     # t313) * t111 * t112 / 0.1440D4 - (0.90D2 * t60 * pi * (-t321 * t2
     #85 + t288) - t313) * t111 * t114 / 0.720D3 - t163 * t285 * t111 * 
     #t165 / 0.8D1
      t336 = FJET(XB1, XB2, s, t276, 0.0D0, -t277, 0.0D0, 0.0D0, t335)
      t338 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t340 = t140 * t135
      t343 = log(0.4D1 * x3 * t137 * t340)
      t344 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t346 = t343 ** 2
      t347 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t359 = t121 * t347
      t360 = t220 * t359
      t364 = t177 * t135
      t366 = log(0.4D1 * t364)
      t369 = t366 ** 2
      t372 = (0.180D3 * t366 * lh + 0.45D2 * t369 + t216 - t218) * t57
      t376 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t391 = (-0.90D2 * t369 * lh + 0.60D2 * lh * t217 - 0.240D3 * zeta3
     # - 0.120D3 * t215 * lh - 0.15D2 * t369 * t366 - t366 * t219) * t57
      t397 = (-0.180D3 * lh - 0.90D2 * t366) * t57
      t403 = log(0.4D1 * t7 * t364)
      t410 = 0.180D3 * t155 * t359
      t417 = log(0.4D1 * t197 * t177)
      t419 = t417 ** 2
      t436 = log(0.4D1 * t71 * t364)
      t452 = log(0.4D1 * t83 * t364)
      t462 = t70 * t137
      t465 = log(0.4D1 * t462 * t340)
      t467 = t465 ** 2
      t482 = -(0.90D2 * t60 * pi * (-t338 + t343 * t344 - t346 * t347 / 
     #0.2D1) - 0.180D3 * t155 * t121 * (-t344 + t343 * t347) - t360) * t
     #111 / 0.1440D4 + t372 * t121 * t344 / 0.1440D4 + t60 * pi * t376 /
     # 0.16D2 + t391 * t359 / 0.1440D4 + t397 * t121 * t338 / 0.1440D4 -
     # (0.90D2 * t60 * pi * (t403 * t347 - t344) + t410) * t111 * t112 /
     # 0.1440D4 - (0.90D2 * t60 * pi * (-t338 + t417 * t344 - t419 * t34
     #7 / 0.2D1) - 0.180D3 * t155 * t121 * (-t344 + t417 * t347) - t360)
     # * t112 / 0.1440D4 - (0.90D2 * t60 * pi * (-t344 + t436 * t347) + 
     #t410) * t111 * t114 / 0.720D3 + t163 * t347 * t111 * t165 / 0.8D1 
     #+ (0.90D2 * t60 * pi * (-t452 * t347 + t344) - t410) * t112 * t114
     # / 0.720D3 - (0.90D2 * t60 * pi * (-t338 + t465 * t344 - t467 * t3
     #47 / 0.2D1) - 0.180D3 * t155 * t121 * (-t344 + t465 * t347) - t360
     #) * t114 / 0.720D3
      t483 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t482)
      t485 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t486 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t488 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t500 = t121 * t488
      t501 = t220 * t500
      t508 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t523 = 0.180D3 * t155 * t500
      t580 = -(0.90D2 * t60 * pi * (-t485 + t343 * t486 - t346 * t488 / 
     #0.2D1) - 0.180D3 * t155 * t121 * (-t486 + t343 * t488) - t501) * t
     #111 / 0.1440D4 + t372 * t121 * t486 / 0.1440D4 + t60 * pi * t508 /
     # 0.16D2 + t391 * t500 / 0.1440D4 + t397 * t121 * t485 / 0.1440D4 -
     # (0.90D2 * t60 * pi * (t403 * t488 - t486) + t523) * t111 * t112 /
     # 0.1440D4 - (0.90D2 * t60 * pi * (-t485 + t417 * t486 - t419 * t48
     #8 / 0.2D1) - 0.180D3 * t155 * t121 * (-t486 + t417 * t488) - t501)
     # * t112 / 0.1440D4 - (0.90D2 * t60 * pi * (-t486 + t436 * t488) + 
     #t523) * t111 * t114 / 0.720D3 + t163 * t488 * t111 * t165 / 0.8D1 
     #+ (0.90D2 * t60 * pi * (t486 - t452 * t488) - t523) * t112 * t114 
     #/ 0.720D3 - (0.90D2 * t60 * pi * (-t485 + t465 * t486 - t467 * t48
     #8 / 0.2D1) - 0.180D3 * t155 * t121 * (-t486 + t465 * t488) - t501)
     # * t114 / 0.720D3
      t581 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t580)
      t583 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0.
     #0D0, 0.0D0, 0.0D0)
      t584 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0.
     #0D0, 0.0D0, 0.0D0)
      t587 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0.
     #0D0, 0.0D0, 0.0D0)
      t598 = t121 * t584
      t609 = 0.180D3 * t155 * t598
      t627 = -(0.90D2 * t60 * pi * (t583 + t284 * t584 / 0.2D1 - t283 * 
     #t587) - 0.180D3 * t155 * t121 * (t587 - t283 * t584) + t220 * t598
     #) * t111 / 0.1440D4 - (0.90D2 * t60 * pi * (t587 - t306 * t584) - 
     #t609) * t111 * t112 / 0.1440D4 - (0.90D2 * t60 * pi * (t587 - t321
     # * t584) - t609) * t111 * t114 / 0.720D3 - t163 * t584 * t111 * t1
     #65 / 0.8D1
      t628 = FJET(XB1, XB2, s, 0.0D0, t276, 0.0D0, -t277, 0.0D0, t627)
      t631 = Sqrt(-t7 * t22)
      t632 = t19 * t631
      t633 = 0.2D1 * t632
      t635 = 0.2D1 * t632 * x2
      t638 = t2 * (-x2 - x3 + t13 - t11 - t633 + t635) * t34
      t642 = t2 * t38 * (-t7 - 0.1D1 + x3 + t633) * t34
      t643 = 0.1D1 - x2 + t67
      t644 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0.
     #0D0, 0.0D0, 0.0D0)
      t648 = t33 ** 2
      t654 = log(-0.4D1 * t7 * t138 * t140 * t178 * t22 / t648)
      t655 = t654 * t643
      t656 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0.
     #0D0, 0.0D0, 0.0D0)
      t663 = 0.1D1 / (t11 - t73 - t67 + t69 + x2 - t86 - t635 + 0.2D1 * 
     #t632 * t67 + t633 - 0.1D1)
      t667 = t155 * t59
      t668 = pi * t643
      t669 = t656 * t663
      t677 = t60 * t668
      t681 = -(0.90D2 * t60 * pi * (t643 * t644 - t655 * t656) * t663 - 
     #0.180D3 * t667 * t668 * t669) * t111 * t112 / 0.1440D4 - t677 * t6
     #69 * t115 / 0.8D1
      t682 = FJET(XB1, XB2, s, t638, 0.0D0, -t642, 0.0D0, 0.0D0, t681)
      t684 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t686 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t691 = t121 * t684
      t693 = 0.180D3 * t155 * t691
      t698 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t728 = -(0.90D2 * t60 * pi * (-t182 * t684 + t686) - t693) * t111 
     #* t112 / 0.1440D4 - (0.90D2 * t60 * pi * (t698 + t202 * t684 / 0.2
     #D1 - t200 * t686) - 0.180D3 * t155 * t121 * (t686 - t200 * t684) +
     # t220 * t691) * t112 / 0.1440D4 - t163 * t684 * t111 * t165 / 0.8D
     #1 + (0.90D2 * t60 * pi * (t232 * t684 - t686) + t693) * t112 * t11
     #4 / 0.720D3
      t729 = FJET(XB1, XB2, s, -t173, 0.0D0, t175, 0.0D0, 0.0D0, t728)
      t731 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t3
     #7, 0.0D0, -t56)
      t736 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t3
     #7, 0.0D0, -t56)
      t749 = t163 * t731 * t111 * t165 / 0.8D1 + (0.90D2 * t60 * pi * (t
     #736 - t259 * t731) - 0.180D3 * t155 * t121 * t731) * t112 * t114 /
     # 0.720D3
      t750 = FJET(XB1, XB2, s, t248, t37, -t247, 0.0D0, -t56, t749)
      t752 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t46, -t36, t50,
     # t41, -t56)
      t757 = FJET(XB1, XB2, s, t41, -t36, t50, t46, -t56, t60 * t61 * t7
     #52 * t116 / 0.8D1)
      t765 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t767 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t772 = t121 * t765
      t774 = 0.180D3 * t155 * t772
      t779 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0.
     #0D0, 0.0D0, 0.0D0)
      t809 = -(0.90D2 * t60 * pi * (-t182 * t765 + t767) - t774) * t111 
     #* t112 / 0.1440D4 - (0.90D2 * t60 * pi * (t779 + t202 * t765 / 0.2
     #D1 - t200 * t767) - 0.180D3 * t155 * t121 * (t767 - t200 * t765) +
     # t220 * t772) * t112 / 0.1440D4 - t163 * t765 * t111 * t165 / 0.8D
     #1 + (0.90D2 * t60 * pi * (t232 * t765 - t767) + t774) * t112 * t11
     #4 / 0.720D3
      t810 = FJET(XB1, XB2, s, t175, 0.0D0, -t173, 0.0D0, 0.0D0, t809)
      t812 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t813 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t815 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t827 = t121 * t815
      t828 = t220 * t827
      t835 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t850 = 0.180D3 * t155 * t827
      t907 = -(0.90D2 * t60 * pi * (-t812 + t343 * t813 - t346 * t815 / 
     #0.2D1) - 0.180D3 * t155 * t121 * (-t813 + t343 * t815) - t828) * t
     #111 / 0.1440D4 + t372 * t121 * t813 / 0.1440D4 + t60 * pi * t835 /
     # 0.16D2 + t391 * t827 / 0.1440D4 + t397 * t121 * t812 / 0.1440D4 -
     # (0.90D2 * t60 * pi * (t403 * t815 - t813) + t850) * t111 * t112 /
     # 0.1440D4 - (0.90D2 * t60 * pi * (-t812 + t417 * t813 - t419 * t81
     #5 / 0.2D1) - 0.180D3 * t155 * t121 * (-t813 + t417 * t815) - t828)
     # * t112 / 0.1440D4 - (0.90D2 * t60 * pi * (-t813 + t436 * t815) + 
     #t850) * t111 * t114 / 0.720D3 + t163 * t815 * t111 * t165 / 0.8D1 
     #+ (0.90D2 * t60 * pi * (-t452 * t815 + t813) - t850) * t112 * t114
     # / 0.720D3 - (0.90D2 * t60 * pi * (-t812 - t467 * t815 / 0.2D1 + t
     #465 * t813) - 0.180D3 * t155 * t121 * (-t813 + t465 * t815) - t828
     #) * t114 / 0.720D3
      t908 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t907)
      t910 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t37
     #, 0.0D0, 0.0D0)
      t912 = t31 * t142
      t916 = log(0.4D1 * t71 * t137 * t340 * t912)
      t917 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t37
     #, 0.0D0, 0.0D0)
      t923 = t121 * t917
      t925 = 0.180D3 * t155 * t923
      t937 = log(0.4D1 * t229 * t177 * t912)
      t952 = log(0.4D1 * t462 * t140 * t135 * t31 * t142)
      t954 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t37
     #, 0.0D0, 0.0D0)
      t955 = t952 ** 2
      t971 = -(0.90D2 * t60 * pi * (t910 - t916 * t917) - t925) * t111 *
     # t114 / 0.720D3 - t163 * t917 * t111 * t165 / 0.8D1 + (0.90D2 * t6
     #0 * pi * (-t910 + t937 * t917) + t925) * t112 * t114 / 0.720D3 - (
     #0.90D2 * t60 * pi * (-t952 * t910 + t954 + t955 * t917 / 0.2D1) - 
     #0.180D3 * t155 * t121 * (t910 - t952 * t917) + t220 * t923) * t114
     # / 0.720D3
      t972 = FJET(XB1, XB2, s, t37, -t4, 0.0D0, 0.0D0, 0.0D0, t971)
      t974 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t975 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t977 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t989 = t121 * t977
      t990 = t220 * t989
      t997 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t1009 = 0.180D3 * t155 * t989
      t1069 = -(0.90D2 * t60 * pi * (-t974 + t343 * t975 - t346 * t977 /
     # 0.2D1) - 0.180D3 * t155 * t121 * (-t975 + t343 * t977) - t990) * 
     #t111 / 0.1440D4 + t397 * t121 * t974 / 0.1440D4 + t60 * pi * t997 
     #/ 0.16D2 + t391 * t989 / 0.1440D4 - (0.90D2 * t60 * pi * (-t975 + 
     #t403 * t977) + t1009) * t111 * t112 / 0.1440D4 - (0.90D2 * t60 * p
     #i * (t417 * t975 - t419 * t977 / 0.2D1 - t974) - 0.180D3 * t155 * 
     #t121 * (-t975 + t417 * t977) - t990) * t112 / 0.1440D4 + t372 * t1
     #21 * t975 / 0.1440D4 - (0.90D2 * t60 * pi * (t436 * t977 - t975) +
     # t1009) * t111 * t114 / 0.720D3 + t163 * t977 * t111 * t165 / 0.8D
     #1 + (0.90D2 * t60 * pi * (-t452 * t977 + t975) - t1009) * t112 * t
     #114 / 0.720D3 - (0.90D2 * t60 * pi * (-t467 * t977 / 0.2D1 + t465 
     #* t975 - t974) - 0.180D3 * t155 * t121 * (t465 * t977 - t975) - t9
     #90) * t114 / 0.720D3
      t1070 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1069)
      t1072 = t119 * t57 * t122 * t62 * t68 * t109 * t115 / 0.8D1 + t170
     # * t169 + t243 * t242 + t274 * t273 + t336 * t335 + t483 * t482 + 
     #t581 * t580 + t628 * t627 + t682 * t681 + t729 * t728 + t750 * t74
     #9 + t757 * t57 * t122 * t752 * t68 * t109 * t115 / 0.8D1 + t810 * 
     #t809 + t908 * t907 + t972 * t971 + t1070 * t1069
      t1073 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1074 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1080 = t121 * t1074
      t1082 = 0.180D3 * t155 * t1080
      t1100 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1117 = -(0.90D2 * t60 * pi * (t1073 - t916 * t1074) - t1082) * t1
     #11 * t114 / 0.720D3 - t163 * t1074 * t111 * t165 / 0.8D1 + (0.90D2
     # * t60 * pi * (t937 * t1074 - t1073) + t1082) * t112 * t114 / 0.72
     #0D3 - (0.90D2 * t60 * pi * (t1100 - t952 * t1073 + t955 * t1074 / 
     #0.2D1) - 0.180D3 * t155 * t121 * (t1073 - t952 * t1074) + t220 * t
     #1080) * t114 / 0.720D3
      t1118 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t37, -t4, 0.0D0, t1117)
      t1120 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0
     #.0D0, 0.0D0, 0.0D0)
      t1121 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0
     #.0D0, 0.0D0, 0.0D0)
      t1124 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0
     #.0D0, 0.0D0, 0.0D0)
      t1135 = t121 * t1121
      t1146 = 0.180D3 * t155 * t1135
      t1164 = -(0.90D2 * t60 * pi * (t1120 + t284 * t1121 / 0.2D1 - t283
     # * t1124) - 0.180D3 * t155 * t121 * (-t283 * t1121 + t1124) + t220
     # * t1135) * t111 / 0.1440D4 - (0.90D2 * t60 * pi * (t1124 - t306 *
     # t1121) - t1146) * t111 * t112 / 0.1440D4 - (0.90D2 * t60 * pi * (
     #-t321 * t1121 + t1124) - t1146) * t111 * t114 / 0.720D3 - t163 * t
     #1121 * t111 * t165 / 0.8D1
      t1165 = FJET(XB1, XB2, s, -t277, 0.0D0, t276, 0.0D0, 0.0D0, t1164)
      t1167 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1168 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1174 = t121 * t1168
      t1176 = 0.180D3 * t155 * t1174
      t1194 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1211 = -(0.90D2 * t60 * pi * (t1167 - t916 * t1168) - t1176) * t1
     #11 * t114 / 0.720D3 - t163 * t1168 * t111 * t165 / 0.8D1 + (0.90D2
     # * t60 * pi * (-t1167 + t937 * t1168) + t1176) * t112 * t114 / 0.7
     #20D3 - (0.90D2 * t60 * pi * (t1194 + t955 * t1168 / 0.2D1 - t952 *
     # t1167) - 0.180D3 * t155 * t121 * (-t952 * t1168 + t1167) + t220 *
     # t1174) * t114 / 0.720D3
      t1212 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t37, 0.0D0, t1211)
      t1214 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t
     #37, 0.0D0, -t56)
      t1220 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t
     #37, 0.0D0, -t56)
      t1232 = t163 * t1214 * t111 * t165 / 0.8D1 + (0.90D2 * t60 * pi * 
     #(-t259 * t1214 + t1220) - 0.180D3 * t155 * t121 * t1214) * t112 * 
     #t114 / 0.720D3
      t1233 = FJET(XB1, XB2, s, t37, t248, 0.0D0, -t247, -t56, t1232)
      t1235 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t46, -t36, t50
     #, t41, -t56)
      t1240 = FJET(XB1, XB2, s, t50, t46, t41, -t36, -t56, t60 * t61 * t
     #1235 * t116 / 0.8D1)
      t1248 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1249 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1255 = t121 * t1249
      t1257 = 0.180D3 * t155 * t1255
      t1276 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t3
     #7, 0.0D0, 0.0D0)
      t1292 = -(0.90D2 * t60 * pi * (t1248 - t916 * t1249) - t1257) * t1
     #11 * t114 / 0.720D3 - t163 * t1249 * t111 * t165 / 0.8D1 + (0.90D2
     # * t60 * pi * (-t1248 + t937 * t1249) + t1257) * t112 * t114 / 0.7
     #20D3 - (0.90D2 * t60 * pi * (-t952 * t1248 + t1276 + t955 * t1249 
     #/ 0.2D1) - 0.180D3 * t155 * t121 * (t1248 - t952 * t1249) + t220 *
     # t1255) * t114 / 0.720D3
      t1293 = FJET(XB1, XB2, s, -t4, t37, 0.0D0, 0.0D0, 0.0D0, t1292)
      t1295 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -
     #t129, t132, 0.0D0)
      t1296 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -
     #t129, t132, 0.0D0)
      t1313 = -(0.90D2 * t60 * pi * (-t1295 + t147 * t1296) + 0.180D3 * 
     #t155 * t121 * t1296) * t111 * t114 / 0.720D3 + t163 * t1296 * t111
     # * t165 / 0.8D1
      t1314 = FJET(XB1, XB2, s, t132, -t134, -t129, t131, 0.0D0, t1313)
      t1316 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -
     #t129, t132, 0.0D0)
      t1317 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -
     #t129, t132, 0.0D0)
      t1334 = -(0.90D2 * t60 * pi * (-t1316 + t147 * t1317) + 0.180D3 * 
     #t155 * t121 * t1317) * t111 * t114 / 0.720D3 + t163 * t1317 * t111
     # * t165 / 0.8D1
      t1335 = FJET(XB1, XB2, s, t131, -t129, -t134, t132, 0.0D0, t1334)
      t1337 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t
     #37, 0.0D0, -t56)
      t1343 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t248, -t247, t
     #37, 0.0D0, -t56)
      t1355 = t163 * t1337 * t111 * t165 / 0.8D1 + (0.90D2 * t60 * pi * 
     #(-t259 * t1337 + t1343) - 0.180D3 * t155 * t121 * t1337) * t112 * 
     #t114 / 0.720D3
      t1356 = FJET(XB1, XB2, s, 0.0D0, -t247, t37, t248, -t56, t1355)
      t1358 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0
     #.0D0, 0.0D0, 0.0D0)
      t1359 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0
     #.0D0, 0.0D0, 0.0D0)
      t1362 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t277, t276, 0
     #.0D0, 0.0D0, 0.0D0)
      t1373 = t121 * t1359
      t1384 = 0.180D3 * t155 * t1373
      t1402 = -(0.90D2 * t60 * pi * (t1358 + t284 * t1359 / 0.2D1 - t283
     # * t1362) - 0.180D3 * t155 * t121 * (-t283 * t1359 + t1362) + t220
     # * t1373) * t111 / 0.1440D4 - (0.90D2 * t60 * pi * (-t306 * t1359 
     #+ t1362) - t1384) * t111 * t112 / 0.1440D4 - (0.90D2 * t60 * pi * 
     #(-t321 * t1359 + t1362) - t1384) * t111 * t114 / 0.720D3 - t163 * 
     #t1359 * t111 * t165 / 0.8D1
      t1403 = FJET(XB1, XB2, s, 0.0D0, -t277, 0.0D0, t276, 0.0D0, t1402)
      t1405 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0
     #.0D0, 0.0D0, 0.0D0)
      t1407 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0
     #.0D0, 0.0D0, 0.0D0)
      t1412 = t121 * t1405
      t1414 = 0.180D3 * t155 * t1412
      t1422 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t173, t175, 0
     #.0D0, 0.0D0, 0.0D0)
      t1449 = -(0.90D2 * t60 * pi * (-t182 * t1405 + t1407) - t1414) * t
     #111 * t112 / 0.1440D4 - (0.90D2 * t60 * pi * (t202 * t1405 / 0.2D1
     # - t200 * t1407 + t1422) - 0.180D3 * t155 * t121 * (-t200 * t1405 
     #+ t1407) + t220 * t1412) * t112 / 0.1440D4 - t163 * t1405 * t111 *
     # t165 / 0.8D1 + (0.90D2 * t60 * pi * (t232 * t1405 - t1407) + t141
     #4) * t112 * t114 / 0.720D3
      t1450 = FJET(XB1, XB2, s, 0.0D0, t175, 0.0D0, -t173, 0.0D0, t1449)
      t1452 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -
     #t129, t132, 0.0D0)
      t1453 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t131, -t134, -
     #t129, t132, 0.0D0)
      t1470 = -(0.90D2 * t60 * pi * (-t1452 + t147 * t1453) + 0.180D3 * 
     #t155 * t121 * t1453) * t111 * t114 / 0.720D3 + t163 * t1453 * t111
     # * t165 / 0.8D1
      t1471 = FJET(XB1, XB2, s, -t134, t132, t131, -t129, 0.0D0, t1470)
      t1473 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0
     #.0D0, 0.0D0, 0.0D0)
      t1475 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0
     #.0D0, 0.0D0, 0.0D0)
      t1482 = t1475 * t663
      t1493 = -(0.90D2 * t60 * pi * (t643 * t1473 - t655 * t1475) * t663
     # - 0.180D3 * t667 * t668 * t1482) * t111 * t112 / 0.1440D4 - t677 
     #* t1482 * t115 / 0.8D1
      t1494 = FJET(XB1, XB2, s, 0.0D0, -t642, 0.0D0, t638, 0.0D0, t1493)
      t1496 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0
     #.0D0, 0.0D0, 0.0D0)
      t1498 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0
     #.0D0, 0.0D0, 0.0D0)
      t1505 = t1498 * t663
      t1516 = -(0.90D2 * t60 * pi * (t643 * t1496 - t655 * t1498) * t663
     # - 0.180D3 * t667 * t668 * t1505) * t111 * t112 / 0.1440D4 - t677 
     #* t1505 * t115 / 0.8D1
      t1517 = FJET(XB1, XB2, s, 0.0D0, t638, 0.0D0, -t642, 0.0D0, t1516)
      t1519 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0
     #.0D0, 0.0D0, 0.0D0)
      t1521 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t642, t638, 0
     #.0D0, 0.0D0, 0.0D0)
      t1528 = t1521 * t663
      t1539 = -(0.90D2 * t60 * pi * (t643 * t1519 - t655 * t1521) * t663
     # - 0.180D3 * t667 * t668 * t1528) * t111 * t112 / 0.1440D4 - t677 
     #* t1528 * t115 / 0.8D1
      t1540 = FJET(XB1, XB2, s, -t642, 0.0D0, t638, 0.0D0, 0.0D0, t1539)
      t1542 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t46, -t36, t50
     #, t41, -t56)
      t1547 = FJET(XB1, XB2, s, t46, t50, -t36, t41, -t56, t60 * t61 * t
     #1542 * t116 / 0.8D1)
      t1555 = t1118 * t1117 + t1165 * t1164 + t1212 * t1211 + t1233 * t1
     #232 + t1240 * t57 * t122 * t1235 * t68 * t109 * t115 / 0.8D1 + t12
     #93 * t1292 + t1314 * t1313 + t1335 * t1334 + t1356 * t1355 + t1403
     # * t1402 + t1450 * t1449 + t1471 * t1470 + t1494 * t1493 + t1517 *
     # t1516 + t1540 * t1539 + t1547 * t57 * t122 * t1542 * t68 * t109 *
     # t115 / 0.8D1
      rrqg2qght8s2e0 = t1072 + t1555

      end function



      doubleprecision function rrqg2qght8s2em1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t5 = t2 * x1
      t6 = 0.1D1 / t1
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = t6 * t8
      t10 = t9 * pi
      t11 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5, 
     #0.0D0, 0.0D0)
      t12 = 0.1D1 / x2
      t14 = 0.1D1 / x1
      t18 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5, 
     #0.0D0, 0.0D0)
      t19 = x1 ** 2
      t20 = x4 * pi
      t21 = Sin(t20)
      t22 = t21 ** 2
      t23 = t19 * t22
      t24 = z ** 2
      t25 = 0.1D1 / t24
      t27 = t1 ** 2
      t28 = t27 ** 2
      t31 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t33 = t3 ** 2
      t37 = log(0.4D1 * t23 * t25 * t28 * t31 * t33)
      t43 = lh * t6
      t44 = t8 * pi
      t51 = 0.1D1 / x3
      t56 = -t10 * t11 * t12 * t14 / 0.8D1 - (0.90D2 * t9 * pi * (t18 - 
     #t37 * t11) - 0.180D3 * t43 * t44 * t11) * t14 / 0.720D3 - t10 * t1
     #1 * t51 * t14 / 0.8D1
      t57 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t56)
      t59 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = t25 * t28
      t64 = log(0.4D1 * x3 * t22 * t61)
      t65 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t71 = t44 * t65
      t73 = 0.180D3 * t43 * t71
      t77 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t82 = t25 * t22
      t85 = log(0.4D1 * t82 * t28)
      t88 = (-0.180D3 * lh - 0.90D2 * t85) * t6
      t94 = t85 ** 2
      t96 = lh ** 2
      t98 = pi ** 2
      t101 = (0.180D3 * t85 * lh + 0.45D2 * t94 + 0.180D3 * t96 - 0.30D2
     # * t98) * t6
      t104 = t65 * t51
      t108 = x2 * t28
      t111 = log(0.4D1 * t108 * t82)
      t126 = log(0.4D1 * t23 * t61)
      t138 = -(0.90D2 * t9 * pi * (-t59 + t64 * t65) + t73) * t51 / 0.14
     #40D4 + t9 * pi * t77 / 0.16D2 + t88 * t44 * t59 / 0.1440D4 + t101 
     #* t71 / 0.1440D4 + t10 * t104 * t12 / 0.16D2 - (0.90D2 * t9 * pi *
     # (-t59 + t111 * t65) + t73) * t12 / 0.1440D4 + t10 * t65 * t12 * t
     #14 / 0.8D1 - (0.90D2 * t9 * pi * (-t59 + t126 * t65) + t73) * t14 
     #/ 0.720D3 + t10 * t104 * t14 / 0.8D1
      t139 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t138)
      t141 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5,
     # 0.0D0, 0.0D0)
      t146 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5,
     # 0.0D0, 0.0D0)
      t162 = -t10 * t141 * t12 * t14 / 0.8D1 - (0.90D2 * t9 * pi * (t146
     # - t37 * t141) - 0.180D3 * t43 * t44 * t141) * t14 / 0.720D3 - t10
     # * t141 * t51 * t14 / 0.8D1
      t163 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t162)
      t165 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5,
     # 0.0D0, 0.0D0)
      t170 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5,
     # 0.0D0, 0.0D0)
      t186 = -t10 * t165 * t12 * t14 / 0.8D1 - (0.90D2 * t9 * pi * (t170
     # - t37 * t165) - 0.180D3 * t43 * t44 * t165) * t14 / 0.720D3 - t10
     # * t165 * t51 * t14 / 0.8D1
      t187 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t186)
      t189 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5,
     # 0.0D0, 0.0D0)
      t195 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, 0.0D0, t5,
     # 0.0D0, 0.0D0)
      t210 = -t10 * t189 * t12 * t14 / 0.8D1 - (0.90D2 * t9 * pi * (-t37
     # * t189 + t195) - 0.180D3 * t43 * t44 * t189) * t14 / 0.720D3 - t1
     #0 * t189 * t51 * t14 / 0.8D1
      t211 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t210)
      t213 = -0.1D1 + x3
      t214 = t2 * t213
      t215 = t2 * x3
      t220 = log(-0.4D1 * x3 * t28 * t82 * t213)
      t221 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t223 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t234 = t221 * t51
      t241 = -(0.90D2 * t9 * pi * (-t220 * t221 + t223) - 0.180D3 * t43 
     #* t44 * t221) * t51 / 0.1440D4 - t10 * t234 * t14 / 0.8D1 - t10 * 
     #t234 * t12 / 0.16D2
      t242 = FJET(XB1, XB2, s, 0.0D0, -t214, 0.0D0, t215, 0.0D0, t241)
      t244 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t245 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t251 = t44 * t245
      t253 = 0.180D3 * t43 * t251
      t269 = t245 * t51
      t289 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t293 = -(0.90D2 * t9 * pi * (-t244 + t64 * t245) + t253) * t51 / 0
     #.1440D4 + t10 * t245 * t12 * t14 / 0.8D1 - (0.90D2 * t9 * pi * (t1
     #26 * t245 - t244) + t253) * t14 / 0.720D3 + t10 * t269 * t14 / 0.8
     #D1 + t88 * t44 * t244 / 0.1440D4 + t101 * t251 / 0.1440D4 + t10 * 
     #t269 * t12 / 0.16D2 - (0.90D2 * t9 * pi * (-t244 + t111 * t245) + 
     #t253) * t12 / 0.1440D4 + t9 * pi * t289 / 0.16D2
      t294 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t293)
      t296 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t297 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t303 = t44 * t297
      t305 = 0.180D3 * t43 * t303
      t309 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t318 = t297 * t51
      t345 = -(0.90D2 * t9 * pi * (-t296 + t64 * t297) + t305) * t51 / 0
     #.1440D4 + t9 * pi * t309 / 0.16D2 + t88 * t44 * t296 / 0.1440D4 + 
     #t101 * t303 / 0.1440D4 + t10 * t318 * t12 / 0.16D2 - (0.90D2 * t9 
     #* pi * (-t296 + t111 * t297) + t305) * t12 / 0.1440D4 + t10 * t297
     # * t12 * t14 / 0.8D1 - (0.90D2 * t9 * pi * (-t296 + t126 * t297) +
     # t305) * t14 / 0.720D3 + t10 * t318 * t14 / 0.8D1
      t346 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t345)
      t348 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t350 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t361 = t348 * t51
      t368 = -(0.90D2 * t9 * pi * (-t220 * t348 + t350) - 0.180D3 * t43 
     #* t44 * t348) * t51 / 0.1440D4 - t10 * t361 * t14 / 0.8D1 - t10 * 
     #t361 * t12 / 0.16D2
      t369 = FJET(XB1, XB2, s, -t214, 0.0D0, t215, 0.0D0, 0.0D0, t368)
      t371 = -0.1D1 + x2
      t372 = t371 * s
      t373 = t372 * t1
      t375 = x2 * s * t1
      t376 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t385 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t386 = t371 ** 2
      t390 = log(0.4D1 * t108 * t82 * t386)
      t402 = -t10 * t376 * t12 * t14 / 0.8D1 - t10 * t376 * t51 * t12 / 
     #0.16D2 - (0.90D2 * t9 * pi * (t385 - t390 * t376) - 0.180D3 * t43 
     #* t44 * t376) * t12 / 0.1440D4
      t403 = FJET(XB1, XB2, s, -t373, 0.0D0, t375, 0.0D0, 0.0D0, t402)
      t405 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t411 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t426 = -t10 * t405 * t51 * t12 / 0.16D2 - (0.90D2 * t9 * pi * (-t3
     #90 * t405 + t411) - 0.180D3 * t43 * t44 * t405) * t12 / 0.1440D4 -
     # t10 * t405 * t12 * t14 / 0.8D1
      t427 = FJET(XB1, XB2, s, 0.0D0, t375, 0.0D0, -t373, 0.0D0, t426)
      t429 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t431 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t442 = t429 * t51
      t449 = -(0.90D2 * t9 * pi * (-t220 * t429 + t431) - 0.180D3 * t43 
     #* t44 * t429) * t51 / 0.1440D4 - t10 * t442 * t14 / 0.8D1 - t10 * 
     #t442 * t12 / 0.16D2
      t450 = FJET(XB1, XB2, s, t215, 0.0D0, -t214, 0.0D0, 0.0D0, t449)
      t452 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t453 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t459 = t44 * t453
      t461 = 0.180D3 * t43 * t459
      t465 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t474 = t453 * t51
      t501 = -(0.90D2 * t9 * pi * (-t452 + t64 * t453) + t461) * t51 / 0
     #.1440D4 + t9 * pi * t465 / 0.16D2 + t88 * t44 * t452 / 0.1440D4 + 
     #t101 * t459 / 0.1440D4 + t10 * t474 * t12 / 0.16D2 - (0.90D2 * t9 
     #* pi * (-t452 + t111 * t453) + t461) * t12 / 0.1440D4 + t10 * t453
     # * t12 * t14 / 0.8D1 - (0.90D2 * t9 * pi * (-t452 + t126 * t453) +
     # t461) * t14 / 0.720D3 + t10 * t474 * t14 / 0.8D1
      t502 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t501)
      t504 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t510 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t525 = -t10 * t504 * t51 * t12 / 0.16D2 - (0.90D2 * t9 * pi * (-t3
     #90 * t504 + t510) - 0.180D3 * t43 * t44 * t504) * t12 / 0.1440D4 -
     # t10 * t504 * t12 * t14 / 0.8D1
      t526 = FJET(XB1, XB2, s, 0.0D0, -t373, 0.0D0, t375, 0.0D0, t525)
      t528 = t57 * t56 + t139 * t138 + t163 * t162 + t187 * t186 + t211 
     #* t210 + t242 * t241 + t294 * t293 + t346 * t345 + t369 * t368 + t
     #403 * t402 + t427 * t426 + t450 * t449 + t502 * t501 + t526 * t525
      t529 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t538 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t550 = -t10 * t529 * t12 * t14 / 0.8D1 - t10 * t529 * t51 * t12 / 
     #0.16D2 - (0.90D2 * t9 * pi * (t538 - t390 * t529) - 0.180D3 * t43 
     #* t44 * t529) * t12 / 0.1440D4
      t551 = FJET(XB1, XB2, s, t375, 0.0D0, -t373, 0.0D0, 0.0D0, t550)
      t553 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t554 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t214, t215, 0.
     #0D0, 0.0D0, 0.0D0)
      t566 = t554 * t51
      t573 = -(0.90D2 * t9 * pi * (t553 - t220 * t554) - 0.180D3 * t43 *
     # t44 * t554) * t51 / 0.1440D4 - t10 * t566 * t12 / 0.16D2 - t10 * 
     #t566 * t14 / 0.8D1
      t574 = FJET(XB1, XB2, s, 0.0D0, t215, 0.0D0, -t214, 0.0D0, t573)
      t576 = x2 * x3
      t577 = cos(t20)
      t579 = Sqrt(-t576 * t213)
      t580 = t577 * t579
      t581 = 0.2D1 * t580
      t585 = 0.1D1 / (-0.1D1 + t576)
      t587 = t2 * t371 * (-t576 - 0.1D1 + x3 + t581) * t585
      t589 = x2 ** 2
      t590 = t589 * x3
      t592 = 0.2D1 * t580 * x2
      t595 = t2 * (-x2 - x3 + 0.3D1 * t576 - t590 - t581 + t592) * t585
      t596 = x2 * z
      t597 = 0.1D1 - x2 + t596
      t599 = t9 * pi * t597
      t600 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t587, t595, 0.
     #0D0, 0.0D0, 0.0D0)
      t607 = 0.1D1 / (t590 - t590 * z - t596 + t576 * z + x2 - 0.2D1 * t
     #576 - t592 + 0.2D1 * t580 * t596 + t581 - 0.1D1)
      t609 = t51 * t12
      t613 = FJET(XB1, XB2, s, 0.0D0, -t587, 0.0D0, t595, 0.0D0, -t599 *
     # t600 * t607 * t609 / 0.16D2)
      t618 = t607 * t51 * t12
      t622 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t587, t595, 0.
     #0D0, 0.0D0, 0.0D0)
      t627 = FJET(XB1, XB2, s, -t587, 0.0D0, t595, 0.0D0, 0.0D0, -t599 *
     # t622 * t607 * t609 / 0.16D2)
      t634 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t587, t595, 0.
     #0D0, 0.0D0, 0.0D0)
      t639 = FJET(XB1, XB2, s, t595, 0.0D0, -t587, 0.0D0, 0.0D0, -t599 *
     # t634 * t607 * t609 / 0.16D2)
      t646 = t213 * s
      t647 = t1 * t3
      t648 = t646 * t647
      t650 = t646 * t1 * x1
      t652 = x3 * s * t647
      t654 = t2 * x1 * x3
      t655 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t648, -t652, -t
     #650, t654, 0.0D0)
      t660 = FJET(XB1, XB2, s, t648, -t650, -t652, t654, 0.0D0, t10 * t6
     #55 * t51 * t14 / 0.8D1)
      t664 = t51 * t14
      t668 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t648, -t652, -t
     #650, t654, 0.0D0)
      t673 = FJET(XB1, XB2, s, t654, -t652, -t650, t648, 0.0D0, t10 * t6
     #68 * t51 * t14 / 0.8D1)
      t682 = t2 * t3 * x2 * t31
      t683 = t372 * t647
      t688 = s * t27 * x2 * t3 * x1 * t31
      t689 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t683, -t682, t5
     #, 0.0D0, -t688)
      t694 = FJET(XB1, XB2, s, -t682, 0.0D0, t683, t5, -t688, t10 * t689
     # * t12 * t14 / 0.8D1)
      t698 = t12 * t14
      t702 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t648, -t652, -t
     #650, t654, 0.0D0)
      t707 = FJET(XB1, XB2, s, -t650, t648, t654, -t652, 0.0D0, t10 * t7
     #02 * t51 * t14 / 0.8D1)
      t714 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t683, -t682, t5
     #, 0.0D0, -t688)
      t719 = FJET(XB1, XB2, s, 0.0D0, -t682, t5, t683, -t688, t10 * t714
     # * t12 * t14 / 0.8D1)
      t726 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t648, -t652, -t
     #650, t654, 0.0D0)
      t731 = FJET(XB1, XB2, s, -t652, t654, t648, -t650, 0.0D0, t10 * t7
     #26 * t51 * t14 / 0.8D1)
      t738 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t683, -t682, t5
     #, 0.0D0, -t688)
      t743 = FJET(XB1, XB2, s, t5, t683, 0.0D0, -t682, -t688, t10 * t738
     # * t12 * t14 / 0.8D1)
      t750 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t683, -t682, t5
     #, 0.0D0, -t688)
      t755 = FJET(XB1, XB2, s, t683, t5, -t682, 0.0D0, -t688, t10 * t750
     # * t12 * t14 / 0.8D1)
      t762 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t587, t595, 0.
     #0D0, 0.0D0, 0.0D0)
      t767 = FJET(XB1, XB2, s, 0.0D0, t595, 0.0D0, -t587, 0.0D0, -t599 *
     # t762 * t607 * t609 / 0.16D2)
      t774 = t551 * t550 + t574 * t573 - t613 * t6 * t44 * t597 * t600 *
     # t618 / 0.16D2 - t627 * t6 * t44 * t597 * t622 * t618 / 0.16D2 - t
     #639 * t6 * t44 * t597 * t634 * t618 / 0.16D2 + t660 * t6 * t8 * pi
     # * t655 * t664 / 0.8D1 + t673 * t6 * t8 * pi * t668 * t664 / 0.8D1
     # + t694 * t6 * t8 * pi * t689 * t698 / 0.8D1 + t707 * t6 * t8 * pi
     # * t702 * t664 / 0.8D1 + t719 * t6 * t8 * pi * t714 * t698 / 0.8D1
     # + t731 * t6 * t8 * pi * t726 * t664 / 0.8D1 + t743 * t6 * t8 * pi
     # * t738 * t698 / 0.8D1 + t755 * t6 * t8 * pi * t750 * t698 / 0.8D1
     # - t767 * t6 * t44 * t597 * t762 * t618 / 0.16D2
      rrqg2qght8s2em1 = t528 + t774

      end function



      doubleprecision function rrqg2qght8s2em2
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = pi * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = z ** 2
      t29 = Sin(x4 * pi)
      t30 = t29 ** 2
      t32 = t1 ** 2
      t33 = t32 ** 2
      t36 = log(0.4D1 / t26 * t30 * t33)
      t39 = (-0.180D3 * lh - 0.90D2 * t36) * t3
      t40 = t5 * pi
      t44 = t6 * t8 * t9 / 0.16D2 + t6 * t8 * t13 / 0.16D2 + t6 * t8 * t
     #17 / 0.8D1 + t6 * pi * t21 / 0.16D2 + t39 * t40 * t7 / 0.1440D4
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t44)
      t47 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t48 = pi * t47
      t58 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t65 = t6 * t48 * t9 / 0.16D2 + t6 * t48 * t13 / 0.16D2 + t6 * t48 
     #* t17 / 0.8D1 + t6 * pi * t58 / 0.16D2 + t39 * t40 * t47 / 0.1440D
     #4
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t65)
      t68 = t2 * x1
      t70 = t2 * (-0.1D1 + x1)
      t71 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t70, 0.0D0, t68
     #, 0.0D0, 0.0D0)
      t73 = pi * t71 * t17
      t76 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t68, -t70, 0.0D0, -t6 * t73 
     #/ 0.8D1)
      t81 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t70, 0.0D0, t68
     #, 0.0D0, 0.0D0)
      t83 = pi * t81 * t17
      t86 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t70, t68, 0.0D0, -t6 * t83 
     #/ 0.8D1)
      t91 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t92 = pi * t91
      t102 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t109 = t6 * t92 * t9 / 0.16D2 + t6 * t92 * t13 / 0.16D2 + t6 * t92
     # * t17 / 0.8D1 + t6 * pi * t102 / 0.16D2 + t39 * t40 * t91 / 0.144
     #0D4
      t110 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t109)
      t112 = t2 * x3
      t114 = t2 * (-0.1D1 + x3)
      t115 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t114, t112, 0.
     #0D0, 0.0D0, 0.0D0)
      t117 = pi * t115 * t9
      t120 = FJET(XB1, XB2, s, 0.0D0, t112, 0.0D0, -t114, 0.0D0, -t6 * t
     #117 / 0.16D2)
      t126 = x2 * s * t1
      t129 = (-0.1D1 + x2) * s * t1
      t130 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t129, t126, 0.
     #0D0, 0.0D0, 0.0D0)
      t132 = pi * t130 * t13
      t135 = FJET(XB1, XB2, s, 0.0D0, t126, 0.0D0, -t129, 0.0D0, -t6 * t
     #132 / 0.16D2)
      t140 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t114, t112, 0.
     #0D0, 0.0D0, 0.0D0)
      t142 = pi * t140 * t9
      t145 = FJET(XB1, XB2, s, 0.0D0, -t114, 0.0D0, t112, 0.0D0, -t6 * t
     #142 / 0.16D2)
      t150 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t129, t126, 0.
     #0D0, 0.0D0, 0.0D0)
      t152 = pi * t150 * t13
      t155 = FJET(XB1, XB2, s, 0.0D0, -t129, 0.0D0, t126, 0.0D0, -t6 * t
     #152 / 0.16D2)
      t160 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t161 = pi * t160
      t171 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t178 = t6 * t161 * t9 / 0.16D2 + t39 * t40 * t160 / 0.1440D4 + t6 
     #* t161 * t13 / 0.16D2 + t6 * pi * t171 / 0.16D2 + t6 * t161 * t17 
     #/ 0.8D1
      t179 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t178)
      t181 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t70, 0.0D0, t6
     #8, 0.0D0, 0.0D0)
      t183 = pi * t181 * t17
      t186 = FJET(XB1, XB2, s, t68, -t70, 0.0D0, 0.0D0, 0.0D0, -t6 * t18
     #3 / 0.8D1)
      t191 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t114, t112, 0.
     #0D0, 0.0D0, 0.0D0)
      t193 = pi * t191 * t9
      t196 = FJET(XB1, XB2, s, t112, 0.0D0, -t114, 0.0D0, 0.0D0, -t6 * t
     #193 / 0.16D2)
      t201 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t129, t126, 0.
     #0D0, 0.0D0, 0.0D0)
      t203 = pi * t201 * t13
      t206 = FJET(XB1, XB2, s, t126, 0.0D0, -t129, 0.0D0, 0.0D0, -t6 * t
     #203 / 0.16D2)
      t211 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t114, t112, 0.
     #0D0, 0.0D0, 0.0D0)
      t213 = pi * t211 * t9
      t216 = FJET(XB1, XB2, s, -t114, 0.0D0, t112, 0.0D0, 0.0D0, -t6 * t
     #213 / 0.16D2)
      t221 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t70, 0.0D0, t6
     #8, 0.0D0, 0.0D0)
      t223 = pi * t221 * t17
      t226 = FJET(XB1, XB2, s, -t70, t68, 0.0D0, 0.0D0, 0.0D0, -t6 * t22
     #3 / 0.8D1)
      t231 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t129, t126, 0.
     #0D0, 0.0D0, 0.0D0)
      t233 = pi * t231 * t13
      t236 = FJET(XB1, XB2, s, -t129, 0.0D0, t126, 0.0D0, 0.0D0, -t6 * t
     #233 / 0.16D2)
      rrqg2qght8s2em2 = t45 * t44 + t66 * t65 - t76 * t3 * t5 * t73 / 0.
     #8D1 - t86 * t3 * t5 * t83 / 0.8D1 + t110 * t109 - t120 * t3 * t5 *
     # t117 / 0.16D2 - t135 * t3 * t5 * t132 / 0.16D2 - t145 * t3 * t5 *
     # t142 / 0.16D2 - t155 * t3 * t5 * t152 / 0.16D2 + t179 * t178 - t1
     #86 * t3 * t5 * t183 / 0.8D1 - t196 * t3 * t5 * t193 / 0.16D2 - t20
     #6 * t3 * t5 * t203 / 0.16D2 - t216 * t3 * t5 * t213 / 0.16D2 - t22
     #6 * t3 * t5 * t223 / 0.8D1 - t236 * t3 * t5 * t233 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s2em3
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * pi * 
     #t7 / 0.16D2)
      t13 = t5 * pi
      t16 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * pi * 
     #t16 / 0.16D2)
      t24 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t24 / 0.16D2)
      t32 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t32 / 0.16D2)
      rrqg2qght8s2em3 = t11 * t3 * t13 * t7 / 0.16D2 + t20 * t3 * t13 * 
     #t16 / 0.16D2 + t28 * t3 * t13 * t24 / 0.16D2 + t36 * t3 * t13 * t3
     #2 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s2em4
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght8s2em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght8s3e1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t8 * t18)
      t22 = t21 ** 2
      t23 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t26 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t28 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t29 = z * t28
      t30 = t8 * t14
      t31 = t11 * t17
      t32 = -0.1D1 + x3
      t33 = 0.1D1 / t32
      t37 = log(-0.4D1 * t30 * t31 * t33)
      t38 = t37 * z
      t40 = t37 ** 2
      t41 = t40 * z
      t45 = cos(t12)
      t46 = x3 * z
      t48 = Sqrt(-t46 * t32)
      t52 = 0.1D1 / (-z - x3 + 0.2D1 * t45 * t48)
      t58 = lh * t3
      t59 = t5 * pi
      t61 = z * t26
      t69 = lh ** 2
      t71 = pi ** 2
      t73 = 0.180D3 * t69 - 0.30D2 * t71
      t74 = t73 * t3
      t81 = 0.1D1 / x3
      t83 = 0.1D1 / x1
      t86 = t7 * t14
      t89 = log(0.4D1 * t86 * t31)
      t94 = t89 ** 2
      t97 = t94 * t89
      t100 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t111 = 0.60D2 * lh * t71 - 0.240D3 * zeta3 - 0.120D3 * t69 * lh
      t112 = t111 * t3
      t113 = t59 * t23
      t114 = t112 * t113
      t125 = x2 ** 2
      t126 = x3 * t125
      t127 = t126 * t7
      t130 = log(0.4D1 * t127 * t18)
      t136 = log(-0.4D1 * t127 * t15 * t17 * t33)
      t137 = t136 * z
      t141 = -0.1D1 + x2
      t142 = t17 * t141
      t146 = log(-0.4D1 * t127 * t15 * t142)
      t152 = t58 * t5
      t153 = pi * z
      t155 = t153 * t23 * t52
      t160 = 0.1D1 / x2
      t161 = t160 * t83
      t164 = t125 * t7
      t167 = log(0.4D1 * t164 * t18)
      t169 = t167 ** 2
      t172 = t164 * t14
      t176 = log(-0.4D1 * t172 * t31 * t141)
      t178 = t176 ** 2
      t198 = t59 * t26
      t202 = -0.90D2 * t6 * pi * t28 + 0.180D3 * t58 * t198 - t74 * t113
      t203 = x3 * t11
      t204 = t14 * t17
      t205 = t204 * t33
      t208 = log(-0.4D1 * t203 * t205)
      t213 = log(0.4D1 * t203 * t204)
      t214 = t208 * z * t52 + t213
      t216 = pi * t23
      t217 = t213 ** 2
      t219 = t208 ** 2
      t224 = t217 * t213 / 0.6D1 + t219 * t208 * z * t52 / 0.6D1
      t237 = -z * t52 - 0.1D1
      t244 = -0.90D2 * t6 * pi * t26 + 0.180D3 * t58 * t113
      t248 = -t219 * z * t52 / 0.2D1 - t217 / 0.2D1
      t255 = log(0.4D1 * t126 * t18)
      t257 = t255 ** 2
      t260 = t126 * t11
      t261 = t204 * t141
      t264 = log(-0.4D1 * t260 * t261)
      t268 = log(-0.4D1 * t260 * t205)
      t269 = t268 * z
      t271 = t268 ** 2
      t272 = t271 * z
      t277 = t264 ** 2
      t293 = t74 * t5
      t299 = t11 * t125
      t302 = log(0.4D1 * t299 * t204)
      t303 = t302 ** 2
      t306 = log(-0.4D1 * t299 * t261)
      t307 = t306 ** 2
      t309 = t303 / 0.2D1 - t307 / 0.2D1
      t314 = t307 * t306 / 0.6D1 - t303 * t302 / 0.6D1
      t318 = -t302 + t306
      t323 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t325 = t71 ** 2
      t326 = t69 ** 2
      t332 = t325 + 0.60D2 * t326 + 0.480D3 * lh * zeta3 - 0.60D2 * t69 
     #* t71
      t335 = log(0.4D1 * t18)
      t336 = t335 ** 2
      t337 = t336 * t335
      t360 = t336 ** 2
      t367 = (0.90D2 * t6 * pi * (t22 * t23 / 0.2D1 - t21 * t26 - (-t29 
     #+ t38 * t26 - t41 * t23 / 0.2D1) * t52 + t28) - 0.180D3 * t58 * t5
     #9 * (t26 - t21 * t23 - (-t61 + t38 * t23) * t52) + t74 * t59 * (z 
     #* t23 * t52 + t23)) * t81 * t83 / 0.1440D4 - (t74 * t59 * (-t26 + 
     #t89 * t23) + 0.90D2 * t6 * pi * (-t94 * t26 / 0.2D1 + t97 * t23 / 
     #0.6D1 - t100 + t89 * t28) - t114 - 0.180D3 * t58 * t59 * (t89 * t2
     #6 - t94 * t23 / 0.2D1 - t28)) * t83 / 0.1440D4 + (0.90D2 * t6 * pi
     # * (-t130 * t23 - (-t61 + t137 * t23) * t52 + t146 * t23) - 0.180D
     #3 * t152 * t155) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (-t1
     #67 * t26 + t169 * t23 / 0.2D1 + t176 * t26 - t178 * t23 / 0.2D1) -
     # 0.180D3 * t58 * t59 * (-t167 * t23 + t176 * t23)) * t160 * t83 / 
     #0.720D3 + (t202 * t214 - 0.90D2 * t6 * t216 * t224 + (-t74 * t198 
     #- 0.90D2 * t6 * pi * t100 - t114 + 0.180D3 * t58 * t59 * t28) * t2
     #37 + t244 * t248) * t81 / 0.2880D4 + (0.90D2 * t6 * pi * (-t255 * 
     #t26 + t257 * t23 / 0.2D1 + t264 * t26 - (-t29 + t269 * t26 - t272 
     #* t23 / 0.2D1) * t52 - t277 * t23 / 0.2D1) - 0.180D3 * t58 * t59 *
     # (-t255 * t23 - (-t61 + t269 * t23) * t52 + t264 * t23) + t293 * t
     #155) * t81 * t160 / 0.1440D4 - (t244 * t309 - 0.90D2 * t6 * t216 *
     # t314 + t202 * t318) * t160 / 0.1440D4 - (-0.90D2 * t323 - t23 * t
     #332 + 0.15D2 * t337 * t26 - 0.180D3 * (-t336 * t26 / 0.2D1 + t337 
     #* t23 / 0.6D1 - t100 + t335 * t28) * lh - 0.45D2 * t336 * t28 + (-
     #t28 + t335 * t26 - t336 * t23 / 0.2D1) * t73 + 0.90D2 * t335 * t10
     #0 + (-t26 + t335 * t23) * t111 - 0.15D2 / 0.4D1 * t360 * t23) * t3
     # * t59 / 0.2880D4
      t368 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t367)
      t370 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t373 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t375 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t376 = z * t375
      t386 = z * t373
      t408 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t418 = t59 * t370
      t419 = t112 * t418
      t440 = t153 * t370 * t52
      t470 = t59 * t373
      t474 = -0.90D2 * t6 * pi * t375 + 0.180D3 * t58 * t470 - t74 * t41
     #8
      t476 = pi * t370
      t494 = -0.90D2 * t6 * pi * t373 + 0.180D3 * t58 * t418
      t558 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t567 = (0.90D2 * t6 * pi * (t22 * t370 / 0.2D1 - t21 * t373 - (-t3
     #76 + t38 * t373 - t41 * t370 / 0.2D1) * t52 + t375) - 0.180D3 * t5
     #8 * t59 * (-(-t386 + t38 * t370) * t52 - t21 * t370 + t373) + t74 
     #* t59 * (t370 + z * t370 * t52)) * t81 * t83 / 0.1440D4 - (t74 * t
     #59 * (-t373 + t89 * t370) + 0.90D2 * t6 * pi * (-t408 - t94 * t373
     # / 0.2D1 + t89 * t375 + t97 * t370 / 0.6D1) - t419 - 0.180D3 * t58
     # * t59 * (t89 * t373 - t375 - t94 * t370 / 0.2D1)) * t83 / 0.1440D
     #4 + (0.90D2 * t6 * pi * (-t130 * t370 - (-t386 + t137 * t370) * t5
     #2 + t146 * t370) - 0.180D3 * t152 * t440) * t81 * t161 / 0.720D3 +
     # (0.90D2 * t6 * pi * (-t167 * t373 + t169 * t370 / 0.2D1 + t176 * 
     #t373 - t178 * t370 / 0.2D1) - 0.180D3 * t58 * t59 * (-t167 * t370 
     #+ t176 * t370)) * t160 * t83 / 0.720D3 + (t474 * t214 - 0.90D2 * t
     #6 * t476 * t224 + (-t74 * t470 - 0.90D2 * t6 * pi * t408 - t419 + 
     #0.180D3 * t58 * t59 * t375) * t237 + t494 * t248) * t81 / 0.2880D4
     # + (0.90D2 * t6 * pi * (-t255 * t373 + t257 * t370 / 0.2D1 - t277 
     #* t370 / 0.2D1 - (-t376 + t269 * t373 - t272 * t370 / 0.2D1) * t52
     # + t264 * t373) - 0.180D3 * t58 * t59 * (t264 * t370 - (-t386 + t2
     #69 * t370) * t52 - t255 * t370) + t293 * t440) * t81 * t160 / 0.14
     #40D4 - (t494 * t309 - 0.90D2 * t6 * t476 * t314 + t474 * t318) * t
     #160 / 0.1440D4 - (-0.45D2 * t336 * t375 + (-t375 + t335 * t373 - t
     #336 * t370 / 0.2D1) * t73 + 0.90D2 * t335 * t408 + (-t373 + t335 *
     # t370) * t111 - 0.180D3 * (-t408 - t336 * t373 / 0.2D1 + t335 * t3
     #75 + t337 * t370 / 0.6D1) * lh - 0.15D2 / 0.4D1 * t360 * t370 - 0.
     #90D2 * t558 - t370 * t332 + 0.15D2 * t337 * t373) * t3 * t59 / 0.2
     #880D4
      t568 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t567)
      t570 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t571 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t573 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t576 = z * t570
      t586 = z * t571
      t609 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t618 = t59 * t573
      t619 = t112 * t618
      t640 = t153 * t573 * t52
      t670 = t59 * t571
      t674 = -0.90D2 * t6 * pi * t570 + 0.180D3 * t58 * t670 - t74 * t61
     #8
      t676 = pi * t573
      t694 = -0.90D2 * t6 * pi * t571 + 0.180D3 * t58 * t618
      t736 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t767 = (0.90D2 * t6 * pi * (t570 - t21 * t571 + t22 * t573 / 0.2D1
     # - (-t576 + t38 * t571 - t41 * t573 / 0.2D1) * t52) - 0.180D3 * t5
     #8 * t59 * (-(-t586 + t38 * t573) * t52 - t21 * t573 + t571) + t74 
     #* t59 * (z * t573 * t52 + t573)) * t81 * t83 / 0.1440D4 - (t74 * t
     #59 * (t89 * t573 - t571) + 0.90D2 * t6 * pi * (t89 * t570 - t609 -
     # t94 * t571 / 0.2D1 + t97 * t573 / 0.6D1) - t619 - 0.180D3 * t58 *
     # t59 * (-t94 * t573 / 0.2D1 - t570 + t89 * t571)) * t83 / 0.1440D4
     # + (0.90D2 * t6 * pi * (-t130 * t573 - (-t586 + t137 * t573) * t52
     # + t146 * t573) - 0.180D3 * t152 * t640) * t81 * t161 / 0.720D3 + 
     #(0.90D2 * t6 * pi * (-t167 * t571 + t169 * t573 / 0.2D1 + t176 * t
     #571 - t178 * t573 / 0.2D1) - 0.180D3 * t58 * t59 * (-t167 * t573 +
     # t176 * t573)) * t160 * t83 / 0.720D3 + (t674 * t214 - 0.90D2 * t6
     # * t676 * t224 + (-t74 * t670 - 0.90D2 * t6 * pi * t609 - t619 + 0
     #.180D3 * t58 * t59 * t570) * t237 + t694 * t248) * t81 / 0.2880D4 
     #+ (0.90D2 * t6 * pi * (-t255 * t571 - (-t576 + t269 * t571 - t272 
     #* t573 / 0.2D1) * t52 - t277 * t573 / 0.2D1 + t257 * t573 / 0.2D1 
     #+ t264 * t571) - 0.180D3 * t58 * t59 * (-(-t586 + t269 * t573) * t
     #52 - t255 * t573 + t264 * t573) + t293 * t640) * t81 * t160 / 0.14
     #40D4 - (t694 * t309 - 0.90D2 * t6 * t676 * t314 + t674 * t318) * t
     #160 / 0.1440D4 - (-0.90D2 * t736 + (-t336 * t573 / 0.2D1 - t570 + 
     #t335 * t571) * t73 + 0.15D2 * t337 * t571 + 0.90D2 * t335 * t609 +
     # (-t571 + t335 * t573) * t111 - 0.180D3 * (t335 * t570 - t609 - t3
     #36 * t571 / 0.2D1 + t337 * t573 / 0.6D1) * lh - 0.15D2 / 0.4D1 * t
     #360 * t573 - 0.45D2 * t336 * t570 - t573 * t332) * t3 * t59 / 0.28
     #80D4
      t768 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t767)
      t770 = x2 * x3
      t771 = 0.1D1 - x3 + t770
      t772 = 0.1D1 / t771
      t773 = t770 * t772
      t774 = t2 * t773
      t776 = t2 * t32 * t772
      t777 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t776, t774, 0.0D0)
      t780 = t771 ** 2
      t781 = 0.1D1 / t780
      t786 = log(0.4D1 * t126 * t15 * t142 * t32 * t781)
      t787 = t786 * z
      t788 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t776, t774, 0.0D0)
      t790 = t786 ** 2
      t791 = t790 * z
      t792 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t776, t774, 0.0D0)
      t797 = t141 * t32
      t799 = Sqrt(t46 * t797)
      t803 = 0.1D1 / (-z - x3 + t770 + 0.2D1 * t45 * t799)
      t807 = z * t788
      t815 = t153 * t792 * t803
      t821 = t126 * t86
      t822 = t797 * t781
      t826 = log(0.4D1 * t821 * t31 * t822)
      t827 = t826 * z
      t840 = (0.90D2 * t6 * pi * (-z * t777 + t787 * t788 - t791 * t792 
     #/ 0.2D1) * t803 - 0.180D3 * t152 * pi * (-t807 + t787 * t792) * t8
     #03 - t293 * t815) * t81 * t160 / 0.1440D4 + (0.90D2 * t6 * pi * (-
     #t807 + t827 * t792) * t803 + 0.180D3 * t152 * t815) * t81 * t161 /
     # 0.720D3
      t841 = FJET(XB1, XB2, s, 0.0D0, t774, 0.0D0, -t776, 0.0D0, t840)
      t844 = t1 * x1
      t845 = x1 * z
      t846 = -z - x1 + t845
      t847 = 0.1D1 / t846
      t849 = t141 * s * t844 * t847
      t850 = -0.1D1 + x1
      t851 = t2 * t850
      t853 = x2 * s * t844
      t854 = s * t16
      t857 = x1 * t850 * t847
      t858 = t854 * t141 * t857
      t859 = 0.1D1 / t9
      t860 = t859 * t17
      t861 = t850 ** 2
      t862 = t847 * t861
      t867 = log(0.4D1 * t821 * t860 * t862 * t141)
      t868 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t8
     #51, 0.0D0, -t858)
      t870 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t8
     #51, 0.0D0, -t858)
      t875 = t59 * t868
      t881 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t8
     #51, 0.0D0, -t858)
      t882 = t14 * t859
      t884 = t17 * t847
      t889 = log(0.4D1 * t164 * t882 * t884 * t861 * t141)
      t891 = t889 ** 2
      t908 = (0.90D2 * t6 * pi * (-t867 * t868 + t870) - 0.180D3 * t58 *
     # t875) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (t881 - t889 *
     # t870 + t891 * t868 / 0.2D1) - 0.180D3 * t58 * t59 * (t870 - t889 
     #* t868) + t74 * t875) * t160 * t83 / 0.720D3
      t909 = FJET(XB1, XB2, s, 0.0D0, t849, -t851, t853, -t858, t908)
      t912 = t2 * x1 * t847
      t913 = t854 * t857
      t914 = t860 * t862
      t917 = log(-0.4D1 * t30 * t914)
      t918 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, -
     #t851, 0.0D0, t913)
      t920 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, -
     #t851, 0.0D0, t913)
      t921 = z * t846
      t928 = log(0.4D1 * t8 * t882 * t884 * t861 * t33)
      t929 = t928 * z
      t932 = t928 ** 2
      t933 = t932 * z
      t934 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, -
     #t851, 0.0D0, t913)
      t935 = t846 * t934
      t939 = x3 * x1
      t940 = t939 * z
      t942 = 0.2D1 * t8 * z
      t943 = t8 * t9
      t944 = x1 * t9
      t945 = x3 * t9
      t946 = t945 * x1
      t947 = x3 * t846
      t949 = Sqrt(t947 * t32)
      t954 = 0.1D1 / (-t845 - t940 - t46 - t8 + t942 - t943 + t944 + t94
     #6 - t9 + 0.2D1 * t45 * t949 * z)
      t956 = t917 ** 2
      t964 = t921 * t918
      t975 = t59 * (-t934 + t921 * t934 * t954)
      t981 = t86 * t859
      t982 = t884 * t861
      t985 = log(-0.4D1 * t981 * t982)
      t990 = t985 ** 2
      t993 = t990 * t985
      t996 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, -
     #t851, 0.0D0, t913)
      t1002 = t59 * t934
      t1016 = log(-0.4D1 * t821 * t914)
      t1022 = log(0.4D1 * t821 * t860 * t862 * t33)
      t1023 = t1022 * z
      t1039 = log(-0.4D1 * t172 * t914)
      t1041 = t1039 ** 2
      t1058 = (0.90D2 * t6 * pi * (t917 * t918 - t920 - (-t921 * t920 + 
     #t929 * t846 * t918 - t933 * t935 / 0.2D1) * t954 - t956 * t934 / 0
     #.2D1) - 0.180D3 * t58 * t59 * (-t918 + t917 * t934 - (-t964 + t929
     # * t935) * t954) + t74 * t975) * t81 * t83 / 0.1440D4 - (t74 * t59
     # * (-t985 * t934 + t918) + 0.90D2 * t6 * pi * (t990 * t918 / 0.2D1
     # - t993 * t934 / 0.6D1 + t996 - t985 * t920) + t112 * t1002 - 0.18
     #0D3 * t58 * t59 * (t990 * t934 / 0.2D1 - t985 * t918 + t920)) * t8
     #3 / 0.1440D4 + (0.90D2 * t6 * pi * (t1016 * t934 - (-t964 + t1023 
     #* t935) * t954 - t918) - 0.180D3 * t58 * t975) * t81 * t161 / 0.72
     #0D3 + (0.90D2 * t6 * pi * (t1039 * t918 - t1041 * t934 / 0.2D1 - t
     #920) - 0.180D3 * t58 * t59 * (-t918 + t1039 * t934) - t74 * t1002)
     # * t160 * t83 / 0.720D3
      t1059 = FJET(XB1, XB2, s, 0.0D0, -t851, -t912, 0.0D0, t913, t1058)
      t1061 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1064 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1066 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1070 = t846 * t1061
      t1079 = t921 * t1064
      t1091 = t59 * (-t1061 + t921 * t1061 * t954)
      t1106 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1111 = t59 * t1061
      t1154 = (0.90D2 * t6 * pi * (-t956 * t1061 / 0.2D1 + t917 * t1064 
     #- (-t921 * t1066 + t929 * t846 * t1064 - t933 * t1070 / 0.2D1) * t
     #954 - t1066) - 0.180D3 * t58 * t59 * (-t1064 - (-t1079 + t929 * t1
     #070) * t954 + t917 * t1061) + t74 * t1091) * t81 * t83 / 0.1440D4 
     #- (t74 * t59 * (t1064 - t985 * t1061) + 0.90D2 * t6 * pi * (t990 *
     # t1064 / 0.2D1 - t993 * t1061 / 0.6D1 - t985 * t1066 + t1106) + t1
     #12 * t1111 - 0.180D3 * t58 * t59 * (t1066 - t985 * t1064 + t990 * 
     #t1061 / 0.2D1)) * t83 / 0.1440D4 + (0.90D2 * t6 * pi * (t1016 * t1
     #061 - t1064 - (-t1079 + t1023 * t1070) * t954) - 0.180D3 * t58 * t
     #1091) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (-t1041 * t1061
     # / 0.2D1 + t1039 * t1064 - t1066) - 0.180D3 * t58 * t59 * (-t1064 
     #+ t1039 * t1061) - t74 * t1111) * t160 * t83 / 0.720D3
      t1155 = FJET(XB1, XB2, s, 0.0D0, -t912, -t851, 0.0D0, t913, t1154)
      t1157 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1158 = z * t1157
      t1159 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1167 = t153 * t1159 * t803
      t1174 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1195 = (0.90D2 * t6 * pi * (-t1158 + t827 * t1159) * t803 + 0.180
     #D3 * t152 * t1167) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (-
     #z * t1174 + t787 * t1157 - t791 * t1159 / 0.2D1) * t803 - 0.180D3 
     #* t152 * pi * (-t1158 + t787 * t1159) * t803 - t293 * t1167) * t81
     # * t160 / 0.1440D4
      t1196 = FJET(XB1, XB2, s, 0.0D0, -t776, 0.0D0, t774, 0.0D0, t1195)
      t1198 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1199 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1201 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1204 = z * t1198
      t1214 = z * t1199
      t1236 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1246 = t59 * t1201
      t1247 = t112 * t1246
      t1268 = t153 * t1201 * t52
      t1305 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1329 = t59 * t1199
      t1333 = -0.90D2 * t6 * pi * t1198 + 0.180D3 * t58 * t1329 - t74 * 
     #t1246
      t1335 = pi * t1201
      t1353 = -0.90D2 * t6 * pi * t1199 + 0.180D3 * t58 * t1246
      t1395 = (0.90D2 * t6 * pi * (t1198 - t21 * t1199 + t22 * t1201 / 0
     #.2D1 - (-t1204 + t38 * t1199 - t41 * t1201 / 0.2D1) * t52) - 0.180
     #D3 * t58 * t59 * (-(-t1214 + t38 * t1201) * t52 - t21 * t1201 + t1
     #199) + t74 * t59 * (t1201 + z * t1201 * t52)) * t81 * t83 / 0.1440
     #D4 - (t74 * t59 * (-t1199 + t89 * t1201) + 0.90D2 * t6 * pi * (-t1
     #236 + t89 * t1198 + t97 * t1201 / 0.6D1 - t94 * t1199 / 0.2D1) - t
     #1247 - 0.180D3 * t58 * t59 * (t89 * t1199 - t1198 - t94 * t1201 / 
     #0.2D1)) * t83 / 0.1440D4 + (0.90D2 * t6 * pi * (t146 * t1201 - (-t
     #1214 + t137 * t1201) * t52 - t130 * t1201) - 0.180D3 * t152 * t126
     #8) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (t176 * t1199 - t1
     #78 * t1201 / 0.2D1 + t169 * t1201 / 0.2D1 - t167 * t1199) - 0.180D
     #3 * t58 * t59 * (-t167 * t1201 + t176 * t1201)) * t160 * t83 / 0.7
     #20D3 - (-0.180D3 * (-t336 * t1199 / 0.2D1 - t1236 + t335 * t1198 +
     # t337 * t1201 / 0.6D1) * lh - 0.15D2 / 0.4D1 * t360 * t1201 - 0.90
     #D2 * t1305 - 0.45D2 * t336 * t1198 + 0.15D2 * t337 * t1199 + (t335
     # * t1199 - t1198 - t336 * t1201 / 0.2D1) * t73 + (-t1199 + t335 * 
     #t1201) * t111 + 0.90D2 * t335 * t1236 - t1201 * t332) * t3 * t59 /
     # 0.2880D4 + (t1333 * t214 - 0.90D2 * t6 * t1335 * t224 + (-t74 * t
     #1329 - 0.90D2 * t6 * pi * t1236 - t1247 + 0.180D3 * t58 * t59 * t1
     #198) * t237 + t1353 * t248) * t81 / 0.2880D4 + (0.90D2 * t6 * pi *
     # (-(-t1204 + t269 * t1199 - t272 * t1201 / 0.2D1) * t52 - t255 * t
     #1199 + t257 * t1201 / 0.2D1 - t277 * t1201 / 0.2D1 + t264 * t1199)
     # - 0.180D3 * t58 * t59 * (-(-t1214 + t269 * t1201) * t52 - t255 * 
     #t1201 + t264 * t1201) + t293 * t1268) * t81 * t160 / 0.1440D4 - (t
     #1353 * t309 - 0.90D2 * t6 * t1335 * t314 + t1333 * t318) * t160 / 
     #0.1440D4
      t1396 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1395)
      t1398 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1399 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1405 = t59 * t1399
      t1413 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1429 = (0.90D2 * t6 * pi * (t1398 - t867 * t1399) - 0.180D3 * t58
     # * t1405) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (t891 * t13
     #99 / 0.2D1 + t1413 - t889 * t1398) - 0.180D3 * t58 * t59 * (t1398 
     #- t889 * t1399) + t74 * t1405) * t160 * t83 / 0.720D3
      t1430 = FJET(XB1, XB2, s, t853, -t851, t849, 0.0D0, -t858, t1429)
      t1432 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1433 = z * t1432
      t1434 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1442 = t153 * t1434 * t803
      t1449 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1470 = (0.90D2 * t6 * pi * (-t1433 + t827 * t1434) * t803 + 0.180
     #D3 * t152 * t1442) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (-
     #z * t1449 + t787 * t1432 - t791 * t1434 / 0.2D1) * t803 - 0.180D3 
     #* t152 * pi * (-t1433 + t787 * t1434) * t803 - t293 * t1442) * t81
     # * t160 / 0.1440D4
      t1471 = FJET(XB1, XB2, s, t774, 0.0D0, -t776, 0.0D0, 0.0D0, t1470)
      t1473 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1475 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1480 = t59 * t1473
      t1488 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1504 = (0.90D2 * t6 * pi * (-t867 * t1473 + t1475) - 0.180D3 * t5
     #8 * t1480) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (t891 * t1
     #473 / 0.2D1 + t1488 - t889 * t1475) - 0.180D3 * t58 * t59 * (-t889
     # * t1473 + t1475) + t74 * t1480) * t160 * t83 / 0.720D3
      t1505 = FJET(XB1, XB2, s, t849, 0.0D0, t853, -t851, -t858, t1504)
      t1510 = t32 * s * t1 * t850 * t772
      t1511 = t2 * x1
      t1513 = Sqrt(-t947 * t797)
      t1514 = t45 * t1513
      t1520 = t1511 * x2 * (-x3 + t770 - z + t46 - x1 + t939 + t845 - t9
     #40 + 0.2D1 * t1514) * t847 * t772
      t1521 = t851 * t773
      t1522 = t126 * x1
      t1526 = t126 * t845
      t1530 = t1511 * (t1522 + t126 * z - x2 + t770 + 0.2D1 * t1514 * x2
     # + 0.1D1 - x3 - t1526) * t847 * t772
      t1535 = log(-0.4D1 * t126 * t981 * t982 * t822)
      t1536 = t1535 * t846
      t1537 = x2 * x1
      t1538 = t1537 * z
      t1541 = t7 * x2
      t1549 = t46 + t8 - t944 + t9 + 0.2D1 * t1514 * t1538 + t845 - t152
     #2 - 0.2D1 * t1541 * z + t1538 - t770 * z + t770 * x1 - t8 * x2 + t
     #7 * t9 * x2
      t1550 = t9 * x2
      t1563 = -t1550 * x1 - 0.2D1 * t1514 * z + t940 - t942 + t943 - t94
     #6 + t1541 + t1526 - 0.2D1 * t1514 * t1537 - 0.2D1 * t770 * t845 + 
     #t945 * t1537 + 0.2D1 * t8 * x2 * z - t8 * t1550
      t1565 = 0.1D1 / (t1549 + t1563)
      t1566 = -z + t1538 - t1537
      t1567 = t1565 * t1566
      t1568 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1571 = t846 * t1565
      t1572 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1579 = t58 * t59
      t1584 = 0.90D2 * t6 * pi * (t1536 * t1567 * t1568 - t1571 * t1566 
     #* t1572) + 0.180D3 * t1579 * t1571 * t1566 * t1568
      t1588 = FJET(XB1, XB2, s, t1510, t1520, -t1521, -t1530, -t858, t15
     #84 * t81 * t161 / 0.720D3)
      t1591 = t81 * t160 * t83
      t1594 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1597 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1608 = 0.90D2 * t6 * pi * (-t1571 * t1566 * t1594 + t1536 * t1567
     # * t1597) + 0.180D3 * t1579 * t1571 * t1566 * t1597
      t1612 = FJET(XB1, XB2, s, t1520, t1510, -t1530, -t1521, -t858, t16
     #08 * t81 * t161 / 0.720D3)
      t1616 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1619 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1621 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1624 = t846 * t1616
      t1634 = t921 * t1621
      t1646 = t59 * (t921 * t1616 * t954 - t1616)
      t1656 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1666 = t59 * t1616
      t1709 = (0.90D2 * t6 * pi * (-t956 * t1616 / 0.2D1 - (-t921 * t161
     #9 + t929 * t846 * t1621 - t933 * t1624 / 0.2D1) * t954 - t1619 + t
     #917 * t1621) - 0.180D3 * t58 * t59 * (-(-t1634 + t929 * t1624) * t
     #954 + t917 * t1616 - t1621) + t74 * t1646) * t81 * t83 / 0.1440D4 
     #- (t74 * t59 * (-t985 * t1616 + t1621) + 0.90D2 * t6 * pi * (t1656
     # + t990 * t1621 / 0.2D1 - t993 * t1616 / 0.6D1 - t985 * t1619) + t
     #112 * t1666 - 0.180D3 * t58 * t59 * (t990 * t1616 / 0.2D1 + t1619 
     #- t985 * t1621)) * t83 / 0.1440D4 + (0.90D2 * t6 * pi * (t1016 * t
     #1616 - t1621 - (-t1634 + t1023 * t1624) * t954) - 0.180D3 * t58 * 
     #t1646) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (-t1619 - t104
     #1 * t1616 / 0.2D1 + t1039 * t1621) - 0.180D3 * t58 * t59 * (-t1621
     # + t1039 * t1616) - t74 * t1666) * t160 * t83 / 0.720D3
      t1710 = FJET(XB1, XB2, s, -t851, 0.0D0, 0.0D0, -t912, t913, t1709)
      t1712 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1713 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1719 = t59 * t1713
      t1727 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t853, t849, -t
     #851, 0.0D0, -t858)
      t1743 = (0.90D2 * t6 * pi * (t1712 - t867 * t1713) - 0.180D3 * t58
     # * t1719) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (t891 * t17
     #13 / 0.2D1 + t1727 - t889 * t1712) - 0.180D3 * t58 * t59 * (-t889 
     #* t1713 + t1712) + t74 * t1719) * t160 * t83 / 0.720D3
      t1744 = FJET(XB1, XB2, s, -t851, t853, 0.0D0, t849, -t858, t1743)
      t1746 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1749 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1751 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1755 = t846 * t1746
      t1764 = t921 * t1749
      t1776 = t59 * (t921 * t1746 * t954 - t1746)
      t1791 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t912, 
     #-t851, 0.0D0, t913)
      t1796 = t59 * t1746
      t1839 = (0.90D2 * t6 * pi * (-t956 * t1746 / 0.2D1 + t917 * t1749 
     #- t1751 - (-t921 * t1751 + t929 * t846 * t1749 - t933 * t1755 / 0.
     #2D1) * t954) - 0.180D3 * t58 * t59 * (-(-t1764 + t929 * t1755) * t
     #954 - t1749 + t917 * t1746) + t74 * t1776) * t81 * t83 / 0.1440D4 
     #- (t74 * t59 * (-t985 * t1746 + t1749) + 0.90D2 * t6 * pi * (t990 
     #* t1749 / 0.2D1 - t993 * t1746 / 0.6D1 - t985 * t1751 + t1791) + t
     #112 * t1796 - 0.180D3 * t58 * t59 * (t990 * t1746 / 0.2D1 + t1751 
     #- t985 * t1749)) * t83 / 0.1440D4 + (0.90D2 * t6 * pi * (t1016 * t
     #1746 - (-t1764 + t1023 * t1755) * t954 - t1749) - 0.180D3 * t58 * 
     #t1776) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (-t1041 * t174
     #6 / 0.2D1 - t1751 + t1039 * t1749) - 0.180D3 * t58 * t59 * (t1039 
     #* t1746 - t1749) - t74 * t1796) * t160 * t83 / 0.720D3
      t1840 = FJET(XB1, XB2, s, -t912, 0.0D0, 0.0D0, -t851, t913, t1839)
      t1842 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1843 = z * t1842
      t1844 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1852 = t153 * t1844 * t803
      t1859 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t776, t774, 0.0D0)
      t1880 = (0.90D2 * t6 * pi * (-t1843 + t827 * t1844) * t803 + 0.180
     #D3 * t152 * t1852) * t81 * t161 / 0.720D3 + (0.90D2 * t6 * pi * (-
     #z * t1859 + t787 * t1842 - t791 * t1844 / 0.2D1) * t803 - 0.180D3 
     #* t152 * pi * (-t1843 + t787 * t1844) * t803 - t293 * t1852) * t81
     # * t160 / 0.1440D4
      t1881 = FJET(XB1, XB2, s, -t776, 0.0D0, t774, 0.0D0, 0.0D0, t1880)
      t1883 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1886 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1897 = 0.90D2 * t6 * pi * (t1536 * t1567 * t1883 - t1571 * t1566 
     #* t1886) + 0.180D3 * t1579 * t1571 * t1566 * t1883
      t1901 = FJET(XB1, XB2, s, -t1530, -t1521, t1520, t1510, -t858, t18
     #97 * t81 * t161 / 0.720D3)
      t1905 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1908 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t1520, -t1530,
     # t1510, -t1521, -t858)
      t1919 = 0.90D2 * t6 * pi * (t1536 * t1567 * t1905 - t1571 * t1566 
     #* t1908) + 0.180D3 * t1579 * t1571 * t1566 * t1905
      t1923 = FJET(XB1, XB2, s, -t1521, -t1530, t1510, t1520, -t858, t19
     #19 * t81 * t161 / 0.720D3)
      rrqg2qght8s3e1 = t368 * t367 + t568 * t567 + t768 * t767 + t841 * 
     #t840 + t909 * t908 + t1059 * t1058 + t1155 * t1154 + t1196 * t1195
     # + t1396 * t1395 + t1430 * t1429 + t1471 * t1470 + t1505 * t1504 +
     # t1588 * t1584 * t1591 / 0.720D3 + t1612 * t1608 * t1591 / 0.720D3
     # + t1710 * t1709 + t1744 * t1743 + t1840 * t1839 + t1881 * t1880 +
     # t1901 * t1897 * t1591 / 0.720D3 + t1923 * t1919 * t1591 / 0.720D3

      end function



      doubleprecision function rrqg2qght8s3e0
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t22 = log(0.4D1 * t9 * t19)
      t23 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t25 = z * t7
      t26 = t9 * t15
      t27 = t12 * t18
      t28 = -0.1D1 + x3
      t29 = 0.1D1 / t28
      t33 = log(-0.4D1 * t26 * t27 * t29)
      t34 = t33 * z
      t37 = cos(t13)
      t38 = x3 * z
      t40 = Sqrt(-t38 * t28)
      t44 = 0.1D1 / (-z - x3 + 0.2D1 * t37 * t40)
      t50 = lh * t3
      t51 = t5 * pi
      t59 = 0.1D1 / x3
      t61 = 0.1D1 / x1
      t64 = pi * z
      t65 = t6 * t64
      t66 = t23 * t44
      t67 = 0.1D1 / x2
      t69 = t59 * t67 * t61
      t73 = t6 * pi
      t74 = x2 ** 2
      t75 = t74 * t8
      t78 = log(0.4D1 * t75 * t19)
      t80 = t75 * t15
      t81 = -0.1D1 + x2
      t85 = log(-0.4D1 * t80 * t27 * t81)
      t92 = t8 * t15
      t95 = log(0.4D1 * t92 * t27)
      t97 = t95 ** 2
      t100 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t110 = lh ** 2
      t112 = pi ** 2
      t114 = 0.180D3 * t110 - 0.30D2 * t112
      t115 = t114 * t3
      t116 = t51 * t23
      t117 = t115 * t116
      t121 = pi * t23
      t122 = x3 * t12
      t123 = t15 * t18
      t124 = t123 * t29
      t127 = log(-0.4D1 * t122 * t124)
      t128 = t127 ** 2
      t133 = log(0.4D1 * t122 * t123)
      t134 = t133 ** 2
      t136 = -t128 * z * t44 / 0.2D1 - t134 / 0.2D1
      t145 = -0.90D2 * t6 * pi * t7 + 0.180D3 * t50 * t116
      t148 = t127 * z * t44 + t133
      t158 = -z * t44 - 0.1D1
      t168 = 0.60D2 * lh * t112 - 0.240D3 * zeta3 - 0.120D3 * t110 * lh
      t171 = log(0.4D1 * t19)
      t172 = t171 ** 2
      t175 = t172 * t171
      t178 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t195 = x3 * t74
      t198 = log(0.4D1 * t195 * t19)
      t200 = t195 * t12
      t203 = log(-0.4D1 * t200 * t124)
      t204 = t203 * z
      t208 = t123 * t81
      t211 = log(-0.4D1 * t200 * t208)
      t217 = t50 * t5
      t225 = t12 * t74
      t228 = log(0.4D1 * t225 * t123)
      t229 = t228 ** 2
      t232 = log(-0.4D1 * t225 * t208)
      t233 = t232 ** 2
      t235 = t229 / 0.2D1 - t233 / 0.2D1
      t239 = -t228 + t232
      t244 = (0.90D2 * t6 * pi * (t7 - t22 * t23 - (-t25 + t34 * t23) * 
     #t44) - 0.180D3 * t50 * t51 * (z * t23 * t44 + t23)) * t59 * t61 / 
     #0.1440D4 + t65 * t66 * t69 / 0.8D1 + t73 * (-t78 * t23 + t85 * t23
     #) * t67 * t61 / 0.8D1 - (0.90D2 * t6 * pi * (t95 * t7 - t97 * t23 
     #/ 0.2D1 - t100) - 0.180D3 * t50 * t51 * (-t7 + t95 * t23) - t117) 
     #* t61 / 0.1440D4 + (-0.90D2 * t6 * t121 * t136 + t145 * t148 + (-0
     #.90D2 * t6 * pi * t100 + 0.180D3 * t50 * t51 * t7 - t117) * t158) 
     #* t59 / 0.2880D4 - (-t23 * t168 - 0.45D2 * t172 * t7 + 0.15D2 * t1
     #75 * t23 - 0.90D2 * t178 + 0.90D2 * t171 * t100 - 0.180D3 * (-t100
     # + t171 * t7 - t172 * t23 / 0.2D1) * lh + (-t7 + t171 * t23) * t11
     #4) * t3 * t51 / 0.2880D4 + (0.90D2 * t6 * pi * (-t198 * t23 - (-t2
     #5 + t204 * t23) * t44 + t211 * t23) - 0.180D3 * t217 * t64 * t66) 
     #* t59 * t67 / 0.1440D4 - (-0.90D2 * t6 * t121 * t235 + t145 * t239
     #) * t67 / 0.1440D4
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t244)
      t247 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t248 = z * t247
      t249 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t268 = t249 * t44
      t280 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t292 = t51 * t249
      t293 = t115 * t292
      t297 = pi * t249
      t306 = -0.90D2 * t6 * pi * t247 + 0.180D3 * t50 * t292
      t333 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t364 = (0.90D2 * t6 * pi * (-(-t248 + t34 * t249) * t44 - t22 * t2
     #49 + t247) - 0.180D3 * t50 * t51 * (t249 + z * t249 * t44)) * t59 
     #* t61 / 0.1440D4 + t65 * t268 * t69 / 0.8D1 + t73 * (-t78 * t249 +
     # t85 * t249) * t67 * t61 / 0.8D1 - (0.90D2 * t6 * pi * (t95 * t247
     # - t280 - t97 * t249 / 0.2D1) - 0.180D3 * t50 * t51 * (-t247 + t95
     # * t249) - t293) * t61 / 0.1440D4 + (-0.90D2 * t6 * t297 * t136 + 
     #t306 * t148 + (-0.90D2 * t6 * pi * t280 + 0.180D3 * t50 * t51 * t2
     #47 - t293) * t158) * t59 / 0.2880D4 - (-0.180D3 * (-t280 + t171 * 
     #t247 - t172 * t249 / 0.2D1) * lh - 0.45D2 * t172 * t247 - t249 * t
     #168 + (-t247 + t171 * t249) * t114 + 0.90D2 * t171 * t280 - 0.90D2
     # * t333 + 0.15D2 * t175 * t249) * t3 * t51 / 0.2880D4 + (0.90D2 * 
     #t6 * pi * (t211 * t249 - (-t248 + t204 * t249) * t44 - t198 * t249
     #) - 0.180D3 * t217 * t64 * t268) * t59 * t67 / 0.1440D4 - (-0.90D2
     # * t6 * t297 * t235 + t306 * t239) * t67 / 0.1440D4
      t365 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t364)
      t367 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t368 = z * t367
      t369 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t388 = t369 * t44
      t401 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t412 = t51 * t369
      t413 = t115 * t412
      t417 = pi * t369
      t426 = -0.90D2 * t6 * pi * t367 + 0.180D3 * t50 * t412
      t455 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t484 = (0.90D2 * t6 * pi * (-(-t368 + t34 * t369) * t44 - t22 * t3
     #69 + t367) - 0.180D3 * t50 * t51 * (z * t369 * t44 + t369)) * t59 
     #* t61 / 0.1440D4 + t65 * t388 * t69 / 0.8D1 + t73 * (-t78 * t369 +
     # t85 * t369) * t67 * t61 / 0.8D1 - (0.90D2 * t6 * pi * (-t97 * t36
     #9 / 0.2D1 - t401 + t95 * t367) - 0.180D3 * t50 * t51 * (t95 * t369
     # - t367) - t413) * t61 / 0.1440D4 + (-0.90D2 * t6 * t417 * t136 + 
     #t426 * t148 + (-0.90D2 * t6 * pi * t401 + 0.180D3 * t50 * t51 * t3
     #67 - t413) * t158) * t59 / 0.2880D4 - (-0.180D3 * (-t172 * t369 / 
     #0.2D1 - t401 + t171 * t367) * lh + (-t367 + t171 * t369) * t114 + 
     #0.15D2 * t175 * t369 - t369 * t168 - 0.45D2 * t172 * t367 + 0.90D2
     # * t171 * t401 - 0.90D2 * t455) * t3 * t51 / 0.2880D4 + (0.90D2 * 
     #t6 * pi * (-(-t368 + t204 * t369) * t44 - t198 * t369 + t211 * t36
     #9) - 0.180D3 * t217 * t64 * t388) * t59 * t67 / 0.1440D4 - (-0.90D
     #2 * t6 * t417 * t235 + t426 * t239) * t67 / 0.1440D4
      t485 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t484)
      t487 = x2 * x3
      t488 = 0.1D1 - x3 + t487
      t489 = 0.1D1 / t488
      t490 = t487 * t489
      t491 = t2 * t490
      t493 = t2 * t28 * t489
      t494 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t493, t491, 0.0D0)
      t495 = t81 * t28
      t497 = Sqrt(t38 * t495)
      t501 = 0.1D1 / (-z - x3 + t487 + 0.2D1 * t37 * t497)
      t502 = t494 * t501
      t506 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t493, t491, 0.0D0)
      t510 = t488 ** 2
      t516 = log(0.4D1 * t195 * t16 * t18 * t81 * t28 / t510)
      t517 = t516 * z
      t531 = -t65 * t502 * t69 / 0.8D1 + (0.90D2 * t6 * pi * (-z * t506 
     #+ t517 * t494) * t501 + 0.180D3 * t217 * t64 * t502) * t59 * t67 /
     # 0.1440D4
      t532 = FJET(XB1, XB2, s, 0.0D0, t491, 0.0D0, -t493, 0.0D0, t531)
      t535 = t1 * x1
      t536 = x1 * z
      t537 = -z - x1 + t536
      t538 = 0.1D1 / t537
      t540 = t81 * s * t535 * t538
      t541 = -0.1D1 + x1
      t542 = t2 * t541
      t544 = x2 * s * t535
      t545 = s * t17
      t548 = x1 * t541 * t538
      t549 = t545 * t81 * t548
      t550 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t5
     #42, 0.0D0, -t549)
      t552 = t67 * t61
      t556 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t5
     #42, 0.0D0, -t549)
      t557 = 0.1D1 / t10
      t558 = t15 * t557
      t560 = t18 * t538
      t561 = t541 ** 2
      t566 = log(0.4D1 * t75 * t558 * t560 * t561 * t81)
      t579 = t73 * t550 * t59 * t552 / 0.8D1 + (0.90D2 * t6 * pi * (t556
     # - t566 * t550) - 0.180D3 * t50 * t51 * t550) * t67 * t61 / 0.720D
     #3
      t580 = FJET(XB1, XB2, s, 0.0D0, t540, -t542, t544, -t549, t579)
      t583 = t2 * x1 * t538
      t584 = t545 * t548
      t585 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, -
     #t542, 0.0D0, t584)
      t588 = t557 * t18 * t538 * t561
      t591 = log(-0.4D1 * t26 * t588)
      t592 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, -
     #t542, 0.0D0, t584)
      t594 = z * t537
      t601 = log(0.4D1 * t9 * t558 * t560 * t561 * t29)
      t602 = t601 * z
      t606 = x3 * x1
      t607 = t606 * z
      t609 = 0.2D1 * t9 * z
      t610 = t9 * t10
      t611 = x1 * t10
      t612 = x3 * t10
      t613 = t612 * x1
      t614 = x3 * t537
      t616 = Sqrt(t614 * t28)
      t621 = 0.1D1 / (-t536 - t607 - t38 - t9 + t609 - t610 + t611 + t61
     #3 - t10 + 0.2D1 * t37 * t616 * z)
      t629 = -t592 + t594 * t592 * t621
      t643 = log(-0.4D1 * t80 * t588)
      t649 = t51 * t592
      t660 = log(-0.4D1 * t92 * t557 * t560 * t561)
      t661 = t660 ** 2
      t665 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, -
     #t542, 0.0D0, t584)
      t679 = (0.90D2 * t6 * pi * (-t585 + t591 * t592 - (-t594 * t585 + 
     #t602 * t537 * t592) * t621) - 0.180D3 * t50 * t51 * t629) * t59 * 
     #t61 / 0.1440D4 + t73 * t629 * t59 * t552 / 0.8D1 + (0.90D2 * t6 * 
     #pi * (-t585 + t643 * t592) + 0.180D3 * t50 * t649) * t67 * t61 / 0
     #.720D3 - (0.90D2 * t6 * pi * (t661 * t592 / 0.2D1 - t660 * t585 + 
     #t665) - 0.180D3 * t50 * t51 * (-t660 * t592 + t585) + t115 * t649)
     # * t61 / 0.1440D4
      t680 = FJET(XB1, XB2, s, 0.0D0, -t542, -t583, 0.0D0, t584, t679)
      t682 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, -
     #t542, 0.0D0, t584)
      t684 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, -
     #t542, 0.0D0, t584)
      t696 = -t684 + t594 * t684 * t621
      t713 = t51 * t684
      t720 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, -
     #t542, 0.0D0, t584)
      t737 = (0.90D2 * t6 * pi * (-t682 - (-t594 * t682 + t602 * t537 * 
     #t684) * t621 + t591 * t684) - 0.180D3 * t50 * t51 * t696) * t59 * 
     #t61 / 0.1440D4 + t73 * t696 * t59 * t552 / 0.8D1 + (0.90D2 * t6 * 
     #pi * (-t682 + t643 * t684) + 0.180D3 * t50 * t713) * t67 * t61 / 0
     #.720D3 - (0.90D2 * t6 * pi * (t720 - t660 * t682 + t661 * t684 / 0
     #.2D1) - 0.180D3 * t50 * t51 * (t682 - t660 * t684) + t115 * t713) 
     #* t61 / 0.1440D4
      t738 = FJET(XB1, XB2, s, 0.0D0, -t583, -t542, 0.0D0, t584, t737)
      t740 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t493, t491, 0.0D0)
      t741 = t740 * t501
      t745 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t493, t491, 0.0D0)
      t760 = -t65 * t741 * t69 / 0.8D1 + (0.90D2 * t6 * pi * (-z * t745 
     #+ t517 * t740) * t501 + 0.180D3 * t217 * t64 * t741) * t59 * t67 /
     # 0.1440D4
      t761 = FJET(XB1, XB2, s, 0.0D0, -t493, 0.0D0, t491, 0.0D0, t760)
      t763 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t764 = z * t763
      t765 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t784 = t765 * t44
      t796 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t808 = t51 * t765
      t809 = t115 * t808
      t813 = pi * t765
      t822 = -0.90D2 * t6 * pi * t763 + 0.180D3 * t50 * t808
      t851 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t880 = (0.90D2 * t6 * pi * (-(-t764 + t34 * t765) * t44 - t22 * t7
     #65 + t763) - 0.180D3 * t50 * t51 * (t765 + z * t765 * t44)) * t59 
     #* t61 / 0.1440D4 + t65 * t784 * t69 / 0.8D1 + t73 * (-t78 * t765 +
     # t85 * t765) * t67 * t61 / 0.8D1 - (0.90D2 * t6 * pi * (t95 * t763
     # - t796 - t97 * t765 / 0.2D1) - 0.180D3 * t50 * t51 * (-t763 + t95
     # * t765) - t809) * t61 / 0.1440D4 + (-0.90D2 * t6 * t813 * t136 + 
     #t822 * t148 + (-0.90D2 * t6 * pi * t796 + 0.180D3 * t50 * t51 * t7
     #63 - t809) * t158) * t59 / 0.2880D4 - (-0.180D3 * (t171 * t763 - t
     #796 - t172 * t765 / 0.2D1) * lh + 0.15D2 * t175 * t765 + 0.90D2 * 
     #t171 * t796 + (-t763 + t171 * t765) * t114 - 0.45D2 * t172 * t763 
     #- t765 * t168 - 0.90D2 * t851) * t3 * t51 / 0.2880D4 + (0.90D2 * t
     #6 * pi * (-(-t764 + t204 * t765) * t44 - t198 * t765 + t211 * t765
     #) - 0.180D3 * t217 * t64 * t784) * t59 * t67 / 0.1440D4 - (-0.90D2
     # * t6 * t813 * t235 + t822 * t239) * t67 / 0.1440D4
      t881 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t880)
      t883 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t5
     #42, 0.0D0, -t549)
      t888 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t5
     #42, 0.0D0, -t549)
      t901 = t73 * t883 * t59 * t552 / 0.8D1 + (0.90D2 * t6 * pi * (t888
     # - t566 * t883) - 0.180D3 * t50 * t51 * t883) * t67 * t61 / 0.720D
     #3
      t902 = FJET(XB1, XB2, s, t544, -t542, t540, 0.0D0, -t549, t901)
      t904 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t493, t491, 0.0D0)
      t905 = t904 * t501
      t909 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t493, t491, 0.0D0)
      t924 = -t65 * t905 * t69 / 0.8D1 + (0.90D2 * t6 * pi * (-z * t909 
     #+ t517 * t904) * t501 + 0.180D3 * t217 * t64 * t905) * t59 * t67 /
     # 0.1440D4
      t925 = FJET(XB1, XB2, s, t491, 0.0D0, -t493, 0.0D0, 0.0D0, t924)
      t927 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t5
     #42, 0.0D0, -t549)
      t933 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t5
     #42, 0.0D0, -t549)
      t945 = t73 * t927 * t59 * t552 / 0.8D1 + (0.90D2 * t6 * pi * (-t56
     #6 * t927 + t933) - 0.180D3 * t50 * t51 * t927) * t67 * t61 / 0.720
     #D3
      t946 = FJET(XB1, XB2, s, t540, 0.0D0, t544, -t542, -t549, t945)
      t951 = t28 * s * t1 * t541 * t489
      t952 = t2 * x1
      t954 = Sqrt(-t614 * t495)
      t955 = t37 * t954
      t961 = t952 * x2 * (-x3 + t487 - z + t38 - x1 + t606 + t536 - t607
     # + 0.2D1 * t955) * t538 * t489
      t962 = t542 * t490
      t963 = t195 * x1
      t967 = t195 * t536
      t971 = t952 * (t963 + t195 * z - x2 + t487 + 0.2D1 * t955 * x2 + 0
     #.1D1 - x3 - t967) * t538 * t489
      t973 = t8 * x2
      t974 = x2 * x1
      t983 = t10 * x2
      t985 = t974 * z
      t988 = t973 + t607 - t609 + t610 - t613 + t967 - 0.2D1 * t955 * t9
     #74 - 0.2D1 * t487 * t536 + t612 * t974 + 0.2D1 * t9 * x2 * z - t9 
     #* t983 + 0.2D1 * t955 * t985 + t536
      t999 = t38 + t9 - t611 + t10 - t963 - 0.2D1 * t973 * z + t985 - t4
     #87 * z + t487 * x1 - t9 * x2 + t8 * t10 * x2 - t983 * x1 - 0.2D1 *
     # t955 * z
      t1001 = 0.1D1 / (t988 + t999)
      t1003 = t6 * pi * t537 * t1001
      t1004 = -z + t985 - t974
      t1005 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t961, -t971, t
     #951, -t962, -t549)
      t1010 = FJET(XB1, XB2, s, t951, t961, -t962, -t971, -t549, -t1003 
     #* t1004 * t1005 * t69 / 0.8D1)
      t1012 = t51 * t537
      t1014 = t1001 * t1004
      t1019 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t961, -t971, t
     #951, -t962, -t549)
      t1024 = FJET(XB1, XB2, s, t961, t951, -t971, -t962, -t549, -t1003 
     #* t1004 * t1019 * t69 / 0.8D1)
      t1031 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, 
     #-t542, 0.0D0, t584)
      t1033 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, 
     #-t542, 0.0D0, t584)
      t1045 = t594 * t1033 * t621 - t1033
      t1062 = t51 * t1033
      t1071 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, 
     #-t542, 0.0D0, t584)
      t1086 = (0.90D2 * t6 * pi * (-(-t594 * t1031 + t602 * t537 * t1033
     #) * t621 + t591 * t1033 - t1031) - 0.180D3 * t50 * t51 * t1045) * 
     #t59 * t61 / 0.1440D4 + t73 * t1045 * t59 * t552 / 0.8D1 + (0.90D2 
     #* t6 * pi * (-t1031 + t643 * t1033) + 0.180D3 * t50 * t1062) * t67
     # * t61 / 0.720D3 - (0.90D2 * t6 * pi * (t661 * t1033 / 0.2D1 + t10
     #71 - t660 * t1031) - 0.180D3 * t50 * t51 * (-t660 * t1033 + t1031)
     # + t115 * t1062) * t61 / 0.1440D4
      t1087 = FJET(XB1, XB2, s, -t542, 0.0D0, 0.0D0, -t583, t584, t1086)
      t1089 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t
     #542, 0.0D0, -t549)
      t1095 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t544, t540, -t
     #542, 0.0D0, -t549)
      t1107 = t73 * t1089 * t59 * t552 / 0.8D1 + (0.90D2 * t6 * pi * (-t
     #566 * t1089 + t1095) - 0.180D3 * t50 * t51 * t1089) * t67 * t61 / 
     #0.720D3
      t1108 = FJET(XB1, XB2, s, -t542, t544, 0.0D0, t540, -t549, t1107)
      t1110 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, 
     #-t542, 0.0D0, t584)
      t1112 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, 
     #-t542, 0.0D0, t584)
      t1124 = t594 * t1112 * t621 - t1112
      t1141 = t51 * t1112
      t1150 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t583, 
     #-t542, 0.0D0, t584)
      t1165 = (0.90D2 * t6 * pi * (-(-t594 * t1110 + t602 * t537 * t1112
     #) * t621 - t1110 + t591 * t1112) - 0.180D3 * t50 * t51 * t1124) * 
     #t59 * t61 / 0.1440D4 + t73 * t1124 * t59 * t552 / 0.8D1 + (0.90D2 
     #* t6 * pi * (t643 * t1112 - t1110) + 0.180D3 * t50 * t1141) * t67 
     #* t61 / 0.720D3 - (0.90D2 * t6 * pi * (t661 * t1112 / 0.2D1 + t115
     #0 - t660 * t1110) - 0.180D3 * t50 * t51 * (-t660 * t1112 + t1110) 
     #+ t115 * t1141) * t61 / 0.1440D4
      t1166 = FJET(XB1, XB2, s, -t583, 0.0D0, 0.0D0, -t542, t584, t1165)
      t1168 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t493, t491, 0.0D0)
      t1169 = t1168 * t501
      t1173 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t493, t491, 0.0D0)
      t1188 = -t65 * t1169 * t69 / 0.8D1 + (0.90D2 * t6 * pi * (-z * t11
     #73 + t517 * t1168) * t501 + 0.180D3 * t217 * t64 * t1169) * t59 * 
     #t67 / 0.1440D4
      t1189 = FJET(XB1, XB2, s, -t493, 0.0D0, t491, 0.0D0, 0.0D0, t1188)
      t1191 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t961, -t971, t
     #951, -t962, -t549)
      t1196 = FJET(XB1, XB2, s, -t971, -t962, t961, t951, -t549, -t1003 
     #* t1004 * t1191 * t69 / 0.8D1)
      t1203 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t961, -t971, t
     #951, -t962, -t549)
      t1208 = FJET(XB1, XB2, s, -t962, -t971, t951, t961, -t549, -t1003 
     #* t1004 * t1203 * t69 / 0.8D1)
      rrqg2qght8s3e0 = t245 * t244 + t365 * t364 + t485 * t484 + t532 * 
     #t531 + t580 * t579 + t680 * t679 + t738 * t737 + t761 * t760 + t88
     #1 * t880 + t902 * t901 + t925 * t924 + t946 * t945 - t1010 * t3 * 
     #t1012 * t1014 * t1005 * t69 / 0.8D1 - t1024 * t3 * t1012 * t1014 *
     # t1019 * t69 / 0.8D1 + t1087 * t1086 + t1108 * t1107 + t1166 * t11
     #65 + t1189 * t1188 - t1196 * t3 * t1012 * t1014 * t1191 * t69 / 0.
     #8D1 - t1208 * t3 * t1012 * t1014 * t1203 * t69 / 0.8D1

      end function



      doubleprecision function rrqg2qght8s3em1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
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
      t38 = t24 * z * t33 + t37
      t42 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t46 = lh * t3
      t47 = t5 * pi
      t50 = 0.180D3 * t46 * t47 * t7
      t53 = -z * t33 - 0.1D1
      t56 = 0.1D1 / x3
      t59 = x1 ** 2
      t60 = t59 * t15
      t64 = log(0.4D1 * t60 * t11 * t17)
      t71 = 0.1D1 / x1
      t74 = t6 * pi
      t83 = t6 * pi * z
      t85 = 0.1D1 / x2
      t86 = t56 * t85
      t90 = x2 ** 2
      t91 = t11 * t90
      t94 = log(0.4D1 * t91 * t18)
      t95 = -0.1D1 + x2
      t99 = log(-0.4D1 * t91 * t18 * t95)
      t100 = -t94 + t99
      t108 = log(0.4D1 * t11 * t15 * t17)
      t115 = t108 ** 2
      t118 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t120 = lh ** 2
      t122 = pi ** 2
      t124 = 0.180D3 * t120 - 0.30D2 * t122
      t130 = (-0.90D2 * t6 * pi * t7 * t38 + (-0.90D2 * t6 * pi * t42 + 
     #t50) * t53) * t56 / 0.2880D4 - (0.90D2 * t6 * pi * (-t42 + t64 * t
     #7) + t50) * t71 / 0.1440D4 + t74 * (z * t7 * t33 + t7) * t56 * t71
     # / 0.16D2 + t83 * t7 * t33 * t86 / 0.16D2 + t74 * t7 * t100 * t85 
     #/ 0.16D2 - (-0.180D3 * (-t42 + t108 * t7) * lh + 0.90D2 * t108 * t
     #42 - 0.45D2 * t115 * t7 - 0.90D2 * t118 - t7 * t124) * t3 * t47 / 
     #0.2880D4
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t130)
      t133 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t138 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t144 = 0.180D3 * t46 * t47 * t133
      t175 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t188 = (-0.90D2 * t6 * pi * t133 * t38 + (-0.90D2 * t6 * pi * t138
     # + t144) * t53) * t56 / 0.2880D4 - (0.90D2 * t6 * pi * (-t138 + t6
     #4 * t133) + t144) * t71 / 0.1440D4 + t74 * (t133 + z * t133 * t33)
     # * t56 * t71 / 0.16D2 + t83 * t133 * t33 * t86 / 0.16D2 + t74 * t1
     #33 * t100 * t85 / 0.16D2 - (0.90D2 * t108 * t138 - 0.90D2 * t175 -
     # t133 * t124 - 0.180D3 * (-t138 + t108 * t133) * lh - 0.45D2 * t11
     #5 * t133) * t3 * t47 / 0.2880D4
      t189 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t188)
      t191 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t196 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t202 = 0.180D3 * t46 * t47 * t191
      t237 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t246 = (-0.90D2 * t6 * pi * t191 * t38 + (-0.90D2 * t6 * pi * t196
     # + t202) * t53) * t56 / 0.2880D4 - (0.90D2 * t6 * pi * (t64 * t191
     # - t196) + t202) * t71 / 0.1440D4 + t74 * (z * t191 * t33 + t191) 
     #* t56 * t71 / 0.16D2 + t83 * t191 * t33 * t86 / 0.16D2 + t74 * t19
     #1 * t100 * t85 / 0.16D2 - (-0.180D3 * (-t196 + t108 * t191) * lh +
     # 0.90D2 * t108 * t196 - 0.90D2 * t237 - 0.45D2 * t115 * t191 - t19
     #1 * t124) * t3 * t47 / 0.2880D4
      t247 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t246)
      t249 = x2 * x3
      t251 = 0.1D1 / (0.1D1 - x3 + t249)
      t253 = t2 * t249 * t251
      t255 = t2 * t19 * t251
      t256 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t255, t253, 0.0D0)
      t259 = Sqrt(t27 * t95 * t19)
      t263 = 0.1D1 / (-z - x3 + t249 + 0.2D1 * t26 * t259)
      t268 = FJET(XB1, XB2, s, 0.0D0, t253, 0.0D0, -t255, 0.0D0, -t83 * 
     #t256 * t263 * t86 / 0.16D2)
      t273 = t263 * t56 * t85
      t278 = t1 * x1
      t279 = x1 * z
      t280 = -z - x1 + t279
      t281 = 0.1D1 / t280
      t283 = t95 * s * t278 * t281
      t284 = -0.1D1 + x1
      t285 = t2 * t284
      t287 = x2 * s * t278
      t288 = s * t16
      t291 = x1 * t284 * t281
      t292 = t288 * t95 * t291
      t293 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t287, t283, -t2
     #85, 0.0D0, -t292)
      t298 = FJET(XB1, XB2, s, 0.0D0, t283, -t285, t287, -t292, t74 * t2
     #93 * t85 * t71 / 0.8D1)
      t302 = t85 * t71
      t307 = t2 * x1 * t281
      t308 = t288 * t291
      t309 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t317 = t284 ** 2
      t321 = log(-0.4D1 * t60 / t9 * t17 * t281 * t317)
      t323 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t334 = z * t280
      t337 = x3 * t59
      t346 = Sqrt(x3 * t280 * t19)
      t351 = 0.1D1 / (-t279 - x3 * x1 * z - t27 - t337 + 0.2D1 * t337 * 
     #z - t337 * t9 + x1 * t9 + x3 * t9 * x1 - t9 + 0.2D1 * t26 * t346 *
     # z)
      t359 = -t74 * t309 * t85 * t71 / 0.8D1 - (0.90D2 * t6 * pi * (-t32
     #1 * t309 + t323) - 0.180D3 * t46 * t47 * t309) * t71 / 0.1440D4 + 
     #t74 * (-t309 + t334 * t309 * t351) * t56 * t71 / 0.16D2
      t360 = FJET(XB1, XB2, s, 0.0D0, -t285, -t307, 0.0D0, t308, t359)
      t362 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t367 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t386 = -t74 * t362 * t85 * t71 / 0.8D1 - (0.90D2 * t6 * pi * (t367
     # - t321 * t362) - 0.180D3 * t46 * t47 * t362) * t71 / 0.1440D4 + t
     #74 * (-t362 + t334 * t362 * t351) * t56 * t71 / 0.16D2
      t387 = FJET(XB1, XB2, s, 0.0D0, -t307, -t285, 0.0D0, t308, t386)
      t389 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t255, t253, 0.0D0)
      t394 = FJET(XB1, XB2, s, 0.0D0, -t255, 0.0D0, t253, 0.0D0, -t83 * 
     #t389 * t263 * t86 / 0.16D2)
      t401 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t406 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t412 = 0.180D3 * t46 * t47 * t401
      t447 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t456 = (-0.90D2 * t6 * pi * t401 * t38 + (-0.90D2 * t6 * pi * t406
     # + t412) * t53) * t56 / 0.2880D4 - (0.90D2 * t6 * pi * (-t406 + t6
     #4 * t401) + t412) * t71 / 0.1440D4 + t74 * (t401 + z * t401 * t33)
     # * t56 * t71 / 0.16D2 + t83 * t401 * t33 * t86 / 0.16D2 + t74 * t4
     #01 * t100 * t85 / 0.16D2 - (-0.180D3 * (-t406 + t108 * t401) * lh 
     #- 0.45D2 * t115 * t401 - 0.90D2 * t447 + 0.90D2 * t108 * t406 - t4
     #01 * t124) * t3 * t47 / 0.2880D4
      t457 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t456)
      t459 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t287, t283, -t2
     #85, 0.0D0, -t292)
      t464 = FJET(XB1, XB2, s, t287, -t285, t283, 0.0D0, -t292, t74 * t4
     #59 * t85 * t71 / 0.8D1)
      t471 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t255, t253, 0.0D0)
      t476 = FJET(XB1, XB2, s, t253, 0.0D0, -t255, 0.0D0, 0.0D0, -t83 * 
     #t471 * t263 * t86 / 0.16D2)
      t483 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t287, t283, -t2
     #85, 0.0D0, -t292)
      t488 = FJET(XB1, XB2, s, t283, 0.0D0, t287, -t285, -t292, t74 * t4
     #83 * t85 * t71 / 0.8D1)
      t495 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t501 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t519 = -t74 * t495 * t85 * t71 / 0.8D1 - (0.90D2 * t6 * pi * (-t32
     #1 * t495 + t501) - 0.180D3 * t46 * t47 * t495) * t71 / 0.1440D4 + 
     #t74 * (t334 * t495 * t351 - t495) * t56 * t71 / 0.16D2
      t520 = FJET(XB1, XB2, s, -t285, 0.0D0, 0.0D0, -t307, t308, t519)
      t522 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t287, t283, -t2
     #85, 0.0D0, -t292)
      t527 = FJET(XB1, XB2, s, -t285, t287, 0.0D0, t283, -t292, t74 * t5
     #22 * t85 * t71 / 0.8D1)
      t534 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t540 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, -
     #t285, 0.0D0, t308)
      t558 = -t74 * t534 * t85 * t71 / 0.8D1 - (0.90D2 * t6 * pi * (-t32
     #1 * t534 + t540) - 0.180D3 * t46 * t47 * t534) * t71 / 0.1440D4 + 
     #t74 * (t334 * t534 * t351 - t534) * t56 * t71 / 0.16D2
      t559 = FJET(XB1, XB2, s, -t307, 0.0D0, 0.0D0, -t285, t308, t558)
      t561 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t255, t253, 0.0D0)
      t566 = FJET(XB1, XB2, s, -t255, 0.0D0, t253, 0.0D0, 0.0D0, -t83 * 
     #t561 * t263 * t86 / 0.16D2)
      rrqg2qght8s3em1 = t131 * t130 + t189 * t188 + t247 * t246 - t268 *
     # t3 * t47 * z * t256 * t273 / 0.16D2 + t298 * t3 * t5 * pi * t293 
     #* t302 / 0.8D1 + t360 * t359 + t387 * t386 - t394 * t3 * t47 * z *
     # t389 * t273 / 0.16D2 + t457 * t456 + t464 * t3 * t5 * pi * t459 *
     # t302 / 0.8D1 - t476 * t3 * t47 * z * t471 * t273 / 0.16D2 + t488 
     #* t3 * t5 * pi * t483 * t302 / 0.8D1 + t520 * t519 + t527 * t3 * t
     #5 * pi * t522 * t302 / 0.8D1 + t559 * t558 - t566 * t3 * t47 * z *
     # t561 * t273 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s3em2
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = 0.1D1 / x1
      t13 = t6 * pi
      t14 = x4 * pi
      t15 = cos(t14)
      t19 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t25 = -z / (-z - x3 + 0.2D1 * t15 * t19) - 0.1D1
      t27 = 0.1D1 / x3
      t31 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t35 = z ** 2
      t38 = Sin(t14)
      t39 = t38 ** 2
      t41 = t1 ** 2
      t42 = t41 ** 2
      t45 = log(0.4D1 / t35 / z * t39 * t42)
      t50 = t5 * pi
      t53 = t6 * pi * t7 * t9 / 0.16D2 - t13 * t7 * t25 * t27 / 0.32D2 -
     # (-0.90D2 * t31 + 0.180D3 * t7 * lh + 0.90D2 * t45 * t7) * t3 * t5
     #0 / 0.2880D4
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t53)
      t56 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t65 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t75 = t6 * pi * t56 * t9 / 0.16D2 - t13 * t56 * t25 * t27 / 0.32D2
     # - (-0.90D2 * t65 + 0.180D3 * t56 * lh + 0.90D2 * t45 * t56) * t3 
     #* t50 / 0.2880D4
      t76 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t75)
      t78 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t91 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t97 = t6 * pi * t78 * t9 / 0.16D2 - t13 * t78 * t25 * t27 / 0.32D2
     # - (0.180D3 * t78 * lh + 0.90D2 * t45 * t78 - 0.90D2 * t91) * t3 *
     # t50 / 0.2880D4
      t98 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t97)
      t100 = -0.1D1 + x1
      t101 = t2 * t100
      t104 = 0.1D1 / (-z - x1 + x1 * z)
      t106 = t2 * x1 * t104
      t110 = s * t41 * x1 * t100 * t104
      t111 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, -
     #t101, 0.0D0, t110)
      t113 = pi * t111 * t9
      t116 = FJET(XB1, XB2, s, 0.0D0, -t101, -t106, 0.0D0, t110, -t6 * t
     #113 / 0.16D2)
      t121 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, -
     #t101, 0.0D0, t110)
      t123 = pi * t121 * t9
      t126 = FJET(XB1, XB2, s, 0.0D0, -t106, -t101, 0.0D0, t110, -t6 * t
     #123 / 0.16D2)
      t131 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t144 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t150 = t6 * pi * t131 * t9 / 0.16D2 - t13 * t131 * t25 * t27 / 0.3
     #2D2 - (0.180D3 * t131 * lh + 0.90D2 * t45 * t131 - 0.90D2 * t144) 
     #* t3 * t50 / 0.2880D4
      t151 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t150)
      t153 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, -
     #t101, 0.0D0, t110)
      t155 = pi * t153 * t9
      t158 = FJET(XB1, XB2, s, -t101, 0.0D0, 0.0D0, -t106, t110, -t6 * t
     #155 / 0.16D2)
      t163 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, -
     #t101, 0.0D0, t110)
      t165 = pi * t163 * t9
      t168 = FJET(XB1, XB2, s, -t106, 0.0D0, 0.0D0, -t101, t110, -t6 * t
     #165 / 0.16D2)
      rrqg2qght8s3em2 = t54 * t53 + t76 * t75 + t98 * t97 - t116 * t3 * 
     #t5 * t113 / 0.16D2 - t126 * t3 * t5 * t123 / 0.16D2 + t151 * t150 
     #- t158 * t3 * t5 * t155 / 0.16D2 - t168 * t3 * t5 * t165 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s3em3
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * pi * 
     #t7 / 0.32D2)
      t13 = t5 * pi
      t16 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * pi * 
     #t16 / 0.32D2)
      t24 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t24 / 0.32D2)
      t32 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t32 / 0.32D2)
      rrqg2qght8s3em3 = t11 * t3 * t13 * t7 / 0.32D2 + t20 * t3 * t13 * 
     #t16 / 0.32D2 + t28 * t3 * t13 * t24 / 0.32D2 + t36 * t3 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrqg2qght8s3em4
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght8s3em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght8s4e1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = lh ** 2
      t7 = 0.180D3 * t6
      t8 = pi ** 2
      t9 = 0.30D2 * t8
      t10 = t7 - t9
      t11 = 0.1D1 / t1
      t12 = t10 * t11
      t13 = s ** 2
      t14 = 0.1D1 / t13
      t15 = t14 * pi
      t16 = z ** 2
      t18 = 0.1D1 / t16 / z
      t19 = x3 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = x4 * pi
      t23 = Sin(t22)
      t24 = t23 ** 2
      t25 = t21 * t24
      t26 = t25 * t4
      t29 = log(-0.4D1 * t19 * t26)
      t30 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #5, t3, 0.0D0)
      t32 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #5, t3, 0.0D0)
      t36 = t11 * t14
      t37 = t29 ** 2
      t40 = t37 * t29
      t43 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #5, t3, 0.0D0)
      t44 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #5, t3, 0.0D0)
      t51 = 0.60D2 * lh * t8
      t52 = 0.240D3 * zeta3
      t54 = 0.120D3 * t6 * lh
      t55 = t51 - t52 - t54
      t56 = t55 * t11
      t57 = t15 * t30
      t59 = lh * t11
      t68 = 0.1D1 / x3
      t71 = x1 ** 2
      t72 = x3 * t71
      t74 = t24 * t18
      t78 = log(-0.4D1 * t72 * t21 * t74 * t4)
      t80 = t78 ** 2
      t92 = t12 * t57
      t95 = 0.1D1 / x1
      t98 = t72 * x2
      t99 = t18 * t4
      t103 = log(-0.4D1 * t98 * t25 * t99)
      t113 = 0.1D1 / x2
      t114 = t113 * t95
      t117 = x2 * x3
      t118 = t117 * t18
      t121 = log(-0.4D1 * t118 * t26)
      t123 = t121 ** 2
      t139 = -(t12 * t15 * (-t29 * t30 + t32) + 0.90D2 * t36 * pi * (t37
     # * t32 / 0.2D1 - t40 * t30 / 0.6D1 + t43 - t29 * t44) + t56 * t57 
     #- 0.180D3 * t59 * t15 * (t37 * t30 / 0.2D1 + t44 - t29 * t32)) * t
     #68 / 0.1440D4 - (0.90D2 * t36 * pi * (t44 - t78 * t32 + t80 * t30 
     #/ 0.2D1) - 0.180D3 * t59 * t15 * (t32 - t78 * t30) + t92) * t68 * 
     #t95 / 0.720D3 - (0.90D2 * t36 * pi * (-t103 * t30 + t32) - 0.180D3
     # * t59 * t57) * t68 * t114 / 0.720D3 + (0.90D2 * t36 * pi * (t121 
     #* t32 - t123 * t30 / 0.2D1 - t44) - 0.180D3 * t59 * t15 * (t121 * 
     #t30 - t32) - t92) * t68 * t113 / 0.1440D4
      t140 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t139)
      t142 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t5, t3, 0.0D0)
      t143 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t5, t3, 0.0D0)
      t146 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t5, t3, 0.0D0)
      t157 = t15 * t143
      t158 = t12 * t157
      t180 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t5, t3, 0.0D0)
      t215 = -(0.90D2 * t36 * pi * (t142 + t80 * t143 / 0.2D1 - t78 * t1
     #46) - 0.180D3 * t59 * t15 * (t146 - t78 * t143) + t158) * t68 * t9
     #5 / 0.720D3 - (0.90D2 * t36 * pi * (-t103 * t143 + t146) - 0.180D3
     # * t59 * t157) * t68 * t114 / 0.720D3 - (t12 * t15 * (t146 - t29 *
     # t143) + 0.90D2 * t36 * pi * (-t40 * t143 / 0.6D1 + t180 + t37 * t
     #146 / 0.2D1 - t29 * t142) + t56 * t157 - 0.180D3 * t59 * t15 * (t1
     #42 + t37 * t143 / 0.2D1 - t29 * t146)) * t68 / 0.1440D4 + (0.90D2 
     #* t36 * pi * (-t123 * t143 / 0.2D1 + t121 * t146 - t142) - 0.180D3
     # * t59 * t15 * (-t146 + t121 * t143) - t158) * t68 * t113 / 0.1440
     #D4
      t216 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t215)
      t218 = t2 * x1
      t219 = cos(t22)
      t220 = x1 * z
      t221 = -z - x1 + t220
      t223 = x2 * t4
      t225 = Sqrt(x3 * t221 * t223)
      t226 = t219 * t225
      t227 = 0.2D1 * t226
      t230 = x3 * x1
      t231 = t230 * z
      t232 = t117 * z
      t234 = t117 * x1
      t236 = x2 ** 2
      t237 = t236 * x3
      t238 = t237 * x1
      t240 = x3 * z
      t243 = t237 * t220
      t244 = -t227 + 0.2D1 * t226 * x2 + t231 + 0.2D1 * t232 + 0.2D1 * t
     #234 - t238 - t237 * z + t117 - t240 - t230 - x2 - 0.2D1 * t117 * t
     #220 + t243
      t245 = 0.1D1 / t221
      t247 = -0.1D1 + t117
      t248 = 0.1D1 / t247
      t250 = t218 * t244 * t245 * t248
      t251 = -0.1D1 + x1
      t252 = t2 * t251
      t253 = -0.1D1 + x2
      t255 = x3 * t253 * t248
      t256 = t252 * t255
      t261 = t218 * t253 * (-t117 - z + t240 - x1 + t230 + t220 - t231 +
     # t227) * t245 * t248
      t262 = t4 * s
      t263 = t1 * t251
      t265 = t262 * t263 * t248
      t270 = s * t20 * x2 * x1 * t251 * t245
      t271 = x2 * x1
      t272 = t271 * z
      t273 = z + t272 - t220 - t271 + x1
      t274 = t221 * t273
      t275 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -t
     #265, -t256, t270)
      t277 = t71 * t21
      t280 = 0.1D1 / t16
      t281 = t251 ** 2
      t284 = t253 ** 2
      t286 = t247 ** 2
      t287 = 0.1D1 / t286
      t292 = log(0.4D1 * t117 * t277 * t24 * t280 * t281 * t245 * t284 *
     # t4 * t287)
      t293 = t292 * t221
      t294 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -t
     #265, -t256, t270)
      t299 = t71 * x2
      t302 = t71 * t16
      t304 = t16 * x2
      t309 = -0.2D1 * t299 * z + t272 - t232 - t234 - t98 + t238 + t302 
     #* x2 - t304 * x1 + t299 - t16 - 0.2D1 * t220 - 0.2D1 * t226 * t271
      t328 = -0.2D1 * t226 * t220 + x3 * t16 * t271 + 0.2D1 * t72 * x2 *
     # z - t72 * t304 - t243 + 0.2D1 * t226 * t272 + 0.2D1 * t226 * z + 
     #0.2D1 * t226 * x1 - t71 + 0.2D1 * x1 * t16 + 0.2D1 * t71 * z - t30
     #2
      t330 = 0.1D1 / (t309 + t328)
      t334 = t59 * t15
      t339 = 0.90D2 * t36 * pi * (t274 * t275 - t293 * t273 * t294) * t3
     #30 - 0.180D3 * t334 * t274 * t294 * t330
      t343 = FJET(XB1, XB2, s, -t250, -t256, t261, -t265, t270, -t339 * 
     #t68 * t114 / 0.720D3)
      t346 = t68 * t113 * t95
      t349 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t350 = t74 * t21
      t353 = log(0.4D1 * t72 * t350)
      t354 = t353 ** 2
      t355 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t358 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t369 = t15 * t355
      t370 = t12 * t369
      t375 = t71 * t24
      t376 = t18 * t21
      t379 = log(0.4D1 * t375 * t376)
      t385 = t379 ** 2
      t386 = t385 * t379
      t391 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t396 = t56 * t369
      t407 = t36 * pi
      t412 = log(0.4D1 * t98 * t25 * t18 * t284)
      t416 = log(0.4D1 * t98 * t350)
      t423 = t299 * t21
      t427 = log(0.4D1 * t423 * t74 * t284)
      t431 = log(0.4D1 * t299 * t350)
      t432 = t431 ** 2
      t436 = t427 ** 2
      t455 = log(0.4D1 * t117 * t350)
      t457 = t25 * t284
      t460 = log(0.4D1 * t118 * t457)
      t461 = t460 ** 2
      t465 = t455 ** 2
      t488 = x2 * t18
      t491 = log(0.4D1 * t488 * t457)
      t492 = t491 ** 2
      t495 = log(0.4D1 * t488 * t25)
      t496 = t495 ** 2
      t498 = -t492 / 0.2D1 + t496 / 0.2D1
      t504 = -t496 * t495 / 0.6D1 + t492 * t491 / 0.6D1
      t511 = t15 * t358
      t515 = t491 - t495
      t520 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t525 = log(0.4D1 * t350)
      t526 = t525 ** 2
      t529 = t526 * t525
      t533 = (-0.90D2 * t526 * lh + t51 - t52 - t54 - 0.15D2 * t529 - t5
     #25 * t10) * t11
      t538 = log(0.4D1 * t19 * t25)
      t543 = t538 ** 2
      t546 = t543 * t538
      t568 = (0.180D3 * t525 * lh + 0.45D2 * t526 + t7 - t9) * t11
      t575 = (-0.180D3 * lh - 0.90D2 * t525) * t11
      t584 = t8 ** 2
      t585 = t6 ** 2
      t591 = t526 ** 2
      t594 = (0.30D2 * t529 * lh + t526 * t10 / 0.2D1 - t525 * t55 + t58
     #4 + 0.60D2 * t585 + 0.480D3 * lh * zeta3 - 0.60D2 * t6 * t8 + 0.15
     #D2 / 0.4D1 * t591) * t11
      t597 = -(0.90D2 * t36 * pi * (-t349 - t354 * t355 / 0.2D1 + t353 *
     # t358) - 0.180D3 * t59 * t15 * (t353 * t355 - t358) - t370) * t68 
     #* t95 / 0.720D3 + (t12 * t15 * (t358 - t379 * t355) + 0.90D2 * t36
     # * pi * (-t379 * t349 - t386 * t355 / 0.6D1 + t385 * t358 / 0.2D1 
     #+ t391) + t396 - 0.180D3 * t59 * t15 * (-t379 * t358 + t385 * t355
     # / 0.2D1 + t349)) * t95 / 0.720D3 - t407 * (-t412 * t355 + t416 * 
     #t355) * t68 * t114 / 0.8D1 + (0.90D2 * t36 * pi * (t427 * t358 + t
     #432 * t355 / 0.2D1 - t431 * t358 - t436 * t355 / 0.2D1) - 0.180D3 
     #* t59 * t15 * (t427 * t355 - t431 * t355)) * t113 * t95 / 0.720D3 
     #+ (0.90D2 * t36 * pi * (-t455 * t358 - t461 * t355 / 0.2D1 + t460 
     #* t358 + t465 * t355 / 0.2D1) - 0.180D3 * t59 * t15 * (-t455 * t35
     #5 + t460 * t355)) * t68 * t113 / 0.1440D4 - ((-0.90D2 * t36 * pi *
     # t358 + 0.180D3 * t59 * t369) * t498 - 0.90D2 * t36 * pi * t355 * 
     #t504 + (-0.90D2 * t36 * pi * t349 + 0.180D3 * t59 * t511 - t370) *
     # t515) * t113 / 0.1440D4 + t36 * pi * t520 / 0.16D2 + t533 * t511 
     #/ 0.1440D4 - (t12 * t15 * (-t358 + t538 * t355) + 0.90D2 * t36 * p
     #i * (-t543 * t358 / 0.2D1 + t546 * t355 / 0.6D1 - t391 + t538 * t3
     #49) - t396 - 0.180D3 * t59 * t15 * (t538 * t358 - t543 * t355 / 0.
     #2D1 - t349)) * t68 / 0.1440D4 + t568 * t15 * t349 / 0.1440D4 + t57
     #5 * t15 * t391 / 0.1440D4 + t594 * t369 / 0.1440D4
      t598 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t597)
      t600 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -t
     #265, -t256, t270)
      t602 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -t
     #265, -t256, t270)
      t614 = 0.90D2 * t36 * pi * (t274 * t600 - t293 * t273 * t602) * t3
     #30 - 0.180D3 * t334 * t274 * t602 * t330
      t618 = FJET(XB1, XB2, s, -t256, -t250, -t265, t261, t270, -t614 * 
     #t68 * t114 / 0.720D3)
      t624 = t245 * t281
      t628 = log(-0.4D1 * t72 * t24 * t280 * t21 * t624)
      t629 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -t
     #252, 0.0D0, 0.0D0)
      t631 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -t
     #252, 0.0D0, 0.0D0)
      t632 = t628 ** 2
      t633 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -t
     #252, 0.0D0, 0.0D0)
      t645 = t15 * t633
      t646 = t12 * t645
      t655 = log(-0.4D1 * t375 * t280 * t21 * t245 * t281)
      t660 = t655 ** 2
      t661 = t660 * t655
      t667 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -t
     #252, 0.0D0, 0.0D0)
      t682 = t117 * t277
      t683 = t24 * t280
      t684 = t683 * t624
      t687 = log(-0.4D1 * t682 * t684)
      t700 = log(-0.4D1 * t423 * t684)
      t701 = t700 ** 2
      t718 = -(0.90D2 * t36 * pi * (-t628 * t629 + t631 + t632 * t633 / 
     #0.2D1) - 0.180D3 * t59 * t15 * (-t628 * t633 + t629) + t646) * t68
     # * t95 / 0.720D3 + (t12 * t15 * (-t629 + t655 * t633) + 0.90D2 * t
     #36 * pi * (t661 * t633 / 0.6D1 - t660 * t629 / 0.2D1 + t655 * t631
     # - t667) - t56 * t645 - 0.180D3 * t59 * t15 * (-t660 * t633 / 0.2D
     #1 - t631 + t655 * t629)) * t95 / 0.720D3 - (0.90D2 * t36 * pi * (-
     #t687 * t633 + t629) - 0.180D3 * t59 * t645) * t68 * t114 / 0.720D3
     # + (0.90D2 * t36 * pi * (-t631 - t701 * t633 / 0.2D1 + t700 * t629
     #) - 0.180D3 * t59 * t15 * (t700 * t633 - t629) - t646) * t113 * t9
     #5 / 0.720D3
      t719 = FJET(XB1, XB2, s, t218, -t252, 0.0D0, 0.0D0, 0.0D0, t718)
      t721 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t722 = t15 * t721
      t725 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t730 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t736 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t741 = t56 * t722
      t786 = t15 * t725
      t789 = t12 * t722
      t795 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t873 = t594 * t722 / 0.1440D4 - (t12 * t15 * (-t725 + t538 * t721)
     # + 0.90D2 * t36 * pi * (t538 * t730 + t546 * t721 / 0.6D1 - t543 *
     # t725 / 0.2D1 - t736) - t741 - 0.180D3 * t59 * t15 * (-t730 + t538
     # * t725 - t543 * t721 / 0.2D1)) * t68 / 0.1440D4 + (0.90D2 * t36 *
     # pi * (-t455 * t725 + t460 * t725 + t465 * t721 / 0.2D1 - t461 * t
     #721 / 0.2D1) - 0.180D3 * t59 * t15 * (-t455 * t721 + t460 * t721))
     # * t68 * t113 / 0.1440D4 - ((-0.90D2 * t36 * pi * t725 + 0.180D3 *
     # t59 * t722) * t498 - 0.90D2 * t36 * pi * t721 * t504 + (-0.90D2 *
     # t36 * pi * t730 + 0.180D3 * t59 * t786 - t789) * t515) * t113 / 0
     #.1440D4 + t36 * pi * t795 / 0.16D2 + t533 * t786 / 0.1440D4 + t568
     # * t15 * t730 / 0.1440D4 - (0.90D2 * t36 * pi * (-t354 * t721 / 0.
     #2D1 - t730 + t353 * t725) - 0.180D3 * t59 * t15 * (t353 * t721 - t
     #725) - t789) * t68 * t95 / 0.720D3 + (t12 * t15 * (t725 - t379 * t
     #721) + 0.90D2 * t36 * pi * (t736 + t385 * t725 / 0.2D1 - t379 * t7
     #30 - t386 * t721 / 0.6D1) + t741 - 0.180D3 * t59 * t15 * (-t379 * 
     #t725 + t730 + t385 * t721 / 0.2D1)) * t95 / 0.720D3 - t407 * (-t41
     #2 * t721 + t416 * t721) * t68 * t114 / 0.8D1 + (0.90D2 * t36 * pi 
     #* (t432 * t721 / 0.2D1 - t436 * t721 / 0.2D1 + t427 * t725 - t431 
     #* t725) - 0.180D3 * t59 * t15 * (-t431 * t721 + t427 * t721)) * t1
     #13 * t95 / 0.720D3 + t575 * t15 * t736 / 0.1440D4
      t874 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t873)
      t877 = t1 * x1
      t878 = t253 * s * t877
      t880 = t2 * t271 * t245
      t885 = log(-0.4D1 * t682 * t683 * t624 * t284)
      t886 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, -
     #t252, 0.0D0, t270)
      t888 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, -
     #t252, 0.0D0, t270)
      t893 = t15 * t886
      t900 = t280 * t245
      t905 = log(-0.4D1 * t299 * t25 * t900 * t281 * t284)
      t906 = t905 ** 2
      t909 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, -
     #t252, 0.0D0, t270)
      t925 = -(0.90D2 * t36 * pi * (t885 * t886 - t888) + 0.180D3 * t59 
     #* t893) * t68 * t114 / 0.720D3 + (0.90D2 * t36 * pi * (t906 * t886
     # / 0.2D1 + t909 - t905 * t888) - 0.180D3 * t59 * t15 * (t888 - t90
     #5 * t886) + t12 * t893) * t113 * t95 / 0.720D3
      t926 = FJET(XB1, XB2, s, -t878, -t252, -t880, 0.0D0, t270, t925)
      t928 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t929 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t934 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t938 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t945 = t15 * t929
      t946 = t56 * t945
      t991 = t15 * t928
      t994 = t12 * t945
      t1005 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1080 = -(t12 * t15 * (-t928 + t538 * t929) + 0.90D2 * t36 * pi * 
     #(t538 * t934 + t546 * t929 / 0.6D1 - t938 - t543 * t928 / 0.2D1) -
     # t946 - 0.180D3 * t59 * t15 * (-t543 * t929 / 0.2D1 - t934 + t538 
     #* t928)) * t68 / 0.1440D4 + (0.90D2 * t36 * pi * (-t455 * t928 + t
     #460 * t928 + t465 * t929 / 0.2D1 - t461 * t929 / 0.2D1) - 0.180D3 
     #* t59 * t15 * (-t455 * t929 + t460 * t929)) * t68 * t113 / 0.1440D
     #4 - ((-0.90D2 * t36 * pi * t928 + 0.180D3 * t59 * t945) * t498 - 0
     #.90D2 * t36 * pi * t929 * t504 + (-0.90D2 * t36 * pi * t934 + 0.18
     #0D3 * t59 * t991 - t994) * t515) * t113 / 0.1440D4 + t575 * t15 * 
     #t938 / 0.1440D4 + t594 * t945 / 0.1440D4 + t36 * pi * t1005 / 0.16
     #D2 + t533 * t991 / 0.1440D4 - (0.90D2 * t36 * pi * (t353 * t928 - 
     #t934 - t354 * t929 / 0.2D1) - 0.180D3 * t59 * t15 * (t353 * t929 -
     # t928) - t994) * t68 * t95 / 0.720D3 + (t12 * t15 * (t928 - t379 *
     # t929) + 0.90D2 * t36 * pi * (-t379 * t934 - t386 * t929 / 0.6D1 +
     # t385 * t928 / 0.2D1 + t938) + t946 - 0.180D3 * t59 * t15 * (t385 
     #* t929 / 0.2D1 - t379 * t928 + t934)) * t95 / 0.720D3 - t407 * (t4
     #16 * t929 - t412 * t929) * t68 * t114 / 0.8D1 + (0.90D2 * t36 * pi
     # * (t427 * t928 + t432 * t929 / 0.2D1 - t431 * t928 - t436 * t929 
     #/ 0.2D1) - 0.180D3 * t59 * t15 * (t427 * t929 - t431 * t929)) * t1
     #13 * t95 / 0.720D3 + t568 * t15 * t934 / 0.1440D4
      t1081 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1080)
      t1083 = t262 * t263
      t1084 = t262 * t877
      t1086 = x3 * s * t263
      t1087 = t2 * t230
      t1093 = log(0.4D1 * t72 * t25 * t900 * t281 * t4)
      t1094 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1096 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1097 = t1093 ** 2
      t1098 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1110 = t15 * t1098
      t1119 = log(0.4D1 * t682 * t683 * t624 * t4)
      t1131 = -(0.90D2 * t36 * pi * (t1093 * t1094 - t1096 - t1097 * t10
     #98 / 0.2D1) - 0.180D3 * t59 * t15 * (t1093 * t1098 - t1094) - t12 
     #* t1110) * t68 * t95 / 0.720D3 - (0.90D2 * t36 * pi * (t1119 * t10
     #98 - t1094) + 0.180D3 * t59 * t1110) * t68 * t114 / 0.720D3
      t1132 = FJET(XB1, XB2, s, t1083, -t1084, -t1086, t1087, 0.0D0, t11
     #31)
      t1134 = t2 * t255
      t1136 = t2 * t4 * t248
      t1138 = Sqrt(-t240 * t223)
      t1142 = 0.1D1 / (-z - t117 + 0.2D1 * t219 * t1138)
      t1143 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1144 = t1142 * t1143
      t1152 = log(-0.4D1 * t24 * x2 * t72 * t287 * t284 * t99 * t21)
      t1153 = t1152 * t1142
      t1154 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1161 = t59 * t14
      t1162 = pi * t1142
      t1164 = t1162 * t1154 * z
      t1171 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1179 = log(-0.4D1 * t117 * t376 * t24 * t284 * t4 * t287)
      t1180 = t1179 * t1142
      t1182 = t1179 ** 2
      t1183 = t1182 * t1142
      t1197 = t12 * t14
      t1203 = -(0.90D2 * t36 * pi * (t1144 - t1153 * t1154) * z - 0.180D
     #3 * t1161 * t1164) * t68 * t114 / 0.720D3 + (-0.90D2 * t36 * pi * 
     #(t1142 * t1171 - t1180 * t1143 + t1183 * t1154 / 0.2D1) * z + 0.18
     #0D3 * t1161 * pi * (t1144 - t1180 * t1154) * z - t1197 * t1164) * 
     #t68 * t113 / 0.1440D4
      t1204 = FJET(XB1, XB2, s, t1134, 0.0D0, t1136, 0.0D0, 0.0D0, t1203
     #)
      t1206 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1207 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1209 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1221 = t15 * t1209
      t1222 = t12 * t1221
      t1233 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1276 = -(0.90D2 * t36 * pi * (t1206 - t628 * t1207 + t632 * t1209
     # / 0.2D1) - 0.180D3 * t59 * t15 * (t1207 - t628 * t1209) + t1222) 
     #* t68 * t95 / 0.720D3 + (t12 * t15 * (t655 * t1209 - t1207) + 0.90
     #D2 * t36 * pi * (-t660 * t1207 / 0.2D1 + t655 * t1206 - t1233 + t6
     #61 * t1209 / 0.6D1) - t56 * t1221 - 0.180D3 * t59 * t15 * (-t660 *
     # t1209 / 0.2D1 - t1206 + t655 * t1207)) * t95 / 0.720D3 - (0.90D2 
     #* t36 * pi * (t1207 - t687 * t1209) - 0.180D3 * t59 * t1221) * t68
     # * t114 / 0.720D3 + (0.90D2 * t36 * pi * (-t1206 + t700 * t1207 - 
     #t701 * t1209 / 0.2D1) - 0.180D3 * t59 * t15 * (-t1207 + t700 * t12
     #09) - t1222) * t113 * t95 / 0.720D3
      t1277 = FJET(XB1, XB2, s, -t252, t218, 0.0D0, 0.0D0, 0.0D0, t1276)
      t1279 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1280 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1285 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1291 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1296 = t15 * t1280
      t1297 = t56 * t1296
      t1342 = t15 * t1279
      t1345 = t12 * t1296
      t1351 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1431 = -(t12 * t15 * (-t1279 + t538 * t1280) + 0.90D2 * t36 * pi 
     #* (t538 * t1285 - t543 * t1279 / 0.2D1 + t546 * t1280 / 0.6D1 - t1
     #291) - t1297 - 0.180D3 * t59 * t15 * (-t1285 + t538 * t1279 - t543
     # * t1280 / 0.2D1)) * t68 / 0.1440D4 + (0.90D2 * t36 * pi * (-t455 
     #* t1279 + t460 * t1279 + t465 * t1280 / 0.2D1 - t461 * t1280 / 0.2
     #D1) - 0.180D3 * t59 * t15 * (-t455 * t1280 + t460 * t1280)) * t68 
     #* t113 / 0.1440D4 - ((-0.90D2 * t36 * pi * t1279 + 0.180D3 * t59 *
     # t1296) * t498 - 0.90D2 * t36 * pi * t1280 * t504 + (-0.90D2 * t36
     # * pi * t1285 + 0.180D3 * t59 * t1342 - t1345) * t515) * t113 / 0.
     #1440D4 + t36 * pi * t1351 / 0.16D2 + t533 * t1342 / 0.1440D4 + t56
     #8 * t15 * t1285 / 0.1440D4 + t575 * t15 * t1291 / 0.1440D4 - (0.90
     #D2 * t36 * pi * (-t1285 + t353 * t1279 - t354 * t1280 / 0.2D1) - 0
     #.180D3 * t59 * t15 * (t353 * t1280 - t1279) - t1345) * t68 * t95 /
     # 0.720D3 + (t12 * t15 * (t1279 - t379 * t1280) + 0.90D2 * t36 * pi
     # * (t385 * t1279 / 0.2D1 - t386 * t1280 / 0.6D1 + t1291 - t379 * t
     #1285) + t1297 - 0.180D3 * t59 * t15 * (-t379 * t1279 + t385 * t128
     #0 / 0.2D1 + t1285)) * t95 / 0.720D3 - t407 * (t416 * t1280 - t412 
     #* t1280) * t68 * t114 / 0.8D1 + (0.90D2 * t36 * pi * (t427 * t1279
     # - t436 * t1280 / 0.2D1 - t431 * t1279 + t432 * t1280 / 0.2D1) - 0
     #.180D3 * t59 * t15 * (-t431 * t1280 + t427 * t1280)) * t113 * t95 
     #/ 0.720D3 + t594 * t1296 / 0.1440D4
      t1432 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1431)
      t1434 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1436 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1438 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1446 = t1142 * t1436
      t1454 = t1162 * t1438 * z
      t1472 = (-0.90D2 * t36 * pi * (t1142 * t1434 - t1180 * t1436 + t11
     #83 * t1438 / 0.2D1) * z + 0.180D3 * t1161 * pi * (t1446 - t1180 * 
     #t1438) * z - t1197 * t1454) * t68 * t113 / 0.1440D4 - (0.90D2 * t3
     #6 * pi * (t1446 - t1153 * t1438) * z - 0.180D3 * t1161 * t1454) * 
     #t68 * t114 / 0.720D3
      t1473 = FJET(XB1, XB2, s, 0.0D0, t1136, 0.0D0, t1134, 0.0D0, t1472
     #)
      t1475 = t140 * t139 + t216 * t215 - t343 * t339 * t346 / 0.720D3 +
     # t598 * t597 - t618 * t614 * t346 / 0.720D3 + t719 * t718 + t874 *
     # t873 + t926 * t925 + t1081 * t1080 + t1132 * t1131 + t1204 * t120
     #3 + t1277 * t1276 + t1432 * t1431 + t1473 * t1472
      t1476 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1477 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1480 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1491 = t15 * t1477
      t1492 = t12 * t1491
      t1512 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1549 = -(0.90D2 * t36 * pi * (t1476 + t80 * t1477 / 0.2D1 - t78 *
     # t1480) - 0.180D3 * t59 * t15 * (t1480 - t78 * t1477) + t1492) * t
     #68 * t95 / 0.720D3 - (0.90D2 * t36 * pi * (t1480 - t103 * t1477) -
     # 0.180D3 * t59 * t1491) * t68 * t114 / 0.720D3 - (t12 * t15 * (t14
     #80 - t29 * t1477) + 0.90D2 * t36 * pi * (t1512 + t37 * t1480 / 0.2
     #D1 - t29 * t1476 - t40 * t1477 / 0.6D1) + t56 * t1491 - 0.180D3 * 
     #t59 * t15 * (-t29 * t1480 + t1476 + t37 * t1477 / 0.2D1)) * t68 / 
     #0.1440D4 + (0.90D2 * t36 * pi * (t121 * t1480 - t1476 - t123 * t14
     #77 / 0.2D1) - 0.180D3 * t59 * t15 * (-t1480 + t121 * t1477) - t149
     #2) * t68 * t113 / 0.1440D4
      t1550 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t1549)
      t1552 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1553 = t1142 * t1552
      t1554 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1562 = t1162 * t1554 * z
      t1569 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1590 = -(0.90D2 * t36 * pi * (t1553 - t1153 * t1554) * z - 0.180D
     #3 * t1161 * t1562) * t68 * t114 / 0.720D3 + (-0.90D2 * t36 * pi * 
     #(t1142 * t1569 - t1180 * t1552 + t1183 * t1554 / 0.2D1) * z + 0.18
     #0D3 * t1161 * pi * (t1553 - t1180 * t1554) * z - t1197 * t1562) * 
     #t68 * t113 / 0.1440D4
      t1591 = FJET(XB1, XB2, s, t1136, 0.0D0, t1134, 0.0D0, 0.0D0, t1590
     #)
      t1593 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1594 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1597 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1608 = t15 * t1594
      t1624 = -(0.90D2 * t36 * pi * (-t1593 - t1097 * t1594 / 0.2D1 + t1
     #093 * t1597) - 0.180D3 * t59 * t15 * (-t1597 + t1093 * t1594) - t1
     #2 * t1608) * t68 * t95 / 0.720D3 - (0.90D2 * t36 * pi * (-t1597 + 
     #t1119 * t1594) + 0.180D3 * t59 * t1608) * t68 * t114 / 0.720D3
      t1625 = FJET(XB1, XB2, s, t1087, -t1086, -t1084, t1083, 0.0D0, t16
     #24)
      t1627 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1628 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1630 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1642 = t15 * t1630
      t1643 = t12 * t1642
      t1651 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1697 = -(0.90D2 * t36 * pi * (t1627 - t628 * t1628 + t632 * t1630
     # / 0.2D1) - 0.180D3 * t59 * t15 * (t1628 - t628 * t1630) + t1643) 
     #* t68 * t95 / 0.720D3 + (t12 * t15 * (-t1628 + t655 * t1630) + 0.9
     #0D2 * t36 * pi * (-t1651 + t655 * t1627 + t661 * t1630 / 0.6D1 - t
     #660 * t1628 / 0.2D1) - t56 * t1642 - 0.180D3 * t59 * t15 * (t655 *
     # t1628 - t1627 - t660 * t1630 / 0.2D1)) * t95 / 0.720D3 - (0.90D2 
     #* t36 * pi * (t1628 - t687 * t1630) - 0.180D3 * t59 * t1642) * t68
     # * t114 / 0.720D3 + (0.90D2 * t36 * pi * (-t701 * t1630 / 0.2D1 - 
     #t1627 + t700 * t1628) - 0.180D3 * t59 * t15 * (-t1628 + t700 * t16
     #30) - t1643) * t113 * t95 / 0.720D3
      t1698 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t252, t218, 0.0D0, t1697)
      t1700 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t1702 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t1707 = t15 * t1700
      t1716 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t1731 = -(0.90D2 * t36 * pi * (t885 * t1700 - t1702) + 0.180D3 * t
     #59 * t1707) * t68 * t114 / 0.720D3 + (0.90D2 * t36 * pi * (-t905 *
     # t1702 + t906 * t1700 / 0.2D1 + t1716) - 0.180D3 * t59 * t15 * (t1
     #702 - t905 * t1700) + t12 * t1707) * t113 * t95 / 0.720D3
      t1732 = FJET(XB1, XB2, s, 0.0D0, -t880, -t252, -t878, t270, t1731)
      t1734 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1735 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1738 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t1749 = t15 * t1735
      t1765 = -(0.90D2 * t36 * pi * (-t1734 - t1097 * t1735 / 0.2D1 + t1
     #093 * t1738) - 0.180D3 * t59 * t15 * (-t1738 + t1093 * t1735) - t1
     #2 * t1749) * t68 * t95 / 0.720D3 - (0.90D2 * t36 * pi * (-t1738 + 
     #t1119 * t1735) + 0.180D3 * t59 * t1749) * t68 * t114 / 0.720D3
      t1766 = FJET(XB1, XB2, s, -t1086, t1087, t1083, -t1084, 0.0D0, t17
     #65)
      t1768 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t1769 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t1775 = t15 * t1769
      t1781 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t1799 = -(0.90D2 * t36 * pi * (-t1768 + t885 * t1769) + 0.180D3 * 
     #t59 * t1775) * t68 * t114 / 0.720D3 + (0.90D2 * t36 * pi * (t1781 
     #- t905 * t1768 + t906 * t1769 / 0.2D1) - 0.180D3 * t59 * t15 * (t1
     #768 - t905 * t1769) + t12 * t1775) * t113 * t95 / 0.720D3
      t1800 = FJET(XB1, XB2, s, -t252, -t878, 0.0D0, -t880, t270, t1799)
      t1802 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1804 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1810 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1813 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t5, t3, 0.0D0)
      t1819 = t15 * t1802
      t1843 = t12 * t1819
      t1875 = -(t12 * t15 * (-t29 * t1802 + t1804) + 0.90D2 * t36 * pi *
     # (-t40 * t1802 / 0.6D1 + t1810 + t37 * t1804 / 0.2D1 - t29 * t1813
     #) + t56 * t1819 - 0.180D3 * t59 * t15 * (t1813 + t37 * t1802 / 0.2
     #D1 - t29 * t1804)) * t68 / 0.1440D4 - (0.90D2 * t36 * pi * (t80 * 
     #t1802 / 0.2D1 + t1813 - t78 * t1804) - 0.180D3 * t59 * t15 * (-t78
     # * t1802 + t1804) + t1843) * t68 * t95 / 0.720D3 - (0.90D2 * t36 *
     # pi * (t1804 - t103 * t1802) - 0.180D3 * t59 * t1819) * t68 * t114
     # / 0.720D3 + (0.90D2 * t36 * pi * (-t123 * t1802 / 0.2D1 + t121 * 
     #t1804 - t1813) - 0.180D3 * t59 * t15 * (t121 * t1802 - t1804) - t1
     #843) * t68 * t113 / 0.1440D4
      t1876 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t1875)
      t1878 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1880 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1882 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t1136, t1134, 0.0D0)
      t1890 = t1142 * t1880
      t1898 = t1162 * t1882 * z
      t1916 = (-0.90D2 * t36 * pi * (t1142 * t1878 - t1180 * t1880 + t11
     #83 * t1882 / 0.2D1) * z + 0.180D3 * t1161 * pi * (t1890 - t1180 * 
     #t1882) * z - t1197 * t1898) * t68 * t113 / 0.1440D4 - (0.90D2 * t3
     #6 * pi * (t1890 - t1153 * t1882) * z - 0.180D3 * t1161 * t1898) * 
     #t68 * t114 / 0.720D3
      t1917 = FJET(XB1, XB2, s, 0.0D0, t1134, 0.0D0, t1136, 0.0D0, t1916
     #)
      t1919 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1920 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1922 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1934 = t15 * t1922
      t1935 = t12 * t1934
      t1943 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t218, 0.0D0, -
     #t252, 0.0D0, 0.0D0)
      t1989 = -(0.90D2 * t36 * pi * (t1919 - t628 * t1920 + t632 * t1922
     # / 0.2D1) - 0.180D3 * t59 * t15 * (t1920 - t628 * t1922) + t1935) 
     #* t68 * t95 / 0.720D3 + (t12 * t15 * (t655 * t1922 - t1920) + 0.90
     #D2 * t36 * pi * (-t1943 + t661 * t1922 / 0.6D1 + t655 * t1919 - t6
     #60 * t1920 / 0.2D1) - t56 * t1934 - 0.180D3 * t59 * t15 * (-t660 *
     # t1922 / 0.2D1 + t655 * t1920 - t1919)) * t95 / 0.720D3 - (0.90D2 
     #* t36 * pi * (-t687 * t1922 + t1920) - 0.180D3 * t59 * t1934) * t6
     #8 * t114 / 0.720D3 + (0.90D2 * t36 * pi * (-t701 * t1922 / 0.2D1 -
     # t1919 + t700 * t1920) - 0.180D3 * t59 * t15 * (t700 * t1922 - t19
     #20) - t1935) * t113 * t95 / 0.720D3
      t1990 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t218, -t252, 0.0D0, t1989)
      t1992 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -
     #t265, -t256, t270)
      t1994 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -
     #t265, -t256, t270)
      t2006 = 0.90D2 * t36 * pi * (t274 * t1992 - t293 * t273 * t1994) *
     # t330 - 0.180D3 * t334 * t274 * t1994 * t330
      t2010 = FJET(XB1, XB2, s, t261, -t265, -t250, -t256, t270, -t2006 
     #* t68 * t114 / 0.720D3)
      t2014 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -
     #t265, -t256, t270)
      t2016 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t261, -t250, -
     #t265, -t256, t270)
      t2028 = 0.90D2 * t36 * pi * (t274 * t2014 - t293 * t273 * t2016) *
     # t330 - 0.180D3 * t334 * t274 * t2016 * t330
      t2032 = FJET(XB1, XB2, s, -t265, t261, -t256, -t250, t270, -t2028 
     #* t68 * t114 / 0.720D3)
      t2036 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t2038 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t2043 = t15 * t2036
      t2050 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t878, -t880, 
     #-t252, 0.0D0, t270)
      t2067 = -(0.90D2 * t36 * pi * (t885 * t2036 - t2038) + 0.180D3 * t
     #59 * t2043) * t68 * t114 / 0.720D3 + (0.90D2 * t36 * pi * (-t905 *
     # t2038 + t2050 + t906 * t2036 / 0.2D1) - 0.180D3 * t59 * t15 * (-t
     #905 * t2036 + t2038) + t12 * t2043) * t113 * t95 / 0.720D3
      t2068 = FJET(XB1, XB2, s, -t880, 0.0D0, -t878, -t252, t270, t2067)
      t2070 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t2071 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t2074 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t1084, t1087,
     # t1083, -t1086, 0.0D0)
      t2085 = t15 * t2071
      t2101 = -(0.90D2 * t36 * pi * (-t2070 - t1097 * t2071 / 0.2D1 + t1
     #093 * t2074) - 0.180D3 * t59 * t15 * (-t2074 + t1093 * t2071) - t1
     #2 * t2085) * t68 * t95 / 0.720D3 - (0.90D2 * t36 * pi * (-t2074 + 
     #t1119 * t2071) + 0.180D3 * t59 * t2085) * t68 * t114 / 0.720D3
      t2102 = FJET(XB1, XB2, s, -t1084, t1083, t1087, -t1086, 0.0D0, t21
     #01)
      t2104 = t1550 * t1549 + t1591 * t1590 + t1625 * t1624 + t1698 * t1
     #697 + t1732 * t1731 + t1766 * t1765 + t1800 * t1799 + t1876 * t187
     #5 + t1917 * t1916 + t1990 * t1989 - t2010 * t2006 * t346 / 0.720D3
     # - t2032 * t2028 * t346 / 0.720D3 + t2068 * t2067 + t2102 * t2101
      rrqg2qght8s4e1 = t1475 + t2104

      end function



      doubleprecision function rrqg2qght8s4e0
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t5 = t2 * x1
      t6 = 0.1D1 / t1
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = t6 * t8
      t10 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4, 
     #0.0D0, 0.0D0)
      t11 = x1 ** 2
      t12 = x3 * t11
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t1 ** 2
      t20 = t19 ** 2
      t22 = x1 * z
      t23 = -z - x1 + t22
      t24 = 0.1D1 / t23
      t25 = t3 ** 2
      t26 = t24 * t25
      t30 = log(-0.4D1 * t12 * t15 * t18 * t20 * t26)
      t31 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4, 
     #0.0D0, 0.0D0)
      t37 = lh * t6
      t38 = t8 * pi
      t39 = t38 * t31
      t41 = 0.180D3 * t37 * t39
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t9 * pi
      t50 = 0.1D1 / x2
      t51 = t50 * t45
      t55 = x2 * t11
      t56 = t55 * t20
      t61 = log(-0.4D1 * t56 * t15 * t18 * t26)
      t71 = t11 * t15
      t77 = log(-0.4D1 * t71 * t18 * t20 * t24 * t25)
      t78 = t77 ** 2
      t81 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4, 
     #0.0D0, 0.0D0)
      t92 = lh ** 2
      t93 = 0.180D3 * t92
      t94 = pi ** 2
      t95 = 0.30D2 * t94
      t96 = t93 - t95
      t97 = t96 * t6
      t102 = -(0.90D2 * t9 * pi * (t10 - t30 * t31) - t41) * t43 * t45 /
     # 0.720D3 - t48 * t31 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi * (-t
     #10 + t61 * t31) + t41) * t50 * t45 / 0.720D3 + (0.90D2 * t9 * pi *
     # (-t78 * t31 / 0.2D1 - t81 + t77 * t10) - 0.180D3 * t37 * t38 * (t
     #77 * t31 - t10) - t97 * t39) * t45 / 0.720D3
      t103 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = t2 * x3
      t106 = -0.1D1 + x3
      t107 = t2 * t106
      t108 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t107, t105, 0.0D0)
      t111 = 0.1D1 / t17 / z
      t112 = t15 * t111
      t116 = log(-0.4D1 * t12 * t20 * t112 * t106)
      t117 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t107, t105, 0.0D0)
      t123 = t38 * t117
      t125 = 0.180D3 * t37 * t123
      t134 = x3 * t111
      t135 = t20 * t15
      t136 = t135 * t106
      t139 = log(-0.4D1 * t134 * t136)
      t140 = t139 ** 2
      t143 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t107, t105, 0.0D0)
      t158 = x2 * x3
      t159 = t158 * t111
      t162 = log(-0.4D1 * t159 * t136)
      t172 = -(0.90D2 * t9 * pi * (t108 - t116 * t117) - t125) * t43 * t
     #45 / 0.720D3 - t48 * t117 * t43 * t51 / 0.8D1 - (0.90D2 * t9 * pi 
     #* (t140 * t117 / 0.2D1 + t143 - t139 * t108) - 0.180D3 * t37 * t38
     # * (-t139 * t117 + t108) + t97 * t123) * t43 / 0.1440D4 + (0.90D2 
     #* t9 * pi * (t162 * t117 - t108) + t125) * t43 * t50 / 0.1440D4
      t173 = FJET(XB1, XB2, s, 0.0D0, t105, 0.0D0, -t107, 0.0D0, t172)
      t175 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t176 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t182 = t38 * t176
      t184 = 0.180D3 * t37 * t182
      t205 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t219 = -(0.90D2 * t9 * pi * (t175 - t30 * t176) - t184) * t43 * t4
     #5 / 0.720D3 - t48 * t176 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi *
     # (t61 * t176 - t175) + t184) * t50 * t45 / 0.720D3 + (0.90D2 * t9 
     #* pi * (-t78 * t176 / 0.2D1 + t77 * t175 - t205) - 0.180D3 * t37 *
     # t38 * (t77 * t176 - t175) - t97 * t182) * t45 / 0.720D3
      t220 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t219)
      t222 = t112 * t20
      t225 = log(0.4D1 * t12 * t222)
      t226 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t228 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t233 = t38 * t226
      t235 = 0.180D3 * t37 * t233
      t240 = -0.1D1 + x2
      t241 = t240 ** 2
      t245 = log(0.4D1 * t56 * t112 * t241)
      t249 = log(0.4D1 * t55 * t222)
      t256 = t111 * t20
      t259 = log(0.4D1 * t71 * t256)
      t261 = t259 ** 2
      t264 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t274 = t97 * t233
      t279 = log(0.4D1 * t222)
      t282 = t279 ** 2
      t285 = (0.180D3 * t279 * lh + 0.45D2 * t282 + t93 - t95) * t6
      t289 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t295 = log(0.4D1 * t134 * t135)
      t297 = t295 ** 2
      t315 = (-0.180D3 * lh - 0.90D2 * t279) * t6
      t330 = (-0.90D2 * t282 * lh + 0.60D2 * lh * t94 - 0.240D3 * zeta3 
     #- 0.120D3 * t92 * lh - 0.15D2 * t282 * t279 - t279 * t96) * t6
      t335 = log(0.4D1 * t158 * t222)
      t337 = t135 * t241
      t340 = log(0.4D1 * t159 * t337)
      t348 = x2 * t111
      t351 = log(0.4D1 * t348 * t337)
      t352 = t351 ** 2
      t355 = log(0.4D1 * t348 * t135)
      t356 = t355 ** 2
      t358 = -t352 / 0.2D1 + t356 / 0.2D1
      t366 = t351 - t355
      t371 = -(0.90D2 * t9 * pi * (t225 * t226 - t228) + t235) * t43 * t
     #45 / 0.720D3 + t48 * (t245 * t226 - t249 * t226) * t50 * t45 / 0.8
     #D1 + (0.90D2 * t9 * pi * (-t259 * t228 + t261 * t226 / 0.2D1 + t26
     #4) - 0.180D3 * t37 * t38 * (t228 - t259 * t226) + t274) * t45 / 0.
     #720D3 + t285 * t38 * t228 / 0.1440D4 + t9 * pi * t289 / 0.16D2 - (
     #0.90D2 * t9 * pi * (t295 * t228 - t297 * t226 / 0.2D1 - t264) - 0.
     #180D3 * t37 * t38 * (-t228 + t295 * t226) - t274) * t43 / 0.1440D4
     # + t315 * t38 * t264 / 0.1440D4 + t330 * t233 / 0.1440D4 + t48 * (
     #-t335 * t226 + t340 * t226) * t43 * t50 / 0.16D2 - (-0.90D2 * t9 *
     # pi * t226 * t358 + (-0.90D2 * t9 * pi * t228 + t235) * t366) * t5
     #0 / 0.1440D4
      t372 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t371)
      t374 = t106 * s
      t375 = t1 * x1
      t376 = t374 * t375
      t377 = t1 * t3
      t378 = t374 * t377
      t379 = x3 * x1
      t380 = t2 * t379
      t382 = x3 * s * t377
      t383 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t3
     #78, -t382, 0.0D0)
      t385 = t18 * t24
      t390 = log(0.4D1 * t12 * t135 * t385 * t25 * t106)
      t391 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t3
     #78, -t382, 0.0D0)
      t408 = -(0.90D2 * t9 * pi * (-t383 + t390 * t391) + 0.180D3 * t37 
     #* t38 * t391) * t43 * t45 / 0.720D3 + t48 * t391 * t43 * t51 / 0.8
     #D1
      t409 = FJET(XB1, XB2, s, -t376, t378, t380, -t382, 0.0D0, t408)
      t411 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t107, t105, 0.0D0)
      t412 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t107, t105, 0.0D0)
      t418 = t38 * t412
      t420 = 0.180D3 * t37 * t418
      t430 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t107, t105, 0.0D0)
      t455 = -(0.90D2 * t9 * pi * (t411 - t116 * t412) - t420) * t43 * t
     #45 / 0.720D3 - t48 * t412 * t43 * t51 / 0.8D1 - (0.90D2 * t9 * pi 
     #* (-t139 * t411 + t430 + t140 * t412 / 0.2D1) - 0.180D3 * t37 * t3
     #8 * (t411 - t139 * t412) + t97 * t418) * t43 / 0.1440D4 + (0.90D2 
     #* t9 * pi * (-t411 + t162 * t412) + t420) * t43 * t50 / 0.1440D4
      t456 = FJET(XB1, XB2, s, -t107, 0.0D0, t105, 0.0D0, 0.0D0, t455)
      t459 = t240 * s * t375
      t460 = x2 * x1
      t462 = t2 * t460 * t24
      t467 = s * t19 * x2 * x1 * t3 * t24
      t468 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, -
     #t4, 0.0D0, t467)
      t473 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, -
     #t4, 0.0D0, t467)
      t479 = log(-0.4D1 * t55 * t135 * t385 * t25 * t241)
      t492 = t48 * t468 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi * (t473 
     #- t479 * t468) - 0.180D3 * t37 * t38 * t468) * t50 * t45 / 0.720D3
      t493 = FJET(XB1, XB2, s, -t459, -t4, -t462, 0.0D0, t467, t492)
      t495 = cos(t13)
      t497 = x2 * t106
      t499 = Sqrt(x3 * t23 * t497)
      t500 = t495 * t499
      t501 = 0.2D1 * t500
      t504 = t379 * z
      t505 = t158 * z
      t507 = t158 * x1
      t509 = x2 ** 2
      t510 = t509 * x3
      t511 = t510 * x1
      t513 = x3 * z
      t516 = t510 * t22
      t517 = -t501 + 0.2D1 * t500 * x2 + t504 + 0.2D1 * t505 + 0.2D1 * t
     #507 - t511 - t510 * z + t158 - t513 - t379 - x2 - 0.2D1 * t158 * t
     #22 + t516
      t519 = -0.1D1 + t158
      t520 = 0.1D1 / t519
      t522 = t5 * t517 * t24 * t520
      t524 = x3 * t240 * t520
      t525 = t4 * t524
      t530 = t5 * t240 * (-t158 - z + t513 - x1 + t379 + t22 - t504 + t5
     #01) * t24 * t520
      t532 = t374 * t377 * t520
      t534 = t460 * z
      t535 = z + t534 - t22 - t460 + x1
      t537 = t9 * pi * t23 * t535
      t538 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t530, -t522, -t
     #532, -t525, t467)
      t542 = t11 * t17
      t544 = t17 * x2
      t549 = -0.2D1 * t55 * z + t534 - t505 - t507 - t12 * x2 + t511 + t
     #542 * x2 - t544 * x1 + t55 - t17 - 0.2D1 * t22 - 0.2D1 * t500 * t4
     #60
      t568 = -0.2D1 * t500 * t22 + x3 * t17 * t460 + 0.2D1 * t12 * x2 * 
     #z - t12 * t544 - t516 + 0.2D1 * t500 * t534 + 0.2D1 * t500 * z + 0
     #.2D1 * t500 * x1 - t11 + 0.2D1 * x1 * t17 + 0.2D1 * t11 * z - t542
      t570 = 0.1D1 / (t549 + t568)
      t573 = t43 * t50 * t45
      t577 = FJET(XB1, XB2, s, -t522, -t525, t530, -t532, t467, -t537 * 
     #t538 * t570 * t573 / 0.8D1)
      t579 = t38 * t23
      t586 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, -
     #t4, 0.0D0, t467)
      t592 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, -
     #t4, 0.0D0, t467)
      t604 = t48 * t586 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi * (-t479
     # * t586 + t592) - 0.180D3 * t37 * t38 * t586) * t50 * t45 / 0.720D
     #3
      t605 = FJET(XB1, XB2, s, -t462, 0.0D0, -t459, -t4, t467, t604)
      t607 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t609 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t614 = t38 * t607
      t616 = 0.180D3 * t37 * t614
      t631 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t641 = t97 * t614
      t645 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t691 = -(0.90D2 * t9 * pi * (t225 * t607 - t609) + t616) * t43 * t
     #45 / 0.720D3 + t48 * (t245 * t607 - t249 * t607) * t50 * t45 / 0.8
     #D1 + (0.90D2 * t9 * pi * (t261 * t607 / 0.2D1 - t259 * t609 + t631
     #) - 0.180D3 * t37 * t38 * (t609 - t259 * t607) + t641) * t45 / 0.7
     #20D3 + t9 * pi * t645 / 0.16D2 + t330 * t614 / 0.1440D4 - (0.90D2 
     #* t9 * pi * (-t297 * t607 / 0.2D1 - t631 + t295 * t609) - 0.180D3 
     #* t37 * t38 * (-t609 + t295 * t607) - t641) * t43 / 0.1440D4 + t28
     #5 * t38 * t609 / 0.1440D4 + t315 * t38 * t631 / 0.1440D4 + t48 * (
     #-t335 * t607 + t340 * t607) * t43 * t50 / 0.16D2 - (-0.90D2 * t9 *
     # pi * t607 * t358 + (-0.90D2 * t9 * pi * t609 + t616) * t366) * t5
     #0 / 0.1440D4
      t692 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t691)
      t694 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t696 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t701 = t38 * t694
      t703 = 0.180D3 * t37 * t701
      t723 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t738 = -(0.90D2 * t9 * pi * (-t30 * t694 + t696) - t703) * t43 * t
     #45 / 0.720D3 - t48 * t694 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi 
     #* (t61 * t694 - t696) + t703) * t50 * t45 / 0.720D3 + (0.90D2 * t9
     # * pi * (-t78 * t694 / 0.2D1 - t723 + t77 * t696) - 0.180D3 * t37 
     #* t38 * (-t696 + t77 * t694) - t97 * t701) * t45 / 0.720D3
      t739 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t738)
      t741 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t742 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t748 = t38 * t742
      t750 = 0.180D3 * t37 * t748
      t769 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t785 = -(0.90D2 * t9 * pi * (t741 - t30 * t742) - t750) * t43 * t4
     #5 / 0.720D3 - t48 * t742 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi *
     # (-t741 + t61 * t742) + t750) * t50 * t45 / 0.720D3 + (0.90D2 * t9
     # * pi * (t77 * t741 - t769 - t78 * t742 / 0.2D1) - 0.180D3 * t37 *
     # t38 * (-t741 + t77 * t742) - t97 * t748) * t45 / 0.720D3
      t786 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t785)
      t788 = t2 * t524
      t790 = t2 * t106 * t520
      t792 = Sqrt(-t513 * t497)
      t796 = 0.1D1 / (-z - t158 + 0.2D1 * t495 * t792)
      t797 = pi * t796
      t798 = t9 * t797
      t799 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #790, t788, 0.0D0)
      t800 = t799 * z
      t804 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #790, t788, 0.0D0)
      t808 = t519 ** 2
      t814 = log(-0.4D1 * t158 * t256 * t15 * t241 * t106 / t808)
      t815 = t814 * t796
      t822 = t37 * t8
      t830 = -t798 * t800 * t573 / 0.8D1 + (-0.90D2 * t9 * pi * (t796 * 
     #t804 - t815 * t799) * z + 0.180D3 * t822 * t797 * t800) * t43 * t5
     #0 / 0.1440D4
      t831 = FJET(XB1, XB2, s, 0.0D0, t788, 0.0D0, t790, 0.0D0, t830)
      t833 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, -
     #t4, 0.0D0, t467)
      t838 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, -
     #t4, 0.0D0, t467)
      t851 = t48 * t833 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi * (t838 
     #- t479 * t833) - 0.180D3 * t37 * t38 * t833) * t50 * t45 / 0.720D3
      t852 = FJET(XB1, XB2, s, 0.0D0, -t462, -t4, -t459, t467, t851)
      t854 = t103 * t102 + t173 * t172 + t220 * t219 + t372 * t371 + t40
     #9 * t408 + t456 * t455 + t493 * t492 - t577 * t6 * t579 * t535 * t
     #538 * t570 * t573 / 0.8D1 + t605 * t604 + t692 * t691 + t739 * t73
     #8 + t786 * t785 + t831 * t830 + t852 * t851
      t855 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t857 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t862 = t38 * t855
      t864 = 0.180D3 * t37 * t862
      t879 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t889 = t97 * t862
      t911 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t939 = -(0.90D2 * t9 * pi * (t225 * t855 - t857) + t864) * t43 * t
     #45 / 0.720D3 + t48 * (-t249 * t855 + t245 * t855) * t50 * t45 / 0.
     #8D1 + (0.90D2 * t9 * pi * (-t259 * t857 + t261 * t855 / 0.2D1 + t8
     #79) - 0.180D3 * t37 * t38 * (t857 - t259 * t855) + t889) * t45 / 0
     #.720D3 - (0.90D2 * t9 * pi * (-t879 + t295 * t857 - t297 * t855 / 
     #0.2D1) - 0.180D3 * t37 * t38 * (-t857 + t295 * t855) - t889) * t43
     # / 0.1440D4 + t285 * t38 * t857 / 0.1440D4 + t9 * pi * t911 / 0.16
     #D2 + t330 * t862 / 0.1440D4 + t315 * t38 * t879 / 0.1440D4 + t48 *
     # (-t335 * t855 + t340 * t855) * t43 * t50 / 0.16D2 - (-0.90D2 * t9
     # * pi * t855 * t358 + (-0.90D2 * t9 * pi * t857 + t864) * t366) * 
     #t50 / 0.1440D4
      t940 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t939)
      t942 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #790, t788, 0.0D0)
      t943 = t942 * z
      t947 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #790, t788, 0.0D0)
      t962 = -t798 * t943 * t573 / 0.8D1 + (-0.90D2 * t9 * pi * (t796 * 
     #t947 - t815 * t942) * z + 0.180D3 * t822 * t797 * t943) * t43 * t5
     #0 / 0.1440D4
      t963 = FJET(XB1, XB2, s, t788, 0.0D0, t790, 0.0D0, 0.0D0, t962)
      t965 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t530, -t522, -t
     #532, -t525, t467)
      t970 = FJET(XB1, XB2, s, t530, -t532, -t522, -t525, t467, -t537 * 
     #t965 * t570 * t573 / 0.8D1)
      t978 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t530, -t522, -t
     #532, -t525, t467)
      t983 = FJET(XB1, XB2, s, -t525, -t522, -t532, t530, t467, -t537 * 
     #t978 * t570 * t573 / 0.8D1)
      t991 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t993 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t998 = t38 * t991
      t1000 = 0.180D3 * t37 * t998
      t1013 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1025 = t97 * t998
      t1047 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1075 = -(0.90D2 * t9 * pi * (t225 * t991 - t993) + t1000) * t43 *
     # t45 / 0.720D3 + t48 * (-t249 * t991 + t245 * t991) * t50 * t45 / 
     #0.8D1 + (0.90D2 * t9 * pi * (-t259 * t993 + t1013 + t261 * t991 / 
     #0.2D1) - 0.180D3 * t37 * t38 * (t993 - t259 * t991) + t1025) * t45
     # / 0.720D3 - (0.90D2 * t9 * pi * (-t1013 + t295 * t993 - t297 * t9
     #91 / 0.2D1) - 0.180D3 * t37 * t38 * (-t993 + t295 * t991) - t1025)
     # * t43 / 0.1440D4 + t285 * t38 * t993 / 0.1440D4 + t9 * pi * t1047
     # / 0.16D2 + t330 * t998 / 0.1440D4 + t315 * t38 * t1013 / 0.1440D4
     # + t48 * (-t335 * t991 + t340 * t991) * t43 * t50 / 0.16D2 - (-0.9
     #0D2 * t9 * pi * t991 * t358 + (-0.90D2 * t9 * pi * t993 + t1000) *
     # t366) * t50 / 0.1440D4
      t1076 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1075)
      t1078 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t530, -t522, -
     #t532, -t525, t467)
      t1083 = FJET(XB1, XB2, s, -t532, t530, -t525, -t522, t467, -t537 *
     # t1078 * t570 * t573 / 0.8D1)
      t1091 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t107, t105, 0.0D0)
      t1092 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t107, t105, 0.0D0)
      t1098 = t38 * t1092
      t1100 = 0.180D3 * t37 * t1098
      t1109 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t107, t105, 0.0D0)
      t1135 = -(0.90D2 * t9 * pi * (t1091 - t116 * t1092) - t1100) * t43
     # * t45 / 0.720D3 - t48 * t1092 * t43 * t51 / 0.8D1 - (0.90D2 * t9 
     #* pi * (t1109 + t140 * t1092 / 0.2D1 - t139 * t1091) - 0.180D3 * t
     #37 * t38 * (t1091 - t139 * t1092) + t97 * t1098) * t43 / 0.1440D4 
     #+ (0.90D2 * t9 * pi * (-t1091 + t162 * t1092) + t1100) * t43 * t50
     # / 0.1440D4
      t1136 = FJET(XB1, XB2, s, t105, 0.0D0, -t107, 0.0D0, 0.0D0, t1135)
      t1138 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t
     #378, -t382, 0.0D0)
      t1139 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t
     #378, -t382, 0.0D0)
      t1156 = -(0.90D2 * t9 * pi * (-t1138 + t390 * t1139) + 0.180D3 * t
     #37 * t38 * t1139) * t43 * t45 / 0.720D3 + t48 * t1139 * t43 * t51 
     #/ 0.8D1
      t1157 = FJET(XB1, XB2, s, t380, -t382, -t376, t378, 0.0D0, t1156)
      t1159 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t107, t105, 0.0D0)
      t1161 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t107, t105, 0.0D0)
      t1166 = t38 * t1159
      t1168 = 0.180D3 * t37 * t1166
      t1177 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t107, t105, 0.0D0)
      t1203 = -(0.90D2 * t9 * pi * (-t116 * t1159 + t1161) - t1168) * t4
     #3 * t45 / 0.720D3 - t48 * t1159 * t43 * t51 / 0.8D1 - (0.90D2 * t9
     # * pi * (t1177 + t140 * t1159 / 0.2D1 - t139 * t1161) - 0.180D3 * 
     #t37 * t38 * (-t139 * t1159 + t1161) + t97 * t1166) * t43 / 0.1440D
     #4 + (0.90D2 * t9 * pi * (t162 * t1159 - t1161) + t1168) * t43 * t5
     #0 / 0.1440D4
      t1204 = FJET(XB1, XB2, s, 0.0D0, -t107, 0.0D0, t105, 0.0D0, t1203)
      t1206 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t790, t788, 0.0D0)
      t1207 = t1206 * z
      t1211 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t790, t788, 0.0D0)
      t1226 = -t798 * t1207 * t573 / 0.8D1 + (-0.90D2 * t9 * pi * (t796 
     #* t1211 - t815 * t1206) * z + 0.180D3 * t822 * t797 * t1207) * t43
     # * t50 / 0.1440D4
      t1227 = FJET(XB1, XB2, s, t790, 0.0D0, t788, 0.0D0, 0.0D0, t1226)
      t1229 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, 
     #-t4, 0.0D0, t467)
      t1234 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t459, -t462, 
     #-t4, 0.0D0, t467)
      t1247 = t48 * t1229 * t43 * t51 / 0.8D1 + (0.90D2 * t9 * pi * (t12
     #34 - t479 * t1229) - 0.180D3 * t37 * t38 * t1229) * t50 * t45 / 0.
     #720D3
      t1248 = FJET(XB1, XB2, s, -t4, -t459, 0.0D0, -t462, t467, t1247)
      t1250 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t
     #378, -t382, 0.0D0)
      t1252 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t
     #378, -t382, 0.0D0)
      t1268 = -(0.90D2 * t9 * pi * (t390 * t1250 - t1252) + 0.180D3 * t3
     #7 * t38 * t1250) * t43 * t45 / 0.720D3 + t48 * t1250 * t43 * t51 /
     # 0.8D1
      t1269 = FJET(XB1, XB2, s, t378, -t376, -t382, t380, 0.0D0, t1268)
      t1271 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t
     #378, -t382, 0.0D0)
      t1272 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t376, t380, t
     #378, -t382, 0.0D0)
      t1289 = -(0.90D2 * t9 * pi * (-t1271 + t390 * t1272) + 0.180D3 * t
     #37 * t38 * t1272) * t43 * t45 / 0.720D3 + t48 * t1272 * t43 * t51 
     #/ 0.8D1
      t1290 = FJET(XB1, XB2, s, -t382, t380, t378, -t376, 0.0D0, t1289)
      t1292 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t790, t788, 0.0D0)
      t1293 = t1292 * z
      t1297 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t790, t788, 0.0D0)
      t1312 = -t798 * t1293 * t573 / 0.8D1 + (-0.90D2 * t9 * pi * (t796 
     #* t1297 - t815 * t1292) * z + 0.180D3 * t822 * t797 * t1293) * t43
     # * t50 / 0.1440D4
      t1313 = FJET(XB1, XB2, s, 0.0D0, t790, 0.0D0, t788, 0.0D0, t1312)
      t1315 = t940 * t939 + t963 * t962 - t970 * t6 * t579 * t535 * t965
     # * t570 * t573 / 0.8D1 - t983 * t6 * t579 * t535 * t978 * t570 * t
     #573 / 0.8D1 + t1076 * t1075 - t1083 * t6 * t579 * t535 * t1078 * t
     #570 * t573 / 0.8D1 + t1136 * t1135 + t1157 * t1156 + t1204 * t1203
     # + t1227 * t1226 + t1248 * t1247 + t1269 * t1268 + t1290 * t1289 +
     # t1313 * t1312
      rrqg2qght8s4e0 = t854 + t1315

      end function



      doubleprecision function rrqg2qght8s4em1
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
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
      t21 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t27 = lh * t3
      t28 = t5 * pi
      t29 = t28 * t21
      t31 = 0.180D3 * t27 * t29
      t33 = 0.1D1 / x3
      t36 = x1 ** 2
      t37 = t36 * t14
      t41 = log(0.4D1 * t37 * t10 * t16)
      t48 = 0.1D1 / x1
      t51 = t6 * pi
      t56 = x2 * t10
      t57 = -0.1D1 + x2
      t58 = t57 ** 2
      t62 = log(0.4D1 * t56 * t17 * t58)
      t65 = log(0.4D1 * t56 * t17)
      t66 = t62 - t65
      t68 = 0.1D1 / x2
      t72 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t80 = log(0.4D1 * t10 * t14 * t16)
      t83 = (-0.180D3 * lh - 0.90D2 * t80) * t3
      t89 = t80 ** 2
      t91 = lh ** 2
      t93 = pi ** 2
      t96 = (0.180D3 * t80 * lh + 0.45D2 * t89 + 0.180D3 * t91 - 0.30D2 
     #* t93) * t3
      t99 = -(0.90D2 * t6 * pi * (-t7 + t20 * t21) + t31) * t33 / 0.1440
     #D4 + (0.90D2 * t6 * pi * (t7 - t41 * t21) - t31) * t48 / 0.720D3 +
     # t51 * t21 * t33 * t48 / 0.8D1 + t51 * t21 * t66 * t68 / 0.16D2 + 
     #t6 * pi * t72 / 0.16D2 + t83 * t28 * t7 / 0.1440D4 + t96 * t29 / 0
     #.1440D4
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t99)
      t102 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t103 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t109 = t28 * t103
      t111 = 0.180D3 * t27 * t109
      t131 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t140 = -(0.90D2 * t6 * pi * (-t102 + t20 * t103) + t111) * t33 / 0
     #.1440D4 + (0.90D2 * t6 * pi * (t102 - t41 * t103) - t111) * t48 / 
     #0.720D3 + t51 * t103 * t33 * t48 / 0.8D1 + t51 * t103 * t66 * t68 
     #/ 0.16D2 + t6 * pi * t131 / 0.16D2 + t83 * t28 * t102 / 0.1440D4 +
     # t96 * t109 / 0.1440D4
      t141 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t140)
      t143 = t2 * x1
      t144 = -0.1D1 + x1
      t145 = t2 * t144
      t146 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t155 = 0.1D1 / (-z - x1 + x1 * z)
      t157 = t144 ** 2
      t161 = log(-0.4D1 * t37 / t8 * t16 * t155 * t157)
      t163 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t178 = -t51 * t146 * t68 * t48 / 0.8D1 + (0.90D2 * t6 * pi * (t161
     # * t146 - t163) + 0.180D3 * t27 * t28 * t146) * t48 / 0.720D3 - t5
     #1 * t146 * t33 * t48 / 0.8D1
      t179 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t143, -t145, 0.0D0, t178)
      t181 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t186 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t202 = -t51 * t181 * t68 * t48 / 0.8D1 + (0.90D2 * t6 * pi * (-t18
     #6 + t161 * t181) + 0.180D3 * t27 * t28 * t181) * t48 / 0.720D3 - t
     #51 * t181 * t33 * t48 / 0.8D1
      t203 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t145, t143, 0.0D0, t202)
      t205 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t206 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t212 = t28 * t206
      t214 = 0.180D3 * t27 * t212
      t239 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t243 = -(0.90D2 * t6 * pi * (-t205 + t20 * t206) + t214) * t33 / 0
     #.1440D4 + (0.90D2 * t6 * pi * (t205 - t41 * t206) - t214) * t48 / 
     #0.720D3 + t51 * t206 * t33 * t48 / 0.8D1 + t83 * t28 * t205 / 0.14
     #40D4 + t96 * t212 / 0.1440D4 + t51 * t206 * t66 * t68 / 0.16D2 + t
     #6 * pi * t239 / 0.16D2
      t244 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t243)
      t246 = t2 * x3
      t247 = -0.1D1 + x3
      t248 = t2 * t247
      t249 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t250 = t249 * t33
      t257 = log(-0.4D1 * t11 * t17 * t247)
      t259 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t273 = -t51 * t250 * t48 / 0.8D1 - (0.90D2 * t6 * pi * (-t257 * t2
     #49 + t259) - 0.180D3 * t27 * t28 * t249) * t33 / 0.1440D4 - t51 * 
     #t250 * t68 / 0.16D2
      t274 = FJET(XB1, XB2, s, 0.0D0, t246, 0.0D0, -t248, 0.0D0, t273)
      t276 = x2 * x3
      t278 = 0.1D1 / (-0.1D1 + t276)
      t280 = t2 * t247 * t278
      t283 = t2 * x3 * t57 * t278
      t284 = cos(t12)
      t288 = Sqrt(-x3 * z * x2 * t247)
      t292 = 0.1D1 / (-z - t276 + 0.2D1 * t284 * t288)
      t294 = t6 * pi * t292
      t295 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #280, t283, 0.0D0)
      t297 = t33 * t68
      t301 = FJET(XB1, XB2, s, 0.0D0, t280, 0.0D0, t283, 0.0D0, -t294 * 
     #t295 * z * t297 / 0.16D2)
      t306 = z * t33 * t68
      t310 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #280, t283, 0.0D0)
      t315 = FJET(XB1, XB2, s, 0.0D0, t283, 0.0D0, t280, 0.0D0, -t294 * 
     #t310 * z * t297 / 0.16D2)
      t322 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t324 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t335 = t322 * t33
      t342 = -(0.90D2 * t6 * pi * (-t257 * t322 + t324) - 0.180D3 * t27 
     #* t28 * t322) * t33 / 0.1440D4 - t51 * t335 * t68 / 0.16D2 - t51 *
     # t335 * t48 / 0.8D1
      t343 = FJET(XB1, XB2, s, 0.0D0, -t248, 0.0D0, t246, 0.0D0, t342)
      t347 = t2 * x1 * x2 * t155
      t349 = t1 * x1
      t350 = t57 * s * t349
      t355 = s * t15 * x2 * x1 * t144 * t155
      t356 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t350, -t347, -
     #t145, 0.0D0, t355)
      t361 = FJET(XB1, XB2, s, 0.0D0, -t347, -t145, -t350, t355, t51 * t
     #356 * t68 * t48 / 0.8D1)
      t365 = t68 * t48
      t369 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t370 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t376 = t28 * t370
      t378 = 0.180D3 * t27 * t376
      t397 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t407 = -(0.90D2 * t6 * pi * (-t369 + t20 * t370) + t378) * t33 / 0
     #.1440D4 + t83 * t28 * t369 / 0.1440D4 + (0.90D2 * t6 * pi * (t369 
     #- t41 * t370) - t378) * t48 / 0.720D3 + t51 * t370 * t33 * t48 / 0
     #.8D1 + t6 * pi * t397 / 0.16D2 + t96 * t376 / 0.1440D4 + t51 * t37
     #0 * t66 * t68 / 0.16D2
      t408 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t407)
      t410 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t415 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t431 = -t51 * t410 * t68 * t48 / 0.8D1 + (0.90D2 * t6 * pi * (-t41
     #5 + t161 * t410) + 0.180D3 * t27 * t28 * t410) * t48 / 0.720D3 - t
     #51 * t410 * t33 * t48 / 0.8D1
      t432 = FJET(XB1, XB2, s, t143, -t145, 0.0D0, 0.0D0, 0.0D0, t431)
      t434 = t100 * t99 + t141 * t140 + t179 * t178 + t203 * t202 + t244
     # * t243 + t274 * t273 - t301 * t3 * t28 * t292 * t295 * t306 / 0.1
     #6D2 - t315 * t3 * t28 * t292 * t310 * t306 / 0.16D2 + t343 * t342 
     #+ t361 * t3 * t5 * pi * t356 * t365 / 0.8D1 + t408 * t407 + t432 *
     # t431
      t435 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t436 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t448 = t436 * t33
      t455 = -(0.90D2 * t6 * pi * (t435 - t257 * t436) - 0.180D3 * t27 *
     # t28 * t436) * t33 / 0.1440D4 - t51 * t448 * t68 / 0.16D2 - t51 * 
     #t448 * t48 / 0.8D1
      t456 = FJET(XB1, XB2, s, t246, 0.0D0, -t248, 0.0D0, 0.0D0, t455)
      t459 = t2 * x1 * x3
      t461 = t1 * t144
      t462 = x3 * s * t461
      t463 = t247 * s
      t464 = t463 * t349
      t465 = t463 * t461
      t466 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t464, t459, t4
     #65, -t462, 0.0D0)
      t471 = FJET(XB1, XB2, s, t459, -t462, -t464, t465, 0.0D0, t51 * t4
     #66 * t33 * t48 / 0.8D1)
      t475 = t33 * t48
      t479 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #280, t283, 0.0D0)
      t484 = FJET(XB1, XB2, s, t280, 0.0D0, t283, 0.0D0, 0.0D0, -t294 * 
     #t479 * z * t297 / 0.16D2)
      t491 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t464, t459, t4
     #65, -t462, 0.0D0)
      t496 = FJET(XB1, XB2, s, t465, -t464, -t462, t459, 0.0D0, t51 * t4
     #91 * t33 * t48 / 0.8D1)
      t503 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #280, t283, 0.0D0)
      t508 = FJET(XB1, XB2, s, t283, 0.0D0, t280, 0.0D0, 0.0D0, -t294 * 
     #t503 * z * t297 / 0.16D2)
      t515 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t521 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t143, 0.0D0, -t
     #145, 0.0D0, 0.0D0)
      t536 = -t51 * t515 * t68 * t48 / 0.8D1 + (0.90D2 * t6 * pi * (t161
     # * t515 - t521) + 0.180D3 * t27 * t28 * t515) * t48 / 0.720D3 - t5
     #1 * t515 * t33 * t48 / 0.8D1
      t537 = FJET(XB1, XB2, s, -t145, t143, 0.0D0, 0.0D0, 0.0D0, t536)
      t539 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t350, -t347, -
     #t145, 0.0D0, t355)
      t544 = FJET(XB1, XB2, s, -t145, -t350, 0.0D0, -t347, t355, t51 * t
     #539 * t68 * t48 / 0.8D1)
      t551 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t552 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t564 = t552 * t33
      t571 = -(0.90D2 * t6 * pi * (t551 - t257 * t552) - 0.180D3 * t27 *
     # t28 * t552) * t33 / 0.1440D4 - t51 * t564 * t68 / 0.16D2 - t51 * 
     #t564 * t48 / 0.8D1
      t572 = FJET(XB1, XB2, s, -t248, 0.0D0, t246, 0.0D0, 0.0D0, t571)
      t574 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t464, t459, t4
     #65, -t462, 0.0D0)
      t579 = FJET(XB1, XB2, s, -t462, t459, t465, -t464, 0.0D0, t51 * t5
     #74 * t33 * t48 / 0.8D1)
      t586 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t350, -t347, -
     #t145, 0.0D0, t355)
      t591 = FJET(XB1, XB2, s, -t350, -t145, -t347, 0.0D0, t355, t51 * t
     #586 * t68 * t48 / 0.8D1)
      t598 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t464, t459, t4
     #65, -t462, 0.0D0)
      t603 = FJET(XB1, XB2, s, -t464, t465, t459, -t462, 0.0D0, t51 * t5
     #98 * t33 * t48 / 0.8D1)
      t610 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t350, -t347, -
     #t145, 0.0D0, t355)
      t615 = FJET(XB1, XB2, s, -t347, 0.0D0, -t350, -t145, t355, t51 * t
     #610 * t68 * t48 / 0.8D1)
      t622 = t456 * t455 + t471 * t3 * t5 * pi * t466 * t475 / 0.8D1 - t
     #484 * t3 * t28 * t292 * t479 * t306 / 0.16D2 + t496 * t3 * t5 * pi
     # * t491 * t475 / 0.8D1 - t508 * t3 * t28 * t292 * t503 * t306 / 0.
     #16D2 + t537 * t536 + t544 * t3 * t5 * pi * t539 * t365 / 0.8D1 + t
     #572 * t571 + t579 * t3 * t5 * pi * t574 * t475 / 0.8D1 + t591 * t3
     # * t5 * pi * t586 * t365 / 0.8D1 + t603 * t3 * t5 * pi * t598 * t4
     #75 / 0.8D1 + t615 * t3 * t5 * pi * t610 * t365 / 0.8D1
      rrqg2qght8s4em1 = t434 + t622

      end function



      doubleprecision function rrqg2qght8s4em2
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = pi * t7
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x3
      t17 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t22 = z ** 2
      t26 = Sin(x4 * pi)
      t27 = t26 ** 2
      t29 = t1 ** 2
      t30 = t29 ** 2
      t33 = log(0.4D1 / t22 / z * t27 * t30)
      t36 = (-0.180D3 * lh - 0.90D2 * t33) * t3
      t37 = t5 * pi
      t41 = t6 * t8 * t9 / 0.8D1 + t6 * t8 * t13 / 0.16D2 + t6 * pi * t1
     #7 / 0.16D2 + t36 * t37 * t7 / 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t45 = pi * t44
      t52 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t59 = t6 * t45 * t9 / 0.8D1 + t6 * t45 * t13 / 0.16D2 + t6 * pi * 
     #t52 / 0.16D2 + t36 * t37 * t44 / 0.1440D4
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t59)
      t62 = t2 * x1
      t64 = t2 * (-0.1D1 + x1)
      t65 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t62, 0.0D0, -t64
     #, 0.0D0, 0.0D0)
      t67 = pi * t65 * t9
      t70 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t62, -t64, 0.0D0, -t6 * t67 
     #/ 0.8D1)
      t75 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t62, 0.0D0, -t64
     #, 0.0D0, 0.0D0)
      t77 = pi * t75 * t9
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t64, t62, 0.0D0, -t6 * t77 
     #/ 0.8D1)
      t85 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t86 = pi * t85
      t93 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t100 = t6 * t86 * t9 / 0.8D1 + t6 * t86 * t13 / 0.16D2 + t6 * pi *
     # t93 / 0.16D2 + t36 * t37 * t85 / 0.1440D4
      t101 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t100)
      t103 = t2 * x3
      t105 = t2 * (-0.1D1 + x3)
      t106 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t105, t103, 0.0D0)
      t108 = pi * t106 * t13
      t111 = FJET(XB1, XB2, s, 0.0D0, t103, 0.0D0, -t105, 0.0D0, -t6 * t
     #108 / 0.16D2)
      t116 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t105, t103, 0.0D0)
      t118 = pi * t116 * t13
      t121 = FJET(XB1, XB2, s, 0.0D0, -t105, 0.0D0, t103, 0.0D0, -t6 * t
     #118 / 0.16D2)
      t126 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t127 = pi * t126
      t134 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t141 = t6 * t127 * t9 / 0.8D1 + t6 * t127 * t13 / 0.16D2 + t6 * pi
     # * t134 / 0.16D2 + t36 * t37 * t126 / 0.1440D4
      t142 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t141)
      t144 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t62, 0.0D0, -t6
     #4, 0.0D0, 0.0D0)
      t146 = pi * t144 * t9
      t149 = FJET(XB1, XB2, s, t62, -t64, 0.0D0, 0.0D0, 0.0D0, -t6 * t14
     #6 / 0.8D1)
      t154 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t105, t103, 0.0D0)
      t156 = pi * t154 * t13
      t159 = FJET(XB1, XB2, s, t103, 0.0D0, -t105, 0.0D0, 0.0D0, -t6 * t
     #156 / 0.16D2)
      t164 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t62, 0.0D0, -t6
     #4, 0.0D0, 0.0D0)
      t166 = pi * t164 * t9
      t169 = FJET(XB1, XB2, s, -t64, t62, 0.0D0, 0.0D0, 0.0D0, -t6 * t16
     #6 / 0.8D1)
      t174 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t105, t103, 0.0D0)
      t176 = pi * t174 * t13
      t179 = FJET(XB1, XB2, s, -t105, 0.0D0, t103, 0.0D0, 0.0D0, -t6 * t
     #176 / 0.16D2)
      rrqg2qght8s4em2 = t42 * t41 + t60 * t59 - t70 * t3 * t5 * t67 / 0.
     #8D1 - t80 * t3 * t5 * t77 / 0.8D1 + t101 * t100 - t111 * t3 * t5 *
     # t108 / 0.16D2 - t121 * t3 * t5 * t118 / 0.16D2 + t142 * t141 - t1
     #49 * t3 * t5 * t146 / 0.8D1 - t159 * t3 * t5 * t156 / 0.16D2 - t16
     #9 * t3 * t5 * t166 / 0.8D1 - t179 * t3 * t5 * t176 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s4em3
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

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
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * pi * 
     #t7 / 0.16D2)
      t13 = t5 * pi
      t16 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * pi * 
     #t16 / 0.16D2)
      t24 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t24 / 0.16D2)
      t32 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * pi * 
     #t32 / 0.16D2)
      rrqg2qght8s4em3 = t11 * t3 * t13 * t7 / 0.16D2 + t20 * t3 * t13 * 
     #t16 / 0.16D2 + t28 * t3 * t13 * t24 / 0.16D2 + t36 * t3 * t13 * t3
     #2 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s4em4
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
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght8s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh81J1
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t7 = S24 ** 2
      t8 = 0.2D1 * t7
      t9 = S23 * S24
      t16 = t7 * S24
      t17 = 0.2D1 * t16
      t18 = S23 ** 2
      t19 = S24 * t18
      t21 = t7 * S23
      t25 = t18 * S23
      rrqg2qgh81J1 = (-S24 * t2 * t4 + (S24 - 0.4D1 * S23 + (t8 + 0.3D1 
     #* t9) * t2) * S12 - t8 - 0.4D1 * t9 + (-t17 - 0.3D1 * t19 - 0.4D1 
     #* t21) * t2 + (-0.4D1 * t25 + t17 + 0.6D1 * t21 + 0.7D1 * t19 + (S
     #24 * t25 + 0.2D1 * t16 * S23 + 0.2D1 * t7 * t18) * t2) / S12) / pi
     # * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh81J2
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S24 ** 2
      t7 = 0.2D1 * t6
      t8 = S23 * S24
      t16 = S23 ** 2
      t19 = S14 ** 2
      t20 = S24 * t19
      t21 = 0.2D1 * t20
      t22 = S24 * t16
      t25 = 0.2D1 * t6 * S14
      t26 = t6 * S23
      rrqg2qgh81J2 = (-S24 * t2 * t4 + (S24 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * S14 * S23 + 0.8D1 * t16 - 0.4D1 * t8 - t7 + (-t21 - 
     #0.3D1 * t22 - t25 - 0.4D1 * t26) * t2 + (t25 - 0.4D1 * t16 * S14 -
     # 0.4D1 * S23 * t19 + t21 + 0.7D1 * t22 + 0.6D1 * t26 + (0.2D1 * t6
     # * t16 + 0.2D1 * t20 * S23 + 0.2D1 * t26 * S14 + S24 * t16 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh81J3
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S24 ** 2
      t7 = 0.2D1 * t6
      t8 = S23 * S24
      t16 = S23 ** 2
      t19 = S14 ** 2
      t20 = S24 * t19
      t21 = 0.2D1 * t20
      t22 = S24 * t16
      t25 = 0.2D1 * t6 * S14
      t26 = t6 * S23
      rrqg2qgh81J3 = (-S24 * t2 * t4 + (S24 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * S14 * S23 + 0.8D1 * t16 - 0.4D1 * t8 - t7 + (-t21 - 
     #0.3D1 * t22 - t25 - 0.4D1 * t26) * t2 + (t25 - 0.4D1 * t16 * S14 -
     # 0.4D1 * S23 * t19 + t21 + 0.7D1 * t22 + 0.6D1 * t26 + (0.2D1 * t6
     # * t16 + 0.2D1 * t20 * S23 + 0.2D1 * t26 * S14 + S24 * t16 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh81J4
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S24 ** 2
      t7 = 0.2D1 * t6
      t8 = S23 * S24
      t16 = S23 ** 2
      t19 = S14 ** 2
      t20 = S24 * t19
      t21 = 0.2D1 * t20
      t22 = S24 * t16
      t25 = 0.2D1 * t6 * S14
      t26 = t6 * S23
      rrqg2qgh81J4 = (-S24 * t2 * t4 + (S24 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * S14 * S23 + 0.8D1 * t16 - 0.4D1 * t8 - t7 + (-t21 - 
     #0.3D1 * t22 - t25 - 0.4D1 * t26) * t2 + (t25 - 0.4D1 * t16 * S14 -
     # 0.4D1 * S23 * t19 + t21 + 0.7D1 * t22 + 0.6D1 * t26 + (0.2D1 * t6
     # * t16 + 0.2D1 * t20 * S23 + 0.2D1 * t26 * S14 + S24 * t16 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh81J5
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S24 ** 2
      t7 = 0.2D1 * t6
      t8 = S23 * S24
      t16 = S23 ** 2
      t19 = S14 ** 2
      t20 = S24 * t19
      t21 = 0.2D1 * t20
      t22 = S24 * t16
      t25 = 0.2D1 * t6 * S14
      t26 = t6 * S23
      rrqg2qgh81J5 = (-S24 * t2 * t4 + (S24 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * S14 * S23 + 0.8D1 * t16 - 0.4D1 * t8 - t7 + (-t21 - 
     #0.3D1 * t22 - t25 - 0.4D1 * t26) * t2 + (t25 - 0.4D1 * t16 * S14 -
     # 0.4D1 * S23 * t19 + t21 + 0.7D1 * t22 + 0.6D1 * t26 + (0.2D1 * t6
     # * t16 + 0.2D1 * t20 * S23 + 0.2D1 * t26 * S14 + S24 * t16 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh81J6
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
      t3 = S23 ** 2
      t7 = S24 ** 2
      t8 = t7 * S24
      t9 = t7 * S14
      t10 = S14 ** 2
      t11 = S24 * t10
      t15 = 0.1D1 / (S12 + S13 + S23)
      rrqg2qgh81J6 = (0.4D1 * S23 * S12 + 0.8D1 * t3 - 0.4D1 * S14 * S23
     # + (0.2D1 * t8 - 0.2D1 * t9 - 0.2D1 * t11) * t15 + (-0.2D1 * t8 + 
     #0.2D1 * t11 - 0.4D1 * t3 * S14 + 0.4D1 * t3 * S23 + 0.2D1 * t9 - 0
     #.4D1 * S23 * t10 + (0.2D1 * t11 * S23 - 0.2D1 * t8 * S23 + 0.2D1 *
     # t7 * S23 * S14) * t15) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh82J1
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
      t5 = S23 ** 2
      t7 = S24 ** 2
      rrqg2qgh82J1 = (0.8D1 / 0.9D1 * S24 * S12 + 0.8D1 / 0.9D1 * S23 * 
     #S24 + (0.8D1 / 0.9D1 * S24 * t5 + 0.8D1 / 0.9D1 * t7 * S23 + 0.8D1
     # / 0.9D1 * t7 * S24) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh82J2
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
      t3 = S24 ** 2
      t5 = S23 ** 2
      rrqg2qgh82J2 = (-0.8D1 / 0.9D1 * S23 * S24 - 0.16D2 / 0.9D1 * t3 +
     # (-0.8D1 / 0.9D1 * S24 * t5 - 0.8D1 / 0.9D1 * t3 * S23 - 0.4D1 / 0
     #.9D1 * S13 * S23 * S14) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh82J3
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t7 = S24 ** 2
      t12 = S23 ** 2
      t13 = t12 * S13
      t18 = S13 ** 2
      rrqg2qgh82J3 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.16D2 / 0.9D1 * 
     #t7 - 0.8D1 / 0.9D1 * S23 * S24 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * S24 * t12 + 0.4D1 / 0.9D1 * t18 *
     # S23 - 0.8D1 / 0.9D1 * t7 * S23 - 0.8D1 / 0.9D1 * t1 * S14 + 0.8D1
     # / 0.9D1 * t13) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh82J4
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t7 = S24 ** 2
      t12 = S23 ** 2
      t13 = t12 * S13
      t18 = S13 ** 2
      rrqg2qgh82J4 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.16D2 / 0.9D1 * 
     #t7 - 0.8D1 / 0.9D1 * S23 * S24 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * S24 * t12 + 0.4D1 / 0.9D1 * t18 *
     # S23 - 0.8D1 / 0.9D1 * t7 * S23 - 0.8D1 / 0.9D1 * t1 * S14 + 0.8D1
     # / 0.9D1 * t13) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh82J5
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t7 = S24 ** 2
      t12 = S23 ** 2
      t13 = t12 * S13
      t18 = S13 ** 2
      rrqg2qgh82J5 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.16D2 / 0.9D1 * 
     #t7 - 0.8D1 / 0.9D1 * S23 * S24 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * S24 * t12 + 0.4D1 / 0.9D1 * t18 *
     # S23 - 0.8D1 / 0.9D1 * t7 * S23 - 0.8D1 / 0.9D1 * t1 * S14 + 0.8D1
     # / 0.9D1 * t13) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh82J6
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
      t2 = S23 * S13
      t4 = 0.1D1 / (S12 + S14 + S24)
      t11 = S24 ** 2
      t14 = S23 ** 2
      t15 = t14 * S13
      t24 = S13 ** 2
      rrqg2qgh82J6 = ((-0.8D1 / 0.9D1 * S24 - 0.4D1 / 0.9D1 * t2 * t4) *
     # S12 - 0.16D2 / 0.9D1 * S23 * S24 - 0.16D2 / 0.9D1 * t11 + 0.4D1 /
     # 0.9D1 * t2 - 0.4D1 / 0.9D1 * t15 * t4 + (-0.16D2 / 0.9D1 * S24 * 
     #t14 - 0.16D2 / 0.9D1 * t11 * S23 - 0.8D1 / 0.9D1 * t2 * S14 + 0.4D
     #1 / 0.9D1 * S23 * t24 + 0.8D1 / 0.9D1 * t15 - 0.8D1 / 0.9D1 * t11 
     #* S24) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh82J7
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t8 = S23 ** 2
      t9 = t8 * S13
      t14 = S13 ** 2
      rrqg2qgh82J7 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 + 0.4D1 / 0.9D1 * t
     #1 - 0.4D1 / 0.9D1 * t9 * t3 + (-0.4D1 / 0.9D1 * t1 * S14 + 0.4D1 /
     # 0.9D1 * S23 * t14 + 0.8D1 / 0.9D1 * t9) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh83J1
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
      t5 = S13 ** 2
      t7 = S14 ** 2
      rrqg2qgh83J1 = (0.8D1 / 0.9D1 * S12 * S13 + 0.8D1 / 0.9D1 * S13 * 
     #S14 + (0.8D1 / 0.9D1 * S14 * t5 + 0.8D1 / 0.9D1 * t7 * S13 + 0.8D1
     # / 0.9D1 * t5 * S13) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh83J2
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
      t3 = S13 ** 2
      t7 = S14 ** 2
      rrqg2qgh83J2 = (-0.8D1 / 0.9D1 * S13 * S14 - 0.16D2 / 0.9D1 * t3 +
     # (-0.8D1 / 0.9D1 * t3 * S14 - 0.8D1 / 0.9D1 * t7 * S13 - 0.4D1 / 0
     #.9D1 * S24 * S23 * S14) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh83J3
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t7 = S13 ** 2
      t12 = S14 ** 2
      t13 = S24 * t12
      t24 = S24 ** 2
      rrqg2qgh83J3 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.16D2 / 0.9D1 * 
     #t7 - 0.8D1 / 0.9D1 * S13 * S14 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t7 * S14 - 0.8D1 / 0.9D1 * t12 * 
     #S13 + 0.8D1 / 0.9D1 * t13 - 0.8D1 / 0.9D1 * S24 * S23 * S14 + 0.4D
     #1 / 0.9D1 * t24 * S14) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh83J4
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t7 = S13 ** 2
      t12 = S14 ** 2
      t13 = S24 * t12
      t24 = S24 ** 2
      rrqg2qgh83J4 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.16D2 / 0.9D1 * 
     #t7 - 0.8D1 / 0.9D1 * S13 * S14 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t7 * S14 - 0.8D1 / 0.9D1 * t12 * 
     #S13 + 0.8D1 / 0.9D1 * t13 - 0.8D1 / 0.9D1 * S24 * S23 * S14 + 0.4D
     #1 / 0.9D1 * t24 * S14) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh83J5
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t7 = S13 ** 2
      t12 = S14 ** 2
      t13 = S24 * t12
      t24 = S24 ** 2
      rrqg2qgh83J5 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.16D2 / 0.9D1 * 
     #t7 - 0.8D1 / 0.9D1 * S13 * S14 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t7 * S14 - 0.8D1 / 0.9D1 * t12 * 
     #S13 + 0.8D1 / 0.9D1 * t13 - 0.8D1 / 0.9D1 * S24 * S23 * S14 + 0.4D
     #1 / 0.9D1 * t24 * S14) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh83J6
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
      t2 = S24 * S14
      t4 = 0.1D1 / (S12 + S13 + S23)
      t9 = S13 ** 2
      t14 = S14 ** 2
      t15 = S24 * t14
      t28 = S24 ** 2
      rrqg2qgh83J6 = ((-0.8D1 / 0.9D1 * S13 - 0.4D1 / 0.9D1 * t2 * t4) *
     # S12 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t2 - 0.16D2 / 0.9D1 *
     # S13 * S14 - 0.4D1 / 0.9D1 * t15 * t4 + (-0.16D2 / 0.9D1 * t14 * S
     #13 - 0.16D2 / 0.9D1 * S14 * t9 - 0.8D1 / 0.9D1 * S24 * S23 * S14 -
     # 0.8D1 / 0.9D1 * t9 * S13 + 0.8D1 / 0.9D1 * t15 + 0.4D1 / 0.9D1 * 
     #t28 * S14) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh83J7
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t8 = S14 ** 2
      t9 = S24 * t8
      t13 = S24 ** 2
      rrqg2qgh83J7 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 + 0.4D1 / 0.9D1 * t
     #1 - 0.4D1 / 0.9D1 * t9 * t3 + (0.8D1 / 0.9D1 * t9 + 0.4D1 / 0.9D1 
     #* t13 * S14 - 0.4D1 / 0.9D1 * S24 * S23 * S14) / S12) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrqg2qgh84J1
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
      t4 = S12 ** 2
      t7 = S13 ** 2
      t8 = 0.2D1 * t7
      t9 = S13 * S14
      t16 = t7 * S13
      t17 = 0.2D1 * t16
      t18 = S14 ** 2
      t19 = t18 * S13
      t21 = t7 * S14
      t25 = t18 * S14
      rrqg2qgh84J1 = (-S13 * t2 * t4 + (-0.4D1 * S14 + S13 + (t8 + 0.3D1
     # * t9) * t2) * S12 - t8 - 0.4D1 * t9 + (-t17 - 0.3D1 * t19 - 0.4D1
     # * t21) * t2 + (-0.4D1 * t25 + t17 + 0.7D1 * t19 + 0.6D1 * t21 + (
     #0.2D1 * t16 * S14 + 0.2D1 * t7 * t18 + S13 * t25) * t2) / S12) / p
     #i * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh84J2
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
      t4 = S12 ** 2
      t6 = S13 ** 2
      t7 = 0.2D1 * t6
      t8 = S13 * S14
      t17 = S14 ** 2
      t19 = t17 * S13
      t22 = 0.2D1 * t6 * S23
      t23 = S23 ** 2
      t25 = 0.2D1 * t23 * S13
      t26 = t6 * S14
      rrqg2qgh84J2 = (-S13 * t2 * t4 + (S13 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * t8 - 0.4D1 * S14 * S23 - t7 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (-0.4D1 * t23 * S14 - 0.4D
     #1 * S23 * t17 + 0.7D1 * t19 + t25 + t22 + 0.6D1 * t26 + (S13 * t17
     # * S14 + 0.2D1 * t8 * t23 + 0.2D1 * t26 * S23 + 0.2D1 * t6 * t17) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh84J3
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
      t4 = S12 ** 2
      t6 = S13 ** 2
      t7 = 0.2D1 * t6
      t8 = S13 * S14
      t17 = S14 ** 2
      t19 = t17 * S13
      t22 = 0.2D1 * t6 * S23
      t23 = S23 ** 2
      t25 = 0.2D1 * t23 * S13
      t26 = t6 * S14
      rrqg2qgh84J3 = (-S13 * t2 * t4 + (S13 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * t8 - 0.4D1 * S14 * S23 - t7 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (-0.4D1 * t23 * S14 - 0.4D
     #1 * S23 * t17 + 0.7D1 * t19 + t25 + t22 + 0.6D1 * t26 + (S13 * t17
     # * S14 + 0.2D1 * t8 * t23 + 0.2D1 * t26 * S23 + 0.2D1 * t6 * t17) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh84J4
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
      t4 = S12 ** 2
      t6 = S13 ** 2
      t7 = 0.2D1 * t6
      t8 = S13 * S14
      t17 = S14 ** 2
      t19 = t17 * S13
      t22 = 0.2D1 * t6 * S23
      t23 = S23 ** 2
      t25 = 0.2D1 * t23 * S13
      t26 = t6 * S14
      rrqg2qgh84J4 = (-S13 * t2 * t4 + (S13 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * t8 - 0.4D1 * S14 * S23 - t7 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (-0.4D1 * t23 * S14 - 0.4D
     #1 * S23 * t17 + 0.7D1 * t19 + t25 + t22 + 0.6D1 * t26 + (S13 * t17
     # * S14 + 0.2D1 * t8 * t23 + 0.2D1 * t26 * S23 + 0.2D1 * t6 * t17) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh84J5
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
      t4 = S12 ** 2
      t6 = S13 ** 2
      t7 = 0.2D1 * t6
      t8 = S13 * S14
      t17 = S14 ** 2
      t19 = t17 * S13
      t22 = 0.2D1 * t6 * S23
      t23 = S23 ** 2
      t25 = 0.2D1 * t23 * S13
      t26 = t6 * S14
      rrqg2qgh84J5 = (-S13 * t2 * t4 + (S13 + (t7 + 0.3D1 * t8) * t2) * 
     #S12 - 0.4D1 * t8 - 0.4D1 * S14 * S23 - t7 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (-0.4D1 * t23 * S14 - 0.4D
     #1 * S23 * t17 + 0.7D1 * t19 + t25 + t22 + 0.6D1 * t26 + (S13 * t17
     # * S14 + 0.2D1 * t8 * t23 + 0.2D1 * t26 * S23 + 0.2D1 * t6 * t17) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh84J6
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
      t3 = S14 ** 2
      t7 = S13 ** 2
      t8 = t7 * S23
      t9 = S23 ** 2
      t10 = t9 * S13
      t11 = t7 * S13
      t15 = 0.1D1 / (S12 + S14 + S24)
      rrqg2qgh84J6 = (0.4D1 * S12 * S14 + 0.8D1 * t3 - 0.4D1 * S14 * S23
     # + (-0.2D1 * t8 - 0.2D1 * t10 + 0.2D1 * t11) * t15 + (0.4D1 * t3 *
     # S14 + 0.2D1 * t8 - 0.4D1 * S14 * t9 - 0.2D1 * t11 + 0.2D1 * t10 -
     # 0.4D1 * t3 * S23 + (-0.2D1 * t11 * S14 + 0.2D1 * S13 * S14 * t9 +
     # 0.2D1 * t7 * S23 * S14) * t15) / S12) / pi * wd / z

      end function
  
 