  
      subroutine rrgq2qght5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh51J1  
      doubleprecision rrgq2qgh51J2  
      doubleprecision rrgq2qgh51J3  
      doubleprecision rrgq2qgh51J4  
      doubleprecision rrgq2qgh51J5  
      doubleprecision rrgq2qgh51J6  
      doubleprecision rrgq2qght5s1e1  
      doubleprecision rrgq2qght5s1e0  
      doubleprecision rrgq2qght5s1em1  
      doubleprecision rrgq2qght5s1em2  
      doubleprecision rrgq2qght5s1em3  
      doubleprecision rrgq2qght5s1em4  
      doubleprecision rrgq2qght5s2e1  
      doubleprecision rrgq2qght5s2e0  
      doubleprecision rrgq2qght5s2em1  
      doubleprecision rrgq2qght5s2em2  
      doubleprecision rrgq2qght5s2em3  
      doubleprecision rrgq2qght5s2em4  
      doubleprecision rrgq2qght5s3e1  
      doubleprecision rrgq2qght5s3e0  
      doubleprecision rrgq2qght5s3em1  
      doubleprecision rrgq2qght5s3em2  
      doubleprecision rrgq2qght5s3em3  
      doubleprecision rrgq2qght5s3em4  
      doubleprecision rrgq2qght5s4e1  
      doubleprecision rrgq2qght5s4e0  
      doubleprecision rrgq2qght5s4em1  
      doubleprecision rrgq2qght5s4em2  
      doubleprecision rrgq2qght5s4em3  
      doubleprecision rrgq2qght5s4em4  
      doubleprecision rrgq2qght5s5e1  
      doubleprecision rrgq2qght5s5e0  
      doubleprecision rrgq2qght5s5em1  
      doubleprecision rrgq2qght5s5em2  
      doubleprecision rrgq2qght5s5em3  
      doubleprecision rrgq2qght5s5em4  
      doubleprecision rrgq2qght5s6e1  
      doubleprecision rrgq2qght5s6e0  
      doubleprecision rrgq2qght5s6em1  
      doubleprecision rrgq2qght5s6em2  
      doubleprecision rrgq2qght5s6em3  
      doubleprecision rrgq2qght5s6em4  
      doubleprecision rrgq2qght5s7e1  
      doubleprecision rrgq2qght5s7e0  
      doubleprecision rrgq2qght5s7em1  
      doubleprecision rrgq2qght5s7em2  
      doubleprecision rrgq2qght5s7em3  
      doubleprecision rrgq2qght5s7em4  
      doubleprecision rrgq2qght5s8e1  
      doubleprecision rrgq2qght5s8e0  
      doubleprecision rrgq2qght5s8em1  
      doubleprecision rrgq2qght5s8em2  
      doubleprecision rrgq2qght5s8em3  
      doubleprecision rrgq2qght5s8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght5s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght5s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgq2qght5s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgq2qght5s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgq2qght5s7em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgq2qght5s8em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght5s1e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t29 = t16 * x4
      t30 = -0.1D1 + x4
      t31 = t29 * t30
      t34 = log(-0.4D1 * t12 * t14 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = lh ** 2
      t59 = pi ** 2
      t61 = 0.180D3 * t57 - 0.30D2 * t59
      t62 = pi * t61
      t63 = t12 * t17
      t65 = log(0.4D1 * t63)
      t70 = t65 ** 2
      t73 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t77 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t88 = 0.60D2 * lh * t59 - 0.240D3 * zeta3 - 0.120D3 * t57 * lh
      t89 = pi * t88
      t90 = t44 * t25
      t91 = t89 * t90
      t102 = t4 * t7
      t103 = x3 * t8
      t104 = t103 * t11
      t105 = x4 * t30
      t109 = log(-0.4D1 * t104 * t17 * t105)
      t113 = log(0.4D1 * t104 * t18)
      t116 = 0.1D1 / x3
      t118 = t52 * t54
      t122 = t11 * t14
      t123 = t122 * t16
      t124 = t103 * t123
      t126 = log(0.4D1 * t124)
      t128 = t126 ** 2
      t140 = t62 * t90
      t145 = t7 * t22
      t153 = log(0.4D1 * t122 * t29)
      t154 = t153 ** 2
      t157 = log(-0.4D1 * t122 * t31)
      t158 = t157 ** 2
      t162 = t7 * t25
      t173 = t7 * t77
      t183 = log(0.4D1 * t123)
      t184 = t183 * pi
      t186 = t183 ** 2
      t187 = t186 * pi
      t191 = t186 * t183 * pi
      t211 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t215 = t59 ** 2
      t216 = t57 ** 2
      t229 = t186 ** 2
      t236 = x3 * t11
      t237 = t236 * t14
      t240 = log(-0.4D1 * t237 * t31)
      t242 = t240 ** 2
      t247 = log(0.4D1 * t236 * t18)
      t249 = t247 ** 2
      t268 = log(0.4D1 * t236 * t17)
      t273 = t268 ** 2
      t294 = -(0.90D2 * t4 * t7 * (t21 * t22 - t24 * t25 / 0.2D1 - t34 *
     # t22 + t36 * t25 / 0.2D1) - 0.180D3 * t43 * t44 * (t21 * t25 - t34
     # * t25)) * t52 * t54 / 0.720D3 - (t62 * t44 * (-t22 + t65 * t25) +
     # 0.90D2 * t4 * t7 * (-t70 * t22 / 0.2D1 - t73 + t70 * t65 * t25 / 
     #0.6D1 + t65 * t77) - t91 - 0.180D3 * t43 * t44 * (t65 * t22 - t70 
     #* t25 / 0.2D1 - t77)) * t52 / 0.720D3 - t102 * (-t109 * t25 + t113
     # * t25) * t116 * t118 / 0.8D1 - (0.90D2 * t4 * t7 * (t126 * t22 - 
     #t128 * t25 / 0.2D1 - t77) - 0.180D3 * t43 * t44 * (-t22 + t126 * t
     #25) - t140) * t116 * t52 / 0.720D3 + ((-0.90D2 * t4 * t145 + 0.180
     #D3 * t43 * t90) * (-t154 / 0.2D1 + t158 / 0.2D1) - 0.90D2 * t4 * t
     #162 * (-t158 * t157 / 0.6D1 + t154 * t153 / 0.6D1) + (0.180D3 * t4
     #3 * t44 * t22 - t140 - 0.90D2 * t4 * t173) * (t153 - t157)) * t54 
     #/ 0.1440D4 + (t89 - t184 * t61 - 0.90D2 * t187 * lh - 0.15D2 * t19
     #1) * t3 * t145 / 0.1440D4 + (t62 + 0.180D3 * t184 * lh + 0.45D2 * 
     #t187) * t3 * t173 / 0.1440D4 + (-0.180D3 * t43 - 0.90D2 * t184) * 
     #t3 * t7 * t73 / 0.1440D4 + t4 * t7 * t211 / 0.16D2 + (pi * (t215 +
     # 0.60D2 * t216 + 0.480D3 * lh * zeta3 - 0.60D2 * t57 * t59) - t184
     # * t88 + t187 * t61 / 0.2D1 + 0.30D2 * t191 * lh + 0.15D2 / 0.4D1 
     #* t229 * pi) * t3 * t162 / 0.1440D4 + (0.90D2 * t4 * t7 * (t240 * 
     #t22 - t242 * t25 / 0.2D1 - t247 * t22 + t249 * t25 / 0.2D1) - 0.18
     #0D3 * t43 * t44 * (t240 * t25 - t247 * t25)) * t116 * t54 / 0.1440
     #D4 - (t62 * t44 * (-t22 + t268 * t25) + 0.90D2 * t4 * t7 * (-t273 
     #* t22 / 0.2D1 - t73 + t273 * t268 * t25 / 0.6D1 + t268 * t77) - t9
     #1 - 0.180D3 * t43 * t44 * (t268 * t22 - t273 * t25 / 0.2D1 - t77))
     # * t116 / 0.1440D4
      t295 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t294)
      t297 = 0.1D1 - x1
      t298 = 0.1D1 - x3
      t299 = KAPPA2(t297, x2, t298, 0.10D1, z)
      t300 = s * t299
      t301 = -t297
      t302 = t1 * t301
      t303 = -t298
      t304 = t302 * t303
      t305 = t300 * t304
      t306 = t302 * x3
      t307 = t300 * t306
      t308 = t1 * x1
      t309 = t300 * t308
      t310 = t299 ** 2
      t313 = t301 * x1
      t315 = s * t310 * t15 * t313 * x3
      t317 = 0.1D1 / (-0.2D1 + t299)
      t318 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t305, t309, -t3
     #07, 0.0D0, -t315)
      t319 = t317 * t318
      t320 = t103 * t122
      t321 = t301 ** 2
      t322 = t16 * t321
      t323 = t303 * x4
      t324 = t310 ** 2
      t329 = log(-0.4D1 * t320 * t322 * t323 * t324)
      t331 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t305, t309, -t3
     #07, 0.0D0, -t315)
      t337 = t43 * t3
      t339 = t7 * t317 * t331
      t349 = log(-0.4D1 * t320 * t322 * t303 * t324)
      t350 = t349 * t317
      t352 = t349 ** 2
      t356 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t305, t309, -t3
     #07, 0.0D0, -t315)
      t367 = t62 * t3
      t373 = -(0.90D2 * t4 * t7 * (t319 - t329 * t317 * t331) - 0.180D3 
     #* t337 * t339) * t116 * t118 / 0.720D3 - (0.90D2 * t4 * t7 * (-t35
     #0 * t318 + t352 * t317 * t331 / 0.2D1 + t317 * t356) - 0.180D3 * t
     #43 * t44 * (t319 - t350 * t331) + t367 * t339) * t116 * t52 / 0.72
     #0D3
      t374 = FJET(XB1, XB2, s, t305, -t307, t309, 0.0D0, -t315, t373)
      t376 = -t30
      t377 = KAPPA2(t297, x2, t298, t376, z)
      t378 = s * t377
      t379 = t378 * t304
      t380 = t378 * t306
      t381 = t308 * t30
      t382 = t378 * t381
      t383 = t308 * x4
      t384 = t378 * t383
      t385 = t377 ** 2
      t390 = cos(t9)
      t393 = Sqrt(x3 * t303 * t105)
      t398 = s * t385 * t15 * t313 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t390 * t393)
      t400 = 0.1D1 / (-0.2D1 + t377)
      t401 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t379, -t382, -t
     #380, t384, t398)
      t404 = t385 ** 2
      t409 = log(0.4D1 * t124 * t321 * t303 * t105 * t404)
      t411 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t379, -t382, -t
     #380, t384, t398)
      t421 = 0.90D2 * t4 * t7 * (-t400 * t401 + t409 * t400 * t411) + 0.
     #180D3 * t337 * t7 * t400 * t411
      t425 = FJET(XB1, XB2, s, t379, -t380, -t382, t384, t398, -t421 * t
     #116 * t118 / 0.720D3)
      t431 = t2 * t301
      t432 = t2 * x1
      t437 = log(0.4D1 * t123 * t8 * t321 * x4)
      t438 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t431, t432, 0.
     #0D0, 0.0D0, 0.0D0)
      t440 = t437 ** 2
      t441 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t431, t432, 0.
     #0D0, 0.0D0, 0.0D0)
      t444 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t431, t432, 0.
     #0D0, 0.0D0, 0.0D0)
      t454 = t44 * t441
      t455 = t62 * t454
      t459 = t16 * t8
      t460 = t459 * t321
      t463 = log(0.4D1 * t122 * t460)
      t468 = t463 ** 2
      t471 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t431, t432, 0.
     #0D0, 0.0D0, 0.0D0)
      t490 = t321 * x4
      t494 = log(0.4D1 * t237 * t459 * t490)
      t507 = log(0.4D1 * t237 * t460)
      t509 = t507 ** 2
      t525 = -(0.90D2 * t4 * t7 * (-t437 * t438 + t440 * t441 / 0.2D1 + 
     #t444) - 0.180D3 * t43 * t44 * (t438 - t437 * t441) + t455) * t52 *
     # t54 / 0.720D3 - (t62 * t44 * (t438 - t463 * t441) + 0.90D2 * t4 *
     # t7 * (t468 * t438 / 0.2D1 + t471 - t468 * t463 * t441 / 0.6D1 - t
     #463 * t444) + t89 * t454 - 0.180D3 * t43 * t44 * (-t463 * t438 + t
     #468 * t441 / 0.2D1 + t444)) * t52 / 0.720D3 - (0.90D2 * t4 * t7 * 
     #(t438 - t494 * t441) - 0.180D3 * t43 * t454) * t116 * t118 / 0.720
     #D3 - (0.90D2 * t4 * t7 * (-t507 * t438 + t509 * t441 / 0.2D1 + t44
     #4) - 0.180D3 * t43 * t44 * (t438 - t507 * t441) + t455) * t116 * t
     #52 / 0.720D3
      t526 = FJET(XB1, XB2, s, -t431, 0.0D0, t432, 0.0D0, 0.0D0, t525)
      t528 = t2 * t303
      t529 = t2 * x3
      t533 = log(-0.4D1 * t104 * t17 * t323)
      t534 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t528, 0.0D0, t
     #529, 0.0D0, 0.0D0)
      t537 = t29 * t30 * t303
      t540 = log(0.4D1 * t320 * t537)
      t547 = t17 * t303
      t550 = log(-0.4D1 * t104 * t547)
      t551 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t528, 0.0D0, t
     #529, 0.0D0, 0.0D0)
      t553 = t550 ** 2
      t556 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t528, 0.0D0, t
     #529, 0.0D0, 0.0D0)
      t566 = t44 * t534
      t576 = log(-0.4D1 * t237 * t16 * t303 * x4)
      t578 = t576 ** 2
      t583 = log(0.4D1 * t237 * t537)
      t585 = t583 ** 2
      t604 = log(-0.4D1 * t236 * t547)
      t609 = t604 ** 2
      t612 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t528, 0.0D0, t
     #529, 0.0D0, 0.0D0)
      t632 = -t102 * (-t533 * t534 + t540 * t534) * t116 * t118 / 0.8D1 
     #- (-0.90D2 * t4 * t7 * (t550 * t551 - t553 * t534 / 0.2D1 - t556) 
     #+ 0.180D3 * t43 * t44 * (-t551 + t550 * t534) + t62 * t566) * t116
     # * t52 / 0.720D3 + (0.90D2 * t4 * t7 * (t576 * t551 - t578 * t534 
     #/ 0.2D1 - t583 * t551 + t585 * t534 / 0.2D1) - 0.180D3 * t43 * t44
     # * (t576 * t534 - t583 * t534)) * t116 * t54 / 0.1440D4 - (-t62 * 
     #t44 * (-t551 + t604 * t534) - 0.90D2 * t4 * t7 * (-t609 * t551 / 0
     #.2D1 - t612 + t609 * t604 * t534 / 0.6D1 + t604 * t556) + t89 * t5
     #66 + 0.180D3 * t43 * t44 * (t604 * t551 - t609 * t534 / 0.2D1 - t5
     #56)) * t116 / 0.1440D4
      t633 = FJET(XB1, XB2, s, -t528, t529, 0.0D0, 0.0D0, 0.0D0, t632)
      t635 = KAPPA2(t297, x2, 0.10D1, t376, z)
      t636 = s * t635
      t637 = t636 * t302
      t638 = t636 * t381
      t639 = t636 * t383
      t640 = t635 ** 2
      t644 = s * t640 * t15 * t313 * x4
      t645 = t640 ** 2
      t650 = log(-0.4D1 * t63 * t490 * t30 * t645)
      t652 = 0.1D1 / (-0.2D1 + t635)
      t653 = t650 * t652
      t654 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t637, -t638, 0
     #.0D0, t639, -t644)
      t656 = t650 ** 2
      t658 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t637, -t638, 0
     #.0D0, t639, -t644)
      t661 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t637, -t638, 0
     #.0D0, t639, -t644)
      t667 = t652 * t654
      t674 = t7 * t652 * t658
      t683 = log(-0.4D1 * t320 * t322 * t105 * t645)
      t696 = -(0.90D2 * t4 * t7 * (-t653 * t654 + t656 * t652 * t658 / 0
     #.2D1 + t652 * t661) - 0.180D3 * t43 * t44 * (t667 - t653 * t658) +
     # t367 * t674) * t52 * t54 / 0.720D3 - (0.90D2 * t4 * t7 * (t667 - 
     #t683 * t652 * t658) - 0.180D3 * t337 * t674) * t116 * t118 / 0.720
     #D3
      t697 = FJET(XB1, XB2, s, -t637, 0.0D0, -t638, t639, -t644, t696)
      rrgq2qght5s1e1 = t295 * t294 + t374 * t373 - t425 * t421 * t116 * 
     #t52 * t54 / 0.720D3 + t526 * t525 + t633 * t632 + t697 * t696

      end function



      doubleprecision function rrgq2qght5s1e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = t17 * x4
      t27 = -0.1D1 + x4
      t28 = t26 * t27
      t31 = log(-0.4D1 * t13 * t15 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t17
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t23
      t56 = 0.180D3 * t52 * t54
      t58 = 0.1D1 / x3
      t62 = t13 * t18
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t69 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t79 = lh ** 2
      t81 = pi ** 2
      t83 = 0.180D3 * t79 - 0.30D2 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t89 = t7 * t23
      t92 = log(0.4D1 * t42 * t26)
      t93 = t92 ** 2
      t96 = log(-0.4D1 * t42 * t28)
      t97 = t96 ** 2
      t103 = t7 * t40
      t113 = log(0.4D1 * t43)
      t114 = t113 * pi
      t117 = t113 ** 2
      t118 = t117 * pi
      t124 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t152 = x3 * t12
      t153 = t152 * t15
      t156 = log(-0.4D1 * t153 * t28)
      t160 = log(0.4D1 * t152 * t19)
      t169 = log(0.4D1 * t152 * t18)
      t171 = t169 ** 2
      t186 = -t8 * (t22 * t23 - t31 * t23) * t34 * t36 / 0.8D1 - (0.90D2
     # * t4 * t7 * (-t40 + t46 * t23) + t56) * t58 * t34 / 0.720D3 - (0.
     #90D2 * t4 * t7 * (t64 * t40 - t66 * t23 / 0.2D1 - t69) - 0.180D3 *
     # t52 * t53 * (-t40 + t64 * t23) - t85) * t34 / 0.720D3 + (-0.90D2 
     #* t4 * t89 * (-t93 / 0.2D1 + t97 / 0.2D1) + (-0.90D2 * t4 * t103 +
     # t56) * (t92 - t96)) * t36 / 0.1440D4 + (t84 + 0.180D3 * t114 * lh
     # + 0.45D2 * t118) * t3 * t103 / 0.1440D4 + t4 * t7 * t124 / 0.16D2
     # + (pi * (0.60D2 * lh * t81 - 0.240D3 * zeta3 - 0.120D3 * t79 * lh
     #) - t114 * t83 - 0.90D2 * t118 * lh - 0.15D2 * t117 * t113 * pi) *
     # t3 * t89 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t114) * t3 * t7 
     #* t69 / 0.1440D4 + t8 * (t156 * t23 - t160 * t23) * t58 * t36 / 0.
     #16D2 - (0.90D2 * t4 * t7 * (t169 * t40 - t171 * t23 / 0.2D1 - t69)
     # - 0.180D3 * t52 * t53 * (-t40 + t169 * t23) - t85) * t58 / 0.1440
     #D4
      t187 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t186)
      t189 = 0.1D1 - x1
      t190 = 0.1D1 - x3
      t191 = KAPPA2(t189, x2, t190, 0.10D1, z)
      t192 = s * t191
      t193 = -t189
      t194 = t1 * t193
      t195 = -t190
      t196 = t194 * t195
      t197 = t192 * t196
      t198 = t194 * x3
      t199 = t192 * t198
      t200 = t1 * x1
      t201 = t192 * t200
      t202 = t191 ** 2
      t205 = t193 * x1
      t207 = s * t202 * t16 * t205 * x3
      t209 = 0.1D1 / (-0.2D1 + t191)
      t210 = t7 * t209
      t212 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t197, t201, -t1
     #99, 0.0D0, -t207)
      t214 = t34 * t36
      t218 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t197, t201, -t1
     #99, 0.0D0, -t207)
      t221 = t193 ** 2
      t223 = t202 ** 2
      t228 = log(-0.4D1 * t41 * t42 * t17 * t221 * t195 * t223)
      t235 = t52 * t3
      t243 = -t4 * t210 * t212 * t58 * t214 / 0.8D1 - (0.90D2 * t4 * t7 
     #* (t209 * t218 - t228 * t209 * t212) - 0.180D3 * t235 * t210 * t21
     #2) * t58 * t34 / 0.720D3
      t244 = FJET(XB1, XB2, s, t197, -t199, t201, 0.0D0, -t207, t243)
      t246 = -t27
      t247 = KAPPA2(t189, x2, t190, t246, z)
      t248 = s * t247
      t249 = t248 * t196
      t250 = t248 * t198
      t251 = t200 * t27
      t252 = t248 * t251
      t253 = t200 * x4
      t254 = t248 * t253
      t255 = t247 ** 2
      t260 = cos(t10)
      t264 = Sqrt(x3 * t195 * x4 * t27)
      t269 = s * t255 * t16 * t205 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t260 * t264)
      t271 = 0.1D1 / (-0.2D1 + t247)
      t274 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t249, -t252, -t
     #250, t254, t269)
      t279 = FJET(XB1, XB2, s, t249, -t250, -t252, t254, t269, t4 * t7 *
     # t271 * t274 * t58 * t214 / 0.8D1)
      t288 = t2 * t193
      t289 = t2 * x1
      t290 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t288, t289, 0.
     #0D0, 0.0D0, 0.0D0)
      t295 = log(0.4D1 * t43 * t9 * t221 * x4)
      t296 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t288, t289, 0.
     #0D0, 0.0D0, 0.0D0)
      t302 = t53 * t296
      t304 = 0.180D3 * t52 * t302
      t314 = t17 * t9 * t221
      t317 = log(0.4D1 * t153 * t314)
      t329 = log(0.4D1 * t42 * t314)
      t331 = t329 ** 2
      t334 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t288, t289, 0.
     #0D0, 0.0D0, 0.0D0)
      t348 = -(0.90D2 * t4 * t7 * (t290 - t295 * t296) - t304) * t34 * t
     #36 / 0.720D3 - t8 * t296 * t58 * t214 / 0.8D1 - (0.90D2 * t4 * t7 
     #* (t290 - t317 * t296) - t304) * t58 * t34 / 0.720D3 - (0.90D2 * t
     #4 * t7 * (-t329 * t290 + t331 * t296 / 0.2D1 + t334) - 0.180D3 * t
     #52 * t53 * (t290 - t329 * t296) + t84 * t302) * t34 / 0.720D3
      t349 = FJET(XB1, XB2, s, -t288, 0.0D0, t289, 0.0D0, 0.0D0, t348)
      t351 = t2 * t195
      t352 = t2 * x3
      t353 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t351, 0.0D0, t
     #352, 0.0D0, 0.0D0)
      t355 = t18 * t195
      t358 = log(-0.4D1 * t41 * t12 * t355)
      t359 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, 0.0D0, t
     #352, 0.0D0, 0.0D0)
      t365 = t53 * t359
      t376 = log(-0.4D1 * t153 * t17 * t195 * x4)
      t382 = log(0.4D1 * t153 * t26 * t27 * t195)
      t391 = log(-0.4D1 * t152 * t355)
      t393 = t391 ** 2
      t396 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t351, 0.0D0, t
     #352, 0.0D0, 0.0D0)
      t410 = -(-0.90D2 * t4 * t7 * (-t353 + t358 * t359) - 0.180D3 * t52
     # * t365) * t58 * t34 / 0.720D3 + t8 * (t376 * t359 - t382 * t359) 
     #* t58 * t36 / 0.16D2 - (-0.90D2 * t4 * t7 * (t391 * t353 - t393 * 
     #t359 / 0.2D1 - t396) + 0.180D3 * t52 * t53 * (-t353 + t391 * t359)
     # + t84 * t365) * t58 / 0.1440D4
      t411 = FJET(XB1, XB2, s, -t351, t352, 0.0D0, 0.0D0, 0.0D0, t410)
      t413 = KAPPA2(t189, x2, 0.10D1, t246, z)
      t414 = s * t413
      t415 = t414 * t194
      t416 = t414 * t251
      t417 = t414 * t253
      t418 = t413 ** 2
      t422 = s * t418 * t16 * t205 * x4
      t424 = 0.1D1 / (-0.2D1 + t413)
      t425 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t415, -t416, 0
     #.0D0, t417, -t422)
      t428 = t418 ** 2
      t433 = log(-0.4D1 * t62 * t221 * x4 * t27 * t428)
      t435 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t415, -t416, 0
     #.0D0, t417, -t422)
      t441 = t7 * t424
      t454 = -(0.90D2 * t4 * t7 * (t424 * t425 - t433 * t424 * t435) - 0
     #.180D3 * t235 * t441 * t435) * t34 * t36 / 0.720D3 - t4 * t441 * t
     #435 * t58 * t214 / 0.8D1
      t455 = FJET(XB1, XB2, s, -t415, 0.0D0, -t416, t417, -t422, t454)
      rrgq2qght5s1e0 = t187 * t186 + t244 * t243 + t279 * pi * t53 * t27
     #1 * t274 * t58 * t34 * t36 / 0.8D1 + t349 * t348 + t411 * t410 + t
     #455 * t454

      end function



      doubleprecision function rrgq2qght5s1em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t17 * t20 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t47 = log(0.4D1 * t43 * t44)
      t48 = -0.1D1 + x4
      t52 = log(-0.4D1 * t43 * t44 * t48)
      t55 = 0.1D1 / x4
      t62 = log(0.4D1 * t43 * t25)
      t63 = t62 * pi
      t70 = lh ** 2
      t72 = pi ** 2
      t78 = t62 ** 2
      t86 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t90 = x3 * t20
      t93 = log(0.4D1 * t90 * t26)
      t102 = t8 * t9 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (-t16 + t
     #29 * t9) + t39) * t12 / 0.720D3 - t8 * t9 * (t47 - t52) * t55 / 0.
     #16D2 + (-0.180D3 * t35 - 0.90D2 * t63) * t3 * t7 * t16 / 0.1440D4 
     #+ (pi * (0.180D3 * t70 - 0.30D2 * t72) + 0.180D3 * t63 * lh + 0.45
     #D2 * t78 * pi) * t3 * t7 * t9 / 0.1440D4 + t4 * t7 * t86 / 0.16D2 
     #- (0.90D2 * t4 * t7 * (-t16 + t93 * t9) + t39) * t10 / 0.1440D4
      t103 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = 0.1D1 - x1
      t106 = 0.1D1 - x3
      t107 = KAPPA2(t105, x2, t106, 0.10D1, z)
      t108 = s * t107
      t109 = -t105
      t110 = t1 * t109
      t111 = -t106
      t113 = t108 * t110 * t111
      t115 = t108 * t110 * x3
      t116 = t1 * x1
      t117 = t108 * t116
      t118 = t107 ** 2
      t121 = t109 * x1
      t123 = s * t118 * t24 * t121 * x3
      t126 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t113, t117, -t1
     #15, 0.0D0, -t123)
      t129 = 0.1D1 / (-0.2D1 + t107) * t126 * t10 * t12
      t132 = FJET(XB1, XB2, s, t113, -t115, t117, 0.0D0, -t123, -t8 * t1
     #29 / 0.8D1)
      t137 = t2 * t109
      t138 = t2 * x1
      t139 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t137, t138, 0.
     #0D0, 0.0D0, 0.0D0)
      t144 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t137, t138, 0.
     #0D0, 0.0D0, 0.0D0)
      t146 = t109 ** 2
      t150 = log(0.4D1 * t43 * t25 * t17 * t146)
      t166 = -t8 * t139 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (t144 
     #- t150 * t139) - 0.180D3 * t35 * t36 * t139) * t12 / 0.720D3 - t8 
     #* t139 * t12 * t55 / 0.8D1
      t167 = FJET(XB1, XB2, s, -t137, 0.0D0, t138, 0.0D0, 0.0D0, t166)
      t169 = t2 * t111
      t170 = t2 * x3
      t171 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t169, 0.0D0, t
     #170, 0.0D0, 0.0D0)
      t176 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t169, 0.0D0, t
     #170, 0.0D0, 0.0D0)
      t180 = log(-0.4D1 * t90 * t26 * t111)
      t192 = -t8 * t171 * t10 * t12 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t17
     #6 + t180 * t171) - 0.180D3 * t35 * t36 * t171) * t10 / 0.1440D4
      t193 = FJET(XB1, XB2, s, -t169, t170, 0.0D0, 0.0D0, 0.0D0, t192)
      t196 = KAPPA2(t105, x2, 0.10D1, -t48, z)
      t197 = s * t196
      t198 = t197 * t110
      t200 = t197 * t116 * t48
      t202 = t197 * t116 * x4
      t203 = t196 ** 2
      t207 = s * t203 * t24 * t121 * x4
      t210 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t198, -t200, 0
     #.0D0, t202, -t207)
      t213 = 0.1D1 / (-0.2D1 + t196) * t210 * t12 * t55
      t216 = FJET(XB1, XB2, s, -t198, 0.0D0, -t200, t202, -t207, -t8 * t
     #213 / 0.8D1)
      rrgq2qght5s1em1 = t103 * t102 - t132 * pi * t36 * t129 / 0.8D1 + t
     #167 * t166 + t193 * t192 - t216 * pi * t36 * t213 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s1em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t45 = t2 * (-0.1D1 + x1)
      t46 = t2 * x1
      t47 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t45, t46, 0.0D0
     #, 0.0D0, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, -t45, 0.0D0, t46, 0.0D0, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t58 = t2 * (-0.1D1 + x3)
      t59 = t2 * x3
      t60 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t58, 0.0D0, t59
     #, 0.0D0, 0.0D0)
      t62 = t7 * t60 * t37
      t65 = FJET(XB1, XB2, s, -t58, t59, 0.0D0, 0.0D0, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrgq2qght5s1em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s1em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s1em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s1em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s2e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = t12 * t14
      t29 = t16 * x4
      t30 = -0.1D1 + x4
      t31 = t29 * t30
      t34 = log(-0.4D1 * t28 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = lh ** 2
      t59 = pi ** 2
      t61 = 0.180D3 * t57 - 0.30D2 * t59
      t62 = pi * t61
      t63 = t12 * t17
      t65 = log(0.4D1 * t63)
      t70 = t65 ** 2
      t73 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t77 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t88 = 0.60D2 * lh * t59 - 0.240D3 * zeta3 - 0.120D3 * t57 * lh
      t89 = pi * t88
      t90 = t44 * t25
      t91 = t89 * t90
      t102 = t4 * t7
      t103 = x3 * t8
      t104 = t103 * t11
      t107 = log(0.4D1 * t104 * t18)
      t109 = x4 * t30
      t113 = log(-0.4D1 * t104 * t17 * t109)
      t116 = 0.1D1 / x3
      t118 = t52 * t54
      t122 = t11 * t14
      t123 = t122 * t16
      t124 = t103 * t123
      t126 = log(0.4D1 * t124)
      t128 = t126 ** 2
      t140 = t62 * t90
      t145 = t7 * t22
      t153 = log(0.4D1 * t122 * t29)
      t154 = t153 ** 2
      t157 = log(-0.4D1 * t122 * t31)
      t158 = t157 ** 2
      t162 = t7 * t25
      t173 = t7 * t77
      t183 = log(0.4D1 * t123)
      t184 = t183 * pi
      t186 = t183 ** 2
      t187 = t186 * pi
      t191 = t186 * t183 * pi
      t211 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t215 = t59 ** 2
      t216 = t57 ** 2
      t229 = t186 ** 2
      t236 = x3 * t11
      t237 = t236 * t14
      t240 = log(-0.4D1 * t237 * t31)
      t242 = t240 ** 2
      t247 = log(0.4D1 * t236 * t18)
      t249 = t247 ** 2
      t268 = log(0.4D1 * t236 * t17)
      t273 = t268 ** 2
      t294 = (0.90D2 * t4 * t7 * (-t21 * t22 + t24 * t25 / 0.2D1 + t34 *
     # t22 - t36 * t25 / 0.2D1) - 0.180D3 * t43 * t44 * (-t21 * t25 + t3
     #4 * t25)) * t52 * t54 / 0.720D3 + (t62 * t44 * (t22 - t65 * t25) +
     # 0.90D2 * t4 * t7 * (t70 * t22 / 0.2D1 + t73 - t70 * t65 * t25 / 0
     #.6D1 - t65 * t77) + t91 - 0.180D3 * t43 * t44 * (-t65 * t22 + t70 
     #* t25 / 0.2D1 + t77)) * t52 / 0.720D3 + t102 * (-t107 * t25 + t113
     # * t25) * t116 * t118 / 0.8D1 + (0.90D2 * t4 * t7 * (-t126 * t22 +
     # t128 * t25 / 0.2D1 + t77) - 0.180D3 * t43 * t44 * (t22 - t126 * t
     #25) + t140) * t116 * t52 / 0.720D3 + ((-0.90D2 * t4 * t145 + 0.180
     #D3 * t43 * t90) * (-t154 / 0.2D1 + t158 / 0.2D1) - 0.90D2 * t4 * t
     #162 * (-t158 * t157 / 0.6D1 + t154 * t153 / 0.6D1) + (0.180D3 * t4
     #3 * t44 * t22 - t140 - 0.90D2 * t4 * t173) * (t153 - t157)) * t54 
     #/ 0.1440D4 + (t89 - t184 * t61 - 0.90D2 * t187 * lh - 0.15D2 * t19
     #1) * t3 * t145 / 0.1440D4 + (t62 + 0.180D3 * t184 * lh + 0.45D2 * 
     #t187) * t3 * t173 / 0.1440D4 + (-0.180D3 * t43 - 0.90D2 * t184) * 
     #t3 * t7 * t73 / 0.1440D4 + t4 * t7 * t211 / 0.16D2 + (pi * (t215 +
     # 0.60D2 * t216 + 0.480D3 * lh * zeta3 - 0.60D2 * t57 * t59) - t184
     # * t88 + t187 * t61 / 0.2D1 + 0.30D2 * t191 * lh + 0.15D2 / 0.4D1 
     #* t229 * pi) * t3 * t162 / 0.1440D4 + (0.90D2 * t4 * t7 * (t240 * 
     #t22 - t242 * t25 / 0.2D1 - t247 * t22 + t249 * t25 / 0.2D1) - 0.18
     #0D3 * t43 * t44 * (t240 * t25 - t247 * t25)) * t116 * t54 / 0.1440
     #D4 - (t62 * t44 * (-t22 + t268 * t25) + 0.90D2 * t4 * t7 * (-t273 
     #* t22 / 0.2D1 - t73 + t273 * t268 * t25 / 0.6D1 + t268 * t77) - t9
     #1 - 0.180D3 * t43 * t44 * (t268 * t22 - t273 * t25 / 0.2D1 - t77))
     # * t116 / 0.1440D4
      t295 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t294)
      t297 = 0.1D1 - x1
      t298 = 0.1D1 - x3
      t299 = KAPPA2(t297, x2, t298, 0.0D0, z)
      t300 = s * t299
      t301 = -t297
      t302 = t1 * t301
      t303 = -t298
      t304 = t302 * t303
      t305 = t300 * t304
      t306 = t302 * x3
      t307 = t300 * t306
      t308 = t1 * x1
      t309 = t300 * t308
      t310 = t299 ** 2
      t313 = t301 * x1
      t315 = s * t310 * t15 * t313 * t303
      t317 = 0.1D1 / (-0.2D1 + t299)
      t318 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t305, 0.0D0, -t
     #307, t309, t315)
      t319 = t317 * t318
      t320 = t103 * t122
      t321 = t301 ** 2
      t322 = t16 * t321
      t323 = t303 * x4
      t324 = t310 ** 2
      t329 = log(-0.4D1 * t320 * t322 * t323 * t324)
      t331 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t305, 0.0D0, -t
     #307, t309, t315)
      t337 = t43 * t3
      t339 = t7 * t317 * t331
      t349 = log(-0.4D1 * t320 * t322 * t303 * t324)
      t350 = t349 * t317
      t352 = t349 ** 2
      t356 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t305, 0.0D0, -t
     #307, t309, t315)
      t367 = t62 * t3
      t373 = (0.90D2 * t4 * t7 * (-t319 + t329 * t317 * t331) + 0.180D3 
     #* t337 * t339) * t116 * t118 / 0.720D3 + (0.90D2 * t4 * t7 * (t350
     # * t318 - t352 * t317 * t331 / 0.2D1 - t317 * t356) - 0.180D3 * t4
     #3 * t44 * (-t319 + t350 * t331) - t367 * t339) * t116 * t52 / 0.72
     #0D3
      t374 = FJET(XB1, XB2, s, t305, -t307, 0.0D0, t309, t315, t373)
      t376 = KAPPA2(t297, x2, t298, x4, z)
      t377 = s * t376
      t378 = t377 * t304
      t379 = t377 * t306
      t380 = t308 * x4
      t381 = t377 * t380
      t382 = t308 * t30
      t383 = t377 * t382
      t384 = t376 ** 2
      t389 = cos(t9)
      t392 = Sqrt(x3 * t303 * t109)
      t397 = s * t384 * t15 * t313 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t389 * t392)
      t399 = 0.1D1 / (-0.2D1 + t376)
      t400 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t378, t381, -t3
     #79, -t383, t397)
      t403 = t384 ** 2
      t408 = log(0.4D1 * t124 * t321 * t303 * t109 * t403)
      t410 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t378, t381, -t3
     #79, -t383, t397)
      t420 = 0.90D2 * t4 * t7 * (t399 * t400 - t408 * t399 * t410) - 0.1
     #80D3 * t337 * t7 * t399 * t410
      t424 = FJET(XB1, XB2, s, t378, -t379, t381, -t383, t397, t420 * t1
     #16 * t118 / 0.720D3)
      t430 = t2 * t303
      t431 = t2 * x3
      t435 = log(-0.4D1 * t104 * t17 * t323)
      t436 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t430, 0.0D0, t
     #431, 0.0D0, 0.0D0)
      t439 = t29 * t30 * t303
      t442 = log(0.4D1 * t320 * t439)
      t449 = t17 * t303
      t452 = log(-0.4D1 * t104 * t449)
      t453 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t430, 0.0D0, t
     #431, 0.0D0, 0.0D0)
      t455 = t452 ** 2
      t458 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t430, 0.0D0, t
     #431, 0.0D0, 0.0D0)
      t468 = t44 * t436
      t478 = log(-0.4D1 * t237 * t16 * t303 * x4)
      t480 = t478 ** 2
      t485 = log(0.4D1 * t237 * t439)
      t487 = t485 ** 2
      t506 = log(-0.4D1 * t236 * t449)
      t511 = t506 ** 2
      t514 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t430, 0.0D0, t
     #431, 0.0D0, 0.0D0)
      t534 = t102 * (t435 * t436 - t442 * t436) * t116 * t118 / 0.8D1 + 
     #(0.90D2 * t4 * t7 * (t452 * t453 - t455 * t436 / 0.2D1 - t458) - 0
     #.180D3 * t43 * t44 * (-t453 + t452 * t436) - t62 * t468) * t116 * 
     #t52 / 0.720D3 + (0.90D2 * t4 * t7 * (t478 * t453 - t480 * t436 / 0
     #.2D1 - t485 * t453 + t487 * t436 / 0.2D1) - 0.180D3 * t43 * t44 * 
     #(t478 * t436 - t485 * t436)) * t116 * t54 / 0.1440D4 - (-t62 * t44
     # * (-t453 + t506 * t436) - 0.90D2 * t4 * t7 * (-t511 * t453 / 0.2D
     #1 - t514 + t511 * t506 * t436 / 0.6D1 + t506 * t458) + t89 * t468 
     #+ 0.180D3 * t43 * t44 * (t506 * t453 - t511 * t436 / 0.2D1 - t458)
     #) * t116 / 0.1440D4
      t535 = FJET(XB1, XB2, s, -t430, t431, 0.0D0, 0.0D0, 0.0D0, t534)
      t537 = KAPPA2(t297, x2, 0.10D1, 0.0D0, z)
      t538 = s * t537
      t539 = t538 * t302
      t540 = t538 * t308
      t541 = t537 ** 2
      t545 = s * t541 * t15 * t301 * x1
      t546 = t541 ** 2
      t548 = t322 * x4 * t546
      t551 = log(0.4D1 * t28 * t548)
      t553 = 0.1D1 / (-0.2D1 + t537)
      t554 = t551 * t553
      t555 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t539, 0.0D0, 0
     #.0D0, t540, -t545)
      t557 = t551 ** 2
      t559 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t539, 0.0D0, 0
     #.0D0, t540, -t545)
      t562 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t539, 0.0D0, 0
     #.0D0, t540, -t545)
      t563 = t553 * t562
      t568 = t553 * t555
      t575 = t7 * t553 * t559
      t576 = t367 * t575
      t583 = log(0.4D1 * t28 * t322 * t546)
      t584 = t583 * t553
      t589 = t583 ** 2
      t590 = t589 * t553
      t593 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t539, 0.0D0, 0
     #.0D0, t540, -t545)
      t617 = log(0.4D1 * t320 * t548)
      t633 = log(0.4D1 * t104 * t17 * t321 * t546)
      t634 = t633 * t553
      t636 = t633 ** 2
      t653 = (0.90D2 * t4 * t7 * (-t554 * t555 + t557 * t553 * t559 / 0.
     #2D1 + t563) - 0.180D3 * t43 * t44 * (t568 - t554 * t559) + t576) *
     # t52 * t54 / 0.720D3 + (t62 * t44 * (t568 - t584 * t559) + 0.90D2 
     #* t4 * t7 * (t590 * t555 / 0.2D1 + t553 * t593 - t589 * t583 * t55
     #3 * t559 / 0.6D1 - t584 * t562) + t89 * t3 * t575 - 0.180D3 * t43 
     #* t44 * (-t584 * t555 + t590 * t559 / 0.2D1 + t563)) * t52 / 0.720
     #D3 + (0.90D2 * t4 * t7 * (t568 - t617 * t553 * t559) - 0.180D3 * t
     #337 * t575) * t116 * t118 / 0.720D3 + (0.90D2 * t4 * t7 * (-t634 *
     # t555 + t636 * t553 * t559 / 0.2D1 + t563) - 0.180D3 * t43 * t44 *
     # (t568 - t634 * t559) + t576) * t116 * t52 / 0.720D3
      t654 = FJET(XB1, XB2, s, -t539, 0.0D0, 0.0D0, t540, -t545, t653)
      t656 = KAPPA2(t297, x2, 0.10D1, x4, z)
      t657 = s * t656
      t658 = t657 * t302
      t659 = t657 * t380
      t660 = t657 * t382
      t661 = t656 ** 2
      t665 = s * t661 * t15 * t313 * t30
      t667 = t661 ** 2
      t672 = log(-0.4D1 * t63 * t321 * x4 * t30 * t667)
      t674 = 0.1D1 / (-0.2D1 + t656)
      t675 = t672 * t674
      t676 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t658, t659, 0.
     #0D0, -t660, t665)
      t678 = t672 ** 2
      t680 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t658, t659, 0.
     #0D0, -t660, t665)
      t683 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t658, t659, 0.
     #0D0, -t660, t665)
      t689 = t674 * t676
      t696 = t7 * t674 * t680
      t705 = log(-0.4D1 * t320 * t322 * t109 * t667)
      t718 = (0.90D2 * t4 * t7 * (t675 * t676 - t678 * t674 * t680 / 0.2
     #D1 - t674 * t683) - 0.180D3 * t43 * t44 * (-t689 + t675 * t680) - 
     #t367 * t696) * t52 * t54 / 0.720D3 + (0.90D2 * t4 * t7 * (-t689 + 
     #t705 * t674 * t680) + 0.180D3 * t337 * t696) * t116 * t118 / 0.720
     #D3
      t719 = FJET(XB1, XB2, s, -t658, 0.0D0, t659, -t660, t665, t718)
      rrgq2qght5s2e1 = t295 * t294 + t374 * t373 + t424 * t420 * t116 * 
     #t52 * t54 / 0.720D3 + t535 * t534 + t654 * t653 + t719 * t718

      end function



      doubleprecision function rrgq2qght5s2e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = t13 * t15
      t26 = t17 * x4
      t27 = -0.1D1 + x4
      t28 = t26 * t27
      t31 = log(-0.4D1 * t25 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t17
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t23
      t56 = 0.180D3 * t52 * t54
      t58 = 0.1D1 / x3
      t62 = t13 * t18
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t69 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t79 = lh ** 2
      t81 = pi ** 2
      t83 = 0.180D3 * t79 - 0.30D2 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t89 = t7 * t23
      t92 = log(0.4D1 * t42 * t26)
      t93 = t92 ** 2
      t96 = log(-0.4D1 * t42 * t28)
      t97 = t96 ** 2
      t103 = t7 * t40
      t113 = log(0.4D1 * t43)
      t114 = t113 * pi
      t117 = t113 ** 2
      t118 = t117 * pi
      t124 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t152 = x3 * t12
      t153 = t152 * t15
      t156 = log(-0.4D1 * t153 * t28)
      t160 = log(0.4D1 * t152 * t19)
      t169 = log(0.4D1 * t152 * t18)
      t171 = t169 ** 2
      t186 = t8 * (-t22 * t23 + t31 * t23) * t34 * t36 / 0.8D1 + (0.90D2
     # * t4 * t7 * (t40 - t46 * t23) - t56) * t58 * t34 / 0.720D3 + (0.9
     #0D2 * t4 * t7 * (-t64 * t40 + t66 * t23 / 0.2D1 + t69) - 0.180D3 *
     # t52 * t53 * (t40 - t64 * t23) + t85) * t34 / 0.720D3 + (-0.90D2 *
     # t4 * t89 * (-t93 / 0.2D1 + t97 / 0.2D1) + (-0.90D2 * t4 * t103 + 
     #t56) * (t92 - t96)) * t36 / 0.1440D4 + (t84 + 0.180D3 * t114 * lh 
     #+ 0.45D2 * t118) * t3 * t103 / 0.1440D4 + t4 * t7 * t124 / 0.16D2 
     #+ (pi * (0.60D2 * lh * t81 - 0.240D3 * zeta3 - 0.120D3 * t79 * lh)
     # - t114 * t83 - 0.90D2 * t118 * lh - 0.15D2 * t117 * t113 * pi) * 
     #t3 * t89 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t114) * t3 * t7 *
     # t69 / 0.1440D4 + t8 * (t156 * t23 - t160 * t23) * t58 * t36 / 0.1
     #6D2 - (0.90D2 * t4 * t7 * (t169 * t40 - t171 * t23 / 0.2D1 - t69) 
     #- 0.180D3 * t52 * t53 * (-t40 + t169 * t23) - t85) * t58 / 0.1440D
     #4
      t187 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t186)
      t189 = 0.1D1 - x1
      t190 = 0.1D1 - x3
      t191 = KAPPA2(t189, x2, t190, 0.0D0, z)
      t192 = s * t191
      t193 = -t189
      t194 = t1 * t193
      t195 = -t190
      t196 = t194 * t195
      t197 = t192 * t196
      t198 = t194 * x3
      t199 = t192 * t198
      t200 = t1 * x1
      t201 = t192 * t200
      t202 = t191 ** 2
      t205 = t193 * x1
      t207 = s * t202 * t16 * t205 * t195
      t209 = 0.1D1 / (-0.2D1 + t191)
      t210 = t7 * t209
      t212 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t197, 0.0D0, -t
     #199, t201, t207)
      t214 = t34 * t36
      t218 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t197, 0.0D0, -t
     #199, t201, t207)
      t221 = t193 ** 2
      t222 = t17 * t221
      t223 = t202 ** 2
      t228 = log(-0.4D1 * t41 * t42 * t222 * t195 * t223)
      t235 = t52 * t3
      t243 = -t4 * t210 * t212 * t58 * t214 / 0.8D1 + (0.90D2 * t4 * t7 
     #* (-t209 * t218 + t228 * t209 * t212) + 0.180D3 * t235 * t210 * t2
     #12) * t58 * t34 / 0.720D3
      t244 = FJET(XB1, XB2, s, t197, -t199, 0.0D0, t201, t207, t243)
      t246 = KAPPA2(t189, x2, t190, x4, z)
      t247 = s * t246
      t248 = t247 * t196
      t249 = t247 * t198
      t250 = t200 * x4
      t251 = t247 * t250
      t252 = t200 * t27
      t253 = t247 * t252
      t254 = t246 ** 2
      t259 = cos(t10)
      t263 = Sqrt(x3 * t195 * x4 * t27)
      t268 = s * t254 * t16 * t205 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t259 * t263)
      t270 = 0.1D1 / (-0.2D1 + t246)
      t273 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t248, t251, -t2
     #49, -t253, t268)
      t278 = FJET(XB1, XB2, s, t248, -t249, t251, -t253, t268, t4 * t7 *
     # t270 * t273 * t58 * t214 / 0.8D1)
      t287 = t2 * t195
      t288 = t2 * x3
      t289 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t287, 0.0D0, t
     #288, 0.0D0, 0.0D0)
      t290 = t41 * t12
      t291 = t18 * t195
      t294 = log(-0.4D1 * t290 * t291)
      t295 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t287, 0.0D0, t
     #288, 0.0D0, 0.0D0)
      t301 = t53 * t295
      t312 = log(-0.4D1 * t153 * t17 * t195 * x4)
      t318 = log(0.4D1 * t153 * t26 * t27 * t195)
      t327 = log(-0.4D1 * t152 * t291)
      t329 = t327 ** 2
      t332 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t287, 0.0D0, t
     #288, 0.0D0, 0.0D0)
      t346 = (0.90D2 * t4 * t7 * (-t289 + t295 * t294) + 0.180D3 * t52 *
     # t301) * t58 * t34 / 0.720D3 + t8 * (t312 * t295 - t318 * t295) * 
     #t58 * t36 / 0.16D2 - (-0.90D2 * t4 * t7 * (t327 * t289 - t329 * t2
     #95 / 0.2D1 - t332) + 0.180D3 * t52 * t53 * (-t289 + t327 * t295) +
     # t84 * t301) * t58 / 0.1440D4
      t347 = FJET(XB1, XB2, s, -t287, t288, 0.0D0, 0.0D0, 0.0D0, t346)
      t349 = KAPPA2(t189, x2, 0.10D1, 0.0D0, z)
      t350 = s * t349
      t351 = t350 * t194
      t352 = t350 * t200
      t353 = t349 ** 2
      t357 = s * t353 * t16 * t193 * x1
      t359 = 0.1D1 / (-0.2D1 + t349)
      t360 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t351, 0.0D0, 0
     #.0D0, t352, -t357)
      t361 = t359 * t360
      t362 = t353 ** 2
      t367 = log(0.4D1 * t25 * t222 * x4 * t362)
      t369 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, 0.0D0, 0
     #.0D0, t352, -t357)
      t375 = t7 * t359
      t376 = t375 * t369
      t378 = 0.180D3 * t235 * t376
      t392 = log(0.4D1 * t290 * t18 * t221 * t362)
      t406 = log(0.4D1 * t25 * t222 * t362)
      t407 = t406 * t359
      t409 = t406 ** 2
      t413 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t351, 0.0D0, 0
     #.0D0, t352, -t357)
      t429 = (0.90D2 * t4 * t7 * (t361 - t367 * t359 * t369) - t378) * t
     #34 * t36 / 0.720D3 + t4 * t375 * t369 * t58 * t214 / 0.8D1 + (0.90
     #D2 * t4 * t7 * (t361 - t392 * t359 * t369) - t378) * t58 * t34 / 0
     #.720D3 + (0.90D2 * t4 * t7 * (-t407 * t360 + t409 * t359 * t369 / 
     #0.2D1 + t359 * t413) - 0.180D3 * t52 * t53 * (t361 - t407 * t369) 
     #+ t84 * t3 * t376) * t34 / 0.720D3
      t430 = FJET(XB1, XB2, s, -t351, 0.0D0, 0.0D0, t352, -t357, t429)
      t432 = KAPPA2(t189, x2, 0.10D1, x4, z)
      t433 = s * t432
      t434 = t433 * t194
      t435 = t433 * t250
      t436 = t433 * t252
      t437 = t432 ** 2
      t441 = s * t437 * t16 * t205 * t27
      t443 = 0.1D1 / (-0.2D1 + t432)
      t444 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t434, t435, 0.
     #0D0, -t436, t441)
      t447 = t437 ** 2
      t452 = log(-0.4D1 * t62 * t221 * x4 * t27 * t447)
      t454 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t434, t435, 0.
     #0D0, -t436, t441)
      t460 = t7 * t443
      t473 = (0.90D2 * t4 * t7 * (-t443 * t444 + t452 * t443 * t454) + 0
     #.180D3 * t235 * t460 * t454) * t34 * t36 / 0.720D3 - t4 * t460 * t
     #454 * t58 * t214 / 0.8D1
      t474 = FJET(XB1, XB2, s, -t434, 0.0D0, t435, -t436, t441, t473)
      rrgq2qght5s2e0 = t187 * t186 + t244 * t243 + t278 * pi * t53 * t27
     #0 * t273 * t58 * t34 * t36 / 0.8D1 + t347 * t346 + t430 * t429 + t
     #474 * t473

      end function



      doubleprecision function rrgq2qght5s2em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t21 = t17 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t21 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t47 = log(0.4D1 * t43 * t44)
      t48 = -0.1D1 + x4
      t52 = log(-0.4D1 * t43 * t44 * t48)
      t55 = 0.1D1 / x4
      t62 = log(0.4D1 * t43 * t25)
      t63 = t62 * pi
      t70 = lh ** 2
      t72 = pi ** 2
      t78 = t62 ** 2
      t86 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t90 = x3 * t20
      t93 = log(0.4D1 * t90 * t26)
      t102 = t8 * t9 * t10 * t12 / 0.8D1 + (0.90D2 * t4 * t7 * (t16 - t2
     #9 * t9) - t39) * t12 / 0.720D3 - t8 * t9 * (t47 - t52) * t55 / 0.1
     #6D2 + (-0.180D3 * t35 - 0.90D2 * t63) * t3 * t7 * t16 / 0.1440D4 +
     # (pi * (0.180D3 * t70 - 0.30D2 * t72) + 0.180D3 * t63 * lh + 0.45D
     #2 * t78 * pi) * t3 * t7 * t9 / 0.1440D4 + t4 * t7 * t86 / 0.16D2 -
     # (0.90D2 * t4 * t7 * (-t16 + t93 * t9) + t39) * t10 / 0.1440D4
      t103 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = 0.1D1 - x1
      t106 = 0.1D1 - x3
      t107 = KAPPA2(t105, x2, t106, 0.0D0, z)
      t108 = s * t107
      t109 = -t105
      t110 = t1 * t109
      t111 = -t106
      t113 = t108 * t110 * t111
      t115 = t108 * t110 * x3
      t116 = t1 * x1
      t117 = t108 * t116
      t118 = t107 ** 2
      t121 = t109 * x1
      t123 = s * t118 * t24 * t121 * t111
      t126 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t113, 0.0D0, -t
     #115, t117, t123)
      t128 = t10 * t12
      t129 = 0.1D1 / (-0.2D1 + t107) * t126 * t128
      t132 = FJET(XB1, XB2, s, t113, -t115, 0.0D0, t117, t123, -t8 * t12
     #9 / 0.8D1)
      t137 = t2 * t111
      t138 = t2 * x3
      t139 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t137, 0.0D0, t
     #138, 0.0D0, 0.0D0)
      t144 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t137, 0.0D0, t
     #138, 0.0D0, 0.0D0)
      t148 = log(-0.4D1 * t90 * t26 * t111)
      t160 = -t8 * t139 * t10 * t12 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t14
     #4 + t148 * t139) - 0.180D3 * t35 * t36 * t139) * t10 / 0.1440D4
      t161 = FJET(XB1, XB2, s, -t137, t138, 0.0D0, 0.0D0, 0.0D0, t160)
      t163 = KAPPA2(t105, x2, 0.10D1, 0.0D0, z)
      t164 = s * t163
      t165 = t164 * t110
      t166 = t164 * t116
      t167 = t163 ** 2
      t171 = s * t167 * t24 * t109 * x1
      t173 = 0.1D1 / (-0.2D1 + t163)
      t174 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t165, 0.0D0, 0
     #.0D0, t166, -t171)
      t175 = t173 * t174
      t179 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t165, 0.0D0, 0
     #.0D0, t166, -t171)
      t182 = t109 ** 2
      t184 = t167 ** 2
      t188 = log(0.4D1 * t21 * t23 * t25 * t182 * t184)
      t203 = t12 * t55
      t207 = t8 * t175 * t128 / 0.8D1 + (0.90D2 * t4 * t7 * (t173 * t179
     # - t188 * t173 * t174) - 0.180D3 * t35 * t3 * t7 * t173 * t174) * 
     #t12 / 0.720D3 + t8 * t175 * t203 / 0.8D1
      t208 = FJET(XB1, XB2, s, -t165, 0.0D0, 0.0D0, t166, -t171, t207)
      t210 = KAPPA2(t105, x2, 0.10D1, x4, z)
      t211 = s * t210
      t212 = t211 * t110
      t214 = t211 * t116 * x4
      t216 = t211 * t116 * t48
      t217 = t210 ** 2
      t221 = s * t217 * t24 * t121 * t48
      t224 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t212, t214, 0.
     #0D0, -t216, t221)
      t226 = 0.1D1 / (-0.2D1 + t210) * t224 * t203
      t229 = FJET(XB1, XB2, s, -t212, 0.0D0, t214, -t216, t221, -t8 * t2
     #26 / 0.8D1)
      rrgq2qght5s2em1 = t103 * t102 - t132 * pi * t36 * t129 / 0.8D1 + t
     #161 * t160 + t208 * t207 - t229 * pi * t36 * t226 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s2em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t44 = 0.1D1 - x1
      t45 = KAPPA2(t44, x2, 0.10D1, 0.0D0, z)
      t46 = s * t45
      t47 = -t44
      t49 = t46 * t1 * t47
      t51 = t46 * t1 * x1
      t52 = t45 ** 2
      t56 = s * t52 * t26 * t47 * x1
      t59 = 0.1D1 / (-0.2D1 + t45)
      t60 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t49, 0.0D0, 0.0
     #D0, t51, -t56)
      t65 = FJET(XB1, XB2, s, -t49, 0.0D0, 0.0D0, t51, -t56, t4 * t7 * t
     #59 * t60 * t10 / 0.8D1)
      t74 = t2 * (-0.1D1 + x3)
      t75 = t2 * x3
      t76 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t74, 0.0D0, t75
     #, 0.0D0, 0.0D0)
      t78 = t7 * t76 * t37
      t81 = FJET(XB1, XB2, s, -t74, t75, 0.0D0, 0.0D0, 0.0D0, -t4 * t78 
     #/ 0.16D2)
      rrgq2qght5s2em2 = t42 * t41 + t65 * pi * t3 * t7 * t59 * t60 * t10
     # / 0.8D1 - t81 * pi * t3 * t78 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s2em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s2em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s2em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s2em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s3e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t28 = t12 * t14
      t29 = t16 * x4
      t30 = -0.1D1 + x4
      t31 = t29 * t30
      t34 = log(-0.4D1 * t28 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = lh ** 2
      t59 = pi ** 2
      t61 = 0.180D3 * t57 - 0.30D2 * t59
      t62 = pi * t61
      t63 = t12 * t17
      t65 = log(0.4D1 * t63)
      t70 = t65 ** 2
      t73 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t77 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t88 = 0.60D2 * lh * t59 - 0.240D3 * zeta3 - 0.120D3 * t57 * lh
      t89 = pi * t88
      t90 = t44 * t25
      t91 = t89 * t90
      t102 = t4 * t7
      t103 = x3 * t8
      t104 = t103 * t11
      t105 = x4 * t30
      t109 = log(-0.4D1 * t104 * t17 * t105)
      t113 = log(0.4D1 * t104 * t18)
      t116 = 0.1D1 / x3
      t118 = t52 * t54
      t122 = t11 * t14
      t123 = t122 * t16
      t124 = t103 * t123
      t126 = log(0.4D1 * t124)
      t128 = t126 ** 2
      t140 = t62 * t90
      t145 = t7 * t22
      t153 = log(0.4D1 * t122 * t29)
      t154 = t153 ** 2
      t157 = log(-0.4D1 * t122 * t31)
      t158 = t157 ** 2
      t162 = t7 * t25
      t173 = t7 * t77
      t183 = log(0.4D1 * t123)
      t184 = t183 * pi
      t186 = t183 ** 2
      t187 = t186 * pi
      t191 = t186 * t183 * pi
      t211 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t215 = t59 ** 2
      t216 = t57 ** 2
      t229 = t186 ** 2
      t236 = x3 * t11
      t239 = log(0.4D1 * t236 * t18)
      t241 = t239 ** 2
      t244 = t236 * t14
      t247 = log(-0.4D1 * t244 * t31)
      t249 = t247 ** 2
      t268 = log(0.4D1 * t236 * t17)
      t273 = t268 ** 2
      t294 = (0.90D2 * t4 * t7 * (-t21 * t22 + t24 * t25 / 0.2D1 + t34 *
     # t22 - t36 * t25 / 0.2D1) - 0.180D3 * t43 * t44 * (-t21 * t25 + t3
     #4 * t25)) * t52 * t54 / 0.720D3 + (t62 * t44 * (t22 - t65 * t25) +
     # 0.90D2 * t4 * t7 * (t70 * t22 / 0.2D1 + t73 - t70 * t65 * t25 / 0
     #.6D1 - t65 * t77) + t91 - 0.180D3 * t43 * t44 * (-t65 * t22 + t70 
     #* t25 / 0.2D1 + t77)) * t52 / 0.720D3 - t102 * (-t109 * t25 + t113
     # * t25) * t116 * t118 / 0.8D1 + (0.90D2 * t4 * t7 * (-t126 * t22 +
     # t128 * t25 / 0.2D1 + t77) - 0.180D3 * t43 * t44 * (t22 - t126 * t
     #25) + t140) * t116 * t52 / 0.720D3 + ((-0.90D2 * t4 * t145 + 0.180
     #D3 * t43 * t90) * (-t154 / 0.2D1 + t158 / 0.2D1) - 0.90D2 * t4 * t
     #162 * (-t158 * t157 / 0.6D1 + t154 * t153 / 0.6D1) + (0.180D3 * t4
     #3 * t44 * t22 - t140 - 0.90D2 * t4 * t173) * (t153 - t157)) * t54 
     #/ 0.1440D4 + (t89 - t184 * t61 - 0.90D2 * t187 * lh - 0.15D2 * t19
     #1) * t3 * t145 / 0.1440D4 + (t62 + 0.180D3 * t184 * lh + 0.45D2 * 
     #t187) * t3 * t173 / 0.1440D4 + (-0.180D3 * t43 - 0.90D2 * t184) * 
     #t3 * t7 * t73 / 0.1440D4 + t4 * t7 * t211 / 0.16D2 + (pi * (t215 +
     # 0.60D2 * t216 + 0.480D3 * lh * zeta3 - 0.60D2 * t57 * t59) - t184
     # * t88 + t187 * t61 / 0.2D1 + 0.30D2 * t191 * lh + 0.15D2 / 0.4D1 
     #* t229 * pi) * t3 * t162 / 0.1440D4 + (0.90D2 * t4 * t7 * (-t239 *
     # t22 + t241 * t25 / 0.2D1 + t247 * t22 - t249 * t25 / 0.2D1) - 0.1
     #80D3 * t43 * t44 * (-t239 * t25 + t247 * t25)) * t116 * t54 / 0.14
     #40D4 - (t62 * t44 * (-t22 + t268 * t25) + 0.90D2 * t4 * t7 * (-t27
     #3 * t22 / 0.2D1 - t73 + t273 * t268 * t25 / 0.6D1 + t268 * t77) - 
     #t91 - 0.180D3 * t43 * t44 * (t268 * t22 - t273 * t25 / 0.2D1 - t77
     #)) * t116 / 0.1440D4
      t295 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t294)
      t297 = 0.1D1 - x1
      t298 = KAPPA2(t297, x2, 0.0D0, 0.10D1, z)
      t299 = s * t298
      t300 = -t297
      t301 = t1 * t300
      t302 = t299 * t301
      t303 = t1 * x1
      t304 = t299 * t303
      t305 = t298 ** 2
      t309 = s * t305 * t15 * t300 * x1
      t310 = t300 ** 2
      t311 = t16 * t310
      t312 = t305 ** 2
      t314 = t311 * x4 * t312
      t317 = log(0.4D1 * t28 * t314)
      t319 = 0.1D1 / (-0.2D1 + t298)
      t320 = t317 * t319
      t321 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t304, -t
     #302, 0.0D0, -t309)
      t323 = t317 ** 2
      t325 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t304, -t
     #302, 0.0D0, -t309)
      t328 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t304, -t
     #302, 0.0D0, -t309)
      t329 = t319 * t328
      t334 = t319 * t321
      t340 = t62 * t3
      t342 = t7 * t319 * t325
      t343 = t340 * t342
      t350 = log(0.4D1 * t28 * t311 * t312)
      t351 = t350 * t319
      t356 = t350 ** 2
      t357 = t356 * t319
      t360 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t304, -t
     #302, 0.0D0, -t309)
      t382 = t103 * t122
      t385 = log(0.4D1 * t382 * t314)
      t392 = t43 * t3
      t402 = log(0.4D1 * t104 * t17 * t310 * t312)
      t403 = t402 * t319
      t405 = t402 ** 2
      t422 = (0.90D2 * t4 * t7 * (-t320 * t321 + t323 * t319 * t325 / 0.
     #2D1 + t329) - 0.180D3 * t43 * t44 * (t334 - t320 * t325) + t343) *
     # t52 * t54 / 0.720D3 + (t62 * t44 * (t334 - t351 * t325) + 0.90D2 
     #* t4 * t7 * (t357 * t321 / 0.2D1 + t319 * t360 - t356 * t350 * t31
     #9 * t325 / 0.6D1 - t351 * t328) + t89 * t3 * t342 - 0.180D3 * t43 
     #* t44 * (-t351 * t321 + t357 * t325 / 0.2D1 + t329)) * t52 / 0.720
     #D3 - (0.90D2 * t4 * t7 * (-t334 + t385 * t319 * t325) + 0.180D3 * 
     #t392 * t342) * t116 * t118 / 0.720D3 + (0.90D2 * t4 * t7 * (-t403 
     #* t321 + t405 * t319 * t325 / 0.2D1 + t329) - 0.180D3 * t43 * t44 
     #* (t334 - t403 * t325) + t343) * t116 * t52 / 0.720D3
      t423 = FJET(XB1, XB2, s, 0.0D0, -t302, t304, 0.0D0, -t309, t422)
      t425 = -t30
      t426 = KAPPA2(t297, x2, 0.0D0, t425, z)
      t427 = s * t426
      t428 = t427 * t301
      t429 = t303 * t30
      t430 = t427 * t429
      t431 = t303 * x4
      t432 = t427 * t431
      t433 = t426 ** 2
      t436 = t300 * x1
      t438 = s * t433 * t15 * t436 * t30
      t440 = t433 ** 2
      t445 = log(-0.4D1 * t63 * t310 * x4 * t30 * t440)
      t447 = 0.1D1 / (-0.2D1 + t426)
      t448 = t445 * t447
      t449 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t430, -
     #t428, t432, t438)
      t451 = t445 ** 2
      t453 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t430, -
     #t428, t432, t438)
      t456 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t430, -
     #t428, t432, t438)
      t462 = t447 * t449
      t469 = t7 * t447 * t453
      t478 = log(-0.4D1 * t382 * t311 * t105 * t440)
      t491 = (-0.90D2 * t4 * t7 * (-t448 * t449 + t451 * t447 * t453 / 0
     #.2D1 + t447 * t456) + 0.180D3 * t43 * t44 * (t462 - t448 * t453) -
     # t340 * t469) * t52 * t54 / 0.720D3 - (0.90D2 * t4 * t7 * (t462 - 
     #t478 * t447 * t453) - 0.180D3 * t392 * t469) * t116 * t118 / 0.720
     #D3
      t492 = FJET(XB1, XB2, s, 0.0D0, -t428, -t430, t432, t438, t491)
      t494 = t2 * x3
      t495 = -0.1D1 + x3
      t496 = t2 * t495
      t498 = t29 * t30 * t495
      t501 = log(0.4D1 * t382 * t498)
      t502 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t494, 0.0D0, -t
     #496, 0.0D0, 0.0D0)
      t504 = t495 * x4
      t508 = log(-0.4D1 * t104 * t17 * t504)
      t515 = t17 * t495
      t518 = log(-0.4D1 * t104 * t515)
      t519 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t494, 0.0D0, -t
     #496, 0.0D0, 0.0D0)
      t521 = t518 ** 2
      t524 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t494, 0.0D0, -t
     #496, 0.0D0, 0.0D0)
      t534 = t44 * t502
      t542 = log(0.4D1 * t244 * t498)
      t544 = t542 ** 2
      t551 = log(-0.4D1 * t244 * t16 * t495 * x4)
      t553 = t551 ** 2
      t572 = log(-0.4D1 * t236 * t515)
      t577 = t572 ** 2
      t580 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t494, 0.0D0, -t
     #496, 0.0D0, 0.0D0)
      t600 = -t102 * (t501 * t502 - t508 * t502) * t116 * t118 / 0.8D1 +
     # (0.90D2 * t4 * t7 * (t518 * t519 - t521 * t502 / 0.2D1 - t524) - 
     #0.180D3 * t43 * t44 * (-t519 + t518 * t502) - t62 * t534) * t116 *
     # t52 / 0.720D3 + (0.90D2 * t4 * t7 * (-t542 * t519 + t544 * t502 /
     # 0.2D1 + t551 * t519 - t553 * t502 / 0.2D1) - 0.180D3 * t43 * t44 
     #* (-t542 * t502 + t551 * t502)) * t116 * t54 / 0.1440D4 - (t62 * t
     #44 * (t519 - t572 * t502) + 0.90D2 * t4 * t7 * (t577 * t519 / 0.2D
     #1 + t580 - t577 * t572 * t502 / 0.6D1 - t572 * t524) + t89 * t534 
     #- 0.180D3 * t43 * t44 * (-t572 * t519 + t577 * t502 / 0.2D1 + t524
     #)) * t116 / 0.1440D4
      t601 = FJET(XB1, XB2, s, t494, -t496, 0.0D0, 0.0D0, 0.0D0, t600)
      t603 = KAPPA2(t297, x2, x3, 0.10D1, z)
      t604 = s * t603
      t605 = t301 * x3
      t606 = t604 * t605
      t607 = t301 * t495
      t608 = t604 * t607
      t609 = t604 * t303
      t610 = t603 ** 2
      t614 = s * t610 * t15 * t436 * t495
      t616 = 0.1D1 / (-0.2D1 + t603)
      t617 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t606, t609, t6
     #08, 0.0D0, t614)
      t618 = t616 * t617
      t619 = t610 ** 2
      t624 = log(-0.4D1 * t382 * t311 * t504 * t619)
      t626 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t606, t609, t6
     #08, 0.0D0, t614)
      t633 = t7 * t616 * t626
      t643 = log(-0.4D1 * t382 * t311 * t495 * t619)
      t644 = t643 * t616
      t646 = t643 ** 2
      t650 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t606, t609, t6
     #08, 0.0D0, t614)
      t666 = -(0.90D2 * t4 * t7 * (t618 - t624 * t616 * t626) - 0.180D3 
     #* t392 * t633) * t116 * t118 / 0.720D3 + (-0.90D2 * t4 * t7 * (-t6
     #44 * t617 + t646 * t616 * t626 / 0.2D1 + t616 * t650) + 0.180D3 * 
     #t43 * t44 * (t618 - t644 * t626) - t340 * t633) * t116 * t52 / 0.7
     #20D3
      t667 = FJET(XB1, XB2, s, -t606, t608, t609, 0.0D0, t614, t666)
      t669 = KAPPA2(t297, x2, x3, t425, z)
      t670 = s * t669
      t671 = t670 * t605
      t672 = t670 * t607
      t673 = t670 * t429
      t674 = t670 * t431
      t675 = t669 ** 2
      t680 = cos(t9)
      t683 = Sqrt(x3 * t495 * t105)
      t688 = s * t675 * t15 * t436 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t680 * t683)
      t690 = 0.1D1 / (-0.2D1 + t669)
      t691 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t671, -t673, t
     #672, t674, t688)
      t694 = t675 ** 2
      t699 = log(0.4D1 * t124 * t310 * t495 * t105 * t694)
      t701 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t671, -t673, t
     #672, t674, t688)
      t711 = -0.90D2 * t4 * t7 * (t690 * t691 - t699 * t690 * t701) + 0.
     #180D3 * t392 * t7 * t690 * t701
      t715 = FJET(XB1, XB2, s, -t671, t672, -t673, t674, t688, -t711 * t
     #116 * t118 / 0.720D3)
      rrgq2qght5s3e1 = t295 * t294 + t423 * t422 + t492 * t491 + t601 * 
     #t600 + t667 * t666 - t715 * t711 * t116 * t52 * t54 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s3e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t25 = t13 * t15
      t26 = t17 * x4
      t27 = -0.1D1 + x4
      t28 = t26 * t27
      t31 = log(-0.4D1 * t25 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t17
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t23
      t56 = 0.180D3 * t52 * t54
      t58 = 0.1D1 / x3
      t62 = t13 * t18
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t69 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t79 = lh ** 2
      t81 = pi ** 2
      t83 = 0.180D3 * t79 - 0.30D2 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t89 = t7 * t23
      t92 = log(0.4D1 * t42 * t26)
      t93 = t92 ** 2
      t96 = log(-0.4D1 * t42 * t28)
      t97 = t96 ** 2
      t103 = t7 * t40
      t113 = log(0.4D1 * t43)
      t114 = t113 * pi
      t117 = t113 ** 2
      t118 = t117 * pi
      t124 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t152 = x3 * t12
      t155 = log(0.4D1 * t152 * t19)
      t157 = t152 * t15
      t160 = log(-0.4D1 * t157 * t28)
      t169 = log(0.4D1 * t152 * t18)
      t171 = t169 ** 2
      t186 = t8 * (-t22 * t23 + t31 * t23) * t34 * t36 / 0.8D1 + (0.90D2
     # * t4 * t7 * (t40 - t46 * t23) - t56) * t58 * t34 / 0.720D3 + (0.9
     #0D2 * t4 * t7 * (-t64 * t40 + t66 * t23 / 0.2D1 + t69) - 0.180D3 *
     # t52 * t53 * (t40 - t64 * t23) + t85) * t34 / 0.720D3 + (-0.90D2 *
     # t4 * t89 * (-t93 / 0.2D1 + t97 / 0.2D1) + (-0.90D2 * t4 * t103 + 
     #t56) * (t92 - t96)) * t36 / 0.1440D4 + (t84 + 0.180D3 * t114 * lh 
     #+ 0.45D2 * t118) * t3 * t103 / 0.1440D4 + t4 * t7 * t124 / 0.16D2 
     #+ (pi * (0.60D2 * lh * t81 - 0.240D3 * zeta3 - 0.120D3 * t79 * lh)
     # - t114 * t83 - 0.90D2 * t118 * lh - 0.15D2 * t117 * t113 * pi) * 
     #t3 * t89 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t114) * t3 * t7 *
     # t69 / 0.1440D4 + t8 * (-t155 * t23 + t160 * t23) * t58 * t36 / 0.
     #16D2 - (0.90D2 * t4 * t7 * (t169 * t40 - t171 * t23 / 0.2D1 - t69)
     # - 0.180D3 * t52 * t53 * (-t40 + t169 * t23) - t85) * t58 / 0.1440
     #D4
      t187 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t186)
      t189 = 0.1D1 - x1
      t190 = KAPPA2(t189, x2, 0.0D0, 0.10D1, z)
      t191 = s * t190
      t192 = -t189
      t193 = t1 * t192
      t194 = t191 * t193
      t195 = t1 * x1
      t196 = t191 * t195
      t197 = t190 ** 2
      t201 = s * t197 * t16 * t192 * x1
      t203 = 0.1D1 / (-0.2D1 + t190)
      t204 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t196, -t
     #194, 0.0D0, -t201)
      t205 = t203 * t204
      t206 = t192 ** 2
      t207 = t17 * t206
      t208 = t197 ** 2
      t213 = log(0.4D1 * t25 * t207 * x4 * t208)
      t215 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t196, -t
     #194, 0.0D0, -t201)
      t221 = t52 * t3
      t222 = t7 * t203
      t223 = t222 * t215
      t225 = 0.180D3 * t221 * t223
      t232 = t34 * t36
      t236 = t41 * t12
      t241 = log(0.4D1 * t236 * t18 * t206 * t208)
      t255 = log(0.4D1 * t25 * t207 * t208)
      t256 = t255 * t203
      t258 = t255 ** 2
      t262 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t196, -t
     #194, 0.0D0, -t201)
      t278 = (0.90D2 * t4 * t7 * (t205 - t213 * t203 * t215) - t225) * t
     #34 * t36 / 0.720D3 + t4 * t222 * t215 * t58 * t232 / 0.8D1 + (0.90
     #D2 * t4 * t7 * (t205 - t241 * t203 * t215) - t225) * t58 * t34 / 0
     #.720D3 + (0.90D2 * t4 * t7 * (-t256 * t204 + t258 * t203 * t215 / 
     #0.2D1 + t203 * t262) - 0.180D3 * t52 * t53 * (t205 - t256 * t215) 
     #+ t84 * t3 * t223) * t34 / 0.720D3
      t279 = FJET(XB1, XB2, s, 0.0D0, -t194, t196, 0.0D0, -t201, t278)
      t281 = -t27
      t282 = KAPPA2(t189, x2, 0.0D0, t281, z)
      t283 = s * t282
      t284 = t283 * t193
      t285 = t195 * t27
      t286 = t283 * t285
      t287 = t195 * x4
      t288 = t283 * t287
      t289 = t282 ** 2
      t292 = t192 * x1
      t294 = s * t289 * t16 * t292 * t27
      t296 = 0.1D1 / (-0.2D1 + t282)
      t297 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t286, -
     #t284, t288, t294)
      t300 = t289 ** 2
      t305 = log(-0.4D1 * t62 * t206 * x4 * t27 * t300)
      t307 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t286, -
     #t284, t288, t294)
      t313 = t7 * t296
      t326 = (-0.90D2 * t4 * t7 * (t296 * t297 - t305 * t296 * t307) + 0
     #.180D3 * t221 * t313 * t307) * t34 * t36 / 0.720D3 - t4 * t313 * t
     #307 * t58 * t232 / 0.8D1
      t327 = FJET(XB1, XB2, s, 0.0D0, -t284, -t286, t288, t294, t326)
      t329 = t2 * x3
      t330 = -0.1D1 + x3
      t331 = t2 * t330
      t332 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t329, 0.0D0, -t
     #331, 0.0D0, 0.0D0)
      t333 = t18 * t330
      t336 = log(-0.4D1 * t236 * t333)
      t337 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t329, 0.0D0, -t
     #331, 0.0D0, 0.0D0)
      t343 = t53 * t337
      t354 = log(0.4D1 * t157 * t26 * t27 * t330)
      t360 = log(-0.4D1 * t157 * t17 * t330 * x4)
      t369 = log(-0.4D1 * t152 * t333)
      t371 = t369 ** 2
      t374 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t329, 0.0D0, -t
     #331, 0.0D0, 0.0D0)
      t388 = (0.90D2 * t4 * t7 * (-t332 + t336 * t337) + 0.180D3 * t52 *
     # t343) * t58 * t34 / 0.720D3 + t8 * (-t354 * t337 + t360 * t337) *
     # t58 * t36 / 0.16D2 - (0.90D2 * t4 * t7 * (-t369 * t332 + t371 * t
     #337 / 0.2D1 + t374) - 0.180D3 * t52 * t53 * (t332 - t369 * t337) +
     # t84 * t343) * t58 / 0.1440D4
      t389 = FJET(XB1, XB2, s, t329, -t331, 0.0D0, 0.0D0, 0.0D0, t388)
      t391 = KAPPA2(t189, x2, x3, 0.10D1, z)
      t392 = s * t391
      t393 = t193 * x3
      t394 = t392 * t393
      t395 = t193 * t330
      t396 = t392 * t395
      t397 = t392 * t195
      t398 = t391 ** 2
      t402 = s * t398 * t16 * t292 * t330
      t404 = 0.1D1 / (-0.2D1 + t391)
      t405 = t7 * t404
      t407 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t394, t397, t3
     #96, 0.0D0, t402)
      t412 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t394, t397, t3
     #96, 0.0D0, t402)
      t415 = t398 ** 2
      t420 = log(-0.4D1 * t41 * t42 * t207 * t330 * t415)
      t434 = -t4 * t405 * t407 * t58 * t232 / 0.8D1 + (-0.90D2 * t4 * t7
     # * (t404 * t412 - t420 * t404 * t407) + 0.180D3 * t221 * t405 * t4
     #07) * t58 * t34 / 0.720D3
      t435 = FJET(XB1, XB2, s, -t394, t396, t397, 0.0D0, t402, t434)
      t437 = KAPPA2(t189, x2, x3, t281, z)
      t438 = s * t437
      t439 = t438 * t393
      t440 = t438 * t395
      t441 = t438 * t285
      t442 = t438 * t287
      t443 = t437 ** 2
      t448 = cos(t10)
      t452 = Sqrt(x3 * t330 * x4 * t27)
      t457 = s * t443 * t16 * t292 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t448 * t452)
      t459 = 0.1D1 / (-0.2D1 + t437)
      t462 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t439, -t441, t
     #440, t442, t457)
      t467 = FJET(XB1, XB2, s, -t439, t440, -t441, t442, t457, t4 * t7 *
     # t459 * t462 * t58 * t232 / 0.8D1)
      rrgq2qght5s3e0 = t187 * t186 + t279 * t278 + t327 * t326 + t389 * 
     #t388 + t435 * t434 + t467 * pi * t53 * t459 * t462 * t58 * t34 * t
     #36 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s3em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t21 = t17 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t21 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t47 = log(0.4D1 * t43 * t44)
      t48 = -0.1D1 + x4
      t52 = log(-0.4D1 * t43 * t44 * t48)
      t55 = 0.1D1 / x4
      t62 = log(0.4D1 * t43 * t25)
      t63 = t62 * pi
      t70 = lh ** 2
      t72 = pi ** 2
      t78 = t62 ** 2
      t86 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t90 = x3 * t20
      t93 = log(0.4D1 * t90 * t26)
      t102 = t8 * t9 * t10 * t12 / 0.8D1 + (0.90D2 * t4 * t7 * (t16 - t2
     #9 * t9) - t39) * t12 / 0.720D3 - t8 * t9 * (t47 - t52) * t55 / 0.1
     #6D2 + (-0.180D3 * t35 - 0.90D2 * t63) * t3 * t7 * t16 / 0.1440D4 +
     # (pi * (0.180D3 * t70 - 0.30D2 * t72) + 0.180D3 * t63 * lh + 0.45D
     #2 * t78 * pi) * t3 * t7 * t9 / 0.1440D4 + t4 * t7 * t86 / 0.16D2 -
     # (0.90D2 * t4 * t7 * (-t16 + t93 * t9) + t39) * t10 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = 0.1D1 - x1
      t106 = KAPPA2(t105, x2, 0.0D0, 0.10D1, z)
      t107 = s * t106
      t108 = -t105
      t109 = t1 * t108
      t110 = t107 * t109
      t111 = t1 * x1
      t112 = t107 * t111
      t113 = t106 ** 2
      t117 = s * t113 * t24 * t108 * x1
      t119 = 0.1D1 / (-0.2D1 + t106)
      t120 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t112, -t
     #110, 0.0D0, -t117)
      t121 = t119 * t120
      t122 = t10 * t12
      t126 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t112, -t
     #110, 0.0D0, -t117)
      t129 = t108 ** 2
      t131 = t113 ** 2
      t135 = log(0.4D1 * t21 * t23 * t25 * t129 * t131)
      t150 = t12 * t55
      t154 = t8 * t121 * t122 / 0.8D1 + (0.90D2 * t4 * t7 * (t119 * t126
     # - t135 * t119 * t120) - 0.180D3 * t35 * t3 * t7 * t119 * t120) * 
     #t12 / 0.720D3 + t8 * t121 * t150 / 0.8D1
      t155 = FJET(XB1, XB2, s, 0.0D0, -t110, t112, 0.0D0, -t117, t154)
      t158 = KAPPA2(t105, x2, 0.0D0, -t48, z)
      t159 = s * t158
      t160 = t159 * t109
      t162 = t159 * t111 * t48
      t164 = t159 * t111 * x4
      t165 = t158 ** 2
      t168 = t108 * x1
      t170 = s * t165 * t24 * t168 * t48
      t173 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t162, -
     #t160, t164, t170)
      t175 = 0.1D1 / (-0.2D1 + t158) * t173 * t150
      t178 = FJET(XB1, XB2, s, 0.0D0, -t160, -t162, t164, t170, -t8 * t1
     #75 / 0.8D1)
      t183 = t2 * x3
      t184 = -0.1D1 + x3
      t185 = t2 * t184
      t186 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t183, 0.0D0, -t
     #185, 0.0D0, 0.0D0)
      t191 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t183, 0.0D0, -t
     #185, 0.0D0, 0.0D0)
      t195 = log(-0.4D1 * t90 * t26 * t184)
      t207 = -t8 * t186 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (t191 
     #- t195 * t186) - 0.180D3 * t35 * t36 * t186) * t10 / 0.1440D4
      t208 = FJET(XB1, XB2, s, t183, -t185, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = KAPPA2(t105, x2, x3, 0.10D1, z)
      t211 = s * t210
      t213 = t211 * t109 * x3
      t215 = t211 * t109 * t184
      t216 = t211 * t111
      t217 = t210 ** 2
      t221 = s * t217 * t24 * t168 * t184
      t224 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t213, t216, t2
     #15, 0.0D0, t221)
      t226 = 0.1D1 / (-0.2D1 + t210) * t224 * t122
      t229 = FJET(XB1, XB2, s, -t213, t215, t216, 0.0D0, t221, -t8 * t22
     #6 / 0.8D1)
      rrgq2qght5s3em1 = t103 * t102 + t155 * t154 - t178 * pi * t36 * t1
     #75 / 0.8D1 + t208 * t207 - t229 * pi * t36 * t226 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s3em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t44 = 0.1D1 - x1
      t45 = KAPPA2(t44, x2, 0.0D0, 0.10D1, z)
      t46 = s * t45
      t47 = -t44
      t49 = t46 * t1 * t47
      t51 = t46 * t1 * x1
      t52 = t45 ** 2
      t56 = s * t52 * t26 * t47 * x1
      t59 = 0.1D1 / (-0.2D1 + t45)
      t60 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t51, -t49
     #, 0.0D0, -t56)
      t65 = FJET(XB1, XB2, s, 0.0D0, -t49, t51, 0.0D0, -t56, t4 * t7 * t
     #59 * t60 * t10 / 0.8D1)
      t73 = t2 * x3
      t75 = t2 * (-0.1D1 + x3)
      t76 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t73, 0.0D0, -t75
     #, 0.0D0, 0.0D0)
      t78 = t7 * t76 * t37
      t81 = FJET(XB1, XB2, s, t73, -t75, 0.0D0, 0.0D0, 0.0D0, -t4 * t78 
     #/ 0.16D2)
      rrgq2qght5s3em2 = t42 * t41 + t65 * pi * t3 * t7 * t59 * t60 * t10
     # / 0.8D1 - t81 * pi * t3 * t78 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s3em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s3em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s3em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s3em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s4e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t29 = t16 * x4
      t30 = -0.1D1 + x4
      t31 = t29 * t30
      t34 = log(-0.4D1 * t12 * t14 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = lh ** 2
      t59 = pi ** 2
      t61 = 0.180D3 * t57 - 0.30D2 * t59
      t62 = pi * t61
      t63 = t12 * t17
      t65 = log(0.4D1 * t63)
      t70 = t65 ** 2
      t73 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t77 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t88 = 0.60D2 * lh * t59 - 0.240D3 * zeta3 - 0.120D3 * t57 * lh
      t89 = pi * t88
      t90 = t44 * t25
      t91 = t89 * t90
      t102 = t4 * t7
      t103 = x3 * t8
      t104 = t103 * t11
      t105 = x4 * t30
      t109 = log(-0.4D1 * t104 * t17 * t105)
      t113 = log(0.4D1 * t104 * t18)
      t116 = 0.1D1 / x3
      t118 = t52 * t54
      t122 = t11 * t14
      t123 = t122 * t16
      t124 = t103 * t123
      t126 = log(0.4D1 * t124)
      t128 = t126 ** 2
      t140 = t62 * t90
      t145 = t7 * t22
      t153 = log(0.4D1 * t122 * t29)
      t154 = t153 ** 2
      t157 = log(-0.4D1 * t122 * t31)
      t158 = t157 ** 2
      t162 = t7 * t25
      t173 = t7 * t77
      t183 = log(0.4D1 * t123)
      t184 = t183 * pi
      t186 = t183 ** 2
      t187 = t186 * pi
      t191 = t186 * t183 * pi
      t211 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t215 = t59 ** 2
      t216 = t57 ** 2
      t229 = t186 ** 2
      t236 = x3 * t11
      t239 = log(0.4D1 * t236 * t18)
      t241 = t239 ** 2
      t244 = t236 * t14
      t247 = log(-0.4D1 * t244 * t31)
      t249 = t247 ** 2
      t268 = log(0.4D1 * t236 * t17)
      t273 = t268 ** 2
      t294 = (0.90D2 * t4 * t7 * (-t21 * t22 + t24 * t25 / 0.2D1 + t34 *
     # t22 - t36 * t25 / 0.2D1) - 0.180D3 * t43 * t44 * (-t21 * t25 + t3
     #4 * t25)) * t52 * t54 / 0.720D3 - (t62 * t44 * (-t22 + t65 * t25) 
     #+ 0.90D2 * t4 * t7 * (-t70 * t22 / 0.2D1 - t73 + t70 * t65 * t25 /
     # 0.6D1 + t65 * t77) - t91 - 0.180D3 * t43 * t44 * (t65 * t22 - t70
     # * t25 / 0.2D1 - t77)) * t52 / 0.720D3 - t102 * (-t109 * t25 + t11
     #3 * t25) * t116 * t118 / 0.8D1 + (0.90D2 * t4 * t7 * (-t126 * t22 
     #+ t128 * t25 / 0.2D1 + t77) - 0.180D3 * t43 * t44 * (t22 - t126 * 
     #t25) + t140) * t116 * t52 / 0.720D3 + ((-0.90D2 * t4 * t145 + 0.18
     #0D3 * t43 * t90) * (-t154 / 0.2D1 + t158 / 0.2D1) - 0.90D2 * t4 * 
     #t162 * (-t158 * t157 / 0.6D1 + t154 * t153 / 0.6D1) + (0.180D3 * t
     #43 * t44 * t22 - t140 - 0.90D2 * t4 * t173) * (t153 - t157)) * t54
     # / 0.1440D4 + (t89 - t184 * t61 - 0.90D2 * t187 * lh - 0.15D2 * t1
     #91) * t3 * t145 / 0.1440D4 + (t62 + 0.180D3 * t184 * lh + 0.45D2 *
     # t187) * t3 * t173 / 0.1440D4 + (-0.180D3 * t43 - 0.90D2 * t184) *
     # t3 * t7 * t73 / 0.1440D4 + t4 * t7 * t211 / 0.16D2 + (pi * (t215 
     #+ 0.60D2 * t216 + 0.480D3 * lh * zeta3 - 0.60D2 * t57 * t59) - t18
     #4 * t88 + t187 * t61 / 0.2D1 + 0.30D2 * t191 * lh + 0.15D2 / 0.4D1
     # * t229 * pi) * t3 * t162 / 0.1440D4 + (0.90D2 * t4 * t7 * (-t239 
     #* t22 + t241 * t25 / 0.2D1 + t247 * t22 - t249 * t25 / 0.2D1) - 0.
     #180D3 * t43 * t44 * (-t239 * t25 + t247 * t25)) * t116 * t54 / 0.1
     #440D4 - (t62 * t44 * (-t22 + t268 * t25) + 0.90D2 * t4 * t7 * (-t2
     #73 * t22 / 0.2D1 - t73 + t273 * t268 * t25 / 0.6D1 + t268 * t77) -
     # t91 - 0.180D3 * t43 * t44 * (t268 * t22 - t273 * t25 / 0.2D1 - t7
     #7)) * t116 / 0.1440D4
      t295 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t294)
      t297 = -0.1D1 + x1
      t298 = t2 * t297
      t299 = t2 * x1
      t300 = t297 ** 2
      t305 = log(0.4D1 * t123 * t8 * t300 * x4)
      t306 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t298, t299, 0.0D0)
      t308 = t305 ** 2
      t309 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t298, t299, 0.0D0)
      t312 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t298, t299, 0.0D0)
      t322 = t44 * t309
      t323 = t62 * t322
      t327 = t16 * t8
      t328 = t327 * t300
      t331 = log(0.4D1 * t122 * t328)
      t336 = t331 ** 2
      t339 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t298, t299, 0.0D0)
      t358 = t300 * x4
      t362 = log(0.4D1 * t244 * t327 * t358)
      t375 = log(0.4D1 * t244 * t328)
      t377 = t375 ** 2
      t393 = (0.90D2 * t4 * t7 * (t305 * t306 - t308 * t309 / 0.2D1 - t3
     #12) - 0.180D3 * t43 * t44 * (-t306 + t305 * t309) - t323) * t52 * 
     #t54 / 0.720D3 - (t62 * t44 * (t306 - t331 * t309) + 0.90D2 * t4 * 
     #t7 * (t336 * t306 / 0.2D1 + t339 - t336 * t331 * t309 / 0.6D1 - t3
     #31 * t312) + t89 * t322 - 0.180D3 * t43 * t44 * (-t331 * t306 + t3
     #36 * t309 / 0.2D1 + t312)) * t52 / 0.720D3 - (0.90D2 * t4 * t7 * (
     #t306 - t362 * t309) - 0.180D3 * t43 * t322) * t116 * t118 / 0.720D
     #3 + (0.90D2 * t4 * t7 * (t375 * t306 - t377 * t309 / 0.2D1 - t312)
     # - 0.180D3 * t43 * t44 * (-t306 + t375 * t309) - t323) * t116 * t5
     #2 / 0.720D3
      t394 = FJET(XB1, XB2, s, 0.0D0, -t298, 0.0D0, t299, 0.0D0, t393)
      t396 = -t297
      t397 = KAPPA2(t396, x2, 0.0D0, x4, z)
      t398 = s * t397
      t399 = t1 * t297
      t400 = t398 * t399
      t401 = t1 * x1
      t402 = t401 * x4
      t403 = t398 * t402
      t404 = t401 * t30
      t405 = t398 * t404
      t406 = t397 ** 2
      t409 = t297 * x1
      t411 = s * t406 * t15 * t409 * x4
      t412 = t406 ** 2
      t417 = log(-0.4D1 * t63 * t358 * t30 * t412)
      t419 = 0.1D1 / (-0.2D1 + t397)
      t420 = t417 * t419
      t421 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t403, -t
     #400, -t405, -t411)
      t423 = t417 ** 2
      t425 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t403, -t
     #400, -t405, -t411)
      t428 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t403, -t
     #400, -t405, -t411)
      t434 = t419 * t421
      t440 = t62 * t3
      t442 = t7 * t419 * t425
      t447 = t103 * t122
      t448 = t16 * t300
      t453 = log(-0.4D1 * t447 * t448 * t105 * t412)
      t460 = t43 * t3
      t467 = (-0.90D2 * t4 * t7 * (-t420 * t421 + t423 * t419 * t425 / 0
     #.2D1 + t419 * t428) + 0.180D3 * t43 * t44 * (t434 - t420 * t425) -
     # t440 * t442) * t52 * t54 / 0.720D3 - (0.90D2 * t4 * t7 * (t434 - 
     #t453 * t419 * t425) - 0.180D3 * t460 * t442) * t116 * t118 / 0.720
     #D3
      t468 = FJET(XB1, XB2, s, 0.0D0, -t400, t403, -t405, -t411, t467)
      t470 = t2 * x3
      t471 = -0.1D1 + x3
      t472 = t2 * t471
      t474 = t29 * t30 * t471
      t477 = log(0.4D1 * t447 * t474)
      t478 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t470, 0.0D0, -t
     #472, 0.0D0, 0.0D0)
      t480 = t471 * x4
      t484 = log(-0.4D1 * t104 * t17 * t480)
      t491 = t17 * t471
      t494 = log(-0.4D1 * t104 * t491)
      t495 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t470, 0.0D0, -t
     #472, 0.0D0, 0.0D0)
      t497 = t494 ** 2
      t500 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t470, 0.0D0, -t
     #472, 0.0D0, 0.0D0)
      t510 = t44 * t478
      t518 = log(0.4D1 * t244 * t474)
      t520 = t518 ** 2
      t527 = log(-0.4D1 * t244 * t16 * t471 * x4)
      t529 = t527 ** 2
      t548 = log(-0.4D1 * t236 * t491)
      t553 = t548 ** 2
      t556 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t470, 0.0D0, -t
     #472, 0.0D0, 0.0D0)
      t576 = -t102 * (t477 * t478 - t484 * t478) * t116 * t118 / 0.8D1 +
     # (0.90D2 * t4 * t7 * (t494 * t495 - t497 * t478 / 0.2D1 - t500) - 
     #0.180D3 * t43 * t44 * (-t495 + t494 * t478) - t62 * t510) * t116 *
     # t52 / 0.720D3 + (0.90D2 * t4 * t7 * (-t518 * t495 + t520 * t478 /
     # 0.2D1 + t527 * t495 - t529 * t478 / 0.2D1) - 0.180D3 * t43 * t44 
     #* (-t518 * t478 + t527 * t478)) * t116 * t54 / 0.1440D4 - (-t62 * 
     #t44 * (-t495 + t548 * t478) - 0.90D2 * t4 * t7 * (-t553 * t495 / 0
     #.2D1 - t556 + t553 * t548 * t478 / 0.6D1 + t548 * t500) + t89 * t5
     #10 + 0.180D3 * t43 * t44 * (t548 * t495 - t553 * t478 / 0.2D1 - t5
     #00)) * t116 / 0.1440D4
      t577 = FJET(XB1, XB2, s, t470, -t472, 0.0D0, 0.0D0, 0.0D0, t576)
      t579 = KAPPA2(t396, x2, x3, 0.0D0, z)
      t580 = s * t579
      t581 = t399 * x3
      t582 = t580 * t581
      t583 = t399 * t471
      t584 = t580 * t583
      t585 = t580 * t401
      t586 = t579 ** 2
      t590 = s * t586 * t15 * t409 * x3
      t592 = 0.1D1 / (-0.2D1 + t579)
      t593 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t582, 0.0D0, t
     #584, t585, -t590)
      t594 = t592 * t593
      t595 = t586 ** 2
      t600 = log(-0.4D1 * t447 * t448 * t480 * t595)
      t602 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t582, 0.0D0, t
     #584, t585, -t590)
      t609 = t7 * t592 * t602
      t619 = log(-0.4D1 * t447 * t448 * t471 * t595)
      t620 = t619 * t592
      t622 = t619 ** 2
      t626 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t582, 0.0D0, t
     #584, t585, -t590)
      t642 = -(0.90D2 * t4 * t7 * (t594 - t600 * t592 * t602) - 0.180D3 
     #* t460 * t609) * t116 * t118 / 0.720D3 + (-0.90D2 * t4 * t7 * (-t6
     #20 * t593 + t622 * t592 * t602 / 0.2D1 + t592 * t626) + 0.180D3 * 
     #t43 * t44 * (t594 - t620 * t602) - t440 * t609) * t116 * t52 / 0.7
     #20D3
      t643 = FJET(XB1, XB2, s, -t582, t584, 0.0D0, t585, -t590, t642)
      t645 = KAPPA2(t396, x2, x3, x4, z)
      t646 = s * t645
      t647 = t646 * t581
      t648 = t646 * t583
      t649 = t646 * t402
      t650 = t646 * t404
      t651 = t645 ** 2
      t656 = cos(t9)
      t659 = Sqrt(x3 * t471 * t105)
      t664 = s * t651 * t15 * t409 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t656 * t659)
      t666 = 0.1D1 / (-0.2D1 + t645)
      t667 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t647, t649, t6
     #48, -t650, t664)
      t670 = t651 ** 2
      t675 = log(0.4D1 * t124 * t300 * t471 * t105 * t670)
      t677 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t647, t649, t6
     #48, -t650, t664)
      t687 = -0.90D2 * t4 * t7 * (t667 * t666 - t675 * t666 * t677) + 0.
     #180D3 * t460 * t7 * t666 * t677
      t691 = FJET(XB1, XB2, s, -t647, t648, t649, -t650, t664, -t687 * t
     #116 * t118 / 0.720D3)
      rrgq2qght5s4e1 = t295 * t294 + t394 * t393 + t468 * t467 + t577 * 
     #t576 + t643 * t642 - t691 * t687 * t116 * t52 * t54 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s4e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t26 = t17 * x4
      t27 = -0.1D1 + x4
      t28 = t26 * t27
      t31 = log(-0.4D1 * t13 * t15 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t17
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t23
      t56 = 0.180D3 * t52 * t54
      t58 = 0.1D1 / x3
      t62 = t13 * t18
      t64 = log(0.4D1 * t62)
      t66 = t64 ** 2
      t69 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t79 = lh ** 2
      t81 = pi ** 2
      t83 = 0.180D3 * t79 - 0.30D2 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t89 = t7 * t23
      t92 = log(0.4D1 * t42 * t26)
      t93 = t92 ** 2
      t96 = log(-0.4D1 * t42 * t28)
      t97 = t96 ** 2
      t103 = t7 * t40
      t113 = log(0.4D1 * t43)
      t114 = t113 * pi
      t117 = t113 ** 2
      t118 = t117 * pi
      t124 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t152 = x3 * t12
      t155 = log(0.4D1 * t152 * t19)
      t157 = t152 * t15
      t160 = log(-0.4D1 * t157 * t28)
      t169 = log(0.4D1 * t152 * t18)
      t171 = t169 ** 2
      t186 = t8 * (-t22 * t23 + t31 * t23) * t34 * t36 / 0.8D1 + (0.90D2
     # * t4 * t7 * (t40 - t46 * t23) - t56) * t58 * t34 / 0.720D3 - (0.9
     #0D2 * t4 * t7 * (t64 * t40 - t66 * t23 / 0.2D1 - t69) - 0.180D3 * 
     #t52 * t53 * (-t40 + t64 * t23) - t85) * t34 / 0.720D3 + (-0.90D2 *
     # t4 * t89 * (-t93 / 0.2D1 + t97 / 0.2D1) + (-0.90D2 * t4 * t103 + 
     #t56) * (t92 - t96)) * t36 / 0.1440D4 + (t84 + 0.180D3 * t114 * lh 
     #+ 0.45D2 * t118) * t3 * t103 / 0.1440D4 + t4 * t7 * t124 / 0.16D2 
     #+ (pi * (0.60D2 * lh * t81 - 0.240D3 * zeta3 - 0.120D3 * t79 * lh)
     # - t114 * t83 - 0.90D2 * t118 * lh - 0.15D2 * t117 * t113 * pi) * 
     #t3 * t89 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t114) * t3 * t7 *
     # t69 / 0.1440D4 + t8 * (-t155 * t23 + t160 * t23) * t58 * t36 / 0.
     #16D2 - (0.90D2 * t4 * t7 * (t169 * t40 - t171 * t23 / 0.2D1 - t69)
     # - 0.180D3 * t52 * t53 * (-t40 + t169 * t23) - t85) * t58 / 0.1440
     #D4
      t187 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t186)
      t189 = -0.1D1 + x1
      t190 = t2 * t189
      t191 = t2 * x1
      t192 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t190, t191, 0.0D0)
      t193 = t189 ** 2
      t198 = log(0.4D1 * t43 * t9 * t193 * x4)
      t199 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t190, t191, 0.0D0)
      t205 = t53 * t199
      t207 = 0.180D3 * t52 * t205
      t213 = t34 * t36
      t218 = t17 * t9 * t193
      t221 = log(0.4D1 * t157 * t218)
      t233 = log(0.4D1 * t42 * t218)
      t235 = t233 ** 2
      t238 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t190, t191, 0.0D0)
      t252 = (0.90D2 * t4 * t7 * (-t192 + t198 * t199) + t207) * t34 * t
     #36 / 0.720D3 - t8 * t199 * t58 * t213 / 0.8D1 + (0.90D2 * t4 * t7 
     #* (-t192 + t221 * t199) + t207) * t58 * t34 / 0.720D3 - (0.90D2 * 
     #t4 * t7 * (-t233 * t192 + t235 * t199 / 0.2D1 + t238) - 0.180D3 * 
     #t52 * t53 * (t192 - t233 * t199) + t84 * t205) * t34 / 0.720D3
      t253 = FJET(XB1, XB2, s, 0.0D0, -t190, 0.0D0, t191, 0.0D0, t252)
      t255 = -t189
      t256 = KAPPA2(t255, x2, 0.0D0, x4, z)
      t257 = s * t256
      t258 = t1 * t189
      t259 = t257 * t258
      t260 = t1 * x1
      t261 = t260 * x4
      t262 = t257 * t261
      t263 = t260 * t27
      t264 = t257 * t263
      t265 = t256 ** 2
      t268 = t189 * x1
      t270 = s * t265 * t16 * t268 * x4
      t272 = 0.1D1 / (-0.2D1 + t256)
      t273 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t262, -t
     #259, -t264, -t270)
      t276 = t265 ** 2
      t281 = log(-0.4D1 * t62 * t193 * x4 * t27 * t276)
      t283 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t262, -t
     #259, -t264, -t270)
      t289 = t52 * t3
      t290 = t7 * t272
      t303 = (-0.90D2 * t4 * t7 * (t272 * t273 - t281 * t272 * t283) + 0
     #.180D3 * t289 * t290 * t283) * t34 * t36 / 0.720D3 - t4 * t290 * t
     #283 * t58 * t213 / 0.8D1
      t304 = FJET(XB1, XB2, s, 0.0D0, -t259, t262, -t264, -t270, t303)
      t306 = t2 * x3
      t307 = -0.1D1 + x3
      t308 = t2 * t307
      t309 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t306, 0.0D0, -t
     #308, 0.0D0, 0.0D0)
      t311 = t18 * t307
      t314 = log(-0.4D1 * t41 * t12 * t311)
      t315 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t306, 0.0D0, -t
     #308, 0.0D0, 0.0D0)
      t321 = t53 * t315
      t332 = log(0.4D1 * t157 * t26 * t27 * t307)
      t338 = log(-0.4D1 * t157 * t17 * t307 * x4)
      t347 = log(-0.4D1 * t152 * t311)
      t349 = t347 ** 2
      t352 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t306, 0.0D0, -t
     #308, 0.0D0, 0.0D0)
      t366 = (0.90D2 * t4 * t7 * (-t309 + t314 * t315) + 0.180D3 * t52 *
     # t321) * t58 * t34 / 0.720D3 + t8 * (-t332 * t315 + t338 * t315) *
     # t58 * t36 / 0.16D2 - (-0.90D2 * t4 * t7 * (t347 * t309 - t349 * t
     #315 / 0.2D1 - t352) + 0.180D3 * t52 * t53 * (-t309 + t347 * t315) 
     #+ t84 * t321) * t58 / 0.1440D4
      t367 = FJET(XB1, XB2, s, t306, -t308, 0.0D0, 0.0D0, 0.0D0, t366)
      t369 = KAPPA2(t255, x2, x3, 0.0D0, z)
      t370 = s * t369
      t371 = t258 * x3
      t372 = t370 * t371
      t373 = t258 * t307
      t374 = t370 * t373
      t375 = t370 * t260
      t376 = t369 ** 2
      t380 = s * t376 * t16 * t268 * x3
      t382 = 0.1D1 / (-0.2D1 + t369)
      t383 = t7 * t382
      t385 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t372, 0.0D0, t
     #374, t375, -t380)
      t390 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t372, 0.0D0, t
     #374, t375, -t380)
      t394 = t376 ** 2
      t399 = log(-0.4D1 * t41 * t42 * t17 * t193 * t307 * t394)
      t413 = -t4 * t383 * t385 * t58 * t213 / 0.8D1 + (-0.90D2 * t4 * t7
     # * (t382 * t390 - t399 * t382 * t385) + 0.180D3 * t289 * t383 * t3
     #85) * t58 * t34 / 0.720D3
      t414 = FJET(XB1, XB2, s, -t372, t374, 0.0D0, t375, -t380, t413)
      t416 = KAPPA2(t255, x2, x3, x4, z)
      t417 = s * t416
      t418 = t417 * t371
      t419 = t417 * t373
      t420 = t417 * t261
      t421 = t417 * t263
      t422 = t416 ** 2
      t427 = cos(t10)
      t431 = Sqrt(x3 * t307 * x4 * t27)
      t436 = s * t422 * t16 * t268 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t427 * t431)
      t438 = 0.1D1 / (-0.2D1 + t416)
      t441 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t418, t420, t4
     #19, -t421, t436)
      t446 = FJET(XB1, XB2, s, -t418, t419, t420, -t421, t436, t4 * t7 *
     # t438 * t441 * t58 * t213 / 0.8D1)
      rrgq2qght5s4e0 = t187 * t186 + t253 * t252 + t304 * t303 + t367 * 
     #t366 + t414 * t413 + t446 * pi * t53 * t438 * t441 * t58 * t34 * t
     #36 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s4em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t17 * t20 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t47 = log(0.4D1 * t43 * t44)
      t48 = -0.1D1 + x4
      t52 = log(-0.4D1 * t43 * t44 * t48)
      t55 = 0.1D1 / x4
      t62 = log(0.4D1 * t43 * t25)
      t63 = t62 * pi
      t70 = lh ** 2
      t72 = pi ** 2
      t78 = t62 ** 2
      t86 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t90 = x3 * t20
      t93 = log(0.4D1 * t90 * t26)
      t102 = t8 * t9 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (-t16 + t
     #29 * t9) + t39) * t12 / 0.720D3 - t8 * t9 * (t47 - t52) * t55 / 0.
     #16D2 + (-0.180D3 * t35 - 0.90D2 * t63) * t3 * t7 * t16 / 0.1440D4 
     #+ (pi * (0.180D3 * t70 - 0.30D2 * t72) + 0.180D3 * t63 * lh + 0.45
     #D2 * t78 * pi) * t3 * t7 * t9 / 0.1440D4 + t4 * t7 * t86 / 0.16D2 
     #- (0.90D2 * t4 * t7 * (-t16 + t93 * t9) + t39) * t10 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = -0.1D1 + x1
      t106 = t2 * t105
      t107 = t2 * x1
      t108 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t106, t107, 0.0D0)
      t113 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t106, t107, 0.0D0)
      t115 = t105 ** 2
      t119 = log(0.4D1 * t43 * t25 * t17 * t115)
      t135 = -t8 * t108 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (t113 
     #- t119 * t108) - 0.180D3 * t35 * t36 * t108) * t12 / 0.720D3 - t8 
     #* t108 * t12 * t55 / 0.8D1
      t136 = FJET(XB1, XB2, s, 0.0D0, -t106, 0.0D0, t107, 0.0D0, t135)
      t138 = -t105
      t139 = KAPPA2(t138, x2, 0.0D0, x4, z)
      t140 = s * t139
      t141 = t1 * t105
      t142 = t140 * t141
      t143 = t1 * x1
      t145 = t140 * t143 * x4
      t147 = t140 * t143 * t48
      t148 = t139 ** 2
      t151 = t105 * x1
      t153 = s * t148 * t24 * t151 * x4
      t156 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t145, -t
     #142, -t147, -t153)
      t159 = 0.1D1 / (-0.2D1 + t139) * t156 * t12 * t55
      t162 = FJET(XB1, XB2, s, 0.0D0, -t142, t145, -t147, -t153, -t8 * t
     #159 / 0.8D1)
      t167 = t2 * x3
      t168 = -0.1D1 + x3
      t169 = t2 * t168
      t170 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t167, 0.0D0, -t
     #169, 0.0D0, 0.0D0)
      t175 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t167, 0.0D0, -t
     #169, 0.0D0, 0.0D0)
      t179 = log(-0.4D1 * t90 * t26 * t168)
      t191 = -t8 * t170 * t10 * t12 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t17
     #5 + t179 * t170) - 0.180D3 * t35 * t36 * t170) * t10 / 0.1440D4
      t192 = FJET(XB1, XB2, s, t167, -t169, 0.0D0, 0.0D0, 0.0D0, t191)
      t194 = KAPPA2(t138, x2, x3, 0.0D0, z)
      t195 = s * t194
      t197 = t195 * t141 * x3
      t199 = t195 * t141 * t168
      t200 = t195 * t143
      t201 = t194 ** 2
      t205 = s * t201 * t24 * t151 * x3
      t208 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t197, 0.0D0, t
     #199, t200, -t205)
      t211 = 0.1D1 / (-0.2D1 + t194) * t208 * t10 * t12
      t214 = FJET(XB1, XB2, s, -t197, t199, 0.0D0, t200, -t205, -t8 * t2
     #11 / 0.8D1)
      rrgq2qght5s4em1 = t103 * t102 + t136 * t135 - t162 * pi * t36 * t1
     #59 / 0.8D1 + t192 * t191 - t214 * pi * t36 * t211 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s4em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t45 = t2 * (-0.1D1 + x1)
      t46 = t2 * x1
      t47 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #45, t46, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, 0.0D0, -t45, 0.0D0, t46, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t57 = t2 * x3
      t59 = t2 * (-0.1D1 + x3)
      t60 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t57, 0.0D0, -t59
     #, 0.0D0, 0.0D0)
      t62 = t7 * t60 * t37
      t65 = FJET(XB1, XB2, s, t57, -t59, 0.0D0, 0.0D0, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrgq2qght5s4em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s4em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s4em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s4em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s4em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s5e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = lh ** 2
      t42 = pi ** 2
      t44 = 0.180D3 * t40 - 0.30D2 * t42
      t45 = pi * t44
      t46 = t34 * t25
      t47 = t45 * t46
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t12 * t17
      t56 = log(0.4D1 * t54)
      t61 = t56 ** 2
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t78 = 0.60D2 * lh * t42 - 0.240D3 * zeta3 - 0.120D3 * t40 * lh
      t79 = t78 * pi
      t80 = t79 * t46
      t91 = t4 * t7
      t92 = x3 * t8
      t93 = t92 * t11
      t96 = log(0.4D1 * t93 * t18)
      t98 = -0.1D1 + x3
      t99 = t98 * x4
      t103 = log(-0.4D1 * t93 * t17 * t99)
      t106 = 0.1D1 / x3
      t108 = t49 * t51
      t112 = t11 * t14
      t113 = t112 * t16
      t114 = t92 * t113
      t116 = log(0.4D1 * t114)
      t118 = t116 ** 2
      t121 = t17 * t98
      t124 = log(-0.4D1 * t93 * t121)
      t126 = t124 ** 2
      t143 = t16 * x4
      t146 = log(0.4D1 * t112 * t143)
      t151 = t146 ** 2
      t172 = x3 * t11
      t173 = t172 * t14
      t178 = log(-0.4D1 * t173 * t16 * t98 * x4)
      t180 = t178 ** 2
      t185 = log(0.4D1 * t172 * t18)
      t187 = t185 ** 2
      t204 = t7 * t22
      t212 = log(-0.4D1 * t172 * t121)
      t213 = t212 ** 2
      t216 = log(0.4D1 * t172 * t17)
      t217 = t216 ** 2
      t221 = t7 * t25
      t232 = t7 * t28
      t242 = log(0.4D1 * t113)
      t243 = t242 * pi
      t245 = t242 ** 2
      t246 = t245 * pi
      t250 = t245 * t242 * pi
      t270 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t274 = t42 ** 2
      t275 = t40 ** 2
      t288 = t245 ** 2
      t295 = (0.90D2 * t4 * t7 * (-t21 * t22 + t24 * t25 / 0.2D1 + t28) 
     #- 0.180D3 * t33 * t34 * (t22 - t21 * t25) + t47) * t49 * t51 / 0.7
     #20D3 - (t45 * t34 * (-t22 + t56 * t25) + 0.90D2 * t4 * t7 * (-t61 
     #* t22 / 0.2D1 - t64 + t61 * t56 * t25 / 0.6D1 + t56 * t28) - t80 -
     # 0.180D3 * t33 * t34 * (t56 * t22 - t61 * t25 / 0.2D1 - t28)) * t4
     #9 / 0.720D3 - t91 * (t96 * t25 - t103 * t25) * t106 * t108 / 0.8D1
     # - (0.90D2 * t4 * t7 * (t116 * t22 - t118 * t25 / 0.2D1 - t124 * t
     #22 + t126 * t25 / 0.2D1) - 0.180D3 * t33 * t34 * (t116 * t25 - t12
     #4 * t25)) * t106 * t49 / 0.720D3 - (t45 * t34 * (-t22 + t146 * t25
     #) + 0.90D2 * t4 * t7 * (-t151 * t22 / 0.2D1 - t64 + t151 * t146 * 
     #t25 / 0.6D1 + t146 * t28) - t80 - 0.180D3 * t33 * t34 * (t146 * t2
     #2 - t151 * t25 / 0.2D1 - t28)) * t51 / 0.1440D4 + (0.90D2 * t4 * t
     #7 * (t178 * t22 - t180 * t25 / 0.2D1 - t185 * t22 + t187 * t25 / 0
     #.2D1) - 0.180D3 * t33 * t34 * (t178 * t25 - t185 * t25)) * t106 * 
     #t51 / 0.1440D4 + ((-0.90D2 * t4 * t204 + 0.180D3 * t33 * t46) * (t
     #213 / 0.2D1 - t217 / 0.2D1) - 0.90D2 * t4 * t221 * (t217 * t216 / 
     #0.6D1 - t213 * t212 / 0.6D1) + (0.180D3 * t33 * t34 * t22 - t47 - 
     #0.90D2 * t4 * t232) * (-t212 + t216)) * t106 / 0.1440D4 + (t79 - t
     #243 * t44 - 0.90D2 * t246 * lh - 0.15D2 * t250) * t3 * t204 / 0.14
     #40D4 + (t45 + 0.180D3 * t243 * lh + 0.45D2 * t246) * t3 * t232 / 0
     #.1440D4 + (-0.180D3 * t33 - 0.90D2 * t243) * t3 * t7 * t64 / 0.144
     #0D4 + t4 * t7 * t270 / 0.16D2 + (pi * (t274 + 0.60D2 * t275 + 0.48
     #0D3 * lh * zeta3 - 0.60D2 * t40 * t42) - t243 * t78 + t246 * t44 /
     # 0.2D1 + 0.30D2 * t250 * lh + 0.15D2 / 0.4D1 * t288 * pi) * t3 * t
     #221 / 0.1440D4
      t296 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t295)
      t298 = -0.1D1 + x4
      t299 = t2 * t298
      t300 = t2 * x4
      t302 = t143 * t298
      t305 = log(-0.4D1 * t12 * t14 * t302)
      t306 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t299, 0
     #.0D0, t300, 0.0D0)
      t308 = t305 ** 2
      t309 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t299, 0
     #.0D0, t300, 0.0D0)
      t312 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t299, 0
     #.0D0, t300, 0.0D0)
      t322 = t34 * t309
      t328 = t92 * t112
      t330 = t143 * t298 * t98
      t333 = log(0.4D1 * t328 * t330)
      t335 = x4 * t298
      t339 = log(-0.4D1 * t93 * t17 * t335)
      t348 = log(-0.4D1 * t112 * t302)
      t353 = t348 ** 2
      t356 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t299, 0
     #.0D0, t300, 0.0D0)
      t378 = log(-0.4D1 * t173 * t302)
      t380 = t378 ** 2
      t385 = log(0.4D1 * t173 * t330)
      t387 = t385 ** 2
      t404 = (0.90D2 * t4 * t7 * (t305 * t306 - t308 * t309 / 0.2D1 - t3
     #12) - 0.180D3 * t33 * t34 * (-t306 + t305 * t309) - t45 * t322) * 
     #t49 * t51 / 0.720D3 - t91 * (t333 * t309 - t339 * t309) * t106 * t
     #108 / 0.8D1 - (t45 * t34 * (t306 - t348 * t309) + 0.90D2 * t4 * t7
     # * (t353 * t306 / 0.2D1 + t356 - t353 * t348 * t309 / 0.6D1 - t348
     # * t312) + t79 * t322 - 0.180D3 * t33 * t34 * (-t348 * t306 + t353
     # * t309 / 0.2D1 + t312)) * t51 / 0.1440D4 + (0.90D2 * t4 * t7 * (t
     #378 * t306 - t380 * t309 / 0.2D1 - t385 * t306 + t387 * t309 / 0.2
     #D1) - 0.180D3 * t33 * t34 * (t378 * t309 - t385 * t309)) * t106 * 
     #t51 / 0.1440D4
      t405 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t299, t300, 0.0D0, t404)
      t407 = t2 * x1
      t408 = -0.1D1 + x1
      t409 = t2 * t408
      t410 = t408 ** 2
      t415 = log(0.4D1 * t113 * t8 * t410 * x4)
      t416 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t407, -t409, 0.
     #0D0, 0.0D0, 0.0D0)
      t418 = t415 ** 2
      t419 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t407, -t409, 0.
     #0D0, 0.0D0, 0.0D0)
      t422 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t407, -t409, 0.
     #0D0, 0.0D0, 0.0D0)
      t432 = t34 * t419
      t433 = t45 * t432
      t437 = t16 * t8
      t438 = t437 * t410
      t441 = log(0.4D1 * t112 * t438)
      t446 = t441 ** 2
      t449 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t407, -t409, 0.
     #0D0, 0.0D0, 0.0D0)
      t468 = t410 * x4
      t472 = log(0.4D1 * t173 * t437 * t468)
      t485 = log(0.4D1 * t173 * t438)
      t487 = t485 ** 2
      t503 = (0.90D2 * t4 * t7 * (t415 * t416 - t418 * t419 / 0.2D1 - t4
     #22) - 0.180D3 * t33 * t34 * (-t416 + t415 * t419) - t433) * t49 * 
     #t51 / 0.720D3 - (t45 * t34 * (t416 - t441 * t419) + 0.90D2 * t4 * 
     #t7 * (t446 * t416 / 0.2D1 + t449 - t446 * t441 * t419 / 0.6D1 - t4
     #41 * t422) + t79 * t432 - 0.180D3 * t33 * t34 * (-t441 * t416 + t4
     #46 * t419 / 0.2D1 + t422)) * t49 / 0.720D3 - (0.90D2 * t4 * t7 * (
     #t416 - t472 * t419) - 0.180D3 * t33 * t432) * t106 * t108 / 0.720D
     #3 - (0.90D2 * t4 * t7 * (-t485 * t416 + t487 * t419 / 0.2D1 + t422
     #) - 0.180D3 * t33 * t34 * (t416 - t485 * t419) + t433) * t106 * t4
     #9 / 0.720D3
      t504 = FJET(XB1, XB2, s, t407, 0.0D0, -t409, 0.0D0, 0.0D0, t503)
      t506 = -t298
      t507 = KAPPA2(x1, x2, 0.10D1, t506, z)
      t508 = s * t507
      t509 = t1 * x1
      t510 = t508 * t509
      t511 = t1 * t408
      t512 = t511 * t298
      t513 = t508 * t512
      t514 = t511 * x4
      t515 = t508 * t514
      t516 = t507 ** 2
      t519 = x1 * t408
      t521 = s * t516 * t15 * t519 * x4
      t522 = t516 ** 2
      t527 = log(-0.4D1 * t54 * t468 * t298 * t522)
      t529 = 0.1D1 / (-0.2D1 + t507)
      t530 = t527 * t529
      t531 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t510, t513, 0.0
     #D0, -t515, -t521)
      t533 = t527 ** 2
      t535 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t510, t513, 0.0
     #D0, -t515, -t521)
      t538 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t510, t513, 0.0
     #D0, -t515, -t521)
      t544 = t529 * t531
      t550 = t45 * t3
      t552 = t7 * t529 * t535
      t557 = t16 * t410
      t562 = log(-0.4D1 * t328 * t557 * t335 * t522)
      t569 = t33 * t3
      t576 = (-0.90D2 * t4 * t7 * (-t530 * t531 + t533 * t529 * t535 / 0
     #.2D1 + t529 * t538) + 0.180D3 * t33 * t34 * (t544 - t530 * t535) -
     # t550 * t552) * t49 * t51 / 0.720D3 - (0.90D2 * t4 * t7 * (t544 - 
     #t562 * t529 * t535) - 0.180D3 * t569 * t552) * t106 * t108 / 0.720
     #D3
      t577 = FJET(XB1, XB2, s, t510, 0.0D0, t513, -t515, -t521, t576)
      t579 = -t98
      t580 = KAPPA2(x1, x2, t579, 0.10D1, z)
      t581 = s * t580
      t582 = t509 * t98
      t583 = t581 * t582
      t584 = t509 * x3
      t585 = t581 * t584
      t586 = t581 * t511
      t587 = t580 ** 2
      t591 = s * t587 * t15 * t519 * x3
      t593 = 0.1D1 / (-0.2D1 + t580)
      t594 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t583, -t586, t
     #585, 0.0D0, -t591)
      t595 = t593 * t594
      t596 = t587 ** 2
      t601 = log(-0.4D1 * t328 * t557 * t99 * t596)
      t603 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t583, -t586, t
     #585, 0.0D0, -t591)
      t610 = t7 * t593 * t603
      t620 = log(-0.4D1 * t328 * t557 * t98 * t596)
      t621 = t620 * t593
      t623 = t620 ** 2
      t627 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t583, -t586, t
     #585, 0.0D0, -t591)
      t643 = -(0.90D2 * t4 * t7 * (t595 - t601 * t593 * t603) - 0.180D3 
     #* t569 * t610) * t106 * t108 / 0.720D3 - (0.90D2 * t4 * t7 * (-t62
     #1 * t594 + t623 * t593 * t603 / 0.2D1 + t593 * t627) - 0.180D3 * t
     #33 * t34 * (t595 - t621 * t603) + t550 * t610) * t106 * t49 / 0.72
     #0D3
      t644 = FJET(XB1, XB2, s, -t583, t585, -t586, 0.0D0, -t591, t643)
      t646 = KAPPA2(x1, x2, t579, t506, z)
      t647 = s * t646
      t648 = t647 * t582
      t649 = t647 * t584
      t650 = t647 * t512
      t651 = t647 * t514
      t652 = t646 ** 2
      t657 = cos(t9)
      t660 = Sqrt(x3 * t98 * t335)
      t665 = s * t652 * t15 * t519 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t657 * t660)
      t667 = 0.1D1 / (-0.2D1 + t646)
      t668 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t648, t650, t6
     #49, -t651, t665)
      t671 = t652 ** 2
      t676 = log(0.4D1 * t114 * t410 * t98 * t335 * t671)
      t678 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t648, t650, t6
     #49, -t651, t665)
      t688 = -0.90D2 * t4 * t7 * (t667 * t668 - t676 * t667 * t678) + 0.
     #180D3 * t569 * t7 * t667 * t678
      t692 = FJET(XB1, XB2, s, -t648, t649, t650, -t651, t665, -t688 * t
     #106 * t108 / 0.720D3)
      rrgq2qght5s5e1 = t296 * t295 + t405 * t404 + t504 * t503 + t577 * 
     #t576 + t644 * t643 - t692 * t688 * t106 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s5e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t15 * x4
      t19 = log(0.4D1 * t13 * t16)
      t20 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = t19 ** 2
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = lh ** 2
      t40 = pi ** 2
      t42 = 0.180D3 * t38 - 0.30D2 * t40
      t43 = pi * t42
      t44 = t32 * t23
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t50 = t13 * t15
      t52 = log(0.4D1 * t50)
      t53 = t52 * pi
      t56 = t52 ** 2
      t57 = t56 * pi
      t61 = t7 * t20
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t83 = t7 * t23
      t93 = x1 ** 2
      t94 = t93 * t10
      t95 = t12 * t15
      t96 = t95 * x4
      t99 = log(0.4D1 * t94 * t96)
      t106 = 0.180D3 * t31 * t44
      t108 = 0.1D1 / x1
      t112 = t4 * t7
      t113 = x3 * t93
      t116 = log(0.4D1 * t113 * t50)
      t119 = -0.1D1 + x3
      t120 = t95 * t119
      t123 = log(-0.4D1 * t113 * t10 * t120)
      t126 = 0.1D1 / x3
      t131 = t94 * t95
      t133 = log(0.4D1 * t131)
      t135 = t133 ** 2
      t150 = x3 * t10
      t151 = t150 * t12
      t156 = log(-0.4D1 * t151 * t15 * t119 * x4)
      t160 = log(0.4D1 * t150 * t96)
      t169 = log(-0.4D1 * t150 * t120)
      t170 = t169 ** 2
      t173 = log(0.4D1 * t150 * t95)
      t174 = t173 ** 2
      t188 = -(0.90D2 * t4 * t7 * (t19 * t20 - t22 * t23 / 0.2D1 - t26) 
     #- 0.180D3 * t31 * t32 * (-t20 + t19 * t23) - t45) * t47 / 0.1440D4
     # + (t43 + 0.180D3 * t53 * lh + 0.45D2 * t57) * t3 * t61 / 0.1440D4
     # + t4 * t7 * t64 / 0.16D2 + (pi * (0.60D2 * t40 * lh - 0.240D3 * z
     #eta3 - 0.120D3 * t38 * lh) - t53 * t42 - 0.90D2 * t57 * lh - 0.15D
     #2 * t56 * t52 * pi) * t3 * t83 / 0.1440D4 + (-0.180D3 * t31 - 0.90
     #D2 * t53) * t3 * t7 * t26 / 0.1440D4 + (0.90D2 * t4 * t7 * (t20 - 
     #t99 * t23) - t106) * t108 * t47 / 0.720D3 - t112 * (t116 * t23 - t
     #123 * t23) * t126 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (t133 * t20
     # - t135 * t23 / 0.2D1 - t26) - 0.180D3 * t31 * t32 * (-t20 + t133 
     #* t23) - t45) * t108 / 0.720D3 + t112 * (t156 * t23 - t160 * t23) 
     #* t126 * t47 / 0.16D2 + (-0.90D2 * t4 * t83 * (t170 / 0.2D1 - t174
     # / 0.2D1) + (-0.90D2 * t4 * t61 + t106) * (-t169 + t173)) * t126 /
     # 0.1440D4
      t189 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t188)
      t191 = -0.1D1 + x4
      t192 = t2 * t191
      t193 = t2 * x4
      t194 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t192, 0
     #.0D0, t193, 0.0D0)
      t196 = t16 * t191
      t199 = log(-0.4D1 * t94 * t12 * t196)
      t200 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t192, 0
     #.0D0, t193, 0.0D0)
      t206 = t32 * t200
      t215 = log(-0.4D1 * t151 * t196)
      t221 = log(0.4D1 * t151 * t16 * t191 * t119)
      t230 = log(-0.4D1 * t13 * t196)
      t232 = t230 ** 2
      t235 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t192, 0
     #.0D0, t193, 0.0D0)
      t249 = (0.90D2 * t4 * t7 * (-t194 + t199 * t200) + 0.180D3 * t31 *
     # t206) * t108 * t47 / 0.720D3 + t112 * (t215 * t200 - t221 * t200)
     # * t126 * t47 / 0.16D2 - (0.90D2 * t4 * t7 * (-t230 * t194 + t232 
     #* t200 / 0.2D1 + t235) - 0.180D3 * t31 * t32 * (t194 - t230 * t200
     #) + t43 * t206) * t47 / 0.1440D4
      t250 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t192, t193, 0.0D0, t249)
      t252 = t2 * x1
      t253 = -0.1D1 + x1
      t254 = t2 * t253
      t255 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t252, -t254, 0.
     #0D0, 0.0D0, 0.0D0)
      t256 = t253 ** 2
      t261 = log(0.4D1 * t50 * t93 * t256 * x4)
      t262 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t252, -t254, 0.
     #0D0, 0.0D0, 0.0D0)
      t268 = t32 * t262
      t270 = 0.180D3 * t31 * t268
      t276 = t108 * t47
      t281 = t15 * t93 * t256
      t284 = log(0.4D1 * t151 * t281)
      t296 = log(0.4D1 * t13 * t281)
      t298 = t296 ** 2
      t301 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t252, -t254, 0.
     #0D0, 0.0D0, 0.0D0)
      t315 = (0.90D2 * t4 * t7 * (-t255 + t261 * t262) + t270) * t108 * 
     #t47 / 0.720D3 - t112 * t262 * t126 * t276 / 0.8D1 - (0.90D2 * t4 *
     # t7 * (t255 - t284 * t262) - t270) * t126 * t108 / 0.720D3 - (0.90
     #D2 * t4 * t7 * (-t296 * t255 + t298 * t262 / 0.2D1 + t301) - 0.180
     #D3 * t31 * t32 * (t255 - t296 * t262) + t43 * t268) * t108 / 0.720
     #D3
      t316 = FJET(XB1, XB2, s, t252, 0.0D0, -t254, 0.0D0, 0.0D0, t315)
      t318 = -t191
      t319 = KAPPA2(x1, x2, 0.10D1, t318, z)
      t320 = s * t319
      t321 = t1 * x1
      t322 = t320 * t321
      t323 = t1 * t253
      t324 = t323 * t191
      t325 = t320 * t324
      t326 = t323 * x4
      t327 = t320 * t326
      t328 = t319 ** 2
      t331 = x1 * t253
      t333 = s * t328 * t14 * t331 * x4
      t335 = 0.1D1 / (-0.2D1 + t319)
      t336 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t322, t325, 0.0
     #D0, -t327, -t333)
      t339 = t328 ** 2
      t344 = log(-0.4D1 * t131 * t256 * x4 * t191 * t339)
      t346 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t322, t325, 0.0
     #D0, -t327, -t333)
      t352 = t31 * t3
      t353 = t7 * t335
      t366 = (-0.90D2 * t4 * t7 * (t335 * t336 - t344 * t335 * t346) + 0
     #.180D3 * t352 * t353 * t346) * t108 * t47 / 0.720D3 - t4 * t353 * 
     #t346 * t126 * t276 / 0.8D1
      t367 = FJET(XB1, XB2, s, t322, 0.0D0, t325, -t327, -t333, t366)
      t369 = -t119
      t370 = KAPPA2(x1, x2, t369, 0.10D1, z)
      t371 = s * t370
      t372 = t321 * t119
      t373 = t371 * t372
      t374 = t321 * x3
      t375 = t371 * t374
      t376 = t371 * t323
      t377 = t370 ** 2
      t381 = s * t377 * t14 * t331 * x3
      t383 = 0.1D1 / (-0.2D1 + t370)
      t384 = t7 * t383
      t386 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, -t376, t
     #375, 0.0D0, -t381)
      t391 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, -t376, t
     #375, 0.0D0, -t381)
      t395 = t377 ** 2
      t400 = log(-0.4D1 * t113 * t13 * t15 * t256 * t119 * t395)
      t414 = -t4 * t384 * t386 * t126 * t276 / 0.8D1 - (0.90D2 * t4 * t7
     # * (t383 * t391 - t400 * t383 * t386) - 0.180D3 * t352 * t384 * t3
     #86) * t126 * t108 / 0.720D3
      t415 = FJET(XB1, XB2, s, -t373, t375, -t376, 0.0D0, -t381, t414)
      t417 = KAPPA2(x1, x2, t369, t318, z)
      t418 = s * t417
      t419 = t418 * t372
      t420 = t418 * t374
      t421 = t418 * t324
      t422 = t418 * t326
      t423 = t417 ** 2
      t428 = cos(t8)
      t432 = Sqrt(x3 * t119 * x4 * t191)
      t437 = s * t423 * t14 * t331 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t428 * t432)
      t439 = 0.1D1 / (-0.2D1 + t417)
      t442 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t419, t421, t4
     #20, -t422, t437)
      t447 = FJET(XB1, XB2, s, -t419, t420, t421, -t422, t437, t4 * t7 *
     # t439 * t442 * t126 * t276 / 0.8D1)
      rrgq2qght5s5e0 = t189 * t188 + t250 * t249 + t316 * t315 + t367 * 
     #t366 + t415 * t414 + t447 * pi * t32 * t439 * t442 * t126 * t108 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s5em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t38 = t13 * t16
      t41 = log(0.4D1 * t36 * t11 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t57 = -0.1D1 + x3
      t61 = log(-0.4D1 * t56 * t38 * t57)
      t64 = log(0.4D1 * t56 * t38)
      t67 = 0.1D1 / x3
      t74 = log(0.4D1 * t14 * t16)
      t75 = t74 * pi
      t82 = lh ** 2
      t84 = pi ** 2
      t90 = t74 ** 2
      t98 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t102 = -(0.90D2 * t4 * t7 * (-t8 + t20 * t21) + t31) * t33 / 0.144
     #0D4 - (0.90D2 * t4 * t7 * (-t8 + t41 * t21) + t31) * t48 / 0.720D3
     # + t51 * t21 * t48 * t33 / 0.8D1 - t51 * t21 * (-t61 + t64) * t67 
     #/ 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t75) * t3 * t7 * t8 / 0.1440
     #D4 + (pi * (0.180D3 * t82 - 0.30D2 * t84) + 0.180D3 * t75 * lh + 0
     #.45D2 * t90 * pi) * t3 * t7 * t21 / 0.1440D4 + t4 * t7 * t98 / 0.1
     #6D2
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t102)
      t105 = -0.1D1 + x4
      t106 = t2 * t105
      t107 = t2 * x4
      t108 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, 0
     #.0D0, t107, 0.0D0)
      t112 = log(-0.4D1 * t14 * t17 * t105)
      t113 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, 0
     #.0D0, t107, 0.0D0)
      t129 = -(0.90D2 * t4 * t7 * (t108 - t112 * t113) - 0.180D3 * t27 *
     # t28 * t113) * t33 / 0.1440D4 - t51 * t113 * t48 * t33 / 0.8D1
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t106, t107, 0.0D0, t129)
      t132 = t2 * x1
      t133 = -0.1D1 + x1
      t134 = t2 * t133
      t135 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t132, -t134, 0.
     #0D0, 0.0D0, 0.0D0)
      t140 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t132, -t134, 0.
     #0D0, 0.0D0, 0.0D0)
      t142 = t133 ** 2
      t146 = log(0.4D1 * t14 * t16 * t36 * t142)
      t162 = -t51 * t135 * t67 * t48 / 0.8D1 - (0.90D2 * t4 * t7 * (t140
     # - t146 * t135) - 0.180D3 * t27 * t28 * t135) * t48 / 0.720D3 - t5
     #1 * t135 * t48 * t33 / 0.8D1
      t163 = FJET(XB1, XB2, s, t132, 0.0D0, -t134, 0.0D0, 0.0D0, t162)
      t166 = KAPPA2(x1, x2, 0.10D1, -t105, z)
      t167 = s * t166
      t168 = t1 * x1
      t169 = t167 * t168
      t170 = t1 * t133
      t172 = t167 * t170 * t105
      t174 = t167 * t170 * x4
      t175 = t166 ** 2
      t178 = x1 * t133
      t180 = s * t175 * t15 * t178 * x4
      t183 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t169, t172, 0.0
     #D0, -t174, -t180)
      t186 = 0.1D1 / (-0.2D1 + t166) * t183 * t48 * t33
      t189 = FJET(XB1, XB2, s, t169, 0.0D0, t172, -t174, -t180, -t51 * t
     #186 / 0.8D1)
      t195 = KAPPA2(x1, x2, -t57, 0.10D1, z)
      t196 = s * t195
      t198 = t196 * t168 * t57
      t200 = t196 * t168 * x3
      t201 = t196 * t170
      t202 = t195 ** 2
      t206 = s * t202 * t15 * t178 * x3
      t209 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t198, -t201, t
     #200, 0.0D0, -t206)
      t212 = 0.1D1 / (-0.2D1 + t195) * t209 * t67 * t48
      t215 = FJET(XB1, XB2, s, -t198, t200, -t201, 0.0D0, -t206, -t51 * 
     #t212 / 0.8D1)
      rrgq2qght5s5em1 = t103 * t102 + t130 * t129 + t163 * t162 - t189 *
     # pi * t28 * t186 / 0.8D1 - t215 * pi * t28 * t212 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s5em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t44 = t2 * x1
      t46 = t2 * (-0.1D1 + x1)
      t47 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t44, -t46, 0.0D0
     #, 0.0D0, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, t44, 0.0D0, -t46, 0.0D0, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t58 = t2 * (-0.1D1 + x4)
      t59 = t2 * x4
      t60 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t58, 0.0
     #D0, t59, 0.0D0)
      t62 = t7 * t60 * t14
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t58, t59, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrgq2qght5s5em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s5em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s5em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s5em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s5em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s6e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t28 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = lh ** 2
      t42 = pi ** 2
      t44 = 0.180D3 * t40 - 0.30D2 * t42
      t45 = pi * t44
      t46 = t34 * t25
      t47 = t45 * t46
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t12 * t17
      t56 = log(0.4D1 * t54)
      t61 = t56 ** 2
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t78 = 0.60D2 * lh * t42 - 0.240D3 * zeta3 - 0.120D3 * t40 * lh
      t79 = t78 * pi
      t80 = t79 * t46
      t91 = t4 * t7
      t92 = x3 * t8
      t93 = t92 * t11
      t94 = -0.1D1 + x3
      t95 = t94 * x4
      t99 = log(-0.4D1 * t93 * t17 * t95)
      t103 = log(0.4D1 * t93 * t18)
      t106 = 0.1D1 / x3
      t108 = t49 * t51
      t112 = t17 * t94
      t115 = log(-0.4D1 * t93 * t112)
      t117 = t115 ** 2
      t120 = t11 * t14
      t121 = t120 * t16
      t122 = t92 * t121
      t124 = log(0.4D1 * t122)
      t126 = t124 ** 2
      t143 = t16 * x4
      t146 = log(0.4D1 * t120 * t143)
      t151 = t146 ** 2
      t172 = x3 * t11
      t175 = log(0.4D1 * t172 * t18)
      t177 = t175 ** 2
      t180 = t172 * t14
      t185 = log(-0.4D1 * t180 * t16 * t94 * x4)
      t187 = t185 ** 2
      t204 = t7 * t22
      t212 = log(-0.4D1 * t172 * t112)
      t213 = t212 ** 2
      t216 = log(0.4D1 * t172 * t17)
      t217 = t216 ** 2
      t221 = t7 * t25
      t232 = t7 * t28
      t242 = log(0.4D1 * t121)
      t243 = t242 * pi
      t245 = t242 ** 2
      t246 = t245 * pi
      t250 = t245 * t242 * pi
      t270 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t274 = t42 ** 2
      t275 = t40 ** 2
      t288 = t245 ** 2
      t295 = (0.90D2 * t4 * t7 * (-t21 * t22 + t24 * t25 / 0.2D1 + t28) 
     #- 0.180D3 * t33 * t34 * (t22 - t21 * t25) + t47) * t49 * t51 / 0.7
     #20D3 + (t45 * t34 * (t22 - t56 * t25) + 0.90D2 * t4 * t7 * (t61 * 
     #t22 / 0.2D1 + t64 - t61 * t56 * t25 / 0.6D1 - t56 * t28) + t80 - 0
     #.180D3 * t33 * t34 * (-t56 * t22 + t61 * t25 / 0.2D1 + t28)) * t49
     # / 0.720D3 - t91 * (-t99 * t25 + t103 * t25) * t106 * t108 / 0.8D1
     # + (0.90D2 * t4 * t7 * (t115 * t22 - t117 * t25 / 0.2D1 - t124 * t
     #22 + t126 * t25 / 0.2D1) - 0.180D3 * t33 * t34 * (t115 * t25 - t12
     #4 * t25)) * t106 * t49 / 0.720D3 - (t45 * t34 * (-t22 + t146 * t25
     #) + 0.90D2 * t4 * t7 * (-t151 * t22 / 0.2D1 - t64 + t151 * t146 * 
     #t25 / 0.6D1 + t146 * t28) - t80 - 0.180D3 * t33 * t34 * (t146 * t2
     #2 - t151 * t25 / 0.2D1 - t28)) * t51 / 0.1440D4 - (0.90D2 * t4 * t
     #7 * (t175 * t22 - t177 * t25 / 0.2D1 - t185 * t22 + t187 * t25 / 0
     #.2D1) - 0.180D3 * t33 * t34 * (t175 * t25 - t185 * t25)) * t106 * 
     #t51 / 0.1440D4 + ((-0.90D2 * t4 * t204 + 0.180D3 * t33 * t46) * (t
     #213 / 0.2D1 - t217 / 0.2D1) - 0.90D2 * t4 * t221 * (t217 * t216 / 
     #0.6D1 - t213 * t212 / 0.6D1) + (0.180D3 * t33 * t34 * t22 - t47 - 
     #0.90D2 * t4 * t232) * (-t212 + t216)) * t106 / 0.1440D4 + (t79 - t
     #243 * t44 - 0.90D2 * t246 * lh - 0.15D2 * t250) * t3 * t204 / 0.14
     #40D4 + (t45 + 0.180D3 * t243 * lh + 0.45D2 * t246) * t3 * t232 / 0
     #.1440D4 + (-0.180D3 * t33 - 0.90D2 * t243) * t3 * t7 * t64 / 0.144
     #0D4 + t4 * t7 * t270 / 0.16D2 + (pi * (t274 + 0.60D2 * t275 + 0.48
     #0D3 * lh * zeta3 - 0.60D2 * t40 * t42) - t243 * t78 + t246 * t44 /
     # 0.2D1 + 0.30D2 * t250 * lh + 0.15D2 / 0.4D1 * t288 * pi) * t3 * t
     #221 / 0.1440D4
      t296 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t295)
      t298 = t2 * x4
      t299 = -0.1D1 + x4
      t300 = t2 * t299
      t301 = t12 * t14
      t302 = t143 * t299
      t305 = log(-0.4D1 * t301 * t302)
      t306 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t308 = t305 ** 2
      t309 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t312 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t322 = t34 * t309
      t328 = x4 * t299
      t332 = log(-0.4D1 * t93 * t17 * t328)
      t334 = t92 * t120
      t336 = t143 * t299 * t94
      t339 = log(0.4D1 * t334 * t336)
      t348 = log(-0.4D1 * t120 * t302)
      t353 = t348 ** 2
      t356 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t378 = log(-0.4D1 * t180 * t302)
      t380 = t378 ** 2
      t385 = log(0.4D1 * t180 * t336)
      t387 = t385 ** 2
      t404 = (0.90D2 * t4 * t7 * (t305 * t306 - t308 * t309 / 0.2D1 - t3
     #12) - 0.180D3 * t33 * t34 * (-t306 + t305 * t309) - t45 * t322) * 
     #t49 * t51 / 0.720D3 - t91 * (-t332 * t309 + t339 * t309) * t106 * 
     #t108 / 0.8D1 - (t45 * t34 * (t306 - t348 * t309) + 0.90D2 * t4 * t
     #7 * (t353 * t306 / 0.2D1 + t356 - t353 * t348 * t309 / 0.6D1 - t34
     #8 * t312) + t79 * t322 - 0.180D3 * t33 * t34 * (-t348 * t306 + t35
     #3 * t309 / 0.2D1 + t312)) * t51 / 0.1440D4 - (0.90D2 * t4 * t7 * (
     #-t378 * t306 + t380 * t309 / 0.2D1 + t385 * t306 - t387 * t309 / 0
     #.2D1) - 0.180D3 * t33 * t34 * (-t378 * t309 + t385 * t309)) * t106
     # * t51 / 0.1440D4
      t405 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t298, -t300, 0.0D0, t404)
      t407 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t408 = s * t407
      t409 = t1 * x1
      t410 = t408 * t409
      t411 = -0.1D1 + x1
      t412 = t1 * t411
      t413 = t408 * t412
      t414 = t407 ** 2
      t418 = s * t414 * t15 * x1 * t411
      t419 = t411 ** 2
      t420 = t16 * t419
      t421 = t414 ** 2
      t423 = t420 * x4 * t421
      t426 = log(0.4D1 * t301 * t423)
      t428 = 0.1D1 / (-0.2D1 + t407)
      t429 = t426 * t428
      t430 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t410, 0.0D0, 0.
     #0D0, -t413, -t418)
      t432 = t426 ** 2
      t434 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t410, 0.0D0, 0.
     #0D0, -t413, -t418)
      t437 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t410, 0.0D0, 0.
     #0D0, -t413, -t418)
      t438 = t428 * t437
      t443 = t428 * t430
      t449 = t45 * t3
      t451 = t7 * t428 * t434
      t452 = t449 * t451
      t459 = log(0.4D1 * t301 * t420 * t421)
      t460 = t459 * t428
      t465 = t459 ** 2
      t466 = t465 * t428
      t469 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, t410, 0.0D0, 0.
     #0D0, -t413, -t418)
      t493 = log(0.4D1 * t334 * t423)
      t500 = t33 * t3
      t510 = log(0.4D1 * t93 * t17 * t419 * t421)
      t511 = t510 * t428
      t513 = t510 ** 2
      t530 = (0.90D2 * t4 * t7 * (-t429 * t430 + t432 * t428 * t434 / 0.
     #2D1 + t438) - 0.180D3 * t33 * t34 * (t443 - t429 * t434) + t452) *
     # t49 * t51 / 0.720D3 + (t45 * t34 * (t443 - t460 * t434) + 0.90D2 
     #* t4 * t7 * (t466 * t430 / 0.2D1 + t428 * t469 - t465 * t459 * t42
     #8 * t434 / 0.6D1 - t460 * t437) + t79 * t3 * t451 - 0.180D3 * t33 
     #* t34 * (-t460 * t430 + t466 * t434 / 0.2D1 + t438)) * t49 / 0.720
     #D3 - (0.90D2 * t4 * t7 * (-t443 + t493 * t428 * t434) + 0.180D3 * 
     #t500 * t451) * t106 * t108 / 0.720D3 + (0.90D2 * t4 * t7 * (-t511 
     #* t430 + t513 * t428 * t434 / 0.2D1 + t438) - 0.180D3 * t33 * t34 
     #* (t443 - t511 * t434) + t452) * t106 * t49 / 0.720D3
      t531 = FJET(XB1, XB2, s, t410, 0.0D0, 0.0D0, -t413, -t418, t530)
      t533 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t534 = s * t533
      t535 = t534 * t409
      t536 = t412 * x4
      t537 = t534 * t536
      t538 = t412 * t299
      t539 = t534 * t538
      t540 = t533 ** 2
      t543 = x1 * t411
      t545 = s * t540 * t15 * t543 * t299
      t546 = t540 ** 2
      t551 = log(-0.4D1 * t54 * t328 * t419 * t546)
      t553 = 0.1D1 / (-0.2D1 + t533)
      t554 = t551 * t553
      t555 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t535, -t537, 0.
     #0D0, t539, t545)
      t557 = t551 ** 2
      t559 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t535, -t537, 0.
     #0D0, t539, t545)
      t562 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t535, -t537, 0.
     #0D0, t539, t545)
      t568 = t553 * t555
      t575 = t7 * t553 * t559
      t585 = log(-0.4D1 * t334 * t143 * t299 * t419 * t546)
      t598 = (-0.90D2 * t4 * t7 * (-t554 * t555 + t557 * t553 * t559 / 0
     #.2D1 + t553 * t562) + 0.180D3 * t33 * t34 * (t568 - t554 * t559) -
     # t449 * t575) * t49 * t51 / 0.720D3 - (0.90D2 * t4 * t7 * (t568 - 
     #t585 * t553 * t559) - 0.180D3 * t500 * t575) * t106 * t108 / 0.720
     #D3
      t599 = FJET(XB1, XB2, s, t535, 0.0D0, -t537, t539, t545, t598)
      t601 = -t94
      t602 = KAPPA2(x1, x2, t601, 0.0D0, z)
      t603 = s * t602
      t604 = t409 * t94
      t605 = t603 * t604
      t606 = t409 * x3
      t607 = t603 * t606
      t608 = t603 * t412
      t609 = t602 ** 2
      t613 = s * t609 * t15 * t543 * t94
      t615 = 0.1D1 / (-0.2D1 + t602)
      t616 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t605, 0.0D0, t
     #607, -t608, t613)
      t617 = t615 * t616
      t618 = t609 ** 2
      t623 = log(-0.4D1 * t334 * t420 * t95 * t618)
      t625 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t605, 0.0D0, t
     #607, -t608, t613)
      t632 = t7 * t615 * t625
      t642 = log(-0.4D1 * t334 * t420 * t94 * t618)
      t643 = t642 * t615
      t645 = t642 ** 2
      t649 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t605, 0.0D0, t
     #607, -t608, t613)
      t665 = -(0.90D2 * t4 * t7 * (t617 - t623 * t615 * t625) - 0.180D3 
     #* t500 * t632) * t106 * t108 / 0.720D3 + (0.90D2 * t4 * t7 * (t643
     # * t616 - t645 * t615 * t625 / 0.2D1 - t615 * t649) - 0.180D3 * t3
     #3 * t34 * (-t617 + t643 * t625) - t449 * t632) * t106 * t49 / 0.72
     #0D3
      t666 = FJET(XB1, XB2, s, -t605, t607, 0.0D0, -t608, t613, t665)
      t668 = KAPPA2(x1, x2, t601, x4, z)
      t669 = s * t668
      t670 = t669 * t604
      t671 = t669 * t606
      t672 = t669 * t536
      t673 = t669 * t538
      t674 = t668 ** 2
      t679 = cos(t9)
      t682 = Sqrt(x3 * t94 * t328)
      t687 = s * t674 * t15 * t543 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t679 * t682)
      t689 = 0.1D1 / (-0.2D1 + t668)
      t690 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t670, -t672, t
     #671, t673, t687)
      t693 = t674 ** 2
      t698 = log(0.4D1 * t122 * t328 * t419 * t94 * t693)
      t700 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t670, -t672, t
     #671, t673, t687)
      t710 = -0.90D2 * t4 * t7 * (t689 * t690 - t698 * t689 * t700) + 0.
     #180D3 * t500 * t7 * t689 * t700
      t714 = FJET(XB1, XB2, s, -t670, t671, -t672, t673, t687, -t710 * t
     #106 * t108 / 0.720D3)
      rrgq2qght5s6e1 = t296 * t295 + t405 * t404 + t530 * t531 + t599 * 
     #t598 + t666 * t665 - t714 * t710 * t106 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s6e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t15 * x4
      t19 = log(0.4D1 * t13 * t16)
      t20 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = t19 ** 2
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t26 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = lh ** 2
      t40 = pi ** 2
      t42 = 0.180D3 * t38 - 0.30D2 * t40
      t43 = pi * t42
      t44 = t32 * t23
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t50 = t13 * t15
      t52 = log(0.4D1 * t50)
      t53 = t52 * pi
      t56 = t52 ** 2
      t57 = t56 * pi
      t61 = t7 * t20
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t83 = t7 * t23
      t93 = x1 ** 2
      t94 = t93 * t10
      t95 = t12 * t15
      t96 = t95 * x4
      t99 = log(0.4D1 * t94 * t96)
      t106 = 0.180D3 * t31 * t44
      t108 = 0.1D1 / x1
      t112 = t4 * t7
      t113 = x3 * t93
      t114 = t113 * t10
      t115 = -0.1D1 + x3
      t116 = t95 * t115
      t119 = log(-0.4D1 * t114 * t116)
      t123 = log(0.4D1 * t113 * t50)
      t126 = 0.1D1 / x3
      t131 = t94 * t95
      t133 = log(0.4D1 * t131)
      t135 = t133 ** 2
      t150 = x3 * t10
      t153 = log(0.4D1 * t150 * t96)
      t155 = t150 * t12
      t160 = log(-0.4D1 * t155 * t15 * t115 * x4)
      t169 = log(-0.4D1 * t150 * t116)
      t170 = t169 ** 2
      t173 = log(0.4D1 * t150 * t95)
      t174 = t173 ** 2
      t188 = -(0.90D2 * t4 * t7 * (t19 * t20 - t22 * t23 / 0.2D1 - t26) 
     #- 0.180D3 * t31 * t32 * (-t20 + t19 * t23) - t45) * t47 / 0.1440D4
     # + (t43 + 0.180D3 * t53 * lh + 0.45D2 * t57) * t3 * t61 / 0.1440D4
     # + t4 * t7 * t64 / 0.16D2 + (pi * (0.60D2 * t40 * lh - 0.240D3 * z
     #eta3 - 0.120D3 * t38 * lh) - t53 * t42 - 0.90D2 * t57 * lh - 0.15D
     #2 * t56 * t52 * pi) * t3 * t83 / 0.1440D4 + (-0.180D3 * t31 - 0.90
     #D2 * t53) * t3 * t7 * t26 / 0.1440D4 + (0.90D2 * t4 * t7 * (t20 - 
     #t99 * t23) - t106) * t108 * t47 / 0.720D3 + t112 * (t119 * t23 - t
     #123 * t23) * t126 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (-t133 * t2
     #0 + t135 * t23 / 0.2D1 + t26) - 0.180D3 * t31 * t32 * (t20 - t133 
     #* t23) + t45) * t108 / 0.720D3 - t112 * (t153 * t23 - t160 * t23) 
     #* t126 * t47 / 0.16D2 + (-0.90D2 * t4 * t83 * (t170 / 0.2D1 - t174
     # / 0.2D1) + (-0.90D2 * t4 * t61 + t106) * (-t169 + t173)) * t126 /
     # 0.1440D4
      t189 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t188)
      t191 = t2 * x4
      t192 = -0.1D1 + x4
      t193 = t2 * t192
      t194 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t191, 0.
     #0D0, -t193, 0.0D0)
      t195 = t94 * t12
      t196 = t16 * t192
      t199 = log(-0.4D1 * t195 * t196)
      t200 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t191, 0.
     #0D0, -t193, 0.0D0)
      t206 = t32 * t200
      t215 = log(-0.4D1 * t155 * t196)
      t221 = log(0.4D1 * t155 * t16 * t192 * t115)
      t230 = log(-0.4D1 * t13 * t196)
      t232 = t230 ** 2
      t235 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t191, 0.
     #0D0, -t193, 0.0D0)
      t249 = (0.90D2 * t4 * t7 * (-t194 + t199 * t200) + 0.180D3 * t31 *
     # t206) * t108 * t47 / 0.720D3 - t112 * (-t215 * t200 + t221 * t200
     #) * t126 * t47 / 0.16D2 - (0.90D2 * t4 * t7 * (-t230 * t194 + t232
     # * t200 / 0.2D1 + t235) - 0.180D3 * t31 * t32 * (t194 - t230 * t20
     #0) + t43 * t206) * t47 / 0.1440D4
      t250 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t191, -t193, 0.0D0, t249)
      t252 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t253 = s * t252
      t254 = t1 * x1
      t255 = t253 * t254
      t256 = -0.1D1 + x1
      t257 = t1 * t256
      t258 = t253 * t257
      t259 = t252 ** 2
      t263 = s * t259 * t14 * x1 * t256
      t265 = 0.1D1 / (-0.2D1 + t252)
      t266 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t255, 0.0D0, 0.
     #0D0, -t258, -t263)
      t267 = t265 * t266
      t268 = t256 ** 2
      t269 = t15 * t268
      t270 = t259 ** 2
      t275 = log(0.4D1 * t195 * t269 * x4 * t270)
      t277 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t255, 0.0D0, 0.
     #0D0, -t258, -t263)
      t283 = t31 * t3
      t284 = t7 * t265
      t285 = t284 * t277
      t287 = 0.180D3 * t283 * t285
      t294 = t108 * t47
      t302 = log(0.4D1 * t114 * t95 * t268 * t270)
      t316 = log(0.4D1 * t195 * t269 * t270)
      t317 = t316 * t265
      t319 = t316 ** 2
      t323 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t255, 0.0D0, 0.
     #0D0, -t258, -t263)
      t339 = (0.90D2 * t4 * t7 * (t267 - t275 * t265 * t277) - t287) * t
     #108 * t47 / 0.720D3 + t4 * t284 * t277 * t126 * t294 / 0.8D1 + (0.
     #90D2 * t4 * t7 * (t267 - t302 * t265 * t277) - t287) * t126 * t108
     # / 0.720D3 + (0.90D2 * t4 * t7 * (-t317 * t266 + t319 * t265 * t27
     #7 / 0.2D1 + t265 * t323) - 0.180D3 * t31 * t32 * (t267 - t317 * t2
     #77) + t43 * t3 * t285) * t108 / 0.720D3
      t340 = FJET(XB1, XB2, s, t255, 0.0D0, 0.0D0, -t258, -t263, t339)
      t342 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t343 = s * t342
      t344 = t343 * t254
      t345 = t257 * x4
      t346 = t343 * t345
      t347 = t257 * t192
      t348 = t343 * t347
      t349 = t342 ** 2
      t352 = x1 * t256
      t354 = s * t349 * t14 * t352 * t192
      t356 = 0.1D1 / (-0.2D1 + t342)
      t357 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t344, -t346, 0.
     #0D0, t348, t354)
      t359 = x4 * t192
      t360 = t349 ** 2
      t365 = log(-0.4D1 * t131 * t359 * t268 * t360)
      t367 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t344, -t346, 0.
     #0D0, t348, t354)
      t373 = t7 * t356
      t386 = (-0.90D2 * t4 * t7 * (t356 * t357 - t365 * t356 * t367) + 0
     #.180D3 * t283 * t373 * t367) * t108 * t47 / 0.720D3 - t4 * t373 * 
     #t367 * t126 * t294 / 0.8D1
      t387 = FJET(XB1, XB2, s, t344, 0.0D0, -t346, t348, t354, t386)
      t389 = -t115
      t390 = KAPPA2(x1, x2, t389, 0.0D0, z)
      t391 = s * t390
      t392 = t254 * t115
      t393 = t391 * t392
      t394 = t254 * x3
      t395 = t391 * t394
      t396 = t391 * t257
      t397 = t390 ** 2
      t401 = s * t397 * t14 * t352 * t115
      t403 = 0.1D1 / (-0.2D1 + t390)
      t404 = t7 * t403
      t406 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t393, 0.0D0, t
     #395, -t396, t401)
      t411 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t393, 0.0D0, t
     #395, -t396, t401)
      t414 = t397 ** 2
      t419 = log(-0.4D1 * t113 * t13 * t269 * t115 * t414)
      t433 = -t4 * t404 * t406 * t126 * t294 / 0.8D1 + (0.90D2 * t4 * t7
     # * (-t403 * t411 + t419 * t403 * t406) + 0.180D3 * t283 * t404 * t
     #406) * t126 * t108 / 0.720D3
      t434 = FJET(XB1, XB2, s, -t393, t395, 0.0D0, -t396, t401, t433)
      t436 = KAPPA2(x1, x2, t389, x4, z)
      t437 = s * t436
      t438 = t437 * t392
      t439 = t437 * t394
      t440 = t437 * t345
      t441 = t437 * t347
      t442 = t436 ** 2
      t447 = cos(t8)
      t450 = Sqrt(x3 * t115 * t359)
      t455 = s * t442 * t14 * t352 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t447 * t450)
      t457 = 0.1D1 / (-0.2D1 + t436)
      t460 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t438, -t440, t
     #439, t441, t455)
      t465 = FJET(XB1, XB2, s, -t438, t439, -t440, t441, t455, t4 * t7 *
     # t457 * t460 * t126 * t294 / 0.8D1)
      rrgq2qght5s6e0 = t189 * t188 + t250 * t249 + t340 * t339 + t387 * 
     #t386 + t434 * t433 + t465 * pi * t32 * t457 * t460 * t126 * t108 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s6em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t37 = t36 * t11
      t38 = t13 * t16
      t41 = log(0.4D1 * t37 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t57 = -0.1D1 + x3
      t61 = log(-0.4D1 * t56 * t38 * t57)
      t64 = log(0.4D1 * t56 * t38)
      t67 = 0.1D1 / x3
      t74 = log(0.4D1 * t14 * t16)
      t75 = t74 * pi
      t82 = lh ** 2
      t84 = pi ** 2
      t90 = t74 ** 2
      t98 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t102 = -(0.90D2 * t4 * t7 * (-t8 + t20 * t21) + t31) * t33 / 0.144
     #0D4 + (0.90D2 * t4 * t7 * (t8 - t41 * t21) - t31) * t48 / 0.720D3 
     #+ t51 * t21 * t48 * t33 / 0.8D1 - t51 * t21 * (-t61 + t64) * t67 /
     # 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t75) * t3 * t7 * t8 / 0.1440D
     #4 + (pi * (0.180D3 * t82 - 0.30D2 * t84) + 0.180D3 * t75 * lh + 0.
     #45D2 * t90 * pi) * t3 * t7 * t21 / 0.1440D4 + t4 * t7 * t98 / 0.16
     #D2
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t102)
      t105 = t2 * x4
      t106 = -0.1D1 + x4
      t107 = t2 * t106
      t108 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t112 = log(-0.4D1 * t14 * t17 * t106)
      t113 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t129 = -(0.90D2 * t4 * t7 * (t108 - t112 * t113) - 0.180D3 * t27 *
     # t28 * t113) * t33 / 0.1440D4 - t51 * t113 * t48 * t33 / 0.8D1
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t105, -t107, 0.0D0, t129)
      t132 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t133 = s * t132
      t134 = t1 * x1
      t135 = t133 * t134
      t136 = -0.1D1 + x1
      t137 = t1 * t136
      t138 = t133 * t137
      t139 = t132 ** 2
      t143 = s * t139 * t15 * x1 * t136
      t145 = 0.1D1 / (-0.2D1 + t132)
      t146 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t135, 0.0D0, 0.
     #0D0, -t138, -t143)
      t147 = t145 * t146
      t148 = t67 * t48
      t152 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t135, 0.0D0, 0.
     #0D0, -t138, -t143)
      t155 = t136 ** 2
      t157 = t139 ** 2
      t161 = log(0.4D1 * t37 * t13 * t16 * t155 * t157)
      t176 = t48 * t33
      t180 = t51 * t147 * t148 / 0.8D1 + (0.90D2 * t4 * t7 * (t145 * t15
     #2 - t161 * t145 * t146) - 0.180D3 * t27 * t3 * t7 * t145 * t146) *
     # t48 / 0.720D3 + t51 * t147 * t176 / 0.8D1
      t181 = FJET(XB1, XB2, s, t135, 0.0D0, 0.0D0, -t138, -t143, t180)
      t183 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t184 = s * t183
      t185 = t184 * t134
      t187 = t184 * t137 * x4
      t189 = t184 * t137 * t106
      t190 = t183 ** 2
      t193 = x1 * t136
      t195 = s * t190 * t15 * t193 * t106
      t198 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t185, -t187, 0.
     #0D0, t189, t195)
      t200 = 0.1D1 / (-0.2D1 + t183) * t198 * t176
      t203 = FJET(XB1, XB2, s, t185, 0.0D0, -t187, t189, t195, -t51 * t2
     #00 / 0.8D1)
      t209 = KAPPA2(x1, x2, -t57, 0.0D0, z)
      t210 = s * t209
      t212 = t210 * t134 * t57
      t214 = t210 * t134 * x3
      t215 = t210 * t137
      t216 = t209 ** 2
      t220 = s * t216 * t15 * t193 * t57
      t223 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t212, 0.0D0, t
     #214, -t215, t220)
      t225 = 0.1D1 / (-0.2D1 + t209) * t223 * t148
      t228 = FJET(XB1, XB2, s, -t212, t214, 0.0D0, -t215, t220, -t51 * t
     #225 / 0.8D1)
      rrgq2qght5s6em1 = t103 * t102 + t130 * t129 + t181 * t180 - t203 *
     # pi * t28 * t200 / 0.8D1 - t228 * pi * t28 * t225 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s6em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t45 = s * t44
      t47 = t45 * t1 * x1
      t48 = -0.1D1 + x1
      t50 = t45 * t1 * t48
      t51 = t44 ** 2
      t55 = s * t51 * t30 * x1 * t48
      t58 = 0.1D1 / (-0.2D1 + t44)
      t59 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t47, 0.0D0, 0.0D
     #0, -t50, -t55)
      t64 = FJET(XB1, XB2, s, t47, 0.0D0, 0.0D0, -t50, -t55, t4 * t7 * t
     #58 * t59 * t10 / 0.8D1)
      t72 = t2 * x4
      t74 = t2 * (-0.1D1 + x4)
      t75 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t72, 0.0D
     #0, -t74, 0.0D0)
      t77 = t7 * t75 * t14
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t72, -t74, 0.0D0, -t4 * t77 
     #/ 0.16D2)
      rrgq2qght5s6em2 = t42 * t41 + t64 * pi * t3 * t7 * t58 * t59 * t10
     # / 0.8D1 - t80 * pi * t3 * t77 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s6em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s6em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s6em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s6em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s7e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = lh ** 2
      t42 = pi ** 2
      t44 = 0.180D3 * t40 - 0.30D2 * t42
      t45 = pi * t44
      t46 = t34 * t25
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t12 * t17
      t56 = log(0.4D1 * t54)
      t61 = t56 ** 2
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t78 = 0.60D2 * lh * t42 - 0.240D3 * zeta3 - 0.120D3 * t40 * lh
      t79 = t78 * pi
      t80 = t79 * t46
      t91 = t4 * t7
      t92 = x3 * t8
      t93 = t92 * t11
      t96 = log(0.4D1 * t93 * t18)
      t98 = -0.1D1 + x3
      t99 = t98 * x4
      t103 = log(-0.4D1 * t93 * t17 * t99)
      t106 = 0.1D1 / x3
      t108 = t49 * t51
      t112 = t11 * t14
      t113 = t112 * t16
      t114 = t92 * t113
      t116 = log(0.4D1 * t114)
      t118 = t116 ** 2
      t121 = t17 * t98
      t124 = log(-0.4D1 * t93 * t121)
      t126 = t124 ** 2
      t143 = t16 * x4
      t146 = log(0.4D1 * t112 * t143)
      t151 = t146 ** 2
      t172 = x3 * t11
      t173 = t172 * t14
      t178 = log(-0.4D1 * t173 * t16 * t98 * x4)
      t180 = t178 ** 2
      t185 = log(0.4D1 * t172 * t18)
      t187 = t185 ** 2
      t204 = t172 * t17
      t206 = log(0.4D1 * t204)
      t208 = t206 ** 2
      t213 = log(-0.4D1 * t172 * t121)
      t215 = t213 ** 2
      t247 = log(0.4D1 * t113)
      t248 = t247 * pi
      t250 = t247 ** 2
      t251 = t250 * pi
      t255 = t250 * t247 * pi
      t277 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t281 = t42 ** 2
      t282 = t40 ** 2
      t295 = t250 ** 2
      t303 = -(0.90D2 * t4 * t7 * (t21 * t22 - t24 * t25 / 0.2D1 - t28) 
     #- 0.180D3 * t33 * t34 * (-t22 + t21 * t25) - t45 * t46) * t49 * t5
     #1 / 0.720D3 - (t45 * t34 * (-t22 + t56 * t25) + 0.90D2 * t4 * t7 *
     # (-t61 * t22 / 0.2D1 - t64 + t61 * t56 * t25 / 0.6D1 + t56 * t28) 
     #- t80 - 0.180D3 * t33 * t34 * (t56 * t22 - t61 * t25 / 0.2D1 - t28
     #)) * t49 / 0.720D3 - t91 * (t96 * t25 - t103 * t25) * t106 * t108 
     #/ 0.8D1 - (0.90D2 * t4 * t7 * (t116 * t22 - t118 * t25 / 0.2D1 - t
     #124 * t22 + t126 * t25 / 0.2D1) - 0.180D3 * t33 * t34 * (t116 * t2
     #5 - t124 * t25)) * t106 * t49 / 0.720D3 + (t45 * t34 * (t22 - t146
     # * t25) + 0.90D2 * t4 * t7 * (t151 * t22 / 0.2D1 + t64 - t151 * t1
     #46 * t25 / 0.6D1 - t146 * t28) + t80 - 0.180D3 * t33 * t34 * (-t14
     #6 * t22 + t151 * t25 / 0.2D1 + t28)) * t51 / 0.1440D4 + (0.90D2 * 
     #t4 * t7 * (t178 * t22 - t180 * t25 / 0.2D1 - t185 * t22 + t187 * t
     #25 / 0.2D1) - 0.180D3 * t33 * t34 * (t178 * t25 - t185 * t25)) * t
     #106 * t51 / 0.1440D4 + (-0.180D3 * t33 * t34 * (-t206 * t22 + t208
     # * t25 / 0.2D1 + t213 * t22 - t215 * t25 / 0.2D1) + 0.90D2 * t4 * 
     #t7 * (-t215 * t22 / 0.2D1 + t215 * t213 * t25 / 0.6D1 + t213 * t28
     # + t208 * t22 / 0.2D1 - t208 * t206 * t25 / 0.6D1 - t206 * t28) + 
     #t45 * t34 * (-t206 * t25 + t213 * t25)) * t106 / 0.1440D4 + (t79 -
     # t248 * t44 - 0.90D2 * t251 * lh - 0.15D2 * t255) * t3 * t7 * t22 
     #/ 0.1440D4 + (t45 + 0.180D3 * t248 * lh + 0.45D2 * t251) * t3 * t7
     # * t28 / 0.1440D4 + (-0.180D3 * t33 - 0.90D2 * t248) * t3 * t7 * t
     #64 / 0.1440D4 + t4 * t7 * t277 / 0.16D2 + (pi * (t281 + 0.60D2 * t
     #282 + 0.480D3 * lh * zeta3 - 0.60D2 * t40 * t42) - t248 * t78 + t2
     #51 * t44 / 0.2D1 + 0.30D2 * t255 * lh + 0.15D2 / 0.4D1 * t295 * pi
     #) * t3 * t7 * t25 / 0.1440D4
      t304 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t303)
      t306 = -0.1D1 + x4
      t307 = t2 * t306
      t308 = t2 * x4
      t310 = t143 * t306
      t313 = log(-0.4D1 * t12 * t14 * t310)
      t314 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, 0
     #.0D0, t308, 0.0D0)
      t316 = t313 ** 2
      t317 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, 0
     #.0D0, t308, 0.0D0)
      t320 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, 0
     #.0D0, t308, 0.0D0)
      t330 = t34 * t317
      t338 = t143 * t306 * t98
      t341 = log(0.4D1 * t92 * t112 * t338)
      t343 = x4 * t306
      t347 = log(-0.4D1 * t93 * t17 * t343)
      t356 = log(-0.4D1 * t112 * t310)
      t361 = t356 ** 2
      t364 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t307, 0
     #.0D0, t308, 0.0D0)
      t386 = log(-0.4D1 * t173 * t310)
      t388 = t386 ** 2
      t393 = log(0.4D1 * t173 * t338)
      t395 = t393 ** 2
      t412 = -(0.90D2 * t4 * t7 * (-t313 * t314 + t316 * t317 / 0.2D1 + 
     #t320) - 0.180D3 * t33 * t34 * (t314 - t313 * t317) + t45 * t330) *
     # t49 * t51 / 0.720D3 - t91 * (t341 * t317 - t347 * t317) * t106 * 
     #t108 / 0.8D1 + (-t45 * t34 * (t314 - t356 * t317) - 0.90D2 * t4 * 
     #t7 * (t361 * t314 / 0.2D1 + t364 - t361 * t356 * t317 / 0.6D1 - t3
     #56 * t320) - t79 * t330 + 0.180D3 * t33 * t34 * (-t356 * t314 + t3
     #61 * t317 / 0.2D1 + t320)) * t51 / 0.1440D4 + (0.90D2 * t4 * t7 * 
     #(t386 * t314 - t388 * t317 / 0.2D1 - t393 * t314 + t395 * t317 / 0
     #.2D1) - 0.180D3 * t33 * t34 * (t386 * t317 - t393 * t317)) * t106 
     #* t51 / 0.1440D4
      t413 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t307, t308, 0.0D0, t412)
      t415 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t416 = s * t415
      t417 = t1 * x1
      t418 = t416 * t417
      t419 = -0.1D1 + x1
      t420 = t1 * t419
      t421 = t416 * t420
      t422 = t415 ** 2
      t426 = s * t422 * t15 * x1 * t419
      t427 = t419 ** 2
      t428 = t8 * t427
      t429 = t422 ** 2
      t431 = t428 * x4 * t429
      t434 = log(0.4D1 * t113 * t431)
      t436 = 0.1D1 / (-0.2D1 + t415)
      t437 = t434 * t436
      t438 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t421, t
     #418, 0.0D0, -t426)
      t440 = t434 ** 2
      t442 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t421, t
     #418, 0.0D0, -t426)
      t445 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t421, t
     #418, 0.0D0, -t426)
      t446 = t436 * t445
      t451 = t436 * t438
      t457 = t45 * t3
      t459 = t7 * t436 * t442
      t460 = t457 * t459
      t467 = log(0.4D1 * t113 * t428 * t429)
      t468 = t467 * t436
      t473 = t467 ** 2
      t474 = t473 * t436
      t477 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t421, t
     #418, 0.0D0, -t426)
      t501 = log(0.4D1 * t204 * t431)
      t508 = t33 * t3
      t519 = log(0.4D1 * t173 * t16 * t8 * t427 * t429)
      t520 = t519 * t436
      t522 = t519 ** 2
      t539 = -(0.90D2 * t4 * t7 * (t437 * t438 - t440 * t436 * t442 / 0.
     #2D1 - t446) - 0.180D3 * t33 * t34 * (-t451 + t437 * t442) - t460) 
     #* t49 * t51 / 0.720D3 - (-t45 * t34 * (t451 - t468 * t442) - 0.90D
     #2 * t4 * t7 * (t474 * t438 / 0.2D1 + t436 * t477 - t473 * t467 * t
     #436 * t442 / 0.6D1 - t468 * t445) - t79 * t3 * t459 + 0.180D3 * t3
     #3 * t34 * (-t468 * t438 + t474 * t442 / 0.2D1 + t446)) * t49 / 0.7
     #20D3 - (0.90D2 * t4 * t7 * (-t451 + t501 * t436 * t442) + 0.180D3 
     #* t508 * t459) * t106 * t108 / 0.720D3 - (0.90D2 * t4 * t7 * (t520
     # * t438 - t522 * t436 * t442 / 0.2D1 - t446) - 0.180D3 * t33 * t34
     # * (-t451 + t520 * t442) - t460) * t106 * t49 / 0.720D3
      t540 = FJET(XB1, XB2, s, 0.0D0, t418, -t421, 0.0D0, -t426, t539)
      t542 = -t306
      t543 = KAPPA2(x1, x2, 0.0D0, t542, z)
      t544 = s * t543
      t545 = t544 * t417
      t546 = t420 * t306
      t547 = t544 * t546
      t548 = t420 * x4
      t549 = t544 * t548
      t550 = t543 ** 2
      t553 = x1 * t419
      t555 = s * t550 * t15 * t553 * t306
      t557 = t550 ** 2
      t562 = log(-0.4D1 * t54 * t427 * x4 * t306 * t557)
      t564 = 0.1D1 / (-0.2D1 + t543)
      t565 = t562 * t564
      t566 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t547, t5
     #45, -t549, t555)
      t568 = t562 ** 2
      t570 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t547, t5
     #45, -t549, t555)
      t573 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t547, t5
     #45, -t549, t555)
      t579 = t564 * t566
      t586 = t7 * t564 * t570
      t595 = log(-0.4D1 * t204 * t428 * t343 * t557)
      t608 = -(-0.90D2 * t4 * t7 * (t565 * t566 - t568 * t564 * t570 / 0
     #.2D1 - t564 * t573) + 0.180D3 * t33 * t34 * (-t579 + t565 * t570) 
     #+ t457 * t586) * t49 * t51 / 0.720D3 - (0.90D2 * t4 * t7 * (t579 -
     # t595 * t564 * t570) - 0.180D3 * t508 * t586) * t106 * t108 / 0.72
     #0D3
      t609 = FJET(XB1, XB2, s, 0.0D0, t545, t547, -t549, t555, t608)
      t611 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t612 = s * t611
      t613 = t417 * x3
      t614 = t612 * t613
      t615 = t417 * t98
      t616 = t612 * t615
      t617 = t612 * t420
      t618 = t611 ** 2
      t622 = s * t618 * t15 * t553 * t98
      t624 = 0.1D1 / (-0.2D1 + t611)
      t625 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t614, -t617, -t
     #616, 0.0D0, t622)
      t626 = t624 * t625
      t627 = t618 ** 2
      t632 = log(-0.4D1 * t204 * t428 * t99 * t627)
      t634 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t614, -t617, -t
     #616, 0.0D0, t622)
      t641 = t7 * t624 * t634
      t651 = log(-0.4D1 * t204 * t428 * t98 * t627)
      t652 = t651 * t624
      t654 = t651 ** 2
      t658 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t614, -t617, -t
     #616, 0.0D0, t622)
      t674 = -(0.90D2 * t4 * t7 * (t626 - t632 * t624 * t634) - 0.180D3 
     #* t508 * t641) * t106 * t108 / 0.720D3 - (-0.90D2 * t4 * t7 * (t65
     #2 * t625 - t654 * t624 * t634 / 0.2D1 - t624 * t658) + 0.180D3 * t
     #33 * t34 * (-t626 + t652 * t634) + t457 * t641) * t106 * t49 / 0.7
     #20D3
      t675 = FJET(XB1, XB2, s, t614, -t616, -t617, 0.0D0, t622, t674)
      t677 = KAPPA2(x1, x2, x3, t542, z)
      t678 = s * t677
      t679 = t678 * t613
      t680 = t678 * t615
      t681 = t678 * t546
      t682 = t678 * t548
      t683 = t677 ** 2
      t688 = cos(t9)
      t691 = Sqrt(x3 * t98 * t343)
      t696 = s * t683 * t15 * t553 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t688 * t691)
      t698 = 0.1D1 / (-0.2D1 + t677)
      t699 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t679, t681, -t6
     #80, -t682, t696)
      t702 = t683 ** 2
      t707 = log(0.4D1 * t114 * t427 * t98 * t343 * t702)
      t709 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t679, t681, -t6
     #80, -t682, t696)
      t719 = -0.90D2 * t4 * t7 * (t698 * t699 - t707 * t698 * t709) + 0.
     #180D3 * t508 * t7 * t698 * t709
      t723 = FJET(XB1, XB2, s, t679, -t680, t681, -t682, t696, -t719 * t
     #106 * t108 / 0.720D3)
      rrgq2qght5s7e1 = t304 * t303 + t413 * t412 + t540 * t539 + t609 * 
     #t608 + t675 * t674 - t723 * t719 * t106 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s7e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t15 * x4
      t19 = log(0.4D1 * t13 * t16)
      t20 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = t19 ** 2
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = lh ** 2
      t40 = pi ** 2
      t42 = 0.180D3 * t38 - 0.30D2 * t40
      t43 = pi * t42
      t44 = t32 * t23
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t50 = t13 * t15
      t52 = log(0.4D1 * t50)
      t53 = t52 * pi
      t56 = t52 ** 2
      t57 = t56 * pi
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t93 = x1 ** 2
      t94 = t93 * t10
      t95 = t12 * t15
      t96 = t95 * x4
      t99 = log(0.4D1 * t94 * t96)
      t108 = 0.1D1 / x1
      t112 = t4 * t7
      t113 = x3 * t93
      t116 = log(0.4D1 * t113 * t50)
      t119 = -0.1D1 + x3
      t120 = t95 * t119
      t123 = log(-0.4D1 * t113 * t10 * t120)
      t126 = 0.1D1 / x3
      t131 = t94 * t95
      t133 = log(0.4D1 * t131)
      t135 = t133 ** 2
      t150 = x3 * t10
      t151 = t150 * t12
      t156 = log(-0.4D1 * t151 * t15 * t119 * x4)
      t160 = log(0.4D1 * t150 * t96)
      t167 = t150 * t95
      t169 = log(0.4D1 * t167)
      t171 = t169 ** 2
      t176 = log(-0.4D1 * t150 * t120)
      t178 = t176 ** 2
      t194 = (0.90D2 * t4 * t7 * (-t19 * t20 + t22 * t23 / 0.2D1 + t26) 
     #- 0.180D3 * t31 * t32 * (t20 - t19 * t23) + t45) * t47 / 0.1440D4 
     #+ (t43 + 0.180D3 * t53 * lh + 0.45D2 * t57) * t3 * t7 * t20 / 0.14
     #40D4 + t4 * t7 * t64 / 0.16D2 + (pi * (0.60D2 * t40 * lh - 0.240D3
     # * zeta3 - 0.120D3 * t38 * lh) - t53 * t42 - 0.90D2 * t57 * lh - 0
     #.15D2 * t56 * t52 * pi) * t3 * t7 * t23 / 0.1440D4 + (-0.180D3 * t
     #31 - 0.90D2 * t53) * t3 * t7 * t26 / 0.1440D4 - (0.90D2 * t4 * t7 
     #* (-t20 + t99 * t23) + 0.180D3 * t31 * t44) * t108 * t47 / 0.720D3
     # - t112 * (t116 * t23 - t123 * t23) * t126 * t108 / 0.8D1 - (0.90D
     #2 * t4 * t7 * (t133 * t20 - t135 * t23 / 0.2D1 - t26) - 0.180D3 * 
     #t31 * t32 * (-t20 + t133 * t23) - t45) * t108 / 0.720D3 + t112 * (
     #t156 * t23 - t160 * t23) * t126 * t47 / 0.16D2 + (0.90D2 * t4 * t7
     # * (-t169 * t20 + t171 * t23 / 0.2D1 + t176 * t20 - t178 * t23 / 0
     #.2D1) - 0.180D3 * t31 * t32 * (-t169 * t23 + t176 * t23)) * t126 /
     # 0.1440D4
      t195 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t194)
      t197 = -0.1D1 + x4
      t198 = t2 * t197
      t199 = t2 * x4
      t200 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t198, 0
     #.0D0, t199, 0.0D0)
      t202 = t16 * t197
      t205 = log(-0.4D1 * t94 * t12 * t202)
      t206 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t198, 0
     #.0D0, t199, 0.0D0)
      t212 = t32 * t206
      t221 = log(-0.4D1 * t151 * t202)
      t227 = log(0.4D1 * t151 * t16 * t197 * t119)
      t236 = log(-0.4D1 * t13 * t202)
      t238 = t236 ** 2
      t241 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t198, 0
     #.0D0, t199, 0.0D0)
      t255 = -(0.90D2 * t4 * t7 * (t200 - t205 * t206) - 0.180D3 * t31 *
     # t212) * t108 * t47 / 0.720D3 + t112 * (t221 * t206 - t227 * t206)
     # * t126 * t47 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t236 * t200 + t238
     # * t206 / 0.2D1 + t241) + 0.180D3 * t31 * t32 * (t200 - t236 * t20
     #6) - t43 * t212) * t47 / 0.1440D4
      t256 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t198, t199, 0.0D0, t255)
      t258 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t259 = s * t258
      t260 = t1 * x1
      t261 = t259 * t260
      t262 = -0.1D1 + x1
      t263 = t1 * t262
      t264 = t259 * t263
      t265 = t258 ** 2
      t269 = s * t265 * t14 * x1 * t262
      t271 = 0.1D1 / (-0.2D1 + t258)
      t272 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t264, t
     #261, 0.0D0, -t269)
      t273 = t271 * t272
      t274 = t262 ** 2
      t275 = t93 * t274
      t276 = t265 ** 2
      t281 = log(0.4D1 * t50 * t275 * x4 * t276)
      t283 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t264, t
     #261, 0.0D0, -t269)
      t289 = t31 * t3
      t290 = t7 * t271
      t291 = t290 * t283
      t293 = 0.180D3 * t289 * t291
      t300 = t108 * t47
      t309 = log(0.4D1 * t151 * t15 * t93 * t274 * t276)
      t323 = log(0.4D1 * t50 * t275 * t276)
      t324 = t323 * t271
      t326 = t323 ** 2
      t330 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t264, t
     #261, 0.0D0, -t269)
      t346 = -(0.90D2 * t4 * t7 * (-t273 + t281 * t271 * t283) + t293) *
     # t108 * t47 / 0.720D3 + t4 * t290 * t283 * t126 * t300 / 0.8D1 - (
     #0.90D2 * t4 * t7 * (-t273 + t309 * t271 * t283) + t293) * t126 * t
     #108 / 0.720D3 - (-0.90D2 * t4 * t7 * (-t324 * t272 + t326 * t271 *
     # t283 / 0.2D1 + t271 * t330) + 0.180D3 * t31 * t32 * (t273 - t324 
     #* t283) - t43 * t3 * t291) * t108 / 0.720D3
      t347 = FJET(XB1, XB2, s, 0.0D0, t261, -t264, 0.0D0, -t269, t346)
      t349 = -t197
      t350 = KAPPA2(x1, x2, 0.0D0, t349, z)
      t351 = s * t350
      t352 = t351 * t260
      t353 = t263 * t197
      t354 = t351 * t353
      t355 = t263 * x4
      t356 = t351 * t355
      t357 = t350 ** 2
      t360 = x1 * t262
      t362 = s * t357 * t14 * t360 * t197
      t364 = 0.1D1 / (-0.2D1 + t350)
      t365 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t354, t3
     #52, -t356, t362)
      t368 = t357 ** 2
      t373 = log(-0.4D1 * t131 * t274 * x4 * t197 * t368)
      t375 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t354, t3
     #52, -t356, t362)
      t381 = t7 * t364
      t394 = -(-0.90D2 * t4 * t7 * (-t364 * t365 + t373 * t364 * t375) -
     # 0.180D3 * t289 * t381 * t375) * t108 * t47 / 0.720D3 - t4 * t381 
     #* t375 * t126 * t300 / 0.8D1
      t395 = FJET(XB1, XB2, s, 0.0D0, t352, t354, -t356, t362, t394)
      t397 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t398 = s * t397
      t399 = t260 * x3
      t400 = t398 * t399
      t401 = t260 * t119
      t402 = t398 * t401
      t403 = t398 * t263
      t404 = t397 ** 2
      t408 = s * t404 * t14 * t360 * t119
      t410 = 0.1D1 / (-0.2D1 + t397)
      t411 = t7 * t410
      t413 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t400, -t403, -t
     #402, 0.0D0, t408)
      t418 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t400, -t403, -t
     #402, 0.0D0, t408)
      t420 = t404 ** 2
      t425 = log(-0.4D1 * t167 * t275 * t119 * t420)
      t439 = -t4 * t411 * t413 * t126 * t300 / 0.8D1 - (-0.90D2 * t4 * t
     #7 * (-t410 * t418 + t425 * t410 * t413) - 0.180D3 * t289 * t411 * 
     #t413) * t126 * t108 / 0.720D3
      t440 = FJET(XB1, XB2, s, t400, -t402, -t403, 0.0D0, t408, t439)
      t442 = KAPPA2(x1, x2, x3, t349, z)
      t443 = s * t442
      t444 = t443 * t399
      t445 = t443 * t401
      t446 = t443 * t353
      t447 = t443 * t355
      t448 = t442 ** 2
      t453 = cos(t8)
      t457 = Sqrt(x3 * t119 * x4 * t197)
      t462 = s * t448 * t14 * t360 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t453 * t457)
      t464 = 0.1D1 / (-0.2D1 + t442)
      t467 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t444, t446, -t4
     #45, -t447, t462)
      t472 = FJET(XB1, XB2, s, t444, -t445, t446, -t447, t462, t4 * t7 *
     # t464 * t467 * t126 * t300 / 0.8D1)
      rrgq2qght5s7e0 = t195 * t194 + t256 * t255 + t347 * t346 + t395 * 
     #t394 + t440 * t439 + t472 * pi * t32 * t464 * t467 * t126 * t108 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s7em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t38 = t13 * t16
      t41 = log(0.4D1 * t36 * t11 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t59 = log(0.4D1 * t56 * t38)
      t61 = -0.1D1 + x3
      t65 = log(-0.4D1 * t56 * t38 * t61)
      t69 = 0.1D1 / x3
      t74 = t14 * t16
      t76 = log(0.4D1 * t74)
      t77 = t76 * pi
      t84 = lh ** 2
      t86 = pi ** 2
      t92 = t76 ** 2
      t100 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t104 = (0.90D2 * t4 * t7 * (t8 - t20 * t21) - t31) * t33 / 0.1440D
     #4 - (0.90D2 * t4 * t7 * (-t8 + t41 * t21) + t31) * t48 / 0.720D3 +
     # t51 * t21 * t48 * t33 / 0.8D1 + t4 * t7 * (-t59 * t21 + t65 * t21
     #) * t69 / 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t77) * t3 * t7 * t8 
     #/ 0.1440D4 + (pi * (0.180D3 * t84 - 0.30D2 * t86) + 0.180D3 * t77 
     #* lh + 0.45D2 * t92 * pi) * t3 * t7 * t21 / 0.1440D4 + t4 * t7 * t
     #100 / 0.16D2
      t105 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t104)
      t107 = -0.1D1 + x4
      t108 = t2 * t107
      t109 = t2 * x4
      t110 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t108, 0
     #.0D0, t109, 0.0D0)
      t114 = log(-0.4D1 * t14 * t17 * t107)
      t115 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t108, 0
     #.0D0, t109, 0.0D0)
      t131 = (-0.90D2 * t4 * t7 * (t110 - t114 * t115) + 0.180D3 * t27 *
     # t28 * t115) * t33 / 0.1440D4 - t51 * t115 * t48 * t33 / 0.8D1
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t108, t109, 0.0D0, t131)
      t134 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t135 = s * t134
      t136 = t1 * x1
      t137 = t135 * t136
      t138 = -0.1D1 + x1
      t139 = t1 * t138
      t140 = t135 * t139
      t141 = t134 ** 2
      t145 = s * t141 * t15 * x1 * t138
      t147 = 0.1D1 / (-0.2D1 + t134)
      t148 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t140, t
     #137, 0.0D0, -t145)
      t149 = t147 * t148
      t150 = t69 * t48
      t154 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t140, t
     #137, 0.0D0, -t145)
      t156 = t138 ** 2
      t158 = t141 ** 2
      t162 = log(0.4D1 * t74 * t36 * t156 * t158)
      t177 = t48 * t33
      t181 = t51 * t149 * t150 / 0.8D1 - (-0.90D2 * t4 * t7 * (t147 * t1
     #54 - t162 * t147 * t148) + 0.180D3 * t27 * t3 * t7 * t147 * t148) 
     #* t48 / 0.720D3 + t51 * t149 * t177 / 0.8D1
      t182 = FJET(XB1, XB2, s, 0.0D0, t137, -t140, 0.0D0, -t145, t181)
      t185 = KAPPA2(x1, x2, 0.0D0, -t107, z)
      t186 = s * t185
      t187 = t186 * t136
      t189 = t186 * t139 * t107
      t191 = t186 * t139 * x4
      t192 = t185 ** 2
      t195 = x1 * t138
      t197 = s * t192 * t15 * t195 * t107
      t200 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t189, t1
     #87, -t191, t197)
      t202 = 0.1D1 / (-0.2D1 + t185) * t200 * t177
      t205 = FJET(XB1, XB2, s, 0.0D0, t187, t189, -t191, t197, -t51 * t2
     #02 / 0.8D1)
      t210 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t211 = s * t210
      t213 = t211 * t136 * x3
      t215 = t211 * t136 * t61
      t216 = t211 * t139
      t217 = t210 ** 2
      t221 = s * t217 * t15 * t195 * t61
      t224 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t213, -t216, -t
     #215, 0.0D0, t221)
      t226 = 0.1D1 / (-0.2D1 + t210) * t224 * t150
      t229 = FJET(XB1, XB2, s, t213, -t215, -t216, 0.0D0, t221, -t51 * t
     #226 / 0.8D1)
      rrgq2qght5s7em1 = t105 * t104 + t132 * t131 + t182 * t181 - t205 *
     # pi * t28 * t202 / 0.8D1 - t229 * pi * t28 * t226 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s7em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t44 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t45 = s * t44
      t47 = t45 * t1 * x1
      t48 = -0.1D1 + x1
      t50 = t45 * t1 * t48
      t51 = t44 ** 2
      t55 = s * t51 * t30 * x1 * t48
      t58 = 0.1D1 / (-0.2D1 + t44)
      t59 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t50, t47
     #, 0.0D0, -t55)
      t64 = FJET(XB1, XB2, s, 0.0D0, t47, -t50, 0.0D0, -t55, t4 * t7 * t
     #58 * t59 * t10 / 0.8D1)
      t73 = t2 * (-0.1D1 + x4)
      t74 = t2 * x4
      t75 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t73, 0.0
     #D0, t74, 0.0D0)
      t77 = t7 * t75 * t14
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t73, t74, 0.0D0, -t4 * t77 
     #/ 0.16D2)
      rrgq2qght5s7em2 = t42 * t41 + t64 * pi * t3 * t7 * t58 * t59 * t10
     # / 0.8D1 - t80 * pi * t3 * t77 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s7em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s7em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s7em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s7em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght5s8e1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t12 * t18)
      t22 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t28 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = lh ** 2
      t42 = pi ** 2
      t44 = 0.180D3 * t40 - 0.30D2 * t42
      t45 = pi * t44
      t46 = t34 * t25
      t47 = t45 * t46
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t12 * t17
      t56 = log(0.4D1 * t54)
      t61 = t56 ** 2
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t78 = 0.60D2 * lh * t42 - 0.240D3 * zeta3 - 0.120D3 * t40 * lh
      t79 = pi * t78
      t80 = t79 * t46
      t91 = t4 * t7
      t92 = x3 * t8
      t93 = t92 * t11
      t96 = log(0.4D1 * t93 * t18)
      t98 = -0.1D1 + x3
      t99 = t98 * x4
      t103 = log(-0.4D1 * t93 * t17 * t99)
      t106 = 0.1D1 / x3
      t108 = t49 * t51
      t112 = t17 * t98
      t115 = log(-0.4D1 * t93 * t112)
      t117 = t115 ** 2
      t120 = t11 * t14
      t121 = t120 * t16
      t122 = t92 * t121
      t124 = log(0.4D1 * t122)
      t126 = t124 ** 2
      t143 = t16 * x4
      t146 = log(0.4D1 * t120 * t143)
      t151 = t146 ** 2
      t172 = x3 * t11
      t175 = log(0.4D1 * t172 * t18)
      t177 = t175 ** 2
      t180 = t172 * t14
      t185 = log(-0.4D1 * t180 * t16 * t98 * x4)
      t187 = t185 ** 2
      t204 = t7 * t22
      t210 = t172 * t17
      t212 = log(0.4D1 * t210)
      t213 = t212 ** 2
      t216 = log(-0.4D1 * t172 * t112)
      t217 = t216 ** 2
      t221 = t7 * t25
      t232 = t7 * t28
      t242 = log(0.4D1 * t121)
      t243 = t242 * pi
      t245 = t242 ** 2
      t246 = t245 * pi
      t250 = t245 * t242 * pi
      t270 = rrgq2qgh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t274 = t42 ** 2
      t275 = t40 ** 2
      t288 = t245 ** 2
      t295 = (0.90D2 * t4 * t7 * (-t21 * t22 + t24 * t25 / 0.2D1 + t28) 
     #- 0.180D3 * t33 * t34 * (t22 - t21 * t25) + t47) * t49 * t51 / 0.7
     #20D3 - (t45 * t34 * (-t22 + t56 * t25) + 0.90D2 * t4 * t7 * (-t61 
     #* t22 / 0.2D1 - t64 + t61 * t56 * t25 / 0.6D1 + t56 * t28) - t80 -
     # 0.180D3 * t33 * t34 * (t56 * t22 - t61 * t25 / 0.2D1 - t28)) * t4
     #9 / 0.720D3 + t91 * (-t96 * t25 + t103 * t25) * t106 * t108 / 0.8D
     #1 + (0.90D2 * t4 * t7 * (t115 * t22 - t117 * t25 / 0.2D1 - t124 * 
     #t22 + t126 * t25 / 0.2D1) - 0.180D3 * t33 * t34 * (t115 * t25 - t1
     #24 * t25)) * t106 * t49 / 0.720D3 - (t45 * t34 * (-t22 + t146 * t2
     #5) + 0.90D2 * t4 * t7 * (-t151 * t22 / 0.2D1 - t64 + t151 * t146 *
     # t25 / 0.6D1 + t146 * t28) - t80 - 0.180D3 * t33 * t34 * (t146 * t
     #22 - t151 * t25 / 0.2D1 - t28)) * t51 / 0.1440D4 - (0.90D2 * t4 * 
     #t7 * (t175 * t22 - t177 * t25 / 0.2D1 - t185 * t22 + t187 * t25 / 
     #0.2D1) - 0.180D3 * t33 * t34 * (t175 * t25 - t185 * t25)) * t106 *
     # t51 / 0.1440D4 - ((-0.90D2 * t4 * t204 + 0.180D3 * t33 * t46) * (
     #t213 / 0.2D1 - t217 / 0.2D1) - 0.90D2 * t4 * t221 * (t217 * t216 /
     # 0.6D1 - t213 * t212 / 0.6D1) + (0.180D3 * t33 * t34 * t22 - t47 -
     # 0.90D2 * t4 * t232) * (-t212 + t216)) * t106 / 0.1440D4 + (t79 - 
     #t243 * t44 - 0.90D2 * t246 * lh - 0.15D2 * t250) * t3 * t204 / 0.1
     #440D4 + (t45 + 0.180D3 * t243 * lh + 0.45D2 * t246) * t3 * t232 / 
     #0.1440D4 + (-0.180D3 * t33 - 0.90D2 * t243) * t3 * t7 * t64 / 0.14
     #40D4 + t4 * t7 * t270 / 0.16D2 + (pi * (t274 + 0.60D2 * t275 + 0.4
     #80D3 * lh * zeta3 - 0.60D2 * t40 * t42) - t243 * t78 + t246 * t44 
     #/ 0.2D1 + 0.30D2 * t250 * lh + 0.15D2 / 0.4D1 * t288 * pi) * t3 * 
     #t221 / 0.1440D4
      t296 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t295)
      t298 = t2 * x4
      t299 = -0.1D1 + x4
      t300 = t2 * t299
      t302 = t143 * t299
      t305 = log(-0.4D1 * t12 * t14 * t302)
      t306 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t308 = t305 ** 2
      t309 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t312 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t322 = t34 * t309
      t328 = x4 * t299
      t332 = log(-0.4D1 * t93 * t17 * t328)
      t334 = t92 * t120
      t336 = t143 * t299 * t98
      t339 = log(0.4D1 * t334 * t336)
      t348 = log(-0.4D1 * t120 * t302)
      t353 = t348 ** 2
      t356 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t298, 0.
     #0D0, -t300, 0.0D0)
      t378 = log(-0.4D1 * t180 * t302)
      t380 = t378 ** 2
      t385 = log(0.4D1 * t180 * t336)
      t387 = t385 ** 2
      t404 = (0.90D2 * t4 * t7 * (t305 * t306 - t308 * t309 / 0.2D1 - t3
     #12) - 0.180D3 * t33 * t34 * (-t306 + t305 * t309) - t45 * t322) * 
     #t49 * t51 / 0.720D3 + t91 * (t332 * t309 - t339 * t309) * t106 * t
     #108 / 0.8D1 - (t45 * t34 * (t306 - t348 * t309) + 0.90D2 * t4 * t7
     # * (t353 * t306 / 0.2D1 + t356 - t353 * t348 * t309 / 0.6D1 - t348
     # * t312) + t79 * t322 - 0.180D3 * t33 * t34 * (-t348 * t306 + t353
     # * t309 / 0.2D1 + t312)) * t51 / 0.1440D4 - (0.90D2 * t4 * t7 * (-
     #t378 * t306 + t380 * t309 / 0.2D1 + t385 * t306 - t387 * t309 / 0.
     #2D1) - 0.180D3 * t33 * t34 * (-t378 * t309 + t385 * t309)) * t106 
     #* t51 / 0.1440D4
      t405 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t298, -t300, 0.0D0, t404)
      t407 = t2 * x1
      t408 = -0.1D1 + x1
      t409 = t2 * t408
      t410 = t408 ** 2
      t411 = t8 * t410
      t415 = log(0.4D1 * t121 * t411 * x4)
      t416 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #407, -t409, 0.0D0)
      t418 = t415 ** 2
      t419 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #407, -t409, 0.0D0)
      t422 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #407, -t409, 0.0D0)
      t432 = t34 * t419
      t433 = t45 * t432
      t437 = t16 * t8
      t438 = t437 * t410
      t441 = log(0.4D1 * t120 * t438)
      t446 = t441 ** 2
      t449 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #407, -t409, 0.0D0)
      t472 = log(0.4D1 * t180 * t437 * t410 * x4)
      t485 = log(0.4D1 * t180 * t438)
      t487 = t485 ** 2
      t503 = (0.90D2 * t4 * t7 * (t415 * t416 - t418 * t419 / 0.2D1 - t4
     #22) - 0.180D3 * t33 * t34 * (-t416 + t415 * t419) - t433) * t49 * 
     #t51 / 0.720D3 - (t45 * t34 * (t416 - t441 * t419) + 0.90D2 * t4 * 
     #t7 * (t446 * t416 / 0.2D1 + t449 - t446 * t441 * t419 / 0.6D1 - t4
     #41 * t422) + t79 * t432 - 0.180D3 * t33 * t34 * (-t441 * t416 + t4
     #46 * t419 / 0.2D1 + t422)) * t49 / 0.720D3 + (0.90D2 * t4 * t7 * (
     #-t416 + t472 * t419) + 0.180D3 * t33 * t432) * t106 * t108 / 0.720
     #D3 + (0.90D2 * t4 * t7 * (t485 * t416 - t487 * t419 / 0.2D1 - t422
     #) - 0.180D3 * t33 * t34 * (-t416 + t485 * t419) - t433) * t106 * t
     #49 / 0.720D3
      t504 = FJET(XB1, XB2, s, 0.0D0, t407, 0.0D0, -t409, 0.0D0, t503)
      t506 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t507 = s * t506
      t508 = t1 * x1
      t509 = t507 * t508
      t510 = t1 * t408
      t511 = t510 * x4
      t512 = t507 * t511
      t513 = t510 * t299
      t514 = t507 * t513
      t515 = t506 ** 2
      t518 = x1 * t408
      t520 = s * t515 * t15 * t518 * x4
      t521 = t515 ** 2
      t526 = log(-0.4D1 * t54 * t328 * t521 * t410)
      t528 = 0.1D1 / (-0.2D1 + t506)
      t529 = t526 * t528
      t530 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #509, t514, -t520)
      t532 = t526 ** 2
      t534 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #509, t514, -t520)
      t537 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #509, t514, -t520)
      t543 = t528 * t530
      t549 = t45 * t3
      t551 = t7 * t528 * t534
      t561 = log(-0.4D1 * t334 * t143 * t299 * t410 * t521)
      t568 = t33 * t3
      t575 = (-0.90D2 * t4 * t7 * (-t529 * t530 + t532 * t528 * t534 / 0
     #.2D1 + t528 * t537) + 0.180D3 * t33 * t34 * (t543 - t529 * t534) -
     # t549 * t551) * t49 * t51 / 0.720D3 + (0.90D2 * t4 * t7 * (-t543 +
     # t561 * t528 * t534) + 0.180D3 * t568 * t551) * t106 * t108 / 0.72
     #0D3
      t576 = FJET(XB1, XB2, s, 0.0D0, t509, -t512, t514, -t520, t575)
      t578 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t579 = s * t578
      t580 = t508 * x3
      t581 = t579 * t580
      t582 = t508 * t98
      t583 = t579 * t582
      t584 = t579 * t510
      t585 = t578 ** 2
      t589 = s * t585 * t15 * t518 * x3
      t591 = 0.1D1 / (-0.2D1 + t578)
      t592 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t581, 0.0D0, -t
     #583, -t584, -t589)
      t593 = t591 * t592
      t594 = t585 ** 2
      t599 = log(-0.4D1 * t210 * t411 * t99 * t594)
      t601 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t581, 0.0D0, -t
     #583, -t584, -t589)
      t608 = t7 * t591 * t601
      t618 = log(-0.4D1 * t210 * t411 * t98 * t594)
      t619 = t618 * t591
      t621 = t618 ** 2
      t625 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, t581, 0.0D0, -t
     #583, -t584, -t589)
      t641 = (0.90D2 * t4 * t7 * (-t593 + t599 * t591 * t601) + 0.180D3 
     #* t568 * t608) * t106 * t108 / 0.720D3 + (-0.90D2 * t4 * t7 * (-t6
     #19 * t592 + t621 * t591 * t601 / 0.2D1 + t591 * t625) + 0.180D3 * 
     #t33 * t34 * (t593 - t619 * t601) - t549 * t608) * t106 * t49 / 0.7
     #20D3
      t642 = FJET(XB1, XB2, s, t581, -t583, 0.0D0, -t584, -t589, t641)
      t644 = KAPPA2(x1, x2, x3, x4, z)
      t645 = s * t644
      t646 = t645 * t580
      t647 = t645 * t582
      t648 = t645 * t511
      t649 = t645 * t513
      t650 = t644 ** 2
      t655 = cos(t9)
      t658 = Sqrt(x3 * t98 * t328)
      t663 = s * t650 * t15 * t518 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t655 * t658)
      t665 = 0.1D1 / (-0.2D1 + t644)
      t666 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t646, -t648, -t
     #647, t649, t663)
      t669 = t650 ** 2
      t674 = log(0.4D1 * t122 * t328 * t410 * t98 * t669)
      t676 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t646, -t648, -t
     #647, t649, t663)
      t686 = 0.90D2 * t4 * t7 * (t665 * t666 - t674 * t665 * t676) - 0.1
     #80D3 * t568 * t7 * t665 * t676
      t690 = FJET(XB1, XB2, s, t646, -t647, -t648, t649, t663, t686 * t1
     #06 * t108 / 0.720D3)
      rrgq2qght5s8e1 = t296 * t295 + t405 * t404 + t504 * t503 + t576 * 
     #t575 + t642 * t641 + t690 * t686 * t106 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrgq2qght5s8e0
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t15 * x4
      t19 = log(0.4D1 * t13 * t16)
      t20 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = t19 ** 2
      t23 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t26 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = lh ** 2
      t40 = pi ** 2
      t42 = 0.180D3 * t38 - 0.30D2 * t40
      t43 = pi * t42
      t44 = t32 * t23
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t50 = t13 * t15
      t52 = log(0.4D1 * t50)
      t53 = t52 * pi
      t56 = t52 ** 2
      t57 = t56 * pi
      t61 = t7 * t20
      t64 = rrgq2qgh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t83 = t7 * t23
      t93 = x1 ** 2
      t94 = t93 * t10
      t95 = t12 * t15
      t96 = t95 * x4
      t99 = log(0.4D1 * t94 * t96)
      t106 = 0.180D3 * t31 * t44
      t108 = 0.1D1 / x1
      t112 = t4 * t7
      t113 = x3 * t93
      t115 = -0.1D1 + x3
      t116 = t95 * t115
      t119 = log(-0.4D1 * t113 * t10 * t116)
      t123 = log(0.4D1 * t113 * t50)
      t126 = 0.1D1 / x3
      t131 = t94 * t95
      t133 = log(0.4D1 * t131)
      t135 = t133 ** 2
      t150 = x3 * t10
      t153 = log(0.4D1 * t150 * t96)
      t155 = t150 * t12
      t160 = log(-0.4D1 * t155 * t15 * t115 * x4)
      t167 = t150 * t95
      t169 = log(0.4D1 * t167)
      t170 = t169 ** 2
      t173 = log(-0.4D1 * t150 * t116)
      t174 = t173 ** 2
      t188 = -(0.90D2 * t4 * t7 * (t19 * t20 - t22 * t23 / 0.2D1 - t26) 
     #- 0.180D3 * t31 * t32 * (-t20 + t19 * t23) - t45) * t47 / 0.1440D4
     # + (t43 + 0.180D3 * t53 * lh + 0.45D2 * t57) * t3 * t61 / 0.1440D4
     # + t4 * t7 * t64 / 0.16D2 + (pi * (0.60D2 * t40 * lh - 0.240D3 * z
     #eta3 - 0.120D3 * t38 * lh) - t53 * t42 - 0.90D2 * t57 * lh - 0.15D
     #2 * t56 * t52 * pi) * t3 * t83 / 0.1440D4 + (-0.180D3 * t31 - 0.90
     #D2 * t53) * t3 * t7 * t26 / 0.1440D4 + (0.90D2 * t4 * t7 * (t20 - 
     #t99 * t23) - t106) * t108 * t47 / 0.720D3 + t112 * (t119 * t23 - t
     #123 * t23) * t126 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (t133 * t20
     # - t135 * t23 / 0.2D1 - t26) - 0.180D3 * t31 * t32 * (-t20 + t133 
     #* t23) - t45) * t108 / 0.720D3 - t112 * (t153 * t23 - t160 * t23) 
     #* t126 * t47 / 0.16D2 - (-0.90D2 * t4 * t83 * (t170 / 0.2D1 - t174
     # / 0.2D1) + (-0.90D2 * t4 * t61 + t106) * (-t169 + t173)) * t126 /
     # 0.1440D4
      t189 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t188)
      t191 = t2 * x4
      t192 = -0.1D1 + x4
      t193 = t2 * t192
      t194 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t191, 0.
     #0D0, -t193, 0.0D0)
      t196 = t16 * t192
      t199 = log(-0.4D1 * t94 * t12 * t196)
      t200 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t191, 0.
     #0D0, -t193, 0.0D0)
      t206 = t32 * t200
      t215 = log(-0.4D1 * t155 * t196)
      t221 = log(0.4D1 * t155 * t16 * t192 * t115)
      t230 = log(-0.4D1 * t13 * t196)
      t232 = t230 ** 2
      t235 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t191, 0.
     #0D0, -t193, 0.0D0)
      t249 = (0.90D2 * t4 * t7 * (-t194 + t199 * t200) + 0.180D3 * t31 *
     # t206) * t108 * t47 / 0.720D3 - t112 * (-t215 * t200 + t221 * t200
     #) * t126 * t47 / 0.16D2 - (0.90D2 * t4 * t7 * (-t230 * t194 + t232
     # * t200 / 0.2D1 + t235) - 0.180D3 * t31 * t32 * (t194 - t230 * t20
     #0) + t43 * t206) * t47 / 0.1440D4
      t250 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t191, -t193, 0.0D0, t249)
      t252 = t2 * x1
      t253 = -0.1D1 + x1
      t254 = t2 * t253
      t255 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #252, -t254, 0.0D0)
      t256 = t253 ** 2
      t257 = t93 * t256
      t261 = log(0.4D1 * t50 * t257 * x4)
      t262 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #252, -t254, 0.0D0)
      t268 = t32 * t262
      t270 = 0.180D3 * t31 * t268
      t276 = t108 * t47
      t281 = t15 * t93 * t256
      t284 = log(0.4D1 * t155 * t281)
      t296 = log(0.4D1 * t13 * t281)
      t298 = t296 ** 2
      t301 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #252, -t254, 0.0D0)
      t315 = (0.90D2 * t4 * t7 * (-t255 + t261 * t262) + t270) * t108 * 
     #t47 / 0.720D3 - t112 * t262 * t126 * t276 / 0.8D1 + (0.90D2 * t4 *
     # t7 * (-t255 + t284 * t262) + t270) * t126 * t108 / 0.720D3 - (0.9
     #0D2 * t4 * t7 * (-t296 * t255 + t298 * t262 / 0.2D1 + t301) - 0.18
     #0D3 * t31 * t32 * (t255 - t296 * t262) + t43 * t268) * t108 / 0.72
     #0D3
      t316 = FJET(XB1, XB2, s, 0.0D0, t252, 0.0D0, -t254, 0.0D0, t315)
      t318 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t319 = s * t318
      t320 = t1 * x1
      t321 = t319 * t320
      t322 = t1 * t253
      t323 = t322 * x4
      t324 = t319 * t323
      t325 = t322 * t192
      t326 = t319 * t325
      t327 = t318 ** 2
      t330 = x1 * t253
      t332 = s * t327 * t14 * t330 * x4
      t334 = 0.1D1 / (-0.2D1 + t318)
      t335 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t324, t
     #321, t326, -t332)
      t337 = x4 * t192
      t338 = t327 ** 2
      t343 = log(-0.4D1 * t131 * t337 * t338 * t256)
      t345 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t324, t
     #321, t326, -t332)
      t351 = t31 * t3
      t352 = t7 * t334
      t365 = (-0.90D2 * t4 * t7 * (t334 * t335 - t343 * t334 * t345) + 0
     #.180D3 * t351 * t352 * t345) * t108 * t47 / 0.720D3 - t4 * t352 * 
     #t345 * t126 * t276 / 0.8D1
      t366 = FJET(XB1, XB2, s, 0.0D0, t321, -t324, t326, -t332, t365)
      t368 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t369 = s * t368
      t370 = t320 * x3
      t371 = t369 * t370
      t372 = t320 * t115
      t373 = t369 * t372
      t374 = t369 * t322
      t375 = t368 ** 2
      t379 = s * t375 * t14 * t330 * x3
      t381 = 0.1D1 / (-0.2D1 + t368)
      t382 = t7 * t381
      t384 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t371, 0.0D0, -t
     #373, -t374, -t379)
      t389 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, t371, 0.0D0, -t
     #373, -t374, -t379)
      t391 = t375 ** 2
      t396 = log(-0.4D1 * t167 * t257 * t115 * t391)
      t410 = -t4 * t382 * t384 * t126 * t276 / 0.8D1 + (-0.90D2 * t4 * t
     #7 * (t381 * t389 - t396 * t381 * t384) + 0.180D3 * t351 * t382 * t
     #384) * t126 * t108 / 0.720D3
      t411 = FJET(XB1, XB2, s, t371, -t373, 0.0D0, -t374, -t379, t410)
      t413 = KAPPA2(x1, x2, x3, x4, z)
      t414 = s * t413
      t415 = t414 * t370
      t416 = t414 * t372
      t417 = t414 * t323
      t418 = t414 * t325
      t419 = t413 ** 2
      t424 = cos(t8)
      t427 = Sqrt(x3 * t115 * t337)
      t432 = s * t419 * t14 * t330 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t424 * t427)
      t434 = 0.1D1 / (-0.2D1 + t413)
      t437 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t415, -t417, -t
     #416, t418, t432)
      t442 = FJET(XB1, XB2, s, t415, -t416, -t417, t418, t432, t4 * t7 *
     # t434 * t437 * t126 * t276 / 0.8D1)
      rrgq2qght5s8e0 = t189 * t188 + t250 * t249 + t316 * t315 + t366 * 
     #t365 + t411 * t410 + t442 * pi * t32 * t434 * t437 * t126 * t108 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s8em1
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t38 = t13 * t16
      t41 = log(0.4D1 * t36 * t11 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t59 = log(0.4D1 * t56 * t38)
      t60 = -0.1D1 + x3
      t64 = log(-0.4D1 * t56 * t38 * t60)
      t67 = 0.1D1 / x3
      t74 = log(0.4D1 * t14 * t16)
      t75 = t74 * pi
      t82 = lh ** 2
      t84 = pi ** 2
      t90 = t74 ** 2
      t98 = rrgq2qgh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t102 = -(0.90D2 * t4 * t7 * (-t8 + t20 * t21) + t31) * t33 / 0.144
     #0D4 - (0.90D2 * t4 * t7 * (-t8 + t41 * t21) + t31) * t48 / 0.720D3
     # + t51 * t21 * t48 * t33 / 0.8D1 + t51 * t21 * (-t59 + t64) * t67 
     #/ 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t75) * t3 * t7 * t8 / 0.1440
     #D4 + (pi * (0.180D3 * t82 - 0.30D2 * t84) + 0.180D3 * t75 * lh + 0
     #.45D2 * t90 * pi) * t3 * t7 * t21 / 0.1440D4 + t4 * t7 * t98 / 0.1
     #6D2
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t102)
      t105 = t2 * x4
      t106 = -0.1D1 + x4
      t107 = t2 * t106
      t108 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t112 = log(-0.4D1 * t14 * t17 * t106)
      t113 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t129 = -(0.90D2 * t4 * t7 * (t108 - t112 * t113) - 0.180D3 * t27 *
     # t28 * t113) * t33 / 0.1440D4 - t51 * t113 * t48 * t33 / 0.8D1
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t105, -t107, 0.0D0, t129)
      t132 = t2 * x1
      t133 = -0.1D1 + x1
      t134 = t2 * t133
      t135 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #132, -t134, 0.0D0)
      t140 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #132, -t134, 0.0D0)
      t142 = t133 ** 2
      t146 = log(0.4D1 * t14 * t16 * t36 * t142)
      t162 = -t51 * t135 * t67 * t48 / 0.8D1 - (0.90D2 * t4 * t7 * (t140
     # - t146 * t135) - 0.180D3 * t27 * t28 * t135) * t48 / 0.720D3 - t5
     #1 * t135 * t48 * t33 / 0.8D1
      t163 = FJET(XB1, XB2, s, 0.0D0, t132, 0.0D0, -t134, 0.0D0, t162)
      t165 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t166 = s * t165
      t167 = t1 * x1
      t168 = t166 * t167
      t169 = t1 * t133
      t171 = t166 * t169 * x4
      t173 = t166 * t169 * t106
      t174 = t165 ** 2
      t177 = x1 * t133
      t179 = s * t174 * t15 * t177 * x4
      t182 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t171, t
     #168, t173, -t179)
      t185 = 0.1D1 / (-0.2D1 + t165) * t182 * t48 * t33
      t188 = FJET(XB1, XB2, s, 0.0D0, t168, -t171, t173, -t179, -t51 * t
     #185 / 0.8D1)
      t193 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t194 = s * t193
      t196 = t194 * t167 * x3
      t198 = t194 * t167 * t60
      t199 = t194 * t169
      t200 = t193 ** 2
      t204 = s * t200 * t15 * t177 * x3
      t207 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, t196, 0.0D0, -t
     #198, -t199, -t204)
      t210 = 0.1D1 / (-0.2D1 + t193) * t207 * t67 * t48
      t213 = FJET(XB1, XB2, s, t196, -t198, 0.0D0, -t199, -t204, -t51 * 
     #t210 / 0.8D1)
      rrgq2qght5s8em1 = t103 * t102 + t130 * t129 + t163 * t162 - t188 *
     # pi * t28 * t185 / 0.8D1 - t213 * pi * t28 * t210 / 0.8D1

      end function



      doubleprecision function rrgq2qght5s8em2
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrgq2qgh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = t2 * x1
      t46 = t2 * (-0.1D1 + x1)
      t47 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t4
     #4, -t46, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, 0.0D0, t44, 0.0D0, -t46, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t57 = t2 * x4
      t59 = t2 * (-0.1D1 + x4)
      t60 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t57, 0.0D
     #0, -t59, 0.0D0)
      t62 = t7 * t60 * t14
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t57, -t59, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrgq2qght5s8em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s8em3
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrgq2qght5s8em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrgq2qght5s8em4
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
      doubleprecision rrgq2qgh51J1
      doubleprecision rrgq2qgh51J2
      doubleprecision rrgq2qgh51J3
      doubleprecision rrgq2qgh51J4
      doubleprecision rrgq2qgh51J5
      doubleprecision rrgq2qgh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght5s8em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh51J1
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
      t1 = S12 ** 2
      t4 = S13 ** 2
      t7 = S34 ** 2
      t10 = S24 ** 2
      rrgq2qgh51J1 = (-0.4D1 * t1 * S13 - 0.4D1 * t4 * S13 + (-0.4D1 * t
     #7 * S34 * S24 - 0.4D1 * S34 * t10 * S24) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh51J2
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
      t2 = -0.4D1 * S13 - 0.4D1 * S24
      t4 = S13 ** 2
      t8 = S34 ** 2
      t10 = S24 ** 2
      rrgq2qgh51J2 = ((t2 * S34 + 0.8D1 * t4) * S12 + t2 * t8 + (-0.4D1 
     #* t4 - 0.4D1 * t10) * S34 + 0.8D1 * t10 * t8 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh51J3
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
      t2 = -0.4D1 * S13 - 0.4D1 * S24
      t4 = S13 ** 2
      t8 = S34 ** 2
      t10 = S24 ** 2
      rrgq2qgh51J3 = ((t2 * S34 + 0.8D1 * t4) * S12 + t2 * t8 + (-0.4D1 
     #* t4 - 0.4D1 * t10) * S34 + 0.8D1 * t10 * t8 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh51J4
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
      t2 = -0.4D1 * S13 - 0.4D1 * S24
      t4 = S13 ** 2
      t8 = S34 ** 2
      t10 = S24 ** 2
      rrgq2qgh51J4 = ((t2 * S34 + 0.8D1 * t4) * S12 + t2 * t8 + (-0.4D1 
     #* t4 - 0.4D1 * t10) * S34 + 0.8D1 * t10 * t8 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh51J5
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
      t2 = -0.4D1 * S13 - 0.4D1 * S24
      t4 = S13 ** 2
      t8 = S34 ** 2
      t10 = S24 ** 2
      rrgq2qgh51J5 = ((t2 * S34 + 0.8D1 * t4) * S12 + t2 * t8 + (-0.4D1 
     #* t4 - 0.4D1 * t10) * S34 + 0.8D1 * t10 * t8 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh51J6
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
      t1 = S12 ** 2
      t5 = -0.4D1 * S13 - 0.4D1 * S24
      t7 = S13 ** 2
      t11 = S34 ** 2
      t13 = S24 ** 2
      rrgq2qgh51J6 = (0.4D1 * t1 * S13 + (t5 * S34 + 0.8D1 * t7) * S12 +
     # t5 * t11 + (-0.4D1 * t7 - 0.4D1 * t13) * S34 + 0.4D1 * t7 * S13 +
     # (0.4D1 * S34 * t13 * S24 + 0.8D1 * t11 * t13 + 0.4D1 * t11 * S34 
     #* S24) / S12) / pi * wd / z

      end function
  
 