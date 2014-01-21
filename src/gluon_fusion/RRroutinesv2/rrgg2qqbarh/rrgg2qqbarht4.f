      subroutine rrgg2qqbarht4
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt4
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt4
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhard41J1  
      doubleprecision rrgg2qqbarhhard41J2  
      doubleprecision rrgg2qqbarhhard41J3  
      doubleprecision rrgg2qqbarhhard41J4  
      doubleprecision rrgg2qqbarhhard41J5  
      doubleprecision rrgg2qqbarhhard41J6  
      doubleprecision rrgg2qqbarhhardt4s1e1  
      doubleprecision rrgg2qqbarhhardt4s1e0  
      doubleprecision rrgg2qqbarhhardt4s1em1  
      doubleprecision rrgg2qqbarhhardt4s1em2  
      doubleprecision rrgg2qqbarhhardt4s1em3  
      doubleprecision rrgg2qqbarhhardt4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt4s1e1
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
      doubleprecision rrgg2qqbarhhard41J1
      doubleprecision rrgg2qqbarhhard41J2
      doubleprecision rrgg2qqbarhhard41J3
      doubleprecision rrgg2qqbarhhard41J4
      doubleprecision rrgg2qqbarhhard41J5
      doubleprecision rrgg2qqbarhhard41J6

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
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = x1 ** 2
      t16 = t4 ** 2
      t17 = t15 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t14 * t18)
      t22 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t3, -t5, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t3, -t5, 0.0D0)
      t28 = rrgg2qqbarhhard41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t3, -t5, 0.0D0)
      t33 = pi * lh
      t34 = t6 * t8
      t40 = lh ** 2
      t42 = pi ** 2
      t44 = 0.180D3 * t40 - 0.30D2 * t42
      t45 = pi * t44
      t46 = t34 * t25
      t47 = t45 * t46
      t49 = 0.1D1 / x4
      t54 = log(0.4D1 * t14 * t17)
      t55 = t54 * pi
      t58 = t54 ** 2
      t59 = t58 * pi
      t66 = rrgg2qqbarhhard41J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t3, -t5, 0.0D0)
      t95 = x3 * t11
      t96 = t95 * t13
      t99 = log(0.4D1 * t96 * t18)
      t108 = 0.1D1 / x3
      t116 = log(0.4D1 * t95 * t13 * t15 * t16)
      t118 = t116 ** 2
      t133 = -(0.90D2 * t7 * t8 * (t21 * t22 - t24 * t25 / 0.2D1 - t28) 
     #- 0.180D3 * t33 * t34 * (-t22 + t21 * t25) - t47) * t49 / 0.720D3 
     #+ (t45 + 0.180D3 * t55 * lh + 0.45D2 * t59) * t6 * t8 * t22 / 0.72
     #0D3 + t7 * t8 * t66 / 0.8D1 + (pi * (0.60D2 * lh * t42 - 0.240D3 *
     # zeta3 - 0.120D3 * t40 * lh) - t55 * t44 - 0.90D2 * t59 * lh - 0.1
     #5D2 * t58 * t54 * pi) * t6 * t8 * t25 / 0.720D3 + (-0.180D3 * t33 
     #- 0.90D2 * t55) * t6 * t8 * t28 / 0.720D3 + (0.90D2 * t7 * t8 * (t
     #22 - t99 * t25) - 0.180D3 * t33 * t46) * t108 * t49 / 0.720D3 - (0
     #.90D2 * t7 * t8 * (t116 * t22 - t118 * t25 / 0.2D1 - t28) - 0.180D
     #3 * t33 * t34 * (-t22 + t116 * t25) - t47) * t108 / 0.720D3
      t134 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t133)
      t136 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t137 = s * t136
      t138 = t1 * x1
      t139 = t137 * t138
      t140 = t1 * t4
      t141 = t140 * x4
      t142 = t137 * t141
      t143 = -0.1D1 + x4
      t144 = t140 * t143
      t145 = t137 * t144
      t146 = t136 ** 2
      t149 = x1 * t4
      t151 = s * t146 * t6 * t149 * x4
      t154 = x4 * t143
      t155 = t146 ** 2
      t157 = t154 * t155 * t16
      t160 = log(-0.4D1 * t15 * t11 * t13 * t157)
      t161 = t160 * t146
      t163 = 0.1D1 / (-0.2D1 + t136)
      t164 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t142, t139, t145, -t151)
      t167 = t160 ** 2
      t169 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t142, t139, t145, -t151)
      t170 = t163 * t169
      t173 = t146 * t163
      t174 = rrgg2qqbarhhard41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t142, t139, t145, -t151)
      t176 = t161 * t163 * t164 - t167 * t146 * t170 / 0.2D1 - t173 * t1
     #74
      t180 = t173 * t164
      t182 = -t180 + t161 * t170
      t186 = t45 * t6
      t188 = t8 * t146 * t170
      t189 = t186 * t188
      t193 = x3 * t15 * t14
      t196 = log(-0.4D1 * t193 * t157)
      t203 = t33 * t6
      t208 = (0.90D2 * t7 * t8 * (t180 - t196 * t146 * t170) - 0.180D3 *
     # t203 * t188) * t108 * t49
      t210 = -(0.90D2 * t7 * t8 * t176 - 0.180D3 * t33 * t34 * t182 - t1
     #89) * t49 / 0.720D3 + t208 / 0.720D3
      t211 = FJET(XB1, XB2, s, 0.0D0, t139, -t142, t145, -t151, t210)
      t213 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t133)
      t226 = -(0.90D2 * t7 * t8 * t176 - 0.180D3 * t33 * t34 * t182 - t1
     #89) * t49 / 0.720D3 + t208 / 0.720D3
      t227 = FJET(XB1, XB2, s, t139, 0.0D0, t145, -t142, -t151, t226)
      t229 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t230 = s * t229
      t231 = t138 * x3
      t232 = t230 * t231
      t233 = -0.1D1 + x3
      t234 = t138 * t233
      t235 = t230 * t234
      t236 = t230 * t140
      t237 = t229 ** 2
      t241 = s * t237 * t6 * t149 * x3
      t243 = 0.1D1 / (-0.2D1 + t229)
      t244 = t237 * t243
      t245 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, t232, 0.
     #0D0, -t235, -t236, -t241)
      t246 = t244 * t245
      t247 = t16 * t233
      t248 = t237 ** 2
      t253 = log(-0.4D1 * t193 * t247 * x4 * t248)
      t255 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t232, 0.
     #0D0, -t235, -t236, -t241)
      t256 = t243 * t255
      t263 = t8 * t237 * t256
      t273 = log(-0.4D1 * t96 * t17 * t233 * t248)
      t274 = t273 * t237
      t277 = t273 ** 2
      t281 = rrgg2qqbarhhard41J3(s, XB1, XB2, z, lh, wd, nf, s, t232, 0.
     #0D0, -t235, -t236, -t241)
      t296 = (0.90D2 * t7 * t8 * (t246 - t253 * t237 * t256) - 0.180D3 *
     # t203 * t263) * t108 * t49 / 0.720D3 - (-0.90D2 * t7 * t8 * (-t274
     # * t243 * t245 + t277 * t237 * t256 / 0.2D1 + t244 * t281) + 0.180
     #D3 * t33 * t34 * (t246 - t274 * t256) - t186 * t263) * t108 / 0.72
     #0D3
      t297 = FJET(XB1, XB2, s, t232, -t235, 0.0D0, -t236, -t241, t296)
      t299 = KAPPA2(x1, x2, x3, x4, z)
      t300 = s * t299
      t301 = t300 * t231
      t302 = t300 * t234
      t303 = t300 * t141
      t304 = t300 * t144
      t305 = t299 ** 2
      t310 = cos(t9)
      t313 = Sqrt(x3 * t233 * t154)
      t318 = s * t305 * t6 * t149 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t310 * t313)
      t320 = 0.1D1 / (-0.2D1 + t299)
      t322 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, t301, -t
     #303, -t302, t304, t318)
      t324 = t305 ** 2
      t329 = log(0.4D1 * t193 * t154 * t247 * t324)
      t331 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t301, -t
     #303, -t302, t304, t318)
      t332 = t320 * t331
      t334 = -t305 * t320 * t322 + t329 * t305 * t332
      t341 = 0.180D3 * t203 * t8 * t305 * t332
      t342 = 0.90D2 * t7 * t8 * t334 + t341
      t346 = FJET(XB1, XB2, s, t301, -t302, -t303, t304, t318, t342 * t1
     #08 * t49 / 0.720D3)
      t348 = t108 * t49
      t351 = FJET(XB1, XB2, s, -t235, t232, -t236, 0.0D0, -t241, t296)
      t357 = 0.90D2 * t7 * t8 * t334 + t341
      t361 = FJET(XB1, XB2, s, -t302, t301, t304, -t303, t318, t357 * t1
     #08 * t49 / 0.720D3)
      rrgg2qqbarhhardt4s1e1 = t134 * t133 + t211 * t210 + t213 * t133 + 
     #t227 * t226 + t297 * t296 + t346 * t342 * t348 / 0.720D3 + t351 * 
     #t296 + t361 * t357 * t348 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarhhardt4s1e0
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
      doubleprecision rrgg2qqbarhhard41J1
      doubleprecision rrgg2qqbarhhard41J2
      doubleprecision rrgg2qqbarhhard41J3
      doubleprecision rrgg2qqbarhhard41J4
      doubleprecision rrgg2qqbarhhard41J5
      doubleprecision rrgg2qqbarhhard41J6

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
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t3, -t5, 0.0D0)
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = x1 ** 2
      t17 = t4 ** 2
      t18 = t16 * t17
      t22 = log(0.4D1 * t15 * t18 * x4)
      t23 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t3, -t5, 0.0D0)
      t29 = pi * lh
      t30 = t6 * t8
      t33 = 0.180D3 * t29 * t30 * t23
      t35 = 0.1D1 / x4
      t39 = 0.1D1 / x3
      t44 = x3 * t12
      t49 = log(0.4D1 * t44 * t14 * t16 * t17)
      t61 = log(0.4D1 * t15 * t18)
      t62 = t61 * pi
      t69 = lh ** 2
      t71 = pi ** 2
      t77 = t61 ** 2
      t85 = rrgg2qqbarhhard41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t3, -t5, 0.0D0)
      t89 = -(0.90D2 * t7 * t8 * (-t9 + t22 * t23) + t33) * t35 / 0.720D
     #3 + t7 * t8 * t23 * t39 * t35 / 0.8D1 - (0.90D2 * t7 * t8 * (-t9 +
     # t49 * t23) + t33) * t39 / 0.720D3 + (-0.180D3 * t29 - 0.90D2 * t6
     #2) * t6 * t8 * t9 / 0.720D3 + (pi * (0.180D3 * t69 - 0.30D2 * t71)
     # + 0.180D3 * t62 * lh + 0.45D2 * t77 * pi) * t6 * t8 * t23 / 0.720
     #D3 + t7 * t8 * t85 / 0.8D1
      t90 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t89)
      t92 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t93 = s * t92
      t94 = t1 * x1
      t95 = t93 * t94
      t96 = t1 * t4
      t97 = t96 * x4
      t98 = t93 * t97
      t99 = -0.1D1 + x4
      t100 = t96 * t99
      t101 = t93 * t100
      t102 = t92 ** 2
      t105 = x1 * t4
      t107 = s * t102 * t6 * t105 * x4
      t109 = 0.1D1 / (-0.2D1 + t92)
      t111 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t98, t95, t101, -t107)
      t115 = x4 * t99
      t116 = t102 ** 2
      t121 = log(-0.4D1 * t16 * t12 * t14 * t115 * t116 * t17)
      t123 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t98, t95, t101, -t107)
      t124 = t109 * t123
      t126 = -t102 * t109 * t111 + t121 * t102 * t124
      t130 = t29 * t6
      t131 = t8 * t102
      t134 = 0.180D3 * t130 * t131 * t124
      t139 = t39 * t35
      t142 = t7 * t131 * t124 * t139 / 0.8D1
      t143 = -(0.90D2 * t7 * t8 * t126 + t134) * t35 / 0.720D3 + t142
      t144 = FJET(XB1, XB2, s, 0.0D0, t95, -t98, t101, -t107, t143)
      t146 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t89)
      t155 = -(0.90D2 * t7 * t8 * t126 + t134) * t35 / 0.720D3 + t142
      t156 = FJET(XB1, XB2, s, t95, 0.0D0, t101, -t98, -t107, t155)
      t158 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t159 = s * t158
      t160 = t94 * x3
      t161 = t159 * t160
      t162 = -0.1D1 + x3
      t163 = t94 * t162
      t164 = t159 * t163
      t165 = t159 * t96
      t166 = t158 ** 2
      t170 = s * t166 * t6 * t105 * x3
      t171 = t8 * t166
      t174 = 0.1D1 / (-0.2D1 + t158)
      t175 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t161, 0.
     #0D0, -t164, -t165, -t170)
      t176 = t174 * t175
      t181 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, t161, 0.
     #0D0, -t164, -t165, -t170)
      t184 = t166 ** 2
      t189 = log(-0.4D1 * t44 * t14 * t18 * t162 * t184)
      t202 = t7 * t171 * t176 * t139 / 0.8D1 - (-0.90D2 * t7 * t8 * (t16
     #6 * t174 * t181 - t189 * t166 * t176) + 0.180D3 * t130 * t171 * t1
     #76) * t39 / 0.720D3
      t203 = FJET(XB1, XB2, s, t161, -t164, 0.0D0, -t165, -t170, t202)
      t205 = KAPPA2(x1, x2, x3, x4, z)
      t206 = s * t205
      t207 = t206 * t160
      t208 = t206 * t163
      t209 = t206 * t97
      t210 = t206 * t100
      t211 = t205 ** 2
      t216 = cos(t10)
      t219 = Sqrt(x3 * t162 * t115)
      t224 = s * t211 * t6 * t105 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t216 * t219)
      t228 = 0.1D1 / (-0.2D1 + t205)
      t229 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t207, -t
     #209, -t208, t210, t224)
      t233 = t7 * t8 * t211 * t228 * t229 * t139 / 0.8D1
      t234 = FJET(XB1, XB2, s, t207, -t208, -t209, t210, t224, -t233)
      t240 = t211 * t228 * t229 * t39 * t35
      t243 = FJET(XB1, XB2, s, -t164, t161, -t165, 0.0D0, -t170, t202)
      t245 = FJET(XB1, XB2, s, -t208, t207, t210, -t209, t224, -t233)
      rrgg2qqbarhhardt4s1e0 = t90 * t89 + t144 * t143 + t146 * t89 + t15
     #6 * t155 + t203 * t202 - t234 * pi * t30 * t240 / 0.8D1 + t243 * t
     #202 - t245 * pi * t30 * t240 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt4s1em1
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
      doubleprecision rrgg2qqbarhhard41J1
      doubleprecision rrgg2qqbarhhard41J2
      doubleprecision rrgg2qqbarhhard41J3
      doubleprecision rrgg2qqbarhhard41J4
      doubleprecision rrgg2qqbarhhard41J5
      doubleprecision rrgg2qqbarhhard41J6

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
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t3, -t5, 0.0D0)
      t10 = t8 * t9
      t11 = 0.1D1 / x3
      t15 = rrgg2qqbarhhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t3, -t5, 0.0D0)
      t22 = sin(x2 * pi)
      t23 = t22 ** 2
      t24 = z ** 2
      t27 = x1 ** 2
      t28 = t4 ** 2
      t32 = log(0.4D1 * t23 / t24 * t27 * t28)
      t39 = 0.1D1 / x4
      t43 = t7 * t10 * t11 / 0.8D1 + t7 * t8 * t15 / 0.8D1 + (-0.180D3 *
     # pi * lh - 0.90D2 * t32 * pi) * t6 * t10 / 0.720D3 + t7 * t10 * t3
     #9 / 0.8D1
      t44 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t43)
      t46 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t47 = s * t46
      t48 = t1 * x1
      t49 = t47 * t48
      t50 = t1 * t4
      t52 = t47 * t50 * x4
      t55 = t47 * t50 * (-0.1D1 + x4)
      t56 = t46 ** 2
      t59 = x1 * t4
      t61 = s * t56 * t6 * t59 * x4
      t62 = t7 * t8
      t66 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #52, t49, t55, -t61)
      t68 = t56 / (-0.2D1 + t46) * t66 * t39
      t70 = t62 * t68 / 0.8D1
      t71 = FJET(XB1, XB2, s, 0.0D0, t49, -t52, t55, -t61, t70)
      t73 = t6 * t8
      t77 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t43)
      t79 = FJET(XB1, XB2, s, t49, 0.0D0, t55, -t52, -t61, t70)
      t84 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t85 = s * t84
      t87 = t85 * t48 * x3
      t90 = t85 * t48 * (-0.1D1 + x3)
      t91 = t85 * t50
      t92 = t84 ** 2
      t96 = s * t92 * t6 * t59 * x3
      t100 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t87, 0.0
     #D0, -t90, -t91, -t96)
      t102 = t92 / (-0.2D1 + t84) * t100 * t11
      t104 = t62 * t102 / 0.8D1
      t105 = FJET(XB1, XB2, s, t87, -t90, 0.0D0, -t91, -t96, t104)
      t110 = FJET(XB1, XB2, s, -t90, t87, -t91, 0.0D0, -t96, t104)
      rrgg2qqbarhhardt4s1em1 = t44 * t43 + t71 * pi * t73 * t68 / 0.8D1 
     #+ t77 * t43 + t79 * pi * t73 * t68 / 0.8D1 + t105 * pi * t73 * t10
     #2 / 0.8D1 + t110 * pi * t73 * t102 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt4s1em2
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
      doubleprecision rrgg2qqbarhhard41J1
      doubleprecision rrgg2qqbarhhard41J2
      doubleprecision rrgg2qqbarhhard41J3
      doubleprecision rrgg2qqbarhhard41J4
      doubleprecision rrgg2qqbarhhard41J5
      doubleprecision rrgg2qqbarhhard41J6

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
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarhhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t3, -t5, 0.0D0)
      t12 = pi * t6 * t8 * t9 / 0.8D1
      t13 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t12)
      rrgg2qqbarhhardt4s1em2 = t13 * pi * t16 / 0.8D1 + t18 * pi * t16 /
     # 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt4s1em3
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
      doubleprecision rrgg2qqbarhhard41J1
      doubleprecision rrgg2qqbarhhard41J2
      doubleprecision rrgg2qqbarhhard41J3
      doubleprecision rrgg2qqbarhhard41J4
      doubleprecision rrgg2qqbarhhard41J5
      doubleprecision rrgg2qqbarhhard41J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt4s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt4s1em4
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
      doubleprecision rrgg2qqbarhhard41J1
      doubleprecision rrgg2qqbarhhard41J2
      doubleprecision rrgg2qqbarhhard41J3
      doubleprecision rrgg2qqbarhhard41J4
      doubleprecision rrgg2qqbarhhard41J5
      doubleprecision rrgg2qqbarhhard41J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarhhard41J1
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
      t4 = S23 ** 2
      t5 = S24 ** 2
      rrgg2qqbarhhard41J1 = (-nf * S12 / 0.3D1 + (-S24 - S23) * nf / 0.3
     #D1 + (-t4 - t5) * nf / S12 / 0.3D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard41J2
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
      t1 = S24 ** 2
      t2 = S23 ** 2
      rrgg2qqbarhhard41J2 = (-t1 - t2 + 0.2D1 * S23 * S24) * nf / S12 / 
     #pi * wd / z / 0.3D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard41J3
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
      t4 = S23 ** 2
      t7 = S24 ** 2
      rrgg2qqbarhhard41J3 = (nf * S12 / 0.3D1 + (S23 + S24) * nf / 0.3D1
     # + (-t4 + 0.4D1 * S23 * S24 - t7) * nf / S12 / 0.3D1) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard41J4
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
      t7 = S23 ** 2
      t10 = S24 ** 2
      rrgg2qqbarhhard41J4 = (0.2D1 / 0.3D1 * nf * S12 + (0.2D1 * S23 + 0
     #.2D1 * S24) * nf / 0.3D1 + (-t7 + 0.6D1 * S23 * S24 - t10) * nf / 
     #S12 / 0.3D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard41J5
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
      t6 = S23 ** 2
      t9 = S24 ** 2
      rrgg2qqbarhhard41J5 = (nf * S12 + (0.3D1 * S23 + 0.3D1 * S24) * nf
     # / 0.3D1 + (-t6 + 0.8D1 * S23 * S24 - t9) * nf / S12 / 0.3D1) / pi
     # * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard41J6
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
      t9 = S23 ** 2
      t10 = S24 ** 2
      rrgg2qqbarhhard41J6 = (0.10D2 / 0.3D1 * nf * S12 + 0.5D1 / 0.3D1 *
     # (0.2D1 * S23 + 0.2D1 * S24) * nf + 0.5D1 / 0.3D1 * (0.2D1 * S23 *
     # S24 + t9 + t10) * nf / S12) / pi * wd / z

      end function
  
   
      subroutine rrgg2qqbarhsoftt4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt4s1e1  
      doubleprecision rrgg2qqbarhsoftt4s1e0  
      doubleprecision rrgg2qqbarhsoftt4s1em1  
      doubleprecision rrgg2qqbarhsoftt4s1em2  
      doubleprecision rrgg2qqbarhsoftt4s1em3  
      doubleprecision rrgg2qqbarhsoftt4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt4s1e1
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
      rrgg2qqbarhsoftt4s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt4s1e0
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
      rrgg2qqbarhsoftt4s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt4s1em1
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
      rrgg2qqbarhsoftt4s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt4s1em2
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
      rrgg2qqbarhsoftt4s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt4s1em3
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
      rrgg2qqbarhsoftt4s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt4s1em4
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
      rrgg2qqbarhsoftt4s1em4 = 0.0D0

      end function
