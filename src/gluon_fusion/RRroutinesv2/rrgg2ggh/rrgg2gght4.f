      subroutine rrgg2gght4
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt4
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt4
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhard41J1  
      doubleprecision rrgg2gghhard41J2  
      doubleprecision rrgg2gghhard41J3  
      doubleprecision rrgg2gghhard41J4  
      doubleprecision rrgg2gghhard41J5  
      doubleprecision rrgg2gghhard41J6  
      doubleprecision rrgg2gghhardt4s1e1  
      doubleprecision rrgg2gghhardt4s1e0  
      doubleprecision rrgg2gghhardt4s1em1  
      doubleprecision rrgg2gghhardt4s1em2  
      doubleprecision rrgg2gghhardt4s1em3  
      doubleprecision rrgg2gghhardt4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt4s1e1
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
      doubleprecision rrgg2gghhard41J1
      doubleprecision rrgg2gghhard41J2
      doubleprecision rrgg2gghhard41J3
      doubleprecision rrgg2gghhard41J4
      doubleprecision rrgg2gghhard41J5
      doubleprecision rrgg2gghhard41J6

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
      t22 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t28 = rrgg2gghhard41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t33 = pi * lh
      t34 = t6 * t8
      t40 = pi ** 2
      t42 = lh ** 2
      t44 = -0.30D2 * t40 + 0.180D3 * t42
      t45 = pi * t44
      t46 = t34 * t25
      t47 = t45 * t46
      t49 = 0.1D1 / x4
      t54 = log(0.4D1 * t14 * t17)
      t55 = t54 * pi
      t58 = t54 ** 2
      t59 = t58 * pi
      t66 = rrgg2gghhard41J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t95 = x3 * t11
      t96 = t95 * t13
      t99 = log(0.4D1 * t96 * t18)
      t108 = 0.1D1 / x3
      t116 = log(0.4D1 * t95 * t13 * t15 * t16)
      t118 = t116 ** 2
      t133 = (0.90D2 * t7 * t8 * (-t21 * t22 + t24 * t25 / 0.2D1 + t28) 
     #- 0.180D3 * t33 * t34 * (t22 - t21 * t25) + t47) * t49 / 0.720D3 +
     # (t45 + 0.180D3 * t55 * lh + 0.45D2 * t59) * t6 * t8 * t22 / 0.720
     #D3 + t7 * t8 * t66 / 0.8D1 + (pi * (-0.240D3 * zeta3 - 0.120D3 * t
     #42 * lh + 0.60D2 * lh * t40) - t55 * t44 - 0.90D2 * t59 * lh - 0.1
     #5D2 * t58 * t54 * pi) * t6 * t8 * t25 / 0.720D3 + (-0.180D3 * t33 
     #- 0.90D2 * t55) * t6 * t8 * t28 / 0.720D3 + (0.90D2 * t7 * t8 * (t
     #22 - t99 * t25) - 0.180D3 * t33 * t46) * t108 * t49 / 0.720D3 + (0
     #.90D2 * t7 * t8 * (-t116 * t22 + t118 * t25 / 0.2D1 + t28) - 0.180
     #D3 * t33 * t34 * (t22 - t116 * t25) + t47) * t108 / 0.720D3
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
      t164 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t14
     #2, t139, t145, -t151)
      t167 = t160 ** 2
      t169 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t14
     #2, t139, t145, -t151)
      t170 = t163 * t169
      t173 = t146 * t163
      t174 = rrgg2gghhard41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t14
     #2, t139, t145, -t151)
      t180 = t173 * t164
      t186 = t45 * t6
      t188 = t8 * t146 * t170
      t193 = x3 * t15 * t14
      t196 = log(-0.4D1 * t193 * t157)
      t203 = t33 * t6
      t210 = (0.90D2 * t7 * t8 * (-t161 * t163 * t164 + t167 * t146 * t1
     #70 / 0.2D1 + t173 * t174) - 0.180D3 * t33 * t34 * (t180 - t161 * t
     #170) + t186 * t188) * t49 / 0.720D3 + (0.90D2 * t7 * t8 * (t180 - 
     #t196 * t146 * t170) - 0.180D3 * t203 * t188) * t108 * t49 / 0.720D
     #3
      t211 = FJET(XB1, XB2, s, 0.0D0, t139, -t142, t145, -t151, t210)
      t213 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t133)
      t215 = FJET(XB1, XB2, s, t139, 0.0D0, t145, -t142, -t151, t210)
      t217 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t218 = s * t217
      t219 = t138 * x3
      t220 = t218 * t219
      t221 = -0.1D1 + x3
      t222 = t138 * t221
      t223 = t218 * t222
      t224 = t218 * t140
      t225 = t217 ** 2
      t229 = s * t225 * t6 * t149 * x3
      t231 = 0.1D1 / (-0.2D1 + t217)
      t232 = t225 * t231
      t233 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, t220, 0.0D0
     #, -t223, -t224, -t229)
      t234 = t232 * t233
      t235 = t16 * t221
      t236 = t225 ** 2
      t241 = log(-0.4D1 * t193 * t235 * x4 * t236)
      t243 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t220, 0.0D0
     #, -t223, -t224, -t229)
      t244 = t231 * t243
      t251 = t8 * t225 * t244
      t261 = log(-0.4D1 * t96 * t17 * t221 * t236)
      t262 = t261 * t225
      t265 = t261 ** 2
      t269 = rrgg2gghhard41J3(s, XB1, XB2, z, lh, wd, nf, s, t220, 0.0D0
     #, -t223, -t224, -t229)
      t284 = (0.90D2 * t7 * t8 * (t234 - t241 * t225 * t244) - 0.180D3 *
     # t203 * t251) * t108 * t49 / 0.720D3 + (0.90D2 * t7 * t8 * (-t262 
     #* t231 * t233 + t265 * t225 * t244 / 0.2D1 + t232 * t269) - 0.180D
     #3 * t33 * t34 * (t234 - t262 * t244) + t186 * t251) * t108 / 0.720
     #D3
      t285 = FJET(XB1, XB2, s, t220, -t223, 0.0D0, -t224, -t229, t284)
      t287 = KAPPA2(x1, x2, x3, x4, z)
      t288 = s * t287
      t289 = t288 * t219
      t290 = t288 * t222
      t291 = t288 * t141
      t292 = t288 * t144
      t293 = t287 ** 2
      t298 = cos(t9)
      t301 = Sqrt(x3 * t221 * t154)
      t306 = s * t293 * t6 * t149 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t298 * t301)
      t308 = 0.1D1 / (-0.2D1 + t287)
      t310 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, t289, -t291
     #, -t290, t292, t306)
      t312 = t293 ** 2
      t317 = log(0.4D1 * t193 * t154 * t235 * t312)
      t319 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t289, -t291
     #, -t290, t292, t306)
      t320 = t308 * t319
      t330 = 0.90D2 * t7 * t8 * (-t293 * t308 * t310 + t317 * t293 * t32
     #0) + 0.180D3 * t203 * t8 * t293 * t320
      t333 = t330 * t108 * t49 / 0.720D3
      t334 = FJET(XB1, XB2, s, t289, -t290, -t291, t292, t306, t333)
      t336 = t108 * t49
      t339 = FJET(XB1, XB2, s, -t223, t220, -t224, 0.0D0, -t229, t284)
      t341 = FJET(XB1, XB2, s, -t290, t289, t292, -t291, t306, t333)
      rrgg2gghhardt4s1e1 = t134 * t133 + t211 * t210 + t213 * t133 + t21
     #5 * t210 + t285 * t284 + t334 * t330 * t336 / 0.720D3 + t339 * t28
     #4 + t341 * t330 * t336 / 0.720D3

      end function



      doubleprecision function rrgg2gghhardt4s1e0
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
      doubleprecision rrgg2gghhard41J1
      doubleprecision rrgg2gghhard41J2
      doubleprecision rrgg2gghhard41J3
      doubleprecision rrgg2gghhard41J4
      doubleprecision rrgg2gghhard41J5
      doubleprecision rrgg2gghhard41J6

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
      t9 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
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
      t23 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t29 = pi * lh
      t30 = t6 * t8
      t33 = 0.180D3 * t29 * t30 * t23
      t35 = 0.1D1 / x4
      t39 = 0.1D1 / x3
      t44 = x3 * t12
      t49 = log(0.4D1 * t44 * t14 * t16 * t17)
      t61 = log(0.4D1 * t15 * t18)
      t62 = t61 * pi
      t69 = pi ** 2
      t71 = lh ** 2
      t77 = t61 ** 2
      t85 = rrgg2gghhard41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t89 = (0.90D2 * t7 * t8 * (t9 - t22 * t23) - t33) * t35 / 0.720D3 
     #+ t7 * t8 * t23 * t39 * t35 / 0.8D1 + (0.90D2 * t7 * t8 * (t9 - t4
     #9 * t23) - t33) * t39 / 0.720D3 + (-0.180D3 * t29 - 0.90D2 * t62) 
     #* t6 * t8 * t9 / 0.720D3 + (pi * (-0.30D2 * t69 + 0.180D3 * t71) +
     # 0.180D3 * t62 * lh + 0.45D2 * t77 * pi) * t6 * t8 * t23 / 0.720D3
     # + t7 * t8 * t85 / 0.8D1
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
      t111 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t98
     #, t95, t101, -t107)
      t115 = x4 * t99
      t116 = t102 ** 2
      t121 = log(-0.4D1 * t16 * t12 * t14 * t115 * t116 * t17)
      t123 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t98
     #, t95, t101, -t107)
      t124 = t109 * t123
      t130 = t29 * t6
      t131 = t8 * t102
      t139 = t39 * t35
      t143 = (0.90D2 * t7 * t8 * (t102 * t109 * t111 - t121 * t102 * t12
     #4) - 0.180D3 * t130 * t131 * t124) * t35 / 0.720D3 + t7 * t131 * t
     #124 * t139 / 0.8D1
      t144 = FJET(XB1, XB2, s, 0.0D0, t95, -t98, t101, -t107, t143)
      t146 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t89)
      t148 = FJET(XB1, XB2, s, t95, 0.0D0, t101, -t98, -t107, t143)
      t150 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t151 = s * t150
      t152 = t94 * x3
      t153 = t151 * t152
      t154 = -0.1D1 + x3
      t155 = t94 * t154
      t156 = t151 * t155
      t157 = t151 * t96
      t158 = t150 ** 2
      t162 = s * t158 * t6 * t105 * x3
      t163 = t8 * t158
      t166 = 0.1D1 / (-0.2D1 + t150)
      t167 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t153, 0.0D0
     #, -t156, -t157, -t162)
      t168 = t166 * t167
      t173 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, t153, 0.0D0
     #, -t156, -t157, -t162)
      t176 = t158 ** 2
      t181 = log(-0.4D1 * t44 * t14 * t18 * t154 * t176)
      t194 = t7 * t163 * t168 * t139 / 0.8D1 + (0.90D2 * t7 * t8 * (t158
     # * t166 * t173 - t181 * t158 * t168) - 0.180D3 * t130 * t163 * t16
     #8) * t39 / 0.720D3
      t195 = FJET(XB1, XB2, s, t153, -t156, 0.0D0, -t157, -t162, t194)
      t197 = KAPPA2(x1, x2, x3, x4, z)
      t198 = s * t197
      t199 = t198 * t152
      t200 = t198 * t155
      t201 = t198 * t97
      t202 = t198 * t100
      t203 = t197 ** 2
      t208 = cos(t10)
      t211 = Sqrt(x3 * t154 * t115)
      t216 = s * t203 * t6 * t105 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t208 * t211)
      t220 = 0.1D1 / (-0.2D1 + t197)
      t221 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201
     #, -t200, t202, t216)
      t225 = t7 * t8 * t203 * t220 * t221 * t139 / 0.8D1
      t226 = FJET(XB1, XB2, s, t199, -t200, -t201, t202, t216, -t225)
      t232 = t203 * t220 * t221 * t39 * t35
      t235 = FJET(XB1, XB2, s, -t156, t153, -t157, 0.0D0, -t162, t194)
      t237 = FJET(XB1, XB2, s, -t200, t199, t202, -t201, t216, -t225)
      rrgg2gghhardt4s1e0 = t90 * t89 + t144 * t143 + t146 * t89 + t148 *
     # t143 + t195 * t194 - t226 * pi * t30 * t232 / 0.8D1 + t235 * t194
     # - t237 * pi * t30 * t232 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt4s1em1
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
      doubleprecision rrgg2gghhard41J1
      doubleprecision rrgg2gghhard41J2
      doubleprecision rrgg2gghhard41J3
      doubleprecision rrgg2gghhard41J4
      doubleprecision rrgg2gghhard41J5
      doubleprecision rrgg2gghhard41J6

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
      t9 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t10 = t8 * t9
      t11 = 0.1D1 / x3
      t15 = rrgg2gghhard41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
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
      t66 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t52,
     # t49, t55, -t61)
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
      t100 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, t87, 0.0D0,
     # -t90, -t91, -t96)
      t102 = t92 / (-0.2D1 + t84) * t100 * t11
      t104 = t62 * t102 / 0.8D1
      t105 = FJET(XB1, XB2, s, t87, -t90, 0.0D0, -t91, -t96, t104)
      t110 = FJET(XB1, XB2, s, -t90, t87, -t91, 0.0D0, -t96, t104)
      rrgg2gghhardt4s1em1 = t44 * t43 + t71 * pi * t73 * t68 / 0.8D1 + t
     #77 * t43 + t79 * pi * t73 * t68 / 0.8D1 + t105 * pi * t73 * t102 /
     # 0.8D1 + t110 * pi * t73 * t102 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt4s1em2
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
      doubleprecision rrgg2gghhard41J1
      doubleprecision rrgg2gghhard41J2
      doubleprecision rrgg2gghhard41J3
      doubleprecision rrgg2gghhard41J4
      doubleprecision rrgg2gghhard41J5
      doubleprecision rrgg2gghhard41J6

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
      t9 = rrgg2gghhard41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t12 = pi * t6 * t8 * t9 / 0.8D1
      t13 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t12)
      rrgg2gghhardt4s1em2 = t13 * pi * t16 / 0.8D1 + t18 * pi * t16 / 0.
     #8D1

      end function



      doubleprecision function rrgg2gghhardt4s1em3
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
      doubleprecision rrgg2gghhard41J1
      doubleprecision rrgg2gghhard41J2
      doubleprecision rrgg2gghhard41J3
      doubleprecision rrgg2gghhard41J4
      doubleprecision rrgg2gghhard41J5
      doubleprecision rrgg2gghhard41J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt4s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt4s1em4
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
      doubleprecision rrgg2gghhard41J1
      doubleprecision rrgg2gghhard41J2
      doubleprecision rrgg2gghhard41J3
      doubleprecision rrgg2gghhard41J4
      doubleprecision rrgg2gghhard41J5
      doubleprecision rrgg2gghhard41J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2gghhard41J1
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = t2 + t4
      t11 = 0.9D1 * S24
      t16 = 0.9D1 * S23
      t20 = S34 ** 2
      t30 = S24 ** 2
      t32 = S23 ** 2
      t34 = S23 * S24
      t35 = 0.9D1 / 0.2D1 * t34
      t43 = 0.1D1 / S12
      t48 = t2 * t4
      t49 = S12 ** 2
      t67 = t20 ** 2
      rrgg2gghhard41J1 = ((0.9D1 / 0.2D1 * t5 * S12 - 0.27D2 / 0.2D1 * t
     #5 * S34 + (-t11 + 0.9D1 / 0.2D1 * S23) * t2 + (0.9D1 / 0.2D1 * S24
     # - t16) * t4 + (0.18D2 * t5 * t20 + ((0.27D2 / 0.2D1 * S24 + t16) 
     #* t2 + (0.27D2 / 0.2D1 * S23 + t11) * t4) * S34 + (0.9D1 / 0.2D1 *
     # t30 + 0.9D1 * t32 - t35) * t2 + (-t35 + 0.9D1 / 0.2D1 * t32 + 0.9
     #D1 * t30) * t4) * t43) * s * z + 0.9D1 * t48 * t49 * S12 - 0.18D2 
     #* S34 * t4 * t2 * t49 + (0.18D2 + 0.27D2 * t48 * t20) * S12 - 0.18
     #D2 * t48 * t20 * S34 + 0.90D2 * S34 + 0.72D2 * S24 + 0.72D2 * S23 
     #+ (0.9D1 * t48 * t67 + 0.9D1 * t20 + (0.63D2 * S24 + 0.63D2 * S23)
     # * S34 + 0.18D2 * t30 + 0.54D2 * t34 + 0.18D2 * t32) * t43) / pi *
     # wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard41J2
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = t2 + t4
      t17 = S34 ** 2
      t24 = S24 ** 2
      t26 = S23 ** 2
      t28 = S23 * S24
      t29 = 0.9D1 / 0.2D1 * t28
      t37 = 0.1D1 / S12
      t42 = t2 * t4
      t43 = S12 ** 2
      t61 = t17 ** 2
      rrgg2gghhard41J2 = ((0.9D1 / 0.2D1 * t5 * S12 + (-0.9D1 * S24 + 0.
     #9D1 / 0.2D1 * S23) * t2 + (0.9D1 / 0.2D1 * S24 - 0.9D1 * S23) * t4
     # + (0.18D2 * t5 * t17 + (0.9D1 * S23 * t2 + 0.9D1 * S24 * t4) * S3
     #4 + (0.9D1 / 0.2D1 * t24 + 0.9D1 * t26 - t29) * t2 + (-t29 + 0.9D1
     # / 0.2D1 * t26 + 0.9D1 * t24) * t4) * t37) * s * z + 0.9D1 * t42 *
     # t43 * S12 - 0.18D2 * S34 * t4 * t2 * t43 + (0.18D2 + 0.27D2 * t42
     # * t17) * S12 - 0.18D2 * t42 * t17 * S34 + 0.81D2 * S34 + 0.72D2 *
     # S24 + 0.72D2 * S23 + (0.9D1 * t42 * t61 + (0.54D2 * S23 + 0.54D2 
     #* S24) * S34 + 0.18D2 * t24 + 0.54D2 * t28 + 0.18D2 * t26) * t37) 
     #/ pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard41J3
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = t2 + t4
      t10 = 0.9D1 * S24
      t15 = 0.9D1 * S23
      t19 = S34 ** 2
      t29 = S24 ** 2
      t31 = S23 ** 2
      t33 = S23 * S24
      t34 = 0.9D1 / 0.2D1 * t33
      t42 = 0.1D1 / S12
      t47 = t2 * t4
      t48 = S12 ** 2
      t66 = t19 ** 2
      rrgg2gghhard41J3 = ((0.9D1 / 0.2D1 * t5 * S12 + 0.27D2 / 0.2D1 * t
     #5 * S34 + (-t10 + 0.9D1 / 0.2D1 * S23) * t2 + (0.9D1 / 0.2D1 * S24
     # - t15) * t4 + (0.18D2 * t5 * t19 + ((-0.27D2 / 0.2D1 * S24 + t15)
     # * t2 + (-0.27D2 / 0.2D1 * S23 + t10) * t4) * S34 + (0.9D1 / 0.2D1
     # * t29 + 0.9D1 * t31 - t34) * t2 + (-t34 + 0.9D1 / 0.2D1 * t31 + 0
     #.9D1 * t29) * t4) * t42) * s * z + 0.9D1 * t47 * t48 * S12 - 0.18D
     #2 * S34 * t4 * t2 * t48 + (0.18D2 + 0.27D2 * t47 * t19) * S12 - 0.
     #18D2 * t47 * t19 * S34 + 0.72D2 * S34 + 0.72D2 * S24 + 0.72D2 * S2
     #3 + (0.9D1 * t47 * t66 - 0.9D1 * t19 + (0.45D2 * S24 + 0.45D2 * S2
     #3) * S34 + 0.18D2 * t29 + 0.54D2 * t33 + 0.18D2 * t31) * t42) / pi
     # * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard41J4
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = t2 + t4
      t10 = 0.9D1 * S24
      t15 = 0.9D1 * S23
      t19 = S34 ** 2
      t29 = S24 ** 2
      t31 = S23 ** 2
      t33 = S23 * S24
      t34 = 0.9D1 / 0.2D1 * t33
      t42 = 0.1D1 / S12
      t47 = t2 * t4
      t48 = S12 ** 2
      t66 = t19 ** 2
      rrgg2gghhard41J4 = ((0.9D1 / 0.2D1 * t5 * S12 + 0.27D2 * t5 * S34 
     #+ (-t10 + 0.9D1 / 0.2D1 * S23) * t2 + (0.9D1 / 0.2D1 * S24 - t15) 
     #* t4 + (0.18D2 * t5 * t19 + ((-0.27D2 * S24 + t15) * t2 + (-0.27D2
     # * S23 + t10) * t4) * S34 + (0.9D1 / 0.2D1 * t29 + 0.9D1 * t31 - t
     #34) * t2 + (-t34 + 0.9D1 / 0.2D1 * t31 + 0.9D1 * t29) * t4) * t42)
     # * s * z + 0.9D1 * t47 * t48 * S12 - 0.18D2 * S34 * t4 * t2 * t48 
     #+ (0.18D2 + 0.27D2 * t47 * t19) * S12 - 0.18D2 * t47 * t19 * S34 +
     # 0.63D2 * S34 + 0.72D2 * S24 + 0.72D2 * S23 + (0.9D1 * t47 * t66 -
     # 0.18D2 * t19 + (0.36D2 * S24 + 0.36D2 * S23) * S34 + 0.18D2 * t29
     # + 0.54D2 * t33 + 0.18D2 * t31) * t42) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard41J5
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = t2 + t4
      t10 = 0.9D1 * S24
      t15 = 0.9D1 * S23
      t19 = S34 ** 2
      t29 = S24 ** 2
      t31 = S23 ** 2
      t33 = S23 * S24
      t34 = 0.9D1 / 0.2D1 * t33
      t42 = 0.1D1 / S12
      t47 = t2 * t4
      t48 = S12 ** 2
      t66 = t19 ** 2
      rrgg2gghhard41J5 = ((0.9D1 / 0.2D1 * t5 * S12 + 0.81D2 / 0.2D1 * t
     #5 * S34 + (-t10 + 0.9D1 / 0.2D1 * S23) * t2 + (0.9D1 / 0.2D1 * S24
     # - t15) * t4 + (0.18D2 * t5 * t19 + ((-0.81D2 / 0.2D1 * S24 + t15)
     # * t2 + (-0.81D2 / 0.2D1 * S23 + t10) * t4) * S34 + (0.9D1 / 0.2D1
     # * t29 + 0.9D1 * t31 - t34) * t2 + (-t34 + 0.9D1 / 0.2D1 * t31 + 0
     #.9D1 * t29) * t4) * t42) * s * z + 0.9D1 * t47 * t48 * S12 - 0.18D
     #2 * S34 * t4 * t2 * t48 + (0.18D2 + 0.27D2 * t47 * t19) * S12 - 0.
     #18D2 * t47 * t19 * S34 + 0.54D2 * S34 + 0.72D2 * S24 + 0.72D2 * S2
     #3 + (0.9D1 * t47 * t66 - 0.27D2 * t19 + (0.27D2 * S24 + 0.27D2 * S
     #23) * S34 + 0.18D2 * t29 + 0.54D2 * t33 + 0.18D2 * t31) * t42) / p
     #i * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard41J6
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
      t4 = 0.1D1 / (S12 + S13 + S23)
      t5 = -t2 - t4
      t11 = 0.45D2 * S24
      t16 = 0.45D2 * S23
      t20 = S34 ** 2
      t30 = S24 ** 2
      t32 = S23 ** 2
      t34 = S23 * S24
      t35 = 0.45D2 / 0.2D1 * t34
      t43 = 0.1D1 / S12
      t48 = t2 * t4
      t49 = S12 ** 2
      t67 = t20 ** 2
      rrgg2gghhard41J6 = ((0.45D2 / 0.2D1 * t5 * S12 - 0.135D3 * t5 * S3
     #4 + (t11 - 0.45D2 / 0.2D1 * S23) * t2 + (-0.45D2 / 0.2D1 * S24 + t
     #16) * t4 + (0.90D2 * t5 * t20 + ((-0.135D3 * S24 - t16) * t2 + (-0
     #.135D3 * S23 - t11) * t4) * S34 + (-0.45D2 / 0.2D1 * t30 - 0.45D2 
     #* t32 + t35) * t2 + (t35 - 0.45D2 / 0.2D1 * t32 - 0.45D2 * t30) * 
     #t4) * t43) * s * z - 0.45D2 * t48 * t49 * S12 + 0.90D2 * S34 * t4 
     #* t2 * t49 + (-0.90D2 - 0.135D3 * t48 * t20) * S12 + 0.90D2 * t48 
     #* t20 * S34 - 0.495D3 * S34 - 0.360D3 * S24 - 0.360D3 * S23 + (-0.
     #45D2 * t48 * t67 - 0.90D2 * t20 + (-0.360D3 * S24 - 0.360D3 * S23)
     # * S34 - 0.90D2 * t30 - 0.270D3 * t34 - 0.90D2 * t32) * t43) / pi 
     #* wd / z

      end function
  
   
      subroutine rrgg2gghsoftt4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt4s1e1  
      doubleprecision rrgg2gghsoftt4s1e0  
      doubleprecision rrgg2gghsoftt4s1em1  
      doubleprecision rrgg2gghsoftt4s1em2  
      doubleprecision rrgg2gghsoftt4s1em3  
      doubleprecision rrgg2gghsoftt4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt4s1e1
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
      rrgg2gghsoftt4s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt4s1e0
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
      rrgg2gghsoftt4s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt4s1em1
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
      rrgg2gghsoftt4s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt4s1em2
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
      rrgg2gghsoftt4s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt4s1em3
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
      rrgg2gghsoftt4s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt4s1em4
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
      rrgg2gghsoftt4s1em4 = 0.0D0

      end function
