      subroutine rrgg2qqbarht3
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt3
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt3
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhard31J1  
      doubleprecision rrgg2qqbarhhard31J2  
      doubleprecision rrgg2qqbarhhard31J3  
      doubleprecision rrgg2qqbarhhard31J4  
      doubleprecision rrgg2qqbarhhard31J5  
      doubleprecision rrgg2qqbarhhard31J6  
      doubleprecision rrgg2qqbarhhard31J7  
      doubleprecision rrgg2qqbarhhardt3s1e1  
      doubleprecision rrgg2qqbarhhardt3s1e0  
      doubleprecision rrgg2qqbarhhardt3s1em1  
      doubleprecision rrgg2qqbarhhardt3s1em2  
      doubleprecision rrgg2qqbarhhardt3s1em3  
      doubleprecision rrgg2qqbarhhardt3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt3s1e1
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
      doubleprecision rrgg2qqbarhhard31J1
      doubleprecision rrgg2qqbarhhard31J2
      doubleprecision rrgg2qqbarhhard31J3
      doubleprecision rrgg2qqbarhhard31J4
      doubleprecision rrgg2qqbarhhard31J5
      doubleprecision rrgg2qqbarhhard31J6
      doubleprecision rrgg2qqbarhhard31J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = pi * t11
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrgg2qqbarhhard31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t21 = t19 * t20
      t22 = x2 * pi
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = x1 ** 2
      t29 = t27 * t28
      t30 = t6 ** 2
      t31 = t30 * x4
      t32 = t9 ** 2
      t36 = log(0.4D1 * t29 * t31 * t32)
      t37 = t36 * t9
      t38 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t39 = t18 * t38
      t41 = t36 ** 2
      t43 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t44 = t18 * t43
      t51 = pi * lh
      t52 = t11 * t16
      t53 = t19 * t38
      t59 = lh ** 2
      t61 = pi ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t64 = pi * t63
      t65 = t64 * t11
      t67 = t16 * t9 * t44
      t68 = t65 * t67
      t70 = 0.1D1 / x4
      t73 = t28 * t30
      t74 = t73 * t32
      t77 = log(0.4D1 * t27 * t74)
      t78 = t77 * pi
      t81 = t77 ** 2
      t82 = t81 * pi
      t90 = rrgg2qqbarhhard31J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t120 = x3 * t24
      t121 = t120 * t26
      t126 = log(0.4D1 * t121 * t73 * x4 * t32)
      t133 = t51 * t11
      t137 = 0.1D1 / x3
      t143 = log(0.4D1 * t121 * t74)
      t144 = t143 * t9
      t146 = t143 ** 2
      t162 = (0.90D2 * t15 * t16 * (-t21 + t37 * t39 - t41 * t9 * t44 / 
     #0.2D1) - 0.180D3 * t51 * t52 * (-t53 + t37 * t44) - t68) * t70 / 0
     #.720D3 - (t64 + 0.180D3 * t78 * lh + 0.45D2 * t82) * t11 * t16 * t
     #53 / 0.720D3 - t15 * t16 * t19 * t90 / 0.8D1 - (pi * (0.60D2 * lh 
     #* t61 - 0.240D3 * zeta3 - 0.120D3 * t59 * lh) - t78 * t63 - 0.90D2
     # * t82 * lh - 0.15D2 * t81 * t77 * pi) * t11 * t16 * t19 * t43 / 0
     #.720D3 - (-0.180D3 * t51 - 0.90D2 * t78) * t11 * t16 * t21 / 0.720
     #D3 + (0.90D2 * t15 * t16 * (-t53 + t126 * t9 * t44) + 0.180D3 * t1
     #33 * t67) * t137 * t70 / 0.720D3 + (0.90D2 * t15 * t16 * (-t21 + t
     #144 * t39 - t146 * t9 * t44 / 0.2D1) - 0.180D3 * t51 * t52 * (-t53
     # + t144 * t44) - t68) * t137 / 0.720D3
      t163 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t162)
      t165 = 0.1D1 - x4
      t166 = KAPPA2(x1, x2, 0.0D0, t165, z)
      t167 = s * t166
      t168 = t167 * t4
      t169 = -t165
      t170 = t7 * t169
      t171 = t167 * t170
      t172 = t7 * x4
      t173 = t167 * t172
      t174 = t166 ** 2
      t177 = x1 * t6
      t179 = s * t174 * t11 * t177 * t169
      t181 = 0.1D1 / (-0.2D1 + t166)
      t182 = t174 * t181
      t183 = rrgg2qqbarhhard31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #171, t168, -t173, t179)
      t185 = t174 ** 2
      t187 = t31 * t169 * t185
      t190 = log(-0.4D1 * t29 * t187)
      t191 = t190 * t174
      t192 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #171, t168, -t173, t179)
      t195 = t190 ** 2
      t197 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #171, t168, -t173, t179)
      t198 = t181 * t197
      t205 = t182 * t192
      t212 = t16 * t174 * t198
      t217 = t120 * t26 * t28
      t220 = log(-0.4D1 * t217 * t187)
      t233 = (0.90D2 * t15 * t16 * (t182 * t183 - t191 * t181 * t192 + t
     #195 * t174 * t198 / 0.2D1) - 0.180D3 * t51 * t52 * (t205 - t191 * 
     #t198) + t65 * t212) * t70 / 0.720D3 + (0.90D2 * t15 * t16 * (t205 
     #- t220 * t174 * t198) - 0.180D3 * t133 * t212) * t137 * t70 / 0.72
     #0D3
      t234 = FJET(XB1, XB2, s, 0.0D0, t168, t171, -t173, t179, t233)
      t236 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t162)
      t238 = FJET(XB1, XB2, s, t168, 0.0D0, -t173, t171, t179, t233)
      t240 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t241 = s * t240
      t242 = t4 * x3
      t243 = t241 * t242
      t244 = -0.1D1 + x3
      t245 = t4 * t244
      t246 = t241 * t245
      t247 = t241 * t7
      t248 = t240 ** 2
      t252 = s * t248 * t11 * t177 * t244
      t254 = 0.1D1 / (-0.2D1 + t240)
      t255 = t248 * t254
      t256 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, t243, -t
     #247, -t246, 0.0D0, t252)
      t257 = t255 * t256
      t258 = t30 * t244
      t259 = t248 ** 2
      t264 = log(-0.4D1 * t217 * t258 * x4 * t259)
      t266 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, t243, -t
     #247, -t246, 0.0D0, t252)
      t267 = t254 * t266
      t274 = t16 * t248 * t267
      t280 = rrgg2qqbarhhard31J3(s, XB1, XB2, z, lh, wd, nf, s, t243, -t
     #247, -t246, 0.0D0, t252)
      t286 = log(-0.4D1 * t121 * t73 * t244 * t259)
      t287 = t286 * t248
      t290 = t286 ** 2
      t307 = (0.90D2 * t15 * t16 * (t257 - t264 * t248 * t267) - 0.180D3
     # * t133 * t274) * t137 * t70 / 0.720D3 + (-0.90D2 * t15 * t16 * (-
     #t255 * t280 + t287 * t254 * t256 - t290 * t248 * t267 / 0.2D1) + 0
     #.180D3 * t51 * t52 * (-t257 + t287 * t267) + t65 * t274) * t137 / 
     #0.720D3
      t308 = FJET(XB1, XB2, s, t243, -t246, -t247, 0.0D0, t252, t307)
      t310 = KAPPA2(x1, x2, x3, t165, z)
      t311 = s * t310
      t312 = t311 * t242
      t313 = t311 * t245
      t314 = t311 * t170
      t315 = t311 * t172
      t316 = t310 ** 2
      t321 = cos(t22)
      t323 = x4 * t169
      t325 = Sqrt(x3 * t244 * t323)
      t330 = s * t316 * t11 * t177 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t321 * t325)
      t332 = 0.1D1 / (-0.2D1 + t310)
      t334 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, t312, t3
     #14, -t313, -t315, t330)
      t336 = t316 ** 2
      t341 = log(0.4D1 * t217 * t258 * t323 * t336)
      t343 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, t312, t3
     #14, -t313, -t315, t330)
      t344 = t332 * t343
      t346 = -t316 * t332 * t334 + t341 * t316 * t344
      t353 = 0.180D3 * t133 * t16 * t316 * t344
      t354 = 0.90D2 * t15 * t16 * t346 + t353
      t358 = FJET(XB1, XB2, s, t312, -t313, t314, -t315, t330, t354 * t1
     #37 * t70 / 0.720D3)
      t360 = t137 * t70
      t363 = FJET(XB1, XB2, s, -t246, t243, 0.0D0, -t247, t252, t307)
      t369 = 0.90D2 * t15 * t16 * t346 + t353
      t373 = FJET(XB1, XB2, s, -t313, t312, -t315, t314, t330, t369 * t1
     #37 * t70 / 0.720D3)
      rrgg2qqbarhhardt3s1e1 = t163 * t162 + t234 * t233 + t236 * t162 + 
     #t238 * t233 + t308 * t307 + t358 * t354 * t360 / 0.720D3 + t363 * 
     #t307 + t373 * t369 * t360 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarhhardt3s1e0
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
      doubleprecision rrgg2qqbarhhard31J1
      doubleprecision rrgg2qqbarhhard31J2
      doubleprecision rrgg2qqbarhhard31J3
      doubleprecision rrgg2qqbarhhard31J4
      doubleprecision rrgg2qqbarhhard31J5
      doubleprecision rrgg2qqbarhhard31J6
      doubleprecision rrgg2qqbarhhard31J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = pi * t11
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t21 = t19 * t20
      t22 = x2 * pi
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = x1 ** 2
      t29 = t27 * t28
      t30 = t6 ** 2
      t31 = t30 * x4
      t32 = t9 ** 2
      t36 = log(0.4D1 * t29 * t31 * t32)
      t38 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t39 = t18 * t38
      t45 = pi * lh
      t46 = t45 * t11
      t47 = t16 * t9
      t50 = 0.180D3 * t46 * t47 * t39
      t52 = 0.1D1 / x4
      t56 = 0.1D1 / x3
      t57 = t56 * t52
      t62 = x3 * t24 * t26
      t63 = t28 * t30
      t64 = t63 * t32
      t67 = log(0.4D1 * t62 * t64)
      t78 = rrgg2qqbarhhard31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t85 = log(0.4D1 * t27 * t64)
      t86 = t85 * pi
      t93 = lh ** 2
      t95 = pi ** 2
      t101 = t85 ** 2
      t110 = (0.90D2 * t15 * t16 * (-t21 + t36 * t9 * t39) + t50) * t52 
     #/ 0.720D3 - t15 * t47 * t39 * t57 / 0.8D1 + (0.90D2 * t15 * t16 * 
     #(-t21 + t67 * t9 * t39) + t50) * t56 / 0.720D3 - t15 * t16 * t19 *
     # t78 / 0.8D1 - (-0.180D3 * t45 - 0.90D2 * t86) * t11 * t16 * t21 /
     # 0.720D3 - (pi * (0.180D3 * t93 - 0.30D2 * t95) + 0.180D3 * t86 * 
     #lh + 0.45D2 * t101 * pi) * t11 * t16 * t19 * t38 / 0.720D3
      t111 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t110)
      t113 = 0.1D1 - x4
      t114 = KAPPA2(x1, x2, 0.0D0, t113, z)
      t115 = s * t114
      t116 = t115 * t4
      t117 = -t113
      t118 = t7 * t117
      t119 = t115 * t118
      t120 = t7 * x4
      t121 = t115 * t120
      t122 = t114 ** 2
      t125 = x1 * t6
      t127 = s * t122 * t11 * t125 * t117
      t129 = 0.1D1 / (-0.2D1 + t114)
      t131 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #119, t116, -t121, t127)
      t133 = t122 ** 2
      t138 = log(-0.4D1 * t29 * t31 * t117 * t133)
      t140 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #119, t116, -t121, t127)
      t141 = t129 * t140
      t147 = t16 * t122
      t158 = (0.90D2 * t15 * t16 * (t122 * t129 * t131 - t138 * t122 * t
     #141) - 0.180D3 * t46 * t147 * t141) * t52 / 0.720D3 + t15 * t147 *
     # t141 * t57 / 0.8D1
      t159 = FJET(XB1, XB2, s, 0.0D0, t116, t119, -t121, t127, t158)
      t161 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t110)
      t163 = FJET(XB1, XB2, s, t116, 0.0D0, -t121, t119, t127, t158)
      t165 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t166 = s * t165
      t167 = t4 * x3
      t168 = t166 * t167
      t169 = -0.1D1 + x3
      t170 = t4 * t169
      t171 = t166 * t170
      t172 = t166 * t7
      t173 = t165 ** 2
      t177 = s * t173 * t11 * t125 * t169
      t178 = t16 * t173
      t181 = 0.1D1 / (-0.2D1 + t165)
      t182 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, t168, -t
     #172, -t171, 0.0D0, t177)
      t183 = t181 * t182
      t188 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, t168, -t
     #172, -t171, 0.0D0, t177)
      t190 = t173 ** 2
      t195 = log(-0.4D1 * t62 * t63 * t169 * t190)
      t208 = t15 * t178 * t183 * t57 / 0.8D1 + (-0.90D2 * t15 * t16 * (-
     #t173 * t181 * t188 + t195 * t173 * t183) - 0.180D3 * t46 * t178 * 
     #t183) * t56 / 0.720D3
      t209 = FJET(XB1, XB2, s, t168, -t171, -t172, 0.0D0, t177, t208)
      t211 = KAPPA2(x1, x2, x3, t113, z)
      t212 = s * t211
      t213 = t212 * t167
      t214 = t212 * t170
      t215 = t212 * t118
      t216 = t212 * t120
      t217 = t211 ** 2
      t222 = cos(t22)
      t226 = Sqrt(x3 * t169 * x4 * t117)
      t231 = s * t217 * t11 * t125 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t222 * t226)
      t235 = 0.1D1 / (-0.2D1 + t211)
      t236 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, t213, t2
     #15, -t214, -t216, t231)
      t240 = t15 * t16 * t217 * t235 * t236 * t57 / 0.8D1
      t241 = FJET(XB1, XB2, s, t213, -t214, t215, -t216, t231, -t240)
      t243 = t11 * t16
      t248 = t217 * t235 * t236 * t56 * t52
      t251 = FJET(XB1, XB2, s, -t171, t168, 0.0D0, -t172, t177, t208)
      t253 = FJET(XB1, XB2, s, -t214, t213, -t216, t215, t231, -t240)
      rrgg2qqbarhhardt3s1e0 = t111 * t110 + t159 * t158 + t161 * t110 + 
     #t163 * t158 + t209 * t208 - t241 * pi * t243 * t248 / 0.8D1 + t251
     # * t208 - t253 * pi * t243 * t248 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt3s1em1
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
      doubleprecision rrgg2qqbarhhard31J1
      doubleprecision rrgg2qqbarhhard31J2
      doubleprecision rrgg2qqbarhhard31J3
      doubleprecision rrgg2qqbarhhard31J4
      doubleprecision rrgg2qqbarhhard31J5
      doubleprecision rrgg2qqbarhhard31J6
      doubleprecision rrgg2qqbarhhard31J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t17 = pi * t11 * t16
      t20 = t9 / (-0.2D1 + t1)
      t21 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t22 = 0.1D1 / x3
      t27 = rrgg2qqbarhhard31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t34 = sin(x2 * pi)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = x1 ** 2
      t40 = t6 ** 2
      t42 = t9 ** 2
      t46 = log(0.4D1 * t35 / t36 * t39 * t40 * t42)
      t55 = 0.1D1 / x4
      t60 = -t17 * t20 * t21 * t22 / 0.8D1 - t17 * t20 * t27 / 0.8D1 - (
     #-0.180D3 * pi * lh - 0.90D2 * t46 * pi) * t11 * t16 * t20 * t21 / 
     #0.720D3 - t17 * t20 * t21 * t55 / 0.8D1
      t61 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t60)
      t63 = 0.1D1 - x4
      t64 = KAPPA2(x1, x2, 0.0D0, t63, z)
      t65 = s * t64
      t66 = t65 * t4
      t67 = -t63
      t69 = t65 * t7 * t67
      t71 = t65 * t7 * x4
      t72 = t64 ** 2
      t75 = x1 * t6
      t77 = s * t72 * t11 * t75 * t67
      t81 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6
     #9, t66, -t71, t77)
      t83 = t72 / (-0.2D1 + t64) * t81 * t55
      t85 = t17 * t83 / 0.8D1
      t86 = FJET(XB1, XB2, s, 0.0D0, t66, t69, -t71, t77, t85)
      t88 = t11 * t16
      t92 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t60)
      t94 = FJET(XB1, XB2, s, t66, 0.0D0, -t71, t69, t77, t85)
      t99 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t100 = s * t99
      t102 = t100 * t4 * x3
      t103 = -0.1D1 + x3
      t105 = t100 * t4 * t103
      t106 = t100 * t7
      t107 = t99 ** 2
      t111 = s * t107 * t11 * t75 * t103
      t115 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, t102, -t
     #106, -t105, 0.0D0, t111)
      t117 = t107 / (-0.2D1 + t99) * t115 * t22
      t119 = t17 * t117 / 0.8D1
      t120 = FJET(XB1, XB2, s, t102, -t105, -t106, 0.0D0, t111, t119)
      t125 = FJET(XB1, XB2, s, -t105, t102, 0.0D0, -t106, t111, t119)
      rrgg2qqbarhhardt3s1em1 = t61 * t60 + t86 * pi * t88 * t83 / 0.8D1 
     #+ t92 * t60 + t94 * pi * t88 * t83 / 0.8D1 + t120 * pi * t88 * t11
     #7 / 0.8D1 + t125 * pi * t88 * t117 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt3s1em2
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
      doubleprecision rrgg2qqbarhhard31J1
      doubleprecision rrgg2qqbarhhard31J2
      doubleprecision rrgg2qqbarhhard31J3
      doubleprecision rrgg2qqbarhhard31J4
      doubleprecision rrgg2qqbarhhard31J5
      doubleprecision rrgg2qqbarhhard31J6
      doubleprecision rrgg2qqbarhhard31J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t19 = 0.1D1 / (-0.2D1 + t1)
      t21 = rrgg2qqbarhhard31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #8, t5, 0.0D0, -t14)
      t24 = pi * t11 * t16 * t9 * t19 * t21 / 0.8D1
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, -t24)
      t30 = t16 * t9 * t19 * t21
      t32 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, -t24)
      rrgg2qqbarhhardt3s1em2 = -t25 * pi * t11 * t30 / 0.8D1 - t32 * pi 
     #* t11 * t30 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt3s1em3
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
      doubleprecision rrgg2qqbarhhard31J1
      doubleprecision rrgg2qqbarhhard31J2
      doubleprecision rrgg2qqbarhhard31J3
      doubleprecision rrgg2qqbarhhard31J4
      doubleprecision rrgg2qqbarhhard31J5
      doubleprecision rrgg2qqbarhhard31J6
      doubleprecision rrgg2qqbarhhard31J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt3s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt3s1em4
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
      doubleprecision rrgg2qqbarhhard31J1
      doubleprecision rrgg2qqbarhhard31J2
      doubleprecision rrgg2qqbarhhard31J3
      doubleprecision rrgg2qqbarhhard31J4
      doubleprecision rrgg2qqbarhhard31J5
      doubleprecision rrgg2qqbarhhard31J6
      doubleprecision rrgg2qqbarhhard31J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarhhard31J1
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
      rrgg2qqbarhhard31J1 = (0.4D1 / 0.3D1 * nf * S34 - 0.4D1 / 0.3D1 * 
     #S14 * S23 * nf / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard31J2
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
      t7 = S14 ** 2
      t9 = S23 ** 2
      rrgg2qqbarhhard31J2 = (0.4D1 / 0.3D1 * nf * S12 + 0.8D1 / 0.3D1 * 
     #nf * S34 + (-0.16D2 * S14 * S23 - 0.4D1 * t7 - 0.4D1 * t9) * nf / 
     #S12 / 0.6D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard31J3
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
      t1 = nf * S34
      t2 = 0.1D1 / S12
      t10 = S23 + S14
      t14 = S34 ** 2
      t24 = S14 ** 2
      t26 = S23 ** 2
      rrgg2qqbarhhard31J3 = (0.8D1 * t1 * t2 * s * z + 0.4D1 * nf * S12 
     #+ 0.2D1 * t1 + t10 * nf / 0.3D1 + (-0.7D1 * nf * t14 - 0.13D2 / 0.
     #3D1 * t10 * nf * S34 + (-0.28D2 * S14 * S23 - 0.7D1 * t24 - 0.7D1 
     #* t26) * nf / 0.6D1) * t2) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard31J4
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
      t1 = nf * S34
      t2 = 0.1D1 / S12
      t10 = S23 + S14
      t14 = S34 ** 2
      t24 = S14 ** 2
      t26 = S23 ** 2
      rrgg2qqbarhhard31J4 = (0.16D2 * t1 * t2 * s * z + 0.20D2 / 0.3D1 *
     # nf * S12 + 0.4D1 / 0.3D1 * t1 + 0.2D1 / 0.3D1 * t10 * nf + (-0.14
     #D2 * nf * t14 - 0.26D2 / 0.3D1 * t10 * nf * S34 + (-0.40D2 * S14 *
     # S23 - 0.10D2 * t24 - 0.10D2 * t26) * nf / 0.6D1) * t2) / pi * wd 
     #/ z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard31J5
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
      t1 = nf * S34
      t2 = 0.1D1 / S12
      t10 = S23 + S14
      t14 = S34 ** 2
      t24 = S14 ** 2
      t26 = S23 ** 2
      rrgg2qqbarhhard31J5 = (0.24D2 * t1 * t2 * s * z + 0.28D2 / 0.3D1 *
     # nf * S12 + 0.2D1 / 0.3D1 * t1 + t10 * nf + (-0.21D2 * nf * t14 - 
     #0.13D2 * t10 * nf * S34 + (-0.52D2 * S14 * S23 - 0.13D2 * t24 - 0.
     #13D2 * t26) * nf / 0.6D1) * t2) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard31J6
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
      t1 = nf * S34
      t2 = 0.1D1 / S12
      t10 = S23 + S14
      t14 = S34 ** 2
      t22 = S14 ** 2
      t23 = S23 ** 2
      rrgg2qqbarhhard31J6 = (0.32D2 * t1 * t2 * s * z + 0.12D2 * nf * S1
     #2 - 0.8D1 * t1 + 0.4D1 / 0.3D1 * t10 * nf + (-0.28D2 * nf * t14 - 
     #0.52D2 / 0.3D1 * t10 * nf * S34 + (-0.16D2 * t22 - 0.16D2 * t23 - 
     #0.16D2 * S14 * S23) * nf / 0.6D1) * t2) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard31J7
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
      t1 = nf * S34
      t2 = 0.1D1 / S12
      t10 = S23 + S14
      t14 = S34 ** 2
      t22 = S14 ** 2
      t25 = S23 ** 2
      rrgg2qqbarhhard31J7 = (0.40D2 * t1 * t2 * s * z + 0.20D2 / 0.3D1 *
     # nf * S12 - 0.10D2 * t1 + 0.5D1 / 0.3D1 * t10 * nf + (-0.35D2 * nf
     # * t14 - 0.65D2 / 0.3D1 * t10 * nf * S34 + 0.5D1 / 0.6D1 * (t22 - 
     #0.4D1 * S14 * S23 + t25) * nf) * t2) / pi * wd / z

      end function
  
   
      subroutine rrgg2qqbarhsoftt3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt3s1e1  
      doubleprecision rrgg2qqbarhsoftt3s1e0  
      doubleprecision rrgg2qqbarhsoftt3s1em1  
      doubleprecision rrgg2qqbarhsoftt3s1em2  
      doubleprecision rrgg2qqbarhsoftt3s1em3  
      doubleprecision rrgg2qqbarhsoftt3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt3s1e1
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
      rrgg2qqbarhsoftt3s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt3s1e0
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
      rrgg2qqbarhsoftt3s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt3s1em1
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
      rrgg2qqbarhsoftt3s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt3s1em2
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
      rrgg2qqbarhsoftt3s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt3s1em3
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
      rrgg2qqbarhsoftt3s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt3s1em4
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
      rrgg2qqbarhsoftt3s1em4 = 0.0D0

      end function
