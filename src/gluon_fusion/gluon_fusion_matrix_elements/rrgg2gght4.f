  
      subroutine rrgg2gght4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh41J1  
      doubleprecision rrgg2ggh41J2  
      doubleprecision rrgg2ggh41J3  
      doubleprecision rrgg2ggh41J4  
      doubleprecision rrgg2ggh41J5  
      doubleprecision rrgg2ggh41J6  
      doubleprecision rrgg2gght4s1e1  
      doubleprecision rrgg2gght4s1e0  
      doubleprecision rrgg2gght4s1em1  
      doubleprecision rrgg2gght4s1em2  
      doubleprecision rrgg2gght4s1em3  
      doubleprecision rrgg2gght4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght4s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh41J1
      doubleprecision rrgg2ggh41J2
      doubleprecision rrgg2ggh41J3
      doubleprecision rrgg2ggh41J4
      doubleprecision rrgg2ggh41J5
      doubleprecision rrgg2ggh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = x2 * 0.3141592653589793D1
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
      t22 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t24 = t21 ** 2
      t25 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t28 = rrgg2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t33 = 0.3141592653589793D1 * lh
      t34 = t6 * t8
      t40 = lh ** 2
      t42 = 0.3141592653589793D1 ** 2
      t44 = 0.180D3 * t40 - 0.30D2 * t42
      t45 = 0.3141592653589793D1 * t44
      t46 = t34 * t25
      t47 = t45 * t46
      t49 = 0.1D1 / x4
      t54 = log(0.4D1 * t14 * t17)
      t55 = t54 * 0.3141592653589793D1
      t58 = t54 ** 2
      t59 = t58 * 0.3141592653589793D1
      t66 = rrgg2ggh41J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t94 = x3 * t11
      t95 = t94 * t13
      t98 = log(0.4D1 * t95 * t18)
      t107 = 0.1D1 / x3
      t115 = log(0.4D1 * t94 * t13 * t15 * t16)
      t117 = t115 ** 2
      t132 = (0.90D2 * t7 * t8 * (-t22 * t21 + t24 * t25 / 0.2D1 + t28) 
     #- 0.180D3 * t33 * t34 * (t22 - t21 * t25) + t47) * t49 / 0.720D3 +
     # (t45 + 0.180D3 * t55 * lh + 0.45D2 * t59) * t6 * t8 * t22 / 0.720
     #D3 + t7 * t8 * t66 / 0.8D1 + (0.3141592653589793D1 * (0.60D2 * lh 
     #* t42 - 0.2884936567583026D3 - 0.120D3 * t40 * lh) - t55 * t44 - 0
     #.90D2 * t59 * lh - 0.15D2 * t58 * t54 * 0.3141592653589793D1) * t6
     # * t8 * t25 / 0.720D3 + (-0.180D3 * t33 - 0.90D2 * t55) * t6 * t8 
     #* t28 / 0.720D3 - (0.90D2 * t7 * t8 * (-t22 + t98 * t25) + 0.180D3
     # * t33 * t46) * t107 * t49 / 0.720D3 + (0.90D2 * t7 * t8 * (-t115 
     #* t22 + t117 * t25 / 0.2D1 + t28) - 0.180D3 * t33 * t34 * (t22 - t
     #115 * t25) + t47) * t107 / 0.720D3
      t133 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t132)
      t135 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t136 = s * t135
      t137 = t1 * x1
      t138 = t136 * t137
      t139 = t1 * t4
      t140 = t139 * x4
      t141 = t136 * t140
      t142 = -0.1D1 + x4
      t143 = t139 * t142
      t144 = t136 * t143
      t145 = t135 ** 2
      t148 = x1 * t4
      t150 = s * t145 * t6 * t148 * x4
      t153 = x4 * t142
      t154 = t145 ** 2
      t156 = t153 * t154 * t16
      t159 = log(-0.4D1 * t15 * t11 * t13 * t156)
      t160 = t159 * t145
      t162 = 0.1D1 / (-0.2D1 + t135)
      t163 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t166 = t159 ** 2
      t168 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t169 = t162 * t168
      t172 = t145 * t162
      t173 = rrgg2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t179 = t172 * t163
      t185 = t45 * t6
      t187 = t8 * t145 * t169
      t192 = x3 * t15 * t14
      t195 = log(-0.4D1 * t192 * t156)
      t202 = t33 * t6
      t209 = (0.90D2 * t7 * t8 * (-t160 * t162 * t163 + t166 * t145 * t1
     #69 / 0.2D1 + t172 * t173) - 0.180D3 * t33 * t34 * (t179 - t160 * t
     #169) + t185 * t187) * t49 / 0.720D3 - (0.90D2 * t7 * t8 * (-t179 +
     # t195 * t145 * t169) + 0.180D3 * t202 * t187) * t107 * t49 / 0.720
     #D3
      t210 = FJET(XB1, XB2, s, 0.0D0, t138, -t141, t144, -t150, t209)
      t212 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t132)
      t214 = FJET(XB1, XB2, s, t138, 0.0D0, t144, -t141, -t150, t209)
      t216 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t217 = s * t216
      t218 = t137 * x3
      t219 = t217 * t218
      t220 = -0.1D1 + x3
      t221 = t137 * t220
      t222 = t217 * t221
      t223 = t217 * t139
      t224 = t216 ** 2
      t228 = s * t224 * t6 * t148 * x3
      t230 = 0.1D1 / (-0.2D1 + t216)
      t231 = t224 * t230
      t232 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t233 = t231 * t232
      t234 = t16 * t220
      t235 = t224 ** 2
      t240 = log(-0.4D1 * t192 * t234 * x4 * t235)
      t242 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t243 = t230 * t242
      t250 = t8 * t224 * t243
      t260 = log(-0.4D1 * t95 * t17 * t220 * t235)
      t261 = t260 * t224
      t264 = t260 ** 2
      t268 = rrgg2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t283 = -(0.90D2 * t7 * t8 * (-t233 + t240 * t224 * t243) + 0.180D3
     # * t202 * t250) * t107 * t49 / 0.720D3 + (0.90D2 * t7 * t8 * (-t26
     #1 * t230 * t232 + t264 * t224 * t243 / 0.2D1 + t231 * t268) - 0.18
     #0D3 * t33 * t34 * (t233 - t261 * t243) + t185 * t250) * t107 / 0.7
     #20D3
      t284 = FJET(XB1, XB2, s, t219, -t222, 0.0D0, -t223, -t228, t283)
      t286 = KAPPA2(x1, x2, x3, x4, z)
      t287 = s * t286
      t288 = t287 * t218
      t289 = t287 * t221
      t290 = t287 * t140
      t291 = t287 * t143
      t292 = t286 ** 2
      t297 = cos(t9)
      t300 = Sqrt(x3 * t220 * t153)
      t305 = s * t292 * t6 * t148 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t297 * t300)
      t307 = 0.1D1 / (-0.2D1 + t286)
      t309 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t311 = t292 ** 2
      t316 = log(0.4D1 * t192 * t153 * t234 * t311)
      t318 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t319 = t307 * t318
      t329 = 0.90D2 * t7 * t8 * (t292 * t307 * t309 - t316 * t292 * t319
     #) - 0.180D3 * t202 * t8 * t292 * t319
      t332 = t329 * t107 * t49 / 0.720D3
      t333 = FJET(XB1, XB2, s, t288, -t289, -t290, t291, t305, -t332)
      t335 = t107 * t49
      t338 = FJET(XB1, XB2, s, -t222, t219, -t223, 0.0D0, -t228, t283)
      t340 = FJET(XB1, XB2, s, -t289, t288, t291, -t290, t305, -t332)
      rrgg2gght4s1e1 = t133 * t132 + t210 * t209 + t212 * t132 + t214 * 
     #t209 + t284 * t283 - t333 * t329 * t335 / 0.720D3 + t338 * t283 - 
     #t340 * t329 * t335 / 0.720D3

      end function



      doubleprecision function rrgg2gght4s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh41J1
      doubleprecision rrgg2ggh41J2
      doubleprecision rrgg2ggh41J3
      doubleprecision rrgg2ggh41J4
      doubleprecision rrgg2ggh41J5
      doubleprecision rrgg2ggh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D0
     #)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = x1 ** 2
      t17 = t4 ** 2
      t18 = t16 * t17
      t22 = log(0.4D1 * t15 * t18 * x4)
      t23 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t29 = 0.3141592653589793D1 * lh
      t30 = t6 * t8
      t33 = 0.180D3 * t29 * t30 * t23
      t35 = 0.1D1 / x4
      t39 = 0.1D1 / x3
      t44 = x3 * t12
      t49 = log(0.4D1 * t44 * t14 * t16 * t17)
      t61 = log(0.4D1 * t15 * t18)
      t62 = t61 * 0.3141592653589793D1
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t77 = t61 ** 2
      t85 = rrgg2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t89 = (0.90D2 * t7 * t8 * (t9 - t22 * t23) - t33) * t35 / 0.720D3 
     #+ t7 * t8 * t23 * t39 * t35 / 0.8D1 + (0.90D2 * t7 * t8 * (t9 - t4
     #9 * t23) - t33) * t39 / 0.720D3 + (-0.180D3 * t29 - 0.90D2 * t62) 
     #* t6 * t8 * t9 / 0.720D3 + (0.3141592653589793D1 * (0.180D3 * t69 
     #- 0.30D2 * t71) + 0.180D3 * t62 * lh + 0.45D2 * t77 * 0.3141592653
     #589793D1) * t6 * t8 * t23 / 0.720D3 + t7 * t8 * t85 / 0.8D1
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
      t111 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t115 = x4 * t99
      t116 = t102 ** 2
      t121 = log(-0.4D1 * t16 * t12 * t14 * t115 * t116 * t17)
      t123 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
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
      t167 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t168 = t166 * t167
      t173 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
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
      t221 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t225 = t7 * t8 * t203 * t220 * t221 * t139 / 0.8D1
      t226 = FJET(XB1, XB2, s, t199, -t200, -t201, t202, t216, -t225)
      t232 = t203 * t220 * t221 * t39 * t35
      t235 = FJET(XB1, XB2, s, -t156, t153, -t157, 0.0D0, -t162, t194)
      t237 = FJET(XB1, XB2, s, -t200, t199, t202, -t201, t216, -t225)
      rrgg2gght4s1e0 = t90 * t89 + t144 * t143 + t146 * t89 + t148 * t14
     #3 + t195 * t194 - t226 * 0.3141592653589793D1 * t30 * t232 / 0.8D1
     # + t235 * t194 - t237 * 0.3141592653589793D1 * t30 * t232 / 0.8D1

      end function



      doubleprecision function rrgg2gght4s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh41J1
      doubleprecision rrgg2ggh41J2
      doubleprecision rrgg2ggh41J3
      doubleprecision rrgg2ggh41J4
      doubleprecision rrgg2ggh41J5
      doubleprecision rrgg2ggh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D0
     #)
      t10 = t8 * t9
      t11 = 0.1D1 / x3
      t15 = rrgg2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t27 = x1 ** 2
      t28 = t4 ** 2
      t32 = log(0.4D1 * t23 / t24 * t27 * t28)
      t39 = 0.1D1 / x4
      t43 = t7 * t10 * t11 / 0.8D1 + t7 * t8 * t15 / 0.8D1 + (-0.180D3 *
     # 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.3141592653589793D1) 
     #* t6 * t10 / 0.720D3 + t7 * t10 * t39 / 0.8D1
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
      t62 = t8 * t7
      t66 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
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
      t100 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t102 = t92 / (-0.2D1 + t84) * t100 * t11
      t104 = t62 * t102 / 0.8D1
      t105 = FJET(XB1, XB2, s, t87, -t90, 0.0D0, -t91, -t96, t104)
      t110 = FJET(XB1, XB2, s, -t90, t87, -t91, 0.0D0, -t96, t104)
      rrgg2gght4s1em1 = t44 * t43 + t71 * 0.3141592653589793D1 * t73 * t
     #68 / 0.8D1 + t77 * t43 + t79 * 0.3141592653589793D1 * t73 * t68 / 
     #0.8D1 + t105 * 0.3141592653589793D1 * t73 * t102 / 0.8D1 + t110 * 
     #0.3141592653589793D1 * t73 * t102 / 0.8D1

      end function



      doubleprecision function rrgg2gght4s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh41J1
      doubleprecision rrgg2ggh41J2
      doubleprecision rrgg2ggh41J3
      doubleprecision rrgg2ggh41J4
      doubleprecision rrgg2ggh41J5
      doubleprecision rrgg2ggh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D0
     #)
      t12 = 0.3141592653589793D1 * t6 * t8 * t9 / 0.8D1
      t13 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t12)
      rrgg2gght4s1em2 = t13 * 0.3141592653589793D1 * t16 / 0.8D1 + t18 *
     # 0.3141592653589793D1 * t16 / 0.8D1

      end function



      doubleprecision function rrgg2gght4s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh41J1
      doubleprecision rrgg2ggh41J2
      doubleprecision rrgg2ggh41J3
      doubleprecision rrgg2ggh41J4
      doubleprecision rrgg2ggh41J5
      doubleprecision rrgg2ggh41J6
      rrgg2gght4s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght4s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh41J1
      doubleprecision rrgg2ggh41J2
      doubleprecision rrgg2ggh41J3
      doubleprecision rrgg2ggh41J4
      doubleprecision rrgg2ggh41J5
      doubleprecision rrgg2ggh41J6
      rrgg2gght4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh41J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t12 = t10 * t11
      t13 = 0.1D1 - x1
      t14 = t3 * t13
      t17 = 0.1D1 - x4
      t20 = s - t2 * t14 * x4 - t2 * t14 * t17
      t21 = t12 * t20
      t23 = t11 * s
      t24 = t23 * z
      t25 = t24 * t10
      t26 = t20 * t23
      t27 = z * t26
      t28 = t1 ** 2
      t29 = t28 ** 2
      t30 = t3 ** 2
      t31 = t30 ** 2
      t34 = x1 ** 2
      t35 = t13 ** 2
      t36 = t34 * t35
      t40 = cos(x2 * 0.3141592653589793D1)
      t44 = Sqrt(x3 * t7 * x4 * t17)
      t47 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t40 * t44
      t48 = t47 ** 2
      t54 = t24 * t10 * t28
      t55 = t30 * x1
      t57 = t55 * t13 * t47
      t61 = t26 * z * t28
      t64 = t11 ** 2
      t65 = t29 ** 2
      t67 = t31 ** 2
      t69 = t34 ** 2
      t70 = t35 ** 2
      t72 = t48 ** 2
      t98 = t28 * t1
      t100 = t24 * t10 * t98
      t101 = t30 * t3
      t103 = t7 * t13
      t105 = t101 * t34 * t103 * t47
      t119 = t101 * t35 * t17 * x1 * t47
      t130 = t31 * t34 * t35 * t48
      t133 = t10 * t20
      t135 = t133 * t11 * t98
      t140 = -0.4D1 * t21 - t25 - t27 - 0.4D1 * t26 * t29 * t31 * t36 * 
     #t48 * z + 0.3D1 * t54 * t57 + 0.3D1 * t57 * t61 - 0.2D1 * t64 * t6
     #5 * t67 * t69 * t70 * t72 - 0.6D1 * t64 * t29 * t31 * t36 * t48 + 
     #0.4D1 * t64 * t28 * t30 * x1 * t13 * t47 + 0.4D1 * t64 * t29 * t28
     # * t31 * t30 * t34 * x1 * t35 * t13 * t48 * t47 + 0.2D1 * t100 * t
     #105 + 0.3D1 * t26 * t98 * t101 * t34 * t7 * z * t13 * t47 + 0.3D1 
     #* t100 * t119 + 0.2D1 * t26 * z * t98 * t119 - 0.4D1 * t24 * t10 *
     # t29 * t130 + 0.14D2 * t135 * t105 + 0.14D2 * t135 * t119
      t150 = t55 * t103 * t17
      t153 = t28 * t30
      t154 = t17 ** 2
      t156 = t153 * t35 * t154
      t159 = t7 ** 2
      t161 = t153 * t34 * t159
      t170 = t1 * t3
      t172 = t170 * x1 * t7
      t175 = t13 * t17
      t176 = t170 * t175
      t195 = -0.2D1 * t133 * t11 * t29 * t130 - 0.20D2 * t133 * t11 * t2
     #8 * t57 + t54 * t150 - 0.2D1 * t64 - 0.4D1 * t21 * t156 - 0.4D1 * 
     #t21 * t161 - 0.2D1 * t25 * t161 - t25 * t156 - 0.2D1 * t27 * t156 
     #- t27 * t161 + 0.16D2 * t21 * t172 + 0.16D2 * t21 * t176 + t25 * t
     #172 - 0.2D1 * t26 * t1 * t4 * t7 * z - 0.2D1 * t25 * t176 + t27 * 
     #t176 - 0.12D2 * t12 * t153 * t175 * t20 * x1 * t7 + t61 * t150
      rrgg2ggh41J1 = -0.9D1 / 0.2D1 * wd * (t140 + t195) / t10 / t20 / s
     # / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh41J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t5 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t11 = t10 ** 2
      t13 = x1 ** 2
      t14 = t13 ** 2
      t15 = 0.1D1 - x1
      t16 = t15 ** 2
      t17 = t16 ** 2
      t22 = cos(x2 * 0.3141592653589793D1)
      t23 = 0.1D1 - x3
      t25 = 0.1D1 - x4
      t28 = Sqrt(x3 * t23 * x4 * t25)
      t31 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t28
      t32 = t31 ** 2
      t33 = t32 ** 2
      t39 = t13 * t16
      t60 = s * t3
      t61 = t8 * x1
      t66 = s - t60 * t61 * x3 - t60 * t61 * t23
      t67 = t66 * t1
      t68 = t8 * t15
      t73 = s - t60 * t68 * x4 - t60 * t68 * t25
      t74 = t67 * t73
      t76 = s * t1
      t77 = t76 * z
      t78 = t77 * t66
      t79 = t73 * t76
      t80 = t79 * z
      t82 = t4 * t9
      t83 = t25 ** 2
      t85 = t82 * t16 * t83
      t88 = t23 ** 2
      t90 = t82 * t13 * t88
      t99 = t3 * t8
      t101 = t99 * x1 * t23
      t104 = t15 * t25
      t105 = t99 * t104
      t109 = -0.2D1 * t2 * t6 * t11 * t14 * t17 * t33 - 0.6D1 * t2 * t5 
     #* t10 * t39 * t32 + 0.4D1 * t2 * t4 * t9 * x1 * t15 * t31 + 0.4D1 
     #* t2 * t5 * t4 * t10 * t9 * t13 * x1 * t16 * t15 * t32 * t31 - 0.4
     #D1 * t74 - t78 - t80 - 0.2D1 * t2 - 0.4D1 * t74 * t85 - 0.4D1 * t7
     #4 * t90 - 0.2D1 * t78 * t90 - t78 * t85 - 0.2D1 * t80 * t85 - t80 
     #* t90 + 0.16D2 * t74 * t101 + 0.16D2 * t74 * t105 + t78 * t101
      t114 = 0.2D1 * t79 * t3 * t61 * t23 * z
      t116 = 0.2D1 * t78 * t105
      t117 = t80 * t105
      t118 = t3 * t4
      t120 = t77 * t66 * t118
      t121 = t8 * t9
      t123 = t23 * t15
      t125 = t121 * t13 * t123 * t31
      t127 = 0.2D1 * t120 * t125
      t134 = t79 * t118 * t121 * t13 * t23 * z * t15 * t31
      t139 = t121 * t16 * t25 * x1 * t31
      t140 = t120 * t139
      t145 = 0.2D1 * t79 * z * t118 * t139
      t150 = t10 * t13 * t16 * t32
      t152 = 0.4D1 * t77 * t66 * t5 * t150
      t158 = 0.4D1 * t79 * t5 * t10 * t39 * t32 * z
      t160 = t77 * t66 * t4
      t161 = t9 * x1
      t163 = t161 * t15 * t31
      t164 = t160 * t163
      t167 = t79 * z * t4
      t168 = t167 * t163
      t170 = t66 * t73
      t173 = t170 * t1 * t5 * t150
      t177 = t170 * t1 * t4 * t163
      t180 = t161 * t123 * t25
      t181 = t160 * t180
      t182 = t167 * t180
      t188 = 0.12D2 * t67 * t82 * t104 * t73 * x1 * t23
      t190 = t170 * t1 * t118
      t191 = t190 * t125
      t193 = t190 * t139
      t195 = -t114 - t116 + t117 + t127 + 0.3D1 * t134 + 0.3D1 * t140 + 
     #t145 - t152 - t158 + 0.3D1 * t164 + 0.3D1 * t168 - 0.2D1 * t173 - 
     #0.20D2 * t177 + t181 + t182 - t188 + 0.14D2 * t191 + 0.14D2 * t193
      t208 = t114 + t116 - t117 - t127 - 0.6D1 * t134 - 0.6D1 * t140 - t
     #145 + t152 + t158 - 0.6D1 * t164 - 0.6D1 * t168 + 0.4D1 * t173 + 0
     #.22D2 * t177 - t181 - t182 + t188 - 0.16D2 * t191 - 0.16D2 * t193
      rrgg2ggh41J2 = -0.9D1 / 0.2D1 * (0.2D1 * wd * (t109 + t195) + wd *
     # (-t109 + t208)) / t66 / t73 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh41J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 * s
      t14 = t11 * t13
      t15 = t14 * z
      t16 = t1 * t3
      t17 = t4 * t8
      t18 = t16 * t17
      t19 = t15 * t18
      t20 = t13 * z
      t21 = t3 * x1
      t24 = 0.1D1 - x3
      t27 = s - t2 * t21 * x3 - t2 * t21 * t24
      t28 = t20 * t27
      t30 = 0.2D1 * t28 * t18
      t35 = 0.2D1 * t14 * t1 * t21 * t24 * z
      t37 = t16 * x1 * t24
      t38 = t28 * t37
      t39 = t27 * t12
      t40 = t39 * t11
      t42 = 0.16D2 * t40 * t18
      t44 = 0.16D2 * t40 * t37
      t45 = t1 ** 2
      t46 = t3 ** 2
      t47 = t45 * t46
      t48 = x1 ** 2
      t49 = t24 ** 2
      t51 = t47 * t48 * t49
      t52 = t15 * t51
      t53 = t4 ** 2
      t54 = t8 ** 2
      t56 = t47 * t53 * t54
      t58 = 0.2D1 * t15 * t56
      t59 = t28 * t56
      t61 = 0.2D1 * t28 * t51
      t63 = 0.4D1 * t40 * t51
      t65 = 0.4D1 * t40 * t56
      t67 = t20 * t27 * t45
      t68 = t46 * x1
      t72 = cos(x2 * 0.3141592653589793D1)
      t76 = Sqrt(x3 * t24 * x4 * t8)
      t79 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t72 * t76
      t81 = t68 * t4 * t79
      t82 = t67 * t81
      t85 = t14 * z * t45
      t86 = t85 * t81
      t88 = 0.4D1 * t40
      t89 = t19 - t30 - t35 + t38 + t42 + t44 - t52 - t58 - t59 - t61 - 
     #t63 - t65 + 0.3D1 * t82 + 0.3D1 * t86 - t88 - t28 - t15
      t90 = t45 * t1
      t93 = t46 * t3
      t97 = t93 * t53 * t8 * x1 * t79
      t99 = 0.2D1 * t14 * z * t90 * t97
      t100 = t45 ** 2
      t103 = t46 ** 2
      t105 = t79 ** 2
      t107 = t103 * t48 * t53 * t105
      t109 = 0.4D1 * t20 * t27 * t100 * t107
      t112 = t48 * t53
      t116 = 0.4D1 * t14 * t100 * t103 * t112 * t105 * z
      t117 = t24 * t4
      t119 = t68 * t117 * t8
      t120 = t85 * t119
      t121 = t27 * t11
      t124 = t121 * t12 * t100 * t107
      t128 = t121 * t12 * t45 * t81
      t130 = t67 * t119
      t132 = t121 * t12 * t90
      t135 = t93 * t48 * t117 * t79
      t136 = t132 * t135
      t138 = t132 * t97
      t140 = t12 ** 2
      t141 = 0.2D1 * t140
      t143 = t20 * t27 * t90
      t145 = 0.2D1 * t143 * t135
      t152 = t14 * t90 * t93 * t48 * t24 * z * t4 * t79
      t154 = t143 * t97
      t161 = 0.12D2 * t39 * t47 * t17 * t11 * x1 * t24
      t162 = t100 ** 2
      t164 = t103 ** 2
      t166 = t48 ** 2
      t167 = t53 ** 2
      t169 = t105 ** 2
      t172 = 0.2D1 * t140 * t162 * t164 * t166 * t167 * t169
      t177 = 0.6D1 * t140 * t100 * t103 * t112 * t105
      t183 = 0.4D1 * t140 * t45 * t46 * x1 * t4 * t79
      t194 = 0.4D1 * t140 * t100 * t45 * t103 * t46 * t48 * x1 * t53 * t
     #4 * t105 * t79
      t195 = t99 - t109 - t116 + t120 - 0.2D1 * t124 - 0.20D2 * t128 + t
     #130 + 0.14D2 * t136 + 0.14D2 * t138 - t141 + t145 + 0.3D1 * t152 +
     # 0.3D1 * t154 - t161 - t172 - t177 + t183 + t194
      t201 = -t19 + t30 + t35 - t38 - t42 - t44 + t52 + t58 + t59 + t61 
     #+ t63 + t65 - 0.6D1 * t82 - 0.6D1 * t86 + t88 + t28 + t15
      t208 = -t99 + t109 + t116 - t120 + 0.4D1 * t124 + 0.22D2 * t128 - 
     #t130 - 0.16D2 * t136 - 0.16D2 * t138 + t141 - t145 - 0.6D1 * t152 
     #- 0.6D1 * t154 + t161 + t172 + t177 - t183 - t194
      rrgg2ggh41J3 = -0.9D1 / 0.2D1 * (0.3D1 * wd * (t89 + t195) + 0.2D1
     # * wd * (t201 + t208)) / t27 / t11 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh41J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = t2 * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = s * t4
      t6 = 0.1D1 - z
      t7 = t6 * x1
      t10 = 0.1D1 - x3
      t13 = s - t5 * t7 * x3 - t5 * t7 * t10
      t14 = t4 ** 2
      t15 = t14 * t4
      t17 = t3 * t13 * t15
      t18 = t6 ** 2
      t19 = t18 * t6
      t20 = x1 ** 2
      t22 = 0.1D1 - x1
      t23 = t10 * t22
      t27 = cos(x2 * 0.3141592653589793D1)
      t29 = 0.1D1 - x4
      t32 = Sqrt(x3 * t10 * x4 * t29)
      t35 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t27 * t32
      t37 = t19 * t20 * t23 * t35
      t39 = 0.2D1 * t17 * t37
      t40 = t6 * t22
      t45 = s - t5 * t40 * x4 - t5 * t40 * t29
      t46 = t45 * t2
      t53 = t46 * t15 * t19 * t20 * t10 * z * t22 * t35
      t55 = t13 * t1
      t56 = t14 * t18
      t58 = t22 * t29
      t63 = 0.12D2 * t55 * t56 * t58 * t45 * x1 * t10
      t64 = t13 * t45
      t66 = t64 * t1 * t15
      t67 = t66 * t37
      t69 = t22 ** 2
      t73 = t19 * t69 * t29 * x1 * t35
      t74 = t66 * t73
      t76 = t1 ** 2
      t77 = t14 ** 2
      t78 = t77 ** 2
      t80 = t18 ** 2
      t81 = t80 ** 2
      t83 = t20 ** 2
      t84 = t69 ** 2
      t86 = t35 ** 2
      t87 = t86 ** 2
      t90 = 0.2D1 * t76 * t78 * t81 * t83 * t84 * t87
      t93 = t20 * t69
      t96 = 0.6D1 * t76 * t77 * t80 * t93 * t86
      t102 = 0.4D1 * t76 * t14 * t18 * x1 * t22 * t35
      t113 = 0.4D1 * t76 * t77 * t14 * t80 * t18 * t20 * x1 * t69 * t22 
     #* t86 * t35
      t115 = t46 * z * t14
      t116 = t18 * x1
      t118 = t116 * t22 * t35
      t119 = t115 * t118
      t124 = 0.2D1 * t46 * z * t15 * t73
      t129 = t80 * t20 * t69 * t86
      t131 = 0.4D1 * t3 * t13 * t77 * t129
      t137 = 0.4D1 * t46 * t77 * t80 * t93 * t86 * z
      t138 = t55 * t45
      t139 = t29 ** 2
      t141 = t56 * t69 * t139
      t143 = 0.4D1 * t138 * t141
      t144 = t4 * t6
      t146 = t144 * x1 * t10
      t148 = 0.16D2 * t138 * t146
      t149 = t46 * z
      t150 = t10 ** 2
      t152 = t56 * t20 * t150
      t153 = t149 * t152
      t155 = 0.2D1 * t149 * t141
      t156 = t39 + 0.3D1 * t53 - t63 + 0.14D2 * t67 + 0.14D2 * t74 - t90
     # - t96 + t102 + t113 + 0.3D1 * t119 + t124 - t131 - t137 - t143 + 
     #t148 - t153 - t155
      t157 = t3 * t13
      t158 = t157 * t141
      t160 = 0.2D1 * t157 * t152
      t162 = 0.4D1 * t138 * t152
      t163 = t144 * t58
      t165 = 0.16D2 * t138 * t163
      t170 = 0.2D1 * t46 * t4 * t7 * t10 * z
      t171 = t157 * t146
      t173 = 0.2D1 * t157 * t163
      t174 = t149 * t163
      t176 = t3 * t13 * t14
      t177 = t176 * t118
      t179 = 0.4D1 * t138
      t180 = t17 * t73
      t184 = t64 * t1 * t77 * t129
      t188 = t64 * t1 * t14 * t118
      t191 = t116 * t23 * t29
      t192 = t176 * t191
      t193 = t115 * t191
      t194 = 0.2D1 * t76
      t195 = -t158 - t160 - t162 + t165 - t170 + t171 - t173 + t174 + 0.
     #3D1 * t177 - t179 - t157 - t149 + 0.3D1 * t180 - 0.2D1 * t184 - 0.
     #20D2 * t188 + t192 + t193 - t194
      t203 = -t39 - 0.6D1 * t53 + t63 - 0.16D2 * t67 - 0.16D2 * t74 + t9
     #0 + t96 - t102 - t113 - 0.6D1 * t119 - t124 + t131 + t137 + t143 -
     # t148 + t153 + t155
      t208 = t158 + t160 + t162 - t165 + t170 - t171 + t173 - t174 - 0.6
     #D1 * t177 + t179 + t157 + t149 - 0.6D1 * t180 + 0.4D1 * t184 + 0.2
     #2D2 * t188 - t192 - t193 + t194
      rrgg2ggh41J4 = -0.9D1 / 0.2D1 * (0.4D1 * wd * (t156 + t195) + 0.3D
     #1 * wd * (t203 + t208)) / t13 / t45 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh41J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = 0.1D1 - x1
      t12 = t3 * t11
      t15 = 0.1D1 - x4
      t18 = s - t2 * t12 * x4 - t2 * t12 * t15
      t19 = t10 * t18
      t20 = s ** 2
      t21 = t1 ** 2
      t22 = t21 ** 2
      t25 = t3 ** 2
      t26 = t25 ** 2
      t27 = x1 ** 2
      t29 = t11 ** 2
      t33 = cos(x2 * 0.3141592653589793D1)
      t37 = Sqrt(x3 * t7 * x4 * t15)
      t40 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t33 * t37
      t41 = t40 ** 2
      t43 = t26 * t27 * t29 * t41
      t44 = t19 * t20 * t22 * t43
      t48 = t25 * x1
      t50 = t48 * t11 * t40
      t51 = t19 * t20 * t21 * t50
      t53 = t20 * s
      t54 = t53 * z
      t56 = t54 * t10 * t21
      t57 = t7 * t11
      t59 = t48 * t57 * t15
      t60 = t56 * t59
      t61 = t18 * t53
      t63 = t61 * z * t21
      t64 = t63 * t59
      t65 = t21 * t1
      t67 = t54 * t10 * t65
      t68 = t25 * t3
      t71 = t68 * t27 * t57 * t40
      t73 = 0.2D1 * t67 * t71
      t74 = t10 * t20
      t75 = t74 * t18
      t76 = t21 * t25
      t77 = t7 ** 2
      t79 = t76 * t27 * t77
      t81 = 0.4D1 * t75 * t79
      t82 = t54 * t10
      t84 = 0.2D1 * t82 * t79
      t85 = t15 ** 2
      t87 = t76 * t29 * t85
      t88 = t82 * t87
      t89 = t61 * z
      t91 = 0.2D1 * t89 * t87
      t92 = t89 * t79
      t93 = t1 * t3
      t95 = t93 * x1 * t7
      t97 = 0.16D2 * t75 * t95
      t98 = t11 * t15
      t99 = t93 * t98
      t101 = 0.16D2 * t75 * t99
      t102 = t82 * t95
      t107 = 0.2D1 * t61 * t1 * t4 * t7 * z
      t109 = 0.2D1 * t82 * t99
      t110 = t89 * t99
      t111 = t20 ** 2
      t112 = 0.2D1 * t111
      t113 = -0.2D1 * t44 - 0.20D2 * t51 + t60 + t64 + t73 - t81 - t84 -
     # t88 - t91 - t92 + t97 + t101 + t102 - t107 - t109 + t110 - t112
      t119 = t68 * t29 * t15 * x1 * t40
      t121 = 0.2D1 * t61 * z * t65 * t119
      t122 = 0.4D1 * t75
      t126 = 0.4D1 * t54 * t10 * t22 * t43
      t129 = t27 * t29
      t133 = 0.4D1 * t61 * t22 * t26 * t129 * t41 * z
      t134 = t56 * t50
      t136 = t67 * t119
      t139 = 0.4D1 * t75 * t87
      t141 = t19 * t20 * t65
      t142 = t141 * t119
      t144 = t63 * t50
      t146 = t22 ** 2
      t148 = t26 ** 2
      t150 = t27 ** 2
      t151 = t29 ** 2
      t153 = t41 ** 2
      t156 = 0.2D1 * t111 * t146 * t148 * t150 * t151 * t153
      t161 = 0.6D1 * t111 * t22 * t26 * t129 * t41
      t167 = 0.4D1 * t111 * t21 * t25 * x1 * t11 * t40
      t178 = 0.4D1 * t111 * t22 * t21 * t26 * t25 * t27 * x1 * t29 * t11
     # * t41 * t40
      t184 = 0.12D2 * t74 * t76 * t98 * t18 * x1 * t7
      t185 = t141 * t71
      t193 = t61 * t65 * t68 * t27 * t7 * z * t11 * t40
      t195 = t121 - t122 - t82 - t89 - t126 - t133 + 0.3D1 * t134 + 0.3D
     #1 * t136 - t139 + 0.14D2 * t142 + 0.3D1 * t144 - t156 - t161 + t16
     #7 + t178 - t184 + 0.14D2 * t185 + 0.3D1 * t193
      t201 = 0.4D1 * t44 + 0.22D2 * t51 - t60 - t64 - t73 + t81 + t84 + 
     #t88 + t91 + t92 - t97 - t101 - t102 + t107 + t109 - t110 + t112
      t208 = -t121 + t122 + t82 + t89 + t126 + t133 - 0.6D1 * t134 - 0.6
     #D1 * t136 + t139 - 0.16D2 * t142 - 0.6D1 * t144 + t156 + t161 - t1
     #67 - t178 + t184 - 0.16D2 * t185 - 0.6D1 * t193
      rrgg2ggh41J5 = -0.9D1 / 0.2D1 * (0.5D1 * wd * (t113 + t195) + 0.4D
     #1 * wd * (t201 + t208)) / t10 / t18 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh41J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 * s
      t14 = t11 * t13
      t15 = t1 ** 2
      t17 = t14 * z * t15
      t18 = t3 ** 2
      t19 = t18 * x1
      t20 = 0.1D1 - x3
      t21 = t20 * t4
      t23 = t19 * t21 * t8
      t25 = t13 * z
      t26 = t3 * x1
      t31 = s - t2 * t26 * x3 - t2 * t26 * t20
      t32 = t15 * t1
      t34 = t25 * t31 * t32
      t35 = t18 * t3
      t36 = x1 ** 2
      t41 = cos(x2 * 0.3141592653589793D1)
      t45 = Sqrt(x3 * t20 * x4 * t8)
      t48 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t41 * t45
      t50 = t35 * t36 * t21 * t48
      t61 = t4 ** 2
      t65 = t35 * t61 * t8 * x1 * t48
      t73 = t25 * t31 * t15
      t75 = t19 * t4 * t48
      t80 = t14 * z
      t81 = t15 * t18
      t82 = t20 ** 2
      t84 = t81 * t36 * t82
      t86 = t31 * t12
      t87 = t86 * t11
      t88 = t1 * t3
      t90 = t88 * x1 * t20
      t93 = t4 * t8
      t94 = t88 * t93
      t97 = t25 * t31
      t114 = -t17 * t23 - 0.2D1 * t34 * t50 - 0.6D1 * t14 * t32 * t35 * 
     #t36 * t20 * z * t4 * t48 - 0.6D1 * t34 * t65 - 0.2D1 * t14 * z * t
     #32 * t65 - 0.6D1 * t73 * t75 - 0.6D1 * t17 * t75 + t80 * t84 - 0.1
     #6D2 * t87 * t90 - 0.16D2 * t87 * t94 - t97 * t90 + 0.2D1 * t14 * t
     #1 * t26 * t20 * z + 0.2D1 * t97 * t94 - t80 * t94 + 0.12D2 * t86 *
     # t81 * t93 * t11 * x1 * t20 + 0.4D1 * t87 + t97
      t116 = t12 ** 2
      t118 = t8 ** 2
      t120 = t81 * t61 * t118
      t130 = t15 ** 2
      t133 = t18 ** 2
      t135 = t48 ** 2
      t137 = t133 * t36 * t61 * t135
      t140 = t130 ** 2
      t142 = t133 ** 2
      t144 = t36 ** 2
      t145 = t61 ** 2
      t147 = t135 ** 2
      t153 = t36 * t61
      t180 = t31 * t11
      t182 = t180 * t12 * t32
      t195 = t80 - t73 * t23 + 0.2D1 * t116 + 0.4D1 * t87 * t120 + 0.4D1
     # * t87 * t84 + 0.2D1 * t97 * t84 + t97 * t120 + 0.2D1 * t80 * t120
     # + 0.4D1 * t25 * t31 * t130 * t137 + 0.2D1 * t116 * t140 * t142 * 
     #t144 * t145 * t147 + 0.6D1 * t116 * t130 * t133 * t153 * t135 - 0.
     #4D1 * t116 * t15 * t18 * x1 * t4 * t48 - 0.4D1 * t116 * t130 * t15
     # * t133 * t18 * t36 * x1 * t61 * t4 * t135 * t48 + 0.4D1 * t14 * t
     #130 * t133 * t153 * t135 * z - 0.16D2 * t182 * t50 - 0.16D2 * t182
     # * t65 + 0.4D1 * t180 * t12 * t130 * t137 + 0.22D2 * t180 * t12 * 
     #t15 * t75
      rrgg2ggh41J6 = -0.45D2 / 0.2D1 * wd * (t114 + t195) / t31 / t11 / 
     #s / z / 0.3141592653589793D1

      end function
  
 