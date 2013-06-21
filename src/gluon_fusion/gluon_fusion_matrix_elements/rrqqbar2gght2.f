  
      subroutine rrqqbar2gght2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh21J1  
      doubleprecision rrqqbar2ggh21J2  
      doubleprecision rrqqbar2ggh21J3  
      doubleprecision rrqqbar2gght2s1e1  
      doubleprecision rrqqbar2gght2s1e0  
      doubleprecision rrqqbar2gght2s1em1  
      doubleprecision rrqqbar2gght2s1em2  
      doubleprecision rrqqbar2gght2s1em3  
      doubleprecision rrqqbar2gght2s1em4  
      doubleprecision rrqqbar2gght2s2e1  
      doubleprecision rrqqbar2gght2s2e0  
      doubleprecision rrqqbar2gght2s2em1  
      doubleprecision rrqqbar2gght2s2em2  
      doubleprecision rrqqbar2gght2s2em3  
      doubleprecision rrqqbar2gght2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = 0.1D1 - x3
      t9 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4
     #)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -t8
      t22 = t19 * t4 * t20
      t25 = log(0.4D1 * t16 * t22)
      t26 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x
     #4)
      t28 = t25 ** 2
      t29 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x
     #4)
      t32 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t33 = t19 * t4
      t36 = log(-0.4D1 * t16 * t33)
      t37 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t39 = t36 ** 2
      t40 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t47 = 0.3141592653589793D1 * lh
      t48 = t1 * t7
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = 0.180D3 * t55 - 0.30D2 * t57
      t60 = 0.3141592653589793D1 * t59
      t61 = t29 - t40
      t65 = 0.1D1 / x3
      t69 = t12 * t15
      t72 = log(-0.4D1 * t69 * t33)
      t73 = t72 * 0.3141592653589793D1
      t82 = t72 ** 2
      t83 = t82 * 0.3141592653589793D1
      t107 = x1 ** 2
      t108 = x3 * t107
      t109 = t108 * t69
      t112 = log(0.4D1 * t109 * t22)
      t115 = t15 * t18
      t116 = x4 * t4
      t120 = log(-0.4D1 * t108 * t12 * t115 * t116)
      t132 = 0.1D1 / x1
      t135 = t107 * t12
      t139 = log(-0.4D1 * t135 * t15 * t33)
      t141 = t139 ** 2
      t158 = (0.90D2 * t6 * t7 * (t9 - t25 * t26 + t28 * t29 / 0.2D1 - t
     #32 + t36 * t37 - t39 * t40 / 0.2D1) - 0.180D3 * t47 * t48 * (t26 -
     # t25 * t29 - t37 + t36 * t40) + t60 * t48 * t61) * t65 / 0.1440D4 
     #- (-0.180D3 * t47 - 0.90D2 * t73) * t1 * t7 * t32 / 0.1440D4 - (t6
     #0 + 0.180D3 * t73 * lh + 0.45D2 * t83) * t1 * t7 * t37 / 0.1440D4 
     #- (0.3141592653589793D1 * (0.60D2 * lh * t57 - 0.2884936567583026D
     #3 - 0.120D3 * t55 * lh) - t73 * t59 - 0.90D2 * t83 * lh - 0.15D2 *
     # t82 * t72 * 0.3141592653589793D1) * t1 * t7 * t40 / 0.1440D4 - (0
     #.90D2 * t6 * t7 * (-t26 + t112 * t29 + t37 - t120 * t40) + 0.180D3
     # * t47 * t48 * t61) * t65 * t132 / 0.720D3 + (0.90D2 * t6 * t7 * (
     #-t32 + t139 * t37 - t141 * t40 / 0.2D1) - 0.180D3 * t47 * t48 * (-
     #t37 + t139 * t40) - t60 * t48 * t40) * t132 / 0.720D3
      t159 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t158)
      t161 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t158)
      t163 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t164 = s * t163
      t165 = t1 * x1
      t166 = t164 * t165
      t167 = -0.1D1 + x1
      t168 = t1 * t167
      t169 = t168 * x4
      t170 = t164 * t169
      t171 = t168 * t4
      t172 = t164 * t171
      t173 = t163 ** 2
      t176 = x1 * t167
      t178 = s * t173 * t17 * t176 * t4
      t179 = t167 * t173
      t181 = 0.1D1 / (-0.2D1 + t163)
      t182 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t184 = t179 * t181 * t182
      t185 = t167 ** 2
      t187 = t173 ** 2
      t192 = log(-0.4D1 * t109 * t19 * t4 * t185 * t187)
      t194 = t173 * t181
      t195 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t196 = t194 * t195
      t202 = t47 * t48
      t204 = t179 * t181 * t195
      t210 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t218 = log(-0.4D1 * t135 * t115 * t116 * t187 * t185)
      t219 = t218 * t167
      t222 = t218 ** 2
      t240 = -(0.90D2 * t6 * t7 * (-t184 + t192 * t167 * t196) + 0.180D3
     # * t202 * t204) * t65 * t132 / 0.720D3 + (0.90D2 * t6 * t7 * (t179
     # * t181 * t210 - t219 * t194 * t182 + t222 * t167 * t196 / 0.2D1) 
     #- 0.180D3 * t47 * t48 * (t184 - t219 * t196) + t60 * t48 * t204) *
     # t132 / 0.720D3
      t241 = FJET(XB1, XB2, s, t166, 0.0D0, -t170, t172, t178, t240)
      t243 = FJET(XB1, XB2, s, -t170, t172, t166, 0.0D0, t178, t240)
      t245 = KAPPA2(x1, x2, t8, x4, z)
      t246 = s * t245
      t248 = t246 * t165 * t20
      t250 = t246 * t165 * x3
      t251 = t246 * t169
      t252 = t246 * t171
      t253 = t245 ** 2
      t258 = cos(t10)
      t261 = Sqrt(x3 * t20 * t116)
      t266 = s * t253 * t17 * t176 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t258 * t261)
      t267 = t167 * t253
      t269 = 0.1D1 / (-0.2D1 + t245)
      t270 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, x4)
      t276 = t253 ** 2
      t281 = log(0.4D1 * t108 * t69 * t18 * t116 * t185 * t20 * t276)
      t284 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, x4)
      t295 = 0.90D2 * t6 * t7 * (t267 * t269 * t270 - t281 * t167 * t253
     # * t269 * t284) - 0.180D3 * t202 * t267 * t269 * t284
      t298 = t295 * t65 * t132 / 0.720D3
      t299 = FJET(XB1, XB2, s, -t248, t250, -t251, t252, t266, -t298)
      t301 = t65 * t132
      t304 = FJET(XB1, XB2, s, -t251, t252, -t248, t250, t266, -t298)
      rrqqbar2gght2s1e1 = t159 * t158 + t161 * t158 + t241 * t240 + t243
     # * t240 - t299 * t295 * t301 / 0.720D3 - t304 * t295 * t301 / 0.72
     #0D3

      end function



      doubleprecision function rrqqbar2gght2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = 0.1D1 - x3
      t9 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x4
     #)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -t8
      t25 = log(0.4D1 * t16 * t19 * t4 * t20)
      t26 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t8, x
     #4)
      t28 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t29 = t19 * t4
      t32 = log(-0.4D1 * t16 * t29)
      t33 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t39 = 0.3141592653589793D1 * lh
      t40 = t1 * t7
      t41 = t26 - t33
      t46 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t56 = x1 ** 2
      t57 = t56 * t12
      t61 = log(-0.4D1 * t57 * t15 * t29)
      t73 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t81 = log(-0.4D1 * t12 * t15 * t29)
      t82 = t81 * 0.3141592653589793D1
      t89 = lh ** 2
      t91 = 0.3141592653589793D1 ** 2
      t97 = t81 ** 2
      t105 = (0.90D2 * t6 * t7 * (t9 - t25 * t26 - t28 + t32 * t33) - 0.
     #180D3 * t39 * t40 * t41) * t46 / 0.1440D4 + t6 * t7 * t41 * t46 * 
     #t52 / 0.8D1 + (0.90D2 * t6 * t7 * (-t28 + t61 * t33) + 0.180D3 * t
     #39 * t40 * t33) * t52 / 0.720D3 - t6 * t7 * t73 / 0.16D2 - (-0.180
     #D3 * t39 - 0.90D2 * t82) * t1 * t7 * t28 / 0.1440D4 - (0.314159265
     #3589793D1 * (0.180D3 * t89 - 0.30D2 * t91) + 0.180D3 * t82 * lh + 
     #0.45D2 * t97 * 0.3141592653589793D1) * t1 * t7 * t33 / 0.1440D4
      t106 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t105)
      t108 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t105)
      t110 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t111 = s * t110
      t112 = t1 * x1
      t113 = t111 * t112
      t114 = -0.1D1 + x1
      t115 = t1 * t114
      t116 = t115 * x4
      t117 = t111 * t116
      t118 = t115 * t4
      t119 = t111 * t118
      t120 = t110 ** 2
      t123 = x1 * t114
      t125 = s * t120 * t17 * t123 * t4
      t127 = t6 * t7 * t114
      t129 = 0.1D1 / (-0.2D1 + t110)
      t130 = t120 * t129
      t131 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t137 = t114 * t120
      t138 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t143 = x4 * t4
      t144 = t120 ** 2
      t145 = t114 ** 2
      t150 = log(-0.4D1 * t57 * t15 * t18 * t143 * t144 * t145)
      t166 = t127 * t130 * t131 * t46 * t52 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t137 * t129 * t138 - t150 * t114 * t130 * t131) - 0.180D3 * t39
     # * t40 * t137 * t129 * t131) * t52 / 0.720D3
      t167 = FJET(XB1, XB2, s, t113, 0.0D0, -t117, t119, t125, t166)
      t169 = FJET(XB1, XB2, s, -t117, t119, t113, 0.0D0, t125, t166)
      t171 = KAPPA2(x1, x2, t8, x4, z)
      t172 = s * t171
      t174 = t172 * t112 * t20
      t176 = t172 * t112 * x3
      t177 = t172 * t116
      t178 = t172 * t118
      t179 = t171 ** 2
      t184 = cos(t10)
      t187 = Sqrt(x3 * t20 * t143)
      t192 = s * t179 * t17 * t123 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t184 * t187)
      t196 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t8, x4)
      t199 = t179 / (-0.2D1 + t171) * t196 * t46 * t52
      t201 = t127 * t199 / 0.8D1
      t202 = FJET(XB1, XB2, s, -t174, t176, -t177, t178, t192, -t201)
      t204 = t40 * t114
      t208 = FJET(XB1, XB2, s, -t177, t178, -t174, t176, t192, -t201)
      rrqqbar2gght2s1e0 = t106 * t105 + t108 * t105 + t167 * t166 + t169
     # * t166 - t202 * 0.3141592653589793D1 * t204 * t199 / 0.8D1 - t208
     # * 0.3141592653589793D1 * t204 * t199 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t32 = log(-0.4D1 * t22 / t23 * t27 * x4 * t4)
      t40 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1
     # - x3, x4)
      t47 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.16D2 - (-0.180D3 
     #* 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.3141592653589793D1)
     # * t1 * t9 / 0.1440D4 + t6 * t7 * (t40 - t8) / x3 / 0.16D2
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t47)
      t50 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t47)
      t52 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t53 = s * t52
      t55 = t53 * t1 * x1
      t56 = -0.1D1 + x1
      t57 = t1 * t56
      t59 = t53 * t57 * x4
      t61 = t53 * t57 * t4
      t62 = t52 ** 2
      t67 = s * t62 * t26 * x1 * t56 * t4
      t71 = 0.1D1 / (-0.2D1 + t52)
      t73 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 
     #x4)
      t77 = t6 * t7 * t56 * t62 * t71 * t73 * t10 / 0.8D1
      t78 = FJET(XB1, XB2, s, t55, 0.0D0, -t59, t61, t67, t77)
      t80 = t1 * t7
      t85 = t56 * t62 * t71 * t73 * t10
      t88 = FJET(XB1, XB2, s, -t59, t61, t55, 0.0D0, t67, t77)
      rrqqbar2gght2s1em1 = t47 * t48 + t50 * t47 + t78 * 0.3141592653589
     #793D1 * t80 * t85 / 0.8D1 + t88 * 0.3141592653589793D1 * t80 * t85
     # / 0.8D1

      end function



      doubleprecision function rrqqbar2gght2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t11 = 0.3141592653589793D1 * t1 * t7 * t8 / 0.16D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t11)
      t15 = t1 * t7 * t8
      t17 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrqqbar2gght2s1em2 = -t12 * 0.3141592653589793D1 * t15 / 0.16D2 - 
     #t17 * 0.3141592653589793D1 * t15 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      rrqqbar2gght2s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      rrqqbar2gght2s1em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght2s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = x3 * t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x4
      t19 = -0.1D1 + x3
      t21 = t18 * t4 * t19
      t24 = log(0.4D1 * t15 * t21)
      t25 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t27 = t24 ** 2
      t28 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t31 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t32 = t18 * t4
      t35 = log(-0.4D1 * t15 * t32)
      t36 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t38 = t35 ** 2
      t39 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t46 = 0.3141592653589793D1 * lh
      t47 = t1 * t7
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t58 = 0.180D3 * t54 - 0.30D2 * t56
      t59 = 0.3141592653589793D1 * t58
      t60 = t28 - t39
      t64 = 0.1D1 / x3
      t68 = t11 * t14
      t71 = log(-0.4D1 * t68 * t32)
      t72 = t71 * 0.3141592653589793D1
      t81 = t71 ** 2
      t82 = t81 * 0.3141592653589793D1
      t106 = x1 ** 2
      t107 = x3 * t106
      t109 = t14 * t17
      t110 = x4 * t4
      t114 = log(-0.4D1 * t107 * t11 * t109 * t110)
      t116 = t107 * t68
      t119 = log(0.4D1 * t116 * t21)
      t131 = 0.1D1 / x1
      t134 = t106 * t11
      t138 = log(-0.4D1 * t134 * t14 * t32)
      t140 = t138 ** 2
      t157 = (0.90D2 * t6 * t7 * (t8 - t24 * t25 + t27 * t28 / 0.2D1 - t
     #31 + t35 * t36 - t38 * t39 / 0.2D1) - 0.180D3 * t46 * t47 * (t25 -
     # t24 * t28 - t36 + t35 * t39) + t59 * t47 * t60) * t64 / 0.1440D4 
     #- (-0.180D3 * t46 - 0.90D2 * t72) * t1 * t7 * t31 / 0.1440D4 - (t5
     #9 + 0.180D3 * t72 * lh + 0.45D2 * t82) * t1 * t7 * t36 / 0.1440D4 
     #- (0.3141592653589793D1 * (0.60D2 * lh * t56 - 0.2884936567583026D
     #3 - 0.120D3 * t54 * lh) - t72 * t58 - 0.90D2 * t82 * lh - 0.15D2 *
     # t81 * t71 * 0.3141592653589793D1) * t1 * t7 * t39 / 0.1440D4 - (0
     #.90D2 * t6 * t7 * (t36 - t114 * t39 - t25 + t119 * t28) + 0.180D3 
     #* t46 * t47 * t60) * t64 * t131 / 0.720D3 + (0.90D2 * t6 * t7 * (-
     #t31 + t138 * t36 - t140 * t39 / 0.2D1) - 0.180D3 * t46 * t47 * (-t
     #36 + t138 * t39) - t59 * t47 * t39) * t131 / 0.720D3
      t158 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t157)
      t160 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t161 = s * t160
      t162 = t1 * x1
      t163 = t161 * t162
      t164 = -0.1D1 + x1
      t165 = t1 * t164
      t166 = t165 * x4
      t167 = t161 * t166
      t168 = t165 * t4
      t169 = t161 * t168
      t170 = t160 ** 2
      t173 = x1 * t164
      t175 = s * t170 * t16 * t173 * x4
      t176 = t164 * t170
      t178 = 0.1D1 / (-0.2D1 + t160)
      t179 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t181 = t176 * t178 * t179
      t182 = t164 ** 2
      t184 = t170 ** 2
      t189 = log(-0.4D1 * t116 * t18 * t4 * t182 * t184)
      t191 = t170 * t178
      t192 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t193 = t191 * t192
      t199 = t46 * t47
      t201 = t176 * t178 * t192
      t207 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t215 = log(-0.4D1 * t134 * t109 * t110 * t184 * t182)
      t216 = t215 * t164
      t219 = t215 ** 2
      t237 = -(0.90D2 * t6 * t7 * (-t181 + t189 * t164 * t193) + 0.180D3
     # * t199 * t201) * t64 * t131 / 0.720D3 + (0.90D2 * t6 * t7 * (t176
     # * t178 * t207 - t216 * t191 * t179 + t219 * t164 * t193 / 0.2D1) 
     #- 0.180D3 * t46 * t47 * (t181 - t216 * t193) + t59 * t47 * t201) *
     # t131 / 0.720D3
      t238 = FJET(XB1, XB2, s, 0.0D0, t163, -t167, t169, -t175, t237)
      t240 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t157)
      t242 = KAPPA2(x1, x2, x3, x4, z)
      t243 = s * t242
      t245 = t243 * t162 * x3
      t247 = t243 * t162 * t19
      t248 = t243 * t166
      t249 = t243 * t168
      t250 = t242 ** 2
      t255 = cos(t9)
      t258 = Sqrt(x3 * t19 * t110)
      t263 = s * t250 * t16 * t173 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t255 * t258)
      t264 = t164 * t250
      t266 = 0.1D1 / (-0.2D1 + t242)
      t267 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t273 = t250 ** 2
      t278 = log(0.4D1 * t107 * t68 * t17 * t110 * t182 * t19 * t273)
      t281 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t292 = 0.90D2 * t6 * t7 * (t264 * t266 * t267 - t278 * t164 * t250
     # * t266 * t281) - 0.180D3 * t199 * t264 * t266 * t281
      t295 = t292 * t64 * t131 / 0.720D3
      t296 = FJET(XB1, XB2, s, t245, -t247, -t248, t249, t263, -t295)
      t298 = t64 * t131
      t301 = FJET(XB1, XB2, s, -t167, t169, 0.0D0, t163, -t175, t237)
      t303 = FJET(XB1, XB2, s, -t248, t249, t245, -t247, t263, -t295)
      rrqqbar2gght2s2e1 = t158 * t157 + t238 * t237 + t240 * t157 - t296
     # * t292 * t298 / 0.720D3 + t301 * t237 - t303 * t292 * t298 / 0.72
     #0D3

      end function



      doubleprecision function rrqqbar2gght2s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = x3 * t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x4
      t19 = -0.1D1 + x3
      t24 = log(0.4D1 * t15 * t18 * t4 * t19)
      t25 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t27 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t28 = t18 * t4
      t31 = log(-0.4D1 * t15 * t28)
      t32 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t38 = 0.3141592653589793D1 * lh
      t39 = t1 * t7
      t40 = t25 - t32
      t45 = 0.1D1 / x3
      t51 = 0.1D1 / x1
      t55 = x1 ** 2
      t56 = t55 * t11
      t60 = log(-0.4D1 * t56 * t14 * t28)
      t72 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t80 = log(-0.4D1 * t11 * t14 * t28)
      t81 = t80 * 0.3141592653589793D1
      t88 = lh ** 2
      t90 = 0.3141592653589793D1 ** 2
      t96 = t80 ** 2
      t104 = (0.90D2 * t6 * t7 * (t8 - t24 * t25 - t27 + t31 * t32) - 0.
     #180D3 * t38 * t39 * t40) * t45 / 0.1440D4 + t6 * t7 * t40 * t45 * 
     #t51 / 0.8D1 + (0.90D2 * t6 * t7 * (-t27 + t60 * t32) + 0.180D3 * t
     #38 * t39 * t32) * t51 / 0.720D3 - t6 * t7 * t72 / 0.16D2 - (-0.180
     #D3 * t38 - 0.90D2 * t81) * t1 * t7 * t27 / 0.1440D4 - (0.314159265
     #3589793D1 * (0.180D3 * t88 - 0.30D2 * t90) + 0.180D3 * t81 * lh + 
     #0.45D2 * t96 * 0.3141592653589793D1) * t1 * t7 * t32 / 0.1440D4
      t105 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t104)
      t107 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t108 = s * t107
      t109 = t1 * x1
      t110 = t108 * t109
      t111 = -0.1D1 + x1
      t112 = t1 * t111
      t113 = t112 * x4
      t114 = t108 * t113
      t115 = t112 * t4
      t116 = t108 * t115
      t117 = t107 ** 2
      t120 = x1 * t111
      t122 = s * t117 * t16 * t120 * x4
      t124 = t6 * t7 * t111
      t126 = 0.1D1 / (-0.2D1 + t107)
      t127 = t117 * t126
      t128 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t134 = t111 * t117
      t135 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t140 = x4 * t4
      t141 = t117 ** 2
      t142 = t111 ** 2
      t147 = log(-0.4D1 * t56 * t14 * t17 * t140 * t141 * t142)
      t163 = t124 * t127 * t128 * t45 * t51 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t134 * t126 * t135 - t147 * t111 * t127 * t128) - 0.180D3 * t38
     # * t39 * t134 * t126 * t128) * t51 / 0.720D3
      t164 = FJET(XB1, XB2, s, 0.0D0, t110, -t114, t116, -t122, t163)
      t166 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t104)
      t168 = KAPPA2(x1, x2, x3, x4, z)
      t169 = s * t168
      t171 = t169 * t109 * x3
      t173 = t169 * t109 * t19
      t174 = t169 * t113
      t175 = t169 * t115
      t176 = t168 ** 2
      t181 = cos(t9)
      t184 = Sqrt(x3 * t19 * t140)
      t189 = s * t176 * t16 * t120 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t181 * t184)
      t193 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t196 = t176 / (-0.2D1 + t168) * t193 * t45 * t51
      t198 = t124 * t196 / 0.8D1
      t199 = FJET(XB1, XB2, s, t171, -t173, -t174, t175, t189, -t198)
      t201 = t39 * t111
      t205 = FJET(XB1, XB2, s, -t114, t116, 0.0D0, t110, -t122, t163)
      t207 = FJET(XB1, XB2, s, -t174, t175, t171, -t173, t189, -t198)
      rrqqbar2gght2s2e0 = t105 * t104 + t164 * t163 + t166 * t104 - t199
     # * 0.3141592653589793D1 * t201 * t196 / 0.8D1 + t205 * t163 - t207
     # * 0.3141592653589793D1 * t201 * t196 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght2s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t32 = log(-0.4D1 * t22 / t23 * t27 * x4 * t4)
      t39 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t46 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.16D2 - (-0.180D3 
     #* 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.3141592653589793D1)
     # * t1 * t9 / 0.1440D4 + t6 * t7 * (t39 - t8) / x3 / 0.16D2
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t46)
      t49 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t50 = s * t49
      t52 = t50 * t1 * x1
      t53 = -0.1D1 + x1
      t54 = t1 * t53
      t56 = t50 * t54 * x4
      t58 = t50 * t54 * t4
      t59 = t49 ** 2
      t64 = s * t59 * t26 * x1 * t53 * x4
      t68 = 0.1D1 / (-0.2D1 + t49)
      t70 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x
     #4)
      t74 = t6 * t7 * t53 * t59 * t68 * t70 * t10 / 0.8D1
      t75 = FJET(XB1, XB2, s, 0.0D0, t52, -t56, t58, -t64, t74)
      t77 = t1 * t7
      t82 = t53 * t59 * t68 * t70 * t10
      t85 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t46)
      t87 = FJET(XB1, XB2, s, -t56, t58, 0.0D0, t52, -t64, t74)
      rrqqbar2gght2s2em1 = t46 * t47 + t75 * 0.3141592653589793D1 * t77 
     #* t82 / 0.8D1 + t85 * t46 + t87 * 0.3141592653589793D1 * t77 * t82
     # / 0.8D1

      end function



      doubleprecision function rrqqbar2gght2s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t11 = 0.3141592653589793D1 * t1 * t7 * t8 / 0.16D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t11)
      t15 = t1 * t7 * t8
      t17 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrqqbar2gght2s2em2 = -t12 * 0.3141592653589793D1 * t15 / 0.16D2 - 
     #t17 * 0.3141592653589793D1 * t15 / 0.16D2

      end function



      doubleprecision function rrqqbar2gght2s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      rrqqbar2gght2s2em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght2s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3
      rrqqbar2gght2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh21J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = s * t3
      t5 = 0.1D1 - z
      t6 = t5 * x1
      t9 = 0.1D1 - x3
      t12 = s - t4 * t6 * x3 - t4 * t6 * t9
      t13 = t12 ** 2
      t14 = t2 * t13
      t15 = 0.1D1 - x1
      t16 = t5 * t15
      t19 = 0.1D1 - x4
      t22 = s - t4 * t16 * x4 - t4 * t16 * t19
      t23 = t14 * t22
      t24 = t3 * t5
      t25 = x1 * x3
      t26 = t24 * t25
      t29 = t12 * t22
      t30 = t1 ** 2
      t31 = t29 * t30
      t32 = t3 ** 2
      t33 = t5 ** 2
      t34 = t32 * t33
      t35 = x1 ** 2
      t36 = x3 ** 2
      t38 = t34 * t35 * t36
      t43 = t32 * t3
      t44 = t33 * t5
      t45 = t43 * t44
      t46 = t35 * x1
      t54 = x1 * t9
      t55 = t24 * t54
      t58 = t9 ** 2
      t60 = t34 * t35 * t58
      t72 = t15 ** 2
      t73 = t19 ** 2
      t74 = t72 * t73
      t78 = t30 * s
      t79 = z ** 2
      t81 = t78 * t79 * t22
      t86 = x4 ** 2
      t87 = t72 * t86
      t91 = t13 * t22
      t92 = t32 ** 2
      t93 = t2 * t92
      t94 = t33 ** 2
      t96 = t91 * t93 * t94
      t97 = t35 * t9
      t102 = cos(x2 * 0.3141592653589793D1)
      t106 = Sqrt(x3 * t9 * x4 * t19)
      t109 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t102 * t106
      t110 = t72 * x4 * t109
      t115 = t14 * t22 * t32
      t116 = t33 * x1
      t117 = x3 * t15
      t119 = t116 * t117 * t19
      t122 = t30 * t43
      t124 = t29 * t122 * t44
      t125 = t72 * t19
      t126 = t125 * x4
      t127 = t25 * t126
      t130 = -0.275D3 * t23 * t26 + 0.94D2 * t31 * t38 + 0.47D2 * t31 * 
     #t26 + 0.47D2 * t31 * t45 * t46 * t36 * x3 - 0.17D2 * t23 * t38 - 0
     #.275D3 * t23 * t55 + 0.94D2 * t31 * t60 + 0.47D2 * t31 * t45 * t46
     # * t58 * t9 + 0.47D2 * t31 * t55 - 0.17D2 * t23 * t60 + 0.32D2 * t
     #23 * t34 * t74 - 0.64D2 * t81 * t60 - 0.64D2 * t81 * t55 + 0.32D2 
     #* t23 * t34 * t87 + 0.216D3 * t96 * t97 * t110 + 0.306D3 * t115 * 
     #t119 + 0.16D2 * t124 * t127
      t131 = t78 * t92
      t132 = t94 * t35
      t133 = t131 * t132
      t134 = x3 * t12
      t139 = t14 * t22 * t43
      t140 = t44 * t72
      t146 = t92 * t3
      t147 = t78 * t146
      t148 = t94 * t5
      t149 = t148 * t35
      t150 = t149 * t9
      t152 = t72 * t15
      t155 = t109 * t19 * x4
      t156 = t12 * t152 * t155
      t159 = t2 * t43
      t161 = t91 * t159 * t44
      t162 = t54 * t126
      t165 = t30 * t92
      t166 = t94 * x1
      t167 = t165 * t166
      t168 = x3 * t13
      t169 = t152 * t19
      t170 = t169 * t86
      t174 = t152 * t73
      t175 = t174 * x4
      t179 = t30 * t146
      t180 = t29 * t179
      t181 = t148 * t46
      t182 = t9 * t72
      t183 = t109 ** 2
      t185 = t181 * t182 * t183
      t188 = t29 * t122
      t189 = t44 * t35
      t190 = t9 * t15
      t195 = t44 * x1
      t196 = t122 * t195
      t200 = t131 * t166
      t207 = t78 * t43
      t208 = t207 * t195
      t212 = t30 * t32
      t213 = t29 * t212
      t215 = t116 * t190 * x4
      t218 = t58 * t15
      t223 = t92 * t32
      t226 = t94 * t33 * t46
      t227 = t78 * t223 * t226
      t228 = t9 * t12
      t230 = t152 * x4 * t183
      t237 = t165 * t132
      t238 = t9 * t13
      t243 = t30 * t223 * t226
      t247 = -0.112D3 * t133 * t134 * t110 - 0.32D2 * t139 * t140 * t19 
     #* x1 * t109 - 0.288D3 * t147 * t150 * t156 + 0.144D3 * t161 * t162
     # + 0.108D3 * t167 * t168 * t170 - 0.108D3 * t167 * t168 * t175 + 0
     #.312D3 * t180 * t185 - 0.128D3 * t188 * t189 * t190 * t109 + 0.328
     #D3 * t196 * t168 * t126 + 0.108D3 * t200 * t134 * t175 - 0.108D3 *
     # t200 * t134 * t170 + 0.56D2 * t208 * t134 * t126 + 0.96D2 * t213 
     #* t215 + 0.96D2 * t188 * t189 * t218 * x4 - 0.216D3 * t227 * t228 
     #* t230 + 0.112D3 * t133 * t228 * t110 - 0.584D3 * t237 * t238 * t1
     #10 - 0.72D2 * t243 * t168 * t230
      t250 = t116 * t190 * t19
      t254 = t195 * t182 * t73
      t258 = t152 * t86 * x4
      t263 = t195 * t182 * t86
      t268 = t179 * t149
      t270 = t152 * t86 * t109
      t277 = t147 * t149
      t284 = t36 * t15
      t293 = t147 * t181
      t294 = t72 * t183
      t298 = t207 * t189
      t299 = t15 * t109
      t308 = t78 * t3 * t5
      t312 = t30 * t3
      t313 = t312 * t5
      t320 = 0.148D3 * t213 * t250 - 0.12D2 * t188 * t254 + 0.36D2 * t16
     #7 * t238 * t258 - 0.252D3 * t139 * t263 - 0.36D2 * t139 * t254 - 0
     #.72D2 * t268 * t168 * t270 - 0.328D3 * t237 * t168 * t110 + 0.144D
     #3 * t277 * t134 * t270 + 0.216D3 * t227 * t134 * t230 + 0.96D2 * t
     #188 * t189 * t284 * t19 + 0.96D2 * t213 * t119 + 0.144D3 * t161 * 
     #t127 - 0.168D3 * t293 * t134 * t294 + 0.144D3 * t298 * t134 * t299
     # - 0.128D3 * t188 * t189 * t117 * t109 - 0.28D2 * t308 * t25 * t12
     # + 0.192D3 * t313 * t54 * t13 - 0.28D2 * t308 * t54 * t12
      t321 = t125 * t109
      t328 = t212 * t116
      t329 = t15 * t19
      t336 = x3 * t72
      t338 = t195 * t336 * t73
      t348 = t165 * t94 * t46
      t359 = t149 * x3
      t362 = t13 * t152 * t155
      t365 = t159 * t189
      t370 = t15 * x4
      t378 = t78 * t32 * t116
      t386 = t169 * t183
      t393 = 0.112D3 * t133 * t134 * t321 - 0.168D3 * t293 * t228 * t294
     # - 0.530D3 * t328 * t168 * t329 - 0.328D3 * t237 * t238 * t321 - 0
     #.252D3 * t139 * t338 + 0.748D3 * t188 * t263 + 0.148D3 * t188 * t1
     #89 * t218 * t19 - 0.128D3 * t348 * t299 * t29 * t58 - 0.112D3 * t1
     #33 * t228 * t321 - 0.72D2 * t96 * t97 * t321 + 0.144D3 * t179 * t3
     #59 * t362 - 0.528D3 * t365 * t299 * t91 * x3 - 0.530D3 * t328 * t2
     #38 * t370 + 0.36D2 * t200 * t134 * t258 + 0.36D2 * t378 * t134 * t
     #370 - 0.128D3 * t348 * t299 * t29 * t36 + 0.72D2 * t243 * t168 * t
     #386 - 0.36D2 * t200 * t228 * t258
      t408 = t122 * t189
      t412 = t179 * t181
      t419 = t35 ** 2
      t421 = t78 * t92 * t43 * t94 * t44 * t419
      t423 = t152 * t183 * t109
      t430 = t33 * t35
      t439 = t152 * t73 * t19
      t464 = -0.36D2 * t378 * t228 * t370 - 0.28D2 * t208 * t228 * t87 +
     # 0.430D3 * t196 * t238 * t87 + 0.144D3 * t298 * t228 * t299 + 0.40
     #0D3 * t408 * t168 * t299 + 0.56D2 * t412 * t168 * t294 + 0.144D3 *
     # t421 * t134 * t423 + 0.430D3 * t196 * t168 * t74 + 0.128D3 * t213
     # * t430 * t58 * z + 0.192D3 * t313 * t25 * t13 + 0.36D2 * t200 * t
     #228 * t439 + 0.36D2 * t378 * t228 * t329 - 0.28D2 * t208 * t228 * 
     #t74 - 0.108D3 * t200 * t228 * t175 + 0.108D3 * t200 * t228 * t170 
     #+ 0.56D2 * t208 * t228 * t126 + 0.16D2 * t124 * t162 + 0.32D2 * t9
     #3 * t132 * t294 * t91
      t475 = t165 * t132 * t72
      t476 = t109 * t12
      t488 = t91 * t2 * t146
      t492 = t195 * t336 * t86
      t508 = t116 * t117 * x4
      t511 = t174 * t109
      t529 = 0.72D2 * t243 * t238 * t230 - 0.72D2 * t268 * t238 * t270 +
     # 0.144D3 * t277 * t228 * t270 + 0.240D3 * t475 * t476 * t22 * t9 *
     # t19 - 0.880D3 * t475 * t476 * t22 * x4 * t9 - 0.72D2 * t488 * t18
     #5 - 0.36D2 * t139 * t492 + 0.748D3 * t188 * t338 + 0.328D3 * t196 
     #* t238 * t126 + 0.306D3 * t115 * t215 - 0.28D2 * t208 * t134 * t87
     # + 0.122D3 * t115 * t250 + 0.122D3 * t115 * t508 - 0.72D2 * t268 *
     # t168 * t511 - 0.32D2 * t139 * t140 * x4 * x1 * t109 - 0.584D3 * t
     #237 * t168 * t321 + 0.144D3 * t277 * t134 * t511 - 0.216D3 * t227 
     #* t134 * t386
      t531 = t22 * x3
      t541 = t181 * t336 * t183
      t552 = t29 * t312
      t568 = t35 * x3
      t594 = -0.880D3 * t475 * t476 * t531 * t19 + 0.240D3 * t475 * t476
     # * t531 * x4 - 0.72D2 * t488 * t541 + 0.312D3 * t180 * t541 + 0.21
     #6D3 * t227 * t228 * t386 - 0.288D3 * t147 * t359 * t156 + 0.128D3 
     #* t552 * t6 * t9 * z + 0.128D3 * t213 * t430 * t36 * z + 0.128D3 *
     # t552 * t6 * x3 * z - 0.36D2 * t167 * t168 * t258 + 0.216D3 * t96 
     #* t568 * t321 - 0.72D2 * t96 * t568 * t110 - 0.72D2 * t243 * t238 
     #* t386 - 0.36D2 * t200 * t134 * t439 + 0.144D3 * t179 * t150 * t36
     #2 - 0.528D3 * t365 * t299 * t91 * t9 - 0.36D2 * t167 * t238 * t439
     # + 0.108D3 * t167 * t238 * t175
      t646 = -0.108D3 * t167 * t238 * t170 + 0.10D2 * t196 * t168 * t87 
     #- 0.182D3 * t328 * t168 * t370 + 0.36D2 * t167 * t168 * t439 + 0.1
     #0D2 * t196 * t238 * t74 - 0.182D3 * t328 * t238 * t329 - 0.72D2 * 
     #t268 * t238 * t511 + 0.144D3 * t277 * t228 * t511 - 0.12D2 * t188 
     #* t492 + 0.148D3 * t213 * t508 + 0.148D3 * t188 * t189 * t284 * x4
     # + 0.400D3 * t408 * t238 * t299 + 0.56D2 * t412 * t238 * t294 + 0.
     #144D3 * t421 * t228 * t423 - 0.64D2 * t81 * t38 - 0.36D2 * t378 * 
     #t134 * t329 - 0.28D2 * t208 * t134 * t74 - 0.64D2 * t81 * t26
      rrqqbar2ggh21J1 = -wd * (t130 + t247 + t320 + t393 + t464 + t529 +
     # t594 + t646) / t1 / t13 / t22 / z / 0.3141592653589793D1 / 0.27D2

      end function
  
   
 

      doubleprecision function rrqqbar2ggh21J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t9 = 0.1D1 - z
      t10 = t9 ** 2
      t11 = t10 ** 2
      t13 = x1 ** 2
      t14 = t13 * x1
      t15 = t11 * t9 * t14
      t16 = t3 * t7 * t15
      t17 = s * t4
      t18 = x1 * t9
      t21 = 0.1D1 - x3
      t24 = s - t17 * t18 * x3 - t17 * t18 * t21
      t25 = x3 * t24
      t26 = 0.1D1 - x1
      t27 = t26 ** 2
      t31 = cos(x2 * 0.3141592653589793D1)
      t33 = 0.1D1 - x4
      t34 = x4 * t33
      t36 = Sqrt(x3 * t21 * t34)
      t39 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t31 * t36
      t40 = t39 ** 2
      t41 = t27 * t40
      t45 = t5 * t4
      t47 = t10 * t9
      t48 = t47 * t13
      t49 = t3 * t45 * t48
      t50 = t26 * t39
      t54 = t24 ** 2
      t55 = t9 * t26
      t60 = s - t17 * t55 * x4 - t17 * t55 * t33
      t61 = t54 * t60
      t62 = t1 * s
      t63 = t62 * t6
      t64 = t61 * t63
      t65 = t11 * t14
      t66 = x3 ** 2
      t67 = t66 * t26
      t72 = t2 * t6
      t73 = t72 * t65
      t74 = t66 * x3
      t75 = t74 * t54
      t76 = t26 * t33
      t80 = t21 ** 2
      t81 = t80 * t21
      t82 = t81 * t54
      t87 = t10 * x1
      t88 = t3 * t5 * t87
      t89 = t21 * t24
      t90 = t26 * x4
      t94 = t2 * t45
      t95 = t47 * x1
      t96 = t94 * t95
      t97 = t21 * t54
      t98 = x4 ** 2
      t99 = t27 * t98
      t103 = t2 * t5
      t104 = t103 * t87
      t108 = x3 * t54
      t109 = t33 ** 2
      t110 = t27 * t109
      t117 = t94 * t48
      t121 = t2 * t7
      t122 = t121 * t15
      t126 = t11 * t13
      t127 = t72 * t126
      t129 = t27 * x4 * t39
      t133 = t6 * t5
      t136 = t11 * t10 * t14
      t137 = t3 * t133 * t136
      t138 = t27 * t26
      t140 = t138 * x4 * t40
      t145 = t3 * t6 * t126
      t149 = 0.344D3 * t16 * t25 * t41 - 0.256D3 * t49 * t25 * t50 + 0.7
     #2D2 * t64 * t65 * t67 * t39 + 0.72D2 * t73 * t75 * t76 - 0.72D2 * 
     #t73 * t82 * t76 + 0.72D2 * t88 * t89 * t90 - 0.542D3 * t96 * t97 *
     # t99 + 0.444D3 * t104 * t97 * t90 - 0.542D3 * t96 * t108 * t110 - 
     #0.256D3 * t49 * t89 * t50 - 0.288D3 * t117 * t108 * t50 + 0.24D2 *
     # t122 * t108 * t41 + 0.256D3 * t127 * t108 * t129 - 0.72D2 * t137 
     #* t25 * t140 + 0.144D3 * t145 * t25 * t129
      t150 = t62 * t54
      t152 = t150 * t60 * t45
      t153 = t47 * t27
      t166 = t72 * t126 * t27
      t167 = t39 * t24
      t168 = t60 * x3
      t183 = t66 * t54
      t187 = t80 * t54
      t192 = t150 * t60 * t5
      t211 = t61 * t62 * t7
      t212 = t21 * t27
      t214 = t15 * t212 * t40
      t220 = 0.64D2 * t152 * t153 * x4 * x1 * t39 - 0.288D3 * t117 * t97
     # * t50 + 0.24D2 * t122 * t97 * t41 + 0.640D3 * t166 * t167 * t168 
     #* t33 + 0.64D2 * t166 * t167 * t168 * x4 - 0.72D2 * t73 * t75 * t9
     #0 + 0.72D2 * t73 * t82 * t90 + 0.256D3 * t117 * t183 * t76 + 0.256
     #D3 * t117 * t187 * t76 - 0.64D2 * t192 * t10 * t27 * t34 - 0.216D3
     # * t73 * t183 * t50 + 0.64D2 * t166 * t167 * t60 * t21 * t33 + 0.6
     #40D3 * t166 * t167 * t60 * x4 * t21 + 0.72D2 * t211 * t214 + 0.256
     #D3 * t117 * t183 * t90
      t228 = t27 * t33
      t229 = t228 * t39
      t233 = t24 * t60
      t234 = t94 * t47
      t235 = t233 * t234
      t236 = x1 * x3
      t237 = t228 * x4
      t241 = t233 * t94
      t242 = x3 * t27
      t247 = t233 * t103
      t248 = t21 * t26
      t250 = t87 * t248 * t33
      t261 = t80 * t26
      t266 = x3 * t26
      t268 = t87 * t266 * t33
      t275 = t2 * t133 * t136
      t277 = t138 * t33 * t40
      t288 = t233 * t121
      t291 = 0.256D3 * t117 * t187 * t90 + 0.72D2 * t122 * t183 * t129 -
     # 0.72D2 * t122 * t183 * t229 - 0.608D3 * t235 * t236 * t237 - 0.59
     #2D3 * t241 * t95 * t242 * t109 - 0.216D3 * t247 * t250 - 0.16D2 * 
     #t241 * t95 * t212 * t109 - 0.592D3 * t241 * t95 * t212 * t98 - 0.2
     #16D3 * t241 * t48 * t261 * t33 - 0.640D3 * t247 * t268 - 0.72D2 * 
     #t88 * t89 * t76 + 0.72D2 * t275 * t97 * t277 - 0.72D2 * t137 * t89
     # * t277 + 0.496D3 * t73 * t50 * t233 * t80 - 0.264D3 * t288 * t214
      t304 = t87 * t266 * x4
      t337 = t15 * t242 * t40
      t347 = t87 * t248 * x4
      t350 = 0.496D3 * t241 * t48 * t248 * t39 - 0.144D3 * t145 * t25 * 
     #t229 - 0.16D2 * t241 * t95 * t242 * t98 - 0.216D3 * t247 * t304 - 
     #0.216D3 * t241 * t48 * t67 * x4 + 0.64D2 * t152 * t153 * t33 * x1 
     #* t39 - 0.640D3 * t241 * t48 * t67 * t33 + 0.72D2 * t122 * t187 * 
     #t229 + 0.72D2 * t64 * t65 * t261 * t39 - 0.62D2 * t96 * t97 * t110
     # + 0.108D3 * t104 * t97 * t76 + 0.72D2 * t88 * t25 * t76 + 0.72D2 
     #* t211 * t337 - 0.264D3 * t288 * t337 + 0.496D3 * t241 * t48 * t26
     #6 * t39 - 0.640D3 * t247 * t347
      t353 = t2 * t4
      t354 = t353 * t9
      t359 = t3 * t4 * t9
      t363 = x1 * t21
      t374 = t150 * t60
      t375 = t4 * t9
      t376 = t375 * t236
      t379 = t233 * t2
      t380 = t5 * t10
      t381 = t13 * t66
      t382 = t380 * t381
      t387 = t45 * t47
      t388 = t14 * t74
      t389 = t387 * t388
      t394 = t375 * t363
      t397 = t13 * t80
      t398 = t380 * t397
      t401 = t14 * t81
      t402 = t387 * t401
      t407 = -0.30D2 * t354 * t236 * t54 + 0.56D2 * t359 * t236 * t24 - 
     #0.30D2 * t354 * t363 * t54 + 0.56D2 * t359 * t363 * t24 - 0.134D3 
     #* t192 * t304 - 0.190D3 * t192 * t268 + 0.156D3 * t374 * t376 - 0.
     #108D3 * t379 * t382 - 0.54D2 * t379 * t376 - 0.54D2 * t379 * t389 
     #+ 0.490D3 * t374 * t382 + 0.156D3 * t374 * t394 - 0.108D3 * t379 *
     # t398 - 0.54D2 * t379 * t402 - 0.54D2 * t379 * t394
      t413 = z ** 2
      t415 = t3 * t413 * t60
      t437 = t13 ** 2
      t439 = t3 * t6 * t45 * t11 * t47 * t437
      t441 = t138 * t40 * t39
      t457 = 0.490D3 * t374 * t398 - 0.32D2 * t374 * t380 * t110 + 0.128
     #D3 * t415 * t398 + 0.128D3 * t415 * t394 - 0.32D2 * t374 * t380 * 
     #t99 + 0.128D3 * t415 * t382 + 0.128D3 * t415 * t376 - 0.72D2 * t37
     #4 * t389 - 0.72D2 * t374 * t402 - 0.72D2 * t275 * t97 * t140 - 0.1
     #44D3 * t439 * t25 * t441 + 0.72D2 * t137 * t89 * t140 - 0.144D3 * 
     #t145 * t89 * t129 + 0.544D3 * t127 * t97 * t129 + 0.72D2 * t275 * 
     #t108 * t140
      t475 = t62 * t45 * t48
      t507 = -0.640D3 * t241 * t48 * t261 * x4 - 0.604D3 * t96 * t108 * 
     #t237 - 0.144D3 * t439 * t89 * t441 + 0.344D3 * t16 * t89 * t41 - 0
     #.134D3 * t192 * t250 + 0.752D3 * t475 * t50 * t61 * t21 + 0.144D3 
     #* t145 * t89 * t229 + 0.256D3 * t127 * t97 * t229 - 0.72D2 * t275 
     #* t108 * t277 + 0.544D3 * t127 * t108 * t229 + 0.72D2 * t137 * t25
     # * t277 - 0.72D2 * t88 * t25 * t90 - 0.62D2 * t96 * t108 * t99 + 0
     #.108D3 * t104 * t108 * t90 - 0.604D3 * t96 * t97 * t237
      t513 = t103 * t10
      t541 = t10 * t13
      t546 = t233 * t353
      t565 = -0.190D3 * t192 * t347 + 0.136D3 * t234 * t401 * t54 - 0.25
     #6D3 * t513 * t381 * t54 + 0.136D3 * t234 * t388 * t54 - 0.256D3 * 
     #t513 * t397 * t54 + 0.752D3 * t475 * t50 * t61 * x3 + 0.496D3 * t7
     #3 * t50 * t233 * t66 + 0.444D3 * t104 * t108 * t76 - 0.608D3 * t23
     #5 * t363 * t237 - 0.64D2 * t63 * t126 * t41 * t61 - 0.256D3 * t247
     # * t541 * t80 * z - 0.256D3 * t546 * t18 * t21 * z - 0.256D3 * t24
     #7 * t541 * t66 * z - 0.256D3 * t546 * t18 * x3 * z - 0.216D3 * t73
     # * t187 * t50 - 0.72D2 * t122 * t187 * t129
      rrqqbar2ggh21J2 = -wd * (t149 + t220 + t291 + t350 + t407 + t457 +
     # t507 + t565) / t1 / t54 / t60 / z / 0.3141592653589793D1 / 0.27D2

      end function
  
   
 

      doubleprecision function rrqqbar2ggh21J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
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
      t21 = t20 ** 2
      t22 = t1 ** 2
      t23 = t21 * t22
      t24 = t19 * t23
      t25 = t3 ** 2
      t26 = t25 * x1
      t27 = x3 * t11
      t29 = t26 * t27 * x4
      t32 = t21 * t1
      t33 = t32 * t3
      t34 = x1 * x3
      t35 = t10 ** 2
      t39 = t21 * s
      t41 = t39 * t1 * t3
      t45 = x1 * t7
      t53 = t26 * t27 * t15
      t56 = t22 * t1
      t57 = t21 * t56
      t58 = t25 * t3
      t59 = t58 * x1
      t60 = t57 * t59
      t61 = t7 * t35
      t62 = t11 ** 2
      t63 = t15 ** 2
      t64 = t62 * t63
      t68 = t23 * t26
      t69 = t11 * t15
      t73 = t35 * x3
      t81 = t19 * t57 * t58
      t82 = t62 * t15
      t83 = t82 * x4
      t87 = t22 ** 2
      t88 = t21 * t87
      t89 = t25 ** 2
      t90 = x1 ** 2
      t91 = t90 * x1
      t93 = t88 * t89 * t91
      t94 = x3 ** 2
      t95 = t94 * t35
      t99 = cos(x2 * 0.3141592653589793D1)
      t103 = Sqrt(x3 * t7 * x4 * t15)
      t106 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t99 * t103
      t107 = t11 * t106
      t111 = t7 ** 2
      t112 = t111 * t35
      t116 = t58 * t90
      t117 = t57 * t116
      t118 = t11 * x4
      t128 = t87 * t1
      t129 = t21 * t128
      t131 = t89 * t3 * t91
      t132 = t129 * t131
      t133 = t106 ** 2
      t134 = t62 * t133
      t139 = t39 * t128 * t131
      t140 = t7 * t10
      t144 = t25 * t90
      t149 = 0.128D3 * t24 * t29 - 0.28792D5 * t33 * t34 * t35 - 0.144D3
     # * t41 * t34 * t10 - 0.28792D5 * t33 * t45 * t35 - 0.144D3 * t41 *
     # t45 * t10 + 0.256D3 * t24 * t53 - 0.304D3 * t60 * t61 * t64 + 0.2
     #9128D5 * t68 * t61 * t69 + 0.28208D5 * t60 * t73 * t64 + 0.536D3 *
     # t68 * t73 * t69 + 0.128D3 * t81 * t45 * t83 - 0.80D2 * t93 * t95 
     #* t107 - 0.80D2 * t93 * t112 * t107 + 0.25296D5 * t117 * t95 * t11
     #8 + 0.25264D5 * t117 * t112 * t118 + 0.74288D5 * t117 * t61 * t107
     # + 0.32D2 * t132 * t61 * t134 - 0.144D3 * t139 * t140 * t134 + 0.1
     #28D3 * t24 * t144 * t94 * z
      t150 = t19 * t32
      t155 = x4 ** 2
      t156 = t62 * t155
      t160 = t89 * t90
      t161 = t88 * t160
      t163 = t62 * x4 * t106
      t171 = t88 * t160 * t62
      t172 = t106 * t10
      t187 = t20 * s
      t189 = t187 * t56 * t116
      t190 = t35 * t18
      t199 = t18 * x3
      t204 = t187 * t35
      t206 = t204 * t18 * t22
      t207 = t7 * t11
      t209 = t26 * t207 * t15
      t214 = t19 * t57
      t215 = t7 * t62
      t228 = t19 * t129
      t229 = x3 * t62
      t235 = t39 * t56 * t116
      t245 = x3 * t10
      t249 = 0.512D3 * t150 * t4 * x3 * z + 0.28208D5 * t60 * t61 * t156
     # - 0.74208D5 * t161 * t61 * t163 - 0.74320D5 * t161 * t73 * t163 -
     # 0.128D3 * t171 * t172 * t18 * t7 * t15 - 0.128D3 * t171 * t172 * 
     #t18 * x4 * t7 + 0.512D3 * t150 * t4 * t7 * z - 0.74296D5 * t189 * 
     #t107 * t190 * x3 - 0.8D1 * t93 * t107 * t19 * t94 - 0.128D3 * t171
     # * t172 * t199 * t15 - 0.304D3 * t206 * t209 + 0.128D3 * t24 * t20
     #9 + 0.64D2 * t214 * t59 * t215 * t63 - 0.74296D5 * t189 * t107 * t
     #190 * t7 - 0.128D3 * t171 * t172 * t199 * x4 + 0.64D2 * t228 * t13
     #1 * t229 * t133 + 0.288D3 * t235 * t140 * t107 + 0.74288D5 * t117 
     #* t73 * t107 + 0.32D2 * t132 * t73 * t134 - 0.144D3 * t139 * t245 
     #* t134
      t258 = z ** 2
      t260 = t39 * t258 * t18
      t261 = t1 * t3
      t262 = t261 * t34
      t265 = t22 * t25
      t266 = t90 * t94
      t267 = t265 * t266
      t270 = t261 * t45
      t273 = t90 * t111
      t274 = t265 * t273
      t277 = t204 * t18
      t280 = t19 * t21
      t283 = t56 * t58
      t311 = t26 * t207 * x4
      t317 = 0.288D3 * t235 * t245 * t107 + 0.128D3 * t24 * t144 * t111 
     #* z - 0.64D2 * t260 * t262 - 0.64D2 * t260 * t267 - 0.64D2 * t260 
     #* t270 - 0.64D2 * t260 * t274 + 0.25220D5 * t277 * t274 - 0.252D3 
     #* t280 * t270 + 0.4D1 * t280 * t283 * t91 * t111 * t7 + 0.8D1 * t2
     #80 * t274 + 0.28372D5 * t277 * t270 + 0.25220D5 * t277 * t267 + 0.
     #4D1 * t280 * t283 * t91 * t94 * x3 - 0.252D3 * t280 * t262 + 0.8D1
     # * t280 * t267 + 0.28372D5 * t277 * t262 + 0.64D2 * t214 * t59 * t
     #215 * t155 + 0.256D3 * t24 * t311 + 0.27920D5 * t60 * t73 * t83
      t318 = t23 * t25
      t361 = t82 * t106
      t383 = -0.25248D5 * t318 * t266 * t35 - 0.25248D5 * t318 * t273 * 
     #t35 + 0.25264D5 * t117 * t95 * t69 + 0.25296D5 * t117 * t112 * t69
     # + 0.536D3 * t68 * t61 * t118 - 0.304D3 * t60 * t73 * t156 + 0.291
     #28D5 * t68 * t73 * t118 + 0.27920D5 * t60 * t61 * t83 + 0.28224D5 
     #* t206 * t311 - 0.8D1 * t93 * t107 * t19 * t111 + 0.64D2 * t228 * 
     #t131 * t215 * t133 - 0.8D1 * t214 * t116 * t207 * t106 - 0.8D1 * t
     #214 * t116 * t27 * t106 - 0.74320D5 * t161 * t61 * t361 - 0.74208D
     #5 * t161 * t73 * t361 - 0.304D3 * t206 * t29 + 0.28224D5 * t206 * 
     #t53 + 0.128D3 * t81 * t34 * t83 + 0.64D2 * t214 * t59 * t229 * t63
     # + 0.64D2 * t214 * t59 * t229 * t155
      rrqqbar2ggh21J3 = -wd * (t149 + t249 + t317 + t383) / t20 / t35 / 
     #t18 / z / 0.3141592653589793D1 / 0.27D2

      end function
  
 