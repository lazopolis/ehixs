  
      subroutine ggbbH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision ggbbH41J1  
      doubleprecision ggbbH41J2  
      doubleprecision ggbbH41J3  
      doubleprecision ggbbH4n1e1  
      doubleprecision ggbbH4n1e0  
      doubleprecision ggbbH4n1em1  
      doubleprecision ggbbH4n1em2  
      doubleprecision ggbbH4n1em3  
      doubleprecision ggbbH4n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=ggbbH4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=ggbbH4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=ggbbH4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=ggbbH4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=ggbbH4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=ggbbH4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function ggbbH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH41J1
      doubleprecision ggbbH41J2
      doubleprecision ggbbH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = x1 ** 2
      t14 = t4 ** 2
      t15 = t13 * t14
      t18 = log(0.4D1 * t12 * t15)
      t21 = t1 ** 2
      t23 = 0.1D1 / s
      t24 = ggbbH41J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t33 = t18 ** 2
      t37 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t44 = t28 - t30
      t52 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t53 = t23 * t52
      t55 = lh * t21
      t56 = t15 * x4
      t59 = log(0.4D1 * t12 * t56)
      t65 = t21 * t23
      t67 = t59 ** 2
      t73 = t44 * t21
      t74 = t73 * t53
      t76 = 0.1D1 / x4
      t78 = x3 * t9
      t79 = t78 * t11
      t82 = log(0.4D1 * t79 * t56)
      t90 = 0.1D1 / x3
      t97 = log(0.4D1 * t78 * t11 * t13 * t14)
      t104 = t97 ** 2
      t113 = (-0.180D3 * lh - 0.90D2 * t18) * t21 * t23 * t24 / 0.2880D4
     # + (t28 - t30 + 0.180D3 * t18 * lh + 0.45D2 * t33) * t21 * t23 * t
     #37 / 0.2880D4 + (-0.2884936567583026D3 - 0.120D3 * t27 * lh + 0.60
     #D2 * lh * t29 - t18 * t44 - 0.90D2 * t33 * lh - 0.15D2 * t33 * t18
     #) * t21 * t53 / 0.2880D4 + (-0.180D3 * t55 * t23 * (t37 - t59 * t5
     #2) + 0.90D2 * t65 * (t24 - t59 * t37 + t67 * t52 / 0.2D1) + t74) *
     # t76 / 0.2880D4 + (0.90D2 * t65 * (t37 - t82 * t52) - 0.180D3 * t5
     #5 * t53) * t90 * t76 / 0.2880D4 + (-0.180D3 * t55 * t23 * (t37 - t
     #97 * t52) + 0.90D2 * t65 * (t24 - t97 * t37 + t104 * t52 / 0.2D1) 
     #+ t74) * t90 / 0.2880D4
      t114 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t113)
      t116 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t117 = s * t116
      t118 = t1 * x1
      t119 = t117 * t118
      t120 = t1 * t4
      t121 = t120 * x4
      t122 = t117 * t121
      t123 = -0.1D1 + x4
      t124 = t120 * t123
      t125 = t117 * t124
      t126 = t116 ** 2
      t129 = x1 * t4
      t131 = s * t126 * t21 * t129 * x4
      t133 = 0.1D1 / (-0.2D1 + t116)
      t134 = t126 * t133
      t135 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t136 = t134 * t135
      t139 = x4 * t123
      t140 = t126 ** 2
      t142 = t139 * t140 * t14
      t145 = log(-0.4D1 * t13 * t9 * t11 * t142)
      t146 = t145 * t126
      t147 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t148 = t133 * t147
      t154 = ggbbH41J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t158 = t145 ** 2
      t165 = t73 * t23
      t166 = t134 * t147
      t171 = x3 * t13 * t12
      t174 = log(-0.4D1 * t171 * t142)
      t180 = t55 * t23
      t187 = (-0.180D3 * t55 * t23 * (t136 - t146 * t148) + 0.90D2 * t65
     # * (t134 * t154 - t146 * t133 * t135 + t158 * t126 * t148 / 0.2D1)
     # + t165 * t166) * t76 / 0.2880D4 + (0.90D2 * t65 * (t136 - t174 * 
     #t126 * t148) - 0.180D3 * t180 * t166) * t90 * t76 / 0.2880D4
      t188 = FJET(XB1, XB2, s, 0.0D0, t119, -t122, t125, -t131, t187)
      t190 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t113)
      t192 = FJET(XB1, XB2, s, t119, 0.0D0, t125, -t122, -t131, t187)
      t194 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t195 = s * t194
      t196 = t118 * x3
      t197 = t195 * t196
      t198 = -0.1D1 + x3
      t199 = t118 * t198
      t200 = t195 * t199
      t201 = t195 * t120
      t202 = t194 ** 2
      t206 = s * t202 * t21 * t129 * x3
      t208 = 0.1D1 / (-0.2D1 + t194)
      t209 = t202 * t208
      t210 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t211 = t209 * t210
      t212 = t14 * t198
      t213 = t202 ** 2
      t218 = log(-0.4D1 * t171 * t212 * x4 * t213)
      t220 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t221 = t208 * t220
      t226 = t209 * t220
      t236 = log(-0.4D1 * t79 * t15 * t198 * t213)
      t237 = t236 * t202
      t243 = ggbbH41J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t247 = t236 ** 2
      t258 = (0.90D2 * t65 * (t211 - t218 * t202 * t221) - 0.180D3 * t18
     #0 * t226) * t90 * t76 / 0.2880D4 + (-0.180D3 * t55 * t23 * (t211 -
     # t237 * t221) + 0.90D2 * t65 * (t209 * t243 - t237 * t208 * t210 +
     # t247 * t202 * t221 / 0.2D1) + t165 * t226) * t90 / 0.2880D4
      t259 = FJET(XB1, XB2, s, t197, -t200, 0.0D0, -t201, -t206, t258)
      t261 = KAPPA2(x1, x2, x3, x4, z)
      t262 = s * t261
      t263 = t262 * t196
      t264 = t262 * t199
      t265 = t262 * t121
      t266 = t262 * t124
      t267 = t261 ** 2
      t272 = cos(t7)
      t275 = sqrt(x3 * t198 * t139)
      t280 = s * t267 * t21 * t129 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t272 * t275)
      t282 = 0.1D1 / (-0.2D1 + t261)
      t283 = t267 * t282
      t284 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t286 = t267 ** 2
      t291 = log(0.4D1 * t171 * t139 * t212 * t286)
      t293 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t296 = -t283 * t284 + t291 * t267 * t282 * t293
      t301 = 0.180D3 * t180 * t283 * t293
      t302 = 0.90D2 * t65 * t296 + t301
      t306 = FJET(XB1, XB2, s, t263, -t264, -t265, t266, t280, t302 * t9
     #0 * t76 / 0.2880D4)
      t308 = t90 * t76
      t311 = FJET(XB1, XB2, s, -t200, t197, -t201, 0.0D0, -t206, t258)
      t316 = 0.90D2 * t65 * t296 + t301
      t320 = FJET(XB1, XB2, s, -t264, t263, t266, -t265, t280, t316 * t9
     #0 * t76 / 0.2880D4)
      ggbbH4n1e1 = t114 * t113 + t188 * t187 + t190 * t113 + t192 * t187
     # + t259 * t258 + t306 * t302 * t308 / 0.2880D4 + t311 * t258 + t32
     #0 * t316 * t308 / 0.2880D4

      end function



      doubleprecision function ggbbH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH41J1
      doubleprecision ggbbH41J2
      doubleprecision ggbbH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
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
      t23 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t28 = lh * t6
      t29 = t7 * t23
      t31 = 0.180D3 * t28 * t29
      t33 = 0.1D1 / x4
      t36 = 0.1D1 / x3
      t41 = x3 * t12
      t46 = log(0.4D1 * t41 * t14 * t16 * t17)
      t54 = ggbbH41J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t60 = log(0.4D1 * t15 * t18)
      t67 = lh ** 2
      t69 = 0.3141592653589793D1 ** 2
      t73 = t60 ** 2
      t79 = (0.90D2 * t8 * (t9 - t22 * t23) - t31) * t33 / 0.2880D4 + t8
     # * t23 * t36 * t33 / 0.32D2 + (0.90D2 * t8 * (t9 - t46 * t23) - t3
     #1) * t36 / 0.2880D4 + t8 * t54 / 0.32D2 + (-0.180D3 * lh - 0.90D2 
     #* t60) * t6 * t7 * t9 / 0.2880D4 + (0.180D3 * t67 - 0.30D2 * t69 +
     # 0.180D3 * t60 * lh + 0.45D2 * t73) * t6 * t29 / 0.2880D4
      t80 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t79)
      t82 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t83 = s * t82
      t84 = t1 * x1
      t85 = t83 * t84
      t86 = t1 * t4
      t87 = t86 * x4
      t88 = t83 * t87
      t89 = -0.1D1 + x4
      t90 = t86 * t89
      t91 = t83 * t90
      t92 = t82 ** 2
      t95 = x1 * t4
      t97 = s * t92 * t6 * t95 * x4
      t99 = 0.1D1 / (-0.2D1 + t82)
      t100 = t92 * t99
      t101 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t105 = x4 * t89
      t106 = t92 ** 2
      t111 = log(-0.4D1 * t16 * t12 * t14 * t105 * t106 * t17)
      t113 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t114 = t99 * t113
      t119 = t28 * t7
      t127 = t36 * t33
      t131 = (0.90D2 * t8 * (t100 * t101 - t111 * t92 * t114) - 0.180D3 
     #* t119 * t100 * t113) * t33 / 0.2880D4 + t8 * t92 * t114 * t127 / 
     #0.32D2
      t132 = FJET(XB1, XB2, s, 0.0D0, t85, -t88, t91, -t97, t131)
      t134 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t79)
      t136 = FJET(XB1, XB2, s, t85, 0.0D0, t91, -t88, -t97, t131)
      t138 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t139 = s * t138
      t140 = t84 * x3
      t141 = t139 * t140
      t142 = -0.1D1 + x3
      t143 = t84 * t142
      t144 = t139 * t143
      t145 = t139 * t86
      t146 = t138 ** 2
      t150 = s * t146 * t6 * t95 * x3
      t153 = 0.1D1 / (-0.2D1 + t138)
      t154 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t155 = t153 * t154
      t159 = t146 * t153
      t160 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t163 = t146 ** 2
      t168 = log(-0.4D1 * t41 * t14 * t18 * t142 * t163)
      t180 = t8 * t146 * t155 * t127 / 0.32D2 + (0.90D2 * t8 * (t159 * t
     #160 - t168 * t146 * t155) - 0.180D3 * t119 * t159 * t154) * t36 / 
     #0.2880D4
      t181 = FJET(XB1, XB2, s, t141, -t144, 0.0D0, -t145, -t150, t180)
      t183 = KAPPA2(x1, x2, x3, x4, z)
      t184 = s * t183
      t185 = t184 * t140
      t186 = t184 * t143
      t187 = t184 * t87
      t188 = t184 * t90
      t189 = t183 ** 2
      t194 = cos(t10)
      t197 = sqrt(x3 * t142 * t105)
      t202 = s * t189 * t6 * t95 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 *
     # t194 * t197)
      t206 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t208 = 0.1D1 / (-0.2D1 + t183) * t206 * t127
      t210 = t8 * t189 * t208 / 0.32D2
      t211 = FJET(XB1, XB2, s, t185, -t186, -t187, t188, t202, -t210)
      t213 = t7 * t189
      t217 = FJET(XB1, XB2, s, -t144, t141, -t145, 0.0D0, -t150, t180)
      t219 = FJET(XB1, XB2, s, -t186, t185, t188, -t187, t202, -t210)
      ggbbH4n1e0 = t80 * t79 + t132 * t131 + t134 * t79 + t136 * t131 + 
     #t181 * t180 - t211 * t6 * t213 * t208 / 0.32D2 + t217 * t180 - t21
     #9 * t6 * t213 * t208 / 0.32D2

      end function



      doubleprecision function ggbbH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH41J1
      doubleprecision ggbbH41J2
      doubleprecision ggbbH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = ggbbH41J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t19 = x1 ** 2
      t20 = t4 ** 2
      t24 = log(0.4D1 * t15 / t16 * t19 * t20)
      t28 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t32 = 0.1D1 / x4
      t36 = 0.1D1 / x3
      t40 = t8 * t9 / 0.32D2 + (-0.180D3 * lh - 0.90D2 * t24) * t6 * t7 
     #* t28 / 0.2880D4 + t8 * t28 * t32 / 0.32D2 + t8 * t28 * t36 / 0.32
     #D2
      t41 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t40)
      t43 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t44 = s * t43
      t45 = t1 * x1
      t46 = t44 * t45
      t47 = t1 * t4
      t49 = t44 * t47 * x4
      t52 = t44 * t47 * (-0.1D1 + x4)
      t53 = t43 ** 2
      t56 = x1 * t4
      t58 = s * t53 * t6 * t56 * x4
      t61 = 0.1D1 / (-0.2D1 + t43)
      t62 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t66 = t8 * t53 * t61 * t62 * t32 / 0.32D2
      t67 = FJET(XB1, XB2, s, 0.0D0, t46, -t49, t52, -t58, t66)
      t72 = t53 * t61 * t62 * t32
      t75 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t40)
      t77 = FJET(XB1, XB2, s, t46, 0.0D0, t52, -t49, -t58, t66)
      t82 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t83 = s * t82
      t85 = t83 * t45 * x3
      t88 = t83 * t45 * (-0.1D1 + x3)
      t89 = t83 * t47
      t90 = t82 ** 2
      t94 = s * t90 * t6 * t56 * x3
      t97 = 0.1D1 / (-0.2D1 + t82)
      t98 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.0D0)
      t102 = t8 * t90 * t97 * t98 * t36 / 0.32D2
      t103 = FJET(XB1, XB2, s, t85, -t88, 0.0D0, -t89, -t94, t102)
      t108 = t90 * t97 * t98 * t36
      t111 = FJET(XB1, XB2, s, -t88, t85, -t89, 0.0D0, -t94, t102)
      ggbbH4n1em1 = t41 * t40 + t67 * t6 * t7 * t72 / 0.32D2 + t75 * t40
     # + t77 * t6 * t7 * t72 / 0.32D2 + t103 * t6 * t7 * t108 / 0.32D2 +
     # t111 * t6 * t7 * t108 / 0.32D2

      end function



      doubleprecision function ggbbH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH41J1
      doubleprecision ggbbH41J2
      doubleprecision ggbbH41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t9 = ggbbH41J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.0D0)
      t11 = t6 * t7 * t9 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t11)
      t14 = t7 * t9
      t16 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t11)
      ggbbH4n1em2 = t12 * t6 * t14 / 0.32D2 + t16 * t6 * t14 / 0.32D2

      end function



      doubleprecision function ggbbH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH41J1
      doubleprecision ggbbH41J2
      doubleprecision ggbbH41J3
      ggbbH4n1em3 = 0.0D0

      end function



      doubleprecision function ggbbH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH41J1
      doubleprecision ggbbH41J2
      doubleprecision ggbbH41J3
      ggbbH4n1em4 = 0.0D0

      end function
  
 

      doubleprecision function ggbbH41J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = z ** 2
      t7 = t1 * z
      t8 = kappa2(x1, x2, x3, x4, z)
      t9 = s * t8
      t10 = 0.1D1 - z
      t11 = t10 * x1
      t14 = 0.1D1 - x3
      t17 = s - t9 * t11 * x3 - t9 * t11 * t14
      t20 = 0.1D1 - x1
      t21 = t10 * t20
      t24 = 0.1D1 - x4
      t25 = t21 * t24
      t27 = s - t9 * t21 * x4 - t9 * t25
      t28 = t27 * t1
      t31 = t7 * t27
      t34 = t27 * s * t17
      t35 = t8 * t10
      t36 = x1 * t14
      t41 = t35 * t20 * t24
      t50 = t1 * t8 * t10
      t55 = t8 ** 2
      t57 = t10 ** 2
      t58 = t20 ** 2
      t60 = t24 ** 2
      t63 = t1 * t3
      t66 = t17 * t1
      t68 = x1 ** 2
      t70 = t14 ** 2
      t83 = -0.2D1 * t2 * t3 * z + 0.3D1 * t7 * t17 + t28 * t8 * t25 - t
     #2 + 0.3D1 * t31 - 0.6D1 * t34 * t35 * t36 - 0.6D1 * t34 * t41 - 0.
     #12D2 * t27 * t17 * s * z + 0.6D1 * t34 + 0.3D1 * t50 * t36 * t17 *
     # z + t28 * t55 * t57 * t58 * t60 + 0.4D1 * t63 * t17 + t66 * t55 *
     # t57 * t68 * t70 + t50 * t36 * t17 - 0.4D1 * t2 * t3 + 0.3D1 * t31
     # * t41 - 0.3D1 * t2 * z + t28 + 0.4D1 * t63 * t27 + t66
      ggbbH41J1 = 0.16D2 / 0.3D1 * wd * t83 / t17 / t27

      end function
  
   
 

      doubleprecision function ggbbH41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t9 = t5 * t8
      t11 = s - t2 * t5 * x4 - t2 * t9
      t13 = t3 * x1
      t16 = 0.1D1 - x3
      t19 = s - t2 * t13 * x3 - t2 * t13 * t16
      t20 = t11 * s * t19
      t21 = t1 * t3
      t22 = x1 * t16
      t26 = s ** 2
      t27 = t11 * t26
      t30 = t26 * z
      t31 = t30 * t11
      t33 = t21 * t4 * t8
      t35 = t1 ** 2
      t37 = t3 ** 2
      t38 = t4 ** 2
      t40 = t8 ** 2
      t47 = t26 * t1 * t3
      t50 = t26 * s
      t51 = t19 * t26
      t53 = x1 ** 2
      t55 = t16 ** 2
      t67 = 0.6D1 * t20 * t21 * t22 - t27 * t1 * t9 - t31 * t33 - t27 - 
     #t27 * t35 * t37 * t38 * t40 + 0.6D1 * t20 * t33 - 0.6D1 * t20 - t4
     #7 * t22 * t19 + t50 - t51 - t51 * t35 * t37 * t53 * t55 - t31 - t3
     #0 * t19 + t50 * z - t47 * t22 * t19 * z + 0.4D1 * t11 * t19 * s * 
     #z
      ggbbH41J2 = 0.16D2 / 0.3D1 * wd * t67 / t19 / t11

      end function
  
   
 

      doubleprecision function ggbbH41J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t4 = t1 * z
      t5 = kappa2(x1, x2, x3, x4, z)
      t6 = s * t5
      t7 = 0.1D1 - z
      t9 = t7 * (0.1D1 - x1)
      t13 = t9 * (0.1D1 - x4)
      t15 = s - t6 * t9 * x4 - t6 * t13
      t17 = t15 * t1
      t20 = t7 * x1
      t23 = 0.1D1 - x3
      t26 = s - t6 * t20 * x3 - t6 * t20 * t23
      ggbbH41J3 = 0.16D2 / 0.3D1 * wd * (t2 * z - t4 * t15 + t2 - t17 * 
     #t5 * t13 - t26 * t1 - t17 - t4 * t26 - t1 * t5 * t7 * x1 * t23 * t
     #26) / t26 / t15

      end function
  
 