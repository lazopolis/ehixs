  
      subroutine rrqqbar2qqbarht6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2qqbarht6s1e1  
      doubleprecision rrqqbar2qqbarht6s1e0  
      doubleprecision rrqqbar2qqbarht6s1em1  
      doubleprecision rrqqbar2qqbarht6s1em2  
      doubleprecision rrqqbar2qqbarht6s1em3  
      doubleprecision rrqqbar2qqbarht6s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2qqbarht6s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = t11 * t3 * x1
      t18 = t6 / z
      t19 = t16 * t18
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t36 = t33 * x4 * t34
      t39 = log(0.4D1 * t30 * t36)
      t40 = t39 ** 2
      t42 = 0.1D1 / (-0.2D1 + t1)
      t49 = t16 * t18 * wd
      t50 = lh * t22
      t56 = lh ** 2
      t58 = 0.3141592653589793D1 ** 2
      t60 = 0.180D3 * t56 - 0.30D2 * t58
      t63 = t22 * t42 * t34
      t65 = t19 * wd * t60 * t63
      t67 = 0.1D1 / x4
      t77 = log(0.4D1 * t30 * t33 * t34)
      t79 = t77 ** 2
      t90 = x3 * t25
      t95 = log(0.4D1 * t90 * t27 * t29 * t36)
      t100 = lh * wd
      t105 = 0.1D1 / x3
      t109 = t90 * t27
      t110 = t29 * t31
      t115 = log(0.4D1 * t109 * t110 * t32 * t34)
      t116 = t115 ** 2
      t130 = -0.16D2 / 0.405D3 * (0.45D2 * t19 * t23 * t40 * t42 * t34 +
     # 0.180D3 * t49 * t50 * t39 * t42 * t34 + t65) * t67 - 0.16D2 / 0.4
     #05D3 * (0.60D2 * lh * t58 - 0.2884936567583026D3 - 0.120D3 * t56 *
     # lh - t77 * t60 - 0.90D2 * t79 * lh - 0.15D2 * t79 * t77) * wd * t
     #16 * t18 * t63 - 0.4D1 / 0.405D3 * (-0.360D3 * t19 * wd * t95 * t6
     #3 - 0.720D3 * t19 * t100 * t63) * t105 * t67 - 0.16D2 / 0.405D3 * 
     #(0.45D2 * t19 * t23 * t116 * t42 * t34 + 0.180D3 * t49 * t50 * t11
     #5 * t42 * t34 + t65) * t105
      t131 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t130)
      t133 = sqrt(x4)
      t134 = -0.1D1 + t133
      t135 = t133 + 0.1D1
      t136 = t134 * t135
      t137 = KAPPA2(x1, x2, 0.0D0, -t136, z)
      t138 = s * t137
      t140 = t7 * x4
      t144 = t6 * t134 * t135
      t146 = t137 ** 2
      t149 = x1 * t6
      t153 = t146 ** 2
      t154 = t153 * t29
      t156 = t136 * x4
      t160 = log(-0.4D1 * t154 * t33 * t28 * t156)
      t161 = t160 ** 2
      t164 = 0.1D1 / (-0.2D1 + t137)
      t166 = Sqrt(-t136)
      t167 = t166 ** 2
      t168 = t164 * t153 * t167
      t176 = t60 * t22
      t187 = log(-0.4D1 * t154 * t33 * t27 * t90 * t156)
      t190 = t153 * t167 * t22
      t202 = -0.16D2 / 0.405D3 * (-0.45D2 * t49 * t22 * t161 * t168 - 0.
     #180D3 * t49 * t50 * t160 * t168 - t49 * t176 * t168) * t67 - 0.4D1
     # / 0.405D3 * (0.360D3 * t49 * t187 * t164 * t190 + 0.720D3 * t49 *
     # lh * t164 * t190) * t105 * t67
      t203 = FJET(XB1, XB2, s, t138 * t4, 0.0D0, -t138 * t140, t138 * t3
     # * t144, s * t146 * t11 * t149 * (-0.1D1 + x4), t202)
      t205 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t206 = s * t205
      t208 = sqrt(x3)
      t209 = -0.1D1 + t208
      t211 = t208 + 0.1D1
      t212 = x1 * t209 * t211
      t214 = t4 * x3
      t217 = t205 ** 2
      t223 = t32 * t27
      t228 = t217 ** 2
      t233 = log(-0.4D1 * t110 * t223 * x3 * t25 * t211 * t209 * x4 * t2
     #28)
      t235 = 0.1D1 / (-0.2D1 + t205)
      t238 = t209 * t211
      t239 = Sqrt(-t238)
      t240 = t239 ** 2
      t241 = t228 * t22 * t240
      t258 = log(-0.4D1 * t110 * t223 * t90 * t238 * t228)
      t259 = t258 ** 2
      t262 = t235 * t228 * t240
      t275 = -0.4D1 / 0.405D3 * (0.360D3 * t49 * t233 * t235 * t241 + 0.
     #720D3 * t49 * lh * t235 * t241) * t105 * t67 - 0.16D2 / 0.405D3 * 
     #(-0.45D2 * t49 * t22 * t259 * t262 - 0.180D3 * t49 * t50 * t258 * 
     #t262 - t49 * t176 * t262) * t105
      t276 = FJET(XB1, XB2, s, -t206 * t3 * t212, t206 * t214, 0.0D0, -t
     #206 * t7, s * t217 * t11 * t149 * (-0.1D1 + x3), t275)
      t278 = KAPPA2(x1, x2, x3, -t136, z)
      t279 = s * t278
      t280 = t279 * t3
      t285 = t278 ** 2
      t292 = Sqrt(t238 * t136)
      t302 = t285 ** 2
      t308 = log(0.4D1 * t110 * t32 * t109 * t136 * t211 * t302 * t209 *
     # x4)
      t314 = (t208 * t133 - 0.2D1 * t21 * t292) ** 2
      t318 = t314 * t302 / (-0.2D1 + t278)
      t325 = -0.90D2 * t19 * wd * t308 * t318 - 0.180D3 * t19 * t100 * t
     #318
      t329 = FJET(XB1, XB2, s, -t280 * t212, t279 * t214, -t279 * t140, 
     #t280 * t144, s * t285 * t11 * t149 * (x3 - 0.1D1 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t21 * t208 * t133 * t292), -0.4D1 / 0.405D3 * t325
     # * t105 * t67)
      rrqqbar2qqbarht6s1e1 = t131 * t130 + t203 * t202 + t276 * t275 - 0
     #.4D1 / 0.405D3 * t329 * t325 * t105 * t67

      end function



      doubleprecision function rrqqbar2qqbarht6s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t39 = log(0.4D1 * t30 * t33 * x4 * t34)
      t41 = 0.1D1 / (-0.2D1 + t1)
      t48 = t22 * t41
      t49 = t48 * t34
      t52 = 0.180D3 * t19 * wd * lh * t49
      t54 = 0.1D1 / x4
      t58 = t16 * t18 * wd
      t59 = 0.1D1 / x3
      t65 = x3 * t25
      t67 = t29 * t31
      t72 = log(0.4D1 * t65 * t27 * t67 * t32 * t34)
      t81 = lh ** 2
      t83 = 0.3141592653589793D1 ** 2
      t88 = log(0.4D1 * t30 * t33 * t34)
      t91 = t88 ** 2
      t99 = -0.16D2 / 0.405D3 * (-0.90D2 * t19 * t23 * t39 * t41 * t34 -
     # t52) * t54 - 0.32D2 / 0.9D1 * t58 * t48 * t34 * t59 * t54 - 0.16D
     #2 / 0.405D3 * (-0.90D2 * t19 * t23 * t72 * t41 * t34 - t52) * t59 
     #- 0.16D2 / 0.405D3 * (0.180D3 * t81 - 0.30D2 * t83 + 0.180D3 * t88
     # * lh + 0.45D2 * t91) * wd * t16 * t18 * t49
      t100 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t99)
      t102 = sqrt(x4)
      t103 = -0.1D1 + t102
      t104 = t102 + 0.1D1
      t105 = t103 * t104
      t106 = KAPPA2(x1, x2, 0.0D0, -t105, z)
      t107 = s * t106
      t109 = t7 * x4
      t113 = t6 * t103 * t104
      t115 = t106 ** 2
      t118 = x1 * t6
      t122 = t115 ** 2
      t129 = log(-0.4D1 * t122 * t29 * t33 * t28 * t105 * x4)
      t132 = 0.1D1 / (-0.2D1 + t106)
      t134 = Sqrt(-t105)
      t135 = t134 ** 2
      t136 = t132 * t122 * t135
      t154 = -0.16D2 / 0.405D3 * (0.90D2 * t58 * t22 * t129 * t136 + 0.1
     #80D3 * t58 * lh * t132 * t122 * t135 * t22) * t54 + 0.32D2 / 0.9D1
     # * t58 * t136 * t22 * t59 * t54
      t155 = FJET(XB1, XB2, s, t107 * t4, 0.0D0, -t107 * t109, t107 * t3
     # * t113, s * t115 * t11 * t118 * (-0.1D1 + x4), t154)
      t157 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t158 = s * t157
      t160 = sqrt(x3)
      t161 = -0.1D1 + t160
      t163 = t160 + 0.1D1
      t164 = x1 * t161 * t163
      t166 = t4 * x3
      t169 = t157 ** 2
      t176 = 0.1D1 / (-0.2D1 + t157)
      t177 = t169 ** 2
      t178 = t176 * t177
      t180 = t161 * t163
      t181 = Sqrt(-t180)
      t182 = t181 ** 2
      t194 = log(-0.4D1 * t67 * t32 * t27 * t65 * t180 * t177)
      t209 = 0.32D2 / 0.9D1 * t58 * t178 * t22 * t182 * t59 * t54 - 0.16
     #D2 / 0.405D3 * (0.90D2 * t58 * t22 * t194 * t178 * t182 + 0.180D3 
     #* t58 * lh * t176 * t177 * t22 * t182) * t59
      t210 = FJET(XB1, XB2, s, -t158 * t3 * t164, t158 * t166, 0.0D0, -t
     #158 * t7, s * t169 * t11 * t118 * (-0.1D1 + x3), t209)
      t212 = KAPPA2(x1, x2, x3, -t105, z)
      t213 = s * t212
      t214 = t213 * t3
      t219 = t212 ** 2
      t226 = Sqrt(t180 * t105)
      t237 = (t160 * t102 - 0.2D1 * t21 * t226) ** 2
      t238 = t219 ** 2
      t243 = 0.1D1 / (-0.2D1 + t212) * t59 * t54
      t247 = FJET(XB1, XB2, s, -t214 * t164, t213 * t166, -t213 * t109, 
     #t214 * t113, s * t219 * t11 * t118 * (x3 - 0.1D1 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t21 * t160 * t102 * t226), -0.8D1 / 0.9D1 * t58 * 
     #t237 * t238 * t243)
      rrqqbar2qqbarht6s1e0 = t100 * t99 + t155 * t154 + t210 * t209 - 0.
     #8D1 / 0.9D1 * t247 * t15 * t118 * t17 * wd * t237 * t238 * t243

      end function



      doubleprecision function rrqqbar2qqbarht6s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = t9 ** 2
      t27 = t25 * t26
      t28 = 0.1D1 / x3
      t34 = sin(t20)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = t11 ** 2
      t41 = x1 ** 2
      t42 = t6 ** 2
      t47 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t26)
      t57 = 0.1D1 / x4
      t62 = -0.32D2 / 0.9D1 * t19 * t23 * t27 * t28 - 0.16D2 / 0.405D3 *
     # (-0.180D3 * lh - 0.90D2 * t47) * wd * t16 * t18 * t22 * t25 * t26
     # - 0.32D2 / 0.9D1 * t19 * t23 * t27 * t57
      t63 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 *
     # t11 * x1 * t6, t62)
      t65 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t66 = s * t65
      t68 = sqrt(x3)
      t69 = -0.1D1 + t68
      t71 = t68 + 0.1D1
      t77 = t65 ** 2
      t80 = x1 * t6
      t85 = t16 * t18 * wd
      t87 = 0.1D1 / (-0.2D1 + t65)
      t89 = t77 ** 2
      t91 = Sqrt(-t69 * t71)
      t92 = t91 ** 2
      t94 = t89 * t92 * t28
      t98 = FJET(XB1, XB2, s, -t66 * t3 * x1 * t69 * t71, t66 * t4 * x3,
     # 0.0D0, -t66 * t7, s * t77 * t11 * t80 * (-0.1D1 + x3), 0.32D2 / 0
     #.9D1 * t85 * t22 * t87 * t94)
      t100 = t80 * t17
      t106 = sqrt(x4)
      t107 = -0.1D1 + t106
      t108 = t106 + 0.1D1
      t109 = t107 * t108
      t110 = KAPPA2(x1, x2, 0.0D0, -t109, z)
      t111 = s * t110
      t119 = t110 ** 2
      t126 = 0.1D1 / (-0.2D1 + t110)
      t128 = t119 ** 2
      t129 = Sqrt(-t109)
      t130 = t129 ** 2
      t132 = t128 * t130 * t57
      t136 = FJET(XB1, XB2, s, t111 * t4, 0.0D0, -t111 * t7 * x4, t111 *
     # t3 * t6 * t107 * t108, s * t119 * t11 * t80 * (-0.1D1 + x4), 0.32
     #D2 / 0.9D1 * t85 * t22 * t126 * t132)
      rrqqbar2qqbarht6s1em1 = t63 * t62 + 0.32D2 / 0.9D1 * t98 * t15 * t
     #100 * t23 * t87 * t94 + 0.32D2 / 0.9D1 * t136 * t15 * t100 * t23 *
     # t126 * t132

      end function



      doubleprecision function rrqqbar2qqbarht6s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t17 = 0.1D1 / z
      t21 = cos(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = t9 ** 2
      t31 = FJET(XB1, XB2, s, t2 * t3 * x1, 0.0D0, 0.0D0, -t2 * t3 * t6,
     # -s * t9 * t11 * x1 * t6, -0.32D2 / 0.9D1 * t15 * x1 * t6 * t17 * 
     #wd * t22 * t25 * t26)
      rrqqbar2qqbarht6s1em2 = -0.32D2 / 0.9D1 * t31 * t15 * x1 * t6 * t1
     #7 * wd * t22 * t25 * t26

      end function



      doubleprecision function rrqqbar2qqbarht6s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2qqbarht6s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht6s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2qqbarht6s1em4 = 0.0D0

      end function
