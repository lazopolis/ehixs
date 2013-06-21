  
      subroutine bbarqqbarH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarqqbarH2n1e1  
      doubleprecision bbarqqbarH2n1e0  
      doubleprecision bbarqqbarH2n1em1  
      doubleprecision bbarqqbarH2n1em2  
      doubleprecision bbarqqbarH2n1em3  
      doubleprecision bbarqqbarH2n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarqqbarH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = t8 * t10
      t12 = x3 * t4
      t15 = log(-0.4D1 * t11 * t12)
      t17 = lh ** 2
      t19 = 0.3141592653589793D1 ** 2
      t21 = 0.180D3 * t17 - 0.30D2 * t19
      t23 = t15 ** 2
      t38 = x3 * wd
      t41 = z * lh
      t42 = t41 * t1
      t45 = t10 * x3 * t4
      t48 = log(-0.4D1 * x2 * t8 * t45)
      t53 = t1 * z
      t54 = t48 ** 2
      t59 = z * t21
      t60 = t1 * wd
      t61 = t60 * x3
      t62 = t59 * t61
      t65 = 0.1D1 / x2
      t68 = x1 ** 2
      t73 = log(-0.4D1 * x2 * t68 * t8 * t45)
      t79 = 0.360D3 * t41 * t61
      t82 = 0.1D1 / x1
      t85 = t68 * t8
      t88 = log(-0.4D1 * t85 * t45)
      t92 = t88 ** 2
      t99 = -(-t15 * z * t21 - 0.90D2 * t23 * z * lh + z * (0.60D2 * lh 
     #* t19 - 0.2884936567583026D3 - 0.120D3 * t17 * lh) - 0.15D2 * t23 
     #* t15 * z) * t1 * t38 / 0.180D3 - (0.360D3 * t42 * wd * t48 * x3 +
     # 0.90D2 * t53 * wd * t54 * x3 + 0.2D1 * t62) * t65 / 0.360D3 - (-0
     #.180D3 * t53 * wd * t73 * x3 - t79) * t65 * t82 / 0.180D3 - (0.180
     #D3 * t42 * t38 * t88 + 0.45D2 * t53 * t38 * t92 + t62) * t82 / 0.9
     #0D2
      t100 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t99)
      t102 = x2 * x3
      t103 = 0.2D1 * t102
      t104 = cos(t6)
      t105 = -0.1D1 + x2
      t107 = x2 * t105 * t12
      t108 = Sqrt(t107)
      t110 = 0.2D1 * t104 * t108
      t112 = t2 * (-x3 + t103 - x2 + t110)
      t114 = t2 * (0.1D1 - x2 - x3 + t103 + t110)
      t115 = x2 * z
      t117 = (-x2 + 0.1D1 + t115) ** 2
      t118 = 0.1D1 / t117
      t119 = x2 - t115
      t120 = t118 * t119
      t122 = t105 * x3
      t126 = log(0.4D1 * t11 * x2 * t122 * t4)
      t127 = t126 * t118
      t128 = 0.2D1 * x3
      t129 = -x2 + t115 + t128
      t136 = t126 ** 2
      t146 = wd * t118 * t129
      t151 = t85 * t10
      t154 = log(0.4D1 * t151 * t107)
      t167 = -(-0.180D3 * t41 * t60 * (-t120 + t127 * t129) + 0.90D2 * t
     #53 * wd * (t127 * t119 - t136 * t118 * t129 / 0.2D1) - t59 * t1 * 
     #t146) * t65 / 0.360D3 - (0.90D2 * t53 * wd * (-t120 + t154 * t118 
     #* t129) + 0.180D3 * t42 * t146) * t65 * t82 / 0.180D3
      t168 = FJET(XB1, XB2, s, 0.0D0, -t112, 0.0D0, t114, 0.0D0, t167)
      t170 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t99)
      t172 = x3 * x1
      t173 = t2 * t172
      t174 = -0.1D1 + x1
      t175 = t172 * z
      t176 = t102 * x1
      t177 = x1 * z
      t178 = t102 * t177
      t179 = 0.1D1 - x1 + t177
      t183 = Sqrt(t122 * t179 * x2 * t4)
      t185 = 0.2D1 * t104 * t183
      t188 = 0.1D1 / t179
      t190 = t2 * t174 * (-x3 + t172 - t175 + t103 - t176 + t178 - x2 + 
     #t185) * t188
      t192 = t2 * x1 * t4
      t193 = x2 * x1
      t194 = t193 * z
      t195 = 0.1D1 - x1 + t177 - x2 + t193 - t194 - x3 + t172 - t175 + t
     #103 - t176 + t178 + t185
      t198 = t2 * t174 * t195 * t188
      t199 = t1 ** 2
      t204 = s * t199 * x2 * x1 * t174 * t188
      t206 = t85 * t10 * x2
      t208 = t174 ** 2
      t213 = log(0.4D1 * t206 * t122 * t4 * t188 * t208)
      t216 = (x2 - t193 - 0.1D1 + x1 - t177 - t115 + t194) ** 2
      t217 = 0.1D1 / t216
      t221 = t217 * (x2 - t193 - t115 + t194 - t128 + 0.2D1 * t172 - 0.2
     #D1 * t175)
      t234 = 0.90D2 * t53 * wd * (t213 * t179 * t221 - t179 * t217 * (-x
     #2 + t193 + t115 - t194)) + 0.180D3 * t42 * wd * t179 * t221
      t237 = t234 * t65 * t82 / 0.180D3
      t238 = FJET(XB1, XB2, s, t173, t190, -t192, -t198, -t204, -t237)
      t240 = t65 * t82
      t244 = t2 * t174 * x3
      t246 = t2 * t174 * t4
      t248 = t12 * t188 * t208
      t251 = log(-0.4D1 * t206 * t248)
      t262 = log(-0.4D1 * t151 * t248)
      t266 = t262 ** 2
      t273 = -(0.180D3 * t53 * wd * t251 * x3 + t79) * t65 * t82 / 0.180
     #D3 - (-0.180D3 * t42 * t38 * t262 - 0.45D2 * t53 * t38 * t266 - t6
     #2) * t82 / 0.90D2
      t274 = FJET(XB1, XB2, s, t173, -t244, -t192, t246, 0.0D0, t273)
      t276 = FJET(XB1, XB2, s, t190, t173, -t198, -t192, -t204, -t237)
      t280 = FJET(XB1, XB2, s, -t112, 0.0D0, t114, 0.0D0, 0.0D0, t167)
      t282 = FJET(XB1, XB2, s, -t244, t173, t246, -t192, 0.0D0, t273)
      bbarqqbarH2n1e1 = t100 * t99 + t168 * t167 + t170 * t99 - t238 * t
     #234 * t240 / 0.180D3 + t274 * t273 - t276 * t234 * t240 / 0.180D3 
     #+ t280 * t167 + t282 * t273

      end function



      doubleprecision function bbarqqbarH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = t1 * z
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t12 * x3 * t4
      t17 = log(-0.4D1 * x2 * t9 * t14)
      t22 = z * lh
      t23 = t1 * wd
      t25 = t22 * t23 * x3
      t28 = 0.1D1 / x2
      t33 = 0.1D1 / x1
      t35 = t6 * x3 * wd * t28 * t33
      t36 = x3 * wd
      t37 = x1 ** 2
      t38 = t37 * t9
      t41 = log(-0.4D1 * t38 * t14)
      t45 = 0.180D3 * t25
      t49 = t9 * t12
      t50 = x3 * t4
      t53 = log(-0.4D1 * t49 * t50)
      t57 = lh ** 2
      t59 = 0.3141592653589793D1 ** 2
      t63 = t53 ** 2
      t70 = -(-0.180D3 * t6 * wd * t17 * x3 - 0.360D3 * t25) * t28 / 0.3
     #60D3 - t35 - (-0.90D2 * t6 * t36 * t41 - t45) * t33 / 0.90D2 - (0.
     #180D3 * t53 * z * lh + z * (0.180D3 * t57 - 0.30D2 * t59) + 0.45D2
     # * t63 * z) * t1 * t36 / 0.180D3
      t71 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t70)
      t73 = x2 * x3
      t74 = 0.2D1 * t73
      t75 = cos(t7)
      t76 = -0.1D1 + x2
      t79 = Sqrt(x2 * t76 * t50)
      t81 = 0.2D1 * t75 * t79
      t83 = t2 * (-x3 + t74 - x2 + t81)
      t85 = t2 * (0.1D1 - x2 - x3 + t74 + t81)
      t86 = x2 * z
      t88 = (-x2 + 0.1D1 + t86) ** 2
      t89 = 0.1D1 / t88
      t93 = t76 * x3
      t97 = log(0.4D1 * t49 * x2 * t93 * t4)
      t99 = 0.2D1 * x3
      t100 = -x2 + t86 + t99
      t116 = t28 * t33
      t120 = -(0.90D2 * t6 * wd * (-t89 * (x2 - t86) + t97 * t89 * t100)
     # + 0.180D3 * t22 * t1 * wd * t89 * t100) * t28 / 0.360D3 + t6 * wd
     # * t89 * t100 * t116 / 0.2D1
      t121 = FJET(XB1, XB2, s, 0.0D0, -t83, 0.0D0, t85, 0.0D0, t120)
      t123 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t70)
      t125 = x3 * x1
      t126 = t2 * t125
      t127 = -0.1D1 + x1
      t128 = t125 * z
      t129 = t73 * x1
      t130 = x1 * z
      t131 = t73 * t130
      t132 = 0.1D1 - x1 + t130
      t136 = Sqrt(t93 * t132 * x2 * t4)
      t138 = 0.2D1 * t75 * t136
      t141 = 0.1D1 / t132
      t143 = t2 * t127 * (-x3 + t125 - t128 + t74 - t129 + t131 - x2 + t
     #138) * t141
      t145 = t2 * x1 * t4
      t146 = x2 * x1
      t147 = t146 * z
      t148 = 0.1D1 - x1 + t130 - x2 + t146 - t147 - x3 + t125 - t128 + t
     #74 - t129 + t131 + t138
      t151 = t2 * t127 * t148 * t141
      t152 = t1 ** 2
      t157 = s * t152 * x2 * x1 * t127 * t141
      t161 = (x2 - t146 - 0.1D1 + x1 - t130 - t86 + t147) ** 2
      t162 = 0.1D1 / t161
      t165 = x2 - t146 - t86 + t147 - t99 + 0.2D1 * t125 - 0.2D1 * t128
      t169 = t6 * wd * t132 * t162 * t165 * t116 / 0.2D1
      t170 = FJET(XB1, XB2, s, t126, t143, -t145, -t151, -t157, t169)
      t176 = t132 * t162 * t165 * t28 * t33
      t180 = t2 * t127 * x3
      t182 = t2 * t127 * t4
      t184 = t127 ** 2
      t189 = log(-0.4D1 * t38 * t12 * t50 * t141 * t184)
      t196 = t35 - (0.90D2 * t6 * t36 * t189 + t45) * t33 / 0.90D2
      t197 = FJET(XB1, XB2, s, t126, -t180, -t145, t182, 0.0D0, t196)
      t199 = FJET(XB1, XB2, s, t143, t126, -t151, -t145, -t157, t169)
      t204 = FJET(XB1, XB2, s, -t83, 0.0D0, t85, 0.0D0, 0.0D0, t120)
      t206 = FJET(XB1, XB2, s, -t180, t126, t182, -t145, 0.0D0, t196)
      bbarqqbarH2n1e0 = t71 * t70 + t121 * t120 + t123 * t70 + t170 * z 
     #* t23 * t176 / 0.2D1 + t197 * t196 + t199 * z * t23 * t176 / 0.2D1
     # + t204 * t120 + t206 * t196

      end function



      doubleprecision function bbarqqbarH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t14 = x3 * t4
      t17 = log(-0.4D1 * t10 / t11 * t14)
      t22 = x3 * wd
      t25 = t1 * z
      t26 = 0.1D1 / x2
      t31 = t22 / x1
      t32 = t25 * t31
      t33 = -(-0.180D3 * z * lh - 0.90D2 * t17 * z) * t1 * t22 / 0.180D3
     # - t25 * t22 * t26 / 0.2D1 - t32
      t34 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t33)
      t37 = 0.2D1 * x2 * x3
      t38 = cos(t8)
      t42 = Sqrt(x2 * (-0.1D1 + x2) * t14)
      t44 = 0.2D1 * t38 * t42
      t46 = t2 * (-x3 + t37 - x2 + t44)
      t48 = t2 * (0.1D1 - x2 - x3 + t37 + t44)
      t50 = x2 * z
      t52 = (-x2 + 0.1D1 + t50) ** 2
      t53 = 0.1D1 / t52
      t55 = -x2 + t50 + 0.2D1 * x3
      t59 = t25 * wd * t53 * t55 * t26 / 0.4D1
      t60 = FJET(XB1, XB2, s, 0.0D0, -t46, 0.0D0, t48, 0.0D0, t59)
      t65 = wd * t53 * t55 * t26
      t68 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t33)
      t71 = t2 * x1 * x3
      t72 = -0.1D1 + x1
      t74 = t2 * t72 * x3
      t76 = t2 * x1 * t4
      t78 = t2 * t72 * t4
      t79 = FJET(XB1, XB2, s, t71, -t74, -t76, t78, 0.0D0, t32)
      t83 = FJET(XB1, XB2, s, -t46, 0.0D0, t48, 0.0D0, 0.0D0, t59)
      t88 = FJET(XB1, XB2, s, -t74, t71, t78, -t76, 0.0D0, t32)
      bbarqqbarH2n1em1 = t34 * t33 + t60 * z * t1 * t65 / 0.4D1 + t68 * 
     #t33 + t79 * z * t1 * t31 + t83 * z * t1 * t65 / 0.4D1 + t88 * z * 
     #t1 * t31

      end function



      doubleprecision function bbarqqbarH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x3
      t5 = t2 * (-0.1D1 + x3)
      t9 = z * t1 * x3 * wd / 0.2D1
      t10 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t9)
      t13 = t1 * x3 * wd
      t15 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t9)
      bbarqqbarH2n1em2 = -t10 * z * t13 / 0.2D1 - t15 * z * t13 / 0.2D1

      end function



      doubleprecision function bbarqqbarH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarqqbarH2n1em3 = 0.0D0

      end function



      doubleprecision function bbarqqbarH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarqqbarH2n1em4 = 0.0D0

      end function
