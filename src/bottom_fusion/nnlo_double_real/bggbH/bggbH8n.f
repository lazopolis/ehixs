  
      subroutine bggbH8n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH8n1e1  
      doubleprecision bggbH8n1e0  
      doubleprecision bggbH8n1em1  
      doubleprecision bggbH8n1em2  
      doubleprecision bggbH8n1em3  
      doubleprecision bggbH8n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH8n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH8n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH8n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH8n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH8n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH8n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH8n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t11 * t4
      t15 = log(-0.4D1 * x2 * t8 * t12)
      t16 = t15 * z
      t19 = lh ** 2
      t21 = 0.3141592653589793D1 ** 2
      t23 = 0.180D3 * t19 - 0.30D2 * t21
      t24 = z * t23
      t25 = t15 ** 2
      t26 = t25 * z
      t29 = t1 ** 2
      t31 = (0.180D3 * t16 * lh + t24 + 0.45D2 * t26) * t29 * wd
      t48 = x2 * z
      t50 = (0.1D1 - x2 + t48) ** 2
      t51 = 0.1D1 / t50
      t52 = (-t31 + (-t16 * t23 - 0.90D2 * t26 * lh + z * (-0.2884936567
     #583026D3 - 0.120D3 * t19 * lh + 0.60D2 * lh * t21) - 0.15D2 * t25 
     #* t15 * z) * t29 * wd) * t51
      t54 = z * t29
      t55 = t54 * wd
      t57 = z * lh
      t58 = t29 * wd
      t60 = 0.180D3 * t57 * t58
      t61 = -0.90D2 * t55 - t60
      t62 = t61 * t51
      t63 = x2 * x3
      t64 = t8 * t11
      t65 = t64 * t4
      t68 = log(-0.4D1 * t63 * t65)
      t71 = wd * t51
      t72 = t68 ** 2
      t77 = t60 + t24 * t58
      t78 = t77 * t51
      t79 = 0.9D1 * t78
      t81 = 0.1D1 / x3
      t84 = x1 ** 2
      t85 = t63 * t84
      t88 = log(-0.4D1 * t85 * t65)
      t95 = 0.1D1 / x1
      t99 = x2 * t84
      t102 = log(-0.4D1 * t99 * t65)
      t106 = t102 ** 2
      t114 = t52 / 0.30D2 + (-0.9D1 * t62 * t68 + 0.405D3 * t54 * t71 * 
     #t72 + t79) * t81 / 0.270D3 - (0.810D3 * t54 * wd * t88 * t51 - 0.9
     #D1 * t62) * t95 * t81 / 0.135D3 - (0.9D1 * t61 * t102 * t51 - 0.40
     #5D3 * t54 * wd * t106 * t51 - t79) * t95 / 0.135D3
      t115 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t114)
      t117 = 0.2D1 * t63
      t118 = cos(t6)
      t120 = -0.1D1 + x3
      t123 = Sqrt(x2 * t4 * x3 * t120)
      t125 = 0.2D1 * t118 * t123
      t127 = t2 * (0.1D1 - x2 - x3 + t117 + t125)
      t129 = t2 * (-x3 + t117 - x2 + t125)
      t134 = log(0.4D1 * t63 * t8 * t12 * t120)
      t135 = t134 * t120
      t140 = t134 ** 2
      t156 = log(0.4D1 * t85 * t64 * t4 * t120)
      t158 = t156 * t51 * t120
      t164 = t62 * t120
      t170 = (t62 * (-0.2D1 * t135 + 0.2D1 - 0.2D1 * x3) + 0.90D2 * t54 
     #* t71 * (0.2D1 * t135 + t140 * t120) + 0.2D1 * t78 * t120) * t81 /
     # 0.270D3 - (0.90D2 * t54 * wd * (0.2D1 * t51 * t120 + 0.2D1 * t158
     #) - 0.2D1 * t164) * t95 * t81 / 0.135D3
      t171 = FJET(XB1, XB2, s, t127, 0.0D0, -t129, 0.0D0, 0.0D0, t170)
      t174 = -0.1D1 + x1
      t176 = t4 * s * t1 * t174
      t177 = t2 * x1
      t179 = x1 * z
      t180 = 0.1D1 - x1 + t179
      t181 = 0.1D1 / t180
      t183 = t2 * t174 * x2 * t181
      t188 = s * t29 * x2 * t174 * x1 * t181
      t190 = t63 * t84 * t8
      t191 = t11 * t181
      t192 = t174 ** 2
      t193 = t192 * t4
      t194 = t191 * t193
      t197 = log(-0.4D1 * t190 * t194)
      t199 = x2 * x1
      t200 = t199 * z
      t202 = (-0.1D1 + x1 - t179 + x2 - t199 - t48 + t200) ** 2
      t203 = 0.1D1 / t202
      t204 = t203 * t180
      t205 = t197 * t174 * t204
      t206 = t174 * t203
      t207 = t206 * t180
      t213 = t61 * t174
      t214 = t213 * t204
      t222 = log(-0.4D1 * t99 * t8 * t194)
      t224 = t222 * t174 * t204
      t228 = t222 ** 2
      t230 = t228 * t174 * t204
      t237 = t77 * t174 * t204
      t242 = -(0.90D2 * t54 * wd * (0.2D1 * t205 + 0.2D1 * t207) - 0.2D1
     # * t214) * t95 * t81 / 0.135D3 - (t61 * (0.2D1 * t207 + 0.2D1 * t2
     #24) + 0.90D2 * t54 * wd * (-t230 - 0.2D1 * t224) - 0.2D1 * t237) *
     # t95 / 0.135D3
      t243 = FJET(XB1, XB2, s, t176, t177, -t183, 0.0D0, -t188, t242)
      t245 = x1 * x3
      t246 = t245 * z
      t247 = t63 * x1
      t248 = t63 * t179
      t253 = Sqrt(x3 * t4 * t180 * x2 * t120)
      t255 = 0.2D1 * t118 * t253
      t259 = t2 * t174 * (-x3 + t245 - t246 + t117 - t247 + t248 - x2 + 
     #t255) * t181
      t260 = t2 * t245
      t261 = 0.1D1 - x1 + t179 - x2 + t199 - t200 - x3 + t245 - t246 + t
     #117 - t247 + t248 + t255
      t264 = t2 * t174 * t261 * t181
      t267 = t120 * s * t1 * x1
      t272 = log(0.4D1 * t190 * t191 * t193 * t120)
      t275 = t180 * t120
      t281 = t204 * t120
      t283 = 0.810D3 * t54 * wd * t272 * t206 * t275 - 0.9D1 * t61 * t17
     #4 * t281
      t287 = FJET(XB1, XB2, s, t259, t260, -t264, -t267, -t188, -t283 * 
     #t95 * t81 / 0.135D3)
      t289 = t95 * t81
      t301 = 0.2D1 * t78
      t324 = t102 * t51
      t337 = t52 / 0.135D3 + (t62 * (-0.2D1 * t68 - 0.2D1) + 0.90D2 * t5
     #4 * t71 * (0.2D1 * t68 + t72) + t301) * t81 / 0.270D3 - (-(-0.180D
     #3 * t57 - 0.90D2 * t16) * t29 * wd + t31) * t51 / 0.135D3 - (0.90D
     #2 * t54 * wd * (0.2D1 * t51 + 0.2D1 * t88 * t51) - 0.2D1 * t62) * 
     #t95 * t81 / 0.135D3 - (t61 * (0.2D1 * t51 + 0.2D1 * t324) + 0.90D2
     # * t54 * wd * (-t106 * t51 - 0.2D1 * t324) - t301) * t95 / 0.135D3
      t338 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t337)
      t358 = -(0.810D3 * t55 * t158 - 0.9D1 * t164) * t95 * t81 / 0.135D
     #3 + (-0.9D1 * t62 * t135 + 0.405D3 * t55 * t51 * t140 * t120 + 0.9
     #D1 * t78 * t120) * t81 / 0.270D3
      t359 = FJET(XB1, XB2, s, -t129, 0.0D0, t127, 0.0D0, 0.0D0, t358)
      t376 = -(0.810D3 * t55 * t205 - 0.9D1 * t214) * t95 * t81 / 0.135D
     #3 - (0.9D1 * t61 * t222 * t207 - 0.405D3 * t55 * t230 - 0.9D1 * t2
     #37) * t95 / 0.135D3
      t377 = FJET(XB1, XB2, s, -t183, 0.0D0, t176, t177, -t188, t376)
      t388 = -0.90D2 * t55 * (0.2D1 - 0.2D1 * x1 - 0.2D1 * t272 * t174) 
     #* t203 * t275 - 0.2D1 * t213 * t281
      t392 = FJET(XB1, XB2, s, -t264, -t267, t259, t260, -t188, -t388 * 
     #t95 * t81 / 0.135D3)
      bggbH8n1e1 = t115 * t114 + t171 * t170 + t243 * t242 - t287 * t283
     # * t289 / 0.135D3 + t338 * t337 + t359 * t358 + t377 * t376 - t392
     # * t388 * t289 / 0.135D3

      end function



      doubleprecision function bggbH8n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = z * t6
      t8 = t7 * wd
      t9 = x2 * z
      t11 = (0.1D1 - x2 + t9) ** 2
      t12 = 0.1D1 / t11
      t13 = 0.1D1 / x1
      t15 = 0.1D1 / x3
      t17 = t8 * t12 * t13 * t15
      t19 = x1 ** 2
      t20 = x2 * t19
      t21 = x4 * 0.3141592653589793D1
      t22 = Sin(t21)
      t23 = t22 ** 2
      t24 = z ** 2
      t25 = 0.1D1 / t24
      t27 = t23 * t25 * t4
      t30 = log(-0.4D1 * t20 * t27)
      t35 = 0.90D2 * t8
      t36 = z * lh
      t37 = t6 * wd
      t40 = -t35 - 0.180D3 * t36 * t37
      t41 = t40 * t12
      t42 = 0.9D1 * t41
      t46 = wd * t12
      t47 = x2 * x3
      t50 = log(-0.4D1 * t47 * t27)
      t59 = t25 * t4
      t62 = log(-0.4D1 * x2 * t23 * t59)
      t63 = t62 * z
      t67 = (-0.180D3 * t36 - 0.90D2 * t63) * t6 * wd
      t70 = lh ** 2
      t72 = 0.3141592653589793D1 ** 2
      t76 = t62 ** 2
      t83 = (-t67 + (0.180D3 * t63 * lh + z * (0.180D3 * t70 - 0.30D2 * 
     #t72) + 0.45D2 * t76 * z) * t6 * wd) * t12
      t85 = 0.6D1 * t17 - (0.810D3 * t7 * wd * t30 * t12 - t42) * t13 / 
     #0.135D3 + (-0.810D3 * t7 * t46 * t50 + t42) * t15 / 0.270D3 + t83 
     #/ 0.30D2
      t86 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t85)
      t88 = 0.2D1 * t47
      t89 = cos(t21)
      t91 = -0.1D1 + x3
      t94 = Sqrt(x2 * t4 * x3 * t91)
      t96 = 0.2D1 * t89 * t94
      t98 = t2 * (0.1D1 - x2 - x3 + t88 + t96)
      t100 = t2 * (-x3 + t88 - x2 + t96)
      t102 = t13 * t15
      t104 = t8 * t12 * t91 * t102
      t110 = log(0.4D1 * t47 * t23 * t59 * t91)
      t122 = 0.4D1 / 0.3D1 * t104 + (0.90D2 * t7 * t46 * (-0.2D1 * t110 
     #* t91 + 0.2D1 - 0.2D1 * x3) + 0.2D1 * t41 * t91) * t15 / 0.270D3
      t123 = FJET(XB1, XB2, s, t98, 0.0D0, -t100, 0.0D0, 0.0D0, t122)
      t126 = -0.1D1 + x1
      t128 = t4 * s * t1 * t126
      t129 = t2 * x1
      t131 = x1 * z
      t132 = 0.1D1 - x1 + t131
      t133 = 0.1D1 / t132
      t135 = t2 * t126 * x2 * t133
      t140 = s * t6 * x2 * t126 * x1 * t133
      t142 = t7 * wd * t126
      t143 = x2 * x1
      t144 = t143 * z
      t146 = (-0.1D1 + x1 - t131 + x2 - t143 - t9 + t144) ** 2
      t147 = 0.1D1 / t146
      t148 = t147 * t132
      t150 = t142 * t148 * t102
      t156 = t126 ** 2
      t161 = log(-0.4D1 * t20 * t23 * t25 * t133 * t156 * t4)
      t163 = t161 * t126 * t148
      t170 = t40 * t126 * t148
      t175 = 0.4D1 / 0.3D1 * t150 - (0.90D2 * t7 * wd * (0.2D1 * t126 * 
     #t147 * t132 + 0.2D1 * t163) - 0.2D1 * t170) * t13 / 0.135D3
      t176 = FJET(XB1, XB2, s, t128, t129, -t135, 0.0D0, -t140, t175)
      t178 = x1 * x3
      t179 = t178 * z
      t180 = t47 * x1
      t181 = t47 * t131
      t186 = Sqrt(x3 * t4 * t132 * x2 * t91)
      t188 = 0.2D1 * t89 * t186
      t192 = t2 * t126 * (-x3 + t178 - t179 + t88 - t180 + t181 - x2 + t
     #188) * t133
      t193 = t2 * t178
      t194 = 0.1D1 - x1 + t131 - x2 + t143 - t144 - x3 + t178 - t179 + t
     #88 - t180 + t181 + t188
      t197 = t2 * t126 * t194 * t133
      t200 = t91 * s * t1 * x1
      t201 = 0.9D1 * t126
      t206 = t148 * t91 * t13 * t15
      t209 = FJET(XB1, XB2, s, t192, t193, -t197, -t200, -t140, 0.2D1 / 
     #0.3D1 * t7 * wd * t201 * t206)
      t220 = 0.2D1 * t41
      t238 = (0.90D2 * t7 * t46 * (-0.2D1 * t50 - 0.2D1) + t220) * t15 /
     # 0.270D3 - (-t35 + t67) * t12 / 0.135D3 + t83 / 0.135D3 + 0.4D1 / 
     #0.3D1 * t17 - (0.90D2 * t7 * wd * (0.2D1 * t12 + 0.2D1 * t30 * t12
     #) - t220) * t13 / 0.135D3
      t239 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t238)
      t251 = (-0.810D3 * t8 * t12 * t110 * t91 + 0.9D1 * t41 * t91) * t1
     #5 / 0.270D3 + 0.6D1 * t104
      t252 = FJET(XB1, XB2, s, -t100, 0.0D0, t98, 0.0D0, 0.0D0, t251)
      t261 = 0.6D1 * t150 - (0.810D3 * t8 * t163 - 0.9D1 * t170) * t13 /
     # 0.135D3
      t262 = FJET(XB1, XB2, s, -t135, 0.0D0, t128, t129, -t140, t261)
      t266 = FJET(XB1, XB2, s, -t197, -t200, t192, t193, -t140, 0.4D1 / 
     #0.3D1 * t142 * t206)
      bggbH8n1e0 = t86 * t85 + t123 * t122 + t176 * t175 + 0.2D1 / 0.3D1
     # * t209 * z * t37 * t201 * t206 + t239 * t238 + t252 * t251 + t262
     # * t261 + 0.4D1 / 0.3D1 * t266 * z * t37 * t126 * t206

      end function



      doubleprecision function bggbH8n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = z * t6
      t8 = t7 * wd
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t21 = log(-0.4D1 * x2 * t14 / t16 * t4)
      t28 = x2 * z
      t30 = (0.1D1 - x2 + t28) ** 2
      t31 = 0.1D1 / t30
      t32 = (-0.90D2 * t8 + (-0.180D3 * z * lh - 0.90D2 * t21 * z) * t6 
     #* wd) * t31
      t34 = wd * t31
      t35 = 0.1D1 / x3
      t37 = t7 * t34 * t35
      t39 = 0.1D1 / x1
      t41 = t7 * t34 * t39
      t43 = t32 / 0.30D2 + 0.3D1 * t37 + 0.6D1 * t41
      t44 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t43)
      t47 = 0.2D1 * x2 * x3
      t48 = cos(t12)
      t50 = -0.1D1 + x3
      t53 = Sqrt(x2 * t4 * x3 * t50)
      t55 = 0.2D1 * t48 * t53
      t57 = t2 * (0.1D1 - x2 - x3 + t47 + t55)
      t59 = t2 * (-x3 + t47 - x2 + t55)
      t60 = 0.2D1 * t50
      t65 = FJET(XB1, XB2, s, t57, 0.0D0, -t59, 0.0D0, 0.0D0, t8 * t31 *
     # t60 * t35 / 0.3D1)
      t73 = -0.1D1 + x1
      t75 = t4 * s * t1 * t73
      t76 = t2 * x1
      t78 = x1 * z
      t79 = 0.1D1 - x1 + t78
      t80 = 0.1D1 / t79
      t82 = t2 * t73 * x2 * t80
      t87 = s * t6 * x2 * t73 * x1 * t80
      t88 = x2 * x1
      t91 = (-0.1D1 + x1 - t78 + x2 - t88 - t28 + t88 * z) ** 2
      t95 = t73 / t91 * t79 * t39
      t96 = t8 * t95
      t98 = FJET(XB1, XB2, s, t75, t76, -t82, 0.0D0, -t87, 0.4D1 / 0.3D1
     # * t96)
      t100 = t6 * wd
      t109 = t32 / 0.135D3 + 0.2D1 / 0.3D1 * t37 - 0.2D1 / 0.3D1 * t7 * 
     #t34 + 0.4D1 / 0.3D1 * t41
      t110 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t109)
      t113 = FJET(XB1, XB2, s, -t82, 0.0D0, t75, t76, -t87, 0.6D1 * t96)
      t122 = FJET(XB1, XB2, s, -t59, 0.0D0, t57, 0.0D0, 0.0D0, 0.3D1 * t
     #8 * t31 * t50 * t35)
      bggbH8n1em1 = t44 * t43 + t65 * z * t6 * t34 * t60 * t35 / 0.3D1 +
     # 0.4D1 / 0.3D1 * t98 * z * t100 * t95 + t110 * t109 + 0.6D1 * t113
     # * z * t100 * t95 + 0.3D1 * t122 * z * t6 * t34 * t50 * t35

      end function



      doubleprecision function bggbH8n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t5 = t2 * (-0.1D1 + x2)
      t6 = t1 ** 2
      t10 = (0.1D1 - x2 + x2 * z) ** 2
      t11 = 0.1D1 / t10
      t13 = z * t6 * wd * t11
      t15 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, 0.3D1 * t13)
      t18 = t6 * wd * t11
      t22 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, 0.2D1 / 0.3D
     #1 * t13)
      bggbH8n1em2 = 0.3D1 * t15 * z * t18 + 0.2D1 / 0.3D1 * t22 * z * t1
     #8

      end function



      doubleprecision function bggbH8n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bggbH8n1em3 = 0.0D0

      end function



      doubleprecision function bggbH8n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bggbH8n1em4 = 0.0D0

      end function
