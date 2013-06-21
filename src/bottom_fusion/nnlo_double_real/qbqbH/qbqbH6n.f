  
      subroutine qbqbH6n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision qbqbH6n1e1  
      doubleprecision qbqbH6n1e0  
      doubleprecision qbqbH6n1em1  
      doubleprecision qbqbH6n1em2  
      doubleprecision qbqbH6n1em3  
      doubleprecision qbqbH6n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=qbqbH6n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=qbqbH6n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=qbqbH6n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=qbqbH6n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=qbqbH6n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=qbqbH6n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function qbqbH6n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = t8 * t10
      t12 = t4 * x2
      t15 = log(-0.4D1 * t11 * t12)
      t17 = x2 * z
      t19 = (0.1D1 - x2 + t17) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t22 = lh ** 2
      t24 = 0.3141592653589793D1 ** 2
      t26 = 0.180D3 * t22 - 0.30D2 * t24
      t29 = t15 ** 2
      t46 = t1 ** 2
      t47 = t46 ** 2
      t49 = cos(t6)
      t50 = t49 ** 2
      t51 = z * t50
      t52 = Sqrt(-t12)
      t53 = t52 ** 2
      t57 = wd * lh
      t58 = t47 * z
      t59 = t57 * t58
      t61 = t10 * x2
      t62 = t61 * t4
      t65 = log(-0.4D1 * x3 * t8 * t62)
      t67 = t50 * t53
      t71 = wd * t47
      t72 = t71 * z
      t73 = t65 ** 2
      t78 = wd * t26
      t79 = t78 * t47
      t81 = z * t21 * t67
      t82 = t79 * t81
      t85 = 0.1D1 / x3
      t88 = x1 ** 2
      t89 = x3 * t88
      t93 = log(-0.4D1 * t89 * t8 * t62)
      t98 = t57 * t47
      t103 = 0.1D1 / x1
      t106 = t88 * t8
      t109 = log(-0.4D1 * t106 * t62)
      t111 = t21 * t53
      t115 = t109 ** 2
      t123 = -(-t15 * wd * t21 * t26 - 0.90D2 * t29 * wd * t21 * lh + wd
     # * t21 * (0.60D2 * lh * t24 - 0.2884936567583026D3 - 0.120D3 * t22
     # * lh) - 0.15D2 * t29 * t15 * wd * t21) * t47 * t51 * t53 / 0.45D2
     # + (-0.720D3 * t59 * t65 * t21 * t67 - 0.180D3 * t72 * t73 * t21 *
     # t67 - 0.4D1 * t82) * t85 / 0.180D3 + (0.360D3 * t72 * t93 * t21 *
     # t67 + 0.720D3 * t98 * t81) * t85 * t103 / 0.90D2 - 0.2D1 / 0.45D2
     # * (0.180D3 * t59 * t50 * t109 * t111 + 0.45D2 * t72 * t50 * t115 
     #* t111 + t82) * t103
      t124 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t
     #123)
      t126 = x2 * x3
      t127 = 0.2D1 * t126
      t128 = sqrt(x3)
      t129 = t49 * t128
      t130 = -0.1D1 + t128
      t131 = t128 + 0.1D1
      t132 = t130 * t131
      t134 = Sqrt(t12 * t132)
      t136 = 0.2D1 * t129 * t134
      t141 = x3 * t4
      t143 = t61 * t132
      t146 = log(0.4D1 * t141 * t8 * t143)
      t147 = 0.1D1 / t19
      t150 = 0.2D1 * t128 * x2
      t154 = (t150 - t128 + 0.2D1 * t49 * t134) ** 2
      t156 = 0.2D1 * t126 * z
      t157 = x3 * z
      t162 = (-0.1D1 - t17 + t156 - t136 - t157 - t127 + 0.2D1 * t129 * 
     #t134 * z + x3 + x2) ** 2
      t164 = t154 / t162
      t168 = t146 ** 2
      t174 = z * t147 * t164
      t183 = log(0.4D1 * t89 * t4 * t8 * t143)
      t194 = (0.180D3 * t59 * t146 * t147 * t164 + 0.45D2 * t72 * t168 *
     # t147 * t164 + t79 * t174) * t85 / 0.180D3 + (-0.90D2 * t72 * t183
     # * t147 * t164 - 0.180D3 * t98 * t174) * t85 * t103 / 0.90D2
      t195 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t127 - x2 + t136), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t127 + t136), 0.0D0, t194)
      t197 = -0.1D1 + x1
      t199 = x1 * z
      t200 = 0.1D1 - x1 + t199
      t201 = 0.1D1 / t200
      t212 = s * t46 * x2 * x1 * t197 * t201
      t214 = t197 ** 2
      t216 = t12 * t201 * t214
      t219 = log(-0.4D1 * t89 * t11 * t216)
      t222 = t4 * t200
      t224 = Sqrt(-t222 * x2)
      t225 = t224 ** 2
      t226 = x1 * x2
      t227 = t226 * z
      t229 = (-0.1D1 + x1 - t199 + x2 - t226 - t17 + t227) ** 2
      t230 = t229 ** 2
      t231 = 0.1D1 / t230
      t233 = t214 * t197
      t234 = t200 * t233
      t236 = t225 * t231 * t234 * t50
      t250 = log(-0.4D1 * t106 * t10 * t216)
      t253 = t231 * t200 * t233
      t258 = t250 ** 2
      t268 = (0.360D3 * t71 * z * t219 * t236 + 0.720D3 * t59 * t236) * 
     #t85 * t103 / 0.90D2 - 0.2D1 / 0.45D2 * (0.180D3 * t57 * t58 * t50 
     #* t250 * t225 * t253 + 0.45D2 * t71 * t51 * t258 * t225 * t253 + t
     #78 * t58 * t236) * t103
      t269 = FJET(XB1, XB2, s, 0.0D0, -t2 * t197 * x2 * t201, x1 * t1 * 
     #s, t4 * t197 * t2, -t212, t268)
      t271 = x3 * x1
      t273 = t271 * z
      t274 = t271 * x2
      t275 = t271 * t17
      t279 = Sqrt(t222 * x2 * t130 * t131)
      t281 = 0.2D1 * t129 * t279
      t289 = 0.1D1 - x1 + t199 - x2 + t226 - t227 - x3 + t271 - t273 + t
     #127 - t274 + t275 + t281
      t302 = log(0.4D1 * t141 * t214 * t88 * t8 * t10 * t130 * t131 * x2
     # * t201)
      t306 = t128 * x1
      t315 = (0.2D1 * t306 * t17 - t306 * z + t150 - 0.2D1 * t306 * x2 -
     # t128 + t306 + 0.2D1 * t49 * t279) ** 2
      t317 = t279 * z
      t323 = t88 * x2
      t330 = 0.1D1 - x1 - x2 - x3 + t199 - t227 - 0.2D1 * t129 * t317 + 
     #0.2D1 * t129 * t317 * x1 + t157 + t323 * x3 * t9 - t271 * x2 * t9 
     #- 0.2D1 * t323 * t157
      t339 = 0.4D1 * t275 - 0.2D1 * t273 - 0.3D1 * t274 + t271 * t9 + t3
     #23 * x3 + t271 + t127 + t17 - t156 + t281 - 0.2D1 * t129 * t279 * 
     #x1 + t226
      t341 = (t330 + t339) ** 2
      t344 = 0.1D1 / t229 * t315 * t234 / t341
      t349 = -0.90D2 * t71 * z * t302 * t344 - 0.180D3 * t59 * t344
      t353 = FJET(XB1, XB2, s, t2 * t271, t2 * t197 * (-x3 + t271 - t273
     # + t127 - t274 + t275 - x2 + t281) * t201, -t2 * x1 * t130 * t131,
     # -t2 * t197 * t289 * t201, -t212, t349 * t85 * t103 / 0.90D2)
      qbqbH6n1e1 = t124 * t123 + t195 * t194 + t269 * t268 + t353 * t349
     # * t85 * t103 / 0.90D2

      end function



      doubleprecision function qbqbH6n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = t1 ** 2
      t7 = t6 ** 2
      t8 = wd * t7
      t9 = t8 * z
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * x2
      t17 = t16 * t4
      t20 = log(-0.4D1 * x3 * t12 * t17)
      t21 = x2 * z
      t23 = (0.1D1 - x2 + t21) ** 2
      t24 = t23 ** 2
      t25 = 0.1D1 / t24
      t27 = cos(t10)
      t28 = t27 ** 2
      t29 = t4 * x2
      t30 = Sqrt(-t29)
      t31 = t30 ** 2
      t32 = t28 * t31
      t36 = wd * lh
      t37 = t36 * t7
      t38 = z * t25
      t40 = t37 * t38 * t32
      t43 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t48 = t43 * t47
      t52 = x1 ** 2
      t53 = t52 * t12
      t56 = log(-0.4D1 * t53 * t17)
      t69 = log(-0.4D1 * t12 * t15 * t29)
      t75 = lh ** 2
      t77 = 0.3141592653589793D1 ** 2
      t81 = t69 ** 2
      t87 = z * t28
      t91 = (0.360D3 * t9 * t20 * t25 * t32 + 0.720D3 * t40) * t43 / 0.1
     #80D3 - 0.4D1 * t8 * t38 * t32 * t48 - 0.2D1 / 0.45D2 * (-0.90D2 * 
     #t9 * t28 * t56 * t25 * t31 - 0.180D3 * t40) * t47 - (0.180D3 * t69
     # * wd * t25 * lh + wd * t25 * (0.180D3 * t75 - 0.30D2 * t77) + 0.4
     #5D2 * t81 * wd * t25) * t7 * t87 * t31 / 0.45D2
      t92 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t9
     #1)
      t94 = x2 * x3
      t95 = 0.2D1 * t94
      t96 = sqrt(x3)
      t97 = t27 * t96
      t98 = -0.1D1 + t96
      t99 = t96 + 0.1D1
      t100 = t98 * t99
      t102 = Sqrt(t29 * t100)
      t104 = 0.2D1 * t97 * t102
      t114 = log(0.4D1 * x3 * t4 * t12 * t16 * t100)
      t115 = 0.1D1 / t23
      t118 = 0.2D1 * t96 * x2
      t122 = (t118 - t96 + 0.2D1 * t27 * t102) ** 2
      t124 = 0.2D1 * t94 * z
      t125 = x3 * z
      t130 = (-0.1D1 - t21 + t124 - t104 - t125 - t95 + 0.2D1 * t97 * t1
     #02 * z + x3 + x2) ** 2
      t132 = t122 / t130
      t136 = z * t115
      t146 = (-0.90D2 * t9 * t114 * t115 * t132 - 0.180D3 * t37 * t136 *
     # t132) * t43 / 0.180D3 + t8 * t136 * t132 * t48
      t147 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t95 - x2 + t104), 0.0
     #D0, t2 * (0.1D1 - x2 - x3 + t95 + t104), 0.0D0, t146)
      t149 = -0.1D1 + x1
      t151 = x1 * z
      t152 = 0.1D1 - x1 + t151
      t153 = 0.1D1 / t152
      t164 = s * t6 * x2 * x1 * t149 * t153
      t165 = t4 * t152
      t167 = Sqrt(-t165 * x2)
      t168 = t167 ** 2
      t170 = x1 * x2
      t171 = t170 * z
      t173 = (-0.1D1 + x1 - t151 + x2 - t170 - t21 + t171) ** 2
      t174 = t173 ** 2
      t175 = 0.1D1 / t174
      t178 = t149 ** 2
      t179 = t178 * t149
      t180 = t152 * t179
      t192 = log(-0.4D1 * t53 * t15 * t29 * t153 * t178)
      t199 = t7 * z
      t209 = -0.4D1 * t8 * z * t168 * t175 * t180 * t28 * t43 * t47 - 0.
     #2D1 / 0.45D2 * (-0.90D2 * t8 * t87 * t192 * t168 * t175 * t152 * t
     #179 - 0.180D3 * t36 * t199 * t168 * t175 * t180 * t28) * t47
      t210 = FJET(XB1, XB2, s, 0.0D0, -t2 * t149 * x2 * t153, x1 * t1 * 
     #s, t4 * t149 * t2, -t164, t209)
      t212 = x3 * x1
      t214 = t212 * z
      t215 = t212 * x2
      t216 = t212 * t21
      t220 = Sqrt(t165 * x2 * t98 * t99)
      t222 = 0.2D1 * t97 * t220
      t230 = 0.1D1 - x1 + t151 - x2 + t170 - t171 - x3 + t212 - t214 + t
     #95 - t215 + t216 + t222
      t234 = 0.1D1 / t173
      t236 = t96 * x1
      t245 = (0.2D1 * t236 * t21 - t236 * z + t118 - 0.2D1 * t236 * x2 -
     # t96 + t236 + 0.2D1 * t27 * t220) ** 2
      t248 = t220 * z
      t254 = t52 * x2
      t261 = 0.1D1 - x1 - x2 - x3 + t151 - t171 - 0.2D1 * t97 * t248 + 0
     #.2D1 * t97 * t248 * x1 + t125 + t254 * x3 * t14 - t212 * x2 * t14 
     #- 0.2D1 * t254 * t125
      t270 = 0.4D1 * t216 - 0.2D1 * t214 - 0.3D1 * t215 + t212 * t14 + t
     #254 * x3 + t212 + t95 + t21 - t124 + t222 - 0.2D1 * t97 * t220 * x
     #1 + t170
      t272 = (t261 + t270) ** 2
      t275 = 0.1D1 / t272 * t43 * t47
      t278 = FJET(XB1, XB2, s, t2 * t212, t2 * t149 * (-x3 + t212 - t214
     # + t95 - t215 + t216 - x2 + t222) * t153, -t2 * x1 * t98 * t99, -t
     #2 * t149 * t230 * t153, -t164, t8 * z * t234 * t245 * t180 * t275)
      qbqbH6n1e0 = t92 * t91 + t147 * t146 + t210 * t209 + t278 * wd * t
     #199 * t234 * t245 * t152 * t179 * t275

      end function



      doubleprecision function qbqbH6n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = x2 * z
      t8 = (0.1D1 - x2 + t6) ** 2
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = z ** 2
      t20 = t4 * x2
      t23 = log(-0.4D1 * t16 / t17 * t20)
      t28 = t1 ** 2
      t29 = t28 ** 2
      t31 = cos(t14)
      t32 = t31 ** 2
      t33 = z * t32
      t34 = Sqrt(-t20)
      t35 = t34 ** 2
      t39 = wd * t29
      t40 = t39 * z
      t41 = t10 * t32
      t42 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t52 = -(-0.180D3 * wd * t10 * lh - 0.90D2 * t23 * wd * t10) * t29 
     #* t33 * t35 / 0.45D2 - 0.2D1 * t40 * t41 * t35 * t42 - 0.4D1 * t40
     # * t41 * t35 * t47
      t53 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t5
     #2)
      t55 = x2 * x3
      t56 = 0.2D1 * t55
      t57 = sqrt(x3)
      t58 = t31 * t57
      t63 = Sqrt(t20 * (-0.1D1 + t57) * (t57 + 0.1D1))
      t65 = 0.2D1 * t58 * t63
      t76 = (0.2D1 * t57 * x2 - t57 + 0.2D1 * t31 * t63) ** 2
      t85 = (-0.1D1 - t6 + 0.2D1 * t55 * z - t65 - x3 * z - t56 + 0.2D1 
     #* t58 * t63 * z + x3 + x2) ** 2
      t88 = 0.1D1 / t8 * t76 / t85 * t42
      t91 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t56 - x2 + t65), 0.0D0
     #, t2 * (0.1D1 - x2 - x3 + t56 + t65), 0.0D0, t40 * t88 / 0.2D1)
      t93 = t29 * z
      t97 = -0.1D1 + x1
      t99 = x1 * z
      t100 = 0.1D1 - x1 + t99
      t101 = 0.1D1 / t100
      t116 = Sqrt(-t4 * t100 * x2)
      t117 = t116 ** 2
      t118 = x1 * x2
      t121 = (-0.1D1 + x1 - t99 + x2 - t118 - t6 + t118 * z) ** 2
      t122 = t121 ** 2
      t125 = t97 ** 2
      t129 = t117 / t122 * t100 * t125 * t97 * t47
      t132 = FJET(XB1, XB2, s, 0.0D0, -t2 * t97 * x2 * t101, x1 * t1 * s
     #, t4 * t97 * t2, -s * t28 * x2 * x1 * t97 * t101, -0.4D1 * t39 * t
     #33 * t129)
      qbqbH6n1em1 = t53 * t52 + t91 * wd * t93 * t88 / 0.2D1 - 0.4D1 * t
     #132 * wd * t93 * t32 * t129

      end function



      doubleprecision function qbqbH6n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = t1 ** 2
      t7 = t6 ** 2
      t12 = (0.1D1 - x2 + x2 * z) ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t16 = cos(x4 * 0.3141592653589793D1)
      t17 = t16 ** 2
      t20 = Sqrt(-t4 * x2)
      t21 = t20 ** 2
      t25 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, -0
     #.2D1 * wd * t7 * z * t14 * t17 * t21)
      qbqbH6n1em2 = -0.2D1 * t25 * wd * t7 * z * t14 * t17 * t21

      end function



      doubleprecision function qbqbH6n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      qbqbH6n1em3 = 0.0D0

      end function



      doubleprecision function qbqbH6n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      qbqbH6n1em4 = 0.0D0

      end function
