  
      subroutine bqqbH6n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bqqbH6n1e1  
      doubleprecision bqqbH6n1e0  
      doubleprecision bqqbH6n1em1  
      doubleprecision bqqbH6n1em2  
      doubleprecision bqqbH6n1em3  
      doubleprecision bqqbH6n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bqqbH6n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bqqbH6n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bqqbH6n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bqqbH6n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bqqbH6n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bqqbH6n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bqqbH6n1e1
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
      t16 = cos(t6)
      t17 = t16 ** 2
      t19 = Sqrt(-t12)
      t20 = t19 ** 2
      t21 = lh ** 2
      t23 = 0.3141592653589793D1 ** 2
      t25 = 0.180D3 * t21 - 0.30D2 * t23
      t28 = t15 ** 2
      t46 = t1 ** 2
      t47 = t46 ** 2
      t48 = z * t47
      t49 = x2 * z
      t51 = (0.1D1 - x2 + t49) ** 2
      t52 = t51 ** 2
      t53 = 0.1D1 / t52
      t57 = lh * wd
      t58 = t57 * t48
      t60 = t10 * x2
      t61 = t60 * t4
      t64 = log(-0.4D1 * x3 * t8 * t61)
      t66 = t20 * t53
      t70 = wd * z
      t71 = t70 * t47
      t72 = t64 ** 2
      t78 = t25 * wd * z
      t80 = t47 * t17 * t66
      t81 = t78 * t80
      t84 = 0.1D1 / x3
      t87 = x1 ** 2
      t88 = x3 * t87
      t92 = log(-0.4D1 * t88 * t8 * t61)
      t97 = t57 * z
      t102 = 0.1D1 / x1
      t105 = t17 * lh
      t107 = t87 * t8
      t110 = log(-0.4D1 * t107 * t61)
      t115 = t17 * wd
      t117 = t110 ** 2
      t125 = -(-t15 * t17 * t20 * t25 - 0.90D2 * t28 * t17 * t20 * lh + 
     #t17 * t20 * (0.60D2 * lh * t23 - 0.2884936567583026D3 - 0.120D3 * 
     #t21 * lh) - 0.15D2 * t28 * t15 * t17 * t20) * wd * t48 * t53 / 0.4
     #5D2 + (-0.720D3 * t58 * t64 * t17 * t66 - 0.180D3 * t71 * t72 * t1
     #7 * t66 - 0.4D1 * t81) * t84 / 0.180D3 + (0.360D3 * t71 * t92 * t1
     #7 * t66 + 0.720D3 * t97 * t80) * t84 * t102 / 0.90D2 - 0.2D1 / 0.4
     #5D2 * (0.180D3 * t105 * t70 * t47 * t110 * t66 + 0.45D2 * t115 * z
     # * t47 * t117 * t66 + t81) * t102
      t126 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #125)
      t128 = -0.1D1 + x1
      t129 = x3 * x1
      t130 = t129 * z
      t131 = x2 * x3
      t132 = 0.2D1 * t131
      t133 = t129 * x2
      t134 = t129 * t49
      t135 = sqrt(x3)
      t136 = t16 * t135
      t137 = x1 * z
      t138 = 0.1D1 - x1 + t137
      t139 = t4 * t138
      t140 = -0.1D1 + t135
      t142 = t135 + 0.1D1
      t145 = Sqrt(t139 * x2 * t140 * t142)
      t147 = 0.2D1 * t136 * t145
      t150 = 0.1D1 / t138
      t154 = x1 * x2
      t155 = t154 * z
      t156 = 0.1D1 - x1 + t137 - x2 + t154 - t155 - x3 + t129 - t130 + t
     #132 - t133 + t134 + t147
      t167 = s * t46 * x2 * x1 * t128 * t150
      t168 = t4 * x3
      t169 = t128 ** 2
      t179 = log(0.4D1 * t168 * t169 * t87 * t8 * t10 * t140 * t142 * x2
     # * t150)
      t182 = t169 * t128
      t184 = (-0.1D1 + x1 - t137 + x2 - t154 - t49 + t155) ** 2
      t190 = t87 * x2
      t195 = x3 * z
      t199 = 0.1D1 - 0.2D1 * t136 * t145 * x1 - x1 - x2 - x3 + t137 + t1
     #47 - t155 + t190 * x3 * t9 - t129 * x2 * t9 - 0.2D1 * t190 * t195 
     #+ 0.4D1 * t134
      t200 = t145 * z
      t211 = 0.2D1 * t131 * z
      t212 = -0.2D1 * t136 * t200 + 0.2D1 * t136 * t200 * x1 + t195 - 0.
     #2D1 * t130 - 0.3D1 * t133 + t129 * t9 + t190 * x3 - t211 + t129 + 
     #t132 + t49 + t154
      t214 = (t199 + t212) ** 2
      t216 = t135 * x1
      t221 = 0.2D1 * t135 * x2
      t227 = (-t216 * z + 0.2D1 * t216 * t49 - t135 + t216 + t221 - 0.2D
     #1 * t216 * x2 + 0.2D1 * t16 * t145) ** 2
      t230 = t182 / t184 / t214 * t227 * t138
      t235 = -0.90D2 * t70 * t47 * t179 * t230 - 0.180D3 * t58 * t230
      t239 = FJET(XB1, XB2, s, t2 * t128 * (-x3 + t129 - t130 + t132 - t
     #133 + t134 - x2 + t147) * t150, t2 * t129, -t2 * t128 * t156 * t15
     #0, -t2 * x1 * t140 * t142, -t167, t235 * t84 * t102 / 0.90D2)
      t244 = t140 * t142
      t246 = Sqrt(t12 * t244)
      t248 = 0.2D1 * t136 * t246
      t254 = t60 * t244
      t257 = log(0.4D1 * t168 * t8 * t254)
      t258 = 0.1D1 / t51
      t263 = (-t135 + t221 + 0.2D1 * t16 * t246) ** 2
      t268 = (-0.1D1 - t49 + t211 - t248 + 0.2D1 * t136 * t246 * z - t19
     #5 + x3 - t132 + x2) ** 2
      t270 = t263 / t268
      t274 = t257 ** 2
      t280 = t47 * t258 * t270
      t289 = log(0.4D1 * t88 * t4 * t8 * t254)
      t300 = (0.180D3 * t58 * t257 * t258 * t270 + 0.45D2 * t71 * t274 *
     # t258 * t270 + t78 * t280) * t84 / 0.180D3 + (-0.90D2 * t71 * t289
     # * t258 * t270 - 0.180D3 * t97 * t280) * t84 * t102 / 0.90D2
      t301 = FJET(XB1, XB2, s, -t2 * (-x3 + t132 - x2 + t248), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t132 + t248), 0.0D0, 0.0D0, t300)
      t312 = t12 * t150 * t169
      t315 = log(-0.4D1 * t88 * t11 * t312)
      t319 = t184 ** 2
      t322 = Sqrt(-t139 * x2)
      t323 = t322 ** 2
      t325 = 0.1D1 / t319 * t323 * t138
      t326 = t17 * t182 * t325
      t339 = log(-0.4D1 * t107 * t10 * t312)
      t345 = t339 ** 2
      t358 = (0.360D3 * t70 * t47 * t315 * t326 + 0.720D3 * t58 * t326) 
     #* t84 * t102 / 0.90D2 - 0.2D1 / 0.45D2 * (0.180D3 * t105 * t71 * t
     #339 * t182 * t325 + 0.45D2 * t115 * t48 * t345 * t182 * t325 + t17
     # * t25 * t70 * t47 * t182 * t325) * t102
      t359 = FJET(XB1, XB2, s, -t2 * t128 * x2 * t150, 0.0D0, t4 * t128 
     #* t2, x1 * t1 * s, -t167, t358)
      bqqbH6n1e1 = t126 * t125 + t239 * t235 * t84 * t102 / 0.90D2 + t30
     #1 * t300 + t359 * t358

      end function



      doubleprecision function bqqbH6n1e0
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
      t6 = wd * z
      t7 = t1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * x2
      t17 = t16 * t4
      t20 = log(-0.4D1 * x3 * t12 * t17)
      t21 = cos(t10)
      t22 = t21 ** 2
      t24 = t4 * x2
      t25 = Sqrt(-t24)
      t26 = t25 ** 2
      t27 = x2 * z
      t29 = (0.1D1 - x2 + t27) ** 2
      t30 = t29 ** 2
      t31 = 0.1D1 / t30
      t32 = t26 * t31
      t36 = lh * wd
      t37 = t36 * z
      t38 = t8 * t22
      t40 = t37 * t38 * t32
      t43 = 0.1D1 / x3
      t46 = t6 * t38
      t47 = 0.1D1 / x1
      t48 = t43 * t47
      t54 = x1 ** 2
      t55 = t54 * t12
      t58 = log(-0.4D1 * t55 * t17)
      t70 = log(-0.4D1 * t12 * t15 * t24)
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t82 = t70 ** 2
      t88 = z * t8
      t92 = (0.360D3 * t9 * t20 * t22 * t32 + 0.720D3 * t40) * t43 / 0.1
     #80D3 - 0.4D1 * t46 * t32 * t48 - 0.2D1 / 0.45D2 * (-0.90D2 * t22 *
     # wd * z * t8 * t58 * t32 - 0.180D3 * t40) * t47 - (0.180D3 * t70 *
     # t22 * t26 * lh + t22 * t26 * (0.180D3 * t76 - 0.30D2 * t78) + 0.4
     #5D2 * t82 * t22 * t26) * wd * t88 * t31 / 0.45D2
      t93 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t9
     #2)
      t95 = -0.1D1 + x1
      t96 = x3 * x1
      t97 = t96 * z
      t98 = x2 * x3
      t99 = 0.2D1 * t98
      t100 = t96 * x2
      t101 = t96 * t27
      t102 = sqrt(x3)
      t103 = t21 * t102
      t104 = x1 * z
      t105 = 0.1D1 - x1 + t104
      t106 = t4 * t105
      t107 = -0.1D1 + t102
      t109 = t102 + 0.1D1
      t112 = Sqrt(t106 * x2 * t107 * t109)
      t114 = 0.2D1 * t103 * t112
      t117 = 0.1D1 / t105
      t121 = x1 * x2
      t122 = t121 * z
      t123 = 0.1D1 - x1 + t104 - x2 + t121 - t122 - x3 + t96 - t97 + t99
     # - t100 + t101 + t114
      t134 = s * t7 * x2 * x1 * t95 * t117
      t135 = t95 ** 2
      t136 = t135 * t95
      t139 = (-0.1D1 + x1 - t104 + x2 - t121 - t27 + t122) ** 2
      t140 = 0.1D1 / t139
      t146 = t54 * x2
      t151 = x3 * z
      t155 = 0.1D1 - 0.2D1 * t103 * t112 * x1 - x1 - x2 - x3 + t104 + t1
     #14 - t122 + t146 * x3 * t14 - t96 * x2 * t14 - 0.2D1 * t146 * t151
     # + 0.4D1 * t101
      t156 = t112 * z
      t167 = 0.2D1 * t98 * z
      t168 = -0.2D1 * t103 * t156 + 0.2D1 * t103 * t156 * x1 + t151 - 0.
     #2D1 * t97 - 0.3D1 * t100 + t96 * t14 + t146 * x3 - t167 + t96 + t9
     #9 + t27 + t121
      t170 = (t155 + t168) ** 2
      t171 = 0.1D1 / t170
      t172 = t102 * x1
      t177 = 0.2D1 * t102 * x2
      t183 = (-t172 * z + 0.2D1 * t172 * t27 - t102 + t172 + t177 - 0.2D
     #1 * t172 * x2 + 0.2D1 * t21 * t112) ** 2
      t186 = t105 * t43 * t47
      t189 = FJET(XB1, XB2, s, t2 * t95 * (-x3 + t96 - t97 + t99 - t100 
     #+ t101 - x2 + t114) * t117, t2 * t96, -t2 * t95 * t123 * t117, -t2
     # * x1 * t107 * t109, -t134, t6 * t8 * t136 * t140 * t171 * t183 * 
     #t186)
      t197 = t107 * t109
      t199 = Sqrt(t24 * t197)
      t201 = 0.2D1 * t103 * t199
      t211 = log(0.4D1 * x3 * t4 * t12 * t16 * t197)
      t212 = 0.1D1 / t29
      t217 = (-t102 + t177 + 0.2D1 * t21 * t199) ** 2
      t222 = (-0.1D1 - t27 + t167 - t201 + 0.2D1 * t103 * t199 * z - t15
     #1 + x3 - t99 + x2) ** 2
      t224 = t217 / t222
      t228 = t8 * t212
      t238 = (-0.90D2 * t9 * t211 * t212 * t224 - 0.180D3 * t37 * t228 *
     # t224) * t43 / 0.180D3 + t6 * t228 * t224 * t48
      t239 = FJET(XB1, XB2, s, -t2 * (-x3 + t99 - x2 + t201), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t99 + t201), 0.0D0, 0.0D0, t238)
      t250 = t139 ** 2
      t253 = Sqrt(-t106 * x2)
      t254 = t253 ** 2
      t255 = 0.1D1 / t250 * t254
      t264 = log(-0.4D1 * t55 * t15 * t24 * t117 * t135)
      t266 = t255 * t105
      t278 = -0.4D1 * t6 * t38 * t136 * t255 * t186 - 0.2D1 / 0.45D2 * (
     #-0.90D2 * t46 * t264 * t136 * t266 - 0.180D3 * t36 * t88 * t22 * t
     #136 * t266) * t47
      t279 = FJET(XB1, XB2, s, -t2 * t95 * x2 * t117, 0.0D0, t4 * t95 * 
     #t2, x1 * t1 * s, -t134, t278)
      bqqbH6n1e0 = t93 * t92 + t189 * wd * t88 * t136 * t140 * t171 * t1
     #83 * t186 + t239 * t238 + t279 * t278

      end function



      doubleprecision function bqqbH6n1em1
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
      t7 = cos(t6)
      t8 = t7 ** 2
      t9 = t4 * x2
      t10 = Sqrt(-t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t15 = Sin(t6)
      t16 = t15 ** 2
      t17 = z ** 2
      t22 = log(-0.4D1 * t16 / t17 * t9)
      t28 = t1 ** 2
      t29 = t28 ** 2
      t30 = z * t29
      t31 = x2 * z
      t33 = (0.1D1 - x2 + t31) ** 2
      t34 = t33 ** 2
      t35 = 0.1D1 / t34
      t39 = wd * z
      t40 = t39 * t29
      t41 = 0.1D1 / x3
      t46 = 0.1D1 / x1
      t51 = -(-0.180D3 * t12 * lh - 0.90D2 * t22 * t8 * t11) * wd * t30 
     #* t35 / 0.45D2 - 0.2D1 * t40 * t12 * t35 * t41 - 0.4D1 * t40 * t12
     # * t35 * t46
      t52 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t5
     #1)
      t54 = x2 * x3
      t55 = 0.2D1 * t54
      t56 = sqrt(x3)
      t57 = t7 * t56
      t62 = Sqrt(t9 * (-0.1D1 + t56) * (t56 + 0.1D1))
      t64 = 0.2D1 * t57 * t62
      t75 = (-t56 + 0.2D1 * t56 * x2 + 0.2D1 * t7 * t62) ** 2
      t84 = (-0.1D1 - t31 + 0.2D1 * t54 * z - t64 + 0.2D1 * t57 * t62 * 
     #z - x3 * z + x3 - t55 + x2) ** 2
      t87 = 0.1D1 / t33 * t75 / t84 * t41
      t90 = FJET(XB1, XB2, s, -t2 * (-x3 + t55 - x2 + t64), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t55 + t64), 0.0D0, 0.0D0, t40 * t87 / 0.2D1)
      t95 = -0.1D1 + x1
      t97 = x1 * z
      t98 = 0.1D1 - x1 + t97
      t99 = 0.1D1 / t98
      t113 = t95 ** 2
      t115 = x1 * x2
      t118 = (-0.1D1 + x1 - t97 + x2 - t115 - t31 + t115 * z) ** 2
      t119 = t118 ** 2
      t124 = Sqrt(-t4 * t98 * x2)
      t125 = t124 ** 2
      t128 = t113 * t95 / t119 * t125 * t98 * t46
      t131 = FJET(XB1, XB2, s, -t2 * t95 * x2 * t99, 0.0D0, t4 * t95 * t
     #2, x1 * t1 * s, -s * t28 * x2 * x1 * t95 * t99, -0.4D1 * t39 * t29
     # * t8 * t128)
      bqqbH6n1em1 = t52 * t51 + t90 * wd * t30 * t87 / 0.2D1 - 0.4D1 * t
     #131 * wd * t30 * t8 * t128

      end function



      doubleprecision function bqqbH6n1em2
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
      t7 = t1 ** 2
      t8 = t7 ** 2
      t11 = cos(x4 * 0.3141592653589793D1)
      t12 = t11 ** 2
      t14 = Sqrt(-t4 * x2)
      t15 = t14 ** 2
      t19 = (0.1D1 - x2 + x2 * z) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t25 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.2D1 * wd * z * t8 * t12 * t15 * t21)
      bqqbH6n1em2 = -0.2D1 * t25 * wd * z * t8 * t12 * t15 * t21

      end function



      doubleprecision function bqqbH6n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bqqbH6n1em3 = 0.0D0

      end function



      doubleprecision function bqqbH6n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bqqbH6n1em4 = 0.0D0

      end function
