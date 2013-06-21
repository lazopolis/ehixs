  
      subroutine bggbH7n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH7n1e1  
      doubleprecision bggbH7n1e0  
      doubleprecision bggbH7n1em1  
      doubleprecision bggbH7n1em2  
      doubleprecision bggbH7n1em3  
      doubleprecision bggbH7n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH7n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH7n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH7n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH7n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH7n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH7n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH7n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * lh
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t17 = log(-0.4D1 * x3 * t10 * t13 * t4)
      t18 = t17 * z
      t25 = lh ** 2
      t27 = 0.3141592653589793D1 ** 2
      t29 = 0.180D3 * t25 - 0.30D2 * t27
      t30 = z * t29
      t31 = t17 ** 2
      t32 = t31 * z
      t56 = t1 * wd
      t57 = 0.8D1 * x3
      t58 = x2 * x3
      t59 = t10 * t13
      t60 = t59 * t4
      t63 = log(-0.4D1 * t58 * t60)
      t64 = t63 * t4
      t70 = z * t1
      t71 = 0.4D1 * x3
      t73 = t63 ** 2
      t81 = -0.4D1 * t4
      t85 = 0.1D1 / x2
      t88 = x1 ** 2
      t89 = t58 * t88
      t92 = log(-0.4D1 * t89 * t60)
      t105 = 0.1D1 / x1
      t108 = t70 * t4
      t109 = t1 * t4
      t110 = t6 * t109
      t113 = (-0.180D3 * t108 - 0.180D3 * t110) * wd
      t114 = x3 * t88
      t117 = log(-0.4D1 * t114 * t60)
      t119 = t4 * wd
      t120 = t117 ** 2
      t128 = (0.90D2 * t108 + 0.360D3 * t110 + t30 * t109) * wd
      t132 = ((-0.180D3 * t6 - 0.90D2 * t18) * t1 * t4 - 0.2D1 * (0.180D
     #3 * t18 * lh + t30 + 0.45D2 * t32) * t1 * t4 + (-t18 * t29 - 0.90D
     #2 * t32 * lh + z * (-0.2884936567583026D3 - 0.120D3 * t25 * lh + 0
     #.60D2 * lh * t27) - 0.15D2 * t31 * t17 * z) * t1 * t4) * wd / 0.13
     #5D3 - (-0.180D3 * t6 * t56 * (-0.8D1 + t57 + 0.4D1 * t64) + 0.90D2
     # * t70 * wd * (0.4D1 - t71 - 0.8D1 * t64 - 0.2D1 * t73 * t4) + t30
     # * t56 * t81) * t85 / 0.540D3 + (0.90D2 * t70 * wd * (0.8D1 - t57 
     #- 0.4D1 * t92 * t4) + 0.180D3 * t6 * t56 * t81) * t85 * t105 / 0.2
     #70D3 + 0.2D1 / 0.135D3 * (-t113 * t117 + 0.45D2 * t70 * t119 * t12
     #0 + t128) * t105
      t133 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #132)
      t135 = -0.1D1 + x1
      t136 = x1 * x3
      t137 = t136 * z
      t138 = 0.2D1 * t58
      t139 = t58 * x1
      t140 = x1 * z
      t141 = t58 * t140
      t142 = cos(t8)
      t143 = -0.1D1 + x2
      t145 = 0.1D1 - x1 + t140
      t149 = Sqrt(x3 * t143 * t145 * x2 * t4)
      t151 = 0.2D1 * t142 * t149
      t154 = 0.1D1 / t145
      t157 = t2 * t136
      t158 = x2 * x1
      t159 = t158 * z
      t160 = 0.1D1 - x1 + t140 - x2 + t158 - t159 - x3 + t136 - t137 + t
     #138 - t139 + t141 + t151
      t164 = t4 * s
      t166 = t164 * t1 * x1
      t167 = t1 ** 2
      t174 = 0.4D1 * t137
      t175 = 0.4D1 * t136
      t176 = x2 * z
      t177 = 0.13D2 * t176
      t178 = 0.4D1 * t140
      t179 = 0.4D1 * x1
      t180 = 0.13D2 * x2
      t182 = 0.4D1 - 0.13D2 * t158 - t174 + t175 - t177 + t178 - t179 + 
     #t180 - t71 + 0.13D2 * t159
      t186 = 0.5D1 * t176
      t187 = 0.5D1 * x2
      t191 = t58 * t88 * t10
      t192 = t13 * t154
      t193 = t135 ** 2
      t199 = log(0.4D1 * t191 * t192 * t193 * t143 * t4)
      t205 = (-0.1D1 + x1 - t140 + x2 - t158 - t176 + t159) ** 2
      t206 = 0.1D1 / t205
      t210 = t6 * t1
      t216 = -0.90D2 * t70 * wd * (-t145 * t182 + t145 * (t179 + 0.5D1 *
     # t158 - 0.5D1 * t159 + t174 + t186 - 0.4D1 - t187 + t71 - t178 - t
     #175) - t199 * t145 * t182) * t206 + 0.180D3 * t210 * wd * t145 * t
     #182 * t206
      t220 = FJET(XB1, XB2, s, t2 * t135 * (-x3 + t136 - t137 + t138 - t
     #139 + t141 - x2 + t151) * t154, t157, -t2 * t135 * t160 * t154, -t
     #166, -s * t167 * x2 * t135 * x1 * t154, t216 * t85 * t105 / 0.270D
     #3)
      t228 = Sqrt(x2 * t143 * x3 * t4)
      t230 = 0.2D1 * t142 * t228
      t236 = (0.1D1 - x2 + t176) ** 2
      t237 = 0.1D1 / t236
      t238 = -t186 + 0.4D1 + t187 - t71
      t239 = t237 * t238
      t245 = log(0.4D1 * t58 * t10 * t13 * t143 * t4)
      t247 = (-0.1D1 - t245) * t237
      t248 = -0.4D1 + t71 - t180 + t177
      t255 = t245 ** 2
      t266 = wd * t237 * t248
      t275 = log(0.4D1 * t89 * t59 * t143 * t4)
      t289 = -(-0.180D3 * t6 * t56 * (t239 + t247 * t248) + 0.90D2 * t70
     # * wd * (t247 * t238 + (t245 + t255 / 0.2D1) * t237 * t248) + t30 
     #* t1 * t266) * t85 / 0.540D3 + (0.90D2 * t70 * wd * (-t239 - (-0.1
     #D1 - t275) * t237 * t248) + 0.180D3 * t210 * t266) * t85 * t105 / 
     #0.270D3
      t290 = FJET(XB1, XB2, s, -t2 * (-x3 + t138 - x2 + t230), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t138 + t230), 0.0D0, 0.0D0, t289)
      t297 = t192 * t193 * t4
      t300 = log(-0.4D1 * t191 * t297)
      t317 = log(-0.4D1 * t114 * t10 * t297)
      t319 = t317 ** 2
      t326 = (0.90D2 * t70 * wd * (-0.8D1 + t57 + 0.4D1 * t300 * t4) + 0
     #.720D3 * t6 * t56 * t4) * t85 * t105 / 0.270D3 + 0.2D1 / 0.135D3 *
     # (t113 * t317 - 0.45D2 * t70 * t119 * t319 - t128) * t105
      t327 = FJET(XB1, XB2, s, -t2 * t135 * x3, t157, t164 * t1 * t135, 
     #-t166, 0.0D0, t326)
      bggbH7n1e1 = t133 * t132 + t220 * t216 * t85 * t105 / 0.270D3 + t2
     #90 * t289 + t327 * t326

      end function



      doubleprecision function bggbH7n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * t1
      t8 = x2 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t15 = t11 * t13 * t4
      t18 = log(-0.4D1 * t8 * t15)
      t25 = z * lh
      t26 = t1 * wd
      t28 = -0.4D1 * t4
      t33 = 0.1D1 / x2
      t36 = t6 * wd
      t39 = 0.1D1 / x1
      t43 = t4 * wd
      t44 = x1 ** 2
      t45 = x3 * t44
      t48 = log(-0.4D1 * t45 * t15)
      t52 = t6 * t4
      t57 = (-0.180D3 * t52 - 0.180D3 * t25 * t1 * t4) * wd
      t67 = log(-0.4D1 * x3 * t11 * t13 * t4)
      t68 = t67 * z
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t82 = t67 ** 2
      t91 = -(0.90D2 * t6 * wd * (-0.8D1 + 0.8D1 * x3 + 0.4D1 * t18 * t4
     #) - 0.180D3 * t25 * t26 * t28) * t33 / 0.540D3 - t36 * t28 * t33 *
     # t39 / 0.3D1 + 0.2D1 / 0.135D3 * (-0.90D2 * t6 * t43 * t48 + t57) 
     #* t39 + (0.90D2 * t52 - 0.2D1 * (-0.180D3 * t25 - 0.90D2 * t68) * 
     #t1 * t4 + (0.180D3 * t68 * lh + z * (0.180D3 * t76 - 0.30D2 * t78)
     # + 0.45D2 * t82 * z) * t1 * t4) * wd / 0.135D3
      t92 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t9
     #1)
      t94 = -0.1D1 + x1
      t95 = x1 * x3
      t96 = t95 * z
      t97 = 0.2D1 * t8
      t98 = t8 * x1
      t99 = x1 * z
      t100 = t8 * t99
      t101 = cos(t9)
      t102 = -0.1D1 + x2
      t104 = 0.1D1 - x1 + t99
      t108 = Sqrt(x3 * t102 * t104 * x2 * t4)
      t110 = 0.2D1 * t101 * t108
      t113 = 0.1D1 / t104
      t116 = t2 * t95
      t117 = x2 * x1
      t118 = t117 * z
      t119 = 0.1D1 - x1 + t99 - x2 + t117 - t118 - x3 + t95 - t96 + t97 
     #- t98 + t100 + t110
      t123 = t4 * s
      t125 = t123 * t1 * x1
      t126 = t1 ** 2
      t137 = x2 * z
      t138 = 0.13D2 * t137
      t141 = 0.13D2 * x2
      t142 = 0.4D1 * x3
      t144 = 0.4D1 - 0.13D2 * t117 - 0.4D1 * t96 + 0.4D1 * t95 - t138 + 
     #0.4D1 * t99 - 0.4D1 * x1 + t141 - t142 + 0.13D2 * t118
      t146 = (-0.1D1 + x1 - t99 + x2 - t117 - t137 + t118) ** 2
      t147 = 0.1D1 / t146
      t149 = t33 * t39
      t153 = FJET(XB1, XB2, s, t2 * t94 * (-x3 + t95 - t96 + t97 - t98 +
     # t100 - x2 + t110) * t113, t116, -t2 * t94 * t119 * t113, -t125, -
     #s * t126 * x2 * t94 * x1 * t113, -t6 * wd * t104 * t144 * t147 * t
     #149 / 0.3D1)
      t165 = Sqrt(x2 * t102 * x3 * t4)
      t167 = 0.2D1 * t101 * t165
      t173 = (0.1D1 - x2 + t137) ** 2
      t174 = 0.1D1 / t173
      t184 = log(0.4D1 * t8 * t11 * t13 * t102 * t4)
      t187 = -0.4D1 + t142 - t141 + t138
      t205 = -(0.90D2 * t6 * wd * (t174 * (-0.5D1 * t137 + 0.4D1 + 0.5D1
     # * x2 - t142) + (-0.1D1 - t184) * t174 * t187) - 0.180D3 * t25 * t
     #1 * wd * t174 * t187) * t33 / 0.540D3 - t36 * t174 * t187 * t149 /
     # 0.3D1
      t206 = FJET(XB1, XB2, s, -t2 * (-x3 + t97 - x2 + t167), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t97 + t167), 0.0D0, 0.0D0, t205)
      t218 = t94 ** 2
      t223 = log(-0.4D1 * t45 * t11 * t13 * t113 * t218 * t4)
      t230 = -0.4D1 / 0.3D1 * t36 * t4 * t33 * t39 + 0.2D1 / 0.135D3 * (
     #0.90D2 * t6 * t43 * t223 - t57) * t39
      t231 = FJET(XB1, XB2, s, -t2 * t94 * x3, t116, t123 * t1 * t94, -t
     #125, 0.0D0, t230)
      bggbH7n1e0 = t92 * t91 - t153 * z * t26 * t104 * t144 * t147 * t33
     # * t39 / 0.3D1 + t206 * t205 + t231 * t230

      end function



      doubleprecision function bggbH7n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * t1
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = z ** 2
      t20 = log(-0.4D1 * x3 * t13 / t15 * t4)
      t32 = 0.1D1 / x2
      t38 = t4 * wd / x1
      t40 = 0.4D1 / 0.3D1 * t6 * t38
      t41 = (-0.180D3 * t6 * t4 + (-0.180D3 * z * lh - 0.90D2 * t20 * z)
     # * t1 * t4) * wd / 0.135D3 + 0.2D1 / 0.3D1 * t6 * t4 * wd * t32 + 
     #t40
      t42 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #1)
      t45 = 0.2D1 * x2 * x3
      t46 = cos(t11)
      t51 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t53 = 0.2D1 * t46 * t51
      t59 = x2 * z
      t61 = (0.1D1 - x2 + t59) ** 2
      t62 = 0.1D1 / t61
      t66 = -0.4D1 + 0.4D1 * x3 - 0.13D2 * x2 + 0.13D2 * t59
      t71 = FJET(XB1, XB2, s, -t2 * (-x3 + t45 - x2 + t53), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t45 + t53), 0.0D0, 0.0D0, -t6 * wd * t62 * t66 
     #* t32 / 0.6D1)
      t79 = -0.1D1 + x1
      t84 = t4 * s
      t89 = FJET(XB1, XB2, s, -t2 * t79 * x3, t2 * x1 * x3, t84 * t1 * t
     #79, -t84 * t1 * x1, 0.0D0, -t40)
      bggbH7n1em1 = t42 * t41 - t71 * z * t1 * wd * t62 * t66 * t32 / 0.
     #6D1 - 0.4D1 / 0.3D1 * t89 * z * t1 * t38

      end function



      doubleprecision function bggbH7n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t10 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, 0.
     #2D1 / 0.3D1 * z * t1 * t4 * wd)
      bggbH7n1em2 = 0.2D1 / 0.3D1 * t10 * z * t1 * t4 * wd

      end function



      doubleprecision function bggbH7n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bggbH7n1em3 = 0.0D0

      end function



      doubleprecision function bggbH7n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bggbH7n1em4 = 0.0D0

      end function
