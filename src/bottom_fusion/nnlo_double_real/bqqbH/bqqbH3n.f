  
      subroutine bqqbH3n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bqqbH31J1  
      doubleprecision bqqbH31J2  
      doubleprecision bqqbH3n1e1  
      doubleprecision bqqbH3n1e0  
      doubleprecision bqqbH3n1em1  
      doubleprecision bqqbH3n1em2  
      doubleprecision bqqbH3n1em3  
      doubleprecision bqqbH3n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bqqbH3n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bqqbH3n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bqqbH3n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bqqbH3n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bqqbH3n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bqqbH3n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bqqbH3n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH31J1
      doubleprecision bqqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = t8 * t10
      t12 = x2 * t4
      t15 = log(-0.4D1 * t11 * t12)
      t18 = lh ** 2
      t19 = 0.180D3 * t18
      t20 = 0.3141592653589793D1 ** 2
      t21 = 0.30D2 * t20
      t22 = t15 ** 2
      t25 = t1 ** 2
      t27 = 0.1D1 / s
      t28 = x2 * z
      t30 = 0.1D1 / (0.1D1 - x2 + t28)
      t31 = t27 * t30
      t32 = bqqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t36 = t19 - t21
      t48 = bqqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t49 = t31 * t48
      t52 = lh * t25
      t53 = x3 * t8
      t54 = t10 * x2
      t55 = t54 * t4
      t58 = log(-0.4D1 * t53 * t55)
      t64 = t25 * t27
      t66 = t58 ** 2
      t73 = t36 * t25
      t74 = t73 * t49
      t76 = 0.1D1 / x3
      t79 = t30 * t32
      t80 = x1 ** 2
      t81 = x3 * t80
      t82 = t81 * t8
      t85 = log(-0.4D1 * t82 * t55)
      t95 = 0.1D1 / x1
      t98 = t80 * t8
      t101 = log(-0.4D1 * t98 * t55)
      t102 = t101 * t30
      t109 = t101 ** 2
      t119 = (0.180D3 * t15 * lh + t19 - t21 + 0.45D2 * t22) * t25 * t31
     # * t32 / 0.5760D4 + (-t15 * t36 - 0.90D2 * t22 * lh + 0.60D2 * lh 
     #* t20 - 0.2884936567583026D3 - 0.120D3 * t18 * lh - 0.15D2 * t22 *
     # t15) * t25 * t49 / 0.5760D4 - (-0.180D3 * t52 * t31 * (-t32 + t58
     # * t48) + 0.90D2 * t64 * t30 * (t58 * t32 - t66 * t48 / 0.2D1) - t
     #74) * t76 / 0.5760D4 - (0.90D2 * t64 * (-t79 + t85 * t30 * t48) + 
     #0.180D3 * t52 * t49) * t76 * t95 / 0.2880D4 + (-0.180D3 * t52 * t2
     #7 * (t79 - t102 * t48) + 0.90D2 * t64 * (-t102 * t32 + t109 * t30 
     #* t48 / 0.2D1) + t74) * t95 / 0.2880D4
      t120 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #119)
      t122 = -0.1D1 + x1
      t123 = x3 * x1
      t124 = t123 * z
      t126 = 0.2D1 * x2 * x3
      t127 = t123 * x2
      t128 = t123 * t28
      t129 = cos(t6)
      t131 = x1 * z
      t132 = 0.1D1 - x1 + t131
      t134 = -0.1D1 + x3
      t137 = Sqrt(x3 * t4 * t132 * x2 * t134)
      t139 = 0.2D1 * t129 * t137
      t142 = 0.1D1 / t132
      t146 = x1 * x2
      t147 = t146 * z
      t148 = 0.1D1 - x1 + t131 - x2 + t146 - t147 - x3 + t123 - t124 + t
     #126 - t127 + t128 + t139
      t158 = s * t25 * x2 * x1 * t122 * t142
      t160 = 0.1D1 / (-0.1D1 + x1 - t131 + x2 - t146 - t28 + t147)
      t161 = t122 * t160
      t162 = -t122
      t163 = bqqbH31J2(s, XB1, XB2, z, lh, wd, t162, x2, x3, x4)
      t165 = t81 * t11
      t166 = t122 ** 2
      t167 = t142 * t166
      t172 = log(0.4D1 * t165 * t12 * t167 * t134)
      t174 = bqqbH31J1(s, XB1, XB2, z, lh, wd, t162, x2, x3, x4)
      t180 = t52 * t27
      t184 = 0.90D2 * t64 * (-t161 * t163 + t172 * t122 * t160 * t174) +
     # 0.180D3 * t180 * t161 * t174
      t188 = FJET(XB1, XB2, s, t2 * t122 * (-x3 + t123 - t124 + t126 - t
     #127 + t128 - x2 + t139) * t142, t2 * t123, -t2 * t122 * t148 * t14
     #2, -t2 * x1 * t134, -t158, -t184 * t76 * t95 / 0.2880D4)
      t195 = Sqrt(t12 * x3 * t134)
      t197 = 0.2D1 * t129 * t195
      t202 = bqqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t207 = log(0.4D1 * t53 * t10 * t12 * t134)
      t208 = bqqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t215 = t207 ** 2
      t222 = t31 * t208
      t232 = log(0.4D1 * t82 * t54 * t4 * t134)
      t244 = -(-0.180D3 * t52 * t31 * (t202 - t207 * t208) + 0.90D2 * t6
     #4 * t30 * (-t207 * t202 + t215 * t208 / 0.2D1) + t73 * t222) * t76
     # / 0.5760D4 - (0.90D2 * t64 * (t30 * t202 - t232 * t30 * t208) - 0
     #.180D3 * t52 * t222) * t76 * t95 / 0.2880D4
      t245 = FJET(XB1, XB2, s, -t2 * (-x3 + t126 - x2 + t197), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t126 + t197), 0.0D0, 0.0D0, t244)
      t254 = bqqbH31J2(s, XB1, XB2, z, lh, wd, t162, x2, 0.0D0, x4)
      t255 = t161 * t254
      t256 = t12 * t167
      t259 = log(-0.4D1 * t165 * t256)
      t261 = bqqbH31J1(s, XB1, XB2, z, lh, wd, t162, x2, 0.0D0, x4)
      t262 = t160 * t261
      t267 = t161 * t261
      t276 = log(-0.4D1 * t98 * t10 * t256)
      t277 = t276 * t122
      t285 = t276 ** 2
      t297 = -(0.90D2 * t64 * (t255 - t259 * t122 * t262) - 0.180D3 * t1
     #80 * t267) * t76 * t95 / 0.2880D4 + (0.180D3 * t52 * t27 * (t255 -
     # t277 * t262) - 0.90D2 * t64 * (-t277 * t160 * t254 + t285 * t122 
     #* t262 / 0.2D1) - t73 * t27 * t267) * t95 / 0.2880D4
      t298 = FJET(XB1, XB2, s, -t2 * t122 * x2 * t142, 0.0D0, t4 * t122 
     #* t2, x1 * t1 * s, -t158, t297)
      bqqbH3n1e1 = t120 * t119 - t188 * t184 * t76 * t95 / 0.2880D4 + t2
     #45 * t244 + t298 * t297

      end function



      doubleprecision function bqqbH3n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH31J1
      doubleprecision bqqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = x2 * z
      t11 = 0.1D1 / (0.1D1 - x2 + t9)
      t12 = bqqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = x3 * t15
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t18 * x2 * t4
      t23 = log(-0.4D1 * t16 * t20)
      t24 = bqqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t30 = lh * t6
      t31 = t7 * t11
      t32 = t31 * t24
      t34 = 0.180D3 * t30 * t32
      t36 = 0.1D1 / x3
      t39 = t8 * t11
      t41 = 0.1D1 / x1
      t46 = x1 ** 2
      t47 = t46 * t15
      t50 = log(-0.4D1 * t47 * t20)
      t61 = x2 * t4
      t64 = log(-0.4D1 * t15 * t18 * t61)
      t73 = lh ** 2
      t75 = 0.3141592653589793D1 ** 2
      t77 = t64 ** 2
      t83 = -(0.90D2 * t8 * t11 * (-t12 + t23 * t24) + t34) * t36 / 0.57
     #60D4 + t39 * t24 * t36 * t41 / 0.32D2 + (0.90D2 * t8 * (t11 * t12 
     #- t50 * t11 * t24) - t34) * t41 / 0.2880D4 + (-0.180D3 * lh - 0.90
     #D2 * t64) * t6 * t31 * t12 / 0.5760D4 + (0.180D3 * t64 * lh + 0.18
     #0D3 * t73 - 0.30D2 * t75 + 0.45D2 * t77) * t6 * t32 / 0.5760D4
      t84 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t8
     #3)
      t86 = -0.1D1 + x1
      t87 = x3 * x1
      t88 = t87 * z
      t90 = 0.2D1 * x2 * x3
      t91 = t87 * x2
      t92 = t87 * t9
      t93 = cos(t13)
      t95 = x1 * z
      t96 = 0.1D1 - x1 + t95
      t98 = -0.1D1 + x3
      t101 = Sqrt(x3 * t4 * t96 * x2 * t98)
      t103 = 0.2D1 * t93 * t101
      t106 = 0.1D1 / t96
      t110 = x1 * x2
      t111 = t110 * z
      t112 = 0.1D1 - x1 + t95 - x2 + t110 - t111 - x3 + t87 - t88 + t90 
     #- t91 + t92 + t103
      t122 = s * t6 * x2 * x1 * t86 * t106
      t123 = t8 * t86
      t125 = 0.1D1 / (-0.1D1 + x1 - t95 + x2 - t110 - t9 + t111)
      t126 = -t86
      t127 = bqqbH31J1(s, XB1, XB2, z, lh, wd, t126, x2, x3, x4)
      t129 = t36 * t41
      t130 = t125 * t127 * t129
      t133 = FJET(XB1, XB2, s, t2 * t86 * (-x3 + t87 - t88 + t90 - t91 +
     # t92 - x2 + t103) * t106, t2 * t87, -t2 * t86 * t112 * t106, -t2 *
     # x1 * t98, -t122, t123 * t130 / 0.32D2)
      t141 = Sqrt(t61 * x3 * t98)
      t143 = 0.2D1 * t93 * t141
      t148 = bqqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t153 = log(0.4D1 * t16 * t18 * t61 * t98)
      t154 = bqqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t170 = -(0.90D2 * t8 * t11 * (t148 - t153 * t154) - 0.180D3 * t30 
     #* t31 * t154) * t36 / 0.5760D4 - t39 * t154 * t36 * t41 / 0.32D2
      t171 = FJET(XB1, XB2, s, -t2 * (-x3 + t90 - x2 + t143), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t90 + t143), 0.0D0, 0.0D0, t170)
      t180 = bqqbH31J1(s, XB1, XB2, z, lh, wd, t126, x2, 0.0D0, x4)
      t181 = t125 * t180
      t185 = t86 * t125
      t186 = bqqbH31J2(s, XB1, XB2, z, lh, wd, t126, x2, 0.0D0, x4)
      t189 = t86 ** 2
      t194 = log(-0.4D1 * t47 * t18 * t61 * t106 * t189)
      t207 = -t123 * t181 * t129 / 0.32D2 + (-0.90D2 * t8 * (t185 * t186
     # - t194 * t86 * t181) + 0.180D3 * t30 * t7 * t185 * t180) * t41 / 
     #0.2880D4
      t208 = FJET(XB1, XB2, s, -t2 * t86 * x2 * t106, 0.0D0, t4 * t86 * 
     #t2, x1 * t1 * s, -t122, t207)
      bqqbH3n1e0 = t84 * t83 + t133 * t6 * t7 * t86 * t130 / 0.32D2 + t1
     #71 * t170 + t207 * t208

      end function



      doubleprecision function bqqbH3n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH31J1
      doubleprecision bqqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = x2 * z
      t11 = 0.1D1 / (0.1D1 - x2 + t9)
      t12 = bqqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t17 = x4 * 0.3141592653589793D1
      t18 = Sin(t17)
      t19 = t18 ** 2
      t20 = z ** 2
      t23 = x2 * t4
      t26 = log(-0.4D1 * t19 / t20 * t23)
      t31 = bqqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t35 = t11 * t31
      t36 = 0.1D1 / x3
      t40 = 0.1D1 / x1
      t44 = t8 * t11 * t12 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t26) * t
     #6 * t7 * t11 * t31 / 0.5760D4 + t8 * t35 * t36 / 0.64D2 + t8 * t35
     # * t40 / 0.32D2
      t45 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #4)
      t48 = 0.2D1 * x2 * x3
      t49 = cos(t17)
      t53 = Sqrt(t23 * x3 * (-0.1D1 + x3))
      t55 = 0.2D1 * t49 * t53
      t60 = bqqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t62 = t11 * t60 * t36
      t65 = FJET(XB1, XB2, s, -t2 * (-x3 + t48 - x2 + t55), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t48 + t55), 0.0D0, 0.0D0, -t8 * t62 / 0.64D2)
      t70 = -0.1D1 + x1
      t72 = x1 * z
      t74 = 0.1D1 / (0.1D1 - x1 + t72)
      t87 = x1 * x2
      t90 = 0.1D1 / (-0.1D1 + x1 - t72 + x2 - t87 - t9 + t87 * z)
      t92 = bqqbH31J1(s, XB1, XB2, z, lh, wd, -t70, x2, 0.0D0, x4)
      t97 = FJET(XB1, XB2, s, -t2 * t70 * x2 * t74, 0.0D0, t4 * t70 * t2
     #, x1 * t1 * s, -s * t6 * x2 * x1 * t70 * t74, -t8 * t70 * t90 * t9
     #2 * t40 / 0.32D2)
      bqqbH3n1em1 = t45 * t44 - t65 * t6 * t7 * t62 / 0.64D2 - t97 * t6 
     #* t7 * t70 * t90 * t92 * t40 / 0.32D2

      end function



      doubleprecision function bqqbH3n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH31J1
      doubleprecision bqqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t11 = 0.1D1 / (0.1D1 - x2 + x2 * z)
      t12 = bqqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t16 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * (-0.1D1 + x2), 0.0D0
     #, 0.0D0, t6 * t7 * t11 * t12 / 0.64D2)
      bqqbH3n1em2 = t16 * t6 * t7 * t11 * t12 / 0.64D2

      end function



      doubleprecision function bqqbH3n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH31J1
      doubleprecision bqqbH31J2
      bqqbH3n1em3 = 0.0D0

      end function



      doubleprecision function bqqbH3n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH31J1
      doubleprecision bqqbH31J2
      bqqbH3n1em4 = 0.0D0

      end function
  
 

      doubleprecision function bqqbH31J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t20 = t8 * t4 + t10 * x2 - 0.2D1 * t13 * t17
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = t26 * s
      t28 = t1 ** 2
      t32 = x2 * x1 * t23 * t5
      t37 = s ** 2
      t38 = t37 * z
      t44 = z ** 2
      t50 = t28 ** 2
      t52 = x2 ** 2
      t54 = x1 ** 2
      t55 = t23 ** 2
      t57 = t4 ** 2
      t58 = 0.1D1 / t57
      t72 = t20 ** 2
      bqqbH31J1 = -0.16D2 * wd * (0.2D1 * t27 * t28 * t32 + 0.2D1 * t27 
     #* z + 0.2D1 * t38 * t1 * t21 - t27 * t1 * t21 - t27 - 0.2D1 * t37 
     #* t44 - 0.2D1 * t38 * t28 * t32 - 0.2D1 * t37 * t50 * t52 * t54 * 
     #t55 * t58 + 0.4D1 * t37 * t28 * t1 * t54 * t58 * t20 * x2 * t23 - 
     #0.2D1 * t37 * t28 * t54 * t58 * t72) / t26

      end function
  
   
 

      doubleprecision function bqqbH31J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t21 = x1 / t4 * (t8 * t4 + t10 * x2 - 0.2D1 * t13 * t17)
      t26 = s - t2 * t21 - t2 * (0.1D1 - x1) * x3
      t27 = t26 * s
      bqqbH31J2 = -0.16D2 * wd * (-t27 * t1 * t21 + t27) / t26

      end function
  
 