  
      subroutine bggbH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH11J1  
      doubleprecision bggbH11J2  
      doubleprecision bggbH11J3  
      doubleprecision bggbH1n1e1  
      doubleprecision bggbH1n1e0  
      doubleprecision bggbH1n1em1  
      doubleprecision bggbH1n1em2  
      doubleprecision bggbH1n1em3  
      doubleprecision bggbH1n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH11J1
      doubleprecision bggbH11J2
      doubleprecision bggbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t16 = log(-0.4D1 * x3 * t9 * t12 * t4)
      t20 = 0.1D1 / s
      t21 = bggbH11J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t16 ** 2
      t35 = bggbH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t39 = t28 - t30
      t51 = bggbH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t52 = t20 * t51
      t55 = lh * t1
      t56 = x2 * x3
      t57 = t9 * t12
      t58 = t57 * t4
      t61 = log(-0.4D1 * t56 * t58)
      t68 = 0.90D2 * t1 * t20
      t70 = t61 ** 2
      t75 = t39 * t1
      t76 = t75 * t52
      t78 = 0.1D1 / x2
      t81 = x1 ** 2
      t82 = t56 * t81
      t85 = log(-0.4D1 * t82 * t58)
      t93 = 0.1D1 / x1
      t96 = x3 * t81
      t99 = log(-0.4D1 * t96 * t58)
      t106 = t99 ** 2
      t114 = (-0.180D3 * lh - 0.90D2 * t16) * t1 * t20 * t21 / 0.5760D4 
     #+ (0.180D3 * t16 * lh + t28 - t30 + 0.45D2 * t31) * t1 * t20 * t35
     # / 0.5760D4 + (-t16 * t39 - 0.90D2 * t31 * lh - 0.2884936567583026
     #D3 - 0.120D3 * t27 * lh + 0.60D2 * lh * t29 - 0.15D2 * t31 * t16) 
     #* t1 * t52 / 0.5760D4 - (-0.180D3 * t55 * t20 * (-t35 + t61 * t51)
     # + t68 * (-t21 + t61 * t35 - t70 * t51 / 0.2D1) - t76) * t78 / 0.5
     #760D4 + (t68 * (t35 - t85 * t51) - 0.180D3 * t55 * t52) * t78 * t9
     #3 / 0.2880D4 + (-0.180D3 * t55 * t20 * (t35 - t99 * t51) + t68 * (
     #t21 - t99 * t35 + t106 * t51 / 0.2D1) + t76) * t93 / 0.2880D4
      t115 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #114)
      t117 = -0.1D1 + x1
      t118 = x1 * x3
      t119 = t118 * z
      t120 = 0.2D1 * t56
      t121 = t56 * x1
      t122 = x1 * z
      t123 = t56 * t122
      t124 = cos(t7)
      t125 = -0.1D1 + x2
      t127 = 0.1D1 - x1 + t122
      t131 = Sqrt(x3 * t125 * t127 * x2 * t4)
      t133 = 0.2D1 * t124 * t131
      t136 = 0.1D1 / t127
      t139 = t2 * t118
      t140 = x2 * x1
      t141 = t140 * z
      t142 = 0.1D1 - x1 + t122 - x2 + t140 - t141 - x3 + t118 - t119 + t
     #120 - t121 + t123 + t133
      t146 = t4 * s
      t148 = t146 * t1 * x1
      t149 = t1 ** 2
      t155 = x2 * z
      t157 = 0.1D1 / (-0.1D1 + x1 - t122 + x2 - t140 - t155 + t141)
      t158 = t127 * t157
      t159 = -t117
      t160 = bggbH11J2(s, XB1, XB2, z, lh, wd, t159, x2, x3, x4)
      t163 = t56 * t81 * t9
      t164 = t12 * t136
      t165 = t117 ** 2
      t171 = log(0.4D1 * t163 * t164 * t165 * t125 * t4)
      t173 = bggbH11J1(s, XB1, XB2, z, lh, wd, t159, x2, x3, x4)
      t182 = t68 * (-t158 * t160 + t171 * t127 * t157 * t173) + 0.180D3 
     #* t55 * t20 * t158 * t173
      t186 = FJET(XB1, XB2, s, t2 * t117 * (-x3 + t118 - t119 + t120 - t
     #121 + t123 - x2 + t133) * t136, t139, -t2 * t117 * t142 * t136, -t
     #148, -s * t149 * x2 * x1 * t117 * t136, t182 * t78 * t93 / 0.2880D
     #4)
      t192 = x3 * t4
      t194 = Sqrt(x2 * t125 * t192)
      t196 = 0.2D1 * t124 * t194
      t202 = 0.1D1 / (0.1D1 - x2 + t155)
      t203 = bggbH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t204 = t202 * t203
      t210 = log(0.4D1 * x2 * t9 * t12 * t192 * t125)
      t211 = t210 * t202
      t212 = bggbH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t218 = bggbH11J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t221 = t210 ** 2
      t228 = t20 * t202 * t212
      t237 = log(0.4D1 * t82 * t57 * t125 * t4)
      t248 = -(-0.180D3 * t55 * t20 * (t204 - t211 * t212) + t68 * (t202
     # * t218 - t211 * t203 + t221 * t202 * t212 / 0.2D1) + t75 * t228) 
     #* t78 / 0.5760D4 + (t68 * (-t204 + t237 * t202 * t212) + 0.180D3 *
     # t55 * t228) * t78 * t93 / 0.2880D4
      t249 = FJET(XB1, XB2, s, -t2 * (-x3 + t120 - x2 + t196), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t120 + t196), 0.0D0, 0.0D0, t248)
      t255 = bggbH11J2(s, XB1, XB2, z, lh, wd, t159, 0.0D0, x3, x4)
      t257 = t164 * t165 * t4
      t260 = log(-0.4D1 * t163 * t257)
      t261 = bggbH11J1(s, XB1, XB2, z, lh, wd, t159, 0.0D0, x3, x4)
      t265 = t20 * t261
      t274 = log(-0.4D1 * t96 * t9 * t257)
      t280 = bggbH11J3(s, XB1, XB2, z, lh, wd, t159, 0.0D0, x3, x4)
      t282 = t274 ** 2
      t291 = (-t68 * (t255 - t260 * t261) + 0.180D3 * t55 * t265) * t78 
     #* t93 / 0.2880D4 + (0.180D3 * t55 * t20 * (t255 - t274 * t261) - t
     #68 * (t280 - t274 * t255 + t282 * t261 / 0.2D1) - t75 * t265) * t9
     #3 / 0.2880D4
      t292 = FJET(XB1, XB2, s, -t2 * t117 * x3, t139, t146 * t1 * t117, 
     #-t148, 0.0D0, t291)
      bggbH1n1e1 = t115 * t114 + t186 * t182 * t78 * t93 / 0.2880D4 + t2
     #49 * t248 + t292 * t291

      end function



      doubleprecision function bggbH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH11J1
      doubleprecision bggbH11J2
      doubleprecision bggbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.90D2 * t1
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bggbH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t10 = x2 * x3
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = t13 * t15 * t4
      t20 = log(-0.4D1 * t10 * t17)
      t21 = bggbH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t25 = lh * t1
      t26 = t7 * t21
      t28 = 0.180D3 * t25 * t26
      t30 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t38 = x1 ** 2
      t39 = x3 * t38
      t42 = log(-0.4D1 * t39 * t17)
      t49 = bggbH11J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t57 = log(-0.4D1 * x3 * t13 * t15 * t4)
      t66 = lh ** 2
      t68 = 0.3141592653589793D1 ** 2
      t70 = t57 ** 2
      t76 = -(t8 * (-t9 + t20 * t21) + t28) * t30 / 0.5760D4 + t8 * t21 
     #* t30 * t34 / 0.2880D4 + (t8 * (t9 - t42 * t21) - t28) * t34 / 0.2
     #880D4 + t8 * t49 / 0.5760D4 + (-0.180D3 * lh - 0.90D2 * t57) * t1 
     #* t7 * t9 / 0.5760D4 + (0.180D3 * t57 * lh + 0.180D3 * t66 - 0.30D
     #2 * t68 + 0.45D2 * t70) * t1 * t26 / 0.5760D4
      t77 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #6)
      t79 = -0.1D1 + x1
      t80 = x1 * x3
      t81 = t80 * z
      t82 = 0.2D1 * t10
      t83 = t10 * x1
      t84 = x1 * z
      t85 = t10 * t84
      t86 = cos(t11)
      t87 = -0.1D1 + x2
      t89 = 0.1D1 - x1 + t84
      t93 = Sqrt(x3 * t87 * t89 * x2 * t4)
      t95 = 0.2D1 * t86 * t93
      t98 = 0.1D1 / t89
      t101 = t2 * t80
      t102 = x2 * x1
      t103 = t102 * z
      t104 = 0.1D1 - x1 + t84 - x2 + t102 - t103 - x3 + t80 - t81 + t82 
     #- t83 + t85 + t95
      t108 = t4 * s
      t110 = t108 * t1 * x1
      t111 = t1 ** 2
      t118 = x2 * z
      t121 = -t79
      t122 = bggbH11J1(s, XB1, XB2, z, lh, wd, t121, x2, x3, x4)
      t125 = 0.1D1 / (-0.1D1 + x1 - t84 + x2 - t102 - t118 + t103) * t12
     #2 * t30 * t34
      t128 = FJET(XB1, XB2, s, t2 * t79 * (-x3 + t80 - t81 + t82 - t83 +
     # t85 - x2 + t95) * t98, t101, -t2 * t79 * t104 * t98, -t110, -s * 
     #t111 * x2 * x1 * t79 * t98, -t8 * t89 * t125 / 0.2880D4)
      t135 = x3 * t4
      t137 = Sqrt(x2 * t87 * t135)
      t139 = 0.2D1 * t86 * t137
      t145 = 0.1D1 / (0.1D1 - x2 + t118)
      t146 = bggbH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t153 = log(0.4D1 * x2 * t13 * t15 * t135 * t87)
      t155 = bggbH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t171 = -(t8 * (t145 * t146 - t153 * t145 * t155) - 0.180D3 * t25 *
     # t7 * t145 * t155) * t30 / 0.5760D4 - t8 * t145 * t155 * t30 * t34
     # / 0.2880D4
      t172 = FJET(XB1, XB2, s, -t2 * (-x3 + t82 - x2 + t139), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t82 + t139), 0.0D0, 0.0D0, t171)
      t178 = bggbH11J1(s, XB1, XB2, z, lh, wd, t121, 0.0D0, x3, x4)
      t182 = bggbH11J2(s, XB1, XB2, z, lh, wd, t121, 0.0D0, x3, x4)
      t185 = t79 ** 2
      t190 = log(-0.4D1 * t39 * t13 * t15 * t98 * t185 * t4)
      t200 = -t8 * t178 * t30 * t34 / 0.2880D4 + (-t8 * (t182 - t190 * t
     #178) + 0.180D3 * t25 * t7 * t178) * t34 / 0.2880D4
      t201 = FJET(XB1, XB2, s, -t2 * t79 * x3, t101, t108 * t1 * t79, -t
     #110, 0.0D0, t200)
      bggbH1n1e0 = t77 * t76 - t128 * t6 * t7 * t89 * t125 / 0.2880D4 + 
     #t172 * t171 + t201 * t200

      end function



      doubleprecision function bggbH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH11J1
      doubleprecision bggbH11J2
      doubleprecision bggbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.90D2 * t1
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = bggbH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t17 = z ** 2
      t22 = log(-0.4D1 * x3 * t15 / t17 * t4)
      t26 = bggbH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t30 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t38 = t8 * t9 / 0.5760D4 + (-0.180D3 * lh - 0.90D2 * t22) * t1 * t
     #7 * t26 / 0.5760D4 + t8 * t26 * t30 / 0.5760D4 + t8 * t26 * t34 / 
     #0.2880D4
      t39 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #8)
      t42 = 0.2D1 * x2 * x3
      t43 = cos(t13)
      t48 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t50 = 0.2D1 * t43 * t48
      t58 = bggbH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t60 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t58 * t30
      t63 = FJET(XB1, XB2, s, -t2 * (-x3 + t42 - x2 + t50), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t42 + t50), 0.0D0, 0.0D0, -t8 * t60 / 0.5760D4)
      t68 = -0.1D1 + x1
      t73 = t4 * s
      t79 = bggbH11J1(s, XB1, XB2, z, lh, wd, -t68, 0.0D0, x3, x4)
      t83 = FJET(XB1, XB2, s, -t2 * t68 * x3, t2 * x1 * x3, t73 * t1 * t
     #68, -t73 * t1 * x1, 0.0D0, -t8 * t79 * t34 / 0.2880D4)
      bggbH1n1em1 = t39 * t38 - t63 * t6 * t7 * t60 / 0.5760D4 - t83 * t
     #6 * t7 * t79 * t34 / 0.2880D4

      end function



      doubleprecision function bggbH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH11J1
      doubleprecision bggbH11J2
      doubleprecision bggbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = 0.90D2 * t1
      t7 = 0.1D1 / s
      t9 = bggbH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t12 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, t6 * t7 * t9 / 0.5760D4)
      bggbH1n1em2 = t12 * t6 * t7 * t9 / 0.5760D4

      end function



      doubleprecision function bggbH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH11J1
      doubleprecision bggbH11J2
      doubleprecision bggbH11J3
      bggbH1n1em3 = 0.0D0

      end function



      doubleprecision function bggbH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH11J1
      doubleprecision bggbH11J2
      doubleprecision bggbH11J3
      bggbH1n1em4 = 0.0D0

      end function
  
 

      doubleprecision function bggbH11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = 0.1D1 - x1
      t7 = t6 ** 2
      t8 = t5 * t7
      t9 = t2 * z * t8
      t10 = 0.1D1 - x2
      t13 = z + x1 * t10 * t4
      t14 = x1 * t4
      t15 = z + t14
      t16 = 0.1D1 / t15
      t17 = t13 * t16
      t18 = t4 * s
      t20 = x3 * t10
      t22 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t20 * t15 * x2 * t22)
      t32 = t20 * t15 + x2 * t22 - 0.2D1 * t25 * t29
      t37 = s - t18 * x1 * t16 * t32 - t18 * t6 * x3
      t39 = t17 * t37 * t22
      t43 = t2 * t5 * t7
      t46 = t37 * t2
      t48 = x2 * x1
      t50 = t48 * t6 * t16
      t54 = t17 * t37 * x3
      t56 = z ** 2
      t59 = t6 * t13
      t64 = t1 * t5
      t65 = t64 * t7
      t66 = t37 ** 2
      t72 = t1 * t4 * t6
      t78 = t2 * t4 * t6
      t83 = t66 * t1
      t84 = t83 * z
      t89 = t5 * t4
      t90 = t89 * x2
      t91 = t46 * t90
      t92 = x1 * t7
      t93 = t16 * z
      t102 = 0.8D1 * t9 * t39 + 0.7D1 * t43 * t39 - 0.72D2 * t46 * t5 * 
     #t50 + t43 * t54 + 0.16D2 * t2 * t56 * t4 * t59 * t16 * t37 + 0.15D
     #2 * t65 * t17 * t66 * x3 + 0.16D2 * t72 * t17 * t66 * z - 0.20D2 *
     # t78 * t17 * t37 * z - 0.8D1 * t84 * t14 * t16 * t32 + 0.27D2 * t9
     #1 * t92 * t93 * t22 + 0.30D2 * t91 * t92 * t93 * x3
      t103 = t5 ** 2
      t105 = t46 * t103 * x2
      t106 = t6 * t7
      t107 = x1 * t106
      t108 = t16 * t22
      t113 = t83 * t90
      t114 = t92 * t108
      t118 = t15 ** 2
      t119 = 0.1D1 / t118
      t120 = t119 * t66
      t126 = t92 * t16 * x3
      t129 = x3 ** 2
      t135 = t106 * t13
      t136 = t2 * t103 * t135
      t137 = t119 * t37
      t144 = t5 * x2
      t145 = t46 * t144
      t146 = x1 * t6
      t147 = t146 * t93
      t150 = t1 ** 2
      t151 = t150 * z
      t161 = t7 * t13
      t162 = t2 * t89 * t161
      t168 = t4 * t6
      t169 = t168 * x3
      t171 = -0.19D2 * t105 * t107 * t108 * x3 - 0.19D2 * t113 * t114 + 
     #0.21D2 * t64 * t59 * t120 * x1 * t32 - 0.11D2 * t113 * t126 - 0.11
     #D2 * t105 * t107 * t16 * t129 + 0.9D1 * t136 * t137 * t48 * t22 + 
     #0.8D1 * t9 * t54 - 0.112D3 * t145 * t147 + 0.8D1 * t151 * t103 * t
     #106 * x3 * t13 * t119 * x2 * x1 - 0.27D2 * t162 * t137 * t48 * z -
     # 0.9D1 * t46 - t46 * t169
      t175 = t46 * z
      t199 = -t46 * t8 * t129 + 0.27D2 * t175 - 0.36D2 * t46 * t56 + 0.8
     #D1 * t84 - 0.16D2 * t1 * t56 * t66 + 0.16D2 * t2 * t56 * z * t37 +
     # 0.8D1 * t84 * t168 * t22 - 0.24D2 * t72 * t17 * t66 + 0.3D1 * t17
     #5 * t169 + t78 * t17 * t37 + 0.72D2 * t91 * t114
      t203 = t22 ** 2
      t230 = x1 ** 2
      t248 = x2 ** 2
      t256 = -0.45D2 * t162 * t137 * t48 - 0.9D1 * t105 * t107 * t16 * t
     #203 - 0.26D2 * t151 * t89 * t7 * t13 * t119 * t48 - 0.36D2 * t145 
     #* t146 * t16 * t56 + 0.30D2 * t83 * t144 * t147 - 0.14D2 * t1 * t8
     #9 * t161 * t120 * t48 + 0.21D2 * t65 * t17 * t66 * t22 + 0.11D2 * 
     #t113 * t230 * t6 * t119 * t32 + 0.58D2 * t91 * t126 + 0.10D2 * t13
     #6 * t137 * t48 * x3 + 0.9D1 * t2 * t103 * t4 * t135 / t118 / t15 *
     # t37 * t248 * t230 + 0.45D2 * t83 * t5 * t50
      bggbH11J1 = 0.16D2 / 0.3D1 * wd * (t102 + t171 + t199 + t256) / t6
     #6 / s

      end function
  
   
 

      doubleprecision function bggbH11J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = 0.1D1 - x1
      t7 = t6 ** 2
      t8 = t5 * t7
      t9 = t2 * z * t8
      t10 = 0.1D1 - x2
      t13 = z + x1 * t10 * t4
      t14 = x1 * t4
      t15 = z + t14
      t16 = 0.1D1 / t15
      t17 = t13 * t16
      t18 = t4 * s
      t20 = x3 * t10
      t22 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t20 * t15 * x2 * t22)
      t32 = t20 * t15 + x2 * t22 - 0.2D1 * t25 * t29
      t33 = x1 * t16 * t32
      t37 = s - t18 * t33 - t18 * t6 * x3
      t39 = t17 * t37 * x3
      t42 = t37 * t2
      t43 = t5 * x2
      t46 = t16 * z
      t47 = x1 * t6 * t46
      t50 = t5 * t4
      t52 = t7 * t13
      t53 = t2 * t50 * t52
      t54 = t15 ** 2
      t55 = 0.1D1 / t54
      t56 = t55 * t37
      t57 = x2 * x1
      t63 = t17 * t37 * t22
      t66 = t42 * z
      t68 = t37 ** 2
      t69 = t68 * t1
      t70 = t69 * z
      t71 = t50 * x2
      t72 = t42 * t71
      t73 = x1 * t7
      t74 = t16 * t22
      t75 = t73 * t74
      t79 = t1 ** 2
      t80 = t79 * z
      t81 = t5 ** 2
      t82 = t6 * t7
      t93 = t82 * t13
      t94 = t2 * t81 * t93
      t98 = t1 * t5
      t99 = t98 * t7
      t113 = t57 * t6 * t16
      t117 = t2 * t5 * t7
      t125 = t2 * t4 * t6
      t135 = t1 * t4 * t6
      t140 = -t94 * t56 * t57 * t22 - 0.22D2 * t99 * t17 * t68 * t22 - 0
     #.9D1 * t72 * t73 * t46 * t22 - 0.11D2 * t69 * t43 * t47 + 0.72D2 *
     # t42 * t5 * t113 - t117 * t39 - 0.52D2 * t69 * t5 * t113 - 0.15D2 
     #* t117 * t63 + 0.27D2 * t125 * t17 * t37 * z - 0.16D2 * t99 * t17 
     #* t68 * x3 - 0.11D2 * t135 * t17 * t68 * z
      t146 = t4 * t6
      t147 = t146 * x3
      t149 = x3 ** 2
      t158 = t69 * t71
      t175 = t55 * t68
      t180 = 0.8D1 * t70 * t14 * t16 * t32 + t42 * t147 + t42 * t8 * t14
     #9 + 0.36D2 * t80 * t50 * t7 * t13 * t55 * t57 + 0.19D2 * t158 * t7
     #5 - 0.8D1 * t69 * t4 * t33 - 0.16D2 * t70 * t146 * t22 + 0.24D2 * 
     #t135 * t17 * t68 - t66 * t147 - t125 * t17 * t37 - 0.22D2 * t98 * 
     #t6 * t13 * t175 * x1 * t32
      t182 = t73 * t16 * x3
      t186 = t42 * t81 * x2
      t187 = x1 * t82
      t192 = x1 ** 2
      t201 = t22 ** 2
      t222 = x2 ** 2
      t236 = 0.11D2 * t158 * t182 + 0.11D2 * t186 * t187 * t16 * t149 - 
     #0.12D2 * t158 * t192 * t6 * t55 * t32 + 0.52D2 * t53 * t56 * t57 +
     # 0.9D1 * t186 * t187 * t16 * t201 + 0.19D2 * t186 * t187 * t74 * x
     #3 - 0.58D2 * t72 * t182 - 0.10D2 * t94 * t56 * t57 * x3 - 0.8D1 * 
     #t2 * t81 * t4 * t93 / t54 / t15 * t37 * t222 * t192 + 0.14D2 * t1 
     #* t50 * t52 * t175 * t57 - 0.10D2 * t72 * t73 * t46 * x3
      bggbH11J2 = 0.16D2 / 0.3D1 * wd * (-0.16D2 * t9 * t39 + 0.40D2 * t
     #42 * t43 * t47 - 0.8D1 * t53 * t56 * t57 * z - 0.8D1 * t9 * t63 - 
     #0.9D1 * t66 + t70 - 0.72D2 * t72 * t75 + 0.9D1 * t42 - 0.16D2 * t8
     #0 * t81 * t82 * x3 * t13 * t55 * x2 * x1 - t69 + t140 + t180 + t23
     #6) / t68 / s

      end function
  
   
 

      doubleprecision function bggbH11J3
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
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t20 = t8 * t4 + x2 * t10 - 0.2D1 * t13 * t17
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t27 * t28
      t30 = t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t34 = t23 ** 2
      t37 = z + x1 * t7 * t1
      t39 = t30 * t32 * t34 * t37
      t40 = t4 ** 2
      t41 = 0.1D1 / t40
      t42 = t41 * t26
      t43 = x2 * x1
      t47 = t26 * t30
      t48 = t31 ** 2
      t50 = t47 * t48 * x2
      t51 = t34 * t23
      t52 = x1 * t51
      t53 = t5 * t10
      t58 = t32 * x2
      t59 = t29 * t58
      t60 = x1 * t34
      t65 = t51 * t37
      t66 = t30 * t48 * t65
      t81 = x2 ** 2
      t82 = x1 ** 2
      t86 = t28 * t31
      t94 = t1 * t23
      t95 = t94 * x3
      t98 = t31 * t34
      t99 = x3 ** 2
      t102 = t47 * t58
      t103 = t5 * z
      t108 = t31 * x2
      t111 = x1 * t23 * t103
      t121 = t60 * t5 * x3
      t128 = t29 + 0.18D2 * t39 * t42 * t43 + 0.9D1 * t50 * t52 * t53 * 
     #x3 + 0.9D1 * t59 * t60 * t53 - 0.8D1 * t66 * t42 * t43 * t10 - 0.9
     #D1 * t66 * t42 * t43 * x3 - t30 * t48 * t1 * t65 / t40 / t4 * t26 
     #* t81 * t82 - 0.9D1 * t86 * t23 * t37 * t41 * t27 * x1 * t20 + 0.9
     #D1 * t47 * t95 + t47 * t98 * t99 - 0.9D1 * t102 * t60 * t103 * x3 
     #- 0.8D1 * t29 * t108 * t111 + 0.36D2 * t47 * t108 * t111 + t39 * t
     #42 * t43 * z + 0.10D2 * t59 * t121 + 0.10D2 * t50 * t52 * t5 * t99
      t134 = t86 * t34
      t135 = t37 * t5
      t141 = t30 * t31 * t34
      t147 = t135 * t26 * x3
      t154 = t28 * t1 * t23
      t159 = t28 ** 2
      t160 = t159 * z
      t171 = t29 * z
      t205 = -0.18D2 * t29 * t31 * t43 * t23 * t5 - 0.9D1 * t134 * t135 
     #* t27 * t10 + 0.9D1 * t141 * t135 * t26 * t10 - t141 * t147 - 0.11
     #D2 * t134 * t135 * t27 * x3 - 0.8D1 * t154 * t135 * t27 * z + 0.8D
     #1 * t160 * t48 * t51 * x3 * t37 * t41 * x2 * x1 - 0.36D2 * t102 * 
     #t121 + 0.7D1 * t171 + 0.8D1 * t30 * z * t98 * t147 + 0.8D1 * t29 *
     # t1 * t21 + 0.8D1 * t171 * t94 * t10 + 0.9D1 * t154 * t135 * t27 -
     # 0.9D1 * t47 * z * t95 - 0.9D1 * t30 * t1 * t23 * t135 * t26 - 0.1
     #0D2 * t160 * t32 * t34 * t37 * t41 * t43 - 0.9D1 * t59 * t82 * t23
     # * t41 * t20
      bggbH11J3 = 0.16D2 / 0.3D1 * wd * (t128 + t205) / t27 / s

      end function
  
 