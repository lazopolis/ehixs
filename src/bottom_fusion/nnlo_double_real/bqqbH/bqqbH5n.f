  
      subroutine bqqbH5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bqqbH5n1e1  
      doubleprecision bqqbH5n1e0  
      doubleprecision bqqbH5n1em1  
      doubleprecision bqqbH5n1em2  
      doubleprecision bqqbH5n1em3  
      doubleprecision bqqbH5n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bqqbH5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bqqbH5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bqqbH5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bqqbH5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bqqbH5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bqqbH5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bqqbH5n1e1
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
      t17 = lh ** 2
      t19 = 0.3141592653589793D1 ** 2
      t21 = 0.180D3 * t17 - 0.30D2 * t19
      t23 = t15 ** 2
      t37 = t1 ** 2
      t38 = t37 * t1
      t40 = x2 * wd
      t41 = x2 * z
      t43 = (0.1D1 - x2 + t41) ** 2
      t44 = 0.1D1 / t43
      t45 = t40 * t44
      t48 = z * lh
      t49 = t48 * t38
      t50 = x3 * t8
      t51 = t10 * x2
      t52 = t51 * t4
      t55 = log(-0.4D1 * t50 * t52)
      t61 = z * t38 * x2
      t62 = wd * t44
      t63 = t55 ** 2
      t68 = z * t21 * t38
      t69 = t68 * t45
      t71 = 0.1D1 / x3
      t74 = x1 ** 2
      t75 = x3 * t74
      t76 = t75 * t8
      t79 = log(-0.4D1 * t76 * t52)
      t85 = 0.180D3 * t49 * t45
      t88 = 0.1D1 / x1
      t91 = t74 * t8
      t94 = log(-0.4D1 * t91 * t52)
      t99 = t94 ** 2
      t107 = -(-t15 * z * t21 - 0.90D2 * t23 * z * lh + z * (0.60D2 * lh
     # * t19 - 0.2884936567583026D3 - 0.120D3 * t17 * lh) - 0.15D2 * t23
     # * t15 * z) * t38 * t45 / 0.180D3 + (-0.180D3 * t49 * t40 * t44 * 
     #t55 - 0.45D2 * t61 * t62 * t63 - t69) * t71 / 0.180D3 - (-0.90D2 *
     # t61 * wd * t79 * t44 - t85) * t71 * t88 / 0.90D2 + (-0.180D3 * t4
     #9 * t40 * t94 * t44 - 0.45D2 * t61 * wd * t99 * t44 - t69) * t88 /
     # 0.90D2
      t108 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #107)
      t110 = -0.1D1 + x1
      t111 = x3 * x1
      t112 = t111 * z
      t114 = 0.2D1 * x2 * x3
      t115 = t111 * x2
      t116 = t111 * t41
      t117 = cos(t6)
      t119 = x1 * z
      t120 = 0.1D1 - x1 + t119
      t122 = -0.1D1 + x3
      t125 = Sqrt(x3 * t4 * t120 * x2 * t122)
      t127 = 0.2D1 * t117 * t125
      t130 = 0.1D1 / t120
      t134 = x1 * x2
      t135 = t134 * z
      t136 = 0.1D1 - x1 + t119 - x2 + t134 - t135 - x3 + t111 - t112 + t
     #114 - t115 + t116 + t127
      t146 = s * t37 * x2 * x1 * t110 * t130
      t147 = t75 * t11
      t148 = t110 ** 2
      t149 = t130 * t148
      t154 = log(0.4D1 * t147 * t12 * t149 * t122)
      t157 = (-0.1D1 + x1 - t119 + x2 - t134 - t41 + t135) ** 2
      t159 = t148 / t157
      t163 = t40 * t159
      t165 = 0.180D3 * t49 * t163
      t166 = -0.90D2 * t61 * wd * t154 * t159 - t165
      t170 = FJET(XB1, XB2, s, t2 * t110 * (-x3 + t111 - t112 + t114 - t
     #115 + t116 - x2 + t127) * t130, t2 * t111, -t2 * t110 * t136 * t13
     #0, -t2 * x1 * t122, -t146, -t166 * t71 * t88 / 0.90D2)
      t177 = Sqrt(t12 * x3 * t122)
      t179 = 0.2D1 * t117 * t177
      t188 = log(0.4D1 * t50 * t10 * t12 * t122)
      t193 = t188 ** 2
      t204 = log(0.4D1 * t76 * t51 * t4 * t122)
      t213 = (0.180D3 * t49 * t40 * t44 * t188 + 0.45D2 * t61 * t62 * t1
     #93 + t69) * t71 / 0.180D3 - (0.90D2 * t61 * wd * t204 * t44 + t85)
     # * t71 * t88 / 0.90D2
      t214 = FJET(XB1, XB2, s, -t2 * (-x3 + t114 - x2 + t179), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t114 + t179), 0.0D0, 0.0D0, t213)
      t223 = t12 * t149
      t226 = log(-0.4D1 * t147 * t223)
      t239 = log(-0.4D1 * t91 * t10 * t223)
      t244 = t239 ** 2
      t253 = -(0.90D2 * t61 * wd * t226 * t159 + t165) * t71 * t88 / 0.9
     #0D2 + (0.180D3 * t48 * t38 * x2 * wd * t239 * t159 + 0.45D2 * t61 
     #* wd * t244 * t159 + t68 * t163) * t88 / 0.90D2
      t254 = FJET(XB1, XB2, s, -t2 * t110 * x2 * t130, 0.0D0, t4 * t110 
     #* t2, x1 * t1 * s, -t146, t253)
      bqqbH5n1e1 = t108 * t107 - t170 * t166 * t71 * t88 / 0.90D2 + t214
     # * t213 + t254 * t253

      end function



      doubleprecision function bqqbH5n1e0
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
      t7 = t6 * t1
      t8 = z * t7
      t9 = t8 * x2
      t10 = x2 * z
      t12 = (0.1D1 - x2 + t10) ** 2
      t13 = 0.1D1 / t12
      t14 = wd * t13
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = x3 * t17
      t19 = z ** 2
      t20 = 0.1D1 / t19
      t22 = t20 * x2 * t4
      t25 = log(-0.4D1 * t18 * t22)
      t30 = z * lh * t7
      t31 = x2 * wd
      t32 = t31 * t13
      t34 = 0.180D3 * t30 * t32
      t36 = 0.1D1 / x3
      t39 = 0.1D1 / x1
      t40 = t36 * t39
      t42 = t9 * t14 * t40
      t43 = x1 ** 2
      t44 = t43 * t17
      t47 = log(-0.4D1 * t44 * t22)
      t56 = t4 * x2
      t59 = log(-0.4D1 * t17 * t20 * t56)
      t63 = lh ** 2
      t65 = 0.3141592653589793D1 ** 2
      t69 = t59 ** 2
      t76 = (0.90D2 * t9 * t14 * t25 + t34) * t36 / 0.180D3 - t42 + (0.9
     #0D2 * t9 * wd * t47 * t13 + t34) * t39 / 0.90D2 - (0.180D3 * t59 *
     # z * lh + z * (0.180D3 * t63 - 0.30D2 * t65) + 0.45D2 * t69 * z) *
     # t7 * t32 / 0.180D3
      t77 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #6)
      t79 = -0.1D1 + x1
      t80 = x3 * x1
      t81 = t80 * z
      t83 = 0.2D1 * x2 * x3
      t84 = t80 * x2
      t85 = t80 * t10
      t86 = cos(t15)
      t88 = x1 * z
      t89 = 0.1D1 - x1 + t88
      t91 = -0.1D1 + x3
      t94 = Sqrt(x3 * t4 * t89 * x2 * t91)
      t96 = 0.2D1 * t86 * t94
      t99 = 0.1D1 / t89
      t103 = x1 * x2
      t104 = t103 * z
      t105 = 0.1D1 - x1 + t88 - x2 + t103 - t104 - x3 + t80 - t81 + t83 
     #- t84 + t85 + t96
      t115 = s * t6 * x2 * x1 * t79 * t99
      t117 = t79 ** 2
      t119 = (-0.1D1 + x1 - t88 + x2 - t103 - t10 + t104) ** 2
      t120 = 0.1D1 / t119
      t121 = t117 * t120
      t123 = t8 * t31 * t121 * t40
      t124 = FJET(XB1, XB2, s, t2 * t79 * (-x3 + t80 - t81 + t83 - t84 +
     # t85 - x2 + t96) * t99, t2 * t80, -t2 * t79 * t105 * t99, -t2 * x1
     # * t91, -t115, -t123)
      t135 = Sqrt(t56 * x3 * t91)
      t137 = 0.2D1 * t86 * t135
      t146 = log(0.4D1 * t18 * t20 * t56 * t91)
      t153 = (-0.90D2 * t9 * t14 * t146 - t34) * t36 / 0.180D3 + t42
      t154 = FJET(XB1, XB2, s, -t2 * (-x3 + t83 - x2 + t137), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t83 + t137), 0.0D0, 0.0D0, t153)
      t168 = log(-0.4D1 * t44 * t20 * t56 * t99 * t117)
      t179 = t123 + (-0.90D2 * t9 * wd * t168 * t121 - 0.180D3 * t30 * t
     #31 * t121) * t39 / 0.90D2
      t180 = FJET(XB1, XB2, s, -t2 * t79 * x2 * t99, 0.0D0, t4 * t79 * t
     #2, x1 * t1 * s, -t115, t179)
      bqqbH5n1e0 = t76 * t77 - t124 * z * t7 * x2 * wd * t117 * t120 * t
     #36 * t39 + t154 * t153 + t180 * t179

      end function



      doubleprecision function bqqbH5n1em1
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
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t14 = t4 * x2
      t17 = log(-0.4D1 * t10 / t11 * t14)
      t21 = t1 ** 2
      t22 = t21 * t1
      t24 = x2 * wd
      t25 = x2 * z
      t27 = (0.1D1 - x2 + t25) ** 2
      t28 = 0.1D1 / t27
      t33 = z * t22 * x2
      t34 = wd * t28
      t35 = 0.1D1 / x3
      t38 = t33 * t34 * t35 / 0.2D1
      t39 = 0.1D1 / x1
      t42 = -(-0.180D3 * z * lh - 0.90D2 * t17 * z) * t22 * t24 * t28 / 
     #0.180D3 - t38 - t33 * t34 * t39
      t43 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #2)
      t46 = 0.2D1 * x2 * x3
      t47 = cos(t8)
      t51 = Sqrt(t14 * x3 * (-0.1D1 + x3))
      t53 = 0.2D1 * t47 * t51
      t58 = FJET(XB1, XB2, s, -t2 * (-x3 + t46 - x2 + t53), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t46 + t53), 0.0D0, 0.0D0, t38)
      t65 = -0.1D1 + x1
      t67 = x1 * z
      t69 = 0.1D1 / (0.1D1 - x1 + t67)
      t81 = t65 ** 2
      t83 = x1 * x2
      t86 = (-0.1D1 + x1 - t67 + x2 - t83 - t25 + t83 * z) ** 2
      t89 = wd * t81 / t86 * t39
      t91 = FJET(XB1, XB2, s, -t2 * t65 * x2 * t69, 0.0D0, t4 * t65 * t2
     #, x1 * t1 * s, -s * t21 * x2 * x1 * t65 * t69, t33 * t89)
      bqqbH5n1em1 = t43 * t42 + t58 * z * t22 * t24 * t28 * t35 / 0.2D1 
     #+ t91 * z * t22 * x2 * t89

      end function



      doubleprecision function bqqbH5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = t1 ** 2
      t7 = t6 * t1
      t12 = (0.1D1 - x2 + x2 * z) ** 2
      t14 = x2 * wd / t12
      t17 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * (-0.1D1 + x2), 0.0D0
     #, 0.0D0, -z * t7 * t14 / 0.2D1)
      bqqbH5n1em2 = -t17 * z * t7 * t14 / 0.2D1

      end function



      doubleprecision function bqqbH5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bqqbH5n1em3 = 0.0D0

      end function



      doubleprecision function bqqbH5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bqqbH5n1em4 = 0.0D0

      end function
