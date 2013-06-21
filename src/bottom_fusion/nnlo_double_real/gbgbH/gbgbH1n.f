  
      subroutine gbgbH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH11J1  
      doubleprecision gbgbH11J2  
      doubleprecision gbgbH11J3  
      doubleprecision gbgbH1n1e1  
      doubleprecision gbgbH1n1e0  
      doubleprecision gbgbH1n1em1  
      doubleprecision gbgbH1n1em2  
      doubleprecision gbgbH1n1em3  
      doubleprecision gbgbH1n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH11J1
      doubleprecision gbgbH11J2
      doubleprecision gbgbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z ** 2
      t8 = 0.1D1 / t6 / z
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t16 = log(-0.4D1 * t12 * x3 * t4)
      t19 = lh ** 2
      t20 = 0.180D3 * t19
      t21 = 0.3141592653589793D1 ** 2
      t22 = 0.30D2 * t21
      t23 = t16 ** 2
      t27 = 0.1D1 / s
      t28 = gbgbH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t32 = t20 - t22
      t44 = gbgbH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t45 = t27 * t44
      t48 = lh * t1
      t49 = gbgbH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t50 = x2 * x3
      t52 = -0.1D1 + x2
      t57 = log(0.4D1 * t50 * t8 * t11 * t52 * t4)
      t58 = gbgbH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t60 = t12 * t4
      t63 = log(-0.4D1 * t50 * t60)
      t70 = 0.90D2 * t1 * t27
      t71 = gbgbH11J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t73 = t63 ** 2
      t76 = gbgbH11J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t78 = t57 ** 2
      t83 = t32 * t1
      t84 = t58 - t44
      t88 = 0.1D1 / x2
      t98 = x1 ** 2
      t99 = t50 * t98
      t102 = log(-0.4D1 * t99 * t60)
      t108 = log(0.4D1 * t99 * t12 * t52 * t4)
      t118 = 0.1D1 / x1
      t121 = x3 * t98
      t124 = log(-0.4D1 * t121 * t60)
      t131 = t124 ** 2
      t140 = (0.180D3 * t16 * lh + t20 - t22 + 0.45D2 * t23) * t1 * t27 
     #* t28 / 0.5760D4 + (-t16 * t32 - 0.90D2 * t23 * lh - 0.28849365675
     #83026D3 - 0.120D3 * t19 * lh + 0.60D2 * lh * t21 - 0.15D2 * t23 * 
     #t16) * t1 * t45 / 0.5760D4 - (-0.180D3 * t48 * t27 * (t49 - t57 * 
     #t58 - t28 + t63 * t44) + t70 * (-t71 + t63 * t28 - t73 * t44 / 0.2
     #D1 + t76 - t57 * t49 + t78 * t58 / 0.2D1) + t83 * t27 * t84) * t88
     # / 0.5760D4 + (-0.180D3 * lh - 0.90D2 * t16) * t1 * t27 * t71 / 0.
     #5760D4 + (t70 * (-t102 * t44 - t49 + t108 * t58 + t28) + 0.180D3 *
     # t48 * t27 * t84) * t88 * t118 / 0.2880D4 + (-0.180D3 * t48 * t27 
     #* (t28 - t124 * t44) + t70 * (t71 - t124 * t28 + t131 * t44 / 0.2D
     #1) + t83 * t45) * t118 / 0.2880D4
      t141 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #140)
      t143 = x1 * x3
      t145 = -0.1D1 + x1
      t147 = t2 * t145 * x3
      t148 = t4 * s
      t152 = t148 * t1 * t145
      t153 = gbgbH11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t155 = t50 * t98 * t11
      t156 = 0.1D1 / t6
      t157 = t145 ** 2
      t159 = x1 * z
      t160 = -z - x1 + t159
      t161 = 0.1D1 / t160
      t163 = t156 * t157 * t161 * t4
      t166 = log(0.4D1 * t155 * t163)
      t167 = gbgbH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t171 = t27 * t167
      t180 = log(0.4D1 * t121 * t11 * t163)
      t186 = gbgbH11J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t188 = t180 ** 2
      t197 = (t70 * (-t153 + t166 * t167) + 0.180D3 * t48 * t171) * t88 
     #* t118 / 0.2880D4 + (0.180D3 * t48 * t27 * (t153 - t180 * t167) - 
     #t70 * (t186 - t180 * t153 + t188 * t167 / 0.2D1) - t83 * t171) * t
     #118 / 0.2880D4
      t198 = FJET(XB1, XB2, s, t2 * t143, -t147, -t148 * t1 * x1, t152, 
     #0.0D0, t197)
      t200 = x3 * z
      t201 = t143 * z
      t202 = t50 * z
      t203 = t50 * x1
      t204 = t50 * t159
      t205 = cos(t9)
      t210 = Sqrt(-x3 * t52 * t160 * x2 * t4)
      t212 = 0.2D1 * t205 * t210
      t218 = x2 * x1
      t220 = z + x1 - t159 - x2 * z - t218 + t218 * z - t200 - t143 + t2
     #01 + t202 + t203 - t204 + t50 + t212
      t224 = t1 ** 2
      t230 = gbgbH11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t237 = log(-0.4D1 * t155 * t156 * t161 * t157 * t52 * t4)
      t238 = gbgbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t245 = t70 * (t230 - t237 * t238) - 0.180D3 * t48 * t27 * t238
      t249 = FJET(XB1, XB2, s, t2 * x1 * (-t200 - t143 + t201 + t202 + t
     #203 - t204 - x2 + t50 + t212) * t161, -t147, -t2 * x1 * t220 * t16
     #1, t152, s * t224 * x2 * x1 * t145 * t161, t245 * t88 * t118 / 0.2
     #880D4)
      gbgbH1n1e1 = t141 * t140 + t198 * t197 + t249 * t245 * t88 * t118 
     #/ 0.2880D4

      end function



      doubleprecision function gbgbH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH11J1
      doubleprecision gbgbH11J2
      doubleprecision gbgbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.90D2 * t1
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = gbgbH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t10 = x2 * x3
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = -0.1D1 + x2
      t23 = log(0.4D1 * t10 * t13 * t17 * t18 * t4)
      t24 = gbgbH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t26 = gbgbH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t27 = t13 * t17
      t28 = t27 * t4
      t31 = log(-0.4D1 * t10 * t28)
      t32 = gbgbH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t36 = lh * t1
      t37 = t24 - t32
      t42 = 0.1D1 / x2
      t47 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = x3 * t51
      t55 = log(-0.4D1 * t52 * t28)
      t59 = t7 * t32
      t65 = gbgbH11J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t72 = log(-0.4D1 * t27 * x3 * t4)
      t81 = lh ** 2
      t83 = 0.3141592653589793D1 ** 2
      t85 = t72 ** 2
      t91 = -(t8 * (t9 - t23 * t24 - t26 + t31 * t32) - 0.180D3 * t36 * 
     #t7 * t37) * t42 / 0.5760D4 - t8 * t37 * t42 * t47 / 0.2880D4 + (t8
     # * (t26 - t55 * t32) - 0.180D3 * t36 * t59) * t47 / 0.2880D4 + t8 
     #* t65 / 0.5760D4 + (-0.180D3 * lh - 0.90D2 * t72) * t1 * t7 * t26 
     #/ 0.5760D4 + (0.180D3 * t72 * lh + 0.180D3 * t81 - 0.30D2 * t83 + 
     #0.45D2 * t85) * t1 * t59 / 0.5760D4
      t92 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t9
     #1)
      t94 = x1 * x3
      t96 = -0.1D1 + x1
      t98 = t2 * t96 * x3
      t99 = t4 * s
      t103 = t99 * t1 * t96
      t104 = gbgbH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t108 = gbgbH11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t111 = x1 * z
      t112 = -z - x1 + t111
      t113 = 0.1D1 / t112
      t115 = t96 ** 2
      t120 = log(0.4D1 * t52 * t17 / t11 * t113 * t115 * t4)
      t130 = -t8 * t104 * t42 * t47 / 0.2880D4 + (-t8 * (t108 - t120 * t
     #104) + 0.180D3 * t36 * t7 * t104) * t47 / 0.2880D4
      t131 = FJET(XB1, XB2, s, t2 * t94, -t98, -t99 * t1 * x1, t103, 0.0
     #D0, t130)
      t133 = x3 * z
      t134 = t94 * z
      t135 = t10 * z
      t136 = t10 * x1
      t137 = t10 * t111
      t138 = cos(t15)
      t143 = Sqrt(-x3 * t18 * t112 * x2 * t4)
      t145 = 0.2D1 * t138 * t143
      t151 = x2 * x1
      t153 = z + x1 - t111 - x2 * z - t151 + t151 * z - t133 - t94 + t13
     #4 + t135 + t136 - t137 + t10 + t145
      t157 = t1 ** 2
      t163 = gbgbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t165 = t163 * t42 * t47
      t168 = FJET(XB1, XB2, s, t2 * x1 * (-t133 - t94 + t134 + t135 + t1
     #36 - t137 - x2 + t10 + t145) * t113, -t98, -t2 * x1 * t153 * t113,
     # t103, s * t157 * x2 * x1 * t96 * t113, t8 * t165 / 0.2880D4)
      gbgbH1n1e0 = t92 * t91 + t131 * t130 + t168 * t6 * t7 * t165 / 0.2
     #880D4

      end function



      doubleprecision function gbgbH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH11J1
      doubleprecision gbgbH11J2
      doubleprecision gbgbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.90D2 * t1
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = gbgbH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t13 = z ** 2
      t17 = Sin(x4 * 0.3141592653589793D1)
      t18 = t17 ** 2
      t23 = log(-0.4D1 / t13 / z * t18 * x3 * t4)
      t27 = gbgbH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t31 = gbgbH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t37 = 0.1D1 / x1
      t41 = t8 * t9 / 0.5760D4 + (-0.180D3 * lh - 0.90D2 * t23) * t1 * t
     #7 * t27 / 0.5760D4 - t8 * (t31 - t27) / x2 / 0.5760D4 + t8 * t27 *
     # t37 / 0.2880D4
      t42 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t4
     #1)
      t46 = -0.1D1 + x1
      t49 = t4 * s
      t54 = gbgbH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t58 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t46 * x3, -t49 * t1 * 
     #x1, t49 * t1 * t46, 0.0D0, -t8 * t54 * t37 / 0.2880D4)
      gbgbH1n1em1 = t42 * t41 - t58 * t6 * t7 * t54 * t37 / 0.2880D4

      end function



      doubleprecision function gbgbH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH11J1
      doubleprecision gbgbH11J2
      doubleprecision gbgbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = 0.90D2 * t1
      t7 = 0.1D1 / s
      t9 = gbgbH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, t6 * t7 * t9 / 0.5760D4)
      gbgbH1n1em2 = t12 * t6 * t7 * t9 / 0.5760D4

      end function



      doubleprecision function gbgbH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH11J1
      doubleprecision gbgbH11J2
      doubleprecision gbgbH11J3
      gbgbH1n1em3 = 0.0D0

      end function



      doubleprecision function gbgbH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH11J1
      doubleprecision gbgbH11J2
      doubleprecision gbgbH11J3
      gbgbH1n1em4 = 0.0D0

      end function
  
 

      doubleprecision function gbgbH11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t31 * x2
      t33 = t29 * t32
      t34 = x1 ** 2
      t35 = t34 * t23
      t36 = t4 ** 2
      t37 = 0.1D1 / t36
      t38 = t37 * t20
      t39 = t35 * t38
      t42 = t30 ** 2
      t44 = t34 * x1
      t46 = t28 * t42 * t44 * t26
      t47 = x2 * t23
      t48 = t47 * t38
      t51 = t28 * t1
      t52 = x1 * t26
      t54 = t26 ** 2
      t55 = t27 * t54
      t58 = t37 * z
      t64 = t29 * t42 * x2
      t65 = t44 * t23
      t67 = 0.1D1 / t36 / t4
      t71 = t10 * t7 * t4 + x2 * x3 + t19
      t72 = t71 ** 2
      t77 = t55 * t32
      t78 = t37 * t71
      t79 = t35 * t78
      t86 = t55 * z
      t87 = t5 * t71
      t94 = 0.58D2 * t33 * t39 + 0.10D2 * t46 * t48 + t51 * t52 - 0.24D2
     # * t55 * t3 + 0.30D2 * t33 * t35 * t58 * t20 - 0.9D1 * t64 * t65 *
     # t67 * t72 - 0.19D2 * t77 * t79 + 0.9D1 * t46 * t47 * t78 - 0.9D1 
     #* t29 + 0.8D1 * t86 * t3 * t87 + 0.16D2 * t55 * t3 * z
      t98 = z ** 2
      t104 = t28 * t31
      t107 = t23 * t5
      t111 = t29 * t30
      t113 = x2 * x1 * t107
      t116 = t29 * z
      t117 = t5 * t20
      t121 = t30 * t34
      t127 = t34 * x2 * t107
      t133 = x2 ** 2
      t135 = t23 ** 2
      t140 = t55 * t30
      t146 = t30 * x2
      t147 = t29 * t146
      t148 = x1 * t23
      t149 = t5 * z
      t150 = t148 * t149
      t155 = -0.20D2 * t51 * t52 * z + 0.16D2 * t28 * t98 * t26 * t1 * x
     #1 - 0.45D2 * t104 * t34 * t26 * x2 * t107 - 0.72D2 * t111 * t113 +
     # 0.3D1 * t116 * t3 * t117 + 0.8D1 * t116 * t121 * t87 - 0.14D2 * t
     #55 * t31 * t127 + 0.9D1 * t28 * t42 * t1 * t44 * t26 * t133 * t135
     # * t37 + 0.45D2 * t140 * t113 + 0.8D1 * t116 * t121 * t117 - 0.112
     #D3 * t147 * t150 + 0.72D2 * t33 * t79
      t179 = t27 ** 2
      t180 = t179 * z
      t186 = t20 ** 2
      t190 = t28 * t30 * t34
      t191 = t26 * t5
      t195 = 0.27D2 * t33 * t35 * t58 * t71 - 0.27D2 * t104 * t34 * t26 
     #* t47 * t149 - 0.11D2 * t77 * t39 + 0.27D2 * t116 - 0.16D2 * t27 *
     # t98 * t54 + 0.16D2 * t28 * t98 * z * t26 + 0.8D1 * t86 - 0.36D2 *
     # t29 * t98 + 0.8D1 * t180 * t42 * t44 * t48 - t111 * t34 * t37 * t
     #186 + 0.7D1 * t190 * t191 * t71
      t199 = t34 * t5
      t238 = 0.21D2 * t140 * t148 * x3 + 0.15D2 * t140 * t199 * t20 + t1
     #90 * t191 * t20 - t29 * t1 * t21 - 0.8D1 * t86 * t1 * t23 * x3 + 0
     #.21D2 * t140 * t199 * t71 + 0.30D2 * t55 * t146 * t150 - 0.36D2 * 
     #t147 * t148 * t5 * t98 - 0.11D2 * t64 * t65 * t67 * t186 - 0.26D2 
     #* t180 * t31 * t127 - 0.19D2 * t64 * t65 * t67 * t71 * t20 + 0.11D
     #2 * t77 * x1 * t135 * t5 * x3
      gbgbH11J1 = 0.16D2 / 0.3D1 * wd * (t94 + t155 + t195 + t238) / t54
     # / s

      end function
  
   
 

      doubleprecision function gbgbH11J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t29 * z
      t31 = t1 ** 2
      t32 = x1 ** 2
      t33 = t31 * t32
      t37 = t10 * t7 * t4 + x2 * x3 + t19
      t38 = t5 * t37
      t42 = t26 ** 2
      t43 = t42 * t27
      t44 = t31 * t1
      t47 = t23 * t5
      t48 = t32 * x2 * t47
      t51 = t27 ** 2
      t52 = t51 * z
      t53 = t31 ** 2
      t54 = t32 * x1
      t57 = t4 ** 2
      t58 = 0.1D1 / t57
      t59 = t58 * t20
      t60 = x2 * t23
      t61 = t59 * t60
      t67 = x2 ** 2
      t69 = t23 ** 2
      t74 = t44 * x2
      t75 = t43 * t74
      t76 = t32 * t23
      t77 = t76 * t59
      t80 = t5 * t20
      t84 = t29 * t53 * x2
      t85 = t54 * t23
      t87 = 0.1D1 / t57 / t4
      t93 = t29 * t74
      t94 = t58 * z
      t99 = t28 * t44
      t102 = t5 * z
      t112 = t37 ** 2
      t117 = t29 * t31
      t119 = x2 * x1 * t47
      t123 = t43 * t31
      t129 = t43 * z
      t133 = t28 * t1
      t134 = x1 * t26
      t142 = 0.9D1 * t84 * t85 * t87 * t112 + 0.72D2 * t117 * t119 + 0.9
     #D1 * t29 - 0.52D2 * t123 * t119 - 0.16D2 * t30 * t33 * t80 - 0.16D
     #2 * t129 * t3 * t38 - t133 * t134 + 0.24D2 * t43 * t3 + t129 - 0.9
     #D1 * t30 - 0.11D2 * t43 * t3 * z
      t145 = t1 * t23 * x3
      t151 = t58 * t37
      t152 = t76 * t151
      t157 = t28 * t53 * t54 * t26
      t160 = t31 * x2
      t162 = x1 * t23
      t163 = t162 * t102
      t180 = t20 ** 2
      t187 = -0.8D1 * t43 * t145 + 0.27D2 * t133 * t134 * z + 0.19D2 * t
     #75 * t152 - t157 * t60 * t151 + 0.40D2 * t29 * t160 * t163 - 0.72D
     #2 * t93 * t152 - 0.11D2 * t43 * t160 * t163 - 0.10D2 * t93 * t76 *
     # t94 * t20 - 0.12D2 * t75 * x1 * t69 * t5 * x3 + 0.11D2 * t84 * t8
     #5 * t87 * t180 - 0.58D2 * t93 * t77
      t193 = t32 * t5
      t198 = t28 * t31 * t32
      t199 = t26 * t5
      t218 = -0.10D2 * t157 * t61 - 0.22D2 * t123 * t162 * x3 - 0.16D2 *
     # t123 * t193 * t20 - t198 * t199 * t20 + t29 * t1 * t21 + t117 * t
     #32 * t58 * t180 - 0.15D2 * t198 * t199 * t37 + 0.8D1 * t129 * t145
     # - 0.22D2 * t123 * t193 * t37 + 0.36D2 * t52 * t44 * t48 - t43
      gbgbH11J2 = 0.16D2 / 0.3D1 * wd * (-0.8D1 * t30 * t33 * t38 + 0.14
     #D2 * t43 * t44 * t48 - 0.16D2 * t52 * t53 * t54 * t61 - 0.8D1 * t2
     #8 * t53 * t1 * t54 * t26 * t67 * t69 * t58 + 0.11D2 * t75 * t77 - 
     #t30 * t3 * t80 + 0.19D2 * t84 * t85 * t87 * t37 * t20 - 0.9D1 * t9
     #3 * t76 * t94 * t37 - 0.8D1 * t99 * t32 * t26 * t60 * t102 + 0.52D
     #2 * t99 * t32 * t26 * x2 * t47 + t142 + t187 + t218) / t42 / s

      end function
  
   
 

      doubleprecision function gbgbH11J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t7 = x1 ** 2
      t8 = t7 * x1
      t9 = s * t3
      t10 = x1 * t3
      t11 = z + t10
      t12 = 0.1D1 / t11
      t14 = 0.1D1 - x2
      t15 = x3 * t14
      t17 = 0.1D1 - x3
      t20 = cos(x4 * 0.3141592653589793D1)
      t24 = Sqrt(t15 * t11 * x2 * t17)
      t26 = 0.2D1 * t20 * t24
      t27 = t15 * t11 + x2 * t17 - t26
      t28 = x1 * t12 * t27
      t30 = 0.1D1 - x1
      t33 = s - t9 * t28 - t9 * t30 * x3
      t35 = t2 * t5 * t8 * t33
      t36 = t30 * x2
      t37 = t11 ** 2
      t38 = 0.1D1 / t37
      t42 = t17 * t14 * t11 + x2 * x3 + t26
      t43 = t38 * t42
      t47 = t33 * t2
      t48 = t4 * x2
      t50 = x1 * t30
      t51 = t12 * z
      t52 = t50 * t51
      t56 = t47 * t5 * x2
      t57 = t8 * t30
      t59 = 0.1D1 / t37 / t11
      t65 = t4 * t3
      t66 = t2 * t65
      t71 = t33 ** 2
      t72 = t71 * t1
      t73 = t72 * z
      t75 = t1 ** 2
      t76 = t75 * z
      t79 = t30 * t12
      t86 = t65 * x2
      t87 = t72 * t86
      t88 = t7 * t30
      t92 = t47 * t86
      t106 = t2 * t4 * t7
      t107 = t33 * t12
      t111 = t72 * t4
      t112 = t7 * t12
      t122 = -0.8D1 * t35 * t36 * t43 + 0.36D2 * t47 * t48 * t52 + 0.9D1
     # * t56 * t57 * t59 * t42 * t27 + t66 * t7 * t33 * t36 * t51 + 0.7D
     #1 * t73 - 0.10D2 * t76 * t65 * t7 * x2 * t79 - 0.8D1 * t72 * t48 *
     # t52 + 0.9D1 * t87 * t88 * t43 - 0.9D1 * t92 * t88 * t38 * z * t27
     # - 0.8D1 * t72 * t10 * z + 0.8D1 * t72 * t3 * t30 * x3 + t72 + 0.9
     #D1 * t106 * t107 * t42 - 0.9D1 * t111 * t112 * t42 - 0.9D1 * t111 
     #* t50 * x3 - 0.11D2 * t111 * t112 * t27
      t130 = t27 ** 2
      t134 = t2 * z * t33
      t136 = t12 * t27
      t151 = x2 ** 2
      t153 = t30 ** 2
      t169 = t38 * t27
      t170 = t88 * t169
      t181 = t169 * t36
      t193 = -t106 * t107 * t27 + 0.9D1 * t47 * t3 * t28 + t47 * t4 * t7
     # * t38 * t130 + 0.8D1 * t134 * t4 * t7 * t136 + 0.8D1 * t73 * t10 
     #* t12 * t42 - 0.18D2 * t111 * x2 * x1 * t79 - t2 * t5 * t3 * t8 * 
     #t33 * t151 * t153 * t38 - 0.9D1 * t134 * t10 * t136 + 0.18D2 * t66
     # * t7 * t33 * x2 * t79 + 0.10D2 * t56 * t57 * t59 * t130 + 0.10D2 
     #* t87 * t170 + 0.9D1 * t72 * t10 - 0.9D1 * t2 * t3 * x1 * t33 + 0.
     #8D1 * t76 * t5 * t8 * t181 - 0.9D1 * t87 * x1 * t153 * t12 * x3 - 
     #0.36D2 * t92 * t170 - 0.9D1 * t35 * t181
      gbgbH11J3 = 0.16D2 / 0.3D1 * wd * (t122 + t193) / t71 / s

      end function
  
 