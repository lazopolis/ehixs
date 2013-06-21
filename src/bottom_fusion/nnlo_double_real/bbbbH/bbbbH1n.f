  
      subroutine bbbbH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH11J1  
      doubleprecision bbbbH11J2  
      doubleprecision bbbbH11J3  
      doubleprecision bbbbH1n1e1  
      doubleprecision bbbbH1n1e0  
      doubleprecision bbbbH1n1em1  
      doubleprecision bbbbH1n1em2  
      doubleprecision bbbbH1n1em3  
      doubleprecision bbbbH1n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH11J1
      doubleprecision bbbbH11J2
      doubleprecision bbbbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * z
      t4 = x3 * x1
      t5 = t4 * z
      t6 = x2 * x3
      t7 = t6 * z
      t8 = t4 * x2
      t9 = x2 * z
      t10 = t4 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t14 = x3 * (-0.1D1 + x2)
      t15 = x1 * z
      t16 = -z - x1 + t15
      t18 = -0.1D1 + x3
      t21 = Sqrt(-t14 * t16 * x2 * t18)
      t23 = 0.2D1 * t12 * t21
      t26 = 0.1D1 / t16
      t29 = -0.1D1 + x1
      t32 = x1 * x2
      t33 = t32 * z
      t34 = z + x1 - t15 - t9 - t32 + t33 - t3 - t4 + t5 + t7 + t8 - t10
     # + t6 + t23
      t40 = t1 ** 2
      t46 = 0.1D1 / s
      t49 = 0.1D1 / (z + x1 - t15 - t32 + t33)
      t50 = bbbbH11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t55 = Sin(t11)
      t56 = t55 ** 2
      t58 = z ** 2
      t60 = x1 ** 2
      t63 = t29 ** 2
      t69 = log(-0.4D1 * t26 * t56 / t58 * t60 * t63 * x2 * t14 * t18)
      t74 = bbbbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t78 = t40 * t46 * t49 * t50 / 0.32D2 + (-0.180D3 * lh - 0.90D2 * t
     #69) * t40 * t46 * t49 * t74 / 0.2880D4
      t79 = FJET(XB1, XB2, s, t2 * x1 * (-t3 - t4 + t5 + t7 + t8 - t10 -
     # x2 + t6 + t23) * t26, -t2 * t29 * x3, -t2 * x1 * t34 * t26, t2 * 
     #t29 * t18, s * t40 * x2 * x1 * t29 * t26, t78)
      bbbbH1n1e1 = t79 * t78

      end function



      doubleprecision function bbbbH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH11J1
      doubleprecision bbbbH11J2
      doubleprecision bbbbH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * z
      t4 = x3 * x1
      t5 = t4 * z
      t6 = x2 * x3
      t7 = t6 * z
      t8 = t4 * x2
      t9 = x2 * z
      t10 = t4 * t9
      t12 = cos(x4 * 0.3141592653589793D1)
      t15 = x1 * z
      t16 = -z - x1 + t15
      t18 = -0.1D1 + x3
      t21 = Sqrt(-x3 * (-0.1D1 + x2) * t16 * x2 * t18)
      t23 = 0.2D1 * t12 * t21
      t26 = 0.1D1 / t16
      t29 = -0.1D1 + x1
      t32 = x1 * x2
      t33 = t32 * z
      t34 = z + x1 - t15 - t9 - t32 + t33 - t3 - t4 + t5 + t7 + t8 - t10
     # + t6 + t23
      t40 = t1 ** 2
      t46 = 0.1D1 / s
      t49 = 0.1D1 / (z + x1 - t15 - t32 + t33)
      t50 = bbbbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t54 = FJET(XB1, XB2, s, t2 * x1 * (-t3 - t4 + t5 + t7 + t8 - t10 -
     # x2 + t6 + t23) * t26, -t2 * t29 * x3, -t2 * x1 * t34 * t26, t2 * 
     #t29 * t18, s * t40 * x2 * x1 * t29 * t26, t40 * t46 * t49 * t50 / 
     #0.32D2)
      bbbbH1n1e0 = t54 * t40 * t46 * t49 * t50 / 0.32D2

      end function



      doubleprecision function bbbbH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH11J1
      doubleprecision bbbbH11J2
      doubleprecision bbbbH11J3
      bbbbH1n1em1 = 0.0D0

      end function



      doubleprecision function bbbbH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH11J1
      doubleprecision bbbbH11J2
      doubleprecision bbbbH11J3
      bbbbH1n1em2 = 0.0D0

      end function



      doubleprecision function bbbbH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH11J1
      doubleprecision bbbbH11J2
      doubleprecision bbbbH11J3
      bbbbH1n1em3 = 0.0D0

      end function



      doubleprecision function bbbbH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbbbH11J1
      doubleprecision bbbbH11J2
      doubleprecision bbbbH11J3
      bbbbH1n1em4 = 0.0D0

      end function
  
 

      doubleprecision function bbbbH11J1
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
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t30 = t10 * t7 * t4 + x2 * x3 + t19
      t33 = t23 * t10
      t35 = s - t2 * t6 * t30 - t2 * t33
      t36 = t26 * t35
      t37 = s ** 2
      t38 = t37 * s
      t39 = t1 ** 2
      t40 = t39 * t1
      t42 = t36 * t38 * t40
      t45 = z + x1 * t7 * t1
      t46 = t23 * t45
      t47 = t4 ** 2
      t48 = 0.1D1 / t47
      t49 = x1 ** 2
      t50 = t48 * t49
      t55 = t35 ** 2
      t56 = t26 * t55
      t57 = t37 * t40
      t58 = t56 * t57
      t59 = t23 ** 2
      t60 = t59 * t45
      t63 = t60 * t48 * x2 * x1
      t66 = t26 ** 2
      t67 = t66 * t35
      t68 = t37 * t39
      t69 = t67 * t68
      t70 = t46 * t6
      t77 = t37 * t1
      t80 = t46 * t5 * z
      t84 = t39 * t23
      t86 = t45 * t5
      t88 = t86 * x1 * z
      t96 = t60 * t5 * t10
      t99 = t67 * t57
      t102 = t49 * x2 * t23 * t5
      t105 = t48 * x1
      t107 = t46 * t105 * t20
      t112 = t60 * t5 * x3
      t123 = t56 * t68
      t126 = t67 * t37
      t127 = t3 * z
      t130 = t56 * t37
      t133 = t66 * t55
      t134 = t133 * s
      t135 = t1 * t23
      t139 = t39 ** 2
      t153 = -0.4D1 * t69 * t112 + 0.4D1 * t42 * t60 * t6 * x3 + 0.8D1 *
     # t58 * t102 + 0.8D1 * t99 * t63 - 0.4D1 * t123 * t70 + 0.6D1 * t12
     #6 * t127 + 0.6D1 * t130 * t127 + 0.2D1 * t134 * t135 * t10 - 0.4D1
     # * t36 * t38 * t139 * t60 * t50 * x2 + 0.4D1 * t134 * t135 * x3 + 
     #0.6D1 * t66 * t38 * t84 * t88
      t159 = t39 * x1
      t163 = t159 * t33
      t166 = t39 * t49
      t167 = t5 * t20
      t168 = t166 * t167
      t173 = t5 * t30
      t174 = t166 * t173
      t177 = t135 * t86
      t198 = t77 * x1
      t211 = -0.4D1 * t130 * t174 + t130 * t177 - 0.8D1 * t134 * t177 + 
     #0.6D1 * t67 * t77 * t80 - 0.4D1 * t134 + 0.2D1 * t56 * t198 + t67 
     #* t198 - 0.8D1 * t133 * t2 * x1 + 0.2D1 * t123 * t107 - 0.12D2 * t
     #123 * t112 - 0.4D1 * t123 * t96
      bbbbH11J1 = -0.16D2 / 0.3D1 * wd * (0.4D1 * t42 * t46 * t50 * t30 
     #+ 0.7D1 * t58 * t63 - 0.4D1 * t69 * t70 + 0.2D1 * t42 * t46 * t50 
     #* t20 + 0.6D1 * t56 * t77 * t80 + 0.6D1 * t55 * t38 * t84 * t88 + 
     #0.2D1 * t42 * t60 * t6 * t10 - 0.13D2 * t69 * t96 + 0.7D1 * t99 * 
     #t102 + 0.5D1 * t69 * t107 + t153 + 0.6D1 * t123 * t46 * t105 * t30
     # + 0.6D1 * t126 * t159 * t24 + 0.2D1 * t126 * t163 - 0.4D1 * t126 
     #* t168 - 0.13D2 * t130 * t168 - 0.12D2 * t126 * t174 + 0.2D1 * t12
     #6 * t177 + 0.5D1 * t130 * t163 + 0.4D1 * t134 * t3 * t173 + 0.2D1 
     #* t134 * t3 * t167 + t211) / t66 / t55

      end function
  
   
 

      doubleprecision function bbbbH11J2
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
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t26 = s - t2 * t6 * t20 - t2 * t23 * x3
      t30 = t10 * t7 * t4 + x2 * x3 + t19
      t33 = t23 * t10
      t35 = s - t2 * t6 * t30 - t2 * t33
      t36 = t35 ** 2
      t37 = t26 * t36
      t38 = s ** 2
      t39 = t1 ** 2
      t40 = t38 * t39
      t41 = t37 * t40
      t44 = z + x1 * t7 * t1
      t45 = t23 * t44
      t46 = t45 * t6
      t49 = t4 ** 2
      t50 = 0.1D1 / t49
      t53 = t45 * t50 * x1 * t20
      t56 = t26 ** 2
      t57 = t56 * t35
      t58 = t57 * t40
      t59 = t23 ** 2
      t60 = t59 * t44
      t66 = t60 * t5 * t10
      t69 = t39 * t1
      t70 = t38 * t69
      t71 = t37 * t70
      t74 = t60 * t50 * x2 * x1
      t76 = t57 * t70
      t77 = x1 ** 2
      t78 = t77 * x2
      t80 = t78 * t23 * t5
      t82 = t38 * s
      t83 = t56 * t82
      t84 = t69 * t23
      t86 = t44 * t50
      t91 = t69 * t59
      t93 = t44 * t5
      t98 = t39 ** 2
      t99 = t98 * t59
      t101 = t86 * t78
      t104 = t36 * t82
      t110 = 0.2D1 * t41 * t46 - 0.2D1 * t41 * t53 + 0.4D1 * t58 * t60 *
     # t5 * x3 + t58 * t66 + t58 * t53 - t71 * t74 - t76 * t80 + 0.2D1 *
     # t83 * t84 * t86 * t77 * t20 + 0.2D1 * t83 * t91 * t93 * x1 * x3 -
     # 0.2D1 * t83 * t99 * t101 + 0.2D1 * t104 * t91 * t93 * x1 * t10
      t111 = t26 * t35
      t113 = t111 * t82 * t69
      t114 = t50 * t77
      t119 = t38 * t1
      t122 = t45 * t5 * z
      t125 = t39 * t23
      t128 = t93 * x1 * z
      t135 = t56 * t36
      t136 = t135 * s
      t154 = -0.2D1 * t113 * t45 * t114 * t20 + 0.6D1 * t37 * t119 * t12
     #2 - 0.6D1 * t83 * t125 * t128 - 0.4D1 * t113 * t45 * t114 * t30 + 
     #0.4D1 * t136 + 0.2D1 * t58 * t46 + 0.4D1 * t41 * t66 - 0.2D1 * t76
     # * t74 - 0.6D1 * t104 * t125 * t128 - 0.2D1 * t113 * t60 * t6 * t1
     #0 + 0.6D1 * t57 * t119 * t122
      t156 = t119 * x1
      t183 = t57 * t38
      t185 = t39 * x1 * t33
      t188 = t39 * t77
      t189 = t5 * t20
      t190 = t188 * t189
      t193 = t37 * t38
      t195 = -0.2D1 * t37 * t156 - t57 * t156 - 0.2D1 * t135 * t2 * x1 +
     # 0.2D1 * t104 * t84 * t86 * t77 * t30 - 0.2D1 * t104 * t99 * t101 
     #- 0.4D1 * t113 * t60 * t6 * x3 - 0.2D1 * t71 * t80 + 0.4D1 * t111 
     #* t82 * t98 * t60 * t114 * x2 - 0.2D1 * t183 * t185 + 0.4D1 * t183
     # * t190 + t193 * t190
      t196 = t1 * t23
      t197 = t196 * t93
      t201 = t5 * t30
      t217 = t3 * z
      t225 = -0.2D1 * t183 * t197 + t193 * t185 - 0.4D1 * t136 * t3 * t2
     #01 + 0.4D1 * t193 * t188 * t201 - 0.2D1 * t136 * t3 * t189 - t193 
     #* t197 - 0.2D1 * t136 * t197 - 0.4D1 * t136 * t196 * x3 + 0.6D1 * 
     #t193 * t217 - 0.2D1 * t136 * t196 * t10 + 0.6D1 * t183 * t217
      bbbbH11J2 = -0.16D2 / 0.3D1 * wd * (t110 + t154 + t195 + t225) / t
     #56 / t36

      end function
  
   
 

      doubleprecision function bbbbH11J3
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
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t26 = s - t2 * t6 * t20 - t2 * t23 * x3
      t30 = t10 * t7 * t4 + x2 * x3 + t19
      t33 = t23 * t10
      t35 = s - t2 * t6 * t30 - t2 * t33
      t36 = t35 ** 2
      t37 = t26 * t36
      t38 = s ** 2
      t39 = t37 * t38
      t40 = t1 ** 2
      t41 = x1 ** 2
      t42 = t40 * t41
      t44 = t42 * t5 * t20
      t47 = t26 ** 2
      t48 = t47 * t36
      t52 = t47 * t35
      t53 = t52 * t38
      t60 = z + x1 * t7 * t1
      t61 = t60 * t5
      t62 = t1 * t23 * t61
      t67 = t38 * t40
      t68 = t67 * t37
      t69 = t23 ** 2
      t70 = t69 * t60
      t72 = t70 * t5 * t10
      t79 = t52 * t67
      t80 = t23 * t60
      t81 = t80 * t6
      t85 = t40 * x1 * t33
      t88 = t38 * s
      t89 = t47 * t88
      t90 = t40 * t1
      t91 = t90 * t69
      t97 = t36 * t88
      t103 = t4 ** 2
      t104 = 0.1D1 / t103
      t107 = t80 * t104 * x1 * t20
      t110 = 0.2D1 * t39 * t44 - 0.4D1 * t48 * t2 * x1 + 0.4D1 * t53 * t
     #44 - 0.4D1 * t48 * s * t62 - 0.2D1 * t53 * t62 + 0.4D1 * t68 * t72
     # + 0.4D1 * t39 * t42 * t5 * t30 + 0.4D1 * t79 * t81 - 0.4D1 * t53 
     #* t85 - 0.2D1 * t89 * t91 * t61 * x1 * x3 - 0.2D1 * t97 * t91 * t6
     #1 * x1 * t10 + 0.2D1 * t79 * t107
      t111 = t38 * t90
      t119 = t41 * x2
      t126 = t90 * t23
      t128 = t60 * t104
      t133 = t40 ** 2
      t134 = t133 * t69
      t136 = t128 * t119
      t161 = -0.2D1 * t52 * t111 * t70 * t104 * x2 * x1 - 0.2D1 * t37 * 
     #t111 * t119 * t23 * t5 + 0.4D1 * t68 * t81 - 0.2D1 * t97 * t126 * 
     #t128 * t41 * t30 + 0.2D1 * t97 * t134 * t136 - 0.4D1 * t68 * t107 
     #- 0.2D1 * t37 * t38 * t1 * x1 + 0.2D1 * t89 * t134 * t136 + 0.4D1 
     #* t79 * t70 * t5 * x3 - 0.2D1 * t89 * t126 * t128 * t41 * t20 + 0.
     #2D1 * t39 * t85 + 0.2D1 * t79 * t72
      bbbbH11J3 = -0.16D2 / 0.3D1 * wd * (t110 + t161) / t47 / t36

      end function
  
 