  
      subroutine rrgq2qght10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh101J1  
      doubleprecision rrgq2qgh101J2  
      doubleprecision rrgq2qgh101J3  
      doubleprecision rrgq2qgh101J4  
      doubleprecision rrgq2qgh101J5  
      doubleprecision rrgq2qgh101J6  
      doubleprecision rrgq2qgh101J7  
      doubleprecision rrgq2qght10s1e1  
      doubleprecision rrgq2qght10s1e0  
      doubleprecision rrgq2qght10s1em1  
      doubleprecision rrgq2qght10s1em2  
      doubleprecision rrgq2qght10s1em3  
      doubleprecision rrgq2qght10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght10s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = x3 * x1
      t5 = t4 * z
      t6 = x2 * x3
      t7 = 0.2D1 * t6
      t8 = t6 * x1
      t9 = x1 * z
      t10 = t6 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t13 = -0.1D1 + x2
      t14 = x3 * t13
      t15 = 0.1D1 - x1 + t9
      t17 = -0.1D1 + x3
      t20 = Sqrt(t14 * t15 * x2 * t17)
      t22 = 0.2D1 * t12 * t20
      t25 = 0.1D1 / t15
      t29 = x2 * x1
      t30 = t29 * z
      t31 = 0.1D1 - x1 + t9 - x2 + t29 - t30 - x3 + t4 - t5 + t7 - t8 + 
     #t10 + t22
      t38 = t1 ** 2
      t44 = 0.3141592653589793D1 * t1
      t45 = 0.1D1 / s
      t46 = t15 * t3
      t47 = x2 * z
      t49 = (-0.1D1 + x1 - t9 + x2 - t29 - t47 + t30) ** 2
      t50 = 0.1D1 / t49
      t51 = -t3
      t52 = rrgq2qgh101J2(s, XB1, XB2, z, lh, wd, nf, t51, x2, x3, x4)
      t55 = x1 ** 2
      t56 = Sin(t11)
      t57 = t56 ** 2
      t58 = t55 * t57
      t59 = z ** 2
      t60 = 0.1D1 / t59
      t63 = t38 ** 2
      t65 = t3 ** 2
      t71 = log(0.4D1 * t6 * t58 * t60 * t63 * t25 * t65 * t13 * t17)
      t74 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, t51, x2, x3, x4)
      t81 = 0.3141592653589793D1 * lh
      t88 = 0.90D2 * t44 * t45 * (t46 * t50 * t52 - t71 * t15 * t3 * t50
     # * t74) - 0.180D3 * t81 * t1 * t45 * t46 * t50 * t74
      t89 = 0.1D1 / x1
      t92 = FJET(XB1, XB2, s, t2 * t3 * (-x3 + t4 - t5 + t7 - t8 + t10 -
     # x2 + t22) * t25, t2 * t4, -t2 * t3 * t31 * t25, -t17 * s * t1 * x
     #1, -s * t38 * x2 * x1 * t3 * t25, t89 * t88 / 0.720D3)
      t98 = Sqrt(t14 * x2 * t17)
      t100 = 0.2D1 * t12 * t98
      t106 = (0.1D1 - x2 + t47) ** 2
      t107 = 0.1D1 / t106
      t108 = rrgq2qgh101J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x
     #4)
      t113 = t60 * t63 * t13 * t17
      t116 = log(0.4D1 * t6 * t58 * t113)
      t118 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x
     #4)
      t125 = t45 * t107
      t126 = t125 * t118
      t132 = rrgq2qgh101J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x
     #4)
      t140 = log(0.4D1 * t6 * t57 * t113)
      t141 = t140 * 0.3141592653589793D1
      t150 = t140 ** 2
      t153 = lh ** 2
      t155 = 0.3141592653589793D1 ** 2
      t163 = (0.90D2 * t44 * t45 * (t107 * t108 - t116 * t107 * t118) - 
     #0.180D3 * t81 * t1 * t126) * t89 / 0.720D3 + t44 * t125 * t132 / 0
     #.16D2 + (-0.180D3 * t81 - 0.90D2 * t141) * t1 * t125 * t108 / 0.14
     #40D4 + (0.180D3 * t141 * lh + 0.45D2 * t150 * 0.3141592653589793D1
     # + 0.3141592653589793D1 * (0.180D3 * t153 - 0.30D2 * t155)) * t1 *
     # t126 / 0.1440D4
      t164 = FJET(XB1, XB2, s, -t2 * (-x3 + t7 - x2 + t100), 0.0D0, t2 *
     # (0.1D1 - x2 - x3 + t7 + t100), 0.0D0, 0.0D0, t163)
      rrgq2qght10s1e1 = t92 * t88 * t89 / 0.720D3 + t164 * t163

      end function



      doubleprecision function rrgq2qght10s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = x3 * x1
      t5 = t4 * z
      t6 = x2 * x3
      t7 = 0.2D1 * t6
      t8 = t6 * x1
      t9 = x1 * z
      t10 = t6 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t13 = -0.1D1 + x2
      t14 = x3 * t13
      t15 = 0.1D1 - x1 + t9
      t17 = -0.1D1 + x3
      t20 = Sqrt(t14 * t15 * x2 * t17)
      t22 = 0.2D1 * t12 * t20
      t25 = 0.1D1 / t15
      t29 = x2 * x1
      t30 = t29 * z
      t31 = 0.1D1 - x1 + t9 - x2 + t29 - t30 - x3 + t4 - t5 + t7 - t8 + 
     #t10 + t22
      t38 = t1 ** 2
      t44 = 0.3141592653589793D1 * t1
      t45 = 0.1D1 / s
      t48 = x2 * z
      t50 = (-0.1D1 + x1 - t9 + x2 - t29 - t48 + t30) ** 2
      t51 = 0.1D1 / t50
      t54 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, -t3, x2, x3, x4)
      t55 = 0.1D1 / x1
      t60 = FJET(XB1, XB2, s, t2 * t3 * (-x3 + t4 - t5 + t7 - t8 + t10 -
     # x2 + t22) * t25, t2 * t4, -t2 * t3 * t31 * t25, -t17 * s * t1 * x
     #1, -s * t38 * x2 * x1 * t3 * t25, t44 * t45 * t15 * t3 * t51 * t54
     # * t55 / 0.8D1)
      t72 = Sqrt(t14 * x2 * t17)
      t74 = 0.2D1 * t12 * t72
      t81 = (0.1D1 - x2 + t48) ** 2
      t82 = 0.1D1 / t81
      t83 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t88 = t45 * t82
      t89 = rrgq2qgh101J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t95 = Sin(t11)
      t96 = t95 ** 2
      t98 = z ** 2
      t100 = t38 ** 2
      t106 = log(0.4D1 * t6 * t96 / t98 * t100 * t13 * t17)
      t114 = t44 * t45 * t82 * t83 * t55 / 0.8D1 + t44 * t88 * t89 / 0.1
     #6D2 + (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t106 * 0.31
     #41592653589793D1) * t1 * t88 * t83 / 0.1440D4
      t115 = FJET(XB1, XB2, s, -t2 * (-x3 + t7 - x2 + t74), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t7 + t74), 0.0D0, 0.0D0, t114)
      rrgq2qght10s1e0 = t60 * 0.3141592653589793D1 * t1 * t45 * t15 * t3
     # * t51 * t54 * t55 / 0.8D1 + t115 * t114

      end function



      doubleprecision function rrgq2qght10s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * 0.3141592653589793D1)
      t12 = Sqrt(x3 * (-0.1D1 + x2) * x2 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t23 = (0.1D1 - x2 + x2 * z) ** 2
      t26 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t27 = 0.1D1 / s / t23 * t26
      t30 = FJET(XB1, XB2, s, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, t2 * (
     #0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 0.0D0, 0.3141592653589793D1 * 
     #t1 * t27 / 0.16D2)
      rrgq2qght10s1em1 = t30 * 0.3141592653589793D1 * t1 * t27 / 0.16D2

      end function



      doubleprecision function rrgq2qght10s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7
      rrgq2qght10s1em2 = 0.0D0

      end function



      doubleprecision function rrgq2qght10s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7
      rrgq2qght10s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght10s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7
      rrgq2qght10s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh101J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t36 = t3 ** 2
      t38 = x2 ** 2
      t44 = x3 ** 2
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t56 = t1 * t2
      t57 = t29 * t14
      t60 = 0.1D1 / t7
      t61 = x1 * t60
      t65 = t4 * x1
      t66 = t60 * t51
      t67 = t29 * x3
      t71 = t4 * t29
      t73 = t60 * t24
      t84 = t1 * t3 * t2
      t85 = t84 * t30
      t89 = t84 * t5
      t90 = t9 * t24
      t91 = x2 * t29
      t122 = -0.136D3 * t4 * x2 * x1 * t29 * t60 - 0.106D3 * t85 * t50 *
     # t61 + 0.136D3 * t89 * t90 * t91 + 0.136D3 * t89 * t9 * t51 * t91 
     #- 0.136D3 * t65 * t73 * t57 - 0.136D3 * t71 * t14 * x1 * t66 - 0.9
     #4D2 * t85 * t15 * t61 + 0.106D3 * t4 * t30 * x3 * t14 + 0.136D3 * 
     #t56 * t67 - 0.68D2 * t56 * t61 * t51 + 0.68D2 * t4 * t5 * t90 * t5
     #1
      rrgq2qgh101J1 = 0.2D1 / 0.9D1 * (0.34D2 * t4 * t10 * t25 + 0.47D2 
     #* t4 * t30 * t31 + 0.34D2 * t1 + 0.47D2 * t1 * t36 * t38 * t5 * t3
     #0 * t9 + 0.59D2 * t4 * t30 * t44 + 0.34D2 * t4 * t10 * t52 + 0.136
     #D3 * t56 * t57 - 0.68D2 * t56 * t61 * t24 - 0.136D3 * t65 * t66 * 
     #t67 - 0.136D3 * t71 * x3 * x1 * t73 + t122) * wd / s / z / 0.31415
     #92653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh101J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t60 = 0.1D1 / t7
      t61 = x1 * t60
      t63 = t56 * t61 * t24
      t65 = t4 * x1
      t66 = t60 * t51
      t67 = t29 * x3
      t69 = t65 * t66 * t67
      t71 = t4 * t29
      t73 = t60 * t24
      t75 = t71 * x3 * x1 * t73
      t81 = t4 * x2 * x1 * t29 * t60
      t84 = t1 * t3 * t2
      t85 = t84 * t30
      t87 = t85 * t50 * t61
      t89 = t84 * t5
      t90 = t9 * t24
      t91 = x2 * t29
      t93 = t89 * t90 * t91
      t97 = t89 * t9 * t51 * t91
      t100 = t65 * t73 * t57
      t104 = t71 * t14 * x1 * t66
      t107 = t85 * t15 * t61
      t111 = t4 * t30 * x3 * t14
      t113 = t56 * t67
      t116 = t56 * t61 * t51
      t120 = t4 * t5 * t90 * t51
      t122 = -0.136D3 * t81 - 0.106D3 * t87 + 0.136D3 * t93 + 0.136D3 * 
     #t97 - 0.136D3 * t100 - 0.136D3 * t104 - 0.94D2 * t107 + 0.106D3 * 
     #t111 + 0.136D3 * t113 - 0.68D2 * t116 + 0.68D2 * t120
      t147 = 0.216D3 * t81 + 0.170D3 * t87 - 0.216D3 * t93 - 0.216D3 * t
     #97 + 0.216D3 * t100 + 0.216D3 * t104 + 0.146D3 * t107 - 0.170D3 * 
     #t111 - 0.216D3 * t113 + 0.108D3 * t116 - 0.108D3 * t120
      rrgq2qgh101J2 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 + 0.136D3 *
     # t58 - 0.68D2 * t63 - 0.136D3 * t69 - 0.136D3 * t75 + t122) * wd +
     # (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.97
     #D2 * t46 - 0.54D2 * t54 - 0.216D3 * t58 + 0.108D3 * t63 + 0.216D3 
     #* t69 + 0.216D3 * t75 + t147) * wd) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh101J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t60 = 0.1D1 / t7
      t61 = x1 * t60
      t63 = t56 * t61 * t24
      t65 = t4 * x1
      t66 = t60 * t51
      t67 = t29 * x3
      t69 = t65 * t66 * t67
      t71 = t4 * t29
      t73 = t60 * t24
      t75 = t71 * x3 * x1 * t73
      t81 = t4 * x2 * x1 * t29 * t60
      t84 = t1 * t3 * t2
      t85 = t84 * t30
      t87 = t85 * t50 * t61
      t89 = t84 * t5
      t90 = t9 * t24
      t91 = x2 * t29
      t93 = t89 * t90 * t91
      t97 = t89 * t9 * t51 * t91
      t100 = t65 * t73 * t57
      t104 = t71 * t14 * x1 * t66
      t107 = t85 * t15 * t61
      t111 = t4 * t30 * x3 * t14
      t113 = t56 * t67
      t116 = t56 * t61 * t51
      t120 = t4 * t5 * t90 * t51
      t122 = -0.136D3 * t81 - 0.106D3 * t87 + 0.136D3 * t93 + 0.136D3 * 
     #t97 - 0.136D3 * t100 - 0.136D3 * t104 - 0.94D2 * t107 + 0.106D3 * 
     #t111 + 0.136D3 * t113 - 0.68D2 * t116 + 0.68D2 * t120
      t147 = 0.216D3 * t81 + 0.170D3 * t87 - 0.216D3 * t93 - 0.216D3 * t
     #97 + 0.216D3 * t100 + 0.216D3 * t104 + 0.146D3 * t107 - 0.170D3 * 
     #t111 - 0.216D3 * t113 + 0.108D3 * t116 - 0.108D3 * t120
      t172 = -0.40D2 * t116 + 0.20D2 * t54 + 0.20D2 * t27 + 0.26D2 * t33
     # + 0.26D2 * t42 + 0.38D2 * t46 + 0.20D2 * t1 - 0.80D2 * t100 + 0.6
     #4D2 * t111 - 0.80D2 * t81 + 0.80D2 * t97
      rrgq2qgh101J3 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 + 0.136D3 *
     # t58 - 0.68D2 * t63 - 0.136D3 * t69 - 0.136D3 * t75 + t122) * wd +
     # (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.97
     #D2 * t46 - 0.54D2 * t54 - 0.216D3 * t58 + 0.108D3 * t63 + 0.216D3 
     #* t69 + 0.216D3 * t75 + t147) * wd + (-0.52D2 * t107 + 0.40D2 * t1
     #20 - 0.80D2 * t69 + 0.80D2 * t113 - 0.40D2 * t63 + 0.80D2 * t93 - 
     #0.64D2 * t87 + 0.80D2 * t58 - 0.80D2 * t104 - 0.80D2 * t75 + t172)
     # * wd) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh101J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t60 = 0.1D1 / t7
      t61 = x1 * t60
      t63 = t56 * t61 * t24
      t65 = t4 * x1
      t66 = t60 * t51
      t67 = t29 * x3
      t69 = t65 * t66 * t67
      t71 = t4 * t29
      t73 = t60 * t24
      t75 = t71 * x3 * x1 * t73
      t81 = t4 * x2 * x1 * t29 * t60
      t84 = t1 * t3 * t2
      t85 = t84 * t30
      t87 = t85 * t50 * t61
      t89 = t84 * t5
      t90 = t9 * t24
      t91 = x2 * t29
      t93 = t89 * t90 * t91
      t97 = t89 * t9 * t51 * t91
      t100 = t65 * t73 * t57
      t104 = t71 * t14 * x1 * t66
      t107 = t85 * t15 * t61
      t111 = t4 * t30 * x3 * t14
      t113 = t56 * t67
      t116 = t56 * t61 * t51
      t120 = t4 * t5 * t90 * t51
      t122 = -0.136D3 * t81 - 0.106D3 * t87 + 0.136D3 * t93 + 0.136D3 * 
     #t97 - 0.136D3 * t100 - 0.136D3 * t104 - 0.94D2 * t107 + 0.106D3 * 
     #t111 + 0.136D3 * t113 - 0.68D2 * t116 + 0.68D2 * t120
      t147 = 0.216D3 * t81 + 0.170D3 * t87 - 0.216D3 * t93 - 0.216D3 * t
     #97 + 0.216D3 * t100 + 0.216D3 * t104 + 0.146D3 * t107 - 0.170D3 * 
     #t111 - 0.216D3 * t113 + 0.108D3 * t116 - 0.108D3 * t120
      t172 = -0.40D2 * t116 + 0.20D2 * t54 + 0.20D2 * t27 + 0.26D2 * t33
     # + 0.26D2 * t42 + 0.38D2 * t46 + 0.20D2 * t1 - 0.80D2 * t100 + 0.6
     #4D2 * t111 - 0.80D2 * t81 + 0.80D2 * t97
      rrgq2qgh101J4 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 + 0.136D3 *
     # t58 - 0.68D2 * t63 - 0.136D3 * t69 - 0.136D3 * t75 + t122) * wd +
     # (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.97
     #D2 * t46 - 0.54D2 * t54 - 0.216D3 * t58 + 0.108D3 * t63 + 0.216D3 
     #* t69 + 0.216D3 * t75 + t147) * wd + (-0.52D2 * t107 + 0.40D2 * t1
     #20 - 0.80D2 * t69 + 0.80D2 * t113 - 0.40D2 * t63 + 0.80D2 * t93 - 
     #0.64D2 * t87 + 0.80D2 * t58 - 0.80D2 * t104 - 0.80D2 * t75 + t172)
     # * wd) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh101J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t60 = 0.1D1 / t7
      t61 = x1 * t60
      t63 = t56 * t61 * t24
      t65 = t4 * x1
      t66 = t60 * t51
      t67 = t29 * x3
      t69 = t65 * t66 * t67
      t71 = t4 * t29
      t73 = t60 * t24
      t75 = t71 * x3 * x1 * t73
      t81 = t4 * x2 * x1 * t29 * t60
      t84 = t1 * t3 * t2
      t85 = t84 * t30
      t87 = t85 * t50 * t61
      t89 = t84 * t5
      t90 = t9 * t24
      t91 = x2 * t29
      t93 = t89 * t90 * t91
      t97 = t89 * t9 * t51 * t91
      t100 = t65 * t73 * t57
      t104 = t71 * t14 * x1 * t66
      t107 = t85 * t15 * t61
      t111 = t4 * t30 * x3 * t14
      t113 = t56 * t67
      t116 = t56 * t61 * t51
      t120 = t4 * t5 * t90 * t51
      t122 = -0.136D3 * t81 - 0.106D3 * t87 + 0.136D3 * t93 + 0.136D3 * 
     #t97 - 0.136D3 * t100 - 0.136D3 * t104 - 0.94D2 * t107 + 0.106D3 * 
     #t111 + 0.136D3 * t113 - 0.68D2 * t116 + 0.68D2 * t120
      t147 = 0.216D3 * t81 + 0.170D3 * t87 - 0.216D3 * t93 - 0.216D3 * t
     #97 + 0.216D3 * t100 + 0.216D3 * t104 + 0.146D3 * t107 - 0.170D3 * 
     #t111 - 0.216D3 * t113 + 0.108D3 * t116 - 0.108D3 * t120
      t172 = -0.40D2 * t116 + 0.20D2 * t54 + 0.20D2 * t27 + 0.26D2 * t33
     # + 0.26D2 * t42 + 0.38D2 * t46 + 0.20D2 * t1 - 0.80D2 * t100 + 0.6
     #4D2 * t111 - 0.80D2 * t81 + 0.80D2 * t97
      rrgq2qgh101J5 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 + 0.136D3 *
     # t58 - 0.68D2 * t63 - 0.136D3 * t69 - 0.136D3 * t75 + t122) * wd +
     # (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.97
     #D2 * t46 - 0.54D2 * t54 - 0.216D3 * t58 + 0.108D3 * t63 + 0.216D3 
     #* t69 + 0.216D3 * t75 + t147) * wd + (-0.52D2 * t107 + 0.40D2 * t1
     #20 - 0.80D2 * t69 + 0.80D2 * t113 - 0.40D2 * t63 + 0.80D2 * t93 - 
     #0.64D2 * t87 + 0.80D2 * t58 - 0.80D2 * t104 - 0.80D2 * t75 + t172)
     # * wd) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh101J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t60 = 0.1D1 / t7
      t61 = x1 * t60
      t63 = t56 * t61 * t24
      t65 = t4 * x1
      t66 = t60 * t51
      t67 = t29 * x3
      t69 = t65 * t66 * t67
      t71 = t4 * t29
      t73 = t60 * t24
      t75 = t71 * x3 * x1 * t73
      t81 = t4 * x2 * x1 * t29 * t60
      t84 = t1 * t3 * t2
      t85 = t84 * t30
      t87 = t85 * t50 * t61
      t89 = t84 * t5
      t90 = t9 * t24
      t91 = x2 * t29
      t93 = t89 * t90 * t91
      t97 = t89 * t9 * t51 * t91
      t100 = t65 * t73 * t57
      t104 = t71 * t14 * x1 * t66
      t107 = t85 * t15 * t61
      t111 = t4 * t30 * x3 * t14
      t113 = t56 * t67
      t116 = t56 * t61 * t51
      t120 = t4 * t5 * t90 * t51
      t122 = 0.216D3 * t81 + 0.170D3 * t87 - 0.216D3 * t93 - 0.216D3 * t
     #97 + 0.216D3 * t100 + 0.216D3 * t104 + 0.146D3 * t107 - 0.170D3 * 
     #t111 - 0.216D3 * t113 + 0.108D3 * t116 - 0.108D3 * t120
      t147 = -0.40D2 * t116 + 0.20D2 * t54 + 0.20D2 * t27 + 0.26D2 * t33
     # + 0.26D2 * t42 + 0.38D2 * t46 + 0.20D2 * t1 - 0.80D2 * t100 + 0.6
     #4D2 * t111 - 0.80D2 * t81 + 0.80D2 * t97
      rrgq2qgh101J6 = 0.2D1 / 0.9D1 * ((-0.54D2 * t27 - 0.73D2 * t33 - 0
     #.54D2 * t1 - 0.73D2 * t42 - 0.97D2 * t46 - 0.54D2 * t54 - 0.216D3 
     #* t58 + 0.108D3 * t63 + 0.216D3 * t69 + 0.216D3 * t75 + t122) * wd
     # + (-0.52D2 * t107 + 0.40D2 * t120 - 0.80D2 * t69 + 0.80D2 * t113 
     #- 0.40D2 * t63 + 0.80D2 * t93 - 0.64D2 * t87 + 0.80D2 * t58 - 0.80
     #D2 * t104 - 0.80D2 * t75 + t147) * wd) / s / z / 0.314159265358979
     #3D1

      end function
  
   
 

      doubleprecision function rrgq2qgh101J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t5 = t1 * t3 * t2
      t6 = 0.1D1 - x1
      t7 = t6 ** 2
      t8 = t5 * t7
      t9 = 0.1D1 - x3
      t10 = x2 * t9
      t12 = z + t2 * x1
      t13 = 0.1D1 / t12
      t14 = x1 * t13
      t18 = t1 * t3
      t19 = x1 ** 2
      t21 = t12 ** 2
      t22 = 0.1D1 / t21
      t23 = 0.1D1 - x2
      t24 = x3 * t23
      t27 = cos(x4 * 0.3141592653589793D1)
      t31 = Sqrt(t24 * t12 * x2 * t9)
      t33 = 0.2D1 * t27 * t31
      t34 = t24 * t12 + t10 - t33
      t35 = t22 * t34
      t38 = x2 * x3
      t39 = t9 * t23 * t12 + t38 + t33
      t43 = t18 * x1
      t44 = t13 * t39
      t45 = t6 * x3
      t49 = t1 * t2
      t55 = t5 * t19
      t56 = x2 * t6
      t63 = t6 * t9
      t66 = t18 * t6
      t72 = t13 * t34
      t80 = t19 * t22
      t81 = t39 ** 2
      t85 = t34 ** 2
      t89 = t9 ** 2
      t93 = t3 ** 2
      t95 = x2 ** 2
      t101 = x3 ** 2
      t122 = -0.40D2 * t49 * t14 * t39 + 0.20D2 * t18 * t80 * t81 + 0.20
     #D2 * t18 * t80 * t85 + 0.26D2 * t18 * t7 * t89 + 0.26D2 * t1 * t93
     # * t95 * t19 * t7 * t22 + 0.38D2 * t18 * t7 * t101 + 0.20D2 * t1 -
     # 0.80D2 * t43 * t72 * t63 + 0.64D2 * t18 * t7 * x3 * t9 - 0.80D2 *
     # t18 * x2 * x1 * t6 * t13 + 0.80D2 * t55 * t22 * t39 * t56
      rrgq2qgh101J7 = 0.2D1 / 0.9D1 * (-0.52D2 * t8 * t10 * t14 + 0.40D2
     # * t18 * t19 * t35 * t39 - 0.80D2 * t43 * t44 * t45 + 0.80D2 * t49
     # * t45 - 0.40D2 * t49 * t14 * t34 + 0.80D2 * t55 * t35 * t56 - 0.6
     #4D2 * t8 * t38 * t14 + 0.80D2 * t49 * t63 - 0.80D2 * t66 * t9 * x1
     # * t44 - 0.80D2 * t66 * x3 * x1 * t72 + t122) * wd / s / z / 0.314
     #1592653589793D1

      end function
  
 