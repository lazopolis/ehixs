  
      subroutine rrqg2qght10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh101J1  
      doubleprecision rrqg2qgh101J2  
      doubleprecision rrqg2qgh101J3  
      doubleprecision rrqg2qgh101J4  
      doubleprecision rrqg2qgh101J5  
      doubleprecision rrqg2qgh101J6  
      doubleprecision rrqg2qgh101J7  
      doubleprecision rrqg2qght10s1e1  
      doubleprecision rrqg2qght10s1e0  
      doubleprecision rrqg2qght10s1em1  
      doubleprecision rrqg2qght10s1em2  
      doubleprecision rrqg2qght10s1em3  
      doubleprecision rrqg2qght10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght10s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = 0.3141592653589793D1 * t1
      t20 = 0.1D1 / s
      t21 = x2 * z
      t23 = (0.1D1 + t21 - x2) ** 2
      t24 = 0.1D1 / t23
      t25 = rrqg2qgh101J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t27 = x1 ** 2
      t28 = Sin(t5)
      t29 = t28 ** 2
      t30 = t27 * t29
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t34 = t1 ** 2
      t35 = t34 ** 2
      t38 = t33 * t35 * t7 * t9
      t41 = log(0.4D1 * t3 * t30 * t38)
      t43 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t49 = 0.3141592653589793D1 * lh
      t51 = t20 * t24
      t52 = t51 * t43
      t56 = 0.1D1 / x1
      t59 = rrqg2qgh101J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t67 = log(0.4D1 * t3 * t29 * t38)
      t68 = t67 * 0.3141592653589793D1
      t77 = t67 ** 2
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t90 = (0.90D2 * t19 * t20 * (t24 * t25 - t41 * t24 * t43) - 0.180D
     #3 * t49 * t1 * t52) * t56 / 0.720D3 + t19 * t51 * t59 / 0.16D2 + (
     #-0.180D3 * t49 - 0.90D2 * t68) * t1 * t51 * t25 / 0.1440D4 + (0.18
     #0D3 * t68 * lh + 0.45D2 * t77 * 0.3141592653589793D1 + 0.314159265
     #3589793D1 * (0.180D3 * t80 - 0.30D2 * t82)) * t1 * t52 / 0.1440D4
      t91 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0,
     # t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, t90)
      t93 = x3 * x1
      t95 = -0.1D1 + x1
      t96 = t93 * z
      t97 = t3 * x1
      t98 = x1 * z
      t99 = t3 * t98
      t101 = 0.1D1 - x1 + t98
      t105 = Sqrt(x3 * t7 * t101 * x2 * t9)
      t107 = 0.2D1 * t6 * t105
      t110 = 0.1D1 / t101
      t116 = x2 * x1
      t117 = t116 * z
      t118 = 0.1D1 - x1 + t98 - x2 + t116 - t117 - x3 + t93 - t96 + t4 -
     # t97 + t99 + t107
      t127 = t101 * t95
      t129 = (-0.1D1 + x1 - t21 + t117 + x2 - t116 - t98) ** 2
      t130 = 0.1D1 / t129
      t131 = -t95
      t132 = rrqg2qgh101J2(s, XB1, XB2, z, lh, wd, nf, t131, x2, x3, x4)
      t138 = t95 ** 2
      t144 = log(0.4D1 * t3 * t30 * t33 * t35 * t110 * t138 * t7 * t9)
      t147 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, t131, x2, x3, x4)
      t160 = 0.90D2 * t19 * t20 * (t127 * t130 * t132 - t144 * t101 * t9
     #5 * t130 * t147) - 0.180D3 * t49 * t1 * t20 * t127 * t130 * t147
      t163 = FJET(XB1, XB2, s, t2 * t93, t2 * t95 * (-x3 + t93 - t96 + t
     #4 - t97 + t99 - x2 + t107) * t110, -t9 * s * t1 * x1, -t2 * t95 * 
     #t118 * t110, -s * t34 * x2 * x1 * t95 * t110, t160 * t56 / 0.720D3
     #)
      rrqg2qght10s1e1 = t91 * t90 + t163 * t160 * t56 / 0.720D3

      end function



      doubleprecision function rrqg2qght10s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = 0.3141592653589793D1 * t1
      t20 = 0.1D1 / s
      t22 = x2 * z
      t24 = (0.1D1 + t22 - x2) ** 2
      t25 = 0.1D1 / t24
      t26 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t28 = 0.1D1 / x1
      t32 = t20 * t25
      t33 = rrqg2qgh101J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t39 = Sin(t5)
      t40 = t39 ** 2
      t42 = z ** 2
      t44 = t1 ** 2
      t45 = t44 ** 2
      t51 = log(0.4D1 * t3 * t40 / t42 * t45 * t9 * t7)
      t59 = t19 * t20 * t25 * t26 * t28 / 0.8D1 + t19 * t32 * t33 / 0.16
     #D2 + (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t51 * 0.3141
     #592653589793D1) * t1 * t32 * t26 / 0.1440D4
      t60 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0,
     # t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, t59)
      t62 = x3 * x1
      t64 = -0.1D1 + x1
      t65 = t62 * z
      t66 = t3 * x1
      t67 = x1 * z
      t68 = t3 * t67
      t70 = 0.1D1 - x1 + t67
      t74 = Sqrt(x3 * t7 * t70 * x2 * t9)
      t76 = 0.2D1 * t6 * t74
      t79 = 0.1D1 / t70
      t85 = x2 * x1
      t86 = t85 * z
      t87 = 0.1D1 - x1 + t67 - x2 + t85 - t86 - x3 + t62 - t65 + t4 - t6
     #6 + t68 + t76
      t99 = (-0.1D1 + x1 - t22 + t86 + x2 - t85 - t67) ** 2
      t100 = 0.1D1 / t99
      t103 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, -t64, x2, x3, x4)
      t108 = FJET(XB1, XB2, s, t2 * t62, t2 * t64 * (-x3 + t62 - t65 + t
     #4 - t66 + t68 - x2 + t76) * t79, -t9 * s * t1 * x1, -t2 * t64 * t8
     #7 * t79, -s * t44 * x2 * x1 * t64 * t79, t19 * t20 * t70 * t64 * t
     #100 * t103 * t28 / 0.8D1)
      rrqg2qght10s1e0 = t60 * t59 + t108 * 0.3141592653589793D1 * t1 * t
     #20 * t70 * t64 * t100 * t103 * t28 / 0.8D1

      end function



      doubleprecision function rrqg2qght10s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * 0.3141592653589793D1)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t23 = (0.1D1 + x2 * z - x2) ** 2
      t26 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t27 = 0.1D1 / s / t23 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0,
     # t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 0.3141592653589793D1 * 
     #t1 * t27 / 0.16D2)
      rrqg2qght10s1em1 = t30 * 0.3141592653589793D1 * t1 * t27 / 0.16D2

      end function



      doubleprecision function rrqg2qght10s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7
      rrqg2qght10s1em2 = 0.0D0

      end function



      doubleprecision function rrqg2qght10s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7
      rrqg2qght10s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght10s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7
      rrqg2qght10s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh101J1
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
      t7 = z + x1 * t2
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
      t56 = t4 * x1
      t57 = 0.1D1 / t7
      t58 = t57 * t51
      t59 = t29 * x3
      t63 = t4 * t29
      t65 = t57 * t24
      t75 = t1 * t3 * t2
      t76 = t75 * t30
      t77 = x1 * t57
      t82 = t75 * t5
      t83 = t9 * t24
      t84 = x2 * t29
      t92 = t29 * t14
      t103 = t1 * t2
      t122 = 0.136D3 * t82 * t83 * t84 + 0.136D3 * t82 * t9 * t51 * t84 
     #- 0.136D3 * t56 * t65 * t92 - 0.136D3 * t63 * t14 * x1 * t58 - 0.9
     #4D2 * t76 * t15 * t77 - 0.68D2 * t103 * t77 * t24 + 0.106D3 * t4 *
     # t30 * x3 * t14 + 0.136D3 * t103 * t59 - 0.68D2 * t103 * t77 * t51
     # + 0.136D3 * t103 * t92 + 0.68D2 * t4 * t5 * t83 * t51
      rrqg2qgh101J1 = 0.2D1 / 0.9D1 * (0.34D2 * t4 * t10 * t25 + 0.47D2 
     #* t4 * t30 * t31 + 0.34D2 * t1 + 0.47D2 * t1 * t36 * t38 * t5 * t3
     #0 * t9 + 0.59D2 * t4 * t30 * t44 + 0.34D2 * t4 * t10 * t52 - 0.136
     #D3 * t56 * t58 * t59 - 0.136D3 * t63 * x3 * x1 * t65 - 0.136D3 * t
     #4 * x2 * x1 * t29 * t57 - 0.106D3 * t76 * t50 * t77 + t122) * wd /
     # s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh101J2
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
      t7 = z + x1 * t2
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
      t56 = t4 * x1
      t57 = 0.1D1 / t7
      t58 = t57 * t51
      t59 = t29 * x3
      t61 = t56 * t58 * t59
      t63 = t4 * t29
      t65 = t57 * t24
      t67 = t63 * x3 * x1 * t65
      t72 = t4 * x2 * x1 * t29 * t57
      t75 = t1 * t3 * t2
      t76 = t75 * t30
      t77 = x1 * t57
      t79 = t76 * t50 * t77
      t82 = t75 * t5
      t83 = t9 * t24
      t84 = x2 * t29
      t86 = t82 * t83 * t84
      t90 = t82 * t9 * t51 * t84
      t92 = t29 * t14
      t94 = t56 * t65 * t92
      t98 = t63 * t14 * x1 * t58
      t101 = t76 * t15 * t77
      t103 = t1 * t2
      t105 = t103 * t77 * t24
      t109 = t4 * t30 * x3 * t14
      t111 = t103 * t59
      t114 = t103 * t77 * t51
      t116 = t103 * t92
      t120 = t4 * t5 * t83 * t51
      t122 = 0.136D3 * t86 + 0.136D3 * t90 - 0.136D3 * t94 - 0.136D3 * t
     #98 - 0.94D2 * t101 - 0.68D2 * t105 + 0.106D3 * t109 + 0.136D3 * t1
     #11 - 0.68D2 * t114 + 0.136D3 * t116 + 0.68D2 * t120
      t147 = -0.216D3 * t86 - 0.216D3 * t90 + 0.216D3 * t94 + 0.216D3 * 
     #t98 + 0.146D3 * t101 - 0.216D3 * t116 - 0.170D3 * t109 - 0.216D3 *
     # t111 + 0.108D3 * t114 - 0.108D3 * t120 + 0.108D3 * t105
      rrqg2qgh101J2 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 - 0.136D3 *
     # t61 - 0.136D3 * t67 - 0.136D3 * t72 - 0.106D3 * t79 + t122) * wd 
     #+ (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.9
     #7D2 * t46 - 0.54D2 * t54 + 0.216D3 * t61 + 0.216D3 * t67 + 0.216D3
     # * t72 + 0.170D3 * t79 + t147) * wd) / s / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrqg2qgh101J3
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
      t7 = z + x1 * t2
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
      t56 = t4 * x1
      t57 = 0.1D1 / t7
      t58 = t57 * t51
      t59 = t29 * x3
      t61 = t56 * t58 * t59
      t63 = t4 * t29
      t65 = t57 * t24
      t67 = t63 * x3 * x1 * t65
      t72 = t4 * x2 * x1 * t29 * t57
      t75 = t1 * t3 * t2
      t76 = t75 * t30
      t77 = x1 * t57
      t79 = t76 * t50 * t77
      t82 = t75 * t5
      t83 = t9 * t24
      t84 = x2 * t29
      t86 = t82 * t83 * t84
      t90 = t82 * t9 * t51 * t84
      t92 = t29 * t14
      t94 = t56 * t65 * t92
      t98 = t63 * t14 * x1 * t58
      t101 = t76 * t15 * t77
      t103 = t1 * t2
      t105 = t103 * t77 * t24
      t109 = t4 * t30 * x3 * t14
      t111 = t103 * t59
      t114 = t103 * t77 * t51
      t116 = t103 * t92
      t120 = t4 * t5 * t83 * t51
      t122 = 0.136D3 * t86 + 0.136D3 * t90 - 0.136D3 * t94 - 0.136D3 * t
     #98 - 0.94D2 * t101 - 0.68D2 * t105 + 0.106D3 * t109 + 0.136D3 * t1
     #11 - 0.68D2 * t114 + 0.136D3 * t116 + 0.68D2 * t120
      t147 = -0.216D3 * t86 - 0.216D3 * t90 + 0.216D3 * t94 + 0.216D3 * 
     #t98 + 0.146D3 * t101 - 0.216D3 * t116 - 0.170D3 * t109 - 0.216D3 *
     # t111 + 0.108D3 * t114 - 0.108D3 * t120 + 0.108D3 * t105
      t172 = 0.20D2 * t54 + 0.26D2 * t42 + 0.20D2 * t27 + 0.20D2 * t1 + 
     #0.80D2 * t111 - 0.80D2 * t98 - 0.80D2 * t72 - 0.40D2 * t105 - 0.80
     #D2 * t61 - 0.52D2 * t101 + 0.80D2 * t116
      rrqg2qgh101J3 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 - 0.136D3 *
     # t61 - 0.136D3 * t67 - 0.136D3 * t72 - 0.106D3 * t79 + t122) * wd 
     #+ (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.9
     #7D2 * t46 - 0.54D2 * t54 + 0.216D3 * t61 + 0.216D3 * t67 + 0.216D3
     # * t72 + 0.170D3 * t79 + t147) * wd + (0.40D2 * t120 - 0.40D2 * t1
     #14 - 0.80D2 * t67 - 0.80D2 * t94 + 0.64D2 * t109 - 0.64D2 * t79 + 
     #0.80D2 * t86 + 0.80D2 * t90 + 0.26D2 * t33 + 0.38D2 * t46 + t172) 
     #* wd) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh101J4
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
      t7 = z + x1 * t2
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
      t56 = t4 * x1
      t57 = 0.1D1 / t7
      t58 = t57 * t51
      t59 = t29 * x3
      t61 = t56 * t58 * t59
      t63 = t4 * t29
      t65 = t57 * t24
      t67 = t63 * x3 * x1 * t65
      t72 = t4 * x2 * x1 * t29 * t57
      t75 = t1 * t3 * t2
      t76 = t75 * t30
      t77 = x1 * t57
      t79 = t76 * t50 * t77
      t82 = t75 * t5
      t83 = t9 * t24
      t84 = x2 * t29
      t86 = t82 * t83 * t84
      t90 = t82 * t9 * t51 * t84
      t92 = t29 * t14
      t94 = t56 * t65 * t92
      t98 = t63 * t14 * x1 * t58
      t101 = t76 * t15 * t77
      t103 = t1 * t2
      t105 = t103 * t77 * t24
      t109 = t4 * t30 * x3 * t14
      t111 = t103 * t59
      t114 = t103 * t77 * t51
      t116 = t103 * t92
      t120 = t4 * t5 * t83 * t51
      t122 = 0.136D3 * t86 + 0.136D3 * t90 - 0.136D3 * t94 - 0.136D3 * t
     #98 - 0.94D2 * t101 - 0.68D2 * t105 + 0.106D3 * t109 + 0.136D3 * t1
     #11 - 0.68D2 * t114 + 0.136D3 * t116 + 0.68D2 * t120
      t147 = -0.216D3 * t86 - 0.216D3 * t90 + 0.216D3 * t94 + 0.216D3 * 
     #t98 + 0.146D3 * t101 - 0.216D3 * t116 - 0.170D3 * t109 - 0.216D3 *
     # t111 + 0.108D3 * t114 - 0.108D3 * t120 + 0.108D3 * t105
      t172 = 0.20D2 * t54 + 0.26D2 * t42 + 0.20D2 * t27 + 0.20D2 * t1 + 
     #0.80D2 * t111 - 0.80D2 * t98 - 0.80D2 * t72 - 0.40D2 * t105 - 0.80
     #D2 * t61 - 0.52D2 * t101 + 0.80D2 * t116
      rrqg2qgh101J4 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 - 0.136D3 *
     # t61 - 0.136D3 * t67 - 0.136D3 * t72 - 0.106D3 * t79 + t122) * wd 
     #+ (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.9
     #7D2 * t46 - 0.54D2 * t54 + 0.216D3 * t61 + 0.216D3 * t67 + 0.216D3
     # * t72 + 0.170D3 * t79 + t147) * wd + (0.40D2 * t120 - 0.40D2 * t1
     #14 - 0.80D2 * t67 - 0.80D2 * t94 + 0.64D2 * t109 - 0.64D2 * t79 + 
     #0.80D2 * t86 + 0.80D2 * t90 + 0.26D2 * t33 + 0.38D2 * t46 + t172) 
     #* wd) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh101J5
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
      t7 = z + x1 * t2
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
      t56 = t4 * x1
      t57 = 0.1D1 / t7
      t58 = t57 * t51
      t59 = t29 * x3
      t61 = t56 * t58 * t59
      t63 = t4 * t29
      t65 = t57 * t24
      t67 = t63 * x3 * x1 * t65
      t72 = t4 * x2 * x1 * t29 * t57
      t75 = t1 * t3 * t2
      t76 = t75 * t30
      t77 = x1 * t57
      t79 = t76 * t50 * t77
      t82 = t75 * t5
      t83 = t9 * t24
      t84 = x2 * t29
      t86 = t82 * t83 * t84
      t90 = t82 * t9 * t51 * t84
      t92 = t29 * t14
      t94 = t56 * t65 * t92
      t98 = t63 * t14 * x1 * t58
      t101 = t76 * t15 * t77
      t103 = t1 * t2
      t105 = t103 * t77 * t24
      t109 = t4 * t30 * x3 * t14
      t111 = t103 * t59
      t114 = t103 * t77 * t51
      t116 = t103 * t92
      t120 = t4 * t5 * t83 * t51
      t122 = 0.136D3 * t86 + 0.136D3 * t90 - 0.136D3 * t94 - 0.136D3 * t
     #98 - 0.94D2 * t101 - 0.68D2 * t105 + 0.106D3 * t109 + 0.136D3 * t1
     #11 - 0.68D2 * t114 + 0.136D3 * t116 + 0.68D2 * t120
      t147 = -0.216D3 * t86 - 0.216D3 * t90 + 0.216D3 * t94 + 0.216D3 * 
     #t98 + 0.146D3 * t101 - 0.216D3 * t116 - 0.170D3 * t109 - 0.216D3 *
     # t111 + 0.108D3 * t114 - 0.108D3 * t120 + 0.108D3 * t105
      t172 = 0.20D2 * t54 + 0.26D2 * t42 + 0.20D2 * t27 + 0.20D2 * t1 + 
     #0.80D2 * t111 - 0.80D2 * t98 - 0.80D2 * t72 - 0.40D2 * t105 - 0.80
     #D2 * t61 - 0.52D2 * t101 + 0.80D2 * t116
      rrqg2qgh101J5 = 0.2D1 / 0.9D1 * ((0.34D2 * t27 + 0.47D2 * t33 + 0.
     #34D2 * t1 + 0.47D2 * t42 + 0.59D2 * t46 + 0.34D2 * t54 - 0.136D3 *
     # t61 - 0.136D3 * t67 - 0.136D3 * t72 - 0.106D3 * t79 + t122) * wd 
     #+ (-0.54D2 * t27 - 0.73D2 * t33 - 0.54D2 * t1 - 0.73D2 * t42 - 0.9
     #7D2 * t46 - 0.54D2 * t54 + 0.216D3 * t61 + 0.216D3 * t67 + 0.216D3
     # * t72 + 0.170D3 * t79 + t147) * wd + (0.40D2 * t120 - 0.40D2 * t1
     #14 - 0.80D2 * t67 - 0.80D2 * t94 + 0.64D2 * t109 - 0.64D2 * t79 + 
     #0.80D2 * t86 + 0.80D2 * t90 + 0.26D2 * t33 + 0.38D2 * t46 + t172) 
     #* wd) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh101J6
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
      t7 = z + x1 * t2
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
      t56 = t4 * x1
      t57 = 0.1D1 / t7
      t58 = t57 * t51
      t59 = t29 * x3
      t61 = t56 * t58 * t59
      t63 = t4 * t29
      t65 = t57 * t24
      t67 = t63 * x3 * x1 * t65
      t72 = t4 * x2 * x1 * t29 * t57
      t75 = t1 * t3 * t2
      t76 = t75 * t30
      t77 = x1 * t57
      t79 = t76 * t50 * t77
      t82 = t75 * t5
      t83 = t9 * t24
      t84 = x2 * t29
      t86 = t82 * t83 * t84
      t90 = t82 * t9 * t51 * t84
      t92 = t29 * t14
      t94 = t56 * t65 * t92
      t98 = t63 * t14 * x1 * t58
      t101 = t76 * t15 * t77
      t103 = t1 * t2
      t104 = t103 * t92
      t108 = t4 * t30 * x3 * t14
      t110 = t103 * t59
      t113 = t103 * t77 * t51
      t117 = t4 * t5 * t83 * t51
      t120 = t103 * t77 * t24
      t122 = -0.216D3 * t86 - 0.216D3 * t90 + 0.216D3 * t94 + 0.216D3 * 
     #t98 + 0.146D3 * t101 - 0.216D3 * t104 - 0.170D3 * t108 - 0.216D3 *
     # t110 + 0.108D3 * t113 - 0.108D3 * t117 + 0.108D3 * t120
      t147 = 0.20D2 * t54 + 0.26D2 * t42 + 0.20D2 * t27 + 0.20D2 * t1 + 
     #0.80D2 * t110 - 0.80D2 * t98 - 0.80D2 * t72 - 0.40D2 * t120 - 0.80
     #D2 * t61 - 0.52D2 * t101 + 0.80D2 * t104
      rrqg2qgh101J6 = 0.2D1 / 0.9D1 * ((-0.54D2 * t27 - 0.73D2 * t33 - 0
     #.54D2 * t1 - 0.73D2 * t42 - 0.97D2 * t46 - 0.54D2 * t54 + 0.216D3 
     #* t61 + 0.216D3 * t67 + 0.216D3 * t72 + 0.170D3 * t79 + t122) * wd
     # + (0.40D2 * t117 - 0.40D2 * t113 - 0.80D2 * t67 - 0.80D2 * t94 + 
     #0.64D2 * t108 - 0.64D2 * t79 + 0.80D2 * t86 + 0.80D2 * t90 + 0.26D
     #2 * t33 + 0.38D2 * t46 + t147) * wd) / s / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrqg2qgh101J7
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
      t8 = z + x1 * t2
      t9 = t8 ** 2
      t10 = 0.1D1 / t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t8 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t8 + t15 - t23
      t25 = t10 * t24
      t28 = x2 * x3
      t29 = t14 * t11 * t8 + t28 + t23
      t33 = t1 * t2
      t34 = 0.1D1 / t8
      t35 = x1 * t34
      t39 = 0.1D1 - x1
      t40 = t4 * t39
      t42 = t34 * t24
      t46 = t4 * x1
      t47 = t39 * t14
      t51 = t39 ** 2
      t57 = t1 * t3 * t2
      t58 = t57 * t51
      t62 = t57 * t5
      t63 = x2 * t39
      t71 = t14 ** 2
      t75 = x3 ** 2
      t80 = t5 * t10
      t81 = t29 ** 2
      t85 = t3 ** 2
      t87 = x2 ** 2
      t93 = t24 ** 2
      t98 = t39 * x3
      t102 = t34 * t29
      t122 = 0.20D2 * t4 * t80 * t81 + 0.26D2 * t1 * t85 * t87 * t5 * t5
     #1 * t10 + 0.20D2 * t4 * t80 * t93 + 0.20D2 * t1 + 0.80D2 * t33 * t
     #98 - 0.80D2 * t40 * t14 * x1 * t102 - 0.80D2 * t4 * x2 * x1 * t39 
     #* t34 - 0.40D2 * t33 * t35 * t24 - 0.80D2 * t46 * t102 * t98 - 0.5
     #2D2 * t58 * t15 * t35 + 0.80D2 * t33 * t47
      rrqg2qgh101J7 = 0.2D1 / 0.9D1 * (0.40D2 * t4 * t5 * t25 * t29 - 0.
     #40D2 * t33 * t35 * t29 - 0.80D2 * t40 * x3 * x1 * t42 - 0.80D2 * t
     #46 * t42 * t47 + 0.64D2 * t4 * t51 * x3 * t14 - 0.64D2 * t58 * t28
     # * t35 + 0.80D2 * t62 * t25 * t63 + 0.80D2 * t62 * t10 * t29 * t63
     # + 0.26D2 * t4 * t51 * t71 + 0.38D2 * t4 * t51 * t75 + t122) * wd 
     #/ s / z / 0.3141592653589793D1

      end function
  
 