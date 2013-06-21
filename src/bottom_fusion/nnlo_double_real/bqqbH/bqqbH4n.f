  
      subroutine bqqbH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bqqbH41J1  
      doubleprecision bqqbH41J2  
      doubleprecision bqqbH4n1e1  
      doubleprecision bqqbH4n1e0  
      doubleprecision bqqbH4n1em1  
      doubleprecision bqqbH4n1em2  
      doubleprecision bqqbH4n1em3  
      doubleprecision bqqbH4n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bqqbH4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bqqbH4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bqqbH4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bqqbH4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bqqbH4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bqqbH4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bqqbH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH41J1
      doubleprecision bqqbH41J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = x3 * x1
      t5 = t4 * z
      t7 = 0.2D1 * x2 * x3
      t8 = t4 * x2
      t9 = x2 * z
      t10 = t4 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t13 = -0.1D1 + x2
      t15 = x1 * z
      t16 = 0.1D1 - x1 + t15
      t18 = -0.1D1 + x3
      t21 = Sqrt(x3 * t13 * t16 * x2 * t18)
      t23 = 0.2D1 * t12 * t21
      t26 = 0.1D1 / t16
      t30 = x1 * x2
      t31 = t30 * z
      t32 = 0.1D1 - x1 + t15 - x2 + t30 - t31 - x3 + t4 - t5 + t7 - t8 +
     # t10 + t23
      t38 = t1 ** 2
      t44 = 0.1D1 / s
      t45 = t38 * t44
      t46 = t16 * t3
      t48 = (-0.1D1 + x1 - t15 + x2 - t30 - t9 + t31) ** 2
      t49 = 0.1D1 / t48
      t50 = -t3
      t51 = bqqbH41J2(s, XB1, XB2, z, lh, wd, t50, x2, x3, x4)
      t54 = x1 ** 2
      t55 = x3 * t54
      t56 = Sin(t11)
      t57 = t56 ** 2
      t58 = z ** 2
      t59 = 0.1D1 / t58
      t62 = x2 * t13
      t63 = t3 ** 2
      t69 = log(0.4D1 * t55 * t57 * t59 * t62 * t26 * t63 * t18)
      t72 = bqqbH41J1(s, XB1, XB2, z, lh, wd, t50, x2, x3, x4)
      t78 = lh * t38
      t84 = 0.90D2 * t45 * (-t46 * t49 * t51 + t69 * t16 * t3 * t49 * t7
     #2) + 0.180D3 * t78 * t44 * t46 * t49 * t72
      t85 = 0.1D1 / x1
      t88 = FJET(XB1, XB2, s, t2 * t3 * (-x3 + t4 - t5 + t7 - t8 + t10 -
     # x2 + t23) * t26, t2 * t4, -t2 * t3 * t32 * t26, -t2 * x1 * t18, -
     #s * t38 * x2 * x1 * t3 * t26, t84 * t85 / 0.2880D4)
      t94 = Sqrt(t62 * x3 * t18)
      t96 = 0.2D1 * t12 * t94
      t102 = (0.1D1 - x2 + t9) ** 2
      t103 = 0.1D1 / t102
      t104 = bqqbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t105 = t103 * t104
      t112 = log(0.4D1 * t55 * t57 * t59 * x2 * t13 * t18)
      t114 = bqqbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t131 = log(0.4D1 * x3 * t57 * t59 * t62 * t18)
      t132 = t131 * t103
      t138 = lh ** 2
      t140 = 0.3141592653589793D1 ** 2
      t146 = t131 ** 2
      t154 = (-0.90D2 * t45 * (t105 - t112 * t103 * t114) + 0.180D3 * t7
     #8 * t44 * t103 * t114) * t85 / 0.2880D4 + (-0.180D3 * (-t105 + t13
     #2 * t114) * lh - t103 * t114 * (0.180D3 * t138 - 0.30D2 * t140) + 
     #0.90D2 * t132 * t104 - 0.45D2 * t146 * t103 * t114) * t38 * t44 / 
     #0.5760D4
      t155 = FJET(XB1, XB2, s, -t2 * (-x3 + t7 - x2 + t96), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t7 + t96), 0.0D0, 0.0D0, t154)
      bqqbH4n1e1 = t88 * t84 * t85 / 0.2880D4 + t155 * t154

      end function



      doubleprecision function bqqbH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH41J1
      doubleprecision bqqbH41J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = x3 * x1
      t5 = t4 * z
      t7 = 0.2D1 * x2 * x3
      t8 = t4 * x2
      t9 = x2 * z
      t10 = t4 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t13 = -0.1D1 + x2
      t15 = x1 * z
      t16 = 0.1D1 - x1 + t15
      t18 = -0.1D1 + x3
      t21 = Sqrt(x3 * t13 * t16 * x2 * t18)
      t23 = 0.2D1 * t12 * t21
      t26 = 0.1D1 / t16
      t30 = x1 * x2
      t31 = t30 * z
      t32 = 0.1D1 - x1 + t15 - x2 + t30 - t31 - x3 + t4 - t5 + t7 - t8 +
     # t10 + t23
      t38 = t1 ** 2
      t44 = 0.1D1 / s
      t45 = t38 * t44
      t48 = (-0.1D1 + x1 - t15 + x2 - t30 - t9 + t31) ** 2
      t52 = bqqbH41J1(s, XB1, XB2, z, lh, wd, -t3, x2, x3, x4)
      t53 = 0.1D1 / x1
      t55 = t3 / t48 * t52 * t53
      t58 = FJET(XB1, XB2, s, t2 * t3 * (-x3 + t4 - t5 + t7 - t8 + t10 -
     # x2 + t23) * t26, t2 * t4, -t2 * t3 * t32 * t26, -t2 * x1 * t18, -
     #s * t38 * x2 * x1 * t3 * t26, -t45 * t16 * t55 / 0.32D2)
      t64 = x2 * t13
      t67 = Sqrt(t64 * x3 * t18)
      t69 = 0.2D1 * t12 * t67
      t75 = (0.1D1 - x2 + t9) ** 2
      t76 = 0.1D1 / t75
      t77 = bqqbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t78 = t76 * t77
      t84 = bqqbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t87 = Sin(t11)
      t88 = t87 ** 2
      t90 = z ** 2
      t96 = log(0.4D1 * x3 * t88 / t90 * t64 * t18)
      t104 = -t45 * t78 * t53 / 0.32D2 + (0.180D3 * t78 * lh - 0.90D2 * 
     #t76 * t84 + 0.90D2 * t96 * t76 * t77) * t38 * t44 / 0.5760D4
      t105 = FJET(XB1, XB2, s, -t2 * (-x3 + t7 - x2 + t69), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t7 + t69), 0.0D0, 0.0D0, t104)
      bqqbH4n1e0 = -t58 * t38 * t44 * t16 * t55 / 0.32D2 + t105 * t104

      end function



      doubleprecision function bqqbH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH41J1
      doubleprecision bqqbH41J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * 0.3141592653589793D1)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t19 = t1 ** 2
      t20 = 0.1D1 / s
      t24 = (0.1D1 - x2 + x2 * z) ** 2
      t25 = 0.1D1 / t24
      t26 = bqqbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t30 = FJET(XB1, XB2, s, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, t2 * (
     #0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 0.0D0, -t19 * t20 * t25 * t26 
     #/ 0.64D2)
      bqqbH4n1em1 = -t30 * t19 * t20 * t25 * t26 / 0.64D2

      end function



      doubleprecision function bqqbH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH41J1
      doubleprecision bqqbH41J2
      bqqbH4n1em2 = 0.0D0

      end function



      doubleprecision function bqqbH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH41J1
      doubleprecision bqqbH41J2
      bqqbH4n1em3 = 0.0D0

      end function



      doubleprecision function bqqbH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH41J1
      doubleprecision bqqbH41J2
      bqqbH4n1em4 = 0.0D0

      end function
  
 

      doubleprecision function bqqbH41J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bqqbH41J1 = -0.16D2 * s * z * wd

      end function
  
   
 

      doubleprecision function bqqbH41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bqqbH41J2 = 0.16D2 * s * z * wd

      end function
  
 