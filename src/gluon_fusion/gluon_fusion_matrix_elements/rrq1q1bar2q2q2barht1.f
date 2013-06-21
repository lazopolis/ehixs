  
      subroutine rrq1q1bar2q2q2barht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrq1q1bar2q2q2barh11J1  
      doubleprecision rrq1q1bar2q2q2barh11J2  
      doubleprecision rrq1q1bar2q2q2barht1s1e1  
      doubleprecision rrq1q1bar2q2q2barht1s1e0  
      doubleprecision rrq1q1bar2q2q2barht1s1em1  
      doubleprecision rrq1q1bar2q2q2barht1s1em2  
      doubleprecision rrq1q1bar2q2q2barht1s1em3  
      doubleprecision rrq1q1bar2q2q2barht1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrq1q1bar2q2q2barht1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = 0.3141592653589793D1 * t2
      t14 = rrq1q1bar2q2q2barh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0
     #, x3, x4)
      t15 = x1 * z
      t16 = -z - x1 + t15
      t17 = 0.1D1 / t16
      t18 = x2 * t17
      t19 = x4 * 0.3141592653589793D1
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t21 * t23
      t26 = t2 ** 2
      t27 = t26 ** 2
      t28 = x1 ** 2
      t30 = t5 ** 2
      t36 = log(0.4D1 * t18 * t24 * t27 * t28 * t30 * x3 * t8)
      t37 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0
     #, x3, x4)
      t42 = 0.3141592653589793D1 * lh
      t47 = 0.1D1 / x2
      t53 = t28 * t30
      t54 = x3 * t8
      t58 = log(0.4D1 * t17 * t21 * t23 * t27 * t53 * t54)
      t59 = t58 * 0.3141592653589793D1
      t66 = t58 ** 2
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t79 = -(0.90D2 * t13 * (-t14 + t36 * t37) + 0.180D3 * t42 * t2 * t
     #37) * t47 / 0.720D3 + (-0.180D3 * t42 - 0.90D2 * t59) * t2 * t14 /
     # 0.720D3 + (0.180D3 * t59 * lh + 0.45D2 * t66 * 0.3141592653589793
     #D1 + 0.3141592653589793D1 * (0.180D3 * t69 - 0.30D2 * t71)) * t2 *
     # t37 / 0.720D3
      t80 = FJET(XB1, XB2, s, t1 * t3, -t7, -t8 * x1 * t3, t12, 0.0D0, t
     #79)
      t82 = x3 * z
      t83 = t1 * z
      t84 = x2 * x3
      t85 = t84 * z
      t86 = t84 * x1
      t87 = t84 * t15
      t88 = cos(t19)
      t89 = -0.1D1 + x2
      t94 = Sqrt(-x3 * t89 * t16 * x2 * t8)
      t96 = 0.2D1 * t88 * t94
      t102 = x2 * x1
      t104 = z + x1 - t15 - x2 * z - t102 + t102 * z - t82 - t1 + t83 + 
     #t85 + t86 - t87 + t84 + t96
      t113 = rrq1q1bar2q2q2barh11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 
     #x3, x4)
      t120 = log(-0.4D1 * t18 * t24 * t27 * t53 * t54 * t89)
      t121 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 
     #x3, x4)
      t129 = 0.90D2 * t13 * (t113 - t120 * t121) - 0.180D3 * t42 * t2 * 
     #t121
      t132 = FJET(XB1, XB2, s, t3 * x1 * (-t82 - t1 + t83 + t85 + t86 - 
     #t87 - x2 + t84 + t96) * t17, -t7, -t3 * x1 * t104 * t17, t12, s * 
     #t26 * x2 * x1 * t5 * t17, -t129 * t47 / 0.720D3)
      rrq1q1bar2q2q2barht1s1e1 = t80 * t79 - t132 * t129 * t47 / 0.720D3

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = 0.3141592653589793D1 * t2
      t14 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0
     #, x3, x4)
      t15 = 0.1D1 / x2
      t19 = rrq1q1bar2q2q2barh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0
     #, x3, x4)
      t24 = x1 * z
      t25 = -z - x1 + t24
      t26 = 0.1D1 / t25
      t27 = x4 * 0.3141592653589793D1
      t28 = Sin(t27)
      t29 = t28 ** 2
      t31 = z ** 2
      t33 = t2 ** 2
      t34 = t33 ** 2
      t37 = x1 ** 2
      t38 = t5 ** 2
      t44 = log(0.4D1 * t26 * t29 / t31 * t34 * t37 * t38 * x3 * t8)
      t51 = t13 * t14 * t15 / 0.8D1 + t13 * t19 / 0.8D1 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t44 * 0.3141592653589793D1) * t
     #2 * t14 / 0.720D3
      t52 = FJET(XB1, XB2, s, t1 * t3, -t7, -t8 * x1 * t3, t12, 0.0D0, t
     #51)
      t54 = x3 * z
      t55 = t1 * z
      t56 = x2 * x3
      t57 = t56 * z
      t58 = t56 * x1
      t59 = t56 * t24
      t60 = cos(t27)
      t66 = Sqrt(-x3 * (-0.1D1 + x2) * t25 * x2 * t8)
      t68 = 0.2D1 * t60 * t66
      t74 = x2 * x1
      t76 = z + x1 - t24 - x2 * z - t74 + t74 * z - t54 - t1 + t55 + t57
     # + t58 - t59 + t56 + t68
      t85 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x
     #3, x4)
      t89 = FJET(XB1, XB2, s, t3 * x1 * (-t54 - t1 + t55 + t57 + t58 - t
     #59 - x2 + t56 + t68) * t26, -t7, -t3 * x1 * t76 * t26, t12, s * t3
     #3 * x2 * x1 * t5 * t26, -t13 * t85 * t15 / 0.8D1)
      rrq1q1bar2q2q2barht1s1e0 = t52 * t51 - t89 * 0.3141592653589793D1 
     #* t2 * t85 * t15 / 0.8D1

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t14 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0
     #, x3, x4)
      t17 = FJET(XB1, XB2, s, x3 * x1 * t3, -t3 * t5 * x3, -t8 * x1 * t3
     #, t3 * t5 * t8, 0.0D0, 0.3141592653589793D1 * t2 * t14 / 0.8D1)
      rrq1q1bar2q2q2barht1s1em1 = t17 * 0.3141592653589793D1 * t2 * t14 
     #/ 0.8D1

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2
      rrq1q1bar2q2q2barht1s1em2 = 0.0D0

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2
      rrq1q1bar2q2q2barht1s1em3 = 0.0D0

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2
      rrq1q1bar2q2q2barht1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrq1q1bar2q2q2barh11J1
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
      t6 = 0.1D1 - x1
      t9 = z + x1 * t2
      t10 = 0.1D1 / t9
      t14 = t6 ** 2
      t15 = x3 ** 2
      t18 = x1 ** 2
      t19 = t9 ** 2
      t21 = t18 / t19
      t22 = 0.1D1 - x2
      t23 = x3 * t22
      t25 = 0.1D1 - x3
      t28 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(t23 * t9 * x2 * t25)
      t34 = 0.2D1 * t28 * t32
      t35 = t23 * t9 + x2 * t25 - t34
      t36 = t35 ** 2
      t39 = t25 ** 2
      t45 = t25 * t22 * t9 + x2 * x3 + t34
      t46 = t45 ** 2
      t49 = t4 * x1
      rrq1q1bar2q2q2barh11J1 = 0.32D2 / 0.9D1 * wd * (0.2D1 * t4 * x2 * 
     #x1 * t6 * t10 + t4 * t14 * t15 + t4 * t21 * t36 + t4 * t14 * t39 +
     # t4 * t21 * t46 - 0.2D1 * t49 * t10 * t35 * t6 * t25 - 0.2D1 * t49
     # * t10 * t45 * t6 * x3) * (nf - 0.1D1) / t1 / z / 0.31415926535897
     #93D1

      end function
  
   
 

      doubleprecision function rrq1q1bar2q2q2barh11J2
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
      t5 = t4 * x1
      t7 = z + x1 * t2
      t8 = 0.1D1 / t7
      t9 = 0.1D1 - x3
      t10 = 0.1D1 - x2
      t15 = cos(x4 * 0.3141592653589793D1)
      t16 = x3 * t10
      t20 = Sqrt(t16 * t7 * x2 * t9)
      t22 = 0.2D1 * t15 * t20
      t23 = t9 * t10 * t7 + x2 * x3 + t22
      t24 = t8 * t23
      t25 = 0.1D1 - x1
      t26 = t25 * t9
      t32 = t16 * t7 + x2 * t9 - t22
      t33 = t8 * t32
      t37 = t25 * x3
      t41 = x1 ** 2
      t43 = t7 ** 2
      t44 = 0.1D1 / t43
      t49 = t25 ** 2
      t57 = t9 ** 2
      t60 = x3 ** 2
      t63 = t41 * t44
      t64 = t23 ** 2
      t67 = t32 ** 2
      rrq1q1bar2q2q2barh11J2 = 0.32D2 / 0.9D1 * wd * (-0.2D1 * t5 * t24 
     #* t26 - 0.2D1 * t5 * t33 * t26 - 0.2D1 * t5 * t33 * t37 - 0.2D1 * 
     #t4 * t41 * t44 * t32 * t23 - 0.2D1 * t4 * t49 * x3 * t9 - 0.2D1 * 
     #t5 * t24 * t37 - t4 * t49 * t57 - t4 * t49 * t60 - t4 * t63 * t64 
     #- t4 * t63 * t67) * (nf - 0.1D1) / t1 / z / 0.3141592653589793D1

      end function
  
 