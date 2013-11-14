  
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
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t4 = t1 * t3
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t10 = t8 * x1 * t3
      t12 = t3 * t5 * t8
      t13 = pi * t2
      t14 = rrq1q1bar2q2q2barh11J2(s, XB1, XB2, z, lh, wd, nf, s, t4, -t
     #10, -t7, t12, 0.0D0)
      t15 = x1 * z
      t16 = -z - x1 + t15
      t17 = 0.1D1 / t16
      t18 = x2 * t17
      t19 = x4 * pi
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
      t37 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, s, t4, -t
     #10, -t7, t12, 0.0D0)
      t42 = pi * lh
      t47 = 0.1D1 / x2
      t53 = t28 * t30
      t54 = x3 * t8
      t58 = log(0.4D1 * t17 * t21 * t23 * t27 * t53 * t54)
      t59 = t58 * pi
      t66 = t58 ** 2
      t69 = lh ** 2
      t71 = pi ** 2
      t79 = -(0.90D2 * t13 * (-t14 + t36 * t37) + 0.180D3 * t42 * t2 * t
     #37) * t47 / 0.720D3 + (-0.180D3 * t42 - 0.90D2 * t59) * t2 * t14 /
     # 0.720D3 + (0.180D3 * t59 * lh + 0.45D2 * t66 * pi + pi * (0.180D3
     # * t69 - 0.30D2 * t71)) * t2 * t37 / 0.720D3
      t80 = FJET(XB1, XB2, s, t4, -t7, -t10, t12, 0.0D0, t79)
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
      t100 = t3 * x1 * (-t82 - t1 + t83 + t85 + t86 - t87 - x2 + t84 + t
     #96) * t17
      t102 = x2 * x1
      t104 = z + x1 - t15 - x2 * z - t102 + t102 * z - t82 - t1 + t83 + 
     #t85 + t86 - t87 + t84 + t96
      t107 = t3 * x1 * t104 * t17
      t112 = s * t26 * x2 * x1 * t5 * t17
      t113 = rrq1q1bar2q2q2barh11J2(s, XB1, XB2, z, lh, wd, nf, s, t100,
     # -t107, -t7, t12, t112)
      t120 = log(-0.4D1 * t18 * t24 * t27 * t53 * t54 * t89)
      t121 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, s, t100,
     # -t107, -t7, t12, t112)
      t129 = 0.90D2 * t13 * (t113 - t120 * t121) - 0.180D3 * t42 * t2 * 
     #t121
      t132 = FJET(XB1, XB2, s, t100, -t7, -t107, t12, t112, -t129 * t47 
     #/ 0.720D3)
      rrq1q1bar2q2q2barht1s1e1 = t80 * t79 - t132 * t129 * t47 / 0.720D3

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t4 = t1 * t3
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t10 = t8 * x1 * t3
      t12 = t3 * t5 * t8
      t13 = pi * t2
      t14 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, s, t4, -t
     #10, -t7, t12, 0.0D0)
      t15 = 0.1D1 / x2
      t19 = rrq1q1bar2q2q2barh11J2(s, XB1, XB2, z, lh, wd, nf, s, t4, -t
     #10, -t7, t12, 0.0D0)
      t24 = x1 * z
      t25 = -z - x1 + t24
      t26 = 0.1D1 / t25
      t27 = x4 * pi
      t28 = Sin(t27)
      t29 = t28 ** 2
      t31 = z ** 2
      t33 = t2 ** 2
      t34 = t33 ** 2
      t37 = x1 ** 2
      t38 = t5 ** 2
      t44 = log(0.4D1 * t26 * t29 / t31 * t34 * t37 * t38 * x3 * t8)
      t51 = t13 * t14 * t15 / 0.8D1 + t13 * t19 / 0.8D1 + (-0.180D3 * pi
     # * lh - 0.90D2 * t44 * pi) * t2 * t14 / 0.720D3
      t52 = FJET(XB1, XB2, s, t4, -t7, -t10, t12, 0.0D0, t51)
      t54 = x3 * z
      t55 = t1 * z
      t56 = x2 * x3
      t57 = t56 * z
      t58 = t56 * x1
      t59 = t56 * t24
      t60 = cos(t27)
      t66 = Sqrt(-x3 * (-0.1D1 + x2) * t25 * x2 * t8)
      t68 = 0.2D1 * t60 * t66
      t72 = t3 * x1 * (-t54 - t1 + t55 + t57 + t58 - t59 - x2 + t56 + t6
     #8) * t26
      t74 = x2 * x1
      t76 = z + x1 - t24 - x2 * z - t74 + t74 * z - t54 - t1 + t55 + t57
     # + t58 - t59 + t56 + t68
      t79 = t3 * x1 * t76 * t26
      t84 = s * t33 * x2 * x1 * t5 * t26
      t85 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, s, t72, -
     #t79, -t7, t12, t84)
      t89 = FJET(XB1, XB2, s, t72, -t7, -t79, t12, t84, -t13 * t85 * t15
     # / 0.8D1)
      rrq1q1bar2q2q2barht1s1e0 = t52 * t51 - t89 * pi * t2 * t85 * t15 /
     # 0.8D1

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = -0.1D1 + z
      t3 = t2 * s
      t4 = x3 * x1 * t3
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t10 = t8 * x1 * t3
      t12 = t3 * t5 * t8
      t14 = rrq1q1bar2q2q2barh11J1(s, XB1, XB2, z, lh, wd, nf, s, t4, -t
     #10, -t7, t12, 0.0D0)
      t17 = FJET(XB1, XB2, s, t4, -t7, -t10, t12, 0.0D0, pi * t2 * t14 /
     # 0.8D1)
      rrq1q1bar2q2q2barht1s1em1 = t17 * pi * t2 * t14 / 0.8D1

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrq1q1bar2q2q2barht1s1em2 = 0.0D0

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrq1q1bar2q2q2barht1s1em3 = 0.0D0

      end function



      doubleprecision function rrq1q1bar2q2q2barht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q1bar2q2q2barh11J1
      doubleprecision rrq1q1bar2q2q2barh11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrq1q1bar2q2q2barht1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrq1q1bar2q2q2barh11J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = nf - 0.1D1
      t8 = S14 ** 2
      t9 = S23 ** 2
      t10 = S13 ** 2
      t11 = S24 ** 2
      t16 = S12 ** 2
      rrq1q1bar2q2q2barh11J1 = (0.64D2 / 0.9D1 * t1 * S34 / S12 + 0.32D2
     # / 0.9D1 * (-0.2D1 * S14 * S23 + t8 + t9 + t10 + t11 - 0.2D1 * S13
     # * S24) * t1 / t16) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrq1q1bar2q2q2barh11J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t7 = S23 ** 2
      t8 = S14 ** 2
      t9 = S13 ** 2
      t10 = S24 ** 2
      t21 = S12 ** 2
      rrq1q1bar2q2q2barh11J2 = 0.32D2 / 0.9D1 * wd * (-0.2D1 * S13 * S23
     # - 0.2D1 * S23 * S24 - 0.2D1 * S14 * S23 - t7 - t8 - t9 - t10 - 0.
     #2D1 * S14 * S24 - 0.2D1 * S13 * S14 - 0.2D1 * S13 * S24) * (nf - 0
     #.1D1) / t21 / z / pi

      end function
  
 