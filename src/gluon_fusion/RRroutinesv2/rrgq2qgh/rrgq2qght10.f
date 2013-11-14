  
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
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
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
      t11 = x4 * pi
      t12 = cos(t11)
      t13 = -0.1D1 + x2
      t15 = 0.1D1 - x1 + t9
      t17 = -0.1D1 + x3
      t20 = Sqrt(x3 * t13 * t15 * x2 * t17)
      t22 = 0.2D1 * t12 * t20
      t25 = 0.1D1 / t15
      t27 = t2 * t3 * (-x3 + t4 - t5 + t7 - t8 + t10 - x2 + t22) * t25
      t28 = t2 * t4
      t29 = x2 * x1
      t30 = t29 * z
      t31 = 0.1D1 - x1 + t9 - x2 + t29 - t30 - x3 + t4 - t5 + t7 - t8 + 
     #t10 + t22
      t34 = t2 * t3 * t31 * t25
      t37 = t17 * s * t1 * x1
      t38 = t1 ** 2
      t43 = s * t38 * x2 * x1 * t3 * t25
      t44 = pi * t1
      t45 = 0.1D1 / s
      t46 = t15 * t3
      t47 = x2 * z
      t49 = (-0.1D1 + x1 - t9 + x2 - t29 - t47 + t30) ** 2
      t50 = 0.1D1 / t49
      t51 = rrgq2qgh101J2(s, XB1, XB2, z, lh, wd, nf, s, t27, -t34, t28,
     # -t37, -t43)
      t54 = x1 ** 2
      t55 = Sin(t11)
      t56 = t55 ** 2
      t57 = t54 * t56
      t58 = z ** 2
      t59 = 0.1D1 / t58
      t62 = t38 ** 2
      t64 = t3 ** 2
      t70 = log(0.4D1 * t6 * t57 * t59 * t62 * t25 * t64 * t13 * t17)
      t73 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, t27, -t34, t28,
     # -t37, -t43)
      t80 = pi * lh
      t87 = -0.90D2 * t44 * t45 * (t46 * t50 * t51 - t70 * t15 * t3 * t5
     #0 * t73) + 0.180D3 * t80 * t1 * t45 * t46 * t50 * t73
      t88 = 0.1D1 / x1
      t91 = FJET(XB1, XB2, s, t27, t28, -t34, -t37, -t43, -t87 * t88 / 0
     #.720D3)
      t98 = Sqrt(x2 * t13 * x3 * t17)
      t100 = 0.2D1 * t12 * t98
      t102 = t2 * (-x3 + t7 - x2 + t100)
      t104 = t2 * (0.1D1 - x2 - x3 + t7 + t100)
      t106 = (0.1D1 - x2 + t47) ** 2
      t107 = 0.1D1 / t106
      t108 = rrgq2qgh101J2(s, XB1, XB2, z, lh, wd, nf, s, -t102, t104, 0
     #.0D0, 0.0D0, 0.0D0)
      t113 = t59 * t62 * t13 * t17
      t116 = log(0.4D1 * t6 * t57 * t113)
      t118 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, -t102, t104, 0
     #.0D0, 0.0D0, 0.0D0)
      t125 = t45 * t107
      t126 = t125 * t118
      t132 = rrgq2qgh101J3(s, XB1, XB2, z, lh, wd, nf, s, -t102, t104, 0
     #.0D0, 0.0D0, 0.0D0)
      t140 = log(0.4D1 * t6 * t56 * t113)
      t141 = t140 * pi
      t150 = t140 ** 2
      t153 = lh ** 2
      t155 = pi ** 2
      t163 = -(-0.90D2 * t44 * t45 * (t107 * t108 - t116 * t107 * t118) 
     #+ 0.180D3 * t80 * t1 * t126) * t88 / 0.720D3 + t44 * t125 * t132 /
     # 0.16D2 - (0.180D3 * t80 + 0.90D2 * t141) * t1 * t125 * t108 / 0.1
     #440D4 - (-0.180D3 * t141 * lh - 0.45D2 * t150 * pi + pi * (-0.180D
     #3 * t153 + 0.30D2 * t155)) * t1 * t126 / 0.1440D4
      t164 = FJET(XB1, XB2, s, -t102, 0.0D0, t104, 0.0D0, 0.0D0, t163)
      rrgq2qght10s1e1 = -t91 * t87 * t88 / 0.720D3 + t164 * t163

      end function



      doubleprecision function rrgq2qght10s1e0
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
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
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
      t11 = x4 * pi
      t12 = cos(t11)
      t13 = -0.1D1 + x2
      t15 = 0.1D1 - x1 + t9
      t17 = -0.1D1 + x3
      t20 = Sqrt(x3 * t13 * t15 * x2 * t17)
      t22 = 0.2D1 * t12 * t20
      t25 = 0.1D1 / t15
      t27 = t2 * t3 * (-x3 + t4 - t5 + t7 - t8 + t10 - x2 + t22) * t25
      t28 = t2 * t4
      t29 = x2 * x1
      t30 = t29 * z
      t31 = 0.1D1 - x1 + t9 - x2 + t29 - t30 - x3 + t4 - t5 + t7 - t8 + 
     #t10 + t22
      t34 = t2 * t3 * t31 * t25
      t37 = t17 * s * t1 * x1
      t38 = t1 ** 2
      t43 = s * t38 * x2 * x1 * t3 * t25
      t44 = pi * t1
      t45 = 0.1D1 / s
      t48 = x2 * z
      t50 = (-0.1D1 + x1 - t9 + x2 - t29 - t48 + t30) ** 2
      t51 = 0.1D1 / t50
      t53 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, t27, -t34, t28,
     # -t37, -t43)
      t54 = 0.1D1 / x1
      t59 = FJET(XB1, XB2, s, t27, t28, -t34, -t37, -t43, t44 * t45 * t1
     #5 * t3 * t51 * t53 * t54 / 0.8D1)
      t72 = Sqrt(x2 * t13 * x3 * t17)
      t74 = 0.2D1 * t12 * t72
      t76 = t2 * (-x3 + t7 - x2 + t74)
      t78 = t2 * (0.1D1 - x2 - x3 + t7 + t74)
      t81 = (0.1D1 - x2 + t48) ** 2
      t82 = 0.1D1 / t81
      t83 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, -t76, t78, 0.0D
     #0, 0.0D0, 0.0D0)
      t88 = t45 * t82
      t89 = rrgq2qgh101J2(s, XB1, XB2, z, lh, wd, nf, s, -t76, t78, 0.0D
     #0, 0.0D0, 0.0D0)
      t95 = Sin(t11)
      t96 = t95 ** 2
      t98 = z ** 2
      t100 = t38 ** 2
      t106 = log(0.4D1 * t6 * t96 / t98 * t100 * t13 * t17)
      t114 = t44 * t45 * t82 * t83 * t54 / 0.8D1 + t44 * t88 * t89 / 0.1
     #6D2 - (0.180D3 * pi * lh + 0.90D2 * t106 * pi) * t1 * t88 * t83 / 
     #0.1440D4
      t115 = FJET(XB1, XB2, s, -t76, 0.0D0, t78, 0.0D0, 0.0D0, t114)
      rrgq2qght10s1e0 = t59 * pi * t1 * t45 * t15 * t3 * t51 * t53 * t54
     # / 0.8D1 + t115 * t114

      end function



      doubleprecision function rrgq2qght10s1em1
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
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * pi)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t23 = (0.1D1 - x2 + x2 * z) ** 2
      t26 = rrgq2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.0D
     #0, 0.0D0, 0.0D0)
      t27 = 0.1D1 / s / t23 * t26
      t30 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, pi * t1 * 
     #t27 / 0.16D2)
      rrgq2qght10s1em1 = t30 * pi * t1 * t27 / 0.16D2

      end function



      doubleprecision function rrgq2qght10s1em2
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
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght10s1em2 = 0.0D0

      end function



      doubleprecision function rrgq2qght10s1em3
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
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght10s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght10s1em4
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
      doubleprecision rrgq2qgh101J1
      doubleprecision rrgq2qgh101J2
      doubleprecision rrgq2qgh101J3
      doubleprecision rrgq2qgh101J4
      doubleprecision rrgq2qgh101J5
      doubleprecision rrgq2qgh101J6
      doubleprecision rrgq2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght10s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh101J1
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
      t7 = S34 ** 2
      t15 = S13 ** 2
      t17 = S24 ** 2
      t23 = S23 ** 2
      t25 = S14 ** 2
      t35 = 0.94D2 / 0.9D1 * t7 + (-0.272D3 / 0.9D1 * S13 + 0.212D3 / 0.
     #9D1 * S23 - 0.272D3 / 0.9D1 * S14 + 0.188D3 / 0.9D1 * S24) * S34 +
     # 0.68D2 / 0.9D1 * t15 + 0.94D2 / 0.9D1 * t17 - 0.272D3 / 0.9D1 * S
     #24 * S14 + 0.212D3 / 0.9D1 * S23 * S24 + 0.118D3 / 0.9D1 * t23 + 0
     #.68D2 / 0.9D1 * t25 + 0.136D3 / 0.9D1 * S13 * S14 - 0.272D3 / 0.9D
     #1 * S14 * S23 - 0.272D3 / 0.9D1 * S23 * S13 - 0.272D3 / 0.9D1 * S1
     #3 * S24
      rrgq2qgh101J1 = (0.68D2 / 0.9D1 * S12 + 0.136D3 / 0.9D1 * S13 - 0.
     #272D3 / 0.9D1 * S24 + 0.136D3 / 0.9D1 * S14 - 0.272D3 / 0.9D1 * S3
     #4 - 0.272D3 / 0.9D1 * S23 + t35 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh101J2
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
      t7 = S34 ** 2
      t15 = S13 ** 2
      t17 = S24 ** 2
      t23 = S23 ** 2
      t25 = S14 ** 2
      t35 = -0.52D2 / 0.9D1 * t7 + (0.160D3 / 0.9D1 * S13 - 0.128D3 / 0.
     #9D1 * S23 + 0.160D3 / 0.9D1 * S14 - 0.104D3 / 0.9D1 * S24) * S34 -
     # 0.40D2 / 0.9D1 * t15 - 0.52D2 / 0.9D1 * t17 + 0.160D3 / 0.9D1 * S
     #24 * S14 - 0.128D3 / 0.9D1 * S23 * S24 - 0.76D2 / 0.9D1 * t23 - 0.
     #40D2 / 0.9D1 * t25 - 0.80D2 / 0.9D1 * S13 * S14 + 0.160D3 / 0.9D1 
     #* S14 * S23 + 0.160D3 / 0.9D1 * S23 * S13 + 0.160D3 / 0.9D1 * S13 
     #* S24
      rrgq2qgh101J2 = (-0.40D2 / 0.9D1 * S12 - 0.80D2 / 0.9D1 * S13 + 0.
     #160D3 / 0.9D1 * S24 - 0.80D2 / 0.9D1 * S14 + 0.160D3 / 0.9D1 * S34
     # + 0.160D3 / 0.9D1 * S23 + t35 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh101J3
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
      rrgq2qgh101J3 = 0.0D0

      end function
  
   
 

      doubleprecision function rrgq2qgh101J4
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
      rrgq2qgh101J4 = 0.0D0

      end function
  
   
 

      doubleprecision function rrgq2qgh101J5
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
      rrgq2qgh101J5 = 0.0D0

      end function
  
   
 

      doubleprecision function rrgq2qgh101J6
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
      t7 = S34 ** 2
      t15 = S13 ** 2
      t17 = S24 ** 2
      t23 = S23 ** 2
      t25 = S14 ** 2
      t35 = -0.94D2 / 0.9D1 * t7 + (0.272D3 / 0.9D1 * S13 - 0.212D3 / 0.
     #9D1 * S23 + 0.272D3 / 0.9D1 * S14 - 0.188D3 / 0.9D1 * S24) * S34 -
     # 0.68D2 / 0.9D1 * t15 - 0.94D2 / 0.9D1 * t17 + 0.272D3 / 0.9D1 * S
     #24 * S14 - 0.212D3 / 0.9D1 * S23 * S24 - 0.118D3 / 0.9D1 * t23 - 0
     #.68D2 / 0.9D1 * t25 - 0.136D3 / 0.9D1 * S13 * S14 + 0.272D3 / 0.9D
     #1 * S14 * S23 + 0.272D3 / 0.9D1 * S23 * S13 + 0.272D3 / 0.9D1 * S1
     #3 * S24
      rrgq2qgh101J6 = (-0.68D2 / 0.9D1 * S12 - 0.136D3 / 0.9D1 * S13 + 0
     #.272D3 / 0.9D1 * S24 - 0.136D3 / 0.9D1 * S14 + 0.272D3 / 0.9D1 * S
     #34 + 0.272D3 / 0.9D1 * S23 + t35 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh101J7
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
      t7 = S34 ** 2
      t19 = S14 ** 2
      t21 = S13 ** 2
      t25 = S23 ** 2
      t29 = S24 ** 2
      t35 = 0.52D2 / 0.9D1 * t7 + (-0.160D3 / 0.9D1 * S14 - 0.160D3 / 0.
     #9D1 * S13 + 0.104D3 / 0.9D1 * S24 + 0.128D3 / 0.9D1 * S23) * S34 +
     # 0.80D2 / 0.9D1 * S13 * S14 + 0.128D3 / 0.9D1 * S23 * S24 + 0.40D2
     # / 0.9D1 * t19 + 0.40D2 / 0.9D1 * t21 - 0.160D3 / 0.9D1 * S24 * S1
     #4 + 0.76D2 / 0.9D1 * t25 - 0.160D3 / 0.9D1 * S13 * S24 + 0.52D2 / 
     #0.9D1 * t29 - 0.160D3 / 0.9D1 * S23 * S13 - 0.160D3 / 0.9D1 * S14 
     #* S23
      rrgq2qgh101J7 = (0.40D2 / 0.9D1 * S12 + 0.80D2 / 0.9D1 * S14 + 0.8
     #0D2 / 0.9D1 * S13 - 0.160D3 / 0.9D1 * S23 - 0.160D3 / 0.9D1 * S34 
     #- 0.160D3 / 0.9D1 * S24 + t35 / S12) / pi * wd / z

      end function
  
 