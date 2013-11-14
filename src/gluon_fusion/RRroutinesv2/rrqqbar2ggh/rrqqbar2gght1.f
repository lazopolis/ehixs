  
      subroutine rrqqbar2gght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh11J1  
      doubleprecision rrqqbar2ggh11J2  
      doubleprecision rrqqbar2ggh11J3  
      doubleprecision rrqqbar2gght1s1e1  
      doubleprecision rrqqbar2gght1s1e0  
      doubleprecision rrqqbar2gght1s1em1  
      doubleprecision rrqqbar2gght1s1em2  
      doubleprecision rrqqbar2gght1s1em3  
      doubleprecision rrqqbar2gght1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght1s1e1
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
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3

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
      t14 = rrqqbar2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t4, -t10, -t7
     #, t12, 0.0D0)
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
      t37 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t4, -t10, -t7
     #, t12, 0.0D0)
      t42 = pi * lh
      t47 = 0.1D1 / x2
      t50 = rrqqbar2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, t4, -t10, -t7
     #, t12, 0.0D0)
      t57 = t28 * t30
      t58 = x3 * t8
      t62 = log(0.4D1 * t17 * t21 * t23 * t27 * t57 * t58)
      t63 = t62 * pi
      t71 = t62 ** 2
      t74 = lh ** 2
      t76 = pi ** 2
      t84 = -(0.90D2 * t13 * (-t14 + t36 * t37) + 0.180D3 * t42 * t2 * t
     #37) * t47 / 0.720D3 + t13 * t50 / 0.8D1 + (-0.180D3 * t42 - 0.90D2
     # * t63) * t2 * t14 / 0.720D3 + (0.180D3 * t63 * lh + 0.45D2 * t71 
     #* pi + pi * (0.180D3 * t74 - 0.30D2 * t76)) * t2 * t37 / 0.720D3
      t85 = FJET(XB1, XB2, s, t4, -t7, -t10, t12, 0.0D0, t84)
      t87 = x3 * z
      t88 = t1 * z
      t89 = x2 * x3
      t90 = t89 * z
      t91 = t89 * x1
      t92 = t89 * t15
      t93 = cos(t19)
      t94 = -0.1D1 + x2
      t99 = Sqrt(-x3 * t94 * t16 * x2 * t8)
      t101 = 0.2D1 * t93 * t99
      t105 = t3 * x1 * (-t87 - t1 + t88 + t90 + t91 - t92 - x2 + t89 + t
     #101) * t17
      t107 = x2 * x1
      t109 = z + x1 - t15 - x2 * z - t107 + t107 * z - t87 - t1 + t88 + 
     #t90 + t91 - t92 + t89 + t101
      t112 = t3 * x1 * t109 * t17
      t117 = s * t26 * x2 * x1 * t5 * t17
      t118 = rrqqbar2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t105, -t112,
     # -t7, t12, t117)
      t125 = log(-0.4D1 * t18 * t24 * t27 * t57 * t58 * t94)
      t126 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t105, -t112,
     # -t7, t12, t117)
      t134 = 0.90D2 * t13 * (t118 - t125 * t126) - 0.180D3 * t42 * t2 * 
     #t126
      t137 = FJET(XB1, XB2, s, t105, -t7, -t112, t12, t117, -t134 * t47 
     #/ 0.720D3)
      rrqqbar2gght1s1e1 = t85 * t84 - t137 * t134 * t47 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght1s1e0
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
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3

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
      t14 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t4, -t10, -t7
     #, t12, 0.0D0)
      t15 = 0.1D1 / x2
      t19 = rrqqbar2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t4, -t10, -t7
     #, t12, 0.0D0)
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
      t85 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t72, -t79, -t
     #7, t12, t84)
      t89 = FJET(XB1, XB2, s, t72, -t7, -t79, t12, t84, -t13 * t85 * t15
     # / 0.8D1)
      rrqqbar2gght1s1e0 = t52 * t51 - t89 * pi * t2 * t85 * t15 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght1s1em1
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
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3

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
      t14 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t4, -t10, -t7
     #, t12, 0.0D0)
      t17 = FJET(XB1, XB2, s, t4, -t7, -t10, t12, 0.0D0, pi * t2 * t14 /
     # 0.8D1)
      rrqqbar2gght1s1em1 = t17 * pi * t2 * t14 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght1s1em2
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
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght1s1em2 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght1s1em3
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
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght1s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght1s1em4
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
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh11J1
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
      t1 = S12 ** 2
      t2 = 0.1D1 / t1
      t4 = S12 + S13 + S23
      t5 = 0.1D1 / t4
      t7 = S12 + S14 + S24
      t8 = 0.1D1 / t7
      t9 = s ** 2
      t12 = z ** 2
      t17 = S34 * t5
      t18 = 0.1D1 / S12
      t22 = -t8 - t5
      t24 = 0.64D2 * t22 * S34
      t26 = S24 * S14
      t27 = 0.32D2 / 0.3D1 * t26
      t28 = S24 ** 2
      t29 = 0.16D2 / 0.3D1 * t28
      t30 = S14 ** 2
      t31 = 0.16D2 / 0.3D1 * t30
      t33 = t7 ** 2
      t34 = 0.1D1 / t33
      t36 = S23 * S24
      t37 = S14 * S23
      t38 = S13 * S24
      t39 = S13 * S14
      t44 = S13 ** 2
      t45 = 0.16D2 / 0.3D1 * t44
      t46 = S23 * S13
      t47 = 0.32D2 / 0.3D1 * t46
      t48 = S23 ** 2
      t49 = 0.16D2 / 0.3D1 * t48
      t51 = t4 ** 2
      t52 = 0.1D1 / t51
      t65 = S13 + S23
      t74 = 0.64D2 / 0.3D1 * t30
      t75 = 0.64D2 / 0.3D1 * t44
      t76 = 0.128D3 / 0.3D1 * t46
      t77 = 0.64D2 / 0.3D1 * t28
      t78 = 0.64D2 / 0.3D1 * t48
      t79 = 0.128D3 / 0.3D1 * t26
      t86 = 0.32D2 / 0.3D1 * t48
      t87 = 0.32D2 / 0.3D1 * t38
      t88 = 0.32D2 / 0.3D1 * t36
      t89 = 0.32D2 / 0.3D1 * t44
      t90 = 0.32D2 / 0.3D1 * t37
      t91 = 0.32D2 / 0.3D1 * t39
      t94 = 0.32D2 / 0.3D1 * t28
      t95 = 0.32D2 / 0.3D1 * t30
      t104 = t5 * t8
      t114 = S34 ** 2
      t118 = 0.1088D4 / 0.27D2 * S13
      t120 = 0.1088D4 / 0.27D2 * S23
      t124 = 0.1088D4 / 0.27D2 * S24
      t125 = 0.1088D4 / 0.27D2 * S14
      t148 = t114 * S34
      t172 = 0.1088D4 / 0.27D2 * t37
      t174 = 0.1088D4 / 0.27D2 * t39
      t175 = 0.1088D4 / 0.27D2 * t38
      t177 = 0.1088D4 / 0.27D2 * t36
      t184 = 0.32D2 * t36
      t187 = 0.32D2 * t39
      t206 = 0.1664D4 / 0.3D1 * t37
      t207 = 0.64D2 / 0.3D1 * t46
      t208 = 0.1648D4 / 0.3D1 * t36
      t209 = 0.1648D4 / 0.3D1 * t39
      t210 = 0.1664D4 / 0.3D1 * t38
      t213 = 0.64D2 / 0.3D1 * t26
      t228 = 0.16D2 / 0.3D1 * t36
      t229 = 0.16D2 / 0.3D1 * t38
      t230 = 0.16D2 / 0.3D1 * t39
      t231 = 0.16D2 / 0.3D1 * t37
      t246 = -0.160D3 / 0.3D1 * t22 * t148 + (0.256D3 / 0.3D1 + 0.160D3 
     #/ 0.3D1 * t65 * t8 + (0.160D3 / 0.3D1 * S14 + 0.160D3 / 0.3D1 * S2
     #4) * t5) * t114 + ((-t228 + t229 - t230 - t94 - t95 + t213 + t231)
     # * t8 + (t207 + t231 - t86 - t230 - t228 + t229 - t89) * t5) * S34
     # + 0.16D2 * t44 + 0.16D2 * t28 + 0.16D2 * t48 + t184 + 0.160D3 / 0
     #.3D1 * t38 + 0.160D3 / 0.3D1 * t37 + t187 + 0.16D2 * t30 + 0.32D2 
     #* t46 + 0.32D2 * t26
      rrqqbar2ggh11J1 = (0.64D2 / 0.3D1 * t2 * S34 * t5 * t8 * t9 * s * 
     #t12 * z + (0.64D2 * t17 * t8 * t18 + t24 * t2 + ((t27 - t29 - t31)
     # * t34 + (0.32D2 / 0.3D1 * t36 - 0.32D2 / 0.3D1 * t37 - 0.32D2 / 0
     #.3D1 * t38 + 0.32D2 / 0.3D1 * t39) * t8 * t5 + (-t45 + t47 - t49) 
     #* t52) * S34 / t1 / S12) * t9 * t12 + (0.64D2 * t17 * t8 + t24 * t
     #18 + ((0.128D3 + 0.64D2 * t65 * t8 + (0.64D2 * S14 + 0.64D2 * S24 
     #+ (0.128D3 / 0.3D1 * t36 - 0.128D3 / 0.3D1 * t38 - 0.128D3 / 0.3D1
     # * t37 + 0.128D3 / 0.3D1 * t39 + t74 + t75 - t76 + t77 + t78 - t79
     #) * t8) * t5) * S34 + (t86 - t87 - t27 + t31 + t88 + t89 + t29 - t
     #90 + t91) * t8 + (t88 + t91 - t87 + t49 - t47 + t45 + t94 + t95 - 
     #t90) * t5) * t2) * s * z + (-0.472D3 / 0.27D2 * t34 + 0.64D2 / 0.3
     #D1 * t104 - 0.472D3 / 0.27D2 * t52) * S34 * S12 + (0.1088D4 / 0.27
     #D2 * t34 + 0.128D3 / 0.27D2 * t104 + 0.1088D4 / 0.27D2 * t52) * t1
     #14 + (0.3466D4 / 0.27D2 * t8 + (-0.848D3 / 0.27D2 * S24 + t118 - 0
     #.848D3 / 0.27D2 * S14 + t120) * t34 + 0.3466D4 / 0.27D2 * t5 + (t1
     #24 + t125 - 0.848D3 / 0.27D2 * S13 - 0.848D3 / 0.27D2 * S23) * t52
     #) * S34 + (-0.1648D4 / 0.3D1 * S13 - 0.1648D4 / 0.3D1 * S23 - 0.16
     #D2 / 0.3D1 * S14 - 0.16D2 / 0.3D1 * S24) * t8 + (-0.1648D4 / 0.3D1
     # * S14 - 0.16D2 / 0.3D1 * S23 - 0.16D2 / 0.3D1 * S13 - 0.1648D4 / 
     #0.3D1 * S24) * t5 + ((-0.272D3 / 0.27D2 * t34 - 0.224D3 / 0.27D2 *
     # t104 - 0.272D3 / 0.27D2 * t52) * t148 + (-0.544D3 / 0.9D1 * t8 + 
     #(-0.544D3 / 0.27D2 * S13 - 0.544D3 / 0.27D2 * S23 + t124 + t125) *
     # t34 - 0.544D3 / 0.9D1 * t5 + (t120 + t118 - 0.544D3 / 0.27D2 * S1
     #4 - 0.544D3 / 0.27D2 * S24) * t52) * t114 + (-0.2632D4 / 0.9D1 + (
     #0.3523D4 / 0.27D2 * S14 - 0.1244D4 / 0.27D2 * S13 + 0.3523D4 / 0.2
     #7D2 * S24 - 0.1244D4 / 0.27D2 * S23) * t8 + (-0.272D3 / 0.27D2 * t
     #44 - 0.272D3 / 0.27D2 * t48 - 0.752D3 / 0.27D2 * t26 - 0.376D3 / 0
     #.27D2 * t30 + t172 - 0.376D3 / 0.27D2 * t28 + t174 + t175 - 0.544D
     #3 / 0.27D2 * t46 + t177) * t34 + (-0.1244D4 / 0.27D2 * S24 - 0.124
     #4D4 / 0.27D2 * S14 + 0.3523D4 / 0.27D2 * S13 + 0.3523D4 / 0.27D2 *
     # S23 + (-t79 + t75 + t77 + t74 + t184 - 0.32D2 * t38 - 0.32D2 * t3
     #7 + t187 + t78 - t76) * t8) * t5 + (-0.272D3 / 0.27D2 * t28 - 0.27
     #2D3 / 0.27D2 * t30 + t172 + t174 - 0.544D3 / 0.27D2 * t26 + t177 -
     # 0.752D3 / 0.27D2 * t46 - 0.376D3 / 0.27D2 * t48 + t175 - 0.376D3 
     #/ 0.27D2 * t44) * t52) * S34 + 0.1664D4 / 0.3D1 * S23 + 0.1664D4 /
     # 0.3D1 * S13 + 0.1664D4 / 0.3D1 * S14 + 0.1664D4 / 0.3D1 * S24 + (
     #-t31 - t29 - t206 - t207 - t208 - t27 - t209 - t210) * t8 + (-t206
     # - t209 - t210 - t47 - t213 - t208 - t45 - t49) * t5) * t18 + t246
     # * t2) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh11J2
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
      t1 = S12 ** 2
      t2 = 0.1D1 / t1
      t4 = S12 + S13 + S23
      t5 = 0.1D1 / t4
      t7 = S12 + S14 + S24
      t8 = 0.1D1 / t7
      t9 = s ** 2
      t12 = z ** 2
      t17 = S34 * t5
      t18 = 0.1D1 / S12
      t22 = t8 + t5
      t24 = 0.64D2 * t22 * S34
      t26 = S24 * S14
      t28 = S24 ** 2
      t30 = S14 ** 2
      t33 = t7 ** 2
      t34 = 0.1D1 / t33
      t36 = S14 * S23
      t37 = S13 * S24
      t38 = S23 * S24
      t39 = S13 * S14
      t44 = S23 ** 2
      t46 = S13 ** 2
      t48 = S23 * S13
      t51 = t4 ** 2
      t52 = 0.1D1 / t51
      t57 = 0.1D1 / t1 / S12
      t67 = (-0.64D2 * S23 - 0.64D2 * S13) * t8
      t70 = (-0.64D2 * S24 - 0.64D2 * S14) * t5
      t73 = 0.32D2 / 0.3D1 * t39
      t74 = 0.32D2 / 0.3D1 * t46
      t75 = 0.32D2 / 0.3D1 * t28
      t76 = 0.32D2 / 0.3D1 * t36
      t77 = 0.32D2 / 0.3D1 * t38
      t78 = 0.64D2 / 0.3D1 * t48
      t79 = 0.32D2 / 0.3D1 * t44
      t80 = 0.32D2 / 0.3D1 * t30
      t81 = 0.32D2 / 0.3D1 * t37
      t82 = 0.64D2 / 0.3D1 * t26
      t105 = t5 * t8
      t118 = S34 ** 2
      t121 = 0.64D2 * S13
      t124 = 0.64D2 * S23
      t130 = 0.64D2 * S14
      t131 = 0.64D2 * S24
      t152 = t118 * S34
      t173 = 0.16D2 * t44
      t174 = 0.32D2 * t48
      t175 = 0.64D2 * t39
      t177 = 0.16D2 * t46
      t178 = 0.64D2 * t38
      t180 = 0.64D2 * t36
      t181 = 0.64D2 * t37
      t190 = 0.16D2 * t28
      t191 = 0.16D2 * t30
      t194 = 0.32D2 * t26
      t204 = 0.1744D4 / 0.3D1 * t38
      t207 = 0.1744D4 / 0.3D1 * t39
      t209 = 0.576D3 * t36
      t210 = 0.576D3 * t37
      t229 = 0.8D1 / 0.3D1 * t39
      t230 = 0.8D1 / 0.3D1 * t38
      t231 = 0.8D1 / 0.3D1 * t36
      t232 = 0.8D1 / 0.3D1 * t37
      t247 = -0.160D3 / 0.3D1 * t22 * t152 + (-0.160D3 + t67 + t70) * t1
     #18 + (-0.104D3 * S14 - 0.104D3 * S23 - 0.104D3 * S24 - 0.104D3 * S
     #13 + (t229 + t230 - t231 - t74 - t78 - t79 - t232) * t8 + (-t231 -
     # t232 - t75 + t229 - t80 - t82 + t230) * t5) * S34 - 0.112D3 / 0.3
     #D1 * t46 - 0.112D3 / 0.3D1 * t28 - 0.112D3 / 0.3D1 * t44 - 0.224D3
     # / 0.3D1 * t38 - t181 - t180 - 0.224D3 / 0.3D1 * t39 - 0.112D3 / 0
     #.3D1 * t30 - 0.224D3 / 0.3D1 * t48 - 0.224D3 / 0.3D1 * t26
      t249 = -0.64D2 / 0.3D1 * t2 * S34 * t5 * t8 * t9 * s * t12 * z + (
     #-0.64D2 * t17 * t8 * t18 + t24 * t2 + ((-0.32D2 / 0.3D1 * t26 + 0.
     #16D2 / 0.3D1 * t28 + 0.16D2 / 0.3D1 * t30) * t34 + (0.32D2 / 0.3D1
     # * t36 + 0.32D2 / 0.3D1 * t37 - 0.32D2 / 0.3D1 * t38 - 0.32D2 / 0.
     #3D1 * t39) * t8 * t5 + (0.16D2 / 0.3D1 * t44 + 0.16D2 / 0.3D1 * t4
     #6 - 0.32D2 / 0.3D1 * t48) * t52) * S34 * t57) * t9 * t12 + (-0.64D
     #2 * t17 * t8 + t24 * t18 + ((-0.128D3 + t67 + t70) * S34 + (t73 - 
     #t74 - t75 - t76 + t77 - t78 - t79 - t80 - t81 + t82) * t8 + (-t76 
     #- t74 - t75 + t78 - t80 + t77 + t73 - t79 - t82 - t81) * t5) * t2 
     #+ ((t75 - t82 + t80) * t8 * t4 - 0.64D2 / 0.3D1 * t38 + 0.64D2 / 0
     #.3D1 * t37 - 0.64D2 / 0.3D1 * t39 + 0.64D2 / 0.3D1 * t36 + (t79 + 
     #t74 - t78) * t7 * t5) * t57) * s * z + ((0.776D3 / 0.27D2 * t34 - 
     #0.352D3 / 0.27D2 * t105 + 0.776D3 / 0.27D2 * t52) * S34 + 0.344D3 
     #/ 0.3D1 * t8 + 0.344D3 / 0.3D1 * t5) * S12 + (-0.64D2 * t34 - 0.10
     #24D4 / 0.27D2 * t105 - 0.64D2 * t52) * t118 + (0.4940D4 / 0.27D2 *
     # t8 + (-t121 + 0.1360D4 / 0.27D2 * S24 + 0.1360D4 / 0.27D2 * S14 -
     # t124) * t34 + 0.4940D4 / 0.27D2 * t5 + (0.1360D4 / 0.27D2 * S13 +
     # 0.1360D4 / 0.27D2 * S23 - t130 - t131) * t52) * S34 - 0.688D3 / 0
     #.3D1 + (0.80D2 * S14 + 0.80D2 * S24 + 0.576D3 * S13 + 0.576D3 * S2
     #3) * t8 + (0.80D2 * S13 + 0.576D3 * S24 + 0.576D3 * S14 + 0.80D2 *
     # S23) * t5 + ((0.16D2 * t34 + 0.1376D4 / 0.27D2 * t105 + 0.16D2 * 
     #t52) * t152 + (0.4816D4 / 0.27D2 * t8 + (-t130 - t131 + 0.32D2 * S
     #23 + 0.32D2 * S13) * t34 + 0.4816D4 / 0.27D2 * t5 + (0.32D2 * S24 
     #- t124 - t121 + 0.32D2 * S14) * t52) * t118 + (-0.9344D4 / 0.27D2 
     #+ (0.3776D4 / 0.27D2 * S23 + 0.3776D4 / 0.27D2 * S13 + 0.5266D4 / 
     #0.27D2 * S24 + 0.5266D4 / 0.27D2 * S14) * t8 + (0.584D3 / 0.27D2 *
     # t28 + t173 + t174 - t175 + 0.1168D4 / 0.27D2 * t26 + t177 - t178 
     #+ 0.584D3 / 0.27D2 * t30 - t180 - t181) * t34 + (0.5266D4 / 0.27D2
     # * S13 + 0.3776D4 / 0.27D2 * S14 + 0.3776D4 / 0.27D2 * S24 + 0.526
     #6D4 / 0.27D2 * S23) * t5 + (t190 + t191 + 0.1168D4 / 0.27D2 * t48 
     #+ 0.584D3 / 0.27D2 * t44 + t194 - t180 - t181 + 0.584D3 / 0.27D2 *
     # t46 - t178 - t175) * t52) * S34 - 0.1624D4 / 0.3D1 * S24 - 0.1624
     #D4 / 0.3D1 * S13 - 0.1624D4 / 0.3D1 * S14 - 0.1624D4 / 0.3D1 * S23
     # + (t204 - 0.104D3 / 0.3D1 * t28 + t173 - 0.104D3 / 0.3D1 * t30 + 
     #t177 + t207 + t174 - 0.208D3 / 0.3D1 * t26 + t209 + t210) * t8 + (
     #t209 + t207 + t210 - 0.208D3 / 0.3D1 * t48 + t191 + t204 + t190 - 
     #0.104D3 / 0.3D1 * t44 - 0.104D3 / 0.3D1 * t46 + t194) * t5) * t18 
     #+ t247 * t2
      rrqqbar2ggh11J2 = t249 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh11J3
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
      t1 = S12 + S14 + S24
      t2 = t1 ** 2
      t3 = 0.1D1 / t2
      t5 = S12 + S13 + S23
      t6 = 0.1D1 / t5
      t7 = 0.1D1 / t1
      t8 = t6 * t7
      t9 = 0.64D2 / 0.3D1 * t8
      t10 = t5 ** 2
      t11 = 0.1D1 / t10
      t20 = S34 ** 2
      t25 = 0.640D3 / 0.27D2 * S23
      t26 = 0.640D3 / 0.27D2 * S13
      t30 = 0.640D3 / 0.27D2 * S24
      t31 = 0.640D3 / 0.27D2 * S14
      t59 = S24 ** 2
      t61 = S23 ** 2
      t63 = S23 * S13
      t66 = 0.640D3 / 0.27D2 * S13 * S14
      t67 = S24 * S14
      t69 = S13 ** 2
      t72 = 0.640D3 / 0.27D2 * S23 * S24
      t73 = S14 ** 2
      t76 = 0.640D3 / 0.27D2 * S14 * S23
      t78 = 0.640D3 / 0.27D2 * S13 * S24
      rrqqbar2ggh11J3 = ((-0.304D3 / 0.27D2 * t3 - t9 - 0.304D3 / 0.27D2
     # * t11) * S34 * S12 + (0.640D3 / 0.27D2 * t3 + 0.128D3 / 0.3D1 * t
     #8 + 0.640D3 / 0.27D2 * t11) * t20 + (-0.424D3 / 0.27D2 * t7 + (-0.
     #512D3 / 0.27D2 * S24 - 0.512D3 / 0.27D2 * S14 + t25 + t26) * t3 - 
     #0.424D3 / 0.27D2 * t6 + (t30 + t31 - 0.512D3 / 0.27D2 * S23 - 0.51
     #2D3 / 0.27D2 * S13) * t11) * S34 + ((-0.160D3 / 0.27D2 * t3 - t9 -
     # 0.160D3 / 0.27D2 * t11) * t20 * S34 + (-0.304D3 / 0.9D1 * t7 + (-
     #0.320D3 / 0.27D2 * S13 + t31 + t30 - 0.320D3 / 0.27D2 * S23) * t3 
     #- 0.304D3 / 0.9D1 * t6 + (t25 + t26 - 0.320D3 / 0.27D2 * S24 - 0.3
     #20D3 / 0.27D2 * S14) * t11) * t20 + (0.64D2 + (-t25 - t26 - 0.260D
     #3 / 0.9D1 * S24 - 0.260D3 / 0.9D1 * S14) * t7 + (-0.208D3 / 0.27D2
     # * t59 - 0.160D3 / 0.27D2 * t61 - 0.320D3 / 0.27D2 * t63 + t66 - 0
     #.416D3 / 0.27D2 * t67 - 0.160D3 / 0.27D2 * t69 + t72 - 0.208D3 / 0
     #.27D2 * t73 + t76 + t78) * t3 + (-0.260D3 / 0.9D1 * S13 - t31 - t3
     #0 - 0.260D3 / 0.9D1 * S23) * t6 + (-0.160D3 / 0.27D2 * t59 - 0.160
     #D3 / 0.27D2 * t73 - 0.416D3 / 0.27D2 * t63 - 0.208D3 / 0.27D2 * t6
     #1 - 0.320D3 / 0.27D2 * t67 + t76 + t78 - 0.208D3 / 0.27D2 * t69 + 
     #t72 + t66) * t11) * S34) / S12) / pi * wd / z

      end function
  
 