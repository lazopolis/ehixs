  
      subroutine rrqqbar2qqbarht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2qqbarh11J1  
      doubleprecision rrqqbar2qqbarh11J2  
      doubleprecision rrqqbar2qqbarh11J3  
      doubleprecision rrqqbar2qqbarht1s1e1  
      doubleprecision rrqqbar2qqbarht1s1e0  
      doubleprecision rrqqbar2qqbarht1s1em1  
      doubleprecision rrqqbar2qqbarht1s1em2  
      doubleprecision rrqqbar2qqbarht1s1em3  
      doubleprecision rrqqbar2qqbarht1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2qqbarht1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh11J1
      doubleprecision rrqqbar2qqbarh11J2
      doubleprecision rrqqbar2qqbarh11J3
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = 0.3141592653589793D1 * t2
      t14 = rrqqbar2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3
     #, x4)
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
      t37 = rrqqbar2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3
     #, x4)
      t42 = 0.3141592653589793D1 * lh
      t47 = 0.1D1 / x2
      t50 = rrqqbar2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3
     #, x4)
      t57 = t28 * t30
      t58 = x3 * t8
      t62 = log(0.4D1 * t17 * t21 * t23 * t27 * t57 * t58)
      t63 = t62 * 0.3141592653589793D1
      t71 = t62 ** 2
      t74 = lh ** 2
      t76 = 0.3141592653589793D1 ** 2
      t84 = -(0.90D2 * t13 * (-t14 + t36 * t37) + 0.180D3 * t42 * t2 * t
     #37) * t47 / 0.720D3 + t13 * t50 / 0.8D1 + (-0.180D3 * t42 - 0.90D2
     # * t63) * t2 * t14 / 0.720D3 + (0.180D3 * t63 * lh + 0.45D2 * t71 
     #* 0.3141592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t74 - 0
     #.30D2 * t76)) * t2 * t37 / 0.720D3
      t85 = FJET(XB1, XB2, s, t1 * t3, -t7, -t8 * x1 * t3, t12, 0.0D0, t
     #84)
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
      t107 = x2 * x1
      t109 = z + x1 - t15 - x2 * z - t107 + t107 * z - t87 - t1 + t88 + 
     #t90 + t91 - t92 + t89 + t101
      t118 = rrqqbar2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t125 = log(-0.4D1 * t18 * t24 * t27 * t57 * t58 * t94)
      t126 = rrqqbar2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 
     #x4)
      t134 = 0.90D2 * t13 * (t118 - t125 * t126) - 0.180D3 * t42 * t2 * 
     #t126
      t137 = FJET(XB1, XB2, s, t3 * x1 * (-t87 - t1 + t88 + t90 + t91 - 
     #t92 - x2 + t89 + t101) * t17, -t7, -t3 * x1 * t109 * t17, t12, s *
     # t26 * x2 * x1 * t5 * t17, -t134 * t47 / 0.720D3)
      rrqqbar2qqbarht1s1e1 = t85 * t84 - t137 * t134 * t47 / 0.720D3

      end function



      doubleprecision function rrqqbar2qqbarht1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh11J1
      doubleprecision rrqqbar2qqbarh11J2
      doubleprecision rrqqbar2qqbarh11J3
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = 0.3141592653589793D1 * t2
      t14 = rrqqbar2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3
     #, x4)
      t15 = 0.1D1 / x2
      t19 = rrqqbar2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3
     #, x4)
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
      t85 = rrqqbar2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x
     #4)
      t89 = FJET(XB1, XB2, s, t3 * x1 * (-t54 - t1 + t55 + t57 + t58 - t
     #59 - x2 + t56 + t68) * t26, -t7, -t3 * x1 * t76 * t26, t12, s * t3
     #3 * x2 * x1 * t5 * t26, -t13 * t85 * t15 / 0.8D1)
      rrqqbar2qqbarht1s1e0 = t52 * t51 - t89 * 0.3141592653589793D1 * t2
     # * t85 * t15 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh11J1
      doubleprecision rrqqbar2qqbarh11J2
      doubleprecision rrqqbar2qqbarh11J3
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t14 = rrqqbar2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3
     #, x4)
      t17 = FJET(XB1, XB2, s, x3 * x1 * t3, -t3 * t5 * x3, -t8 * x1 * t3
     #, t3 * t5 * t8, 0.0D0, 0.3141592653589793D1 * t2 * t14 / 0.8D1)
      rrqqbar2qqbarht1s1em1 = t17 * 0.3141592653589793D1 * t2 * t14 / 0.
     #8D1

      end function



      doubleprecision function rrqqbar2qqbarht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh11J1
      doubleprecision rrqqbar2qqbarh11J2
      doubleprecision rrqqbar2qqbarh11J3
      rrqqbar2qqbarht1s1em2 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh11J1
      doubleprecision rrqqbar2qqbarh11J2
      doubleprecision rrqqbar2qqbarh11J3
      rrqqbar2qqbarht1s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh11J1
      doubleprecision rrqqbar2qqbarh11J2
      doubleprecision rrqqbar2qqbarh11J3
      rrqqbar2qqbarht1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2qqbarh11J1
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
      t10 = t5 / t8
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + x2 * t14 - t23
      t25 = t24 ** 2
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t35 = t4 * x1
      t36 = 0.1D1 / t7
      t50 = t14 * t11 * t7 + x2 * x3 + t23
      t56 = x3 ** 2
      t60 = t50 ** 2
      rrqqbar2qqbarh11J1 = -0.32D2 / 0.27D2 * wd * (-0.4D1 * t4 * t10 * 
     #t25 - 0.4D1 * t4 * t30 * t31 + 0.8D1 * t35 * t36 * t24 * t29 * t14
     # - 0.8D1 * t4 * x2 * x1 * t29 * t36 + 0.4D1 * t35 * t36 * t50 * t2
     #9 * x3 - 0.3D1 * t4 * t30 * t56 - 0.3D1 * t4 * t10 * t60) / t1 / z
     # / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh11J2
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
      t11 = 0.1D1 - x3
      t12 = 0.1D1 - x2
      t17 = cos(x4 * 0.3141592653589793D1)
      t18 = x3 * t12
      t22 = Sqrt(t18 * t8 * x2 * t11)
      t24 = 0.2D1 * t17 * t22
      t25 = t11 * t12 * t8 + x2 * x3 + t24
      t26 = t10 * t25
      t29 = t18 * t8 + x2 * t11 - t24
      t33 = t1 * t2
      t34 = 0.1D1 - x1
      t35 = t34 * x3
      t37 = t4 * x1
      t38 = 0.1D1 / t8
      t39 = t38 * t29
      t40 = t34 * t11
      t44 = t34 ** 2
      t54 = t38 * t25
      t61 = t5 * t10
      t62 = t25 ** 2
      t66 = t29 ** 2
      t72 = t1 * t3 * t2 * x2
      t76 = t11 ** 2
      t83 = x3 ** 2
      t91 = 0.6D1 * t4 * t5 * t26 * t29 + t33 * t35 + 0.8D1 * t37 * t39 
     #* t40 + 0.6D1 * t4 * t44 * x3 * t11 + 0.6D1 * t4 * t34 * x3 * x1 *
     # t39 + 0.6D1 * t37 * t54 * t40 + 0.6D1 * t37 * t54 * t35 + 0.3D1 *
     # t4 * t61 * t62 + 0.4D1 * t4 * t61 * t66 + t72 * t5 * t34 * t26 + 
     #0.4D1 * t4 * t44 * t76 + t33 * x1 * t38 * t25 + 0.3D1 * t4 * t44 *
     # t83 + t72 * x1 * t44 * t38 * x3
      rrqqbar2qqbarh11J2 = -0.32D2 / 0.27D2 * wd * t91 / t1 / z / 0.3141
     #592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh11J3
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
      t6 = t5 * x2
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t11 = z + x1 * t2
      t12 = 0.1D1 / t11
      t16 = t1 * t3
      t17 = x1 ** 2
      t19 = t11 ** 2
      t20 = 0.1D1 / t19
      t21 = 0.1D1 - x3
      t22 = 0.1D1 - x2
      t27 = cos(x4 * 0.3141592653589793D1)
      t28 = x3 * t22
      t32 = Sqrt(t28 * t11 * x2 * t21)
      t34 = 0.2D1 * t27 * t32
      t35 = t21 * t22 * t11 + x2 * x3 + t34
      t36 = t20 * t35
      t38 = x2 * t21
      t39 = t28 * t11 + t38 - t34
      t45 = t1 * t2
      t46 = x1 * t12
      t59 = t16 * x1
      t60 = t12 * t39
      t61 = t7 * t21
      t70 = t21 ** 2
      t75 = t39 ** 2
      t85 = -t6 * x1 * t8 * t12 * x3 + t16 * t17 * t36 * t39 - t6 * t17 
     #* t7 * t36 - t45 * t46 * t35 - t45 * t7 * x3 + t16 * t8 * x3 * t21
     # - t5 * t17 * t20 * t39 * x2 * t7 + 0.2D1 * t59 * t60 * t61 - t45 
     #* t46 * t39 + t59 * t12 * t35 * t61 + t16 * t8 * t70 - t45 * t61 +
     # t16 * t17 * t20 * t75 - t5 * t8 * t38 * t46 + t16 * t7 * x3 * x1 
     #* t60
      rrqqbar2qqbarh11J3 = -0.32D2 / 0.27D2 * wd * t85 / t1 / z / 0.3141
     #592653589793D1

      end function
  
 