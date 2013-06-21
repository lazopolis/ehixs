  
      subroutine rrqqbar2qqbarht5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2qqbarht5s1e1  
      doubleprecision rrqqbar2qqbarht5s1e0  
      doubleprecision rrqqbar2qqbarht5s1em1  
      doubleprecision rrqqbar2qqbarht5s1em2  
      doubleprecision rrqqbar2qqbarht5s1em3  
      doubleprecision rrqqbar2qqbarht5s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2qqbarht5s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x3 * x1
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t8 * t5 * t2
      t13 = x1 * z
      t14 = -z - x1 + t13
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 * t1
      t19 = x1 * t5
      t20 = t15 * t17 * t19
      t21 = 0.1D1 / z
      t22 = t21 * wd
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t27 = z ** 2
      t28 = 0.1D1 / t27
      t31 = t16 ** 2
      t32 = x1 ** 2
      t34 = t5 ** 2
      t40 = log(0.4D1 * x2 * t15 * t26 * t28 * t31 * t32 * t34 * x3 * t8
     #)
      t41 = cos(t24)
      t42 = t41 ** 2
      t46 = Sqrt(x3 * t14 * t8)
      t47 = t46 ** 2
      t54 = t15 * lh * t17 * x1
      t55 = t5 * t21
      t62 = 0.1D1 / x2
      t68 = t32 * t34
      t73 = log(0.4D1 * t15 * t26 * t28 * t31 * t68 * x3 * t8)
      t77 = t73 ** 2
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t93 = 0.4D1 / 0.405D3 * (-0.360D3 * t20 * t22 * t40 * t42 * t47 - 
     #0.720D3 * t54 * t55 * wd * t42 * t47) * t62 - 0.16D2 / 0.405D3 * (
     #-0.180D3 * t73 * t15 * lh - 0.45D2 * t77 * t15 + t15 * (-0.180D3 *
     # t80 + 0.30D2 * t82)) * t17 * t19 * t22 * t42 * t47
      t94 = FJET(XB1, XB2, s, t2 * t3, -t7, -t8 * x1 * t2, t12, 0.0D0, t
     #93)
      t96 = x3 * z
      t97 = t3 * z
      t98 = x2 * x3
      t99 = t98 * z
      t100 = t98 * x1
      t101 = t98 * t13
      t102 = sqrt(x2)
      t104 = -0.1D1 + t102
      t106 = t102 + 0.1D1
      t110 = Sqrt(-x3 * t104 * t106 * t14 * t8)
      t112 = 0.2D1 * t41 * t102 * t110
      t118 = x2 * x1
      t120 = z + x1 - t13 - x2 * z - t118 + t118 * z - t96 - t3 + t97 + 
     #t99 + t100 - t101 + t98 + t112
      t138 = log(-0.4D1 * t15 * t31 * t68 * t28 * t104 * t106 * x3 * t8 
     #* t26 * x2)
      t144 = (-t102 + 0.2D1 * t102 * x3 + 0.2D1 * t41 * t110) ** 2
      t153 = 0.90D2 * t20 * t22 * t138 * t144 + 0.180D3 * t54 * t55 * wd
     # * t144
      t156 = FJET(XB1, XB2, s, t2 * x1 * (-t96 - t3 + t97 + t99 + t100 -
     # t101 - x2 + t98 + t112) * t15, -t7, -t2 * x1 * t120 * t15, t12, s
     # * t16 * x2 * t19 * t15, 0.4D1 / 0.405D3 * t153 * t62)
      rrqqbar2qqbarht5s1e1 = t94 * t93 + 0.4D1 / 0.405D3 * t156 * t153 *
     # t62

      end function



      doubleprecision function rrqqbar2qqbarht5s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x3 * x1
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t8 * t5 * t2
      t13 = x1 * z
      t14 = -z - x1 + t13
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 * t1
      t19 = x1 * t5
      t20 = t15 * t17 * t19
      t21 = 0.1D1 / z
      t22 = t21 * wd
      t23 = x4 * 0.3141592653589793D1
      t24 = cos(t23)
      t25 = t24 ** 2
      t28 = Sqrt(x3 * t14 * t8)
      t29 = t28 ** 2
      t30 = t25 * t29
      t31 = 0.1D1 / x2
      t38 = Sin(t23)
      t39 = t38 ** 2
      t41 = z ** 2
      t43 = t16 ** 2
      t46 = x1 ** 2
      t47 = t5 ** 2
      t53 = log(0.4D1 * t15 * t39 / t41 * t43 * t46 * t47 * x3 * t8)
      t62 = 0.32D2 / 0.9D1 * t20 * t22 * t30 * t31 - 0.16D2 / 0.405D3 * 
     #(0.180D3 * t15 * lh + 0.90D2 * t53 * t15) * t17 * t19 * t22 * t30
      t63 = FJET(XB1, XB2, s, t2 * t3, -t7, -t8 * x1 * t2, t12, 0.0D0, t
     #62)
      t65 = x3 * z
      t66 = t3 * z
      t67 = x2 * x3
      t68 = t67 * z
      t69 = t67 * x1
      t70 = t67 * t13
      t71 = sqrt(x2)
      t79 = Sqrt(-x3 * (-0.1D1 + t71) * (t71 + 0.1D1) * t14 * t8)
      t81 = 0.2D1 * t24 * t71 * t79
      t87 = x2 * x1
      t89 = z + x1 - t13 - x2 * z - t87 + t87 * z - t65 - t3 + t66 + t68
     # + t69 - t70 + t67 + t81
      t102 = (-t71 + 0.2D1 * t71 * x3 + 0.2D1 * t24 * t79) ** 2
      t107 = FJET(XB1, XB2, s, t2 * x1 * (-t65 - t3 + t66 + t68 + t69 - 
     #t70 - x2 + t67 + t81) * t15, -t7, -t2 * x1 * t89 * t15, t12, s * t
     #16 * x2 * t19 * t15, -0.8D1 / 0.9D1 * t20 * t22 * t102 * t31)
      rrqqbar2qqbarht5s1e0 = t63 * t62 - 0.8D1 / 0.9D1 * t107 * t15 * t1
     #7 * x1 * t5 * t21 * wd * t102 * t31

      end function



      doubleprecision function rrqqbar2qqbarht5s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t14 = -z - x1 + x1 * z
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 * t1
      t21 = 0.1D1 / z
      t24 = cos(x4 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t28 = Sqrt(x3 * t14 * t8)
      t29 = t28 ** 2
      t34 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t5 * x3, -t8 * x1 * t2
     #, t8 * t5 * t2, 0.0D0, 0.32D2 / 0.9D1 * t15 * t17 * x1 * t5 * t21 
     #* wd * t25 * t29)
      rrqqbar2qqbarht5s1em1 = 0.32D2 / 0.9D1 * t34 * t15 * t17 * x1 * t5
     # * t21 * wd * t25 * t29

      end function



      doubleprecision function rrqqbar2qqbarht5s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2qqbarht5s1em2 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht5s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2qqbarht5s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2qqbarht5s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqqbar2qqbarht5s1em4 = 0.0D0

      end function
