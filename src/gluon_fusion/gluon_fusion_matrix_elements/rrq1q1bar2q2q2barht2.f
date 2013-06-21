  
      subroutine rrq1q1bar2q2q2barht2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrq1q1bar2q2q2barht2s1e1  
      doubleprecision rrq1q1bar2q2q2barht2s1e0  
      doubleprecision rrq1q1bar2q2q2barht2s1em1  
      doubleprecision rrq1q1bar2q2q2barht2s1em2  
      doubleprecision rrq1q1bar2q2q2barht2s1em3  
      doubleprecision rrq1q1bar2q2q2barht2s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrq1q1bar2q2q2barht2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrq1q1bar2q2q2barht2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = t2 ** 2
      t14 = t13 * t2
      t15 = t14 * x1
      t16 = t5 * wd
      t17 = nf - 0.1D1
      t20 = 0.1D1 / z
      t21 = x1 * z
      t22 = -z - x1 + t21
      t23 = 0.1D1 / t22
      t24 = t20 * t23
      t26 = x4 * 0.3141592653589793D1
      t27 = Sin(t26)
      t28 = t27 ** 2
      t29 = z ** 2
      t30 = 0.1D1 / t29
      t33 = t13 ** 2
      t34 = x1 ** 2
      t35 = t33 * t34
      t36 = t5 ** 2
      t42 = log(0.4D1 * x2 * t23 * t28 * t30 * t35 * t36 * x3 * t8)
      t43 = cos(t26)
      t44 = t43 ** 2
      t48 = Sqrt(x3 * t22 * t8)
      t49 = t48 ** 2
      t54 = lh * t14
      t55 = x1 * t5
      t56 = t55 * wd
      t58 = t17 * t20
      t61 = t58 * t23 * t44 * t49
      t65 = 0.1D1 / x2
      t76 = log(0.4D1 * t23 * t28 * t30 * t33 * t34 * t36 * x3 * t8)
      t79 = t76 ** 2
      t81 = lh ** 2
      t83 = 0.3141592653589793D1 ** 2
      t90 = 0.4D1 / 0.405D3 * (-0.360D3 * t15 * t16 * t17 * t24 * t42 * 
     #t44 * t49 - 0.720D3 * t54 * t56 * t61) * t65 + 0.16D2 / 0.405D3 * 
     #(0.180D3 * t76 * lh + 0.45D2 * t79 + 0.180D3 * t81 - 0.30D2 * t83)
     # * t14 * t56 * t61
      t91 = FJET(XB1, XB2, s, t1 * t3, -t7, -t8 * x1 * t3, t12, 0.0D0, t
     #90)
      t93 = x3 * z
      t94 = t1 * z
      t95 = x2 * x3
      t96 = t95 * z
      t97 = t95 * x1
      t98 = t95 * t21
      t99 = sqrt(x2)
      t101 = -0.1D1 + t99
      t103 = t99 + 0.1D1
      t107 = Sqrt(-x3 * t101 * t103 * t22 * t8)
      t109 = 0.2D1 * t43 * t99 * t107
      t115 = x2 * x1
      t117 = z + x1 - t21 - x2 * z - t115 + t115 * z - t93 - t1 + t94 + 
     #t96 + t97 - t98 + t95 + t109
      t136 = log(-0.4D1 * t35 * t36 * x2 * x3 * t8 * t28 * t30 * t101 * 
     #t103 * t23)
      t143 = (-t99 + 0.2D1 * t99 * x3 + 0.2D1 * t43 * t107) ** 2
      t154 = 0.90D2 * t15 * t16 * t58 * t23 * t136 * t143 + 0.180D3 * t5
     #4 * t55 * wd * t17 * t24 * t143
      t157 = FJET(XB1, XB2, s, t3 * x1 * (-t93 - t1 + t94 + t96 + t97 - 
     #t98 - x2 + t95 + t109) * t23, -t7, -t3 * x1 * t117 * t23, t12, s *
     # t13 * x2 * t55 * t23, 0.4D1 / 0.405D3 * t154 * t65)
      rrq1q1bar2q2q2barht2s1e1 = t91 * t90 + 0.4D1 / 0.405D3 * t157 * t1
     #54 * t65

      end function



      doubleprecision function rrq1q1bar2q2q2barht2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = t2 ** 2
      t14 = t13 * t2
      t15 = t14 * x1
      t16 = t5 * wd
      t17 = nf - 0.1D1
      t20 = 0.1D1 / z
      t21 = x1 * z
      t22 = -z - x1 + t21
      t23 = 0.1D1 / t22
      t25 = x4 * 0.3141592653589793D1
      t26 = cos(t25)
      t27 = t26 ** 2
      t30 = Sqrt(x3 * t22 * t8)
      t31 = t30 ** 2
      t33 = 0.1D1 / x2
      t39 = Sin(t25)
      t40 = t39 ** 2
      t42 = z ** 2
      t44 = t13 ** 2
      t47 = x1 ** 2
      t48 = t5 ** 2
      t54 = log(0.4D1 * t23 * t40 / t42 * t44 * t47 * t48 * x3 * t8)
      t58 = x1 * t5
      t59 = t58 * wd
      t61 = t17 * t20
      t67 = 0.32D2 / 0.9D1 * t15 * t16 * t17 * t20 * t23 * t27 * t31 * t
     #33 + 0.16D2 / 0.405D3 * (-0.180D3 * lh - 0.90D2 * t54) * t14 * t59
     # * t61 * t23 * t27 * t31
      t68 = FJET(XB1, XB2, s, t1 * t3, -t7, -t8 * x1 * t3, t12, 0.0D0, t
     #67)
      t70 = x3 * z
      t71 = t1 * z
      t72 = x2 * x3
      t73 = t72 * z
      t74 = t72 * x1
      t75 = t72 * t21
      t76 = sqrt(x2)
      t84 = Sqrt(-x3 * (-0.1D1 + t76) * (t76 + 0.1D1) * t22 * t8)
      t86 = 0.2D1 * t26 * t76 * t84
      t92 = x2 * x1
      t94 = z + x1 - t21 - x2 * z - t92 + t92 * z - t70 - t1 + t71 + t73
     # + t74 - t75 + t72 + t86
      t108 = (-t76 + 0.2D1 * t76 * x3 + 0.2D1 * t26 * t84) ** 2
      t111 = t61 * t23 * t108 * t33
      t114 = FJET(XB1, XB2, s, t3 * x1 * (-t70 - t1 + t71 + t73 + t74 - 
     #t75 - x2 + t72 + t86) * t23, -t7, -t3 * x1 * t94 * t23, t12, s * t
     #13 * x2 * t58 * t23, -0.8D1 / 0.9D1 * t15 * t16 * t111)
      rrq1q1bar2q2q2barht2s1e0 = t68 * t67 - 0.8D1 / 0.9D1 * t114 * t14 
     #* t59 * t111

      end function



      doubleprecision function rrq1q1bar2q2q2barht2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t13 = t2 ** 2
      t14 = t13 * t2
      t22 = -z - x1 + x1 * z
      t25 = cos(x4 * 0.3141592653589793D1)
      t26 = t25 ** 2
      t30 = Sqrt(x3 * t22 * t8)
      t31 = t30 ** 2
      t33 = (nf - 0.1D1) / z / t22 * t26 * t31
      t36 = FJET(XB1, XB2, s, x3 * x1 * t3, -t3 * t5 * x3, -t8 * x1 * t3
     #, t3 * t5 * t8, 0.0D0, 0.32D2 / 0.9D1 * t14 * x1 * t5 * wd * t33)
      rrq1q1bar2q2q2barht2s1em1 = 0.32D2 / 0.9D1 * t36 * t14 * x1 * t5 *
     # wd * t33

      end function



      doubleprecision function rrq1q1bar2q2q2barht2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrq1q1bar2q2q2barht2s1em2 = 0.0D0

      end function



      doubleprecision function rrq1q1bar2q2q2barht2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrq1q1bar2q2q2barht2s1em3 = 0.0D0

      end function



      doubleprecision function rrq1q1bar2q2q2barht2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrq1q1bar2q2q2barht2s1em4 = 0.0D0

      end function
