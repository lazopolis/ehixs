  
      subroutine gbgbH5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH51J1  
      doubleprecision gbgbH51J2  
      doubleprecision gbgbH52J1  
      doubleprecision gbgbH52J2  
      doubleprecision gbgbH52J3  
      doubleprecision gbgbH53J1  
      doubleprecision gbgbH53J2  
      doubleprecision gbgbH54J1  
      doubleprecision gbgbH54J2  
      doubleprecision gbgbH54J3  
      doubleprecision gbgbH5n1e1  
      doubleprecision gbgbH5n1e0  
      doubleprecision gbgbH5n1em1  
      doubleprecision gbgbH5n1em2  
      doubleprecision gbgbH5n1em3  
      doubleprecision gbgbH5n1em4  
      doubleprecision gbgbH5n2e1  
      doubleprecision gbgbH5n2e0  
      doubleprecision gbgbH5n2em1  
      doubleprecision gbgbH5n2em2  
      doubleprecision gbgbH5n2em3  
      doubleprecision gbgbH5n2em4  
      doubleprecision gbgbH5n3e1  
      doubleprecision gbgbH5n3e0  
      doubleprecision gbgbH5n3em1  
      doubleprecision gbgbH5n3em2  
      doubleprecision gbgbH5n3em3  
      doubleprecision gbgbH5n3em4  
      doubleprecision gbgbH5n4e1  
      doubleprecision gbgbH5n4e0  
      doubleprecision gbgbH5n4em1  
      doubleprecision gbgbH5n4em2  
      doubleprecision gbgbH5n4em3  
      doubleprecision gbgbH5n4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH5n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=gbgbH5n3e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=gbgbH5n4e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH5n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=gbgbH5n3e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=gbgbH5n4e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH5n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=gbgbH5n3em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=gbgbH5n4em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH5n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=gbgbH5n3em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=gbgbH5n4em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH5n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=gbgbH5n3em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=gbgbH5n4em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=gbgbH5n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=gbgbH5n3em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=gbgbH5n4em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH5n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t6 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t7 = x1 ** 2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t16 = log(0.4D1 * t14)
      t17 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t19 = t16 ** 2
      t20 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = 0.180D3 * t26 - 0.30D2 * t28
      t31 = t4 * t30
      t36 = t19 * t16
      t48 = -0.2884936567583026D3 - 0.120D3 * t26 * lh + 0.60D2 * lh * t
     #28
      t49 = t4 * t48
      t50 = t49 * t20
      t52 = 0.1D1 / x1
      t55 = t7 * x3
      t56 = t13 * t10
      t59 = log(0.4D1 * t55 * t56)
      t61 = z * t17
      t62 = -0.1D1 + x3
      t63 = 0.1D1 / t62
      t64 = t56 * t63
      t67 = log(-0.4D1 * t55 * t64)
      t68 = t67 * z
      t71 = cos(t8)
      t73 = Sqrt(-x3 * t62)
      t78 = 0.1D1 / (-z - x3 + 0.2D1 * t71 * t73 * z)
      t84 = t59 ** 2
      t87 = z * t6
      t89 = t67 ** 2
      t90 = t89 * z
      t100 = t20 + z * t20 * t78
      t103 = 0.1D1 / x3
      t107 = x2 ** 2
      t108 = t107 * x3
      t109 = t108 * t14
      t111 = log(0.4D1 * t109)
      t113 = t108 * t7
      t116 = log(-0.4D1 * t113 * t64)
      t117 = t116 * z
      t124 = -t100
      t129 = 0.1D1 / x2
      t130 = t129 * t52
      t133 = t107 * t7
      t136 = log(0.4D1 * t133 * t56)
      t142 = t136 ** 2
      t148 = t31 * t20
      t154 = log(0.4D1 * t56)
      t155 = t154 ** 2
      t158 = t155 * t154
      t161 = t155 ** 2
      t168 = t28 ** 2
      t169 = t26 ** 2
      t173 = 0.5769873135166051D3 * lh + t168 + 0.60D2 * t169 - 0.60D2 *
     # t26 * t28
      t196 = x3 * t10
      t199 = log(0.4D1 * t196 * t13)
      t200 = t199 ** 2
      t204 = log(-0.4D1 * t196 * t13 * t63)
      t205 = t204 ** 2
      t209 = -t200 / 0.2D1 - t205 * z * t78 / 0.2D1
      t218 = t204 * z * t78 + t199
      t226 = t200 * t199 / 0.6D1 + t205 * t204 * z * t78 / 0.6D1
      t234 = -z * t78 - 0.1D1
      t242 = log(0.4D1 * t107 * t13 * t10)
      t244 = t242 ** 2
      t254 = t244 * t242
      t267 = log(0.4D1 * t108 * t56)
      t271 = log(-0.4D1 * t108 * t64)
      t272 = t271 * z
      t280 = t267 ** 2
      t284 = t271 ** 2
      t285 = t284 * z
      t298 = (-0.180D3 * t5 * (-t6 + t16 * t17 - t19 * t20 / 0.2D1) + t3
     #1 * (-t17 + t16 * t20) + 0.90D2 * t4 * (t16 * t6 + t36 * t20 / 0.6
     #D1 - t19 * t17 / 0.2D1) - t50) * t52 / 0.5760D4 - (-0.180D3 * t5 *
     # (-t59 * t20 + t17 + (t61 - t68 * t20) * t78) + 0.90D2 * t4 * (t6 
     #- t59 * t17 + t84 * t20 / 0.2D1 + (t87 - t68 * t17 + t90 * t20 / 0
     #.2D1) * t78) + t31 * t100) * t103 * t52 / 0.5760D4 + (0.90D2 * t4 
     #* (t111 * t20 - t17 - (t61 - t117 * t20) * t78) - 0.180D3 * t5 * t
     #124) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t136 * t20 - t17
     #) + 0.90D2 * t4 * (-t6 + t136 * t17 - t142 * t20 / 0.2D1) - t148) 
     #* t129 * t52 / 0.2880D4 - (0.45D2 * t155 * t6 - 0.15D2 * t158 * t1
     #7 + 0.15D2 / 0.4D1 * t161 * t20 + (t17 - t154 * t20) * t48 + t20 *
     # t173 + (t6 - t154 * t17 + t155 * t20 / 0.2D1) * t30 - 0.180D3 * (
     #-t154 * t6 + t155 * t17 / 0.2D1 - t158 * t20 / 0.6D1) * lh) * t4 /
     # 0.11520D5 + ((0.90D2 * t4 * t17 - 0.180D3 * t5 * t20) * t209 + (0
     #.90D2 * t4 * t6 - 0.180D3 * t5 * t17 + t148) * t218 + 0.90D2 * t4 
     #* t20 * t226 + (-0.180D3 * t5 * t6 + t31 * t17 + t50) * t234) * t1
     #03 / 0.11520D5 - (-0.180D3 * t5 * (t6 - t242 * t17 + t244 * t20 / 
     #0.2D1) + t31 * (t17 - t242 * t20) + 0.90D2 * t4 * (-t242 * t6 - t2
     #54 * t20 / 0.6D1 + t244 * t17 / 0.2D1) + t50) * t129 / 0.5760D4 + 
     #(-0.180D3 * t5 * (t267 * t20 - t17 - (t61 - t272 * t20) * t78) + 0
     #.90D2 * t4 * (-t6 + t267 * t17 - t280 * t20 / 0.2D1 - (t87 - t272 
     #* t17 + t285 * t20 / 0.2D1) * t78) + t31 * t124) * t103 * t129 / 0
     #.5760D4
      t299 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t298)
      t301 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t304 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t319 = t49 * t301
      t324 = z * t304
      t344 = t301 + z * t301 * t78
      t357 = -t344
      t374 = t31 * t301
      t463 = (-0.180D3 * t5 * (-t19 * t301 / 0.2D1 + t16 * t304) + t31 *
     # (t16 * t301 - t304) + 0.90D2 * t4 * (t36 * t301 / 0.6D1 - t19 * t
     #304 / 0.2D1) - t319) * t52 / 0.5760D4 - (-0.180D3 * t5 * (t304 - t
     #59 * t301 + (t324 - t68 * t301) * t78) + 0.90D2 * t4 * (-t59 * t30
     #4 + t84 * t301 / 0.2D1 + (-t68 * t304 + t90 * t301 / 0.2D1) * t78)
     # + t31 * t344) * t103 * t52 / 0.5760D4 + (0.90D2 * t4 * (-t304 + t
     #111 * t301 - (t324 - t117 * t301) * t78) - 0.180D3 * t5 * t357) * 
     #t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t136 * t301 - t304) + 0
     #.90D2 * t4 * (-t142 * t301 / 0.2D1 + t136 * t304) - t374) * t129 *
     # t52 / 0.2880D4 - (-0.15D2 * t158 * t304 + 0.15D2 / 0.4D1 * t161 *
     # t301 + (-t154 * t301 + t304) * t48 + t301 * t173 + (-t154 * t304 
     #+ t155 * t301 / 0.2D1) * t30 - 0.180D3 * (t155 * t304 / 0.2D1 - t1
     #58 * t301 / 0.6D1) * lh) * t4 / 0.11520D5 + ((0.90D2 * t4 * t304 -
     # 0.180D3 * t5 * t301) * t209 + (-0.180D3 * t5 * t304 + t374) * t21
     #8 + 0.90D2 * t4 * t301 * t226 + (t31 * t304 + t319) * t234) * t103
     # / 0.11520D5 - (-0.180D3 * t5 * (t244 * t301 / 0.2D1 - t242 * t304
     #) + t31 * (-t242 * t301 + t304) + 0.90D2 * t4 * (-t254 * t301 / 0.
     #6D1 + t244 * t304 / 0.2D1) + t319) * t129 / 0.5760D4 + (-0.180D3 *
     # t5 * (-t304 + t267 * t301 - (t324 - t272 * t301) * t78) + 0.90D2 
     #* t4 * (t267 * t304 - (-t272 * t304 + t285 * t301 / 0.2D1) * t78 -
     # t280 * t301 / 0.2D1) + t31 * t357) * t103 * t129 / 0.5760D4
      t464 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t463)
      t466 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t468 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t484 = t49 * t468
      t489 = z * t466
      t509 = t468 + z * t468 * t78
      t522 = -t509
      t539 = t31 * t468
      t628 = (-0.180D3 * t5 * (t16 * t466 - t19 * t468 / 0.2D1) + t31 * 
     #(t16 * t468 - t466) + 0.90D2 * t4 * (-t19 * t466 / 0.2D1 + t36 * t
     #468 / 0.6D1) - t484) * t52 / 0.5760D4 - (-0.180D3 * t5 * (t466 - t
     #59 * t468 + (t489 - t68 * t468) * t78) + 0.90D2 * t4 * (-t59 * t46
     #6 + t84 * t468 / 0.2D1 + (-t68 * t466 + t90 * t468 / 0.2D1) * t78)
     # + t31 * t509) * t103 * t52 / 0.5760D4 + (0.90D2 * t4 * (-t466 - (
     #t489 - t117 * t468) * t78 + t111 * t468) - 0.180D3 * t5 * t522) * 
     #t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (-t466 + t136 * t468) + 
     #0.90D2 * t4 * (-t142 * t468 / 0.2D1 + t136 * t466) - t539) * t129 
     #* t52 / 0.2880D4 - (-0.15D2 * t158 * t466 + 0.15D2 / 0.4D1 * t161 
     #* t468 + (-t154 * t468 + t466) * t48 + t468 * t173 + (-t154 * t466
     # + t155 * t468 / 0.2D1) * t30 - 0.180D3 * (t155 * t466 / 0.2D1 - t
     #158 * t468 / 0.6D1) * lh) * t4 / 0.11520D5 + ((0.90D2 * t4 * t466 
     #- 0.180D3 * t5 * t468) * t209 + (-0.180D3 * t5 * t466 + t539) * t2
     #18 + 0.90D2 * t4 * t468 * t226 + (t31 * t466 + t484) * t234) * t10
     #3 / 0.11520D5 - (-0.180D3 * t5 * (-t242 * t466 + t244 * t468 / 0.2
     #D1) + t31 * (-t242 * t468 + t466) + 0.90D2 * t4 * (t244 * t466 / 0
     #.2D1 - t254 * t468 / 0.6D1) + t484) * t129 / 0.5760D4 + (-0.180D3 
     #* t5 * (-t466 + t267 * t468 - (t489 - t272 * t468) * t78) + 0.90D2
     # * t4 * (t267 * t466 - t280 * t468 / 0.2D1 - (-t272 * t466 + t285 
     #* t468 / 0.2D1) * t78) + t31 * t522) * t103 * t129 / 0.5760D4
      t629 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t628)
      t631 = t2 * x1
      t632 = -0.1D1 + x1
      t633 = x1 * z
      t634 = 0.1D1 - x1 + t633
      t635 = 0.1D1 / t634
      t637 = t2 * t632 * t635
      t638 = t1 ** 2
      t639 = s * t638
      t641 = t632 * x1 * t635
      t642 = t639 * t641
      t643 = t13 * t635
      t644 = t632 ** 2
      t645 = t643 * t644
      t648 = log(0.4D1 * t11 * t645)
      t649 = t648 ** 2
      t650 = -t632
      t651 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4)
      t654 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4)
      t664 = t649 * t648
      t674 = z * t654
      t678 = t10 * t644 * t635 * t63
      t681 = log(-0.4D1 * t55 * t13 * t678)
      t682 = t681 * z
      t686 = x1 * x3
      t687 = 0.2D1 * t686
      t688 = x1 * t12
      t689 = x3 * t12
      t690 = t689 * x1
      t692 = 0.2D1 * t55 * z
      t693 = t55 * t12
      t694 = t686 * z
      t695 = 0.3D1 * t694
      t696 = x3 * t634
      t698 = Sqrt(-t696 * t62)
      t702 = -t55 - z + t687 - x3 + t633 - t688 + t690 + t692 - t693 - t
     #695 + 0.2D1 * t71 * t698 * z
      t703 = 0.1D1 / t702
      t708 = log(0.4D1 * t55 * t10 * t645)
      t714 = t708 ** 2
      t718 = t681 ** 2
      t719 = t718 * z
      t729 = t634 * t703
      t731 = -t651 - z * t651 * t729
      t737 = t635 * t644
      t741 = log(0.4D1 * t113 * t56 * t737)
      t744 = t108 * t7 * t13
      t747 = log(-0.4D1 * t744 * t678)
      t748 = t747 * z
      t763 = t133 * t10
      t766 = log(0.4D1 * t763 * t645)
      t772 = t766 ** 2
      t783 = (-0.180D3 * t5 * (t649 * t651 / 0.2D1 - t648 * t654) + t31 
     #* (t654 - t648 * t651) + 0.90D2 * t4 * (t649 * t654 / 0.2D1 - t664
     # * t651 / 0.6D1) + t49 * t651) * t52 / 0.5760D4 - (-0.180D3 * t5 *
     # (-(t674 - t682 * t651) * t634 * t703 - t654 + t708 * t651) + 0.90
     #D2 * t4 * (t708 * t654 - t714 * t651 / 0.2D1 - (-t682 * t654 + t71
     #9 * t651 / 0.2D1) * t634 * t703) + t31 * t731) * t103 * t52 / 0.57
     #60D4 + (0.90D2 * t4 * (-t741 * t651 + t654 + (t674 - t748 * t651) 
     #* t634 * t703) + 0.180D3 * t5 * t731) * t103 * t130 / 0.2880D4 + (
     #-0.180D3 * t5 * (t654 - t766 * t651) + 0.90D2 * t4 * (-t766 * t654
     # + t772 * t651 / 0.2D1) + t31 * t651) * t129 * t52 / 0.2880D4
      t784 = FJET(XB1, XB2, s, 0.0D0, t631, -t637, 0.0D0, -t642, t783)
      t786 = x2 * s
      t787 = t786 * t1
      t788 = -0.1D1 + x2
      t789 = t788 * s
      t790 = t789 * t1
      t791 = t56 * t788
      t794 = log(-0.4D1 * t113 * t791)
      t795 = -t788
      t796 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4)
      t798 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4)
      t810 = log(-0.4D1 * t133 * t791)
      t815 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4)
      t817 = t810 ** 2
      t823 = t31 * t796
      t832 = log(-0.4D1 * t107 * t10 * t13 * t788)
      t833 = t832 ** 2
      t846 = t833 * t832
      t858 = log(-0.4D1 * t108 * t791)
      t864 = t858 ** 2
      t874 = (0.90D2 * t4 * (-t794 * t796 + t798) - 0.180D3 * t5 * t796)
     # * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t798 - t810 * t796) 
     #+ 0.90D2 * t4 * (t815 - t810 * t798 + t817 * t796 / 0.2D1) + t823)
     # * t129 * t52 / 0.2880D4 - (-0.180D3 * t5 * (-t815 - t833 * t796 /
     # 0.2D1 + t832 * t798) + t31 * (-t798 + t832 * t796) + 0.90D2 * t4 
     #* (-t833 * t798 / 0.2D1 + t832 * t815 + t846 * t796 / 0.6D1) - t49
     # * t796) * t129 / 0.5760D4 + (-0.180D3 * t5 * (-t858 * t796 + t798
     #) + 0.90D2 * t4 * (-t858 * t798 + t864 * t796 / 0.2D1 + t815) + t8
     #23) * t103 * t129 / 0.5760D4
      t875 = FJET(XB1, XB2, s, 0.0D0, t787, 0.0D0, -t790, 0.0D0, t874)
      t877 = x2 * x3
      t880 = Sqrt(x3 * t788 * t62)
      t881 = t71 * t880
      t883 = 0.2D1 * t881 * x2
      t885 = 0.1D1 - x3 + t877
      t886 = 0.1D1 / t885
      t888 = t2 * (0.1D1 - x3 - x2 + t877 + t108 + t883) * t886
      t893 = t2 * x2 * (-0.1D1 + t877 + 0.2D1 * t881) * t886
      t894 = x2 * z
      t895 = t894 - z - x2
      t896 = t62 * t886
      t897 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t898 = t895 * t897
      t900 = t885 ** 2
      t901 = 0.1D1 / t900
      t903 = t10 * t788 * t62 * t901
      t906 = log(0.4D1 * t744 * t903)
      t907 = t906 * t895
      t908 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t912 = t108 * z
      t913 = t877 * z
      t919 = 0.1D1 / (t912 - t894 - t913 + z - t108 + x3 + x2 - t883 - 0
     #.2D1 * t881 * z + 0.2D1 * t881 * t894)
      t923 = t895 * t908 * t919
      t933 = log(0.4D1 * t108 * t13 * t903)
      t934 = t933 * t895
      t941 = t933 ** 2
      t942 = t941 * t895
      t954 = (0.90D2 * t4 * (t898 - t907 * t908) * t919 - 0.180D3 * t5 *
     # t923) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t898 - t934 * 
     #t908) * t919 + 0.90D2 * t4 * (-t934 * t897 + t942 * t908 / 0.2D1) 
     #* t919 + t31 * t923) * t103 * t129 / 0.5760D4
      t955 = FJET(XB1, XB2, s, 0.0D0, t888, 0.0D0, -t893, 0.0D0, t954)
      t957 = t1 * t632
      t959 = t789 * t957 * t635
      t960 = t786 * t957
      t962 = t639 * t788 * t641
      t965 = t643 * t644 * t788
      t968 = log(-0.4D1 * t108 * t11 * t965)
      t969 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t971 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t982 = log(-0.4D1 * t763 * t965)
      t987 = t982 ** 2
      t999 = (0.90D2 * t4 * (t968 * t969 - t971) + 0.180D3 * t5 * t969) 
     #* t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (-t971 + t982 * t969) 
     #+ 0.90D2 * t4 * (-t987 * t969 / 0.2D1 + t982 * t971) - t31 * t969)
     # * t129 * t52 / 0.2880D4
      t1000 = FJET(XB1, XB2, s, 0.0D0, t959, t631, -t960, t962, t999)
      t1002 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4
     #)
      t1004 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4
     #)
      t1024 = t31 * t1002
      t1063 = (0.90D2 * t4 * (-t794 * t1002 + t1004) - 0.180D3 * t5 * t1
     #002) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (-t810 * t1002 + 
     #t1004) + 0.90D2 * t4 * (-t810 * t1004 + t817 * t1002 / 0.2D1) + t1
     #024) * t129 * t52 / 0.2880D4 - (-0.180D3 * t5 * (-t833 * t1002 / 0
     #.2D1 + t832 * t1004) + t31 * (-t1004 + t832 * t1002) + 0.90D2 * t4
     # * (t846 * t1002 / 0.6D1 - t833 * t1004 / 0.2D1) - t49 * t1002) * 
     #t129 / 0.5760D4 + (-0.180D3 * t5 * (t1004 - t858 * t1002) + 0.90D2
     # * t4 * (-t858 * t1004 + t864 * t1002 / 0.2D1) + t1024) * t103 * t
     #129 / 0.5760D4
      t1064 = FJET(XB1, XB2, s, 0.0D0, -t790, 0.0D0, t787, 0.0D0, t1063)
      t1066 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1069 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1089 = z * t1069
      t1111 = -z * t1066 * t729 - t1066
      t1147 = (-0.180D3 * t5 * (t649 * t1066 / 0.2D1 - t648 * t1069) + t
     #31 * (-t648 * t1066 + t1069) + 0.90D2 * t4 * (t649 * t1069 / 0.2D1
     # - t664 * t1066 / 0.6D1) + t49 * t1066) * t52 / 0.5760D4 - (-0.180
     #D3 * t5 * (-t1069 + t708 * t1066 - (t1089 - t682 * t1066) * t634 *
     # t703) + 0.90D2 * t4 * (t708 * t1069 - t714 * t1066 / 0.2D1 - (-t6
     #82 * t1069 + t719 * t1066 / 0.2D1) * t634 * t703) + t31 * t1111) *
     # t103 * t52 / 0.5760D4 + (0.90D2 * t4 * (t1069 - t741 * t1066 + (t
     #1089 - t748 * t1066) * t634 * t703) + 0.180D3 * t5 * t1111) * t103
     # * t130 / 0.2880D4 + (-0.180D3 * t5 * (t1069 - t766 * t1066) + 0.9
     #0D2 * t4 * (-t766 * t1069 + t772 * t1066 / 0.2D1) + t31 * t1066) *
     # t129 * t52 / 0.2880D4
      t1148 = FJET(XB1, XB2, s, 0.0D0, -t637, t631, 0.0D0, -t642, t1147)
      t1150 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1151 = t895 * t1150
      t1152 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1159 = t895 * t1152 * t919
      t1171 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1185 = (0.90D2 * t4 * (t1151 - t907 * t1152) * t919 - 0.180D3 * t
     #5 * t1159) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t1151 - t9
     #34 * t1152) * t919 + 0.90D2 * t4 * (t895 * t1171 - t934 * t1150 + 
     #t942 * t1152 / 0.2D1) * t919 + t31 * t1159) * t103 * t129 / 0.5760
     #D4
      t1186 = FJET(XB1, XB2, s, 0.0D0, -t893, 0.0D0, t888, 0.0D0, t1185)
      t1188 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, 
     #x4)
      t1189 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, 
     #x4)
      t1192 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, 
     #x4)
      t1208 = t49 * t1189
      t1213 = z * t1192
      t1220 = z * t1188
      t1234 = t1189 + z * t1189 * t78
      t1247 = -t1234
      t1264 = t31 * t1189
      t1361 = (-0.180D3 * t5 * (-t1188 - t19 * t1189 / 0.2D1 + t16 * t11
     #92) + t31 * (t16 * t1189 - t1192) + 0.90D2 * t4 * (t16 * t1188 + t
     #36 * t1189 / 0.6D1 - t19 * t1192 / 0.2D1) - t1208) * t52 / 0.5760D
     #4 - (-0.180D3 * t5 * (t1192 - t59 * t1189 + (t1213 - t68 * t1189) 
     #* t78) + 0.90D2 * t4 * ((t1220 - t68 * t1192 + t90 * t1189 / 0.2D1
     #) * t78 - t59 * t1192 + t1188 + t84 * t1189 / 0.2D1) + t31 * t1234
     #) * t103 * t52 / 0.5760D4 + (0.90D2 * t4 * (-t1192 - (t1213 - t117
     # * t1189) * t78 + t111 * t1189) - 0.180D3 * t5 * t1247) * t103 * t
     #130 / 0.2880D4 + (-0.180D3 * t5 * (-t1192 + t136 * t1189) + 0.90D2
     # * t4 * (t136 * t1192 - t142 * t1189 / 0.2D1 - t1188) - t1264) * t
     #129 * t52 / 0.2880D4 - (0.45D2 * t155 * t1188 - 0.15D2 * t158 * t1
     #192 + 0.15D2 / 0.4D1 * t161 * t1189 + (-t154 * t1189 + t1192) * t4
     #8 + t1189 * t173 + (t1188 - t154 * t1192 + t155 * t1189 / 0.2D1) *
     # t30 - 0.180D3 * (-t154 * t1188 + t155 * t1192 / 0.2D1 - t158 * t1
     #189 / 0.6D1) * lh) * t4 / 0.11520D5 + ((0.90D2 * t4 * t1192 - 0.18
     #0D3 * t5 * t1189) * t209 + (0.90D2 * t4 * t1188 - 0.180D3 * t5 * t
     #1192 + t1264) * t218 + 0.90D2 * t4 * t1189 * t226 + (-0.180D3 * t5
     # * t1188 + t31 * t1192 + t1208) * t234) * t103 / 0.11520D5 - (-0.1
     #80D3 * t5 * (t1188 + t244 * t1189 / 0.2D1 - t242 * t1192) + t31 * 
     #(-t242 * t1189 + t1192) + 0.90D2 * t4 * (t244 * t1192 / 0.2D1 - t2
     #42 * t1188 - t254 * t1189 / 0.6D1) + t1208) * t129 / 0.5760D4 + (-
     #0.180D3 * t5 * (t267 * t1189 - t1192 - (t1213 - t272 * t1189) * t7
     #8) + 0.90D2 * t4 * (-t280 * t1189 / 0.2D1 + t267 * t1192 - (t1220 
     #- t272 * t1192 + t285 * t1189 / 0.2D1) * t78 - t1188) + t31 * t124
     #7) * t103 * t129 / 0.5760D4
      t1362 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1361)
      t1364 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1367 = gbgbH54J3(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1368 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1389 = z * t1368
      t1412 = -z * t1364 * t729 - t1364
      t1448 = (-0.180D3 * t5 * (t649 * t1364 / 0.2D1 + t1367 - t648 * t1
     #368) + t31 * (t1368 - t648 * t1364) + 0.90D2 * t4 * (-t664 * t1364
     # / 0.6D1 - t648 * t1367 + t649 * t1368 / 0.2D1) + t49 * t1364) * t
     #52 / 0.5760D4 - (-0.180D3 * t5 * (t708 * t1364 - t1368 - (t1389 - 
     #t682 * t1364) * t634 * t703) + 0.90D2 * t4 * (-t1367 + t708 * t136
     #8 - t714 * t1364 / 0.2D1 - (z * t1367 - t682 * t1368 + t719 * t136
     #4 / 0.2D1) * t634 * t703) + t31 * t1412) * t103 * t52 / 0.5760D4 +
     # (0.90D2 * t4 * (t1368 - t741 * t1364 + (t1389 - t748 * t1364) * t
     #634 * t703) + 0.180D3 * t5 * t1412) * t103 * t130 / 0.2880D4 + (-0
     #.180D3 * t5 * (t1368 - t766 * t1364) + 0.90D2 * t4 * (t772 * t1364
     # / 0.2D1 + t1367 - t766 * t1368) + t31 * t1364) * t129 * t52 / 0.2
     #880D4
      t1449 = FJET(XB1, XB2, s, t631, 0.0D0, 0.0D0, -t637, -t642, t1448)
      t1451 = t299 * t298 + t464 * t463 + t629 * t628 + t784 * t783 + t8
     #75 * t874 + t955 * t954 + t1000 * t999 + t1064 * t1063 + t1148 * t
     #1147 + t1186 * t1185 + t1362 * t1361 + t1449 * t1448
      t1452 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1454 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1467 = gbgbH54J3(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1479 = (0.90D2 * t4 * (t968 * t1452 - t1454) + 0.180D3 * t5 * t14
     #52) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t982 * t1452 - t1
     #454) + 0.90D2 * t4 * (-t1467 - t987 * t1452 / 0.2D1 + t982 * t1454
     #) - t31 * t1452) * t129 * t52 / 0.2880D4
      t1480 = FJET(XB1, XB2, s, t631, -t960, 0.0D0, t959, t962, t1479)
      t1482 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4
     #)
      t1484 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4
     #)
      t1504 = t31 * t1482
      t1543 = (0.90D2 * t4 * (-t794 * t1482 + t1484) - 0.180D3 * t5 * t1
     #482) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (-t810 * t1482 + 
     #t1484) + 0.90D2 * t4 * (t817 * t1482 / 0.2D1 - t810 * t1484) + t15
     #04) * t129 * t52 / 0.2880D4 - (-0.180D3 * t5 * (-t833 * t1482 / 0.
     #2D1 + t832 * t1484) + t31 * (t832 * t1482 - t1484) + 0.90D2 * t4 *
     # (-t833 * t1484 / 0.2D1 + t846 * t1482 / 0.6D1) - t49 * t1482) * t
     #129 / 0.5760D4 + (-0.180D3 * t5 * (t1484 - t858 * t1482) + 0.90D2 
     #* t4 * (t864 * t1482 / 0.2D1 - t858 * t1484) + t1504) * t103 * t12
     #9 / 0.5760D4
      t1544 = FJET(XB1, XB2, s, t787, 0.0D0, -t790, 0.0D0, 0.0D0, t1543)
      t1546 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1547 = t895 * t1546
      t1548 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1555 = t895 * t1548 * t919
      t1567 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1581 = (0.90D2 * t4 * (t1547 - t907 * t1548) * t919 - 0.180D3 * t
     #5 * t1555) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t1547 - t9
     #34 * t1548) * t919 + 0.90D2 * t4 * (t895 * t1567 - t934 * t1546 + 
     #t942 * t1548 / 0.2D1) * t919 + t31 * t1555) * t103 * t129 / 0.5760
     #D4
      t1582 = FJET(XB1, XB2, s, t888, 0.0D0, -t893, 0.0D0, 0.0D0, t1581)
      t1584 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1585 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1602 = gbgbH52J3(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1611 = (0.90D2 * t4 * (-t1584 + t968 * t1585) + 0.180D3 * t5 * t1
     #585) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (-t1584 + t982 * 
     #t1585) + 0.90D2 * t4 * (t982 * t1584 - t987 * t1585 / 0.2D1 - t160
     #2) - t31 * t1585) * t129 * t52 / 0.2880D4
      t1612 = FJET(XB1, XB2, s, t959, 0.0D0, -t960, t631, t962, t1611)
      t1615 = t631 * t877 * t886
      t1616 = t2 * t632
      t1617 = t108 * x1
      t1618 = t108 * t633
      t1619 = t788 * t62
      t1621 = Sqrt(t696 * t1619)
      t1622 = t71 * t1621
      t1624 = 0.2D1 * t1622 * x2
      t1628 = t1616 * (t108 - x2 + t877 + 0.1D1 - x3 - t1617 + t1618 + t
     #1624) * t635 * t886
      t1632 = t62 * s * t1 * x1 * t886
      t1638 = t1616 * x2 * (-0.1D1 + t877 + x1 - t686 - t633 + t694 + 0.
     #2D1 * t1622) * t635 * t886
      t1639 = x2 * x1
      t1640 = t1639 * z
      t1641 = x2 - t1639 + z - t894 + t1640
      t1642 = t634 * t1641
      t1643 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1649 = log(0.4D1 * t109 * t737 * t1619 * t901)
      t1650 = t1649 * t634
      t1651 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1652 = t1641 * t1651
      t1658 = -x3 - z + t1618 + t108 + t894 - t912 + t913 + t633 - t55 +
     # t687 - t688 + t690 + t692 - t693 - t695 + 0.2D1 * t1622 * t1640 -
     # t1617
      t1663 = x2 * t7
      t1681 = t1624 - 0.3D1 * t1640 + t55 * x2 - t877 * x1 + t1639 * t12
     # + 0.2D1 * t1663 * z - t1663 * t12 + 0.2D1 * t1622 * z - 0.2D1 * t
     #1622 * t894 - 0.2D1 * t1622 * t1639 - t689 * t1639 - 0.2D1 * t55 *
     # t894 + t55 * t12 * x2 + 0.2D1 * t877 * t633 + 0.2D1 * t1639 - t16
     #63 - x2
      t1683 = 0.1D1 / (t1658 + t1681)
      t1686 = t5 * t634
      t1690 = -0.90D2 * t4 * (t1642 * t1643 - t1650 * t1652) * t1683 + 0
     #.180D3 * t1686 * t1652 * t1683
      t1694 = FJET(XB1, XB2, s, t1615, -t1628, -t1632, t1638, t962, t169
     #0 * t103 * t130 / 0.2880D4)
      t1697 = t103 * t129 * t52
      t1700 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1702 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1703 = t1641 * t1702
      t1712 = -0.90D2 * t4 * (t1642 * t1700 - t1650 * t1703) * t1683 + 0
     #.180D3 * t1686 * t1703 * t1683
      t1716 = FJET(XB1, XB2, s, t1638, -t1632, -t1628, t1615, t962, t171
     #2 * t103 * t130 / 0.2880D4)
      t1720 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4
     #)
      t1721 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4
     #)
      t1736 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, t795, 0.10D1, x4
     #)
      t1743 = t31 * t1721
      t1783 = (0.90D2 * t4 * (t1720 - t794 * t1721) - 0.180D3 * t5 * t17
     #21) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t1720 - t810 * t1
     #721) + 0.90D2 * t4 * (t1736 - t810 * t1720 + t817 * t1721 / 0.2D1)
     # + t1743) * t129 * t52 / 0.2880D4 - (-0.180D3 * t5 * (-t833 * t172
     #1 / 0.2D1 + t832 * t1720 - t1736) + t31 * (t832 * t1721 - t1720) +
     # 0.90D2 * t4 * (-t833 * t1720 / 0.2D1 + t832 * t1736 + t846 * t172
     #1 / 0.6D1) - t49 * t1721) * t129 / 0.5760D4 + (-0.180D3 * t5 * (t1
     #720 - t858 * t1721) + 0.90D2 * t4 * (t864 * t1721 / 0.2D1 + t1736 
     #- t858 * t1720) + t1743) * t103 * t129 / 0.5760D4
      t1784 = FJET(XB1, XB2, s, -t790, 0.0D0, t787, 0.0D0, 0.0D0, t1783)
      t1786 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1789 = gbgbH52J3(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1790 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t650, 0.10D1, 0.10D1, x4
     #)
      t1811 = z * t1790
      t1834 = -z * t1786 * t729 - t1786
      t1870 = (-0.180D3 * t5 * (t649 * t1786 / 0.2D1 + t1789 - t648 * t1
     #790) + t31 * (t1790 - t648 * t1786) + 0.90D2 * t4 * (-t664 * t1786
     # / 0.6D1 - t648 * t1789 + t649 * t1790 / 0.2D1) + t49 * t1786) * t
     #52 / 0.5760D4 - (-0.180D3 * t5 * (t708 * t1786 - t1790 - (t1811 - 
     #t682 * t1786) * t634 * t703) + 0.90D2 * t4 * (-t1789 + t708 * t179
     #0 - t714 * t1786 / 0.2D1 - (z * t1789 - t682 * t1790 + t719 * t178
     #6 / 0.2D1) * t634 * t703) + t31 * t1834) * t103 * t52 / 0.5760D4 +
     # (0.90D2 * t4 * ((t1811 - t748 * t1786) * t634 * t703 + t1790 - t7
     #41 * t1786) + 0.180D3 * t5 * t1834) * t103 * t130 / 0.2880D4 + (-0
     #.180D3 * t5 * (-t766 * t1786 + t1790) + 0.90D2 * t4 * (t772 * t178
     #6 / 0.2D1 + t1789 - t766 * t1790) + t31 * t1786) * t129 * t52 / 0.
     #2880D4
      t1871 = FJET(XB1, XB2, s, -t637, 0.0D0, 0.0D0, t631, -t642, t1870)
      t1873 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1875 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t650, t795, 0.10D1, x4)
      t1899 = (0.90D2 * t4 * (t968 * t1873 - t1875) + 0.180D3 * t5 * t18
     #73) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t982 * t1873 - t1
     #875) + 0.90D2 * t4 * (-t987 * t1873 / 0.2D1 + t982 * t1875) - t31 
     #* t1873) * t129 * t52 / 0.2880D4
      t1900 = FJET(XB1, XB2, s, -t960, t631, t959, 0.0D0, t962, t1899)
      t1902 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1903 = t895 * t1902
      t1904 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t795, -t896, x4)
      t1911 = t895 * t1904 * t919
      t1935 = (0.90D2 * t4 * (t1903 - t907 * t1904) * t919 - 0.180D3 * t
     #5 * t1911) * t103 * t130 / 0.2880D4 + (-0.180D3 * t5 * (t1903 - t9
     #34 * t1904) * t919 + 0.90D2 * t4 * (-t934 * t1902 + t942 * t1904 /
     # 0.2D1) * t919 + t31 * t1911) * t103 * t129 / 0.5760D4
      t1936 = FJET(XB1, XB2, s, -t893, 0.0D0, t888, 0.0D0, 0.0D0, t1935)
      t1938 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1940 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1941 = t1641 * t1940
      t1950 = -0.90D2 * t4 * (t1642 * t1938 - t1650 * t1941) * t1683 + 0
     #.180D3 * t1686 * t1941 * t1683
      t1954 = FJET(XB1, XB2, s, -t1632, t1638, t1615, -t1628, t962, t195
     #0 * t103 * t130 / 0.2880D4)
      t1958 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1960 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t650, t795, -t896, x4)
      t1961 = t1641 * t1960
      t1970 = -0.90D2 * t4 * (t1642 * t1958 - t1650 * t1961) * t1683 + 0
     #.180D3 * t1686 * t1961 * t1683
      t1974 = FJET(XB1, XB2, s, -t1628, t1615, t1638, -t1632, t962, t197
     #0 * t103 * t130 / 0.2880D4)
      t1978 = t1480 * t1479 + t1544 * t1543 + t1582 * t1581 + t1612 * t1
     #611 + t1694 * t1690 * t1697 / 0.2880D4 + t1716 * t1712 * t1697 / 0
     #.2880D4 + t1784 * t1783 + t1871 * t1870 + t1900 * t1899 + t1936 * 
     #t1935 + t1954 * t1950 * t1697 / 0.2880D4 + t1974 * t1970 * t1697 /
     # 0.2880D4
      gbgbH5n1e1 = t1451 + t1978

      end function



      doubleprecision function gbgbH5n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = x1 ** 2
      t6 = t5 * x3
      t7 = z ** 2
      t8 = 0.1D1 / t7
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t15 = log(0.4D1 * t6 * t12)
      t16 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t18 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t19 = z * t18
      t20 = -0.1D1 + x3
      t21 = 0.1D1 / t20
      t22 = t12 * t21
      t25 = log(-0.4D1 * t6 * t22)
      t26 = t25 * z
      t29 = cos(t9)
      t31 = Sqrt(-x3 * t20)
      t36 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t31 * z)
      t41 = t4 * lh
      t44 = t16 + z * t16 * t36
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t53 = -t44
      t55 = 0.1D1 / x2
      t57 = t48 * t55 * t50
      t60 = x2 ** 2
      t61 = t60 * t5
      t64 = log(0.4D1 * t61 * t12)
      t70 = 0.180D3 * t41 * t16
      t75 = t5 * t11
      t78 = log(0.4D1 * t75 * t8)
      t83 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t85 = t78 ** 2
      t91 = lh ** 2
      t93 = 0.3141592653589793D1 ** 2
      t95 = 0.180D3 * t91 - 0.30D2 * t93
      t96 = t4 * t95
      t97 = t96 * t16
      t104 = x3 * t11
      t108 = log(-0.4D1 * t104 * t8 * t21)
      t113 = log(0.4D1 * t104 * t8)
      t114 = t108 * z * t36 + t113
      t117 = t113 ** 2
      t118 = t108 ** 2
      t122 = -t117 / 0.2D1 - t118 * z * t36 / 0.2D1
      t131 = -z * t36 - 0.1D1
      t136 = t60 * x3
      t139 = log(0.4D1 * t136 * t12)
      t143 = log(-0.4D1 * t136 * t22)
      t144 = t143 * z
      t160 = log(0.4D1 * t60 * t8 * t11)
      t166 = t160 ** 2
      t176 = log(0.4D1 * t12)
      t181 = t176 ** 2
      t191 = -0.2884936567583026D3 - 0.120D3 * t91 * lh + 0.60D2 * lh * 
     #t93
      t197 = t181 * t176
      t203 = -(0.90D2 * t4 * (-t15 * t16 + t18 + (t19 - t26 * t16) * t36
     #) - 0.180D3 * t41 * t44) * t48 * t50 / 0.5760D4 + t4 * t53 * t57 /
     # 0.32D2 + (0.90D2 * t4 * (t64 * t16 - t18) + t70) * t55 * t50 / 0.
     #2880D4 + (-0.180D3 * t41 * (-t18 + t78 * t16) + 0.90D2 * t4 * (-t8
     #3 + t78 * t18 - t85 * t16 / 0.2D1) - t97) * t50 / 0.5760D4 + ((0.9
     #0D2 * t4 * t18 - t70) * t114 + 0.90D2 * t4 * t16 * t122 + (0.90D2 
     #* t4 * t83 - 0.180D3 * t41 * t18 + t97) * t131) * t48 / 0.11520D5 
     #+ (0.90D2 * t4 * (t139 * t16 - t18 - (t19 - t144 * t16) * t36) - 0
     #.180D3 * t41 * t53) * t48 * t55 / 0.5760D4 - (-0.180D3 * t41 * (t1
     #8 - t160 * t16) + 0.90D2 * t4 * (t83 - t160 * t18 + t166 * t16 / 0
     #.2D1) + t97) * t55 / 0.5760D4 - ((t18 - t176 * t16) * t95 - 0.180D
     #3 * (t83 - t176 * t18 + t181 * t16 / 0.2D1) * lh + t16 * t191 - 0.
     #90D2 * t176 * t83 + 0.45D2 * t181 * t18 - 0.15D2 * t197 * t16) * t
     #4 / 0.11520D5
      t204 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t203)
      t206 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t207 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t209 = z * t206
      t218 = t207 + z * t207 * t36
      t225 = -t218
      t234 = 0.180D3 * t41 * t207
      t249 = t96 * t207
      t310 = -(0.90D2 * t4 * (t206 - t15 * t207 + (t209 - t26 * t207) * 
     #t36) - 0.180D3 * t41 * t218) * t48 * t50 / 0.5760D4 + t4 * t225 * 
     #t57 / 0.32D2 + (0.90D2 * t4 * (t64 * t207 - t206) + t234) * t55 * 
     #t50 / 0.2880D4 + (-0.180D3 * t41 * (t78 * t207 - t206) + 0.90D2 * 
     #t4 * (-t85 * t207 / 0.2D1 + t78 * t206) - t249) * t50 / 0.5760D4 +
     # ((0.90D2 * t4 * t206 - t234) * t114 + 0.90D2 * t4 * t207 * t122 +
     # (-0.180D3 * t41 * t206 + t249) * t131) * t48 / 0.11520D5 + (0.90D
     #2 * t4 * (-t206 + t139 * t207 - (t209 - t144 * t207) * t36) - 0.18
     #0D3 * t41 * t225) * t48 * t55 / 0.5760D4 - (-0.180D3 * t41 * (-t16
     #0 * t207 + t206) + 0.90D2 * t4 * (t166 * t207 / 0.2D1 - t160 * t20
     #6) + t249) * t55 / 0.5760D4 - ((-t176 * t207 + t206) * t95 - 0.180
     #D3 * (-t176 * t206 + t181 * t207 / 0.2D1) * lh + 0.45D2 * t181 * t
     #206 + t207 * t191 - 0.15D2 * t197 * t207) * t4 / 0.11520D5
      t311 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t310)
      t313 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t314 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t316 = z * t313
      t325 = t314 + z * t314 * t36
      t332 = -t325
      t341 = 0.180D3 * t41 * t314
      t356 = t96 * t314
      t417 = -(0.90D2 * t4 * (t313 - t15 * t314 + (t316 - t26 * t314) * 
     #t36) - 0.180D3 * t41 * t325) * t48 * t50 / 0.5760D4 + t4 * t332 * 
     #t57 / 0.32D2 + (0.90D2 * t4 * (-t313 + t64 * t314) + t341) * t55 *
     # t50 / 0.2880D4 + (-0.180D3 * t41 * (t78 * t314 - t313) + 0.90D2 *
     # t4 * (t78 * t313 - t85 * t314 / 0.2D1) - t356) * t50 / 0.5760D4 +
     # ((0.90D2 * t4 * t313 - t341) * t114 + 0.90D2 * t4 * t314 * t122 +
     # (-0.180D3 * t41 * t313 + t356) * t131) * t48 / 0.11520D5 + (0.90D
     #2 * t4 * (-t313 + t139 * t314 - (t316 - t144 * t314) * t36) - 0.18
     #0D3 * t41 * t332) * t48 * t55 / 0.5760D4 - (-0.180D3 * t41 * (-t16
     #0 * t314 + t313) + 0.90D2 * t4 * (-t160 * t313 + t166 * t314 / 0.2
     #D1) + t356) * t55 / 0.5760D4 - ((-t176 * t314 + t313) * t95 - 0.15
     #D2 * t197 * t314 - 0.180D3 * (-t176 * t313 + t181 * t314 / 0.2D1) 
     #* lh + 0.45D2 * t181 * t313 + t314 * t191) * t4 / 0.11520D5
      t418 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t417)
      t420 = t2 * x1
      t421 = -0.1D1 + x1
      t422 = x1 * z
      t423 = 0.1D1 - x1 + t422
      t424 = 0.1D1 / t423
      t426 = t2 * t421 * t424
      t427 = t1 ** 2
      t428 = s * t427
      t430 = t421 * x1 * t424
      t431 = t428 * t430
      t432 = -t421
      t433 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4)
      t436 = t421 ** 2
      t442 = log(-0.4D1 * t6 * t8 * t11 * t436 * t424 * t21)
      t443 = t442 * z
      t444 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4)
      t448 = x1 * x3
      t449 = 0.2D1 * t448
      t450 = x1 * t7
      t451 = t7 * x3
      t452 = t451 * x1
      t454 = 0.2D1 * z * t6
      t455 = t6 * t7
      t456 = t448 * z
      t457 = 0.3D1 * t456
      t458 = x3 * t423
      t460 = Sqrt(-t458 * t20)
      t464 = -t6 - z + t449 - x3 + t422 - t450 + t452 + t454 - t455 - t4
     #57 + 0.2D1 * t29 * t460 * z
      t465 = 0.1D1 / t464
      t468 = t8 * t424
      t469 = t468 * t436
      t472 = log(0.4D1 * t6 * t11 * t469)
      t478 = t423 * t465
      t480 = -t444 - z * t444 * t478
      t491 = t61 * t11
      t494 = log(0.4D1 * t491 * t469)
      t507 = log(0.4D1 * t75 * t469)
      t512 = t507 ** 2
      t523 = -(0.90D2 * t4 * (-(z * t433 - t443 * t444) * t423 * t465 - 
     #t433 + t472 * t444) - 0.180D3 * t41 * t480) * t48 * t50 / 0.5760D4
     # - t4 * t480 * t57 / 0.32D2 + (0.90D2 * t4 * (t433 - t494 * t444) 
     #- 0.180D3 * t41 * t444) * t55 * t50 / 0.2880D4 + (-0.180D3 * t41 *
     # (t433 - t507 * t444) + 0.90D2 * t4 * (t512 * t444 / 0.2D1 - t507 
     #* t433) + t96 * t444) * t50 / 0.5760D4
      t524 = FJET(XB1, XB2, s, 0.0D0, t420, -t426, 0.0D0, -t431, t523)
      t526 = x2 * s
      t527 = t526 * t1
      t528 = -0.1D1 + x2
      t529 = t528 * s
      t530 = t529 * t1
      t531 = t12 * t528
      t534 = log(-0.4D1 * t136 * t531)
      t535 = -t528
      t536 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4)
      t538 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4)
      t543 = 0.180D3 * t41 * t536
      t552 = log(-0.4D1 * t60 * t11 * t8 * t528)
      t557 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4)
      t558 = t552 ** 2
      t574 = log(-0.4D1 * t61 * t531)
      t583 = (0.90D2 * t4 * (-t534 * t536 + t538) - t543) * t48 * t55 / 
     #0.5760D4 - (-0.180D3 * t41 * (-t538 + t552 * t536) + 0.90D2 * t4 *
     # (-t557 - t558 * t536 / 0.2D1 + t552 * t538) - t96 * t536) * t55 /
     # 0.5760D4 + t4 * t536 * t57 / 0.32D2 + (0.90D2 * t4 * (t538 - t574
     # * t536) - t543) * t55 * t50 / 0.2880D4
      t584 = FJET(XB1, XB2, s, 0.0D0, t527, 0.0D0, -t530, 0.0D0, t583)
      t586 = x2 * x3
      t589 = Sqrt(x3 * t528 * t20)
      t590 = t29 * t589
      t592 = 0.2D1 * t590 * x2
      t594 = 0.1D1 - x3 + t586
      t595 = 0.1D1 / t594
      t597 = t2 * (0.1D1 - x3 - x2 + t586 + t136 + t592) * t595
      t602 = t2 * x2 * (-0.1D1 + t586 + 0.2D1 * t590) * t595
      t603 = x2 * z
      t604 = t603 - z - x2
      t605 = t4 * t604
      t606 = t20 * t595
      t607 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t609 = t136 * z
      t610 = t586 * z
      t616 = 0.1D1 / (t609 - t603 - t610 + z - t136 + x3 + x2 - t592 - 0
     #.2D1 * t590 * z + 0.2D1 * t590 * t603)
      t618 = t55 * t50
      t619 = t616 * t48 * t618
      t622 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t626 = t594 ** 2
      t632 = log(0.4D1 * t136 * t8 * t11 * t528 * t20 / t626)
      t633 = t632 * t604
      t647 = t605 * t607 * t619 / 0.32D2 + (0.90D2 * t4 * (t604 * t622 -
     # t633 * t607) * t616 - 0.180D3 * t41 * t604 * t607 * t616) * t48 *
     # t55 / 0.5760D4
      t648 = FJET(XB1, XB2, s, 0.0D0, t597, 0.0D0, -t602, 0.0D0, t647)
      t650 = t1 * t421
      t652 = t529 * t650 * t424
      t653 = t526 * t650
      t655 = t428 * t528 * t430
      t656 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t660 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t665 = log(-0.4D1 * t491 * t468 * t436 * t528)
      t676 = -t4 * t656 * t57 / 0.32D2 + (0.90D2 * t4 * (-t660 + t665 * 
     #t656) + 0.180D3 * t41 * t656) * t55 * t50 / 0.2880D4
      t677 = FJET(XB1, XB2, s, 0.0D0, t652, t420, -t653, t655, t676)
      t679 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4)
      t684 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4)
      t689 = 0.180D3 * t41 * t679
      t716 = t4 * t679 * t57 / 0.32D2 + (0.90D2 * t4 * (-t574 * t679 + t
     #684) - t689) * t55 * t50 / 0.2880D4 + (0.90D2 * t4 * (t684 - t534 
     #* t679) - t689) * t48 * t55 / 0.5760D4 - (-0.180D3 * t41 * (-t684 
     #+ t552 * t679) + 0.90D2 * t4 * (-t558 * t679 / 0.2D1 + t552 * t684
     #) - t96 * t679) * t55 / 0.5760D4
      t717 = FJET(XB1, XB2, s, 0.0D0, -t530, 0.0D0, t527, 0.0D0, t716)
      t719 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4)
      t720 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4)
      t732 = -z * t720 * t478 - t720
      t767 = -(0.90D2 * t4 * (-t719 + t472 * t720 - (z * t719 - t443 * t
     #720) * t423 * t465) - 0.180D3 * t41 * t732) * t48 * t50 / 0.5760D4
     # - t4 * t732 * t57 / 0.32D2 + (0.90D2 * t4 * (t719 - t494 * t720) 
     #- 0.180D3 * t41 * t720) * t55 * t50 / 0.2880D4 + (-0.180D3 * t41 *
     # (-t507 * t720 + t719) + 0.90D2 * t4 * (t512 * t720 / 0.2D1 - t507
     # * t719) + t96 * t720) * t50 / 0.5760D4
      t768 = FJET(XB1, XB2, s, 0.0D0, -t426, t420, 0.0D0, -t431, t767)
      t770 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t774 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t789 = t605 * t770 * t619 / 0.32D2 + (0.90D2 * t4 * (t604 * t774 -
     # t633 * t770) * t616 - 0.180D3 * t41 * t604 * t770 * t616) * t48 *
     # t55 / 0.5760D4
      t790 = FJET(XB1, XB2, s, 0.0D0, -t602, 0.0D0, t597, 0.0D0, t789)
      t792 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t793 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t795 = z * t792
      t804 = t793 + z * t793 * t36
      t811 = -t804
      t820 = 0.180D3 * t41 * t793
      t829 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t836 = t96 * t793
      t901 = -(0.90D2 * t4 * (t792 - t15 * t793 + (t795 - t26 * t793) * 
     #t36) - 0.180D3 * t41 * t804) * t48 * t50 / 0.5760D4 + t4 * t811 * 
     #t57 / 0.32D2 + (0.90D2 * t4 * (-t792 + t64 * t793) + t820) * t55 *
     # t50 / 0.2880D4 + (-0.180D3 * t41 * (t78 * t793 - t792) + 0.90D2 *
     # t4 * (-t829 - t85 * t793 / 0.2D1 + t78 * t792) - t836) * t50 / 0.
     #5760D4 - ((-t176 * t793 + t792) * t95 - 0.15D2 * t197 * t793 - 0.1
     #80D3 * (t829 - t176 * t792 + t181 * t793 / 0.2D1) * lh + 0.45D2 * 
     #t181 * t792 + t793 * t191 - 0.90D2 * t176 * t829) * t4 / 0.11520D5
     # + ((0.90D2 * t4 * t792 - t820) * t114 + 0.90D2 * t4 * t793 * t122
     # + (0.90D2 * t4 * t829 - 0.180D3 * t41 * t792 + t836) * t131) * t4
     #8 / 0.11520D5 + (0.90D2 * t4 * (t139 * t793 - t792 - (t795 - t144 
     #* t793) * t36) - 0.180D3 * t41 * t811) * t48 * t55 / 0.5760D4 - (-
     #0.180D3 * t41 * (-t160 * t793 + t792) + 0.90D2 * t4 * (t829 + t166
     # * t793 / 0.2D1 - t160 * t792) + t836) * t55 / 0.5760D4
      t902 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t901)
      t904 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4)
      t906 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4)
      t917 = -z * t904 * t478 - t904
      t944 = gbgbH54J3(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4)
      t953 = -(0.90D2 * t4 * (t472 * t904 - t906 - (z * t906 - t443 * t9
     #04) * t423 * t465) - 0.180D3 * t41 * t917) * t48 * t50 / 0.5760D4 
     #- t4 * t917 * t57 / 0.32D2 + (0.90D2 * t4 * (t906 - t494 * t904) -
     # 0.180D3 * t41 * t904) * t55 * t50 / 0.2880D4 + (-0.180D3 * t41 * 
     #(t906 - t507 * t904) + 0.90D2 * t4 * (t512 * t904 / 0.2D1 + t944 -
     # t507 * t906) + t96 * t904) * t50 / 0.5760D4
      t954 = FJET(XB1, XB2, s, t420, 0.0D0, 0.0D0, -t426, -t431, t953)
      t956 = t204 * t203 + t311 * t310 + t418 * t417 + t524 * t523 + t58
     #4 * t583 + t648 * t647 + t677 * t676 + t717 * t716 + t768 * t767 +
     # t790 * t789 + t902 * t901 + t954 * t953
      t957 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t962 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t972 = -t4 * t957 * t57 / 0.32D2 + (0.90D2 * t4 * (t665 * t957 - t
     #962) + 0.180D3 * t41 * t957) * t55 * t50 / 0.2880D4
      t973 = FJET(XB1, XB2, s, t420, -t653, 0.0D0, t652, t655, t972)
      t975 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4)
      t980 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4)
      t985 = 0.180D3 * t41 * t975
      t1012 = t4 * t975 * t57 / 0.32D2 + (0.90D2 * t4 * (-t574 * t975 + 
     #t980) - t985) * t55 * t50 / 0.2880D4 + (0.90D2 * t4 * (t980 - t534
     # * t975) - t985) * t48 * t55 / 0.5760D4 - (-0.180D3 * t41 * (t552 
     #* t975 - t980) + 0.90D2 * t4 * (-t558 * t975 / 0.2D1 + t552 * t980
     #) - t96 * t975) * t55 / 0.5760D4
      t1013 = FJET(XB1, XB2, s, t527, 0.0D0, -t530, 0.0D0, 0.0D0, t1012)
      t1015 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t1019 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t1034 = t605 * t1015 * t619 / 0.32D2 + (0.90D2 * t4 * (t604 * t101
     #9 - t633 * t1015) * t616 - 0.180D3 * t41 * t604 * t1015 * t616) * 
     #t48 * t55 / 0.5760D4
      t1035 = FJET(XB1, XB2, s, t597, 0.0D0, -t602, 0.0D0, 0.0D0, t1034)
      t1037 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t1041 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t1052 = -t4 * t1037 * t57 / 0.32D2 + (0.90D2 * t4 * (-t1041 + t665
     # * t1037) + 0.180D3 * t41 * t1037) * t55 * t50 / 0.2880D4
      t1053 = FJET(XB1, XB2, s, t652, 0.0D0, -t653, t420, t655, t1052)
      t1056 = t420 * t586 * t595
      t1057 = t2 * t421
      t1058 = t136 * x1
      t1059 = t136 * t422
      t1062 = Sqrt(t458 * t528 * t20)
      t1063 = t29 * t1062
      t1065 = 0.2D1 * t1063 * x2
      t1069 = t1057 * (t136 - x2 + t586 + 0.1D1 - x3 - t1058 + t1059 + t
     #1065) * t424 * t595
      t1073 = t20 * s * t1 * x1 * t595
      t1079 = t1057 * x2 * (-0.1D1 + t586 + x1 - t448 - t422 + t456 + 0.
     #2D1 * t1063) * t424 * t595
      t1080 = t4 * t423
      t1081 = x2 * x1
      t1082 = t1081 * z
      t1083 = x2 - t1081 + z - t603 + t1082
      t1084 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t432, t535, -t606, x4)
      t1091 = x2 * t5
      t1100 = 0.2D1 * t1081 + t6 * x2 - t586 * x1 + t1081 * t7 + 0.2D1 *
     # t1091 * z - t1091 * t7 + 0.2D1 * t1063 * z - t1058 + t1065 - 0.3D
     #1 * t1082 - t609 + t610 - x2 - t1091 - z - x3 + 0.2D1 * t1063 * t1
     #082
      t1112 = t136 + t603 + t422 + t452 + t454 - t455 - t457 + t1059 - 0
     #.2D1 * t1063 * t603 - 0.2D1 * t1063 * t1081 - t451 * t1081 - 0.2D1
     # * t6 * t603 + t6 * t7 * x2 + 0.2D1 * t586 * t422 - t6 + t449 - t4
     #50
      t1114 = 0.1D1 / (t1100 + t1112)
      t1116 = t1114 * t48 * t618
      t1119 = FJET(XB1, XB2, s, t1056, -t1069, -t1073, t1079, t655, -t10
     #80 * t1083 * t1084 * t1116 / 0.32D2)
      t1121 = t423 * t1083
      t1127 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t432, t535, -t606, x4)
      t1132 = FJET(XB1, XB2, s, t1079, -t1073, -t1069, t1056, t655, -t10
     #80 * t1083 * t1127 * t1116 / 0.32D2)
      t1139 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4
     #)
      t1143 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4
     #)
      t1149 = 0.180D3 * t41 * t1139
      t1169 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, t535, 0.10D1, x4
     #)
      t1177 = t4 * t1139 * t57 / 0.32D2 + (0.90D2 * t4 * (t1143 - t574 *
     # t1139) - t1149) * t55 * t50 / 0.2880D4 + (0.90D2 * t4 * (t1143 - 
     #t534 * t1139) - t1149) * t48 * t55 / 0.5760D4 - (-0.180D3 * t41 * 
     #(t552 * t1139 - t1143) + 0.90D2 * t4 * (-t558 * t1139 / 0.2D1 + t5
     #52 * t1143 - t1169) - t96 * t1139) * t55 / 0.5760D4
      t1178 = FJET(XB1, XB2, s, -t530, 0.0D0, t527, 0.0D0, 0.0D0, t1177)
      t1180 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4
     #)
      t1182 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4
     #)
      t1193 = -z * t1180 * t478 - t1180
      t1220 = gbgbH52J3(s, XB1, XB2, z, lh, wd, t432, 0.10D1, 0.10D1, x4
     #)
      t1229 = -(0.90D2 * t4 * (t472 * t1180 - t1182 - (z * t1182 - t443 
     #* t1180) * t423 * t465) - 0.180D3 * t41 * t1193) * t48 * t50 / 0.5
     #760D4 - t4 * t1193 * t57 / 0.32D2 + (0.90D2 * t4 * (-t494 * t1180 
     #+ t1182) - 0.180D3 * t41 * t1180) * t55 * t50 / 0.2880D4 + (-0.180
     #D3 * t41 * (t1182 - t507 * t1180) + 0.90D2 * t4 * (t512 * t1180 / 
     #0.2D1 + t1220 - t507 * t1182) + t96 * t1180) * t50 / 0.5760D4
      t1230 = FJET(XB1, XB2, s, -t426, 0.0D0, 0.0D0, t420, -t431, t1229)
      t1232 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t1237 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t432, t535, 0.10D1, x4)
      t1247 = -t4 * t1232 * t57 / 0.32D2 + (0.90D2 * t4 * (t665 * t1232 
     #- t1237) + 0.180D3 * t41 * t1232) * t55 * t50 / 0.2880D4
      t1248 = FJET(XB1, XB2, s, -t653, t420, t652, 0.0D0, t655, t1247)
      t1250 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t1254 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t535, -t606, x4)
      t1269 = t605 * t1250 * t619 / 0.32D2 + (0.90D2 * t4 * (t604 * t125
     #4 - t633 * t1250) * t616 - 0.180D3 * t41 * t604 * t1250 * t616) * 
     #t48 * t55 / 0.5760D4
      t1270 = FJET(XB1, XB2, s, -t602, 0.0D0, t597, 0.0D0, 0.0D0, t1269)
      t1272 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t432, t535, -t606, x4)
      t1277 = FJET(XB1, XB2, s, -t1073, t1079, t1056, -t1069, t655, -t10
     #80 * t1083 * t1272 * t1116 / 0.32D2)
      t1284 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t432, t535, -t606, x4)
      t1289 = FJET(XB1, XB2, s, -t1069, t1056, t1079, -t1073, t655, -t10
     #80 * t1083 * t1284 * t1116 / 0.32D2)
      t1296 = t973 * t972 + t1013 * t1012 + t1035 * t1034 + t1053 * t105
     #2 - t1119 * t4 * t1121 * t1084 * t1114 * t57 / 0.32D2 - t1132 * t4
     # * t1121 * t1127 * t1114 * t57 / 0.32D2 + t1178 * t1177 + t1230 * 
     #t1229 + t1248 * t1247 + t1270 * t1269 - t1277 * t4 * t1121 * t1272
     # * t1114 * t57 / 0.32D2 - t1289 * t4 * t1121 * t1284 * t1114 * t57
     # / 0.32D2
      gbgbH5n1e0 = t956 + t1296

      end function



      doubleprecision function gbgbH5n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t21 = t4 * lh
      t23 = 0.180D3 * t21 * t16
      t25 = 0.1D1 / x1
      t29 = cos(t7)
      t30 = -0.1D1 + x3
      t32 = Sqrt(-x3 * t30)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t32 * z)
      t39 = t16 + z * t16 * t37
      t41 = 0.1D1 / x3
      t42 = t41 * t25
      t45 = t4 * t16
      t46 = 0.1D1 / x2
      t47 = t46 * t25
      t52 = t41 * t46
      t55 = x2 ** 2
      t59 = log(0.4D1 * t55 * t12 * t9)
      t69 = log(0.4D1 * t12 * t9)
      t74 = lh ** 2
      t76 = 0.3141592653589793D1 ** 2
      t78 = 0.180D3 * t74 - 0.30D2 * t76
      t80 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t84 = t69 ** 2
      t90 = x3 * t9
      t95 = log(-0.4D1 * t90 * t12 / t30)
      t100 = log(0.4D1 * t90 * t12)
      t101 = t95 * z * t37 + t100
      t108 = -z * t37 - 0.1D1
      t113 = (0.90D2 * t4 * (-t5 + t15 * t16) + t23) * t25 / 0.5760D4 - 
     #t4 * t39 * t42 / 0.64D2 - t45 * t47 / 0.32D2 - t4 * t39 * t52 / 0.
     #64D2 - (0.90D2 * t4 * (t5 - t59 * t16) - t23) * t46 / 0.5760D4 - (
     #-0.180D3 * (t5 - t69 * t16) * lh + t16 * t78 + 0.90D2 * t80 - 0.90
     #D2 * t69 * t5 + 0.45D2 * t84 * t16) * t4 / 0.11520D5 + (0.90D2 * t
     #45 * t101 + (0.90D2 * t4 * t5 - t23) * t108) * t41 / 0.11520D5
      t114 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t113)
      t116 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t118 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t123 = 0.180D3 * t21 * t116
      t129 = t116 + z * t116 * t37
      t133 = t4 * t116
      t168 = (0.90D2 * t4 * (t15 * t116 - t118) + t123) * t25 / 0.5760D4
     # - t4 * t129 * t42 / 0.64D2 - t133 * t47 / 0.32D2 - t4 * t129 * t5
     #2 / 0.64D2 - (0.90D2 * t4 * (-t59 * t116 + t118) - t123) * t46 / 0
     #.5760D4 - (-0.180D3 * (-t69 * t116 + t118) * lh - 0.90D2 * t69 * t
     #118 + t116 * t78 + 0.45D2 * t84 * t116) * t4 / 0.11520D5 + (0.90D2
     # * t133 * t101 + (0.90D2 * t4 * t118 - t123) * t108) * t41 / 0.115
     #20D5
      t169 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t168)
      t171 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t173 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t178 = 0.180D3 * t21 * t171
      t184 = t171 + z * t171 * t37
      t188 = t4 * t171
      t223 = (0.90D2 * t4 * (t15 * t171 - t173) + t178) * t25 / 0.5760D4
     # - t4 * t184 * t42 / 0.64D2 - t188 * t47 / 0.32D2 - t4 * t184 * t5
     #2 / 0.64D2 - (0.90D2 * t4 * (-t59 * t171 + t173) - t178) * t46 / 0
     #.5760D4 - (-0.180D3 * (-t69 * t171 + t173) * lh - 0.90D2 * t69 * t
     #173 + t171 * t78 + 0.45D2 * t84 * t171) * t4 / 0.11520D5 + (0.90D2
     # * t188 * t101 + (0.90D2 * t4 * t173 - t178) * t108) * t41 / 0.115
     #20D5
      t224 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t223)
      t226 = t2 * x1
      t227 = -0.1D1 + x1
      t228 = x1 * z
      t229 = 0.1D1 - x1 + t228
      t230 = 0.1D1 / t229
      t232 = t2 * t227 * t230
      t233 = t1 ** 2
      t234 = s * t233
      t236 = t227 * x1 * t230
      t237 = t234 * t236
      t238 = -t227
      t239 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t241 = t227 ** 2
      t245 = log(0.4D1 * t10 * t12 * t230 * t241)
      t246 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t257 = t6 * x3
      t258 = x1 * x3
      t270 = Sqrt(-x3 * t229 * t30)
      t274 = -t257 - z + 0.2D1 * t258 - x3 + t228 - x1 * t11 + x3 * t11 
     #* x1 + 0.2D1 * t257 * z - t257 * t11 - 0.3D1 * t258 * z + 0.2D1 * 
     #t29 * t270 * z
      t276 = t229 / t274
      t285 = (0.90D2 * t4 * (t239 - t245 * t246) - 0.180D3 * t21 * t246)
     # * t25 / 0.5760D4 - t4 * (-t246 - z * t246 * t276) * t42 / 0.64D2 
     #+ t4 * t246 * t47 / 0.32D2
      t286 = FJET(XB1, XB2, s, 0.0D0, t226, -t232, 0.0D0, -t237, t285)
      t288 = x2 * s
      t289 = t288 * t1
      t290 = -0.1D1 + x2
      t291 = t290 * s
      t292 = t291 * t1
      t293 = -t290
      t294 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t295 = t4 * t294
      t298 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t303 = log(-0.4D1 * t55 * t9 * t12 * t290)
      t315 = t295 * t52 / 0.64D2 - (0.90D2 * t4 * (-t298 + t303 * t294) 
     #+ 0.180D3 * t21 * t294) * t46 / 0.5760D4 + t295 * t47 / 0.32D2
      t316 = FJET(XB1, XB2, s, 0.0D0, t289, 0.0D0, -t292, 0.0D0, t315)
      t318 = x2 * x3
      t319 = t55 * x3
      t322 = Sqrt(x3 * t290 * t30)
      t323 = t29 * t322
      t325 = 0.2D1 * t323 * x2
      t328 = 0.1D1 / (0.1D1 - x3 + t318)
      t330 = t2 * (0.1D1 - x3 - x2 + t318 + t319 + t325) * t328
      t335 = t2 * x2 * (-0.1D1 + t318 + 0.2D1 * t323) * t328
      t336 = x2 * z
      t337 = t336 - z - x2
      t338 = t4 * t337
      t339 = t30 * t328
      t340 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, -t339, x4)
      t349 = 0.1D1 / (t319 * z - t336 - t318 * z + z - t319 + x3 + x2 - 
     #t325 - 0.2D1 * t323 * z + 0.2D1 * t323 * t336)
      t351 = t349 * t41 * t46
      t354 = FJET(XB1, XB2, s, 0.0D0, t330, 0.0D0, -t335, 0.0D0, t338 * 
     #t340 * t351 / 0.64D2)
      t361 = t1 * t227
      t363 = t291 * t361 * t230
      t364 = t288 * t361
      t366 = t234 * t290 * t236
      t367 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t238, t293, 0.10D1, x4)
      t371 = FJET(XB1, XB2, s, 0.0D0, t363, t226, -t364, t366, -t4 * t36
     #7 * t47 / 0.32D2)
      t377 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t378 = t4 * t377
      t381 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t393 = t378 * t52 / 0.64D2 - (0.90D2 * t4 * (-t381 + t303 * t377) 
     #+ 0.180D3 * t21 * t377) * t46 / 0.5760D4 + t378 * t47 / 0.32D2
      t394 = FJET(XB1, XB2, s, 0.0D0, -t292, 0.0D0, t289, 0.0D0, t393)
      t396 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t398 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t416 = (0.90D2 * t4 * (-t245 * t396 + t398) - 0.180D3 * t21 * t396
     #) * t25 / 0.5760D4 - t4 * (-z * t396 * t276 - t396) * t42 / 0.64D2
     # + t4 * t396 * t47 / 0.32D2
      t417 = FJET(XB1, XB2, s, 0.0D0, -t232, t226, 0.0D0, -t237, t416)
      t419 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, -t339, x4)
      t423 = FJET(XB1, XB2, s, 0.0D0, -t335, 0.0D0, t330, 0.0D0, t338 * 
     #t419 * t351 / 0.64D2)
      t430 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t432 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t437 = 0.180D3 * t21 * t430
      t443 = t430 + z * t430 * t37
      t447 = t4 * t430
      t459 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t484 = (0.90D2 * t4 * (t15 * t430 - t432) + t437) * t25 / 0.5760D4
     # - t4 * t443 * t42 / 0.64D2 - t447 * t47 / 0.32D2 - (-0.180D3 * (-
     #t69 * t430 + t432) * lh - 0.90D2 * t69 * t432 + t430 * t78 + 0.45D
     #2 * t84 * t430 + 0.90D2 * t459) * t4 / 0.11520D5 + (0.90D2 * t447 
     #* t101 + (0.90D2 * t4 * t432 - t437) * t108) * t41 / 0.11520D5 - t
     #4 * t443 * t52 / 0.64D2 - (0.90D2 * t4 * (-t59 * t430 + t432) - t4
     #37) * t46 / 0.5760D4
      t485 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t484)
      t487 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t488 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t507 = (0.90D2 * t4 * (t487 - t245 * t488) - 0.180D3 * t21 * t488)
     # * t25 / 0.5760D4 - t4 * (-z * t488 * t276 - t488) * t42 / 0.64D2 
     #+ t4 * t488 * t47 / 0.32D2
      t508 = FJET(XB1, XB2, s, t226, 0.0D0, 0.0D0, -t232, -t237, t507)
      t510 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t238, t293, 0.10D1, x4)
      t514 = FJET(XB1, XB2, s, t226, -t364, 0.0D0, t363, t366, -t4 * t51
     #0 * t47 / 0.32D2)
      t520 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t521 = t4 * t520
      t527 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t536 = t521 * t47 / 0.32D2 + t521 * t52 / 0.64D2 - (0.90D2 * t4 * 
     #(t303 * t520 - t527) + 0.180D3 * t21 * t520) * t46 / 0.5760D4
      t537 = FJET(XB1, XB2, s, t289, 0.0D0, -t292, 0.0D0, 0.0D0, t536)
      t539 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, -t339, x4)
      t543 = FJET(XB1, XB2, s, t330, 0.0D0, -t335, 0.0D0, 0.0D0, t338 * 
     #t539 * t351 / 0.64D2)
      t550 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t238, t293, 0.10D1, x4)
      t554 = FJET(XB1, XB2, s, t363, 0.0D0, -t364, t226, t366, -t4 * t55
     #0 * t47 / 0.32D2)
      t560 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t561 = t4 * t560
      t567 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t293, 0.10D1, x4)
      t576 = t561 * t47 / 0.32D2 + t561 * t52 / 0.64D2 - (0.90D2 * t4 * 
     #(t303 * t560 - t567) + 0.180D3 * t21 * t560) * t46 / 0.5760D4
      t577 = FJET(XB1, XB2, s, -t292, 0.0D0, t289, 0.0D0, 0.0D0, t576)
      t579 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t580 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t238, 0.10D1, 0.10D1, x4)
      t599 = (0.90D2 * t4 * (t579 - t245 * t580) - 0.180D3 * t21 * t580)
     # * t25 / 0.5760D4 - t4 * (-z * t580 * t276 - t580) * t42 / 0.64D2 
     #+ t4 * t580 * t47 / 0.32D2
      t600 = FJET(XB1, XB2, s, -t232, 0.0D0, 0.0D0, t226, -t237, t599)
      t602 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t238, t293, 0.10D1, x4)
      t606 = FJET(XB1, XB2, s, -t364, t226, t363, 0.0D0, t366, -t4 * t60
     #2 * t47 / 0.32D2)
      t612 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t293, -t339, x4)
      t616 = FJET(XB1, XB2, s, -t335, 0.0D0, t330, 0.0D0, 0.0D0, t338 * 
     #t612 * t351 / 0.64D2)
      gbgbH5n1em1 = t114 * t113 + t169 * t168 + t224 * t223 + t286 * t28
     #5 + t316 * t315 + t354 * t4 * t337 * t340 * t349 * t52 / 0.64D2 - 
     #t371 * t4 * t367 * t46 * t25 / 0.32D2 + t394 * t393 + t417 * t416 
     #+ t423 * t4 * t337 * t419 * t349 * t52 / 0.64D2 + t485 * t484 + t5
     #08 * t507 - t514 * t4 * t510 * t46 * t25 / 0.32D2 + t537 * t536 + 
     #t543 * t4 * t337 * t539 * t349 * t52 / 0.64D2 - t554 * t4 * t550 *
     # t46 * t25 / 0.32D2 + t577 * t576 + t600 * t599 - t606 * t4 * t602
     # * t46 * t25 / 0.32D2 + t616 * t4 * t337 * t612 * t349 * t52 / 0.6
     #4D2

      end function



      doubleprecision function gbgbH5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t15 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t17 = z ** 2
      t19 = x4 * 0.3141592653589793D1
      t20 = Sin(t19)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t17 * t21)
      t30 = cos(t19)
      t33 = Sqrt(-x3 * (-0.1D1 + x3))
      t42 = (-z / (-z - x3 + 0.2D1 * t30 * t33 * z) - 0.1D1) / x3
      t45 = -t6 * t7 / 0.64D2 - t6 * t10 / 0.64D2 - (-0.180D3 * t5 * lh 
     #+ 0.90D2 * t15 - 0.90D2 * t24 * t5) * t4 / 0.11520D5 + t6 * t42 / 
     #0.128D3
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t45)
      t48 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t49 = t4 * t48
      t58 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t65 = -t49 * t7 / 0.64D2 - t49 * t10 / 0.64D2 - (-0.180D3 * t48 * 
     #lh - 0.90D2 * t24 * t48 + 0.90D2 * t58) * t4 / 0.11520D5 + t49 * t
     #42 / 0.128D3
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t65)
      t68 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t69 = t4 * t68
      t78 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t85 = -t69 * t7 / 0.64D2 - t69 * t10 / 0.64D2 - (-0.180D3 * t68 * 
     #lh - 0.90D2 * t24 * t68 + 0.90D2 * t78) * t4 / 0.11520D5 + t69 * t
     #42 / 0.128D3
      t86 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t85)
      t88 = t2 * x1
      t89 = -0.1D1 + x1
      t92 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t94 = t2 * t89 * t92
      t95 = t1 ** 2
      t99 = s * t95 * t89 * x1 * t92
      t100 = -t89
      t101 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t100, 0.10D1, 0.10D1, x4)
      t105 = FJET(XB1, XB2, s, 0.0D0, t88, -t94, 0.0D0, -t99, t4 * t101 
     #* t7 / 0.64D2)
      t111 = x2 * s * t1
      t112 = -0.1D1 + x2
      t114 = t112 * s * t1
      t115 = -t112
      t116 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t115, 0.10D1, x4)
      t120 = FJET(XB1, XB2, s, 0.0D0, t111, 0.0D0, -t114, 0.0D0, t4 * t1
     #16 * t10 / 0.64D2)
      t125 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t115, 0.10D1, x4)
      t129 = FJET(XB1, XB2, s, 0.0D0, -t114, 0.0D0, t111, 0.0D0, t4 * t1
     #25 * t10 / 0.64D2)
      t134 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t100, 0.10D1, 0.10D1, x4)
      t138 = FJET(XB1, XB2, s, 0.0D0, -t94, t88, 0.0D0, -t99, t4 * t134 
     #* t7 / 0.64D2)
      t143 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t144 = t4 * t143
      t151 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t160 = -t144 * t7 / 0.64D2 - t144 * t10 / 0.64D2 - (-0.180D3 * t14
     #3 * lh + 0.90D2 * t151 - 0.90D2 * t24 * t143) * t4 / 0.11520D5 + t
     #144 * t42 / 0.128D3
      t161 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t160)
      t163 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t100, 0.10D1, 0.10D1, x4)
      t167 = FJET(XB1, XB2, s, t88, 0.0D0, 0.0D0, -t94, -t99, t4 * t163 
     #* t7 / 0.64D2)
      t172 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t115, 0.10D1, x4)
      t176 = FJET(XB1, XB2, s, t111, 0.0D0, -t114, 0.0D0, 0.0D0, t4 * t1
     #72 * t10 / 0.64D2)
      t181 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t100, 0.10D1, 0.10D1, x4)
      t185 = FJET(XB1, XB2, s, -t94, 0.0D0, 0.0D0, t88, -t99, t4 * t181 
     #* t7 / 0.64D2)
      t190 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t115, 0.10D1, x4)
      t194 = FJET(XB1, XB2, s, -t114, 0.0D0, t111, 0.0D0, 0.0D0, t4 * t1
     #90 * t10 / 0.64D2)
      gbgbH5n1em2 = t46 * t45 + t66 * t65 + t86 * t85 + t105 * t4 * t101
     # * t7 / 0.64D2 + t120 * t4 * t116 * t10 / 0.64D2 + t129 * t4 * t12
     #5 * t10 / 0.64D2 + t138 * t4 * t134 * t7 / 0.64D2 + t161 * t160 + 
     #t167 * t4 * t163 * t7 / 0.64D2 + t176 * t4 * t172 * t10 / 0.64D2 +
     # t185 * t4 * t181 * t7 / 0.64D2 + t194 * t4 * t190 * t10 / 0.64D2

      end function



      doubleprecision function gbgbH5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t4 * t5 / 
     #0.128D3)
      t11 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t4 * t11 
     #/ 0.128D3)
      t17 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t4 * t17 
     #/ 0.128D3)
      t23 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t4 * t23 
     #/ 0.128D3)
      gbgbH5n1em3 = -t8 * t4 * t5 / 0.128D3 - t14 * t4 * t11 / 0.128D3 -
     # t20 * t4 * t17 / 0.128D3 - t26 * t4 * t23 / 0.128D3

      end function



      doubleprecision function gbgbH5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      gbgbH5n1em4 = 0.0D0

      end function


      doubleprecision function gbgbH5n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.3D1 * t3
      t5 = x2 ** 2
      t6 = t5 * x3
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t9 = -0.1D1 + x3
      t11 = Sqrt(-t3 * t9)
      t12 = t8 * t11
      t13 = 0.2D1 * t12
      t15 = 0.2D1 * t12 * x2
      t17 = t3 - 0.1D1
      t18 = 0.1D1 / t17
      t20 = t2 * (-x2 - x3 + t4 - t6 - t13 + t15) * t18
      t21 = -0.1D1 + x2
      t25 = t2 * t21 * (-t3 - 0.1D1 + x3 + t13) * t18
      t26 = s ** 2
      t27 = 0.1D1 / t26
      t28 = x2 * z
      t29 = 0.1D1 + t28 - x2
      t30 = t9 * t18
      t31 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t32 = t29 * t31
      t33 = x1 ** 2
      t34 = Sin(t7)
      t35 = t34 ** 2
      t36 = t33 * t35
      t37 = t3 * t36
      t38 = z ** 2
      t39 = 0.1D1 / t38
      t40 = t21 ** 2
      t41 = t39 * t40
      t42 = t17 ** 2
      t43 = 0.1D1 / t42
      t45 = t41 * t9 * t43
      t48 = log(-0.4D1 * t37 * t45)
      t49 = t48 * t29
      t50 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t54 = t6 * z
      t55 = t3 * z
      t56 = 0.2D1 * t3
      t60 = 0.1D1 / (-t54 + t6 - t28 + t55 + x2 - t56 - t15 + 0.2D1 * t1
     #2 * t28 - 0.1D1 + t13)
      t63 = t27 * lh
      t65 = t29 * t50 * t60
      t69 = 0.1D1 / x3
      t71 = 0.1D1 / x2
      t72 = 0.1D1 / x1
      t73 = t71 * t72
      t79 = log(-0.4D1 * t3 * t35 * t45)
      t80 = t79 * t29
      t86 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t89 = t79 ** 2
      t90 = t89 * t29
      t97 = lh ** 2
      t98 = 0.180D3 * t97
      t99 = 0.3141592653589793D1 ** 2
      t100 = 0.30D2 * t99
      t101 = t98 - t100
      t102 = t27 * t101
      t108 = -(-0.90D2 * t27 * (t32 - t49 * t50) * t60 + 0.180D3 * t63 *
     # t65) * t69 * t73 / 0.2880D4 - (0.180D3 * t63 * (t32 - t80 * t50) 
     #* t60 - 0.90D2 * t27 * (t29 * t86 - t80 * t31 + t90 * t50 / 0.2D1)
     # * t60 - t102 * t65) * t69 * t71 / 0.5760D4
      t109 = FJET(XB1, XB2, s, t20, 0.0D0, -t25, 0.0D0, 0.0D0, t108)
      t111 = -0.1D1 + x1
      t113 = x1 * z
      t114 = 0.1D1 - x1 + t113
      t115 = 0.1D1 / t114
      t117 = t2 * t111 * x2 * t115
      t118 = t2 * x1
      t119 = t21 * s
      t120 = t1 * t111
      t121 = t119 * t120
      t122 = t1 ** 2
      t127 = s * t122 * x2 * t111 * x1 * t115
      t128 = -t111
      t129 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t130 = t39 * t115
      t131 = t111 ** 2
      t133 = t130 * t131 * t40
      t136 = log(0.4D1 * t37 * t133)
      t137 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t147 = x2 * t33
      t148 = t147 * t35
      t151 = log(0.4D1 * t148 * t133)
      t156 = t151 ** 2
      t168 = -(0.90D2 * t27 * (t129 - t136 * t137) - 0.180D3 * t63 * t13
     #7) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (t151 * t137 - t129)
     # + 0.90D2 * t27 * (-t156 * t137 / 0.2D1 + t151 * t129) - t102 * t1
     #37) * t71 * t72 / 0.2880D4
      t169 = FJET(XB1, XB2, s, 0.0D0, -t117, t118, t121, -t127, t168)
      t171 = t9 * s
      t172 = t1 * x1
      t173 = t171 * t172
      t174 = t171 * t120
      t175 = x1 * x3
      t176 = t2 * t175
      t178 = t2 * t111 * x3
      t179 = -t9
      t180 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t181 = x3 * t33
      t182 = t181 * t35
      t184 = t130 * t131 * t9
      t187 = log(-0.4D1 * t182 * t184)
      t188 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t194 = t187 ** 2
      t197 = gbgbH54J3(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t207 = log(-0.4D1 * t37 * t184)
      t218 = (-0.180D3 * t63 * (-t180 + t187 * t188) + 0.90D2 * t27 * (t
     #187 * t180 - t194 * t188 / 0.2D1 - t197) - t102 * t188) * t69 * t7
     #2 / 0.2880D4 - (0.90D2 * t27 * (t180 - t207 * t188) - 0.180D3 * t6
     #3 * t188) * t69 * t73 / 0.2880D4
      t219 = FJET(XB1, XB2, s, -t173, t174, t176, -t178, 0.0D0, t218)
      t221 = t119 * t1
      t223 = x2 * s * t1
      t224 = t181 * x2
      t225 = t35 * t39
      t226 = t225 * t40
      t229 = log(0.4D1 * t224 * t226)
      t230 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t232 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t244 = log(0.4D1 * t147 * t226)
      t250 = t244 ** 2
      t256 = t102 * t230
      t261 = x2 * t35
      t264 = log(0.4D1 * t261 * t41)
      t265 = t264 ** 2
      t277 = t265 * t264
      t284 = 0.120D3 * t97 * lh
      t286 = 0.60D2 * lh * t99
      t287 = -0.2884936567583026D3 - t284 + t286
      t288 = t27 * t287
      t295 = log(0.4D1 * t3 * t226)
      t300 = t295 ** 2
      t311 = -(0.90D2 * t27 * (t229 * t230 - t232) + 0.180D3 * t63 * t23
     #0) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (t232 - t244 * t230)
     # + 0.90D2 * t27 * (-t244 * t232 + t250 * t230 / 0.2D1) + t256) * t
     #71 * t72 / 0.2880D4 + (-0.180D3 * t63 * (t265 * t230 / 0.2D1 - t26
     #4 * t232) + t102 * (-t264 * t230 + t232) + 0.90D2 * t27 * (t265 * 
     #t232 / 0.2D1 - t277 * t230 / 0.6D1) + t288 * t230) * t71 / 0.5760D
     #4 - (-0.180D3 * t63 * (t295 * t230 - t232) + 0.90D2 * t27 * (-t300
     # * t230 / 0.2D1 + t295 * t232) - t256) * t69 * t71 / 0.5760D4
      t312 = FJET(XB1, XB2, s, -t221, 0.0D0, t223, 0.0D0, 0.0D0, t311)
      t314 = t36 * t39
      t316 = log(0.4D1 * t314)
      t317 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t319 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t320 = t316 ** 2
      t321 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t333 = t320 * t316
      t339 = t288 * t321
      t345 = log(0.4D1 * t181 * t225)
      t351 = t345 ** 2
      t357 = t102 * t321
      t362 = t3 * t314
      t364 = log(0.4D1 * t362)
      t377 = log(0.4D1 * t147 * t225)
      t382 = t377 ** 2
      t394 = log(0.4D1 * t225)
      t395 = t394 ** 2
      t396 = t395 ** 2
      t400 = t99 ** 2
      t401 = t97 ** 2
      t407 = t395 * t394
      t411 = (0.15D2 / 0.4D1 * t396 - t394 * t287 + 0.5769873135166051D3
     # * lh + t400 + 0.60D2 * t401 - 0.60D2 * t97 * t99 + t395 * t101 / 
     #0.2D1 + 0.30D2 * t407 * lh) * t27
      t418 = (0.180D3 * t394 * lh + t98 - t100 + 0.45D2 * t395) * t27
      t426 = (-t394 * t101 - 0.90D2 * t395 * lh - 0.2884936567583026D3 -
     # t284 + t286 - 0.15D2 * t407) * t27
      t431 = log(0.4D1 * t261 * t39)
      t432 = t431 ** 2
      t443 = t432 * t431
      t456 = log(0.4D1 * t3 * t225)
      t462 = t456 ** 2
      t472 = x3 * t35
      t475 = log(0.4D1 * t472 * t39)
      t477 = t475 ** 2
      t489 = t477 * t475
      t498 = -(-0.180D3 * t63 * (-t316 * t317 + t319 + t320 * t321 / 0.2
     #D1) + t102 * (t317 - t316 * t321) + 0.90D2 * t27 * (-t316 * t319 +
     # t320 * t317 / 0.2D1 - t333 * t321 / 0.6D1) + t339) * t72 / 0.2880
     #D4 + (-0.180D3 * t63 * (-t317 + t345 * t321) + 0.90D2 * t27 * (t34
     #5 * t317 - t319 - t351 * t321 / 0.2D1) - t357) * t69 * t72 / 0.288
     #0D4 - (0.90D2 * t27 * (t317 - t364 * t321) - 0.180D3 * t63 * t321)
     # * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (-t317 + t377 * t321) 
     #+ 0.90D2 * t27 * (-t382 * t321 / 0.2D1 + t377 * t317 - t319) - t35
     #7) * t71 * t72 / 0.2880D4 - t411 * t321 / 0.5760D4 - t418 * t319 /
     # 0.5760D4 - t426 * t317 / 0.5760D4 + (-0.180D3 * t63 * (-t432 * t3
     #21 / 0.2D1 + t431 * t317 - t319) + t102 * (-t317 + t431 * t321) + 
     #0.90D2 * t27 * (t431 * t319 + t443 * t321 / 0.6D1 - t432 * t317 / 
     #0.2D1) - t339) * t71 / 0.5760D4 - (-0.180D3 * t63 * (t317 - t456 *
     # t321) + 0.90D2 * t27 * (-t456 * t317 + t319 + t462 * t321 / 0.2D1
     #) + t357) * t69 * t71 / 0.5760D4 + (-0.180D3 * t63 * (t475 * t317 
     #- t319 - t477 * t321 / 0.2D1) + t102 * (-t317 + t475 * t321) + 0.9
     #0D2 * t27 * (t475 * t319 - t477 * t317 / 0.2D1 + t489 * t321 / 0.6
     #D1) - t339) * t69 / 0.5760D4
      t499 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t498)
      t501 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t504 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t523 = t288 * t504
      t556 = t102 * t504
      t618 = -t426 * t501 / 0.5760D4 - t411 * t504 / 0.5760D4 + (-0.180D
     #3 * t63 * (t475 * t501 - t477 * t504 / 0.2D1) + t102 * (t475 * t50
     #4 - t501) + 0.90D2 * t27 * (-t477 * t501 / 0.2D1 + t489 * t504 / 0
     #.6D1) - t523) * t69 / 0.5760D4 - (-0.180D3 * t63 * (-t316 * t501 +
     # t320 * t504 / 0.2D1) + t102 * (-t316 * t504 + t501) + 0.90D2 * t2
     #7 * (t320 * t501 / 0.2D1 - t333 * t504 / 0.6D1) + t523) * t72 / 0.
     #2880D4 + (-0.180D3 * t63 * (t345 * t504 - t501) + 0.90D2 * t27 * (
     #t345 * t501 - t351 * t504 / 0.2D1) - t556) * t69 * t72 / 0.2880D4 
     #- (0.90D2 * t27 * (-t364 * t504 + t501) - 0.180D3 * t63 * t504) * 
     #t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (-t501 + t377 * t504) + 0
     #.90D2 * t27 * (t377 * t501 - t382 * t504 / 0.2D1) - t556) * t71 * 
     #t72 / 0.2880D4 + (-0.180D3 * t63 * (t431 * t501 - t432 * t504 / 0.
     #2D1) + t102 * (-t501 + t431 * t504) + 0.90D2 * t27 * (t443 * t504 
     #/ 0.6D1 - t432 * t501 / 0.2D1) - t523) * t71 / 0.5760D4 - (-0.180D
     #3 * t63 * (-t456 * t504 + t501) + 0.90D2 * t27 * (-t456 * t501 + t
     #462 * t504 / 0.2D1) + t556) * t69 * t71 / 0.5760D4
      t619 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t618)
      t621 = t2 * t111
      t622 = t130 * t131
      t625 = log(0.4D1 * t36 * t622)
      t626 = t625 ** 2
      t627 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t630 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t638 = t626 * t625
      t651 = log(0.4D1 * t182 * t622)
      t656 = t651 ** 2
      t663 = t102 * t627
      t667 = t115 * t131
      t671 = log(0.4D1 * t224 * t225 * t667)
      t683 = log(0.4D1 * t148 * t622)
      t688 = t683 ** 2
      t699 = -(-0.180D3 * t63 * (-t626 * t627 / 0.2D1 + t625 * t630) + t
     #102 * (-t630 + t625 * t627) + 0.90D2 * t27 * (t638 * t627 / 0.6D1 
     #- t626 * t630 / 0.2D1) - t288 * t627) * t72 / 0.2880D4 + (-0.180D3
     # * t63 * (t630 - t651 * t627) + 0.90D2 * t27 * (t656 * t627 / 0.2D
     #1 - t651 * t630) + t663) * t69 * t72 / 0.2880D4 - (0.90D2 * t27 * 
     #(t671 * t627 - t630) + 0.180D3 * t63 * t627) * t69 * t73 / 0.2880D
     #4 + (-0.180D3 * t63 * (t630 - t683 * t627) + 0.90D2 * t27 * (t688 
     #* t627 / 0.2D1 - t683 * t630) + t663) * t71 * t72 / 0.2880D4
      t700 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t118, -t621, 0.0D0, t699)
      t702 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t703 = t29 * t702
      t704 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t718 = t29 * t704 * t60
      t735 = -(0.180D3 * t63 * (t703 - t80 * t704) * t60 - 0.90D2 * t27 
     #* (-t80 * t702 + t90 * t704 / 0.2D1) * t60 - t102 * t718) * t69 * 
     #t71 / 0.5760D4 - (-0.90D2 * t27 * (t703 - t49 * t704) * t60 + 0.18
     #0D3 * t63 * t718) * t69 * t73 / 0.2880D4
      t736 = FJET(XB1, XB2, s, 0.0D0, t20, 0.0D0, -t25, 0.0D0, t735)
      t738 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t739 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t742 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t772 = t102 * t739
      t801 = (-0.180D3 * t63 * (t738 + t265 * t739 / 0.2D1 - t264 * t742
     #) + t102 * (t742 - t264 * t739) + 0.90D2 * t27 * (-t264 * t738 + t
     #265 * t742 / 0.2D1 - t277 * t739 / 0.6D1) + t288 * t739) * t71 / 0
     #.5760D4 - (-0.180D3 * t63 * (t295 * t739 - t742) + 0.90D2 * t27 * 
     #(-t738 + t295 * t742 - t300 * t739 / 0.2D1) - t772) * t69 * t71 / 
     #0.5760D4 - (0.90D2 * t27 * (-t742 + t229 * t739) + 0.180D3 * t63 *
     # t739) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (-t244 * t739 + 
     #t742) + 0.90D2 * t27 * (t738 + t250 * t739 / 0.2D1 - t244 * t742) 
     #+ t772) * t71 * t72 / 0.2880D4
      t802 = FJET(XB1, XB2, s, 0.0D0, -t221, 0.0D0, t223, 0.0D0, t801)
      t804 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t807 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t809 = gbgbH52J3(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t837 = t102 * t804
      t864 = -(-0.180D3 * t63 * (-t626 * t804 / 0.2D1 + t625 * t807 - t8
     #09) + t102 * (t625 * t804 - t807) + 0.90D2 * t27 * (t625 * t809 + 
     #t638 * t804 / 0.6D1 - t626 * t807 / 0.2D1) - t288 * t804) * t72 / 
     #0.2880D4 + (-0.180D3 * t63 * (-t651 * t804 + t807) + 0.90D2 * t27 
     #* (t656 * t804 / 0.2D1 - t651 * t807 + t809) + t837) * t69 * t72 /
     # 0.2880D4 - (0.90D2 * t27 * (t671 * t804 - t807) + 0.180D3 * t63 *
     # t804) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (-t683 * t804 + 
     #t807) + 0.90D2 * t27 * (t809 + t688 * t804 / 0.2D1 - t683 * t807) 
     #+ t837) * t71 * t72 / 0.2880D4
      t865 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t621, t118, 0.0D0, t864)
      t867 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t868 = t29 * t867
      t869 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t876 = t29 * t869 * t60
      t900 = -(-0.90D2 * t27 * (t868 - t49 * t869) * t60 + 0.180D3 * t63
     # * t876) * t69 * t73 / 0.2880D4 - (0.180D3 * t63 * (t868 - t80 * t
     #869) * t60 - 0.90D2 * t27 * (-t80 * t867 + t90 * t869 / 0.2D1) * t
     #60 - t102 * t876) * t69 * t71 / 0.5760D4
      t901 = FJET(XB1, XB2, s, -t25, 0.0D0, t20, 0.0D0, 0.0D0, t900)
      t903 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t904 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t920 = gbgbH54J3(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t930 = -(0.90D2 * t27 * (t903 - t136 * t904) - 0.180D3 * t63 * t90
     #4) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (t151 * t904 - t903)
     # + 0.90D2 * t27 * (-t156 * t904 / 0.2D1 - t920 + t151 * t903) - t1
     #02 * t904) * t71 * t72 / 0.2880D4
      t931 = FJET(XB1, XB2, s, t118, t121, 0.0D0, -t117, -t127, t930)
      t936 = Sqrt(-x3 * t114 * x2 * t9)
      t937 = t8 * t936
      t938 = 0.2D1 * t937
      t939 = t6 * x1
      t940 = t175 * z
      t941 = t3 * x1
      t944 = 0.2D1 * t937 * x2
      t945 = t6 * t113
      t946 = t3 * t113
      t948 = t175 - t6 + t4 - t938 - x2 - x3 + t939 - t940 - 0.2D1 * t94
     #1 + t944 - t945 + 0.2D1 * t946
      t951 = t621 * t948 * t115 * t18
      t954 = t118 * x3 * t21 * t18
      t959 = t621 * t21 * (-t3 - 0.1D1 + x3 + x1 - t175 - t113 + t940 + 
     #t938) * t115 * t18
      t961 = t171 * t172 * t18
      t962 = x2 * x1
      t963 = t962 * z
      t964 = -0.1D1 + x2 - t962 - t28 + t963 - t113 + x1
      t965 = t114 * t964
      t966 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t973 = log(-0.4D1 * t362 * t667 * t40 * t9 * t43)
      t974 = t973 * t114
      t975 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t976 = t964 * t975
      t989 = 0.1D1 - t6 + t56 + t28 - t55 + t54 - x2 - 0.2D1 * t33 * z +
     # t33 * t38 - t147 + 0.2D1 * t962 - t938 + 0.4D1 * t946 - t945 - 0.
     #2D1 * t181 * t28 - x3 * t38 * t962
      t1010 = -0.2D1 * t937 * t28 + t181 * t38 * x2 - 0.2D1 * t937 * t96
     #2 - 0.2D1 * t937 * t113 + 0.2D1 * t937 * t963 + 0.2D1 * t113 - 0.2
     #D1 * x1 + t33 + t224 + t962 * t38 + 0.2D1 * t147 * z - t147 * t38 
     #+ 0.2D1 * t937 * x1 - 0.3D1 * t963 + t939 - 0.3D1 * t941 + t944
      t1012 = 0.1D1 / (t989 + t1010)
      t1015 = t63 * t114
      t1019 = 0.90D2 * t27 * (t965 * t966 - t974 * t976) * t1012 - 0.180
     #D3 * t1015 * t976 * t1012
      t1023 = FJET(XB1, XB2, s, -t951, t954, t959, t961, -t127, -t1019 *
     # t69 * t73 / 0.2880D4)
      t1026 = t69 * t71 * t72
      t1029 = t2 * x3
      t1030 = t2 * t9
      t1031 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1032 = t225 * t9
      t1035 = log(-0.4D1 * t181 * t1032)
      t1036 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1041 = t1035 ** 2
      t1044 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1049 = t102 * t1036
      t1056 = log(-0.4D1 * t224 * t1032)
      t1070 = log(-0.4D1 * t472 * t39 * t9)
      t1071 = t1070 ** 2
      t1082 = t1071 * t1070
      t1096 = log(-0.4D1 * t3 * t1032)
      t1102 = t1096 ** 2
      t1112 = (-0.180D3 * t63 * (t1031 - t1035 * t1036) + 0.90D2 * t27 *
     # (t1041 * t1036 / 0.2D1 + t1044 - t1035 * t1031) + t1049) * t69 * 
     #t72 / 0.2880D4 - (0.90D2 * t27 * (t1056 * t1036 - t1031) + 0.180D3
     # * t63 * t1036) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (t1044 
     #+ t1071 * t1036 / 0.2D1 - t1070 * t1031) + t102 * (t1031 - t1070 *
     # t1036) + 0.90D2 * t27 * (-t1070 * t1044 - t1082 * t1036 / 0.6D1 +
     # t1071 * t1031 / 0.2D1) + t288 * t1036) * t69 / 0.5760D4 - (-0.180
     #D3 * t63 * (-t1031 + t1096 * t1036) + 0.90D2 * t27 * (t1096 * t103
     #1 - t1044 - t1102 * t1036 / 0.2D1) - t1049) * t69 * t71 / 0.5760D4
      t1113 = FJET(XB1, XB2, s, t1029, 0.0D0, -t1030, 0.0D0, 0.0D0, t111
     #2)
      t1115 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1117 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1134 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1138 = t102 * t1115
      t1178 = -(0.90D2 * t27 * (t229 * t1115 - t1117) + 0.180D3 * t63 * 
     #t1115) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (-t244 * t1115 +
     # t1117) + 0.90D2 * t27 * (-t244 * t1117 + t250 * t1115 / 0.2D1 + t
     #1134) + t1138) * t71 * t72 / 0.2880D4 + (-0.180D3 * t63 * (t1134 +
     # t265 * t1115 / 0.2D1 - t264 * t1117) + t102 * (-t264 * t1115 + t1
     #117) + 0.90D2 * t27 * (-t264 * t1134 + t265 * t1117 / 0.2D1 - t277
     # * t1115 / 0.6D1) + t288 * t1115) * t71 / 0.5760D4 - (-0.180D3 * t
     #63 * (-t1117 + t295 * t1115) + 0.90D2 * t27 * (-t300 * t1115 / 0.2
     #D1 - t1134 + t295 * t1117) - t1138) * t69 * t71 / 0.5760D4
      t1179 = FJET(XB1, XB2, s, t223, 0.0D0, -t221, 0.0D0, 0.0D0, t1178)
      t1181 = gbgbH54J3(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t1182 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t1184 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t1214 = t102 * t1184
      t1241 = -(-0.180D3 * t63 * (-t1181 + t625 * t1182 - t626 * t1184 /
     # 0.2D1) + t102 * (-t1182 + t625 * t1184) + 0.90D2 * t27 * (t625 * 
     #t1181 + t638 * t1184 / 0.6D1 - t626 * t1182 / 0.2D1) - t288 * t118
     #4) * t72 / 0.2880D4 + (-0.180D3 * t63 * (t1182 - t651 * t1184) + 0
     #.90D2 * t27 * (t656 * t1184 / 0.2D1 - t651 * t1182 + t1181) + t121
     #4) * t69 * t72 / 0.2880D4 - (0.90D2 * t27 * (-t1182 + t671 * t1184
     #) + 0.180D3 * t63 * t1184) * t69 * t73 / 0.2880D4 + (-0.180D3 * t6
     #3 * (-t683 * t1184 + t1182) + 0.90D2 * t27 * (t1181 + t688 * t1184
     # / 0.2D1 - t683 * t1182) + t1214) * t71 * t72 / 0.2880D4
      t1242 = FJET(XB1, XB2, s, t118, -t621, 0.0D0, 0.0D0, 0.0D0, t1241)
      t1244 = t109 * t108 + t169 * t168 + t219 * t218 + t312 * t311 + t4
     #99 * t498 + t619 * t618 + t700 * t699 + t736 * t735 + t802 * t801 
     #+ t865 * t864 + t901 * t900 + t931 * t930 - t1023 * t1019 * t1026 
     #/ 0.2880D4 + t1113 * t1112 + t1179 * t1178 + t1242 * t1241
      t1245 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t1247 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, 0.10D1, x4)
      t1276 = t102 * t1247
      t1303 = -(-0.180D3 * t63 * (t625 * t1245 - t626 * t1247 / 0.2D1) +
     # t102 * (-t1245 + t625 * t1247) + 0.90D2 * t27 * (t638 * t1247 / 0
     #.6D1 - t626 * t1245 / 0.2D1) - t288 * t1247) * t72 / 0.2880D4 + (-
     #0.180D3 * t63 * (t1245 - t651 * t1247) + 0.90D2 * t27 * (t656 * t1
     #247 / 0.2D1 - t651 * t1245) + t1276) * t69 * t72 / 0.2880D4 - (0.9
     #0D2 * t27 * (-t1245 + t671 * t1247) + 0.180D3 * t63 * t1247) * t69
     # * t73 / 0.2880D4 + (-0.180D3 * t63 * (-t683 * t1247 + t1245) + 0.
     #90D2 * t27 * (t688 * t1247 / 0.2D1 - t683 * t1245) + t1276) * t71 
     #* t72 / 0.2880D4
      t1304 = FJET(XB1, XB2, s, -t621, t118, 0.0D0, 0.0D0, 0.0D0, t1303)
      t1306 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t1308 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t1309 = t964 * t1308
      t1318 = 0.90D2 * t27 * (t965 * t1306 - t974 * t1309) * t1012 - 0.1
     #80D3 * t1015 * t1309 * t1012
      t1322 = FJET(XB1, XB2, s, t959, t961, -t951, t954, -t127, -t1318 *
     # t69 * t73 / 0.2880D4)
      t1326 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1328 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1338 = t102 * t1326
      t1387 = (-0.180D3 * t63 * (-t1035 * t1326 + t1328) + 0.90D2 * t27 
     #* (-t1035 * t1328 + t1041 * t1326 / 0.2D1) + t1338) * t69 * t72 / 
     #0.2880D4 - (0.90D2 * t27 * (t1056 * t1326 - t1328) + 0.180D3 * t63
     # * t1326) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (t1071 * t132
     #6 / 0.2D1 - t1070 * t1328) + t102 * (t1328 - t1070 * t1326) + 0.90
     #D2 * t27 * (-t1082 * t1326 / 0.6D1 + t1071 * t1328 / 0.2D1) + t288
     # * t1326) * t69 / 0.5760D4 - (-0.180D3 * t63 * (t1096 * t1326 - t1
     #328) + 0.90D2 * t27 * (-t1102 * t1326 / 0.2D1 + t1096 * t1328) - t
     #1338) * t69 * t71 / 0.5760D4
      t1388 = FJET(XB1, XB2, s, -t1030, 0.0D0, t1029, 0.0D0, 0.0D0, t138
     #7)
      t1390 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1393 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1408 = t288 * t1390
      t1422 = t102 * t1390
      t1507 = -(-0.180D3 * t63 * (t320 * t1390 / 0.2D1 - t316 * t1393) +
     # t102 * (t1393 - t316 * t1390) + 0.90D2 * t27 * (t320 * t1393 / 0.
     #2D1 - t333 * t1390 / 0.6D1) + t1408) * t72 / 0.2880D4 + (-0.180D3 
     #* t63 * (-t1393 + t345 * t1390) + 0.90D2 * t27 * (t345 * t1393 - t
     #351 * t1390 / 0.2D1) - t1422) * t69 * t72 / 0.2880D4 - (0.90D2 * t
     #27 * (-t364 * t1390 + t1393) - 0.180D3 * t63 * t1390) * t69 * t73 
     #/ 0.2880D4 + (-0.180D3 * t63 * (t377 * t1390 - t1393) + 0.90D2 * t
     #27 * (-t382 * t1390 / 0.2D1 + t377 * t1393) - t1422) * t71 * t72 /
     # 0.2880D4 - t426 * t1393 / 0.5760D4 + (-0.180D3 * t63 * (t431 * t1
     #393 - t432 * t1390 / 0.2D1) + t102 * (-t1393 + t431 * t1390) + 0.9
     #0D2 * t27 * (-t432 * t1393 / 0.2D1 + t443 * t1390 / 0.6D1) - t1408
     #) * t71 / 0.5760D4 - (-0.180D3 * t63 * (t1393 - t456 * t1390) + 0.
     #90D2 * t27 * (-t456 * t1393 + t462 * t1390 / 0.2D1) + t1422) * t69
     # * t71 / 0.5760D4 - t411 * t1390 / 0.5760D4 + (-0.180D3 * t63 * (-
     #t477 * t1390 / 0.2D1 + t475 * t1393) + t102 * (-t1393 + t475 * t13
     #90) + 0.90D2 * t27 * (-t477 * t1393 / 0.2D1 + t489 * t1390 / 0.6D1
     #) - t1408) * t69 / 0.5760D4
      t1508 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1507)
      t1510 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1511 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1519 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1523 = t102 * t1511
      t1573 = (-0.180D3 * t63 * (t1510 - t1035 * t1511) + 0.90D2 * t27 *
     # (t1041 * t1511 / 0.2D1 - t1035 * t1510 + t1519) + t1523) * t69 * 
     #t72 / 0.2880D4 - (0.90D2 * t27 * (t1056 * t1511 - t1510) + 0.180D3
     # * t63 * t1511) * t69 * t73 / 0.2880D4 - (-0.180D3 * t63 * (t1096 
     #* t1511 - t1510) + 0.90D2 * t27 * (-t1102 * t1511 / 0.2D1 + t1096 
     #* t1510 - t1519) - t1523) * t69 * t71 / 0.5760D4 + (-0.180D3 * t63
     # * (t1519 + t1071 * t1511 / 0.2D1 - t1070 * t1510) + t102 * (-t107
     #0 * t1511 + t1510) + 0.90D2 * t27 * (-t1070 * t1519 - t1082 * t151
     #1 / 0.6D1 + t1071 * t1510 / 0.2D1) + t288 * t1511) * t69 / 0.5760D
     #4
      t1574 = FJET(XB1, XB2, s, 0.0D0, -t1030, 0.0D0, t1029, 0.0D0, t157
     #3)
      t1576 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t1577 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t1602 = (-0.180D3 * t63 * (-t1576 + t187 * t1577) + 0.90D2 * t27 *
     # (t187 * t1576 - t194 * t1577 / 0.2D1) - t102 * t1577) * t69 * t72
     # / 0.2880D4 - (0.90D2 * t27 * (-t207 * t1577 + t1576) - 0.180D3 * 
     #t63 * t1577) * t69 * t73 / 0.2880D4
      t1603 = FJET(XB1, XB2, s, t174, -t173, -t178, t176, 0.0D0, t1602)
      t1605 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t1607 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t1608 = t964 * t1607
      t1617 = 0.90D2 * t27 * (t965 * t1605 - t974 * t1608) * t1012 - 0.1
     #80D3 * t1015 * t1608 * t1012
      t1621 = FJET(XB1, XB2, s, t961, t959, t954, -t951, -t127, -t1617 *
     # t69 * t73 / 0.2880D4)
      t1625 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1628 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1657 = t102 * t1625
      t1686 = (-0.180D3 * t63 * (t265 * t1625 / 0.2D1 - t264 * t1628) + 
     #t102 * (-t264 * t1625 + t1628) + 0.90D2 * t27 * (t265 * t1628 / 0.
     #2D1 - t277 * t1625 / 0.6D1) + t288 * t1625) * t71 / 0.5760D4 - (-0
     #.180D3 * t63 * (t295 * t1625 - t1628) + 0.90D2 * t27 * (t295 * t16
     #28 - t300 * t1625 / 0.2D1) - t1657) * t69 * t71 / 0.5760D4 - (0.90
     #D2 * t27 * (-t1628 + t229 * t1625) + 0.180D3 * t63 * t1625) * t69 
     #* t73 / 0.2880D4 + (-0.180D3 * t63 * (-t244 * t1625 + t1628) + 0.9
     #0D2 * t27 * (t250 * t1625 / 0.2D1 - t244 * t1628) + t1657) * t71 *
     # t72 / 0.2880D4
      t1687 = FJET(XB1, XB2, s, 0.0D0, t223, 0.0D0, -t221, 0.0D0, t1686)
      t1689 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t1691 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t1715 = -(0.90D2 * t27 * (-t136 * t1689 + t1691) - 0.180D3 * t63 *
     # t1689) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (t151 * t1689 -
     # t1691) + 0.90D2 * t27 * (-t156 * t1689 / 0.2D1 + t151 * t1691) - 
     #t102 * t1689) * t71 * t72 / 0.2880D4
      t1716 = FJET(XB1, XB2, s, t121, t118, -t117, 0.0D0, -t127, t1715)
      t1718 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t1720 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t128, x2, t30, x4)
      t1721 = t964 * t1720
      t1730 = 0.90D2 * t27 * (t965 * t1718 - t974 * t1721) * t1012 - 0.1
     #80D3 * t1015 * t1721 * t1012
      t1734 = FJET(XB1, XB2, s, t954, -t951, t961, t959, -t127, -t1730 *
     # t69 * t73 / 0.2880D4)
      t1738 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1741 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1742 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1760 = t288 * t1742
      t1774 = t102 * t1742
      t1861 = -t426 * t1738 / 0.5760D4 - (-0.180D3 * t63 * (t1741 + t320
     # * t1742 / 0.2D1 - t316 * t1738) + t102 * (t1738 - t316 * t1742) +
     # 0.90D2 * t27 * (-t316 * t1741 + t320 * t1738 / 0.2D1 - t333 * t17
     #42 / 0.6D1) + t1760) * t72 / 0.2880D4 + (-0.180D3 * t63 * (-t1738 
     #+ t345 * t1742) + 0.90D2 * t27 * (-t1741 - t351 * t1742 / 0.2D1 + 
     #t345 * t1738) - t1774) * t69 * t72 / 0.2880D4 - (0.90D2 * t27 * (t
     #1738 - t364 * t1742) - 0.180D3 * t63 * t1742) * t69 * t73 / 0.2880
     #D4 + (-0.180D3 * t63 * (t377 * t1742 - t1738) + 0.90D2 * t27 * (-t
     #382 * t1742 / 0.2D1 + t377 * t1738 - t1741) - t1774) * t71 * t72 /
     # 0.2880D4 - t411 * t1742 / 0.5760D4 - t418 * t1741 / 0.5760D4 + (-
     #0.180D3 * t63 * (t431 * t1738 - t1741 - t432 * t1742 / 0.2D1) + t1
     #02 * (-t1738 + t431 * t1742) + 0.90D2 * t27 * (t431 * t1741 - t432
     # * t1738 / 0.2D1 + t443 * t1742 / 0.6D1) - t1760) * t71 / 0.5760D4
     # - (-0.180D3 * t63 * (-t456 * t1742 + t1738) + 0.90D2 * t27 * (-t4
     #56 * t1738 + t462 * t1742 / 0.2D1 + t1741) + t1774) * t69 * t71 / 
     #0.5760D4 + (-0.180D3 * t63 * (-t477 * t1742 / 0.2D1 - t1741 + t475
     # * t1738) + t102 * (t475 * t1742 - t1738) + 0.90D2 * t27 * (t475 *
     # t1741 - t477 * t1738 / 0.2D1 + t489 * t1742 / 0.6D1) - t1760) * t
     #69 / 0.5760D4
      t1862 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1861)
      t1864 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t1865 = t29 * t1864
      t1866 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t1873 = t29 * t1866 * t60
      t1885 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t30, x4)
      t1899 = -(-0.90D2 * t27 * (t1865 - t49 * t1866) * t60 + 0.180D3 * 
     #t63 * t1873) * t69 * t73 / 0.2880D4 - (0.180D3 * t63 * (t1865 - t8
     #0 * t1866) * t60 - 0.90D2 * t27 * (t29 * t1885 - t80 * t1864 + t90
     # * t1866 / 0.2D1) * t60 - t102 * t1873) * t69 * t71 / 0.5760D4
      t1900 = FJET(XB1, XB2, s, 0.0D0, -t25, 0.0D0, t20, 0.0D0, t1899)
      t1902 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t1903 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t1910 = gbgbH52J3(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t1929 = (-0.180D3 * t63 * (-t1902 + t187 * t1903) + 0.90D2 * t27 *
     # (-t194 * t1903 / 0.2D1 - t1910 + t187 * t1902) - t102 * t1903) * 
     #t69 * t72 / 0.2880D4 - (0.90D2 * t27 * (t1902 - t207 * t1903) - 0.
     #180D3 * t63 * t1903) * t69 * t73 / 0.2880D4
      t1930 = FJET(XB1, XB2, s, -t178, t176, t174, -t173, 0.0D0, t1929)
      t1932 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1934 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t179, x4)
      t1944 = t102 * t1932
      t1993 = (-0.180D3 * t63 * (-t1035 * t1932 + t1934) + 0.90D2 * t27 
     #* (-t1035 * t1934 + t1041 * t1932 / 0.2D1) + t1944) * t69 * t72 / 
     #0.2880D4 - (0.90D2 * t27 * (t1056 * t1932 - t1934) + 0.180D3 * t63
     # * t1932) * t69 * t73 / 0.2880D4 - (-0.180D3 * t63 * (t1096 * t193
     #2 - t1934) + 0.90D2 * t27 * (t1096 * t1934 - t1102 * t1932 / 0.2D1
     #) - t1944) * t69 * t71 / 0.5760D4 + (-0.180D3 * t63 * (t1071 * t19
     #32 / 0.2D1 - t1070 * t1934) + t102 * (-t1070 * t1932 + t1934) + 0.
     #90D2 * t27 * (-t1082 * t1932 / 0.6D1 + t1071 * t1934 / 0.2D1) + t2
     #88 * t1932) * t69 / 0.5760D4
      t1994 = FJET(XB1, XB2, s, 0.0D0, t1029, 0.0D0, -t1030, 0.0D0, t199
     #3)
      t1996 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t1997 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t128, 0.0D0, t179, x4)
      t2022 = (-0.180D3 * t63 * (-t1996 + t187 * t1997) + 0.90D2 * t27 *
     # (-t194 * t1997 / 0.2D1 + t187 * t1996) - t102 * t1997) * t69 * t7
     #2 / 0.2880D4 - (0.90D2 * t27 * (t1996 - t207 * t1997) - 0.180D3 * 
     #t63 * t1997) * t69 * t73 / 0.2880D4
      t2023 = FJET(XB1, XB2, s, t176, -t178, -t173, t174, 0.0D0, t2022)
      t2025 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t2026 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t2043 = gbgbH52J3(s, XB1, XB2, z, lh, wd, t128, x2, 0.10D1, x4)
      t2052 = -(0.90D2 * t27 * (t2025 - t136 * t2026) - 0.180D3 * t63 * 
     #t2026) * t69 * t73 / 0.2880D4 + (-0.180D3 * t63 * (-t2025 + t151 *
     # t2026) + 0.90D2 * t27 * (-t156 * t2026 / 0.2D1 + t151 * t2025 - t
     #2043) - t102 * t2026) * t71 * t72 / 0.2880D4
      t2053 = FJET(XB1, XB2, s, -t117, 0.0D0, t121, t118, -t127, t2052)
      t2055 = t1304 * t1303 - t1322 * t1318 * t1026 / 0.2880D4 + t1388 *
     # t1387 + t1508 * t1507 + t1574 * t1573 + t1603 * t1602 - t1621 * t
     #1617 * t1026 / 0.2880D4 + t1687 * t1686 + t1716 * t1715 - t1734 * 
     #t1730 * t1026 / 0.2880D4 + t1862 * t1861 + t1900 * t1899 + t1930 *
     # t1929 + t1994 * t1993 + t2023 * t2022 + t2053 * t2052
      gbgbH5n2e1 = t1244 + t2055

      end function



      doubleprecision function gbgbH5n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = -t4
      t9 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t18 = x1 * z
      t19 = 0.1D1 - x1 + t18
      t20 = 0.1D1 / t19
      t21 = t17 * t20
      t22 = t4 ** 2
      t23 = t21 * t22
      t26 = log(0.4D1 * t15 * t23)
      t27 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t32 = t7 * lh
      t34 = 0.180D3 * t32 * t27
      t36 = 0.1D1 / x3
      t38 = 0.1D1 / x1
      t42 = 0.1D1 / x2
      t44 = t36 * t42 * t38
      t47 = x2 * t10
      t48 = t47 * t14
      t51 = log(0.4D1 * t48 * t23)
      t60 = t10 * t14
      t63 = log(0.4D1 * t60 * t23)
      t68 = t63 ** 2
      t75 = lh ** 2
      t76 = 0.180D3 * t75
      t77 = 0.3141592653589793D1 ** 2
      t78 = 0.30D2 * t77
      t79 = t76 - t78
      t80 = t7 * t79
      t85 = (0.90D2 * t7 * (t9 - t26 * t27) - t34) * t36 * t38 / 0.2880D
     #4 + t7 * t27 * t44 / 0.32D2 + (0.90D2 * t7 * (t9 - t51 * t27) - t3
     #4) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 * (-t9 + t63 * t27) + 
     #0.90D2 * t7 * (-t68 * t27 / 0.2D1 + t63 * t9) - t80 * t27) * t38 /
     # 0.2880D4
      t86 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t85)
      t90 = t2 * t4 * x2 * t20
      t91 = -0.1D1 + x2
      t92 = t91 * s
      t93 = t1 * t4
      t94 = t92 * t93
      t95 = t1 ** 2
      t100 = s * t95 * x2 * t4 * x1 * t20
      t101 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t105 = t91 ** 2
      t110 = log(0.4D1 * t48 * t21 * t22 * t105)
      t112 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t122 = -t7 * t101 * t44 / 0.32D2 + (0.90D2 * t7 * (t110 * t101 - t
     #112) + 0.180D3 * t32 * t101) * t42 * t38 / 0.2880D4
      t123 = FJET(XB1, XB2, s, 0.0D0, -t90, t3, t94, -t100, t122)
      t125 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t126 = t17 * t14
      t129 = log(0.4D1 * t11 * t126)
      t130 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t136 = 0.180D3 * t32 * t130
      t146 = log(0.4D1 * t47 * t126)
      t157 = log(0.4D1 * t60 * t17)
      t162 = t157 ** 2
      t169 = t80 * t130
      t173 = x2 * x3
      t176 = log(0.4D1 * t173 * t126)
      t185 = x2 * t14
      t188 = log(0.4D1 * t185 * t17)
      t194 = t188 ** 2
      t203 = x3 * t14
      t206 = log(0.4D1 * t203 * t17)
      t211 = t206 ** 2
      t222 = log(0.4D1 * t126)
      t224 = t222 ** 2
      t234 = (-t222 * t79 - 0.90D2 * t224 * lh - 0.2884936567583026D3 - 
     #0.120D3 * t75 * lh + 0.60D2 * lh * t77 - 0.15D2 * t224 * t222) * t
     #7
      t241 = (0.180D3 * t222 * lh + t76 - t78 + 0.45D2 * t224) * t7
      t244 = (0.90D2 * t7 * (-t125 + t129 * t130) + t136) * t36 * t38 / 
     #0.2880D4 - t7 * t130 * t44 / 0.32D2 + (0.90D2 * t7 * (t146 * t130 
     #- t125) + t136) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 * (t125 -
     # t157 * t130) + 0.90D2 * t7 * (t162 * t130 / 0.2D1 - t157 * t125) 
     #+ t169) * t38 / 0.2880D4 - (0.90D2 * t7 * (t125 - t176 * t130) - t
     #136) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (-t125 + t188 * t1
     #30) + 0.90D2 * t7 * (t188 * t125 - t194 * t130 / 0.2D1) - t169) * 
     #t42 / 0.5760D4 + (-0.180D3 * t32 * (-t125 + t206 * t130) + 0.90D2 
     #* t7 * (-t211 * t130 / 0.2D1 + t206 * t125) - t169) * t36 / 0.5760
     #D4 - t234 * t130 / 0.5760D4 - t241 * t125 / 0.5760D4
      t245 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t244)
      t247 = t2 * x3
      t248 = -0.1D1 + x3
      t249 = t2 * t248
      t250 = t126 * t248
      t253 = log(-0.4D1 * t173 * t250)
      t254 = -t248
      t255 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t257 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t262 = 0.180D3 * t32 * t255
      t269 = log(-0.4D1 * t11 * t250)
      t284 = log(-0.4D1 * t203 * t17 * t248)
      t289 = t284 ** 2
      t300 = -(0.90D2 * t7 * (t253 * t255 - t257) + t262) * t36 * t42 / 
     #0.5760D4 + (0.90D2 * t7 * (-t269 * t255 + t257) - t262) * t36 * t3
     #8 / 0.2880D4 + t7 * t255 * t44 / 0.32D2 + (-0.180D3 * t32 * (-t284
     # * t255 + t257) + 0.90D2 * t7 * (t289 * t255 / 0.2D1 - t284 * t257
     #) + t80 * t255) * t36 / 0.5760D4
      t301 = FJET(XB1, XB2, s, 0.0D0, t247, 0.0D0, -t249, 0.0D0, t300)
      t303 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t307 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t318 = -t7 * t303 * t44 / 0.32D2 + (0.90D2 * t7 * (-t307 + t110 * 
     #t303) + 0.180D3 * t32 * t303) * t42 * t38 / 0.2880D4
      t319 = FJET(XB1, XB2, s, -t90, 0.0D0, t94, t3, -t100, t318)
      t322 = x2 * s * t1
      t323 = t92 * t1
      t324 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t328 = t126 * t105
      t331 = log(0.4D1 * t47 * t328)
      t333 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t338 = 0.180D3 * t32 * t324
      t345 = log(0.4D1 * t173 * t328)
      t354 = t17 * t105
      t357 = log(0.4D1 * t185 * t354)
      t362 = t357 ** 2
      t373 = t7 * t324 * t44 / 0.32D2 + (0.90D2 * t7 * (-t331 * t324 + t
     #333) - t338) * t42 * t38 / 0.2880D4 - (0.90D2 * t7 * (t345 * t324 
     #- t333) + t338) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (-t357 
     #* t324 + t333) + 0.90D2 * t7 * (t362 * t324 / 0.2D1 - t357 * t333)
     # + t80 * t324) * t42 / 0.5760D4
      t374 = FJET(XB1, XB2, s, 0.0D0, t322, 0.0D0, -t323, 0.0D0, t373)
      t376 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t381 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t391 = -t7 * t376 * t44 / 0.32D2 + (0.90D2 * t7 * (t110 * t376 - t
     #381) + 0.180D3 * t32 * t376) * t42 * t38 / 0.2880D4
      t392 = FJET(XB1, XB2, s, t3, t94, 0.0D0, -t90, -t100, t391)
      t394 = 0.3D1 * t173
      t395 = x2 ** 2
      t396 = t395 * x3
      t397 = cos(t12)
      t399 = Sqrt(-t173 * t248)
      t400 = t397 * t399
      t401 = 0.2D1 * t400
      t403 = 0.2D1 * t400 * x2
      t405 = t173 - 0.1D1
      t406 = 0.1D1 / t405
      t408 = t2 * (-x2 - x3 + t394 - t396 - t401 + t403) * t406
      t412 = t2 * t91 * (-t173 - 0.1D1 + x3 + t401) * t406
      t413 = x2 * z
      t414 = 0.1D1 + t413 - x2
      t415 = t7 * t414
      t416 = t248 * t406
      t417 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t419 = t396 * z
      t420 = t173 * z
      t421 = 0.2D1 * t173
      t425 = 0.1D1 / (-t419 + t396 - t413 + t420 + x2 - t421 - t403 + 0.
     #2D1 * t400 * t413 - 0.1D1 + t401)
      t427 = t42 * t38
      t428 = t425 * t36 * t427
      t431 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t434 = t405 ** 2
      t440 = log(-0.4D1 * t173 * t14 * t354 * t248 / t434)
      t441 = t440 * t414
      t455 = t415 * t417 * t428 / 0.32D2 - (-0.90D2 * t7 * (t414 * t431 
     #- t441 * t417) * t425 + 0.180D3 * t32 * t414 * t417 * t425) * t36 
     #* t42 / 0.5760D4
      t456 = FJET(XB1, XB2, s, t408, 0.0D0, -t412, 0.0D0, 0.0D0, t455)
      t458 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t459 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t465 = 0.180D3 * t32 * t459
      t485 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t496 = (0.90D2 * t7 * (t458 - t269 * t459) - t465) * t36 * t38 / 0
     #.2880D4 + t7 * t459 * t44 / 0.32D2 - (0.90D2 * t7 * (t253 * t459 -
     # t458) + t465) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (-t284 *
     # t459 + t458) + 0.90D2 * t7 * (t485 + t289 * t459 / 0.2D1 - t284 *
     # t458) + t80 * t459) * t36 / 0.5760D4
      t497 = FJET(XB1, XB2, s, 0.0D0, -t249, 0.0D0, t247, 0.0D0, t496)
      t499 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t500 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t506 = 0.180D3 * t32 * t500
      t527 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t533 = t80 * t500
      t578 = (-0.180D3 * lh - 0.90D2 * t222) * t7
      t581 = (0.90D2 * t7 * (-t499 + t129 * t500) + t506) * t36 * t38 / 
     #0.2880D4 - t7 * t500 * t44 / 0.32D2 + (0.90D2 * t7 * (-t499 + t146
     # * t500) + t506) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 * (t499 
     #- t157 * t500) + 0.90D2 * t7 * (-t157 * t499 + t527 + t162 * t500 
     #/ 0.2D1) + t533) * t38 / 0.2880D4 - t234 * t500 / 0.5760D4 + (-0.1
     #80D3 * t32 * (-t499 + t206 * t500) + 0.90D2 * t7 * (t206 * t499 - 
     #t527 - t211 * t500 / 0.2D1) - t533) * t36 / 0.5760D4 - t241 * t499
     # / 0.5760D4 - (0.90D2 * t7 * (t499 - t176 * t500) - t506) * t36 * 
     #t42 / 0.5760D4 + (-0.180D3 * t32 * (-t499 + t188 * t500) + 0.90D2 
     #* t7 * (-t194 * t500 / 0.2D1 + t188 * t499 - t527) - t533) * t42 /
     # 0.5760D4 - t578 * t527 / 0.5760D4
      t582 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t581)
      t584 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t588 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t603 = t415 * t584 * t428 / 0.32D2 - (-0.90D2 * t7 * (t414 * t588 
     #- t441 * t584) * t425 + 0.180D3 * t32 * t414 * t584 * t425) * t36 
     #* t42 / 0.5760D4
      t604 = FJET(XB1, XB2, s, -t412, 0.0D0, t408, 0.0D0, 0.0D0, t603)
      t606 = x1 * x3
      t610 = Sqrt(-x3 * t19 * x2 * t248)
      t611 = t397 * t610
      t612 = 0.2D1 * t611
      t613 = t396 * x1
      t614 = t606 * z
      t615 = t173 * x1
      t618 = 0.2D1 * t611 * x2
      t619 = t396 * t18
      t620 = t173 * t18
      t622 = t606 - t396 + t394 - t612 - x2 - x3 + t613 - t614 - 0.2D1 *
     # t615 + t618 - t619 + 0.2D1 * t620
      t625 = t5 * t622 * t20 * t406
      t628 = t3 * x3 * t91 * t406
      t633 = t5 * t91 * (-t173 - 0.1D1 + x3 + x1 - t606 - t18 + t614 + t
     #612) * t20 * t406
      t634 = t248 * s
      t635 = t1 * x1
      t637 = t634 * t635 * t406
      t638 = t7 * t19
      t639 = x2 * x1
      t640 = t639 * z
      t641 = -0.1D1 + x2 - t639 - t413 + t640 - t18 + x1
      t642 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t8, x2, t416, x4)
      t662 = 0.1D1 - t619 + 0.4D1 * t620 - x3 * t16 * t639 - 0.2D1 * t11
     # * t413 + t11 * t16 * x2 - 0.2D1 * t611 * t413 - 0.2D1 * t611 * t6
     #39 - 0.2D1 * t611 * t18 - x2 - 0.2D1 * x1 + 0.2D1 * t611 * t640 + 
     #t10 + t613 - 0.3D1 * t615 + t618
      t676 = -0.3D1 * t640 + t11 * x2 + t639 * t16 + 0.2D1 * t47 * z - t
     #47 * t16 + 0.2D1 * t611 * x1 + t419 - t420 + t421 - t396 + t413 - 
     #t612 + 0.2D1 * t639 - 0.2D1 * t10 * z + t10 * t16 - t47 + 0.2D1 * 
     #t18
      t678 = 0.1D1 / (t662 + t676)
      t680 = t678 * t36 * t427
      t683 = FJET(XB1, XB2, s, -t625, t628, t633, t637, -t100, -t638 * t
     #641 * t642 * t680 / 0.32D2)
      t685 = t19 * t641
      t691 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t8, x2, t416, x4)
      t696 = FJET(XB1, XB2, s, t637, t633, t628, -t625, -t100, -t638 * t
     #641 * t691 * t680 / 0.32D2)
      t703 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t8, x2, t416, x4)
      t708 = FJET(XB1, XB2, s, t628, -t625, t637, t633, -t100, -t638 * t
     #641 * t703 * t680 / 0.32D2)
      t715 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t717 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t722 = 0.180D3 * t32 * t715
      t745 = gbgbH52J3(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t753 = (0.90D2 * t7 * (-t26 * t715 + t717) - t722) * t36 * t38 / 0
     #.2880D4 + t7 * t715 * t44 / 0.32D2 + (0.90D2 * t7 * (-t51 * t715 +
     # t717) - t722) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 * (t63 * t
     #715 - t717) + 0.90D2 * t7 * (-t68 * t715 / 0.2D1 + t63 * t717 - t7
     #45) - t80 * t715) * t38 / 0.2880D4
      t754 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t753)
      t756 = t2 * t606
      t758 = t2 * t4 * x3
      t759 = t634 * t635
      t760 = t634 * t93
      t761 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t766 = log(-0.4D1 * t15 * t21 * t22 * t248)
      t767 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t781 = (0.90D2 * t7 * (-t761 + t766 * t767) + 0.180D3 * t32 * t767
     #) * t36 * t38 / 0.2880D4 - t7 * t767 * t44 / 0.32D2
      t782 = FJET(XB1, XB2, s, t756, -t758, -t759, t760, 0.0D0, t781)
      t784 = t86 * t85 + t123 * t122 + t245 * t244 + t301 * t300 + t319 
     #* t318 + t374 * t373 + t392 * t391 + t456 * t455 + t497 * t496 + t
     #582 * t581 + t604 * t603 - t683 * t7 * t685 * t642 * t678 * t44 / 
     #0.32D2 - t696 * t7 * t685 * t691 * t678 * t44 / 0.32D2 - t708 * t7
     # * t685 * t703 * t678 * t44 / 0.32D2 + t754 * t753 + t782 * t781
      t785 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t786 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t800 = (0.90D2 * t7 * (-t785 + t766 * t786) + 0.180D3 * t32 * t786
     #) * t36 * t38 / 0.2880D4 - t7 * t786 * t44 / 0.32D2
      t801 = FJET(XB1, XB2, s, -t759, t760, t756, -t758, 0.0D0, t800)
      t803 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t807 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t822 = t415 * t803 * t428 / 0.32D2 - (-0.90D2 * t7 * (t414 * t807 
     #- t441 * t803) * t425 + 0.180D3 * t32 * t414 * t803 * t425) * t36 
     #* t42 / 0.5760D4
      t823 = FJET(XB1, XB2, s, 0.0D0, -t412, 0.0D0, t408, 0.0D0, t822)
      t825 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t829 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t835 = 0.180D3 * t32 * t825
      t862 = t7 * t825 * t44 / 0.32D2 + (0.90D2 * t7 * (t829 - t331 * t8
     #25) - t835) * t42 * t38 / 0.2880D4 - (0.90D2 * t7 * (t345 * t825 -
     # t829) + t835) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (-t357 *
     # t825 + t829) + 0.90D2 * t7 * (t362 * t825 / 0.2D1 - t357 * t829) 
     #+ t80 * t825) * t42 / 0.5760D4
      t863 = FJET(XB1, XB2, s, -t323, 0.0D0, t322, 0.0D0, 0.0D0, t862)
      t865 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t8, x2, t416, x4)
      t870 = FJET(XB1, XB2, s, t633, t637, -t625, t628, -t100, -t638 * t
     #641 * t865 * t680 / 0.32D2)
      t877 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t878 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t884 = 0.180D3 * t32 * t878
      t904 = gbgbH54J3(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t915 = (0.90D2 * t7 * (t877 - t26 * t878) - t884) * t36 * t38 / 0.
     #2880D4 + t7 * t878 * t44 / 0.32D2 + (0.90D2 * t7 * (-t51 * t878 + 
     #t877) - t884) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 * (-t877 + 
     #t63 * t878) + 0.90D2 * t7 * (-t904 + t63 * t877 - t68 * t878 / 0.2
     #D1) - t80 * t878) * t38 / 0.2880D4
      t916 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t915)
      t918 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t922 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t416, x4)
      t937 = t415 * t918 * t428 / 0.32D2 - (-0.90D2 * t7 * (t414 * t922 
     #- t441 * t918) * t425 + 0.180D3 * t32 * t414 * t918 * t425) * t36 
     #* t42 / 0.5760D4
      t938 = FJET(XB1, XB2, s, 0.0D0, t408, 0.0D0, -t412, 0.0D0, t937)
      t940 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t941 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t955 = (0.90D2 * t7 * (-t940 + t766 * t941) + 0.180D3 * t32 * t941
     #) * t36 * t38 / 0.2880D4 - t7 * t941 * t44 / 0.32D2
      t956 = FJET(XB1, XB2, s, t760, -t759, -t758, t756, 0.0D0, t955)
      t958 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t959 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t965 = 0.180D3 * t32 * t959
      t985 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t996 = (0.90D2 * t7 * (t958 - t269 * t959) - t965) * t36 * t38 / 0
     #.2880D4 + t7 * t959 * t44 / 0.32D2 - (0.90D2 * t7 * (-t958 + t253 
     #* t959) + t965) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (t958 -
     # t284 * t959) + 0.90D2 * t7 * (t985 + t289 * t959 / 0.2D1 - t284 *
     # t958) + t80 * t959) * t36 / 0.5760D4
      t997 = FJET(XB1, XB2, s, t247, 0.0D0, -t249, 0.0D0, 0.0D0, t996)
      t999 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1004 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1009 = 0.180D3 * t32 * t999
      t1026 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1037 = t7 * t999 * t44 / 0.32D2 + (0.90D2 * t7 * (-t331 * t999 + 
     #t1004) - t1009) * t42 * t38 / 0.2880D4 - (0.90D2 * t7 * (-t1004 + 
     #t345 * t999) + t1009) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (
     #-t357 * t999 + t1004) + 0.90D2 * t7 * (t1026 + t362 * t999 / 0.2D1
     # - t357 * t1004) + t80 * t999) * t42 / 0.5760D4
      t1038 = FJET(XB1, XB2, s, t322, 0.0D0, -t323, 0.0D0, 0.0D0, t1037)
      t1040 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1045 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1050 = 0.180D3 * t32 * t1040
      t1067 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1078 = t7 * t1040 * t44 / 0.32D2 + (0.90D2 * t7 * (-t331 * t1040 
     #+ t1045) - t1050) * t42 * t38 / 0.2880D4 - (0.90D2 * t7 * (t345 * 
     #t1040 - t1045) + t1050) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 *
     # (t1045 - t357 * t1040) + 0.90D2 * t7 * (t1067 + t362 * t1040 / 0.
     #2D1 - t357 * t1045) + t80 * t1040) * t42 / 0.5760D4
      t1079 = FJET(XB1, XB2, s, 0.0D0, -t323, 0.0D0, t322, 0.0D0, t1078)
      t1081 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t1082 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t1088 = 0.180D3 * t32 * t1082
      t1118 = (0.90D2 * t7 * (t1081 - t26 * t1082) - t1088) * t36 * t38 
     #/ 0.2880D4 + t7 * t1082 * t44 / 0.32D2 + (0.90D2 * t7 * (-t51 * t1
     #082 + t1081) - t1088) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 * (
     #-t1081 + t63 * t1082) + 0.90D2 * t7 * (t63 * t1081 - t68 * t1082 /
     # 0.2D1) - t80 * t1082) * t38 / 0.2880D4
      t1119 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t1118)
      t1121 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t1126 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t1136 = -t7 * t1121 * t44 / 0.32D2 + (0.90D2 * t7 * (t110 * t1121 
     #- t1126) + 0.180D3 * t32 * t1121) * t42 * t38 / 0.2880D4
      t1137 = FJET(XB1, XB2, s, t94, t3, -t90, 0.0D0, -t100, t1136)
      t1139 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1140 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1146 = 0.180D3 * t32 * t1140
      t1166 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1173 = t80 * t1140
      t1217 = (0.90D2 * t7 * (-t1139 + t129 * t1140) + t1146) * t36 * t3
     #8 / 0.2880D4 - t7 * t1140 * t44 / 0.32D2 + (0.90D2 * t7 * (t146 * 
     #t1140 - t1139) + t1146) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 *
     # (t1139 - t157 * t1140) + 0.90D2 * t7 * (t1166 + t162 * t1140 / 0.
     #2D1 - t157 * t1139) + t1173) * t38 / 0.2880D4 - t241 * t1139 / 0.5
     #760D4 - (0.90D2 * t7 * (-t176 * t1140 + t1139) - t1146) * t36 * t4
     #2 / 0.5760D4 + (-0.180D3 * t32 * (-t1139 + t188 * t1140) + 0.90D2 
     #* t7 * (t188 * t1139 - t1166 - t194 * t1140 / 0.2D1) - t1173) * t4
     #2 / 0.5760D4 - t234 * t1140 / 0.5760D4 + (-0.180D3 * t32 * (t206 *
     # t1140 - t1139) + 0.90D2 * t7 * (-t211 * t1140 / 0.2D1 - t1166 + t
     #206 * t1139) - t1173) * t36 / 0.5760D4 - t578 * t1166 / 0.5760D4
      t1218 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1217)
      t1220 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t1222 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t254, x4)
      t1227 = 0.180D3 * t32 * t1220
      t1257 = (0.90D2 * t7 * (-t269 * t1220 + t1222) - t1227) * t36 * t3
     #8 / 0.2880D4 + t7 * t1220 * t44 / 0.32D2 - (0.90D2 * t7 * (t253 * 
     #t1220 - t1222) + t1227) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 *
     # (t1222 - t284 * t1220) + 0.90D2 * t7 * (t289 * t1220 / 0.2D1 - t2
     #84 * t1222) + t80 * t1220) * t36 / 0.5760D4
      t1258 = FJET(XB1, XB2, s, -t249, 0.0D0, t247, 0.0D0, 0.0D0, t1257)
      t1260 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t1261 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t254, x4)
      t1275 = (0.90D2 * t7 * (-t1260 + t766 * t1261) + 0.180D3 * t32 * t
     #1261) * t36 * t38 / 0.2880D4 - t7 * t1261 * t44 / 0.32D2
      t1276 = FJET(XB1, XB2, s, -t758, t756, t760, -t759, 0.0D0, t1275)
      t1278 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1280 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1285 = 0.180D3 * t32 * t1278
      t1311 = t80 * t1278
      t1353 = (0.90D2 * t7 * (t129 * t1278 - t1280) + t1285) * t36 * t38
     # / 0.2880D4 - t7 * t1278 * t44 / 0.32D2 + (0.90D2 * t7 * (-t1280 +
     # t146 * t1278) + t1285) * t42 * t38 / 0.2880D4 - (-0.180D3 * t32 *
     # (-t157 * t1278 + t1280) + 0.90D2 * t7 * (-t157 * t1280 + t162 * t
     #1278 / 0.2D1) + t1311) * t38 / 0.2880D4 + (-0.180D3 * t32 * (t206 
     #* t1278 - t1280) + 0.90D2 * t7 * (t206 * t1280 - t211 * t1278 / 0.
     #2D1) - t1311) * t36 / 0.5760D4 - t241 * t1280 / 0.5760D4 - (0.90D2
     # * t7 * (-t176 * t1278 + t1280) - t1285) * t36 * t42 / 0.5760D4 + 
     #(-0.180D3 * t32 * (-t1280 + t188 * t1278) + 0.90D2 * t7 * (t188 * 
     #t1280 - t194 * t1278 / 0.2D1) - t1311) * t42 / 0.5760D4 - t234 * t
     #1278 / 0.5760D4
      t1354 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1353)
      t1356 = t801 * t800 + t823 * t822 + t863 * t862 - t870 * t7 * t685
     # * t865 * t678 * t44 / 0.32D2 + t916 * t915 + t938 * t937 + t956 *
     # t955 + t997 * t996 + t1038 * t1037 + t1079 * t1078 + t1119 * t111
     #8 + t1137 * t1136 + t1218 * t1217 + t1258 * t1257 + t1276 * t1275 
     #+ t1354 * t1353
      gbgbH5n2e0 = t784 + t1356

      end function



      doubleprecision function gbgbH5n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = z ** 2
      t5 = 0.1D1 / t4
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t11 = log(0.4D1 * t5 * t8)
      t14 = s ** 2
      t15 = 0.1D1 / t14
      t16 = (-0.180D3 * lh - 0.90D2 * t11) * t15
      t17 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t20 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t21 = t15 * t20
      t22 = 0.1D1 / x3
      t23 = 0.1D1 / x2
      t24 = t22 * t23
      t27 = x2 * t8
      t30 = log(0.4D1 * t27 * t5)
      t35 = t15 * lh
      t37 = 0.180D3 * t35 * t20
      t43 = lh ** 2
      t45 = 0.3141592653589793D1 ** 2
      t47 = t11 ** 2
      t50 = (0.180D3 * t11 * lh + 0.180D3 * t43 - 0.30D2 * t45 + 0.45D2 
     #* t47) * t15
      t53 = x1 ** 2
      t54 = t53 * t8
      t57 = log(0.4D1 * t54 * t5)
      t63 = 0.1D1 / x1
      t66 = t22 * t63
      t69 = t23 * t63
      t72 = x3 * t8
      t75 = log(0.4D1 * t72 * t5)
      t83 = -t16 * t17 / 0.5760D4 - t21 * t24 / 0.64D2 + (0.90D2 * t15 *
     # (-t17 + t30 * t20) + t37) * t23 / 0.5760D4 - t50 * t20 / 0.5760D4
     # - (0.90D2 * t15 * (-t57 * t20 + t17) - t37) * t63 / 0.2880D4 - t2
     #1 * t66 / 0.32D2 - t21 * t69 / 0.32D2 + (0.90D2 * t15 * (t75 * t20
     # - t17) + t37) * t22 / 0.5760D4
      t84 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t83)
      t87 = x2 * s * t1
      t88 = -0.1D1 + x2
      t89 = t88 * s
      t90 = t89 * t1
      t91 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t92 = t15 * t91
      t97 = t88 ** 2
      t101 = log(0.4D1 * t27 * t5 * t97)
      t103 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t112 = t92 * t69 / 0.32D2 + t92 * t24 / 0.64D2 + (0.90D2 * t15 * (
     #-t101 * t91 + t103) - 0.180D3 * t35 * t91) * t23 / 0.5760D4
      t113 = FJET(XB1, XB2, s, t87, 0.0D0, -t90, 0.0D0, 0.0D0, t112)
      t115 = t2 * x3
      t116 = -0.1D1 + x3
      t117 = t2 * t116
      t118 = -t116
      t119 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t120 = t15 * t119
      t123 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t127 = log(-0.4D1 * t72 * t5 * t116)
      t139 = t120 * t66 / 0.32D2 + (0.90D2 * t15 * (t123 - t127 * t119) 
     #- 0.180D3 * t35 * t119) * t22 / 0.5760D4 + t120 * t24 / 0.64D2
      t140 = FJET(XB1, XB2, s, t115, 0.0D0, -t117, 0.0D0, 0.0D0, t139)
      t142 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t143 = t15 * t142
      t149 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t158 = t143 * t69 / 0.32D2 + t143 * t24 / 0.64D2 + (0.90D2 * t15 *
     # (-t101 * t142 + t149) - 0.180D3 * t35 * t142) * t23 / 0.5760D4
      t159 = FJET(XB1, XB2, s, 0.0D0, t87, 0.0D0, -t90, 0.0D0, t158)
      t161 = t2 * x1
      t162 = -0.1D1 + x1
      t163 = t2 * t162
      t164 = -t162
      t165 = gbgbH54J2(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t168 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t170 = t162 ** 2
      t174 = log(0.4D1 * t54 * t5 * t168 * t170)
      t175 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t185 = t15 * t175
      t190 = -(0.90D2 * t15 * (-t165 + t174 * t175) + 0.180D3 * t35 * t1
     #75) * t63 / 0.2880D4 + t185 * t66 / 0.32D2 + t185 * t69 / 0.32D2
      t191 = FJET(XB1, XB2, s, t161, -t163, 0.0D0, 0.0D0, 0.0D0, t190)
      t193 = x2 * x3
      t195 = x2 ** 2
      t196 = t195 * x3
      t197 = cos(t6)
      t199 = Sqrt(-t193 * t116)
      t200 = t197 * t199
      t201 = 0.2D1 * t200
      t203 = 0.2D1 * t200 * x2
      t206 = 0.1D1 / (t193 - 0.1D1)
      t208 = t2 * (-x2 - x3 + 0.3D1 * t193 - t196 - t201 + t203) * t206
      t212 = t2 * t88 * (-t193 - 0.1D1 + x3 + t201) * t206
      t213 = x2 * z
      t214 = 0.1D1 + t213 - x2
      t215 = t15 * t214
      t216 = t116 * t206
      t217 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t216, x4)
      t225 = 0.1D1 / (-t196 * z + t196 - t213 + t193 * z + x2 - 0.2D1 * 
     #t193 - t203 + 0.2D1 * t200 * t213 - 0.1D1 + t201)
      t227 = t225 * t22 * t23
      t230 = FJET(XB1, XB2, s, 0.0D0, t208, 0.0D0, -t212, 0.0D0, t215 * 
     #t217 * t227 / 0.64D2)
      t237 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t216, x4)
      t241 = FJET(XB1, XB2, s, -t212, 0.0D0, t208, 0.0D0, 0.0D0, t215 * 
     #t237 * t227 / 0.64D2)
      t248 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t216, x4)
      t252 = FJET(XB1, XB2, s, t208, 0.0D0, -t212, 0.0D0, 0.0D0, t215 * 
     #t248 * t227 / 0.64D2)
      t259 = gbgbH53J2(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t260 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t270 = t15 * t260
      t275 = -(0.90D2 * t15 * (-t259 + t174 * t260) + 0.180D3 * t35 * t2
     #60) * t63 / 0.2880D4 + t270 * t66 / 0.32D2 + t270 * t69 / 0.32D2
      t276 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t161, -t163, 0.0D0, t275)
      t278 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t279 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t285 = 0.180D3 * t35 * t279
      t289 = t15 * t279
      t307 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t317 = -(0.90D2 * t15 * (t278 - t57 * t279) - t285) * t63 / 0.2880
     #D4 - t289 * t66 / 0.32D2 - t289 * t69 / 0.32D2 - t16 * t278 / 0.57
     #60D4 - t289 * t24 / 0.64D2 + (0.90D2 * t15 * (-t278 + t30 * t279) 
     #+ t285) * t23 / 0.5760D4 - t50 * t279 / 0.5760D4 - t15 * t307 / 0.
     #64D2 + (0.90D2 * t15 * (-t278 + t75 * t279) + t285) * t22 / 0.5760
     #D4
      t318 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t317)
      t320 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t321 = t15 * t320
      t326 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t336 = t321 * t69 / 0.32D2 + t321 * t24 / 0.64D2 + (0.90D2 * t15 *
     # (t326 - t101 * t320) - 0.180D3 * t35 * t320) * t23 / 0.5760D4
      t337 = FJET(XB1, XB2, s, 0.0D0, -t90, 0.0D0, t87, 0.0D0, t336)
      t339 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t216, x4)
      t343 = FJET(XB1, XB2, s, 0.0D0, -t212, 0.0D0, t208, 0.0D0, t215 * 
     #t339 * t227 / 0.64D2)
      t350 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t351 = t15 * t350
      t357 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t366 = t351 * t66 / 0.32D2 + t351 * t24 / 0.64D2 + (0.90D2 * t15 *
     # (-t127 * t350 + t357) - 0.180D3 * t35 * t350) * t22 / 0.5760D4
      t367 = FJET(XB1, XB2, s, 0.0D0, t115, 0.0D0, -t117, 0.0D0, t366)
      t369 = gbgbH51J2(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t370 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t380 = t15 * t370
      t385 = -(0.90D2 * t15 * (-t369 + t174 * t370) + 0.180D3 * t35 * t3
     #70) * t63 / 0.2880D4 + t380 * t66 / 0.32D2 + t380 * t69 / 0.32D2
      t386 = FJET(XB1, XB2, s, -t163, t161, 0.0D0, 0.0D0, 0.0D0, t385)
      t388 = t84 * t83 + t113 * t112 + t140 * t139 + t159 * t158 + t191 
     #* t190 + t230 * t15 * t214 * t217 * t225 * t24 / 0.64D2 + t241 * t
     #15 * t214 * t237 * t225 * t24 / 0.64D2 + t252 * t15 * t214 * t248 
     #* t225 * t24 / 0.64D2 + t276 * t275 + t318 * t317 + t337 * t336 + 
     #t343 * t15 * t214 * t339 * t225 * t24 / 0.64D2 + t367 * t366 + t38
     #6 * t385
      t389 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t390 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t396 = 0.180D3 * t35 * t390
      t400 = t15 * t390
      t425 = -(0.90D2 * t15 * (t389 - t57 * t390) - t396) * t63 / 0.2880
     #D4 - t400 * t66 / 0.32D2 - t400 * t69 / 0.32D2 + (0.90D2 * t15 * (
     #-t389 + t75 * t390) + t396) * t22 / 0.5760D4 - t16 * t389 / 0.5760
     #D4 - t50 * t390 / 0.5760D4 - t400 * t24 / 0.64D2 + (0.90D2 * t15 *
     # (-t389 + t30 * t390) + t396) * t23 / 0.5760D4
      t426 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t425)
      t428 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t429 = t15 * t428
      t435 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t444 = t429 * t69 / 0.32D2 + t429 * t24 / 0.64D2 + (0.90D2 * t15 *
     # (-t101 * t428 + t435) - 0.180D3 * t35 * t428) * t23 / 0.5760D4
      t445 = FJET(XB1, XB2, s, -t90, 0.0D0, t87, 0.0D0, 0.0D0, t444)
      t447 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t448 = t15 * t447
      t454 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t463 = t448 * t66 / 0.32D2 + t448 * t24 / 0.64D2 + (0.90D2 * t15 *
     # (-t127 * t447 + t454) - 0.180D3 * t35 * t447) * t22 / 0.5760D4
      t464 = FJET(XB1, XB2, s, 0.0D0, -t117, 0.0D0, t115, 0.0D0, t463)
      t466 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t469 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t472 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t475 = t15 * t472
      t483 = 0.180D3 * t35 * t472
      t505 = -t15 * t466 / 0.64D2 - t16 * t469 / 0.5760D4 - t50 * t472 /
     # 0.5760D4 - t475 * t24 / 0.64D2 + (0.90D2 * t15 * (-t469 + t30 * t
     #472) + t483) * t23 / 0.5760D4 - (0.90D2 * t15 * (t469 - t57 * t472
     #) - t483) * t63 / 0.2880D4 - t475 * t66 / 0.32D2 - t475 * t69 / 0.
     #32D2 + (0.90D2 * t15 * (t75 * t472 - t469) + t483) * t22 / 0.5760D
     #4
      t506 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t505)
      t508 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t509 = t15 * t508
      t512 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t118, x4)
      t524 = t509 * t66 / 0.32D2 + (0.90D2 * t15 * (t512 - t127 * t508) 
     #- 0.180D3 * t35 * t508) * t22 / 0.5760D4 + t509 * t24 / 0.64D2
      t525 = FJET(XB1, XB2, s, -t117, 0.0D0, t115, 0.0D0, 0.0D0, t524)
      t529 = t2 * t162 * x2 * t168
      t530 = t1 * t162
      t531 = t89 * t530
      t532 = t1 ** 2
      t537 = s * t532 * x2 * t162 * x1 * t168
      t538 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t164, x2, 0.10D1, x4)
      t542 = FJET(XB1, XB2, s, 0.0D0, -t529, t161, t531, -t537, -t15 * t
     #538 * t69 / 0.32D2)
      t548 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t164, x2, 0.10D1, x4)
      t552 = FJET(XB1, XB2, s, -t529, 0.0D0, t531, t161, -t537, -t15 * t
     #548 * t69 / 0.32D2)
      t559 = t2 * t162 * x3
      t561 = t2 * x1 * x3
      t562 = t116 * s
      t563 = t562 * t530
      t565 = t562 * t1 * x1
      t566 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, t118, x4)
      t570 = FJET(XB1, XB2, s, -t559, t561, t563, -t565, 0.0D0, -t15 * t
     #566 * t66 / 0.32D2)
      t576 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, t118, x4)
      t580 = FJET(XB1, XB2, s, t561, -t559, -t565, t563, 0.0D0, -t15 * t
     #576 * t66 / 0.32D2)
      t586 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, t118, x4)
      t590 = FJET(XB1, XB2, s, t563, -t565, -t559, t561, 0.0D0, -t15 * t
     #586 * t66 / 0.32D2)
      t596 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, t118, x4)
      t600 = FJET(XB1, XB2, s, -t565, t563, t561, -t559, 0.0D0, -t15 * t
     #596 * t66 / 0.32D2)
      t606 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t164, x2, 0.10D1, x4)
      t610 = FJET(XB1, XB2, s, t531, t161, -t529, 0.0D0, -t537, -t15 * t
     #606 * t69 / 0.32D2)
      t616 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t618 = gbgbH52J2(s, XB1, XB2, z, lh, wd, t164, 0.0D0, 0.10D1, x4)
      t627 = t15 * t616
      t632 = -(0.90D2 * t15 * (t174 * t616 - t618) + 0.180D3 * t35 * t61
     #6) * t63 / 0.2880D4 + t627 * t66 / 0.32D2 + t627 * t69 / 0.32D2
      t633 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t163, t161, 0.0D0, t632)
      t635 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t164, x2, 0.10D1, x4)
      t639 = FJET(XB1, XB2, s, t161, t531, 0.0D0, -t529, -t537, -t15 * t
     #635 * t69 / 0.32D2)
      t645 = t426 * t425 + t445 * t444 + t464 * t463 + t506 * t505 + t52
     #5 * t524 - t542 * t15 * t538 * t23 * t63 / 0.32D2 - t552 * t15 * t
     #548 * t23 * t63 / 0.32D2 - t570 * t15 * t566 * t22 * t63 / 0.32D2 
     #- t580 * t15 * t576 * t22 * t63 / 0.32D2 - t590 * t15 * t586 * t22
     # * t63 / 0.32D2 - t600 * t15 * t596 * t22 * t63 / 0.32D2 - t610 * 
     #t15 * t606 * t23 * t63 / 0.32D2 + t633 * t632 - t639 * t15 * t635 
     #* t23 * t63 / 0.32D2
      gbgbH5n2em1 = t388 + t645

      end function



      doubleprecision function gbgbH5n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t17 = z ** 2
      t20 = Sin(x4 * 0.3141592653589793D1)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t17 * t21)
      t27 = (-0.180D3 * lh - 0.90D2 * t24) * t4
      t30 = 0.1D1 / x3
      t33 = -t6 * t7 / 0.32D2 - t6 * t10 / 0.64D2 - t4 * t13 / 0.64D2 - 
     #t27 * t5 / 0.5760D4 - t6 * t30 / 0.64D2
      t34 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t33)
      t36 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t37 = t4 * t36
      t42 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t49 = -t37 * t7 / 0.32D2 - t37 * t10 / 0.64D2 - t4 * t42 / 0.64D2 
     #- t27 * t36 / 0.5760D4 - t37 * t30 / 0.64D2
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t52 = t2 * x1
      t53 = -0.1D1 + x1
      t54 = t2 * t53
      t55 = -t53
      t56 = gbgbH53J1(s, XB1, XB2, z, lh, wd, t55, 0.0D0, 0.10D1, x4)
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t52, -t54, 0.0D0, t4 * t56 *
     # t7 / 0.32D2)
      t65 = gbgbH52J1(s, XB1, XB2, z, lh, wd, t55, 0.0D0, 0.10D1, x4)
      t69 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t54, t52, 0.0D0, t4 * t65 *
     # t7 / 0.32D2)
      t74 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t75 = t4 * t74
      t84 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t87 = -t75 * t7 / 0.32D2 - t27 * t74 / 0.5760D4 - t75 * t30 / 0.64
     #D2 - t75 * t10 / 0.64D2 - t4 * t84 / 0.64D2
      t88 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t87)
      t90 = t2 * x3
      t91 = -0.1D1 + x3
      t92 = t2 * t91
      t93 = -t91
      t94 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t93, x4)
      t98 = FJET(XB1, XB2, s, 0.0D0, t90, 0.0D0, -t92, 0.0D0, t4 * t94 *
     # t30 / 0.64D2)
      t104 = x2 * s * t1
      t107 = (-0.1D1 + x2) * s * t1
      t108 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t112 = FJET(XB1, XB2, s, 0.0D0, t104, 0.0D0, -t107, 0.0D0, t4 * t1
     #08 * t10 / 0.64D2)
      t117 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t93, x4)
      t121 = FJET(XB1, XB2, s, 0.0D0, -t92, 0.0D0, t90, 0.0D0, t4 * t117
     # * t30 / 0.64D2)
      t126 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t130 = FJET(XB1, XB2, s, 0.0D0, -t107, 0.0D0, t104, 0.0D0, t4 * t1
     #26 * t10 / 0.64D2)
      t135 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t138 = t4 * t135
      t145 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t148 = -t27 * t135 / 0.5760D4 - t138 * t30 / 0.64D2 - t138 * t7 / 
     #0.32D2 - t138 * t10 / 0.64D2 - t4 * t145 / 0.64D2
      t149 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t148)
      t151 = gbgbH54J1(s, XB1, XB2, z, lh, wd, t55, 0.0D0, 0.10D1, x4)
      t155 = FJET(XB1, XB2, s, t52, -t54, 0.0D0, 0.0D0, 0.0D0, t4 * t151
     # * t7 / 0.32D2)
      t160 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t93, x4)
      t164 = FJET(XB1, XB2, s, t90, 0.0D0, -t92, 0.0D0, 0.0D0, t4 * t160
     # * t30 / 0.64D2)
      t169 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t173 = FJET(XB1, XB2, s, t104, 0.0D0, -t107, 0.0D0, 0.0D0, t4 * t1
     #69 * t10 / 0.64D2)
      t178 = gbgbH51J1(s, XB1, XB2, z, lh, wd, t55, 0.0D0, 0.10D1, x4)
      t182 = FJET(XB1, XB2, s, -t54, t52, 0.0D0, 0.0D0, 0.0D0, t4 * t178
     # * t7 / 0.32D2)
      t187 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t191 = FJET(XB1, XB2, s, -t107, 0.0D0, t104, 0.0D0, 0.0D0, t4 * t1
     #87 * t10 / 0.64D2)
      t196 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t93, x4)
      t200 = FJET(XB1, XB2, s, -t92, 0.0D0, t90, 0.0D0, 0.0D0, t4 * t196
     # * t30 / 0.64D2)
      gbgbH5n2em2 = t34 * t33 + t50 * t49 + t60 * t4 * t56 * t7 / 0.32D2
     # + t69 * t4 * t65 * t7 / 0.32D2 + t88 * t87 + t98 * t4 * t94 * t30
     # / 0.64D2 + t112 * t4 * t108 * t10 / 0.64D2 + t121 * t4 * t117 * t
     #30 / 0.64D2 + t130 * t4 * t126 * t10 / 0.64D2 + t149 * t148 + t155
     # * t4 * t151 * t7 / 0.32D2 + t164 * t4 * t160 * t30 / 0.64D2 + t17
     #3 * t4 * t169 * t10 / 0.64D2 + t182 * t4 * t178 * t7 / 0.32D2 + t1
     #91 * t4 * t187 * t10 / 0.64D2 + t200 * t4 * t196 * t30 / 0.64D2

      end function



      doubleprecision function gbgbH5n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t4 * t5 / 
     #0.64D2)
      t11 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t4 * t11 
     #/ 0.64D2)
      t17 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t4 * t17 
     #/ 0.64D2)
      t23 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t4 * t23 
     #/ 0.64D2)
      gbgbH5n2em3 = -t8 * t4 * t5 / 0.64D2 - t14 * t4 * t11 / 0.64D2 - t
     #20 * t4 * t17 / 0.64D2 - t26 * t4 * t23 / 0.64D2

      end function



      doubleprecision function gbgbH5n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      gbgbH5n2em4 = 0.0D0

      end function


      doubleprecision function gbgbH5n3e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = t10 * t13
      t16 = log(0.4D1 * t14)
      t17 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t19 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t20 = t16 ** 2
      t21 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t27 = lh ** 2
      t29 = 0.3141592653589793D1 ** 2
      t31 = 0.180D3 * t27 - 0.30D2 * t29
      t32 = t4 * t31
      t39 = t20 * t16
      t49 = -0.2884936567583026D3 - 0.120D3 * t27 * lh + 0.60D2 * lh * t
     #29
      t50 = t4 * t49
      t53 = 0.1D1 / x1
      t56 = z * t17
      t57 = t6 * x3
      t58 = t9 * t13
      t59 = -0.1D1 + x3
      t60 = 0.1D1 / t59
      t61 = t58 * t60
      t64 = log(-0.4D1 * t57 * t61)
      t65 = t64 * z
      t68 = cos(t7)
      t69 = x3 * z
      t71 = Sqrt(-t69 * t59)
      t75 = 0.1D1 / (-z - x3 + 0.2D1 * t68 * t71)
      t79 = log(0.4D1 * t57 * t58)
      t85 = t79 ** 2
      t88 = z * t19
      t90 = t64 ** 2
      t91 = t90 * z
      t100 = z * t21 * t75
      t104 = 0.1D1 / x3
      t108 = 0.1D1 - x2
      t109 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t110 = x2 ** 2
      t111 = x3 * t110
      t114 = log(0.4D1 * t111 * t14)
      t116 = t111 * t6
      t117 = -t108
      t118 = t58 * t117
      t121 = log(-0.4D1 * t116 * t118)
      t122 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t126 = log(-0.4D1 * t116 * t61)
      t127 = t126 * z
      t134 = -t100 + t122 - t21
      t139 = 0.1D1 / x2
      t140 = t139 * t53
      t143 = t110 * t6
      t146 = log(-0.4D1 * t143 * t118)
      t150 = log(0.4D1 * t143 * t58)
      t155 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t158 = t146 ** 2
      t161 = t150 ** 2
      t167 = -t21 + t122
      t174 = log(0.4D1 * t58)
      t175 = t174 ** 2
      t178 = t175 * t174
      t181 = t175 ** 2
      t188 = t29 ** 2
      t189 = t27 ** 2
      t193 = 0.5769873135166051D3 * lh + t188 + 0.60D2 * t189 - 0.60D2 *
     # t27 * t29
      t216 = x3 * t13
      t219 = log(0.4D1 * t216 * t9)
      t220 = t219 ** 2
      t224 = log(-0.4D1 * t216 * t9 * t60)
      t225 = t224 ** 2
      t229 = -t220 / 0.2D1 - t225 * z * t75 / 0.2D1
      t239 = t224 * z * t75 + t219
      t247 = t220 * t219 / 0.6D1 + t225 * t224 * z * t75 / 0.6D1
      t257 = -z * t75 - 0.1D1
      t262 = t13 * t110
      t263 = t9 * t117
      t266 = log(-0.4D1 * t262 * t263)
      t267 = t266 ** 2
      t273 = log(0.4D1 * t262 * t9)
      t275 = t273 ** 2
      t288 = t267 * t266
      t294 = t275 * t273
      t306 = log(-0.4D1 * t111 * t118)
      t310 = log(0.4D1 * t111 * t58)
      t314 = log(-0.4D1 * t111 * t61)
      t315 = t314 * z
      t322 = t310 ** 2
      t327 = t306 ** 2
      t331 = t314 ** 2
      t332 = t331 * z
      t345 = -(-0.180D3 * t5 * (-t16 * t17 + t19 + t20 * t21 / 0.2D1) + 
     #t32 * (t17 - t16 * t21) + 0.90D2 * t4 * (-t16 * t19 + t20 * t17 / 
     #0.2D1 - t39 * t21 / 0.6D1) + t50 * t21) * t53 / 0.5760D4 + (-0.180
     #D3 * t5 * (-(t56 - t65 * t21) * t75 - t17 + t79 * t21) + 0.90D2 * 
     #t4 * (t79 * t17 - t19 - t85 * t21 / 0.2D1 - (t88 - t65 * t17 + t91
     # * t21 / 0.2D1) * t75) + t32 * (-t21 - t100)) * t104 * t53 / 0.576
     #0D4 + (0.90D2 * t4 * (t109 + t114 * t21 - t121 * t122 - t17 - (t56
     # - t127 * t21) * t75) - 0.180D3 * t5 * t134) * t104 * t140 / 0.288
     #0D4 + (-0.180D3 * t5 * (-t146 * t122 - t17 + t109 + t150 * t21) + 
     #0.90D2 * t4 * (t155 - t19 - t146 * t109 + t150 * t17 + t158 * t122
     # / 0.2D1 - t161 * t21 / 0.2D1) + t32 * t167) * t139 * t53 / 0.2880
     #D4 - (0.45D2 * t175 * t19 - 0.15D2 * t178 * t17 + 0.15D2 / 0.4D1 *
     # t181 * t21 + (t17 - t174 * t21) * t49 + t21 * t193 + (-t174 * t17
     # + t19 + t175 * t21 / 0.2D1) * t31 - 0.180D3 * (-t174 * t19 + t175
     # * t17 / 0.2D1 - t178 * t21 / 0.6D1) * lh) * t4 / 0.11520D5 + ((-0
     #.180D3 * t21 * lh + 0.90D2 * t17) * t4 * t229 + (t21 * t31 + 0.90D
     #2 * t19 - 0.180D3 * t17 * lh) * t4 * t239 + 0.90D2 * t21 * t4 * t2
     #47 + (t17 * t31 - 0.180D3 * t19 * lh + t21 * t49) * t4 * t257) * t
     #104 / 0.11520D5 + (-0.180D3 * t5 * (t267 * t122 / 0.2D1 - t266 * t
     #109 + t155 + t273 * t17 - t19 - t275 * t21 / 0.2D1) + t32 * (t273 
     #* t21 - t17 + t109 - t266 * t122) + 0.90D2 * t4 * (-t266 * t155 + 
     #t267 * t109 / 0.2D1 - t288 * t122 / 0.6D1 + t273 * t19 - t275 * t1
     #7 / 0.2D1 + t294 * t21 / 0.6D1) + t50 * t167) * t139 / 0.5760D4 + 
     #(-0.180D3 * t5 * (-t306 * t122 + t109 - t17 + t310 * t21 - (t56 - 
     #t315 * t21) * t75) + 0.90D2 * t4 * (-t322 * t21 / 0.2D1 + t310 * t
     #17 - t306 * t109 + t155 + t327 * t122 / 0.2D1 - t19 - (t88 - t315 
     #* t17 + t332 * t21 / 0.2D1) * t75) + t32 * t134) * t104 * t139 / 0
     #.5760D4
      t346 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t345)
      t348 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t351 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t370 = z * t351
      t390 = z * t348 * t75
      t397 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t401 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t407 = -t348 - t390 + t401
      t428 = t401 - t348
      t535 = -(-0.180D3 * t5 * (t20 * t348 / 0.2D1 - t16 * t351) + t32 *
     # (t351 - t16 * t348) + 0.90D2 * t4 * (t20 * t351 / 0.2D1 - t39 * t
     #348 / 0.6D1) + t50 * t348) * t53 / 0.5760D4 + (-0.180D3 * t5 * (-t
     #351 - (t370 - t65 * t348) * t75 + t79 * t348) + 0.90D2 * t4 * (t79
     # * t351 - t85 * t348 / 0.2D1 - (-t65 * t351 + t91 * t348 / 0.2D1) 
     #* t75) + t32 * (-t390 - t348)) * t104 * t53 / 0.5760D4 + (0.90D2 *
     # t4 * (t397 - t351 - (t370 - t127 * t348) * t75 - t121 * t401 + t1
     #14 * t348) - 0.180D3 * t5 * t407) * t104 * t140 / 0.2880D4 + (-0.1
     #80D3 * t5 * (-t146 * t401 + t397 + t150 * t348 - t351) + 0.90D2 * 
     #t4 * (t158 * t401 / 0.2D1 - t161 * t348 / 0.2D1 + t150 * t351 - t1
     #46 * t397) + t32 * t428) * t139 * t53 / 0.2880D4 - (-0.15D2 * t178
     # * t351 + 0.15D2 / 0.4D1 * t181 * t348 + (t351 - t174 * t348) * t4
     #9 + t348 * t193 + (t175 * t348 / 0.2D1 - t174 * t351) * t31 - 0.18
     #0D3 * (t175 * t351 / 0.2D1 - t178 * t348 / 0.6D1) * lh) * t4 / 0.1
     #1520D5 + ((-0.180D3 * t348 * lh + 0.90D2 * t351) * t4 * t229 + (-0
     #.180D3 * t351 * lh + t348 * t31) * t4 * t239 + 0.90D2 * t348 * t4 
     #* t247 + (t351 * t31 + t348 * t49) * t4 * t257) * t104 / 0.11520D5
     # + (-0.180D3 * t5 * (t267 * t401 / 0.2D1 + t273 * t351 - t266 * t3
     #97 - t275 * t348 / 0.2D1) + t32 * (t273 * t348 - t351 + t397 - t26
     #6 * t401) + 0.90D2 * t4 * (t267 * t397 / 0.2D1 - t275 * t351 / 0.2
     #D1 - t288 * t401 / 0.6D1 + t294 * t348 / 0.6D1) + t50 * t428) * t1
     #39 / 0.5760D4 + (-0.180D3 * t5 * (-t306 * t401 - t351 + t310 * t34
     #8 + t397 - (t370 - t315 * t348) * t75) + 0.90D2 * t4 * (-(-t315 * 
     #t351 + t332 * t348 / 0.2D1) * t75 - t322 * t348 / 0.2D1 + t327 * t
     #401 / 0.2D1 - t306 * t397 + t310 * t351) + t32 * t407) * t104 * t1
     #39 / 0.5760D4
      t536 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t535)
      t538 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t541 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t560 = z * t541
      t580 = z * t538 * t75
      t587 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t589 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t597 = t589 - t580 - t538
      t618 = t589 - t538
      t725 = -(-0.180D3 * t5 * (t20 * t538 / 0.2D1 - t16 * t541) + t32 *
     # (t541 - t16 * t538) + 0.90D2 * t4 * (t20 * t541 / 0.2D1 - t39 * t
     #538 / 0.6D1) + t50 * t538) * t53 / 0.5760D4 + (-0.180D3 * t5 * (-(
     #t560 - t65 * t538) * t75 + t79 * t538 - t541) + 0.90D2 * t4 * (-(-
     #t65 * t541 + t91 * t538 / 0.2D1) * t75 - t85 * t538 / 0.2D1 + t79 
     #* t541) + t32 * (-t580 - t538)) * t104 * t53 / 0.5760D4 + (0.90D2 
     #* t4 * (t587 + t114 * t538 - t121 * t589 - (t560 - t127 * t538) * 
     #t75 - t541) - 0.180D3 * t5 * t597) * t104 * t140 / 0.2880D4 + (-0.
     #180D3 * t5 * (t587 - t146 * t589 - t541 + t150 * t538) + 0.90D2 * 
     #t4 * (t158 * t589 / 0.2D1 + t150 * t541 - t161 * t538 / 0.2D1 - t1
     #46 * t587) + t32 * t618) * t139 * t53 / 0.2880D4 - (-0.15D2 * t178
     # * t541 + 0.15D2 / 0.4D1 * t181 * t538 + (t541 - t174 * t538) * t4
     #9 + t538 * t193 + (t175 * t538 / 0.2D1 - t174 * t541) * t31 - 0.18
     #0D3 * (t175 * t541 / 0.2D1 - t178 * t538 / 0.6D1) * lh) * t4 / 0.1
     #1520D5 + ((-0.180D3 * t538 * lh + 0.90D2 * t541) * t4 * t229 + (-0
     #.180D3 * t541 * lh + t538 * t31) * t4 * t239 + 0.90D2 * t538 * t4 
     #* t247 + (t541 * t31 + t538 * t49) * t4 * t257) * t104 / 0.11520D5
     # + (-0.180D3 * t5 * (t267 * t589 / 0.2D1 - t266 * t587 - t275 * t5
     #38 / 0.2D1 + t273 * t541) + t32 * (-t541 + t587 + t273 * t538 - t2
     #66 * t589) + 0.90D2 * t4 * (t267 * t587 / 0.2D1 - t275 * t541 / 0.
     #2D1 - t288 * t589 / 0.6D1 + t294 * t538 / 0.6D1) + t50 * t618) * t
     #139 / 0.5760D4 + (-0.180D3 * t5 * (t587 - t541 - t306 * t589 - (t5
     #60 - t315 * t538) * t75 + t310 * t538) + 0.90D2 * t4 * (-(-t315 * 
     #t541 + t332 * t538 / 0.2D1) * t75 + t310 * t541 - t322 * t538 / 0.
     #2D1 + t327 * t589 / 0.2D1 - t306 * t587) + t32 * t597) * t104 * t1
     #39 / 0.5760D4
      t726 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t725)
      t728 = x2 * x3
      t729 = 0.1D1 - x3 + t728
      t730 = 0.1D1 / t729
      t731 = t728 * t730
      t732 = t2 * t731
      t733 = t59 * t730
      t734 = t2 * t733
      t735 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t736 = z * t735
      t737 = t111 * t10
      t739 = t729 ** 2
      t740 = 0.1D1 / t739
      t741 = t59 * t740
      t745 = log(0.4D1 * t737 * t13 * t117 * t741)
      t746 = t745 * z
      t747 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t751 = t117 * t59
      t753 = Sqrt(t69 * t751)
      t757 = 0.1D1 / (-z - x3 + t728 + 0.2D1 * t68 * t753)
      t761 = z * t747 * t757
      t772 = log(0.4D1 * t111 * t13 * t263 * t741)
      t773 = t772 * z
      t779 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t782 = t772 ** 2
      t783 = t782 * z
      t795 = (0.90D2 * t4 * (t736 - t746 * t747) * t757 - 0.180D3 * t5 *
     # t761) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (t736 - t773 * 
     #t747) * t757 + 0.90D2 * t4 * (z * t779 - t773 * t735 + t783 * t747
     # / 0.2D1) * t757 + t32 * t761) * t104 * t139 / 0.5760D4
      t796 = FJET(XB1, XB2, s, 0.0D0, t732, 0.0D0, -t734, 0.0D0, t795)
      t799 = t1 * x1
      t800 = x1 * z
      t801 = -z - x1 + t800
      t802 = 0.1D1 / t801
      t804 = t117 * s * t799 * t802
      t805 = -0.1D1 + x1
      t806 = t2 * t805
      t808 = x2 * s * t799
      t809 = t1 ** 2
      t810 = s * t809
      t813 = x1 * t805 * t802
      t814 = t810 * t117 * t813
      t815 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t816 = 0.1D1 / t11
      t817 = t816 * t802
      t818 = t805 ** 2
      t820 = t817 * t818 * t117
      t823 = log(0.4D1 * t737 * t820)
      t824 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t834 = t143 * t9
      t837 = log(0.4D1 * t834 * t820)
      t842 = t837 ** 2
      t854 = (0.90D2 * t4 * (-t815 + t823 * t824) + 0.180D3 * t5 * t824)
     # * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (-t815 + t837 * t824)
     # + 0.90D2 * t4 * (-t842 * t824 / 0.2D1 + t837 * t815) - t32 * t824
     #) * t139 * t53 / 0.2880D4
      t855 = FJET(XB1, XB2, s, 0.0D0, t804, -t806, t808, -t814, t854)
      t858 = t2 * x1 * t802
      t859 = t810 * t813
      t860 = t817 * t818
      t863 = log(-0.4D1 * t10 * t860)
      t864 = t863 ** 2
      t865 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t868 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t876 = t864 * t863
      t888 = z * t801
      t889 = t888 * t868
      t890 = t57 * t9
      t892 = t817 * t818 * t60
      t895 = log(0.4D1 * t890 * t892)
      t896 = t895 * z
      t897 = t801 * t865
      t900 = x1 * x3
      t901 = t900 * z
      t903 = 0.2D1 * t57 * z
      t904 = x1 * t11
      t905 = x3 * t11
      t906 = t905 * x1
      t907 = t57 * t11
      t908 = x3 * t801
      t910 = Sqrt(t908 * t59)
      t915 = 0.1D1 / (-t800 - t901 - t69 - t57 + t903 + t904 + t906 - t9
     #07 + 0.2D1 * t68 * t910 * z - t11)
      t919 = log(-0.4D1 * t890 * t860)
      t926 = t895 ** 2
      t927 = t926 * z
      t933 = t919 ** 2
      t941 = -t888 * t865 * t915 + t865
      t949 = log(0.4D1 * t737 * t892)
      t950 = t949 * z
      t955 = t802 * t818
      t959 = log(-0.4D1 * t116 * t9 * t816 * t955)
      t972 = log(-0.4D1 * t834 * t860)
      t978 = t972 ** 2
      t989 = -(-0.180D3 * t5 * (-t864 * t865 / 0.2D1 + t863 * t868) + t3
     #2 * (t863 * t865 - t868) + 0.90D2 * t4 * (t876 * t865 / 0.6D1 - t8
     #64 * t868 / 0.2D1) - t50 * t865) * t53 / 0.5760D4 + (-0.180D3 * t5
     # * (t868 - (t889 - t896 * t897) * t915 - t919 * t865) + 0.90D2 * t
     #4 * (-(-t896 * t801 * t868 + t927 * t897 / 0.2D1) * t915 - t919 * 
     #t868 + t933 * t865 / 0.2D1) + t32 * t941) * t104 * t53 / 0.5760D4 
     #+ (0.90D2 * t4 * (t868 - (t889 - t950 * t897) * t915 - t959 * t865
     #) - 0.180D3 * t5 * t941) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5
     # * (t868 - t972 * t865) + 0.90D2 * t4 * (-t972 * t868 + t978 * t86
     #5 / 0.2D1) + t32 * t865) * t139 * t53 / 0.2880D4
      t990 = FJET(XB1, XB2, s, 0.0D0, -t806, -t858, 0.0D0, t859, t989)
      t992 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t995 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1014 = t888 * t995
      t1015 = t801 * t992
      t1037 = t992 - t888 * t992 * t915
      t1071 = -(-0.180D3 * t5 * (-t864 * t992 / 0.2D1 + t863 * t995) + t
     #32 * (t863 * t992 - t995) + 0.90D2 * t4 * (t876 * t992 / 0.6D1 - t
     #864 * t995 / 0.2D1) - t50 * t992) * t53 / 0.5760D4 + (-0.180D3 * t
     #5 * (t995 - (t1014 - t896 * t1015) * t915 - t919 * t992) + 0.90D2 
     #* t4 * (t933 * t992 / 0.2D1 - (-t896 * t801 * t995 + t927 * t1015 
     #/ 0.2D1) * t915 - t919 * t995) + t32 * t1037) * t104 * t53 / 0.576
     #0D4 + (0.90D2 * t4 * (-(t1014 - t950 * t1015) * t915 + t995 - t959
     # * t992) - 0.180D3 * t5 * t1037) * t104 * t140 / 0.2880D4 + (-0.18
     #0D3 * t5 * (-t972 * t992 + t995) + 0.90D2 * t4 * (-t972 * t995 + t
     #978 * t992 / 0.2D1) + t32 * t992) * t139 * t53 / 0.2880D4
      t1072 = FJET(XB1, XB2, s, 0.0D0, -t858, -t806, 0.0D0, t859, t1071)
      t1074 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t1075 = z * t1074
      t1076 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t1083 = z * t1076 * t757
      t1107 = (0.90D2 * t4 * (t1075 - t746 * t1076) * t757 - 0.180D3 * t
     #5 * t1083) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (t1075 - t7
     #73 * t1076) * t757 + 0.90D2 * t4 * (-t773 * t1074 + t783 * t1076 /
     # 0.2D1) * t757 + t32 * t1083) * t104 * t139 / 0.5760D4
      t1108 = FJET(XB1, XB2, s, 0.0D0, -t734, 0.0D0, t732, 0.0D0, t1107)
      t1110 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x
     #4)
      t1113 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x
     #4)
      t1114 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x
     #4)
      t1135 = z * t1114
      t1145 = z * t1113
      t1155 = z * t1110 * t75
      t1163 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t1167 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t1172 = -t1155 + t1167 - t1110
      t1184 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, t108, 0.10D1, x4)
      t1194 = t1167 - t1110
      t1309 = -(-0.180D3 * t5 * (t20 * t1110 / 0.2D1 + t1113 - t16 * t11
     #14) + t32 * (-t16 * t1110 + t1114) + 0.90D2 * t4 * (-t16 * t1113 +
     # t20 * t1114 / 0.2D1 - t39 * t1110 / 0.6D1) + t50 * t1110) * t53 /
     # 0.5760D4 + (-0.180D3 * t5 * (t79 * t1110 - t1114 - (t1135 - t65 *
     # t1110) * t75) + 0.90D2 * t4 * (t79 * t1114 - t1113 - t85 * t1110 
     #/ 0.2D1 - (t1145 - t65 * t1114 + t91 * t1110 / 0.2D1) * t75) + t32
     # * (-t1155 - t1110)) * t104 * t53 / 0.5760D4 + (0.90D2 * t4 * (t11
     #4 * t1110 + t1163 - (t1135 - t127 * t1110) * t75 - t121 * t1167 - 
     #t1114) - 0.180D3 * t5 * t1172) * t104 * t140 / 0.2880D4 + (-0.180D
     #3 * t5 * (t1163 - t1114 + t150 * t1110 - t146 * t1167) + 0.90D2 * 
     #t4 * (-t1113 + t1184 - t146 * t1163 + t150 * t1114 - t161 * t1110 
     #/ 0.2D1 + t158 * t1167 / 0.2D1) + t32 * t1194) * t139 * t53 / 0.28
     #80D4 - (0.45D2 * t175 * t1113 - 0.15D2 * t178 * t1114 + 0.15D2 / 0
     #.4D1 * t181 * t1110 + (t1114 - t174 * t1110) * t49 + t1110 * t193 
     #+ (t175 * t1110 / 0.2D1 + t1113 - t174 * t1114) * t31 - 0.180D3 * 
     #(-t174 * t1113 + t175 * t1114 / 0.2D1 - t178 * t1110 / 0.6D1) * lh
     #) * t4 / 0.11520D5 + ((-0.180D3 * t1110 * lh + 0.90D2 * t1114) * t
     #4 * t229 + (-0.180D3 * t1114 * lh + 0.90D2 * t1113 + t1110 * t31) 
     #* t4 * t239 + 0.90D2 * t1110 * t4 * t247 + (t1114 * t31 - 0.180D3 
     #* t1113 * lh + t1110 * t49) * t4 * t257) * t104 / 0.11520D5 + (-0.
     #180D3 * t5 * (t267 * t1167 / 0.2D1 - t1113 + t1184 - t266 * t1163 
     #- t275 * t1110 / 0.2D1 + t273 * t1114) + t32 * (-t1114 + t1163 + t
     #273 * t1110 - t266 * t1167) + 0.90D2 * t4 * (-t266 * t1184 + t267 
     #* t1163 / 0.2D1 - t275 * t1114 / 0.2D1 - t288 * t1167 / 0.6D1 + t2
     #94 * t1110 / 0.6D1 + t273 * t1113) + t50 * t1194) * t139 / 0.5760D
     #4 + (-0.180D3 * t5 * (t1163 - t306 * t1167 - (t1135 - t315 * t1110
     #) * t75 + t310 * t1110 - t1114) + 0.90D2 * t4 * (-(t1145 - t315 * 
     #t1114 + t332 * t1110 / 0.2D1) * t75 + t1184 - t1113 + t327 * t1167
     # / 0.2D1 - t306 * t1163 - t322 * t1110 / 0.2D1 + t310 * t1114) + t
     #32 * t1172) * t104 * t139 / 0.5760D4
      t1310 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1309)
      t1312 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1314 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1338 = (0.90D2 * t4 * (t823 * t1312 - t1314) + 0.180D3 * t5 * t13
     #12) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (-t1314 + t837 * t
     #1312) + 0.90D2 * t4 * (t837 * t1314 - t842 * t1312 / 0.2D1) - t32 
     #* t1312) * t139 * t53 / 0.2880D4
      t1339 = FJET(XB1, XB2, s, t808, -t806, t804, 0.0D0, -t814, t1338)
      t1341 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t1342 = z * t1341
      t1343 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t1350 = z * t1343 * t757
      t1374 = (0.90D2 * t4 * (t1342 - t746 * t1343) * t757 - 0.180D3 * t
     #5 * t1350) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (t1342 - t7
     #73 * t1343) * t757 + 0.90D2 * t4 * (-t773 * t1341 + t783 * t1343 /
     # 0.2D1) * t757 + t32 * t1350) * t104 * t139 / 0.5760D4
      t1375 = FJET(XB1, XB2, s, t732, 0.0D0, -t734, 0.0D0, 0.0D0, t1374)
      t1377 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1378 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1395 = gbgbH52J3(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1404 = (0.90D2 * t4 * (-t1377 + t823 * t1378) + 0.180D3 * t5 * t1
     #378) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (t837 * t1378 - t
     #1377) + 0.90D2 * t4 * (t837 * t1377 - t842 * t1378 / 0.2D1 - t1395
     #) - t32 * t1378) * t139 * t53 / 0.2880D4
      t1405 = FJET(XB1, XB2, s, t804, 0.0D0, t808, -t806, -t814, t1404)
      t1410 = t59 * s * t1 * t805 * t730
      t1411 = t2 * x1
      t1413 = Sqrt(-t908 * t751)
      t1414 = t68 * t1413
      t1420 = t1411 * x2 * (-x3 + t728 - z + t69 - x1 + t900 + t800 - t9
     #01 + 0.2D1 * t1414) * t802 * t730
      t1421 = t806 * t731
      t1424 = t111 * x1
      t1426 = t111 * t800
      t1430 = t1411 * (-x2 + t728 + 0.2D1 * t1414 * x2 + 0.1D1 - x3 + t1
     #424 + t111 * z - t1426) * t802 * t730
      t1431 = x2 * x1
      t1432 = t1431 * z
      t1433 = -z - t1431 + t1432
      t1434 = t801 * t1433
      t1435 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1443 = log(-0.4D1 * t111 * t10 * t816 * t955 * t751 * t740)
      t1444 = t1443 * t801
      t1445 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1446 = t1433 * t1445
      t1452 = x2 * t6
      t1468 = t728 * x1 - t1431 * t11 - 0.2D1 * t1452 * z + t1452 * t11 
     #+ t905 * t1431 + 0.2D1 * t57 * x2 * z - t57 * t11 * x2 - 0.2D1 * t
     #728 * t800 - t1424 + t1432 - 0.2D1 * t1414 * z - t57 * x2 - t728 *
     # z
      t1473 = -0.2D1 * t1414 * t1431 + t901 - t903 - t906 + t907 + t800 
     #+ 0.2D1 * t1414 * t1432 + t11 + t69 + t57 - t904 + t1452 + t1426
      t1475 = 0.1D1 / (t1468 + t1473)
      t1478 = t5 * t801
      t1482 = 0.90D2 * t4 * (t1434 * t1435 - t1444 * t1446) * t1475 - 0.
     #180D3 * t1478 * t1446 * t1475
      t1486 = FJET(XB1, XB2, s, t1410, t1420, -t1421, -t1430, -t814, t14
     #82 * t104 * t140 / 0.2880D4)
      t1489 = t104 * t139 * t53
      t1492 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1494 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1495 = t1433 * t1494
      t1504 = 0.90D2 * t4 * (t1434 * t1492 - t1444 * t1495) * t1475 - 0.
     #180D3 * t1478 * t1495 * t1475
      t1508 = FJET(XB1, XB2, s, t1420, t1410, -t1430, -t1421, -t814, t15
     #04 * t104 * t140 / 0.2880D4)
      t1512 = gbgbH54J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1513 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1515 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1536 = t888 * t1513
      t1537 = t801 * t1515
      t1560 = -t888 * t1515 * t915 + t1515
      t1594 = -(-0.180D3 * t5 * (-t1512 + t863 * t1513 - t864 * t1515 / 
     #0.2D1) + t32 * (t863 * t1515 - t1513) + 0.90D2 * t4 * (t863 * t151
     #2 - t864 * t1513 / 0.2D1 + t876 * t1515 / 0.6D1) - t50 * t1515) * 
     #t53 / 0.5760D4 + (-0.180D3 * t5 * (-(t1536 - t896 * t1537) * t915 
     #+ t1513 - t919 * t1515) + 0.90D2 * t4 * (-(t888 * t1512 - t896 * t
     #801 * t1513 + t927 * t1537 / 0.2D1) * t915 - t919 * t1513 + t1512 
     #+ t933 * t1515 / 0.2D1) + t32 * t1560) * t104 * t53 / 0.5760D4 + (
     #0.90D2 * t4 * (-t959 * t1515 + t1513 - (t1536 - t950 * t1537) * t9
     #15) - 0.180D3 * t5 * t1560) * t104 * t140 / 0.2880D4 + (-0.180D3 *
     # t5 * (-t972 * t1515 + t1513) + 0.90D2 * t4 * (t978 * t1515 / 0.2D
     #1 - t972 * t1513 + t1512) + t32 * t1515) * t139 * t53 / 0.2880D4
      t1595 = FJET(XB1, XB2, s, -t806, 0.0D0, 0.0D0, -t858, t859, t1594)
      t1597 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1599 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1615 = gbgbH54J3(s, XB1, XB2, z, lh, wd, x1, t108, 0.10D1, x4)
      t1624 = (0.90D2 * t4 * (t823 * t1597 - t1599) + 0.180D3 * t5 * t15
     #97) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (-t1599 + t837 * t
     #1597) + 0.90D2 * t4 * (t837 * t1599 - t842 * t1597 / 0.2D1 - t1615
     #) - t32 * t1597) * t139 * t53 / 0.2880D4
      t1625 = FJET(XB1, XB2, s, -t806, t808, 0.0D0, t804, -t814, t1624)
      t1627 = gbgbH52J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1628 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1630 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1652 = t888 * t1628
      t1653 = t801 * t1630
      t1675 = -t888 * t1630 * t915 + t1630
      t1709 = -(-0.180D3 * t5 * (-t1627 + t863 * t1628 - t864 * t1630 / 
     #0.2D1) + t32 * (t863 * t1630 - t1628) + 0.90D2 * t4 * (t863 * t162
     #7 - t864 * t1628 / 0.2D1 + t876 * t1630 / 0.6D1) - t50 * t1630) * 
     #t53 / 0.5760D4 + (-0.180D3 * t5 * (-t919 * t1630 - (t1652 - t896 *
     # t1653) * t915 + t1628) + 0.90D2 * t4 * (-(t888 * t1627 - t896 * t
     #801 * t1628 + t927 * t1653 / 0.2D1) * t915 - t919 * t1628 + t1627 
     #+ t933 * t1630 / 0.2D1) + t32 * t1675) * t104 * t53 / 0.5760D4 + (
     #0.90D2 * t4 * (t1628 - (t1652 - t950 * t1653) * t915 - t959 * t163
     #0) - 0.180D3 * t5 * t1675) * t104 * t140 / 0.2880D4 + (-0.180D3 * 
     #t5 * (-t972 * t1630 + t1628) + 0.90D2 * t4 * (t978 * t1630 / 0.2D1
     # - t972 * t1628 + t1627) + t32 * t1630) * t139 * t53 / 0.2880D4
      t1710 = FJET(XB1, XB2, s, -t858, 0.0D0, 0.0D0, -t806, t859, t1709)
      t1712 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t1713 = z * t1712
      t1714 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t1721 = z * t1714 * t757
      t1733 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, t108, -t733, x4)
      t1747 = (0.90D2 * t4 * (t1713 - t746 * t1714) * t757 - 0.180D3 * t
     #5 * t1721) * t104 * t140 / 0.2880D4 + (-0.180D3 * t5 * (t1713 - t7
     #73 * t1714) * t757 + 0.90D2 * t4 * (z * t1733 - t773 * t1712 + t78
     #3 * t1714 / 0.2D1) * t757 + t32 * t1721) * t104 * t139 / 0.5760D4
      t1748 = FJET(XB1, XB2, s, -t734, 0.0D0, t732, 0.0D0, 0.0D0, t1747)
      t1750 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1752 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1753 = t1433 * t1752
      t1762 = 0.90D2 * t4 * (t1434 * t1750 - t1444 * t1753) * t1475 - 0.
     #180D3 * t1478 * t1753 * t1475
      t1766 = FJET(XB1, XB2, s, -t1430, -t1421, t1420, t1410, -t814, t17
     #62 * t104 * t140 / 0.2880D4)
      t1770 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1772 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, t108, -t733, x4)
      t1773 = t1433 * t1772
      t1782 = 0.90D2 * t4 * (t1434 * t1770 - t1444 * t1773) * t1475 - 0.
     #180D3 * t1478 * t1773 * t1475
      t1786 = FJET(XB1, XB2, s, -t1421, -t1430, t1410, t1420, -t814, t17
     #82 * t104 * t140 / 0.2880D4)
      gbgbH5n3e1 = t346 * t345 + t536 * t535 + t726 * t725 + t796 * t795
     # + t855 * t854 + t990 * t989 + t1072 * t1071 + t1108 * t1107 + t13
     #10 * t1309 + t1339 * t1338 + t1375 * t1374 + t1405 * t1404 + t1486
     # * t1482 * t1489 / 0.2880D4 + t1508 * t1504 * t1489 / 0.2880D4 + t
     #1595 * t1594 + t1625 * t1624 + t1710 * t1709 + t1748 * t1747 + t17
     #66 * t1762 * t1489 / 0.2880D4 + t1786 * t1782 * t1489 / 0.2880D4

      end function



      doubleprecision function gbgbH5n3e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t6 = z * t5
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t16 = -0.1D1 + x3
      t17 = 0.1D1 / t16
      t18 = t15 * t17
      t21 = log(-0.4D1 * t8 * t18)
      t22 = t21 * z
      t23 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = cos(t9)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t16)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t8 * t15)
      t42 = t4 * lh
      t44 = z * t23 * t33
      t49 = 0.1D1 / x3
      t51 = 0.1D1 / x1
      t54 = 0.1D1 - x2
      t55 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t56 = -t44 + t55 - t23
      t58 = 0.1D1 / x2
      t60 = t49 * t58 * t51
      t63 = x2 ** 2
      t64 = t63 * t7
      t65 = -t54
      t66 = t15 * t65
      t69 = log(-0.4D1 * t64 * t66)
      t71 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t74 = log(0.4D1 * t64 * t15)
      t79 = -t23 + t55
      t86 = t7 * t11
      t89 = log(0.4D1 * t86 * t14)
      t95 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t96 = t89 ** 2
      t102 = lh ** 2
      t104 = 0.3141592653589793D1 ** 2
      t106 = 0.180D3 * t102 - 0.30D2 * t104
      t107 = t4 * t106
      t117 = x3 * t14
      t121 = log(-0.4D1 * t117 * t11 * t17)
      t126 = log(0.4D1 * t117 * t11)
      t127 = t121 * z * t33 + t126
      t130 = t126 ** 2
      t131 = t121 ** 2
      t135 = -t130 / 0.2D1 - t131 * z * t33 / 0.2D1
      t145 = -z * t33 - 0.1D1
      t150 = x3 * t63
      t153 = log(-0.4D1 * t150 * t66)
      t157 = log(0.4D1 * t150 * t15)
      t161 = log(-0.4D1 * t150 * t18)
      t162 = t161 * z
      t175 = t14 * t63
      t178 = log(0.4D1 * t175 * t11)
      t180 = t11 * t65
      t183 = log(-0.4D1 * t175 * t180)
      t188 = t183 ** 2
      t192 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t194 = t178 ** 2
      t205 = log(0.4D1 * t15)
      t210 = t205 ** 2
      t220 = -0.2884936567583026D3 - 0.120D3 * t102 * lh + 0.60D2 * lh *
     # t104
      t226 = t210 * t205
      t232 = (0.90D2 * t4 * (-(t6 - t22 * t23) * t33 - t5 + t37 * t23) -
     # 0.180D3 * t42 * (-t23 - t44)) * t49 * t51 / 0.5760D4 + t4 * t56 *
     # t60 / 0.32D2 + (0.90D2 * t4 * (-t69 * t55 - t5 + t71 + t74 * t23)
     # - 0.180D3 * t42 * t79) * t58 * t51 / 0.2880D4 - (-0.180D3 * t42 *
     # (t5 - t89 * t23) + 0.90D2 * t4 * (-t89 * t5 + t95 + t96 * t23 / 0
     #.2D1) + t107 * t23) * t51 / 0.5760D4 + ((-0.180D3 * t23 * lh + 0.9
     #0D2 * t5) * t4 * t127 + 0.90D2 * t23 * t4 * t135 + (t23 * t106 + 0
     #.90D2 * t95 - 0.180D3 * t5 * lh) * t4 * t145) * t49 / 0.11520D5 + 
     #(0.90D2 * t4 * (-t153 * t55 + t71 - t5 + t157 * t23 - (t6 - t162 *
     # t23) * t33) - 0.180D3 * t42 * t56) * t49 * t58 / 0.5760D4 + (-0.1
     #80D3 * t42 * (t178 * t23 - t5 + t71 - t183 * t55) + 0.90D2 * t4 * 
     #(t188 * t55 / 0.2D1 - t183 * t71 + t192 + t178 * t5 - t95 - t194 *
     # t23 / 0.2D1) + t107 * t79) * t58 / 0.5760D4 - ((t5 - t205 * t23) 
     #* t106 - 0.180D3 * (-t205 * t5 + t95 + t210 * t23 / 0.2D1) * lh + 
     #t23 * t220 - 0.90D2 * t205 * t95 + 0.45D2 * t210 * t5 - 0.15D2 * t
     #226 * t23) * t4 / 0.11520D5
      t233 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t232)
      t235 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t236 = z * t235
      t237 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t246 = z * t237 * t33
      t254 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t255 = -t237 - t246 + t254
      t260 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t265 = t254 - t237
      t353 = (0.90D2 * t4 * (-t235 - (t236 - t22 * t237) * t33 + t37 * t
     #237) - 0.180D3 * t42 * (-t246 - t237)) * t49 * t51 / 0.5760D4 + t4
     # * t255 * t60 / 0.32D2 + (0.90D2 * t4 * (-t69 * t254 + t260 + t74 
     #* t237 - t235) - 0.180D3 * t42 * t265) * t58 * t51 / 0.2880D4 - (-
     #0.180D3 * t42 * (t235 - t89 * t237) + 0.90D2 * t4 * (t96 * t237 / 
     #0.2D1 - t89 * t235) + t107 * t237) * t51 / 0.5760D4 + ((-0.180D3 *
     # t237 * lh + 0.90D2 * t235) * t4 * t127 + 0.90D2 * t237 * t4 * t13
     #5 + (-0.180D3 * t235 * lh + t237 * t106) * t4 * t145) * t49 / 0.11
     #520D5 + (0.90D2 * t4 * (-t153 * t254 - t235 + t157 * t237 + t260 -
     # (t236 - t162 * t237) * t33) - 0.180D3 * t42 * t255) * t49 * t58 /
     # 0.5760D4 + (-0.180D3 * t42 * (t178 * t237 - t235 + t260 - t183 * 
     #t254) + 0.90D2 * t4 * (t188 * t254 / 0.2D1 + t178 * t235 - t183 * 
     #t260 - t194 * t237 / 0.2D1) + t107 * t265) * t58 / 0.5760D4 - ((t2
     #35 - t205 * t237) * t106 - 0.180D3 * (t210 * t237 / 0.2D1 - t205 *
     # t235) * lh + 0.45D2 * t210 * t235 + t237 * t220 - 0.15D2 * t226 *
     # t237) * t4 / 0.11520D5
      t354 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t353)
      t356 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t357 = z * t356
      t358 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t367 = z * t358 * t33
      t375 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t376 = t375 - t367 - t358
      t380 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t386 = t375 - t358
      t474 = (0.90D2 * t4 * (-(t357 - t22 * t358) * t33 + t37 * t358 - t
     #356) - 0.180D3 * t42 * (-t367 - t358)) * t49 * t51 / 0.5760D4 + t4
     # * t376 * t60 / 0.32D2 + (0.90D2 * t4 * (t380 - t69 * t375 - t356 
     #+ t74 * t358) - 0.180D3 * t42 * t386) * t58 * t51 / 0.2880D4 - (-0
     #.180D3 * t42 * (t356 - t89 * t358) + 0.90D2 * t4 * (t96 * t358 / 0
     #.2D1 - t89 * t356) + t107 * t358) * t51 / 0.5760D4 + ((-0.180D3 * 
     #t358 * lh + 0.90D2 * t356) * t4 * t127 + 0.90D2 * t358 * t4 * t135
     # + (-0.180D3 * t356 * lh + t358 * t106) * t4 * t145) * t49 / 0.115
     #20D5 + (0.90D2 * t4 * (t380 - t356 - t153 * t375 - (t357 - t162 * 
     #t358) * t33 + t157 * t358) - 0.180D3 * t42 * t376) * t49 * t58 / 0
     #.5760D4 + (-0.180D3 * t42 * (-t356 + t380 + t178 * t358 - t183 * t
     #375) + 0.90D2 * t4 * (t188 * t375 / 0.2D1 - t183 * t380 - t194 * t
     #358 / 0.2D1 + t178 * t356) + t107 * t386) * t58 / 0.5760D4 - ((t35
     #6 - t205 * t358) * t106 - 0.15D2 * t226 * t358 - 0.180D3 * (t210 *
     # t358 / 0.2D1 - t205 * t356) * lh + 0.45D2 * t210 * t356 + t358 * 
     #t220) * t4 / 0.11520D5
      t475 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t474)
      t477 = x2 * x3
      t478 = 0.1D1 - x3 + t477
      t479 = 0.1D1 / t478
      t480 = t477 * t479
      t481 = t2 * t480
      t482 = t16 * t479
      t483 = t2 * t482
      t484 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t487 = t478 ** 2
      t493 = log(0.4D1 * t150 * t14 * t180 * t16 / t487)
      t494 = t493 * z
      t495 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t499 = t65 * t16
      t501 = Sqrt(t27 * t499)
      t505 = 0.1D1 / (-z - x3 + t477 + 0.2D1 * t26 * t501)
      t516 = t4 * z
      t519 = t58 * t51
      t520 = t505 * t49 * t519
      t523 = (0.90D2 * t4 * (z * t484 - t494 * t495) * t505 - 0.180D3 * 
     #t42 * z * t495 * t505) * t49 * t58 / 0.5760D4 + t516 * t495 * t520
     # / 0.32D2
      t524 = FJET(XB1, XB2, s, 0.0D0, t481, 0.0D0, -t483, 0.0D0, t523)
      t527 = t1 * x1
      t528 = x1 * z
      t529 = -z - x1 + t528
      t530 = 0.1D1 / t529
      t532 = t65 * s * t527 * t530
      t533 = -0.1D1 + x1
      t534 = t2 * t533
      t536 = x2 * s * t527
      t537 = t1 ** 2
      t538 = s * t537
      t541 = x1 * t533 * t530
      t542 = t538 * t65 * t541
      t543 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t547 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t548 = t64 * t11
      t550 = 0.1D1 / t12 * t530
      t551 = t533 ** 2
      t556 = log(0.4D1 * t548 * t550 * t551 * t65)
      t567 = -t4 * t543 * t60 / 0.32D2 + (0.90D2 * t4 * (-t547 + t556 * 
     #t543) + 0.180D3 * t42 * t543) * t58 * t51 / 0.2880D4
      t568 = FJET(XB1, XB2, s, 0.0D0, t532, -t534, t536, -t542, t567)
      t571 = t2 * x1 * t530
      t572 = t538 * t541
      t573 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t574 = z * t529
      t576 = t8 * t11
      t581 = log(0.4D1 * t576 * t550 * t551 * t17)
      t582 = t581 * z
      t583 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t587 = x1 * x3
      t588 = t587 * z
      t590 = 0.2D1 * t8 * z
      t591 = x1 * t12
      t592 = x3 * t12
      t593 = t592 * x1
      t594 = t8 * t12
      t595 = x3 * t529
      t597 = Sqrt(t595 * t16)
      t602 = 0.1D1 / (-t528 - t588 - t27 - t8 + t590 + t591 + t593 - t59
     #4 + 0.2D1 * t26 * t597 * z - t12)
      t604 = t550 * t551
      t607 = log(-0.4D1 * t576 * t604)
      t614 = -t574 * t583 * t602 + t583
      t626 = log(-0.4D1 * t548 * t604)
      t639 = log(-0.4D1 * t86 * t604)
      t644 = t639 ** 2
      t655 = (0.90D2 * t4 * (t573 - (t574 * t573 - t582 * t529 * t583) *
     # t602 - t607 * t583) - 0.180D3 * t42 * t614) * t49 * t51 / 0.5760D
     #4 + t4 * t614 * t60 / 0.32D2 + (0.90D2 * t4 * (t573 - t626 * t583)
     # - 0.180D3 * t42 * t583) * t58 * t51 / 0.2880D4 - (-0.180D3 * t42 
     #* (t639 * t583 - t573) + 0.90D2 * t4 * (-t644 * t583 / 0.2D1 + t63
     #9 * t573) - t107 * t583) * t51 / 0.5760D4
      t656 = FJET(XB1, XB2, s, 0.0D0, -t534, -t571, 0.0D0, t572, t655)
      t658 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t660 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t671 = t660 - t574 * t660 * t602
      t705 = (0.90D2 * t4 * (t658 - (t574 * t658 - t582 * t529 * t660) *
     # t602 - t607 * t660) - 0.180D3 * t42 * t671) * t49 * t51 / 0.5760D
     #4 + t4 * t671 * t60 / 0.32D2 + (0.90D2 * t4 * (-t626 * t660 + t658
     #) - 0.180D3 * t42 * t660) * t58 * t51 / 0.2880D4 - (-0.180D3 * t42
     # * (t639 * t660 - t658) + 0.90D2 * t4 * (-t644 * t660 / 0.2D1 + t6
     #39 * t658) - t107 * t660) * t51 / 0.5760D4
      t706 = FJET(XB1, XB2, s, 0.0D0, -t571, -t534, 0.0D0, t572, t705)
      t708 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t712 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t727 = t516 * t708 * t520 / 0.32D2 + (0.90D2 * t4 * (z * t712 - t4
     #94 * t708) * t505 - 0.180D3 * t42 * z * t708 * t505) * t49 * t58 /
     # 0.5760D4
      t728 = FJET(XB1, XB2, s, 0.0D0, -t483, 0.0D0, t481, 0.0D0, t727)
      t730 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t732 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t733 = z * t732
      t741 = z * t730 * t33
      t749 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t750 = -t741 + t749 - t730
      t754 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t760 = t749 - t730
      t773 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t841 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t853 = (0.90D2 * t4 * (t37 * t730 - t732 - (t733 - t22 * t730) * t
     #33) - 0.180D3 * t42 * (-t741 - t730)) * t49 * t51 / 0.5760D4 + t4 
     #* t750 * t60 / 0.32D2 + (0.90D2 * t4 * (t754 - t732 + t74 * t730 -
     # t69 * t749) - 0.180D3 * t42 * t760) * t58 * t51 / 0.2880D4 - (-0.
     #180D3 * t42 * (-t89 * t730 + t732) + 0.90D2 * t4 * (t96 * t730 / 0
     #.2D1 + t773 - t89 * t732) + t107 * t730) * t51 / 0.5760D4 - ((t732
     # - t205 * t730) * t106 - 0.15D2 * t226 * t730 - 0.180D3 * (t210 * 
     #t730 / 0.2D1 + t773 - t205 * t732) * lh + 0.45D2 * t210 * t732 + t
     #730 * t220 - 0.90D2 * t205 * t773) * t4 / 0.11520D5 + ((-0.180D3 *
     # t730 * lh + 0.90D2 * t732) * t4 * t127 + 0.90D2 * t730 * t4 * t13
     #5 + (-0.180D3 * t732 * lh + 0.90D2 * t773 + t730 * t106) * t4 * t1
     #45) * t49 / 0.11520D5 + (0.90D2 * t4 * (t754 - t153 * t749 - (t733
     # - t162 * t730) * t33 + t157 * t730 - t732) - 0.180D3 * t42 * t750
     #) * t49 * t58 / 0.5760D4 + (-0.180D3 * t42 * (-t732 + t754 + t178 
     #* t730 - t183 * t749) + 0.90D2 * t4 * (t188 * t749 / 0.2D1 - t773 
     #+ t841 - t183 * t754 - t194 * t730 / 0.2D1 + t178 * t732) + t107 *
     # t760) * t58 / 0.5760D4
      t854 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t853)
      t856 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t860 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t871 = -t4 * t856 * t60 / 0.32D2 + (0.90D2 * t4 * (-t860 + t556 * 
     #t856) + 0.180D3 * t42 * t856) * t58 * t51 / 0.2880D4
      t872 = FJET(XB1, XB2, s, t536, -t534, t532, 0.0D0, -t542, t871)
      t874 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t878 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t893 = t516 * t874 * t520 / 0.32D2 + (0.90D2 * t4 * (z * t878 - t4
     #94 * t874) * t505 - 0.180D3 * t42 * z * t874 * t505) * t49 * t58 /
     # 0.5760D4
      t894 = FJET(XB1, XB2, s, t481, 0.0D0, -t483, 0.0D0, 0.0D0, t893)
      t896 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t901 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t911 = -t4 * t896 * t60 / 0.32D2 + (0.90D2 * t4 * (t556 * t896 - t
     #901) + 0.180D3 * t42 * t896) * t58 * t51 / 0.2880D4
      t912 = FJET(XB1, XB2, s, t532, 0.0D0, t536, -t534, -t542, t911)
      t917 = t16 * s * t1 * t533 * t479
      t918 = t2 * x1
      t920 = Sqrt(-t595 * t499)
      t921 = t26 * t920
      t927 = t918 * x2 * (-x3 + t477 - z + t27 - x1 + t587 + t528 - t588
     # + 0.2D1 * t921) * t530 * t479
      t928 = t534 * t480
      t931 = t150 * x1
      t933 = t150 * t528
      t937 = t918 * (-x2 + t477 + 0.2D1 * t921 * x2 + 0.1D1 - x3 + t931 
     #+ t150 * z - t933) * t530 * t479
      t938 = t4 * t529
      t939 = x2 * x1
      t940 = t939 * z
      t941 = -z - t939 + t940
      t942 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, t54, -t482, x4)
      t953 = x2 * t7
      t957 = 0.2D1 * t921 * t940 + t12 - t931 + t940 - 0.2D1 * t921 * z 
     #- t8 * x2 - t477 * z + t477 * x1 - t939 * t12 - 0.2D1 * t953 * z +
     # t953 * t12 + t528 + t27
      t968 = t8 - t591 + t588 - t590 - t593 + t594 + t953 + t933 - 0.2D1
     # * t921 * t939 + t592 * t939 + 0.2D1 * t8 * x2 * z - t8 * t12 * x2
     # - 0.2D1 * t477 * t528
      t970 = 0.1D1 / (t957 + t968)
      t972 = t970 * t49 * t519
      t975 = FJET(XB1, XB2, s, t917, t927, -t928, -t937, -t542, t938 * t
     #941 * t942 * t972 / 0.32D2)
      t977 = t529 * t941
      t983 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, t54, -t482, x4)
      t988 = FJET(XB1, XB2, s, t927, t917, -t937, -t928, -t542, t938 * t
     #941 * t983 * t972 / 0.32D2)
      t995 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t997 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1008 = -t574 * t997 * t602 + t997
      t1032 = gbgbH54J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1043 = (0.90D2 * t4 * (-(t574 * t995 - t582 * t529 * t997) * t602
     # + t995 - t607 * t997) - 0.180D3 * t42 * t1008) * t49 * t51 / 0.57
     #60D4 + t4 * t1008 * t60 / 0.32D2 + (0.90D2 * t4 * (-t626 * t997 + 
     #t995) - 0.180D3 * t42 * t997) * t58 * t51 / 0.2880D4 - (-0.180D3 *
     # t42 * (t639 * t997 - t995) + 0.90D2 * t4 * (-t1032 + t639 * t995 
     #- t644 * t997 / 0.2D1) - t107 * t997) * t51 / 0.5760D4
      t1044 = FJET(XB1, XB2, s, -t534, 0.0D0, 0.0D0, -t571, t572, t1043)
      t1046 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t1050 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t1061 = -t4 * t1046 * t60 / 0.32D2 + (0.90D2 * t4 * (-t1050 + t556
     # * t1046) + 0.180D3 * t42 * t1046) * t58 * t51 / 0.2880D4
      t1062 = FJET(XB1, XB2, s, -t534, t536, 0.0D0, t532, -t542, t1061)
      t1064 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1066 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1077 = -t574 * t1064 * t602 + t1064
      t1101 = gbgbH52J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1112 = (0.90D2 * t4 * (-t607 * t1064 - (t574 * t1066 - t582 * t52
     #9 * t1064) * t602 + t1066) - 0.180D3 * t42 * t1077) * t49 * t51 / 
     #0.5760D4 + t4 * t1077 * t60 / 0.32D2 + (0.90D2 * t4 * (-t626 * t10
     #64 + t1066) - 0.180D3 * t42 * t1064) * t58 * t51 / 0.2880D4 - (-0.
     #180D3 * t42 * (t639 * t1064 - t1066) + 0.90D2 * t4 * (-t1101 + t63
     #9 * t1066 - t644 * t1064 / 0.2D1) - t107 * t1064) * t51 / 0.5760D4
      t1113 = FJET(XB1, XB2, s, -t571, 0.0D0, 0.0D0, -t534, t572, t1112)
      t1115 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t1119 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t482, x4)
      t1134 = t516 * t1115 * t520 / 0.32D2 + (0.90D2 * t4 * (z * t1119 -
     # t494 * t1115) * t505 - 0.180D3 * t42 * z * t1115 * t505) * t49 * 
     #t58 / 0.5760D4
      t1135 = FJET(XB1, XB2, s, -t483, 0.0D0, t481, 0.0D0, 0.0D0, t1134)
      t1137 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, t54, -t482, x4)
      t1142 = FJET(XB1, XB2, s, -t937, -t928, t927, t917, -t542, t938 * 
     #t941 * t1137 * t972 / 0.32D2)
      t1149 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, t54, -t482, x4)
      t1154 = FJET(XB1, XB2, s, -t928, -t937, t917, t927, -t542, t938 * 
     #t941 * t1149 * t972 / 0.32D2)
      gbgbH5n3e0 = t233 * t232 + t354 * t353 + t475 * t474 + t524 * t523
     # + t568 * t567 + t656 * t655 + t706 * t705 + t728 * t727 + t854 * 
     #t853 + t872 * t871 + t894 * t893 + t912 * t911 + t975 * t4 * t977 
     #* t942 * t970 * t60 / 0.32D2 + t988 * t4 * t977 * t983 * t970 * t6
     #0 / 0.32D2 + t1044 * t1043 + t1062 * t1061 + t1113 * t1112 + t1135
     # * t1134 + t1142 * t4 * t977 * t1137 * t970 * t60 / 0.32D2 + t1154
     # * t4 * t977 * t1149 * t970 * t60 / 0.32D2

      end function



      doubleprecision function gbgbH5n3em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t22 = t4 * lh
      t26 = 0.1D1 / x1
      t30 = cos(t7)
      t31 = x3 * z
      t32 = -0.1D1 + x3
      t34 = Sqrt(-t31 * t32)
      t38 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t34)
      t39 = z * t17 * t38
      t42 = 0.1D1 / x3
      t43 = t42 * t26
      t46 = 0.1D1 - x2
      t47 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t48 = -t17 + t47
      t50 = 0.1D1 / x2
      t51 = t50 * t26
      t56 = t42 * t50
      t59 = x2 ** 2
      t60 = t13 * t59
      t63 = log(0.4D1 * t60 * t9)
      t65 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t66 = -t46
      t70 = log(-0.4D1 * t60 * t9 * t66)
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t84 = 0.180D3 * t80 - 0.30D2 * t82
      t88 = log(0.4D1 * t13 * t9)
      t95 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t97 = t88 ** 2
      t104 = x3 * t13
      t109 = log(-0.4D1 * t104 * t9 / t32)
      t114 = log(0.4D1 * t104 * t9)
      t115 = t109 * z * t38 + t114
      t124 = -z * t38 - 0.1D1
      t129 = -(0.90D2 * t4 * (t5 - t16 * t17) - 0.180D3 * t22 * t17) * t
     #26 / 0.5760D4 + t4 * (-t17 - t39) * t43 / 0.64D2 + t4 * t48 * t51 
     #/ 0.32D2 + t4 * (-t39 + t47 - t17) * t56 / 0.64D2 + (0.90D2 * t4 *
     # (t63 * t17 - t5 + t65 - t70 * t47) - 0.180D3 * t22 * t48) * t50 /
     # 0.5760D4 - (t17 * t84 - 0.180D3 * (t5 - t88 * t17) * lh - 0.90D2 
     #* t88 * t5 + 0.90D2 * t95 + 0.45D2 * t97 * t17) * t4 / 0.11520D5 +
     # (0.90D2 * t17 * t4 * t115 + (-0.180D3 * t17 * lh + 0.90D2 * t5) *
     # t4 * t124) * t42 / 0.11520D5
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t129)
      t132 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t133 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t144 = z * t133 * t38
      t149 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t150 = t149 - t133
      t159 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t193 = -(0.90D2 * t4 * (t132 - t16 * t133) - 0.180D3 * t22 * t133)
     # * t26 / 0.5760D4 + t4 * (-t144 - t133) * t43 / 0.64D2 + t4 * t150
     # * t51 / 0.32D2 + t4 * (-t133 - t144 + t149) * t56 / 0.64D2 + (0.9
     #0D2 * t4 * (t63 * t133 - t132 + t159 - t70 * t149) - 0.180D3 * t22
     # * t150) * t50 / 0.5760D4 - (t133 * t84 - 0.180D3 * (t132 - t88 * 
     #t133) * lh + 0.45D2 * t97 * t133 - 0.90D2 * t88 * t132) * t4 / 0.1
     #1520D5 + (0.90D2 * t133 * t4 * t115 + (-0.180D3 * t133 * lh + 0.90
     #D2 * t132) * t4 * t124) * t42 / 0.11520D5
      t194 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t193)
      t196 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t197 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t208 = z * t197 * t38
      t213 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t214 = t213 - t197
      t222 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t257 = -(0.90D2 * t4 * (t196 - t16 * t197) - 0.180D3 * t22 * t197)
     # * t26 / 0.5760D4 + t4 * (-t208 - t197) * t43 / 0.64D2 + t4 * t214
     # * t51 / 0.32D2 + t4 * (t213 - t208 - t197) * t56 / 0.64D2 + (0.90
     #D2 * t4 * (-t196 + t222 + t63 * t197 - t70 * t213) - 0.180D3 * t22
     # * t214) * t50 / 0.5760D4 - (-0.180D3 * (t196 - t88 * t197) * lh +
     # t197 * t84 + 0.45D2 * t97 * t197 - 0.90D2 * t88 * t196) * t4 / 0.
     #11520D5 + (0.90D2 * t197 * t4 * t115 + (-0.180D3 * t197 * lh + 0.9
     #0D2 * t196) * t4 * t124) * t42 / 0.11520D5
      t258 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t257)
      t260 = x2 * x3
      t262 = 0.1D1 / (0.1D1 - x3 + t260)
      t264 = t2 * t260 * t262
      t265 = t32 * t262
      t266 = t2 * t265
      t267 = t4 * z
      t268 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, -t265, x4)
      t272 = Sqrt(t31 * t66 * t32)
      t276 = 0.1D1 / (-z - x3 + t260 + 0.2D1 * t30 * t272)
      t278 = t276 * t42 * t50
      t281 = FJET(XB1, XB2, s, 0.0D0, t264, 0.0D0, -t266, 0.0D0, t267 * 
     #t268 * t278 / 0.64D2)
      t289 = t1 * x1
      t290 = x1 * z
      t291 = -z - x1 + t290
      t292 = 0.1D1 / t291
      t294 = t66 * s * t289 * t292
      t295 = -0.1D1 + x1
      t296 = t2 * t295
      t298 = x2 * s * t289
      t299 = t1 ** 2
      t300 = s * t299
      t303 = x1 * t295 * t292
      t304 = t300 * t66 * t303
      t305 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, t46, 0.10D1, x4)
      t309 = FJET(XB1, XB2, s, 0.0D0, t294, -t296, t298, -t304, -t4 * t3
     #05 * t51 / 0.32D2)
      t316 = t2 * x1 * t292
      t317 = t300 * t303
      t320 = t295 ** 2
      t324 = log(-0.4D1 * t10 / t11 * t292 * t320)
      t325 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t327 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t336 = z * t291
      t339 = t6 * x3
      t348 = Sqrt(x3 * t291 * t32)
      t353 = 0.1D1 / (-t290 - x3 * x1 * z - t31 - t339 + 0.2D1 * t339 * 
     #z + x1 * t11 + x3 * t11 * x1 - t339 * t11 + 0.2D1 * t30 * t348 * z
     # - t11)
      t363 = -(0.90D2 * t4 * (t324 * t325 - t327) + 0.180D3 * t22 * t325
     #) * t26 / 0.5760D4 + t4 * (-t336 * t325 * t353 + t325) * t43 / 0.6
     #4D2 + t4 * t325 * t51 / 0.32D2
      t364 = FJET(XB1, XB2, s, 0.0D0, -t296, -t316, 0.0D0, t317, t363)
      t366 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t368 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t386 = -(0.90D2 * t4 * (t324 * t366 - t368) + 0.180D3 * t22 * t366
     #) * t26 / 0.5760D4 + t4 * (t366 - t336 * t366 * t353) * t43 / 0.64
     #D2 + t4 * t366 * t51 / 0.32D2
      t387 = FJET(XB1, XB2, s, 0.0D0, -t316, -t296, 0.0D0, t317, t386)
      t389 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, -t265, x4)
      t393 = FJET(XB1, XB2, s, 0.0D0, -t266, 0.0D0, t264, 0.0D0, t267 * 
     #t389 * t278 / 0.64D2)
      t400 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t402 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t412 = z * t400 * t38
      t417 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t418 = t417 - t400
      t426 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t46, 0.10D1, x4)
      t446 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t463 = -(0.90D2 * t4 * (-t16 * t400 + t402) - 0.180D3 * t22 * t400
     #) * t26 / 0.5760D4 + t4 * (-t412 - t400) * t43 / 0.64D2 + t4 * t41
     #8 * t51 / 0.32D2 + t4 * (-t412 + t417 - t400) * t56 / 0.64D2 + (0.
     #90D2 * t4 * (-t402 + t426 + t63 * t400 - t70 * t417) - 0.180D3 * t
     #22 * t418) * t50 / 0.5760D4 - (-0.180D3 * (t402 - t88 * t400) * lh
     # + t400 * t84 + 0.45D2 * t97 * t400 - 0.90D2 * t88 * t402 + 0.90D2
     # * t446) * t4 / 0.11520D5 + (0.90D2 * t400 * t4 * t115 + (-0.180D3
     # * t400 * lh + 0.90D2 * t402) * t4 * t124) * t42 / 0.11520D5
      t464 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t463)
      t466 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, t46, 0.10D1, x4)
      t470 = FJET(XB1, XB2, s, t298, -t296, t294, 0.0D0, -t304, -t4 * t4
     #66 * t51 / 0.32D2)
      t476 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, -t265, x4)
      t480 = FJET(XB1, XB2, s, t264, 0.0D0, -t266, 0.0D0, 0.0D0, t267 * 
     #t476 * t278 / 0.64D2)
      t487 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, t46, 0.10D1, x4)
      t491 = FJET(XB1, XB2, s, t294, 0.0D0, t298, -t296, -t304, -t4 * t4
     #87 * t51 / 0.32D2)
      t497 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t499 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t517 = -(0.90D2 * t4 * (t324 * t497 - t499) + 0.180D3 * t22 * t497
     #) * t26 / 0.5760D4 + t4 * (-t336 * t497 * t353 + t497) * t43 / 0.6
     #4D2 + t4 * t497 * t51 / 0.32D2
      t518 = FJET(XB1, XB2, s, -t296, 0.0D0, 0.0D0, -t316, t317, t517)
      t520 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, t46, 0.10D1, x4)
      t524 = FJET(XB1, XB2, s, -t296, t298, 0.0D0, t294, -t304, -t4 * t5
     #20 * t51 / 0.32D2)
      t530 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t532 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t550 = -(0.90D2 * t4 * (t324 * t530 - t532) + 0.180D3 * t22 * t530
     #) * t26 / 0.5760D4 + t4 * (-t336 * t530 * t353 + t530) * t43 / 0.6
     #4D2 + t4 * t530 * t51 / 0.32D2
      t551 = FJET(XB1, XB2, s, -t316, 0.0D0, 0.0D0, -t296, t317, t550)
      t553 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t46, -t265, x4)
      t557 = FJET(XB1, XB2, s, -t266, 0.0D0, t264, 0.0D0, 0.0D0, t267 * 
     #t553 * t278 / 0.64D2)
      gbgbH5n3em1 = t130 * t129 + t194 * t193 + t258 * t257 + t281 * t4 
     #* z * t268 * t276 * t56 / 0.64D2 - t309 * t4 * t305 * t50 * t26 / 
     #0.32D2 + t364 * t363 + t387 * t386 + t393 * t4 * z * t389 * t276 *
     # t56 / 0.64D2 + t464 * t463 - t470 * t4 * t466 * t50 * t26 / 0.32D
     #2 + t480 * t4 * z * t476 * t276 * t56 / 0.64D2 - t491 * t4 * t487 
     #* t50 * t26 / 0.32D2 + t518 * t517 - t524 * t4 * t520 * t50 * t26 
     #/ 0.32D2 + t550 * t551 + t557 * t4 * z * t553 * t276 * t56 / 0.64D
     #2

      end function



      doubleprecision function gbgbH5n3em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 - x2
      t11 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t14 = 0.1D1 / x2
      t19 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t21 = z ** 2
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t21 / z * t26)
      t35 = cos(t24)
      t39 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t47 = (-z / (-z - x3 + 0.2D1 * t35 * t39) - 0.1D1) / x3
      t50 = -t6 * t7 / 0.64D2 + t5 * (-t3 + t11) * t14 / 0.64D2 - (-0.18
     #0D3 * t3 * lh + 0.90D2 * t19 - 0.90D2 * t29 * t3) * t5 / 0.11520D5
     # + t6 * t47 / 0.128D3
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t54 = t53 * t5
      t57 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t62 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t73 = -t54 * t7 / 0.64D2 + t5 * (t57 - t53) * t14 / 0.64D2 - (0.90
     #D2 * t62 - 0.180D3 * t53 * lh - 0.90D2 * t29 * t53) * t5 / 0.11520
     #D5 + t54 * t47 / 0.128D3
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t73)
      t76 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t77 = t76 * t5
      t80 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t89 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t96 = -t77 * t7 / 0.64D2 + t5 * (t80 - t76) * t14 / 0.64D2 - (-0.1
     #80D3 * t76 * lh - 0.90D2 * t29 * t76 + 0.90D2 * t89) * t5 / 0.1152
     #0D5 + t77 * t47 / 0.128D3
      t97 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t96)
      t99 = -0.1D1 + x1
      t100 = t2 * t99
      t103 = 0.1D1 / (-z - x1 + x1 * z)
      t105 = t2 * x1 * t103
      t106 = t1 ** 2
      t110 = s * t106 * x1 * t99 * t103
      t111 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t115 = FJET(XB1, XB2, s, 0.0D0, -t100, -t105, 0.0D0, t110, t5 * t1
     #11 * t7 / 0.64D2)
      t120 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t124 = FJET(XB1, XB2, s, 0.0D0, -t105, -t100, 0.0D0, t110, t5 * t1
     #20 * t7 / 0.64D2)
      t129 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t130 = t5 * t129
      t133 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t142 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t149 = -t130 * t7 / 0.64D2 + t5 * (t133 - t129) * t14 / 0.64D2 - (
     #-0.180D3 * t129 * lh - 0.90D2 * t29 * t129 + 0.90D2 * t142) * t5 /
     # 0.11520D5 + t130 * t47 / 0.128D3
      t150 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t149)
      t152 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t156 = FJET(XB1, XB2, s, -t100, 0.0D0, 0.0D0, -t105, t110, t5 * t1
     #52 * t7 / 0.64D2)
      t161 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t165 = FJET(XB1, XB2, s, -t105, 0.0D0, 0.0D0, -t100, t110, t5 * t1
     #61 * t7 / 0.64D2)
      gbgbH5n3em2 = t51 * t50 + t74 * t73 + t97 * t96 + t115 * t5 * t111
     # * t7 / 0.64D2 + t124 * t5 * t120 * t7 / 0.64D2 + t150 * t149 + t1
     #56 * t5 * t152 * t7 / 0.64D2 + t165 * t5 * t161 * t7 / 0.64D2

      end function



      doubleprecision function gbgbH5n3em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t2 = s * (-0.1D1 + z)
      t3 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t3 * t5 / 
     #0.128D3)
      t11 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11 * t5 
     #/ 0.128D3)
      t17 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t17 * t5 
     #/ 0.128D3)
      t23 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t5 * t23 
     #/ 0.128D3)
      gbgbH5n3em3 = -t8 * t3 * t5 / 0.128D3 - t14 * t11 * t5 / 0.128D3 -
     # t20 * t17 * t5 / 0.128D3 - t26 * t5 * t23 / 0.128D3

      end function



      doubleprecision function gbgbH5n3em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      gbgbH5n3em4 = 0.0D0

      end function


      doubleprecision function gbgbH5n4e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = 0.1D1 / t3 / z
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = log(0.4D1 * t9)
      t12 = lh ** 2
      t13 = 0.180D3 * t12
      t14 = 0.3141592653589793D1 ** 2
      t15 = 0.30D2 * t14
      t16 = t13 - t15
      t18 = t11 ** 2
      t22 = 0.120D3 * t12 * lh
      t24 = 0.60D2 * lh * t14
      t25 = t18 * t11
      t28 = s ** 2
      t29 = 0.1D1 / t28
      t30 = (-t11 * t16 - 0.90D2 * t18 * lh - 0.2884936567583026D3 - t22
     # + t24 - 0.15D2 * t25) * t29
      t31 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t34 = t18 ** 2
      t36 = -0.2884936567583026D3 - t22 + t24
      t39 = t14 ** 2
      t40 = t12 ** 2
      t49 = (0.15D2 / 0.4D1 * t34 - t11 * t36 + 0.5769873135166051D3 * l
     #h + t39 + 0.60D2 * t40 - 0.60D2 * t12 * t14 + t18 * t16 / 0.2D1 + 
     #0.30D2 * t25 * lh) * t29
      t50 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t53 = t29 * lh
      t57 = log(0.4D1 * x3 * t5 * t8)
      t58 = t57 ** 2
      t65 = t29 * t16
      t71 = t58 * t57
      t77 = t29 * t36
      t78 = t77 * t50
      t80 = 0.1D1 / x3
      t83 = x1 ** 2
      t84 = t83 * t8
      t85 = t84 * t5
      t87 = log(0.4D1 * t85)
      t88 = t87 ** 2
      t100 = t88 * t87
      t107 = 0.1D1 / x1
      t110 = x3 * t83
      t113 = log(0.4D1 * t110 * t9)
      t118 = t113 ** 2
      t130 = t110 * x2
      t131 = -0.1D1 + x2
      t132 = t131 ** 2
      t133 = t9 * t132
      t136 = log(0.4D1 * t130 * t133)
      t137 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t139 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t140 = x2 * x3
      t143 = log(0.4D1 * t140 * t85)
      t148 = -t137 + t50
      t153 = 0.1D1 / x2
      t154 = t153 * t107
      t157 = x2 * t83
      t160 = log(0.4D1 * t157 * t9)
      t164 = log(0.4D1 * t157 * t133)
      t169 = t164 ** 2
      t174 = t160 ** 2
      t180 = t65 * t148
      t185 = x2 * t5
      t188 = log(0.4D1 * t185 * t8)
      t189 = t188 ** 2
      t192 = t8 * t132
      t195 = log(0.4D1 * t185 * t192)
      t198 = t195 ** 2
      t212 = t189 * t188
      t215 = t198 * t195
      t227 = log(0.4D1 * t140 * t9)
      t231 = log(0.4D1 * t140 * t133)
      t236 = t227 ** 2
      t239 = t231 ** 2
      t251 = -t30 * t31 / 0.5760D4 - t49 * t50 / 0.5760D4 + (-0.180D3 * 
     #t53 * (-t58 * t50 / 0.2D1 + t57 * t31) + t65 * (-t31 + t57 * t50) 
     #+ 0.90D2 * t29 * (-t58 * t31 / 0.2D1 + t71 * t50 / 0.6D1) - t78) *
     # t80 / 0.5760D4 - (-0.180D3 * t53 * (t88 * t50 / 0.2D1 - t87 * t31
     #) + t65 * (t31 - t87 * t50) + 0.90D2 * t29 * (t88 * t31 / 0.2D1 - 
     #t100 * t50 / 0.6D1) + t78) * t107 / 0.2880D4 + (-0.180D3 * t53 * (
     #t113 * t50 - t31) + 0.90D2 * t29 * (-t118 * t50 / 0.2D1 + t113 * t
     #31) - t65 * t50) * t80 * t107 / 0.2880D4 - (0.90D2 * t29 * (t31 + 
     #t136 * t137 - t139 - t143 * t50) - 0.180D3 * t53 * t148) * t80 * t
     #154 / 0.2880D4 - (-0.180D3 * t53 * (-t160 * t50 + t31 - t139 + t16
     #4 * t137) + 0.90D2 * t29 * (-t169 * t137 / 0.2D1 - t160 * t31 + t1
     #64 * t139 + t174 * t50 / 0.2D1) + t180) * t153 * t107 / 0.2880D4 -
     # (-0.180D3 * t53 * (t189 * t50 / 0.2D1 + t195 * t139 - t188 * t31 
     #- t198 * t137 / 0.2D1) + t65 * (t195 * t137 - t139 + t31 - t188 * 
     #t50) + 0.90D2 * t29 * (t189 * t31 / 0.2D1 - t198 * t139 / 0.2D1 - 
     #t212 * t50 / 0.6D1 + t215 * t137 / 0.6D1) + t77 * t148) * t153 / 0
     #.5760D4 - (-0.180D3 * t53 * (-t227 * t50 - t139 + t231 * t137 + t3
     #1) + 0.90D2 * t29 * (t236 * t50 / 0.2D1 - t239 * t137 / 0.2D1 - t2
     #27 * t31 + t231 * t139) + t180) * t80 * t153 / 0.5760D4
      t252 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t251)
      t254 = x1 * x3
      t255 = t2 * t254
      t256 = -0.1D1 + x1
      t258 = t2 * t256 * x3
      t259 = -0.1D1 + x3
      t260 = t259 * s
      t261 = t1 * x1
      t262 = t260 * t261
      t263 = t1 * t256
      t264 = t260 * t263
      t265 = -t259
      t266 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t267 = t110 * t8
      t268 = 0.1D1 / t3
      t269 = x1 * z
      t270 = -z - x1 + t269
      t271 = 0.1D1 / t270
      t272 = t268 * t271
      t273 = t256 ** 2
      t275 = t272 * t273 * t259
      t278 = log(0.4D1 * t267 * t275)
      t279 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t285 = gbgbH52J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t286 = t278 ** 2
      t296 = t140 * t84
      t299 = log(0.4D1 * t296 * t275)
      t310 = (-0.180D3 * t53 * (-t266 + t278 * t279) + 0.90D2 * t29 * (t
     #278 * t266 - t285 - t286 * t279 / 0.2D1) - t65 * t279) * t80 * t10
     #7 / 0.2880D4 - (0.90D2 * t29 * (-t299 * t279 + t266) - 0.180D3 * t
     #53 * t279) * t80 * t154 / 0.2880D4
      t311 = FJET(XB1, XB2, s, t255, -t258, -t262, t264, 0.0D0, t310)
      t313 = t2 * t259
      t314 = t2 * x3
      t315 = t9 * t259
      t318 = log(-0.4D1 * t110 * t315)
      t319 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t321 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t325 = t318 ** 2
      t328 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t333 = t65 * t319
      t340 = log(-0.4D1 * t130 * t315)
      t354 = log(-0.4D1 * t9 * x3 * t259)
      t355 = t354 ** 2
      t368 = t355 * t354
      t380 = log(-0.4D1 * t140 * t315)
      t386 = t380 ** 2
      t396 = (-0.180D3 * t53 * (-t318 * t319 + t321) + 0.90D2 * t29 * (t
     #325 * t319 / 0.2D1 + t328 - t318 * t321) + t333) * t80 * t107 / 0.
     #2880D4 - (0.90D2 * t29 * (-t321 + t340 * t319) + 0.180D3 * t53 * t
     #319) * t80 * t154 / 0.2880D4 + (-0.180D3 * t53 * (t355 * t319 / 0.
     #2D1 + t328 - t354 * t321) + t65 * (t321 - t354 * t319) + 0.90D2 * 
     #t29 * (-t354 * t328 + t355 * t321 / 0.2D1 - t368 * t319 / 0.6D1) +
     # t77 * t319) * t80 / 0.5760D4 - (-0.180D3 * t53 * (-t321 + t380 * 
     #t319) + 0.90D2 * t29 * (-t328 + t380 * t321 - t386 * t319 / 0.2D1)
     # - t333) * t80 * t153 / 0.5760D4
      t397 = FJET(XB1, XB2, s, -t313, 0.0D0, t314, 0.0D0, 0.0D0, t396)
      t399 = t2 * t256
      t400 = t2 * x1
      t401 = t272 * t273
      t404 = log(-0.4D1 * t84 * t401)
      t405 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t407 = t404 ** 2
      t408 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t417 = t407 * t404
      t430 = log(-0.4D1 * t267 * t401)
      t435 = t430 ** 2
      t442 = t65 * t408
      t447 = t273 * t271
      t451 = log(-0.4D1 * t130 * t8 * t268 * t447)
      t461 = t157 * t8
      t464 = log(-0.4D1 * t461 * t401)
      t470 = t464 ** 2
      t480 = -(-0.180D3 * t53 * (t404 * t405 - t407 * t408 / 0.2D1) + t6
     #5 * (t404 * t408 - t405) + 0.90D2 * t29 * (t417 * t408 / 0.6D1 - t
     #407 * t405 / 0.2D1) - t77 * t408) * t107 / 0.2880D4 + (-0.180D3 * 
     #t53 * (-t430 * t408 + t405) + 0.90D2 * t29 * (t435 * t408 / 0.2D1 
     #- t430 * t405) + t442) * t80 * t107 / 0.2880D4 - (0.90D2 * t29 * (
     #-t405 + t451 * t408) + 0.180D3 * t53 * t408) * t80 * t154 / 0.2880
     #D4 - (-0.180D3 * t53 * (t464 * t408 - t405) + 0.90D2 * t29 * (t464
     # * t405 - t470 * t408 / 0.2D1) - t442) * t153 * t107 / 0.2880D4
      t481 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t399, t400, 0.0D0, t480)
      t483 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t484 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t495 = t65 * t484
      t544 = (-0.180D3 * t53 * (t483 - t318 * t484) + 0.90D2 * t29 * (t3
     #25 * t484 / 0.2D1 - t318 * t483) + t495) * t80 * t107 / 0.2880D4 -
     # (0.90D2 * t29 * (-t483 + t340 * t484) + 0.180D3 * t53 * t484) * t
     #80 * t154 / 0.2880D4 - (-0.180D3 * t53 * (-t483 + t380 * t484) + 0
     #.90D2 * t29 * (-t386 * t484 / 0.2D1 + t380 * t483) - t495) * t80 *
     # t153 / 0.5760D4 + (-0.180D3 * t53 * (-t354 * t483 + t355 * t484 /
     # 0.2D1) + t65 * (-t354 * t484 + t483) + 0.90D2 * t29 * (-t368 * t4
     #84 / 0.6D1 + t355 * t483 / 0.2D1) + t77 * t484) * t80 / 0.5760D4
      t545 = FJET(XB1, XB2, s, 0.0D0, -t313, 0.0D0, t314, 0.0D0, t544)
      t548 = t140 - 0.1D1
      t549 = 0.1D1 / t548
      t550 = x3 * t131 * t549
      t551 = t2 * t550
      t552 = t259 * t549
      t553 = t2 * t552
      t554 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t555 = z * t554
      t557 = t548 ** 2
      t558 = 0.1D1 / t557
      t559 = t259 * t558
      t563 = log(-0.4D1 * t140 * t5 * t192 * t559)
      t564 = t563 * z
      t565 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t568 = cos(t6)
      t569 = x3 * z
      t570 = x2 * t259
      t572 = Sqrt(-t569 * t570)
      t576 = 0.1D1 / (-z - t140 + 0.2D1 * t568 * t572)
      t580 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t583 = t563 ** 2
      t584 = t583 * z
      t592 = z * t565 * t576
      t602 = log(-0.4D1 * t296 * t5 * t132 * t559)
      t603 = t602 * z
      t615 = -(0.180D3 * t53 * (t555 - t564 * t565) * t576 - 0.90D2 * t2
     #9 * (z * t580 - t564 * t554 + t584 * t565 / 0.2D1) * t576 - t65 * 
     #t592) * t80 * t153 / 0.5760D4 - (-0.90D2 * t29 * (t555 - t603 * t5
     #65) * t576 + 0.180D3 * t53 * t592) * t80 * t154 / 0.2880D4
      t616 = FJET(XB1, XB2, s, 0.0D0, t551, 0.0D0, t553, 0.0D0, t615)
      t618 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t620 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t627 = gbgbH54J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t645 = (-0.180D3 * t53 * (t278 * t618 - t620) + 0.90D2 * t29 * (-t
     #286 * t618 / 0.2D1 + t278 * t620 - t627) - t65 * t618) * t80 * t10
     #7 / 0.2880D4 - (0.90D2 * t29 * (-t299 * t618 + t620) - 0.180D3 * t
     #53 * t618) * t80 * t154 / 0.2880D4
      t646 = FJET(XB1, XB2, s, t264, -t262, -t258, t255, 0.0D0, t645)
      t649 = t131 * s * t261
      t650 = x2 * x1
      t652 = t2 * t650 * t271
      t653 = t1 ** 2
      t658 = s * t653 * x2 * x1 * t256 * t271
      t659 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t662 = t268 * t273 * t271 * t132
      t665 = log(-0.4D1 * t296 * t662)
      t666 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t678 = log(-0.4D1 * t461 * t662)
      t683 = gbgbH54J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t685 = t678 ** 2
      t696 = -(0.90D2 * t29 * (t659 - t665 * t666) - 0.180D3 * t53 * t66
     #6) * t80 * t154 / 0.2880D4 - (-0.180D3 * t53 * (-t678 * t666 + t65
     #9) + 0.90D2 * t29 * (t683 - t678 * t659 + t685 * t666 / 0.2D1) + t
     #65 * t666) * t153 * t107 / 0.2880D4
      t697 = FJET(XB1, XB2, s, -t399, -t649, 0.0D0, -t652, t658, t696)
      t699 = t254 * z
      t702 = Sqrt(x3 * t270 * t570)
      t703 = t568 * t702
      t704 = 0.2D1 * t703
      t709 = t400 * t131 * (-t140 - z + t569 - x1 + t254 + t269 - t699 +
     # t704) * t271 * t549
      t711 = t260 * t263 * t549
      t712 = x2 ** 2
      t713 = t712 * x3
      t714 = t713 * x1
      t716 = t140 * z
      t718 = t140 * x1
      t722 = t713 * t269
      t725 = -t569 - t254 + t140 - t704 - x2 - t714 - t713 * z + t699 + 
     #0.2D1 * t716 + 0.2D1 * t718 + 0.2D1 * t703 * x2 + t722 - 0.2D1 * t
     #140 * t269
      t728 = t400 * t725 * t271 * t549
      t729 = t399 * t550
      t730 = t650 * z
      t731 = z + x1 - t269 - t650 + t730
      t732 = t270 * t731
      t733 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t742 = log(0.4D1 * t140 * t84 * t268 * t447 * t132 * t259 * t558)
      t743 = t742 * t270
      t744 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t745 = t731 * t744
      t759 = 0.2D1 * t703 * z + 0.2D1 * t703 * x1 - t83 - 0.2D1 * t269 -
     # t130 + t714 + t730 - t716 - t718 + 0.2D1 * t703 * t730 - t650 * t
     #3 - 0.2D1 * t157 * z
      t777 = t157 * t3 - 0.2D1 * t703 * t650 - 0.2D1 * t703 * t269 + 0.2
     #D1 * x1 * t3 + 0.2D1 * t83 * z - t83 * t3 + t157 - t3 + x3 * t3 * 
     #t650 + 0.2D1 * t110 * x2 * z - t110 * t3 * x2 - t722
      t779 = 0.1D1 / (t759 + t777)
      t782 = t53 * t270
      t786 = -0.90D2 * t29 * (t732 * t733 - t743 * t745) * t779 + 0.180D
     #3 * t782 * t745 * t779
      t790 = FJET(XB1, XB2, s, t709, -t711, -t728, -t729, t658, -t786 * 
     #t80 * t154 / 0.2880D4)
      t793 = t80 * t153 * t107
      t796 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t798 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t799 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t816 = t77 * t799
      t835 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t837 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t842 = -t837 + t799
      t857 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t864 = t65 * t842
      t873 = (0.180D3 * t11 * lh + t13 - t15 + 0.45D2 * t18) * t29
      t948 = -(-0.180D3 * t53 * (-t87 * t796 + t798 + t88 * t799 / 0.2D1
     #) + t65 * (t796 - t87 * t799) + 0.90D2 * t29 * (-t87 * t798 + t88 
     #* t796 / 0.2D1 - t100 * t799 / 0.6D1) + t816) * t107 / 0.2880D4 + 
     #(-0.180D3 * t53 * (t113 * t799 - t796) + 0.90D2 * t29 * (-t118 * t
     #799 / 0.2D1 + t113 * t796 - t798) - t65 * t799) * t80 * t107 / 0.2
     #880D4 - (0.90D2 * t29 * (-t835 - t143 * t799 + t136 * t837 + t796)
     # - 0.180D3 * t53 * t842) * t80 * t154 / 0.2880D4 - (-0.180D3 * t53
     # * (-t160 * t799 + t796 - t835 + t164 * t837) + 0.90D2 * t29 * (-t
     #169 * t837 / 0.2D1 + t164 * t835 - t857 - t160 * t796 + t798 + t17
     #4 * t799 / 0.2D1) + t864) * t153 * t107 / 0.2880D4 - t873 * t798 /
     # 0.5760D4 - t30 * t796 / 0.5760D4 - t49 * t799 / 0.5760D4 + (-0.18
     #0D3 * t53 * (t57 * t796 - t798 - t58 * t799 / 0.2D1) + t65 * (-t79
     #6 + t57 * t799) + 0.90D2 * t29 * (t57 * t798 - t58 * t796 / 0.2D1 
     #+ t71 * t799 / 0.6D1) - t816) * t80 / 0.5760D4 - (-0.180D3 * t53 *
     # (t189 * t799 / 0.2D1 - t188 * t796 + t798 + t195 * t835 - t857 - 
     #t198 * t837 / 0.2D1) + t65 * (t195 * t837 - t835 + t796 - t188 * t
     #799) + 0.90D2 * t29 * (-t188 * t798 + t189 * t796 / 0.2D1 - t212 *
     # t799 / 0.6D1 + t195 * t857 - t198 * t835 / 0.2D1 + t215 * t837 / 
     #0.6D1) + t77 * t842) * t153 / 0.5760D4 - (-0.180D3 * t53 * (-t227 
     #* t799 + t796 - t835 + t231 * t837) + 0.90D2 * t29 * (t236 * t799 
     #/ 0.2D1 - t227 * t796 + t798 + t231 * t835 - t857 - t239 * t837 / 
     #0.2D1) + t864) * t80 * t153 / 0.5760D4
      t949 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t948)
      t951 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t952 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t966 = gbgbH52J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t978 = -(0.90D2 * t29 * (t951 - t665 * t952) - 0.180D3 * t53 * t95
     #2) * t80 * t154 / 0.2880D4 - (-0.180D3 * t53 * (t951 - t678 * t952
     #) + 0.90D2 * t29 * (t966 - t678 * t951 + t685 * t952 / 0.2D1) + t6
     #5 * t952) * t153 * t107 / 0.2880D4
      t979 = FJET(XB1, XB2, s, -t652, 0.0D0, -t649, -t399, t658, t978)
      t981 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t983 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t984 = t731 * t983
      t993 = -0.90D2 * t29 * (t732 * t981 - t743 * t984) * t779 + 0.180D
     #3 * t782 * t984 * t779
      t997 = FJET(XB1, XB2, s, -t728, -t729, t709, -t711, t658, -t993 * 
     #t80 * t154 / 0.2880D4)
      t1001 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t1002 = z * t1001
      t1003 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t1010 = z * t1003 * t576
      t1034 = -(-0.90D2 * t29 * (t1002 - t603 * t1003) * t576 + 0.180D3 
     #* t53 * t1010) * t80 * t154 / 0.2880D4 - (0.180D3 * t53 * (t1002 -
     # t564 * t1003) * t576 - 0.90D2 * t29 * (-t564 * t1001 + t584 * t10
     #03 / 0.2D1) * t576 - t65 * t1010) * t80 * t153 / 0.5760D4
      t1035 = FJET(XB1, XB2, s, 0.0D0, t553, 0.0D0, t551, 0.0D0, t1034)
      t1037 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1040 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1068 = t65 * t1037
      t1095 = -(-0.180D3 * t53 * (-t407 * t1037 / 0.2D1 + t404 * t1040) 
     #+ t65 * (t404 * t1037 - t1040) + 0.90D2 * t29 * (-t407 * t1040 / 0
     #.2D1 + t417 * t1037 / 0.6D1) - t77 * t1037) * t107 / 0.2880D4 + (-
     #0.180D3 * t53 * (-t430 * t1037 + t1040) + 0.90D2 * t29 * (t435 * t
     #1037 / 0.2D1 - t430 * t1040) + t1068) * t80 * t107 / 0.2880D4 - (0
     #.90D2 * t29 * (t451 * t1037 - t1040) + 0.180D3 * t53 * t1037) * t8
     #0 * t154 / 0.2880D4 - (-0.180D3 * t53 * (-t1040 + t464 * t1037) + 
     #0.90D2 * t29 * (t464 * t1040 - t470 * t1037 / 0.2D1) - t1068) * t1
     #53 * t107 / 0.2880D4
      t1096 = FJET(XB1, XB2, s, t400, -t399, 0.0D0, 0.0D0, 0.0D0, t1095)
      t1098 = t252 * t251 + t311 * t310 + t397 * t396 + t481 * t480 + t5
     #45 * t544 + t616 * t615 + t646 * t645 + t697 * t696 - t790 * t786 
     #* t793 / 0.2880D4 + t949 * t948 + t979 * t978 - t997 * t993 * t793
     # / 0.2880D4 + t1035 * t1034 + t1096 * t1095
      t1099 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t1101 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t1125 = (-0.180D3 * t53 * (t278 * t1099 - t1101) + 0.90D2 * t29 * 
     #(t278 * t1101 - t286 * t1099 / 0.2D1) - t65 * t1099) * t80 * t107 
     #/ 0.2880D4 - (0.90D2 * t29 * (t1101 - t299 * t1099) - 0.180D3 * t5
     #3 * t1099) * t80 * t154 / 0.2880D4
      t1126 = FJET(XB1, XB2, s, -t258, t255, t264, -t262, 0.0D0, t1125)
      t1128 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t1129 = z * t1128
      t1130 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t1137 = z * t1130 * t576
      t1161 = -(-0.90D2 * t29 * (t1129 - t603 * t1130) * t576 + 0.180D3 
     #* t53 * t1137) * t80 * t154 / 0.2880D4 - (0.180D3 * t53 * (t1129 -
     # t564 * t1130) * t576 - 0.90D2 * t29 * (-t564 * t1128 + t584 * t11
     #30 / 0.2D1) * t576 - t65 * t1137) * t80 * t153 / 0.5760D4
      t1162 = FJET(XB1, XB2, s, t551, 0.0D0, t553, 0.0D0, 0.0D0, t1161)
      t1164 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1165 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1190 = -(0.90D2 * t29 * (t1164 - t665 * t1165) - 0.180D3 * t53 * 
     #t1165) * t80 * t154 / 0.2880D4 - (-0.180D3 * t53 * (-t678 * t1165 
     #+ t1164) + 0.90D2 * t29 * (-t678 * t1164 + t685 * t1165 / 0.2D1) +
     # t65 * t1165) * t153 * t107 / 0.2880D4
      t1191 = FJET(XB1, XB2, s, -t649, -t399, -t652, 0.0D0, t658, t1190)
      t1193 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t1194 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t1200 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t1206 = t65 * t1194
      t1256 = (-0.180D3 * t53 * (t1193 - t318 * t1194) + 0.90D2 * t29 * 
     #(-t318 * t1193 + t1200 + t325 * t1194 / 0.2D1) + t1206) * t80 * t1
     #07 / 0.2880D4 - (0.90D2 * t29 * (t340 * t1194 - t1193) + 0.180D3 *
     # t53 * t1194) * t80 * t154 / 0.2880D4 - (-0.180D3 * t53 * (-t1193 
     #+ t380 * t1194) + 0.90D2 * t29 * (-t386 * t1194 / 0.2D1 + t380 * t
     #1193 - t1200) - t1206) * t80 * t153 / 0.5760D4 + (-0.180D3 * t53 *
     # (t355 * t1194 / 0.2D1 - t354 * t1193 + t1200) + t65 * (t1193 - t3
     #54 * t1194) + 0.90D2 * t29 * (-t354 * t1200 - t368 * t1194 / 0.6D1
     # + t355 * t1193 / 0.2D1) + t77 * t1194) * t80 / 0.5760D4
      t1257 = FJET(XB1, XB2, s, 0.0D0, t314, 0.0D0, -t313, 0.0D0, t1256)
      t1259 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1260 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1262 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1279 = t77 * t1262
      t1298 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1301 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1305 = -t1298 + t1262
      t1323 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1327 = t65 * t1305
      t1406 = -(-0.180D3 * t53 * (t1259 - t87 * t1260 + t88 * t1262 / 0.
     #2D1) + t65 * (-t87 * t1262 + t1260) + 0.90D2 * t29 * (-t87 * t1259
     # + t88 * t1260 / 0.2D1 - t100 * t1262 / 0.6D1) + t1279) * t107 / 0
     #.2880D4 + (-0.180D3 * t53 * (t113 * t1262 - t1260) + 0.90D2 * t29 
     #* (-t1259 - t118 * t1262 / 0.2D1 + t113 * t1260) - t65 * t1262) * 
     #t80 * t107 / 0.2880D4 - (0.90D2 * t29 * (t136 * t1298 - t143 * t12
     #62 - t1301 + t1260) - 0.180D3 * t53 * t1305) * t80 * t154 / 0.2880
     #D4 - (-0.180D3 * t53 * (-t160 * t1262 - t1301 + t164 * t1298 + t12
     #60) + 0.90D2 * t29 * (t1259 + t164 * t1301 + t174 * t1262 / 0.2D1 
     #- t169 * t1298 / 0.2D1 - t160 * t1260 - t1323) + t1327) * t153 * t
     #107 / 0.2880D4 + (-0.180D3 * t53 * (-t1259 + t57 * t1260 - t58 * t
     #1262 / 0.2D1) + t65 * (t57 * t1262 - t1260) + 0.90D2 * t29 * (t57 
     #* t1259 - t58 * t1260 / 0.2D1 + t71 * t1262 / 0.6D1) - t1279) * t8
     #0 / 0.5760D4 - t873 * t1259 / 0.5760D4 - t49 * t1262 / 0.5760D4 - 
     #(-0.180D3 * t53 * (t189 * t1262 / 0.2D1 - t1323 + t1259 - t188 * t
     #1260 - t198 * t1298 / 0.2D1 + t195 * t1301) + t65 * (-t1301 + t126
     #0 + t195 * t1298 - t188 * t1262) + 0.90D2 * t29 * (-t188 * t1259 +
     # t189 * t1260 / 0.2D1 - t198 * t1301 / 0.2D1 - t212 * t1262 / 0.6D
     #1 + t215 * t1298 / 0.6D1 + t195 * t1323) + t77 * t1305) * t153 / 0
     #.5760D4 - (-0.180D3 * t53 * (-t1301 - t227 * t1262 + t1260 + t231 
     #* t1298) + 0.90D2 * t29 * (t231 * t1301 + t1259 - t227 * t1260 - t
     #239 * t1298 / 0.2D1 - t1323 + t236 * t1262 / 0.2D1) + t1327) * t80
     # * t153 / 0.5760D4 - t30 * t1260 / 0.5760D4
      t1407 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1406)
      t1409 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t1410 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t265, x4)
      t1435 = (-0.180D3 * t53 * (-t1409 + t278 * t1410) + 0.90D2 * t29 *
     # (t278 * t1409 - t286 * t1410 / 0.2D1) - t65 * t1410) * t80 * t107
     # / 0.2880D4 - (0.90D2 * t29 * (-t299 * t1410 + t1409) - 0.180D3 * 
     #t53 * t1410) * t80 * t154 / 0.2880D4
      t1436 = FJET(XB1, XB2, s, -t262, t264, t255, -t258, 0.0D0, t1435)
      t1438 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t1439 = z * t1438
      t1440 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t1447 = z * t1440 * t576
      t1459 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t552, x4)
      t1473 = -(-0.90D2 * t29 * (t1439 - t603 * t1440) * t576 + 0.180D3 
     #* t53 * t1447) * t80 * t154 / 0.2880D4 - (0.180D3 * t53 * (t1439 -
     # t564 * t1440) * t576 - 0.90D2 * t29 * (z * t1459 - t564 * t1438 +
     # t584 * t1440 / 0.2D1) * t576 - t65 * t1447) * t80 * t153 / 0.5760
     #D4
      t1474 = FJET(XB1, XB2, s, t553, 0.0D0, t551, 0.0D0, 0.0D0, t1473)
      t1476 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1480 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1496 = t77 * t1480
      t1515 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1517 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1522 = -t1515 + t1480
      t1543 = t65 * t1522
      t1615 = -t30 * t1476 / 0.5760D4 - (-0.180D3 * t53 * (-t87 * t1476 
     #+ t88 * t1480 / 0.2D1) + t65 * (-t87 * t1480 + t1476) + 0.90D2 * t
     #29 * (t88 * t1476 / 0.2D1 - t100 * t1480 / 0.6D1) + t1496) * t107 
     #/ 0.2880D4 + (-0.180D3 * t53 * (t113 * t1480 - t1476) + 0.90D2 * t
     #29 * (-t118 * t1480 / 0.2D1 + t113 * t1476) - t65 * t1480) * t80 *
     # t107 / 0.2880D4 - (0.90D2 * t29 * (t136 * t1515 + t1476 - t1517 -
     # t143 * t1480) - 0.180D3 * t53 * t1522) * t80 * t154 / 0.2880D4 - 
     #(-0.180D3 * t53 * (-t160 * t1480 + t1476 + t164 * t1515 - t1517) +
     # 0.90D2 * t29 * (-t169 * t1515 / 0.2D1 + t164 * t1517 - t160 * t14
     #76 + t174 * t1480 / 0.2D1) + t1543) * t153 * t107 / 0.2880D4 - t49
     # * t1480 / 0.5760D4 - (-0.180D3 * t53 * (t189 * t1480 / 0.2D1 - t1
     #88 * t1476 - t198 * t1515 / 0.2D1 + t195 * t1517) + t65 * (-t1517 
     #+ t1476 + t195 * t1515 - t188 * t1480) + 0.90D2 * t29 * (t189 * t1
     #476 / 0.2D1 - t198 * t1517 / 0.2D1 - t212 * t1480 / 0.6D1 + t215 *
     # t1515 / 0.6D1) + t77 * t1522) * t153 / 0.5760D4 - (-0.180D3 * t53
     # * (t1476 - t227 * t1480 - t1517 + t231 * t1515) + 0.90D2 * t29 * 
     #(-t227 * t1476 + t236 * t1480 / 0.2D1 - t239 * t1515 / 0.2D1 + t23
     #1 * t1517) + t1543) * t80 * t153 / 0.5760D4 + (-0.180D3 * t53 * (-
     #t58 * t1480 / 0.2D1 + t57 * t1476) + t65 * (-t1476 + t57 * t1480) 
     #+ 0.90D2 * t29 * (-t58 * t1476 / 0.2D1 + t71 * t1480 / 0.6D1) - t1
     #496) * t80 / 0.5760D4
      t1616 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1615)
      t1618 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1621 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1623 = gbgbH52J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1651 = t65 * t1618
      t1678 = -(-0.180D3 * t53 * (-t407 * t1618 / 0.2D1 + t404 * t1621 -
     # t1623) + t65 * (-t1621 + t404 * t1618) + 0.90D2 * t29 * (t404 * t
     #1623 + t417 * t1618 / 0.6D1 - t407 * t1621 / 0.2D1) - t77 * t1618)
     # * t107 / 0.2880D4 + (-0.180D3 * t53 * (-t430 * t1618 + t1621) + 0
     #.90D2 * t29 * (t435 * t1618 / 0.2D1 + t1623 - t430 * t1621) + t165
     #1) * t80 * t107 / 0.2880D4 - (0.90D2 * t29 * (-t1621 + t451 * t161
     #8) + 0.180D3 * t53 * t1618) * t80 * t154 / 0.2880D4 - (-0.180D3 * 
     #t53 * (t464 * t1618 - t1621) + 0.90D2 * t29 * (-t1623 - t470 * t16
     #18 / 0.2D1 + t464 * t1621) - t1651) * t153 * t107 / 0.2880D4
      t1679 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t400, -t399, 0.0D0, t1678)
      t1681 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t1682 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t265, x4)
      t1693 = t65 * t1682
      t1742 = (-0.180D3 * t53 * (t1681 - t318 * t1682) + 0.90D2 * t29 * 
     #(t325 * t1682 / 0.2D1 - t318 * t1681) + t1693) * t80 * t107 / 0.28
     #80D4 - (0.90D2 * t29 * (-t1681 + t340 * t1682) + 0.180D3 * t53 * t
     #1682) * t80 * t154 / 0.2880D4 + (-0.180D3 * t53 * (t355 * t1682 / 
     #0.2D1 - t354 * t1681) + t65 * (t1681 - t354 * t1682) + 0.90D2 * t2
     #9 * (t355 * t1681 / 0.2D1 - t368 * t1682 / 0.6D1) + t77 * t1682) *
     # t80 / 0.5760D4 - (-0.180D3 * t53 * (-t1681 + t380 * t1682) + 0.90
     #D2 * t29 * (t380 * t1681 - t386 * t1682 / 0.2D1) - t1693) * t80 * 
     #t153 / 0.5760D4
      t1743 = FJET(XB1, XB2, s, t314, 0.0D0, -t313, 0.0D0, 0.0D0, t1742)
      t1745 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1747 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1771 = -(0.90D2 * t29 * (-t665 * t1745 + t1747) - 0.180D3 * t53 *
     # t1745) * t80 * t154 / 0.2880D4 - (-0.180D3 * t53 * (-t678 * t1745
     # + t1747) + 0.90D2 * t29 * (-t678 * t1747 + t685 * t1745 / 0.2D1) 
     #+ t65 * t1745) * t153 * t107 / 0.2880D4
      t1772 = FJET(XB1, XB2, s, 0.0D0, -t652, -t399, -t649, t658, t1771)
      t1774 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1777 = gbgbH54J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1778 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1807 = t65 * t1774
      t1834 = -(-0.180D3 * t53 * (-t407 * t1774 / 0.2D1 - t1777 + t404 *
     # t1778) + t65 * (-t1778 + t404 * t1774) + 0.90D2 * t29 * (t404 * t
     #1777 - t407 * t1778 / 0.2D1 + t417 * t1774 / 0.6D1) - t77 * t1774)
     # * t107 / 0.2880D4 + (-0.180D3 * t53 * (t1778 - t430 * t1774) + 0.
     #90D2 * t29 * (-t430 * t1778 + t1777 + t435 * t1774 / 0.2D1) + t180
     #7) * t80 * t107 / 0.2880D4 - (0.90D2 * t29 * (-t1778 + t451 * t177
     #4) + 0.180D3 * t53 * t1774) * t80 * t154 / 0.2880D4 - (-0.180D3 * 
     #t53 * (t464 * t1774 - t1778) + 0.90D2 * t29 * (t464 * t1778 - t470
     # * t1774 / 0.2D1 - t1777) - t1807) * t153 * t107 / 0.2880D4
      t1835 = FJET(XB1, XB2, s, -t399, t400, 0.0D0, 0.0D0, 0.0D0, t1834)
      t1837 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t1839 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t1840 = t731 * t1839
      t1849 = -0.90D2 * t29 * (t732 * t1837 - t743 * t1840) * t779 + 0.1
     #80D3 * t782 * t1840 * t779
      t1853 = FJET(XB1, XB2, s, -t729, -t728, -t711, t709, t658, -t1849 
     #* t80 * t154 / 0.2880D4)
      t1857 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t1859 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, t552, x4)
      t1860 = t731 * t1859
      t1869 = -0.90D2 * t29 * (t732 * t1857 - t743 * t1860) * t779 + 0.1
     #80D3 * t782 * t1860 * t779
      t1873 = FJET(XB1, XB2, s, -t711, t709, -t729, -t728, t658, -t1869 
     #* t80 * t154 / 0.2880D4)
      t1877 = t1126 * t1125 + t1162 * t1161 + t1191 * t1190 + t1257 * t1
     #256 + t1407 * t1406 + t1436 * t1435 + t1474 * t1473 + t1616 * t161
     #5 + t1679 * t1678 + t1743 * t1742 + t1772 * t1771 + t1835 * t1834 
     #- t1853 * t1849 * t793 / 0.2880D4 - t1873 * t1869 * t793 / 0.2880D
     #4
      gbgbH5n4e1 = t1098 + t1877

      end function



      doubleprecision function gbgbH5n4e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = x3 * z
      t5 = x1 * x3
      t6 = x2 * x3
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t9 = x1 * z
      t10 = -z - x1 + t9
      t12 = -0.1D1 + x3
      t13 = x2 * t12
      t15 = Sqrt(x3 * t10 * t13)
      t16 = t8 * t15
      t17 = 0.2D1 * t16
      t18 = x2 ** 2
      t19 = t18 * x3
      t20 = t19 * x1
      t22 = t5 * z
      t23 = t6 * z
      t25 = t6 * x1
      t29 = t19 * t9
      t32 = -t4 - t5 + t6 - t17 - x2 - t20 - t19 * z + t22 + 0.2D1 * t23
     # + 0.2D1 * t25 + 0.2D1 * t16 * x2 + t29 - 0.2D1 * t6 * t9
      t33 = 0.1D1 / t10
      t35 = t6 - 0.1D1
      t36 = 0.1D1 / t35
      t38 = t3 * t32 * t33 * t36
      t39 = -0.1D1 + x1
      t40 = t2 * t39
      t41 = -0.1D1 + x2
      t43 = x3 * t41 * t36
      t44 = t40 * t43
      t49 = t3 * t41 * (-t6 - z + t4 - x1 + t5 + t9 - t22 + t17) * t33 *
     # t36
      t50 = t12 * s
      t51 = t1 * t39
      t53 = t50 * t51 * t36
      t54 = t1 ** 2
      t59 = s * t54 * x2 * x1 * t39 * t33
      t60 = s ** 2
      t61 = 0.1D1 / t60
      t62 = t61 * t10
      t63 = x2 * x1
      t64 = t63 * z
      t65 = z + x1 - t9 - t63 + t64
      t66 = t12 * t36
      t67 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, t66, x4)
      t74 = x1 ** 2
      t76 = t74 * x3
      t80 = z ** 2
      t82 = x2 * t74
      t85 = 0.2D1 * t16 * z + 0.2D1 * t16 * x1 - t74 - 0.2D1 * t9 - t76 
     #* x2 + t20 + t64 - t23 - t25 + 0.2D1 * t16 * t64 - t63 * t80 - 0.2
     #D1 * t82 * z
      t103 = t82 * t80 - 0.2D1 * t16 * t63 - 0.2D1 * t16 * t9 + 0.2D1 * 
     #x1 * t80 + 0.2D1 * t74 * z - t74 * t80 + t82 - t80 + x3 * t80 * t6
     #3 + 0.2D1 * t76 * x2 * z - t76 * t80 * x2 - t29
      t105 = 0.1D1 / (t85 + t103)
      t106 = 0.1D1 / x3
      t108 = 0.1D1 / x2
      t109 = 0.1D1 / x1
      t110 = t108 * t109
      t111 = t105 * t106 * t110
      t114 = FJET(XB1, XB2, s, -t38, -t44, t49, -t53, t59, t62 * t65 * t
     #67 * t111 / 0.32D2)
      t116 = t10 * t65
      t120 = t106 * t108 * t109
      t125 = 0.1D1 / t80 / z
      t126 = Sin(t7)
      t127 = t126 ** 2
      t128 = t125 * t127
      t131 = log(0.4D1 * t76 * t128)
      t132 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t134 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t138 = t61 * lh
      t145 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t146 = -t145 + t132
      t152 = log(0.4D1 * t82 * t128)
      t154 = t41 ** 2
      t155 = t128 * t154
      t158 = log(0.4D1 * t82 * t155)
      t160 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t165 = 0.180D3 * t138 * t146
      t170 = t74 * t127
      t173 = log(0.4D1 * t170 * t125)
      t179 = t173 ** 2
      t185 = lh ** 2
      t186 = 0.180D3 * t185
      t187 = 0.3141592653589793D1 ** 2
      t188 = 0.30D2 * t187
      t189 = t186 - t188
      t190 = t61 * t189
      t191 = t190 * t132
      t196 = log(0.4D1 * t128)
      t199 = t196 ** 2
      t202 = (0.180D3 * t196 * lh + t186 - t188 + 0.45D2 * t199) * t61
      t207 = log(0.4D1 * t6 * t128)
      t211 = log(0.4D1 * t6 * t155)
      t220 = x2 * t125
      t221 = t127 * t154
      t224 = log(0.4D1 * t220 * t221)
      t228 = log(0.4D1 * t220 * t127)
      t233 = t228 ** 2
      t237 = t224 ** 2
      t258 = (-t196 * t189 - 0.90D2 * t199 * lh - 0.2884936567583026D3 -
     # 0.120D3 * t185 * lh + 0.60D2 * lh * t187 - 0.15D2 * t199 * t196) 
     #* t61
      t264 = log(0.4D1 * x3 * t125 * t127)
      t269 = t264 ** 2
      t279 = (0.90D2 * t61 * (t131 * t132 - t134) + 0.180D3 * t138 * t13
     #2) * t106 * t109 / 0.2880D4 - t61 * t146 * t120 / 0.32D2 - (0.90D2
     # * t61 * (-t152 * t132 + t134 + t158 * t145 - t160) - t165) * t108
     # * t109 / 0.2880D4 - (-0.180D3 * t138 * (-t173 * t132 + t134) + 0.
     #90D2 * t61 * (-t173 * t134 + t179 * t132 / 0.2D1) + t191) * t109 /
     # 0.2880D4 - t202 * t134 / 0.5760D4 - (0.90D2 * t61 * (t134 - t207 
     #* t132 - t160 + t211 * t145) - t165) * t106 * t108 / 0.5760D4 - (-
     #0.180D3 * t138 * (-t160 + t134 + t224 * t145 - t228 * t132) + 0.90
     #D2 * t61 * (t233 * t132 / 0.2D1 - t228 * t134 - t237 * t145 / 0.2D
     #1 + t224 * t160) + t190 * t146) * t108 / 0.5760D4 - t258 * t132 / 
     #0.5760D4 + (-0.180D3 * t138 * (-t134 + t264 * t132) + 0.90D2 * t61
     # * (-t269 * t132 / 0.2D1 + t264 * t134) - t191) * t106 / 0.5760D4
      t280 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t279)
      t282 = t76 * t127
      t284 = 0.1D1 / t80 * t33
      t285 = t39 ** 2
      t286 = t284 * t285
      t289 = log(-0.4D1 * t282 * t286)
      t290 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t292 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t297 = 0.180D3 * t138 * t290
      t305 = t82 * t127
      t308 = log(-0.4D1 * t305 * t286)
      t319 = log(-0.4D1 * t170 * t286)
      t324 = t319 ** 2
      t335 = (0.90D2 * t61 * (-t289 * t290 + t292) - t297) * t106 * t109
     # / 0.2880D4 + t61 * t290 * t120 / 0.32D2 - (0.90D2 * t61 * (-t292 
     #+ t308 * t290) + t297) * t108 * t109 / 0.2880D4 - (-0.180D3 * t138
     # * (t319 * t290 - t292) + 0.90D2 * t61 * (-t324 * t290 / 0.2D1 + t
     #319 * t292) - t190 * t290) * t109 / 0.2880D4
      t336 = FJET(XB1, XB2, s, t3, -t40, 0.0D0, 0.0D0, 0.0D0, t335)
      t339 = t2 * t63 * t33
      t341 = t1 * x1
      t342 = t41 * s * t341
      t343 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t351 = log(-0.4D1 * t305 * t284 * t285 * t154)
      t353 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t363 = -t61 * t343 * t120 / 0.32D2 - (0.90D2 * t61 * (-t351 * t343
     # + t353) - 0.180D3 * t138 * t343) * t108 * t109 / 0.2880D4
      t364 = FJET(XB1, XB2, s, 0.0D0, -t339, -t40, -t342, t59, t363)
      t366 = t2 * t66
      t367 = t2 * t43
      t368 = t61 * z
      t369 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t372 = Sqrt(-t4 * t13)
      t376 = 0.1D1 / (-z - t6 + 0.2D1 * t8 * t372)
      t378 = t376 * t106 * t110
      t381 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t384 = t35 ** 2
      t390 = log(-0.4D1 * t6 * t125 * t221 * t12 / t384)
      t391 = t390 * z
      t405 = t368 * t369 * t378 / 0.32D2 - (-0.90D2 * t61 * (z * t381 - 
     #t391 * t369) * t376 + 0.180D3 * t138 * z * t369 * t376) * t106 * t
     #108 / 0.5760D4
      t406 = FJET(XB1, XB2, s, t366, 0.0D0, t367, 0.0D0, 0.0D0, t405)
      t411 = (-0.180D3 * lh - 0.90D2 * t196) * t61
      t412 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t415 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t418 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t429 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t430 = -t429 + t418
      t435 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t441 = 0.180D3 * t138 * t430
      t456 = t190 * t418
      t476 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t503 = -t411 * t412 / 0.5760D4 - t202 * t415 / 0.5760D4 + (0.90D2 
     #* t61 * (t131 * t418 - t415) + 0.180D3 * t138 * t418) * t106 * t10
     #9 / 0.2880D4 - t61 * t430 * t120 / 0.32D2 - (0.90D2 * t61 * (-t152
     # * t418 - t435 + t158 * t429 + t415) - t441) * t108 * t109 / 0.288
     #0D4 - (-0.180D3 * t138 * (-t173 * t418 + t415) + 0.90D2 * t61 * (t
     #412 - t173 * t415 + t179 * t418 / 0.2D1) + t456) * t109 / 0.2880D4
     # - (0.90D2 * t61 * (-t435 - t207 * t418 + t415 + t211 * t429) - t4
     #41) * t106 * t108 / 0.5760D4 - (-0.180D3 * t138 * (-t435 + t415 + 
     #t224 * t429 - t228 * t418) + 0.90D2 * t61 * (t233 * t418 / 0.2D1 -
     # t476 + t412 - t228 * t415 - t237 * t429 / 0.2D1 + t224 * t435) + 
     #t190 * t430) * t108 / 0.5760D4 - t258 * t418 / 0.5760D4 + (-0.180D
     #3 * t138 * (t264 * t418 - t415) + 0.90D2 * t61 * (-t412 + t264 * t
     #415 - t269 * t418 / 0.2D1) - t456) * t106 / 0.5760D4
      t504 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t503)
      t506 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t508 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t513 = 0.180D3 * t138 * t506
      t536 = gbgbH52J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t544 = (0.90D2 * t61 * (-t289 * t506 + t508) - t513) * t106 * t109
     # / 0.2880D4 + t61 * t506 * t120 / 0.32D2 - (0.90D2 * t61 * (t308 *
     # t506 - t508) + t513) * t108 * t109 / 0.2880D4 - (-0.180D3 * t138 
     #* (-t508 + t319 * t506) + 0.90D2 * t61 * (-t324 * t506 / 0.2D1 + t
     #319 * t508 - t536) - t190 * t506) * t109 / 0.2880D4
      t545 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t40, 0.0D0, t544)
      t547 = t2 * x3
      t548 = t2 * t12
      t549 = -t12
      t550 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t551 = t128 * t12
      t554 = log(-0.4D1 * t76 * t551)
      t555 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t561 = 0.180D3 * t138 * t555
      t571 = log(-0.4D1 * t6 * t551)
      t583 = log(-0.4D1 * t128 * x3 * t12)
      t588 = t583 ** 2
      t599 = (0.90D2 * t61 * (t550 - t554 * t555) - t561) * t106 * t109 
     #/ 0.2880D4 + t61 * t555 * t120 / 0.32D2 - (0.90D2 * t61 * (-t550 +
     # t571 * t555) + t561) * t106 * t108 / 0.5760D4 + (-0.180D3 * t138 
     #* (t550 - t583 * t555) + 0.90D2 * t61 * (t588 * t555 / 0.2D1 - t58
     #3 * t550) + t190 * t555) * t106 / 0.5760D4
      t600 = FJET(XB1, XB2, s, t547, 0.0D0, -t548, 0.0D0, 0.0D0, t599)
      t602 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t604 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t609 = 0.180D3 * t138 * t602
      t631 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t640 = (0.90D2 * t61 * (-t554 * t602 + t604) - t609) * t106 * t109
     # / 0.2880D4 + t61 * t602 * t120 / 0.32D2 - (0.90D2 * t61 * (-t604 
     #+ t571 * t602) + t609) * t106 * t108 / 0.5760D4 + (-0.180D3 * t138
     # * (t604 - t583 * t602) + 0.90D2 * t61 * (t588 * t602 / 0.2D1 + t6
     #31 - t583 * t604) + t190 * t602) * t106 / 0.5760D4
      t641 = FJET(XB1, XB2, s, -t548, 0.0D0, t547, 0.0D0, 0.0D0, t640)
      t644 = t2 * t39 * x3
      t645 = t2 * t5
      t646 = t50 * t51
      t647 = t50 * t341
      t652 = log(0.4D1 * t282 * t284 * t285 * t12)
      t653 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t655 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t668 = (0.90D2 * t61 * (t652 * t653 - t655) + 0.180D3 * t138 * t65
     #3) * t106 * t109 / 0.2880D4 - t61 * t653 * t120 / 0.32D2
      t669 = FJET(XB1, XB2, s, -t644, t645, t646, -t647, 0.0D0, t668)
      t671 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t672 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t678 = 0.180D3 * t138 * t672
      t693 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t709 = (0.90D2 * t61 * (t671 - t554 * t672) - t678) * t106 * t109 
     #/ 0.2880D4 + t61 * t672 * t120 / 0.32D2 + (-0.180D3 * t138 * (t671
     # - t583 * t672) + 0.90D2 * t61 * (t588 * t672 / 0.2D1 - t583 * t67
     #1 + t693) + t190 * t672) * t106 / 0.5760D4 - (0.90D2 * t61 * (-t67
     #1 + t571 * t672) + t678) * t106 * t108 / 0.5760D4
      t710 = FJET(XB1, XB2, s, 0.0D0, t547, 0.0D0, -t548, 0.0D0, t709)
      t712 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t714 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t724 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t725 = -t724 + t712
      t730 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t736 = 0.180D3 * t138 * t725
      t751 = t190 * t712
      t799 = (0.90D2 * t61 * (t131 * t712 - t714) + 0.180D3 * t138 * t71
     #2) * t106 * t109 / 0.2880D4 - t61 * t725 * t120 / 0.32D2 - (0.90D2
     # * t61 * (-t152 * t712 + t714 - t730 + t158 * t724) - t736) * t108
     # * t109 / 0.2880D4 - (-0.180D3 * t138 * (t714 - t173 * t712) + 0.9
     #0D2 * t61 * (t179 * t712 / 0.2D1 - t173 * t714) + t751) * t109 / 0
     #.2880D4 + (-0.180D3 * t138 * (-t714 + t264 * t712) + 0.90D2 * t61 
     #* (-t269 * t712 / 0.2D1 + t264 * t714) - t751) * t106 / 0.5760D4 -
     # t202 * t714 / 0.5760D4 - (0.90D2 * t61 * (-t207 * t712 - t730 + t
     #211 * t724 + t714) - t736) * t106 * t108 / 0.5760D4 - (-0.180D3 * 
     #t138 * (t224 * t724 - t730 + t714 - t228 * t712) + 0.90D2 * t61 * 
     #(t233 * t712 / 0.2D1 + t224 * t730 - t228 * t714 - t237 * t724 / 0
     #.2D1) + t190 * t725) * t108 / 0.5760D4 - t258 * t712 / 0.5760D4
      t800 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t799)
      t802 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t806 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t821 = t368 * t802 * t378 / 0.32D2 - (-0.90D2 * t61 * (z * t806 - 
     #t391 * t802) * t376 + 0.180D3 * t138 * z * t802 * t376) * t106 * t
     #108 / 0.5760D4
      t822 = FJET(XB1, XB2, s, 0.0D0, t366, 0.0D0, t367, 0.0D0, t821)
      t824 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t829 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t839 = -t61 * t824 * t120 / 0.32D2 - (0.90D2 * t61 * (-t351 * t824
     # + t829) - 0.180D3 * t138 * t824) * t108 * t109 / 0.2880D4
      t840 = FJET(XB1, XB2, s, -t40, -t342, 0.0D0, -t339, t59, t839)
      t842 = t114 * t61 * t116 * t67 * t105 * t120 / 0.32D2 + t280 * t27
     #9 + t336 * t335 + t364 * t363 + t406 * t405 + t504 * t503 + t545 *
     # t544 + t600 * t599 + t641 * t640 + t669 * t668 + t710 * t709 + t8
     #00 * t799 + t822 * t821 + t840 * t839
      t843 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t847 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t858 = -t61 * t843 * t120 / 0.32D2 - (0.90D2 * t61 * (t847 - t351 
     #* t843) - 0.180D3 * t138 * t843) * t108 * t109 / 0.2880D4
      t859 = FJET(XB1, XB2, s, -t339, 0.0D0, -t342, -t40, t59, t858)
      t861 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t863 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t873 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t874 = -t873 + t861
      t879 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t885 = 0.180D3 * t138 * t874
      t895 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t901 = t190 * t861
      t936 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t952 = (0.90D2 * t61 * (t131 * t861 - t863) + 0.180D3 * t138 * t86
     #1) * t106 * t109 / 0.2880D4 - t61 * t874 * t120 / 0.32D2 - (0.90D2
     # * t61 * (-t152 * t861 + t863 - t879 + t158 * t873) - t885) * t108
     # * t109 / 0.2880D4 - (-0.180D3 * t138 * (t863 - t173 * t861) + 0.9
     #0D2 * t61 * (-t173 * t863 + t895 + t179 * t861 / 0.2D1) + t901) * 
     #t109 / 0.2880D4 + (-0.180D3 * t138 * (-t863 + t264 * t861) + 0.90D
     #2 * t61 * (t264 * t863 - t895 - t269 * t861 / 0.2D1) - t901) * t10
     #6 / 0.5760D4 - (0.90D2 * t61 * (-t207 * t861 + t863 - t879 + t211 
     #* t873) - t885) * t106 * t108 / 0.5760D4 - (-0.180D3 * t138 * (t22
     #4 * t873 - t879 + t863 - t228 * t861) + 0.90D2 * t61 * (t233 * t86
     #1 / 0.2D1 - t228 * t863 + t895 + t224 * t879 - t936 - t237 * t873 
     #/ 0.2D1) + t190 * t874) * t108 / 0.5760D4 - t411 * t895 / 0.5760D4
     # - t202 * t863 / 0.5760D4 - t258 * t861 / 0.5760D4
      t953 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t952)
      t955 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t956 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t962 = 0.180D3 * t138 * t956
      t984 = gbgbH54J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t993 = (0.90D2 * t61 * (t955 - t289 * t956) - t962) * t106 * t109 
     #/ 0.2880D4 + t61 * t956 * t120 / 0.32D2 - (0.90D2 * t61 * (t308 * 
     #t956 - t955) + t962) * t108 * t109 / 0.2880D4 - (-0.180D3 * t138 *
     # (-t955 + t319 * t956) + 0.90D2 * t61 * (-t324 * t956 / 0.2D1 - t9
     #84 + t319 * t955) - t190 * t956) * t109 / 0.2880D4
      t994 = FJET(XB1, XB2, s, -t40, t3, 0.0D0, 0.0D0, 0.0D0, t993)
      t996 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t997 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t549, x4)
      t1003 = 0.180D3 * t138 * t997
      t1033 = (0.90D2 * t61 * (t996 - t554 * t997) - t1003) * t106 * t10
     #9 / 0.2880D4 + t61 * t997 * t120 / 0.32D2 - (0.90D2 * t61 * (-t996
     # + t571 * t997) + t1003) * t106 * t108 / 0.5760D4 + (-0.180D3 * t1
     #38 * (-t583 * t997 + t996) + 0.90D2 * t61 * (-t583 * t996 + t588 *
     # t997 / 0.2D1) + t190 * t997) * t106 / 0.5760D4
      t1034 = FJET(XB1, XB2, s, 0.0D0, -t548, 0.0D0, t547, 0.0D0, t1033)
      t1036 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t1037 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t1051 = (0.90D2 * t61 * (-t1036 + t652 * t1037) + 0.180D3 * t138 *
     # t1037) * t106 * t109 / 0.2880D4 - t61 * t1037 * t120 / 0.32D2
      t1052 = FJET(XB1, XB2, s, -t647, t646, t645, -t644, 0.0D0, t1051)
      t1054 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1059 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1069 = -t61 * t1054 * t120 / 0.32D2 - (0.90D2 * t61 * (-t351 * t1
     #054 + t1059) - 0.180D3 * t138 * t1054) * t108 * t109 / 0.2880D4
      t1070 = FJET(XB1, XB2, s, -t342, -t40, -t339, 0.0D0, t59, t1069)
      t1072 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t1073 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t1087 = (0.90D2 * t61 * (-t1072 + t652 * t1073) + 0.180D3 * t138 *
     # t1073) * t106 * t109 / 0.2880D4 - t61 * t1073 * t120 / 0.32D2
      t1088 = FJET(XB1, XB2, s, t645, -t644, -t647, t646, 0.0D0, t1087)
      t1090 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t1092 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t549, x4)
      t1105 = (0.90D2 * t61 * (t652 * t1090 - t1092) + 0.180D3 * t138 * 
     #t1090) * t106 * t109 / 0.2880D4 - t61 * t1090 * t120 / 0.32D2
      t1106 = FJET(XB1, XB2, s, t646, -t647, -t644, t645, 0.0D0, t1105)
      t1108 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t1112 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t1127 = t368 * t1108 * t378 / 0.32D2 - (-0.90D2 * t61 * (z * t1112
     # - t391 * t1108) * t376 + 0.180D3 * t138 * z * t1108 * t376) * t10
     #6 * t108 / 0.5760D4
      t1128 = FJET(XB1, XB2, s, t367, 0.0D0, t366, 0.0D0, 0.0D0, t1127)
      t1130 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1132 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1137 = 0.180D3 * t138 * t1130
      t1167 = (0.90D2 * t61 * (-t289 * t1130 + t1132) - t1137) * t106 * 
     #t109 / 0.2880D4 + t61 * t1130 * t120 / 0.32D2 - (0.90D2 * t61 * (t
     #308 * t1130 - t1132) + t1137) * t108 * t109 / 0.2880D4 - (-0.180D3
     # * t138 * (t319 * t1130 - t1132) + 0.90D2 * t61 * (t319 * t1132 - 
     #t324 * t1130 / 0.2D1) - t190 * t1130) * t109 / 0.2880D4
      t1168 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t40, t3, 0.0D0, t1167)
      t1170 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t1174 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t66, x4)
      t1189 = t368 * t1170 * t378 / 0.32D2 - (-0.90D2 * t61 * (z * t1174
     # - t391 * t1170) * t376 + 0.180D3 * t138 * z * t1170 * t376) * t10
     #6 * t108 / 0.5760D4
      t1190 = FJET(XB1, XB2, s, 0.0D0, t367, 0.0D0, t366, 0.0D0, t1189)
      t1192 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t66, x4)
      t1197 = FJET(XB1, XB2, s, t49, -t53, -t38, -t44, t59, t62 * t65 * 
     #t1192 * t111 / 0.32D2)
      t1204 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, t66, x4)
      t1209 = FJET(XB1, XB2, s, -t44, -t38, -t53, t49, t59, t62 * t65 * 
     #t1204 * t111 / 0.32D2)
      t1216 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, t66, x4)
      t1221 = FJET(XB1, XB2, s, -t53, t49, -t44, -t38, t59, t62 * t65 * 
     #t1216 * t111 / 0.32D2)
      t1228 = t859 * t858 + t953 * t952 + t994 * t993 + t1034 * t1033 + 
     #t1052 * t1051 + t1070 * t1069 + t1088 * t1087 + t1106 * t1105 + t1
     #128 * t1127 + t1168 * t1167 + t1190 * t1189 + t1197 * t61 * t116 *
     # t1192 * t105 * t120 / 0.32D2 + t1209 * t61 * t116 * t1204 * t105 
     #* t120 / 0.32D2 + t1221 * t61 * t116 * t1216 * t105 * t120 / 0.32D
     #2
      gbgbH5n4e0 = t842 + t1228

      end function



      doubleprecision function gbgbH5n4em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t22 = t4 * lh
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t31 = t30 * t26
      t34 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t35 = -t34 + t17
      t36 = t4 * t35
      t37 = 0.1D1 / x2
      t38 = t37 * t26
      t41 = t30 * t37
      t44 = x2 * t13
      t45 = -0.1D1 + x2
      t46 = t45 ** 2
      t50 = log(0.4D1 * t44 * t9 * t46)
      t52 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t55 = log(0.4D1 * t44 * t9)
      t65 = gbgbH52J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t69 = t13 * t9
      t71 = log(0.4D1 * t69)
      t74 = (-0.180D3 * lh - 0.90D2 * t71) * t4
      t79 = lh ** 2
      t81 = 0.3141592653589793D1 ** 2
      t83 = t71 ** 2
      t86 = (0.180D3 * t71 * lh + 0.180D3 * t79 - 0.30D2 * t81 + 0.45D2 
     #* t83) * t4
      t92 = log(0.4D1 * x3 * t13 * t9)
      t100 = -(0.90D2 * t4 * (t5 - t16 * t17) - t24) * t26 / 0.2880D4 - 
     #t4 * t17 * t31 / 0.32D2 - t36 * t38 / 0.32D2 - t36 * t41 / 0.64D2 
     #- (0.90D2 * t4 * (t50 * t34 - t52 + t5 - t55 * t17) - 0.180D3 * t2
     #2 * t35) * t37 / 0.5760D4 - t4 * t65 / 0.64D2 - t74 * t5 / 0.5760D
     #4 - t86 * t17 / 0.5760D4 + (0.90D2 * t4 * (-t5 + t92 * t17) + t24)
     # * t30 / 0.5760D4
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t100)
      t103 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t106 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t107 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t108 = -t106 + t107
      t109 = t4 * t108
      t113 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t130 = 0.180D3 * t22 * t107
      t146 = -t74 * t103 / 0.5760D4 - t109 * t41 / 0.64D2 - (0.90D2 * t4
     # * (t50 * t106 - t113 + t103 - t55 * t107) - 0.180D3 * t22 * t108)
     # * t37 / 0.5760D4 - t86 * t107 / 0.5760D4 - (0.90D2 * t4 * (t103 -
     # t16 * t107) - t130) * t26 / 0.2880D4 - t4 * t107 * t31 / 0.32D2 -
     # t109 * t38 / 0.32D2 + (0.90D2 * t4 * (-t103 + t92 * t107) + t130)
     # * t30 / 0.5760D4
      t147 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t146)
      t149 = t2 * x1
      t150 = -0.1D1 + x1
      t151 = t2 * t150
      t152 = gbgbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t156 = 0.1D1 / (-z - x1 + x1 * z)
      t158 = t150 ** 2
      t162 = log(-0.4D1 * t10 / t11 * t156 * t158)
      t163 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t173 = t4 * t163
      t178 = -(0.90D2 * t4 * (-t152 + t162 * t163) + 0.180D3 * t22 * t16
     #3) * t26 / 0.2880D4 + t173 * t31 / 0.32D2 + t173 * t38 / 0.32D2
      t179 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t149, -t151, 0.0D0, t178)
      t181 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t183 = gbgbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t192 = t4 * t181
      t197 = -(0.90D2 * t4 * (t162 * t181 - t183) + 0.180D3 * t22 * t181
     #) * t26 / 0.2880D4 + t192 * t31 / 0.32D2 + t192 * t38 / 0.32D2
      t198 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t151, t149, 0.0D0, t197)
      t200 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t203 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t206 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t207 = -t206 + t203
      t208 = t4 * t207
      t211 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t227 = 0.180D3 * t22 * t203
      t243 = -t74 * t200 / 0.5760D4 - t86 * t203 / 0.5760D4 - t208 * t41
     # / 0.64D2 - (0.90D2 * t4 * (-t211 + t200 + t50 * t206 - t55 * t203
     #) - 0.180D3 * t22 * t207) * t37 / 0.5760D4 - (0.90D2 * t4 * (-t16 
     #* t203 + t200) - t227) * t26 / 0.2880D4 - t4 * t203 * t31 / 0.32D2
     # - t208 * t38 / 0.32D2 + (0.90D2 * t4 * (-t200 + t92 * t203) + t22
     #7) * t30 / 0.5760D4
      t244 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t243)
      t246 = t2 * x3
      t247 = -0.1D1 + x3
      t248 = t2 * t247
      t249 = -t247
      t250 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t251 = t4 * t250
      t256 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t260 = log(-0.4D1 * t69 * x3 * t247)
      t270 = t251 * t31 / 0.32D2 + t251 * t41 / 0.64D2 + (0.90D2 * t4 * 
     #(t256 - t260 * t250) - 0.180D3 * t22 * t250) * t30 / 0.5760D4
      t271 = FJET(XB1, XB2, s, 0.0D0, t246, 0.0D0, -t248, 0.0D0, t270)
      t273 = x2 * x3
      t275 = 0.1D1 / (t273 - 0.1D1)
      t276 = t247 * t275
      t277 = t2 * t276
      t280 = t2 * x3 * t45 * t275
      t281 = t4 * z
      t282 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t276, x4)
      t284 = cos(t7)
      t288 = Sqrt(-x3 * z * x2 * t247)
      t292 = 0.1D1 / (-z - t273 + 0.2D1 * t284 * t288)
      t294 = t292 * t30 * t37
      t297 = FJET(XB1, XB2, s, 0.0D0, t277, 0.0D0, t280, 0.0D0, t281 * t
     #282 * t294 / 0.64D2)
      t304 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t276, x4)
      t308 = FJET(XB1, XB2, s, 0.0D0, t280, 0.0D0, t277, 0.0D0, t281 * t
     #304 * t294 / 0.64D2)
      t315 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t316 = t4 * t315
      t322 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t331 = t316 * t31 / 0.32D2 + t316 * t41 / 0.64D2 + (0.90D2 * t4 * 
     #(-t260 * t315 + t322) - 0.180D3 * t22 * t315) * t30 / 0.5760D4
      t332 = FJET(XB1, XB2, s, 0.0D0, -t248, 0.0D0, t246, 0.0D0, t331)
      t336 = t2 * x1 * x2 * t156
      t338 = t1 * x1
      t339 = t45 * s * t338
      t340 = t1 ** 2
      t345 = s * t340 * x2 * x1 * t150 * t156
      t346 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t350 = FJET(XB1, XB2, s, 0.0D0, -t336, -t151, -t339, t345, -t4 * t
     #346 * t38 / 0.32D2)
      t356 = gbgbH54J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t359 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t363 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t368 = 0.180D3 * t22 * t359
      t375 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t376 = -t375 + t359
      t377 = t4 * t376
      t382 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t402 = -t4 * t356 / 0.64D2 - t86 * t359 / 0.5760D4 - (0.90D2 * t4 
     #* (-t16 * t359 + t363) - t368) * t26 / 0.2880D4 - t4 * t359 * t31 
     #/ 0.32D2 - t377 * t38 / 0.32D2 - t377 * t41 / 0.64D2 - (0.90D2 * t
     #4 * (-t382 + t363 + t50 * t375 - t55 * t359) - 0.180D3 * t22 * t37
     #6) * t37 / 0.5760D4 - t74 * t363 / 0.5760D4 + (0.90D2 * t4 * (t92 
     #* t359 - t363) + t368) * t30 / 0.5760D4
      t403 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t402)
      t405 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t407 = gbgbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t416 = t4 * t405
      t421 = -(0.90D2 * t4 * (t162 * t405 - t407) + 0.180D3 * t22 * t405
     #) * t26 / 0.2880D4 + t416 * t31 / 0.32D2 + t416 * t38 / 0.32D2
      t422 = FJET(XB1, XB2, s, t149, -t151, 0.0D0, 0.0D0, 0.0D0, t421)
      t424 = t101 * t100 + t147 * t146 + t179 * t178 + t198 * t197 + t24
     #4 * t243 + t271 * t270 + t297 * t4 * z * t282 * t292 * t41 / 0.64D
     #2 + t308 * t4 * z * t304 * t292 * t41 / 0.64D2 + t332 * t331 - t35
     #0 * t4 * t346 * t37 * t26 / 0.32D2 + t403 * t402 + t422 * t421
      t425 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t426 = t4 * t425
      t431 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t441 = t426 * t31 / 0.32D2 + t426 * t41 / 0.64D2 + (0.90D2 * t4 * 
     #(t431 - t260 * t425) - 0.180D3 * t22 * t425) * t30 / 0.5760D4
      t442 = FJET(XB1, XB2, s, t246, 0.0D0, -t248, 0.0D0, 0.0D0, t441)
      t445 = t2 * x1 * x3
      t447 = t2 * t150 * x3
      t448 = t247 * s
      t449 = t448 * t338
      t451 = t448 * t1 * t150
      t452 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t249, x4)
      t456 = FJET(XB1, XB2, s, t445, -t447, -t449, t451, 0.0D0, -t4 * t4
     #52 * t31 / 0.32D2)
      t462 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t276, x4)
      t466 = FJET(XB1, XB2, s, t277, 0.0D0, t280, 0.0D0, 0.0D0, t281 * t
     #462 * t294 / 0.64D2)
      t473 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t249, x4)
      t477 = FJET(XB1, XB2, s, t451, -t449, -t447, t445, 0.0D0, -t4 * t4
     #73 * t31 / 0.32D2)
      t483 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t276, x4)
      t487 = FJET(XB1, XB2, s, t280, 0.0D0, t277, 0.0D0, 0.0D0, t281 * t
     #483 * t294 / 0.64D2)
      t494 = gbgbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t495 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t505 = t4 * t495
      t510 = -(0.90D2 * t4 * (-t494 + t162 * t495) + 0.180D3 * t22 * t49
     #5) * t26 / 0.2880D4 + t505 * t31 / 0.32D2 + t505 * t38 / 0.32D2
      t511 = FJET(XB1, XB2, s, -t151, t149, 0.0D0, 0.0D0, 0.0D0, t510)
      t513 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t517 = FJET(XB1, XB2, s, -t151, -t339, 0.0D0, -t336, t345, -t4 * t
     #513 * t38 / 0.32D2)
      t523 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t524 = t4 * t523
      t529 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t249, x4)
      t539 = t524 * t31 / 0.32D2 + t524 * t41 / 0.64D2 + (0.90D2 * t4 * 
     #(t529 - t260 * t523) - 0.180D3 * t22 * t523) * t30 / 0.5760D4
      t540 = FJET(XB1, XB2, s, -t248, 0.0D0, t246, 0.0D0, 0.0D0, t539)
      t542 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t249, x4)
      t546 = FJET(XB1, XB2, s, -t447, t445, t451, -t449, 0.0D0, -t4 * t5
     #42 * t31 / 0.32D2)
      t552 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t556 = FJET(XB1, XB2, s, -t339, -t151, -t336, 0.0D0, t345, -t4 * t
     #552 * t38 / 0.32D2)
      t562 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t249, x4)
      t566 = FJET(XB1, XB2, s, -t449, t451, t445, -t447, 0.0D0, -t4 * t5
     #62 * t31 / 0.32D2)
      t572 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t576 = FJET(XB1, XB2, s, -t336, 0.0D0, -t339, -t151, t345, -t4 * t
     #572 * t38 / 0.32D2)
      t582 = t442 * t441 - t456 * t4 * t452 * t30 * t26 / 0.32D2 + t466 
     #* t4 * z * t462 * t292 * t41 / 0.64D2 - t477 * t4 * t473 * t30 * t
     #26 / 0.32D2 + t487 * t4 * z * t483 * t292 * t41 / 0.64D2 + t511 * 
     #t510 - t517 * t4 * t513 * t37 * t26 / 0.32D2 + t540 * t539 - t546 
     #* t4 * t542 * t30 * t26 / 0.32D2 - t556 * t4 * t552 * t37 * t26 / 
     #0.32D2 - t566 * t4 * t562 * t30 * t26 / 0.32D2 - t576 * t4 * t572 
     #* t37 * t26 / 0.32D2
      gbgbH5n4em1 = t424 + t582

      end function



      doubleprecision function gbgbH5n4em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t13 = 0.1D1 / x2
      t16 = gbgbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t20 = z ** 2
      t24 = Sin(x4 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t31 = (-0.180D3 * lh - 0.90D2 * t28) * t4
      t34 = 0.1D1 / x3
      t37 = -t6 * t7 / 0.32D2 - t4 * (-t10 + t5) * t13 / 0.64D2 - t4 * t
     #16 / 0.64D2 - t31 * t5 / 0.5760D4 - t6 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t40 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t41 = t4 * t40
      t44 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t49 = gbgbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t56 = -t41 * t7 / 0.32D2 - t4 * (-t44 + t40) * t13 / 0.64D2 - t4 *
     # t49 / 0.64D2 - t31 * t40 / 0.5760D4 - t41 * t34 / 0.64D2
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t56)
      t59 = t2 * x1
      t61 = t2 * (-0.1D1 + x1)
      t62 = gbgbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t59, -t61, 0.0D0, t4 * t62 *
     # t7 / 0.32D2)
      t71 = gbgbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t75 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t61, t59, 0.0D0, t4 * t71 *
     # t7 / 0.32D2)
      t80 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t81 = t4 * t80
      t88 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t93 = gbgbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t96 = -t81 * t7 / 0.32D2 - t31 * t80 / 0.5760D4 - t81 * t34 / 0.64
     #D2 - t4 * (-t88 + t80) * t13 / 0.64D2 - t4 * t93 / 0.64D2
      t97 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t96)
      t99 = t2 * x3
      t100 = -0.1D1 + x3
      t101 = t2 * t100
      t102 = -t100
      t103 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t102, x4)
      t107 = FJET(XB1, XB2, s, 0.0D0, t99, 0.0D0, -t101, 0.0D0, t4 * t10
     #3 * t34 / 0.64D2)
      t112 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t102, x4)
      t116 = FJET(XB1, XB2, s, 0.0D0, -t101, 0.0D0, t99, 0.0D0, t4 * t11
     #2 * t34 / 0.64D2)
      t121 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t122 = t4 * t121
      t129 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t134 = gbgbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t137 = -t122 * t7 / 0.32D2 - t31 * t121 / 0.5760D4 - t122 * t34 / 
     #0.64D2 - t4 * (-t129 + t121) * t13 / 0.64D2 - t4 * t134 / 0.64D2
      t138 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t137)
      t140 = gbgbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t144 = FJET(XB1, XB2, s, t59, -t61, 0.0D0, 0.0D0, 0.0D0, t4 * t140
     # * t7 / 0.32D2)
      t149 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t102, x4)
      t153 = FJET(XB1, XB2, s, t99, 0.0D0, -t101, 0.0D0, 0.0D0, t4 * t14
     #9 * t34 / 0.64D2)
      t158 = gbgbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t162 = FJET(XB1, XB2, s, -t61, t59, 0.0D0, 0.0D0, 0.0D0, t4 * t158
     # * t7 / 0.32D2)
      t167 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t102, x4)
      t171 = FJET(XB1, XB2, s, -t101, 0.0D0, t99, 0.0D0, 0.0D0, t4 * t16
     #7 * t34 / 0.64D2)
      gbgbH5n4em2 = t38 * t37 + t57 * t56 + t66 * t4 * t62 * t7 / 0.32D2
     # + t75 * t4 * t71 * t7 / 0.32D2 + t97 * t96 + t107 * t4 * t103 * t
     #34 / 0.64D2 + t116 * t4 * t112 * t34 / 0.64D2 + t138 * t137 + t144
     # * t4 * t140 * t7 / 0.32D2 + t153 * t4 * t149 * t34 / 0.64D2 + t16
     #2 * t4 * t158 * t7 / 0.32D2 + t171 * t4 * t167 * t34 / 0.64D2

      end function



      doubleprecision function gbgbH5n4em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = gbgbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t4 * t5 / 
     #0.64D2)
      t11 = gbgbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t4 * t11 
     #/ 0.64D2)
      t17 = gbgbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t4 * t17 
     #/ 0.64D2)
      t23 = gbgbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t4 * t23 
     #/ 0.64D2)
      gbgbH5n4em3 = -t8 * t4 * t5 / 0.64D2 - t14 * t4 * t11 / 0.64D2 - t
     #20 * t4 * t17 / 0.64D2 - t26 * t4 * t23 / 0.64D2

      end function



      doubleprecision function gbgbH5n4em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision gbgbH51J1
      doubleprecision gbgbH51J2
      doubleprecision gbgbH52J1
      doubleprecision gbgbH52J2
      doubleprecision gbgbH52J3
      doubleprecision gbgbH53J1
      doubleprecision gbgbH53J2
      doubleprecision gbgbH54J1
      doubleprecision gbgbH54J2
      doubleprecision gbgbH54J3
      gbgbH5n4em4 = 0.0D0

      end function
  
 

      doubleprecision function gbgbH51J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t2 * t3
      t5 = 0.1D1 - x1
      t6 = 0.1D1 - x3
      t7 = t5 * t6
      t10 = s * t3
      t12 = z + x1 * t3
      t13 = 0.1D1 / t12
      t14 = x1 * t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t20 = cos(x4 * 0.3141592653589793D1)
      t24 = Sqrt(t16 * t12 * x2 * t6)
      t26 = 0.2D1 * t20 * t24
      t32 = s - t10 * t14 * (t16 * t12 + x2 * t6 - t26) - t10 * t5 * x3
      t34 = t32 * t1 * s
      t35 = t3 ** 2
      t36 = t34 * t35
      t37 = t5 ** 2
      t38 = t37 * t6
      t42 = t35 * t3
      t46 = x3 ** 2
      t51 = t34 * t35 * t5
      t52 = t6 * x1
      t56 = t6 * t15 * t12 + x2 * x3 + t26
      t57 = t13 * t56
      t58 = t52 * t57
      t61 = t7 * z
      t64 = t2 * t35
      t72 = x1 ** 2
      t74 = t12 ** 2
      t76 = t56 ** 2
      t78 = t6 * t72 / t74 * t76
      t85 = t34 * t3
      t93 = z ** 2
      t94 = t7 * t93
      t116 = 0.9D1 * t4 * t7 - 0.18D2 * t36 * t38 * x3 + 0.9D1 * t34 * t
     #42 * t37 * t5 * t6 * t46 - 0.18D2 * t51 * t58 - 0.18D2 * t4 * t61 
     #+ 0.18D2 * t64 * t7 * z * x1 * t57 + 0.9D1 * t2 * t42 * t5 * t78 +
     # 0.18D2 * t36 * t38 * z * x3 - 0.18D2 * t85 * t61 + 0.18D2 * t51 *
     # t6 * z * t14 * t56 + 0.18D2 * t4 * t94 + 0.18D2 * t34 * t42 * t37
     # * t52 * t57 * x3 - 0.18D2 * t64 * t5 * t58 + 0.9D1 * t34 * t3 * t
     #5 * t6 + 0.18D2 * t85 * t94 + 0.9D1 * t34 * t42 * t5 * t78
      gbgbH51J1 = -0.16D2 / 0.3D1 * wd * t116 / t32 / s

      end function
  
   
 

      doubleprecision function gbgbH51J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t26 = s - t2 * x1 * t5 * (t8 * t4 + x2 * t10 - t19) - t2 * t23 * x
     #3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t34 = t27 ** 2
      t39 = t1 ** 2
      t40 = t1 * t39
      t42 = t23 ** 2
      t45 = x3 ** 2
      t51 = t10 * x1
      t55 = t10 * t7 * t4 + x2 * x3 + t19
      t56 = t5 * t55
      t57 = t51 * t56
      t68 = x1 ** 2
      t70 = t4 ** 2
      t72 = t55 ** 2
      t74 = t10 * t68 / t70 * t72
      gbgbH51J2 = -0.16D2 / 0.3D1 * wd * (-0.9D1 * t29 * t1 * t23 * t10 
     #- 0.9D1 * t34 * t1 * t23 * t10 - 0.9D1 * t29 * t40 * t42 * t23 * t
     #10 * t45 + 0.27D2 * t34 * t39 * t23 * t57 - 0.27D2 * t29 * t40 * t
     #42 * t51 * t56 * x3 - 0.9D1 * t34 * t40 * t23 * t74 + 0.9D1 * t29 
     #* t39 * t23 * t57 - 0.9D1 * t29 * t40 * t23 * t74 + 0.18D2 * t29 *
     # t39 * t42 * t10 * x3) / t26 / s

      end function
  
   
 

      doubleprecision function gbgbH52J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = t1 * x1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t22 = t6 * t21
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t22 - t2 * t25
      t28 = s ** 2
      t29 = t28 * s
      t30 = t27 * t29
      t31 = t1 ** 2
      t32 = t30 * t31
      t33 = x1 ** 2
      t34 = t4 ** 2
      t35 = 0.1D1 / t34
      t36 = t33 * t35
      t37 = t21 ** 2
      t41 = t28 ** 2
      t43 = t41 * t31 * t33
      t46 = t14 * t4 + x2 * t7 - t20
      t47 = t46 ** 2
      t52 = t31 * t1
      t53 = t41 * t52
      t54 = t33 * x1
      t55 = t53 * t54
      t57 = 0.1D1 / t34 / t4
      t64 = z ** 2
      t68 = t29 * t1
      t69 = t68 * t24
      t70 = t7 * t27
      t74 = t30 * t1
      t89 = t29 * t31
      t90 = t24 ** 2
      t92 = t7 ** 2
      t97 = t30 * t52
      t98 = t54 * t57
      t103 = t98 * t47 * t46
      t109 = t90 * t92
      t113 = t29 * t52
      t119 = t89 * t25
      t121 = t5 * t21
      t122 = t27 * x1 * t121
      t125 = -0.3D1 * t32 * t36 * t37 + 0.3D1 * t43 * t35 * t47 * z + t5
     #5 * t57 * t46 * t37 + 0.4D1 * t41 * t1 * x1 * t5 * t46 * t64 - 0.6
     #D1 * t69 * t70 * z + 0.6D1 * t74 * t6 * t46 - 0.6D1 * t32 * t36 * 
     #t47 - t55 * t57 * t47 * t21 + 0.3D1 * t74 * t22 + 0.4D1 * t69 * t7
     #0 * t64 + 0.3D1 * t89 * t90 * t92 * t27 * z + t97 * t98 * t37 * t2
     #1 + t97 * t103 + t53 * t103 + 0.3D1 * t68 * t25 * t27 - 0.3D1 * t8
     #9 * t109 * t27 + t113 * t90 * t24 * t92 * t7 * t27 - 0.6D1 * t119 
     #* t122
      t131 = t35 * t37
      t135 = t31 * t33
      t139 = t35 * t46 * z * t21
      t160 = t30 * z
      t176 = t3 * t121
      t179 = t30 * t64
      t187 = 0.3D1 * t113 * t109 * t122 + 0.3D1 * t113 * t25 * t27 * t33
     # * t131 + 0.6D1 * t30 * t135 * t139 + 0.6D1 * t119 * t27 * z * t22
     # - t30 - 0.3D1 * t43 * t139 - 0.12D2 * t74 * t6 * t46 * z - 0.6D1 
     #* t32 * t36 * t46 * t21 + 0.3D1 * t32 * t36 * t47 * z + 0.3D1 * t1
     #60 * t135 * t131 + 0.3D1 * t97 * t98 * t46 * t37 + 0.4D1 * t74 * t
     #6 * t46 * t64 + 0.3D1 * t97 * t98 * t47 * t21 - 0.6D1 * t160 * t17
     #6 + 0.4D1 * t179 * t176 + 0.3D1 * t160 + 0.2D1 * t30 * t64 * z - 0
     #.4D1 * t179
      gbgbH52J1 = -0.16D2 / 0.3D1 * wd * (t125 + t187) / t27 / s

      end function
  
   
 

      doubleprecision function gbgbH52J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = 0.1D1 - x3
      t9 = t7 * t8
      t11 = s * t3
      t12 = x1 * t3
      t13 = z + t12
      t14 = 0.1D1 / t13
      t15 = x1 * t14
      t16 = 0.1D1 - x2
      t21 = cos(x4 * 0.3141592653589793D1)
      t22 = x3 * t16
      t26 = Sqrt(t22 * t13 * x2 * t8)
      t28 = 0.2D1 * t21 * t26
      t29 = t8 * t16 * t13 + x2 * x3 + t28
      t30 = t15 * t29
      t33 = s - t11 * t30 - t11 * t9
      t34 = x1 ** 2
      t36 = t13 ** 2
      t37 = 0.1D1 / t36
      t38 = t29 ** 2
      t39 = t38 * t37
      t43 = t33 * t2
      t44 = t4 * t34
      t48 = t22 * t13 + x2 * t8 - t28
      t51 = t37 * t48 * z * t29
      t54 = t1 ** 2
      t55 = t54 * t5
      t56 = t34 * x1
      t58 = 0.1D1 / t36 / t13
      t59 = t56 * t58
      t60 = t48 ** 2
      t62 = t59 * t60 * t48
      t64 = t2 * t3
      t68 = t2 * t4
      t69 = t68 * t9
      t74 = t43 * t4
      t75 = t34 * t37
      t80 = t54 * t4 * t34
      t84 = t55 * t56
      t92 = t43 * t3
      t104 = t7 ** 2
      t106 = t8 ** 2
      t110 = -0.3D1 * t6 * t9 * t33 * t34 * t39 - 0.2D1 * t43 * t44 * t5
     #1 + t43 - t55 * t62 - 0.2D1 * t64 * t9 * t33 - 0.2D1 * t69 * t33 *
     # z * t30 + 0.3D1 * t74 * t75 * t38 - t80 * t37 * t60 * z - t84 * t
     #58 * t48 * t38 + t64 * t7 * t8 * t33 * z - 0.6D1 * t92 * t15 * t48
     # + 0.6D1 * t74 * t75 * t60 + t84 * t58 * t60 * t29 - 0.3D1 * t92 *
     # t30 - t68 * t104 * t106 * t33 * z
      t111 = t43 * t5
      t116 = t43 * z
      t118 = t14 * t29
      t119 = t33 * x1 * t118
      t122 = t104 * t106
      t159 = -t111 * t59 * t38 * t29 - t111 * t62 - t116 + 0.5D1 * t69 *
     # t119 - 0.3D1 * t6 * t122 * t119 + t80 * t51 + 0.4D1 * t92 * t15 *
     # t48 * z + 0.6D1 * t74 * t75 * t48 * t29 - 0.3D1 * t111 * t59 * t6
     #0 * t29 - 0.3D1 * t111 * t59 * t48 * t38 - t116 * t44 * t39 - t74 
     #* t75 * t60 * z + 0.2D1 * t116 * t12 * t118 + 0.2D1 * t68 * t122 *
     # t33 - t6 * t104 * t7 * t106 * t8 * t33
      gbgbH52J2 = -0.16D2 / 0.3D1 * wd * (t110 + t159) / t33 / s

      end function
  
   
 

      doubleprecision function gbgbH52J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - x1
      t7 = 0.1D1 - x3
      t8 = t6 * t7
      t9 = t5 * t8
      t10 = s * t3
      t11 = x1 * t3
      t12 = z + t11
      t13 = 0.1D1 / t12
      t15 = 0.1D1 - x2
      t20 = cos(x4 * 0.3141592653589793D1)
      t21 = x3 * t15
      t25 = Sqrt(t21 * t12 * x2 * t7)
      t27 = 0.2D1 * t20 * t25
      t28 = t7 * t15 * t12 + x2 * x3 + t27
      t29 = x1 * t13 * t28
      t32 = s - t10 * t29 - t10 * t8
      t36 = t32 * t2
      t38 = x1 ** 2
      t39 = t12 ** 2
      t40 = 0.1D1 / t39
      t42 = t28 ** 2
      t46 = t4 * t38
      t50 = t21 * t12 + x2 * t7 - t27
      t53 = t40 * t50 * z * t28
      t55 = t1 ** 2
      t56 = t4 * t3
      t58 = t38 * x1
      t59 = t55 * t56 * t58
      t61 = 0.1D1 / t39 / t12
      t65 = t36 * t56
      t66 = t58 * t61
      t67 = t50 ** 2
      t72 = t13 * t28
      t73 = t32 * x1 * t72
      t76 = t2 * t3
      t84 = t36 * z
      t87 = t6 ** 2
      t88 = t7 ** 2
      t89 = t87 * t88
      t92 = t2 * t56
      t95 = t40 * t42
      t117 = -t9 * t32 * z * t29 + 0.2D1 * t36 * t4 * t38 * t40 * t42 - 
     #t36 * t46 * t53 - t59 * t61 * t50 * t42 - t65 * t66 * t67 * t28 + 
     #0.3D1 * t9 * t73 + t76 * t6 * t7 * t32 * z + t59 * t61 * t67 * t28
     # + t84 * t11 * t72 + t5 * t89 * t32 - 0.2D1 * t92 * t8 * t32 * t38
     # * t95 - t36 * t3 * t29 + t55 * t4 * t38 * t53 - t84 * t46 * t95 -
     # t65 * t66 * t42 * t28 - t92 * t89 * t73 - 0.2D1 * t65 * t66 * t50
     # * t42 - t76 * t8 * t32
      gbgbH52J3 = -0.16D2 / 0.3D1 * wd * t117 / t32 / s

      end function
  
   
 

      doubleprecision function gbgbH53J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * t6 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t33 = t30 * t31 * x1
      t36 = t14 * t4 + x2 * t7 - t20
      t37 = t5 * t36
      t43 = t30 * t1
      t44 = t36 * z
      t48 = t31 * t1
      t51 = t24 ** 2
      t52 = x3 ** 2
      t54 = t37 * t51 * t52
      t57 = t28 ** 2
      t63 = x1 ** 2
      t64 = t4 ** 2
      t65 = 0.1D1 / t64
      t73 = t65 * t36
      t74 = t24 * x3
      t79 = t57 * t31
      t81 = t37 * t74
      t94 = t57 * t1
      t95 = t94 * x1
      t96 = z ** 2
      t100 = t6 * t36
      t119 = t21 ** 2
      t124 = 0.18D2 * t33 * t37 * z * t24 * x3 - 0.18D2 * t43 * t6 * t44
     # + 0.9D1 * t30 * t48 * x1 * t54 + 0.9D1 * t57 * t48 * x1 * t54 - 0
     #.18D2 * t30 * t31 * t63 * t65 * t36 * t21 + 0.18D2 * t30 * t48 * t
     #63 * t73 * t74 * t21 - 0.18D2 * t79 * x1 * t81 + 0.18D2 * t79 * t6
     # * t44 * t74 + 0.18D2 * t30 * t31 * t63 * t73 * z * t21 + 0.18D2 *
     # t95 * t37 * t96 + 0.9D1 * t94 * t100 + 0.9D1 * t43 * t100 - 0.18D
     #2 * t33 * t81 + 0.18D2 * t43 * t6 * t36 * t96 - 0.18D2 * t95 * t37
     # * z + 0.9D1 * t30 * t48 * t63 * x1 / t64 / t4 * t36 * t119
      gbgbH53J1 = -0.16D2 / 0.3D1 * wd * t124 / t27 / s

      end function
  
   
 

      doubleprecision function gbgbH53J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * t6 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = x1 ** 2
      t36 = t4 ** 2
      t37 = 0.1D1 / t36
      t40 = t14 * t4 + x2 * t7 - t20
      t42 = t24 * x3
      t52 = t21 ** 2
      t59 = t5 * t40
      t60 = t59 * t42
      t63 = t28 ** 2
      t69 = t6 * t40
      t80 = t24 ** 2
      t81 = x3 ** 2
      t83 = t59 * t80 * t81
      gbgbH53J2 = -0.16D2 / 0.3D1 * wd * (-0.27D2 * t30 * t32 * t33 * t3
     #7 * t40 * t42 * t21 - 0.9D1 * t30 * t32 * t33 * x1 / t36 / t4 * t4
     #0 * t52 + 0.9D1 * t30 * t31 * x1 * t60 + 0.27D2 * t63 * t31 * x1 *
     # t60 - 0.9D1 * t63 * t1 * t69 + 0.18D2 * t30 * t31 * t33 * t37 * t
     #40 * t21 - 0.9D1 * t30 * t32 * x1 * t83 - 0.9D1 * t30 * t1 * t69 -
     # 0.9D1 * t63 * t32 * x1 * t83) / t27 / s

      end function
  
   
 

      doubleprecision function gbgbH54J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t20 = t8 * t4 + x2 * t10 - 0.2D1 * t13 * t17
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t21 - t2 * t24
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t29 * t1
      t31 = t23 * t10
      t35 = z ** 2
      t36 = t31 * t35
      t39 = t27 ** 2
      t40 = t1 ** 2
      t41 = t39 * t40
      t42 = t23 ** 2
      t48 = t29 * t35
      t49 = t1 * t23
      t50 = t49 * x3
      t53 = t29 * t40
      t54 = x1 ** 2
      t55 = t4 ** 2
      t56 = 0.1D1 / t55
      t57 = t54 * t56
      t58 = t20 ** 2
      t64 = t29 * z
      t67 = t40 * t1
      t68 = t29 * t67
      t69 = t42 * t23
      t70 = t10 ** 2
      t72 = t69 * t70 * x3
      t75 = t28 * t67
      t84 = t40 * t42
      t85 = x3 ** 2
      t86 = t84 * t85
      t90 = t42 * t70 * z
      t93 = t42 * t10
      t98 = t69 * t10 * t85
      t102 = t20 * t26
      t111 = t28 * t40
      t112 = t111 * t6
      t126 = -0.12D2 * t30 * t31 * z + 0.4D1 * t30 * t36 - 0.3D1 * t41 *
     # t42 * t10 * z * x3 + 0.4D1 * t48 * t50 - 0.3D1 * t53 * t57 * t58 
     #+ 0.3D1 * t30 * t21 - 0.6D1 * t64 * t50 + 0.3D1 * t68 * t72 + t75 
     #* t54 * x1 / t55 / t4 * t58 * t20 * t26 + 0.3D1 * t64 * t86 + 0.3D
     #1 * t53 * t90 - 0.6D1 * t53 * t93 * x3 + 0.3D1 * t68 * t98 + 0.3D1
     # * t75 * t6 * t102 * t42 * t85 + 0.6D1 * t53 * t93 * z * x3 + 0.6D
     #1 * t112 * t102 * z * t23 * x3 + 0.3D1 * t75 * t57 * t58 * t26 * t
     #24 - 0.6D1 * t112 * t102 * t24
      t127 = t5 * t20
      t145 = t67 * t69
      t146 = t70 * t10
      t165 = t39 * t67
      t176 = -0.6D1 * t64 * t3 * t127 + 0.3D1 * t111 * t54 * t56 * t58 *
     # t26 * z + 0.4D1 * t28 * t1 * x1 * t127 * t26 * t35 - 0.4D1 * t48 
     #+ 0.3D1 * t64 - t29 + t29 * t145 * t146 - 0.3D1 * t29 * t86 + 0.3D
     #1 * t29 * t50 - 0.6D1 * t29 * t84 * t70 + 0.4D1 * t39 * t1 * t36 +
     # 0.6D1 * t29 * t49 * t10 + t29 * t145 * t85 * x3 - t165 * t72 + 0.
     #3D1 * t41 * t90 + t165 * t98 + 0.2D1 * t28 * t35 * z * t26 + t165 
     #* t69 * t146
      gbgbH54J1 = -0.16D2 / 0.3D1 * wd * (t126 + t176) / t26 / s

      end function
  
   
 

      doubleprecision function gbgbH54J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = 0.1D1 - x3
      t12 = x3 ** 2
      t13 = t9 * t10 * t12
      t15 = s * t3
      t16 = x1 * t3
      t17 = z + t16
      t18 = 0.1D1 / t17
      t19 = x1 * t18
      t21 = x3 * (0.1D1 - x2)
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t21 * t17 * x2 * t10)
      t32 = t21 * t17 + x2 * t10 - 0.2D1 * t25 * t29
      t33 = t19 * t32
      t35 = t7 * x3
      t37 = s - t15 * t33 - t15 * t35
      t38 = s * t1
      t39 = t38 * t37
      t40 = t39 * t4
      t41 = x1 ** 2
      t42 = t17 ** 2
      t43 = 0.1D1 / t42
      t44 = t41 * t43
      t45 = t32 ** 2
      t49 = t39 * t3
      t52 = t39 * z
      t53 = t3 * t7
      t54 = t53 * x3
      t57 = t39 * t5
      t58 = t10 ** 2
      t60 = t9 * t58 * x3
      t63 = t38 * t5
      t72 = t8 * t4
      t73 = t72 * t12
      t76 = t8 * t58 * z
      t78 = t10 * t8
      t88 = t2 * t4
      t93 = t58 * t10
      t96 = -t6 * t13 + t39 + 0.2D1 * t40 * t44 * t45 - 0.2D1 * t49 * t3
     #3 + 0.2D1 * t52 * t54 - 0.3D1 * t57 * t60 - t63 * t41 * x1 / t42 /
     # t17 * t45 * t32 * t37 - t52 * t73 - t40 * t76 + 0.6D1 * t40 * t78
     # * x3 - 0.3D1 * t57 * t13 + 0.4D1 * t49 * t7 * t10 * z + t88 * t8 
     #* t10 * z * x3 - t52 - t6 * t9 * t93
      t102 = t4 * t38
      t118 = t5 * t9
      t130 = t102 * t19
      t131 = t32 * t37
      t148 = -0.3D1 * t63 * t44 * t45 * t37 * t35 - t102 * t41 * t43 * t
     #45 * t37 * z + t52 * t16 * t18 * t32 - 0.2D1 * t40 * t78 * z * x3 
     #- 0.6D1 * t39 * t53 * t10 - t39 * t118 * t12 * x3 + t6 * t60 - t88
     # * t76 - t39 * t118 * t93 + 0.3D1 * t39 * t73 - 0.3D1 * t39 * t54 
     #+ 0.5D1 * t130 * t131 * t35 - 0.3D1 * t63 * t19 * t131 * t8 * t12 
     #- 0.2D1 * t130 * t131 * z * t7 * x3 + 0.6D1 * t39 * t72 * t58
      gbgbH54J2 = -0.16D2 / 0.3D1 * wd * (t96 + t148) / t37 / s

      end function
  
   
 

      doubleprecision function gbgbH54J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t20 = t8 * t4 + x2 * t10 - 0.2D1 * t13 * t17
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t21 - t2 * t24
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t29 * z
      t31 = t1 ** 2
      t32 = t23 ** 2
      t34 = x3 ** 2
      t35 = t31 * t32 * t34
      t37 = t29 * t31
      t38 = x1 ** 2
      t39 = t4 ** 2
      t41 = t38 / t39
      t42 = t20 ** 2
      t48 = t31 * t1
      t49 = t28 * t48
      t51 = t20 * t26
      t61 = t1 * t23 * x3
      t64 = t28 * t31 * t6
      t69 = t27 ** 2
      t70 = t69 * t48
      t71 = t32 * t23
      t72 = t10 ** 2
      t74 = t71 * t72 * x3
      t80 = t71 * t10 * t34
      t83 = t29 * t48
      t104 = -t30 * t35 + t37 * t41 * t42 + t30 * t3 * t5 * t20 - 0.2D1 
     #* t49 * t6 * t51 * t32 * t34 - t37 * t32 * t10 * z * x3 - t29 * t6
     #1 - t64 * t51 * z * t23 * x3 + t70 * t74 + 0.3D1 * t64 * t51 * t24
     # - t70 * t80 + t30 * t61 - 0.2D1 * t83 * t80 + t69 * t31 * t32 * t
     #10 * z * x3 - t83 * t74 + 0.2D1 * t29 * t35 - t29 * t1 * t21 - t49
     # * t41 * t42 * t26 * t24 - t29 * t48 * t71 * t34 * x3
      gbgbH54J3 = -0.16D2 / 0.3D1 * wd * t104 / t26 / s

      end function
  
 