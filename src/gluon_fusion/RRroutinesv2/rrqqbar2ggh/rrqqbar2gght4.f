  
      subroutine rrqqbar2gght4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh41J1  
      doubleprecision rrqqbar2ggh41J2  
      doubleprecision rrqqbar2ggh41J3  
      doubleprecision rrqqbar2gght4s1e1  
      doubleprecision rrqqbar2gght4s1e0  
      doubleprecision rrqqbar2gght4s1em1  
      doubleprecision rrqqbar2gght4s1em2  
      doubleprecision rrqqbar2gght4s1em3  
      doubleprecision rrqqbar2gght4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght4s1e1
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
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3

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
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = x1 ** 2
      t19 = t4 ** 2
      t20 = t18 * t19
      t24 = log(0.4D1 * t17 * t20 * x4)
      t25 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t27 = t24 ** 2
      t28 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t35 = pi * lh
      t36 = t1 * t7
      t42 = lh ** 2
      t44 = pi ** 2
      t46 = 0.180D3 * t42 - 0.30D2 * t44
      t47 = pi * t46
      t48 = t36 * t28
      t49 = t47 * t48
      t51 = 0.1D1 / x4
      t54 = t16 * t18
      t55 = t54 * t19
      t58 = log(0.4D1 * t14 * t55)
      t59 = t58 * pi
      t67 = t58 ** 2
      t68 = t67 * pi
      t91 = x3 * t11
      t92 = t91 * t13
      t97 = log(0.4D1 * t92 * t54 * t19 * x4)
      t106 = 0.1D1 / x3
      t111 = log(0.4D1 * t92 * t55)
      t113 = t111 ** 2
      t128 = -(0.90D2 * t6 * t7 * (t8 - t24 * t25 + t27 * t28 / 0.2D1) -
     # 0.180D3 * t35 * t36 * (t25 - t24 * t28) + t49) * t51 / 0.720D3 - 
     #(-0.180D3 * t35 - 0.90D2 * t59) * t1 * t7 * t8 / 0.720D3 - (t47 + 
     #0.180D3 * t59 * lh + 0.45D2 * t68) * t1 * t7 * t25 / 0.720D3 - (pi
     # * (0.60D2 * lh * t44 - 0.240D3 * zeta3 - 0.120D3 * t42 * lh) - t5
     #9 * t46 - 0.90D2 * t68 * lh - 0.15D2 * t67 * t58 * pi) * t1 * t7 *
     # t28 / 0.720D3 + (0.90D2 * t6 * t7 * (-t25 + t97 * t28) + 0.180D3 
     #* t35 * t48) * t106 * t51 / 0.720D3 - (0.90D2 * t6 * t7 * (t8 - t1
     #11 * t25 + t113 * t28 / 0.2D1) - 0.180D3 * t35 * t36 * (t25 - t111
     # * t28) + t49) * t106 / 0.720D3
      t129 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t128)
      t131 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t132 = s * t131
      t133 = t1 * x1
      t134 = t132 * t133
      t135 = t1 * t4
      t136 = t135 * x4
      t137 = t132 * t136
      t138 = -0.1D1 + x4
      t139 = t135 * t138
      t140 = t132 * t139
      t141 = t131 ** 2
      t144 = x1 * t4
      t146 = s * t141 * t15 * t144 * x4
      t148 = 0.1D1 / (-0.2D1 + t131)
      t149 = t141 * t148
      t150 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t137
     #, t134, t140, -t146)
      t153 = t13 * t16
      t155 = x4 * t138
      t156 = t141 ** 2
      t161 = log(-0.4D1 * t18 * t11 * t153 * t155 * t156 * t19)
      t162 = t161 * t141
      t163 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t137
     #, t134, t140, -t146)
      t166 = t161 ** 2
      t168 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t137
     #, t134, t140, -t146)
      t169 = t148 * t168
      t176 = t149 * t163
      t182 = t47 * t1
      t184 = t7 * t141 * t169
      t188 = x3 * t18
      t196 = log(-0.4D1 * t188 * t14 * t16 * x4 * t138 * t19 * t156)
      t203 = t35 * t1
      t210 = -(0.90D2 * t6 * t7 * (t149 * t150 - t162 * t148 * t163 + t1
     #66 * t141 * t169 / 0.2D1) - 0.180D3 * t35 * t36 * (t176 - t162 * t
     #169) + t182 * t184) * t51 / 0.720D3 + (0.90D2 * t6 * t7 * (-t176 +
     # t196 * t141 * t169) + 0.180D3 * t203 * t184) * t106 * t51 / 0.720
     #D3
      t211 = FJET(XB1, XB2, s, 0.0D0, t134, -t137, t140, -t146, t210)
      t213 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t128)
      t215 = FJET(XB1, XB2, s, t134, 0.0D0, t140, -t137, -t146, t210)
      t217 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t218 = s * t217
      t219 = t133 * x3
      t220 = t218 * t219
      t221 = -0.1D1 + x3
      t222 = t133 * t221
      t223 = t218 * t222
      t224 = t218 * t135
      t225 = t217 ** 2
      t229 = s * t225 * t15 * t144 * x3
      t231 = 0.1D1 / (-0.2D1 + t217)
      t232 = t225 * t231
      t233 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, t220, 0.0D0,
     # -t223, -t224, -t229)
      t234 = t232 * t233
      t235 = t91 * t153
      t237 = t225 ** 2
      t242 = log(-0.4D1 * t235 * t20 * t221 * x4 * t237)
      t244 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, t220, 0.0D0,
     # -t223, -t224, -t229)
      t245 = t231 * t244
      t252 = t7 * t225 * t245
      t258 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, s, t220, 0.0D0,
     # -t223, -t224, -t229)
      t264 = log(-0.4D1 * t235 * t20 * t221 * t237)
      t265 = t264 * t225
      t268 = t264 ** 2
      t285 = (0.90D2 * t6 * t7 * (-t234 + t242 * t225 * t245) + 0.180D3 
     #* t203 * t252) * t106 * t51 / 0.720D3 - (0.90D2 * t6 * t7 * (t232 
     #* t258 - t265 * t231 * t233 + t268 * t225 * t245 / 0.2D1) - 0.180D
     #3 * t35 * t36 * (t234 - t265 * t245) + t182 * t252) * t106 / 0.720
     #D3
      t286 = FJET(XB1, XB2, s, t220, -t223, 0.0D0, -t224, -t229, t285)
      t288 = KAPPA2(x1, x2, x3, x4, z)
      t289 = s * t288
      t290 = t289 * t219
      t291 = t289 * t222
      t292 = t289 * t136
      t293 = t289 * t139
      t294 = t288 ** 2
      t299 = cos(t9)
      t302 = Sqrt(x3 * t221 * t155)
      t307 = s * t294 * t15 * t144 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t299 * t302)
      t309 = 0.1D1 / (-0.2D1 + t288)
      t311 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, t290, -t292,
     # -t291, t293, t307)
      t315 = t294 ** 2
      t320 = log(0.4D1 * t188 * t17 * t155 * t19 * t221 * t315)
      t322 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, t290, -t292,
     # -t291, t293, t307)
      t323 = t309 * t322
      t333 = 0.90D2 * t6 * t7 * (t294 * t309 * t311 - t320 * t294 * t323
     #) - 0.180D3 * t203 * t7 * t294 * t323
      t336 = t333 * t106 * t51 / 0.720D3
      t337 = FJET(XB1, XB2, s, t290, -t291, -t292, t293, t307, t336)
      t339 = t106 * t51
      t342 = FJET(XB1, XB2, s, -t223, t220, -t224, 0.0D0, -t229, t285)
      t344 = FJET(XB1, XB2, s, -t291, t290, t293, -t292, t307, t336)
      rrqqbar2gght4s1e1 = t129 * t128 + t211 * t210 + t213 * t128 + t215
     # * t210 + t286 * t285 + t337 * t333 * t339 / 0.720D3 + t342 * t285
     # + t344 * t333 * t339 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght4s1e0
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
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3

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
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x1 ** 2
      t19 = t4 ** 2
      t20 = t18 * t19
      t24 = log(0.4D1 * t14 * t16 * t20 * x4)
      t25 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t31 = pi * lh
      t32 = t1 * t7
      t35 = 0.180D3 * t31 * t32 * t25
      t37 = 0.1D1 / x4
      t41 = 0.1D1 / x3
      t46 = x3 * t11
      t49 = t16 * t18 * t19
      t52 = log(0.4D1 * t46 * t13 * t49)
      t61 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t68 = log(0.4D1 * t14 * t49)
      t69 = t68 * pi
      t76 = lh ** 2
      t78 = pi ** 2
      t84 = t68 ** 2
      t92 = -(0.90D2 * t6 * t7 * (t8 - t24 * t25) - t35) * t37 / 0.720D3
     # - t6 * t7 * t25 * t41 * t37 / 0.8D1 - (0.90D2 * t6 * t7 * (t8 - t
     #52 * t25) - t35) * t41 / 0.720D3 - t6 * t7 * t61 / 0.8D1 - (-0.180
     #D3 * t31 - 0.90D2 * t69) * t1 * t7 * t8 / 0.720D3 - (pi * (0.180D3
     # * t76 - 0.30D2 * t78) + 0.180D3 * t69 * lh + 0.45D2 * t84 * pi) *
     # t1 * t7 * t25 / 0.720D3
      t93 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t92)
      t95 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t96 = s * t95
      t97 = t1 * x1
      t98 = t96 * t97
      t99 = t1 * t4
      t100 = t99 * x4
      t101 = t96 * t100
      t102 = -0.1D1 + x4
      t103 = t99 * t102
      t104 = t96 * t103
      t105 = t95 ** 2
      t108 = x1 * t4
      t110 = s * t105 * t15 * t108 * x4
      t112 = 0.1D1 / (-0.2D1 + t95)
      t114 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t101
     #, t98, t104, -t110)
      t117 = t13 * t16
      t119 = x4 * t102
      t120 = t105 ** 2
      t125 = log(-0.4D1 * t18 * t11 * t117 * t119 * t120 * t19)
      t127 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t101
     #, t98, t104, -t110)
      t128 = t112 * t127
      t134 = t31 * t1
      t135 = t7 * t105
      t143 = t41 * t37
      t147 = -(0.90D2 * t6 * t7 * (t105 * t112 * t114 - t125 * t105 * t1
     #28) - 0.180D3 * t134 * t135 * t128) * t37 / 0.720D3 - t6 * t135 * 
     #t128 * t143 / 0.8D1
      t148 = FJET(XB1, XB2, s, 0.0D0, t98, -t101, t104, -t110, t147)
      t150 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t92)
      t152 = FJET(XB1, XB2, s, t98, 0.0D0, t104, -t101, -t110, t147)
      t154 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t155 = s * t154
      t156 = t97 * x3
      t157 = t155 * t156
      t158 = -0.1D1 + x3
      t159 = t97 * t158
      t160 = t155 * t159
      t161 = t155 * t99
      t162 = t154 ** 2
      t166 = s * t162 * t15 * t108 * x3
      t167 = t7 * t162
      t170 = 0.1D1 / (-0.2D1 + t154)
      t171 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, t157, 0.0D0,
     # -t160, -t161, -t166)
      t172 = t170 * t171
      t177 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, t157, 0.0D0,
     # -t160, -t161, -t166)
      t180 = t162 ** 2
      t185 = log(-0.4D1 * t46 * t117 * t20 * t158 * t180)
      t198 = -t6 * t167 * t172 * t143 / 0.8D1 - (0.90D2 * t6 * t7 * (t16
     #2 * t170 * t177 - t185 * t162 * t172) - 0.180D3 * t134 * t167 * t1
     #72) * t41 / 0.720D3
      t199 = FJET(XB1, XB2, s, t157, -t160, 0.0D0, -t161, -t166, t198)
      t201 = KAPPA2(x1, x2, x3, x4, z)
      t202 = s * t201
      t203 = t202 * t156
      t204 = t202 * t159
      t205 = t202 * t100
      t206 = t202 * t103
      t207 = t201 ** 2
      t212 = cos(t9)
      t215 = Sqrt(x3 * t158 * t119)
      t220 = s * t207 * t15 * t108 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t212 * t215)
      t224 = 0.1D1 / (-0.2D1 + t201)
      t225 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, t203, -t205,
     # -t204, t206, t220)
      t229 = t6 * t7 * t207 * t224 * t225 * t143 / 0.8D1
      t230 = FJET(XB1, XB2, s, t203, -t204, -t205, t206, t220, t229)
      t236 = t207 * t224 * t225 * t41 * t37
      t239 = FJET(XB1, XB2, s, -t160, t157, -t161, 0.0D0, -t166, t198)
      t241 = FJET(XB1, XB2, s, -t204, t203, t206, -t205, t220, t229)
      rrqqbar2gght4s1e0 = t93 * t92 + t148 * t147 + t150 * t92 + t152 * 
     #t147 + t199 * t198 + t230 * pi * t32 * t236 / 0.8D1 + t239 * t198 
     #+ t241 * pi * t32 * t236 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght4s1em1
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
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3

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
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x3
      t14 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t28 = x1 ** 2
      t30 = t4 ** 2
      t34 = log(0.4D1 * t22 / t23 * t27 * t28 * t30)
      t41 = 0.1D1 / x4
      t45 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.8D1 - (-0.180D3 *
     # pi * lh - 0.90D2 * t34 * pi) * t1 * t9 / 0.720D3 - t6 * t9 * t41 
     #/ 0.8D1
      t46 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t45)
      t48 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t49 = s * t48
      t50 = t1 * x1
      t51 = t49 * t50
      t52 = t1 * t4
      t54 = t49 * t52 * x4
      t57 = t49 * t52 * (-0.1D1 + x4)
      t58 = t48 ** 2
      t61 = x1 * t4
      t63 = s * t58 * t26 * t61 * x4
      t64 = t6 * t7
      t68 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t54, 
     #t51, t57, -t63)
      t70 = t58 / (-0.2D1 + t48) * t68 * t41
      t72 = t64 * t70 / 0.8D1
      t73 = FJET(XB1, XB2, s, 0.0D0, t51, -t54, t57, -t63, -t72)
      t75 = t1 * t7
      t79 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t45)
      t81 = FJET(XB1, XB2, s, t51, 0.0D0, t57, -t54, -t63, -t72)
      t86 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t87 = s * t86
      t89 = t87 * t50 * x3
      t92 = t87 * t50 * (-0.1D1 + x3)
      t93 = t87 * t52
      t94 = t86 ** 2
      t98 = s * t94 * t26 * t61 * x3
      t102 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, t89, 0.0D0, 
     #-t92, -t93, -t98)
      t104 = t94 / (-0.2D1 + t86) * t102 * t10
      t106 = t64 * t104 / 0.8D1
      t107 = FJET(XB1, XB2, s, t89, -t92, 0.0D0, -t93, -t98, -t106)
      t112 = FJET(XB1, XB2, s, -t92, t89, -t93, 0.0D0, -t98, -t106)
      rrqqbar2gght4s1em1 = t46 * t45 - t73 * pi * t75 * t70 / 0.8D1 + t7
     #9 * t45 - t81 * pi * t75 * t70 / 0.8D1 - t107 * pi * t75 * t104 / 
     #0.8D1 - t112 * pi * t75 * t104 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght4s1em2
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
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3

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
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t11 = pi * t1 * t7 * t8 / 0.8D1
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t11)
      t15 = t1 * t7 * t8
      t17 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t11)
      rrqqbar2gght4s1em2 = -t12 * pi * t15 / 0.8D1 - t17 * pi * t15 / 0.
     #8D1

      end function



      doubleprecision function rrqqbar2gght4s1em3
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
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght4s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght4s1em4
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
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh41J1
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t5 = 0.1D1 / (S12 + S14 + S24)
      t6 = S12 ** 2
      t9 = S34 ** 2
      rrqqbar2ggh41J1 = (0.16D2 / 0.27D2 * S34 * t2 * t5 * t6 + 0.16D2 /
     # 0.27D2 * t9 * S34 * t2 * t5) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh41J2
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t5 = 0.1D1 / (S12 + S14 + S24)
      t6 = S12 ** 2
      t10 = S34 ** 2
      rrqqbar2ggh41J2 = (-0.16D2 / 0.27D2 * S34 * t2 * t5 * t6 + 0.32D2 
     #/ 0.27D2 * t10 * t2 * t5 * S12 - 0.16D2 / 0.27D2 * t10 * S34 * t2 
     #* t5 + 0.16D2 / 0.27D2 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh41J3
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t5 = 0.1D1 / (S12 + S14 + S24)
      t6 = S12 ** 2
      t10 = S34 ** 2
      t26 = S24 ** 2
      t28 = S23 ** 2
      rrqqbar2ggh41J3 = (-0.16D2 / 0.27D2 * S34 * t2 * t5 * t6 + 0.32D2 
     #/ 0.27D2 * t10 * t2 * t5 * S12 - 0.16D2 / 0.27D2 * t10 * S34 * t2 
     #* t5 + 0.16D2 / 0.27D2 * S34 + ((-0.32D2 / 0.27D2 + 0.16D2 / 0.27D
     #2 * S24 * t5 + 0.16D2 / 0.27D2 * S23 * t2) * t10 + (-0.16D2 / 0.27
     #D2 * S23 - 0.16D2 / 0.27D2 * S24 + 0.16D2 / 0.27D2 * t26 * t5 + 0.
     #16D2 / 0.27D2 * t28 * t2) * S34) / S12) / pi * wd / z

      end function
  
 