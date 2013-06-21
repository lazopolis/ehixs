  
      subroutine bbarbbarH7n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH7n1e1  
      doubleprecision bbarbbarH7n1e0  
      doubleprecision bbarbbarH7n1em1  
      doubleprecision bbarbbarH7n1em2  
      doubleprecision bbarbbarH7n1em3  
      doubleprecision bbarbbarH7n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH7n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH7n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH7n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH7n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH7n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH7n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH7n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t11 * t4
      t15 = log(-0.4D1 * x2 * t8 * t12)
      t16 = t15 * z
      t19 = lh ** 2
      t21 = 0.3141592653589793D1 ** 2
      t23 = 0.180D3 * t19 - 0.30D2 * t21
      t24 = z * t23
      t25 = t15 ** 2
      t26 = t25 * z
      t29 = t1 ** 2
      t31 = x2 * z
      t33 = (0.1D1 - x2 + t31) ** 2
      t34 = 0.1D1 / t33
      t35 = wd * t34
      t37 = (0.180D3 * t16 * lh + t24 + 0.45D2 * t26) * t29 * t35 / 0.54
     #0D3
      t51 = (-t16 * t23 - 0.90D2 * t26 * lh + z * (-0.2884936567583026D3
     # - 0.120D3 * t19 * lh + 0.60D2 * lh * t21) - 0.15D2 * t25 * t15 * 
     #z) * t29
      t54 = z * lh
      t55 = t54 * t29
      t56 = x2 * x3
      t57 = t8 * t11
      t58 = t57 * t4
      t61 = log(-0.4D1 * t56 * t58)
      t67 = z * t29
      t68 = t61 ** 2
      t74 = t29 * wd
      t75 = t74 * t34
      t77 = 0.4D1 * t24 * t75
      t79 = 0.1D1 / x3
      t82 = z * t34
      t83 = x1 ** 2
      t84 = t83 * x3
      t85 = t84 * x2
      t88 = log(-0.4D1 * t85 * t58)
      t89 = t88 * z
      t98 = 0.1D1 / x1
      t102 = lh * t29
      t103 = x2 * t83
      t106 = log(-0.4D1 * t103 * t58)
      t107 = t106 * z
      t108 = t107 * t34
      t114 = t106 ** 2
      t115 = t114 * z
      t124 = t37 - t51 * t35 / 0.135D3 - (-0.180D3 * t55 * t35 * (-0.1D1
     # - 0.4D1 * t61) + 0.90D2 * t67 * t35 * (0.2D1 * t68 + t61) + t77) 
     #* t79 / 0.540D3 - (0.90D2 * t74 * (-t82 - 0.4D1 * t89 * t34) - 0.7
     #20D3 * t54 * t75) * t98 * t79 / 0.270D3 - (-0.180D3 * t102 * wd * 
     #(-0.4D1 * t108 - t82) + 0.90D2 * t74 * (t108 + 0.2D1 * t115 * t34)
     # + t77) * t98 / 0.270D3
      t125 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t124)
      t127 = 0.2D1 * t56
      t128 = cos(t6)
      t129 = x3 * t4
      t130 = -0.1D1 + x3
      t133 = Sqrt(t129 * x2 * t130)
      t135 = 0.2D1 * t128 * t133
      t137 = t2 * (-x3 + t127 - x2 + t135)
      t139 = t2 * (0.1D1 - x2 - x3 + t127 + t135)
      t144 = log(0.4D1 * t56 * t8 * t12 * t130)
      t145 = 0.4D1 * t130
      t151 = t144 ** 2
      t154 = -t130
      t155 = t144 * t154
      t160 = t24 * t29
      t170 = log(0.4D1 * t85 * t57 * t4 * t130)
      t171 = t170 * z
      t174 = t82 * t154
      t178 = t102 * wd
      t186 = -(-0.180D3 * t55 * t35 * (-t144 * t145 + 0.1D1 - x3) + 0.90
     #D2 * t67 * t35 * (t151 * t145 / 0.2D1 - t155) + t160 * t35 * t145)
     # * t79 / 0.540D3 - (0.90D2 * t74 * (-t171 * t34 * t145 + t174) - 0
     #.180D3 * t178 * t82 * t145) * t98 * t79 / 0.270D3
      t187 = FJET(XB1, XB2, s, 0.0D0, -t137, 0.0D0, t139, 0.0D0, t186)
      t189 = -0.1D1 + x1
      t191 = x1 * z
      t192 = 0.1D1 - x1 + t191
      t193 = 0.1D1 / t192
      t195 = t2 * t189 * x2 * t193
      t196 = t2 * x1
      t199 = t4 * s * t1 * t189
      t204 = s * t29 * x2 * t189 * x1 * t193
      t206 = t56 * t83 * t8
      t207 = t11 * t193
      t208 = t189 ** 2
      t209 = t208 * t4
      t210 = t207 * t209
      t213 = log(-0.4D1 * t206 * t210)
      t214 = t213 * t189
      t215 = x2 * x1
      t216 = t215 * z
      t218 = (-0.1D1 + x1 - t191 + x2 - t215 - t31 + t216) ** 2
      t219 = 0.1D1 / t218
      t220 = 0.4D1 * z
      t221 = t83 * t10
      t222 = 0.5D1 * t191
      t223 = 0.2D1 * t216
      t224 = t103 * t10
      t225 = x1 * t10
      t226 = 0.4D1 * t225
      t227 = t215 * t10
      t229 = 0.2D1 * t103 * z
      t231 = 0.2D1 * t83 * z
      t232 = -t215 + t220 - t221 + t103 - t222 + t223 - t83 + x1 + t224 
     #+ t226 - t227 - t229 + t231
      t233 = t219 * t232
      t235 = t189 * t219
      t236 = 0.2D1 * t191
      t237 = t221 - t223 - t231 - t225 - t103 + t215 + t236 + t83 - z - 
     #x1 + t227 + t229 - t224
      t238 = t235 * t237
      t242 = t235 * t232
      t251 = log(-0.4D1 * t103 * t8 * t210)
      t252 = t251 * t189
      t258 = t251 ** 2
      t259 = t258 * t189
      t263 = t252 * t219 * t237
      t268 = t23 * t29 * wd
      t273 = -(0.90D2 * t74 * (-t214 * t233 + t238) - 0.180D3 * t178 * t
     #242) * t98 * t79 / 0.270D3 - (-0.180D3 * t102 * wd * (-t252 * t233
     # + t238) + 0.90D2 * t74 * (t259 * t233 / 0.2D1 - t263) + t268 * t2
     #42) * t98 / 0.270D3
      t274 = FJET(XB1, XB2, s, 0.0D0, -t195, t196, t199, -t204, t273)
      t276 = 0.3D1 * t31
      t277 = 0.3D1 * x2
      t278 = -t276 + x3 + t277 - 0.1D1
      t307 = -(-0.180D3 * t55 * t35 * (-x3 + 0.1D1 - t144 * t278) + 0.90
     #D2 * t67 * t35 * (t151 * t278 / 0.2D1 - t155) + t160 * t35 * t278)
     # * t79 / 0.540D3 - (0.90D2 * t74 * (t174 - t171 * t34 * t278) - 0.
     #180D3 * t178 * t82 * t278) * t98 * t79 / 0.270D3
      t308 = FJET(XB1, XB2, s, t139, 0.0D0, -t137, 0.0D0, 0.0D0, t307)
      t310 = x1 * x3
      t311 = t2 * t310
      t312 = t310 * z
      t313 = t56 * x1
      t314 = t56 * t191
      t318 = Sqrt(t129 * t192 * x2 * t130)
      t320 = 0.2D1 * t128 * t318
      t324 = t2 * t189 * (-x3 + t310 - t312 + t127 - t313 + t314 - x2 + 
     #t320) * t193
      t327 = t130 * s * t1 * x1
      t328 = 0.1D1 - x1 + t191 - x2 + t215 - t216 - x3 + t310 - t312 + t
     #127 - t313 + t314 + t320
      t331 = t2 * t189 * t328 * t193
      t332 = 0.2D1 * t312
      t333 = x3 * t10
      t334 = t333 * x1
      t336 = 0.2D1 * t84 * z
      t337 = x1 - t236 - t215 - t221 + t103 + t225 + t231 + z - t310 + t
     #332 + t313 - t334 - t336
      t338 = t84 * t10
      t339 = 0.2D1 * t314
      t340 = t333 * t215
      t342 = 0.2D1 * t84 * t31
      t343 = x2 * t10
      t344 = t84 * t343
      t345 = x3 * z
      t346 = -t85 + t338 - t83 - t339 + t340 + t342 - t344 + t84 - t345 
     #+ t223 + t224 - t227 - t229
      t348 = t235 * (t337 + t346)
      t353 = log(0.4D1 * t206 * t207 * t209 * t130)
      t354 = t353 * t189
      t357 = -x1 + t222 + t215 + t221 - t103 - t226 - t231 - t220 + t310
     # - 0.5D1 * t312 - t313 + 0.4D1 * t334 + t336
      t359 = t85 - t338 + t83 + t339 - t340 - t342 + t344 - t84 + 0.4D1 
     #* t345 - t223 - t224 + t227 + t229
      t360 = t357 + t359
      t369 = 0.90D2 * t74 * (t348 - t354 * t219 * t360) - 0.180D3 * t178
     # * t235 * t360
      t373 = FJET(XB1, XB2, s, t311, t324, -t327, -t331, -t204, -t369 * 
     #t98 * t79 / 0.270D3)
      t375 = t98 * t79
      t378 = 0.3D1 * t343
      t379 = 0.5D1 * t216
      t380 = 0.4D1 * t227
      t381 = -t276 - t215 + z + t231 - t221 + t103 - t236 - t83 + t378 +
     # x1 + t224 + t379 - t380 - t229 + t225
      t382 = t219 * t381
      t387 = t235 * t381
      t407 = -(0.90D2 * t74 * (-t214 * t382 + t238) - 0.180D3 * t178 * t
     #387) * t98 * t79 / 0.270D3 - (-0.180D3 * t102 * wd * (-t252 * t382
     # + t238) + 0.90D2 * t74 * (-t263 + t259 * t382 / 0.2D1) + t268 * t
     #387) * t98 / 0.270D3
      t408 = FJET(XB1, XB2, s, t199, t196, -t195, 0.0D0, -t204, t407)
      t410 = t276 - t277 + 0.1D1
      t411 = t35 * t410
      t425 = t160 * t411
      t429 = t34 * t410
      t454 = t37 - t51 * t411 / 0.540D3 - (-0.180D3 * t55 * t35 * (-0.1D
     #1 - t61 * t410) + 0.90D2 * t67 * t35 * (t61 + t68 * t410 / 0.2D1) 
     #+ t425) * t79 / 0.540D3 - (0.90D2 * t74 * (-t89 * t429 - t82) - 0.
     #180D3 * t178 * t82 * t410) * t98 * t79 / 0.270D3 - (-0.180D3 * t10
     #2 * wd * (-t82 - t107 * t429) + 0.90D2 * t74 * (t115 * t429 / 0.2D
     #1 + t108) + t425) * t98 / 0.270D3
      t455 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t454)
      t457 = -x1 + t236 + t215 + t221 - t103 - t225 - t231 - z + t310 + 
     #t276 - t332 - t313 + t334 + t336
      t458 = t85 - t338 + t83 + t339 - t340 - t342 + t344 - t84 + t345 -
     # t378 - t379 - t224 + t380 + t229
      t459 = t457 + t458
      t468 = 0.90D2 * t74 * (t348 - t354 * t219 * t459) - 0.180D3 * t178
     # * t235 * t459
      t472 = FJET(XB1, XB2, s, -t331, -t327, t324, t311, -t204, -t468 * 
     #t98 * t79 / 0.270D3)
      bbarbbarH7n1e1 = t125 * t124 + t187 * t186 + t274 * t273 + t308 * 
     #t307 - t373 * t369 * t375 / 0.270D3 + t408 * t407 + t455 * t454 - 
     #t472 * t468 * t375 / 0.270D3

      end function



      doubleprecision function bbarbbarH7n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = z * t6
      t8 = x2 * z
      t10 = (0.1D1 - x2 + t8) ** 2
      t11 = 0.1D1 / t10
      t12 = wd * t11
      t13 = x2 * x3
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t16 * t18 * t4
      t23 = log(-0.4D1 * t13 * t20)
      t29 = z * lh
      t30 = t6 * wd
      t33 = 0.720D3 * t29 * t30 * t11
      t35 = 0.1D1 / x3
      t38 = t7 * wd
      t39 = 0.1D1 / x1
      t44 = x1 ** 2
      t45 = x2 * t44
      t48 = log(-0.4D1 * t45 * t20)
      t49 = t48 * z
      t52 = z * t11
      t61 = t18 * t4
      t64 = log(-0.4D1 * x2 * t16 * t61)
      t65 = t64 * z
      t70 = (-0.180D3 * t29 - 0.90D2 * t65) * t6 * t12 / 0.540D3
      t73 = lh ** 2
      t75 = 0.3141592653589793D1 ** 2
      t79 = t64 ** 2
      t83 = (0.180D3 * t65 * lh + z * (0.180D3 * t73 - 0.30D2 * t75) + 0
     #.45D2 * t79 * z) * t6
      t86 = -(0.90D2 * t7 * t12 * (-0.1D1 - 0.4D1 * t23) - t33) * t35 / 
     #0.540D3 - 0.4D1 / 0.3D1 * t38 * t11 * t39 * t35 - (0.90D2 * t30 * 
     #(-0.4D1 * t49 * t11 - t52) - t33) * t39 / 0.270D3 + t70 - t83 * t1
     #2 / 0.135D3
      t87 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t86)
      t89 = 0.2D1 * t13
      t90 = cos(t14)
      t91 = x3 * t4
      t92 = -0.1D1 + x3
      t95 = Sqrt(t91 * x2 * t92)
      t97 = 0.2D1 * t90 * t95
      t99 = t2 * (-x3 + t89 - x2 + t97)
      t101 = t2 * (0.1D1 - x2 - x3 + t89 + t97)
      t102 = 0.4D1 * t92
      t104 = t39 * t35
      t112 = log(0.4D1 * t13 * t16 * t61 * t92)
      t119 = lh * t6 * wd
      t126 = -t38 * t11 * t102 * t104 / 0.3D1 - (0.90D2 * t7 * t12 * (-t
     #112 * t102 + 0.1D1 - x3) - 0.180D3 * t119 * t52 * t102) * t35 / 0.
     #540D3
      t127 = FJET(XB1, XB2, s, 0.0D0, -t99, 0.0D0, t101, 0.0D0, t126)
      t129 = -0.1D1 + x1
      t131 = x1 * z
      t132 = 0.1D1 - x1 + t131
      t133 = 0.1D1 / t132
      t135 = t2 * t129 * x2 * t133
      t136 = t2 * x1
      t139 = t4 * s * t1 * t129
      t144 = s * t6 * x2 * t129 * x1 * t133
      t145 = t30 * t129
      t146 = x2 * x1
      t147 = t146 * z
      t149 = (-0.1D1 + x1 - t131 + x2 - t146 - t8 + t147) ** 2
      t150 = 0.1D1 / t149
      t151 = 0.4D1 * z
      t152 = t44 * t17
      t153 = 0.5D1 * t131
      t154 = 0.2D1 * t147
      t155 = t45 * t17
      t156 = x1 * t17
      t157 = 0.4D1 * t156
      t158 = t146 * t17
      t160 = 0.2D1 * t45 * z
      t162 = 0.2D1 * t44 * z
      t163 = -t146 + t151 - t152 + t45 - t153 + t154 - t44 + x1 + t155 +
     # t157 - t158 - t160 + t162
      t164 = t150 * t163
      t170 = t129 ** 2
      t175 = log(-0.4D1 * t45 * t16 * t18 * t133 * t170 * t4)
      t176 = t175 * t129
      t178 = t129 * t150
      t179 = 0.2D1 * t131
      t180 = t152 - t154 - t162 - t156 - t45 + t146 + t179 + t44 - z - x
     #1 + t158 + t160 - t155
      t181 = t178 * t180
      t191 = -t145 * t164 * t104 / 0.3D1 - (0.90D2 * t30 * (-t176 * t164
     # + t181) - 0.180D3 * t119 * t178 * t163) * t39 / 0.270D3
      t192 = FJET(XB1, XB2, s, 0.0D0, -t135, t136, t139, -t144, t191)
      t194 = 0.3D1 * t8
      t195 = 0.3D1 * x2
      t196 = -t194 + x3 + t195 - 0.1D1
      t212 = -t38 * t11 * t196 * t104 / 0.3D1 - (0.90D2 * t7 * t12 * (-x
     #3 + 0.1D1 - t112 * t196) - 0.180D3 * t119 * t52 * t196) * t35 / 0.
     #540D3
      t213 = FJET(XB1, XB2, s, t101, 0.0D0, -t99, 0.0D0, 0.0D0, t212)
      t215 = x1 * x3
      t216 = t2 * t215
      t217 = t215 * z
      t218 = t13 * x1
      t219 = t13 * t131
      t223 = Sqrt(t91 * t132 * x2 * t92)
      t225 = 0.2D1 * t90 * t223
      t229 = t2 * t129 * (-x3 + t215 - t217 + t89 - t218 + t219 - x2 + t
     #225) * t133
      t232 = t92 * s * t1 * x1
      t233 = 0.1D1 - x1 + t131 - x2 + t146 - t147 - x3 + t215 - t217 + t
     #89 - t218 + t219 + t225
      t236 = t2 * t129 * t233 * t133
      t237 = t44 * x3
      t238 = x2 * t17
      t239 = t237 * t238
      t241 = 0.2D1 * t237 * t8
      t242 = x3 * t17
      t243 = t242 * t146
      t244 = -t154 - t155 + t158 + t160 + t239 - t241 - t243 - t45 - t15
     #7 - t162 + t146 + t152 + t153
      t245 = 0.2D1 * t219
      t246 = t237 * x2
      t247 = t237 * t17
      t248 = t242 * x1
      t251 = 0.2D1 * t237 * z
      t253 = x3 * z
      t255 = -t151 + t44 + t245 + t246 - t247 + 0.4D1 * t248 + t251 - 0.
     #5D1 * t217 - t218 - t237 + 0.4D1 * t253 + t215 - x1
      t258 = t150 * (t244 + t255) * t104
      t261 = FJET(XB1, XB2, s, t216, t229, -t232, -t236, -t144, -t145 * 
     #t258 / 0.3D1)
      t263 = wd * t129
      t267 = 0.3D1 * t238
      t268 = 0.5D1 * t147
      t269 = 0.4D1 * t158
      t270 = -t194 - t146 + z + t162 - t152 + t45 - t179 - t44 + t267 + 
     #x1 + t155 + t268 - t269 - t160 + t156
      t271 = t150 * t270
      t285 = -t145 * t271 * t104 / 0.3D1 - (0.90D2 * t30 * (-t176 * t271
     # + t181) - 0.180D3 * t119 * t178 * t270) * t39 / 0.270D3
      t286 = FJET(XB1, XB2, s, t139, t136, -t135, 0.0D0, -t144, t285)
      t288 = t194 - t195 + 0.1D1
      t296 = 0.180D3 * t119 * t52 * t288
      t303 = t11 * t288
      t314 = -(0.90D2 * t7 * t12 * (-0.1D1 - t23 * t288) - t296) * t35 /
     # 0.540D3 + t70 - t83 * t12 * t288 / 0.540D3 - t38 * t303 * t104 / 
     #0.3D1 - (0.90D2 * t30 * (-t52 - t49 * t303) - t296) * t39 / 0.270D
     #3
      t315 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t314)
      t317 = -t268 - t155 + t269 + t160 + t239 - t241 - t243 - t45 - t15
     #6 - t162 + t146 + t152 + t179 + t194
      t319 = -z + t44 + t245 + t246 - t247 + t248 + t251 - 0.2D1 * t217 
     #- t218 - t267 - t237 + t253 + t215 - x1
      t322 = t150 * (t317 + t319) * t104
      t325 = FJET(XB1, XB2, s, -t236, -t232, t229, t216, -t144, -t145 * 
     #t322 / 0.3D1)
      bbarbbarH7n1e0 = t87 * t86 + t127 * t126 + t192 * t191 + t213 * t2
     #12 - t261 * t6 * t263 * t258 / 0.3D1 + t286 * t285 + t315 * t314 -
     # t325 * t6 * t263 * t322 / 0.3D1

      end function



      doubleprecision function bbarbbarH7n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = z * t6
      t8 = x2 * z
      t10 = (0.1D1 - x2 + t8) ** 2
      t11 = 0.1D1 / t10
      t12 = wd * t11
      t14 = t7 * t12 / 0.6D1
      t17 = x4 * 0.3141592653589793D1
      t18 = Sin(t17)
      t19 = t18 ** 2
      t21 = z ** 2
      t26 = log(-0.4D1 * x2 * t19 / t21 * t4)
      t30 = (-0.180D3 * z * lh - 0.90D2 * t26 * z) * t6
      t33 = 0.1D1 / x3
      t37 = 0.1D1 / x1
      t41 = t14 - t30 * t12 / 0.135D3 - 0.2D1 / 0.3D1 * t7 * t12 * t33 -
     # 0.4D1 / 0.3D1 * t7 * t12 * t37
      t42 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t41)
      t45 = 0.2D1 * x2 * x3
      t46 = cos(t17)
      t48 = -0.1D1 + x3
      t51 = Sqrt(x3 * t4 * x2 * t48)
      t53 = 0.2D1 * t46 * t51
      t55 = t2 * (-x3 + t45 - x2 + t53)
      t57 = t2 * (0.1D1 - x2 - x3 + t45 + t53)
      t58 = t6 * wd
      t59 = t58 * z
      t60 = 0.4D1 * t48
      t65 = FJET(XB1, XB2, s, 0.0D0, -t55, 0.0D0, t57, 0.0D0, -t59 * t11
     # * t60 * t33 / 0.6D1)
      t68 = z * t11
      t73 = -0.1D1 + x1
      t75 = x1 * z
      t77 = 0.1D1 / (0.1D1 - x1 + t75)
      t79 = t2 * t73 * x2 * t77
      t80 = t2 * x1
      t83 = t4 * s * t1 * t73
      t88 = s * t6 * x2 * t73 * x1 * t77
      t89 = t58 * t73
      t90 = x2 * x1
      t91 = t90 * z
      t93 = (-0.1D1 + x1 - t75 + x2 - t90 - t8 + t91) ** 2
      t94 = 0.1D1 / t93
      t96 = x1 ** 2
      t97 = t96 * t21
      t98 = x2 * t96
      t101 = t98 * t21
      t102 = x1 * t21
      t104 = t90 * t21
      t106 = 0.2D1 * t98 * z
      t108 = 0.2D1 * t96 * z
      t109 = -t90 + 0.4D1 * z - t97 + t98 - 0.5D1 * t75 + 0.2D1 * t91 - 
     #t96 + x1 + t101 + 0.4D1 * t102 - t104 - t106 + t108
      t114 = FJET(XB1, XB2, s, 0.0D0, -t79, t80, t83, -t88, -t89 * t94 *
     # t109 * t37 / 0.3D1)
      t117 = t73 * t94
      t122 = 0.3D1 * t8
      t123 = 0.3D1 * x2
      t124 = -t122 + x3 + t123 - 0.1D1
      t129 = FJET(XB1, XB2, s, t57, 0.0D0, -t55, 0.0D0, 0.0D0, -t59 * t1
     #1 * t124 * t33 / 0.6D1)
      t141 = -t122 - t90 + z + t108 - t97 + t98 - 0.2D1 * t75 - t96 + 0.
     #3D1 * x2 * t21 + x1 + t101 + 0.5D1 * t91 - 0.4D1 * t104 - t106 + t
     #102
      t146 = FJET(XB1, XB2, s, t83, t80, -t79, 0.0D0, -t88, -t89 * t94 *
     # t141 * t37 / 0.3D1)
      t153 = t122 - t123 + 0.1D1
      t157 = t11 * t153
      t164 = t14 - t30 * t12 * t153 / 0.540D3 - t59 * t157 * t33 / 0.6D1
     # - t59 * t157 * t37 / 0.3D1
      t165 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t164)
      bbarbbarH7n1em1 = t42 * t41 - t65 * t6 * wd * t68 * t60 * t33 / 0.
     #6D1 - t114 * t6 * wd * t117 * t109 * t37 / 0.3D1 - t129 * t6 * wd 
     #* t68 * t124 * t33 / 0.6D1 - t146 * t6 * wd * t117 * t141 * t37 / 
     #0.3D1 + t165 * t164

      end function



      doubleprecision function bbarbbarH7n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t5 = t2 * (-0.1D1 + x2)
      t6 = t1 ** 2
      t8 = x2 * z
      t10 = (0.1D1 - x2 + t8) ** 2
      t11 = 0.1D1 / t10
      t15 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -0.2D1 / 0.3
     #D1 * z * t6 * wd * t11)
      t17 = t6 * wd
      t25 = z * t11 * (0.3D1 * t8 - 0.3D1 * x2 + 0.1D1)
      t28 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, -t17 * t25 /
     # 0.6D1)
      bbarbbarH7n1em2 = -0.2D1 / 0.3D1 * t15 * z * t17 * t11 - t28 * t6 
     #* wd * t25 / 0.6D1

      end function



      doubleprecision function bbarbbarH7n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH7n1em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH7n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH7n1em4 = 0.0D0

      end function
