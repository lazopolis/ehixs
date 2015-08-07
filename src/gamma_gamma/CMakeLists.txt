file( GLOB QQ2YYG_Z1_COEFFS_D qq2yyg/qq2yygz1double.cpp qq2yyg/coeffsz1/*double.cpp )
file( GLOB QQ2YYG_Z1_COEFFS_Q qq2yyg/qq2yygz1quad.cpp   qq2yyg/coeffsz1/*quad.cpp )
file( GLOB QQ2YYG_Z0_COEFFS_D qq2yyg/qq2yygz0double.cpp qq2yyg/coeffsz0/*double.cpp )
file( GLOB QQ2YYG_Z0_COEFFS_Q qq2yyg/qq2yygz0quad.cpp   qq2yyg/coeffsz0/*quad.cpp )

if (CLN_FOUND)
    file( GLOB QQ2YYG_Z1_COEFFS_R qq2yyg/qq2yygz1rat.cpp qq2yyg/coeffsz1/*rat.cpp )
    file( GLOB QQ2YYG_Z0_COEFFS_R qq2yyg/qq2yygz0rat.cpp qq2yyg/coeffsz0/*rat.cpp )
endif (CLN_FOUND)

add_library(gammagamma
            qq2gammagammaX
            gammagamma
            gammagamma_me
            ${QQ2YYG_Z1_COEFFS_D}
            ${QQ2YYG_Z1_COEFFS_Q}
            ${QQ2YYG_Z1_COEFFS_R}
            ${QQ2YYG_Z0_COEFFS_D}
            ${QQ2YYG_Z0_COEFFS_Q}
            ${QQ2YYG_Z0_COEFFS_R}
            )
