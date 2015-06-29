file( GLOB QQ2YYG_STU_COEFFS_D qq2yyg/coeffsstucol6Ddouble.cpp qq2yyg/dirstucol6D/*double.cpp )
file( GLOB QQ2YYG_STU_COEFFS_Q qq2yyg/coeffsstucol6Dquad.cpp qq2yyg/dirstucol6D/*quad.cpp )

if (CLN_FOUND)
    file( GLOB QQ2YYG_STU_COEFFS_R qq2yyg/coeffsstucol6Drat.cpp qq2yyg/dirstucol6D/*rat.cpp )
endif (CLN_FOUND)

file( GLOB QQ2YYG_STU_NOBAR_COEFFS qq2yyg/coeffsstucol6Dnobar.cpp qq2yyg/dirstucol6Dnobar/*.cpp )

add_library(gammagamma
            qq2gammagammaX
            gammagamma
            gammagamma_me
            ${QQ2YYG_STU_COEFFS_D}
            ${QQ2YYG_STU_COEFFS_Q}
            ${QQ2YYG_STU_COEFFS_R}
            ${QQ2YYG_STU_NOBAR_COEFFS}
            )
