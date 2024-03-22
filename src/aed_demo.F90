!###############################################################################
!#                                                                             #
!# aed_demo.F90                                                                #
!#                                                                             #
!#  Developed by :                                                             #
!#      AquaticEcoDynamics (AED) Group                                         #
!#      School of Agriculture and Environment                                  #
!#      The University of Western Australia                                    #
!#                                                                             #
!#      http://aquatic.science.uwa.edu.au/                                     #
!#                                                                             #
!#  Copyright 2013 - 2024 -  The University of Western Australia               #
!#                                                                             #
!#   AED is free software: you can redistribute it and/or modify               #
!#   it under the terms of the GNU General Public License as published by      #
!#   the Free Software Foundation, either version 3 of the License, or         #
!#   (at your option) any later version.                                       #
!#                                                                             #
!#   AED is distributed in the hope that it will be useful,                    #
!#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
!#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
!#   GNU General Public License for more details.                              #
!#                                                                             #
!#   You should have received a copy of the GNU General Public License         #
!#   along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Created May 2020                                                            #
!#                                                                             #
!###############################################################################

#include "aed.h"


!###############################################################################
MODULE aed_demo
!-------------------------------------------------------------------------------
   USE aed_core, ONLY : aed_model_data_t

   USE aed_test
   USE aed_testptm
!  USE aed_dummy

   IMPLICIT NONE

   !#---------------------------------------------------------------------------

   PRIVATE   !# By default make everything private

   PUBLIC aed_new_dmo_model, aed_print_dmo_version

   !#---------------------------------------------------------------------------

CONTAINS
!===============================================================================


!###############################################################################
FUNCTION aed_new_dmo_model(modelname) RESULT(model)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CHARACTER(*),INTENT(in) :: modelname
!
!LOCALS
   CLASS (aed_model_data_t),POINTER :: model
   CHARACTER(len=4) :: prefix
!
!-------------------------------------------------------------------------------
!BEGIN
   NULLIFY(model)

   SELECT CASE (modelname)
!     CASE ('aed_dummy');          prefix = 'DUM'; ALLOCATE(aed2_dummy_data_t::model)
      CASE ('aed_test');           prefix = 'TST'; ALLOCATE(aed_test_data_t::model)
      CASE ('aed_testptm');        prefix = 'TPT'; ALLOCATE(aed_testptm_data_t::model)
   END SELECT

   IF (ASSOCIATED(model)) THEN
      model%aed_model_name = modelname
      model%aed_model_prefix = prefix
   ENDIF
END FUNCTION aed_new_dmo_model
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_print_dmo_version
!-------------------------------------------------------------------------------
!BEGIN
   print*,"    libaed-demo version ", TRIM(AED_VERSION)
#ifdef __INTEL_COMPILER
   print*,"    libaed built using intel fortran version ", __INTEL_COMPILER
#else
# ifdef __PGI
   print*,"    libaed built using pgfortran version ", __PGIC__, '.', __PGIC_MINOR__, '.', __PGIC_PATCHLEVEL__
# else
#  ifdef __GNUC__
    print*,"    libaed built using gfortran version ", __GNUC__, '.', __GNUC_MINOR__, '.', __GNUC_PATCHLEVEL__
#  else
#   ifdef __clang__
     print*,"    libaed built using flang version ", __clang_major__, '.', __clang_minor__, '.', __clang_patchlevel__
#   else
     print*,"    libaed built using unknown fortran version "
#   endif
#  endif
# endif
#endif
END SUBROUTINE aed_print_dmo_version
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!===============================================================================
END MODULE aed_demo
