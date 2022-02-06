!###############################################################################
!#                                                                             #
!# aed_test.F90                                                                #
!#                                                                             #
!#  Developed by :                                                             #
!#      AquaticEcoDynamics (AED) Group                                         #
!#      School of Agriculture and Environment                                  #
!#      The University of Western Australia                                    #
!#                                                                             #
!#      http://aquatic.science.uwa.edu.au/                                     #
!#                                                                             #
!#  Copyright 2015 - 2021 -  The University of Western Australia               #
!#                                                                             #
!#   GLM is free software: you can redistribute it and/or modify               #
!#   it under the terms of the GNU General Public License as published by      #
!#   the Free Software Foundation, either version 3 of the License, or         #
!#   (at your option) any later version.                                       #
!#                                                                             #
!#   GLM is distributed in the hope that it will be useful,                    #
!#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
!#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
!#   GNU General Public License for more details.                              #
!#                                                                             #
!#   You should have received a copy of the GNU General Public License         #
!#   along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Created Feb 2015                                                            #
!#                                                                             #
!###############################################################################

#include "aed+.h"

!
MODULE aed_test
!-------------------------------------------------------------------------------
! aed_test --- test model
!
! The AED2 module test contains basic equations that have no dependencies
!-------------------------------------------------------------------------------
   USE aed_core

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed_test_data_t
!
   TYPE,extends(aed_model_data_t) :: aed_test_data_t
      !# Variable identifiers
      INTEGER :: id_tst_pel, id_tst_ben, id_tst_zon_ben, id_tst_zon_pel
      INTEGER :: id_tst_flux_pel, id_tst_zon_temp, id_tst_zon_rad
      INTEGER :: id_tst_lh, id_tst_act, id_tst_act2, id_tst_act3
      INTEGER :: id_par, id_nir, id_uva, id_uvb, id_tem
      INTEGER :: id_tst_par, id_tst_nir, id_tst_uva, id_tst_uvb
      INTEGER :: id_sed_zone, id_sedz
      INTEGER :: id_coln, id_colid, id_cid_s1, id_cid_s2, id_fsedza

      CONTAINS
         PROCEDURE :: define             => aed_define_test
         PROCEDURE :: calculate          => aed_calculate_test
         PROCEDURE :: calculate_benthic  => aed_calculate_benthic_test
         PROCEDURE :: calculate_riparian => aed_calculate_riparian_test
         PROCEDURE :: calculate_dry      => aed_calculate_dry_test
!        PROCEDURE :: equilibrate        => aed_equilibrate_test
!        PROCEDURE :: mobility           => aed_mobility_test
!        PROCEDURE :: light_extinction   => aed_light_extinction_test
!        PROCEDURE :: delete             => aed_delete_test
   END TYPE

!MODULE GLOBALS
   INTEGER :: diag_level = 10

!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed_define_test(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the AED model
!
!  Here, the aed namelist is read and the variables exported
!  by the model are registered with AED
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: namlst
   CLASS (aed_test_data_t),INTENT(inout) :: data
!
!LOCALS
!
!-------------------------------------------------------------------------------
!BEGIN
   print *,"        aed_test configuration"

   data%id_tst_lh = aed_locate_global('layer_ht')
   data%id_tst_pel = aed_define_variable("pel",'mmol/m**3','test_pel', zero_)
   data%id_tst_flux_pel = aed_define_variable("pel_bflux",'mmol/m**3','pelagic variable getting fluxed to from a zone', 0.0001)
   data%id_tst_ben = aed_define_sheet_variable("ben",'mmol/m**2','test_ben', zero_)
   data%id_tst_act = aed_define_sheet_diag_variable("act",'mmol/m**2','active column')
   data%id_tst_act2 = aed_define_sheet_diag_variable("act2",'mmol/m**2','non-zero depth column')
   data%id_tst_act3 = aed_define_sheet_diag_variable("act3",'mmol/m**2','active XOR non-zero depth')

   data%id_sed_zone = aed_locate_sheet_global('sed_zone')
   data%id_tst_zon_ben = aed_define_sheet_diag_variable("zonID",'num','sed_zone ID of this unit')
   data%id_tst_zon_pel = aed_define_diag_variable("zonLY",'num','sed_zone impacting this water layer')
   data%id_tst_zon_temp = aed_define_diag_variable("ztemp",'-','avg temp of this sedzone region')
   data%id_tst_zon_rad = aed_define_diag_variable("zrad",'-','avg rad of this sedzone region')

   data%id_par = aed_locate_global('par')
   data%id_nir = aed_locate_global('nir')
   data%id_uva = aed_locate_global('uva')
   data%id_uvb = aed_locate_global('uvb')
   data%id_tem = aed_locate_global('temperature')

   data%id_tst_par = aed_define_diag_variable('tst_par', '', 'test PAR')
   data%id_tst_nir = aed_define_diag_variable('tst_nir', '', 'test NIR')
   data%id_tst_uva = aed_define_diag_variable('tst_uva', '', 'test UVA')
   data%id_tst_uvb = aed_define_diag_variable('tst_uvb', '', 'test UVB')

   data%id_colid = aed_define_diag_variable('colid', 'no units', 'DBG column id diag')
!#                  aed_define_sheet_diag_variable(name, units, longname, surf, zavg) RESULT(ret)
   data%id_cid_s1 = aed_define_sheet_diag_variable('cid_s1', 'no units', 'DBG sheet colid NZA', .FALSE., zavg=.FALSE.)
   data%id_cid_s2 = aed_define_sheet_diag_variable('cid_s2', 'no units', 'DBG sheet colid ZA', .FALSE., zavg=.TRUE.)

   data%id_fsedza = aed_locate_sheet_variable('SDF_Fsed_poc',update_from_zone=.TRUE.)

   data%id_coln = aed_locate_sheet_global('col_num')
   data%id_sedz = aed_locate_sheet_global('sed_zone')
END SUBROUTINE aed_define_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_test(data,column,layer_idx)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_test_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   AED_REAL :: pel
!
!-------------------------------------------------------------------------------
!BEGIN
   pel = _STATE_VAR_(data%id_tst_pel)
   pel = 20 - pel
   _FLUX_VAR_(data%id_tst_pel) = pel / secs_per_day

   _DIAG_VAR_(data%id_tst_par) = _STATE_VAR_(data%id_par)
   _DIAG_VAR_(data%id_tst_nir) = _STATE_VAR_(data%id_nir)
   _DIAG_VAR_(data%id_tst_uva) = _STATE_VAR_(data%id_uva)
   _DIAG_VAR_(data%id_tst_uvb) = _STATE_VAR_(data%id_uvb)

   _DIAG_VAR_(data%id_colid) = _STATE_VAR_S_(data%id_coln)
END SUBROUTINE aed_calculate_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_riparian_test(data,column,layer_idx, pc_wet)
!-------------------------------------------------------------------------------
! Calculate riparian fluxes and benthic sink and source terms of AED test.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_test_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
   AED_REAL,INTENT(in) :: pc_wet
!
!LOCALS
   ! Temporary variables
   AED_REAL :: ben, lh, sedz
!
!-------------------------------------------------------------------------------
!BEGIN
   _DIAG_VAR_S_(data%id_tst_act) = pc_wet

   sedz = _STATE_VAR_S_(data%id_sed_zone)
   _DIAG_VAR_(data%id_tst_zon_pel) = sedz

   lh = _STATE_VAR_(data%id_tst_lh)
   IF (lh > 0.0) THEN
      lh = lh + 100
   ELSE
      lh = 0.0
   ENDIF
   _DIAG_VAR_S_(data%id_tst_act2) = lh

   IF ( pc_wet > 0.0 ) THEN
      IF ( lh > 0.0 ) THEN
          _DIAG_VAR_S_(data%id_tst_act3) = 0.0
      ELSE
          _DIAG_VAR_S_(data%id_tst_act3) = 1.0
      ENDIF
   ELSE
      IF ( lh > 0.0 ) THEN
          _DIAG_VAR_S_(data%id_tst_act3) = 2.0
      ELSE
          _DIAG_VAR_S_(data%id_tst_act3) = 0.0
      ENDIF
   ENDIF
END SUBROUTINE aed_calculate_riparian_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_benthic_test(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate pelagic bottom fluxes and benthic sink and source terms of AED test.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_test_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Temporary variables
   AED_REAL :: ben, lh, sedz
!
!-------------------------------------------------------------------------------
!BEGIN
   ben = _STATE_VAR_S_(data%id_tst_ben)
   ben = 10 - ben
   _FLUX_VAR_B_(data%id_tst_ben) = ben / secs_per_day

   sedz = _STATE_VAR_S_(data%id_sed_zone)
   _DIAG_VAR_S_(data%id_tst_zon_ben) = sedz
   _DIAG_VAR_(data%id_tst_zon_pel) = sedz
   _DIAG_VAR_(data%id_tst_zon_temp) = _STATE_VAR_(data%id_tem)
   _DIAG_VAR_(data%id_tst_zon_rad) = _STATE_VAR_(data%id_par)
   !## TEST FLUX VAR TO DIAGNOSE flux_pel beign disaggregated onto non-benthic variables, from sediment zones
   _FLUX_VAR_(data%id_tst_flux_pel) = 0.01*sedz / secs_per_day

   _DIAG_VAR_S_(data%id_cid_s1) = _DIAG_VAR_(data%id_colid)
   _DIAG_VAR_S_(data%id_cid_s2) = _DIAG_VAR_(data%id_colid)
END SUBROUTINE aed_calculate_benthic_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_dry_test(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate fluxes and values for dry column.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_test_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Temporary variables
   AED_REAL :: ben
!
!-------------------------------------------------------------------------------
!BEGIN
END SUBROUTINE aed_calculate_dry_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#if 0
!###############################################################################
SUBROUTINE aed_bio_drag_test(data, column, layer_idx, drag)
   USE aed_macrophyte
!-------------------------------------------------------------------------------
! Get the effect of macrophyte biomass on benthic drag
!
!  WARNING - THIS IS ADDING MACROPHYTE EFFECT TO ALL CELLS IN WATER COLUMN
!
!-------------------------------------------------------------------------------
   CLASS (aed_macrophyte_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
   AED_REAL,INTENT(inout) :: drag
!
!LOCALS
   AED_REAL :: dz,mphy,sedz,vel
   INTEGER  :: mphy_i
   AED_REAL :: veg_height, diameter, n_stems
   AED_REAL :: K_CD
!-------------------------------------------------------------------------------
!BEGIN

   ! Hard set the parameters, for now (we'll read in via namelist later)
   veg_height = 0.1
   diameter = 10.
   n_stems = 2.

   ! get local macrophyte density
   mphy = zero_ ! _STATE_VAR_S_(data%id_veg) !must be configured in "define"

   ! get enviroment info from host - cell thickness and velocity.
   dz   = zero_ !_STATE_VAR_(data%id_dz)  ! dz = 0.5.   !must be configured in "define"
   vel  = zero_ !_STATE_VAR_(data%id_cellvel)  ! vel = 0.1.  !must be configured in "define"

   ! now do computaion of drag or whatever I like
   K_CD = zero_ ! write some equation here to compute drag, etc etc etc

   !# additional drag due to biomass
   drag = drag + (K_CD * (mphy/dz) )

END SUBROUTINE aed_bio_drag_test
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#endif


END MODULE aed_test
