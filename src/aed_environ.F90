!###############################################################################
!#                                                                             #
!# aed_environ.F90                                                             #
!#                                                                             #
!#  Developed by :                                                             #
!#      AquaticEcoDynamics (AED) Group                                         #
!#      School of Agriculture and Environment                                  #
!#      The University of Western Australia                                    #
!#                                                                             #
!#      http://aquatic.science.uwa.edu.au/                                     #
!#                                                                             #
!#  Copyright 2025 -  The University of Western Australia                      #
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
!# Created Feb 2025                                                            #
!#                                                                             #
!###############################################################################

#include "aed.h"

#define MAX_ENV 100

!
MODULE aed_environ
!-------------------------------------------------------------------------------
! aed_environ --- environ model
!
! The AED module environ contains basic equations that are all dependencies
!-------------------------------------------------------------------------------
   USE aed_core

   IMPLICIT NONE

   PRIVATE
!
   PUBLIC aed_environ_data_t
!
   TYPE,extends(aed_model_data_t) :: aed_environ_data_t
      INTEGER :: n_ev, n_ev_s
      !# Variable identifiers
      INTEGER :: id_env(100)
      INTEGER :: id_d_env(100)
      INTEGER :: id_env_s(100)
      INTEGER :: id_d_env_s(100)

      CONTAINS
         PROCEDURE :: define             => aed_define_environ
         PROCEDURE :: calculate          => aed_calculate_environ
         PROCEDURE :: calculate_benthic  => aed_calculate_benthic_environ
         PROCEDURE :: calculate_riparian => aed_calculate_riparian_environ
         PROCEDURE :: calculate_dry      => aed_calculate_dry_environ
!        PROCEDURE :: equilibrate        => aed_equilibrate_environ
!        PROCEDURE :: mobility           => aed_mobility_environ
!        PROCEDURE :: light_extinction   => aed_light_extinction_environ
!        PROCEDURE :: delete             => aed_delete_environ
   END TYPE

!MODULE GLOBALS
   INTEGER :: diag_level = 10

!===============================================================================
CONTAINS



!###############################################################################
SUBROUTINE aed_define_environ(data, namlst)
!-------------------------------------------------------------------------------
! Initialise the AED model
!
!  Here, the aed namelist is read and the variables exported
!  by the model are registered with AED
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: namlst
   CLASS (aed_environ_data_t),INTENT(inout) :: data
!
!LOCALS
   INTEGER :: ev
   TYPE(aed_variable_t),POINTER :: tvar

!  %% NAMELIST   %%  /aed_testptm/
!  AED_REAL :: vvel_new = 0.
!  AED_REAL :: vvel_old = 0.
!  AED_REAL :: decay_rate_new = 0.
!  AED_REAL :: decay_rate_old = 0.
!  AED_REAL :: mass_limit = 10.
!  AED_REAL :: X_cdw = 0.5
!  AED_REAL :: X_nc = 0.1
!  AED_REAL :: X_pc = 0.01
!  AED_REAL :: X_dwww = 1.0

! %% From Module Global
!  INTEGER :: diag_level = 10                ! 0 = no diagnostic outputs
!                                            ! 1 = basic diagnostic outputs
!                                            ! 2 = flux rates, and supporitng
!                                            ! 3 = other metrics
!                                            !10 = all debug & checking outputs
!  %% END NAMELIST   %%  /aed_testptm/

!  NAMELIST /aed_environ/ vvel_new, vvel_old,                 &
!                         decay_rate_new, decay_rate_old,     &
!                         mass_limit, extra_diag, diag_level, &
!                         X_dwww, X_cdw, X_nc, X_pc

!
!-------------------------------------------------------------------------------
!BEGIN
   print *,"        aed_environ configuration"

!  ! Read the namelist
!  read(namlst,nml=aed_testptm,iostat=status)
!  IF (status /= 0) STOP 'Error reading namelist aed_testptm'

   data%n_ev = 0 ; data%n_ev_s = 0
   ev = 1
   DO WHILE (aed_get_var(ev, tvar))
         print*,"var :", tvar%name,tvar%index,tvar%units,tvar%longname,tvar%extern
      IF ( tvar%extern ) THEN
         print*,"extern var :", tvar%name,tvar%index,tvar%units,tvar%longname
         IF ( tvar%sheet ) THEN
           data%n_ev_s = data%n_ev_s + 1
           data%id_env_s(data%n_ev_s) = aed_locate_sheet_global(tvar%name)
           data%id_d_env_s(data%n_ev_s) =                                      &
                          aed_define_sheet_diag_variable(tvar%name,tvar%units, &
                                               tvar%longname,tvar%top,tvar%zavg)
         ELSE
           data%n_ev = data%n_ev + 1
           data%id_env(data%n_ev) = aed_locate_global(tvar%name)
           data%id_d_env(data%n_ev) = aed_define_diag_variable(tvar%name,      &
                                                      tvar%units, tvar%longname)
         ENDIF
      ENDIF
      ev = ev + 1
   ENDDO
END SUBROUTINE aed_define_environ
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_environ(data,column,layer_idx)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_environ_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   INTEGER :: ev
!
!-------------------------------------------------------------------------------
!BEGIN
   DO ev=1,data%n_ev
     _DIAG_VAR_(data%id_d_env(ev)) = _STATE_VAR_(data%id_env(ev))
   ENDDO
END SUBROUTINE aed_calculate_environ
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_riparian_environ(data,column,layer_idx, pc_wet)
!-------------------------------------------------------------------------------
! Calculate riparian fluxes and benthic sink and source terms of AED environ.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_environ_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
   AED_REAL,INTENT(in) :: pc_wet
!
!LOCALS
   INTEGER :: ev
!
!-------------------------------------------------------------------------------
!BEGIN
   DO ev=1,data%n_ev_s
      _DIAG_VAR_S_(data%id_d_env_s(ev)) = _STATE_VAR_S_(data%id_env_s(ev))
   ENDDO
END SUBROUTINE aed_calculate_riparian_environ
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_benthic_environ(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate pelagic bottom fluxes and benthic sink and source terms of AED environ.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_environ_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   INTEGER :: ev
!
!-------------------------------------------------------------------------------
!BEGIN
   DO ev=1,data%n_ev_s
      _DIAG_VAR_S_(data%id_d_env_s(ev)) = _STATE_VAR_S_(data%id_env_s(ev))
   END DO
END SUBROUTINE aed_calculate_benthic_environ
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_calculate_dry_environ(data,column,layer_idx)
!-------------------------------------------------------------------------------
! Calculate fluxes and values for dry column.
! Everything in units per surface area (not volume!) per time.
!-------------------------------------------------------------------------------
!ARGUMENTS
   CLASS (aed_environ_data_t),INTENT(in) :: data
   TYPE (aed_column_t),INTENT(inout) :: column(:)
   INTEGER,INTENT(in) :: layer_idx
!
!LOCALS
   ! Temporary variables
   AED_REAL :: ben
!
!-------------------------------------------------------------------------------
!BEGIN
END SUBROUTINE aed_calculate_dry_environ
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


END MODULE aed_environ
