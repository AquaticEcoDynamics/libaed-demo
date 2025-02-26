###############################################################################
#                                                                             #
# Makefile to build libaed-demo                                               #
#                                                                             #
#  Developed by :                                                             #
#      AquaticEcoDynamics (AED) Group                                         #
#      School of Agriculture and Environment                                  #
#      The University of Western Australia                                    #
#                                                                             #
#      http://aquatic.science.uwa.edu.au/                                     #
#                                                                             #
#  Copyright 2013 - 2025 -  The University of Western Australia               #
#                                                                             #
#   AED is free software: you can redistribute it and/or modify               #
#   it under the terms of the GNU General Public License as published by      #
#   the Free Software Foundation, either version 3 of the License, or         #
#   (at your option) any later version.                                       #
#                                                                             #
#   AED is distributed in the hope that it will be useful,                    #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#   GNU General Public License for more details.                              #
#                                                                             #
#   You should have received a copy of the GNU General Public License         #
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                             #
###############################################################################

LIBAEDDMO=aed-demo
OUTLIB=lib$(LIBAEDDMO)

INCLUDES=-I../libaed-water/${incdir}  -I../libaed-water/${moddir}

include ../libaed-water/make_defs.inc

OBJS=${objdir}/aed_test.o \
     ${objdir}/aed_testptm.o \
     ${objdir}/aed_environ.o \
     ${objdir}/aed_demo.o

include ../libaed-water/make_rules.inc
