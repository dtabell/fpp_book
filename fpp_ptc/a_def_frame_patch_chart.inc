!The Polymorphic Tracking Code
!Copyright (C) Etienne Forest and CERN

TYPE AFFINE_FRAME
   REAL(DP), POINTER,DIMENSION(:)  ::   ANGLE(:)
   REAL(DP), POINTER,DIMENSION(:)  ::   D(:)
   REAL(DP), POINTER,DIMENSION(:)  ::   A
   REAL(DP), POINTER,DIMENSION(:,:)::   ENT
   REAL(DP), POINTER,DIMENSION(:)  ::   B
   REAL(DP), POINTER,DIMENSION(:,:)::   EXI
END TYPE AFFINE_FRAME


TYPE MAGNET_FRAME
   REAL(DP), POINTER,DIMENSION(:)  ::   A
   REAL(DP), POINTER,DIMENSION(:,:)::   ENT
   REAL(DP), POINTER,DIMENSION(:)  ::   O
   REAL(DP), POINTER,DIMENSION(:,:)::   MID
   REAL(DP), POINTER,DIMENSION(:)  ::   B
   REAL(DP), POINTER,DIMENSION(:,:)::   EXI
END TYPE MAGNET_FRAME

TYPE PATCH
   INTEGER(2), POINTER:: PATCH    ! IF TRUE, SPACIAL PATCHES NEEDED
   INTEGER, POINTER :: A_X1,A_X2   ! FOR ROTATION OF PI AT ENTRANCE = -1, DEFAULT = 1 ,
   INTEGER, POINTER :: B_X1,B_X2   ! FOR ROTATION OF PI AT EXIT = -1    , DEFAULT = 1
   REAL(DP),DIMENSION(:), POINTER:: A_D,B_D      !ENTRACE AND EXIT TRANSLATIONS  A_D(3)
   REAL(DP),DIMENSION(:), POINTER:: A_ANG,B_ANG   !ENTRACE AND EXIT ROTATIONS    A_ANG(3)
   INTEGER(2), POINTER:: ENERGY   ! IF TRUE, ENERGY PATCHES NEEDED
   INTEGER(2), POINTER:: TIME     ! IF TRUE, TIME PATCHES NEEDED
   REAL(DP), POINTER:: A_T,B_T     ! TIME SHIFT NEEDED SOMETIMES WHEN RELATIVE TIME IS USED
   REAL(DP), POINTER:: p0b,b0b     ! for bmad patching
END TYPE PATCH

TYPE CHART
   type(magnet_frame), pointer :: f
   !  FIBRE MISALIGNMENTS
   real(dp),dimension(:),  POINTER::   D_IN,ANG_IN
   real(dp),dimension(:),  POINTER::   D_OUT,ANG_OUT
END TYPE CHART
