!=======================================================================
  Subroutine Det_d8 (id,ML,MS,Idet)
!=======================================================================
! determinamt id and corresponding ML,MS for subshell d8 
!-----------------------------------------------------------------------
 
  Implicit none
 
  Integer, intent(in)  :: id
  Integer, intent(out) :: ML,MS,Idet(*)
 
  Integer, parameter :: iq_d8 =   8
  Integer, parameter :: kd_d8 =  45
 
  Integer :: Idet_d8 (iq_d8,kd_d8)
 
  Integer :: ML_d8 (kd_d8)
  Integer :: MS_d8 (kd_d8)
 
  if(id.le.0.or.id.gt.kd_d8) Stop "Det_d8: index id is out of range"
 
  ML = ML_d8 (id)
  MS = MS_d8 (id)
 
  Idet (1:iq_d8)= Idet_d8 (:,id)
 

  Data Idet_d8 ( 1,:)/ &
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, &
   2, 2, 3  /

  Data Idet_d8 ( 2,:)/ &
   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3, 3, 3, 3, 3, &
   3, 4, 4  /

  Data Idet_d8 ( 3,:)/ &
   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 5, 5, 4, 4, 4, 4, 4, 4, &
   5, 5, 5  /

  Data Idet_d8 ( 4,:)/ &
   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 5, 6, 6, 6, 5, 5, 5, 5, 5, 6, &
   6, 6, 6  /

  Data Idet_d8 ( 5,:)/ &
   5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 6, 6, 6, 6, 7, 7, 6, 6, 6, 6, 7, 7, 7, 6, 6, 6, 6, 7, 7, 7, 7, 6, 6, 6, 6, 7, 7, &
   7, 7, 7  /

  Data Idet_d8 ( 6,:)/ &
   6, 6, 6, 6, 6, 6, 7, 7, 7, 8, 7, 7, 7, 8, 8, 7, 7, 7, 8, 8, 8, 7, 7, 7, 8, 8, 8, 8, 7, 7, 7, 8, 8, 8, 8, 8, 7, 7, 7, 8, 8, 8, &
   8, 8, 8  /

  Data Idet_d8 ( 7,:)/ &
   7, 7, 7, 8, 8, 9, 8, 8, 9, 9, 8, 8, 9, 9, 9, 8, 8, 9, 9, 9, 9, 8, 8, 9, 9, 9, 9, 9, 8, 8, 9, 9, 9, 9, 9, 9, 8, 8, 9, 9, 9, 9, &
   9, 9, 9  /

  Data Idet_d8 ( 8,:)/ &
   8, 9,10, 9,10,10, 9,10,10,10, 9,10,10,10,10, 9,10,10,10,10,10, 9,10,10,10,10,10,10, 9,10,10,10,10,10,10,10, 9,10,10,10,10,10, &
  10,10,10  /

  Data ML_d8 / &
 -7,  1,  1,  1,  1,  9, -5, -5,  3,  3, -5, -5,  3,  3, -3, -1, -1,  7,  7,  1,  1, -1, -1,  7,  7,  1,  1,  5, -3, -3,  5,  5, &
 -1, -1,  3,  3, -3, -3,  5,  5, -1, -1,  3,  3,  1  /

  Data MS_d8 / &
  1, -1,  1,  1,  3,  1, -1,  1, -1,  1,  1,  3,  1,  3,  1, -1,  1, -1,  1, -1,  1,  1,  3,  1,  3,  1,  3,  1, -1,  1, -1,  1, &
 -1,  1, -1,  1,  1,  3,  1,  3,  1,  3,  1,  3,  1  /
 
  End Subroutine Det_d8 
