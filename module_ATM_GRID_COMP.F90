#include "../ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_ATM_GRID_COMP
!
!-----------------------------------------------------------------------
!***  This module contains codes directly related to the ATM component.
!-----------------------------------------------------------------------
!
!***  The ATM component lies in the hierarchy seen here:
!
!          Main program
!               |
!               |
!          NEMS component
!               |     |________________________.
!               |                              |
!          EARTH component        Ensemble Coupler component
!              /|\
!             / | \
!          ATM/OCN/ICE components
!          |    |
!          |    |
!          |    |
!          |    (MOM5, HYCOM, etc.)
!          |
!          CORE component (GFS, NMM, FIM, GEN, etc.)
!
!-----------------------------------------------------------------------
!  2011-05-11  Theurich & Yang  - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  2011-10/04  Yang  - Modified for using the ESMF 5.2.0r library.
!  2013-07     Theurich - NUOPC option to be compliant with ESMF 6.2.0 reference.
!-----------------------------------------------------------------------
!
      USE esmf_mod

#ifdef WITH_NUOPC
      use NUOPC
      use NUOPC_Model, only: &
        model_routine_SS    => routine_SetServices, &
        model_label_Advance => label_Advance
#endif

!
      USE module_ATM_INTERNAL_STATE,ONLY: ATM_INTERNAL_STATE            &
                                         ,WRAP_ATM_INTERNAL_STATE
!
      USE module_NMM_GRID_COMP,ONLY: NMM_REGISTER
      USE module_GFS_GRID_COMP,ONLY: GFS_REGISTER
      USE module_FIM_GRID_COMP,ONLY: FIM_REGISTER
      USE module_GEN_GRID_COMP,ONLY: GEN_REGISTER   ! For the "Generic Core" gridded component.
!
      USE module_ERR_MSG,ONLY: ERR_MSG,MESSAGE_CHECK
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: ATM_REGISTER
!
!-----------------------------------------------------------------------
!
      TYPE(ATM_INTERNAL_STATE),POINTER,SAVE :: ATM_INT_STATE
      TYPE(WRAP_ATM_INTERNAL_STATE)   ,SAVE :: WRAP
!
      TYPE(ESMF_Clock),SAVE :: CLOCK_ATM                                   !<-- The Clock of the ATM component
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE ATM_REGISTER(ATM_GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!***  Register the Init, Run, and Finalize routines of 
!***  the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      INTEGER            ,INTENT(OUT)   :: RC_REG                          !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------

#ifdef WITH_NUOPC
      ! the NUOPC model component will register the generic methods
      call model_routine_SS(ATM_GRID_COMP, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! The default InitP0 which will set InitializePhaseMap to IPDv00

      ! NUOPC_Model IPDv00 requires InitP1, where Fields should be advertised
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP, ESMF_METHOD_INITIALIZE, &
        InitializeP1, phase=1, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! NUOPC_Model IPDv00 requires InitP2, where Fields should be realized,
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP, ESMF_METHOD_INITIALIZE, &
        InitializeP2, phase=2, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! attach specializing method(s)
      call ESMF_MethodAdd(ATM_GRID_COMP, label=model_label_Advance, &
        userRoutine=ATM_ADVANCE, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! Overwrite generic NUOPC_Model Finalize method
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP, ESMF_METHOD_FINALIZE, &
        ATM_FINALIZE, phase=1, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#ifdef FRONT_HYCOM        
      ! extend the NUOPC Field Dictionary to hold required entries
      if (.not.NUOPC_FieldDictionaryHasEntry("air_temperature_at_sea_level")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="air_temperature_at_sea_level", &
          canonicalUnits="K", defaultLongName="Air Temperature at Sea Level", &
          defaultShortName="tmsl", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
#endif
#ifdef FRONT_MOM5
#endif

#else

!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for ATM Initialize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP                     &  !<-- The ATM component
                                     ,ESMF_METHOD_INITIALIZE            &  !<-- Subroutine type (Initialize)
                                     ,ATM_INITIALIZE                    &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for ATM Run"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!

      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP                     &  !<-- The ATM component
                                     ,ESMF_METHOD_RUN                   &  !<-- Subroutine type (Run)
                                     ,ATM_RUN                           &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for ATM Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP                     &  !<-- The ATM component
                                     ,ESMF_METHOD_FINALIZE              &  !<-- Subroutine type (Finalize)
                                     ,ATM_FINALIZE                      &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
#endif
!-----------------------------------------------------------------------
!
      IF(RC_REG==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_REGISTER succeeded'
      ELSE
        WRITE(0,*)' ATM_REGISTER failed  RC_REG=',RC_REG
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ATM_REGISTER
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    type(ESMF_VM)                          :: vm
    type(ESMF_Grid)                        :: gridIn
    type(ESMF_Grid)                        :: gridOut

    integer                                :: npet, npet_x, npet_y

    rc = ESMF_SUCCESS
#ifdef FRONT_HYCOM
    ! importable field: sea_surface_temperature
    call NUOPC_StateAdvertiseField(importState, &
      StandardName="sea_surface_temperature", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="air_pressure_at_sea_level", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: air_temperature_at_sea_level
    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="air_temperature_at_sea_level", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, petCount=npet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create a Grid object for Fields
    ! we are going to create a single tile tripolar grid from a gridspec
    ! file. For now, a simple decomposition is used. 
    !gridIn = ESMF_GridCreate('ocean_hgrid.nc', ESMF_FILEFORMAT_GRIDSPEC, &
    !    (/1, npet/), isSphere=.true., coordNames=(/'x', 'y'/), rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    gridIn = NUOPC_GridCreateSimpleXY( &
      0._ESMF_KIND_R8, 5.75_ESMF_KIND_R8, &
      -1.5_ESMF_KIND_R8, 2.0_ESMF_KIND_R8, &
      100, 100, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    gridOut = gridIn ! for now out same as in

    call ATM_RealizeImportFields(importState, gridIn, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ATM_RealizeExportFields(exportState, gridOut, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine
  
  !-----------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Grid)         :: gridIn, gridOut
    type(ESMF_Field)        :: field

    rc = ESMF_SUCCESS
    
    ! call into the actual NEMS/ATM initialize rooutine
    
    call ATM_INITIALIZE(gcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef FRONT_HYCOM    
    ! create a DUMMY Grid object for Fields
    
    gridIn = NUOPC_GridCreateSimpleXY( &
      x_min=10._ESMF_KIND_R8,   x_max=20._ESMF_KIND_R8, &
      y_min=100._ESMF_KIND_R8,  y_max=200._ESMF_KIND_R8, &
      i_count=1000, j_count=100, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    gridOut = gridIn
      
    ! realize Fields
    
    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: air_temperature_at_sea_level
    field = ESMF_FieldCreate(name="tmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
#ifdef FRONT_MOM5
#endif

  end subroutine

!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
#endif

      SUBROUTINE ATM_INITIALIZE(ATM_GRID_COMP                           &
                               ,IMP_STATE                               &
                               ,EXP_STATE                               &
                               ,CLOCK_EARTH                             &
                               ,RC_INIT)
!
!-----------------------------------------------------------------------
!***  The Initialize step of the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The ATM import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The ATM export state
      TYPE(ESMF_Clock)                  :: CLOCK_EARTH                     !<-- The Clock of the EARTH component
      INTEGER            ,INTENT(OUT)   :: RC_INIT                         !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
      TYPE(ESMF_Config) :: CF
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_INIT = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
#ifdef WITH_NUOPC
      call NUOPC_ClockPrintCurrTime(CLOCK_EARTH, &
        string="entering ATM_INITIALIZE with CLOCK_EARTH current: ")
      call NUOPC_ClockPrintStartTime(CLOCK_EARTH, &
        string="entering ATM_INITIALIZE with CLOCK_EARTH start:   ")
      call NUOPC_ClockPrintStopTime(CLOCK_EARTH, &
        string="entering ATM_INITIALIZE with CLOCK_EARTH stop:    ")
#endif

!
!-----------------------------------------------------------------------
!***  Allocate the ATM component's internal state, point at it,
!***  and attach it to the ATM component.
!-----------------------------------------------------------------------
!
      ALLOCATE(ATM_INT_STATE,stat=RC)
      wrap%ATM_INT_STATE=>ATM_INT_STATE
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set the ATM Internal State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetInternalState(ATM_GRID_COMP                  &
                                        ,WRAP                           &
                                        ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the EARTH Clock within
!***  the ATM component.
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
      call NUOPC_GridCompSetClock(ATM_GRID_COMP, CLOCK_EARTH, rc=RC_INIT)
      if (ESMF_LogFoundError(rcToCheck=RC_INIT, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      atm_int_state%CLOCK_ATM = ESMF_ClockCreate(CLOCK_EARTH, rc=RC_INIT)
      if (ESMF_LogFoundError(rcToCheck=RC_INIT, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

#else

      atm_int_state%CLOCK_ATM=CLOCK_EARTH

#endif

!
!-----------------------------------------------------------------------
!***  Create the configure object for the ATM configure file which
!***  specifies the dynamic core.
!-----------------------------------------------------------------------
!
      CF=ESMF_ConfigCreate(rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Load the ATM configure file"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigLoadFile(config=CF ,filename='atmos.configure' ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Attach the configure object to the ATM component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Attach the configure file to the ATM component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSet(gridcomp=ATM_GRID_COMP                      &  !<-- The ATM component
                           ,config  =CF                                 &  !<-- The associated configure object
                           ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Extract the dynamic core name from the configure file.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Extract dynamic core from the ATM configure file"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigGetAttribute(config=CF                            &  !<-- The ATM configure object
                                  ,value =atm_int_state%CORE            &  !<-- The dynamic core name
                                  ,label ='core:'                       &  !<-- The label in the configure file
                                  ,rc    =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Create the ATM subcomponent and its associated import/export
!***  states for the core name that was extracted.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      atm_int_state%CORE_GRID_COMP=ESMF_GridCompCreate(name=TRIM(atm_int_state%CORE)//' component' &
                                                      ,rc  =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Attach the configure object to the CORE component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     MESSAGE_CHECK="Attach the configure file to the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!     CALL ESMF_GridCompSet(gridcomp=atm_int_state%CORE_GRID_COMP       &  !<-- The ATM component
!                          ,config  =CF                                 &  !<-- The associated configure object
!                          ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Register the subcomponent's Init, Run, and Finalize subroutines.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Register the CORE component's Init, Run, and Finalize steps"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      SELECT CASE(atm_int_state%CORE)
!
        CASE('nmm')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,NMM_REGISTER                   &
                                        ,rc=RC)
!
        CASE('gfs')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,GFS_REGISTER                   &
                                        ,rc=RC)
!
        CASE('fim')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,FIM_REGISTER                   &
                                        ,rc=RC)

        CASE('gen')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,GEN_REGISTER                   &
                                        ,rc=RC)
        CASE DEFAULT
          write(0,*)' ATM_INITIALIZE requires unknown core: ',TRIM(atm_int_state%CORE)                      
!
      END SELECT
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Create the Core component's import/export states.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the CORE import state"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      atm_int_state%CORE_IMP_STATE=ESMF_StateCreate(STATENAME   = "CORE Import"            &
                                                   ,stateintent = ESMF_STATEINTENT_IMPORT  &
                                                   ,rc          = RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the CORE export state"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      atm_int_state%CORE_EXP_STATE=ESMF_StateCreate(STATENAME   = "CORE Export"            &
                                                   ,stateintent = ESMF_STATEINTENT_EXPORT  &
                                                   ,rc          = RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Nest the import/export states of the CORE component into the
!***  analgous states of the ATM component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK= "Add the CORE states into the ATMOS states"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
#ifndef WITH_NUOPC
! - Cannot bring these items out through the ATM Import and Export States 
! - under NUOPC, because NUOPC requires a minumum of Field metadata for 
! - anything that is going in/out of a component (e.g. to timestamp).
      CALL ESMF_StateAdd(IMP_STATE,LISTWRAPPER(atm_int_state%CORE_IMP_STATE),rc = RC)
      CALL ESMF_StateAdd(EXP_STATE,LISTWRAPPER(atm_int_state%CORE_EXP_STATE),rc = RC)
#endif
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Initialize the CORE component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Initialize the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompInitialize(gridcomp   =atm_int_state%CORE_GRID_COMP &
                                  ,importState=atm_int_state%CORE_IMP_STATE &
                                  ,exportState=atm_int_state%CORE_EXP_STATE &
                                  ,clock      =atm_int_state%CLOCK_ATM      &
                                  ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
      IF(RC_INIT==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_INITIALIZE succeeded'
      ELSE
        WRITE(0,*)' ATM_INITIALIZE failed  RC_INIT=',RC_INIT
      ENDIF
!
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
      call NUOPC_ClockPrintCurrTime(CLOCK_EARTH, &
        string="leaving  ATM_INITIALIZE with CLOCK_EARTH current: ")
      call NUOPC_ClockPrintStartTime(CLOCK_EARTH, &
        string="leaving  ATM_INITIALIZE with CLOCK_EARTH start:   ")
      call NUOPC_ClockPrintStopTime(CLOCK_EARTH, &
        string="leaving  ATM_INITIALIZE with CLOCK_EARTH stop:    ")

      call NUOPC_ClockPrintCurrTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_INITIALIZE with CLOCK_ATM current: ")
      call NUOPC_ClockPrintStartTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_INITIALIZE with CLOCK_ATM start:   ")
      call NUOPC_ClockPrintStopTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_INITIALIZE with CLOCK_ATM stop:    ")
#endif
!
!-----------------------------------------------------------------------
!

      END SUBROUTINE ATM_INITIALIZE
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
#ifndef WITH_NUOPC

      SUBROUTINE ATM_RUN(ATM_GRID_COMP                                  &
                        ,IMP_STATE                                      &
                        ,EXP_STATE                                      &
                        ,CLOCK_EARTH                                    &
                        ,RC_RUN)
!
!-----------------------------------------------------------------------
!***  The Run step of the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
! 
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The ATM import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The ATM export state
      TYPE(ESMF_Clock)                  :: CLOCK_EARTH                     !<-- The Clock of the EARTH component
      INTEGER            ,INTENT(OUT)   :: RC_RUN                          !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
      TYPE(ESMF_Time) :: CURRTIME                                       &
                        ,STARTTIME
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the EARTH Clock within
!***  the ATM component.
!-----------------------------------------------------------------------
!
      atm_int_state%CLOCK_ATM=CLOCK_EARTH
!
!-----------------------------------------------------------------------
!***  Execute the Run step of the selected dynamic core.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Run step of the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompRun(gridcomp   =atm_int_state%CORE_GRID_COMP    &
                           ,importState=atm_int_state%CORE_IMP_STATE    &
                           ,exportState=atm_int_state%CORE_EXP_STATE    &
                           ,clock      =atm_int_state%CLOCK_ATM         &
                           ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Update the ATMOS clock.
!-----------------------------------------------------------------------

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK = "Update the current time of the ATMOS clock"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ClockGet(clock      =atm_int_state%CLOCK_ATM            &
                        ,startTime  =STARTTIME                          &
                        ,runDuration=RUNDURATION                        &
                        ,rc         =RC)
!
      CURRTIME=STARTTIME+RUNDURATION
!
      CALL ESMF_ClockSet(clock   =atm_int_state%CLOCK_ATM               &
                        ,currTime=CURRTIME                              &
                        ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
      IF(RC_RUN==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_RUN succeeded'
      ELSE
        WRITE(0,*)' ATM_RUN failed  RC_RUN=',RC_RUN
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ATM_RUN
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

#else

      SUBROUTINE ATM_ADVANCE(ATM_GRID_COMP                                  &
                        ,RC_RUN)
!
!-----------------------------------------------------------------------
!***  The Run step of the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      INTEGER            ,INTENT(OUT)   :: RC_RUN                          !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
      TYPE(ESMF_Time) :: CURRTIME                                       &
                        ,STARTTIME
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION
      type(ESMF_Pointer)      :: this
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Use the internal Clock set by NUOPC layer for ATM
!-----------------------------------------------------------------------
!
      call NUOPC_ClockPrintCurrTime(atm_int_state%CLOCK_ATM, &
        string="entering ATM_ADVANCE with CLOCK_ATM current: ")
      call NUOPC_ClockPrintStartTime(atm_int_state%CLOCK_ATM, &
        string="entering ATM_ADVANCE with CLOCK_ATM start:   ")
      call NUOPC_ClockPrintStopTime(atm_int_state%CLOCK_ATM, &
        string="entering ATM_ADVANCE with CLOCK_ATM stop:    ")

!-----------------------------------------------------------------------
!***  Execute the Run step of the selected dynamic core.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Run step of the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompRun(gridcomp   =atm_int_state%CORE_GRID_COMP    &
                           ,importState=atm_int_state%CORE_IMP_STATE    &
                           ,exportState=atm_int_state%CORE_EXP_STATE    &
                           ,clock      =atm_int_state%CLOCK_ATM         &
                           ,phase      =1                               &
                           ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!
!-----------------------------------------------------------------------
!
      IF(RC_RUN==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_RUN succeeded'
      ELSE
        WRITE(0,*)' ATM_RUN failed  RC_RUN=',RC_RUN
      ENDIF
!-----------------------------------------------------------------------
!
      call NUOPC_ClockPrintCurrTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_ADVANCE with CLOCK_ATM current: ")
      call NUOPC_ClockPrintStartTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_ADVANCE with CLOCK_ATM start:   ")
      call NUOPC_ClockPrintStopTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_ADVANCE with CLOCK_ATM stop:    ")

!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ATM_ADVANCE
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
#endif

      SUBROUTINE ATM_FINALIZE(ATM_GRID_COMP                             &
                             ,IMP_STATE                                 &
                             ,EXP_STATE                                 &
                             ,CLOCK_EARTH                               &
                             ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!***  Finalize the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The ATM import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The ATM import state
      TYPE(ESMF_Clock)                  :: CLOCK_EARTH                     !<-- The Clock of the EARTH component
      INTEGER            ,INTENT(OUT)   :: RC_FINALIZE                     !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Finalize step of the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompFinalize(gridcomp   =atm_int_state%CORE_GRID_COMP &
                                ,importState=atm_int_state%CORE_IMP_STATE &
                                ,exportState=atm_int_state%CORE_EXP_STATE &
                                ,clock      =atm_int_state%CLOCK_ATM      &
                                ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_FINALIZE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
!-----------------------------------------------------------------------
!
      call ESMF_ClockDestroy(atm_int_state%CLOCK_ATM, rc=RC_FINALIZE)
      if (ESMF_LogFoundError(rcToCheck=RC_FINALIZE, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#endif

      IF(RC_FINALIZE==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_FINALIZE succeeded'
      ELSE
        WRITE(0,*)' ATM_FINALIZE failed  RC_FINALIZE=',RC_FINALIZE
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ATM_FINALIZE
!
!-----------------------------------------------------------------------
!
  subroutine ATM_RealizeImportFields(importState, gridIn, rc)

    type(ESMF_State), intent(inout)             :: importState
    type(ESMF_Grid), intent(in)                 :: gridIn
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS
    
    ! importable field: surface_Agrid_eastward_velocity
    if(.not. NUOPC_FieldDictionaryHasEntry('US', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='US', &
        canonicalUnits='m s-1 ', &
        defaultLongName='surface_Agrid_eastward_velocity', &
        defaultShortName='US', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="US", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='US', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_Agrid_northward_velocity
    if(.not. NUOPC_FieldDictionaryHasEntry('VS', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='VS', &
        canonicalUnits='m s-1 ', &
        defaultLongName='surface_Agrid_northward_velocity', &
        defaultShortName='VS', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="VS", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='VS', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_Bgrid_X_velocity
    if(.not. NUOPC_FieldDictionaryHasEntry('USB', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='USB', &
        canonicalUnits='m s-1 ', &
        defaultLongName='surface_Bgrid_X_velocity', &
        defaultShortName='USB', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="USB", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='USB', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_Bgrid_Y_velocity
    if(.not. NUOPC_FieldDictionaryHasEntry('VSB', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='VSB', &
        canonicalUnits='m s-1 ', &
        defaultLongName='surface_Bgrid_Y_velocity', &
        defaultShortName='VSB', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="VSB", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='VSB', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_temperature
    if(.not. NUOPC_FieldDictionaryHasEntry('TS', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='TS', &
        canonicalUnits='K', &
        defaultLongName='surface_temperature', &
        defaultShortName='TS', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="TS", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='TS', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_salinity
    if(.not. NUOPC_FieldDictionaryHasEntry('SS', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='SS', &
        canonicalUnits='psu', &
        defaultLongName='surface_salinity', &
        defaultShortName='SS', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="SS", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='SS', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: Mom4_ocean_mask_at_t-points
    if(.not. NUOPC_FieldDictionaryHasEntry('MOM_3D_MASK', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='MOM_3D_MASK', &
        canonicalUnits='1', &
        defaultLongName='Mom4_ocean_mask_at_t-points', &
        defaultShortName='MOM_3D_MASK', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="MOM_3D_MASK", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='MOM_3D_MASK', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: Mom4_ocean_area_at_t-points
    if(.not. NUOPC_FieldDictionaryHasEntry('AREA', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='AREA', &
        canonicalUnits='m+2', &
        defaultLongName='Mom4_ocean_area_at_t-points', &
        defaultShortName='AREA', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="AREA", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='AREA', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: sea_level_height
    if(.not. NUOPC_FieldDictionaryHasEntry('SSH', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='SSH', &
        canonicalUnits='m', &
        defaultLongName='sea_level_height', &
        defaultShortName='SSH', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="SSH", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='SSH', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: layer_thickness
    if(.not. NUOPC_FieldDictionaryHasEntry('DH', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='DH', &
        canonicalUnits='m', &
        defaultLongName='layer_thickness', &
        defaultShortName='DH', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="DH", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='DH', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: density
    if(.not. NUOPC_FieldDictionaryHasEntry('RHO', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='RHO', &
        canonicalUnits='kg m-3', &
        defaultLongName='density', &
        defaultShortName='RHO', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="RHO", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='RHO', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: mass_per_unit_area
    if(.not. NUOPC_FieldDictionaryHasEntry('MASSCELLO', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='MASSCELLO', &
        canonicalUnits='kg m-2', &
        defaultLongName='mass_per_unit_area', &
        defaultShortName='MASSCELLO', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="MASSCELLO", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='MASSCELLO', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: heat_content
    if(.not. NUOPC_FieldDictionaryHasEntry('HC', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='HC', &
        canonicalUnits='J m-2', &
        defaultLongName='heat_content', &
        defaultShortName='HC', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="HC", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='HC', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: eastward_current
    if(.not. NUOPC_FieldDictionaryHasEntry('U', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='U', &
        canonicalUnits='m s-1', &
        defaultLongName='eastward_current', &
        defaultShortName='U', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="U", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='U', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: northward_current
    if(.not. NUOPC_FieldDictionaryHasEntry('V', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='V', &
        canonicalUnits='m s-1', &
        defaultLongName='northward_current', &
        defaultShortName='V', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="V", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='V', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: x_current
    if(.not. NUOPC_FieldDictionaryHasEntry('UX', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='UX', &
        canonicalUnits='m s-1', &
        defaultLongName='x_current', &
        defaultShortName='UX', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="UX", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='UX', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: y_current
    if(.not. NUOPC_FieldDictionaryHasEntry('VX', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='VX', &
        canonicalUnits='m s-1', &
        defaultLongName='y_current', &
        defaultShortName='VX', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="VX", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='VX', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: potential_temperature
    if(.not. NUOPC_FieldDictionaryHasEntry('T', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='T', &
        canonicalUnits='K', &
        defaultLongName='potential_temperature', &
        defaultShortName='T', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="T", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='T', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: conservative_temperature
    if(.not. NUOPC_FieldDictionaryHasEntry('TCON', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='TCON', &
        canonicalUnits='K', &
        defaultLongName='conservative_temperature', &
        defaultShortName='TCON', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="TCON", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='TCON', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: salinity
    if(.not. NUOPC_FieldDictionaryHasEntry('S', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='S', &
        canonicalUnits='psu', &
        defaultLongName='salinity', &
        defaultShortName='S', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="S", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='S', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: upward_mass_transport
    if(.not. NUOPC_FieldDictionaryHasEntry('WMO', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='WMO', &
        canonicalUnits='tonne s-1', &
        defaultLongName='upward_mass_transport', &
        defaultShortName='WMO', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="WMO", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='WMO', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: upward_mass_transport_squared
    if(.not. NUOPC_FieldDictionaryHasEntry('WMOSQ', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='WMOSQ', &
        canonicalUnits='tonne2 s-2', &
        defaultLongName='upward_mass_transport_squared', &
        defaultShortName='WMOSQ', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="WMOSQ", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='WMOSQ', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_temperature_squared
    if(.not. NUOPC_FieldDictionaryHasEntry('TOSSQ', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='TOSSQ', &
        canonicalUnits='K2', &
        defaultLongName='surface_temperature_squared', &
        defaultShortName='TOSSQ', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="TOSSQ", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='TOSSQ', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: pressure_at_sea_floor
    if(.not. NUOPC_FieldDictionaryHasEntry('PBO', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PBO', &
        canonicalUnits='dbar', &
        defaultLongName='pressure_at_sea_floor', &
        defaultShortName='PBO', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PBO", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='PBO', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: maximum_mixed_layer_thickness
    if(.not. NUOPC_FieldDictionaryHasEntry('OMLDAMAX', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='OMLDAMAX', &
        canonicalUnits='m', &
        defaultLongName='maximum_mixed_layer_thickness', &
        defaultShortName='OMLDAMAX', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="OMLDAMAX", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='OMLDAMAX', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: layer_depth
    if(.not. NUOPC_FieldDictionaryHasEntry('DEPTH', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='DEPTH', &
        canonicalUnits='m', &
        defaultLongName='layer_depth', &
        defaultShortName='DEPTH', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="DEPTH", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='DEPTH', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: mixed_layer_depth
    if(.not. NUOPC_FieldDictionaryHasEntry('MLD', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='MLD', &
        canonicalUnits='m', &
        defaultLongName='mixed_layer_depth', &
        defaultShortName='MLD', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="MLD", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='MLD', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: barotropic streamfunction
    if(.not. NUOPC_FieldDictionaryHasEntry('PSI', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PSI', &
        canonicalUnits='kg s-1', &
        defaultLongName='barotropic streamfunction', &
        defaultShortName='PSI', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PSI", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='PSI', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: river_discharge_at_ocean_points
    if(.not. NUOPC_FieldDictionaryHasEntry('DISCHARGE', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='DISCHARGE', &
        canonicalUnits='kg m-2 s-1', &
        defaultLongName='river_discharge_at_ocean_points', &
        defaultShortName='DISCHARGE', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="DISCHARGE", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='DISCHARGE', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine ATM_RealizeImportFields

  subroutine ATM_RealizeExportFields(exportState, gridOut, rc)

    type(ESMF_State), intent(inout)             :: exportState
    type(ESMF_Grid), intent(in)                 :: gridOut
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS

    ! exportable field: Agrid_eastward_stress_on_skin
    if(.not. NUOPC_FieldDictionaryHasEntry('TAUX', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='TAUX', &
        canonicalUnits='N m-2', &
        defaultLongName='Agrid_eastward_stress_on_skin', &
        defaultShortName='TAUX', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="TAUX", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='TAUX', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: Agrid_northward_stress_on_skin
    if(.not. NUOPC_FieldDictionaryHasEntry('TAUY', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='TAUY', &
        canonicalUnits='N m-2', &
        defaultLongName='Agrid_northward_stress_on_skin', &
        defaultShortName='TAUY', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="TAUY", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='TAUY', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: Surface Atmospheric Pressure
    if(.not. NUOPC_FieldDictionaryHasEntry('PS', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PS', &
        canonicalUnits='Pa', &
        defaultLongName='Surface Atmospheric Pressure', &
        defaultShortName='PS', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PS", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='PS', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: pressure due to ice weight
    if(.not. NUOPC_FieldDictionaryHasEntry('PICE', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PICE', &
        canonicalUnits='Pa', &
        defaultLongName='pressure due to ice weight', &
        defaultShortName='PICE', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PICE", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='PICE', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: solar_heating_rate
    if(.not. NUOPC_FieldDictionaryHasEntry('SWHEAT', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='SWHEAT', &
        canonicalUnits='W m-2', &
        defaultLongName='solar_heating_rate', &
        defaultShortName='SWHEAT', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="SWHEAT", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='SWHEAT', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: freshwater_flux_from_skin_to_ocean
    if(.not. NUOPC_FieldDictionaryHasEntry('QFLX', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='QFLX', &
        canonicalUnits='kg m-2 s-1', &
        defaultLongName='freshwater_flux_from_skin_to_ocean', &
        defaultShortName='QFLX', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="QFLX", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='QFLX', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: turbulent_heat_flux_from_skin_to_ocean
    if(.not. NUOPC_FieldDictionaryHasEntry('HFLX', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='HFLX', &
        canonicalUnits='W m-2', &
        defaultLongName='turbulent_heat_flux_from_skin_to_ocean', &
        defaultShortName='HFLX', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="HFLX", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='HFLX', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: salt_flux_from_skin_to_ocean
    if(.not. NUOPC_FieldDictionaryHasEntry('SFLX', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='SFLX', &
        canonicalUnits='kg m-2 s-1', &
        defaultLongName='salt_flux_from_skin_to_ocean', &
        defaultShortName='SFLX', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="SFLX", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='SFLX', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: net_downward_penetrating_direct_UV_flux
    if(.not. NUOPC_FieldDictionaryHasEntry('PENUVR', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PENUVR', &
        canonicalUnits='W m-2', &
        defaultLongName='net_downward_penetrating_direct_UV_flux', &
        defaultShortName='PENUVR', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PENUVR", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='PENUVR', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: net_downward_penetrating_direct_PAR_flux
    if(.not. NUOPC_FieldDictionaryHasEntry('PENPAR', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PENPAR', &
        canonicalUnits='W m-2', &
        defaultLongName='net_downward_penetrating_direct_PAR_flux', &
        defaultShortName='PENPAR', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PENPAR", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='PENPAR', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: net_downward_penetrating_diffuse_UV_flux
    if(.not. NUOPC_FieldDictionaryHasEntry('PENUVF', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PENUVF', &
        canonicalUnits='W m-2', &
        defaultLongName='net_downward_penetrating_diffuse_UV_flux', &
        defaultShortName='PENUVF', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PENUVF", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='PENUVF', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: net_downward_penetrating_diffuse_PAR_flux
    if(.not. NUOPC_FieldDictionaryHasEntry('PENPAF', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='PENPAF', &
        canonicalUnits='W m-2', &
        defaultLongName='net_downward_penetrating_diffuse_PAR_flux', &
        defaultShortName='PENPAF', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="PENPAF", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='PENPAF', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: tracer_mixing_ratios
    if(.not. NUOPC_FieldDictionaryHasEntry('TR', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='TR', &
        canonicalUnits='1', &
        defaultLongName='tracer_mixing_ratios', &
        defaultShortName='TR', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="TR", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='TR', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: river_discharge_at_ocean_points
    if(.not. NUOPC_FieldDictionaryHasEntry('DISCHARGE', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='DISCHARGE', &
        canonicalUnits='kg m-2 s-1', &
        defaultLongName='river_discharge_at_ocean_points', &
        defaultShortName='DISCHARGE', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    field = ESMF_FieldCreate(name="DISCHARGE", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='DISCHARGE', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine ATM_RealizeExportFields


      END MODULE module_ATM_GRID_COMP
!
!-----------------------------------------------------------------------
