!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (FUnit) UnitSuite

    implicit none

contains
    module subroutine init_suite(self, name)
        class(UnitSuite), intent(in out)       :: self
        character(len=*), optional, intent(in) :: name

        self%list => null()
        self%last => null()

        if (present(name)) then
            allocate(character(len(name)) :: self%name)
            self%name = name
        else
            allocate(character(1) :: self%name)
            self%name = ''
        end if
    end subroutine

    module subroutine clean_suite(self)
        class(UnitSuite), intent(in out)  :: self
        type(UnitCaseEntry), pointer      :: entry

        print *, 'UnitSuite / CLEAN'

        do while (associated(self%list))
            entry => self%list
            self%list => self%list%next

            deallocate(entry)
        end do

        deallocate(self%name)
    end subroutine

    module subroutine add_suite(self, case, name)
        class(UnitSuite), intent(in out)         :: self
        procedure(UnitCase), pointer, intent(in) :: case
        character(len=*),   optional, intent(in) :: name

        type(UnitCaseEntry), pointer :: entry
        type(UnitCaseEntry), pointer :: previous

        allocate(entry)
        entry%next => null()
        entry%case => case

        if (present(name)) then
            allocate(character(len(name)) :: entry%name)
            entry%name = name
        else
            allocate(character(1) :: entry%name)
            entry%name = ''
        end if

        if (.not. associated(self%last)) then
            self%list => entry
        else
            previous      => self%last
            previous%next => entry
        end if

        self%last => entry
    end subroutine

    module subroutine run_suite(self, resume)
        class(UnitSuite), target, intent(in out) :: self
        logical,        optional, intent(in)     :: resume

        type(UnitCaseEntry),  pointer :: entry
        class(UnitSuite),     pointer :: suite
        class(UnitCaseEntry), pointer :: case
        class(UnitCaseEntry), pointer :: caseOld
        class(UnitLogger),    pointer :: logger

        type(UnitContext) context
        logical           resuming
        logical           processed

        suite  => self
        logger => context%getLogger()

        ! set resume option
        if (present(resume)) then
            resuming = resume
        else
            resuming = .false.
        end if

        call context%setSuite(suite)
        if (.not. resuming) then
            call logger%log(TYPE_SUITE, self%name)
        end if

        entry     => self%list
        processed =  .false.

        do while (associated(entry))
            case    => entry
            caseOld => context%getCase()

            ! find a first unit case to run by skipping already executed
            if ((resuming)) then
                if (associated(caseOld, case)) then
                    processed = .true.
                    entry => entry%next
                    cycle
                else
                    if (.not. processed) then
                        entry => entry%next
                        cycle
                    end if
                end if
            end if

            ! set case default status to success
            case%status = .true.

            call context%setCase(case)
            call entry%case(self)
            if (case%status) then
                call logger%log(type = TYPE_CASE, name = entry%name, status = case%status)
            end if

            entry => entry%next
        end do
    end subroutine
end submodule
