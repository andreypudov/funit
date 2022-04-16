!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Unit) UnitRunner

    use ifport

    implicit none

contains
    module subroutine init_runner(self, name)
        class(UnitRunner), intent(in out)      :: self
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

        ! initialize assertion handler
        call setHandler()
    end subroutine

    module subroutine clean_runner(self)
        class(UnitRunner), intent(in out) :: self
        type(UnitSuiteEntry), pointer     :: entry

        type(UnitContext) context

        do while (associated(self%list))
            entry => self%list
            self%list => self%list%next

            call entry%suite%clean()

            deallocate(entry)
        end do

        deallocate(self%name)
        call context%clean()
    end subroutine

    module subroutine add_runner(self, suite)
        class(UnitRunner), intent(in out)     :: self
        class(UnitSuite), pointer, intent(in) :: suite

        type(UnitSuiteEntry), pointer :: entry
        type(UnitSuiteEntry), pointer :: previous

        allocate(entry)
        entry%next  => null()
        entry%suite => suite

        call entry%suite%init()

        if (.not. associated(self%last)) then
            self%list => entry
        else
            previous      => self%last
            previous%next => entry
        end if

        self%last => entry
    end subroutine

    module subroutine run_runner(self, resume)
        class(UnitRunner), target, intent(in) :: self
        logical,         optional, intent(in) :: resume

        type(UnitSuiteEntry), pointer :: entry
        class(UnitRunner),    pointer :: runner
        class(UnitSuite),     pointer :: suite
        class(UnitLogger),    pointer :: logger

        type(UnitContext) context
        logical           resuming

        runner => self
        logger => context%getLogger()

        ! set resume option
        if (present(resume)) then
            resuming = resume
        else
            call context%setRunner(runner)
            call logger%log(TYPE_RUNNER, self%name)

            resuming = .false.
        end if

        entry => self%list

        do while (associated(entry))
            suite => context%getSuite()
            if ((resuming) .and. (.not. associated(suite, entry%suite))) then
                entry => entry%next
                cycle
            end if

            call entry%suite%run(resuming)
            resuming = .false.

            entry => entry%next
        end do

        call logger%log(TYPE_RESULT, '')
    end subroutine

    subroutine setHandler()
        use, intrinsic :: IEEE_EXCEPTIONS

        interface
            subroutine handler(signo, siginfo)
                integer, intent(in) :: signo
                integer, intent(in) :: siginfo
            end subroutine
        end interface

        integer result
        result = 0

        if (.not. ieee_support_flag(IEEE_DIVIDE_BY_ZERO)) then
            error stop 'Could not set assertion handler.'
        end if

        result = ieee_handler('set', 'division', resume)
        if (result /= 0) then
            error stop 'Could not set assertion handler.'
        end if
    end subroutine

    subroutine resume(signo, siginfo)
        integer, intent(in) :: signo
        integer, intent(in) :: siginfo

        class(UnitRunner), pointer :: runner
        type(UnitContext) context

        ! initialize assertion handler
        call setHandler()

        ! resume unit running
        runner => context%getRunner()
        call runner%run(resume = .true.)
    end subroutine
end submodule
