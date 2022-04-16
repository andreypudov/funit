!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Unit) UnitContext

    implicit none

    type(UnitContext), pointer :: instance

contains
    module subroutine init_context()
        type(ConsoleLogger), pointer :: console
        type(JSONLogger),    pointer :: json

        ! type(ArgumentsParser) parser
        character(len=128)    filename

        if (.not. associated(instance)) then
            allocate(instance)
            ! call parser%parse()

            ! select case(parser%getLoggerType())
            ! case (LOGGER_JSON)
            !    filename = parser%getLoggerFile()
            !    allocate(json)

            !    call json%init()
            !    instance%logger => json
            !case default
                allocate(console)

                call console%init()
                instance%logger => console
            ! end select
        end if
    end subroutine

    module subroutine clean_context()
        if (.not. associated(instance)) then
            ! deallocate(instance%logger)
            deallocate(instance)
        end if
    end subroutine

    module function getLogger_context() result(value)
        class(UnitLogger),  pointer :: value

        call init_context()
        value => instance%logger
    end function

    module function getRunner_context() result(value)
        class(UnitRunner), pointer :: value

        call init_context()
        value => instance%runner
    end function

    module function getSuite_context() result(value)
        class(UnitSuite), pointer :: value

        call init_context()
        value => instance%suite
    end function

    module function getCase_context() result(value)
        class(UnitCaseEntry), pointer :: value

        call init_context()
        value => instance%case
    end function

    module subroutine setRunner_context(runner)
        class(UnitRunner), pointer, intent(in) :: runner

        call init_context()
        instance%runner => runner
    end subroutine

    module subroutine setSuite_context(suite)
        class(UnitSuite), pointer, intent(in) :: suite

        call init_context()
        instance%suite => suite
    end subroutine

    module subroutine setCase_context(case)
        class(UnitCaseEntry), pointer, intent(in) :: case

        call init_context()
        instance%case => case
    end subroutine
end submodule
