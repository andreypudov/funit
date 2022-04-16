!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (FUnit) SameAsserts

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'Same condition'

contains
    module subroutine same_assert_character(unexpected, actual, message)
        character(len=*), optional, intent(in) :: message
        character,        pointer,  intent(in) :: unexpected
        character,        pointer,  intent(in) :: actual

        if (.not. same(unexpected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine same_assert_complex(unexpected, actual, message)
        character(len=*), optional, intent(in) :: message
        complex,          pointer, intent(in)  :: unexpected
        complex,          pointer, intent(in)  :: actual

        if (.not. same(unexpected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine same_assert_double_precision(unexpected, actual, message)
        character(len=*), optional, intent(in) :: message
        double precision, pointer,  intent(in) :: unexpected
        double precision, pointer,  intent(in) :: actual

        if (.not. same(unexpected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine same_assert_integer(unexpected, actual, message)
        character(len=*), optional, intent(in) :: message
        integer,          pointer,  intent(in) :: unexpected
        integer,          pointer,  intent(in) :: actual

        if (.not. same(unexpected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine same_assert_logical(unexpected, actual, message)
        character(len=*), optional, intent(in) :: message
        logical,          pointer,  intent(in) :: unexpected
        logical,          pointer,  intent(in) :: actual

        if (.not. same(unexpected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine same_assert_real(unexpected, actual, message)
        character(len=*), optional, intent(in) :: message
        real,             pointer,  intent(in) :: unexpected
        real,             pointer,  intent(in) :: actual

        if (.not. same(unexpected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine
end submodule
