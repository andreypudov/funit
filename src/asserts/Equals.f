!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Unit) EqualsAsserts

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'Equals condition'

contains
    module subroutine equals_assert_character(expected, actual, message)
        character(len=*), optional, intent(in) :: message

        character, intent(in) :: expected
        character, intent(in) :: actual

        if (.not. equals(expected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine equals_assert_complex(expected, actual, delta, message)
        character(len=*), optional, intent(in) :: message

        complex, intent(in) :: expected
        complex, intent(in) :: actual
        real,    intent(in) :: delta

        if (.not. equals(expected, actual, delta)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine equals_assert_double_precision(expected, actual, delta, message)
        character(len=*), optional, intent(in) :: message

        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: delta

        if (.not. equals(expected, actual, delta)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine equals_assert_integer(expected, actual, message)
        character(len=*), optional, intent(in) :: message

        integer, intent(in) :: expected
        integer, intent(in) :: actual

        if (.not. equals(expected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine equals_assert_logical(expected, actual, message)
        character(len=*), optional, intent(in) :: message

        logical, intent(in) :: expected
        logical, intent(in) :: actual

        if (.not. equals(expected, actual)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine equals_assert_real(expected, actual, delta, message)
        character(len=*), optional, intent(in) :: message

        real, intent(in) :: expected
        real, intent(in) :: actual
        real, intent(in) :: delta

        if (.not. equals(expected, actual, delta)) then
            call fail_assert(message, default)
        end if
    end subroutine
end submodule
