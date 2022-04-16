!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (FUnit) ArrayEqualsExpects

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'Array equals condition'

contains
    module subroutine arrayEquals_expect_character(expected, actual, message)
        character(len=*), optional, intent(in) :: message

        character, dimension(:), intent(in) :: expected
        character, dimension(:), intent(in) :: actual

        if (.not. arrayEquals(expected, actual)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine arrayEquals_expect_complex(expected, actual, delta, message)
        character(len=*), optional, intent(in) :: message

        complex, dimension(:), intent(in) :: expected
        complex, dimension(:), intent(in) :: actual
        real,                  intent(in) :: delta

        if (.not. arrayEquals(expected, actual, delta)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine arrayEquals_expect_double_precision(expected, actual, delta, message)
        character(len=*), optional, intent(in) :: message

        double precision, dimension(:), intent(in) :: expected
        double precision, dimension(:), intent(in) :: actual
        double precision,               intent(in) :: delta

        if (.not. arrayEquals(expected, actual, delta)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine arrayEquals_expect_integer(expected, actual, message)
        character(len=*), optional, intent(in) :: message

        integer, dimension(:), intent(in) :: expected
        integer, dimension(:), intent(in) :: actual

        if (.not. arrayEquals(expected, actual)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine arrayEquals_expect_logical(expected, actual, message)
        character(len=*), optional, intent(in) :: message

        logical, dimension(:), intent(in) :: expected
        logical, dimension(:), intent(in) :: actual

        if (.not. arrayEquals(expected, actual)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine arrayEquals_expect_real(expected, actual, delta, message)
        character(len=*), optional, intent(in) :: message

        real, dimension(:), intent(in) :: expected
        real, dimension(:), intent(in) :: actual
        real,               intent(in) :: delta

        if (.not. arrayEquals(expected, actual, delta)) then
            call fail_expect(message, default)
        end if
    end subroutine
end submodule
