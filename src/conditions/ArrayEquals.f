!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Conditions) ArrayEquals

    implicit none

contains
    module pure function arrayEquals_character(expected, actual) result(value)
        character, dimension(:), intent(in) :: expected
        character, dimension(:), intent(in) :: actual

        logical value

        if (size(expected) /= size(actual)) then
            value = .false.
            return
        end if

        value = all(expected == actual)
    end function

    module pure function arrayEquals_complex(expected, actual, delta) result(value)
        complex, dimension(:), intent(in) :: expected
        complex, dimension(:), intent(in) :: actual
        real,                  intent(in) :: delta

        logical value

        if (size(expected) /= size(actual)) then
            value = .false.
            return
        end if

        value = (all(abs(real(expected) - real(actual)) < delta) .and. &
                all(abs(aimag(expected) - aimag(actual)) < delta))
    end function

    module pure function arrayEquals_double_precision(expected, actual, delta) result(value)
        double precision, dimension(:), intent(in) :: expected
        double precision, dimension(:), intent(in) :: actual
        double precision,               intent(in) :: delta

        logical value

        if (size(expected) /= size(actual)) then
            value = .false.
            return
        end if

        value = all(abs(expected - actual) < delta)
    end function

    module pure function arrayEquals_integer(expected, actual) result(value)
        integer, dimension(:), intent(in) :: expected
        integer, dimension(:), intent(in) :: actual

        logical value

        if (size(expected) /= size(actual)) then
            value = .false.
            return
        end if

        value = all(expected == actual)
    end function

    module pure function arrayEquals_logical(expected, actual) result(value)
        logical, dimension(:), intent(in) :: expected
        logical, dimension(:), intent(in) :: actual

        logical value

        if (size(expected) /= size(actual)) then
            value = .false.
            return
        end if

        value = all(expected .eqv. actual)
    end function

    module pure function arrayEquals_real(expected, actual, delta) result(value)
        real, dimension(:), intent(in) :: expected
        real, dimension(:), intent(in) :: actual
        real,               intent(in) :: delta

        logical value

        if (size(expected) /= size(actual)) then
            value = .false.
            return
        end if

        value = all(abs(expected - actual) < delta)
    end function
end submodule
