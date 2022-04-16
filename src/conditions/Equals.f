!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Conditions) Equals

    implicit none

contains
    module pure function equals_character(expected, actual) result(value)
        character, intent(in) :: expected
        character, intent(in) :: actual

        logical value

        value = (expected == actual)
    end function

    module pure function equals_complex(expected, actual, delta) result(value)
        complex, intent(in) :: expected
        complex, intent(in) :: actual
        real,    intent(in) :: delta

        logical value

        value = ((abs(real(expected) - real(actual)) < delta) .and. &
                (abs(aimag(expected) - aimag(actual)) < delta))
    end function

    module pure function equals_double_precision(expected, actual, delta) result(value)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: delta

        logical value

        value = (abs(expected - actual) < delta)
    end function

    module pure function equals_integer(expected, actual) result(value)
        integer, intent(in) :: expected
        integer, intent(in) :: actual

        logical value

        value = (expected == actual)
    end function

    module pure function equals_logical(expected, actual) result(value)
        logical, intent(in) :: expected
        logical, intent(in) :: actual

        logical value

        value = (expected .eqv. actual)
    end function

    module pure function equals_real(expected, actual, delta) result(value)
        real, intent(in) :: expected
        real, intent(in) :: actual
        real, intent(in) :: delta

        logical value

        value = (abs(expected - actual) < delta)
    end function
end submodule
