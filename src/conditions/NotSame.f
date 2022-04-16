!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Conditions) NotSame

    implicit none

contains
    module pure function notSame_character(unexpected, actual) result(value)
        character, pointer,  intent(in) :: unexpected
        character, pointer,  intent(in) :: actual

        logical value

        value = (.not. associated(unexpected, actual))
    end function

    module pure function notSame_complex(unexpected, actual) result(value)
        complex, pointer, intent(in)  :: unexpected
        complex, pointer, intent(in)  :: actual

        logical value

        value = (.not. associated(unexpected, actual))
    end function

    module pure function notSame_double_precision(unexpected, actual) result(value)
        double precision, pointer,  intent(in) :: unexpected
        double precision, pointer,  intent(in) :: actual

        logical value

        value = (.not. associated(unexpected, actual))
    end function

    module pure function notSame_integer(unexpected, actual) result(value)
        integer, pointer,  intent(in) :: unexpected
        integer, pointer,  intent(in) :: actual

        logical value

        value = (.not. associated(unexpected, actual))
    end function

    module pure function notSame_logical(unexpected, actual) result(value)
        logical, pointer,  intent(in) :: unexpected
        logical, pointer,  intent(in) :: actual

        logical value

        value = (.not. associated(unexpected, actual))
    end function

    module pure function notSame_real(unexpected, actual) result(value)
        real, pointer,  intent(in) :: unexpected
        real, pointer,  intent(in) :: actual

        logical value

        value = (.not. associated(unexpected, actual))
    end function
end submodule
