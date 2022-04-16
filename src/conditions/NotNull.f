!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Conditions) NotNull

    implicit none

contains
    module pure function notNull_character(pointer) result(value)
        character, pointer,  intent(in) :: pointer
        logical value

        value = associated(pointer)
    end function

    module pure function notNull_complex(pointer) result(value)
        complex, pointer, intent(in)  :: pointer
        logical value

        value = associated(pointer)
    end function

    module pure function notNull_double_precision(pointer) result(value)
        double precision, pointer,  intent(in) :: pointer
        logical value

        value = associated(pointer)
    end function

    module pure function notNull_integer(pointer) result(value)
        integer, pointer,  intent(in) :: pointer
        logical value

        value = associated(pointer)
    end function

    module pure function notNull_logical(pointer) result(value)
        logical, pointer,  intent(in) :: pointer
        logical value

        value = associated(pointer)
    end function

    module pure function notNull_real(pointer) result(value)
        real, pointer,  intent(in) :: pointer
        logical value

        value = associated(pointer)
    end function
end submodule
