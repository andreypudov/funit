!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

program Example

    use ExampleRunnerUnit

    implicit none

    type(ExampleRunner) :: runner

    call runner%init()
    call runner%run()
    call runner%clean()
end program
