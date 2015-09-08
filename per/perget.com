$ if p1.eqs."" then inquire p1 "Which subroutine?"
$ lib/extract='p1'/output='p1'.for/text per:perlib
