program strread
use lexer

implicit none

character(len=1024) :: filename
type(textlines) :: lines
type(wordlines) :: words
integer :: i, j

real :: f
write(*,*) 'enter a filename'
read(*,*)filename

call readfile(filename,lines)
call tokenize_lines(lines,words)
write(*,*)size(lines%lines)

do i = 1, size(lines%lines)

   write(*,*)i,len(lines%lines(i)%text),' ',('"',lines%lines(i)%text(j:j),'" ',j=1,len(lines%lines(i)%text))
end do

do i = 1, size(words%lines)
do j = 1, size(words%lines(i)%words)
   write(*,*)i,j,'"',words%lines(i)%words(j)%text(:),'"',len(words%lines(i)%words(j)%text),words%lines(i)%words(j)%numchars
end do
end do

!call atoi(words%lines(3)%words(1)%text,i)
!call atof(words%lines(3)%words(2)%text,f)
!write(*,*)i,f
!call atoi(words%lines(3)%words(1)%text,i)
!call atof(words%lines(3)%words(1)%text,f)
!write(*,*)i,f




end program strread
