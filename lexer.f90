Module lexer
  Use ISO_FORTRAN_ENV

  Type charline

     Integer :: numchars
     Character(1), Allocatable, Dimension(:) :: text

  End Type charline

  Type string

     Integer :: numchars
     Character(len=:), Allocatable :: text

  End Type string

  Type textlines
     
     Integer :: numlines
     Type(string), Dimension(:), Allocatable :: lines
     
  End Type textlines

  Type charlines 

     Integer :: numlines
     Type(charline), Dimension(:), Allocatable :: lines

  End Type charlines

  Type wordline
     
     Integer :: numwords
     Type(string), Dimension(:), Allocatable :: words

  End Type wordline

  Type wordlines

     Type(wordline), Dimension(:), Allocatable :: lines
     Integer :: numlines

  End Type wordlines

! So I would have liked these to have been functions, but fortran doesn't
! understand that a function return value can, in principal, be of the same
! type but different kind value and still be unambiguous. Subroutines don't suffer 
! from this so subroutines these procedures shall be.
  
  Interface atof
     Module Procedure str2real, str2double
  End Interface

  Interface atoi
     Module Procedure str2i8, str2i16, str2i32, str2i64
  End Interface

  Private

  Public :: atof, atoi
  Public :: findStr
  Public :: readfile, tokenize_lines
  Public :: wordlines, wordline, string, textlines

Contains

!  Function getValuesForKey(tokenlines,key,iIgnoreString)
!    ! Searches tokenlines for occurences of key and returns the values proceeding it
!    ! Multiple occurences of key are packed into one
!    ! iIgnorestring is a set of words which are stripped from the key and values 
!    ! before the checking if it is a key, and from the resulting matche values, resp.
!    Implicit none
!    Type(wordline) :: getValuesForKey
!    Type(wordlines) :: tokenlines
!    Character(Len=*) :: key
!    Type(wordline), Optional :: iIgnoreString
!    Character(Len=len(key)) :: subbedkey
!    Integer :: i, j
!
!    Type(wordlines) :: ignoreString
!
!    If(Present(iIgnoreString)) Then
!       Allocate(ignoreString%words(iIgnoreString%numwords))
!       ignoreString%numwords = iIgnoreString%numwords
!       Do i = 1, ignoreString%numwords
!          Allocate(Character(Len=iIgnoreString%words(i)%numchars) :: ignoreString%words(i)%text)
!          ignoreString%words(i)%text = iIgnoreString%words(i)%text
!          ignoreString%words(i)%numchars = iIgnoreString%words(i)%numchars
!       End Do
!    Else
!       Allocate(ignoreString%words(1))
!       ignoreString%numwords = 1
!       Allocate(Character(Len=1) :: ignoreString%words(1)%text)
!       ignoreString%words(1)%numchars = 1
!       ignoreString%words(1)%text = '='
!    End If
!
!    subbedkey = key
!
!    Do i = 1, ignoreString%numwords
!       subbedkey = str_replace(subbedkey,ignoreString%words(i)%text,'')
!    End Do
!
!
!    Do i = 1, tokenlines%numlines
!       Do j = 1, tokenlines%lines(i)%numwords
!       End Do
!    End Do
!
!  End Function getValuesForKey

  Function str_replace(inputStr,search,replace)
    ! Limitations: Single pass only. No recursion because infinity is hard
    Implicit none
    Character(*) :: inputStr, search, replace
    Character(Len=:), Allocatable :: str_replace
    Integer :: inputStrlen, searchlen, i
    Integer, Dimension(len(inputStr)) :: actionString

    ! The i'th element of actionString corresponds to either 
    ! applying the replace string to position i or not. 1 => apply
    actionString = 0
    
    ! Approach
    ! 1. Scan through string and mark where changes will be made
    ! 2. Allocate output string
    ! 3. Scan through string making changes as necessary

    inputStrlen = len(inputStr)
    searchlen = len(search)

    Do i = 1, inputStrlen
       If(i + searchlen - 1 > inputStrlen) Exit ! Gotten too far in string to have a valid search
       If(inputStr(i:i+searchlen) == search(1:searchlen)) Then
          actionString(i) = 1
       End If
    End Do
    
    ! We have a difficulty if the search and replace strings are different sizes since the resultant
    ! string is also of a different size. So figure out the ultimate size now and allocate.

    If( len(search) .eq. len(replace) ) Then
       ! No problem
       Allocate(Character(Len=inputStrlen) :: str_replace)
    Else
       
    End If

  End Function str_replace


  Function findStr(tokenlines,searchString,offset)
    ! Returns line and word with first occurence of string "searchString" 
    ! in the tokenized list of lines tokenlines
    ! Optional integer offset used if you want to find second/third/etc occurence
    ! Returns -1 if key is not found
    Implicit none
    Integer, Dimension(1:2) :: findStr
    Type(wordlines) :: tokenlines
    Character(*) :: searchString
    Integer, Optional :: offset

    Integer :: lineindex, wordindex, inOffset

    If(Present(offset)) Then
       If(offset .lt. 1) Then
          Write(*,*)'Error: Offset must be greater than 1'
          Stop
       End If
       inOffset = offset
    Else
       inOffset = 1
    End If
    
    Do lineindex = 1, size(tokenlines%lines)
       Do wordindex = 1, size(tokenlines%lines(lineindex)%words)
          If( Index(tokenlines%lines(lineindex)%words(wordindex)%text, searchString) .ne. 0 ) Then
             inOffset = inOffset - 1
             If(inOffset .eq. 0) Then
                findStr(1) = lineindex
                findStr(2) = wordindex
                Return
             End If
          End If
       End Do
    End Do
    
    findStr = -1

  End Function findStr

  Subroutine str2real(str,out)
    ! Explain once, as the rest are almost identical
    ! The string str is internally read into a variable of specified type and kind
    ! With an error catching in case of an unexpected conversion error.
    Implicit none
    Character(*) :: str
    Real( Kind = REAL32 ) :: out
    Integer :: ios
    Read(str,*, iostat = ios) out
    If(ios .ne. 0) Then
       Write(*,*) 'Error: Could not convert value "',str,'" to a floating point number.'
       Stop
    End If    
  End Subroutine str2real

  Subroutine str2double(str,out)
    Implicit none
    Character(*) :: str
    Real( Kind = REAL64 ) :: out
    Integer :: ios
    Read(str,*, iostat = ios) out
    If(ios .ne. 0) Then
       Write(*,*) 'Error: Could not convert value "',str,'" to a floating point number.'
       Stop
    End If    
  End Subroutine str2double

  Subroutine str2i8(str,out)
    Implicit none
    Character(*) :: str
    Integer( Kind = INT8 ) :: out
    Integer :: ios
    Real( Kind = REAL64 ) :: hope
    Read(str,*, iostat = ios) out
    If(ios .ne. 0) Then
       ! Here we have a problem, if we are attempting to convert a value such as "1.23" to an integer
       ! the internal read actually fails. So the trick is to try reading the value in as a real number, 
       ! THEN cast it to an integer! This appears to not affect Intel as it implicitly converts as expected.
       Write(*,*) 'Warning: Non-integral value "',str,'" failed conversion to integer. Attempting recast...'
       Call str2double(str,hope)
       out = int(hope, INT8)
    End If
  End Subroutine str2i8

  Subroutine str2i16(str,out)
    Implicit none
    Character(*) :: str
    Integer( Kind = INT16 ) :: out
    Integer :: ios
    Real( Kind = REAL64 ) :: hope
    Read(str,*, iostat = ios) out
    If(ios .ne. 0) Then
       Write(*,*) 'Warning: Non-integral value "',str,'" failed conversion to integer. Attempting recast...'
       Call str2double(str,hope)
       out = int(hope, INT16)
    End If
  End Subroutine str2i16

  Subroutine str2i32(str,out)
    Implicit none
    Character(*) :: str
    Integer( Kind = INT32 ) :: out
    Integer :: ios
    Real( Kind = REAL64 ) :: hope
    Read(str,*, iostat = ios) out
    If(ios .ne. 0) Then
       Write(*,*) 'Warning: Non-integral value "',str,'" failed conversion to integer. Attempting recast...'
       Call str2double(str,hope)
       out = int(hope, INT32)
    End If
  End Subroutine str2i32

  Subroutine str2i64(str,out)
    Implicit none
    Character(*) :: str
    Integer( Kind = INT64 ) :: out
    Integer :: ios
    Real( Kind = REAL64 ) :: hope
    Read(str,*, iostat = ios) out
    If(ios .ne. 0) Then
       Write(*,*) 'Warning: Non-integral value "',str,'" failed conversion to integer. Attempting to recast...'
       Call str2double(str,hope)
       out = int(hope, INT64)
    End If
  End Subroutine str2i64

  Subroutine readfile(filename,lines)
    ! Read a file in a single pass and return a list containing each line of the file
    Implicit none
    Character(*), Intent(In) :: filename
    Type(textlines), Intent(InOut) :: lines

    Logical :: EndOfFile
    Integer :: ios, UnitNum
    Character(1) :: chbuff
    Type(charline) :: linebuff

    If(allocated(lines%lines)) Then
       Deallocate(lines%lines)
    End If

    UnitNum = 10

    Open(Unit=UnitNum,file=filename,status='old',action='read',iostat=ios)
    If(ios .ne. 0) Then
       Write(ERROR_UNIT,*)'Failed to open file "',Trim(filename),'". Aborting'
       Stop
    End If
    
    EndOfFile = .False.
    Do While(EndOfFile .eqv. .False.)
       ! Non-advancingly read a character in from the file, then
       ! check to see if we have hit the end of line/file
       Read(UnitNum,'(1a)', advance = 'no', iostat = ios) chbuff
       If(ios .eq. IOSTAT_EOR) Then
          ! Check to see if we hit a blank line
          If(.not. Allocated(linebuff%text)) Then
             Call append(linebuff, ' ')
          End If
          Call append_file(lines,linebuff)
          If(Allocated(linebuff%text)) Then
             Deallocate(linebuff%text)
          End If
          linebuff%numchars = 0
          Cycle
       End If
       If(ios .eq. IOSTAT_END) Then
          ! Not needed since EOR occurs BEFORE EOF
          ! Call append_file(lines,linebuff)
          EndOfFile = .True.
       End If
       If(EndOfFile .eqv. .False.) Call append(linebuff,chbuff)
    End Do

  End Subroutine readfile

  Subroutine tokenize_lines(lines,wordlists,tokens)
    ! Iterate over the set of lines and tokenize each line into a list of words
    ! Storing each wordlist in wordlists
    Implicit none
    Type(textlines), Intent(In) :: lines
    Type(wordlines), Intent(InOut) :: wordlists
    Character(*), Optional, Intent(In) :: tokens

    Integer :: i, j
    
    If(Allocated(wordlists%lines)) Then
       Deallocate(wordlists%lines)
       wordlists%numlines = 0
    End If

    Allocate(wordlists%lines(size(lines%lines)))
    wordlists%numlines = lines%numlines

    Do i = 1, lines%numlines
       Call tokenize_line(lines%lines(i),wordlists%lines(i),tokens)
    End Do

  End Subroutine tokenize_lines
  
  Subroutine tokenize_line(line,wordlist,intokens)
    Implicit none
    Type(string), Intent(In) :: line
    Type(wordline), Intent(InOut) :: wordlist
    Character(*), Optional, Intent(In) :: intokens
    
    Character(:), Allocatable :: tokens, wordbuff
    Integer :: chptr, i, bufferOffset, textsize
    Logical :: haveWord

    ! Should be dead code, but just be safe
    If(Allocated(wordlist%words)) Then
       Deallocate(wordlist%words)
       wordlist%numwords = 0
    End If

    ! Default token is just a blank space
    If( Present( intokens ) ) Then
       tokens = intokens
    Else
       tokens = " "//achar(9)
    End If
    
    ! Allocate a temporary buffer to store the word in. 
    ! The size of the line is obviously sufficient
    
    Allocate(Character(len=line%numchars) :: wordbuff)

    Do i = 1, len(wordbuff)
       wordbuff(i:i) = " "
    End Do

    ! Steps:
    ! 0. Manually left-trim until we hit a character not in our token buffer
    ! 0. a) If we hit the end, and the list is empty return a single space as the only word
    ! 1. Start saving the text until we hit a character in our token buffer or reach the end.
    !    Once hit, save the word in the list and repeat step 0 if we are not at the end
    
    haveWord = .False.
    bufferOffset = 1
    
    Do chptr = 1, line%numchars
       If(.not.isAnyOf(line%text(chptr:chptr),tokens)) Then
          haveWord = .True.
          wordbuff(bufferOffset:bufferOffset) = line%text(chptr:chptr)
          bufferOffset = bufferOffset + 1
       Else
          If(haveWord) Then
             Call addWord(wordbuff,bufferOffset - 1,wordlist)
             Do i = 1, bufferOffset
                wordbuff(i:i) = ' '
             End Do
             bufferOffset = 1
             haveWord = .False.
          End If
       End If
    End Do

    ! Need the haveword == .False. here otherwise we accidentally blank a word
    ! that is the sole resident of a line
    If((.not.(allocated(wordlist%words))) .and. (haveWord .eqv. .False.)) Then
       If(allocated(wordbuff)) Then
          Deallocate(wordbuff)
       End If
       Allocate(Character(len=1) :: wordbuff)
       wordbuff = ' '
       Call addWord(wordbuff,1,wordlist)
       wordlist%numwords = 0
       wordlist%words(1)%numchars = 0
    Else
       ! Final call to addWord to populate the last possible word to the buffer
       If(haveWord) Then
          Call addWord(wordbuff,bufferOffset - 1,wordlist)
       End If
    End If

    Contains

      Logical Function isAnyOf(testch, chlist)
        ! Tests if the character testch matches any of 
        ! the characters in the chlist string
        Implicit none
        Character(1) :: testch
        Character(*) :: chlist
        Integer :: i
        isAnyOf = .False.
        Do i = 1, len(chlist)
           If(testch .eq. chlist(i:i)) Then
              isAnyOf = .True.
              Return
           End If
        End Do
      End Function isAnyOf

  End Subroutine tokenize_line

  Subroutine addWord(wordbuff,bufferOffset,wordlist)
    ! Store the word contained in wordbuff (with the end of it known by bufferOffset)
    ! into the list of words in wordlist
    Implicit none
    Integer, Intent(In) :: bufferOffset
    Character(*), Intent(In) :: wordbuff
    Type(wordline), Intent(InOut) :: wordlist
    Type(string), Dimension(:), Allocatable :: tmpwords
    Character(:), Allocatable :: tmptext
    Integer :: i, j
    Allocate(Character(len=bufferOffset) :: tmptext)
    tmptext(1:bufferOffset) = wordbuff(1:bufferOffset)

    ! I'll explain this once as it is used frequently in the code
    ! If the allocatable array in the output is not allocated, allocate
    ! a single element and then move the input over to it. This is done
    ! with the intrinsic function move_alloc

    If(.not.allocated(wordlist%words)) Then
       Allocate(wordlist%words(1))
       wordlist%numwords = 0 ! Incremented later
       Allocate(Character(len=bufferOffset) :: wordlist%words(1)%text)
       wordlist%words(1)%text(1:bufferOffset) = tmptext(1:bufferOffset)
    Else
       ! If the allocatable array in the output is already allocated, then
       ! allocate a temporary array, populate all but the last elements with
       ! the values from the output, then move the new input into the temp. array
       ! Finally move the temp. array into the output array, replacing the old one
       Allocate(tmpwords(1:size(wordlist%words) + 1))
       tmpwords(1:size(wordlist%words)) = wordlist%words
       ! Workaround for gfortran not allocating the length of deferred character lengths
       Allocate(Character(len=bufferOffset) :: tmpwords(size(wordlist%words)+1)%text)
       Call move_alloc(tmptext,tmpwords(size(wordlist%words) + 1)%text)
       Call move_alloc(tmpwords,wordlist%words)
    End If

    wordlist%numwords = wordlist%numwords + 1
    wordlist%words(size(wordlist%words))%numchars = bufferOffset
  End Subroutine addWord

  Subroutine append(line,newch)
    ! Append a character to a list of characters
    Implicit none
    Type(charline) :: line
    Character(1) :: newch
    Character(1), Allocatable, Dimension(:) :: tmp

    If(.not. Allocated(line%text)) Then
       Allocate(line%text(1))
       line%text(1) = newch
       line%numchars = 1
    Else
       Allocate(character(1) :: tmp(1:Size(line%text) + 1 ) )
       tmp(:Size(line%text)) = line%text
       tmp(Size(line%text) + 1) = newch
       Call move_alloc(tmp,line%text)
       line%numchars = line%numchars + 1
    End If

  End Subroutine append

  Subroutine append_file(fileset,newline)
    ! Confusingly named. This takes a list of lines and append another line to it
    Implicit None
    Type(textlines) :: fileset, tmp
    Type(charline) :: newline
    Integer :: i, strsize

    ! Workaround needed for gfortran not being able to cope with 
    ! Allocate(Character(len=size(newline%text))...)
    strsize = size(newline%text)
    
    If(.not. Allocated(fileset%lines)) Then
       Allocate(fileset%lines(1))
       Allocate(Character(len=strsize) :: fileset%lines(1)%text)
       Do i = 1, size(newline%text)
          fileset%lines(1)%text(i:i) = newline%text(i)
       End Do
       fileset%lines(1)%numchars = newline%numchars
       fileset%numlines = 1
    Else
       Allocate(tmp%lines(1:Size(fileset%lines) + 1 ) )
       tmp%lines(:Size(fileset%lines)) = fileset%lines
       Allocate(Character(len=strsize) :: tmp%lines(size(tmp%lines))%text)
       Do i = 1, size(newline%text)
          tmp%lines(size(tmp%lines))%text(i:i) = newline%text(i)
       End Do
       tmp%lines(size(tmp%lines))%numchars = newline%numchars
       Call move_alloc(tmp%lines, fileset%lines)
       fileset%numlines = fileset%numlines + 1
    End If
       
  End Subroutine append_file

End Module lexer
