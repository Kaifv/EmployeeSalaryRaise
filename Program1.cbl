       identification division.
       program-id. T2-01-P2.
       author. Kaifkhan Vakil.
       date-written. 2021-04-13.
      *Program Description:
      *THis program will read from a input file and will do the salary 
      *calculation for the teacher staff. 
       environment division.
       input-output section.
       file-control.
      *Input file position
           select teacher-file
               assign to "../../../../T2-01-P2.dat"
               organization is line sequential.
      *Output file position
           select print-file
               assign to "../../../../T2-01-P2.out"
               organization is line sequential.
      *
       data division.
       file section.
      *File division 
       fd teacher-file
           data record is teacher-rec
           record contains 26 characters.
      *Defnining input file
       01 teacher-rec.
         05 in-name                    pic x(20).
         05 in-salary                  pic 9(5).
         05 in-performance             pic x.
      *File division
       fd print-file
           data record is print-line
           record contains 132 characters.

       01 print-line                   pic x(132).

      *Working storage section
       working-storage section.


      *Constant for end of file
       01 ws-eof-flag                  pic x       value "n".

      *Detail line output section
       01 ws-detail-output.
         05 filler                     pic x(5).
         05 ws-prt-name                pic x(20).
         05 filler                     pic x(5).
         05 ws-rt-old-salary           pic zz,zz9.
         05 filler                     pic x(5).
         05 ws-prt-calc-raise          pic zz,zz9.
         05 filler                     pic x(5).
         05 ws-prt-actual-raise        pic zz,zz9.
         05 filler                     pic x(6).
         05 ws-prt-new-salary          pic zz,zz9.
         05 filler                     pic x(4).
         05 ws-prt-comment             pic x(30).
         05 filler                     pic x(28).

      *Heading section
       01 ws-heading1.
         05 filler                     pic x(8)    value '    NAME'.
         05 filler                     pic x(23)   value spaces.
         05 filler                     pic x(3)    value 'OLD'.
         05 filler                     pic x(5)    value spaces.
         05 filler                     pic x(10)   value 'CALCULATED'.
         05 filler                     pic x(4)    value spaces.
         05 filler                     pic x(6)    value 'ACTUAL'.
         05 filler                     pic x(5)    value spaces.
         05 filler                     pic x(3)    value 'NEW'.

      *Heading section 2
       01 ws-heading2.
         05 filler                     pic x(30)   value spaces.
         05 filler                     pic x(6)    value 'SALARY'.
         05 filler                     pic x(5)    value spaces.
         05 filler                     pic x(6)    value 'RAISE'.
         05 filler                     pic x(6)    value spaces.
         05 filler                     pic x(6)    value 'RAISE'.
         05 filler                     pic x(4)    value spaces.
         05 filler                     pic x(6)    value 'SALARY'.


      *Total line section.
       01 ws-total-line.
         05 filler                     pic x(18)   value spaces.
         05 filler                     pic x(30)   value 
         "         TOTAL RAISE AMOUNT = ".
      *               ----+----1----+----2----+----3
         05 ws-tl-total-raise          pic $$,$$$,$$9.
         05 filler                     pic x(84)   value spaces.

      *Total adjusted line section.
       01 ws-total-adj-line.
         05 filler                     pic x(18)   value spaces.
         05 filler                     pic x(30)   value 
         "TOTAL ADJUSTED RAISE AMOUNT = ".
      *               ----+----1----+----2----+----3
         05 ws-tl-total-adj-raise      pic $$,$$$,$$9.
         05 filler                     pic x(84)   value spaces.


      *Constants to be used in the program. 
       77 ws-calc-raise                pic 9(7)    value 0.
       77 ws-interim                   pic 9(7)    value 0.
       77 ws-act-raise                 pic 9(7)    value 0.
       77 ws-new-sal                   pic 9(7)    value 0.
       77 ws-3-percent-cnst            pic 9(3)v999
                                                   value 0.035.
       77 ws-1-percent-cnst            pic 9(4)v999        
                                                   value 0.016.
       77 ws-3-half-percent-cnst       pic 9(4)v999
                                                   value 0.032.
       77 ws-70k-cnst                  pic 9(8)    value 70000.
       77 ws-actual-raise-calc         pic 9(8)    value 0.
       77 ws-new-salary-calc           pic 9(8)    value 0.
       77 ws-comment-cnst              pic x(15)   value 
       "SALARY ADJUSTED".
       77 ws-total-calc-sal-counter    pic 9(7)    value 0.
       77 ws-total-act-sal-counter     pic 9(7)    value 0.
      *
       procedure division.
       000-main.
      *Open the files
           open input teacher-file,
             output print-file.
      *Printing headings
           write print-line            from ws-heading1
             after advancing 1 line.
           write print-line            from ws-heading2
             after advancing 1 line.
      *Reading from the file
           read teacher-file
               at end
                   move "y"            to ws-eof-flag.
      *Perform loop to process lines of input file
           perform 100-process-logic
             until ws-eof-flag = "y"

      *Writing total line at the end of the report
           move ws-total-calc-sal-counter
                                       to ws-tl-total-raise.
           move ws-total-act-sal-counter
                                       to ws-tl-total-adj-raise.

           write print-line            from ws-total-line
             after advancing 2 lines.
           write print-line            from ws-total-adj-line
             after advancing 2 lines.

      *Closing the files. 
           close teacher-file, print-file.
      *
           stop run.
      *
       100-process-logic.
      *  main logic paragraph - add main loop logic here
      *
           move spaces                 to ws-prt-comment.
           compute ws-calc-raise  = in-salary * ws-3-percent-cnst.
          

           if(in-performance is equal  'A')
                   move ws-calc-raise  to ws-prt-calc-raise
           end-if.
           if (in-performance is equal 'G')
               compute ws-interim = in-salary + ws-calc-raise
               perform 300-good
           end-if.
           if (in-performance is equal 'S')
               compute ws-interim = in-salary + ws-calc-raise
               perform 400-superior
           end-if.
          compute ws-new-salary-calc = ws-calc-raise + in-salary.
           if (ws-new-salary-calc <= ws-70k-cnst)
               move ws-calc-raise      to ws-act-raise
           else
               compute ws-actual-raise-calc = ws-new-salary-calc - 
               ws-70k-cnst 
              
               compute ws-actual-raise-calc = ws-calc-raise - 
               ws-actual-raise-calc
               move ws-actual-raise-calc
                                       to ws-act-raise
               move ws-comment-cnst    to ws-prt-comment
           end-if.

           

           compute ws-new-sal = in-salary + ws-act-raise.
           move ws-act-raise           to ws-prt-actual-raise.
           move ws-new-sal             to ws-prt-new-salary.
           move in-name                to ws-prt-name.
           move in-salary              to ws-rt-old-salary.


           add ws-calc-raise           to ws-total-calc-sal-counter.
           add ws-act-raise            to ws-total-act-sal-counter.
      *
           write print-line            from ws-detail-output
             after advancing 2 lines.
      *
           read teacher-file
               at end
                   move "y" to ws-eof-flag.

      ******************************************************************

       300-good.
      * as indicated in the test write up you will need paragraphs
      * for performance = g and s. you are welcome to rename
      * these if you choose.
           compute ws-interim  = ws-interim * 
           ws-1-percent-cnst.
           compute ws-calc-raise = ws-interim + ws-calc-raise.
           move ws-calc-raise          to ws-prt-calc-raise.
      *
      ******************************************************************
      *
       400-superior.
           compute ws-interim  =ws-interim *
             ws-3-half-percent-cnst.
           compute ws-calc-raise = ws-interim + ws-calc-raise.
           move ws-calc-raise          to ws-prt-calc-raise.
      *
      ******************************************************************
       end program T2-01-P2.