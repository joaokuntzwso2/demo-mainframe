       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LINE               PIC X(80).
       01  WS-ACCTID             PIC X(8) VALUE SPACES.
       01  WS-NAME               PIC X(30) VALUE SPACES.
       01  WS-SEGMENT            PIC X(10) VALUE SPACES.
       01  WS-CCY                PIC X(3)  VALUE "LKR".
       01  WS-BALANCE            PIC 9(9)V99 VALUE 0.
       01  WS-BALTXT             PIC 9(9).99.
       01  WS-JSON               PIC X(220).

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-LINE FROM SYSIN
           IF WS-LINE(1:7) = "ACCTID="
              MOVE WS-LINE(8:8) TO WS-ACCTID
           END-IF

           IF WS-ACCTID = "00000001"
              MOVE "ALICE PERERA" TO WS-NAME
              MOVE "PLATINUM"     TO WS-SEGMENT
              MOVE 000012500050   TO WS-BALANCE
           ELSE
           IF WS-ACCTID = "00000002"
              MOVE "BOB SILVA"    TO WS-NAME
              MOVE "STANDARD"     TO WS-SEGMENT
              MOVE 000000045200   TO WS-BALANCE
           ELSE
              MOVE "NOT FOUND"    TO WS-NAME
              MOVE "N/A"          TO WS-SEGMENT
              MOVE 000000000000   TO WS-BALANCE
           END-IF
           END-IF

           COMPUTE WS-BALTXT = WS-BALANCE / 100

           STRING
             'JSON:{"type":"AccountInquiry",'
             '"accountId":"' WS-ACCTID '",'
             '"name":"' WS-NAME '",'
             '"segment":"' WS-SEGMENT '",'
             '"currency":"' WS-CCY '",'
             '"balance":' WS-BALTXT '}'
             DELIMITED BY SIZE
             INTO WS-JSON
           END-STRING

           DISPLAY WS-JSON
           GOBACK.

