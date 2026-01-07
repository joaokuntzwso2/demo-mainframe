       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXNPOST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LINE        PIC X(80).
       01  WS-FROM        PIC X(8) VALUE SPACES.
       01  WS-TO          PIC X(8) VALUE SPACES.
       01  WS-AMOUNT      PIC X(20) VALUE SPACES.
       01  WS-CCY         PIC X(3)  VALUE "LKR".
       01  WS-STATUS      PIC X(12) VALUE SPACES.
       01  WS-AUTH        PIC X(10) VALUE SPACES.
       01  WS-JSON        PIC X(260).

       PROCEDURE DIVISION.
       MAIN.
           PERFORM UNTIL 1 = 2
              ACCEPT WS-LINE FROM SYSIN
              IF WS-LINE = SPACES
                 EXIT PERFORM
              END-IF

              IF WS-LINE(1:5) = "FROM="
                 MOVE WS-LINE(6:8) TO WS-FROM
              END-IF
              IF WS-LINE(1:3) = "TO="
                 MOVE WS-LINE(4:8) TO WS-TO
              END-IF
              IF WS-LINE(1:7) = "AMOUNT="
                 MOVE WS-LINE(8:20) TO WS-AMOUNT
              END-IF
              IF WS-LINE(1:4) = "CCY="
                 MOVE WS-LINE(5:3) TO WS-CCY
              END-IF
           END-PERFORM

           IF WS-FROM = "00000001" AND WS-TO = "00000002"
              MOVE "APPROVED"  TO WS-STATUS
              MOVE "AUTH000123" TO WS-AUTH
           ELSE
              MOVE "REJECTED"  TO WS-STATUS
              MOVE "AUTH000000" TO WS-AUTH
           END-IF

           STRING
             'JSON:{"type":"TransactionPost",'
             '"from":"' WS-FROM '",'
             '"to":"' WS-TO '",'
             '"amount":"' WS-AMOUNT '",'
             '"currency":"' WS-CCY '",'
             '"status":"' WS-STATUS '",'
             '"authCode":"' WS-AUTH '"}'
             DELIMITED BY SIZE
             INTO WS-JSON
           END-STRING

           DISPLAY WS-JSON
           GOBACK.

