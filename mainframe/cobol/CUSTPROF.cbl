       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROF.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LINE        PIC X(80).
       01  WS-CUSTID      PIC X(10) VALUE SPACES.
       01  WS-NIC         PIC X(12) VALUE SPACES.
       01  WS-NAME        PIC X(30) VALUE SPACES.
       01  WS-MOBILE      PIC X(12) VALUE SPACES.
       01  WS-RISK        PIC X(10) VALUE SPACES.
       01  WS-JSON        PIC X(240).

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-LINE FROM SYSIN
           IF WS-LINE(1:7) = "CUSTID="
              MOVE WS-LINE(8:10) TO WS-CUSTID
           END-IF

           IF WS-CUSTID = "C000000001"
              MOVE "901234567V"        TO WS-NIC
              MOVE "ALICE PERERA"      TO WS-NAME
              MOVE "+94771234567"      TO WS-MOBILE
              MOVE "LOW"              TO WS-RISK
           ELSE
           IF WS-CUSTID = "C000000002"
              MOVE "881112223V"        TO WS-NIC
              MOVE "BOB SILVA"         TO WS-NAME
              MOVE "+94770001122"      TO WS-MOBILE
              MOVE "MEDIUM"           TO WS-RISK
           ELSE
              MOVE "UNKNOWN"           TO WS-NIC
              MOVE "NOT FOUND"         TO WS-NAME
              MOVE "UNKNOWN"           TO WS-MOBILE
              MOVE "UNKNOWN"           TO WS-RISK
           END-IF
           END-IF

           STRING
             'JSON:{"type":"CustomerProfile",'
             '"customerId":"' WS-CUSTID '",'
             '"nic":"' WS-NIC '",'
             '"name":"' WS-NAME '",'
             '"mobile":"' WS-MOBILE '",'
             '"risk":"' WS-RISK '"}'
             DELIMITED BY SIZE
             INTO WS-JSON
           END-STRING

           DISPLAY WS-JSON
           GOBACK.

