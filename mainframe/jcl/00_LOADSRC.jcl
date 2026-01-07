//LOADSRC JOB (SETUP),'WSO2DEMO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//* Create PDS for COBOL and JCL and load members using IEBUPDTE
//STEP1   EXEC PGM=IEFBR14
_toggle
//COBOLPDS DD DSN=IBMUSER.COBOL,DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,5)),UNIT=SYSDA,
//            DCB=(DSORG=PO,RECFM=FB,LRECL=80,BLKSIZE=3120)
//JCLPDS   DD DSN=IBMUSER.JCL,DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,5)),UNIT=SYSDA,
//            DCB=(DSORG=PO,RECFM=FB,LRECL=80,BLKSIZE=3120)
//STEP2   EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD DSN=IBMUSER.COBOL,DISP=SHR
//SYSIN    DD *
./ ADD NAME=ACCTINQ,LIST=ALL
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
./ ADD NAME=CUSTPROF,LIST=ALL
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
./ ADD NAME=TXNPOST,LIST=ALL
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
              MOVE "APPROVED"   TO WS-STATUS
              MOVE "AUTH000123" TO WS-AUTH
           ELSE
              MOVE "REJECTED"   TO WS-STATUS
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
//SYSUT2   DD DSN=IBMUSER.JCL,DISP=SHR
//SYSIN    DD *
./ ADD NAME=ACCTCMP,LIST=ALL
//ACCTCMP JOB (BLD),'WSO2DEMO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//* Compile+link ACCTINQ into IBMUSER.LOAD(ACCTINQ)
//* Uses COBOL-74 style compile. If your TK has a proc named UCLG, use that.
//STEP1   EXEC PROC=UCLG
//COBOL.SYSIN DD DSN=IBMUSER.COBOL(ACCTINQ),DISP=SHR
//LKED.SYSLMOD DD DSN=IBMUSER.LOAD(ACCTINQ),DISP=SHR
./ ADD NAME=CUSTCMP,LIST=ALL
//CUSTCMP JOB (BLD),'WSO2DEMO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//STEP1   EXEC PROC=UCLG
//COBOL.SYSIN DD DSN=IBMUSER.COBOL(CUSTPROF),DISP=SHR
//LKED.SYSLMOD DD DSN=IBMUSER.LOAD(CUSTPROF),DISP=SHR
./ ADD NAME=TXNCMP,LIST=ALL
//TXNCMP  JOB (BLD),'WSO2DEMO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//STEP1   EXEC PROC=UCLG
//COBOL.SYSIN DD DSN=IBMUSER.COBOL(TXNPOST),DISP=SHR
//LKED.SYSLMOD DD DSN=IBMUSER.LOAD(TXNPOST),DISP=SHR
./ ADD NAME=ACCTRUN,LIST=ALL
//ACCTRUN JOB (RUN),'WSO2DEMO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//STEP1   EXEC PGM=ACCTINQ
//STEPLIB DD  DSN=IBMUSER.LOAD,DISP=SHR
//SYSIN   DD  *
ACCTID=00000001
/*
./ ADD NAME=CUSTRUN,LIST=ALL
//CUSTRUN JOB (RUN),'WSO2DEMO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//STEP1   EXEC PGM=CUSTPROF
//STEPLIB DD  DSN=IBMUSER.LOAD,DISP=SHR
//SYSIN   DD  *
CUSTID=C000000001
/*
./ ADD NAME=TXNRUN,LIST=ALL
//TXNRUN  JOB (RUN),'WSO2DEMO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//STEP1   EXEC PGM=TXNPOST
//STEPLIB DD  DSN=IBMUSER.LOAD,DISP=SHR
//SYSIN   DD  *
FROM=00000001
TO=00000002
AMOUNT=125.50
CCY=LKR
/*
//

