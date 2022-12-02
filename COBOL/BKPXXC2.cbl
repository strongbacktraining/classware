      * PROCESS CICS('COBOL3,APOST,SP')                                 00000100
      * PROCESS APOST TRUNC(BIN) NOSSRANGE SIZE(4000K) TEST             00000200
      ***************************************************************** 00000300
      *                                                               * 00000400
      *  DFH0CALL DIVIDED INTO CLIENT AND SERVER FOR BANKING DEMO     * 00000501
      ***************************************************************** 00003500
       IDENTIFICATION DIVISION.                                         00003600
       PROGRAM-ID. BKPXXC2.                                             00003701
       ENVIRONMENT DIVISION.                                            00003800
       DATA DIVISION.                                                   00003900
       WORKING-STORAGE SECTION.                                         00004000
       77  MESSAGES     PIC X(39).                                      00005700
                                                                        00005705
       01  TEMP-NAME    PIC X(20).                                      00005800
       01  FILLER REDEFINES TEMP-NAME.                                  00005900
           02  TEMPA    PIC X OCCURS 20 INDEXED BY TEMPI.               00006000
       77  RESPONSE     PIC 9(9)  COMP.                                 00006220
       01  INSPECT-IND  PIC X.                                          00006300
           88  NO-MORE  VALUE '1'.                                      00006400
                        COPY BKMXXGA.                                   00006501
                        COPY BKMXXGB.                                   00006601
       01 CONSTANTS.                                                    00006700
          05 CRUD-PGM-NAME PIC X(8) VALUE 'BKPXXS1 '.                   00006801
          05 MENU-MAPS     PIC X(8) VALUE 'BKMXXGA '.                   00007201
          05 DETAIL-MAPS   PIC X(8) VALUE 'BKMXXGB '.                   00007601
          05 MENU-TRAN     PIC X(8) VALUE 'BKXX'.                       00008001
      *    Comment out the next line and remove the one of the comment  00008100
      *    from the next two lines to pick a right currency symobol     00008200
          05 INIT-AMOUNT   PIC X(8) VALUE '-000.00'.                    0008300
      *    05 INIT-AMOUNT   PIC X(8) VALUE '£0000.00'.                  00008400
      *    05 INIT-AMOUNT   PIC X(8) VALUE '¥0000.00'.                  00008500
      *                                                                 00009130
      *    NEXT FIELD TO VERIFY AMOUNTI                                 00009200
      *                                                                 00009300
       01  AMOUNTN.                                                     00009400
           03  AMOUNTN1  PIC X.                                         00009500
           03  AMOUNTN25 PIC X(4).                                      00009600
           03  AMOUNTN6  PIC X.                                         00009700
           03  AMOUNTN78 PIC X(2).                                      00009800
       01  LOGA.        COPY BKCLOG.                                    00009930
                        COPY DFHBMSCA.                                  00010000
       01  LINK-AREA.                                                   00010101
           05  LINKA.    COPY BKCLINK.                                  00010220
           05  FILEA.    COPY BKCACCT.                                  00010320
           05  COMM-AREA.    COPY BKCACCT.                              00010447
       LINKAGE SECTION.                                                 00010500
       01  DFHCOMMAREA.                                                 00010601
           05  FILLER.    COPY BKCLINK.                                 00010700
           05  FILLER.    COPY BKCACCT.                                 00010847
           05  FILLER.    COPY BKCACCT.                                 00010907
       PROCEDURE DIVISION.                                              00011000
      *                                                                 00011147
      *    THE "ERROR" EXIT IS SET UP. THIS IS TO TRAP ANY UNEXPECTED   00011200
      *    ERRORS, ALL OTHER ERROR CONDITIONS ARE TESTED FOR EXPLICITLY 00011300
      *    BY THE PROGRAM, USING THE "RESP" OPTION ON COMMANDS.         00011400
      *                                                                 00011500
           EXEC CICS HANDLE CONDITION ERROR(ERRORS) END-EXEC.           00011600
      *                                                                 00011700
           MOVE DFHCOMMAREA TO LINK-AREA.                               00011800
      *                                                                 00011900
      *    THE LENGTH OF THE "LINK AREA" IS TESTED. IF >= LINK-AREA THEN00012007
      *    THIS IS THE VALIDATION STAGE OF AN ADD OR UPDATE.            00012100
      *                                                                 00012200
           IF EIBCALEN >= LENGTH OF LINK-AREA THEN                      00012301
              GO TO READ-INPUT.                                         00012401
                                                                        00012507
           MOVE LOW-VALUES TO DETAILO.                                  00013200
      *                                                                 00013300
           IF ACCOUNT-OPEN OF LINK-AREA THEN                            00013905
              MOVE 'OPEN NEW ACCOUNT' TO TITLEO                         00014033
              MOVE 'ENTER DATA AND PRESS ENTER KEY' TO MSG3O            00014100
      *                                                                 00014200
              MOVE KEYNUM OF LINK-AREA TO NUMBO                         00014607
      *                                                                 00014700
      *       FOR THE "ADDS" TRANSACTION, THE AMOUNT FIELD HAS THE      00014800
      *       ATTRIBUTE BYTE SET TO NUMERIC SO ONLY NUMERIC DATA CAN    00014900
      *       BE ENTERED.                                               00015000
      *                                                                 00015100
              MOVE DFHBMUNN TO AMOUNTA                                  00015200
              MOVE INIT-AMOUNT TO AMOUNTO                               00015300
              GO TO MAP-SEND.                                           00015407
      *                                                                 00015500
      *    READ THE FILE RECORD INTO THE FILE AREA.                     00015600
      *                                                                 00015700
           SET ACCOUNT-READ OF LINK-AREA TO TRUE.                       00015805
           EXEC CICS LINK PROGRAM(CRUD-PGM-NAME)                        00015901
                 RESP(RESPONSE)COMMAREA(LINK-AREA) END-EXEC.            00016001
      *                                                                 00016133
      *    CHECK RESPONSE TO COMMAND                                    00016233
      *                                                                 00016333
           IF RESPONSE NOT = DFHRESP(NORMAL)                            00016433
              THEN GO TO CHECK-RESP                                     00016533
              ELSE PERFORM CHECK-LINK-RESP.                             00016633
           IF ACCOUNT-VIEW OF LINK-AREA THEN                            00016705
      *                                                                 00017000
      *       IF PROGRAM IS INVOKED BY " INQY", A TITLE AND COMMAND     00017100
      *       MESSAGE ARE MOVED TO THE MAP AREA.                        00017200
      *                                                                 00017300
              MOVE 'ACCOUNT INQUIRY' TO TITLEO                          00017433
              MOVE 'PRESS ENTER TO CONTINUE' TO MSG3O                   00017500
      *                                                                 00017600
      *       ALL FIELD ATTRIBUTES ARE PROTECTED.                       00017700
      *                                                                 00017800
              MOVE DFHBMPRO TO NAMEA                                    00017900
              MOVE DFHBMPRO TO ADDRA                                    00018000
              MOVE DFHBMPRO TO PHONEA                                   00018100
              MOVE DFHBMPRO TO DATEA                                    00018200
              MOVE DFHBMPRO TO AMOUNTA                                  00018300
              MOVE DFHBMPRO TO COMMENTA                                 00018400
      *                                                                 00018500
      *       THE FILE RECORD FIELDS ARE MOVED TO THE MAP AREA, AND     00018600
      *       THE INQUIRY SCREEN IS DISPLAYED.                          00018700
      *                                                                 00018800
              PERFORM MAP-BUILD THRU MAP-SEND                           00018900
      *                                                                 00019000
      *       THIS INVOCATION OF THE PROGRAM TERMINATES.                00019107
      *       THE "MENU" TRANSACTION WILL TO BE INVOKED                 00019207
      *       WHEN THE NEXT RESPONSE IS RECEIVED FROM THE TERMINAL.     00019307
      *                                                                 00019500
              EXEC CICS RETURN TRANSID(MENU-TRAN) END-EXEC.             00019633
      *                                                                 00019707
           IF ACCOUNT-CHANGE OF LINK-AREA THEN                          00019805
              MOVE 'ACCOUNT UPDATE' TO TITLEO                           00020233
              MOVE 'CHANGE FIELDS AND PRESS ENTER' TO MSG3O .           00020307
                                                                        00020407
       MAP-BUILD.                                                       00020507
      *                                                                 00021300
      *    THE FIELDS FROM THE FILE AREA ARE MOVED TO THE MAP AREA      00021400
      *                                                                 00021500
           MOVE NUMB    IN FILEA TO NUMBO.                              00021607
           MOVE NAME    IN FILEA TO NAMEO.                              00021707
           MOVE ADDRX   IN FILEA TO ADDRO.                              00021800
           MOVE PHONE   IN FILEA TO PHONEO.                             00021900
           MOVE DATEX   IN FILEA TO DATEO.                              00022000
           MOVE AMOUNT  IN FILEA TO AMOUNTO.                            00022100
           MOVE COMMENT IN FILEA TO COMMENTO.                           00022200
                                                                        00022307
       MAP-SEND.                                                        00022400
      *                                                                 00022500
      *    THE SCREEN IS ERASED BEFORE THE MAP IS DISPLAYED.            00022707
      *                                                                 00022800
           EXEC CICS SEND MAP('DETAIL') MAPSET(DETAIL-MAPS)             00022900
                     ERASE END-EXEC.                                    00023000
       FIN.                                                             00023100
      *                                                                 00023207
      *    THIS PROGRAM IS INVOKED WITH NEXT TERMINAL RESPONSE.         00023307
      *                                                                 00023407
           EXEC CICS RETURN TRANSID(EIBTRNID)                           00023504
                 COMMAREA(LINK-AREA)  END-EXEC.                         00023604
                                                                        00023704
      *                                                                 00023800
      *    CONTROL IS PASSED HERE WHEN THE TEST OF "EIBCALEN", AT THE   00023900
      *    BEGINING  OF THE PROGRAM, FINDS THAT A "LINK-AREA" HAS BEEN  00024007
      *    RECEIVED. THIS PART OF THE PROGRAM MAPS IN DATA FOR AN ADD   00024100
      *    OR UPDATE REQUEST, PERFORMS VALIDATION AND UPDATES "FILEA".  00024200
      *                                                                 00024300
       READ-INPUT.                                                      00024400
      *                                                                 00024500
      *    THE "RECEIVE MAP" COMMAND MAPS IN THE VARIABLES FROM THE     00024600
      *    SCREEN.                                                      00024700
      *                                                                 00024800
           EXEC CICS RECEIVE MAP('DETAIL') MAPSET(DETAIL-MAPS)          00024900
                     RESP(RESPONSE) END-EXEC.                           00025000
           IF RESPONSE NOT = DFHRESP(NORMAL) THEN GO TO CHECK-RESP.     00025100
                                                                        00025207
      *                                                                 00026633
      *          THE UPDATE FLAG IS SET IN THE RECORD AREA AND THE      00028400
      *          MESSAGE "RECORD UPDATED" IS MOVED TO THE MESSAGE AREA  00028500
      *          READY FOR DISPLAY ON THE OPERATOR INSTRUCTION SCREEN.  00028600
      *                                                                 00028700
           IF ACCOUNT-CHANGE OF LINK-AREA THEN                          00028807
                 MOVE 'U' TO STAT IN FILEA                              00028900
                 PERFORM CHECK THRU FILE-WRITE                          00029000
                 MOVE 'RECORD UPDATED' TO MESSAGES GO TO MENU.          00029100
      *                                                                 00029200
      *    IF THIS IS AN ADD REQUEST THE ADD FLAG IS SET IN THE NEW     00029300
      *    RECORD AND THE MESSAGE "RECORD ADDED" IS MOVED TO THE MESSAGE00029400
      *    AREA READY FOR DISPLAY ON THE OPERATOR INSTRUCTION SCREEN.   00029500
      *    example of compare                                           00029600
           IF ACCOUNT-OPEN OF LINK-AREA THEN                            00029705
                 MOVE LOW-VALUES TO FILEREC IN FILEA                    00029900
                 MOVE 'A' TO STAT IN FILEA                              00030000
                 PERFORM CHECK THRU FILE-WRITE                          00030100
                 MOVE 'RECORD ADDED' TO MESSAGES GO TO MENU.            00030200
      *                                                                 00030300
      *    CHECK FIELDS ADDED/UPDATED                                   00030400
      *                                                                 00030500
       CHECK.                                                           00030600
           IF NAMEI    = LOW-VALUES AND                                 00030700
              ADDRI    = LOW-VALUES AND                                 00030800
      *                                                                 00030900
      *       ANY REQUIRED EDITING STEPS SHOULD BE INSERTED HERE.       00031000
      *       A SUITABLE FORM OF EDITING SHOULD BE USED TO ENSURE VALID 00031100
      *       RECORDS ARE PLACED ON THE FILE. THE ROUTINE "INSP-NAME"   00031200
      *       IS CALLED AS AN EXAMPLE, TO REMOVE ALL NON-ALPHANUMERIC   00031300
      *       CHARACTERS AND REPLACE THEM WITH SPACES.                  00031400
      *                                                                 00031500
              PHONEI   = LOW-VALUES AND                                 00031600
              DATEI    = LOW-VALUES AND                                 00031700
              AMOUNTI  = LOW-VALUES AND                                 00031800
              COMMENTI = LOW-VALUES                                     00031900
              THEN MOVE 'RECORD NOT MODIFIED' TO MESSAGES               00032000
                   GO TO MENU.                                          00032100
      *                                                                 00032200
      *   INSP-NAME CHANGES ALL NON-ALPHABETIC CHARACTERS THAT ARE      00032300
      *   VALID IN A NAME TO SPACES SO THAT AN ALPHABETIC TEST MAY      00032400
      *   MAY BE APPLIED TO IT. THE CHANGED NAME IS RETURNED IN         00032500
      *   TEMP-NAME.                                                    00032600
      *                                                                 00032700
           PERFORM INSP-NAME.                                           00032800
           IF ACCOUNT-OPEN OF LINK-AREA THEN                            00032905
              IF TEMP-NAME NOT ALPHABETIC THEN GO TO DATA-ERROR.        00033000
           IF ACCOUNT-CHANGE OF LINK-AREA THEN                          00033105
               IF NAMEI NOT = LOW-VALUES                                00033200
               AND TEMP-NAME NOT ALPHABETIC THEN GO TO DATA-ERROR.      00033300
      *                                                                 00033400
      *    AMOUNTI MUST BE IN FORMAT NNNN.NN OR $NNNN.NN               00033500
      *                                                                 00033600
           IF AMOUNTI = LOW-VALUE THEN GO TO FILE-WRITE.                00033700
           MOVE AMOUNTI TO AMOUNTN.                                     00033800
           IF (AMOUNTN1 = '£' OR '$' OR '¥') AND                        00033900
              (AMOUNTN25 IS NUMERIC)  AND                               00034000
              (AMOUNTN6 = '.')        AND                               00034100
              (AMOUNTN78 IS NUMERIC)                                    00034200
               THEN GO TO FILE-WRITE                                    00034300
           ELSE                                                         00034400
               GO TO DATA-ERROR.                                        00034500
                                                                        00034600
       INSP-NAME.                                                       00034700
           MOVE NAMEI TO TEMP-NAME.                                     00034800
           MOVE SPACE TO INSPECT-IND.                                   00034900
      *    CANNOT USE 'INSPECT' FEATURE IN OLD COBOL                    00035000
           PERFORM SEARCH-NAME UNTIL NO-MORE.                           00035100
                                                                        00035200
       SEARCH-NAME.                                                     00035300
           SEARCH TEMPA VARYING TEMPI                                   00035400
           AT END MOVE '1' TO INSPECT-IND                               00035500
           WHEN TEMPA(TEMPI) EQUAL TO '.' MOVE SPACE TO TEMPA(TEMPI)    00035600
           WHEN TEMPA(TEMPI) EQUAL TO '-' MOVE SPACE TO TEMPA(TEMPI)    00035700
           WHEN TEMPA(TEMPI) EQUAL TO QUOTES MOVE SPACE TO TEMPA(TEMPI).00035800
                                                                        00035900
       FILE-WRITE.                                                      00036000
      *                                                                 00036400
      *    THIS CODE CREATES OR UPDATES THE ACCOUNT RECORD. ANY FIELD   00036500
      *    WHICH HAS BEEN ENTERED IS MOVED TO THE ACCOUNT RECORD.       00036600
      *                                                                 00036700
           IF NAMEI   NOT = LOW-VALUE MOVE NAMEI   TO NAME   IN FILEA.  00036807
           IF ADDRI   NOT = LOW-VALUE MOVE ADDRI   TO ADDRX  IN FILEA.  00036900
           IF PHONEI  NOT = LOW-VALUE MOVE PHONEI  TO PHONE  IN FILEA.  00037000
           IF DATEI   NOT = LOW-VALUE MOVE DATEI   TO DATEX  IN FILEA.  00037100
           IF AMOUNTI NOT = LOW-VALUE MOVE AMOUNTI TO AMOUNT IN FILEA.  00037200
           IF AMOUNTI = LOW-VALUE AND ACCOUNT-OPEN OF LINK-AREA THEN    00037305
              MOVE INIT-AMOUNT TO AMOUNT IN FILEA.                      00037400
           IF COMMENTI NOT = LOW-VALUE THEN                             00037500
              MOVE COMMENTI TO COMMENT IN FILEA.                        00037600
                                                                        00037733
           IF ACCOUNT-CHANGE OF LINK-AREA THEN                          00038305
      *                                                                 00038400
      *       FOR AN UPDATE REQUEST THE UPDATED ACCOUNT RECORD IS       00038500
      *       REWRITTEN TO "FILEA"                                      00038646
      *                                                                 00038700
             SET ACCOUNT-UPDATE OF LINK-AREA TO TRUE                    00038805
             EXEC CICS LINK PROGRAM(CRUD-PGM-NAME)                      00038901
                  RESP(RESPONSE) COMMAREA(LINK-AREA) END-EXEC           00039001
              IF RESPONSE NOT = DFHRESP(NORMAL) THEN GO TO CHECK-RESP   00039500
              ELSE PERFORM CHECK-LINK-RESP                              00039633
           ELSE                                                         00039733
      *                                                                 00039800
      *       FOR AN CREATE REQUEST THE NEW ACCOUNT                     00039907
      *       RECORD IS WRITTEN TO "FILEA"                              00040046
             MOVE KEYNUM OF LINK-AREA TO NUMB OF FILEA                  00040107
             SET ACCOUNT-CREATE OF LINK-AREA TO TRUE                    00040505
             EXEC CICS LINK PROGRAM(CRUD-PGM-NAME)                      00040601
                   RESP(RESPONSE) COMMAREA(LINK-AREA) END-EXEC          00040701
            IF RESPONSE NOT = DFHRESP(NORMAL) THEN GO TO CHECK-RESP     00040933
              ELSE PERFORM CHECK-LINK-RESP.                             00041033
                                                                        00042733
       DATA-ERROR.                                                      00043200
           MOVE DFHBMASB TO MSG3A.                                      00043300
      *                                                                 00043400
      *    WHEN A DATA ERROR IS DETECTED THE SCREEN IS REDISPLAYED FOR  00043500
      *    ERRORS TO BE CORRECTED. AN ERROR MESSAGE IS MOVED TO THE MAP 00043600
      *    AREA AND HIGHLIGHTED.                                        00043700
      *                                                                 00043800
           MOVE 'DATA ERROR - CORRECT AND PRESS ENTER' TO MSG3O         00043900
      *                                                                 00044000
      *    THE FIELD ATTRIBUTE IS SET TO MODIFIED SO DATA WILL DISPLAY  00044100
      *    AMOUNT IS SET NUMERIC ALSO.                                  00044200
      *                                                                 00044300
           MOVE DFHUNNUM TO AMOUNTA.                                    00044400
      *                                                                 00044500
      *    THE MODIFIED DATA TAG IS SET ON FOR ALL THE DATA FIELDS SO   00044600
      *    THAT ALL THE DATA IS RECEIVED AT THE NEXT "RECEIVE MAP".     00044700
      *                                                                 00044800
           MOVE DFHBMFSE TO NAMEA, ADDRA, PHONEA, DATEA,                00044900
                COMMENTA.                                               00045000
      *                                                                 00045100
      *    THE CONTENTS OF MAP "BKACCGB" ARE SENT TO THE SCREEN. THE    00045200
      *    CONSTANT INFORMATION ON THE SCREEN IS NOT REFRESHED AS A     00045300
      *    RESULT OF THE USE OF THE "DATAONLY" OPTION.                  00045400
      *                                                                 00045500
           EXEC CICS SEND MAP('DETAIL') MAPSET(DETAIL-MAPS)             00045600
                     DATAONLY END-EXEC.                                 00045700
       CICS-CONTROL.                                                    00046400
      *                                                                 00046500
      *    AFTER THE "FILE ADD" OR "FILE UPDATE" SCREEN HAS BEEN        00046600
      *    DISPLAYED THE PROGRAM BRANCHES HERE TO RETURN TO CICS        00046700
      *    AWAITING A RESPONSE FROM THE TERMINAL. THE "RETURN" GIVES    00046800
      *    CICS THE TRANSACTION IDENTIFIER FOR NEXT TRANSACTION AT      00046900
      *    THIS TERMINAL TOGETHER WITH A "COMMAREA" CONTAINING ALL      00047000
      *    INFORMATION THAT THE PROGRAM NEEDS TO CONTINUE THE UPDATE.   00047100
      *    THE "COMMAREA" IS PASSED TO THE NEXT INVOCATION OF THIS      00047200
      *    PROGRAM.                                                     00047300
      *                                                                 00047400
           EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(LINK-AREA)       00047504
                     END-EXEC.                                          00047604
      *                                                                 00047700
      *    THIS ROUTINE GAINS CONTROL WHENEVER A CICS COMMAND RETURNS A 00047800
      *    NON-"NORMAL" RESPONSE. THE ROUTINE EXPLICITLY CHECKS FOR THE 00047900
      *    RESPONSES "NOTFND", "DUPREC", AND "MAPFAIL". THERE IS ALSO A 00048000
      *    'CATCH ALL' TO TRAP ANY RESPONSE THAT IS NOT "NORMAL". THE   00048100
      *    ROUTINE "MENU" IS CALLED TO RE-DISPLAY THE MENU MAP, ALONG   00048200
      *    WITH AN ERROR MESSAGE.                                       00048300
      *                                                                 00048400
       CHECK-LINK-RESP.                                                 00048514
      *       IF THE CURRENT FILE RECORD IS NOT THE SAME AS THE ONE     00048733
      *       SAVED IN THE "COMMAREA" THEN ANOTHER USER HAS UPDATED THE 00048833
      *       RECORD. A WARNING MESSAGE IS DISPLAYED, WITH FIELDS FROM  00048933
      *       THE RECORD READ FROM "FILEA", FOR REENTRY OF THE UPDATES. 00049033
      *                                                                 00049133
      *       IF FILEREC IN FILEA NOT = FILEREC IN COMM-AREA THEN       00049247
      *           RCODE = 98 IS NOW RETURNED WHEN THIS IS TRUE          00049333
           IF RCODE OF LINK-AREA = 98 THEN                              00049405
              MOVE 'RECORD UPDATED BY OTHER USER, TRY AGAIN' TO MSG1O   00049533
              MOVE DFHBMASB TO MSG1A                                    00049633
              MOVE DFHPROTN TO MSG3A                                    00049733
              PERFORM MAP-BUILD                                         00049833
              EXEC CICS SEND MAP('DETAIL') MAPSET(DETAIL-MAPS)          00049900
                           END-EXEC                                     00050033
              GO TO CICS-CONTROL.                                       00050333
                                                                        00050433
           IF RCODE OF LINK-AREA = DFHRESP(NOTFND) THEN                 00050505
              MOVE 'INVALID NUMBER - PLEASE REENTER' TO MESSAGES        00050600
              GO TO MENU.                                               00050700
           IF RCODE OF LINK-AREA = DFHRESP(DUPREC) THEN                 00050805
              MOVE 'DUPLICATE RECORD' TO MESSAGES                       00050900
              GO TO MENU.                                               00051000
           IF RCODE OF LINK-AREA NOT = DFHRESP(NORMAL) THEN             00051105
              MOVE 'TRANSACTION TERMINATED' TO MESSAGES                 00051314
              GO TO MENU.                                               00051414
       CHECK-RESP.                                                      00051514
           IF RESPONSE = DFHRESP(MAPFAIL) THEN                          00051600
              IF EIBCALEN = 0 THEN                                      00051700
                 MOVE 'PRESS CLEAR TO EXIT' TO MESSAGES                 00051800
                 GO TO MENU                                             00051900
              ELSE                                                      00052000
                 MOVE 'RECORD NOT MODIFIED' TO MESSAGES                 00052100
                 GO TO MENU.                                            00052200
           IF RESPONSE NOT = DFHRESP(NORMAL) THEN                       00052300
              MOVE 'TRANSACTION TERMINATED' TO MESSAGES                 00052500
              GO TO MENU.                                               00052600
      *                                                                 00052700
      *    IF A CICS COMMAND FAILS WITH THE "ERROR" CONDITION, THE      00053900
      *    MESSAGE "ERROR. TRANSACTION TERMINATED" IS MOVED TO          00054000
      *    "MESSAGES" FOR DISPLAY ON THE MENU SCREEN.                   00054100
      *                                                                 00054200
       ERRORS.                                                          00054300
           MOVE 'ERROR. TRANSACTION TERMINATED' TO MESSAGES.            00054400
           GO TO MENU.                                                  00054500
       MENU.                                                            00054600
      *                                                                 00054700
      *    THIS CODE GETS CONTROL WHEN AN ADD OR UPDATE IS COMPLETE.    00054800
      *    AN INFORMATION OR ERROR MESSAGE IS IN "MESSAGES".            00054900
      *    THE OPERATOR INSTRUCTION MAP AREA IS CLEARED. THE MESSAGE    00055000
      *    IS MOVED TO THE MAP AREA AND HIGHLIGHTED.                    00055100
      *                                                                 00055200
           MOVE LOW-VALUE TO MENUO.                                     00055300
           MOVE DFHBMASB  TO MSGA.                                      00055400
           MOVE MESSAGES  TO MSGO.                                      00055500
      *                                                                 00055600
      *    THE OPERATOR INSTRUCTION MAP "BKACCGA" IS DISPLAYED ON AN    00055700
      *    ERASED SCREEN.                                               00055800
      *                                                                 00055900
           EXEC CICS SEND MAP('MENU') MAPSET(MENU-MAPS)                 00056000
                     ERASE END-EXEC.                                    00056100
      *                                                                 00056200
      *    THE PROGRAM TERMINATES BY RETURNING TO CICS.                 00056300
      *    NO TRANSACTION IDENTIFIER OR "COMMAREA" IS SPECIFIED.        00056400
      *                                                                 00056500
           EXEC CICS RETURN TRANSID(MENU-TRAN) END-EXEC.                00056601
           GOBACK.                                                      00056700
