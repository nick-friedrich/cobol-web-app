      *> ============================================================
      *>  STREAKS - a habit tracker web app, written in COBOL.
      *>  The HTTP server, the routing, the HTML and the database
      *>  are all in this one file. Build and run:
      *>      cobc -x -free -fstatic-call habits.cbl -o habits
      *>      ./habits            (then open http://localhost:8080)
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HABITS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HABIT-FILE ASSIGN TO "data/habits.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           SELECT LOG-FILE ASSIGN TO "data/log.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           SELECT CSS-FILE ASSIGN TO "public/style.css"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  HABIT-FILE.
       01  HABIT-RECORD.
           05 HR-ID              PIC 9(4).
           05 HR-NAME            PIC X(120).
       FD  LOG-FILE.
       01  LOG-RECORD.
           05 LR-HABIT           PIC 9(4).
           05 LR-DATE            PIC 9(8).
       FD  CSS-FILE.
       01  CSS-LINE              PIC X(200).

       WORKING-STORAGE SECTION.
      *> ---- the "database", loaded fresh for every request -------
       01  HABIT-TABLE.
           05 HABIT-COUNT        PIC 9(4) COMP-5 VALUE 0.
           05 NEXT-ID            PIC 9(4) VALUE 1.
           05 HABIT OCCURS 100 TIMES.
              10 H-ID            PIC 9(4).
              10 H-NAME          PIC X(120).
       01  LOG-TABLE.
           05 LOG-COUNT          PIC 9(4) COMP-5 VALUE 0.
           05 LOG-ENTRY OCCURS 5000 TIMES.
              10 L-HABIT         PIC 9(4).
              10 L-DATE          PIC 9(8).
       01  FILE-STATUS           PIC XX.

      *> ---- sockets: we talk to the C library directly ------------
       01  SERVER-FD             BINARY-LONG.
       01  CLIENT-FD             BINARY-LONG.
       01  CHILD-PID             BINARY-LONG.
       01  RC                    BINARY-LONG.
       01  NULL-PTR              USAGE POINTER VALUE NULL.
       01  SIG-IGNORE            BINARY-DOUBLE VALUE 1.
       01  SOCKET-ADDRESS.
           05 FILLER             PIC X     VALUE X"10".
           05 FILLER             PIC X     VALUE X"02".
           05 FILLER             PIC X(2)  VALUE X"1F90".
           05 FILLER             PIC X(4)  VALUE X"7F000001".
           05 FILLER             PIC X(8)  VALUE LOW-VALUES.
       01  OPTION-ON             PIC X(4)  VALUE X"01000000".
       01  FIVE-SECONDS          PIC X(16) VALUE
           X"05000000000000000000000000000000".

      *> ---- the request --------------------------------------------
       01  REQUEST               PIC X(16384).
       01  REQUEST-LEN           BINARY-LONG.
       01  BYTES-READ            BINARY-LONG.
       01  READ-ROOM             BINARY-DOUBLE.
       01  HEADER-LEN            BINARY-LONG.
       01  BODY-START            BINARY-LONG.
       01  BODY-LEN              BINARY-LONG.
       01  CONTENT-LENGTH        BINARY-LONG.
       01  UPPER-HEADERS         PIC X(4096).
       01  CL-OFFSET             BINARY-LONG.
       01  CL-TEXT               PIC X(10).
       01  HTTP-METHOD                PIC X(8).
       01  REQ-PATH              PIC X(256).
       01  FORM-VALUE            PIC X(1024).
       01  DECODED               PIC X(1024).
       01  DECODED-LEN           BINARY-LONG.

      *> ---- the response -------------------------------------------
       01  RESPONSE-BODY         PIC X(200000).
       01  BP                    BINARY-LONG.
       01  RESPONSE-HEAD         PIC X(512).
       01  HP                    BINARY-LONG.
       01  STATUS-LINE           PIC X(32).
       01  CONTENT-TYPE          PIC X(40).
       01  LOCATION              PIC X(64).
       01  LEN-TEXT              PIC Z(8)9.
       01  SEND-LEN              BINARY-DOUBLE.
       01  SENT                  BINARY-LONG.

      *> ---- dates and rendering -----------------------------------
       01  TODAY                 PIC 9(8).
       01  TODAY-INT             BINARY-LONG.
       01  DAY-INT               BINARY-LONG.
       01  A-DATE                PIC 9(8).
       01  A-DATE-PARTS REDEFINES A-DATE.
           05 A-YEAR             PIC 9(4).
           05 A-MONTH            PIC 99.
           05 A-DAY              PIC 99.
       01  WEEKDAY               BINARY-LONG.
       01  DAY-NAMES             PIC X(63) VALUE
           "Monday   Tuesday  WednesdayThursday Friday   Saturday Sunday   ".
       01  DAY-NAME REDEFINES DAY-NAMES PIC X(9) OCCURS 7 TIMES.
       01  MONTH-NAMES           PIC X(108) VALUE
           "January  February March    April    May      June     July     August   SeptemberOctober  November December ".
       01  MONTH-NAME REDEFINES MONTH-NAMES PIC X(9) OCCURS 12 TIMES.
       01  DAY-LETTERS           PIC X(7) VALUE "MTWTFSS".
       01  I                     BINARY-LONG.
       01  J                     BINARY-LONG.
       01  K                     BINARY-LONG.
       01  FOUND-AT              BINARY-LONG.
       01  WANT-HABIT            PIC 9(4).
       01  WANT-DATE             PIC 9(8).
       01  STREAK                BINARY-LONG.
       01  DONE-TODAY            BINARY-LONG.
       01  PERCENT               BINARY-LONG.
       01  NUM-TEXT              PIC ZZZ9.
       01  DAY-TEXT              PIC Z9.
       01  EMOJI                 PIC X(12).
       01  UPPER-NAME            PIC X(120).
       01  HITS                  BINARY-LONG.
       01  ONE-CHAR                    PIC X.
       01  HEX-DIGITS            PIC X(16) VALUE "0123456789ABCDEF".
       01  HEX-PAIR              PIC XX.
       01  NIBBLE                BINARY-LONG.
       01  BYTE-VALUE            BINARY-LONG.

       PROCEDURE DIVISION.
       MAIN.
           CALL "signal" USING BY VALUE 20 BY VALUE SIG-IGNORE
           CALL "socket" USING BY VALUE 2 BY VALUE 1 BY VALUE 0
               RETURNING SERVER-FD
           CALL "setsockopt" USING BY VALUE SERVER-FD
               BY VALUE 65535 BY VALUE 4
               BY REFERENCE OPTION-ON BY VALUE 4
           CALL "bind" USING BY VALUE SERVER-FD
               BY REFERENCE SOCKET-ADDRESS BY VALUE 16
               RETURNING RC
           IF RC NOT = 0
               DISPLAY "Port 8080 is busy." STOP RUN
           END-IF
           CALL "listen" USING BY VALUE SERVER-FD BY VALUE 64
           DISPLAY "STREAKS is listening on http://localhost:8080"

           PERFORM FOREVER
               CALL "accept" USING BY VALUE SERVER-FD
                   BY VALUE NULL-PTR BY VALUE NULL-PTR
                   RETURNING CLIENT-FD
               IF CLIENT-FD >= 0
      *>           one process per request, like a web server in 1995
                   CALL "fork" RETURNING CHILD-PID
                   IF CHILD-PID = 0
                       CALL "close" USING BY VALUE SERVER-FD
                       PERFORM HANDLE-REQUEST
                       CALL "close" USING BY VALUE CLIENT-FD
                       STOP RUN
                   END-IF
                   CALL "close" USING BY VALUE CLIENT-FD
               END-IF
           END-PERFORM.

      *> ============================================================
      *>  HTTP
      *> ============================================================
       HANDLE-REQUEST.
           CALL "setsockopt" USING BY VALUE CLIENT-FD
               BY VALUE 65535 BY VALUE 4130
               BY REFERENCE OPTION-ON BY VALUE 4
           CALL "setsockopt" USING BY VALUE CLIENT-FD
               BY VALUE 65535 BY VALUE 4102
               BY REFERENCE FIVE-SECONDS BY VALUE 16
           PERFORM READ-REQUEST
           IF REQUEST-LEN = 0 EXIT PARAGRAPH END-IF

           UNSTRING REQUEST DELIMITED BY " " OR "?" INTO HTTP-METHOD REQ-PATH
           DISPLAY FUNCTION TRIM(HTTP-METHOD) " " FUNCTION TRIM(REQ-PATH)

           PERFORM LOAD-DATABASE
           MOVE FUNCTION CURRENT-DATE(1:8) TO TODAY
           COMPUTE TODAY-INT = FUNCTION INTEGER-OF-DATE(TODAY)

           EVALUATE TRUE
               WHEN HTTP-METHOD = "GET" AND REQ-PATH = "/"
                   PERFORM RENDER-PAGE
               WHEN HTTP-METHOD = "GET" AND REQ-PATH = "/style.css"
                   PERFORM SERVE-CSS
               WHEN HTTP-METHOD = "POST" AND REQ-PATH = "/add"
                   PERFORM ADD-HABIT
               WHEN HTTP-METHOD = "POST" AND REQ-PATH = "/toggle"
                   PERFORM TOGGLE-DAY
               WHEN HTTP-METHOD = "POST" AND REQ-PATH = "/delete"
                   PERFORM DELETE-HABIT
               WHEN OTHER
                   MOVE "404 Not Found" TO STATUS-LINE
                   MOVE "text/plain" TO CONTENT-TYPE
                   MOVE "Nothing here." TO RESPONSE-BODY
                   MOVE 14 TO BP
                   PERFORM SEND-RESPONSE
           END-EVALUATE.

       READ-REQUEST.
           MOVE SPACES TO REQUEST
           MOVE 0 TO REQUEST-LEN CONTENT-LENGTH BODY-START
           PERFORM WITH TEST AFTER
                   UNTIL BYTES-READ <= 0
                      OR (BODY-START > 0 AND
                          REQUEST-LEN >= BODY-START - 1 + CONTENT-LENGTH)
                      OR REQUEST-LEN >= 16000
               COMPUTE READ-ROOM = 16000 - REQUEST-LEN
               CALL "read" USING BY VALUE CLIENT-FD
                   BY REFERENCE REQUEST(REQUEST-LEN + 1:)
                   BY VALUE READ-ROOM
                   RETURNING BYTES-READ
               IF BYTES-READ > 0
                   ADD BYTES-READ TO REQUEST-LEN
                   IF BODY-START = 0 PERFORM FIND-BODY END-IF
               END-IF
           END-PERFORM
           IF BODY-START > 0
               COMPUTE BODY-LEN = REQUEST-LEN - BODY-START + 1
           END-IF.

       FIND-BODY.
           MOVE 0 TO HEADER-LEN
           INSPECT REQUEST(1:REQUEST-LEN) TALLYING HEADER-LEN
               FOR CHARACTERS BEFORE INITIAL X"0D0A0D0A"
           IF HEADER-LEN < REQUEST-LEN
               COMPUTE BODY-START = HEADER-LEN + 5
               MOVE FUNCTION UPPER-CASE(REQUEST(1:HEADER-LEN))
                   TO UPPER-HEADERS
               MOVE 0 TO CL-OFFSET
               INSPECT UPPER-HEADERS TALLYING CL-OFFSET
                   FOR CHARACTERS BEFORE INITIAL "CONTENT-LENGTH:"
               IF CL-OFFSET < HEADER-LEN
                   UNSTRING UPPER-HEADERS(CL-OFFSET + 16:)
                       DELIMITED BY X"0D" INTO CL-TEXT
                   COMPUTE CONTENT-LENGTH = FUNCTION NUMVAL(CL-TEXT)
               END-IF
           END-IF.

       SEND-RESPONSE.
           COMPUTE SEND-LEN = BP - 1
           MOVE SEND-LEN TO LEN-TEXT
           MOVE SPACES TO RESPONSE-HEAD
           MOVE 1 TO HP
           STRING "HTTP/1.1 " FUNCTION TRIM(STATUS-LINE) X"0D0A"
                  "Server: GnuCOBOL" X"0D0A"
                  "Connection: close" X"0D0A"
                  "Content-Type: " FUNCTION TRIM(CONTENT-TYPE) X"0D0A"
                  "Content-Length: " FUNCTION TRIM(LEN-TEXT) X"0D0A"
               DELIMITED BY SIZE INTO RESPONSE-HEAD WITH POINTER HP
           IF LOCATION NOT = SPACES
               STRING "Location: " FUNCTION TRIM(LOCATION) X"0D0A"
                   DELIMITED BY SIZE INTO RESPONSE-HEAD WITH POINTER HP
           END-IF
           STRING X"0D0A" DELIMITED BY SIZE
               INTO RESPONSE-HEAD WITH POINTER HP
           COMPUTE SEND-LEN = HP - 1
           CALL "write" USING BY VALUE CLIENT-FD
               BY REFERENCE RESPONSE-HEAD BY VALUE SEND-LEN
           MOVE 1 TO I
           PERFORM UNTIL I >= BP
               COMPUTE SEND-LEN = BP - I
               CALL "write" USING BY VALUE CLIENT-FD
                   BY REFERENCE RESPONSE-BODY(I:) BY VALUE SEND-LEN
                   RETURNING SENT
               IF SENT <= 0 EXIT PERFORM END-IF
               ADD SENT TO I
           END-PERFORM.

       REDIRECT-HOME.
           MOVE "303 See Other" TO STATUS-LINE
           MOVE "text/plain" TO CONTENT-TYPE
           MOVE "/" TO LOCATION
           MOVE 1 TO BP
           PERFORM SEND-RESPONSE.

      *> ============================================================
      *>  ROUTES
      *> ============================================================
       ADD-HABIT.
           PERFORM GET-FORM-VALUE
           IF DECODED NOT = SPACES AND HABIT-COUNT < 100
               ADD 1 TO HABIT-COUNT
               MOVE NEXT-ID TO H-ID(HABIT-COUNT)
               MOVE FUNCTION TRIM(DECODED) TO H-NAME(HABIT-COUNT)
               PERFORM SAVE-HABITS
           END-IF
           PERFORM REDIRECT-HOME.

       TOGGLE-DAY.
      *>   the button sends t=HHHH-YYYYMMDD
           PERFORM GET-FORM-VALUE
           MOVE DECODED(1:4) TO WANT-HABIT
           MOVE DECODED(6:8) TO WANT-DATE
           PERFORM FIND-LOG
           IF FOUND-AT > 0
               MOVE LOG-ENTRY(LOG-COUNT) TO LOG-ENTRY(FOUND-AT)
               SUBTRACT 1 FROM LOG-COUNT
           ELSE
               ADD 1 TO LOG-COUNT
               MOVE WANT-HABIT TO L-HABIT(LOG-COUNT)
               MOVE WANT-DATE  TO L-DATE(LOG-COUNT)
           END-IF
           PERFORM SAVE-LOG
           PERFORM REDIRECT-HOME.

       DELETE-HABIT.
           PERFORM GET-FORM-VALUE
           MOVE DECODED(1:4) TO WANT-HABIT
           MOVE 0 TO J
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HABIT-COUNT
               IF H-ID(I) NOT = WANT-HABIT
                   ADD 1 TO J
                   MOVE HABIT(I) TO HABIT(J)
               END-IF
           END-PERFORM
           MOVE J TO HABIT-COUNT
           MOVE 0 TO J
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LOG-COUNT
               IF L-HABIT(I) NOT = WANT-HABIT
                   ADD 1 TO J
                   MOVE LOG-ENTRY(I) TO LOG-ENTRY(J)
               END-IF
           END-PERFORM
           MOVE J TO LOG-COUNT
           PERFORM SAVE-HABITS
           PERFORM SAVE-LOG
           PERFORM REDIRECT-HOME.

       SERVE-CSS.
           MOVE "200 OK" TO STATUS-LINE
           MOVE "text/css; charset=utf-8" TO CONTENT-TYPE
           MOVE 1 TO BP
           OPEN INPUT CSS-FILE
           PERFORM UNTIL FILE-STATUS NOT = "00"
               READ CSS-FILE
                   NOT AT END
                       STRING FUNCTION TRIM(CSS-LINE TRAILING) X"0A"
                           DELIMITED BY SIZE
                           INTO RESPONSE-BODY WITH POINTER BP
               END-READ
           END-PERFORM
           CLOSE CSS-FILE
           PERFORM SEND-RESPONSE.

      *> ============================================================
      *>  FORMS: the body looks like  n=Read+10+pages
      *> ============================================================
       GET-FORM-VALUE.
           MOVE SPACES TO FORM-VALUE DECODED
           IF BODY-LEN > 0
               UNSTRING REQUEST(BODY-START:BODY-LEN)
                   DELIMITED BY "=" OR "&"
                   INTO FORM-VALUE FORM-VALUE
           END-IF
           MOVE 0 TO DECODED-LEN
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 1000
                   OR FORM-VALUE(I:) = SPACES
               MOVE FORM-VALUE(I:1) TO ONE-CHAR
               EVALUATE ONE-CHAR
                   WHEN "+"
                       MOVE " " TO ONE-CHAR
                   WHEN "%"
                       MOVE FUNCTION UPPER-CASE(FORM-VALUE(I + 1:2))
                           TO HEX-PAIR
                       PERFORM HEX-TO-BYTE
                       MOVE FUNCTION CHAR(BYTE-VALUE + 1) TO ONE-CHAR
                       ADD 2 TO I
               END-EVALUATE
               ADD 1 TO DECODED-LEN
               MOVE ONE-CHAR TO DECODED(DECODED-LEN:1)
           END-PERFORM.

       HEX-TO-BYTE.
           MOVE 0 TO BYTE-VALUE
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 2
               MOVE 0 TO NIBBLE
               INSPECT HEX-DIGITS TALLYING NIBBLE
                   FOR CHARACTERS BEFORE INITIAL HEX-PAIR(K:1)
               COMPUTE BYTE-VALUE = BYTE-VALUE * 16 + NIBBLE
           END-PERFORM.

      *> ============================================================
      *>  DATABASE: two plain text files
      *> ============================================================
       LOAD-DATABASE.
           MOVE 0 TO HABIT-COUNT LOG-COUNT
           MOVE 1 TO NEXT-ID
           OPEN INPUT HABIT-FILE
           IF FILE-STATUS = "00"
               PERFORM UNTIL FILE-STATUS NOT = "00"
                   READ HABIT-FILE
                       NOT AT END
                           ADD 1 TO HABIT-COUNT
                           MOVE HABIT-RECORD TO HABIT(HABIT-COUNT)
                           IF HR-ID >= NEXT-ID
                               COMPUTE NEXT-ID = HR-ID + 1
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE HABIT-FILE
           END-IF
           OPEN INPUT LOG-FILE
           IF FILE-STATUS = "00"
               PERFORM UNTIL FILE-STATUS NOT = "00"
                   READ LOG-FILE
                       NOT AT END
                           ADD 1 TO LOG-COUNT
                           MOVE LOG-RECORD TO LOG-ENTRY(LOG-COUNT)
                   END-READ
               END-PERFORM
               CLOSE LOG-FILE
           END-IF.

       SAVE-HABITS.
           OPEN OUTPUT HABIT-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HABIT-COUNT
               WRITE HABIT-RECORD FROM HABIT(I)
           END-PERFORM
           CLOSE HABIT-FILE.

       SAVE-LOG.
           OPEN OUTPUT LOG-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LOG-COUNT
               WRITE LOG-RECORD FROM LOG-ENTRY(I)
           END-PERFORM
           CLOSE LOG-FILE.

       FIND-LOG.
           MOVE 0 TO FOUND-AT
           PERFORM VARYING K FROM 1 BY 1
                   UNTIL K > LOG-COUNT OR FOUND-AT > 0
               IF L-HABIT(K) = WANT-HABIT AND L-DATE(K) = WANT-DATE
                   MOVE K TO FOUND-AT
               END-IF
           END-PERFORM.

      *> ============================================================
      *>  HTML
      *> ============================================================
       RENDER-PAGE.
           MOVE "200 OK" TO STATUS-LINE
           MOVE "text/html; charset=utf-8" TO CONTENT-TYPE
           MOVE 1 TO BP

           MOVE 0 TO DONE-TODAY
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HABIT-COUNT
               MOVE H-ID(I) TO WANT-HABIT
               MOVE TODAY TO WANT-DATE
               PERFORM FIND-LOG
               IF FOUND-AT > 0 ADD 1 TO DONE-TODAY END-IF
           END-PERFORM
           MOVE 0 TO PERCENT
           IF HABIT-COUNT > 0
               COMPUTE PERCENT = DONE-TODAY * 100 / HABIT-COUNT
           END-IF

           MOVE TODAY TO A-DATE
           COMPUTE WEEKDAY = FUNCTION MOD(TODAY-INT - 1, 7) + 1
           MOVE A-DAY TO DAY-TEXT

           STRING
             "<!doctype html><html lang='en'><head><meta charset='utf-8'>"
             "<meta name='viewport' content='width=device-width,initial-scale=1'>"
             "<title>Streaks</title><link rel='stylesheet' href='/style.css'>"
             "</head><body><main>"
             "<header><div><p class='eyebrow'>"
             FUNCTION TRIM(DAY-NAME(WEEKDAY)) ", "
             FUNCTION TRIM(DAY-TEXT) " " FUNCTION TRIM(MONTH-NAME(A-MONTH))
             "</p><h1>Streaks</h1></div>"
             DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP

           MOVE PERCENT TO NUM-TEXT
           STRING "<div class='ring' style='--p:" FUNCTION TRIM(NUM-TEXT)
             "'><span>" DELIMITED BY SIZE
             INTO RESPONSE-BODY WITH POINTER BP
           MOVE DONE-TODAY TO NUM-TEXT
           STRING FUNCTION TRIM(NUM-TEXT) "<small>/" DELIMITED BY SIZE
             INTO RESPONSE-BODY WITH POINTER BP
           MOVE HABIT-COUNT TO NUM-TEXT
           STRING FUNCTION TRIM(NUM-TEXT) "</small></span></div></header>"
             "<form class='add' method='post' action='/add'>"
             "<input name='n' placeholder='Add a habit, e.g. Read 10 pages'"
             " maxlength='40' autocomplete='off' required>"
             "<button>Add</button></form><ul class='habits'>"
             DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > HABIT-COUNT
               PERFORM RENDER-HABIT
           END-PERFORM

           IF HABIT-COUNT = 0
               STRING "<li class='empty'>No habits yet. Add your first one above.</li>"
                 DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP
           END-IF
           STRING "</ul><footer>Rendered by COBOL &middot; GnuCOBOL 3.2"
             " &middot; zero JavaScript</footer></main></body></html>"
             DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP
           PERFORM SEND-RESPONSE.

       RENDER-HABIT.
           MOVE H-ID(J) TO WANT-HABIT
           PERFORM PICK-EMOJI
           PERFORM COUNT-STREAK
           COMPUTE K = FUNCTION MOD(H-ID(J), 6)
           MOVE K TO DAY-TEXT
           STRING "<li class='habit c" FUNCTION TRIM(DAY-TEXT) "'>"
             "<form method='post' action='/toggle'><div class='top'>"
             "<span class='emoji'>" FUNCTION TRIM(EMOJI) "</span>"
             "<div class='meta'><h2>"
             DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP
           PERFORM EMIT-ESCAPED-NAME
           MOVE STREAK TO NUM-TEXT
           IF STREAK = 0
               STRING "</h2><p>Start a streak today</p></div>"
                 DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP
           ELSE
               STRING "</h2><p><b>&#128293; " FUNCTION TRIM(NUM-TEXT)
                 "</b> day streak</p></div>"
                 DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP
           END-IF
           MOVE TODAY TO WANT-DATE
           PERFORM FIND-LOG
           STRING "<button class='check" DELIMITED BY SIZE
             INTO RESPONSE-BODY WITH POINTER BP
           IF FOUND-AT > 0
               STRING " on" DELIMITED BY SIZE
                 INTO RESPONSE-BODY WITH POINTER BP
           END-IF
           STRING "' name='t' value='" WANT-HABIT "-" TODAY
             "' aria-label='Done today'>&#10003;</button></div>"
             "<div class='week'>"
             DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP

      *>   the last seven days, oldest first
           PERFORM VARYING I FROM 6 BY -1 UNTIL I < 0
               COMPUTE DAY-INT = TODAY-INT - I
               COMPUTE WANT-DATE = FUNCTION DATE-OF-INTEGER(DAY-INT)
               MOVE WANT-DATE TO A-DATE
               MOVE A-DAY TO DAY-TEXT
               COMPUTE WEEKDAY = FUNCTION MOD(DAY-INT - 1, 7) + 1
               PERFORM FIND-LOG
               STRING "<button class='day" DELIMITED BY SIZE
                 INTO RESPONSE-BODY WITH POINTER BP
               IF FOUND-AT > 0
                   STRING " on" DELIMITED BY SIZE
                     INTO RESPONSE-BODY WITH POINTER BP
               END-IF
               IF I = 0
                   STRING " today" DELIMITED BY SIZE
                     INTO RESPONSE-BODY WITH POINTER BP
               END-IF
               STRING "' name='t' value='" WANT-HABIT "-" WANT-DATE "'>"
                 "<small>" DAY-LETTERS(WEEKDAY:1) "</small>"
                 "<b>" FUNCTION TRIM(DAY-TEXT) "</b></button>"
                 DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP
           END-PERFORM

           STRING "</div><button class='del' formaction='/delete'"
             " name='t' value='" WANT-HABIT "'>Remove</button>"
             "</form></li>"
             DELIMITED BY SIZE INTO RESPONSE-BODY WITH POINTER BP.

       COUNT-STREAK.
      *>   walk backwards from today until a day is missing.
      *>   an unticked today does not break the streak (yet).
           MOVE 0 TO STREAK
           MOVE TODAY-INT TO DAY-INT
           MOVE TODAY TO WANT-DATE
           PERFORM FIND-LOG
           IF FOUND-AT = 0 SUBTRACT 1 FROM DAY-INT END-IF
           PERFORM WITH TEST AFTER UNTIL FOUND-AT = 0
               COMPUTE WANT-DATE = FUNCTION DATE-OF-INTEGER(DAY-INT)
               PERFORM FIND-LOG
               IF FOUND-AT > 0
                   ADD 1 TO STREAK
                   SUBTRACT 1 FROM DAY-INT
               END-IF
           END-PERFORM.

       PICK-EMOJI.
      *>   a very serious artificial intelligence
           MOVE FUNCTION UPPER-CASE(H-NAME(J)) TO UPPER-NAME
           MOVE "&#10024;" TO EMOJI
           PERFORM CHECK-WORD-READ
           PERFORM CHECK-WORD-RUN
           PERFORM CHECK-WORD-WATER
           PERFORM CHECK-WORD-CODE
           PERFORM CHECK-WORD-SLEEP
           PERFORM CHECK-WORD-MEDITATE
           PERFORM CHECK-WORD-WALK
           PERFORM CHECK-WORD-GYM.

       CHECK-WORD-READ.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "READ" ALL "BOOK"
           IF HITS > 0 MOVE "&#128218;" TO EMOJI END-IF.
       CHECK-WORD-RUN.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "RUN" ALL "JOG"
           IF HITS > 0 MOVE "&#127939;" TO EMOJI END-IF.
       CHECK-WORD-WATER.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "WATER" ALL "DRINK"
           IF HITS > 0 MOVE "&#128167;" TO EMOJI END-IF.
       CHECK-WORD-CODE.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "CODE" ALL "COBOL"
           IF HITS > 0 MOVE "&#128187;" TO EMOJI END-IF.
       CHECK-WORD-SLEEP.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "SLEEP" ALL "BED"
           IF HITS > 0 MOVE "&#128564;" TO EMOJI END-IF.
       CHECK-WORD-MEDITATE.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "MEDITAT" ALL "BREATH"
           IF HITS > 0 MOVE "&#129496;" TO EMOJI END-IF.
       CHECK-WORD-WALK.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "WALK" ALL "STEPS"
           IF HITS > 0 MOVE "&#128694;" TO EMOJI END-IF.
       CHECK-WORD-GYM.
           MOVE 0 TO HITS
           INSPECT UPPER-NAME TALLYING HITS FOR ALL "GYM" ALL "LIFT" ALL "PUSH"
           IF HITS > 0 MOVE "&#127947;" TO EMOJI END-IF.

       EMIT-ESCAPED-NAME.
      *>   never trust user input, not even in 1959
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 120
                   OR H-NAME(J)(K:) = SPACES
               MOVE H-NAME(J)(K:1) TO ONE-CHAR
               EVALUATE ONE-CHAR
                   WHEN "<"
                       STRING "&lt;" DELIMITED BY SIZE
                         INTO RESPONSE-BODY WITH POINTER BP
                   WHEN ">"
                       STRING "&gt;" DELIMITED BY SIZE
                         INTO RESPONSE-BODY WITH POINTER BP
                   WHEN "&"
                       STRING "&amp;" DELIMITED BY SIZE
                         INTO RESPONSE-BODY WITH POINTER BP
                   WHEN "'"
                       STRING "&#39;" DELIMITED BY SIZE
                         INTO RESPONSE-BODY WITH POINTER BP
                   WHEN OTHER
                       STRING ONE-CHAR DELIMITED BY SIZE
                         INTO RESPONSE-BODY WITH POINTER BP
               END-EVALUATE
           END-PERFORM.
