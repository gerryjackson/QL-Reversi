( REVERSI stand alone version 1.8   copyright (c) 1985 G.W.Jackson )

( Last modified: )

( 21/10/86  Version 1.8 Definition of #main altered to suit monitor )

( 15/7/86   Version 1.7 created, minor bug fixes, 1.6 never issued )

( 26/1/86   All piece drawing done in #BOARD to hopefully make UK and USA )
(           versions the same, X_CEN, Y_CEN, SET_SCALE, DRAW_PIECE changed )
( 8/2/86    Option added to kill sound  )
(           Cursor movement on board slowed down )
(           Error after H vs H followed by exchange with White to move, )
(                 when a move was missed is fixed  )
(           OTHER_SKILL initialised to 2   )
( 10/2/86   Start of game sequence changed to go straight to PLAYER_MOVE )

CLS 3 1 CSIZE CR .(       LOADING REVERSI) CR 1 0 CSIZE
: #CON 2CONSTANT  DOES> 2@ 2DUP #IN 2! #OUT 2! ;
0 0 #CON #TITLE     ( loaded with channel ID's below )
0 0 #CON #MOVES
0 0 #CON #SCORE
0 0 #CON #MESS
0 0 #CON #BOARD
0 0 #CON #BIG
#OUT 2@ #CON #MAIN

: OPEN_WIN 32768 - 0 32768 2@ D+ 0 1 TRAP2 DUP ?ERROR ; ( ad --- ID )

: WIN_WORD CREATE BL WORD 0 HERE ! HERE 1+ OVER C@ 1+ CMOVE
                  HERE @ 2+ ALLOT
           DOES>  OPEN_WIN ;

WIN_WORD TITLE_W SCR_180X50A52X44
WIN_WORD MOVES_W CON_180X80A52X94
WIN_WORD SCORE_W SCR_180X24A52X174
WIN_WORD MESS_W  CON_420X54A52X199
WIN_WORD BOARD_W CON_202X136A268X44
WIN_WORD BIG_W   SCR_512X256A0X0
win_word main_w  con_460x244a32x12

: OPEN_WINDOWS main_w  [ ' #MAIN >BODY ] LITERAL 2!
               BIG_W   [ ' #BIG   >BODY ] LITERAL 2!
               8 MODE #BIG 5 PAPER CLS #MAIN
               [ ' #BIG >BODY ] LITERAL 2@ CLOSE
               TITLE_W [ ' #TITLE >BODY ] LITERAL 2!
               MOVES_W [ ' #MOVES >BODY ] LITERAL 2!
               SCORE_W [ ' #SCORE >BODY ] LITERAL 2!
               MESS_W  [ ' #MESS  >BODY ] LITERAL 2!
               BOARD_W [ ' #BOARD >BODY ] LITERAL 2!   ;

: INIT_SCR DUP PAPER STRIP INK DUP BORDER ;
: DRAW_SCR #MAIN  0 1 5 INIT_SCR CLS
         #TITLE 0 7 1 INIT_SCR CLS
         #MOVES 0 1 6 INIT_SCR CLS
         #SCORE 0 7 2 INIT_SCR CLS 56 2 CURSOR ." SCORE"
         #MESS  1 1 5 INIT_SCR CLS
         #BOARD 0 1 4 INIT_SCR CLS      ;
: DRAW_SIDES  #MAIN
              56 173  39 DO 222 I CURSOR DUP EMIT 1- 17
                       +LOOP DROP
              65 430 243 DO I 173 CURSOR DUP EMIT 1+ 25
                       +LOOP DROP
              173 33 DO 1 202 1 237 I BLOCK_FILL 17
                       +LOOP
              444 237 DO 1 2 136 I 33 BLOCK_FILL 25
                       +LOOP 2 0 CSIZE  ;
HEX
: FP DUP IF 0 SWAP -10 BEGIN OVER 4000 U< WHILE
     SWAP 2* SWAP 1- REPEAT 81F + ELSE 0 0 THEN ;
DECIMAL
VARIABLE C_COL    VARIABLE P_COL   VARIABLE COLOUR
VARIABLE P_SCORE  VARIABLE C_SCORE VARIABLE MEN
VARIABLE COMP     VARIABLE PLAYER  VARIABLE MEN_FLIPPED
VARIABLE NEW_MOVE VARIABLE QFLAG   VARIABLE HEAP
VARIABLE MOVE_NO  VARIABLE PRUNE
7 CONSTANT WHITE  0 CONSTANT BLACK  92 CONSTANT SIZE
0 CONSTANT CURTAIL      ( to interrupt computer's move )
: ARRAY CREATE ALLOT DOES> + ;
64 ARRAY GAME  CREATE START_BOARD 92 ALLOT
: FPARRAY CREATE DOES> SWAP 6 * + ;
: FP, FP , , ,  ;
FPARRAY X_CEN  11 FP,  29 FP,  48 FP,  66 FP,
               85 FP, 103 FP, 122 FP, 140 FP,
FPARRAY Y_CEN   6 FP,  23 FP,  41 FP,  57 FP,
               75 FP,  92 FP, 109 FP, 126 FP,
CREATE PARAMETERS 0 FP, 6 FP, 1 FP, 0 FP, 0 FP,

: SET_SCALE #BOARD HEAP @ 500 + 18 0 FILL
            [ 136 FP SWAP ] LITERAL LITERAL [ DROP ]
            HEAP @ 512 + 2!
            HEAP @ 500 + SCALE #MAIN  ;

HEX CREATE SQU_VALUES ( values assigned to each square )
1010 , 1010 , 1010 , 1010 , 1010 , 2D19 , 211F , 1F21 ,
192D , 1019 , 111B , 1B1B , 1B11 , 1910 , 211B , 211F ,
1F21 , 1B21 , 101F , 1B1F , 0007 , 1F1B , 1F10 , 1F1B ,
1F07 , 001F , 1B1F , 1021 , 1B21 , 1F1F , 211B , 2110 ,
1911 , 1B1B , 1B1B , 1119 , 102D , 1921 , 1F1F , 2119 ,
2D10 , 1010 , 1010 , 1010 , 1010 , 1010 , DECIMAL
: BD_ARRAY CREATE SIZE ALLOT
           DOES>  ( OVER SIZE 1- U> IF )
         ( ." Board array access error " QUIT THEN ) + ;
BD_ARRAY P0_BOARD  BD_ARRAY P1_BOARD  BD_ARRAY P2_BOARD
BD_ARRAY P3_BOARD  BD_ARRAY P4_BOARD  BD_ARRAY P5_BOARD
BD_ARRAY P6_BOARD
VARIABLE BOARD_AD
: BOARD BOARD_AD @ EXECUTE ; ' P0_BOARD BOARD_AD !
: SCORE 0 P_SCORE ! 0 C_SCORE ! 81 10 DO
    I BOARD C@ DUP 16 < IF P_COL @ = IF 1 P_SCORE
    ELSE 1 C_SCORE THEN +! ELSE DROP THEN LOOP
    #SCORE P_SCORE @ C_SCORE @ 2DUP + MEN !
    C_COL @ WHITE =
    IF SWAP THEN
    24 14 CURSOR . 120 14 CURSOR . ;
: PUT_COORD PARAMETERS + 6 CMOVE  ;
: DRAW_PIECE #BOARD DUP 10 - 9 /MOD
       Y_CEN 18 PUT_COORD X_CEN 24 PUT_COORD BOARD C@
       INK PARAMETERS FILL_ON CIRCLE FILL_OFF 1 INK ;
EXVEC: DRAW_MAN  ASSIGN DRAW_MAN TO-DO DRAW_PIECE
VARIABLE HEAP_BASE
VARIABLE SCORE0 2 ALLOT    VARIABLE SCORE1
VARIABLE SCORE2            VARIABLE SCORE3
VARIABLE SCORE4            VARIABLE SCORE5
VARIABLE MOVE_AD           VARIABLE SIZE_PTR
VARIABLE OUT               VARIABLE SKILL 2 SKILL !
VARIABLE C_TAB             VARIABLE P_TAB
VARIABLE P_BEST?           VARIABLE P_BEST
VARIABLE OLD_SKILL         VARIABLE RED_PIECES
VARIABLE P0_MOVES    VARIABLE P0_SIZE    VARIABLE P0_PTR
VARIABLE P1_MOVES    VARIABLE P1_SIZE    VARIABLE P1_PTR
VARIABLE P2_MOVES    VARIABLE P2_SIZE    VARIABLE P2_PTR
VARIABLE P3_MOVES    VARIABLE P3_SIZE    VARIABLE P3_PTR
VARIABLE P4_MOVES    VARIABLE P4_SIZE    VARIABLE P4_PTR
VARIABLE P5_MOVES    VARIABLE P5_SIZE    VARIABLE P5_PTR
VARIABLE P6_MOVES    VARIABLE P6_SIZE    VARIABLE P6_PTR
VARIABLE GAME_MODE   VARIABLE I_FLAG
VARIABLE OTHER_SKILL       2 OTHER_SKILL !
VARIABLE B_SKILL    VARIABLE W_SKILL
VARIABLE X_FLAG     VARIABLE S_FLAG   -1 S_FLAG !

: DRAW_COLS   #TITLE 0 90 10 0 40 BLOCK_FILL 7 90 10 90 40 BLOCK_FILL ;
: INIT-BOARD #MAIN DRAW_SCR DRAW_SIDES
        ['] P0_BOARD BOARD_AD !
        SQU_VALUES 0 BOARD SIZE CMOVE
        SQU_VALUES START_BOARD SIZE CMOVE
        0 MOVE_NO ! -1 QFLAG !
        40 DRAW_PIECE 41 DRAW_PIECE 49 DRAW_PIECE
        50 DRAW_PIECE
        DRAW_COLS    ;

: EVALUATE MEN @ 11 / 3 - 1 MAX MEN_FLIPPED @ *
      NEW_MOVE @
      IF 3 PICK BOARD C@ 16 - +
         0 NEW_MOVE ! 1 SIZE_PTR @ +!
         3 PICK MOVE_AD @ @ C! 4 MOVE_AD @ +!
         MOVE_AD @ @ 2- !
      ELSE MOVE_AD @ @ 2- +!
      THEN  ;

: OFF 2+ 2@ CLOSE ;

: CLOSE_ALL ['] #TITLE OFF
            ['] #MOVES OFF
            ['] #SCORE OFF
            ['] #MESS  OFF
            ['] #BOARD OFF  ;

: HEADER    #MAIN 2 STRIP 7 INK
            2 420 28 20 0  BLOCK_FILL
            2 1 CSIZE  46  4 CURSOR ." REVERSI"
            2 0 CSIZE 142 12 CURSOR ." from"
            2 1 CSIZE 204  4 CURSOR ." Digital Precision"
            5 STRIP 1 INK 2 0 CSIZE ;

: SAVE_MOVE DUP C@ COLOUR @ IF 128 + THEN
            MOVE_NO @ GAME C! 1 MOVE_NO +! ;
: SET_TABS 2 10 C_COL @ BLACK = IF SWAP THEN
           C_TAB ! P_TAB ! #TITLE 0 15 CURSOR
           GAME_MODE @ ?DUP
           IF 1 = IF ."  Human  Human"
                  ELSE ."  QL/"  B_SKILL @ . SPACE
                       ."   QL/" W_SKILL @ . SPACE
                  THEN
           ELSE
             SKILL @ C_COL @ BLACK =
             IF   ."  QL/" . ."   Human"
             ELSE ."  Human   QL/" .
           THEN  THEN ;
: CLRMSG #MESS 1 1 BORDER CLS 0 1 AT ;

: .SQUARE 10 - 9 /MOD SWAP 65 + EMIT 49 + EMIT ;

: COMP_COL C_COL @ COLOUR ! ;
: PLAY_COL P_COL @ COLOUR ! ;
: LOOK 2 0 CSIZE #MAIN 0 0 AT P0_MOVES @ 20 0 DO CR DUP
       C@ .  2+ DUP @ . 2+ LOOP DROP ;
: INITP0 ['] P0_BOARD BOARD_AD ! ;
EXVEC: OPERATION ( used to flip pieces or make a move )
: CHECK_1_WAY DUP >R 2DUP + BOARD C@ DUP 16 <
     IF COLOUR @ <>
        IF 1 MEN_FLIPPED !
           BEGIN R@ + 2DUP + BOARD C@ DUP 16 <
              IF COLOUR @ =
                 IF R@ OPERATION
                 ELSE 1 MEN_FLIPPED +! 0 THEN
              ELSE DROP -1 THEN
           UNTIL THEN DUP THEN 2DROP R>  ;
: CHECK_2_WAYS CHECK_1_WAY NEGATE CHECK_1_WAY ;
: CHECK_8_WAYS 1  CHECK_2_WAYS 8 - CHECK_2_WAYS
               1+ CHECK_2_WAYS  2+ CHECK_2_WAYS 2DROP  ;
: GEN_MOVES 80 10
           DO I 8 + I
              DO -1 NEW_MOVE ! I BOARD C@ 16 >
                 IF I CHECK_8_WAYS THEN LOOP 9 +LOOP ;
: FINISH #MESS 5 1 BORDER CLS #MAIN 0 20 AT ;
: ABANDON   FINISH  2 0 CSIZE
            ." REVERSI terminated "  CR
            ." Control C to return to SuperBASIC"
            CLOSE_ALL  BYE
            ASSIGN ERROR TO-DO (ERROR)  QUIT   ;
: CR? 10 = IF CR SPACE THEN ;

: MYMOVE SCORE0 C@ DUP 0>
         IF 10 - 9 /MOD SWAP #MOVES C_TAB @ TAB
            65 + EMIT  49 + EMIT C_TAB @ CR?
            S_FLAG @ IF 10 2000 BEEP THEN
         ELSE C_COL @ WHITE = IF #MOVES CR SPACE THEN 
              DROP CLRMSG ."    I can't go" 50 5000 BEEP
              0 OLD_SKILL ! 200 SUSPEND_ME THEN ;
: FULL P_SCORE @ C_SCORE @ + 64 = ;
: FLIP_PIECES OVER 3 PICK + OVER 0< - 3 PICK
       DO RED_PIECES @ 0= IF COLOUR @ I BOARD C! THEN
          I DRAW_MAN DUP +LOOP ;
: FLIP ASSIGN OPERATION TO-DO FLIP_PIECES C@ ?DUP
       IF CHECK_8_WAYS  THEN ;
: MAKE_MOVE ASSIGN DRAW_MAN TO-DO DROP FLIP ;
: DRAW_ALL_MEN #BOARD CLS DRAW_SIDES 80 10 DO I 8 + I
     DO I BOARD C@ 16 < IF I DRAW_PIECE THEN
     LOOP 9 +LOOP SCORE ;
VARIABLE SQUARE   VARIABLE X     VARIABLE Y  VARIABLE YG_FLAG
VARIABLE GAME_START        ( Marks first move of game )

: SET_YG_FLAG -1 YG_FLAG ! ;
: CL_YG_FLAG   0 YG_FLAG ! ;

: SWAP_SIDES
     P_COL @ C_COL @ P_COL ! C_COL ! 1 PLAYER ! 1 COMP !
     P_TAB @ C_TAB @ P_TAB ! C_TAB ! SET_TABS 0 -1 CL_YG_FLAG
     GAME_MODE @ 0= X_FLAG !  ( Only set swapped flag if H vs QL or QL vs H )
     ;

: QUIT_GAME 0 QFLAG ! 0 PLAYER ! 0 COMP ! 0 -1
            CL_YG_FLAG   ;
: INIT_CSOR 40 SQUARE ! 83 X ! 73 Y ! ;
: SET_SQU X @ 25 / 73 + Y @ 17 / 9 * - SQUARE !
          #BOARD X @ Y @ CURSOR     ;
: L/R X @ + 200 + 200 MOD X ! SET_SQU CURSOR_ON 10 SUSPEND_ME ;
: LEFT -25 L/R ;
: RIGHT 25 L/R ;
: U/D Y @ + 136 + 136 MOD Y ! SET_SQU CURSOR_ON 10 SUSPEND_ME ;
: UP  -17 U/D ;
: DOWN 17 U/D ;
: PUT_PIECE #MAIN SQUARE @ BOARD C!
            SQUARE @ DRAW_PIECE #BOARD ;
: PUT_BLACK 0 PUT_PIECE ;
: PUT_WHITE 7 PUT_PIECE ;

: EMPTY 4 PUT_PIECE SQU_VALUES SQUARE @ + C@ DUP 16 <
        IF DROP 37 THEN SQUARE @ BOARD C! ;

: CLEAR SQU_VALUES 0 BOARD SIZE CMOVE
        DRAW_ALL_MEN #BOARD INIT_CSOR ;

: .CSOR_POS SET_SQU #MESS 24 1 AT SQUARE @ .SQUARE ;

: YOUR_GO$ CLRMSG ."  Your move - cursor at    " CR CR
                  ."  press "
      GAME_START @
      IF                   ." X to play white" CR
                  ."        "
      THEN
                           ." O to list options"    ;

: SET_PRUNE          ( Sets level of pruning according to skill level )
      SKILL @ DUP 5 = SWAP 7 = OR
      IF 5 ELSE 100 THEN
      PRUNE !    ;

: SKILL? CLRMSG ." Level of skill ( 1 to 9 )" CR 0
   BEGIN DROP CURSOR_ON KEY CURSOR_OFF 49 - DUP 9 U<
   UNTIL 1+ SKILL ! SET_TABS
   SET_PRUNE SET_YG_FLAG ;

: MOVE_CSOR #BOARD X @ Y @ CURSOR .CSOR_POS ;

: RETRACT START_BOARD 0 P0_BOARD SIZE CMOVE INITP0
    0 OLD_SKILL ! -1 MOVE_NO +! MOVE_NO @ 0<
    IF 0 MOVE_NO ! 100 5000 BEEP CLRMSG
       5 2 AT ." At starting position" 150 SUSPEND_ME
    ELSE MOVE_NO @ 0> IF MOVE_NO @ 0
    DO I GAME C@ DUP 127 > IF WHITE ELSE BLACK THEN
       COLOUR ! 127 AND PLAYER C! PLAYER MAKE_MOVE
    LOOP THEN SWAP_SIDES 2DROP DRAW_ALL_MEN #MOVES CLS
    THEN PLAY_COL 0 ASSIGN OPERATION TO-DO EVALUATE
    SET_YG_FLAG SET_SQU    ;

: .BEST CLRMSG OLD_SKILL @ 1 >
        IF P_BEST @ ."  Your best move is "
             .SQUARE
        ELSE ."  I don't know"
        THEN 250 SUSPEND_ME 0 SET_YG_FLAG ;

: LOAD_XY   DUP  1-   9 MOD  25 * 8 + X !
            8  SWAP   9 /  - 17 * 5 + Y !
            SET_SQU    ;

: SET_ROW   7 SWAP - 17 * 5 + Y ! SET_SQU  ;

: SET_COL   25 * 8 + X ! MOVE_CSOR  ;

: REDRAW DRAW_SCR DRAW_ALL_MEN HEADER DRAW_COLS
         SET_TABS  SET_YG_FLAG   ;

: GET_KEY CURSOR_ON KEY DUP 97 - 26 U<
          IF BL - THEN
          CURSOR_OFF ;

: SET_HELP CLRMSG 0 0 AT ."  Arrow keys move the cursor"
           CR ."  W or B places a white/black piece"
           CR ."  N      clears the square"
           CR ."  C      clears the board"
           CR ."  ESC    to terminate"   ;

: SET_POSITION #MOVES CLS INITP0 INIT_CSOR SET_HELP
      0 GAME_START !             ( Can't make random first move )
      DRAW_ALL_MEN #BOARD
      BEGIN
            X @ Y @ CURSOR GET_KEY DUP
            CASE 192 OF   LEFT      ENDOF
                 200 OF   RIGHT     ENDOF
                 208 OF   UP        ENDOF
                 216 OF   DOWN      ENDOF
                  87 OF 7 PUT_PIECE ENDOF
                  66 OF 0 PUT_PIECE ENDOF
                  78 OF   EMPTY     ENDOF
                  67 OF   CLEAR     ENDOF
                  27 OF 50 SUSPEND_ME ENDOF
                 DEFAULT
            ENDCASE      27 =
      UNTIL SCORE 0 BOARD START_BOARD SIZE CMOVE
                  0 MOVE_NO !  SET_YG_FLAG ;

: .OPTIONS CLRMSG 0 0 AT
       ."  X exchange sides  ? best move" CR
       ."  S set up board    Q quit game" CR
       ."  R retract move    L skill level" CR
       ."  T toggle sound    W draw screen" CR
       ."  ESC quit REVERSI  M set mode"      ;

: ENTER    SQUARE @ -1
           #MOVES P_TAB @ TAB
           SQUARE @ .SQUARE  CL_YG_FLAG  ;

: ?2SKILLS  CLRMSG ." Enter skill levels ( 1 to 9 )" CR
                   ."       Black "
            GET_KEY 48 - 1 MAX 9 MIN DUP B_SKILL ! DUP . CR 
                   ."       White "
            GET_KEY 48 - 1 MAX 9 MIN DUP W_SKILL ! DUP .
            COLOUR @ BLACK = IF SWAP THEN
            OTHER_SKILL ! SKILL ! 1 PLAYER !
            0   -1   2  CL_YG_FLAG
            40 SUSPEND_ME   ;

: MIXED                       ( To set H vs QL or QL vs H )
      -1 X_FLAG !             ( To ensure QL gets a go after H's move )
       0                      ( To give H a move next )
       0                      ( For GAME_MODE )
      -1    ;                 ( To leave current loop )


: ?MODE     CLRMSG SET_YG_FLAG
            ."   H  for Human v Human" CR
            ."   Q  for   QL  v  QL"   CR
            ."   Enter/Space for other"
            BEGIN
              GET_KEY
              CASE 72 OF      0 1 -1 ENDOF
                   81 OF ?2SKILLS -1 ENDOF
                   10 OF MIXED       ENDOF
                   BL OF MIXED       ENDOF
                   DEFAULT         0
              ENDCASE
            UNTIL
            GAME_MODE ! SET_TABS  50 SUSPEND_ME ;

: ?OPTION   CL_YG_FLAG
            DUP 79 = IF DROP .OPTIONS GET_KEY SET_YG_FLAG THEN
            CASE  192 OF LEFT   0   ENDOF
                  200 OF RIGHT  0   ENDOF
                  208 OF UP     0   ENDOF
                  216 OF DOWN   0   ENDOF
                   10 OF ENTER      ENDOF
                   BL OF ENTER      ENDOF
                   88 OF SWAP_SIDES ENDOF
                   77 OF ?MODE      ENDOF
                   81 OF QUIT_GAME  ENDOF
                   27 OF ABANDON    ENDOF
                   82 OF RETRACT    ENDOF
                   63 OF .BEST      ENDOF
                   76 OF SKILL? 0   ENDOF
                   83 OF SET_POSITION 0 ENDOF
                   84 OF -1 S_FLAG @ - S_FLAG ! 0 ENDOF
                   87 OF REDRAW   0 ENDOF
                   DEFAULT 0
            ENDCASE
            YG_FLAG @ IF YOUR_GO$ THEN     ;

: GET_MOVE
      LOAD_XY YOUR_GO$ 
      BEGIN
        .CSOR_POS
        #BOARD GET_KEY
        49 - DUP 7 U>
        IF
          16 - DUP 7 U>
          IF
            65 + ?OPTION
          ELSE
            SET_COL 0
          THEN
        ELSE
          SET_ROW  0
        THEN
      UNTIL  ;

: M$BW CLRMSG ." Do you wish to play " CR
       ." black or white ? ( B or W ) " WHITE BLACK
       GET_KEY CLRMSG 66 =
       IF SWAP THEN OVER COLOUR ! C_COL ! P_COL ! ;
: DRAW_RED DUP BOARD C@ SWAP 2 OVER BOARD C!
           DUP DRAW_PIECE BOARD C! ;
: DRAW_THEM DUP C@
       IF ASSIGN DRAW_MAN TO-DO DRAW_RED
          -1 RED_PIECES ! DUP FLIP
          #MOVES 150 SUSPEND_ME THEN
       ASSIGN DRAW_MAN TO-DO DRAW_PIECE
       0 RED_PIECES ! FLIP   ;
: DRAW_MEN DRAW_THEM SCORE #MOVES ;
: COMP-MOVE ASSIGN OPERATION TO-DO EVALUATE GEN_MOVES
            5 KEYROW 4 = DUP
            IF GAME_MODE @ 1 <> IF -1 I_FLAG ! THEN THEN
            ['] CURTAIL >BODY ! ;

: GENP0_MOVES INITP0 HEAP_BASE @ HEAP !
    HEAP @ DUP P0_MOVES ! P0_PTR ! P0_PTR MOVE_AD !
    0 P0_SIZE ! P0_SIZE SIZE_PTR ! 0 0 P0_MOVES @ 2!
    COMP-MOVE P0_PTR @ 2+ 2+ DUP P1_MOVES ! HEAP !  ;
: GENP1_MOVES ['] P1_BOARD BOARD_AD !
    P1_MOVES @ P1_PTR ! P1_PTR MOVE_AD !
    0 P1_SIZE ! P1_SIZE SIZE_PTR ! 0 0 P1_MOVES @ 2!
    COMP-MOVE P1_PTR @ 2+ 2+ DUP P2_MOVES ! HEAP !  ;
: GENP2_MOVES ['] P2_BOARD BOARD_AD !
    P2_MOVES @ P2_PTR ! P2_PTR MOVE_AD !
    0 P2_SIZE ! P2_SIZE SIZE_PTR ! 0 0 P2_MOVES @ 2!
    COMP-MOVE P2_PTR @ 2+ 2+ DUP P3_MOVES ! HEAP !  ;
: GENP3_MOVES ['] P3_BOARD BOARD_AD !
    P3_MOVES @ P3_PTR ! P3_PTR MOVE_AD !
    0 P3_SIZE ! P3_SIZE SIZE_PTR ! 0 0 P3_MOVES @ 2!
    COMP-MOVE P3_PTR @ 2+ 2+ P4_MOVES ! ;
: GENP4_MOVES ['] P4_BOARD BOARD_AD !
    P4_MOVES @ P4_PTR ! P4_PTR MOVE_AD !
    0 P4_SIZE ! P4_SIZE SIZE_PTR ! 0 0 P4_MOVES @ 2!
    COMP-MOVE P4_PTR @ 2+ 2+ P5_MOVES ! ;
: GENP5_MOVES ['] P5_BOARD BOARD_AD !
    P5_MOVES @ P5_PTR ! P5_PTR MOVE_AD !
    0 P5_SIZE ! P5_SIZE SIZE_PTR ! 0 0 P5_MOVES @ 2!
    COMP-MOVE P5_PTR @ 2+ 2+ P6_MOVES ! ;
: GENP6_MOVES ['] P6_BOARD BOARD_AD ! P6_MOVES @
    P6_PTR ! P6_PTR MOVE_AD ! 0 P6_SIZE !
    P6_SIZE SIZE_PTR ! 0 0 P6_MOVES @ 2! COMP-MOVE ;
: P1_SCORE P0_PTR @ 2+ @ P1_PTR @ 2+ @ - ;
: P2_SCORE P1_SCORE P2_PTR @ 2+ @ + ;
: P3_SCORE P2_SCORE P3_PTR @ 2+ @ - ;
: P4_SCORE P3_SCORE P4_PTR @ 2+ @ + ;
: P5_SCORE P4_SCORE P5_PTR @ 2+ @ - ;
: .MOVE AT P0_PTR @ C@ .SQUARE  ;

: BEST$ SKILL @ 2 > IF #MESS 24 1 .MOVE
            ."  (" SCORE0 2+ @ 4 .R ."  )  " THEN ;
: MOVE$ SKILL @ 2 > IF #MESS 24 3 .MOVE  THEN ;
: TEST_P4_SCORE SCORE4 @ DUP SCORE3 @ MIN SCORE3 !
                SCORE2 @ <  CURTAIL OR ;
: TEST_P3_SCORE SCORE3 @ DUP SCORE2 @ MAX SCORE2 !
                SCORE1 @ >  CURTAIL OR ;
: TEST_P2_SCORE SCORE2 @ DUP SCORE1 @ < IF DUP SCORE1 !
            P1_PTR @ C@ P_BEST? !  THEN
            SCORE0 2+ @ <  CURTAIL OR ;
: TEST_P1_SCORE SCORE1 @ SKILL @ 2 > IF 24 2 .MOVE
            26 TAB DUP ."  (" 4 .R ."  )" THEN
    SCORE0 2+ @ 2DUP = IF 2DROP TIME DROP 1 AND 0 THEN >
    IF SCORE1 @ P0_PTR @ @ SCORE0 2! BEST$
       P_BEST? @ P_BEST ! THEN ;
: TEST_P5_SCORE SCORE5 @ DUP SCORE4 @ MAX SCORE4 !
                SCORE3 @ > CURTAIL OR ;
: TEST_P6_SCORE P5_SCORE + DUP SCORE5 @ MIN SCORE5 !
                SCORE4 @ < CURTAIL OR ;
( Othello extensions - words to sort moves )
EXVEC: CMP ( used to select order for sort )
: HI/LO    ( n1 ad --- n1 ad n2 ) ( n2 = position )
     DUP 2+ 2+ OVER 3 PICK DUP 1 >
     IF 1 DO >R R@ 2+ @ OVER 2+ @ CMP
             IF R> DROP DUP >R THEN 2+ 2+ R> LOOP
     ELSE DROP THEN SWAP DROP   ;
: DESC ASSIGN CMP TO-DO < ;
: ASC  ASSIGN CMP TO-DO > ;
: SORT OVER 1 > IF OVER 1 DO HI/LO >R DUP 2@ R@ 2@
       4 PICK 2! R> 2! SWAP 1- SWAP 2+ 2+ LOOP
       THEN 2DROP ;
: SORT_HI DESC SORT ;
: SORT_LO ASC SORT ;
: ADD_SCORE P0_PTR @ 2+ @ P1_PTR @ 2+ @ - +
            DUP SCORE1 @ MIN SCORE1 !   ;
: P0->P1 ['] P0_BOARD >BODY ['] P1_BOARD DUP BOARD_AD !
         >BODY SIZE CMOVE ;
: P1->P2 ['] P1_BOARD >BODY ['] P2_BOARD DUP BOARD_AD !
         >BODY SIZE CMOVE ;
: P2->P3 ['] P2_BOARD >BODY ['] P3_BOARD DUP BOARD_AD !
         >BODY SIZE CMOVE ;
: P3->P4 ['] P3_BOARD >BODY ['] P4_BOARD DUP BOARD_AD !
         >BODY SIZE CMOVE ;
: P4->P5 ['] P4_BOARD >BODY ['] P5_BOARD DUP BOARD_AD !
         >BODY SIZE CMOVE ;
: P5->P6 ['] P5_BOARD >BODY ['] P6_BOARD DUP BOARD_AD !
         >BODY SIZE CMOVE ;
: SUB_S @ 2+ OVER NEGATE SWAP +! ;
EXVEC: SUB_SIZE ASSIGN SUB_SIZE TO-DO SUB_S
: GET_P0_MOVES 32768 0 SCORE0 2! COMP_COL GENP0_MOVES
      P0_SIZE @ DUP P0_MOVES @ DUP P0_PTR ! SORT_HI
      DUP 1 > SKILL @ 1 > AND  ;
: GET_P1_MOVES COMP_COL P0->P1 P0_PTR @ MAKE_MOVE
      PLAY_COL GENP1_MOVES P1_SIZE @ P0_PTR SUB_SIZE
      DUP P1_MOVES @ DUP C@ P_BEST? ! DUP
      P1_PTR ! SORT_HI 32767 SCORE1 ! SKILL @ 2 > ;
: GET_P2_MOVES PLAY_COL P1->P2 P1_PTR @ MAKE_MOVE
      COMP_COL GENP2_MOVES P2_SIZE @ P1_PTR SUB_SIZE
      DUP P2_MOVES @ DUP
      P2_PTR ! SORT_HI 32768 SCORE2 ! SKILL @ 3 > ;
: GET_P3_MOVES COMP_COL P2->P3 P2_PTR @ MAKE_MOVE
      PLAY_COL GENP3_MOVES P3_SIZE @ P2_PTR SUB_SIZE
      DUP P3_MOVES @ DUP
      P3_PTR ! SORT_HI 32767 SCORE3 ! SKILL @ 4 > ;
: GET_P4_MOVES PLAY_COL P3->P4 P3_PTR @ MAKE_MOVE
      COMP_COL GENP4_MOVES P4_SIZE @ P3_PTR SUB_SIZE
      DUP P4_MOVES @ DUP
      P4_PTR ! SORT_HI 32768 SCORE4 ! SKILL @ 6 > ;
: GET_P5_MOVES COMP_COL P4->P5 P4_PTR @ MAKE_MOVE
      PLAY_COL GENP5_MOVES P5_SIZE @ P4_PTR SUB_SIZE
      DUP P5_MOVES @ DUP
      P5_PTR ! SORT_HI 32767 SCORE5 ! SKILL @ 8 > ;
: BEST_P6_MOVE PLAY_COL P5->P6 P5_PTR @ MAKE_MOVE
      COMP_COL GENP6_MOVES P6_SIZE @ P5_PTR SUB_SIZE
      P6_MOVES @ HI/LO
      2+ @ >R 2DROP R>  ;
: PRUNE? 1 MAX PRUNE @ MIN 0 ;
: TRY_456_MOVES
      DO GET_P4_MOVES IF PRUNE?
         DO GET_P5_MOVES IF 1 MAX 0
            DO BEST_P6_MOVE TEST_P6_SCORE IF LEAVE THEN
               4 P5_PTR +!
            LOOP
            ELSE DROP P5_SCORE SCORE5 ! THEN
                 TEST_P5_SCORE IF LEAVE THEN 4 P4_PTR +!
         LOOP
         ELSE DROP P4_SCORE SCORE4 ! THEN
              TEST_P4_SCORE IF LEAVE THEN 4 P3_PTR +!
      LOOP  ;
: BEST_MOVE GET_P0_MOVES IF 0
       DO GET_P1_MOVES MOVE$ IF 1 MAX PRUNE @ 2* MIN 0
         DO GET_P2_MOVES IF PRUNE?
           DO GET_P3_MOVES IF PRUNE?
             TRY_456_MOVES
             ELSE DROP P3_SCORE SCORE3 ! THEN
                TEST_P3_SCORE IF LEAVE THEN
                4 P2_PTR +! LOOP
           ELSE DROP P2_SCORE SCORE2 ! THEN
              TEST_P2_SCORE IF LEAVE THEN
              4 P1_PTR +! LOOP
         ELSE DROP P1_SCORE SCORE1 !
              P1_MOVES @ C@ P_BEST? ! THEN
            TEST_P1_SCORE 4 P0_PTR +!
            CURTAIL IF LEAVE THEN  LOOP
       ELSE DROP P0_PTR @ 2@ SCORE0 2! THEN     ;

: GO SKILL @ OLD_SKILL !
     CLRMSG ."   My move" SKILL @ 2 >
     IF   3 SPACES ." best so far" CR
         12 SPACES ." last  was  " CR
         12 SPACES ." considering"
     THEN
     2 4 AT ." ( Hold I down to interrupt )"
     BEST_MOVE MYMOVE ['] P0_BOARD BOARD_AD !
     COMP_COL
     SCORE0 SAVE_MOVE DRAW_MEN SCORE0 C@ COMP ! ;
: P_CR? #MOVES P_TAB @ CR? ;
: PLAYER-MOVE  0 I_FLAG ! CURTAIL IF SET_TABS THEN
     BEGIN PLAY_COL
     ASSIGN OPERATION TO-DO EVALUATE GENP0_MOVES
     P0_MOVES @ C@ ?DUP
     IF GET_MOVE DUP
        IF PLAYER C! P0_MOVES @ 0 OVER ! P0_PTR !
           -1 NEW_MOVE ! PLAYER C@ DUP BOARD C@ 15 >
           IF CHECK_8_WAYS P0_MOVES @ C@ 0= THEN
           IF CLRMSG ."    Illegal move, try again "
              100 5000 BEEP 150 SUSPEND_ME
              #MOVES P_TAB @ TAB 2 SPACES 0
           ELSE PLAYER SAVE_MOVE DRAW_MEN -1 P_CR?
                0 GAME_START !
           THEN
        ELSE 1- THEN
     ELSE 0 PLAYER ! CLRMSG ."    You can't go "
        50 5000 BEEP 200 SUSPEND_ME -1 P_CR?
     THEN UNTIL  ;
: WIN COMP @ PLAYER @ + 0= FULL OR ;
: AGAIN? CLRMSG 6 2 AT ." Another game ? ( Y/N ) "
         0
         BEGIN
           DROP GET_KEY DUP 89 = OVER 78 = OR
         UNTIL
         78 =     ;

: GAME_OVER QFLAG @
      IF CLRMSG 3 1 CSIZE 7 1 AT
         P_SCORE @ C_SCORE @ 2DUP =
         IF ." Game drawn" 2DROP
         ELSE
           GAME_MODE @
           IF P_COL @ WHITE = IF SWAP THEN
                > IF   ."   Black wins"
                  ELSE ."   White wins"
                  THEN
           ELSE
                > IF   ."   You win"
                  ELSE ."   QL wins"
                  THEN
           THEN
         THEN 2 0 CSIZE 200 SUSPEND_ME
      THEN ;

: FIRST_MOVE COMP_COL TIME DROP 3 AND
        CASE 0 OF 32 ENDOF  1 OF 42 ENDOF
             2 OF 48 ENDOF  3 OF 58 ENDOF DEFAULT
        ENDCASE SCORE0 C! MYMOVE
        SCORE0 SAVE_MOVE DRAW_MEN  ;

: REV_ERROR #MESS CLS 1 1 AT ." QDOS error - Reversi terminated"
            CLOSE_ALL BYE  ;

: SWAP_PARS P_COL @ C_COL @ P_COL ! C_COL !
            P_TAB @ C_TAB @ P_TAB ! C_TAB !
            PLAYER @ COMP @ PLAYER ! COMP !
            SKILL @ OTHER_SKILL @ SKILL ! OTHER_SKILL !   ;

: PLAY/COMP SWAP_PARS 1 =
            IF   PLAY_COL PLAYER-MOVE
            ELSE
              GAME_START @
              IF   FIRST_MOVE 0 GAME_START !
              ELSE SET_PRUNE COMP_COL GO
              THEN
            THEN   ;

: COPYRIGHT  CLS ." Reversi 1.8  (c) 1985 G.W.Jackson "  ;

: INIT_VARS             ( Initialises some variables at start of game )
      BLACK P_COL !     ( Human is black   )
      WHITE C_COL !     ( QL    is white   )
      0 GAME_MODE !     ( H vs QL          )
      0 I_FLAG    !     ( Not interrupted  )
      1 OLD_SKILL !     ( To prevent .BEST )
     -1 GAME_START ! ;  ( Next is first move )

: REVERSI OPEN_WINDOWS SET_SCALE  ASSIGN ERROR TO-DO REV_ERROR
          BEGIN
            INIT-BOARD HEADER SCORE INIT_VARS #MOVES CLS
            SET_TABS SET_PRUNE
            BEGIN
              I_FLAG @
              IF 0 GAME_MODE ! THEN
              GAME_MODE @ ?DUP
              IF PLAY/COMP
              ELSE PLAY_COL PLAYER-MOVE
              THEN
              WIN
              IF -1
              ELSE
                   GAME_MODE @ ?DUP
                   IF I_FLAG @ 0=
                     IF 0 X_FLAG !
                        PLAY/COMP
                        X_FLAG @               ( If have swapped sides ... )
                        IF COMP_COL GO THEN    ( ... give the QL a go )
                     ELSE DROP
                     THEN
                   ELSE
                     GAME_START @
                     IF   FIRST_MOVE  0 GAME_START !
                     ELSE COMP_COL GO
                     THEN
                   THEN
                   WIN
              THEN
            UNTIL GAME_OVER AGAIN?
          UNTIL ABANDON ;
HERE HEAP_BASE ! HERE HEAP ! ( 1000 ALLOT  )
END_FILE 
