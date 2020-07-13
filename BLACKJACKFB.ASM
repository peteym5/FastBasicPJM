; Imported symbols
	.global CHARSET_GAME_HI
	.global DISPLAY_LIST_TITLE
	.global DLIV0
	.global DLIV1
	.global SCREEN_ADDR
	.global TITLE00_DLI
	.global TITLE_VBI
	.globalzp CLS

; Exported symbols
	.export bytecode_start
	.exportzp NUM_VARS

	.include "atari.inc"

; TOKENS:
	.importzp	TOK_END
	.importzp	TOK_NUM
	.importzp	TOK_BYTE
	.importzp	TOK_CSTRING
	.importzp	TOK_VAR_SADDR
	.importzp	TOK_VAR_LOAD
	.importzp	TOK_0
	.importzp	TOK_1
	.importzp	TOK_PUSH
	.importzp	TOK_PUSH_VAR_LOAD
	.importzp	TOK_PUSH_NUM
	.importzp	TOK_PUSH_BYTE
	.importzp	TOK_PUSH_0
	.importzp	TOK_PUSH_1
	.importzp	TOK_NEG
	.importzp	TOK_ABS
	.importzp	TOK_ADD
	.importzp	TOK_SUB
	.importzp	TOK_MUL
	.importzp	TOK_DIV
	.importzp	TOK_MOD
	.importzp	TOK_ADD_VAR
	.importzp	TOK_PEEK
	.importzp	TOK_DPEEK
	.importzp	TOK_RAND
	.importzp	TOK_L_NOT
	.importzp	TOK_L_OR
	.importzp	TOK_L_AND
	.importzp	TOK_LT
	.importzp	TOK_GT
	.importzp	TOK_NEQ
	.importzp	TOK_EQ
	.importzp	TOK_COMP_0
	.importzp	TOK_POKE
	.importzp	TOK_DPOKE
	.importzp	TOK_VAR_STORE
	.importzp	TOK_SADDR
	.importzp	TOK_DECVAR
	.importzp	TOK_BYTE_POKE
	.importzp	TOK_NUM_POKE
	.importzp	TOK_VAR_STORE_0
	.importzp	TOK_POSITION
	.importzp	TOK_PRINT_STR
	.importzp	TOK_GETKEY
	.importzp	TOK_INPUT_STR
	.importzp	TOK_PUT
	.importzp	TOK_JUMP
	.importzp	TOK_CJUMP
	.importzp	TOK_CNJUMP
	.importzp	TOK_CALL
	.importzp	TOK_RET
	.importzp	TOK_CRET
	.importzp	TOK_FOR
	.importzp	TOK_FOR_NEXT
	.importzp	TOK_FOR_EXIT
	.importzp	TOK_DIM
	.importzp	TOK_USHL
	.importzp	TOK_COPY_STR
	.importzp	TOK_VAL
	.importzp	TOK_CMP_STR
	.importzp	TOK_INT_STR
	.importzp	TOK_CAT_STR
	.importzp	TOK_CHR
	.importzp	TOK_INT_FP
	.importzp	TOK_FLOAT
	.importzp	TOK_FP_MUL
	.importzp	TOK_FP_INT
	.importzp	TOK_FP_STR
;-----------------------------
; Variables
NUM_VARS = 140
	.import heap_start
	.export fb_var_A
	.export fb_var_ACES
	.export fb_var_ALLOWSPLIT
	.export fb_var_ALLOWSURRENDER
	.export fb_var_B
	.export fb_var_BETIN
	.export fb_var_BETOK
	.export fb_var_BETPLAYERNUMBER
	.export fb_var_BETVAL
	.export fb_var_BJCARDVALUE
	.export fb_var_BONUS678
	.export fb_var_BONUS777
	.export fb_var_BONUSREWARD
	.export fb_var_BONUS_678_OPTION_SHOW
	.export fb_var_BONUS_777_OPTION_SHOW
	.export fb_var_BUTTONUP
	.export fb_var_C
	.export fb_var_CARDBACKIMAGE
	.export fb_var_CARDCHARLIEWINSET
	.export fb_var_CARDIMAGE
	.export fb_var_CARDIMG
	.export fb_var_CARDNO
	.export fb_var_CARDORDER
	.export fb_var_CARDPLACE
	.export fb_var_CARDPOSX
	.export fb_var_CARDPOSY
	.export fb_var_CARDSPACING
	.export fb_var_CARDSUITVALUE
	.export fb_var_COLOR21BONUS
	.export fb_var_D
	.export fb_var_DBL_DOWN_3UP
	.export fb_var_DBL_DOWN_SET
	.export fb_var_DEALERHIDDEN
	.export fb_var_DEALERNOHIDDEN
	.export fb_var_DEALERSOFT17HIT
	.export fb_var_DEALERTIEWINS
	.export fb_var_DECK
	.export fb_var_DECKSIZE
	.export fb_var_DISPDEALERLEFT
	.export fb_var_DISPLAYBETPLAYERNUMBER
	.export fb_var_DISPPLAYERLEFT
	.export fb_var_DISPPLAYERSPACING
	.export fb_var_DISPPLAYERTOP
	.export fb_var_DISPPLAYERVERTICAL
	.export fb_var_E
	.export fb_var_END_PROGRAM
	.export fb_var_ERRORMESSAGE
	.export fb_var_EXITGAME
	.export fb_var_F
	.export fb_var_FALSE
	.export fb_var_FILENUMBERSTRING
	.export fb_var_FROM
	.export fb_var_G
	.export fb_var_G0
	.export fb_var_G1
	.export fb_var_G2
	.export fb_var_GETBETPLAYERNUMBER
	.export fb_var_GONEXTPLAYER
	.export fb_var_H
	.export fb_var_HANDNUMBER
	.export fb_var_HANDREWARD
	.export fb_var_HANDTOTAL
	.export fb_var_I
	.export fb_var_IMAGEGET
	.export fb_var_J
	.export fb_var_JOKERBETRETURN
	.export fb_var_JOKERSPECIALACE
	.export fb_var_JOKERSPRESANT
	.export fb_var_K
	.export fb_var_KEYPRESSED
	.export fb_var_KEYUP
	.export fb_var_KSIZE
	.export fb_var_L
	.export fb_var_LASTCARD
	.export fb_var_LASTKEYPRESSED
	.export fb_var_LASTPLAYER
	.export fb_var_M
	.export fb_var_MAXPLAYERS
	.export fb_var_MODE_BITDEPTH
	.export fb_var_MODE_RES_X
	.export fb_var_MODE_RES_Y
	.export fb_var_MOUSECLICK
	.export fb_var_MX
	.export fb_var_MY
	.export fb_var_N
	.export fb_var_NEWCARDVALUE
	.export fb_var_NUMBEROFDECKS
	.export fb_var_NUMBER_OF_DECKS
	.export fb_var_NUMBER_OF_PLAYERS_OPTION_SHOW
	.export fb_var_O
	.export fb_var_OLDORDER
	.export fb_var_P
	.export fb_var_PKCARDVALUE
	.export fb_var_PLAYERBJCARDVAL
	.export fb_var_PLAYERNACES
	.export fb_var_PLAYERNBET
	.export fb_var_PLAYERNBETENTERY
	.export fb_var_PLAYERNCASH
	.export fb_var_PLAYERNDOUBLEUSED
	.export fb_var_PLAYERNDRAWN
	.export fb_var_PLAYERNMESSAGE
	.export fb_var_PLAYERNNAME
	.export fb_var_PLAYERNO
	.export fb_var_PLAYERNSPLIT
	.export fb_var_PLAYERNSPLITCARD
	.export fb_var_PLAYERNTOTAL
	.export fb_var_PLAYERNUMBER
	.export fb_var_PLAYERNWINS
	.export fb_var_PLAYERPKCARDVAL
	.export fb_var_PLAYERSUCARDVAL
	.export fb_var_Q
	.export fb_var_QUITBOXLEFT
	.export fb_var_R
	.export fb_var_RESET_TITLE
	.export fb_var_RESHUFFLENO
	.export fb_var_ROUNDSPLAYED
	.export fb_var_S
	.export fb_var_SELECTED_BITDEPTH
	.export fb_var_SELECTED_MODE
	.export fb_var_SELECTED_RES_X
	.export fb_var_SELECTED_RES_Y
	.export fb_var_SPLITBASE
	.export fb_var_SPLITEXTRALEFT
	.export fb_var_SPLITREWARD
	.export fb_var_SPLITSET
	.export fb_var_SS
	.export fb_var_SUIT21BONUS
	.export fb_var_SUITEDAJBONUS
	.export fb_var_SUITSIZE
	.export fb_var_T
	.export fb_var_TOTAL
	.export fb_var_TOTALCARDS
	.export fb_var_TRUE
	.export fb_var_U
	.export fb_var_USEIMAGE
	.export fb_var_V
	.export fb_var_W
	.export fb_var_X
	.export fb_var_Y
	.export fb_var_Z
fb_var_A	= heap_start + 154	; Word variable
fb_var_ACES	= heap_start + 220	; Word variable
fb_var_ALLOWSPLIT	= heap_start + 90	; Word variable
fb_var_ALLOWSURRENDER	= heap_start + 134	; Word variable
fb_var_B	= heap_start + 156	; Word variable
fb_var_BETIN	= heap_start + 250	; String variable
fb_var_BETOK	= heap_start + 252	; Word variable
fb_var_BETPLAYERNUMBER	= heap_start + 244	; Word variable
fb_var_BETVAL	= heap_start + 254	; Word variable
fb_var_BJCARDVALUE	= heap_start + 46	; Byte Array variable
fb_var_BONUS678	= heap_start + 18	; Word variable
fb_var_BONUS777	= heap_start + 16	; Word variable
fb_var_BONUSREWARD	= heap_start + 236	; Word variable
fb_var_BONUS_678_OPTION_SHOW	= heap_start + 14	; String Array variable
fb_var_BONUS_777_OPTION_SHOW	= heap_start + 12	; String Array variable
fb_var_BUTTONUP	= heap_start + 270	; Word variable
fb_var_C	= heap_start + 158	; Word variable
fb_var_CARDBACKIMAGE	= heap_start + 44	; String variable
fb_var_CARDCHARLIEWINSET	= heap_start + 24	; Word variable
fb_var_CARDIMAGE	= heap_start + 42	; String Array variable
fb_var_CARDIMG	= heap_start + 54	; Word Array variable
fb_var_CARDNO	= heap_start + 78	; Word variable
fb_var_CARDORDER	= heap_start + 52	; Byte Array variable
fb_var_CARDPLACE	= heap_start + 208	; Word variable
fb_var_CARDPOSX	= heap_start + 228	; Word variable
fb_var_CARDPOSY	= heap_start + 230	; Word variable
fb_var_CARDSPACING	= heap_start + 226	; Word variable
fb_var_CARDSUITVALUE	= heap_start + 50	; Byte Array variable
fb_var_COLOR21BONUS	= heap_start + 30	; Word variable
fb_var_D	= heap_start + 160	; Word variable
fb_var_DBL_DOWN_3UP	= heap_start + 4	; Word variable
fb_var_DBL_DOWN_SET	= heap_start + 2	; Word variable
fb_var_DEALERHIDDEN	= heap_start + 76	; Word variable
fb_var_DEALERNOHIDDEN	= heap_start + 132	; Word variable
fb_var_DEALERSOFT17HIT	= heap_start + 22	; Word variable
fb_var_DEALERTIEWINS	= heap_start + 20	; Word variable
fb_var_DECK	= heap_start + 206	; Word variable
fb_var_DECKSIZE	= heap_start + 8	; Word variable
fb_var_DISPDEALERLEFT	= heap_start + 258	; Word variable
fb_var_DISPLAYBETPLAYERNUMBER	= heap_start + 248	; Word variable
fb_var_DISPPLAYERLEFT	= heap_start + 64	; Word variable
fb_var_DISPPLAYERSPACING	= heap_start + 62	; Word variable
fb_var_DISPPLAYERTOP	= heap_start + 66	; Word variable
fb_var_DISPPLAYERVERTICAL	= heap_start + 68	; Word variable
fb_var_E	= heap_start + 162	; Word variable
fb_var_END_PROGRAM	= heap_start + 152	; Word variable
fb_var_ERRORMESSAGE	= heap_start + 128	; String variable
fb_var_EXITGAME	= heap_start + 92	; Word variable
fb_var_F	= heap_start + 164	; String variable
fb_var_FALSE	= heap_start + 38	; Word variable
fb_var_FILENUMBERSTRING	= heap_start + 214	; String variable
fb_var_FROM	= heap_start + 216	; Word variable
fb_var_G	= heap_start + 166	; Word variable
fb_var_G0	= heap_start + 262	; Word variable
fb_var_G1	= heap_start + 264	; Word variable
fb_var_G2	= heap_start + 266	; Word variable
fb_var_GETBETPLAYERNUMBER	= heap_start + 246	; Word variable
fb_var_GONEXTPLAYER	= heap_start + 88	; Word variable
fb_var_H	= heap_start + 168	; Word variable
fb_var_HANDNUMBER	= heap_start + 240	; Word variable
fb_var_HANDREWARD	= heap_start + 124	; Word Array variable
fb_var_HANDTOTAL	= heap_start + 242	; Word variable
fb_var_I	= heap_start + 170	; Word variable
fb_var_IMAGEGET	= heap_start + 210	; Word variable
fb_var_J	= heap_start + 172	; Word variable
fb_var_JOKERBETRETURN	= heap_start + 138	; Word variable
fb_var_JOKERSPECIALACE	= heap_start + 140	; Word variable
fb_var_JOKERSPRESANT	= heap_start + 142	; Word variable
fb_var_K	= heap_start + 174	; Word variable
fb_var_KEYPRESSED	= heap_start + 126	; String variable
fb_var_KEYUP	= heap_start + 232	; Word variable
fb_var_KSIZE	= heap_start + 6	; Word variable
fb_var_L	= heap_start + 176	; Word variable
fb_var_LASTCARD	= heap_start + 212	; Word variable
fb_var_LASTKEYPRESSED	= heap_start + 130	; Word variable
fb_var_LASTPLAYER	= heap_start + 40	; Word variable
fb_var_M	= heap_start + 178	; Word variable
fb_var_MAXPLAYERS	= heap_start + 34	; Word variable
fb_var_MODE_BITDEPTH	= heap_start + 60	; Byte Array variable
fb_var_MODE_RES_X	= heap_start + 56	; Byte Array variable
fb_var_MODE_RES_Y	= heap_start + 58	; Byte Array variable
fb_var_MOUSECLICK	= heap_start + 268	; Word variable
fb_var_MX	= heap_start + 274	; Word variable
fb_var_MY	= heap_start + 276	; Word variable
fb_var_N	= heap_start + 180	; Word variable
fb_var_NEWCARDVALUE	= heap_start + 82	; Word variable
fb_var_NUMBEROFDECKS	= heap_start + 94	; Word variable
fb_var_NUMBER_OF_DECKS	= heap_start + 10	; Word variable
fb_var_NUMBER_OF_PLAYERS_OPTION_SHOW	= heap_start + 32	; String Array variable
fb_var_O	= heap_start + 182	; Word variable
fb_var_OLDORDER	= heap_start + 0	; Word Array variable
fb_var_P	= heap_start + 184	; Word variable
fb_var_PKCARDVALUE	= heap_start + 48	; Byte Array variable
fb_var_PLAYERBJCARDVAL	= heap_start + 112	; Byte Array variable
fb_var_PLAYERNACES	= heap_start + 110	; Byte Array variable
fb_var_PLAYERNBET	= heap_start + 106	; Word Array variable
fb_var_PLAYERNBETENTERY	= heap_start + 104	; Word Array variable
fb_var_PLAYERNCASH	= heap_start + 102	; Word Array variable
fb_var_PLAYERNDOUBLEUSED	= heap_start + 120	; Word Array variable
fb_var_PLAYERNDRAWN	= heap_start + 98	; Byte Array variable
fb_var_PLAYERNMESSAGE	= heap_start + 122	; String Array variable
fb_var_PLAYERNNAME	= heap_start + 96	; String Array variable
fb_var_PLAYERNO	= heap_start + 86	; Word variable
fb_var_PLAYERNSPLIT	= heap_start + 108	; Byte Array variable
fb_var_PLAYERNSPLITCARD	= heap_start + 118	; Byte Array variable
fb_var_PLAYERNTOTAL	= heap_start + 100	; Byte Array variable
fb_var_PLAYERNUMBER	= heap_start + 222	; Word variable
fb_var_PLAYERNWINS	= heap_start + 144	; Word Array variable
fb_var_PLAYERPKCARDVAL	= heap_start + 114	; Byte Array variable
fb_var_PLAYERSUCARDVAL	= heap_start + 116	; Byte Array variable
fb_var_Q	= heap_start + 186	; Word variable
fb_var_QUITBOXLEFT	= heap_start + 256	; Word variable
fb_var_R	= heap_start + 188	; Word variable
fb_var_RESET_TITLE	= heap_start + 278	; Word variable
fb_var_RESHUFFLENO	= heap_start + 80	; Word variable
fb_var_ROUNDSPLAYED	= heap_start + 84	; Word variable
fb_var_S	= heap_start + 190	; Word variable
fb_var_SELECTED_BITDEPTH	= heap_start + 70	; Word variable
fb_var_SELECTED_MODE	= heap_start + 272	; Word variable
fb_var_SELECTED_RES_X	= heap_start + 72	; Word variable
fb_var_SELECTED_RES_Y	= heap_start + 74	; Word variable
fb_var_SPLITBASE	= heap_start + 238	; Word variable
fb_var_SPLITEXTRALEFT	= heap_start + 260	; Word variable
fb_var_SPLITREWARD	= heap_start + 234	; Word variable
fb_var_SPLITSET	= heap_start + 26	; Word variable
fb_var_SS	= heap_start + 150	; Word variable
fb_var_SUIT21BONUS	= heap_start + 28	; Word variable
fb_var_SUITEDAJBONUS	= heap_start + 136	; Word variable
fb_var_SUITSIZE	= heap_start + 148	; Word variable
fb_var_T	= heap_start + 192	; Word variable
fb_var_TOTAL	= heap_start + 218	; Word variable
fb_var_TOTALCARDS	= heap_start + 146	; Word variable
fb_var_TRUE	= heap_start + 36	; Word variable
fb_var_U	= heap_start + 194	; Word variable
fb_var_USEIMAGE	= heap_start + 224	; Word variable
fb_var_V	= heap_start + 196	; Word variable
fb_var_W	= heap_start + 198	; Word variable
fb_var_X	= heap_start + 200	; Word variable
fb_var_Y	= heap_start + 202	; Word variable
fb_var_Z	= heap_start + 204	; Word variable
;-----------------------------
; Bytecode
	.segment "BYTECODE"
bytecode_start:
@FastBasic_LINE_9:	; LINE 9
	.byte	TOK_NUM
	.word	1026
	.byte	TOK_DIM
	.byte	0
@FastBasic_LINE_11:	; LINE 11
	.byte	TOK_VAR_STORE_0
	.byte	1
@FastBasic_LINE_12:	; LINE 12
	.byte	TOK_VAR_STORE_0
	.byte	2
@FastBasic_LINE_13:	; LINE 13
	.byte	TOK_BYTE
	.byte	52
	.byte	TOK_VAR_STORE
	.byte	4
@FastBasic_LINE_14:	; LINE 14
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_VAR_STORE
	.byte	5
@FastBasic_LINE_16:	; LINE 16
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_DIM
	.byte	6
@FastBasic_LINE_17:	; LINE 17
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	6
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	24, "NO BONUS WITH 777       "
	.byte	TOK_COPY_STR
@FastBasic_LINE_18:	; LINE 18
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	6
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	24, "PAYS 3:1, SUIT 4:1      "
	.byte	TOK_COPY_STR
@FastBasic_LINE_20:	; LINE 20
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_DIM
	.byte	7
@FastBasic_LINE_21:	; LINE 21
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	7
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	24, "NO BONUS WITH 678       "
	.byte	TOK_COPY_STR
@FastBasic_LINE_22:	; LINE 22
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	7
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	24, "PAYS 3:1, SUIT 4:1      "
	.byte	TOK_COPY_STR
@FastBasic_LINE_23:	; LINE 23
	.byte	TOK_VAR_STORE_0
	.byte	8
@FastBasic_LINE_24:	; LINE 24
	.byte	TOK_VAR_STORE_0
	.byte	9
@FastBasic_LINE_26:	; LINE 26
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	10
@FastBasic_LINE_28:	; LINE 28
	.byte	TOK_VAR_STORE_0
	.byte	11
@FastBasic_LINE_30:	; LINE 30
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_VAR_STORE
	.byte	12
@FastBasic_LINE_32:	; LINE 32
	.byte	TOK_VAR_STORE_0
	.byte	13
@FastBasic_LINE_34:	; LINE 34
	.byte	TOK_VAR_STORE_0
	.byte	14
@FastBasic_LINE_35:	; LINE 35
	.byte	TOK_VAR_STORE_0
	.byte	15
@FastBasic_LINE_37:	; LINE 37
	.byte	TOK_BYTE
	.byte	8
	.byte	TOK_DIM
	.byte	16
@FastBasic_LINE_38:	; LINE 38
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	16
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	13, "ONE PLAYER   "
	.byte	TOK_COPY_STR
@FastBasic_LINE_39:	; LINE 39
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	16
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	13, "TWO PLAYERS  "
	.byte	TOK_COPY_STR
@FastBasic_LINE_40:	; LINE 40
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	16
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	13, "THREE PLAYERS"
	.byte	TOK_COPY_STR
@FastBasic_LINE_41:	; LINE 41
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_ADD_VAR
	.byte	16
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	13, "FOUR PLAYERS "
	.byte	TOK_COPY_STR
@FastBasic_LINE_45:	; LINE 45
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_VAR_STORE
	.byte	17
@FastBasic_LINE_46:	; LINE 46
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	18
@FastBasic_LINE_47:	; LINE 47
	.byte	TOK_VAR_STORE_0
	.byte	19
@FastBasic_LINE_48:	; LINE 48
	.byte	TOK_VAR_STORE_0
	.byte	20
@FastBasic_LINE_49:	; LINE 49
	.byte	TOK_BYTE
	.byte	114
	.byte	TOK_DIM
	.byte	21
@FastBasic_LINE_50:	; LINE 50
	.byte	TOK_VAR_SADDR
	.byte	22
	.byte	TOK_CSTRING
	.byte	9, "         "
	.byte	TOK_COPY_STR
@FastBasic_LINE_51:	; LINE 51
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	21
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	9, "         "
	.byte	TOK_COPY_STR
@FastBasic_LINE_52:	; LINE 52
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	21
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	9, "         "
	.byte	TOK_COPY_STR
@FastBasic_LINE_53:	; LINE 53
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	21
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	9, "         "
	.byte	TOK_COPY_STR
@FastBasic_LINE_54:	; LINE 54
	.byte	TOK_NUM
	.word	561
	.byte	TOK_DIM
	.byte	23
@FastBasic_LINE_55:	; LINE 55
	.byte	TOK_NUM
	.word	561
	.byte	TOK_DIM
	.byte	24
@FastBasic_LINE_56:	; LINE 56
	.byte	TOK_NUM
	.word	561
	.byte	TOK_DIM
	.byte	25
@FastBasic_LINE_57:	; LINE 57
	.byte	TOK_NUM
	.word	561
	.byte	TOK_DIM
	.byte	26
@FastBasic_LINE_58:	; LINE 58
	.byte	TOK_BYTE
	.byte	122
	.byte	TOK_DIM
	.byte	27
@FastBasic_LINE_59:	; LINE 59
	.byte	TOK_BYTE
	.byte	65
	.byte	TOK_DIM
	.byte	28
@FastBasic_LINE_60:	; LINE 60
	.byte	TOK_BYTE
	.byte	65
	.byte	TOK_DIM
	.byte	29
@FastBasic_LINE_61:	; LINE 61
	.byte	TOK_BYTE
	.byte	65
	.byte	TOK_DIM
	.byte	30
@FastBasic_LINE_62:	; LINE 62
	.byte	TOK_VAR_STORE_0
	.byte	31
@FastBasic_LINE_63:	; LINE 63
	.byte	TOK_VAR_STORE_0
	.byte	32
@FastBasic_LINE_64:	; LINE 64
	.byte	TOK_VAR_STORE_0
	.byte	33
@FastBasic_LINE_65:	; LINE 65
	.byte	TOK_VAR_STORE_0
	.byte	34
@FastBasic_LINE_66:	; LINE 66
	.byte	TOK_VAR_STORE_0
	.byte	35
@FastBasic_LINE_67:	; LINE 67
	.byte	TOK_VAR_STORE_0
	.byte	36
@FastBasic_LINE_68:	; LINE 68
	.byte	TOK_VAR_STORE_0
	.byte	37
@FastBasic_LINE_69:	; LINE 69
	.byte	TOK_VAR_STORE_0
	.byte	38
@FastBasic_LINE_70:	; LINE 70
	.byte	TOK_VAR_STORE_0
	.byte	39
@FastBasic_LINE_71:	; LINE 71
	.byte	TOK_VAR_STORE_0
	.byte	40
@FastBasic_LINE_72:	; LINE 72
	.byte	TOK_VAR_STORE_0
	.byte	41
@FastBasic_LINE_73:	; LINE 73
	.byte	TOK_VAR_STORE_0
	.byte	42
@FastBasic_LINE_74:	; LINE 74
	.byte	TOK_VAR_STORE_0
	.byte	43
@FastBasic_LINE_75:	; LINE 75
	.byte	TOK_VAR_STORE_0
	.byte	44
@FastBasic_LINE_76:	; LINE 76
	.byte	TOK_VAR_STORE_0
	.byte	45
@FastBasic_LINE_77:	; LINE 77
	.byte	TOK_VAR_STORE_0
	.byte	46
@FastBasic_LINE_78:	; LINE 78
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_VAR_STORE
	.byte	47
@FastBasic_LINE_79:	; LINE 79
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_DIM
	.byte	48
@FastBasic_LINE_80:	; LINE 80
	.byte	TOK_BYTE
	.byte	5
	.byte	TOK_DIM
	.byte	49
@FastBasic_LINE_81:	; LINE 81
	.byte	TOK_BYTE
	.byte	17
	.byte	TOK_DIM
	.byte	50
@FastBasic_LINE_82:	; LINE 82
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_DIM
	.byte	51
@FastBasic_LINE_83:	; LINE 83
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_DIM
	.byte	52
@FastBasic_LINE_84:	; LINE 84
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_DIM
	.byte	53
@FastBasic_LINE_85:	; LINE 85
	.byte	TOK_BYTE
	.byte	5
	.byte	TOK_DIM
	.byte	54
@FastBasic_LINE_86:	; LINE 86
	.byte	TOK_BYTE
	.byte	5
	.byte	TOK_DIM
	.byte	55
@FastBasic_LINE_87:	; LINE 87
	.byte	TOK_BYTE
	.byte	81
	.byte	TOK_DIM
	.byte	56
@FastBasic_LINE_88:	; LINE 88
	.byte	TOK_BYTE
	.byte	81
	.byte	TOK_DIM
	.byte	57
@FastBasic_LINE_89:	; LINE 89
	.byte	TOK_BYTE
	.byte	81
	.byte	TOK_DIM
	.byte	58
@FastBasic_LINE_90:	; LINE 90
	.byte	TOK_BYTE
	.byte	81
	.byte	TOK_DIM
	.byte	59
@FastBasic_LINE_91:	; LINE 91
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_DIM
	.byte	60
@FastBasic_LINE_92:	; LINE 92
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_DIM
	.byte	61
@FastBasic_LINE_93:	; LINE 93
	.byte	TOK_BYTE
	.byte	8
	.byte	TOK_DIM
	.byte	62
@FastBasic_LINE_94:	; LINE 94
	.byte	TOK_VAR_SADDR
	.byte	63
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_95:	; LINE 95
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_96:	; LINE 96
	.byte	TOK_VAR_STORE_0
	.byte	65
@FastBasic_LINE_97:	; LINE 97
	.byte	TOK_VAR_STORE_0
	.byte	10
@FastBasic_LINE_98:	; LINE 98
	.byte	TOK_VAR_STORE_0
	.byte	11
@FastBasic_LINE_99:	; LINE 99
	.byte	TOK_VAR_STORE_0
	.byte	66
@FastBasic_LINE_100:	; LINE 100
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_VAR_STORE
	.byte	12
@FastBasic_LINE_101:	; LINE 101
	.byte	TOK_VAR_STORE_0
	.byte	67
@FastBasic_LINE_102:	; LINE 102
	.byte	TOK_VAR_STORE_0
	.byte	14
@FastBasic_LINE_103:	; LINE 103
	.byte	TOK_VAR_STORE_0
	.byte	15
@FastBasic_LINE_104:	; LINE 104
	.byte	TOK_VAR_STORE_0
	.byte	68
@FastBasic_LINE_105:	; LINE 105
	.byte	TOK_VAR_STORE_0
	.byte	69
@FastBasic_LINE_106:	; LINE 106
	.byte	TOK_VAR_STORE_0
	.byte	70
@FastBasic_LINE_107:	; LINE 107
	.byte	TOK_VAR_STORE_0
	.byte	71
@FastBasic_LINE_108:	; LINE 108
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_DIM
	.byte	72
@FastBasic_LINE_109:	; LINE 109
	.byte	TOK_VAR_STORE_0
	.byte	73
@FastBasic_LINE_110:	; LINE 110
	.byte	TOK_VAR_STORE_0
	.byte	4
@FastBasic_LINE_111:	; LINE 111
	.byte	TOK_VAR_STORE_0
	.byte	74
@FastBasic_LINE_112:	; LINE 112
	.byte	TOK_VAR_STORE_0
	.byte	75
@FastBasic_LINE_113:	; LINE 113
	.byte	TOK_VAR_STORE_0
	.byte	76
@FastBasic_LINE_114:	; LINE 114
	.byte	TOK_VAR_STORE_0
	.byte	77
@FastBasic_LINE_115:	; LINE 115
	.byte	TOK_VAR_STORE_0
	.byte	78
@FastBasic_LINE_116:	; LINE 116
	.byte	TOK_VAR_STORE_0
	.byte	79
@FastBasic_LINE_117:	; LINE 117
	.byte	TOK_VAR_STORE_0
	.byte	80
@FastBasic_LINE_118:	; LINE 118
	.byte	TOK_VAR_STORE_0
	.byte	81
@FastBasic_LINE_119:	; LINE 119
	.byte	TOK_VAR_SADDR
	.byte	82
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_120:	; LINE 120
	.byte	TOK_VAR_STORE_0
	.byte	83
@FastBasic_LINE_121:	; LINE 121
	.byte	TOK_VAR_STORE_0
	.byte	84
@FastBasic_LINE_122:	; LINE 122
	.byte	TOK_VAR_STORE_0
	.byte	85
@FastBasic_LINE_123:	; LINE 123
	.byte	TOK_VAR_STORE_0
	.byte	86
@FastBasic_LINE_124:	; LINE 124
	.byte	TOK_VAR_STORE_0
	.byte	87
@FastBasic_LINE_125:	; LINE 125
	.byte	TOK_VAR_STORE_0
	.byte	88
@FastBasic_LINE_126:	; LINE 126
	.byte	TOK_VAR_STORE_0
	.byte	89
@FastBasic_LINE_127:	; LINE 127
	.byte	TOK_VAR_STORE_0
	.byte	90
@FastBasic_LINE_128:	; LINE 128
	.byte	TOK_VAR_STORE_0
	.byte	91
@FastBasic_LINE_129:	; LINE 129
	.byte	TOK_VAR_STORE_0
	.byte	92
@FastBasic_LINE_130:	; LINE 130
	.byte	TOK_VAR_STORE_0
	.byte	93
@FastBasic_LINE_131:	; LINE 131
	.byte	TOK_VAR_STORE_0
	.byte	94
@FastBasic_LINE_132:	; LINE 132
	.byte	TOK_VAR_STORE_0
	.byte	95
@FastBasic_LINE_133:	; LINE 133
	.byte	TOK_VAR_STORE_0
	.byte	96
@FastBasic_LINE_134:	; LINE 134
	.byte	TOK_VAR_STORE_0
	.byte	97
@FastBasic_LINE_135:	; LINE 135
	.byte	TOK_VAR_STORE_0
	.byte	98
@FastBasic_LINE_136:	; LINE 136
	.byte	TOK_VAR_STORE_0
	.byte	99
@FastBasic_LINE_137:	; LINE 137
	.byte	TOK_VAR_STORE_0
	.byte	100
@FastBasic_LINE_138:	; LINE 138
	.byte	TOK_VAR_STORE_0
	.byte	101
@FastBasic_LINE_139:	; LINE 139
	.byte	TOK_VAR_STORE_0
	.byte	102
@FastBasic_LINE_140:	; LINE 140
	.byte	TOK_VAR_LOAD
	.byte	19
	.byte	TOK_VAR_STORE
	.byte	76
@FastBasic_LINE_142:	; LINE 142
	.export	jump_lbl_3
jump_lbl_3:
@FastBasic_LINE_143:	; LINE 143
	.byte	TOK_CALL
	.word	fb_lbl_INIT_DEFAULTS
@FastBasic_LINE_144:	; LINE 144
	.byte	TOK_CALL
	.word	fb_lbl_TITLE_SCREEN
@FastBasic_LINE_146:	; LINE 146
	.export	jump_lbl_4
jump_lbl_4:
@FastBasic_LINE_147:	; LINE 147
	.byte	TOK_JUMP
	.word	jump_lbl_4
@FastBasic_LINE_155:	; LINE 155
	.export	jump_lbl_6
jump_lbl_6:
@FastBasic_LINE_156:	; LINE 156
	.byte	TOK_CALL
	.word	fb_lbl_GETBETS
@FastBasic_LINE_157:	; LINE 157
	.byte	TOK_VAR_LOAD
	.byte	46
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_7
@FastBasic_LINE_158:	; LINE 158
	.byte	TOK_VAR_STORE_0
	.byte	43
@FastBasic_LINE_160:	; LINE 160
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_DEAL
@FastBasic_LINE_161:	; LINE 161
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_VAR_LOAD
	.byte	20
	.byte	TOK_GT
	.byte	TOK_CNJUMP
	.word	jump_lbl_8
@FastBasic_LINE_162:	; LINE 162
	.export	jump_lbl_9
jump_lbl_9:
@FastBasic_LINE_163:	; LINE 163
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_PLAYERINPUT
@FastBasic_LINE_164:	; LINE 164
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_10
@FastBasic_LINE_165:	; LINE 165
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_NEXTHAND
@FastBasic_LINE_166:	; LINE 166
	.byte	TOK_JUMP
	.word	jump_lbl_11
	.export	jump_lbl_10
jump_lbl_10:
@FastBasic_LINE_167:	; LINE 167
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_168:	; LINE 168
	.export	jump_lbl_11
jump_lbl_11:
@FastBasic_LINE_169:	; LINE 169
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_VAR_LOAD
	.byte	20
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_9
@FastBasic_LINE_170:	; LINE 170
	.export	jump_lbl_8
jump_lbl_8:
@FastBasic_LINE_171:	; LINE 171
	.byte	TOK_VAR_STORE_0
	.byte	43
@FastBasic_LINE_172:	; LINE 172
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_DEALER
@FastBasic_LINE_173:	; LINE 173
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_REWARDS
@FastBasic_LINE_174:	; LINE 174
	.export	jump_lbl_12
jump_lbl_12:
@FastBasic_LINE_175:	; LINE 175
	.byte	TOK_CALL
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_176:	; LINE 176
	.byte	TOK_GETKEY
	.byte	TOK_VAR_STORE
	.byte	65
@FastBasic_LINE_177:	; LINE 177
	.byte	TOK_VAR_LOAD
	.byte	65
	.byte	TOK_PUSH_BYTE
	.byte	96
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	65
	.byte	TOK_PUSH_BYTE
	.byte	123
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_13
	.byte	TOK_VAR_LOAD
	.byte	65
	.byte	TOK_PUSH_BYTE
	.byte	32
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	65
	.export	jump_lbl_13
jump_lbl_13:
@FastBasic_LINE_178:	; LINE 178
	.byte	TOK_VAR_SADDR
	.byte	63
	.byte	TOK_VAR_LOAD
	.byte	65
	.byte	TOK_CHR
	.byte	TOK_COPY_STR
@FastBasic_LINE_179:	; LINE 179
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "S"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_14
@FastBasic_LINE_181:	; LINE 181
	.byte	TOK_VAR_SADDR
	.byte	82
	.byte	TOK_CSTRING
	.byte	12, "IMGBLACKJACK"
	.byte	TOK_COPY_STR
@FastBasic_LINE_183:	; LINE 183
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	75
	.byte	TOK_VAR_STORE
	.byte	75
@FastBasic_LINE_184:	; LINE 184
	.byte	TOK_VAR_LOAD
	.byte	75
	.byte	TOK_PUSH_BYTE
	.byte	9
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_14
	.byte	TOK_VAR_STORE_0
	.byte	75
@FastBasic_LINE_185:	; LINE 185
	.export	jump_lbl_14
jump_lbl_14:
@FastBasic_LINE_186:	; LINE 186
	.byte	TOK_GETKEY
	.byte	TOK_VAR_STORE
	.byte	65
	.byte	TOK_VAR_SADDR
	.byte	63
	.byte	TOK_VAR_LOAD
	.byte	65
	.byte	TOK_CHR
	.byte	TOK_COPY_STR
@FastBasic_LINE_187:	; LINE 187
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, " "
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_12
@FastBasic_LINE_189:	; LINE 189
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	39
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_16
	.export	jump_lbl_17
jump_lbl_17:
@FastBasic_LINE_190:	; LINE 190
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	86
@FastBasic_LINE_192:	; LINE 192
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_17
	.export	jump_lbl_16
jump_lbl_16:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_193:	; LINE 193
	.byte	TOK_VAR_LOAD
	.byte	39
	.byte	TOK_PUSH_VAR_LOAD
	.byte	40
	.byte	TOK_LT
	.byte	TOK_CNJUMP
	.word	jump_lbl_7
	.byte	TOK_CALL
	.word	fb_lbl_SHUFFLEDECKS
@FastBasic_LINE_194:	; LINE 194
	.export	jump_lbl_7
jump_lbl_7:
@FastBasic_LINE_195:	; LINE 195
	.byte	TOK_VAR_LOAD
	.byte	46
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_6
@FastBasic_LINE_197:	; LINE 197
	.byte	TOK_VAR_LOAD
	.byte	76
	.byte	TOK_PUSH_VAR_LOAD
	.byte	18
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_3
@FastBasic_LINE_199:	; LINE 199
	.byte	TOK_END
@FastBasic_LINE_6:	; LINE 6
	.export	fb_lbl_BLACKJACKVALUE
fb_lbl_BLACKJACKVALUE:
	.byte	11
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.byte	6
	.byte	7
	.byte	8
	.byte	9
	.byte	10
	.byte	10
	.byte	10
	.byte	10
	.byte	11
@FastBasic_LINE_7:	; LINE 7
	.export	fb_lbl_POKERVALUE
fb_lbl_POKERVALUE:
	.byte	15
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.byte	6
	.byte	7
	.byte	8
	.byte	9
	.byte	10
	.byte	12
	.byte	13
	.byte	14
	.byte	11
@FastBasic_LINE_202:	; LINE 202
	.byte	TOK_VAR_STORE_0
	.byte	103
@FastBasic_LINE_203:	; LINE 203
	.byte	TOK_VAR_STORE_0
	.byte	104
@FastBasic_LINE_204:	; LINE 204
	.byte	TOK_VAR_STORE_0
	.byte	105
@FastBasic_LINE_205:	; LINE 205
	.byte	TOK_VAR_STORE_0
	.byte	106
@FastBasic_LINE_206:	; LINE 206
	.byte	TOK_VAR_SADDR
	.byte	107
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_210:	; LINE 210
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	300
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	621
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_21
@FastBasic_LINE_211:	; LINE 211
	.byte	TOK_BYTE
	.byte	244
	.byte	TOK_VAR_STORE
	.byte	33
@FastBasic_LINE_212:	; LINE 212
	.byte	TOK_BYTE
	.byte	27
	.byte	TOK_VAR_STORE
	.byte	34
@FastBasic_LINE_213:	; LINE 213
	.export	jump_lbl_21
jump_lbl_21:
@FastBasic_LINE_214:	; LINE 214
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	620
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	780
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_22
@FastBasic_LINE_215:	; LINE 215
	.byte	TOK_BYTE
	.byte	248
	.byte	TOK_VAR_STORE
	.byte	33
@FastBasic_LINE_216:	; LINE 216
	.byte	TOK_BYTE
	.byte	28
	.byte	TOK_VAR_STORE
	.byte	34
@FastBasic_LINE_217:	; LINE 217
	.export	jump_lbl_22
jump_lbl_22:
@FastBasic_LINE_218:	; LINE 218
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	778
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	1000
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_23
@FastBasic_LINE_219:	; LINE 219
	.byte	TOK_NUM
	.word	280
	.byte	TOK_VAR_STORE
	.byte	33
@FastBasic_LINE_220:	; LINE 220
	.byte	TOK_BYTE
	.byte	30
	.byte	TOK_VAR_STORE
	.byte	34
@FastBasic_LINE_221:	; LINE 221
	.export	jump_lbl_23
jump_lbl_23:
@FastBasic_LINE_222:	; LINE 222
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	999
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_24
@FastBasic_LINE_223:	; LINE 223
	.byte	TOK_NUM
	.word	320
	.byte	TOK_VAR_STORE
	.byte	33
@FastBasic_LINE_224:	; LINE 224
	.byte	TOK_BYTE
	.byte	32
	.byte	TOK_VAR_STORE
	.byte	34
@FastBasic_LINE_225:	; LINE 225
	.export	jump_lbl_24
jump_lbl_24:
@FastBasic_LINE_226:	; LINE 226
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_25
@FastBasic_LINE_227:	; LINE 227
	.byte	TOK_BYTE
	.byte	250
	.byte	TOK_VAR_STORE
	.byte	31
@FastBasic_LINE_228:	; LINE 228
	.byte	TOK_BYTE
	.byte	150
	.byte	TOK_VAR_STORE
	.byte	32
@FastBasic_LINE_229:	; LINE 229
	.byte	TOK_JUMP
	.word	jump_lbl_26
	.export	jump_lbl_25
jump_lbl_25:
@FastBasic_LINE_230:	; LINE 230
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_27
@FastBasic_LINE_231:	; LINE 231
	.byte	TOK_VAR_LOAD
	.byte	36
	.byte	TOK_PUSH_BYTE
	.byte	120
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	20
	.byte	TOK_DIV
	.byte	TOK_VAR_STORE
	.byte	31
@FastBasic_LINE_232:	; LINE 232
	.byte	TOK_BYTE
	.byte	50
	.byte	TOK_VAR_STORE
	.byte	32
@FastBasic_LINE_233:	; LINE 233
	.byte	TOK_JUMP
	.word	jump_lbl_28
	.export	jump_lbl_27
jump_lbl_27:
@FastBasic_LINE_234:	; LINE 234
	.byte	TOK_VAR_LOAD
	.byte	36
	.byte	TOK_PUSH_BYTE
	.byte	160
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	20
	.byte	TOK_DIV
	.byte	TOK_VAR_STORE
	.byte	31
@FastBasic_LINE_235:	; LINE 235
	.byte	TOK_BYTE
	.byte	75
	.byte	TOK_VAR_STORE
	.byte	32
@FastBasic_LINE_236:	; LINE 236
	.export	jump_lbl_28
jump_lbl_28:
@FastBasic_LINE_237:	; LINE 237
	.byte	TOK_VAR_LOAD
	.byte	31
	.byte	TOK_PUSH_NUM
	.word	271
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_26
@FastBasic_LINE_238:	; LINE 238
	.byte	TOK_NUM
	.word	270
	.byte	TOK_VAR_STORE
	.byte	31
@FastBasic_LINE_239:	; LINE 239
	.byte	TOK_BYTE
	.byte	25
	.byte	TOK_VAR_STORE
	.byte	32
@FastBasic_LINE_241:	; LINE 241
	.export	jump_lbl_26
jump_lbl_26:
@FastBasic_LINE_242:	; LINE 242
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_BYTE
	.byte	56
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_30
@FastBasic_LINE_243:	; LINE 243
	.byte	TOK_BYTE
	.byte	55
	.byte	TOK_VAR_STORE
	.byte	106
@FastBasic_LINE_244:	; LINE 244
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_VAR_STORE
	.byte	74
@FastBasic_LINE_245:	; LINE 245
	.byte	TOK_JUMP
	.word	jump_lbl_31
	.export	jump_lbl_30
jump_lbl_30:
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_BYTE
	.byte	52
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_32
@FastBasic_LINE_246:	; LINE 246
	.byte	TOK_BYTE
	.byte	51
	.byte	TOK_VAR_STORE
	.byte	106
@FastBasic_LINE_247:	; LINE 247
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_VAR_STORE
	.byte	74
@FastBasic_LINE_248:	; LINE 248
	.byte	TOK_JUMP
	.word	jump_lbl_31
	.export	jump_lbl_32
jump_lbl_32:
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_BYTE
	.byte	48
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_31
@FastBasic_LINE_249:	; LINE 249
	.byte	TOK_BYTE
	.byte	47
	.byte	TOK_VAR_STORE
	.byte	106
@FastBasic_LINE_250:	; LINE 250
	.byte	TOK_BYTE
	.byte	12
	.byte	TOK_VAR_STORE
	.byte	74
@FastBasic_LINE_251:	; LINE 251
	.export	jump_lbl_31
jump_lbl_31:
@FastBasic_LINE_252:	; LINE 252
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_VAR_LOAD
	.byte	5
	.byte	TOK_MUL
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	73
@FastBasic_LINE_253:	; LINE 253
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_PUSH_BYTE
	.byte	22
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	40
@FastBasic_LINE_254:	; LINE 254
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	47
	.byte	TOK_PUSH_BYTE
	.byte	5
	.byte	TOK_GT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_35
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_PUSH_BYTE
	.byte	28
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	40
	.export	jump_lbl_35
jump_lbl_35:
@FastBasic_LINE_255:	; LINE 255
	.byte	TOK_VAR_SADDR
	.byte	104
	.byte	TOK_PUSH_1
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	64
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_36
	.export	jump_lbl_37
jump_lbl_37:
@FastBasic_LINE_257:	; LINE 257
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_37
	.export	jump_lbl_36
jump_lbl_36:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_258:	; LINE 258
	.byte	TOK_VAR_STORE_0
	.byte	39
@FastBasic_LINE_259:	; LINE 259
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	105
@FastBasic_LINE_260:	; LINE 260
	.byte	TOK_VAR_SADDR
	.byte	104
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	106
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_38
	.export	jump_lbl_39
jump_lbl_39:
@FastBasic_LINE_261:	; LINE 261
	.byte	TOK_VAR_LOAD
	.byte	104
	.byte	TOK_PUSH_VAR_LOAD
	.byte	74
	.byte	TOK_MOD
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_40
	.byte	TOK_VAR_STORE_0
	.byte	39
	.export	jump_lbl_40
jump_lbl_40:
@FastBasic_LINE_262:	; LINE 262
	.byte	TOK_NUM
	.word	fb_lbl_BLACKJACKVALUE
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	98
@FastBasic_LINE_263:	; LINE 263
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_ADD_VAR
	.byte	104
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	98
	.byte	TOK_POKE
@FastBasic_LINE_264:	; LINE 264
	.byte	TOK_NUM
	.word	fb_lbl_POKERVALUE
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	98
@FastBasic_LINE_265:	; LINE 265
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_ADD_VAR
	.byte	104
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	98
	.byte	TOK_POKE
@FastBasic_LINE_266:	; LINE 266
	.byte	TOK_VAR_LOAD
	.byte	25
	.byte	TOK_ADD_VAR
	.byte	104
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_PUSH_VAR_LOAD
	.byte	104
	.byte	TOK_PUSH_VAR_LOAD
	.byte	74
	.byte	TOK_DIV
	.byte	TOK_INT_FP
	.byte	TOK_FP_INT
	.byte	TOK_ADD
	.byte	TOK_POKE
@FastBasic_LINE_267:	; LINE 267
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_VAR_STORE
	.byte	39
@FastBasic_LINE_268:	; LINE 268
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_39
	.export	jump_lbl_38
jump_lbl_38:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_269:	; LINE 269
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_VAR_STORE
	.byte	39
@FastBasic_LINE_270:	; LINE 270
	.byte	TOK_VAR_STORE_0
	.byte	71
@FastBasic_LINE_271:	; LINE 271
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_BYTE
	.byte	56
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_41
	.byte	TOK_BYTE
	.byte	14
	.byte	TOK_VAR_STORE
	.byte	74
	.export	jump_lbl_41
jump_lbl_41:
@FastBasic_LINE_272:	; LINE 272
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_1
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	17
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_42
	.export	jump_lbl_43
jump_lbl_43:
@FastBasic_LINE_273:	; LINE 273
	.byte	TOK_VAR_LOAD
	.byte	90
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	51
	.byte	TOK_SADDR
	.byte	TOK_NUM
	.word	5000
	.byte	TOK_DPOKE
@FastBasic_LINE_274:	; LINE 274
	.byte	TOK_VAR_LOAD
	.byte	90
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_275:	; LINE 275
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_43
	.export	jump_lbl_42
jump_lbl_42:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_276:	; LINE 276
	.byte	TOK_RET
@FastBasic_LINE_278:	; LINE 278
	.export	fb_lbl_CALCTOTAL
fb_lbl_CALCTOTAL:
@FastBasic_LINE_279:	; LINE 279
	.byte	TOK_VAR_STORE_0
	.byte	108
@FastBasic_LINE_280:	; LINE 280
	.byte	TOK_VAR_STORE_0
	.byte	109
@FastBasic_LINE_281:	; LINE 281
	.byte	TOK_VAR_STORE_0
	.byte	110
@FastBasic_LINE_282:	; LINE 282
	.byte	TOK_VAR_LOAD
	.byte	55
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_283:	; LINE 283
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_46
@FastBasic_LINE_284:	; LINE 284
	.byte	TOK_VAR_STORE_0
	.byte	108
@FastBasic_LINE_285:	; LINE 285
	.export	jump_lbl_46
jump_lbl_46:
@FastBasic_LINE_286:	; LINE 286
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_47
@FastBasic_LINE_287:	; LINE 287
	.byte	TOK_BYTE
	.byte	7
	.byte	TOK_VAR_STORE
	.byte	108
@FastBasic_LINE_288:	; LINE 288
	.export	jump_lbl_47
jump_lbl_47:
@FastBasic_LINE_289:	; LINE 289
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_48
@FastBasic_LINE_290:	; LINE 290
	.byte	TOK_BYTE
	.byte	14
	.byte	TOK_VAR_STORE
	.byte	108
@FastBasic_LINE_291:	; LINE 291
	.export	jump_lbl_48
jump_lbl_48:
@FastBasic_LINE_292:	; LINE 292
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_VAR_LOAD
	.byte	108
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_49
	.export	jump_lbl_50
jump_lbl_50:
@FastBasic_LINE_293:	; LINE 293
	.byte	TOK_VAR_LOAD
	.byte	109
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	109
@FastBasic_LINE_294:	; LINE 294
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	11
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_51
@FastBasic_LINE_295:	; LINE 295
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	110
	.byte	TOK_VAR_STORE
	.byte	110
@FastBasic_LINE_296:	; LINE 296
	.byte	TOK_VAR_LOAD
	.byte	55
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	55
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_POKE
@FastBasic_LINE_297:	; LINE 297
	.export	jump_lbl_51
jump_lbl_51:
@FastBasic_LINE_298:	; LINE 298
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_50
	.export	jump_lbl_49
jump_lbl_49:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_300:	; LINE 300
	.byte	TOK_VAR_LOAD
	.byte	110
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_52
@FastBasic_LINE_301:	; LINE 301
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	110
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_53
	.export	jump_lbl_54
jump_lbl_54:
@FastBasic_LINE_302:	; LINE 302
	.byte	TOK_VAR_LOAD
	.byte	109
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_55
	.byte	TOK_VAR_LOAD
	.byte	109
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	109
	.export	jump_lbl_55
jump_lbl_55:
@FastBasic_LINE_303:	; LINE 303
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_54
	.export	jump_lbl_53
jump_lbl_53:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_304:	; LINE 304
	.export	jump_lbl_52
jump_lbl_52:
@FastBasic_LINE_305:	; LINE 305
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_56
@FastBasic_LINE_306:	; LINE 306
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	109
	.byte	TOK_POKE
@FastBasic_LINE_307:	; LINE 307
	.export	jump_lbl_56
jump_lbl_56:
@FastBasic_LINE_308:	; LINE 308
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CRET
@FastBasic_LINE_309:	; LINE 309
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	109
	.byte	TOK_POKE
@FastBasic_LINE_311:	; LINE 311
	.byte	TOK_RET
@FastBasic_LINE_313:	; LINE 313
	.export	fb_lbl_PERFORM_DEAL
fb_lbl_PERFORM_DEAL:
@FastBasic_LINE_314:	; LINE 314
	.byte	TOK_VAR_STORE_0
	.byte	111
@FastBasic_LINE_315:	; LINE 315
	.byte	TOK_VAR_STORE_0
	.byte	112
@FastBasic_LINE_316:	; LINE 316
	.byte	TOK_VAR_STORE_0
	.byte	113
@FastBasic_LINE_317:	; LINE 317
	.byte	TOK_BYTE
	.byte	16
	.byte	TOK_VAR_STORE
	.byte	113
@FastBasic_LINE_318:	; LINE 318
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	42
	.byte	TOK_VAR_STORE
	.byte	42
@FastBasic_LINE_319:	; LINE 319
	.byte	TOK_BYTE
	.byte	CLS
	.byte	TOK_PUT
@FastBasic_LINE_320:	; LINE 320
	.byte	TOK_CALL
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_321:	; LINE 321
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_60
	.byte	TOK_BYTE
	.byte	14
	.byte	TOK_VAR_STORE
	.byte	113
	.export	jump_lbl_60
jump_lbl_60:
@FastBasic_LINE_322:	; LINE 322
	.byte	TOK_VAR_SADDR
	.byte	111
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_61
	.export	jump_lbl_62
jump_lbl_62:
@FastBasic_LINE_323:	; LINE 323
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_63
	.export	jump_lbl_64
jump_lbl_64:
@FastBasic_LINE_324:	; LINE 324
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	111
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_325:	; LINE 325
	.byte	TOK_VAR_LOAD
	.byte	57
	.byte	TOK_PUSH_VAR_LOAD
	.byte	111
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_326:	; LINE 326
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_64
	.export	jump_lbl_63
jump_lbl_63:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_327:	; LINE 327
	.byte	TOK_VAR_LOAD
	.byte	111
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_328:	; LINE 328
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_329:	; LINE 329
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_330:	; LINE 330
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_331:	; LINE 331
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_332:	; LINE 332
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_333:	; LINE 333
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_334:	; LINE 334
	.byte	TOK_VAR_LOAD
	.byte	55
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_335:	; LINE 335
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_336:	; LINE 336
	.byte	TOK_VAR_LOAD
	.byte	59
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_337:	; LINE 337
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_1
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_65
	.export	jump_lbl_66
jump_lbl_66:
@FastBasic_LINE_338:	; LINE 338
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_67
@FastBasic_LINE_339:	; LINE 339
	.byte	TOK_BYTE
	.byte	16
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_340:	; LINE 340
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_341:	; LINE 341
	.byte	TOK_VAR_LOAD
	.byte	90
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_70
@FastBasic_LINE_343:	; LINE 343
	.byte	TOK_VAR_LOAD
	.byte	39
	.byte	TOK_VAR_STORE
	.byte	38
@FastBasic_LINE_344:	; LINE 344
	.byte	TOK_JUMP
	.word	jump_lbl_70
@FastBasic_LINE_347:	; LINE 347
	.export	jump_lbl_67
jump_lbl_67:
@FastBasic_LINE_348:	; LINE 348
	.byte	TOK_BYTE
	.byte	16
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_349:	; LINE 349
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_350:	; LINE 350
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	640
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_70
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_352:	; LINE 352
	.export	jump_lbl_70
jump_lbl_70:
@FastBasic_LINE_354:	; LINE 354
	.byte	TOK_CALL
	.word	fb_lbl_ANIMATEHIT
@FastBasic_LINE_355:	; LINE 355
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	41
@FastBasic_LINE_356:	; LINE 356
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	41
	.byte	TOK_POKE
@FastBasic_LINE_357:	; LINE 357
	.byte	TOK_VAR_LOAD
	.byte	57
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_358:	; LINE 358
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	25
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_359:	; LINE 359
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_72
@FastBasic_LINE_360:	; LINE 360
	.byte	TOK_VAR_LOAD
	.byte	90
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_73
@FastBasic_LINE_361:	; LINE 361
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_74
@FastBasic_LINE_362:	; LINE 362
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_363:	; LINE 363
	.byte	TOK_JUMP
	.word	jump_lbl_73
	.export	jump_lbl_74
jump_lbl_74:
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_76
@FastBasic_LINE_364:	; LINE 364
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_365:	; LINE 365
	.byte	TOK_JUMP
	.word	jump_lbl_73
	.export	jump_lbl_76
jump_lbl_76:
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_73
@FastBasic_LINE_366:	; LINE 366
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	11
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_79
@FastBasic_LINE_367:	; LINE 367
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_368:	; LINE 368
	.byte	TOK_JUMP
	.word	jump_lbl_73
	.export	jump_lbl_79
jump_lbl_79:
@FastBasic_LINE_369:	; LINE 369
	.byte	TOK_BYTE
	.byte	255
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_372:	; LINE 372
	.export	jump_lbl_73
jump_lbl_73:
@FastBasic_LINE_373:	; LINE 373
	.byte	TOK_VAR_LOAD
	.byte	90
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_72
@FastBasic_LINE_374:	; LINE 374
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_82
@FastBasic_LINE_375:	; LINE 375
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_376:	; LINE 376
	.byte	TOK_JUMP
	.word	jump_lbl_83
	.export	jump_lbl_82
jump_lbl_82:
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_84
@FastBasic_LINE_377:	; LINE 377
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_378:	; LINE 378
	.byte	TOK_JUMP
	.word	jump_lbl_83
	.export	jump_lbl_84
jump_lbl_84:
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_83
@FastBasic_LINE_379:	; LINE 379
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	11
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_87
@FastBasic_LINE_380:	; LINE 380
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_381:	; LINE 381
	.byte	TOK_JUMP
	.word	jump_lbl_83
	.export	jump_lbl_87
jump_lbl_87:
@FastBasic_LINE_382:	; LINE 382
	.byte	TOK_BYTE
	.byte	254
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_384:	; LINE 384
	.export	jump_lbl_83
jump_lbl_83:
@FastBasic_LINE_385:	; LINE 385
	.byte	TOK_VAR_LOAD
	.byte	83
	.byte	TOK_PUSH_VAR_LOAD
	.byte	84
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_72
@FastBasic_LINE_386:	; LINE 386
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_POKE
@FastBasic_LINE_387:	; LINE 387
	.byte	TOK_VAR_LOAD
	.byte	59
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_390:	; LINE 390
	.export	jump_lbl_72
jump_lbl_72:
@FastBasic_LINE_391:	; LINE 391
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_VAR_STORE
	.byte	39
@FastBasic_LINE_392:	; LINE 392
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_66
	.export	jump_lbl_65
jump_lbl_65:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_393:	; LINE 393
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_POKE
@FastBasic_LINE_394:	; LINE 394
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_90
@FastBasic_LINE_395:	; LINE 395
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_396:	; LINE 396
	.byte	TOK_JUMP
	.word	jump_lbl_91
	.export	jump_lbl_90
jump_lbl_90:
@FastBasic_LINE_397:	; LINE 397
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_398:	; LINE 398
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_92
@FastBasic_LINE_399:	; LINE 399
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	19, "TOTAL:21  BLACKJACK"
	.byte	TOK_COPY_STR
@FastBasic_LINE_400:	; LINE 400
	.byte	TOK_JUMP
	.word	jump_lbl_93
	.export	jump_lbl_92
jump_lbl_92:
@FastBasic_LINE_401:	; LINE 401
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	11, "TOTAL:     "
	.byte	TOK_COPY_STR
@FastBasic_LINE_402:	; LINE 402
	.export	jump_lbl_93
jump_lbl_93:
@FastBasic_LINE_403:	; LINE 403
	.byte	TOK_CALL
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_404:	; LINE 404
	.export	jump_lbl_91
jump_lbl_91:
@FastBasic_LINE_405:	; LINE 405
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_62
	.export	jump_lbl_61
jump_lbl_61:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_406:	; LINE 406
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_407:	; LINE 407
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_94
@FastBasic_LINE_408:	; LINE 408
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_409:	; LINE 409
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_94
@FastBasic_LINE_410:	; LINE 410
	.byte	TOK_BYTE
	.byte	3
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_411:	; LINE 411
	.byte	TOK_BYTE
	.byte	3
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_94
@FastBasic_LINE_412:	; LINE 412
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_413:	; LINE 413
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_94
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	17
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_416:	; LINE 416
	.export	jump_lbl_94
jump_lbl_94:
@FastBasic_LINE_417:	; LINE 417
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_CRET
@FastBasic_LINE_418:	; LINE 418
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	56
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	11
	.byte	TOK_EQ
	.byte	TOK_CRET
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	17
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_420:	; LINE 420
	.byte	TOK_RET
@FastBasic_LINE_422:	; LINE 422
	.export	fb_lbl_ANIMATEHIT
fb_lbl_ANIMATEHIT:
@FastBasic_LINE_423:	; LINE 423
	.byte	TOK_VAR_STORE_0
	.byte	114
@FastBasic_LINE_424:	; LINE 424
	.byte	TOK_NUM
	.word	65512
	.byte	TOK_VAR_STORE
	.byte	115
@FastBasic_LINE_425:	; LINE 425
	.byte	TOK_VAR_STORE_0
	.byte	90
@FastBasic_LINE_426:	; LINE 426
	.export	jump_lbl_102
jump_lbl_102:
@FastBasic_LINE_427:	; LINE 427
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	114
	.byte	TOK_VAR_STORE
	.byte	114
@FastBasic_LINE_428:	; LINE 428
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	115
	.byte	TOK_VAR_STORE
	.byte	115
@FastBasic_LINE_429:	; LINE 429
	.byte	TOK_DECVAR
	.byte	90
@FastBasic_LINE_430:	; LINE 430
	.byte	TOK_VAR_LOAD
	.byte	90
	.byte	TOK_PUSH_1
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_102
@FastBasic_LINE_431:	; LINE 431
	.byte	TOK_JUMP
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_434:	; LINE 434
	.export	fb_lbl_PERFORM_PLAYERINPUT
fb_lbl_PERFORM_PLAYERINPUT:
@FastBasic_LINE_436:	; LINE 436
	.byte	TOK_VAR_STORE_0
	.byte	116
@FastBasic_LINE_437:	; LINE 437
	.byte	TOK_VAR_STORE_0
	.byte	44
@FastBasic_LINE_438:	; LINE 438
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_439:	; LINE 439
	.byte	TOK_BYTE
	.byte	CLS
	.byte	TOK_PUT
@FastBasic_LINE_440:	; LINE 440
	.export	jump_lbl_105
jump_lbl_105:
@FastBasic_LINE_441:	; LINE 441
	.byte	TOK_CALL
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_442:	; LINE 442
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	51
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_106
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_DPOKE
	.export	jump_lbl_106
jump_lbl_106:
@FastBasic_LINE_443:	; LINE 443
	.byte	TOK_VAR_LOAD
	.byte	1
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_LT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	11
	.byte	TOK_GT
	.byte	TOK_L_OR
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_107
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_DPOKE
	.export	jump_lbl_107
jump_lbl_107:
@FastBasic_LINE_444:	; LINE 444
	.byte	TOK_VAR_LOAD
	.byte	1
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	9
	.byte	TOK_LT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	11
	.byte	TOK_GT
	.byte	TOK_L_OR
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_108
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_DPOKE
	.export	jump_lbl_108
jump_lbl_108:
@FastBasic_LINE_445:	; LINE 445
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_LT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_109
@FastBasic_LINE_446:	; LINE 446
	.byte	TOK_CALL
	.word	fb_lbl_SHOWCOMMANDS
@FastBasic_LINE_448:	; LINE 448
	.byte	TOK_GETKEY
	.byte	TOK_VAR_STORE
	.byte	65
@FastBasic_LINE_449:	; LINE 449
	.byte	TOK_VAR_LOAD
	.byte	65
	.byte	TOK_PUSH_BYTE
	.byte	96
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	65
	.byte	TOK_PUSH_BYTE
	.byte	123
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_110
	.byte	TOK_VAR_LOAD
	.byte	65
	.byte	TOK_PUSH_BYTE
	.byte	32
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	65
	.export	jump_lbl_110
jump_lbl_110:
@FastBasic_LINE_450:	; LINE 450
	.byte	TOK_VAR_SADDR
	.byte	63
	.byte	TOK_VAR_LOAD
	.byte	65
	.byte	TOK_CHR
	.byte	TOK_COPY_STR
@FastBasic_LINE_451:	; LINE 451
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "H"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_111
@FastBasic_LINE_452:	; LINE 452
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_HIT
@FastBasic_LINE_453:	; LINE 453
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	2
	.byte	TOK_PUSH_VAR_LOAD
	.byte	19
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_112
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_DPOKE
	.export	jump_lbl_112
jump_lbl_112:
@FastBasic_LINE_454:	; LINE 454
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	2
	.byte	TOK_PUSH_VAR_LOAD
	.byte	18
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_111
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_DPOKE
@FastBasic_LINE_455:	; LINE 455
	.export	jump_lbl_111
jump_lbl_111:
@FastBasic_LINE_456:	; LINE 456
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "S"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_114
@FastBasic_LINE_457:	; LINE 457
	.byte	TOK_VAR_LOAD
	.byte	18
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_458:	; LINE 458
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_115
@FastBasic_LINE_459:	; LINE 459
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_460:	; LINE 460
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_461:	; LINE 461
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	10, "  STANDING"
	.byte	TOK_CAT_STR
@FastBasic_LINE_462:	; LINE 462
	.export	jump_lbl_115
jump_lbl_115:
@FastBasic_LINE_463:	; LINE 463
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_116
@FastBasic_LINE_464:	; LINE 464
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_465:	; LINE 465
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_466:	; LINE 466
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	9, " STAND|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_467:	; LINE 467
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_468:	; LINE 468
	.export	jump_lbl_116
jump_lbl_116:
@FastBasic_LINE_469:	; LINE 469
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_114
@FastBasic_LINE_470:	; LINE 470
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_471:	; LINE 471
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_472:	; LINE 472
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, "|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_473:	; LINE 473
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_474:	; LINE 474
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, " STAND"
	.byte	TOK_CAT_STR
@FastBasic_LINE_476:	; LINE 476
	.export	jump_lbl_114
jump_lbl_114:
@FastBasic_LINE_477:	; LINE 477
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "D"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_DPEEK
	.byte	TOK_COMP_0
	.byte	TOK_L_NOT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_118
@FastBasic_LINE_478:	; LINE 478
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_479:	; LINE 479
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_HIT
@FastBasic_LINE_480:	; LINE 480
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_481:	; LINE 481
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_DPOKE
@FastBasic_LINE_482:	; LINE 482
	.export	jump_lbl_118
jump_lbl_118:
@FastBasic_LINE_483:	; LINE 483
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "T"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_119
@FastBasic_LINE_484:	; LINE 484
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_SPLITDEAL
@FastBasic_LINE_485:	; LINE 485
	.export	jump_lbl_119
jump_lbl_119:
@FastBasic_LINE_486:	; LINE 486
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "U"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	67
	.byte	TOK_PUSH_VAR_LOAD
	.byte	18
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_120
@FastBasic_LINE_487:	; LINE 487
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_NEG
	.byte	TOK_POKE
@FastBasic_LINE_488:	; LINE 488
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	11, "SURRENDERED"
	.byte	TOK_COPY_STR
@FastBasic_LINE_489:	; LINE 489
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_490:	; LINE 490
	.export	jump_lbl_120
jump_lbl_120:
@FastBasic_LINE_491:	; LINE 491
	.byte	TOK_VAR_LOAD
	.byte	63
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "Q"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_121
@FastBasic_LINE_492:	; LINE 492
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_493:	; LINE 493
	.export	jump_lbl_121
jump_lbl_121:
@FastBasic_LINE_494:	; LINE 494
	.byte	TOK_VAR_SADDR
	.byte	63
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_495:	; LINE 495
	.byte	TOK_JUMP
	.word	jump_lbl_122
	.export	jump_lbl_109
jump_lbl_109:
@FastBasic_LINE_496:	; LINE 496
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_497:	; LINE 497
	.export	jump_lbl_122
jump_lbl_122:
@FastBasic_LINE_498:	; LINE 498
	.byte	TOK_VAR_LOAD
	.byte	44
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_105
@FastBasic_LINE_499:	; LINE 499
	.byte	TOK_RET
@FastBasic_LINE_501:	; LINE 501
	.export	fb_lbl_PERFORM_DEALER
fb_lbl_PERFORM_DEALER:
@FastBasic_LINE_502:	; LINE 502
	.byte	TOK_BYTE
	.byte	100
	.byte	TOK_VAR_STORE
	.byte	102
@FastBasic_LINE_503:	; LINE 503
	.byte	TOK_VAR_STORE_0
	.byte	83
@FastBasic_LINE_504:	; LINE 504
	.export	jump_lbl_125
jump_lbl_125:
@FastBasic_LINE_505:	; LINE 505
	.byte	TOK_VAR_LOAD
	.byte	102
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	102
@FastBasic_LINE_507:	; LINE 507
	.byte	TOK_VAR_LOAD
	.byte	102
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_125
@FastBasic_LINE_508:	; LINE 508
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	640
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_126
@FastBasic_LINE_509:	; LINE 509
	.byte	TOK_BYTE
	.byte	68
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_510:	; LINE 510
	.byte	TOK_JUMP
	.word	jump_lbl_128
	.export	jump_lbl_126
jump_lbl_126:
@FastBasic_LINE_511:	; LINE 511
	.byte	TOK_BYTE
	.byte	76
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_514:	; LINE 514
	.export	jump_lbl_128
jump_lbl_128:
@FastBasic_LINE_515:	; LINE 515
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	102
	.byte	TOK_VAR_STORE
	.byte	102
@FastBasic_LINE_517:	; LINE 517
	.byte	TOK_VAR_LOAD
	.byte	102
	.byte	TOK_PUSH_BYTE
	.byte	99
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_128
@FastBasic_LINE_518:	; LINE 518
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_519:	; LINE 519
	.byte	TOK_VAR_STORE_0
	.byte	43
@FastBasic_LINE_520:	; LINE 520
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_129
@FastBasic_LINE_521:	; LINE 521
	.export	jump_lbl_130
jump_lbl_130:
@FastBasic_LINE_522:	; LINE 522
	.byte	TOK_VAR_LOAD
	.byte	11
	.byte	TOK_PUSH_VAR_LOAD
	.byte	19
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_131
@FastBasic_LINE_523:	; LINE 523
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	17
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_133
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_HIT
@FastBasic_LINE_524:	; LINE 524
	.byte	TOK_JUMP
	.word	jump_lbl_133
	.export	jump_lbl_131
jump_lbl_131:
@FastBasic_LINE_525:	; LINE 525
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	17
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_134
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_HIT
	.export	jump_lbl_134
jump_lbl_134:
@FastBasic_LINE_526:	; LINE 526
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_527:	; LINE 527
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	17
	.byte	TOK_EQ
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	55
	.byte	TOK_PEEK
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_135
	.byte	TOK_CALL
	.word	fb_lbl_PERFORM_HIT
	.export	jump_lbl_135
jump_lbl_135:
@FastBasic_LINE_528:	; LINE 528
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_529:	; LINE 529
	.export	jump_lbl_133
jump_lbl_133:
@FastBasic_LINE_530:	; LINE 530
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	16
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_130
@FastBasic_LINE_531:	; LINE 531
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	22
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_136
@FastBasic_LINE_532:	; LINE 532
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_533:	; LINE 533
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_534:	; LINE 534
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	11, "   STANDING"
	.byte	TOK_CAT_STR
@FastBasic_LINE_535:	; LINE 535
	.byte	TOK_JUMP
	.word	fb_lbl_SHOWSTATS
	.export	jump_lbl_136
jump_lbl_136:
@FastBasic_LINE_536:	; LINE 536
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_537:	; LINE 537
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_538:	; LINE 538
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	9, "   BUSTED"
	.byte	TOK_CAT_STR
@FastBasic_LINE_540:	; LINE 540
	.byte	TOK_JUMP
	.word	fb_lbl_SHOWSTATS
	.export	jump_lbl_129
jump_lbl_129:
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	49
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_541:	; LINE 541
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_542:	; LINE 542
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_543:	; LINE 543
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	11, "  BLACKJACK"
	.byte	TOK_CAT_STR
@FastBasic_LINE_545:	; LINE 545
	.byte	TOK_JUMP
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_548:	; LINE 548
	.export	fb_lbl_PERFORM_HIT
fb_lbl_PERFORM_HIT:
@FastBasic_LINE_549:	; LINE 549
	.byte	TOK_BYTE
	.byte	16
	.byte	TOK_VAR_STORE
	.byte	113
@FastBasic_LINE_550:	; LINE 550
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_VAR_STORE
	.byte	111
@FastBasic_LINE_551:	; LINE 551
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	111
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	90
@FastBasic_LINE_552:	; LINE 552
	.byte	TOK_VAR_LOAD
	.byte	111
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_142
@FastBasic_LINE_553:	; LINE 553
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	49
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	90
@FastBasic_LINE_554:	; LINE 554
	.byte	TOK_VAR_STORE_0
	.byte	83
@FastBasic_LINE_555:	; LINE 555
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	640
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_143
@FastBasic_LINE_556:	; LINE 556
	.byte	TOK_BYTE
	.byte	58
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_557:	; LINE 557
	.byte	TOK_JUMP
	.word	jump_lbl_145
	.export	jump_lbl_143
jump_lbl_143:
@FastBasic_LINE_558:	; LINE 558
	.byte	TOK_BYTE
	.byte	64
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_560:	; LINE 560
	.byte	TOK_JUMP
	.word	jump_lbl_145
	.export	jump_lbl_142
jump_lbl_142:
@FastBasic_LINE_561:	; LINE 561
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_146
	.byte	TOK_BYTE
	.byte	14
	.byte	TOK_VAR_STORE
	.byte	113
	.export	jump_lbl_146
jump_lbl_146:
@FastBasic_LINE_562:	; LINE 562
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_147
@FastBasic_LINE_563:	; LINE 563
	.byte	TOK_VAR_LOAD
	.byte	32
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_ADD
	.byte	TOK_PUSH_VAR_LOAD
	.byte	113
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_564:	; LINE 564
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_565:	; LINE 565
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	640
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_145
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_566:	; LINE 566
	.byte	TOK_JUMP
	.word	jump_lbl_145
	.export	jump_lbl_147
jump_lbl_147:
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_145
@FastBasic_LINE_567:	; LINE 567
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	111
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	90
@FastBasic_LINE_568:	; LINE 568
	.byte	TOK_VAR_LOAD
	.byte	32
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	78
	.byte	TOK_ADD
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_INT_FP
	.byte	TOK_FLOAT
	.byte	$3F, $11, $00, $00, $00, $00
	.byte	TOK_FP_MUL
	.byte	TOK_FP_INT
	.byte	TOK_ADD
	.byte	TOK_PUSH_VAR_LOAD
	.byte	113
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_569:	; LINE 569
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_570:	; LINE 570
	.byte	TOK_VAR_LOAD
	.byte	37
	.byte	TOK_PUSH_NUM
	.word	640
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_145
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_PUSH_VAR_LOAD
	.byte	90
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_572:	; LINE 572
	.export	jump_lbl_145
jump_lbl_145:
@FastBasic_LINE_573:	; LINE 573
	.byte	TOK_CALL
	.word	fb_lbl_ANIMATEHIT
@FastBasic_LINE_574:	; LINE 574
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	41
@FastBasic_LINE_575:	; LINE 575
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	41
	.byte	TOK_POKE
@FastBasic_LINE_576:	; LINE 576
	.byte	TOK_VAR_LOAD
	.byte	57
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_577:	; LINE 577
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	25
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_578:	; LINE 578
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_VAR_STORE
	.byte	39
@FastBasic_LINE_579:	; LINE 579
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_POKE
@FastBasic_LINE_580:	; LINE 580
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_581:	; LINE 581
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_NEQ
@FastBasic_LINE_583:	; LINE 583
	.byte	TOK_CNJUMP
	.word	jump_lbl_153
@FastBasic_LINE_585:	; LINE 585
	.export	jump_lbl_153
jump_lbl_153:
@FastBasic_LINE_586:	; LINE 586
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	200
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_154
@FastBasic_LINE_587:	; LINE 587
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_155
@FastBasic_LINE_588:	; LINE 588
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_589:	; LINE 589
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_590:	; LINE 590
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	8, "  BUSTED"
	.byte	TOK_CAT_STR
@FastBasic_LINE_591:	; LINE 591
	.byte	TOK_VAR_LOAD
	.byte	18
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_592:	; LINE 592
	.export	jump_lbl_155
jump_lbl_155:
@FastBasic_LINE_593:	; LINE 593
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	22
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_156
@FastBasic_LINE_594:	; LINE 594
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_595:	; LINE 595
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_596:	; LINE 596
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	8, " BUST|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_597:	; LINE 597
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_598:	; LINE 598
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_156
	.byte	TOK_VAR_LOAD
	.byte	18
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_599:	; LINE 599
	.export	jump_lbl_156
jump_lbl_156:
@FastBasic_LINE_600:	; LINE 600
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_GT
	.byte	TOK_L_AND
	.byte	TOK_CRET
@FastBasic_LINE_601:	; LINE 601
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_602:	; LINE 602
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_603:	; LINE 603
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, "|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_604:	; LINE 604
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_605:	; LINE 605
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	5, " BUST"
	.byte	TOK_CAT_STR
@FastBasic_LINE_607:	; LINE 607
	.byte	TOK_RET
	.export	jump_lbl_154
jump_lbl_154:
@FastBasic_LINE_608:	; LINE 608
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_160
@FastBasic_LINE_609:	; LINE 609
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_610:	; LINE 610
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_611:	; LINE 611
	.byte	TOK_JUMP
	.word	jump_lbl_161
	.export	jump_lbl_160
jump_lbl_160:
@FastBasic_LINE_612:	; LINE 612
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_613:	; LINE 613
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_614:	; LINE 614
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, "|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_615:	; LINE 615
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_616:	; LINE 616
	.export	jump_lbl_161
jump_lbl_161:
@FastBasic_LINE_617:	; LINE 617
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	13
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_PUSH_VAR_LOAD
	.byte	12
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_162
@FastBasic_LINE_618:	; LINE 618
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_163
@FastBasic_LINE_619:	; LINE 619
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_620:	; LINE 620
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_621:	; LINE 621
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	17, " SIX CARD CHARLIE"
	.byte	TOK_CAT_STR
@FastBasic_LINE_622:	; LINE 622
	.byte	TOK_JUMP
	.word	jump_lbl_164
	.export	jump_lbl_163
jump_lbl_163:
@FastBasic_LINE_623:	; LINE 623
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_624:	; LINE 624
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_625:	; LINE 625
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, " C6|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_626:	; LINE 626
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_627:	; LINE 627
	.export	jump_lbl_164
jump_lbl_164:
@FastBasic_LINE_628:	; LINE 628
	.byte	TOK_VAR_LOAD
	.byte	18
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_629:	; LINE 629
	.export	jump_lbl_162
jump_lbl_162:
@FastBasic_LINE_630:	; LINE 630
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	5
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_PUSH_VAR_LOAD
	.byte	12
	.byte	TOK_PUSH_BYTE
	.byte	5
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CRET
@FastBasic_LINE_631:	; LINE 631
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_166
@FastBasic_LINE_632:	; LINE 632
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_633:	; LINE 633
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_634:	; LINE 634
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	18, " FIVE CARD CHARLIE"
	.byte	TOK_CAT_STR
@FastBasic_LINE_635:	; LINE 635
	.byte	TOK_JUMP
	.word	jump_lbl_167
	.export	jump_lbl_166
jump_lbl_166:
@FastBasic_LINE_636:	; LINE 636
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_637:	; LINE 637
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_638:	; LINE 638
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, " C5|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_639:	; LINE 639
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_640:	; LINE 640
	.export	jump_lbl_167
jump_lbl_167:
@FastBasic_LINE_641:	; LINE 641
	.byte	TOK_VAR_LOAD
	.byte	18
	.byte	TOK_VAR_STORE
	.byte	44
@FastBasic_LINE_644:	; LINE 644
	.byte	TOK_RET
@FastBasic_LINE_646:	; LINE 646
	.export	fb_lbl_PERFORM_REWARDS
fb_lbl_PERFORM_REWARDS:
@FastBasic_LINE_647:	; LINE 647
	.byte	TOK_VAR_STORE_0
	.byte	117
@FastBasic_LINE_648:	; LINE 648
	.byte	TOK_VAR_STORE_0
	.byte	111
@FastBasic_LINE_649:	; LINE 649
	.byte	TOK_VAR_STORE_0
	.byte	118
@FastBasic_LINE_650:	; LINE 650
	.byte	TOK_VAR_STORE_0
	.byte	119
@FastBasic_LINE_651:	; LINE 651
	.byte	TOK_VAR_STORE_0
	.byte	120
@FastBasic_LINE_652:	; LINE 652
	.byte	TOK_VAR_STORE_0
	.byte	121
@FastBasic_LINE_653:	; LINE 653
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_654:	; LINE 654
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_655:	; LINE 655
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_656:	; LINE 656
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_657:	; LINE 657
	.byte	TOK_VAR_SADDR
	.byte	111
	.byte	TOK_PUSH_1
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_170
	.export	jump_lbl_171
jump_lbl_171:
@FastBasic_LINE_658:	; LINE 658
	.byte	TOK_VAR_LOAD
	.byte	111
	.byte	TOK_VAR_STORE
	.byte	43
@FastBasic_LINE_659:	; LINE 659
	.byte	TOK_VAR_STORE_0
	.byte	120
@FastBasic_LINE_660:	; LINE 660
	.byte	TOK_VAR_STORE_0
	.byte	119
@FastBasic_LINE_661:	; LINE 661
	.export	jump_lbl_172
jump_lbl_172:
@FastBasic_LINE_662:	; LINE 662
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_173
@FastBasic_LINE_663:	; LINE 663
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_664:	; LINE 664
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_665:	; LINE 665
	.byte	TOK_VAR_SADDR
	.byte	79
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_174
	.export	jump_lbl_175
jump_lbl_175:
@FastBasic_LINE_666:	; LINE 666
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_176
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_POKE
	.export	jump_lbl_176
jump_lbl_176:
@FastBasic_LINE_667:	; LINE 667
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_175
	.export	jump_lbl_174
jump_lbl_174:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_668:	; LINE 668
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_177
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	121
	.export	jump_lbl_177
jump_lbl_177:
@FastBasic_LINE_669:	; LINE 669
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_178
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	121
	.export	jump_lbl_178
jump_lbl_178:
@FastBasic_LINE_670:	; LINE 670
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_173
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	121
@FastBasic_LINE_671:	; LINE 671
	.export	jump_lbl_173
jump_lbl_173:
@FastBasic_LINE_672:	; LINE 672
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_180
@FastBasic_LINE_673:	; LINE 673
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_181
@FastBasic_LINE_674:	; LINE 674
	.byte	TOK_VAR_STORE_0
	.byte	79
@FastBasic_LINE_675:	; LINE 675
	.byte	TOK_VAR_LOAD
	.byte	15
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_182
@FastBasic_LINE_676:	; LINE 676
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_183
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_183
jump_lbl_183:
@FastBasic_LINE_677:	; LINE 677
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_184
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_184
jump_lbl_184:
@FastBasic_LINE_678:	; LINE 678
	.byte	TOK_VAR_LOAD
	.byte	79
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_185
@FastBasic_LINE_679:	; LINE 679
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_680:	; LINE 680
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_186
@FastBasic_LINE_681:	; LINE 681
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	21, "BLACKJACK BLACK  WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_682:	; LINE 682
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_683:	; LINE 683
	.export	jump_lbl_186
jump_lbl_186:
@FastBasic_LINE_684:	; LINE 684
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_685:	; LINE 685
	.byte	TOK_JUMP
	.word	jump_lbl_182
	.export	jump_lbl_185
jump_lbl_185:
@FastBasic_LINE_686:	; LINE 686
	.byte	TOK_VAR_STORE_0
	.byte	79
@FastBasic_LINE_687:	; LINE 687
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_188
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_188
jump_lbl_188:
@FastBasic_LINE_688:	; LINE 688
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_189
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_189
jump_lbl_189:
@FastBasic_LINE_689:	; LINE 689
	.byte	TOK_VAR_LOAD
	.byte	79
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_190
@FastBasic_LINE_690:	; LINE 690
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_691:	; LINE 691
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_191
@FastBasic_LINE_692:	; LINE 692
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	21, "BLACKJACK BLACK  WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_693:	; LINE 693
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_694:	; LINE 694
	.export	jump_lbl_191
jump_lbl_191:
@FastBasic_LINE_695:	; LINE 695
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_696:	; LINE 696
	.byte	TOK_JUMP
	.word	jump_lbl_182
	.export	jump_lbl_190
jump_lbl_190:
@FastBasic_LINE_697:	; LINE 697
	.byte	TOK_VAR_STORE_0
	.byte	79
@FastBasic_LINE_700:	; LINE 700
	.export	jump_lbl_182
jump_lbl_182:
@FastBasic_LINE_701:	; LINE 701
	.byte	TOK_VAR_LOAD
	.byte	79
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_181
@FastBasic_LINE_702:	; LINE 702
	.byte	TOK_VAR_LOAD
	.byte	14
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_194
@FastBasic_LINE_703:	; LINE 703
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_704:	; LINE 704
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_195
@FastBasic_LINE_705:	; LINE 705
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	23, "SUITED BLACKJACK   WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_706:	; LINE 706
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_707:	; LINE 707
	.export	jump_lbl_195
jump_lbl_195:
@FastBasic_LINE_708:	; LINE 708
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_709:	; LINE 709
	.byte	TOK_JUMP
	.word	jump_lbl_181
	.export	jump_lbl_194
jump_lbl_194:
@FastBasic_LINE_710:	; LINE 710
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_FP
	.byte	TOK_FLOAT
	.byte	$40, $01, $50, $00, $00, $00
	.byte	TOK_FP_MUL
	.byte	TOK_FP_INT
	.byte	TOK_DPOKE
@FastBasic_LINE_711:	; LINE 711
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_197
@FastBasic_LINE_712:	; LINE 712
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	16, "BLACKJACK   WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_713:	; LINE 713
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_FP
	.byte	TOK_FLOAT
	.byte	$40, $01, $50, $00, $00, $00
	.byte	TOK_FP_MUL
	.byte	TOK_FP_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_714:	; LINE 714
	.export	jump_lbl_197
jump_lbl_197:
@FastBasic_LINE_715:	; LINE 715
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_718:	; LINE 718
	.export	jump_lbl_181
jump_lbl_181:
@FastBasic_LINE_719:	; LINE 719
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_228
@FastBasic_LINE_720:	; LINE 720
	.byte	TOK_VAR_STORE_0
	.byte	118
@FastBasic_LINE_721:	; LINE 721
	.byte	TOK_VAR_LOAD
	.byte	8
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_199
@FastBasic_LINE_722:	; LINE 722
	.byte	TOK_VAR_STORE_0
	.byte	79
@FastBasic_LINE_723:	; LINE 723
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_200
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_200
jump_lbl_200:
@FastBasic_LINE_724:	; LINE 724
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_201
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_201
jump_lbl_201:
@FastBasic_LINE_725:	; LINE 725
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_202
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_202
jump_lbl_202:
@FastBasic_LINE_726:	; LINE 726
	.byte	TOK_VAR_LOAD
	.byte	79
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_203
@FastBasic_LINE_727:	; LINE 727
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_204
@FastBasic_LINE_728:	; LINE 728
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_205
@FastBasic_LINE_729:	; LINE 729
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	28, "21  SUITED 777 BONUS    WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_730:	; LINE 730
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_USHL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_731:	; LINE 731
	.export	jump_lbl_205
jump_lbl_205:
@FastBasic_LINE_732:	; LINE 732
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_733:	; LINE 733
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	118
@FastBasic_LINE_734:	; LINE 734
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_735:	; LINE 735
	.byte	TOK_JUMP
	.word	jump_lbl_199
	.export	jump_lbl_204
jump_lbl_204:
@FastBasic_LINE_736:	; LINE 736
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_207
@FastBasic_LINE_737:	; LINE 737
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	20, "21  777 BONUS   WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_738:	; LINE 738
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_MUL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_739:	; LINE 739
	.export	jump_lbl_207
jump_lbl_207:
@FastBasic_LINE_740:	; LINE 740
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_MUL
	.byte	TOK_DPOKE
@FastBasic_LINE_741:	; LINE 741
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	118
@FastBasic_LINE_742:	; LINE 742
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_744:	; LINE 744
	.byte	TOK_JUMP
	.word	jump_lbl_199
	.export	jump_lbl_203
jump_lbl_203:
@FastBasic_LINE_745:	; LINE 745
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_VAR_LOAD
	.byte	14
	.byte	TOK_PUSH_VAR_LOAD
	.byte	18
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_VAR_LOAD
	.byte	118
	.byte	TOK_PUSH_VAR_LOAD
	.byte	19
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_199
@FastBasic_LINE_746:	; LINE 746
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_210
@FastBasic_LINE_747:	; LINE 747
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	23, "SUITED 21 BONUS    WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_748:	; LINE 748
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_749:	; LINE 749
	.export	jump_lbl_210
jump_lbl_210:
@FastBasic_LINE_750:	; LINE 750
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_751:	; LINE 751
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	118
@FastBasic_LINE_752:	; LINE 752
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_755:	; LINE 755
	.export	jump_lbl_199
jump_lbl_199:
@FastBasic_LINE_756:	; LINE 756
	.byte	TOK_VAR_LOAD
	.byte	9
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	118
	.byte	TOK_COMP_0
	.byte	TOK_L_NOT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_211
@FastBasic_LINE_757:	; LINE 757
	.byte	TOK_VAR_STORE_0
	.byte	79
@FastBasic_LINE_758:	; LINE 758
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_212
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_212
jump_lbl_212:
@FastBasic_LINE_759:	; LINE 759
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_213
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_213
jump_lbl_213:
@FastBasic_LINE_760:	; LINE 760
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_214
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	79
	.byte	TOK_VAR_STORE
	.byte	79
	.export	jump_lbl_214
jump_lbl_214:
@FastBasic_LINE_761:	; LINE 761
	.byte	TOK_VAR_LOAD
	.byte	79
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_215
@FastBasic_LINE_762:	; LINE 762
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_216
@FastBasic_LINE_763:	; LINE 763
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_217
@FastBasic_LINE_764:	; LINE 764
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	27, "21  SUITED 678 BONUS   WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_765:	; LINE 765
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_USHL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_766:	; LINE 766
	.export	jump_lbl_217
jump_lbl_217:
@FastBasic_LINE_767:	; LINE 767
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_768:	; LINE 768
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	118
@FastBasic_LINE_769:	; LINE 769
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_770:	; LINE 770
	.byte	TOK_JUMP
	.word	jump_lbl_211
	.export	jump_lbl_216
jump_lbl_216:
@FastBasic_LINE_771:	; LINE 771
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_219
@FastBasic_LINE_772:	; LINE 772
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	19, "21  678 BONUS  WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_773:	; LINE 773
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_MUL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_774:	; LINE 774
	.export	jump_lbl_219
jump_lbl_219:
@FastBasic_LINE_775:	; LINE 775
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_MUL
	.byte	TOK_DPOKE
@FastBasic_LINE_776:	; LINE 776
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	118
@FastBasic_LINE_777:	; LINE 777
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_779:	; LINE 779
	.byte	TOK_JUMP
	.word	jump_lbl_211
	.export	jump_lbl_215
jump_lbl_215:
@FastBasic_LINE_780:	; LINE 780
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_VAR_LOAD
	.byte	14
	.byte	TOK_PUSH_VAR_LOAD
	.byte	18
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_VAR_LOAD
	.byte	118
	.byte	TOK_PUSH_VAR_LOAD
	.byte	19
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_211
@FastBasic_LINE_781:	; LINE 781
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_222
@FastBasic_LINE_782:	; LINE 782
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	21, "SUITED 21 BONUS  WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_783:	; LINE 783
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_784:	; LINE 784
	.export	jump_lbl_222
jump_lbl_222:
@FastBasic_LINE_785:	; LINE 785
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_USHL
	.byte	TOK_DPOKE
@FastBasic_LINE_786:	; LINE 786
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	118
@FastBasic_LINE_787:	; LINE 787
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_790:	; LINE 790
	.export	jump_lbl_211
jump_lbl_211:
@FastBasic_LINE_791:	; LINE 791
	.byte	TOK_VAR_LOAD
	.byte	118
	.byte	TOK_COMP_0
	.byte	TOK_L_NOT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_GT
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_228
@FastBasic_LINE_792:	; LINE 792
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_GT
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_GT
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_224
@FastBasic_LINE_793:	; LINE 793
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_225
@FastBasic_LINE_794:	; LINE 794
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	7, "TOTAL :"
	.byte	TOK_COPY_STR
@FastBasic_LINE_795:	; LINE 795
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_796:	; LINE 796
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "  WON:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_797:	; LINE 797
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_798:	; LINE 798
	.export	jump_lbl_225
jump_lbl_225:
@FastBasic_LINE_799:	; LINE 799
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_DPOKE
@FastBasic_LINE_800:	; LINE 800
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_801:	; LINE 801
	.export	jump_lbl_224
jump_lbl_224:
@FastBasic_LINE_802:	; LINE 802
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_228
@FastBasic_LINE_803:	; LINE 803
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_227
@FastBasic_LINE_804:	; LINE 804
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	7, "TOTAL :"
	.byte	TOK_COPY_STR
@FastBasic_LINE_805:	; LINE 805
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_806:	; LINE 806
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	8, "    PUSH"
	.byte	TOK_CAT_STR
@FastBasic_LINE_807:	; LINE 807
	.export	jump_lbl_227
jump_lbl_227:
@FastBasic_LINE_808:	; LINE 808
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_812:	; LINE 812
	.byte	TOK_JUMP
	.word	jump_lbl_228
	.export	jump_lbl_180
jump_lbl_180:
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_229
@FastBasic_LINE_813:	; LINE 813
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_230
@FastBasic_LINE_814:	; LINE 814
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_INT_STR
	.byte	TOK_COPY_STR
@FastBasic_LINE_815:	; LINE 815
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	14, " BUSTED  LOST:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_816:	; LINE 816
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_817:	; LINE 817
	.export	jump_lbl_230
jump_lbl_230:
@FastBasic_LINE_818:	; LINE 818
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_NEG
	.byte	TOK_DPOKE
@FastBasic_LINE_819:	; LINE 819
	.byte	TOK_JUMP
	.word	jump_lbl_228
	.export	jump_lbl_229
jump_lbl_229:
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_0
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_232
@FastBasic_LINE_820:	; LINE 820
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_233
@FastBasic_LINE_821:	; LINE 821
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	18, "SURRENDERED  LOST:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_822:	; LINE 822
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_DIV
	.byte	TOK_INT_FP
	.byte	TOK_FP_INT
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_823:	; LINE 823
	.export	jump_lbl_233
jump_lbl_233:
@FastBasic_LINE_824:	; LINE 824
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_DIV
	.byte	TOK_INT_FP
	.byte	TOK_FP_INT
	.byte	TOK_NEG
	.byte	TOK_DPOKE
@FastBasic_LINE_825:	; LINE 825
	.byte	TOK_JUMP
	.word	jump_lbl_228
	.export	jump_lbl_232
jump_lbl_232:
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_BYTE
	.byte	22
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_228
@FastBasic_LINE_826:	; LINE 826
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	12
	.byte	TOK_LT
	.byte	TOK_L_NOT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_236
@FastBasic_LINE_827:	; LINE 827
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_237
@FastBasic_LINE_828:	; LINE 828
	.byte	TOK_VAR_LOAD
	.byte	12
	.byte	TOK_PUSH_BYTE
	.byte	5
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_238
@FastBasic_LINE_829:	; LINE 829
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	23, "FIVE CARD CHARLIE  WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_830:	; LINE 830
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_831:	; LINE 831
	.export	jump_lbl_238
jump_lbl_238:
@FastBasic_LINE_832:	; LINE 832
	.byte	TOK_VAR_LOAD
	.byte	12
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_237
@FastBasic_LINE_833:	; LINE 833
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	22, "SIX CARD CHARLIE  WON:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_834:	; LINE 834
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_836:	; LINE 836
	.export	jump_lbl_237
jump_lbl_237:
@FastBasic_LINE_837:	; LINE 837
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_DPOKE
@FastBasic_LINE_838:	; LINE 838
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_839:	; LINE 839
	.byte	TOK_JUMP
	.word	jump_lbl_228
	.export	jump_lbl_236
jump_lbl_236:
@FastBasic_LINE_840:	; LINE 840
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_GT
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	21
	.byte	TOK_GT
	.byte	TOK_L_OR
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_VAR_LOAD
	.byte	12
	.byte	TOK_LT
	.byte	TOK_L_NOT
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_241
@FastBasic_LINE_841:	; LINE 841
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_DPOKE
@FastBasic_LINE_842:	; LINE 842
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_242
@FastBasic_LINE_843:	; LINE 843
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_844:	; LINE 844
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_845:	; LINE 845
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	7, "   WON:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_846:	; LINE 846
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_847:	; LINE 847
	.export	jump_lbl_242
jump_lbl_242:
@FastBasic_LINE_848:	; LINE 848
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	72
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_849:	; LINE 849
	.export	jump_lbl_241
jump_lbl_241:
@FastBasic_LINE_850:	; LINE 850
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_EQ
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	22
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_243
@FastBasic_LINE_851:	; LINE 851
	.byte	TOK_VAR_LOAD
	.byte	10
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	17
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_244
@FastBasic_LINE_852:	; LINE 852
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_245
@FastBasic_LINE_853:	; LINE 853
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	4, "TIE:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_854:	; LINE 854
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_855:	; LINE 855
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	7, "  LOST:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_856:	; LINE 856
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_857:	; LINE 857
	.export	jump_lbl_245
jump_lbl_245:
@FastBasic_LINE_858:	; LINE 858
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_NEG
	.byte	TOK_DPOKE
@FastBasic_LINE_859:	; LINE 859
	.byte	TOK_JUMP
	.word	jump_lbl_243
	.export	jump_lbl_244
jump_lbl_244:
@FastBasic_LINE_860:	; LINE 860
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_861:	; LINE 861
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_862:	; LINE 862
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	8, "    PUSH"
	.byte	TOK_CAT_STR
@FastBasic_LINE_863:	; LINE 863
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_DPOKE
@FastBasic_LINE_865:	; LINE 865
	.export	jump_lbl_243
jump_lbl_243:
@FastBasic_LINE_866:	; LINE 866
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_LT
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	50
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	22
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_228
@FastBasic_LINE_867:	; LINE 867
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_248
@FastBasic_LINE_868:	; LINE 868
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	6, "TOTAL:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_869:	; LINE 869
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	121
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_870:	; LINE 870
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	7, "  LOST:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_871:	; LINE 871
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_872:	; LINE 872
	.export	jump_lbl_248
jump_lbl_248:
@FastBasic_LINE_873:	; LINE 873
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_DPEEK
	.byte	TOK_NEG
	.byte	TOK_DPOKE
@FastBasic_LINE_876:	; LINE 876
	.export	jump_lbl_228
jump_lbl_228:
@FastBasic_LINE_877:	; LINE 877
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_249
@FastBasic_LINE_878:	; LINE 878
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, "T:"
	.byte	TOK_COPY_STR
@FastBasic_LINE_879:	; LINE 879
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_880:	; LINE 880
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_250
@FastBasic_LINE_881:	; LINE 881
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, " W:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_882:	; LINE 882
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_883:	; LINE 883
	.export	jump_lbl_250
jump_lbl_250:
@FastBasic_LINE_884:	; LINE 884
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_0
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_251
@FastBasic_LINE_885:	; LINE 885
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, " L:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_886:	; LINE 886
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_ABS
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_887:	; LINE 887
	.export	jump_lbl_251
jump_lbl_251:
@FastBasic_LINE_888:	; LINE 888
	.byte	TOK_0
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_252
@FastBasic_LINE_889:	; LINE 889
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, " P"
	.byte	TOK_CAT_STR
@FastBasic_LINE_890:	; LINE 890
	.export	jump_lbl_252
jump_lbl_252:
@FastBasic_LINE_891:	; LINE 891
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, "|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_892:	; LINE 892
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_893:	; LINE 893
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_253
@FastBasic_LINE_894:	; LINE 894
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, " W:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_895:	; LINE 895
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_896:	; LINE 896
	.export	jump_lbl_253
jump_lbl_253:
@FastBasic_LINE_897:	; LINE 897
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_0
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_254
@FastBasic_LINE_898:	; LINE 898
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, " L:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_899:	; LINE 899
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_ABS
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_900:	; LINE 900
	.export	jump_lbl_254
jump_lbl_254:
@FastBasic_LINE_901:	; LINE 901
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_249
@FastBasic_LINE_902:	; LINE 902
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	2, " P"
	.byte	TOK_CAT_STR
@FastBasic_LINE_904:	; LINE 904
	.export	jump_lbl_249
jump_lbl_249:
@FastBasic_LINE_905:	; LINE 905
	.byte	TOK_BYTE
	.byte	7
	.byte	TOK_ADD_VAR
	.byte	119
	.byte	TOK_VAR_STORE
	.byte	119
@FastBasic_LINE_906:	; LINE 906
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_LT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_256
@FastBasic_LINE_907:	; LINE 907
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_VAR_STORE
	.byte	120
@FastBasic_LINE_908:	; LINE 908
	.byte	TOK_JUMP
	.word	jump_lbl_257
	.export	jump_lbl_256
jump_lbl_256:
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_258
@FastBasic_LINE_909:	; LINE 909
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_259
@FastBasic_LINE_910:	; LINE 910
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	120
@FastBasic_LINE_911:	; LINE 911
	.byte	TOK_JUMP
	.word	jump_lbl_257
	.export	jump_lbl_259
jump_lbl_259:
@FastBasic_LINE_912:	; LINE 912
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_VAR_STORE
	.byte	120
@FastBasic_LINE_914:	; LINE 914
	.byte	TOK_JUMP
	.word	jump_lbl_257
	.export	jump_lbl_258
jump_lbl_258:
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	5
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_257
@FastBasic_LINE_915:	; LINE 915
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	120
	.byte	TOK_VAR_STORE
	.byte	120
@FastBasic_LINE_916:	; LINE 916
	.export	jump_lbl_257
jump_lbl_257:
@FastBasic_LINE_917:	; LINE 917
	.byte	TOK_VAR_LOAD
	.byte	120
	.byte	TOK_PUSH_1
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_172
@FastBasic_LINE_918:	; LINE 918
	.byte	TOK_VAR_LOAD
	.byte	111
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	51
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	111
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	51
	.byte	TOK_DPEEK
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_ADD_VAR
	.byte	62
	.byte	TOK_DPEEK
	.byte	TOK_ADD
	.byte	TOK_DPOKE
@FastBasic_LINE_919:	; LINE 919
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_171
	.export	jump_lbl_170
jump_lbl_170:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_921:	; LINE 921
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	42
	.byte	TOK_VAR_STORE
	.byte	42
@FastBasic_LINE_922:	; LINE 922
	.byte	TOK_RET
@FastBasic_LINE_924:	; LINE 924
	.export	fb_lbl_PERFORM_SPLITDEAL
fb_lbl_PERFORM_SPLITDEAL:
@FastBasic_LINE_925:	; LINE 925
	.byte	TOK_BYTE
	.byte	14
	.byte	TOK_VAR_STORE
	.byte	113
@FastBasic_LINE_926:	; LINE 926
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_265
@FastBasic_LINE_927:	; LINE 927
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_266
	.export	jump_lbl_267
jump_lbl_267:
@FastBasic_LINE_928:	; LINE 928
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_929:	; LINE 929
	.byte	TOK_VAR_LOAD
	.byte	57
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_930:	; LINE 930
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_267
	.export	jump_lbl_266
jump_lbl_266:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_931:	; LINE 931
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_268
@FastBasic_LINE_932:	; LINE 932
	.byte	TOK_VAR_LOAD
	.byte	32
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	78
	.byte	TOK_ADD
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_INT_FP
	.byte	TOK_FLOAT
	.byte	$3F, $11, $00, $00, $00, $00
	.byte	TOK_FP_MUL
	.byte	TOK_FP_INT
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_933:	; LINE 933
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_934:	; LINE 934
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_935:	; LINE 935
	.byte	TOK_VAR_LOAD
	.byte	57
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	24
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_936:	; LINE 936
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_1
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_269
	.export	jump_lbl_270
jump_lbl_270:
@FastBasic_LINE_937:	; LINE 937
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_938:	; LINE 938
	.byte	TOK_VAR_LOAD
	.byte	57
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_939:	; LINE 939
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_270
	.export	jump_lbl_269
jump_lbl_269:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_940:	; LINE 940
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_POKE
@FastBasic_LINE_941:	; LINE 941
	.byte	TOK_VAR_LOAD
	.byte	59
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	90
@FastBasic_LINE_943:	; LINE 943
	.byte	TOK_VAR_LOAD
	.byte	32
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_ADD
	.byte	TOK_ADD_VAR
	.byte	113
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_944:	; LINE 944
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_945:	; LINE 945
	.byte	TOK_CALL
	.word	fb_lbl_ANIMATEHIT
@FastBasic_LINE_946:	; LINE 946
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	41
@FastBasic_LINE_947:	; LINE 947
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_948:	; LINE 948
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	25
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_949:	; LINE 949
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_VAR_STORE
	.byte	39
@FastBasic_LINE_950:	; LINE 950
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_POKE
@FastBasic_LINE_951:	; LINE 951
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_POKE
@FastBasic_LINE_952:	; LINE 952
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_953:	; LINE 953
	.byte	TOK_VAR_LOAD
	.byte	32
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	78
	.byte	TOK_ADD
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_INT_FP
	.byte	TOK_FLOAT
	.byte	$3F, $11, $00, $00, $00, $00
	.byte	TOK_FP_MUL
	.byte	TOK_FP_INT
	.byte	TOK_ADD
	.byte	TOK_ADD_VAR
	.byte	113
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_954:	; LINE 954
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_955:	; LINE 955
	.byte	TOK_CALL
	.word	fb_lbl_ANIMATEHIT
@FastBasic_LINE_956:	; LINE 956
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	23
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_957:	; LINE 957
	.byte	TOK_VAR_LOAD
	.byte	58
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	25
	.byte	TOK_PUSH_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_PEEK
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_958:	; LINE 958
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	39
	.byte	TOK_VAR_STORE
	.byte	39
@FastBasic_LINE_959:	; LINE 959
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	3
	.byte	TOK_POKE
@FastBasic_LINE_960:	; LINE 960
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	9
	.byte	TOK_POKE
@FastBasic_LINE_961:	; LINE 961
	.byte	TOK_CALL
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_962:	; LINE 962
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_0
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_COPY_STR
@FastBasic_LINE_963:	; LINE 963
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_CSTRING
	.byte	3, "|T:"
	.byte	TOK_CAT_STR
@FastBasic_LINE_964:	; LINE 964
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	50
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_INT_STR
	.byte	TOK_CAT_STR
@FastBasic_LINE_965:	; LINE 965
	.export	jump_lbl_268
jump_lbl_268:
@FastBasic_LINE_966:	; LINE 966
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_POKE
@FastBasic_LINE_967:	; LINE 967
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_POKE
@FastBasic_LINE_968:	; LINE 968
	.byte	TOK_JUMP
	.word	fb_lbl_SHOWSTATS
@FastBasic_LINE_969:	; LINE 969
	.export	jump_lbl_265
jump_lbl_265:
@FastBasic_LINE_970:	; LINE 970
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CRET
@FastBasic_LINE_971:	; LINE 971
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_BYTE
	.byte	14
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_273
	.export	jump_lbl_274
jump_lbl_274:
@FastBasic_LINE_972:	; LINE 972
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_973:	; LINE 973
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_274
	.export	jump_lbl_273
jump_lbl_273:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_974:	; LINE 974
	.byte	TOK_VAR_LOAD
	.byte	32
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_PUSH_BYTE
	.byte	148
	.byte	TOK_ADD
	.byte	TOK_PUSH_VAR_LOAD
	.byte	31
	.byte	TOK_INT_FP
	.byte	TOK_FLOAT
	.byte	$3F, $17, $00, $00, $00, $00
	.byte	TOK_FP_MUL
	.byte	TOK_FP_INT
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	83
@FastBasic_LINE_975:	; LINE 975
	.byte	TOK_VAR_LOAD
	.byte	33
	.byte	TOK_PUSH_VAR_LOAD
	.byte	34
	.byte	TOK_PUSH_0
	.byte	TOK_MUL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	84
@FastBasic_LINE_976:	; LINE 976
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_275
@FastBasic_LINE_977:	; LINE 977
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_1
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_978:	; LINE 978
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_1
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_276
	.export	jump_lbl_277
jump_lbl_277:
@FastBasic_LINE_979:	; LINE 979
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_980:	; LINE 980
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_277
	.export	jump_lbl_276
jump_lbl_276:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_981:	; LINE 981
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_POKE
@FastBasic_LINE_982:	; LINE 982
	.export	jump_lbl_275
jump_lbl_275:
@FastBasic_LINE_983:	; LINE 983
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CRET
@FastBasic_LINE_984:	; LINE 984
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	14
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_985:	; LINE 985
	.byte	TOK_VAR_SADDR
	.byte	90
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_DPOKE
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_279
	.export	jump_lbl_280
jump_lbl_280:
@FastBasic_LINE_986:	; LINE 986
	.byte	TOK_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_ADD_VAR
	.byte	90
	.byte	TOK_ADD
	.byte	TOK_SADDR
	.byte	TOK_0
	.byte	TOK_POKE
@FastBasic_LINE_987:	; LINE 987
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_280
	.export	jump_lbl_279
jump_lbl_279:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_988:	; LINE 988
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	8
	.byte	TOK_POKE
@FastBasic_LINE_992:	; LINE 992
	.byte	TOK_RET
@FastBasic_LINE_994:	; LINE 994
	.export	fb_lbl_PERFORM_NEXTHAND
fb_lbl_PERFORM_NEXTHAND:
@FastBasic_LINE_995:	; LINE 995
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_283
@FastBasic_LINE_996:	; LINE 996
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	3
	.byte	TOK_POKE
@FastBasic_LINE_997:	; LINE 997
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	9
	.byte	TOK_POKE
@FastBasic_LINE_998:	; LINE 998
	.export	jump_lbl_283
jump_lbl_283:
@FastBasic_LINE_999:	; LINE 999
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	56
	.byte	TOK_PUSH_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_BYTE
	.byte	20
	.byte	TOK_MUL
	.byte	TOK_PUSH_BYTE
	.byte	14
	.byte	TOK_ADD
	.byte	TOK_ADD
	.byte	TOK_PEEK
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_1000:	; LINE 1000
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_POKE
@FastBasic_LINE_1001:	; LINE 1001
	.byte	TOK_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_SADDR
	.byte	TOK_BYTE
	.byte	15
	.byte	TOK_POKE
@FastBasic_LINE_1003:	; LINE 1003
	.byte	TOK_JUMP
	.word	fb_lbl_CALCTOTAL
@FastBasic_LINE_1006:	; LINE 1006
	.export	fb_lbl_GETBETS
fb_lbl_GETBETS:
@FastBasic_LINE_1007:	; LINE 1007
	.byte	TOK_VAR_STORE_0
	.byte	123
@FastBasic_LINE_1008:	; LINE 1008
	.byte	TOK_VAR_STORE_0
	.byte	124
@FastBasic_LINE_1009:	; LINE 1009
	.byte	TOK_VAR_STORE_0
	.byte	90
@FastBasic_LINE_1010:	; LINE 1010
	.byte	TOK_VAR_SADDR
	.byte	125
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_1011:	; LINE 1011
	.byte	TOK_VAR_STORE_0
	.byte	126
@FastBasic_LINE_1012:	; LINE 1012
	.byte	TOK_VAR_STORE_0
	.byte	127
@FastBasic_LINE_1013:	; LINE 1013
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_1014:	; LINE 1014
	.byte	TOK_VAR_STORE_0
	.byte	128
@FastBasic_LINE_1015:	; LINE 1015
	.byte	TOK_BYTE
	.byte	CLS
	.byte	TOK_PUT
@FastBasic_LINE_1016:	; LINE 1016
	.byte	TOK_VAR_SADDR
	.byte	125
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_1017:	; LINE 1017
	.byte	TOK_VAR_SADDR
	.byte	123
	.byte	TOK_PUSH_1
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_287
	.export	jump_lbl_288
jump_lbl_288:
@FastBasic_LINE_1018:	; LINE 1018
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_1019:	; LINE 1019
	.export	jump_lbl_289
jump_lbl_289:
@FastBasic_LINE_1020:	; LINE 1020
	.byte	TOK_VAR_LOAD
	.byte	19
	.byte	TOK_VAR_STORE
	.byte	126
@FastBasic_LINE_1021:	; LINE 1021
	.byte	TOK_VAR_SADDR
	.byte	124
	.byte	TOK_PUSH_1
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_290
	.export	jump_lbl_291
jump_lbl_291:
@FastBasic_LINE_1024:	; LINE 1024
	.byte	TOK_VAR_LOAD
	.byte	123
	.byte	TOK_PUSH_VAR_LOAD
	.byte	124
	.byte	TOK_NEQ
	.byte	TOK_CJUMP
	.word	jump_lbl_292
@FastBasic_LINE_1026:	; LINE 1026
	.export	jump_lbl_292
jump_lbl_292:
@FastBasic_LINE_1027:	; LINE 1027
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_291
	.export	jump_lbl_290
jump_lbl_290:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1030:	; LINE 1030
	.byte	TOK_VAR_SADDR
	.byte	125
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_1031:	; LINE 1031
	.byte	TOK_BYTE
	.byte	63
	.byte	TOK_PUT
	.byte	TOK_VAR_SADDR
	.byte	125
	.byte	TOK_INPUT_STR
	.byte	TOK_COPY_STR
@FastBasic_LINE_1032:	; LINE 1032
	.byte	TOK_VAR_LOAD
	.byte	125
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	46
	.byte	TOK_COMP_0
	.byte	TOK_L_NOT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_293
@FastBasic_LINE_1033:	; LINE 1033
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	24, "You Must Enter a Number."
	.byte	TOK_COPY_STR
@FastBasic_LINE_1034:	; LINE 1034
	.byte	TOK_JUMP
	.word	jump_lbl_294
	.export	jump_lbl_293
jump_lbl_293:
	.byte	TOK_VAR_LOAD
	.byte	125
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	46
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_295
@FastBasic_LINE_1035:	; LINE 1035
	.byte	TOK_BYTE
	.byte	2
	.byte	TOK_VAR_STORE
	.byte	46
@FastBasic_LINE_1036:	; LINE 1036
	.byte	TOK_JUMP
	.word	jump_lbl_294
	.export	jump_lbl_295
jump_lbl_295:
@FastBasic_LINE_1037:	; LINE 1037
	.byte	TOK_VAR_LOAD
	.byte	125
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	1, "Q"
	.byte	TOK_CMP_STR
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_297
@FastBasic_LINE_1038:	; LINE 1038
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	29, "Quit? Press Enter to Confirm."
	.byte	TOK_COPY_STR
@FastBasic_LINE_1039:	; LINE 1039
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	46
@FastBasic_LINE_1040:	; LINE 1040
	.byte	TOK_JUMP
	.word	jump_lbl_294
	.export	jump_lbl_297
jump_lbl_297:
@FastBasic_LINE_1041:	; LINE 1041
	.byte	TOK_VAR_STORE_0
	.byte	46
@FastBasic_LINE_1042:	; LINE 1042
	.byte	TOK_VAR_LOAD
	.byte	125
	.byte	TOK_VAL
	.byte	TOK_VAR_STORE
	.byte	127
@FastBasic_LINE_1043:	; LINE 1043
	.byte	TOK_VAR_LOAD
	.byte	127
	.byte	TOK_PUSH_BYTE
	.byte	9
	.byte	TOK_GT
	.byte	TOK_PUSH_VAR_LOAD
	.byte	127
	.byte	TOK_PUSH_NUM
	.word	1001
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_299
@FastBasic_LINE_1044:	; LINE 1044
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	126
@FastBasic_LINE_1045:	; LINE 1045
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_COPY_STR
@FastBasic_LINE_1046:	; LINE 1046
	.byte	TOK_VAR_LOAD
	.byte	123
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	53
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	127
	.byte	TOK_DPOKE
@FastBasic_LINE_1047:	; LINE 1047
	.byte	TOK_JUMP
	.word	jump_lbl_300
	.export	jump_lbl_299
jump_lbl_299:
	.byte	TOK_VAR_LOAD
	.byte	127
	.byte	TOK_PUSH_NUM
	.word	1000
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_301
@FastBasic_LINE_1048:	; LINE 1048
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	24, "The Maximum Bet is 1000."
	.byte	TOK_COPY_STR
@FastBasic_LINE_1049:	; LINE 1049
	.byte	TOK_JUMP
	.word	jump_lbl_300
	.export	jump_lbl_301
jump_lbl_301:
	.byte	TOK_VAR_LOAD
	.byte	127
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_300
@FastBasic_LINE_1050:	; LINE 1050
	.byte	TOK_VAR_SADDR
	.byte	64
	.byte	TOK_CSTRING
	.byte	22, "The Minimal Bet is 10."
	.byte	TOK_COPY_STR
@FastBasic_LINE_1051:	; LINE 1051
	.export	jump_lbl_300
jump_lbl_300:
@FastBasic_LINE_1052:	; LINE 1052
	.byte	TOK_VAR_LOAD
	.byte	64
	.byte	TOK_PUSH
	.byte	TOK_CSTRING
	.byte	0
	.byte	TOK_CMP_STR
	.byte	TOK_NEQ
	.byte	TOK_CJUMP
	.word	jump_lbl_294
@FastBasic_LINE_1053:	; LINE 1053
	.byte	TOK_VAR_STORE_0
	.byte	126
@FastBasic_LINE_1056:	; LINE 1056
	.export	jump_lbl_294
jump_lbl_294:
@FastBasic_LINE_1059:	; LINE 1059
	.byte	TOK_VAR_LOAD
	.byte	126
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	46
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_289
@FastBasic_LINE_1060:	; LINE 1060
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_288
	.export	jump_lbl_287
jump_lbl_287:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1061:	; LINE 1061
	.byte	TOK_RET
@FastBasic_LINE_1063:	; LINE 1063
	.export	fb_lbl_SHOWSTATS
fb_lbl_SHOWSTATS:
@FastBasic_LINE_1064:	; LINE 1064
	.byte	TOK_VAR_STORE_0
	.byte	124
@FastBasic_LINE_1065:	; LINE 1065
	.byte	TOK_VAR_STORE_0
	.byte	129
@FastBasic_LINE_1067:	; LINE 1067
	.byte	TOK_VAR_SADDR
	.byte	124
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_307
	.export	jump_lbl_308
jump_lbl_308:
@FastBasic_LINE_1068:	; LINE 1068
	.byte	TOK_VAR_LOAD
	.byte	124
	.byte	TOK_COMP_0
@FastBasic_LINE_1071:	; LINE 1071
	.byte	TOK_CJUMP
	.word	jump_lbl_310
@FastBasic_LINE_1075:	; LINE 1075
	.byte	TOK_VAR_LOAD
	.byte	124
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	61
	.byte	TOK_DPEEK
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	22
	.byte	TOK_LT
@FastBasic_LINE_1077:	; LINE 1077
	.byte	TOK_CNJUMP
	.word	jump_lbl_310
@FastBasic_LINE_1080:	; LINE 1080
	.export	jump_lbl_310
jump_lbl_310:
@FastBasic_LINE_1081:	; LINE 1081
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_308
	.export	jump_lbl_307
jump_lbl_307:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1082:	; LINE 1082
	.byte	TOK_RET
@FastBasic_LINE_1084:	; LINE 1084
	.export	fb_lbl_SHOWCOMMANDS
fb_lbl_SHOWCOMMANDS:
@FastBasic_LINE_1085:	; LINE 1085
	.byte	TOK_VAR_STORE_0
	.byte	130
@FastBasic_LINE_1086:	; LINE 1086
	.byte	TOK_VAR_STORE_0
	.byte	124
@FastBasic_LINE_1087:	; LINE 1087
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_EQ
	.byte	TOK_L_OR
	.byte	TOK_CJUMP
	.word	jump_lbl_315
@FastBasic_LINE_1088:	; LINE 1088
	.byte	TOK_VAR_STORE_0
	.byte	130
@FastBasic_LINE_1089:	; LINE 1089
	.byte	TOK_JUMP
	.word	jump_lbl_316
	.export	jump_lbl_315
jump_lbl_315:
@FastBasic_LINE_1090:	; LINE 1090
	.byte	TOK_VAR_STORE_0
	.byte	130
@FastBasic_LINE_1091:	; LINE 1091
	.export	jump_lbl_316
jump_lbl_316:
@FastBasic_LINE_1092:	; LINE 1092
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_PUSH_1
	.byte	TOK_SUB
	.byte	TOK_VAR_STORE
	.byte	124
@FastBasic_LINE_1097:	; LINE 1097
	.byte	TOK_VAR_LOAD
	.byte	43
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	60
	.byte	TOK_DPEEK
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_317
@FastBasic_LINE_1100:	; LINE 1100
	.export	jump_lbl_317
jump_lbl_317:
@FastBasic_LINE_1101:	; LINE 1101
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_318
@FastBasic_LINE_1104:	; LINE 1104
	.export	jump_lbl_318
jump_lbl_318:
@FastBasic_LINE_1105:	; LINE 1105
	.byte	TOK_VAR_LOAD
	.byte	67
	.byte	TOK_PUSH_VAR_LOAD
	.byte	18
	.byte	TOK_EQ
	.byte	TOK_PUSH_VAR_LOAD
	.byte	49
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_319
@FastBasic_LINE_1108:	; LINE 1108
	.export	jump_lbl_319
jump_lbl_319:
@FastBasic_LINE_1109:	; LINE 1109
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_320
@FastBasic_LINE_1111:	; LINE 1111
	.export	jump_lbl_320
jump_lbl_320:
@FastBasic_LINE_1112:	; LINE 1112
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_321
@FastBasic_LINE_1114:	; LINE 1114
	.export	jump_lbl_321
jump_lbl_321:
@FastBasic_LINE_1115:	; LINE 1115
	.byte	TOK_VAR_LOAD
	.byte	54
	.byte	TOK_ADD_VAR
	.byte	43
	.byte	TOK_PEEK
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_EQ
	.byte	TOK_CRET
@FastBasic_LINE_1118:	; LINE 1118
	.byte	TOK_RET
@FastBasic_LINE_1120:	; LINE 1120
	.export	fb_lbl_SHUFFLEDECKS
fb_lbl_SHUFFLEDECKS:
@FastBasic_LINE_1121:	; LINE 1121
	.byte	TOK_VAR_STORE_0
	.byte	78
@FastBasic_LINE_1122:	; LINE 1122
	.byte	TOK_VAR_STORE_0
	.byte	85
@FastBasic_LINE_1123:	; LINE 1123
	.byte	TOK_VAR_STORE_0
	.byte	86
@FastBasic_LINE_1124:	; LINE 1124
	.byte	TOK_VAR_STORE_0
	.byte	87
@FastBasic_LINE_1125:	; LINE 1125
	.byte	TOK_VAR_STORE_0
	.byte	88
@FastBasic_LINE_1126:	; LINE 1126
	.byte	TOK_VAR_STORE_0
	.byte	90
@FastBasic_LINE_1127:	; LINE 1127
	.byte	TOK_VAR_STORE_0
	.byte	89
@FastBasic_LINE_1128:	; LINE 1128
	.byte	TOK_VAR_STORE_0
	.byte	91
@FastBasic_LINE_1130:	; LINE 1130
	.byte	TOK_VAR_STORE_0
	.byte	84
@FastBasic_LINE_1131:	; LINE 1131
	.byte	TOK_VAR_STORE_0
	.byte	131
@FastBasic_LINE_1132:	; LINE 1132
	.byte	TOK_VAR_STORE_0
	.byte	132
@FastBasic_LINE_1133:	; LINE 1133
	.byte	TOK_VAR_STORE_0
	.byte	133
@FastBasic_LINE_1141:	; LINE 1141
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_325
	.export	jump_lbl_326
jump_lbl_326:
@FastBasic_LINE_1142:	; LINE 1142
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_SADDR
	.byte	TOK_1
	.byte	TOK_NEG
	.byte	TOK_POKE
@FastBasic_LINE_1144:	; LINE 1144
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_326
	.export	jump_lbl_325
jump_lbl_325:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1145:	; LINE 1145
	.export	jump_lbl_327
jump_lbl_327:
@FastBasic_LINE_1146:	; LINE 1146
	.byte	TOK_BYTE
	.byte	5
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_RAND
	.byte	TOK_USHL
	.byte	TOK_ADD
	.byte	TOK_VAR_STORE
	.byte	86
@FastBasic_LINE_1147:	; LINE 1147
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_PUSH_VAR_LOAD
	.byte	86
	.byte	TOK_MOD
	.byte	TOK_PUSH_0
	.byte	TOK_GT
	.byte	TOK_PUSH_1
	.byte	TOK_ADD_VAR
	.byte	73
	.byte	TOK_PUSH_VAR_LOAD
	.byte	86
	.byte	TOK_MOD
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_NEQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_1
	.byte	TOK_ADD_VAR
	.byte	73
	.byte	TOK_PUSH_VAR_LOAD
	.byte	86
	.byte	TOK_MOD
	.byte	TOK_PUSH_BYTE
	.byte	3
	.byte	TOK_NEQ
	.byte	TOK_L_AND
	.byte	TOK_PUSH_1
	.byte	TOK_ADD_VAR
	.byte	73
	.byte	TOK_PUSH_VAR_LOAD
	.byte	86
	.byte	TOK_MOD
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_NEQ
	.byte	TOK_L_AND
	.byte	TOK_CJUMP
	.word	jump_lbl_327
@FastBasic_LINE_1148:	; LINE 1148
	.byte	TOK_VAR_STORE_0
	.byte	39
@FastBasic_LINE_1149:	; LINE 1149
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_328
	.export	jump_lbl_329
jump_lbl_329:
@FastBasic_LINE_1150:	; LINE 1150
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_PEEK
	.byte	TOK_PUSH_1
	.byte	TOK_NEG
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_330
@FastBasic_LINE_1151:	; LINE 1151
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	39
	.byte	TOK_POKE
@FastBasic_LINE_1152:	; LINE 1152
	.byte	TOK_VAR_LOAD
	.byte	39
	.byte	TOK_ADD_VAR
	.byte	86
	.byte	TOK_PUSH_1
	.byte	TOK_ADD_VAR
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_MOD
	.byte	TOK_VAR_STORE
	.byte	39
@FastBasic_LINE_1153:	; LINE 1153
	.export	jump_lbl_330
jump_lbl_330:
@FastBasic_LINE_1154:	; LINE 1154
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_329
	.export	jump_lbl_328
jump_lbl_328:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1155:	; LINE 1155
	.byte	TOK_VAR_STORE_0
	.byte	39
@FastBasic_LINE_1156:	; LINE 1156
	.byte	TOK_VAR_STORE_0
	.byte	89
@FastBasic_LINE_1157:	; LINE 1157
	.byte	TOK_VAR_SADDR
	.byte	89
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	5
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_331
	.export	jump_lbl_332
jump_lbl_332:
@FastBasic_LINE_1158:	; LINE 1158
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_333
	.export	jump_lbl_334
jump_lbl_334:
@FastBasic_LINE_1159:	; LINE 1159
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_RAND
	.byte	TOK_VAR_STORE
	.byte	86
@FastBasic_LINE_1160:	; LINE 1160
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_PUSH_VAR_LOAD
	.byte	86
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_335
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_RAND
	.byte	TOK_VAR_STORE
	.byte	86
	.export	jump_lbl_335
jump_lbl_335:
@FastBasic_LINE_1161:	; LINE 1161
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_PUSH_VAR_LOAD
	.byte	86
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_336
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_RAND
	.byte	TOK_VAR_STORE
	.byte	86
	.export	jump_lbl_336
jump_lbl_336:
@FastBasic_LINE_1162:	; LINE 1162
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	86
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	87
@FastBasic_LINE_1163:	; LINE 1163
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	86
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_PEEK
	.byte	TOK_POKE
@FastBasic_LINE_1164:	; LINE 1164
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	87
	.byte	TOK_POKE
@FastBasic_LINE_1165:	; LINE 1165
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_334
	.export	jump_lbl_333
jump_lbl_333:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1166:	; LINE 1166
	.byte	TOK_VAR_LOAD
	.byte	89
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_337
@FastBasic_LINE_1167:	; LINE 1167
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_338
	.export	jump_lbl_339
jump_lbl_339:
@FastBasic_LINE_1168:	; LINE 1168
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	0
	.byte	TOK_SADDR
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_PEEK
	.byte	TOK_DPOKE
@FastBasic_LINE_1169:	; LINE 1169
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_339
	.export	jump_lbl_338
jump_lbl_338:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1170:	; LINE 1170
	.export	jump_lbl_337
jump_lbl_337:
@FastBasic_LINE_1171:	; LINE 1171
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_332
	.export	jump_lbl_331
jump_lbl_331:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1172:	; LINE 1172
	.byte	TOK_VAR_LOAD
	.byte	5
	.byte	TOK_PUSH_BYTE
	.byte	5
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_340
@FastBasic_LINE_1173:	; LINE 1173
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_VAR_STORE
	.byte	88
@FastBasic_LINE_1174:	; LINE 1174
	.byte	TOK_JUMP
	.word	jump_lbl_341
	.export	jump_lbl_340
jump_lbl_340:
	.byte	TOK_VAR_LOAD
	.byte	5
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_342
@FastBasic_LINE_1175:	; LINE 1175
	.byte	TOK_BYTE
	.byte	7
	.byte	TOK_VAR_STORE
	.byte	88
@FastBasic_LINE_1176:	; LINE 1176
	.byte	TOK_JUMP
	.word	jump_lbl_341
	.export	jump_lbl_342
jump_lbl_342:
@FastBasic_LINE_1177:	; LINE 1177
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_VAR_STORE
	.byte	88
@FastBasic_LINE_1178:	; LINE 1178
	.export	jump_lbl_341
jump_lbl_341:
@FastBasic_LINE_1182:	; LINE 1182
	.byte	TOK_VAR_STORE_0
	.byte	85
@FastBasic_LINE_1183:	; LINE 1183
	.export	jump_lbl_344
jump_lbl_344:
@FastBasic_LINE_1184:	; LINE 1184
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	0
	.byte	TOK_DPEEK
	.byte	TOK_VAR_STORE
	.byte	87
@FastBasic_LINE_1185:	; LINE 1185
	.byte	TOK_VAR_LOAD
	.byte	26
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_PEEK
	.byte	TOK_VAR_STORE
	.byte	86
@FastBasic_LINE_1186:	; LINE 1186
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_PUSH_VAR_LOAD
	.byte	88
	.byte	TOK_MOD
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_345
@FastBasic_LINE_1188:	; LINE 1188
	.export	jump_lbl_345
jump_lbl_345:
@FastBasic_LINE_1189:	; LINE 1189
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_PUSH_VAR_LOAD
	.byte	88
	.byte	TOK_MOD
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_346
@FastBasic_LINE_1191:	; LINE 1191
	.export	jump_lbl_346
jump_lbl_346:
@FastBasic_LINE_1194:	; LINE 1194
	.byte	TOK_1
	.byte	TOK_ADD_VAR
	.byte	85
	.byte	TOK_VAR_STORE
	.byte	85
@FastBasic_LINE_1195:	; LINE 1195
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_PUSH_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_GT
	.byte	TOK_CJUMP
	.word	jump_lbl_344
@FastBasic_LINE_1196:	; LINE 1196
	.byte	TOK_VAR_STORE_0
	.byte	39
@FastBasic_LINE_1197:	; LINE 1197
	.byte	TOK_VAR_SADDR
	.byte	85
	.byte	TOK_PUSH_0
	.byte	TOK_DPOKE
	.byte	TOK_VAR_LOAD
	.byte	73
	.byte	TOK_ADD_VAR
	.byte	71
	.byte	TOK_PUSH_1
	.byte	TOK_FOR
	.byte	TOK_CNJUMP
	.word	jump_lbl_347
	.export	jump_lbl_348
jump_lbl_348:
@FastBasic_LINE_1199:	; LINE 1199
	.byte	TOK_FOR_NEXT
	.byte	TOK_CJUMP
	.word	jump_lbl_348
	.export	jump_lbl_347
jump_lbl_347:
	.byte	TOK_FOR_EXIT
@FastBasic_LINE_1200:	; LINE 1200
	.byte	TOK_RET
@FastBasic_LINE_1202:	; LINE 1202
	.export	fb_lbl_INIT_DEFAULTS
fb_lbl_INIT_DEFAULTS:
@FastBasic_LINE_1203:	; LINE 1203
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	20
@FastBasic_LINE_1204:	; LINE 1204
	.byte	TOK_BYTE
	.byte	52
	.byte	TOK_VAR_STORE
	.byte	4
@FastBasic_LINE_1205:	; LINE 1205
	.byte	TOK_BYTE
	.byte	4
	.byte	TOK_VAR_STORE
	.byte	5
@FastBasic_LINE_1206:	; LINE 1206
	.byte	TOK_VAR_STORE_0
	.byte	1
@FastBasic_LINE_1207:	; LINE 1207
	.byte	TOK_VAR_STORE_0
	.byte	13
@FastBasic_LINE_1208:	; LINE 1208
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	8
@FastBasic_LINE_1209:	; LINE 1209
	.byte	TOK_VAR_STORE_0
	.byte	9
@FastBasic_LINE_1210:	; LINE 1210
	.byte	TOK_VAR_STORE_0
	.byte	10
@FastBasic_LINE_1211:	; LINE 1211
	.byte	TOK_VAR_STORE_0
	.byte	11
@FastBasic_LINE_1212:	; LINE 1212
	.byte	TOK_BYTE
	.byte	6
	.byte	TOK_VAR_STORE
	.byte	12
@FastBasic_LINE_1213:	; LINE 1213
	.byte	TOK_VAR_STORE_0
	.byte	14
@FastBasic_LINE_1214:	; LINE 1214
	.byte	TOK_VAR_STORE_0
	.byte	15
@FastBasic_LINE_1215:	; LINE 1215
	.byte	TOK_VAR_STORE_0
	.byte	2
@FastBasic_LINE_1216:	; LINE 1216
	.byte	TOK_VAR_STORE_0
	.byte	67
@FastBasic_LINE_1217:	; LINE 1217
	.byte	TOK_RET
@FastBasic_LINE_1220:	; LINE 1220
	.export	fb_lbl_TITLE_SCREEN
fb_lbl_TITLE_SCREEN:
@FastBasic_LINE_1221:	; LINE 1221
	.byte	TOK_VAR_STORE_0
	.byte	77
@FastBasic_LINE_1222:	; LINE 1222
	.byte	TOK_VAR_STORE_0
	.byte	78
@FastBasic_LINE_1223:	; LINE 1223
	.byte	TOK_VAR_STORE_0
	.byte	79
@FastBasic_LINE_1224:	; LINE 1224
	.byte	TOK_VAR_STORE_0
	.byte	85
@FastBasic_LINE_1225:	; LINE 1225
	.byte	TOK_VAR_STORE_0
	.byte	86
@FastBasic_LINE_1226:	; LINE 1226
	.byte	TOK_VAR_STORE_0
	.byte	87
@FastBasic_LINE_1227:	; LINE 1227
	.byte	TOK_VAR_STORE_0
	.byte	88
@FastBasic_LINE_1228:	; LINE 1228
	.byte	TOK_VAR_STORE_0
	.byte	90
@FastBasic_LINE_1229:	; LINE 1229
	.byte	TOK_VAR_STORE_0
	.byte	134
@FastBasic_LINE_1230:	; LINE 1230
	.byte	TOK_VAR_STORE_0
	.byte	135
@FastBasic_LINE_1231:	; LINE 1231
	.byte	TOK_VAR_STORE_0
	.byte	136
@FastBasic_LINE_1232:	; LINE 1232
	.byte	TOK_VAR_STORE_0
	.byte	137
@FastBasic_LINE_1233:	; LINE 1233
	.byte	TOK_VAR_STORE_0
	.byte	138
@FastBasic_LINE_1239:	; LINE 1239
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	77
@FastBasic_LINE_1240:	; LINE 1240
	.byte	TOK_1
	.byte	TOK_VAR_STORE
	.byte	90
@FastBasic_LINE_1241:	; LINE 1241
	.byte	TOK_VAR_STORE_0
	.byte	78
@FastBasic_LINE_1245:	; LINE 1245
	.byte	TOK_VAR_STORE_0
	.byte	139
@FastBasic_LINE_1246:	; LINE 1246
	.byte	TOK_0
	.byte	TOK_BYTE_POKE
	.byte	82
@FastBasic_LINE_1247:	; LINE 1247
	.byte	TOK_BYTE
	.byte	210
	.byte	TOK_NUM_POKE
	.word	710
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_NUM_POKE
	.word	709
	.byte	TOK_BYTE
	.byte	210
	.byte	TOK_NUM_POKE
	.word	712
@FastBasic_LINE_1248:	; LINE 1248
	.byte	TOK_NUM
	.word	756
	.byte	TOK_SADDR
	.byte	TOK_NUM
	.word	CHARSET_GAME_HI
	.byte	TOK_POKE
@FastBasic_LINE_1249:	; LINE 1249
	.byte	TOK_NUM
	.word	560
	.byte	TOK_SADDR
	.byte	TOK_NUM
	.word	DISPLAY_LIST_TITLE
	.byte	TOK_DPOKE
@FastBasic_LINE_1250:	; LINE 1250
	.byte	TOK_NUM
	.word	548
	.byte	TOK_SADDR
	.byte	TOK_NUM
	.word	TITLE_VBI
	.byte	TOK_DPOKE
@FastBasic_LINE_1251:	; LINE 1251
	.byte	TOK_NUM
	.word	512
	.byte	TOK_SADDR
	.byte	TOK_NUM
	.word	TITLE00_DLI
	.byte	TOK_DPOKE
@FastBasic_LINE_1252:	; LINE 1252
	.byte	TOK_BYTE
	.byte	88
	.byte	TOK_SADDR
	.byte	TOK_NUM
	.word	SCREEN_ADDR
	.byte	TOK_DPOKE
@FastBasic_LINE_1253:	; LINE 1253
	.byte	TOK_BYTE
	.byte	192
	.byte	TOK_NUM_POKE
	.word	54286
@FastBasic_LINE_1254:	; LINE 1254
	.byte	TOK_0
	.byte	TOK_NUM_POKE
	.word	DLIV0
@FastBasic_LINE_1255:	; LINE 1255
	.byte	TOK_0
	.byte	TOK_NUM_POKE
	.word	DLIV1
@FastBasic_LINE_1256:	; LINE 1256
	.byte	TOK_1
	.byte	TOK_NUM_POKE
	.word	752
@FastBasic_LINE_1257:	; LINE 1257
	.byte	TOK_0
	.byte	TOK_PUSH_0
	.byte	TOK_POSITION
@FastBasic_LINE_1258:	; LINE 1258
	.byte	TOK_BYTE
	.byte	CLS
	.byte	TOK_PUT
@FastBasic_LINE_1259:	; LINE 1259
	.byte	TOK_CSTRING
	.byte	40, "   ATARI CASINO         BLACK JACK     ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1260:	; LINE 1260
	.byte	TOK_0
	.byte	TOK_PUSH_1
	.byte	TOK_POSITION
@FastBasic_LINE_1261:	; LINE 1261
	.byte	TOK_CSTRING
	.byte	40, "               A Game By Peter J. Meyer", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1264:	; LINE 1264
	.byte	TOK_0
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_POSITION
@FastBasic_LINE_1265:	; LINE 1265
	.byte	TOK_CSTRING
	.byte	168, "   Deck Size:", 155, "Number Decks:", 155, " Dealer Hits:", 155, "  Dealer Tie:", 155, "  Black Jack:", 155, " Double Down:", 155, "Unbust Limit:", 155, "  Split Rule:", 155, "   Surrender:", 155, "   777 Bonus:", 155, "   678 Bonus:", 155, "     Players:", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1289:	; LINE 1289
	.byte	TOK_BYTE
	.byte	10
	.byte	TOK_PUSH_BYTE
	.byte	17
	.byte	TOK_POSITION
@FastBasic_LINE_1290:	; LINE 1290
	.byte	TOK_CSTRING
	.byte	21, "Press Start to Begin", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1301:	; LINE 1301
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_POSITION
@FastBasic_LINE_1302:	; LINE 1302
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_BYTE
	.byte	52
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_355
@FastBasic_LINE_1303:	; LINE 1303
	.byte	TOK_CSTRING
	.byte	26, "48 CARD DECKS(Spanish 21)", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1304:	; LINE 1304
	.byte	TOK_JUMP
	.word	jump_lbl_356
	.export	jump_lbl_355
jump_lbl_355:
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_BYTE
	.byte	48
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_357
@FastBasic_LINE_1305:	; LINE 1305
	.byte	TOK_CSTRING
	.byte	26, "52 CARD DECKS(Standard)  ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1306:	; LINE 1306
	.byte	TOK_JUMP
	.word	jump_lbl_356
	.export	jump_lbl_357
jump_lbl_357:
	.byte	TOK_VAR_LOAD
	.byte	4
	.byte	TOK_PUSH_BYTE
	.byte	56
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_356
@FastBasic_LINE_1307:	; LINE 1307
	.byte	TOK_CSTRING
	.byte	26, "56 CARD DECKS            ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1308:	; LINE 1308
	.export	jump_lbl_356
jump_lbl_356:
@FastBasic_LINE_1311:	; LINE 1311
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	5
	.byte	TOK_POSITION
@FastBasic_LINE_1312:	; LINE 1312
	.byte	TOK_VAR_LOAD
	.byte	5
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_360
@FastBasic_LINE_1313:	; LINE 1313
	.byte	TOK_CSTRING
	.byte	13, "FOUR DECKS  ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1314:	; LINE 1314
	.byte	TOK_JUMP
	.word	jump_lbl_361
	.export	jump_lbl_360
jump_lbl_360:
	.byte	TOK_VAR_LOAD
	.byte	5
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_362
@FastBasic_LINE_1315:	; LINE 1315
	.byte	TOK_CSTRING
	.byte	13, "SIX DECKS   ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1316:	; LINE 1316
	.byte	TOK_JUMP
	.word	jump_lbl_361
	.export	jump_lbl_362
jump_lbl_362:
	.byte	TOK_VAR_LOAD
	.byte	5
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_361
@FastBasic_LINE_1317:	; LINE 1317
	.byte	TOK_CSTRING
	.byte	13, "EIGHT DECKS ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1318:	; LINE 1318
	.export	jump_lbl_361
jump_lbl_361:
@FastBasic_LINE_1322:	; LINE 1322
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_POSITION
@FastBasic_LINE_1323:	; LINE 1323
	.byte	TOK_VAR_LOAD
	.byte	11
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_365
@FastBasic_LINE_1324:	; LINE 1324
	.byte	TOK_CSTRING
	.byte	26, "HOLDS ON 17 OR MORE      ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1325:	; LINE 1325
	.byte	TOK_JUMP
	.word	jump_lbl_366
	.export	jump_lbl_365
jump_lbl_365:
@FastBasic_LINE_1326:	; LINE 1326
	.byte	TOK_CSTRING
	.byte	26, "DEALER HITS SOFT 17      ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1327:	; LINE 1327
	.export	jump_lbl_366
jump_lbl_366:
@FastBasic_LINE_1330:	; LINE 1330
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	7
	.byte	TOK_POSITION
@FastBasic_LINE_1331:	; LINE 1331
	.byte	TOK_VAR_LOAD
	.byte	10
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_367
@FastBasic_LINE_1332:	; LINE 1332
	.byte	TOK_CSTRING
	.byte	26, "BET RETURNED. PUSH.      ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1333:	; LINE 1333
	.byte	TOK_JUMP
	.word	jump_lbl_368
	.export	jump_lbl_367
jump_lbl_367:
@FastBasic_LINE_1334:	; LINE 1334
	.byte	TOK_CSTRING
	.byte	26, "LOOSE BET, GOES TO DEALER", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1335:	; LINE 1335
	.export	jump_lbl_368
jump_lbl_368:
@FastBasic_LINE_1337:	; LINE 1337
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	8
	.byte	TOK_POSITION
@FastBasic_LINE_1338:	; LINE 1338
	.byte	TOK_VAR_LOAD
	.byte	14
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_369
@FastBasic_LINE_1339:	; LINE 1339
	.byte	TOK_CSTRING
	.byte	27, "NO BLACKJACK BONUS ALL 3:2", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1340:	; LINE 1340
	.byte	TOK_JUMP
	.word	jump_lbl_370
	.export	jump_lbl_369
jump_lbl_369:
	.byte	TOK_VAR_LOAD
	.byte	14
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_371
@FastBasic_LINE_1341:	; LINE 1341
	.byte	TOK_CSTRING
	.byte	27, "SUIT BLACKJACK PAYS 2:1   ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1342:	; LINE 1342
	.byte	TOK_JUMP
	.word	jump_lbl_370
	.export	jump_lbl_371
jump_lbl_371:
	.byte	TOK_VAR_LOAD
	.byte	14
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_370
@FastBasic_LINE_1343:	; LINE 1343
	.byte	TOK_CSTRING
	.byte	27, "SAME COLOR  PAYS 2:1      ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1344:	; LINE 1344
	.export	jump_lbl_370
jump_lbl_370:
@FastBasic_LINE_1347:	; LINE 1347
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	9
	.byte	TOK_POSITION
@FastBasic_LINE_1348:	; LINE 1348
	.byte	TOK_VAR_LOAD
	.byte	1
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_374
@FastBasic_LINE_1349:	; LINE 1349
	.byte	TOK_CSTRING
	.byte	20, "ANY TOTAL ALLOWED  ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1350:	; LINE 1350
	.byte	TOK_JUMP
	.word	jump_lbl_375
	.export	jump_lbl_374
jump_lbl_374:
	.byte	TOK_VAR_LOAD
	.byte	1
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_376
@FastBasic_LINE_1351:	; LINE 1351
	.byte	TOK_CSTRING
	.byte	20, "10 OR 11 ONLY      ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1352:	; LINE 1352
	.byte	TOK_JUMP
	.word	jump_lbl_375
	.export	jump_lbl_376
jump_lbl_376:
	.byte	TOK_VAR_LOAD
	.byte	1
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_378
@FastBasic_LINE_1353:	; LINE 1353
	.byte	TOK_CSTRING
	.byte	20, "9, 10, OR 11 ONLY  ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1354:	; LINE 1354
	.byte	TOK_JUMP
	.word	jump_lbl_375
	.export	jump_lbl_378
jump_lbl_378:
@FastBasic_LINE_1355:	; LINE 1355
	.byte	TOK_CSTRING
	.byte	20, "UPTO 3 CARDS       ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1356:	; LINE 1356
	.export	jump_lbl_375
jump_lbl_375:
@FastBasic_LINE_1360:	; LINE 1360
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	10
	.byte	TOK_POSITION
@FastBasic_LINE_1361:	; LINE 1361
	.byte	TOK_VAR_LOAD
	.byte	12
	.byte	TOK_PUSH_BYTE
	.byte	6
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_380
@FastBasic_LINE_1362:	; LINE 1362
	.byte	TOK_CSTRING
	.byte	24, "SIX CARD CHARLIE WINS  ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1363:	; LINE 1363
	.byte	TOK_JUMP
	.word	jump_lbl_381
	.export	jump_lbl_380
jump_lbl_380:
@FastBasic_LINE_1364:	; LINE 1364
	.byte	TOK_CSTRING
	.byte	24, "FIVE CARD CHARLIE WINS ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1365:	; LINE 1365
	.export	jump_lbl_381
jump_lbl_381:
@FastBasic_LINE_1368:	; LINE 1368
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	11
	.byte	TOK_POSITION
@FastBasic_LINE_1369:	; LINE 1369
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_382
@FastBasic_LINE_1370:	; LINE 1370
	.byte	TOK_CSTRING
	.byte	26, "SPLIT ANY PAIR 10=(J,Q,K)", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1371:	; LINE 1371
	.byte	TOK_JUMP
	.word	jump_lbl_383
	.export	jump_lbl_382
jump_lbl_382:
	.byte	TOK_VAR_LOAD
	.byte	13
	.byte	TOK_PUSH_1
	.byte	TOK_EQ
	.byte	TOK_CJUMP
	.word	jump_lbl_384
@FastBasic_LINE_1372:	; LINE 1372
	.byte	TOK_CSTRING
	.byte	26, "SPLIT EXACT PAIR         ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1373:	; LINE 1373
	.byte	TOK_JUMP
	.word	jump_lbl_383
	.export	jump_lbl_384
jump_lbl_384:
@FastBasic_LINE_1374:	; LINE 1374
	.byte	TOK_CSTRING
	.byte	26, "NO SPLIT ACES            ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1375:	; LINE 1375
	.export	jump_lbl_383
jump_lbl_383:
@FastBasic_LINE_1379:	; LINE 1379
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	12
	.byte	TOK_POSITION
@FastBasic_LINE_1380:	; LINE 1380
	.byte	TOK_VAR_LOAD
	.byte	67
	.byte	TOK_COMP_0
	.byte	TOK_CNJUMP
	.word	jump_lbl_386
@FastBasic_LINE_1381:	; LINE 1381
	.byte	TOK_CSTRING
	.byte	26, "NO PLAYER SURRENDER      ", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1382:	; LINE 1382
	.byte	TOK_JUMP
	.word	jump_lbl_387
	.export	jump_lbl_386
jump_lbl_386:
@FastBasic_LINE_1383:	; LINE 1383
	.byte	TOK_CSTRING
	.byte	26, "ALLOWED HALF BET RETURNED", 155
	.byte	TOK_PRINT_STR
@FastBasic_LINE_1384:	; LINE 1384
	.export	jump_lbl_387
jump_lbl_387:
@FastBasic_LINE_1389:	; LINE 1389
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	13
	.byte	TOK_POSITION
@FastBasic_LINE_1390:	; LINE 1390
	.byte	TOK_VAR_LOAD
	.byte	8
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_388
@FastBasic_LINE_1391:	; LINE 1391
	.byte	TOK_VAR_LOAD
	.byte	8
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	6
	.byte	TOK_DPEEK
	.byte	TOK_PRINT_STR
	.byte	TOK_BYTE
	.byte	155
	.byte	TOK_PUT
@FastBasic_LINE_1392:	; LINE 1392
	.export	jump_lbl_388
jump_lbl_388:
@FastBasic_LINE_1396:	; LINE 1396
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	14
	.byte	TOK_POSITION
@FastBasic_LINE_1397:	; LINE 1397
	.byte	TOK_VAR_LOAD
	.byte	9
	.byte	TOK_PUSH_BYTE
	.byte	2
	.byte	TOK_LT
	.byte	TOK_CJUMP
	.word	jump_lbl_389
@FastBasic_LINE_1398:	; LINE 1398
	.byte	TOK_VAR_LOAD
	.byte	9
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	7
	.byte	TOK_DPEEK
	.byte	TOK_PRINT_STR
	.byte	TOK_BYTE
	.byte	155
	.byte	TOK_PUT
@FastBasic_LINE_1399:	; LINE 1399
	.export	jump_lbl_389
jump_lbl_389:
@FastBasic_LINE_1401:	; LINE 1401
	.byte	TOK_BYTE
	.byte	13
	.byte	TOK_PUSH_BYTE
	.byte	15
	.byte	TOK_POSITION
@FastBasic_LINE_1402:	; LINE 1402
	.byte	TOK_VAR_LOAD
	.byte	20
	.byte	TOK_VAR_STORE
	.byte	85
@FastBasic_LINE_1403:	; LINE 1403
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_PUSH_BYTE
	.byte	4
	.byte	TOK_LT
	.byte	TOK_CRET
@FastBasic_LINE_1404:	; LINE 1404
	.byte	TOK_VAR_LOAD
	.byte	85
	.byte	TOK_USHL
	.byte	TOK_ADD_VAR
	.byte	16
	.byte	TOK_DPEEK
	.byte	TOK_PRINT_STR
	.byte	TOK_BYTE
	.byte	155
	.byte	TOK_PUT
@FastBasic_LINE_1406:	; LINE 1406
	.byte	TOK_RET
