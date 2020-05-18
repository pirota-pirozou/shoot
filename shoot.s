;;; iNES�w�b�_���
  .inesprg 1   ; 1x 15KB PRG
  .ineschr 1   ; 1x  8KB CHR
  .inesmir 1   ; background mirroring
  .inesmap 0   ; mapper 0 = NROM, no bank swapping

;-----------------------------------------------------
; MACRO
;-----------------------------------------------------
; �[���y�[�W���W�X�^�i���j��word����
mov_iw	.macro
		lda	#LOW(\2)
		sta	<\1
		lda	#HIGH(\2)
		sta	<\1+1
		.endm

; �[���y�[�W���W�X�^�i���j��byte����
mov_ib	.macro
		lda	#LOW(\2)
		sta	<\1
		.endm

	;---- ��������ɒu�������[�h�ϐ��ǂ������r���� ----
cmp_mw	.macro
		sec
		lda	(\1)
		sbc	(\2)
		lda	(\1)+1
		sbc	(\2)+1
		.endm

mov_mw	.macro
		clc
		lda	(\2)
		sta (\1)
		lda	(\2)+1
		sta (\1)+1
		.endm

add_iw	.macro
		clc
		lda	<(\1)
		adc	#LOW(\2)
		sta <(\1)
		lda	<(\1)+1
		adc #HIGH(\2)
		sta <(\1)+1
		.endm



add_zw	.macro
		clc
		lda	<(\1)
		adc	<(\2)
		sta <(\1)
		lda	<(\1)+1
		adc <(\2)+1
		sta <(\1)+1
		.endm

blo		.macro
		bcc	\1
		.endm

bhi		.macro
		bcs	\1
		.endm
		

; HuC6280
say		.macro
		sta	<tmp0
		tya	
		ldy	<tmp0
		.endm

sax		.macro
		sta	<tmp0
		txa	
		ldx	<tmp0
		.endm

; PAD Bit
PAD_A	   = $01
PAD_B	   = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_UP     = $10
PAD_DOWN   = $20
PAD_LEFT   = $40
PAD_RIGHT  = $80

PAD_UDLR   = $F0




;-----------------------------------------------------
; �[���y�[�W $0000-$00FF
;-----------------------------------------------------
	.zp
z0	.ds	2
z1	.ds	2

_AL	.ds	1
_AH	.ds	1
_BL	.ds	1
_BH	.ds	1
_CL	.ds	1
_CH	.ds	1
_DL	.ds	1
_DH	.ds	1

_AX = _AL
_BX = _BL
_CX = _CL
_DX = _DL

tmp0	.ds	1
tmp1	.ds	1
tmp2	.ds	1
tmp3	.ds	1

;--- ����
arg0	.ds	2
arg1	.ds	2
arg2	.ds	2
arg3	.ds	2

; �L�[�̒l
GetKey		.ds	1				
GetKey_bak	.ds	1
GetTrg		.ds	1

GetRep		.ds	1	; �L�[���̓��s�[�g
RepSt		.ds	1	; �L�[���̓��s�[�g�X�e�[�g
RepCou		.ds	1	; �L�[���̓��s�[�g�J�E���^


;-----------------------------------------------------
; �X�^�b�N $0100-$01FF
;-----------------------------------------------------

;-----------------------------------------------------
; WORK RAM $0200-$7FFF
;-----------------------------------------------------
	.bss
WORKRAM:
OAMWORK:
	.ds	$100			; 
;---------------- GETDEC8,16,32 ���[�N ------------------------------
DECSTR	.ds	10				; '00000',0 Reserved
ZSFLG	.ds	1				; �[���T�v���X�t���O

HEXSTR	.ds	4				; '00',0 Reserved

;--------------------------------------------------------------------
TESTVAL	.ds	3				; ������



;	.ds	$500			; ���ꂪ���[�NRAM�S���I���Ȃ��I


;-----------------------------------------------------
; Code (ROM HIGH) �Œ�bank
;-----------------------------------------------------
	.code
	.bank 1

; ���荞�݃x�N�^
	.org    $FFFA           ; $FFFA ����J�n

	.word	Vblank_int
	.word	RESET
	.word	$0000

;-----------------------------------------------------
; Code (ROM LOW)
;-----------------------------------------------------

	.code
	.bank 0
	.org $8000

RESET:
    sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx $4017  ; disable APU frame IRQ
    ldx #$ff
    txs        ; Set up stack
    inx        ; now X = 0
    stx $2000  ; disable NMI
    stx $2001  ; disable rendering
    stx $4010  ; disable DMC IRQs

	jsr	vsync			; VSync�҂�

; �X�N���[���I�t
;   lda #%00010000      ; ���������� VBlank ���荞�݋֎~
;    sta $2000
;	lda	#$00
;	sta	$2001

; WRAM������ ($0000-$07FF)
    ldx #0
    txa
clrmem:
    sta $000,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne clrmem

	; ---- OAM������
	ldx	#0
	lda	#240
initoam:
	sta	OAMWORK,x
	inx
	inx
	inx
	inx
	bne initoam

; �p���b�g�e�[�u���֓]��(BG�p�̂ݓ]��)
	lda	#$3f
	sta	$2006
	lda	#$00
	sta	$2006
	ldx	#$00
	ldy	#$10
copypal:
	lda	palettes, x
	sta	$2007
	inx
	dey
	bne	copypal

; �l�[���e�[�u���@�N���A
	lda	#$20			; High
	sta	$2006
	lda	#$00			; Low
	sta	$2006

	ldy	#$10
nameclr:	
	ldx	#$00
	lda	#$00
nameclr_0:
	sta	$2007
	dex
	bne	nameclr_0
	dey
	bne	nameclr

	mov_iw	arg0,string
	ldx	#4
	ldy	#9
	jsr	PRINT

; �������[�v
	lda	#123
	sta	TESTVAL
;mainloop:
	lda	#1
	sta	ZSFLG

	lda	#255
	jsr	GETDEC8

	mov_iw	arg0,DECSTR
	ldx	#4
	ldy #10
	jsr	PRINT


	mov_iw _AX,65535
	jsr	GETDEC16

	mov_iw	arg0,DECSTR
	ldx	#4
	ldy #11
	jsr	PRINT

;	mov_iw	_AX,65535
	mov_iw	_AX,2
	mov_iw	_BX,1
	jsr	GETDEC32

	mov_iw	arg0,DECSTR
	ldx	#4
	ldy	#12
	jsr	PRINT

	lda	#$DE
	jsr	GETHEX

	mov_iw	arg0,HEXSTR
	ldx	#4
	ldy #13
	jsr	PRINT


	mov_mw _AX,TESTVAL
	jsr	GETDEC16

	mov_iw	arg0,DECSTR
	ldx	#4
	ldy #11
	jsr	PRINT

	; TESTVAL++ 
	clc
	lda	TESTVAL
	adc #1
	sta TESTVAL
	lda	TESTVAL
	adc #0
	sta TESTVAL


; �X�N���[���ݒ�
	lda	#$00
	sta	$2005
	sta	$2005

; �X�N���[���I��
	lda	#$88
	sta	$2000
	lda	#$1e
	sta	$2001
	
	CLI	; ���荞�݋���



stoploop:
	nop
;	jsr	get_pad0


	;--- vsync wait ----
;	jsr	vsync

	jmp stoploop

; VSync �҂�
vsync:
	lda     $2002
    bpl     vsync     ; VBlank���������� $2002 ��7�r�b�g�ڂ�1�ɂȂ�܂őҋ@
	rts

	;------------
	; VBlank int
	;------------
Vblank_int:
	SEI			; Disable INT
	php
	pha
	txa
	pha
	tya
	pha
	; �X�v���C�g�`��(DMA�𗘗p)
	lda #HIGH(OAMWORK)  ; OAM
	sta $4014 ; �X�v���C�gDMA���W�X�^��A���X�g�A���āA�X�v���C�g�f�[�^��DMA�]������

;	�p�b�h����
;    tmppad = GetKey;
;    GetTrg = ~GetKey_bak & tmppad;
;    GetKey_bak = tmppad;
	jsr	get_pad0
	sta <GetKey
	sta	<tmp0

	lda <GetKey_bak
	eor	#255
	and	<tmp0
	sta <GetTrg

;      // ���s�[�g�擾
;      tmppad = GetKey & PAD_UDLR;
;      if (tmppad == 0) {
;        // �����L�[�����ꂽ�ꍇ
;        RepSt = 0;
;        GetRep = 0;
;      } else {
;        if (RepSt == 0) {
;          GetRep = GetKey;
;          RepCou = 15;            // ���s�[�g�����^�C��
;          RepSt++;
;      } else {
;        if ((--RepCou)<=0) {
;          GetRep = GetKey;
;          RepCou = 2;           // ���s�[�g�p���^�C��
;        } else {
;          GetRep = 0;
;        }
;      }
;      }

	; ���s�[�g����
	and	#PAD_UDLR
	bne pad_touch

	; �L�[�������
	ldx	#0
	stx <RepSt
	stx <GetRep
	jmp pad_touch_ex

pad_touch:
	ldx <RepSt
	bne pad_touch_0

	; �������s�[�g
	lda <GetKey
	sta <GetRep			; GetRep = GetKey
	lda	#15				; ���s�[�g�����^�C��
	sta	<RepCou
	inc	<RepSt
	jmp pad_touch_ex

pad_touch_0	
	; �񎟃��s�[�g
	ldx	<RepCou
	dex
	beq	pad_touch_1

	lda <GetKey
	sta <GetRep			; GetRep = GetKey
	lda	#2				; ���s�[�g�^�C��
	sta	<RepCou

	jmp	pad_touch_ex

pad_touch_1:
	; �L�[���͂Ȃ��������Ƃ�
	lda	#0
	sta	<GetRep

	; ���s�[�g�������I��
pad_touch_ex:




Vblank_exit:
	pla
	tay
	pla
	tax
	pla
	plp
	CLI			; Enable INT
	rti


	;-------------------------------------------------
	; ������v�����g
	; arg0 = String
	; Xreg = X�ʒu
	; Yreg = Y�ʒu
	;-------------------------------------------------
PRINT:
; �l�[���e�[�u���֓]��(��ʂ̒����t��)
;	Y * 32 + X + $2000
	lda	#0
	stx <_AL
	sta	<_AH
	add_iw	_AX,#$2000
	sty	<_BL
	sta	<_BH
;	add_zw	_BX,_BX
	add_zw	_BX,_BX
	add_zw	_BX,_BX
	add_zw	_BX,_BX
	add_zw	_BX,_BX
	add_zw	_BX,_BX
	add_zw	_BX,_AX

	lda	<_BH
	sta	$2006			; High
	lda	<_BL
	sta	$2006			; Low
	ldy	#$00
prt_loop:
	lda	[arg0],y
	beq	prt_ex
	sec
	sbc #$20
	sta	$2007
	iny
	bne	prt_loop
prt_ex:
	rts

	;-------------------------------------------------
	; 1P ���͂�ǂݎ���� $00 �ɕۑ�����(�`��: ABSTUDLR)
	; http://taotao54321.hatenablog.com/entry/2017/04/11/011850
	;-------------------------------------------------
get_pad0:
	; �ǂݎ�菀���B$4016 �� 1, 0 �̏��ŏ�������
	ldx #1
	stx $4016
	dex
	stx $4016

	; 1 �{�^�����ǂݎ��A���ʂ̃r�b�g�� $00 �փV�t�g�C�����Ă���
	ldx #8
pad0_loop:
	lda $4016
	lsr a     ; �����ŃL�����[�t���O C �ɓ��͏�Ԃ�����
	rol $00   ; ���[�e�[�g�ɂ�� C ���V�t�g�C������
	dex
	bne pad0_loop
	rts

	;----------------------------------------------------
	;---- �o�C�g�l���R���̂P�O�i���̕�����ɕϊ����� ----
	;----------------------------------------------------
	; In:	Areg = �ϊ������������l
	;	ZSFLG �[���T�v���X�t���O 0=���Ȃ� 1=����
	; Break:Xreg,Yreg
	; Ret:	DECSTR �ɕϊ����ꂽ�����񂪕Ԃ�
GETDEC8:
	sta	<tmp0
	ldx #0					; x=�I�t�Z�b�g
getdec8_lp1:
	ldy	#0					; Y=0����Ȃ��t���O
	; ���Z�̓��������߂�
	lda #0
	sta <tmp1
getdec8_lp2:
	lda	tmp0
	cmp	dectbl8,x			; param>tbl?
	blo	gd8_next

	sec
	sbc	dectbl8,x
	sta	<tmp0
	iny						; �O����Ȃ��t���O���Ă�
	inc <tmp1
	jmp	getdec8_lp2

gd8_next:
	cpy #0				; �O����Ȃ�������X�L�b�v
	bne	gd8_next2

	lda	<tmp0
	cmp	#0
	bmi	gd8_next2

	lda	ZSFLG
	and	#1
	bne	gd8_next2			; �[���T�v���X���邩�H
	cpx	#2				; �Ō�̂P�������H
	beq	gd8_next2
	lda	#' '
	jmp	gd8_next3
gd8_next2:
	lda	<tmp1
	ldy #0
	clc
	adc	#'0'
gd8_next3:
	sta	DECSTR,x
	inx
	cpx	#3
	bcc	getdec8_lp1

	lda	#0
	sta	DECSTR,x			; eos
	rts

dectbl8:
	.byte	100
	.byte	10
	.byte	1

	;------------------------------------------
	;---- ���[�h�l���T���̐�����ɕϊ����� ----
	;------------------------------------------
	; In:	_AX = �ϊ������������l
	;	ZSFLG �[���T�v���X�t���O 0=���Ȃ� 1=����
	; Break:ALL reg
	;	_BX,_CL
	; Ret:	DECSTR �ɕϊ����ꂽ�����񂪕Ԃ�
GETDEC16:
	ldx	#0				; X=DECTBL16�̾��
	stx	<_CL			; �O����Ȃ��t���O�@�N���A
gd16_lp1:
	txa
	pha

	asl	a
	tax
	lda	dectbl16,x
	sta	<_BL
	lda	dectbl16+1,x
	sta	<_BH				; z1=�����鐔

	pla
	tax
	ldy	#'0'
gd16_lp2:
	cmp_mw	_AX,_BX
	blo	gd16_ex1
	mov_ib	_CL,1		; �����t���O���Ă�
	iny					; ���l������

	sec					; z0-z1
	lda	<_AL
	sbc	<_BL
	sta	<_AL
	lda	<_AH
	sbc	<_BH
	sta	<_AH
	jmp	gd16_lp2

gd16_ex1:
	lda	<_CL
	and	#1						; �O�ȊO�o���t���O
	bne	gd16_ex2

	lda	ZSFLG
	and	#1
	bne	gd16_ex2		; �[���T�v���X����
	cpx	#4			; ������̂��܂����H
	beq	gd16_ex2
	
	ldy	#' '			; �[���T�v���X���Ȃ�
gd16_ex2:
	tya
	sta	DECSTR,x		; ��������쐬
	inx
	cpx	#5
	bcc	gd16_lp1

	lda	#0
	sta	DECSTR,x		; EOS
	rts

dectbl16:
	.word	10000
	.word	1000
	.word	100
	.word	10
	.word	1

	;------------------------------------------------
	;---- �����O���[�h�l���W���̐�����ɕϊ����� ----
	;----                                        ----
	;---- ....�Ƃ������͉��ʂQ�S�r�b�g�����L���� ----
	;---- ������ł���ˁB�B�����Ȃ��E�E�E�B�@�@ ----
	;------------------------------------------------
	; In:	_BX:_AX = �ϊ������������l
	;	ZSFLG �[���T�v���X�t���O 0=���Ȃ� 1=����
	; Break:ALL reg
	;	_CX,_DX,tmp1
	; Ret:	DECSTR �ɕϊ����ꂽ�����񂪕Ԃ�
GETDEC32:
	ldx	#0				; X=DECTBL16�̾��
	stx	<tmp1
gd32_lp1:
	txa
	pha
	asl	a
	asl	a				; x4
	tax

	lda	dectbl32,x
	sta	<_CL
	lda	dectbl32+1,x
	sta	<_CH				; _CX=���鐔L
	lda	dectbl32+2,x
	sta	<_DL
	lda	dectbl32+3,x
	sta	<_DH				; _DX=���鐔H
	pla
	tax

	ldy	#'0'
gd32_lp2:
	;- �R�Q�r�b�g��r -
	sec
	lda	<_AL				; _AX-_CX
	sbc	<_CL
	lda	<_AH
	sbc	<_CH

	lda	<_BL				; _BX-_DX
	sbc	<_DL
	lda	<_BH
	sbc	<_DH
	blo	gd32_ex1

gd32_lp2_2:
	iny					; ���l������
	mov_ib	tmp1,1		; �O�łȂ����l���o���t���O

	sec
	lda	<_AL				; _AX-_CX
	sbc	<_CL
	sta	<_AL
	lda	<_AH
	sbc	<_CH
	sta	<_AH

	lda	<_BL				; _BX-_DX
	sbc	<_DL
	sta	<_BL
	lda	<_BH
	sbc	<_DH
	sta	<_BH

	jmp	gd32_lp2

gd32_ex1:
;	bbs0	tmp1,gd32_ex3		; �O�ȊO�o���t���O
	lda	<tmp1
	and	#1
	bne	gd32_ex3
;	tst	#1,ZSFLG		; �[���T�v���X�`�F�b�N
	lda	ZSFLG
	and	#1
	bne	gd32_ex3
	cpx	#7			; ������̂��܂����H
	beq	gd32_ex3

	ldy	#' '
gd32_ex3:
	tya
	sta	DECSTR,x		; ��������쐬
	inx
	cpx	#8
	bcc	gd32_lp1

gd32_exit:
	lda	#0
	sta	DECSTR,x		; EOS
	rts

dectbl32:
;	dw	$E100,$05F5			;100000000
	dw	$9680,$0098			; 10000000
	dw	$4240,$000F			;  1000000
	dw	$86A0,$0001			;   100000
	dw	$2710,$0000			;    10000
	dw	$03E8,$0000			;     1000
	dw	$0064,$0000			;      100
	dw	$000A,$0000			;       10
	dw	$0001,$0000			;        1

	;----------------------------------------------------
	;---- �o�C�g�l���Q���̂P�U�i���̕�����ɕϊ����� ----
	;----------------------------------------------------
	; In:	Areg = �ϊ������������l
	; Break:Xreg,Yreg
	; Ret:	HEXSTR �ɕϊ����ꂽ�����񂪕Ԃ�
GETHEX:
	tay
gethex_lp1:
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	tax
	lda	hextbl,x
	sta	HEXSTR

	tya
	and	#$0F
	tax
	lda	hextbl,x
	sta	HEXSTR+1
	lda	#0
	sta	HEXSTR+2			; EOS

	rts

hextbl:
	.byte	"0123456789ABCDEF"

; �p���b�g�e�[�u��
palettes:
	.byte	$0f, $00, $10, $20
	.byte	$0f, $06, $16, $26
	.byte	$0f, $08, $18, $28
	.byte	$0f, $0a, $1a, $2a

; �\��������
string:
	.byte	"HELLO, I'M PIROTA!"
	.byte	0


;-----------------------------------------------------
;------------------------
; �t�H���g�f�[�^��ǂݍ���
;------------------------
  .bank 2
    ;; BG
    .org    $0000
	.incbin "bin/ascii.chr"
