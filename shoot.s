;;; iNESヘッダ情報
  .inesprg 1   ; 1x 15KB PRG
  .ineschr 1   ; 1x  8KB CHR
  .inesmir 1   ; background mirroring
  .inesmap 0   ; mapper 0 = NROM, no bank swapping

;-----------------------------------------------------
; MACRO
;-----------------------------------------------------
; ゼロページレジスタ（仮）にwordを代入
mov_iw	.macro
		lda	#LOW(\2)
		sta	<\1
		lda	#HIGH(\2)
		sta	<\1+1
		.endm

; ゼロページレジスタ（仮）にbyteを代入
mov_ib	.macro
		lda	#LOW(\2)
		sta	<\1
		.endm

	;---- メモリ上に置いたワード変数どうしを比較する ----
cmp_mw	.macro
		sec
		lda	(\1)
		sbc	(\2)
		lda	(\1)+1
		sbc	(\2)+1
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

;-----------------------------------------------------
; ゼロページ $0000-$00FF
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

;--- 引数
arg0	.ds	2
arg1	.ds	2
arg2	.ds	2
arg3	.ds	2


;-----------------------------------------------------
; スタック $0100-$01FF
;-----------------------------------------------------

;-----------------------------------------------------
; WORK RAM $0200-$7FFF
;-----------------------------------------------------
	.bss
WORKRAM:
OAMWORK:
	.ds	$100			; 
;---------------- GETDEC8,16,32 ワーク ------------------------------
DECSTR	.ds	10				; '00000',0 Reserved
ZSFLG	.ds	1				; ゼロサプレスフラグ

HEXSTR	.ds	4				; '00',0 Reserved

;--------------------------------------------------------------------
TESTVAL	.ds	3				; 文字列


;	.ds	$500			; これがワークRAM全部！少ない！



;-----------------------------------------------------
; Code (ROM LOW)
;-----------------------------------------------------

	.code
	.bank 0
	.org $8000

RESET:
	sei
	ldx	#$ff
	txs

; スクリーンオフ
    lda #%00010000      ; 初期化中は VBlank 割り込み禁止
    sta $2000
	lda	#$00
	sta	$2001

; WRAM初期化 ($0000-$07FF)
	lda	#$00
	ldx #$08
WRAMCLR:	
	ldy #$00	;256 times
	sta	<z0
	sta <z0+1
;	mov_iw	z0, 0		; マクロ
WRAMCLR_0:
	sta	(z0)
	inc	(z0)
	dey
	bne	WRAMCLR_0
	dex
	bne	WRAMCLR

	jsr	vsync			; VSync待ち

; パレットテーブルへ転送(BG用のみ転送)
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

; ネームテーブル　クリア
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

; スプライトテーブル初期化
	ldy	#$00
	lda #$00
	sta	$2003		; スプライトアドレスレジスタ
oam_clr_loop:
	sta $2004		; スプライトデータレジスタ
	dey
	bne	oam_clr_loop

;.ifdef 0
; ネームテーブルへ転送(画面の中央付近)
;	lda	#$21			; High
;	sta	$2006
;	lda	#$c7			; Low
;	sta	$2006
;	ldx	#$00
;copymap:
;	lda	string, x
;	beq	copyex
;	sec
;	sbc #$20
;	sta	$2007
;	inx
;	dey
;	bne	copymap

;copyex:
;.endif

	mov_iw	arg0,string
	ldx	#4
	ldy	#9
	jsr	PRINT

; 無限ループ
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

; スクロール設定
	lda	#$00
	sta	$2005
	sta	$2005

; スクリーンオン
	lda	#$08
	sta	$2000
	lda	#$1e
	sta	$2001
	
;	lda	#%10010000      ; 割り込み許可
;	sta $2000

stoploop:
	jsr	get_pad0
	jsr	vsync
	jmp stoploop

; VSync 待ち
vsync:
	lda     $2002
    bpl     vsync     ; VBlankが発生して $2002 の7ビット目が1になるまで待機
	rts


	;-------------------------------------------------
	; 文字列プリント
	; arg0 = String
	; Xreg = X位置
	; Yreg = Y位置
	;-------------------------------------------------
PRINT:
; ネームテーブルへ転送(画面の中央付近)
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
	; 1P 入力を読み取って $00 に保存する(形式: ABSTUDLR)
	; http://taotao54321.hatenablog.com/entry/2017/04/11/011850
	;-------------------------------------------------
get_pad0:
	; 読み取り準備。$4016 に 1, 0 の順で書き込む
	ldx #1
	stx $4016
	dex
	stx $4016

	; 1 ボタンずつ読み取り、結果のビットを $00 へシフトインしていく
	ldx #8
pad0_loop:
	lda $4016
	lsr a     ; ここでキャリーフラグ C に入力状態が入る
	rol $00   ; ローテートにより C がシフトインする
	dex
	bne pad0_loop
	rts

	;----------------------------------------------------
	;---- バイト値を３桁の１０進数の文字列に変換する ----
	;----------------------------------------------------
	; In:	Areg = 変換させたい数値
	;	ZSFLG ゼロサプレスフラグ 0=しない 1=する
	; Break:Xreg,Yreg
	; Ret:	DECSTR に変換された文字列が返る
GETDEC8:
	sta	<tmp0
	ldx #0					; x=オフセット
getdec8_lp1:
	ldy	#0					; Y=0じゃないフラグ
	; 除算の答えを求める
	lda #0
	sta <tmp1
getdec8_lp2:
	lda	tmp0
	cmp	dectbl8,x			; param>tbl?
	blo	gd8_next

	sec
	sbc	dectbl8,x
	sta	<tmp0
	iny						; ０じゃないフラグ立てる
	inc <tmp1
	jmp	getdec8_lp2

gd8_next:
	cpy #0				; ０じゃなかったらスキップ
	bne	gd8_next2

	lda	<tmp0
	cmp	#0
	bmi	gd8_next2

	lda	ZSFLG
	and	#1
	bne	gd8_next2			; ゼロサプレスするか？
	cpx	#2				; 最後の１文字か？
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
	;---- ワード値を５桁の数字列に変換する ----
	;------------------------------------------
	; In:	_AX = 変換させたい数値
	;	ZSFLG ゼロサプレスフラグ 0=しない 1=する
	; Break:ALL reg
	;	_BX,_CL
	; Ret:	DECSTR に変換された文字列が返る
GETDEC16:
	ldx	#0				; X=DECTBL16ｵﾌｾｯﾄ
	stx	<_CL			; ０じゃないフラグ　クリア
gd16_lp1:
	txa
	pha

	asl	a
	tax
	lda	dectbl16,x
	sta	<_BL
	lda	dectbl16+1,x
	sta	<_BH				; z1=割られる数

	pla
	tax
	ldy	#'0'
gd16_lp2:
	cmp_mw	_AX,_BX
	blo	gd16_ex1
	mov_ib	_CL,1		; 数えフラグ立てる
	iny					; 数値数える

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
	and	#1						; ０以外出現フラグ
	bne	gd16_ex2

	lda	ZSFLG
	and	#1
	bne	gd16_ex2		; ゼロサプレスする
	cpx	#4			; 文字列のしまいか？
	beq	gd16_ex2
	
	ldy	#' '			; ゼロサプレスしない
gd16_ex2:
	tya
	sta	DECSTR,x		; 文字列を作成
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
	;---- ロングワード値を８桁の数字列に変換する ----
	;----                                        ----
	;---- ....という事は下位２４ビットしか有効で ----
	;---- 無いんですよね。曖昧だなぁ・・・。　　 ----
	;------------------------------------------------
	; In:	_BX:_AX = 変換させたい数値
	;	ZSFLG ゼロサプレスフラグ 0=しない 1=する
	; Break:ALL reg
	;	_CX,_DX,tmp1
	; Ret:	DECSTR に変換された文字列が返る
GETDEC32:
	ldx	#0				; X=DECTBL16ｵﾌｾｯﾄ
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
	sta	<_CH				; _CX=割る数L
	lda	dectbl32+2,x
	sta	<_DL
	lda	dectbl32+3,x
	sta	<_DH				; _DX=割る数H
	pla
	tax

	ldy	#'0'
gd32_lp2:
	;- ３２ビット比較 -
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
	iny					; 数値数える
	mov_ib	tmp1,1		; ０でない数値が出たフラグ

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
;	bbs0	tmp1,gd32_ex3		; ０以外出現フラグ
	lda	<tmp1
	and	#1
	bne	gd32_ex3
;	tst	#1,ZSFLG		; ゼロサプレスチェック
	lda	ZSFLG
	and	#1
	bne	gd32_ex3
	cpx	#7			; 文字列のしまいか？
	beq	gd32_ex3

	ldy	#' '
gd32_ex3:
	tya
	sta	DECSTR,x		; 文字列を作成
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
	;---- バイト値を２桁の１６進数の文字列に変換する ----
	;----------------------------------------------------
	; In:	Areg = 変換させたい数値
	; Break:Xreg,Yreg
	; Ret:	HEXSTR に変換された文字列が返る
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

; パレットテーブル
palettes:
	.byte	$0f, $00, $10, $20
	.byte	$0f, $06, $16, $26
	.byte	$0f, $08, $18, $28
	.byte	$0f, $0a, $1a, $2a

; 表示文字列
string:
	.byte	"HELLO, I'M PIROTA!"
	.byte	0


;-----------------------------------------------------
; Code (ROM HIGH) 固定bank
;-----------------------------------------------------
	.bank 1
;	.org	$C000


; 割り込みベクタ
	.org    $FFFA           ; $FFFA から開始

	.word	$0000
	.word	RESET
	.word	$0000

;------------------------
; フォントデータを読み込み
;------------------------
  .bank 2
    ;; BG
    .org    $0000
	.incbin "bin/ascii.chr"
