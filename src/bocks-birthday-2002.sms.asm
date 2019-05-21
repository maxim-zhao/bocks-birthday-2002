; Bock's birthday 2002

; VRAM mapping stuff
.define SpriteSet           1       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define NameTableAddress    $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes

;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
DEFAULTSLOT 2
SLOTSIZE $4000
SLOT 0 $0000
SLOT 1 $4000
SLOT 2 $8000
.ENDME

.ROMBANKMAP
BANKSTOTAL 5
BANKSIZE $4000
BANKS 5
.ENDRO

.bank 0 slot 0
.org $0000

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 1.00,"Bock's Birthday 2002","","Maxim"

;Useful defines and macros:
.include "graphics.inc"

.org $0000
;==============================================================
; Boot section
;==============================================================
.section "!Boot section" FORCE   ; Standard stuff (for the SMS anyway)
    di              ; disable interrupts (re-enable later)
    im 1            ; Interrupt mode 1
    ld sp, $dff0    ; load stack pointer to not-quite-the-end of user RAM (avoiding paging regs)
    jp main         ; jump to main program
.ends

.org $0066
;==============================================================
; Pause button handler
;==============================================================
.section "!NMI handler" FORCE
    retn
.ends


;==============================================================
; Main program
;==============================================================
main:
    ; Load VDP with default values, thanks to Mike G :P
    ; hl = address of data
    ; b = size of data
    ; c = port
    ; otir = while (b>0) do {out (hl),c; b--}
    ld hl,VdpData
    ld b,VdpDataEnd-VdpData
    ld c,$bf
    otir

    ; Disable sprites
    call NoSprites

    ld ix,DrawData      ; ix = table lookup
                        ; +0-1 = tiles
                        ; +2-3 = number of tiles
                        ; +4-5 = tile numbers
                        ; +6   = 1 if tile words, 0 if bytes (could use +2 instead)
                        ; +7   = bank it's stored in
                        ; +8-9 = palette
                        ; +a   = palette count
                        ; +b   = 0
                        ; +c-d = (IsPic=1)
DrawingLoop:    ; Main drawing loop
    ; 1. See if we have data
    push ix             ; keep it pushed
    ld a,(ix+0)
    and (ix+1)
    cp $ff              ; if pointer is to $ffff then repeat
    jp nz,+
    pop ix
    ld ix,DrawData
    push ix
  +:
    ; 2. Screen off
    ld a,%10000000
    out ($bf),a
    ld a,$81
    out ($bf),a

    ; 2.5 Is it text?
    ld a,(ix+$c)
    cp 1
    jp z,LoadImage
    ; It's text
    push ix
    call LoadFont
    call ClearNameTable
    pop ix
    ; Write text
    ld h,(ix+1)
    ld l,(ix+0)
    ld iy,NameTableAddress+32*2*10
    call WriteASCII
    jp FadeIn

    LoadImage:
    ; Load image ----------------------------
    ; 3. page it in
    ld a,(ix+7)
    ld ($ffff),a
    ; 4. Load tiles - 0 -> hl, (ix) -> ix, (ix+2) -> bc, 4 -> d
    push ix
        ld h,(ix+1)
        ld l,(ix+0)
        ld b,(ix+3)
        ld c,(ix+2)
        push hl
        pop ix
        ld hl,0
        ld d,4
        call LoadTiles
    pop ix
    ; 5. Draw tile map - (ix+4) -> hl, (ix+6) = 1 if bytes
    ld hl,NameTableAddress
    call VRAMToHL
    ld h,(ix+5)
    ld l,(ix+4)
    ld a,(ix+6)
    cp 1
    push af
        call z,DrawScreenBytes
    pop af
    call nz,DrawScreenWords

    ; 6. Copy palette to RAM
.define PaletteFadeRAM $c000
    ld a,clRGB000   ; blank unused colours
    ld bc,16
    ld hl,PaletteFadeRAM
  -:ld (hl),a
    inc hl
    djnz -

    ld de,PaletteFadeRAM    ; de = where to
    ld h,(ix+9)     ; hl = where from
    ld l,(ix+8)
    ld c,(ix+$a)    ; bc = how many
    ld b,0
    ldir            ; copy
    
    ; ---------------------------------

    FadeIn:
    ; 7. Fade palette in
    ld a,0
    ld (PaletteFadeRAM+$20),a
    call FadePalette

    ; 8. Wait for button press
    call WaitForButton

    ; 9. Fade palette out
    ld a,1
    ld (PaletteFadeRAM+$20),a
    call FadePalette

    ; 10. Add DrawDataStructureSize to ix and repeat
    ld bc,DrawDataStructureSize
    pop ix
    add ix,bc
    jp DrawingLoop

ShowNextText:
    call LoadFont
    call ClearNameTable
    ; Write text
    ld hl,Text
    ld iy,NameTableAddress+32*2*10
    call WriteASCII
    ; Fade palette in
    ld a,0
    ld (PaletteFadeRAM+$20),a
    call FadePalette

    call WaitForButton

    ; Fade palette out
    ld a,1
    ld (PaletteFadeRAM+$20),a
    call FadePalette
    ret

Text:
;    12345678901234567890123456789012
.db "         HELLO WORLD!",10
.db "HELLO AGAIN",0
.db "MESSAGE 2",0


; Palette fader
PaletteFadeLookup:
.db 0,0,0,0,0,0,1,1,0,1,1,2,0,1,2,3

AdjustColour:   ; pass colour in a, amount<<2 in b; returns adjusted colour in a, b unaffected
    or b            ; now a=amount:colour, which is the index to PaletteFadeLookup
    ld d,0
    ld e,a
    ld hl,PaletteFadeLookup
    add hl,de
    ld a,(hl)     ; now a=adjusted amount
    ret

FadePalette:
    ; Load $C000-$C00f with palette
    ; Set $C020 to 1 to fade to black, 0 to fade from black

    ; Initial multiplication value:
    ld b,%0000  ; Zero, unless a!=0
    ld a,(PaletteFadeRAM+$20)
    cp 0
    jp z,+
    ld b,%1000
    +:

    PaletteFadeLoop:
    ; Copy palette using lookup to fade colours in
        ld c,16 ; 16 palette entries to process
        ld ix,PaletteFadeRAM ; original palette and offset to new one
        AdjustColourAtIX:
            push bc
            ld a,(ix+0)     ; red
            and %00000011
            call AdjustColour
            ld c,a
            ld a,(ix+0)     ; green
            srl a           ; >>2
            srl a
            and %00000011
            call AdjustColour
            sla a
            sla a
            or c
            ld c,a
            ld a,(ix+0)     ; blue
            srl a           ; >>4
            srl a
            srl a
            srl a
            and %00000011
            call AdjustColour
            sla a
            sla a
            sla a
            sla a
            or c
            ld c,a
            ; a is now the colour I want
            ld (ix+$10),a   ; write to fading palette
            inc ix
            pop bc
            dec c
            jr nz,AdjustColourAtIX

        ; Full palette fade done in RAM, now load it
        ld hl,PaletteFadeRAM+$10
        push bc
            ld b,16
            ld c,0
            call LoadPalette

            ld a,%11000000  ; Turn screen on
            out ($bf),a
            ld a,$81
            out ($bf),a

            ld c,5
            call WaitForCFrames
        pop bc
        ; Are we fading in or out?
        ld a,(PaletteFadeRAM+$20)
        cp 0
        jp z,+
        ; FadeToBlack=1
        ; so decrement b and jump if >=0
        ld a,b
        sub %100
        ld b,a
        jp p,PaletteFadeLoop   ; if it's >0 then repeat
        jp ++
        +:
        ; FadeToBlack=0
        ; so inceremnt b and jump if !=%1000
        ld a,b
        add a,%100
        ld b,a
        cp %10000   ; if it's not 4<<2 then repeat
        jp nz,PaletteFadeLoop
        ++:
    ret


WaitForButton:
    push af
      -:in a,($dc)
        and %00010000
        cp  %00000000
        jp nz,-
        ; Button down, wait for it to vome up
      -:in a,($dc)
        and %00010000
        cp  %00010000
    pop af
    ret

DrawScreenWords:    ; set hl to address to copy from
    ld c,$be
    ld b,$00
    otir
    otir
    otir
    otir
    otir
    otir
    ret

DrawScreenBytes:
    ld bc,32*24
  -:ld a,(hl)
    out ($be),a     ; first byte
    ld a,$00
    out ($be),a     ; high byte = 0

    inc hl          ; move to next data
    dec bc          ; decrement counter
    ld a,b
    or c
    jp nz,-         ; repeat if counter>0

    ret


FontData:
.include "SMS Cast font.inc"
FontPalette:
.db clRGB000,clRGB002,clRGB223

LoadFont:
    ; Load tiles
    ld hl,0
    ld ix,FontData
    ld bc,$40
    ld d,2
    call LoadTiles
    ; Copy palette to RAM
    ld a,clRGB000   ; blank unused colours
    ld bc,16
    ld hl,PaletteFadeRAM
  -:ld (hl),a
    inc hl
    djnz -
    ld de,PaletteFadeRAM    ; de = where to
    ld hl,FontPalette       ; hl = where from
    ld bc,3                 ; bc = how many
    ldir            ; copy
    ret






;==============================================================
; VDP initialisation data
;==============================================================
VdpData:
.db %00000100,$80
;    |||||||`- Disable synch
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during scrolling
;    `-------- Fix right 8 columns during scrolling
.db %10000000,$81
;    ||||| |`- Zoomed sprites -> 16x16 pixels
;    ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;    ||||`---- 30 row/240 line mode
;    |||`----- 28 row/224 line mode
;    ||`------ VBlank interrupts
;    |`------- Enable display
;    `-------- Must be set (VRAM size bit)
.db NameTableAddress>>10,$82
.db SpriteTableAddress>>7,$85
.db SpriteSet<<2,$86
.db $f,$87
;    `-------- Border palette colour (sprite palette)
.db $00,$88
;    ``------- Horizontal scroll
.db $00,$89
;    ``------- Vertical scroll
.db $ff,$8a
;    ``------- Line interrupt spacing
VdpDataEnd:

DrawData:
.define IsText 0
.define IsPic  1
.define DrawDataStructureSize 7*2
.define TextFiller 0,0,0,0,0,IsText

.dw Text1,     0,         0,            0,        0,0 ,IsText
.dw Text2,     0,         0,            0,        0,0 ,IsText
.dw Text3,     0,         0,            0,        0,0 ,IsText
.dw Text4,     0,         0,            0,        0,0 ,IsText
.dw Text5,     0,         0,            0,        0,0 ,IsText
.dw Tiles3 ,$13c,TileNums3 ,0+:Tiles3 <<8,Palette3 ,16,IsPic
.dw Text6,     0,         0,            0,        0,0 ,IsText
.dw Tiles2 ,$0cf,TileNums2 ,1+:Tiles2 <<8,Palette2 ,9 ,IsPic
.dw Tiles6 ,$0f9,TileNums6 ,1+:Tiles6 <<8,Palette6 ,13,IsPic
.dw Tiles7 ,$0dd,TileNums7 ,1+:Tiles7 <<8,Palette7 ,14,IsPic
.dw Text7,     0,         0,            0,        0,0 ,IsText
.dw Tiles8 ,$0f0,TileNums8 ,1+:Tiles8 <<8,Palette8 ,14,IsPic
.dw Text8,     0,         0,            0,        0,0 ,IsText
.dw Tiles9 ,$091,TileNums9 ,1+:Tiles9 <<8,Palette9 ,14,IsPic
.dw Text9,     0,         0,            0,        0,0 ,IsText
.dw Text10,    0,         0,            0,        0,0 ,IsText
.dw Text11,    0,         0,            0,        0,0 ,IsText
.dw Text12,    0,         0,            0,        0,0 ,IsText
.dw Text13,    0,         0,            0,        0,0 ,IsText
.dw Text14,    0,         0,            0,        0,0 ,IsText
.dw Tiles10,$0ba,TileNums10,1+:Tiles10<<8,Palette10,16,IsPic
.dw Text15,    0,         0,            0,        0,0 ,IsText
.dw Tiles1 ,$143,TileNums1 ,0+:Tiles1 <<8,Palette1 ,9 ,IsPic
.dw Text17,    0,         0,            0,        0,0 ,IsText
.dw Tiles5 ,$067,TileNums5 ,1+:Tiles5 <<8,Palette5 ,5 ,IsPic
.dw Text18,    0,         0,            0,        0,0 ,IsText
.dw Text19,    0,         0,            0,        0,0 ,IsText
.dw Text20,    0,         0,            0,        0,0 ,IsText
.dw Text21,    0,         0,            0,        0,0 ,IsText
.dw Tiles4 ,$01b,TileNums4 ,1+:Tiles4 <<8,Palette4 ,6 ,IsPic
.dw Text22,    0,         0,            0,        0,0 ,IsText
.dw Text23,    0,         0,            0,        0,0 ,IsText
.dw Text24,    0,         0,            0,        0,0 ,IsText
.dw Text25,    0,         0,            0,        0,0 ,IsText
.dw Text26,    0,         0,            0,        0,0 ,IsText
.dw Text27,    0,         0,            0,        0,0 ,IsText
.dw Text28,    0,         0,            0,        0,0 ,IsText
.dw Text29,    0,         0,            0,        0,0 ,IsText
.dw Text30,    0,         0,            0,        0,0 ,IsText
.dw Text31,    0,         0,            0,        0,0 ,IsText


EndDrawData: .dw $ffff

Text1:
;    12345678901234567890123456789012
.DB "      BOCK'S BIRTHDAY 2002",10
.DB "      HAPPY BIRTHDAY BOCK!",0
Text2:
;    12345678901234567890123456789012
.DB "  A.K.A. SMS POWER RUSH CODING",10
.DB "         COMPETITION #2",0
Text3:
;    12345678901234567890123456789012
.DB "  \"WHAT? IT'S BOCK'S BIRTHDAY?",10
.DB "   AGAIN? OH HELL, I FORGOT\"",0
Text4:
;    12345678901234567890123456789012
.DB " ANYWAY, HERE IS THE LIFE STORY",10
.DB "OF MR. BOCK LEE TEMJIM OMAR ZOOP",10
.DB "     SEGA COOL CORNUT-KITTY",0
Text5:
;    12345678901234567890123456789012
.DB " ON 25TH NOVEMBER 1980 BOCK WAS",10
.DB "  BORN, BUT WITH THE NAME...",0
; OMAR CORNUT IN MIRACLE WORLD
Text6:
;    12345678901234567890123456789012
.DB "     THEN IN 1988 OR SO HE",10
.DB "  DISCOVERED COMPUTER GAMES...",0
; BOCK PANIC
; FANTASY ZOOP
; OMAR HE HEDGEHOG
Text7:
;    12345678901234567890123456789012
.DB "      HE SOON BECAME A...",0
; SMS STAR
Text8:
;    12345678901234567890123456789012
.DB "HE WENT ON TO WRITE THE WORLD'S",10
.DB " BEST MASTER SYSTEM EMULATOR...",0
; MEKA
Text9:
;    12345678901234567890123456789012
.DB "  BUT WORK IS NEVER FINISHED...",0
Text10:
;    12345678901234567890123456789012
.DB "         SEGA GAME 1000",10
.DB "       SEGA COMPUTER 3000",0
Text11:
;    12345678901234567890123456789012
.DB "       SEGA MASTER SYSTEM",10
.DB "         SEGA GAME GEAR",0
Text12:
;    12345678901234567890123456789012
.DB " SUPER CONTROL STATION SF-7000",10
.DB "          COLECOVISION",0
Text13:
;    12345678901234567890123456789012
.DB "             (NES)",0
Text14:
;    12345678901234567890123456789012
.DB " WITH ALL THESE SUPPORTED, MEKA",10
.DB "HAD BECOME A MONSTER OF AN EMU..",0
; MEKA WORLD 2
Text15:
;    12345678901234567890123456789012
.DB "NOT CONTENT WITH THAT, HE ALSO",10
.DB "RUNS THE WORLD'S BEST EMULATION",10
.DB "      WEBSITE (WITH ROMS!)",0
; SMS POWER
Text16:
;    12345678901234567890123456789012
.DB " I'LL SEE YOU ALL ON THE FORUMS",10
.DB "  WHY NOT LISTEN TO SOME VGMS?",0
Text17:
;    12345678901234567890123456789012
.DB " ANYWAY, THAT JUST ABOUT BRINGS",10
.DB "   US UP TO THE PRESENT DAY...",10
.DB "         ...WHICH IS:",0
; 2002-11-25
Text18:
;    12345678901234567890123456789012
.DB "  AND THEN THERE'S THE FUTURE.",10
.DB "   DON'T FORGET THAT IN 2006",0
Text19:
;    12345678901234567890123456789012
.DB "WE'RE ALL GOING TO HAVE A PARTY",10
.DB "  TO CELEBRATE THE OPENING OF",0
Text20:
;    12345678901234567890123456789012
.DB "         THE SMS MUSEUM",0
Text21:
;    12345678901234567890123456789012
.DB "SO WE'RE ALL GOING TO MEET UP AT",0
; BOCK'S HOUSE
Text22:
;    12345678901234567890123456789012
.DB "AND SO CONCLUDES OUR STORY. IT'S",10
.DB " NOT VERY LONG BUT TOOK AGES TO",10
.DB "   WRITE/RIP/CONVERT/DEBUG...",0
Text23:
;    12345678901234567890123456789012
.DB "AND IF ANYONE COMPLAINS ABOUT IT",10
.DB "     NOT HAVING ANY MUSIC...",0
Text24:
;    12345678901234567890123456789012
.DB "  I'LL COME AND KICK YOUR ASS!",10
.DB "      (EITHER THAT, OR...",0
Text25:
;    12345678901234567890123456789012
.DB "   ...MAKE A SCATHING COMMENT",10
.DB "          ON THE FORUM.)",0
Text26:
;    12345678901234567890123456789012
.DB "  ANYWAY, I'M JUST ABOUT DONE.",10
.DB "      SO, ONE LAST TIME:",0
Text27:
;    12345678901234567890123456789012
.DB "             HAPPY",10,10
.DB "            BIRTHDAY",10,10
.DB "             BOCK!",0
Text28:.DB 0
Text29:.DB 0
Text30:.DB 0
Text31:
;    12345678901234567890123456789012
.DB "    AND NOW WE WILL REPEAT...",0



.bank 1 slot 2
.org 0
Tiles1:
.include "kenseiden.inc"
TileNums1:
.include "kenseiden (tile numbers).inc"
Palette1:
.db clRGB000,clRGB100,clRGB133,clRGB210,clRGB300,clRGB303,clRGB320,clRGB330,clRGB333
Tiles5:
.include "date.inc"
TileNums5:
.include "date (tile numbers).inc"
Palette5:
.db clRGB000,clRGB112,clRGB122,clRGB330,clRGB333

.bank 2
.org 0
Tiles2:
.include "bp.inc"
TileNums2:
.include "bp (tile numbers).inc"
Palette2:
.db clRGB000,clRGB100,clRGB210,clRGB300,clRGB310,clRGB320,clRGB330,clRGB222,clRGB333
Tiles6:
.include "fz2.inc"
TileNums6:
.include "fz2 (tile numbers).inc"
Palette6:
.db clRGB000,clRGB013,clRGB020,clRGB030,clRGB023,clRGB033,clRGB303,clRGB313,clRGB221,clRGB320,clRGB330,clRGB323,clRGB333

.bank 3
.org 0
Tiles3:
.include "akmw.inc"
TileNums3:
.include "akmw (tile numbers).inc"
Palette3:
.db clRGB000,clRGB010,clRGB110,clRGB003,clRGB020,clRGB030,clRGB023,clRGB033,clRGB200,clRGB210,clRGB300,clRGB320,clRGB330,clRGB223,clRGB332,clRGB333
Tiles4:
.include "bh.inc"
TileNums4:
.include "bh (tile numbers).inc"
Palette4:
.db clRGB000,clRGB100,clRGB111,clRGB200,clRGB210,clRGB333

.bank 4
.org 0
Tiles7:
.include "sonic.inc"
TileNums7:
.include "sonic (tile numbers).inc"
Palette7:
.db clRGB000,clRGB001,clRGB100,clRGB013,clRGB023,clRGB133,clRGB210,clRGB300,clRGB320,clRGB321,clRGB330,clRGB233,clRGB332,clRGB333
Tiles8:
.include "PS.inc"
TileNums8:
.include "PS (tile numbers).inc"
Palette8:
.db clRGB000,clRGB100,clRGB013,clRGB112,clRGB033,clRGB210,clRGB312,clRGB320,clRGB330,clRGB222,clRGB322,clRGB323,clRGB332,clRGB333

.bank 0
.org 0
.section "blah" SEMIFREE
Tiles9:
.include "pp.inc"
TileNums9:
.include "pp (tile numbers).inc"
Palette9:
.db clRGB000,clRGB010,clRGB100,clRGB013,clRGB020,clRGB030,clRGB023,clRGB032,clRGB033,clRGB200,clRGB300,clRGB320,clRGB222,clRGB333
Tiles10:
.include "wb3.inc"
TileNums10:
.include "wb3 (tile numbers).inc"
Palette10:
.db clRGB000,clRGB001,clRGB011,clRGB002,clRGB003,clRGB013,clRGB021,clRGB023,clRGB133,clRGB310,clRGB303,clRGB320,clRGB330,clRGB331,clRGB332,clRGB333
.ends
