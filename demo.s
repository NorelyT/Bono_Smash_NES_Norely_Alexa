.segment "HEADER"
  ;.byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "ZEROPAGE"
  x_tile: .res 1
  y_tile: .res 1
  facing: .res 1
  on_ground: .res 1
  stored_variable: .res 1
  player_decider: .res 1
  
  ;player 1
  controller1: .res 1
  x_pos_player1: .res 1
  y_pos_player1: .res 1
  direction_player1: .res 1
  sprite_action_player1: .res 1
  take_damage_player1: .res 1
  counter_player1: .res 1
  jump_frames_player1: .res 1
  walking_step_player1: .res 1
  lives_player1: .res 1 ;16

  ;player 2
  controller2: .res 1
  x_pos_player2: .res 1
  y_pos_player2: .res 1
  direction_player2: .res 1
  sprite_action_player2: .res 1
  take_damage_player2: .res 1 ;22
  counter_player2: .res 1
  jump_frames_player2: .res 1
  walking_step_player2: .res 1
  lives_player2: .res 1 ;26


; Main code segment for the program
.segment "CODE"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx $2000	; disable NMI
  stx $2001 	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit $2002
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory

;; second wait for vblank, PPU is ready after this
vblankwait2:
  bit $2002
  bpl vblankwait2


main:
load_palettes:
  lda $2002 ;reads from the CPU-RAM PPU address register to reset it
  lda #$3f  ;loads the higher byte of the PPU address register of the palettes in a (we want to write in $3f00 of the PPU since it is the address where the palettes of the PPU are stored)
  sta $2006 ;store what's in a (higher byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  lda #$00  ;loads the lower byte of the PPU address register in a
  sta $2006 ;store what's in a (lower byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  ldx #$00  ;AFTER THIS, THE PPU-RAM GRAPHICS POINTER WILL BE POINTING TO THE MEMORY LOCATION THAT CONTAINS THE SPRITES, NOW WE NEED TO TRANSFER SPRITES FROM THE CPU-ROM TO THE PPU-RAM
            ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT

; NO NEED TO MODIFY THIS LOOP SUBROUTINE, IT ALWAYS LOADS THE SAME AMOUNT OF PALETTE REGISTER. TO MODIFY PALETTES, REFER TO THE PALETTE SECTION
@loop: 
  lda palettes, x   ; as x starts at zero, it starts loading in a the first element in the palettes code section ($0f). This address mode allows us to copy elements from a tag with .data directives and the index in x
  sta $2007         ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT
  inx
  cpx #$20
  bne @loop


; displaySprites:
;   ldx #$00
; displaySpritesLoop:
;   lda sprites, x 	; oLad the hello message into SPR-RAM one by one, the pointer is increased every time a byte is written. Sprites are referenced by using the third byte of the 4-byte arrays in "hello"
;   sta $0200, x
;   inx
;   cpx #$48           ;ATTENTION: if you add more letters, you must increase this number by 4 per each additional letter. This is the limit for the sprite memory copy routine
;   bne displaySpritesLoop

constants:
  button_A = %10000000
  button_B = %01000000
  button_Start = %00010000
  button_Select = %00100000
  button_Up = %00001000
  button_down = %00000100
  button_left = %00000010
  button_right = %00000001
  

;loads player1 at top of screen
lda #$20
sta x_pos_player1
lda #$20
sta y_pos_player1
lda #$01
sta direction_player1
lda #$00
sta take_damage_player1
lda #$03
sta sprite_action_player1

;loads player2 at top of screen
lda #$C8
sta x_pos_player2
lda #$20
sta y_pos_player2
lda #$00
sta direction_player2
lda #$00
sta take_damage_player2
lda #$03
sta sprite_action_player2


;loads 5 lives for player 1
ldx #$00
ldy #$00
lda #$01
sta facing
load_player1_lives:
  lda #$08
  sta $0238, y
  iny
  lda #$0E
  sta $0238, y
  iny
  lda facing
  sta $0238, y
  iny
  lda player_lives_x_positions, x
  sta $0238, y
  inx
  iny
  cpx #$05
  bne load_player1_lives

lda #$05
sta lives_player1

;loads 5 lives for player 2
  lda #$00
  sta facing
  load_player2_lives:
  lda #$0F
  sta $0238, y
  iny
  lda #$0E
  sta $0238, y
  iny
  lda facing
  sta $0238, y
  iny
  lda player_lives_x_positions, x
  sta $0238, y
  inx
  iny
  cpx #$0A
  bne load_player2_lives

lda #$05
sta lives_player2

lda #%00000000
sta on_ground 


LoadBackground:
    LDA $2002
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006
    ldx #$00

Loop:
    lda background, x ; Loop que va a ayudar a cargar el fondo de pantalla
    sta $2007
    inx
    cpx #$00
    bne Loop
    ldx $00
Loop02:
    lda background02, x
    sta $2007
    inx
    cpx #$00
    bne Loop02
    ldx $00
Loop03:
    lda background03, x 
    sta $2007
    inx
    cpx #$00
    bne Loop03
    ldx $00
Loop04:
    lda background04, x 
    sta $2007
    inx
    cpx #$00
    bne Loop04


    
enable_rendering: ; DO NOT MODIFY THIS
  lda #%10010000	; Enable NMI
  sta $2000
  lda #%00011110	; Enable Sprites
  sta $2001

forever: ;FOREVER LOOP WAITING FOR THEN NMI INTERRUPT, WHICH OCCURS WHENEVER THE LAST PIXEL IN THE BOTTOM RIGHT CORNER IS PROJECTED
  ; use it to animate sprites statically
  jmp forever

; nmi:  ;WHENEVER AN NMI INTERRUPT OCCURS, THE PROGRAM JUMPS HERE (60fps)
;   ldx #$00 	; Set SPR-RAM address to 0
;   stx $2003 ;Sets the PPU-RAM pointer to $2003 to start receiving sprite information saved under the tag "hello"B



nmi:
  lda #$00
  sta $2003
  lda #$02
  sta $4014
  lda #$00
  sta $2005



  jsr readcontrollers ;reads controllers

; Checks if Gameover state needs to be applied and checks controller inputs if not
  lda #$00
  cmp lives_player1 
  beq player1_gameover
  jsr manageinput1
  jmp continue_nmi1
  player1_gameover:
    jsr set_gameover_state
    lda #$04
    sta sprite_action_player1

  continue_nmi1:

  lda #$00
  cmp lives_player2
  beq player2_gameover
  jsr manageinput2
  jmp continue_nmi2
  player2_gameover:
    jsr set_gameover_state
    lda #$04
    sta sprite_action_player2

  continue_nmi2:

  jsr checkIfOnPlatform ;checks if player1 on platform
  jsr checkIfOnGround ;checks if player1 on ground
  jsr checkIfOnPlatform2 ;checks if player2 on platform
  jsr checkIfOnGround2 ;checks if player2 on ground
  jsr gravity ;applies gravity to player1
  jsr gravity2 ;applies gravity to player2
  lda #$00
  sta player_decider
  jsr action_decider ;displays the sprite for each player

rti


counter_section:
  inc counter_player1
  ldx #$15
  cpx counter_player1
  bne endCounterSection
  lda #$00
  sta counter_player1
  ldx walking_step_player1
  inx
  stx walking_step_player1
  cpx #$04
  bne endCounterSection
  lda #$00
  sta walking_step_player1
  endCounterSection:
  rts

counter_section2:
  inc counter_player2
  ldx #$15
  cpx counter_player2
  bne endCounterSection2
  lda #$00
  sta counter_player2
  ldx walking_step_player2
  inx
  stx walking_step_player2
  cpx #$04
  bne endCounterSection2
  lda #$00
  sta walking_step_player2
  endCounterSection2:
  rts

walking_animation:
  ldx walking_step_player1
  cpx #$00
  beq walk1
  cpx #$01
  beq stand
  cpx #$02
  beq walk2
  jmp stand

  walk1:
  lda #$01
  sta sprite_action_player1
  rts

  walk2:
  lda #$02
  sta sprite_action_player1
  rts

  stand:
  lda #$00
  sta sprite_action_player1
  rts

  walking_animation2:
  ldx walking_step_player2
  cpx #$00
  beq walk12
  cpx #$01
  beq stand2
  cpx #$02
  beq walk22
  jmp stand2


  walk12:
  lda #$01
  sta sprite_action_player2
  rts

  walk22:
  lda #$02
  sta sprite_action_player2
  rts

  stand2:
  lda #$00
  sta sprite_action_player2
  rts


gravity:
  lda on_ground
  ;lda on_ground_player1
  and #%00000001
  ;cmp #$01
  bne noGravity
  lda y_pos_player1
  clc
  adc #$03
  sta y_pos_player1
  noGravity:
    rts


gravity2:
  lda on_ground
  ;lda on_ground_player2
  and #%00000010
  ;cmp #$01
  bne noGravity2
  lda y_pos_player2
  clc
  adc #$03
  sta y_pos_player2
  noGravity2:
    rts


checkIfOnPlatform:
  lda x_pos_player1
  cmp #$38
  bmi notOnPlatform
  cmp #$C0
  bpl notOnPlatform
  lda y_pos_player1
  cmp #$AB
  bpl notOnPlatform ; branch if the register is larger than the one i'm comparing it to
  cmp #$A8
  bmi notOnPlatform ; branch if the register is smaller than the one i'm comparing it to
  lda on_ground
  ora #%00000001
  sta on_ground
  lda #$00
  sta jump_frames_player1
  jmp endPlatformCheck
  notOnPlatform:
    lda on_ground
    and #%11111110
    sta on_ground
  endPlatformCheck:
  rts

checkIfOnPlatform2:
  lda x_pos_player2
  cmp #$38
  bmi notOnPlatform2
  cmp #$C0
  bpl notOnPlatform2
  lda y_pos_player2
  cmp #$AB
  bpl notOnPlatform2 ; branch if the register is larger than the one i'm comparing it to
  cmp #$A8
  bmi notOnPlatform2 ; branch if the register is smaller than the one i'm comparing it to
  lda on_ground
  ora #%00000010
  sta on_ground
  lda #$00
  sta jump_frames_player2
  jmp endPlatformCheck2
  notOnPlatform2:
    lda on_ground
    and #%11111101
    sta on_ground
  endPlatformCheck2:
  rts


checkIfOnGround:
  lda y_pos_player1
  cmp #$D3
  bpl notOnGround
  cmp #$D0
  bmi notOnGround
  lda on_ground
  ora #%00000001
  sta on_ground
  lda #$00
  sta jump_frames_player1
  notOnGround:
  lda on_ground
  and #%00000001
  bne endGroundCheck
  lda #$03
  sta sprite_action_player1
  endGroundCheck:
  rts


checkIfOnGround2:
  lda y_pos_player2
  cmp #$D3
  bpl notOnGround2
  cmp #$D0
  bmi notOnGround2
  lda on_ground
  ora #%00000010
  sta on_ground
  lda #$00
  sta jump_frames_player2
  notOnGround2:
  lda on_ground
  and #%00000010
  bne endGroundCheck2
  lda #$03
  sta sprite_action_player2
  endGroundCheck2:
  rts


jump:
  lda y_pos_player1
  clc
  sbc #$06
  sta y_pos_player1
  inc jump_frames_player1
  rts

jump2:
  lda y_pos_player2
  clc
  sbc #$06
  sta y_pos_player2
  inc jump_frames_player2
  rts



readcontrollers:
  lda #1
  sta controller1

  sta $4016
  lda #0
  sta $4016

  read_loop:
    lda $4017
    lsr a
    rol controller2

    lda $4016
    lsr a
    rol controller1
    bcc read_loop
rts


manageinput1:
  lda #$00
  sta player_decider

  lda #$01
  cmp take_damage_player2
  bne continue_manageinput1
  jsr damage_counter

  continue_manageinput1:

  lda controller1
  cmp #$00
  beq noinput

  lda controller1
  and #button_right
  cmp #button_right
  bne notRight
  ;if right pressed
  jsr counter_section
  jsr walking_animation
  lda #01
  sta direction_player1
  lda x_pos_player1
  clc
  adc #$03
  tax
  cpx #$f3
  bcs notRight
  sta x_pos_player1

  notRight:

  lda controller1
  and #button_left
  cmp #button_left
  bne notLeft
  ;if left pressed
  jsr counter_section
  jsr walking_animation
  lda #00
  sta direction_player1
  lda x_pos_player1
  clc
  sec
  sbc #$03
  bcc notLeft
  sta x_pos_player1

  notLeft:

  lda controller1
  and #button_A
  cmp #button_A
  bne notA
  ; if A pressed
  lda jump_frames_player1
  cmp #$10
  bpl notA
  jsr jump
  jmp endAcheck
  notA:
    lda #$11
    sta jump_frames_player1
  endAcheck:

  lda controller1
  and #button_Select
  cmp #button_Select
  bne notSelect
  ;if select pressed
  ; lda #$01
  ; sta take_damage_player1
  ; lda #$04
  ; sta sprite_action_player1  

  notSelect:

  lda controller1
  and #button_Start
  cmp #button_Start
  bne notStart
  ; start_pressed:
  jmp reset

  notStart:

  lda controller1
  and #button_B
  cmp #button_B
  bne notB
  ;if B pressed
  lda #$05
  sta sprite_action_player1  
  jsr attack_counter 

  notB:

  rts
  noinput:
  lda #$00
  sta sprite_action_player1
  ;if no button is pressed

rts


;control 2
manageinput2:
  lda #$01
  sta player_decider

  lda #$01
  cmp take_damage_player1
  bne continue_manageinput2
  jsr damage_counter2

  continue_manageinput2:

  lda controller2
  cmp #$00
  beq noinput2
  lda controller2
  and #button_right
  cmp #button_right
  bne notRight2

  ;if right pressed
  jsr counter_section2
  jsr walking_animation2
  lda #01
  sta direction_player2
  lda x_pos_player2
  clc
  adc #$03
  tax
  cpx #$f3
  bcs notRight2
  sta x_pos_player2

  notRight2:

  lda controller2
  and #button_left
  cmp #button_left
  bne notLeft2
  ;if left pressed
  jsr counter_section2
  jsr walking_animation2
  lda #00
  sta direction_player2
  lda x_pos_player2
  clc
  sec
  sbc #$03
  bcc notLeft2
  sta x_pos_player2

  notLeft2:

  lda controller2
  and #button_A
  cmp #button_A
  bne notA2
  ; if A pressed
  lda jump_frames_player2
  cmp #$10
  bpl notA2
  jsr jump2
  jmp endAcheck2
  notA2:
    lda #$11
    sta jump_frames_player2
  endAcheck2:

  lda controller2
  and #button_Select
  cmp #button_Select
  bne notSelect2
  ;if select pressed
  ; lda #$01
  ; sta take_damage_player2
  ; lda #$04
  ; sta sprite_action_player2

  notSelect2:

  lda controller2
  and #button_Start
  cmp #button_Start
  bne notStart2
  ;if start pressed
  jmp reset

  notStart2:

  lda controller2
  and #button_B
  cmp #button_B
  bne notB2
  ;if B pressed
  lda #$05
  sta sprite_action_player2
  jsr attack_counter2

  notB2:

  rts

  noinput2:
  lda #$00
  sta sprite_action_player2
  ;if no button is pressed

rts


damage_counter:
  inc counter_player1
  ldx #$05
  cpx counter_player1
  bne endDamageCounterSection
  lda #$00
  sta counter_player1
  lda #$00
  sta take_damage_player2
  endDamageCounterSection:
  rts


damage_counter2:
  inc counter_player2
  ldx #$05
  cpx counter_player2
  bne endDamageCounterSection2
  lda #$00
  sta counter_player2
  lda #$00
  sta take_damage_player1
  endDamageCounterSection2:
  rts


attack_counter:
  inc counter_player1
  ldx #$0C
  cpx counter_player1
  bpl endAttackCounterSection
  lda #$00
  sta counter_player1
  jsr check_if_hitbox_collision
  endAttackCounterSection:
  rts


attack_counter2:
  inc counter_player2
  ldx #$0C
  cpx counter_player2
  bpl endAttackCounterSection2
  lda #$00
  sta counter_player2
  jsr check_if_hitbox_collision
  endAttackCounterSection2:
  rts


action_decider:
  jsr check_which_player_x_pos
  sta x_tile
  jsr check_which_player_y_pos
  sta y_tile
  jsr check_which_player_sprite_action
  cmp #$00
  beq display_base_kitty
  cmp #$01
  beq display_walking_kitty1
  cmp #$02
  beq display_walking_kitty2
  cmp #$03
  beq display_jumping_kitty
  cmp #$04
  beq display_gameover_kitty
  cmp #$05
  beq display_attack_kitty


display_base_kitty:
  ldx #$00
  jmp display_single_kitty

display_walking_kitty1:
  ldx #$08
  jmp display_single_kitty

display_walking_kitty2:
  ldx #$10
  jmp display_single_kitty
 
display_jumping_kitty:
  ldx #$18
  jmp display_single_kitty 

display_gameover_kitty:
  ldx #$20
  jmp display_single_kitty
  
display_attack_kitty:
  ldx #$28
  jmp display_single_kitty


display_single_kitty:
  jsr check_which_player_direction
  jsr direction_decider

  ldy #$00
  
set_sprite_tileattr_loop:
  iny
  lda sprites, x
  sta stored_variable
  jsr store_player_memory_location
  inx
  iny
  lda facing
  sta stored_variable
  jsr store_player_memory_location
  iny
  iny
  cpy #$10
  bne set_sprite_tileattr_loop

  ldy #$00

set_sprite_xy_loop:

  cpy #$00
  beq load_tiles
  jsr not_first_tile

  load_tiles:
    lda y_tile
    sta stored_variable
    jsr store_player_memory_location
    iny
    iny
    iny
    lda x_tile
    sta stored_variable
    jsr store_player_memory_location
    iny ; x = 4 ; new line of bits
    ;do a clear carry before???
    cpy #$10 ; 4 8 12 16
    bne set_sprite_xy_loop

  jsr check_which_player_sprite_action
  cmp #$05
  jsr display_scratch_sprites
  jsr delete_scratch_sprites

  lda player_decider
  cmp #$00
  beq load_second_player
  bne end_sprite_display
  load_second_player:
    lda #$01
    sta player_decider
    jmp action_decider
  end_sprite_display:
    lda #$00
    sta player_decider
  rts



direction_decider:
  cpy #$00
  beq left
  bne right

  left:
    ldy #$00
    jsr check_if_player2_palette
    jsr check_if_damage_taken
    cmp #$00
    beq store_left_attribute
    ldy #$02
    store_left_attribute:
      sty facing
      rts

  right:
    ldy #$40
    jsr check_if_player2_palette
    jsr check_if_damage_taken
    cmp #$00
    beq store_right_attribute
    ldy #$42
    store_right_attribute:
      sty facing

      txa
      clc
      adc #$04
      tax
      rts
  


not_first_tile:
  cpy #$04
  beq right_tiles

  cpy #$0C
  beq right_tiles

  cpy #$08
  beq lower_tiles

  right_tiles:
    lda x_tile
    clc
    adc #$08
    sta x_tile
    rts

  lower_tiles:
    lda y_tile
    clc

    adc #$08
    sta y_tile

    lda x_tile
    sec
    sbc #$08
    sta x_tile
    rts



display_scratch_sprites: 
  beq scratch
  rts
  scratch:
    lda facing
    cmp #$40
    bmi left_scratch
    bpl flip_scratch
    left_scratch:
      lda x_tile
      sec
      sbc #$10
      sta x_tile
      jmp load_scratch_bytes
    flip_scratch:
      lda x_tile
      clc
      adc #$08
      sta x_tile
      jmp load_scratch_bytes
    load_scratch_bytes:
    ldx #$30
    lda y_tile
    sec
    sbc #$08
    sta stored_variable
    jsr store_player_memory_location
    iny
    lda sprites, x
    sta stored_variable
    jsr store_player_memory_location
    iny
    inx
    lda facing
    sta stored_variable
    jsr store_player_memory_location
    iny
    lda x_tile
    sta stored_variable
    jsr store_player_memory_location
    iny

    lda y_tile
    sta stored_variable
    jsr store_player_memory_location
    iny
    lda sprites, x
    sta stored_variable
    jsr store_player_memory_location
    iny
    inx
    lda facing
    sta stored_variable
    jsr store_player_memory_location
    iny
    lda x_tile
    sta stored_variable
    jsr store_player_memory_location
    iny
  rts


delete_scratch_sprites:
  bne delete_scratch
  rts    
  delete_scratch:
    lda #$00
    iny
    sta stored_variable
    jsr store_player_memory_location
    iny
    iny
    iny
    iny
    sta stored_variable
    jsr store_player_memory_location
  rts



check_which_player_x_pos:
  lda player_decider
  cmp #$00
  beq player1_x_pos
  bne player2_x_pos
  player1_x_pos:
    lda x_pos_player1
    rts
  player2_x_pos:
    lda x_pos_player2
    rts    


check_which_player_y_pos:
  lda player_decider
  cmp #$00
  beq player1_y_pos
  bne player2_y_pos
  player1_y_pos:
    lda y_pos_player1
    rts
  player2_y_pos:
    lda y_pos_player2
    rts
  

check_which_player_sprite_action:
  lda player_decider
  cmp #$00
  beq player1_sprite_action
  bne player2_sprite_action
  player1_sprite_action:
    lda sprite_action_player1
    rts
  player2_sprite_action:
    lda sprite_action_player2
    rts


check_which_player_direction:
  lda player_decider
  cmp #$00
  beq player1_direction
  bne player2_direction
  player1_direction:
    ldy direction_player1
    rts
  player2_direction:
    ldy direction_player2
    rts


check_if_player2_palette:
  lda player_decider
  cmp #$01
  bne player2_palette
  rts
  player2_palette:
    iny
    rts


store_player_memory_location:
  lda player_decider
  cmp #$00
  beq player1_memory_location
  bne player2_memory_location
  player1_memory_location:
    lda stored_variable
    sta $0200, y
    rts
  player2_memory_location:
    lda stored_variable
    sta $0218, y
    rts


check_if_damage_taken:
  lda player_decider
  cmp #$00
  beq player1_damage_taken
  bne player2_damage_taken
  player1_damage_taken:
    lda take_damage_player1
    rts
  player2_damage_taken:
    lda take_damage_player2
    rts


take_damage:
  lda player_decider
  cmp #$00
  beq player1_take_damage
  bne player2_take_damage
  player1_take_damage:
    lda #$01
    sta take_damage_player1
    rts
  player2_take_damage:
    lda #$01
    sta take_damage_player2
    rts


switch_players:
  lda #$00
  cmp player_decider
  beq switch_to_second_player
  bne switch_to_first_player
  switch_to_first_player:
    dec player_decider
    rts
  switch_to_second_player:
    inc player_decider
    rts


check_if_hitbox_collision:
  jsr check_which_player_direction
  cpy #$00
  beq left_side_scratch
  bne right_side_scratch

  left_side_scratch:
    jsr check_which_player_x_pos
    tay
    sec
    sbc #$08
    tax
    jmp check_if_scratch_within_player

  right_side_scratch:
    jsr check_which_player_x_pos
    clc
    adc #$10
    tax
    clc
    adc #$08
    tay

  check_if_scratch_within_player:
    jsr switch_players

    check_if_left_side_scratch_within_player:
      jsr check_which_player_x_pos
      sta x_tile ;here left side of player2
      cpx x_tile
      bmi check_if_right_side_scratch_within_player
      clc
      adc #$10
      sta x_tile ;here right side of player2
      cpx x_tile
      bpl check_if_right_side_scratch_within_player ; branch if the register is larger than the one i'm comparing it to      
      jmp height_scratch

    check_if_right_side_scratch_within_player:
      jsr check_which_player_x_pos
      sta x_tile ;here left side of player2
      cpy x_tile
      bmi no_hitbox_collision
      clc
      adc #$10
      sta x_tile ;here right side of player2
      cpy x_tile
      bpl no_hitbox_collision ; branch if the register is larger than the one i'm comparing it to      

    height_scratch:
      jsr switch_players
      jsr check_which_player_y_pos
      tay
      clc
      adc #$10
      tax

      jsr switch_players

    check_if_bottom_side_scratch_within_player:
      jsr check_which_player_y_pos
      sta y_tile ;here left side of player2
      cpx y_tile
      bmi check_if_top_side_scratch_within_player
      clc
      adc #$10
      sta y_tile ;here right side of player2
      cpx y_tile
      bpl check_if_top_side_scratch_within_player ; branch if the register is larger than the one i'm comparing it to      
      jmp hitbox_collision

    check_if_top_side_scratch_within_player:
      jsr check_which_player_y_pos
      sta y_tile ;here left side of player2
      cpy y_tile
      bmi no_hitbox_collision
      clc
      adc #$10
      sta y_tile ;here right side of player2
      cpy y_tile
      bpl no_hitbox_collision ; branch if the register is larger than the one i'm comparing it to      
      jmp hitbox_collision

  hitbox_collision:
    jsr take_damage
    jsr delete_player_lives
    rts

  no_hitbox_collision:
    rts


check_which_player_lives:
  lda player_decider
  cmp #$00
  beq player1_lives
  bne player2_lives
  player1_lives:
    lda lives_player1
    ldy #$49
    rts
  player2_lives:
    lda lives_player2
    ldy #$45
    rts
  

delete_player_lives:
  jsr check_which_player_lives
  cmp #$05
  beq lives_5_left
  cmp #$04
  beq lives_4_left
  cmp #$03
  beq lives_3_left
  cmp #$02
  beq lives_2_left
  cmp #$01
  beq lives_1_left
  cmp #$00
  beq lives_0_left
    
    lives_5_left:
      tya
      sec
      sbc #$00
      jmp delete_player_lives_in_memory
    lives_4_left:
      tya
      sec
      sbc #$04
      jmp delete_player_lives_in_memory
    lives_3_left:
      tya
      sec
      sbc #$08
      jmp delete_player_lives_in_memory
    lives_2_left:
      tya
      sec
      sbc #$0C
      jmp delete_player_lives_in_memory
    lives_1_left:
      tya
      sec
      sbc #$10
      jmp delete_player_lives_in_memory
    lives_0_left:
      jmp end_delete_player_lives


  delete_player_lives_in_memory:
    tay
    lda #$00
    sta stored_variable
    jsr store_player_memory_location
    jsr decrement_which_player_lives
    end_delete_player_lives:
  rts


decrement_which_player_lives:
  lda player_decider
  cmp #$00
  beq player1_decrement_lives
  bne player2_decrement_lives
  player1_decrement_lives:
    dec lives_player1
    lda lives_player1
    rts
  player2_decrement_lives:
    dec lives_player2
    lda lives_player2
    rts


store_which_player_sprite_action:
  lda player_decider
  cmp #$00
  beq player1_store_sprite
  bne player2_store_sprite
  player1_store_sprite:
    sta sprite_action_player1
    rts
  player2_store_sprite:
    sta sprite_action_player2
    rts


Loop_GameOver:
  lda sprites_GameOver, x
  sta $0260, x
  inx
  cpx #$20
  bne Loop_GameOver
  rts


set_gameover_state:
  ldx #$00
  jsr Loop_GameOver
  rts


palettes:
;Background Palettes
  .byte $31, $27, $17, $38
  .byte $31, $37, $27, $17
  .byte $31, $0F, $19, $29
  .byte $31, $28, $19, $29

;Player Sprite Palettes
  .byte $31, $03, $3D, $36
  .byte $31, $0f, $28, $35 ;light-blue (transparent), black, golden-yellow, light-pink
  .byte $31, $06, $37, $35 ;light-blue (transparent), dark-red, golden-yellow, light-pink
  .byte $31, $0f, $21, $30


background:
	.byte $00,$00,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$35,$37,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$25,$36,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
   
  background02:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
	.byte $06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09
	.byte $0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

  background03:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07
	.byte $08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09
	.byte $0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$05,$09,$18,$26,$26,$26,$26,$26,$26,$26
	.byte $16,$16,$16,$16,$16,$16,$16,$17,$0a,$06,$00,$00,$00,$00,$00,$00

  background04:
	.byte $00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00
	.byte $24,$21,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$22,$13
	.byte $23,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$23
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
	.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
	.byte $fb,$3b,$00,$00,$00,$00,$00,$00,$8f,$0b,$02,$80,$00,$0a,$00,$00
	.byte $00,$08,$a0,$00,$80,$a0,$a0,$a3,$08,$0a,$00,$00,$08,$0a,$0a,$02
	.byte $aa,$aa,$00,$00,$00,$00,$00,$00,$02,$26,$00,$00,$00,$00,$00,$00
	.byte $20,$00,$00,$00,$00,$00,$00,$80,$05,$05,$05,$05,$05,$05,$05,$05




sprites:
  .byte $01 ;left base kitty
  .byte $02
  .byte $11
  .byte $12

  ;flip is $40

  .byte $02 ;right base kitty
  .byte $01
  .byte $12
  .byte $11

  .byte $03 ;left walking kitty 1
  .byte $04
  .byte $13
  .byte $14

  .byte $04 ;right walking kitty 1
  .byte $03
  .byte $14
  .byte $13

  .byte $03 ;left walking kitty 2 (uses the 'just bottom half')
  .byte $04
  .byte $15
  .byte $16

  .byte $04 ;right walking kitty 2 (uses the 'just bottom half')
  .byte $03
  .byte $16
  .byte $15

  .byte $07 ;left jumping kitty
  .byte $08
  .byte $17
  .byte $18

  .byte $08 ;right jumping kitty
  .byte $07
  .byte $18
  .byte $17

  .byte $09 ;left game-over kitty
  .byte $0A
  .byte $19
  .byte $1A

  .byte $0A ;right game-over kitty
  .byte $09
  .byte $1A
  .byte $19

  .byte $0C ;left attack kitty
  .byte $0D
  .byte $1C
  .byte $1D

  .byte $0D ;right attack kitty
  .byte $0C
  .byte $1D
  .byte $1C

  .byte $0B ;left scratch
  .byte $1B

  .byte $0E ;life orb


player_lives_x_positions:
  .byte $02
  .byte $0C
  .byte $16
  .byte $20
  .byte $2A

  .byte $F6
  .byte $EC
  .byte $E2
  .byte $D8
  .byte $CE


sprites_GameOver: 
  ;       Y   T    A    X         Y = y position; T = tile; A = direction and color; X = x position
  .byte $48, $F0, $00, $60 ; G
  .byte $48, $F1, $00, $68 ; A
  .byte $48, $F2, $00, $70 ; M
  .byte $48, $F3, $00, $78 ; E

  .byte $48, $F4, $00, $80 ; O
  .byte $48, $F5, $00, $88 ; V
  .byte $48, $F3, $00, $90 ; E
  .byte $48, $F6, $00, $98 ; R

  ; 72 y, 96 x


; Character memory
.segment "CHARS"
.incbin "background_and_kittiesSprites2.chr"