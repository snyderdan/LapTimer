CON

  _CLKMODE = XTAL1 + PLL16X
  _CLKFREQ = 80_000_000
  D1_SENSOR = 1 << 7
  D2_SENSOR = 1 << 5
  OE_PIN = 1 << 10
  LATCH_PIN = 1 << 0
  R1_PIN = 1 << 22
  G1_PIN = 1 << 23
  B1_PIN = 1 << 24
  R2_PIN = 1 << 18
  G2_PIN = 1 << 19
  B2_PIN = 1 << 20
  CLK_PIN = 1 << 12
  ADA_PIN = 1 << 16
  ADB_PIN = 1 << 4
  ADC_PIN = 1 << 14
  ADD_PIN = 1 << 2

  ROWS = 32
  COLS = 64


VAR

  long SystemTime

  long d1LapNum
  long d1LastTime
  long d1BestTime

  long d2LapNum
  long d2LastTime
  long d2BestTime
  long fontTable[5]

  word pixels[2048]

PUB Start | i
  ' Things that need to happen:
  '  1) Start time keeper core
  '  2) Start display driver core
  '  3) Start UI manager core
  '  4) Start driver 1 monitor core
  '  5) Start driver 2 monitor core
  '  6) Start monitoring for user input
  '
  ' for UI manager: keep a global 'mode' variable that determines if we're in
  ' timing laps, racing, or in the UI. This will determine our priorities

  'pixels[65] := $0F00
  cognew(@time_keeper, @SystemTime)
  cognew(@display_driver, @pixels)

  _pixels := @pixels
  fontTable[0] := @font3x5
  fontTable[1] := @font1x6
  fontTable[2] := @font4x6
  fontTable[3] := @font5x7
  fontTable[4] := @font6x8
  font_table := @fontTable
  cognew(@ui_manager, @SystemTime)

  'waitcnt(1000000+cnt)
  'pixels[2] := $0F00

  'waitcnt(10000000+cnt)
  'pixels[453] := $00F0

  systime_adr  := @SystemTime

  sensor_pin   := D1_SENSOR
  lastlap_adr  := @d1LastTime
  bestlap_adr  := @d1BestTime
  lapcnt_adr   := @d1LapNum
  'cognew(@driver_monitor, 0)

  'waitcnt(20000 + cnt)  ' allow ample time for cog to initialize

  sensor_pin   := D2_SENSOR
  lastlap_adr  := @d2LastTime
  bestlap_adr  := @d2BestTime
  lapcnt_adr   := @d2LapNum
  'cognew(@driver_monitor, 0)


DAT TimeKeeper

            org     0
time_keeper
            mov     timeMSHub, PAR
            mov     nextPhase, CNT
            add     nextPhase, oneMS
sync_here
            waitcnt nextPhase, oneMS
            add     timeMS, #1
            wrlong  timeMS, timeMSHub
            jmp     #sync_here

oneMS       long    _CLKFREQ / 1000
timeMS      long    0
nextPhase   res     1
timeMSHub   res     1

DAT UIManager

            org     0
ui_manager
            mov     x, #40
            mov     y, #0
            mov     fontsize, #2
            call    #set_font
            mov     advance_x, #5
            neg     advance_x, advance_x

            rdlong  systime, PAR
            mov     ttp, systime
            call    #print_timer
            jmp     #ui_manager


            mov     char, #4
            call    #drawchar

            mov     char, #3
            call    #drawchar

            mov     advance_x, #2
            neg     advance_x, advance_x
            mov     char, #9
            call    #drawchar

            mov     advance_x, #5
            neg     advance_x, advance_x
            mov     fontsize, #1
            call    #set_font
            mov     char, #0
            call    #drawchar

donothing   jmp     #donothing
loopy
            mov     x, #40
            mov     y, #0
            rdlong  systime, PAR
            mov     ttp, systime
            call    #print_timer
            jmp     #loopy

print_timer
            ' remove 1's and 10's place of ms
            mov     dividend, ttp
            mov     divisor, #100
            call    #div

            ' draw 100's place of ms
            ' movi    advance, #%100001_001
            mov     advance_x, #2
            neg     advance_x, advance_x
            mov     dividend, quotient
            mov     divisor, #10
            call    #div
            mov     char, remainder
            call    #drawchar

            ' draw .
            mov     advance_x, #5
            neg     advance_x, advance_x
            mov     fontsize, #1
            call    #set_font
            mov     char, #1
            call    #drawchar
            mov     fontsize, #2
            call    #set_font

            ' draw 1's place of seconds
            mov     dividend, quotient
            call    #div
            mov     char, remainder
            call    #drawchar

            mov     advance_x, #2
            neg     advance_x, advance_x
            ' draw 10's place of seconds
            mov     dividend, quotient
            mov     divisor, #6
            call    #div
            mov     char, remainder
            call    #drawchar

            ' draw :
            mov     advance_x, #5
            neg     advance_x, advance_x
            mov     fontsize, #1
            call    #set_font
            mov     char, #0
            call    #drawchar
            mov     fontsize, #2
            call    #set_font

            ' draw minutes
            mov     char, quotient
            call    #drawchar
print_timer_ret
            ret
ttp         long    0

drawchar
            mov     R4, y
            mov     lineptr, chartable
            mov     yloop_cnt, h       ' vertical pixel cnt
            mov     value1, char       ' load char for multiply
            mov     value2, h          ' bytes per char
            call    #mult              ' calculate offset into chartable
            add     lineptr, value1    ' go to start of char

y_loop
            rdbyte  line, lineptr      ' read line
            add     lineptr, #1        ' advance line ptr

            shl     line, #24          ' move bitmap to end of reg
            mov     xloop_cnt, w       ' horizontal pixel cnt
            mov     value1, y
            mov     value2, #COLS      ' pixel = y * cols + x
            call    #mult
            add     value1, x
            shl     value1, #1         ' multiply by 2 (2 bytes per pixel)
            mov     pixptr, value1
            add     pixptr, _pixels    ' add pixel address
x_loop
            xor     R0, R0             ' zero new pixel
            shl     line, #1  wc       ' get value of pixel
            muxc    R0, color          ' set colors to on if pixel is on
            wrword  R0, pixptr         ' write pixel
            add     pixptr, #2
            djnz    xloop_cnt, #x_loop

            add     y, #1
            djnz    yloop_cnt, #y_loop

            mov     y, R4
            add     w, #1
advance     add     x, advance_x
            sub     w, #1
drawchar_ret
            ret
' parameters        %0000_RRRR_GGGG_BBBB
color       long    $0000_0FFF
char        long    1
x           long    0
y           long    0
fontsize    long    0
advance_x   long    0
' internal variables
h           long    0
w           long    0
fonts       long    0
chartable   long    0
lineptr     long    0
line        long    0
_pixels     long    0
pixptr      long    0
yloop_cnt   long    0
xloop_cnt   long    0
font_table  long    0

set_font
            mov     R0, fontsize   ' font size corresponds to a char table
            shl     R0, #2         ' shift to long boundary
            add     R0, font_table ' add base address of ptr table

            rdlong  R1, R0         ' get address of char table

            rdbyte  R0, R1         ' read width
            mov     w, R0
            add     R1, #1

            rdbyte  R0, R1         ' read height
            mov     h, R0
            add     R1, #1
            mov     chartable, R1  ' save char table for selected font
set_font_ret
            ret

write_digit
' R0 = digit to write
' R1 = X,Y

mult
' value1 = value1 * value2
            xor     temp, temp
            tjz     value1, #mult_ret
            tjz     value2, #mult_ret
do_mult
            add     temp, value1
            djnz    value2, #do_mult
            mov     value1, temp
mult_ret
            ret
value1      long    0
value2      long    0
temp        long    0

div
            xor     quotient, quotient
do_div
            sub     dividend, divisor  wc
      if_b  jmp     #div_done
            add     quotient, #1
            jmp     #do_div
div_done
            mov     remainder, dividend
            add     remainder, divisor
div_ret
            ret
dividend    long    0
divisor     long    0
quotient    long    0
remainder   long    0

systime     res     1
R0          res     1
R1          res     1
R2          res     1
R3          res     1
R4          res     1
            fit


DAT DisplayDriver

            org     0
display_driver
            mov     pixeladdr, PAR
            mov     DIRA, allout    ' set output mode on all pins
            or      OUTA, latch     ' always keep it latched

            mov     waitfor, frame_wait
            add     waitfor, cnt
next_row
            ' do row checks
            or      OUTA, disable       ' disable output before switch
            add     row_addr, #1
            and     row_addr, #%1111

            test    row_addr, #1    wz
            muxz    OUTA, addr_a
            test    row_addr, #2    wz
            muxz    OUTA, addr_b

            test    row_addr, #4    wz
            muxz    OUTA, addr_c
            test    row_addr, #8    wz
            muxz    OUTA, addr_d

            waitcnt waitfor, frame_wait ' need to wait approximately 1µS for address switch

next_plane  ' next 'plane' of color depth for binary coded modulation
            mov     toprowaddr, row_addr
            shl     toprowaddr, #7        ' multiply by 128 (64 words/row * 2 bytes/word)
            add     toprowaddr, pixeladdr ' add base address of pixels
            add     toprowaddr, #124      ' move to end of row

            mov     botrowaddr, toprowaddr
            add     botrowaddr, row_offset
            mov     batchcnt, #32
next_batch
            ' output rows
            rdlong  toprow, toprowaddr
            sub     toprowaddr, #4
            mov     R01, toprow

            rdlong  botrow, botrowaddr
            sub     botrowaddr, #4
            mov     R11, botrow

            shr     R01, #16
            shl     toprow, #16
            or      toprow, R01

            shr     R11, #16
            shl     botrow, #16
            or      botrow, R11

next_pix    ' first pixel (right)
            shr     toprow, #5 wc
      if_c  or      OUTA, blue2
            shr     toprow, #5 wc
      if_c  or      OUTA, green2
            shr     toprow, #5 wc
      if_c  or      OUTA, red2
            shr     toprow, #1

            shr     botrow, #5 wc
      if_c  or      OUTA, blue1
            shr     botrow, #5 wc
      if_c  or      OUTA, green1
            shr     botrow, #5 wc
      if_c  or      OUTA, red1
            shr     botrow, #1

            or      OUTA, tick
            and     OUTA, tock
            ' second pixel (left)
            shr     toprow, #5 wc
      if_c  or      OUTA, blue2
            shr     toprow, #5 wc
      if_c  or      OUTA, green2
            shr     toprow, #5 wc
      if_c  or      OUTA, red2

            shr     botrow, #5 wc
      if_c  or      OUTA, blue1
            shr     botrow, #5 wc
      if_c  or      OUTA, green1
            shr     botrow, #5 wc
      if_c  or      OUTA, red1

            or      OUTA, tick
            and     OUTA, tock
      ' proceed to next batch of pixels
            djnz    batchcnt, #next_batch

            and     OUTA, enable        ' enable output
            waitcnt waitfor, addr_wait  ' allow pixels to shine
            jmp     #next_row


allout      long    LATCH_PIN | OE_PIN | R1_PIN | R2_PIN | G1_PIN | G2_PIN | B1_PIN | B2_PIN | CLK_PIN | ADA_PIN | ADB_PIN | ADC_PIN | ADD_PIN
tick        long    CLK_PIN
tock        long    !(R1_PIN | R2_PIN | G1_PIN | G2_PIN | B1_PIN | B2_PIN | CLK_PIN)
latch       long    LATCH_PIN
unlatch     long    !LATCH_PIN
disable     long    OE_PIN
enable      long    !OE_PIN
red1        long    R1_PIN
red2        long    R2_PIN
green1      long    G1_PIN
green2      long    G2_PIN
blue1       long    B1_PIN
blue2       long    B2_PIN
addr_a      long    ADA_PIN
addr_b      long    ADB_PIN
addr_c      long    ADC_PIN
addr_d      long    ADD_PIN
frame_wait  long    (_CLKFREQ / 2000)
addr_wait   long    (_CLKFREQ / 1000000)
row_offset  long    2048
zeros       long    0
row_addr    long    0
pixel_mask  long    $E000_0000
r_ones      long    $FFFF_FFFF
plane       res     1
pixeladdr   res     1
batchcnt    res     1
pixcnt      res     1
toprow      res     1
botrow      res     1
toprowaddr  res     1
botrowaddr  res     1
waitfor     res     1
R01          res     1
R11          res     1
R21          res     1
row_data    res     32
            fit

DAT DriverMonitor

            org     0
driver_monitor
            rdlong  lapstart, systime_adr

monitor_loop
            waitpne sensor_pin, sensor_pin

            rdlong  lastlap, systime_adr
            mov     tmp, lastlap
            sub     lastlap, lapstart
            mov     lapstart, tmp

            test    bestlap, bestlap    wz
    if_z    mov     bestlap, lastlap

            cmp     lastlap, bestlap    wc
    if_c    mov     bestlap, lastlap

            add     lapcnt, #1

            wrlong  lastlap, lastlap_adr
            wrlong  bestlap, bestlap_adr
            wrlong  lapcnt, lapcnt_adr

            jmp     #monitor_loop


sensor_pin  long    0
lastlap_adr long    0
bestlap_adr long    0
lapcnt_adr  long    0
systime_adr long    0
lapcnt      long    0
bestlap     long    0
lastlap     res     1
lapstart    res     1
tmp         res     1
            fit

DAT

            byte    "Starting here"
font3x5
            byte    3, 5

font1x6
            byte    1, 6
            byte    %0000_0000
            byte    %1000_0000
            byte    %0000_0000
            byte    %0000_0000
            byte    %1000_0000
            byte    %0000_0000

            byte    %0000_0000
            byte    %0000_0000
            byte    %0000_0000
            byte    %0000_0000
            byte    %0000_0000
            byte    %1000_0000

font4x6
            byte    4, 6
            byte    %0110_0000
            byte    %1001_0000
            byte    %1001_0000
            byte    %1001_0000
            byte    %1001_0000
            byte    %0110_0000

            byte    %0010_0000
            byte    %0110_0000
            byte    %0010_0000
            byte    %0010_0000
            byte    %0010_0000
            byte    %0111_0000

            byte    %0110_0000
            byte    %1001_0000
            byte    %0001_0000
            byte    %0110_0000
            byte    %1000_0000
            byte    %1111_0000

            byte    %0110_0000
            byte    %1001_0000
            byte    %0010_0000
            byte    %0001_0000
            byte    %1001_0000
            byte    %0110_0000

            byte    %0011_0000
            byte    %0101_0000
            byte    %1001_0000
            byte    %1111_0000
            byte    %0001_0000
            byte    %0001_0000

            byte    %1111_0000
            byte    %1000_0000
            byte    %1110_0000
            byte    %0001_0000
            byte    %1001_0000
            byte    %0110_0000

            byte    %0110_0000
            byte    %1000_0000
            byte    %1110_0000
            byte    %1001_0000
            byte    %1001_0000
            byte    %0110_0000

            byte    %1111_0000
            byte    %0010_0000
            byte    %0100_0000
            byte    %0100_0000
            byte    %0100_0000
            byte    %0100_0000

            byte    %0110_0000
            byte    %1001_0000
            byte    %0110_0000
            byte    %1001_0000
            byte    %1001_0000
            byte    %0110_0000

            byte    %0110_0000
            byte    %1001_0000
            byte    %1001_0000
            byte    %0111_0000
            byte    %0001_0000
            byte    %0110_0000

            byte    %0000_0000
            byte    %0000_0000
            byte    %0000_0000
            byte    %0000_0000
            byte    %0000_0000
            byte    %0000_0000

font5x7
            byte    5, 7

font6x8
            byte    6, 8
