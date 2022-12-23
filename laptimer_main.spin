CON

  _CLKMODE = XTAL1 + PLL16X
  _CLKFREQ = 80_000_000
  D1_SENSOR = 1 << 0
  D2_SENSOR = 1 << 1

  R1_PIN = 1 << 3
  G1_PIN = 1 << 7
  B1_PIN = 1 << 11
  R2_PIN = 1 << 2
  G2_PIN = 1 << 6
  B2_PIN = 1 << 10

  ADA_PIN = 1 << 14
  ADB_PIN = 1 << 15
  ADC_PIN = 1 << 16
  ADD_PIN = 1 << 17

  OE_PIN = 1 << 22
  CLK_PIN = 1 << 24
  LATCH_PIN = 1 << 26

  ROWS = 32
  COLS = 64


VAR

  long SystemTime

  long d1SensorPin
  long d1LapNum
  long d1LapStart
  long d1DeltaTime
  long d1LastTime
  long d1BestTime
  long d1SysTimePtr

  long d2SensorPin
  long d2LapNum
  long d2LapStart
  long d2DeltaTime
  long d2LastTime
  long d2BestTime
  long d2SysTimePtr

  long fontTable[6]

  word pixels[2048]

PUB Start | i, states, tempTime
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

  cognew(@time_keeper, @SystemTime)
  cognew(@display_driver, @pixels)

  d1SensorPin   := D1_SENSOR
  d1SysTimePtr  := @SystemTime
  cognew(@driver_monitor, @d1SensorPin)

  d2SensorPin   := D2_SENSOR
  d2SysTimePtr  := @SystemTime
  cognew(@driver_monitor, @d2SensorPin)

  _pixels := @pixels
  fontTable[0] := @font4x5
  fontTable[1] := @font4x6
  fontTable[2] := @font5x7
  fontTable[3] := @font6x8
  font_table := @fontTable
  cognew(@ui_manager, @SystemTime)


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

DAT DriverMonitor
'  long d1SensorPin
'  long d1LapNum        - updates each lap
'  long d1LapStart      - updates each lap
'  long d1DeltaTime     - updates each lap
'  long d1LastTime      - updates each lap
'  long d1BestTime      - updated sometimes
'  long d1SysTimePtr
            org     0
driver_monitor
            rdlong  sensor_pin, PAR

            add     PAR, #4
            mov     lapcnt_adr, PAR

            add     PAR, #4
            mov     lapstrt_adr, PAR

            add     PAR, #4
            mov     delta_adr, PAR

            add     PAR, #4
            mov     lastlap_adr, PAR

            add     PAR, #4
            mov     bestlap_adr, PAR

            add     PAR, #4
            mov     systime_adr, PAR

start_loop
            rdlong  lapstart, systime_adr       ' get system time
            test    INA, sensor_pin  wz         ' test sensor pin
            wrlong  lapstart, lapstrt_adr       ' write lap start to hub
      if_nz jmp     #start_loop                 ' if pin is high, continue waiting

monitor_loop
            waitpeq sensor_pin, sensor_pin      ' wait until pin is high (beam is unbroken)
            waitpne sensor_pin, sensor_pin      ' wait until pin is low (beam broken)

            mov     lapdelta, lastlap           ' move last lap time to delta
            rdlong  lastlap, systime_adr        ' read current time

            mov     tmp, lastlap                ' copy current time
            sub     lastlap, lapstart           ' calculate time taken for last lap
            wrlong  lastlap, lastlap_adr        ' write last lap time to hub

            sub     lapdelta, lastlap           ' calculate lap delta
            add     lapcnt, #1                  ' increment lap count
            wrlong  lapcnt, lapcnt_adr          ' write lap counter to hub

            mov     lapstart, tmp               ' copy new lap start
            wrlong  lapstart, lapstrt_adr       ' write lap start time to hub

            test    bestlap, bestlap    wz      ' check if there was a previous best lap
    if_z    mov     bestlap, lastlap            ' if not, set last lap to best lap
            wrlong  lapdelta, delta_adr         ' write lap delta to hub

            cmp     lastlap, bestlap    wc      ' compare last lap to best lap
    if_c    mov     bestlap, lastlap            ' if new best, set it
            wrlong  bestlap, bestlap_adr        ' write best lap

            jmp     #monitor_loop


neg_one     long    $FFFF_FFFF
sensor_pin  long    0
lastlap_adr long    0
lapstrt_adr long    0
delta_adr   long    0
bestlap_adr long    0
lapcnt_adr  long    0
systime_adr long    0
lapcnt      long    0
bestlap     long    0
lapdelta    res     1
lastlap     res     1
lapstart    res     1
tmp         res     1
            fit

DAT UIManager

            org     0
ui_manager
            mov     fontsize, #2
            mov     y, #0
            mov     x, #7
            call    #setfont
            mov     char, #"L"
            call    #printchar
            mov     char, #"A"
            call    #printchar
            mov     char, #"P"
            call    #printchar
            mov     char, #" "
            call    #printchar
            mov     char, #"T"
            call    #printchar
            mov     char, #"I"
            call    #printchar
            mov     char, #"M"
            call    #printchar
            mov     char, #"E"
            call    #printchar
            mov     char, #"R"
            call    #printchar

            mov     x, #64
            mov     y, #9
            mov     color, yellow
drawline
            sub     x, #1
            'call    #drawpixel
            sub     y, #1
            call    #drawpixel
            add     x, #1
            add     y, #1
            djnz    x, #drawline

            mov     y, #24
            'mov     color, red
            mov     x, #31
drawline2
            add     y, #7
            'call    #drawpixel
            add     x, #1
            call    #drawpixel
            sub     x, #1
            sub     y, #7
            djnz    y, #drawline2

lbl
            mov     x, #0
            mov     y, #10
            call    #showtimes
            mov     x, #33
            mov     y, #10
            call    #showtimes
            jmp     #lbl

showtimes
            mov     :x_origin, x
            mov     :y_origin, y
            mov     fontsize, #3
            call    #setfont
            mov     color, white
            rdlong  systime, PAR
            'mov     R0, d1params
            'add     R0, #4
            'rdlong  R0, R0
            'sub     systime, R0
            mov     ttp, systime
            call    #print_timer

            mov     fontsize, #1
            call    #setfont
            mov     x, :x_origin
            add     x, #8
            add     y, #9
            mov     R0, d1params
            add     R0, #16
            rdlong  ttp, R0
            call    #print_timer

            mov     color, green
            mov     R0, d1params
            add     R0, #8
            rdlong  ttp, R0
            shl     ttp, #1   wc, wz
            sar     ttp, #1
      if_c  mov     color, red
      if_c  mov     char, #"+"
      if_nc mov     color, green
      if_nc mov     char, #"-"
      if_z  mov     color, white
      if_z  mov     char, #" "
            abs     ttp, ttp
            mov     x, :x_origin
            add     x, #4
            add     y, #7
            call    #printchar
            call    #print_timer
            jmp     #showtimes_ret
:x_origin   long    0
:y_origin   long    0
driver      long    0

showtimes_ret
            ret

print_timer
            cmp     ttp, time_limit  wc, wz
      if_a  mov     ttp, time_limit
            ' remove 1's and 10's place of ms
            mov     dividend, ttp
            mov     divisor, #100
            call    #div
            ' compute 10ths of seconds
            mov     dividend, quotient
            mov     divisor, #10
            call    #div
            mov     tenths_sec, remainder
            ' compute seconds 1's place
            mov     dividend, quotient
            call    #div
            mov     ones_sec, remainder
            ' compute 10's of seconds and minutes
            mov     dividend, quotient
            mov     divisor, #6
            call    #div
            mov     tens_sec, remainder
            mov     minutes, quotient
            ' actually print time
            mov     char, minutes   ' minutes
            call    #printnum

            mov     char, #":"      ' :
            call    #printchar

            mov     char, tens_sec  ' 10's of seconds
            call    #printnum

            mov     char, ones_sec  ' 1's of seconds
            call    #printnum

            mov     char, #"."      ' .
            call    #printchar
                                    ' tenths of seconds
            mov     char, tenths_sec
            call    #printnum
print_timer_ret
            ret
time_limit  long    599999
ttp         long    0
minutes     long    0
tens_sec    long    0
ones_sec    long    0
tenths_sec  long    0
hunths_sec  long    0
thous_sec   long    0

printstr
            rdbyte  char, stringptr wz
      if_z  jmp     #printstr_ret
            add     stringptr, #1
            call    #printchar
            jmp     #printstr
printstr_ret
            ret


printchar
            sub     char, #" "
            call    #drawchar
printchar_ret
            ret

printnum
            add     char, #16          ' 16 = "0" - " "
            call    #drawchar
printnum_ret
            ret

drawchar
            mov     R4, y
            mov     lineptr, chartable
            mov     yloop_cnt, h       ' vertical pixel cnt
            mov     value1, char       ' load char for multiply
            mov     value2, h          ' bytes per char
            call    #mult              ' calculate offset into chartable
            add     lineptr, value1    ' go to start of char
            mov     temp_w, w
y_loop
            rdbyte  line, lineptr      ' read line
            add     lineptr, #1        ' advance line ptr
            mov     R0, line           ' copy line
            and     R0, #%11    wz     ' length modifier?
      if_z  jmp     #do_print          ' N - proceed
            mov     w, R0              ' Y - get modified length
do_print
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
            add     x, w
            add     x, #1
            mov     w, temp_w
drawchar_ret
            ret

drawpixel
            mov     value1, y
            mov     value2, #COLS
            call    #mult
            add     value1, x
            shl     value1, #1
            add     value1, _pixels
            wrword  color, value1
drawpixel_ret
            ret
' parameters        %0000_RRRR_GGGG_BBBB
stringptr   long    0
color       long    $0000_0FFF
char        long    1
x           long    0
y           long    0
fontsize    long    0
' internal variables
h           long    0
w           long    0
temp_w      long    0
fonts       long    0
chartable   long    0
lineptr     long    0
line        long    0
_pixels     long    0
pixptr      long    0
yloop_cnt   long    0
xloop_cnt   long    0
font_table  long    0

setfont
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
setfont_ret
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

off         long    $0000
white       long    $0FFF
red         long    $0F00
green       long    $00F0
blue        long    $000F
yellow      long    $0FF0
cyan        long    $00FF
magenta     long    $0F0F

'd1params    long    0

systime_ar res     1
d1params    res     1
d2params    res     1
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

            waitcnt waitfor, frame_wait ' need to wait approximately 1uS for address switch

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
            shr     toprow, #4 wc
      if_c  or      OUTA, blue2
            shr     toprow, #4 wc
      if_c  or      OUTA, green2
            shr     toprow, #4 wc
      if_c  or      OUTA, red2
            shr     toprow, #4

            shr     botrow, #4 wc
      if_c  or      OUTA, blue1
            shr     botrow, #4 wc
      if_c  or      OUTA, green1
            shr     botrow, #4 wc
      if_c  or      OUTA, red1
            shr     botrow, #4

            or      OUTA, tick
            and     OUTA, tock
            ' second pixel (left)
            shr     toprow, #4 wc
      if_c  or      OUTA, blue2
            shr     toprow, #4 wc
      if_c  or      OUTA, green2
            shr     toprow, #4 wc
      if_c  or      OUTA, red2

            shr     botrow, #4 wc
      if_c  or      OUTA, blue1
            shr     botrow, #4 wc
      if_c  or      OUTA, green1
            shr     botrow, #4 wc
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

DAT

            byte    "Starting here"
font4x5
            byte    4, 5
            byte    %01100000
            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %01100000

            byte    %00100000
            byte    %01100000
            byte    %00100000
            byte    %00100000
            byte    %01110000

            byte    %01100000
            byte    %10010000
            byte    %01100000
            byte    %10000000
            byte    %11110000

            byte    %01100000
            byte    %10010000
            byte    %00100000
            byte    %10010000
            byte    %01100000

            byte    %00100000
            byte    %01010000
            byte    %11110000
            byte    %00010000
            byte    %00010000

            byte    %11110000
            byte    %10000000
            byte    %11100000
            byte    %00010000
            byte    %11100000

            byte    %00100000
            byte    %01000000
            byte    %11100000
            byte    %10010000
            byte    %01100000

            byte    %11110000
            byte    %00100000
            byte    %01000000
            byte    %01000000
            byte    %01000000

            byte    %01100000
            byte    %10010000
            byte    %01100000
            byte    %10010000
            byte    %01100000

            byte    %01100000
            byte    %10010000
            byte    %01110000
            byte    %00010000
            byte    %01100000

            byte    %00000001
            byte    %10000000
            byte    %00000000
            byte    %10000000
            byte    %00000000

            byte    %00000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %10000000

            byte    %01100000
            byte    %10010000
            byte    %11110000
            byte    %10010000
            byte    %10010000

            byte    %11100000
            byte    %10010000
            byte    %11100000
            byte    %10010000
            byte    %11100000

            byte    %01110000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %01110000

            byte    %11100000
            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %11100000

            byte    %11110000
            byte    %10000000
            byte    %11000000
            byte    %10000000
            byte    %11110000

            byte    %11110000
            byte    %10000000
            byte    %11100000
            byte    %10000000
            byte    %10000000

            byte    %01110000
            byte    %10000000
            byte    %10110000
            byte    %10010000
            byte    %01110000

            byte    %10010000
            byte    %10010000
            byte    %11110000
            byte    %10010000
            byte    %10010000

            byte    %01110000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %01110000

            byte    %00010000
            byte    %00010000
            byte    %00010000
            byte    %10010000
            byte    %01100000

            byte    %10010000
            byte    %10100000
            byte    %11000000
            byte    %10100000
            byte    %10010000

            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %11110000

            byte    %10010000
            byte    %11110000
            byte    %10010000
            byte    %10010000
            byte    %10010000

            byte    %10010000
            byte    %11010000
            byte    %10110000
            byte    %10010000
            byte    %10010000

            byte    %01100000
            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %01100000

            byte    %11100000
            byte    %10010000
            byte    %11100000
            byte    %10000000
            byte    %10000000

            byte    %01100000
            byte    %10010000
            byte    %10010000
            byte    %10110000
            byte    %01110000

            byte    %11100000
            byte    %10010000
            byte    %11100000
            byte    %10010000
            byte    %10010000

            byte    %01110000
            byte    %10000000
            byte    %01100000
            byte    %00010000
            byte    %11100000

            byte    %01110000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000

            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %01100000

            byte    %10100011
            byte    %10100000
            byte    %10100000
            byte    %10100000
            byte    %01000000

            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %11110000
            byte    %10010000

            byte    %10010000
            byte    %10010000
            byte    %01100000
            byte    %10010000
            byte    %10010000

            byte    %10010000
            byte    %10010000
            byte    %01110000
            byte    %00010000
            byte    %01100000

            byte    %11110000
            byte    %00100000
            byte    %01000000
            byte    %10000000
            byte    %11110000

            byte    %00000010
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

font4x6
            byte    4, 6
            ' <space>
            byte    %00000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' !
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' "
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' #
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' $
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' %
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' &
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' '
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' (
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' )
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' *
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' +
            byte    %00000011
            byte    %01000000
            byte    %11100000
            byte    %01000000
            byte    %00000000
            byte    %00000000
            ' ,
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' -
            byte    %00000011
            byte    %00000000
            byte    %11100000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' .
            byte    %00000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %10000000
            ' /
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

            byte    %01100000
            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %10010000
            byte    %01100000

            byte    %00100000
            byte    %01100000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %01110000

            byte    %01100000
            byte    %10010000
            byte    %00010000
            byte    %01100000
            byte    %10000000
            byte    %11110000

            byte    %01100000
            byte    %10010000
            byte    %00100000
            byte    %00010000
            byte    %10010000
            byte    %01100000

            byte    %00110000
            byte    %01010000
            byte    %10010000
            byte    %11110000
            byte    %00010000
            byte    %00010000

            byte    %11110000
            byte    %10000000
            byte    %11100000
            byte    %00010000
            byte    %10010000
            byte    %01100000

            byte    %01100000
            byte    %10000000
            byte    %11100000
            byte    %10010000
            byte    %10010000
            byte    %01100000

            byte    %11110000
            byte    %00100000
            byte    %01000000
            byte    %01000000
            byte    %01000000
            byte    %01000000

            byte    %01100000
            byte    %10010000
            byte    %01100000
            byte    %10010000
            byte    %10010000
            byte    %01100000

            byte    %01100000
            byte    %10010000
            byte    %10010000
            byte    %01110000
            byte    %00010000
            byte    %01100000
            ' :
            byte    %00000001
            byte    %00000000
            byte    %10000000
            byte    %00000000
            byte    %10000000
            byte    %00000000
            ' ;
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' <
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' =
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' >
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' ?
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' @
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000

font5x7
            byte    5, 7
            ' space
            byte    %00000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' !
            byte    %10000001
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %00000000
            byte    %10000000
            ' "
            byte    %10100011
            byte    %10100000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' #
            byte    %00000000
            byte    %01010000
            byte    %11111000
            byte    %01010000
            byte    %11111000
            byte    %01010000
            byte    %00000000
            ' $
            byte    %00100000
            byte    %01111000
            byte    %10100000
            byte    %01110000
            byte    %00101000
            byte    %11110000
            byte    %00100000
            ' %
            byte    %00000000
            byte    %10001000
            byte    %00010000
            byte    %00100000
            byte    %01000000
            byte    %10001000
            byte    %00000000
            ' &
            byte    %01100000
            byte    %10010000
            byte    %01100000
            byte    %01100000
            byte    %10010000
            byte    %10011000
            byte    %01101000
            ' '
            byte    %10000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' (
            byte    %00100011
            byte    %01000000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %01000000
            byte    %00100000
            ' )
            byte    %10000011
            byte    %01000000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %01000000
            byte    %10000000
            ' *
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' +
            byte    %00000000
            byte    %00100000
            byte    %00100000
            byte    %11111000
            byte    %00100000
            byte    %00100000
            byte    %00000000
            ' ,
            byte    %00000010
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %01000000
            byte    %10000000
            ' -
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %11111000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' .
            byte    %00000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %10000000
            ' /
            byte    %00001000
            byte    %00001000
            byte    %00010000
            byte    %00100000
            byte    %01000000
            byte    %10000000
            byte    %10000000
            ' 0
            byte    %01110000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %01110000

            byte    %00100000
            byte    %01100000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %01110000

            byte    %01110000
            byte    %10001000
            byte    %00001000
            byte    %00010000
            byte    %00100000
            byte    %01000000
            byte    %11111000

            byte    %01110000
            byte    %10001000
            byte    %00001000
            byte    %00110000
            byte    %00001000
            byte    %10001000
            byte    %01110000

            byte    %00010000
            byte    %00100000
            byte    %01000000
            byte    %10010000
            byte    %11111000
            byte    %00010000
            byte    %00010000

            byte    %11111000
            byte    %10000000
            byte    %10000000
            byte    %11110000
            byte    %00001000
            byte    %10001000
            byte    %01110000

            byte    %00100000
            byte    %01000000
            byte    %10000000
            byte    %11110000
            byte    %10001000
            byte    %10001000
            byte    %01110000

            byte    %11111000
            byte    %00001000
            byte    %00010000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000

            byte    %01110000
            byte    %10001000
            byte    %10001000
            byte    %01110000
            byte    %10001000
            byte    %10001000
            byte    %01110000
            ' 9
            byte    %01110000
            byte    %10001000
            byte    %10001000
            byte    %01111000
            byte    %00001000
            byte    %00010000
            byte    %00100000
            ' :
            byte    %00000011
            byte    %00000000
            byte    %01000000
            byte    %00000000
            byte    %01000000
            byte    %00000000
            byte    %00000000
            ' ;
            byte    %00000011
            byte    %00000000
            byte    %01000000
            byte    %00000000
            byte    %01000000
            byte    %10000000
            byte    %00000000
            ' <
            byte    %00000000
            byte    %00011000
            byte    %01100000
            byte    %11000000
            byte    %01100000
            byte    %00011000
            byte    %00000000
            ' =
            byte    %00000000
            byte    %00000000
            byte    %11111000
            byte    %00000000
            byte    %11111000
            byte    %00000000
            byte    %00000000
            ' >
            byte    %00000000
            byte    %11000000
            byte    %00110000
            byte    %00011000
            byte    %00110000
            byte    %11000000
            byte    %00000000
            ' ?
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' @
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' A
            byte    %01110000
            byte    %10001000
            byte    %10001000
            byte    %11111000
            byte    %10001000
            byte    %10001000
            byte    %10001000

            byte    %11110000
            byte    %10001000
            byte    %10001000
            byte    %11110000
            byte    %10001000
            byte    %10001000
            byte    %11110000

            byte    %01110000
            byte    %10001000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %10001000
            byte    %01110000

            byte    %11100000
            byte    %10010000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10010000
            byte    %11100000

            byte    %11111000
            byte    %10000000
            byte    %10000000
            byte    %11111000
            byte    %10000000
            byte    %10000000
            byte    %11111000

            byte    %11111000
            byte    %10000000
            byte    %10000000
            byte    %11100000
            byte    %10000000
            byte    %10000000
            byte    %10000000

            byte    %01110000
            byte    %10001000
            byte    %10000000
            byte    %10111000
            byte    %10001000
            byte    %10001000
            byte    %01110000

            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %11111000
            byte    %10001000
            byte    %10001000
            byte    %10001000

            byte    %11111000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %11111000

            byte    %11111000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %10100000
            byte    %10100000
            byte    %01100000

            byte    %10001000
            byte    %10010000
            byte    %10100000
            byte    %11000000
            byte    %10100000
            byte    %10010000
            byte    %10001000

            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %10000000
            byte    %11111000

            byte    %10001000
            byte    %11011000
            byte    %10101000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000

            byte    %10001000
            byte    %10001000
            byte    %11001000
            byte    %10101000
            byte    %10011000
            byte    %10001000
            byte    %10001000

            byte    %01110000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %01110000

            byte    %11110000
            byte    %10001000
            byte    %10001000
            byte    %11110000
            byte    %10000000
            byte    %10000000
            byte    %10000000

            byte    %01110000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10011000
            byte    %01110000

            byte    %11110000
            byte    %10001000
            byte    %10001000
            byte    %11110000
            byte    %10100000
            byte    %10010000
            byte    %10001000

            byte    %01110000
            byte    %10001000
            byte    %10000000
            byte    %01110000
            byte    %00001000
            byte    %10001000
            byte    %01110000

            byte    %11111000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000

            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %01110000

            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %01010000
            byte    %00100000

            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10001000
            byte    %10101000
            byte    %10101000
            byte    %01010000

            byte    %10001000
            byte    %10001000
            byte    %01010000
            byte    %00100000
            byte    %01010000
            byte    %10001000
            byte    %10001000

            byte    %10001000
            byte    %10001000
            byte    %01010000
            byte    %00100000
            byte    %00100000
            byte    %00100000
            byte    %00100000

            byte    %11111000
            byte    %00001000
            byte    %00010000
            byte    %00100000
            byte    %01000000
            byte    %10000000
            byte    %11111000

font6x8
            byte    6, 8
            ' <space>
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' !
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' "
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' #
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' $
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' %
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' &
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' '
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' (
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' )
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' *
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' +
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' ,
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' -
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' .
            byte    %00000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %10000000
            ' /
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            ' 0
            byte    %01111000
            byte    %11001100
            byte    %11001100
            byte    %11001100
            byte    %11001100
            byte    %11001100
            byte    %11001100
            byte    %01111000

            byte    %00110000
            byte    %01110000
            byte    %00110000
            byte    %00110000
            byte    %00110000
            byte    %00110000
            byte    %00110000
            byte    %01111000

            byte    %01111000
            byte    %11001100
            byte    %00001100
            byte    %00011000
            byte    %00110000
            byte    %01100000
            byte    %11000000
            byte    %11111100

            byte    %01111000
            byte    %11001100
            byte    %00001100
            byte    %00111000
            byte    %00001100
            byte    %00001100
            byte    %11001100
            byte    %01111000

            byte    %00011000
            byte    %00110000
            byte    %01100000
            byte    %11000000
            byte    %11011000
            byte    %11111100
            byte    %00011000
            byte    %00011000

            byte    %11111100
            byte    %11000000
            byte    %11000000
            byte    %11111000
            byte    %00001100
            byte    %00001100
            byte    %11001100
            byte    %01111000

            byte    %00011000
            byte    %00110000
            byte    %01100000
            byte    %11000000
            byte    %11111000
            byte    %11001100
            byte    %11001100
            byte    %01111000

            byte    %11111100
            byte    %00001100
            byte    %00001100
            byte    %00011000
            byte    %00110000
            byte    %00110000
            byte    %00110000
            byte    %00110000

            byte    %01111000
            byte    %11001100
            byte    %11001100
            byte    %01111000
            byte    %11001100
            byte    %11001100
            byte    %11001100
            byte    %01111000

            byte    %01111000
            byte    %11001100
            byte    %11001100
            byte    %01111100
            byte    %00001100
            byte    %00011000
            byte    %00110000
            byte    %01100000

            byte    %00000001
            byte    %00000000
            byte    %10000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %10000000
            byte    %00000000

            byte    %00000001
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %10000000

            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000
            byte    %00000000