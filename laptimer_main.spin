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


VAR

  long SystemTime

  long d1LapNum
  long d1LastTime
  long d1BestTime

  long d2LapNum
  long d2LastTime
  long d2BestTime

  long pixels[512]

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

  ' cognew(@time_keeper, @SystemTime)

  repeat i from 1 to 2
    pixels[i] := %100_010_001_111__000_000_000_000__0000_0000
  repeat i from 16 to 17
    pixels[i] := %100_010_001_111__000_000_000_000__0000_0000
  repeat i from 256 to 258
    pixels[i] := %000_001_010_011__000_000_000_000__0000_0000

  pixels[128] := %000_001_010_011__000_000_000_000__0000_0000
  pixels[256+16] := %000_001_010_011__000_000_000_000__0000_0000
  pixels[256+8] := %000_001_010_011__000_000_000_000__0000_0000
  pixels[256+24] := %100_101_110_111__000_000_000_000__0000_0000
  pixels[511] := %000_001_010_011__000_000_000_000__0000_0000

  cognew(@display_driver, @pixels)
  ' cognew(@ui_manager, 0)

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
            add     timeMS, 1
            wrlong  timeMS, timeMSHub
            jmp     #sync_here

oneMS       long    _CLKFREQ / 1000
timeMS      long    0
nextPhase   res     1
timeMSHub   res     1

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
            cmp     row_addr, #16   wz
      if_z  xor     row_addr, row_addr

            test    row_addr, #1    wz
            muxz    OUTA, addr_a
            test    row_addr, #2    wz
            muxz    OUTA, addr_b

            test    row_addr, #4    wz
            muxz    OUTA, addr_c
            test    row_addr, #8    wz
            muxz    OUTA, addr_d

            waitcnt waitfor, frame_wait ' need to wait approximately 1µS for address switch

            mov     botrowaddr, #31
            sub     botrowaddr, row_addr
            shl     botrowaddr, #6
            add     botrowaddr, pixeladdr

            mov     toprowaddr, botrowaddr
            sub     toprowaddr, row_offset
            sub     botrowaddr, #4
            mov     batchcnt, #16
next_batch
            ' output rows
            rdlong  toprow, toprowaddr
            add     botrowaddr, #4

            rdlong  botrow, botrowaddr
            add     toprowaddr, #4

next_pix
      ' first pixel in batch
            shl     toprow, #1 wc
      if_c  or      OUTA, red1
            shl     botrow, #1 wc
      if_c  or      OUTA, red2

            shl     toprow, #1 wc
      if_c  or      OUTA, green1
            shl     botrow, #1 wc
      if_c  or      OUTA, green2

            shl     toprow, #1 wc
      if_c  or      OUTA, blue1
            shl     botrow, #1 wc
      if_c  or      OUTA, blue2
      ' activate clock to send pixel
            or      OUTA, tick
            and     OUTA, tock
      ' second pixel in batch
            shl     toprow, #1 wc
      if_c  or      OUTA, red1
            shl     botrow, #1 wc
      if_c  or      OUTA, red2

            shl     toprow, #1 wc
      if_c  or      OUTA, green1
            shl     botrow, #1 wc
      if_c  or      OUTA, green2

            shl     toprow, #1 wc
      if_c  or      OUTA, blue1
            shl     botrow, #1 wc
      if_c  or      OUTA, blue2
      ' activate clock to send pixel
            or      OUTA, tick
            and     OUTA, tock
      ' third pixel in batch
            shl     toprow, #1 wc
      if_c  or      OUTA, red1
            shl     botrow, #1 wc
      if_c  or      OUTA, red2

            shl     toprow, #1 wc
      if_c  or      OUTA, green1
            shl     botrow, #1 wc
      if_c  or      OUTA, green2

            shl     toprow, #1 wc
      if_c  or      OUTA, blue1
            shl     botrow, #1 wc
      if_c  or      OUTA, blue2
      ' activate clock to send pixel
            or      OUTA, tick
            and     OUTA, tock
      ' fourth pixel in batch
            shl     toprow, #1 wc
      if_c  or      OUTA, red1
            shl     botrow, #1 wc
      if_c  or      OUTA, red2

            shl     toprow, #1 wc
      if_c  or      OUTA, green1
            shl     botrow, #1 wc
      if_c  or      OUTA, green2

            shl     toprow, #1 wc
      if_c  or      OUTA, blue1
            shl     botrow, #1 wc
      if_c  or      OUTA, blue2
      ' activate clock to send pixel
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
row_offset  long    1024
zeros       long    0
row_addr    long    0
pixel_mask  long    $E000_0000
r_ones      long    $FFFF_FFFF
pixeladdr   res     1
batchcnt    res     1
pixcnt      res     1
toprow      res     1
botrow      res     1
toprowaddr  res     1
botrowaddr  res     1
waitfor     res     1
R0          res     1
R1          res     1
R2          res     1
            fit

DAT DriverMonitor

            org     0
driver_monitor
            rdlong  lapstart, systime_adr

monitor_loop
            waitpne sensor_pin, sensor_pin

            rdlong  lastlap, systime_adr
            mov     temp, lastlap
            sub     lastlap, lapstart
            mov     lapstart, temp

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
temp        res     1
            fit

DAT CharTable

table
            word    %000_000_000_000_000
            word    %000_000_000_000_000
            word    %000_000_000_000_000
            word    %000_000_000_000_000